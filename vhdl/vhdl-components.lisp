(in-package :vhdl)

(fpga-support-version-reporter "FPGA VHDL Components" 0 2 0
                               "Time-stamp: <2022-03-23 16:56:35 gorbag>"
                               "new")

;; 0.2.0   3/21/22 New package for VHDL generator components

;; Here we define functions and macros to generate the important parts of a
;; VHDL design. Calling/using these will generate ASCII text to *vhdl-stream*
;; which may be bound as needed before use (see vhdl-defs.lisp).

;; the formatting these fns and macros generate generally follows
;; "VHDL by Example" by Blaine Readler 2014 ISBN 978-0-9834973-5-6
;; and yes, even the auto-generated comments do ;-).

(defun prin-initial-library-usage (library usage-terms)
  "called the first time for a particular library to be used with usage
terms. Note that the scope of a library and use clause is just the entity, so
be sure to have this for each entity. Architectures inherit from the entity,
not the preceeding library/use clause"
  (declare (type keyword library)
           (type list usage-terms))

  (format *vhdl-stream* "~&library ~A;~%" library)
  (mapc #'prin-library-usage usage-terms)
  (terpri *vhdl-stream*))

(defun prin-library-usage (term)
  "called with a particular use term"
  (declare (type string term))
  (format *vhdl-stream* "~&use ~A;~%" term))

(defun prin-signal-description (desc maxsiglen)
  "Print an individual signal description within the entity. A special
signal named :comment will treat the rest of the arguments as a string
to print out in a comment to aid in reading the signal descriptions at
the user's discression."
  (cond
   ((eql (car desc) :comment)
    (prin-comment-line (format nil "~(~{~A ~}~)" (cdr desc)) 1 9))
   (t
    (destructuring-bind (signal-name class signal-type &optional vector-desc) desc
      (format *vhdl-stream* "~9t~(~a~a : ~3a ~a~a~);~%"
              signal-name 
              (make-pad (- maxsiglen (length (string signal-name))))
              class 
              signal-type
              (or vector-desc ""))))))
            
(defun prin-entity (entity-name signal-descriptions)
  (declare (type string entity-name)
           (type list signal-descriptions))
  (format *vhdl-stream* "entity ~A is~%  port (~%"
          entity-name)
  (let ((maxsiglen (apply #'max (mapcar #'(lambda (x) (length (string (car x)))) signal-descriptions))))
    (mapc #'(lambda (x) (prin-signal-description x maxsiglen)) signal-descriptions))
  (format *vhdl-stream* "~&~7t);
end entity ~A;~%"
          entity-name))

;; defining an entity consists of setting up any needed library and use
;; clauses, then naming the signal vectors and scalars. This macro should help
;; with the common use-case, though one can of course define things without it.

(defmacro defentity (entity-name (&optional library &rest usage-terms) &body body)
  "define an entity with a single library and optionally multiple usage
terms. The body should consist of triples (lists), of pattern ( <signal-name>
:in|:out <signal-type> ) where <signal-type> is something like std_logic or
std_logic_vector(3 downto 0)"
  `(progn
     (header-comment '("Header information") t)
     ,(when library
        `(prin-initial-library-usage ',library ',usage-terms))
     (prin-entity ,entity-name ',body)
     (terpri *vhdl-stream*)))
  
;; architecture. refers to an entity by name, and inherits library/usage from
;; that entity.  it's up to the user to make sure that the architecture is
;; synthesizable. There are two "kinds" of architecture, one is just RTL and
;; the other is a process. Mainly the difference is syntactic. (?)

(defun upcase-ops (sym)
  "if the symbol names an operation in IEEE logic, upcase it, else downcase it for printing"
  (cond
   ((member sym *ops-to-upcase*)
    (string-upcase (string sym)))
   (t
    (string-downcase (string sym)))))

(defun prin-rtl-rhs (exprs)
  "the RHS exprs may themselves be lists of exprs so we need to do this recursively"
  (cond
   ((not (consp exprs))
    (upcase-ops exprs))
   (t
    (format nil "(~{~A ~})"
            (mapcar #'prin-rtl-rhs exprs)))))

(defun prin-rtl-description (desc maxsiglen)
  (declare (list desc))
  "Print an individual RTL line to *vhdl-stream*. As with
prin-signal-description, a description list that begins with the
keyword :comment will have the rest of the list printed as a comment
into the stream to aid debugging"
  (cond
   ((eql (car desc) :comment)
    (prin-comment-line (format nil "~(~{~A ~}~)" (cdr desc)) 1 4))
   (t
    (destructuring-bind (signal &rest combinatoric-logic) desc
      ;; slams everything into one line, may want to be able to split
      ;; for prettyness sake (TBD)
      (format *vhdl-stream* "~4T~(~A~)~A <= ~{~A ~};~%" 
              signal
              (make-pad (- maxsiglen (length (string signal))))
              (mapcar #'prin-rtl-rhs combinatoric-logic))))))

(defmacro defarch-rtl (arch-name entity-name (&rest decls) &body body)
  (declare (type string arch-name entity-name))
  
  `(progn
     (format *vhdl-stream* "~&architecture ~(~A of ~A~) is~%~%"
             ',arch-name ',entity-name)
     ;; if there are any declarations, insert them here
     ,(when decls
        (let ((maxsiglen (apply #'max (mapcar #'(lambda (d) (length (string (car d)))) decls))))
          `(progn (mapc #'(lambda (d)
                            (format *vhdl-stream* "~3Tsignal ~(~A~)~A : ~(~{~A~}~);~%" 
                                    (car d)
                                    (make-pad (- ,maxsiglen (length (string (car d)))))
                                    (cdr d)))
                        ',decls)
             (terpri *vhdl-stream*))))
     (format *vhdl-stream* "begin~%")
     ;; really should tab over here
     (prin-comment-line "Design implementation" 1 4)
     (terpri *vhdl-stream*)
     ,(let ((maxsiglen (apply #'max (mapcar #'(lambda (d) (if (eql (car d) :comment)
                                                            0
                                                            (length (string (car d)))))
                                            body))))
        `(mapc #'(lambda (x) (prin-rtl-description x ,maxsiglen)) ',body))

     (format *vhdl-stream* "~&~%end architecture ~(~A~);~%~%" ',arch-name)))
