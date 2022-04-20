(in-package :vhdl)

(fpga-support-version-reporter "FPGA VHDL Components" 0 2 2
                               "Time-stamp: <2022-04-11 17:11:22 gorbag>"
                               "defcentity")

;; 0.2.2   4/11/22 add defcentity for complex entities (>1 library)
;;                    This will be needed so we can include predefined VHDL
;;                    generated (by hand mostly) to support the lisp macros
;;                    for specifying processors

;; 0.2.1   4/ 5/22 add simple comments (:scomment). These are not centered
;;                    horizontally.

;; 0.2.0   3/21/22 -- 4/1/22 New package for VHDL generator components

;; Here we define functions and macros to generate the important parts of a
;; VHDL design. Calling/using these will generate ASCII text to *vhdl-stream*
;; which may be bound as needed before use (see vhdl-defs.lisp).

;; the formatting these fns and macros generate generally follows
;; "VHDL by Example" by Blaine Readler 2014 ISBN 978-0-9834973-5-6
;; and yes, even the auto-generated comments do ;-).

;; need to cleanup handling of indent/tabs (maybe a special var for indent
;; depth) (TBD) also auto indent after if/elsif/for etc. not quite working
;; because we only look at the first symbol on the line rather than scanning
;; the whole line for the proper (following) indent. May be better to scan
;; and insert auto breaks, e.g.

;; (defclk
;;    (for j in 1 to 3 loop wait until falling_edge (clk))
;;    (end loop))

;; currently generates:
;;    for j in 1 to 3 loop wait until falling_edge (clk ) ;
;;    end loop ; 

;; and it might be better if it generated the more natural looking:
;;    for j in 1 to 3 loop
;;        wait until falling_edge (clk);
;;    end loop;

;; but this isn't critical since it should compile out the same regardless
;; (only useful when auditing the VHDL in other words).

(defun prin-library-usage (term)
  "called with a particular use term"
  (declare (type string term))
  (format *vhdl-stream* "~&use ~A;~%" term))

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

(defun prin-signal-description (desc maxsiglen initial-padlen &optional final-p)
  "Print an individual signal description within the entity. A special
signal named :comment will treat the rest of the arguments as a string
to print out in a comment to aid in reading the signal descriptions at
the user's discression."
  (cond
   ((eql (car desc) :comment)
    (prin-comment-line (format nil "~(~{~A ~}~)" (cdr desc)) 1 initial-padlen))
   ((eql (car desc) :scomment)
    (prin-simple-comment (format nil "~(~{~A ~}~)" (cdr desc)) initial-padlen))
   (t
    (destructuring-bind (signal-name class signal-type &optional vector-desc) desc
      (format *vhdl-stream* "~a~(~a~a : ~3a ~a~a~)~A~%"
              (make-pad initial-padlen)
              (downcase-if-symbol signal-name)
              (make-pad (- maxsiglen (length (string signal-name))))
              class 
              signal-type
              (or vector-desc "")
              (if final-p "" ";")))))) ; last clause supresses the semicolon.

(defun prin-entity-internal (entity-p entity-name signal-descriptions)
  (declare ;(type string entity-name)
           (type list signal-descriptions))
  (let ((title (if entity-p "entity" "component"))
        (title-pad (if entity-p "" "   "))
        (is-entity (if entity-p " is" ""))
        (padded-port (if signal-descriptions (if entity-p "  port (" "     port (") "")))
    (format *vhdl-stream* "~A~A ~A~A~%~A~%"
            title-pad
            title
            (downcase-if-symbol entity-name)
            is-entity
            padded-port)
    (if (not (endp signal-descriptions))
      (let ((maxsiglen (apply #'max 0 (mapcar #'(lambda (x) (length (string (car x)))) signal-descriptions))))
        (mapl #'(lambda (x) (prin-signal-description (car x) maxsiglen (if entity-p 9 12) (endp (cdr x)))) signal-descriptions)))
    (format *vhdl-stream* "~a~Aend ~A ~A;~%"
            (if (not (endp signal-descriptions)) (format nil "~7t);~%") "")
            title-pad
            title
            (if entity-p entity-name ""))))

(defun prin-entity (entity-name signal-descriptions)
  (declare ;(type string entity-name)
           (type list signal-descriptions))
  (prin-entity-internal t (downcase-if-symbol entity-name) signal-descriptions))

;; defining an entity consists of setting up any needed library and use
;; clauses, then naming the signal vectors and scalars. This macro should help
;; with the common use-case, though one can of course define things without it.

(defmacro defentity (entity-name (&optional library &rest usage-terms) &body body)
  "define an entity with a single library and optionally multiple usage
terms. The body should consist of triples (lists), of pattern ( <signal-name>
:in|:out <signal-type> ) where <signal-type> is something like std_logic or
std_logic_vector(3 downto 0)"
  `(progn
     ;(header-comment '("Header information") t)
     ,(when library
        `(prin-initial-library-usage ',library ',usage-terms))
     (prin-entity ,(downcase-if-symbol entity-name) ',body)
     (terpri *vhdl-stream*)))

(defmacro defcentity (entity-name library-spec-list &body body)
  "similar to defentity, but takes a list of library/usage spec terms, e.g.
\(defcentity foo ((ieee ieee.std_logic_1164.all ieee.numeric_std.all)
                  (lib lib.clock.all)) ..."
  `(progn
     ,@(mapcar 
        #'(lambda (libspec)
            `(prin-initial-library-usage ',(car libspec) ',(cdr libspec)))
        library-spec-list)
     (prin-entity ,(downcase-if-symbol entity-name) ',body)
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
    (format nil "~(~A~)" sym)))) ; use format so it handles numbers too

(defun prin-rtl-rhs (exprs)
  "the RHS exprs may themselves be lists of exprs so we need to do this recursively"
  (cond
   ((and (not (consp exprs))
         (assoc exprs *special-constants*))
    (cdr (assoc exprs *special-constants*)))
   ((not (consp exprs))
    (upcase-ops exprs))
   ((member (car exprs) '(:stringquote)) ; special for printing strings
    (if (stringp (cadr exprs)) ; probably just the string without a base
      (format nil "\"~{~A~}\"" (mapcar #'prin-rtl-rhs (cdr exprs)))
      ;deal with the base
      (format nil "~A\"~{~A~}\"" (string-upcase (string (cadr exprs))) (mapcar #'prin-rtl-rhs (cddr exprs)))))
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
    ;; this creates some ugliness in terms of extra spaces; we should
    ;; probably just avoid format here (TBD)
    (prin-comment-line (format nil "~(~{~A ~}~)" (cdr desc)) 1 4))
   ((eql (car desc) :scomment)
    (prin-simple-comment (format nil "~(~{~A ~}~)" (cdr desc)) 4))
   (t
    (destructuring-bind (signal &rest combinatoric-logic) desc
      ;; slams everything into one line, may want to be able to split
      ;; for prettyness sake. (TBD)
      (format *vhdl-stream* "~4T~(~A~)~A <=~{ ~A~};~%" 
              signal
              (make-pad (- maxsiglen (length (string signal))))
              (mapcar #'prin-rtl-rhs combinatoric-logic))))))

(defun prin-proc-description (desc starting-indent)
  (declare (list desc))
  "Print an individual process line to *vhdl-stream*. As with
prin-rtl-description, a description list that begins with the
keyword :comment will have the rest of the list printed as a comment
into the stream to aid debugging"
  (cond
   ((eql (car desc) :comment)
    ;; this creates some ugliness in terms of extra spaces; we should
    ;; probably just avoid format here (TBD)
    (prin-comment-line (format nil "~(~{~A ~}~)" (cdr desc)) 1 starting-indent))
   ((eql (car desc) :scomment)
    (prin-simple-comment (format nil "~(~{~A ~}~)" (cdr desc)) starting-indent))
   (t
    ;; slams everything into one line, may want to be able to split
    ;; for prettyness sake. (TBD)
    (format *vhdl-stream* "~A~{~A ~};~%" 
            (make-pad starting-indent)
            (mapcar #'prin-rtl-rhs desc)))))

(defun prin-arch-decls (decls)
  "print the signal declarations associated with an architeture"
  (let ((maxsiglen (apply #'max (mapcar #'(lambda (d) (length (string (car d)))) decls))))
    (mapc #'(lambda (d)
              (cond
                ((eql (car d) :comment)
                 (prin-comment-line (format nil "~(~{~A ~}~)" (cdr d)) 1 3))
                ((eql (car d) :scomment)
                 (prin-simple-comment (format nil "~(~{~A ~}~)" (cdr d)) 3))
                (t
                 (format *vhdl-stream* "~3Tsignal ~(~A~)~A : ~(~{~A~}~);~%" 
                         (car d)
                         (make-pad (- maxsiglen (length (string (car d)))))
                         (cdr d)))))
          decls)
    (terpri *vhdl-stream*)))

;; two differences (so far) between this and prin-arch-decls: the
;; increased indent level for the begin and end of process, and we
;; deal with variables instead of signals. (eventually we probably
;; should have commond code and pass that in)
(defun prin-proc-decls (decls)
  "print the variable declarations associated with a process"
  (let ((maxsiglen (apply #'max (mapcar #'(lambda (d) (length (string (car d)))) decls))))
    (mapc #'(lambda (d)
              (format *vhdl-stream* "~7Tvariable ~A~A :~{ ~A~};~%" 
                      (downcase-if-symbol (car d))
                      (make-pad (- maxsiglen (length (string (car d)))))
                      (mapcar #'prin-rtl-rhs (cdr d))))
          decls)
    (terpri *vhdl-stream*)))

(defmacro defrtl (&body body)
  (let ((maxsiglen (if (not (endp (cdr body)))
                     (apply #'max (mapcar #'(lambda (d) (if (member (car d) '(:comment :scomment))
                                                          0
                                                          (length (string (car d)))))
                                          body))
                     (length (string (caar body))))))
    `(mapc #'(lambda (x) (prin-rtl-description x ,maxsiglen)) ',body)))

;; these (defarch-rtl and defarch-clk) are for the "simple" case where
;; there is only one operative clause. If you want to put together a
;; more complex architecture, use the following macros
;; with-architecture, defprocess, etc.
(defmacro defarch-rtl (arch-name entity-name (&rest signal-decls) &body body)
  "the intent here is to handle purely combinatoric architecture. See defarch-clk for the alternative"
  ;(declare (type string arch-name entity-name))
  
  `(progn
     (format *vhdl-stream* "~&architecture ~(~A of ~A~) is~%~%"
             ,(downcase-if-symbol arch-name) ,(downcase-if-symbol entity-name))
     ;; if there are any declarations, insert them here
     ,(when signal-decls
        `(prin-arch-decls ',signal-decls))
     (format *vhdl-stream* "begin~%") ; 
     ;(prin-comment-line "Design implementation" 1 4)
     (terpri *vhdl-stream*)
     (defrtl ,@body)

     (format *vhdl-stream* "~&~%end architecture ~(~A~);~%~%" ,(downcase-if-symbol arch-name))))

(defmacro defclk (&body body)
  (let ((indent 7))
    `(progn ,@(mapcar #'(lambda (x) 
                          (let (ret)
                            (when (member (car x) '(end elsif else))
                              (decf indent 3))
                            (setq ret `(prin-proc-description ',x ,indent))
                            (when (member (car x) '(if for elsif else))
                              (incf indent 3))
                            ret))
                      body))))

(defmacro defarch-clk (arch-name entity-name process-name (sensitivity-list &rest signal-decls) (&rest variable-decls) &body body)
  "similar to defarch-rtl but creates a process structure with a
sensitifity list (i.e., what one typically would use for clocked
logic)"
  ;(declare (type string arch-name entity-name process-name))
  
  `(progn
     (format *vhdl-stream* "~&architecture ~(~A of ~A~) is~%~%"
             ,(downcase-if-symbol arch-name) ,(downcase-if-symbol entity-name))
     ;; if there are any declarations, insert them here
     ,(when signal-decls
        `(prin-arch-decls ',signal-decls))
     (format *vhdl-stream* "begin~%")
     ;(prin-comment-line "Design implementation" 1 4)
     (terpri *vhdl-stream*)

     ;; I REALLY hate format... reminds me of TECO :-(
     (apply #'format *vhdl-stream* "~4T~A: process ~((~#[~;~a~:;~@{~A~^, ~}~]~))~%" ,(downcase-if-symbol process-name) ',sensitivity-list)
     ,(when variable-decls
        `(prin-proc-decls ',variable-decls))
     (format *vhdl-stream* "~4Tbegin~%")
     (defclk ,@body)

     (format *vhdl-stream* "~&~4Tend process;~%~%end architecture ~(~A~);~%~%" ',arch-name)))

(defmacro with-architecture ((arch-name entity-name &rest decl-fns) &body body)
  "wrap the body with an architecture declaration. Unlike defarch-*
forms, this one allows individual statement to be constructed for
declarations (e.g. type declarations as well as signals and variables)
and multiple processes if desired. Note that body should be lisp terms
to be evaluated, not pseudo-VHDL as defarch-* expects, however
functions such as prin-proc-decls may be used to themselves accept
pseudo-VHDL."
  ;(declare (type string arch-name entity-name))  
  `(progn 
     (format *vhdl-stream* "~&architecture ~A of ~(~A~) is~%~%"
             ,(downcase-if-symbol arch-name)
             ,(downcase-if-symbol entity-name))
     ,@decl-fns
     (format *vhdl-stream* "~%begin~%")
     ;(prin-comment-line "Design implementation" 1 4)
     (terpri *vhdl-stream*)
     ,@body
     (format *vhdl-stream* "~&~%end architecture ~A;~%~%" ,(downcase-if-symbol arch-name))))

(defun prin-constant (constant-name constant-type constant-value &optional (value-type "") 
                                    &key (maxlen (length (string constant-name))) comment)
  ;; currently tab in 3 as constants appear at top level within architecture.
  (format *vhdl-stream* "~3tconstant ~A~A : ~(~A := ~A ~A~); "
          (string-upcase (string constant-name))
          (make-pad (- maxlen (length (string constant-name))))
          constant-type
          constant-value
          value-type)
  (if comment
    (format *vhdl-stream* "-- ~a~%" comment) 
    (terpri *vhdl-stream*)))

(defmacro defcomponent (component-name &body body)
  "A component is like an entity, but appears inside an architecture
statement to refer to some other (already defined) entity. The body
consists of the usual ports that are typically copied from the entity
description."
  `(progn
     (prin-entity-internal nil ,(downcase-if-symbol component-name) ',body)
     (terpri *vhdl-stream*)))

(defun prin-portmap-description (desc maxsiglen last-p)
  (declare (list desc))
  "Print an individual port mapping line to *vhdl-stream*. As with
prin-signal-description, a description list that begins with the
keyword :comment will have the rest of the list printed as a comment
into the stream to aid debugging"
  (cond
   ((eql (car desc) :comment)
    ;; this creates some ugliness in terms of extra spaces; we should
    ;; probably just avoid format here (TBD)
    (prin-comment-line (format nil "~(~{~A ~}~)" (cdr desc)) 1 9))
   ((eql (car desc) :scomment)
    ;; this creates some ugliness in terms of extra spaces; we should
    ;; probably just avoid format here (TBD)
    (prin-simple-comment (format nil "~(~{~A ~}~)" (cdr desc)) 9))
   (t
    (destructuring-bind (signal component-padname) desc
      ;; slams everything into one line, may want to be able to split
      ;; for prettyness sake. (TBD)
      (format *vhdl-stream* "~9T~(~A~A => ~A~)~A~%"
              signal
              (make-pad (- maxsiglen (length (string signal))))
              component-padname
              (if last-p "" ","))))))

(defun prin-portmap (decls)
  "print the port mapping declarations associated with an copy of a component"
  (let ((maxsiglen (apply #'max (mapcar #'(lambda (d) (if (member (car d) '(:comment :scomment))
                                                        0
                                                        (length (string (car d)))))
                                        decls))))
    
    (mapl #'(lambda (d)
              (prin-portmap-description (car d) maxsiglen (endp (cdr d))))
          decls)))

(defmacro component-portmap (copy-name component-name &body body)
  "when we instantiate a particular component, we have to map the
ports in the local description to the ports in the component
definition (see defcomponent)"
  `(progn
     (format *vhdl-stream* "~&~3T~A: ~A~%~3Tport map~%~7T(~%" 
             ,(downcase-if-symbol copy-name)
             ,(downcase-if-symbol component-name))
     (prin-portmap ',body)
     (format *vhdl-stream* "~7T);~%~%")))

(defmacro defprocess ((process-name sensitivity-list &rest decl-fns) &body body)
  "Similar to with-architecture, the decl-fns and body should be lisp
functions that generate the appropriate code onto *vhdl-stream*,
rather than pseudo-vhdl that defarch-clk allows, and should typically
occur inside the body of with-architecture."
  (declare ;(type string process-name)
           (type list sensitivity-list))
  `(progn
     (apply #'format *vhdl-stream* "~4T~A: ~(process ~#[~;(~a)~:;(~@{~A~^, ~})~]~)~%" ,(downcase-if-symbol process-name) ',sensitivity-list)
     ,(if decl-fns `(terpri))
     ,@decl-fns
     (format *vhdl-stream* "~4Tbegin~%")
     ,@body
     (format *vhdl-stream* "~4Tend process ~a;~%~%" ,(downcase-if-symbol process-name))))

