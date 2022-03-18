(in-package :microlisp-int)

(fpga-support-version-reporter "FPGA PLA ulisp Support" 0 2 0
                               "Time-stamp: <2022-03-18 15:12:28 gorbag>"
                               "line disambiguation")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.2.0   3/18/22 snapping a line: 0.2 release of library supports scheme-79 test-0 thru test-3 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.1.3   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;; 0.1.2   1/31/22 add suppress-logging property on uops

;; 0.1.1   1/13/22 add abiility to define covering sets of control wires (see comments)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.1   1/ 6/22 move ufun-p umac-p etc. here
;;                 define opcode-fn so client code doesn't need us use property
;;                    also other prop accessors

;; 0.0.0  12/ 3/21 - 12/15/21 Starting transferring more portable stuff
;;                      from simulator directory in support of a
;;                      generalized microlisp

(defun known-microfunction-p (sym)
  "Returns non-nil if the symbol names a declared microfunction (in the project)."
  (or (assoc sym *ulisp-operations-alist*)
      (assoc sym *ulisp-macro-alist*)
      ))

(defun all-microfunctions ()
  (append (mapcar #'car *ulisp-operations-alist*)
          (mapcar #'car *ulisp-macro-alist*)
          (mapcar #'car *internal-ucode-operations-alist*)))

(defmacro defupred-inverse (normal-pred inverse-pred)
  `(update-alist ',normal-pred ',inverse-pred *predicate-inverses*))

(defun invert-predicate (pred)
  (or (cdr (assoc pred *predicate-inverses*))
      (car (rassoc pred *predicate-inverses*))))

(define-property-accessor upred-p :ulisp-ucode-pred)

(define-property-accessor ucode-sense-wire :ulisp-ucode-sense-wire)

(define-property-accessor ucode-pred-type :ulisp-ucode-pred-type)

(define-property-accessor ucode-pred-from-register :ulisp-ucode-pred-from-register)

(define-property-accessor ucode-pred-defn :ulisp-ucode-pred-defn)

;; add note that sense-wire may be negated. 
(defun upred-desc (pred-symbol)
  "returns three values: the associated sense wire for the predicate
and if it is indirect (tests what FROM points to, not the register
itself), implicit register(s) (if any), the CAR of which to be used
for FROM if a list, and finally the symbol used to define the
expansion function if specialized. Note that the sense wire may also
be (NOT <sense-wire>) indicating the negated sense-wire should be
used."
  (values (ucode-sense-wire pred-symbol)
          (ucode-pred-type pred-symbol)
          (ucode-pred-from-register pred-symbol)
          (ucode-pred-defn pred-symbol)))

(define-property-accessor opcode-fn :ulisp-ucode-fn)

(defun ufun-p (fn-symbol)
  (not (null (opcode-fn fn-symbol))))

(defun umac-p (mac-symbol)
  (not (null (member mac-symbol fpga-pla-build-tools:*defumac-macros*))))

(define-property-accessor ucode-precedence :ulisp-ucode-precedence)

(define-property-accessor ucode-constituent :ulisp-ucode-constituent)

(define-property-accessor ucode-suppress-logging :ulisp-ucode-suppress-logging)

;; some helper functions to access bit encoded values vs. symbolic information
;; note these mostly depend on variables set up AFTER the microcode is loaded!
(defun pointer-type-name->int (symbol)
  (cadr (assoc symbol **pointer-types**)))

(defun int->pointer-type-name (int)
  (car (rassoc int **pointer-types** :key #'car)))

(defun non-pointer-type-name->int (symbol)
  (cadr (assoc symbol **non-pointer-types**)))

(defun int->non-pointer-type-name (int)
  (car (rassoc int **non-pointer-types** :key #'car)))

(defun type-name->int (symbol)
  "generalized version of above"
  (or (pointer-type-name->int symbol)
      (non-pointer-type-name->int symbol)))

(defun int->type-name (int)
  "generalized version of above"
  (cl:if (< int **pointer**) ; demarks boundary
      (int->pointer-type-name int)
      (int->non-pointer-type-name int)))

;; some validation functions
(defun validate-register-name (name &optional bus-p)
  (let ((valid-list (mapcar #'car **machine-registers**)))
    (when bus-p
      (setq valid-list (cons 'microlisp::bus valid-list)))
    (assert (member name valid-list) (name)
            "~A is not a member of **machine-registers**~s" name (cl:if bus-p
                                                                     " or bus"
                                                                     ""))))

(defun validate-register-control (register-name control-name)
  (assert (member control-name (cdr (assoc register-name *register-control-wires*)))
          (register-name)
          "Register ~s does not support control ~a!" register-name control-name))

(defun validate-register-sense (register-name sense-name)
  (assert (member sense-name (cdr (assoc register-name *register-sense-wires*)))
          (register-name)
          "Register ~s does not support sense ~a!" register-name sense-name))

(defun validate-numargs (exp numargs)
  (let ((len (length (cdr exp))))
    (cl:cond
      ((or (eq numargs t) ; any
           (null numargs)) ; unknown
       t)
      ((consp numargs) ; low-high
       (assert (and (<= len (cadr numargs))
                    (>= len (car numargs)))
               ()
               "Incorrect number of args in expression ~s" exp))
      (t
       (assert (= (length (cdr exp)) numargs) () "Incorrect number of args in expression ~s" exp)))))


;; note that many of the fields have relationships. Allow covering
;; sets to be declared as this allows us to translate references to a
;; covering field to the union of the partitioning fields (i.e. if we
;; want to move something TO the VAL register, we can use the set of
;; TO-TYPE and TO-ADDRESS as that's the partition for TO (VAL does not
;; declare a TO control wire, but it does declare the latter). In the
;; AIM, the authors allude to having nanocode for this, but rather
;; than writing a special parser for the TO field (which is where this
;; comes up in scheme-79) I think having this kind of declaration will
;; be more general.


(defun declare-covering-set (imaginary-control-wire &rest covering-control-wires)
  "Note that the imaginary-control-wire may not be imaginary for some
registers, but if a register does NOT have the imaginary control wire,
we can check if it does, instead, have the covering control wires and
use those as an alias as it were (multiple active TO wires are
allowed, for instance). We can check these recursively, if needed."
  ;; Since there can be more than one cover for a control wire, we
  ;; need to always cons on new wires
  (format *error-output* "~&; Setting up covering set for ~s~%" imaginary-control-wire)
  (setq *control-wire-covering-sets-alist*
        (acons imaginary-control-wire
               covering-control-wires
               *control-wire-covering-sets-alist*)))

(defun find-covering-set (desired-control-wire available-control-wires)
  "Tries to find a set of control wires from the list that implement the desired-control-wire"
  (if (find desired-control-wire available-control-wires) 
    ;; well that was easy
    (list desired-control-wire)
    (some #'(lambda (entry)     ;; we check each as there may be more than one cover
              (when (eql (car entry) desired-control-wire)
                (let ((diff (set-difference (cdr entry) available-control-wires)))
                  (cond ((null diff) ;match
                         (cdr entry))
                        (t
                         (append
                          (intersection available-control-wires (cdr entry)) ; control wires we do have
                          (mapcan #'(lambda (mismatch)
                                      (let ((hits (find-covering-set mismatch available-control-wires)))
                                        (when (null hits)
                                        ; every one better generate a result
                                          (return-from find-covering-set nil))
                                        (copy-list hits))) ; mapcan is destructive
                                  diff)))))))
          *control-wire-covering-sets-alist*)))

(defun control-wires-for-register-op (register-name desired-control)
  "Interface for find-covering-set for a specific register"
  (find-covering-set desired-control (cdr (assoc register-name *register-control-wires*))))

