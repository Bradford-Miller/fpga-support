(in-package :microlisp-int)

(fpga-support-version-reporter "FPGA PLA ulisp macros" 0 1 0
                               "Time-stamp: <2022-01-11 17:00:46 gorbag>"
                               "0.1 release")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.0   1/ 4/22  New file - moved here from simulator/ucode-validator.lisp
;;                     Support for microlisp macros

;; see compiler-core-defs.lisp for microlisp definitions, and ??? for
;; microPLA definitions

;; defmacro <macro name> <argument list> &body
;; defmicromacro <macro name> <argument list> &body

;; I'm going to presume for now this is similar to a simple
;; substitution macro as I note the use of comma form without
;; backquote so presumably the entire body has an implicit backquote.
;; I'm guessing comma forms can only reference the arguments, rather
;; than being lisp expressions that get evaluated in the compile-time
;; environment.

;; What this means is that we have to override the default common-lisp
;; readtable when loading compiling/loading the microcode file into
;; the interpreter.

;; note per comment above, we ignore defmacro at this point as it was
;; for the MIT simulator, and we've implemented save/restore as
;; microcode (which the AIM refers to!)

(defvar *macro-defn-alist* nil)

(defun clear-macro-defs ()
  (setq *macro-defn-alist* nil))

(defun setup-macro (macro-defn)
  "We've already checked that the CAR of macro-defn is 'defmacro"
  (destructuring-bind (name formals &rest body) (cdr macro-defn)
    (update-alist name (list* formals body) *macro-defn-alist*)))

(defun macro-symbol-p (sym)
  "return non-nil if the sym is a defined macro name"
  (not (null (assoc sym *macro-defn-alist*))))

(defun expand-macro (exp)
  "We've determined exp is a macro instance and needs expanded."
  (note-if *debug-validator*
           "expanding macro: ~s~%" exp)
  (destructuring-bind (formals body) (cdr (assoc (car exp) *macro-defn-alist*))
    ;; set up a binding list for the formals to the actuals
    ;; if this were code we were really going to use a lot, we would check that the lambda lists match, and generate an error otherwise.
    (let ((bindings (mapcar #'(lambda (f v)
                                (cons f v))
                            formals (cdr exp))))
      ;; now scan through the macro body for *comma* terms we can match to our formals and substitute the binding.
      (progfoo (expand-macro-internal body bindings)
        (note-if *debug-validator*
                 "macro expansion: ~s~%" foo)
        ))))

(defun expand-macro-internal (body bindings)
  (cl:cond
   ((and (consp body) (eql (car body) '*comma*))
    (cdr (assoc (cdr body) bindings)))
   ((and (consp body) (macro-symbol-p (car body)))
    (expand-macro body))
   ((consp body)
    (cons (expand-macro-internal (car body) bindings)
          (expand-macro-internal (cdr body) bindings)))
   (t
    body)))

