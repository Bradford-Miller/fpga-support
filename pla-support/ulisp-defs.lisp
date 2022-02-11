(in-package :microlisp-int)

(fpga-support-version-reporter "FPGA PLA ulisp Defs" 0 1 2
                               "Time-stamp: <2022-02-09 12:01:36 gorbag>"
                               "line disambiguation")

;; 0.1.2   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;; 0.1.1   1/13/22 add *control-wire-covering-sets-alist* and reset fn
;;                 break out declare-register-control-wires and declare-register-sense-wires
;;                   from defreg (so defureg can use them)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.4   1/10/22 move export-ulisp-symbol to fpga-suport-modules.lisp

;; 0.0.3   1/ 6/22 accessor function for microcode-symbol

;; 0.0.2   1/ 5/22 fix sharing ulisp symbols - use shared pkg if the symbol's home
;;                   package is there, and otherwise the unshared
;;                   ulisp-pkg to fix load and modification issues in
;;                   the project.

;; 0.0.1   1/ 4/22 Move ucode readtable definition here from ucode-validator.lisp
;;                 Also move defschip, defreg here from ucode-validator.lisp
;;                 as well as: type-dispatch validation code

;; 0.0.0  12/ 3/21 -- 12/15/21 Starting transferring more portable stuff
;;                      from simulator directory in support of a
;;                      generalized microlisp

;; see compiler-core-defs.lisp for microlisp definitions, and ??? for
;; microPLA definitions

;; generalized microlisp operations declarations. See
;; simulator/ucode-defs for actual operations for scheme-79

;; these divisions were originally made for the non-compiler validator
;; (to check that I had correctly transcribed the microcode). At some
;; point they should be made uniform declarations and probably built
;; up from the defining form (e.g defufn) rather than the separate
;; declarations in ucode-defs.lisp (TBD)

;; BUT right now (as of 12/2021), there are more known functions than
;; definitions (i.e. I haven't finished defining the less used
;; operations yet), but I'm defining a class for general operator
;; description anyway which we should move toward using.

;; define a readtable for microlisp to handle the non-CL comma usage

;; We will define our own readtable to do this
;; (and package, which is defined in packages.lisp)

(defun ucode-read-comma (stream char)
  "read the following token and match it to the parameters of the macro"
  (declare (ignore char))

  (let ((subsymbol (read stream t nil t)))
    (list* '*comma* subsymbol))) ; tbd

(defreadtable ucode-syntax
    (:merge :standard)
  (:macro-char #\, #'ucode-read-comma) ; deal with it differently since it
                                       ; does not need to be in a backquote
  )

(defclass-x ulisp-operator-descriptor ()
  ((opname :reader opname
           :initarg :name
           :type symbol
           :documentation "name of the operator")
   (numargs :reader numargs
            :initarg :numargs
            :type (or null list integer) ; null means unknown (any), list is a list suitable for typep
            :documentation "number of arguments the operator takes, a list of lower and upper bound, or nil if any")
   (argtypes :reader argtypes
             :initarg :argtypes
             :type list
             :documentation "type descriptors for each arg, can use dotted list for \"rest\" type")
   ))

(defclass-x ulisp-macro-descriptor (ulisp-operator-descriptor)
  ((embedded-ops :reader embedded-ops
                 :initarg :embedded-ops
                 :type :list
                 :documentation "operations the macro typically
                 expands into. Obviously we can do the expansion to be
                 sure, but for typical validation purposes, this is
                 faster.")
   ))

(defun share-ulisp-symbol (sym)
  (let ((home-pkg (symbol-package sym)))
    (cl:cond
     ((eql home-pkg *ulang-shared-pkg*)
      (export (list sym) *ulang-shared-pkg*))
     (t
      (import (list sym) *ulang-pkg*)
      (export (list sym) *ulang-pkg*)))))

(defun create-ulopd (name numargs &rest argtypes)
  "Create ulisp operator descriptor"
  (share-ulisp-symbol name)
  (cons name (make-ulisp-operator-descriptor :name name :numargs numargs
                                             :argtypes argtypes)))

(defvar *ulisp-operations-alist* nil
  "New consolodated version of *hardware-operations-alist*, *core-register-operations-alist*,
and *ucode-operations-alist*")

(defvar *ulisp-macro-alist* nil
  "New consoldated version of *embedded-ucode-operations-alist* and
  *special-ucode-operations-alist*")

(defun create-ulmd-internal (name numargs argtypes expansion-ops)
  (declare (type symbol name)
           (type list numargs argtypes expansion-ops))
  (cons name (make-ulisp-macro-descriptor :name name
                                          :numargs (if (endp (cdr numargs))
                                                          (car numargs)
                                                          numargs)
                                          :argtypes argtypes
                                          :embedded-ops expansion-ops)))


(defmacro create-ulmd (name numargs argtypes &rest expansion-ops)
  "Create ulisp macro descriptor"
  `(progn 
     (share-ulisp-symbol ',name)
     (create-ulmd-internal ',name 
                           ',(if (not (consp numargs))
                               (list numargs)
                               numargs)
                           ',argtypes
                           ',expansion-ops)))

(defparameter *internal-ucode-operations-alist* nil
  "Automatically generated by defufn for functions not on any other
list - i.e. something that is used internally generally to
automatically specialize some other operation (say the predicate on
COND)")

(defvar *registers-whose-types-are-tags* nil
  "These are registers that when we do a &set-type on we are giving
  them a tag (so we will return/jump to it at some point")

(defparameter *boolean-terms* '(and or not))

(defvar *predicate-inverses* nil)

;; these will be defined by microcode file
(declaim (special **pointer-types** **non-pointer-types** **pointer** **machine-registers**))

(defparameter *defschip-symbols*
  '(**pointer-types** **non-pointer-types** **pointer** **machine-registers**)
  "These are the valid symbols that can be used with the defschip form, and are used to create an
association between bit representations and the symbols in the microcode.")

(defun clear-schip-defs ()
  (mapc #'(lambda (sym)
            (set sym nil))
        *defschip-symbols*))

;; define some macros that appear in the ulisp.

;; So these are the toplevel macros/constructs in the microcode, and my own
;; comments/understanding of what each will do (which will drive the
;; implemetation here). I will correct these as I discover what works and
;; does not so hopefully a "later reader" will see the documentation for
;; what is actually implemented rather than my earlier theory.

;; defschip name-symbol <quoted alist>
;;  the name-symbols appear to be predefined (they are not otherwise
;;  referenced) for the interpreter and/or hardware.
;;  the quoted-alist only contains proper sublists (that is, the car is the
;;  key and the cadr the value). The exception is **pointer** which is just
;;  a number instead of a quoted alist (so we can presume defschip is a
;;  function rather than a macro

;;  the used name-symbols are:
;;    **pointer-types**      ;; I expect these two are distinguished for the GC
;;    **non-pointer-types**  ;; as pointer-types must be followed to be marked
;;    **pointer** ;; a number - note that as the numbers are in octal this
;;                ;; is a bit, and also note all the *non-pointer-types*
;;                ;; have this bit set
;;    **machine-registers**  ;; if these have a value it seems to be an
;;                           ;; alias for the register, i.e. shared between
;;                           ;; the main processor and the storage manager


(cl:defmacro defschip (name interpreted-argument)
  "Used internally to the interpreter - creates an association between the
  bit representation and symbols used in the microcode for some predefined
  types. These should be declared before use as they will be used to
  validate the microcode."
  ;; note we have an equivalent "hard" version of this in the various
  ;; machine files. I suspect these declarations in the MIT code was
  ;; to drive their own lisp based simulator prior to building out the
  ;; chip; in my case since I'm trying to automate building the chip I
  ;; need this information before the microcode can be read.

  ;; validate name
  (assert (member name *defschip-symbols*) (name) "~A is not a valid defschip name" name) 

  `(eval-when (:load-toplevel :execute)
     (setq ,name ,interpreted-argument)
     (when (and **pointer-types** **non-pointer-types**)
       (initialize-types-and-tags))))


;; defreg <register-name> <list of control wires> <list of sense
;; wires> the semantics of the control and sense wires doesn't seem to
;; be mentioned so this seems to just be a bridge between the hardware
;; implementation of the registers and the microcode interpreter, or
;; maybe instructions to the hardware layout mechanism? At any rate,
;; as I already mentioned this is information that is redundant at
;; this point with what's in the machine*.lisp files. But we parse it
;; for validation purposes. At come point we might even compare the
;; two.

(defvar *register-control-wires* nil)
(defvar *register-sense-wires* nil)

(defun clear-register-defs ()
  (setq *register-control-wires* nil)
  (setq *register-sense-wires* nil))

;; this gets called in .mcr files. It's really redundant with defureg which is in the project files,
;; but we maintain both for the time being.

(defun declare-register-control-wires (register-name control-wires)
  (update-alist register-name control-wires *register-control-wires*))

(defun declare-register-sense-wires (register-name sense-wires)
  (update-alist register-name sense-wires *register-sense-wires*))

(cl:defmacro defreg (name control-wires sense-wires)
  ;; validate name (should be a key in the **machine-registers**, not an
  ;; alias or value)
  (assert (boundp '**machine-registers**) () "defschip for **machine-registers** must be read before defreg")
  (validate-register-name name t)
  `(cl:progn
     (declare-register-control-wires ',name ',control-wires)
     (declare-register-sense-wires ',name ',sense-wires)))

;; covering sets (see ulisp-support)
(defvar *control-wire-covering-sets-alist* nil)

(defun reset-covering-set-alist ()
  (setq *control-wire-covering-sets-alist* nil))

;; type dispatch (validation)

;;
;; The next items I'm not completely clear about yet, but this is my current
;; best guess (as of 11/18/20) based on reading the microcode. I will refine
;; as the interpreter is implemented and we try to make it work!
;;

;; NB: at this point thesea re only used in validation (1/4/22), but
;; should eventually get used for compilation as well! See also
;; type-dispatch as nanocode, which is specific to the scheme-79
;; usage, however, microlisp seems to generally support type
;; operations (based on the type field) generally so we're including
;; support here to the extent possible.

(defvar *type-dispatch-alist* nil)

(defvar *type-return-dispatch-alist* nil)

(defun clear-type-dispatch ()
  (setq *type-dispatch-alist* nil)
  (setq *type-return-dispatch-alist* nil))

(defvar *pc-dispatch-alist* nil)

(defun clear-pc-dispatch ()
  (setq *pc-dispatch-alist* nil))

(defvar *to-registers-used* nil
  "keep track of registers that were used in the TO field for nanocode
  as antecedents for the whole register")

(defvar *from-registers-used* nil
  "keep track of registers that were used in the FROM field for
  nanocode as antecedents for the whole register")

(defvar *ucode-function-expansion-alist* nil
  "For defufn's that expand into a recursive call on compile, this is
used to record the expansion for validation purposes (i.e. counting
register references, etc.). It should be an alist of the function name
and the registers used in the expansion.")

(define-property-accessor microcode-symbol :microcode-symbol)
;; allow additional project specific declarations on opcodes
(define-property-accessor microcode-declarations :microcode-declarations) 
