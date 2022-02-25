(in-package :fpga-registers)

(fpga-support-version-reporter "FPGA Register Support" 0 1 5
                               "Time-stamp: <2022-02-25 17:50:08 gorbag>"
                               "explicit word-size")

;; 0.1.5   2/24/22 make word-size explicit (was register-size)

;; 0.1.4   2/18/22 add assert to set-control-wire-fn to detect if there isn't 
;;                    such a control wire

;; 0.1.3   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;; 0.1.2   1/18/22 cleanup obsolete code: removing special treatment of registers
;;                    which required multiple control wires for TO as new covering
;;                    set computation deals with it.

;; support for registers

;; 0.1.1   1/13/22 set-control-wire-fn - moves this out of project
;;                     specific code and adds ability to substitute
;;                     covering set of controls in place of missing
;;                     one

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.3    1/ 6/22 move special-register-p here
;;                  add accessors for properties

;; 0.0.2    1/ 4/22 defchip-reg and defchip-special-reg now
;;                     repatriates name arg to microlisp-shared

;; 0.0.1   12/15/21 Moved in additional register support fns from
;;                     simulator directory

;; 0.0.0   12/ 3/21 New: moved from various simulation files,
;;                     e.g. machine-defs

(defparameter *word-size* 32)

(defparameter *register-size* *word-size*
  "effectively our word size, unless one wants to consider a CONS cell
a word instead of two words... this establishes how many bits we
should allocate as a bit vector to represent a given
register (e.g. created via defchip-reg). Note that this can be
overridden in a project, just make sure it gets set before defining
registers or loading the microcode.")

(defvar *all-register-names* nil
  "keep track of all the defined registers. This may include
pseudo-registers, but not aliases, so it's different from
**machine-registers**.  This is so we can analyze our references to
actual physical registers on the chip (which includes pseudo-registers
since they act similarly, just not in the programmers model)")

(defun make-register (&optional (size *register-size*))
  (make-n-bit-vector size))

(define-property-accessor register-flags :flags) ; for compatability with cl-lib

;; helper functions for accessing flags
(defun register-flag-accessor (flag)
  ;; register flag accessors should be in the project machine
  (intern (format nil "REGISTER-~A-P" (string flag)) *project-machine-pkg*)) 

;; registers are represented in lisp (both for declaration and
;; simulation purposes) as special vars, surrounding by the usual
;; astericks. So to get the actual register name we have to strip the
;; astericks. Also registers can have fields (which represent
;; continguous groups of bits in the register) and we represent that
;; as *<register-name>-<field-name>*. Here are some functions to make
;; using those representations easier.

;; naming help
(defun strip-register-name (register-name)
  (subseq (string register-name) 1 (1- (length (string register-name)))))

(defun make-register-field-symbol (register-name field-name)
  (intern (format nil "*~A-~A*"
                  (strip-register-name register-name)
                  (string field-name))
          *project-machine-pkg*))

;; a value on the bus can be populated FROM a register or sent TO a
;; register. While the bus can be sent TO as many registers as you
;; like in a cycle, it can only come FROM a single register. While
;; this code was generated assuming there was a single bus, there's no
;; reason each bus can't have their own FROM and TO registers in a
;; given cycle.

;; Controls allow us to have bits that represent that a register
;; should be dumped onto the bus or taken from the bus during a
;; cycle. They get defined on the register (defureg) and again the
;; current implementation assumes a single bus because that's what
;; scheme-79 has, but there's no reason this can't be easily (?)
;; extended specifying the bus (well one can argue some registers
;; might only be accessable to some busses and that declaration should
;; be part of either the bus definition of defureg or the register. In
;; either case we'd need to extend our implementation of defureg, and
;; probably it would make more sense to have a specialized version for
;; the bus spec (defubus?) (TBD)

;; note that controls can be against a field and not the whole
;; register.  also note that fields are "global" in the sense that a
;; particular range of bits cannot be contextually determined by the
;; register, but a register can either support the field or not. That
;; is, if bits 1-7 are defined to be the "type" field, then any
;; register with a "type" field must use bits 1..7 for that. But not
;; all registers need to have control bits to load/store the type
;; field independent of other fields or the rest of the
;; register. Fields can also overlap, e.g. the address field in
;; scheme-79 is subdivided into the displacement and frame fields,
;; while the type field includes the ~pointer bit.

;; field specifics are project specific, so we only put the helper
;; functions here not the field definitions.

;; latches are similar to registers, but typically associated with pads, which have the same symbol format
;; as registers (i.e. *<name>*)

(defun latch-name (name)
  "return the name for the latch (memory/flip flop) of a pad"
  (intern (format nil "~A-LATCH*"
                  (subseq (string name) 0 (1- (length (string name)))))
          *project-machine-pkg*))


;; macros to allow projects to declare registers. Currently these set
;; up variables of use during simulation but eventually should
;; generate appropriate HDL as well.

(define-property-accessor register-alias :register-alias)

(define-property-accessor register-refs :register-refs)

(define-property-accessor register-alist :register-alist)

(defmacro defchip-reg (name &optional original)
  "Used to create initial definion of register and an alias if needed."
  `(cl:progn ,@(cl:cond
                 (original
                  `((defvar ,name ,original)
                    (setf (register-alias ',name) ',original)))
                 (t
                  `((defvar ,name (make-register))
                    (pushnew ',name *all-register-names*)
                    ;; will be an association list later, but for now just make it have something non-nil.
                    (setf (register-alist ',name) '((nil))) 
                    (setf (register-refs ',name) 0))))
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (export-ulisp-symbol ',name))))

(defmacro defchip-special-reg (name bits)
  "Used to create internal registers that aren't really machine registers"
  ;; note, this is NOT the same as *special-registers*. Hopefully we can get rid of that.
  
  ;; make sure we get the package right for export
  `(cl:progn (defvar ,name (make-register ,bits))
             (pushnew ',name *all-register-names*)
             ;; will be an association list later, but for now just make it have something non-nil.
             (setf (register-alist ',name) '((nil))) 
             (setf (register-refs ',name) 0)
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (export-ulisp-symbol ',name))))

(defun register-p (symbol)
  (and (symbolp symbol)
       (or (not (null (register-alist symbol)))
           (not (null (register-alias symbol))))))

;; used on registers
(define-property-accessor valid-control-wires :valid-control-wires)
(define-property-accessor valid-sense-wires :valid-sense-wires)

;; control and sense wires are used on registers
(define-property-accessor sense-wire-name :sense-wire-name)
(define-property-accessor sense-wire-register :sense-wire-register) ; register associated with this sense wire
(define-property-accessor sense-wire-encoding :sense-wire-encoding)

;; for simulation, at least, here is code for setting and clearing the control wires
;; we have separate from and to control

(defun set-control-wire-fn (control-wire-name register-name)
  (let ((control-wires (microlisp-int:control-wires-for-register-op register-name control-wire-name))
        (nv (gensym)))
    (assert control-wires (control-wire-name) "Control wire ~a for register ~a does not exist!" control-wire-name register-name)
    `(lambda (,nv)
       ,@(mapcar #'(lambda (cl)
                     `(setf (,(register-flag-accessor cl) ',register-name) ,nv))
                 control-wires))))

      
