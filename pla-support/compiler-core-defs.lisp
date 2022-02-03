(in-package :fpga-pla-build-tools)

(fpga-support-version-reporter "FPGA PLA ulisp Comp. Defs" 0 1 3
                               "Time-stamp: <2022-01-31 17:55:08 gorbag>"
                               "add supress-logging key to defufn")

;; 0.1.3   1/31/22 add explicit supress-logging key to defufn to prevent
;;                      overloading :constituent for this

;; 0.1.2   1/27/22 add code to support expansion for constitutent
;;                      assignment

;; 0.1.1   1/25/22 add :arg option for defupred which just puts the
;;                      argument or from register onto the bus (rather
;;                      than taking the car or cdr of what it points
;;                      to).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.2   1/ 5/22 new *defupred-predicates*

;; 0.0.1  12/14/21 new *defumac-macros*

;; 0.0.0  12/ 2/21 Starting transferring more portable compiler stuff
;;                      from simulator/compiler-defs in support of a
;;                      generalized microlisp

;; Microlisp definitions
;;
;; These are my general assumptions about microlisp, not necessarily
;; the original authors (unfortunately, I could not find a definitive
;; description, so came up with these through the practice of
;; implementing Scheme-79)...

;; Lisp syntax (obviously).

;; Support for macros. Many of the macros are built-in but the
;; microcode file can define new ones with defmicromacro

;; Typed cells. These are mapped to their binary representation via
;; **pointer-types** and **non-pointer-types**.
;;
;; The code associated with these are defined with the deftype form

;; Program Counter: sections of code can be defined associated with a
;; symbol using defpc and defreturn. The difference is that defpc is
;; the target of a go-to instruction, while defreturn defines a new
;; non-pointer type (not listed in **non-pointer-types**) that is used
;; as the target of (&set-type *stack* <defreturn
;; sym>). dispatch-on-stack is then used to go-to that address.


;; Register orientation. There are a number of registers and a
;; particular microlisp instruction will generally have a to and/or a
;; from register. In some cases, there may be more than one from
;; register, as when we write to a location (one register may be the
;; address, and another the intended content). Future machines may
;; have multiple busses to facilitate efficient movement of data
;; through the processor, but the concept of "from" and "to" registers
;; would remain.


;; note that while there may be some common functions and macros
;; available in all microlisps, for the most part we expect them to be
;; defined by the project. Here we are defining representations and
;; compilation tools that can take those definitions and transform a
;; microcode expression into something appropriate for our assembler
;; (q.v.)

;; during compilation (parsing forms) we may temporarily store some of
;; our discoveries on stack-bound variables. 

(defvar *from-register* nil)
(defvar *to-register* nil)
(defvar *line-opcode* nil)
(defvar *enclosing-opcode* nil)
(defvar *function-being-compiled* nil)
(defvar *constituent-assignment-fn* nil)
(defvar *defumac-macros* nil
  "macros defined via defumac")
(defvar *defupred-predicates* nil
  "predicates defined via defupred")

;; notes on the (scheme-79) ucode-compiler:

;; While this is inspired by the notes in AIM 559, it is not a
;; faithful rendition of the original compiler as it is not adequately
;; documented for that. However, I hope it does replicate the original
;; "in spirit" and remains faithful to the underlying concepts.

;; Note that the microcode is read in and from that some information
;; is gathered that is used to create/compile the nanocode. The
;; nanocode is compiled before the microcode is, so the appropriate
;; integer bit representations for registers offsets into the nano
;; array, etc. can be determined as AIM 559 says the original did. We
;; also use thier field scheme:

;; Nano-words are selected by state information from the micro control
;; and any next state generated from the nano control. The micro
;; control specifies to and from registers (as bits per above). The
;; nano control generates pad and register controls (as well as the
;; next nano state). Note that freeze may be asserted for multi-word
;; nano sequences to prevent additional micro words from being
;; decoded. Example multiple word nano instructions are given in the
;; paper but are not exhaustive, and rely on microinstructions that
;; differ from the documented microcode (so may be either output from
;; the microword compiler based on that input, or are from a different
;; version of the same - their intent is to communicate the flavor of
;; thier implementation not document it for reproduction in the future
;; after all, though one would really have some expectation of the
;; latter as a matter of *science* - results should be reproducable!).

;; (defnano (do-car)
;;    ((from*) (ale))
;;    ((to*) (read)))

;; (defnano (do-cdr)
;;    ((from*) (ale))
;;    ((to*) (read cdr)))

;; (defnano (do-restore)
;;    (() (from-stack ale))
;;    ((to*) (read))
;;    (() (read cdr to-address-stack to-type-stack)))

;; Here the from* and to* forms are meant to be taken as pronouns that
;; refer to the values passed from the microcode, so "do-restore" can
;; be seen to ignore the from* value.

;; To translate the above into what (I think) actually happens, the
;; first do-car instruction will take whatever is in the FROM field in
;; the microcode and put it on the bus, and raise ALE (so it's
;; transmitted to the external memory as an address). The second
;; instruction reads the response from memory onto the bus and gates
;; it into the TO register. So our job is to make sure we do this
;; gating during the appropriate PH1 and PH2 (simulated) clock cycles,
;; have the external memory respond to it, etc. in the hardware
;; emulation, but for the work below it's just to capture our
;; representations for registers that will be used for both the micro
;; and nano code, and then to compile the "defnano" terms into the
;; nanocontrol array generating a symbol table for linking the
;; microcode to nanocontrolarray offsets (states).

;; note in the AIM the lisp code is translated first from micro-lisp
;; into a micro PLA specification that looks vaguely similar to the
;; nanocode, e.g.

;; 
;;  *args* = *leader*; *display* = *node-pointer*
;; (defpc mark-node
;;  (assign *leader* (&car (fetch *node-pointer*)))
;;  (cond ((and (&pointer? (fetch *leader*))

;;
;; MARK-NODE
;; ((FROM *DISPLAY*) (TO *ARGS*) DO-CAR)
;; ((FROM *ARGS*) (BRANCH TYPE=POINTER-BUS MARK-NODE-3 MARK-NODE-1))
;;
;; [??: MARK-NODE] -> GO-TO 250, DO-CAR, FROM 7 TO 15 (250 200 007 15)
;; [250] -> GOTO 44, BRANCH, FROM 6 (044 307 006 00)
;;
;; [44: MARK-NODE-3] CONDITION-BIT CLEAR (even) - typically 'fail'
;; [45: MARK-NODE-1] CONDITION-BIT SET (odd) - typically 'success'

;; generaly to-addresses look like (fetch *register*)

;; may want to make this a generic so we can easily extend in the project? (TBD)
(defun parse-to-address-term (term)
  "gets an appropriate to-address from term"
  (assert (consp term) (term) "parse-to-address-term: term is not a cons: ~s")
  (ecase (car term)
    (fetch
     (cadr term)) ; should name a register
    ((&car &cdr)
     ;; need to generate an access function
     (tbd "need an access function"))))


(defun setup-ucode-expansion (name expansion-def)
  (update-alist name expansion-def
                *ucode-function-expansion-alist*))

(defun ucode-has-expansion-p (sym)
  (not (null (assoc sym *ucode-function-expansion-alist*))))

;; declare a function as machine-level for ucode
(defmacro defufn (fn-name-symbol (&rest lambda-list) &body body)
  "defufn associates microcode functions with nanocode
implementations. It needs to create a function that will return the
encoded microcode as a list of an atom plus three
integers: (<next-ustate> <nano-operation> <from> <to>) where
<next-ustate> will typically be NIL and filled in by the microcode
compiler later, but can also be a symbolic tag referring to a location."
  ;; make sure we use an appropriate version of the fn-name-symbol
  (let ((args-last (extract-keyword :args-last lambda-list))
        (ucode-expansion (extract-keyword :expansion lambda-list))
        (constituent-status (extract-keyword :constituent lambda-list))
        (suppress-logging (extract-keyword :suppress-logging lambda-list))
        (special-declarations (extract-keyword :declarations lambda-list)))
      `(cl:progn
         (defun ,fn-name-symbol (,@(remove-keyword-args
                                    '(:args-last 
                                      :expansion
                                      :declarations
                                      :constituent
                                      :suppress-logging)
                                    lambda-list))
           ,@body)
         ;; particularly an issue before we repatriate defufn into support
         ,@(unless (or (eq (symbol-package fn-name-symbol) *ulang-shared-pkg*) ; already in the right place
                       (eq (symbol-package fn-name-symbol) *ulang-pkg*)) ; if here we don't want to export it
             `((export-ulisp-symbol ',fn-name-symbol)))

         ;; this would be a good place to add a descriptor too
         ,@(unless (known-microfunction-p fn-name-symbol)
             `((pushnew (list ',fn-name-symbol) *internal-ucode-operations-alist*)))
         ,(if args-last ; because we set up an environment for the args
            `(setf (ucode-precedence ',fn-name-symbol) :args-last) 
            `(setf (ucode-precedence ',fn-name-symbol) :args-first)) ; typical lisp-like
         ,(if ucode-expansion
            `(setup-ucode-expansion ',fn-name-symbol ',ucode-expansion))
         ,(if special-declarations
              `(setf (microcode-declarations ',fn-name-symbol) ',special-declarations))
         ;; constituent means the result of the op is typically
         ;; assigned to another register (appears inside an assign or
         ;; morally equivalent statement)
         ,(if constituent-status
              `(setf (ucode-constituent ',fn-name-symbol) ',constituent-status))
         ,(if suppress-logging
            `(setf (ucode-suppress-logging ',fn-name-symbol) ',suppress-logging))
         ;; so we know we are running a hardware supported function:
         (setf (opcode-fn ',fn-name-symbol) #',fn-name-symbol))))

(defmacro defumac (mac-name-symbol (&rest lambda-list) &body body)
  "similar to defufn but generates microlisp that needs compiled instead of pla code for assembly and "
  `(cl:progn
     (defufn ,mac-name-symbol (,@lambda-list) ,@body)
     (pushnew ',mac-name-symbol *defumac-macros*)))

(defmacro defupred (pred-symbol (sense-line indirect-p &optional register) &body body)
  "defupred declares microcode predicates.
For now, it just sets up a property list. indirect-p if false means
the predicate tests the sense-line on the (from) register, and if :car
means it tests the car of what the (from) register points to via
testing the sense-line on *bus*, and if :cdr the cdr, and if :arg the
register directly (generally this means to move the register onto the
bus to enable the comparison). If register is supplied, it is an
implicit FROM register for this predicate. If body is present, it
constructs a function of two arguments (fail-tag and success-tag) that
builds a specialized set of microinstructions that result in a
branch."
  `(cl:progn
     (export-ulisp-symbol ',pred-symbol)
     (setf (upred-p ',pred-symbol) t)
     (setf (ucode-sense-line ',pred-symbol) ',sense-line)
     (setf (ucode-pred-type ',pred-symbol) ,indirect-p)
     (pushnew ',pred-symbol *defupred-predicates*) ; for symmetry with defumac
     ,(when register
        `(setf (ucode-pred-from-register ',pred-symbol) ',register))
     ,(when (not (null body))
        `(cl:progn
           (defun ,pred-symbol (fail-tag success-tag)
             ,@body)
           (setf (ucode-pred-defn ',pred-symbol) ',pred-symbol)))))

(defun assign-constituent-highlevel (result-reference)
  "when we are compiling an embedded expression for a constituent,
this allows us to produce the code to assign the result to the correct
target, generating microlisp for compilation."
  (assert *constituent-assignment-fn* () "No constituent assignment function!")
  (append *constituent-assignment-fn* (list result-reference)))

#||
(defun assign-constituent-lowlevel (result-reference &optional lowlevel-ready-p)
  "Similar to highlevel, but generates assembly code (result of compilation)."
  (cl:cond
   (lowlevel-ready-p
    )
   (t
    (let ((highlevel (assign-constituent-highlevel result-reference)))
      (apply (car highlevel) (cdr highlevel)))))) ; calls the compiler on the form
||#
