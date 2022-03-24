(in-package :cl-user)
(defvar *fpga-support-version-reporter-initializations* nil)

(cl-lib:detailed-version-reporter "FPGA Dev Support packages" 0 2 3
                                  "Time-stamp: <2022-03-24 12:24:16 gorbag>"
                                  "add make-pad to common"
                                  :initialization-list-symbol *fpga-support-version-reporter-initializations*)

;; 0.2.3   3/24/22 make-pad in common

;; 0.2.2   3/23/22 center-pad-string in common

;; 0.2.1   3/21/22 new VHDL package

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.2.0   3/18/22 snapping a line: 0.2 release of library supports scheme-79 test-0 thru test-3 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.1.7   2/24/22 move *word-size* and *register-size* to project defs (so it's
;;                    clear the project can pick something else)

;; 0.1.6   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;; 0.1.5   2/ 2/22 upla-write-code upla-write-code-annotation upla-write-tag

;; 0.1.4   1/31/22 ucode-suppress-logging

;; 0.1.3   1/25/22 export macro tags fail-tag and success-tag so they
;;                    can be used in the body

;; 0.1.2   1/18/22 cleanup obsolete code: removing special treatment of registers
;;                    which required multiple control wires for TO as new covering
;;                    set computation deals with it.

;; 0.1.1   1/13/22 add ability to declare covering sets for control wires and look them up.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.20  1/11/22 new announce-project-version generic fn

;; 0.0.19  1/10/22 migrate dump-nanoop-usage and dump-nanocontrol-symtab
;;                         defchip-pad defchip-pads -> pads.lisp 

;; 0.0.18  1/ 7/22 pad-defn, etc.
;;                 *debug-compiler*
;;                 compile-microcode compile-function compile-expression
;;                 run-assembler, run-assembler-pass*

;; 0.0.17  1/ 6/22 analyze-code now a generic function for the validator
;;                    (project must define)
;;                 add other generics for projects (see project-defs)
;;                 export accessors like opcode-fn so client code doesn't need to
;;                     use properties unless it's private
;;                 new macro: define-property-accessor

;; 0.0.16  1/ 4/22 move ucode-syntax to microlisp-int (was in
;;                     :scheme-79-mcr-i)
;;                 *register-control-wires*, register-sense-wires* ->
;;                     microlisp-int
;;                 validate-register-* -> microlisp-int 
;;                 *read-microcode-for-interpreter -> microlisp-int
;;                 *debug-validator*, *debug-precompiler* ->
;;                     fpga-project-defs
;;                 Add generate-cond-test as a generic function for
;;                     the validator to invoke (project must define)

;; 0.0.15 12/15/21 move export-ulisp-symbol fn to microlisp-int

;; 0.0.14 12/14/21 add :microlisp-shared package to avoid increasingly
;;                    verbose import-from :microlisp clauses
;;                 new *defumac-macros* *project-machine-pkg*
;;            

;; 0.0.13 12/ 9/21 *ulang-pkg* move to support/project-defs

;; 0.0.12 12/ 2/21 moving some defns from :scheme-mach to :fpga-pla-build-tools

;; 0.0.11 11-24-21 *note-output*

;; 0.0.10 11-23-21 moved to common: zerop-field, decrement-field and increment-field (from machine-defs.lisp)

;; 0.0.9  10-25-21 export pla-read

;; 0.0.8  10-21-21 start to migrate general PLA support here, including
;;                    compiler/assembler tools that can be specialized
;;                    by the projects. This is important both so we
;;                    can generate the HDL that will build the PLA and
;;                    to separate the tools needed to, e.g. compile
;;                    and load the microcode from the actual project
;;                    microcode or it's project-specific expression.

;; 0.0.7  10-12-21 shift-right fn moved to common.lisp

;; 0.0.6  09-20-21 move clock support init lists to :fpga-clocked

;; 0.0.5  09-16-21 moved a number of clock (phase) related fns and variables to :fpga-clocked

;; 0.0.4  09-14-21 add new packages fpga-combinatorics and fpga-clocked

;; 0.0.3  09-08-21 moved some diagnostic support material from
;;                    simulator/diagnostics-defs to fpga-diagnostics
;;                 moved grid-impl to gui/ in it's entirety

;; 0.0.2  09-07-21 moved rewind-stream to common

;; 0.0.1  Add debug-support, fpga-gui-support packages

;; 0.0.0  New - starting to separate general support for fpga
;;           (processor) development from the scheme-79 project itself

(defpackage :common ;; candidates for cl-lib?
  (:use :cl-lib common-lisp)
  (:export #:cadaddr #:rewind-stream #:duplicates-p 
           #:note #:note-if #:note-banner #:*note-output*
           #:define-property-accessor

           #:bit-vector->integer #:integer->bit-vector #:make-n-bit-vector #:bv-zerop #:copy-field
           #:shift-right #:zerop-field #:decrement-field #:increment-field

           ;; string ops
           #:center-pad-string #:make-pad
           
           ;; announcements
           #:print-semi-line #:announcement-banner #:*banner-length*

           ;; tabulated output
           #:print-tab-line #:setup-tabulated-output #:date-string

           ;; debug and partial implementation
           #:tbd #:*break-on-stub*
           ))

(defpackage :fpga-support
  (:use :common :cl-lib common-lisp)
  (:export
   #:fpga-support-version-reporter #:announce-fpga-support-version
   #:*fpga-support-initializations* #:initialize-fpga-support

   ;; common functions we should (re-)implement in the project as
   ;; needed; defined here so available everywhere
   #:reset #:power-on-reset))

(defpackage :fpga-project-defs ;; stuff to set up a specific project
  (:use :fpga-support :common :cl-lib common-lisp)
  (:export
   #:*fpga-project-initializations*
   #:announce-project-version
   
   #:*ulang-pkg* #:*ulang-shared-pkg* #:*project-machine-pkg*

   #:*debug-pad-timing* #:*debug-validator* #:*debug-precompiler* #:*debug-compiler*
   #:*debug-assembler*

   ;; important configuration parameters
   #:*word-size* #:*register-size*


   ;; generic functions the project should define methods for
   #:generate-cond-test #:analyze-code

   ;; generic functions the project can optionally define methods for
   #:clear-project-specific-tables
   #:initialize-project-specific-tables #:export-ulisp-symbol))

(defpackage :vhdl ;; VHDL generation
  (:use :fpga-project-defs :fpga-support :common :cl-lib common-lisp)
  (:export
   #:ensure-vhdl-stream #:*vhdl-stream*))

(defpackage :debug-support
  (:use :fpga-project-defs :fpga-support :common :cl-lib common-lisp)
  (:export
   ;; DSO module uses these (currently defined at project level) TBD
   #:record-pads-p #:record-busses-p #:record-pad #:record-bus

   ;; diagnostics module uses these (currently defined at project level) TBD
   #:diagnostics-running-p

   ;; these are defined here at the package level (to make it easy to
   ;; call from most fpga related packages), but actual implementation
   ;; is GUI dependent
   #:start-console #:start-dso #:reset-dso #:start-diagnostics 
   #:test
   ))

(defpackage :fpga-combinatorics
  (:use :debug-support :fpga-project-defs :fpga-support :common :cl-lib common-lisp)
  (:export
   ))
  
(defpackage :fpga-clocked
  (:use :fpga-combinatorics :debug-support :fpga-project-defs :fpga-support :common :cl-lib common-lisp)
  (:export
   ;; support for (symbolic) clock
   #:*tick* #:*ph1-clock-state* #:*ph2-clock-state* #:*symbolic-clock-phase* #:*phase-names*
   #:*total-clock-phases*

   #:configure-clock

   #:remap-phase-name #:get-end-phase #:next-phase #:phase-equal #:in-phase-interval-p

   ;; init lists
   #:*ph1-rising-list* #:*ph1-falling-list* #:*ph2-rising-list* #:*ph2-falling-list*
   #:*ph1-high-list* #:*ph2-high-list*
   #:*neutral1-list* #:*neutral2-list* #:*both-high-list* #:*all-ph-list*
   #:*all-ph-pre-update-list*
   #:init-list-name

   ;; simulation support fns
   #:execute-during-clock #:execute-once-during-clock #:execute-now-or-during-clock
   ))

(defpackage :fpga-plas
  (:documentation "Support for programmable logic arrays on the FPGA (essentially memory for firmware)")
  (:use :debug-support :fpga-project-defs :fpga-support :common :cl-lib common-lisp)
  (:export #:pla-read #:finalize-microcontroller-array
           ))

(defpackage :microlisp-shared
  (:documentation "language symbols we commonly have to import explicitly from microlisp pkg")
  (:import-from common-lisp t nil)
  (:export
   #:from #:to #:go-to #:branch #:branch-type #:from-type-const #:from-const #:tag
   #:sense-and-branch #:sense-type-and-branch ; nano-operations
   
   ;; also these symbols that should migrate here soon
   #:defschip #:defreg #:defreturn #:defmicromacro #:defpc

   ;; expect microcode file to define these
   #:**pointer-types** #:**non-pointer-types** #:**pointer** #:**machine-registers**

   ;; microlisp support AND helps define internal controls
   ))

;; the project microcode should live in this package (i.e. when
;; reading in microcode.mcr, it should be into this package).  if i
;; had done this correctly from the start, i would not have had to run
;; around and make all the cl:cond and cl:if uses explicit since there
;; are far fewer uses of scheme-79-mcr:if for instance. still it would
;; be a useful cleanup since in the future that's what we'll want to
;; do. (todo)

(defpackage :microlisp ; was :scheme-79-mcr
  (:documentation "Microlisp language symbols") 
  (:use :named-readtables :microlisp-shared)
  (:import-from common-lisp t nil)
  (:export
   ;; yeah, we have our own versions of these, but have to referenced explictly for the most part
   #:cond #:if #:progn

   #:deftype #:defmacro
   ))

(defpackage :microlisp-int
  (:documentation "General Microlisp support")
  (:use :microlisp-shared :debug-support :fpga-project-defs :fpga-support :common :named-readtables :cl-lib common-lisp)
  (:export
   #:ucode-syntax ; special readtable for microlisp ucode

   #:*register-control-wires* #:*register-sense-wires*
   #:*to-registers-used* #:*from-registers-used* #:*ucode-function-expansion-alist*

   #:validate-register-control #:validate-register-sense #:validate-register-name

   ;; toplevel validator / report functions
   #:read-microcode-for-interpreter
   #:generate-cond-test

   ;; toplevel compiler fns
   #:compile-microcode #:compile-function #:compile-expression
           
   ;;; database vars
   #:*registers-whose-types-are-tags*
   #:*boolean-terms*
   #:*ulisp-operations-alist* #:*ulisp-macro-alist* 
   #:*special-ucode-operations-alist*
   #:*internal-ucode-operations-alist*
   #:*control-wire-covering-sets-alist*

   ;;; database functions
   #:mark-register-use  #:create-ulopd #:create-ulmd
   #:reset-covering-set-alist #:declare-covering-set #:find-covering-set
   #:control-wires-for-register-op
   #:declare-register-control-wires #:declare-register-sense-wires
   
   ;; predicate support
   #:defupred-inverse #:invert-predicate

   ;; lookup functions (used after loading microcode!)
   #:ulisp-operator-descriptor #:opname #:numargs #:argtypes
   #:ulisp-macro-descriptor #:embedded-ops

   #:opcode-fn
   #:upred-p #:upred-desc #:ufun-p #:umac-p
   #:ucode-sense-wire #:ucode-pred-type #:ucode-pred-from-register #:ucode-pred-defn
   #:ucode-precedence #:ucode-constituent #:ucode-suppress-logging
   #:microcodes-used  

   #:microcode-symbol #:microcode-declarations
   #:symbol-table-elements #:predicates-used
   #:known-microfunction-p #:all-microfunctions
   #:pointer-type-name->int #:int->pointer-type-name
   #:non-pointer-type-name->int #:int->non-pointer-type-name
   #:type-name->int #:int->type-name

   ;; re-export
   #:**pointer-types** #:**non-pointer-types** #:**pointer** #:**machine-registers**
   ))

(defpackage :fpga-pla-build-tools
  (:documentation "Support for tools to compile/assemble code into PLAs, expandable in the project")
  (:use :microlisp-int :debug-support :fpga-project-defs :fpga-support :common :cl-lib common-lisp)
  
  (:export
   #:*upla-stream* #:*upla-file-name* #:*upla-suppress-annotation*
   #:upla-write #:upla-write-rtn #:upla-write-double-rtn #:upla-write-comment #:upla-write-header
   #:upla-write-code-annotation #:upla-write-code #:upla-write-tag #:upla-write-local-comment
   #:write-generated-code

   ;; parsing microlisp
   #:setup-ucode-expansion #:ucode-has-expansion-p 

   #:parse-to-address-term
   
   #:*from-register* #:*to-register* #:*expression-opcode* #:*enclosing-opcode* #:*constituent-assignment-fn*
   #:*function-being-compiled*
   #:*defumac-macros* #:*defupred-predicates*

   ;; defining microlisp operations, predicates, and macros
   #:defufn #:defumac #:defupred #:fail-tag #:success-tag #:assign-constituent-highlevel #:assign-constituent-lowlevel

   ;; assembling
   #:*nanocontrol-symtab* #:*nanocontrol-array*
   #:dump-nanocontrol-symtab #:dump-nanoop-usage
   
   #:*nanocontrol-array-bitvectors*
   #:microcontrol-symbol-value
   #:nanoop-symbol #:microop-symbol 
   #:finalize-nanocontroller-array #:create-microcontrol-jump-table

   #:run-assembler #:run-assembler-pass1 #:run-assembler-pass2 #:run-assembler-pass3
   #:run-assembler-pass4 #:run-assembler-pass5

   #:*microcontrol-symtab* #:*microcontrol-array*
   #:*microcontrol-annotations* #:*microcontrol-array-initial-size*
   #:*microcontrol-array-bitvectors*
   #:clear-microcontrol-array #:clear-annotations #:get-uc-annotation
   ))

(defpackage :fpga-registers
  (:use :debug-support :microlisp-int :fpga-project-defs :fpga-support :common :cl-lib common-lisp)
  (:export
   #:make-register #:*all-register-names*
   #:register-flags #:register-flag-accessor #:register-alias #:register-refs #:register-alist

   #:strip-register-name #:make-register-field-symbol

   #:latch-name

   ;; register definitions
   #:defchip-reg #:defchip-special-reg #:register-p #:valid-control-wires #:valid-sense-wires
   #:sense-wire-name #:sense-wire-register #:sense-wire-encoding

   #:set-control-wire-fn
   ))

(defpackage :fpga-pads
  (:use :fpga-registers :fpga-clocked :debug-support :fpga-project-defs :fpga-support :common :cl-lib common-lisp)
  (:export
   #:*output-pad-types* #:*input-pad-types* #:*all-pad-names* #:*all-bus-names*
   #:make-pads #:set-pad #:clear-pad #:test-pad #:test-pad-immediate
   #:pad-defn #:get-pad-defn-value

   #:defchip-pad #:defchip-pads
   ))

#+capi
(defpackage :fpga-gui-support
  (:use :debug-support :fpga-project-defs :fpga-support :common :capi :hcl :lispworks :cl-lib common-lisp)
  (:shadowing-import-from :common #:date-string)
  (:shadow #:accepts-focus-p) ; used in grid-imp
  (:export
   ;; these are from grid-impl.lisp (only exported symbols I've used)
   #:grid-data-row #:grid-widget #:grid-column-section #:grid-display-column
   #:grid-special-row #:grid-row-section

   #:header-title
   
   #:band-data-writer
   
   #:cell-data-object #:cell-displayer
   #:redraw-cell

   #:name #:top-level-widget))

(defpackage :fpga-diagnostics
  (:use :microlisp-int :fpga-project-defs :fpga-support :common :cl-lib common-lisp)
  
  (:export
   #:diagnostic-test-core #:test-name #:run-p #:test-run-p
   #:result #:test-result #:code #:test-code #:num-runs #:test-num-runs 
   #:result-interpreter #:test-result-interpreter
   #:num-successful #:test-num-successful #:num-failed #:test-num-failed

   #:set-test-run-p #:set-test-result

   #:diagnostic-test #:test-associated-groups #:test-conditional-p
   #:diagnostic-test-group #:group-test-number #:group-associated-tests

   #:create-diagnostic-test #:simple-find-test
   ))

