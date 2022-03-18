(in-package :fpga-project-defs)

(fpga-support-version-reporter "FPGA Dev Support Proj Defs" 0 2 0
                               "Time-stamp: <2022-03-18 15:08:29 gorbag>"
                               "line disambiguation")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.2.0   3/18/22 snapping a line: 0.2 release of library supports scheme-79 test-0 thru test-3 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.1.2   2/ 9/22 way too many things (fns, variables) with "line" in their name
;;                    and it's ambiguous.  Splitting so "line" refers to,
;;                    e.g. an output (log) line, "expression" refers to a
;;                    'line' of code (single expression in nano or microcode
;;                    land typically, and because we used (READ) it wasn't
;;                    confined to a single input line anyway) and "wire" to
;;                    refer to, e.g., a control or sense 'line' on a register.

;; 0.1.1   1/18/22 cleanup obsolete code: removing special treatment of registers
;;                    which required multiple control wires for TO as new covering
;;                    set computation deals with it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.8   1/11/22  new announce-project-version

;; 0.0.7   1/10/22  move export-ulisp-symol here and make generic

;; 0.0.6   1/ 7/22  move *debug-compiler*, *debug-assembler* here

;; 0.0.5   1/ 6/22  add additional generic functions to link to projects

;; 0.0.4   1/ 4/22  *debug-validator*, *debug-precompiler* -> here Add
;;                  generate-cond-test as a generic function for the
;;                      validator to invoke (project must define)

;; 0.0.3  12/14/21 add *ulang-shared-pkg*, *project-machine-pkg*

;; 0.0.2  12/ 9/21 move *ulang-pkg* defvar here (can be overridden in project)

;; 0.0.1  12/ 2/21 Add initialization for fpga-support package

;; 0.0.0          New

;; this contains variables and macros that help define a new project,
;; and should be bound/used within specific project files (loaded
;; after the fpga-support module). The intent is to make it easier to
;; describe some meta information about the project, so this is more
;; at the level of version control and documentation.

;; More specific macros and functions that can be used by projects to
;; construct a processor are defined in other files (e.g. how to
;; declare registers)

(defvar *fpga-project-initializations* nil)

(add-initialization "init fpga support"
                    '(initialize-fpga-support)
                    nil
                    '*fpga-project-initializations*)

;; these are variables that should be set in the project, preferably
;; during compilation as well as runtime

(defvar *ulang-pkg* (find-package :microlisp)  ; default to the microlisp package
  "package into which we should read microcode files")

;; we differentiate a shared package, because some symbols in the
;; :microlisp package collide with common-lisp the shared package
;; should be one that can be :use d without collision
(defvar *ulang-shared-pkg* (find-package :microlisp-shared)
  "package into which we should import ulisp functions, etc. so other packages will see them")

(defvar *project-machine-pkg* (find-package :common-lisp-user)
  "package into which machine specific defs should go (should be set by project!")

;; some common debug vars that could/should be set by the project
(defparameter *debug-pad-timing* nil
  "If non-nil, log timing related information for (external)
  pads (i.e. calls to set-pad and clear-pad)")

(defparameter *debug-validator* nil
  "be very verbose during validation of microcode to solve parsing
  issues")

(defparameter *debug-precompiler* t ; stuff used for precompilation
  "currently of limited use but generates warnings when making
  assumptions during validation")

(defparameter *debug-compiler* t
  "when non-nil some issues are logged instead of generating a
  warning (possible break) and is more verbose about validation
  results during setup")

(defparameter *debug-assembler* t
  "when non-nil we keep the assembler pass files instead of deleting
  them and record extra diagnostic information")



;; these are some generic functions the project should define methods for (we may not have any defined!)
(defgeneric generate-cond-test (test-clause success-tag fail-tag)
  (:documentation "This function will get called on each test-clause
                   inside of a COND form or the test for an IF form.
                   success-tag and fail-tag are symbols that the
                   produced code should use as next-instruction points
                   for the success or failure of the test. The
                   function should return a list of microcode
                   statements that cause the success-tag or fail-tag
                   to be reached, as appropriate, based on the outcome
                   of the test."))

(defgeneric analyze-code (expression-list)
  (:documentation  "Analyze code as a precompilation step and to enable encoding the
arguments to nano-operations (actual register usage in TO and FROM
fields, for instance)"))

;; these are generic functions the project is expected to specialize
;; (but we have defined at least one method, generally on
;; unspecialized parameters)

(defgeneric clear-project-specific-tables ()
  (:documentation "Any tables specific to the project that should be
cleared before we read (new) microcode should be done via a method on
this generic (generally a :before or :after or :around since there are
no parameters)"))

(defmethod clear-project-specific-tables ()
  )

(defgeneric initialize-project-specific-tables ()
  (:documentation "Any tables specific to the project that should be
initialized before we read (new) microcode should be done via a method on
this generic (generally a :before or :after or :around since there are
no parameters)"))

(defmethod initialize-project-specific-tables ()
  )

(defgeneric export-ulisp-symbol (sym)
  (:documentation "Sets up symbol to be read in from microlisp
  package (i.e. is part of mcr file OR generatated for assembler)"))

(defmethod export-ulisp-symbol (sym)
  (declare (type symbol sym))
  
  (import (list sym) *ulang-shared-pkg*)
  (export (list sym) *ulang-shared-pkg*))

(defgeneric announce-project-version (&optional stream version-only-p)
  (:documentation "project-specific version of announce-fpga-support-version"))

(defmethod announce-project-version (&optional (stream *error-output*) version-only-p)
  (announce-fpga-support-version stream version-only-p))
  
