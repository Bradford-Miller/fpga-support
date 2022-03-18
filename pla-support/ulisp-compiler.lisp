(in-package :microlisp-int)

(fpga-support-version-reporter "FPGA PLA ulisp Compiler" 0 2 0
                               "Time-stamp: <2022-03-18 15:11:50 gorbag>"
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

;; 0.1.2   2/ 2/22 use intentional upla fns

;; 0.1.1   1/27/22 cosmetics

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.1   1/11/22 use new announce-project-version generic fn

;; 0.0.0   1/ 7/22 Repatriating general/generic functionality from ucode-compiler.lisp and allowing
;;                    for project specialization where warrented.

(defgeneric compile-microcode (filename)
  (:documentation "Read the microcode for the chip. This goes beyond
the read-microcode-for-interpreter function in that it generates
binary microcode that can be executed by the chip simulator via the
nanocode (that is, it takes symbolic microcode and eventually compiles
it into sequences of binary microcode that are of the form:

ustate: <next-ustate> <nano-operation> <from> <to>

To do this it goes through an intermediate micro PLA symbolic verstion
that gets assembled into the final form just as in the AIM"))

(defgeneric compile-function (function-tag tag-type function-definition)
  (:documentation "compiles the microlisp function into micro PLA"))

;; for this release, this is the interface to project specific
;; code. Eventually, I plan to rewrite more of that code to make it
;; generic so the interface will be pushed down a couple levels of
;; abstraction than just the "expression".
(defgeneric compile-expression (expression)
  (:documentation "project-specific compilation of an expression (represented
  as a list) into code suitable for the assembler"))

(defmethod compile-microcode (filename)
  (let* ((*package* *ulang-pkg*)
         (*readtable* (find-readtable 'ucode-syntax))
         (*read-base* 8)
         ;; open up a temp file for our micro PLA
         (pathname (pathname filename))
         (fpga-pla-build-tools:*upla-stream* (hcl:open-temp-file 
                         :file-type "upla"
                         :prefix (setq fpga-pla-build-tools:*upla-file-name* (format nil "uPLA-~A-" (pathname-name pathname)))
                         :delete-when-close nil)))

    (announcement-banner (format nil "ulisp-compiler run ~a" (date-string)) fpga-pla-build-tools:*upla-stream*)
    (announce-project-version fpga-pla-build-tools:*upla-stream*) ; full versioning for now

    ;; prevalidate, set up tables, etc.
    (read-microcode-for-interpreter filename fpga-pla-build-tools:*upla-stream*)

    ;; ok the *type-dispatch-alist* *type-return-dispatch-alist* and
    ;; *pc-disptach-alist* should be set up with the various forms,
    ;; the anaphor table set up, etc. So what we do now is go back
    ;; through those and translate the forms (which would have macros
    ;; expanded already) into a binary representation suitable for
    ;; storing on-chip.

    ;; get some symbol information from the validator
    (mlet (tags-defined types-defined pointer-types non-pointer-types) (symbol-table-elements)
      (when *debug-compiler*
        (format fpga-pla-build-tools:*upla-stream* ";; compile-microcode: validation results --~%")
        ;; print each separtely using formatting so we get appropriate line breaks
        (format-justified-string ";;              Tags" (format nil "~s" tags-defined)
                                 *banner-length* fpga-pla-build-tools:*upla-stream*)
        (format-justified-string ";;             Types" (format nil "~s" types-defined)
                                 *banner-length* fpga-pla-build-tools:*upla-stream*)
        (format-justified-string ";;     Pointer-Types" (format nil "~s" pointer-types)
                                 *banner-length* fpga-pla-build-tools:*upla-stream*)
        (format-justified-string ";; Non-Pointer-Types" (format nil "~s" non-pointer-types)
                                 *banner-length* fpga-pla-build-tools:*upla-stream*))

      (unwind-protect
           (cl:progn
             (when *debug-compiler*
               (note "recording micro-PLA to ~s" (pathname fpga-pla-build-tools:*upla-stream*))
               (terpri *note-output*))
             ;; deal with simple tags first
             (dolist (tag types-defined)
               ;; right now, we don't deal with the kind of
               ;; compression we do with the nanocode (i.e. looking
               ;; for already defined functions with the same tail so
               ;; we can GOTO them). We can add that later if we need
               ;; to.
                 
               ;; Instead, each expression of the symbolic microcode is
               ;; transformed into a list of four elements by calling
               ;; the function associated with the microcode name
               ;; (created via the defufn macro). We can also get the
               ;; symbol's :s79-ucode-fn property - it should also be
               ;; that function. These can then be placed into the
               ;; ucode table
                 
               ;; but first we generate symbolic PLA which can then be
               ;; transformed into that representation but makes it
               ;; easier to deal with tags, conditionals, etc.
               (note "Compiling uLisp type function ~S ..." tag)
               (compile-function tag :type (cdr (assoc :microcode-value (microcode-symbol tag)))))
             (dolist (tag tags-defined)
               (note "Compiling uLisp tag function ~S ..." tag)
               (compile-function tag :tag (cdr (assoc :microcode-value (microcode-symbol tag)))))

             (fpga-pla-build-tools:run-assembler fpga-pla-build-tools:*upla-stream*)
             (fpga-plas:finalize-microcontroller-array)) ; should be ready to simulate or dump for FPGA
        (close fpga-pla-build-tools:*upla-stream*)))))

(defmethod compile-function (function-tag tag-type function-definition)
  "compiles the microlisp function into micro PLA"
  (let ((fpga-pla-build-tools:*function-being-compiled* function-tag))
    (fpga-pla-build-tools:upla-write-header "~a ~a:" tag-type function-tag)
    (fpga-pla-build-tools:upla-write-tag function-tag) ; put the tag in the file
    (dolist (expression function-definition)
      (let ((compiled-expression (compile-expression expression))) ; side effect fills in upla-stream
        (declare (ignore compiled-expression))
        ))))
