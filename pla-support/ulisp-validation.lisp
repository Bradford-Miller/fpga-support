(in-package :microlisp-int)

(fpga-support-version-reporter "FPGA PLA ulisp Validator" 0 1 3
                               "Time-stamp: <2022-01-18 12:35:26 gorbag>"
                               "cleanup special-register-p")

;; 0.1.2   1/18/22 cleanup obsolete code: removing special treatment of registers
;;                    which required multiple control lines for TO as new covering
;;                    set computation deals with it.

;; 0.1.1   1/14/22 call reset-covering-set-alist when initializing validator (retracted)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.2   1/ 6/22 add some specific package references
;;                 update property names

;; 0.0.1   1/ 5/22 &set-type must be referenced as
;;                     microlisp-shared::&set-type to avoid problems
;;                     later

;; 0.0.0   1/ 4/22 Remaining(?) code from ucode-validator repatriated here.
;;                 This should be mostly microlisp generic, with the project
;;                     setting up sepecific microcode (and nanocode).

;; OK this is NOT something MIT folks published (unfortuately), so I've had
;; to reverse-engineer based on the published microcode with the various
;; explainations in the papers.

;; Note that I origninally wrote the validator to make sure the OCR
;; and corrections to the published code were at least
;; consistant. Later this evolved into setting up some tables the
;; compiler would use as well.  As of 1/4/22 I'm (slowly) rewriting
;; this to both maintain a separation of "validation" of the microlisp
;; (which could, in theory, be used for syntax-directed editing), from
;; compilation as well as making the validator, compiler and assembler, etc. a
;; bit more table-driven / OOP style rather than having all the
;; embedded case statements.

;; NB: we side effect some symbols defined in the microcode such that
;; we can easily look up their definitions for simulation or the
;; console/debug capabilies. When we start to emit HDL (TBD) that
;; information would be lost (in the HDL) unless it is copied into a
;; PLA itself (we don't expect the FPGA to online validate its own
;; microcode, however, so this is more a note to myself about being
;; hygenic when building out the HDL production code).

#||
;; see also the (scheme-79:test) function which may be easier to type
(read-microcode-for-interpreter "src:scheme-79;simulator;microcode.mcr")
||#

;; so we have to read in the microcode definitions, and set up the
;; function to do so here. We define our own readtable to do this
;; (and package, which is defined in packages.lisp)

(defun read-microcode-for-interpreter (filename &optional (usage-stream *error-output*))
  "Reads the microcode for the lisp interpreter. This does NOT
assemble the microcode into a form that can be used to drive the chip
implementation. The intent is just to do a first-level check that the
microcode will work to interpret s-code."
  (let ((*package* *ulang-pkg*)
        (*readtable* (find-readtable 'ucode-syntax))
        (*read-base* 8) ; those MIT guys seem to like octal. Why not hex?!
        current-exp)

    (clear-macro-defs) ; start afresh (currently only handle ONE microcode file!)
    (clear-register-defs)
    (clear-schip-defs)
    (clear-type-dispatch)
    (clear-pc-dispatch)
    (clear-types-and-tags)
    (clear-usage)
    (clear-project-specific-tables)
    (fpga-pla-build-tools:clear-annotations)
    (fpga-pla-build-tools:clear-microcontrol-array)

    (with-open-file (stream filename :direction :input)
      ;; read expressions from the file and convert them to a
      ;; common-lisp representation suitable for simulation
      (while-not (equal (setq current-exp (read stream nil :eof nil))
                        :eof)
        ;; ok, depending on what form we read, dwir (do what is right)
        (note-if *debug-validator*
                 ";; read-microcode-for-interpreter: Interpreting ~S~%" current-exp)
        (ecase (car current-exp)
          ((defschip defreg)
           (eval current-exp) ; it's a macro with a simple expansion
           )
          (microlisp:deftype
           (do-deftype (cadr current-exp) (cddr current-exp)))
          (defreturn
           (do-defreturn (cadr current-exp) (cddr current-exp)))
          (defmicromacro ; (defmacro defmicromacro)
           ;; I think the distinction here is the MIT code presumed
           ;; the defmacro forms would be expanded by the lisp
           ;; interpreter, while the defmicromacro were intended for
           ;; the microcompiler. We're treating them both the same
           ;; here.

           ;; v 0.0.15 - note that defmacro fns (save/restore) now
           ;; translate into microcode so no longer should be treated
           ;; as macros, and rather get ignored here (treat them like
           ;; normal opcodes).
           ;;
           ;; see 'do-restore' nanofunction as described in the AIM!
           (setup-macro current-exp))   ; trickier. Can't use CL's
                                        ; defmacro because of funny
                                        ; comma notation. May have
                                        ; been a maclisp thing, I'm
                                        ; too lazy to check (1979
                                        ; predates common lisp which
                                        ; was 1984...)
          (microlisp:defmacro
              nil) ; just ignore it - we now treat the object as regular opcodes
          (defpc
           (do-defpc (cadr current-exp) (cddr current-exp))))))

    ;; some final checks
    (validate-types)
    (validate-tags)

    (report-types-and-tags usage-stream)

    ;; report our microcode's usage of opcodes
    (report-usage usage-stream)

    ;; set up nanocode anaphor table, etc.
    (initialize-project-specific-tables)
    ))


;; reporting functions

(let ((opcode-usage-alist nil)
      ;; separtely account from "macro" microcode (microcode written
      ;; in terms of other microcode)
      (opcode-internal-usage-alist nil)
      (pred-usage-alist nil)
      (pred-internal-usage-alist nil)
      (unknown-usage-alist nil))
  
  ;; Keep track of op usage (initially occurances in the microcode,
  ;; later, # calls in simulator; this version is potentially pretty
  ;; expensive - we'll later put a property on the symbol if needed
  ;; instead.

  (defun dump-usage ()
    (format *error-output* "Values: Opcode-usage Opcode-internal-usage Pred-Usage Pred-internal-usage Unknown-usage")
    (values opcode-usage-alist
            opcode-internal-usage-alist
            pred-usage-alist
            pred-internal-usage-alist
            unknown-usage-alist))
  
  (defun clear-usage ()
    (setq opcode-usage-alist nil)
    (setq opcode-internal-usage-alist nil)
    (setq pred-usage-alist nil)
    (setq pred-internal-usage-alist nil)
    (setq unknown-usage-alist nil)
    ;; also clear register usage statistics
    (mapc #'(lambda (reg)
              (setf (fpga-registers:register-alist reg) '((nil))) ; back to an empty alist
              (setf (fpga-registers:register-refs reg) 0))
          fpga-registers:*all-register-names*)
    (setq *to-registers-used* nil)
    (setq *from-registers-used* nil))

  (defun microcodes-used (&optional sort-p)
    (cl:cond
      (sort-p
       ;; have to combine first
       (let ((combined-usage-alist
               (append opcode-usage-alist unknown-usage-alist))) ; shoiuldn't have anything in common
         (mapc #'(lambda (internal-used)
                   (let* ((opcode (car internal-used))
                          (internal-usage (cdr internal-used))
                          (regular-usage (or (cdr (assoc opcode opcode-usage-alist)) 0)))
                     (update-alist opcode (+ internal-usage regular-usage) combined-usage-alist)))
               opcode-internal-usage-alist)
         (mapcar #'car (sort combined-usage-alist #'> :key #'cdr))))
      (t
       ;; can be hacky
       (mapcar #'car (append opcode-usage-alist opcode-internal-usage-alist unknown-usage-alist)))))

  (defun predicates-used (&optional sort-p)
    (cl:cond
      (sort-p
       (let ((combined-usage-alist (copy-list pred-usage-alist)))
         (mapc #'(lambda (internal-used)
                   (let* ((pred (car internal-used))
                          (internal-usage (cdr internal-used))
                          (regular-usage (or (cdr (assoc pred pred-usage-alist)) 0)))
                     (update-alist pred (+ internal-usage regular-usage) combined-usage-alist)))
               pred-internal-usage-alist)
         (mapcar #'car (sort combined-usage-alist #'> :key #'cdr))))
      (t
       (mapcar #'car (append pred-usage-alist pred-internal-usage-alist)))))

  (defun mark-used (term)
    (cl:cond
      ((ufun-p term)
       (mark-opcode-used term))
      ((upred-p term)
       (mark-pred-used term))
      (t
       (mark-unknown-used term))))
  
  (defun mark-opcode-used (op)
    (let ((usage-entry (assoc op opcode-usage-alist)))
      (update-alist op (cl:if usage-entry
                           (1+ (cdr usage-entry))
                           1)
                    opcode-usage-alist)))

  (defun mark-pred-used (op)
    (let ((usage-entry (assoc op pred-usage-alist)))
      (update-alist op (cl:if usage-entry
                           (1+ (cdr usage-entry))
                           1)
                    pred-usage-alist)))

  (defun mark-unknown-used (op)
    (when (eql op 'not)
      (break "NOT?!")) ; debugging
    (assert (and (not (upred-p op)) (not (ufun-p op))) (op) "Marking op ~s as unknown but it's known!" op)
    (let ((usage-entry (assoc op unknown-usage-alist)))
      (update-alist op (cl:if usage-entry
                           (1+ (cdr usage-entry))
                           1)
                    unknown-usage-alist)))

  (defun mark-used-internally (term)
    (cl:cond
      ((ufun-p term)
       (mark-opcode-used-internally term))
      ((upred-p term)
       (mark-pred-used-internally term))
      (t
       (mark-unknown-used term))))

  (defun mark-opcode-used-internally (op)
    (let ((usage-entry (assoc op opcode-internal-usage-alist)))
      (update-alist op (cl:if usage-entry
                           (1+ (cdr usage-entry))
                           1)
                    opcode-internal-usage-alist)))

  (defun mark-pred-used-internally (op)
    (let ((usage-entry (assoc op pred-internal-usage-alist)))
      (update-alist op (cl:if usage-entry
                           (1+ (cdr usage-entry))
                           1)
                    pred-internal-usage-alist)))

  (defun mark-register-usage-if-needed (op)
    (cl:if (fpga-pla-build-tools:ucode-has-expansion-p op) ; need to count those register references as well
        (let ((usage (cdr (assoc op *ucode-function-expansion-alist*))))
          (dolist (u usage)
            (mapc #'(lambda (x) (mark-register-use (car u) x)) (cdr u))))))

  (defun expanded-ucode-operations (op)
    (let*-non-null ((embedded-def (cdr (assoc op *ulisp-macro-alist* ;*embedded-ucode-operations-alist*
                                             ))))
       (embedded-ops embedded-def)))
  
  (defun mark-used-recursively (op)
    (mark-used op) ; first one gets normal mark
    (mapc #'mark-used-recursively-internal (expanded-ucode-operations op))
    (mark-register-usage-if-needed op))

  (defun mark-used-recursively-internal (op)
    (mark-used-internally op)
    (mapc #'mark-used-recursively-internal (expanded-ucode-operations op))
    (mark-register-usage-if-needed op))

  (defun report-usage (&optional (stream *error-output*))
    (print-semi-line stream)
    ;; sort the usage-alist
    (setq opcode-usage-alist (sort opcode-usage-alist #'> :key #'cdr)) ; see most used first
    (setq opcode-internal-usage-alist (sort opcode-internal-usage-alist #'> :key #'cdr))
    (setq pred-usage-alist (sort pred-usage-alist #'> :key #'cdr))
    (setq pred-internal-usage-alist (sort pred-internal-usage-alist #'> :key #'cdr))

    (flet ((ru (all-uses normal-alist internal-alist header-string defined-prop)

             (let* ((normal-count (substitute 0 nil (mapcar #'cdr normal-alist)))
                    (internal-count (substitute 0 nil (mapcar #'cdr internal-alist)))
                    (tab-sizes (setup-tabulated-output
                                5
                                (list (cons header-string
                                            all-uses)
                                      normal-count
                                      internal-count
                                      '("99") ; fake total
                                      '("    (unimplemented)")))))
               (princ ";;   " stream)
               (print-tab-line stream tab-sizes header-string "X" "I" "T" "")
               (terpri stream)
               (dolist (use all-uses)
                 (let ((usage (cdr (assoc use normal-alist)))
                       (iusage (cdr (assoc use internal-alist)))
                       (uusage (cdr (assoc use unknown-usage-alist)))
                       (defined-p (get use defined-prop)))
                   (princ ";;   " stream)
                   (print-tab-line stream
                                   tab-sizes
                                   use
                                   (or usage " ")
                                   (or iusage " ")
                                   (+ (or usage 0) (or iusage 0) (or uusage 0))
                                   (cl:if defined-p
                                       ""
                                       "    (unimplemented)"))
                   (terpri stream))))
             (print-semi-line stream)))
      (ru (microcodes-used t)
          opcode-usage-alist
          opcode-internal-usage-alist
          "Opcode usage (eXplicit, Implicit, Total):"
          :ulisp-ucode-fn)
      (ru (predicates-used t)
          pred-usage-alist
          pred-internal-usage-alist
          "Predicate usage (eXplicit, Implicit, Total):"
          :ulisp-ucode-pred))

    (flet ((register-title (register)                           
             (format nil "~a" register)))
      (let* ((header-string "register usage (Special registers don't have TO):")
             (tab-sizes (setup-tabulated-output
                         5
                         (list (cons header-string (mapcar #'register-title fpga-registers:*all-register-names*))
                               '("TOxx")
                               '("FROM")
                               '("TOTAL")
                               '("SPECIAL")))))
        (princ ";;   " stream)
        (print-tab-line stream tab-sizes header-string "TO" "FROM" "TOTAL" "SPECIAL")
        (terpri stream)

        (dolist (register fpga-registers:*all-register-names*)
          (princ ";;   " stream)
          (let ((to-refs (or (cdr (assoc :to-refs (fpga-registers:register-alist register)))
                             0))
                (from-refs (or (cdr (assoc :from-refs (fpga-registers:register-alist register))) 0))
                (total-refs (fpga-registers:register-refs register)))
            (print-tab-line stream tab-sizes
                            (register-title register)
                            to-refs
                            from-refs
                            total-refs
                            (- total-refs from-refs to-refs )))
          (terpri stream))))
    (format-justified-string ";; TO registers used"
                             (format nil "~s"
                                     *to-registers-used*)
                             *banner-length* stream)
    (format-justified-string ";; FROM registers used"
                             (format nil "~s"
                                     *from-registers-used*)
                             *banner-length* stream)
    (print-semi-line stream)
    (terpri stream)
    ))

;; types and tags. Note that both can designate a goto point in the code.

(let ((tags-defined nil)
      (tags-used nil)
      (types-defined nil)
      (pointer-types nil)
      (non-pointer-types nil)
      (last-pointer-type-number 0)
      (last-non-pointer-type-number 0))
  ;; keep track of types and general address tags we've seen so we can
  ;; validate at the end
  ; currently only using last-pointer-type-number...
  (declare (ignorable last-non-pointer-type-number)) 
  
  (defun clear-types-and-tags ()
    (setq tags-defined nil)
    (setq tags-used nil)
    (setq types-defined nil)
    (setq pointer-types nil)
    (setq non-pointer-types nil))

  (defun symbol-table-elements ()
    (values tags-defined types-defined pointer-types
non-pointer-types))

  (defun initialize-types-and-tags ()
    "Run after both **pointer-types** and **non-pointer-types** have
been defined"
    (unless pointer-types ; don't run twice
      (setq pointer-types (mapcar #'car **pointer-types**))
      (setq non-pointer-types (mapcar #'car **non-pointer-types**))
      ;; find highest non-pointer type, but ignore 7xx series
      (setq last-non-pointer-type-number (apply #'max
                                                (remove-if
                                                 #'(lambda (x) (>= x
                                                                   #o700))
                                                 (mapcar #'cadr
                                                         **non-pointer-types**))))
      (setq last-pointer-type-number (apply #'max
                                            (mapcar #'cadr
                                                    **pointer-types**)))))

  (defun tag-defined-p (tag)
    (not (null (member tag tags-defined))))

  (defun note-tag-defined (tag)
    (cl:cond
      ((tag-defined-p tag)
       (warn "Redefining tag ~A" tag))
      (t
       (setf (microcode-symbol tag) '((:type :tag))) ; make it easier on the compiler
       (push tag tags-defined))))

  (defun note-tag-used (tag)
    (pushnew tag tags-used))

  (defun tag-used-p (tag)
    (not (null (member tag tags-used))))

  (defun pointer-type-p (typename)
    (member typename pointer-types))

  (defun non-pointer-type-p (typename)
    (member typename non-pointer-types))

  (defun note-type-defined (typename)
    (cl:cond
      ((member typename types-defined)
       (warn "Redefining type ~A" typename)
       ;; undefine it then define it
       (setq types-defined (remove typename types-defined))
       (setf (microcode-symbol typename) nil)
       (note-type-defined typename))
      ((and (not (pointer-type-p typename))
            (not (non-pointer-type-p typename)))
       ;; presumably a micro return address, so make it a pointer type
       ;; as "The (micro) return address is stored as the type of the
       ;; stack pointer. Thus all micro return addresses must be
       ;; pointer types - something the compiler must know about!"
       (when *debug-precompiler*
         (warn "defining new type ~A as a pointer type (micro-return-address) #o~o!"
               typename (incf last-pointer-type-number)))
       (push typename types-defined)
       (setf (microcode-symbol typename) '((:type :micro-return-address))) ; make it easier on the compiler
       (update-alist typename (list last-pointer-type-number)
                     **pointer-types**))
      ((pointer-type-p typename)
       (setf (microcode-symbol typename) '((:type :pointer-type)))
       (push typename types-defined))
      (t
       (setf (microcode-symbol typename) '((:type :non-pointer-type)))
       (push typename types-defined))))

  (defun validate-types ()
    "Check that all types in **pointer-types** and
**non-pointer-types** have been defined"

    ;; we've already checked that the type was in those lists so just
    ;; need to check there is nothing left
    (let ((undefined-pointer-types (set-difference pointer-types types-defined))
          ;; gc-special-type won't show up as a deftype as it's internal to the gc
          (undefined-non-pointer-types
            (set-difference non-pointer-types (cons 'gc-special-type types-defined))))
      (when undefined-pointer-types
        (warn "Pointer types ~S were declared but not defined!"
              undefined-pointer-types))
      (when undefined-non-pointer-types
        (warn "Non-pointer types ~S were declared but not defined!"
              undefined-non-pointer-types))))

  (defun validate-tags ()
    "Check that all tags-used have been defined, and warn about tags
that were defined but not used"

    (let ((defined-not-used (set-difference tags-defined tags-used))
          ;; note that types can also be considered tags
          (used-not-defined (set-difference tags-used
                                            (union tags-defined types-defined))))

      (when defined-not-used
        (announcement-banner "TAGS DEFINED BUT NOT USED (NOT DANGEROUS)")
        (format *error-output* "~{;; ~s~%~}" defined-not-used)
        (print-semi-line))
      
      (when used-not-defined
        ;; use warn so we obey *break-on-warnings*
        (warn ";;;;;;;;;; TAGS USED BUT NOT DEFINED (POSSIBLE DANGER!)
;;;;;;;;;;;~%~{;; ~s~%~};;;;;;;;;;~%"
              used-not-defined))))

  (defun report-types-and-tags (&optional (stream *error-output*))
    (print-semi-line stream)
    (let* ((header-string "Defined Type usage:")
           (tab-sizes (setup-tabulated-output
                       2
                       (list (cons header-string types-defined)
                             (mapcar #'(lambda (x)
                                         (cadr (assoc :type
                                                      (microcode-symbol x))))
                                     types-defined)))))
      (princ ";; " stream)

      (print-tab-line stream tab-sizes header-string "Type" "Usage")
      (terpri stream)

      (dolist (type types-defined)
        (princ ";; " stream)
        (print-tab-line stream tab-sizes
                        type
                        (cadr (assoc :type (microcode-symbol type))))
        (terpri stream)))

    (terpri stream)
    (print-semi-line stream)
    (let* ((header-string "Defined Tags:")
           (tab-sizes (setup-tabulated-output
                       1
                       (list tags-used))))
      (princ ";; " stream)

      (print-tab-line stream tab-sizes header-string "Tag")
      (terpri stream)

      (dolist (tag tags-used)
        (princ ";; " stream)
        (print-tab-line stream tab-sizes
                        tag)
        (terpri stream))))
  )

(defun mark-register-use (type register-name)
  (cond-binding-predicate-to foo
   ((fpga-registers:register-alias register-name)
    (mark-register-use type foo))
   ((not (fpga-registers:register-p register-name))
    (error "~s does not name a register!" register-name))
   (t
    (incf (fpga-registers:register-refs register-name))
    (let ((temp nil)
          (type-key (cl:if (eql type :to)
                           :to-refs
                           :from-refs)))
      (update-alist type-key
                    (cl:if (setq temp (cdr (assoc type-key (fpga-registers:register-alist register-name))))
                           (1+ temp)
                           1)
                    (fpga-registers:register-alist register-name))
      (cl:if (eql type :to)
             (pushnew register-name *to-registers-used*)
             (pushnew register-name *from-registers-used*))))))

;; code validation by expression (which also sets up tables of types,
;; tags, etc. so has to be done at this point before we attempt to
;; compile)

;; this can probably be simplified (given the repeated code) but the
;; main thing is to have separate processing arms for the different
;; kinds of opcodes so we can eventually specialize the validation,
;; etc.

;; analyze-code should be implemented by the project!

(defun expand-and-validate-ucode (expression-list &optional (permissive nil))
  ;; since this is the top level, we can analyze the generated code
  ;; here without reanalyzing fragments
  (progfoo (expand-and-validate-ucode-internal expression-list permissive)
    (analyze-code foo)))

;; new generalized version that works with descriptors instead of lists
(defun expand-and-validate-by-descriptor (descriptor expression)
  (let ((processed-args 0))
    (validate-numargs expression (numargs descriptor))
    (when (argtypes descriptor)
      (do* ((arglist (cdr expression) (cdr arglist))
            (arg (car arglist) (car arglist))
            (typelist (argtypes descriptor) (cdr typelist))
            (type (car typelist) (car typelist)))
           ((or (endp arglist) (endp typelist)) nil)
        (case type
          (:register-name
           (validate-register-name arg)
           (incf processed-args))
          (:type-name 
           (if (and (member (cadr expression) *registers-whose-types-are-tags*) ; works for &set-type anyway
                    (symbolp arg))
               (note-tag-used arg)))
          (:tag
           (note-tag-used arg)))))
    ;; don't mark operators
    (unless (member (car expression) *boolean-terms*)
      (mark-used (car expression)))
    ;; also handle special case of &set-type *stack* as that has a tag
    ;;
    ;; not clear (yet) if this is something that should be part of
    ;; core microlisp (in which case the op should be set up already)
    ;; or part of the project (in which case this clause should be via
    ;; some kind of method we can extend within a project. (TBD). For
    ;; now, we'll just hack so it works.
    (when (and (eql (car expression) 'microlisp-shared::&set-type) ; may not be exported yet
               (member (cadr expression) *registers-whose-types-are-tags*)
               (symbolp (caddr expression))) ; only counts if it's a tag, not an evaluable expression
      (note-tag-used (caddr expression)))
    (let ((result (list (pop expression))))
      (dotimes (i processed-args)
        (push (pop expression) result))
      (nconc (nreverse result)
             ;; note that expression has processed elements popped off
             (expand-and-validate-ucode-internal expression t))))) 

(defun process-pair (pair)
  (cl:if (eq (car pair) t)
    (list 't
          (expand-and-validate-ucode-internal `(microlisp:progn ,@(cdr pair)) t))
    (list ; was list*
     (expand-and-validate-predicate-expression (car pair)) ; the predicate
     (expand-and-validate-ucode-internal `(microlisp:progn ,@(cdr pair)) t) ; the implicit progn
     )))

(defun process-pairs (pairs)
  (mapcar #'process-pair pairs))

(defun expand-and-validate-macro-ucode (desc exp)
  ;; similar to above, but may include additional ucodes that are
  ;; generated during compilation
  (validate-numargs exp (numargs desc))

  (case (car exp)
    (microlisp:cond
      ;; have to treat body as pairs
     (list* (car exp)
            (process-pairs (cdr exp))))
    (microlisp:if
        (list* (car exp)
               (expand-and-validate-predicate-expression (cadr exp))
               (mapcar #'(lambda (x)
                           (expand-and-validate-ucode-internal x t))
                       (cddr exp))))
    (t
     (when (member :tag (argtypes desc))
       ;; get the position (may be >1) and the correspoinding argument in the exp
       (mapc #'(lambda (arg type)
                 (when (eql type :tag)
                   (note-tag-used arg)))
             (cdr exp)
             (argtypes desc)))
           
     (mark-used-recursively (car exp))
     (list* (car exp) 
            (expand-and-validate-ucode-internal (cdr exp) t)))))

(defun expand-and-validate-predicate-expression (expression)
  "When we have a predicate expression, we want to expand it so we count the transformed uops"
  (let ((expanded-predicate (generate-cond-test expression 's-tag 'f-tag)))
    ;; we use the expanded-predicate only for metrics
    (mapc #'(lambda (op)
              (when (ufun-p (car op))
                (mark-used-recursively-internal (car op))))
          expanded-predicate)
    ;; and the original code for our results
    (expand-and-validate-ucode-internal expression t))) ; note that predicates could be simple

;; ok, this is kind of ugly, and at some point we want to generalize
;; this into a declaration-based (unification?) parser to make future
;; micro languages easier to specify.
(defun expand-and-validate-ucode-internal (expression-list permissive)
  "for each expression in expression-list, expand it if it is a macro, and validate it or the expansion result"
  (when expression-list
    (let ((exp (car expression-list))
          vexp)
      (note-if *debug-validator*
               ";; Validating: ~S~%" exp)
      
      (when (and (consp exp) (macro-symbol-p (car exp)))
        (note-if *debug-validator*
                 ";; Expanding macro ~s as ~s" (car exp) (expand-macro exp))
        (setq exp (expand-macro exp)))
      
      (cond-binding-predicate-to foo
        ((and permissive (not (consp exp)))
         (cl:cond
           ((upred-p exp)
            (mark-used exp)
            (mlet (sense-line pred-type from-register) (upred-desc exp)
              (declare (ignore sense-line pred-type))    
              (when from-register ; make sure we count it
                (cl:if (consp from-register) ; several
                  (mapc #'(lambda (x) (mark-register-use :from x)) from-register)
                  (mark-register-use :from from-register)))))
           ((ufun-p exp)
            (mark-used exp))
           ((or (fpga-registers:register-p exp) ; don't alert on registers
                (pointer-type-p exp) ; or known tags
                (non-pointer-type-p exp)
                (tag-defined-p exp)
                (tag-used-p exp)
                (member exp *boolean-terms*)) 
            nil) ; ignore it
           (t
            ;; mark as unknown but used
            (mark-unknown-used exp)
            (note-if *debug-validator*
                     ";; Ucode-Validator: ~s is being marked unknown-used as we can't classify it" exp)))
         
         (setq vexp exp))

        ((not (consp exp))
         (error "Expected the expression ~S to be a cons (in ~S)" exp expression-list))

        ((eql (car exp) 'tag)
         nil) ; ignore it

        ;; generalized version
        ((assoc (car exp) *ulisp-operations-alist*)
         (setq vexp (expand-and-validate-by-descriptor (cdr foo) exp)))
        
        ((assoc (car exp) *ulisp-macro-alist*)
         (setq vexp (expand-and-validate-macro-ucode (cdr foo) exp)))

        ((assoc (car exp) *internal-ucode-operations-alist*)
         ;; not much to do, but count the fn itself at least
         (setq vexp exp)
         (mark-used-internally (car exp)))
        (t
         (print-semi-line)
         (warn "do not recognize ~s for validation!" exp)
         (cl:if *debug-validator*
             (break "~s unrecognized" exp))
         (print-semi-line)))
      (cl:if vexp
        (cons vexp 
              (expand-and-validate-ucode-internal (cdr expression-list) permissive))
        (expand-and-validate-ucode-internal (cdr expression-list) permissive)))))

;; specific processing arms for some top-level constructs in microlisp
;; if implemented as a macro, it's in ulisp-defs!

;; deftype <type-name> <implicit-progn>
;;  each <type-name> should appear either in **pointer-types** or
;;  **non-pointer-types** and seems to associate code with handling
;;  the type of the object. (How we invoke that handler I'm still
;;  figuring out - dispatch is one mechanism as far as I can see)
;;  Typcially ends with a go-to.

(defun do-deftype (name body)
  ;; we will be interpreting the associated body so we want to do a quick check of it now. We also expand
  ;; macros at this point.
  (let ((ucode (expand-and-validate-ucode body)))
    (note-type-defined name)
    (update-alist :microcode-value ucode (microcode-symbol name))
    (update-alist name ucode *type-dispatch-alist*)))


;; defreturn <return-name> <implicit-progn>
;;  used to (&set-type *stack* <return-name>) and seems to be
;;  associated with processing the stack when doing a
;;  dispatch-on-stack for instance. I'm not sure how defreturn is
;;  different from deftype, other than type-names are in the type
;;  lists (associating them with bit representations) and returns
;;  aren't.

(defun do-defreturn (name body)
  ;; for now, it's the same as do-deftype other than a different alist
  (let ((ucode (expand-and-validate-ucode body)))
    ;; (note-tag-defined name) ; plist will get clobbered anyway
    (note-type-defined name) ; need to assign a type number to it
    (update-alist :microcode-value ucode (microcode-symbol name))
    (update-alist name ucode *type-return-dispatch-alist*)))

;; defpc <tag-name> <implicit-progn>
;;  seems to set up a tag (for a go-to <tag-name>) so is the
;;  equivalent of marking the program counter (defpc, pc stands for
;;  program counter?) with a label and is treated as such as far as I
;;  can see. Typically ends with a go-to itself. I'm guessing the high
;;  degreee of spaghetti (lots of go-tos) has to do with keeping the
;;  microcode as compact as possible, avoiding duplication.

(defun do-defpc (name body)
  ;; for now, it's the same as do-deftype< other than a different alist

  (let ((ucode (expand-and-validate-ucode body)))
    (note-tag-defined name)
    (update-alist :microcode-value ucode (microcode-symbol name))
    (update-alist name ucode *pc-dispatch-alist*)))
