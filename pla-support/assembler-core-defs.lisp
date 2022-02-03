(in-package :fpga-pla-build-tools)

(fpga-support-version-reporter "FPGA PLA ulisp Assem. Defs" 0 1 1
                               "Time-stamp: <2022-01-27 14:36:33 gorbag>"
                               "allow microcontrol-symbol-value to check for errors")

;; 0.1.1   1/27/22 add error-p argument to microcontrol-symbol-value to
;;                    allow it call error if the argument doesn't have
;;                    a value in the microcontrol symbol table.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.2   1/10/22 Migrate nanocontrol dump fns here from upla-assembler.lisp
;;                    (since they're generic and don't interpret the contents)

;; 0.0.1   1/ 7/22 Move microcontrol-symbol-value here

;; 0.0.0  12/14/21 Starting transferring more portable assembler stuff
;;                      from simulator/compiler-defs and other files in support of a
;;                      generalized microlisp -> pla compiler/assembler

(defvar *nanocontrol-symtab* nil
  "alist of nanoinstruction symbols and entry points (offsets into the
  nanocontrol array)")

(defun nanoop-symbol (nanoop)
  (car (rassoc nanoop *nanocontrol-symtab* :test #'=)))

(defun dump-nanocontrol-symtab (output-stream)
  ;; dump nanocontrol symtab so we can check the values are correct
  (print-semi-line output-stream)
  (format output-stream ";;nanocontrol symtab (name sort):~%")
  (let ((tab-sizes (setup-tabulated-output
                    3
                    (list '(";;")
                          (mapcar #'car *nanocontrol-symtab*)
                          (mapcar #'(lambda (x) (format nil "~d" (cdr x))) *nanocontrol-symtab*)))))
    (dolist (entry (sort *nanocontrol-symtab* #'string-lessp :key #'car))
      (print-tab-line output-stream tab-sizes ";;" (car entry) (format nil "~d" (cdr entry)))
      (terpri output-stream))

    ;; redo but sorted by line
    (print-semi-line output-stream)
    (format output-stream ";;nanocontrol symtab (line sort):~%"))
  (let ((tab-sizes (setup-tabulated-output
                    3
                    (list '(";;")
                          (mapcar #'(lambda (x) (format nil "~d" (cdr x))) *nanocontrol-symtab*)
                          (mapcar #'car *nanocontrol-symtab*)))))
    (dolist (entry (sort *nanocontrol-symtab* #'< :key #'cdr))
      (print-tab-line output-stream tab-sizes ";;" (format nil "~d" (cdr entry)) (car entry))
      (terpri output-stream)))
    
  (print-semi-line output-stream))

(defun dump-nanoop-usage (nanoop-alist output-stream)
  ;; dump nanocontrol alist so we can check actual usage
  (print-semi-line output-stream)
  (format output-stream ";;nanoop usage (name sort):~%")
  (let ((tab-sizes (setup-tabulated-output
                    3
                    (list '(";;")
                          (mapcar #'car nanoop-alist)
                          (mapcar #'(lambda (x) (format nil "~d" (cdr x))) nanoop-alist)))))
    (dolist (entry (sort nanoop-alist #'string-lessp :key #'car))
      (print-tab-line output-stream tab-sizes ";;" (car entry) (format nil "~d" (cdr entry)))
      (terpri output-stream)))

  ;; redo but sorted by line
  (print-semi-line output-stream)
  (format output-stream ";;nanoop usage (line sort):~%")
  (let ((tab-sizes (setup-tabulated-output
                    3
                    (list '(";;")
                          (mapcar #'(lambda (x) (format nil "~d" (cdr x))) nanoop-alist)
                          (mapcar #'car nanoop-alist)))))
    (dolist (entry (sort nanoop-alist #'< :key #'cdr))
      (print-tab-line output-stream tab-sizes ";;" (format nil "~d" (cdr entry)) (car entry))
      (terpri output-stream)))
    
  (print-semi-line output-stream))

(defvar *nanocontrol-array* (make-array 0 :element-type t :adjustable t :fill-pointer t))

(defvar *nanocontrol-array-bitvectors* nil
  "when the nanocontrol array is finalized, we make an array of bitvectors")

;; of course, the microcontrol is an array too
(defparameter *microcontrol-array-initial-size* #o777
  "This is set to the maximum value allowed in the
  **non-pointer-types** declaration")

(defvar *microcontrol-symtab* nil
  "alist of microcontrol symbols and entry points (offsets into the
  microcontrol array)")

(defun microcontrol-symbol-value (symbol &optional error-p)
  "Look up the value of the symbol in *microcontrol-symtab*"
  (progfoo (cdr (assoc symbol *microcontrol-symtab*))
    (if (and error-p (null foo))
      (error "microcontrol-symbol-value: ~s not found in symtab!" symbol))))

(defun microop-symbol-tagtype (micropc)
  (some #'(lambda (symtab-entry)
            (when (and (= micropc (cdr symtab-entry))
                       (microcode-symbol (car symtab-entry)))
              (car symtab-entry)))
        *microcontrol-symtab*))

(defun microop-symbol-internal (micropc)
  "get first symbol found"
  (car (rassoc micropc *microcontrol-symtab* :test #'=)))

(defun microop-symbol (micropc)
  "prefer defined tags over temporary tags"
  (or (microop-symbol-tagtype micropc)
      (microop-symbol-internal micropc)))

(defvar *microcontrol-array* (make-array *microcontrol-array-initial-size*
                                         :element-type t :adjustable t :fill-pointer t))

(defun clear-microcontrol-array ()
  (setq *microcontrol-array* (make-array *microcontrol-array-initial-size*
                                         :element-type t :adjustable t :fill-pointer t))
  (setf (fill-pointer *microcontrol-array*) 0)) ; just to be sure

(defvar *microcontrol-annotations* nil
  "Like microcontrol-symtab, but contains symbolic information on how
  the content was generated (for, e.g., diagnostics and debugging)")

(defun clear-annotations ()
  (setq *microcontrol-annotations* nil))

;; mainly for console and debug notes
(defun get-uc-annotation (uaddr)
  (cdr (assoc uaddr *microcontrol-annotations*)))

(defvar *microcontrol-array-bitvectors* nil
  "when the microcontorl array is finalized, we make an array of bitvectors")
