(in-package :fpga-pla-build-tools)

(fpga-support-version-reporter "FPGA PLA Build Tool Defs" 0 1 1
                               "Time-stamp: <2022-02-02 17:01:04 gorbag>"
                               "intention-related upla-write fns")

;; 0.1.1   2/ 2/22 add intention-related upla-write fns

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.0  12/ 2/21 Starting transferring more portable stuff
;;                      from simulator directory in support of a
;;                      generalized microlisp

;; see compiler-core-defs.lisp for microlisp definitions, and ??? for microPLA definitions

(defvar *upla-stream* nil
  "Allow a stream for recording what we are doing while compiling or
assembling (will be bound appropriately during the various tool
passes). The intent is that each pass generates an intermediate file
used as the input to the next pass to facilitate debugging. (This
isn't particularly efficient, but since compiling microlisp or
microPLA should be relatively rare, it seems a good choice until the
tools are completely stable.)")

(defvar *upla-file-name* nil
  "Bind to the file-name *upla-stream* points to.")

;; functions to write into the *upla-stream*
(defun upla-write (object)
  (when *upla-stream*
    (let ((*print-escape* nil))
      (write object :stream *upla-stream* :pretty nil :level nil :lines nil :length nil))))

(defun upla-write-rtn (object)
  (upla-write object)
  (terpri *upla-stream*))

(defun upla-write-double-rtn (object)
  (upla-write-rtn object)
  (terpri *upla-stream*))

;; insert comments into the u-pla file to make it easier to follow
(defun upla-write-comment (control &rest args)
  (upla-write (format nil "~%;; ~?~%" control args)))

(defun upla-write-header (control &rest args)
  ;; fancier comment to make it easier to find
  (upla-write (format nil "~%;;;~%;;; ~?~%;;;~%~%" control args)))

;; the intent here is to have intention-related fns that help us know
;; why we are writing into the pla-stream (e.g. for debugging)

(defvar *upla-suppress-annotation* nil
  "can bind to t to supress (recursive) annotation")

(defun upla-write-code-annotation (code-or-string)
  (upla-write-comment "~s:" code-or-string))

(defun upla-write-code (code)
  (upla-write-rtn code))

(defun upla-write-tag (tag)
  (upla-write-rtn tag))

(defun upla-write-local-comment (code-or-string)
  "Similar to write-code-annotation but ends in --- to prevent loading into annotation database"
  (upla-write-comment "~s---" code-or-string))

(defun write-generated-code (stream input code &optional debug)
  (when (and stream code) ; some internal generation calls suppress stream to keep from writing yet
    (let ((*upla-stream* stream))
      (when input (upla-write-code-annotation input)) ; may be calling just to generate output
      (upla-write-code (car code)))
    ;; I used to assert on this but there's too many low level things
    ;; that generate >1 line, esp. when we load the bus before doing a
    ;; test...
    (unless (endp (cdr code)) 
      (upla-write-comment "NB: multiple codes generated") ; generally not what we want so note it
      (write-generated-code stream input (cdr code) debug)))
  code)
