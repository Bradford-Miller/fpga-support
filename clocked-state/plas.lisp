(in-package :fpga-plas)

(fpga-support-version-reporter "FPGA PLA Support" 0 2 0
                               "Time-stamp: <2022-03-18 15:09:51 gorbag>"
                               "0.2 release")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.2.0   3/18/22 snapping a line: 0.2 release of library supports scheme-79 test-0 thru test-3 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.2  12/06/21 move finalize-microcontroller-array here

;; 0.0.1  10/25/21 pla-read

;; 0.0.0  10/21/21 place to separate out the code needed to define and
;;                     use a PLA by the project so it can be compiled
;;                     into HDL as well as simulated

;; So CL has a number of "ordinary functions" that we can't just write
;; methods for. FPGAs will represent most "integers" as bit-vectors
;; (registers - arrays of flip flops) so for ease of conversion of
;; code into HDL, we want to work with such bit vectors as much as
;; possible, which also helps us parse out functions that can be
;; easily transformed into HDL, e.g. accessing things in a programmable
;; logic array.

;; in CL we'd use aref to pull something out of an array, with an
;; integer index.  here we have an equivalent function to pull
;; something out of a pla using a bit-vector index. (What gets pulled
;; out should itself be a bit-vector, of course, but we'll ignore that
;; for now since we're transitioning to a hardware-optimized
;; description)

(defun pla-read (array index)
  (declare (type bit-vector index))

  ;; for the purposes of simulation we can convert this to a simple CL function call
  (aref array (bit-vector->integer index)))

;; finalize array (prep for installing into FPGA)

;;; convert to bitvector representation
(defgeneric finalize-microcontroller-array ()
  (:documentation "Similar to the nanocontroller version, convert the
tuple representation of microcode to fixed size bit arrays suitable
for loading into the FPGA."))


(defmethod finalize-microcontroller-array ()
  ;;(tbd) ; no need to have a break here
  (values))
