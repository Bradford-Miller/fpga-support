(in-package :fpga-support)

(cl-lib:detailed-version-reporter "FPGA Dev Support Defs" 0 2 0
                                  "Time-stamp: <2022-03-18 15:26:01 gorbag>"
                                  "0.2 release"
                                  :initialization-list-symbol cl-user::*fpga-support-version-reporter-initializations*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.2.0   3/18/22 snapping a line: 0.2 release of library supports scheme-79 test-0 thru test-3 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.1  12/2/21 Add initialize-fpga-support fn

;; 0.0.0          New

;; The intention for this file is project support for the FPGA Support subsystem

(defmacro fpga-support-version-reporter (part-name  major-version minor-version micro-version date comments)
  `(cl-lib:detailed-version-reporter ,part-name ,major-version ,minor-version ,micro-version ,date ,comments 
                                     :initialization-list-symbol
                                     cl-user::*fpga-support-version-reporter-initializations*))

(defparameter *fpga-support-version-number* "0.2")

;; 0.2: support scheme-79 tests 0 thru 3. Mainly additional repatriation of code and
;;         some minor enhancements for debugging.

;; 0.1: support scheme-79 test-0 and test-1: much more segregation and
;;         repatriation including high level compiler and assembler,
;;         register support, microlisp internal and language packages,
;;         other pla tools, fpga-pad macros and functions, and
;;         defining some generic functions that can be used as entry
;;         points to more elaborated methods in the project code. I
;;         expect this process to continue until the project code is
;;         mostly declarative or specialized to the 'project value
;;         adds' and all core processor definition simulation and HDL
;;         generation is in this library.

;; 0.0: starting segregation of code from scheme-79 packages

(defvar *fpga-support-initializations* nil)

(defun announce-fpga-support-version (&optional (stream *error-output*) version-only-p)
  (announcement-banner (format nil "FPGA SUPPORT VERSION ~A" *fpga-support-version-number*)
                       stream)
  (unless version-only-p
    (let ((*error-output* stream))
      (report-version nil 'cl-user::*fpga-support-version-reporter-initializations*)))
  (terpri stream))

(defun initialize-fpga-support ()
  "Runs initializations for the FPGA Support package"
  (initializations '*fpga-support-initializations* t))

