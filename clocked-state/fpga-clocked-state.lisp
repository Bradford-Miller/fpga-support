(in-package :fpga-clocked)

(fpga-support-version-reporter "FPGA Clocked State Support" 0 2 0
                               "Time-stamp: <2022-03-18 15:09:02 gorbag>"
                               "0.2 release")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.2.0   3/18/22 snapping a line: 0.2 release of library supports scheme-79 test-0 thru test-3 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.0  9/14/21 New

;; The intent here is to have functions that support defining clocked
;; state (vs. combinatoric) functions for implementation on the FPGA
;; through an HDL. Note that MOST functions on an FPGA will be clocked
;; because it's the only way to insure consistency (lack of race
;; conditions). This file has some overarching definitions and macros

;; we can consider all clocked systems to have a "tick" for the major
;; cycle. Obviously most if not all systems will actually count the
;; number of ticks, but we do so to make it easier to debug (e.g. we
;; can record state on every tick).

(defvar *tick* 0
  "Major clock cycle since simulation start")

;; more clock definitions are in fpga-clocks.lisp
