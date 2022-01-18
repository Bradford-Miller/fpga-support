(in-package :fpga-combinatorics)

(fpga-support-version-reporter "FPGA Combinatorics Support" 0 1 0
                               "Time-stamp: <2022-01-11 13:49:46 gorbag>"
                               "0.1 release")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.0  9/14/21 New

;; The intent here is to have functions that support defining
;; combinatoric (unclocked) functions for implementation on the FPGA
;; through an HDL. This file has some overarching definitions and macros
