(in-package :fpga-clocked)

(fpga-support-version-reporter "FPGA Clock Simulator" 0 1 0
                               "Time-stamp: <2022-01-11 13:47:28 gorbag>"
                               "0.1 release")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.0  12/14/21 moved functions from machine-predefs etc. where
;;                    there weren't specific dependencies on the
;;                    scheme-79 simulation

;;; set up macros to produce appropriate add-initialization forms
;;; depending on the clock phase it should be active, etc. Note that
;;; this MIGHT get used for generation of HDL as well, but at this
;;; point it's purely simulative.

(defmacro execute-during-clock ((test-name-string test-during) &body body)
  "sets up a delay until the specified clock state rolls around and
then executes the body. NB: this does NOT check for the current clock
state so if the specified state is equal to the current state a full
major cycle will run before execution."
  (let ((initialization-list-name (init-list-name (symbol-value test-during)))
        )
    `(add-initialization ,test-name-string
                         '(cl:progn ,@body)
                         nil
                         ',initialization-list-name)))

(defmacro execute-once-during-clock ((test-name-string test-during &optional defer-test-p) &body body)
  "Like execute-during-clock but only runs the body once, the first
time the clock constraint is met (rather than every time the clock
spec holds). If defer-test-p is non-nil, we skip the next occurance of
the clock phase and run it the one after that."
  (let* ((initialization-list-name (init-list-name test-during))
         (next-phase-init-list (init-list-name (next-phase test-during))))

    (cl:if defer-test-p
        `(add-initialization ,test-name-string
                             '(cl:progn
                               (note-if *debug-pad-timing*
                                "[in phase ~s] running deferral of ~s" *symbolic-clock-phase* ',body)
                               ;; delete first as we will use same name string
                               (delete-initialization ,test-name-string nil ',next-phase-init-list)
                               (execute-once-during-clock (,test-name-string ,test-during nil) ,@body))
                             nil
                             ',next-phase-init-list)
                               
        `(add-initialization ,test-name-string
                             '(cl:progn
                               ,@body
                               (delete-initialization ,test-name-string nil ',initialization-list-name))
                             nil
                             ',initialization-list-name))))

(defmacro execute-now-or-during-clock ((test-name-string test-during valid-phases) &body body)
  "similar to execute-once-during-clock but DOES check the current clock
state, and if the specified state is current or within the
valid-phases after that (0 means the current state only), then the
body is executed immediately and not deferred. Execution-done is always performed."
  (let ((captured-fn (gensym)))
    `(flet ((,captured-fn () ,@body))
       (cl:cond
         ((in-phase-interval-p *tick* ,test-during ,valid-phases)
          (,captured-fn))
         (t
          (when *debug-pad-timing*
            (warn "~a being executed out of phase (expected phase: ~s or ~d after). Deferring until ~s"
                  ,test-name-string ,test-during ,valid-phases ,test-during))
          (execute-once-during-clock (,test-name-string ,test-during) (,captured-fn)))))))

