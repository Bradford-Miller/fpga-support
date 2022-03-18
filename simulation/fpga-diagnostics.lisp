(in-package :fpga-diagnostics)

(fpga-support-version-reporter "FPGA Dev Diag Support" 0 2 0
                               "Time-stamp: <2022-03-18 15:13:13 gorbag>"
                               "0.2 release")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.2.0   3/18/22 snapping a line: 0.2 release of library supports scheme-79 test-0 thru test-3 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.1  10/12/21 set initform for test-result in case we bring up after run

;; 0.0.0   9/ 8/21 Split a number of vars and functions from
;;                  simulator/diagnostics-defs to here (the more
;;                  universal stuff)

;; The intent here is to create a basic capability for tracking
;; diagnostics, i.e., tests (that can be validated) around a
;; particular fpga processor project.
;;
;; a diagnostic-test tests or documents the result of a test for a
;; particular feature, and a test group is usually a higher level test
;; (such as a program) that tests a number of features.


(defclass diagnostic-test-core ()
  ((name :reader test-name
         :initarg :name)
   (run-p :initform nil :reader test-run-p) ; generate our own setf function
   (result :initform :unknown :reader test-result) ; same
   ;; result shows the result of the last run
   (code :initarg :code
         :reader test-code) ; code to run the test (if available)
   ;; e.g. (code) should run a test. See the comments for test
   ;; functions to allow this to be automated.
   (result-interpreter :initarg :result-interpreter
                       :reader test-result-interpreter) ; code to interpret the result of the test (if available)
   ;; e.g. (result-interpreter (code)) should run a test and interpret
   ;; the result allowing us to set the result slot. See the comments
   ;; for result-interpreter functions to allow this to be automated.
   (num-runs :initform 0
             :accessor test-num-runs)
   ;; num-runs if we want to collect statistics, shows the number of times the diagnostic ran, etc.
   (num-successful :initform 0
                   :accessor test-num-successful)
   (num-failed :initform 0
               :accessor test-num-failed)
   ))

(defun set-test-run-p (i v)
  (setf (slot-value i 'run-p) v)
  (when v
    (incf (test-num-runs i))))

(defsetf test-run-p set-test-run-p)

(defun set-test-result (i v)
  (setf (slot-value i 'result) v)
  (if v
      (setf (test-num-successful i) (- (test-num-runs i) (test-num-failed i)))
      (setf (test-num-failed i) (- (test-num-runs i) (test-num-successful i)))))

(defsetf test-result set-test-result)

(defclass diagnostic-test (diagnostic-test-core)
  ((associated-groups :initform nil
                      :initarg :associated-groups
                      :accessor test-associated-groups)
   (conditional-p :initform nil
                  :initarg :conditional-p
                  :reader test-conditional-p)
   ;; In our immediate use case, the test is on a particular micro or
   ;; nano instruction, and the associated tests are those tests in
   ;; the test library that use that instruction.
   ))

(defclass diagnostic-test-group (diagnostic-test)
  ((number :initform 0
           :initarg :number
           :accessor group-test-number)
   (associated-tests :initform nil
                     :initarg :associated-tests
                     :accessor group-associated-tests)))

(defun create-diagnostic-test (test-name var-name)
  (set var-name
       (append (symbol-value var-name)
               (list (make-instance 'diagnostic-test
                                    :name test-name)))))

(defun simple-find-test (test-name test-list)
  (find (string test-name)
        (cond
          ((consp test-list)
           test-list)
          (t
           (return-from simple-find-test nil)))
        :test #'equalp
        :key #'(lambda (x) (string (test-name x)))))
