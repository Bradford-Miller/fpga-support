(in-package :fpga-pads)

(fpga-support-version-reporter "FPGA Pad Support" 0 2 0 
                               "Time-stamp: <2022-03-18 15:09:36 gorbag>"
                               "0.2 release")

;; support for pads

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.2.0   3/18/22 snapping a line: 0.2 release of library supports scheme-79 test-0 thru test-3 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.1   1/ 7/22 Add pad-defn property accessor

;; 0.0.0  12/ 3/21 New: moved from various simulation files,
;;                    e.g. machine-defs

(defvar *input-pad-types* '(:input :io :latched-io)
  "list of defchip-pad types that accept an external input")

(defvar *output-pad-types* '(:io :latched-io :output)
  "list of defchip-pad types that create an external output")

(defvar *all-pad-names* nil
  "list of pads defined with defchip-pad")

(defvar *all-bus-names* nil
  "list of (external) busses defined with defchip-pads")

(defun make-pads (n)
  "create a set of pads. Like registers but tied to pads on the chip"
  (make-n-bit-vector n))

(defgeneric set-pad (name)
  (:documentation "only for output or io pads")
  )

(defgeneric clear-pad (name &optional already-checked-p)
  (:documentation "only for latched output pads")
  )

(defgeneric test-pad (name &key callback-fn retest-until-p test-immediate-p)
  (:documentation "only for input or io pads")
  )

(defun test-pad-immediate (pad-name)
  (test-pad pad-name :test-immediate-p t)) ; access internal tester with special flag

(define-property-accessor pad-defn :pad-defn)

(defun get-pad-defn-value (pad key)
  "Lookup the value of the key on the pad-defn property"
  (cdr (assoc key (pad-defn pad))))

;; these macros currently expand into code to implement the
;; simulation, but will eventually also produce (HDL) code (into a
;; file?) for programming the FPGA

(defun create-test-pad-method (name test-during valid-for original)
  (declare (ignore original) ; should use these too to determine if we can test immediately, and if there's an alias we can use
           (special *symbolic-clock-phase*)) ; to avoid the warning
  
  `((defmethod test-pad ((name (eql ',name)) &key callback-fn retest-until-p test-immediate-p)
      "If no callback-fn is specified, then the test is immediate
providing we are in the correct clock phase and otherwise generates a
warning and terminates false. If there is a callback-fn then if
retest-until-p is nil (the default) we wait until the phase specified
in test-during is reached and then check, calling the callback
function (which should accept two parameters: the name of the pad and
the value) with the value at that time. If retest-until-p is true,
however, then we continue to test the pad until it does return true
and then invoke the callback function (which can ignore the second
parameter as it will always be non-nil). This is useful, e.g., for
monitoring the *interrupt-request* pad."
      (cl:cond
        ((and callback-fn retest-until-p)
         ;; if we're in the right phase AND the test holds, call
         ;; the callback-fn and return. Otherwise post an
         ;; initialization starting with the requested phase and
         ;; retesting each phase thereafter until we get a hit.
         (cl:cond
           ((and (not (zerop (bit ,name 0)))
                 (in-phase-interval-p *tick* ,test-during 0))
            (funcall callback-fn ',name t))
           ;; no, so add an initialization
           (t
            ,@(let ((initialization-name (format nil "TEST-~A-CONTINUALLY" (string name))))
                `((when (and (not (zerop (bit ,name 0))) *debug-pad-timing*)
                    (warn "call to test-pad ~s possibly out of phase (~s), deferring (until phase: ~s) and continually testing"
                          ',name *symbolic-clock-phase* ,test-during))
                  (execute-during-clock (,initialization-name ,test-during)
                    (cl:cond ((not (zerop (bit ,name 0)))
                           (delete-initialization ,initialization-name nil (init-list-name ,test-during)) ; (execution-done)
                           (funcall callback-fn ',name t))
                          (t
                           ;; keep trying every phase change:
                           (execute-during-clock (,initialization-name :all)
                              (when (not (zerop (bit ',name 0)))
                                (delete-initialization ,initialization-name nil (init-list-name ,test-during)) ; (execution-done)
                                (funcall callback-fn ',name t)))))))))))
        (callback-fn
         ;; post an initialization to kick off when the clock
         ;; phase obtains which will then call the
         ;; callback-fn. Note that if we are already in the
         ;; correct phase, we should call the callback-fn
         ;; immediately.
         ,(let ((initialization-name (format nil "TEST-~A" (string name))))
            `(execute-now-or-during-clock (,initialization-name ,test-during ,valid-for)
                (funcall callback-fn ',name (not (zerop (bit ,name 0)))))))

        ;; immediate test only. Are we in the correct phase?
        ((or test-immediate-p
             (in-phase-interval-p *tick* ,test-during ,valid-for))
         (not (zerop (bit ,name 0))))
        
        ,@(unless (member test-during '(:all :any))
            `(((and *debug-pad-timing* (not test-immediate-p))
               (warn "[in phase ~s] call to test-pad ~s out of phase (expected phase: ~s or ~d after)"
                     *symbolic-clock-phase* ',name ,test-during ,valid-for)))))
      )))

(defun delete-clear-pad-from-prior-tick (clear-initialization-name)
  ;; if we are setting a pad that is scheduled to be cleared RIGHT NOW, remove the clear. 
  (delete-initialization clear-initialization-name nil (init-list-name *symbolic-clock-phase*)))

(defun create-set-pad-methods (name type assert-during valid-for original)
  ;; if type = :latched-output, then we have to explicitly call clear-pad (no auto-clear)
  (declare (ignore original)) ;for now
  (let* ((latch (cl:if (member type '(:latched-output :latched-io)) (latch-name name)))
         (initialization-name (format nil "SET-PAD-~A" (string name)))
         (clear-initialization-name (format nil "CLEAR-PAD-~A" (string name))))
    (mlet (clear-phase clear-phase-defer-tick-p)
        (get-end-phase assert-during valid-for)
          

     `((defmethod set-pad ((name (eql ',name)))
         ;; ok, if current-phase is equal to assert-during, we can set
         ;; and then setup a clear (unless latched) If not, we want to
         ;; defer the work until that phase

         (cl:cond
          ((or (eql ,assert-during :any)
               (phase-equal ,assert-during *symbolic-clock-phase*))
           (setf (bit ,name 0) 1)
           (delete-clear-pad-from-prior-tick ,clear-initialization-name) ; in case we were scheduled to also clear this pad
           ,@(cl:if latch
                 `((setq ,latch (cons *tick* *symbolic-clock-phase*)))
                 ;; not latched, so also set up clear
                 `((note-if *debug-pad-timing*
                            "[in phase ~s] set-pad ~a, setting up clear-pad for phase ~s (nt: ~s)"
                            *symbolic-clock-phase* ',name ,clear-phase ,clear-phase-defer-tick-p)
                   (execute-once-during-clock (,clear-initialization-name ,clear-phase ,clear-phase-defer-tick-p)
                     (clear-pad ',name t)))))
          (t
           (when *debug-pad-timing*
             (warn "call to set-pad ~a out of phase (~s), deferring until phase: ~s"
                   ',name *symbolic-clock-phase* ',assert-during))
           (execute-once-during-clock (,initialization-name ,assert-during)
              (note-if *debug-pad-timing*                   
                       "[in phase ~s] setting deferred pad ~s" *symbolic-clock-phase* ',name)
              (setf (bit ,name 0) 1)
              (delete-clear-pad-from-prior-tick ,clear-initialization-name) ; in case we were scheduled to also clear this pad
              ,@(cl:if latch
                    `((setq ,latch (cons *tick* *symbolic-clock-phase*)))
                    ;; not latched, so also set up clear (do it here so we defer the clear until the following cycle)
                    `((note-if *debug-pad-timing*
                               "[in phase ~s] deferred set-pad ~a, setting up clear-pad for phase ~s (nt: ~s)"
                               *symbolic-clock-phase* ',name ,clear-phase ,clear-phase-defer-tick-p)
                      (execute-once-during-clock (,clear-initialization-name ,clear-phase ,clear-phase-defer-tick-p)
                        (clear-pad ',name t))))))))
      
      (defmethod clear-pad ((name (eql ',name)) &optional already-checked-p)
        ;; if we're calling after the minimum valid time for the pad, just do it.
        (note-if *debug-pad-timing*
                 "[in phase ~s] clear-pad ~a (checked-p: ~s)"
                 *symbolic-clock-phase* ',name already-checked-p)
        (cl:cond
          (already-checked-p
           (setf (bit ,name 0) 0))
          ((eql (bit ,name 0) 0)) ; already clear 9/16/21 - if we
                                  ; clear a latched signal that is
                                  ; already clear, then we won't have
                                  ; a valid value on the latch, so we
                                  ; just ignore it.
          ,@(cl:if latch ; latch should be a list for the tick and
                         ; phase when it was set
                ;; one less than valid-for so if we are end of interval we're ok to clear
              `(((in-phase-interval-p  (car ,latch) (cdr ,latch) (1- ,valid-for)) 
                 ;; Too soon, have to defer
                 (when *debug-pad-timing*
                   (warn "clear-pad ~a, incorrect phase ~s, deferring until phase ~s (nt: ~s)"
                         ',name *symbolic-clock-phase* ,clear-phase ,clear-phase-defer-tick-p))
                 (execute-once-during-clock (,clear-initialization-name ,clear-phase ,clear-phase-defer-tick-p)
                   (note-if *debug-pad-timing*
                            "[in phase ~s] running deferred clear-pad ~a" *symbolic-clock-phase* ',name)
                   (setf (bit ,name 0) 0)))
                (t
                 (note-if *debug-pad-timing*
                          "CP: clearing latched pad ~A t: ~d ph: ~s (set t: ~d ph: ~s)"
                          ',name *tick* *symbolic-clock-phase* (car ,latch) (cdr ,latch))
                 (setf (bit ,name 0) 0)))
              ;; not latched, not checked (all vetted calls are checked), seems strange
              `((t
                 (break "unlatched pad ~A being cleared outside of set-pad initialization" ',name)
                 (setf (bit ,name 0) 0))))))))))

(defun setup-pad-support (name type assert-during test-during valid-for original)
  "because pads should for the most part be synchronous with the
  clock, this sets up initializations to set (or clear) a pad as an
  output or read the pad as an input but delay doing so until the
  approprate clock (and to hold through the valid-for period extended
  by freeze if needed). Because we want these to be compiled in as
  part of the defchip-pad macro, it returns code to do this!"
  ;; if original is non-nil, it's the name of the pad set that is
  ;; being shared so we can tie into that.

  ;; return all the needed method definitions
  `(
    ;; is this an input pad? (treat outputs as input pads so we can
    ;; use the test-pad method)
    ,@(when t ;(member type '(:io :latched-io :input))
        (create-test-pad-method name test-during valid-for original))

    ;; is this an output pad? (treat inputs as output pads so we can
    ;; use the set-pad method from the front-panel)
    ,@(when t
        (create-set-pad-methods name type assert-during valid-for original))

    ))

(defmacro defchip-pad (name type assert-during test-during valid-for)
  "Used to create an (external) pad. Type should be :input :output
:latched-output :latched-io or :io assert-during is :ph1 or :ph2 -
when the signal should be asserted; test-during is the same or
following phase during which it should be considered set if high (may
be :any if it holds through multiple phases like freeze), and
valid-for is the number of phase changes over which is valid, or the
minimum if indefinite (e.g.  freeze). Note that neither should start
or end up during a NEUTRAL phase (nither clock high), and eventually
we may want to restrict to landing on RISING phases as good EE
practice. NB: assert-during and test-during may be variables 
\(typically global parameters) and will be evaluated."

  `(cl:progn (defvar ,name (make-pads 1))
     ,@(when (member type '(:latched-output :latched-io))
         `((defvar ,(latch-name name) nil)
           (export-ulisp-symbol ',(latch-name name))))
     (setf (pad-defn ',name)
           '((:type . ,type)
             (:bus-p . nil)
             (:assert . ,(symbol-value assert-during))
             (:test . ,(symbol-value test-during))
             (:valid . ,valid-for)))
          (pushnew ',name *all-pad-names*)
          ,@(setup-pad-support name type (symbol-value assert-during) (symbol-value test-during) valid-for nil)
          (eval-when (:compile-toplevel :load-toplevel :execute)
            (export-ulisp-symbol ',name))))

(defmacro defchip-pads (name n type assert-during test-during valid-for &optional original)
  "Like defchip-pad but for a set of pins (e.g. address/data). Can
also be used to set an alias for shared pads. Note that assert-during and test-during may be variables if bound before use"
  `(cl:progn
     ,@(cl:if original
           `((defvar ,name ,original)
             ,@(setup-pad-support name type (symbol-value assert-during) (symbol-value test-during) valid-for original))
           `((defvar ,name (make-pads ,n))
             (pushnew ',name *all-bus-names*) ; only if original is null
             (setf (pad-defn ',name)
                   '((:type . ,type)
                     (:bus-p . t)
                     (:assert . ,(symbol-value assert-during))
                     (:test . ,(symbol-value test-during))
                     (:valid . ,valid-for)))
             ,@(setup-pad-support name type (symbol-value assert-during) (symbol-value test-during) valid-for nil)))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export-ulisp-symbol ',name)))) ; export from both so same symbol
