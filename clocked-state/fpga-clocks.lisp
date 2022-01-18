(in-package :fpga-clocked)

(fpga-support-version-reporter "FPGA Clock Support" 0 1 1
                               "Time-stamp: <2022-01-14 12:24:58 gorbag>"
                               "note neutral clock phase usage")

;; 0.1.1   1/14/22 change from warn to note-if *debug-pad-timing* the
;;                   warning about using a neutral clock phase for an
;;                   action (this may be necessary for simulation but
;;                   is bad for hardware!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.2  9/20/21 Move in init list definitions and init-list-name fn
;;                   from clock.lisp

;; 0.0.1  9/16/21 Move some basic clock variables in from
;;                   simulator/clock.lisp and add a clock
;;                   configuration function to support alternatives to
;;                   the two-phase non-overlapping of scheme-79.

;; 0.0.0  9/14/21 New

;; here we have support functions to broadly support clocking. The
;; intent is that these functions are aligned to easily recognizable
;; clock functions on an FPGA. Note that projects are free to add more
;; detailed versions that go beyond what we define here, but then they
;; will also need to eventually define a mechanism to translate that
;; into FPGA support through HDL!

;; At least on our current target (The Xilinx Artix) we can set up a
;; two phase clock. The current project uses a two phase
;; non-overlapping external clock, but we're going to assume we will
;; generate it internally as is also needed for simulation.

;; Here we will define some macros and functions that give the project
;; the choice of 1 or two phases, a frequency (currently ignored but
;; can be passed to the synthesis tools in the future) if two phases
;; if they overlap or are distinct. For our simulation purposes we
;; also name each logical phase of the clock; some of these phase
;; names will ony make sense if there are two phases and if the phases
;; overlap or not, but the naming of these cases is presented here.

;; note that most synchronous HDL is triggered on either the rising or
;; falling edge of a phase (and typically the rising edge only is
;; considered good practice). But we NAME all these phases here as the
;; software simulation will typically assign things that may occur
;; later (due to combinatoric logic) in the clock cycle to a later
;; phase. We'll sort this out better as we get closer to converting
;; the project specifications to HDL.

;; our general naming convention is as follows. For the single or two
;; phase case, the first or only phase is PH1, and in the two phase
;; case the second phase is PH2.

;; PHx-RISING indicates the rising edge, PHx-FALLING the falling edge
;; of a particular phase.

;; Case 1: single phase

;;         PH1-HIGH       NEUTRAL-1
;;
;;      |-------------|
;;      |             |
;;      |             |
;;      |             |
;; -----/             \----------
;;   
;;      ^             ^
;;   PH1-RISING   PH1-FALLING

;; Note that all low parts of the clock in this case are called
;; NETURAL-1

;; Case 2: two phases, non-overlapping

;;         PH1-HIGH       NEUTRAL-1       PH2-HIGH       NEUTRAL-2
;;
;;      |-------------|               |-------------|
;;      |             |               |             |
;;      |             |               |             |
;;      |             |               |             |
;; -----/             \---------------/             \----------
;;   
;;      ^             ^               ^             ^
;;   PH1-RISING   PH1-FALLING     PH2-RISING   PH2-FALLING

;; Note that NEUTRAL-2 preceeds the start of a new TICK (major clock
;; cycle) while NEUTRAL-1 is between phases within the TICK

;; Case 3: two phases, overlapping

;;    PH1-HIGH  BOTH-HIGH  PH2-HIGH   NEUTRAL-1
;;
;;      |-------------|               
;;      |             |               
;;      |        |----+-----------    
;;      |        |    |          |    
;; -----/--------/    \----------\--------------
;;   
;;      ^             ^           
;;   PH1-RISING   PH1-FALLING     
;;               ^               ^
;;           PH2-RISING   PH2-FALLING

;; Note that in "real life" the PH1 and PH2 clocks would be on
;; different pins; drawing them as if they have different voltage
;; levels is just an artifact of presentation.

;; Also note that if we test if PH1-HIGH during BOTH-HIGH it should
;; return true, etc. That is, PH1-HIGH does not end until PH1-FALLING,
;; and PH2-HIGH starts after the end of PH2-RISING.

;; Now in the real world, the length of the phase being high out of
;; the total (major) clock period, the amount of overlap, etc. all may
;; matter and presumably can be specified. For now we will ignore this
;; as the simulation is run off of the LOGICAL phases (i.e. these
;; different symbols above) through initialization lists: we specify
;; what should happen during each logical phase and the various
;; functions are kicked off as that phase obtains.

;; state of individual phase. These aren't actually used for anthing right now, but they're available to the project.
;; only show nil/t for low/high state at this point

(defvar *ph1-clock-state* nil
  "t when ph1 is high, we really only care about transitions but some
  things might need to be true while a clock is high")

(defvar *ph2-clock-state* nil)

;; different phase names depending on the scenario
(defparameter *phase-name-alist*
  '((:uniphase :ph1-rising :ph1-high :ph1-falling :neutral-1)
    (:biphase-nonoverlapping :ph1-rising :ph1-high :ph1-falling :neutral-1
                             :ph2-rising :ph2-high :ph2-falling :neutral-2)
    (:biphase-overlapping :ph1-rising :ph1-high :ph2-rising :both-high
                          :ph1-falling :ph2-high :ph2-falling :neutral-1)))

(defparameter *phase-names* (cdr (assoc :biphase-nonoverlapping *phase-name-alist*)))

;; note :neutral-1 is always a member of *phase-names*
(defvar *symbolic-clock-phase* :neutral-1
  "should be a member of *phase-names*")

(defparameter *total-clock-phases* (list-length *phase-names*))

(defun configure-clock (type frequency)
  (declare (ignore frequency)) ; don't use yet
  (setq *phase-names* (cdr (assoc type *phase-name-alist*)))
  (assert *phase-names* (type)
          "Type did not name a type of clock, should be one of: ~{~A ~}"
          (mapcar #'car *phase-name-alist*))
  (setq *total-clock-phases* (list-length *phase-names*)))

;; some core functions for computing phases in simulation

(defun remap-phase-name (phase)
  "Some phases have aliases to make it easier for user-entered macros"
  (case phase
    (:ph1
     :ph1-high)
    (:ph2
     :ph2-high)
    ((:any :all) ; normally used as a selector or filter
     :ph1-high) ; for now, see also init-list-name
    (t
     phase)))
        
(defun get-end-phase (start-phase phase-count)
  "given a phase and a phase count, determine the name of the ending phase. Second value is non-nil if the phase occurs in the NEXT tick and the current clock is before that phase in the CURRENT tick."
  (setq start-phase (remap-phase-name start-phase))
  (let ((posn (position start-phase *phase-names*)))
    (assert (not (null posn)) (start-phase) "Phase ~s was not found in *phase-names*" start-phase)
    (values (nth (mod (+ posn phase-count) *total-clock-phases*) *phase-names*)
            (>= phase-count *total-clock-phases*)))) ; note that this phase is NOT the current or upcoming one 7/2/21

(defun next-phase (phase)
  (let* ((start-phase (remap-phase-name phase))
         (posn (position start-phase *phase-names*)))
    (assert (not (null posn)) (start-phase) "Phase ~s was not found in *phase-names*" start-phase)
    (nth (mod (1+ posn) *total-clock-phases*) *phase-names*)))

(defun phase-equal (ph1 ph2)
  (or (eql ph1 ph2)
      (eql (remap-phase-name ph1) (remap-phase-name ph2)))) ; this way we only do it if needed

(defun in-phase-interval-p (start-tick start-phase phase-count)
  "return non-nil if the current phase is either the start-phase or at
most phase-count away. start-tick is the start of the interval and
should be before the current tick"
  (let ((mapped-phase (remap-phase-name start-phase))) ;fixup
    (cl:cond
     ((> start-tick *tick*) ; not there yet
      nil)
     ((and (or (member start-phase '(:any :all))
               (phase-equal mapped-phase *symbolic-clock-phase*))
           (<= (- *tick* start-tick) (floor (/ phase-count *total-clock-phases*))))
      t)
     (t
       ;; this has to be the slowest way to do this, but the
       ;; alternative (using, e.g. member) would have to handle
       ;; wrapping ;-) the fastest way to do this would be a simple
       ;; lookup table, but that needs memory... which some of us
       ;; still think is precious even when it no longer is... When I
       ;; get time I can at least wrap with a memoization routine.

       (assert (member mapped-phase *phase-names*) (start-phase) "Phase ~s was not found in *phase-names*" start-phase)
       
       (let* ((posn (position mapped-phase *phase-names*))
              (count phase-count)
              (current-tick start-tick))
         (while (plusp count)
           (setq posn (mod (1+ posn) *total-clock-phases*))
           (decf count)
           (when (zerop posn) ; looking to the next tick, effectively
             (incf current-tick))
           (cl:if (and (phase-equal (nth posn *phase-names*) *symbolic-clock-phase*)
                       (>= current-tick *tick*)) ; calculated tick is after the current one so we are within the interval 
                  (return-from in-phase-interval-p t))))))))

;; for software simulation purposes, we associate each possible state
;; of the clock with an initialization list, which we reset and run
;; for each state. These lists kick off functions that simulate what
;; should change at that point, e.g. moving data from or to a
;; register. Note that as we aren't simulating the continuity of a
;; signal in electronics, but rather just a single state change the
;; order in which we simulate is important.

;; for example, in electronics a signal propegates and might only
;; become stable at a given point in time, but other states are
;; calculated based on that propegated signal. In simulation we might
;; have to repeat our calculation to make sure we capture the "final
;; state" after some intermediate states are calculated. While it is
;; possible to simulate the fluency (e.g. that signal S is the AND
;; between signals Q and P at all times) for practical purposes we
;; check Q and P and update S at specific times and it is therefore
;; important we understand that S may influence Q and P and thus has
;; to be resampled when that is critical. It is beyond the (current)
;; state of this library to help with that, other than to allow
;; multiple clock phases to repeat the calculation as the project sees
;; fit.

;; At any rate, here we define the various (possible) initialization
;; lists associated with the three clocking scenarios outlined
;; above. The specific lists that are used will depend on the
;; configuration of the clock subsystem via "configure-clock".

(defvar *ph1-rising-list* nil)

(defvar *ph1-high-list* nil)

(defvar *ph1-falling-list* nil)

(defvar *ph2-rising-list* nil)

(defvar *ph2-high-list* nil)

(defvar *ph2-falling-list* nil)

(defvar *neutral1-list* nil
  "Not normally used to trigger actions, but we might end up having some CLEAR-PADs get queued to the neutral lists
due to minimum validity times; we can sort that out later")

(defvar *neutral2-list* nil
  "Only used for biphase-nonoverlapping clocks")

(defvar *both-high-list* nil
  "Only used for biphase-overlapping clocks")

(defvar *all-ph-list* nil
  "Trigger on every phase change, comes AFTER phase-specific initializations")

(defvar *all-ph-pre-update-list* nil
  "Trigger just before the phase change starts, i.e. BEFORE the *symbolic-clock-phase* is updated and BEFORE
any phase specific initialization lists are called.")

;; translate between a keyword and the initialization list name. We can add a check (TBD) that the list we are trying
;; to access is in fact valid for our currently configured clock.


(defun init-list-name (clock-phase)
  (ecase clock-phase
    ((:ph1 :ph1-high)
     '*ph1-high-list*)
    ((:ph2 :ph2-high)
     '*ph2-high-list*)
    (:ph1-rising
     '*ph1-rising-list*)
    (:ph2-rising
     '*ph2-rising-list*)
    (:ph1-falling
     '*ph1-falling-list*)
    (:ph2-falling
     '*ph2-falling-list*)
    (:neutral-1
     (note-if *debug-pad-timing* "using a NEUTRAL clock phase for an action")
     '*neutral1-list*)
    (:neutral-2
     (note-if *debug-pad-timing* "using a NEUTRAL clock phase for an action")
     '*neutral2-list*)
    (:both-high
     '*both-high-list*)
    (:any
     '*ph1-high-list*) ; for now treat as phase1
    (:all
     '*all-ph-list*)))
