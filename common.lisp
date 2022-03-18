(in-package :common)

(cl-lib:detailed-version-reporter "FPGA Dev Support Common" 0 2 0
                                  "Time-stamp: <2022-03-18 15:07:08 gorbag>"
                                  "fix date-string with leading zero pads"
                                  :initialization-list-symbol cl-user::*fpga-support-version-reporter-initializations*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.2.0   3/18/22 snapping a line: 0.2 release of library supports scheme-79 test-0 thru test-3 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.1.1   1/27/22 date-string should print leading zeros for minutes
;;                    and seconds!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 0.0.8   1/ 6/22 add define-property-accessor

;; 0.0.7  12/ 2/21 add initialization for *note-output*

;; 0.0.6  12/ 1/21 send note-output to hcl:*background-output*

;; 0.0.5  11/24/21 note-banner, *note-output*

;; 0.0.4  10/12/21 add verbose-p option to copy-field (defaults true):
;;                    will add note if padding or trimming

;; 0.0.3   9/12/21 fix copy-field when padding result

;; 0.0.2   9/ 9/21 moved copy-field from core-support.lisp, enhanced to 
;;                    allow destination bits to be specified

;; 0.0.1   9/ 7/21 moved rewind-stream from core-support.lisp

;; 0.0.0   9/ 2/21 moved from scheme-79/common and some functions that
;;                    were in scheme-79-defs

;; extensions that probably should be part of cl-lib

(defun cadaddr (x)
  (car (cdr (car (cdr (cdr x))))))

(defun rewind-stream (file-stream)
  "Reset a file stream to the start of the file"
  (file-position file-stream :start))

;; many of these could be moved to CL-LIB though may need improved in terms of efficiency first

(defvar *note-output*  hcl:*background-output*
  "set or bind to something else if you don't want notes (generally for debugging) going to *error-output*")

;; make sure the binding is done when we initialize
(add-initialization "Bind *note-output* globally"
                    '(setq *note-output* hcl:*background-output*)
                    nil
                    '*fpga-support-initializations*)

(defun note (nstring &rest nargs)
  "Like warn but not serious"
  ;; right now, only handle format type notes
  (cond
    ((stringp nstring)
     (format *note-output* "~&;; Note: ~?~%" nstring nargs))
    (t
     (warn "note doesn't (currently) handle conditions the way warn does!"))))

(defun note-if (boolean nstring &rest nargs)
  "Note with a condition"
  (when boolean
    (apply #'note nstring nargs)))

(defun prin-note-ast (n)
  (let ((ast-string (make-string n :initial-element #\*)))
    (note ast-string)))

;; banners
(defparameter *banner-length* 120)


(defun do-banner-string (string max-surround) ; just a big number
  (let* ((stringlen (length string))
         (modlen (- *banner-length* stringlen 2)) ; allow for spaces around string
         (prelen (floor (/ modlen 2)))
         (actual-prelen (min prelen max-surround))
         (postlen (- modlen prelen))
         (actual-postlen (min postlen max-surround)))

    (note (format nil "~a~a~a~a~a" 
                  (make-string actual-prelen :initial-element #\*)
                  (make-string (1+ (- prelen actual-prelen)) :initial-element #\space)
                  string
                  (make-string (1+ (- postlen actual-postlen)) :initial-element #\space)
                  (make-string actual-postlen :initial-element #\*)))))


(defun note-banner (strings &optional (max-surround *banner-length*))
  (let* ((maxstringlen (apply #'max (mapcar #'length strings)))
         (*banner-length* (max *banner-length* (+ maxstringlen 8))))
    (labels ()
      (prin-note-ast *banner-length*)
      (do-banner-string (make-string maxstringlen :initial-element #\space) max-surround)
      (mapc #'(lambda (x) (do-banner-string x max-surround)) strings)
      (do-banner-string (make-string maxstringlen :initial-element #\space) max-surround)
      (prin-note-ast *banner-length*))))

(defun prin-semi (n &optional (stream *error-output*))
  (dotimes (i n)
    (princ ";" stream)))

(defun print-semi-line (&optional (stream *error-output*))
  (terpri stream)
  (prin-semi *banner-length* stream)
  (terpri stream))

(defun announcement-banner (string &optional (stream *error-output*))
  (let* ((stringlen (length string))
         (modlen (- *banner-length* stringlen))
         (prelen (floor (/ modlen 2)))
         (postlen (- modlen prelen)))

    (print-semi-line stream)

    (prin-semi prelen stream)      (format stream string)       (prin-semi postlen stream)

    (print-semi-line stream)))

;; tabulated output
(defun length-for-printing (x)
  (cond ((stringp x)
         (length x))
        ((symbolp x)
         (length (string x)))
        ((numberp x)
         (length (format nil "~d" x)))))

(defun setup-tabulated-output (fields choice-lists)
  "go through the choice lists for each field finding the largest entry and return a list of sizes"
  (let ((result nil))
    (dotimes (i fields)
      (let ((max 0))
        (dolist (x (elt choice-lists i))
          (let ((len (length-for-printing x)))
            (if (> len max)
              (setq max len))))
        (push max result)))
    (nreverse result)))

(defun print-tab-line (stream field-max &rest entries)
  (dolist (m field-max)
    (princ (car entries) stream)
    (let ((r (- m (length-for-printing (car entries)))))
      (dotimes (i r)
        (princ #\space stream)))
    (when (cdr entries)
      (princ " | " stream))
    (setq entries (cdr entries))))

(defun date-string (&optional (precision :minute))
  "Return the current date and time as a string. The precision option lets us just get the date, the time to the second, etc."
  (mlet (second minute hour day month year) (get-decoded-time)
    (case precision
      (:date
       (format nil "~d/~d/~d" month day year))
      (:minute
       (format nil "~d/~d/~d ~d:~2,'0d" month day year hour minute))
      (:second
       (format nil "~d/~d/~d ~d:~2,'0d:~2,'0d" month day year hour minute second)))))

;; debug and partial implementation

(defvar *break-on-stub* nil
  "lets us look for rare stubs that are annoying to find (because there are so many that are OK ;-)")

;; to allow us to mark stub functions
(defun tbd (&rest comments)
  (warn "This is not implemented yet (~s)" comments)
  #+lispworks
  (dbg:bug-backtrace nil :debugger-stack (dbg::grab-stack nil))
  (when *break-on-stub*
    (break)))

(defun duplicates-p (l &key (key 'identity) (test 'eql))
  "return list of elements of l that are duplicates, or NIL if none"
  ;; not a fast way to do this, but gets the job done
  (cond
    ((not (endp l))
     (let ((dup (member (funcall key (car l)) (cdr l) :key key :test test)))
       (if (not (null dup))
           (cons (car l)
                 (duplicates-p (cdr l) :key key :test test))
           (duplicates-p (cdr l) :key key :test test))))))
    
;; bit vector functions

;; for creating n-bit vectors
(defun make-n-bit-vector (n)
  (make-array n :element-type 'bit :initial-element 0))

(defun copy-field (from to &key result-bits (verbose-p t))
  "From and to are registers or fields (bit arrays). Copy the content of from to to."
  (let* ((destination-field-len (length to))
         ;(result-differential (if result-bits (- result-bits destination-field-len) 0))
         (source-field-len (length from))
         (diff (- (or result-bits destination-field-len) source-field-len))
         (adj-diff 0))

    (when result-bits
      (assert (<= result-bits destination-field-len) (result-bits) 
        "Can't require a result longer than the destination field (~d > ~d)"
        result-bits destination-field-len)
      (setq adj-diff (- destination-field-len result-bits)))

    (let ((from-index (if (minusp diff) (abs diff) 0))
          (to-index 0))
      ;; if we are copying fewer bits (e.g. for pads) we need to pad out the result
      (when (plusp diff) ; result is longer than from
        (when verbose-p
          (note "copy-field: padding result")) ; just so we know we're doing that
        (dotimes (i diff)
          (setf (bit to i) 0))
        (setf to-index diff))
      
      (when (minusp diff) ; result is shorter than from
        (when verbose-p
          (note "copy-field: trimming result"))
        (dotimes (i adj-diff) ; if we're also pushing past the start we need to zero out the other bits
          (setf (bit to i) 0))
        (setf to-index adj-diff))
    
;      (format t "from-i to-i diff adj-diff ~d ~d ~d ~d" from-index to-index diff adj-diff)
      (while (< from-index source-field-len)
        (setf (bit to to-index) (bit from from-index))
        (incf from-index)
        (incf to-index)))
    to))

;; shifting (non-destructive) not sure I really need this...
(defun shift-right (x n)
  (let* ((sv-size (length x))
         (new (make-n-bit-vector sv-size)))
    
    (dotimes (j n)
      (dotimes (i (1- sv-size))
        (setf (bit new i) (bit x (1+ i))))
      (setf (bit new (1- sv-size)) 0))
    new))

;; from https://www.lispforum.com/viewtopic.php?t=1205 by edgar-rft:

(defun bit-vector->integer (bit-vector)
  "Create a positive integer from a bit-vector."
  (reduce #'(lambda (first-bit second-bit)
              (+ (* first-bit 2) second-bit))
          bit-vector))

;; add ability to pad to a given bit-length bwm
(defun integer->bit-vector (integer &key result-bits)
  "Create a bit-vector from a positive integer."
  (labels ((integer->bit-list (int &optional accum)
             (cond ((> int 0)
                    (multiple-value-bind (i r) (truncate int 2)
                      (integer->bit-list i (push r accum))))
                   ((null accum) (push 0 accum))
                   (t accum))))
    (let ((result (coerce (integer->bit-list integer) 'bit-vector)))
      ;; if we don't have enough result bits, expand
      (if result-bits
        (let* ((current-bits (length result))
               (bit-shortage (- result-bits current-bits)))
          (if (> bit-shortage 0)
            ;; pad
            (progfoo (make-n-bit-vector result-bits)
              (dotimes (i current-bits)
                (setf (bit foo (+ i bit-shortage)) (bit result i))))
            result))
        result))))

(defun bv-zerop (bv)
  "true if bit vector is all 0s"
  (every #'zerop bv))

(defun zerop-field (x)
  "test if a field is 0"
  (bv-zerop x))

(defun decrement-field (x)
  "subtract 1 from field; note the hardware has a separate register with such a field tied to another register."
  (let* ((field-len (length x))
         (result (make-n-bit-vector field-len))
         (carry t)
         (i (1- field-len)))
    ;; note that the largest bit offset is the LSbit

    (while-not (minusp i)
      (cl:cond
       ((and carry (zerop (bit x i)))
        (setf (bit result i) 1))
       (carry
        (setf (bit result i) 0)
        (setq carry nil))
       (t ; just finish copying bits
        (setf (bit result i) (bit x i))))
      (decf i))
    result))

(defun increment-field (x)
  "add 1 to field"
  (let* ((field-len (length x))
         (result (make-n-bit-vector field-len))
         (carry t)
         (i (1- field-len)))
    (while-not (minusp i)
      (cl:cond
       ((and carry (zerop (bit x i)))
        (setf (bit result i) 1)
        (setq carry nil))
       (carry
        (setf (bit result i) 0))
       (t ; just finish copying bits
        (setf (bit result i) (bit x i))))
      (decf i))
    result))

(defmacro define-property-accessor (accessor-name property-name)
  (let ((symarg (gensym))
        (newvalue (gensym))
        (setter (intern (format nil "SET-~A" (string accessor-name)))))
    `(progn (defun ,accessor-name (,symarg)
              (declare (type symbol ,symarg))
              (get ,symarg ,property-name))
            (defun ,setter (,symarg ,newvalue)
              (setf (get ,symarg ,property-name) ,newvalue))
            (defsetf ,accessor-name ,setter))))
