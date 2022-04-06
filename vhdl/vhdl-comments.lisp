(in-package :vhdl)

(fpga-support-version-reporter "FPGA VHDL Comments" 0 2 1
                               "Time-stamp: <2022-03-23 16:56:35 gorbag>"
                               "prin-simple-comment")

;; 0.2.1   4/ 5/22 prin-simple-comment
;; 0.2.0   3/24/22 New: VHDL comment generation fns
;; vhdl comment character is "--" so we need our own version of banners

;; note that dash is allowed in comments, but not in identifiers (use underbar instead). 

(defparameter *comment-preferred-length* 80)

(defun prin-dash-line (n &optional (indent 0))
  (declare (type fixnum n indent))
  
  (let ((dash-string (make-string (- n 3 indent) :initial-element #\-)))
    (format *vhdl-stream* "~&~A-- ~A~%" (make-pad indent) dash-string)))

;; similar to do-banner-string, so maybe something we should generify and add to cl-lib?
(defun prin-comment-line (text min-textlen &optional (indent 0))
  (declare (type string text)
           (type fixnum min-textlen indent))

  (assert (stringp text) (text) "Prin-comment-line non-text input")
  (let* ((textlen (length text))
         (modlen (- *comment-preferred-length* (max textlen min-textlen) (+ indent 5))) ; allow for intro and spaces around comment
         (prelen (floor (/ modlen 2)))
         (postlen (- modlen prelen)))

    (format *vhdl-stream* "~a-- ~a ~a ~a~%"
            (make-pad indent)
            (make-string prelen :initial-element #\-)
            (center-pad-string text (max textlen min-textlen))
            (make-string postlen :initial-element #\-))))

(defun prin-simple-comment (text &optional (indent 0))
  (declare (type string text)
           (type fixnum indent))
  (assert (stringp text) (text) "Prin-simple-comment non-text input")
  (format *vhdl-stream* "~a-- ~a~%"
          (make-pad indent)
          text))

(defun header-comment (texts &optional short-header-p (indent 0))
  (declare (type list texts))
  
  (let* ((max-textlen (apply #'max (mapcar #'length texts)))
         (*comment-preferred-length* (max (- *comment-preferred-length* indent) (+ max-textlen 5)))
         (spaces (make-pad max-textlen)))
    (prin-dash-line *comment-preferred-length* indent)
    (unless short-header-p
      (prin-comment-line spaces max-textlen indent))
    (mapc #'(lambda (x) (prin-comment-line x max-textlen indent)) texts)
    (unless short-header-p
      (prin-comment-line spaces max-textlen indent))
    (prin-dash-line *comment-preferred-length* indent)
    (terpri *vhdl-stream*)))
