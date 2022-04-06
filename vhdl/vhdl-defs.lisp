(in-package :vhdl)

(fpga-support-version-reporter "FPGA VHDL Defs" 0 2 0
                               "Time-stamp: <2022-04-01 17:56:51 gorbag>"
                               "new")

;; 0.2.0   3/21/22 -- 4/1/22 New package for VHDL generator defs

;; when we are evaluating macros that build both simulation and VHDL
;; components, we need to have a file to put the VHDL in. It probably makes
;; more sense to bind this to a different file for each file we compile, though
;; it should work just to open this once and dump all the VHDL into it (as we
;; won't have to worry about collecting it all up again later).

(defvar *vhdl-stream* nil)

(defparameter *ops-to-upcase* '(and or not)
  "when these operators appear in an RTL description we upcase them
\(see prin-rtl-description and defarch-rtl). VHDL is case-insensitive
(as is Lisp, by default) so this is just to aid in debugging when
reading the HDL, and will not change the behavior of the generated
code.")

(defparameter *special-constants* '((:zero . "'0'")
                                    (:one . "'1'")
                                    (:comma . ",")
                                    (:semi . ";")
                                    (:= . ":="))
  "Alist of special constants in VHDL that we can't as easily
represent in Lisp other than as strings - this is just to simplify
calling the macros using, e.g. :zero instead of '0' which will be
mistaken as quoting the following symbol.")

(defun vhdl-pathname (x)
  (let ((host (pathname-host x))
        (dev (pathname-device x))
        (dir (pathname-directory x))
        (name (pathname-name x)))
    
    (make-pathname :host host :device dev :directory dir :name name :type "vhdl" :version :newest)))

(defun open-vhdl-stream (filename)
  "force the extension of filename to be .vhdl, and open it, returning the stream"
  (open (vhdl-pathname filename) :direction :output))

(defun ensure-vhdl-stream ()
  "called whenever we are outputting vhdl, this makes sure we have a vhdl
stream and if not creates one as appropriate. For now, we will SET
*vhdl-stream* to the new stream, later we may want to BIND it during the
compilation of this particular file and/or function. We will accumulate any
interesting vhdl into /private/var/tmp, again, for the nonce; it may make more
sense to put it somewhere that will be easier to pass to our vhdl simulation
tools or the design synthesizer and bitstream generator."
  (unless *vhdl-stream*
    (setq *vhdl-stream* (open-vhdl-stream "/private/var/tmp/generated.vhdl"))))

;; commonly used to deal with passing symbols instead of strings to vhdl macros
(defun downcase-if-symbol (x)
  (if (stringp x)
    x
    (string-downcase (string x))))