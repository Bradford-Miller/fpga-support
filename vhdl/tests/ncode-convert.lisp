(defun ncode-convert (pad reg next)
  "conversion code from separate fields to packed hex for vhdl tests. Enter hex numbers as #xffff etc."
  ;; pad is 14 bits, reg is 10, and next is 4. We can leave next alone, we need to shift pad
  ;; two bits to the right and take the lost two bits and put them as the high order bits on reg to give
  ;; the consolodated output.
  (let ((first-12bits (ash pad -2))
        (next-12bits (logior (ash (logand pad #x3) 10) reg)))
    (format t "~3,'0X~3,'0X~1,'0X" first-12bits next-12bits next)))
