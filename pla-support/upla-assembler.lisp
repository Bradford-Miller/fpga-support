(in-package :fpga-pla-build-tools)

(fpga-support-version-reporter "FPGA PLA ulisp Assembler" 0 1 0
                               "Time-stamp: <2022-01-11 17:01:30 gorbag>"
                               "0.1 release")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0.1.0   1/11/22 snapping a line: 0.1 release of library supports scheme-79 test-0 and test-1. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; note in the AIM the lisp code is translated first from micro-lisp
;; into a micro PLA specification that looks vaguely similar to the
;; nanocode, e.g.

;; 
;;  *args* = *leader*; *display* = *node-pointer*
;; (defpc mark-node
;;  (assign *leader* (&car (fetch *node-pointer*)))
;;  (cond ((and (&pointer? (fetch *leader*))

;;
;; MARK-NODE
;; ((FROM *DISPLAY*) (TO *ARGS*) DO-CAR)
;; ((FROM *ARGS*) (BRANCH TYPE=POINTER-BUS MARK-NODE-3 MARK-NODE-1))
;;
;; [??: MARK-NODE] -> GO-TO 250, DO-CAR, FROM 7 TO 15 (250 200 007 15)
;; [250] -> GOTO 44, BRANCH, FROM 6 (044 307 006 00)
;;
;; [44: MARK-NODE-3] 
;; [45: MARK-NODE-1]

;; so given the above, here's a precis on the symbolic micro-PLA I'm
;; writing (this is the only reference to micro-pla, so I get to make
;; up as I go along)
;;
;; as before we load up the defufn macros with generators for this
;; symbolic version (rather than directly to the binary)
;;
;; each instruction is a list of the form
;;    ([(FROM <register>)] [(TO <register>)] <nanocode-op>)
;; or
;;    ((FROM <register>) (BRANCH <condition> <fail-tag> <success-tag>)
;; or
;;    ((<assembler-op> [<parameter>]))
;;
;; where 
;;    <register> is the usual *args* etc. names
;;
;;    <condition> is composed of the condition-name (from the defreg)
;;                and the register name (absent the '*'s) e.g.
;;                *newcell* has an address=bus sense wire, so it would
;;                be named address=bus-newcell while the bus itself
;;                has address=0 so it's named address=0-bus

;;
;; (<assembler-op> <parameter>) is one of the following 
;;     GO-TO <tag> which will generate the appropriate
;;                 next-instruction field or a no-op with a next
;;                 instruction of <tag> [note that <tag> need not be
;;                 an 'interned' symbol]

;; OK this is a simple multi-pass assembler:

;; pass 1: we will assign "instruction numbers" to the source file as
;;         well as any tags in the source

;; pass 2 (optional): if we are doing (basic) block elimination, we
;;                    look for duplicate blocks of code (q.v.) and
;;                    replace all but one with a jump to that one.
;;                    The basic idea is a block starts with an
;;                    instruction (which may or may not have a tag)
;;                    and is followed by one or more instructions
;;                    ending with some kind of transfer of control
;;                    (e.g.  jump, conditional, etc.) If two such
;;                    'basic blocks' are identical, including the tag
;;                    to which control is transfered, then they are
;;                    duplicate blocks. Note that we can either keep
;;                    running through the program identifying blocks
;;                    and eliminating them until we have a pass where
;;                    no more are eliminated (naive, simple, slow), or
;;                    we can try to eliminate likely duplicates first
;;                    by working backward from the transfer of control
;;                    points (e.g. if we see <tag-a> (go-to done) and
;;                    <tag-b> (go-to done) we should eliminate one of
;;                    the tags, e.g. tag-b, by changing any references
;;                    to tag-a. We leave <tag-b> (go-to done) in place
;;                    until the next pass where we can eliminate
;;                    unreachable code.

;; pass 3 (optional): if we are doing unreachable code elimination we
;;                    run through and find tags that are both not
;;                    referenced AND which are not "fallen through" by
;;                    the preceeding code, e.g. <tag1> (goto b) <tag2>
;;                    (goto c) where there are no references to <tag2>
;;                    (perhaps it had originally been the success arc
;;                    of a condition but was optimized away in pass
;;                    2), then we can eliminate <tag2> (goto c). Note
;;                    we have to preserve tags and types referenced by
;;                    the user in **pointer-types**, and
;;                    **non-pointer-types** as these will be
;;                    effectively generated indirectly the
;;                    dispatch-on- u-instructions. We also have to
;;                    make sure we don't screw up how branches work
;;                    (the fail/ success results must be to the
;;                    following two words with low bit 0 meaning fail,
;;                    and 1 meaning success)

;; pass 4 (required if pass 2 and/or pass 3 are run): revisit pass 1 renumbering the lines

;; pass 5: transform from symbolic to numeric representation (machine
;;         ucode) filling in *microcontrol-array*

;; and that's all folks!

(defgeneric run-assembler (compiler-stream)
  (:documentation "assembles the file open on stream in multiple
passes. As this isn't something done all THAT often (recompiling the
ucode that is) no effort was made to make this efficient (e.g. single
pass with backpatching)."))

(defgeneric run-assembler-pass1 (in out)
  (:documentation "pass1 just adds offsets for each instruction and
  fills out the symbol table for tags found."))

(defgeneric run-assembler-pass2 (in out)
  (:documentation "If we are doing (basic) block elimination, we look
                   for duplicate blocks of code (q.v.) and replace all
                   but one with a jump to that one.  The basic idea is
                   a block starts with an instruction (which may or
                   may not have a tag) and is followed by one or more
                   instructions ending with some kind of transfer of
                   control (e.g.  jump, conditional, etc.) If two such
                   'basic blocks' are identical, including the tag to
                   which control is transfered, then they are
                   duplicate blocks. Note that we can either keep
                   running through the program identifying blocks and
                   eliminating them until we have a pass where no more
                   are eliminated (naive, simple, slow), or we can try
                   to eliminate likely duplicates first by working
                   backward from the transfer of control
                   points (e.g. if we see <tag-a> (go-to done) and
                   <tag-b> (go-to done) we should eliminate one of the
                   tags, e.g. tag-b, by changing any references to
                   tag-a. We leave <tag-b> (go-to done) in place until
                   the next pass where we can eliminate unreachable
                   code."))

(defgeneric run-assembler-pass3 (in out)
  (:documentation "If we are doing unreachable code elimination we
                   run through and find tags that are both not
                   referenced AND which are not \"fallen through\" by
                   the preceeding code, e.g. <tag1> (goto b) <tag2>
                   (goto c) where there are no references to <tag2>
                   (perhaps it had originally been the success arc of
                   a condition but was optimized away in pass 2), then
                   we can eliminate <tag2> (goto c). Note we have to
                   preserve tags and types referenced by the user in
                   **pointer-types**, and **non-pointer-types** as
                   these will be effectively generated indirectly the
                   dispatch-on- u-instructions. We also have to make
                   sure we don't screw up how branches work
                   (the fail/ success results must be to the following
                   two words with low bit 0 meaning fail, and 1
                   meaning success)"))

(defgeneric run-assembler-pass4 (in out)
  (:documentation "(required if pass 2 and/or pass 3 are run): revisit
pass 1 renumbering the lines"))

(defgeneric run-assembler-pass5 (in out)
  (:documentation "Convert alpha-symbolic to numeric representation"))

(defgeneric create-microcontrol-jump-table ()
  (:documentation "The initial part of the microcontrol-array is a
jump table for each of the declared pointers and non-pointers used for
type dispatching"))

(defmethod create-microcontrol-jump-table ()
  (labels ((add-jump (entry)
             (destructuring-bind (type-name jump-location) entry
               ;; NB: not using bit-vector for index because this
               ;; isn't simulating the chip!
               (setf (aref *microcontrol-array* jump-location) 
                   (let ((code-posn (microcontrol-symbol-value type-name)))
                     (cl:if code-posn
                         (list code-posn 0 0 0) ; this is a jump
                         (cl:progn
                           (warn "jump table: no code for ~s" type-name)
                           ;; probably not the right thing, but lets us put something there for now.
                           (list jump-location 0 0 0)))))))) 
    (mapc #'add-jump **pointer-types**)
    (mapc #'add-jump **non-pointer-types**)))


(defmethod run-assembler (compiler-stream)
  ;; to start, rewind the stream so we can process from the beginning
  (rewind-stream compiler-stream)
  ;; and clear out the microcontrol
  (setq *microcontrol-symtab* nil)

  ;; create jump table as start of the microcontrol-array
  (let ((max-table-entry (apply #'max (mapcar #'cadr **non-pointer-types**))))
    (setf (fill-pointer *microcontrol-array*) (1+ max-table-entry))) ; leave room for the jump table

  ;; right now, we're only using passes 1 and 5, so don't bother
  ;; opening the other streams. (TBD)
  (let ((pass1-stream (hcl:open-temp-file :file-type "upla"
                                          :prefix (format nil "uPLA1-~A" (subseq *upla-file-name* 5))
                                          :delete-when-close (not *debug-assembler*)))
        (pass5-stream (hcl:open-temp-file :file-type "upla"
                                          :prefix (format nil "uPLA5-~A" (subseq *upla-file-name* 5))
                                          :delete-when-close (not *debug-assembler*))))

    (unwind-protect
         (cl:progn 
           (run-assembler-pass1 compiler-stream pass1-stream)
           (rewind-stream pass1-stream) ; now will be an input
           (run-assembler-pass5 pass1-stream pass5-stream)
           ;; now fill in the initial jump table
           (create-microcontrol-jump-table)
           ;; print some final statistics on the compiled file?
           ;; (compiler-stream should be at eof thanks to pass1)
           (announcement-banner
            (format nil "Final microcode array usage: ~d lines" (fill-pointer *microcontrol-array*))
            compiler-stream))
      
      (close pass1-stream) ; compiler-stream is closed by caller
      (close pass5-stream))))

    
