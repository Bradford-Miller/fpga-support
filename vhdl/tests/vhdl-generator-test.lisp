(in-package :vhdl)

(fpga-support-version-reporter "FPGA VHDL Tests" 0 2 0
                               "Time-stamp: <2022-04-04 16:55:10 gorbag>"
                               "new")

;; 0.2.0   4/ 4/22 Simple test of the vhdl generator

(defun test-vhdl-stream ()
  "Create a vhdl stream and write a vhdl program to it"
  (let (*vhdl-stream*)
    (unwind-protect
         (progn (setq *vhdl-stream* (ensure-vhdl-stream)) ; opens /private/var/tmp/generated.vhdl
                ;; simulation example from xxx
                (defentity sim_sample (ieee ieee.std_logic_1164.all ieee.numeric_std.all)
                  (:scomment "Inputs")
                  (clk in std_logic)
                  (rst in std_logic)
                  (:scomment)
                  (dat_in in std_logic_vector(7 downto 0))
                  (enable in std_logic)
                  (:scomment "Outputs")
                  (comp_cnt out std_logic_vector(9 downto 0)))

                (with-architecture (sim_sample_arch sim_sample
                                                    (prin-arch-decls '((dat_in_d1 std_logic_vector(7 downto 0))
                                                                       (comp_cnt_lcl unsigned(9 downto 0)))))
                  (defprocess ("Sim_Sample_Proc" (clk rst))
                      (defclk
                          (if (rst = :one) then
                              dat_in_d1 <= (:stringquote X"00"))
                          (comp_cnt_lcl <= (:stringquote "00") & (:stringquote X"00"))
                        (elsif rising_edge(clk) then
                               if (enable = :one) then
                               dat_in_d1 <= dat_in)
                        (if (dat_in_d1 = dat_in) then
                            comp_cnt_lcl <= comp_cnt_lcl + 1)
                        ;; indenting doesn't flow quite right in lisp version so easy to get confused here.
                        ;; also our auto formatter for vhdl misses the embedded if inside the elsif clause (TBD)
                        (end if) ; dat_in_d1..
                        (end if) ; enable = :one...
                        (end if))) ;rising_edge(clk)
                  (defrtl
                      (comp_cnt std_logic_vector(comp_cnt_lcl))))

                (header-comment '("Simple Testbench Using Embedded, Automatic Vectors"))
                (defentity tb_sim_sample_2 (ieee ieee.std_logic_1164.all ieee.numeric_std.all))

                (with-architecture ("Behavioral" tb_sim_sample_2
                                    (defcomponent sim_sample
                                      (:scomment "Inputs")
                                      (clk in std_logic)
                                      (rst in std_logic)
                                      (:scomment)
                                      (dat_in in std_logic_vector(7 downto 0))
                                      (enable in std_logic)
                                      (:scomment "Outputs")
                                      (comp_cnt out std_logic_vector(9 downto 0)))

                                    ;; these are ripe for macros to make them closer to the VHDL... (TBD)
                                    (prin-constant :half_period :time 5 :ns :comment "100MHz = 10ns")
                                    (prin-constant :quant_vectors :integer 30 "" :comment "number of vectors")
                                    (prin-simple-comment "semi-random seed" 3)
                                    (prin-constant :seed "unsigned(5 downto 0)" "\"100111\"")

                                    (terpri *vhdl-stream*)
                                    (prin-arch-decls '((:scomment module-under-test inputs)
                                                       (clk std_logic)
                                                       (rst std_logic)
                                                       (data_val std_logic_vector(7 downto 0))
                                                       (en std_logic)
                                                       (:scomment module-under-test outputs)
                                                       (comp_cnt std_logic_vector(9 downto 0)))))

                  (prin-simple-comment "module under test" 4)

                  (component-portmap "MUT" sim_sample
                                     (:scomment "Inputs")
                                     (clk clk)
                                     (rst rst)
                                     (dat_in data_val)
                                     (enable en)
                                     (:scomment "Outputs")
                                     (comp_cnt comp_cnt))

                  (header-comment '("Clock Generator"))

                  (defrtl
                      (clk :zero after half_period when clk = :one else
                           :one after half_period))

                  (terpri *vhdl-stream*)
                  (header-comment '("Reset Generator"))

                  (defprocess ("Reset_Gen" ())
                      (defclk
                          (:scomment generate reset)
                          (for i in 1 to 5 loop
                               if (i < 4) then
                               rst <= :one)
                        (else
                         rst <= :zero)
                        (end if)
                        (wait until falling_edge(clk))
                        (end loop)
                        (:scomment)
                        (:scomment de-activate this process)
                        (assert false report "Reset_Gen completed" severity note)
                        (wait on rst)))

                  (header-comment '("Vector Generator"))

                  (defprocess ("Vector_Generate_Implicit"
                               ()
                               (prin-proc-decls
                                '((semi_random unsigned(5 downto 0) := (:stringquote "101010"))
                                  (data_val_un unsigned(7 downto 0) := (:stringquote X"00")))))
                      (defclk
                          (data_val <= (:stringquote X"00"))
                          (en <= :zero)

                        (wait until falling_edge(rst))

                        (for j in 1 to 3 loop
                             wait until falling_edge(clk))
                        (end loop)

                        (en <= :one)

                        (for i in 1 to quant_vectors loop
                             semi_random := semi_random + seed)

                        (if (semi_random(5 downto 4) /= (:stringquote "00")) then
                            data_val_un := data_val_un + 1)
                        (end if)

                        (data_val <= std_logic_vector(data_val_un))
                        (wait until falling_edge(clk))
                        (end loop)

                        (en <= :zero)

                        (:scomment de-activate this process)
                        (assert false report "end of test" severity note)
                        (wait on rst))))))

      (if (and (streamp *vhdl-stream*)
               (open-stream-p *vhdl-stream*))
          (close *vhdl-stream*))))

(defparameter *vhdl-simulator-path* "/usr/local/bin/ghdl"
  "Set up the path to the vhdl simulator. At this point we only handle
ghdl with the options provided, but later we can also allow, e.g.,
vivado (if we're running on Linux, that is)")

(defparameter *vhdl-analysis-options* "-a")

(defparameter *vhdl-evaluation-options* "-e")

(defparameter *vhdl-simulation-options* "-r"
  "what we need to pass to the vhdl-simulator to run it on generated
code")

(defparameter *vhdl-runopts*  "--wave=generated.GHW" ; a file for the waves to be viewed with gtkwave
  "run options")

(defparameter *vhdl-all-options* "--std=08"
  "options that should be passed to the simualtion program regardless
of analysis/evlaution or running the sim")

(defun sim-vhdl (top-unit &optional (file "generated.vhdl"))
  "File should contain VHDL which we can simulate"
  ;; ghdl -a --std=08 -P/private/var/tmp/ <file>
  ;; ghdl -e --std=08 -P/private/var/tmp/ <file>
  ;; ghdl -r --std=08 -P/private/var/tmp/ <file>
  (let ((current-directory (hcl:get-working-directory)))
    (unwind-protect
         (progn
           (hcl:change-directory "/private/var/tmp/")
           (system:call-system-showing-output (format nil "~a ~a ~a ~A" 
                                                      *vhdl-simulator-path*
                                                      *vhdl-analysis-options*
                                                      *vhdl-all-options*
                                                      file)
                                              :prefix "*** "
                                              :kill-process-on-abort t)

           (unless (y-or-n-p "Evaluate?")
             (return-from sim-vhdl :analyzed))
           (system:call-system-showing-output (format nil "~a ~a ~a ~A" 
                                                      *vhdl-simulator-path*
                                                      *vhdl-evaluation-options*
                                                      *vhdl-all-options*
                                                      top-unit)
                                              :prefix "*** "
                                              :kill-process-on-abort t)

           (unless (y-or-n-p "Run?")
             (return-from sim-vhdl :elaborated))
           (system:call-system-showing-output (format nil "~a ~a ~a ~A ~A" 
                                                      *vhdl-simulator-path*
                                                      *vhdl-simulation-options*
                                                      *vhdl-all-options*
                                                      top-unit
                                                      *vhdl-runopts*)
                                              :prefix "*** "
                                              :kill-process-on-abort t))

      (hcl:change-directory current-directory))))

#||
;; testbench is the 'top unit'
(sim-vhdl "tb_sim_sample_2")
||#

;; NB: I used gtkviewer as advised by ghdl docs. This doesn't seem to
;; work all that well on Macs (i installed via macports) but with some
;; fiddling, i.e. changing the size of the window (drag the corner)
;; one can get the screen to update (it won't scroll, unfortunately,
;; but one can set the From and To times and then fiddle again).  The
;; test here has to be killed shortly after running it else data
;; points continue to be accumulated. This can be addressed by
;; specifying a stop-time when running (i.e.  --stop-time=<TIME> where
;; time in the above case could be 370ns to run it until after enable is 0.

;; I'm also guessing one can put an error assertion at the end of the
;; testbench to terminate running?
