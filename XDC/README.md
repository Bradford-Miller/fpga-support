# XDC Generation Notes
### Time-stamp: <2022-04-11 10:24:53 gorbag>

## Future Goals 

Right now, loading the VHDL into tools appropriate for generation of
an FPGA bitstream file is manual. Additionally, constraints such as
for the clock speed, clock type, etc. has to be done manually (despite
the configure-clock macro which is supposed to automate that but
currently only affects the Lisp simulation).

The idea is that these macros can also construct appropriate XDC
(Xilinx Design Constraints, because I'm using an Arty-A7 right now)
and possibly Tcl statements to drive Vivado to build the bitstream
without having to run the interactive IDE (which is large, slow, and
most "project" files are unsuitable for source control).


