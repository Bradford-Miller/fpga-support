#Lisp support for defining FPGA structures
simulation support code and firmware compilation

Time-stamp: <2022-02-08 13:26:10 gorbag>

After some initial work on simulating the Scheme-79 chip (see the
project files), code that could be reused for other such FPGA projects
is being segregated here. In particular, code for compiling and
assembling uPLA code (which is used to populate on-chip programmable
logic arrays), code needed for specifying clocks, registers, etc. that
should allow us to compile a spec for a chip into HDL that is suitable
for conversion into a bit stream for an FPGA (e.g., by presenting the
HDL to such tools, not to replicate those tools).

## Assumptions

Note our code here makes a few assumptions about the FPGAs we are going to
build (it is NOT a generic Lisp->HDL library, though some parts may be suitable
for such use in the future!)

* We are going to describe processors (i.e., pure digital components, not
  analogue hardware), and common peripherals (allowing SOCs for a sufficiently
  limited conscept of system).
* We will emphasize a declaratory approach to describing the hardware to the
  extent possible
* The kind of processors we describe are expected to be microprogrammed
  * Our microprogramming language will be some version/extension of MIT's
    "microlisp" though probably not always just what was used to construct the
    Scheme-79 chip
  * Microprograms will be compiled and/or assembled into a PLA (programmable
    logic array), that is, an array of bit-vectors suitable for
    loading/compiling into the FPGA. Again I will be starting off with
    something that's as close as I can (reasonably) get to what I think MIT
    used for Scheme-79 (given published references), but given the sometimes
    sketchy documentation I'm going to take some liberties.

    
## Design Goals
* To the extent possible, we will want 'the same code' (again mostly
  declaratory) to generate HDL that specifies the FPGA, and also the simulation
  of that component (under the host Lisp).
* Specifics about microprogram constants (e.g. register names and
  characteristics) will be declaratory, thus part of both the
  'microprogrammer's model' and the actual hardware realization.
    
Status:

####1-11-22 BWM

I've segregated and repatriated more code from the simulation directory,
particularly dealing with microlisp, and made a number of the places where the
split between 'project code' and 'general declaration of processor code' into
generic functions, i.e. the generic code may have high level or default methods
for, e.g. compiling a function and the project then adds methods to compile
code specific to the project. Most of these interfaces have been listed in the
file project-defs.lisp

####11-19-21 BWM

Additional directories for gui support (simulation) and pla-support (offline
tools for generating code for PLAs, i.e.  microcode or in the case of Scheme-79
at least, also nanocode). The idea is that these tools are run to compile /
assemble microcode, but then the resultant code (which should be an array of
bit vectors) gets "downloaded" into the FPGA chip as a read-only memory array.

####10-21-21 Bradford W. Miller

Segregating code into that which supports both simulation and translation into
HDL here.

## Contact

Ideally, posting bugs in the project or repository would be the ideal way to
contact me (even if it's just a misunderstanding of the documentation).

