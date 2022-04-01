# Common Lisp support for defining FPGA structures, simulation support code and firmware compilation

Time-stamp: <2022-03-24 14:02:48 gorbag>

After some initial work on simulating the Scheme-79 chip (see the project
files), code that could be reused for other such FPGA projects is being
segregated here. In particular, code for compiling and assembling uPLA code
(which is used to populate on-chip programmable logic arrays), code needed for
specifying clocks, registers, etc. that should allow us to compile a spec for
a chip into HDL that is suitable for conversion into a bit stream for an FPGA
(e.g., by presenting the HDL to such tools, not to replicate those tools).

## Loading / Running the FPGA

(This is evolving)

Right now, I'm using [GHDL](https://ghdl.github.io/ghdl/index.html) since it works on
my development Mac and I don't have to keep a VM running with Linux (but will
eventually do so to run Vivado to program an
[Arty A7](https://digilent.com/shop/arty-a7-artix-7-fpga-development-board/). Eventually
I will include scripts needed to either run the simulation using ghdl or
vivado, but for now you'll have to figure this out on your own (note that
vivado can be run from the command line or a batch file:


```
vivado -mode tcl # command-line mode
vivado -mode batch -source tcl-script.tcl # scripted mode
```

Note that it may be possible to use https://openocd.org both to load the
bitstream and support debugging from the Mac as well, but other than the link
I don't (yet) know much about it.

I am manually running... (TBD)

## Loading / Running the Simulation on the host

Right now, the macros generate everything needed to build a simulation on the
host machine (in Lisp code). See the Scheme-79 project for an example of usage
and running the machine (which consists of setting up the RAM with some
content, triggering the Power-On-Reset, and then starting the clock simulation
either by microtick, tick, or just allowing it to rip until a breakpoint is
reached). The test function does this for a particular microcode and RAM
content, so look at tests/test-0.* for a more specific example of this.

More detail to come! (TBD)

## USAGE

Documentation for the individual macros to come here. (TBD)

## Assumptions

Note our code here makes a few assumptions about the FPGAs we are going
to build (it is NOT a generic Lisp->HDL library, though some parts may
be suitable for such use in the future!)

* We are going to describe processors (i.e., pure digital components,
  not analogue hardware), and common peripherals (allowing SOCs for a
  sufficiently limited conscept of system).
* We will emphasize a declaratory approach to describing the hardware
  to the extent possible
* The kind of processors we describe are expected to be microprogrammed
  * Our microprogramming language will be some version/extension of
    MIT's "microlisp" though probably not always just what was used to
    construct the Scheme-79 chip
  * Microprograms will be compiled and/or assembled into a PLA
    (programmable logic array), that is, an array of bit-vectors
    suitable for loading/compiling into the FPGA. Again I will be
    starting off with something that's as close as I can (reasonably)
    get to what I think MIT used for Scheme-79 (given published
    references), but given the sometimes sketchy documentation I'm
    going to take some liberties.
    
## Design Goals
* To the extent possible, we will want 'the same code' (again mostly
  declaratory) to generate HDL that specifies the FPGA, and also the
  simulation of that component (under the host Lisp).
  * That means that we can simulate to some degree the intent with a lot of
    tools for looking at the registers, single stepping at the nanocode level,
    poking through memory, using the Lisp simulation.
  * It also means we will be able to SEPARATELY simulate the VHDL using an
    appropriate tool (e.g., ghdl) to see if the behavior is correct / matches
    the lisp simulation. It is probably the case we won't have the same
    flexibility we will have with the Lisp simulation, e.g. source-level
    microcode and nanocode debugging in the console.
    * However, we may want to add communications between the FPGA
      (e.g. through the USB/JTAG interface) and use that to have a "remote"
      front-panel that can do such inspection (presumably needing some
      additional functionality on the FPGA in terms of IP or other ways to do
      the monitoring). There are commercial packages for this (with certain
      development boards) and OpenOCD might be of use here as well.
* Specifics about microprogram constants (e.g. register names and
  characteristics) will be declaratory, thus part of both the
  'microprogrammer's model' and the actual hardware realization.


    
## Status:

Note the TODO.txt file documents specific tasks that are planned (in
some sense ;-) or previous TODO items that have been completed.

#### 3-18-22 BWM

test-3 now runs in the s79-simulator directory, so I'm incrementing the
minor release version to 0.2. The next milestone will be to produce FPGA
code so expect a lot of changes to this library!

#### 1-11-22 BWM

I've segregated and repatriated more code from the simulation
directory, particularly dealing with microlisp, and made a number of
the places where the split between 'project code' and 'general
declaration of processor code' into generic functions, i.e. the generic
code may have high level or default methods for, e.g. compiling a
function and the project then adds methods to compile code specific to
the project. Most of these interfaces have been listed in the file
project-defs.lisp

#### 11-19-21 BWM

Additional directories for gui support (simulation) and pla-support
(offline tools for generating code for PLAs, i.e.  microcode or in the
case of Scheme-79 at least, also nanocode). The idea is that these
tools are run to compile / assemble microcode, but then the resultant
code (which should be an array of bit vectors) gets "downloaded" into
the FPGA chip as a read-only memory array.

#### 10-21-21 Bradford W. Miller

Segregating code into that which supports both simulation and
translation into HDL here.

## Contact

Ideally, posting bugs in the project or repository would be the ideal
way to contact me (even if it's just a misunderstanding of the
documentation).

