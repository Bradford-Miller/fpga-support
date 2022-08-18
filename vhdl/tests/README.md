# VHDL Generation Tests
#### Time-stamp: <2022-06-03 11:45:46 Bradford W. Miller(on Boromir)>

For the most part, this directory contains handwritten tests in VHDL that can
be run in ghdl and analyzed with gtkwave. The intent is to explore what the
code should look like when the lisp functions pump out VHDL for various
declarations (e.g. fields in the word, register capabilities, pad setup and
hold times, etc.)

## Manually running tests
Each of the directories below contain a makefile

1. Analyze and build each file in order, e.g. (for 1reg)

	```
    make init
    make elab
    make run
	```

2. observe with gtkwave (there should be a GHW file in your current directory). 
   
### 1reg

This was my first attempt to define a register and what will ultimately become
a shared bus (the only internal bus in scheme-79). The simulation was
hand-checked for validity.

Note that this test (as does the others) currently utelizes tristate
busses. Since that isn't really internally supported by current FPGA chips I
will need to reimplement using a MUX strategy such as that used by common more
modern busses like AXI or Wishbone. However as these are really outside the
spirit of the original single bus on the Scheme-79 chip I expect to roll my own
version that, while it cannot count on having pullup or pulldown resistors for
a tristate bus, does simplify the protocol so it is driven the same way the
chip did, through the microcontrol (and nanocontrol) PLA(s). It may also mean
that the bus will have to be split so each direction to the components are
separate (i.e. incoming vs outgoing vs. the single bus of the chip) with the
outgoing busses then muxed and selected to gate onto the incoming bus. This may
also introduce a clock delay that would not have been seen with tristate logic,
but I'll wait until I implement it and update this comment (TBD).

### 3reg

This is an extanded test that uses shared code from 1reg (../common) and made
sure it would work if registers were read and written to via the bus as the
same time (which did require some tweaks :-). This should be a reasonable start
for what we should generate for various field definitions, registers, the bus,
etc. and should be driven from the lisp declarations in s79-simulator. Again,
the timing was hand-checked for validity.

See the comment under 1reg for dealing with the tristate bus (TBD).

### pads

Here are some experiments with pads that are needed on the scheme-79 chip. Note
that for the most part our FPGA will not need to actually assign these to
actual FPGA IOs (which is something we need to do separately from HDL at any
rate) as they will be used to connect to other IP that gets us to, e.g. the DDR
memory on the board. So our implementation of the scheme-79 chip is going to be
virtual in some sense, "internal" to a more complete implementation that would
have been present on a board-level Scheme-79 implementation! Regardless, we
will try to deal with the various setup and hold constraints (in terms of when
signals are established and released) and hopefully that will translate into
what we need to do should they be assigned to actual IO pads.

Note that memory is also simulated in this test, though only for timing
purposes (it doesn't actually read or write anything anywhere).

While developing this code, I noted that the timing from the AIM would be
difficult to replicate in FPGA, e.g., one that shows data present AFTER ph1
rises and stops ON ph1 falling. While in the original (custom hardware)
implementation, it is relatively easy to add, e.g., a series of inverters to
clock signals to delay them (to, e.g. have a signal gated to the bus on ph1
rising but sampled only some time later allowing the signal to stablize), this
is not good practice on an FPGA where signals should be sampled only on clock
edges. As a result, I've had to implement additional shifted phase clocks
(clk1a and clk2a) that are 90 degrees out from the original clock signals and
essentially allow us to sample a signal mid-phase (for the original ph1 and ph2
clock signals). I would expect that in modern practice, one has a single clock
that is run at a high frequency and then divided into necessary phased clocks
to allow appropriate sequencing of processing and in fact this is what the
pad-test.vhdl testbench does! Also to avoid a possible race condition, it means
that the hold time of (external) bus data does not end at the falling edge of
clk1, but is held until the falling edge of clk1a (so it should always be valid
on falling_edge(clk1).

I will replicate this strategy in the lisp simulator as the declarations are
modified to generate VHDL as well as lisp code (5-17-22: TBD). See also the
comment under 1reg for dealing with the tristate bus (TBD).

### plas

While the original Scheme-79 chip used a PLA (programmable AND columns and OR
rows) for microcode, we will rely on the FPGA tools to pick the appropriate
representation for the microcode. Here we simply define the interfaces that
allow us to execute a given microcode or nanocode array in a representation
similar to what was used on the orignal chip (multiple binary fields in a given
microcode or nanocode "word"). 

Our test consists of a few simple microinstructions and nanoinstructions
inspired by what we used on the lisp simulator, specifically that which we
already tested under the pads directory: reading and writing to memory. While
the memory controller itself will be scripted as before, the micro and
nanocontroller should be a proper subset of what we need for the actual
scheme-79 implementation and exercise control of registers and pads as well as
multiple nanocycles within a single microcycle (by freezing it using the
run_nano signal) just as the chip does when reading and writing to memory.

Even constructing this limited version does indicate that some special cases
used for expediency in the lisp simulator will need to be regularized to be
constructed by the declarative macros we hope to leverage to build both the
lisp simulation and VHDL implementation of a processor.
