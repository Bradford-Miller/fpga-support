# VHDL Generation Tests
#### Time-stamp: <2022-05-10 14:45:48 gorbag>

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

2. observe with gtkwave (there should be a GHW file in your current directory)
   
### 1reg

This was my first attempt to define a register and what will ultimately become
a shared bus (the only internal bus in scheme-79). The simulation was
hand-checked for validity.

### 3reg

This is an extanded test that uses shared code from 1reg (../common) and made
sure it would work if registers were read and written to via the bus as the
same time (which did require some tweaks :-). This should be a reasonable start
for what we should generate for various field definitions, registers, the bus,
etc. and should be driven from the lisp declarations in s79-simulator. Again,
the timing was hand-checked for validity.

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
