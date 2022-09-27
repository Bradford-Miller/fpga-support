# VHDL Generation Notes
### Time-stamp: <2022-09-13 11:13:11 Bradford W. Miller(on Boromir)>

## Goals
While building the simulation environment for a processor, we want to be able
to automatically create appropriate VHDL to program the FPGA for not only the
processor, but required ancillary devices (more of a SOM or System On Module
than just a processor in other words). Many FPGAs can be aquired as a SOM so in
some sense this is a matter of describing the existing SOM and providing
(links) to any IP or VHDL needed by the FPGA to interface to it.

Because the processor-specifying lisp code is more delarative than functional,
we can use several techniques for generating VHDL:

1. The (Common-Lisp) macro that is used to declare the relevant part of the
   architecture can write the VHDL file directly (the first version of the
   library in this directory was written with that in mind).
2. The (CL) macro or relevant functions can create a database of information
   needed to construct the VHDL and a later function call can be used to
   actually generate the VHDL (this is the approach I'm currently leaning
   toward).
3. We can do a code walk of the declarative code with a separate routine to
   generate the VHDL. This treats the declaration as a separate sublanguage
   which can then be either used to generate the simulation code or the VHDL
   code depending on which code-walker we use. Given CL's macro capabilities,
   this seems more trouble than it's worth, but would have the most flexibility
   (certainly nothing prevents us from interpreting it as if they are macros
   and hand them off to CL anyway).

## Discussion
The current approach to simulating Scheme-79 separated the functionality that
would occur on a given clock cycle from the phase of the clock that would
invoke it. That makes simulating in lisp easy: we just run all the functions
associated with each clock phase (once) and step through the clock phases. That
means we can take the phase that triggers a function as a variable (well we use
parameters, but same difference) which allows us to experiment with the
simulation to determine the best point to invoke a function and to easily make
changes to that in the experimental simulation environment. However, when we're
writing VHDL the VHDL code is triggered by changes to signals (including the
clock) which have to be known at the point we emit the code AFAICS. If we want
to preserve the ability to segregate the process from the trigger, we have to
emit the code separately from specifying it, allowing us to make changes to
that binding and then reemitting the code without having to modify possibly
every declaration that would fix that relationship at the time it is
interpreted.

While that is somewhat more work than just emitting VHDL as we interpret each
declaration, (it is somewhat equivalent to backpatching) it allows us to
maintain the flexibilty of the initial simulation environment with little
perterbation to it, other than standardizing certain operations such as
assignment of a process to a clock phase and possibly limiting the clock phases
that can be assigned to (although given we have PLLs we can subdivide the clock
as much as we like, that is, if the simulation used a single clock but used the
rising edge, the falling edge and both the high and low parts of the clock
cycle (not a good idea but may have been needed to get things working given
we're not actually simulating electronics just functional results) we can
multiply our clock by 4 and then split it into 4 separate clock cycles
assigning everyting to the rising phase of the appropriate subclock. (This is
for illustrative purposes only, I don't expect we will need to go this far;
most of the issue with the simulation environment is ordering of fns which
should "self correct" if they run simultaneously and we wait until the end of
the clock period for it to settle, but that does imply that the clocks for
simulation and VHDL might differ so we will need to handle that. Alternatively
we could also add explicit buffers to the simulation code so order would matter
less (the state of the input would be captured at the end of the prior clock
and functions would work from that rather than the current state; this is
somewhat closer to how the hardware works as well, I expect).)

## Issues
Some issues with our goals: if our goal was to have a more or less perfect
replica of a given processor, it falls short because we won't have the same
pin-out (or even pin availability) as the original chip. By this I don't mean
we won't necessarily have enough pins (FPGAs usually have plenty and one can
spend more and get a larger one if needed) but that the kinds of peripheral
devices that would have been external to the chip (in say, 1979) would now have
completely different interfaces with modern devices. Since my goal (at least)
is to recreate the processing technology and not the board-level technology of
the period (at least with the scheme retroprocessor work), that implies that
some devices that would have been external may now map to different external
devices with different interface requirements or simply get implemented on part
of the FPGA fabric.

For the Scheme-79 project, for instance, the external pins of that chip, which
are simulated by the lisp processor, may be retained as internal-to-the-FPGA
signals but then get mapped approrpriately for the DDR-3 driver that talks to
the memory on the Digilent Arty-A7 board I am using. Similarly, rather than the
external 2-phase non-overlapping clock the original Scheme-79 chip used, I will
be using the internal resource of the Artix-7(TM) chip.

I also plan at this point to connect to some PMOD devices to aid in observation
and debugging of the processor, for which the drivers will need to be part of
the VHDL and appropriate connections to processor activity made. This might
require additional microcode beyond the original loadout or perhaps monitoring
some of the internal signals to drive PMOD displays, and writing PMOD inputs to
special memory addresses (requiring microcode support) or directly entering
information into the registers or memory. (The original Scheme-79 chip does
have microcode support for reading and writing register contents onto the
memory bus, however, they also had the advantage of having an interface to a
lisp machine to supply S-Code and monitor the execution of the chip; we may try
to use the USB/JTag link for similar effect but obviously this is NOT a direct
recreation of the original functionality or interface. Hopefully, in some
sense, it is better :).)

## Documentation

Note that library code in the subdirectory sim_lib can be included via the
usual mechanisms in VHDL, e.g.,

``` VHDL
library sim_lib
use sim_lib.clock_nonoverlapping_twophase.all
```

(nonworking example) which is supported in lisp by using defentity or
defcentity (the latter supports more than one library like ieee).

My original plan was to supply two libraries, one, sim-lib, to be used for
testbench support in simulation (for example, specifying appropriate clock
signals using VHDL), and one, lib, to be used for synthesizable code (e.g.,
where the clock will be generated by a black box and thus does not require VHDL
support but may require XDC or other appropriate declarations to the
synthesizer).

However at the current time, I have been writing VHDL tests (under the tests
directory) manually (rather than generating from lisp) to explore the code that
should be generated by the library. It is not yet synthesizable (though I hope
to get there) and is not, in general, in the format the final code will take,
it is just a series of experiments. Eventually I expect to only generate
synthesizable VHDL, and use (manually constructed?) testbenches for simulation
runs (where, e.g., the testbench generates the clock signals or pretends to be
external memory, etc.)

