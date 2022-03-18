# FPGA-Support Library Hardware Descriptions

Time-stamp: <2022-02-08 13:31:39 gorbag>

This directory contains hardware descriptions needed by the FPGA-Support
library both to pass on to the underlying HDL tools as well as to provide
appropriate simulated versions (if needed).

The hope is that abstracted versions of the hardware (which get supported via
HDL) can be made available to the project(s) but drivers, etc. for lower level
interfaces would still be project-specific. Ideally, we would be able to
implement these drivers in either PL or PS if we have an appropriate chip
architecture (e.g., ZYNQ) which is one reason to keep that part of the
definition in the project rather than autogenerate (which would have to be able
to, e.g., create microlisp operators which is currently done only at the
project level).


### Limitations: 

Note that the current implementation will create CAPI (LispWorks UI) code only
for the simulation, just as the rest of the library does.
