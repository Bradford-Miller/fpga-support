-- --------------------------------------------------------------------
--                                                                   --
-- Temporary pad support to help develop pad code                    --
--                                                                   --
-- Time-stamp: <2022-09-01 11:57:20 Bradford W. Miller(on Boromir)>  --
--                                                                   --
-- --------------------------------------------------------------------

-- Note that VHDL doesn't deal with assigning signals to actual IO pads on the
-- FPGA, and we'll have to use (vendor) tools for that, or at least set up
-- appropriate mapping files for our board. However we can develop
-- the signals (setup and hold time, etc.) and then separately decide on pad
-- assignment (manually or through a tool).

-- Note that while these signals were external pads on the original chip, in
-- some cases they may be internal only for us. That's because, e.g., our
-- clock is internally generated, our memory is on-board, DDR3 rather than
-- external, and thus we will use provided IP for their interface, but we can
-- develop the normal timing and signals of the chip and then convert those
-- for use in the provided IP. Consider it a "SOC" where the original chip
-- we're interested in is part of the FPGA and the support circuits that
-- would have been on the board that chip was installed onto is now also
-- absorbed by the FPGA fabric...

-- As with other VHDL files, this is to help figure out what VHDL the lisp
-- macros should generate (possibly via a separate generator pass after storing
-- away the important parameters through the declarations). Unlike our lisp
-- implementation, we can't just use variables for which clock (phase) to
-- trigger on, etc. and will have to bind that before generating the VHDL. The
-- point being that many things used as "constants" here, like
-- "rising_edge(clk1)" may end up being something different depending on the
-- output from the VHDL generator invoked when building the (lisp) project.

-- showing the (original) declarations here for translation. Also include
-- "internal pads" like conditional though obviously they are just signals, not
-- pads (we'll put them somewhere else, eventually)

-- NB: naming conventions for VHDL and lisp are obviously
-- different. Registers and pads are names as if they were special variables
-- (which in some sense they are) for the lisp simulator, but in VHDL names
-- cannot have special characters other than underscore (which may never lead
-- or trail alphanumeric characters). The first character must also be an
-- alpha! So for our purposes we will map these (for now) from *<name>* to
-- reg_<name> or pad_<name> and any interial dash will be replaced with an
-- underscore.


-- --------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- library work;
use work.regpkg.all;
use work.padpkg.all;

entity chip_master is
  generic (
    Simulation_on :boolean := false; -- not currently using
    Word_width    :integer := 32);

  port (
    -- inputs from FPGA logic
    clk1                  : in     std_logic;
    clk2                  : in     std_logic;
    clk1a                 : in     std_logic;
    clk2a                 : in     std_logic;
    rst                   : in     std_logic;

    my_pad_controls       : in     pad_controls; -- defined in padpkg.vhdl

    -- outputs to FPGA logic
    freeze_p              : out    std_logic := '0';
    read_state_p          : out    std_logic := '0';
    load_state_p          : out    std_logic := '0';
    reset_p               : out    std_logic := '0';
    interrupt_p           : out    std_logic := '0';
    
    -- outputs from chip to board (or other FPGA logic)
    pad_ph1               : out    std_logic := '0'; -- clock
    pad_ph2               : out    std_logic := '0'; -- clock
    pad_ale               : out    std_logic := '0'; -- address latch enable
    pad_read              : out    std_logic := '0';
    pad_write             : out    std_logic := '0';
    pad_cdr               : out    std_logic := '0';
    pad_read_interrupt    : out    std_logic := '0';
    pad_gc_needed         : out    std_logic := '0';

    -- inputs from board to chip (or other FPGA logic)
    pad_freeze            : in     std_logic;
    pad_read_state        : in     std_logic;
    pad_load_state        : in     std_logic;
    pad_reset             : in     std_logic; -- not in original chip!
    pad_interrupt_request : in     std_logic;

    -- IO to/from board (or other FPGA logic)
    -- note that pads_memory_bus, being an EXTERNAL bus (at least in the
    -- original chip) really should be an inout.

    -- should be external to the FPGA but maybe not really...  (we could use
    -- the DDR3 driver, in which case inout would not be supported). One
    -- option would be to encapulate this chip_master or some other lower
    -- level entity that provides the inout semantic if need be...

    -- moved to mux driver code
    -- pads_memory_bus       : inout  s79_word := s79_word_ignore; 

    -- pseudo pads (internal signals);
    conditional_p         : out    std_logic := '0';
    run_nano_p            : out    std_logic := '0';
    mask_interrupt_p      : out    std_logic := '0');

end entity chip_master;

architecture behav_chip_master of chip_master is
  -- local signals and variables
  signal gc_needed_latch  : std_logic := '0';
  signal mask_interrupt_latch : std_logic := '0';
  signal run_nano_latch : std_logic := '0';
  
begin
  -- output clocks (hopefully combinatoric) these are tied to our clock (clk1
-- and clk2), which will be generated via "black box" on the fpga and through
-- the test bench on the VHDL simulation. That means there's really nothing
-- we need to do here...

-- (defchip-pad *ph1* :output :ph1-rising :any 2) ; input in original. 
-- (defchip-pad *ph2* :output :ph2-rising :any 2) ; input in original. 

  pad_ph1 <= clk1;
  pad_ph2 <= clk2;

-- we don't need to output the offset versions - those are internal only (at
-- this point, anyway!)

-- freeze; causes chip to inhibit any state change, it must be stable
--   during ph1. Inhibits from-x and to-x controls in the register
--   array, and causes the two state machines to refech their current
--   state. Control outputs should be ignored during frozen cycles.

-- (defchip-pad *freeze* :input :any :any 8) -- full tick (minimum) hold, so
--                                              8 "phases"
  Freeze_Control_Proc: process(clk2)
    begin
    if falling_edge(clk2) then
      freeze_p <= pad_freeze; -- only update on clk2 so stable during clk1 So
                              -- also meets "full tick hold" time.
    end if;
  end process Freeze_Control_Proc;

-- read-state; should be used with freeze asserted. reads out current 
--   MICRO state onto 9 bits of the chip bus. (bits 23, 22,
--   30-24). The chip can be single stepped by lowering freeze for one
--   cycle and the internal data observed on the chip-bus. There are
--   special sequences in the microcode that have been provided just
--   for scanning in and out the internal registers, so that a
--   load-state to the right state followed by single stepping the
--   chip will accompish an examine or deposit from the "switches".

-- (defchip-pad *read-state* :input :ph2-rising :ph2 3)

  Read_State_Control_Proc: process(clk2)
    begin
    if falling_edge(clk2) then
      read_state_p <= pad_read_state;
    end if;
  end process Read_State_Control_Proc;

-- load-state; should be used with freeze asserted. Sets the microcode
--   state number from 9 bits of the chip-bus. (bits 23, 22, 30-24)

-- (defchip-pad *load-state* :input :ph2-rising :ph2 3)

  Load_State_Control_Proc: process(clk2)
    begin
    if falling_edge(clk2) then -- if input changes in ph2-rising, we have to
                               -- check when it is falling
      load_state_p <= pad_load_state;
    end if;
  end process Load_State_Control_Proc;

-- [reset] ; I don't see this in the AIM, but there is an associated
--   microaddress for reset, and I'm not sure we can always count on the chip
--   powering up in a reset state (or we may want to force a reset, no? Seems
--   like something useful to have on the board).  [Perhaps they used
--   "load-state" to force a reset, but for simplicity, I'm adding it and
--   will add a reset switch to the console as well. - BWM 1/13/21] [Also
--   note we are not really constrained by their original pin count
--   limitations! - BWM 5/5/22]
  
-- (defchip-pad *reset* :input :any :ph1-rising 5)
  Reset_Control_Proc: process(clk1)
    begin
    if falling_edge(clk1) then -- clk1 following lisp simulator, may want to
                               -- make clk2 for consistency?
      reset_p <= pad_reset;
    end if;
  end process Reset_Control_Proc;

-- (add-initialization "Check Reset Pad"
--                     '(when (test-pad '*reset*)
--                       (do-reset))
--                     nil
--                     '*ph1-falling-list*)

-- interrupt-request; when raised and when microcode checks, an
--   interrupt point is created which encapsulates the state of the
--   current process, and a read-interrupt cycle is performed to
--   obtain an address from the interrupting device. The CAR of this
--   address should be a user procedure of one argument (the
--   interrupt-point) which will handle the interrupt. It is assumed
--   there are i/o locations that control interrupt priority level and
--   re-enabling the interrupt system.

-- note this should be held until read-interrupt is raised so 3 is just a
-- minimum:
  
-- (defchip-pad *interrupt-request* :input :any :any 3) 

  Interrupt_Request_Control_Proc: process(clk2)
    begin
    if (falling_edge(clk2) and (mask_interrupt_latch = '0')) then
      interrupt_p <= pad_interrupt_request;
    end if;
  end process Interrupt_Request_Control_Proc;
      
-- outputs:

-- "The external world is conceptualized as a set of registers with special
-- capabilities."  "The external ADDRESS register is used for accessing
-- memory and can be set from the bus."  [looks like a register but it's
-- not?!]

-- (defchip-reg *address*)
-- (defureg *address* (to from) ())

-- "The pseudo-register MEMORY can be read onto the bus or written from the
-- bus. The actual access is preformed to the list cell addressed by the
-- ADDRESS register. The CDR bit controls which half of the cell is being
-- accessed."

-- (defchip-reg *memory*)
-- (defureg *memory* (to from) ())

-- [but we treat the pads as separate so we can enforce timing constraints on them]

-- (defchip-pads <pad-group-name> <number-of-wires> <type> <setup-phase>
--               <sample-phase> <hold-phases> [ <shared-pads> ])

-- so it looks like we can simplify memory-pads, address-pads and interrupt
-- into one pad process! (will need to update the PLA appropriately - but I
-- think that's consistent with the original TR! (TBD))

  -- NB: in the paper we show start of data assertion on rise of ph1 and end on
  -- drop of ph1. What we want it so make sure it is valid on drop of ph1
  -- (because good FPGA programming practices dictate that things should stay
  -- valid on a clock change we are going to sample on) so we assert on rise of
  -- clk1a (to allow the mux to run to get a valid input) and drop on fall of clk1a.

  -- Bottom line, if we read on drop of clk1, the memory bus should be valid,
  -- and if we assert data on rise of clk1 (for write) it will get there in
  -- time. UNFORTUNATELY mux delays the signal by a half phase SO this code is
  -- bing moved INTO the mux logic (to avoid the delay!)

  /*
  Memory_Pads_Control_Proc: process(clk1, clk1a)
    variable im_driving_membus : std_logic := '0';
    
  begin
    if (rst = '1') then
      obus <= output_bus_init;
    elsif falling_edge(clk1a) then -- delay until falling edge of clk1a so we
                                   -- know it is valid during falling_ege(clk1)!

      if (im_driving_membus = '1') then
        im_driving_membus := '0';
      end if;
      
      pads_memory_bus <= s79_word_ignore;
      
    elsif rising_edge(clk1a) then -- rising clk1 is when ibus becomes valid, so
                                  -- we have to wait until after that but
                                  -- before falling clk1, which is what clk1a
                                  -- is for!
      if (my_pad_controls.set_memory_pads = '1') then
        pads_memory_bus <= ibus.shared_bus_data;
        im_driving_membus := '1';
      else
        pads_memory_bus <= s79_word_ignore;
      end if;
      
      if (my_pad_controls.get_memory_pads = '1') then
        obus.output_bus_data <= pads_memory_bus;
      else
        obus.output_bus_data <= s79_word_init;
      end if;
      
    end if;

  end process Memory_Pads_Control_Proc;
  */
-- (defchip-pads *memory-pads* *word-size* :io
--   *put-memory-content-onto-pads* *get-memory-content-from-pads* 1)

-- (defchip-pads *address-pads* *address-field-length* :io
   -- since we reuse the low order bits, just pretend they're the same
--   *put-memory-content-onto-pads*  *get-address-from-pads* 1 *memory-pads*) 

-- "One more external register, INTERRUPT, which can be read onto the
-- bus, contains the address of a global symbol whose value (its CAR)
-- is an appropriate interrupt handler."

-- (defchip-reg *interrupt*)
-- (defureg *interrupt* (from) ())

-- (defchip-pads *interrupt-pads* *address-field-length* :input
--               *put-memory-content-onto-pads* *get-address-from-pads* 1 *memory-pads*) ;
-- like the address pads

-- ale; address-latch-enable; 24 bit node address on low-order bits of
--   chip bus and ALE asserted, then a following cycle asserts either
--   read or write to access the node, simultaneously specifying which
--   half (CAR or CDR) of the node with the cdr signal. If the memory is
--   slower than the chip, it should assert freeze until the cycle where
--   the memory can complete the memory operation. See figures 8 & 9.

-- (defchip-pad *ale* :output *run-nanocontroller-p1*
--              *run-external-data-transfer* 8) ; keep high for full tick

-- delay to clk2a rising edge (from clk2 rising edge) as the request to set_ale
-- is typically on rising clk2. 8-31-22
  
  ALE_Pad_Control_Proc: process(clk2, clk2a)
  begin
    if (rising_edge(clk2) and (my_pad_controls.set_ale = '0')) then
      pad_ale <= '0';     
    elsif (rising_edge(clk2a) and (my_pad_controls.set_ale = '1')) then
      pad_ale <= '1';
    end if;
  end process ALE_Pad_Control_Proc;

-- read (see above)

-- per Figure 8 of AIM559:
  
-- read cycle has ALE asserted during ph2, and dropped on falling edge of next
-- ph2. During ph1 address data is transmitted. THen on following ph2 read is
-- asserted and the content follows during ph1. If freeze is asserted when read
-- is then the freeze should drob before the following ph2 and data then gets
-- sent on ph1 (read stays asserted during freeze)

-- NB: nanocode splits this into two nano cycles, the first establishes the
-- address and ALE, the second sets up the receiving register and asserts READ

-- (defchip-pad *read* :output *run-nanocontroller-p1*
--              *run-external-data-transfer* 8) ; during the following phase 1 the data
-- read should be placed on the pads

  Read_Pad_Control_Proc: process(clk2, clk2a)
  begin
    if (rising_edge(clk2) and (my_pad_controls.set_read = '0')) then
      pad_read <= '0';
    elsif (rising_edge(clk2a) and (my_pad_controls.set_read = '1')) then
      pad_read <= '1';
    end if;
  end process Read_Pad_Control_Proc;

-- write (see above)

-- per Figure 9 of AIM559:

-- Same as read but assert write. Also shows CDR asserted during ph2 to get the
-- cdr during folowing ph1.

-- (defchip-pad *write* :output *run-nanocontroller-p1*
--              *run-external-data-transfer* 8) ; similar to read but the following ph1
-- will have the data to write on the pads

  Write_Pad_Control_Proc: process(clk2, clk2a)
  begin
    if (rising_edge(clk2) and (my_pad_controls.set_write = '0')) then
      pad_write <= '0';
    elsif (rising_edge(clk2a) and (my_pad_controls.set_write = '1')) then
      pad_write <= '1';
    end if;
  end process Write_Pad_Control_Proc;

-- cdr (see above)

-- (defchip-pad *cdr* :output *run-nanocontroller-p1*
--              *run-external-data-transfer* 8)

  CDR_Pad_Control_Proc: process(clk2, clk2a)
    begin
    if (rising_edge(clk2) and (my_pad_controls.set_cdr = '0')) then
      pad_cdr <= '0';
    elsif (rising_edge(clk2a) and (my_pad_controls.set_cdr = '1')) then
      pad_cdr <= '1';
    end if;
  end process CDR_Pad_Control_Proc;

-- read-interrupt (see interrupt-request - interrupting device should
--   supply an address)

-- (defchip-pad *read-interrupt* :output *run-nanocontroller-p1*
--              *run-external-data-transfer* 8) ; treat like a read

  Read_Interrupt_Control_Proc: process(clk2, clk2a)
  begin
    if (rising_edge(clk2) and (my_pad_controls.set_read_interrupt = '0')) then
      pad_read_interrupt <= '0';
    elsif (rising_edge(clk2a) and (my_pad_controls.set_read_interrupt = '1')) then
      pad_read_interrupt <= '1';
    end if;
  end process Read_Interrupt_Control_Proc;

-- gc-needed ; tied to allocation = memory limit, and allows external
--   interrupt to supply an address of a procedure to handle gc. The
--   s-code primitive 'mark' clears the internal gc-needed flip-flop
--   and performs a gc.

-- because gc-needed is a latch, also set up a variable to store the state of
-- that latch (*gc-needed-latch*)

-- (defchip-pad *gc-needed* :latched-output *run-nanocontroller-p1*
--              *run-external-data-transfer* 7)

  GC_Needed_Control_Proc: process(clk1, clk1a, clk2, clk2a) -- sensitive to any
                                                            -- clock
  begin
    -- this should execute on any clock transition
    if (my_pad_controls.set_gc_needed = '1') then
      gc_needed_latch <= '1';
    elsif (my_pad_controls.clear_gc_needed = '1') then
      gc_needed_latch <= '0';
    end if;
    
    if rising_edge(clk2) then
      pad_gc_needed <= gc_needed_latch;
    end if;
  end process GC_Needed_Control_Proc;
    
-- ------------------------------------------------------------------------------
-- internal signals

-- pseudo pad to deal with conditionals - really switches MUX on least
-- order bit of micro-pc to OR of relevant conditional wires

--nano runs on :ph2-rising, and we keep high until we update PC on
-- :ph1-falling
--
--(defchip-pad *conditional* :pseudo *run-nanocontroller-p1*
  --             *update-sense-wires* 7)
  Conditional_Control_Proc: process(clk2)
  begin
    if falling_edge(clk2) then -- delay to falling edge as bit set by nano on
                               -- rising edge
      conditional_p <= my_pad_controls.set_conditional;
    end if;
  end process Conditional_Control_Proc;
  
-- run-nano is the pseudo pad for the internal version of freeze, bascially
-- allows us to run multiple nano cycles without updating the
-- microcode. Unlike the external freeze, which inhibits both micro and
-- nanocycles (and thus does not allow registers to be updated)

-- (defchip-pad *run-nano* :latched-io *run-nanocontroller-p1* :any 8) ; full
-- clock cycle as we may reassert
  Run_Nano_Control_Proc: process (clk2, clk2a)
  begin
    if (my_pad_controls.set_run_nano = '1') then
      run_nano_latch <= '1';
    elsif (my_pad_controls.clear_run_nano = '1') then
      run_nano_latch <= '0';
    end if;
        
    if falling_edge(clk2a) then
      run_nano_p <= run_nano_latch;
    end if;
  end process Run_Nano_Control_Proc;
  
-- instruction DISPATCH-ON-EXP-ALLOWING-INTERRUPTS implies there is some mask
-- of the interrupt-request pad, so we create this pseudo-pad to suppress
-- interrupts.
--
-- (defchip-pad *mask-interrupts* :latched-io :any :any 3) ; should match up
-- with *interrupt-request*
  Mask_Interrupt_Control_Proc: process (clk1, clk2)
    begin
    if (my_pad_controls.set_mask_interrupt = '1') then
      mask_interrupt_latch <= '1';
    elsif (my_pad_controls.clear_mask_interrupt = '1') then
      mask_interrupt_latch <= '0';
    end if;

    if falling_edge(clk2) then
      mask_interrupt_p <= mask_interrupt_latch;
    end if;
  end process Mask_Interrupt_Control_Proc;
end architecture behav_chip_master;
