-- ------------------------------------------------------------------------
--                                                                       --
-- Testbench for my-plas and associated structures for micro and nanocode--
--                                                                       --
--   Time-stamp: <2022-10-12 12:08:24 Bradford W. Miller(on Boromir)>    --
--                                                                       --
-- ------------------------------------------------------------------------

-- here we pull in our prior defined registers, bus, and pads to let the code
-- in our pseudo-PLA drive them rather than the inline code we had in our
-- prior test benches. We aren't trying to implement the scheme-79 test
-- directory here, but rather just enough to allow us to see how our nano and
-- microcontroller should turn into appropriate VHDL for implementation and
-- the array definitions that will be leveraged as appropriate for the actual
-- micro and nanocontroller arrays.

-- We will implement a few simple microinstructions and nanoinstructions from
-- the lisp simulator, specifically that which we already tested under the pads
-- directory: reading and writing to memory. While the memory controller itself
-- will be scripted as before, the micro and nanocontroller should be identical
-- (other than size) to what we need for the actual scheme-79 implementation
-- and exercise control of registers and pads as well as multiple nanocycles
-- within a single microcycle just as the chip does when reading and writing to
-- memory.

-- 9/19/22 BWM - added explicit test code to make this self-verifying (so we don't
-- have to keep staring at the waveform charts to check that everything is
-- (still) working).

use std.env.all;
use std.textio.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- library work;
-- need regpkg for bus defns
use work.regpkg.all; -- temporary register types
use work.padpkg.all;
use work.plapkg.all; 
      
-- put together a testbed to exercise the plas
-- we will have a couple accumulator registers, the memory and address
-- register, and try a read (car/cdr of same address) and write (car/cdr of two
-- different addresses) and match the wave to what was presented in the AIM.
-- We'll also test a smattering of the other signals, but as most are really
-- the same code we will not be exhaustive.

-- test plas
entity tb_plas_1 is
end entity tb_plas_1;
    
-- it should be enough to run this for 18 ticks 180 ns (last command is tick
-- 16) 
architecture behav_plas_1 of tb_plas_1 is
  component bus_master7
    generic (
      Simulation_on : boolean := false); -- not currently using, but 'just in case'
    port (
      clk1     : in    std_logic;
      clk2     : in    std_logic;     -- two phase clock
      clk1a    : in    std_logic;
      clk2a    : in    std_logic;
      rst      : in    std_logic;

      -- these are about pads, but we put in the mux to save tempo
      my_pad_controls       : in     pad_controls;
      pads_memory_bus       : inout  s79_word; 

      obus     : out   input_bus;
      ibus1    : in    output_bus;
      ibus2    : in    output_bus;
      ibus3    : in    output_bus;
      ibus4    : in    output_bus;
      ibus5    : in    output_bus;
      ibus6    : in    output_bus;
      ibus7    : in    output_bus);
  end component bus_master7;

  component my_register
    generic (
      Simulation_on : boolean := false); -- not currently using, but 'just in case'
    port (
      clk1     : in    std_logic;
      clk2     : in    std_logic;     -- two phase clock
      rst      : in    std_logic;

      name     : in    string(1 to 10);
      tick     : in    unsigned(11 downto 0);

      ibus     : in    input_bus;
      obus     : out   output_bus;
  
      controls : in    register_controls := register_controls_init;
      senses   : out   register_senses);
  end component my_register;

  component chip_master
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
      freeze_p              : out    std_logic;
      read_state_p          : out    std_logic;
      load_state_p          : out    std_logic;
      reset_p               : out    std_logic;
      interrupt_p           : out    std_logic;
    
      -- outputs from chip to board (or other FPGA logic)
      pad_ph1               : out    std_logic; -- clock
      pad_ph2               : out    std_logic; -- clock
      pad_ale               : out    std_logic; -- address latch enable
      pad_read              : out    std_logic;
      pad_write             : out    std_logic;
      pad_cdr               : out    std_logic;
      pad_read_interrupt    : out    std_logic;
      pad_gc_needed         : out    std_logic;

      -- inputs from board to chip (or other FPGA logic)
      pad_freeze            : in     std_logic;
      pad_read_state        : in     std_logic;
      pad_load_state        : in     std_logic;
      pad_reset             : in     std_logic; -- not in original chip!
      pad_interrupt_request : in     std_logic;

      -- IO to/from board (or other FPGA logic)

      -- should be external to the FPGA but maybe not really...  (we could
      -- use the DDR3 driver, in which case inout would not be
      -- supported). One option would be to encapulate this chip_master or
      -- some other lower level entity that provides the inout semantic if
      -- need be...
      -- pads_memory_bus       : inout  s79_word; 

      conditional_p         : out    std_logic;
      run_nano_p            : out    std_logic;
      mask_interrupt_p      : out    std_logic);
  end component chip_master;

  component pla_master
    generic (
      Simulation_on : boolean := false; -- not currently using
      -- from lisp pla declarations (eventually). Reserve 0 for no-op!
      boot_load_address  : ucode_address := to_ucode_address(1));

    port (
      -- inputs from FPGA logic
      clk1              : in     std_logic;
      clk1a             : in     std_logic;
      clk2              : in     std_logic;
      clk2a             : in     std_logic;
      rst               : in     std_logic;

      ibus              : in     input_bus;
      obus              : out    output_bus;
    
      run_nano          : in  std_logic;
      pad_freeze        : in  std_logic;
      pad_conditional_p : in  std_logic;

      ncode_value       : out ncode_word;
      ucode_value       : out ucode_word;

      my_pad_controls   : out pad_controls;

      reg1_controls     : out register_controls;
      reg2_controls     : out register_controls;
      reg3_controls     : out register_controls

      --address_controls  : out register_controls;
      --memory_controls   : out register_controls
      );

    
  end component pla_master;

  -- MUT inputs
  signal clk1                   : std_logic := '0';
  signal clk2                   : std_logic := '0';
  signal clk1a                  : std_logic := '0';
  signal clk2a                  : std_logic := '0';
  signal rst                    : std_logic := '0';
  signal reg1_controls          : register_controls := register_controls_init;
  signal reg1_senses            : register_senses;
  signal reg2_controls          : register_controls := register_controls_init;
  signal reg2_senses            : register_senses;
  signal reg3_controls          : register_controls := register_controls_init;
  signal reg3_senses            : register_senses;
  /*
  signal reg_memory_controls    : register_controls := register_controls_init;
  signal reg_memory_senses      : register_senses;
  signal reg_address_controls   : register_controls := register_controls_init;
  signal reg_address_senses     : register_senses;
  */
  
  signal ibus                   : input_bus;
  signal obus1 : output_bus; -- from reg1
  signal obus2 : output_bus; -- from reg2
  signal obus3 : output_bus; -- reg3
  signal obus4 : output_bus := output_bus_init; -- memory
  signal obus5 : output_bus := output_bus_init; -- address
  signal obus6 : output_bus; -- pla
  
  signal obusme : output_bus; -- test input

  -- pad control inputs
  signal my_pad_controls        : pad_controls := pad_controls_init;

  -- pad control outputs
  signal freeze_p               : std_logic;
  signal read_state_p           : std_logic;
  signal load_state_p           : std_logic;
  signal reset_p                : std_logic;
  signal interrupt_p            : std_logic;

  -- pad inputs
  signal pad_freeze             : std_logic := '0';
  signal pad_read_state         : std_logic := '0';
  signal pad_load_state         : std_logic := '0';
  signal pad_reset              : std_logic := '0';
  signal pad_interrupt_request  : std_logic := '0';
  signal pads_memory_bus        : s79_word := s79_word_init;

  -- pad outputs
  signal pad_ph1                : std_logic;
  signal pad_ph2                : std_logic;
  signal pad_ale                : std_logic;
  signal pad_read               : std_logic;
  signal pad_write              : std_logic;
  signal pad_cdr                : std_logic;
  signal pad_read_interrupt     : std_logic;
  signal pad_gc_needed          : std_logic;

  -- pseudo pads (signals internal to chip)
  signal conditional_p          : std_logic;
  signal run_nano_p             : std_logic;
  signal mask_interrupt_p       : std_logic;
  
  -- clock
  constant MASTER_PERIOD  : time                  := 1250 ps; -- really a quarter period
  signal master_clk       : std_logic             := '0'; 
  signal tick             : unsigned(11 downto 0) := X"000"; -- count the ticks to aid debugging
  signal master_clk_count : unsigned(2 downto 0)  := "000"; -- just for division

  -- from my-plas
  signal current_ncode  : ncode_word;
  signal current_ucode  : ucode_word;

  signal reg1name : string(1 to 10) := "Reg1      ";
  signal reg2name : string(1 to 10) := "Reg2      ";
  signal reg3name : string(1 to 10) := "Reg3      ";
  --signal memregname : string(1 to 10) := "Memory    ";
  --signal addregname : string(1 to 10) := "Address   ";

  
begin
  My_Bus : bus_master7
    port map (
      clk1 => clk1,
      clk2 => clk2,
      clk1a => clk1a,
      clk2a => clk2a,
      rst => rst,

      my_pad_controls => my_pad_controls,
      pads_memory_bus => pads_memory_bus,
      
      ibus1 => obus1,
      ibus2 => obus2,
      ibus3 => obus3,
      ibus4 => obus4,
      ibus5 => obus5,
      ibus6 => obus6,
      ibus7 => obusme,
      obus => ibus);


  REG_1 : my_register
    port map (
      clk1 => clk1,
      clk2 => clk2,
      rst => rst,
      name => reg1name,
      tick => tick,
      ibus => ibus,
      obus => obus1,
      controls => reg1_controls,
      senses => reg1_senses);

  REG_2 : my_register
    port map (
      clk1 => clk1,
      clk2 => clk2,
      rst => rst,
      name => reg2name,
      tick => tick,
      ibus => ibus,
      obus => obus2,
      controls => reg2_controls,
      senses => reg2_senses);

  REG_3 : my_register
    port map (
      clk1 => clk1,
      clk2 => clk2,
      rst => rst,
      name => reg3name,
      tick => tick,
      ibus => ibus,
      obus => obus3,
      controls => reg3_controls,
      senses => reg3_senses);

  /*
  Reg_Memory : my_register
    port map (
      clk1 => clk1,
      clk2 => clk2,
      rst => rst,
      name => memregname,
      tick => tick,
      ibus => ibus,
      obus => obus4,
      controls => reg_memory_controls,
      senses => reg_memory_senses);
  
  Reg_Address : my_register
    port map (
      clk1 => clk1,
      clk2 => clk2,
      rst => rst,
      name => addregname,
      tick => tick,
      ibus => ibus,
      obus => obus5,
      controls => reg_address_controls,
      senses => reg_address_senses);
  */
  
  Chip_Pads : chip_master
    port map (
      clk1 => clk1,
      clk2 => clk2,
      clk1a => clk1a,
      clk2a => clk2a,
      rst => rst,

      -- consoldated controls
      my_pad_controls       => my_pad_controls,

      -- outputs to FPGA logic
      freeze_p              => freeze_p,
      read_state_p          => read_state_p,
      load_state_p          => load_state_p,
      reset_p               => reset_p,
      interrupt_p           => interrupt_p,
    
      -- outputs from chip to board (or other FPGA logic)
      pad_ph1               => pad_ph1,
      pad_ph2               => pad_ph2,
      pad_ale               => pad_ale,
      pad_read              => pad_read,
      pad_write             => pad_write,
      pad_cdr               => pad_cdr,
      pad_read_interrupt    => pad_read_interrupt,
      pad_gc_needed         => pad_gc_needed,

      -- inputs from board to chip (or other FPGA logic)
      pad_freeze            => pad_freeze,
      pad_read_state        => pad_read_state,
      pad_load_state        => pad_load_state,
      pad_reset             => pad_reset,
      pad_interrupt_request => pad_interrupt_request,

      -- pseudo pads (internal signals)
      conditional_p         => conditional_p,
      run_nano_p            => run_nano_p,
      mask_interrupt_p      => mask_interrupt_p);

  Pla_Interface : pla_master
    port map (
      clk1 => clk1,
      clk1a => clk1a,
      clk2 => clk2,
      clk2a => clk2a,
      rst  => rst,
      ibus => ibus,
      obus => obus6,

      run_nano => run_nano_p,
      pad_freeze => pad_freeze,
      pad_conditional_p => conditional_p,

      ncode_value => current_ncode,
      ucode_value => current_ucode,

      my_pad_controls => my_pad_controls,

      reg1_controls => reg1_controls,
      reg2_controls => reg2_controls,
      reg3_controls => reg3_controls);

      -- address_controls => reg_address_controls,
      -- memory_controls => reg_memory_controls);

  ------------------
  -- clock generator
  ------------------
  -- we did generate (as in the TR) 2 nonoverlapping clock (phases) clk1 and clk2.
  -- however, unlike ASIC development, we really can't just have delays in the
  -- clock signal for FPGAs if we want reproducable results and no race
  -- conditions. SO I'm introducing an "A" phase which is offset from the
  -- primary phase by 90 degrees (i.e. the rising edge will occur half way
  -- through the high side of the associated phase clock and the falling edge
  -- will trail by a similar time after the phase clock falls).

  -- That means the "master clock" has to run 8x instead of 4x per tick.
  
  master_clk <= '0' after MASTER_PERIOD when master_clk = '1' else
                '1' after MASTER_PERIOD;

  Clock_Gen : process (master_clk)
  begin
    master_clk_count <= master_clk_count + 1;
    if (master_clk_count = 0) then
      clk1  <= '1';
      clk1a <= '0';
      clk2  <= '0';
      clk2a <= '0';
      tick  <= tick +1;
    elsif (master_clk_count = 1) then
      clk1  <= '1';
      clk1a <= '1';
      clk2  <= '0';
      clk2a <= '0';
    elsif (master_clk_count = 2) then
      clk1  <= '0';
      clk1a <= '1';
      clk2  <= '0';
      clk2a <= '0';
    elsif (master_clk_count = 3) then
      clk1  <= '0';
      clk1a <= '0';
      clk2  <= '0';
      clk2a <= '0';
    elsif (master_clk_count = 4) then
      clk1  <= '0';
      clk1a <= '0';
      clk2  <= '1';
      clk2a <= '0';
    elsif (master_clk_count = 5) then
      clk1  <= '0';
      clk1a <= '0';
      clk2  <= '1';
      clk2a <= '1';
    elsif (master_clk_count = 6) then
      clk1  <= '0';
      clk1a <= '0';
      clk2  <= '0';
      clk2a <= '1';
    elsif (master_clk_count = 7) then
      clk1  <= '0';
      clk1a <= '0';
      clk2  <= '0';
      clk2a <= '0';
    end if;
  end process Clock_Gen;
  
  ------------------
  -- reset generator
  ------------------
  Reset_Gen : process
  begin
    assert false report "Starting reset" severity note;
    for i in 1 to 5 loop
      if (i < 4) then
        rst <= '1';
      else
        rst <= '0';
      end if;
      wait until falling_edge(clk2);
    end loop;
    -- allow pad to reset!
    wait on reset_p;
  end process Reset_Gen;

  ------------------
  -- Test generator
  ------------------
  --
  -- Basic clock cycle used here is:
  -- rising(clk1): perform register and bus operations, run microcontrol cycle
  --                 (unless supressed with freeze or run-nano)
  -- falling(clk1): clear register controls
  -- rising(clk2): set bus and register sense lines, also run nanocontrol cycle
  --                 which will...
  -- falling(clk2): set up controls for next clk1 cycle
  --
  -- note we don't have an actual microcontrol or nanocontrol but will simulate
  -- it here.
  
  Pla_Test : process
    -- allow us to peek inside registers for self-verification
    -- SADLY not supported by GHDL! (Vivado might though)
    -- "translate_name: cannot handle IIR_KIND_EXTERNAL_SIGNAL_NAME"
    --alias reg1_internal is << signal REG_1.register_local_state : s79_word >>;
    --alias reg2_internal is << signal REG_2.register_local_state : s79_word >>;
    --alias reg3_internal is << signal REG_3.register_local_state : s79_word >>;
  begin
    wait until ((rst = '1') and falling_edge(clk1));
    obusme <= output_bus_init;
    
    wait until falling_edge(rst); -- end of clk2 tick 3

    assert false report "Reset Ended, Starting Test!" severity note;
    -- in theory, our microcode should go to boot_load_address and start
    -- executing. Which means we really don't need to do anything here, just
    -- set up the microcode and nanocode correctly, then when we detect the
    -- final address, we can print out a message!

    -- the following self-verifies the program we loaded (so we have to change
    -- this if we change the program!). Note that we will use 2008 VHDL
    -- non-synthesizable extensions to get information inside of other
    -- entities/components. 

    -- at end of rst, we should be at falling_edge(clk2) during tick 3.
    -- ucode_pc updates on the rising_edge of clk1a
    
    -- tick 3: ucode_pc should be 1 (we execute it on the NEXT tick) Note that
    --         as clk1 rises on tick 4, ucode_pc will still be 1 and updates on
    --         clk1a rising.
    --
    --         Basic schedule of events:
    --
    --         clk1 rising:
    --              tick updated, ucode from
    --              ucode_pc is decoded (ucode_from, ucode_nano, ucode_next and ucode_to)
    --              register reads processed
    --              bus controls that effect registers processed (during read)
    --
    --         clk1a rising:
    --              ucode_pc updated (from ucode_next) so is ready for the next
    --              clk1 rising
    --              ncode_pc is updated from ucode_nano
    --
    --         clk1 falling:
    --              register internal state updated based on control lines
    --                  (these are set up by the nanocode during clk2 of the prior
    --                  tick!)
    --              clean up register/bus/pad controls for next tick
    --
    --         clk1a falling:
    --              pads_memory_bus cleared
    --
    --         clk2 rising:
    --              nanocode acted on, setting various bus requests
    --              ale cleared if requested
    --              other pads cleared if requested
    --              registers stop trying to output if they did on clk1 rising
    --              bus senses updated
    --              register senses updated
    --
    --         clk2a rising:
    --              ncode_pc cleared or set to the linked ncode (for
    --              multi-instruction ncodes)
    --              ale set if requested, other pads as well (e.g. pad_read, pad_write)
    --
    --         clk2 falling:
    --              pin inputs read
    --              conditional_p set if requested
    --
    --         clk2a falling:
    --              run_nano_p updated

    wait until rising_edge(clk1);
    -- tick 4: ucode_pc should be 2 (starting at clk1a rising), during clk2 we
    --                  will run nanocode A to execute 1: load reg1 <- 17
    --                  (constant)

    wait until rising_edge(clk1);
    -- tick 5: ucode_pc should be 3, nanocode for 2 (nc A): load reg2 <- 31 (constant)
    --                  on clk1 falling we should see reg1's internal state change to
    --                  17. (we can sample on clk1a falling)
    wait until falling_edge(clk1a);
    -- can't do this because GHDL doesn't support external names
    --assert reg1_internal = ( mark_bit => '0',
    --                         not_pointer_bit => '1',
    --                         type_rest => (others => '0'),
    --                         displacement => (others => '0'),
    --                         frame => x"017") report "reg1 state incorrect, tick 5" severity error;

    wait until rising_edge(clk1);
    -- tick 6: ucode_pc should be 4, executing 3 (nc 6): rplaca reg1 reg2, 17 is
    --                                            address, 31 should be written.
    --                                            17 output and ALE should get set.
    --                  on clk1 falling we should see reg2's internal state
    --                  change to 31.
    --                  on clk2 rising we should see various set operations for
    --                  pads (like set_ale) and on clk2a rising we should see
    --                  the memory bus turn on for output (currently 0).
    --                  pad_ale should also turn on. The ucode_pc should update
    --                  to 7 for the next nano instruction in the write cycle.
    --                  on clk2a falling we should see run_nano_p turn on to
    --                  supress updating the (decoded) ucode
    wait until falling_edge(clk1a);
    --assert reg2_internal = ( mark_bit => '0',
    --                         not_pointer_bit => '1',
    --                         type_rest => (others => '0'),
    --                         displacement => (others => '0'),
    --                         frame => x"031") report "reg2 state incorect, tick 6" severity error;

    wait until rising_edge(clk1);
    -- tick 7: ucode_pc should still be 4 (running nanocode at 7), actual write
    --                                                             to CAR
    --                  on the rising_edge of clk1a the memory bus should
    --                  present the address (17) for the write (from reg1), and
    --                  we can sample it on the falling edge of clk1. On the
    --                  falling edge of clk1a the memory bus should tristate.
    --                  (technically it doesn't need to since it will be turned
    --                  on to output very soon but that's how this
    --                  code works).
    --                  on the rising edge of clk2a ws should see the ncode_pc
    --                  go to 0 (as this was the final instruction in the
    --                  series, pad_write go high, and the memory bus turn on
    --                  again (still has the 17 on it, but it isn't valid yet
    --                  so it shouldn't matter).
    
    wait until rising_edge(clk1);
    -- tick 8: ucode_pc should be 5, executing 4: load reg2 <- 42

    wait until rising_edge(clk1);
    -- tick 9: ucode_pc should be 6, executing 5: ALE 17
    wait until falling_edge(clk1a);
    --assert reg2_internal = ( mark_bit => '0',
    --                         not_pointer_bit => '1',
    --                         type_rest => (others => '0'),
    --                         displacement => (others => '0'),
    --                         frame => x"042") report "reg2 state incorrect, tick 9" severity error;
    
    wait until rising_edge(clk1);    
    -- tick A: ucode_pc should still be 6, executing 5, (running nanocode at 9),
    --                               writing and marking CDR as 42
    
    wait until rising_edge(clk1);
    -- tick B: ucode_pc should be 7, executing 6 (reading car of reg1 to reg3)
    --                               ALE 17

    wait until rising_edge(clk1);  
    -- tick C: ucode_pc should still be 7, executing 6 (running nanocode at 3)
    --                               reading CAR as 31 into reg3, well really
    --                               x7ff0f0f0 because our memory_sim (below)
    --                               doesn't actually store anything

    wait until rising_edge(clk1);
    -- tick D: ucode_pc should be 0, executing 7 (reading CDR of reg1 to reg3)
    --                               ALE 17
    
    wait until rising_edge(clk1);    
    -- tick E: ucode_pc should still be 0, executing 7 (running nanocode at 5)
    --                               reading CDR as 42 into reg3 (really
    --                               x7ff0f0f0 again) [check log for memory interaction]
    
    wait until rising_edge(clk1);
    -- tick F: all wrapped up

    -- deactivate this process (need to go study the signal diagram!)
    wait on rst;
  end process Pla_Test;
  
  Memory_Sim : process(clk1, clk1a, clk2)
    variable expect_address : std_logic := '0';
    variable address : unsigned(23 downto 0);
    
  begin
    -- memory is external to the chip, but we simulate from the pad
    -- activity here... this is essentially the same as (monitor-pads) in
    -- s79-simulator/external-support.lisp, though we don't actually store
    -- anything here (probably should, but we presumably will be using the DDR
    -- controller rather than block memory anyway for this)
    if rising_edge(clk1) then -- *put-memory-content-onto-pads*
      if (pad_freeze = '0') and (pad_read = '1') then
        -- fake reading the memory
        assert false report "**Memory_Sim** tick: " & to_hstring(tick) &
          " FAKEING reading memory at address " & to_hstring(address) &
          " cdr_p " & to_string(pad_cdr) &
          " RETURNING x7ff0f0f0"
          severity note;
        pads_memory_bus <= ( mark_bit => '0',
                             not_pointer_bit => '1',
                             type_rest => (others => '1'),
                             displacement => x"f0f",
                             frame => x"0f0");
      elsif (pad_read_interrupt = '1') then
        assert false report "**Memory_Sim** tick: " & to_hstring(tick) &
          "FAKEING reading interrupt vector (set to constant x000111)"
          severity note;
        pads_memory_bus <= ( mark_bit => '0',
                             not_pointer_bit => '0',
                             type_rest => (others => '0'),
                             displacement => x"000",
                             frame => x"111");
      else
        pads_memory_bus <= s79_word_ignore;
      end if;
    elsif falling_edge(clk1) then -- *get-address-from-pads*, *get-memory-content-from-pads*
      if (expect_address = '1') then
        address := pads_memory_bus.displacement & pads_memory_bus.frame;
        assert false report "**Memory_Sim** tick: " & to_hstring(tick) &
          " capturing address " & to_hstring(address)
          severity note;
        expect_address := '0';
      elsif (pad_freeze = '0') and (pad_write = '1') then
        -- fake writing the memory
        assert false report "**Memory_Sim** tick: " & to_hstring(tick) &
          " FAKEING writing " & to_string(pads_memory_bus) &
          " to address " & to_hstring(address) &
          " cdr_p " & to_string(pad_cdr)
          severity note;
      end if;
    elsif falling_edge(clk1a) and (pad_freeze = '0') and (pad_read = '1') then
        pads_memory_bus <= s79_word_ignore; -- stop writing to bus at end of clk1a
    elsif falling_edge(clk2) then -- *test-ale-to-expect-address*
      if (pad_ale = '1') then
        assert false report "**Memory_Sim** tick: " & to_hstring(tick) &
          " ale detected" severity note;
        expect_address := '1';
      end if;
    end if;
  end process Memory_Sim;

end architecture behav_plas_1;
