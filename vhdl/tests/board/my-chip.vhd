-- ------------------------------------------------------------------------
--                                                                       --
--     Chip-level description (pulls together architecural bits into     --
--              something better resembling the Scheme-79 chip)          --
--                                                                       --
--   Time-stamp: <2022-11-16 12:40:48 Bradford W. Miller(on Boromir)>    --
--                                                                       --
-- ------------------------------------------------------------------------

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

-- define the interfaces to the chip and then glue them to the underlying
-- architectural elements

entity s79_chip is
  generic (
    Simulation_on :boolean := false; -- not currently using
    Word_width    :integer := 32);

  -- trying to keep the port names short as we will have to work with them in Vivado
  port (
    s79_clkph1  : in     std_logic;
    s79_clkph2  : in     std_logic;
    -- freeze
    s79_frz     : in     std_logic; -- not used yet
    -- read state
    s79_rds     : in     std_logic; -- not used yet
    -- load state
    s79_lds     : in     std_logic; -- not used yet
    -- interrupt request
    s79_irq     : in     std_logic;

    -- address/data bus; memory interface
    s79_adb     : inout  s79_word; -- defined in regpkg
    s79_cdr     : out    std_logic := '0'; 
    s79_ale     : out    std_logic := '0';
    -- read
    s79_rd      : out    std_logic := '0';
    -- write
    s79_wr      : out    std_logic := '0';
    
    -- read interrupt
    s79_rdint   : out    std_logic := '0';
    -- gc needed
    s79_gcn     : out    std_logic := '0';

    -- bonus signals [I added]
    -- reset
    s79_rst     : in     std_logic := '0';
    -- 90degree delayed clocks (There may be a way to ask Vivado to generate
    -- these from clkph1 and clkph2, but for now I'm just planning to use
    -- them as additional inputs to the chip and let the board-level design
    -- generate all 4 clocks)
    s79_clkph1a : in     std_logic;
    s79_clkph2a : in     std_logic);
    
end entity s79_chip;

-- for our present purposes this is NOT a complete s79 chip (of course) just
-- the small subset we wrote by hand in VHDL.

architecture behav_s79_chip of s79_chip is
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

  -- glue signals and variables
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

  -- for our I/O bus
  signal ibus  : input_bus;
  signal obus1 : output_bus; -- from reg1
  signal obus2 : output_bus; -- from reg2
  signal obus3 : output_bus; -- reg3
  signal obus4 : output_bus := output_bus_init; -- memory
  signal obus5 : output_bus := output_bus_init; -- address
  signal obus6 : output_bus; -- pla
  signal obus7 : output_bus := output_bus_init; -- not currently used (was for
                                                -- testbench
  
  -- pad control inputs
  signal my_pad_controls        : pad_controls := pad_controls_init;

  -- pad control outputs
  signal freeze_p               : std_logic;
  signal read_state_p           : std_logic;
  signal load_state_p           : std_logic;
  signal reset_p                : std_logic;
  signal interrupt_p            : std_logic;

  -- pseudo pads (signals internal to chip)
  signal conditional_p          : std_logic;
  signal run_nano_p             : std_logic;
  signal mask_interrupt_p       : std_logic;
  
  -- clock
  signal tick             : unsigned(11 downto 0) := X"000"; -- count the ticks to aid debugging

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
      clk1 => s79_clkph1,
      clk2 => s79_clkph2,
      clk1a => s79_clkph1a,
      clk2a => s79_clkph2a,
      rst => s79_rst,

      my_pad_controls => my_pad_controls,
      pads_memory_bus => s79_adb,
      
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
      clk1 => s79_clkph1,
      clk2 => s79_clkph2,
      rst => s79_rst,
      name => reg1name,
      tick => tick,
      ibus => ibus,
      obus => obus1,
      controls => reg1_controls,
      senses => reg1_senses);

  REG_2 : my_register
    port map (
      clk1 => s79_clkph1,
      clk2 => s79_clkph2,
      rst => s79_rst,
      name => reg2name,
      tick => tick,
      ibus => ibus,
      obus => obus2,
      controls => reg2_controls,
      senses => reg2_senses);

  REG_3 : my_register
    port map (
      clk1 => s79_clkph1,
      clk2 => s79_clkph2,
      rst => s79_rst,
      name => reg3name,
      tick => tick,
      ibus => ibus,
      obus => obus3,
      controls => reg3_controls,
      senses => reg3_senses);

  /*
  Reg_Memory : my_register
    port map (
      clk1 => s79_clkph1,
      clk2 => s79_clkph2,
      rst => s79_rst,
      name => memregname,
      tick => tick,
      ibus => ibus,
      obus => obus4,
      controls => reg_memory_controls,
      senses => reg_memory_senses);
  
  Reg_Address : my_register
    port map (
      clk1 => s79_clkph1,
      clk2 => s79_clkph2,
      rst => s79_rst,
      name => addregname,
      tick => tick,
      ibus => ibus,
      obus => obus5,
      controls => reg_address_controls,
      senses => reg_address_senses);
  */
  
  Chip_Pads : chip_master
    port map (
      clk1 => s79_clkph1,
      clk2 => s79_clkph2,
      clk1a => s79_clkph1a,
      clk2a => s79_clkph2a,
      rst => s79_rst,

      -- consoldated controls
      my_pad_controls       => my_pad_controls,

      -- outputs to FPGA logic
      freeze_p              => freeze_p,
      read_state_p          => read_state_p,
      load_state_p          => load_state_p,
      reset_p               => reset_p,
      interrupt_p           => interrupt_p,
    
      -- outputs from chip to board (or other FPGA logic)
      pad_ale               => s79_ale,
      pad_read              => s79_rd,
      pad_write             => s79_wr,
      pad_cdr               => s79_cdr,
      pad_read_interrupt    => s79_rdint,
      pad_gc_needed         => s79_gcn,

      -- inputs from board to chip (or other FPGA logic)
      pad_ph1               => s79_clkph1,
      pad_ph2               => s79_clkph2,

      pad_freeze            => s79_frz,
      pad_read_state        => s79_rds,
      pad_load_state        => s79_lds,
      pad_reset             => s79_rst,
      pad_interrupt_request => s79_irq,

      -- pseudo pads (internal signals)
      conditional_p         => conditional_p,
      run_nano_p            => run_nano_p,
      mask_interrupt_p      => mask_interrupt_p);

  Pla_Interface : pla_master
    port map (
      clk1 => s79_clkph1,
      clk1a => s79_clkph1a,
      clk2 => s79_clkph2,
      clk2a => s79_clkph2a,
      rst  => s79_rst,
      ibus => ibus,
      obus => obus6,

      run_nano => run_nano_p,
      pad_freeze => s79_frz,
      pad_conditional_p => conditional_p,

      ncode_value => current_ncode,
      ucode_value => current_ucode,

      my_pad_controls => my_pad_controls,

      reg1_controls => reg1_controls,
      reg2_controls => reg2_controls,
      reg3_controls => reg3_controls);

      -- address_controls => reg_address_controls,
      -- memory_controls => reg_memory_controls);

  Clock_Tick : process (s79_clkph1)
  begin
    if rising_edge(s79_clkph1) then
      tick <= tick + 1; -- just needed for debugging
    end if;
  end process;
    
end architecture behav_s79_chip;
