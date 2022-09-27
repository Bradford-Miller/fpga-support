-- ------------------------------------------------------------------------
--                                                                       --
-- Temporary registers to develop pad code  and check timing             --
--                                                                       --
-- Time-stamp: <2022-09-12 16:39:54 Bradford W. Miller(on Boromir)>      --
--                                                                       --
--       This is the pad timing test of the Testbench!                   --
--                                                                       --
-- ------------------------------------------------------------------------

use std.env.all;
    
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- library work;
-- need regpkg for bus defns
use work.regpkg.all; -- temporary register types
use work.padpkg.all;

      
-- put together a testbed to exercise the pads
-- we will have a couple accumulator registers, the memory and address
-- register, and try a read (car/cdr of same address) and write (car/cdr of two
-- different addresses) and match the wave to what was presented in the AIM.
-- We'll also test a smattering of the other signals, but as most are really
-- the same code we will not be exhaustive.

-- test pads
entity tb_pads_1 is
end entity tb_pads_1;
    
-- it should be enough to run this for 18 ticks 180 ns (last command is tick
-- 16) 
architecture behav_pads_1 of tb_pads_1 is
  component bus_master6
    generic (
      Simulation_on : boolean := false); -- not currently using, but 'just in case'
    port (
      clk1     : in    std_logic;
      clk2     : in    std_logic;     -- two phase clock
      clk1a    : in    std_logic;
      clk2a    : in    std_logic;
      rst      : in    std_logic;

      my_pad_controls       : in     pad_controls;
      pads_memory_bus       : inout  s79_word; 

      obus     : out   input_bus;
      ibus1    : in    output_bus;
      ibus2    : in    output_bus;
      ibus3    : in    output_bus;
      ibus4    : in    output_bus;
      ibus5    : in    output_bus;
      ibus6    : in    output_bus);
  end component bus_master6;

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

  -- MUT inputs
  signal clk1  : std_logic := '0';
  signal clk2  : std_logic := '0';
  signal clk1a : std_logic := '0';
  signal clk2a : std_logic := '0';
  signal rst   : std_logic := '0';
  signal acc1_controls : register_controls := register_controls_init;
  signal acc1_senses   : register_senses;
  signal acc2_controls : register_controls := register_controls_init;
  signal acc2_senses   : register_senses;
  signal reg_memory_controls  : register_controls := register_controls_init;
  signal reg_memory_senses    : register_senses;
  signal reg_address_controls : register_controls := register_controls_init;
  signal reg_address_senses   : register_senses;

  signal ibus : input_bus;
  signal obus1 : output_bus; -- from acc1
  signal obus2 : output_bus; -- from acc2
  signal obus3 : output_bus; -- memory
  signal obus4 : output_bus; -- address
  signal obus5 : output_bus := output_bus_init; -- pads (no longer used)
  signal obusme : output_bus; -- test input

  -- pad control inputs
  signal my_pad_controls : pad_controls := pad_controls_init;

  -- pad control outputs
  signal freeze_p     : std_logic;
  signal read_state_p : std_logic;
  signal load_state_p : std_logic;
  signal reset_p      : std_logic;
  signal interrupt_p  : std_logic;

  -- pad inputs
  signal pad_freeze            : std_logic := '0';
  signal pad_read_state        : std_logic := '0';
  signal pad_load_state        : std_logic := '0';
  signal pad_reset             : std_logic := '0';
  signal pad_interrupt_request : std_logic := '0';
  signal pads_memory_bus       : s79_word := s79_word_init;

  -- pad outputs
  signal pad_ph1            : std_logic;
  signal pad_ph2            : std_logic;
  signal pad_ale            : std_logic;
  signal pad_read           : std_logic;
  signal pad_write          : std_logic;
  signal pad_cdr            : std_logic;
  signal pad_read_interrupt : std_logic;
  signal pad_gc_needed      : std_logic;

  -- pseudo pads (internal signals)
  signal conditional_p        : std_logic;
  signal run_nano_p           : std_logic;
  signal mask_interrupt_p     : std_logic;


  -- clock
  constant MASTER_PERIOD  : time                  := 1250 ps; -- really a quarter period
  signal master_clk       : std_logic             := '0'; 
  signal tick             : unsigned(11 downto 0) := X"000"; -- count the ticks to aid debugging
  signal master_clk_count : unsigned(2 downto 0)  := "000"; -- just for division

  signal acc1name : string(1 to 10) := "Acc1      ";
  signal acc2name : string(1 to 10) := "Acc2      ";
  signal memregname : string(1 to 10) := "Memory    ";
  signal addregname : string(1 to 10) := "Address   ";
  
begin
  My_Bus : bus_master6
    port map (
      clk1 => clk1,
      clk2 => clk2,
      clk1a => clk1a,
      clk2a => clk2a,
      rst => rst,

      my_pad_controls       => my_pad_controls,
      pads_memory_bus       => pads_memory_bus,

      ibus1 => obus1,
      ibus2 => obus2,
      ibus3 => obus3,
      ibus4 => obus4,
      ibus5 => obus5, -- no longer used in this test!
      ibus6 => obusme,
      obus => ibus);

  ACC_1 : my_register
    port map (
      clk1 => clk1,
      clk2 => clk2,
      rst => rst,
      name => acc1name,
      tick => tick,
      ibus => ibus,
      obus => obus1,
      controls => acc1_controls,
      senses => acc1_senses);

  ACC_2 : my_register
    port map (
      clk1 => clk1,
      clk2 => clk2,
      rst => rst,
      name => acc2name,
      tick => tick,
      ibus => ibus,
      obus => obus2,
      controls => acc2_controls,
      senses => acc2_senses);

  Reg_Memory : my_register
    port map (
      clk1 => clk1,
      clk2 => clk2,
      rst => rst,
      name => memregname,
      tick => tick,
      ibus => ibus,
      obus => obus3,
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
      obus => obus4,
      controls => reg_address_controls,
      senses => reg_address_senses);

  Chip_Pads : chip_master
    port map (
      clk1 => clk1,
      clk2 => clk2,
      clk1a => clk1a,
      clk2a => clk2a,
      rst => rst,

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

      -- pseudo pads (signals)
      conditional_p         => conditional_p,
      run_nano_p            => run_nano_p,
      mask_interrupt_p      => mask_interrupt_p);

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
  -- note we don't have an actual microcontrol or nanocontrol but will try to
  -- have the cycles match

  -- this tries to work out the "lead time" needed for placing signals on the
  -- bus or controls to get the "right" (per the AIM) timing for memory
  -- read/write on the "external" bus (ipads_memory_bus).
  
  Pad_Test : process
  begin
    wait until ((rst = '1') and falling_edge(clk1));
    obusme <= output_bus_init;
        
    wait until falling_edge(rst); -- end of clk2 tick 3
    
    -- on fall of clk2, set controls for next clk1
    -- note we wait because on rise of clk2 senses are updated
    
    wait until falling_edge(clk2); -- a tick later tick 4
    -- wait a tick to see if we detect address_eq_zero, etc.
    -- load the tick into acc1 and acc2
    obusme.output_bus_data.frame <= tick;
    obusme.output_bus_data.displacement <= tick;
    obusme.output_bus_data.type_rest <= "000000";
    obusme.output_bus_data.not_pointer_bit <= '0';
    obusme.output_bus_data.mark_bit <= '0';
    SetControl(acc1_controls.rc_to, clk1);
    
    wait until falling_edge(clk2); -- a tick later tick 5
    -- load the tick
    obusme.output_bus_data.frame <= tick;        
    obusme.output_bus_data.displacement <= tick;
    SetControl(acc2_controls.rc_to, clk1);

    -- wait until falling_edge(clk1); -- tick 6 -- setcontrol already waited to
    -- this point!
    my_pad_controls.set_run_nano <= '1';
    my_pad_controls.set_ale <= '1';
    my_pad_controls.set_memory_pads <= '1';
    -- simulate a WRITE cycle from address in acc1 of content in acc2
    SetControls(reg_address_controls.rc_to, acc1_controls.rc_from, clk1);

    -- wait until falling_edge(clk1); -- tick 7
    -- now we can clear the ALE
    my_pad_controls.set_ale <= '0';
    my_pad_controls.set_write <= '1'; -- leave write high for two cycles
    SetControls3(reg_memory_controls.rc_to, obusme.bus_controls.bc_set_unmark, acc2_controls.rc_from, clk1);
    
    -- wait until falling_edge(clk1); -- tick 8
    my_pad_controls.set_run_nano <= '0'; -- now clear it (drops at end of ph2?)
    my_pad_controls.clear_run_nano <= '1';
    my_pad_controls.set_cdr <= '1'; -- sets with start of ph2
    -- now the same for the cdr
    SetControls3(reg_memory_controls.rc_to, obusme.bus_controls.bc_set_unmark, acc1_controls.rc_from, clk1);

    -- wait until falling_edge(clk2); -- tick 8

    -- wait until falling_edge(clk1); -- tick 9
    my_pad_controls.set_write <= '0';
    my_pad_controls.set_memory_pads <= '0';
    my_pad_controls.set_cdr <= '0';
    my_pad_controls.clear_run_nano <= '0';
    
    wait until falling_edge(clk2); -- tick 9
    -- clear the accumulators
    obusme.output_bus_data <= s79_word_init;
    SetControl(acc2_controls.rc_to, clk1);

    -- wait until falling_edge(clk1); -- tick A
    my_pad_controls.set_run_nano <= '1';
    my_pad_controls.set_ale <= '1';
    my_pad_controls.set_memory_pads <= '1';
    
    wait until falling_edge(clk2); -- tick A
    -- now read the cdr, address should be 0x4
    SetControls(reg_address_controls.rc_to, acc1_controls.rc_from, clk1); --

    -- wait until falling_edge(clk1); -- tick B
    my_pad_controls.set_ale <= '0';
    my_pad_controls.set_memory_pads <= '0';
    my_pad_controls.set_cdr <= '1';
    my_pad_controls.set_run_nano <= '0';
    my_pad_controls.set_read <= '1';
    my_pad_controls.get_memory_pads <= '1';
    
    wait until falling_edge(clk2); -- tick B
    SetControls(reg_memory_controls.rc_to, acc2_controls.rc_to, clk1);
    -- ***** all seems right.

    -- wait until falling_edge(clk1); -- tick C
    my_pad_controls.set_cdr <= '0';
    my_pad_controls.set_read <= '0';
    my_pad_controls.get_memory_pads <= '0';
    
    wait until falling_edge(clk1); -- tick D
    my_pad_controls.set_gc_needed <= '1';
    my_pad_controls.set_read_interrupt <= '1';
    my_pad_controls.get_memory_pads <= '1';
    
    wait until falling_edge(clk1); -- tick E
    my_pad_controls.set_gc_needed <= '0';
    my_pad_controls.set_read_interrupt <= '0';
    my_pad_controls.get_memory_pads <= '0';
    my_pad_controls.clear_gc_needed <= '1';
    
    wait until falling_edge(clk1); -- tick F
    my_pad_controls.clear_gc_needed <= '0';
    
    wait until falling_edge(clk1); -- tick 10
    
    -- deactivate this process (need to go study the signal diagram!)
    finish;
    wait on rst;
  end process Pad_Test;
  
  Memory_Sim : process(clk1, clk1a, clk2)
    variable expect_address : std_logic := '0';
    variable address : unsigned(23 downto 0);
    
  begin
    -- memory is external to the chip, but we simulate from the pad
    -- activity here... this is essentially the same as (monitor-pads) in
    -- s79-simulator/external-support.lisp
    if rising_edge(clk1) then -- *put-memory-content-onto-pads*
      if (pad_freeze = '0') and (pad_read = '1') then
        -- fake reading the memory
        report "tick: " & to_string(tick) &
          " reading memory at address " & to_string(address) &
          " cdr_p " & to_string(pad_cdr) &
          " set to x7ff0f0f0";
        pads_memory_bus <= ( mark_bit => '0',
                             not_pointer_bit => '1',
                             type_rest => (others => '1'),
                             displacement => x"f0f",
                             frame => x"0f0");
      elsif (pad_read_interrupt = '1') then
        report "tick: " & to_string(tick) &
          " reading interrupt vector (set to x000111)";
        pads_memory_bus <= ( mark_bit => '0',
                             not_pointer_bit => '0',
                             type_rest => (others => '0'),
                             displacement => x"000",
                             frame => x"111");
      else
        pads_memory_bus <= s79_word_ignore;
      end if;
    end if;
    
    if falling_edge(clk1) then -- *get-address-from-pads*, *get-memory-content-from-pads*
      if (expect_address = '1') then
        address := pads_memory_bus.displacement & pads_memory_bus.frame;
        report "tick: " & to_string(tick) &
          " reading address " & to_string(address);
        expect_address := '0';
      elsif (pad_freeze = '0') and (pad_write = '1') then
        -- fake writing the memory
        report "tick: " & to_string(tick) &
          " writing " & to_string(pads_memory_bus) &
          " to address " & to_string(address) &
          " cdr_p " & to_string(pad_cdr);
      end if;
    elsif falling_edge(clk1a) and (pad_freeze = '0') and (pad_read = '1) then
      pads_memory_bus <= s79_word_ignore; -- stop writing to bus at end of clk1a
    elsif falling_edge(clk2) then -- *test-ale-to-expect-address*
      if (pad_ale = '1') then
        report "tick: " & to_string(tick) &
          " ale detected";
        expect_address := '1';
      end if;
    end if;
  end process Memory_Sim;

end architecture behav_pads_1;
