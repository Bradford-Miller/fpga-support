-- ------------------------------------------------
--                                               --
-- Temporary registers to develop register code  --
--                                               --
-- Time-stamp: <2022-08-12 13:34:40 Bradford W. Miller(on Boromir)>      --
--                                               --
--   This is the three register version of the   --
--                   Testbench!                  --
--                                               --
-- ------------------------------------------------

use std.env.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- library work;
use work.regpkg.all; -- temporary register types

      
-- put together a testbed to exercise the registers
-- (later, move this to it's own file TBD)

-- test single register (bascially observe the bus)
entity tb_register_3 is
end entity tb_register_3;
    
-- it should be enough to run this for 24 ticks (240 ns) (last command is tick 21)
architecture behav_reg_3 of tb_register_3 is
  component my_register
    generic (
      Simulation_on : boolean := false; -- not currently using, but 'just in case'
      Word_width    : integer := 32); -- ideally we'd pass this to io_bus or get
                                    -- it from there?
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

  component bus_master4
    generic (
      Simulation_on : boolean := false; -- not currently using, but 'just in case'
      Word_width    : integer := 32); -- ideally we'd pass this to io_bus or get
                                      -- it from there?
    port (
      clk1     : in    std_logic;
      clk2     : in    std_logic;     -- two phase clock
      clk1a    : in    std_logic;
      clk2a    : in    std_logic;
      rst      : in    std_logic;

      obus     : out   input_bus;
      ibus1    : in    output_bus;
      ibus2    : in    output_bus;
      ibus3    : in    output_bus;
      ibus4    : in    output_bus);
  end component bus_master4;

  -- MUT inputs
  signal clk1 : std_logic := '0';
  signal clk2 : std_logic := '0';
  signal clk1a : std_logic := '0';
  signal clk2a : std_logic := '0';
  signal rst  : std_logic := '0';
  signal r1controls : register_controls := register_controls_init;
  signal r1senses : register_senses;
  signal r2controls : register_controls := register_controls_init;
  signal r2senses : register_senses;
  signal r3controls : register_controls := register_controls_init;
  signal r3senses : register_senses;
  signal ibus : input_bus;
  signal obus1 : output_bus; -- from r1
  signal obus2 : output_bus; -- from r2
  signal obus3 : output_bus; -- from r3
  signal obusme : output_bus; -- test input

  constant MASTER_PERIOD : time := 2500 ps;
  signal master_clk : std_logic := '0';
  signal tick : unsigned(11 downto 0) := X"000";
  signal master_clk_count : unsigned(2 downto 0) := "000";
  signal reg1name : string(1 to 10) := "Reg1      ";
  signal reg2name : string(1 to 10) := "Reg2      ";
  signal reg3name : string(1 to 10) := "Reg3      ";
  
begin
  My_Bus : bus_master4
    port map (
      clk1 => clk1,
      clk2 => clk2,
      clk1a => clk1a,
      clk2a => clk2a,
      rst => rst,
      ibus1 => obus1,
      ibus2 => obus2,
      ibus3 => obus3,
      ibus4 => obusme,
      obus => ibus);

  Reg_1 : my_register
    port map (
      clk1 => clk1,
      clk2 => clk2,
      rst => rst,
      name => reg1name,
      tick => tick,
      ibus => ibus,
      obus => obus1,
      controls => r1controls,
      senses => r1senses);
  
  Reg_2 : my_register
    port map (
      clk1 => clk1,
      clk2 => clk2,
      rst => rst,
      name => reg2name,
      tick => tick,
      ibus => ibus,
      obus => obus2,
      controls => r2controls,
      senses => r2senses);

  Reg_3 : my_register
    port map (
      clk1 => clk1,
      clk2 => clk2,
      rst => rst,
      name => reg3name,
      tick => tick,
      ibus => ibus,
      obus => obus3,
      controls => r3controls,
      senses => r3senses);

  ------------------
  -- clock generator
  ------------------
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
    -- deactivate this process
    wait on rst;
  end process Reset_Gen;
  
  ------------------
  -- Test generator
  ------------------
  --
  -- Basic clock cycle used here is:
  -- rising(clk1): perform register and bus operations
  -- falling(clk1): clear register controls
  -- rising(clk2): set bus and register sense lines
  -- falling(clk2): set up controls for next clk1 cycle

  -- starting with a similar test as we had for one register, but now
  -- designating a target to load when we are getting stuff FROM the register
  -- (i.e., still have a set of initial TO loads to get set up, then will copy
  -- various fields to the other registers). We'll then load up one of the
  -- other registers with data and try to copy it around too just to be sure
  -- all the code is agnostic about the register used.

  Register_Test : process
  begin
    wait until ((rst = '1') and falling_edge(clk1)) ;
    obusme <= output_bus_init;
    
    wait until falling_edge(rst); -- end of clk2 tick 3
    -- on fall of clk2, set controls for next clk1
    -- note we sait because on rise of clk2 senses are updated
    
    wait until falling_edge(clk2); -- a tick later tick 4
    -- wait a tick to see if we detect address_eq_zero, etc.
    
    wait until falling_edge(clk2); -- a tick later tick 5
    -- load the tick
    obusme.output_bus_data.frame <= tick;
    obusme.output_bus_data.displacement <= tick;
    obusme.output_bus_data.type_rest <= "010101"; -- 15
    obusme.output_bus_data.not_pointer_bit <= '1';
    obusme.output_bus_data.mark_bit <= '0';
    SetControl(r1controls.rc_to, clk1);
    report "attempting write of " & to_string(obusme.output_bus_data) & " to register 1";
    
    wait until falling_edge(clk2); -- tick 6
    -- modify the displacement
    obusme.output_bus_data.frame <= tick;
    obusme.output_bus_data.displacement <= tick;
    obusme.output_bus_data.type_rest <= "010111"; -- 17
    SetControl(r1controls.rc_to_displacement, clk1);
    report "attempting write of " & to_string(obusme.output_bus_data) & " to register 1 displacement";
    
    wait until falling_edge(clk2); -- tick 7
    -- modify the frame
    obusme.output_bus_data.frame <= tick;
    obusme.output_bus_data.displacement <= tick; 
    obusme.output_bus_data.type_rest <= "010110"; -- 16
    SetControl(r1controls.rc_to_frame, clk1);
    report "attempting write of " & to_string(obusme.output_bus_data) & " to register 1 frame";
    
    wait until falling_edge(clk2); -- tick 8
    -- modify the type
    obusme.output_bus_data.frame <= tick;
    obusme.output_bus_data.displacement <= tick; 
    obusme.output_bus_data.type_rest <= "011011"; -- 1B
    SetControl(r1controls.rc_to_type, clk1);
    report "attempting write of " & to_string(obusme.output_bus_data) & " to register 1 type";
    
    wait until falling_edge(clk2); -- tick 9
    -- modify the address
    obusme.output_bus_data.displacement <= tick;
    obusme.output_bus_data.frame <= tick;
    obusme.output_bus_data.type_rest <= "101011"; -- 2B
    SetControl(r1controls.rc_to_address, clk1);
    report "attempting write of " & to_string(obusme.output_bus_data) & " to register 1 address";
    
    wait until falling_edge(clk2); -- tick A
    -- now test sending register contents to another register
    -- content should be at this point frame: 09, displacement: 09,
    -- type_rest: 1B, mark: 0 not_pointer: 0.
    obusme <= output_bus_init;
    SetControls(r1controls.rc_from, r2controls.rc_to, clk1);
    
    report "attempting read of register 1 falling(clk1), got  " & to_string(ibus.shared_bus_data); 
    wait until falling_edge(clk1a);
    report "attempting read of register 1 falling(clk1a), got  " & to_string(ibus.shared_bus_data);
    wait until rising_edge(clk2);
    report "attempting read of register 1 rising(clk2), got  " & to_string(ibus.shared_bus_data);
    wait until rising_edge(clk2a);
    report "attempting read of register 1 rising(clk2a), got  " & to_string(ibus.shared_bus_data);
    
    wait until falling_edge(clk2); -- tick B
    report "attempting read of register 1 falling(clk2), got  " & to_string(ibus.shared_bus_data);
    SetControls(r1controls.rc_from_decremented, r3controls.rc_to_address, clk1);
    
    wait until falling_edge(clk2); -- tick C
    SetControls(r1controls.rc_from_incremented, r2controls.rc_to_address, clk1);
    
    wait until falling_edge(clk2); -- tick D
    SetControls(r1controls.rc_from_decremented_frame, r3controls.rc_to_frame, clk1);
    
    wait until falling_edge(clk2); -- tick E
    SetControls(r1controls.rc_from_decremented_displacement, r2controls.rc_to_displacement, clk1);
    
    wait until falling_edge(clk2); -- tick F NB: We are putting into the
    -- frame field!
    SetControls(r1controls.rc_from_type, r3controls.rc_to_frame, clk1);
    
    -- bus controls only work when we're also loading from the register
    wait until falling_edge(clk2); -- tick 10
    SetControls3(obusme.bus_controls.bc_set_mark, r1controls.rc_from, r2controls.rc_to, clk1); 
    
    wait until falling_edge(clk2); -- tick 11
    SetControls3(obusme.bus_controls.bc_set_unmark, r1controls.rc_from, r2controls.rc_to, clk1);
    
    wait until falling_edge(clk2); -- tick 12
    SetControls3(obusme.bus_controls.bc_set_pointer, r1controls.rc_from, r3controls.rc_to, clk1);
    
    wait until falling_edge(clk2); -- tick 13
    SetControls3(obusme.bus_controls.bc_set_type, r1controls.rc_from, r3controls.rc_to, clk1);
    
    wait until falling_edge(clk2); -- tick 14
    -- ok now make sure we can copy from a different register!
    obusme.output_bus_data.frame <= tick;
    obusme.output_bus_data.displacement <= tick;
    obusme.output_bus_data.type_rest <= "111101"; -- 3C
    obusme.output_bus_data.not_pointer_bit <= '1';
    obusme.output_bus_data.mark_bit <= '0';
    SetControl(r2controls.rc_to, clk1);
    
    wait until falling_edge(clk2); -- tick 15
    obusme <= output_bus_init;
    
    SetControls3(obusme.bus_controls.bc_set_mark, r2controls.rc_from, r1controls.rc_to, clk1);
    
    -- deactivate this process (need to go study the signal diagram!)
    wait until falling_edge(clk2); -- tick 16
    finish; -- new in std08
    wait on rst;
  end process Register_Test;
end architecture behav_reg_3;
