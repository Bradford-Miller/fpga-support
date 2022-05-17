-- ------------------------------------------------
--                                               --
-- Temporary registers to develop register code  --
--                                               --
-- Time-stamp: <2022-05-11 11:56:14 gorbag>      --
--                                               --
--    This is the one register version of the    --
--                   Testbench!                  --
--                                               --
-- ------------------------------------------------

    
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- library work;
use work.regpkg.all; -- temporary register types

      
-- put together a testbed to exercise the registers
-- (later, move this to it's own file TBD)

-- test single register (bascially observe the bus)
entity tb_register_1 is
end entity tb_register_1;
    
-- it should be enough to run this for 22 ticks 220 ns (last command is tick 20)
architecture behav_reg_1 of tb_register_1 is
  component my_register
    generic (
      Simulation_on : boolean := false; -- not currently using, but 'just in case'
      Word_width    : integer := 32); -- ideally we'd pass this to io_bus or get
                                    -- it from there?
    port (
      clk1     : in    std_logic;
      clk2     : in    std_logic;     -- two phase clock
      rst      : in    std_logic;
      ibus     : inout io_bus;            -- Currently support single bus
      controls : in    register_controls := register_controls_init;
      senses   : out   register_senses);
  end component my_register;

  component bus_master
    generic (
      Simulation_on : boolean := false; -- not currently using, but 'just in case'
      Word_width    : integer := 32); -- ideally we'd pass this to io_bus or get
                                      -- it from there?
    port (
      clk1     : in    std_logic;
      clk2     : in    std_logic;     -- two phase clock
      rst      : in    std_logic;
      ibus     : inout io_bus);            -- Currently support single bus
  end component bus_master;

  -- MUT inputs
  signal clk1 : std_logic := '0';
  signal clk2 : std_logic := '0';
  signal rst  : std_logic := '0';
  signal controls : register_controls := register_controls_init;
  signal senses : register_senses;
  signal ibus : io_bus := io_bus_dont_drive;

  constant MASTER_PERIOD : time := 2500 ps;
  signal master_clk : std_logic := '0';
  signal tick : unsigned(11 downto 0) := X"000";
  signal master_clk_count : unsigned(1 downto 0) := "00";
  
begin
  My_Bus : bus_master
    port map (
      clk1 => clk1,
      clk2 => clk2,
      rst => rst,
      ibus => ibus);

  Reg_1 : my_register
    port map (
      clk1 => clk1,
      clk2 => clk2,
      rst => rst,
      ibus => ibus,
      controls => controls,
      senses => senses);

  ------------------
  -- clock generator
  ------------------
  master_clk <= '0' after MASTER_PERIOD when master_clk = '1' else
                '1' after MASTER_PERIOD;

  Clock_Gen : process (master_clk)
  begin
    master_clk_count <= master_clk_count + 1;
    if (master_clk_count = 0) then
      clk1 <= '1';
      clk2 <= '0';
      tick <= tick +1;
    elsif (master_clk_count = 1) then
      clk1 <= '0';
      clk2 <= '0';
    elsif (master_clk_count = 2) then
      clk1 <= '0';
      clk2 <= '1';
    elsif (master_clk_count = 3) then
      clk1 <= '0';
      clk2 <= '0';
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
  ----------------------
  -- clear prior command
  ----------------------

  Command_Clear : process
  begin
    loop
      wait until falling_edge(clk1);
      -- this should be the only process running the controls, and they should
      -- be cleared by SetControl
      ibus.bus_data <= s79_word_ignore; -- stop driving (if we are)
    end loop;
  end process Command_Clear;
  
  ------------------
  -- Test generator
  ------------------
  --
  -- Basic clock cycle used here is:
  -- rising(clk1): perform register and bus operations
  -- falling(clk1): clear register controls
  -- rising(clk2): set bus and register sense lines
  -- falling(clk2): set up controls for next clk1 cycle
  
  Register_Test : process
    begin
      if ((rst = '1') and falling_edge(clk1)) then
        ibus.bus_controls <= io_bus_controls_init;
      end if;
        
      wait until falling_edge(rst); -- end of clk2 tick 3

      -- on fall of clk2, set controls for next clk1
      -- note we sait because on rise of clk2 senses are updated

      wait until falling_edge(clk2); -- a tick later tick 4
      -- wait a tick to see if we detect address_eq_zero, etc.
      -- 
      wait until falling_edge(clk2); -- a tick later tick 5
      -- load the tick
      ibus.bus_data.frame <= tick;
      ibus.bus_data.displacement <= tick;
      ibus.bus_data.type_rest <= "010101"; -- 15
      ibus.bus_data.not_pointer_bit <= '1';
      ibus.bus_data.mark_bit <= '0';
      SetControl(controls.rc_to, clk1);
      
      wait until falling_edge(clk2); -- tick 6
      -- modify the displacement
      ibus.bus_data.frame <= tick;
      ibus.bus_data.displacement <= tick;
      ibus.bus_data.type_rest <= "010111"; -- 17
      SetControl(controls.rc_to_displacement, clk1);

      wait until falling_edge(clk2); -- tick 7
      -- modify the frame
      ibus.bus_data.frame <= tick;
      ibus.bus_data.displacement <= tick; 
      ibus.bus_data.type_rest <= "010110"; -- 16
      SetControl(controls.rc_to_frame, clk1);

      wait until falling_edge(clk2); -- tick 8
      -- modify the type
      ibus.bus_data.frame <= tick;
      ibus.bus_data.displacement <= tick; 
      ibus.bus_data.type_rest <= "011011"; -- 1B
      SetControl(controls.rc_to_type, clk1);

      wait until falling_edge(clk2); -- tick 9
      -- modify the address
      ibus.bus_data.displacement <= tick;
      ibus.bus_data.frame <= tick;
      ibus.bus_data.type_rest <= "101011"; -- 2B
      SetControl(controls.rc_to_address, clk1);
      
      wait until falling_edge(clk2); -- tick A
      -- now test sending register contents to the bus
      -- content should be at this point frame: 09, displacement: 09,
      -- type_rest: 1B, mark: 0 not_pointer: 0.
      ibus <= io_bus_dont_drive;

      SetControl(controls.rc_from, clk1);

      wait until falling_edge(clk2); -- tick B
      SetControl(controls.rc_from_decremented, clk1);

      wait until falling_edge(clk2); -- tick C
      SetControl(controls.rc_from_incremented, clk1);

      wait until falling_edge(clk2); -- tick D
      SetControl(controls.rc_from_decremented_frame, clk1);

      wait until falling_edge(clk2); -- tick E
      SetControl(controls.rc_from_decremented_displacement, clk1);

      wait until falling_edge(clk2); -- tick F
      SetControl(controls.rc_from_type, clk1);

      -- bus controls only work when we're also loading from the register
      wait until falling_edge(clk2); -- tick 10
      ibus.bus_controls.bc_set_mark <= '1';
      SetControl(controls.rc_from, clk1); -- setcontrol only handles one control
                                          -- for now...
      ibus.bus_controls.bc_set_mark <= '0';
      
      wait until falling_edge(clk2); -- tick 11
      controls.rc_from <= '1';
      SetControl(ibus.bus_controls.bc_set_unmark, clk1);
      controls.rc_from <= '0';
      
      wait until falling_edge(clk2); -- tick 12
      controls.rc_from <= '1';
      SetControl(ibus.bus_controls.bc_set_pointer, clk1);
      controls.rc_from <= '0';
      
      wait until falling_edge(clk2); -- tick 13
      controls.rc_from <= '1';
      SetControl(ibus.bus_controls.bc_set_type, clk1);
      controls.rc_from <= '0';

      -- deactivate this process (need to go study the signal diagram!)
      wait until falling_edge(clk2); -- tick 14
      wait on rst;
    end process Register_Test;
end architecture behav_reg_1;
