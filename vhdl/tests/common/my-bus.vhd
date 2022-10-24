
-- --------------------------------------------------------------------
--                                                                   --
-- Temporary bus support to help develop bus & register code         --
--                                                                   --
-- Time-stamp: <2022-08-11 12:39:34 Bradford W. Miller(on Boromir)>  --
--                                                                   --
-- --------------------------------------------------------------------

    
-- implement bus controls and senses
-- there may be many registers interconnected with the bus, but we want only
-- one instance updating the bus senses and controls. That may mean we need to
-- treat the bus as a separate entity.

-- 7/11/22 bite the bullet and separate input and output busses, adding arbitration

-- 8/xx/22 looks like the array version doesn't work (at least under GHDL) so
-- will convert to versions with explicit signals like ibus1 ibus2, etc. Since
-- we will be generating with software this isn't a major issue.

-- also by having a separate entity, we can add arbitration later if needed?
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- library work;
use work.regpkg.all; -- temporary register types

entity bus_master is

  generic (
    Simulation_on : boolean := false; -- not currently using, but 'just in case'
    Word_width    : integer := 32); -- ideally we'd pass this to io_bus or get
                                    -- it from there?
  port (
    clk1     : in    std_logic;
    clk2     : in    std_logic;     -- two phase clock
    clk1a    : in    std_logic;     -- delayed clocks
    clk2a    : in    std_logic;
    rst      : in    std_logic;

    obus     : out   input_bus;     -- input to other modules
    ibuses   : in    bus_mux);      -- output from other modules

end entity bus_master;

architecture bus_arch of bus_master is
begin
  Bus_Mux_Proc : process (rst, clk1, clk2, clk1a, clk2a)
    variable disp : std_logic_vector (11 downto 0); 
    variable fram : std_logic_vector (11 downto 0); 
    variable mux_out : s79_word;
  begin
    if (rst = '1') then
      obus.shared_bus_data <= s79_word_init;
    else
      mux_out := s79_word_init;
      disp := (others => '0');
      fram := (others => '0');
      for i in 0 to bus_mux_max loop
        mux_out.mark_bit := mux_out.mark_bit or ibuses(i).output_bus_data.mark_bit;
        mux_out.not_pointer_bit := mux_out.not_pointer_bit or ibuses(i).output_bus_data.not_pointer_bit;
        mux_out.type_rest := mux_out.type_rest or ibuses(i).output_bus_data.type_rest;
        disp := disp or std_logic_vector(ibuses(i).output_bus_data.displacement);
        fram := fram or std_logic_vector(ibuses(i).output_bus_data.frame);
      end loop;
      mux_out.displacement := unsigned(disp);
      mux_out.frame := unsigned(fram);
      obus.shared_bus_data <= mux_out;
    end if;
  end process Bus_Mux_Proc;
  
  Bus_Sense_Proc : process (rst, clk2)
  begin
    if (rst = '1') then
      obus.bus_senses <= io_bus_senses_init;
    elsif (rising_edge(clk2)) then
      if (obus.shared_bus_data.mark_bit = '1') then
        obus.bus_senses.mark_p <= '1';
      else
        obus.bus_senses.mark_p <= '0';
      end if;
      
      if (obus.shared_bus_data.not_pointer_bit = '1') then
        obus.bus_senses.type_not_pointer <= '1';
      else
        obus.bus_senses.type_not_pointer <= '0';
      end if;
      
      if (obus.shared_bus_data.frame = X"000") then
        obus.bus_senses.frame_eq_zero <= '1';
      else
        obus.bus_senses.frame_eq_zero <= '0';
      end if;
      
      if (obus.shared_bus_data.displacement = X"000") then
        obus.bus_senses.displacement_eq_zero <= '1';
      else
        obus.bus_senses.displacement_eq_zero <= '0';
      end if;
      
      if (obus.shared_bus_data.frame = X"000" AND
          obus.shared_bus_data.displacement = X"000") then
        obus.bus_senses.address_eq_zero <= '1';
      else
        obus.bus_senses.address_eq_zero <= '0';
      end if;
    end if; -- rising_edge(clk2)
  end process Bus_Sense_Proc;
end architecture bus_arch;
