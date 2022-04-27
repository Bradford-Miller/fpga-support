-- ------------------------------------------------
--                                               --
-- Temporary registers to develop register code  --
--                                               --
-- Time-stamp: <2022-04-26 14:27:32 gorbag>      --
--                                               --
-- ------------------------------------------------

    
-- implement bus controls and senses
-- there may be many registers interconnected with the bus, but we want only
-- one instance updating the bus senses and controls. That may mean we need to
-- treat the bus as a separate entity.

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
    rst      : in    std_logic;
    ibus     : inout io_bus);       -- Currently support single bus

end entity bus_master;

architecture bus_arch of bus_master is
begin
  Bus_Sense_Proc : process (rst, clk2)
  begin
    if (rst = '1') then
      ibus.bus_senses <= io_bus_senses_init;
    elsif (rising_edge(clk2)) then
      if (ibus.bus_data.mark_bit = '1') then
        ibus.bus_senses.mark_p <= '1';
      else
        ibus.bus_senses.mark_p <= '0';
      end if;
      
      if (ibus.bus_data.not_pointer_bit = '1') then
        ibus.bus_senses.type_not_pointer <= '1';
      else
        ibus.bus_senses.type_not_pointer <= '0';
      end if;
      
      if (ibus.bus_data.frame = X"000") then
        ibus.bus_senses.frame_eq_zero <= '1';
      else
        ibus.bus_senses.frame_eq_zero <= '0';
      end if;
      
      if (ibus.bus_data.displacement = X"000") then
        ibus.bus_senses.displacement_eq_zero <= '1';
      else
        ibus.bus_senses.displacement_eq_zero <= '0';
      end if;
      
      if (ibus.bus_data.frame = X"000" AND
          ibus.bus_data.displacement = X"000") then
        ibus.bus_senses.address_eq_zero <= '1';
      else
        ibus.bus_senses.address_eq_zero <= '0';
      end if;
    end if; -- rising_edge(clk2)
  end process Bus_Sense_proc;
end architecture bus_arch;
