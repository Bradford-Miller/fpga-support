-- ------------------------------------------------
--                                               --
-- Temporary registers to develop register code  --
--                                               --
-- Time-stamp: <2022-04-26 14:30:35 gorbag>      --
--                                               --
-- ------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- library work;
use work.regpkg.all; -- temporary register types

entity my_register is
  
  generic (
    Simulation_on : boolean := false; -- not currently using, but 'just in case'
    Word_width    : integer := 32); -- ideally we'd pass this to io_bus or get
                                    -- it from there?
  port (
    clk1     : in    std_logic;
    clk2     : in    std_logic;     -- two phase clock
    rst      : in    std_logic;
    ibus     : inout io_bus;            -- Currently support single bus
    controls : in    register_controls;
    senses   : out   register_senses := register_senses_init);

end entity my_register;

-- define a "generic" register

-- implement controls
architecture register_arch of my_register is
  -- record internal state
  signal register_local_state : s79_word := s79_word_init;

begin
  Register_Input_Control_Proc : process(clk1)
  begin
    if ((rst = '0') and rising_edge(clk1)) then
      if (controls.rc_to = '1') then
        register_local_state <= ibus.bus_data;
      else
        if (controls.rc_to_type = '1') then
          register_local_state.not_pointer_bit <= ibus.bus_data.not_pointer_bit;
          register_local_state.type_rest <= ibus.bus_data.type_rest;
        end if;
        if (controls.rc_to_displacement = '1') then
          register_local_state.displacement <= ibus.bus_data.displacement;
        end if;
        if (controls.rc_to_frame = '1') then
          register_local_state.frame <= ibus.bus_data.frame;
        end if;
        if (controls.rc_to_address = '1') then
          register_local_state.displacement <= ibus.bus_data.displacement;
          register_local_state.frame <= ibus.bus_data.frame;
        end if;
      end if; -- controls.rc_to
    end if; -- rising_edge(clk1)
  end process Register_Input_Control_Proc;

  Register_Output_Control_Proc: process(clk1)
  begin
    if ((rst = '0') and rising_edge(clk1)) then
      if (controls.rc_from = '1') then
        ibus.bus_data.type_rest <= register_local_state.type_rest;
        ibus.bus_data.displacement <= register_local_state.displacement;
        ibus.bus_data.frame <= register_local_state.frame;

        -- I beleive (looking at the microcode) these only happen when we also
        -- load FROM a register so implementing here.
        if (ibus.bus_controls.bc_set_mark = '1') then
          ibus.bus_data.mark_bit <= '1';
        elsif (ibus.bus_controls.bc_set_unmark = '1') then
          ibus.bus_data.mark_bit <= '0';
        else
          ibus.bus_data.mark_bit <= register_local_state.mark_bit;
        end if;
        
        if (ibus.bus_controls.bc_set_pointer = '1') then
          ibus.bus_data.not_pointer_bit <= '0';
        elsif (ibus.bus_controls.bc_set_type = '1') then
          ibus.bus_data.not_pointer_bit <= '1';
        else
          ibus.bus_data.not_pointer_bit <= register_local_state.not_pointer_bit;
        end if;

      else -- we ignore other controls if we had a RC_FROM
        
        if (controls.rc_from_decremented = '1') then
          if (register_local_state.frame = x"000") then
            ibus.bus_data.displacement <= register_local_state.displacement - 1;
            ibus.bus_data.frame <= x"fff";
          else
            ibus.bus_data.displacement <= register_local_state.displacement;
            ibus.bus_data.frame <= register_local_state.frame - 1;
          end if;
        elsif (controls.rc_from_incremented = '1') then
          if (register_local_state.frame = x"fff") then
            ibus.bus_data.displacement <= register_local_state.displacement + 1;
            ibus.bus_data.frame <= x"000";
          else
            ibus.bus_data.displacement <= register_local_state.displacement;
            ibus.bus_data.frame <= register_local_state.frame + 1;
          end if;
        else
          if (controls.rc_from_decremented_frame = '1') then
            ibus.bus_data.frame <= register_local_state.frame - 1;
          end if;
          if (controls.rc_from_decremented_displacement = '1') then
            ibus.bus_data.displacement <= register_local_state.displacement - 1;
          end if;
        end if; -- controls.rc_from_decremented

        -- treat type field separately and move into low order bits (a bit of a
        -- hack but that's how we wrote the microcode - for now anyway. Later
        -- may just want to mux with the microPC (TBD)
        if (controls.rc_from_type = '1') then
          ibus.bus_data.frame(11 downto 7) <= "00000";
          ibus.bus_data.frame(6) <= register_local_state.not_pointer_bit;
          ibus.bus_data.frame(5 downto 0) <= unsigned(register_local_state.type_rest);
        end if;
      end if; -- controls.rc_from

      -- if this register isn't presenting anything to the bus, set impedance
      -- high so some other register can
      
      if ((controls.rc_from = '0') and
          (controls.rc_from_decremented = '0') and
          (controls.rc_from_incremented = '0') and
          (controls.rc_from_decremented_frame = '0') and
          (controls.rc_from_decremented_displacement = '0') and
          (controls.rc_from_type = '0')) then
        ibus <= io_bus_ignore;
      end if;
    end if; -- rising_edge(clk1)
  end process Register_Output_Control_Proc;
    
  Register_Sense_Proc : process(clk2, rst)
    variable s_eq_b : std_logic := '0';
    variable t_eq_b : std_logic := '0';
  begin
    if (rst = '1') then
      senses <= register_senses_init;
      
    elsif rising_edge(clk2) then
      if (ibus.bus_data.displacement = register_local_state.displacement AND
          ibus.bus_data.frame = register_local_state.frame) then
        senses.address_eq_bus <= '1';
        s_eq_b := '1';
      else
        senses.address_eq_bus <= '0';
        s_eq_b := '0';
      end if;
      if (ibus.bus_data.not_pointer_bit = register_local_state.not_pointer_bit AND
          ibus.bus_data.type_rest = register_local_state.type_rest) then
        senses.type_eq_bus <= '1';
        t_eq_b := '1';
      else
        senses.type_eq_bus <= '0';
        t_eq_b := '0';
      end if;
      if (s_eq_b = '1' and t_eq_b = '1') then
        senses.eq_bus <= '1';
      else
        senses.eq_bus <= '0';
      end if;
    end if;
  end process Register_Sense_Proc;
  
end architecture register_arch;
    
