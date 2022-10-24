-- --------------------------------------------------------------------
--                                                                   --
-- Temporary register support to help develop register code          --
--                                                                   --
-- Time-stamp: <2022-08-11 12:32:36 Bradford W. Miller(on Boromir)>  --
--                                                                   --
-- --------------------------------------------------------------------

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

    name     : in    string(1 to 10); -- for debug
    tick     : in    unsigned(11 downto 0); -- for debug

    ibus     : in    input_bus;
    obus     : out   output_bus;
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
    if ((rst = '0') and falling_edge(clk1)) then
      -- since FROM happens on rising edge of clk1, we need to wait for the bus
      -- to stablize before we copy TO. So let's do it on the falling edge of
      -- clk1 since clk2 is for setting the sense bits
      if (controls.rc_to = '1') then
        assert false report "Register: " & name &
          " Tick: " & to_string(tick) &
          " writing to local state: " & to_string(ibus.shared_bus_data) severity note;
        register_local_state <= ibus.shared_bus_data;
      else
        if (controls.rc_to_type = '1') then
          assert false report "Register: " & name &
            " Tick: " & to_string(tick) &
            " writing to local type state: " & to_string(ibus.shared_bus_data) severity note;
          register_local_state.not_pointer_bit <= ibus.shared_bus_data.not_pointer_bit;
          register_local_state.type_rest <= ibus.shared_bus_data.type_rest;
        end if;
        if (controls.rc_to_displacement = '1') then
          assert false report "Register: " & name &
            " Tick: " & to_string(tick) &
            " writing to local displacement state: " & to_string(ibus.shared_bus_data) severity note;    
          register_local_state.displacement <= ibus.shared_bus_data.displacement;
        end if;
        if (controls.rc_to_frame = '1') then
          assert false report "Register: " & name &
            " Tick: " & to_string(tick) &
            " writing to local frame state: " & to_string(ibus.shared_bus_data) severity note;     
          register_local_state.frame <= ibus.shared_bus_data.frame;
        end if;
        if (controls.rc_to_address = '1') then
          assert false report "Register: " & name &
            " Tick: " & to_string(tick) &
            " writing to local address state: " & to_string(ibus.shared_bus_data) severity note;     
          register_local_state.displacement <= ibus.shared_bus_data.displacement;
          register_local_state.frame <= ibus.shared_bus_data.frame;
        end if;
      end if; -- controls.rc_to
    end if; -- falling_edge(clk1)
  end process Register_Input_Control_Proc;

  Register_Output_Control_Proc: process(clk1, clk2)
    variable im_driving : std_logic := '0';

  begin
    if (rst = '1') then
      obus <= output_bus_init;
    elsif rising_edge(clk1) then
      if (controls.rc_from = '1') then
        im_driving := '1';
        obus.output_bus_data.type_rest <= register_local_state.type_rest;
        obus.output_bus_data.displacement <= register_local_state.displacement;
        obus.output_bus_data.frame <= register_local_state.frame;
        assert false report "Register: " & name &       
          " Tick: " & to_string(tick) &
          " reading from local state: " & to_string(register_local_state) severity note;

        -- I beleive (looking at the microcode) these only happen when we also
        -- load FROM a register so implementing here.
        if (ibus.shared_bus_controls.bc_set_mark = '1') then
          obus.output_bus_data.mark_bit <= '1';
        elsif (ibus.shared_bus_controls.bc_set_unmark = '1') then
          obus.output_bus_data.mark_bit <= '0';
        else
          obus.output_bus_data.mark_bit <= register_local_state.mark_bit;
        end if;
        
        if (ibus.shared_bus_controls.bc_set_pointer = '1') then
          obus.output_bus_data.not_pointer_bit <= '0';
        elsif (ibus.shared_bus_controls.bc_set_type = '1') then
          obus.output_bus_data.not_pointer_bit <= '1';
        else
          obus.output_bus_data.not_pointer_bit <= register_local_state.not_pointer_bit;
        end if;

      else -- we ignore other controls if we had a RC_FROM
        
        if (controls.rc_from_decremented = '1') then
          im_driving := '1';
          assert false report "Register: " & name &       
            " Tick: " & to_string(tick) &
            " reading from local state decremented: " & to_string(register_local_state) severity note;
          if (register_local_state.frame = x"000") then
            obus.output_bus_data.displacement <= register_local_state.displacement - 1;
            obus.output_bus_data.frame <= x"fff";
          else
            obus.output_bus_data.displacement <= register_local_state.displacement;
            obus.output_bus_data.frame <= register_local_state.frame - 1;
          end if;
        elsif (controls.rc_from_incremented = '1') then
          im_driving := '1';
          assert false report "Register: " & name &       
            " Tick: " & to_string(tick) &
            " reading from local state incremented: " & to_string(register_local_state) severity note;
          if (register_local_state.frame = x"fff") then
            obus.output_bus_data.displacement <= register_local_state.displacement + 1;
            obus.output_bus_data.frame <= x"000";
          else
            obus.output_bus_data.displacement <= register_local_state.displacement;
            obus.output_bus_data.frame <= register_local_state.frame + 1;
          end if;
        elsif (controls.rc_from_decremented_frame = '1') then
            im_driving := '1';
            assert false report "Register: " & name &       
              " Tick: " & to_string(tick) &
              " reading from local state decremented frame: " & to_string(register_local_state) severity note;
            obus.output_bus_data.frame <= register_local_state.frame - 1;
        elsif (controls.rc_from_decremented_displacement = '1') then
            im_driving := '1';
            assert false report "Register: " & name &       
              " Tick: " & to_string(tick) &
              " reading from local state decremented displacement: " & to_string(register_local_state) severity note;
            obus.output_bus_data.displacement <= register_local_state.displacement - 1;
        else
          obus.output_bus_data <= s79_word_init;
        end if;

        -- treat type field separately and move into low order bits (a bit of a
        -- hack but that's how we wrote the microcode - for now anyway. Later
        -- may just want to mux with the microPC (TBD)
        if (controls.rc_from_type = '1') then
          im_driving := '1';
          assert false report "Register: " & name &       
            " Tick: " & to_string(tick) &
            " reading from local state type: " & to_string(register_local_state) severity note;
          obus.output_bus_data.frame(11 downto 7) <= "00000";
          obus.output_bus_data.frame(6) <= register_local_state.not_pointer_bit;
          obus.output_bus_data.frame(5 downto 0) <= unsigned(register_local_state.type_rest);
        end if;
      end if; -- controls.rc_from

      -- if this register isn't presenting anything to the bus, set output bus
      -- low so it can be or'd with other bus outputs.
      if ((controls.rc_from = '0') and
          (controls.rc_from_decremented = '0') and
          (controls.rc_from_incremented = '0') and
          (controls.rc_from_decremented_frame = '0') and
          (controls.rc_from_decremented_displacement = '0') and
          (controls.rc_from_type = '0')) then
        im_driving := '0';
        obus <= output_bus_init;
      end if;
    end if; -- rising_edge(clk1)

    -- stop driving when clk2 ends (we will have set any senses)
    if (falling_edge(clk2) and (im_driving = '1')) then
      obus <= output_bus_init;
      im_driving := '0';
    end if;
  end process Register_Output_Control_Proc;
    
  Register_Sense_Proc : process(clk2, rst)
    variable s_eq_b : std_logic := '0';
    variable t_eq_b : std_logic := '0';
  begin
    if (rst = '1') then
      senses <= register_senses_init;
      
    elsif rising_edge(clk2) then
      if (ibus.shared_bus_data.displacement = register_local_state.displacement AND
          ibus.shared_bus_data.frame = register_local_state.frame) then
        senses.address_eq_bus <= '1';
        s_eq_b := '1';
      else
        senses.address_eq_bus <= '0';
        s_eq_b := '0';
      end if;
      if (ibus.shared_bus_data.not_pointer_bit = register_local_state.not_pointer_bit AND
          ibus.shared_bus_data.type_rest = register_local_state.type_rest) then
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
    
