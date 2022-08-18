-- ------------------------------------------------
--                                               --
-- Temporary registers to develop register code  --
--                                               --
-- Time-stamp: <2022-08-18 14:17:54 Bradford W. Miller(on Boromir)>      --
--                                               --
-- ------------------------------------------------

    
-- implement bus controls and senses
-- there may be many registers interconnected with the bus, but we want only
-- one instance updating the bus senses and controls. That may mean we need to
-- treat the bus as a separate entity.

-- 7/11/22 bite the bullet and separate input and output busses, adding arbitration

-- Note that this also delays getting the output shared between processes due
-- to buffering. This ends up being critical for the (external) memory bus so
-- we will move the memory bus gate logic here!! -- 8/18/22

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- library work;
use work.regpkg.all; -- temporary register types
use work.padpkg.all; -- pad controls 

entity bus_master6 is

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

    my_pad_controls : in pad_controls; -- add this to see if we are trying to
                                       -- read the external bus
    pads_memory_bus : inout s79_word := s79_word_ignore;

    obus     : out   input_bus;     -- input to other modules
    ibus1    : in    output_bus;      -- output from module1
    ibus2    : in    output_bus;
    ibus3    : in    output_bus;
    ibus4    : in    output_bus;
    ibus5    : in    output_bus;
    ibus6    : in    output_bus);     -- output from module2

end entity bus_master6;

architecture bus_arch of bus_master6 is
begin
  Bus_Mux_Proc : process (rst, clk1, clk2, clk1a, clk2a)
    variable disp : std_logic_vector (11 downto 0); 
    variable fram : std_logic_vector (11 downto 0); 
    variable mux_out : s79_word;
    variable controls_out : io_bus_controls;
  begin
    if (rst = '1') then
      obus.shared_bus_data <= s79_word_init;
      obus.shared_bus_controls <= io_bus_controls_init;
    else
      if falling_edge(clk1a) then
        pads_memory_bus <= s79_word_ignore; -- don't drive beyond clk1a
      end if;
      
      mux_out := s79_word_init;
      disp := (others => '0');
      fram := (others => '0');
      controls_out := io_bus_controls_init;

      if (my_pad_controls.get_memory_pads = '1') then
        -- supercede with the memory bus. 
        mux_out.mark_bit := pads_memory_bus.mark_bit;
        mux_out.not_pointer_bit := pads_memory_bus.not_pointer_bit;
        mux_out.type_rest := pads_memory_bus.type_rest;
        disp := std_logic_vector(pads_memory_bus.displacement);
        fram := std_logic_vector(pads_memory_bus.frame);
      else
        mux_out.mark_bit := ibus1.output_bus_data.mark_bit or ibus2.output_bus_data.mark_bit or
                            ibus3.output_bus_data.mark_bit or ibus4.output_bus_data.mark_bit or
                            ibus5.output_bus_data.mark_bit or ibus6.output_bus_data.mark_bit;
        mux_out.not_pointer_bit := ibus1.output_bus_data.not_pointer_bit or ibus2.output_bus_data.not_pointer_bit or
                                   ibus3.output_bus_data.not_pointer_bit or ibus4.output_bus_data.not_pointer_bit or
                                   ibus5.output_bus_data.not_pointer_bit or ibus6.output_bus_data.not_pointer_bit;
        mux_out.type_rest := ibus1.output_bus_data.type_rest or ibus2.output_bus_data.type_rest or
                             ibus3.output_bus_data.type_rest or ibus4.output_bus_data.type_rest or
                             ibus5.output_bus_data.type_rest or ibus6.output_bus_data.type_rest;
        disp := std_logic_vector(ibus1.output_bus_data.displacement) or std_logic_vector(ibus2.output_bus_data.displacement) or
                std_logic_vector(ibus3.output_bus_data.displacement) or std_logic_vector(ibus4.output_bus_data.displacement) or
                std_logic_vector(ibus5.output_bus_data.displacement) or std_logic_vector(ibus6.output_bus_data.displacement);
        fram := std_logic_vector(ibus1.output_bus_data.frame) or std_logic_vector(ibus2.output_bus_data.frame) or
                std_logic_vector(ibus3.output_bus_data.frame) or std_logic_vector(ibus4.output_bus_data.frame) or
                std_logic_vector(ibus5.output_bus_data.frame) or std_logic_vector(ibus6.output_bus_data.frame);
      end if;
      
      controls_out.bc_set_mark := ibus1.bus_controls.bc_set_mark or ibus2.bus_controls.bc_set_mark or
                                  ibus3.bus_controls.bc_set_mark or ibus4.bus_controls.bc_set_mark or
                                  ibus5.bus_controls.bc_set_mark or ibus6.bus_controls.bc_set_mark;
      controls_out.bc_set_unmark := ibus1.bus_controls.bc_set_unmark or ibus2.bus_controls.bc_set_unmark or
                                    ibus3.bus_controls.bc_set_unmark or ibus4.bus_controls.bc_set_unmark or
                                    ibus5.bus_controls.bc_set_unmark or ibus6.bus_controls.bc_set_unmark;
      controls_out.bc_set_pointer := ibus1.bus_controls.bc_set_pointer or ibus2.bus_controls.bc_set_pointer or
                                     ibus3.bus_controls.bc_set_pointer or ibus4.bus_controls.bc_set_pointer or
                                     ibus5.bus_controls.bc_set_pointer or ibus6.bus_controls.bc_set_pointer;
      controls_out.bc_set_type := ibus1.bus_controls.bc_set_type or ibus2.bus_controls.bc_set_type or
                                  ibus3.bus_controls.bc_set_type or ibus4.bus_controls.bc_set_type or
                                  ibus5.bus_controls.bc_set_type or ibus6.bus_controls.bc_set_type;
      
      mux_out.displacement := unsigned(disp);
      mux_out.frame := unsigned(fram);
      obus.shared_bus_data <= mux_out;
      obus.shared_bus_controls <= controls_out;
      if (my_pad_controls.set_memory_pads = '1') then
        pads_memory_bus <= mux_out; -- putting this here saves us a half phase
                                    -- on signal validity!
      else
        pads_memory_bus <= s79_word_ignore;
      end if;
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

  /* this WAS in my-pads. It is now integrated into Bus_Mux_Proc to avoid the
     1/2 phase delay!
  
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

end architecture bus_arch;
