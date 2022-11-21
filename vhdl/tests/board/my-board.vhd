-- ------------------------------------------------------------------------
--                                                                       --
--   Board-level description (pulls together chip into Arty A7 board)    --
--                                                                       --
--   Time-stamp: <2022-11-16 12:38:43 Bradford W. Miller(on Boromir)>    --
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

entity arty_board_s7 is
  component arty_interface
    port (
      ck_rst    : in   std_logic; -- connects to reset button on the board and
                                  -- chipkit line (arduino)
      );
  end component arty_interface;

end entity arty_board_s7;

architecture board_arch of arty_board_s7 is
  component s79_chip
    generic (
      Simulation_on :boolean := false; -- not currently using
      Word_width    :integer := 32);

    -- trying to keep the port names short as we will have to work with them in Vivado
    port (
      s79_clkph1  : in     std_logic;
      s79_clkph2  : in     std_logic;
      -- freeze
      s79_frz     : in     std_logic;
      -- read state
      s79_rds     : in     std_logic;
      -- load state
      s79_lds     : in     std_logic;
      -- interrupt request
      s79_irq     : in     std_logic;
      -- address/data bus
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
  end component s79_chip;

  component debounce
    generic(
      clk_freq    : INTEGER := 100_000_000;  --system clock frequency in Hz
      stable_time : INTEGER := 10);         --time button must remain stable in ms
    PORT(
      clk     : IN  STD_LOGIC;  --input clock
      reset_n : IN  STD_LOGIC;  --asynchronous active low reset
      button  : IN  STD_LOGIC;  --input signal to be debounced
      result  : OUT STD_LOGIC); --debounced signal

  end component debounce;
  -- signals we will use for I/O to the board
  debounced_rst     : in     std_logic; 

begin
  Debounce1 : debounce
    port map (
      clk => s79_clkph1,
      reset_n => ck_rst, -- active low, so when button is off we reset
      button => ck_rst,
      result => debounced_rst);
  
  My_Chip : s79_chip
    port map (
      s79_clkph1 =>
      s79_clkph2 =>
      s79_frz =>
      s79_rds =>
      s79_lds =>
      s79_irq =>

      s79_adb =>
      s79_cdr =>
      s79_ale =>
      s79_rd =>
      s79_wr =>
      s79_rdint =>
      s79_gcn =>

      s79_rst => debounced_rst,
      s79_clkph1a =>
      s79_clkph2a =>
    );

  -- we will use a memory interface?
  /*
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
  */
  
end architecture board_arch;
