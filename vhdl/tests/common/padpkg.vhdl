-- --------------------------------------------------------------------
--                                                                   --
-- Temporary "library" to help develop pad code                      --
--                                                                   --
-- Time-stamp: <2022-08-09 12:01:52 Bradford W. Miller(on Boromir)>  --
--                                                                   --
-- --------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package padpkg is
  -- bit encoding of pad field in nanocode (see machine-wires.lisp)
  -- This should be the same as the order in pad_controls so may not be needed!
  constant set_run_nano_pad_bit          : natural := 0; -- 0x0001
  constant clear_run_nano_pad_bit        : natural := 1; -- 0x0002
  constant ale_pad_bit                   : natural := 2; -- 0x0004
  constant read_pad_bit                  : natural := 3; -- 0x0008
  constant write_pad_bit                 : natural := 4; -- 0x0010
  constant cdr_pad_bit                   : natural := 5; -- 0x0020
  constant read_interrupt_pad_bit        : natural := 6; -- 0x0040
  constant gc_needed_pad_bit             : natural := 7; -- 0x0080
  constant clear_gc_pad_bit              : natural := 8; -- 0x0100
  constant conditional_pad_bit           : natural := 9; -- 0x0200
  constant set_mask_interrupts_pad_bit   : natural := 10; -- 0x0400
  constant clear_mask_interrupts_pad_bit : natural := 11; -- 0x0800
  constant set_memory_pads_bit           : natural := 12; -- 0x1000
  constant get_memory_pads_bit           : natural := 13; -- 0x2000
  
  -- See machine-wires.lisp  
  type pad_controls is record
    set_run_nano          : std_logic;
    clear_run_nano        : std_logic; -- latched
    set_ale               : std_logic;
    set_read              : std_logic;
    set_write             : std_logic;
    set_cdr               : std_logic;
    set_read_interrupt    : std_logic;
    set_gc_needed         : std_logic;
    clear_gc_needed       : std_logic; -- latched

    set_conditional       : std_logic;
    set_mask_interrupt    : std_logic;
    clear_mask_interrupt  : std_logic;

    set_memory_pads       : std_logic; -- connect FROM ibus TO pad bus
    get_memory_pads       : std_logic; -- connect FROM pad bus TO ibus

  end record pad_controls;

  constant pad_controls_init : pad_controls := (others => '0');
  constant c_pad_bits : natural := 14; -- should be a generic as in plapkg
  function to_string(a : pad_controls) return string;
  function to_pad_controls(entry : std_logic_vector(c_pad_bits - 1 downto 0)) return pad_controls;
  
end package padpkg;

package body padpkg is
  function to_string(a: pad_controls) return string is
  begin
    return "Run Nano: " & to_string(a.set_run_nano) & "; " &
      "Clear Run Nano: " & to_string(a.clear_run_nano) & "; " &
      "Set Ale: " & to_string(a.set_ale) & "; " &
      "Set Read: " & to_string(a.set_read) & "; " &
      "Set Write: " & to_string(a.set_write) & "; " &
      "Set CDR: " & to_string(a.set_cdr) & "; " &
      "Set Read Interrupt: " & to_string(a.set_read_interrupt) & "; " &
      "Set GC Needed: " & to_string(a.set_gc_needed) & "; " &
      "Clear GC Needed: " & to_string(a.clear_gc_needed) & "; " &
      "Set Conditional: " & to_string(a.set_conditional) & "; " &
      "Set Mask Interrupt: " & to_string(a.set_mask_interrupt) & "; " &
      "Clear Mask Interrupt: " & to_string(a.clear_mask_interrupt) & "; " &
      "Set Memory Pads: " & to_string(a.set_memory_pads) & "; " &
      "Get Memory Pads: " & to_string(a.get_memory_pads);
  end function to_string;

  function to_pad_controls(entry : std_logic_vector(c_pad_bits - 1 downto 0)) return pad_controls is
    variable controls : pad_controls;
  begin
    controls.set_run_nano := entry(set_run_nano_pad_bit);
    controls.clear_run_nano := entry(clear_run_nano_pad_bit);
    controls.set_ale := entry(ale_pad_bit);
    controls.set_read := entry(read_pad_bit);
    controls.set_write := entry(write_pad_bit);
    controls.set_cdr := entry(cdr_pad_bit);
    controls.set_read_interrupt := entry(read_interrupt_pad_bit);
    controls.set_gc_needed :=  entry(gc_needed_pad_bit);
    controls.clear_gc_needed := entry(clear_gc_pad_bit);

    controls.set_conditional := entry(conditional_pad_bit);
    controls.set_mask_interrupt := entry(set_mask_interrupts_pad_bit);
    controls.clear_mask_interrupt := entry(clear_mask_interrupts_pad_bit);

    controls.set_memory_pads := entry(set_memory_pads_bit);
    controls.get_memory_pads := entry(get_memory_pads_bit);
    return controls;
  end function to_pad_controls;

end package body padpkg;













