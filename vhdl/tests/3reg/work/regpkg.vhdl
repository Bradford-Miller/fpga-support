-- ------------------------------------------------
--                                               --
-- Temporary "library" to develop register code  --
--                                               --
-- Time-stamp: <2022-04-26 14:46:26 gorbag>      --
--                                               --
-- ------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package regpkg is
  
  type io_bus_controls is record
    -- I beleive the following are ONLY used on the bus
    bc_set_mark         : std_logic;
    bc_set_unmark       : std_logic;
    bc_set_pointer      : std_logic;
    bc_set_type         : std_logic;
  end record io_bus_controls;

  constant io_bus_controls_ignore : io_bus_controls := (bc_set_mark => 'Z',
                                                        bc_set_unmark => 'Z',
                                                        bc_set_pointer => 'Z',
                                                        bc_set_type => 'Z');

  constant io_bus_controls_init  : io_bus_controls := (bc_set_mark => '0',
                                                       bc_set_unmark => '0',
                                                       bc_set_pointer => '0',
                                                       bc_set_type => '0');

  type io_bus_senses is record
    -- these are ONLY used on the bus
    mark_p              : std_logic;
    type_not_pointer    : std_logic; -- next two rolled into one bit
    frame_eq_zero       : std_logic;
    displacement_eq_zero : std_logic;
    address_eq_zero     : std_logic;
    sub_error           : std_logic;     -- added this to aid debug - something bad
                                         -- (probably microcode) we can tie to an
                                         -- external pin
  end record io_bus_senses;

  constant io_bus_senses_ignore : io_bus_senses := (mark_p => 'Z',
                                                    type_not_pointer => 'Z',
                                                    frame_eq_zero => 'Z',
                                                    displacement_eq_zero => 'Z',
                                                    address_eq_zero => 'Z',
                                                    sub_error => 'Z');
  
  constant io_bus_senses_init  : io_bus_senses := (mark_p => '0',
                                                   type_not_pointer => '0',
                                                   frame_eq_zero => '0',
                                                   displacement_eq_zero => '0',
                                                   address_eq_zero => '0',
                                                   sub_error => 'L'); -- weak
  
  type s79_word is record
    -- defined for scheme-79; this would have to be generated for generality
    -- currently this is multiple definitions in machine-defs so we probably want
    -- a single macro to build this (TBD)
    
    mark_bit        : std_logic;          -- should be MSB
    not_pointer_bit : std_logic;
    type_rest       : std_logic_vector(5 downto 0);  -- not-pointer-bit is part
                                                     -- of the type!
    displacement    : unsigned(11 downto 0);
    frame           : unsigned(11 downto 0); -- displacement+frame is address
  end record s79_word;
  
  constant s79_word_init: s79_word := ( mark_bit => '0',
                                        not_pointer_bit => '0',
                                        type_rest => (others => '0'),
                                        displacement => (others => '0'),
                                        frame => (others => '0'));
  
  constant s79_word_ignore: s79_word := ( mark_bit => 'Z',
                                          not_pointer_bit => 'Z',
                                          type_rest => (others => 'Z'),
                                          displacement => (others => 'Z'),
                                          frame => (others => 'Z'));
                                          
  type io_bus is record
    bus_data        : s79_word;
    bus_controls    : io_bus_controls;
    bus_senses      : io_bus_senses;
  end record io_bus;

  constant io_bus_ignore : io_bus := ( bus_data => s79_word_ignore,
                                       bus_controls => io_bus_controls_ignore,
                                       bus_senses => io_bus_senses_ignore); 

  constant io_bus_init : io_bus := ( bus_data => s79_word_init,
                                     bus_controls => io_bus_controls_init,
                                     bus_senses => io_bus_senses_init);

  constant io_bus_dont_drive : io_bus := ( bus_data => s79_word_ignore,
                                           bus_controls => io_bus_controls_init,   
                                           bus_senses => io_bus_senses_ignore);

  type register_controls is record
    -- defined for scheme-79; this would have to be generated by project for generality
    -- see *control-wires* defined in s79-simulator/machine-defs
    rc_to               : std_logic; -- = to_type AND to_address
    rc_to_type          : std_logic;
    rc_to_displacement  : std_logic;
    rc_to_frame         : std_logic;
    rc_to_address       : std_logic; -- = to_displacement AND to_frame
    rc_from             : std_logic;
    rc_from_decremented : std_logic;
    rc_from_incremented : std_logic;
    rc_from_decremented_frame : std_logic;
    rc_from_decremented_displacement : std_logic;
    rc_from_type        : std_logic;
  end record register_controls;
  
  constant register_controls_init : register_controls := (rc_to => '0',
                                                          rc_to_type => '0',
                                                          rc_to_displacement => '0',
                                                          rc_to_frame => '0',
                                                          rc_to_address => '0',
                                                          rc_from => '0',
                                                          rc_from_decremented => '0',
                                                          rc_from_incremented => '0',
                                                          rc_from_decremented_frame => '0',
                                                          rc_from_decremented_displacement => '0',
                                                          rc_from_type => '0');

  constant register_controls_ignore : register_controls := (others => 'Z');

  type register_senses is record
    -- defined for scheme-79; this would have to be generated by project for generality
    address_eq_bus      : std_logic;
    type_eq_bus         : std_logic;
    eq_bus              : std_logic;
  end record register_senses;
  
  constant register_senses_init : register_senses := (address_eq_bus  => '0',
                                                      type_eq_bus => '0',
                                                      eq_bus => '0');

  procedure SetControl (
    signal Control : out std_logic;
    signal clk1 : in std_logic);

  procedure SetControls (
    signal Control1 : out std_logic;
    signal Control2 : out std_logic;
    signal clk1 : in std_logic);

  procedure SetControls3 (
    signal Control1 : out std_logic;
    signal Control2 : out std_logic;
    signal Control3 : out std_logic;
    signal clk1 : in std_logic);
          
end package regpkg;

package body regpkg is
  -- expect this to be called on a single control signal during
  -- falling_edge(clk2) (for now). Note that there should be only one "process"
  -- that is creating control calls so we can drive 0 (instead of Z) otherwise.
  procedure SetControl(signal Control : out std_logic;
                       signal clk1 : in std_logic) is
  begin
    Control <= '1';
    wait until falling_edge(clk1);
    Control <= '0';
  end procedure SetControl;

  -- setting two controls (from/to) is pretty common so that's what SetControls
  -- does.
  procedure SetControls(signal Control1 : out std_logic;
                        signal Control2 : out std_logic;
                        signal clk1 : in std_logic) is
  begin
    Control1 <= '1';
    Control2 <= '1';
    wait until falling_edge(clk1);
    Control1 <= '0';
    Control2 <= '0';
  end procedure SetControls;

  -- probably only used when we are also setting a bus control
  procedure SetControls3(signal Control1 : out std_logic;
                         signal Control2 : out std_logic;
                         signal Control3 : out std_logic;
                         signal clk1 : in std_logic) is
  begin
    Control1 <= '1';
    Control2 <= '1';
    Control3 <= '1';
    wait until falling_edge(clk1);
    Control1 <= '0';
    Control2 <= '0';
    Control3 <= '1';
  end procedure SetControls;
  
end package body regpkg;
  
