-- --------------------------------------------------------------------
--                                                                   --
-- Temporary "library" to help develop register code                 --
--                                                                   --
-- Time-stamp: <2022-10-05 16:28:16 Bradford W. Miller(on Boromir)>  --
--                                                                   --
-- --------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package regpkg is
  -- bit encoding of register field in nanocode (see machine-wires.lisp)
  -- nrc: nanocode register control
  constant nrc_from_star_bit            : natural := 0; -- 0x0001
  constant nrc_to_star_bit              : natural := 1; -- 0x0002
  constant nrc_from_to_star_bit         : natural := 2; -- 0x0004
  constant nrc_from_type_const_star_bit : natural := 3; -- 0x0008
  constant nrc_from_const_star_bit      : natural := 4; -- 0x0010
  constant nrc_from_star_type_bit       : natural := 5; -- 0x0020
  -- these would be generated from the macros, we can add temporaries here

  -- bus controls is also in register field? (maybe should be in pad field
  -- since there's only one bus?)
  constant nrc_set_mark_bit             : natural := 6; -- 0x0040
  constant nrc_set_unmark_bit           : natural := 7; -- 0x0080
  constant nrc_set_pointer_bit          : natural := 8; -- 0x0100
  constant nrc_set_type_bit             : natural := 9; -- 0x0200

  type ncode_register_controls is record
    nrc_from_star               : std_logic;
    nrc_to_star                 : std_logic;
    nrc_from_to_star            : std_logic;
    nrc_from_type_const_star    : std_logic;
    nrc_from_const_star         : std_logic;
    nrc_from_star_type          : std_logic;
    nrc_set_mark                : std_logic;
    nrc_set_unmark              : std_logic;
    nrc_set_pointer             : std_logic;
    nrc_set_type                : std_logic;
  end record ncode_register_controls;

  function to_string(a : ncode_register_controls) return string;

  constant ncode_register_bits : natural := 10; -- should be generic as in
                                                -- plapkg.vhdl

  constant ncode_register_controls_init : ncode_register_controls := ((others => '0'));

  type io_bus_controls is record
    bc_set_mark         : std_logic;
    bc_set_unmark       : std_logic;
    bc_set_pointer      : std_logic;
    bc_set_type         : std_logic;
  end record io_bus_controls;

  -- shouldn't be needed 7/11/22
  /*
  constant io_bus_controls_ignore : io_bus_controls := (bc_set_mark => 'Z',
                                                        bc_set_unmark => 'Z',
                                                        bc_set_pointer => 'Z',
                                                        bc_set_type => 'Z');
  */

  constant io_bus_controls_init  : io_bus_controls := (bc_set_mark => '0',
                                                       bc_set_unmark => '0',
                                                       bc_set_pointer => '0',
                                                       bc_set_type => '0');

  -- bit encoding of TO field in microcode when representing sense bits for
  -- conditionals (added 10/5/22)
  -- (mbs: microcode bus sense)

  -- a more complete solution would allow us to specify the register (inc. the
  -- bus) and the related sense, but since it is sparse we would still want to
  -- only encode those that are actually in use!
  constant mbs_mark_p               : natural := 0; -- 0x01
  constant mbs_type_not_pointer     : natural := 1; -- 0x02
  constant mbs_frame_eq_zero        : natural := 2; -- 0x04
  constant mbs_displacement_eq_zero : natural := 3; -- 0x08
  -- NB: for our temporary test code we can only generate the first 4 bits (to
  -- fit into our abbreviated TO field in the microcode representation)
  -- later we'll have to be able to generate these and appropriate register
  -- specific sense line references in the TO field!!
  constant mbs_address_eq_zero      : natural := 4; -- 0x10
  constant mbs_sub_error            : natural := 5; -- 0x20 -- not part of
                                                    -- original chip!
  
  type io_bus_senses is record
    -- these are ONLY used on the bus
    mark_p               : std_logic;
    type_not_pointer     : std_logic; -- next two rolled into one bit
    frame_eq_zero        : std_logic;
    displacement_eq_zero : std_logic;
    address_eq_zero      : std_logic;
    sub_error            : std_logic;     -- added this to aid debug - something bad
                                          -- (probably microcode) we can tie to an
                                          -- external pin
  end record io_bus_senses;

  -- shouldn't be needed 7/11/22
  /*
    constant io_bus_senses_ignore : io_bus_senses := (mark_p => 'Z',
                                                    type_not_pointer => 'Z',
                                                    frame_eq_zero => 'Z',
                                                    displacement_eq_zero => 'Z',
                                                    address_eq_zero => 'Z',
                                                    sub_error => 'Z');
  */
  
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

  -- shouldn't be needed 7/11/22

  constant s79_word_ignore: s79_word := ( mark_bit => 'Z',
                                          not_pointer_bit => 'Z',
                                          type_rest => (others => 'Z'),
                                          displacement => (others => 'Z'),
                                          frame => (others => 'Z'));

  function to_string ( a: s79_word) return string;


  -- output and input here are from the perspective of a bus client.

  type output_bus is record
    output_bus_data : s79_word;
    bus_controls    : io_bus_controls;
  end record output_bus;

  type input_bus is record
    shared_bus_data : s79_word;
    shared_bus_controls : io_bus_controls;
    bus_senses      : io_bus_senses;
  end record input_bus;

  constant output_bus_init : output_bus := ( output_bus_data => s79_word_init,
                                             bus_controls => io_bus_controls_init);

  constant rc_to_bit                            : natural := 0; -- 0x0001
  constant rc_to_type_bit                       : natural := 1; -- 0x0002
  constant rc_to_displacement_bit               : natural := 2; -- 0x0004
  constant rc_to_frame_bit                      : natural := 3; -- 0x0008
  constant rc_to_address_bit                    : natural := 4; -- 0x0010
  constant rc_from_bit                          : natural := 5; -- 0x0020
  constant rc_from_decremented_bit              : natural := 6; -- 0x0040
  constant rc_from_incremented_bit              : natural := 7; -- 0x0080
  constant rc_from_decremented_frame_bit        : natural := 8; -- 0x0100
  constant rc_from_decremented_displacement_bit : natural := 9; -- 0x0200
  constant rc_from_type_bit                     : natural := 10; -- 0x0400


  type register_controls is record
    -- defined for scheme-79; this would have to be generated by project for generality
    -- see *control-wires* defined in s79-simulator/machine-defs
    rc_to                               : std_logic; -- = to_type AND to_address
    rc_to_type                          : std_logic;
    rc_to_displacement                  : std_logic;
    rc_to_frame                         : std_logic;
    rc_to_address                       : std_logic; -- = to_displacement AND to_frame
    rc_from                             : std_logic;
    rc_from_decremented                 : std_logic;
    rc_from_incremented                 : std_logic;
    rc_from_decremented_frame           : std_logic;
    rc_from_decremented_displacement    : std_logic;
    rc_from_type                        : std_logic;
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
  

  -- shouldn't be needed 7/11/22
  /*
  constant register_controls_ignore : register_controls := (others => 'Z');
  */

  subtype ncode_register_controls_field is std_logic_vector(ncode_register_bits - 1 downto 0);

  function to_ncode_register_controls(entry : ncode_register_controls_field) return ncode_register_controls;

  -- function to_register_controls(entry : ncode_register_controls_field) return register_controls;

  function to_from_register_controls(entry : ncode_register_controls) return register_controls;

  function to_to_register_controls(entry : ncode_register_controls) return register_controls;

  function to_string(a : register_controls) return string;

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
    Control3 <= '0';
  end procedure SetControls3;

  function to_string(a: s79_word) return string is
  begin
    return to_hstring(a.mark_bit & a.not_pointer_bit & a.type_rest(5 downto 4)) &
      to_hstring(a.type_rest(3 downto 0)) &
      to_hstring(a.displacement) &
      to_hstring(a.frame);
  end function to_string;

  function to_string(a : register_controls) return string is
  begin
    return 
      "rc_to: " & to_string(a.rc_to) &
      "; rc_to_type: " & to_string(a.rc_to_type) & 
      "; rc_to_displacement: " & to_string(a.rc_to_displacement) & 
      "; rc_to_frame: " & to_string(a.rc_to_frame) & 
      "; rc_to_address: " & to_string(a.rc_to_address) & 
      "; rc_from: " & to_string(a.rc_from) & 
      "; rc_from_decremented: " & to_string(a.rc_from_decremented) & 
      "; rc_from_incremented: " & to_string(a.rc_from_incremented) & 
      "; rc_from_decremented_frame: " & to_string(a.rc_from_decremented_frame) & 
      "; rc_from_decremented_displacement: " & to_string(a.rc_from_decremented_displacement) & 
      "; rc_from_type: " & to_string(a.rc_from_type);
  end function to_string;

  function to_ncode_register_controls(entry : ncode_register_controls_field)
    return ncode_register_controls is  
    variable controls : ncode_register_controls;
  begin
    controls.nrc_from_star := entry(nrc_from_star_bit);
    controls.nrc_to_star := entry(nrc_to_star_bit);
    controls.nrc_from_to_star := entry(nrc_from_to_star_bit);
    controls.nrc_from_type_const_star := entry(nrc_from_type_const_star_bit);
    controls.nrc_from_const_star := entry(nrc_from_const_star_bit);
    controls.nrc_from_star_type := entry(nrc_from_star_type_bit);
    controls.nrc_set_mark := entry(nrc_set_mark_bit);
    controls.nrc_set_unmark := entry(nrc_set_unmark_bit);
    controls.nrc_set_pointer := entry(nrc_set_pointer_bit);
    controls.nrc_set_type := entry(nrc_set_type_bit);
    return controls;
  end function to_ncode_register_controls;

  -- presumably only comes up when using anaphors? Anyway this covers the cases
  -- in the current test. Note we will need to take a pass at the LISP
  -- simulation to elminate some unnessary(?) explicit controls on particular
  -- registers when anaphor could be used instead (as in the original chip),
  -- which means there will probably me more of the star controls (indirect
  -- through the ucode) and this will become more populated.

  -- also note that BUS controls are not addressed yet (TBD)
  function to_from_register_controls(entry : ncode_register_controls) return register_controls is
    variable controls : register_controls;
  begin
    controls.rc_to := '0';
    controls.rc_to_type := '0';
    controls.rc_to_displacement := '0';
    controls.rc_to_frame := '0';
    controls.rc_to_address := '0';
    controls.rc_from := entry.nrc_from_star or entry.nrc_from_to_star;
    controls.rc_from_decremented := '0'; -- ?
    controls.rc_from_incremented := '0'; -- ?
    controls.rc_from_decremented_frame := '0'; -- ?
    controls.rc_from_decremented_displacement := '0'; -- ?
    controls.rc_from_type := entry.nrc_from_star_type;
    return controls;
  end function to_from_register_controls;
  
  function to_to_register_controls(entry : ncode_register_controls) return register_controls is
    variable controls : register_controls;
  begin
    controls.rc_to := entry.nrc_to_star;
    controls.rc_to_type := '0';
    controls.rc_to_displacement := '0';
    controls.rc_to_frame := '0';
    controls.rc_to_address := '0';
    controls.rc_from := '0';
    controls.rc_from_decremented := '0'; 
    controls.rc_from_incremented := '0'; 
    controls.rc_from_decremented_frame := '0'; 
    controls.rc_from_decremented_displacement := '0'; 
    controls.rc_from_type := '0';
    return controls;
  end function to_to_register_controls;

  function to_string(a : ncode_register_controls) return string is
  begin
    return "nrc_from_star: " & to_string(a.nrc_from_star) & "; " &
      "nrc_to_star: " & to_string(a.nrc_to_star) & "; " &
      "nrc_from_to_star: " & to_string(a.nrc_from_to_star) & "; " &
      "nrc_from_type_const_star: " & to_string(a.nrc_from_type_const_star) & "; " &
      "nrc_from_const_star: " & to_string(a.nrc_from_const_star) & "; " &
      "nrc_from_star_type: " & to_string(a.nrc_from_star_type) & "; " &
      "nrc_set_mark: " & to_string(a.nrc_set_mark) & "; " &
      "nrc_set_unmark: " & to_string(a.nrc_set_unmark) & "; " &
      "nrc_set_pointer: " & to_string(a.nrc_set_pointer) & "; " &
      "nrc_set_type: " & to_string(a.nrc_set_type) & " ";
  end function to_string;
end package body regpkg;

