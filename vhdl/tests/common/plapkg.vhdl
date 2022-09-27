-- ------------------------------------------------
--                                               --
-- Temporary "library" to develop pla code       --
--                                               --
-- Time-stamp: <2022-09-26 20:59:27 Bradford W. Miller(on Boromir)>      --
--                                               --
-- ------------------------------------------------

use std.textio.all;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all; -- log2

use work.regpkg.all;
use work.padpkg.all;

    /* moved to plapkg
    from_codes         :integer := 255;   -- number of distinguished from
                                          -- codes (see setup-anaphors), or max
                                          -- constant
    to_codes           :integer := 15;    -- similar to from_codes, but no constants

    pad_bits           :natural := 14;    -- see
                                          -- s79-simulator/machine-wires.lisp
                                          -- and padpkg.vhdl
    register_bits      :natural := 11;    -- see
                                          -- s79-simulator/machine-wires.lisp
                                          -- and regpkg.vhdl (note we are doing
                                          -- the minimum here, not everything
                                          -- currently in machine-wires)
    ucode_max_address  :natural := 15;    -- from lisp pla declarations
                                          -- (eventually)
    ncode_max_address  :natural := 15;    -- from lisp pla declarations
                                          -- (eventually)
    */

   -- would like to use 2008 standard generics in packages here, BUT not
   -- supported by GHDL apparently, so will have to generate libraries
   -- using constants
   -- generic (Word_width, from_codes, to_codes, pad_bits, ucode_max_address, ncode_max_address : NATURAL);

package plapkg is
  constant word_width : natural := 32;
  constant from_codes : natural := 255;
  constant to_codes   : natural := 15;
  constant pad_bits   : natural := 14; -- should be the size of pad_controls constant
  constant ucode_max_address   : natural := 15; -- 0 is an address so this is 16
  constant ncode_max_address : natural := 15; -- ditto

  function ceil_log2 (Arg : positive) return natural;

  subtype ucode_address is unsigned(ceil_log2(ucode_max_address) - 1 downto 0);
  subtype ncode_address is unsigned(ceil_log2(ncode_max_address) - 1 downto 0);
  subtype ncode_pad_control is std_logic_vector(pad_bits - 1 downto 0);

  constant ucode_address_init : ucode_address := (others => '0');
  constant ncode_address_init : ncode_address := (others => '0');

  function to_ucode_address(entry : integer) return ucode_address;
  function to_ncode_address(entry : integer) return ncode_address;

  type register_name is (none, reg1, reg2, reg3); -- ok should be the actual names
                                                  -- of registers we will be using!
                                                  -- (Maybe this should be in
                                                  -- my-registers?)

  -- leftmost enum is represented as "0".
  type from_register_code is (none, reg1, reg2); -- those allowed in the from field
                                           -- (scheme79 optimization);
                                           -- cardinality should be <=
                                           -- from_codes

  type to_register_code is (none, reg1, reg2, reg3); -- those allowed in the to
                                               -- field (scheme79
                                               -- optimization); cardinality
                                               -- should be <= to_codes

  -- fields include the codes but also possibly other things so have a separate
  -- type
  subtype ucode_from_field is std_logic_vector(ceil_log2(from_codes) - 1 downto 0);
  subtype ucode_to_field is std_logic_vector(ceil_log2(to_codes) - 1 downto 0);

  constant ucode_from_field_init : ucode_from_field := (others => '0');
  constant ucode_to_field_init : ucode_to_field := (others => '0');

  function to_from_register_code(code : ucode_from_field) return from_register_code;
  function to_to_register_code(code : ucode_to_field) return to_register_code;

  type ucode_word is record
    ucode_next : ucode_address; -- offset into the ucode array
    ucode_nano : ncode_address; -- offset into the ncode array
    ucode_from : ucode_from_field;
    ucode_to   : ucode_to_field;
  end record ucode_word;

  constant ucode_width : natural := ceil_log2(ucode_max_address) +
                                    ceil_log2(ncode_max_address) +
                                    ceil_log2(from_codes) +
                                    ceil_log2(to_codes);

  type ucode_rom_type is array (0 to ucode_max_address)
    of std_logic_vector(ucode_width - 1 downto 0);

  constant ucode_logic_init : std_logic_vector(ucode_width - 1 downto 0) := (others => '0');

  constant ucode_word_init : ucode_word := (ucode_next => ucode_address_init,
                                            ucode_nano => ncode_address_init,
                                            ucode_from => ucode_from_field_init,
                                            ucode_to   => ucode_to_field_init);

  constant ucode_rom_init : ucode_rom_type := (others => ucode_logic_init);

  function to_ucode_word(entry : std_logic_vector(ucode_width - 1 downto 0)) return ucode_word;
  
  -- nanocode also has fields, which are
  --     pad control bits
  --     register control bits
  --     next nanocontrol instruction
  
  type ncode_word is record
    ncode_pad      : pad_controls;
    ncode_register : ncode_register_controls;
    ncode_next     : ncode_address; -- offset into the ncode array
  end record ncode_word;

  constant ncode_width : natural := pad_bits +
                                    ncode_register_bits +
                                    ceil_log2(ncode_max_address);

  type ncode_rom_type is array (0 to ncode_max_address)
    of std_logic_vector(ncode_width - 1 downto 0); -- coerce to ncode_word?

  constant ncode_logic_init : std_logic_vector(ncode_width - 1 downto 0) := (others => '0');

  constant ncode_word_init : ncode_word := (ncode_pad      => pad_controls_init,
                                            ncode_register => ncode_register_controls_init,
                                            ncode_next     => ncode_address_init);

  constant ncode_rom_init : ncode_rom_type := (others => ncode_logic_init);

  function to_ncode_word(entry : std_logic_vector(ncode_width - 1 downto 0)) return ncode_word;

  function to_string(a : ncode_word) return string;
  function to_string(a : ucode_word) return string;

  constant idle_ucode_pc : ucode_address := to_ucode_address(0);
  constant idle_ncode_pc : ncode_address := to_ncode_address(0);
end package plapkg;

package body plapkg is
  -----------------------------------------------------------------------------------
  -- Combine the ceil and log2 functions.  ceil_log2(x) then gives the minimum number
  -- of bits required to represent 'x'.  ceil_log2(4) = 3, ceil_log2(7) = 3, etc.
  -----------------------------------------------------------------------------------
  function ceil_log2 (Arg : positive) return natural is
    variable RetVal:    natural;
  begin
    RetVal := Natural(log2(Real(Arg)));
    if (Arg > (2**RetVal)) then
      -- just so we can check this fn is correct!
      -- report "ceil_log2(" & to_string(Arg) & ") = " & to_string(RetVal +1);
      return(RetVal + 1); -- RetVal is too small, so bump it up by 1 and return
    else
      -- just so we can check this fn is correct!
      -- report "ceil_log2(" & to_string(Arg) & ") = " & to_string(RetVal);
      return(RetVal); -- Just right
    end if;
  end function ceil_log2;

  function to_string(a: ncode_word) return string is
  begin
    return "NCODE WORD::PAD: " & to_string(a.ncode_pad) & "; REGISTER: " &
      to_string(a.ncode_register) & "; NEXT: " &
      to_hstring(a.ncode_next);
  end function to_string;

  function to_string(a: ucode_word) return string is
  begin
    return "UCODE_WORD::NEXT: " & to_string(a.ucode_next) & "; NANO: " &
      to_hstring(a.ucode_nano) & "; FROM: " &
      to_hstring(a.ucode_from) & "; TO: " &
      to_hstring(a.ucode_to);
  end function to_string;

  function to_ncode_word(entry : std_logic_vector(ncode_width - 1 downto 0)) return ncode_word is
    variable RetVal : ncode_word;
  begin
    -- test
    -- report "ncode_width: " & to_string(ncode_width) & -- 29
    -- " pad_bits: " & to_string(pad_bits) & -- 14
    -- " ncode_register_bits: " & to_string(ncode_register_bits) & -- 10
    -- " ceil_log2(ncode_max_address): " & to_string(ceil_log2(ncode_max_address)); -- 4
    -- input vector 28 downto 0
    -- ncode_pad 28 downto 15
    -- ncode_register 14 downto 4
    -- ncode_register_controls_field 13 downto 0
    -- ncode_next 3 downto 0
    RetVal.ncode_pad := to_pad_controls(entry(ncode_width - 1 downto ncode_width - pad_bits));
    RetVal.ncode_register := to_ncode_register_controls(entry(ncode_width - pad_bits - 1 downto
                                                              ncode_width - pad_bits - ncode_register_bits));
    RetVal.ncode_next := unsigned(entry(ceil_log2(ncode_max_address) - 1 downto 0));
    return RetVal;
  end function to_ncode_word;
                                         
  function to_ucode_word(entry : std_logic_vector(ucode_width - 1 downto 0)) return ucode_word is
    variable RetVal : ucode_word;
  begin
    RetVal.ucode_next := unsigned(entry(ucode_width - 1 downto
                                        ucode_width - ceil_log2(ucode_max_address)));
    RetVal.ucode_nano := unsigned(entry(ucode_width - ceil_log2(ucode_max_address) - 1 downto
                                        ucode_width - ceil_log2(ucode_max_address) - ceil_log2(ncode_max_address)));
    RetVal.ucode_from := entry(ceil_log2(from_codes) + ceil_log2(to_codes) - 1 downto
                               ceil_log2(to_codes));
    RetVal.ucode_to := entry(ceil_log2(to_codes) - 1 downto 0);
    return RetVal;
  end function to_ucode_word;

  function to_ucode_address(entry : integer) return ucode_address is
    variable return_value : ucode_address;
  begin
    return_value := to_unsigned(entry, return_value'length);
    return return_value;
  end function to_ucode_address;
    
  function to_ncode_address(entry : integer) return ncode_address is
    variable return_value : ncode_address;
  begin
    return_value := to_unsigned(entry, return_value'length);
    return return_value;
  end function to_ncode_address;

  -- there are simpler ways to do this IF the enum type is a complete covering
  -- of the source (integer) field. But we don't have that, so...
  function to_from_register_code(code : ucode_from_field) return from_register_code is
  begin
    assert false report "to_from_register_code: " & to_string(code) severity note;
    case (code) is
      when x"01" =>
        return(reg1);
      when x"02" =>
        return(reg2);
      when others =>
        return(none);
    end case;
  end function to_from_register_code;
    
  function to_to_register_code(code : ucode_to_field) return to_register_code is
  begin
    assert false report "to_to_register_code: " & to_string(code) severity note;
    case (code) is
      when "0001" =>
        return(reg1);
      when "0010" =>
        return(reg2);
      when "0011" =>
        return(reg3);
      when others =>
        return(none);
    end case;
  end function to_to_register_code;

end package body plapkg;

-- instance for s79, should come from project
/* -- not currently fully supported by ghdl, unfortunately
  package s79 is new work.plapkg
                   generic map (Word_width => 32,
                                from_codes => 255,
                                to_codes => 15,
                                pad_bits => 14, -- should be the size of pad_controls
                                ucode_max_address => 15, -- 0 is an address so this is 16
                                ncode_max_address => 15); -- ditto
                 */                 
