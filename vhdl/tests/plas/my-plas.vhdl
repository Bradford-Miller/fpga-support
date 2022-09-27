-- ------------------------------------------------------------------------
--                                                                       --
--               Temporary plas (ROMs) to develop micro and              --
--                  nanocontroller code representation                   --
--                                                                       --
--   Time-stamp: <2022-09-26 20:50:34 Bradford W. Miller(on Boromir)>    --
--                                                                       --
-- ------------------------------------------------------------------------

-- while the original Scheme-79 chip used a PLA (programmable AND columns and
-- OR rows) for microcode, we will rely on the FPGA tools to pick the
-- appropriate representation for the microcode (e.g. ROM). Here we simply
-- define the interfaces that allow us to execute a given microcode or
-- nanocode array in a representation similar to what was used on the orignal
-- chip (multiple binary fields in a given microcode or nanocode "word")

-- in addition, because we will be loading our microcode and nanocode from
-- project defined tools, we load the initial contents from a file and then
-- treat the array as a ROM here. New releases of microcode or nanocode
-- therefore require new bitfiles to be downloaded to the FPGA, but future
-- versions could add the ability to do firmware upgrades (though as the
-- actual process would be similar to just reloading the FPGA it hardly seems
-- necessary to add the complexity and larger footprint of a writable array).

  -- read hex values from a file:
  -- modified from https://vhdlwhiz.com/initialize-ram-from-file/ on 5/20/22.

-- constant rom_depth : natural := 256;
-- constant rom_width : natural := 32;

-- type rom_type is array (0 to rom_depth -1)
--   of std_logic_vector(rom_width - 1 downto 0);

--impure function init_rom_hex return rom_type is
--  file text_file : text open read_mode is "rom_content_hex.txt";
--  variable text_line : line;
--  variable rom_content : rom_type;
-- begin
--  for i in 0 to rom_depth - 1 loop
--    readline(text_file, text_line);
--    hread(text_line, rom_content(i));
--  end loop;
--  return rom_content;
-- end function;

-- signal rom_hex : rom_type := init_rom_hex;

use std.textio.all;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.regpkg.all;
use work.padpkg.all;
use work.plapkg.all;

entity pla_master is
  generic (
    Simulation_on       : boolean := false; -- not currently using
    -- get this from plapkg now
    -- word_width          : integer := 32;    -- size of CAR and CDR (each; 2x is
                                               -- a CONS)
    
     -- from lisp pla declarations (eventually). Reserve 0 for no-op!
    boot_load_address   : ucode_address := to_ucode_address(1));

  port (
    clk1                : in  std_logic;
    clk1a               : in  std_logic;
    clk2                : in  std_logic;
    clk2a               : in  std_logic;
    rst                 : in  std_logic;

    ibus                : in  input_bus;
    obus                : out output_bus;
    
    run_nano            : in  std_logic;
    pad_freeze          : in  std_logic;

    ncode_value         : out ncode_word;
    ucode_value         : out ucode_word;

    my_pad_controls     : out pad_controls;

    reg1_controls       : out register_controls;
    reg2_controls       : out register_controls;
    reg3_controls       : out register_controls);

    -- address and memory are treated as (pseudo) registers, gated by pads
--    address_controls    : out register_controls;
--    memory_controls     : out register_controls);

end entity pla_master;

architecture behav_pla_master of pla_master is
  -- local signals and variables

  -- ucode has four fields: the next ucode instruction address,
  --                        the nanooperation (first) address
  --                        the from register code
  --                        the to register code(s)

  -- these can be overlaid with, e.g. the sense bits for a conditional
  -- operation (in the to field), etc. For our immediate purposes we're
  -- keeping it simple but the point is that these four fields do different
  -- things so while we read them in from one address, we need to segregate
  -- them to process them.

  -- initialize the nano and micro pla

  impure function init_ucode_rom return ucode_rom_type is
    file text_file : text open read_mode is "ucode_hex.txt"; -- probably want
                                                             -- to pass this as
                                                             -- an argument or
                                                             -- default to some
                                                             -- constant?
    variable text_line : line;
    variable ucode_content : ucode_rom_type := ucode_rom_init;
  begin
    for i in 0 to ucode_max_address loop
      readline(text_file, text_line);
      --report "read text_line: " & text_line.all; -- worked!
      hread(text_line, ucode_content(i));
      --report "converted: " & to_string(ucode_content(i)); -- worked!
    end loop;
    -- print out a few entries to make sure we got it
    --report "ucode_content(1): " & to_string(to_ucode_word(ucode_content(1))) severity note;
    --report "ucode_content(2): " & to_string(to_ucode_word(ucode_content(2))) severity note;
    return ucode_content;
  end function init_ucode_rom;

  impure function init_ncode_rom return ncode_rom_type is
    file text_file : text open read_mode is "ncode_hex.txt"; -- ditto
    variable text_line : line;
    variable ncode_content : ncode_rom_type := ncode_rom_init;
  begin
    for i in 0 to ncode_max_address loop
      readline(text_file, text_line);
      -- report "read text_line: " & text_line.all; -- worked!
      hread(text_line, ncode_content(i));
      -- report "converted: " & to_string(ncode_content(i)); -- worked!
    end loop;
    -- print out a few entries to make sure we got it
    --report "ncode_content(1): " & to_string(to_ncode_word(ncode_content(1)));
    --report "ncode_content(2): " & to_string(to_ncode_word(ncode_content(2)));
    return ncode_content;
  end function init_ncode_rom;
  
  signal ucode_rom : ucode_rom_type := init_ucode_rom;
  signal ncode_rom : ncode_rom_type := init_ncode_rom;

  signal ucode_pc : ucode_address;
  signal ncode_pc : ncode_address;

  impure function get_ucode(a : ucode_address) return ucode_word is
    variable i : integer := to_integer(a);
  begin
    return to_ucode_word(ucode_rom(i));
  end function get_ucode;

  impure function get_ncode(a : ncode_address) return ncode_word is
    variable i : integer := to_integer(a);
  begin
    return to_ncode_word(ncode_rom(i));
  end function get_ncode;
  
  procedure set_from_anaphor_control(variable code : in ucode_from_field;
                                     variable reg_control : in ncode_register_controls;
                                     signal xreg1_controls : out register_controls;
                                     signal xreg2_controls : out register_controls) is
    variable from_code : from_register_code := to_from_register_code(code);
  begin
    assert false report "set_from_anaphor_control from_code, code: " & to_string(from_code) & " " & to_string(code) severity note;
    -- this is something we clearly want to auto-generate! Note that control may
    -- have multiple bits set
    case (from_code) is
      when reg1 =>
        xreg1_controls <= to_from_register_controls(reg_control);
        xreg2_controls <= register_controls_init;   -- we may set to controls
                                                    -- on this register, but
                                                    -- we also don't have
                                                    -- internal tristates so
                                                    -- may need to modify
                                                    -- this
      when reg2 =>
        xreg2_controls <= to_from_register_controls(reg_control);
        xreg1_controls <= register_controls_init;
      when none =>
        assert false report "illegitimate from_anaphor_code: " & to_string(code);
    end case;
  end procedure set_from_anaphor_control;

  procedure set_to_anaphor_control(variable code : in ucode_to_field;
                                   variable reg_control : in ncode_register_controls;
                                   signal xreg1_controls : out register_controls;
                                   signal xreg2_controls : out register_controls;
                                   signal xreg3_controls : out register_controls) is
    variable to_code : to_register_code := to_to_register_code(code);
  begin
  -- this is something we clearly want to auto-generate!
    assert false report "set_to_anaphor_control to_code, code: " & to_string(to_code) & " " & to_string(code) severity note;
    case (to_code) is
      when reg1 =>
        xreg1_controls <= to_to_register_controls(reg_control);
        xreg2_controls <= register_controls_init;
        xreg3_controls <= register_controls_init;
      when reg2 =>
        xreg1_controls <= register_controls_init;
        xreg2_controls <= to_to_register_controls(reg_control);
        xreg3_controls <= register_controls_init;
      when reg3 =>
        xreg1_controls <= register_controls_init;
        xreg2_controls <= register_controls_init;
        xreg3_controls <= to_to_register_controls(reg_control);
      when none =>      
        assert false report "illegitimate to_anaphor_code: " & to_string(code);
    end case;
  end procedure set_to_anaphor_control;

  
  -- main loop

  -- current lisp sim runs microcontroller on ph1-falling, nanocontroller-p2 on
  -- ph1-falling and nanocontroller-p1 on ph2-rising

  -- we may need to use the phase shifted versions?

  -- were in our prior tests we had the testbed run the controls on the
  -- registers and bus, now that should all be done by the nanocontroller. The
  -- good news is there is only one place where the controls are so we don't
  -- need to worry about tristating other than the data bus (which we'll have
  -- to make one-way and OR or MUX rather than let the tool do that to make
  -- sure we have full control over it; no internal pullups or pulldowns!)

  -- the from and to fields in the ucode may need translation to identify which
  -- set of controls to modify. Note that the from and to fields have different
  -- encodings so the lisp code has to generate two tables, one for the from
  -- code, one for the to code as it does in the lisp simulation. In addition,
  -- since, unlike lisp, we don't have symbolic processing, we have to create
  -- enumeration values for the register names and controls so we can use a 2D
  -- table (given a register number and control number) that will map to the
  -- particular control signal we will want to manipulate. As in the lisp code,
  -- that requires standardized naming conventions for generation.

  -- Basic clock cycle used here is:
  -- rising(clk1): perform register and bus operations, run microcontrol cycle
  --                 (unless supressed with freeze or run-nano)
  -- falling(clk1): clear register controls
  -- rising(clk2): set bus and register sense lines, also run nanocontrol cycle
  --                 which will...
  -- falling(clk2): set up controls for next clk1 cycle

begin
  MicroControl_Proc : process(clk1, clk1a, clk2, clk2a) -- currently runs both micro and
                                          -- nano, may want to separate?
    -- used when decoding the nanocontrol array. They should only be set by one
    -- process (otherwise we need to set up protected types)
    -- variable current_ncode_pc : ncode_address;
    variable current_ncode_pad_controls : pad_controls;
    variable current_ncode_register_controls : ncode_register_controls;
    variable u_to : ucode_to_field;
    variable u_from : ucode_from_field;
    variable u_to_converted : ucode_from_field; -- when we use
                                                -- nrc_from_to_star, the to
                                                -- field is treated as the from
                                                -- field, (the from field is
                                                -- the address) but we have to
                                                -- convert it to the right
                                                -- number of bits
  begin
    if (rst = '1') then
      if falling_edge(clk1) then -- when we currently check the reset
                                 -- pad
        --init microcode pc to boot-load
        ucode_pc <= boot_load_address;
        ncode_pc <= idle_ncode_pc;
        -- init bus & controls
        obus <= output_bus_init;
        my_pad_controls <= pad_controls_init;
        -- init outputs
        ncode_value <= ncode_word_init;
        ucode_value <= ucode_word_init; 
        reg1_controls <= register_controls_init;
        reg2_controls <= register_controls_init;
        reg3_controls <= register_controls_init;
      end if;
    else
      if ((pad_freeze = '0') AND (run_nano = '0')) then
        if rising_edge(clk1) then
          -- run microcontroller. Real work done by nanocontroller on clk2 rising.
          ucode_value <= get_ucode(ucode_pc); -- updates ucode_next, ucode_nano,
                                              -- ucode_from, ucode_to
          assert false report "updating ucode_pc (for next cycle) from: " & to_hstring(ucode_pc) &
            " to: " & to_hstring(get_ucode(ucode_pc).ucode_next) &
            " current value: " & to_string(get_ucode(ucode_pc)) severity note;
        elsif rising_edge(clk1a) then
          ucode_pc <= get_ucode(ucode_pc).ucode_next;
          ncode_pc <= get_ucode(ucode_pc).ucode_nano; 
        end if;
      end if;
      
      if ((pad_freeze = '0') AND rising_edge(clk2)) then
        --if (run_nano = '0') then
        --  current_ncode_pc := ucode_value.ucode_nano; -- for THIS
                                                      -- nano cycle
        --else
        --  current_ncode_pc := ncode_pc; -- for THIS nano cycle (what we
                                        -- updated last cycle)
        --end if;
        assert false report "ncode_pc: " & to_hstring(ncode_pc) severity note; 

        u_from := ucode_value.ucode_from;
        u_to := ucode_value.ucode_to;
        
        -- now run the nano cycle... this consists of using the from and to
        -- fields of the micro instruction if needed as register references,
        -- and setting the appropriate pad and register control bits

        -- following exec-nano-expression for now.
        ncode_value <= get_ncode(ncode_pc);
        current_ncode_pad_controls := get_ncode(ncode_pc).ncode_pad;
        current_ncode_register_controls := get_ncode(ncode_pc).ncode_register;

        -- pad and register controls are bit encoded
        my_pad_controls <= current_ncode_pad_controls;

        -- registers are a bit tricksy (Gollum!)
        -- for now we have to check by bit as exec-nano-expression does
        if (current_ncode_register_controls.nrc_from_star = '1') then -- from is
                                                                      -- the ucode
                                                                      -- from field
          set_from_anaphor_control(u_from,
                                   current_ncode_register_controls,
                                   reg1_controls, reg2_controls);
        end if;

        if (current_ncode_register_controls.nrc_to_star = '1') then
          set_to_anaphor_control(u_to,
                                 current_ncode_register_controls, 
                                 reg1_controls, reg2_controls, reg3_controls);
        end if;
        
        -- ok, this is mutex with the above two, but we don't currently need to
        -- save as much space as the original chip did.
        if (current_ncode_register_controls.nrc_from_to_star = '1') then
          -- temporary - we need to do this more generically (TBD)
          u_to_converted(7 downto 4) := x"0";
          u_to_converted(3 downto 0) := u_to;
          set_from_anaphor_control(u_to_converted,
                                   current_ncode_register_controls,
                                   reg1_controls, reg2_controls);
        end if;
        
        if (current_ncode_register_controls.nrc_from_star_type = '1') then
          set_from_anaphor_control(u_from,
                                   current_ncode_register_controls,
                                   reg1_controls, reg2_controls);
        end if;

        -- also mutex with other froms, etc.
        if (current_ncode_register_controls.nrc_from_const_star = '1') then
           -- u_from is currently only 8 bits
          obus.output_bus_data.mark_bit <= '0';
          obus.output_bus_data.not_pointer_bit <= '1'; -- its a constant so not pointer??
          obus.output_bus_data.type_rest <= "000000"; -- self-evaluating immediate
          obus.output_bus_data.displacement <= x"000";
          obus.output_bus_data.frame(11 downto 8) <= x"0";
          obus.output_bus_data.frame(7 downto 0) <= unsigned(u_from);
        else
          obus.output_bus_data <= s79_word_init; -- not sure we want to do this if
                                                 -- another arm chosen...
        end if;

        if (current_ncode_register_controls.nrc_from_type_const_star = '1') then
          -- ranges as used in regpkg, may want to make their own (type)
          -- constants...

          -- this really should also limit what we copy to only the type field
          -- too, but hopefully the microcode already does that in the current_ncode_register_controls.
          obus.output_bus_data.not_pointer_bit <= u_from(6);
          obus.output_bus_data.type_rest <= u_from(5 downto 0);
        end if;

        -- in exec-nano-expression, we would now go through the spec for each
        -- defined control-wire and set up the appropriate set and clear fo
        -- the register or field thereof (e.g. +rr-from-memtop+). Our test
        -- won't need this, but I suspect we want to simplify this as much as
        -- possible, since otherwise the register-controls field is quite
        -- wide; we really want to limit to the register_controls record we
        -- define in regpkg (which sets the field) and encodes the register
        -- being referred to separately. That would reduce the number of bits
        -- we need from (right now) from 35 down (not counting the *
        -- reference codes). It's possible the need for most of those codes
        -- were from a earlier revision of the simulator before the from* and
        -- to* anaphors were completely implemented... since reading the AIM
        -- they implemented that to reduce the number of explicit register
        -- references neede in the nanocode!

      elsif rising_edge(clk2a) then
        ncode_pc <= get_ncode(ncode_pc).ncode_next; -- for NEXT nano cycle
        
      elsif ((pad_freeze = '0') AND falling_edge(clk1)) then
        -- clean up nanocontrols. We can just set everything back to zero, I think
        reg1_controls <= register_controls_init;
        reg2_controls <= register_controls_init;
        reg3_controls <= register_controls_init;
        obus.bus_controls <= io_bus_controls_init;
        my_pad_controls <= pad_controls_init;
      end if;
      
    end if;
  end process MicroControl_Proc;
end architecture behav_pla_master;
