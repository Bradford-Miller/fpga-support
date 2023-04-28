-- ------------------------------------------------------------------------
--                                                                       --
-- Time-stamp: <2023-04-28 14:24:40 gorbag>                              --
--                                                                       --
--  This is a driver for INternally defined BRAM                         --
--                                                                       --
-- ------------------------------------------------------------------------

-- 2008
-- use std.env.all;
    
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- ok contra bram-ext, this version defines the BRAM locally and allows the
-- external IP to connect to it instead. 

entity bram_io_int is
  port (
    -- from parent (channel B)
    clkb       : in  std_logic; 
    read_rq    : in  std_logic; -- please read
    write_rq   : in  std_logic; -- please write
    read_addr  : in  std_logic_vector(5 downto 0); -- need 6 bits to address 64 locations
    write_addr : in  std_logic_vector(5 downto 0); -- need 6 bits to address 64 locations
    read_data  : out std_logic_vector(31 downto 0);
    write_data : in  std_logic_vector(31 downto 0);
    busy       : out std_logic; -- let parent know we're busy handling the request

    --  assume it's 64x32 (32 bits wide, 64 bits deep),
    -- we can rewrite this to use generics for that later

    -- for external interface
    
    clka  : in  std_logic; 
    ena   : in  std_logic; -- enable
    wea   : in  std_logic; --
    addra : in  std_logic_vector(5 downto 0); -- need 6 bits to address 64 locations
    dia   : in  std_logic_vector(31 downto 0); -- input to the bram, 32 bits wide
    doa   : out std_logic_vector(31 downto 0)); -- output from the bram, 32 bits wide
end entity bram_io_int;

architecture behav_bram_io_2 of bram_io_int is
  type ram_type is array (63 downto 0) of std_logic_vector(31 downto 0);
  shared variable RAM : ram_type;

  signal enb : std_logic := '0'; -- enable
  signal web : std_logic := '0'; --
  signal addrb : std_logic_vector(5 downto 0) := "000000"; -- need 6 bits to address 64 locations
  signal dib : std_logic_vector(31 downto 0) := X"00000000"; -- input to the bram, 32 bits wide
  signal dob : std_logic_vector(31 downto 0); -- output from the bram, 32 bits wide
  
begin
  -- parent should present a read_addr and read_rq and will get back a
  --                       read_data the following clock(?)
  --        or present a write_rq, write_addr and write_data (all on same clock)
  --                       and write will happen on following clock(?)

  -- per the timeing diagrams in PG058, e.g. 3-9..3-11, addr and en should be present
  -- during the rising clock, and then dout will be available on the following
  -- rising clock (assuming no output registration which we shouldn't need).
  -- Similarly, addr, we, and din should be presented on the rising clock and
  -- dout will then be the appropriate value (depending on write-first,
  -- read-first or no-change mode) on the following rising clock.

  -- (Not that it makes a difference at this point, but the scheme79 timing is
  -- different, so we'll need a custom version of this code to deal with the
  -- data strobe on write coming after the address strobe as addr, din, and
  -- dout use shared pins in that model).

  -- port B (external port):
  process(CLKB)
  begin
    if CLKB'event and CLKB = '1' then
      -- report "bram-int: performing clkb cycle";
      if ENB = '1' then
        DOB <= RAM(to_integer(unsigned(ADDRB)));
        report "bram-int: performing read to dob";
        if WEB = '1' then
          RAM(to_integer(unsigned(ADDRB))) := DIB;
          report "bram-int: performing write to dob";
        end if;
      end if;
    end if;
  end process;

  -- port A (external port):
  process(CLKA)
  begin
    if CLKA'event and CLKA = '1' then
      -- report "bram-int: performing clka cycle";
      if ENA = '1' then
        doa <= RAM(to_integer(unsigned(addra)));
        report "bram-int: performing read to doa";
        if WEA = '1' then
          RAM(to_integer(unsigned(addra))) := dia;
          report "bram-int: performing write to doa";
        end if;
      end if;
    end if;
  end process;

  Bram_iop: process
  begin
    busy <= '0';
    read_data <= X"00000000";
    loop
      wait until falling_edge(clkb);
      if (read_rq = '1') then
        report "bram-int: performing read_rq";
        enb <= '1';
        addrb <= read_addr;
        busy <= '1';
        wait until falling_edge(clkb);
        -- bram should process this cycle
        -- wait until falling_edge(clkb);
        -- output should be ready
        read_data <= dob;
        busy <= '0';
        enb <= '0';
      elsif (write_rq = '1') then
        report "bram-int: performing write_rq";
        enb <= '1';
        web <= '1';
        addrb <= write_addr;
        busy <= '1';
        dib <= write_data;
        wait until falling_edge(clkb);
        -- wait until falling_edge(clkb);
        read_data <= dob;
        busy <= '0';
        enb <= '0';
        web <= '0';
      end if;
    end loop;
  end process Bram_iop;

end architecture behav_bram_io_2;
