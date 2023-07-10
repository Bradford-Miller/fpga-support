-- ------------------------------------------------------------------------
--                                                                       --
-- Time-stamp: <2023-04-28 14:39:40 gorbag>                              --
--                                                                       --
--  This is a driver for externally defined BRAM                         --
--                                                                       --
-- ------------------------------------------------------------------------

-- use std.env.all;
    
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- following PG058 (August 6, 2021): we assume the BRAM is build by the BMG
-- (v8.4) and we interface to that rather than have our own VHDL get recognized
-- as BRAM. Not sure which is better, but this should make it easier(?) to
-- interface to other IP since the true-dual port should have a port available
-- for, e.g., an AXI4 interface.

entity bram_io_ext is
  port (
    -- from parent
    clkin      : in  std_logic; 
    read_rq    : in  std_logic; -- please read
    write_rq   : in  std_logic; -- please write
    read_addr  : in  unsigned(5 downto 0); -- need 6 bits to address 64 locations
    write_addr : in  unsigned(5 downto 0); -- need 6 bits to address 64 locations
    read_data  : out unsigned(31 downto 0);
    write_data : in  unsigned(31 downto 0);
    busy       : out std_logic; -- let parent know we're busy handling the request
    
    -- outputs to the bram IP, assume it's 64x32 (32 bits wide, 64 bits deep),
    -- we can rewrite this to use generics for that later
    clka  : out std_logic; 
    ena   : out std_logic; -- enable
    wea   : out std_logic; --
    addra : out unsigned(5 downto 0); -- need 6 bits to address 64 locations
    dia   : out unsigned(31 downto 0); -- input to the bram, 32 bits wide
    doa   : in  unsigned(31 downto 0)); -- output from the bram, 32 bits wide
end entity bram_io_ext;

architecture behav_bram_io_1 of bram_io_ext is
  -- local signals
  signal reading : std_logic := '0';
  signal writing : std_logic := '0';

begin
  Echo_clk: process (clkin)
  begin
    clka <= clkin;
  end process Echo_clk;

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

  Bram_iop: process(clkin)
  begin
    busy <= '0';
    read_data <= X"00000000";
    if rising_edge(clkin) then
      if (read_rq = '1') then
        ena <= '1';
        addra <= read_addr;
        busy <= '1';
        reading <= '1';
      elsif (write_rq = '1') then
        ena <= '1';
        wea <= '1';
        addra <= write_addr;
        busy <= '1';
        dia <= write_data;
      end if;
    end if;

    if falling_edge (clkin) then
      -- obviously could be simplified, but this is test code so...
      if (reading = '1') then
        -- bram should process this cycle
        -- output should be ready
        read_data <= doa;
        busy <= '0';
        ena <= '0';
        reading <= '0';
      elsif (writing = '1') then
        read_data <= doa;
        busy <= '0';
        ena <= '0';
        wea <= '0';
        writing <= '0';
      end if;
    end if;
  end process Bram_iop;
end architecture behav_bram_io_1;
