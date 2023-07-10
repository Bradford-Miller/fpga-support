-- ------------------------------------------------------------------------
--                                                                       --
-- Time-stamp: <2023-04-28 16:37:13 gorbag>                              --
--                                                                       --
--  This is a driver for interfaces we hope to access from microblaze!   --
--                                                                       --
-- This version uses EXTERNALLY defined BRAM                             --
--                                                                       --
-- ------------------------------------------------------------------------

use std.env.all;
    
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- library work;
-- use work.regpkg.all; -- temporary register types

entity int_test_ext is
  port (
    clkin   : in  std_logic;
    status1 : out std_logic;
    status2 : out std_logic;
    status3 : out std_logic;
    status4 : out std_logic;
    c_start : in  std_logic;
    c_wait  : in  std_logic;

    -- bram interface (at least what we're using)
    clka     : out std_logic;
    ena      : out std_logic;
    wea      : out std_logic;
    addra    : out std_logic_vector(5 downto 0);
    dia      : out std_logic_vector(31 downto 0);
    doa      : in  std_logic_vector(31 downto 0));
end entity int_test_ext;
    
architecture behav_int_test_1 of int_test_ext is
  component bram_io_ext
    port (
      clkin      : in  std_logic; 
      read_rq    : in  std_logic; -- please read
      write_rq   : in  std_logic; -- please write
      read_addr  : in  std_logic_vector(5 downto 0); -- need 6 bits to address 64 locations
      write_addr : in  std_logic_vector(5 downto 0); -- need 6 bits to address 64 locations
      read_data  : out std_logic_vector(31 downto 0);
      write_data : in  std_logic_vector(31 downto 0);
      busy       : out std_logic; -- let parent know we're busy handling the request

      -- outputs to the (external) bram IP, assume it's 64x32 (32 bits wide, 64 bits deep)
      clka  : out std_logic; 
      ena   : out std_logic; -- enable
      wea   : out std_logic; -- enable write
      addra : out std_logic_vector(5 downto 0); -- need 6 bits to address 64 locations
      dia   : out std_logic_vector(31 downto 0); -- input to the bram, 32 bits wide
      doa   : in  std_logic_vector(31 downto 0)); -- output from the bram, 32 bits wide
  end component bram_io_ext;

  component gpio_stuff
    port (
      status1 : out std_logic;
      status2 : out std_logic;
      status3 : out std_logic;
      status4 : out std_logic;
      
      c_start : in  std_logic;
      c_wait  : in  std_logic);
  end component gpio_stuff;

  signal read_rq    : std_logic := '0';
  signal write_rq   : std_logic := '0';
  signal read_addr  : std_logic_vector(5 downto 0) := "000000";
  signal write_addr : std_logic_vector(5 downto 0) := "000000";
  signal read_data  : std_logic_vector(31 downto 0) := X"00000000";
  signal write_data : std_logic_vector(31 downto 0) := X"00000000";
  signal busy       : std_logic;
  signal tick       : unsigned(11 downto 0) := x"000";
  
begin
  My_Bram : bram_io_ext
    port map (
      clkin => clkin,
      read_rq => read_rq,
      write_rq => write_rq,
      read_addr => read_addr,
      write_addr => write_addr,
      read_data => read_data,
      write_data => write_data,
      busy => busy,
      
      clka => clka,
      ena => ena,
      wea => wea,
      addra => addra,
      dia => dia,
      doa => doa);

  My_GPIO : gpio_stuff
    port map (
      status1 => status1,
      status2 => status2,
      status3 => status3,
      status4 => status4,
      c_start => c_start,
      c_wait => c_wait);

  Tick_count: process(clkin)
  begin
    if rising_edge(clkin) then
      tick <= tick + 1;
    end if;   
  end process Tick_count;
    
  Int_test: process
  begin
    -- for the purposes of this test, we wait for c_start, then step through
    -- memory performing a rotation (so we can easily detect that we did
    -- something) e.g. "1000" -> "0001" and "0001" -> "0010" etc.
    -- we change the state of status1 whenever we encounter "1111" on read, and
    -- we turn on status2 while we are not idle. We thus turn off status2 when
    -- we are either waiting or we are done.

    -- if c_wait is asserted, we temporarily become idle until it is cleared
    -- then go on. When c_start is asserted we start processing, but we won't
    -- restart processing until it cycles (i.e. goes off and comes back on). 

    -- obviously this isn't how we would normally use the BRAM, it's just a
    -- test to make sure we can communicate with an external BRAM which is also
    -- accessible to the microblaze, allowing us to mix IP with our VHDL code!

    L0: loop
      report "int_test: waiting for c_start";

      wait until (c_start = '1');
      read_addr <= "000000"; -- we could use a loop iterator for this, of course
      write_addr <= "000000";
      status1 <= '0';
      status2 <= '0';
      status3 <= '0';     
      status4 <= '0';

      wait until (rising_edge(clkin));

      report "int_test: c_start! tick: " & integer'image(to_integer(unsigned(tick)));
      wait until (falling_edge(clkin)); -- next clock  -- clumsy, should just
                                        -- implement signals to indicate what
                                        -- stage we're in...

      L1: loop
        report "int_test: waiting until bram not busy. tick: " & integer'image(to_integer(unsigned(tick)));
        wait until ((busy = '0') AND rising_edge(clkin));
        read_rq <= '1';
        -- wait for memory to cycle
        wait until (busy = '1');
        read_rq <= '0';
        wait until ((busy = '0') AND rising_edge(clkin)); -- read_data should now be valid

        report "int_test: reading data from bram: "
          & integer'image(to_integer(unsigned(read_addr)))
          & "->"
          & integer'image(to_integer(unsigned(read_data)))
          & ", doing left shift";
        write_data <= std_logic_vector(shift_left(unsigned(read_data), 1)); -- change it in a way we can detect
        write_rq <= '1'; -- already have the address setup
        -- wait for memory to cycle
        report "int_test: writing bram, waiting busy. tick: " & integer'image(to_integer(unsigned(tick)));
        wait until ((busy = '1') AND rising_edge(clkin));
        write_rq <= '0';

        -- increment the address, check that we haven't been told to wait
        report "int_test: incrementing addresses (read_addr: "
          & integer'image(to_integer(unsigned(read_addr)))
          & "), go loop. tick: "  & integer'image(to_integer(unsigned(tick)));
        read_addr <= std_logic_vector(unsigned(read_addr) + 1);
        write_addr <= std_logic_vector(unsigned(write_addr) + 1);

        if (read_addr = "111111") then
          status1 <= '1'; -- indicate we're done for now
          exit L1;
        end if;

        -- check to see if we've been paused
        if (c_wait = '1') then
          -- we can't just wait until it's '0' as if it's already '0' we'll
          -- wait until it's non-zero first, apparently!
          wait until (c_wait = '0');
        end if;
      end loop L1;
      
      wait until (c_start = '0'); -- this won't catch turning it off then on while the
                                  -- test is running, but we're not testing that.
      report "int_test: prepping for next invoke. Tick: " & integer'image(to_integer(unsigned(tick)));
    end loop L0;
    -- should never reach here, we just wait for another cycle.
    wait on c_start;
    -- finish; -- new in std08
  end process Int_test;
end architecture behav_int_test_1;
