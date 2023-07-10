-- ------------------------------------------------------------------------
--                                                                       --
-- Time-stamp: <2023-06-27 12:52:37 gorbag>                              --
--                                                                       --
-- This is a top-level for interfaces we hope to access from microblaze! --
--                                                                       --
-- This version uses EXTERNALLY defined BRAM                             --
--                                                                       --
-- ------------------------------------------------------------------------

-- use std.env.all;
    
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Vivado doesn't seem to like "top-level" RTL packages using VHDL 2008. So
-- this wrapper doesn't do much, but surrounds the actual int-test-ext.vhd to
-- allow it to be VHDL 2008 while this is the default VHDL version.

entity int_test_wrapper_1 is
  port (
    clkin   : in  std_logic;
    status1 : out std_logic;
    status2 : out std_logic;
    status3 : out std_logic;
    status4 : out std_logic;
    c_start : in  std_logic;
    c_wait  : in  std_logic;

    -- external bram interface (at least what we're using)
    clka     : out std_logic;
    ena      : out std_logic;
    wea      : out std_logic;
    addra    : out std_logic_vector(5 downto 0);
    dia      : out std_logic_vector(31 downto 0);
    doa      : in  std_logic_vector(31 downto 0));
end entity int_test_wrapper_1;

architecture behav_int_test_wrapper_1 of int_test_wrapper_1 is
  component int_test_ext
    port (
      clkin   : in  std_logic;
      status1 : out std_logic;
      status2 : out std_logic;
      status3 : out std_logic;
      status4 : out std_logic;
      c_start : in  std_logic;
      c_wait  : in  std_logic;

      -- external bram interface (at least what we're using)
      clka     : out std_logic;
      ena      : out std_logic;
      wea      : out std_logic;
      addra    : out std_logic_vector(5 downto 0);
      dia      : out std_logic_vector(31 downto 0);
      doa      : in  std_logic_vector(31 downto 0));
  end component int_test_ext;

  signal toptick : unsigned(31 downto 0);

begin
  My_Int_test: int_test_ext
    port map (
      clkin => clkin,
      status1 => status1,
      status2 => status2,
      status3 => status3,
      status4 => status4,
      c_start => c_start,
      c_wait => c_wait,
      
      clka => clka,
      ena => ena,
      wea => wea,
      addra => addra,
      dia => dia,
      doa => doa);
  
  Tick_count_0: process (clkin)
  begin
    if rising_edge(clkin) then
      toptick <= toptick + 1;
      end if;
  end process Tick_count_0;
  
end architecture behav_int_test_wrapper_1;
