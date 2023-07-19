-- ------------------------------------------------------------------------
--                                                                       --
-- Time-stamp: <2023-07-13 14:17:31 gorbag>                              --
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

use work.my_gpio_interface.all;

-- Vivado doesn't seem to like "top-level" RTL packages using VHDL 2008. So
-- this wrapper doesn't do much, but surrounds the actual int-test-ext.vhd to
-- allow it to be VHDL 2008 while this is the default VHDL version.

entity int_test_wrapper_1 is
  port (
    clkin   : in  std_logic;
    -- t_status : out gpio_outputs; -- vivado doesn't support anything other
    -- than std_logic or std_logic_vector at toplevel of RTL
    status1 : out std_logic;
    status2 : out std_logic;
    status3 : out std_logic;
    status4 : out std_logic;
    -- t_controls : in gpio_inputs;
    c_start : in  std_logic;
    c_wait  : in  std_logic;
    
    reset   : in  std_logic;

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
      t_status : out gpio_outputs; 
      t_controls : in gpio_inputs := (others => '0');

      reset   : in  std_logic;

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
      t_status.status1 => status1,
      t_status.status2 => status2,
      t_status.status3 => status3,
      t_status.status4 => status4,
      t_controls.c_start => c_start,
      t_controls.c_wait => c_wait,

      reset  => reset,
      
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
