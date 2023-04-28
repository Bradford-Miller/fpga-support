-- ------------------------------------------------------------------------
--                                                                       --
-- Time-stamp: <2023-04-24 13:22:00 gorbag>      --
--                                                                       --
--  This is a driver for interfaces we hope to access from microblaze!   --
--                                                                       --
-- ------------------------------------------------------------------------

-- use pre 08 std for test
-- use std.env.all;
    
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity gpio_stuff is
  port (
    -- we can also tie these to LEDs...
    status1 : out std_logic;
    status2 : out std_logic;
    status3 : out std_logic;
    status4 : out std_logic;

    -- control lines: start the test (after the microblaze fills up the test 64
    -- words of 32 bit memory locations)
    c_start : in  std_logic;
    -- allows us to pause the test (after we wait a couple clocks for the
    -- current loop to finish) so we can check intermediate progress if needed
    c_wait  : in  std_logic);
end entity gpio_stuff;

architecture behav_gpio_1 of gpio_stuff is
  -- local signals
begin
  -- local behavior
end architecture behav_gpio_1;
