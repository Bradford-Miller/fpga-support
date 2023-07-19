-- ------------------------------------------------------------------------
--                                                                       --
-- Time-stamp: <2023-07-13 14:17:51 gorbag>      --
--                                                                       --
--  This is a driver for interfaces we hope to access from microblaze!   --
--                                                                       --
-- ------------------------------------------------------------------------

-- use pre 08 std for test
-- use std.env.all;
    
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package my_gpio_interface is
  type gpio_outputs is record
    -- we can also tie these to LEDs...
    status1 : std_logic;
    status2 : std_logic;
    status3 : std_logic;
    status4 : std_logic;
  end record gpio_outputs;
  
  type gpio_inputs is record
    c_start : std_logic;
    c_wait  : std_logic;
  end record gpio_inputs;
  
end package my_gpio_interface;

package body my_gpio_interface is
end package body my_gpio_interface;
