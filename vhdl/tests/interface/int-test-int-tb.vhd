
-- ------------------------------------------------------------------------
--                                                                       --
-- Time-stamp: <2023-08-01 12:38:45 gorbag>                              --
--                                                                       --
--  This is a testbed for the  driver interfaces (Bram, GPIO)            --
--                                                                       --
--  This version uses INTERNALLY defined BRAM                            --
--                                                                       --
-- ------------------------------------------------------------------------

-- use pre 08 std for test
-- use std.env.all;

    
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity int_test_tb_1 is
end entity int_test_tb_1;

use work.my_gpio_interface.all;

architecture behav_int_test_tb_1 of int_test_tb_1 is
  component int_test_int
    port (
      clkb    : in  std_logic;
      t_status : out gpio_outputs;
      t_controls : in gpio_inputs;

      reset : in std_logic;

      -- bram interface (at least what we're using)
      clka     : in std_logic;
      ena      : in std_logic;
      wea      : in std_logic;
      addra    : in std_logic_vector(5 downto 0);
      dia      : in std_logic_vector(31 downto 0);
      doa      : out std_logic_vector(31 downto 0));
  end component int_test_int;

  -- DUT inputs
  signal clkb : std_logic := '0';
  signal clka : std_logic := '0';
  signal t_controls : gpio_inputs := (others => '0');
  signal ena : std_logic := '0';
  signal wea : std_logic := '0';
  signal addra : std_logic_vector(5 downto 0) := "000000";
  signal dia : std_logic_vector(31 downto 0) := X"00000000";

  -- DUT outputs
  signal t_status : gpio_outputs;
  signal doa : std_logic_vector(31 downto 0);

  constant CLOCK_PERIOD : time := 500 ps;
  signal clk : std_logic := '0';
  signal tick : unsigned(11 downto 0) := X"000"; -- mainly so we can keep track
                                                 -- of where we are in the test!
  signal rst : std_logic := '0';
begin
  DUT : int_test_int
    port map (
      clkb => clkb,
      t_status => t_status,
      t_controls => t_controls,

      reset => rst,
      
      clka => clka,
      ena => ena,
      wea => wea,
      addra => addra,
      dia => dia,
      doa => doa);

  ------------------
  -- clock generator
  ------------------
  clk <= '0' after CLOCK_PERIOD when clk = '1' else
         '1' after CLOCK_PERIOD;

  Clock_Gen : process (clk)
  begin
    if rising_edge(clk) then
      tick <= tick + 1;
      -- for now, run both clocks at the same rate. (We can try something
      -- different later)
      clka <= '1';
      clkb <= '1';
    elsif falling_edge(clk) then
      clka <= '0';
      clkb <= '0';
    end if;   
  end process Clock_Gen;

  ------------------
  -- reset generator
  ------------------

  -- unlike most of our other tests, we don't actually need a rst signal, but
  -- it's good to allow things to settle before we start so I kept this.
  
  Reset_Gen : process 
  begin
    for i in 1 to 5 loop
      wait on clk;
      if (i < 4) then
        rst <= '1';
      else
        rst <= '0';
      end if;
    end loop;
    -- deactivate this process
    wait on rst;
  end process Reset_Gen;

  -------
  -- Test
  -------

  -- 1) Fill our memory with some values
  -- 2) start the subprocess by turning on c_start
  -- 3) while running (after some number of cycles) turn on c_wait so we can
  --    check (in the wave file) that we waited
  -- 4) let run to completion, turn off c_start
  -- 5) dump out the (hopefully new) contents of memory for visual check (could
  --    automate)

  Interface_Test : process
    variable b : integer := 0;
  begin
    report "waiting for rst signal";
    wait until (rst = '1'); -- startup
    report "waiting until rst signal falls";
    wait until falling_edge(rst); -- tick 2 center (clk falling edge)

    -- access the dual port memory via the 'a' ports...
    report "writing initial problem to memory";
    for a in 0 to 63 loop -- address
      addra <= std_logic_vector(to_unsigned(a, addra'length));
      dia <= std_logic_vector(to_unsigned(a + 1, dia'length)); -- increment so we have at least one '1'
                                      -- bit
      wea <= '1';
      ena <= '1';
      -- memory update happens on rising edge
      wait until rising_edge(clka);
      report "TICK " & integer'image(to_integer(unsigned(tick)));
      -- wait until rising_edge(clka); -- temp
      -- should get written, update address and write data
    end loop;
    -- tick x42
    wea <= '0';
    -- ena <= '0';

    -- dump memory, for now to the console
    -- ena <= '1';
    report "dumping memory pre-test";
    b := 0;
    for a in 0 to 63 loop
      addra <= std_logic_vector(to_unsigned(a, addra'length));
      wait until rising_edge(clka); -- read happen on rising_edge so we should see it in doa
                                    -- on Following rising edge, but we can
                                    -- update address first.
      if (a > 0) then
        report "a = " & integer'image(b) & " *a = " & integer'image(to_integer(unsigned(doa)));
        b := a; -- for next loop
      end if;
    end loop;
    -- print the last one
    -- wait an extra clock
    wait until rising_edge(clka); 
    report "a = " & integer'image(b) & " *a = " & integer'image(to_integer(unsigned(doa)));
    ena <= '0';
    
    -- start circuit
    report "TB: setting c_start at tick " & integer'image(to_integer(unsigned(tick)));
    t_controls.c_start <= '1';
    
    -- int-test-int takes a clock to read and then another clock to write
    -- (maybe even two each because it uses the busy signal to check). Anyway,
    -- we want to wait for some number of clocks less than the full 128/256 needed
    -- to update all memory so we can check that c_wait is working.
    wait until tick = 200;
    report "TB: injecting wait at tick " & integer'image(to_integer(unsigned(tick)));
    t_controls.c_wait <= '1';
    -- wait a few ticks so we can check it on the scope
    wait until tick = 220;
    report "TB: clearing wait, waiting until completed at tick " & integer'image(to_integer(unsigned(tick)));
    t_controls.c_wait <= '0';
    -- now run to completion
    wait until t_status.status1 = '1';
    t_controls.c_start <= '0';

    -- dump memory, for now to the console
    report "TB: done, dumping memory post-test";
    ena <= '1';
    b := 0;
    for a in 0 to 63 loop
      addra <= std_logic_vector(to_unsigned(a, addra'length));
      wait until rising_edge(clka);
      if (a > 0) then
        report "a = " & integer'image(b) & " *a = " & integer'image(to_integer(unsigned(doa)));
        b := a;
      end if;
    end loop;
    wait until rising_edge(clka);
    report "a = " & integer'image(b) & " *a = " & integer'image(to_integer(unsigned(doa)));
    ena <= '0';
    
  end process Interface_Test;
end architecture behav_int_test_tb_1;
