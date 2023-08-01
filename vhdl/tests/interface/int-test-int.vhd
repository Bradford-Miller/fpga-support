-- ------------------------------------------------------------------------
--                                                                       --
-- Time-stamp: <2023-08-01 12:35:50 gorbag>                              --
--                                                                       --
--  This is a driver for interfaces we hope to access from microblaze!   --
--                                                                       --
--  This version uses INTERNALLY defined BRAM                            --
--                                                                       --
-- ------------------------------------------------------------------------
--
-- 7/31/23 As we did for int-test-ext, make a state machine (copy code and
-- modify as needed).

-- use pre 08 std for test
-- use std.env.all;
    
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.my_gpio_interface.all;

-- library work;
-- use work.regpkg.all; -- temporary register types


entity int_test_int is
  port (
    clkb   : in  std_logic;
    t_status : out gpio_outputs;
    t_controls : in  gpio_inputs;

    reset  : in std_logic;

    -- bram interface (at least what we're using)
    clka     : in std_logic;
    ena      : in std_logic;
    wea      : in std_logic;
    addra    : in std_logic_vector(5 downto 0);
    dia      : in std_logic_vector(31 downto 0);
    doa      : out  std_logic_vector(31 downto 0));
end entity int_test_int;
    
architecture behav_int_test_int_1 of int_test_int is
  component bram_io_int
    port (
      clkb       : in  std_logic; 
      read_rq    : in  std_logic; -- please read
      write_rq   : in  std_logic; -- please write
      read_addr  : in  std_logic_vector(5 downto 0); -- need 6 bits to address 64 locations
      write_addr : in  std_logic_vector(5 downto 0); -- need 6 bits to address 64 locations
      read_data  : out std_logic_vector(31 downto 0);
      write_data : in  std_logic_vector(31 downto 0);
      busy       : out std_logic; -- let parent know we're busy handling the request

      -- outputs for the (external) bram interface, assume it's 64x32 (32 bits wide, 64 bits deep)
      clka  : in  std_logic; 
      ena   : in  std_logic; -- enable
      wea   : in  std_logic; --
      addra : in  std_logic_vector(5 downto 0); -- need 6 bits to address 64 locations
      dia   : in  std_logic_vector(31 downto 0); -- input to the bram, 32 bits wide
      doa   : out std_logic_vector(31 downto 0)); -- output from the bram, 32 bits wide
  end component bram_io_int;

  signal read_rq    : std_logic := '0';
  signal write_rq   : std_logic := '0';
  signal read_addr  : std_logic_vector(5 downto 0) := "000000";
  signal write_addr : std_logic_vector(5 downto 0) := "000000";
  signal read_data  : std_logic_vector(31 downto 0) := X"00000000";
  signal write_data : std_logic_vector(31 downto 0) := X"00000000";
  signal busy       : std_logic;
  signal tick       : unsigned(11 downto 0) := x"000"; -- used for debug messages
  
  -- test loop track states
  type state_labels is (  Idle,
                          Started,
                          Rd_Rq,
                          Rd_Proc,
                          Wr_Rq,
                          Wr_Proc,
                          Hold,
                          Done
                       );
  signal current_state : state_labels;

begin
  My_Bram : bram_io_int
    port map (
      clkb => clkb,
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

  Int_test: process(clkb)
  begin
    if rising_edge(clkb) then
      tick <= tick + 1;

      if reset = '1' then
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

        read_addr <= "000000"; -- we could use a loop iterator for this, of course
        write_addr <= "000000";
        t_status.status1 <= '0';
        t_status.status2 <= '0';
        t_status.status3 <= '0';
        t_status.status4 <= '0';
        current_state <= Idle;
      else
        case (current_state) is
          when Idle =>
            if t_controls.c_start = '1' then
              report "int_test: c_start! tick: " &
                integer'image(to_integer(unsigned(tick)));
              current_state <= Started;
            end if;

          when Started =>
            -- read
            read_rq <= '1';
            current_state <= Rd_Rq;

          when Rd_Rq =>
            if busy = '1' then
              read_rq <= '0';
              current_state <= Rd_Proc;
              report "int_test: waiting until bram not busy. tick: " &
                integer'image(to_integer(unsigned(tick)));
            end if;

          when Rd_Proc =>
            if busy = '0' then
              -- get read result
              report "int_test: reading data from bram: "
                & integer'image(to_integer(unsigned(read_addr)))
                & "->"
                & integer'image(to_integer(unsigned(read_data)))
                & ", doing left shift";
              write_data <= std_logic_vector(shift_left(unsigned(read_data), 1)); -- change it in a way we can detect
              write_rq <= '1'; -- already have the address setup
              current_state <= Wr_Rq;
            end if;

          when Wr_Rq =>
            if busy = '1' then
              report "int_test: writing bram, waiting busy. tick: "
                & integer'image(to_integer(unsigned(tick)));
              write_rq <= '0';
              current_state <= Wr_Proc;
            end if;

          when Wr_Proc =>
            if busy = '0' then
              -- increment the address, check that we haven't been told to wait
              report "int_test: incrementing addresses (read_addr: "
                & integer'image(to_integer(unsigned(read_addr)))
                & "), go loop. tick: "  & integer'image(to_integer(unsigned(tick)));
              read_addr <= std_logic_vector(unsigned(read_addr) + 1);
              write_addr <= std_logic_vector(unsigned(write_addr) + 1);

              if (read_addr = "111111") then
                t_status.status1 <= '1'; -- indicate we're done for now
                current_state <= Done;
              elsif t_controls.c_wait = '1' then
                current_state <= Hold;
              else
                current_state <= Started;
              end if;
            end if;
            
          when Hold =>
            if t_controls.c_wait = '0' then
              current_state <= Started;
            end if;
            
          when Done =>
            if t_controls.c_start = '0' then
              report "int_test: prepping for next invoke. Tick: "
                & integer'image(to_integer(unsigned(tick)));
              current_state <= Idle;
            end if;
        end case;
      end if;
    end if;
  end process Int_test;
end architecture behav_int_test_int_1;
