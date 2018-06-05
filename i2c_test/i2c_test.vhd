--------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;
--------------------------------------------------------------------------------
entity bin2bcd_12bit is
    Port ( binIN : in  STD_LOGIC_VECTOR (11 downto 0);
           ones : out  STD_LOGIC_VECTOR (3 downto 0);
           tens : out  STD_LOGIC_VECTOR (3 downto 0);
           hundreds : out  STD_LOGIC_VECTOR (3 downto 0);
           thousands : out  STD_LOGIC_VECTOR (3 downto 0)
          );
end bin2bcd_12bit;

architecture Behavioral of bin2bcd_12bit is

begin

bcd1: process(binIN)

  -- temporary variable
  variable temp : STD_LOGIC_VECTOR (11 downto 0);
  
  -- variable to store the output BCD number
  -- organized as follows
  -- thousands = bcd(15 downto 12)
  -- hundreds = bcd(11 downto 8)
  -- tens = bcd(7 downto 4)
  -- units = bcd(3 downto 0)
  variable bcd : UNSIGNED (15 downto 0) := (others => '0');

  -- by
  -- https://en.wikipedia.org/wiki/Double_dabble
  
  begin
    -- zero the bcd variable
    bcd := (others => '0');
    
    -- read input into temp variable
    temp(11 downto 0) := binIN;
    
    -- cycle 12 times as we have 12 input bits
    -- this could be optimized, we do not need to check and add 3 for the 
    -- first 3 iterations as the number can never be >4
    for i in 0 to 11 loop
    
      if bcd(3 downto 0) > 4 then 
        bcd(3 downto 0) := bcd(3 downto 0) + 3;
      end if;
      
      if bcd(7 downto 4) > 4 then 
        bcd(7 downto 4) := bcd(7 downto 4) + 3;
      end if;
    
      if bcd(11 downto 8) > 4 then  
        bcd(11 downto 8) := bcd(11 downto 8) + 3;
      end if;
    
      -- thousands can't be >4 for a 12-bit input number
      -- so don't need to do anything to upper 4 bits of bcd
    
      -- shift bcd left by 1 bit, copy MSB of temp into LSB of bcd
      bcd := bcd(14 downto 0) & temp(11);
    
      -- shift temp left by 1 bit
      temp := temp(10 downto 0) & '0';
    
    end loop;
 
    -- set outputs
    ones <= STD_LOGIC_VECTOR(bcd(3 downto 0));
    tens <= STD_LOGIC_VECTOR(bcd(7 downto 4));
    hundreds <= STD_LOGIC_VECTOR(bcd(11 downto 8));
    thousands <= STD_LOGIC_VECTOR(bcd(15 downto 12));
  
  end process bcd1;            
  
end architecture;
--------------------------------------------------------------------------------
--############################################################################--
--------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
--------------------------------------------------------------------------------
entity i2c_test is
port (
	clk, start_button, rst:			 	in std_logic;
   c_led, z_led:                    out std_logic;
	sda, scl: 								inout std_logic);
end entity;
--------------------------------------------------------------------------------
architecture moore_fsm of i2c_test is
	--I2C Element Declarations
	type i2c_element is (START, ONE, ZERO, S_ACK, M_ACK, RD, STOP);
	type i2c_message is array(natural range <>) of i2c_element;

	--FSM-related declarations:
	type state is (Waiting, HiA, HiB, LoA, LoB);
	signal pr_state, nx_state: state;

	--Timer-related delcarations:
	constant T1: natural := 500/4; --100kHz @ fclk=50MHz
	constant T2: natural	:= 4000; -- 10 microseconds @ fclk=50MHz
	constant tmax: natural := T2 + 1;
	signal t: natural range 0 to tmax;
	
	--Data and address
      constant setup: i2c_message(0 to 57) := (	START,
                                                ONE, ZERO, ONE, ZERO, ZERO, ONE, ZERO, -- 0x52
                                                ZERO, -- Write
                                                S_ACK,
                                                ONE, ONE, ONE, ONE, ZERO, ZERO, ZERO, ZERO, --0xF0
                                                S_ACK,
                                                ZERO, ONE, ZERO, ONE, ZERO, ONE, ZERO, ONE, --0x55
                                                S_ACK,
                                                STOP,
                                                
                                                START,
                                                ONE, ZERO, ONE, ZERO, ZERO, ONE, ZERO, -- 0x52
                                                ZERO, -- Write
                                                S_ACK,
                                                ONE, ONE, ONE, ONE, ONE, ZERO, ONE, ONE, --0xFB
                                                S_ACK,
                                                ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, --0x00
                                                S_ACK,
                                                STOP);
   constant receive: i2c_message(0 to 39) :=  ( START,
                                                ONE, ZERO, ONE, ZERO, ZERO, ONE, ZERO, -- 0x52
                                                ZERO, -- Write
                                                S_ACK,
                                                ZERO, ZERO, ZERO, ZERO, ZERO, ONE, ZERO, ONE, -- 0x05
                                                S_ACK,
                                                STOP,
                                                START,
                                                ONE, ZERO, ONE, ZERO, ZERO, ONE, ZERO, -- 0x52
                                                ONE, -- Read
                                                S_ACK,
                                                RD, RD, RD, RD, RD, RD, RD, RD,
                                                M_ACK,
                                                STOP);
	constant setup_length: natural := 58;
   constant receive_length: natural := 40;
	signal i: natural range 0 to setup_length;
   signal j: natural range 0 to receive_length;
   signal stickX: std_logic_vector(7 downto 0);
   
   --Display signals
   signal stickXOnes: std_logic_vector(3 downto 0);
   signal stickXTens: std_logic_vector(3 downto 0);
   signal stickXHuns: std_logic_vector(3 downto 0);
   signal stickXThou: std_logic_vector(3 downto 0);
   
   component bin2bcd_12bit is
    port ( binIN : in  STD_LOGIC_VECTOR (11 downto 0);
           ones : out  STD_LOGIC_VECTOR (3 downto 0);
           tens : out  STD_LOGIC_VECTOR (3 downto 0);
           hundreds : out  STD_LOGIC_VECTOR (3 downto 0);
           thousands : out  STD_LOGIC_VECTOR (3 downto 0)
          );
   end component bin2bcd_12bit;
   
begin

	--Clock and Reset
	process (clk, rst)
	begin
      if rst = '0' then
         t <= 0;
			i <= 0;
         j <= 0;
         pr_state <= Waiting;
		elsif rising_edge(clk) then
         if pr_state /= nx_state then
				t <= 0;
            if nx_state = LoB or nx_state = Waiting then
               if i < setup_length then
                  i <= i + 1;
               elsif j < receive_length - 1 then
                  j <= j + 1;
               else
                  j <= 0;
               end if;
            end if;
			elsif t /= tmax then
				t <= t + 1;
			end if;
         pr_state <= nx_state;
		end if;
	end process;

	--FSM combinational logic:
	process (all)
		variable internal_element: i2c_element;
		begin
		case pr_state is
			when Waiting =>
            scl <= '1';
				sda <= 'Z';
            if t < T2 - 1 then
               nx_state <= Waiting;
            else
               nx_state <= HiA;
            end if;
				
			when HiA =>
				scl <= '1';
            if i < setup_length then
               internal_element := setup(i);
            else
               internal_element := receive(j);
            end if;
				case internal_element is
					when START => 	sda <= 'Z';
					when ONE => 	sda <= 'Z';
					when ZERO => 	sda <= '0';
					when S_ACK => 	sda <= 'Z';
					when M_ACK => 	sda <= '0';
					when STOP => 	sda <= '0';
					when RD => 		sda <= 'Z';
					when others => sda <= 'Z';
				end case;
            if j >= 30 and j <= 37 then
               stickX(j - 30) <= sda;
            end if;
				if t >= T1-1 then
					nx_state <= HiB;
				else
					nx_state <= HiA;
				end if;
				
			when HiB =>
				scl <= '1';
            if i < setup_length then
               internal_element := setup(i);
            else
               internal_element := receive(j);
            end if;
				case internal_element is
					when START => 	sda <= '0';
					when ONE => 	sda <= 'Z';
					when ZERO => 	sda <= '0';
					when S_ACK => 	sda <= 'Z';
					when M_ACK => 	sda <= '0';
					when STOP => 	sda <= '0';
					when RD => 		sda <= 'Z';
					when others => sda <= 'Z';
				end case;
				if t >= T1-1 then
					if internal_element = STOP then
						nx_state <= Waiting;
					else
						nx_state <= LoA;
					end if;
				else
					nx_state <= HiB;
				end if;
				
			when LoA =>
				scl <= '0';
            if i < setup_length then
               internal_element := setup(i);
            else
               internal_element := receive(j);
            end if;
				case internal_element is
					when START => 	sda <= '0';
					when ONE => 	sda <= 'Z';
					when ZERO => 	sda <= '0';
					when S_ACK => 	sda <= 'Z';
					when M_ACK => 	sda <= '0';
					when STOP => 	sda <= 'Z';
					when RD => 		sda <= 'Z';
					when others => sda <= 'Z';
				end case;
				if t >= T1-1 then
					nx_state <= LoB;
				else
					nx_state <= LoA;
				end if;
				
			when LoB =>
				scl <= '0';
            if i < setup_length then
               internal_element := setup(i);
            else
               internal_element := receive(j);
            end if;
				case internal_element is
					when START => 	sda <= 'Z';
					when ONE => 	sda <= 'Z';
					when ZERO => 	sda <= '0';
					when S_ACK => 	sda <= 'Z';
					when M_ACK => 	sda <= '0';
					when STOP => 	sda <= '0';
					when RD => 		sda <= 'Z';
					when others => sda <= 'Z';
				end case;
				if t >= T1-1 then
					nx_state <= HiA;
				else
					nx_state <= LoB;
				end if;
            
		end case;
	end process;
   
   --Display
   z_led <= stickX(0);
   c_led <= stickX(1);
   
end architecture;
--------------------------------------------------------------------------------