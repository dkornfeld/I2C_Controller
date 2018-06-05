--------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
--------------------------------------------------------------------------------
entity i2c_test is
port (
	clk, start_button, rst:			 	in std_logic;
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
	constant setup_length: natural := setup'length;
	signal i: natural range 0 to setup_length - 1;

begin
	--Timer
	process (clk, rst)
	begin
		if rst='0' then -- Active low reset
			t <= 0;
			i <= 0;
		elsif rising_edge(clk) then
			if pr_state /= nx_state then
				t <= 0;
				if nx_state = LoB or nx_state = Waiting then
					i <= i+1;
				end if;
			elsif t /= tmax then
				t <= t + 1;
			end if;
		end if;
	end process;

	--FSM state register:
	process (clk, rst)
	begin
		if rst='0' then -- Active low reset
			pr_state <= Waiting;
		elsif rising_edge(clk) then
			pr_state <= nx_state;
		end if;
	end process;

	--FSM combinational logic:
	process (all)
		--variable i: natural range 0 to setup_length - 1;
		begin
		case pr_state is
			when Waiting =>
				scl <= '1';
				sda <= 'Z';
				if t >= T2-1 and start_button = '0' then
					nx_state <= HiA;
				else
					nx_state <= Waiting;
				end if;
				
			when HiA =>
				scl <= '1';
				case setup(i) is
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
					nx_state <= HiB;
				else
					nx_state <= HiA;
				end if;
				
			when HiB =>
				scl <= '1';
				case setup(i) is
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
					if setup(i) = STOP then
						nx_state <= Waiting;
					else
						nx_state <= LoA;
					end if;
				else
					nx_state <= HiB;
				end if;
				
			when LoA =>
				scl <= '0';
				case setup(i) is
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
				case setup(i) is
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
end architecture;
--------------------------------------------------------------------------------