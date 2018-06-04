--------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
--------------------------------------------------------------------------------
entity i2c_test is
port (
	clk, start_button, rst:			 	in std_logic;
	sda, scl: 								out std_logic);
end entity;
--------------------------------------------------------------------------------
architecture moore_fsm of i2c_test is
	--FSM-related declarations:
	type state is (idle, start_A, start_B, SCL_high_A, SCL_high_B, SCL_low_A, SCL_low_B);
	signal pr_state, nx_state: state;

	--Timer-related delcarations:
	constant T1: natural := 500; --100kHz @ fclk=50MHz
	constant tmax: natural := 500;
	signal t: natural range 0 to tmax;
	
	--Data and address
	constant data: std_logic_vector(0 to 7) := "10101010";
	constant address: std_logic_vector(0 to 6) := "1101101";

begin
	--Timer (using strategy #1):
	process (clk, rst)
	begin
		if rst='0' then -- Active low reset
			t <= 0;
		elsif rising_edge(clk) then
			if pr_state /= nx_state then
				t <= 0;
			elsif t /= tmax then
				t <= t + 1;
			end if;
		end if;
	end process;

	--FSM state register:
	process (clk, rst)
	begin
		if rst='0' then -- Active low reset
			pr_state <= idle;
		elsif rising_edge(clk) then
			pr_state <= nx_state;
		end if;
	end process;

	--FSM combinational logic:
	process (all)
		variable i: natural range 0 to 7;
		begin
		case pr_state is
			-- All state logic follows same template:
			-- when STATE
			--		ssd <= proper SSD for this state
			--		if t >= state timeout then
			--			if going clockwise then
			--				go down the list of states
			--			else (counterclockwise)
			--				go up the list of states
			--		else (not enough time passed)
			--			stay in current state
			when idle =>
				scl <= '1';
				sda <= '1';
				if t >= T1-1 then
					if start_button='0' then
						nx_state <= start_A;
					end if;
				else
					nx_state <= idle;
				end if;
				
			when start_A =>
				scl <= '1';
				sda <= '0';
				if t >= T1-1 then
					nx_state <= start_B;
				else
					nx_state <= start_A;
				end if;
				
			when start_B =>
				scl <= '0';
				sda <= '0';
				if t >= T1-1 then
					nx_state <= SCL_Low_A;
				else
					nx_state <= start_B;
				end if;
				
			when SCL_Low_A =>
				scl <= '0';
				sda <= address(i);
				if t >= T1-1 then
					nx_state <= SCL_Low_B;
				else
					nx_state <= SCL_Low_A;
				end if;
				
			when SCL_Low_B =>
				scl <= '0';
				sda <= address(i);
				if t >= T1-1 then
					nx_state <= SCL_High_A;
				else
					nx_state <= SCL_Low_B;
				end if;
				
			when SCL_High_A =>
				scl <= '1';
				sda <= address(i);
				if t >= T1-1 then
					nx_state <= SCL_High_B;
				else
					nx_state <= SCL_High_A;
				end if;
				
			when SCL_High_B =>
				scl <= '1';
				sda <= address(i);
				if t >= T1-1 then
					i := i + 1;
					if i=7 then
						nx_state <= idle;
						i := 0;
					else
						nx_state <= SCL_Low_A;
					end if;
				else
					nx_state <= SCL_High_B;
				end if;
			
		end case;
	end process;
end architecture;
--------------------------------------------------------------------------------