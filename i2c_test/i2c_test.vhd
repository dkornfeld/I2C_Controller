library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
--------------------------------------------------------------------------------
entity i2c_test is
port (
	clk, start_button, rst:			 	in std_logic;
   data_display:                    out std_logic_vector(7 downto 0);
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
   constant receive: i2c_message(0 to 84) :=  ( START,
                                                ONE, ZERO, ONE, ZERO, ZERO, ONE, ZERO, -- 0x52
                                                ZERO, -- Write
                                                S_ACK,
                                                ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, -- 0x00
                                                S_ACK,
                                                STOP,
                                                START,
                                                ONE, ZERO, ONE, ZERO, ZERO, ONE, ZERO, -- 0x52
                                                ONE, -- Read
                                                S_ACK,
                                                RD, RD, RD, RD, RD, RD, RD, RD,
                                                M_ACK,
																RD, RD, RD, RD, RD, RD, RD, RD,
                                                M_ACK,
																RD, RD, RD, RD, RD, RD, RD, RD,
                                                M_ACK,
																RD, RD, RD, RD, RD, RD, RD, RD,
                                                M_ACK,
																RD, RD, RD, RD, RD, RD, RD, RD,
                                                M_ACK,
																RD, RD, RD, RD, RD, RD, RD, RD,
                                                M_ACK,
                                                STOP);
	constant setup_length: natural := setup'length;
   constant receive_length: natural := receive'length;
	signal i: natural range 0 to setup_length;
   signal j: natural range 0 to receive_length;
	constant NUM_BYTES: natural := 6;
	signal DP: natural range 0 to 8*NUM_BYTES - 1;
   signal raw_data: std_logic_vector(0 to 8*NUM_BYTES - 1);
   
begin

	--Clock and Reset
	process (clk, rst)
	begin
      if rst = '0' then
         t <= 0;
			i <= 0;
         j <= 0;
			DP <= 0;
         pr_state <= Waiting;
		elsif rising_edge(clk) then
         if pr_state /= nx_state then
				t <= 0;
            if nx_state = LoB or nx_state = Waiting then
               if i < setup_length then
                  i <= i + 1;
               elsif j < receive_length - 1 then
						if receive(j) = RD then
							DP <= DP + 1;
						end if;
                  j <= j + 1;
               else
                  j <= 0;
						DP <= 0;
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
					when STOP => 	sda <= 'Z';
					when RD => 		sda <= 'Z';
					when others => sda <= 'Z';
				end case;
				if receive(j) = RD then
               raw_data(DP) <= sda;
            end if;
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
					when S_ACK => 	sda <= '0';
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
   data_display(7 downto 0) <= raw_data(0 to 7);
   
end architecture;
--------------------------------------------------------------------------------