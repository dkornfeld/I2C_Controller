library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
--------------------------------------------------------------------------------
entity Wii_Nunchuk_Master is
port (
	clk, rst:							 	in std_logic;	-- System clock and reset
   LED_Display:                    	out std_logic_vector(9 downto 0); -- For Accel. Demo
	SSD5: 									out std_logic_vector(6 downto 0); -- For Joystick
	SSD4: 									out std_logic_vector(6 downto 0); -- Demo
	SSD3: 									out std_logic_vector(6 downto 0);
	SSD2: 									out std_logic_vector(6 downto 0);
	SSD1: 									out std_logic_vector(6 downto 0);
	SSD0: 									out std_logic_vector(6 downto 0);
	sda, scl: 								inout std_logic); -- I2C ports
end entity;
--------------------------------------------------------------------------------
architecture fsm of Wii_Nunchuk_Master is
	--I2C Element Declarations
	type i2c_element is (START, ONE, ZERO, S_ACK, M_ACK, M_NACK, RD, STOP);
	type i2c_message is array(natural range <>) of i2c_element;

	--FSM-related declarations:
	type state is (Waiting, HiA, HiB, LoA, LoB);
	signal pr_state, nx_state: state;

	--Timer-related delcarations:
	constant T1: natural := 500/4; --100kHz @ fclk=50MHz
	constant T2: natural	:= 4000;  -- 80 uS @ fclk=50MHz
	constant tmax: natural := T2 + 1;
	signal t: natural range 0 to tmax; -- For timing state transitions
	
	--Data and address
	
	-- Sets up the registers in the Wii Nunchuk to respond to reading registers
	-- Wii Nunchuk 7-bit address = 0x52
	-- Configuration Bytes: [0xF0, 0x55] then [0xFB, 0x00]
	constant setup: i2c_message(0 to 57) := (		START,-- Begin first message
																ONE, ZERO, ONE, ZERO, ZERO, ONE, ZERO, -- 0x52
																ZERO, -- Write
																S_ACK,-- Let Slave acknowledge
																ONE, ONE, ONE, ONE, ZERO, ZERO, ZERO, ZERO, --0xF0
																S_ACK,-- Let Slave acknowledge
																ZERO, ONE, ZERO, ONE, ZERO, ONE, ZERO, ONE, --0x55
																S_ACK,-- Let Slave acknowledge
																STOP, -- Done with first message
																
																START,-- Begin next message
																ONE, ZERO, ONE, ZERO, ZERO, ONE, ZERO, -- 0x52
																ZERO, -- Write
																S_ACK,-- Let Slave acknowledge
																ONE, ONE, ONE, ONE, ONE, ZERO, ONE, ONE, --0xFB
																S_ACK,-- Let Slave acknowledge
																ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, --0x00
																S_ACK,-- Let Slave acknowledge
																STOP);-- End of setup
	-- Handled receiving the data, loops once setup is done
	-- Wii Nunchuk 7-bit address = 0x52
	-- Reading NUM_BYTES bytes, starting from register 0x00
	constant receive: i2c_message(0 to 84) :=  ( START,-- Begin request message
																ONE, ZERO, ONE, ZERO, ZERO, ONE, ZERO, -- 0x52
																ZERO, -- Write
																S_ACK,-- Let Slave acknowledge
																ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, -- 0x00
																S_ACK,-- Let Slave acknowledge
																STOP, -- Done requesting
																START,-- Begin RX message
																ONE, ZERO, ONE, ZERO, ZERO, ONE, ZERO, -- 0x52
																ONE,  -- Read
																S_ACK,-- Let Slave acknowledge
																RD, RD, RD, RD, RD, RD, RD, RD, -- Read a byte
																M_ACK,-- Acknowledge the received byte
																RD, RD, RD, RD, RD, RD, RD, RD, -- Read a byte
																M_ACK,-- Acknowledge the received byte
																RD, RD, RD, RD, RD, RD, RD, RD, -- Read a byte
																M_ACK,-- Acknowledge the received byte
																RD, RD, RD, RD, RD, RD, RD, RD, -- Read a byte
																M_ACK,-- Acknowledge the received byte
																RD, RD, RD, RD, RD, RD, RD, RD, -- Read a byte
																M_ACK,-- Acknowledge the received byte
																RD, RD, RD, RD, RD, RD, RD, RD, -- Read a byte
																M_NACK,-- Acknowledge the received byte and end transaction
																STOP);
	constant setup_length: natural := setup'length;
   constant receive_length: natural := receive'length;
	
	-- Indices for looping through send/receive data (made larger to detect sat.)
	signal i: natural range 0 to setup_length;
   signal j: natural range 0 to receive_length;
	
	-- For parsing I2C Data
	constant NUM_BYTES: natural := 6; -- Number bytes to read back
	signal DP: natural range 0 to 8*NUM_BYTES - 1; -- Data pointer for Rx data
   signal raw_data: std_logic_vector(0 to 8*NUM_BYTES - 1); -- Raw data dump register
	
	-- For output data - Gets values from appropriate indices of raw_data
	signal C_Button: std_logic;						-- C button (active low)
	signal Z_Button: std_logic;						-- Z button (active low)
	signal XAccel: std_logic_vector(9 downto 0);	-- X accelerometer
	signal YAccel: std_logic_vector(9 downto 0);	-- Y accelerometer
	signal ZAccel: std_logic_vector(9 downto 0);	-- Z accelerometer
	signal XStick: std_logic_vector(7 downto 0); -- X joystick
	signal YStick: std_logic_vector(7 downto 0); -- Y joystick
   
	-- declare the LED pattern generator (for displaying output data)
	component LED_Accel is
		port (
			acceleration1:				in std_logic_vector(9 downto 0);
			acceleration2:				in std_logic_vector(9 downto 0);
			acceleration3:				in std_logic_vector(9 downto 0);
			set_input1, set_input0: in std_logic;
			LED_Pattern:				out std_logic_vector(9 downto 0));
	end component LED_Accel;
	
	-- declare the SSD pattern generator (for displaying output data)
	component byte_to_ssd is
		port (
			x:     in std_logic_vector(7 downto 0);
			ssd2: out std_logic_vector(6 downto 0);
			ssd1: out std_logic_vector(6 downto 0);
			ssd0: out std_logic_vector(6 downto 0));
	end component byte_to_ssd;
	
begin

	--Clock and Reset
	process (clk, rst)                                      
	begin
      if rst = '0' then
         t <= 0;                        --reset the system clock counter
			i <= 0;                        --reset the initialization message index
         j <= 0;                        --reset the receive loop message index
         DP <= 0;                       --reset the data pointer
         pr_state <= Waiting;           --go back to the idle state
      elsif rising_edge(clk) then
      if pr_state /= nx_state then      --if we are transitioning to a new state
         t <= 0;                        --reset the system clock counter
         if nx_state = LoB or nx_state = Waiting then    --and check if we need
                                                         --to move on to the next
                                                         --message element
            if i < setup_length then             
               i <= i + 1;                       --if i is saturated, then
            elsif j < receive_length - 1 then    --we are in the receive loop
               if receive(j) = RD then           --if the last message element
                 DP <= DP + 1;                   --was a read, increment the
               end if;                           --data pointer
               j <= j + 1;                       --move on to next element
            else                                 --otherwise, done receiving once
               j <= 0;                           --so reset receive loop index
               DP <= 0;                          --and the data pointer
            end if;
         end if;
      elsif t /= tmax then              --if not transitioning to the next
         t <= t + 1;                    --state, increment the system clock
      end if;                           --counter, if not already saturated
      pr_state <= nx_state;             --update the current state as fast
     end if;                            --as possible (on fclk)
	end process;

	--FSM combinational logic:
	process (all)
		variable internal_element: i2c_element;
    begin
		case pr_state is
			
        when Waiting =>                         --in the idle state
            scl <= '1';                         --want both the serial data
            sda <= 'Z';                         --and serial clock lines high
            if t < T2 - 1 then                  --if timer counter is below
               nx_state <= Waiting;             --the idle time, stay here
            else
               nx_state <= HiA;                 --otherwise, move on
            end if;
            
        when HiA =>                             --in the first half of the
            scl <= '1';                         --serial clock's high time
            if i < setup_length then
               internal_element := setup(i);    --still in the setup loop
            else
               internal_element := receive(j);  --or in the receive loop
            end if;
            case internal_element is            --the serial data should be
                when START => 	sda <= 'Z';    --both sda and scl are high
                                                --right after starting
                when ONE => 	sda <= 'Z';     	--high when sending a one
                when ZERO => 	sda <= '0';     	--low when sending a zero
                when S_ACK => sda <= 'Z';     	--hiZ to let the slave ack
                when M_ACK => sda <= '0';     	--low to perform an ack
                when M_NACK =>sda <= 'Z';     	--high to ack when done sending
                when STOP => 	sda <= '0';     	--bring scl high first when
                                                --stopping so leave sda low
                when RD => 	sda <= 'Z';     	--hiZ when reading to let slave
                                                --transmit
                when others =>sda <= 'Z';     	--hiZ by default
            end case;
            if t >= T1-1 then                   --if timer counter is below
                nx_state <= HiB;                --loop time, stay here
            else
                nx_state <= HiA;                --otherwise, move on
            end if;
            
        when HiB =>                             --in the second half of the
            scl <= '1';                         --serial clock's high time
            if i < setup_length then
               internal_element := setup(i);    --still in the setup loop
            else
               internal_element := receive(j);  --or in the receive loop
            end if;
            case internal_element is            --the serial data should be
                when START => sda <= '0';     	--bring sda low while scl is
                                                --high to start i2c 
                when ONE => 	sda <= 'Z';     	--high when sending a one
                when ZERO => 	sda <= '0';     	--low when sending a zero
                when S_ACK => sda <= 'Z';     	--hiZ to let the slave ack
                when M_ACK => sda <= '0';     	--low to perform an ack
                when M_NACK =>sda <= 'Z';     	--high to ack when done sending
                when STOP => 	sda <= 'Z';     	--now bring sda high because
                                                --scl already went high first
                when RD => 	sda <= 'Z';     	--hiZ when reading to let slave
                                                --transmit
                when others =>sda <= 'Z';     	--hiZ by default
            end case;                           
            if receive(j) = RD then             --if have a READ message, then
               raw_data(DP) <= sda;             --store the serial data bit 
            end if;                             --in the raw_data buffer
            if t >= T1-1 then                   --next state calculations:
                if internal_element = STOP then --if have a STOP message, then
                    nx_state <= Waiting;        --go back to the idle state
                else
                    nx_state <= LoA;            --otherwise, move on
                end if;
            else
                nx_state <= HiB;                --stay here, by default
            end if;
            
        when LoA =>                             --in the first half of the
            scl <= '0';                         --serial clock's low time
            if i < setup_length then            
               internal_element := setup(i);    --still in the setup loop
            else
               internal_element := receive(j);  --otherwise, in receive loop
            end if;
            case internal_element is            --the serial data should be
                when START => sda <= '0';     	--keep sda low just after scl
																--is brought low to start i2c
                when ONE => 	sda <= 'Z';     	--high when sending a one
                when ZERO => 	sda <= '0';     	--low when sending a zero
                when S_ACK => sda <= '0';     	--hiZ to let the slave ack
                when M_ACK => sda <= '0';     	--low to perform an ack
                when M_NACK =>sda <= 'Z';     	--high to ack when done sending
                when STOP => 	sda <= '0';     	--scl comes high first when
                                                --stopping, so leave sda low
                when RD => 	sda <= 'Z';     	--hiZ when reading to let slave
                                                --transmit
                when others =>sda <= 'Z';     	--hiZ by default
            end case;
            if t >= T1-1 then                   --if timer counter is high
                nx_state <= LoB;                --enough, then move on
            else
                nx_state <= LoA;                --otherwise, stay here
            end if;
            
        when LoB =>                             --in the second half of the 
            scl <= '0';                         --serial clock's low time
            if i < setup_length then            
               internal_element := setup(i);    --still in the setup loop
            else
               internal_element := receive(j);  --otherwise, in receive loop
            end if;
            case internal_element is            --the serial data should be
                when START => sda <= 'Z';     	--high when starting
                when ONE => 	sda <= 'Z';     	--high when sending a one
                when ZERO => 	sda <= '0';     	--low when sending a zero
                when S_ACK => sda <= 'Z';     	--hiZ to let the slave ack
                when M_ACK => sda <= '0';     	--low to perform an ack
                when M_NACK =>sda <= 'Z';     	--high to ack when done sending
                when STOP => 	sda <= '0';     	--again, scl comes high first
                                                --when stopping, so sda is low
                when RD => 	sda <= 'Z';     	--hiZ when reading to let slave
																--transmit
                when others =>sda <= 'Z';     	--hiZ by default
            end case;
            if t >= T1-1 then                   --if timer counter is high
                nx_state <= HiA;                --enough, then move on
            else
                nx_state <= LoB;                --otherwise, stay here
            end if;
            
		end case;
	end process;
   
   --Display
	
	-- Get data into proper order from raw_data
	XStick(7 downto 0) <= raw_data(0 to 7);
	YStick(7 downto 0) <= raw_data(8 to 15);
	XAccel(9 downto 0) <= raw_data(16 to 23) & raw_data(44 to 45);
	YAccel(9 downto 0) <= raw_data(24 to 31) & raw_data(42 to 43);
	ZAccel(9 downto 0) <= raw_data(32 to 39) & raw_data(40 to 41);
	C_Button <= not raw_data(8*NUM_BYTES - 2);
	Z_Button <= not raw_data(8*NUM_BYTES - 1);
   -- Connect LED Pattern display
	U1: LED_Accel port map(XAccel, YAccel, ZAccel, C_Button, Z_Button, LED_Display);
	-- Connect Low 3 SSDs display
	U2: byte_to_ssd port map(XStick, SSD5, SSD4, SSD3);
	-- Connect Top 3 SSDs display
	U3: byte_to_ssd port map(Ystick, SSD2, SSD1, SSD0);
   
end architecture;
--------------------------------------------------------------------------------