library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
---------------------------------------------------------------------------------
entity LED_Accel is
port (
	acceleration1:				in std_logic_vector(9 downto 0);
	acceleration2:				in std_logic_vector(9 downto 0);
	acceleration3:				in std_logic_vector(9 downto 0);
	set_input1, set_input0: in std_logic;
	LED_Pattern:				out std_logic_vector(9 downto 0));
end entity;
---------------------------------------------------------------------------------
architecture LED_Accel_case_based of LED_Accel is
	signal acc1, acc2, acc3: integer range 0 to 1023;
	signal acc: integer range 0 to 1023;
	signal input_vector: std_logic_vector(1 downto 0);
begin
	input_vector <= (set_input1, set_input0);
	acc1 <= to_integer(unsigned(acceleration1));
	acc2 <= to_integer(unsigned(acceleration2));
	acc3 <= to_integer(unsigned(acceleration3));
	with input_vector select acc <=
		acc1 when "00",
		acc2 when "01",
		acc3 when "10",
		0 when others;
	process(acc)
	begin
		if acc >= 0 and acc <= 100 then
			LED_Pattern <= "1000000000";
		elsif acc >= 101 and acc <= 200 then
			LED_Pattern <= "0100000000";
		elsif acc >= 201 and acc <= 300 then
			LED_Pattern <= "0010000000";
		elsif acc >= 301 and acc <= 400 then
			LED_Pattern <= "0001000000";
		elsif acc >= 401 and acc <= 500 then
			LED_Pattern <= "0000100000";
		elsif acc >= 501 and acc <= 600 then
			LED_Pattern <= "0000010000";
		elsif acc >= 601 and acc <= 700 then
			LED_Pattern <= "0000001000";
		elsif acc >= 701 and acc <= 800 then
			LED_Pattern <= "0000000100";
		elsif acc >= 801 and acc <= 900 then
			LED_Pattern <= "0000000010";
		elsif acc >= 901 and acc <= 1023 then
			LED_Pattern <= "0000000001";
		end if;
	end process;
end architecture;
------------------------------------------------------------------------------------
	