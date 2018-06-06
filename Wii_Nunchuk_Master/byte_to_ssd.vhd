----------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
----------------------------------------------------------------------------------
entity byte_to_ssd is
port (
   x:     in std_logic_vector(7 downto 0);
   ssd2: out std_logic_vector(6 downto 0);
   ssd1: out std_logic_vector(6 downto 0);
   ssd0: out std_logic_vector(6 downto 0));
end entity;
----------------------------------------------------------------------------------
architecture byte_to_ssd_sequential of byte_to_ssd is
   signal ones_digit: integer range 0 to 9;
   signal tens_digit: integer range 0 to 9;
   signal huns_digit: integer range 0 to 9;
begin
   ones_digit <= to_integer(unsigned(x)) mod 10;
   tens_digit <= (to_integer(unsigned(x)) mod 100) / 10;
   huns_digit <= to_integer(unsigned(x)) / 100;
   with ones_digit select
       ssd0 <= "0000001" when 0,
               "1001111" when 1,
               "0010010" when 2,
               "0000110" when 3,
               "1001100" when 4,
               "0100100" when 5,
               "0100000" when 6,
               "0001111" when 7,
               "0000000" when 8,
               "0000100" when 9,
               null when others;
   with tens_digit select
       ssd1 <= "0000001" when 0,
               "1001111" when 1,
               "0010010" when 2,
               "0000110" when 3,
               "1001100" when 4,
               "0100100" when 5,
               "0100000" when 6,
               "0001111" when 7,
               "0000000" when 8,
               "0000100" when 9,
               null when others;
    with huns_digit select
       ssd2 <= "0000001" when 0,
               "1001111" when 1,
               "0010010" when 2,
               "0000110" when 3,
               "1001100" when 4,
               "0100100" when 5,
               "0100000" when 6,
               "0001111" when 7,
               "0000000" when 8,
               "0000100" when 9,
               null when others;
end architecture;
----------------------------------------------------------------------------------