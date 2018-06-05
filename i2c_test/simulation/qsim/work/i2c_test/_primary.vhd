library verilog;
use verilog.vl_types.all;
entity i2c_test is
    port(
        clk             : in     vl_logic;
        start_button    : in     vl_logic;
        rst             : in     vl_logic;
        sda             : inout  vl_logic;
        scl             : inout  vl_logic
    );
end i2c_test;
