library verilog;
use verilog.vl_types.all;
entity i2c_test_vlg_sample_tst is
    port(
        clk             : in     vl_logic;
        rst             : in     vl_logic;
        scl             : in     vl_logic;
        sda             : in     vl_logic;
        start_button    : in     vl_logic;
        sampler_tx      : out    vl_logic
    );
end i2c_test_vlg_sample_tst;
