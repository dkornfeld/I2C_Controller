library verilog;
use verilog.vl_types.all;
entity i2c_test_vlg_check_tst is
    port(
        scl             : in     vl_logic;
        sda             : in     vl_logic;
        sampler_rx      : in     vl_logic
    );
end i2c_test_vlg_check_tst;
