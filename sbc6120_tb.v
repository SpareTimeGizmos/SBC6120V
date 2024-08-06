//++
//sbc6120_tb.v
//
//   SBC6120V Test Bench
//   Copyright (C) 2011 by Spare Time Gizmos.  All rights reserved.
//
// DESIGN NAME:	SBC6120V
// DESCRIPTION:
//
// TARGET DEVICES
// TOOL VERSIONS 
// REVISION HISTORY:
//--
//000000011111111112222222222333333333344444444445555555555666666666677777777778
//345678901234567890123456789012345678901234567890123456789012345678901234567890
`include "sbc6120_h.v"


module sbc6120_tb;
  parameter CLOCK_PERIOD = 10;

  reg clock, reset;
  wire [15:0] xram_ma;  wire [11:0] xram_md;  wire xram_oe_n, xram_we_n;
  wire [2:0] ide_da;  wire [15:0] ide_dd;  wire [0:2] leds;
  wire ide_dior, ide_diow_n, ide_dreset_n, ide_cs1fx_n, ide_cs3fx_n, ide_dasp_n;
  wire console_rxd, console_txd;

  sbc6120 sbc6120 (
    .clock(clock), .reset(reset),
    .xram_ma(xram_ma), .xram_md(xram_md),
    .xram_oe_n(xram_oe_n), .xram_we_n(xram_we_n),
    .ide_da(ide_da), .ide_dd(ide_dd),
    .ide_dior_n(ide_dior_n), .ide_diow_n(ide_diow_n),
    .ide_cs1fx_n(ide_cs1fx_n), .ide_cs3fx_n(ide_cs3fx_n),
    .ide_dreset_n(ide_dreset_n), .ide_dasp_n(ide_dasp_n),
    .console_rxd(console_rxd), .console_txd(console_txd)
  );

  initial begin
//    $display("sbc6120:(%6d) <<< ASSERTING RESET >>>", $time);
    reset = 1'b1;  #1 @(negedge clock);  reset = 1'b0;
    $display("<<< INITIALIZATION DONE >>>");
  end

  always begin
    clock = 1'b0;
    forever #(CLOCK_PERIOD/2) clock = ~clock;
  end
endmodule

