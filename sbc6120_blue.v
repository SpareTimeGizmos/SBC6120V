//++
//sbc6120_blue.v
//
//   TOP LEVEL MODULE FOR SPARE TIME GIZMOS BLUE SBC6120
//   Copyright (C) 2011 by Spare Time Gizmos.  All rights reserved.
//
// DESIGN NAME:	SBC6120V
// DESCRIPTION:
//   This module runs the SBC6120V on the Spare Time Gizmos "Blue" SBC6120.
//
// REVISION HISTORY:
// 16-Mar-11 RLA  new file.
//--
//000000011111111112222222222333333333344444444445555555555666666666677777777778
//345678901234567890123456789012345678901234567890123456789012345678901234567890
`include "sbc6120_h.v"


module blue_bell (
  //++
  //  We're gonna guess that the beeper has a resonant frequency of about 4kHz.
  //--
  input  clock,		// free running 50Mhz system clock
  input  reset,		// global asynchronous reset
  input  bell,		// asserted for one tick to trigger the bell
  output speaker,	// push-pull speaker drive
  output speaker_n	//  ...
);
  parameter LOG2_DELAY = 23; // about 0.16s at 50Mhz
  reg [LOG2_DELAY-1:0] count;
  
  always @(posedge clock or posedge reset)
    if (reset)
      count <= 0;
    else if (bell)
      count <= 2**LOG2_DELAY-1;
    else if (count != 0)
      count <= count-1;
 
  assign speaker   = (count != 0) ?  count[13] : 0;
  assign speaker_n = (count != 0) ? ~count[13] : 0;
endmodule // ume2_bell


module blue_ui
  //++
  //   All the LEDs and switches on the Blue SBC6120 are interfaced with two
  // simple shift registers.  The output shift register is a fancy ST chip
  // that includes a constant current sink specifically for driving LEDs, but
  // logically it's just a shift register.  The input side is nothing more than
  // a chain of 74HC165 parallel in/serial out registers.  Conveniently there
  // are 32 bits on both the input and output side.
  //
  //   This module basically just a simple little state machine.  At the start
  // of every cycle it loads a 32 bit shift register with the current state of
  // all the LEDs, then it generates 32 clocks to shift out the LED data and
  // shift in the switch data, and lastly it updates the switch register state
  // from the data in the shift register.  It's not rocket science :-)
  //
  //   BTW, the SPI clock frequency will be 1/2 of what ever clock input you
  // give to this module.  The STP08DP05 parts are rated for 30MHz so they're
  // probably safe at any frequency we can manage, however the 74HC165s are
  // technically only guaranteed to about 10MHz at 3.3V.  In reality all the
  // ones I've seen will work way faster than that, but there's no need for
  // speed here and no point in pushing our luck.
  //--
#(
  parameter CLOCK_FREQUENCY = 12_000_000
) (
  input		clock,		// free running 50Mhz system clock
  input		reset,		// global asynchronous reset
  // FP6120 connections ...
  input [`WORD] data_leds,	// data display LEDs
  input [`WORD] addr_leds,	// address display LEDs
  input [`EMA]  ema_leds,	// EMA display LEDs
  input 	run_led,	// RUN LED
  input [0:3]   post_leds,	// POST code LEDs (on the back!)
  output reg [`FNSW_BUS]  fnsw,	// function switches (9 of them!)
  output reg [`ROTSW_BUS] rotsw,// rotary switch setting
  output reg [`WORD]      swreg,// switch register
  output reg [0:3] dipsw,	// DIP configuration switches
  output reg    sd_cd_n,	// SD card detect switch
  output reg    sd_wp_n,	// SD card write protect switch
  // SBC6120 "Blue" front panel connections ...
  output  reg   panel_sclk,	// SPI clock
  output        panel_mosi,	// master serial out, slave in
  input         panel_miso,	// master serial in, slave out
  output  reg   panel_load_n	// slave select (synchronizes a data word)
);
`include "clog2.v"

  // Local parameters ...
  localparam
    // DATA_BITS is the number of bits in the front panel SPI chain ...
    DATA_BITS = 32,
    //  DELAY_COUNT is the number of clock ticks to waste between update passes.
    // It's calculated to give approximately a 200Hz refresh rate.
    DELAY_COUNT = CLOCK_FREQUENCY/200 - DATA_BITS*2 - 1,
    COUNT_BITS = clog2(DELAY_COUNT),
    //   PASS_BITS is the number of bits in the update pass counter and, because
    // the switch state is updated every time the pass counter rolls over, also
    // implicitly sets the debounce interval.  Two bits gives a switch update
    // four passes, or about every 20ms with a 200Hz refresh rate.
    PASS_BITS = 2;
  
  // Our state machine states ...
  localparam [1:0]
    LOAD   = 0,			// load/unload shift register
    SHIFT1 = 1,			// positive SCLK part
    SHIFT2 = 2,			// negative SCLK part
    DELAY  = 3;			// delay before the next update
  reg [1:0] state, next;

  // Other locals ...
  reg [COUNT_BITS-1:0] count,	// general purpose counter for delays and bits
		       next_count;
  reg [DATA_BITS-1:0]  sr,	// shift register for data I/O
	               next_sr;
  reg [PASS_BITS-1:0]  pass,	// counts update passes for polling switches
		       next_pass;
  reg [`FNSW_BUS]  next_fnsw;	// updated function switch value
  reg [`ROTSW_BUS] next_rotsw;	//  "   "  rotary      "     "
  reg [`WORD]      next_swreg;	//  "   "  switch registers  "
  reg [0:3]        next_dipsw;	//  "   "  configuration     "
  reg next_sd_cd_n;		// updated SD card detected bit
  reg next_sd_wp_n;		//  "   "  "  write protected "


  //++
  //   The LEDs aren't mapped in a nice, neat 1:1 order onto the STP08DP05
  // outputs - instead the LED driver outputs are assigned to whatever LEDs
  // are physically closest and in whatever order makes the PCB layout most
  // convenient.  It really doesn't matter since all the LEDs and LED drivers
  // are electrically equivalent, but it does mean we have to do some random
  // permutation of the bits in order to map things correctly...
  //--
  function [DATA_BITS-1:0] map_leds (input [`WORD] data, input [`WORD] addr,
			     input [`EMA] ema, input run, input [0:3] post);
  begin
    map_leds[ 0] = run;		// U6, OUT7
    map_leds[ 1] = addr[9];	// U6, OUT6
    map_leds[ 2] = addr[11];	// U6, OUT5
    map_leds[ 3] = addr[10];	// U6, OUT4
    map_leds[ 4] = post[0];	// U6, OUT3
    map_leds[ 5] = post[1];	// U6, OUT2
    map_leds[ 6] = post[2];	// U6, OUT1
    map_leds[ 7] = post[3];	// U6, OUT0
    map_leds[ 8] = addr[5];	// U4, OUT7
    map_leds[ 9] = data[8];	// U4, OUT6
    map_leds[10] = addr[6];	// U4, OUT5
    map_leds[11] = data[9];	// U4, OUT4
    map_leds[12] = addr[7];	// U4, OUT3
    map_leds[13] = data[10];	// U4, OUT2
    map_leds[14] = addr[8];	// U4, OUT1
    map_leds[15] = data[11];	// U4, OUT0
    map_leds[16] = ema[0];	// U7, OUT7
    map_leds[17] = ema[1];	// U7, OUT6
    map_leds[18] = ema[2];	// U7, OUT5
    map_leds[19] = data[0];	// U7, OUT4
    map_leds[20] = addr[0];	// U7, OUT3
    map_leds[21] = data[1];	// U7, OUT2
    map_leds[22] = data[2];	// U7, OUT1
    map_leds[23] = data[3];	// U7, OUT0
    map_leds[24] = addr[1];	// U8, OUT7
    map_leds[25] = addr[3];	// U8, OUT6
    map_leds[26] = data[4];	// U8, OUT5
    map_leds[27] = addr[2];	// U8, OUT4
    map_leds[28] = data[5];	// U8, OUT3
    map_leds[29] = addr[4];	// U8, OUT2
    map_leds[30] = data[6];	// U8, OUT1
    map_leds[31] = data[7];	// U8, OUT0
  end
  endfunction // map_leds

  //++
  //   The switches have a similar problem - the actual switches are wired to
  // inputs ont he 74HC165s in a way that makes the PCB layout the easiest, and
  // not in any kind of sensible order.  Once again, we just need some random
  // logic around to permute them into the right places...
  //--
  function [0:3] map_dipsw (input [DATA_BITS-1:0] in);
  begin
    map_dipsw[0]	     = ~in[19];	// U11,D4
    map_dipsw[1] 	     = ~in[18];	// U11,D5
    map_dipsw[2] 	     = ~in[17];	// U11,D6
    map_dipsw[3] 	     = ~in[16];	// U11,D7
  end
  endfunction // map_dipsw
  function [`FNSW_BUS] map_fnsw (input [DATA_BITS-1:0] in);
  begin
    map_fnsw[`FNSW_BOOT]    = ~in[ 6];	// U9, D1
    map_fnsw[`FNSW_CLEAR]   = ~in[11];	// U10,D4 
    map_fnsw[`FNSW_CONT]    = ~in[22];	// U11,D1
    map_fnsw[`FNSW_DEP]     = ~in[20];	// U11,D3
    map_fnsw[`FNSW_EXAM]    = ~in[21];	// U11,D2
    map_fnsw[`FNSW_HALT]    = ~in[23];	// U11,D0
    map_fnsw[`FNSW_LA]      = ~in[ 5];	// U9, D2
    map_fnsw[`FNSW_LOCK]    = 0;	// U12,D3 (not used!)
    map_fnsw[`FNSW_LXA]     = ~in[ 4];	// U9, D3
  end
  endfunction // map_fnsw
  function [`ROTSW_BUS] map_rotsw (input [DATA_BITS-1:0] in);
  begin
    map_rotsw[`ROTSW_AC]    = ~in[31];	// U12,D0
    map_rotsw[`ROTSW_FLAGS] = ~in[25];	// U12,D6
    map_rotsw[`ROTSW_MD]    = ~in[29];	// U12,D2
    map_rotsw[`ROTSW_MQ]    = ~in[30];	// U12,D1
    map_rotsw[`ROTSW_POST]  = ~in[24];	// U12,D7
  end
  endfunction // map_rotsw
  function [`WORD] map_swreg (input [DATA_BITS-1:0] in);
  begin  
    map_swreg[ 0]           = ~in[ 2];	// U9, D5
    map_swreg[ 1]           = ~in[ 3];	// U9, D4
    map_swreg[ 2]           = ~in[ 1];	// U9, D6
    map_swreg[ 3]	    = ~in[ 7];	// U9, D0
    map_swreg[ 4]           = ~in[ 0];	// U9, D7
    map_swreg[ 5] 	    = ~in[15];	// U10,D0
    map_swreg[ 6] 	    = ~in[14];	// U10,D1
    map_swreg[ 7] 	    = ~in[13];	// U10,D2 
    map_swreg[ 8] 	    = ~in[12];	// U10,D3 
    map_swreg[ 9]	    = ~in[ 9];	// U10,D6 
    map_swreg[10] 	    = ~in[10];	// U10,D5 
    map_swreg[11]	    = ~in[ 8];	// U10,D7
  end
  endfunction // map_swreg


  // Synthesize the state FFs ...
  always @(posedge clock or posedge reset) begin
    if (reset) begin
      state <= LOAD;  sr <= 0;  count <= 0;  pass <= 0;
      fnsw <= 0;  rotsw <= 0;  swreg <= 0;  dipsw <= 0;
      sd_cd_n <= 1;  sd_wp_n <= 1;
    end else begin
      state <= next;  sr <= next_sr;
      count <= next_count;  pass <= next_pass;  swreg <= next_swreg;
      fnsw <= next_fnsw;  rotsw <= next_rotsw;  dipsw <= next_dipsw;
      sd_cd_n <= next_sd_cd_n;  sd_wp_n <= next_sd_wp_n;
    end
  end // always @ (posedge clock)

  // MOSI is always just the LSB of the output shifter.  It's that easy ...
  assign panel_mosi = sr[0];
  
  // left to right, LSB out first ....  LSB is bit 0 of the shift register.
  always @* begin
    next = state;  next_count = count;  next_pass = pass;
    next_sr = sr;  next_swreg = swreg;
    next_fnsw = fnsw;  next_rotsw = rotsw;  next_dipsw = dipsw;
    next_sd_cd_n = sd_cd_n;  next_sd_wp_n = sd_wp_n;
    panel_load_n = 1;  panel_sclk = 1;
    
    case (state)
      LOAD: begin
	//   If the pass counter has rolled over then it's time to update the
	// switch state.  It may seem wrong to update the switches at the
	// beginning of a pass, but at this point the shift register contains 
	// the valid switch data from the end of the last pass.
        if (pass == 0) begin
	  next_fnsw = map_fnsw(sr);  next_rotsw = map_rotsw(sr);
	  next_swreg = map_swreg(sr);  next_dipsw = map_dipsw(sr);
	  next_sd_cd_n =  sr[26]; // U12,D5
	  next_sd_wp_n =  sr[27]; // U12,D4
	end
	next_pass = pass+1;
	//   We also assert panel_load in this state, which parallel loads the
	// current switch states into the 74HC165 shift registers - that's what
	// we be reading into the shift register in the SHIFT1/2 states. There's
	// no harm in always doing this on every pass, even though we actually
	// only use the data we read in every fourth time.
	panel_load_n = 0;
	//   Load the shift register with the current LED data - that's what
	// we'll be sending out next, and set the count register to 31 to
	// keep track of the bits sent.
	next_sr = map_leds(data_leds, addr_leds, ema_leds, run_led, post_leds);
	//   Note that the sclk idle state is 1, so the next state needs to
	// be SHIFT2 so that we start with a negative edge!  This also gives
	// one clock for MOSI to settle after the SR is loaded...
	next_count = DATA_BITS-1;  next = SHIFT2;
      end
      
      SHIFT1: begin
	//   Panel SCLK goes to 1 during the SHIFT1 state.  The STP08DP05s latch
	// serial data on the positive clock edge so they'll latch the current
	// LSB of the SR at the beginning of this state.  Likewise the HC165s
	// shift on the positive clock edge, so they'll change MISO at the
	// start of this state.  Then at the end of SHIFT1, we update the shift
	// register, latching the new MISO bit and changing the MOSI bit.
	panel_sclk = 1;
        if (count == 0) begin
	  // We've finished all 32 bits - do the delay next...
	  next_count = DELAY_COUNT-1;  next = DELAY;
        end else begin
	  // Latch MISO and update MOSI (sr[0]) ...
          next_sr = {panel_miso, sr[31:1]}; 
	  next_count = count-1;  next=SHIFT2;
	end
      end

      SHIFT2: begin
	//   The SHIFT2 state doesn't really need to do anything except to
	// complete the clock cycle.
	panel_sclk = 0;  next=SHIFT1;
      end

      DELAY: begin
	// DELAY just spins here until it's time for another update cycle ...
	if (count == 0)
	  next = LOAD;
	else
	  next_count = count-1;
      end
    endcase // case (state)
  end // always @ *
  
endmodule // blue_ui


module sbc6120_blue (
  input        clock_50mhz_in,	// 50MHz master clock
  input        fpga_clk,	// 12MHz clock from the PIC
  // Serial port connections ...
//output       slu0_txd,	// serial line #0 transmit data
//input        slu0_rxd,	// serial line #0 receive data
//output       slu1_txd,	// serial line #1 transmit data
//input        slu1_rxd,	// serial line #1 receive data
  // PS/2 keyboard connections ...
//inout	       ps2_clock_n,	// PS/2 keyboard clock
//inout	       ps2_data_n,	// PS/2 keyboard data
  // VGA connections ...
//output       vga_red,		// VGA red video
//output       vga_green,	// VGA green video
//output       vga_blue,	// VGA blue video
//output       vga_hsync,	// VGA horizontal sync
//output       vga_vsync,	// VGA vertical sync
  // MMC/SD card and serial flash chip SPI interface ...
//output       sd_sclk,		// SPI clock
//output       sd_mosi,		// master (that's us!) serial out, slave in
//input        sd_miso,		// master serial in, slave out
//output       sd_cs_n,		// slave select
//output       flash_cs_n,	// flash chip select
  // DS1302 interface ...
//output       rtc_sclk,	// serial clock
//output       rtc_cs,		// chip enable and byte sync
//inout	       rtc_sio,		// bidirectional serial data
  // Speaker connections ...
//output       speaker,		// push pull drive for the piezo beeper
//output       speaker_n,	// ...
  // Front panel connections ...
  output       panel_sclk,	// SPI clock
  output       panel_mosi,	// master serial out, slave in
  input        panel_miso,	// master serial in, slave out
  output       panel_load_n	// slave select
  // XuLA SDRAM interface ....
);
  // Parameters ...
//parameter CLOCK_FREQUENCY      = 50_000_000;
//parameter CPU_CLOCK_DIVIDE     = 4;
//parameter CONSOLE_BAUD_RATE    = 19200;
//parameter PIXEL_CLOCK_MULTIPLY = 17;
//parameter PIXEL_CLOCK_DIVIDE   = 30;
//localparam CPU_CLOCK_FREQUENCY = CLOCK_FREQUENCY / CPU_CLOCK_DIVIDE;
//localparam PIXEL_FREQUENCY     = (CLOCK_FREQUENCY * PIXEL_CLOCK_MULTIPLY) / PIXEL_CLOCK_DIVIDE;

  wire panel_clk;
  ck_divider #(.IN_FREQ(12_000_000), .OUT_FREQ(1_000_000))
    ck_panel (.clock(fpga_clk), .reset(0), .out(panel_clk));

  wire [`FNSW_BUS] fnsw;  wire [`ROTSW_BUS] rotsw;  wire [`WORD] swreg;
  wire [0:3] dipsw;  wire sd_cd_n, sd_wp_n;
  blue_ui #(.CLOCK_FREQUENCY(1_000_000)) ui (
    .clock(fpga_clk), .reset(0),
//    .data_leds(12'o0200), .addr_leds(12'o0020), .ema_leds(3'O2), .run_led(1'B1), .post_leds(4'b0),
//    .data_leds(swreg), .addr_leds(~swreg), .ema_leds(3'o7),
//    .run_led(sd_cd_n), .post_leds(dipsw),
//    .data_leds(rotsw), .addr_leds(fnsw), .ema_leds(3'o7),
//    .run_led(1), .post_leds(dipsw),
    .data_leds(swreg), .addr_leds({rotsw[2:4], fnsw}), .ema_leds({1'b1,rotsw[0:1]}),
    .run_led(sd_cd_n), .post_leds(dipsw),
    .fnsw(fnsw), .rotsw(rotsw), .swreg(swreg), .dipsw(dipsw),
    .sd_cd_n(sd_cd_n), .sd_wp_n(sd_wp_n),
    .panel_sclk(panel_sclk), .panel_mosi(panel_mosi),
    .panel_miso(panel_miso), .panel_load_n(panel_load_n)
  );
  
endmodule // sbc6120_blue
