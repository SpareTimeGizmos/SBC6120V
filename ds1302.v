//++
//ds1302.v - time of day clock and calendar interface
//Copyright (C) 2011-2014 by Spare Time Gizmos.  All rights reserved.
//
// DESCRIPTION:
//   This module implements a PDP-8 interface to the Dallas Semiconductor DS1302
// real time clock/calendar and non-volatile RAM chip.  The DS1302 uses a
// proprietary serial protocol that's a little like SPI but with a bidirectional
// data line rather than the usual MOSI/MISO.
//
//   The PDP-8 side of the interface contains an eight bit data register and a
// seven bit command register.  To send data to the DS1302, the PDP-8 software
// would first load the data register with the data to be sent and then load the
// command register with the command (which is really nothing more than the
// register address).  To read data from the DS1302 the software would load the
// command register first, wait for the done flag, and then read the result
// from the data register.  In both cases the state machine in this module takes
// care of all the necessary handshaking with the DS1302 and sets the DONE flag
// when it's finished.
//
// The IOTs implemented by this interface are -
//
//   6140 CCCM - load command register, clear ac, clear done, and go
//   6141 CCSF - skip on DONE flag
//   6142 CCWD - write data register and clear the AC
//   6143 CCRD - read (jam transfer) data register
//
//   FWIW, this interface and these IOTs are exactly compatible with Jim
// Kearney's implementation for the IOB6120.
//
// REVISION HISTORY:
// 12-Feb-11  RLA  New file.
// 29-Dec-13  RLA  MMC commands are 7 bits, not 8 (the MSB is always 1)
//  1-Jan-14  RLA  Eliminate the system wide reset input (although we still have
//		   IOCLR).  Add an initial block to define the initial state.
//--
//000000001111111111222222222233333333334444444444555555555566666666667777777777
//234567890123456789012345678901234567890123456789012345678901234567890123456789
`include "sbc6120_h.v"		// global declarations for this project
`include "opcodes_h.v"		// PDP-8/HD6120 opcode mnemonics


module rtc_timing
  //++
  //   This module generates the clock for DS1302 serial communications (yes,
  // it's the "clock" for the "clock"!).  The datasheet says a maximum clock
  // frequency of 500kHz for the DS1302, so we'll aim for 400kHz just to be
  // safe.
  //
  //   N.B. The state machine actually runs at 2x the DS1302 SCLK frequency -
  // that's it can shift data on one SCLK edge and latch it on the other.
  // That means the ouptut here is actually about 800kHz...
  //--
#(
  parameter SYSTEM_CLOCK = 25_000_000,	// clock frequency (in Hz!)
  parameter DS1302_CLOCK =    400_000	// desired DS1302 serial clock
) (
  input	 clock,		// CPU master clock (usually 25MHz or so)
  output sclk_tick	// derived RTC tick
 );
`include "clog2.v"
  
  // Locals ...
  localparam
   DIVISOR = SYSTEM_CLOCK/(DS1302_CLOCK*2),
   NBITS   = clog2(DIVISOR);
  reg [NBITS-1:0] count;

  initial 
    count = 0;
   
  always @(posedge clock)
    if (count == (DIVISOR-1))
      count <= 0;
    else
      count <= count+1;

  assign sclk_tick = (count == (DIVISOR-1));
endmodule // rtc_timing


module rtc_control
  //++
  // DS1302 I/O state machine
  //--
#(
  parameter SETUP_TICKS  = 3,		// DS1302 setup time
  parameter HOLD_TICKS   = 6		// DS1302 hold time
) (
  // Bus interface signals ...	     
  input            clock,	// system wide clock
  input		   ioclr,	// clear all I/O devices (CAF)
  input      [6:0] cmd,		// command 
  input      [7:0] dout,	// data out to DS1302
  output reg [7:0] din,		// data in from DS1302
  input            start,	// asserted for 1 tick to start state machine
  output reg       idle,	// asserted when state machine is idle
  // DS1302 interface ...
  input		   sclk_tick,	// timing tick for DS1302 serial clock
  output reg       rtc_sclk,	// serial clock
  output reg       rtc_ce,	// chip enable and byte sync
  inout            rtc_sio	// bidirectional serial data
);
  // State machine definitions ...
  localparam [3:0]
    IDLE    = 0,		// wait for "start"
    SETUP   = 1,		// delay for DS1302 CE setup time requirement
    TXCMD0  = 2,		// send command bits (SCLK low)
    TXCMD1  = 3,		//  "    "   "   "   (SCLK high)
    TXDATA0 = 4,		// send data bits    (SCLK low)
    TXDATA1 = 5,		//   "   "    "      (SCLK high)
    RXDATA0 = 6,		// receive data bits (SCLK low)
    RXDATA1 = 7,		//  "   "    "   "   (SCLK high)
    HOLD    = 8;		// delay for DS1302 CE hold/inactive time
  reg [3:0] state, next;

  // Locals ...
  reg [7:0] ssr, next_ssr;	// serial data shift register
  reg [2:0] nbits, next_nbits;	// number of bits in ssr left to send
  reg 	    rwbit, next_rwbit;	// the read/-write bit from the command byte
  reg 	    rtc_dout;		// data to drive SIO when we're sending
  reg	    drive_sio;		// TRUE to enable SIO bus driver
  reg [7:0] next_din;  reg next_sclk, next_ce;

  // Create the tri-state bus driver for rtc_sio ...
  assign rtc_sio = drive_sio ? rtc_dout : 1'bz;

  // Define the initial state for everything ...
  initial begin
    state = IDLE;  din = 0;  ssr = 0;  nbits = 0;  rwbit = 0;
    rtc_sclk = 0;  rtc_ce = 0;
  end
  
  // State machine flip-flops ....
  always @(posedge clock) begin
    if (ioclr) begin
      state <= IDLE;  din <= 0;  ssr <= 0;  nbits <= 0;  rwbit <= 0;
      rtc_sclk <= 0;  rtc_ce <= 0;
    end else begin
      state <= next;  din <= next_din;  ssr <= next_ssr;
      nbits <= next_nbits;  rwbit <= next_rwbit;
      rtc_sclk <= next_sclk;  rtc_ce <= next_ce;
    end
  end
  
  // And here's the state machine ...
  always @* begin
    next = state;  next_din = din;  next_ssr = ssr;  next_nbits = nbits;
    next_rwbit = rwbit;  next_sclk = rtc_sclk;  next_ce = rtc_ce;
    rtc_dout = 0;  drive_sio = 0;  idle = 0;

    case (state)
      // While idle, just wait for a "start" tick ...
      IDLE: begin
	idle = 1;
	if (start) begin
	  //   Capture the command byte and R/-W bit now, and start the delay
	  // for the CE setup time.  Note that the MSB of the command byte is
	  // always "1", and that's hardwired here...
	  next_ssr = {1'b1, cmd};  next_rwbit = cmd[0];  next_ce = 1;
	  next_nbits = SETUP_TICKS-1;  next = SETUP;
	end
      end // case: IDLE

      //   This DS1302 requires a fairly long time, about 2 SCLK times, between
      // asserting CE and the first rising clock edge.  This state generates
      // that delay.  Remember that our state machine clock is 2x the actual
      // SCLK frequency, and notice that we're reusing the nbits counter here
      // to count SCLKs, not actual bits ...
      SETUP: if (sclk_tick) begin
	if (nbits == 0) begin
	  // Ready to send the command next....
	  next_nbits = 7;  next = TXCMD0;
	end else
	  next_nbits = nbits-1;
      end // case: SETUP

      //   The TXCMD0/1 states transmit the byte in the ssr, LSB first.  The
      // current bit is asserted on SIO in the TXCMD0 state, and SCLK is set
      // at the end of TXCMD0 and cleared at the end of TXCMD1.  The ssr is
      // then shifted left 1 bit at the end of TXCMD1, and so on...
      TXCMD0: begin
        rtc_dout = ssr[0];  drive_sio = 1;
	if (sclk_tick) begin
	  next_sclk = 1;  next = TXCMD1;
	end
      end // case: TXCMD0
      TXCMD1: begin
	rtc_dout = ssr[0];  drive_sio = 1;
	if (sclk_tick) begin
	  next_sclk = 0;
	  if (nbits == 0) begin
	    // Transmit or receive data next ...
	    if (rwbit) begin
	      next_ssr = 0;     next_nbits = 7;  next = RXDATA0;
	    end else begin
	      next_ssr = dout;  next_nbits = 7;  next = TXDATA0;
	    end
	  end else begin
	    next_ssr = {1'b0,ssr[7:1]};  next_nbits = nbits-1;  next = TXCMD0;
	  end
	end
      end // case: TXCMD1

      // Transmit a data byte - it's pretty much the same as TXCMD ...
      TXDATA0: begin
        rtc_dout = ssr[0];  drive_sio = 1;
	if (sclk_tick) begin
	  next_sclk = 1;  next = TXDATA1;
	end
      end // case: TXDATA0
      TXDATA1: begin
	rtc_dout = ssr[0];  drive_sio = 1;
	if (sclk_tick) begin
	  next_sclk = 0;
	  if (nbits != 0) begin
	    next_ssr = {1'b0,ssr[7:1]};  next_nbits = nbits-1;  next = TXDATA0;
	  end else begin
	    next_nbits = HOLD_TICKS-1;  next = HOLD;
	  end
	end
      end // case: TXDATA1

      //   Receive a byte - this is pretty much like TXCMD too, except this time
      // we leave our SIO pin in the Hi-Z state and read back what the DS1302
      // sends to us.  We still generate the clock, though, and we sample SIO
      // on the rising edge.  The ssr is used to accumulate the first seven
      // bits, and then the entire byte is transferred to din on the 8th bit.
      RXDATA0: if (sclk_tick) begin
	next_sclk = 1;  next = RXDATA1;
	if (nbits != 0)
	  next_ssr = {rtc_sio, ssr[7:1]};
	else
	  next_din = {rtc_sio, ssr[7:1]};
      end // case: RXDATA0
      RXDATA1: if (sclk_tick) begin
	next_sclk = 0;
	if (nbits != 0) begin
	  next_nbits = nbits-1;  next = RXDATA0;
	end else begin
	  next_nbits = HOLD_TICKS-1;  next = HOLD;
	end
      end // case: RXDATA1

      //   The DS1302 requires at least 240ns between the last SCLK and the time
      // CE is released, and then it needs CE to be inactive (low) for at least
      // 4us before the next transaction.  240ns is not very long on the order
      // of the SCLK speed, but it's still several CPU clocks.  To make things
      // easy, we just delay an extra SCLK tick and release CE after the first
      // one.  It's longer than it needs to be, but it works.
      HOLD: if (sclk_tick) begin
	next_ce = 0;
	if (nbits == 0)
	  next = IDLE;
	else
	  next_nbits = nbits-1;
      end // if (sclk_tick)
    endcase // case (state)
  end // always @ *
endmodule // rtc_control


module rtc_1302
  //++
  //--
#(
  parameter SYSTEM_CLOCK = 25_000_000,	// clock frequency (in Hz!)
  parameter SELECT = 6'O14		// all IOTs for this interface
) (
  // Bus interface signals ...	     
  input		 clock,		// system wide clock
  input  [`WORD] ax,		// address bus (carries the IOT)
  inout  [`WORD] dx,		// bidirectional data bus
  input  	 iowr,		// I/O write strobe
  input		 iord,		// I/O read strobe
  input		 ioclr,		// clear all I/O devices (CAF)
  output	 ioskip,	// asserted if this IOT should skip
  output	 ioc0,		// asserted if this IOT should clear the AC
  output	 ioc1,		// asserted if this IOT should load the AC
  // DS1302 interface ...
  output	 rtc_sclk,	// serial clock
  output	 rtc_ce,	// chip enable and byte sync
  inout		 rtc_sio	// bidirectional serial data
);
  wire selected = ax[3:8] == SELECT;

  // Locals ...
  reg  [7:0] dout;		// data -> DS1302 (out) register
  wire [7:0] din;		// data <- DS1302 (in)  register
  wire       idle;		// state machine idle
  wire	     sclk_tick;		// state machine timing tick
  
  // CCCM, CCWD, and CCRD all clear the AC.  CCRD also loads the AC ...
  assign ioc0 = iowr & selected & (`isCCCM|`isCCWD|`isCCRD);
  assign ioc1 = iowr & selected & `isCCRD;

  // CCRD reads the data in - the last byte that came back from the DS1302 ...
  assign dx = (iord & selected & `isCCRD) ? {4'b0, din} : 12'bz;

  // Define the initial state for dout ...
  initial
    dout = 0;

  // CCWR writes the data out - the next byte to be sent to the DS1302 ...
  always @(posedge clock)
    if (iowr & selected & `isCCWD)
      dout <= dx[4:11];

  // CCSF skips when the state machine is idle ...
  assign ioskip = iowr & selected & `isCCSF & idle;

  //    The DS1302 serial clock can't run at our full CPU clock speed, so
  // create a slowed down clock that we'll use for pacing the state machine.
  rtc_timing #(.SYSTEM_CLOCK(SYSTEM_CLOCK))
    timing (.clock(clock), .sclk_tick(sclk_tick));
  
  // Create the DS1302 I/O state machine and we're done ...
  // Note that the command register does not exist as a separate entity - we
  // assert "start" during the iowr part of CCCM, and the data from dx[5:11]
  // is loaded directly into the transmitter shift register.
  rtc_control control (
    .clock(clock), .cmd(dx[5:11]), .din(din), .dout(dout), .ioclr(ioclr),
    .start(iowr & selected & `isCCCM), .idle(idle), .sclk_tick(sclk_tick),
    .rtc_sclk(rtc_sclk), .rtc_ce(rtc_ce), .rtc_sio(rtc_sio)
  );
endmodule // rtc_1302
