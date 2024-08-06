//++
//uart.v - Simplified 6402 style UART implementation
//Copyright (C) 2011-2014 by Spare Time Gizmos.  All rights reserved.
//
// DESCRIPTION:
//   These modules implement a simplified 6402 style UART.  The interface and
// port names are pretty much identical in name and function to the real 6402,
// however this version has a fixed 8N1 data format.  It detects overrun and
// framing errors on receive, however it neither transmits nor checks parity.
// On the other hand, it does have a built in baud rate generator which the real
// 6402 can't match, so this version is not a complete loss!
//
//   Plus, if SIMULATION is defined, it generates some simple code that will
// mimic a UART and echo any characters send to the transmitter buffer to
// stdout.  Unfortunately the simulation version is output only - there's
// currently no way to type on stdin and send the data to the receiver buffer.
//
// REVISION HISTORY:
// 17-Jan-11  RLA  New file.
// 16-Feb-11  RLA  Remove the baud rate generator module.  Just use the generic
//		   ck_divider module instead.
//  1-Jan-14  RLA  Make reset synchronous.  Add initial block to initialize all
//		   flip-flops so an initial reset isn't necessary.
//--
//000000001111111111222222222233333333334444444444555555555566666666667777777777
//234567890123456789012345678901234567890123456789012345678901234567890123456789


module uart_rx (
  //++
  //  This is the basic UART receiver - it's a little state machine that watches
  // the serial data for a start bit, assembles 8 data bits, and checks for a
  // valid stop bit.  It's pretty classic.  Note that its NOT BUFFERED, so the
  // rxdata is only valid for an instant while rxdone is asserted.  It's assumed
  // that the parent module will implement some kind of buffer for it, probably
  // something like the uart_rxbuf module that you'll find later.
  //--
  input clock,			// master clock
  input reset,			// synchronous reset
  input baudclk,		// baud rate clock
  input rxd,			// serial data input
  output reg rxdone,		// asserted when rxdata is valid
  output reg fe,		// framing error (break) detected
  output reg [7:0] rxdata	// assembled data byte, ready to use
);
  // Receiver state definitions ...
  localparam [2:0]
    IDLE	= 3'b000,	// waiting for start bit
    START	= 3'b001,	// receiving start bit
    DATA	= 3'b010,	// receiving data bits
    STOP	= 3'b011,	// receiving stop bit
    RESYNC	= 3'b100;	// waiting for rxd high

  // Locals ...
  reg [3:0] ticks, next_ticks;  // counts baud rate clock ticks per bit
  reg [2:0] bits, next_bits;	// counts data bits received
  reg [2:0] state, next_state;	// current state, next state
  reg [7:0] next_rxdata;	// next value for rxdata
  reg next_fe;			// next value for framing error

  // Define the initial state for all flip flops ...
  initial begin
    ticks = 0;  bits = 0;  rxdata = 8'b0;  state = IDLE;  fe = 0;
  end
  
  // Synthesize the state flip flops ...
  always @(posedge clock)
    if (reset) begin
      ticks <= 0;  bits <= 0;  rxdata <= 8'b0;  state <= IDLE;  fe <= 0;
    end else begin
      ticks <= next_ticks;  bits <= next_bits;  fe <= next_fe;
      rxdata <= next_rxdata;  state <= next_state;
    end

  // Combinatorial logic to determine the next state of the FSM ...
  always @* begin
    // These are the "default" values for the next state of everything...
    next_state = state;  next_ticks = ticks;  next_bits = bits;
    next_rxdata = rxdata;  next_fe = fe;  rxdone = 0;

    case (state)
      //   Wait for rxd to go low and, when it does, clear the tick clock
      // and go to the START state next...
      IDLE: if (~rxd) begin   next_state = START;  next_ticks = 0;  end

      //   Count for seven more ticks (to put our sampling window into the
      // middle of the bit time) and then go to the DATA state...
      START: if (baudclk) begin
	if (ticks == 7) begin
	  next_state = DATA;  next_ticks = 0;
	  next_bits = 0;  next_rxdata = 8'b0;
	end else
	  next_ticks = ticks + 1'b1;
      end

      // Receive eight data bits and shift them into the rxdata register...
      DATA: if (baudclk) begin
	if (ticks == 15) begin
	  next_ticks = 0;  next_rxdata = {rxd, rxdata[7:1]};
	  if (bits == 7)  next_state = STOP;  else next_bits = bits + 1'b1;
	end else
	  next_ticks = ticks + 1'b1;
      end

      //   Wait for the middle of the stop bit and sample rxd. If it's 1 then
      // we're done; otherwise we have a framing error...
      STOP: if (baudclk) begin
	if (ticks == 15) begin
	  if (rxd) begin
	    rxdone = 1;  next_fe = 0;  next_state = IDLE;
	  end else begin
	    rxdone = 0;  next_fe = 1;  next_state = RESYNC;
	  end
	end else
	  next_ticks = ticks + 1'b1;
      end

      // After a framing error, wait for rxd high before returning to IDLE.
      RESYNC: if (rxd) next_state = IDLE;
    endcase // case (state)
  end // always @ *
endmodule // uart_rx


module uart_rxbuf (
  //++
  //  This module implements a simple single buffer for the receiver, pretty
  // much the same as what's actually in the 6402.  This allows the receiver to
  // work on assembling the next byte without destroying the last one, and gives
  // the host an entire character time to respond to data ready.  You could, if
  // you wanted to be fancy, replace this with a 16450 style FIFO some day...
  //--
  input clock,			// master clock
  input reset,			// synchronous reset
  input rxdone,			// asserted by uart_rx when rxdata is valid
  input [7:0] rxdata,		// assembled data byte from uart_rx
  output reg [7:0] rbr,		// received data buffer
  output reg dr,		// data received flag
  input drr,			// reset data received (dr) flag
  output reg oe			// overrun error
);
  reg [7:0] next_rbr;  reg next_dr, next_oe;

  // Define the initial state for all flip flops ...
  initial begin
    rbr = 8'b0;  dr = 0;  oe = 0;
  end
  
  // Synthesize the buffer register, dr and oe flipflops...
  always @(posedge clock)
    if (reset) begin
      rbr <= 8'b0;  dr <= 0;  oe <= 0;
    end else begin
      rbr <= next_rbr;  dr <= next_dr;  oe <= next_oe;
    end

  // Next state logic ...
  always @* begin
    next_rbr = rbr;  next_dr = dr;  next_oe = oe;

    //   If the receiver is done, transfer the data to the buffer and set the
    // dr (data ready) flag.  If dr is already set then also set oe (overrun
    // error) UNLESS rxdone, and drr just happen to occur in the same clock -
    // then the new byte replaces the old one and there's no overrun.
    if (rxdone) begin
      next_rbr = rxdata;  next_dr = 1;
      if (dr & !drr) next_oe = 1;
    end 

    //   If drr (data received reset) is asserted, then clear the dr FF UNLESS
    // rxdone also just happens to be set in the same clock.  drr always clears
    // oe (overrun error), regardless.
    if (drr) begin
      if (!rxdone) next_dr = 0;
      next_oe = 0;
    end
  end
endmodule // uart_rxbuf


module uart_tx (
  //++
  //  This module implements the transmitting side of the UART.  It's another
  // state machine that sends a start bit, eight data bits, and a stop bit.
  // The shift register serves as a pseudo-buffer so that the txdata wire do
  // not have to remain stable for the entire duration of the character,
  // however it's still useful to have another buffer so that the host can
  // output a second byte while the first one is being transmitted.  The
  // uart_txbuf module, down below, is perfect for the job.
  //--
  input clock,			// master clock
  input reset,			// synchronous reset
  input baudclk,		// baud rate clock
  input txstart,		// txdata is valid, start transmitter
  input [7:0] txdata,		// data byte to transmit
  output reg txidle,		// transmitter is idle
  output reg txd		// serial data output
);
  // Transmitter state definitions ...
  localparam [1:0]
    IDLE	= 2'b00,	// waiting for txstart
    START	= 2'b01,	// sending start bit
    DATA	= 2'b10,	// sending data bits
    STOP	= 2'b11;	// sending stop bit

  // Locals ...
  reg [7:0] txreg, next_txreg;	// transmitter shift register
  reg [3:0] ticks, next_ticks;  // counts baud rate clock ticks per bit
  reg [2:0] bits, next_bits;	// counts data bits sent
  reg [1:0] state, next_state;	// current state, next state
  reg next_txd;			// next state for txd

  // Define the initial state ...
  initial begin
    ticks = 0;  bits = 0;  txreg = 8'b0;  state = IDLE;  txd = 1;
  end
  
  // Synthesize the state flip flops ...
  always @(posedge clock)
    if (reset) begin
      ticks <= 0;  bits <= 0;  txreg <= 8'b0;  state <= IDLE;  txd <= 1;
    end else begin
      ticks <= next_ticks;  bits <= next_bits;  txd <= next_txd;
      txreg <= next_txreg;  state <= next_state;
    end

  // Combinatorial logic to determine the next state of the FSM ...
  always @* begin
    // These are the "default" values for the next state of everything...
    next_state = state;  next_ticks = ticks;  next_bits = bits;
    next_txreg = txreg;  next_txd = txd;  txidle = 0;

    case (state)
      // Wait for a tick on txstart and then load txreg ...
      IDLE: begin
	next_txd = 1;  txidle = 1;
	if (txstart) begin
	  next_state = START;  next_ticks = 0;  next_txreg = txdata;
	end
      end

      // Send the start bit ...
      START: begin
	next_txd = 0;
	if (baudclk) begin
	  if (ticks == 15) begin
	    next_state = DATA;  next_ticks = 0;  next_bits = 0;
	  end else
	    next_ticks = ticks + 1'b1;
	end
      end

      // Send eight data bits, one at a time ...
      DATA: begin
	next_txd = txreg[0];
	if (baudclk) begin
	  if (ticks == 15) begin
	    next_ticks = 0;  next_txreg = txreg >> 1;
	    if (bits == 7)
	      next_state = STOP;
	    else
	      next_bits = bits + 1'b1;
	  end else
	    next_ticks = ticks + 1'b1;
	end
      end

      // Send the stop bit and go back to idle ...
      STOP: begin
	next_txd = 1;
	if (baudclk) begin
	  if (ticks == 15) 
	    next_state = IDLE;
	  else
	    next_ticks = ticks + 1'b1;
	end
      end
    endcase // case (state)
  end // always @ *
endmodule // uart_tx


module uart_txbuf (
  //++
  //   This module implements a simple single buffer for the transmitter along
  // with the associated buffer empty flag and it connects to the uart_tx module
  // by the txidle and txstart flags.  If you wanted to be fancy, you could
  // enhance this to include a FIFO, but I think that's overkill.
  //--
  input clock,			// master clock
  input reset,			// synchronous reset
  input txidle,			// uart_tx is idle, ready for txstart
  output reg txstart,		// start transmitter with data in tbr
  output reg [7:0] tbr,		// transmitter buffer register
  output reg tbre,		// buffer register empty, ready for tbrl
  input tbrl,			// load transmitter buffer
  input [7:0] tbr_in		// data to be loaded into the buffer
);
  reg [7:0] next_tbr;  reg next_tbre;

  // Define the initial state ...
  initial begin
    tbr = 8'b0;  tbre = 1;
  end
  
  // Synthesize the buffer and buffer empty flipflops...
  always @(posedge clock)
    if (reset) begin
      tbr <= 8'b0;  tbre <= 1;
    end else begin
      tbr <= next_tbr;  tbre <= next_tbre;
    end

  // Next state logic ...
  always @* begin
    next_tbr = tbr;  next_tbre = tbre;  txstart = 0;

    if (txidle & !tbre) begin
      txstart = 1;
      if (!tbrl) next_tbre = 1;
    end

    if (tbrl) begin
      next_tbr = tbr_in;  next_tbre = 0;
    end
  end
endmodule // uart_txbuf


module uart (
  //++
  //   And this module ties it all together to make a complete 6402 UART, except
  // for the baud rate generator.  The latter is easy to create since there's
  // a nice module for it up above, but it's still up to you to connect them.
  //--
  // Global signals ...
  input        clock,		// master clock for all operations
  input        reset,		// synchronous reset of registers and state
  // Receiver connections ...
  input	       rrc,		// receiver baud clock
  input        rri,		// serial data input
  output       dr,		// data received flag
  input        drr,		// reset data received (dr) flag
  output [7:0] rbr,		// received data buffer
  output       fe,		// framing error (break received)
  output       oe,		// overrun error
  // Transmitter connections ...
  input	       trc,		// transmitter baud clock
  output       tro,		// serial data output
  output       tre,		// transmitter register is empty (idle)
  output       tbre,		// transmitter buffer is empty
  input        tbrl,		// load transmitter buffer
  input [7:0]  tbr		// transmitted data buffer
);
  // Implement the receiver side ...
  wire [7:0] rxdata;  wire rxdone;
  uart_rx rx (
    .clock(clock), .reset(reset), .baudclk(rrc), 
    .rxd(rri), .fe(fe), .rxdone(rxdone), .rxdata(rxdata)
  );
  uart_rxbuf rxbuf (
  .clock(clock), .reset(reset), .rxdone(rxdone), .rxdata(rxdata),
  .rbr(rbr), .dr(dr), .drr(drr), .oe(oe)
  );

  // And the transmitter side ...
  wire [7:0] txdata;  wire txstart, txidle;
  uart_txbuf txbuf (
    .clock(clock), .reset(reset), .txidle(txidle), .txstart(txstart), 
    .tbr(txdata), .tbre(tbre), .tbrl(tbrl), .tbr_in(tbr)
  );
  uart_tx tx (
    .clock(clock), .reset(reset), .baudclk(trc), 
    .txstart(txstart), .txdata(txdata), .txidle(txidle), .txd(tro)
  );
  //   Avoid a 1 clock "glitch" on tre while we transfer data from the txbuf
  // to the transmitter...
  assign tre = txidle & tbre;
endmodule
