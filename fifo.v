//++
//fifo.v - simple, basic, first in - first out buffer
//Copyright (C) 2007-2014 by Spare Time Gizmos.  All rights reserved.
//
// DESCRIPTION:
//   This module creates a basic first in - first out buffer, which is useful
// for all kinds of things.  It provides separate data in and data out busses,
// with read and write strobes, as well as buffer full and buffer empty status
// outputs.  Writing when then buffer is already full will cause data to be
// lost, and reading when the buffer is empty will return garbage.
//
// REVISION HISTORY:
// 15-Jul-07  RLA  New file.
//  1-Jan-14  RLA  Make reset input synchronous.  Add initial block to
//		   initialize all counters (so reset isn't required).
//--
//000000011111111112222222222333333333344444444445555555555666666666677777777778
//345678901234567890123456789012345678901234567890123456789012345678901234567890


module fifo
#(
  parameter LOG2_DEPTH = 4,	// Log2(FIFO size), in bytes or words
  parameter DATA_WIDTH = 8	// data bus width, in bits
) (
  input  clock,				// system wide clock
  input  reset,				// synchronous reset to empty
  input  [DATA_WIDTH-1:0] din,		// data input bus for writing
  input  write,				// strobe to store data
  output [DATA_WIDTH-1:0] dout,		// data output bus for reading
  input  read,				// strobe to read data
  output reg full,			// the FIFO is full
  output reg empty,			//  "    "   " empty
  //   Note that count can range from zero to 2**LOG2_DEPTH (in the case where
  // the FIFO is completely full) and it therefore needs an extra bit.  So this
  // is correct and it's not LOG2_DEPTH-1 !
  output reg [LOG2_DEPTH:0] count	// number of bytes/words used
);
  // Locals ...
  reg [DATA_WIDTH-1:0] data [2**LOG2_DEPTH-1:0]; 
  reg [LOG2_DEPTH-1:0] wr_ptr, next_wr_ptr, succ_wr_ptr;
  reg [LOG2_DEPTH-1:0] rd_ptr, next_rd_ptr, succ_rd_ptr;
  reg [LOG2_DEPTH:0] next_count;
  reg next_full, next_empty;

  // Define the initial state of all counters ...
  initial begin
    wr_ptr = 0;  rd_ptr = 0;  full = 0;  empty = 1;  count = 0;
  end
  
  // Write to the FIFO any time write is asserted and the FIFO is not full ...
  always @(posedge clock)
    if (write & ~full) data[wr_ptr] <= din;

  // And dout is always the top item in the FIFO ...
  assign dout = data[rd_ptr];

  // Synthesize the state registers ..  
  always @(posedge clock)
    if (reset) begin
      wr_ptr <= 0;  rd_ptr <= 0;  full <= 0;  empty <= 1;  count <= 0;
    end else begin
      wr_ptr <= next_wr_ptr;  rd_ptr <= next_rd_ptr;
      full <= next_full;  empty <= next_empty;
      count <= next_count;
    end

  // And here's the nex state logic for read, write and both ...
  always @* begin
    next_wr_ptr = wr_ptr;  succ_wr_ptr = wr_ptr + 1'b1;
    next_rd_ptr = rd_ptr;  succ_rd_ptr = rd_ptr + 1'b1;  
    next_full = full;  next_empty = empty;  next_count = count;

    case ({write, read})
      // Neither read nor write - how hard can that be??
      //2'b00:  ;

      // Read operation only ...
      2'b01: begin
        if (~empty) begin
          next_rd_ptr = succ_rd_ptr;  next_full = 0;
          if (succ_rd_ptr == wr_ptr) next_empty = 1;
	  next_count = count - 1'b1;
        end
      end // case: 2'b01

      // Write operation only ...
      2'b10: begin
        if (~full) begin
          next_wr_ptr = succ_wr_ptr;  next_empty = 0;
          if (succ_wr_ptr == rd_ptr) next_full = 1;
	  next_count = count + 1'b1;
        end
      end // case: 2'b10

      // Both read and write ...
      2'b11: begin
        next_wr_ptr = succ_wr_ptr;  next_rd_ptr = succ_rd_ptr;
      end // case: 2'b11
    endcase // case ({write, read})
  end // always @ *
endmodule // fifo
