//++
//divider.v - simple clock divider/timer
//Copyright (C) 2011-2014 by Spare Time Gizmos.  All rights reserved.
//
// DESCRIPTION:
//   This module implements a simple clock divider/timer unit.  It counts
// incoming clocks and produces a one tick wide output pulse at the desired
// frequency.
//
// REVISION HISTORY:
// 16-Feb-11  RLA  New file.
//  1-Jan-14  RLA  Replace global reset with initial block.
//--
//000000001111111111222222222233333333334444444444555555555566666666667777777777
//234567890123456789012345678901234567890123456789012345678901234567890123456789


module ck_divider #(
  parameter IN_FREQ  = 0,	// clock frequency (in Hz!)
  parameter OUT_FREQ = 0	// desired output frequency
) (
  input     clock,		// master clock 
  output    out			// 1 clock wide output tick
);
`include "clog2.v"
  localparam
   DIVISOR = IN_FREQ/OUT_FREQ,
   NBITS   = clog2(DIVISOR);
  reg [NBITS-1:0] count;

  initial
    count = 0;
  
  always @(posedge clock)
    if (count == (DIVISOR-1))
      count <= 0;
    else
      count <= count + 1'b1;

  assign out = (count == (DIVISOR-1));
endmodule // ck_divider

