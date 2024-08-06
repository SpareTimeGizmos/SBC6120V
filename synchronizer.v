//++
//synchronizer.v - Synchronize asynchronous and cross clock domain signals
//Copyright (C) 2011-2014 by Spare Time Gizmos.  All rights reserved.
//
// DESCRIPTION:
//   This module contains a couple of simple modules for synchronizing signals
// that cross clock domains, or  for synchronizing asynchronous signals.
// They're all simple and classic, and just double register the input to reduce
// the odds of the output becoming metastable, however there are a few Xilinx
// specific optimizations that can be applied.  The XST directives and comments
// you see below are stolen straight from the Xilinx examples.
//
// REVISION HISTORY:
// 09-Feb-11  RLA  New file.
// 29-Dec-13  RLA  Remove HBLKNM attribute - it's more trouble than it's worth.
//  1-Jan-14  RLA  Replace global reset with initial block.
//--
//000000001111111111222222222233333333334444444444555555555566666666667777777777
//234567890123456789012345678901234567890123456789012345678901234567890123456789

module synchronize_level
  //++
  //   This module synchronizes "level" sensitive signals - i.e. signals that
  // change fairly slowly when compared with the clock.  Don't use it for
  // "short" or "fast" signals - i.e. things that are one tick wide - because
  // they may disappear entirely.
  //--
(
  input clock,			// the "new" (i.e. output domain) clock
  // Note -
  //  TIG="TRUE"  - Specifies a timing ignore for the asynchronous input
  //  IOB="FALSE" - Specifies to not place the register into the IOB allowing 
  //                both synchronization registers to exist in the same slice 
  //                allowing for the shortest propagation time between them
  (* TIG="TRUE", IOB="FALSE" *)
  input in,			// input signal
  output reg out		// and output signal
);

  // Note -
  //  SHIFT_EXTRACT="NO" - Specifies to the synthesis tool to not infer an SRL
  //  HBLKNM="sync_reg" - Specifies to pack both registers into the same slice
  (* SHIFT_EXTRACT="NO" *) reg [1:0] sreg;
 
  // Define the initial state of all flip-flops ...
  initial begin
    out = 0;  sreg = 2'b0;
  end

  //   Just register the signal twice (three times if you count the output reg)
  // and hope for the best ..
  always @(posedge clock) begin
    out <= sreg[1];  sreg <= {sreg[0], in};
  end
endmodule // synchronize_level


module synchronize_tick
  //++
  //   This module is similar to synchronize_level, except that this one is
  // specifically intended for signals that are very short, say only one clock
  // tick wide.  It guarantees that a) the tick never completely disappears,
  // and b) the output signal in the new clock domain is also exactly one
  // clock tick wide.
  //
  //   The basic idea is to convert input ticks into a signal that toggles
  // on each tick, then use the previous technique to synchronize that signal,
  // and then convert the edges to ticks in the new clock domain.  Easy when
  // you know the trick :-)
  //--
(
  input  clock_in,		// the clock for the input tick
  input  in,			// and the tick to be synchronized
  output clock_out,		// clock for the output tick
  output out			// and of course the output
);
  reg flag;
  (* SHIFT_EXTRACT="NO" *) reg [2:0] sreg;

  // Define the initial state of all flip-flops ...
  initial begin
    flag = 0;  sreg = 3'b0;
  end
  
  // Flag toggles for each tick on "in" ...
  always @(posedge clock_in) begin
    if (in) flag <= ~flag;
  end

  // Sync flag to clock_out ...
  always @(posedge clock_out) begin
    sreg <= {sreg[1:0], flag};
  end

  // And create an output tick whenever flag changes state ...
  assign out = sreg[2] ^ sreg[1];
endmodule // synchronize_tick
