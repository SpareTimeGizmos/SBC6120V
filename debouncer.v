//++
//debouncer.v - debounce mechanical switch inputs
//Copyright (C) 2011-2014 by Spare Time Gizmos.  All rights reserved.
//
// DESCRIPTION:
//   This module contains a simple debouncer for mechanical switches.  It can
// generate an output for the current state of the switch and two separate
// on/off outputs that are asserted for exactly one clock tick when the switch
// changes state.  The latter can also be used as "button pressed" and "button
// released" signals if your switch happens to be a push button.
//
// REVISION HISTORY:
// 09-Feb-11  RLA  New file.
//  1-Jan-14  RLA  Replace global reset with initial block.
//--
//000000001111111111222222222233333333334444444444555555555566666666667777777777
//234567890123456789012345678901234567890123456789012345678901234567890123456789

module debouncer #(
  parameter LOG2_DELAY   = 16,	// LOG2(debounce delay), in clock cycles
  parameter ACTIVE_STATE = 1	// 1 -> switch input is high when switch is "on"
) (
  input  clock,			// system clock
  input  switch,		// raw input from the switch
  output reg state,		// current, debounced, state
  output on, 			// true for 1 tick when the switch turns "on"
  output off			//  "    "  "   "    "   "    "      "   "off"
);
  reg [LOG2_DELAY-1:0] count;
  initial begin 
    count = 0;  state = 0;
  end
   
  //   Synchronize the raw switch input to our clock domain and then derive two
  // new signals - the current state of the switch (allowing for whether its
  // normally active high or active low), and sw_changed, which is true when
  // the current switch state is not the same as the debounced state.
  wire sync_sw, sw_now, sw_changed;
  synchronize_level sl (.clock(clock), .in(switch), .out(sync_sw));
  assign sw_now = sync_sw ^ ~ACTIVE_STATE;
  assign sw_changed = sw_now != state;

  //  The idea is simple - whenever the switch changes state, start counting
  // clocks.  When the counter maxes out, change the debounced state.  If the
  // switch bounces before the counter maxes out, then the debounced state never
  // changes and we start counting again from zero.
  always @(posedge clock)
    if (!sw_changed) begin
      count <= 0;
    end else begin
      count <= count + 1;
      if (&count) state <= ~state;         
    end

  //   The "on" and "off" outputs are asserted for just one clock cycle (that
  // final tick when the count rolls over) when the switch changes state.
  assign on  =  sw_now & sw_changed & &count;
  assign off = ~sw_now & sw_changed & &count;
endmodule // debouncer
