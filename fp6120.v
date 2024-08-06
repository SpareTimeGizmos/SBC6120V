//++
//fp6120.v - FP6120 compatible front panel interface
//Copyright (C) 2011-2014 by Spare Time Gizmos.  All rights reserved.
//
// DESCRIPTION:
//   This module implements an interface to a front panel (you remember, those
// things with all the lights and switches!) that's compatible with the FP6120
// used on the original SBC6120.  Physically and functionally it looks a lot
// like a PDP-8/E front panel, however the electrical interface is completely
// different.  The FP6120 is basically a lights and switches I/O device, with
// IOTs that load the displays and read the switch states.  It's nothing like
// the way you'd normally think on implementing a front panel for a CPU, but
// remember that the HD6120 is a 40 pin DIP microprocessor.  It doesn't have
// enough pins for a real front panel, and it was always designed to have a
// "software" front panel using (what else??) panel mode.
//
//   Yes, in the Verilog implementation we have access to the inside of the
// HD6120 and all its internal registers and we could actually implement a much
// more traditional front panel.  That's not what this module does, though -
// the goal here is to be software compatible with the original SBC6120 front
// panel, the FP6120.
//
// The original FP6120 IOTs implemented by this interface are -
//
//   6430 CCPR   - clear AC, HALT SW REQ and 30Hz REQ flags
//   6431 SHSW   - skip on HALT SW REQ flag
//   6432 SPLK   - skip on panel lock
//   6433 SCPT   - skip on 30Hz timer REQ flag
//   6434 RFNS   - read function switches (BOOT, EXAM, DEP, ROTSW, etc)
//   6435 RLOF   - turn RUN LED off
//   6437 RLON   - turn RUN LED on
//
// Plus these two new IOTs that exist only on the SBC6120-RC model -
//
//   6415 PLOF - turn POWER LED off
//   6417 PLON - turn POWER LED on 
//
//   And just like the original FP6120, this module generates a 30Hz clock
// that causes a control panel trap every 33.3ms.  The BTS6120 firmware uses
// this to update the front panel while a main memory program is running.
//
//   Lastly, if you're looking for the actual front panel - the logic that
// talks to the LEDs, push buttons, switches, whatever - then you're in the
// wrong place.  This module just decodes the SBC6120 side of the interface
// and produces a set of discrete inputs and outputs that could be interfaced
// directly to the LEDs and switches.  That actual interface, however, is
// platform dependent and you'll find it elsewhere.
//
// REVISION HISTORY:
// 15-Feb-11  RLA  New file.
//  1-Jan-14  RLA  Eliminate the system wide reset input.  Add an initial block
//		   to define the initial state where required.
//--
//000000001111111111222222222233333333334444444444555555555566666666667777777777
//234567890123456789012345678901234567890123456789012345678901234567890123456789
`include "sbc6120_h.v"          // global declarations for this project
`include "opcodes_h.v"          // PDP-8/HD6120 opcode mnemonics


module fp6120
  //++
  //--
#(
  parameter SYSTEM_CLOCK  = 25_000_000, // clock frequency (in Hz!)
  parameter SELECT_FP6120 = 6'O43,      // select code for FP6120 IOTs
  parameter SELECT_RC     = 6'O41       // select code for "extra" -RC IOTs
) (
  // Bus interface signals ...       
  input              clock,     // system wide clock
  input [`EMA]       ema,       // extended memory address
  input [`WORD]      ax,        // address bus (also carries the IOT)
  inout [`WORD]      dx,        // bidirectional data bus
  input              mrd,       // memory read strobe
  input              mwr,       // memory write strobe
  input              ifetch,    // the current memory read is an instruction
  input              cpmem,     // access control panel memory
  input              iowr,      // I/O write strobe
  input              iord,      // I/O read strobe
  input		     ioclr,	// clear all I/O devices (CAF)
  output             ioskip,    // asserted if this IOT should skip
  output             ioc0,      // asserted if this IOT should clear the AC
  output             ioc1,      // asserted if this IOT should load the AC
  output             cpreq,     // control panel trap request
  // Front panel connections ...
  input [2:0]        post,      // POST code from post module
  input      [`WORD] dispdata,  // display data from WSR IOT
  output     [`WORD] data_leds, // data display LEDs
  output reg [`WORD] addr_leds, // address display LEDs
  output reg [`EMA]  ema_leds,  // EMA display LEDs
  output reg         run_led,   // RUN LED
  output reg         power_led, // POWER LED
  input [`FNSW_BUS]  fnsw,      // function switches (9 of them)
  input [`ROTSW_BUS] rotsw      // rotary switch setting
);
  reg [`WORD] mdreg;
  wire cp_timer_tick;  reg cp_timer_req;
  reg last_halt_sw, halt_sw_req;
  wire selected    = ax[3:8] == SELECT_FP6120;
  wire selected_rc = ax[3:8] == SELECT_RC;

  // Define the initial state for everything ...
  initial begin
    run_led = 0;  power_led = 1;
    addr_leds = 12'o0;  ema_leds = 3'o0;  mdreg = 12'o0;
    cp_timer_req = 0;  halt_sw_req = 0;
  end
  
  // The RUN LED is simply turned on or off by these IOTs...
  always @(posedge clock)
    if (iowr & selected & `isRLOF)
      run_led <= 0;
    else if (iowr & selected & `isRLON)
      run_led <= 1;

  // The -RC POWER LED is nearly the same, except for the IOTs used.
  always @(posedge clock)
    if (iowr & selected_rc & `isPLOF)
      power_led <= 0;
    else if (iowr & selected_rc & `isPLON)
      power_led <= 1;

  //   The address and EMA LEDs always record the address used by the last main
  // memory access.  They're clocked by any mrd or mwr when cpmem is NOT true.
  always @(posedge clock)
    if ((mrd | mwr) & !cpmem) begin
      addr_leds <= ax;  ema_leds <= ema;
    end

  //   If the rotary switch is set to the "MD DISP" position, then the data LEDs
  // (like address and ema) always reflect the last main memory access. They're
  // clocked by any memory read or write when cpmem is NOT asserted.  If the
  // rotary switch is set to "POST", then the data LEDs ahow the most recent POST
  // code.  However, if the rotary switch is set to any other position, then the
  // data LEDs are loaded explicitly by the WSR instruction and nothing else
  // affects them ...
  //
  //   This is implemented as a twelve bit register (to latch the memory data)
  // and a MUX to select one of the three display sources ...
  always @(posedge clock)
    if ((mrd | mwr) & !cpmem) mdreg <= dx;
  assign data_leds =   rotsw[`ROTSW_MD]   ? mdreg
                     : rotsw[`ROTSW_POST] ? {9'b0, post}
                     : dispdata;

  // The cp_timer_req FF is set by the 30Hz clock and cleared by CCPR.
  ck_divider #(.IN_FREQ(SYSTEM_CLOCK), .OUT_FREQ(30))
    cptimer(.clock(clock), .out(cp_timer_tick));
  always @(posedge clock)
    if (iowr & selected & `isCCPR)
      cp_timer_req <= 0;
    else if (cp_timer_tick)
      cp_timer_req <= 1;

  //   The halt_sw_req FF is set by the rising edge of HALT/-RUN (i.e. when the
  // switch is flipped from the "RUN" to the "HALT" position).  It's cleared
  // by the CCPR IOT ...
  always @(posedge clock)
    last_halt_sw <= fnsw[`FNSW_HALT];
  always @(posedge clock)
    if (iowr & selected & `isCCPR)
      halt_sw_req <= 0;
    else if (fnsw[`FNSW_HALT] & !last_halt_sw)
      halt_sw_req <= 1;

  //   cpreq is asserted whenever we fetch an instruction from main memory (we
  // DON'T want to assert cpreq if we've already trapped to panel mode!) AND
  // either a) the HALT switch was flipped and the panel's not locked (if the
  // panel's locked, all switches are ignored) OR b) the 30Hz timer ticks and
  // the rotary switch is NOT set to "MD DISP".  The latter condition is because
  // if the rotsw IS set to MD DISP, then the hardware (see the data LEDs case
  // above) is taking care of updating the data display and there's no need to
  // trap to BTS6120.
  assign cpreq = ifetch & mrd & !cpmem & (
                      (halt_sw_req & !fnsw[`FNSW_LOCK])
                    | (cp_timer_req & !rotsw[`ROTSW_MD]) );

  //   The SHSW (skip on halt sw req), SCPT (skip on cp timer req), and SPLK
  // (skip on panel lock) IOTs skip if the associated flip-flop is set.
  assign ioskip = iowr & selected & (
                      (`isSHSW & halt_sw_req)
                    | (`isSCPT & cp_timer_req)
                    | (`isSPLK & fnsw[`FNSW_LOCK]) );

  // CCPR and RFNS both clear the AC, and RFNS also loads the AC.
  assign ioc0 = iowr & selected & (`isCCPR | `isRFNS);
  assign ioc1 = iowr & selected & `isRFNS;

  //   The dx bus is driven with the current state of all the function and
  // rotary switches for the RFNS IOT.
  assign dx = (iord & selected & `isRFNS) ? {fnsw[0:7], rotsw[0:3]} : 12'bz;

endmodule // fp6120
