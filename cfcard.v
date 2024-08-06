//++
//cfcard.v
//
//   SBC6120 COMPACT FLASH CARD INTERFACE
//   Copyright (C) 2011 by Spare Time Gizmos.  All rights reserved.
//
// DESIGN NAME:	SBC6120V
// DESCRIPTION:
//   This module implements PDP-8 interface to CompactFlash memory cards using
// the Common Memory Model interface.  This interface is not actually exactly
// compatible with any SBC6120 interface, although it is similar to the Compact
// Flash interface that was part of the IOB6120 board.  The main difference is
// that the IOB6120 memory mapped the CF registers into RAM disk space, and this
// interface I/O maps the registers using IOT instructions.  Since the Compact
// Flash card interface is still basically the IDE/ATA register and command set,
// this interface is not unlike an original SBC6120 with a CF card adapter
// plugged into the IDE port. The same firmware will work here, with some minor
// changes to the IOTs used.
//
//   FWIW, the Common Memory Model mode is hot swappable (unlike the TrueIDE
// CF mode, which is not) so the possibility exists to allow users to hot swap
// cards, if the software wants to support it.
//
//   The CompactFlash card interface uses three groups of eight IOTs and has
// three different device select addresses.  The first group of IOTS, called
// SELECT_R in this interface, all read the corresponding CompactFlash register
// (e.g. IOT 6550 reads the CF/IDE/ATA data register, 6557 reads the status
// register, etc) and the second group, SELECT_W, all write registers (e.g. IOT
// 6567 writes the CF/IDE/ATA command register, 6563 writes LB0, etc). Although
// this interface implements all 16 R/W and register combinations, only a few
// of them are actually used by the software.  The IOTs and mnemonics used by
// BTS6120 are -
//
//   6550 CFRDAT - read data register
//   6560 CFWDAT - write data register
//   6551 CFRERR - read error register
//   6562 CFWCNT - write sector count
//   6563 CFWLB0 - write LBA byte 0 (or sector number)
//   6564 CFWLB1 - write LBA byte 1 (or cylinder low)
//   6565 CFWLB2 - write LBA byte 2 (or cylinder high)
//   6566 CFWLB3 - write LBA byte 3 (or device/head)
//   6557 CFRSTS - read status register
//   6567 CFWCMD - write command register
//
//   The Common Memory Model interface is only eight bits wide and all these
// IOTs transfer only eight bits, including CFRDAT/CFWDAT.  The CF interface is
// specified such that CFRDAT/CFWDAT will read/write sixteen bit data words low
// byte first, then high byte.  AC bits 0-3 are always returned as zeros on
// reads and are ignored for writes.
//
//   The third group performs assorted interface control functions -
//
//   6570 CFICTL - CompactFlash card interface control (see below)
//   6571 CFISRQ - skip if the interface is interrupting
//   6572 CFSRDY - skip on card ready flag
//   ... IOTs 6573 thru 6576 are currently unused ...
//   6577 CFISTS - read card interface status (see below)
//
//   The 6572 IOT, CFSRDY, skips if the ready output is asserted by the CF
// card.  The ready output is defined in the CFA and IDE/ATA specifications.
// A low to high transition on the ready bit also causes an interrupt if
// interrupts are enabled.
//
//   The 6577 IOT, CFISTS, returns the current status of the CompactFlash card
// interface (this is different from the card's status, which the software will
// have to get via CFRSTS).  The CFISTS IOT returns these bits in the AC -
//
//   AC0  = 1 --> card is inserted now
//   AC1  = 1 --> card has been changed
//   AC2  = 1 --> reset is currently asserted
//   AC[3:5]  --> current interface delay
//   ... AC[6:9] are currently unused ...
//   AC10 = 1 --> card ready output is asserted
//   AC11 = 1 --> interrupts are enabled
//
//   AC0, "card inserted,"  is a one if a card is currently inserted into the
// slot.  This tests a simple mechanical contact on the socket and indicates
// whether a card is even present.  The "card changed" bit, AC1, is set when-
// ever a change in the card inserted status is detected.  This bit remains
// set, no matter how many times the card may be changed, until it's explicitly
// cleared by the software with the CFICTL IOT.  The card changed bit allows
// the software to tell when the user has hot swapped a card. 
//
//   The 6570 IOT, CFICTL, performs various control functions on the interface
// according to which AC bits are set -
//
//   AC0  = 1 --> assert card reset
//   AC1  = 1 --> deassert card reset
//   AC2  = 1 --> load delay from bits AC[3:5]
//   AC[3:5]  --> card delay selection if AC2=1
//   ... AC[6:8] are currently unused ...
//   AC9  = 1 --> clear the "card changed" bit
//   AC10 = 1 --> set the interrupt enable bit
//   AC11 = 1 --> clear "     "       "     "
//
//   AC bits 0 and 1 assert and deassert the CompactFlash card reset signal -
// this can be used to do a local reset of the card, say after it's hot swapped,
// without needing to reset the entire PDP-8.  AC bit 2 enables loading the
// interface delay setting from AC bits 3-5; bits 3-5 are otherwise ignored if
// AC2 is not set.  When set, AC bit 9 clears the "card changed" bit described
// above, and AC bits 10 and 11 set and reset the local interrupt enable bit.
//
//   The common memory model CompactFlash interface would be very simple except
// one unfortunate problem - we're just too fast for the card.  Way too fast,
// in fact.  There's more specific information on the timing in the Verilog
// code, but our IOT read and write cycles are about 10x faster than the slowest
// cards defined in the CompactFlash Alliance 3.0 specification, and even the
// fastest CF cards defined are still about 1/2 our speed.  The good news is
// that this interface takes care of the problem by forcing the CPU to wait
// during CFWxxx and CFRxxx IOTs and as far as the software is concerned all
// these instructions ere synchronous.
//
//   The card delay bits, AC[3:5] in the CFICTL/CFISTS IOTs, determine the
// number of wait states inserted.  The details are a little complicated and
// you're invited to read the cf_timing module to get all the details, but the
// basic idea is that smaller numbers are faster and bigger numbers are slower.
// Thus a delay of 0 is the fastest card, and 7 is the slowest card.
//
//   If interrupts are enabled, this interface will interrupt (a real, PDP-8
// interrupt, not a control panel trap!) whenever the "card changed" bit is
// set OR the card ready output is assertd.  The 6571 IOT, CFISRQ, will skip 
// when this interface is interrupting and can be used in the interrupt skip
// chain to identify the source of the interrupt.  In case of an card changed
// interrupt, the software MUST CLEAR the card changed bit with the CFICTL IOT
// before returning; otherwise you'll be stuck in an interrupt loop.  The
// ready bit interrupt is edge triggered and only causes an interrupt on the
// low to high transition of this signal.
//
// TARGET DEVICES
// TOOL VERSIONS 
//
// REVISION HISTORY:
// 17-Jan-11  RLA  New file.
//  1-Jan-14  RLA  Eliminate the system wide reset input (although we still have
//		   IOCLR).  Add an initial block to define the initial state.
//--
//000000001111111111222222222233333333334444444444555555555566666666667777777777
//234567890123456789012345678901234567890123456789012345678901234567890123456789
`include "sbc6120_h.v"		// global declarations for this project
`include "opcodes_h.v"		// PDP-8/HD6120 opcode mnemonics


module cf_control
  //++
  //   This module implements the card interface control and status registers,
  // along with all the associated bits.  It also implements the logic for all
  // the skip and interrupt functions, although it does not drive the ioc0/1
  // signals - that's up to the parent module.
  //--
(
  // Bus interface signals ...
  input  clock,			// system wide clock
  input   [9:11] ax,		// address bus (carries the IOT)
  inout  [`WORD] dx,		// bidirectional data bus
  input  selected,		// TRUE when our device code is selected
  input  iowr,			// I/O write strobe
  input  iord,			// I/O read strobe
  input  ioclr,			// clear all I/O devices (CAF)
  output ioskip,		// asserted if this IOT should skip
  output intreq,		// asserted if an interrupt is needed
  // Control signals ...
  input  cf_cd,			// card detected (inserted)
  input  cf_ready,		// card ready output
  output reg cf_reset,		// reset card
  output reg [2:0] delay	// interface delay selected
);
  // Locals
  reg ie;			// interrupt enable flag
  reg last_cd;			// last card detect state
  reg cd_changed;		// card changed flag
  reg last_ready;		// last ready state
  reg ready_changed;		// positive edge detected on ready

  // Decode the CFICTL and CFISTS operations ...
  wire wr_control = iowr & selected & `isCFICTL;
  wire rd_status  = iord & selected & `isCFISTS;

  // Define the initial state for all registers ...
  initial begin 
    last_cd = 0;  cd_changed = 0;
    last_ready = 0;  ready_changed = 0;
    ie = 0;  cf_reset = 1;  delay = 3'o7;
  end
  
  // Implement the card changed flag ...
  always @(posedge clock) begin
    if (ioclr) begin
      cd_changed <= 0;  last_cd <= cf_cd;
    end else begin
      if (wr_control & dx[9])
	cd_changed <= 0;
      else if (cf_cd != last_cd)
        cd_changed <= 1;
      last_cd <= cf_cd;
    end
  end // always @ (posedge clock)

  // Implement the rising edge detector for cf_ready ...
  always @(posedge clock) begin
    if (ioclr) begin
      ready_changed <= 0;  last_ready <= cf_ready;
    end else begin
      if (iowr & selected & `isCFSRDY)
	ready_changed <= 0;
      else if (cf_ready & !last_ready)
	ready_changed <= 1;
      last_ready <= cf_ready;
    end
  end // always @ (posedge clock)
    
  // Implement the interrupt enable flag ...
  always @(posedge clock) begin
         if (ioclr)               ie <= 0;
    else if (wr_control & dx[10]) ie <= 1;
    else if (wr_control & dx[11]) ie <= 0;  
  end // always @ (posedge clock)

  // Implement the card reset bit ...
  always @(posedge clock) begin 
         if (ioclr)              cf_reset <= 1;
    else if (wr_control & dx[0]) cf_reset <= 1;
    else if (wr_control & dx[1]) cf_reset <= 0;
  end // always @ (posedge clock)

  // Implement the interface delay select bits ...
  always @(posedge clock) begin
         if (ioclr)              delay <= 3'o7;
    else if (wr_control & dx[2]) delay <= dx[3:5];  
  end // always @ (posedge clock)

  // Implement interrupt requests and the MMCSRQ skip instruction ...
  assign intreq = ie & (cd_changed | ready_changed);
  assign ioskip = iowr & selected & (
		         (`isCFISRQ & intreq)
                       | (`isCFSRDY & cf_ready) );

  // And implement the status register ...
  assign dx = rd_status ?
      {cf_cd, cd_changed, cf_reset, delay, 4'b0, cf_ready, ie}
    :  12'bz;
endmodule // cf_control


module cf_timing
  //++
  //   For a write cycle, the slowest CF cards requre about a 30ns data and 
  // address setup time, a 150ns wide WE pulse, and another 30ns hold time. The
  // read timing is similar, with OE substituted for WE.  One special annoyance
  // is that the 30ns setup applies to the leading edge of the WE or OE pulse,
  // NOT the trailing edge.  On our side, remember that I/O reads and writes
  // normally take just one clock, and we probably have about a 35ns clock
  // period.  That's already way too fast, but add to that the fact that we 
  // really have no idea when our WE/OE becomes active in relation to the 
  // address/data AND we have no significant hold time after the end of the
  // clock.
  //
  //   So essentially we need to do three things -
  //
  //    1) delay the cf_we_n and cf_oe_n outputs for one clock cycle, while
  //    holding cf_a (and cf_d if writing) stable.
  //
  //    2) stretch the cf_we_a and cf_oe_n outputs to last about five clocks.
  //
  //    3) hold cf_a (and cf_d if writing) for one clock after after that.
  //
  //    4) stall the CPU the whole time we're doing this!
  //
  //   Finally, some cards are as much as 3x faster than the specification, and
  // of course we may not always have a 35ns clock. Because of that the "delay"
  // bits in the CFICTL IOT are intended to allow programming of the exact
  // delay used.
  //--
(
  input  clock,			// master system clock
  // CPU timing signals ...
  input  select_w,		// TRUE for a register write operation
  input  select_r,		//   "   "  "   "   "  read    "   "
  input  iowr,			// I/O write strobe
  input  iord,			// I/O read strobe
  output reg iowait,		// asserted to stretch this I/O operation
  input  [2:0] delay,		// selected interface delay
  // CompactFlash card timing signals ...
  output cf_cs_n,		// card select
  output reg cf_oe_n,		// output enable (read strobe)
  output reg cf_we_n		// write strobe
);  
  // State machine definitions ...
  localparam [2:0] 
    IDLE     = 0,	// waiting for I/O 
    SETUP    = 1,	// stretch address and data setup time
    // N.B. The STRETCHx states MUST be numbered sequentially ...
    STRETCH1 = 2,	// stretch WE/OE time
    STRETCH2 = 3,	// ...
    STRETCH3 = 4,	// ...
    STRETCH4 = 5,	// ...
    STRETCH5 = 6,	// ...
    HOLD     = 7;	// stretch address and data hold time
  reg [2:0] wait_state, next_wait;

  // Define the initial state ...
  initial
    wait_state = IDLE;

  //   At the moment we don't do anything special with the cs output - it's
  // asserted for any read or write for the entire duration of the operation.
  assign cf_cs_n = ~((iord & select_r) | (iowr & select_w)); 

  // Synthesize the state flip flops for our delay state machine ...
  always @(posedge clock)
    wait_state <= next_wait;

  //  And here's the logic for the wait state generator ...
  //
  //   Right now the "delay" settings are haphazardly implemented.  Even though
  // there are three bits in the delay setting, only two values actually have
  // any meaning - delay=0 is the fastest possible, with no wait states at all.
  // And delay=7 is the slowest possible, with the most generous timing we can
  // manage.  None of the other values in between do anything (right now any
  // non-zero value is treated as if it were a 7).  It's fairly simple to fix
  // state machine to implement other values if you feel so motivated.
  //
  //   The problem is that it's not really worth the effort.  I have one card
  // which will work reliably at delay=0 (yes, amazing, but true!) and did some
  // benchmarks using the BTS6120 "DF" command.  The overall difference between
  // delay=0 and delay=7 is only about 20% - significant, but hardly profound.
  // Even though the speed of the CFWxxx and CFRxxx IOTs changes dramatically,
  // they're only a small part of the overall instruction mix and making those
  // IOTs faster just doesn't make a big difference.
  //
  //   QED - just run everything with delay=7 and stop worrying...
  always @* begin
    iowait = 0;  cf_oe_n = 1;  cf_we_n = 1;  next_wait = wait_state;
    
    case (wait_state)
      // Wait for a CFRxxx or CFWxxx IOT to come along ...
      IDLE: begin 
        if ((iord & select_r) | (iowr & select_w)) begin
	  if (delay == 3'o0) begin
	    //   If delay=0 then just assert WE/OE right now and never,
	    // ever, leave the IDLE state.  That's the best we can do.
	    if (iord & select_r) cf_oe_n = 0;
            if (iowr & select_w) cf_we_n = 0;
	  end else begin // if (delay == 3'o0)
	    //   Otherwise, don't assert WE/OE yet and instead wait for
	    // cf_a and cf_d (which are already being driven) to settle.
	    iowait = 1;  next_wait = SETUP;
	  end
        end // if ((iord & select_r) | (iowr & select_w))
      end // case: IDLE

      // Another cycle to wait for cf_a and cf_d to settle...
      SETUP: begin
        iowait = 1;  next_wait = STRETCH1;
      end // case: SETUP

      // Assert WE/OE and hold them ...
      STRETCH1,STRETCH2,STRETCH3,STRETCH4,STRETCH5: begin
        iowait = 1;  next_wait = wait_state+1;
	if (iord & select_r) cf_oe_n = 0;
        if (iowr & select_w) cf_we_n = 0;
      end // case: STRETCH1,STRETCH2,STRETCH3,STRETCH4,STRETCH5

      // And hold cf_a/cf_d after removing WE/OE ...
      HOLD: begin
        next_wait = IDLE;
      end // case: HOLD
    endcase // case (wait_state)
  end // always @ *
endmodule // cf_timing


module cf_card
  //++
  // Here's the final CompactFlash card interface ...
  //--
(
  // Bus interface signals ...	     
  input  clock,		// system wide clock
  input  [`WORD] ax,	// address bus (carries the IOT)
  inout  [`WORD] dx,	// bidirectional data bus
  input  iowr,		// I/O write strobe
  input  iord,		// I/O read strobe
  input  ioclr,		// clear all I/O devices (CAF)
  output ioskip,	// asserted if this IOT should skip
  output ioc0,		// asserted if this IOT should clear the AC
  output ioc1,		// asserted if this IOT should load the AC
  output intreq,	// asserted if an interrupt is needed
  output iowait,	// asserted to stretch this I/O operation
  // CompactFlash card interface (in common memory model mode) ...
  output [2:0] cf_a,	// register select bits
  inout  [7:0] cf_d,	// 8 bit data bus
  output cf_oe_n,	// output enable (read strobe)
  output cf_we_n,	// write strobe
  output cf_cs_n,	// card select
  output cf_reset,	// reset the card
  input  cf_ready_n,	// card ready
  input  cf_cd_n	// card inserted
);
  parameter SELECT_R = 6'O55;	// register read  group
  parameter SELECT_W = 6'O56;	// register write group
  parameter SELECT_C = 6'O57;	// miscellaneous functions
  wire select_r = ax[3:8] == SELECT_R;
  wire select_w = ax[3:8] == SELECT_W;
  wire select_c = ax[3:8] == SELECT_C;

  //   The card detected/inserted input is just a mechanical connection (it's an
  // extra set of pins on the CF card that shorts the CD input to ground when
  // the card is inserted) so it needs to be debounced as if it were a switch.
  // And the cf_ready input is from another clock domain (who knows what's going
  // on inside the CF card !!) and needs to be synchronized.  We also take this
  // opportunity to invert the sense of both signals, which are active low.
  wire cf_ready_sync_n, cf_cd;
  debouncer #(.ACTIVE_STATE(0))
    cd_debounce(.clock(clock), .switch(cf_cd_n), .state(cf_cd));
  synchronize_level
    ready_sync(.clock(clock), .in(cf_ready_n), .out(cf_ready_sync_n));

  //   Drive the ioc0/1 signals.  All read and all write operations clear the
  // AC, as do CFISTS and CFICTL, and all read operations plus CFISTS load
  // the AC ...
  assign ioc0=iowr & (select_r | select_w | (select_c & (`isCFICTL|`isCFISTS)));
  assign ioc1=iowr & (select_r |            (select_c & (`isCFISTS)));

  // Create the control, skip, flag and status register ...
  wire [2:0] delay;
  cf_control control (
    .clock(clock), .ax(ax[9:11]), .dx(dx), .selected(select_c),
    .iowr(iowr), .iord(iord), .ioclr(ioclr), .ioskip(ioskip), .intreq(intreq),
    .cf_cd(cf_cd), .cf_ready(~cf_ready_sync_n), .cf_reset(cf_reset),
    .delay(delay) );   

  //   The CF address outputs are always the lower order three bits of the
  // IOT, and our data bus drives the CF data for a write and vice versa
  // for a read.  Those parts are easy ...
  assign cf_a = ax[9:11];
  assign cf_d = (iowr & select_w) ? dx[4:11] : 8'bz;
  assign dx = (iord & select_r) ? {4'b0, cf_d} : 12'bz;

  //   The cs, oe and we timing is harder, but fortunately we've got a module
  // for just that very job...
  cf_timing timing (
    .clock(clock), .select_w(select_w), .select_r(select_r),
    .iowr(iowr), .iord(iord), .iowait(iowait), .delay(delay),
    .cf_cs_n(cf_cs_n), .cf_oe_n(cf_oe_n), .cf_we_n(cf_we_n) );  
endmodule // cf_card
