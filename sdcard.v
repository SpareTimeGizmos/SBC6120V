//++
//sdcard.v - MMC/SD card SPI interface
//Copyright (C) 2011-2014 by Spare Time Gizmos.  All rights reserved.
//
// DESCRIPTION:
//   This module implements a PDP-8 interface to MMC/SD memory cards using SPI.
// The interface takes care of implementing the low level details of the SPI
// interface and is able to do all the necessary work to exchange single bytes
// with the card.  The SPI interface implements two software selectable speeds,
// a high speed which runs at 1/2 the CPU clock, and a slow speed which runs at
// 1/32 of the CPU clock.  Slow speed is used for initialization only - some
// older MMC cards must be initialized at the slow speed before they can switch
// to the high speed SPI.
//
// The IOTs implemented by this interface are -
//
//   6540 MMCCTL - card interface control functions (see below)
//   6541 MMCSRQ - skip if the interface is interrupting
//   6542 MMCW   - send a byte to the card
//   6543 MMCCS0 - deselect MMC card (CS/SS = 0)
//   6544 MMCR   - read a byte from the card
//   6545 MMCCS1 - select MMC card (CS/SS = 1)
//   6546 MMCX   - exchange (both read and write) a byte
//   6547 MMCSTS - read card interface status (see below)
//
//   Remember that the SPI interface is unusual in that all transfers are
// automatically bidirectional; the slave always sends the master a byte at the
// same time the master sends it a byte.  There's no way to transfer data in
// just one direction.  Because of that the MMCX IOT, "exchange a byte," is the
// basic I/O operation with the card.  MMCW and MMCR are just special cases of
// MMCX where you don't care about the transmitted or received data - the MMCR
// IOT always sends the byte 0xFF regardless of the initial AC contents, and
// MMCW just discards the received data.  AC bits 0-3 are not used by any of
// the MMCR/MMCW/MMCX IOTs - they're ignored when sending and are always zero
// when reading.
//
//   A subtle but critical point is that the MMCW/MMCR/MMCX IOTs are all
// SYNCHRONOUS - that is, they put the CPU into a wait state and no further
// PDP-8 instructions are executed until the transfer has completed.  That
// means there's no need for a "skip on transfer done" flag or anything like
// that - from the programmer's point of view, the data is transferred the
// instant the IOT is executed.  Since SPI transfers are always 8 bits it takes
// 16 CPU clocks to transfer a byte, plus the normal 2 or 3 clocks it would
// take to execute an IOT.  In the end MMCW takes 18 CPU clocks, and MMCR/MMCX
// each require 19 clocks.  That's in high speed SPI mode, of course.  With
// slow speed selected it takes about 130 CPU clocks to execute an MMC IOT -
// that's a long, long, time of course but it's not recommended to use slow
// speed mode except during card initialization.
//
//   The 6543 and 6545, MMCCS0 and MMCCS1, IOTs deassert and assert the SPI
// "slave select" output (the MMC/SD card definition calls it "chip select").
// They're actually an important part of the protocol and are used to synch-
// ronize message boundaries with the card, but handling them is up to the
// software.  This interface does nothing with the SPI SS/CS signal except
// when directed by these two IOTs.
//
//   The 6547 IOT, MMCSTS, returns the current status of the MMC/SD card
// interface (this is different from the card's status, which the software will
// have to get via SPI).  The MMCSTS IOT returns these bits in the AC -
//
//   AC0  = 1 --> card is inserted now
//   AC1  = 1 --> card has been changed 
//   AC2  = 1 --> write protect switch
//   ... AC[3:8] are currently unused ...
//   AC9  = 1 --> card is selected (SS/CS assertd)
//   AC10 = 1 --> high speed SPI is selected
//   AC11 = 1 --> interrupts are enabled
//
//   AC0, "card inserted,"  is a one if a card is currently inserted into the
// MMC/SD slot.  This tests a simple mechanical switch on the socket, and 
// indicates whether a card is even present.  The "card changed" bit, AC1, is
// set whenever a change in the card inserted status is detected.  This bit
// remains set, no matter how many times the card may be changed, until it's
// explicitly cleared by the software with the MMCCTL IOT.  The card changed
// bit allows the software to tell when the user has hot swapped a card.
//
//   AC2, "write protect," is the status of the physical write protect slide
// switch on the side of some MMC/SD cards.  This switch has no effect on the
// actual operation of the card (yes, you can happily write on a "protected"
// card if you want to) and it's up to the firmware to decide whether it wants
// to obey this setting.  AC bits 10 and 11 indicate the current state of the
// SPI speed and interrupt enable flags - both these bits can be set and cleared
// by the MMCCTL IOT.
//
//   The 6540 IOT, MMCCTL, performs various control functions on the interface
// according to which AC bits are set -
//
//   AC0  = 1 --> select the high speed SPI clock
//   AC1  = 1 --> select the low speed SPI clock
//   AC9  = 1 --> clear the "card changed" bit
//   AC10 = 1 --> set the interrupt enable bit
//   AC11 = 1 --> clear "     "       "     "
//
//   If interrupts are enabled, this interface will interrupt (a real, PDP-8
// interrupt, not a control panel trap!) whenever the "card changed" bit is
// set.  This gives the software an interrupt when the user swaps the card.
// Since there are no "done" bits in this interface, there are no other
// sources for an interrupt request.  The 6541 IOT, MMCSRQ, will skip when
// this interface is interrupting and can be used in the interrupt skip chain
// to identify the source of the interrupt.  In case of an interrupt, the
// software MUST CLEAR the card changed bit with the MMCCTL IOT before
// returning; otherwise you'll be stuck in an interrupt loop.
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


module sd_spi
  //++
  //   This module implements a simple SPI mode 0 (CPOL=0, CPHA=0) shifter for
  // the MMC/SD card interface.  SPI is automatically bidirectional, so eight
  // bits always go out and at the same time eight bits always come back.
  // It doesn't handle the chip/slave select - it's assumed that somebody else
  // is taking care of that.
  //
  //   This is the "simple minded" version that implements the SPI with a state
  // machine.  There's no problem with as far as it goes, but the state machine
  // design limits the SPI clock to 1/2 of the CPU clock.  The difficulty with
  // speeding it up is that SPI really requires that data be shifted on both
  // edges of the SPI SCLK - MISO shifts in on positive SCLK edges and MOSI
  // shifts out on negative SCLK edges.  I'm sure that can be implemented at
  // full CPU clock speed, but it's trickier.  I'll leave that as an exercise
  // for the reader :-)
  //
  //   This module also implements a "slow" SPI speed, which is 1/16th of the
  // full speed (that'd be 1/32nd of the CPU clock frequency).  This is the
  // 400kHz clock that's required to initialize some older cards.
  //--
(
  input  clock,			// master system clock
  // Parallel bus interface ...
  input  start,			// true for one tick to start a transfer
  output reg busy,		// we're busy sending
  output reg done,		// true for one tick when a transfer is done
  input      [7:0] din,		// input bus (data sent to peripheral)
  output reg [7:0] dout,	// output bus (data received from peripheral)
  input  hispeed,		// asserted for the high speed SPI clock
  // SPI interface ...
  output reg sclk,		// SPI clock
  output mosi,			// master (that's us!) our, slave in
  input  miso			// master in, slave out
);

  // Our state machine states ...
  localparam [1:0]
    IDLE   = 0,			// waiting for start 
    SHIFT1 = 1,			// positive SCLK part
    SHIFT2 = 2,			// negative SCLK part
    DONE   = 3;			// all bits sent
  reg [1:0] state, next;

  // Other locals ...
  reg [2:0] nbits, next_nbits;	// number of bits left to send
  reg [7:0] next_dout;		// MISO data
  reg [7:0] sout, next_sout;	// MOSI data
  reg [3:0] delay, next_delay;	// divide by 16 counter for slow SPI clock

  // Define the initial state ...
  initial begin
    state = IDLE;  sout = 0;  dout = 0;  nbits = 0;  delay = 0;
  end
  
  // Synthesize the state FFs ...
  always @(posedge clock) begin
    state <= next;  sout <= next_sout;  dout <= next_dout;
    nbits <= next_nbits;  delay <= next_delay;
  end

  // MOSI is always just the MSB of the output shifter.  It's that easy ...
  assign mosi = sout[7];
  
  //   NOTE - the din parameter is data in TO THIS MODULE!  It actually feeds
  // sout, next_sout and MOSI.  Likewise the dout parameter is DATA OUT FROM
  // THIS MODULE.  It's actually the input shift register and is connected to
  // MISO.  Sorry for the confusion....

  // Next state logic ...
  always @* begin
    next = state;  next_sout = sout;  next_dout = dout;  next_nbits = nbits;
    busy = 0;  done = 0;  done = 0;  sclk = 0;  next_delay = delay;
    
    case (state)
      // Wait for start ...
      IDLE: begin
	if (start) begin
	  next_sout = din;  busy = 1;  next_nbits = 7;  
	  next = SHIFT1;  next_delay = 0;
	end
      end // case: IDLE

      // Negative SCLK - shift in a bit from MISO ...
      SHIFT1: begin
        sclk = 0;  busy = 1;
        if (&delay | hispeed) begin
	  next_dout = {dout[6:0], miso}; 
	  next = SHIFT2;  next_delay = 0;
	end else
	  next_delay = delay+1;
      end

      // Positive SCLK - shift out a bit to MOSI ...
      SHIFT2: begin
        sclk = 1;  busy = 1;
        if (&delay | hispeed) begin
	  next_sout = {sout[6:0], 1'b0};
  	  if (nbits == 0)
	    next = DONE;
	  else begin
	    next_nbits = nbits-1;  next = SHIFT1;  next_delay = 0;
	  end
	end else
	  next_delay = delay+1;
      end // case: SHIFT2

      // All finished - assert done, clear busy ...
      DONE: begin
	sclk = 0;  busy = 0;  done = 1;  next = IDLE;
      end // case: DONE
    endcase // case (state)
  end
endmodule // sd_spi


module sd_control
  //++
  //   This module implements the MMC/SD interface control and status registers,
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
  input  cd,			// card detected (inserted)
  input  wp,			// card write protected
  output reg cs_n,		// card selected output
  output reg hispeed		// high speed SPI is selected
);
  // Locals
  reg ie;			// interrupt enable flag
  reg last_cd;			// last card detect state
  reg cd_changed;		// card changed flag

  // Decode the MMCCTL and MMCSTS operations ...
  wire wr_control = iowr & selected & `isMMCCTL;
  wire rd_status  = iord & selected & `isMMCSTS;

  // Define the initial state ...
  initial begin 
    last_cd = 0;  cd_changed = 0;
    ie = 0;  hispeed = 1;  cs_n = 1;
  end
  
  // Implement the card changed flag ...
  always @(posedge clock) begin
    if (ioclr) begin
      cd_changed <= 0;  last_cd <= cd;
    end else begin
      if (wr_control & dx[9])
	cd_changed <= 0;
      else if (cd != last_cd)
        cd_changed <= 1;
      last_cd <= cd;
    end
  end // always @ (posedge clock)

  // Implement the interrupt enable flag ...
  always @(posedge clock) begin
         if (ioclr)                    ie <= 0;
    else if (wr_control & dx[10])      ie <= 1;
    else if (wr_control & dx[11])      ie <= 0;  
  end // always @ (posedge clock)

  // Implement the High Speed bit ...
  always @(posedge clock) begin
         if (ioclr)                   hispeed <= 1;
    else if (wr_control & dx[0])      hispeed <= 1;
    else if (wr_control & dx[1])      hispeed <= 0;
  end // always @ (posedge clock)

  // Decode the MMCCS0 and MMCCS1 IOTs for the cs output ...
  always @(posedge clock) begin
         if (ioclr | (iowr & selected & `isMMCCS0)) cs_n <= 1;
    else if (        (iowr & selected & `isMMCCS1)) cs_n <= 0;
  end

  // Implement interrupt requests and the MMCSRQ skip instruction ...
  assign intreq = ie & cd_changed;
  assign ioskip = iowr & selected & `isMMCSRQ & intreq;

  // And implement the status register ...
  assign dx = rd_status ? {cd,cd_changed,wp,6'b0,~cs_n,hispeed,ie} : 12'bz;
endmodule // sd_control


module sd_card
  //++
  // Here's the final interface, pretty much as promised ...
  //--
(
  // Bus interface signals ...	     
  input  clock,			// system wide clock
  input  [`WORD] ax,		// address bus (carries the IOT)
  inout  [`WORD] dx,		// bidirectional data bus
  input  iowr,			// I/O write strobe
  input  iord,			// I/O read strobe
  input  ioclr,			// clear all I/O devices (CAF)
  output ioskip,		// asserted if this IOT should skip
  output ioc0,			// asserted if this IOT should clear the AC
  output ioc1,			// asserted if this IOT should load the AC
  output intreq,		// asserted if an interrupt is needed
  output iowait,		// asserted to stretch this I/O operation
  // MMC/SD card SPI interface ...
  output mmc_sclk,		// SPI clock
  output mmc_mosi,		// master (that's us!) our, slave in
  input  mmc_miso,		// master in, slave out
  output mmc_cs_n,		// slave select
  input  mmc_cd_n,		// card detected
  input  mmc_wp_n		// card is write protectedz
);
  parameter SELECT = 6'O54;	// all IOTs for this interface
  wire selected = ax[3:8] == SELECT;

  //   The write protect and card detected inputs come from mechanical switches
  // in the MMC/SD socket, and should be debounced as any other switch input
  // would.
  wire cd, wp;
  debouncer #(.ACTIVE_STATE(0))
    cd_debounce (.clock(clock), .switch(mmc_cd_n), .state(cd));
  debouncer #(.ACTIVE_STATE(1))
    wp_debounce (.clock(clock), .switch(mmc_wp_n), .state(wp));

  // MMCR, MMCX, MMCW, MMCCTL and MMCSTS all clear the AC ...
  // MMCR, MMCX and MMCSTS all load the AC ...
  assign ioc0 = iowr & selected & (`isMMCR|`isMMCW|`isMMCX|`isMMCCTL|`isMMCSTS);
  assign ioc1 = iowr & selected & (`isMMCR|`isMMCX|`isMMCSTS);

  // Create the control, skip, flag and status register ...
  wire hispeed;
  sd_control control (
    .clock(clock), .ax(ax[9:11]), .dx(dx), .selected(selected),
    .iowr(iowr), .iord(iord), .ioclr(ioclr), .ioskip(ioskip), .intreq(intreq),
    .cd(cd), .wp(wp), .cs_n(mmc_cs_n), .hispeed(hispeed) );

  //   All three IOTs, MMCR, MMCR and MMCX start the SPI state machine.  MMCW
  // and MMCX transmit the data in the AC, but MMCR transmits the constant 0xFF.
  // MMCR and MMCX both load the AC and MMCW doesn't, but we don't really have
  // to worry about that much - since ioc1 wasn't asserted for MMCW, no iord
  // strobe will be generated and nothing ever happens with the data read back.
  // The CPU is stalled (iowait=1) as long as the SPI state machine is busy -
  // the timing of the SPI busy output is exactly right for this job.
  wire mmc_start;  wire [7:0] mmc_out, mmc_in;
  assign mmc_start = iowr & selected & (`isMMCR | `isMMCW | `isMMCX);
  assign mmc_out = (iowr & selected & (`isMMCW | `isMMCX)) ? dx[4:11] : 8'hFF;
  sd_spi spi (
    .clock(clock), .hispeed(hispeed),
    .start(mmc_start), .busy(iowait), .done(), .din(mmc_out), .dout(mmc_in),
    .sclk(mmc_sclk), .mosi(mmc_mosi), .miso(mmc_miso)
  );
  assign dx = (iord & selected & (`isMMCR | `isMMCX)) ? {4'b0, mmc_in} : 12'bz;
endmodule // sd_card
