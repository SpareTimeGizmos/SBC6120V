//++
//console.v - PDP-8 compatible serial line unit
//Copyright (C) 2011-2014 by Spare Time Gizmos.  All rights reserved.
//
// DESCRIPTION:
//   This module implements the KL8E compatible serial line unit, less any UART,
// that serves as the console for the SBC6120.  Actually this implementation is
// slightly better than a real SBC6120 because the logic implemented here is
// completely KL8E equivalent.  The SBC6120 doesn't implement the KL8E local
// interrupt enable flag, and it doesn't implement the KCF, KIE, TFL and SPI
// IOTs.  I would have, but there just wasn't room in the GALs!
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

module console (
  // Bus interface signals ...	     
  input         clock,		// system wide clock
  input [`WORD] ax,		// address bus (carries the IOT)
  inout [`WORD] dx,		// bidirectional data bus
  input         iowr,		// I/O write strobe
  input         iord,		// I/O read strobe
  input         ioclr,		// clear all I/O devices (CAF)
  output        ioskip,		// asserted if this IOT should skip
  output        ioc0,		// asserted if this IOT should clear the AC
  output        ioc1,		// asserted if this IOT should load the AC
  output        intreq,		// asserted if an interrupt is needed
  // Terminal I/O signals ...
  input         serial_in,	// RS232 serial data input
  output        serial_out,	// RS232 serial data output
  output	brkreq		// asserted if a break is received
);
  // Parameters
  parameter SYSTEM_CLOCK = 50000000;	// clock frequency (in Hz!)
  parameter BAUD_RATE    = 9600;	// desired baud rate (bps)
  parameter RXDEVICE     = 6'O03;	// keyboard IOT select address
  parameter TXDEVICE     = 6'O04;	// teleprinter IOT select address

  // Local signals ...
  reg  tprflag;			// teleprinter flag
  wire kbdflag;			// keyboard flag
  wire tbre;			// uart transmitter buffer register empty
  wire tbrl;			// load uart transmitter buffer
  reg  last_tbre;		// last tbre output from the uart
  reg  kl8ien;			// kl8v global interrupt enable
  wire kl8irq;			// kl8v global interrupt request
  wire clr_kbdflag;		// set keyboard flag
  wire [7:0] rxbuf;		// UART receiver buffer data
  wire baudclk;			// baud rate clock

  //  rxselect and txselect are asserted whenever their respective device codes
  // appear on the address bus, but this alone isn't enough - you still have to
  // AND select with either iowr or iord  before you can be sure!
  wire rxselect;  assign rxselect = ax[3:8] == RXDEVICE;
  wire txselect;  assign txselect = ax[3:8] == TXDEVICE;

  // Define the initial state ...
  initial begin
    last_tbre = 1'b1;  tprflag = 1'b0;  kl8ien = 1'b1;
  end

  //   The keyboard flag is cleared by KCC or KRB and can be set only when a
  // new character is received.  Note that KRS doesn't clear the flag, and
  // neither apparently does ioclr.  I find that suspicious, but that's the
  // way the real SBC6120 IOT2 GAL works.  BTW, there is no explicit keyboard
  // flag FF here - the data received FF in the UART is enough to do the job...
  assign clr_kbdflag = iowr & rxselect & (`isKCC|`isKRB);

  //   OTOH, the TBRE output of the UART always reflects the buffer status and
  // so the teleprinter section will need a special FF for its flag so that we
  // can set or clear it at will.  The printer flag is set when the transmitter
  // buffer is empty or by the TFL IOT, and is reset by ioclr, TCF or TLS.
  //
  //   Setting the tprflag is messy, though, because we only want to set it on
  // the positive edge of tbre - it can't be level sensitive and set anytime
  // tbre is asserted because TCF wouldn't work.  That means we have to use
  // another temporary FF to delay tbre one clock so we can detect an edge.
  always @(posedge clock)
    last_tbre <= tbre;
  always @(posedge clock) begin
    if (ioclr | (iowr & txselect & (`isTCF | `isTLS)))
      tprflag <= 1'b0;
    else if ((tbre & !last_tbre) | (iowr & txselect & `isTFL))
      tprflag <= 1'b1;
  end

  //   The interrupt enable flag is shared with the transmitter/teleprinter
  // unit.  It is set by reset or ioclr and set/reset from AC[11] by KIE ...
  always @(posedge clock) begin
    if (ioclr)
      kl8ien <= 1'b1;
    else if (iowr & rxselect & `isKIE)
      kl8ien <= dx[11];
  end

  // Assert intreq if either flag is set and KL8 interrupts are enabled ...
  assign kl8irq = tprflag | kbdflag;
  assign intreq = kl8ien & kl8irq;
  
  //  ioc0 (clear the AC) is asserted for KCC and KRB, and ioc1 (read peripheral
  // data) is asserted for KRS and KRB.  Notice that none of the teleprinter
  // IOTs need to assert any of the cx signals ...
  assign ioc0 = iowr & rxselect & (`isKCC | `isKRB);
  assign ioc1 = iowr & rxselect & (`isKRS | `isKRB);

  // ioskip is asserted for -
  //	- KSF if the keyboard flag is set
  //	- TSF if the printer flag is set
  //	- SPI if either device is requesting an interrupt 
  assign ioskip = iowr & (
	            (rxselect & `isKSF & kbdflag)
                  | (txselect & `isTSF & tprflag)
                  | (txselect & `isSPI & kl8irq)  );

  // Drive the receiver data onto the bus during KRB or KRS...
  assign dx = (iord & rxselect & (`isKRS | `isKRB)) ? {4'b0, rxbuf} : 12'bz;
  // And load the transmitter during the iowr phase of TPC or TLS ...
  assign tbrl = iowr & txselect & (`isTPC | `isTLS);

  // That's it - make ourselves a uart and we're done ...
  ck_divider #(.IN_FREQ(SYSTEM_CLOCK), .OUT_FREQ(16*BAUD_RATE))
    brg(.clock(clock), .out(baudclk));
  uart uart (
    .clock(clock), .reset(ioclr),
    .rrc(baudclk),	// receiver baud clock
    .rri(serial_in),	// serial data input
    .dr(kbdflag),	// data received flag
    .drr(clr_kbdflag),	// reset data received (dr) flag
    .rbr(rxbuf),	// received data buffer
    .fe(brkreq),	// framing error (break received)
    .oe(),		// overrun error
    // Transmitter connections ...
    .trc(baudclk),	// transmitter baud clock
    .tro(serial_out),	// serial data output
    .tre(),		// transmitter register is empty (idle)
    .tbre(tbre),	// transmitter buffer is empty
    .tbrl(tbrl),	// load transmitter buffer
    .tbr(dx[4:11])	// transmitted data buffer
  );
endmodule
