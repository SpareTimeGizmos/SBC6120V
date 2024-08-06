//++
//opcodes_h.v
//
//   PDP-8/HD6120 OPCODES AND MNEMONICS
//   Copyright (C) 2011 by Spare Time Gizmos.  All rights reserved.
//
// DESIGN NAME:	SBC6120V
//
// DESCRIPTION:
//   This file contains declarations for all PDP-8/HD6120 opcodes.
//
// REVISION HISTORY:
// 22-Jan-11  RLA  New file.
//--
//000000001111111111222222222233333333334444444444555555555566666666667777777777
//234567890123456789012345678901234567890123456789012345678901234567890123456789
`ifndef opcode_h

// The usual PDP-8 instruction mnemonics and associated bit patterns...
`define isAND(o)  (o[0:2]==3'O0)	// LOGICAL AND 
`define isTAD(o)  (o[0:2]==3'O1)	// TWO'S COMPLEMENT ADD
`define isISZ(o)  (o[0:2]==3'O2)	// INCREMENT AND SKIP IF ZERO
`define isDCA(o)  (o[0:2]==3'O3)	// DEPOSIT AND CLEAR ACCUMULATOR
`define isJMS(o)  (o[0:2]==3'O4)	// JUMP TO SUBROUTINE
`define isJMP(o)  (o[0:2]==3'O5)	// JUMP
`define isIOT(o)  (o[0:2]==3'O6)	// INPUT/OUTPUT TRANSFER
`define isOPR(o)  (o[0:2]==3'O7)	// OPERATE MICROINSTRUCTION
`define isMRI(o)  (o[0:1]!=2'b11)	// any MRI (including JMP/JMS)
`define isOPR1(o) (`isOPR(o)  &!o[3])	      // OPERATE, group 1
`define isOPR2(o) (`isOPR(o)  & o[3] & !o[11])// OPERATE, group 2
`define isOPR3(o) (`isOPR(o)  & o[3] &  o[11])// OPERATE, group 3
`define isIA(o)	  ( o[3])		// indirect addressing mode
`define isDA(o)   (!o[3])		// direct    "     "    "
`define isCP(o)   ( o[4])		// current page addressing
`define isZP(o)   (!o[4])		// zero      "    "    "
`define isAI(a)   (a[0:8]==9'O001)	// autoindex register address
`define EA(a,o) {`isZP(o) ? 5'b0 : a[0:4], o[5:11]}

// These mnemonics define bit positions in the OPR instructions ...
`define Gx_CLA		 4	// clear AC (G1, G2 & G3!)   
`define G1_CLL		 5	// clear link 
`define G1_CMA		 6	// complement AC
`define G1_CML		 7	// complement link
`define G1_RAR		 8	// rotate right
`define G1_RAL		 9	// rotate left
`define G1_TWICE	10	// rotate twice
`define G1_IAC		11	// increment AC (G1)
`define G2_SMA		 5	// skip on minus accumulator
`define G2_SZA		 6	// skip on zero accumulator
`define G2_SNL		 7	// skip on non-zero link
`define G2_NEGATE	 8	// reverse the sense of the skip
`define G2_OSR		 9	// OR with switch register
`define G2_HLT		10	// HALT
`define G3_MQA		 5	// OR transfer MQ to AC
`define G3_MQL		 7	// jam transfer AC to MQ

// Interrupt control instructions ...
`define isPIOT(o) (o[0:8]==9'O600)
`define IO_SKON	       12'o6000	// skip if interrupts enabled (main memory)
`define IO_PRS	       12'o6000	// read panel status flags (panel memory)
`define IO_ION	       12'o6001	// enable interrupts
`define IO_IOF	       12'o6002	// disable interrupts
`define IO_SRQ	       12'o6003	// skip if intreq is asserted (main memory)
`define IO_PGO	       12'o6003	// clear the hltflg (panel memory)
`define IO_GTF	       12'o6004	// get current flags (main memory)
`define IO_PEX	       12'o6004	// exit panel mode (panel memory)
`define IO_RTF	       12'o6005	// restore flags
`define IO_SGT	       12'o6006	// skip on greater than flag
`define IO_CAF	       12'o6007	// clear all flags

// KM8/E Memory extension and timeshare instructions ...
`define isEMA(o) ((o[0:5]==6'o62) & (  (o[9:11]==3'o1) | (o[9:11]==3'o2) \
				     | (o[9:11]==3'o3) | (o[9:11]==3'o4)) )
`define IO_CDF	       12'o62x1	// change data field
`define IO_CIF	       12'o62x2	// change instruction field
`define IO_CXF	       12'o62x3	// change data and instruction field
`define IO_CINT	       12'o6204	// clear user mode interrupt flag
`define IO_RDF	       12'o6214	// read data field
`define IO_RIF	       12'o6224	// read instruction field
`define IO_RIB	       12'o6234	// read interrupt buffer (save field register)
`define IO_RMF	       12'o6244	// return memory fields
`define IO_SINT	       12'o6254	// set user mode interrupt flag
`define IO_CUF	       12'o6264	// clear user buffer/flag
`define IO_SUF	       12'o6274	// set user buffer/flag

// HD6120 stack instructions ...
`define isStack(o) ((o[0:5]==6'o62) & ((o[9:11]==3'o5) | (o[9:11]==3'o7)))
`define IO_PPC1	       12'o6205	// push PC+1 onto stack 1
`define IO_PPC2	       12'o6245	// push PC+1 onto stack 2
`define IO_PAC1	       12'o6215	// push AC onto stack 1
`define IO_PAC2	       12'o6255	// push AC onto stack 2
`define IO_RTN1	       12'o6225	// pop PC from stack 1
`define IO_RTN2	       12'o6265	// pop PC from stack 2
`define IO_POP1	       12'o6235	// pop AC from stack 1
`define IO_POP2	       12'o6275	// pop AC from stack 2
`define IO_RSP1	       12'o6207	// read stack pointer #1
`define IO_RSP2	       12'o6227	// read stack pointer #2
`define IO_LSP1	       12'o6217	// load stack pointer #1
`define IO_LSP2	       12'o6237	// load stack pointer #2

// Other HD6120 special instructions ...
`define is6120(o) ((o[0:5]==6'o62) & (o[9:11]==3'o6))
`define IO_PR0	       12'o6206	// panel request 0
`define IO_PR1	       12'o6216	// panel request 1
`define IO_PR2	       12'o6226	// panel request 2
`define IO_PR3	       12'o6236	// panel request 3
`define IO_WSR	       12'o6246	// write switch register
`define IO_GCF	       12'o6256	// get current flags
`define IO_CPD	       12'o6266	// clear panel data flag
`define IO_SPD	       12'o6276	// set panel deta flag

// Receiver (keyboard) IOTs ...
`define isKCF	(ax[9:11]==3'O0)  // clear keyboard data ready flag
`define isKSF	(ax[9:11]==3'O1)  // skip on receiver data ready flag
`define isKCC	(ax[9:11]==3'O2)  // clear AC and receiver flag
`define isKRS	(ax[9:11]==3'O4)  // OR transfer keyboard into AC
`define isKIE	(ax[9:11]==3'O5)  // set interrupt enable from AC[11]
`define isKRB	(ax[9:11]==3'O6)  // read keyboard and clear flag (KCC + KRS)

// Transmitter (teleprinter) IOTS ...
`define isTFL	(ax[9:11]==3'O0)  // set transmitter flag
`define isTSF	(ax[9:11]==3'O1)  // skip if transmitter flag is set
`define isTCF	(ax[9:11]==3'O2)  // clear transmitter flag
`define isTPC	(ax[9:11]==3'O4)  // load character from AC 
`define isSPI	(ax[9:11]==3'O5)  // skip on transmitter or receiver interrupt
`define isTLS	(ax[9:11]==3'O6)  // print character (TCF + TPC)

// CompactFlash control IOTs ...
`define isCFICTL (ax[9:11]==3'O0) // card interface control functions
`define isCFISRQ (ax[9:11]==3'O1) // skip on interrupt request
`define isCFSRDY (ax[9:11]==3'O2) // skip on CF card ready
// IOTS 3..6 are currently unused
`define isCFISTS (ax[9:11]==3'O7) // read interface status

// MMC/SD card IOTs ...
`define isMMCCTL (ax[9:11]==3'O0) // card interface control functions
`define isMMCSRQ (ax[9:11]==3'O1) // skip on interrupt request
`define isMMCW   (ax[9:11]==3'O2) // write byte (AC -> MMC/SD card)
`define isMMCCS0 (ax[9:11]==3'O3) // deselect card (CS/SS <= 0)
`define isMMCR   (ax[9:11]==3'O4) // read byte (AC <- MMC/SD card)
`define isMMCCS1 (ax[9:11]==3'O5) // select card (CS/SS <= 1)
`define isMMCX   (ax[9:11]==3'O6) // exchange byte with card
`define isMMCSTS (ax[9:11]==3'O7) // read interface status

// DS1302 RTC IOTs ...
`define isCCCM   (ax[9:11]==3'O0) // load command register
`define isCCSF   (ax[9:11]==3'O1) // skip on done
`define isCCWD   (ax[9:11]==3'O2) // write data register
`define isCCRD   (ax[9:11]==3'O3) // read data register

// FP6120 IOTs ...
`define isCCPR	 (ax[9:11]==3'O0) // clear AC, HALT SW REQ and 30Hz REQ flags
`define isSHSW	 (ax[9:11]==3'O1) // skip on HALT SW REQ flag
`define isSPLK	 (ax[9:11]==3'O2) // skip on panel lock
`define isSCPT	 (ax[9:11]==3'O3) // skip on 30Hz timer REQ flag
`define isRFNS	 (ax[9:11]==3'O4) // read function switches
`define isRLOF	 (ax[9:11]==3'O5) // turn RUN LED off
`define isRLON	 (ax[9:11]==3'O7) // turn RUN LED on
`define isPLOF	 (ax[9:11]==3'O5) // turn POWER LED off
`define isPLON	 (ax[9:11]==3'O7) // turn POWER LED on 
`define isSBBLO  (ax[9:11]==3'O5) // skip on backup battery low (DS1321 only!)
`define isSBBLO2 (ax[9:11]==3'O6) //   "  "   "  "   "   "   " (SBC6120-RC only)

`define opcode_h
`endif
