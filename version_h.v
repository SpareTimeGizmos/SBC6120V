//++
//version_h.v - SBC6120 Project Revision History
//Copyright (C) 2014 by Spare Time Gizmos.  All rights reserved.
//
//   This module contains the master edit history for the SBC6120 Verilog source
// code.  Regrettably, I didn't start keeping this history until after the
// project was mostly written, however even late is better than never.
//
//   For what it's worth, this module does have one practical use - it defines
// a single preprocessor macro, FPGA_VERSION, for the current revision.  The
// PDP8 code (e.g. BTS6120) can obtain this value by executing the RTR RTL
// micro-instruction.
//
// DATE       WHO  VER  WHAT
//----------  ---  ---  --------------------------------------------------------
// 01-JAN-11  RLA  001	New file.
//
// 22-JAN-11  RLA  002  Digilent S3 version.  This implements a basic SBC6120
//			plus a serial port ONLY!
//
// 27-JAN-11  RLA  003	UME2 version.  Start adding additional features!
//
// 28-JAN-11  RLA  004  Add VT52 terminal.
//  
// 12-FEB-11  RLA  005  Add DS1302 support.  This works with the same BTS6120
//			RTC code that supported Jim's IOB6120 board.
//			
// 15-FEB-11  RLA  006  Add FP6120 emulation.
//
// 17-FEB-11  RLA  007  Add CompactFlash card support.  This is functionally
//			identical to, and compatible with, the IOB6120 CF IOTs.
//
// 17-FEB-11  RLA  010	Add an SPI controller and MMC/SD support.
//
// [First "released" version, Spring 2011]
//
// 28-DEC-13  RLA  011	Clean up a bunch of XST warnings that crept in with the
//			latest ISE.  I was careless about sizing some constants
//			(e.g. 1'b0 instead of just 0) and Xilinx now complains.
//
// 29-DEC-13  RLA  012	Invent the "writable EPROM" scheme.  In effect, panel
//			RAM is preloaded with BTS6120 by the FPGA configuration
//			bitstream, and there's no longer any need to copy from
//			EPROM to panel RAM.
//
// 31-DEC-13  RLA  013	Do away with the external RAM interface and use BRAM
//			exclusively for SBC6120 main memory.  
//
// 31-DEC_13  RLA  014	Add ifdefs to allow the SBC6120/V to be configured with
//			or without the front panel (FP6120), CompactFlash card,
//			MMC/SD card, and DS1302 RTC interfaces.
//--
//000000011111111112222222222333333333344444444445555555555666666666677777777778
//345678901234567890123456789012345678901234567890123456789012345678901234567890

// End of SBC6120/V edit history ...
`define FPGA_VERSION	12'o0014	// version of this Verilog code
