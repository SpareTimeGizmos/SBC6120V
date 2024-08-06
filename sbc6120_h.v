//++
//sbc6120_h.v - SBC6120/V configuration and global definitions
//Copyright (C) 2011-2014 by Spare Time Gizmos.  All rights reserved.
//
// REVISION HISTORY:
// 09-Jan-11  RLA  New file.
// 28-Dec-13  RLA  Add memory and peripheral configuration options.
//--
//000000001111111111222222222233333333334444444444555555555566666666667777777777
//234567890123456789012345678901234567890123456789012345678901234567890123456789
`ifndef sbc6120_h

// MEMORY CONFIGURATION ...
`define EPROM_FIELDS	3'o2	// The number of panel memory fields allocated
				//  to EPROM, and consequentially, the size of
				//  the BTS6120 image.   Because the EPROM is
				//  loaded via data2mem, you can't actually
				//  change the EPROM size simply by changing
				//  this value - you'll need to fool with the
				//  code in memory.v and the eprom.bmm file.
`define XRAM		1'b1	// When defined, generate the interface for an
				//  external 64Kx12 SRAM.  This contains all of
				//  main memory plus whatever panel fields are
				//  NOT part of the EPROM.  When XRAM is not
				//  defined, panel memory is limited to just
				//  EPROM_FIELDS, and the FPGA's internal block
				//  RAM is used for all main memory.
`define IRAM_FIELDS	3'o3	// The number of 4KW fields allocated to main
				//  memory using FPGA internal block RAM.  This
				//  is valid only when XRAM is NOT defined!

// PERIPHERAL CONFIGURATION ...
`define FP6120		1'b1	// Generate an FP6120 compatible front panel
	  			//  interface when defined.  When NOT defined,
				//  the system is equivalent to the original
				//  SBC6120 w/o front panel.
`define CFCARD		1'b1	// Generate an IOB6120 compatible CompactFlash
	  			//  card interface when defined.
`define RTC		1'b1	// Generate an IOB6120 compatible DS1302 time
	  			//  of day clock interface when defined.
`define SDCARD		1'b1	// Generate a high speed SPI interface for
	  			//  MMC/SD cards when defined.

// Special constants ...
`define WORD		0:11		// width of a PDP-8 word (big endian!)
`define EMA		0:2		//   "   "  "   "   extended address

// Front panel function switch definitions (used by fp6120.v) ...
//   Note that the bit positions assigned to the various switches, as well as
// the rotary switch positons, correspond to the bits returned by the RFNS
// IOT.  
`define FNSW_BUS      0:8	// there are nine function switches in all
`define FNSW_BOOT	0	// BOOT
`define FNSW_LA		1	// Load Address
`define FNSW_LXA	2	// Load eXtended Address
`define FNSW_CLEAR	3	// CLEAR
`define FNSW_CONT	4	// CONTinue 
`define FNSW_EXAM	5	// EXAMine
`define FNSW_DEP	6	// DEPosit
`define FNSW_HALT	7	// run/HALT
`define FNSW_LOCK	8	// panel LOCK

// Front panel rotary switch definitions (used by fp6120.v) ...
`define ROTSW_BUS     0:4	// five positions (in the RC model)
`define ROTSW_MD	0	// display Memory Data
`define ROTSW_MQ	1	// display MQ register
`define ROTSW_AC	2	// display AC register
`define ROTSW_FLAGS	3	// display PS register
`define ROTSW_POST	4	// display POST code

`define sbc6120_h
`endif //  `ifndef sbc6120_h
