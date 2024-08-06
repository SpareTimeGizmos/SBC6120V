//++
//memory.v - SBC6120/V memory subsystem
//Copyright (C) 2011-2014 by Spare Time Gizmos.  All rights reserved.
//
// DESCRIPTION:
//   This module implements a SBC6120 compatible memory subsystem including both
// control panel and main memory spaces.  The original SBC6120 has 64Kx12 of
// static RAM (eight 4KW fields for main memory and 8 fields for panel memory)
// plus 32Kx12 of EPROM for the BTS6120 firmware (although only 2 or 3 fields
// of the EPROM were actually used).  It's difficult to make PDP-8 code run from
// ROM, and BTS6120 never attempted to do so.  Instead the entire contents of
// EPROM were copied to panel RAM during startup and then the code executed from
// RAM.  The EPROM was never used once the RAM was initialized.  The original
// SBC6120 had a simple memory mapping register that controlled EPROM and RAM
// accesses during initialization.
//
//   The Verilog implementation is a bit different.  We still have a logical
// EPROM, however our EPROM is implemented in FPGA block RAM and is writable.
// This "writable EPROM" is permanently mapped as the first two or three fields
// of the panel memory space.  BTS6120 no longer needs to copy itself from real
// EPROM to RAM, and that part of the startup has been removed.  Likewise, the
// memory mapping register is no longer necessary and is not implemented here.
// "Writable EPROM" sounds wierd, but it's really nothing more than simple
// block RAM that's preloaded with the BTS6120 image via the FPGA configuration
// bitstream.
//
//   For implementing the remainder of panel memory and ALL of main memory,
// there are two options available.  Plan A is to implement all other memory
// using a 64Kx12 SRAM external to the FPGA.  In this plan this module generates
// the control signals necessary to drive the external SRAM, but no block RAM
// (other than what's already allocated to the BTS6120 "writable EPROM") is
// used.  Plan A is used when the preprocessor macro XRAM is defined.
//
//   Plan B is to implement main memory, or as much of it as we can fit, using
// only FPGA block RAM. Panel memory beyond what's used by the BTS6120 writable
// EPROM image is not implemented.  No external SRAM is used in this plan. This
// plan has the advantage that no external SRAM is needed, but the disadvantage
// that most FPGAs do not have enough block RAM to implement a full 32Kx12
// main memory.  Plan B is selected when the XRAM preprocessor macro is NOT
// defined.
//
//   One last comment - the original SBC6120 also had several different RAMdisk
// daughter cards.  These were all memory mapped in one way or another using
// the same memory mapping register that controlled panel RAM and EPROM access.
// This RAMdisk option is not implemented in the SBC6120/V either.  Some 
// SBC6120/V versions do implement a completely different RAMdisk using SDRAM
// on the FPGA development board, however this uses an IOT based interface and
// is completely unrelated to the original SBC6120 RAMdisk.  
//
// REVISION HISTORY:
// 20-Jan-11  RLA  New file.
// 28-Dec-13  RLA  Major rewrite - implement "writable EPROM".  Remove mapping
//		    register and RAMdisk modules.  Implement internal block RAM
//		    option.
//--
//000000001111111111222222222233333333334444444444555555555566666666667777777777
//234567890123456789012345678901234567890123456789012345678901234567890123456789
`include "sbc6120_h.v"	// project wide definitions


module mem_eprom 
  //++
  //   This module implements the portion of control panel memory that is
  // preloaded with the BTS6120 firmware.  It's the "writable EPROM" that you
  // read about above.  It uses six RAMB16_S4 objects, each 4K by 4 bits, to
  // build an 8K by 12 bit workd memory.  This RAM isn't initialized here, but
  // before we can actually load it into the FPGA the Xilinx data2mem utility
  // must be used to insert the BTS6120 image into the bitstream.
  //
  //   One problem with data2mem is that we must know the exact instance names
  // of all the block RAMs used in this module and where they fit in the memory
  // map.  The eprom.bmm file describes this layout and is required by data2mem
  // so that it can know which bits to put where.  An unfortunate consequence
  // of this is that we really can't afford to let XST infer anything.  We have
  // to spell out all the BRAMs and how they're connected, and give them known
  // instance names.  FWIW, there might be some clever way to do this with
  // a generate loop, but I haven't figured that one out.
  //
  //   BTW, if you're wondering why a dual port RAM is used here, read the
  // comments about timing in the mem_internal_ram module.
  //--
(
  input cpuclk,		// CPU system clock
  // Shared address and data busses ...
  input [`WORD] ax,	// memory address and IOT opcode
  input [`EMA] ema,	// extended memory address bits
  inout [`WORD] dx,	// memory and I/O data
  // Memory control signals ...
  input ce,	        // panel address space is selected
  input mrd,		// read panel memory
  input mwr		// write panel memory
);
  wire [`WORD] do_f0, do_f1;  wire en_f0, en_f1;
  assign en_f0 = ce & (ema == 3'o0);
  assign en_f1 = ce & (ema == 3'o1);

  RAMB16_S4_S4 eprom_f0h (.DOA(do_f0[8:11]), .ADDRA(ax), .CLKA(~cpuclk), .DIA(    4'b0), .ENA(en_f0), .SSRA(1'b0), .WEA(1'b0),
                          .DOB(           ), .ADDRB(ax), .CLKB( cpuclk), .DIB(dx[8:11]), .ENB(en_f0), .SSRB(1'b0), .WEB(mwr));
  RAMB16_S4_S4 eprom_f0m (.DOA(do_f0[ 4:7]), .ADDRA(ax), .CLKA(~cpuclk), .DIA(    4'b0), .ENA(en_f0), .SSRA(1'b0), .WEA(1'b0),
                          .DOB(           ), .ADDRB(ax), .CLKB( cpuclk), .DIB(dx[ 4:7]), .ENB(en_f0), .SSRB(1'b0), .WEB(mwr));
  RAMB16_S4_S4 eprom_f0l (.DOA(do_f0[ 0:3]), .ADDRA(ax), .CLKA(~cpuclk), .DIA(    4'b0), .ENA(en_f0), .SSRA(1'b0), .WEA(1'b0),
                          .DOB(           ), .ADDRB(ax), .CLKB( cpuclk), .DIB(dx[ 0:3]), .ENB(en_f0), .SSRB(1'b0), .WEB(mwr));

  RAMB16_S4_S4 eprom_f1h (.DOA(do_f1[8:11]), .ADDRA(ax), .CLKA(~cpuclk), .DIA(    4'b0), .ENA(en_f1), .SSRA(1'b0), .WEA(1'b0),
                          .DOB(           ), .ADDRB(ax), .CLKB( cpuclk), .DIB(dx[8:11]), .ENB(en_f1), .SSRB(1'b0), .WEB(mwr));
  RAMB16_S4_S4 eprom_f1m (.DOA(do_f1[ 4:7]), .ADDRA(ax), .CLKA(~cpuclk), .DIA(    4'b0), .ENA(en_f1), .SSRA(1'b0), .WEA(1'b0),
                          .DOB(           ), .ADDRB(ax), .CLKB( cpuclk), .DIB(dx[ 4:7]), .ENB(en_f1), .SSRB(1'b0), .WEB(mwr));
  RAMB16_S4_S4 eprom_f1l (.DOA(do_f1[ 0:3]), .ADDRA(ax), .CLKA(~cpuclk), .DIA(    4'b0), .ENA(en_f1), .SSRA(1'b0), .WEA(1'b0),
                          .DOB(           ), .ADDRB(ax), .CLKB( cpuclk), .DIB(dx[ 0:3]), .ENB(en_f1), .SSRB(1'b0), .WEB(mwr));

  assign dx =   (en_f0    & mrd) ? do_f0
             :  (en_f1    & mrd) ? do_f1
             :  (ce       & mrd) ? 12'o7777
	     :                     12'ozzzz;
endmodule // mem_eprom


`ifndef XRAM
module mem_main_memory 
  //++
  //   This module implements a main memory using only internal block RAMs.
  // Three RAMB16_S4 BRAMs, each 4K by 4 bits, are needed for every PDP-8
  // field.  Unlike the mem_eprom module, these block RAMs are not initialized
  // and we don't care about their instance names, so we can use a generate
  // loop to create them. 
  //
  //   The CPU module reads memory during the first (positive) half of the CPU
  // clock.  Memory data is latched halfway thru the clock cycle, on the falling
  // (negative) clock edge. Memory is written during the second (negative) half
  // of the CPU clock cycle and the memory is expected to latch data on the
  // rising edge of the CPU clock.  A simple single port BRAM is can't be used
  // because it expects the same clock for reading and writing, but fortunately
  // the dual port BRAM does allow separate clocks for each port.  In this case
  // the A port is dedicated to reading and the B port is dedicated to writing.
  // It sounds messy but works just fine, and doesn't cost us any extra hardware
  // because all the necessary logic is already part of the BRAM block.
  //--
(
  input cpuclk,		// CPU system clock
  // Shared address and data busses ...
  input [`WORD] ax,	// memory address and IOT opcode
  input [`EMA] ema,	// extended memory address bits
  inout [`WORD] dx,	// memory and I/O data
  // Memory control signals ...
  input ce,		// RAM address space is decoded
  input mrd,		// drive memory data onto the dx bus
  input mwr		// write the DataBus to memory
);
  genvar i;
  generate
    for (i = 0;  i < `IRAM_FIELDS;  i = i + 1)
    begin : ramgen
      wire [`WORD] do;  wire en;
      assign en = ce & (ema == i);
      RAMB16_S4_S4 ramh (.DOA(do[8:11]), .ADDRA(ax), .CLKA(~cpuclk), .DIA(    4'b0), .ENA(en), .SSRA(1'b0), .WEA(1'b0),
                         .DOB(        ), .ADDRB(ax), .CLKB( cpuclk), .DIB(dx[8:11]), .ENB(en), .SSRB(1'b0), .WEB(mwr));
      RAMB16_S4_S4 ramm (.DOA(do[ 4:7]), .ADDRA(ax), .CLKA(~cpuclk), .DIA(    4'b0), .ENA(en), .SSRA(1'b0), .WEA(1'b0),
                         .DOB(        ), .ADDRB(ax), .CLKB( cpuclk), .DIB(dx[ 4:7]), .ENB(en), .SSRB(1'b0), .WEB(mwr));
      RAMB16_S4_S4 raml (.DOA(do[ 0:3]), .ADDRA(ax), .CLKA(~cpuclk), .DIA(    4'b0), .ENA(en), .SSRA(1'b0), .WEA(1'b0),
                         .DOB(        ), .ADDRB(ax), .CLKB( cpuclk), .DIB(dx[ 0:3]), .ENB(en), .SSRB(1'b0), .WEB(mwr));
      assign dx = (en & mrd) ? do : 12'ozzzz;
    end // block: ramgen
  endgenerate
endmodule // mem_main_memory
`endif //  `ifndef XRAM


`ifdef XRAM
module mem_external_ram 
  //++
  //   This module implements an interface to an external, 64Kx12, SRAM.  This
  // SRAM is used for all of main memory space and the remainder of panel memory
  // space (everything except what's implemented by mem_eprom).  The signals
  // xram_oe_n, xram_we_n, xram_ma and xram_md are generated to control the
  // external SRAM chip.  No chip enable is generated for the SRAM - it's
  // assumed to be permanently enabled is controlled with OE and WE only.
  //
  //   Notice that we're using the external SRAM in a "WE controlled write
  // mode" (i.e. CE is permanently asserted) and any glitches on the WE output
  // can potentially corrupt the RAM contents. Because of that a separate write
  // clock is required which is ANDed with mwr to produce the actual xram_we_n
  // output.  This write clock should be asserted at some point in the 
  // instruction where we're sure mwr is stable.  For practical purposes it's
  // sufficient to make the write clock be the inverse of the CPU clock, so
  // xram_we_n gets asserted during the second half of a machine cycle.
  //--
(
  input cpuclk,		// system CPU clock
  // Shared address and data busses ...
  input [`WORD] ax,	// memory address and IOT opcode
  input [`EMA] ema,	// extended memory address bits
  inout [`WORD] dx,	// memory and I/O data
  // Memory control signals ...
  input ce,		// RAM address space is decoded
  input cpmem,		// memory reference is to CP memory
  input mrd,		// drive memory data onto the dx bus
  input mwr,		// write the DataBus to memory
  // 64Kx12 external SRAM interface ...
  output [15:0] xram_ma,// SRAM address (16 bits)
  inout  [11:0] xram_md,// SRAM data (12 bits)
  output xram_oe_n,	// SRAM output enable
  output xram_we_n	// SRAM write enable
);
  assign xram_ma = {cpmem, ema, ax};
  assign xram_oe_n = ~(mrd & ce);
  assign dx = (mrd & ce) ? xram_md : 12'ozzzz;
  assign xram_we_n = ~(mwr & ce & ~cpuclk);
  assign xram_md = (mwr & ce) ? dx : 12'ozzzz;
endmodule // mem_external_ram
`endif //  `ifdef XRAM


module memory 
  //++
  //   This module pulls together all the previous modules to implement a
  // complete memory subsystem for the SBC6120/V.
  //--
(
  input cpuclk,		// system clock 
  // Shared address and data busses ...
  input [`WORD] ax,	// memory address bus
  input [`EMA] ema,	// extended memory address bits
  inout [`WORD] dx,	// memory and I/O data
`ifdef XRAM
  // 64Kx12 external SRAM interface ...
  output [15:0] xram_ma,// SRAM address (16 bits)
  inout  [11:0] xram_md,// SRAM data (12 bits)
  output xram_oe_n,	// SRAM output enable
  output xram_we_n,	// SRAM write enable
`endif //  `ifdef XRAM
  // Memory control signals ...
  input mrd,		// drive memory data onto the dx bus
  input mwr,		// write the DataBus to memory
  input cpmem		// asserted if memory reference is to CP memory
 );

  wire 	eprom_ce, ram_ce;
  assign eprom_ce =  cpmem & (ema <  `EPROM_FIELDS);
  assign ram_ce   = ~cpmem | (ema >= `EPROM_FIELDS);

  // Implement an intialized RAM for BTS6120 ...
  mem_eprom eprom (
    .cpuclk(cpuclk), .ax(ax), .ema(ema), .dx(dx),
    .mrd(mrd), .mwr(mwr), .ce(eprom_ce)
  );

  // Implement a RAM, either external or internal ...
`ifdef XRAM
  mem_external_ram xram (
    .cpuclk(cpuclk), .ax(ax), .ema(ema), .dx(dx), 
    .ce(ram_ce), .mrd(mrd), .mwr(mwr), .cpmem(cpmem),
    .xram_ma(xram_ma), .xram_md(xram_md),
    .xram_oe_n(xram_oe_n), .xram_we_n(xram_we_n)
  );
`else // !`ifdef XRAM
  mem_main_memory iram (
    .cpuclk(cpuclk), .ax(ax), .ema(ema), .dx(dx), 
    .ce(ram_ce), .mrd(mrd), .mwr(mwr)
  );
`endif // !`ifdef XRAM
endmodule // memory
