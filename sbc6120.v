//++
//sbc6120.v - SBC6120/V top level module
//Copyright (C) 2011-2014 by Spare Time Gizmos.  All rights reserved.
//
// DESCRIPTION:
//   This is the top level module for the SBC6120/V SoC.  It instanciates the
// HD6120 CPU, a memory subsystem, various peripherals, and wires them all
// together into a complete system.  The only thing that's not here really
// are the physical connections to the outside world, and those are board
// specific.
//
//   There are several preprocessor macros that control the peripherals
// generated and the memory size and organization.  These can be used to adapt
// this module to different physical environments.
//
// MEMORY CONFIGURATION OPTIONS
// XRAM -	  When defined, generate the interface for an external 64Kx12
//		  SRAM.  This contains all of main memory plus whatever panel
//		  fields are NOT part of the EPROM.  When XRAM is not defined,
//		  panel memory is limited to just EPROM_FIELDS, and the FPGA's
//		  internal block RAM is used for all main memory.
//
// EPROM_FIELDS	- The number of panel memory fields allocated to EPROM, and
//  		  consequently , the size of the BTS6120 image.   Because the
//		  image is loaded via data2mem, you can't actually change the
//		  EPROM size simply by changing this value - you'll need to
//		  fool with the code in memory.v and the eprom.bmm file.
//
// IRAM_FIELDS -  The number of 4KW fields allocated to main memory using FPGA
//		  internal block RAM.  This is valid only when XRAM is NOT
//		  defined!
//
// PERIPHERAL CONFIGURATION OPTIONS
// FP6120 -	  Generate an FP6120 compatible front panel interface when
//		  defined.  When NOT defined, the system is equivalent to the
//		  original SBC6120 w/o front panel.
//
// CFCARD -       Generate an IOB6120 compatible CompactFlash card interface
//		  when defined.
//
// RTC -          Generate an IOB6120 compatible DS1302 time of day clock
//		  interface when defined.
//
// SDCARD -	  Generate a high speed SPI interface for MMC/SD cards when
//		  defined.
//
// REVISION HISTORY:
// 17-Jan-11  RLA  New file.
//  1-Jan-14  RLA  Do away with global reset net.  Add initial blocks to
//		   initialize flip flops where needed.
//--
//000000011111111112222222222333333333344444444445555555555666666666677777777778
//345678901234567890123456789012345678901234567890123456789012345678901234567890
`include "sbc6120_h.v"		// global declarations for this project


//++
//   This little module implements the SBC6120 POST display.  It's just a three
// bit register which is cleared on reset and clocked by the 644n IOT ...
//--
module post (
  // Bus interface signals ...	     
  input         clock,		// system wide clock
  input [`WORD] ax,		// address bus (carries the IOT)
  input         iowr,		// I/O write strobe
  // Outputs ...
  output reg [2:0] leds		// current post display
);
  parameter SELECT = 6'O44;	// POST IOT select address

  initial leds = 3'o7;
  
  always @(posedge clock) begin
    if (iowr & (ax[3:8]==SELECT)) leds <= ax[9:11];
  end
endmodule // post


module sbc6120 (
  // Global clock and asynchronous reset signals ...
  input  	 cpuclk,	// hd6120 (and all peripherals) clock
  // 64Kx12 SRAM interface signals ...
`ifdef XRAM
  output [15:0]  xram_ma,	// SRAM address (16 bits)
  inout  [11:0]  xram_md,	// SRAM data (12 bits)
  output 	 xram_oe_n,	// SRAM output enable
  output 	 xram_we_n,	// SRAM write enable
`endif
  // CompactFlash card interface (common memory model mode) ...
`ifdef CFCARD
  output [2:0]	 cf_a,		// register select bits
  inout  [7:0]	 cf_d,		// 8 bit data bus
  output	 cf_oe_n,	// output enable (read strobe)
  output	 cf_we_n,	// write strobe
  output	 cf_cs_n,	// card select
  output	 cf_reset,	// reset the card
  input		 cf_ready_n,	// card ready
  input		 cf_cd_n,	// card inserted
`endif
  // MMC/SD card SPI interface ...
`ifdef SDCARD
  output         mmc_sclk,	// SPI clock
  output         mmc_mosi,	// master (that's us!) our, slave in
  input          mmc_miso,	// master in, slave out
  output         mmc_cs_n,	// slave select
  input          mmc_cd_n,	// card detected
  input          mmc_wp_n,	// card is write protectedz
`endif
  // DS1302 interface ...
`ifdef RTC
  output	 rtc_sclk,	// serial clock
  output	 rtc_ce,	// chip enable and byte sync
  inout		 rtc_sio,	// bidirectional serial data
`endif
  // Front panel connections ...
`ifdef FP6120
  output [`WORD] data_leds,	// data display LEDs
  output [`WORD] addr_leds,	// address display LEDs
  output [`EMA]  ema_leds,	// EMA display LEDs
  output 	 run_led,	// RUN LED
  output 	 power_led,	// POWER LED
  input  [`WORD] swreg,		// switch register
  input  [`FNSW_BUS]  fnsw,	// function switches (9 of them)
  input  [`ROTSW_BUS] rotsw,	// rotary switch setting
`endif
  // Miscelleneous inputs and outputs ...
  output [2:0]   post_code,	// POST result
  input		 console_rxd,	// console serial input
  output	 console_txd	// console serial output
);
  // Parameters ...
  parameter CPU_CLOCK=50000000;	// cpu clock frequency
  parameter CONSOLE_BAUD= 9600;	// console serial baud rate

  // Locals ...
  wire [`WORD] ax;		// twelve bit memory and I/O address
  wire [`EMA] ema;		// three bit extended memory address
  wire [`WORD] dx;		// memory and I/O data bus
  wire mrd;			// memory read strobe
  wire mwr;			// memory write strobe
  wire cpmem;			// control panel memory select
  wire dataf;			// data field memory reference
  wire ifetch;			// instruction is being fetched
  wire ioclr;			// clear all I/O devices (CAF)
  wire iord;			// I/O read strobe
  wire iowr;			// I/O write strobe
  wire [`WORD] dispdata;	// "display" data register
  wire cpreq_console;		// panel trap request from console
  wire ioc0_console;		// I/O control #0 (clear AC) from console
  wire ioc0_cf, ioc0_mmc;	//  "     "     "    "   "   from CF/MMC card
  wire ioc1_console;		// I/O control #1 (read AC) from console
  wire ioc1_cf, ioc1_mmc;	//  "     "     "    "   "  from CF/MMC card
  wire ioskip_console;		// this console IOT should skip
  wire ioskip_cf, ioskip_mmc;	//  "   CF card  "     "     "
  wire intreq_console;		// console is requesting an interrupt
  wire intreq_cf, intreq_mmc;	// CF/MMC card  "    "    "   "   "
  wire iowait_cf, iowait_mmc;	// CF/MMC card needs to stretch this I/O cycle
  wire ioc0_rtc, ioc1_rtc;	// I/O control signals from DS1302 RTC
  wire ioskip_rtc;		// this DS1302 IOT should skip
  wire ioc0_panel, ioc1_panel;	// I/O control signals from FP6120
  wire ioskip_panel;		// this FP6120 IOT should skip
  wire cpreq_panel;		// panel trap request from FP6120
`ifndef FP6120
  wire [`WORD] swreg;           // local switch register (from CPU WSR IOT)    
`endif

  // We'll need a CPU!
  hd6120 #(.STARTUP_MODE(1)) cpu (
    .clock(cpuclk), .ax(ax), .ema(ema), .dx(dx), .reset(1'b0),
    .mrd(mrd), .mwr(mwr), .cpmem(cpmem), .ifetch(ifetch), .dataf(dataf),
    .ioclr(ioclr), .iord(iord), .iowr(iowr),
    .ioskip(ioskip_console|ioskip_cf|ioskip_mmc|ioskip_rtc|ioskip_panel),
    .ioc0(ioc0_console|ioc0_cf|ioc0_mmc|ioc0_rtc|ioc0_panel),
    .ioc1(ioc1_console|ioc1_cf|ioc1_mmc|ioc1_rtc|ioc1_panel),
    .intreq(intreq_console|intreq_cf|intreq_mmc),
    .iowait(iowait_cf|iowait_mmc), .intgnt(),
    .cpreq(cpreq_panel|cpreq_console), 
    .swin(swreg), .swout(dispdata)
  );

  // A POST display ...
  post post (  
    .clock(cpuclk), .ax(ax), .iowr(iowr), .leds(post_code)
  );

  // A memory subsystem, including both RAM and EPROM ...
  memory memory (
    .cpuclk(cpuclk), .ax(ax), .ema(ema), .dx(dx),
`ifdef XRAM
    .xram_ma(xram_ma), .xram_md(xram_md),
    .xram_oe_n(xram_oe_n), .xram_we_n(xram_we_n),
`endif
    .mrd(mrd), .mwr(mwr), .cpmem(cpmem)
  );

  // A front panel ...
`ifdef FP6120
  fp6120 #(.SYSTEM_CLOCK(CPU_CLOCK)) fp6120 (
    .clock(cpuclk), .ax(ax), .ema(ema), .dx(dx), .ifetch(ifetch),
    .mrd(mrd), .mwr(mwr), .cpmem(cpmem), .dispdata(dispdata), 
    .ioclr(ioclr), .iord(iord), .iowr(iowr), .cpreq(cpreq_panel), 
    .ioskip(ioskip_panel), .ioc0(ioc0_panel), .ioc1(ioc1_panel),
    .post(post_code), .data_leds(data_leds), .addr_leds(addr_leds),
    .ema_leds(ema_leds), .run_led(run_led), .power_led(power_led),
    .fnsw(fnsw), .rotsw(rotsw)
  );
`else
   assign ioskip_panel = 0;  assign ioc0_panel = 0;  assign ioc1_panel = 0;
   assign cpreq_panel = 0;   assign swreg = dispdata;
`endif // ifdef FP6120

  // A console terminal, including the UART ...
  console #(.SYSTEM_CLOCK(CPU_CLOCK), .BAUD_RATE(CONSOLE_BAUD))
    console (.clock(cpuclk), .ax(ax), .dx(dx), .iowr(iowr), .iord(iord),
    .ioclr(ioclr), .ioskip(ioskip_console), .ioc0(ioc0_console),
    .ioc1(ioc1_console), .intreq(intreq_console), .brkreq(cpreq_console),
    .serial_in(console_rxd), .serial_out(console_txd)
  );

  // A CompactFlash interface ...
`ifdef CFCARD
  cf_card cf_card (
    .clock(cpuclk), .ax(ax), .dx(dx), .iowr(iowr), .iord(iord),
    .ioclr(ioclr), .ioskip(ioskip_cf), .ioc0(ioc0_cf), .ioc1(ioc1_cf),
    .iowait(iowait_cf), .intreq(intreq_cf),
    .cf_a(cf_a), .cf_d(cf_d), .cf_oe_n(cf_oe_n), .cf_we_n(cf_we_n),
    .cf_cd_n(cf_cd_n), .cf_cs_n(cf_cs_n), .cf_reset(cf_reset),
    .cf_ready_n(cf_ready_n)
  );
`else
  assign ioskip_cf = 0;  assign ioc0_cf = 0;  assign ioc1_cf = 0;
  assign iowait_cf = 0;  assign intreq_cf = 0;
`endif // ifdef CFCARD

  // A MMC/SD card interface ...
`ifdef SDCARD
  sd_card sd_card (
    .clock(cpuclk), .ax(ax), .dx(dx), .iowr(iowr), .iord(iord),
    .ioclr(ioclr), .ioskip(ioskip_mmc), .ioc0(ioc0_mmc), .ioc1(ioc1_mmc),
    .iowait(iowait_mmc), .intreq(intreq_mmc),
    .mmc_sclk(mmc_sclk), .mmc_mosi(mmc_mosi), .mmc_miso(mmc_miso),
    .mmc_cs_n(mmc_cs_n), .mmc_cd_n(mmc_cd_n), .mmc_wp_n(mmc_wp_n)
  );
`else
   assign ioskip_mmc = 0;  assign ioc0_mmc = 0;  assign ioc1_mmc = 0;
   assign iowait_mmc = 0;  assign intreq_mmc = 0;
`endif // ifdef SDCARD

  // A DS1302 real time clock/calendar ...
`ifdef RTC
  rtc_1302 #(.SYSTEM_CLOCK(CPU_CLOCK))
    rtc (.clock(cpuclk), .ax(ax), .dx(dx), .iowr(iowr), .iord(iord),
    .ioclr(ioclr), .ioskip(ioskip_rtc), .ioc0(ioc0_rtc), .ioc1(ioc1_rtc),
    .rtc_sclk(rtc_sclk), .rtc_ce(rtc_ce), .rtc_sio(rtc_sio)
  );
`else
   assign ioskip_rtc = 0;  assign ioc0_rtc = 0;  assign ioc1_rtc = 0;
`endif // ifdef RTC

endmodule // sbc6120
