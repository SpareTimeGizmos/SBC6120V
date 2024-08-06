# SBC6120V
  The SBC6120V is a complete Verilog FPGA Implementation of the SBC6120 (see https://github.com/SpareTimeGizmos/SBC6120V.git).  This includes an implementation of the HD6120 CPU chip, a VT52 terminal using a PS/2 and VGA display, and various other peripherals.  A prototype including an FP6120 style front panel was built (called "Blue" because of the color scheme used) however it never made it to product status and was never sold.  Implementations of the SBC6120V are also included here for the iVerilog simulator, the Digilent S3 board, and the Spare Time Gizmos UME3 FPGA board.

## Quick Outline of Files
HEADER FILES
sbc6120_h.v	- project global declarations
opcodes_h.v	- PDP-8/HD6120 opcode mnemonics

SOURCE FILES
hd6120.v	- HD6120 CPU implementation
memory.v	- memory subsystem (RAM, EPROM and MMU)
console.v	- KL8/E compatible SLUs, including console
cfcard.v	- CompactFlash card interface
sdcard.v	- experimental MMC/SD card interface
ds1302.v	- Dallas DS1302 RTC interface
uart.v		- 6402 compatible UART implementation for SLUs
keyboard.v	- PS/2 keyboard interface for VT52 console
display.v	- VGA display interface for VT52 console
vt52.v		- VT52 terminal emulation state machine
fp6120.v	- FP6120 style front panel interface for blue
sbc6120_blue.v	- top level module for the Spare Time Gizmos "blue" hardware
sbc6120_s3.v	- top level module for the Digilent S3 board
sbc6120_ume2.v	- top level module for the Spare Time Gizmos UME2 board

TEST BENCHES
hd6120_tb.v	- HD6120 module tests (runs MAINDEC diagnostics)
sbc6120_tb.v	- SBC6120 test bench for simulation (boots BTS6120)
vt52_tb.v	- VT52 for implementation (uses UART to make a terminal)
