//++
//display.v - VGA text mode display driver
//Copyright (C) 2011-2014 by Spare Time Gizmos.  All rights reserved.
//
// DESCRIPTION:
//   This module generates an 80x24 text display on a standard VGA monitor. The
// video timing used is 720x350 @70Hz with a 9x14 character font.  This timing
// is actually the original IBM PC monochrome display adapter (aka MDA) text
// mode that's been around since the beginning of time, and all VGA monitors
// support it.  This mode was picked rather than the more common 60x480 @60Hz
// because 720x350 is much more friendly for generating a text display with a
// reasonable sized font and a reasonable number of character rows and columns.
// The required pixel clock for this mode is 28.322MHz, which is generated
// externally to this file.  One of the DCMs built into the Spartan-3 devices
// is perfect for the job.  Just a hint - 50Mhz * 17/30 = 28.333MHz, which is
// about a 0.02% error.  That's pretty darn close!
//
//   In addition to the pixel clock, this file interfaces with the rest of the
// VT52 logic thru a fairly simple interface.  The only other interface elements
// are a frame buffer RAM, which contains 80x24 ASCII characters, and a cursor
// position.  The frame buffer RAM would usually be a dual ported RAM with a
// read only port on this side, and the cursor location is just two registers
// that give the current X and Y location of the cursor, in  text cells.  It's
// pretty simple actually.
//
// REVISION HISTORY:
// 28-Jan-11  RLA  New file.
// 28-Dec-13  RLA  Clean up some constants for ISE 14.7
//  1-Jan-14  RLA  Remove global reset net.  Add initial blocks instead.
//--
//000000001111111111222222222233333333334444444444555555555566666666667777777777
//234567890123456789012345678901234567890123456789012345678901234567890123456789
`include "vt52_h.v"		// global declarations for this project


module vga_timing
  //++
  //   This module generates all the standard VGA timing signals including
  // the horizontal and vertical sync pulses and blanking signals, the
  // character clock, and the current column and row addresses for the frame
  // buffer.
  //
  //   One note of caution - the hactive, hsync and cur_col address are all in
  // perfect sync with each other.  This might seem obvious, but remember that
  // the frame buffer/character generator/video shifter logic delay the video
  // output by three character times.  This means that hsync, hactive and
  // (later) curs_here have to be delayed by the same amount or they'll be off
  // by three columns!
  //--
(
  input pixelclk,		    	// pixel clock
  output reg hsync, vsync,	   	// sync pulses to monitor
  output reg hactive, vactive, 		// active video part of the frame
  output char_tick,	            	// last clock of each character cell
  output reg [`COL_BITS] cur_col,	// current character column address 
  output reg [`ROW_BITS]  cur_row,	// current character row address
  output reg [`FONT_BITS] scanline,	// current scan line within row
  output frame_tick		    	// true for one pixel every frame
 );
  //   Note that all the timing constants are defined in vt52.h ...
  // Yes, many of them could have been parameters to this module, but several
  // (e.g. font sizes) are used in other modules as well.  Better to keep them
  // all in one place.
  reg [`FONT_BITS] pixel_count;
  reg [`COL_BITS] char_count;  wire line_tick;
  reg [`V_BITS] line_count;

  // Define the initial state ...
  initial begin
    pixel_count = 0;  scanline = 0;  cur_row = 0;  cur_col = 0;
    char_count = 0;  hsync = ~`H_POLARITY;  hactive = 0;
    line_count = 0;  vsync = ~`V_POLARITY;  vactive = 0;
  end
  
  //   Divide the pixel clock to get the character clock ...   Note that the
  // char_tick output is asserted for the last clock cycle of every character
  // cell - this is used to update the frame buffer, character generator and
  // video shifter with the data for the next cell.
  always @(posedge pixelclk)
    pixel_count <= char_tick ? 1'b0 : pixel_count+1'b1;
  assign char_tick = pixel_count == `H_FONT-1;

  //   Now generate hsync and hactive by counting character cells.  Note that
  // all timing is relative to the leading edge of the sync pulse, and also
  // that line_tick is asserted for the last clock of each scan line.
  always @(posedge pixelclk)
    if (char_tick) begin
      if (char_count == `H_SYNC_WIDTH-1)               hsync   <= `H_POLARITY;
      if (char_count == `H_SYNC_WIDTH+`H_BACK_PORCH-1) hactive <= 1;
      if (char_count == `H_TOTAL-`H_FRONT_PORCH-1)     hactive <= 0;
      if (line_tick) begin
	char_count <= 0;  hsync <= ~`H_POLARITY;  hactive <= 0;
      end else
	char_count <= char_count + 1'b1;
    end
  assign line_tick = (char_count == `H_TOTAL-1) & char_tick;
    
  // The current column counts from 0..79 during the active video part ...
  always @(posedge pixelclk)
    if (char_tick) cur_col <= hactive ? cur_col + 1'b1 : 1'b0;

  //   The vertical timing is similar to the horizontal, but in this case all
  // timing is  done in scan lines rather than character cells.  And once again,
  // frame_tick is asserted for one pixel clock at the very end of each frame.
  // It's used for timing cursor blink, bell sounds, etc.
  always @(posedge pixelclk)
    if (line_tick) begin
      if (line_count == `V_SYNC_WIDTH-1)               vsync   <= `V_POLARITY;
      if (line_count == `V_SYNC_WIDTH+`V_BACK_PORCH-1) vactive <= 1;
      if (line_count == `V_TOTAL-`V_FRONT_PORCH-1)     vactive <= 0;
      if (frame_tick) begin
	line_count <= 0;  vsync <= ~`V_POLARITY;  vactive <= 0;
      end else
	line_count <= line_count + 1'b1;
    end
  assign frame_tick = (line_count == `V_TOTAL-1) & line_tick;

  //   And this logic generates the scan line and character row outputs.  Notice
  // that the scan line output is actually the scan line within each character
  // row/glyph and it's not the same as vertical line count.
  always @(posedge pixelclk)
    if (line_tick) begin
      if (vactive) begin
	if (scanline == `V_FONT-1) begin
	  scanline <= 0;
	  cur_row <= (cur_row == `MAX_ROW) ? 1'b0 : cur_row+1'b1;
	end else
	  scanline <= scanline + 1'b1;
      end else begin
	scanline <= 0;  cur_row <= 0;
      end
    end
endmodule // vga_timing


module vga_chargen
  //++
  //   This module creates a 2K byte font ROM organized as eight bits by 16 scan
  // lines and 128 ASCII character codes.  It's another Xilinx block RAM that's
  // initialized at synthesis time and never written there after.  Note that
  // like all Xilinx block RAM/ROMs, the output must be registered and clocked.
  //--
(
  input pixelclk,			// clock for updating glyph
  input char_tick,			// enables loading new glyph data
  input [`CHAR_BITS] char,		// seven bit ASCII character code
  input [`FONT_BITS] scanline,		// current scan line
  output reg [`GLYPH_BITS] glyph	// and the resulting 8 bit pixel glyph
);
  // Allocate the block ROM and load the font ...
  reg [`GLYPH_BITS] font [`FONT_SIZE-1:0];
  initial
    $readmemh("font.mem", font, 0, `FONT_SIZE-1);

  // And this is the logic for a read only port ...
  always @(posedge pixelclk)
    if (char_tick) glyph <= font[{char, scanline}];
endmodule // vga_chargen


module vga_cursor
  //++
  //   This module generates the logic necessary to display the cursor.  It
  // compares the current scan row and column with the cursor row and column
  // and, when they match, it superimposes either an underline or block cursor
  // onto the video.  Block cursors fill the entire character cell, but under-
  // line cursors appear only on one line at the bottom of the cell.  Blinking
  // cursors blink at about a 1/2Hz rate as determined by the frame clock.
  //--
(
  input pixelclk,			// basic timing clock
  input frame_tick,			// true once every vertical frame
  input [`ROW_BITS] curs_row,		// text row where the cursor should be
  input [`COL_BITS] curs_col,		//  "   column  "  "    "      "    "
  input [`ROW_BITS] cur_row,		// currently displayed text row
  input [`COL_BITS] cur_col,		//  "    "    "    "    "   column
  input [`FONT_BITS] scanline,		// current scan line within the font
  input curs_blink,			// 1 -> blinking cursor
  input curs_underline,			// 1 -> underline, 0 -> block cursor
  output reg curs_here			// the cursor should appear now
);
  reg blink_tick;  reg [5:0] blink_count;

  // Define the initial state ...
  initial begin
    blink_tick = 0;  blink_count = 0;
  end
  
  // Generate the 1/2Hz (more or less) blink clock ...
  always @(posedge pixelclk)
    if (frame_tick) begin
      if (blink_count == `BLINK_DIVISOR) begin
	blink_count <= 0;  blink_tick <= ~blink_tick;
      end else
	blink_count <= blink_count + 1'b1;
    end

  // And then a block of combinatorial logic to figure out what to do ...
  always @* begin
    curs_here = 0;
    if ((curs_row == cur_row) & (curs_col == cur_col))
      if (curs_underline) begin
	// Generate the underline cursor ...
	if ((scanline==(`V_FONT-1)) | (scanline==(`V_FONT-2)))
	  curs_here = curs_blink ? blink_tick : 1'b1;
      end else
	// Generate a block cursor ...
	curs_here = curs_blink ? blink_tick : 1'b1;
  end
endmodule // vga_cursor


module vga_pipeline
  //++
  //   This module combines the inputs from the various other modules to
  // generate the actual RGB video that goes to the monitor.  The main job
  // is to take the eight bit parallel font data from the character generator
  // and convert it to serial video, while applying the blanking an cursor
  // signals at the same time.
  //
  //   That's fairly easy but there's a complication - the BRAM frame buffer
  // output is registered, the character generator ROM output is registered,
  // and of course the video shift register is a register, so it takes no less
  // than three character ticks between the timing generator outputting a
  // column and row address and the time that data appears on the video output.
  // That wouldn't be a problem except it means that the horizontal sync,
  // active scan, and cursor here signals all have to be delayed by the same
  // amount so that they stay in sync.  That's why you see so many "early..."
  // and "delayed..." signals here.
  //
  //  BTW, the same problem applies to vsync and vactive and strictly speaking
  // those should be delayed too.  However on the scale of the vertical timing
  // a three character cell error is inconsequential and we just don't bother.
  //--
(
  input  pixelclk,		 	// 28.322MHz dot clock
  input  char_tick,		 	// TRUE every ninth pixel clock
  input  [7:0] glyph,	 		// glyph from character generator
  input  early_hsync,		 	// hsync from TimingGenerator module
  input  early_hactive,			// TRUE during active video time
  input  vactive,		 	//   "   "  "   "  "    "    "  
  input  early_cursor,		 	// curs_here flag from cursor logic
  output reg delayed_hsync,	 	// delayed hsync for the CRT
  input [`RGB] foreground,		// RGB foreground color
  input [`RGB] background,		//  "  background   "
  output reg [`RGB] video		// RGB video output to CRT
);

  // Generate the delayed active, sync and cursor signals ...
  reg d_hs_0, d_ha_0, d_cur_0, d_hs_1, d_ha_1, d_cur_1;
  reg delayed_hactive, delayed_cursor;
  always @(posedge pixelclk)
    if (char_tick) begin
      delayed_hsync  <=d_hs_1;  d_hs_1 <=d_hs_0;  d_hs_0 <=early_hsync;
      delayed_hactive<=d_ha_1;  d_ha_1 <=d_ha_0;  d_ha_0 <=early_hactive;
      delayed_cursor <=d_cur_1; d_cur_1<=d_cur_0; d_cur_0<=early_cursor;
    end

  //   Now generate the video shift register.  Note that the MSB of the font
  // is the leftmost pixel on the screen, so the video shifter should also
  // shift left and the MSB is the current pixel.  Another subtle point is that
  // our character cell is always nine columns wide, but the font and shifter
  // only hold eight bits.  The right most pixel column is always background
  // and provides the spacing between the characters - that's easily handled
  // by always shifting zeros into the right of this shift register.
  reg [7:0] vshift;  
  always @(posedge pixelclk)
    if (char_tick) begin
      vshift <= glyph;
    end else
      vshift <= {vshift[6:0], 1'b0};

  //   And finally generate the actual video according to the MSB of the shift
  // register by applying the blanking and cursor inputs.  Notice that a zero
  // in the font gives the background color, but blanking always gives black.
  // The cursor simply inverts the foreground and background colors.
  always @* begin
    if (~(delayed_hactive & vactive))
      video = `RGB_BLANKING;
    else begin
      if (delayed_cursor)
	video = vshift[7] ? background : foreground;
      else
	video = vshift[7] ? foreground : background;
    end
  end
endmodule // vga_pipeline


module vga_display
  //++
  // Now pull it all together into something nice ...
  //--
(
  input  pixelclk,			// master pixel clock for display
  // Frame buffer interface ...
  output [`COL_BITS] cur_col,		// column address for frame buffer
  output [`ROW_BITS] cur_row,		// row     "   "   "    "     "
  input  [`CHAR_BITS] cur_char,		// character from frame buffer
  output char_tick,			// read clock for frame buffer data
  // Cursor control ...
  input [`ROW_BITS] curs_row,		// text row of the cursor
  input [`COL_BITS] curs_col,		//  "   column  "    "
  // Monitor interface ...
  output frame_tick,			// 70Hz frame clock
  output hsync,				// horizontal timing signal for CRT
  output vsync,				// vertical    "   "   "     "   "
  output [`RGB] video			// RGB video to the CRT
);
  // Parameters ...
  parameter [`RGB] FOREGROUND = `RGB_YELLOW;	// RGB foreground color
  parameter [`RGB] BACKGROUND = `RGB_BLACK;	//  "  background   "
  parameter CURS_BLINK      = 1'b1;		// 1 for a blinking cursor
  parameter CURS_UNDERLINE  = 1'b1;		// 1 for an underline cursor

  // Locals ...
  wire vactive, early_hsync, early_hactive, early_cursor;
  wire [`FONT_BITS] scanline;  wire [`GLYPH_BITS] glyph;

  // Create the VGA timing ...
  vga_timing vga_timing (
    .pixelclk(pixelclk), .char_tick(char_tick), .frame_tick(frame_tick),
    .hsync(early_hsync), .vsync(vsync),
    .hactive(early_hactive), .vactive(vactive),
    .cur_col(cur_col), .cur_row(cur_row), .scanline(scanline)
  );

  // And a character generator ROM ...
  vga_chargen vga_chargen (
    .pixelclk(pixelclk), .char_tick(char_tick),
    .char(cur_char), .scanline(scanline), .glyph(glyph)
  );

  // Cursor logic ...
  vga_cursor vga_cursor (
    .pixelclk(pixelclk),
    .curs_blink(CURS_BLINK), .curs_underline(CURS_UNDERLINE),
    .curs_row(curs_row), .curs_col(curs_col),
    .cur_row(cur_row), .cur_col(cur_col),
    .scanline(scanline), .frame_tick(frame_tick), .curs_here(early_cursor)
  );

  // And finally the video pipeline ...
  vga_pipeline vga_pipeline (
    .pixelclk(pixelclk), .char_tick(char_tick),
    .glyph(glyph), .early_hsync(early_hsync), .early_hactive(early_hactive),
    .vactive(vactive), .early_cursor(early_cursor), .delayed_hsync(hsync),
    .foreground(FOREGROUND), .background(BACKGROUND), .video(video)
  );
endmodule // vga_display
