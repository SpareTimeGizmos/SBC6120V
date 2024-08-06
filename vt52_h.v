//++
//vt52_h.v - VT52 emulation with VGA display and PS/2 keyboard
//Copyright (C) 2007-2014 by Spare Time Gizmos.  All rights reserved.
//
// DESCRIPTION:
//   The VT52 emulator spans several source files, and this module provides
// the common declarations for all of them...
//
// REVISION HISTORY:
// 12-Jul-07  RLA  New file.
// 28-Dec-13  RLA  Add width specifier to some constants for ISE 14.7
//--
//000000011111111112222222222333333333344444444445555555555666666666677777777778
//345678901234567890123456789012345678901234567890123456789012345678901234567890
`ifndef vt52_h


// Standard MDA text mode (720x350@70Hz) mode display parameters ...
//
//   Notice that this mode just naturally wants to do 25 lines (25 lines * 14
// scan lines per glyph == 350) but for VT52 compatibility we coerce it into
// just 24 lines by adding 14 extra lines to the vertical blanking interval.
// The appropriate parameters for 25 lines are commented out and marked with
// a "25 LINE" comment should you ever want to switch back to that mode.
//
//   FWIW, 28 text lines should be possible in 720x400@70 mode.  The 720x350
// and 720x400 are the exact same mode as far as the monitor is concerned -
// the VBI just shrinks a bit and there are more active scan lines in the
// latter mode.


// Font geometry ...
`define H_FONT		4'd9	// character cells are 9 pixels wide
`define V_FONT		4'd14	//  ... and sixteen scan lines high
`define FONT_BITS	3:0	// bit vector size for font X and Y
`define GLYPH_BITS	7:0	// bits in a font glyph
`define FONT_SIZE (16*128)	// font ROM size (16 scanlines * 128 characters)
//
// HORIZONTAL TIMING PARAMETERS IN CHARACTER CELL TIMES !
// Note that (H_ACTIVE+H_FRONT_PORCH+H_SYNC_WIDTH+H_BACK_PORCH) == H_TOTAL!
`define H_TOTAL		7'd100	// total character times per horizontal line
`define H_ACTIVE	7'd80	// total active characters per horizontal line
`define H_FRONT_PORCH	7'd4	// delay after video but before sync pulse
`define H_SYNC_WIDTH	7'd12	// width of the sync pulse
`define H_BACK_PORCH	7'd4	// delay after sync and before active video
`define H_POLARITY	1'b1	// 1 for positive sync, 0 for negative
//
// VERTICAL TIMING PARAMETERS ARE IN SCAN LINE TIMES !
// Note that (V_ACTIVE+V_FRONT_PORCH+V_SYNC_WIDTH+V_BACK_PORCH) == V_TOTAL!
`define V_TOTAL		10'd449	// total line times per frame
`define V_ACTIVE	10'd336	// total active scan lines per frame
//`define V_ACTIVE	10'd350	// 25 LINES
`define V_FRONT_PORCH	10'd44	// delay after last line but before sync
//`define V_FRONT_PORCH	10'd37	// 25 LINES
`define V_SYNC_WIDTH	10'd2	// width of the sync pulse (in line times)
`define V_BACK_PORCH	10'd67	// delay after sync and before first line
//`define V_BACK_PORCH	10'd60	// 25 LINES
`define V_POLARITY	1'b0	// 1 for positive sync, 0 for negative
`define V_BITS		8:0	// bit vector size for vertical counters

// Text mode screen geometry ...
`define CHAR_BITS	6:0	// width of one character in text memory
`define MAX_COL		7'd79	// total horizontal character columns -1
`define MAX_ROW		5'd23	// total vertical character rows -1
//`define MAX_ROW	5'd24	// 25 LINES
`define COL_BITS	6:0	// seven bits in the column counter
`define ROW_BITS	4:0	// five bits in the row counter
`define MARGIN_COL	7'd72	// column for margin bell

// RGB video colors used ...
`define RGB_BLANKING	3'b000	// RGB for blanking (BLACK!) video
`define RGB_BLACK	3'b000	// black ...
`define RGB_RED		3'b100	// red ...
`define RGB_GREEN	3'b010	// green ...
`define RGB_YELLOW	3'b110	// yellow ...
`define RGB_BLUE	3'b001	// blue ...
`define RGB_MAGENTA	3'b101	// blue ...
`define RGB_WHITE	3'b111	// blue ...
`define RGB		2:0	// number of bits in a video value

// Cursor definitions ...
`define BLINK_DIVISOR	6'd35	// 70Hz/35 gives a 1/2Hz cursor blink rate


// PS/2 keyboard messages ...
`define PS2_RELEASE_KEY	  8'hF0	// standard prefix for key up events
`define PS2_EXTENDED_KEY  8'hE0	//  "   "    "  "   "  extended (PC/AT) keys
`define PS2_ACK		  8'hFA	// acknowledging our message
`define PS2_NACK1	  8'h00	// error status #1
`define PS2_NACK2	  8'hFF	//   "      "   #2
`define PS2_SET_LEDS	  8'hED	// set the LED states
`define PS2_RESET	  8'hFF	// reset the keyboard
`define PS2_POST_OK	  8'hAA	// self test passed
`define PS2_POST_FAIL	  8'hFC	//   "    "  failed (yeah, and now what????)

// And some special PS/2 scan codes that we're interested in ...
`define PS2_LEFT_SHIFT	  8'h12	// left shift
`define PS2_RIGHT_SHIFT	  8'h59	// right shift
`define PS2_CAPS_LOCK	  8'h58	// caps lock
`define PS2_LEFT_CONTROL  8'h14	// control
`define PS2_NUM_LOCK	  8'h77	// num lock
`define PS2_SCROLL_LOCK	  8'h7E	// scroll lock
`define PS2_KP_0	  8'h70	// keypad "0"
`define PS2_KP_1	  8'h69	//   "    "1"
`define PS2_KP_2	  8'h72	//   "    "2"
`define PS2_KP_3	  8'h7A	//   "    "3"
`define PS2_KP_4	  8'h6B	//   "    "4"
`define PS2_KP_5	  8'h73	//   "    "5"
`define PS2_KP_6	  8'h74	//   "    "6"
`define PS2_KP_7	  8'h6C	//   "    "7"
`define PS2_KP_8	  8'h75	//   "    "8"
`define PS2_KP_9	  8'h7D	//   "    "9"
`define PS2_KP_DOT	  8'h71	//   "    "."
`define PS2_KP_STAR	  8'h7C	//   "    "*"
`define PS2_KP_MINUS	  8'h7B	//   "    "-"
`define PS2_KP_SLASH	  8'h4A	//   "    "/"  (extended!)
`define PS2_KP_ENTER	  8'h5A	//   "   ENTER (extended!)
`define PS2_UP_ARROW	  8'h75	// up arrow key (extended!)
`define PS2_DOWN_ARROW	  8'h72	// down  "   "    "    "
`define PS2_LEFT_ARROW	  8'h6B	// left  "   "    "    "
`define PS2_RIGHT_ARROW	  8'h74	// right "   "    "    "

// Alternate key tables in the KeyMap ROM ...
`define KEYTABLE_KEYPAD	4'h9	// table for keypad escape sequences
`define KEYTABLE_ARROWS	4'h8	// table for arrow key escape sequences

// Special ASCII character codes ...
`define ESC		8'h1b
`define XON		8'h11
`define XOFF		8'h13
`endif // ifndef vt52_h
