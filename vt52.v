//++
//vt52.v - VT52 terminal using PS/2 keyboard and VGA monitor 
//Copyright (C) 2011-2014 by Spare Time Gizmos.  All rights reserved.
//
// DESCRIPTION:
//   This module pulls together te PS/2 keyboard (keyboard.v) and the text mode
// VGA display (display.v) to make a complete VT52 compatible terminal.  It
// does pretty much everything a real VT52 can do, including process all the
// same escape sequences, alternate keypad mode, hold screen mode, and graphics
// mode.  The following text describes only the display/output side of the job;
// for information about the PS2 to VT52 key mappings, check the keyboard.v
// file.  Having said that, here's a few highlights of the VT52 emulation -
//
//   * Handles these incoming ASCII control characters -
//
//	* Sound Bell				(<BEL>, 0x07)
//	* Cursor Left				(<BS>,  0x08)
//	* Move to Tab Stop			(<HT>,  0x09)
//	* Cursor Down or Scroll Up		(<LF>,  0x0A or <VT>, 0x0B)
//	* Home Cursor and Erase Screen		(<FF>,  0x0C)
//	* Cursor to Beginning of Line		(<CR>,  0x0D)
//	* Select Normal Character Set		(<SO>,  0x0E)
//	* Select Graphics Character Set		(<SI>,  0x0F)
//	* Escape Sequence			(<ESC>, 0x1B)
//
//   * Handles these VT52 escape sequences -
//
//	* Cursor Up, Down, Left, Right, Home	(<ESC> A, B, C, D, H)
//	* Scroll Up/Down			(LF, <ESC> I)
//	* Erase to End of Line/Screen		(<ESC> K, J)
//	* Direct Cursor Addressing		(<ESC> Y <row> <col>)
//	* Enter/Exit Graphics Mode		(<ESC> F, G)
//	* Alternate/normal Keypad Mode		(<ESC> =, >)
//	* Enter/Exit Hold Screen Mode		(<ESC> [, \)
//	* Identify				(<ESC> Z)
//
//   * The graphics character set is implemented, bowever this uses the VT100
// version of the font (i.e. the glyphs that you'd get if you had a VT100 set
// to VT52 emulation mode) rather than the actual VT52 glyphs.  I actually like
// the VT100 versions better!
//
//   * Yes, hold screen mode is implemented and it will automatically send XON
// and XOFF to the host as needed.  A 64 character receiver FIFO is implemented
// to allow the host a moment to respond to XOFF, but that's not going to last
// very long at 19200 baud and so the host MUST implement XON/XOFF flow control
// for hold screen mode to work.
//
//   * It responds to identify (<ESC> Z) by sending <ESC> / K - that is, a VT52
// with no printer.
//
//   * The bell is supported, although of course that depends on the hardware.
// The vt52 module simply provides an output signal, bell, which is asserted
// for one clock whenever the bell should sound.  It's up to the parent module
// to connect that to some real hardware and make a sound.
//
// REVISION HISTORY:
// 28-Jan-11  RLA  New file.
// 28-Dec-13  RLA  Clean up some constants for ISE 14.7
//  1-Jan-14  RLA  Remove global reset net.  Add initial blocks where needed.
//--
//000000011111111112222222222333333333344444444445555555555666666666677777777778
//345678901234567890123456789012345678901234567890123456789012345678901234567890
`include "vt52_h.v"


module vt52_frame_buffer
  //++
  //   This module creates a block RAM for the frame buffer.  Remember that
  // this is only a text mode terminal so the frame buffer contains 24 rows of
  // 80 characters, or just 1920 bytes.  t'll fit nicely into a 2K BRAM slice,
  // but we have to do a little work to compute "row*80" when we're figuring
  // the frame buffer address.  
  //
  //   The frame buffer is dual ported RAM - one port is the "display" side and
  // is read only, and the other port is the "vt52 controller" side.  That one
  // is R/W, although the current implementation does not use the read feature
  // on the controller side.
  //
  //   Lastly, there's one additional input called "top_row".  This is an offset
  // that's added to the row address, modulo 24.  This allows the screen to be
  // shifted for scrolling.
  //--
(
  // System interface ...
  input clock,				// system wide clock
  // Display side interface ...
  input char_tick,			// read clock for the frame buffer side
  input [`COL_BITS] disp_col,		// frame buffer column address
  input [`ROW_BITS] disp_row,		// frame buffer row address
  input [`ROW_BITS] top_row,
  output reg [`CHAR_BITS] disp_char,	// latched frame buffer data
  // Controller side interface ...
  input [`COL_BITS] vt_col,		// controller column address
  input [`ROW_BITS] vt_row,		// controller row address
//input vt_read,			// read clock for the controller side
//output reg [`CHAR_BITS] vt_dout,	// controller read data out
  input vt_write,			// controller write enable
  input [`CHAR_BITS] vt_din		// controller data in for writing
);
  parameter WIDTH      = 7;		// frame buffer width, in bits
  parameter LOG2_SIZE  = 11;		// log2(frame buffer size)
  reg [WIDTH-1:0] fb [2**LOG2_SIZE-1:0];


  function [10:0] RowBase(input [5:0] row);
  //++
  //   This little function computes "(row mod 24) * 80" for the range 0 <=
  // row <= 48.  It's used to compute the base address for a text row in the
  // frame buffer, of course.  The reason the range goes up to 48 and also
  // for the mod 24 is to allow for scrolling - the input here is the result
  // after top_row has been added to the current row.
  //
  //   It's ugly, but XST will synthesize it as a 64 word x 11 bit ROM and it
  // actually takes surprisingly little logic.  BRAMs are way more precious
  // than CLBs and it's well worth the extra effort to pack the entire display
  // into a single BRAM.
  //
  //   There's probably some nifty way to do this with the Verilog generate
  // statement, but I can't figure it out...
  //--
  begin
    case (row)
       //   This rather awkward way of writing ("MAX_ROW+nn") has the advantage
       // of working even if MAX_ROW is changed, say to 25!
       0, `MAX_ROW+ 1:   RowBase = 80 *  0;
       1, `MAX_ROW+ 2:   RowBase = 80 *  1;
       2, `MAX_ROW+ 3:   RowBase = 80 *  2;
       3, `MAX_ROW+ 4:   RowBase = 80 *  3;
       4, `MAX_ROW+ 5:   RowBase = 80 *  4;
       5, `MAX_ROW+ 6:   RowBase = 80 *  5;
       6, `MAX_ROW+ 7:   RowBase = 80 *  6;
       7, `MAX_ROW+ 8:   RowBase = 80 *  7;
       8, `MAX_ROW+ 9:   RowBase = 80 *  8;
       9, `MAX_ROW+10:   RowBase = 80 *  9;
      10, `MAX_ROW+11:   RowBase = 80 * 10;
      11, `MAX_ROW+12:   RowBase = 80 * 11;
      12, `MAX_ROW+13:   RowBase = 80 * 12;
      13, `MAX_ROW+14:   RowBase = 80 * 13;
      14, `MAX_ROW+15:   RowBase = 80 * 14;
      15, `MAX_ROW+16:   RowBase = 80 * 15;
      16, `MAX_ROW+17:   RowBase = 80 * 16;
      17, `MAX_ROW+18:   RowBase = 80 * 17;
      18, `MAX_ROW+19:   RowBase = 80 * 18;
      19, `MAX_ROW+20:   RowBase = 80 * 19;
      20, `MAX_ROW+21:   RowBase = 80 * 20;
      21, `MAX_ROW+22:   RowBase = 80 * 21;
      22, `MAX_ROW+23:   RowBase = 80 * 22;
      23, `MAX_ROW+24:   RowBase = 80 * 23;
      //   It'd be nice if there was a "`if ..." rather than just "`ifdef ..."
      // in Verilog, but there isn't so you'll have to manually add this line
      // back if you switch to 25 lines..
      // 25 LINES	24, `MAX_ROW+25:   RowBase = 80 * 24;
      default:        RowBase = 80 * (`MAX_ROW+1);
    endcase // case (row)
  end
  endfunction // RowBase


  //   BTW, Xilinx block RAMs are really limited - if you have a chip enable
  // signal then it has to be shared by both the read and write function.  If
  // you ever decide to implement the controller side read port, then you
  // can't say something like "if (vt_write) ... fb <= ..." and "if (vt_read)
  // ... <= fb..." because that looks like two separate chip enables.  Instead
  // you'll have to do it the way it's written below, with "vt_read|vt_write"
  // as the chip enable and vt_write along as the write enable.  Of course that
  // also means the read port contents will be trashed on any write access, but
  // that's probably harmless.

  wire [LOG2_SIZE-1:0] vt_addr, disp_addr;
  assign vt_addr = RowBase(top_row+vt_row) + vt_col;
  assign disp_addr = RowBase(top_row+disp_row) + disp_col;
  always @(posedge clock) begin
//  if (vt_read | vt_write) begin
      if (vt_write) fb[vt_addr] <= vt_din;
//    vt_dout <= fb[vt_addr];
//  end
    if (char_tick) disp_char <= fb[disp_addr];
  end
endmodule // vt52_frame_buffer


module vt52_controller
  //++
  //   Here it is - this state machine takes characters from the host and
  // processes them.  Usually that just involves putting them on the screen
  // and updating the cursor, but for control characters and escape sequences
  // it gets more complicated.  
  //--
#(
  parameter AUTO_NEWLINE = 0, 	   // 1 -> auto newline mode (wrap) enabled
  parameter MARGIN_BELL  = 0	   // 1 -> sound bell at right margin
) (
  // Global stuff ...
  input clock,			   // clock (usually the pixel clock)
  // Frame buffer interface ...
  output reg [`COL_BITS] fb_col,   // frame buffer column address
  output reg [`ROW_BITS] fb_row,   //   "     "    row     "   "
//  output reg fb_read,		   // read frame buffer, latch fb_din
//  input [`CHAR_BITS] fb_din,	   // data read from frame buffer
  output reg fb_write,   	   // write frame buffer with fb_dout
  output reg [`CHAR_BITS] fb_dout, // data to write to frame buffer
  // Cursor and scrolling ...
  output reg [`COL_BITS] curs_col, // current cursor column 
  output reg [`ROW_BITS] curs_row, //   "       "    row
  output reg [`ROW_BITS] top_row,  // top row on the screen after scrolling
  // Serial interface/input stream ...
  input [`CHAR_BITS] rx_data,	   // last character/data byte received
  input rx_ready,		   // rx_data is valid
  output reg rx_clear,		   // clear rx_ready
  input host_silo_full,		   // the host receiver FIFO is full
  // Miscellaneous ...
  output reg bell,		   // trigger bell sound
  // Keyboard controller (ps2_keyboard in keyboard.v) interface ...
  output reg set_alt_keypad_mode,  // select alternate keypad mode
  output reg reset_alt_keypad_mode,// deselect  "   "    "     "
  output reg hold_screen,	   // current hold screen state
  input proceed_one_line,	   // user pressed Scroll Lock
  input proceed_one_screen,	   //  "    "   "  shift/Scroll Lock
  input toggle_hold_screen,	   //  "    "   "  control/Scroll Lock
  output reg send_identify	   // trigger identify response
);

  // These are the states of our VT52 controller ...
  localparam [4:0]
    INIT		=  0,	// initialize
    DONE		=  1,	// clear rx_ready, go to idle
    IDLE		=  2,	// wait for rx_ready, decode character
    ESCAPE0		=  3,	// decode escape sequence, part 1
    ESCAPE1		=  4,	//   "      "     "    "     "  2
    NORMAL0		=  5,	// write printing character to screen, part 1
    NORMAL1		=  6,	//   "    "    "    "    "   "    "      "  2
    CURS_LEFT		=  7,	// move cursor left
    CURS_RIGHT		=  8,	//  "      "   right
    CURS_UP		=  9,	//  "      "   up
    CURS_DOWN		= 10,	//  "      "   down
    CURS_TAB		= 11,	//  "      "   to next tab stop
    CURS_RET		= 12,	//  "      "   to start of line
    CURS_HOME		= 13,	//  "      "   to top left corner of screen
    SCROLL_DOWN		= 14,	// scroll screen down
    SCROLL_UP		= 15,	//   "      "    up
    LINEFD		= 16,	// cursor down or scroll up
    RLINEFD		= 17,	// cursor up or scroll down
    ERASE_CURS_EOL	= 18,	// erase from cursor to end of line
    ERASE_CURS_EOS	= 19,	//   "     "    "    "  end of screen
    ERASE_EOL		= 20,	// erase from (fb_col, fb_row) to end of line
    ERASE_EOS		= 21,	//   "     "     "       "      " end of screen
    CLEAR_SCREEN	= 22,	// home cursor, clear screen
    BELL		= 23,	// sound bell
    DIRECT0		= 24,	// direct cursor addressing, part 1
    DIRECT1		= 25,	//   "      "      "    "     "   2
    DIRECT2		= 26,	//   "      "      "    "     "   3
    HOLD_SCREEN		= 27;	// screen frozen for a while
  reg [4:0] state, next;

  // Other internal state data for the VT52 ...
  reg graphics_mode;		// use graphics character set
  reg [`ROW_BITS] hold_count;	// number of lines scrolled in hold screen mode

  // Next state values for everything ...
  reg [`COL_BITS] next_fb_col, next_curs_col;
  reg [`ROW_BITS] next_fb_row, next_curs_row, next_top_row, next_hold_count;
  reg next_graphics_mode, next_hold_screen;
  

  task DecodeControl;
  //++
  //   Decode the control character in rx_data and set the next to
  // something appropriate ...
  //--
  begin
    next = DONE;  // the "default" action is to ignore
    case (rx_data)
      8'h07: next = BELL;		// ^G BEL - ring the "bell" 
      8'h08: next = CURS_LEFT;		// ^H BS  - cursor left (backspace)
      8'h09: next = CURS_TAB;		// ^I HT  - move right to next tab stop
      8'h0A: next = LINEFD;		// ^J LF  - cursor down or scroll up
      8'h0B: next = LINEFD;		// ^K VT  - same as line feed
      8'h0C: next = CLEAR_SCREEN;	// ^L FF  - home and erase screen
      8'h0D: next = CURS_RET;		// ^M CR  - move cursor to left margin
      8'h0E: next_graphics_mode = 0;	// ^N SO  - normal character set
      8'h0F: next_graphics_mode = 1;	// ^O SI  - graphics character set
      8'h1B: next = ESCAPE0;		// ^[ ESC - escape sequences
      default: /* do nothing */ ;
    endcase
  end
  endtask

  task DecodeEscape;
  //++
  //   Decode the character in rx_data, which is assumed to be the second
  // character of an escape sequence, and set next accordingly...
  //--
  begin
    next = DONE;  // the "default" action is to ignore
    case (rx_data)
      "A": next = CURS_UP;		// Cursor Up
      "B": next = CURS_DOWN;		// Cursor Down
      "C": next = CURS_RIGHT;		// Cursor Right
      "D": next = CURS_LEFT;		// Cursor Left
      "H": next = CURS_HOME;		// Cursor Home
      "I": next = RLINEFD;		// Reverse Line Feed
      "J": next = ERASE_CURS_EOS;	// Erase to end of Screen
      "K": next = ERASE_CURS_EOL;	// Erase to end of Line
      "Y": next = DIRECT0;		// Direct Cursor Addressing
      // These don't really need their own states ...
      "Z": send_identify = 1; 		// Identify
      "F": next_graphics_mode = 1;  	// Alternate Character Set
      "G": next_graphics_mode = 0;	// ASCII Character Set
      "=": set_alt_keypad_mode = 1;	// alternate keypad mode
      ">": reset_alt_keypad_mode = 1;	// normal keypad mode
      "[": next_hold_screen = 1;	// enter hold screen mode
     "\\": next_hold_screen = 0;	// exit hold screen mode
      default: /* do nothing */;
    endcase
  end
  endtask

  // Define the initial state ...
  initial begin
    state = INIT;  hold_screen = 0;
  end

  // Synthesize the state register and all the associated state bits ...
  always @(posedge clock) begin
    fb_col  <= next_fb_col;  curs_col <= next_curs_col;
    fb_row  <= next_fb_row;  curs_row <= next_curs_row;
    top_row <= next_top_row;  hold_count <= next_hold_count;
    graphics_mode <= next_graphics_mode;
    state <= next;
  end
  
  //   The hold screen mode is a special case - the state machine can change
  // it by the next_hold_screen, or the keyboard can toggle it by way of
  // the toggle_hold_screen input.
  always @(posedge clock) begin
    if (toggle_hold_screen)
      hold_screen <= ~hold_screen;
    else
      hold_screen <= next_hold_screen;
  end


  //   Ok, you knew it was coming and here it is - the giant block of logic
  // that is the control logic for the VT52 state machine.  Note that this
  // always block is entirely combinatorial - THERE ARE NO FLIP FLOPS CREATED
  // HERE! 
  always @* begin

    //   Set the "default" value for all state variables and all combinatorial
    // outputs.  That way each individual state only needs to modify those that
    // are actually used in that state...
    next_fb_col = fb_col;  next_curs_col = curs_col;  next_fb_row = fb_row;
    next_curs_row = curs_row;  next_top_row = top_row;
    next_graphics_mode = graphics_mode;  next_hold_screen = hold_screen;
    /*fb_read=0;*/  fb_write = 0;  fb_dout = 0;  rx_clear = 0;  bell = 0;
    send_identify = 0;  set_alt_keypad_mode = 0;  reset_alt_keypad_mode = 0;
    //  Scroll count is bit of a hack - if hold screen mode is off then the
    // default next value is always zero.  That gets scroll count cleared
    // "for free" when and if hold screen mode gets turned off and it keeps
    // us from ever freezing the screen if hold screen isn't enabled.
    next_hold_count = hold_screen ? hold_count : 1'b0;
    //   We set the default next state to be DONE.  That means that unless a
    // state explicitly sets a different next state, by default the state
    // machine will dump the current received character and start looking for
    // the next one at the end of any give state.  It's a bit non-traditional,
    // but it saves a lot of typing in many of the state definitions ...
    next = DONE;

    // Decode the current state and any inputs and figure out what to do...
    case (state)

      // INITIALIZATION ...
      // Initialize all modes, home the cursor and clear the screen.
      INIT: begin
        next_graphics_mode = 0;  next_fb_col = 0;  next_fb_row = 0;
        next_curs_col = 0;  next_curs_row = 0;  next_top_row = 0;
	// Dump any byte in the input buffer, just in case...
	rx_clear = 1;
        next = ERASE_EOS;
      end // case: INIT


      // WAIT FOR AND PROCESS INCOMING CHARACTERS
      //   The DONE state clears the rx_ready flag and then goes to IDLE to
      // wait for the next incoming character.  You might be tempted to have
      // IDLE just clear rx_ready as soon as it's seen the next byte, but that's
      // not safe - once rx_ready is cleared the byte in rx_data could be
      // overwritten any time, and some of the subsequent states need that.
      DONE: begin  rx_clear = 1;  next = IDLE;  end

      // Wait for a character to be received and decode it ...
      IDLE: begin
        if (rx_ready) begin
               if (rx_data < 32)    DecodeControl();
          else if (rx_data != 127)  next = NORMAL0;
          else /* ignore DELETEs */ next = DONE;
        end else
          next = IDLE;
      end // case: IDLE


      // DECODE ESCAPE SEQUENCES
      //   The ESCAPE0 state simply clears the rx_ready flag, and then the
      // ESCAPE1 state waits for the second byte in the escape sequence.
      // When we get it, we decode it to determine the next state...
      ESCAPE0: begin  rx_clear = 1;  next = ESCAPE1;  end
      ESCAPE1: begin
        if (rx_ready)
	  DecodeEscape();
        else
	  next = ESCAPE1;
      end // case: ESCAPE1


      // WRITE PRINTING CHARACTERS TO THE SCREEN
      //   Write a "normal" character to the screen at the current cursor
      // location and move the cursor right.  If the cursor is already one the
      // right edge of the screen then it will either stay there (the usual
      // mode) or we'll simulate a carriage return/line feed.
      //
      //   Like the VT52, we have both a normal mode and an alternate graphics
      // character set mode.  In graphics character set (aka GCS) mode, if we
      // receive an ASCII character from 0x60..0x7E (i.e. a lower case char-
      // acter) then it's replaced with the corresponding character from the
      // GCS instead.  In our particular case, the GCS is stored in the char-
      // acter generator ROM locations corresponding to codes 0x00..0x1F, and
      // we have to convert the 0x60..0x7E code into that range.  Upper case
      // ASCII characters in the range 0x20..0x5F are not affected by the
      // character set mode.
      //
      //   Unfortunately this has to be done it two states - NORMAL0 transfers
      // the current cursor address to the fb_row/col registers, and NORMAL1
      // actually writes the data to the screen,
      NORMAL0: begin
        next_fb_row = curs_row;  next_fb_col = curs_col;  next = NORMAL1;
      end
      NORMAL1: begin
	if (graphics_mode & (rx_data[6:5]==2'b11))
	  fb_dout = rx_data & 7'h1F;
        else
	  fb_dout = rx_data;
        fb_write = 1;
	if (MARGIN_BELL & (curs_col==`MARGIN_COL)) bell = 1;
        if (curs_col != `MAX_COL)
          next_curs_col = curs_col + 1'b1;
        else if (AUTO_NEWLINE) begin
          next_curs_col = 0;  next = LINEFD;
        end
      end // case: NORMAL


      // BASIC CURSOR MOTIONS
      // Cursor left, right, up and down all do what you expect ...
      CURS_LEFT:  if (curs_col != 0       ) next_curs_col = curs_col - 1'b1;
      CURS_RIGHT: if (curs_col != `MAX_COL) next_curs_col = curs_col + 1'b1;
      CURS_UP:    if (curs_row != 0       ) next_curs_row = curs_row - 1'b1;
      CURS_DOWN:  if (curs_row != `MAX_ROW) next_curs_row = curs_row + 1'b1;

      // Carriage return moves the cursor back to the start of the current line.
      CURS_RET:   next_curs_col = 0;

      // Cursor home moves the cursor to the top left corner ...
      CURS_HOME: begin  next_curs_col = 0;  next_curs_row = 0;  end

      //   A tab moves the cursor to the next tab stop. Tabs stops are located
      // at columns 9, 17, 25, 33, etc (i.e. column 8i+1, 0<=i<=9).  After
      // column 73, a tab will only advance the cursor to the next character,
      // and once the cursor reaches the right margin a tab will not advance
      // it any further.
      CURS_TAB: begin
        if (curs_col < (`MAX_COL-8))
          next_curs_col = (curs_col & 7'b1111000) + 7'd8;
        else if (curs_col != `MAX_COL)
          next_curs_col = curs_col + 1'b1;
      end // case: CURS_TAB


      // SCREEN ERASE FUNCTIONS
      //   Erase from the cursor to the end of the line, starting with the
      // character under the cursor now.  The cursor does not actually move.
      ERASE_CURS_EOL: begin
        next_fb_row = curs_row;  next_fb_col = curs_col;  next = ERASE_EOL;
      end // case: ERASE_CURS_EOL

      //   Erase from the location in (fb_col, fb_row) to the end of the line.
      // We loop in this state until all characters have been erased ...
      ERASE_EOL: begin
        fb_dout = " ";  fb_write = 1;
        if (fb_col != `MAX_COL) begin
          next_fb_col = fb_col + 1'b1;  next = ERASE_EOL;
        end
      end // case: ERASE_EOL

      //   Erase from the cursor to the end of the screen, starting with what-
      // ever's under the cursor right now.  The cursor does not actually move.
      ERASE_CURS_EOS: begin
        next_fb_row = curs_row;  next_fb_col = curs_col;  next = ERASE_EOS;
      end // case: ERASE_CURS_EOS

      // Erase from the location in (fb_col, fb_row) to the end of the screen.
      ERASE_EOS: begin
        fb_dout = " ";  fb_write = 1;
        if (fb_col != `MAX_COL) begin
          next_fb_col = fb_col + 1'b1;  next = ERASE_EOS;
        end else if (fb_row != `MAX_ROW) begin
          next_fb_col = 0;  next_fb_row = fb_row + 1'b1;  next = ERASE_EOS;
        end
      end // case: ERASE_EOS


      // SCREEN SCROLLING ROUTINES
      //   Scroll the screen down one line.  The new top line on the screen
      // (which used to be the bottom line) is then cleared.  Note that this
      // does not change the cursor location.
      SCROLL_DOWN: begin
        next_top_row = (top_row != 0) ? top_row - 1'b1 : `MAX_ROW;
        next_fb_row = 0;  next_fb_col = 0;  next = ERASE_EOL;
      end // case: SCROLL_DOWN

      //   Scroll the screen up up one line. The new bottom line on the screen
      // (which used to be the top line) is then cleared.  Note that this does
      // not change the cursor location.
      SCROLL_UP: begin
        next_top_row = (top_row != `MAX_ROW) ? top_row + 1'b1 : 1'b0;
        next_fb_row = `MAX_ROW;  next_fb_col = 0;  next = ERASE_EOL;
      end // case: SCROLL_UP


      // ADVANCED CURSOR MOTIONS
      //   Note - currently, hold screen mode only affects scroll up (i.e. the
      // case invoked by line feed).  It's not clear what, if any, interaction
      // scroll down (the reverse line feed case) should have with hold screen
      // mode.  You can imagine all kinds of complicated interactions, like
      // counting up for scroll up and down for scroll up. The VT52 manual says
      // explicitly that hold screen only applies to line feeds, and we're
      // going with that interpretation.
      //
      //  Line feed moves the cursor down one line unless the cursor is already
      // on the bottom of the screen, in which case the screen is scrolled up
      // one line and the cursor remains in the same location (on the screen).
      LINEFD: begin
        next_hold_count = hold_count + 1'b1;
	if (hold_screen & (next_hold_count>`MAX_ROW))
	  next = HOLD_SCREEN;
        else if (curs_row != `MAX_ROW)
          next_curs_row = curs_row + 1'b1;
	else
          next = SCROLL_UP;
      end // case: LINEFD

      //  Reverse line feed is the opposite case - the cursor moves up one line
      // unless it's already at the top, in which case the screen scrolls down.
      RLINEFD: begin
        if (curs_row != 0)
          next_curs_row = curs_row - 1'b1;
        else
          next = SCROLL_DOWN;
      end // case: RLINEFD

      // Home the cursor and then erase to the end of the screen.  
      CLEAR_SCREEN: begin
        next_curs_col = 0;  next_curs_row = 0;  next_top_row = 0;
        next_fb_col = 0;  next_fb_row = 0;
        next = ERASE_EOS;
      end // case: CLEAR_SCREEN


      // HOLD SCREEN STATE
      //   If hold screen mode is enabled and we've done too many line feeds,
      // then the screen freezes to prevent scrolling.  While that's true, we
      // stay stuck in this state.  No input is processed and rx_clear is never
      // asserted.  Of course this means that the host FIFO will start filling
      // up, but it'll send an XOFF to the host when things get tight.
      //
      //  The only ways out of this state are a) if the user presses Scroll or
      // shift/Scroll to proceed one line or one page (this is just like a real
      // VT52), or b) if the user presses control/Scroll to turn off hold screen
      // mode completely, or c) if the host ignores our XOFF and keeps sending.
      //
      //   In the latter case if the host FIFO fills up, we'll pretend that the
      // user pressed the Scroll key and proceed by one line.  This prevents
      // characters from being lost (and again, a real VT52 would do the same
      // thing) and we'll freeze again at the next line feed.  This will cause
      // the FIFO to fill again another XOF to be sent.  If the host keeps
      // ignoring the XOFFs, we'll just keep scrolling.  In this case the effect
      // is pretty much the same as if hold screen mode were not enabled.
      HOLD_SCREEN: begin
	if (!hold_screen) begin
	  //   Hold screen mode was turned off (by the keyboard, of course)
	  // while we were waiting.  Everything goes back to normal...
	  next_hold_count = 0;  next = SCROLL_UP;
	end else if (proceed_one_line) begin
	  // The user pressed the Scroll Lock (Scroll on a VT52) key ...
	  next_hold_count = `MAX_ROW;  next = SCROLL_UP;
	end else if (proceed_one_screen) begin
	  // The user pressed Shift/Scroll Lock ...
	  next_hold_count = 0;  next = SCROLL_UP;
	end else if (host_silo_full) begin
	  // Host silo full - proceed one line ..
	  next_hold_count = hold_count - 1'b1;  next = SCROLL_UP;
	end else
	  // We're still stuck here...
	  next = HOLD_SCREEN;
      end // case: HOLD_SCREEN


      // <ESC>Y - DIRECT CURSOR ADDRESSING
      //   The <ESC>Y sequence is followed by two data bytes which indicate the
      // line and column that the cursor is to move to. Both values are biased
      // by 32 so that they are appear as printing ASCII characters.  The line
      // number byte should be in the range 32 to 55 (decimal), and the column
      // number byte from 32 to 112 (decimal).  If either byte is out of range,
      // then the cursor moves to that margin.
      //
      //   For example:
      //     <ESC>Y<SP><SP>	- move to (0,0) (i.e. home)
      //     <ESC>Y7o		- move to (79,23) (i.e. lower right)
      //
      // Handle the Y part ...
      DIRECT0: begin  rx_clear = 1;  next = DIRECT1;  end
      DIRECT1: begin
        if (rx_ready) begin
          if ((rx_data >= 32) & (rx_data <= (32+`MAX_ROW)))
            next_curs_row = rx_data[4:0];
          else
            next_curs_row = `MAX_ROW;
          rx_clear = 1;  next = DIRECT2;
        end else
          next = DIRECT1;
      end // case: DIRECT1

      // Handle the X part ...
      DIRECT2: begin
        if (rx_ready) begin
          if ((rx_data >= 32) & (rx_data <= (32+`MAX_COL)))
            next_curs_col = rx_data - 7'd32;
          else
            next_curs_col = `MAX_COL;
        end else
          next = DIRECT2;
      end // case: DIRECT2

      //   Ring the bell (or sound the beeper, or whatever you have).  Note that
      // the bell output is asserted for just one clock - this triggers the bell
      // state machine, which takes care of itself after that.
      BELL: bell = 1;

      // This should never happen, but just in case...
      default: next = INIT;
    endcase // case (state)
  end // always @ *
endmodule // vt52_controller


module vt52_silo
  //++
  //   This module implements FIFO buffers for both the receiver and transmitter
  // side of the VT52.  The transmitter FIFO is really only needed because in
  // the case of escape sequences, the PS/2 keyboard module will output several
  // characters in succession, one per clock tick.  That's way faster than the
  // UART can send them, and we must buffer them or they'll be lost.
  //
  //   The receiver side needs a FIFO on account of hold screen mode - when
  // the screen is frozen, we send an XOFF to the host to tell it to stop
  // sending.  That may take a moment, however, and we need somewhere to stick
  // the characters that we can't put on the screen yet.  When the screen is
  // released for more scrolling, we empty the FIFO, send an XON, and the whole
  // process starts again.
  //
  //   It's worth noting that if it weren't for hold screen mode, there'd be
  // no need for a receive FIFO at all.  It takes, what, a couple of clock ticks
  // for the VT52 state machine up above to handle a character?  At 28MHz, we
  // could keep up with millions of characters per second - that's way faster
  // than any UART or human could ever expect to go.  It's only because of hold
  // screen, when the screen is frozen and we have to stop prcessing received
  // characters, that we need a buffer.
  //
  //   Because of that the XOF/XOFF logic doesn't depend on how many characters
  // are in the buffer.  Instead, whenever the screen freezes we send an XOFF
  // immediately (we may have already sent one, but it does no harm to send
  // another). When the screen unfreezes we first try to use what's in the FIFO
  // and then, only when the FIFO is empty and the screen is unfrozen, do
  // we send an XON.
  //
  //   If the host doesn't respond to the XOFF and keeps sending, then a real
  // VT52 would ignore hold screen and keep scrolling to it could keep up.
  // We do the same when our receive FIFO fills up.
#(
  parameter LOG2_TX_SIZE =  3,	// create an 8 byte FIFO for transmitting
  parameter LOG2_RX_SIZE =  6,	// and a 64 byte FIFO for receiving
  parameter SEND_XOFF_AT = 48,	// send XOFF when FIFO gets 3/4ths full
  parameter SEND_XON_AT  = 8	// and send XON when it gets back down to 8
) (
  // Global stuff ...
  input clock,			// clock (usually the pixel clock)
  // UART interface ...
  input uart_dr,		// data received flag
  input [7:0] uart_rbr,		// receiver buffer register
  output uart_drr,		// reset data received flag
  input uart_tbre,		// transmitter buffer register empty
  output reg uart_tbrl,		// load transmitter buffer
  output reg [7:0] uart_tbr,	// transmitter buffer register
  // PS/2 keyboard interface ...
  //   Note that there is no "reset kbd_ready" signal - the keyboard outputs
  // data for ONE CLOCK TICK ONLY.  We'd better be ready!
  input  kbd_ready,		// keyboard data ready
  input  [7:0] kbd_data,	// keyboard data
  // VT52 display interface ...
  output host_ready,		// received byte ready for display
  output [`CHAR_BITS] host_data,// data for display
  input  host_read,		// clear host_ready
  output host_buffer_full	// our receiver FIFO is full
);
  // Locals ...
  reg send_xon, send_xoff, xon_sent, xoff_sent, tx_fifo_read;
  wire [7:0] tx_fifo_out;  wire [LOG2_RX_SIZE:0] rx_count;
  wire tx_fifo_empty, rx_fifo_empty, rx_data_valid;

  // Create the keyboard -> host buffer ...
  fifo #(.LOG2_DEPTH(LOG2_TX_SIZE)) tx_fifo (
    .clock(clock), .reset(1'b0), .din(kbd_data), .write(kbd_ready), .full(),
    .dout(tx_fifo_out), .read(tx_fifo_read), .empty(tx_fifo_empty), .count()
  );

  //   This always block manages the transmitter side.  The logic goes something
  // like this - if the UART is busy now, wait for it to finish.  If we need to
  // send XON or XOFF, send that next.  Otherwise if there's anything in the
  // keyboard FIFO then send that.  You could make a state machine for this but
  // it isn't really necessary - you can do everything in one state!
  always @* begin
    uart_tbr=8'hxx; uart_tbrl=0; tx_fifo_read=0; xon_sent=0; xoff_sent=0;
    if (uart_tbre) begin
      if (send_xoff) begin
	uart_tbr = `XOFF;  uart_tbrl = 1;  xoff_sent = 1;
      end else if (send_xon) begin
	uart_tbr = `XON;   uart_tbrl = 1;  xon_sent = 1;
      end else if (!tx_fifo_empty) begin
	uart_tbr = tx_fifo_out;  uart_tbrl = 1;  tx_fifo_read = 1;
      end
    end // if (uart_tbre)
  end // always @ *

  //   Now create the host -> display buffer.  There are several things of
  // interest to note about this - first, the rx FIFO is only seven bits wide,
  // not eight.  That's because the whole receive path, including the frame
  // buffer, is also only seven bits wide.  The MSB received from the UART is
  // just discarded.  Also, we don't even store either of characters 0x00 or
  // 0x7F in the FIFO - they're dropped as soon as they're recived.  A real
  // VT52 does just the same.  Lastly, notice that we can just wire the UART
  // drr (data ready reset) back to dr (data ready) - that's because the FIFO
  // is always ready to accept data and we can clear the dr flag in the same
  // clock cycle that writes the FIFO.
  assign rx_data_valid = (uart_rbr[6:0] != 7'h00) & (uart_rbr[6:0] != 7'h7F);
  assign uart_drr = uart_dr;
  fifo #(.LOG2_DEPTH(LOG2_RX_SIZE), .DATA_WIDTH(7)) rx_fifo (
    .clock(clock), .reset(1'b0),
    .din(uart_rbr[6:0]), .write(uart_dr & rx_data_valid),
    .dout(host_data), .read(host_read),
    .empty(rx_fifo_empty), .full(host_buffer_full), .count(rx_count)
  );
  assign host_ready = ~rx_fifo_empty;

  //   And finally here's our little state machine to run the receiver flow
  // control.  It's really simple - when the buffer is empty, wait for it
  // to fill up.  When it fills up, send XOFF then wait for it to empty out.
  // When it empties, send XOFF and go back to waiting for it to fill up again.
  // A perfect textbook example for a state machine.
  localparam [1:0]
    WAIT_FULL	= 0,		// wait for the buffer to fill up
    SEND_XOFF	= 1,		// send an XOFF character
    WAIT_EMPTY	= 2,		// wait for the buffer to empty out
    SEND_XON	= 3;		// and send an XON character
  reg [1:0] flow_state, next_state;

  // Define the initial state ...
  initial
    flow_state = WAIT_FULL;
  
  // Synthesize the state register ...
  always @(posedge clock)
    flow_state <= next_state;

  // And here's the next state logic ...
  always @* begin
    next_state = flow_state;  send_xon = 0;  send_xoff = 0;
    case (flow_state)
      // Monitor buffer level, watch for it to get too full ...
      WAIT_FULL: begin
	if (rx_count >= SEND_XOFF_AT) next_state = SEND_XOFF;
      end
      // Send XOFF, wait for it to be transmitted...
      SEND_XOFF: begin
	send_xoff = 1;
	if (xoff_sent) next_state = WAIT_EMPTY;
      end
      // Wait for buffer level to drop below lower threshold ....
      WAIT_EMPTY: begin
	if (rx_count <= SEND_XON_AT) next_state = SEND_XON;
      end
      // Sned XON, wait for it to be transmitted ...
      SEND_XON: begin
	send_xon = 1;
	if (xon_sent) next_state = WAIT_FULL;
      end
    endcase // case (state)
  end // always @ *
endmodule // vt52_silo


module vt52
  //++
  //   Connect the keyboard, display, frame buffer, VT52 controller, and a
  // UART, together to make a complete terminal...
  //--
(
  // System wide signals ...
  input clock,				// pixel clock (usually 28.322MHz)
  // Serial port interface ...
  input  rxd,				// RS232 formatted serial data input
  output txd,				//   "    "     "    "      "  output
  // Keyboard interface ...
  inout ps2_clock,			// PS/2 keyboard clock (bidirectional)
  inout ps2_data,  			//   "    "   "  data     "      "
  // Monitor interface ...
  output hsync,				// horizontal sync to VGA monitor
  output vsync,				// vertical     "   "  "   "   "
  output [`RGB] video,			// RGB video        "  "   "   "
  // Miscellaneous ...
  output bell				// asserted to trigger bell
);
  parameter CLOCK_FREQUENCY    = 28_322_000;// actual clock frequency (Hz
  parameter BAUD_RATE          = 19200;	    // desired baud rate
  parameter [`RGB] FOREGROUND  = `RGB_GREEN;// RGB foreground color
  parameter [`RGB] BACKGROUND  = `RGB_BLACK;//  "  background   "
  parameter CURS_BLINK         = 1'b0;	    // 1 -> blinking cursor
  parameter CURS_UNDERLINE     = 1'b0;	    // 1 -> underline cursor
  parameter AUTO_NEWLINE       = 1'b0; 	    // 1 -> auto newline mode 
  parameter MARGIN_BELL        = 1'b0; 	    // 1 -> sound bell at margin
  parameter SWAP_CAPS_CTRL     = 1'b0;	    // 1 -> swap caps lock and ctrl
  parameter INIT_KEYPAD_MODE   = 1'b0;	    // 1 -> start in alt keypad mode
  parameter RESET_KEYBOARD     = 1'b1;	    // 1 -> reset kbd when we reset

  // Locals ...
  wire uart_dr, uart_drr, baud_tick, uart_tbrl, uart_tbre, kbd_ready;
  wire [7:0] uart_tbr, uart_rbr, kbd_data;
  wire [`COL_BITS] disp_col, vt_col, curs_col;
  wire [`ROW_BITS] disp_row, vt_row, curs_row, top_row;
  wire [`CHAR_BITS] host_data, disp_char, /*vt_din,*/ vt_dout;
  wire host_ready, host_read, host_silo_full, char_tick, /*vt_read,*/ vt_write;
  wire set_alt_keypad_mode, reset_alt_keypad_mode, send_identify;
  wire hold_screen_led, toggle_hold_screen, proceed_one_screen, proceed_one_line;

  // Create the frame buffer ...
  vt52_frame_buffer frame_buffer (
    .clock(clock), .char_tick(char_tick), .top_row(top_row),
    .disp_col(disp_col), .disp_row(disp_row), .disp_char(disp_char),
    .vt_col(vt_col), .vt_row(vt_row),
//  .vt_read(vt_read), .vt_dout(vt_din),
    .vt_write(vt_write), .vt_din(vt_dout)
  );

  //A VGA display  ...
  vga_display #(
    .FOREGROUND(FOREGROUND), .BACKGROUND(BACKGROUND),
    .CURS_BLINK(CURS_BLINK), .CURS_UNDERLINE(CURS_UNDERLINE)
  ) display (
    .pixelclk(clock), .char_tick(char_tick),
    .cur_col(disp_col), .cur_row(disp_row), .cur_char(disp_char),
    .curs_row(curs_row), .curs_col(curs_col), .frame_tick(),
    .hsync(hsync), .vsync(vsync), .video(video)
  );

  // The VT52 state machine ...
  vt52_controller #(
    .AUTO_NEWLINE(AUTO_NEWLINE), .MARGIN_BELL(MARGIN_BELL)
  ) controller (
    .clock(clock),
    .fb_col(vt_col), .fb_row(vt_row),
//  .fb_read(vt_read), .fb_din(vt_din),
    .fb_write(vt_write), .fb_dout(vt_dout),
    .curs_col(curs_col), .curs_row(curs_row), .top_row(top_row),
    .rx_data(host_data), .rx_ready(host_ready), .rx_clear(host_read),
    .host_silo_full(host_silo_full), .bell(bell),
    .set_alt_keypad_mode(set_alt_keypad_mode),
    .reset_alt_keypad_mode(reset_alt_keypad_mode),
    .hold_screen(hold_screen_led),
    .proceed_one_line(proceed_one_line),
    .proceed_one_screen(proceed_one_screen),
    .toggle_hold_screen(toggle_hold_screen),
    .send_identify(send_identify)
  );

  // A keyboard ...
  ps2_keyboard #(
    .SWAP_CAPS_CTRL(SWAP_CAPS_CTRL),
    .INIT_KEYPAD_MODE(INIT_KEYPAD_MODE),
    .RESET_KEYBOARD(RESET_KEYBOARD)
  ) keyboard (
    .clock(clock), .reset(1'b0),
    .ps2c(ps2_clock), .ps2d(ps2_data),
    .dout(kbd_data), .ready(kbd_ready), 
    .set_alt_keypad_mode(set_alt_keypad_mode),
    .reset_alt_keypad_mode(reset_alt_keypad_mode),
    .hold_screen_led(hold_screen_led),
    .proceed_one_line(proceed_one_line),
    .proceed_one_screen(proceed_one_screen),
    .toggle_hold_screen(toggle_hold_screen),
    .send_identify(send_identify)
  );
  
  // Buffers for both receiving and transmitting, with flow control...
  vt52_silo silo (
    .clock(clock),
    .uart_dr(uart_dr), .uart_rbr(uart_rbr), .uart_drr(uart_drr),
    .uart_tbre(uart_tbre), .uart_tbrl(uart_tbrl), .uart_tbr(uart_tbr),
    .kbd_ready(kbd_ready), .kbd_data(kbd_data),
    .host_ready(host_ready), .host_data(host_data), .host_read(host_read),
    .host_buffer_full(host_silo_full)
  );

  // A baud rate generator and a UART ...
  ck_divider #(.IN_FREQ(CLOCK_FREQUENCY), .OUT_FREQ(16*BAUD_RATE))
    brg (.clock(clock), .out(baud_tick));
  uart uart (
    .clock(clock), .reset(1'b0), .rrc(baud_tick), .trc(baud_tick),
    .rri(rxd), .dr(uart_dr), .drr(uart_drr), .rbr(uart_rbr), .fe(), .oe(),
    .tro(txd), .tre(), .tbre(uart_tbre), .tbrl(uart_tbrl), .tbr(uart_tbr)
  );
endmodule // vt52
