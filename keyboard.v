//++
//keyboard.v - PS/2 keyboard to VT52 parallel ASCII
//Copyright (C) 2011-2014 by Spare Time Gizmos.  All rights reserved.
//
// DESCRIPTION:
//   This file implements an interface to a standard PS/2 keyboard, and a PS/2
// scan code to ASCII conversion that's pretty much compatible with the VT52.
// The PS/2 to ASCII conversion is a state machine that translates the scan
// codes into ASCII characters.  That alone is not so difficult, but the
// tricky part is emulating the escape sequences sent by the VT52's keypad,
// which we attempt to do using the numeric keypad and the arrow keys on the
// PC's keyboard.  This may seem like a waste of time, but it's necessary so
// that PDP-8 and PDP-11 full screen editors like EDT and KEDs will run.
//
//   Here are a few highlights of the VT52 keyboard emulation -
//
//   * All printing characters send their corresponding ASCII codes, as do TAB
// (0x09), ENTER (0x0D), BACKSPACE (0x08), and ESC (0x1B).  The BACKSPACE key
// currently sends backspace (^H), not delete!
//
//   * The SHIFT (both left and right), CTRL (LEFT control only!) and CAPS LOCK
// keys work as you would expect.  If the SWAP_CAPS_AND_CTRL option is enabled,
// then these two functions are swapped.  Note that CAPS LOCK is a "CAPS LOCK",
// and not a "SHIFT LOCK"  in that it affects only alphabetic characters.  The
// right CTRL (if your keyboard has one) and ALT keys (both left and right) do
// nothing.
//
//   * If the "Keypad Application Mode" option is selected, then the numeric
// keypad keys "0" thru "9", "." and ENTER, send escape sequences that map
// approximately to the VT52 keypad.  If the application mode option is not
// enabled, then these keys send the corresponding normal ASCII characters.
//
//   * The keypad "/", "*" and "-" keys map to the VT52 PF1..PF3 (better known
// as the BLUE, RED and GRAY keys) and always send the corresponding VT52 escape
// sequences regardless of the keypad application mode setting.  The four arrow
// keys also always send the corresponding VT52 escape sequences, regardless of
// the keypad application mode.
//
//   * This table summarizes the VT52 escape sequences used by the keypad:
//
//	Key	      Scan Code	      Normal	Application
//      -----------   ---------       ------    ------------------------------
//	UP ARROW	E0 75	      <ESC>A	<ESC>A
//	DOWN ARROW	E0 72	      <ESC>B	<ESC>B
//	RIGHT ARROW	E0 74	      <ESC>C	<ESC>C
//	LEFT ARROW	E0 6B	      <ESC>D	<ESC>D
//	Keypad 0	70		'0'	<ESC>?p
//	Keypad 1	69		'1'	<ESC>?q
//	Keypad 2	72		'2'	<ESC>?r
//	Keypad 3	7A		'3'	<ESC>?s
//	Keypad 4	6B		'4'	<ESC>?t
//	Keypad 5	73		'5'	<ESC>?u
//	Keypad 6	74		'6'	<ESC>?v
//	Keypad 7	6C		'7'	<ESC>?w
//	Keypad 8	75		'8'	<ESC>?x
//	Keypad 9	7D		'9'	<ESC>?y
//	Keypad .	71		'.'	<ESC>?z
//	Keypad +	79		'+'	'+'
//	Keypad /	E0 4A	      <ESC>P	<ESC>P	(VT52 F1 BLUE key)
//	Keypad *	7C	      <ESC>Q	<ESC>Q	(VT52 F2 RED key)
//	Keypad -	7B	      <ESC>R	<ESC>R	(VT52 F3 GRAY key)
//	Keypad ENTER	E0 5A		0D	<ESC>?M
//
//   * All the remaining keys, with the exception of NUM LOCK and SCROLL LOCK, 
// do nothing.  This includes the function keys (F1 thru F12), the Windows keys
// (left, right and menu), the editing keypad (INSERT, DELETE, HOME, PAGE UP,
// PAGE DOWN, and END), PAUSE/BREAK and PRINT SCREEN.
//
//   * The NUM LOCK key locally toggles the application keypad mode.  Although
// application keypad mode is a standard VT52 feature, real VT52s did not have
// a key to locally toggle this mode.
//
//   * The SCROLL LOCK key is equivalent to the SCROLL key on a real VT52, with
// one enhancement.  In hold screen mode, pressing SCROLL LOCK will allow the
// screen to scroll one line, and Shift/SCROLL LOCK will scroll one page. In
// addition, Control/SCROLL LOCK will locally toggle the hold screen mode.
// A real VT52 did not have a way to locally select hold screen mode.
//
//   * The keyboard LEDs are implemented and have the following meaning -
//
//	CAPS LOCK LED  -> lights when CAPS LOCK mode is on
//	NUM LOCK LED   -> lights when alternate keypad mode is selected
//	SCROL LOCK LED -> lights when hold screen mode is enabled
//
// TARGET DEVICES
// TOOL VERSIONS
//
// REVISION HISTORY:
// 28-Jan-11  RLA  New file.
// 29-Dec-13  RLA  Fix up a few constants for ISE 14.7
//		   Remove some duplicated code in ps2_tx()
//  1-Jan-14  RLA  Remove global reset net.  Add initial blocks instead.
//--
//000000011111111112222222222333333333344444444445555555555666666666677777777778
//345678901234567890123456789012345678901234567890123456789012345678901234567890
`include "vt52_h.v"


module ps2_cleanup
  //++
  //   The raw PS/2 clock and data signals have a couple of problems - first of
  // all, they're not synchronized with our clock domain which can lead to all
  // the classic metastabiity problems.  And worse, they can be fairly noisy.
  // The latter is especially a problem for the PS/2 clock since we're looking
  // for falling edges to clock our receiver, and noise can easily be recognized
  // as a spurious edge.
  //
  //   This module does some simple filtering on these signals to improve, but
  // not completely fix, the problem.  The PS/2 clock signal is run thru an
  // eight bit shift register and the filtered PS/2 clock output only changes
  // state after eight consistent samples.  This effectively removes any noise
  // that's less than eight system clocks wide.  The PS/2 data is double
  // buffered to eliminate metastability issues only - it's not otherwise
  // filtered.
  //
  //   It also detects negative (falling) edges on the PS/2 clock - this is
  // all the transmitter and receiver are really interested in, and we get
  // this "for free" with all the other flitering we're doing anyway.
  //--
(
  input clock,			// system clock for filtering
  input ps2c_in,		// raw PS/2 keyboard clock
  input ps2d_in,		//  "    "    "   "  data
  output reg ps2c_out,		// filtered, synchronized clock
  output reg ps2d_out,		// synchronized data
  output ps2c_edge		// negative clock edge detected
);
  reg [7:0] filter;  reg data_buf;
  wire [7:0] next_filter;  wire next_ps2c;

  initial begin
    ps2c_out = 0;  filter = 0;
    ps2d_out = 0;  data_buf = 0;
  end
  
  always @(posedge clock) begin
    filter <= next_filter;  ps2c_out <= next_ps2c;
  end

  assign next_filter = {ps2c_in, filter[7:1]};
  assign next_ps2c  = (filter == 8'hff) ? 1'b1 :
		      (filter == 8'h00) ? 1'b0 :
		       ps2c_out;
  assign ps2c_edge = ps2c_out & ~next_ps2c;

  always @(posedge clock) begin
    ps2d_out <= data_buf;  data_buf <= ps2d_in;
  end
endmodule // ps2_cleanup


module ps2_rx
  //++
  //   This is a fairly simple state machine that looks for negative edges on
  // the PS2 keyboard clock and shifts a bit each time we find one.  Parity and
  // framing errors are detected and will assert the rx_error flag when found.
  // A simple time out on the PS2 clock is implemented by counting system
  // clocks; if too many ticks pass before the next clock pulse, then rx_error
  // is asserted and we return to the idle state.
  //
  //   Note that the output of this module has no buffering - the rx_done flag
  // will be asserted for one clock tick only, and who ever is using this
  // module must capture the byte from rx_data just then.  No second chances!
  //--
(
  input clock,			// master system clock
  input reset,			// reset the receiver state machine
  input ps2d,			// cleaned up serial data from keyboard
  input ps2c_edge,		// keyboard clock edge detected
  input enable,			// inhibit this receiver when not set
  output reg done,		// asserted for 1 clock when data is valid
  output reg error,		// parity, framing or timeout error
  output [7:0] data		// received data
);
  // Parameters ...
  //   The timeout is actually a timeout on the entire transmission, from start
  // bit to stop, and not just a single clock.  According to the PS/2 "standard"
  // the lower bound on the keyboard clock is around 100us, so the worst case
  // time for one byte is a little over a millisecond.  Assuming a clock
  // frequency of 28.322MHz (the VGA/MDA pixel clock) that's around 28000 ticks
  // to time out.  Sixteen bits (a timeout of 65536 clocks) should be fine.
  parameter LOG2_TIMEOUT = 16;	// about 2.3ms with a 28.322MHz clock
  parameter MAX_TIMEOUT = 2**LOG2_TIMEOUT - 1'b1;

  // State definitions (really, this couldn't be simpler!) ...
  localparam [1:0]
    IDLE	= 0,		// waiting for start bit
    DATA	= 1, 		// reading data bits
    DONE	= 2,		// check for errors and load data
    TIMEOUT	= 3;		// gave up waiting for a clock
  reg [1:0] state, next;

  // Locals ...
  reg [3:0] bits, next_bits;	// count of the bits received
  reg [10:0] raw, next_raw;	// raw data (start, 8 data bits, parity, stop)
  reg [LOG2_TIMEOUT-1:0] tcount;// count down timer for clock timeout

  // Initial state ...
  initial begin
    state = IDLE;  bits = 0;  raw = 0;  tcount = MAX_TIMEOUT;
  end

  // State flip flops ...
  always @(posedge clock)
    if (reset) begin
      state <= IDLE;  bits <= 0;  raw <= 0;
    end else begin
      bits <= next_bits;  raw <= next_raw;  state <= next;
    end

  //   The timeout counter cheats a bit - it's held in reset as long as we're
  // in the idle state, and it counts down in all other states.  When it gets
  // zero, it stops counting until we go back to idle...
  always @(posedge clock)
    if (reset)
      tcount <= MAX_TIMEOUT;
    else if (state == IDLE)
      tcount <= MAX_TIMEOUT;
    else if (tcount != 0)
      tcount <= tcount - 1'b1;

  // Next state logic ...
  always @* begin
    next_bits = bits;  next_raw = raw;  next = state;  done = 0;  error = 0;  

    case (state)
      // Watch for a clock edge and start the receiver ...
      IDLE:
        if (ps2c_edge & enable) begin
          // Shift in the start bit and read ten more after that.
	  next_raw = {ps2d, 10'b0};  next_bits = 4'd9;  next = DATA;
        end

      // Read the rest of the bits and shift them into raw ...
      DATA:
        if (ps2c_edge) begin
          next_raw = {ps2d, raw[10:1]};
          if (bits == 0)
            next = DONE;
          else
            next_bits = bits - 1'b1;
	end else if (tcount == 0)
	  next = TIMEOUT;

      //   Check the start and stop bits for proper framing and the parity bit
      // for parity errors, and set either done or error as needed ...
      DONE: begin
        if (!raw[0] & raw[10] & (^raw[9:1]))
	  done = 1;
	else
	  error = 1;
	next = IDLE;
      end // case: DONE

      // Timeout - assert error and go back to idle...
      TIMEOUT: begin
	  error = 1;  next = IDLE;
	end
    endcase // case (state)
   end

   // Just pop the data bits out of the raw data...
   assign data = raw[8:1];
endmodule // ps2_rx


module ps2_tx
  //++
  //   And this module is an equally simple state machine for sending bytes to
  // the keyboard.  Sending is a little harder than receiving because we have
  // first request that the keyboard listen to us (usually it doesn't!) and then
  // wait for the keyboard to generate clocks.  There's a little handshake that
  // goes on between the host and keyboard to turn the line around - first we
  // hold the clock line low for at least 100us, then we let go and wait for
  // the keyboard to generate the first clock, then we hold data low to let it
  // know we're ready to send.  After that the keyboard continues to genereate
  // the clocks and we drive our bits onto the data line.
  //--
(
  input clock,			// master system clock
  input reset,			// reset the transmitter state machine
  inout ps2c,			// raw PS/2 clock line (for output)
  inout ps2d,			//  "    "    "   DATA    "    "
  input ps2c_edge,		// edge detected on PS/2 keyboard clock
  input enable,			// request to transmit a byte
  input [7:0] data,		// DATA to transmit
  output reg done,		// asserted for 1 clock when done sending
  output reg error,		// timeout error while transmitting
  output reg idle		// transmitter is currently IDLE
);
  // Parameters ...
  //   The LOG2_TIMEOUT parameter has basically the same function as in ps2_rx,
  // but since we have to wait for the keyboard to acknowledge us, this time
  // we allow twice as long .
  parameter LOG2_TIMEOUT = 17;	// about 4.5ms with a 28.322MHz clock
  parameter MAX_TIMEOUT = 2**LOG2_TIMEOUT - 1'b1;
  //   The LOG2_DELAY parameter should give at least a 100us delay - we hold
  // the clock line low for at least this long to request to send data.
  parameter LOG2_DELAY = 12;	// gives about 145us at 28.322Mhz
  parameter MAX_DELAY = 2**LOG2_DELAY - 1'b1;

  // State definitions ...
  localparam [2:0]
    IDLE	= 0,		// nothing to do
    RTS		= 1,		// request keyboard to receive DATA from us
    START	= 2,		// send START bit
    DATA	= 3,		// send DATA bits and parity
    STOP	= 4,		// send STOP bit and back to IDLE
    TIMEOUT	= 5;		// timed out waiting for DATA to be sent
  reg [2:0] state, next;

  // Locals ...
  reg [3:0] bits, next_bits;	// count of the bits received
  wire parity;			// parity bit for data
  reg [8:0] datap, next_datap;	// data with parity (9 bits total)
  reg [LOG2_DELAY-1:0] delay,	// PS/2 turnaround delay timer
		  next_delay;	// ...
  reg ps2c_out;			// PS/2 clock output 
  reg ps2d_out;			//  "   data    "
  reg drive_clock;		// enable PS/2 clock driver
  reg drive_data;		//   "     "   data    "
  reg [LOG2_TIMEOUT-1:0] tcount;// count down timer for clock timeout

  // Figure out an odd parity bit ...
  assign parity = ~^data;

  // These tri-state buffers drive the PS/2 data and clock signals ...
  assign ps2c = drive_clock ? ps2c_out : 1'bz;
  assign ps2d  = drive_data  ? ps2d_out  : 1'bz;

  // Initialize the state ...
  initial begin
    state = IDLE;  delay = 0;  bits = 0;  datap = 0;  tcount = MAX_TIMEOUT;
  end
  
  // State flip flops ...
  always @(posedge clock)
    if (reset) begin
      state <= IDLE;  delay <= 0;  bits <= 0;  datap <= 0;
    end else begin
      state <= next;  delay <= next_delay;
      bits <= next_bits;  datap <= next_datap;
    end

  // The timeout counter is just like ps2_rx ...
  always @(posedge clock)
    if (reset)
      tcount <= MAX_TIMEOUT;
    else if (state == IDLE)
      tcount <= MAX_TIMEOUT;
    else if (tcount != 0)
      tcount <= tcount - 1'b1;

  // Next state logic ...
  always @* begin
    next = state;  next_delay = delay;  next_bits = bits;  next_datap = datap;
    ps2c_out = 1;  ps2d_out = 1;  drive_clock = 0;  drive_data = 0;
    done = 0;  idle = 0;  error = 0;

    case (state)
      // Wait here until enable is asserted ...
      IDLE: begin
        idle = 1;
        if (enable)  begin
          next_datap = {parity, data};
	  next_delay = MAX_DELAY;
	  next = RTS;
        end
      end // case: IDLE

      // Hold the PS/2 clock low for at least 100uS ...
      RTS: begin
        ps2c_out = 0;  drive_clock = 1;
        if (delay == 0)
	  next = START;
	else
          next_delay = delay - 1'b1;
      end // case: RTS

      // Send start bit, wait for keyboard to start clocking ...
      START: begin
        ps2d_out = 0;  drive_data = 1;
        if (ps2c_edge) begin
          next_bits = 4'd8;  next = DATA;
        end else if (tcount == 0)
	  next = TIMEOUT;
      end // case: START

      // Send nine bits (eight data plus one parity) ...
      DATA: begin
        ps2d_out = datap[0];  drive_data = 1;
        if (ps2c_edge) begin
          next_datap = {1'b0, datap[8:1]};
          if (bits == 0)
            next = STOP;
          else
            next_bits  = bits - 1'b1;
        end else if (tcount == 0)
	  next = TIMEOUT;
      end // case: DATA

      // Send the stop bit and we're done ...
      // (This assumes that the ps2d line is pulled up high anyway!)
      STOP: begin
        if (ps2c_edge) begin
          done = 1;  next = IDLE;
        end else if (tcount == 0)
	  next = TIMEOUT;
      end // case: STOP

      // Timeout error - assert error and return to idle ...
      TIMEOUT: begin
	error = 1;  next = IDLE;
      end // case: TIMEOUT

    endcase // case (state)
  end // always @ *
endmodule // ps2_tx


module ps2_keymap
  //++
  //   This module implements the PS/2 scan code to ASCII character mapping
  // ROM.  The smallest allocation of block RAM in the Spartan 3 devices is
  // 2K bytes, so there's really no point in attempting to make the ROM any
  // smaller than that.  Also, even though the PS/2 scan code has eight bits,
  // there's no scan code above $7F that we're interested in, so for our
  // purposes a scan code can be considered as only seven bits.  This permits
  // us to fit up to sixteen 128 byte key maps into one ROM.
  //
  //   So the key map ROM will be 2K bytes - that's eleven address bits, the
  // lower seven of which are always the scancode, and the upper four are the
  // selected keymap.  The standard keymaps are -
  //
  //   Keymap	Usage
  //   ------	---------------------------------------
  //	0000	- no modifiers
  //	0001	- CAPS LOCK ON
  //	0010	- Shift key (either one) pressed
  //	0011	- Shift and CAPS LOCK
  //	0100	- CONTROL key pressed
  //	0101	- CONTROL and CAPS LOCK
  //	0110	- CONTROL and Shift
  //	0111	- CONTROL, Shift and CAPS LOCK
  //
  //  The remaining keymaps have special uses.  Keymaps 8 and 9 are used to
  // fill in the final character of escape sequences (arrow keys) and escape
  // "?" sequences (application mode keypad keys)
  //
  //   Keymap	Usage
  //   ------	---------------------------------------
  //	1000	- final character of keyboard <ESC> sequences
  //	1001	- final character of keyboard <ESC>? sequences
  //
  //  The remaining keymaps are currently unused ...
  //
  //   In addition to the mapped character code, this module produces a
  // key_valid output which is asserted for any key that has a valid mapping.
  // Invalid mappings are keys for which either a) the MSB of the scan code
  // is one, or b) the keymap entry is zero.
  //--
(
  input  clock,				// master system clock
  input  [3:0] keymap,			// subtable selection (see above)
  input  [7:0] scancode,		// scan code from the PS/2 keyboard
  input  load,				// update the ascii_key output
  output reg [7:0] ascii,		// mapped ASCII character code
  output key_valid			// this key has a valid mapping
);
  // Define the key mapping ROM and initialize its contents ...
  reg [7:0] keymap_rom [0:2047];
  initial $readmemh("keymap.mem", keymap_rom, 0, 2047);

  //   Scan codes with the MSB set actually return whatever is in byte 0 of
  // the selected keymap - all the maps are set up with zero there.  
  always @(posedge clock) begin
    if (load)
      ascii <= keymap_rom[{keymap, scancode[7] ? 7'b0 : scancode[6:0]}];
  end
  assign key_valid = ascii != 0;
endmodule // ps2_keymap


module ps2_extended_reset
  //++
  //   This module generates a local_reset signal that's just an extended
  // version of the system reset.  A system reset often occurs in conjunction
  // with a power up, and in that situation we need give the keyboard 
  // microprocessor a chance to initialize before we start talking to it.
  // The extended delay holds our ps2_rx module in reset until the keyboard is
  // ready...
  //
  //   Note that the initial condition enables one full delay cycle. This means
  // we'll generate one full delay cycle after FPGA configuration even if out
  // reset input is permanently tied low.
  //--
(
  input clock,				// system wide clock
  input reset,				// system wide reset
  output extended_reset			// our own local, delayed, reset
);
  parameter LOG2_DELAY = 22;
  parameter MAX_DELAY = 2**LOG2_DELAY - 1'b1;
  reg [LOG2_DELAY-1:0] delay;
  initial delay = MAX_DELAY;
  always @(posedge clock)
    if (reset)
      delay <= MAX_DELAY;
    else if (delay != 0)
      delay <= delay - 1'b1;
  assign extended_reset = reset | (delay != 0);
endmodule // ps2_extended_reset


module ps2_controller
  //++
  //   This module implements a state machine that talks to the PS/2 keyboard
  // and converts the scan codes into ASCII.  Sounds easy,  but it can get
  // tricky, especially handling the PS/2 extended codes.  And worse, some of
  // the keys need to send VT52 escape sequences, which means that a single key
  // can translate into two or three ASCII codes.  And as a final complication,
  // the identify input tells use to send the VT52 identify response (<ESC> "/"
  // "K") at our earliest convenience.  The VT52 controller asserts this when
  // it receives the <ESC> "Z" sequence from the host.
  //
  //   This module has one bad habit - it asserts the tx_ready output for only
  // one clock cycle while the data on tx_data is valid.  If there is more
  // than one byte to send (e.g. for an escape sequence) then one byte is
  // sent every clock.  This pretty much requires that some kind of buffer
  // (the fifo module is perfect, BTW) be used at the output to avoid losing
  // data.
  //
  //   It'd be nice if this could be completely stand alone, but this state
  // machine needs to communicate with the VT52 display side state machine 
  // (vt52_controller in vt52.v) for three different things - about the current
  // keypad mode (application vs normal), for hold screen mode and functions,
  // and for the identify command.
  //
  //   The keypad application/normal mode current state FF lives here, and the
  // display side tells us, via the set/reset_alt_keypad_mode signals, when it
  // has received the <ESC> = and <ESC> > sequences from the host.  The hold
  // screen mode FF lives in the display controller and it lets us know, via the
  // hold_screen_led signal, the current state.  We need to know that only so
  // that the keyboard Scroll Lock LED can track it; otherwise the keyboard
  // wouldn't care about the hold screen state.
  //
  //   The keyboard lets the display know, via the toggle_hold_screen, proceed_
  // one_line and proceed_one_screen, signals when the user has pressed the
  // corresponding keys (Scroll Lock, shift/Scroll Lock and control/Scroll
  // Lock).  And finally, the send_identify signal lets us know when the display
  // side has received an inquire, <ESC> Z, sequence.  That tells us that we
  // should send the VT52 identify response, <ESC> / K, at our earliest
  // convenience.
  //--
(
  input clock,			// global system clock
  input reset,	       		// global asynchronous reset
  // PS/2 Keyboard interface ...
  input [7:0] ps2_rx_data,	// PS/2 scan code from the keyboard
  input ps2_rx_done,		// scancode is valid
  input ps2_rx_error,		// any protocol error has occurred
  output reg [7:0] ps2_tx_data,	// data to send to the PS/2 keyboard
  output reg ps2_tx_enable,	// enable transmitting ps2_tx_data
  input ps2_tx_done,		// PS/2 transmitter done
  input ps2_tx_error,		//   "     "    "   error
  input ps2_tx_idle,		//   "     "    "   idle
  output reg ps2_reset_rxtx,	// reset PS/2 receiver and transmitter
  // VT52 ASCII output ...
  output reg [7:0] vt52_data,	// eight bit ASCII output character
  output reg vt52_ready,	// vt52_data is valid (1 clock only!)
  // Display controller, vt52_controller, interfaces ...
  input set_alt_keypad_mode,	// display received <ESC> = sequence
  input reset_alt_keypad_mode,	//  "   "   "    "  <ESC> >  "   "
  input hold_screen_led,	// current hold screen state
  output reg proceed_one_line,	// user pressed Scroll Lock
  output reg proceed_one_screen,//  "    "   "  shift/Scroll Lock
  output reg toggle_hold_screen,//  "    "   "  control/Scroll Lock
  input send_identify		// display received <ESC> Z (identify)
);
  parameter SWAP_CAPS_CTRL   =0;// 1 -> swap CAPS LOCK and CONTROL keys
  parameter INIT_KEYPAD_MODE =0;// initial state of the alt_keypad_mode FF
  parameter RESET_KEYBOARD   =1;// 1 -> reset keyboard when we're reset

// These two little macros make it easy to handle the swap_caps_control flag.
`define CONTROL_KEY   (SWAP_CAPS_CTRL ? `PS2_CAPS_LOCK:`PS2_LEFT_CONTROL)
`define CAPS_LOCK_KEY (SWAP_CAPS_CTRL ? `PS2_LEFT_CONTROL:`PS2_CAPS_LOCK)

  // State definitions for the VT52 keyboard state machine ...
  localparam [4:0]
    INIT0	=  0,		// reset keyboard, part 1
    INIT1	=  1,		//   "    "    "   part 2
    INIT2	=  2,		//   "    "    "   part 3
    INIT3	=  3,		//   "    "    "   part 4
    INIT4       =  4,		//   "    "    "   part 5
    WAITKEY	=  5,		// wait for something to do
    SENDMAP	=  6,		// send mapped ASCII key
    RELEASE	=  7,		// handle key release event
    EXTENDED	=  8,		// handle extended (E0 xx) scan code
    ERELEASE	=  9,		// handle extended release event
    ARROW	= 10,		// send arrow/function key escape sequence
    KEYPAD0	= 11,		// send keypad escape sequences, part 1
    KEYPAD1	= 12,		//   "    "      "     "    "    part 2
    IDENTIFY0	= 13,		// send VT52 identify response, part 1
    IDENTIFY1 	= 14,		//   "    "    "   "    "   "   part 2
    IDENTIFY2	= 15,		//   "    "    "   "    "   "   part 3
    SLEDS0	= 16,		// update keyboard LEDs, part 1
    SLEDS1	= 17,		//   "     "    "   "    part 2
    SLEDS2	= 18,		//   "     "    "   "    part 3
    SLEDS3	= 19,		//   "     "    "   "    part 4
    SLEDS4	= 20;		//   "     "    "   "    part 5
  reg [4:0] state, next;
  
  // Locals...
  wire [7:0] mapped_key;	// translated key from the keymap  
  reg load_mapped;		// load mapped_key with the keymap data
  reg [3:0] keymap;	       	// keymap table to use when mapping key
  wire map_valid;		// scancode has a valid ASCII translation
  reg control_key;		// control key is pressed now
  reg caps_lock;		// caps lock mode is on
  reg left_shift;		// left shift key is pressed now
  reg right_shift;		// right  "    "   "    "     "
  reg alt_keypad_mode;		// alternate keypad mode flag
  reg keypad_mode_changed;	// alt_keypad_mode has changed (update LEDs!)
  reg hold_screen_changed;	// hold screen mode "    "        "     "
  reg last_hold_screen;		// last known state of hold screen
  reg identify_ff;		// VT52 identify message needed
  reg identify_sent;		// clear identify_ff

  // Next state values for everything that needs it ...
  reg next_control_key, next_caps_lock, next_left_shift, next_right_shift;
  reg next_alt_keypad_mode, clear_keypad_mode_changed, clear_hold_screen_changed;


  function isKeypadKey (input [7:0] s);
  //++
  // TRUE if s is the scancode for any VT52 keypad key ...
  //--
  begin
    //   These are the scan codes for the keypad keys 0..9 and "." - these are
    // the keys that are affected by the alt_keypad_mode flag.  Keypad ENTER
    // is also affected by alt_keypad_mode, but it's an extended code and it's
    // not on this list.  Keypad "+" is not here, either - it always sends ASCII
    // "+" regardless of the keypad mode. Likewise, NUM LOCK is not on the list
    // because it doesn't send anything.
    isKeypadKey = (   (s == `PS2_KP_0) | (s == `PS2_KP_1)
		    | (s == `PS2_KP_2) | (s == `PS2_KP_3)
                    | (s == `PS2_KP_4) | (s == `PS2_KP_5)
		    | (s == `PS2_KP_6) | (s == `PS2_KP_7)
		    | (s == `PS2_KP_8) | (s == `PS2_KP_9)
		    | (s == `PS2_KP_DOT) );
  end
  endfunction // isKeypadKey

  function isArrowKey (input [7:0] s);
  //++
  // TRUE if s is the scancode for any of the arrow keys ...
  //--
  begin
    //   These are all extended key sequences - each of these scan codes must
    // be preceeded by E0 first!  That implies that you can only use this
    // function from one of the extended key states...
    isArrowKey = (  (s == `PS2_UP_ARROW  )  |  (s == `PS2_DOWN_ARROW )
	          | (s == `PS2_LEFT_ARROW)  |  (s == `PS2_RIGHT_ARROW)  );
  end
  endfunction // isArrowKey

  task DecodeKey;
  //++
  //   This task will decode the initial byte from the keyboard and figure out
  // the correct next state.  It's called only from the WAITKEY state and the
  // only reason it's a separate task is just to cleanup the code in the main
  // case statement a bit...
  //--
  begin
    //   The message from the keyboard is most likely a key press event, but it
    // also be a release event, an error or status message, or something else.
    // In any case, go ahead and try to map the received byte to ASCII - it
    // saves a state if this is a key press event and does no harm if byte 
    // later turns out to be something else.
    keymap = {1'b0, control_key, left_shift|right_shift, caps_lock};  
    load_mapped = 1;
    //  Now figure out what really happened ..
    if (ps2_rx_data == `PS2_ACK) begin
      //   We don't usually expect to see random ACK (0xFA) messages, but it
      // can happen if a new keyboard is hot pluged (it sends 0xFA after it
      // completes the POST) or maybe for something else.  Just ignore it.
      next = WAITKEY;
    end else if ((ps2_rx_data==`PS2_NACK1) | (ps2_rx_data==`PS2_NACK2)) begin
      //  Random NACKs (0xff or 0x00) usually indicate some kind of transmission
      // errors.  Best to start all over again and reset the keyboard.
      next = INIT0;
    end else if (ps2_rx_data == `PS2_RELEASE_KEY) begin
      // Key up (release) event ...
      next = RELEASE;
    end else if (ps2_rx_data == `PS2_EXTENDED_KEY) begin
      // Extended scan code event (could be either up or down) ...
      next = EXTENDED;
    end else if (ps2_rx_data == `PS2_LEFT_SHIFT) begin
      //   Left shift key - we can handle that right here.  Note that the shift
      // key up event is handled in the RELEASEx state...
      next_left_shift = 1;  next = WAITKEY;
    end else if (ps2_rx_data == `PS2_RIGHT_SHIFT) begin
      // Right shift key - ditto ...
      next_right_shift = 1;  next = WAITKEY;
    end else if (ps2_rx_data == `CONTROL_KEY) begin
      // Control key - also easy ...
      next_control_key = 1;  next = WAITKEY;
    end else if (ps2_rx_data == `CAPS_LOCK_KEY) begin
      //   And caps lock - slightly trickier.  a) caps lock toggles on key press
      // and we don't care about key ups for this key, and b) we have to update
      // the LEDs when the caps lock state changes.
      next_caps_lock = ~caps_lock;  next = SLEDS0;
    end else if (ps2_rx_data == `PS2_NUM_LOCK) begin
      //   Control/Num Lock toggles the current keypad mode.  This is an
      // enhancement (a real VT52 had no equivalent key), but it's very handy.
      if (control_key) begin
	next_alt_keypad_mode = ~alt_keypad_mode;  next = SLEDS0;
      end else
	next = WAITKEY;
    end else if (ps2_rx_data == `PS2_SCROLL_LOCK) begin
      // Scroll Lock has the same function as the HOLD SCREEN key on the VT52...
      casex ({control_key, left_shift, right_shift})
	{1'b1, 1'b0, 1'b0}: toggle_hold_screen = 1;
        {1'b0, 1'b0, 1'b0}: proceed_one_line = 1;
	{1'b0, 1'b1, 1'bx},
	{1'b0, 1'bx, 1'b1}: proceed_one_screen = 1;
	default: ;
      endcase
    end else if ((ps2_rx_data==`PS2_KP_STAR)|(ps2_rx_data==`PS2_KP_MINUS)) begin
      //   Keypad "*" and "-" ALWAYS send escape sequences regardless of
      // the keypad mode (they're mapped to the VT52 RED and GRAY keys) 
      next = ARROW;
    end else if (alt_keypad_mode & isKeypadKey(ps2_rx_data)) begin
      //   In alt keypad mode, keypad keys send escape sequences and have to be
      // handled specially.  In "normal" mode they send ASCII characters and
      // can be mapped and handled like any other keu.
      next = KEYPAD0;
    end else begin
      //   If we get to here, then it must be an ordinary scan code - just map
      // it to ASCII (isn't it good that we loaded the keymap way back there?)
      // and send it to the host.  If it's a key that doesn't have a mapping,
      // eng F1..F12, then the keymap won't assert map_valid and SENDMAP will
      // do nothing...
      next = SENDMAP;
    end
  end
  endtask // DecodeKey
  

  // Generate the scan code to ASCII mapping ROM ...
  ps2_keymap scanmap (
    .clock(clock), .keymap(keymap), .scancode(ps2_rx_data),
    .ascii(mapped_key), .load(load_mapped), .key_valid(map_valid)
  );

  // Initial state ...
  initial begin
    caps_lock = 0;  control_key = 0;  left_shift = 0;  right_shift = 0;
    state = INIT0;  identify_ff = 0;  hold_screen_changed = 0;
    alt_keypad_mode = INIT_KEYPAD_MODE;  keypad_mode_changed = 1;
  end
  
  // Implement the state register for the state machine ...
  always @(posedge clock) begin
    if (reset) begin
      caps_lock <= 0;  control_key <= 0;  left_shift <= 0;  right_shift <= 0;
      state <= RESET_KEYBOARD ? INIT0 : WAITKEY;
    end else begin
      caps_lock <= next_caps_lock;  control_key <= next_control_key;
      left_shift <= next_left_shift;  right_shift <= next_right_shift;
      state <= next;
    end
  end // always @ (posedge clock)

  //   The identify_ff is a special case.  The vt52_controller logic outputs a
  // one clock cycle tick on the send_identify input which sets this FF. It then
  // remains set until the state machine asserts identify_sent, which resets it.
  always @(posedge clock) begin
    if (reset)
      identify_ff <= 0;
    else if (send_identify)
      identify_ff <= 1;
    else if (identify_sent)
      identify_ff <= 0;
  end // always @ (posedge clock)

  //   The alt_keypad_mode ff is yet another funky one - it can be set or reset
  // by the display controller via the set/reset_alt_keypad_mode inputs, or
  // it can be changed internally by the next_alt_keypad_mode bit.  Moreover,
  // if the display controller changes the mode then we have to remember that
  // by setting the keypad_mode_changed ff so that we'll know to update the
  // LEDs.  Ugly, but it all works in the end...
  always @(posedge clock) begin
    if (reset) begin
      alt_keypad_mode <= INIT_KEYPAD_MODE;  keypad_mode_changed <= 1;
    end else begin
      if (set_alt_keypad_mode) begin
	alt_keypad_mode <= 1;  keypad_mode_changed <= 1;
      end else if (reset_alt_keypad_mode) begin
	alt_keypad_mode <= 0;  keypad_mode_changed <= 1;
      end else begin
	alt_keypad_mode <= next_alt_keypad_mode;
	if (clear_keypad_mode_changed) keypad_mode_changed <= 0;
      end
    end
  end // always @ (posedge clock)
  
  //   And one last special case - the hold_screen_changed FF is set when the
  // state of the hold_screen_led input changes.  It lets the state machine
  // know that it's time to update the keyboard LEDs.
  always @(posedge clock)
    last_hold_screen <= hold_screen_led;
  always @(posedge clock) begin
    if (reset) 
      hold_screen_changed <= 0;
    else begin
      if (last_hold_screen != hold_screen_led)
        hold_screen_changed <= 1;
      else if (clear_hold_screen_changed)
        hold_screen_changed <= 0;
    end
  end // always @ (posedge clock)
  

  // And, finally, here is the combinatorial logic for the state machine ...
  always @* begin
    // NONE of these signals should be registered or (God forbid) latched!
    next = state;
    next_caps_lock = caps_lock;  next_control_key = control_key;
    next_left_shift = left_shift;  next_right_shift = right_shift;
    next_alt_keypad_mode = alt_keypad_mode;
    load_mapped = 0;  keymap = 0;  identify_sent = 0;  ps2_reset_rxtx = 0;
    vt52_data = 0;  vt52_ready = 0;  ps2_tx_data = 0;  ps2_tx_enable = 0;
    clear_keypad_mode_changed = 0;  clear_hold_screen_changed = 0;
    toggle_hold_screen = 0;  proceed_one_line = 0;  proceed_one_screen = 0;

    case (state)
      //   The initial startup state attempts to reset the keyboard by sending
      // it the 0xFF reset command.  The keyboard will first reply with an 0xFA
      // ACK byte and then, after it completes the self test, either 0xAA for
      // self test OK, or 0xFC for self test failed.  After receiving a self
      // test OK we send the keyboard the latest LED status and then go wait
      // for a key.
      //
      //   Note that if anything goes wrong in this process - if the self test
      // fails or if the keyboard fails the handshake properly for any of the
      // messages, then we just go back to state INIT0 and try all over again.
      // If the keyboard really is broken then this isn't likely to work the
      // second time around either, but there isn't really anything else we
      // can do. And if the errors are transient, say because the user just hot
      // plugged a keyboard, then it should eventually succeed.
      //
      // Reset our own PS/2 transmitter and receiver to the IDLE state.
      INIT0: begin
        ps2_reset_rxtx = 1;  next = INIT1;
      end // case: INIT0
      
      // Send the keyboard a RESET command ...
      INIT1: begin
        ps2_tx_data = `PS2_RESET;  ps2_tx_enable = 1;
	if (!ps2_tx_idle)  next = INIT2;
      end // case: INIT1
      
      // Wait for the command to finish sending ...
      INIT2: begin
	if (ps2_tx_error)
	  next = INIT0;
	else if (ps2_tx_done)
	  next = INIT3;
      end // case: INIT2
      
      // Wait for the 0xFA byte to be received ...
      INIT3: begin
	if (ps2_rx_error)
	  next = INIT0;
	else if (ps2_rx_done) begin
	  if (ps2_rx_data == `PS2_ACK)
	    next = INIT4;
	  else
	    next = INIT0;
	end
      end // case: INIT3
      
      // Wait for the 0xAA POST OK status ...
      INIT4: begin
	if (ps2_rx_error)
	  next = INIT0;
	else if (ps2_rx_done) begin
	  if (ps2_rx_data == `PS2_POST_OK)
	    next = SLEDS0;
	  else
	    next = INIT0;
	end
      end // case: INIT4

      //   Wait for a) a message from the keyboard, b) a keyboard error, 
      // c) a request to send an identify message, or d) a change in the
      // status LEDs.  That's about all we're good for!
      WAITKEY: begin
	if (ps2_rx_error) begin
	  // Reset the keyboard and start all over again ...
	  next = INIT0;
	end else if (ps2_rx_done) begin
	  // Decode the keycode (or whatever else) ...
	  DecodeKey;
        end else if (identify_ff) begin
	  // send VT52 identify message
          next = IDENTIFY0;
	end else if (keypad_mode_changed | hold_screen_changed) begin
	  // Update the keyboard LEDs
	  next = SLEDS0;
	end
      end // case: WAITKEY

      //   This is a "normal" key press event - if there's an ASCII translation
      // in the keymap (and we'll know from map_valid) then send it.
      SENDMAP: begin
	if (map_valid) begin
	  vt52_data = mapped_key;  vt52_ready = 1;
	end
	next = WAITKEY;
      end // case: SENDMAP

      //   We get here for a key release event.  For the most part we don't
      // care, but we still have to wait for and read the keycode byte that
      // comes after the $F0.  And there are a few keys, notably shifts and
      // control, where we do need to know about key up events.
      RELEASE: begin
	if (ps2_rx_error)
	  next = INIT0;
	else if (ps2_rx_done) begin
	  if (ps2_rx_data == `PS2_LEFT_SHIFT)
	    next_left_shift = 0;
          else if (ps2_rx_data == `PS2_RIGHT_SHIFT)
	    next_right_shift = 0;
	  else if (ps2_rx_data == `CONTROL_KEY)
	    next_control_key = 0;
	  next = WAITKEY;
	end
      end // case: RELEASE

      //   Here for an extended key.  The four arrow keys all send extended
      // sequences and are trapped here.  Besids those, the only two extended
      // keys that we care about are keypad "/" (E0 4A) and keypad ENTER (E0
      // 5A).  Rather than create another map or something we just special
      // case those two keys right here.  The other thing to watch out for is
      // that extended keys, even those that we ignore, send E0 F0 <key> when
      // they are released (NOT the F0 E0 as you might think) so we have to
      // watch for extended release events here too.
      EXTENDED: begin
	if (ps2_rx_error)
	  next = INIT0;
	else if (ps2_rx_done) begin
	  if (ps2_rx_data == `PS2_RELEASE_KEY)
	    next = ERELEASE;
	  else if (ps2_rx_data == `PS2_KP_ENTER) begin
	    //   Keypad ENTER sends either 0x0D or an escape sequence, depending
	    // on the keypad mode.  Note that scan code 5A is already mapped to
	    // ASCII 0x0D (it's the same scan code as the main keyboard ENTER
	    // key) so we can cheat in that case and let SENDMAP handle it.
	    keymap = 0;  load_mapped = 1;
	    next = alt_keypad_mode ? KEYPAD0 : SENDMAP;
	  end else if (ps2_rx_data == `PS2_KP_SLASH)
	    //   Keypad "/" always sends <ESC>P regardless of alt_keypad_mode.
	    // It's equivalent to the VT52 BLUE key...
	    next = ARROW;
	  else if (isArrowKey(ps2_rx_data))
	    next = ARROW;
          else
	    // Just ignore it ...
	    next = WAITKEY;
	end
      end // case: EXTENDED

      //   Here for an extended key up event.  We just wait for the keycode
      // that we know is coming and then eat it - there are none of these keys
      // that we care about.  Note that we can't share this state with the
      // regular release because of the chance that an extended key up might be
      // confused with a shift or control key up event...
      ERELEASE: begin
	if (ps2_rx_error)
	  next = INIT0;
	else if (ps2_rx_done)
	  next = WAITKEY;
      end // case: ERELEASE

      //   This state sends <ESC> and then the mapped ASCII code from keymap 8.
      // It's used for the arrow keys and the three VT52 function keys (BLUE,
      // RED and GRAY - keypad "/", "*" and "-").
      ARROW: begin
	keymap = `KEYTABLE_ARROWS;  load_mapped = 1;
	vt52_data = `ESC;  vt52_ready = 1;  next = SENDMAP;
      end // case: ARROW

      //   And these states send <ESC>, "?" and then the mapped ASCII code from
      // keymap #9.  They're used for the keypad keys "0" thru "9", "." and
      // keypad ENTER in application mode.
      KEYPAD0: begin
	keymap = `KEYTABLE_KEYPAD;  load_mapped = 1;
	vt52_data = `ESC;  vt52_ready = 1;  next = KEYPAD1;
      end // case: KEYPAD0

      KEYPAD1: begin
	vt52_data = "?";  vt52_ready = 1;  next = SENDMAP;
      end // case: KEYPAD1

      //   These three states send the identify response for a VT52 w/o copier,
      // <ESC> "/" and "K".  It's just hardwired to send those - no keymaps or
      // anything else fancy are used ...
      IDENTIFY0: begin
	vt52_data = `ESC;  vt52_ready = 1;  next = IDENTIFY1;
      end // case: IDENTIFY0
      IDENTIFY1: begin
	vt52_data = "/";  vt52_ready = 1;  next = IDENTIFY2;
      end // case: IDENTIFY1
      IDENTIFY2: begin
	vt52_data = "K";  vt52_ready = 1;  identify_sent = 1;  next = WAITKEY;
      end // case: IDENTIFY2
      
      //   These states update the keyboard LEDs -
      //
      //	CAPS LOCK LED   <- caps lock mode
      //	NUM LOCK LED    <- alternate keypad mode
      //	SCROLL LOCK LED <- hold screen mode
      //
      // First we send the keyboard a 0xED byte and wait for it to reply with
      // 0xFA, and then we send a byte with the LED bits and wait for it to
      // reply again with the 0xFA byte.  Unfortunately all this handshaking
      // takes way more states than I'd like, but that's the way it is.
      //
      //    Send the 0xED byte.  Probably we should wait for tx_idle first, but
      // there's really no need - we're the only process that's sending, and we
      // always wait for tx_done, so it must be idle, right??
      SLEDS0: begin
        ps2_tx_data = `PS2_SET_LEDS;  ps2_tx_enable = 1;
	if (!ps2_tx_idle)  next = SLEDS1;
      end // case: SLEDS0

      // Wait for either tx_done or tx_error ...
      SLEDS1: begin
	if (ps2_tx_error)
	  next = INIT0;
	else if (ps2_tx_done)
	  next = SLEDS2;
      end // case: SLEDS1

      // Wait to receive the 0xFA ACK byte ...
      SLEDS2: begin
	if (ps2_rx_error)
	  next = INIT0;
	else if (ps2_rx_done) begin
	  if (ps2_rx_data == `PS2_ACK)
	    next = SLEDS3;
	  else
	    next = INIT0;
	end // if (ps2_rx_done)
      end // case: SLEDS2

      // Send the LED byte ...
      SLEDS3: begin
	ps2_tx_data = {5'b0, caps_lock, alt_keypad_mode, hold_screen_led};
	ps2_tx_enable = 1;
	clear_keypad_mode_changed = 1;  clear_hold_screen_changed = 1;
	if (!ps2_tx_idle)  next = SLEDS4;
      end // case: SLEDS3
      
      // And wait for either tx_done or tx_error ...
      //   Note that the keyboard will send yet another ACK message after it
      // receives the LED byte and we should probably add yet another state to
      // wait for and check this, but this has already gone on too long.  The
      // WAITKEY state, via DecodeKey, will ignore spurious ACK messages anyway,
      // and we'll just let this one slip thru.
      SLEDS4: begin
	if (ps2_tx_error)
	  next = INIT0;
	else if (ps2_tx_done)
	  next = WAITKEY;
      end // case: SLEDS4

      // We should never get here, but just in case ...
      default:  next = INIT0;
    endcase // case (state)
  end // always @ *

`undef CONTROL_KEY
`undef CAPS_LOCK_KEY
endmodule // ps2_controller


module ps2_keyboard
  //++
  //  This module ties it all together into a nice package...
  //--
(
  input clock,			// system clock
  input reset,			// synchronous reset of keyboard state
  inout ps2c,			// raw PS/2 clock from keyboard
  inout ps2d,			//  "    "  data   "     "   "
  output [7:0] dout,		// buffered ASCII data from keyboard
  output ready,			// data ready on dout
  // Display controller, vt52_controller, interfaces ...
  input set_alt_keypad_mode,	// display received <ESC> = sequence
  input reset_alt_keypad_mode,	//  "   "   "    "  <ESC> >  "   "
  input hold_screen_led,	// current hold screen state
  output proceed_one_line,	// user pressed Scroll Lock
  output proceed_one_screen,	//  "    "   "  shift/Scroll Lock
  output toggle_hold_screen,	//  "    "   "  control/Scroll Lock
  input send_identify		// display received <ESC> Z (identify)
);
  parameter SWAP_CAPS_CTRL   = 0;
  parameter INIT_KEYPAD_MODE = 0;
  parameter RESET_KEYBOARD   = 1;

  // Locals ...
  wire clean_ps2d, ps2c_edge, local_reset, ps2_reset_rxtx;
  wire ps2_rx_done, ps2_rx_error, ps2_tx_idle;
  wire [7:0] ps2_rx_data, ps2_tx_data;  
  wire ps2_tx_done, ps2_tx_error, ps2_tx_enable;

  // Clean up the PS/2 clock and data signals ...
  ps2_cleanup cleanup (
    .clock(clock), .ps2c_in(ps2c), .ps2d_in(ps2d),
    .ps2c_out(), .ps2d_out(clean_ps2d), .ps2c_edge(ps2c_edge)
  );

  // Generate an extended reset for this module ...
  ps2_extended_reset extended_reset (
    .clock(clock), .reset(reset), .extended_reset(local_reset)
  );

  // A receiver for raw PS/2 scan codes ...
  //   Note that the receiver is connected to the cleaned up PS/2 data, not
  // the raw ps2d, and it doesn't use the PS/2 clock, only the clock edge.
  ps2_rx rx (
    .clock(clock), .reset(local_reset|ps2_reset_rxtx),
    .ps2c_edge(ps2c_edge), .ps2d(clean_ps2d), .enable(ps2_tx_idle),
    .done(ps2_rx_done), .error(ps2_rx_error), .data(ps2_rx_data)
  );
  
  // A transmitter to send messages back to the keyboard ...
  //  The transmitter, however, is connected to both the raw PS/2 data and
  // clock (so that it can drive them) but it also uses the cleaned up PS/2
  // clock edge for timing ...
  ps2_tx tx (
    .clock(clock), .reset(local_reset|ps2_reset_rxtx), .ps2c_edge(ps2c_edge),
    .ps2d(ps2d), .ps2c(ps2c), .enable(ps2_tx_enable), .data(ps2_tx_data),
    .done(ps2_tx_done), .error(ps2_tx_error), .idle(ps2_tx_idle)
  );

  // A state machine to convert the scan codes to ASCII ...
  ps2_controller #(
    .SWAP_CAPS_CTRL(SWAP_CAPS_CTRL),
    .INIT_KEYPAD_MODE(INIT_KEYPAD_MODE),
    .RESET_KEYBOARD(RESET_KEYBOARD)
  ) controller (
    .clock(clock), .reset(local_reset),
    .ps2_rx_data(ps2_rx_data), .ps2_rx_done(ps2_rx_done),
    .ps2_rx_error(ps2_rx_error), .ps2_tx_data(ps2_tx_data),
    .ps2_tx_enable(ps2_tx_enable), .ps2_tx_done(ps2_tx_done),
    .ps2_tx_error(ps2_tx_error), .ps2_tx_idle(ps2_tx_idle),
    .ps2_reset_rxtx(ps2_reset_rxtx),
    .vt52_data(dout), .vt52_ready(ready),
    .set_alt_keypad_mode(set_alt_keypad_mode),
    .reset_alt_keypad_mode(reset_alt_keypad_mode),
    .hold_screen_led(hold_screen_led),
    .proceed_one_line(proceed_one_line),
    .proceed_one_screen(proceed_one_screen),
    .toggle_hold_screen(toggle_hold_screen),
    .send_identify(send_identify)
  );
endmodule // ps2_keyboard

