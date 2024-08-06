//++
//hd6120.v - one memory cycle per clock HD6120 compatible CPU implementation
//Copyright (C) 2011-2014 by Spare Time Gizmos.  All rights reserved.
//
// DESCRIPTION:
//   This module implements a PDP-8 CPU that's roughly similar to the HD6120
// chip.  The implementation is (or should be!) 100% software compatible with
// the 6120, including panel mode and all the 6120 extensions, however the
// hardware interface is simplified and somewhat different from a real 6120.
// Also, this is not a structural simulation and the internal structure of
// this CPU is nothing like a 6120, and consequently the external bus timing
// is nothing like a real 6120.  Our goal here is to do one memory cycle for
// every clock, so we essentially run as fast as the memory can support.
// That's pretty much the way it worked out, although there are a few states
// (e.g. for OPR instructions) that require a non-memory clock cycle.
//
// DIAGNOSTICS:
//    This module will successfully run the standard set of PDP-8/E CPU
// diagnostics when used with the hd6120_tb test bench file.  The MAINDECs
// successfully passed are -
//
//  Test Description                         SA   SR    Status      Clocks
//  ---- ----------------------------------- ---- ----  ------ -----------
//  d0ab Basic Instruction Test, part 1      0200 7777    PASS     6356184
//  d0bb Basic Instruction Test, part 2      0200 0000    PASS     5384708
//  d0cc Adder Test                          0200 0400    PASS    40178056
//  d0db Random AND Test                     0200 0000    PASS     2506526
//  d0eb Random TAD Test                     0200 0000    PASS     5628487
//  d0fc Random ISZ Test                     0200 0000    PASS     6772799
//  d0gc Random DCA Test                     0200 0000    PASS     4974693
//  d0hc Random JMP Test                     0200 0000    PASS     7337609
//  d0ib BASIC JMP-JMS Test                  0200 0000    PASS     7930307
//  d0jc Random JMP-JMS Test                 0200 0000    PASS     9500452
//  d1ha Memory Extension and Timeshare Test 0200 4007    PASS   196982855
//
//   Note that there are some subtle differences between the HD6120 behavior
// and that of a real 8/E that will cause errors in some of the diagnostics.
// To get a clean bill of health from the MAINDECs, define the DIAGNOSTICS
// simple when compiling this module.  Don't try to define DIAGNOSTICS for
// the real SBC6120, however - BTS6120 won't run that way!  FWIW, the real
// killer in this regard is the way RTF handles the IE flag - on an 8/E, RTF
// always sets IE, but on the HD6120 RTF restores the previous state of IE
// from the AC.  The HD6120 designers clearly did this so that you could use
// RTF to restore IE when exiting from panel mode, and that's what screws
// up BTS6120 if you use the 8/E interpretation.  
//
// NOTES:
//   Here's a list of the known differences between what this module does and
// what a real HD6120 chip would do -
//
// * The address bus is not multiplexed with the data lines, nor are the Cx bits
//   multiplexed with the EMA bits.  Since the address bits are not multiplexed
//   there is no need for LXMAR/LXPAR/LXDAR, and these along with the READ and
//   WRITE strobes have been replaced by mrd, mwr, iord and iowr.
//
// * The cpmem output, in combination with mrd or mwr, indicates a read or write
//   to panel memory.  This can be used in place of LXPAR to implement separate
//   main and panel memory spaces.
//
// * The MEMSEL output of the HD6120 does not exist (I never figured out what
//   that one was good for, anyway!).
//
// * Likewise, the OUT output does not exist - it's superfluous as the DX bus
//   drivers are contained in this module anyway.
//
// * This module has no provision for DMA, and the DMAREQ and DMAGNT signals
//   have been removed.  Likewise, the EMA and AX outputs are not tristate.
//
// * The HD6120 ACK input is not implemented, however the iowait signal performs
//   a similar function.  iowait simply inhibits the update of the internal
//   state that would otherwise occur on a positive clock edge and can be used
//   to add clock cycles to any machine cycle.  It's isn't specifically limited
//   to I/O, although that's its only current application.
//
// * The switch register is implemented as two separate, semi-general purpose
//   I/O busses.  The swin parameter is a 12 bit input bus that's used for the
//   OSR/LAS instructions.  There's no read strobe, and the outside world is
//   expected to drive the contents of the switch register onto this port
//   continuously.  Likewise, the swout parameter is a 12 bit ouptut port
//   that's loaded by the HS6120 WSR instruction. This port continuously outputs
//   the last WSR data.  Note that it's entirely possible, and very convenient,
//   for our parent module to simply connect these two ports together.  That
//   gives the effect of a twelve bit "soft" switch register that's loaded by
//   the WSR instruction and read by OSR/LAS.  
//
// * The function of the STRTUP input is provided by the STARTUP_MODE parameter
//   to this module.  Additionally the startup PC may be modified (the default
//   of 7777 is the same as the HD6120) by the STARTUP_PC parameter.
//
// * The INTGNT output is true for only one clock cycle (the one that occurs
//   while the interrupt JMS 0 is being executed).  In the real HD6120 INTGNT
//   remains asserted until the next IOT instruction - Harris did this to
//   implement the priority interrupt chaining of the 6121 chips, which we
//   don't care about.
//
// * The RUN/-HLT input and the RUN status output are not implemented.  In this
//   implementation the CPU is always running. A HLT instruction in main memory
//   will trap to panel mode via HLTFLG just as a real HD6120 would.  A HLT in
//   panel mode sets HLTFLG but doesn't actually halt - this combination is used
//   for single stepping main memory programs.  HLTFLG can be cleared by a PGO
//   instruction.  Again, this behavior is just like a real HD6120.
//
// * The "illegal" operate instruction RTR RTL loads the FPGA version number
//   into the AC.  [FWIW, the HD6120 R3L instruction is RAR RAL.]
//
// REVISION HISTORY:
// 14-Jan-11  RLA  New file.
//  7-Feb-11  RLA  InterruptNeeded and PanelTrapNeeded really only depend on the
//                   next state if ii and ie - the current state is irrelevant.
//                 Regular interrupts should be inhibited while ctrl_ff is set
//                 RTF always clears the AC!  (Oddly, the diagnostics didn't
//                   catch this - it caused some really odd problems when
//                   booting OS/8 because the AC was getting trashed on exit
//                   from BTS6120 after a PR0 disk read function!)
//                 A CIF followed by a JMS writes the return address to the new
//                   (i.e. IB) field, NOT the current (i.e. IF) !!
//                 Add the iowait input for the CF card interface.
// 15-Feb-11  RLA  Revise the switch register implement to allow for external
//                   switches (a necessity for the FP6120!)...
//  1-Jan-14  RLA  Make the reset input synchronous (not asynchronous).  Move
//                   all the actual register initialization to the S_RESET
//                   state.  Add an initial block to set the initial CPU state
//                   to S_RESET, so an explicit reset input isn't required.
//--
//000000001111111111222222222233333333334444444444555555555566666666667777777777
//234567890123456789012345678901234567890123456789012345678901234567890123456789
`include "sbc6120_h.v"          // project global definitions
`include "opcodes_h.v"          // PDP-8/HD6120 opcode mnemonics
`include "version_h.v"          // define the FPGA_VERSION macro

module hd6120 #(
  // Parameters ...
  parameter STARTUP_MODE=1,     // 1 -> panel mode, 0 -> normal mode
  parameter STARTUP_PC=12'O7777 // initial PC value
) (
  // Global signals ...
  input              clock,     // master clock for all operations
  input              reset,     // force S_RESET state next
  // Shared address and data busses ...
  output reg [`WORD] ax,        // memory address and IOT opcode
  output reg [`EMA]  ema,       // extended memory address bits
  inout [`WORD]      dx,        // memory and I/O data
  // Memory control signals ...
  output reg         mrd,       // drive memory data onto the dx bus
  output reg         mwr,       // write the DataBus to memory
  output reg         cpmem,     // asserted if memory reference is to CP memory
  // I/O device control signals ...
  output reg         ioclr,     // initialize all I/O devices (CAF)
  output reg         iord,      // drive external I/O device data onto dx bus
  output reg         iowr,      // write the DataBus to an external device
  input              ioskip,    // asserted during iowr if the IOT skips
  input              ioc0,      // asserted during iowr to clear the AC
  input              ioc1,      // asserted during iowr to read data into the AC
  input              intreq,    // request an interrupt during next fetch
  output reg         intgnt,    // asserted during an interrupt JMS 0
  input              iowait,    // asserted to stretch an I/O cycle
  // Other status signals ...
  input [`WORD]      swin,      // switch register input port for OSR/LAS
  output reg [`WORD] swout,     // display data output port for WSR
  input              cpreq,     // asserted to request a control panel interrupt
  output reg         ifetch,    // asserted with mrd during instruction fetch
  output reg         dataf      // asserted with mrd when data field is used
);

  //////////////////////////////////////////////////////////////////////////////
  /////////////////////////   D E C L A R A T I O N S   ////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //   These are the states and state variables for our implementation of the
  // HD6120.  Yes, it would probably be better to use a one-hot encoding, but
  // it's easier to write it this way and let XST infer the state machine.
  // Xilinx is perfectly capable of selecting the optimal encoding itself, and
  // it'll convert this to a one-hot implementation without my help...
  localparam [3:0]
    S_RESET     = 4'd0,         // reset state
    S_INTGNT    = 4'd1,         // interrupt acknowledge (JMS 0) cycle
    S_FETCH     = 4'd2,         // fetch opcode and calculate EA
    S_DEFER     = 4'd3,         // fetch indirect address
    S_INDEX     = 4'd4,         // increment and rewrite index register
    S_ANDTAD    = 4'd5,         // execute AND or TAD instruction       
    S_ISZ_0     = 4'd6,         //    "    ISZ (read and increment)
    S_ISZ_1     = 4'd7,         //    "    ISZ (write updated count)
    S_DCA       = 4'd8,         //    "    DCA 
    S_JMS       = 4'd9,         //    "    JMS instruction
    S_IOT_0     = 4'd10,        //    "    IOT (write AC to device)
    S_IOT_1     = 4'd11,        //    "    IOT (read AC from device)
    S_OPR       = 4'd12,        //    "    OPR (all of 'em!)
    S_PNLTRP    = 4'd13;        // control panel trap
  reg [3:0] state, next_state;

  // HD6120 registers and flip-flops ...
  // Registers ...
  reg [`WORD]  ir;      // instruction register
  reg [`WORD]  pc;      // program counter
  reg [`WORD]  ma;      // memory address (temporary)
  reg [`WORD]  mb;      // memory data (temporary)
  reg [`WORD]  sp1;     // stack pointer #1
  reg [`WORD]  sp2;     // stack pointer #2
  reg [`WORD]  mq;      // multiplier quotient
  reg [`WORD]  ac;      // accumulator
  reg          link;    // link
  reg [`EMA]   dfr;     // data field register
  reg [`EMA]   ifr;     // instruction field register
  reg [`EMA]   ibr;     // instruction field buffer register
  reg [`EMA]   dsf;     // save data field (after interrupt)
  reg [`EMA]   isf;     // save instruction field
  // Flags ...
  reg ie_ff;            // interrupt enable
  reg ii_ff;            // interrupt inhibit
  reg ctrl_ff;          // control panel mode
  reg ffetch;           // force an instruction FETCH state next
  reg pex_ff;           // exit panel mode on next JMP/JMS/RTN
  reg fz_ff;            // force field zero address
  reg pdf_ff;           // panel data flag
  reg pnltrp_ff;        // panel mode trap for PRS instructions
  reg hltflg_ff;        // panel mode trap for HLT instruction
  reg btstrp_ff;        // panel mode trap for external cpreq
  reg pwron_ff;         // panel mode trap for power on reset
  reg gt_ff;            // greater than flag

  // These are the new, next state, values for all those same registers ...
  reg [`WORD] next_ir, next_pc, next_ma, next_mb, next_sp1, next_sp2;
  reg [`WORD] next_mq, next_ac, next_swout;
  reg [`EMA] next_dfr, next_ifr, next_ibr, next_dsf, next_isf;  reg next_dataf;
  reg next_link, next_ie, next_ii, next_ctrl, next_fz, next_pdf, next_gt;
  reg next_pnltrp, next_hltflg, next_btstrp, next_pwron, next_pex, last_cpreq;

`ifdef DIAGNOSTICS
  //   The halted flag is set by the HLT instruction but does nothing to change
  // the way HLT works - it still traps to panel memory via hltflg_ff.  This
  // signal is not used internally at all, but the diagnostics testbench looks
  // at it to determine if an 8/E diagnostic program has halted.  For synthesis
  // this signal is simply optimized away, since it drives no loads.
  reg halted;
`endif

  // Split out a separate dx_out bus to drive the tristate data bus...
  reg [`WORD] dx_out;  reg write;
  assign dx = write ? dx_out : 12'ozzzz;
  
  //////////////////////////////////////////////////////////////////////////////
  ////////////   L O C A L   T A S K S   A N D   F U N C T I O N S   ///////////
  //////////////////////////////////////////////////////////////////////////////

  function InterruptNeeded (input dummy);
  //++
  //   This function will decide whether an INTGNT state is needed.  That's
  // not the same as just asking whether intreq is asserted (that's trivial!)
  // - this takes into account the interrupt enable, inhibit, and other FFs.
  // It still sounds easy, but there are a few complications -
  //
  //   On a real 8/E, the IOF instruction clears the IE FF before the priority
  // network decides on whether to interrupt or fetch.  This means that the two
  // instruction sequence "ION, IOF" will not interrupt even if there is an
  // outstanding INTREQ at the time.  To get the same behavior out of our little
  // machine, we have to look ahead at the next state of ie to see if it's
  // going to be cleared soon. 
  //
  //   Likewise, there's a similar problem with the JMS/JMS/RTN instruction that
  // clears the interrupt inhibit.  On a real -8, if there's a request out-
  // standing during a JMP that clears the II FF, then that interrupt will be
  // recognized immediately after the JMP finishes.  It doesn't execute the
  // instruction after the JMP.  To get the same behavior, we have to cheat and
  // look ahead at the next state of the ii_ff...
  //
  //   In fact, in the end it turns out that the current state of the ie_ff and
  // ii_ff don't matter at all - it's the next state of both these FFs that
  // counts.  That makes sense if you think about it, since we're actually here
  // to determine what happens with the next instruction, not the current one.
  //
  //  Lastly, the ION and RTF instructions are always followed by a FETCH, which
  // means that interrupts (and that includes panel traps) are not recognized
  // until after the instruction following ION/RTF has been executed. We use the
  // ffetch flag to get this result - it's asserted in the ION and RTF states,
  // and inhibits an interrupt at the end of that state.
  //--
  begin
    InterruptNeeded = intreq & next_ie & !next_ii & !ctrl_ff & !ffetch;
  end
  endfunction // InterruptNeeded
  
  function PanelTrapNeeded (input dummy);
  //++
  //   And this function decides if a PNLTRP state is needed.  It's like the
  // INTGNT case except that the trap conditions are different and there's no
  // specific enable bit for panel traps.  Panel traps are, however, inhibited
  // by many of the same things that inhibit interrupts, including the ii_ff.
  //
  //   Note that the traps for the PRS and HLT instructions have a "look ahead"
  // problem similar to the ii issue discussed above - PRS and HLT will set
  // their respective pnltrp_ff and hltflg_ff flags, but until after the next
  // state has already been decided.  That means one more instruction will be
  // fetched and executed before a trap occurs, which is bad.  The solution is
  // to cheat a little and look ahead at the next_pnltrp and next_hltflg
  // signals.
  //--
  reg trap;
  begin
    trap = (pnltrp_ff|next_pnltrp) | (hltflg_ff|next_hltflg) | btstrp_ff | pwron_ff;
    PanelTrapNeeded = trap & !ctrl_ff & !next_ii & !ffetch;
  end
  endfunction // PanelTrapNeeded

  task ExecuteDone;
  //++
  //   This task is invoked in the last execution state for most instructions.
  // It sets the next state to be FETCH unless there's a request for either a
  // control panel trap or regular interrupt that can be serviced.
  //--
  begin
    if (PanelTrapNeeded(0))
      next_state = S_PNLTRP;
    else if (InterruptNeeded(0))
      next_state = S_INTGNT;
    else
      next_state = S_FETCH;
  end
  endtask // ExecuteDone


  task ExecuteNext (input [`WORD] opcode);
  //++
  //   And this task sets the next state to the correct execute state for the
  // PDP-8 opcode given as the argument.  Notice that for a JMP instruction,
  // the "execute" state is FETCH!  Yes, that's correct - the EA is loaded
  //  directly into the PC during this fetch, and there's no need for any
  // execute state.
  //--
  begin
         if (`isAND(opcode)) next_state = S_ANDTAD;
    else if (`isTAD(opcode)) next_state = S_ANDTAD;
    else if (`isISZ(opcode)) next_state = S_ISZ_0;
    else if (`isDCA(opcode)) next_state = S_DCA;
    else if (`isJMS(opcode)) next_state = S_JMS;
    else if (`isJMP(opcode)) ExecuteDone;
    else if (`isIOT(opcode)) next_state = S_IOT_0;
    else if (`isOPR(opcode)) next_state = S_OPR;
    //   Now that should take care of all possible cases and we could omit the
    // "if ... isOPR(...)" from the last case, but we intentionally leave it
    // in.  If none of the previous conditions are true (say, for example if
    // IR contains "xxxx") then NextState won't be set and the simulation will
    // immediately bomb out with an unknown state vector.  
  end
  endtask // ExecuteNext   


  task OutputEMA (input is_jms);
  //++
  //   This task outputs the extended memory address bits, and also the cpmem
  // flag, according to the current state of the dataf, ctrl_ff, fz and pdf
  // flags.  It's messy, but a truth table makes it fairly straight forward.
  //
  //   Yet another PDP-8 subtlety is that for a JMS instruction the IB to IF
  // transfer takes place before the return address is written to memory.
  // This is critical, of course, because the return address needs to be saved
  // to the new instruction field, not the old one.  Since we do everything in
  // one cycle we don't have the luxury of doing the IB -> IF first, so instead
  // we have yet another special case for JMS instructions.
  //--
  begin
    casex ({ctrl_ff, fz_ff, pdf_ff, dataf})
      4'b0xx0: begin  ema = is_jms ? ibr : ifr;  cpmem = 0;  end  // main memory direct
      4'b0xx1: begin  ema = dfr;                 cpmem = 0;  end  // main memory indirect
      4'b10x0: begin  ema = is_jms ? ibr : ifr;  cpmem = 1;  end  // panel direct, FZ=0
      4'b11x0: begin  ema = 3'o0;                cpmem = 1;  end  // panel direct, FZ=1
      4'b1x01: begin  ema = dfr;                 cpmem = 0;  end  // panel indirect, PDF=0
      4'b1x11: begin  ema = dfr;                 cpmem = 1;  end  // panel indirect, PDF=1
    endcase // casex ({cntrl_ff, fz_ff, pdf_ff, dataf})
  end
  endtask // OutputEMA


  task StackEMA;
  //++
  //   This task is similar to OutputEMA, except that this version is used for
  // HD6120 stack operations (PPC, PAC, POP, RTN).  The stack follows its own
  // set of rules - the EMA is always zero.  Stack operations in panel memory
  // reference a panel memory stack and main memory stack operations always
  // reference the main memory stack. Things like dataf, pdf_ff, and fz_ff
  // don't have any effect on stack operations.
  //--
  begin
    ema = 3'o0;
    //   There's some fine print in the 6120 data sheet that says a RTN
    // after a PEX fetches the return address from the main memory stack,
    // not the panel memory.  Don't know if anybody would ever notice or
    // care, but we'll try to do the right thing ...
    if (ctrl_ff & pex_ff & ((ir==`IO_RTN1) | (ir==`IO_RTN2)))
      cpmem = 0;
    else
      cpmem = ctrl_ff;
  end
  endtask // StackEMA


  // These two macros create memory read and memory write cycles.
`define MRD(q)      OutputEMA(0);  ax = (q);  mrd = 1;  
`define MWR(q)      OutputEMA(0);  ax = (q);  write = 1;  mwr = 1;
  //   You'd like to have the MRDR and MWRR macros call MRD and MWR, wouldn't
  // you?  I certainly would, and it works in Icarus, but Xilinx XST doesn't
  // seem to understand nested macro expansions.  Bummer!
`define MRDR(q,r)   OutputEMA(0); ax=(q); mrd=1; r=dx;  
`define MWRR(q,r)   dx_out=(r); OutputEMA(0); ax=(q); write=1; mwr=1;  
  // And this set is used exclusively for stack operations ...
`define SRDR(q,r)   StackEMA; ax=(q); mrd=1; r=dx;  
`define SWRR(q,r)   dx_out=(r); StackEMA; ax=(q); write=1; mwr=1;  


  task OperateG1;
  //++
  //   This task implements all the logic necessary to execute a group 1 OPR
  // instruction - CLA, CLL, CMA, CML, RAR, RAL, IAC and any combinations there
  // of.  It decodes the corresponding OPR bits in the instruction register and
  // sets the next_ac and next_link values as needed.
  //--
  reg [`WORD] t1, t2, t3, sum;  // intermediate results for the AC
  reg l1, l2, l3;               //   "    "        "     "   "  LINK
  reg carry;                    // TRUE for a carry out from IAC
  begin
    //   The first state intermediate result is either a) the current ac/link
    // values, or b) zeros, depending on the CLA/CLL bits.   The second stage
    // intermediate results complement either the AC or LINK or both, depending
    // in the CMA and CML bits...
    t1 = ir[`Gx_CLA] ? 12'o0000 : ac;
    l1 = ir[`G1_CLL] ? 1'b0 : link;
    t2 = ir[`G1_CMA] ? ~t1 : t1;
    l2 = ir[`G1_CML] ? ~l1 : l1;
  
    // The third stage intermediate result comes from IAC ...
    {carry, sum} = t2 + 12'o0001;
    t3 = ir[`G1_IAC] ? sum : t2;
    l3 = ir[`G1_IAC] ? (carry ? ~l2 : l2) : l2;
 
    //   And then the last stage is the rotate unit ...  Notice that this code
    // implements the HD6120 only instruction R3L (coded as RAL RAR) and our own
    // special "magic" instruction to return the FPGA version number (coded as
    // RTL RTR)...  You can simply comment out those two lines if you don't
    // want them.
    case ({ir[`G1_RAR], ir[`G1_RAL], ir[`G1_TWICE]})
      4'b001:  {next_link,next_ac} = {l3,t3[6:11],t3[0:5]};       // BSW
      4'b010:  {next_link,next_ac} = {t3[0],t3[1:11],l3};         // RAL
      4'b011:  {next_link,next_ac} = {t3[1],t3[2:11],l3,t3[0]};   // RTL
      4'b100:  {next_link,next_ac} = {t3[11],l3,t3[0:10]};        // RAR
      4'b101:  {next_link,next_ac} = {t3[10],t3[11],l3,t3[0:9]};  // RTR
      4'b110:  {next_link,next_ac} = {l3,t3[3:11],t3[0:2]};       // R3L
      4'b111:  {next_link,next_ac} = {l3,`FPGA_VERSION};          // VERSION
      default: {next_link,next_ac} = {l3,t3};                     // nop
    endcase // case ({ir[`G1_RAR], ir[`G1_RAL], ir[`G1_TWICE]})
  end
  endtask // OperateG1


  task OperateG2;
  //++
  //   This task implements all the logic necessary to execute a group 2 OPR
  // instruction - CLA, SMA, SZA, SNL, OSR and HLT plus any combination of
  // those.  It decodes the corresponding bits in the instruction register and
  // sets the next_ac and next_pc as needed.  Note that in this implementation
  // the OSR instruction just reads the sr internal register - nothing is read
  // from the data bus and no bus cycle is required.  Also note that the HLT
  // instruction is handled here - it simply sets the hltflg via next_hltflg.
  //--
  begin
    //  Even though the list of skip conditions looks pretty long in the PDP-8
    // manual, there's really only three test conditions and a negate bit.  It
    // should come as no suprise to find out that all the skip logic can
    // implemented fairly trivially with just a few gates.
    if (ir[`G2_NEGATE] ^ (  (ir[`G2_SMA] &  ac[0])
                          | (ir[`G2_SZA] & (ac==12'o0000))
                          | (ir[`G2_SNL] &  link))) next_pc = pc + 1'b1;

    // Handle the CLA and OSR bits and update next_ac if necessary ...
    case ({ir[`Gx_CLA], ir[`G2_OSR]})
      2'b00: next_ac = ac;              // nop
      2'b01: next_ac = ac | swin;       // OSR
      2'b10: next_ac = 12'o0000;        // CLA
      2'b11: next_ac = swin;    // CLA OSR
    endcase // case ({ir[`Gx_CLA], ir[`G2_OSR]})

    // And lastly, handle the HLT bit ...
    if (ir[`G2_HLT]) begin
      // The HLT instruction sets the hltflg, which traps to panel memory...
      next_hltflg = 1;
`ifdef DIAGNOSTICS
      halted = 1;
`endif
    end
  end
  endtask // OperateG2


  task OperateG3;
  //++
  //   This task implements all the logic needed to execute a group 3 OPR
  // instruction - CLA, MQA and MQL plus any combinations of those.  It decodes
  // the bits in the IR directly and sets the next_ac and next_mq values as
  // needed.  Pretty simple...
  //--
  begin
    case ({ir[`Gx_CLA], ir[`G3_MQA], ir[`G3_MQL]})
      4'b001:  {next_mq, next_ac} = {ac, 12'o0000};             // MQL
      4'b010:  {next_mq, next_ac} = {mq, ac | mq};              // MQA
      4'b011:  {next_mq, next_ac} = {ac, mq};                   // SWP
      4'b100:  {next_mq, next_ac} = {mq, 12'o0000};             // CLA
      4'b101:  {next_mq, next_ac} = {12'o0000, 12'o0000};       // CAM
      4'b110:  {next_mq, next_ac} = {mq, mq};                   // ACL
      4'b111:  {next_mq, next_ac} = {12'o0000, mq};             // CLA SWP
      default: {next_mq, next_ac} = {mq, ac};                   // nop
    endcase
  end
  endtask // OperateG3


  task PostJump;
  //++
  //   This task should be enabled any time the PC is changed by a JMP, JMS,
  // RTN1 or RTN2 instruction. It takes care of all the side effects associated
  // with these instructions including transferring the IB to the IF after a CIF
  // instruction, or clearing the ctrl_ff after a PEX.
  //--
  begin
    //   Do the traditional PDP-8 IB -> IF and clear interrupt inhibit ...
    // Note that in the HD6120 this also clears the FZ flag.  Strictly speaking
    // FZ is only active in console mode, but it does no harm to clear FZ in
    // main memory mode too.
    if (ii_ff)   begin  next_ifr = ibr;  next_ii = 0;  next_fz = 0;  end
    // If a PEX was executed, then exit panel mode ...
    if (pex_ff)  begin  next_ctrl = 0;  next_pex = 0;  end
  end
  endtask // PostJump


  task ANDTAD;
  //++
  //   This task implements the AND and TAD instructions (we figure out which
  // by looking at the LSB of the IR).  The operand is expected to be on the DX
  // bus and the result is left in next_ac (and possibly next_link in the case
  // of TAD)...
  //--
  reg [`WORD] sum;  reg carry;
  begin
    if (ir[2]) begin
      {carry, sum} = ac + dx;
      next_ac = sum;
      if (carry) next_link = ~link;
    end else begin
      next_ac = ac & dx;
    end
  end
  endtask // ANDTAD


  task InterruptControlIOT;
  //++
  //   This task executes all opcodes of the form 600x.  These are traditionally
  // executed internally by the PDP-8 CPU to control the interrupt system, but
  // over time they have been expanded to include a few non-interrupt functions
  // (e.g. SGT).  On the HD6120 there's a further complication in that many of
  // these behave differently depending on whether they're executed from main
  // memory or panel memory.
  //--
  begin
    casex ({ctrl_ff, ir})

      // PRS (Panel Read Status) - load the various HD6120 panel status flags
      // (e.g. btstrp, pnltrp, pwron, etc) into the AC.  After that most, but
      // not all, of the panel flags are cleared.
      {1'b1, `IO_PRS}: begin
        next_ac = {btstrp_ff, pnltrp_ff, intreq, pwron_ff, hltflg_ff, 7'b0};
        next_pnltrp = 0;  next_pwron = 0;  next_btstrp = 0;
      end

      // SKON - skip if interrupts are enabled.  Note that this also disables
      // interrupts unconditionally!
      {1'b0, `IO_SKON}: begin
        if (ie_ff) next_pc = pc + 1'b1;
        next_ie = 0;
      end

      // ION - turn interrupts on.  The ffetch flag forces the next instruction
      // to be executed, even if intreq is currently asserted.
      {1'bx, `IO_ION}: begin  next_ie = 1;  ffetch = 1;  end

      // IOF - turn interrupts off.
      {1'bx, `IO_IOF}: next_ie = 0;

      // PGO - clear the hltflg.  This is used after a panel trap caused by
      // a HLT instruction executed in main memory...
      {1'b1, `IO_PGO}: next_hltflg = 0;

      // SRQ - skip on interrupt request.  Note that this only samples the state
      // of the intreq input - it doesn't test whether interrupts are enabled.
      {1'b0, `IO_SRQ}: if (intreq) next_pc = pc + 1'b1;

      // PEX - exit from panel memory on the next JMP/JMS/RTN instruction.
      // Note that this also clears the pnltrp and pwron flags ...
      {1'b1, `IO_PEX}: begin
        next_pex = 1;  next_pnltrp = 0;  next_pwron = 0;
      end

      // GTF - get flags.  Various interrupt system and other internal status
      // flags are loaded into the AC.
      {1'b0, `IO_GTF}:
        //   There are some minor differences between the bits this instruction
        // returns (or, at least, what it's documented to return) on the HD6120
        // vs what it returns on a real 8/E.  The problem is with bit 3 - on
        // the 6120 it returns the state of the pwron flag.  On the 8/E it's
        // documented to return the interrupt inhibit flag, but in reality it
        // appears to always return zero on any 8/E.  At least one diagnostic
        // program, D1HA, expects this bit to always be zero.  Take your pick.
        next_ac = {link, gt_ff, intreq,         // Bits 0, 1, and 2
`ifdef DIAGNOSTICS
                   1'b0,                        // or ii_ff - you choose
`else
                   pwron_ff,                    // HD6120 version
`endif
                   ie_ff, 1'b0, isf,dsf};       // and bits 4, 5, and 6..11

      // RTF - restore (or return) flags.  The opposite of GTF; this loads
      // various internal flags from the corresponding bits in the AC ...
      {1'bx, `IO_RTF}: begin
        next_link = ac[0];  next_gt = ac[1];
        //   Note that this instruction always sets the ii_ff, as would a CIF,
        // because the instruction field has been changed...
        next_ibr = ac[6:8];  next_dfr = ac[9:11];  next_ii = 1;
        //   The HD6120 datasheet says that RTF loads IE from bit 4 of the AC,
        // however the 8/E manual says RTF enables interrupts unconditionally.
`ifdef DIAGNOSTICS
        // PDP-8/E behavior ...
        next_ie = 1;  ffetch = 1;
`else
        // HD6120 version (or my interpretation there of)...    
        next_ie = ac[4];
        if (ac[4]) ffetch = 1;
`endif
        // No matter what, RTF clears the AC ...
        next_ac = 12'o0000;
      end // case: {1'bx, `IO_RTF}

      // SGT - skip on greater than flag.  On the HD6120 the GT flag is entirely
      // software driven - the only thing that can affect it is RTF...
      {1'bx, `IO_SGT}: if (gt_ff) next_pc = pc + 1'b1;

      // CAF - clear all (I/O) flags, and also the AC, LINK and GT flag.  And
      // it's not well documented, but CAF also disables interrupts too...
      {1'bx, `IO_CAF}: begin
        next_ac=12'o0000;  next_link=0;  next_gt=0;  ioclr=1;  next_ie = 0;
      end

    endcase // casex ({ctrl_ff, ir})
  end
  endtask // InterruptControlIOT


  task ExtendedMemoryIOT;
  //++
  //   This task implements all opcodes of the form 62xn, where n=1, 2, 3 or 4.
  // These are the traditional PDP-8 extended memory control IOTs (e.g. CIF,
  // CDF, RDF, RIF, etc).  AFAIK, the HD6120 implementation of thse is
  // completely PDP-8 compatible...
  //--
  begin
    casex (ir)
      // CDF - change data field ...
      `IO_CDF: next_dfr = ir[6:8];

      // CIF - change instruction field ...
      `IO_CIF: begin  next_ibr = ir[6:8];  next_ii = 1;  end

      // CXF - change both ...
      `IO_CXF: begin
        next_ibr = ir[6:8];  next_dfr = ir[6:8];  next_ii = 1;
      end

      // RDF - read data field.
      // RIF - read instruction field.
      // RIB - read instruction buffer (isf and dsf).
      // The HD6120 datasheet is a bit ambivalent about whether this is an OR
      // transfer or a jam transfer, but the PDP-8 tradition is an OR transfer
      // and the D1HA diagnostic expects this behaviour...
      `IO_RDF: next_ac = ac | {6'b0, dfr, 3'b0};
      `IO_RIF: next_ac = ac | {6'b0, ifr, 3'b0};
      `IO_RIB: next_ac = ac | {6'b0, isf, dsf};

      // RMF - restore memory fields from isf and dsf ...
      `IO_RMF: begin
        next_ibr = isf;  next_dfr = dsf;  next_ii = 1;
      end

      // For reference, these are the KM8E Timeshare option instructions...
      // They're not implemented by the HD6120 at all...
      `IO_CINT, `IO_SINT, `IO_CUF, `IO_SUF: ;
    endcase // casex (ir)
  end
  endtask // ExtendedMemoryIOT


  task StackIOT;
  //++
  //   This task implements the HD6120 stack instructions, which include all
  // opcodes of the form 62x5 (where x=0..7) OR all opcodes of the form 62x7
  // (where x=0..3).  They're kind of a pain, since they have their own rules
  // about handling the EMA and cpmem flags - notice the use of the SWRR
  // and SRDR macros instead of the usual MWRR and MRDR...
  //--
  begin
    casex (ir)
      // LSP1 - load SP1 from the AC, and clear the AC ...
      `IO_LSP1: begin  next_sp1 = ac;  next_ac = 12'o0000;                  end
      // RSP1 - load the AC from SP1.  SP1 is unchanged.
      `IO_RSP1: begin  next_ac = sp1;                                       end
      // PAC1 - push the AC to stack 1 and then decrement SP1.
      `IO_PAC1: begin  `SWRR(sp1, ac)         next_sp1 = sp1 - 1'b1;        end
      // PPC1 - push the PC+1 to stack 1 and then decrement SP1.
      `IO_PPC1: begin  `SWRR(sp1, pc + 1'b1)  next_sp1 = sp1 - 1'b1;        end
      // POP1 - increment SP1 and then pop the AC from the stack.
      `IO_POP1: begin  `SRDR(sp1 + 1'b1, next_ac)  next_sp1 = sp1 + 1'b1;   end
      // RTN1 - increment SP1 and then pop the PC from the stack.
      `IO_RTN1: begin  `SRDR(sp1 + 1'b1, next_pc)  next_sp1 = sp1 + 1'b1;  PostJump;  end

      // These instructions are exactly the same, but for SP2 ...
      `IO_LSP2: begin  next_sp2 = ac;  next_ac = 12'o0000;                  end
      `IO_RSP2: begin  next_ac = sp2;                                       end
      `IO_PAC2: begin  `SWRR(sp2, ac)              next_sp2 = sp2 - 1'b1;   end
      `IO_PPC2: begin  `SWRR(sp2, pc + 1'b1)       next_sp2 = sp2 - 1'b1;   end
      `IO_POP2: begin  `SRDR(sp2 + 1'b1, next_ac)  next_sp2 = sp2 + 1'b1;   end
      `IO_RTN2: begin  `SRDR(sp2 + 1'b1, next_pc)  next_sp2 = sp2 + 1'b1;  PostJump;  end
    endcase // casex (ir)
  end
  endtask // StackIOT


  task HD6120IOT;
  //++
  //   This task executes all opcodes of the form 62x6, which are special
  // HD6120 internal instructions.  
  //--
  begin
    casex (ir)
      // PR0..PR3 - set the pnltrp_ff and cause a panel trap.  Notice that
      // pnltrp is only set if the PRx instruction is executed from main memory.
      // Executing these instructions from panel memory has no effect.
      `IO_PR0, `IO_PR1, `IO_PR2, `IO_PR3: if (~ctrl_ff) next_pnltrp = 1;

      // WSR - write switch register, and clear the AC.
      `IO_WSR: begin  next_swout = ac;  next_ac = 12'o0000;  end

      // GCF - get current flags.  This is a slight variation on GTF and I'm
      // frankly not sure what the HD6120 designers had in mind, but here it
      // is anyway...
      `IO_GCF:
        next_ac = {link, gt_ff, intreq, pwron_ff, ie_ff, 1'b0, ifr, dfr};

      // CPD - clear panel data flag
      // SPD - set panel data flag
      `IO_CPD: next_pdf = 0;
      `IO_SPD: next_pdf = 1;
    endcase // casex (ir)
  end
  endtask // HD6120IOT

  //////////////////////////////////////////////////////////////////////////////
  /////////////   S T A T E   C O D E   A N D   R E G I S T E R S   ////////////
  //////////////////////////////////////////////////////////////////////////////

  //   Define the initial CPU state after FPGA configuration.  Setting the state
  // is enough - the S_RESET state will initialize all other registers ...
  initial
    state = S_RESET;
    
  // This always block synthesizes all the internal flip flops...  
  always @(posedge clock) begin
    //   Thie iowait input, if asserted, prevents our internal state from being
    // updated at the end of the clock cycle.  This allows the current state to
    // persist, unchanged, into the next clock cycle, and the next and the next,
    // until iowait is removed. It's used to freeze the CPU for slow peripherals
    // (like the CompactFlash card) that need to stretch I/O operations.
    if (!iowait) begin
      //   Just update every single register and state bit with its new value on
      // every clock.  Keep in mind that most of these won't actually change -
      // the "next_xyz" state will be the same as the current state.
      ir  <= next_ir;   pc  <= next_pc;   sp1   <= next_sp1;
      mq  <= next_mq;   ac  <= next_ac;   sp2   <= next_sp2;
      ma  <= next_ma;   mb  <= next_mb;   swout <= next_swout;
      dfr <= next_dfr;  ifr <= next_ifr;  ibr   <= next_ibr;
      dsf <= next_dsf;  isf <= next_isf;
      dataf     <= next_dataf;      
      link      <= next_link;    gt_ff     <= next_gt;
      ie_ff     <= next_ie;      ii_ff     <= next_ii;
      ctrl_ff   <= next_ctrl;    pex_ff    <= next_pex;
      pdf_ff    <= next_pdf;     fz_ff     <= next_fz;
      pnltrp_ff <= next_pnltrp;  pwron_ff  <= next_pwron;
      hltflg_ff <= next_hltflg;  btstrp_ff <= next_btstrp;
      //   The last_cpreq FF tracks (what else?) the last state of the cpreq
      // input.  It's used to implement an edge detector so we can set the
      // btstrp_ff on the rising edge of cpreq...
      last_cpreq <= cpreq;
      //   Lastly, notice that the reset input jams the next state with
      // S_RESET, regardless of anything we might be doing at the moment...
      state <= reset ? S_RESET : next_state;
    end
  end
  
  //////////////////////////////////////////////////////////////////////////////
  /////////////////   H D 6 1 2 0   C O N T R O L   L O G I C   ////////////////
  //////////////////////////////////////////////////////////////////////////////

  //   This giant always block (and it is _enormous_ !!)  really does only two
  // things - 1) it uses the current state and various other inputs like the
  // current instruction, status flags, etc, to figure out the next state.
  // And 2) it generates various control outputs such as mwr, mrd, iowr, intgnt,
  // etc, at the right times and under the right conditions.  Despite its size,
  // this block is entirely combinatorial logic.  THERE ARE NO FLIP FLOPS
  // SYNTHESIZED HERE!
  
  always @* begin
    //   In any given clock cycle most registers won't change and it'd be a huge
    // pain if we actually had to assign a value to each next_xyz variable for
    // every possible case, but fortunately we don't.  We can just assign them
    // all here to be the same as the current value, and then later on we only
    // have to modify the ones that actually change in this clock period.
    next_ir  = ir;   next_pc  = pc;   next_sp1 = sp1;
    next_mq  = mq;   next_ac  = ac;   next_sp2 = sp2;
    next_ma  = ma;   next_mb  = mb;
    next_dfr = dfr;  next_ifr = ifr;  next_ibr = ibr;
    next_dsf = dsf;  next_isf = isf;
    next_link   = link;         next_gt    = gt_ff;
    next_ie     = ie_ff;        next_ii    = ii_ff;
    next_ctrl   = ctrl_ff;      next_pex   = pex_ff;
    next_fz     = fz_ff;        next_pdf   = pdf_ff;
    next_pnltrp = pnltrp_ff;    next_pwron = pwron_ff;
    next_hltflg = hltflg_ff;    next_swout = swout;   
    next_state  = state;

    //   The btstrp flag is set by a rising edge on the cpreq input; otherwise
    // it retains its current value (unless, that is, it's cleared by PRS).
    next_btstrp = (cpreq & !last_cpreq) ? 1'b1 : btstrp_ff;

    //   dataf is a bit odd - you'd expect it to be a combinatorial output
    // decoded from the current state, like ifetch or cpmem.  But because of
    // the way it's used, it's actually more convenient to have dataf delayed
    // by one clock cycle.  Hence it's a clocked FF like the link or ie flags,
    // but unlike them it's always cleared at the end of every state.
    next_dataf = 0;

    //   And for pretty much the same reason we assign default values to all the
    // control signals.  This ensures that anything that got turned on in the
    // last state (e.g. mrd = 1) will get turned off in the next state.  Only
    // the control signals that are explicitly assigned later will be asserted.
    ioclr = 0;  iord = 0;  iowr  = 0;  intgnt = 0;  ffetch = 0;
    mrd   = 0;  mwr  = 0;  cpmem = 0;  ifetch = 0;
    ax = 12'o0000;  ema = 3'o0;  dx_out = 12'ozzzz;  write = 0;
`ifdef DIAGNOSTICS
    halted = 0;
`endif


    // OK!  Here we go - decode the current state and figure out what to do...
    case (state)
      
      //   This is the first state entered when the CPU starts up after a reset.
      // It forces all the registers to known values regardless of anything
      // that might be going on right now.  Notice that the value of the PC
      // depends on the STARTUP_PC parameter (for HD6120 emulation, it's 7777).
      // Note that this state also asserts IOCLR for one cycle.
      S_RESET: begin
        next_ir   = 12'o0000;  next_pc   = STARTUP_PC;  next_sp1   = 12'o0000;
        next_mq   = 12'o0000;  next_ac   = 12'o0000;    next_sp2   = 12'o0000;
        next_ma   = 12'o0000;  next_mb   = 12'o0000;    next_swout = 12'o0000;
        next_dfr  = 3'o0;      next_ifr  = 3'o0;        next_ibr   = 3'o0;
        next_dsf  = 3'o0;      next_isf  = 3'o0;
        next_dataf  = 0;
        next_link   = 0;       next_gt   = 0;
        next_ie     = 0;       next_ii   = 0;
        next_ctrl   = 0;       next_pex  = 0;
        next_pdf    = 0;       next_fz   = 0;
        next_pnltrp = 0;       next_pwron = STARTUP_MODE;
        next_hltflg = 0;       next_btstrp = 0;
        ioclr = 1;  ExecuteDone;
      end

      //   The PNLTRP state causes a trap to panel memory.  It's a lot like
      // INTGNT except that a) we have to force the PC to be saved in panel
      // memory location 0, not main memory, and b) we initialize some of the
      // panel mode flags (e.g. fz, pdf) upon entry.
      S_PNLTRP: begin
        next_ctrl = 1;  next_pc = 12'o7777;  next_pdf = 0;  next_fz = 1;
        //  Note that we can't use MWRR here because the ctrl_ff and fz bits
        // won't be updated until the next clock. We'll just have to do it the
        // hard way.
        ax = 12'o0000;  ema = 3'o0;  dx_out = pc;
        cpmem = 1;  write = 1;  mwr = 1;
        //   And likewise we can't use ExecuteDone here for the same reason -
        // since the flags haven't been updated yet, ExecuteDone would just
        // invoke this same PNLTRP state all over again!
        next_state = S_FETCH;
      end // case: S_PNLTRP

      //   And the INTGNT state causes a traditional PDP-8 style interrupt,
      // which is similar to a PNLTRP but not the same.  In this case the
      // current PC is stored in location 0 of main memory and the new PC is
      // set to 0001.  The current IF and DF are stored in the ISF and DSF
      // registers and the IF, IB and DF are cleared.  And finally the IE
      // bit is cleared to prevent any further interrupts until they're
      // re-enabled by the software.
      S_INTGNT: begin
        ax = 12'o0000;  ema = 3'o0;  dx_out = pc;
        cpmem = 0;  write = 1;  mwr = 1;
        intgnt = 1;  next_ie = 0;  next_pc = 12'o0001;
        next_isf = ifr;   next_dsf = dfr;
        next_ifr = 3'o0;  next_ibr = 3'o0;  next_dfr = 3'o0;
        next_state = S_FETCH;
      end // case: S_INTGNT

      //  The FETCH state basically reads the next opcode from memory and stores
      // it in the IR.  However, so that we can know what to do next, we start
      // decoding the instruction directly from the dx bus before it even makes
      // it to the IR.  And some instructions, like JMP direct, are executed
      // entirely in this state and never go any farther.
      S_FETCH: begin
        ifetch = 1;  next_pc = pc + 1'b1;  `MRDR(pc,next_ir)
        // Decode the instruction and figure out what to do next ...
        if (`isJMP(dx) & `isDA(dx)) begin
          //   For a JMP direct instruction, we can just load the PC directly
          // from the bus and go back to FETCH.  It's that easy!
          next_pc = `EA(pc, dx);  PostJump;  ExecuteDone;
        end else if (`isMRI(dx)) begin
          //   For any MRI, the MA is loaded with the EA.  Notice that the EA
          // must be calculated using the current (i.e. pre-increment) PC!
          next_ma = `EA(pc, dx);
          // Indirect MRIs require a defer state; anything else we can execute.
          if (`isIA(dx))  next_state = S_DEFER;  else ExecuteNext(dx);
        end else
          // And anything else (OPR, IOT) we can execute now...
          ExecuteNext(dx);
      end // case: S_FETCH

      //   During DEFER we do another memory read to fetch C(EA) and get the
      // actual operand address.  If it's not auto indexed, then we can just
      // load the operand address into the MA and proceed to execute.  Auto
      // indexed is harder - for AI during DEFER we load the MB with the memory
      // data + 1 (hence incrementing tha operand address) and leave the MA
      // unchanged.  Then we goto an INDEX state that writes the updated data
      // back to memory and transfers the operand address to the MA.
      //
      //   One subtle point is that the indirect address word (i.e. C(EA)) is
      // in the instruction field.  The data field is not used until the actual
      // operand is fetched during the MRI execute cycle.  That's why the dataf
      // output is registered - it won't actually be set until the next clock
      // cycle, which is just what we want...
      //
      //   One more subtle point is that in the case of JMS the dataf should NOT
      // be set - JMS writes the PC to the instruction field, not the data. 
      // JMS is not like a DCA in that regard...
      //
      //   And one last complication - if the instruction is JMP indirect (but
      // not AI) then we can just load the PC with C(EA) and skip loading the
      // MA completely.  And in that case we're ready for another fetch cycle.
      S_DEFER: begin
        if (!`isAI(ma)) begin
          if (`isJMP(ir)) begin
            // JMP indirect, not auto indexed ...
            `MRDR(ma, next_pc)  PostJump;  ExecuteDone;
          end else begin
            // Any other indirect MRI, not auto index ...
            `MRDR(ma, next_ma)
            if (!`isJMS(ir))  next_dataf = 1;
            ExecuteNext(ir);
          end
        end else begin
          // Any auto indexed MRI, including JMP ...
          `MRD(ma)  next_mb = dx + 1'b1;  next_state = S_INDEX;
        end // else: !if(!`isAI(ma))
      end // case: S_DEFER

      //   Now the MA still contains the EA (the address of the pointer word)
      // and MB contains C(EA)+1 (the address actual data we eventually want to
      // use).  We need to write the MB back to memory, and then copy the MB to
      // the MA for MRI to use when it fetches its operand.
      S_INDEX: begin
        `MWRR(ma, mb)  next_ma = mb;
        if (`isJMP(ir)) begin
          next_pc = mb;  PostJump;  ExecuteDone;
        end else begin
          if (!`isJMS(ir))  next_dataf = 1;
          ExecuteNext(ir);
        end
      end // case: S_INDEX

      //   This common execute state is shared by both AND and TAD.  The address
      // of the operand is in the MA register, so all we have to do is a memory
      // read to fetch the operand and we can calculate the result...
      S_ANDTAD: begin
        `MRD(ma)  ANDTAD;  ExecuteDone;
      end
      
      // DCA is easy - just write the AC to memory and clear it ...
      S_DCA: begin
        `MWRR(ma, ac)  next_ac = 12'o0000;  ExecuteDone;
      end
      
      // JMS is almost as easy as DCA ...
      S_JMS: begin
        //   Sadly, this is yet another special case.  If a CIF was recently
        // executed, then a JMS saves the return address to the NEW field, not
        // the current one!  In a real PDP-8 this is accomplished by doing the
        // IB -> IF transfer before the return address gets written to memory.
        // Since we do everything in one cycle we don't have the luxury of
        // doing it that way, so OutputEMA() has a special case to use the IB
        // as the field rather than IF.  That's all fine, but it also means we
        // can't use the MWRR macro here - we have to spell it out...
        dx_out = pc;  OutputEMA(1);  ax = ma;  write = 1;  mwr = 1;  
        next_pc = ma + 1'b1;  PostJump;  ExecuteDone;
      end

      //   ISZ takes two execute states - in the first we read the operand and
      // load <memory data> + 1 into the MB.  In the second state we write the
      // MB back to the same memory location and, if the MB is zero, also
      // increment the PC.
      //
      //   One tiny wrinkle here - if the ISZ is indirectly addressed, then
      // dataf needs to be asserted for BOTH states.  Since next_dataf is
      // automatically cleared after each state, we have to explicitly
      // propagate it thru to ISZ_1.
      S_ISZ_0: begin
        `MRD(ma)  next_mb = dx + 1'b1;
        next_dataf = dataf;  next_state = S_ISZ_1;
      end
      S_ISZ_1: begin
        if (mb==12'o0000)  next_pc = pc + 1'b1;
        `MWRR(ma, mb)  ExecuteDone;
      end

      //   IOT instructions execute in either one or two clocks.  During the
      // first clock, we send the AC out on the DX bus and the IOT opcode on
      // the AX bus and assert iowr as a strobe.  Any device that's interested
      // can assert ioc0, ioc1 and/or ioskip during the time iowr is active.
      // Asserting ioskip causes the PC to be incremented, and asserting ioc0
      // causes the AC to be cleared, both at the end of state IOT_0.
      //
      //   If ioc1 is asserted during IOT_0 then we'll enter state IOT_1 and
      // assert iord.  At the end of IOT_1 the contents of the DX bus are OR
      // transferred to the AC; this allows data to be read from a device.
      //
      //   Lastly, there are many "internal" IOTs which are executed directly
      // by the HD6120.  These include all opcodes of the forms 600x or 62xx.
      // None of these IOTs assert iowr; none sample ioc0, ioc1 or ioskip, and
      // all of them execute in one state even if they OR data into the AC.
      S_IOT_0: begin
        if ((ir[0:8]==9'o600) | (ir[0:5]==6'o62)) begin
               if (`isPIOT(ir))  InterruptControlIOT;
          else if (`isEMA(ir))   ExtendedMemoryIOT;
          else if (`isStack(ir)) StackIOT;
          else if (`is6120(ir))  HD6120IOT;
          else /* unimplemented internal IOT!!! */;
          ExecuteDone;
        end else begin
          dx_out = ac;  ax = ir;  write = 1;  iowr = 1;
          if (ioskip)  next_pc = pc + 1'b1;
          if (ioc0)    next_ac = 12'o0000;
          if (ioc1)  next_state = S_IOT_1;  else ExecuteDone;
        end
      end // case: S_IOT_0
      
      S_IOT_1: begin
        ax = ir;  iord = 1;  next_ac = ac | dx;  ExecuteDone;
      end

      //   This state is for the execute phase of the OPR instructions.  It's
      // the only execute phase that _doesn't_ reference memory and, if I worked
      // at it a little bit, OPR instructions could probably be executed during
      // the FETCH phase in much the same way that JMPs are handled.  Ah well -
      // something to do for the future...
      S_OPR: begin
             if (`isOPR1(ir)) OperateG1;
        else if (`isOPR2(ir)) OperateG2;
        else if (`isOPR3(ir)) OperateG3;
        ExecuteDone;
      end // case: S_OPR

      // If we ever get here, then punt!
      default: begin
        next_state = S_RESET;
      end
    endcase // case (state)

  end  // always @* from way, way, back

  //   Good practice dictates that we should limit the scope of all the local
  //  macros we defined by undefining them now...
`undef NEXT
`undef MRD
`undef MWR
`undef MRDR
`undef MWRR
`undef SRDR
`undef SWDR
endmodule
