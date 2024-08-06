//++
//clog2.v - replacement for $clog2() Verilog-2005 function
//Copyright (C) 2011 by Spare Time Gizmos.  All rights reserved.
//
// DESCRIPTION:
//   The lame Xilinx XST tool doesn't know about $clog2() (it's a Verilog-2005
// construction, after all), and hence we need this.  This file is not a module
// and it's not intended to stand alone - you'll have to `include it in every
// module where it's needed.  It's kind of lame, but Verilog also lacks any
// idea of global functions and there's really no other way...
//
//   BTW, XST has a bug such that it can't use constant functions like this one
// anywhere except the right hand side of parameter assignments. So, you need
// to have a parameter declared as the result of this function and then use the
// parameter in your port declarations.
//
// For example,
//
//	reg [clog2(MAX)-1:0] count;
//
// doesn't work.  Instead,
//
//	localparam LOG2_MAX = clog2(MAX);
//	reg [LOG2_MAX-1:0] count;
//
// does work...
//
// REVISION HISTORY:
// 13-Feb-11  RLA  New file.
//--
//000000001111111111222222222233333333334444444444555555555566666666667777777777
//234567890123456789012345678901234567890123456789012345678901234567890123456789

function integer clog2 (input integer value);
integer lvalue;
begin
  lvalue = value-1;
  for (clog2 = 0;  lvalue != 0;  clog2 = clog2+1)
    lvalue = lvalue>>1;
end
endfunction // clog2
