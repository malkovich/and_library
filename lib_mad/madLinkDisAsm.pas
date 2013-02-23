// ***************************************************************
//  madLinkDisAsm.pas         version:  1.0   ·  date: 2003-06-09
//  -------------------------------------------------------------
//  just a wrapper to help Delphi's smart linker
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2002 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2003-06-09 1.0  initial version

unit madLinkDisAsm;

interface

implementation

uses madExcept, madDisAsm;

initialization
  // in the madExcept settings dialog you can choose whether you want madExcept
  // to add a disassembly of the exception location to the bug report
  // unfortunately the "disassembler -> clear asm text" function costs ca 16kb
  // normally this would now be linked into every binary which uses madExcept
  // of course we don't like that, so madExcept doesn't touch the 16kb function
  // but in case madExcept really needs it, we do a manual link to it
  // this is exactly what this unit, especially the following line, is doing
  madExcept.DisAsmFunc := madDisAsm.ParseFunctionEx;
  // the IDE wizard links this unit into your project if your settings say that
  // you want to have a disassembly in the bug report
end.