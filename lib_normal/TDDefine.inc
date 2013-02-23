(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999                *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDDefine.inc                                                     *)
(* Compiler define include file                                     *)
(********************************************************************)

{The following compiler define tells the compiler whether to include
 debug information or not; certain units will also compile extra code
 to be used for debugging with this define set}
{$DEFINE DebugMode}

{===DO NOT CHANGE ANYTHING FROM THIS POINT===========================}

{---Calculate compiler defines based on compiler version-------------}
{$IFDEF Windows}
{$IFDEF VER80}
  {$DEFINE Delphi1}
  {$DEFINE Delphi1Plus}
{$ENDIF}
{$ENDIF}

{$IFDEF Win32}
{$IFDEF VER90}
  {$DEFINE Delphi2}
  {$DEFINE Delphi1Plus}
  {$DEFINE Delphi2Plus}
{$ENDIF}
{$IFDEF VER100}
  {$DEFINE Delphi3}
  {$DEFINE Delphi1Plus}
  {$DEFINE Delphi2Plus}
  {$DEFINE Delphi3Plus}
  {$DEFINE HasAssert}
{$ENDIF}
{$IFDEF VER120}
  {$DEFINE Delphi4}
  {$DEFINE Delphi1Plus}
  {$DEFINE Delphi2Plus}
  {$DEFINE Delphi3Plus}
  {$DEFINE Delphi4Plus}
  {$DEFINE HasAssert}
{$ENDIF}
{$IFDEF VER130}
  {$DEFINE Delphi5}
  {$DEFINE Delphi1Plus}
  {$DEFINE Delphi2Plus}
  {$DEFINE Delphi3Plus}
  {$DEFINE Delphi4Plus}
  {$DEFINE Delphi5Plus}
  {$DEFINE HasAssert}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE Delphi6}
  {$DEFINE Delphi1Plus}
  {$DEFINE Delphi2Plus}
  {$DEFINE Delphi3Plus}
  {$DEFINE Delphi4Plus}
  {$DEFINE Delphi5Plus}
  {$DEFINE Delphi6Plus}
  {$DEFINE HasAssert}
{$ENDIF}
{$ENDIF}

{$IFDEF Linux}
  {$DEFINE Kylix1}
  {$DEFINE Kylix1Plus}
  {$DEFINE HasAssert}
{$ENDIF}


{--Compiler options we MUST have set, others are no-care-------------}
{$B-   Short-circuit boolean expressions }
{$I+   I/O checking via exceptions }
{$V-   Disable var string checking }
{$W-   No Windows realmode stack frame }
{$X+   Enable extended syntax }
{$IFDEF DebugMode}
  {$D+,L+  Enable debug information }
{$ENDIF}
{$IFDEF Delphi1}
  {$G+   80286+ type instructions }
{$ENDIF}
{$IFDEF Delphi2Plus}
  {$WARNINGS ON}
  {$HINTS ON}
{$ENDIF}
{$IFDEF Kylix1Plus}
  {$WARNINGS ON}
  {$HINTS ON}
{$ENDIF}


{---Miscellaneous----------------------------------------------------}
{$IFDEF Delphi1}
  {$C MOVEABLE DEMANDLOAD DISCARDABLE}
{$ENDIF}
