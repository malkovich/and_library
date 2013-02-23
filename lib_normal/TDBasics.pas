(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDBasics                                                         *)
(* Standard types and routines throughout library                   *)
(********************************************************************)

unit TDBasics;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  Classes;

{---String table resource constants---}
{$I TDStrRes.inc}

{---Compatibility types for Delphi 1---}


type
  TtdCompareFunc = function (aData1, aData2 : pointer) : integer;
  TtdDisposeProc = procedure (aData : pointer);
  TtdUpcaseFunc = function(aCh : AnsiChar) : AnsiChar;
  TtdVisitProc = procedure (aData       : pointer;
                            aExtraData  : pointer;
                        var aStopVisits : boolean);

type
  TtdNameString = string[31];
  PtdCharSet = ^TtdCharSet;
  TtdCharSet = set of AnsiChar;

{---Exceptions for library---}
type
  EtdException = class(Exception);
  EtdAssertion = class(EtdException);
  EtdStreamException = class(EtdException);
  EtdTListException = class(EtdException);
  EtdArrayException = class(EtdException);
  EtdNodeMgrException = class(EtdException);
  EtdLinkListException = class(EtdException);
  EtdStackException = class(EtdException);
  EtdQueueException = class(EtdException);
  EtdDequeException = class(EtdException);
  EtdHashTableException = class(EtdException);
  EtdSkipListException = class(EtdException);
  EtdBinTreeException = class(EtdException);
  EtdRandGenException = class(EtdException);
  EtdLZException = class(EtdException);
  EtdHuffmanException = class(EtdException);
  EtdSplayException = class(EtdException);
  EtdStateException = class(EtdException);
  EtdRegexException = class(EtdException);


{---Example compare routines---}
function TDCompareLongint(aData1, aData2 : pointer) : integer;
function TDCompareNullStr(aData1, aData2 : pointer) : integer;
function TDCompareNullStrText(aData1, aData2 : pointer) : integer;

{---Example dispose routines---}
procedure TDDisposeObject(aData : pointer);

{---Character/string routines---}
function TDPosCh(aCh : AnsiChar; const S : string) : integer;
function TDChExists(aCh : AnsiChar; const S : string) : boolean;
function TDIsDigit(aCh : AnsiChar) : boolean;


implementation



const
  UnitName = 'TDBasics';

{===Example compare routines=========================================}
function TDCompareLongint(aData1, aData2 : pointer) : integer;
var
  L1 : longint absolute aData1;
  L2 : longint absolute aData2;
begin
  if (L1 < L2) then
    Result := -1
  else if (L1 = L2) then
    Result := 0
  else
    Result := 1
end;
{--------}
function TDCompareNullStr(aData1, aData2 : pointer) : integer;
begin
  Result := StrComp(PAnsiChar(aData1), PAnsiChar(aData2));
end;
{--------}
function TDCompareNullStrText(aData1, aData2 : pointer) : integer;
begin
  Result := StrIComp(PAnsiChar(aData1), PAnsiChar(aData2));
end;


{===Example dispose routines=========================================}
procedure TDDisposeObject(aData : pointer);
begin
  TObject(aData).Free;
end;
{====================================================================}


{===Character/string routines========================================}
function TDChExists(aCh : AnsiChar; const S : string) : boolean;
var
  i : integer;
begin
  Result := true;
  for i := 1 to length(S) do
    if (S[i] = aCh) then
      Exit;
  Result := false;
end;
{--------}
function TDIsDigit(aCh : AnsiChar) : boolean;
begin
  Result := ('0' <= aCh) and (aCh <= '9');
end;
{--------}
function TDPosCh(aCh : ANsiChar; const S : string) : integer;
var
  i : integer;
begin
  Result := 0;
  for i := 1 to length(S) do
    if (S[i] = aCh) then begin
      Result := i;
      Exit;
    end;
end;
{====================================================================}




end.
