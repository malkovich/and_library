{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  A place for common utilities.
Creation:     Apr 25, 2008
Version:      1.12
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2002-2008 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be> 

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to Francois PIETTE. Use a nice stamp and mention your name,
                 street address, EMail address and any comment you like to say.

History:
Apr 25, 2008 V1.00 AGarrels added first functions UnicodeToAscii, UnicodeToAnsi,
             AnsiToUnicode and IsUsAscii .
May 01, 2008 V1.01 AGarrels added StreamWriteString.
May 02, 2008 V1.02 AGarrels a few optimizations and a bugfix in StreamWriteString.
May 11, 2008 V1.03 USchuster added atoi implementations (moved from several units)
May 15, 2008 V1.04 AGarrels fix in IcsAppendStr made StreamWriteString a function.
May 19, 2008 V1.05 AGarrels added BOM-support to StreamWriteString plus two
             overloads. Made UnicodeString a type alias of WideString in compiler
             versions < COMPILER12 in order to enable use of some conversion
             routines for older compilers as well.
May 19, 2008 V1.06 Don't check actual string codepage but assume UTF-16 Le
             in function StreamWriteString() (temp fix).
Jul 14, 2008 V1.07 atoi improved, should be around 3 times faster.
Jul 17, 2008 V1.08 Added OverbyteIcsTypes to the uses clause and removed
             SysUtils, removed some defines for unsupported old compilers.
             StreamWriteString should work with WideStrings as well with old
             compilers.
Jul 20, 2008 V1.09 Added Utf-8 string functions.
Jul 29, 2008 V1.10 Added parameter "SetCodePage" to UnicodeToAnsi(), defaults
             to "False". Utf-8 functions adjusted accordingly. Does effect
             compiler post RDS2007 only.
Jun 05, 2008 Utf-8 functions modified to take and return AnsiString rather than
             Utf8String.
Aug 11, 2008 CheckUnicodeToAnsi() added. Changed the DefaultFailChar to "?". 

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsUtils;

interface

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$H+}           { Use long strings                    }
{$J+}           { Allow typed constant to be modified }
{$I OverbyteIcsDefs.inc}
{$IFDEF COMPILER12_UP}
    {$WARN IMPLICIT_STRING_CAST       OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS  OFF}
    {$WARN EXPLICIT_STRING_CAST       OFF}
    {$WARN EXPLICIT_STRING_CAST_LOSS  OFF}
{$ENDIF}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

uses
    Windows,
    Classes,
    OverbyteIcsTypes; // for TBytes

{$IFNDEF COMPILER12_UP}
{ Should probably move to OverbyteIcsTypes.pas ? }
type
    UnicodeString = WideString;
{$ENDIF}

function  UnicodeToUsAscii(const Str: UnicodeString; FailCh: AnsiChar): AnsiString; overload;
function  UnicodeToUsAscii(const Str: UnicodeString): AnsiString;  overload;
function  UsAsciiToUnicode(const Str: AnsiString; FailCh: AnsiChar): UnicodeString; overload;
function  UsAsciiToUnicode(const Str: AnsiString): UnicodeString; overload;
function  UnicodeToAnsi(const Str: UnicodeString; ACodePage: Cardinal; SetCodePage: Boolean = False): AnsiString; overload;
function  UnicodeToAnsi(const Str: UnicodeString): AnsiString; overload;
function  AnsiToUnicode(const Str: AnsiString; ACodePage: Cardinal): UnicodeString; overload;
function  AnsiToUnicode(const Str: AnsiString): UnicodeString; overload;
function  StreamWriteString(AStream: TStream; Str: PWideChar; cLen: Integer; ACodePage: Cardinal; WriteBOM: Boolean): Integer; overload;
function  StreamWriteString(AStream: TStream; Str: PWideChar; cLen: Integer; ACodePage: Cardinal): Integer; overload;
function  StreamWriteString(AStream: TStream; const Str: UnicodeString; ACodePage: Cardinal; WriteBOM: Boolean): Integer; overload;
function  StreamWriteString(AStream: TStream; const Str: UnicodeString; ACodePage: Cardinal): Integer; overload;
function  StreamWriteString(AStream: TStream; const Str: UnicodeString): Integer; overload;
function  IsUsAscii(const Str: AnsiString): Boolean; overload;
function  IsUsAscii(const Str: UnicodeString): Boolean; overload;
procedure IcsAppendStr(var Dest: AnsiString; const Src: AnsiString);
function  atoi(const Str: AnsiString): Integer; overload;
function  atoi(const Str: UnicodeString): Integer; overload;
{$IFDEF STREAM64}
function  atoi64(const Str: AnsiString): Int64; overload;
function  atoi64(const Str: UnicodeString): Int64; overload;
{$ENDIF}
function  StringToUtf8(const Str: UnicodeString): AnsiString; overload;
function  StringToUtf8(const Str: AnsiString; ACodePage: Cardinal = CP_ACP): AnsiString; overload;
function  Utf8ToStringW(const Str: AnsiString): UnicodeString;
function  Utf8ToStringA(const Str: AnsiString; ACodePage: Cardinal = CP_ACP): AnsiString;
function  CheckUnicodeToAnsi(const Str: UnicodeString; ACodePage: Cardinal = CP_ACP): Boolean;

implementation

const
    DefaultFailChar : AnsiChar = '?';
    CP_UTF16Le = 1200;
    CP_UTF16Be = 1201;
    CP_UTF8    = 65001;
    

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsUsAscii(const Str: AnsiString): Boolean;
var
    I : Integer;
begin
    for I := 1 to Length(Str) do
        if Byte(Str[I]) > 127 then begin
            Result := FALSE;
            Exit;
        end;
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsUsAscii(const Str: UnicodeString): Boolean;
var
    I : Integer;
begin
    for I := 1 to Length(Str) do
        if Ord(Str[I]) > 127 then begin
            Result := FALSE;
            Exit;
        end;
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Assumes parameter Str does not contain any 8Bit characters otherwise they   }
{ are replaced by FailCh. When we use plain ASCII payload this could be the   }
{ fastes cast. Sometimes we handle 7 bit strings only.                        }
function UnicodeToUsAscii(const Str: UnicodeString; FailCh: AnsiChar): AnsiString;
var
    I   : Integer;
    Len : Integer;
begin
    Len := Length(Str);
    SetLength(Result, Len);
    for I := 1 to Len do begin
        if Ord(Str[I]) > 127 then
            Result[I] := FailCh
        else
            Result[I] := AnsiChar(Str[I]);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UnicodeToUsAscii(const Str: UnicodeString): AnsiString;
begin
    Result := UnicodeToUsAscii(Str, DefaultFailChar);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Converts an UnicodeString to an AnsiString.                                 }
function UnicodeToAnsi(const Str: UnicodeString; ACodePage: Cardinal; SetCodePage: Boolean = False): AnsiString;
var
    Len : Integer;
begin
    Len := Length(Str);
    if Len > 0 then begin
        Len := WideCharToMultiByte(ACodePage, 0, Pointer(Str), Len, nil, 0, nil, nil);
        SetLength(Result, Len);
        if Len > 0 then begin
            WideCharToMultiByte(ACodePage, 0, Pointer(Str), Length(Str),
                                Pointer(Result), Len, nil, nil);
        {$IFDEF COMPILER12_UP}
            if SetCodePage and (ACodePage <> CP_ACP) then
                PWord(Integer(Result) - 12)^ := ACodePage;
        {$ENDIF}
        end;
    end
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Converts an UnicodeString to an AnsiString using current code page.         }
function UnicodeToAnsi(const Str: UnicodeString): AnsiString;
begin
    Result := UnicodeToAnsi(Str, CP_ACP);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function AnsiToUnicode(const Str: AnsiString; ACodePage: Cardinal): UnicodeString;
var
    Len : Integer;
begin
    Len := Length(Str);
    if Len > 0 then begin
        Len := MultiByteToWideChar(ACodePage, 0, Pointer(Str),
                                   Len, nil, 0);
        SetLength(Result, Len);
        if Len > 0 then
            MultiByteToWideChar(ACodePage, 0, Pointer(Str), Length(Str),
                                Pointer(Result), Len);
    end
    else
        Result := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function AnsiToUnicode(const Str: AnsiString): UnicodeString;
begin
    Result := AnsiToUnicode(Str, CP_ACP);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UsAsciiToUnicode(const Str: AnsiString; FailCh: AnsiChar): UnicodeString;
var
    I  : Integer;
    P  : PByte;
begin
    SetLength(Result, Length(Str));
    P := Pointer(Result);
    for I := 1 to Length(Str) do begin
        if Byte(Str[I]) > 127 then
            P^ := Byte(FailCh)
        else
            P^ := Byte(Str[I]);
        Inc(P);
        P^ := 0;
        Inc(P);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UsAsciiToUnicode(const Str: AnsiString): UnicodeString;
begin
    Result := UsAsciiToUnicode(Str, DefaultFailChar);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure IcsAppendStr(var Dest: AnsiString; const Src: AnsiString);
begin
{$IFDEF COMPILER12_UP}
    SetLength(Dest, Length(Dest) + Length(Src));
    Move(Pointer(Src)^, Dest[Length(Dest) - Length(Src) + 1], Length(Src));
{$ELSE}
    Dest := Dest + Src;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
function GetBomBytes(ACodePage: Word) : TBytes;
begin
    case ACodePage of
        CP_UTF16Le :
        begin
            SetLength(Result, 2);
            Result[0] := $FF;
            Result[1] := $FE;
        end;
        CP_UTF16Be :
        begin
            SetLength(Result, 2);
            Result[0] := $FE;
            Result[1] := $FF;
        end;
        CP_UTF8    :
        begin
            SetLength(Result, 3);
            Result[0] := $EF;
            Result[1] := $BB;
            Result[2] := $BF;
        end;
        else
            SetLength(Result, 0);
    end;
end;
*)


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Assumes that the string is a Windows, UTF-16 little endian wide string      }
function StreamWriteString(AStream: TStream; Str: PWideChar; cLen: Integer;
  ACodePage: Cardinal; WriteBOM: Boolean): Integer;
var
    SBuf  : array [0..2047] of Byte;
    Len   : Integer;
    HBuf  : PAnsiChar;
    Bom   : TBytes;
    CurCP : Word;
    J     : Integer;
    Swap  : Boolean;
    Dump  : Boolean;
begin
    Result := 0;
    if (Str = nil) or (cLen <= 0) then
        Exit;
    CurCP := CP_UTF16Le; //PWord(Integer(Str) - 12)^;
    case ACodePage of
        CP_UTF16Le :
            begin
                if WriteBOM then begin
                    SetLength(BOM, 2);
                    BOM[0] := $FF;
                    BOM[1] := $FE;
                end;
                Swap := CurCP = CP_UTF16Be;
                Dump := (CurCP = ACodePage) or Swap;
            end;
        CP_UTF16Be :
            begin
                if WriteBOM then begin
                    SetLength(BOM, 2);
                    BOM[0] := $FE;
                    BOM[1] := $FF;
                end;
                Swap := CurCP = CP_UTF16Le;
                Dump := (CurCP = ACodePage) or Swap;
            end;
        CP_UTF8 :
            begin
                if WriteBOM then begin
                    SetLength(BOM, 3);
                    BOM[0] := $EF;
                    BOM[1] := $BB;
                    BOM[2] := $BF;
                end;
                Dump := FALSE;
                Swap := FALSE;
            end;
        else
            SetLength(BOM, 0);
            Dump := FALSE;
            Swap := FALSE;
    end; // case

    if Dump and not Swap then
    begin // No conversion needed
        if Bom <> nil then
            AStream.Write(Bom[0], Length(Bom));
        Result := AStream.Write(Pointer(Str)^, cLen * 2); //Use const char length 
    end
    else begin
        if Dump and Swap then
        begin // We need to swap bytes and write them to the stream
            if Bom <> nil then
                AStream.Write(Bom[0], Length(Bom));
            J := -1;
            for Len := 1 to cLen do
            begin
                Inc(J);
                SBuf[J] := Hi(Word(Str^));
                Inc(J);
                SBuf[J] := Lo(Word(Str^));
                Inc(Str);
                if J = 2047 then begin
                    Result := Result + AStream.Write(SBuf[0], Length(SBuf));
                    J := -1;
                end;
            end;
            if J >= 0 then
                Result := Result + AStream.Write(SBuf[0], J + 1);
        end
        else begin // Charset conversion
            Len := WideCharToMultibyte(ACodePage, 0, Pointer(Str), cLen,
                                       nil, 0, nil, nil);
            if Len <= SizeOf(SBuf) then begin
                Len := WideCharToMultibyte(ACodePage, 0, Pointer(Str), cLen,
                                           @SBuf, Len, nil, nil);
                if (Len > 0) then begin
                    if Bom <> nil then
                        AStream.Write(Bom[0], Length(Bom));
                    Result := AStream.Write(SBuf[0], Len);
                end;
            end
            else begin
                GetMem(HBuf, Len);
                try
                    Len := WideCharToMultibyte(ACodePage, 0, Pointer(Str), cLen,
                                               HBuf, Len, nil, nil);
                    if (Len > 0) then begin
                        if Bom <> nil then
                            AStream.Write(Bom[0], Length(Bom));
                        Result := AStream.Write(HBuf^, Len);
                    end;
                finally
                    FreeMem(HBuf);
                end;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StreamWriteString(AStream: TStream; Str: PWideChar; cLen: Integer;
  ACodePage: Cardinal): Integer;
begin
    Result := StreamWriteString(AStream, Str, cLen, ACodePage, FALSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StreamWriteString(AStream: TStream; const Str: UnicodeString;
 ACodePage: Cardinal; WriteBOM: Boolean): Integer;
begin
    Result := StreamWriteString(AStream, Pointer(Str), Length(Str),
                                ACodePage, WriteBom);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StreamWriteString(AStream: TStream; const Str: UnicodeString): Integer;
begin
    Result:= StreamWriteString(AStream, Pointer(Str), Length(Str),
                               CP_ACP, FALSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StreamWriteString(AStream: TStream; const Str: UnicodeString;
  ACodePage: Cardinal): Integer;
begin
    Result:= StreamWriteString(AStream, Pointer(Str), Length(Str),
                               ACodePage, FALSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
function atoi(const Str: AnsiString): Integer;
var
    I : Integer;
begin
    Result := 0;
    I := 1;
    while (I <= Length(Str)) and (Str[I] = ' ') do
        I := I + 1;
    while (I <= Length(Str)) and (Str[I] >= '0') and (Str[I] <= '9') do begin
        Result := Result * 10 + Ord(Str[I]) - Ord('0');
        I := I + 1;
    end;
end;
*)

{ This one is around 3-4 times faster } { AG }
function atoi(const Str : AnsiString): Integer;
var
    P : PAnsiChar;
begin
    Result := 0;
    P := Pointer(Str);
    if P = nil then
        Exit;
    while P^ = #$20 do Inc(P);
    while True do
    begin
        case P^ of
            '0'..'9' : Result := Result * 10 + Byte(P^) - Byte('0');
        else
            Exit;
        end;
        Inc(P);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
function atoi(const Str: UnicodeString): Integer;
var
    I : Integer;
begin
    Result := 0;
    I := 1;
    while (I <= Length(Str)) and (Str[I] = ' ') do
        I := I + 1;
    while (I <= Length(Str)) and (Str[I] >= '0') and (Str[I] <= '9') do begin
        Result := Result * 10 + Ord(Str[I]) - Ord('0');
        I := I + 1;
    end;
end;
*)

{ This one is around 3-4 times faster } { AG }
function atoi(const Str : UnicodeString): Integer;
var
    P : PWideChar;
begin
    Result := 0;
    P := Pointer(Str);
    if P = nil then
        Exit;
    while P^ = #$0020 do Inc(P);
    while True do
    begin
        case P^ of
            '0'..'9' : Result := Result * 10 + Ord(P^) - Ord('0');
        else
            Exit;
        end;
        Inc(P);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF STREAM64}
(*
function atoi64(const Str: AnsiString): Int64;
var
    I : Integer;
begin
    Result := 0;
    I := 1;
    while (I <= Length(Str)) and (Str[I] = ' ') do
        I := I + 1;
    while (I <= Length(Str)) and (Str[I] >= '0') and (Str[I] <= '9') do begin
        Result := Result * 10 + Ord(Str[I]) - Ord('0');
        I := I + 1;
    end;
end;
*)

{ This one is around 3-4 times faster } { AG }
function atoi64(const Str : AnsiString): Int64;
var
    P : PAnsiChar;
begin
    Result := 0;
    P := Pointer(Str);
    if P = nil then
        Exit;
    while P^ = #$20 do Inc(P);
    while True do
    begin
        case P^ of
            '0'..'9' : Result := Result * 10 + Byte(P^) - Byte('0');
        else
            Exit;
        end;
        Inc(P);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
function atoi64(const Str: UnicodeString): Int64;
var
    I : Integer;
begin
    Result := 0;
    I := 1;
    while (I <= Length(Str)) and (Str[I] = ' ') do
        I := I + 1;
    while (I <= Length(Str)) and (Str[I] >= '0') and (Str[I] <= '9') do begin
        Result := Result * 10 + Ord(Str[I]) - Ord('0');
        I := I + 1;
    end;
end;
*)

{ This one is around 3-4 times faster } { AG }
function atoi64(const Str : UnicodeString): Int64;
var
    P : PWideChar;
begin
    Result := 0;
    P := Pointer(Str);
    if P = nil then
        Exit;
    while P^ = #$0020 do Inc(P);
    while True do
    begin
        case P^ of
            '0'..'9' : Result := Result * 10 + Ord(P^) - Ord('0');
        else
            Exit;
        end;
        Inc(P);
    end;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StringToUtf8(const Str: UnicodeString): AnsiString;
begin
    Result := UnicodeToAnsi(Str, CP_UTF8, True);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function StringToUtf8(const Str: AnsiString; ACodePage: Cardinal = CP_ACP): AnsiString;
var
    Temp : UnicodeString;
begin
    Temp := AnsiToUnicode(Str, ACodePage);
    Result := UnicodeToAnsi(Temp, CP_UTF8, True);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Utf8ToStringW(const Str: AnsiString): UnicodeString;
begin
    Result := AnsiToUnicode(Str, CP_UTF8);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Utf8ToStringA(const Str: AnsiString; ACodePage: Cardinal = CP_ACP): AnsiString;
var
    Temp: UnicodeString;
begin
    Temp := AnsiToUnicode(Str, CP_UTF8);
    Result := UnicodeToAnsi(Temp, ACodePage, True);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CheckUnicodeToAnsi(const Str: UnicodeString; ACodePage: Cardinal = CP_ACP): Boolean;
var
    Len : Integer;
    B   : Bool;
begin
    Len := Length(Str);
    if Len > 0 then begin
        Len := WideCharToMultiByte(ACodePage, 0, Pointer(Str), Len, nil, 0, nil, @B);
        Result := (not B) and (Len > 0);
    end
    else
        Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
