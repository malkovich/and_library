unit EncryptDecryptUnit;

interface
uses
  RC4Extractor, Base64, WinDefine, BuildInRelocFunctions, BuildInFunctions;

type
  LPRC4Valuex = ^TRC4ValueX;
  TRC4ValueX = packed record
    Index: Byte;
    Value: DWORD;
  end;

  LPArrayRC4DWORD = ^TArrayRC4DWORD;
  TArrayRC4DWORD = array[0..65] of DWORD;  
  TRC4Value = array[0..4] of char;

const
  BLOCK_1: array[0..65] of TRC4Value = (
  {$I RC4Fragment.inc}
  );
                   
var
  RC4Context: TRC4Context;
  MsgRC4Context: TRC4Context;
  MARK: TCodeMark;

function InitEncryptModule (CodeMark: TCodeMark): LongBool;
function GetDecryptMSG(var RC4:TRC4Context; Base64Str: PChar; var Size: Integer):PChar;
function GetEncryptMSG(var RC4:TRC4Context; Buffer: Pointer; Size: Integer):PChar;

implementation       

{$I EncryptFunction.inc}

function InitEncryptModule (CodeMark: TCodeMark): LongBool;
var
  I: Integer;
  RC4ValueX: LPRC4ValueX;
  ArrayRC4DWORD: LPArrayRC4DWORD;
begin
  Result := False;
  ArrayRC4DWORD := @RC4Context;
  for I := 0 to 65 do
  begin
    RC4ValueX := @BLOCK_1[I];
    if RC4ValueX.Index > 65 then EXIT;
    ArrayRC4DWORD[RC4ValueX.Index] := RC4ValueX.Value;
  end;
  MsgRC4Context := RC4Context;

  MARK := CodeMark;
  Result := True;
end;



end.
