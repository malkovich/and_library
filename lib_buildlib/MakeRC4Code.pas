unit MakeRC4Code;

interface
uses
  windows, SysUtils, Classes, RC4Extractor, Base64unit;

{$I Loader.inc}

function MakeRC4ToCode(FileBase: PChar; FileSize: Integer; const Key: TGUIDStr):LongBool;

function FindCodeMark(FileBase: PChar; FileSize: Integer): LPTCodeMark;
function GetDLLModuleName (FileBase: PChar; FileSize: Integer): String;
function GetLoadPosition (FileBase: PChar; FileSize: Integer): String;


implementation


var
  IntList: TList;                     

const
  MARK_SIGN = 'ANDSCODESIGN';
  MARK_LENGTH = Length(MARK_SIGN);

function FindCodeMark (FileBase: PChar; FileSize: Integer): LPTCodeMark;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FileSize - MARK_LENGTH do
  begin
    if CompareMem(@MARK_SIGN[1], @FileBase[I], MARK_LENGTH) then
    begin
      Result := @FileBase[I];
      Break;
    end;
  end;
end;

function GetDLLModuleName (FileBase: PChar; FileSize: Integer): String;
var
  CodeMark: LPTCodeMark;
begin
  Result := '';
  CodeMark := FindCodeMark (FileBase, FileSize);
  if not Assigned (CodeMark) then exit;

  Result := CodeMark.MOD_NAME;
end;

function GetLoadPosition (FileBase: PChar; FileSize: Integer): String;
var
  CodeMark: LPTCodeMark;
begin
  Result := '';
  CodeMark := FindCodeMark (FileBase, FileSize);
  if not Assigned (CodeMark) then exit;

  Result := CodeMark.LOAD_POS;
end;


procedure InitRandomInt(Range:Integer);
var
  I: Integer;
begin
  if NOT Assigned(IntList) then
    IntList := TList.Create;

  IntList.Clear;

  for I := 0 to Range do
    IntList.Add(Pointer(I)); 
end;

function GetRandomInt:Integer;
var
  Index: Integer;
begin
  Result := -1;
  if IntList.Count = 0 then EXIT;

  if IntList.Count = 1 then
  begin
    Index := 0;
  end else
  begin
    Randomize;
    Index := Random(IntList.Count - 1);
  end;

  Result := Integer(IntList.Items[Index]);
  IntList.Delete(Index);
end;

const
  SAMPLE_SIGN = 'AND01AND02AND03AND04AND05AND06AND07AND08AND09AND10';
  SAMPLE_LENGTH = Length(SAMPLE_SIGN);

type
  LPRC4Valuex = ^TRC4ValueX;
  TRC4ValueX = packed record
    Index: Byte;
    Value: DWORD;
  end;

  LPArrayRC4ValueX = ^TArrayRC4ValueX;
  TArrayRC4ValueX = array[0..65] of TRC4ValueX;

  LPArrayRC4DWORD = ^TArrayRC4DWORD;
  TArrayRC4DWORD = array[0..65] of DWORD;


function MakeRC4ToCode(FileBase: PChar; FileSize: Integer; const Key: TGUIDStr):LongBool;
var
  RC4Context: TRC4Context;
  I, RC4ValueOffset: Integer;
  ArrayRC4ValueX: LPArrayRC4ValueX;
  ArrayRC4DWORD: LPArrayRC4DWORD;
  Value, Index: Integer;
begin
  Result := False;

  RC4ValueOffset := -1;
  for I := 0 to FileSize - SAMPLE_LENGTH do
  begin
    if CompareMem(@SAMPLE_SIGN[1], @FileBase[I], SAMPLE_LENGTH) then
    begin
      RC4ValueOffset := I;
      Break;
    end;
  end;

  if RC4ValueOffset = -1 then EXIT;
  RC4Init(RC4Context, Key);

  ArrayRC4ValueX := @FileBase[RC4ValueOffset];
  ArrayRC4DWORD := @RC4Context;

  InitRandomInt(65);
  Index := 0;
  repeat
    Value := GetRandomInt;
    if Value = -1 then Break;
    ArrayRC4ValueX[Index].Index := Value;
    ArrayRC4ValueX[Index].Value := ArrayRC4DWORD[Value];
    Inc(Index);
  until False;   

  Result := True;
end;

end.
