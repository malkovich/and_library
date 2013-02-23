unit FuncDataBase;

interface
uses
  Windows, Classes, SysUtils;

function FindFuncEntry (LibName, FuncName: String): POINTER; stdcall; overload;
function FindFuncEntry (LibNameCRC, FuncNameCRC: LongWord): POINTER; stdcall; overload;
function SaveFuncEntry (LibNameCRC, FuncNameCRC: LongWord; FuncEntry: POINTER): LongBool; stdcall; overload;
function SaveFuncEntry (LibName, FuncName: String; FuncEntry: POINTER): LongBool; stdcall; overload;
procedure ClearFuncDB;


function GetStringHash(InputName: PChar):DWORD;stdcall;

implementation

uses TDBinTre;

FUNCTION EnumProc (Sender:POINTER; Name:STRING; Index:INTEGER; FunctionPointer:POINTER):BOOLEAN;
VAR LibName:PChar ABSOLUTE Sender;
BEGIN
  SaveFuncEntry(StrPas(LibName), Name, FunctionPointer);
  RESULT := TRUE;
END;

function GetMainPath(hModule:THandle):string;
var
  Buffer:array[byte] of char;
begin
  ZeroMemory(@buffer[0], 256);
  GetModuleFileName(hModule, @Buffer, 256);
  result := ExtractFilePath(buffer);
end;


function GetStringHash(InputName: PChar):DWORD;stdcall;
var
  G : DWORD;
  i : integer;
  Hash : DWORD;
begin
  Hash := 0;
  for i := 0 to StrLen(InputName) -1 do begin
    Hash := (Hash shl 4) + ord(InputName[i]);
    G := Hash and $F0000000;
    if (G <> 0) then
      Hash := (Hash xor (G shr 24)) xor G;
  end;
  Result := Hash;
end;


Type
  LPTStoreStruct = ^TStoreStruct;
  TStoreStruct = packed record
    LibNameCRC:  LongWord;
    FuncNameCRC: LongWord;
    FuncEntry: POINTER;
  end;

var
  RedBlackTree: TtdRedBlackTree = nil;

function FindFuncEntry (LibNameCRC, FuncNameCRC: LongWord): POINTER; stdcall;
var
  StoreStruct: TStoreStruct;
  pStoreStruct: LPTStoreStruct;
begin
  Result := NIL;
  StoreStruct.LibNameCRC := LibNameCRC;
  StoreStruct.FuncNameCRC := FuncNameCRC;

  pStoreStruct := RedBlackTree.Find(@StoreStruct);
  if Assigned(pStoreStruct) then
    Result := pStoreStruct.FuncEntry;
end;

function FindFuncEntry (LibName, FuncName: String): POINTER;
begin
  LibName := UpperCase(LibName);
  FuncName := UpperCase(FuncName); 
  Result := FindFuncEntry (GetStringHash(@LibName[1]), GetStringHash(@FuncName[1]));
end;

procedure DisposeMem(aData : pointer);
begin
  FreeMem(aData);
end;
function CompareTStoreStruct(aData1, aData2 : pointer) : integer;
var
  L1, L2: Int64;
begin
  L1 := PInt64(aData1)^;
  L2 := PInt64(aData2)^;

  if (L1 < L2) then
    Result := -1
  else if (L1 = L2) then
    Result := 0
  else
    Result := 1
end;

function SaveFuncEntry (LibNameCRC, FuncNameCRC: LongWord; FuncEntry: POINTER): LongBool; stdcall;
var
  StoreStruct: LPTStoreStruct;
begin
  if Not Assigned(RedBlackTree) then
    RedBlackTree := TtdRedBlackTree.Create(CompareTStoreStruct, DisposeMem);

  StoreStruct := AllocMem(SizeOf(TStoreStruct));
  StoreStruct.LibNameCRC := LibNameCRC;
  StoreStruct.FuncNameCRC := FuncNameCRC;
  StoreStruct.FuncEntry := FuncEntry;

  try
    Result := True;
    if RedBlackTree.Find(StoreStruct) = nil then
      RedBlackTree.Insert(StoreStruct);
  except
    Result := False;
  end;
end;

function SaveFuncEntry (LibName, FuncName: String; FuncEntry: POINTER): LongBool; stdcall;
begin
  LibName := UpperCase(LibName);
  FuncName := UpperCase(FuncName);
  Result := SaveFuncEntry(GetStringHash(@LibName[1]), GetStringHash(@FuncName[1]), FuncEntry);
end;

procedure ClearFuncDB;
begin
  RedBlackTree.Free;
  RedBlackTree := NIL;
end;

initialization
  

end.
