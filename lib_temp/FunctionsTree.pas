unit FunctionsTree;

interface
uses
  Windows, Classes, SysUtils;

function FindFuncEntryA (LibName, FuncName: String): POINTER; stdcall;
function FindFuncEntry (LibNameCRC, FuncNameCRC: LongWord): POINTER; stdcall;
function SaveFuncEntry (LibNameCRC, FuncNameCRC: LongWord; FuncEntry: POINTER): LongBool; stdcall; overload;
function SaveFuncEntry (LibName, FuncName: String; FuncEntry: POINTER): LongBool; stdcall; overload;
procedure ClearFuncDB;

procedure SaveDLLFuncs (DLLFullName: String);

function GetStringHash(InputName: PChar):DWORD;stdcall;

implementation

uses TDBinTre, DLLLoader;

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

procedure SaveDLLFuncs (DLLFullName: String);
var
  Name: String;
begin
  Name := ExtractFileName (DLLFullName);
  LoadLibraryExport(PChar(Name), EnumProc, PChar(DLLFullName));
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

function _GetStringCRC(InputName: PChar):DWORD;stdcall;
var
  Buffer: array[byte] of char;
  I: Integer;
  Value: DWORD;
begin
  ZeroMemory(@Buffer[0], 256);
  StrLCopy(@Buffer[3], InputName, 250);
  Result := 0;
  for I := 0 to 250 do
  begin
    Value := PDWORD(@Buffer[I])^;
    INC(Result, Value);
    if Value = 0 then Break;
  end;
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

function FindFuncEntryA (LibName, FuncName: String): POINTER;
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
  StoreEntry: Pointer;
begin
  if Not Assigned(RedBlackTree) then
    RedBlackTree := TtdRedBlackTree.Create(CompareTStoreStruct, DisposeMem);

  StoreStruct := AllocMem(SizeOf(TStoreStruct));
  StoreStruct.LibNameCRC := LibNameCRC;
  StoreStruct.FuncNameCRC := FuncNameCRC;
  StoreStruct.FuncEntry := FuncEntry;

  Result := False;
  try
    StoreEntry := RedBlackTree.Find(StoreStruct);
    if Assigned (StoreEntry) then Exit;

    RedBlackTree.Insert(StoreStruct);
    Result := True;
  except
  end;
end;

function SaveFuncEntry (LibName, FuncName: String; FuncEntry: POINTER): LongBool; stdcall;
var
  TryCount: Integer;
begin
  LibName := UpperCase(LibName);   
  FuncName := UpperCase(FuncName);

  TryCount := 0;
  Repeat
    Result := SaveFuncEntry(GetStringHash(@LibName[1]), GetStringHash(@FuncName[1]), FuncEntry);
    if not Result then
    begin
      FuncName := FuncName + 'X';
      Inc(TryCount);
    end;
  until Result or (TryCount > 5);
  
  if TryCount > 0 then
    OutputDebugString (PChar(Format('Result=%d %s - %s', [Integer(Result), LibName, FuncName])));
end;

procedure ClearFuncDB;
begin
  RedBlackTree.Free;
  RedBlackTree := NIL;
end;


end.
