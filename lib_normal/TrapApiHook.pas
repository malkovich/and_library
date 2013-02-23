unit TrapApiHook;

interface
uses windows, classes, sysutils;

//smart api hooker
function SMHookApi (FuncEntry, Hooker: Pointer): Pointer; stdcall;
function UnSMHookApi (FuncEntry: Pointer): LongBool; stdcall;


implementation

uses madDisasm, TCodeEventNotify, RandomNopCode;

type
  LPTHookInfo = ^THookInfo;
  THookInfo = packed record
    FuncEntry: Pointer;
    Hooker: Pointer;
    RealEntry: Pointer;
    CopyLength: Integer;
    JmpFromAddr: Pointer;
    JmpBackAddr: Pointer;
  end;

var
  HookerList: TList;

function GetCopyLength (FuncEntry: Pointer): Integer;
var
  FuncLength: Integer;
  FunctionInfo: TFunctionInfo;
  CodeInfo: TCodeInfo;
  ScanPtr: Pointer;
  CopyLen: Integer;
begin
  result := 0;

  FunctionInfo := ParseFunction (FuncEntry);
  FuncLength := FunctionInfo.CodeLen;

  CopyLen := 0;
  ScanPtr := FuncEntry;
  repeat
    CodeInfo := ParseCode (ScanPtr);
    if not CodeInfo.IsValid then break;
    if assigned (CodeInfo.Target) then break;
    if PByte(ScanPtr)^ = $C2 then break;
    if PByte(ScanPtr)^ = $C3 then break;
    CopyLen := CopyLen + (Integer(CodeInfo.Next) - Integer(CodeInfo.This));
    ScanPtr := CodeInfo.Next;
  until CopyLen >= FuncLength;

  if CopyLen < 5 then exit;

  Result := CopyLen;
end;


function SMHookApi (FuncEntry, Hooker: Pointer): Pointer; stdcall;
type
  LPTJmpStruct = ^TJmpStruct;
  TJmpStruct=packed record
    Jmp: Byte;
    Offset: Integer;
  end;
var
  Index: Integer;
  HookInfo: LPTHookInfo;
  JmpStru: LPTJmpStruct;
  Template: PChar;
  BeenHook: Bool;
begin
  result := nil;
  if not assigned (HookerList) then
    HookerList := TList.Create;

  HookInfo := nil;
  BeenHook := False;
  for Index := 0 to HookerList.Count - 1 do
  begin
    HookInfo := HookerList.Items[Index];
    if HookInfo.FuncEntry = FuncEntry then
    begin
      BeenHook := True;
      break;
    end;
  end;

  if BeenHook then
  begin
    Result := HookInfo.RealEntry;
    exit;
  end;

  HookInfo := AllocMem (SizeOf(THookInfo));
  HookInfo.FuncEntry := FuncEntry;
  HookInfo.Hooker := Hooker;
  HookInfo.CopyLength := GetCopyLength (HookInfo.FuncEntry);

  if HookInfo.CopyLength = 0 then
  begin
    FreeMem (HookInfo);
    exit;
  end;

  HookInfo.JmpBackAddr := Pointer(Integer(HookInfo.FuncEntry) + HookInfo.CopyLength);
  HookInfo.JmpFromAddr := Pointer(DWORD(HookInfo.JmpBackAddr) - SizeOf(TJmpStruct));
  HookInfo.RealEntry := AllocMem(HookInfo.CopyLength + SizeOf(TJmpStruct));

  CopyMemory(HookInfo.RealEntry, HookInfo.FuncEntry, HookInfo.CopyLength);
  JmpStru := Pointer(Integer(HookInfo.RealEntry) + HookInfo.CopyLength);
  JmpStru.Jmp := $E9;
  JmpStru.Offset := Integer(HookInfo.JmpBackAddr) - Integer(JmpStru) - SizeOf(TJmpStruct);

  Template  := AllocMem(HookInfo.CopyLength);
  JmpStru := Pointer(Integer(Template) + HookInfo.CopyLength - SizeOf(TJmpStruct));
  JmpStru.Jmp := $E9;
  JmpStru.Offset := Integer(HookInfo.Hooker) - Integer(HookInfo.JmpFromAddr) - SizeOf(TJmpStruct);

  FillRandomCode (Template, JmpStru);

  WriteMemory (HookInfo.FuncEntry, Template, HookInfo.CopyLength);

  FreeMem(Template);

  Result := HookInfo.RealEntry;

  HookerList.Add(HookInfo);
end;

function UnSMHookApi (FuncEntry: Pointer): LongBool; stdcall;
var
  Index: Integer;
  HookInfo: LPTHookInfo;
  BeenHook: Bool;
begin
  result := false;
  if FuncEntry = NIL then Exit;
  if not assigned (HookerList) then exit;

  BeenHook := False;
  for Index := 0 to HookerList.Count - 1 do
  begin
    HookInfo := HookerList.Items[Index];
    if HookInfo.FuncEntry = FuncEntry then
    begin
      BeenHook := True;
      break;
    end;
  end;
  if not BeenHook then exit;

  HookInfo := HookerList.Items[Index];
  WriteMemory (HookInfo.FuncEntry, HookInfo.RealEntry, HookInfo.CopyLength);

  HookerList.Delete(Index);
  FreeMem (HookInfo.RealEntry);
  FreeMem (HookInfo);

  result := True;  
end;

end.
