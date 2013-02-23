unit SideRoadApiHook;

interface
uses windows, classes, sysutils;

function SRHookApi (FuncEntry, Hooker: Pointer): Pointer; stdcall;
function UnSRHookApi (FuncEntry: Pointer): LongBool; stdcall;  

implementation

uses madDisasm, TCodeEventNotify, SideRoadFuncs, RandomNopCode;

type
  LPTHookInfo = ^THookInfo;
  THookInfo = packed record
    FuncEntry: Pointer;
    Hooker: Pointer;
    RealEntry: Pointer;
    BackupCopy: Pointer;
    CopyLength: Integer;
    JmpFromAddr: Pointer;
  end;

type
  LPTJmpStruct = ^TJmpStruct;
  TJmpStruct=packed record
    Jmp: Byte;
    Offset: Integer;
  end;

var
  HookerList: TList;

function GetJmpFromAddr (FuncEntry: Pointer): Pointer;
var
  FunctionInfo: TFunctionInfo;
  TotalLen, JmpPosOffset, IterCount: Integer;
begin
  result := NIL;
  FunctionInfo := ParseFunction (FuncEntry);
  TotalLen := FunctionInfo.CodeLen;
  if TotalLen < SizeOf(TJmpStruct) then exit;
  if TotalLen > 32 then
    TotalLen := 32;

  IterCount := 0;
  repeat
    Randomize;
    JmpPosOffset := Random(TotalLen - SizeOf(TJmpStruct));
    Inc (IterCount);
    if IterCount > 18 then Exit;
  until JmpPosOffset > SizeOf(TJmpStruct);

  Result := Pointer(Integer(FuncEntry) + JmpPosOffset);
end;
  
function SRHookApi (FuncEntry, Hooker: Pointer): Pointer; stdcall;
type
  LPTJmpStruct = ^TJmpStruct;
  TJmpStruct=packed record
    Jmp: Byte;
    Offset: Integer;
  end;
var
  Index, JmpStruOffset: Integer;
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
  HookInfo.RealEntry := GetSideRoad (HookInfo.FuncEntry);
  HookInfo.JmpFromAddr := GetJmpFromAddr (HookInfo.FuncEntry);

  if HookInfo.JmpFromAddr = nil then
  begin
    FreeMem (HookInfo);
    Exit;
  end;

  JmpStruOffset := Integer(HookInfo.JmpFromAddr) - Integer(HookInfo.FuncEntry);
  HookInfo.CopyLength := JmpStruOffset  + SizeOf(TJmpStruct);

  Template  := AllocMem(HookInfo.CopyLength);
  JmpStru := Pointer (Integer(Template) + JmpStruOffset);
  JmpStru.Jmp := $E9;
  JmpStru.Offset := Integer(HookInfo.Hooker) - Integer(HookInfo.JmpFromAddr) - SizeOf(TJmpStruct);
  FillRandomCode (Template, JmpStru);

  HookInfo.BackupCopy := AllocMem(HookInfo.CopyLength);
  CopyMemory (HookInfo.BackupCopy, HookInfo.FuncEntry, HookInfo.CopyLength);
  WriteMemory (HookInfo.FuncEntry, Template, HookInfo.CopyLength);

  FreeMem(Template);
  Result := HookInfo.RealEntry;
  HookerList.Add(HookInfo);
end;

function UnSRHookApi (FuncEntry: Pointer): LongBool; stdcall;
var
  Index: Integer;
  HookInfo: LPTHookInfo;
  BeenHook: Bool;
begin
  result := false;

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
  WriteMemory (HookInfo.FuncEntry, HookInfo.BackupCopy, HookInfo.CopyLength);

  HookerList.Delete(Index);
  FreeMem (HookInfo.BackupCopy);
  FreeMem (HookInfo);

  result := True;  
end;

end.
