unit SideRoadUnit;

interface
uses windows, DLLLoader, classes, SysUtils;


function GetSideRoad (FuncEntry: Pointer): Pointer; Stdcall;

implementation

uses madKnl;

var
  FLoadedDLLs: TList = nil;

type
  LPTLoadedDLLInfo = ^TLoadedDLLInfo;
  TLoadedDLLInfo = record
    OriDllHandle: THandle;
    DllName: String;
    DLLLoader: TDLLLoader2;
  end;

function GetSideRoad (FuncEntry: Pointer): Pointer;
var
  DFI: LPDLLFuncInfo;
  I: Integer;
  LoaderInfo: LPTLoadedDLLInfo;
  HasFound: LongBool;
begin
  Result := nil;
  LoaderInfo := nil;

  if not Assigned (FLoadedDLLs) then
    FLoadedDLLs := TList.Create;

  DFI := GetDLL (FuncEntry);
  if DFI.IsMainModule then exit;

  HasFound := False;
  for I := 0 to FLoadedDLLs.Count - 1 do
  begin
    LoaderInfo := FLoadedDLLs[I];
    if LoaderInfo.OriDllHandle = DFI.ModuleHandle then
    begin
      HasFound := True;
      Break;
    end;
  end;                                           

  if not HasFound then
  begin                                         
    LoaderInfo := AllocMem (SizeOf (TLoadedDLLInfo));
    LoaderInfo.OriDllHandle := DFI.ModuleHandle;
    LoaderInfo.DllName := DFI.FullDLLName;
    LoaderInfo.DLLLoader := TDLLLoader2.Create;
    if not LoaderInfo.DLLLoader.LoadDLL(DFI.FullDLLName, NIL) then exit;

    FLoadedDLLs.Add(LoaderInfo);
  end;

  if not assigned (LoaderInfo) then exit;

  Result := LoaderInfo.DLLLoader.FindExport(DFI.FuncName);
end;


end.
