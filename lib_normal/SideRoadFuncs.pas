unit SideRoadFuncs;

interface
uses windows, DLLLoader, classes, SysUtils;

Type
  TSideRoadClass = class (TObject)
  protected
    FLoadedDLLs: TList;
  public
    function GetSideRoad (FuncEntry: Pointer): Pointer;
    constructor Create;
    destructor Destroy; override;
  end;


function GetSideRoad (FuncEntry: Pointer): Pointer; Stdcall;

implementation

uses EntryRecogniser;

var
  SR: TSideRoadClass = nil;

function GetSideRoad (FuncEntry: Pointer): Pointer;  Stdcall;
begin
  if not assigned (SR) then
    SR := TSideRoadClass.Create;

  Result := SR.GetSideRoad (FuncEntry);
end;

type
  LPTLoadedDLLInfo = ^TLoadedDLLInfo;
  TLoadedDLLInfo = record
    OriDllHandle: THandle;
    DllName: String;
    DLLLoader: TDLLLoader2;
  end;

function TSideRoadClass.GetSideRoad (FuncEntry: Pointer): Pointer;
label
  EXIT_ERROR;
var
  DFI: LPDLLFuncInfo;
  I: Integer;
  LoaderInfo: LPTLoadedDLLInfo;
  HasFound: LongBool;
begin
  Result := nil;
  LoaderInfo := nil;

  DFI := GetDLL (FuncEntry);
  if not assigned (DFI) then goto EXIT_ERROR;
  if DFI.IsMainModule then goto EXIT_ERROR;
                                    
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

  if not assigned (LoaderInfo) then goto EXIT_ERROR;

  Result := LoaderInfo.DLLLoader.FindExport(DFI.FuncName);
  exit;
  
EXIT_ERROR:
  outputdebugstring ('GetSideRoad Error');
end;

constructor TSideRoadClass.Create;
begin
  FLoadedDLLs := TList.Create;
end;

destructor TSideRoadClass.destroy;
var
  I: Integer;
  LoaderInfo: LPTLoadedDLLInfo;
begin
  for I := 0 to FLoadedDLLs.Count - 1 do
  begin
    LoaderInfo := FLoadedDLLs[I];
    LoaderInfo.DLLLoader.Free;
    FreeMem (LoaderInfo);
  end;
  FLoadedDLLs.Free;
end;


end.
