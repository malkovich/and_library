unit EntryRecogniser;

interface
uses
  windows, classes;

Type
  LPDLLFuncInfo = ^TDLLFuncInfo;
  TDLLFuncInfo = packed record
    FuncEntry:    Pointer;
    FuncName:     PChar;
    Ordinal:      DWORD;
    FullDLLName:  PChar;
    DLLName:      PChar;
    ModuleHandle: THandle;
    IsMainModule: LongBool;
    Data:         Pointer;
  end;

  TEntryToDLLDetail = class(TObject)
  protected
    FHistory: TList;
  public
    function GetDLL (FuncEntry: Pointer): LPDLLFuncInfo;
    constructor Create;
    destructor Destroy; override;
  end;

function GetDLL (FuncEntry: Pointer): LPDLLFuncInfo;  Stdcall;

implementation

uses madKernel, SysUtils, FuncToImageBase;

var
  ETDD: TEntryToDLLDetail = nil;

function GetDLL (FuncEntry: Pointer): LPDLLFuncInfo;  Stdcall;
begin
  if not assigned (ETDD) then
    ETDD := TEntryToDLLDetail.Create;
  Result := ETDD.GetDLL (FuncEntry);
end;

function TEntryToDLLDetail.GetDLL (FuncEntry: Pointer): LPDLLFuncInfo;
var
  FuncInfo: LPDLLFuncInfo;
  I: Integer;
  im: IModule;
  DllHandle: THandle;
  IEE: IExportEntry;
  TmpStr: String;
begin
  Result := nil;
  if IsBadReadPtr (FuncEntry, 4) then exit;
  
  for I := 0 to FHistory.Count - 1 do
  begin
    FuncInfo := FHistory.Items[I];
    if FuncInfo.FuncEntry = FuncEntry then
    begin
      Result := FuncInfo;
      Exit;
    end;
  end;

  DllHandle := GetDLLHandle (FuncEntry);
  if DllHandle = 0 then exit;

  im := Module (DllHandle);
  if not im.IsValid then exit;

  IEE := im.ExportList.FindItem (FuncEntry);
  if not IEE.IsValid then exit;

  FuncInfo := AllocMem (SizeOf (TDLLFuncInfo));
  FuncInfo.FuncEntry := FuncEntry;
  FuncInfo.ModuleHandle := DllHandle;
  FuncInfo.Ordinal := IEE.Ordinal;
  FuncInfo.IsMainModule := im.IsMainModule;

  TmpStr := ExtractFileName (im.FileName);
  FuncInfo.DLLName := AllocMem (Length (TmpStr) + 1);
  StrCopy (FuncInfo.DLLName, @TmpStr[1]);

  TmpStr := im.FileName;
  FuncInfo.FullDLLName := AllocMem (Length (TmpStr) + 1);
  StrCopy (FuncInfo.FullDLLName, @TmpStr[1]);

  TmpStr := IEE.Name;
  FuncInfo.FuncName := AllocMem (Length (TmpStr) + 1);
  StrCopy (FuncInfo.FuncName, @TmpStr[1]);

  FHistory.Add(FuncInfo);
                       
  Result := FuncInfo;
end;

constructor TEntryToDLLDetail.Create;
begin
  FHistory := TList.Create;
end;

destructor TEntryToDLLDetail.destroy;
var
  FuncInfo: LPDLLFuncInfo;
  I: Integer;
begin
  for I := 0 to FHistory.Count - 1 do
  begin
    FuncInfo := FHistory.Items[I];
    if assigned (FuncInfo) then
    begin
      FreeMem (FuncInfo.FuncName);
      FreeMem (FuncInfo.FullDLLName);
      FreeMem (FuncInfo.DLLName);
      FreeMem (FuncInfo);
    end;
  end;
  FHistory.Free;
end;


end.
