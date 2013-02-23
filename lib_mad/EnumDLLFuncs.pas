unit EnumDLLFuncs;

interface
uses windows;

Type
  TEnumDLLRoutine= procedure (LibName: PChar; FuncName: PChar; FuncEntry: Pointer); stdcall;

procedure EnumDLLFunction (hHandle: THandle; CallBack: TEnumDLLRoutine); stdcall;

implementation

uses MadKernel, SysUtils;

procedure EnumDLLFunction (hHandle: THandle; CallBack: TEnumDLLRoutine); stdcall;
var
  iMod : IModule;
  ExportList: IXxportList;
  ExportEntry: IExportEntry;
  I: Integer;
  ModFileName: String;
begin
  iMod := Module (hHandle);
  if not iMod.IsStillValid then exit;
  if not assigned (CallBack) then exit;
  ExportList := iMod.ExportList;

  ModFileName := ExtractFileName (iMod.FileName);

  for I := 0 to ExportList.ItemCount - 1 do
  begin
    ExportEntry := ExportList.Items[I];
    CallBack(PChar(ModFileName), @ExportEntry.Name[1], ExportEntry.Address);
  end;
end;

end.
