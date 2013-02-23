unit ShellRunApp;

interface
uses windows, sysutils, classes;

function MakeNewExe (GamePath: String; IsRunMin: BOOL = False): DWORD;

implementation

uses shellapi, tlhelp32;

var
  OldProcList: TList;
  FProcName: String;


procedure MakeProceList (out List: TList; ProcName: String);
var
  hSnapshot: THandle;
  pe: PROCESSENTRY32;
  bOk: BOOL;
  FileName: string;
begin
  List.Clear;
  ProcName := uppercase (ExtractFileName (ProcName));

  pe.dwSize := sizeof(PROCESSENTRY32);
  hSnapshot := CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS,0);
                       
  bOk := Process32First(hSnapshot,pe);
  while bOk do
  begin
    FileName := uppercase (ExtractFileName (pe.szExeFile));
    if FileName = ProcName then
      List.Add(Pointer(PE.th32ProcessID));
    bOk := Process32Next(hSnapshot,pe);
  end;         
  CloseHandle (hSnapshot);
end;

procedure MakeOldList (ProcName: String);
begin
  if not assigned (OldProcList) then
    OldProcList := TList.Create;
  FProcName := ProcName;
  MakeProceList (OldProcList, ProcName);
end;

function GetTheNewProcess: DWORD;
var
  NewList: TList;
  I: Integer;
  NewItem, OldItem: Pointer;
begin
  Result := 0;
  
  NewList := TList.Create;
  MakeProceList (NewList, FProcName);

  for I := NewList.Count - 1 downto 0 do
  begin
    NewItem := NewList[I];
    for OldItem in OldProcList  do
      if OldItem = NewItem then
      begin
        NewList.Delete(I);
        Break;
      end;    
  end;

  if NewList.Count = 0 then exit;

  Result := DWORD (NewList[0]);
end;


function MakeNewExe (GamePath: String; IsRunMin: BOOL = False): DWORD;
var
  BeginTick: DWORD;
  GameDir: String;
begin
  MakeOldList (ExtractFileName (GamePath));
  GameDir := ExtractFilePath (GamePath);
  if IsRunMin then
    ShellExecute(0,'Open',PChar(GamePath),nil,PChar(GameDir),SW_SHOWMINNOACTIVE)
  else
    ShellExecute(0,'Open',PChar(GamePath),nil,PChar(GameDir),SW_SHOWNORMAL);

  BeginTick := GetTickCount;
  repeat
    Result := GetTheNewProcess;
    sleep(100);
    if GetTickCount - BeginTick > 10000 then Break;
  until Result > 0;
end;

//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

end.
