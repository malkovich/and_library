unit RunWithControl;

interface
uses windows,sysutils;

Type
  TExitCallback = procedure (AppName: PChar; Handle: THandle; ExitCode: DWORD);

function  RunAsDefault_Create   (AppName: PChar; ExitCallback: TExitCallback): THandle; Stdcall;
procedure RunAsDefault_Terminat (RunHandle: THandle; ExitCode: DWORD); Stdcall;
function  RunAsDefault_PassTime (RunHandle: THandle): DWORD; Stdcall;
function  RunAsDefault_IsRunning(RunHandle: THandle): BOOL; Stdcall;
procedure RunAsDefault_Destroy  (RunHandle: THandle; ExitCode: DWORD); Stdcall;

implementation

uses RunAsUserUnit;

Type
  LPTRunExeObject = ^TRunExeObject;
  TRunExeObject = record
    AppPID: DWORD;
    AppHandle: THandle;
    StartTime: DWORD;
    IsRunning: BOOL;
    Terminate: BOOL;
    TerminateExitCode: DWORD;
    ExitCallback: Pointer;
    AppExitCode: DWORD;
    DaemonThreadID: DWORD;
    DaemonThreadHandle: THandle;
    AppName: String[255];
  end;

procedure RunAsDefault_Terminat (RunHandle: THandle; ExitCode: DWORD); Stdcall;
var
  RunExeObject: LPTRunExeObject absolute RunHandle;
  WaitCount: Integer;
begin
  if RunExeObject.IsRunning then
  begin
    RunExeObject.Terminate := True;
    RunExeObject.TerminateExitCode := ExitCode;
    WaitCount := 0;
    Repeat
      Sleep(500);
      Inc (WaitCount);
    until (Not RunExeObject.IsRunning) or (WaitCount > 60);
  end;
end;

function  RunAsDefault_PassTime (RunHandle: THandle): DWORD; Stdcall;
var
  RunExeObject: LPTRunExeObject absolute RunHandle;
begin
  Result := GetTickCount - RunExeObject.StartTime;
end;

function  RunAsDefault_IsRunning(RunHandle: THandle): BOOL; Stdcall;
var
  RunExeObject: LPTRunExeObject absolute RunHandle;
begin
  Result := RunExeObject.IsRunning;
end;

procedure RunAsDefault_Destroy  (RunHandle: THandle; ExitCode: DWORD); Stdcall;
var
  RunExeObject: LPTRunExeObject absolute RunHandle;
begin
  RunAsDefault_Terminat (RunHandle, ExitCode);
  FreeMem (RunExeObject);
end;

  
function DaemonThreadRoutine (RunParam: LPTRunExeObject): Integer; Stdcall;
var
  WaitResult: Integer;
begin
  Result := 0;
  RunParam.StartTime := GetTickCount;
  RunParam.AppPID := RunAsDefault (@RunParam.AppName[1]);
  RunParam.AppHandle := OpenProcess (PROCESS_ALL_ACCESS, FALSE, RunParam.AppPID);

  Repeat
    if RunParam.Terminate then
    begin
      RunParam.AppExitCode := RunParam.TerminateExitCode;
      TerminateProcess(RunParam.AppHandle, RunParam.AppExitCode);
      Break;
    end;

    WaitResult := WaitForSingleObject(RunParam.AppHandle ,1000);
    case WaitResult of
    WAIT_TIMEOUT: Continue;
    WAIT_OBJECT_0:
      Begin
        GetExitCodeProcess (RunParam.AppHandle, RunParam.AppExitCode);
        Break;
      end;
    ELSE Begin
        RunParam.AppExitCode := $88888888;
        TerminateProcess(RunParam.AppHandle, RunParam.AppExitCode);
        Break;
      end;
    end;     
  Until False;

  CloseHandle (RunParam.AppHandle);   
  TExitCallback (RunParam.ExitCallback) (@RunParam.AppName[1], THandle(RunParam), RunParam.AppExitCode);
  RunParam.IsRunning := False;
end;

function RunAsDefault_Create (AppName: PChar; ExitCallback: TExitCallback): THandle ; Stdcall;
var
  RunExeObject: LPTRunExeObject;
begin
  RunExeObject := AllocMem (SizeOf(TRunExeObject));
  RunExeObject.IsRunning := True;
  RunExeObject.AppName := StrPas (AppName);
  RunExeObject.Terminate := False;
  RunExeObject.ExitCallback := @ExitCallback;
  RunExeObject.AppExitCode := 0;
  RunExeObject.DaemonThreadHandle := CreateThread (nil, 0, @DaemonThreadRoutine, RunExeObject, 0, RunExeObject.DaemonThreadID);
  Result := THandle (RunExeObject);
end;


end.
