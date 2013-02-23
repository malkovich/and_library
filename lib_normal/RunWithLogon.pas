unit RunWithLogon;

interface
uses
  windows, Graphics, SysUtils, classes;

const
  LOGON_WITH_PROFILE = 1;
  LOGON_NETCREDENTIALS_ONLY = 2;
 
function CreateProcessWithLogon(
                                 lpUsername: PWChar;
                                 lpDomain: PWChar;
                                 lpPassword: PWChar;
                                 dwLogonFlags: DWORD;
                                 lpApplicationName: PWChar;
                                 lpCommandLine: PWChar;
                                 dwCreationFlags: DWORD;
                                 lpEnvironment: Pointer;
                                 lpCurrentDirectory: PWChar;
                                 const lpStartupInfo: TStartupInfo;
                                 var lpProcessInfo: TProcessInformation
                               ): BOOL; stdcall;

function RunAsUser(szUserName, szDomain, szPassword, szAppName:WideString; wShowWindow: WORD = SW_SHOWNORMAL):THandle;


implementation

function CreateProcessWithLogon; external advapi32 name 'CreateProcessWithLogonW';


function RunAsUser(szUserName, szDomain, szPassword, szAppName:WideString; wShowWindow: WORD = SW_SHOWNORMAL):THandle;
var
  pwUsername, pwDomain, pwPassword, pwApplicationName: PWideChar;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  pwUsername := Addr(szUserName[1]);
  pwDomain := Addr(szDomain[1]);
  pwPassword := Addr(szPassword[1]);
  pwApplicationName := Addr(szAppName[1]);

  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.wShowWindow := wShowWindow;
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;      

  if CreateProcessWithLogon(pwUsername,pwDomain,pwPassword,LOGON_WITH_PROFILE,
                                nil,pwApplicationName,CREATE_DEFAULT_ERROR_MODE or CREATE_NEW_PROCESS_GROUP,
                                nil,nil,StartupInfo,ProcessInfo) then
  begin
    Result := ProcessInfo.hProcess;
  end else
  begin
    Result := 0;
    outputdebugstring (PChar('CreateProcessWithLogon error ' + SysErrorMessage(GetLastError)));
  end;
end;


end.
