unit RunAsUserUnit;

interface
uses windows, classes, SysUtils, NetAddUser, RunWithLogon;

function RunAppWithUser(szAppName:string; UserName: String = 'ANDSERVER'; Password: String = '没有密码';  wShowWindow: WORD = SW_SHOWNORMAL):THandle;
function RunAppWithUserA (szAppName, UserName, Password: PChar; wShowWindow: WORD):THandle; stdcall;
function NewProcess(FileName: String; var hProcess:THandle): BOOL; stdcall; overload;
function NewProcess(FileName: String): THandle;stdcall; overload;
function NewConsole(AppCmdLine: String; wShowWindow: DWORD = SW_SHOWNORMAL): BOOL; stdcall;
function NewApp(FileName: String; wShowWindow: WORD = SW_SHOWNORMAL): THandle; stdcall;
function WinExecute(FileName: String; wShowWindow: WORD = SW_SHOWNORMAL): THandle;stdcall;
function RunAsDefault (AppName: PChar): DWORD; Stdcall;
function RunConsoleAsDefault (AppName: String): TStringList; Stdcall;

implementation

uses accctrl, madKernel, TraceLastError;


function NewApp(FileName: String; wShowWindow: WORD = SW_SHOWNORMAL): THandle; stdcall;
var
  StartupInfo : TStartupInfo;
  ProcessInfo : TProcessInformation;
begin
  Result := 0;                   
  FillChar(StartupInfo,SizeOf(StartupInfo),#0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow   :=   wShowWindow;
  if CreateProcess(nil, PChar(FileName), nil, nil, false,
                       CREATE_DEFAULT_ERROR_MODE or NORMAL_PRIORITY_CLASS,
                       nil, nil,
                       StartupInfo, ProcessInfo) then
  Result := ProcessInfo.hProcess;
end;

function WinExecute(FileName: String; wShowWindow: WORD = SW_SHOWNORMAL): THandle;stdcall;
begin
  result := Winexec(PChar(FileName), wShowWindow);
end;

function NewProcess(FileName: String): THandle;stdcall;
var
  ProcHandle: THandle;
begin
  Result := 0;
  if NewProcess(FileName, ProcHandle) then
    Result := ProcHandle;
end;

function NewProcess(FileName: String; var hProcess:THandle): BOOL; stdcall;
var
  StartupInfo : TStartupInfo;
  ProcessInfo : TProcessInformation;
begin
  FillChar(StartupInfo,SizeOf(StartupInfo),#0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow   :=   SW_SHOWNORMAL;
  Result := CreateProcess(nil, PChar(FileName), nil, nil, false,
                       CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                       nil, nil,
                       StartupInfo, ProcessInfo);
  hProcess := ProcessInfo.hProcess;
end;

function NewConsole(AppCmdLine: String; wShowWindow: DWORD = SW_SHOWNORMAL): BOOL; stdcall;
var
  StartupInfo : TStartupInfo;
  ProcessInfo : TProcessInformation;
begin
  FillChar(StartupInfo,SizeOf(StartupInfo),#0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow   :=   wShowWindow;
  Result := CreateProcess(nil, PChar(AppCmdLine), nil, nil, false,
                       CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
                       nil, nil,
                       StartupInfo, ProcessInfo);
end;

function RunAppWithUser(szAppName:string; UserName: String = 'ANDSERVER'; Password: String = '没有密码';  wShowWindow: WORD = SW_SHOWNORMAL):THandle;
begin
  Result := RunAppWithUserA (PChar(szAppName), PChar(UserName), PChar(Password), wShowWindow);
end;

function RunAppWithUserA (szAppName, UserName, Password: PChar; wShowWindow: WORD):THandle; stdcall;
var
  SL: TStringList;
  OsUser: String;
  Found: Bool;
begin
  Found := False;
  SL := TStringList.Create;
  GetUsers(SL);
  for OsUser in SL do
    if OsUser = UserName then
       Found := True;
  SL.Free;

  if not Found then
    AddUser(UserName, Password);

  result := RunAsUser(UserName, GetDomainName, Password, szAppName, wShowWindow);
  if Result = 0 then
  begin
    DelUser (UserName);
    AddUser(UserName, Password);
    result := RunAsUser(UserName, GetDomainName, Password, szAppName, wShowWindow);
  end;
end;



function IsVista: boolean;
begin
   Result := not (Win32MajorVersion <6);
end;

function RunAsDefault (AppName: PChar): THandle; Stdcall;
var
  DeskTop: THandle;
  desktopPId: DWORD;
  hProc, hToken, hPrimToken: THANDLE;
  si: STARTUPINFO;
  pi: PROCESS_INFORMATION;
  hNewWinSta: Thandle;
  hNewDesktop: HDESK;
begin
  Result := 0;
  if not Assigned (AppName) then  Exit;

  //打开“WinSta0”工作站上面的“Default”桌面
  hNewWinSta   :=   OpenWindowStation('WinSta0',false,MAXIMUM_ALLOWED);
  SetProcessWindowStation(hNewWinSta);
  hNewDesktop   :=   OpenDesktop('Default',0,false,MAXIMUM_ALLOWED);
  SetThreadDesktop(hNewDesktop);
//
//  if IsVista then
//  begin
//    Result := ShellExecute(0, 'runas', AppName, nil, nil, SW_SHOWNORMAL);
//    exit;
//  end;

  DeskTop := FindWindow('Progman', 'Program Manager');

  if IsWindow(desktop) then
  begin
    GetWindowThreadProcessId (DeskTop, desktopPId);
  end else
  begin
    And_SetLastError ('RunAsDefault', 'IsWindow err ' + IntToStr(DeskTop));
    desktopPId := Process ('C:\Windows\Explorer.exe').ID;
    if desktopPId = 0 then
    begin
      And_SetLastError ('RunAsDefault', 'desktopPId error');
      Exit;
    end;
  end;

  hProc := OpenProcess (PROCESS_QUERY_INFORMATION, FALSE, desktopPId);
  if hProc = 0 then
  begin
    And_SetLastError ('RunAsDefault', 'OpenProcess error ' + SysErrorMessage(GetLastError));
    exit;
  end;

  if not OpenProcessToken(hProc, TOKEN_DUPLICATE, hToken) then
  begin
    And_SetLastError ('RunAsDefault', 'OpenProcessToken error');
    CloseHandle(hProc);
    exit;
  end;                            

  if not DuplicateTokenEx (hToken, TOKEN_ALL_ACCESS, nil, SecurityImpersonation, TokenPrimary, hPrimToken) then
  begin
    And_SetLastError ('RunAsDefault', 'DuplicateTokenEx error');
    CloseHandle(hProc);
    Exit;
  end;             

  ZeroMemory (@si, SizeOf(STARTUPINFO));
  si.cb := SizeOf (STARTUPINFO);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_SHOWNORMAL;

  if CreateProcessAsUser (hPrimToken, nil, PChar(AppName), nil, nil, False, CREATE_DEFAULT_ERROR_MODE or NORMAL_PRIORITY_CLASS, nil, nil, si, pi ) then
  begin
    Result := pi.dwProcessId;
    CloseHandle(pi.hThread);    
    CloseHandle(pi.hProcess);
  end else
    And_SetLastError ('RunAsDefault', 'CreateProcessAsUser error ' + SysErrorMessage(GetLastError));

  CloseHandle(hPrimToken);
  CloseHandle(hProc);
end;


function RunConsoleAsDefault (AppName: String): TStringList; Stdcall;
var
  hreadpipe,hwritepipe:thandle;
  lsa:security_attributes;
  DeskTop: THandle;
  desktopPId, BeginTick: DWORD;
  hProc, hToken, hPrimToken: THANDLE;
  si: STARTUPINFO;
  pi: PROCESS_INFORMATION;
  hNewWinSta: Thandle;
  hNewDesktop: HDESK;
  SubProcDir, ReplyStr: String;
  cchreadbuffer, dwExitCode:dword;
  ReadChar: Char;
begin
  Result := TStringList.Create;
  if AppName = '' then Exit;

  //打开“WinSta0”工作站上面的“Default”桌面
  hNewWinSta   :=   OpenWindowStation('WinSta0',false,MAXIMUM_ALLOWED);
  SetProcessWindowStation(hNewWinSta);
  hNewDesktop   :=   OpenDesktop('Default',0,false,MAXIMUM_ALLOWED);
  SetThreadDesktop(hNewDesktop);

  DeskTop := FindWindow('Progman', 'Program Manager');

  if IsWindow(desktop) then
  begin
    GetWindowThreadProcessId (DeskTop, desktopPId);
  end else
  begin
    And_SetLastError ('RunAsDefault', 'IsWindow err ' + IntToStr(DeskTop));
    desktopPId := Process ('C:\Windows\Explorer.exe').ID;
    if desktopPId = 0 then
    begin
      And_SetLastError ('RunAsDefault', 'desktopPId error');
      Exit;
    end;
  end;

  hProc := OpenProcess (PROCESS_QUERY_INFORMATION, FALSE, desktopPId);
  if hProc = 0 then
  begin
    And_SetLastError ('RunAsDefault', 'OpenProcess error ' + SysErrorMessage(GetLastError));
    exit;
  end;

  if not OpenProcessToken(hProc, TOKEN_DUPLICATE, hToken) then
  begin
    And_SetLastError ('RunAsDefault', 'OpenProcessToken error');
    CloseHandle(hProc);
    exit;
  end;                            

  if not DuplicateTokenEx (hToken, TOKEN_ALL_ACCESS, nil, SecurityImpersonation, TokenPrimary, hPrimToken) then
  begin
    And_SetLastError ('RunAsDefault', 'DuplicateTokenEx error');
    CloseHandle(hProc);
    Exit;
  end;             

  //创建管道
  lsa.nlength := sizeof(SECURITY_ATTRIBUTES);
  lsa.lpsecuritydescriptor := nil;
  lsa.binherithandle := true;
  if not createpipe(hReadpipe,hWritepipe,@lsa,0) then
  begin
    ExitCode := GetLastError;
    exit;
  end;

  //创建进程
  fillchar(si,sizeof(STARTUPINFO),0);
  si.cb   :=sizeof(STARTUPINFO);
  si.dwflags   :=(startf_usestdhandles   or   startf_useshowwindow);
  si.wshowwindow   :=SW_HIDE;
  si.hstdoutput   :=hwritepipe;
  SubProcDir := ExtractFilePath(AppName);
                                                                            // CREATE_DEFAULT_ERROR_MODE or NORMAL_PRIORITY_CLASS
  if CreateProcessAsUser (hPrimToken, nil, PChar(AppName), nil, nil, True, CREATE_NEW_CONSOLE, nil, nil, si, pi ) then
  begin
    //读取运行信息，直到进程结束
    ReplyStr := '';
    BeginTick := GetTickCount;
    while true do
    begin
      if not PeekNamedPipe(hReadPipe, @ReadChar, 1, @cchReadBuffer,nil,nil) then break;
      if cchReadBuffer <> 0 then
      begin
        if ReadFile(hReadPipe, ReadChar, 1,cchReadBuffer,nil)=false then break;
        
        if Assigned (Result) then
        begin
          ReplyStr := ReplyStr + ReadChar;
          if ReadChar = #$D then
          begin
            Result.Add(Trim(ReplyStr));
            ReplyStr := '';
          end;
        end;

      end else
      begin
        if Result.Count > 0 then    
        if WAIT_OBJECT_0 = WaitForSingleObject (pi.hprocess ,888) then
        begin
          GetExitCodeProcess (pi.hprocess, dwExitCode);
          break;
        end;

        if GetTickCount - BeginTick > 2*60*1000 then
        begin
          dwExitCode := DWORD(-1);
          if not TerminateProcess (pi.hprocess, dwExitCode) then
            dwExitCode := GetLastError;
          Break;
        end;
      end;
    end;

    CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);

  end else
    And_SetLastError ('RunAsDefault', 'CreateProcessAsUser error ' + SysErrorMessage(GetLastError));

  CloseHandle(hPrimToken);
  CloseHandle(hProc);
end;

end.
