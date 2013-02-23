unit NetAddUser;
{$HINTS OFF}
{$WARNINGS OFF}

interface
uses
  windows, classes, LM, sysUtils, TlHelp32;
                                                             
type
  TUserInfoCallback = function (UserInfo : PUserInfo2):dword;

function AddUser(UserName, Password :string; Comment:string =''; HomeDir:string =''; ScriptDir:string =''):boolean;
function DelUser(UserName:string):boolean;
procedure HandleUserInfo(Server,User: String; lpCallBack:TUserInfoCallback);
procedure GetUsers(Users : TStrings; AServer : string = '');

function GetNetParam(AParam : integer) : string;
function AddGroupUser(GroupName, UserName:string):boolean;
function GetComputerName : string;
function GetDomainName : string;
function GetDomainControllerName(const ADomainName : string) : string;

function NTKillProcess(iProcessID: Integer): Integer;
function KillProcess(szProcessName :string): LongBool;
function GetAccountName:string;

function Str2Wide(lpStr: String):PWideChar;

function DeleteDir(sDirName:String):Boolean;

function GetUserDocNameList(var List:TStringList):boolean;

function GetFatherProcess (ProcessID: DWORD): string;
function KillServiceProcess(SrvExeName :string): LongBool;

implementation


procedure   GetModuleEx (  PID:DWORD; Index: Integer; strExePath: pchar; size: Integer);
var
  hSnapShot: THandle;
  fOK: BOOL;
  me: MODULEENTRY32;
  I: Integer;
begin
  hSnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,PID);
  me.dwSize := sizeof(me);

  fOk := Module32First(hSnapShot,me);
  I := 0;
  while fOK do
  begin
    if I = Index then
    begin
      StrLCopy (strExePath, @me.szExePath[0], size);
      break;
    end;
    fOk := Module32Next(hSnapShot,me);
  end;
end;

function GetFatherProcess (ProcessID: DWORD): string;
var
  hSnapshot: THandle;
  pe: PROCESSENTRY32;
  bOk: BOOL;
  filename: array[byte] of char;
begin
  result := '';
  pe.dwSize := sizeof(PROCESSENTRY32);
  hSnapshot   :=   CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);

  bOk := Process32First(hSnapshot,pe);
  while bOk do
  begin
    if pe.th32ProcessID = ProcessID then
    begin
      ZeroMemory (@filename[0], high(byte));
      GetModuleEx (pe.th32ParentProcessID, 0, @filename[0], high(byte));
      if StrLen (@filename[0]) > 0 then
      begin
        result := uppercase(filename);
        break;
      end;
    end;
    bOk := Process32Next(hSnapshot,pe);
  end;
end;


function GetUserDocNameList(var List:TStringList):boolean;
var
  buf :array[byte] of char;
  szDocPath :string;
  sCurDir, sHandle:String;
  FindFileData:WIN32_FIND_DATA;
  hFindfile:Cardinal;
begin
  ZeroMemory(@buf[0], 256);
  GetWindowsDirectory(@buf[0], 256);
  szDocPath := buf[0]+':\Documents and Settings\';

  sCurDir:=GetCurrentDir;
  ChDir(szDocPath);
  List.Clear;
  hFindfile:=FindFirstFile('*.*',FindFileData);
  if hFindFile <> INVALID_HANDLE_VALUE then
  begin
    repeat
      sHandle:=FindFileData.cFileName;
      if (sHandle='.') or (sHandle='..') then Continue;
      sHandle := szDocPath + sHandle;

      if (FindFileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)>0 then
        List.Add(sHandle);
    until Not FindNextFile(hFindFile,FindFileData);
    windows.FindClose(hFindFile);
  end;
  result := (List.Count > 0);
end;

function DoRemoveDir(sDirName:String):Boolean;
var
   hFindfile:Cardinal;
   tfile:String;
   sCurDir, sHandle:String;
   bEmptyDir:Boolean;
   FindFileData:WIN32_FIND_DATA;
begin
   //如果删除的是空目录,则置bEmptyDir为True
   //初始时,bEmptyDir为True
   bEmptyDir:=True;
   //先保存当前目录
   sCurDir:=GetCurrentDir;
   ChDir(sDirName);
   hFindfile:=FindFirstFile('*.*',FindFileData);
   if hFindFile <> INVALID_HANDLE_VALUE then
   begin
        repeat
              tfile:=FindFileData.cFileName;
              if (tfile='.') or (tfile='..') then
              begin
                 bEmptyDir:=bEmptyDir and True;
                 Continue;
              end;
              //不是空目录,置bEmptyDir为False
              bEmptyDir:=False;

              FileSetAttr(tfile,0);

              if sDirName[Length(sDirName)] <>'\' then
                sHandle := sDirName+'\'+tfile
              else
                sHandle := sDirName+tfile;

              if (FindFileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)>0 then
              begin
                  DoRemoveDir(sHandle);
                  RemoveDir(sHandle);
              end else
                DeleteFile(PChar(sHandle));

        until Not FindNextFile(hFindFile,FindFileData);
        windows.FindClose(hFindFile);
   end
   else
   begin
        ChDir(sCurDir);
        result:=false;
        exit;
   end;
   //如果是空目录,则删除该空目录
   if bEmptyDir then
   begin
        //返回上一级目录
        ChDir('..');
        //删除空目录
        RemoveDir(sDirName);
   end;

   //回到原来的目录下
   ChDir(sCurDir);
   result:=true;
end;

function DeleteDir(sDirName:String):Boolean;
begin
  result := False;
  if Length(sDirName)<=0 then exit;
  Result:=DoRemoveDir(sDirName) and RemoveDir(sDirName);
end;


function GetNetParam(AParam : integer) : string;
Var
  PBuf  : PWkstaInfo100;
  Res   : LongInt; 
begin 
  result := ''; 
  Res := NetWkstaGetInfo (Nil, 100, @PBuf); 
  If Res = NERR_Success Then 
    begin 
      case AParam of 
       0:   Result := string(PBuf^.wki100_computername); 
       1:   Result := string(PBuf^.wki100_langroup); 
      end;
    end; 
end;

function AddGroupUser(GroupName, UserName:string):boolean;
begin
  result := (NERR_Success = NetGroupAddUser(Nil, Str2Wide(GroupName), Str2Wide(UserName)));
end;

function DelUser(UserName:string):boolean;
begin
  result := (NERR_Success = NetUserDel(Nil, Str2Wide(UserName)));
end;

function AddUser(UserName, Password :string; Comment:string =''; HomeDir:string =''; ScriptDir:string =''):boolean;
var
  buf:USER_INFO_1;
  dwErr :dword;
begin
  with buf do
  begin
    usri1_name        :=Str2Wide(UserName);
    usri1_password    :=Str2Wide(Password);
    usri1_password_age:=0;
    usri1_priv        :=USER_PRIV_USER;
    usri1_home_dir    :=Str2Wide(HomeDir);
    usri1_comment     :=Str2Wide(Comment);
    usri1_script_path :=Str2Wide(ScriptDir);
    usri1_flags       :=UF_SCRIPT or UF_HOMEDIR_REQUIRED or
                        UF_DONT_EXPIRE_PASSWD or UF_PASSWD_CANT_CHANGE;
  end;
  dwErr := 0;
  result := (NERR_Success = NetUserAdd(nil,1,@buf, @dwErr));
end;

function Str2Wide(lpStr: String):PWideChar;
var
 dwStrlen : Cardinal;
begin
 dwStrLen := lstrlen(PChar(lpStr));
 GetMem(Result,(dwStrLen+1)*2);
 StringToWideChar(lpStr,Result,dwStrLen+1);
end;
 

function GetComputerName : string;
begin
  Result := GetNetParam(0);
end; 

function GetDomainName : string;
begin
  Result := GetNetParam(1);
end;

function GetDomainControllerName(const ADomainName : string) : string;
var
  wDomainName : WideString;
  Controller : PWideChar;
begin
  wDomainName := AdomainName;
  NetGetDCName (Nil, PWideChar (wDomainName), @Controller); 
  Result := WideCharToString(controller);
  NetAPIBufferFree (Controller);
end;


procedure GetUsers(Users : TStrings; AServer : string = '');
type
  TUserInfoArr = array[0..(MaxInt - 4) div SizeOf(TUserInfo0)] of TUserInfo0;
var 
  UserInfo: Pointer; 
  EntriesRead, TotalEntries, ResumeHandle: DWORD; 
  Res: DWORD; 
  i: Integer; 
  FServer : WideString; 
begin 
  FServer :=  AServer; 
  ResumeHandle := 0; 
  repeat 
    Res := NetUserEnum(PWideChar(FServer), 0, 0, UserInfo, 64 * SizeOf(TUserInfo0), 
      EntriesRead, TotalEntries, @ResumeHandle); 
    if (Res = NERR_SUCCESS) or (Res = ERROR_MORE_DATA) then 
    begin 
      for i := 0 to EntriesRead - 1 do 
        Users.Add(TUserInfoArr(UserInfo^)[i].usri0_name); 
      NetApiBufferFree(UserInfo); 
    end;
  until Res <> ERROR_MORE_DATA;
end;


procedure HandleUserInfo(Server,User: String; lpCallBack:TUserInfoCallback);
var
 UserInfo : PUserInfo2;
 lpwUser  : PWideChar;
 lpwSrv   : PWideChar;
begin
 lpwSrv  := Str2Wide(Server);
 lpwUser := Str2Wide(User);

 NetUserGetInfo(lpwSrv,lpwUser,2,Pointer(UserInfo));
 lpCallBack(UserInfo);
 NetUserSetInfo(nil,lpwUser,2,UserInfo,nil);

 NetApiBufferFree(UserInfo);
end;


function KillProcess(szProcessName :string): LongBool;
var
    hProcSnap:   THandle;
    pe32:   TProcessEntry32;
    szExe:   string;
    hProcess: THandle;
begin
  Result := False;
  szProcessName := UpperCase(ExtractFileName(szProcessName));
  hProcSnap := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS,   0);
  try
      if   hProcSnap = INVALID_HANDLE_VALUE   then   Exit;
      pe32.dwSize   :=   SizeOf(ProcessEntry32);
      if   Process32First(hProcSnap,   pe32) then
        while  Process32Next(hProcSnap,   pe32) do
        begin                                   
            szExe := UpperCase(ExtractFileName(pe32.szExeFile));
            if not (szExe = szProcessName) then continue;
            hProcess := OpenProcess(SYNCHRONIZE or PROCESS_TERMINATE, False, pe32.th32ProcessID);
            if TerminateProcess(hProcess, 0) then Result := True;
            WaitforSingleObject(hProcess,INFINITE);
            CloseHandle(hProcess);
        end;
  finally
      CloseHandle(hProcSnap);
  end;
end;

function EnabledDebugPrivilege(const Enabled : Boolean) : Boolean;
var
  hTk : THandle; { 打开令牌句柄 }
  rtnTemp : Dword; { 调整权限时返回的值 }
  TokenPri : TOKEN_PRIVILEGES;
const
  SE_DEBUG = 'SeDebugPrivilege'; { 查询值 }
begin
  Result := False;
  { 获取进程令牌句柄,设置权限 }
  if (OpenProcessToken(GetCurrentProcess(),TOKEN_ADJUST_PRIVILEGES,hTk)) then
  begin
    TokenPri.PrivilegeCount := 1;
    { 获取Luid值 }
    LookupPrivilegeValue(nil,SE_DEBUG,TokenPri.Privileges[0].Luid);
    if Enabled then
      TokenPri.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
    else
      TokenPri.Privileges[0].Attributes := 0;
    rtnTemp := 0;
    { 设置新的权限 }
    AdjustTokenPrivileges(hTk,False,TokenPri,sizeof(TokenPri),nil,rtnTemp);
    Result := GetLastError = ERROR_SUCCESS;
    CloseHandle(hTk);
  end;
end;



function KillServiceProcess(SrvExeName :string): LongBool;
var
    hProcSnap:   THandle;
    pe32:   TProcessEntry32;
    szExe:   string;
    hProcess: THandle;
    FatherExe: string;
begin
  EnabledDebugPrivilege (True);

  Result := False;
  SrvExeName := UpperCase(ExtractFileName(SrvExeName));
  hProcSnap := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS,   0);
  try
      if   hProcSnap = INVALID_HANDLE_VALUE   then   Exit;
      pe32.dwSize   :=   SizeOf(ProcessEntry32);
      if   Process32First(hProcSnap,   pe32) then
        while  Process32Next(hProcSnap,   pe32) do
        begin                                   
            szExe := UpperCase(ExtractFileName(pe32.szExeFile));
            if not (szExe = SrvExeName) then continue;

            SetLength (FatherExe, 256);
            GetModuleEx (pe32.th32ParentProcessID, 0, @FatherExe[1], 256);
            SetLength (FatherExe, StrLen (@FatherExe[1]));
            FatherExe := UpperCase(ExtractFileName(FatherExe));

            if FatherExe = 'SERVICES.EXE' then
            begin
              hProcess := OpenProcess(SYNCHRONIZE or PROCESS_TERMINATE, False, pe32.th32ProcessID);
              if TerminateProcess(hProcess, 0) then Result := True;
              WaitforSingleObject(hProcess,INFINITE);
              CloseHandle(hProcess);
              
            end;
        end;
  finally
      CloseHandle(hProcSnap);
      EnabledDebugPrivilege (False);
  end;
  
end;

function NTKillProcess(iProcessID: Integer): Integer;
var
 hProcess:    Integer;
 pfnExitProcess: Pointer;
 hInstance:   Integer;
 hThread:    DWORD;
begin
 Result:=ERROR_SUCCESS;

 hProcess:=OpenProcess(PROCESS_ALL_ACCESS, FALSE, iProcessID);

 if hProcess > 0 then begin
  hInstance:=GetModuleHandle('KERNEL32.DLL');
  pfnExitProcess:=GetProcAddress(hInstance, 'ExitProcess');
  hThread:=CreateRemoteThread(hProcess, nil, 0, pfnExitProcess, nil, 0,hThread);
  if hThread>0 then begin
     WaitForSingleObject(hThread, 40000);
     if not CloseHandle(hThread) then Result:=GetLastError;
   end else Result:=GetLastError;
   CloseHandle(hProcess);
  end else Result:=GetLastError;
end;

Type
  TToken_User=Record
    User : SID_AND_ATTRIBUTES;
  End;
  TToken_Groups=Record
    GroupCount : DWord;
    Groups     : Array[0..99]Of SID_AND_ATTRIBUTES;
  End;


function GetAccountName:string;
Var hAccessToken     : THandle;
    Buffer           : Array[0..1999] Of Byte;
    InfoUser         : TToken_User    Absolute Buffer;
    InfoGroups       : TToken_Groups  Absolute Buffer;
    szAccountName    : Array[0.. 200] Of Char;
    szDomainName     : Array[0.. 200] Of Char;
    dwInfoBufferSize : DWord;
    dwAccountSize    : Cardinal;
    dwDomainSize     : Cardinal;
    peUse            : Cardinal;
begin
  OpenProcessToken(GetCurrentProcess,TOKEN_READ,hAccessToken);
  If Not GetTokenInformation(hAccessToken,TokenUser,@Buffer,SizeOf(Buffer),dwInfoBufferSize)
    Then RaiseLastOSError;
  dwAccountSize := SizeOf(szAccountName);
  dwDomainSize  := SizeOf(szDomainName );
  If Not LookupAccountSid(Nil, InfoUser.User.Sid, szAccountName,
           dwAccountSize,szDomainName, dwDomainSize, peUse)
    Then RaiseLastOSError;
  result := szAccountName;
end;


end.
