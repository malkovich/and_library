unit DropMyRights;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes;

const
  SAFER_SCOPEID_MACHINE = 1;
  SAFER_SCOPEID_USER    = 2;

  SAFER_LEVELID_FULLYTRUSTED = $40000;
  SAFER_LEVELID_NORMALUSER   = $20000;
  SAFER_LEVELID_CONSTRAINED  = $10000;
  SAFER_LEVELID_UNTRUSTED    = $01000;
  SAFER_LEVELID_DISALLOWED   = $00000;

function DropMe (RightLever: Integer; CommandLine: String): DWORD;
function DropMeSuspend (RightLever: Integer; CommandLine: String; var pi: PROCESS_INFORMATION): BOOL;

function NewSA: PSecurityAttributes;
procedure DisposeSA (sa: PSecurityAttributes);

implementation

function NewSA: PSecurityAttributes;
var
  SecurityDescriptor: PSecurityDescriptor;
begin
  New (SecurityDescriptor);
  New (Result);
  InitializeSecurityDescriptor(SecurityDescriptor,SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDACL(SecurityDescriptor,true,nil,false);
  Result.nLength := sizeof(SECURITY_ATTRIBUTES);
  Result.lpSecurityDescriptor := SecurityDescriptor;
  Result.bInheritHandle := false;
end;

procedure DisposeSA (sa: PSecurityAttributes);
begin
  Dispose (sa.lpSecurityDescriptor);
  Dispose (sa);
end;

type
  SAFER_LEVEL_HANDLE = THANDLE;
  PSAFER_LEVEL_HANDLE = ^SAFER_LEVEL_HANDLE;
  TSaferLevelHandle = SAFER_LEVEL_HANDLE;
  PSaferLevelHandle = PSAFER_LEVEL_HANDLE;
  LPVOID = Pointer;  

function SaferCreateLevel(dwScopeId, dwLevelId, OpenFlags: DWORD; pLevelHandle: PSAFER_LEVEL_HANDLE; lpReserved: LPVOID): BOOL; stdcall; external advapi32 name 'SaferCreateLevel';
function SaferCloseLevel(hLevelHandle: SAFER_LEVEL_HANDLE): BOOL; stdcall; external advapi32 name 'SaferCloseLevel';
function SaferComputeTokenFromLevel(LevelHandle: SAFER_LEVEL_HANDLE; InAccessToken: THANDLE; OutAccessToken: PHANDLE; dwFlags: DWORD; lpReserved: LPVOID): BOOL; stdcall; external advapi32 name 'SaferComputeTokenFromLevel';

function DropMe (RightLever: Integer; CommandLine: String): DWORD;
var
  hAuthzLevel: SAFER_LEVEL_HANDLE;
  hToken: THandle;
  si:STARTUPINFO;
  pi: PROCESS_INFORMATION;
begin
  Result := ERROR_SUCCESS;
  hAuthzLevel := 0;
  hToken := 0;
  ZeroMemory(@si, sizeof(STARTUPINFO));
  si.cb := sizeof(STARTUPINFO);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_SHOWNORMAL;

  Repeat
    if not SaferCreateLevel (SAFER_SCOPEID_USER, RightLever, 0, @hAuthzLevel, nil) then Break;
    if not SaferComputeTokenFromLevel (hAuthzLevel, 0, @hToken, 0, nil) then Break;
    if not CreateProcessAsUser(hToken, NIL, PAnsiChar(CommandLine), nil, nil, False, CREATE_NEW_CONSOLE, nil, NIL, si, pi) then Break;

    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
    SaferCloseLevel(hAuthzLevel);
    Exit;
  until True;

  if hAuthzLevel > 0 then SaferCloseLevel(hAuthzLevel);
  Result := GetLastError;
end;

function DropMeSuspend (RightLever: Integer; CommandLine: String; Var pi: PROCESS_INFORMATION): BOOL;
var
  hAuthzLevel: SAFER_LEVEL_HANDLE;
  hToken: THandle;
  si:STARTUPINFO;
begin
  Result := False;
  hAuthzLevel := 0;
  hToken := 0;
  ZeroMemory(@si, sizeof(STARTUPINFO));
  si.cb := sizeof(STARTUPINFO);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_SHOWNORMAL;

  Repeat
    if not SaferCreateLevel (SAFER_SCOPEID_USER, RightLever, 0, @hAuthzLevel, nil) then Break;
    if not SaferComputeTokenFromLevel (hAuthzLevel, 0, @hToken, 0, nil) then Break;
    if not CreateProcessAsUser(hToken, NIL, PAnsiChar(CommandLine), nil, nil, False, CREATE_SUSPENDED or NORMAL_PRIORITY_CLASS, nil, NIL, si, pi) then Break;
    SaferCloseLevel(hAuthzLevel);
    Result := True;
    Exit;
  until True;

  if hAuthzLevel > 0 then SaferCloseLevel(hAuthzLevel);
end;



end.
