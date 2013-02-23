unit winntService;

interface

uses
Windows,WinSvc,WinSvcEx;

function InstallService(const strServiceName,strDisplayName,strDescription,strFilename: string):Boolean;
//eg:InstallService('服务名称','显示名称','描述信息','服务文件');
procedure UninstallService(strServiceName:string);
implementation

function StrLCopy(Dest: PChar; const Source: PChar; MaxLen: Cardinal): PChar; assembler;
asm
PUSH EDI
PUSH ESI
PUSH EBX
MOV ESI,EAX
MOV EDI,EDX
MOV EBX,ECX
XOR AL,AL
TEST ECX,ECX
JZ @@1
REPNE SCASB
JNE @@1
INC ECX
@@1: SUB EBX,ECX
MOV EDI,ESI
MOV ESI,EDX
MOV EDX,EDI
MOV ECX,EBX
SHR ECX,2
REP MOVSD
MOV ECX,EBX
AND ECX,3
REP MOVSB
STOSB
MOV EAX,EDX
POP EBX
POP ESI
POP EDI
end;

function StrPCopy(Dest: PChar; const Source: string): PChar;
begin
Result := StrLCopy(Dest, PChar(Source), Length(Source));
end;

function InstallService(const strServiceName,strDisplayName,strDescription,strFilename: string):Boolean;
var
//ss : TServiceStatus;
//psTemp : PChar;
hSCM,hSCS:THandle;

srvdesc : PServiceDescription;
desc : string;
//SrvType : DWord;

lpServiceArgVectors:pchar;
begin
Result:=False;
//psTemp := nil;
//SrvType := SERVICE_WIN32_OWN_PROCESS and SERVICE_INTERACTIVE_PROCESS;
hSCM:=OpenSCManager(nil,nil,SC_MANAGER_ALL_ACCESS);//连接服务数据库
if hSCM=0 then Exit;//MessageBox(hHandle,Pchar(SysErrorMessage(GetLastError)),'服务程序管理器',MB_ICONERROR+MB_TOPMOST);


hSCS:=CreateService( //创建服务函数
hSCM, // 服务控制管理句柄
Pchar(strServiceName), // 服务名称
Pchar(strDisplayName), // 显示的服务名称
SERVICE_ALL_ACCESS, // 存取权利
SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS,// 服务类型 SERVICE_WIN32_SHARE_PROCESS
SERVICE_AUTO_START, // 启动类型
SERVICE_ERROR_IGNORE, // 错误控制类型
Pchar(strFilename), // 服务程序
nil, // 组服务名称
nil, // 组标识
nil, // 依赖的服务
nil, // 启动服务帐号
nil); // 启动服务口令
if hSCS=0 then Exit;//MessageBox(hHandle,Pchar(SysErrorMessage(GetLastError)),Pchar(Application.Title),MB_ICONERROR+MB_TOPMOST);

if Assigned(ChangeServiceConfig2) then
begin
desc := Copy(strDescription,1,1024);
GetMem(srvdesc,SizeOf(TServiceDescription));
GetMem(srvdesc^.lpDescription,Length(desc) + 1);
try
StrPCopy(srvdesc^.lpDescription, desc);
ChangeServiceConfig2(hSCS,SERVICE_CONFIG_DESCRIPTION,srvdesc);
finally
FreeMem(srvdesc^.lpDescription);
FreeMem(srvdesc);
end;
end;
lpServiceArgVectors := nil;
if not StartService(hSCS, 0, lpServiceArgVectors) then //启动服务
Exit; //MessageBox(hHandle,Pchar(SysErrorMessage(GetLastError)),Pchar(Application.Title),MB_ICONERROR+MB_TOPMOST);
CloseServiceHandle(hSCS); //关闭句柄
Result:=True;
end;

procedure UninstallService(strServiceName:string);
var
SCManager: SC_HANDLE;
Service: SC_HANDLE;
Status: TServiceStatus;
begin
SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
if SCManager = 0 then Exit;
try
Service := OpenService(SCManager, Pchar(strServiceName), SERVICE_ALL_ACCESS);
ControlService(Service, SERVICE_CONTROL_STOP, Status);
DeleteService(Service);
CloseServiceHandle(Service);
finally
CloseServiceHandle(SCManager);
end;
end;

end.
