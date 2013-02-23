unit ServiceManager;

interface
uses windows, Winsvc, sysUtils, classes;

var
  Status: TServiceStatus;
  
function InstallService(ServiceName, DisplayName: pchar; FileName: string): Longbool;
function UninstallService(ServiceName: pchar): Longbool;
function StartServices (ServiceName: string): LongBool;
function StopServices (ServiceName: string): LongBool;

function GetServiceState (DisplayName: String): DWORD;

function ShellOperate (CmdLine: String; var RetStrs: TStringList): LongBool;

implementation       

function ShellOperate (CmdLine: String; var RetStrs: TStringList): LongBool;
var
    hreadpipe,hwritepipe:thandle;
    si:startupinfo;   
    lsa:security_attributes;
    pi:process_information;
    cchreadbuffer:dword;
    ph:pchar;
    fname:pchar;
    ReturnStr: String;
begin
    Result := False;

    fname := allocmem(255);
    ph := allocmem(5000);
    lsa.nlength := sizeof(security_attributes);
    lsa.lpsecuritydescriptor := nil;
    lsa.binherithandle := true;
    
    if   createpipe(hreadpipe,hwritepipe,@lsa,0)=false   then   
    begin
        RetStrs.Add('ERROR: can not create pipe!');
        exit;
    end;   
    fillchar(si,sizeof(startupinfo),0);   
    si.cb   :=sizeof(startupinfo);   
    si.dwflags   :=(startf_usestdhandles   or   startf_useshowwindow);   
    si.wshowwindow   :=sw_hide;   
    si.hstdoutput   :=hwritepipe;

    strpcopy(fname, PChar(CmdLine));
    
    if createprocess( nil, fname, nil, nil, true, 0, nil, nil, si, pi) = false then
    begin   
        RetStrs.Add('ERROR: can not create process');
        freemem(ph);
        freemem(fname);   
        exit;
    end;

    while true do
    begin
        if not peeknamedpipe(hreadpipe,ph,1,@cchreadbuffer,nil,nil) then break;
        if cchreadbuffer <> 0 then
        begin
          if readfile(hreadpipe,ph^,4096,cchreadbuffer,nil)=false then break;
          ph[cchreadbuffer]:=chr(0);
          ReturnStr := Trim(StrPas(ph));
          if Length(ReturnStr) > 0  then
            RetStrs.Add(ReturnStr);
        end else
          if (waitforsingleobject(pi.hprocess ,0)=wait_object_0) then break;
        sleep(100);   
    end;
    
    ph[cchreadbuffer]:=chr(0);
    ReturnStr := Trim(StrPas(ph));
    if Length(ReturnStr) > 0  then
      RetStrs.Add(ReturnStr);

    closehandle(hreadpipe);   
    closehandle(pi.hthread);   
    closehandle(pi.hprocess);   
    closehandle(hwritepipe);   
    freemem(ph);   
    freemem(fname);

    Result := True;
end;


type
  TEnumProc=function(Sender:Pointer; ESS: TEnumServiceStatus): LongBool;

procedure EnumServices(Sender:Pointer; ESSProc: TEnumProc);
type
  TESS = array[0..4096] of TEnumServiceStatus;
  PESS = ^TESS;
var
  pSvc: PESS;
  hSCManager: SC_HANDLE;
  nRetByte, nRetNum, nRetResume: DWORD;
  i: Integer;
begin
  hSCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if hSCManager <= 0  then
    Exit;

  nRetResume := 0;

  New(pSvc);
  EnumServicesStatus(hSCManager, SERVICE_WIN32, SERVICE_STATE_ALL, pSvc^[0], SizeOf(pSvc^),
                    nRetByte, nRetNum, nRetResume);

  for i := 0 to nRetNum - 1 do
   if not ESSProc (Sender, pSvc^[i]) then break;

  Dispose(pSvc);
  CloseServiceHandle(hSCManager);
end;

Type
  LPTEnumResult = ^TEnumResult;
  TEnumResult = record
    DisplayName: string[64];
    dwCurrentState: DWORD;
  end;

function EnumProce(Sender:Pointer; ESS: TEnumServiceStatus): LongBool;
var
  ER: LPTEnumResult absolute Sender;
begin
  Result := True;
  if ER.DisplayName =  StrPas(ESS.lpDisplayName) then
  begin
    ER.dwCurrentState := ESS.ServiceStatus.dwCurrentState;
    Result := False;
  end;
end;

function GetServiceState (DisplayName: String): DWORD;
var
  EnumResult: TEnumResult;
begin
  EnumResult.DisplayName := DisplayName;
  EnumResult.dwCurrentState := 0;
  EnumServices (@EnumResult, EnumProce);
  Result := EnumResult.dwCurrentState;
end;



function UninstallService(ServiceName: pchar): Longbool;
var
  SCManager: SC_HANDLE;
  Service: SC_HANDLE;
begin
  Result := False;
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then Exit;
  try
    Service := OpenService(SCManager, ServiceName, SERVICE_ALL_ACCESS);
    ControlService(Service, SERVICE_CONTROL_STOP, Status);
    Result := DeleteService(Service);
    CloseServiceHandle(Service);
  finally
    CloseServiceHandle(SCManager);
  end;
end;

function InstallService(ServiceName, DisplayName: pchar; FileName: string): Longbool;
var
  SCManager: SC_HANDLE;
  Service: SC_HANDLE;
//  Args: pchar;
begin
  Result := False;
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then Exit;
  try
    Service := CreateService(SCManager, ServiceName, DisplayName, SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS, SERVICE_AUTO_START, SERVICE_ERROR_IGNORE, pchar(FileName), nil, nil, nil, nil, nil);
    if Service = 0 then Exit;
//
//    Args := nil;
//    Result := StartService(Service, 0, Args);

    Result := True;
    CloseServiceHandle(Service);
  finally
    CloseServiceHandle(SCManager);
  end;
end;

function StartServices (ServiceName: string): LongBool;
var
  SCManager: SC_HANDLE;
  Service: SC_HANDLE;
  isSuccess: LongBool;
  lpServiceArgVectors: PChar;
begin
  Result := False; 
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then Exit;
  try
    Service := OpenService(SCManager, PChar(ServiceName), SERVICE_ALL_ACCESS);
    if Service = 0 then exit;

    isSuccess := QueryServiceStatus (Service, Status);
    if not isSuccess then exit;

    if Status.dwCurrentState = SERVICE_STOPPED then
    begin
      isSuccess := StartService (service, 0, lpServiceArgVectors); //Æô¶¯·þÎñ
      if not isSuccess then exit;
    end;

    Result := True;
  finally
    CloseServiceHandle(SCManager);
  end;
end;

function StopServices (ServiceName: string): LongBool;
var
  SCManager: SC_HANDLE;
  Service: SC_HANDLE;
  isSuccess: LongBool;
begin
  Result := False; 
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then Exit;
  try
    Service := OpenService(SCManager, PChar(ServiceName), SERVICE_ALL_ACCESS);
    if Service = 0 then exit;

    isSuccess := QueryServiceStatus (Service, Status);
    if not isSuccess then exit;

    if Status.dwCurrentState <> SERVICE_STOPPED then
    begin
      isSuccess := ControlService (service,SERVICE_CONTROL_STOP,status);
      if not isSuccess then exit;
      Sleep (500);
    end;

    Result := True;
  finally
    CloseServiceHandle(SCManager);
  end;

end;


end.
