unit RunConsoleProceUnit;

interface
uses windows, classes;

function RunProcessWithOneParamData (
                     ExeFile: String;
                     ParmaBase: Pointer; ParamSize: Integer;
                     var ExitCode: DWORD;
                     ReplySL: TStringList = nil): BOOL; Stdcall;


function AppCmdLine (ExeFile: String; ParmaBase: Pointer; ParamSize: Integer; TimeOut:DWORD=60*1000): String;
function GetAppResult (ExeFile: String; ParamBase: Pointer; ParamSize: Integer; var ExitCode: DWORD): BOOL; overload;
function GetAppResult (ExeFile: String; ParamStr: String; var ExitCode: DWORD): BOOL; overload;

implementation
uses Sysutils, Base64Unit, RunAsUserUnit;

function GetAppResult (ExeFile: String; ParamStr: String; var ExitCode: DWORD): BOOL;
var
  CmdLine, SubProcDir: String;
  si: TStartupInfo;
  pi: TProcessInformation;
begin
  Result := False;
  CmdLine := ExeFile + ' ' + ParamStr;

  //创建进程
  fillchar(si,sizeof(TStartupInfo),0);
  si.cb   :=sizeof(TStartupInfo);
  si.dwflags   := startf_useshowwindow;
  si.wshowwindow   :=SW_HIDE;
  SubProcDir := ExtractFilePath(ExeFile);

  if not createprocess(nil, PChar(CmdLine), nil, nil, true, CREATE_NEW_CONSOLE, nil, PChar(SubProcDir), si, pi) then
  begin
    ExitCode := GetLastError;
    exit;
  end;

  if WAIT_OBJECT_0 = WaitForSingleObject (pi.hprocess ,INFINITE) then
    Result := GetExitCodeProcess (pi.hprocess, ExitCode);

  closehandle(pi.hthread);
  closehandle(pi.hprocess);
end;

function GetAppResult (ExeFile: String; ParamBase: Pointer; ParamSize: Integer; var ExitCode: DWORD): BOOL;
var
  CmdLine, SubProcDir: String;
  si:startupinfo;
  pi:process_information;
begin
  Result := False;
  CmdLine := ExeFile + ' ' + EncodeBase64 (ParamBase^, ParamSize);

  //创建进程
  fillchar(si,sizeof(startupinfo),0);
  si.cb   :=sizeof(startupinfo);
  si.dwflags   := startf_useshowwindow;
  si.wshowwindow   :=SW_HIDE;
  SubProcDir := ExtractFilePath(ExeFile);

  if not createprocess(nil, PChar(CmdLine), nil, nil, true, CREATE_NEW_CONSOLE, nil, PChar(SubProcDir), si, pi) then
  begin
    ExitCode := GetLastError;
    exit;
  end;

  if WAIT_OBJECT_0 = WaitForSingleObject (pi.hprocess ,INFINITE) then
    Result := GetExitCodeProcess (pi.hprocess, ExitCode);

  closehandle(pi.hthread);
  closehandle(pi.hprocess);
end;

function AppCmdLine (ExeFile: String; ParmaBase: Pointer; ParamSize: Integer; TimeOut:DWORD=60*1000): String;
var
    hreadpipe,hwritepipe:thandle;
    si:startupinfo;
    lsa:security_attributes;
    pi:process_information;
    CmdLine, ReplyStr:string;
    cchreadbuffer:dword;
    ReadChar: Char;
    BeginTick: DWORD;
    SubProcDir: String;
begin
    Result := '';

    //创建管道
    lsa.nlength := sizeof(SECURITY_ATTRIBUTES);
    lsa.lpsecuritydescriptor := nil;
    lsa.binherithandle := true;
    if not createpipe(hReadpipe,hWritepipe,@lsa,0) then exit;

    //创建进程
    fillchar(si,sizeof(startupinfo),0);
    si.cb   :=sizeof(startupinfo);
    si.dwflags   :=(startf_usestdhandles   or   startf_useshowwindow);
    si.wshowwindow   :=SW_HIDE;
    si.hstdoutput   :=hwritepipe;
    CmdLine := ExeFile + ' ' + EncodeBase64 (ParmaBase^, ParamSize);
    SubProcDir := ExtractFilePath(ExeFile);
    if not createprocess(nil, PChar(CmdLine), nil, nil, true, CREATE_NEW_CONSOLE, nil, PChar(SubProcDir), si, pi) then exit;

    //读取运行信息，直到进程结束
    ReplyStr := '';
    BeginTick := GetTickCount;
    while true do
    begin
      if not PeekNamedPipe(hReadPipe, @ReadChar, 1, @cchreadbuffer,nil,nil) then break;
      if cchreadbuffer <> 0 then
      begin
        if not ReadFile(hReadPipe, ReadChar, 1,cchreadbuffer,nil) then break;

        ReplyStr := ReplyStr + ReadChar;
        if ReadChar = #$D then
        begin
          Result := Trim (ReplyStr);
        end;
      end else
      begin
        if Result <> '' then
        if WAIT_OBJECT_0 = WaitForSingleObject (pi.hprocess ,888) then
          break;

        sleep (10);

        if GetTickCount - BeginTick > TimeOut then
        begin
          TerminateProcess (pi.hprocess, 0);
          Break;
        end;
      end;
    end;

    //关闭管道和回收资源
    closehandle(hreadpipe);
    closehandle(pi.hthread);
    closehandle(pi.hprocess);
    closehandle(hwritepipe);   
end;


function RunProcessWithOneParamData (ExeFile: String; ParmaBase: Pointer; ParamSize: Integer;
                     var ExitCode: DWORD;
                     ReplySL: TStringList = nil): BOOL; Stdcall;
var
    hreadpipe,hwritepipe:thandle;
    si:startupinfo;
    lsa:security_attributes;
    pi:process_information;
    CmdLine, ReplyStr:string;
    cchreadbuffer:dword;
    ReadChar: Char;
    BeginTick: DWORD;
    SubProcDir: String;
begin
    Result := False;

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
    fillchar(si,sizeof(startupinfo),0); 
    si.cb   :=sizeof(startupinfo);      
    si.dwflags   :=(startf_usestdhandles   or   startf_useshowwindow);
    si.wshowwindow   :=SW_HIDE;
    si.hstdoutput   :=hwritepipe;
    CmdLine := ExeFile + ' ' + EncodeBase64 (ParmaBase^, ParamSize);
    SubProcDir := ExtractFilePath(ExeFile);
    if not createprocess(nil, PChar(CmdLine), nil, nil, true, CREATE_NEW_CONSOLE, nil, PChar(SubProcDir), si, pi) then
    begin
      ExitCode := GetLastError;
      exit;
    end;

    //读取运行信息，直到进程结束
    if Assigned (ReplySL) then
      ReplySL.Clear;
    ReplyStr := '';
    BeginTick := GetTickCount;
    while true do
    begin
      if not PeekNamedPipe(hReadPipe, @ReadChar, 1, @cchreadbuffer,nil,nil) then break;
      if cchreadbuffer <> 0 then
      begin
        if ReadFile(hReadPipe, ReadChar, 1,cchreadbuffer,nil)=false then break;
        
        if Assigned (ReplySL) then
        begin
          ReplyStr := ReplyStr + ReadChar;
          if ReadChar = #$D then
          begin
            ReplySL.Add(Trim(ReplyStr));
            ReplyStr := '';
          end;
        end;

      end else
      begin  
        if WAIT_OBJECT_0 = WaitForSingleObject (pi.hprocess ,888) then
        begin
          Result := GetExitCodeProcess (pi.hprocess, ExitCode);
          break;
        end;

        if GetTickCount - BeginTick > 2*60*1000 then
        begin
          ExitCode := DWORD(-1);
          if not TerminateProcess (pi.hprocess, ExitCode) then
            ExitCode := GetLastError;
          Break;
        end;
      end;
    end;

    if Assigned (ReplySL) then
      ReplySL.Add(ReplyStr);

    //关闭管道和回收资源
    closehandle(hreadpipe);   
    closehandle(pi.hthread);   
    closehandle(pi.hprocess);   
    closehandle(hwritepipe);   
end;


end.
