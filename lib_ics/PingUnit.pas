unit PingUnit;

interface
uses
  windows, classes, ping, syncObjs, forms, SysUtils, Winsock;

Type
  TPingClass = class(TPing)
  protected
    Event: TEvent;
    FStatus : Integer;
    FTimeout: DWORD;
    procedure OnMyEchoReply(Sender: TObject; Icmp: TObject; Status : Integer);
    procedure DnsLookupDone(Sender: TObject; Error: Word);
  public
    constructor Create(Owner : TComponent); override;
    function PingHost(Host: string; Timeout:DWORD = 8888): Integer;
  end;


var
  PingModule: TPingClass;

function WinPingI (HostAddr: String): Integer;
function WinPingS (HostAddr: String): String;
function WinPingB (HostAddr: String): LongBool;

implementation

uses icmp, DbgLoger, madShell;

function Quick_WinPing (HostAddr: String; var Addr: TInAddr; var IP: String): LongBool ;
var
    hreadpipe,hwritepipe:thandle;
    si:startupinfo;   
    lsa:security_attributes;   
    pi:process_information;
    CmdLine:string;
    cchreadbuffer:dword;
    ph:pchar;
    fname:pchar;
    ReplySL: TStringList;
    I: Integer;
    BeginTick: DWORD;
begin
    DBG ('使用windows的Ping ：%s', [HostAddr]);
    BeginTick := GetTickCount;

    Result := False;
    IP := '';
    Addr.S_addr := -1;

    fname := allocmem(255);
    ph := allocmem(5000);
    lsa.nlength := sizeof(security_attributes);
    lsa.lpsecuritydescriptor := nil;
    lsa.binherithandle := true;
    
    if   createpipe(hreadpipe,hwritepipe,@lsa,0)=false   then   
    begin
        DBG('can not create pipe!');
        exit;   
    end;   
    fillchar(si,sizeof(startupinfo),0);   
    si.cb   :=sizeof(startupinfo);   
    si.dwflags   :=(startf_usestdhandles   or   startf_useshowwindow);   
    si.wshowwindow   :=sw_hide;   
    si.hstdoutput   :=hwritepipe;
    CmdLine := SysFolder + '\ping.exe ' + HostAddr;
    strpcopy(fname, PChar(CmdLine));
    
    if createprocess(nil, fname, nil, nil, true, 0, nil, nil, si, pi) = false then
    begin   
        DBG('can not create process');
        freemem(ph);
        freemem(fname);   
        exit;
    end;

    ReplySL := TStringList.Create;

    while true do
    begin
        if not PeekNamedPipe(hreadpipe,ph,1,@cchReadBuffer,nil,nil) then break;

        if cchReadBuffer <> 0 then
        begin
          if readfile(hreadpipe,ph^,4096,cchreadbuffer,nil)=false then break;
          ph[cchreadbuffer]:=chr(0);

          ReplySL.Clear;
          if ExtractStrings ([' ',':','[',']'], [' '], @ph[0], ReplySL) > 0 then
          begin
                for I := 0 to ReplySL.Count - 1 do
                begin
                        CmdLine := ReplySL[I];
                        if Length (CmdLine) < 7 then continue;

                        Addr.S_addr := Inet_addr(PChar(CmdLine));
                        if Addr.S_addr = INADDR_NONE then continue;

                        IP := CmdLine;
                        Result := True;
                        Break;
                end;
          end;

          if Result then
          begin
            TerminateProcess(pi.hprocess, 0);
            Break;
          end;

        end else
          if (waitforsingleobject(pi.hprocess ,0)=wait_object_0) then break;
        sleep(100);
    end;

    ReplySL.Free;

    closehandle(hreadpipe);
    closehandle(pi.hthread);
    closehandle(pi.hprocess);
    closehandle(hwritepipe);
    freemem(ph);
    freemem(fname);

    DBG ('使用windows的Ping ：%s 完毕! use %d ms', [HostAddr, GetTickCount - BeginTick]);
end;

function Full_WinPing (HostAddr: String; var Addr: TInAddr; var IP: String): LongBool ;
var
    hreadpipe,hwritepipe:thandle;
    si:startupinfo;   
    lsa:security_attributes;
    pi:process_information;
    CmdLine:string;
    cchreadbuffer:dword;
    ph:pchar;
    fname:pchar;
    ReplySL: TStringList;
    I: Integer;
begin
    DBG ('使用windows的Ping ：%s', [HostAddr]);

    Result := False;
    IP := '';
    Addr.S_addr := -1;

    fname := allocmem(255);
    ph := allocmem(5000);
    lsa.nlength := sizeof(security_attributes);
    lsa.lpsecuritydescriptor := nil;
    lsa.binherithandle := true;
    
    if   createpipe(hreadpipe,hwritepipe,@lsa,0)=false   then   
    begin
        DBG('can not create pipe!');
        exit;   
    end;   
    fillchar(si,sizeof(startupinfo),0);   
    si.cb   :=sizeof(startupinfo);   
    si.dwflags   :=(startf_usestdhandles   or   startf_useshowwindow);   
    si.wshowwindow   :=sw_hide;   
    si.hstdoutput   :=hwritepipe;
    CmdLine := SysFolder + '\ping.exe ' + HostAddr;
    strpcopy(fname, PChar(CmdLine));
    
    if   createprocess(   nil,   fname,   nil,   nil,   true,   0,   nil,   nil,   si,   pi)   =   false     then   
    begin   
        DBG('can not create process');
        freemem(ph);
        freemem(fname);   
        exit;
    end;

    ReplySL := TStringList.Create;
    
    while true do
    begin
        if not peeknamedpipe(hreadpipe,ph,1,@cchreadbuffer,nil,nil) then break;
        if cchreadbuffer <> 0 then
        begin
          if readfile(hreadpipe,ph^,4096,cchreadbuffer,nil)=false then break;
          ph[cchreadbuffer]:=chr(0);
          ReplySL.Add(StrPas(ph));
        end else
          if (waitforsingleobject(pi.hprocess ,0)=wait_object_0) then break;
        sleep(100);   
    end;
    
    ph[cchreadbuffer]:=chr(0);
    ReplySL.Add(StrPas(ph));

    closehandle(hreadpipe);   
    closehandle(pi.hthread);   
    closehandle(pi.hprocess);   
    closehandle(hwritepipe);   
    freemem(ph);   
    freemem(fname);

    CmdLine := ReplySL.Text;
    ReplySL.Clear;
    if ExtractStrings ([' ',':','[',']'], [' '], PChar(CmdLine), ReplySL) > 0 then
    begin
      for I := ReplySL.Count - 1 downto 0 do
      begin
        CmdLine := ReplySL[I];
        if Length (CmdLine) < 7 then
        begin
          ReplySL.Delete(I);
          continue;
        end;
        Addr.S_addr := Inet_addr(PChar(CmdLine));
        if Addr.S_addr = INADDR_NONE then
        begin
          ReplySL.Delete(I);
          continue;
        end;
        IP := CmdLine;
        Result := True;
        Break;
      end;
    end;

    ReplySL.Free;
end;

function WinPingI (HostAddr: String): Integer;
var
  Addr: TInAddr;
  IP: String;
begin
  Result := -1;
  if Quick_WinPing(HostAddr, Addr, IP) then
    Result := Addr.S_addr;
end;

function WinPingS (HostAddr: String): String;
var
  Addr: TInAddr;
  IP: String;
begin
  Result := '';
  if Quick_WinPing(HostAddr, Addr, IP) then
    Result := IP;
end;

function WinPingB (HostAddr: String): LongBool;
var
  Addr: TInAddr;
  IP: String;
begin
  Result := Quick_WinPing(HostAddr, Addr, IP);
end;

procedure TPingClass.OnMyEchoReply(Sender: TObject; Icmp: TObject; Status : Integer);
begin
  FStatus := Status;
  DBG('Ping EchoReply Result : '+ IntToStr(FStatus));
  Event.SetEvent;
end;

procedure TPingClass.DnsLookupDone(Sender: TObject; Error: Word);
begin
  if Error <> IP_SUCCESS then
  begin
    DBG('Ping DnsLookupError=' + GetErrorString+ ' DnsResult=' +DnsResult) ;
    Event.SetEvent;
    Exit;
  end;

  Address := DnsResult;
  Ping;
end;

function TPingClass.PingHost(Host: string; Timeout:DWORD = 8888): Integer;
var
  TickCount: DWORD;
begin
  FStatus := -1;
  FTimeout := Timeout;
  DBG('Ping Star DnsLookup=' + Host);
  DnsLookup(Host);
  TickCount := GetTickCount;
  Event.ResetEvent;
  repeat
    if wrSignaled = Event.WaitFor(100) then break;
    application.ProcessMessages;
  until GetTickCount - TickCount > FTimeout;
  result := FStatus;
end;

constructor TPingClass.Create(Owner : TComponent);
begin
  inherited Create(Owner);
  Flags := IP_FLAG_DF;
  Event := TEvent.Create(nil, False, False, '');
  OnDnsLookupDone := DnsLookupDone;
  OnEchoReply := OnMyEchoReply;
end;



initialization
PingModule := TPingClass.Create(Nil);

finalization
PingModule.Free;

end.
