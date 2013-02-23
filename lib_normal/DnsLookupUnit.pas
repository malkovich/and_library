unit DnsLookupUnit;

interface
uses winsock, classes, windows, SysUtils, WSocket, SyncObjs, forms , PingUnit;

function GetSockAddrIn ( HostAddr, HostPort: String ): SockAddr_in;

function atoi(Value : String) : Integer;

implementation

const
  DNS_LOOKUP_INTERNAL = 1000 * 60 * 60;
  REQUEST_TIME_OUT = 1000 * 8;

type
  LPTAddrInfo = ^TAddrInfo;
  TAddrInfo = record
    CheckTime: DWORD;
    HostAddr: string[64];
    IPAddr: TInAddr;
  end;

Type
  TDnsLookuper = class
  private
    WSO: TWSocket;
    IPAddrResult: TInAddr;
    IsDone: LongBool;
    CriticalSection : TCriticalSection;
    procedure WSocketDnsLookupDone(Sender: TObject;  Error: Word);
  public
    constructor Create;
    Destructor Destroy; override;
    function GetSockAddrIn ( HostAddr, HostPort: String ): SockAddr_in;
  end;

function atoi(Value : String) : Integer;
var
    I : Integer;
begin
    Result := 0;
    I      := 1;
    while (I <= Length(Value)) and (Value[I] = ' ') do
        Inc(I);
    while (I <= Length(Value)) and (Value[I] >= '0') and (Value[I] <= '9')do begin
        Result := Result * 10 + ord(Value[I]) - ord('0');
        Inc(I);
    end;
end;

constructor TDnsLookuper.Create;
begin
  WSO := TWSocket.Create(nil);
  WSO.OnDnsLookupDone := WSocketDnsLookupDone;
  CriticalSection := TCriticalSection.Create;
end;

Destructor TDnsLookuper.Destroy;
begin
  inherited;
  WSO.Free;
  CriticalSection.Free;
end;

procedure TDnsLookuper.WSocketDnsLookupDone(Sender: TObject;  Error: Word);
begin
  if Error = 0 then
    IPAddrResult.S_addr := WSocket_inet_addr(PChar(WSO.DnsResult));

  IsDone := True;
end;

function TDnsLookuper.GetSockAddrIn ( HostAddr, HostPort: String ): SockAddr_in;
var
  BeginTick: DWORD;
begin
  CriticalSection.Enter;

  IPAddrResult.S_addr := INADDR_NONE;

  IsDone := False;
  WSO.DnsLookup(HostAddr);

  BeginTick := GetTickCount;
  repeat
    sleep(100);
    Application.ProcessMessages;
    if IsDone then break;
  until GetTickCount - BeginTick > REQUEST_TIME_OUT;

  Result.sa_family := AF_INET;
  Result.sin_addr := IPAddrResult;
  Result.sin_port := htons(atoi(HostPort));

  CriticalSection.Leave;
end;


var
  DnsLookuper: TDnsLookuper;
  AddrList: TList;           

function GetSockAddrIn ( HostAddr, HostPort: String ): SockAddr_in;
var
  AddrInfo: LPTAddrInfo;
  I: Integer;
begin
  if not assigned(AddrList) then
  begin
    AddrList := TList.Create;
    DnsLookuper := TDnsLookuper.Create;
  end;

  for I := 0 to AddrList.Count - 1 do
  begin
    AddrInfo := AddrList[I];
    if GetTickCount - AddrInfo.CheckTime < DNS_LOOKUP_INTERNAL then
    begin
      if Addrinfo.HostAddr = HostAddr then
      begin
        Result.sa_family := AF_INET;
        Result.sin_addr := Addrinfo.IPAddr;
        Result.sin_port := htons(atoi(HostPort));
        exit;
      end;
    end;
  end;

  Result := DnsLookuper.GetSockAddrIn( HostAddr, HostPort);

  if Result.sin_addr.S_addr = INADDR_NONE then
    Result.sin_addr.S_addr := WinPingI (HostAddr);

  if Result.sin_addr.S_addr <> INADDR_NONE then
  begin
    for I := 0 to AddrList.Count - 1 do
    begin
      AddrInfo := AddrList[I];
      if Addrinfo.HostAddr = HostAddr then
      begin
        Addrinfo.CheckTime := GetTickCount;
        Addrinfo.IPAddr := Result.sin_addr;
        exit;
      end;
    end;

    AddrInfo := AllocMem(SizeOf(TAddrInfo));
    AddrInfo.CheckTime := GetTickCount;
    AddrInfo.HostAddr := HostAddr;
    AddrInfo.IPAddr := Result.sin_addr;

    AddrList.Add(AddrInfo);
  end;  
end;

end.
