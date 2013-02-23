unit NetworkParamsImpl;

interface

uses Windows, Classes, WinSock;

const
  MAX_HOSTNAME_LEN               = 128;
  MAX_DOMAIN_NAME_LEN            = 128;
  MAX_SCOPE_ID_LEN               = 256;
  ANY_SIZE                       = 1;

type
  IPAddr = Cardinal;
  IPMask = Cardinal;
  
  PIP_ADDRESS_STRING = ^IP_ADDRESS_STRING;
  IP_ADDRESS_STRING = record
    S: array [0..15] of Char;
  end;

  IP_MASK_STRING = IP_ADDRESS_STRING;

  PIP_ADDR_STRING = ^IP_ADDR_STRING;
  IP_ADDR_STRING = record
    Next:      PIP_ADDR_STRING;
    IpAddress: IP_ADDRESS_STRING;
    IpMask:    IP_MASK_STRING;
    Context:   DWORD;
  end;

  PFIXED_INFO = ^FIXED_INFO;
  FIXED_INFO = record
    HostName:         array[0..MAX_HOSTNAME_LEN + 3] of Char;
    DomainName:       array[0..MAX_DOMAIN_NAME_LEN + 3] of Char;
    CurrentDnsServer: PIP_ADDR_STRING;
    DnsServerList:    IP_ADDR_STRING;
    NodeType:         UINT;
    ScopeId:          array [0..MAX_SCOPE_ID_LEN + 3] of Char;
    EnableRouting:    UINT;
    EnableProxy:      UINT;
    EnableDns:        UINT;
  end;

  PMIB_IPADDRROW = ^TMIB_IPADDRROW;
  TMIB_IPADDRROW = record
    dwAddr:      DWORD;
    dwIndex:     DWORD;
    dwMask:      DWORD;
    dwBCastAddr: DWORD;
    dwReasmSize: DWORD;
    unused1:     WORD;
    unused2:     WORD;
  end;

  PMIB_IPADDRTABLE = ^TMIB_IPADDRTABLE;
  TMIB_IPADDRTABLE = record
    dwNumEntries: DWORD;
    Table: array [0..ANY_SIZE-1] of TMIB_IPADDRROW;
  end;

  TNetworkParams = class
  private
    FErrMsg: string;
    FHostName: string;
    FDomainName: string;
    FCurrentDNSList: TStrings;
    FDNSServerList: TStrings;
    FLocalIPList: TStrings;

    function IpAddressToString(Addr: DWORD): string; {$IFDEF VER170} inline; {$ENDIF}
    function StringToIpAddress(Addr: string): DWORD; {$IFDEF VER170} inline; {$ENDIF}

    function GetCurrentDNSCount: Cardinal;
    function GetCurrentDNSIP(Index: Integer): string;
    function GetDNSCount: Cardinal;
    function GetDNSIP(const Index: Integer): string;
    function GetMAC(const IP: string): string;

    procedure GetIPList;
    procedure GetDNSList;
  public
    constructor Create;
    destructor Destroy; override;

    property CurrentDNSIP[Index: Integer]: string read GetCurrentDNSIP;
    property DNSIP[const Index: Integer]: string read GetDNSIP;
    property MAC[const IP: string]: string read GetMAC;
  published
    property ErrMsg: string read FErrMsg;
    property HostName: string read FHostName;
    property DomainName: string read FDomainName;
    property CurrentDNSCount: Cardinal read GetCurrentDNSCount;
    property DNSCount: Cardinal read GetDNSCount;
  end;

  function GetNetworkParams(pFixedInfo: PFIXED_INFO; var pOutBufLen: ULONG) : DWORD;
           stdcall; external 'iphlpapi.dll' Name 'GetNetworkParams';
  function SendARP(const DestIP, SrcIP: IPAddr; pMacAddr: PULONG; var PhyAddrLen: ULONG): DWORD;
           stdcall; external 'iphlpapi.dll' name 'SendARP';
  function GetIpAddrTable(pIpAddrTable: PMIB_IPADDRTABLE; var pdwSize: ULONG; bOrder: BOOL): DWORD;
           stdcall; external 'iphlpapi.dll' Name 'GetIpAddrTable';

implementation

uses SysUtils;

{ TNetworkParams }

function TNetworkParams.IpAddressToString(Addr: DWORD): string;
var
  InAddr: TInAddr;
begin
  InAddr.S_addr := Addr;
  Result := inet_ntoa(InAddr);
end;

function TNetworkParams.StringToIpAddress(Addr: string): DWORD;
begin
  Result := inet_addr(PChar(Addr));
end;

constructor TNetworkParams.Create;
begin
  inherited;
  FCurrentDNSList := TStringList.Create;
  FDNSServerList  := TStringList.Create;
  FLocalIPList    := TStringList.Create;

  GetIPList;
  GetDNSList;
end;

destructor TNetworkParams.Destroy;
begin
  FreeAndNil(FLocalIPList);
  FreeAndNil(FDNSServerList);
  FreeAndNil(FCurrentDNSList);
  inherited;
end;

function TNetworkParams.GetCurrentDNSCount: Cardinal;
begin
  Result := FCurrentDNSList.Count;
end;

function TNetworkParams.GetCurrentDNSIP(Index: Integer): string;
begin
  if (Index > 0)and(Index <= FCurrentDNSList.Count) then
    Result := FCurrentDNSList[Index-1];
end;

function TNetworkParams.GetDNSCount: Cardinal;
begin
  Result := FDNSServerList.Count;
end;

function TNetworkParams.GetDNSIP(const Index: Integer): string;
begin
  if (Index > 0)and(Index <= FDNSServerList.Count) then
    Result := FDNSServerList[Index-1];
end;

procedure TNetworkParams.GetDNSList;
var
  PFixed: PFIXED_INFO;

  procedure ParseParams;
  var
    IP: IP_ADDRESS_STRING;
    IPAddrString: PIP_ADDR_STRING;
  begin
    with PFixed^ do
    begin
      FHostName := HostName;
      FDomainName := DomainName;

      IPAddrString := CurrentDNSServer;
      while Assigned(IPAddrString) do
      begin
        IP := IPAddrString^.IpAddress;
        FCurrentDNSList.Add(IP.S);
        IPAddrString := IPAddrString^.Next;
      end;

      IP := DNSServerList.IPAddress;
      FDNSServerList.Add(IP.S);
      IPAddrString := DNSServerList.Next;
      while Assigned(IPAddrString) do
      begin
        IP := IPAddrString^.IPAddress;
        FDNSServerList.Add(IP.S);
        IPAddrString := IPAddrString^.Next;
      end;
    end;  
  end;

var
  Res, dwSize: DWORD;
begin
  dwSize := 0;
  PFixed := nil;
  Res := GetNetworkParams(PFixed, dwSize);
  if Res = ERROR_BUFFER_OVERFLOW then
  begin
    GetMem(PFixed, dwSize);
    try
      FillChar(PFixed^, dwSize, 0);
      Res := GetNetworkParams(PFixed, dwSize);
      if Res = NO_ERROR then ParseParams;
    finally
      FreeMem(PFixed);
    end;
  end;
end;

procedure TNetworkParams.GetIPList;
var
  MIB_IPADDRTABLE: PMIB_IPADDRTABLE;

  procedure ParseParams;
  var
    I: DWORD;
    MIB_IPADDRROW: TMIB_IPADDRROW;
    IP: string;
  begin
    with MIB_IPADDRTABLE^ do
    begin
      for I := 1 to dwNumEntries do
      begin
        MIB_IPADDRROW := Table[I];
        IP := IpAddressToString(MIB_IPADDRROW.dwAddr);
        FLocalIPList.Add(IP);
      end;
    end;
  end;

var
  Res, dwSize: DWORD;
begin
  MIB_IPADDRTABLE := nil;
  dwSize := 0;
  Res := GetIpAddrTable(MIB_IPADDRTABLE, dwSize, False);

  if Res = ERROR_INSUFFICIENT_BUFFER then
  begin
    Getmem(MIB_IPADDRTABLE, dwSize);
    try
      FillChar(MIB_IPADDRTABLE^, dwSize, 0);
      Res := GetIpAddrTable(MIB_IPADDRTABLE, dwSize, False);

      if Res = NO_ERROR then ParseParams;
    finally
      FreeMem(MIB_IPADDRTABLE);
    end;
  end;
end;

function TNetworkParams.GetMAC(const IP: string): string;
var
  DestIP: IPAddr;
  pMacAddr: PULONG;
  addrlen : ULONG;
  macaddr: array[1..6] of Byte;
begin
  Result := '';
  FillChar(macaddr, SizeOf(macaddr), $FF);
  DestIP := StringToIpAddress(IP);
  pMacAddr := PULONG(@macaddr);
  addrlen := SizeOf(macaddr);
  if SendARP(DestIP, 0, pMacAddr, addrlen) = NO_ERROR then
  begin
    Result := Format('%2.2X-%2.2X-%2.2X-%2.2X-%2.2X-%2.2X',
           [macaddr[1],macaddr[2],macaddr[3],macaddr[4],macaddr[5],macaddr[6]]);
  end else
  begin
    Result := 'MAC±àºÅÎ´ÕÒµ½';
  end;
end;

end.
