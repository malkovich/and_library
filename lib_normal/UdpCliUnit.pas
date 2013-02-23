unit UdpCliUnit;
//{$DEFINE LOACAL_DEBUG_MODE}

interface
uses windows, SysUtils, winsock, Classes;

function UDP_Initial (HostAddr, HostPort: String; TimeOut: DWORD = 12000): Integer;
function UDP_Request (hSocket: Integer; CmdLine: String): String;
function UDP_Finally (hSocket: Integer): Integer;

implementation

uses DnsLookupUnit, DbgLoger;

var
  IsInitial: LongBool = False;

procedure InitialSocket;
var
  wsa: TWsadata;
begin
  if IsInitial then exit;
  IsInitial := True;

  WsaStartup(MAKEWORD(2, 2), wsa);
{$IFDEF LOACAL_DEBUG_MODE}
  DBG('InitialSocket: iMaxSockets=%d iMaxUdpDg=%d', [wsa.iMaxSockets, wsa.iMaxUdpDg]);
{$ENDIF}
end;

var
  LastHostAddr: string;
  LastSockAddr_in: SockAddr_in;

function GetSockAddr_in(HostAddr, HostPort: String): SockAddr_in;
begin
  try
    Result := GetSockAddrIn(HostAddr, HostPort);

  except
{$IFDEF LOACAL_DEBUG_MODE}
    DBG('GetSockAddr_in exception !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!');
{$ENDIF}
    if HostAddr = LastHostAddr then
      Result.sin_addr.S_addr := LastSockAddr_in.sin_addr.S_addr;

    Result.sin_port := htons(atoi(HostPort));

    exit;
  end;
  LastSockAddr_in := Result;
end;

function UDP_Initial (HostAddr, HostPort: String; TimeOut: DWORD = 12000): Integer;
var
  hSocket: Integer;
  ServAddr: SockAddr_in;
begin
  Result := -1;
  InitialSocket;

  ZeroMemory( @ServAddr, SizeOf(SockAddr_in) );

  hSocket := Socket(AF_INET, SOCK_DGRAM, 0);
  if hSocket = INVALID_SOCKET then EXIT;

  Repeat
    if SetSockOpt(hSocket, SOL_SOCKET, SO_RCVTIMEO, PChar(@TimeOut), SizeOf(TimeOut)) = SOCKET_ERROR then Break;
    if SetSockOpt(hSocket, SOL_SOCKET, SO_SNDTIMEO, PChar(@TimeOut), SizeOf(TimeOut)) = SOCKET_ERROR then Break;

    ServAddr := GetSockAddr_in(HostAddr, HostPort);
    if SOCKET_ERROR = Connect (hSocket, ServAddr, SizeOf(ServAddr)) then Break;

    Result := hSocket;
    Exit;
  Until True;

  CloseSocket(hSocket);
end;

function UDP_Finally (hSocket: Integer): Integer;
begin
  Result := CloseSocket(hSocket);
end;

function UDP_Request (hSocket: Integer; CmdLine: String): String;
var
  nBytesDone: Integer;
  RecvBuf: PChar;
{$IFDEF LOACAL_DEBUG_MODE}
  BeginTick: DWORD;
{$ENDIF}
begin
{$IFDEF LOACAL_DEBUG_MODE}
  BeginTick := GetTickCount;
{$ENDIF}
  Result := '';
  GetMem(RecvBuf, $1000);
  try
    nBytesDone := Send(hSocket, CmdLine[1], Length(CmdLine), 0);
    if nBytesDone <= 0 then exit;

    nBytesDone := Recv (hSocket, RecvBuf[0], $1000, 0);
    if nBytesDone <= 0 then exit;

{$IFDEF LOACAL_DEBUG_MODE}
    DBG ('UDP_Request uses %d ms', [GetTickCount - BeginTick]);
{$ENDIF}
    RecvBuf[nBytesDone] := #0;
    Result := StrPas(RecvBuf);
    Result := Trim(Result);
  finally
    FreeMem(RecvBuf);
  end;
end;

end.
