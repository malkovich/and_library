unit EchoRequestUnit;

interface
uses
  SysUtils, windows, DbgLoger, winsock, DnsLookupUnit, classes,
  Base64Unit;


type
  TDownloadReporter = procedure (NumberOfBytes: Integer);

  
function EchoRequest(HostAddr, HostPort, CmdHead, FullCmd: String; var ResultStr: String; Report: TDownloadReporter=nil):LongBool;
function GetSplitBlankList (InputStr: String; Separate: TSysCharSet = [' ']): TStringList;


implementation

function GetSplitBlankList (InputStr: String; Separate: TSysCharSet = [' ']): TStringList;
begin
  Result := TStringList.Create;
  ExtractStrings (Separate,[' '],PChar(InputStr),Result);
end;

function ReadLineFrom (hSocket: THandle; ExpectFromStr: String): String;
var
  RecvChar: Char;
  RecvWindow: String;
  RecvWindowSize: Integer;
  HasReturn, HasNewLine: BOOL;
begin
  result := '';
  RecvWindowSize := Length (ExpectFromStr);
  SetLength (RecvWindow, RecvWindowSize);
  ZeroMemory (@RecvWindow[1], RecvWindowSize);

  repeat
    CopyMemory (@RecvWindow[1], @RecvWindow[2], RecvWindowSize - 1);
    if SOCKET_ERROR = Recv (hSocket, RecvWindow[RecvWindowSize], 1, 0) then exit;
  until RecvWindow = ExpectFromStr;

  Result := RecvWindow;
  HasReturn := False;
  HasNewLine := False;
  repeat
    if SOCKET_ERROR = Recv (hSocket, RecvChar, 1, 0) then exit;
    Result := Result + RecvChar;

    case RecvChar of
      #13: HasReturn := True;
      #10: if HasReturn then HasNewLine := True;
      else begin
        HasReturn := False;
        HasNewLine := False;
      end;
    end;
  until HasNewLine;

  Result := Trim(Result);
end;

function ReadLineFrom2 (hSocket: THandle; ExpectFromStr: String; Report: TDownloadReporter=nil): String;
var
  RecvChar: Char;
  RecvBuff: Array[0..1023] of char;
  RecvSize, I: Integer;
  RecvWindow: String;
  RecvWindowSize: Integer;
  HasReturn, HasNewLine: BOOL;
begin
  result := '';
  RecvWindowSize := Length (ExpectFromStr);
  SetLength (RecvWindow, RecvWindowSize);
  ZeroMemory (@RecvWindow[1], RecvWindowSize);

  repeat
    CopyMemory (@RecvWindow[1], @RecvWindow[2], RecvWindowSize - 1);
    if SOCKET_ERROR = Recv (hSocket, RecvWindow[RecvWindowSize], 1, 0) then exit;
  until RecvWindow = ExpectFromStr;

  Result := RecvWindow;
  HasReturn := False;
  HasNewLine := False;
  repeat
    RecvSize := Recv (hSocket, RecvBuff, 1024, 0);
    if SOCKET_ERROR = RecvSize then exit;

    if Assigned (Report) then
      Report (Length (Result));

    I := 0;
    repeat
      RecvChar := RecvBuff[I];
      Result := Result + RecvChar;
      Inc (I);

      case RecvChar of
        #13: HasReturn := True;
        #10: if HasReturn then HasNewLine := True;
        else begin
          HasReturn := False;
          HasNewLine := False;
        end;
      end;
    until (RecvSize = I) or HasNewLine;

  until HasNewLine;

  if Assigned (Report) then
    Report (Length (Result));

  Result := Trim(Result);
end;

var
  IsInitial: LongBool = False;

procedure InitialSocket;
var
  wsa: TWsadata;
begin
  if IsInitial then exit;
  IsInitial := True;

  WsaStartup(MAKEWORD(2, 2), wsa);
  DBG('InitialSocket: iMaxSockets=%d iMaxUdpDg=%d', [wsa.iMaxSockets, wsa.iMaxUdpDg]);
end;

function EchoRequest(HostAddr, HostPort, CmdHead, FullCmd: String; var ResultStr: String; Report: TDownloadReporter=nil):LongBool;
var
  CMD, TempStr: String;
  hSocket: Integer;
  addr: SockAddr_in;
begin
  InitialSocket;

  result := false;

  hSocket := Socket(AF_INET, SOCK_STREAM, 0);
  if hSocket = INVALID_SOCKET then EXIT;                  

  try
    addr := GetSockAddrIn (HostAddr, HostPort);   
    if SOCKET_ERROR = Connect (hSocket, addr, SizeOf(addr)) then exit;

    CMD := FullCmd + #13#10;
    if SOCKET_ERROR = Send(hSocket, CMD[1], Length(CMD), 0) then exit;

    TempStr := ReadLineFrom2 (hSocket, CmdHead, Report);
    Delete (TempStr, 1, Length(CmdHead));

    ResultStr := Trim(TempStr);
    Result := Length(ResultStr) > 0;
  finally
    CloseSocket(hSocket);
  end;
end;

end.
