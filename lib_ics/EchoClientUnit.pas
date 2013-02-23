unit EchoClientUnit;

interface
uses
  SysUtils, windows, winsock, DnsLookupUnit, classes;

type
  TDownloadReporter = procedure (NumberOfBytes: Integer);

function EchoRequest (HostAddr, HostPort, ReadUntilForm, FullCmd: String; var ResultStr: String; Report: TDownloadReporter=nil):LongBool; Overload;
function EchoRequest(HostAddr, HostPort, ReadUntilForm, FullCmd: String;
      IsKeepSocket: BOOL; var InOutSocket: Integer; var ResultStr: String; Report: TDownloadReporter=nil):LongBool; Overload;
      
function GetSplitBlankList (InputStr: String; Separate: TSysCharSet = [' ']): TStringList;
function GetSelfModuleName: string;

implementation

function GetSelfModuleName: string;
var
  SignWord: PWORD;
begin
  SignWord := Pointer (DWORD (@GetSelfModuleName) And $FFFFF000);
  while SignWord^ <> $5A4D do
    SignWord := Pointer (DWORD(SignWord) - $1000);    
  Result := GetModuleName (THandle (SignWord));
end;

function GetSplitBlankList (InputStr: String; Separate: TSysCharSet = [' ']): TStringList;
begin
  Result := TStringList.Create;
  if Trim(InputStr) = '' then Exit;
  ExtractStrings (Separate,[' '],PChar(InputStr),Result);
end;

function ReadALineFrom (hSocket: THandle; ExpectFromStr: String; Report: TDownloadReporter=nil): String;
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
end;

function EchoRequest(HostAddr, HostPort, ReadUntilForm, FullCmd: String; var ResultStr: String; Report: TDownloadReporter=nil):LongBool;
var
  TempStr: String;
  hSocket: Integer;
  addr: SockAddr_in;
begin
  InitialSocket;

  result := false;

  hSocket := Socket(AF_INET, SOCK_STREAM, 0);
  if hSocket = INVALID_SOCKET then EXIT;

  try
    addr := GetSockAddrIn (HostAddr, HostPort);   
    if SOCKET_ERROR = Connect (hSocket, @addr, SizeOf(addr)) then exit;

    FullCmd := FullCmd + #13#10;
    if SOCKET_ERROR = Send(hSocket, FullCmd[1], Length(FullCmd), 0) then exit;

    TempStr := ReadALineFrom (hSocket, ReadUntilForm, Report);
    Delete (TempStr, 1, Length(ReadUntilForm));

    ResultStr := Trim(TempStr);
    Result := Length(ResultStr) > 0;
  finally
    CloseSocket(hSocket);
  end;
end;


function EchoRequest(HostAddr, HostPort, ReadUntilForm, FullCmd: String;
      IsKeepSocket: BOOL; var InOutSocket: Integer;
      var ResultStr: String; Report: TDownloadReporter=nil):LongBool;
var
  TempStr: String;
  hSocket: Integer;
  addr: SockAddr_in;
begin
  InitialSocket;
  result := false;

  hSocket := InOutSocket;
  if hSocket = INVALID_SOCKET then
  begin
    hSocket := Socket(AF_INET, SOCK_STREAM, 0);
    if hSocket = INVALID_SOCKET then EXIT;

    addr := GetSockAddrIn (HostAddr, HostPort);
    if SOCKET_ERROR = Connect (hSocket, addr, SizeOf(addr)) then
    begin
      CloseSocket(hSocket);
      exit;
    end;
  end;         

  FullCmd := FullCmd + #13#10;
  if SOCKET_ERROR = Send(hSocket, FullCmd[1], Length(FullCmd), 0) then
  begin
    CloseSocket(hSocket);
    InOutSocket := SOCKET_ERROR;
    exit;
  end;

  TempStr := ReadALineFrom (hSocket, ReadUntilForm, Report);
  Delete (TempStr, 1, Length(ReadUntilForm));

  ResultStr := TempStr;
  Result := Length(ResultStr) > 0;

  if IsKeepSocket then
  begin
    InOutSocket := hSocket;
    Exit;
  end;

  CloseSocket (hSocket);
  InOutSocket := SOCKET_ERROR;
end;


end.
