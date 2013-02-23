unit LineClient;

interface
uses
  windows, classes, syncObjs, SysUtils, forms,
  WSocket, winsock, TCacheQueueThread, md5;

Type
  TLineSocketClient = class (TObject)
  protected
    FAOwner:TComponent;
    FAddr, FPort: String;
    FWSocket: TWSocket;
    procedure DataAvailable (Sender: TObject; ErrCode: Word);
    procedure SessionConnected(Sender: TObject; Error: Word);
    procedure SessionClosed(Sender: TObject; Error: Word);
    procedure RecvHandler(RecvLine : String); virtual;
    procedure InitialSocket;
  public
    Constructor Create(AOwner:TComponent; Addr, Port: String);
    Destructor Destroy; override;
    function PostLine(Msg: String): LongBool;
    property Socket:TWSocket read FWSocket;
  end;

implementation

procedure TLineSocketClient.InitialSocket;
var
  TickCount: DWORD;
begin
  FWSocket := TWSocket.Create(FAOwner);
  FWSocket.Proto := 'tcp';
  FWSocket.Addr := FAddr;
  FWSocket.Port := FPort;
  FWSocket.LineMode := True;
  FWSocket.LineEnd         := #13#10;
  FWSocket.LineEcho        := FALSE;
  FWSocket.LineEdit        := False;
  FWSocket.OnDataAvailable := DataAvailable;
  FWSocket.OnSessionConnected := SessionConnected;
  FWSocket.OnSessionClosed := SessionClosed;
  FWSocket.MultiThreaded := True;

  try
    FWSocket.Connect;
  except
    exit;
  end;

  TickCount := GetTickCount;
  repeat
    application.ProcessMessages;
    sleep(10);
    if (GetTickCount - TickCount) > 5000 then exit;
    if not assigned (FWSocket) then exit;    
  until FWSocket.State = wsConnected;
end;

Constructor TLineSocketClient.Create(AOwner:TComponent; Addr,Port: String);
begin
  FAOwner := AOwner;
  FAddr := Addr;
  FPort := Port;
  InitialSocket;
end;

Destructor TLineSocketClient.Destroy;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    if assigned(FWSocket) then
    if FWSocket.State in [wsConnecting, wsConnected, wsOpened, wsBound] then
    begin
      SL.Add('      1¡¢CloseDelayed');
      FWSocket.CloseDelayed;

      SL.Add('      2¡¢Start WaitForClose');
      FWSocket.WaitForClose;

      SL.Add('      2¡¢Destroy End');
    end;
  except
    OutputDebugString(PChar(SL.Text));
    SL.Free;
  end;
  inherited;
end;

procedure TLineSocketClient.SessionConnected(Sender: TObject; Error: Word);
begin
end;

procedure TLineSocketClient.SessionClosed(Sender: TObject; Error: Word);
begin
    { Destroy the socket. We can't use Destroy here because we are in       }
    { an event handler. We need to use Release which will delay destruction }
    { until we are out of the event handler.                                }
    FWSocket.Release;
    FWSocket := nil;
end;

procedure TLineSocketClient.DataAvailable (Sender: TObject; ErrCode: Word);
var
  WSocket : TWSocket absolute Sender;
  FRcvBuf: String;
begin
  { Remember: we use line mode. We will always receive complete lines     }
  with Sender as TWSocket do
        FRcvBuf := ReceiveStr;
  { Remove trailing CR/LF, if any }
  if (Length(FRcvBuf) > 1) and (FRcvBuf[Length(FRcvBuf)] = #10) and (FRcvBuf[Length(FRcvBuf) - 1] = #13) then
    SetLength(FRcvBuf, Length(FRcvBuf) - 2);
  if Trim(FRcvBuf) = '' then EXIT;

  RecvHandler (FRcvBuf);   
end;

procedure TLineSocketClient.RecvHandler(RecvLine : String);
begin             
end;

function TLineSocketClient.PostLine(Msg: String): LongBool;
begin
  Result := False;

  if FWSocket = nil then
    InitialSocket;

  if FWSocket.State <> wsConnected then
  begin
    OutputDebugString(PChar('Not Connected, Break Sendling'));
    exit;
  end;
  Result := FWSocket.SendLine(Msg) > 0;
end;


end.
