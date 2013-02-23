unit LineServerUnit2;

{$I ICSDEFS.INC}

interface
uses
  Messages, Windows, SysUtils, Classes, WSocket, WinSock;

type
    // TClientObject handle a single client communication
    TClientObject = class (TObject)
    protected
        FServerObject  :TObject;
        FCliWSocket    : TWSocket;
        FCtrlWindow    : HWND;
        FCommand       : array [0..$1000-1] of char;
        FCmdLen        : Integer;
        FPeerName      : String;
        procedure DataAvailableHandler(Sender : TObject; Error : Word);
        procedure SessionClosedHandler(Sender : TObject; Error : Word);
        procedure CommandInterpreter;
        procedure CMDHandler(CommandVerb ,CommandTail : String); virtual;
    public
        constructor Create(ServerObject :TObject); virtual;
        destructor  Destroy; override;
        procedure   StartClient(ASocket : TSocket);
        property CtrlWindow : HWND read  FCtrlWindow write FCtrlWindow;
        property PeerName : String read  FPeerName;    
    end;
    
    // TServerObject handle all clients sessions
    TServerObject = class (TObject)
    protected
        FSrvWSocket : TWSocket;
        FCliList    : TList;
        FCtrlWindow : HWND;
        FAddr, FPort: String;
        procedure SessionAvailableHandler(Sender : TObject; Error : Word);
        function CreateClient:TClientObject; virtual;
    public
        constructor Create(Addr, Port: String); virtual;
        destructor  Destroy; override;
        procedure StartServer;
        procedure DisconnectedClient(Client : TClientObject);
        procedure DisplayClientList;
        property CtrlWindow : HWND read  FCtrlWindow write FCtrlWindow;
        property SrvWSocket : TWSocket read  FSrvWSocket;
    end;


const
    WM_CLIENT_DISCONNECTED = WM_USER + 1;

implementation
uses DbgLoger;

procedure DBG(Msg:String);
begin
  if IsConsole then
    WriteLn('[' + TimeToStr(Now) + '] ' + Msg)
  else
    DbgLoger.DBG(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TServerObject.Create(Addr, Port: String);
begin
    inherited Create;
    FAddr := Addr;
    FPort := Port;
    FCliList    := TList.Create;
    FSrvWSocket := TWSocket.Create(nil);
    StartServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TServerObject.Destroy;
begin
    if Assigned(FSrvWSocket) then begin
        FSrvWSocket.Destroy;
        FSrvWSocket := nil;
    end;
    if Assigned(FCliList) then begin
        FCliList.Destroy;
        FCliList := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerObject.StartServer;
begin
    FSrvWSocket.Proto := 'tcp';
    FSrvWSocket.Port  := FPort;
    FSrvWSocket.Addr  := FAddr;
    FSrvWSocket.OnSessionAvailable := SessionAvailableHandler;
    FSrvWSocket.Listen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerObject.SessionAvailableHandler(Sender : TObject; Error : Word);
var
    Client : TClientObject;
    ASocket : TSocket;
//    SAddr    : TSockAddrIn;
//    SAddrLen : integer;
begin
//    Write(ServerName + ' 连接建立: ');

    // Create a new object to handle client session
    Client  := CreateClient;

    // Add to our client list
    FCliList.Add(Client);

    // Accept the connection
    ASocket := FSrvWSocket.Accept;

    // Determine who has connected before really starting the session
//    SAddrLen := SizeOf(SAddr);
//    WSocket_getpeername(ASocket, SAddr, SAddrLen);
//    Write(WSocket_inet_ntoa(SAddr.sin_addr));
//    WriteLn(' [', FCliList.Count, ']');

    // Startup the client connection (send banner)
    Client.CtrlWindow := FCtrlWindow;
    Client.StartClient(ASocket);
end;

function TServerObject.CreateClient:TClientObject;
begin
  Result := TClientObject.Create(self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is called to disconnect a client and remove it from our    }
{ client list.                                                              }
procedure TServerObject.DisconnectedClient(Client : TClientObject);
var
    Index : Integer;
begin

    // Search client in our list
    Index := FCliList.IndexOf(Client);
    if Index < 0 then
        Exit;    // Not found already disconnected

    FCliList.Delete(Index);
//    Write(ServerName , Client.PeerName, ' 断开连接');
//    WriteLn('[', FCliList.Count, ']');

    // Then destroy client (this will abort the session if still active)
    Client.Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TServerObject.DisplayClientList;
var
    I      : Integer;
    Client : TClientObject;
begin
    DBG('有 ' + IntToStr(FCliList.Count) + ' 个连接的客户端。');
    for I := 1 to FCliList.Count do begin
        Client := FCliList.Items[I - 1];
        DBG (Client.FPeerName);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TClientObject.Create(ServerObject :TObject);
begin
    inherited Create;
    FServerObject := ServerObject;
    FCliWSocket := TWSocket.Create(nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TClientObject.Destroy;
begin
    if Assigned(FCliWSocket) then begin
        FCliWSocket.Destroy;
        FCliWSocket := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientObject.StartClient(ASocket : TSocket);
begin
    FCliWSocket.LineMode        := TRUE;
    FCliWSocket.LineLimit       := 2 * 1024 * 1024;
    FCliWSocket.LineEnd         := #13#10;
    FCliWSocket.LineEcho        := FALSE;
    FCliWSocket.LineEdit        := False;  
    FCliWSocket.OnDataAvailable := DataAvailableHandler;
    FCliWSocket.OnSessionClosed := SessionClosedHandler;
    FCliWSocket.HSocket         := ASocket;          
    FPeerName                   := FCliWSocket.GetPeerAddr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientObject.SessionClosedHandler(Sender : TObject; Error : Word);
begin
    PostMessage(FCtrlWindow, WM_CLIENT_DISCONNECTED, 0, LParam(Self));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// This handler is called each time we received a complete line from
// connected client (remember we use line mode)
procedure TClientObject.DataAvailableHandler(Sender : TObject; Error : Word);
begin
    // Get data from socket component. We should receive a complete line.
    FCmdLen := FCliWSocket.Receive(@FCommand, SizeOf(FCommand) - 1);
    if FCmdLen <= 0 then Exit;    // No data available
    if FCliWSocket.State <> wsConnected then Exit;    // Ignore any data received while closing

    // Remove trailling CR/LF, if any (could be missing if our buffer
    // was too small)
    while (FCmdLen > 0) and (FCommand[FCmdLen - 1] in [#13, #10]) do
        Dec(FCmdLen);
    FCommand[FCmdLen] := #0;

    // Interpret received command
    CommandInterpreter;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This is the command line interpreter. Should extend the code to support   }
{ every command needed...                                                   }
procedure TClientObject.CommandInterpreter;
var
    CommandVerb : String;
    CommandTail : String;
    I, J        : Integer;
begin
    { Skip leading spaces }
    I := 0;
    while (I < FCmdLen) and (FCommand[I] in [' ', #9]) do
        Inc(I);

    { Find separator and separe CommandVerb and CommandTail }
    J := I;
    while TRUE do begin
        if (J >= FCmdLen) then begin
            SetLength(CommandVerb, FCmdLen - I);
            Move(FCommand[I], CommandVerb[1], Length(CommandVerb));
            CommandTail := '';
            break;
        end;

        if FCommand[J] in [' ', #9] then begin
            SetLength(CommandVerb, J - I);
            Move(FCommand[I], CommandVerb[1], Length(CommandVerb));
            SetLength(CommandTail, FCmdLen - J);
            Move(FCommand[J], CommandTail[1], Length(CommandTail));
            break;
        end;
        Inc(J);
    end;

    if CommandVerb = 'BYE' then
    begin
      PostMessage(FCtrlWindow, WM_CLIENT_DISCONNECTED, 0, LParam(Self));
      exit;
    end;

    CMDHandler(CommandVerb, CommandTail);

end;

procedure TClientObject.CMDHandler(CommandVerb ,CommandTail : String);
begin   
end;


end.
