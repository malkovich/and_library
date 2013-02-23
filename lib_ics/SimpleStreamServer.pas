unit SimpleStreamServer;

interface

uses
  Windows, Classes, SysUtils;

Type
  TConsoleCmdHandler = Procedure (Param: String);
  TScanPacket = function (Buffer: Pointer; Size: Integer): Integer;
  THandlePacker = Procedure (Client: TObject; Packet: Pointer; Size: Integer);
  TOnClientEvent = Procedure(Server, Client: TObject; ErrCode: Word);

Procedure AddEventNotify (Name: String; Notifier: Pointer);  
Procedure AddConsoleCmd (Cmd, Help: String; Handler: TConsoleCmdHandler);
Procedure AddPacketHandler (Scan: TScanPacket; Handler: THandlePacker);

Procedure RunStreamServer (Addr, Port: String);

Function  IsClientValid (Client: TObject): BOOL;
Procedure SendToClient (Client: TObject; Data: Pointer; Size: Integer);
Procedure CloseClient (Client: TObject);
procedure GetClientPeer (Client: TObject; Out RemoteAddr, RemotePort: String);
Function  GetClientCount (Client: TObject): Integer;
Function  GetClientData (Client: TObject): Pointer;
Procedure SetClientData (Client: TObject; Data: Pointer);

implementation

uses
  ConApp, WSocket, WinSock, WSocketS;

const
  MAX_RECVD_BUFFER_SIZE = $2000;
  CmdPrompt = '>>>';

Type
  THandler = Packed Record
    Scan: TScanPacket;
    Handler: THandlePacker;
  end;

type
    { TTcpSrvClient is the class which will be instanciated by server component }
    { for each new client. N simultaneous clients means N TTcpSrvClient will be }
    { instanciated. Each being used to handle only a single client.             }
    { We can add any data that has to be private for each client, such as       }
    { receive buffer or any other data needed for processing.                   }
    TTcpSrvClient = class(TWSocketClient)
    public
        RcvdBuffer  : Array[0..MAX_RECVD_BUFFER_SIZE-1] of Char;
        RcvdLength  : Integer;
        FData: Pointer;
    end;

    TWSocketApplication = class(TConApplication)
    protected
        WSocketServer1 : TWSocketServer;
        procedure WSocketServer1ClientConnect(Sender  : TObject;
                                              Client  : TWSocketClient;
                                              ErrCode : Word);
        procedure WSocketServer1ClientDisconnect(Sender  : TObject;
                                                 Client  : TWSocketClient;
                                                 ErrCode : Word);
        procedure WSocketServer1BgException(Sender       : TObject;
                                            E            : Exception;
                                            var CanClose : Boolean);
        procedure ClientBgException(Sender       : TObject;
                                    E            : Exception;
                                    var CanClose : Boolean);
        procedure ClientDataAvailable(Sender: TObject; ErrCode: Word);
        function  ProcessPacketBlock (Sender: TObject; Buffer: PChar; Size: Integer): Integer;
        procedure WndProc(var Msg: TMsg); override;
    public
        PacketHandler: Array of THandler;
        ConsoleCmdList: TStringList;
        NotifierSL: TStringList;
        HandlerCount: Integer;
        Addr, Port: String;
        procedure DoLineReceived(const Line : String); override;
        procedure StartServer;
        procedure Display(const Msg : String);
        procedure Help;
        procedure Execute; override;
    end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.WndProc(var Msg: TMsg);
begin
    inherited WndProc(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Display(const Msg : String);
begin
    ConsoleWriteLn(Msg);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when listening (server) socket experienced   }
{ a background exception. Should normally never occurs.                     }
procedure TWSocketApplication.WSocketServer1BgException(
    Sender       : TObject;
    E            : Exception;
    var CanClose : Boolean);
begin
    Display('Server exception occured: ' + E.ClassName + ': ' + E.Message);
    CanClose := FALSE;  { Hoping that server will still work ! }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when a client socket experience a background }
{ exception. It is likely to occurs when client aborted connection and data }
{ has not been sent yet.                                                    }
procedure TWSocketApplication.ClientBgException(
    Sender       : TObject;
    E            : Exception;
    var CanClose : Boolean);
begin
    Display('Client exception occured: ' + E.ClassName + ': ' + E.Message);
    CanClose := TRUE;   { Goodbye client ! }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.WSocketServer1ClientConnect(
    Sender  : TObject;
    Client  : TWSocketClient;
    ErrCode : Word);
var
  Index: Integer;
  ClientEventCall: TOnClientEvent;
begin
    with Client as TTcpSrvClient do begin
        OnDataAvailable     := ClientDataAvailable;
        OnBgException       := ClientBgException;
    end;             

    if Assigned (NotifierSL) then
    begin
      Index := NotifierSL.IndexOf('OnClientConnect');
      if Index >= 0 then
      begin
        ClientEventCall := Pointer(NotifierSL.Objects[Index]);
        ClientEventCall (Sender, Client, ErrCode);
      end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.WSocketServer1ClientDisconnect(
    Sender  : TObject;
    Client  : TWSocketClient;
    ErrCode : Word);
var
  Index: Integer;
  ClientEventCall: TOnClientEvent;
begin
    if Assigned (NotifierSL) then
    begin
      Index := NotifierSL.IndexOf('OnClientDisconnect');
      if Index >= 0 then
      begin
        ClientEventCall := Pointer(NotifierSL.Objects[Index]);
        ClientEventCall (Sender, Client, ErrCode);
      end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.ClientDataAvailable(
    Sender  : TObject;
    ErrCode : Word);
var
  RecvLen, ItemSize, HandleSize: Integer;
begin
    with Sender as TTcpSrvClient do
    begin
        RecvLen := Receive (@RcvdBuffer[RcvdLength], MAX_RECVD_BUFFER_SIZE - RcvdLength);
        if RecvLen < 0 then
        begin
          case LastError of
            WSAEWOULDBLOCK: ;  //忽略
            Else begin
            Display ('收包错误 RecvLen<0 LastError = ' + IntToStr(LastError));
            CloseDelayed;
            end;
          end;
          Exit;
        end;

        if RecvLen = 0 then exit;
        Inc (RcvdLength, RecvLen);

        HandleSize := 0;
        Repeat
          ItemSize := ProcessPacketBlock (Sender, @RcvdBuffer[HandleSize], RcvdLength - HandleSize);

          //还没收完，继续
          if ItemSize < 0 then
          begin
            Break;
            Exit;
          end;

          //找不到处理过程，或出现超大的封包，这是不正常的
          if ItemSize = 0 then
          begin
            Display ('找不到处理过程，断开客户端');
            CloseDelayed;
            Exit;
          end;

          Inc (HandleSize, ItemSize);
        until HandleSize >= RcvdLength;

        //已经完成一个分析，移到下一个
        if HandleSize > 0 then
        begin
          CopyMemory (@RcvdBuffer[0], @RcvdBuffer[HandleSize], RcvdLength - HandleSize);
          Dec (RcvdLength, HandleSize);
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketApplication.ProcessPacketBlock (Sender: TObject; Buffer: PChar; Size: Integer): Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to HandlerCount - 1 do
  begin
    Result := PacketHandler[Index].Scan (Buffer, Size);

    if Result > 0 then
    begin
      PacketHandler[Index].Handler (Sender, Buffer, Result);
      Break;
    end;

    if Result < 0 then
    begin
      Exit;
    end;
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.DoLineReceived(const Line : String);
var
    Cmd : String;
    Prm : String;
    I, Index   : Integer;
    CmdHandler : Procedure (Param: String);
begin
    Cmd := '';
    I := 1;
    // Skip white spaces
    while (I <= Length(Line)) and (Line[I] = ' ') do
        Inc(I);
    // Copy first word
    while (I <= Length(Line)) and (Line[I] <> ' ') do begin
        Cmd := Cmd + UpperCase(Line[I]);
        Inc(I);
    end;
    // Skip white spaces
    while (I <= Length(Line)) and (Line[I] = ' ') do
        Inc(I);
    Prm := Copy(Line, I, Length(Line));
    if Cmd = '' then begin
        ConsoleWrite(CmdPrompt);
        Exit;
    end;

    if Assigned (ConsoleCmdList) then
    begin
      Index := ConsoleCmdList.IndexOfName(Cmd);
      if Index >= 0 then
      begin
        CmdHandler := Pointer (ConsoleCmdList.Objects[Index]);
        CmdHandler (Prm);
      end else
        Display('Unknown command');
      ConsoleWrite(CmdPrompt);
    end;           
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Help;
var
  Index: Integer;
  CmdHandler : Procedure (Param: String);
begin
    if Assigned (ConsoleCmdList) then
    begin
      Index := ConsoleCmdList.IndexOfName('help');
      if Index >= 0 then
      begin
        CmdHandler := Pointer (ConsoleCmdList.Objects[Index]);
        CmdHandler ('');
      end;    
    end;   
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.StartServer;
begin
    try
        WSocketServer1.Proto              := 'tcp';
        WSocketServer1.Addr               := Addr;
        WSocketServer1.Port               := Port;
        WSocketServer1.ClientClass        := TTcpSrvClient;
        WSocketServer1.OnClientConnect    := WSocketServer1ClientConnect;
        WSocketServer1.OnClientDisconnect := WSocketServer1ClientDisconnect;
        WSocketServer1.OnBgException      := WSocketServer1BgException;

        WSocketServer1.Listen;
        Display('Listenning at : ' + WSocketServer1.Addr + ' : ' + WSocketServer1.Port);
    except
        on E:Exception do
            Display('Exception' + E.ClassNAme + ': ' + E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Execute;
var
    MyIP : TStrings;
    I    : Integer;
begin
    Display('SimpleStreamServer Starting !');
    Display('');
    Display('LocalHost = ' + LocalHostName);
    MyIP := LocalIPList;
    for I := 0 to MyIP.Count - 1 do
        Display('IP #' + IntToStr(I + 1) + ' = ' + MyIP.Strings[I]);

    WSocketServer1              := TWSocketServer.Create(nil);
    StartServer;

    Display('');
    Display ('控制台命令列表：');
    Help;
    ConsoleWrite(CmdPrompt);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

Function  IsClientValid (Client: TObject): BOOL;
begin
  With ConApplication as TWSocketApplication do
    Result := WSocketServer1.IsClient(Client);
end;

Procedure SendToClient (Client: TObject; Data: Pointer; Size: Integer);
begin
  with Client as TTcpSrvClient do
  begin
    Send(Data, Size);
  end;
end;

Procedure CloseClient (Client: TObject);
begin
  with Client as TTcpSrvClient do
  begin
    CloseDelayed;
  end;
end;


Function  GetClientData (Client: TObject): Pointer;
begin
  with Client as TTcpSrvClient do
  begin
    Result := FData;
  end;
end;

Procedure SetClientData (Client: TObject; Data: Pointer);
begin
  with Client as TTcpSrvClient do
  begin
    FData := Data;
  end;
end;


procedure GetClientPeer (Client: TObject; Out RemoteAddr, RemotePort: String);
begin
  with Client as TTcpSrvClient do
  begin
    RemoteAddr := PeerAddr;
    RemotePort := PeerPort;
  end;
end;

Function GetClientCount (Client: TObject): Integer;
begin
  with Client as TTcpSrvClient do
  begin
    Result := TWSocketServer(FServer).ClientCount;
  end;     
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

var
  PacketHandler: Array of THandler;
  ConsoleCmdList: TStringList;
  EventNotifier: TStringList;

Procedure CmdHelp (Param: String);
var
  RunConApp: TWSocketApplication;
begin
  RunConApp := ConApplication as TWSocketApplication;
  RunConApp.Display (RunConApp.ConsoleCmdList.Text);
end;

Procedure CmdExit (Param: String);
var
  RunConApp: TWSocketApplication;
begin
  RunConApp := ConApplication as TWSocketApplication;
  RunConApp.Terminate;
end;                                    

Procedure AddConsoleCmd (Cmd, Help: String; Handler: TConsoleCmdHandler);
begin
  if not assigned (ConsoleCmdList) then
    ConsoleCmdList := TStringList.Create;
  ConsoleCmdList.AddObject(Cmd+'='+Help, @Handler);
end;

Procedure SetConsoleHandler (ConsoleCmd: TStringList);
var
  RunConApp: TWSocketApplication;
begin
  RunConApp := ConApplication as TWSocketApplication;

  With RunConApp do
  begin
    if not assigned (ConsoleCmdList) then
      ConsoleCmdList := TStringList.Create;

    if Assigned (ConsoleCmd) then
      ConsoleCmdList.AddStrings(ConsoleCmd);

    ConsoleCmdList.AddObject('exit=退出本程序', @CmdExit);
    ConsoleCmdList.AddObject('help=显示本帮助列表', @CmdHelp);
  end;
end;

Procedure AddPacketHandler (Scan: TScanPacket; Handler: THandlePacker);
var
  RawLen: Integer;
begin
  RawLen := Length (PacketHandler);
  SetLength (PacketHandler, RawLen + 1);
  PacketHandler [RawLen].Scan := Scan;
  PacketHandler [RawLen].Handler := Handler;
end;


Procedure SetHandler (Handlers: Array of THandler);
var
  Index, Count: Integer;
  RunConApp: TWSocketApplication;
begin
  RunConApp := ConApplication as TWSocketApplication;
  Count := Length(Handlers);
  SetLength (RunConApp.PacketHandler, Count);
  for Index := 0 to Count - 1 do
    RunConApp.PacketHandler[Index] := Handlers[Index];
  RunConApp.HandlerCount := Count;
end;

Procedure AddEventNotify (Name: String; Notifier: Pointer);
begin
  if Not Assigned (EventNotifier) then
    EventNotifier := TStringList.Create;     
  EventNotifier.AddObject(Name, Notifier);
end;

Procedure SetEventNotify (Notifiers: TStringList);
begin
  if Assigned (Notifiers) then
  with ConApplication as TWSocketApplication do
  begin
    if not Assigned (NotifierSL) then
      NotifierSL := TStringList.Create;
    NotifierSL.AddStrings(Notifiers);
  end;      
end;

Procedure SetHostPort (Addr, Port: String);
var
  RunConApp: TWSocketApplication;
begin
  RunConApp := ConApplication as TWSocketApplication;
  RunConApp.Addr := Addr;
  RunConApp.Port := Port;
end;

Procedure RunStreamServer (Addr, Port: String);
begin
  try
    TConApplication.CreateInstance(TWSocketApplication);

    SetEventNotify (EventNotifier);
    SetHandler (PacketHandler);
    SetConsoleHandler (ConsoleCmdList);
    SetHostPort (Addr, Port);

    TConApplication.Run;
    TConApplication.Done;
  except
    on E:Exception do begin
        WriteLn('Exception ' + E.ClassName + ': ' + E.Message);
        if Assigned(ConApplication) then
            ConApplication.Terminated := True;
    end;
  end;
end;

end.

