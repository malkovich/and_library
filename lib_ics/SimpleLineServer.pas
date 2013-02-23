unit SimpleLineServer;

interface

uses
  Messages, Windows, SysUtils, Classes, WSocket, WinSock, LineServerUnit;

type
    TCmdHandler = function (Param: String; var Close: BOOL): String;

function BeginServer: HWND;
procedure RunServer;

function OpenLineServer (ServerWnd: HWND; Host,Port,Title: String): THandle;
function SetHandler (hServer: THandle; CmdName: String; Handler: TCmdHandler): BOOL;
function GetClientCount (hServer: THandle): Integer;

implementation

procedure DBG(Msg:String);
begin
  if IsConsole then
    WriteLn('[' + TimeToStr(Now) + '] ' + Msg)
  else
    OutputDebugStringA(PChar(Msg));
end;


Type
    TSimpleLineServer = class (TServerObject)
    private
      FServerName: String;
      HandleList: TStringList;
    Public
      constructor CreateSimple(Addr, Port, Title: String); 
      destructor  Destroy; override;
      function ServerName:String; override;
      function CreateClient:TClientObject; override;
    end;

    TSimpleLineClient = class(TClientObject)
        procedure CMDHandler(CommandVerb ,CommandTail : String); override;
    end;

var
  hWndMain: HWND;

procedure ClientDisconnectedEvent(var MsgRec : TMsg);
var
    Client : TSimpleLineClient;
    Server: TSimpleLineServer;
begin
    Server := TSimpleLineServer(MsgRec.wParam);
    Client := TSimpleLineClient(MsgRec.lParam);
    
    if NOT Assigned(Server) then exit;
    if NOT Assigned(Client) then exit;

    Server.DisconnectedClient(Client);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This is a callback routine called by windows when some events occurs.     }
{ We trap those events to close our application.                            }
function CtrlHandlerRoutine(CtrlType : DWORD) : DWORD; stdcall;
begin
    case CtrlType of
    CTRL_C_EVENT,            // User hit CTRL-C
    CTRL_BREAK_EVENT,        // User hit CTRL-BREAK
    CTRL_LOGOFF_EVENT,       // User log off his session
    CTRL_CLOSE_EVENT,        // Close signal
    CTRL_SHUTDOWN_EVENT :    // Window shutdown signal
        begin
            Result := 1;
            PostMessage(hWndMain, WM_QUIT, 0, 0);
        end;
    else
        Result := 0;
    end;
end;

//Message 00000081   WM_NCCREATE
//Message 00000083   WM_NCCALCSIZE
//Message 00000001   WM_CREATE
//Message 00000005   WM_MOVE
//Message 00000003   WM_SIZE
//Message 0000007C   WM_STYLECHANGING
//Message 0000007D   WM_STYLECHANGED
                                           
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MyWindowProc(
    ahWnd   : HWND; auMsg   : Integer; awParam : WPARAM; alParam : LPARAM): Integer; stdcall;
var
    MsgRec : TMsg;
begin
    Result := 0;  // This means we handled the message
    try
        MsgRec.hwnd    := ahWnd;
        MsgRec.message := auMsg;
        MsgRec.wParam  := awParam;
        MsgRec.lParam  := alParam;

        case auMsg of
        WM_CLIENT_DISCONNECTED:
            ClientDisconnectedEvent(MsgRec);
        else
            Result := DefWindowProc(ahWnd, auMsg, awParam, alParam)
        end;
    except
        on E:Exception do
            DBG('Exception ' + E.ClassName + ': ' + E.Message);
    end;
end;

var
    MyWindowClass : TWndClass = (style         : 0;
                                 lpfnWndProc   : @MyWindowProc;
                                 cbClsExtra    : 0;
                                 cbWndExtra    : 0;
                                 hInstance     : 0;
                                 hIcon         : 0;
                                 hCursor       : 0;
                                 hbrBackground : 0;
                                 lpszMenuName  : nil;
                                 lpszClassName : 'lpszClassName');

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MakeGuid: String;
var
  Guid: TGUID;
begin
  CreateGUID (Guid);
  Result := GuidToString (Guid);
end;

var
  ClassNameRamdom: String;

function BeginServer: HWND;
begin
    Result := 0;
    ClassNameRamdom := MakeGuid;
    MyWindowClass.lpszClassName := PChar(ClassNameRamdom);
    if Windows.RegisterClass(MyWindowClass) = 0 then Exit;
    Result := CreateWindowEx(WS_EX_TOOLWINDOW,
                               MyWindowClass.lpszClassName,
                               '',        { Window name   }
                               WS_POPUP,  { Window Style  }
                               0, 0,      { X, Y          }
                               0, 0,      { Width, Height }
                               0,         { hWndParent    }
                               0,         { hMenu         }
                               HInstance, { hInstance     }
                               nil);      { CreateParam   }
    if Result = 0 then  Exit;
    SetConsoleCtrlHandler(@CtrlHandlerRoutine, TRUE);
    hWndMain := Result;
end;

procedure RunServer;
var
    MsgRec : TMsg;
begin
    while GetMessage(MsgRec, 0, 0, 0) do
    begin
        TranslateMessage(MsgRec);
        DispatchMessage(MsgRec)
    end;
end;


function OpenLineServer (ServerWnd: HWND; Host,Port,Title: String): THandle;
var
  SimpleServer: TSimpleLineServer;
begin               
  SimpleServer := TSimpleLineServer.CreateSimple(Host, Port, Title);
  SimpleServer.CtrlWindow := ServerWnd;
  Result := THandle (SimpleServer);
end;

function SetHandler (hServer: THandle; CmdName: String; Handler: TCmdHandler): BOOL;
var
  SimpleServer: TSimpleLineServer absolute hServer;
begin
  Result := SimpleServer.HandleList.AddObject(CmdName, TObject(@Handler)) <> -1;
end;

function GetClientCount (hServer: THandle): Integer;
var
  SimpleServer: TSimpleLineServer absolute hServer;
begin
  Result := SimpleServer.FCliList.Count;
end;

///////////////      //////////////


function TSimpleLineServer.CreateClient:TClientObject;
begin
  Result := TSimpleLineClient.Create(self);
end;


constructor TSimpleLineServer.CreateSimple(Addr, Port, Title: String);
begin
  inherited Create (Addr, Port);
  FServerName := Title;
  HandleList := TStringList.Create;
end;

destructor TSimpleLineServer.Destroy;
begin
  HandleList.Free;
  inherited Destroy;
end;

function TSimpleLineServer.ServerName:String;
begin
  result := FServerName;
end;

procedure TSimpleLineClient.CMDHandler(CommandVerb ,CommandTail : String);
var
  ReturnStr: String;
  Handler: TCmdHandler;
  HandlerIndex: Integer;
  HandlerList: TStringList;
  LineServer: TSimpleLineServer;
  IsClose: BOOL;
begin
    IsClose := False;
    DBG ('>>> ' + CommandVerb + ' ' +CommandTail);

    if Length(CommandVerb) > 0 then
    begin
        CommandVerb := Trim(CommandVerb);
        if Length(CommandTail) > 0 then
        begin
            Delete(CommandTail, 1, 1);
            CommandTail := Trim(CommandTail);
        end;         

        LineServer := FServerObject as TSimpleLineServer;
        HandlerList := LineServer.HandleList;

        HandlerIndex := HandlerList.IndexOf(CommandVerb);
        if HandlerIndex = -1 then
        begin
          PostMessage(FCtrlWindow, WM_CLIENT_DISCONNECTED, 0, LParam(Self));
          EXIT;        
        end;

        Try 
          Handler := Pointer (HandlerList.Objects[HandlerIndex]);
          ReturnStr := Handler (CommandTail, IsClose);

          if ReturnStr = '' then
          begin
            FCliWSocket.SendStr('ERR' + CmdPrompt);
            EXIT;
          end;

          DBG ('<<< '+ReturnStr);

          FCliWSocket.SendStr(ReturnStr + CmdPrompt);
        finally
          if IsClose then
            PostMessage(FCtrlWindow, WM_CLIENT_DISCONNECTED, WParam(Self.FServerObject), LParam(Self));
        end;

    end;
end;

end.

