unit MsgRecv;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, ExtCtrls, Controls, Forms,
  Dialogs, TCacheQueueThread;

const
  DEFAULT_MESSAGE_ID = $13579;
  SET_CAPTION_MESSAGE_ID = $24680;
  PEER_CLOSE_MESSAGE_ID = $123456;
  DEFAULT_SERVER_CAPTION ='default_server_caption';

type

  TMsgTransClass = class(TForm)
  protected
    FRemoteStr:String;
    FRemoteHandle:THandle;
    CacheQueue :TCacheQueueClass;
    FOnDisconnect, FOnReConnect :TNotifyEvent;
    FOnRemoteCaptionChange :TNotifyEvent;
    FOnReset :TNotifyEvent;
    FDisconnectCounter:DWORD;
    procedure SetRecvNotify(OnRecv :TDataHandleOBJ);
    procedure SetRecvProc(OnRecv :TDataHandlePRO);
    procedure CopyDataAvailable(var M:TWMCopyData);message WM_COPYDATA;
    procedure WndProc(var Message: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Send(Buffer:Pointer; Length:DWORD):LONGBOOL;virtual;
    function SetPeerRemoteCaption(ACapStr:string):LONGBOOL;
    function ShutDown:LONGBOOL;

    property RemoteCaption:string read FRemoteStr write FRemoteStr;
    property OnRecvNotify:TDataHandleOBJ write SetRecvNotify;
    property OnRecvProc:TDataHandlePRO write SetRecvProc;
    property OnDisconnect:TNotifyEvent write FOnDisconnect;
    property OnReConnect:TNotifyEvent write FOnReConnect;
    property OnRemoteCaptionChange:TNotifyEvent write FOnRemoteCaptionChange;
    property OnReset:TNotifyEvent write FOnReset;
  end;

  TMsgTransClient = class
  protected
    NewConnect :LONGBOOL;
    FOnReset :TNotifyEvent;
    procedure OnReConnectEvent(Sender:TObject);
    procedure OnDisConnectEvent(Sender:TObject);
    procedure OnRemoteCaptionChangeEvent(Sender:TObject);
    procedure OnResetEvent(Sender:TObject);
    procedure SetRecvNotify(OnRecv :TDataHandleOBJ);
    procedure SetRecvProc(OnRecv :TDataHandlePRO);
  public
    MsgTrans :TMsgTransClass;
    constructor Create(AOwner: TComponent); 
    destructor Destroy; override;
    function Send(Buffer:Pointer; Length:DWORD):LONGBOOL;
    function SendLine(MsgText: String):LONGBOOL;
    procedure Close;
    property OnReset:TNotifyEvent write FOnReset;
    property OnRecv:TDataHandleOBJ write SetRecvNotify;
    property OnRecvProc:TDataHandlePRO write SetRecvProc;
  end;

  TMsgTransServer = class
  protected
    SendErrCount :DWORD;      
    FOnReset :TNotifyEvent;
    Timer:TTimer;
    procedure OnRemoteCaptionChangeEvent(Sender:TObject);
    procedure OnResetEvent(Sender:TObject);
    procedure SetRecvNotify(OnRecv :TDataHandleOBJ);
    procedure SetRecvNotify2(OnRecv :TDataHandlePRO);
    procedure OnTimer(Sender:TObject);
  public
    MsgTrans :TMsgTransClass;
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Send(Buffer:Pointer; Length:DWORD):LONGBOOL;
    function SendLine(MsgText: String):LONGBOOL;   
    procedure Close;
    property OnReset:TNotifyEvent write FOnReset;
    property OnRecv:TDataHandleOBJ write SetRecvNotify;
    property OnRecvPro:TDataHandlePRO write SetRecvNotify2;
  end;

implementation

{$R *.dfm}

uses ActiveX;


function GetRandomCaption:string;
var
  TmpGUID: TGUID;
begin   
  if CoCreateGUID(TmpGUID) = S_OK then
    result := GUIDToString(TmpGUID)
  else
    result := 'default_caption_' + IntToStr(GetCurrentProcessID);
end;

///////////////////////////////////////


procedure TMsgTransServer.OnResetEvent(Sender :TObject);
begin
  MsgTrans.Caption :=DEFAULT_SERVER_CAPTION;
  if Assigned(FOnReset) then
    FOnReset(Self);
end;

procedure TMsgTransServer.SetRecvNotify(OnRecv :TDataHandleOBJ);
begin
  MsgTrans.OnRecvNotify := OnRecv;
end;

procedure TMsgTransServer.SetRecvNotify2(OnRecv :TDataHandlePRO);
begin
  MsgTrans.OnRecvProc := OnRecv;
end;

procedure TMsgTransServer.OnRemoteCaptionChangeEvent(Sender:TObject);
begin
  MsgTrans.Caption := GetRandomCaption;
  MsgTrans.SetPeerRemoteCaption(MsgTrans.Caption);
end;

procedure TMsgTransServer.OnTimer(Sender:TObject);
begin
  if NOT MsgTrans.SetPeerRemoteCaption(MsgTrans.Caption) then
    MsgTrans.Caption :=DEFAULT_SERVER_CAPTION;
end;
 
constructor TMsgTransServer.Create(AOwner: TComponent); 
begin
  inherited Create;            
  MsgTrans := TMsgTransClass.Create(AOwner);
  MsgTrans.OnRemoteCaptionChange :=OnRemoteCaptionChangeEvent;
  MsgTrans.OnReset := OnResetEvent;
  MsgTrans.Caption := DEFAULT_SERVER_CAPTION;
  Timer:=TTimer.Create(MsgTrans);
  Timer.OnTimer := OnTimer;
  Timer.Interval := 5000;
end;

destructor TMsgTransServer.Destroy; 
begin
 Timer.Enabled := False;
 Timer.FreeOnRelease;
 MsgTrans.FreeOnRelease;
 inherited Destroy;
end;

function TMsgTransServer.SendLine(MsgText: String):LONGBOOL;
begin
  MsgText := MsgText + #13#10;
  Result := Send(PChar(MsgText), Length(MsgText));
end;

function TMsgTransServer.Send(Buffer:Pointer; Length:DWORD):LONGBOOL;
begin
  result := MsgTrans.Send(Buffer, Length);
  if result then
  begin
    SendErrCount := 0;
  end else
  begin
    Inc(SendErrCount);
    if SendErrCount = 2 then Close;
  end;    
end;
   
procedure TMsgTransServer.Close;
begin
  MsgTrans.ShutDown;
  MsgTrans.Caption := DEFAULT_SERVER_CAPTION;
  if Assigned(MsgTrans.FOnReset) then
    MsgTrans.FOnReset(self);
end;

/////////////////////////////////////////////////

procedure TMsgTransClient.SetRecvNotify(OnRecv :TDataHandleOBJ);
begin
  MsgTrans.OnRecvNotify := OnRecv;
end;

procedure TMsgTransClient.SetRecvProc(OnRecv :TDataHandlePRO);
begin
  MsgTrans.OnRecvProc := OnRecv;
end;

procedure TMsgTransClient.OnRemoteCaptionChangeEvent(Sender:TObject);
begin
  NewConnect := False;
end;

procedure TMsgTransClient.OnReConnectEvent(Sender:TObject);
begin
  NewConnect := True;
end;
procedure TMsgTransClient.OnDisConnectEvent(Sender:TObject);
begin
  MsgTrans.RemoteCaption := DEFAULT_SERVER_CAPTION;
end;


procedure TMsgTransClient.Close;
begin
  MsgTrans.ShutDown;
  MsgTrans.RemoteCaption := DEFAULT_SERVER_CAPTION;
  if Assigned(MsgTrans.FOnReset) then
    MsgTrans.FOnReset(self);
end;


procedure TMsgTransClient.OnResetEvent(Sender :TObject);
begin
  MsgTrans.RemoteCaption :=DEFAULT_SERVER_CAPTION;
  NewConnect := True;
  if Assigned(FOnReset) then
    FOnReset(Self);
end;


constructor TMsgTransClient.Create(AOwner: TComponent);
begin              
  inherited Create;            
  MsgTrans := TMsgTransClass.Create(AOwner);
  MsgTrans.OnReConnect := OnReConnectEvent;
  MsgTrans.OnDisconnect := OnDisConnectEvent;
  MsgTrans.OnRemoteCaptionChange :=OnRemoteCaptionChangeEvent;
  MsgTrans.OnReset := OnResetEvent;
  MsgTrans.Caption := GetRandomCaption;
  MsgTrans.RemoteCaption := DEFAULT_SERVER_CAPTION;
  NewConnect := True;
end;

destructor TMsgTransClient.Destroy;
begin
 MsgTrans.Free;
 inherited Destroy;  
end;

function TMsgTransClient.SendLine(MsgText: String):LONGBOOL;
begin
  MsgText := MsgText + #13#10;
  Result := Send(PChar(MsgText), Length(MsgText));
end;

function TMsgTransClient.Send(Buffer:Pointer; Length:DWORD):LONGBOOL;
begin
  if NewConnect then
  begin
    MsgTrans.SetPeerRemoteCaption(MsgTrans.Caption);
    result := MsgTrans.Send(Buffer, Length);
  end
  else
    result := MsgTrans.Send(Buffer, Length);
end;

///////////////////////////////////////

procedure TMsgTransClass.WndProc(var Message: TMessage);
begin
  with Message do
    case Msg of
      WM_ACTIVATEAPP, WM_ACTIVATE, WM_SETFOCUS, WM_KILLFOCUS: exit;
    end;  
  inherited;
end;

procedure TMsgTransClass.CreateParams(var Params: TCreateParams);
begin
  Params.Style := Params.Style or SW_SHOWNOACTIVATE;
  inherited;
end;

constructor TMsgTransClass.Create(AOwner: TComponent);
begin
  CacheQueue :=TCacheQueueClass.Create;
  inherited Create(AOwner);
  self.Visible := False;
  self.Width := 1;
  self.Height := 1;
  self.SetZOrder(False);
  self.BorderStyle := bsNone;
end;

destructor TMsgTransClass.Destroy;
begin
 ShutDown;
 CacheQueue.Terminate;
 CacheQueue.WaitUtilTerminate;
 inherited Destroy;
end;

procedure TMsgTransClass.SetRecvNotify(OnRecv :TDataHandleOBJ);
begin
  CacheQueue.OnDataHandle := OnRecv;
end;
procedure TMsgTransClass.SetRecvProc(OnRecv :TDataHandlePRO);
begin
  CacheQueue.OnDataHandlePro := OnRecv;
end;

procedure TMsgTransClass.CopyDataAvailable(var M:TWMCopyData);
begin
  With M.CopyDataStruct^ do
  begin
    case dwData of
    DEFAULT_MESSAGE_ID:
      begin
        if Assigned(lpData) then
          M.Result := DEFAULT_MESSAGE_ID;
        CacheQueue.PushQue(nil, Pchar(lpData)[0], cbData);
      end;
    SET_CAPTION_MESSAGE_ID:
      begin
        FRemoteStr:= StrPas(lpData);
        if Assigned(FOnRemoteCaptionChange) then
          FOnRemoteCaptionChange(self);
      end;
    PEER_CLOSE_MESSAGE_ID:
      begin
        if Assigned(FOnReset) then
          FOnReset(self);  
      end;
    end;
  end;
end;

function TMsgTransClass.Send(Buffer:Pointer; Length:DWORD):LONGBOOL;
var
  DataBuffer :TCopyDataStruct;
  dwResult :DWORD;
  CLA:String;
begin
  result := False;
  if FRemoteHandle = 0 then
  begin
    CLA := ClassName;
    FRemoteHandle := FindWindow(Pchar(CLA), Pchar(FRemoteStr));

    if FRemoteHandle = 0 then
    begin
      INC(FDisconnectCounter);
      if FDisconnectCounter = 2 then
      begin
        if Assigned(FOnDisconnect) then
          FOnDisconnect(self);
      end;
      exit;
    end else
    begin
      if Assigned(FOnReConnect) then
        FOnReConnect(self);
    end;
  end else
    FDisconnectCounter := 0;


  DataBuffer.dwData := DEFAULT_MESSAGE_ID;
  DataBuffer.cbData := Length;
  DataBuffer.lpData := Buffer;
  if 0 = SendMessageTimeout(FRemoteHandle, WM_COPYDATA, 0, Longint(@DataBuffer), SMTO_ABORTIFHUNG, 3000, dwResult) then
    FRemoteHandle := 0
  else
    Result := True;
end;

function TMsgTransClass.SetPeerRemoteCaption(ACapStr:string):LONGBOOL;
var
  DataBuffer :TCopyDataStruct;
  dwResult :DWORD;
  CLA:String;
begin
  result := False;
  if FRemoteHandle = 0 then
  begin
    CLA := ClassName;
    FRemoteHandle := FindWindow(Pchar(CLA), Pchar(FRemoteStr));
  end;
  if FRemoteHandle = 0 then exit;

  ACapStr := ACapStr + #0;
  DataBuffer.dwData := SET_CAPTION_MESSAGE_ID;
  DataBuffer.cbData := Length(ACapStr) + 1;
  DataBuffer.lpData := @ACapStr[1];
  if 0 = SendMessageTimeout(FRemoteHandle, WM_COPYDATA, 0, Longint(@DataBuffer), SMTO_ABORTIFHUNG, 3000, dwResult) then
    FRemoteHandle := 0
  else
    Result := True;
end;

function TMsgTransClass.ShutDown():LONGBOOL;
var
  DataBuffer :TCopyDataStruct;
  dwResult :DWORD;
  CLA:String;
begin
  result := False;
  if FRemoteHandle = 0 then
  begin
    CLA := ClassName;
    FRemoteHandle := FindWindow(Pchar(CLA), Pchar(FRemoteStr));
  end;
  if FRemoteHandle = 0 then exit;

  CLA := 'I_WANT_TO_CLOSE';
  DataBuffer.dwData := PEER_CLOSE_MESSAGE_ID;
  DataBuffer.cbData := Length(CLA) + 1;
  DataBuffer.lpData := @CLA[1];
  if 0 = SendMessageTimeout(FRemoteHandle, WM_COPYDATA, 0, Longint(@DataBuffer), SMTO_ABORTIFHUNG, 3000, dwResult) then
    FRemoteHandle := 0
  else
    Result := True;
end;




end.
