unit LineSocketClientUnit;

interface
uses
  windows, classes, forms, syncObjs, SysUtils,
  WSocket, winsock, TCacheQueueThread, md5;

Type
  TLineSocketClient = class
  protected
    FRecvLine: TStringList;
    FRetWhen: TStringList;
    FWSocket: TWSocket;
    IsDone: BOOL;
    procedure DataAvailable (Sender: TObject; ErrCode: Word);
    procedure SessionConnected(Sender: TObject; Error: Word);
    procedure SessionClosed(Sender: TObject; Error: Word);
    function  GetConnState: LongBool;
  public
    Constructor Create(AOwner:TComponent; Addr,Port: String);
    Destructor Destroy; override;
    function RawRequest(CMD: String; RetWhen: Array of String; Timeout: DWORD = 8000): LongBool;
    property RecvLine: TStringList read FRecvLine;
    property Socket:TWSocket read FWSocket;
    property Connected: LongBool read GetConnState;
  end;

  TBottleStoreMgrClient = class(TLineSocketClient)
    function GetTicket(BabyName: String):String;
    function IsAllRight:LongBool;
  end;

  TBottleDownloadClient = class(TLineSocketClient)
    function MOD_Download(BabyName, TicketStr: String; var POS: String; Timeout: DWORD = 8000):String;
  end;

  TFeedInterfaceBase = class(TLineSocketClient)
    function GetRetStrList(CMD, CMDID: String; RetWhen:array of String; RetListCount: Integer; var RetStrList: TStringList):LongBool;
    function MMC_Request(var VerMd5Str, TicketStr: String):LongBool;
    function TSL_Request(var MotherName: String):LongBool;
    function TSO_Request(IDStr: String; var MotherName: String):LongBool;
  end;                              

  TFeedInterfaceClient = class(TFeedInterfaceBase)
    function USR_Request(MotherName: String; var USRKeyBase:String; var TicketStr:String):LongBool;
    function LOG_Request(USRKeyBase, Offset, Base64Ret:String; var IDStr:String):LongBool;
    function LST_Request(IDStr: String; var ListStr:String):LongBool;
    function MOD_Request(IDStr, BabyName: String; var TicketStr:String):LongBool;
    function RET_Request(IDStr, BabyName, Offset, Base64Ret: String):LongBool;
  end;

  TLoginClient = class(TLineSocketClient)
    function Add(MotherName, IPAddr, MacAddr: string):LongBool;
    function Del(MotherName: String):LongBool;
  end;

  TBroadCastClient = class
  protected
    FWSocket: TWSocket;
    FPort: String;
    function GetConnected:BOOL;
  public
    Constructor Create(AOwner:TComponent; Port: String);
    Destructor Destroy; override;
    Procedure Send(Buff: PChar; Size: Integer); overload;
    Procedure Send(Msg: String); overload;
    Property Port: String Read FPort;
    Property Connected:BOOL read GetConnected;
  end;

implementation

function TBroadCastClient.GetConnected:BOOL;
begin
  Result := FWSocket.State = wsConnected;
end;

Constructor TBroadCastClient.Create(AOwner:TComponent; Port: String);
begin
  FPort := Port;

  FWSocket := TWSocket.Create(AOwner);
  FWSocket.Proto := 'udp';
  FWSocket.Addr := '255.255.255.255';
  FWSocket.Port := FPort;
  FWSocket.MultiCast := False;
  FWSocket.Connect;
end;

Destructor TBroadCastClient.Destroy;
begin
  FWSocket.Close;
  FWSocket.Free;
end;

Procedure TBroadCastClient.Send(Buff: PChar; Size: Integer);
begin
  FWSocket.Send(Buff, Size);
end;

Procedure TBroadCastClient.Send(Msg: String);
begin
  FWSocket.SendStr(Msg);
end;                 

//>>> USR {02D3C01F-BF30-4825-A83A-DE7AF41648AA}
//>>>ERR
//>>> MSN {02D3C01F-BF30-4825-A83A-DE7AF41648AA}
//DAT KNL 10242
//xOO6w6Osu7bTrbniwdnAz8LetcTnzbfXzOy12KOh……
//END
//>>>
function TBottleDownloadClient.MOD_Download(BabyName, TicketStr: String; var POS: String;  Timeout: DWORD = 8000):String;
var
  CMD, CmpStr: String;
  SL: TStringList;
  DataIndex: Integer;
  Found: BOOL;
begin
  result := '';
  CMD :=  BabyName + ' ' + TicketStr;
  if RawRequest(CMD, ['END','>>>ERR'], Timeout) then
  begin
    OutputDebugString(PChar('RawRequest SUCCEED FRecvLine.Count = ' + IntToStr(FRecvLine.Count)));
    if FRecvLine.Count >= 3 then
    begin
      Found := False;
      for DataIndex := 0 to FRecvLine.Count - 1 do
      begin
        CmpStr := FRecvLine[DataIndex];
        OutputDebugString(PChar('CmpStr = ' + CmpStr));
        if CompareMem(@CmpStr[1], PChar('>>>DAT'), 6) then
        begin
          Delete(CmpStr, 1, 3);
          FRecvLine[DataIndex] := CmpStr;
          OutputDebugString(PChar('CompareMem Found = ' + FRecvLine[DataIndex]));
          Found := True;
          Break;
        end;
      end;
      if not Found then DataIndex := 0;

      CMD := FRecvLine[DataIndex];

      SL := TStringList.Create;
      ExtractStrings([' '], [' '], PChar(CMD), SL);

      OutputDebugString(PChar(Format('ExtractStrings = %s; Count = %d', [CMD, SL.Count])));

      if SL.Count = 3 then
      begin
        POS := SL[1];
        Result := FRecvLine[DataIndex + 1];

        OutputDebugString(PChar(Format('Result: POS=%s; RetLen=%d', [POS, Length(Result)])));
      end;
      SL.Free;
    end;
  end;
end;    

//>>> ADD UserName 218.23.54.251 5F43ACD39F0A
//<<< SUC
function TLoginClient.Add(MotherName, IPAddr, MacAddr: string):LongBool;
var
  CMD: String;
begin
  Result := False;
  CMD := 'ADD ' + MotherName + ' ' + IPAddr + ' ' +  MacAddr;
  if RawRequest(CMD, ['>>>SUC','>>>ERR']) then
    if FRecvLine.Count >= 1 then
        if Trim(FRecvLine[0]) = '>>>SUC' then
            Result := True;
end;

//>>> DEL UserName
//<<< SUC
function TLoginClient.Del(MotherName: String):LongBool;
var
  CMD: String;
begin
  Result := False;
  CMD := 'DEL ' + MotherName;
  if RawRequest(CMD, ['>>>SUC','>>>ERR']) then
    if FRecvLine.Count >= 1 then
        if Trim(FRecvLine[0]) = '>>>SUC' then
            Result := True;
end;

//////////////////////////////////////
//////////////////////////////////////

var
  IteraCounter: Integer = 100;

function NumStr: String;
begin
  Result := IntToStr(IteraCounter);
  INC(IteraCounter);
end;


//[396] >>>MMC 144 SUC 192.168.100.144 8088 {B54FEF5B-74D1-45B0-84F1-9267C7FC39E9} f90a0d071ec392efd410f6ca277665f8
//专门处理：返回的最后一行是期待结果的
function TFeedInterfaceBase.GetRetStrList(CMD, CMDID: String; RetWhen:array of String; RetListCount: Integer; var RetStrList: TStringList):LongBool;
begin
  Result := False;

  if RawRequest(CMD, RetWhen) then
    if FRecvLine.Count >= 1 then
      if RetListCount = ExtractStrings([' '], [' '], PChar(FRecvLine[FRecvLine.Count-1]), RetStrList) then
        if RetStrList[1] = CMDID then
          if Trim(RetStrList[2]) = 'SUC' then
            Result := True;

  if not Result then
  begin
    OutputDebugString('      GetRetStrList ERROR!!!!!!!!!!!!!!!!!!!!!!!!!!!');
    OutputDebugString(PChar(FRecvLine.Text));
  end;

end;


const
    MAX_ADAPTER_NAME_LENGTH   =   256;
    MAX_ADAPTER_DESCRIPTION_LENGTH   =   128;
    MAX_ADAPTER_ADDRESS_LENGTH   =   8;

type
    TIP_ADDRESS_STRING   =   record
          IPstring:   array   [0..15]   of   Char;
    end;
    PIP_ADDRESS_STRING   =   ^TIP_ADDRESS_STRING; 
    TIP_MASK_STRING   =   TIP_ADDRESS_STRING;
    PIP_MASK_STRING   =   ^TIP_MASK_STRING; 

    PIP_ADDR_STRING   =   ^TIP_ADDR_STRING;
    TIP_ADDR_STRING   =   record
        Next:   PIP_ADDR_STRING;
        IpAddress:   TIP_ADDRESS_STRING;     //IP地址字符串 
        IpMask:   TIP_MASK_STRING;     //子网掩码字符串
        Context:   DWORD;   //Netword   table   entry
    end; 
    PIP_ADAPTER_INFO   =   ^TIP_ADAPTER_INFO; 
    TIP_ADAPTER_INFO   =   packed   record
          Next:   PIP_ADAPTER_INFO; 
          ComboIndex:   DWORD;
          AdapterName:   array   [0..MAX_ADAPTER_NAME_LENGTH   +   4-1]   of   Char;
          Description:   array   [0..MAX_ADAPTER_DESCRIPTION_LENGTH   +   4-1]   of   Char; 
          AddressLength:   UINT;
          Address:   array   [0..MAX_ADAPTER_ADDRESS_LENGTH-1]   of   BYTE;
          Index:   DWORD;
          dwType:   UINT; 
          DhcpEnabled:   UINT;
          CurrentIpAddress:   PIP_ADDR_STRING;
          IpAddressList:   TIP_ADDR_STRING;
          GatewayList:   TIP_ADDR_STRING;
          DhcpServer:   TIP_ADDR_STRING;
          HaveWins:   BOOL;
          PrimaryWinsServer:   TIP_ADDR_STRING;
          SecondaryWinsServer:   TIP_ADDR_STRING;
    end;

function   GetAdaptersInfo(pAdapterInfo:   PIP_ADAPTER_INFO;
                pOutBufLen:   PDWORD):   DWORD;   stdcall;
                external   'IPHLPAPI.DLL'   name  'GetAdaptersInfo';

function MakeMacsMD5Str():String;
const
  ERROR_BUFFER_OVERFLOW = 111;
  ERROR_SUCCESS = 0;
var
  pbuf:   PIP_ADAPTER_INFO;
  buflen:   DWORD;
  I: Integer;
  OutputTemp, TempMac, TempIP: String;
begin
  Result := '';
  buflen   :=   0;
  if GetAdaptersInfo(NIL, @bufLen) = ERROR_BUFFER_OVERFLOW then
  begin
    GetMem(pBuf, BufLen);

    if GetAdaptersInfo(pbuf, @bufLen) = ERROR_SUCCESS  then
    while pbuf <> nil do
    begin
      TempIP := pbuf.IpAddressList.IpAddress.IPstring;
      if TempIP <> '0.0.0.0' then
      begin
        TempMac := '';
        for I := 0 to pbuf.AddressLength - 1 do
          TempMac := TempMac + IntToHex(pbuf.Address[I], 2);

        OutputTemp := TempIP + ' ' + TempMac;
        Result := Result + ' ' + MD5Print(MD5String(OutputTemp));
      end;
      pbuf := pbuf.Next;
    end;

    FreeMem(pbuf);
  end;
  Result := TrimLeft(Result);
end;


//TSO 230 0FED69AE 0FE345AC
//TSO 230 SUC UserName
//TSO 230 ERR
function TFeedInterfaceBase.TSO_Request(IDStr: String; var MotherName: String):LongBool;
var
  CMD, CMDID: String;
  RetStrList: TStringList;
begin
  Result := False;
  if Trim(IDStr) = '' then exit;
  
  CMDID := NumStr;
  CMD := 'TSO ' + CMDID + ' ' + IDStr;
  RetStrList := TStringList.Create;
  if GetRetStrList(CMD, CMDID, ['>>>TSO'], 4, RetStrList) then
  begin
    MotherName := RetStrList[3];
    Result := True;
  end;
  RetStrList.Free;
end;

//MMC 234 DAEMON
//MMC 234 SUC 192.168.0.5 30 {02D3C01F-BF30-4825-A83A-DE7AF41648AA} ergdfgdfgdffdfsdf
//MMC 234 ERR
//>>>MMC 176 SUC 192.168.100.144 8088 {2A4238B7-1CDE-43CE-B2A2-B93621C169D6} f90a0d071ec392efd410f6ca277665f8

function TFeedInterfaceBase.MMC_Request(var VerMd5Str, TicketStr: String):LongBool;
var
  CMD, CMDID: String;
  RetStrList: TStringList;
begin
  Result := False;
  CMDID := NumStr;
  CMD := 'MMC ' + CMDID + ' DAEMON';
  RetStrList := TStringList.Create;
  if GetRetStrList(CMD, CMDID, ['>>>MMC'], 7, RetStrList) then
  begin
    TicketStr := RetStrList[3] + ' ' + RetStrList[4] + ' ' + RetStrList[5];
    VerMd5Str := RetStrList[6];
    Result := True;
  end;
  RetStrList.Free;
end;

//TSL 231 {26D7A28F-8F72-40CC-8749-08B8838CAF60} {26D7A28F-8F72-40CC-8749-08B8838CAF60}
//TSL 231 SUC UserName
//TSL 231 ERR
function TFeedInterfaceBase.TSL_Request(var MotherName: String):LongBool;
var
  CMD, CMDID: String;
  RetStrList: TStringList;
begin
  Result := False;
  CMDID := NumStr;
  CMD := 'TSL ' + CMDID + ' ' + MakeMacsMD5Str;
  RetStrList := TStringList.Create;
  if GetRetStrList(CMD, CMDID, ['>>>TSL'], 4, RetStrList) then
  begin
    MotherName := RetStrList[3];
    Result := True;
  end;
  RetStrList.Free;
end;

//USR 232 UserName
//USR 232 SUC FFEDEEAE 192.168.0.5 30 {26D7A28F-8F72-40CC-8749-08B8838CAF60}
function TFeedInterfaceClient.USR_Request(MotherName: String; var USRKeyBase:String; var TicketStr:String):LongBool;
var
  CMD, CMDID: String;
  RetStrList: TStringList;
begin
  Result := False;
  CMDID := NumStr;
  CMD := 'USR ' + CMDID + ' ' + MotherName;
  RetStrList := TStringList.Create;
  if GetRetStrList(CMD, CMDID, ['>>>USR'], 7, RetStrList) then
  begin
    USRKeyBase := RetStrList[3];
    TicketStr := RetStrList[4] + ' ' + RetStrList[5] + ' ' + RetStrList[6];
    Result := True;
  end;
  RetStrList.Free;
end;

//LOG 233 FFEDEEAE 40DE as;lkjp3sdaf320
//LOG 233 SUC 0FED69AE 0FE345AC
function TFeedInterfaceClient.LOG_Request(USRKeyBase, Offset, Base64Ret:String; var IDStr:String):LongBool;
var
  CMD, CMDID: String;
  RetStrList: TStringList;
begin
  Result := False;
  CMDID := NumStr;
  CMD := 'LOG ' + CMDID + ' ' + USRKeyBase + ' ' + Offset + ' ' + Base64Ret;
  RetStrList := TStringList.Create;
  if GetRetStrList(CMD, CMDID, ['>>>LOG'], 5, RetStrList) then
  begin
    IDStr := RetStrList[3] + ' ' + RetStrList[4];
    Result := True;
  end;
  RetStrList.Free;
end;

//LST 231 0FED69AE 0FE345AC
//LST 231 SUC "MSN APX"
function TFeedInterfaceClient.LST_Request(IDStr: String; var ListStr:String):LongBool;
var
  CMD, CMDID: String;
  RetStrList: TStringList;
begin
  Result := False;
  CMDID := NumStr;
  CMD := 'LST ' + CMDID + ' ' + IDStr;
  RetStrList := TStringList.Create;
  if GetRetStrList(CMD, CMDID, ['>>>LST'], 4, RetStrList) then
  begin
    ListStr := RetStrList[3];
    Delete(ListStr, 1, 1);
    Delete(ListStr, Length(ListStr), 1);
    Result := True;
  end;
  RetStrList.Free;
end;

//MOD 234 0FED69AE 0FE345AC MSNS
//MOD 234 SUC 192.168.0.5 30 {02D3C01F-BF30-4825-A83A-DE7AF41648AA}
function TFeedInterfaceClient.MOD_Request(IDStr, BabyName: String; var TicketStr:String):LongBool;
var
  CMD, CMDID: String;
  RetStrList: TStringList;
begin
  Result := False;
  CMDID := NumStr;
  CMD := 'MOD ' + CMDID + ' ' + IDStr + ' ' + BabyName;
  RetStrList := TStringList.Create;
  if GetRetStrList(CMD, CMDID, ['>>>MOD'], 6, RetStrList) then
  begin
    TicketStr := RetStrList[3] + ' ' + RetStrList[4] + ' ' + RetStrList[5];
    Result := True;
  end;
  RetStrList.Free;
end;



//RET 235 0FED69AE 0FE345AC MSNS 4FDE sdfp34jsdaf320975439
//RET 236 SUC
function TFeedInterfaceClient.RET_Request(IDStr, BabyName, Offset, Base64Ret: String):LongBool;
var
  CMD, CMDID: String;
  RetStrList: TStringList;
begin
  CMDID := NumStr;
  CMD := 'RET ' + CMDID + ' ' + IDStr + ' ' + BabyName + ' ' + Offset + ' ' + Base64Ret;
  RetStrList := TStringList.Create;
  try
    Result := GetRetStrList(CMD, CMDID, ['>>>RET'], 3, RetStrList);
    OutputDebugString('      GetRetStrList done!!!');
  except
    OutputDebugString('      GetRetStrList exception!!!');
    Result := False;
  end;
  RetStrList.Free;
end;


/////////////////////////////////////////////////////

function TBottleStoreMgrClient.GetTicket(BabyName: String):String;
begin
  result := '';

  if RawRequest(BabyName + ' 1', ['END','>>>ERR']) then
    if FRecvLine.Count > 2 then
      Result := FRecvLine[1];

//  WriteLn('GetTicket ' + BabyName + ' ' + result);
end;

function TBottleStoreMgrClient.IsAllRight:LongBool;
begin
  result := False;
  if Not assigned(FWSocket) then exit;  
  Result :=  wsConnected = FWSocket.State;
end;

///////////////////////////

Constructor TLineSocketClient.Create(AOwner:TComponent; Addr,Port: String);
var
  TickCount: DWORD;
begin
  FRecvLine := TStringList.Create;
  FRetWhen := TStringList.Create;

  FWSocket := TWSocket.Create(AOwner);
  FWSocket.Proto := 'tcp';
  FWSocket.Addr := Addr;
  FWSocket.Port := Port;
  FWSocket.LineMode := True;
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

Destructor TLineSocketClient.Destroy;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    FRecvLine.Free;
    FRetWhen.Free;

    if assigned(FWSocket) then
    if FWSocket.State in [wsConnecting, wsConnected, wsOpened, wsBound] then
    begin
      SL.Add('      1、CloseDelayed');
      FWSocket.CloseDelayed;

      SL.Add('      2、Start WaitForClose');
      FWSocket.WaitForClose;

      SL.Add('      2、Destroy End');
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

function  TLineSocketClient.GetConnState: LongBool;
begin
  Result := (FWSocket.State = wsConnected);
end;

procedure TLineSocketClient.DataAvailable (Sender: TObject; ErrCode: Word);
var
  WSocket : TWSocket absolute Sender;
  FRcvBuf: String;
  CmpLen: Integer;
  I, K : Integer;
  RetWhenStr, CmpStr: String;
begin
  { Remember: we use line mode. We will always receive complete lines     }
  with Sender as TWSocket do
        FRcvBuf := ReceiveStr;
  { Remove trailing CR/LF, if any }
  if (Length(FRcvBuf) > 1) and (FRcvBuf[Length(FRcvBuf)] = #10) and (FRcvBuf[Length(FRcvBuf) - 1] = #13) then
    SetLength(FRcvBuf, Length(FRcvBuf) - 2);

  if Trim(FRcvBuf) = '' then EXIT;
  FRecvLine.Add(FRcvBuf);

  if FRetWhen.Count = 0 then
  begin
    IsDone := True;
    exit;
  end;

  for I := 0 to FRetWhen.Count - 1 do
  begin
    RetWhenStr := FRetWhen[I];
    CmpLen := Length(RetWhenStr);

    for K := 0 to FRecvLine.Count - 1 do
    begin
      CmpStr := FRecvLine[K];
      if Length(CmpStr) >= CmpLen then
      if CompareMem(@CmpStr[1], @RetWhenStr[1], CmpLen) then
      begin
        IsDone := True;
        exit;
      end;
    end;

  end;      
end;

function TLineSocketClient.RawRequest(CMD: String; RetWhen: Array of String; Timeout: DWORD = 8000): LongBool;
var
  I, ArrayLength: Integer;
  BeginTick, UsedTick: DWORD;
begin
  Result := False;

  FRetWhen.Clear;
  ArrayLength := Length(RetWhen);
  for I := 0 to ArrayLength - 1 do
    FRetWhen.Add(RetWhen[I]);

  IsDone := False;
  FRecvLine.Clear;

  if FWSocket.State <> wsConnected then
  begin
    OutputDebugString(PChar('Not Connected, Break Sendling'));
    exit;
  end;
  FWSocket.SendLine(CMD);

  UsedTick := 0;
  BeginTick := GetTickCount;
  repeat
    sleep(10);
    application.ProcessMessages;
    if IsDone then Break;
    UsedTick := GetTickCount - BeginTick;
  until UsedTick > Timeout;

  if UsedTick > 50 then
    outputDebugString(PChar('RawRequest delay : ' + IntToStr(UsedTick)));

  Result := FRecvLine.Count > 0;
end;



end.
