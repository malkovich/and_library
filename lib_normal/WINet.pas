//*****************************************************************//
//                                                                 //
//  TWinWrap - Delphi wrapper for WinInet.DLL                      //
//  Copyright© BrandsPatch LLC                                     //
//  http://www.explainth.at                                        //
//                                                                 //
// See http://msdn2.microsoft.com/en-us/library/aa385473.aspx      //
// for a detailed reference to all WinInet functions               //
//                                                                 //
// See RFC1867 for details on how multipart-form data are          //
// are transferred. The RFC is easily available on the             //
// Internet, e.g. at                                               //
// http://www.cse.ohio-state.edu/cgi-bin/rfc/rfc1867.html          //
//                                                                 //
//  All Rights Reserved                                            //
//                                                                 //
//  Permission is granted to use, modify and redistribute          //
//  the code in this Delphi unit on the condition that this        //
//  notice is retained unchanged.                                  //
//                                                                 //
//  BrandsPatch  declines all responsibility for any losses,       //
//  direct or indirect, that may arise  as a result of using       //
//  this code.                                                     //
//                                                                 //
//*****************************************************************//
unit WINet;

interface

uses Windows,SysUtils,Classes,WinINet;

type TProxyServerTypes = (pstPreConfig,pstDirect,pstGateway,pstProxy);
//type of proxy server in use

type TWinWrapVerbs = (wwvGET,wwvPOST,wwvMPOST);
     //which HTTP verb do we want to use
     TWinWrapCallBackReason = (wwcbrNil,wwcbrResolving,wwcbrResolved,
                               wwcbrConnecting,wwcbrConnected,
                               wwcbrWriting,wwcbrWritten,
                               wwcbrReading,wwcbrRead,
                               wwcbrClosing,wwcbrClosed);
     //why did the callback occur

     TWinWrapUPD = (wwupdUser,wwupdUPwd,wwupdProxy,wwupdProxyPwd);
     //password information. Proxy passwords are quite rare
     TWinWrapErrorCauses = (wwecNil,wwecAttemptConnect,wwecOpen,wwecConnect,
                            wwecOpenRequest,wwecExecRequest,wwecEndRequest,
                            wwecTimeOut,wwecUPD,
                            wwecStatus,wwecContent,
                            wwecReadFile,wwecWriteFile);
     //if there was an error it will be a value > wwecNil

     TOnCallBack = procedure(AReason:TWinWrapCallBackReason) of Object;
     //the prototype for the OnCallBack event

type TWinWrapInfo = record
  wwiAgent:PChar;
  //A string to identify the request source in sever logs
  wwiAttFile:PChar;
  //Name of Attachment (Multipart POST only)
  wwiAttMem:Pointer;
  {Pointer to Attachment memory stream. You must
   supply this before executing the request. Remember
   to free the memory resource once you are done -
   see our example}
  wwiAttSize:Integer;
  //size of the attachment
  wwiAttTarget:PChar;
  {the name used to identify the attachment. This will turn
   up in PHP as $_FILES[wwiAttTarget]. You would use properties
   such as [['name'], ['size'] etc to manipulate the attachment}
  wwiError:Integer;
  //errors will be reported here
  wwiParams:PChar;//required for wwvPOST only
  wwiScript:PChar;
  {required for all methods. For wwvGET this must
   consist of the script to run followed by a ? and
   then by the query to pass to the script - urlencoded
   for safety}
  wwiStatus:Integer;
  //the HTTP status response number, e.g. 200
  wwiURL:PChar;//the url
  wwiUPD:array[TWinWrapUPD] of PChar;//passwords and usernames, not tested
  wwiVerb:TWinWrapVerbs;
  //specify wwvGET, wwvPOST or wwvMPOST
  wwiTimeOut:Byte;//in seconds
  wwiSecure:Boolean;
  //set to communicate using HTTPS - not tested
  wwiCause:TWinWrapErrorCauses;
  //check this to see at what step the error occurred
end;

type EWinWrap = class(Exception);
     TWhenCallBack = procedure(AReason:TWinWrapCallBackReason) of Object;
     TWhenSized = procedure(ASize:Integer) of Object;

type TWinWrap = class(TObject)
private
  FNet:hInternet;
  FRequest:hInternet;
  FSession:hInternet;
  FCallBack:Integer;
  FSize:Integer;
  FTotal:Integer;
  FPort:Integer;
  FOnCallBack:TOnCallBack;

  FAttBound:String;
  FContent:String;
  FProxy:String;
  FUpReqHead:String;
  FData:array[0..4095] of Char;

  FInfo:TWinWrapInfo;
  FResponse:TMemoryStream;
  FSized:TWhenSized;

  function DetectProxyServer:TProxyServerTypes;
  function FetchHeader(AFlags:Integer):Boolean;
  function FixContentType:Boolean;

  function AssignError(ACause:TWinWrapErrorCauses):Boolean;
  function ConfigureRequest:Boolean;
  function OpenConnection:Boolean;
  function OpenRequest:Boolean;
  function ReadResponse:Boolean;
  function StatusGood:Boolean;

  function PerformGet:Boolean;
  function PerformPost:Boolean;

  function BuildAttachment:TMemoryStream;
  function PortToUse(ASecure:Boolean):Integer;
  function SendAttachment:Boolean;
public
  property Content:String read FContent;
  property OnCallBack:TOnCallBack write FOnCallBack;
  property OnSized:TWhenSized write FSized;
  property Response:TMemoryStream read FResponse;
  constructor CreateEx;
  destructor Destroy;override;

  procedure CleanUp;
  function Execute(var AInfo:TWinWrapInfo):Boolean;
  procedure UseThisPort(APort:Integer);
end;

implementation

type TProtoNames = array[Boolean] of PChar;
     TDefaultPorts = array[Boolean] of Integer;
const ProtoNames:TProtoNames = ('http','https');
      DefaultPorts:TDefaultPorts = (INTERNET_DEFAULT_HTTP_PORT,INTERNET_DEFAULT_HTTPS_PORT);
      IN_S_H = INTERNET_SERVICE_HTTP;
      D_C_T = 'Content-Type:application/x-www-form-urlencoded';
      D_C_T_S = Length(D_C_T);
      HSR_INITIATE = $00000008;
      BUFFER_SIZE = 4096;

procedure StatusCallback(ASession:hInternet;AContext,AIS:DWord;AInfo:Pointer;ASIN:DWord);stdcall;
var AReason:TWinWrapCallBackReason;
    AWCB:TWhenCallBack absolute AContext;
begin
  case AIS of
    INTERNET_STATUS_RESOLVING_NAME:AReason:=wwcbrResolving;
    INTERNET_STATUS_NAME_RESOLVED:AReason:=wwcbrResolved;
    INTERNET_STATUS_CONNECTING_TO_SERVER:AReason:=wwcbrConnecting;
    INTERNET_STATUS_CONNECTED_TO_SERVER:AReason:=wwcbrConnected;
    INTERNET_STATUS_SENDING_REQUEST:AReason:=wwcbrWriting;
    INTERNET_STATUS_REQUEST_SENT:AReason:=wwcbrWritten;
    INTERNET_STATUS_RECEIVING_RESPONSE:AReason:=wwcbrReading;
    INTERNET_STATUS_RESPONSE_RECEIVED:AReason:=wwcbrRead;
    INTERNET_STATUS_CLOSING_CONNECTION:AReason:=wwcbrClosing;
    INTERNET_STATUS_CONNECTION_CLOSED:AReason:=wwcbrClosed;
    else exit;
  end;
  AWCB(AReason);
end;
//------------------------------------------------------------------------------
constructor TWinWrap.CreateEx;
begin
  Create;
  FResponse:=TMemoryStream.Create;
end;

destructor TWINWrap.Destroy;
begin
  CleanUp;
  FResponse.Free;
  inherited;
end;
//..............................................................................
function TWINWrap.DetectProxyServer:TProxyServerTypes;
var AInfo:String;

  function FixProxyInfo:TProxyServerTypes;
  var PIPI:PInternetProxyInfo;
      ALen:DWORD;
  begin
    ALen:=BUFFER_SIZE;
    PIPI:=AllocMem(ALen);
    try
      if InternetQueryOption(nil,INTERNET_OPTION_PROXY,PIPI,ALen) then
      with PIPI^ do
      begin
        Result:=TProxyServerTypes(dwAccessType);
        if (Result = pstProxy) then AInfo:=lpszProxy;
      end;
    finally ReAllocMem(PIPI,0) end;
  end;

  function ExtractPertinent(AProt:String):String;
  var APos:Integer;
  begin
    Result:=AInfo;
    AProt:=Format('%s=',[AProt]);
    APos:=Pos(AProt,Result);
    if (APos > 0) then Delete(Result,1,APos + Length(AProt));
    APos:=Pos(';',Result); // In the registry, multiple protocols are ';' delimited ...
    if (APos > 0) then Delete(Result,APos,99999);
    APos:=Pos(' ',Result);
    // ... but the return value may be space delimited. Depends on the OS version
    if (APos > 0) then Delete(Result,APos,99999);
    APos:=Pos(':',Result);

    if (APos = 0) then SetLength(Result,0);
  end;

begin
  Result:=FixProxyInfo;
  if (Result = pstProxy) then FProxy:=ExtractPertinent(ProtoNames[FInfo.wwiSecure]);
end;

procedure TWinWrap.CleanUp;
begin
  if (FNet <> nil) then InternetCloseHandle(FNet);
  FNet:=nil;
  FSized:=nil;
  FResponse.Clear;

  FSize:=0;
  FTotal:=0;

  SetLength(FAttBound,0);
  SetLength(FContent,0);
  SetLength(FProxy,0);
  SetLength(FUpReqHead,0);
  FillChar(FData,sizeof(FData),0);
  FillChar(FInfo,sizeof(FInfo),0);
end;
//******************************************************************************
function TWinWrap.FetchHeader(AFlags:Integer):Boolean;
var BufLen,Index:DWORD;
begin
  Index:=0;
  BufLen:=BUFFER_SIZE;
  Result:=HttpQueryInfo(FRequest,AFlags,@FData,BufLen,Index);
end;

function TWinWrap.FixContentType:Boolean;
begin
  Result:=FetchHeader(HTTP_QUERY_CONTENT_TYPE);
  if Result then FContent:=FData else AssignError(wwecContent);
end;

function TWinWrap.PortToUse(ASecure:Boolean):Integer;
begin
  if (FPort <> 0) then Result:=FPort else Result:=DefaultPorts[ASecure];
end;

function TWinWrap.ReadResponse:Boolean;
var ASize,ARead:DWORD;
    ABuffer:Pointer;
begin
  FResponse.Clear;
  ASize:=BUFFER_SIZE;
  FTotal:=0;
  ABuffer:=AllocMem(ASize);
  try
    repeat
      if not(InternetReadFile(FRequest,ABuffer,ASize,ARead)) then
      begin
        AssignError(wwecReadFile);
        break;
      end;

      if (ARead > 0) then
      begin
        FResponse.WriteBuffer(ABuffer^,ARead);
        inc(FTotal,ARead);
        if Assigned(FSized) then FSized(FTotal);
      end;
    until (ARead = 0);
  finally
    ReAllocMem(ABuffer,0);
    FResponse.Position:=0;
  end;
  Result:=(FInfo.wwiError = 0);
end;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
function TWinWrap.AssignError(ACause:TWinWrapErrorCauses):Boolean;
begin
  with FInfo do
  begin
    wwiError:=GetLastError;
    if (wwiError = 0) then wwiError:=-ord(ACause);
    SetLastError(0);
    wwiCause:=ACause;
    Result:=False;
  end;
end;
//''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
type TUPDOptions = array[TWinWrapUPD] of Byte;
const UPDOptions:TUPDOptions = (INTERNET_OPTION_USERNAME,INTERNET_OPTION_PASSWORD,
                                INTERNET_OPTION_PROXY_USERNAME,
                                INTERNET_OPTION_PROXY_PASSWORD);

function TWinWrap.ConfigureRequest:Boolean;
var i:TWinWrapUPD;

  function SetTimeOuts(ATimeOut:Integer):Boolean;
  begin
    if (ATimeOut < 1) or (ATimeOut > 30) then ATimeOut:=30;
    ATimeOut:=ATimeOut*1000;
    Result:=InternetSetOption(FRequest,INTERNET_OPTION_CONNECT_TIMEOUT,@ATimeOut,sizeof(Integer)) and
            InternetSetOption(FRequest,INTERNET_OPTION_RECEIVE_TIMEOUT,@ATimeOut,sizeof(Integer)) and
            InternetSetOption(FRequest,INTERNET_OPTION_SEND_TIMEOUT,@ATimeOut,sizeof(Integer));

    if not(Result) then AssignError(wwecTimeOut);
  end;

  function SetUPD(AOption:Integer;AUPD:PChar):Boolean;
  begin
    Result:=InternetSetOption(FRequest,AOption,AUPD,Length(AUPD));
    if not(Result) then AssignError(wwecUPD);
  end;

begin
  with FInfo do
  if SetTimeOuts(wwiTimeOut) then
  begin
    Result:=True;
    for i:=Low(i) to High(i) do
    begin
      Result:=Result and ((wwiUPD[i] = nil) or SetUPD(UPDOptions[i],wwiUPD[i]));
      if not(Result) then break;
    end;
  end else Result:=False;
end;

function TWinWrap.OpenConnection:Boolean;
var APST:TProxyServerTypes;

  function WW_AttemptConnect:Boolean;
  begin
    Result:=(CompareText(FInfo.wwiURL,'localhost') = 0) or (InternetAttemptConnect(0) = ERROR_SUCCESS);
    if not(Result) then AssignError(wwecAttemptConnect);
  end;

  function WW_InternetOpen:Boolean;
  begin
    with FInfo do
    FNet:=InternetOpen(wwiAgent,ord(APST),PChar(FProxy),nil,0);
    Result:=(FNet <> nil);
    if Result then
    begin
      InternetSetStatusCallBack(FNet,@StatusCallBack);
      if InternetSetOption(FNet,65,@Result,1) then Beep;
    end else AssignError(wwecOpen);
  end;

  function WW_InternetConnect:Boolean;
  begin
    with FInfo do
    FSession:=InternetConnect(FNet,wwiURL,PortToUse(wwiSecure),'','',IN_S_H,0,FCallBack);
    Result:=(FSession <> nil);
    if not(Result) then AssignError(wwecConnect);
  end;

begin
  if WW_AttemptConnect then
  begin
    APST:=DetectProxyServer;
    SetLastError(0);
    Result:=WW_InternetOpen and WW_InternetConnect;
  end else Result:=False;
end;

type TOpenVerbs = array[Boolean] of PChar;
const OpenVerbs:TOpenVerbs = ('POST','GET');
      HFlags = INTERNET_FLAG_NO_CACHE_WRITE or
               INTERNET_FLAG_RELOAD;
      HGZip = 'Accept-Encoding: gzip,deflate';

function TWinWrap.OpenRequest:Boolean;
var i:DWORD;
begin
  with FInfo do
  begin
    i:=ord(wwiSecure)*INTERNET_FLAG_SECURE or HFlags;
    FRequest:=HTTPOpenRequest(FSession,OpenVerbs[wwiVerb = wwvGET],wwiScript,nil,nil,nil,i,FCallBack);
  end;
  Result:=(FRequest <> nil);
  if not(Result) then AssignError(wwecOpenRequest);
end;

function TWinWrap.StatusGood:Boolean;
begin
  Result:=FetchHeader(HTTP_QUERY_STATUS_CODE) and
          (StrToIntDef(FData,-1) = HTTP_STATUS_OK);
  FInfo.wwiStatus:=StrToIntDef(FData,-1);

  if FetchHeader(HTTP_QUERY_CONTENT_LENGTH) then FSize:=StrToIntDef(FData,0)
  else FSize:=0;
  if Assigned(FSized) then FSized(-FSize);
end;
//------------------------------------------------------------------------------
const AttDisp ='Content-Disposition:form-data';
      AttType = 'Content-Type:application/octet-stream';
      AttMaskH = '%s'#13#10'%s;name="%s";filename="%s"'#13#10'%s'#13#10#13#10;

function TWinWrap.BuildAttachment:TMemoryStream;
var AHeader,AFooter,ATail:String;

  procedure PrepareHelperFields;
  var i,j:Integer;
  begin
    i:=GetTickCount;
    j:=i + (abs(i) mod 200);
    FAttBound:=Format('---%0.8x-%0.8x',[i,j]);
    {The attachment boundary must be a unique byte sequence starting
     with three - characters where it is specified and five where it
     is used as a delimiter. Our boundary sequence is generated using
     the system clock and has always worked for all types of file
     transfers. You may want to change the way it is generated if you
     ever encounter a conflict - i.e. the sequence occurs in a file
     you are trying to upload. In our experience this is very very
     unlikely.}
    FUpReqHead:=Format('Content-Type:multipart/form-data;boundary=%s',[FAttBound]);
    FAttBound:=Format('--%s',[FAttBound]);
  end;

begin
  PrepareHelperFields;
  with FInfo do
  try
    AHeader:=Format(AttMaskH,[FAttBound,AttDisp,wwiAttTarget,wwiAttFile,AttType]);
  except on E:Exception do OutputDebugString(PChar((E.Message))) end;
  AFooter:=#13#10;
  ATail:=Format('%s--'#13#10,[FAttBound]);

  Result:=TMemoryStream.Create;
  with Result do
  begin
    WriteBuffer(AHeader[1],Length(AHeader));
    with FInfo do WriteBuffer(wwiAttMem^,wwiAttSize);
    WriteBuffer(AFooter[1],2);
    WriteBuffer(ATail[1],Length(ATail));
  end;
end;

function TWinWrap.SendAttachment:Boolean;
var ABuffer:INTERNET_BUFFERS;
    AStream:TMemoryStream;

 procedure PrepareUploadBuffer;
 begin
   FillChar(ABuffer,sizeof(ABuffer),0);
   FSize:=AStream.Size;
   with ABuffer do
   begin
     dwStructSize:=sizeof(ABuffer);
     lpcszHeader:=PChar(FUpReqHead);
     dwHeadersLength:=Length(FUpReqHead);
     dwHeadersTotal:=dwHeadersLength;
     dwBufferTotal:=FSize;
   end;
 end;

  function WW_ExecRequest:Boolean;
  begin
    Result:=HTTPSendRequestEx(FRequest,@ABuffer,nil,HSR_INITIATE,0);
    if not(Result) then AssignError(wwecExecRequest);
  end;

  function WriteAttachment:Boolean;
  var i,ASize,AStart:Integer;
      ASend,ASent:DWORD;
      P:Pointer absolute AStart;
  begin
    P:=AStream.Memory;
    FTotal:=0;
    if Assigned(FSized) then FSized(-FSize);
    ASize:=FSize;

    repeat
      i:=ord(ASize > BUFFER_SIZE);
      ASend:=BUFFER_SIZE*i + (1 - i)*ASize;
      if not(InternetWriteFile(FRequest,P,ASend,ASent)) then
      begin
        AssignError(wwecWriteFile);
        ASize:=-1;
        break;
      end;
      inc(AStart,ASent);
      dec(ASize,ASent);
      inc(FTotal,ASent);
      if Assigned(FSized) then FSized(FTotal);
    until (ASize = 0);
    Result:=(ASize = 0);
  end;

begin
  AStream:=BuildAttachment;
  try
    PrepareUploadBuffer;
    try
      Result:=WW_ExecRequest and WriteAttachment;
    finally Result:=HTTPEndRequest(FRequest,nil,0,0) end;
    if not(Result) then AssignError(wwecEndRequest) else
    Result:=FixContentType and ReadResponse;
  finally AStream.Free end;
end;
//******************************************************************************
function TWinWrap.PerformGet:Boolean;
begin
  with FInfo do
  Result:=HTTPSendRequest(FRequest,D_C_T,D_C_T_S,nil,0);
  if not(Result) then AssignError(wwecExecRequest) else
  case StatusGood of
    False:Result:=AssignError(wwecStatus);
     True:Result:=FixContentType and ReadResponse;
  end;
end;

function TWinWrap.PerformPost:Boolean;
begin
  with FInfo do
  Result:=HTTPSendRequest(FRequest,D_C_T,D_C_T_S,wwiParams,Length(wwiParams));
  if not(Result) then AssignError(wwecExecRequest) else
  case StatusGood of
    False:AssignError(wwecStatus);
     True:Result:=FixContentType and ReadResponse;
  end;
end;
//------------------------------------------------------------------------------
function TWinWrap.Execute(var AInfo:TWinWrapInfo):Boolean;
begin
  CleanUp;
  FInfo:=AInfo;
  FSize:=0;FTotal:=0;
  try
    Result:=OpenConnection and OpenRequest and ConfigureRequest;
    if Result then
    case AInfo.wwiVerb of
      wwvGet:Result:=PerformGet;
      wwvPost:Result:=PerformPost;
      wwvMPost:Result:=SendAttachment;
    end;
  finally
    AInfo:=FInfo;
    if Assigned(FSized) then FSized(0);
  end;
end;

procedure TWinWrap.UseThisPort(APort:Integer);
begin
  FPort:=APort;
end;

end.
