unit EchoIpcUnit;

interface
uses windows,classes;

type
  TReplyCallback = function (IpcName: PChar; Buffer: PChar; Size: Integer): Integer; Stdcall;

function MakeEchoChannel (IpcName: PChar; ChannelSize: Integer; Replier: TReplyCallback): LongBool; Stdcall;
function FreeEchoChannel (IpcName: PChar): LongBool; Stdcall;
function SendEchoMessage (IpcName: PChar; AskBuff: PChar; AskSize: Integer; ReplyBuff: Pointer; TimeOut: DWORD; HandleMessage: LongBool): Integer; Stdcall;

implementation

uses SysUtils, SyncObjs, DbgLoger;

type
  LPTHeadStru = ^THeadStru;
  THeadStru = packed record
    SenderPID: DWORD;
    SenderWaitEvent: THandle;
    BufferSize: Integer;
    Reversed: DWORD;
  end;

  LPTMemMapStruct = ^TMemMapStruct;
  TMemMapStruct = packed record
    Head: THeadStru;
    Buffer: Array[0..0] of char;
  end;

type
  LPTIpcStruct = ^TIpcStruct;
  TIpcStruct = record
    IpcName: String;
    MutexName: String;
    Mutex: TMutex;
    MemMapName: String;
    MemMapHandle: THandle;
    MemMapBuffer: LPTMemMapStruct;
    EventName: String;
    Event: TSimpleEvent;
    ChannelSize: Integer;
    RealChannelSize: Integer;
    ReplyCallback :TReplyCallback;
    HandleThreadID: DWORD;
    HandleBreak: LongBool;
  end;

var
  IpcList: TThreadList;

Procedure ReplayHandler (IpcStruct: LPTIpcStruct);
var
  hProcess: THandle;
  hEvent: THandle;
begin      
  With IpcStruct.MemMapBuffer^ do
  begin
    Head.BufferSize := IpcStruct.ReplyCallback (PChar(IpcStruct.IpcName), @Buffer[0], Head.BufferSize);

    if Head.SenderPID = GetCurrentProcessID then
    begin
      SetEvent (Head.SenderWaitEvent);
      IpcStruct.Event.WaitFor(5000);
    end else
    begin
      hProcess := OpenProcess(PROCESS_DUP_HANDLE, True, Head.SenderPID);
      if hProcess > 0 then
      if DuplicateHandle(hProcess, Head.SenderWaitEvent, GetCurrentProcess, @hEvent, EVENT_ALL_ACCESS, False, DUPLICATE_SAME_ACCESS) then
      begin
        SetEvent (hEvent);
        IpcStruct.Event.WaitFor(5000);
      end else
        DBG ( 'ReplayHandler ' +SysErrorMessage (GetLastError));
    end;
  end;
end;

function ThreadRoutine (Parma: Pointer): Integer; stdcall;
var
  IpcStruct: LPTIpcStruct absolute Parma;
  WaitResult: TWaitResult;
begin
  repeat
    WaitResult := IpcStruct.Event.WaitFor(200);
    case WaitResult of
      wrSignaled: ReplayHandler (IpcStruct);
      wrTimeout: ;
      wrAbandoned: Break;
      wrError: ;
    end;  
  until IpcStruct.HandleBreak;
  IpcStruct.HandleThreadID := 0;
  Result := 0;
end;

function MakeEchoChannel (IpcName: PChar; ChannelSize: Integer; Replier: TReplyCallback): LongBool; Stdcall;
var
  IpcStruct: LPTIpcStruct;
begin
  result := false;
  if not assigned (IpcList) then
    IpcList := TThreadList.Create;

  for IpcStruct in IpcList.LockList do
    if IpcStruct.IpcName = StrPas(IpcName) then
    begin
      IpcList.UnlockList;
      Exit;
    end;
  IpcList.UnlockList;

  IpcStruct := AllocMem (SizeOf(TIpcStruct));

  IpcStruct.IpcName := StrPas(IpcName);
  IpcStruct.MutexName  := 'Global\' + IpcName + '[MUTEX]';
  IpcStruct.MemMapName := 'Global\' + IpcName + '[MEMAP]';
  IpcStruct.EventName  := 'Global\' + IpcName + '[EVENT]';
  IpcStruct.ChannelSize := ChannelSize;
  IpcStruct.RealChannelSize := SizeOf(THeadStru) + IpcStruct.ChannelSize;
  IpcStruct.ReplyCallback := Replier;

  IpcStruct.MemMapHandle := CreateFileMapping(Dword(-1), nil, PAGE_READWRITE or SEC_COMMIT, 0, IpcStruct.RealChannelSize, PChar(IpcStruct.MemMapName));
  if IpcStruct.MemMapHandle = 0 then
    RaiseLastOSError;
  IpcStruct.MemMapBuffer := MapViewOfFile (IpcStruct.MemMapHandle, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, IpcStruct.RealChannelSize);
  if not assigned (IpcStruct.MemMapBuffer) then
  begin
    CloseHandle (IpcStruct.MemMapHandle);
    FreeMem (IpcStruct);
    Exit;
  end;
  
  IpcStruct.Mutex := TMutex.Create(nil, false, PChar(IpcStruct.MutexName), false);
  IpcStruct.Event := TSimpleEvent.Create(nil, false, false, PChar(IpcStruct.EventName));
  IpcStruct.HandleBreak := False;

  CreateThread (nil, 0, @ThreadRoutine, IpcStruct, 0, IpcStruct.HandleThreadID);

  IpcList.Add(IpcStruct);

  Result := True;
end;

function FreeEchoChannel (IpcName: PChar): LongBool; Stdcall;
var
  IpcStruct: LPTIpcStruct;
  List: TList;
begin
  result := false;

  List := IpcList.LockList;

  for IpcStruct in List do
    if IpcStruct.IpcName = StrPas(IpcName) then
    begin
      IpcStruct.HandleBreak := True;
      IpcStruct.Event.Free;
      IpcStruct.Mutex.Free;

      UnMapViewOfFile(IpcStruct.MemMapBuffer);
      CloseHandle(IpcStruct.MemMapHandle);

      List.Remove(Pointer(IpcStruct));
      while IpcStruct.HandleThreadID <> 0 do Sleep(10);
      FreeMem (IpcStruct);
      
      Result := True;
      Break;
    end;

  IpcList.UnlockList;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

type
  LPTSendStru = ^TSendStru;
  TSendStru = record
    IpcName: String;
    Mutex: TMutex;
    RecvEvent: TSimpleEvent;
    MemMapHandle: THandle;
    MemMapBuffer: LPTMemMapStruct;
    RemoteEvent: THandle;
    LastActiveTime: DWORD;
    TimeOut: DWORD;
  end;

var
  SendList: TList;
  SendLock: TCriticalSection;


function _ProcessMessage(var Msg: TMsg): Boolean;
const
  WM_QUIT = $0012;
var
  Unicode: Boolean;
  MsgExists: Boolean;
begin
  Result := False;
  if PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then
  begin
    Unicode := (Msg.hwnd <> 0) and IsWindowUnicode(Msg.hwnd);
    if Unicode then
      MsgExists := PeekMessageW(Msg, 0, 0, 0, PM_REMOVE)
    else
      MsgExists := PeekMessage(Msg, 0, 0, 0, PM_REMOVE);
    if not MsgExists then Exit;
    Result := True;
    if Msg.Message <> WM_QUIT then
    begin
        TranslateMessage(Msg);
        if Unicode then
          DispatchMessageW(Msg)
        else
          DispatchMessage(Msg);
    end;
  end;
end;

procedure ProcessMessages;
var
  msg : TMsg;
begin
  while _ProcessMessage(msg) do ;
end;

function HandleMessageWait (Event: TEvent; TimeOut: DWORD): TWaitResult;
var
  BeginTime: DWORD;
begin
  BeginTime := GetTickCount;
  repeat
    Result := Event.WaitFor (10);
    if Result <> wrTimeout then Exit;
    ProcessMessages;
  until GetTickCount - BeginTime > TimeOut;
end;

procedure ClearSendStru (SendStru: LPTSendStru);
begin
  if assigned (SendStru.Mutex) then
    SendStru.Mutex.Free;
  if assigned (SendStru.RecvEvent) then
    SendStru.RecvEvent.Free;
  if SendStru.RemoteEvent > 0 then
    CloseHandle (SendStru.RemoteEvent);
  if SendStru.MemMapHandle > 0 then
    CloseHandle (SendStru.MemMapHandle); 
end;

function SendEchoMessage (IpcName: PChar; AskBuff: PChar; AskSize: Integer; ReplyBuff: Pointer; TimeOut: DWORD; HandleMessage: LongBool): Integer; Stdcall;
var
  SendStru, IteraStru: LPTSendStru;
  ObjName: String;
  IsInitial: BOOL;
  WaitResult: TWaitResult;
begin
  result := -1;
  if not assigned (SendList) then
  begin
    SendLock := TCriticalSection.Create;
    SendList := TList.Create;
  end;

  SendLock.Enter;
  try
  
  SendStru := nil;
  for IteraStru in SendList do
    if IteraStru.IpcName = StrPas(IpcName)  then
    begin
      SendStru := IteraStru;
      Break;
    end;          

  if not assigned (SendStru) then
  begin
    SendStru := AllocMem (SizeOf(TSendStru));
    SendStru.IpcName := StrPas(IpcName);

    IsInitial := False;
    repeat
      ObjName := 'Global\' + SendStru.IpcName + '[MEMAP]';
      SendStru.MemMapHandle := OpenFileMapping (FILE_MAP_READ or FILE_MAP_WRITE, false, PChar(ObjName));
      if SendStru.MemMapHandle = INVALID_HANDLE_VALUE then
      begin
        DBG ('OpenFileMapping err1 %s', [SysErrorMessage(GetLastError)]);
        break;
      end;
      if SendStru.MemMapHandle = 0 then
      begin
        DBG ('OpenFileMapping err2 %s', [SysErrorMessage(GetLastError)]);
        break;
      end;

      SendStru.MemMapBuffer := MapViewOfFile (SendStru.MemMapHandle, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, 0);
      if not assigned (SendStru.MemMapBuffer) then
      begin
        DBG ('MapViewOfFile error %s', [SysErrorMessage(GetLastError)]);
        break;
      end;

      ObjName := 'Global\' + SendStru.IpcName + '[MUTEX]';
      SendStru.Mutex := TMutex.Create(MUTEX_ALL_ACCESS, False, ObjName, false);
      SendStru.RecvEvent := TSimpleEvent.Create(nil, False, False, '');

      ObjName := 'Global\' + SendStru.IpcName + '[EVENT]';
      SendStru.RemoteEvent := OpenEvent (EVENT_ALL_ACCESS, False, PChar(ObjName));

      SendStru.TimeOut := 1000 * 60;

      IsInitial := True;
    until true;

    if not IsInitial then
    begin
      DBG ('IsInitial error');
      ClearSendStru (SendStru);
      FreeMem (SendStru);
      Exit;
    end;

    SendList.Add(SendStru);
  end;

  SendStru.Mutex.Acquire;
  try
    With SendStru.MemMapBuffer.Head do
    begin
      SenderPID := GetCurrentProcessID;
      SenderWaitEvent := SendStru.RecvEvent.Handle;
      BufferSize := AskSize;
    end;
    CopyMemory (@SendStru.MemMapBuffer.Buffer[0], AskBuff, AskSize);
    SetEvent (SendStru.RemoteEvent);

    if HandleMessage then
      WaitResult := HandleMessageWait (SendStru.RecvEvent, TimeOut)
    else
      WaitResult := SendStru.RecvEvent.WaitFor (TimeOut);

    case WaitResult of
      wrSignaled:
      begin
        Result := SendStru.MemMapBuffer.Head.BufferSize;
        CopyMemory (ReplyBuff, @SendStru.MemMapBuffer.Buffer[0], Result);
        SetEvent (SendStru.RemoteEvent);
      end;
      wrTimeout:
      begin
        SendStru.Mutex.Release;
        ClearSendStru (SendStru);
        SendList.Remove(SendStru);
        FreeMem (SendStru);
        SendStru := nil;       
      end;
    end;
  finally
    if Assigned (SendStru) then
      SendStru.Mutex.Release;
  end;     

  finally
    SendLock.Leave;
  end;   
end;


////////////////////////////////////////////////////////////////////////
//
//
//type
//  LPTThreadParma = ^TThreadParma;
//  TThreadParma = record
//    IpcName: PChar;
//    Buffer: PChar;
//    Size: Integer;
//    Reply: Pointer;
//    TimeOut: DWORD;
//  end;
//
//function SendThread (Input: LPTThreadParma): Integer; stdcall;
//begin
//  Result := SendEchoMessage (Input.IpcName, Input.Buffer, Input.size, Input.Reply, Input.TimeOut);
//end;
//
//
//function SendEchoMessageEx (IpcName: PChar; Buffer: PChar; Size: Integer; Reply: Pointer; TimeOut: DWORD): Integer; Stdcall;
//var
//  ThreadParma: TThreadParma;
//  hThread, tid: dword;
//begin
//  Result := -1;
//  ThreadParma.IpcName := IpcName;
//  ThreadParma.Buffer := Buffer;
//  ThreadParma.Size := Size;
//  ThreadParma.Reply := Reply;
//  ThreadParma.TimeOut := TimeOut;
//  hThread := CreateThread (nil, 0, @SendThread, @ThreadParma, 0, tid);
//  While Wait_Object_0 <> WaitForSingleObject (hThread, 10) do
//    ProcessMessages;
//  GetExitCodeThread (hThread, DWORD(Result));
//end;


end.
