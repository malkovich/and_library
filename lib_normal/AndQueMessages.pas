unit AndQueMessages;

interface
uses windows, Classes;

const
  NAME_STRING_LENGTH  = 64;
  DEFAULT_BUFFER_SIZE = $1000 * 256;
  MAX_PUSH_MSG_TIME = 500;

Type
  TMsgCallBack = Procedure (Sender: Pointer; MsgBuf: Pointer; MsgSize: Integer); Stdcall;
  TMsgSLCallBack = Procedure (Sender: Pointer; MsgSL: TStringList); Stdcall;
  TMsgSTRCallBack = Procedure (Sender: Pointer; MsgStr: String); Stdcall;

function CreateMessageQueue (IpcName: String; MsgCallBack: TMsgCallBack; Sender: Pointer = NIL; BufferSize: Integer = DEFAULT_BUFFER_SIZE; MaxPushTime: DWORD = MAX_PUSH_MSG_TIME): BOOL; Stdcall; overload;
function CreateMessageQueue (IpcName: String; MsgCallBack: TMsgSTRCallBack; Sender: Pointer = NIL; BufferSize: Integer = DEFAULT_BUFFER_SIZE; MaxPushTime: DWORD = MAX_PUSH_MSG_TIME): BOOL; Stdcall; overload;
function CreateMessageQueue (IpcName: String; MsgCallBack: TMsgSLCallBack; Sender: Pointer = NIL; BufferSize: Integer = DEFAULT_BUFFER_SIZE; MaxPushTime: DWORD = MAX_PUSH_MSG_TIME): BOOL; Stdcall; overload;

function DestroyMessageQueue (IpcName: String): BOOL; Stdcall;
function GetMaxUseBufferSize (IpcName: String; var RealBuffer, UseBuffer: Integer): BOOL; Stdcall;
function SendMapMessage (IpcName: string; MsgBuf: Pointer; MsgSize: Integer): BOOL;Stdcall;

function SendQueMessage (IpcName: string; MsgBuf: Pointer; MsgSize: Integer): BOOL;Stdcall; overload;
function SendQueMessage (IpcName: string; MsgStr: String): BOOL; Stdcall; overload;
function SendQueMessage (IpcName: string; MsgSL: TStringList): BOOL; Stdcall; overload;
                   
function SendQueMessageEx (IpcName: string; MsgBuf: Pointer; MsgSize: Integer): BOOL; Stdcall; overload;
function SendQueMessageEx (IpcName: string; MsgStr: String): BOOL; Stdcall; overload;
function SendQueMessageEx (IpcName: string; MsgSL: TStringList): BOOL; Stdcall; overload;


function CreateMsgServer (IpcName: PChar; MsgCallBack, Sender: Pointer): BOOL; Stdcall;
function CreateMsgServerEx (IpcName: PChar; MsgCallBack, Sender: Pointer; MaxMsgSize, MaxPushTime: Integer): BOOL; Stdcall;
function SendMsgToServer (IpcName: PChar; MsgBuf: Pointer; MsgSize: Integer): BOOL; Stdcall;
function SendMsgToServerEx (IpcName: PChar; MsgBuf: Pointer; MsgSize: Integer): BOOL; Stdcall;
function CloseMsgServer (IpcName: PChar): BOOL; Stdcall;

implementation

uses SyncObjs, SyncQueueHandler, ComObj, SysUtils, math, DropMyRights;

function CreateMsgServer (IpcName: PChar; MsgCallBack, Sender: Pointer): BOOL; Stdcall;
begin
  Result := CreateMessageQueue (StrPas(IpcName), TMsgCallBack(MsgCallback), Sender);
end;

function CreateMsgServerEx (IpcName: PChar; MsgCallBack, Sender: Pointer; MaxMsgSize, MaxPushTime: Integer): BOOL; Stdcall;
begin
  Result := CreateMessageQueue (StrPas(IpcName), TMsgCallBack(MsgCallback), Sender, MaxMsgSize, MaxPushTime);
end;

function SendMsgToServer (IpcName: PChar; MsgBuf: Pointer; MsgSize: Integer): BOOL; Stdcall;
begin
  Result := SendQueMessage (StrPas(IpcName), MsgBuf, MsgSize);
end;

function SendMsgToServerEx (IpcName: PChar; MsgBuf: Pointer; MsgSize: Integer): BOOL; Stdcall;
begin
  Result := SendQueMessageEx (StrPas(IpcName), MsgBuf, MsgSize);
end;

function CloseMsgServer (IpcName: PChar): BOOL; Stdcall;
begin
  Result := DestroyMessageQueue (StrPas(IpcName));
end;


Procedure DebugView (Msg: String);
begin
  OutputDebugString (PChar(Msg));
end;

Type
  TCallbackType = (cbRawPoint, cbStringList, cbString);

  LPTMapHeadStruct = ^TMapHeadStruct;
  TMapHeadStruct = packed record
    //公共数据
    ShareMemName: String[NAME_STRING_LENGTH];
    RecvEventName: String[NAME_STRING_LENGTH];
    BufferSize: Integer;
    AddIndex: Integer;
    //效率指标
    MaxUseBufferSize: Integer;
    MaxPushTime: DWORD;
    TotalOverPushTimes: Integer;
    LastOverPushTimes: Integer;
    //服务器保存的变量
    MapHandle: THandle;
    MapBase: Pointer;
    RecvEvent: TEvent;
    RecvThreadTerminate: PBOOL;
    MsgQueue: THandle;
    MsgCallBack: Pointer;
    Sender: Pointer;
    CallbackType: TCallbackType;
  end;

  LPTItemPacket = ^TItemPacket;
  TItemPacket = packed record
    ItemSize: Integer;
    Item: Array[WORD] of Char;
  end;


procedure MessageCallbacker(Sender:Pointer; Buffer:Pointer; Size:Integer; var Rollback: BOOL); Stdcall;
var
  MapHead: LPTMapHeadStruct absolute Sender;
  MsgCallBack: TMsgCallBack;
begin
  @MsgCallBack := MapHead.MsgCallBack;
  MsgCallBack (MapHead.Sender, Buffer, Size);
end;

procedure MessageSLCallbacker(Sender:Pointer; Buffer:Pointer; Size:Integer; var Rollback: BOOL); Stdcall;
var
  MapHead: LPTMapHeadStruct absolute Sender;
  MsgCallBack: TMsgSLCallBack;
  MsgSL: TStringList;
begin
  @MsgCallBack := MapHead.MsgCallBack;
  MsgSL := TStringList.Create;
  MsgSL.Text := StrPas (Buffer);
  MsgCallBack (MapHead.Sender, MsgSL);
  MsgSL.Free;
end;

procedure MessageSTRCallbacker(Sender:Pointer; Buffer:Pointer; Size:Integer; var Rollback: BOOL); Stdcall;
var
  MapHead: LPTMapHeadStruct absolute Sender;
  MsgCallBack: TMsgSTRCallBack;
begin
  @MsgCallBack := MapHead.MsgCallBack;
  MsgCallBack (MapHead.Sender, StrPas(Buffer));
end;

function RecvMessageThread (Param: Pointer): Integer;
var
  MapHead: LPTMapHeadStruct absolute Param;
  MutexName: String;
  AccessMutex, RecvEvent, MsgQueue: THandle;
  RecvBuffer: PChar;
  ScanIndex: Integer;
  ScanItem: LPTItemPacket;
  RecvThreadTerminate: PBOOL;
  SleepValue: Integer;
  MaxPushTime: DWORD;
  WaitResult: DWORD;
begin
  Result := 0;
  MutexName := MapHead.ShareMemName + '_Mutex';
  RecvEvent := MapHead.RecvEvent.Handle;
  MsgQueue := MapHead.MsgQueue;
  RecvBuffer := Pointer (DWORD(MapHead) + SizeOf(TMapHeadStruct));
  RecvThreadTerminate := MapHead.RecvThreadTerminate;

  AccessMutex := OpenMutex (MUTEX_ALL_ACCESS, False, PChar(MutexName));
  if AccessMutex = 0 then Exit;

  While WAIT_OBJECT_0 = WaitForSingleObject(RecvEvent, INFINITE) do
  begin
    if RecvThreadTerminate^ then Break;
    Sleep(5);

    WaitResult := WaitForSingleObject(AccessMutex, INFINITE);
    if WaitResult = WAIT_OBJECT_0 then
    begin
        MapHead.MaxUseBufferSize := Max (MapHead.MaxUseBufferSize, MapHead.AddIndex);
        ScanIndex := 0;
        while ScanIndex < MapHead.AddIndex do
        begin
          ScanItem := @RecvBuffer[ScanIndex];
          PushSyncQueue (MsgQueue, MapHead, @ScanItem.Item[0], ScanItem.ItemSize);
          Inc (ScanIndex, ScanItem.ItemSize + SizeOf(Integer));
        end;
        MapHead.AddIndex := 0;
        MaxPushTime := MapHead.MaxPushTime;
        ResetEvent (RecvEvent);
        ReleaseMutex(AccessMutex);
          
        SleepValue := (MapHead.BufferSize - (ScanIndex + MapHead.MaxUseBufferSize) div 2) div 1024;
        SleepValue := Min (MaxPushTime, SleepValue);
        Sleep(SleepValue);
    end else
    begin
        DebugView ('AndMessages RecvMessageThread ELSE ERROR : ' + IntToStr(WaitResult));
        Break;
    end;
  end;

  ReleaseMutex(AccessMutex);
  CloseHandle (AccessMutex);
  FreeMem (RecvThreadTerminate);
  DebugView ('AndMessages RecvMessageThread EXIT');
end;

function CreateMessageQueueRaw (IpcName: String;
                              MsgCallBack: Pointer;
                              Sender: Pointer; 
                              CallbackType: TCallbackType;
                              BufferSize: Integer;
                              MaxPushTime: DWORD): BOOL; Stdcall;
var
  FMappingSize: Integer;
  MapHead: LPTMapHeadStruct;
  FMapHandle: THandle;
  EventName: String;
  TID: DWORD;
  AccessMutex: THandle;
  SA: PSecurityAttributes;
begin
  //初始结果
  Result := False;
  //边界断言
  Assert (Length(IpcName) < NAME_STRING_LENGTH, 'IpcName字符串过长');
  Assert (assigned (MsgCallBack), 'MsgCallBack = NULL');
  Assert (BufferSize >= 1024, 'BufferSize少于1024');

  SA := NewSA;
  AccessMutex := CreateMutex (SA, True, PChar(IpcName + '_Mutex'));
  DisposeSA (SA);
  if AccessMutex = 0 then
  begin
    SetLastError ($FFFF0001);
    Exit;
  end;

  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    SetLastError ($FFFF0002);
    ReleaseMutex(AccessMutex);
    CloseHandle (AccessMutex);
    Exit;
  end;

  FMappingSize := SizeOf (TMapHeadStruct) + BufferSize;
  if (FMappingSize mod $1000) > 0 then
    FMappingSize := (FMappingSize div $1000 + 1) * $1000;

  SA := NewSA;
  FMapHandle := CreateFileMapping(INVALID_HANDLE_VALUE, SA, PAGE_READWRITE, 0, FMappingSize, PChar(IpcName));
  DisposeSA (SA);
  if FMapHandle = 0 then
  begin
    SetLastError ($FFFF0003);
    ReleaseMutex(AccessMutex);
    CloseHandle (AccessMutex);
    Exit;
  end;

  MapHead := MapViewOfFile(FMapHandle, FILE_MAP_ALL_ACCESS, 0, 0, FMappingSize);
  if MapHead = nil then
  begin
    SetLastError ($FFFF0004);
    CloseHandle (FMapHandle);
    ReleaseMutex(AccessMutex);
    CloseHandle (AccessMutex);
    Exit;
  end;

  //初始化必要的结果
  ZeroMemory (MapHead, SizeOf(TMapHeadStruct));

  EventName := CreateClassID;

  //服务器变量准备
  MapHead.MsgCallBack := MsgCallBack;
  MapHead.Sender := Sender;
  MapHead.CallbackType := CallbackType;
  MapHead.MapHandle := FMapHandle;
  MapHead := Pointer(MapHead);
  SA := NewSA;
  MapHead.RecvEvent := TEvent.Create(SA, False, False, EventName);
  DisposeSA (SA);
  MapHead.RecvThreadTerminate := AllocMem (SizeOf(BOOL));
  MapHead.RecvThreadTerminate^ := False;

  case CallbackType of
    cbRawPoint:   MapHead.MsgQueue := MakeSyncQueue (MessageCallbacker);
    cbStringList: MapHead.MsgQueue := MakeSyncQueue (MessageSLCallbacker);
    cbString:     MapHead.MsgQueue := MakeSyncQueue (MessageSTRCallbacker);
    else          MapHead.MsgQueue := MakeSyncQueue (MessageCallbacker);
  end;
                    
  CloseHandle (BeginThread (NIL, 0, RecvMessageThread, MapHead, 0, TID));

  //公共数据区准备，这将正式启动服务
  MapHead.BufferSize := FMappingSize - SizeOf(TMapHeadStruct);
  MapHead.MaxPushTime := MaxPushTime;
  MapHead.MaxUseBufferSize := 0;
  MapHead.TotalOverPushTimes := 0;
  MapHead.LastOverPushTimes := 0;
  MapHead.AddIndex := 0;
  MapHead.ShareMemName := IpcName;
  MapHead.RecvEventName := EventName;
  Result := True;

  ReleaseMutex(AccessMutex);    
end;


function CreateMessageQueue (IpcName: String; MsgCallBack: TMsgCallBack;
                             Sender: Pointer = NIL; 
                             BufferSize: Integer = DEFAULT_BUFFER_SIZE;
                             MaxPushTime: DWORD = MAX_PUSH_MSG_TIME): BOOL; Stdcall;
begin
  Result := CreateMessageQueueRaw (IpcName, @MsgCallBack, Sender, cbRawPoint, BufferSize, MaxPushTime);
end;

function CreateMessageQueue (IpcName: String; MsgCallBack: TMsgSLCallBack;
                             Sender: Pointer = NIL; 
                             BufferSize: Integer = DEFAULT_BUFFER_SIZE;
                             MaxPushTime: DWORD = MAX_PUSH_MSG_TIME): BOOL; Stdcall;
begin
  Result := CreateMessageQueueRaw (IpcName, @MsgCallBack, Sender, cbStringList, BufferSize, MaxPushTime);
end;

function CreateMessageQueue (IpcName: String; MsgCallBack: TMsgSTRCallBack;
                             Sender: Pointer = NIL; 
                             BufferSize: Integer = DEFAULT_BUFFER_SIZE;
                             MaxPushTime: DWORD = MAX_PUSH_MSG_TIME): BOOL; Stdcall;
begin
  Result := CreateMessageQueueRaw (IpcName, @MsgCallBack, Sender, cbString, BufferSize, MaxPushTime);
end;

function DestroyMessageQueue (IpcName: String): BOOL; Stdcall;
var
  FMapHandle, AccessMutex: THandle;
  MapHead: LPTMapHeadStruct;
  MapHeadBk: TMapHeadStruct;
begin
  Result := False;

  AccessMutex := OpenMutex (MUTEX_ALL_ACCESS, False, PChar(IpcName + '_Mutex'));
  if AccessMutex = 0 then
  begin
    SetLastError ($FFFF0001);
    Exit;
  end;

  if WaitForSingleObject(AccessMutex, INFINITE) <> WAIT_OBJECT_0 then
  begin
    SetLastError ($FFFF0002);
    ReleaseMutex (AccessMutex);
    CloseHandle (AccessMutex);
    Exit;
  end;

  FMapHandle := OpenFileMapping (FILE_MAP_READ, False, PChar(IpcName));
  if FMapHandle = 0 then
  begin
    SetLastError ($FFFF0003);
    ReleaseMutex (AccessMutex);
    CloseHandle (AccessMutex);
    Exit;
  end;

  MapHead := MapViewOfFile (FMapHandle, FILE_MAP_READ, 0, 0, SizeOf(TMapHeadStruct));
  if MapHead = nil then
  begin
    SetLastError ($FFFF0004);
    CloseHandle (FMapHandle);
    ReleaseMutex (AccessMutex);
    CloseHandle (AccessMutex);
    Exit;
  end;

  MapHeadBk := MapHead^;
  MapHeadBk.RecvThreadTerminate^ := True;
  MapHeadBk.RecvEvent.Free;
  FreeSyncQueue (MapHeadBk.MsgQueue);

  //释放本次访问的内存映射文件
  UnMapViewOfFile(MapHead);
  CloseHandle (FMapHandle);

  //释放原始内存映射文件
  UnMapViewOfFile (MapHeadBk.MapBase);
  CloseHandle (MapHeadBk.MapHandle);
  ReleaseMutex (AccessMutex);
  CloseHandle (AccessMutex);
  Result := True;
end;

function GetMaxUseBufferSize (IpcName: String; var RealBuffer, UseBuffer: Integer): BOOL; Stdcall;
var
  FMapHandle, AccessMutex: THandle;
  MapHead: LPTMapHeadStruct;
begin
  Result := False;

  AccessMutex := OpenMutex (MUTEX_ALL_ACCESS, False, PChar(IpcName + '_Mutex'));
  if AccessMutex = 0 then
  begin
    SetLastError ($FFFF0001);
    Exit;
  end;

  if WaitForSingleObject(AccessMutex, INFINITE) <> WAIT_OBJECT_0 then
  begin
    SetLastError ($FFFF0002);
    ReleaseMutex (AccessMutex);
    CloseHandle (AccessMutex);
    Exit;
  end;

  FMapHandle := OpenFileMapping (FILE_MAP_READ, False, PChar(IpcName));
  if FMapHandle = 0 then
  begin
    SetLastError ($FFFF0003);
    ReleaseMutex (AccessMutex);
    CloseHandle (AccessMutex);
    Exit;
  end;

  MapHead := MapViewOfFile (FMapHandle, FILE_MAP_READ, 0, 0, SizeOf(TMapHeadStruct));
  if MapHead = nil then
  begin
    SetLastError ($FFFF0004);
    CloseHandle (FMapHandle);
    ReleaseMutex (AccessMutex);
    CloseHandle (AccessMutex);
    Exit;
  end;

  RealBuffer := MapHead.BufferSize;
  UseBuffer := MapHead.MaxUseBufferSize;

  UnMapViewOfFile(MapHead);
  CloseHandle (FMapHandle);
  ReleaseMutex (AccessMutex);
  CloseHandle (AccessMutex);

  Result := True;          
end;


function SendMapMessage (IpcName: string; MsgBuf: Pointer; MsgSize: Integer): BOOL; Stdcall;
var
  FMapHandle, AccessMutex, NotifyEvent: THandle;
  MapHead: LPTMapHeadStruct;
  RecvBuffer: PChar;
  Item: LPTItemPacket;
  AddSize: Integer;
begin
  Result := False;
  AccessMutex := OpenMutex (MUTEX_ALL_ACCESS, False, PChar(IpcName + '_Mutex'));
  if AccessMutex = 0 then
  begin
    SetLastError ($FFFF0001);
    Exit;
  end;

  if WaitForSingleObject(AccessMutex, INFINITE) <> WAIT_OBJECT_0 then
  begin
    CloseHandle (AccessMutex);
    SetLastError ($FFFF0002);       
    Exit;
  end;


  FMapHandle := OpenFileMapping (FILE_MAP_ALL_ACCESS, False, PChar(IpcName));
  if FMapHandle = 0 then
  begin
    SetLastError ($FFFF0003);
    ReleaseMutex (AccessMutex);
    CloseHandle (AccessMutex);
    Exit;
  end;

  MapHead := MapViewOfFile (FMapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  if MapHead = nil then
  begin
    SetLastError ($FFFF0004);
    CloseHandle (FMapHandle);
    ReleaseMutex (AccessMutex);
    CloseHandle (AccessMutex);
    Exit;
  end;

  AddSize := MsgSize + SizeOf(Integer);
  if AddSize > (MapHead.BufferSize - MapHead.AddIndex) then
  begin
    Inc (MapHead.TotalOverPushTimes);
    Inc (MapHead.LastOverPushTimes);
    SetLastError ($FFFF0005);
    UnMapViewOfFile(MapHead);
    CloseHandle (FMapHandle);
    ReleaseMutex (AccessMutex);
    CloseHandle (AccessMutex);
    Exit;
  end;

  RecvBuffer := Pointer (DWORD(MapHead) + SizeOf(TMapHeadStruct));
  Item := @RecvBuffer [MapHead.AddIndex];
  Item.ItemSize := MsgSize;
  CopyMemory (@Item.Item[0], MsgBuf, MsgSize);
  Inc (MapHead.AddIndex, AddSize);

  NotifyEvent := OpenEvent (EVENT_MODIFY_STATE, False, @MapHead.RecvEventName[1]);
  if NotifyEvent = 0 then
  begin
    SetLastError ($FFFF0006);
    UnMapViewOfFile(MapHead);
    CloseHandle (FMapHandle);
    ReleaseMutex (AccessMutex);
    CloseHandle (AccessMutex);
    Exit;
  end;

  SetEvent (NotifyEvent);
  CloseHandle (NotifyEvent);
  
  UnMapViewOfFile(MapHead);
  CloseHandle (FMapHandle);
  ReleaseMutex (AccessMutex);
  CloseHandle (AccessMutex);
  Result := True;
end;

function SendQueMessage (IpcName: string; MsgBuf: Pointer; MsgSize: Integer): BOOL; Stdcall;
var
  TryCount, SleepValue: Integer;
begin
  TryCount := 0;
  Repeat
    Result := SendMapMessage (IpcName, MsgBuf, MsgSize);
    if Result then Break;
    if GetLastError <> $FFFF0005 then Break;
    
    Inc (TryCount);
    SleepValue := Min (500, TryCount * TryCount * 5);
    Sleep(SleepValue); //5  20  45  80  125  180  245 320 405 500  605
  until TryCount > 11;
end;

function SendQueMessage (IpcName: string; MsgStr: String): BOOL;Stdcall;
begin
  MsgStr := MsgStr + #0;
  Result := SendQueMessage (IpcName, PChar(MsgStr), Length(MsgStr));
end;

function SendQueMessage (IpcName: string; MsgSL: TStringList): BOOL; Stdcall;
begin
  Result := SendQueMessage (IpcName, MsgSL.Text);
end;

var
  SyncQueue: THandle;
  
procedure SyncQueMessage (Sender:Pointer; Buffer:Pointer; Size:Integer; var Rollback: BOOL); Stdcall;
var
  IpcName: PChar absolute Sender;
begin
  SendQueMessage (StrPas(IpcName), Buffer, Size);
  FreeMem (IpcName);
end;

function SendQueMessageEx (IpcName: string; MsgBuf: Pointer; MsgSize: Integer): BOOL; Stdcall;
var
  Sender: Pointer;
begin
  if SyncQueue = 0 then
    SyncQueue := MakeSyncQueue (SyncQueMessage);

  Try
    Sender := AllocMem (Length(IpcName) + 1);
    CopyMemory (Sender, PChar(IpcName), Length(IpcName));
    PushSyncQueue (SyncQueue, Sender, MsgBuf, MsgSize);
  Except
    Result := False;
    Exit;
  end;

  Result := True;
end;

function SendQueMessageEx (IpcName: string; MsgStr: String): BOOL; Stdcall;
begin
  MsgStr := MsgStr + #0;
  Result := SendQueMessageEx (IpcName, PChar(MsgStr), Length(MsgStr));
end;

function SendQueMessageEx (IpcName: string; MsgSL: TStringList): BOOL; Stdcall;
begin
  Result := SendQueMessageEx (IpcName, MsgSL.Text);
end;

end.
