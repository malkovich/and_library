unit AndIpcProcedure;

interface
uses windows, classes;

const
  DEFAULT_TRANS_SIZE = $1000;
  DEFAULT_CHANNEL_COUNT = 256;
  DEFAULT_TASK_THREAD_COUNT = 1;

const
  Guid_Name_Length = Length ('{C863B9F5-F82A-4432-955A-4E80457F2183}');

Type
  TIpcCallBack = function (TranSL: TStringList): Integer; Stdcall;

function CreateIpcProcedure (IpcName: String; IpcCallBack: TIpcCallBack;
      ChannelCount: Integer = DEFAULT_CHANNEL_COUNT;
      TransSize: Integer = DEFAULT_TRANS_SIZE;
      TaskThreadCount: Integer = DEFAULT_TASK_THREAD_COUNT): BOOL; Stdcall;
function DestroyIpcProcedure (IpcName: String): BOOL; Stdcall;

function IpcProcedure (IpcName: String; TranSL: TStringList): BOOL; Stdcall;

implementation

uses SyncObjs, SyncQueueHandler, AndQueMessages, ComObj, SysUtils, math, DropMyRights;


Type
  LPTMapHeadStruct = ^TMapHeadStruct;
  TMapHeadStruct = packed record
    //公共数据
    ShareMemName: String[NAME_STRING_LENGTH];
    RecvEventName: String[NAME_STRING_LENGTH];
    ChannelCount: Integer;
    TransSize: Integer;
    //空闲Channel栈空间管理
    StackSize: Integer;
    StackCount: Integer;
    NotEnoughChannel: Integer;
    //任务列表空间管理
    TaskAreaSize: Integer;
    AddIndex: Integer;
    TaskThreadCount: Integer;
    TotalOverPushTimes: Integer;
    LastOverPushTimes: Integer;    
    //服务器保存的变量
    MapHandle: THandle;
    MapBase: Pointer;
    RecvEvent: TEvent;
    RecvThreadTerminate: PBOOL;
    TaskQueues: TList;
    IpcCallBack: Pointer;
  end;

  LPTItemPacket = ^TItemPacket;
  TItemPacket = packed record
    ItemSize: Integer;
    Item: Array[WORD] of Char;
  end;

  LPTTaskItem = ^TTaskItem;
  TTaskItem = packed record
    ChannelIndex: Integer;
    ChannelBase: PChar;
    ChannelSize: Integer;
    FeedBackEventName: Array[0..Guid_Name_Length] of Char;
  end;

  LPTIntArray = ^TIntArray;
  TIntArray = Array[1..High(WORD)] of Integer;

Procedure DebugView (Msg: String);
begin
  OutputDebugString (PChar(Msg));
end;  

procedure IpcQueueHandler(Sender:Pointer; Buffer:Pointer; Size:Integer; var Rollback: BOOL); Stdcall;
var
  IpcCallBack: TIpcCallBack absolute Sender;
  TaskItem: LPTTaskItem absolute Buffer;
  RealSize, ResultSize: Integer;
  TranSL: TStringList;
  ResultStr: String;
  NotifyEvent: THandle;
begin
  RealSize := StrLen (TaskItem.ChannelBase);

  ResultSize := 0;
  if RealSize < TaskItem.ChannelSize then
  begin
    TranSL := TStringList.Create;
    TranSL.Text := StrPas (TaskItem.ChannelBase);
    IpcCallBack (TranSL);     
    if TranSL.Count > 0 then
    begin
      ResultStr := TranSL.Text;
      ResultSize := Length(ResultStr);
      if ResultSize >= TaskItem.ChannelSize then
      begin
        //返回值超出大小
        ResultSize := 0;
      end;
    end;
    TranSL.Free;
  end;

  if ResultSize > 0 then
    CopyMemory (TaskItem.ChannelBase, @ResultStr[1], ResultSize);
  TaskItem.ChannelBase[ResultSize] := #0;

  NotifyEvent := OpenEvent (EVENT_MODIFY_STATE, False, @TaskItem.FeedBackEventName[0]);
  if NotifyEvent > 0 then
  begin
    SetEvent (NotifyEvent);
    CloseHandle (NotifyEvent);
  end;
end;

function RecvMessageThread (Param: Pointer): Integer;
var
  MapHead: LPTMapHeadStruct absolute Param;
  MutexName: String;
  AccessMutex, RecvEvent, RunTaskQueue: THandle;
  TaskQueList: TList;
  RecvBuffer, ChannelBase: PChar;
  ScanIndex, QueIndex: Integer;
  ScanItem: LPTItemPacket;
  TaskItem: LPTTaskItem;
  IpcCallBack: Pointer;
  RecvThreadTerminate: PBOOL;
  WaitResult: DWORD;
begin
  Result := 0;
  MutexName := MapHead.ShareMemName + '_Mutex';
  RecvEvent := MapHead.RecvEvent.Handle;
  IpcCallBack := MapHead.IpcCallBack;
  TaskQueList := MapHead.TaskQueues;
  RecvBuffer := Pointer (Integer(MapHead) + SizeOf(TMapHeadStruct) + MapHead.StackSize);
  RecvThreadTerminate := MapHead.RecvThreadTerminate;

  AccessMutex := OpenMutex (MUTEX_ALL_ACCESS, False, PChar(MutexName));
  if AccessMutex = 0 then Exit;
  QueIndex := 0;

  Try
    While WAIT_OBJECT_0 = WaitForSingleObject(RecvEvent, INFINITE) do
    begin
      if RecvThreadTerminate^ then
      begin
        FreeMem (RecvThreadTerminate);
        Exit;
      end;

      WaitResult := WaitForSingleObject(AccessMutex, INFINITE);
      if WAIT_OBJECT_0 = WaitResult then
      begin
          ScanIndex := 0;
          while ScanIndex < MapHead.AddIndex do
          begin
            ScanItem := @RecvBuffer[ScanIndex];
            TaskItem := @ScanItem.Item[0];

            ChannelBase := Pointer (Integer(MapHead) + SizeOf(TMapHeadStruct) + MapHead.StackSize + MapHead.TaskAreaSize);
            ChannelBase := @ChannelBase[TaskItem.ChannelIndex * MapHead.TransSize];
            TaskItem.ChannelBase := ChannelBase;
            TaskItem.ChannelSize := MapHead.TransSize;
                         
            RunTaskQueue := THandle(TaskQueList[QueIndex]);
            Inc (QueIndex);
            if QueIndex >= TaskQueList.Count then
              QueIndex := 0;

            PushSyncQueue (RunTaskQueue, IpcCallBack, TaskItem, SizeOf(TTaskItem));
            Inc (ScanIndex, ScanItem.ItemSize + SizeOf(Integer));
          end;
          MapHead.AddIndex := 0;
          ResetEvent (RecvEvent);
          ReleaseMutex(AccessMutex);     
      end ELSE
      begin
          DebugView ('RecvMessageThread WaitForSingleObject AccessMutex ERROR ' + IntToStr(WaitResult));
          Exit;
      end;
      Sleep(5);
    end;
  finally
    CloseHandle (AccessMutex);
    DebugView ('AndMessages RecvMessageThread EXIT');
  end;
end;

  
function CreateIpcProcedure (IpcName: String; IpcCallBack: TIpcCallBack;
      ChannelCount: Integer = DEFAULT_CHANNEL_COUNT;
      TransSize: Integer = DEFAULT_TRANS_SIZE;
      TaskThreadCount: Integer = DEFAULT_TASK_THREAD_COUNT): BOOL; Stdcall;
var
  FMappingSize, StackSize, TaskAreaSize, Index: Integer;
  MapHead: LPTMapHeadStruct;
  FMapHandle: THandle;
  EventName: String;
  TID: DWORD;
  AccessMutex: THandle;
  IntArray: LPTIntArray;
  SA: PSecurityAttributes;
begin
  //初始结果
  Result := False;
  //边界断言
  Assert (Length(IpcName) < NAME_STRING_LENGTH, 'IpcName字符串过长');
  Assert (assigned (IpcCallBack), 'IpcCallBack = NULL');
  Assert (TransSize > 0, 'TransSize');
  Assert (TaskThreadCount > 0, 'TaskThreadCount=0');

  SA := NewSA;
  AccessMutex := CreateMutex (SA, True, PChar(IpcName + '_Mutex'));
  DisposeSA (SA);
  if AccessMutex = 0 then
  begin
    SetLastError ($FFFF0001);
    Exit;
  end;

  Try
    if GetLastError = ERROR_ALREADY_EXISTS then
    begin
      SetLastError ($FFFF0002);
      Exit;
    end;

    StackSize := ChannelCount * SizeOf (Integer);
    TaskAreaSize := Max (SizeOf(TTaskItem) * ChannelCount, $1000);
    FMappingSize := SizeOf (TMapHeadStruct) + StackSize + TaskAreaSize + TransSize * ChannelCount;
    if (FMappingSize mod $1000) > 0 then
    begin
      TaskAreaSize := TaskAreaSize + ($1000 - FMappingSize mod $1000);
      FMappingSize := SizeOf (TMapHeadStruct) + StackSize + TaskAreaSize + TransSize * ChannelCount;
    end;

    SA := NewSA;
    FMapHandle := CreateFileMapping(INVALID_HANDLE_VALUE, SA, PAGE_READWRITE, 0, FMappingSize, PChar(IpcName));
    DisposeSA (SA);
    if FMapHandle = 0 then
    begin
      SetLastError ($FFFF0003);
      Exit;
    end;          

    MapHead := MapViewOfFile(FMapHandle, FILE_MAP_ALL_ACCESS, 0, 0, FMappingSize);
    if MapHead = nil then
    begin
      SetLastError ($FFFF0004);
      CloseHandle (FMapHandle);
      Exit;
    end;

    //初始化必要的结果
    ZeroMemory (MapHead, SizeOf(TMapHeadStruct));

    EventName := CreateClassID;

    //服务器变量准备
    MapHead.ShareMemName := IpcName;
    MapHead.RecvEventName := EventName;
    MapHead.ChannelCount := ChannelCount;
    MapHead.TransSize := TransSize;

    MapHead.StackSize := StackSize;
    MapHead.StackCount := ChannelCount;
    MapHead.NotEnoughChannel := 0;
    IntArray := Pointer (DWORD(MapHead) + SizeOf(TMapHeadStruct));
    for Index := 1 to MapHead.StackCount do
      IntArray [Index] := Index - 1;

    MapHead.AddIndex := 0;
    MapHead.TaskAreaSize := TaskAreaSize;
    MapHead.TaskThreadCount := TaskThreadCount;     
    MapHead.TaskQueues := TList.Create;
    for Index := 1 to TaskThreadCount do
      MapHead.TaskQueues.Add (Pointer(MakeSyncQueue (IpcQueueHandler)));

    MapHead.TotalOverPushTimes := 0;
    MapHead.LastOverPushTimes := 0;
                                                
    MapHead.MapHandle := FMapHandle;
    MapHead.MapBase := Pointer(MapHead);
    SA := NewSA;
    MapHead.RecvEvent := TEvent.Create(SA, False, False, EventName);
    DisposeSA (SA);
    MapHead.RecvThreadTerminate := AllocMem (SizeOf(BOOL));
    MapHead.RecvThreadTerminate^ := False;

    MapHead.IpcCallBack := @IpcCallBack;

    CloseHandle (BeginThread (NIL, 0, RecvMessageThread, MapHead, 0, TID));

    Result := True;
  finally
    ReleaseMutex(AccessMutex);
    if Not Result then
      CloseHandle (AccessMutex);
  end;
end;


function DestroyIpcProcedure (IpcName: String): BOOL; Stdcall;
var
  FMapHandle, AccessMutex: THandle;
  MapHead: LPTMapHeadStruct;
  MapHeadBk: TMapHeadStruct;
  Index: Integer;
begin
  Result := False;

  AccessMutex := OpenMutex (MUTEX_ALL_ACCESS, False, PChar(IpcName + '_Mutex'));
  if AccessMutex = 0 then
  begin
    SetLastError ($FFFF0001);
    Exit;
  end;

  Try
      if WaitForSingleObject(AccessMutex, INFINITE) <> WAIT_OBJECT_0 then
      begin
        SetLastError ($FFFF0002);
        Exit;
      end;

      FMapHandle := OpenFileMapping (FILE_MAP_READ, False, PChar(IpcName));
      if FMapHandle = 0 then
      begin
        SetLastError ($FFFF0003);
        Exit;
      end;

      MapHead := MapViewOfFile (FMapHandle, FILE_MAP_READ, 0, 0, SizeOf(TMapHeadStruct));
      if MapHead = nil then
      begin
        SetLastError ($FFFF0004);
        CloseHandle (FMapHandle);
        Exit;
      end;

      MapHeadBk := MapHead^;
      MapHeadBk.RecvThreadTerminate^ := True;
      MapHeadBk.RecvEvent.Free;

      for Index := 0 to MapHeadBk.TaskQueues.Count - 1 do
        FreeSyncQueue (THandle(MapHeadBk.TaskQueues[Index]));   
      MapHead.TaskQueues.Free;

      //释放本次访问的内存映射文件
      UnMapViewOfFile(MapHead);
      CloseHandle (FMapHandle);

      //释放原始内存映射文件
      UnMapViewOfFile (MapHeadBk.MapBase);
      CloseHandle (MapHeadBk.MapHandle);

      Result := True; 
  finally
      ReleaseMutex (AccessMutex);
      CloseHandle (AccessMutex);
  end;
end;

function PostATask (IpcName: String; const ToPost: String;
                    var TransSize: Integer;
                    var ChannelIndex: Integer;
                    var ResultEvent: TEvent): BOOL; Stdcall;
var
  FMapHandle, AccessMutex, NotifyEvent: THandle;
  MapHead: LPTMapHeadStruct;
  RecvBuffer: PChar;
  Item: LPTItemPacket;
  TaskItem: TTaskItem;
  pTaskItem: LPTTaskItem;
  AddSize, PostSize: Integer;
  ResultEventName: String;
  StackArray: LPTIntArray;
  ChannelBase: PChar;
  SA: PSecurityAttributes;
begin
  Result := False;
  TransSize := 0;
  ChannelIndex := -1;

  AccessMutex := OpenMutex (MUTEX_ALL_ACCESS, False, PChar(IpcName + '_Mutex'));
  if AccessMutex = 0 then
  begin
    SetLastError ($FFFF0001);
    Exit;
  end;

  Try
      if WaitForSingleObject(AccessMutex, INFINITE) <> WAIT_OBJECT_0 then
      begin
        SetLastError ($FFFF0002);
        Exit;
      end;

      FMapHandle := OpenFileMapping (FILE_MAP_ALL_ACCESS, False, PChar(IpcName));
      if FMapHandle = 0 then
      begin
        SetLastError ($FFFF0003);
        Exit;
      end;
                      
      MapHead := MapViewOfFile (FMapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);
      if MapHead = nil then
      begin
        SetLastError ($FFFF0004);
        CloseHandle (FMapHandle);
        Exit;
      end;

      //看看要post内容是否太大
      PostSize := Length (ToPost);
      if PostSize >= MapHead.TransSize then
      begin
        SetLastError ($FFFF0005);
        UnMapViewOfFile(MapHead);
        CloseHandle (FMapHandle);
        Exit;
      end;

      //评估，看是否有空间添加
      AddSize := SizeOf(TTaskItem) + SizeOf(Integer);
      if AddSize > (MapHead.TaskAreaSize - MapHead.AddIndex) then
      begin
        Inc (MapHead.TotalOverPushTimes);
        Inc (MapHead.LastOverPushTimes);
        SetLastError ($FFFF0006);
        UnMapViewOfFile(MapHead);
        CloseHandle (FMapHandle);
        Exit;
      end;

      //取出可使用的通道
      if MapHead.StackCount = 0 then
      begin
        Inc (MapHead.NotEnoughChannel);
        SetLastError ($FFFF0007);
        UnMapViewOfFile(MapHead);
        CloseHandle (FMapHandle);
        Exit;
      end;
      StackArray := Pointer (DWORD(MapHead) + SizeOf(TMapHeadStruct));
      ChannelIndex := StackArray[MapHead.StackCount];
      Dec (MapHead.StackCount);

      //填入post的内容
      TransSize := MapHead.TransSize;
      ChannelBase := Pointer (Integer(MapHead) + SizeOf(TMapHeadStruct) + MapHead.StackSize + MapHead.TaskAreaSize);
      ChannelBase := @ChannelBase[ChannelIndex * TransSize];
      CopyMemory (ChannelBase, @ToPost[1], PostSize);
      ChannelBase [PostSize] := #0;

      //创建事件
      ResultEvent := nil;
      Repeat
        ResultEventName := CreateClassID;
        if Assigned (ResultEvent) then
          ResultEvent.Free;
        SA := NewSA;
        ResultEvent:= TEvent.Create(SA, False, False, ResultEventName);
        DisposeSA (SA);
      until ResultEvent.Handle > 0;
      
      ZeroMemory (@TaskItem, SizeOf(TTaskItem));
      TaskItem.ChannelIndex := ChannelIndex;
      CopyMemory (@TaskItem.FeedBackEventName[0], @ResultEventName[1], Length(ResultEventName));

      //填入任务
      RecvBuffer := Pointer (Integer(MapHead) + SizeOf(TMapHeadStruct) + MapHead.StackSize);
      Item := @RecvBuffer [MapHead.AddIndex];
      Item.ItemSize := SizeOf(TTaskItem);
      pTaskItem := @Item.Item[0];
      pTaskItem^ := TaskItem;
      Inc (MapHead.AddIndex, AddSize);
                                        
      //通知对方收任务
      NotifyEvent := OpenEvent (EVENT_MODIFY_STATE, False, @MapHead.RecvEventName[1]);
      if NotifyEvent = 0 then
      begin
        SetLastError ($FFFF0008);
      end else
      begin
        SetEvent (NotifyEvent);
        CloseHandle (NotifyEvent);
        Result := True;
      end;

      UnMapViewOfFile(MapHead);
      CloseHandle (FMapHandle);
  finally
      ReleaseMutex (AccessMutex);
      CloseHandle (AccessMutex);
  end;
end;

Function GetTaskResult (IpcName: String; ChannelIndex: Integer; var ChannelBase: PChar): Integer;
var
  FMapHandle, AccessMutex: THandle;
  MapHead: LPTMapHeadStruct;
  StackArray: LPTIntArray;
  AimChannelBase: PChar;
begin
  Result := -1;
  AccessMutex := OpenMutex (MUTEX_ALL_ACCESS, False, PChar(IpcName + '_Mutex'));
  if AccessMutex = 0 then
  begin
    SetLastError ($FFFF0001);
    Exit;
  end;

  Try
      if WaitForSingleObject(AccessMutex, INFINITE) <> WAIT_OBJECT_0 then
      begin
        SetLastError ($FFFF0002);
        Exit;
      end;

      FMapHandle := OpenFileMapping (FILE_MAP_ALL_ACCESS, False, PChar(IpcName));
      if FMapHandle = 0 then
      begin
        SetLastError ($FFFF0003);
        Exit;
      end;
                      
      MapHead := MapViewOfFile (FMapHandle, FILE_MAP_ALL_ACCESS, 0, 0, 0);
      if MapHead = nil then
      begin
        SetLastError ($FFFF0004);
        CloseHandle (FMapHandle);
        Exit;
      end;                  

      StackArray := Pointer (DWORD(MapHead) + SizeOf(TMapHeadStruct));
      Inc (MapHead.StackCount);
      StackArray[MapHead.StackCount] := ChannelIndex;

      AimChannelBase := Pointer (Integer(MapHead) + SizeOf(TMapHeadStruct) + MapHead.StackSize + MapHead.TaskAreaSize);
      AimChannelBase := @AimChannelBase[ChannelIndex * MapHead.TransSize];

      Result := StrLen (AimChannelBase);
      Result := Min (MapHead.TransSize, Result);
      ChannelBase := AllocMem (Result + 1);
      CopyMemory (ChannelBase, AimChannelBase, Result);

      UnMapViewOfFile(MapHead);
      CloseHandle (FMapHandle);
  finally
      ReleaseMutex (AccessMutex);
      CloseHandle (AccessMutex);
  end;
end;

function IpcProcedureRaw (IpcName: String; TranSL: TStringList): BOOL; 
var
  TransSize, ChannelIndex, ResultSize: Integer;
  ChannelBase: PChar;
  ResultEvent: TEvent;
  HasResult: BOOL;
begin
  Result := False;

  //发送ipc请求
  if not PostATask (IpcName, TranSL.Text, TransSize, ChannelIndex, ResultEvent) then exit;

  //等待对方处理完毕
  HasResult := False;
  case ResultEvent.WaitFor(INFINITE) of
    wrTimeout:   SetLastError ($FFFF0011);
    wrAbandoned: SetLastError ($FFFF0012);
    wrError:     SetLastError ($FFFF0013);
    wrSignaled:  HasResult := True;
  end;       
  ResultEvent.Free;

  //取结果,归还ChannelIndex
  if HasResult then
  begin
    TranSL.Clear;
    ResultSize := GetTaskResult (IpcName, ChannelIndex, ChannelBase);
    if ResultSize > 0 then
    begin
      TranSL.Text := StrPas(ChannelBase);
      FreeMem (ChannelBase);
      Result := True;
    end;     
  end;
end;

function IpcProcedure (IpcName: String; TranSL: TStringList): BOOL; Stdcall;
var
  TryCount, SleepValue: Integer;
  LastErr: DWORD;
begin
  TryCount := 0;
  Repeat
    Result := IpcProcedureRaw (IpcName, TranSL);

    Repeat
      if Result then Exit;
      LastErr := GetLastError;
      if LastErr = $FFFF0006 then Break;
      if LastErr = $FFFF0007 then Break;
      Exit;
    until True;

    Inc (TryCount);
    SleepValue := Min (1000, TryCount * TryCount * 10);
    Sleep(SleepValue); //10  40  90  160  250  360  490 640 810 1000  1210
  until TryCount > 16;
end;



end.
