unit TCacheQueueThread;

interface
uses
  windows, classes, TDStkQue,SysUtils;

type
  TDataHandleOBJ = procedure (Sender:Pointer; pBuf:Pointer; dwLen:dword; var Rollback: BOOL) of object; Stdcall;
  TDataHandlePRO = procedure (Sender:Pointer; pBuf:Pointer; dwLen:dword; var Rollback: BOOL); Stdcall;

  PPackageRecord = ^TPackageRecord;
  TPackageRecord = packed record
    Sender:Pointer; 
    dwLen :dword;
    szBuf :array[0..0] of char;
  end;

  TCacheQueueClass = class(TThread)
  private
    KeysQueue :TtdArrayQueue;
    hEvent: THandle;
    hHeap: THandle;
    FIsRunning :boolean;
    FDataHandleProc :TDataHandleOBJ;
    FDataHandler    :TDataHandlePRO;
    ThreadLock: TRTLCriticalSection; //ÁÙ½çÇø
    LastActiveTime: DWORD;
  protected
    procedure SafeEnqueue (Item: Pointer);
    Procedure PushIn(Sender:Pointer; const Buf; dwLen :dword);
    procedure DataHandleProc(Sender: Pointer; pBuf:pointer; dwLen :dword; var Rollback: BOOL);
    procedure Execute;override;
  public
    Procedure PushQue(Sender:Pointer; const Buf; dwLen :dword);
    Procedure PushStrQue(Sender:Pointer; msg:string);
    function IsWorkingWithin (TickCount: DWORD = 5000): BOOL;
    function IsEmpty: BOOL;
    function Count: DWORD;
  public
    constructor Create();
    destructor Destroy; override;
    procedure WaitUtilTerminate;

    property OnDataHandle:TDataHandleOBJ read FDataHandleProc write FDataHandleProc;
    property OnDataHandlePro:TDataHandlePRO read FDataHandler write FDataHandler;
  end;

function  MakeCacheQueue (Handler: TDataHandlePRO): THandle; Stdcall;
Procedure PushCacheQueue (Handle: THandle; Sender, Buffer: Pointer; Size: Integer); Stdcall;
function  GetCacheCount  (Handle: THandle): DWORD; Stdcall;
function  IsQueueWorking (Handle: THandle; TickCount: DWORD = 5000): BOOL; Stdcall;
Procedure FreeCacheQueue (Handle: THandle); Stdcall;

implementation

function  MakeCacheQueue (Handler: TDataHandlePRO): THandle; Stdcall;
var
  CacheQueue: TCacheQueueClass;
begin
  CacheQueue := TCacheQueueClass.Create;
  CacheQueue.OnDataHandlePro := Handler;
  Result := THandle(CacheQueue);
end;

function  GetCacheCount  (Handle: THandle): DWORD; Stdcall;
var
  CacheQueue: TCacheQueueClass absolute Handle;
begin
  Result := CacheQueue.Count;
end;

function  IsQueueWorking (Handle: THandle; TickCount: DWORD = 5000): BOOL; Stdcall;
var
  CacheQueue: TCacheQueueClass absolute Handle;
begin
  Result := CacheQueue.IsWorkingWithin (TickCount);
end;

Procedure PushCacheQueue (Handle: THandle; Sender, Buffer: Pointer; Size: Integer); Stdcall;
var
  CacheQueue: TCacheQueueClass absolute Handle;
begin
  if Handle = 0 then Exit;
  CacheQueue.PushIn (Sender, Buffer^, Size);
end;

Procedure FreeCacheQueue (Handle: THandle); Stdcall;
var
  CacheQueue: TCacheQueueClass absolute Handle;
begin
  CacheQueue.Terminate;
  CacheQueue.WaitUtilTerminate;
  CacheQueue.Destroy;
end;


const
  HEAP_ZERO_MEMORY = $00000008;
  HEAP_GENERATE_EXCEPTIONS = $00000004;

  PACKET_RECORD_HEAD_LENGTH = SizeOf(TPackageRecord) - 1;

function TCacheQueueClass.Count:dword;
begin                            
  result:=KeysQueue.Count;
end;

function TCacheQueueClass.IsEmpty:BOOL;
begin
  result:=KeysQueue.IsEmpty;
end;

Procedure TCacheQueueClass.PushQue(Sender:Pointer; const Buf; dwLen :dword);
begin
  PushIn(Sender, Buf, dwLen);
end;

Procedure TCacheQueueClass.PushStrQue(Sender:Pointer; msg:string);
begin
  PushIn(Sender, msg[1], Length(msg));
end;

procedure TCacheQueueClass.SafeEnqueue (Item: Pointer);
begin
  EnterCriticalSection(ThreadLock);
  try        
    KeysQueue.Enqueue(Item);       
  finally
    LeaveCriticalSection(ThreadLock);
  end;

  SetEvent(hEvent);
end;

Procedure TCacheQueueClass.PushIn(Sender:Pointer; const Buf; dwLen :dword);
var
  pPackage :PPackageRecord;
begin
  if dwLen = 0 then exit;

  pPackage:= HeapAlloc(hHeap, HEAP_ZERO_MEMORY, dwLen + SizeOf(TPackageRecord));
  pPackage.Sender := Sender;
  pPackage.dwLen := dwLen;
  CopyMemory(@pPackage.szBuf[0], @Buf, dwLen);

  SafeEnqueue (pPackage);
end;

procedure TCacheQueueClass.DataHandleProc(Sender: Pointer; pBuf:pointer; dwLen :dword; var Rollback: BOOL);
begin
  if Assigned(FDataHandleProc) then
    FDataHandleProc(Sender, pBuf, dwLen, Rollback);
  if Assigned(FDataHandler) then
    FDataHandler(Sender, pBuf, dwLen, Rollback);
end;

function TCacheQueueClass.IsWorkingWithin (TickCount: DWORD = 5000): BOOL;
begin
  Result := (GetTickCount - LastActiveTime) < TickCount;
end;

procedure TCacheQueueClass.Execute;
var
  pPackage :PPackageRecord;
  IsRollBack: BOOL;
begin
  FIsRunning :=true;
  try
    Repeat
      LastActiveTime := GetTickCount;

      case WaitForSingleObject(hEvent, 1000) of
        WAIT_OBJECT_0: Begin
          while not KeysQueue.IsEmpty do
          begin
              LastActiveTime := GetTickCount;

              IsRollBack := False;
              pPackage := KeysQueue.Dequeue;

              Try
                DataHandleProc(pPackage.Sender, @pPackage.szBuf[0], pPackage.dwLen, IsRollBack);
              Except
              end;

              if IsRollBack then
              begin
                SafeEnqueue(pPackage);
                Break;
              end else
                HeapFree(hHeap, 0, pPackage);
          end;
        end;
        WAIT_TIMEOUT: Begin
        end;
        ELSE Exit;
      end;
    Until Terminated;
  finally
    FIsRunning :=False;
  end;
end;

constructor TCacheQueueClass.Create();
begin
  InitializeCriticalSection(ThreadLock);
  FDataHandleProc := nil;
  FDataHandler    := nil;      
  FIsRunning:=false;          
  LastActiveTime := GetTickCount;
  hHeap := HeapCreate(HEAP_GENERATE_EXCEPTIONS, $1000, 0);
  hEvent := CreateEvent(Nil, False, True, nil);
  KeysQueue :=TtdArrayQueue.Create(nil, 100);
  inherited Create(False);       
end;

destructor TCacheQueueClass.Destroy;
begin
  CloseHandle(hEvent);
  KeysQueue.free;  
  HeapDestroy(hHeap);
  DeleteCriticalSection(ThreadLock);
  inherited Destroy;
end;

procedure TCacheQueueClass.WaitUtilTerminate;
begin
  repeat
    sleep(88);
  until not FIsRunning;
end;


end.
