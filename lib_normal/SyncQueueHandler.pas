unit SyncQueueHandler;

interface   

uses
  windows;

type
  TSyncQueueProcedure = procedure (Sender:Pointer; Buffer:Pointer; Size:Integer; var Rollback: BOOL); Stdcall;

function  MakeSyncQueue (Handler: TSyncQueueProcedure; DurationIdleTime: DWORD = 10000): THandle; Stdcall;
Procedure PushSyncQueue (Handle: THandle; Sender, Buffer: Pointer; Size: Integer); Stdcall;
Procedure PushSyncQueueSTR (Handle: THandle; Sender: Pointer; Msg: String); Stdcall;
function  GetSyncQueueCount (Handle: THandle): Integer; Stdcall;
Procedure FreeSyncQueue (Handle: THandle); Stdcall;

var
  AppTerminated: BOOL = False;

implementation

uses TDStkQue, SyncObjs, SysUtils, classes, math;

Type
  LPTPackageRecord = ^TPackageRecord;
  TPackageRecord = packed record
    Sender:Pointer; 
    dwLen :dword;
    szBuf :array[0..0] of char;
  end;

  LPTSyncQueueStruct = ^TSyncQueueStruct;
  TSyncQueueStruct = record
    KeysQueue :TtdArrayQueue;
    DurationIdleTime: DWORD;
    hHeap: THandle;
    FDataHandler :TSyncQueueProcedure;
    AccessLock: TCriticalSection; //临界区
    hExecThread: THandle;
    ExecThreadID: DWORD;
    Terminate: BOOL;
  end;

function  MakeSyncQueue (Handler: TSyncQueueProcedure; DurationIdleTime: DWORD = 10000): THandle; Stdcall;
var
  SyncQueue: LPTSyncQueueStruct;
begin
  SyncQueue := AllocMem (SizeOf(TSyncQueueStruct));
  SyncQueue.KeysQueue := TtdArrayQueue.Create(nil, 100);
  SyncQueue.hHeap := HeapCreate(HEAP_GENERATE_EXCEPTIONS, $1000, 0);
  SyncQueue.AccessLock := TCriticalSection.Create;
  SyncQueue.FDataHandler := Handler;
  SyncQueue.hExecThread := 0;
  SyncQueue.ExecThreadID := 0;
  SyncQueue.DurationIdleTime := DurationIdleTime;
  SyncQueue.Terminate := False;
  Result := THandle(SyncQueue);
end;


procedure SafeEnqueue (SyncQueue: LPTSyncQueueStruct; Item: Pointer);
begin
  With SyncQueue^ do
  begin
    AccessLock.Enter;
    Try
      if Terminate then Exit;
      KeysQueue.Enqueue(Item);
    finally
      AccessLock.Leave;
    end;
  end;
end;

function SafeDequeue (SyncQueue: LPTSyncQueueStruct): Pointer;
begin
  Result := NIL;
  With SyncQueue^ do
  begin
    AccessLock.Enter;
    Try
      if Terminate then Exit;
      Result := KeysQueue.Dequeue;
    finally
      AccessLock.Leave;
    end;
  end;
end;


function _ExecuteThread (Param: Pointer): Integer;
const
  SLEEP_TIME = 5;
var
  SyncQueue: LPTSyncQueueStruct;
  Package :LPTPackageRecord;
  IdleBegin, SleepTime: DWORD;
  IsRollBack: BOOL;
  IdleCounter: Integer;
begin
  SyncQueue := Param;
  Result := 0;
  IdleBegin := 0;
  IdleCounter := 1;
            
  With SyncQueue^ do
  Repeat
        SleepTime := Min (100, IdleCounter*2);
        Sleep (SleepTime);
        
        while not KeysQueue.IsEmpty do
        begin
            Sleep(1);
            IdleBegin := 0;
            IdleCounter := 1;
            if Terminate then
            begin
              ExecThreadID := 0;
              Exit;
            end;

            IsRollBack := False;
            Package := SafeDequeue (Param);
            if Assigned (Package) then
            begin
                Try
                  FDataHandler(Package.Sender, @Package.szBuf[0], Package.dwLen, IsRollBack);
                Except
                end;

                if IsRollBack then
                begin
                  SafeEnqueue(Param, Package);
                  Break;
                end else
                  HeapFree(hHeap, 0, Package);
            end;
        end;          
        
        if IdleBegin = 0 then
          IdleBegin := GetTickCount
        else
          Inc (IdleCounter);

        if Terminate then Break;
        if AppTerminated then Break;
  Until GetTickCount - IdleBegin > DurationIdleTime;

  SyncQueue.ExecThreadID := 0;
end;


function ExecuteThread (Param: Pointer): Integer;
var
  SyncQueue: LPTSyncQueueStruct absolute Param;
begin
  Try
    Result := _ExecuteThread (Param);
  Except
    SyncQueue.ExecThreadID := 0;
    Result := 0;
    OutputDebugString ('Handle SyncQueue Thread Error');
  end;
end;


Procedure PushSyncQueue (Handle: THandle; Sender, Buffer: Pointer; Size: Integer); Stdcall;
var
  SyncQueue: LPTSyncQueueStruct absolute Handle;
  Package :LPTPackageRecord;
begin
  if not assigned (SyncQueue) then exit;
  if Size = 0 then exit;
  if SyncQueue.Terminate then Exit;

  Package:= HeapAlloc(SyncQueue.hHeap, HEAP_ZERO_MEMORY, Size + SizeOf(TPackageRecord));
  Package.Sender := Sender;
  Package.dwLen := Size;
  CopyMemory(@Package.szBuf[0], Buffer, Size);

  SafeEnqueue (SyncQueue, Package);


  if SyncQueue.ExecThreadID = 0 then
  begin
    SyncQueue.hExecThread := BeginThread (nil, 0, ExecuteThread, SyncQueue, 0, SyncQueue.ExecThreadID);
    CloseHandle (SyncQueue.hExecThread);
  end;

end;

Procedure PushSyncQueueSTR (Handle: THandle; Sender: Pointer;  Msg: String); Stdcall;
begin
  Msg := Msg + #0;
  PushSyncQueue (Handle, Sender, PChar(Msg), Length (Msg));   
end;

function  GetSyncQueueCount  (Handle: THandle): Integer; Stdcall;
var
  SyncQueue: LPTSyncQueueStruct absolute Handle;
begin
  Result := -1;
  if not assigned (SyncQueue) then exit;
  Result := SyncQueue.KeysQueue.Count;
end;

Procedure FreeSyncQueue (Handle: THandle); Stdcall;
var
  SyncQueue: LPTSyncQueueStruct absolute Handle;
  TryCount: Integer;
begin
  if not assigned (SyncQueue) then exit;

  //先截止所有入队输入
  SyncQueue.AccessLock.Enter;
  SyncQueue.Terminate := True;
  SyncQueue.AccessLock.Leave;

  if SyncQueue.ExecThreadID > 0 then
  begin
    TryCount := 0;
    Repeat
      Sleep(100);
      Inc (TryCount);
    until (TryCount > 10) or (SyncQueue.ExecThreadID = 0);
//    if TryCount > 10 then
//      if SyncQueue.ExecThreadID > 0 then
//        TerminateThread (SyncQueue.hExecThread, 0);
  end;                   

  SyncQueue.KeysQueue.free;
  SyncQueue.AccessLock.Free;
  HeapDestroy (SyncQueue.hHeap);
  FreeMem (SyncQueue);
end;

end.
