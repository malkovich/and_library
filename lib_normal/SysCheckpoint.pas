Unit SysCheckpoint;
//对于耗时运算，需要系统内单一执行状态，但又不想开守护线程
//而通过其他线程获得动力

interface

uses
  windows;

Type
  TCheckCondition = Function (Name: String): BOOL;
  TChecker = Procedure (Name: String);

Function  CreateCheckpoint (Name: String; Condition: TCheckCondition; Checker: TChecker; Internal: DWORD = 1000): THandle;
Procedure DestroyCheckpoint (var Check: THandle);

Function CheckPoint (Check: THandle): BOOL;

implementation

uses SysUtils, SyncObjs;

Type
  LPTCheckItem = ^TCheckItem;
  TCheckItem = record
    Name: String;
    ThreadMutexName: String;
    Condition: TCheckCondition;
    ConditionMutex: TMutex;
    Checker: TChecker;
    Internal: DWORD;
    LastCheckTime: DWORD;
    Data: TObject;
  end;

Function  CreateCheckpoint (Name: String; Condition: TCheckCondition; Checker: TChecker; Internal: DWORD = 1000): THandle;
var
  CheckItem: LPTCheckItem;
begin
  New (CheckItem);
  CheckItem.Name := Name;
  CheckItem.ThreadMutexName := Name + '_Thread';
  CheckItem.Condition := Condition;
  CheckItem.ConditionMutex := TMutex.Create(NIL, False, CheckItem.Name);
  CheckItem.Checker := Checker;
  CheckItem.Internal := Internal;
  Result := THandle (CheckItem);
end;

Procedure DestroyCheckpoint (var Check: THandle);
var
  CheckItem: LPTCheckItem absolute Check;
begin
  CheckItem.ConditionMutex.Free;
  Dispose (CheckItem);
  Check := 0;
end;

function RunCheckProc (Param: Pointer): Integer;
var
  CheckItem: LPTCheckItem absolute Param;
  Mutex: TMutex;
  Event: TEvent;
  Checker: TChecker;
  Name: String;
begin
  Mutex := TMutex.Create(NIL, True, CheckItem.ThreadMutexName);
  Try
    Checker := CheckItem.Checker;
    Name := CheckItem.Name;
    Event := CheckItem.Data as TEvent;
    Event.SetEvent;

    Checker (Name);
  finally
    Mutex.Release;
    Mutex.Free;
  end;
  Result := 0;
end;

Function CheckPoint (Check: THandle): BOOL;
var
  CheckItem: LPTCheckItem absolute Check;
  Event: TEvent;
  ThreadMutex: THandle;
  TID: DWORD;
begin
  Result := False;
  if Check = 0 then exit;
  With CheckItem^ do
  begin
    //减少同进程的从入机会
    if GetTickCount - LastCheckTime > Internal then
    begin
      //确保系统内单一执行权限
      if wrSignaled = ConditionMutex.WaitFor(0) then
      begin
        if Condition(Name) then
        begin
          ThreadMutex := OpenMutex (MUTANT_QUERY_STATE, False, PChar(ThreadMutexName));
          if ThreadMutex = 0 then
          begin
            Event := TEvent.Create(nil, False, False, '');
            CheckItem.Data := Event;
            CloseHandle(BeginThread(NIL, 0, RunCheckProc, Pointer(Check), 0, TID));
            Event.WaitFor(INFINITE);
            Event.Free;
            Result := True;
          end else
            CloseHandle (ThreadMutex);
        end;
        ConditionMutex.Release;
      end;
      LastCheckTime := GetTickCount;
    end;
  end;
end;

end.
