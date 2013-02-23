unit LogerDllUnit;

interface
uses
 Windows, SysUtils, madCHook, AndQueMessages, SyncObjs, madKernel, TCacheQueueThread,
  Classes, MkSqLite3, Base64Unit, RunAsUserUnit;

const
  LOG_REAL_MODE = 0;
  LOG_PROXY_MODE = 1;
  LOG_CACHE_PROXY_MODE = 2;
  LOG_SYNC_REAL_MODE = 3;
  LOG_SYNC_PROXY_MODE = 4;


Procedure LOG (LogType: PChar; LogText: PChar; LogMode: Integer); Stdcall;
                   
Procedure InitialDbgLogerDLL;
Procedure FinalDbgLogerDLL;
                                 
implementation

uses SyncQueueHandler, LogDatabaseUnit;

var
  ProcessName: String;
  ProcessID: String;
  ProxyHostFileName: String;
  
var
  LogQueue: THandle;
  PackageSL: TStringList;
  ModuleMapSL: TStringList;


function GetProxyHostFileName : String;
begin
  Result := GetSelfModuleName;
  Result := ExtractFilePath (Result);
  Result :=Result + 'DbgHost.exe';
end;

function GetCurrentProcessName: String;
var
  Buffer:array[byte] of char;
begin
  ZeroMemory(@buffer[0], 256);
  GetModuleFileName(0, @Buffer, 256);
  result := buffer;
end;


function GetModuleFromMap (CallingModule: THandle): String;
var
  ModuleKey: String;
  ModuleIndex: Integer;
begin
  if Not Assigned (ModuleMapSL) then
    ModuleMapSL := TStringList.Create;

  ModuleKey := IntToStr (CallingModule);
  ModuleIndex := ModuleMapSL.IndexOfName(ModuleKey);
  if ModuleIndex = -1 then
  begin
    Result := GetModuleName (CallingModule);
    ModuleMapSL.Values[ModuleKey] := Result;
  end else
    Result := ModuleMapSL.ValueFromIndex[ModuleIndex];
end;


Type
  LPTQueueStruct = ^TQueueStruct;
  TQueueStruct = record
    Size: Integer;
    CallingModule: THandle;
    LogTime: TDateTime;
    TypeTextBuffer: Array[0..0] of char;
  end;

const
  DBG_HOST_IPC_NAME = '{96C9E8EE-65B2-431C-A61A-8CC45064A65B}';

function GetQueueStruct (CallingModule: THandle; LogType, LogText: PChar): LPTQueueStruct;
var
  LogTypeLen, LotTextLen, StruLen: Integer;
  BuffPtr: PChar;
begin
  LogTypeLen := StrLen (LogType);
  LotTextLen := StrLen (LogText);
  StruLen := SizeOf (TQueueStruct) + LogTypeLen + LotTextLen + 2;

  Result := AllocMem (StruLen);
  Result.Size := StruLen;
  Result.CallingModule := CallingModule;
  Result.LogTime := NOW;

  BuffPtr := @Result.TypeTextBuffer[0];
  StrCopy (BuffPtr, LogType);
  Inc (BuffPtr, LogTypeLen);
  BuffPtr^ := #0;
  Inc (BuffPtr);

  StrCopy (BuffPtr, LogText);
  Inc (BuffPtr, LotTextLen);
  BuffPtr^ := #0;
end;

Procedure GetQueueLogText (QueueStru: LPTQueueStruct; var LogType, LogText: PChar);
var
  LogTypeLen: Integer;
begin
  LogType := @QueueStru.TypeTextBuffer[0];
  LogTypeLen := StrLen (LogType);
  LogText := @QueueStru.TypeTextBuffer[LogTypeLen+1];
end;


Procedure DbgPrint (Msg: String);
begin
  OutputDebugString (PChar(Msg));
end;

Function SendLogMessage (LogMessage: String): BOOL;
begin
  Result := SendQueMessage (DBG_HOST_IPC_NAME, LogMessage);
end;


procedure LogMessageHandle (Sender:Pointer; pBuf:Pointer; dwLen:dword; var Rollback: BOOL); Stdcall;
var
  QueueStruct: LPTQueueStruct absolute pBuf;
  LogType, LogText: PChar;
begin
  GetQueueLogText (QueueStruct, LogType, LogText);

  PackageSL.Clear;
  PackageSL.Values['LogTime'] := mkdateToStr (QueueStruct.LogTime);
  PackageSL.Values['Process'] := ProcessName;
  PackageSL.Values['ProcessID'] := ProcessID;  
  PackageSL.Values['Module']  := GetModuleFromMap (QueueStruct.CallingModule);
  PackageSL.Values['LogType'] := StrPas (LogType);
  PackageSL.Values['LogText'] := EncodeBase64 (StrPas (LogText));
  SendLogMessage (PackageSL.Text);
end;


Procedure DBG_CacheProxy_Mode (CallingModule: THandle; LogType: PChar; LogText: PChar);
var
  QueueStruct: LPTQueueStruct;
begin
  InitialDbgLogerDLL;

  QueueStruct := GetQueueStruct (CallingModule, LogType, LogText);
  PushCacheQueue (LogQueue, nil, Pointer(QueueStruct), QueueStruct.Size);
  FreeMem (QueueStruct);
end;

///////////////////////////////////////////////////////////////////////////////
///

Procedure DBG_Proxy_Mode (CallingModule: THandle; LogType: PChar; LogText: PChar);
var
  PackageSL: TStringList;
begin
  PackageSL := TStringList.Create;
  PackageSL.Values['LogTime'] := mkdateToStr (Now);
  PackageSL.Values['Process'] := ProcessName;
  PackageSL.Values['ProcessID'] := ProcessID;  
  PackageSL.Values['Module']  := GetModuleFromMap (CallingModule);
  PackageSL.Values['LogType'] := StrPas (LogType);
  PackageSL.Values['LogText'] := EncodeBase64 (StrPas (LogText));
  SendLogMessage (PackageSL.Text);
  PackageSL.free;
end;

Procedure DBG_Real_Mode (CallingModule: THandle; LogType: PChar; LogText: PChar);
var
  Module: String;
begin
  Module := GetModuleFromMap (CallingModule);
  PushLogInDatabase (Now, StrPas(LogType), ProcessName, ProcessID, Module, StrPas(LogText));
end;

//////////////////////////////////////////////////////////////////////////
///////

procedure SyncProxyQueueHandle (Sender:Pointer; pBuf:Pointer; dwLen:Integer; var Rollback: BOOL); Stdcall;
var
  QueueStruct: LPTQueueStruct absolute pBuf;
  LogType, LogText: PChar;
begin
  GetQueueLogText (QueueStruct, LogType, LogText);
  DBG_Proxy_Mode (QueueStruct.CallingModule, LogType, LogText);
end;
                      
var
  SyncProxyQueue: THandle;

Procedure DBG_Sync_Proxy_Mode (CallingModule: THandle; LogType: PChar; LogText: PChar);
var
  QueueStruct: LPTQueueStruct;
begin
  if SyncProxyQueue = 0 then
    SyncProxyQueue := MakeSyncQueue (SyncProxyQueueHandle);

  QueueStruct := GetQueueStruct (CallingModule, LogType, LogText);
  PushSyncQueue (SyncProxyQueue, nil, Pointer(QueueStruct), QueueStruct.Size);
  FreeMem (QueueStruct);
end;

//////////////////////////////////////////////////////////////////////////
///////

procedure SyncRealQueueHandle (Sender:Pointer; pBuf:Pointer; dwLen:Integer; var Rollback: BOOL); Stdcall;
var
  QueueStruct: LPTQueueStruct absolute pBuf;
  LogType, LogText: PChar;
begin
  GetQueueLogText (QueueStruct, LogType, LogText);
  DBG_Real_Mode (QueueStruct.CallingModule, LogType, LogText);
end;
                
var
  SyncRealQueue: THandle;

Procedure DBG_Sync_Real_Mode (CallingModule: THandle; LogType: PChar; LogText: PChar);
var
  QueueStruct: LPTQueueStruct;
begin
  if SyncRealQueue = 0 then
    SyncRealQueue := MakeSyncQueue (SyncRealQueueHandle);

  QueueStruct := GetQueueStruct (CallingModule, LogType, LogText);
  PushSyncQueue (SyncRealQueue, nil, Pointer(QueueStruct), QueueStruct.Size);
  FreeMem (QueueStruct);
end;


Procedure LOG (LogType: PChar; LogText: PChar; LogMode: Integer); Stdcall;
var
  CallingModule: THandle;
begin
  if ProcessName = '' then
  begin
    ProcessName := GetCurrentProcessName;
    ProcessID := IntToStr(GetCurrentProcessID);
    ProxyHostFileName := GetProxyHostFileName;
  end;
      
  CallingModule := GetCallingModule;
  case LogMode of
    LOG_CACHE_PROXY_MODE: DBG_CacheProxy_Mode (CallingModule, LogType, LogText);
    LOG_PROXY_MODE:       DBG_Proxy_Mode (CallingModule, LogType, LogText);
    LOG_REAL_MODE:        DBG_Real_Mode (CallingModule, LogType, LogText);
    LOG_SYNC_PROXY_MODE:  DBG_Sync_Proxy_Mode (CallingModule, LogType, LogText);
    LOG_SYNC_REAL_MODE:   DBG_Sync_Real_Mode (CallingModule, LogType, LogText);
    Else                  DBG_Real_Mode (CallingModule, LogType, LogText);
  end;
end;

Procedure InitialDbgLogerDLL;
begin
  if LogQueue = 0 then
    LogQueue := MakeCacheQueue (LogMessageHandle);

  if not Assigned (PackageSL) then
    PackageSL := TStringList.Create;
end;

Procedure FinalDbgLogerDLL;
begin
  if LogQueue > 0 then
  begin
    FreeCacheQueue (LogQueue);
    LogQueue := 0;
  end;

  if SyncRealQueue > 0 then
  begin
    FreeSyncQueue (SyncRealQueue);
    SyncRealQueue := 0;
  end;

  if SyncProxyQueue > 0 then
  begin
    FreeSyncQueue (SyncProxyQueue);
    SyncProxyQueue := 0;
  end;    

  if Assigned (PackageSL) then
    FreeAndNil (PackageSL);

  FinalDo;
end;


end.
