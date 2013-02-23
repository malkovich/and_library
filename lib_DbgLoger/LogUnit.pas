unit LogUnit;

interface

uses Classes, Windows;

Procedure RunLogRoutine;

Procedure SelfLog (LogText: String);

var
  AppTerminate: BOOL;

implementation

uses AndQueMessages, SyncQueueHandler, MkSqLite3, SysUtils, SyncObjs, Base64Unit;

const
  DBG_HOST_IPC_NAME = '{96C9E8EE-65B2-431C-A61A-8CC45064A65B}';

var
  QueueHandle: THandle;
  TempLogSL, SqlCmdList, LogTypeMap, UpdateTimeSL: TStringList;
  LastExecSqlTime, IdleCounter, LastUpdateTypeListTime: DWORD;
  LogDB: TMkSqlite;
  CTS: TCriticalSection;

Procedure DbgPrint (Msg: String);
begin
  OutputDebugString (PChar(Msg));
end;

function GetLogTypeID (TypeName: String): String;
var
  Index, TypeID: Integer;
  RS:IMksqlStmt;
  SqlCmd, TimeStr: String;
begin
  Result := '';
  Index := LogTypeMap.IndexOf(TypeName);
  if Index = -1 then
  begin
    TimeStr := mkdateToStr (Now);
    SqlCmd := 'INSERT INTO TypeList (LogTypeName,LastActiveTime) VALUES (''' + TypeName + ''',''' + TimeStr + ''')';
    LogDB.execCmd(SqlCmd);
    SqlCmd := 'SELECT LogTypeID FROM TypeList WHERE LogTypeName=''' + TypeName + '''';
    RS := LogDB.exec (SqlCmd);
    if RS.EOF then
    begin
      RS := nil;
      Exit;
    end;
    
    TypeID := RS[0];
    RS := nil;
    Result := IntToStr(TypeID);
    LogTypeMap.AddObject(Result, Pointer(TypeID));
    Exit;
  end;
  TypeID := Integer(LogTypeMap.Objects[Index]);
  Result := IntToStr(TypeID);
end;


Procedure UpdateLogToDatabase;
var
  SqlCmd, LogType, TimeStr: String;
begin
  if SqlCmdList.Count = 0 then Exit;

  CTS.Enter;
  LogDB.beginTransaction;
  Try
    //日志入库
    for SqlCmd in SqlCmdList do
      LogDB.execCmd(SqlCmd);
    SqlCmdList.Clear;

    //定时更新列表的最后激活时间
    if GetTickCount - LastUpdateTypeListTime > 30*1000 then
    begin
      TimeStr := mkdateToStr (Now);
      for LogType in UpdateTimeSL do
      begin
        SqlCmd := 'UPDATE TypeList SET LastActiveTime=''' + TimeStr + ''' WHERE LogTypeName=''' + LogType + '''';
        LogDB.execCmd(SqlCmd);
      end;
      UpdateTimeSL.Clear;
      LastUpdateTypeListTime := GetTickCount;
    end;

  finally
    LogDB.Commit;
    CTS.Leave;
  end;          
end;

procedure LogMessageInDataBase (Sender:Pointer; pBuf:Pointer; dwLen:Integer; var Rollback: BOOL); Stdcall;
var
  LogMessage, LogTypeName, LogTypeID: String;
  SqlCmd, LogTime, Process, ProcessID, Module, LogText: String;
begin
  SetLength (LogMessage, dwLen);
  CopyMemory (@LogMessage[1], pBuf, dwLen);
  TempLogSL.Text := LogMessage;
  SetLength (LogMessage, 0);

  //检查映射表，取出类型ID
  LogTypeName := TempLogSL.Values['LogType'];
  LogTypeID := GetLogTypeID (LogTypeName);
  if LogTypeID = '' then Exit;

  //更新时间
  if UpdateTimeSL.IndexOf(LogTypeName) = -1 then
    UpdateTimeSL.Add(LogTypeName);

  //生成sql语句，等待批量入库
  LogTime := TempLogSL.Values['LogTime'];
  Process := TempLogSL.Values['Process'];
  ProcessID := TempLogSL.Values['ProcessID'];
  Module := TempLogSL.Values['Module'];
  LogText := TempLogSL.Values['LogText'];
  if LogText = '' then Exit;

  SqlCmd := 'INSERT INTO LogItems (LogTypeID,LogTime,Process,ProcessID,Module,LogText) VALUES (' +
             LogTypeID + ',''' + LogTime + ''',''' + Process + ''',''' + ProcessID + ''',''' + Module + ''',''' + LogText + ''')';
  SqlCmdList.Add(SqlCmd);

  //批量入库
  if GetTickCount - LastExecSqlTime > 2000 then
  begin
    UpdateLogToDatabase;
    LastExecSqlTime := GetTickCount;
  end;         
end;

Procedure SelfLog (LogText: String);
var
  PackageSL: TStringList;
  SendMsg: String;
begin
  PackageSL := TStringList.Create;
  PackageSL.Values['LogTime'] := mkdateToStr (Now);
  PackageSL.Values['Process'] := GetModuleName(0);
  PackageSL.Values['ProcessID'] := IntToStr(GetCurrentProcessID);  
  PackageSL.Values['Module']  := GetModuleName(0);
  PackageSL.Values['LogType'] := 'DbgLogerHost';
  PackageSL.Values['LogText'] := EncodeBase64 (LogText);
  SendMsg := PackageSL.Text;
  PackageSL.free;

  SendMsg := SendMsg + #0;
  PushSyncQueue (QueueHandle, nil, PChar(SendMsg), Length(SendMsg));
end;

var
  LogTempSL: TStringList;

Procedure LogMessageCallback (MsgBuf: Pointer; MsgSize: Integer); Stdcall;
var
  PrintLine, LogType: String;
begin
  if not assigned (LogTempSL) then
    LogTempSL := TStringList.Create;

  LogTempSL.Text := StrPas (MsgBuf);
  PrintLine := ExtractFileName(LogTempSL.Values['Module']);
  LogType := LogTempSL.Values['LogType'];
  if LogType <> PrintLine then
    PrintLine := PrintLine + '-' + LogType;
  PrintLine := PrintLine + #9 + DecodeBase64(LogTempSL.Values['LogText']);
  WriteLn (PrintLine);

  IdleCounter := 0;
  PushSyncQueue (QueueHandle, nil, MsgBuf, MsgSize);
end;                               


function GetSelfModuleName: string;
var
  SignWord: PWORD;
  SelfHandle: THandle;
  Buffer:array[byte] of char;
begin
  SignWord := @GetSelfModuleName;
  SignWord := Pointer (DWORD (SignWord) And $FFFFF000);

  while SignWord^ <> $5A4D do
    SignWord := Pointer (DWORD(SignWord) - $1000);

  SelfHandle := THandle (SignWord);

  ZeroMemory(@buffer[0], 256);
  GetModuleFileName(SelfHandle, @Buffer, 256);
  result := buffer;
end;

function GetDbgLogerDB : String;
begin
  Result := GetSelfModuleName;
  Result := ExtractFilePath (Result);
  Result :=Result + 'DbgLoger.DB';
end;

Procedure ExtractDBInitFile (DbName: String);
var
  RM: TResourceStream;
begin
  RM := TResourceStream.create(hinstance, 'DBSample', 'DB_FILE');
  RM.SaveToFile(DbName);
  RM.Free;
end;


Procedure RunLogRoutine;
var
  DbName: String;
begin
  DbName := GetDbgLogerDB;
  if not FileExists (DbName) then
    ExtractDBInitFile (DbName);

  LogDB := TMkSqlite.Create(nil);
  LogDB.dbName := AnsiToUtf8 (DbName);
  LogDB.open;

  QueueHandle := MakeSyncQueue (LogMessageInDataBase, 60*1000*3);
  SqlCmdList := TStringList.Create;
  TempLogSL := TStringList.Create;
  LogTypeMap := TStringList.Create;
  UpdateTimeSL := TStringList.Create;
  CTS := TCriticalSection.Create;
  IdleCounter := 0;
  AppTerminate := False;

  if CreateMessageQueue (DBG_HOST_IPC_NAME, LogMessageCallback) then
  begin
    WriteLn ('日志服务器成功启动！');
    Repeat
      Sleep(2000);
      Try
        if GetSyncQueueCount (QueueHandle) = 0 then
        begin
          UpdateLogToDatabase;
        end;
      except
        SelfLog ('RunLogRoutine has Exception!');
      end;
    until AppTerminate;
  end;

  DestroyMessageQueue (DBG_HOST_IPC_NAME);
  FreeSyncQueue (QueueHandle);
  CTS.Free;
  LogDB.close;
  LogDB.Free;

  SqlCmdList.Free;
  TempLogSL.Free;
  UpdateTimeSL.Free;
  LogTypeMap.Free;
end;

end.
