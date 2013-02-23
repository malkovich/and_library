unit LogDatabaseUnit;

interface
uses
  Windows, SysUtils, SyncObjs, Classes, MkSqLite3, Base64Unit, DateUtils;


Type
  TPrintType = (ptTime, ptProcess, ptProcessID, ptModule);
  TPrintTypes = set of TPrintType;

  TLogCallback = Procedure (LogType, Process, ProcessID, Module, LogText: PChar); Stdcall;

const
  ALL_PRINT_TYPE = [ptTime, ptProcess, ptProcessID, ptModule];


function GetLogTypeID (TypeName: String): String;
Procedure PushLogInDatabase (LogTime: TDateTime; LogType, Process, ProcessID, Module, LogText: String);

Function LogFile (LogType: PChar; FormTime, ToTime: TDateTime; OutFile: PChar): BOOL; Stdcall;
Function LogChecker (LogType: PChar; FormTime, ToTime: TDateTime; Checker: TLogCallback): BOOL; Stdcall;
Function LogPrinter (LogType: String; FormTime, ToTime: TDateTime; PrintTypes: TPrintTypes): TStringList;
Function GetActiveType (FromTime: TDateTime): TStringList;

Procedure GetConfigure (var IsViewTime,IsViewProcess,IsViewProcessID,IsViewModule: BOOL; var DaysOfAutoClear: Integer);
Procedure SetConfigure (IsViewTime,IsViewProcess,IsViewProcessID,IsViewModule: BOOL; DaysOfAutoClear: Integer);
Procedure DeleteHistory (ToTime: TDateTime); overload;
Procedure DeleteHistory; overload;
                                         
function GetSelfModuleName: string;

Procedure FinalDo;

implementation

var
  LogDB: TMkSqlite;
  CTS: TCriticalSection;  

///////////////////////////////////////////////////////////////////////////////
///

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
  Result := ExtractFilePath (Result) + 'DbgLoger.DB';
end;

Procedure ExtractDBInitFile (DbName: String);
var
  RM: TResourceStream;
begin
  RM := TResourceStream.create(hinstance, 'DBSample', 'DB_FILE');
  RM.SaveToFile(DbName);
  RM.Free;
end;

var
  LastCheckDeleteTime: DWORD;

Procedure DatabaseCheckpoint;
var
  DbName: String;
begin
  if not assigned (LogDB) then
  begin
    DbName := GetDbgLogerDB;
    //初始化数据库
    if not FileExists (DbName) then
      ExtractDBInitFile (DbName);
    //创建数据库
    LogDB := TMkSqlite.Create(nil);
    LogDB.dbName := AnsiToUtf8 (DbName);
    LogDB.open;
  end;    

  if not assigned (CTS) then
    CTS := TCriticalSection.Create;

  if GetTickCount - LastCheckDeleteTime > 30*60*1000 then
  begin
    LastCheckDeleteTime := GetTickCount;
    DeleteHistory; 
  end;
end;

var
  LogTypeMap: TStringList;

function GetLogTypeID (TypeName: String): String;
var
  Index, TypeID: Integer;
  RS:IMksqlStmt;
  SqlCmd, TimeStr: String;
begin
  DatabaseCheckpoint;
  Result := '';

  if not assigned (LogTypeMap) then
    LogTypeMap:= TStringList.Create;

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

function GetActiveType (FromTime: TDateTime): TStringList;
var
  RS:IMksqlStmt;
  SqlCmd, TimeStr, LogTypeName: String;
begin
  DatabaseCheckpoint;
  
  TimeStr := mkdateToStr (FromTime);
  Result := TStringList.Create;

  SqlCmd := 'SELECT LogTypeName FROM TypeList WHERE LastActiveTime>datetime(''' + TimeStr +''')';
  RS := LogDB.exec (SqlCmd);
  While not RS.EOF do
  begin
    LogTypeName := RS[0];
    Result.Add(LogTypeName);
    RS.next;
  end;

  Result.Sort;

  RS := nil;
end;

function BoolToInteger (IsValue: BOOL): String;
begin
  if IsValue then
    Result := '1'
  else
    Result := '0';
end;

Procedure SetConfigure (IsViewTime,IsViewProcess,IsViewProcessID,IsViewModule: BOOL; DaysOfAutoClear: Integer);
var
  SqlCmd: String;
begin
  DatabaseCheckpoint;

  SqlCmd := 'DELETE FROM Configure';
  LogDB.execCmd(SqlCmd);

  SqlCmd := 'INSERT INTO Configure (IsViewTime,IsViewProcess,IsViewProcessID,IsViewModule,DaysOfAutoClear) VALUES (' +
             BoolToInteger(IsViewTime) + ',' + BoolToInteger(IsViewProcess) + ',' + BoolToInteger(IsViewProcessID) + ',' + BoolToInteger(IsViewModule) + ',' + IntToStr(DaysOfAutoClear) + ')';
  LogDB.execCmd(SqlCmd);
end;

Procedure GetConfigure (var IsViewTime,IsViewProcess,IsViewProcessID,IsViewModule: BOOL; var DaysOfAutoClear: Integer);
var
  SqlCmd: String;
  RS:IMksqlStmt;
begin
  DatabaseCheckpoint;

  SqlCmd := 'SELECT * FROM Configure';
  RS := LogDB.exec (SqlCmd);
  if not RS.EOF then
  begin
    IsViewTime := RS[0] = 1;
    IsViewProcess := RS[1] = 1;
    IsViewProcessID := RS[2] = 1;
    IsViewModule := RS[3] = 1;
    DaysOfAutoClear := RS[4];
    RS := nil;
    Exit;
  end;
  RS := nil;

  IsViewTime := True;
  IsViewProcess := True;
  IsViewProcessID := True;
  IsViewModule := True;
  DaysOfAutoClear := 3;
  SetConfigure (IsViewTime, IsViewProcess, IsViewProcessID, IsViewModule, DaysOfAutoClear);
end;

Procedure DeleteHistory (ToTime: TDateTime);
var
  SqlCmd, ToTimeStr: String;
begin
  ToTimeStr := mkdateToStr (ToTime);
  CTS.Enter;
  Try
    SqlCmd := 'DELETE FROM LogItems WHERE LogTime<datetime(''' +ToTimeStr+''')';
    LogDB.execCmd(SqlCmd);
    SqlCmd := 'DELETE FROM TypeList WHERE LastActiveTime<datetime(''' +ToTimeStr+''')';
    LogDB.execCmd(SqlCmd);
  finally
    CTS.Leave;
  end;
end;

Procedure DeleteHistory;
var
  IsViewTime,IsViewProcess,IsViewProcessID,IsViewModule: BOOL;
  DaysOfAutoClear: Integer;
  ToTime: TDateTime;
begin
  GetConfigure (IsViewTime,IsViewProcess,IsViewProcessID,IsViewModule,DaysOfAutoClear);
  ToTime := Now;
  ToTime := IncDay (ToTime, -DaysOfAutoClear);
  DeleteHistory (ToTime);
end;


Procedure PushLogInDatabase (LogTime: TDateTime; LogType, Process, ProcessID, Module, LogText: String);
var
  LogTextBase64, LogTypeID, LogTimeStr: String;
  SqlCmd: String;
begin
  DatabaseCheckpoint;
  
  LogTextBase64 := EncodeBase64 (LogText);
  LogTimeStr := mkdateToStr (LogTime);

  CTS.Enter;
  Try
    LogTypeID := GetLogTypeID (LogType);
    if LogTypeID = '' then exit;

    SqlCmd := 'INSERT INTO LogItems (LogTypeID,LogTime,Process,ProcessID,Module,LogText) VALUES (' +
             LogTypeID + ',''' + LogTimeStr + ''',''' + Process + ''',''' + ProcessID + ''',''' + Module + ''',''' + LogTextBase64 + ''')';
    LogDB.execCmd(SqlCmd);

    SqlCmd := 'UPDATE TypeList SET LastActiveTime=''' + LogTimeStr + ''' WHERE LogTypeName=''' + LogType + '''';
    LogDB.execCmd(SqlCmd);
  finally
    CTS.Leave;
  end;
end;

Procedure WriteLnToStream (MM: TStream; Line: String);
begin
  Line := Line + #13#10;
  MM.Write(Line[1], Length(Line));
end;

Function LogChecker (LogType: PChar; FormTime, ToTime: TDateTime; Checker: TLogCallback): BOOL; Stdcall;
var
  SqlCmd: String;
  LogTime,Process,ProcessID,Module,LogText: String;
  FromTimeStr, ToTimeStr, LogTypeID: String;
  RS:IMksqlStmt;
begin
  DatabaseCheckpoint;
  Result := False;

  LogTypeID := GetLogTypeID (StrPas(LogType));
  if LogTypeID = '' then exit;

  FromTimeStr := mkdateToStr (FormTime);
  ToTimeStr := mkdateToStr (ToTime);
  SqlCmd := 'SELECT LogTime,Process,ProcessID,Module,LogText FROM LogItems WHERE LogTypeID=' + LogTypeID + ' AND LogTime>datetime(''' +FromTimeStr+''') AND LogTime<datetime(''' + ToTimeStr +''')';

  RS := LogDB.exec(SqlCmd);
  while NOT RS.EOF do
  begin
    LogTime := RS[0];
    Process := RS[1];
    ProcessID := RS[2];
    Module := RS[3];
    LogText := RS[4];
    LogText := DecodeBase64 (LogText);

    Checker (PChar(LogTime), PChar(Process), PChar(ProcessID), PChar(Module), PChar(LogText));
    RS.next;
  end;
end;

function MakeHeadStr (LogTime,Process,ProcessID,Module: String; PrintTypes: TPrintTypes): String;
begin
    Result := '';
    if ptTime in PrintTypes then
      Result := LogTime;

    if ptProcess in PrintTypes then
    begin
      if Result = '' then
        Result := Process
      else
        Result := Result + #9 + Process;

      if ptProcessID in PrintTypes then
        Result := Result + ':' + ProcessID;
    end else
    begin
      if ptProcessID in PrintTypes then
      begin
        if Result = '' then
          Result := ProcessID
        else
          Result := Result + #9 + ProcessID;
      end;
    end;

    if ptModule in PrintTypes then
    begin
      if Result = '' then
        Result := Module
      else
        Result := Result + #9 + Module;
    end;
end;          


Function LogFile (LogType: PChar; FormTime, ToTime: TDateTime; OutFile: PChar): BOOL; Stdcall;
var
  ResultSL: TStringList;
begin
  ResultSL := LogPrinter (StrPas(LogType), FormTime, ToTime, [ptTime,ptProcess,ptProcessID,ptModule]);
  ResultSL.SaveToFile(StrPas(OutFile));
  Result := ResultSL.Count > 0;
  ResultSL.Free;
end;

Procedure DbgLog(msg: String);
begin
  OutputDebugString (PChar(msg));
end;

Function LogPrinter (LogType: String; FormTime, ToTime: TDateTime; PrintTypes: TPrintTypes): TStringList;
var
  SqlCmd: String;
  LogTime,Process,ProcessID,Module,LogText, WriteStr: String;
  FromTimeStr, ToTimeStr, LogTypeID: String;
  RS:IMksqlStmt;
begin
  DatabaseCheckpoint;
  Result := TStringList.Create;

  FromTimeStr := mkdateToStr (FormTime);
  ToTimeStr := mkdateToStr (ToTime);

  if LogType = 'AnyType' then
  begin
    SqlCmd := 'SELECT LogTime,Process,ProcessID,Module,LogText FROM LogItems WHERE LogTime>datetime(''' +FromTimeStr+''') AND LogTime<datetime(''' + ToTimeStr +''')';
  end else
  begin
    LogTypeID := GetLogTypeID (LogType);
    if LogTypeID = '' then exit;
    SqlCmd := 'SELECT LogTime,Process,ProcessID,Module,LogText FROM LogItems WHERE LogTypeID=' + LogTypeID + ' AND LogTime>datetime(''' +FromTimeStr+''') AND LogTime<datetime(''' + ToTimeStr +''')';
  end;

  RS := LogDB.exec(SqlCmd);
  while NOT RS.EOF do
  begin
    LogTime := RS[0];
    Process := RS[1];
    ProcessID := RS[2];
    Module := RS[3];
    LogText := RS[4];

    Process := ExtractFileName (Process);
    Module := ExtractFileName (Module);
    LogText := DecodeBase64 (LogText);

    WriteStr := MakeHeadStr (LogTime,Process,ProcessID,Module, PrintTypes);
    if WriteStr = '' then
      Result.Add (LogText)
    else begin
      WriteStr := WriteStr + #9 + LogText;
      Result.Add (WriteStr);
    end;

    RS.next;
  end;
end;

Procedure FinalDo;
begin
  if assigned (LogDB) then
  begin
    LogDB.close;
    FreeAndNil (LogDB);
  end;

  if assigned (CTS) then
    FreeAndNil (CTS);
end;


end.
