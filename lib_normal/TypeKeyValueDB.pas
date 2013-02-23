unit TypeKeyValueDB;

{$R 'TypeKeyValue.res' 'TypeKeyValue.rc'}

interface
uses windows, classes;

function  ConfigOpen (DbName: String): THandle;
procedure ConfigClose (var DbHandle: THandle);

Procedure ConfigWrite (DbHandle: THandle; Section, Key, Value: String);
Procedure ConfigWriteInt (DbHandle: THandle; Section, Key: String; Value: Integer);

function  ConfigRead (DbHandle: THandle; Section, Key: String): String;
function  ConfigReadInt (DbHandle: THandle; Section, Key: String; Out ReadValue: Integer): BOOL;
function  ConfigReadSection (DbHandle: THandle; Section: String): TStringList;

Procedure ConfigDelete (DbHandle: THandle; Section: String); overload;
Procedure ConfigDelete (DbHandle: THandle; Section, Key: String); overload;
Procedure ConfigDelete (DbHandle: THandle); overload;

implementation

uses MkSqLite3, SyncObjs, SysUtils;

Type
  LPTConfigItem = ^TConfigItem;
  TConfigItem = record
    DbName: String;
    CTS: TCriticalSection;
    DataBase: TMkSqlite;
  end;

function ExtractDBInitFile (DbName: String): BOOL;
var
  RM: TResourceStream;
begin
  RM := TResourceStream.create(hinstance, 'DBSample', 'DB_FILE');
  Result := RM.Size > 0;
  RM.SaveToFile(DbName);
  RM.Free;
end;

function  ConfigOpen (DbName: String): THandle;
var
  ConfigItem: LPTConfigItem;
begin
  Result := 0;
  if Not FileExists (DbName) then
    if not ExtractDBInitFile (DbName) then exit;

  New (ConfigItem);
  ConfigItem.DbName := DbName;
  ConfigItem.CTS := TCriticalSection.Create;
  ConfigItem.DataBase := TMkSqlite.Create(nil);
  ConfigItem.DataBase.dbName := AnsiToUtf8(DbName);
  ConfigItem.DataBase.open;
  Result := THandle (ConfigItem);
end;

procedure ConfigClose (var DbHandle: THandle);
var
  ConfigItem: LPTConfigItem absolute DbHandle;
begin
  if DbHandle = 0 then exit;
  ConfigItem.DataBase.close;
  ConfigItem.DataBase.Free;
  ConfigItem.CTS.Free;
  DisPose(ConfigItem);
  DbHandle := 0;
end;

//TypeKeyValue: ID Type Key Value UpdateTime
Procedure ConfigWrite (DbHandle: THandle; Section, Key, Value: String);
var
  ConfigItem: LPTConfigItem absolute DbHandle;
  SqlCmd, TimeStr: String;
  RS:IMksqlStmt;
  ID: Integer;
  IsUpdate: BOOL;
begin
  if DbHandle = 0 then exit;
  ConfigItem.CTS.Enter;
  Try
    SqlCmd := 'SELECT ID FROM TypeKeyValue WHERE Type=''' + Section + ''' AND Key=''' + Key + '''';
    RS := ConfigItem.DataBase.exec (SqlCmd);
    ID := -1;
    IsUpdate := False;
    if not RS.eof then
    begin
      ID := RS[0];
      IsUpdate := True;
    end;
    RS := nil;
    
    TimeStr := mkdateToStr (NOW);
    if IsUpdate then
      SqlCmd := 'UPDATE TypeKeyValue SET Value=''' + Value + ''',UpdateTime=''' + TimeStr + ''' WHERE ID=' + IntToStr(ID)
    else
      SqlCmd := 'INSERT INTO TypeKeyValue (Type,Key,Value,UpdateTime) VALUES(''' + Section + ''',''' + Key + ''',''' + Value + ''',''' + TimeStr + ''')';
    ConfigItem.DataBase.execCmd (SqlCmd);  
  finally
    ConfigItem.CTS.Leave;
  end;
end;

//TypeKeyValue: ID Type Key Value UpdateTime
function  ConfigRead (DbHandle: THandle; Section, Key: String): String;
var
  ConfigItem: LPTConfigItem absolute DbHandle;
  SqlCmd: String;
  RS:IMksqlStmt;
begin
  Result := '';
  if DbHandle = 0 then exit;
  ConfigItem.CTS.Enter;
  Try
    SqlCmd := 'SELECT Value FROM TypeKeyValue WHERE Type=''' + Section + ''' AND Key=''' + Key + '''';
    RS := ConfigItem.DataBase.exec (SqlCmd);
    if not RS.eof then
    begin
      Result := RS[0];
    end;
    RS := nil;
  finally
    ConfigItem.CTS.Leave;
  end;                   
end;

function  ConfigReadSection (DbHandle: THandle; Section: String): TStringList;
var
  ConfigItem: LPTConfigItem absolute DbHandle;
  SqlCmd, Key, Value: String;
  RS:IMksqlStmt;
begin
  Result := TStringList.Create;
  if DbHandle = 0 then exit;
  
  ConfigItem.CTS.Enter;
  Try
    SqlCmd := 'SELECT Key,Value FROM TypeKeyValue WHERE Type=''' + Section + '''';
    RS := ConfigItem.DataBase.exec (SqlCmd);
    while not RS.eof do
    begin
      Key := RS[0];
      Value := RS[1];
      Result.Values[Key] := Value;
      RS.next;
    end;
    RS := nil;
  finally
    ConfigItem.CTS.Leave;
  end;                   
end;


function  ConfigReadInt (DbHandle: THandle; Section, Key: String; Out ReadValue: Integer): BOOL;
var
  ReadStr: String;
begin
  Result := False;
  ReadStr := ConfigRead (DbHandle, Section, Key);
  if ReadStr = '' then exit;

  if TryStrToInt (ReadStr, ReadValue) then
    Result := True;  
end;

Procedure ConfigWriteInt (DbHandle: THandle; Section, Key: String; Value: Integer);
var
  WriteStr: String;
begin
  WriteStr := IntToStr(Value);
  ConfigWrite (DbHandle, Section, Key, WriteStr);
end;

Procedure ConfigDelete (DbHandle: THandle; Section: String); overload;
var
  ConfigItem: LPTConfigItem absolute DbHandle;
  SqlCmd: String;
begin
  if DbHandle = 0 then exit;
  ConfigItem.CTS.Enter;
  Try
    SqlCmd := 'DELETE FROM TypeKeyValue WHERE Type=''' + Section + '''';
    ConfigItem.DataBase._exec (SqlCmd);
  finally
    ConfigItem.CTS.Leave;
  end;                   
end;

Procedure ConfigDelete (DbHandle: THandle; Section, Key: String); overload;
var
  ConfigItem: LPTConfigItem absolute DbHandle;
  SqlCmd: String;
begin
  if DbHandle = 0 then exit;
  ConfigItem.CTS.Enter;
  Try
    SqlCmd := 'DELETE FROM TypeKeyValue WHERE Type=''' + Section + ''' AND Key=''' + Key + '''';
    ConfigItem.DataBase._exec (SqlCmd); 
  finally
    ConfigItem.CTS.Leave;
  end;                   
end;

Procedure ConfigDelete (DbHandle: THandle); overload;
var
  ConfigItem: LPTConfigItem absolute DbHandle;
  SqlCmd: String;
begin
  if DbHandle = 0 then exit;
  ConfigItem.CTS.Enter;
  Try
    SqlCmd := 'DELETE FROM TypeKeyValue';
    ConfigItem.DataBase._exec (SqlCmd); 
  finally
    ConfigItem.CTS.Leave;
  end;                   
end;


end.
