unit RegistCodeUnit;

interface
uses
  SysUtils, CodeEncrypt, SyncObjs, base64unit, MkSqLite3, Windows, IniFiles, RC4Extractor, Classes,
  DateUtils, Types, ComObj;

procedure Initial (DbFileName: String);
procedure Finially;

function RegistMachine (RegistAccount, RegisterCode: String): BOOL;

function MakeRegistCodesFile (Count: Integer; NumberOfDays: Integer): String;
function MakeRegistCodesFileEx (BatchCode: String; Count: Integer; ExpiredTime: TDateTime): String;
function EnableBatchCode (BatchCode: String; Enable: BOOL): BOOL;
function GetBatchCodeFile (BatchCode: String; Out OutRegistFile: String): BOOL;
function DeleteBatchCode (BatchCode: String): BOOL;
function DeleteExpiredRegistCodes: BOOL;
function DeleteUnActiveRegistCodes: BOOL;
function DeleteUsedRegistCodes: BOOL;
function EmptyRegistCodes: BOOL;

implementation

uses LogHost;

const
  SQLITE_OK         =  0;

var
  UserDB: TMkSqlite;


function CheckBatchCode (BatchCode: String): String;
var
  SqlCmd: String;
  RS:IMksqlStmt;
  IsEof: BOOL;
begin
  Result := '';

  SqlCmd := 'SELECT BatchCode FROM RegisterCodes WHERE BatchCode=''' + BatchCode + '''';
  RS := UserDB.exec (SqlCmd);
  IsEof := RS.EOF;
  RS := nil;

  if not IsEof then
  begin
    Result := BatchCode;
    Exit;
  end;

  SqlCmd := 'SELECT BatchCode FROM RegisterCodes WHERE RegisterCode=''' + BatchCode + '''';
  RS := UserDB.exec (SqlCmd);
  if RS.rowCount = 1 then
    Result := RS[0];
  RS := nil;                             
end;

function EnableBatchCode (BatchCode: String; Enable: BOOL): BOOL;
const
  SQLITE_OK         =  0;   // Successful result
var
  SqlCmd, EnableStr: String;
  SqlResult: Integer;
begin
  Result := False;
  BatchCode := CheckBatchCode (BatchCode);
  if BatchCode = '' then exit;

  if Enable then
    EnableStr := 'True'
  else
    EnableStr := 'False';

  SqlCmd := 'UPDATE RegisterCodes SET IsActive=''' + EnableStr + ''' WHERE BatchCode=''' + BatchCode + '''';
  UserDB.execCmd(SqlCmd);
  Result := True;
  Exit;

  SqlResult := UserDB._exec(SqlCmd);
  Result := SQLITE_OK = SqlResult;
  if not result then
    WriteLn ('SQLITE Result : ' + IntToStr(SqlResult));
end;

function DeleteBatchCode (BatchCode: String): BOOL;
const
  SQLITE_OK         =  0;   // Successful result
var
  SqlCmd: String;
begin
  Result := False;
  BatchCode := CheckBatchCode (BatchCode);
  if BatchCode = '' then exit;
  
  SqlCmd := 'DELETE FROM RegisterCodes WHERE BatchCode=''' + BatchCode + '''';
  Result := SQLITE_OK = UserDB._exec(SqlCmd);
end;

function DeleteExpiredRegistCodes: BOOL;
const
  SQLITE_OK =  0;
var
  SqlCmd, NowTime: String;
begin
  NowTime := mkDateToStr (Now);
  SqlCmd := 'DELETE FROM RegisterCodes WHERE ExpiredTime<datetime(''' +NowTime+''')';
  Result := SQLITE_OK = UserDB._exec(SqlCmd);
end;

function DeleteUnActiveRegistCodes: BOOL;
const
  SQLITE_OK =  0;
var
  SqlCmd: String;
begin
  SqlCmd := 'DELETE FROM RegisterCodes WHERE IsActive=''False''';
  Result := SQLITE_OK = UserDB._exec(SqlCmd);
end;

function DeleteUsedRegistCodes: BOOL;
const
  SQLITE_OK =  0;
var
  SqlCmd: String;
begin
  SqlCmd := 'DELETE FROM RegisterCodes WHERE RegistAccount NOT NULL';
  Result := SQLITE_OK = UserDB._exec(SqlCmd);
end;

function EmptyRegistCodes: BOOL;
const
  SQLITE_OK =  0;
var
  SqlCmd: String;
begin
  SqlCmd := 'DELETE FROM RegisterCodes';
  Result := SQLITE_OK = UserDB._exec(SqlCmd);
end;

function GetBatchCodeFile (BatchCode: String; Out OutRegistFile: String): BOOL;
var
  RegistFile: TStringList;
  EnableStr, RegistAccount, AddStr: String;
  SqlCmd, RegisterCode, CreateTimeStr, ExpiredTimeStr: String;
  AddedCount: Integer;
  RS:IMksqlStmt;
begin
  Result := False;
  OutRegistFile := '';
  BatchCode := CheckBatchCode (BatchCode);
  if BatchCode = '' then exit;


  SqlCmd := 'SELECT IsActive,RegisterCode,CreateTime,ExpiredTime,RegistAccount FROM RegisterCodes WHERE BatchCode=''' + BatchCode + '''';
  RS := UserDB.exec(SqlCmd);

  RegistFile := TStringList.Create;
  AddedCount := 0;
  RS.first;
  while Not RS.eof do
  begin
    if CreateTimeStr = '' then
    begin
      CreateTimeStr := RS[2];
      ExpiredTimeStr := RS[3];
      RegistFile.Append('[注册码批号]');
      RegistFile.Append('BatchCode=' + BatchCode);
      RegistFile.Append('');
      RegistFile.Append('[有效期]');
      RegistFile.Append('CreateTime=' + CreateTimeStr);
      RegistFile.Append('ExpiredTime=' + ExpiredTimeStr);
      RegistFile.Append('');
      RegistFile.Append('[注册码列表]');
    end;

    Inc (AddedCount);
    EnableStr := RS[0];
    RegisterCode := RS[1];
    if RS.isNull(4) then
      AddStr :=  IntToStr(AddedCount) + '=' + RegisterCode + ', IsActive=' + EnableStr
    else begin
      RegistAccount := RS[4];
      AddStr :=  IntToStr(AddedCount) + '=' + RegisterCode + ', IsActive=' + EnableStr +', Account=' + RegistAccount;
    end;              
    RegistFile.Append(AddStr);      

    RS.next;   
  end;                                  

  RS := nil;
  OutRegistFile := RegistFile.Text;
  RegistFile.Free;

  Result := Length (OutRegistFile) > 0;
end;

function MakeRegistCodesFileEx (BatchCode: String; Count: Integer; ExpiredTime: TDateTime): String;
var
  RegistFile: TStringList;
  SqlCmd, RegisterCode, CreateTimeStr, ExpiredTimeStr: String;
  AddedCount: Integer;  
begin
  Result := '';
  if Count < 1 then exit;
  if BatchCode = '' then
    BatchCode := CreateClassID;

  CreateTimeStr := mkDateToStr(Now);
  ExpiredTimeStr := mkDateToStr(ExpiredTime);

  RegistFile := TStringList.Create;
  RegistFile.Append('[注册码批号]');
  RegistFile.Append('BatchCode=' + BatchCode);
  RegistFile.Append('');
  RegistFile.Append('[有效期]');
  RegistFile.Append('CreateTime=' + CreateTimeStr);
  RegistFile.Append('ExpiredTime=' + ExpiredTimeStr);
  RegistFile.Append('');
  RegistFile.Append('[注册码列表]');
        
  UserDB.beginTransaction;
  AddedCount := 0;
  repeat
    RegisterCode := CreateClassID;
    SqlCmd := 'INSERT OR IGNORE INTO RegisterCodes (IsActive,BatchCode,RegisterCode,CreateTime,ExpiredTime) ';
    SqlCmd := SqlCmd + 'VALUES (''True'',''' + BatchCode + ''',''' + RegisterCode + ''',''' + CreateTimeStr + ''',''' + ExpiredTimeStr + ''')';
    UserDB._exec (SqlCmd);
    Inc (AddedCount);
    RegistFile.Append(IntToStr(AddedCount) + '=' + RegisterCode);
  until AddedCount = Count;
  UserDB.Commit;
                  
  Result := RegistFile.Text;
  RegistFile.Free;
end;
      
function MakeRegistCodesFile (Count: Integer; NumberOfDays: Integer): String;
var
  BatchCode: String;
  ExpiredTime: TDateTime;
begin
  BatchCode := CreateClassID;
  ExpiredTime := IncDay (Now, NumberOfDays);    
  Result := MakeRegistCodesFileEx (BatchCode, Count, ExpiredTime);
end;

function RegistMachine (RegistAccount, RegisterCode: String): BOOL;
const
  SQLITE_OK  =  0;
var
  EnableStr, ExpiredTimeStr: String;
  ExpiredTime: TDateTime;
  SqlCmd: String;
  RS:IMksqlStmt;
  TimeCmp: TValueRelationship;
begin
  Result := False;

  SqlCmd := 'SELECT IsActive,ExpiredTime FROM RegisterCodes WHERE RegisterCode=''' + RegisterCode + '''';
  RS := UserDB.exec (SqlCmd);
  if RS.EOF then
  begin
    RS := nil;
    Log ('RegisterCode : ' + RegisterCode + ' is invalid');
    Exit;
  end;

  EnableStr := RS[0];
  ExpiredTimeStr:= RS[1];
  RS := nil;
  if EnableStr = 'False' then
  begin
    Log ('RegisterCode : ' + RegisterCode + ' IsActive=False');
    Exit;
  end;

  ExpiredTime := mkStrToDate (ExpiredTimeStr);
  TimeCmp := CompareDateTime (ExpiredTime, Now);

  if TimeCmp = LessThanValue then
  begin
    SqlCmd := 'UPDATE RegisterCodes SET IsActive=''False'',LastAccessTime=''' + mkDateToStr(Now) + '''';
    SqlCmd := SqlCmd + ' WHERE RegisterCode=''' + RegisterCode + '''';
    UserDB.execCmd(SqlCmd);
    LOG ('RegisterCode ' + RegisterCode + ' is Expired');
    LOG ('ExpiredTime:' + ExpiredTimeStr + '  Now:'+ mkDateToStr(Now));
    Exit;
  end;

  SqlCmd := 'UPDATE RegisterCodes SET IsActive=''False'','
                                   + 'RegistTime=''' + mkDateToStr(Now) + ''','
                                   + 'LastAccessTime=''' + mkDateToStr(Now) + ''','
                                   + 'RegistAccount=''' + RegistAccount  + '''';
  SqlCmd := SqlCmd + ' WHERE RegisterCode=''' + RegisterCode + '''';

  if SQLITE_OK <> UserDB._exec(SqlCmd) then
  begin
    LOG ('UPDATE DB Error:' + SqlCmd);
    Exit;
  end;

  Result := True;
end;
               
function GetFileName(hModule:THandle):string;
var
  Buffer:array[byte] of char;
begin
  ZeroMemory(@buffer[0], 256);
  GetModuleFileName(hModule, @Buffer, 256);
  result := buffer;
end;

function GetDatabaseName (FileName: String): String;
var
  MePath: String;
begin
  MePath := GetFileName (0);
  MePath := ExtractFilePath (MePath);
  MePath :=MePath + FileName;
  Result := AnsiToUtf8 (MePath);
end;

var
  BeenInitial: BOOL;

procedure Initial (DbFileName: String);
begin
  if BeenInitial then Exit;
  BeenInitial := True;

  UserDB := TMkSqlite.Create(nil);
  UserDB.dbName := GetDatabaseName (DbFileName);
  UserDB.open;
end;
              
procedure Finially;
begin
  UserDB.close;
  UserDB.Free;
end;

end.
