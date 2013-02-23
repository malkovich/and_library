program ExtendExpired;

{$APPTYPE CONSOLE}

uses
  SysUtils, MKSqlite3;

var
  UserDB: TMkSqlite;


function GetDatabaseName (FileName: String): String;
var
  MePath: String;
begin
  MePath := GetModuleName (0);
  MePath := ExtractFilePath (MePath);
  MePath :=MePath + FileName;
  Result := AnsiToUtf8 (MePath);
end;

procedure Initial (DbFileName: String);
begin
  UserDB := TMkSqlite.Create(nil);
  UserDB.dbName := GetDatabaseName (DbFileName);
  UserDB.open;
end;

procedure Finially;
begin
  UserDB.close;
  UserDB.Free;
end;

Procedure SetUserExpiredTime (UserName: String; ExpendDays: Integer);
var
  SqlCmd: String;
  RS:IMksqlStmt;
  ExpiredTime: LongWord;
begin
  SqlCmd := 'SELECT ExpiredTime FROM user WHERE Account=''' + UserName + '''';
  RS := UserDB.exec(SqlCmd);
  if RS.eof then
  begin
    RS := nil;
    Exit;
  end;
  ExpiredTime := RS[0];
  RS := nil;
  Inc (ExpiredTime, ExpendDays*24*3600);
  
  SqlCmd := 'UPDATE user SET ExpiredTime=' + IntToStr(ExpiredTime)
                         + ' WHERE Account=''' + UserName + '''';
  UserDB.execCmd(SqlCmd);
end;

var
  ExpiredDays: Integer;
  UserName: String; 
begin
  Initial ('DB.s3db');

  if ParamCount = 2 then
  if TryStrToInt (ParamStr(2), ExpiredDays) then
  begin
    UserName := ParamStr(1);
    SetUserExpiredTime (UserName, ExpiredDays);
  end;
  Finially;
end.
