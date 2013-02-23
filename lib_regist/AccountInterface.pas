unit AccountInterface;

interface
uses MKSqlite3, windows, classes, SysUtils;

procedure Initial (DbFileName: String);
procedure Finially;

function Srv_SetAccount (InputParam: PChar): PChar; Stdcall;
function Srv_GetAccount (InputParam: PChar): PChar; Stdcall;
function Srv_DeleteAccount (InputParam: PChar): PChar; Stdcall;
function Srv_GetAccountList (InputParam: PChar): PChar; Stdcall;


function Rpc_SetAccount (UserName, Password, Flags, ExpiredTime, MaxProcesses, MaxOnline: String): BOOL;
function Rpc_GetAccount (Const UserName: String; Out Password,Flags,ExpiredTime,MaxProcesses,MaxOnline: String): BOOL;
function Rpc_DeleteAccount (UserName: String): BOOL;
function Rpc_GetAccountList (Out AccountList: TStringList): BOOL;


var
  Default_SecurityCode: String = 'Default_SecurityCode';


implementation

uses
  Base64Unit, TuojiCLientUnit;

var
  UserDB: TMkSqlite;

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

///////////////////////////////////////////////

function IsAccessValid (SecurityCode: String): BOOL;
begin
  Result := SecurityCode = Default_SecurityCode;
end;

function GetParamList (InputParam: PChar): TStringList;
var
  InputBase64: String;
begin
  InputBase64 := Strpas (InputParam);
  InputBase64 := DecodeBase64 (InputBase64);
  Result := TStringList.Create;
  Result.Text := InputBase64;
end;                   


function Rpc_SetAccount (UserName, Password, Flags, ExpiredTime, MaxProcesses, MaxOnline: String): BOOL;
var
  TransSL : TStringList;
  SendParam, RunResult: String;
begin
  Result := False;
  TransSL := TStringList.Create;
  TransSL.Values['SecurityCode'] := Default_SecurityCode;
  TransSL.Values['UserName'] := UserName;
  TransSL.Values['Password'] := Password;
  TransSL.Values['Flags'] := Flags;
  TransSL.Values['ExpiredTime'] := ExpiredTime;
  TransSL.Values['MaxProcesses'] := MaxProcesses;
  TransSL.Values['MaxOnline'] := MaxOnline;

  SendParam := EncodeBase64 (TransSL.Text);
  TransSL.Free;

  TransSL := RPC_Internal ('SetAccount', SendParam);
  if TransSL.Count = 2 then
  if TransSL[0] = 'OK' then
  begin
    RunResult := Trim (TransSL[1]);
    Result := RunResult = 'True';
  end;
  TransSL.Free;
end;

//SecurityCode=aadasd33
//UserName=MyuserName
//Password=1234dfdsf2
//Flags=2
//ExpiredTime=34324322
//MaxProcesses=1000
//MaxOnline=10000
function Srv_SetAccount (InputParam: PChar): PChar; Stdcall;
var
  LoadSL: TStringList;
  SecurityCode, UserName, Password, Flags, ExpiredTime, MaxProcesses, MaxOnline: String;
  SqlCmd: String;
  RS:IMksqlStmt;
begin
  Result := 'ERR';
  
  LoadSL := GetParamList (InputParam);
  SecurityCode := LoadSL.Values['SecurityCode'];
  UserName     := LoadSL.Values['UserName'];
  Password := LoadSL.Values['Password'];
  Flags := LoadSL.Values['Flags'];
  ExpiredTime := LoadSL.Values['ExpiredTime'];
  MaxProcesses := LoadSL.Values['MaxProcesses'];
  MaxOnline := LoadSL.Values['MaxOnline'];
  LoadSL.Free;

  if not IsAccessValid (SecurityCode) then Exit;

  SqlCmd := 'SELECT Password FROM user WHERE Account=''' + UserName + '''';
  RS := UserDB.exec(SqlCmd);
  if RS.EOF then
  begin
    SqlCmd := 'INSERT INTO user (Account,Password,Flags,ExpiredTime,Session,Role) ';
    SqlCmd := SqlCmd + 'VALUES('''  + UserName + ''','''
                                  + Password + ''','
                                  + Flags + ','
                                  + ExpiredTime + ','
                                  + MaxProcesses + ','
                                  + MaxOnline + ')';
    UserDB.exec(SqlCmd);
  end else
  begin
    SqlCmd := 'UPDATE user SET Password=''' + Password + ''','
                           + 'Flags=' + Flags + ','
                           + 'ExpiredTime=' + ExpiredTime + ','
                           + 'Session=' + MaxProcesses + ','
                           + 'Role=' + MaxOnline
                           + ' WHERE Account=''' + UserName + '''';
    UserDB.execCmd(SqlCmd);
  end;

  Result := 'True';
end;


function Rpc_GetAccount (Const UserName: String; Out Password,Flags,ExpiredTime,MaxProcesses,MaxOnline: String): BOOL;
var
  TransSL : TStringList;
  SendParam, RunResult: String;
begin
  Result := False;
  TransSL := TStringList.Create;
  TransSL.Values['SecurityCode'] := Default_SecurityCode;
  TransSL.Values['UserName'] := UserName;
  SendParam := EncodeBase64 (TransSL.Text);
  TransSL.Free;

  TransSL := RPC_Internal ('GetAccount', SendParam);
  if TransSL.Count = 2 then
  if TransSL[0] = 'OK' then
  begin
    RunResult := Trim (TransSL[1]);
    RunResult := DecodeBase64 (RunResult);
    TransSL.Clear;
    TransSL.Text := RunResult;

    Password := TransSL.Values['Password'];
    Flags := TransSL.Values['Flags'];
    ExpiredTime := TransSL.Values['ExpiredTime'];
    MaxProcesses := TransSL.Values['MaxProcesses'];
    MaxOnline := TransSL.Values['MaxOnline'];
    Result := True;
  end;
  TransSL.Free;
end;

//>>>SecurityCode=aadasd33
//>>>UserName=MyuserName

//<<<UserName=MyuserName
//<<<Password=1234dfdsf2
//<<<Flags=2
//<<<ExpiredTime=34324322
//<<<MaxProcesses=1000
//<<<MaxOnline=10000
function Srv_GetAccount (InputParam: PChar): PChar; Stdcall;
var
  LoadSL: TStringList;
  SecurityCode, UserName, Password, Flags, ExpiredTime, MaxProcesses, MaxOnline: String;
  SqlCmd, RunResult: String;
  RS:IMksqlStmt;
begin
  Result := 'ERR';
  
  LoadSL := GetParamList (InputParam);
  SecurityCode := LoadSL.Values['SecurityCode'];
  UserName     := LoadSL.Values['UserName'];
  LoadSL.Clear;

  if not IsAccessValid (SecurityCode) then Exit;

  SqlCmd := 'SELECT Password,Flags,ExpiredTime,Session,Role FROM user ' +
            'WHERE Account=''' + UserName + '''';
  RS := UserDB.exec(SqlCmd);

  Result := 'False';
  if not RS.EOF then
  begin
    Password := RS[0];
    Flags := RS[1];
    ExpiredTime := RS[2];
    MaxProcesses := RS[3];
    MaxOnline := RS[4];

    LoadSL.Values['UserName'] := UserName;
    LoadSL.Values['Password'] := Password;
    LoadSL.Values['Flags'] := Flags;
    LoadSL.Values['ExpiredTime'] := ExpiredTime;
    LoadSL.Values['MaxProcesses'] := MaxProcesses;
    LoadSL.Values['MaxOnline'] := MaxOnline;
    RunResult := EncodeBase64 (LoadSL.Text);

    Result := PChar(RunResult);
  end;
  LoadSL.Free;  
end;


function Rpc_DeleteAccount (UserName: String): BOOL;
var
  TransSL : TStringList;
  SendParam, RunResult: String;
begin
  Result := False;
  TransSL := TStringList.Create;
  TransSL.Values['SecurityCode'] := Default_SecurityCode;
  TransSL.Values['UserName'] := UserName;
  SendParam := EncodeBase64 (TransSL.Text);
  TransSL.Free;

  TransSL := RPC_Internal ('DeleteAccount', SendParam);
  if TransSL.Count = 2 then
  if TransSL[0] = 'OK' then
  begin
    RunResult := Trim (TransSL[1]);
    Result := RunResult = 'True';
  end;
  TransSL.Free;
end;

//SecurityCode=aadasd33
//UserName=MyuserName
function Srv_DeleteAccount (InputParam: PChar): PChar; Stdcall;
var
  LoadSL: TStringList;
  SecurityCode, UserName: String;
  SqlCmd: String;
begin
  Result := 'ERR';

  LoadSL := GetParamList (InputParam);
  SecurityCode := LoadSL.Values['SecurityCode'];
  UserName     := LoadSL.Values['UserName'];
  LoadSL.Free;

  if not IsAccessValid (SecurityCode) then Exit;

  SqlCmd := 'DELETE FROM user WHERE Account=''' + UserName + '''';
  UserDB.execCmd (SqlCmd);
  Result := 'True';
end;

function Rpc_GetAccountList (Out AccountList: TStringList): BOOL;
var
  TransSL : TStringList;
  RunResult: String;
begin
  Result := False;
  TransSL := RPC_Internal ('GetAccountList', Default_SecurityCode);
  if TransSL.Count = 2 then
  if TransSL[0] = 'OK' then
  begin
    RunResult := Trim (TransSL[1]);
    Repeat
      if RunResult = 'ERR' then Break;
      if RunResult = 'False' then Break;
      RunResult := DecodeBase64 (RunResult);
      AccountList.Text := RunResult;   
      Result := AccountList.Count > 0;
    until True;      
  end;
  TransSL.Free;
end;

//SecurityCode=aadasd33
function Srv_GetAccountList (InputParam: PChar): PChar; Stdcall;
var
  TransSL: TStringList;
  SecurityCode, UserName: String;
  SqlCmd, RunResult: String;
  RS:IMksqlStmt;
begin
  Result := 'ERR';
  SecurityCode := StrPas (InputParam);
  if not IsAccessValid (SecurityCode) then Exit;

  SqlCmd := 'SELECT Account FROM user';
  RS := UserDB.exec(SqlCmd);

  TransSL := TStringList.Create;
  While not RS.EOF do
  begin
    UserName := Trim (RS[0]);
    TransSL.Add(UserName);
    RS.next;
  end;

  if TransSL.Count = 0 then
  begin
    Result := 'False';
    Exit;
  end;

  RunResult := EncodeBase64 (Trim(TransSL.Text));
  TransSL.Free;
  Result := PChar(RunResult);
end;



end.

