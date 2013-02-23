unit VerifyCodeDllInterface;

interface
uses windows, classes, sysutils;

function Srv_SetUser (InputParam: PChar): PChar; Stdcall;
function Srv_SetMachine (InputParam: PChar): PChar; Stdcall;
function Srv_DeleteUser (InputParam: PChar): PChar; Stdcall;
function Srv_DeleteMachine (InputParam: PChar): PChar; Stdcall;

function Srv_GetMachineMaxCount (InputParam: PChar): PChar; Stdcall;
function Srv_GetMachineList (InputParam: PChar): PChar; Stdcall;
function Srv_GetMachineDetail (InputParam: PChar): PChar; Stdcall;

//-----------------------------------------------------------------------------//

function Rpc_SetUser (UserName: String; MaxMachineCount: Integer): BOOL;
function Rpc_SetMachine (UserName: String; MachineInfo: String): BOOL;
function Rpc_DeleteUser (UserName: String): BOOL;
function Rpc_DeleteMachine (UserName, MachineName: String): BOOL;
function Rpc_GetMachineMaxCount (UserName: String; Out MaxMachineCount: Integer): BOOL;
function Rpc_GetMachineList (UserName: String; Out MachineList: TStringList): BOOL;
function Rpc_GetMachineDetail (UserName, MachineName: String; Out MachineDetail: TStringList): BOOL;


var
  Default_SecurityCode: String = 'Default_SecurityCode';

implementation

uses Base64Unit, TuojiCLientUnit, MkSqLite3;

function SetUser (UserName: PChar; MaxMachineCount: Integer): LongBOOL; Stdcall; External 'VerifyCode.dll';
function SetMachine (UserName: PChar; MachineInfo: PChar): LongBool; Stdcall; External 'VerifyCode.dll';
function DeleteUser (UserName: PChar): LongBool; Stdcall; External 'VerifyCode.dll';
function DeleteMachine (UserName, MachineName: PChar): LongBool; Stdcall; External 'VerifyCode.dll';

function GetMachineMaxCount (UserName: PChar; Out MaxMachineCount: Integer): BOOL; Stdcall; External 'VerifyCode.dll';
function GetMachineList (UserName: PChar; OutBuffer: PChar; MaxOutSize: Integer): Integer; Stdcall; External 'VerifyCode.dll';
function GetMachineDetail (UserName, MachineName: PChar; OutBuffer: PChar; MaxOutSize: Integer): Integer; Stdcall; External 'VerifyCode.dll';


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


//SecurityCode=aadasd33
//UserName=MyuserName
//MaxMachineCount=122
function Rpc_SetUser (UserName: String; MaxMachineCount: Integer): BOOL;
var
  TransSL : TStringList;
  SendParam, RunResult: String;
begin
  Result := False;
  TransSL := TStringList.Create;
  TransSL.Values['SecurityCode'] := Default_SecurityCode;
  TransSL.Values['UserName'] := UserName;
  TransSL.Values['MaxMachineCount'] := IntToStr (MaxMachineCount);

  SendParam := EncodeBase64 (TransSL.Text);
  TransSL.Free;

  TransSL := RPC_Internal ('SetUser', SendParam);
  if TransSL.Count = 2 then
  if TransSL[0] = 'OK' then
  begin
    RunResult := Trim (TransSL[1]);
    Result := RunResult = 'True';
  end;
  TransSL.Free;
end;

function Srv_SetUser (InputParam: PChar): PChar; Stdcall;
var
  SecurityCode, UserName, MaxMachineCountStr: String;
  MaxMachineCount: Integer;
  LoadSL: TStringList;
begin
  Result := 'ERR';

  LoadSL := GetParamList (InputParam);
  SecurityCode := LoadSL.Values['SecurityCode'];
  UserName     := LoadSL.Values['UserName'];
  MaxMachineCountStr := LoadSL.Values['MaxMachineCount'];
  LoadSL.Free;

  if not IsAccessValid (SecurityCode) then Exit;
  if not TryStrToInt (MaxMachineCountStr, MaxMachineCount) then Exit;

  if SetUser (PChar(UserName), MaxMachineCount) then
    Result := 'True'
  else
    Result := 'False';
end;


//SecurityCode=aadasd33
//UserName=MyuserName
//MachineInfo=base64string.....
function Rpc_SetMachine (UserName: String; MachineInfo: String): BOOL;
var
  TransSL : TStringList;
  SendParam, RunResult: String;
begin
  Result := False;
  MachineInfo := Trim (MachineInfo);

  TransSL := TStringList.Create;
  TransSL.Values['SecurityCode'] := Default_SecurityCode;
  TransSL.Values['UserName'] := UserName;
  TransSL.Values['MachineInfo'] := EncodeBase64 (MachineInfo);

  SendParam := EncodeBase64 (TransSL.Text);
  TransSL.Free;

  TransSL := RPC_Internal ('SetMachine', SendParam);
  if TransSL.Count = 2 then
  if TransSL[0] = 'OK' then
  begin
    RunResult := Trim (TransSL[1]);
    Result := RunResult = 'True';
  end;
  TransSL.Free;
end;

function Srv_SetMachine (InputParam: PChar): PChar; Stdcall;
var
  UserName, SecurityCode, MachineInfoStr: String;
  LoadSL: TStringList;
  Buffer: PChar;
begin
  Result := 'ERR';
  
  LoadSL := GetParamList (InputParam);
  SecurityCode := LoadSL.Values['SecurityCode'];
  UserName     := LoadSL.Values['UserName'];
  MachineInfoStr := LoadSL.Values['MachineInfo'];
  LoadSL.Free;

  if not IsAccessValid (SecurityCode) then Exit;

  Buffer := AllocMem ($1000);
  DecodeBase64 (MachineInfoStr, Buffer[0]);

  if SetMachine (PChar(UserName), Buffer) then
    Result := 'True'
  else
    Result := 'False';

  FreeMem (Buffer);
end;


function Rpc_DeleteUser (UserName: String): BOOL;
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

  TransSL := RPC_Internal ('DeleteUser', SendParam);
  if TransSL.Count = 2 then
  if TransSL[0] = 'OK' then
  begin
    RunResult := Trim (TransSL[1]);
    Result := RunResult = 'True';
  end;
  TransSL.Free;
end;


function Srv_DeleteUser (InputParam: PChar): PChar; Stdcall;
var
  UserName, SecurityCode: String;
  LoadSL: TStringList;
begin
  Result := 'ERR';

  LoadSL := GetParamList (InputParam);
  SecurityCode := LoadSL.Values['SecurityCode'];
  UserName     := LoadSL.Values['UserName'];
  LoadSL.Free;

  if not IsAccessValid (SecurityCode) then Exit;

  if DeleteUser (PChar(UserName)) then
    Result := 'True'
  else
    Result := 'False';
end;



function Rpc_DeleteMachine (UserName, MachineName: String): BOOL;
var
  TransSL : TStringList;
  SendParam, RunResult: String;
begin
  Result := False;

  TransSL := TStringList.Create;
  TransSL.Values['SecurityCode'] := Default_SecurityCode;
  TransSL.Values['UserName'] := UserName;
  TransSL.Values['MachineName'] := MachineName;

  SendParam := EncodeBase64 (TransSL.Text);
  TransSL.Free;

  TransSL := RPC_Internal ('DeleteMachine', SendParam);
  if TransSL.Count = 2 then
  if TransSL[0] = 'OK' then
  begin
    RunResult := Trim (TransSL[1]);
    Result := RunResult = 'True';
  end;
  TransSL.Free;
end;

function Srv_DeleteMachine (InputParam: PChar): PChar; Stdcall;
var
  UserName, SecurityCode, MachineName: String;
  LoadSL: TStringList;
begin
  Result := 'ERR';

  LoadSL := GetParamList (InputParam);
  SecurityCode := LoadSL.Values['SecurityCode'];
  UserName     := LoadSL.Values['UserName'];
  MachineName  := LoadSL.Values['MachineName'];
  LoadSL.Free;

  if not IsAccessValid (SecurityCode) then Exit;

  if DeleteMachine (PChar(UserName), PChar(MachineName)) then
    Result := 'True'
  else
    Result := 'False';
end;


function Rpc_GetMachineMaxCount (UserName: String; Out MaxMachineCount: Integer): BOOL;
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

  TransSL := RPC_Internal ('GetMachineMaxCount', SendParam);
  if TransSL.Count = 2 then
  if TransSL[0] = 'OK' then
  begin
    RunResult := Trim (TransSL[1]);
    Repeat
      if RunResult = 'ERR' then Break;
      if RunResult = 'False' then Break;
      Result := TryStrToInt (RunResult, MaxMachineCount);
    until True;    
  end;
  TransSL.Free;
end;

function Srv_GetMachineMaxCount (InputParam: PChar): PChar; Stdcall;
var
  UserName, SecurityCode, RunResult: String;
  MaxMachineCount: Integer;
  LoadSL: TStringList;
begin
  Result := 'ERR';

  LoadSL := GetParamList (InputParam);
  SecurityCode := LoadSL.Values['SecurityCode'];
  UserName     := LoadSL.Values['UserName'];
  LoadSL.Free;

  if not IsAccessValid (SecurityCode) then Exit;

  if GetMachineMaxCount (PChar(UserName), MaxMachineCount) then
  begin
    RunResult := IntToStr (MaxMachineCount);
    Result := PChar(RunResult);
  end else
    Result := 'False';
end;


function Rpc_GetMachineList (UserName: String; Out MachineList: TStringList): BOOL;
var
  TransSL : TStringList;
  SendParam, RunResult: String;
  OutBuffer: PChar;
  MaxOutSize: Integer;
begin
  Result := False;

  TransSL := TStringList.Create;
  TransSL.Values['SecurityCode'] := Default_SecurityCode;
  TransSL.Values['UserName'] := UserName;

  SendParam := EncodeBase64 (TransSL.Text);
  TransSL.Free;

  TransSL := RPC_Internal ('GetMachineList', SendParam);
  if TransSL.Count = 2 then
  if TransSL[0] = 'OK' then
  begin
    RunResult := Trim (TransSL[1]);
    Repeat
      if RunResult = 'ERR' then Break;
      if RunResult = 'False' then Break;

      MaxOutSize := $8000;
      OutBuffer := AllocMem (MaxOutSize);
      DecodeBase64 (RunResult, OutBuffer[0]);
      MachineList.Text := StrPas (OutBuffer);
      FreeMem (OutBuffer);

      Result := MachineList.Count > 0;
    until True;    
  end;
  TransSL.Free;
end;

function Srv_GetMachineList (InputParam: PChar): PChar; Stdcall;
var
  UserName, SecurityCode, RunResult: String;
  OutBuffer: PChar; MaxOutSize, GetSize: Integer;
  LoadSL: TStringList;
begin
  Result := 'ERR';

  LoadSL := GetParamList (InputParam);
  SecurityCode := LoadSL.Values['SecurityCode'];
  UserName     := LoadSL.Values['UserName'];
  LoadSL.Free;

  if not IsAccessValid (SecurityCode) then Exit;

  MaxOutSize := $8000;
  OutBuffer := AllocMem (MaxOutSize);

  GetSize := GetMachineList (PChar(UserName), OutBuffer, MaxOutSize);

  if GetSize > 0 then
  begin
    RunResult := EncodeBase64 (OutBuffer[0], GetSize);
    Result := PChar(RunResult);
  end else
    Result := 'False';

  FreeMem (OutBuffer);
end;



function Rpc_GetMachineDetail (UserName, MachineName: String; Out MachineDetail: TStringList): BOOL;
var
  TransSL : TStringList;
  SendParam, RunResult: String;
  OutBuffer: PChar;
  MaxOutSize: Integer;
begin
  Result := False;

  TransSL := TStringList.Create;
  TransSL.Values['SecurityCode'] := Default_SecurityCode;
  TransSL.Values['UserName'] := UserName;
  TransSL.Values['MachineName'] := MachineName;

  SendParam := EncodeBase64 (TransSL.Text);
  TransSL.Free;

  TransSL := RPC_Internal ('GetMachineDetail', SendParam);
  if TransSL.Count = 2 then
  if TransSL[0] = 'OK' then
  begin
    RunResult := Trim (TransSL[1]);
    Repeat
      if RunResult = 'ERR' then Break;
      if RunResult = 'False' then Break;

      MaxOutSize := $1000;
      OutBuffer := AllocMem (MaxOutSize);
      DecodeBase64 (RunResult, OutBuffer[0]);
      MachineDetail.Text := StrPas (OutBuffer);
      FreeMem (OutBuffer);

      Result := MachineDetail.Count > 0;
    until True;    
  end;
  TransSL.Free;
end;

function Srv_GetMachineDetail (InputParam: PChar): PChar; Stdcall;
var
  UserName, SecurityCode, RunResult, MachineName: String;
  OutBuffer: PChar; MaxOutSize, GetSize: Integer;
  LoadSL: TStringList;
begin
  Result := 'ERR';

  LoadSL := GetParamList (InputParam);
  SecurityCode := LoadSL.Values['SecurityCode'];
  UserName     := LoadSL.Values['UserName'];
  MachineName     := LoadSL.Values['MachineName'];
  LoadSL.Free;

  if not IsAccessValid (SecurityCode) then Exit;

  MaxOutSize := $1000;
  OutBuffer := AllocMem (MaxOutSize);

  GetSize := GetMachineDetail (PChar(UserName), PChar(MachineName), OutBuffer, MaxOutSize);

  if GetSize > 0 then
  begin
    RunResult := EncodeBase64 (OutBuffer[0], GetSize);
    Result := PChar(RunResult);
  end else
    Result := 'False';

  FreeMem (OutBuffer);
end;

end.
