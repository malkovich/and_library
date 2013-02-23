unit RegistCodeInterface;

interface
uses windows, classes, sysutils;


function Srv_ManageBatchCodes (InputParam: PChar): PChar; Stdcall;
function Srv_MakeRegistCodesFileEx (InputParam: PChar): PChar; Stdcall;
function Srv_MakeRegistCodesFile (InputParam: PChar): PChar; Stdcall;
function Srv_ManageRegistCodes (InputParam: PChar): PChar; Stdcall;


function Rpc_MakeRegistCodesFileEx (BatchCode, MakeCount: String; ExpiredTime: TDateTime; Out ResultFile: TStringList): BOOL;
function Rpc_MakeRegistCodesFile (MakeCount, NumberOfDays: String; Out ResultFile: TStringList): BOOL;
function Rpc_GetBatchCodeFile (BatchCode: String; Out ResultFile: TStringList): BOOL;
function Rpc_DeleteBatchCode (BatchCode: String): BOOL;
function Rpc_EnableBatchCode (BatchCode: String; Enable: BOOL): BOOL;

function Rpc_EmptyRegistCodes: BOOL;
function Rpc_DeleteUsedRegistCodes: BOOL;
function Rpc_DeleteUnActiveRegistCodes: BOOL;
function Rpc_DeleteExpiredRegistCodes: BOOL;

var
  Default_SecurityCode: String = 'Default_SecurityCode';

implementation                                      

uses Base64Unit, TuojiCLientUnit, MkSqLite3, RegistCodeUnit, LogHost;


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

function Rpc_MakeRegistCodesFile (MakeCount, NumberOfDays: String; Out ResultFile: TStringList): BOOL;
var
  TransSL : TStringList;
  SendParam, RunResult: String;
begin
  Result := False;

  TransSL := TStringList.Create;
  TransSL.Values['SecurityCode'] := Default_SecurityCode;
  TransSL.Values['MakeCount'] := MakeCount;
  TransSL.Values['NumberOfDays'] := NumberOfDays;

  SendParam := EncodeBase64 (TransSL.Text);
  TransSL.Free;

  TransSL := RPC_Internal ('MakeRegistCodesFile', SendParam);
  if TransSL.Count = 2 then
  if TransSL[0] = 'OK' then
  begin
    RunResult := Trim (TransSL[1]);
    Repeat
      if RunResult = 'ERR' then Break;
      if RunResult = 'False' then Break;

      RunResult := DecodeBase64 (RunResult);
      ResultFile.Text := RunResult;
      Result := True;      
    until True;
  end;
  TransSL.Free;
end;

function Srv_MakeRegistCodesFile (InputParam: PChar): PChar; Stdcall;
var
  SecurityCode, NumberOfDays, MakeCount, RunResult: String;
  nNumberOfDays, nMakeCount: Integer;
  LoadSL: TStringList;
begin
  Result := 'ERR';

  LoadSL := GetParamList (InputParam);
  SecurityCode := LoadSL.Values['SecurityCode'];
  MakeCount     := LoadSL.Values['MakeCount'];
  NumberOfDays     := LoadSL.Values['NumberOfDays'];
  LoadSL.Free;

  if not IsAccessValid (SecurityCode) then Exit;

  Result := 'False';
  if not TryStrToint (MakeCount, nMakeCount) then Exit;
  if not TryStrToint (NumberOfDays, nNumberOfDays) then Exit;

  RunResult := MakeRegistCodesFile (nMakeCount, nNumberOfDays);
  RunResult := EncodeBase64 (RunResult);
  Result := PChar (RunResult);
end;

function Rpc_MakeRegistCodesFileEx (BatchCode, MakeCount: String; ExpiredTime: TDateTime; Out ResultFile: TStringList): BOOL;
var
  TransSL : TStringList;
  SendParam, RunResult: String;
begin
  Result := False;

  TransSL := TStringList.Create;
  TransSL.Values['SecurityCode'] := Default_SecurityCode;
  TransSL.Values['BatchCode'] := BatchCode;
  TransSL.Values['MakeCount'] := MakeCount;
  TransSL.Values['ExpiredTime'] := mkDateToStr(ExpiredTime);

  SendParam := EncodeBase64 (TransSL.Text);
  TransSL.Free;

  TransSL := RPC_Internal ('MakeRegistCodesFileEx', SendParam);
  if TransSL.Count = 2 then
  if TransSL[0] = 'OK' then
  begin
    RunResult := Trim (TransSL[1]);
    Repeat
      if RunResult = 'ERR' then Break;
      if RunResult = 'False' then Break;

      RunResult := DecodeBase64 (RunResult);
      ResultFile.Text := RunResult;
      Result := True;      
    until True;
  end;
  TransSL.Free;
end;

function Srv_MakeRegistCodesFileEx (InputParam: PChar): PChar; Stdcall;
var
  SecurityCode, BatchCode, MakeCount, ExpiredTime, RunResult: String;
  nMakeCount: Integer;
  Expired: TDateTime;
  LoadSL: TStringList;
begin
  Result := 'ERR';

  LoadSL := GetParamList (InputParam);
  SecurityCode := LoadSL.Values['SecurityCode'];
  BatchCode     := LoadSL.Values['BatchCode'];
  MakeCount     := LoadSL.Values['MakeCount'];
  ExpiredTime   := LoadSL.Values['ExpiredTime'];
  Expired := mkStrToDate (ExpiredTime);
  LoadSL.Free;

  if not IsAccessValid (SecurityCode) then Exit;

  Result := 'False';
  if not TryStrToint (MakeCount, nMakeCount) then Exit;
  if BatchCode = '' then exit;

  RunResult := MakeRegistCodesFileEx (BatchCode, nMakeCount, Expired);
  RunResult := EncodeBase64 (RunResult);
  Result := PChar (RunResult);
end;

function Rpc_EnableBatchCode (BatchCode: String; Enable: BOOL): BOOL;
var
  TransSL : TStringList;
  SendParam, RunResult: String;
  EnableStr: String;
begin
  Result := False;
  if Enable then
    EnableStr := 'True'
  else
    EnableStr := 'False';

  TransSL := TStringList.Create;
  TransSL.Values['SecurityCode'] := Default_SecurityCode;
  TransSL.Values['BatchCode'] := BatchCode;
  TransSL.Values['Command'] := 'Enable';
  TransSL.Values['Parameter'] := EnableStr;
  SendParam := TransSL.Text;
  SendParam := EncodeBase64 (SendParam);
  TransSL.Free;

  TransSL := RPC_Internal ('ManageBatchCodes', SendParam);
  if TransSL.Count = 2 then
  if TransSL[0] = 'OK' then
  begin
    RunResult := Trim (TransSL[1]);
    Repeat
      if RunResult = 'ERR' then Break;
      if RunResult = 'False' then Break;
      Result := RunResult = 'True';
    until True;
  end;
  TransSL.Free;
end;

function Rpc_DeleteBatchCode (BatchCode: String): BOOL;
var
  TransSL : TStringList;
  SendParam, RunResult: String;
begin
  Result := False;

  TransSL := TStringList.Create;
  TransSL.Values['SecurityCode'] := Default_SecurityCode;
  TransSL.Values['BatchCode'] := BatchCode;
  TransSL.Values['Command'] := 'Delete';
  SendParam := TransSL.Text;
  SendParam := EncodeBase64 (SendParam);
  TransSL.Free;

  TransSL := RPC_Internal ('ManageBatchCodes', SendParam);
  if TransSL.Count = 2 then
  if TransSL[0] = 'OK' then
  begin
    RunResult := Trim (TransSL[1]);
    Repeat
      if RunResult = 'ERR' then Break;
      if RunResult = 'False' then Break;
      Result := RunResult = 'True';
    until True;
  end;
  TransSL.Free;
end;

function Rpc_GetBatchCodeFile (BatchCode: String; Out ResultFile: TStringList): BOOL;
var
  TransSL : TStringList;
  SendParam, RunResult: String;
begin
  Result := False;

  TransSL := TStringList.Create;
  TransSL.Values['SecurityCode'] := Default_SecurityCode;
  TransSL.Values['BatchCode'] := BatchCode;
  TransSL.Values['Command'] := 'GetFile';
  SendParam := TransSL.Text;
  SendParam := EncodeBase64 (SendParam);
  TransSL.Free;

  TransSL := RPC_Internal ('ManageBatchCodes', SendParam);
  if TransSL.Count = 2 then
  if TransSL[0] = 'OK' then
  begin
    RunResult := Trim (TransSL[1]);
    Repeat
      if RunResult = 'ERR' then Break;
      if RunResult = 'False' then Break;
      RunResult := DecodeBase64 (RunResult);
      ResultFile.Text := RunResult;
      Result := ResultFile.Count > 8;
    until True;
  end;
  TransSL.Free;
end;


function Srv_ManageBatchCodes (InputParam: PChar): PChar; Stdcall;
var
  Enable: BOOL;
  CmdStr, Parameter: String;
  SecurityCode, BatchCode, RegistFile: String;
  LoadSL: TStringList;
begin
  Result := 'ERR';

  LoadSL       := GetParamList (InputParam);
  SecurityCode := LoadSL.Values['SecurityCode'];
  BatchCode    := LoadSL.Values['BatchCode'];
  CmdStr       := LoadSL.Values['Command'];
  Parameter    := LoadSL.Values['Parameter'];
  LoadSL.Free;

  if not IsAccessValid (SecurityCode) then Exit;

  Result := 'False';

  if CmdStr = 'Enable' then
  begin
    Enable := (Parameter = 'True');
    if EnableBatchCode (BatchCode, Enable) then
      Result := 'True';
    Exit;
  end;

  if CmdStr = 'Delete' then
  begin
    if DeleteBatchCode (BatchCode) then
      Result := 'True';
    Exit;
  end;

  if CmdStr = 'GetFile' then
  begin
    if GetBatchCodeFile (BatchCode, RegistFile) then
    begin
      RegistFile := EncodeBase64 (RegistFile);
      Result := PChar(RegistFile);
    end;
  end;

end;

function Rpc_ExecRegistCodes (CmdType: String): BOOL;
var
  TransSL : TStringList;
  SendParam, RunResult: String;
begin
  Result := False;

  TransSL := TStringList.Create;
  TransSL.Values['SecurityCode'] := Default_SecurityCode;
  TransSL.Values['Command'] := CmdType;
  SendParam := TransSL.Text;
  SendParam := EncodeBase64 (SendParam);
  TransSL.Free;

  TransSL := RPC_Internal ('ManageRegistCodes', SendParam);
  if TransSL.Count = 2 then
  if TransSL[0] = 'OK' then
  begin
    RunResult := Trim (TransSL[1]);
    Repeat
      if RunResult = 'ERR' then Break;
      if RunResult = 'False' then Break;
      Result := RunResult = 'True';
    until True;
  end;
  TransSL.Free;
end;

function Rpc_DeleteExpiredRegistCodes: BOOL;
begin
  Result := Rpc_ExecRegistCodes ('DeleteExpiredRegistCodes');
end;

function Rpc_DeleteUnActiveRegistCodes: BOOL;
begin
  Result := Rpc_ExecRegistCodes ('DeleteUnActiveRegistCodes');
end;

function Rpc_DeleteUsedRegistCodes: BOOL;
begin
  Result := Rpc_ExecRegistCodes ('DeleteUsedRegistCodes');
end;

function Rpc_EmptyRegistCodes: BOOL;
begin
  Result := Rpc_ExecRegistCodes ('EmptyRegistCodes');
end;


function Srv_ManageRegistCodes (InputParam: PChar): PChar; Stdcall;
var
  CmdStr: String;
  SecurityCode: String;
  LoadSL: TStringList;
begin
  Result := 'ERR';

  LoadSL       := GetParamList (InputParam);
  SecurityCode := LoadSL.Values['SecurityCode'];
  CmdStr       := LoadSL.Values['Command'];
  LoadSL.Free;

  if not IsAccessValid (SecurityCode) then Exit;

  Result := 'False';

  if CmdStr = 'DeleteExpiredRegistCodes' then
  begin
    if DeleteExpiredRegistCodes then
      Result := 'True';
    Exit;
  end;

  if CmdStr = 'DeleteUnActiveRegistCodes' then
  begin
    if DeleteUnActiveRegistCodes then
      Result := 'True';
    Exit;
  end;

  if CmdStr = 'DeleteUsedRegistCodes' then
  begin
    if DeleteUsedRegistCodes then
      Result := 'True';
    Exit;
  end;

  if CmdStr = 'EmptyRegistCodes' then
  begin
    if EmptyRegistCodes then
      Result := 'True';
    Exit;
  end;

end;


end.
