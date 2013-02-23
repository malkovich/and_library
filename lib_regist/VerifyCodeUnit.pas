unit VerifyCodeUnit;

interface
uses
  SysUtils, CodeEncrypt, SyncObjs, base64unit, MkSqLite3, Windows, IniFiles, RC4Extractor, Classes,
  DateUtils, Types, ComObj;

procedure Initial (DbFileName: String);
procedure Finially;

Function GetVerifyCode (CodeID, OutCodeBase64: PPChar): LongBool; Stdcall;
function IsMachineValid (UserName: PChar; CodeID, CodeResult: PChar; OutMachineID: PInteger): LongBool; stdcall;

function SetUser (UserName: PChar; MaxMachineCount: Integer): LongBOOL; Stdcall;
function SetMachine (UserName: PChar; MachineInfo: PChar): LongBool; Stdcall;
function DeleteUser (UserName: PChar): LongBool; Stdcall;
function DeleteMachine (UserName, MachineName: PChar): LongBool; Stdcall;

function GetMachineMaxCount (UserName: PChar; Out MaxMachineCount: Integer): BOOL; Stdcall;
function GetMachineList (UserName: PChar; OutBuffer: PChar; MaxOutSize: Integer): Integer; Stdcall;
function GetMachineDetail (UserName, MachineName: PChar; OutBuffer: PChar; MaxOutSize: Integer): Integer; Stdcall;


implementation

uses LogHost;

const
  SQLITE_OK         =  0;

function GetModuleCopy: TMemoryStream;
var
  ResMM: TResourceStream;
begin
  ResMM := TResourceStream.create(hinstance, 'VERIFY_DLL', 'PE_FILE');
  ResMM.Seek(0, soFromBeginning);

  Result := TmemoryStream.Create;
  Result.Seek(0, soFromBeginning);
  Result.LoadFromStream(ResMM);
  Result.Seek(0, soFromBeginning);
  ResMM.Free;
end;

var
  VerifyList: THashedStringList;
  VerifyListLock: TCriticalSection;
  TID: DWORD;
  ThreadHandle: THandle;
  UserDB: TMkSqlite;

Type
  LPTVerifyHashBlock = ^TVerifyHashBlock;
  TVerifyHashBlock = record
    CodeID: String;
    CodeString: String;
    CodeResult: String;
    RC4Content: TRC4Context;
    DestroyTime: DWORD;
  end;

Function GetVerifyCode (CodeID, OutCodeBase64: PPChar): LongBool; Stdcall;
var
  ModMM: TMemoryStream;
  GuidKey, ItemGuid: TGuidStr;
  VerifyHashBlock: LPTVerifyHashBlock;
  Index: Integer;
begin
  Repeat
    ItemGuid := GetRandomGUID;
    Index := VerifyList.IndexOf(ItemGuid);
  until Index = -1;

  New (VerifyHashBlock);
  VerifyHashBlock.CodeID := Trim (ItemGuid);

  ModMM := GetModuleCopy;
  GuidKey := GetRandomGUID;
  VerifyHashBlock.CodeString := DynamicCodeEncrypt (ModMM, GuidKey);
  ModMM.Free;

  RC4Init (VerifyHashBlock.RC4Content, GuidKey);
  VerifyHashBlock.DestroyTime := GetTickCount + 1000*60;

  CodeID^ := PChar(VerifyHashBlock.CodeID);
  OutCodeBase64^ := PChar(VerifyHashBlock.CodeString);

  VerifyListLock.Enter;
  Result := VerifyList.AddObject(VerifyHashBlock.CodeID, Pointer(VerifyHashBlock)) >= 0;
  VerifyListLock.Leave;
end;

{$I ..\lib\lib_buildlib\MiniDLL\EncryptFunction.inc}

Function DecryptCodeResult (CodeID, CodeResult: PChar; RealResult: PPChar; ResultSize: PInteger): BOOL; Stdcall;
var
  Index, DecryptSize: Integer;
  VerifyHashBlock: LPTVerifyHashBlock;
  DecryptBuffer: PChar;
  CodeIdStr, CodeResultStr: String;
begin
  Result := False;
  CodeIdStr := Trim(StrPas(CodeID));
  CodeResultStr := Trim(StrPas(CodeResult));

  Index := VerifyList.IndexOf(CodeIdStr);
  if Index = -1 then
  begin
    LOG ('DecryptCodeResult : The CodeID is not valid, may be it''s timeout');
    LOG ('CodeID : ' + CodeIdStr);
    Exit;
  end;
  VerifyHashBlock := Pointer (VerifyList.Objects[Index]);

  Result := True;
  Try
    DecryptBuffer := GetDecryptMSG (VerifyHashBlock.RC4Content, PChar(CodeResultStr), DecryptSize);
    SetLength (VerifyHashBlock.CodeResult, DecryptSize + 1);
    CopyMemory (@VerifyHashBlock.CodeResult[1], DecryptBuffer, DecryptSize + 1);
    FreeMem (DecryptBuffer);

    RealResult^ := @VerifyHashBlock.CodeResult[1];
    ResultSize^ := DecryptSize;
    VerifyHashBlock.DestroyTime := GetTickCount + 1000*30;
  Except
    Result := False;
  end;
end;

Procedure DeleteVerifyCode (Index: Integer); Stdcall;
var
  VerifyHashBlock: LPTVerifyHashBlock;
begin
  if Index >= 0 then
  begin
    VerifyHashBlock := Pointer (VerifyList.Objects[Index]);
    VerifyList.Delete(Index);
    Dispose (VerifyHashBlock);
  end;
end;

Procedure FreeVerifyCode (CodeID: PChar); Stdcall;
var
  Index: Integer;
begin
  VerifyListLock.Enter;

  Index := VerifyList.IndexOf(StrPas(CodeID));
  DeleteVerifyCode (Index);

  VerifyListLock.Leave;
end;

function ModuleDaemonThread (Param: Pointer): Integer; stdcall;
var
  Index: Integer;
  VerifyHashBlock: LPTVerifyHashBlock;
begin
  while True do
  begin
    Sleep (10*1000);
    VerifyListLock.Enter;

    for Index := VerifyList.Count - 1 downto 0 do
    begin
      VerifyHashBlock := Pointer (VerifyList.Objects[Index]);
      if GetTickCount > VerifyHashBlock.DestroyTime then
        DeleteVerifyCode (Index);
    end;

    VerifyListLock.Leave;
  end;
end;

function GetMachineList (UserName: PChar; OutBuffer: PChar; MaxOutSize: Integer): Integer; Stdcall;
var
  SqlCmd, MachineName, RunResult: String;
  RS:IMksqlStmt;
  UserID: Integer;
begin
  Result := 0;
  SqlCmd := 'SELECT UserID FROM UserDetail WHERE UserName=''' + StrPas(UserName) + '''';
  RS := UserDB.exec (SqlCmd);
  if RS.EOF then
  begin
    RS := nil;
    Exit;
  end;
  UserID := RS[0];
  RS := nil;

  SqlCmd := 'SELECT MachineName FROM MachineDetail WHERE UserID=' + IntToStr(UserID);
  RS := UserDB.exec (SqlCmd);

  RunResult := '';
  While not RS.EOF do
  begin
    MachineName := RS[0];
    RunResult := RunResult + MachineName + #13#10;
    RS.next;
  end;
  RS := nil;
  RunResult := Trim (RunResult);

  if RunResult <> '' then
  begin
    Result := Length(RunResult);
    if MaxOutSize - Result <= 0 then
    begin
      Result := MaxOutSize - Result - 1;
      Exit;
    end;
    StrCopy (OutBuffer, PChar(RunResult));
    OutBuffer[Result] := #0;
  end;
end;

function GetMachineDetail (UserName, MachineName: PChar; OutBuffer: PChar; MaxOutSize: Integer): Integer; Stdcall;
var
  SqlCmd, RunResult: String;
  RS:IMksqlStmt;
  UserID: Integer;
  OutPutSL: TStringList;
begin
  Result := 0;
  SqlCmd := 'SELECT UserID FROM UserDetail WHERE UserName=''' + StrPas(UserName) + '''';
  RS := UserDB.exec (SqlCmd);
  if RS.EOF then
  begin
    RS := nil;
    Exit;
  end;
  UserID := RS[0];
  RS := nil;

  SqlCmd := 'SELECT BaseBoard,SMBiosVersion,BiosName,BIOSSerialNumber,FirstDiskInfo,ProcessorInfo,PhysicalMemoryInfo,OperatingSystem,VideoCardName,SoundCardName FROM MachineDetail WHERE MachineName=''' + MachineName + ''' AND UserID=' + IntToStr(UserID);
  RS := UserDB.exec (SqlCmd);
  if RS.EOF then
  begin
    RS := nil;
    Exit;
  end;
  
  OutPutSL:= TStringList.Create;
  OutPutSL.Values['UserName'] := StrPas(UserName);
  OutPutSL.Values['MachineName'] := StrPas(MachineName);
  OutPutSL.Values['BaseBoard'] := RS[0];
  OutPutSL.Values['SMBiosVersion'] := RS[1];
  OutPutSL.Values['BiosName'] := RS[2];
  OutPutSL.Values['BIOSSerialNumber'] := RS[3];
  OutPutSL.Values['FirstDiskInfo'] := RS[4];
  OutPutSL.Values['ProcessorInfo'] := RS[5];
  OutPutSL.Values['PhysicalMemoryInfo'] := RS[6];
  OutPutSL.Values['OperatingSystem'] := RS[7];
  OutPutSL.Values['VideoCardName'] := RS[8];
  OutPutSL.Values['SoundCardName'] := RS[9];
  RunResult := Trim(OutPutSL.Text);
  OutPutSL.free;

  RS := nil;

  if RunResult <> '' then
  begin
    Result := Length(RunResult);
    if MaxOutSize - Result <= 0 then
    begin
      Result := MaxOutSize - Result - 1;
      Exit;
    end;
    StrCopy (OutBuffer, PChar(RunResult));
    OutBuffer[Result] := #0;
  end;
end;

function DeleteMachine (UserName, MachineName: PChar): LongBool; Stdcall;
var
  SqlCmd: String;
  UserID: Integer;
  RS:IMksqlStmt;
begin
  Result := False;
  SqlCmd := 'SELECT UserID FROM UserDetail WHERE UserName=''' + StrPas(UserName) + '''';
  RS := UserDB.exec (SqlCmd);
  if RS.EOF then
  begin
    RS := nil;
    Exit;
  end;
  UserID := RS[0];
  RS := nil;
    
  SqlCmd := 'DELETE FROM MachineDetail WHERE MachineName=''' + MachineName + ''' AND UserID=' + IntToStr(UserID);
  UserDB._exec (SqlCmd);
  Result := True;
end;

function DeleteUser (UserName: PChar): LongBool; Stdcall;
var
  SqlCmd: String;
  UserID: Integer;
  RS:IMksqlStmt;
begin
  Result := False;
  SqlCmd := 'SELECT UserID FROM UserDetail WHERE UserName=''' + StrPas(UserName) + '''';
  RS := UserDB.exec (SqlCmd);
  if RS.EOF then
  begin
    RS := nil;
    Exit;
  end;
  UserID := RS[0];
  RS := nil;

  SqlCmd := 'DELETE FROM UserDetail WHERE UserID=' + IntToStr(UserID);
  UserDB._exec (SqlCmd);
  SqlCmd := 'DELETE FROM MachineDetail WHERE UserID=' + IntToStr(UserID);
  UserDB._exec (SqlCmd);
  Result := True;
end;

//UserName=aadeseXX
//MachineName=神州3服8号机
//BaseBoard=ASUSTek Computer INC. PC-DL
//SMBiosVersion=ASUS PC-DL ACPI BIOS Revision 1009 v2.3
//BiosName=Phoenix - AwardBIOS v6.00PG
//BIOSSerialNumber=NoSerialNumber
//FirstDiskInfo=IDE Model: ST3250823AS, Serial: 4ND01LC0, Size = 137,438,952,960
//ProcessorInfo=Intel(R) Xeon(TM) CPU 2.00GHz=BFEBFBFF00000F29,Intel(R) Xeon(TM) CPU 2.00GHz=BFEBFBFF00000F29,Intel(R) Xeon(TM) CPU 2.00GHz=BFEBFBFF00000F29,Intel(R) Xeon(TM) CPU 2.00GHz=BFEBFBFF00000F29
//PhysicalMemoryInfo=1073741824,1073741824,1073741824,1073741824
//OperatingSystem=Microsoft Windows Server 2003 Enterprise Edition|C:\WINDOWS|\Device\Harddisk0\Partition1
//VideoCardName=NVIDIA GeForce 6800
//SoundCardName=SoundMAX Integrated Digital Audio              

function SetMachine (UserName: PChar; MachineInfo: PChar): LongBool; Stdcall;
var
  RegistStr, SqlCmd, UserNameStr, MachineNameDB: String;
  MachineName,BaseBoard,SMBiosVersion,BiosName,BIOSSerialNumber,FirstDiskInfo,
  ProcessorInfo,PhysicalMemoryInfo,OperatingSystem,VideoCardName,SoundCardName: String;
  RegistStrSL: TStringList;
  RS:IMksqlStmt;
  UserID,MaxMachineCount: Integer;
  IsUpdate: BOOL;
begin
  Result := False;
  UserNameStr := UserName;
  RegistStr := StrPas (MachineInfo);
  RegistStrSL:= TStringList.Create;
  RegistStrSL.Text := RegistStr;
  MachineName := Trim (RegistStrSL.Values['MachineName']);
  BaseBoard := Trim (RegistStrSL.Values['BaseBoard']);
  SMBiosVersion := Trim (RegistStrSL.Values['SMBiosVersion']);
  BiosName := Trim (RegistStrSL.Values['BiosName']);
  BIOSSerialNumber := Trim (RegistStrSL.Values['BIOSSerialNumber']);
  FirstDiskInfo := Trim (RegistStrSL.Values['FirstDiskInfo']);
  ProcessorInfo := Trim (RegistStrSL.Values['ProcessorInfo']);
  PhysicalMemoryInfo := Trim (RegistStrSL.Values['PhysicalMemoryInfo']);
  OperatingSystem := Trim (RegistStrSL.Values['OperatingSystem']);
  VideoCardName := Trim (RegistStrSL.Values['VideoCardName']);
  SoundCardName := Trim (RegistStrSL.Values['SoundCardName']);
  RegistStrSL.Free;

  if MachineName = '' then Exit;
  if BaseBoard = '' then Exit;
  if SMBiosVersion = '' then Exit;
  if BiosName = '' then Exit;
  if BIOSSerialNumber = '' then Exit;
  if FirstDiskInfo = '' then Exit;
  if ProcessorInfo = '' then Exit;
  if PhysicalMemoryInfo = '' then Exit;
  if OperatingSystem = '' then Exit;
  if VideoCardName = '' then Exit;
  if SoundCardName = '' then Exit;

  //验证用户是否合法
  SqlCmd := 'SELECT UserID,MaxMachineCount FROM UserDetail WHERE UserName=''' + UserNameStr + '''';
  RS := UserDB.exec (SqlCmd);
  if RS.EOF then
  begin
    RS := nil;
    Exit;
  end;
  UserID := RS[0];
  MaxMachineCount := RS[1];
  RS := nil;

  //清除表中相同的机器信息
  SqlCmd := 'DELETE FROM MachineDetail WHERE ProcessorInfo=''' + ProcessorInfo + ''' AND FirstDiskInfo=''' + FirstDiskInfo + '''';
  UserDB.execCmd(SqlCmd);

  //取出用户关键信息
  SqlCmd := 'SELECT MachineName,FirstDiskInfo,ProcessorInfo FROM MachineDetail WHERE UserID=' + IntToStr(UserID);
  RS := UserDB.exec (SqlCmd);
  IsUpdate := False;
  While not RS.EOF do
  begin
    MachineNameDB := RS[0];
    if MachineNameDB = MachineName then
    begin
      IsUpdate := True;
      Break;
    end;
    RS.next;
  end;

  //如果超出最大机器数，则添加失败
  if not IsUpdate then
    if RS.rowCount >= MaxMachineCount then
    begin
      RS := nil;
      Exit;
    end;
  //执行操作
  if IsUpdate then
  begin
    SqlCmd := 'UPDATE MachineDetail SET BaseBoard=''' + BaseBoard + ''','
                                     + 'SMBiosVersion=''' + SMBiosVersion + ''','
                                     + 'BiosName=''' + BiosName + ''','
                                     + 'BIOSSerialNumber=''' + BIOSSerialNumber + ''','
                                     + 'FirstDiskInfo=''' + FirstDiskInfo + ''','
                                     + 'ProcessorInfo=''' + ProcessorInfo + ''','
                                     + 'PhysicalMemoryInfo=''' + PhysicalMemoryInfo + ''','
                                     + 'OperatingSystem=''' + OperatingSystem + ''','
                                     + 'VideoCardName=''' + VideoCardName + ''','
                                     + 'SoundCardName=''' + SoundCardName + '''';
    SqlCmd := SqlCmd + ' WHERE MachineName=''' + MachineName + ''' AND UserID=' + IntToStr(UserID);

    Result := SQLITE_OK = UserDB._exec(SqlCmd);
  end else
  begin
    SqlCmd := 'INSERT INTO MachineDetail (UserID,MachineName,BaseBoard,SMBiosVersion,BiosName,BIOSSerialNumber,FirstDiskInfo,ProcessorInfo,PhysicalMemoryInfo,OperatingSystem,VideoCardName,SoundCardName) ';
    SqlCmd := SqlCmd + 'VALUES('  + IntToStr(UserID) + ','''
                                  + MachineName + ''','''
                                  + BaseBoard + ''','''
                                  + SMBiosVersion + ''','''
                                  + BiosName + ''','''
                                  + BIOSSerialNumber + ''','''
                                  + FirstDiskInfo + ''','''
                                  + ProcessorInfo + ''','''
                                  + PhysicalMemoryInfo + ''','''
                                  + OperatingSystem + ''','''
                                  + VideoCardName + ''','''
                                  + SoundCardName + ''')';
    Result := SQLITE_OK = UserDB._exec(SqlCmd);
  end;             
end;

function SetUser (UserName: PChar; MaxMachineCount: Integer): LongBOOL; Stdcall;
var
  UserNameStr, SqlCmd: String;
  RS:IMksqlStmt;
begin
  //验证用户是否已经存在
  UserNameStr := StrPas (UserName);
  SqlCmd := 'SELECT MaxMachineCount FROM UserDetail WHERE UserName=''' + UserNameStr + '''';
  RS := UserDB.exec (SqlCmd);
  if not RS.EOF then
  begin
    if MaxMachineCount <> RS[0] then
    begin
      SqlCmd := 'UPDATE UserDetail SET MaxMachineCount=' + IntToStr(MaxMachineCount) + ' WHERE UserName=''' + UserNameStr + '''';
      UserDB._exec (SqlCmd);
    end;
    RS := nil;
    Result := True;
    Exit;
  end;
  RS := nil;

  SqlCmd := 'INSERT INTO UserDetail (UserName,MaxMachineCount) VALUES(''' + UserNameStr + ''',' + IntToStr(MaxMachineCount) + ')';
  UserDB._exec (SqlCmd);
  Result := True;
end;

function GetMachineMaxCount (UserName: PChar; Out MaxMachineCount: Integer): BOOL; Stdcall;
var
  UserNameStr, SqlCmd: String;
  RS:IMksqlStmt;
begin
  Result := False;
  MaxMachineCount := 0;
  UserNameStr := StrPas (UserName);
  SqlCmd := 'SELECT MaxMachineCount FROM UserDetail WHERE UserName=''' + UserNameStr + '''';
  RS := UserDB.exec (SqlCmd);
  if not RS.EOF then
  begin
    MaxMachineCount := RS[0];
    Result := True;
  end;
  RS := nil;
end;

function RegistMachine (RegistAccount, RegisterCode: String): BOOL; External 'RegistCode.dll';


function IsRegistMachineOK (UserName, RegisterCode, MachineInfo: String): BOOL;
begin
  Result := False;
  Try
    if RegistMachine (UserName, RegisterCode) then
      Result := SetMachine (PChar(UserName), PChar(MachineInfo));
  Except
    log ('Exception : RegistMachine');
  end;
end;

//MachineName=RegistMachineName
//RegisterCode={940B3105-C329-4E58-A156-5EBC31056542}
//BaseBoard=BENQ ODEM
//SMBiosVersion=Version 2.04 v2.3
//BiosName=Insyde Software MobilePRO BIOS Version 4.00.00
//BIOSSerialNumber=98K120114652400257DH7000
//FirstDiskInfo=IDE Model: HTS548060M9AT00, Serial: MRLB95L4JWYDEC, Size = 60,011,642,880
//ProcessorInfo=Intel(R) Pentium(R) M processor 1.60GHz=00000000AFE9F9BF
//PhysicalMemoryInfo=268435456,268435456
//OperatingSystem=Microsoft Windows XP Professional|C:\WINDOWS|\Device\Harddisk0\Partition1
//VideoCardName=ATI MOBILITY RADEON 9600/9700 Series
//SoundCardName=SoundMAX Integrated Digital Audio

function IsMachineValid (UserName: PChar; CodeID, CodeResult: PChar; OutMachineID: PInteger): LongBool; stdcall;
var
  RS:IMksqlStmt;
  RealResult: PChar;
  ResultSize, UserID, MachineID: Integer;
  MachineName, UserNameDB, RunResult, SqlCmd: String;
  RunResultSL: TStringList;
  BaseBoard,SMBiosVersion,BiosName,BIOSSerialNumber,FirstDiskInfo,
  ProcessorInfo,PhysicalMemoryInfo,OperatingSystem,VideoCardName,SoundCardName: String;
  RegisterCode: String;
begin
  Result := DecryptCodeResult (CodeID, CodeResult, @RealResult, @ResultSize);

  if Not Result then
  begin
    LOG ('IsMachineValid : DecryptCodeResult ERROR');
    Exit;
  end;

  RunResultSL := TStringList.Create;
  RunResult := Trim (StrPas (RealResult));
  RunResultSL.Text := RunResult;

  BaseBoard := RunResultSL.Values['BaseBoard'];
  SMBiosVersion := RunResultSL.Values['SMBiosVersion'];
  BiosName := RunResultSL.Values['BiosName'];
  BIOSSerialNumber := RunResultSL.Values['BIOSSerialNumber'];
  FirstDiskInfo := RunResultSL.Values['FirstDiskInfo'];
  ProcessorInfo := RunResultSL.Values['ProcessorInfo'];
  PhysicalMemoryInfo := RunResultSL.Values['PhysicalMemoryInfo'];
  OperatingSystem := RunResultSL.Values['OperatingSystem'];
  VideoCardName := RunResultSL.Values['VideoCardName'];
  SoundCardName := RunResultSL.Values['SoundCardName'];
  RegisterCode :=  RunResultSL.Values['RegisterCode'];
  RunResultSL.Free;
  FreeVerifyCode (CodeID);

  Result := False;
  Repeat
    if BaseBoard = '' then Break;
    if SMBiosVersion = '' then Break;
    if BiosName = '' then Break;
    if BIOSSerialNumber = '' then Break;
    if FirstDiskInfo = '' then Break;
    if ProcessorInfo = '' then Break;
    if PhysicalMemoryInfo = '' then Break;
    if OperatingSystem = '' then Break;
    if VideoCardName = '' then Break;
    if SoundCardName = '' then Break;
    Result := True;
  until True;

  if Not Result then
  begin
    LOG ('IsMachineValid : Parameter Checked ERROR');
    LOG (RunResult);
    Exit;
  end;
  Result := False;

  if RegisterCode <> '' then
  begin
    if IsRegistMachineOK (UserName, RegisterCode, RunResult) then
      LOG ('RegistMachine OK')
    else begin
      LOG ('RegistMachine NO');
      Exit;
    end;
  end;

  SqlCmd := 'SELECT UserID,MachineID,MachineName FROM MachineDetail WHERE '
                                   + 'BaseBoard=''' + BaseBoard + ''' AND '
                                   + 'SMBiosVersion=''' + SMBiosVersion + ''' AND '
                                   + 'BiosName=''' + BiosName + ''' AND '
                                   + 'BIOSSerialNumber=''' + BIOSSerialNumber + ''' AND '
                                   + 'FirstDiskInfo=''' + FirstDiskInfo + ''' AND '
                                   + 'ProcessorInfo=''' + ProcessorInfo + ''' AND '
                                   + 'PhysicalMemoryInfo=''' + PhysicalMemoryInfo + ''' AND '
                                   + 'OperatingSystem=''' + OperatingSystem + ''' AND '
                                   + 'VideoCardName=''' + VideoCardName + ''' AND '
                                   + 'SoundCardName=''' + SoundCardName + '''';
  RS := UserDB.exec (SqlCmd);
  if RS.EOF then
  begin
    RS := nil;
    LOG ('IsMachineValid : The input machine info is NOT ALL match, not found valid user');
    Exit;
  end;
  UserID := RS[0];
  MachineID := RS[1];
  MachineName := RS[2];
  RS := nil;

  LOG ('MachineName: ' + MachineName + ' Login');

  SqlCmd := 'SELECT UserName FROM UserDetail WHERE UserID=' + IntToStr(UserID);
  RS := UserDB.exec (SqlCmd);
  UserNameDB := RS[0];
  RS := nil;
  if UserNameDB <> StrPas(UserName) then
  begin
    LOG ('IsMachineValid : Even machine infos is ok, but the LoginName is NOT valid');
    LOG ('UserNameDB:'+UserNameDB + ' <> LoginName:' + StrPas(UserName));
    Exit;
  end;

  OutMachineID^ := MachineID;
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

  VerifyList := THashedStringList.Create;
  VerifyListLock := TCriticalSection.Create;
  ThreadHandle := CreateThread (nil, 0, @ModuleDaemonThread, nil, 0, TID);
end;
              
procedure Finially;
begin
  UserDB.close;
  UserDB.Free;

  VerifyList.Free;
  VerifyListLock.Free;
  TerminateThread (ThreadHandle, 0);
end;

end.
