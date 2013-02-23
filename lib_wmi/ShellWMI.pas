unit ShellWMI;

interface
uses windows;

function GetBaseBoard (Buffer: PChar): Integer; Stdcall;
function GetSMBiosVersion (Buffer: PChar): Integer; Stdcall;
function GetBiosName (Buffer: PChar): Integer; Stdcall;
function GetBIOSSerialNumber (Buffer: PChar): Integer; Stdcall;
function GetFirstDiskInfo (Buffer: PChar): Integer; Stdcall;
function GetProcessorInfo (Buffer: PChar): Integer; Stdcall;
function GetPhysicalMemoryInfo (Buffer: PChar): Integer; Stdcall;
function GetOperatingSystem (Buffer: PChar): Integer; Stdcall;
function GetVideoCardName (Buffer: PChar): Integer; Stdcall;
function GetSoundCardName (Buffer: PChar): Integer; Stdcall;

function One_GetBaseBoard (Buffer: PChar): Integer; Stdcall;
function One_GetSMBiosVersion (Buffer: PChar): Integer; Stdcall;
function One_GetBiosName (Buffer: PChar): Integer; Stdcall;
function One_GetBIOSSerialNumber (Buffer: PChar): Integer; Stdcall;
function One_GetFirstDiskInfo (Buffer: PChar): Integer; Stdcall;
function One_GetProcessorInfo (Buffer: PChar): Integer; Stdcall;
function One_GetPhysicalMemoryInfo (Buffer: PChar): Integer; Stdcall;
function One_GetOperatingSystem (Buffer: PChar): Integer; Stdcall;
function One_GetVideoCardName (Buffer: PChar): Integer; Stdcall;
function One_GetSoundCardName (Buffer: PChar): Integer; Stdcall;

function CallBufferToString (BuffFunc: Pointer): String;

implementation

uses Classes, SysUtils, RunAsUserUnit, Base64Unit;



function CallBufferToString (BuffFunc: Pointer): String;
var
  BuffPro: function (Buffer: PChar): Integer; Stdcall;
  RunLength: Integer;
begin
  BuffPro := BuffFunc;
  SetLength (Result, $1000);
  RunLength := BuffPro (@Result[1]);
  SetLength (Result, RunLength);
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

var
  MachineExeName: String;

function GetLocalFileName (): String;
begin
  if MachineExeName <> '' then
  begin
    Result := MachineExeName;
    exit;
  end;

  Result := GetSelfModuleName;
  Result := ExtractFilePath (Result);
  Result :=Result + 'SerialNumber.exe';
  MachineExeName := Result;
end;

function CallMachineConsole (KeyName: String): String;
var
  AppName: String;
  RunResult: TStringList;
begin
  Result := '';
  AppName := GetLocalFileName + ' ' +  KeyName;
  RunResult := RunConsoleAsDefault (AppName);
  if RunResult.Count = 1 then
  begin
    Result := Trim(RunResult[0]);
  end;
  RunResult.Free;
end;

function FillInBuffer (ToFillStr: String; Buffer: PChar): Integer;
begin
  ToFillStr := Trim (ToFillStr);
  Result := Length (ToFillStr);
  if Result > 0 then
  begin
    CopyMemory (Buffer, PChar(ToFillStr), Result);
    Buffer [Result] := #0;
  end;
end;

//////////////////////////////////////////////
//////////////////////////////////////////////

var
  GotStr: String;

function GetMachineInfo (Index: Integer; OutBuffer: PChar): Integer;
var
  ToFillStr, UseStr : String;
  MachineInfoList: TStringList;
begin
  if GotStr = '' then
    GotStr := CallMachineConsole ('GetAllInfos');

  UseStr := DecodeBase64 (GotStr);
  MachineInfoList := TStringList.Create;
  MachineInfoList.Text := UseStr;

  Result := 0;
  if Index > MachineInfoList.Count - 1 then
  begin
    MachineInfoList.Free;
    Exit;
  end;

  ToFillStr := MachineInfoList[Index];
  Result := FillInBuffer (ToFillStr, OutBuffer);
  MachineInfoList.Free;
end;

function One_GetBaseBoard (Buffer: PChar): Integer; Stdcall;
begin
  Result := GetMachineInfo (0, Buffer);
end;
function One_GetSMBiosVersion (Buffer: PChar): Integer; Stdcall;
begin
  Result := GetMachineInfo (1, Buffer);
end;
function One_GetBiosName (Buffer: PChar): Integer; Stdcall;
begin
  Result := GetMachineInfo (2, Buffer);
end;
function One_GetBIOSSerialNumber (Buffer: PChar): Integer; Stdcall;
begin
  Result := GetMachineInfo (3, Buffer);
end;
function One_GetFirstDiskInfo (Buffer: PChar): Integer; Stdcall;
begin
  Result := GetMachineInfo (4, Buffer);
end;
function One_GetProcessorInfo (Buffer: PChar): Integer; Stdcall;
begin
  Result := GetMachineInfo (5, Buffer);
end;
function One_GetPhysicalMemoryInfo (Buffer: PChar): Integer; Stdcall;
begin
  Result := GetMachineInfo (6, Buffer);
end;
function One_GetOperatingSystem (Buffer: PChar): Integer; Stdcall;
begin
  Result := GetMachineInfo (7, Buffer);
end;
function One_GetVideoCardName (Buffer: PChar): Integer; Stdcall;
begin
  Result := GetMachineInfo (8, Buffer);
end;
function One_GetSoundCardName (Buffer: PChar): Integer; Stdcall;
begin
  Result := GetMachineInfo (9, Buffer);
end;


function GetBaseBoard (Buffer: PChar): Integer; Stdcall;
begin
  Result := FillInBuffer (CallMachineConsole ('GetBaseBoard'), Buffer);
end;

function GetSMBiosVersion (Buffer: PChar): Integer; Stdcall;
begin
  Result := FillInBuffer (CallMachineConsole ('GetSMBiosVersion'), Buffer);
end;

function GetBiosName (Buffer: PChar): Integer; Stdcall;
begin
  Result := FillInBuffer (CallMachineConsole ('GetBiosName'), Buffer);
end;

function GetBIOSSerialNumber (Buffer: PChar): Integer; Stdcall;
begin
  Result := FillInBuffer (CallMachineConsole ('GetBIOSSerialNumber'), Buffer);
end;

function GetFirstDiskInfo (Buffer: PChar): Integer; Stdcall;
begin
  Result := FillInBuffer (CallMachineConsole ('GetFirstDiskInfo'), Buffer);
end;

function GetProcessorInfo (Buffer: PChar): Integer; Stdcall;
begin
  Result := FillInBuffer (CallMachineConsole ('GetProcessorInfo'), Buffer);
end;

function GetPhysicalMemoryInfo (Buffer: PChar): Integer; Stdcall;
begin
  Result := FillInBuffer (CallMachineConsole ('GetPhysicalMemoryInfo'), Buffer);
end;

function GetOperatingSystem (Buffer: PChar): Integer; Stdcall;
begin
  Result := FillInBuffer (CallMachineConsole ('GetOperatingSystem'), Buffer);
end;

function GetVideoCardName (Buffer: PChar): Integer; Stdcall;
begin
  Result := FillInBuffer (CallMachineConsole ('GetVideoCardName'), Buffer);
end;

function GetSoundCardName (Buffer: PChar): Integer; Stdcall;
begin
  Result := FillInBuffer (CallMachineConsole ('GetSoundCardName'), Buffer);
end;


end.
