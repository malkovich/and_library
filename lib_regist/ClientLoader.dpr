program ClientLoader;

{$APPTYPE CONSOLE}

uses
  SysUtils, DLLLoader, Windows, CodeEncrypt, classes, MakeRC4Code,
  BuildinFunctionTree, VerifyCodeUnit;

function GetFileName(hModule:THandle):string;
var
  Buffer:array[byte] of char;
begin
  ZeroMemory(@buffer[0], high(byte));
  GetModuleFileName(hModule, Buffer, high(byte));
  result := buffer;           
end;

var
  GetMM: TMemoryStream;
  CodeID, OutCodeBase64: PChar;
  RealResult: PChar;
  ResultSize, MachineID: Integer;
  DLLLoader: TDLLLoader;
  ModName: String;

const
  Test_Machine_Info = 'MachineName=神州3服8号机'#13#10 +
    'BaseBoard=ASUSTek Computer INC. PC-DL'#13#10 +
    'SMBiosVersion=ASUS PC-DL ACPI BIOS Revision 1009 v2.3'#13#10 +
    'BiosName=Phoenix - AwardBIOS v6.00PG'#13#10 +
    'BIOSSerialNumber=NoResult'#13#10 +
    'FirstDiskInfo=IDE Model: ST3250823AS, Serial: 4ND01LC0, Size = 137,438,952,960'#13#10 +
    'ProcessorInfo=Intel(R) Xeon(TM) CPU 2.00GHz=BFEBFBFF00000F29,Intel(R) Xeon(TM) CPU 2.00GHz=BFEBFBFF00000F29,Intel(R) Xeon(TM) CPU 2.00GHz=BFEBFBFF00000F29,Intel(R) Xeon(TM) CPU 2.00GHz=BFEBFBFF00000F29'#13#10 +
    'PhysicalMemoryInfo=1073741824,1073741824,1073741824,1073741824'#13#10 +
    'OperatingSystem=Microsoft Windows Server 2003 Enterprise Edition|C:\WINDOWS|\Device\Harddisk0\Partition1'#13#10 +
    'VideoCardName=NVIDIA GeForce 6800'#13#10 +
    'SoundCardName=SoundMAX Integrated Digital Audio';

var
  Buffer: Array[0..$1000-1] of char;
  MachineInfoSL: TStringList;

begin
  Initial ('VerifyCode.DB');
  SetUser ('TestUser', 100);

  MachineInfoSL := TStringList.Create;
  MachineInfoSL.Text := Test_Machine_Info;
  MachineInfoSL.Values['MachineName'] := 'MachineName_RamdonXXX_' + IntToStr(GetTickCount);
  MachineInfoSL.Values['ProcessorInfo'] := 'CPUID_RamdonXXX_' + IntToStr(GetTickCount);
  MachineInfoSL.Values['FirstDiskInfo'] := 'IDE DISK_RamdonXXX_' + IntToStr(GetTickCount);
  SetMachine ('TestUser', PChar(MachineInfoSL.Text));
  MachineInfoSL.Free;
  
  SetMachine ('TestUser', PChar(Test_Machine_Info));

  if not GetVerifyCode (@CodeID, @OutCodeBase64) then
  begin
    WriteLn ('GetVerifyCode ERROR');
    Exit;
  end;

  GetMM := TMemoryStream.Create;
  DynamicCodeDecrypt (Strpas(OutCodeBase64), GetMM);
  GetMM.Seek(0, soFromBeginning);

  ModName := GetDLLModuleName (GetMM.Memory, GetMM.Size);

  DLLLoader := TDLLLoader.Create;

  if DLLLoader.Load (GetMM, GetLibraryEntry) then
  begin
    if IsMachineValid ('TestUser', CodeID, PChar(DLLLoader.AttachResult), @MachineID) then
    begin
      WriteLn ('MachineID ', MachineID);

      GetMachineList ('TestUser', @Buffer[0], $1000);
      WriteLn ('GetMachineList ', Buffer);

      GetMachineDetail ('TestUser', '神州3服8号机', @Buffer[0], $1000);
      WriteLn ('GetMachineList ', Buffer);
    end;
  end;

  GetMM.Free;
  DLLLoader.Free;

  readln;

  DeleteUser ('TestUser');
  Finially;
end.
