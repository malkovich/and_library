library MachineManager;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  Windows,
  SysUtils,
  GetRpcListUnit,
  IniFiles, LogHost,
  Classes,
  VerifyCodeDllInterface in 'VerifyCodeDllInterface.pas',
  AccountInterface in 'AccountInterface.pas';

Exports
  GetRpcList;

function GetFileName(hModule:THandle):string;
var
  Buffer:array[byte] of char;
begin
  ZeroMemory(@buffer[0], 256);
  GetModuleFileName(hModule, @Buffer, 256);
  result := buffer;
end;

var
  ReadSecurityCode: String;

function GetDefaultSecurityCode: String;
var
  IniFile: TIniFile;
  FileName: String;
begin
  if ReadSecurityCode = '' then
  begin
    FileName := GetFileName (0);
    FileName := ExtractFilePath (FileName);
    FileName := FileName + 'MachineManager.ini';
    IniFile := TIniFile.Create (FileName);
    Result := IniFile.ReadString('SecuritySetting', 'SerurityCode', 'Default_SecurityCode');
    IniFile.Free;
    ReadSecurityCode := Result;
  end else
    Result := ReadSecurityCode;
end;
                  
{
RPC模块的要求：
1）自己处理内存
2）自己处理异常
3）Loadlibrary和FreeLibrary要有始有终
}

begin
  Default_LogType := 'MachineManager';

  AccountInterface.Default_SecurityCode := GetDefaultSecurityCode;
  VerifyCodeDllInterface.Default_SecurityCode := GetDefaultSecurityCode;
  AccountInterface.Initial ('DB.s3db');
  
  AddRpcFunction ('SetUser', @Srv_SetUser);
  AddRpcFunction ('SetMachine', @Srv_SetMachine);
  AddRpcFunction ('DeleteUser', @Srv_DeleteUser);
  AddRpcFunction ('DeleteMachine', @Srv_DeleteMachine);
  AddRpcFunction ('GetMachineMaxCount', @Srv_GetMachineMaxCount);
  AddRpcFunction ('GetMachineList', @Srv_GetMachineList);
  AddRpcFunction ('GetMachineDetail', @Srv_GetMachineDetail);

  AddRpcFunction ('SetAccount', @Srv_SetAccount);
  AddRpcFunction ('GetAccount', @Srv_GetAccount);
  AddRpcFunction ('DeleteAccount', @Srv_DeleteAccount);
  AddRpcFunction ('GetAccountList', @Srv_GetAccountList);


end.

