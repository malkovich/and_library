library RegistCode;

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
  RegistCodeUnit in 'RegistCodeUnit.pas',
  RegistCodeInterface in 'RegistCodeInterface.pas';

{$R *.res}

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
    FileName := FileName + 'RegistCode.ini';
    IniFile := TIniFile.Create (FileName);
    Result := IniFile.ReadString('SecuritySetting', 'SerurityCode', 'Default_SecurityCode');
    IniFile.Free;
    ReadSecurityCode := Result;
  end else
    Result := ReadSecurityCode;
end;


Exports
  GetRpcList,
  RegistMachine,
  MakeRegistCodesFile,
  MakeRegistCodesFileEx,
  EnableBatchCode,
  GetBatchCodeFile,
  DeleteBatchCode,
  DeleteExpiredRegistCodes,
  DeleteUnActiveRegistCodes,
  DeleteUsedRegistCodes,
  EmptyRegistCodes;


function Except_MakeRegistCodesFileEx (Param: PChar): PChar; Stdcall;
begin
  Result := 'ERR';
  Try
    Result := Srv_MakeRegistCodesFileEx (Param);
  Except
    LOG ('Exception : MakeRegistCodesFileEx');
  end;
end;

function Except_MakeRegistCodesFile (Param: PChar): PChar; Stdcall;
begin
  Result := 'ERR';
  Try
    Result := Srv_MakeRegistCodesFile (Param);
  Except
    LOG ('Exception : MakeRegistCodesFile');
  end;
end;

function Except_ManageRegistCodes (Param: PChar): PChar; Stdcall;
begin
  Result := 'ERR';
  Try
    Result := Srv_ManageRegistCodes (Param);
  Except
    LOG ('Exception : ManageRegistCodes');
  end;
end;

function Except_ManageBatchCodes (Param: PChar): PChar; Stdcall;
begin
  Result := 'ERR';
  Try
    Result := Srv_ManageBatchCodes (Param);
  Except
    LOG ('Exception : ManageBatchCodes');
  end;
end;

  
begin
  Default_LogType := 'RegistCode';

  Default_SecurityCode := GetDefaultSecurityCode;
  Initial ('RegistCode.DB');          
  
  AddRpcFunction ('MakeRegistCodesFileEx', @Except_MakeRegistCodesFileEx);
  AddRpcFunction ('MakeRegistCodesFile', @Except_MakeRegistCodesFile);
  AddRpcFunction ('ManageBatchCodes', @Except_ManageBatchCodes);
  AddRpcFunction ('ManageRegistCodes', @Except_ManageRegistCodes);
end.
