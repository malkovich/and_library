unit BuildInRelocFunctions;

interface
uses WinDefine;     

{$I ..\lib\lib_buildlib\HASH_FunctionDefine.inc}
{$I ..\lib\lib_buildlib\Loader.inc}

var
  AllocMem: function (Size: LongWord): Pointer; Stdcall;  
  FreeMem: procedure  (var Ptr: Pointer); Stdcall;
  DebugPrinter: procedure (Msg: PChar); stdcall;
  OutputDebugStringA: procedure (lpOutputString: PAnsiChar); stdcall;
  GetUpperNameHash: function (InputName: PChar):DWORD;stdcall;

  ResultOutput: TResultCallBack;
  GetProcedureAddrss : TGetProcedureAddrss;

function InitBuildinRelocFunctions (CodeMark: TCodeMark): BOOL;

implementation

uses EncryptDecryptUnit;

var
  IsInitial: BOOL;

function InitBuildinRelocFunctions (CodeMark: TCodeMark): BOOL;
var
  SelfModuleNameHash : LongWord;
begin
  Result := IsInitial;
  if Result then Exit;
  
  OutputDebugStringA := GetProcedureAddrss (HASH_Kernel32_DLL, HASH_OutputDebugStringA);

  GetUpperNameHash := GetProcedureAddrss (HASH_MeLibrary, HASH_GetUpperNameHash);
  SelfModuleNameHash := GetUpperNameHash (CodeMark.MOD_NAME);
  ResultOutput := GetProcedureAddrss (SelfModuleNameHash, HASH_ModuleRunResult);

  AllocMem     := GetProcedureAddrss (HASH_MeLibrary, HASH_AllocMem);
  FreeMem      := GetProcedureAddrss (HASH_MeLibrary, HASH_FreeMem);
  DebugPrinter := GetProcedureAddrss (HASH_MeLibrary, HASH_DebugPrinter);

  Result := True;
end;

end.
