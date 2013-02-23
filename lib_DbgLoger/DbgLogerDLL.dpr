library DbgLogerDLL;

{$R 'DBInclude.res' 'DBInclude.rc'}

uses
  Windows,
  LogerDllUnit in 'LogerDllUnit.pas',
  LogDatabaseUnit in 'LogDatabaseUnit.pas';

procedure DLLEntryPoint (dwReason: DWord);
begin
  case dwReason  of
    DLL_PROCESS_ATTACH:
    begin

    end;
    DLL_PROCESS_DETACH:
    begin
      FinalDbgLogerDLL;
    end;
  end;
end;


exports
  LOG,
  LogFile,
  LogChecker,
  LogPrinter;

begin
  DllProc   :=   @DLLEntryPoint;
  DLLEntryPoint (DLL_PROCESS_ATTACH);
end.


