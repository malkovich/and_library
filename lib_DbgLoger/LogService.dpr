library LogService;

uses
  Windows, LogHost;

Procedure __LogText (LogType: PChar; LogText: PChar; LogMode: Integer); Stdcall;
begin
  LogHost.LOG (LogType, LogText, LogMode);
end;

Procedure __log (LogType: PChar; LogText: PChar); Stdcall;
begin
  LogHost.LOG (LogType, LogText, LOG_MODE_DEFAULT);
end;


Function __LogFile (LogType: PChar; FormTime, ToTime: TDateTime; OutFile: PChar): BOOL; Stdcall;
begin
  Result := LogHost.LogFile (LogType, FormTime, ToTime, OutFile);
end;

Exports
  __log name 'log',
  __LogText name 'LogText',
  __LogFile name 'LogFile';

begin
  IsInitialDLL;
end.
