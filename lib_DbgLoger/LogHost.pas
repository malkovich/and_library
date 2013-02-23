unit LogHost;

interface
uses windows, classes;

Type
  TPrintType = (ptTime, ptProcess, ptProcessID, ptModule);
  TPrintTypes = set of TPrintType;

  TLogCallback = Procedure (LogType, Process, ProcessID, Module, LogText: PChar); Stdcall;


const
  LOG_REAL_MODE = 0;
  LOG_PROXY_MODE = 1;
  LOG_CACHE_PROXY_MODE = 2;
  LOG_SYNC_REAL_MODE = 3;
  LOG_SYNC_PROXY_MODE = 4;

  LOG_MODE_DEFAULT = LOG_SYNC_PROXY_MODE;

  ALL_PRINT_TYPE = [ptTime, ptProcess, ptProcessID, ptModule];

Procedure LOG (LogType: PChar; LogText: PChar; LogMode: Integer); Stdcall; Overload;
Procedure LOG (LogText: String; LogMode: Integer = LOG_MODE_DEFAULT); Overload;
Procedure LOG (LogFmt: String; Const Args: Array of Const); Overload;
Procedure LOG (LogType, LogFmt: String; Const Args: Array of Const; LogMode: Integer = LOG_MODE_DEFAULT); Overload;
Procedure LOG (LogType, LogText: String; LogMode: Integer = LOG_MODE_DEFAULT); Overload;

procedure LogConsole(msg:string; LimitSize: Integer = 128); Overload;
procedure LogConsole(fmt:string; parma:array of TVarRec; LimitSize: Integer = 128); Overload;
procedure LogConsole(LogText: String; msg:string; LimitSize: Integer = 128); Overload;
procedure LogConsole(LogText: String; fmt:string; parma:array of TVarRec; LimitSize: Integer = 128); Overload;

Function LogFile (LogType: PChar; FormTime, ToTime: TDateTime; OutFile: PChar): BOOL; Stdcall; overload;
Function LogFile (LogType: PChar; LastDays: Integer; OutFile: PChar): BOOL; Stdcall; overload;
Function LogChecker (LogType: PChar; FormTime, ToTime: TDateTime; Checker: TLogCallback): BOOL; Stdcall;
Function LogPrinter (LogType: String; FormTime, ToTime: TDateTime; PrintTypes: TPrintTypes = ALL_PRINT_TYPE): TStringList; overload;
Function LogPrinter (LogType: String; LastMins: Integer; PrintTypes: TPrintTypes = ALL_PRINT_TYPE): TStringList; overload;

function IsInitialDLL: BOOL;

var
  Default_LogType: String;
  LogEnable: BOOL = True;
  ConsoleEnable: BOOL = True;
  Default_LimitSize: Integer = 0;

implementation

uses SysUtils, DateUtils, madShell, dialogs, IniFiles;

var
  _LOG : Procedure(LogType: PChar; LogText: PChar; LogMode: Integer); Stdcall;
  _LogFile : Function(LogType: PChar; FormTime, ToTime: TDateTime; OutFile: PChar): BOOL; Stdcall;
  _LogChecker : Function (LogType: PChar; FormTime, ToTime: TDateTime; Checker: TLogCallback): BOOL; Stdcall;
  _LogPrinter : Function (LogType: String; FormTime, ToTime: TDateTime; PrintTypes: TPrintTypes): TStringList;


Procedure ConsolePrint (Msg: String; LimitSize: Integer = 128);
begin
  if not ConsoleEnable then exit;
  if not IsConsole then exit;

  if Default_LimitSize <> 0 then
  begin
    LimitSize := Default_LimitSize;
  end;

  if LimitSize <> -1 then
  begin
    if Length (Msg) > LimitSize then
    begin
      SetLength (Msg, LimitSize);
      Msg := Msg + '...';
    end;
  end;
  WriteLn(Msg);    
end;  

procedure LogConsole(fmt:string; parma:array of TVarRec; LimitSize: Integer = 128);
var
  dbgStr: String;
begin
  dbgStr := Format(fmt, parma);
  log(dbgStr);
  ConsolePrint (dbgStr, LimitSize);
end;

procedure LogConsole(msg:string; LimitSize: Integer = 128);
begin
  log(msg);
  ConsolePrint (msg);
end;

procedure LogConsole(LogText: String; msg:string; LimitSize: Integer = 128);
begin
  log(LogText, msg);
  ConsolePrint (LogText + ':' + msg, LimitSize);
end;

procedure LogConsole (LogText: String; fmt:string; parma:array of TVarRec; LimitSize: Integer = 128);
var
  dbgStr: String;
begin
  dbgStr := Format(fmt, parma);
  log (LogText, dbgStr);
  ConsolePrint (LogText + ':' + dbgStr, LimitSize);
end;

function ReadDbgLogerINI (IniFileName, Ident: String): String;
begin
  Result := '';
  With TIniFile.Create (IniFileName) do
  Try
    Result := ReadString ('KeyFile', Ident, '');
  finally
    Free;
  end;  
end;


var
  DllHandle: THandle;
  DllName: String;
  LastErrFoundDLL: DWORD;
  IsPrintError: BOOL;

function GetDbgLogDLLName: string;
var
  IniFileName: String;
begin
  Result := '';
  if not GetSpecialFolder (sfAllUsersAppData, IniFileName) then
  begin
    OutputDebugString ('日志模块：获取配置文件路径错误');
    Exit;
  end;

  IniFileName := IniFileName + '\ANDSoft\DbgLoger.ini';
  if FileExists (IniFileName) then
    Result := ReadDbgLogerINI (IniFileName, 'DbgLogerDLL');

  if not FileExists (Result) then
  begin
    if not IsPrintError then
    begin
      OutputDebugString(PChar ('日志模块：可能日志模块安装不正确:'#13#10 + Result));
      IsPrintError := True;
    end;
    Result := '';
  end;
end;

function IsInitialDLL: BOOL;
begin
  Result := False;   
  if GetTickCount - LastErrFoundDLL < 10*1000 then exit;

  if DllHandle = 0 then
  begin
    if DllName = '' then
    begin
      DllName := GetDbgLogDLLName;
      if Default_LogType = '' then
        Default_LogType := ExtractFileName (GetModuleName(0));
    end;

    if FileExists (DllName) then
    begin
      DllHandle := LoadLibrary (PChar(DllName));
      if DllHandle > 0 then
      begin
        _LOG := GetProcAddress (DllHandle, 'LOG');
        _LogFile := GetProcAddress (DllHandle, 'LogFile');
        _LogChecker := GetProcAddress (DllHandle, 'LogChecker');
        _LogPrinter := GetProcAddress (DllHandle, 'LogPrinter');
      end;
    end else
    begin
      LastErrFoundDLL := GetTickCount;
      Exit;
    end;
  end;

  Result := DllHandle > 0;
end;

function GetSelfModuleName: string;
var
  SignWord: PWORD;
  SelfHandle: THandle;
begin
  SignWord := @GetSelfModuleName;
  SignWord := Pointer (DWORD (SignWord) And $FFFFF000);

  while SignWord^ <> $5A4D do
    SignWord := Pointer (DWORD(SignWord) - $1000);

  SelfHandle := THandle (SignWord);
  result := GetModuleName (SelfHandle);
end;

var
  LastCheckBypassLog: DWORD;
  IsByPassLog_Tmp: BOOL;

function IsByPassLog: BOOL;
var
  FileName: String;
begin
  if not LogEnable then
  begin
    Result := True;
    Exit;
  end;

  if GetTickCount - LastCheckBypassLog > 10 * 1000 then
  begin
    FileName := ExtractFilePath (GetSelfModuleName) + 'Debug';
    IsByPassLog_Tmp := not FileExists (FileName);
    LastCheckBypassLog := GetTickCount;
  end;
  Result := IsByPassLog_Tmp;
end;

Procedure LOG (LogType: PChar; LogText: PChar; LogMode: Integer); Stdcall;
begin
  if IsByPassLog then Exit;
  
  if not IsInitialDLL then exit;
  Try
    _Log (LogType, LogText, LogMode);
  Except
    OutputDebugString ('日志模块异常');
  end;
end;

Procedure LOG (LogType, LogText: String; LogMode: Integer = LOG_MODE_DEFAULT);
begin
  LOG (PChar(LogType), PChar(LogText), LogMode);
end;

Procedure LOG (LogText: String; LogMode: Integer = LOG_MODE_DEFAULT);
begin
  if not IsInitialDLL then exit;
  if Default_LogType = '' then Exit;
  Log (PChar(Default_LogType), PChar(LogText), LogMode);
end;

Procedure LOG (LogFmt: String; Const Args: Array of Const);
begin
  LOG (Format (LogFmt, Args));
end;

Procedure LOG (LogType, LogFmt: String; Const Args: Array of Const; LogMode: Integer = LOG_MODE_DEFAULT);
var
  LogText: String;
begin
  LogText := Format (LogFmt, Args);
  LOG (PChar(LogType), PChar(LogText), LogMode);
end;

Function LogFile (LogType: PChar; FormTime, ToTime: TDateTime; OutFile: PChar): BOOL; Stdcall;
begin
  Result := False;
  if not IsInitialDLL then exit;
  Result := _LogFile (LogType, FormTime, ToTime, OutFile);
end;

Function LogFile (LogType: PChar; LastDays: Integer; OutFile: PChar): BOOL; Stdcall;
var
  FormTime, ToTime: TDateTime;
begin
  ToTime := Now;
  FormTime := IncDay (ToTime, -LastDays);
  Result := LogFile (LogType, FormTime, ToTime, OutFile);
end;

Function LogChecker (LogType: PChar; FormTime, ToTime: TDateTime; Checker: TLogCallback): BOOL; Stdcall;
begin
  Result := False;
  if not IsInitialDLL then exit;
  Result := _LogChecker (LogType, FormTime, ToTime, Checker);
end;

Function LogPrinter (LogType: String; FormTime, ToTime: TDateTime; PrintTypes: TPrintTypes = ALL_PRINT_TYPE): TStringList;
begin
  if not IsInitialDLL then
  begin
    Result := TStringList.Create;
    exit;
  end;
  Result := _LogPrinter (LogType, FormTime, ToTime, PrintTypes);
end;

Function LogPrinter (LogType: String; LastMins: Integer; PrintTypes: TPrintTypes = ALL_PRINT_TYPE): TStringList;
var
  FormTime, ToTime: TDateTime;
begin
  ToTime := Now;
  FormTime := IncMinute (ToTime, -LastMins);
  Result := LogPrinter (LogType, FormTime, ToTime, PrintTypes);
end;

procedure LogHostDone;
begin
  Try
    if DllHandle > 0 then
      FreeLibrary (DllHandle);
  except
  end;
end;

initialization

finalization
  LogHostDone;

end.
