program DbgHost;

{$APPTYPE CONSOLE}

uses
  SysUtils, Windows, SyncQueueHandler,
  LogUnit in 'LogUnit.pas';

function CtrlHandlerRoutine(CtrlType : Cardinal) : Cardinal; stdcall;
begin
  Result := 0;
  case CtrlType of
    CTRL_C_EVENT,            // User hit CTRL-C
    CTRL_BREAK_EVENT,        // User hit CTRL-BREAK
    CTRL_LOGOFF_EVENT,       // User log off his session
    CTRL_CLOSE_EVENT,        // Close signal
    CTRL_SHUTDOWN_EVENT :    // Window shutdown signal
    begin
      Result := 1;
      AppTerminate := True;
      SyncQueueHandler.AppTerminated := True;
    end;
  end;
end;


procedure InitialWindowsStyle;
var
  lpBufferInfo: TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo (GetStdHandle(STD_OUTPUT_HANDLE), lpBufferInfo);
  lpBufferInfo.dwSize.Y := lpBufferInfo.dwSize.Y * 10;
  lpBufferInfo.dwSize.X := lpBufferInfo.dwSize.X * 5 div 4;
  lpBufferInfo.dwMaximumWindowSize.X := lpBufferInfo.dwMaximumWindowSize.X * 5 div 4;
  SetConsoleScreenBufferSize( GetStdHandle(STD_OUTPUT_HANDLE), lpBufferInfo.dwSize);
end;

begin
  InitialWindowsStyle;
  SetConsoleCtrlHandler(@CtrlHandlerRoutine, TRUE);
  RunLogRoutine;
  SetConsoleCtrlHandler(@CtrlHandlerRoutine, False);

end.
