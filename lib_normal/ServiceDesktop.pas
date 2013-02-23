unit ServiceDesktop;

interface

function InitServiceDesktop: boolean;
procedure DoneServiceDeskTop;

implementation

uses Windows, SysUtils;

const
  DefaultWindowStation = 'WinSta0';
  DefaultDesktop = 'Default';
var
  hwinstaSave: HWINSTA;
  hdeskSave: HDESK;
  hwinstaUser: HWINSTA;
  hdeskUser: HDESK;
function InitServiceDesktop: boolean;
var
  dwThreadId: DWORD;
begin
  dwThreadId := GetCurrentThreadID;
  // Ensure connection to service window station and desktop, and
  // save their handles.
  hwinstaSave := GetProcessWindowStation;
  hdeskSave := GetThreadDesktop(dwThreadId);

  hwinstaUser := OpenWindowStation(DefaultWindowStation, FALSE, MAXIMUM_ALLOWED);
  if hwinstaUser = 0 then
  begin
    OutputDebugString(PChar('OpenWindowStation failed' + SysErrorMessage(GetLastError)));
    Result := false;
    exit;
  end;

  if not SetProcessWindowStation(hwinstaUser) then
  begin
    OutputDebugString('SetProcessWindowStation failed');
    Result := false;
    exit;
  end;

  hdeskUser := OpenDesktop(DefaultDesktop, 0, FALSE, MAXIMUM_ALLOWED);
  if hdeskUser = 0 then
  begin
    OutputDebugString('OpenDesktop failed');
    SetProcessWindowStation(hwinstaSave);
    CloseWindowStation(hwinstaUser);
    Result := false;
    exit;
  end;
  Result := SetThreadDesktop(hdeskUser);
  if not Result then
    OutputDebugString(PChar('SetThreadDesktop' + SysErrorMessage(GetLastError)));
end;

procedure DoneServiceDeskTop;
begin
  // Restore window station and desktop.
  SetThreadDesktop(hdeskSave);
  SetProcessWindowStation(hwinstaSave);
  if hwinstaUser <> 0 then
    CloseWindowStation(hwinstaUser);
  if hdeskUser <> 0 then
    CloseDesktop(hdeskUser);
end;

end.
