unit TrayIconUnit;

interface
uses
   Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,Registry,ShellAPI, ComCtrls, StdCtrls, Buttons, Menus, ExtCtrls;

const
  ICON_ID=3;
  NIF_INFO = $10;
  NIM_SETVERSION = $00000004;
  NOTIFYICON_VERSION = 3;
  NIM_SETFOCUS = $00000003;
  NIIF_INFO = $00000001;
  NIIF_WARNING = $00000002;
  NIIF_ERROR = $00000003;
  NIN_BALLOONSHOW = WM_USER + 2;
  NIN_BALLOONHIDE = WM_USER + 3;
  NIN_BALLOONTIMEOUT = WM_USER + 4;
  NIN_BALLOONUSERCLICK = WM_USER + 5;
  NIN_SELECT = WM_USER + 0;
  NINF_KEY = $1;
  NIN_KEYSELECT = NIN_SELECT or NINF_KEY;

procedure InstIcon(ToyIcon:TIcon;WinHandle:THandle;cbMessage:Integer; Tip: String);
procedure DeleIcon (winHandle:THandle);
procedure ShowBalloonTips (TipInfo, TipTitle:string);


implementation

Type
  PNewNotifyIconData = ^TNewNotifyIconData;
  TDUMMYUNIONNAME = Record
    case Integer of
      0: (uTimeout: UINT);
      1: (uVersion: UINT);
  end;

  TNewNotifyIconData = Record
    cbSize: DWORD;
    Wnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
    szTip: array [0..127] of Char;    //Version 5.0为128个，以前为64个
    dwState: DWORD;    //Version 5.0
    dwStateMask: DWORD;    //Version 5.0
    szInfo: array [0..255] of Char;   //Version 5.0
    DUMMYUNIONNAME: TDUMMYUNIONNAME;
    szInfoTitle: array [0..63] of Char;   //Version 5.0
    dwInfoFlags: DWORD;   //Version 5.0
  end;

var
  IconData: TNewNotifyIconData;

procedure InstIcon(ToyIcon:TIcon;WinHandle:THandle;cbMessage:Integer; Tip: String);
begin
  IconData.cbSize:=Sizeof(IconData);
  IconData.Wnd:=WinHandle;
  IconData.uID:=ICON_ID;
  IconData.uFlags:=NIF_MESSAGE or NIF_ICON or NIF_TIP;
  IconData.uCallbackMessage:=cbMessage;
  IconData.hIcon:=ToyIcon.Handle;
  StrCopy (IconData.szTip, PChar(Tip));       //使用是该修改
  Shell_NotifyIcon(NIM_ADD,@IconData);
end;

procedure DeleIcon(winHandle:THandle);
begin
  IconData.cbSize:=SizeOf(IconData);
  IconData.Wnd:=winHandle;
  IconData.uID:=ICON_ID;
  Shell_NotifyIcon(NIM_DELETE,@IconData);
end;

procedure ShowBalloonTips(TipInfo,TipTitle:string);
begin
  IconData.cbSize := sizeof(IconData);
  IconData.uFlags := NIF_INFO;
  strPLCopy(IconData.szInfo, TipInfo, sizeof(IconData.szInfo) - 1);
  IconData.DUMMYUNIONNAME.uTimeout := 3000;
  strPLCopy(IconData.szInfoTitle, TipTitle, sizeof(IconData.szInfoTitle) - 1);
  IconData.dwInfoFlags := NIIF_INFO;
  Shell_NotifyIcon(NIM_MODIFY, @IconData);
end;

end.


