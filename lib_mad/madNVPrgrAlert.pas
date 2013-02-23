// ***************************************************************
//  madNVPrgrAlert.pas        version:  1.0   ·  date: 2005-07-09
//  -------------------------------------------------------------
//  thread safe "non VCL" progress bar alert box
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2005 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2005-07-09 1.0  initial release

unit madNVPrgrAlert;

{$I mad.inc}

interface

uses Windows;

// ***************************************************************

type
  IProgressAlert = interface ['{45B8448C-47B8-4F1C-886E-95ED5FD41E5F}']
    function  GetTitle : string;
    procedure SetTitle (value: string);
    property  Title : string read GetTitle write SetTitle;

    function  GetPosition : integer;
    procedure SetPosition (value: integer);
    property  Position : integer read GetPosition write SetPosition;

    procedure AddArea (weight: int64; text: string = '');
    procedure ClearAreas;
    procedure AreaDone;

    function  GetCurrentArea : integer;
    procedure SetCurrentArea (value: integer);
    property  CurrentArea : integer read GetCurrentArea write SetCurrentArea;

    procedure ProcessMessages;

    function  Show (parentWindow: dword = 0; activate: boolean = true) : boolean;
    procedure Close;
  end;

function NewProgressAlert (title     : string;
                           bmpModule : dword  = 0;
                           bmpName   : string = '') : IProgressAlert;

// ***************************************************************

implementation

uses Messages;

// ***************************************************************

const
  CMaxWidth = 70 * 6;
  CMinWidth = 220;

  PBM_SETRANGE = WM_USER + 1;
  PBM_SETPOS   = WM_USER + 2;

type
  TIProgressAlert = class (TInterfacedObject, IProgressAlert)
    FParentWindow   : dword;
    FTitle          : string;
    FTmpDC          : dword;
    FFont, FOldFont : dword;
    FMainWnd        : dword;
    FBmpWnd         : dword;
    FLabel          : dword;
    FBar            : dword;
    FAreas          : array of record      // one for each thread, for that we
                        before : int64;    // have to calculate a stack trace
                        this   : int64;
                        text   : string;
                      end;
    FSum            : int64;               // sum of all areas
    FCurrentArea    : integer;             // current area, 0..high(ProgressAreas)
    FPosition       : integer;
    FBmpModule      : dword;
    FBmpName        : string;

    constructor Create (title: string; bmpModule: dword; bmpName: string);
    destructor Destroy; override;

    function  GetTitle : string;
    procedure SetTitle (value: string);

    function  GetPosition : integer;
    procedure SetPosition (value: integer);

    procedure AddArea (weight: int64; text: string = '');
    procedure ClearAreas;
    procedure AreaDone;

    function  GetCurrentArea : integer;
    procedure SetCurrentArea (value: integer);

    procedure ProcessMessages;

    function  Show (parentWindow: dword = 0; activate: boolean = true) : boolean;
    procedure Close;
  end;

constructor TIProgressAlert.Create(title: string; bmpModule: dword; bmpName: string);
begin
  inherited Create;
  FTitle     := title;
  FBmpModule := bmpModule;
  FBmpName   := bmpName;
end;

destructor TIProgressAlert.Destroy;
begin
  Close;
  if FTmpDC <> 0 then begin
    SelectObject(FTmpDC, FOldFont);
    DeleteObject(FFont);
    DeleteDC(FTmpDC);
  end;
  inherited;
end;

function TIProgressAlert.GetTitle       : string;  begin result := FTitle       end;
function TIProgressAlert.GetPosition    : integer; begin result := FPosition    end;
function TIProgressAlert.GetCurrentArea : integer; begin result := FCurrentArea end;

procedure TIProgressAlert.SetTitle(value: string);
begin
  if FTitle <> value then begin
    FTitle := value;
    SetWindowText(FMainWnd, pchar(FTitle));
  end;
end;

procedure TIProgressAlert.AddArea(weight: int64; text: string = '');
var i1 : integer;
begin
  i1 := length(FAreas);
  if (text = '') and (i1 > 0) then
    text := FAreas[i1 - 1].text;
  SetLength(FAreas, i1 + 1);
  FAreas[i1].before := FSum;
  FAreas[i1].this   := weight;
  FAreas[i1].text   := text;
  inc(FSum, weight);
end;

procedure TIProgressAlert.ClearAreas;
begin
  FAreas := nil;
  FCurrentArea := 0;
  FSum := 0;
end;

procedure TIProgressAlert.AreaDone;
begin
  SetCurrentArea(FCurrentArea + 1);
end;

procedure TIProgressAlert.SetCurrentArea(value: integer);
begin
  if (value <> FCurrentArea) and (value >= 0) and (value <= Length(FAreas)) then begin
    if value = Length(FAreas) then begin
      FCurrentArea := value - 1;
      SetPosition(1000);
    end else begin
      FCurrentArea := value;
      SetPosition(0);
    end;
    if (FCurrentArea < length(FAreas)) and (FLabel <> 0) then
      SetWindowText(FLabel, pchar(FAreas[FCurrentArea].text));
  end;
end;

procedure TIProgressAlert.ProcessMessages;
var msg : TMsg;
begin
  if GetWindowThreadProcessId(FMainWnd, nil) = GetCurrentThreadId then
    while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do begin
      TranslateMessage(msg);
      DispatchMessage(msg);
    end;
end;

procedure TIProgressAlert.SetPosition(value: integer);
begin
  FPosition := value;
  with FAreas[FCurrentArea] do
    value := (before + this * value div 1000) * 1000 div FSum;
  if FBar <> 0 then
    if GetWindowThreadProcessId(FBar, nil) = GetCurrentThreadId then begin
      SendMessage(FBar, PBM_SETPOS, value, 0);
      ProcessMessages;
    end else
      PostMessage(FBar, PBM_SETPOS, value, 0);
end;

function TIProgressAlert.Show(parentWindow: dword = 0; activate: boolean = true) : boolean;

  function DrawGraphic(module: dword; name: string; hDC, size: dword) : boolean;
  var graphic   : dword;     // bitmap handle
      bdc, obh  : dword;     // bitmap dc, old bitmap handle
      i1        : cardinal;
      alpha     : dword;
      c1, c2    : dword;
      p1        : pointer;
      bi        : TBitmapInfo;
      pbih      : PBitmapInfoHeader;
      buf       : array of cardinal;
      backColor : dword;
  begin
    buf := nil;
    result := false;
    c1 := FindResource(module, pchar(name), RT_BITMAP);
    if (c1 <> 0) and (SizeOfResource(module, c1) = sizeOf(pbih^) + size * size * 4) then begin
      c2 := LoadResource(module, c1);
      if c2 <> 0 then begin
        SetLength(buf, size * size);
        pbih := LockResource(c2); Move(pbih^, bi, sizeOf(pbih^));
        inc(pbih);                Move(pbih^, buf[0], size * size * 4);
        UnlockResource(c2);
        FreeResource(c2);
        backColor := GetSysColor(COLOR_BTNFACE);
        backColor := (backColor and $0000FF) shl 16 +    // swap rgb to bgr
                     (backColor and $00FF00)        +
                     (backColor and $FF0000) shr 16;
        for i1 := 0 to size * size - 1 do begin
          alpha := buf[i1] shr 24 + 1;
          buf[i1] := ( (      alpha  * (buf[i1]   and $00FF00FF) +
                       (256 - alpha) * (backColor and $00FF00FF)   ) shr 8) and $00FF00FF +
                     ( (      alpha  * (buf[i1]   and $0000FF00) +
                       (256 - alpha) * (backColor and $0000FF00)   ) shr 8) and $0000FF00;
        end;
        bdc := CreateCompatibleDC(0);
        if bdc <> 0 then begin
          p1 := nil;
          graphic := CreateDIBSection(bdc, bi, DIB_RGB_COLORS, p1, 0, 0);
          if (graphic <> 0) and (p1 <> nil) then begin
            GdiFlush;
            Move(buf[0], p1^, size * size * 4);
            GdiFlush;
            obh := SelectObject(bdc, graphic);
            BitBlt(hDC, 0, 0, size, size, bdc, 0, 0, SRCCOPY);
            SelectObject(bdc, obh);
            DeleteObject(graphic);
            result := true;
          end;
          DeleteDC(bdc);
        end;
      end;
    end;
  end;

  function ProgressAlertWndProc(window, msg: dword; wParam, lParam: integer) : integer; stdcall;
  var b1 : boolean;
      pa : TIProgressAlert;
  begin
    b1 := false;
    case msg of
      WM_CLOSE      : b1 := true;
      WM_SYSCOMMAND : b1 := wParam and $FFF = SC_CLOSE;
      WM_DRAWITEM   : with PDrawItemStruct(lParam)^ do begin
                        pa := pointer(GetWindowLong(hwndItem, GWL_USERDATA));
                        try
                          if (pa <> nil) and (pa is TIProgressAlert) then begin
                            DrawGraphic(pa.FBmpModule, pa.FBmpName, hDC, 32);
                            result := 1;
                            exit;
                          end;
                        except end;
                      end;
    end;
    if b1 and (GetWindowLong(window, GWL_USERDATA) = 0) then
         result := 0
    else result := DefWindowProc(window, msg, wParam, lParam);
  end;

var wndClass : TWndClass;
    iw, ih   : integer;
    c1       : dword;
    r1, r2   : TRect;
    i1       : integer;
begin
  result := FMainWnd = 0;
  if result then begin
    FParentWindow := parentWindow;
    if FParentWindow <> 0 then
      EnableWindow(FParentWindow, false);
    if FAreas = nil then
      AddArea(1000, 'Please wait...');
    ZeroMemory(@wndClass, sizeOf(TWndClass));
    with wndClass do begin
      style         := CS_CLASSDC or CS_PARENTDC;
      lpfnWndProc   := @ProgressAlertWndProc;
      hInstance     := SysInit.HInstance;
      hbrBackground := COLOR_BTNFACE + 1;
      lpszClassname := 'prgrAlertWndClass';
      hCursor       := LoadCursor(0, IDC_ARROW);
    end;
    windows.RegisterClass(wndClass);
    // in NT4 you sometimes have to give exactly the same *pointer*
    // which you used in RegisterClass, the same *string* sometimes fails
    FMainWnd := CreateWindowEx(WS_EX_DLGMODALFRAME or WS_EX_TOPMOST or WS_EX_TOOLWINDOW,
                               wndClass.lpszClassname, pchar(FTitle),
                               WS_CAPTION, 0, 0, 100, 100, FParentWindow, 0, HInstance, nil);
    c1 := GetSystemMenu(FMainWnd, false);
    RemoveMenu(c1, SC_MINIMIZE, MF_BYCOMMAND);
    RemoveMenu(c1, SC_MAXIMIZE, MF_BYCOMMAND);
    RemoveMenu(c1, SC_RESTORE,  MF_BYCOMMAND);
    RemoveMenu(c1, SC_SIZE,     MF_BYCOMMAND);
    FFont := CreateFont(-11, 0, 0, 0, 400, 0, 0, 0, DEFAULT_CHARSET,
                        OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                        DEFAULT_PITCH or FF_DONTCARE, 'Tahoma');
    if FFont = 0 then
      FFont := CreateFont(-12, 0, 0, 0, 400, 0, 0, 0, DEFAULT_CHARSET,
                          OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                          DEFAULT_PITCH or FF_DONTCARE, 'MS Sans Serif');
    FTmpDC := CreateCompatibleDC(0);
    FOldFont := SelectObject(FTmpDC, FFont);
    for i1 := FCurrentArea to high(FAreas) do begin
      SetRect(r2, 0, 0, CMaxWidth, 0);
      DrawText(FTmpDC, pchar(FAreas[i1].text), -1, r2, DT_CALCRECT or DT_WORDBREAK);
      if i1 > FCurrentArea then begin
        if r2.Right  > r1.Right  then r1.Right  := r2.Right;
        if r2.Bottom > r1.Bottom then r1.Bottom := r2.Bottom;
      end else
        r1 := r2;
    end;
    if FindResource(FBmpModule, pchar(FBmpName), PChar(RT_BITMAP)) <> 0 then begin
      inc(r1.Left,  32 + 8);
      inc(r1.Right, 32 + 8);
      FBmpWnd := CreateWindow('Button', '', WS_VISIBLE or WS_CHILD or BS_OWNERDRAW or WS_DISABLED,
                              9, 9, 32, 32, FMainWnd, 0, HInstance, nil);
      SetWindowLong(FBmpWnd, GWL_USERDATA, integer(self));
    end else
      FBmpWnd := 0;
    if CMinWidth > r1.Right then
      r1.Right := CMinWidth;
    r1.Left := 11;
    r1.Top  := 12;
    if FBmpWnd <> 0 then begin
      r1.Left := 11 + 32 + 8;
      if r1.Bottom < 26 then begin
        r1.Top := 12 + (26 - r1.Bottom) div 2;
        r1.Bottom := 26;
      end;
    end;
    iw := r1.Right + 22;
    ih := 12 + r1.Bottom + 12;
    FLabel := CreateWindow('Static', pchar(FAreas[FCurrentArea].text), WS_VISIBLE or WS_CHILD or SS_LEFT,
                                     r1.Left, r1.Top, r1.Right, r1.Bottom, FMainWnd, 0, HInstance, nil);
    SendMessage(FLabel, WM_SETFONT, integer(FFont), 0);
    FBar := CreateWindow('msctls_progress32', '', WS_VISIBLE or WS_CHILD,
                         11, ih, iw - 22, GetSystemMetrics(SM_CYVSCROLL), FMainWnd, 0, HInstance, nil);
    SendMessage(FBar, PBM_SETRANGE, 0, 1000 shl 16);
    inc(iw, GetSystemMetrics(SM_CXFIXEDFRAME) * 2);
    inc(ih, GetSystemMetrics(SM_CYSMCAPTION) + GetSystemMetrics(SM_CYVSCROLL) + 12 + GetSystemMetrics(SM_CYFIXEDFRAME) * 2);
    if IsIconic(FMainWnd) then
      ShowWindow(FMainWnd, SW_RESTORE);
    if activate then begin
      SetWindowPos(FMainWnd, 0, (GetSystemMetrics(SM_CXScreen) - iw) div 2,
                                (GetSystemMetrics(SM_CYScreen) - ih) div 2, iw, ih, SWP_NOZORDER);
      ShowWindow(FMainWnd, SW_SHOWNORMAL);
      BringWindowToTop(FMainWnd);
      SetForegroundWindow(FMainWnd);
    end else begin
      if not SystemParametersInfo(SPI_GETWORKAREA, 0, @r1, 0) then begin
        r1.Right  := GetSystemMetrics(SM_CXFullScreen);
        r1.Bottom := GetSystemMetrics(SM_CYFullScreen) + GetSystemMetrics(SM_CYCAPTION);
      end;
      SetWindowPos(FMainWnd, 0, r1.Right - iw - 2, r1.Bottom - ih - 2, iw, ih, SWP_NOZORDER or SWP_NOACTIVATE);
      ShowWindow(FMainWnd, SW_SHOWNOACTIVATE);
    end;
    SetPosition(FPosition);
  end;
end;

procedure TIProgressAlert.Close;
begin
  if FMainWnd <> 0 then begin
    if FBmpWnd <> 0 then
      SetWindowLong(FBmpWnd, GWL_USERDATA, 0);
    if GetWindowThreadProcessId(FMainWnd, nil) <> GetCurrentThreadId then begin
      SetWindowLong(FMainWnd, GWL_USERDATA, 1);
      SendMessage(FMainWnd, WM_SYSCOMMAND, SC_CLOSE, 0);
    end else
      DestroyWindow(FMainWnd);
    FMainWnd := 0;
    if FParentWindow <> 0 then begin
      EnableWindow(FParentWindow, true);
      SetActiveWindow(FParentWindow);
    end;
  end;
end;

function NewProgressAlert(title: string; bmpModule: dword = 0; bmpName: string = '') : IProgressAlert;
begin
  result := TIProgressAlert.Create(title, bmpModule, bmpName);
end;

// ***************************************************************

end.
