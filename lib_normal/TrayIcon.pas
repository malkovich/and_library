{******************************************************************************
*              系统托盘图标控件 - TTrayIcon                                   *
*                                                                             *
*      功能： 添加图标到系统托盘(System Tray)，并响应相应鼠标事件             *
*      版本： V1.01                                                           *
*      作者： 顾中军                                                          *
*      日期:  2005.3.6                                                        *
*      用法：                                                                 *
*             很简单，试一下就知道，这里我就不多说了                          *
*      说明：                                                                 *
*          这个东东完全是照搬BCB6所带的TrayIcon例子中的做法，换句话说，实际上 *
*      我只是将BCB6的代码改成Delphi而已，并未作多少改进。BCB6源码请参看其安装 *
*      目录下Examples\Controls\Source子目录中的TrayIcon.cpp/TrayIcon.h 。     *
*          总的说来，这个东东要比网上流传甚广的TSysTray的功能要强大一些，而且 *
*      我发现它对弹出菜单的处理要更完善一些（TSysTray这个东东对弹出菜单的处理 *
*      有一些Bug）。                                                          *
*          好了，其他也没什么多说的了，让我们感谢一下Borland所提供的源码吧。  *
*          祝你愉快！！！                                                     *
*                                                                             *
*      Email:     iamdream@yeah.net                                           *
******************************************************************************}

unit TrayIcon;

interface

uses
  Windows, Messages, SysUtils, Controls, Classes, Forms, ExtCtrls, Graphics,
  Menus, ShellApi;

const
  WM_SYSTEM_TRAY_NOTIFY = WM_USER + 1;

type

  TTrayIconMessage = (imClick, imDoubleClick, imMouseDown,
                      imMouseUp, imLeftClickUp, imLeftDoubleClick,
                      imRightClickUp, imRightDoubleClick, imNone);

  TTrayIcon = class(TComponent)
  private
    { Private declarations }
    FData:            TNotifyIconData;
    FIsClicked:       Boolean;
    FIcon:            TIcon;
    FIconList:        TImageList;
    FPopupMenu:       TPopupMenu;
    FTimer:           TTimer;
    FHint:            String;
    FIconIndex:       Integer;
    FVisible:         Boolean;
    FHide:            Boolean;
    FAnimate:         Boolean;
    FAppRestore:      TTrayIconMessage;
    FPopupMenuShow:   TTrayIconMessage;
    FApplicationHook: TWindowHook;

    FOnMinimize:      TNotifyEvent;
    FOnRestore:       TNotifyEvent;
    FOnMouseMove:     TMouseMoveEvent;
    FOnMouseExit:     TMouseMoveEvent;
    FOnMouseEnter:    TMouseMoveEvent;
    FOnClick:         TNotifyEvent;
    FOnDblClick:      TNotifyEvent;
    FOnMouseDown:     TMouseEvent;
    FOnMouseUp:       TMouseEvent;
    FOnAnimate:       TNotifyEvent;
    FOnCreate:        TNotifyEvent;
    FOnDestroy:       TNotifyEvent;
    FOnActivate:      TNotifyEvent;
    FOnDeactivate:    TNotifyEvent;

    procedure SetHint(Hint: String);
    procedure SetHide(Value: Boolean);
    function GetAnimateInterval: Integer;
    procedure SetAnimateInterval(Value: Integer);
    function GetAnimate: Boolean;
    procedure SetAnimate(Value: Boolean);
    procedure EndSession;
    function ShiftState: TShiftState;
    function GetHandle: HWND;

  protected
    { Protected declarations }
    procedure SetVisible(Value: Boolean); virtual;
    procedure DoMessage(var Message: TMessage); virtual;
    procedure DoClick; virtual;
    procedure DoDblClick; virtual;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer); virtual;
    procedure DoOnAnimate(Sender: TObject); virtual;
    procedure Notification(AComponent: TComponent;
        Operation: TOperation); override;

    function ApplicationHookProc(var Message: TMessage): Boolean;

    procedure Loaded; override;
    property Data: TNotifyIconData read FData;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Minimize; virtual;
    procedure Restore; virtual;
    procedure Update; virtual;
    procedure ShowMenu; virtual;
    procedure SetIconIndex(Value: Integer); virtual;
    procedure SetDefaultIcon; virtual;

  published
    { Published declarations }
    property Visible: Boolean read FVisible write SetVisible default False;
    property Hint: String     read FHint    write SetHint;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
    property Hide: Boolean    read FHide    write SetHide;
    property RestoreOn:   TTrayIconMessage  read FAppRestore write FAppRestore;
    property PopupMenuOn: TTrayIconMessage  read FPopupMenuShow write FPopupMenuShow;
    property Icons: TImageList  read FIconList  write FIconList;
    property IconIndex: Integer read FIconIndex write FIconIndex;
    property AnimateInterval: Integer read GetAnimateInterval write SetAnimateInterval default 1000;
    property Animate: Boolean   read GetAnimate write SetAnimate default False;
    property Handle:  HWND      read GetHandle;

    property OnMinimize:   TNotifyEvent read FOnMinimize write FOnMinimize;
    property OnRestore:    TNotifyEvent read FOnRestore  write FOnRestore;
    property OnClick:      TNotifyEvent read FOnClick    write FOnClick;
    property OnMouseEnter: TMouseMoveEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit:  TMouseMoveEvent read FOnMouseExit  write FOnMouseExit;
    property OnMouseMove:  TMouseMoveEvent read FOnMouseMove  write FOnMouseMove;
    property OnMouseUp:    TMouseEvent  read FOnMouseUp  write FOnMouseUp;
    property OnMouseDown:  TMouseEvent  read FOnMouseDown write FOnMouseDown;
    property OnAnimate:    TNotifyEvent read FOnAnimate  write FOnAnimate;
    property OnCreate:     TNotifyEvent read FOnCreate   write FOnCreate;
    property OnDestroy:    TNotifyEvent read FOnDestroy  write FOnDestroy;
    property OnActivate:   TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
     
  end;

procedure Register;

resourcestring
  sCannotCreate = 'Cannot Create System Shell Notification Icon';
  sCannotRemove = 'Cannot Remove System Shell Notification Icon';

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TTrayIcon]);
end;

constructor TTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIcon  := TIcon.Create();
  FTimer := TTimer.Create(Self);

  FIconIndex      := 0;
  FIcon.Assign(Application.Icon);
  FAppRestore     := imDoubleClick;
  FOnAnimate      := DoOnAnimate;
  FPopupMenuShow  := imNone;
  FVisible        := False;
  FHide           := True;
  FTimer.Enabled  := False;
  FTimer.OnTimer  := OnAnimate;
  FTimer.Interval := 1000;

  if not (csDesigning in ComponentState) then
  begin
    FillChar(FData, SizeOf(TNotifyIconData), 0);
    with FData do
    begin
      cbSize := SizeOf(TNotifyIconData);
      Wnd    := Classes.AllocateHWnd(DoMessage);
      uID    := UINT(Self);
      hIcon  := FIcon.Handle;
      uFlags := NIF_ICON or NIF_MESSAGE;
      uCallbackMessage := WM_SYSTEM_TRAY_NOTIFY;
    end;
    FApplicationHook   := ApplicationHookProc;
    Update();
  end;
end;

destructor TTrayIcon.Destroy;
begin
  if not (csDesigning in ComponentState) then
  begin
    Shell_NotifyIcon(NIM_DELETE, @FData);
    Classes.DeallocateHWnd(FData.Wnd);
  end;

  if FIcon <> nil then
    FIcon.Free;
  if FTimer <> nil then
    FTimer.Free;

  inherited Destroy;
end;

procedure TTrayIcon.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    if AComponent = FIconList then
      FIconList := nil
    else if AComponent = FPopupMenu then
      FPopupMenu := nil;
  end;
end;

procedure TTrayIcon.Loaded;
begin
  inherited Loaded;

  if FIconList = nil then
  begin
    FAnimate := False;
    FIcon.Assign(Application.Icon);
  end
  else
  begin
    FTimer.Enabled := FAnimate;
    FIconList.GetIcon(FIconIndex, FIcon);
  end;

  Update();
end;

procedure TTrayIcon.SetVisible(Value: Boolean);
begin
  FVisible := Value;

  if not (csDesigning in ComponentState) then
  begin
    if FVisible then
    begin
      if not Shell_NotifyIcon(NIM_ADD, @FData) then
        raise EOutOfResources.Create(sCannotCreate);
      Hide := True;
      Application.HookMainWindow(FApplicationHook);
    end
    else
    begin
      if not Shell_NotifyIcon(NIM_DELETE, @FData) then
        raise EOutOfResources.Create(sCannotRemove);
      Hide := False;
      Application.UnhookMainWindow(FApplicationHook);
    end;
  end;
end;

procedure TTrayIcon.SetHint(Hint: String);
begin
  // The new hint must be different than the previous hint and less than
  // 64 characters to be modified. 64 is an operating system limit.
  if (FHint <> Hint) and (Length(Hint) < 64) then
  begin
    FHint := Hint;
    StrPLCopy(FData.szTip, Hint, SizeOf(FData.szTip) - 1);

    // If there is no hint then there is no tool tip.
    if Length(Hint) > 0 then
      FData.uFlags := FData.uFlags or NIF_TIP
    else
      FData.uFlags := FData.uFlags and not NIF_TIP;

    Update();
  end;
end;

procedure TTrayIcon.SetHide(Value: Boolean);
begin
  FHide := Value;
  if FVisible then
  begin
    if IsIconic(Application.Handle) then
    begin
      if Value then
        ShowWindow(Application.Handle, SW_HIDE);
    end
    else if not Value then
      ShowWindow(Application.Handle, SW_RESTORE);
  end;
end;

function TTrayIcon.GetAnimateInterval: Integer;
begin
  Result := FTimer.Interval;
end;

procedure TTrayIcon.SetAnimateInterval(Value: Integer);
begin
  FTimer.Interval := Value;
end;

function TTrayIcon.GetAnimate: Boolean;
begin
  Result := FAnimate;
end;

procedure TTrayIcon.SetAnimate(Value: Boolean);
begin
  if (FIconList <> nil) or (csLoading in ComponentState) then
    FAnimate := Value;

  if (FIconList <> nil) and not(csDesigning in ComponentState) then
    FTimer.Enabled := Value;
end;

procedure TTrayIcon.EndSession;
begin
  Shell_NotifyIcon(NIM_DELETE, @FData);
end;

function TTrayIcon.ShiftState: TShiftState;
begin
  if GetKeyState(VK_SHIFT) < 0 then
    Include(Result, ssShift);
  if GetKeyState(VK_CONTROL) < 0 then
    Include(Result, ssCtrl);
  if GetKeyState(VK_MENU) < 0 then
    Include(Result, ssAlt);
end;

procedure TTrayIcon.DoMessage(var Message: TMessage);
var
  APoint: TPoint;
  Shift:  TShiftState;
begin
  case Message.Msg of
    WM_QUERYENDSESSION: Message.Result := 1;
    WM_ENDSESSION:      EndSession;
    WM_SYSTEM_TRAY_NOTIFY:
      begin
        case Message.LParam of
          WM_MOUSEMOVE:
            if Assigned(FOnClick) then
            begin
              Shift := ShiftState();
              GetCursorPos(APoint);
              DoMouseMove(Shift, APoint.X, APoint.Y);
            end;
          WM_LBUTTONDOWN:
            begin
              Shift := ShiftState();
              Include(Shift, ssLeft);
              GetCursorPos(APoint);
              DoMouseDown(mbLeft, Shift, APoint.X, APoint.Y);
              FIsClicked := True;
            end;
          WM_LBUTTONUP:
            begin
              Shift := ShiftState();
              Include(Shift, ssLeft);
              GetCursorPos(APoint);

              if Assigned(FOnClick) then
                DoClick();

              DoMouseUp(mbLeft, Shift, APoint.X, APoint.Y);

              if FAppRestore = imLeftClickUp then
                Restore();
              if FPopupMenuShow = imLeftClickUp then
                ShowMenu();
            end;
          WM_LBUTTONDBLCLK:
            begin
              DoDblClick();

              if FAppRestore = imLeftDoubleClick then
                Restore();
              if FPopupMenuShow = imLeftDoubleClick then
                ShowMenu();
            end;
          WM_RBUTTONDOWN:
            begin
              Shift := ShiftState();
              Include(Shift, ssRight);
              GetCursorPos(APoint);
              DoMouseDown(mbRight, Shift, APoint.X, APoint.Y);
            end;
          WM_RBUTTONUP:
            begin
              Shift := ShiftState();
              Include(Shift, ssRight);
              GetCursorPos(APoint);

              DoMouseUp(mbRight, Shift, APoint.X, APoint.Y);

              if FAppRestore = imRightClickUp then
                Restore();
              if FPopupMenuShow = imRightClickUp then
                ShowMenu();
            end;
          WM_RBUTTONDBLCLK:
            begin
              DoDblClick();

              if FAppRestore = imRightDoubleClick then
                Restore();
              if FPopupMenuShow = imRightDoubleClick then
                ShowMenu();
            end;
          WM_MBUTTONDOWN:
            begin
              Shift := ShiftState();
              Include(Shift, ssMiddle);
              GetCursorPos(APoint);

              DoMouseDown(mbMiddle, Shift, APoint.X, APoint.Y);
            end;
          WM_MBUTTONUP:
            begin
              Shift := ShiftState();
              Include(Shift, ssMiddle);
              GetCursorPos(APoint);

              DoMouseUp(mbMiddle, Shift, APoint.X, APoint.Y);
            end;
          WM_MBUTTONDBLCLK:
            begin
              DoDblClick();
            end;
        end;
      end;
  end;
  inherited Dispatch(Message);
end;

procedure TTrayIcon.ShowMenu;
var
  APoint: TPoint;
begin
  GetCursorPos(APoint);

  if (Screen.ActiveForm <> nil) and (Screen.ActiveForm.Handle <> 0) then
    SetForegroundWindow(Screen.ActiveForm.Handle);

  FPopupMenu.Popup(APoint.X, APoint.Y);
end;

procedure TTrayIcon.DoClick;
begin
  if FAppRestore = imClick then
    Restore();
  if FPopupMenuShow = imClick then
    ShowMenu();

  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TTrayIcon.DoDblClick;
begin
  if FAppRestore = imDoubleClick then
    Restore();
  if FPopupMenuShow = imDoubleClick then
    ShowMenu();

  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure TTrayIcon.DoMouseMove(Shift: TShiftState; X, Y: Integer);
begin
 if Assigned(FOnMouseMove) then
   FOnMouseMove(Self, Shift, X, Y);
end;

procedure TTrayIcon.DoMouseDown(Button: TMouseButton; Shift: TShiftState;
                                X, Y: Integer);
begin
  if FAppRestore = imMouseDown then
    Restore();
  if FPopupMenuShow = imMouseDown then
    ShowMenu();

  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TTrayIcon.DoMouseUp(Button: TMouseButton; Shift: TShiftState;
                                X, Y: Integer);
begin
  if FAppRestore = imMouseUp then
    Restore();
  if FPopupMenuShow = imMouseUp then
    ShowMenu();

  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TTrayIcon.DoOnAnimate(Sender: TObject);
begin
  if IconIndex < FIconList.Count - 1 then
    Inc(FIconIndex)
  else
    FIconIndex := 0;

  SetIconIndex(FIconIndex);
  Update();
end;

//---------------------------------------------------------------------------
// When the application minimizes, hide it, so only the icon in the system
// tray is visible.
//---------------------------------------------------------------------------
procedure TTrayIcon.Minimize;
begin
  Application.Minimize();
  if FHide then
    ShowWindow(Application.Handle, SW_HIDE);

  if Assigned(FOnMinimize) then
    FOnMinimize(Self);
end;

//---------------------------------------------------------------------------
// Restore the application by making its window visible again, which is a
// little weird since its window is invisible, having no height or width, but
// that's what determines whether the button appears on the taskbar.
//---------------------------------------------------------------------------
procedure TTrayIcon.Restore;
begin
  Application.Restore();
  ShowWindow(Application.Handle, SW_RESTORE);
  SetForegroundWindow(Application.Handle);

  if Assigned(FOnRestore) then
    FOnRestore(Self);
end;

procedure TTrayIcon.Update;
begin
  if not (csDesigning in ComponentState) then
  begin
    FData.hIcon := FIcon.Handle;

    if Visible then
      Shell_NotifyIcon(NIM_MODIFY, @FData);
  end;
end;

procedure TTrayIcon.SetIconIndex(Value: Integer);
begin
  FIconIndex := Value;

  if FIconList <> nil then
    FIconList.GetIcon(FIconIndex, FIcon);

  Update();
end;

function TTrayIcon.ApplicationHookProc(var Message: TMessage): Boolean;
begin
  Result := False;

  if Message.Msg = WM_SYSCOMMAND then
  begin
    if Message.WParam = SC_MINIMIZE then
      Minimize()
    else if Message.Msg = SC_RESTORE then
      Restore();
  end;
end;

procedure TTrayIcon.SetDefaultIcon;
begin
  FIcon.Assign(Application.Icon);
  Update();
end;

function TTrayIcon.GetHandle: HWND;
begin
  Result := FData.Wnd;
end;

end.
