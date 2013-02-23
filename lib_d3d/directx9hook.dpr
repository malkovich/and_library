library directx9hook;

uses
  SysUtils,
  Classes,
  advAPIHook,
  Windows,
  Direct3D9,
  D3DX9;

{$R *.res}

var
  D3DObj: IDirect3D9;
  D3DDev: IDirect3DDevice9;
  g_Font: ID3DXFont;

function GetInterfaceMethod(const intf; methodIndex: dword) : pointer;
begin
  result := pointer(pointer(dword(pointer(intf)^) + methodIndex * 4)^);
end;

var EndScene9Next : function (self: pointer): HResult stdcall = nil;
var CreateDevice9Next : function (self: Pointer; Adapter: LongWord; DeviceType: TD3DDevType; hFocusWindow: HWND; BehaviorFlags: DWord; pPresentationParameters: PD3DPresentParameters; out ppReturnedDeviceInterface: IDirect3DDevice9) : HRESULT stdcall = nil;
var Direct3DCreate9Next: function (SDKVersion: LongWord): DWORD stdcall = nil;

function EndScene9Callback(self: pointer): HResult; stdcall;
var
  TextRect: TRect;
begin
  TextRect := Rect(100,100,100,100);

  g_Font.DrawTextA(
    nil,
    PChar('§±§â§Ú§Ó§Ö§ä'),
    -1,
    @TextRect,
    DT_LEFT or DT_NOCLIP,
    D3DCOLOR_RGBA($00, $ff, $ff, $ff)
  );

  Result:=EndScene9Next(self);
end;

function CreateDevice9Callback(self: pointer; Adapter: LongWord; DeviceType: TD3DDevType; hFocusWindow: HWND; BehaviorFlags: DWord; pPresentationParameters: PD3DPresentParameters; out ppReturnedDeviceInterface: IDirect3DDevice9) : HRESULT; stdcall;
var
  F: TStrings;
  A: Integer;
begin
  result := CreateDevice9Next(self, adapter, DeviceType, hFocusWindow, BehaviorFlags, pPresentationParameters, ppReturnedDeviceInterface);
  D3DDev := ppReturnedDeviceInterface;
  if (result = 0) then
  begin
    A := D3DXCreateFont(
      D3DDev,
      100,
      0,
      FW_BOLD,
      1,
      false,
      DEFAULT_CHARSET,
      OUT_DEFAULT_PRECIS,
      ANTIALIASED_QUALITY,
      DEFAULT_PITCH or FF_DONTCARE,
      PChar('Arial'),
      g_Font
    );

    HookCode(GetInterfaceMethod(ppReturnedDeviceInterface{^}, 42), @EndScene9Callback, @EndScene9Next);
  end;
end;

function Direct3DCreate9Callback(SDKVersion: LongWord): DWORD; stdcall;
var
  F: TStrings;
begin
  Result:=Direct3DCreate9Next(SDKVersion);
  D3DObj := IDirect3D9(Result);
  if (Result <> 0) then
  begin
    if (@CreateDevice9Next = nil) then
      UnhookCode(@CreateDevice9Next);
    HookCode(GetInterfaceMethod(result, 16), @CreateDevice9Callback, @CreateDevice9Next);
  end;
end;


begin
  HookProc('d3d9.dll', 'Direct3DCreate9', @Direct3DCreate9Callback, @Direct3DCreate9Next);
end.