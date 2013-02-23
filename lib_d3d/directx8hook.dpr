library directx8hook;

uses
  SysUtils,
  Classes,
  advAPIhook,
  Windows,
  Direct3D8,
  D3DX8;

{$R *.res}

var
  D3DObj: IDirect3D8;
  D3DDev: IDirect3DDevice8;
  g_Font: ID3DXFont;

function GetInterfaceMethod(const intf; methodIndex: dword) : pointer;
begin
  result := pointer(pointer(dword(pointer(intf)^) + methodIndex * 4)^);
end;

var EndScene8Next : function (self: pointer): HResult stdcall = nil;
var CreateDevice8Next : function (self: Pointer; Adapter: LongWord; DeviceType: TD3DDevType; hFocusWindow: HWND; BehaviorFlags: DWord; pPresentationParameters: PD3DPresentParameters; out ppReturnedDeviceInterface: IDirect3DDevice8) : HRESULT stdcall = nil;
var Direct3DCreate8Next: function (SDKVersion: LongWord): DWORD stdcall = nil;

function EndScene8Callback(self: pointer): HResult; stdcall;
var
  TextRect: TRect;
begin
  TextRect := Rect(100,100,100,100);

  g_Font.DrawTextA(PChar('§±§â§Ú§Ó§Ö§ä'),1,TextRect,DT_LEFT or DT_NOCLIP, D3DCOLOR_RGBA($00, $ff, $ff, $ff));

  Result:=EndScene8Next(self);
end;

function CreateDevice8Callback(self: pointer; Adapter: LongWord; DeviceType: TD3DDevType; hFocusWindow: HWND; BehaviorFlags: DWord; pPresentationParameters: PD3DPresentParameters; out ppReturnedDeviceInterface: IDirect3DDevice8) : HRESULT; stdcall;
var
  F: TStrings;
  A: Integer;
begin
  result := CreateDevice8Next(self, adapter, DeviceType, hFocusWindow, BehaviorFlags, pPresentationParameters, ppReturnedDeviceInterface);
  D3DDev := ppReturnedDeviceInterface;
  if (result = 0) then
  begin

    A := D3DXCreateFont(D3DDev,100,g_Font);
      
   HookCode(GetInterfaceMethod(ppReturnedDeviceInterface, 42), @EndScene8Callback, @EndScene8Next);
  end;
end;

function Direct3DCreate8Callback(SDKVersion: LongWord): DWORD; stdcall;
var
  F: TStrings;
begin
  Result:=Direct3DCreate8Next(SDKVersion);
  D3DObj := IDirect3D8(Result);
  if (Result <> 0) then
  begin
    if (@CreateDevice8Next = nil) then
      UnhookCode(@CreateDevice8Next);
    HookCode(GetInterfaceMethod(result, 16), @CreateDevice8Callback, @CreateDevice8Next);
  end;
end;


begin
HookProc('d3d8.dll', 'Direct3DCreate8', @Direct3DCreate8Callback, @Direct3DCreate8Next);
  end.