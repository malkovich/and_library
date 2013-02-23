unit D3DHook;

interface
uses  Windows, SysUtils, Direct3D8, D3DX8, Classes;

Type
  TMyD3DObject = Class(TInterfacedObject, IDirect3D8)
  public
    function RegisterSoftwareDevice(pInitializeFunction: Pointer): HResult; stdcall;
    function GetAdapterCount: LongWord; stdcall;
    function GetAdapterIdentifier(Adapter: LongWord; Flags: DWord; out pIdentifier: TD3DAdapterIdentifier8): HResult; stdcall;
    function GetAdapterModeCount (Adapter: LongWord): LongWord; stdcall;
    function EnumAdapterModes(Adapter, Mode: LongWord; out pMode: TD3DDisplayMode): HResult; stdcall;
    function GetAdapterDisplayMode(Adapter: LongWord; out pMode: TD3DDisplayMode): HResult; stdcall;
    function CheckDeviceType(Adapter: LongWord; CheckType: TD3DDevType; DisplayFormat, BackBufferFormat: TD3DFormat; Windowed: BOOL): HResult; stdcall;
    function CheckDeviceFormat(Adapter: LongWord; DeviceType: TD3DDevType; AdapterFormat: TD3DFormat; Usage: DWord; RType: TD3DResourceType; CheckFormat: TD3DFormat): HResult; stdcall;
    function CheckDeviceMultiSampleType(Adapter: LongWord; DeviceType: TD3DDevType; SurfaceFormat: TD3DFormat; Windowed: BOOL; MultiSampleType: TD3DMultiSampleType): HResult; stdcall;
    function CheckDepthStencilMatch(Adapter: LongWord; DeviceType: TD3DDevType; AdapterFormat, RenderTargetFormat, DepthStencilFormat: TD3DFormat): HResult; stdcall;
    function GetDeviceCaps(Adapter: LongWord; DeviceType: TD3DDevType; out pCaps: TD3DCaps8): HResult; stdcall;
    function GetAdapterMonitor(Adapter: LongWord): HMONITOR; stdcall;
    function CreateDevice(Adapter: LongWord; DeviceType: TD3DDevType; hFocusWindow: HWND; BehaviorFlags: DWord; var pPresentationParameters: TD3DPresentParameters; out ppReturnedDeviceInterface: IDirect3DDevice8): HResult; stdcall;
  public
    RealEntry: IDirect3D8;
    Index: Integer;
  end;

  TMyD3DDevice = Class (TInterfacedObject, IDirect3DDevice8)
  public
    function TestCooperativeLevel: HResult; stdcall;
    function GetAvailableTextureMem: LongWord; stdcall;
    function ResourceManagerDiscardBytes(Bytes: DWord): HResult; stdcall;
    function GetDirect3D(out ppD3D8: IDirect3D8): HResult; stdcall;
    function GetDeviceCaps(out pCaps: TD3DCaps8): HResult; stdcall;
    function GetDisplayMode(out pMode: TD3DDisplayMode): HResult; stdcall;
    function GetCreationParameters(out pParameters: TD3DDeviceCreationParameters): HResult; stdcall;
    function SetCursorProperties(XHotSpot, YHotSpot: LongWord; pCursorBitmap: IDirect3DSurface8): HResult; stdcall;
    procedure SetCursorPosition(XScreenSpace, YScreenSpace: Integer; Flags: DWord); stdcall;
    function ShowCursor(bShow: BOOL): BOOL; stdcall;
    function CreateAdditionalSwapChain(const pPresentationParameters: TD3DPresentParameters; out pSwapChain: IDirect3DSwapChain8): HResult; stdcall;
    function Reset(const pPresentationParameters: TD3DPresentParameters): HResult; stdcall;
    function Present(pSourceRect, pDestRect: PRect; hDestWindowOverride: HWND; pDirtyRegion: PRgnData): HResult; stdcall;
    function GetBackBuffer(BackBuffer: LongWord; _Type: TD3DBackBufferType; out ppBackBuffer: IDirect3DSurface8): HResult; stdcall;
    function GetRasterStatus(out pRasterStatus: TD3DRasterStatus): HResult; stdcall;
    procedure SetGammaRamp(Flags: DWord; const pRamp: TD3DGammaRamp); stdcall;
    procedure GetGammaRamp(out pRamp: TD3DGammaRamp); stdcall;
    function CreateTexture(Width, Height, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool; out ppTexture: IDirect3DTexture8): HResult; stdcall;
    function CreateVolumeTexture(Width, Height, Depth, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool; out ppVolumeTexture: IDirect3DVolumeTexture8): HResult; stdcall;
    function CreateCubeTexture(EdgeLength, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool; out ppCubeTexture: IDirect3DCubeTexture8): HResult; stdcall;
    function CreateVertexBuffer(Length: LongWord; Usage, FVF: DWord; Pool: TD3DPool; out ppVertexBuffer: IDirect3DVertexBuffer8): HResult; stdcall;
    function CreateIndexBuffer(Length: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool; out ppIndexBuffer: IDirect3DIndexBuffer8): HResult; stdcall;
    function CreateRenderTarget(Width, Height: LongWord; Format: TD3DFormat; MultiSample: TD3DMultiSampleType; Lockable: BOOL; out ppSurface: IDirect3DSurface8): HResult; stdcall;
    function CreateDepthStencilSurface(Width, Height: LongWord; Format: TD3DFormat; MultiSample: TD3DMultiSampleType; out ppSurface: IDirect3DSurface8): HResult; stdcall;
    function CreateImageSurface(Width, Height: LongWord; Format: TD3DFormat; out ppSurface: IDirect3DSurface8): HResult; stdcall;
    function CopyRects(pSourceSurface: IDirect3DSurface8; pSourceRectsArray: PRect; cRects: LongWord; pDestinationSurface: IDirect3DSurface8; pDestPointsArray: PPoint): HResult; stdcall;
    function UpdateTexture(pSourceTexture, pDestinationTexture: IDirect3DBaseTexture8): HResult; stdcall;
    function GetFrontBuffer(pDestSurface: IDirect3DSurface8): HResult; stdcall;
    function SetRenderTarget(pRenderTarget, pNewZStencil: IDirect3DSurface8): HResult; stdcall;
    function GetRenderTarget(out ppRenderTarget: IDirect3DSurface8): HResult; stdcall;
    function GetDepthStencilSurface(out ppZStencilSurface: IDirect3DSurface8): HResult; stdcall;
    function BeginScene: HResult; stdcall;
    function EndScene: HResult; stdcall;
    function Clear(Count: DWord; pRects: PD3DRect; Flags: DWord; Color: TD3DColor; Z: Single; Stencil: DWord): HResult; stdcall;
    function SetTransform(State: TD3DTransformStateType; const pMatrix: TD3DMatrix): HResult; stdcall;
    function GetTransform(State: TD3DTransformStateType; out pMatrix: TD3DMatrix): HResult; stdcall;
    function MultiplyTransform(State: TD3DTransformStateType; const pMatrix: TD3DMatrix): HResult; stdcall;
    function SetViewport(const pViewport: TD3DViewport8): HResult; stdcall;
    function GetViewport(out pViewport: TD3DViewport8): HResult; stdcall;
    function SetMaterial(const pMaterial: TD3DMaterial8): HResult; stdcall;
    function GetMaterial(out pMaterial: TD3DMaterial8): HResult; stdcall;
    function SetLight(Index: DWord; const pLight: TD3DLight8): HResult; stdcall;
    function GetLight(Index: DWord; out pLight: TD3DLight8): HResult; stdcall;
    function LightEnable(Index: DWord; Enable: BOOL): HResult; stdcall;
    function GetLightEnable(Index: DWord; out pEnable: BOOL): HResult; stdcall;
    function SetClipPlane(Index: DWord; pPlane: PSingle): HResult; stdcall;
    function GetClipPlane(Index: DWord; pPlane: PSingle): HResult; stdcall;
    function SetRenderState(State: TD3DRenderStateType; Value: DWord): HResult; stdcall;
    function GetRenderState(State: TD3DRenderStateType; out pValue: DWord): HResult; stdcall;
    function BeginStateBlock: HResult; stdcall;
    function EndStateBlock(out pToken: DWord): HResult; stdcall;
    function ApplyStateBlock(Token: DWord): HResult; stdcall;
    function CaptureStateBlock(Token: DWord): HResult; stdcall;
    function DeleteStateBlock(Token: DWord): HResult; stdcall;
    function CreateStateBlock(_Type: TD3DStateBlockType; out Token: DWord): HResult; stdcall;
    function SetClipStatus(const pClipStatus: TD3DClipStatus8): HResult; stdcall;
    function GetClipStatus(out pClipStatus: TD3DClipStatus8): HResult; stdcall;
    function GetTexture(Stage: DWord; out ppTexture: IDirect3DBaseTexture8): HResult; stdcall;
    function SetTexture(Stage: DWord; pTexture: IDirect3DBaseTexture8): HResult; stdcall;
    function GetTextureStageState(Stage: DWord; _Type: TD3DTextureStageStateType; out pValue: DWord): HResult; stdcall;
    function SetTextureStageState(Stage: DWord; _Type: TD3DTextureStageStateType; Value: DWord): HResult; stdcall;
    function ValidateDevice(out pNumPasses: DWord): HResult; stdcall;
    function GetInfo(DevInfoID: DWord; out pDevInfoStruct; DevInfoStructSize: DWord): HResult; stdcall;
    function SetPaletteEntries(PaletteNumber: LongWord; pEntries: pPaletteEntry): HResult; stdcall;
    function GetPaletteEntries(PaletteNumber: LongWord; pEntries: pPaletteEntry): HResult; stdcall;
    function SetCurrentTexturePalette(PaletteNumber: LongWord): HResult; stdcall;
    function GetCurrentTexturePalette(out PaletteNumber: LongWord): HResult; stdcall;
    function DrawPrimitive(PrimitiveType: TD3DPrimitiveType; StartVertex, PrimitiveCount: LongWord): HResult; stdcall;
    function DrawIndexedPrimitive(_Type: TD3DPrimitiveType; minIndex, NumVertices, startIndex, primCount: LongWord): HResult; stdcall;
    function DrawPrimitiveUP(PrimitiveType: TD3DPrimitiveType; PrimitiveCount: LongWord; const pVertexStreamZeroData; VertexStreamZeroStride: LongWord): HResult; stdcall;
    function DrawIndexedPrimitiveUP(PrimitiveType: TD3DPrimitiveType; MinVertexIndex, NumVertexIndices, PrimitiveCount: LongWord; const pIndexData; IndexDataFormat: TD3DFormat; const pVertexStreamZeroData; VertexStreamZeroStride: LongWord): HResult; stdcall;
    function ProcessVertices(SrcStartIndex, DestIndex, VertexCount: LongWord; pDestBuffer: IDirect3DVertexBuffer8; Flags: DWord): HResult; stdcall;
    function CreateVertexShader(pDeclaration, pFunction: PDWord; out pHandle: DWord; Usage: DWord): HResult; stdcall;
    function SetVertexShader(Handle: DWord): HResult; stdcall;
    function GetVertexShader(out pHandle: DWord): HResult; stdcall;
    function DeleteVertexShader(Handle: DWord): HResult; stdcall;
    function SetVertexShaderConstant(_Register: DWord; const pConstantData; ConstantCount: DWord): HResult; stdcall;
    function GetVertexShaderConstant(_Register: DWord; out pConstantData; ConstantCount: DWord): HResult; stdcall;
    function GetVertexShaderDeclaration(Handle: DWord; pData: Pointer; out pSizeOfData: DWord): HResult; stdcall;
    function GetVertexShaderFunction(Handle: DWord; pData: Pointer; out pSizeOfData: DWord): HResult; stdcall;
    function SetStreamSource(StreamNumber: LongWord; pStreamData: IDirect3DVertexBuffer8; Stride: LongWord): HResult; stdcall;
    function GetStreamSource(StreamNumber: LongWord; out ppStreamData: IDirect3DVertexBuffer8; out pStride: LongWord): HResult; stdcall;
    function SetIndices(pIndexData: IDirect3DIndexBuffer8; BaseVertexIndex: LongWord): HResult; stdcall;
    function GetIndices(out ppIndexData: IDirect3DIndexBuffer8; out pBaseVertexIndex: LongWord): HResult; stdcall;
    function CreatePixelShader(pFunction: PDWord; out pHandle: DWord): HResult; stdcall;
    function SetPixelShader(Handle: DWord): HResult; stdcall;
    function GetPixelShader(out Handle: DWord): HResult; stdcall;
    function DeletePixelShader(Handle: DWord): HResult; stdcall;
    function SetPixelShaderConstant(_Register: DWord; const pConstantData; ConstantCount: DWord): HResult; stdcall;
    function GetPixelShaderConstant(_Register: DWord; out pConstantData; ConstantCount: DWord): HResult; stdcall;
    function GetPixelShaderFunction(Handle: DWord; pData: Pointer; var pSizeOfData: DWord): HResult; stdcall;
    function DrawRectPatch(Handle: LongWord; pNumSegs: PSingle; pTriPatchInfo: PD3DRectPatchInfo): HResult; stdcall;
    function DrawTriPatch(Handle: LongWord; pNumSegs: PSingle; pTriPatchInfo: PD3DTriPatchInfo): HResult; stdcall;
    function DeletePatch(Handle: LongWord): HResult; stdcall;
  public
    FatherD3D: TMyD3DObject;
    RealDevice: IDirect3DDevice8;
    Index: Integer;
  end;                                     

var
  ProxyD3DObject: array of TMyD3DObject;
  ProxyD3DDevice: array of TMyD3DDevice;

procedure TriggerD3DHook (IsView: BOOL = True);

implementation

uses CmdHandler, D3DHookLogic, TrapApiHook;

////////////////////////////////////////////////////////////////////////////////
var
  Direct3DCreate8: function (SDKVersion: LongWord): DWORD stdcall;

function Hook_Direct3DCreate8(SDKVersion: LongWord): DWORD; stdcall;
var
  Index: Integer;
begin
  Index := High (ProxyD3DObject) + 1;
  SetLength (ProxyD3DObject, Index + 1);
  ProxyD3DObject [Index] := TMyD3DObject.Create;
  ProxyD3DObject [Index].RealEntry := IDirect3D8 (Direct3DCreate8(SDKVersion));
  ProxyD3DObject [Index].Index := Index;

  Result := DWORD (IDirect3D8(ProxyD3DObject [Index]));

  DBG ('Direct3DCreate8 Done [Index=%d]', [Index]);
end;


var
  BeenInitialed: Bool = false;

procedure TriggerD3DHook (IsView: BOOL = True);
var
  hD3d8: LongWord;
begin
  TrigerTime := GetTickCount;
  IsViewText := IsView;

  if BeenInitialed then exit;

  BeenInitialed := True;
  hD3d8 := Windows.LoadLibraryA ('d3d8.dll');
  Direct3DCreate8 := SMHookApi (GetProcAddress(hD3d8, 'Direct3DCreate8'), @Hook_Direct3DCreate8);
end;

////////////////////////////////////////////////////////////////////////////////

function TMyD3DObject.RegisterSoftwareDevice(pInitializeFunction: Pointer): HResult; stdcall;
begin
  Result := RealEntry.RegisterSoftwareDevice(pInitializeFunction);
end;

function TMyD3DObject.GetAdapterCount: LongWord; stdcall;
begin
  Result := RealEntry.GetAdapterCount;
end;

function TMyD3DObject.GetAdapterIdentifier(Adapter: LongWord; Flags: DWord; out pIdentifier: TD3DAdapterIdentifier8): HResult; stdcall;
begin
  Result := RealEntry.GetAdapterIdentifier (Adapter, Flags, pIdentifier);
end;      

function TMyD3DObject.GetAdapterModeCount (Adapter: LongWord): LongWord; stdcall;
begin
  Result := RealEntry.GetAdapterModeCount (Adapter);
end;

function TMyD3DObject.EnumAdapterModes(Adapter, Mode: LongWord; out pMode: TD3DDisplayMode): HResult; stdcall;
begin
  Result := RealEntry.EnumAdapterModes (Adapter, Mode, pMode);
end;

function TMyD3DObject.GetAdapterDisplayMode(Adapter: LongWord; out pMode: TD3DDisplayMode): HResult; stdcall;
begin
  Result := RealEntry.GetAdapterDisplayMode (Adapter, pMode);
end;

function TMyD3DObject.CheckDeviceType(Adapter: LongWord; CheckType: TD3DDevType; DisplayFormat, BackBufferFormat: TD3DFormat; Windowed: BOOL): HResult; stdcall;
begin
  Result := RealEntry.CheckDeviceType (Adapter, CheckType, DisplayFormat, BackBufferFormat, Windowed);
end;

function TMyD3DObject.CheckDeviceFormat(Adapter: LongWord; DeviceType: TD3DDevType; AdapterFormat: TD3DFormat; Usage: DWord; RType: TD3DResourceType; CheckFormat: TD3DFormat): HResult; stdcall;
begin
  Result := RealEntry.CheckDeviceFormat (Adapter, DeviceType, AdapterFormat, Usage, RType, CheckFormat);
end;

function TMyD3DObject.CheckDeviceMultiSampleType(Adapter: LongWord; DeviceType: TD3DDevType; SurfaceFormat: TD3DFormat; Windowed: BOOL; MultiSampleType: TD3DMultiSampleType): HResult; stdcall;
begin
  Result := RealEntry.CheckDeviceMultiSampleType (Adapter, DeviceType, SurfaceFormat, Windowed, MultiSampleType);
end;

function TMyD3DObject.CheckDepthStencilMatch(Adapter: LongWord; DeviceType: TD3DDevType; AdapterFormat, RenderTargetFormat, DepthStencilFormat: TD3DFormat): HResult; stdcall;
begin
  Result := RealEntry.CheckDepthStencilMatch (Adapter, DeviceType, AdapterFormat, RenderTargetFormat, DepthStencilFormat);
end;

function TMyD3DObject.GetDeviceCaps(Adapter: LongWord; DeviceType: TD3DDevType; out pCaps: TD3DCaps8): HResult; stdcall;
begin
  Result := RealEntry.GetDeviceCaps (Adapter, DeviceType, pCaps);
end;

function TMyD3DObject.GetAdapterMonitor(Adapter: LongWord): HMONITOR; stdcall;
begin
  Result := RealEntry.GetAdapterMonitor (Adapter);
end;

function TMyD3DObject.CreateDevice(Adapter: LongWord; DeviceType: TD3DDevType; hFocusWindow: HWND; BehaviorFlags: DWord; var pPresentationParameters: TD3DPresentParameters; out ppReturnedDeviceInterface: IDirect3DDevice8): HResult; stdcall;
var
  _Index: Integer;
  DeviceInterface: IDirect3DDevice8;
begin
  pPresentationParameters.BackBufferCount := 0;
  pPresentationParameters.AutoDepthStencilFormat := D3DFMT_D16;
  pPresentationParameters.MultiSampleType  := D3DMULTISAMPLE_NONE;
  pPresentationParameters.SwapEffect := D3DSWAPEFFECT_DISCARD;

  Result := RealEntry.CreateDevice (Adapter, DeviceType, hFocusWindow, BehaviorFlags, pPresentationParameters, DeviceInterface);

  _Index := High (ProxyD3DDevice) + 1;
  SetLength (ProxyD3DDevice, _Index + 1);
  ProxyD3DDevice [_Index] := TMyD3DDevice.Create;
  ProxyD3DDevice [_Index].Index := _Index;
  ProxyD3DDevice [_Index].FatherD3D := Self;
  ProxyD3DDevice [_Index].RealDevice := DeviceInterface;

  ppReturnedDeviceInterface := ProxyD3DDevice [_Index];
  DBG ('CreateDevice [Index=%d DeviceIndex=%d]', [Index, _Index]);
end;

////////////////////////////////////////////////////////////////////////////////
///
///                           D3D 设备接口
///
////////////////////////////////////////////////////////////////////////////////

function TMyD3DDevice.TestCooperativeLevel: HResult; stdcall;
begin
  Result := RealDevice.TestCooperativeLevel;
end;
function TMyD3DDevice.GetAvailableTextureMem: LongWord; stdcall;
begin
  Result := RealDevice.GetAvailableTextureMem;
end;
function TMyD3DDevice.ResourceManagerDiscardBytes(Bytes: DWord): HResult; stdcall;
begin
  Result := RealDevice.ResourceManagerDiscardBytes (Bytes);
end;
function TMyD3DDevice.GetDirect3D(out ppD3D8: IDirect3D8): HResult; stdcall;
begin
  Result := RealDevice.GetDirect3D(ppD3D8);
end;
function TMyD3DDevice.GetDeviceCaps(out pCaps: TD3DCaps8): HResult; stdcall;
begin
  Result := RealDevice.GetDeviceCaps(pCaps);
end;
function TMyD3DDevice.GetDisplayMode(out pMode: TD3DDisplayMode): HResult; stdcall;
begin
  Result := RealDevice.GetDisplayMode(pMode);
end;
function TMyD3DDevice.GetCreationParameters(out pParameters: TD3DDeviceCreationParameters): HResult; stdcall;
begin
  Result := RealDevice.GetCreationParameters(pParameters);
end;
function TMyD3DDevice.SetCursorProperties(XHotSpot, YHotSpot: LongWord; pCursorBitmap: IDirect3DSurface8): HResult; stdcall;
begin
  Result := RealDevice.SetCursorProperties (XHotSpot, YHotSpot, pCursorBitmap);
end;
procedure TMyD3DDevice.SetCursorPosition(XScreenSpace, YScreenSpace: Integer; Flags: DWord); stdcall;
begin
  RealDevice.SetCursorPosition (XScreenSpace, YScreenSpace, Flags);
end;
function TMyD3DDevice.ShowCursor(bShow: BOOL): BOOL; stdcall;
begin
  Result := RealDevice.ShowCursor (bShow);
end;
function TMyD3DDevice.CreateAdditionalSwapChain(const pPresentationParameters: TD3DPresentParameters; out pSwapChain: IDirect3DSwapChain8): HResult; stdcall;
begin
  Result := RealDevice.CreateAdditionalSwapChain (pPresentationParameters, pSwapChain);
end;

function TMyD3DDevice.Reset(const pPresentationParameters: TD3DPresentParameters): HResult; stdcall;
begin
  StopView;
  Result := RealDevice.Reset (pPresentationParameters);
  StartView;
end;
function TMyD3DDevice.Present(pSourceRect, pDestRect: PRect; hDestWindowOverride: HWND; pDirtyRegion: PRgnData): HResult; stdcall;
begin
  Result := RealDevice.Present (pSourceRect, pDestRect, hDestWindowOverride, pDirtyRegion);
end;
function TMyD3DDevice.GetBackBuffer(BackBuffer: LongWord; _Type: TD3DBackBufferType; out ppBackBuffer: IDirect3DSurface8): HResult; stdcall;
begin
  Result := RealDevice.GetBackBuffer (BackBuffer, _Type, ppBackBuffer);
end;
function TMyD3DDevice.GetRasterStatus(out pRasterStatus: TD3DRasterStatus): HResult; stdcall;
begin
  Result := RealDevice.GetRasterStatus (pRasterStatus);
end;
procedure TMyD3DDevice.SetGammaRamp(Flags: DWord; const pRamp: TD3DGammaRamp); stdcall;
begin
  RealDevice.SetGammaRamp (Flags, pRamp);
end;
procedure TMyD3DDevice.GetGammaRamp(out pRamp: TD3DGammaRamp); stdcall;
begin
  RealDevice.GetGammaRamp (pRamp);
end;
function TMyD3DDevice.CreateTexture(Width, Height, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool; out ppTexture: IDirect3DTexture8): HResult; stdcall;
begin
  Result := RealDevice.CreateTexture (Width, Height, Levels, Usage, Format, Pool, ppTexture);
end;
function TMyD3DDevice.CreateVolumeTexture(Width, Height, Depth, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool; out ppVolumeTexture: IDirect3DVolumeTexture8): HResult; stdcall;
begin
  Result := RealDevice.CreateVolumeTexture (Width, Height, Depth, Levels, Usage, Format, Pool, ppVolumeTexture);
end;
function TMyD3DDevice.CreateCubeTexture(EdgeLength, Levels: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool; out ppCubeTexture: IDirect3DCubeTexture8): HResult; stdcall;
begin
  Result := RealDevice.CreateCubeTexture (EdgeLength, Levels, Usage, Format, Pool, ppCubeTexture);
end;
function TMyD3DDevice.CreateVertexBuffer(Length: LongWord; Usage, FVF: DWord; Pool: TD3DPool; out ppVertexBuffer: IDirect3DVertexBuffer8): HResult; stdcall;
begin
  Result := RealDevice.CreateVertexBuffer (Length, Usage, FVF, Pool, ppVertexBuffer);
end;
function TMyD3DDevice.CreateIndexBuffer(Length: LongWord; Usage: DWord; Format: TD3DFormat; Pool: TD3DPool; out ppIndexBuffer: IDirect3DIndexBuffer8): HResult; stdcall;
begin
  Result := RealDevice.CreateIndexBuffer (Length, Usage, Format, Pool, ppIndexBuffer);
end;
function TMyD3DDevice.CreateRenderTarget(Width, Height: LongWord; Format: TD3DFormat; MultiSample: TD3DMultiSampleType; Lockable: BOOL; out ppSurface: IDirect3DSurface8): HResult; stdcall;
begin
  Result := RealDevice.CreateRenderTarget (Width, Height, Format, MultiSample, Lockable, ppSurface);
end;
function TMyD3DDevice.CreateDepthStencilSurface(Width, Height: LongWord; Format: TD3DFormat; MultiSample: TD3DMultiSampleType; out ppSurface: IDirect3DSurface8): HResult; stdcall;
begin
  Result := RealDevice.CreateDepthStencilSurface (Width, Height, Format, MultiSample, ppSurface);
end;
function TMyD3DDevice.CreateImageSurface(Width, Height: LongWord; Format: TD3DFormat; out ppSurface: IDirect3DSurface8): HResult; stdcall;
begin
  Result := RealDevice.CreateImageSurface (Width, Height, Format, ppSurface);
end;
function TMyD3DDevice.CopyRects(pSourceSurface: IDirect3DSurface8; pSourceRectsArray: PRect; cRects: LongWord; pDestinationSurface: IDirect3DSurface8; pDestPointsArray: PPoint): HResult; stdcall;
begin
  Result := RealDevice.CopyRects (pSourceSurface, pSourceRectsArray, cRects, pDestinationSurface, pDestPointsArray);
end;
function TMyD3DDevice.UpdateTexture(pSourceTexture, pDestinationTexture: IDirect3DBaseTexture8): HResult; stdcall;
begin
  Result := RealDevice.UpdateTexture (pSourceTexture, pDestinationTexture);
end;
function TMyD3DDevice.GetFrontBuffer(pDestSurface: IDirect3DSurface8): HResult; stdcall;
begin
  Result := RealDevice.GetFrontBuffer (pDestSurface);
end;
function TMyD3DDevice.SetRenderTarget(pRenderTarget, pNewZStencil: IDirect3DSurface8): HResult; stdcall;
begin
  Result := RealDevice.SetRenderTarget (pRenderTarget, pNewZStencil);
end;
function TMyD3DDevice.GetRenderTarget(out ppRenderTarget: IDirect3DSurface8): HResult; stdcall;
begin
  Result := RealDevice.GetRenderTarget (ppRenderTarget);
end;
function TMyD3DDevice.GetDepthStencilSurface(out ppZStencilSurface: IDirect3DSurface8): HResult; stdcall;
begin
  Result := RealDevice.GetDepthStencilSurface (ppZStencilSurface);
end;
function TMyD3DDevice.BeginScene: HResult; stdcall;
begin
  Result := RealDevice.BeginScene;
end;
function TMyD3DDevice.EndScene: HResult; stdcall;
begin
  ViewText (RealDevice);
  Result := RealDevice.EndScene;
end;
function TMyD3DDevice.Clear(Count: DWord; pRects: PD3DRect; Flags: DWord; Color: TD3DColor; Z: Single; Stencil: DWord): HResult; stdcall;
begin
  Result := RealDevice.Clear (Count, pRects, Flags, Color, Z, Stencil);
end;
function TMyD3DDevice.SetTransform(State: TD3DTransformStateType; const pMatrix: TD3DMatrix): HResult; stdcall;
begin
  Result := RealDevice.SetTransform (State, pMatrix);
end;
function TMyD3DDevice.GetTransform(State: TD3DTransformStateType; out pMatrix: TD3DMatrix): HResult; stdcall;
begin
  Result := RealDevice.GetTransform (State, pMatrix);
end;
function TMyD3DDevice.MultiplyTransform(State: TD3DTransformStateType; const pMatrix: TD3DMatrix): HResult; stdcall;
begin
  Result := RealDevice.MultiplyTransform (State, pMatrix);
end;
function TMyD3DDevice.SetViewport(const pViewport: TD3DViewport8): HResult; stdcall;
begin
  Result := RealDevice.SetViewport (pViewport);
end;
function TMyD3DDevice.GetViewport(out pViewport: TD3DViewport8): HResult; stdcall;
begin
  Result := RealDevice.GetViewport (pViewport);
end;
function TMyD3DDevice.SetMaterial(const pMaterial: TD3DMaterial8): HResult; stdcall;
begin
  Result := RealDevice.SetMaterial (pMaterial);
end;
function TMyD3DDevice.GetMaterial(out pMaterial: TD3DMaterial8): HResult; stdcall;
begin
  Result := RealDevice.GetMaterial (pMaterial);
end;
function TMyD3DDevice.SetLight(Index: DWord; const pLight: TD3DLight8): HResult; stdcall;
begin
  Result := RealDevice.SetLight (Index, pLight);
end;
function TMyD3DDevice.GetLight(Index: DWord; out pLight: TD3DLight8): HResult; stdcall;
begin
  Result := RealDevice.GetLight (Index, pLight);
end;
function TMyD3DDevice.LightEnable(Index: DWord; Enable: BOOL): HResult; stdcall;
begin
  Result := RealDevice.LightEnable (Index, Enable);
end;
function TMyD3DDevice.GetLightEnable(Index: DWord; out pEnable: BOOL): HResult; stdcall;
begin
  Result := RealDevice.GetLightEnable (Index, pEnable);
end;
function TMyD3DDevice.SetClipPlane(Index: DWord; pPlane: PSingle): HResult; stdcall;
begin
  Result := RealDevice.SetClipPlane (Index, pPlane);
end;
function TMyD3DDevice.GetClipPlane(Index: DWord; pPlane: PSingle): HResult; stdcall;
begin
  Result := RealDevice.GetClipPlane (Index, pPlane);
end;
function TMyD3DDevice.SetRenderState(State: TD3DRenderStateType; Value: DWord): HResult; stdcall;
begin
  Result := RealDevice.SetRenderState (State, Value);
end;
function TMyD3DDevice.GetRenderState(State: TD3DRenderStateType; out pValue: DWord): HResult; stdcall;
begin
  Result := RealDevice.GetRenderState (State, pValue);
end;
function TMyD3DDevice.BeginStateBlock: HResult; stdcall;
begin
  Result := RealDevice.BeginStateBlock;
end;
function TMyD3DDevice.EndStateBlock(out pToken: DWord): HResult; stdcall;
begin
  Result := RealDevice.EndStateBlock (pToken);
end;
function TMyD3DDevice.ApplyStateBlock(Token: DWord): HResult; stdcall;
begin
  Result := RealDevice.ApplyStateBlock (Token);
end;
function TMyD3DDevice.CaptureStateBlock(Token: DWord): HResult; stdcall;
begin
  Result := RealDevice.CaptureStateBlock (Token);
end;
function TMyD3DDevice.DeleteStateBlock(Token: DWord): HResult; stdcall;
begin
  Result := RealDevice.DeleteStateBlock (Token);
end;
function TMyD3DDevice.CreateStateBlock(_Type: TD3DStateBlockType; out Token: DWord): HResult; stdcall;
begin
  Result := RealDevice.CreateStateBlock (_Type, Token);
end;
function TMyD3DDevice.SetClipStatus(const pClipStatus: TD3DClipStatus8): HResult; stdcall;
begin
  Result := RealDevice.SetClipStatus (pClipStatus);
end;
function TMyD3DDevice.GetClipStatus(out pClipStatus: TD3DClipStatus8): HResult; stdcall;
begin
  Result := RealDevice.GetClipStatus (pClipStatus);
end;
function TMyD3DDevice.GetTexture(Stage: DWord; out ppTexture: IDirect3DBaseTexture8): HResult; stdcall;
begin
  Result := RealDevice.GetTexture (Stage, ppTexture);
end;
function TMyD3DDevice.SetTexture(Stage: DWord; pTexture: IDirect3DBaseTexture8): HResult; stdcall;
begin
  Result := RealDevice.SetTexture (Stage, pTexture);
end;
function TMyD3DDevice.GetTextureStageState(Stage: DWord; _Type: TD3DTextureStageStateType; out pValue: DWord): HResult; stdcall;
begin
  Result := RealDevice.GetTextureStageState (Stage, _Type, pValue);
end;
function TMyD3DDevice.SetTextureStageState(Stage: DWord; _Type: TD3DTextureStageStateType; Value: DWord): HResult; stdcall;
begin
  Result := RealDevice.SetTextureStageState (Stage, _Type, Value);
end;
function TMyD3DDevice.ValidateDevice(out pNumPasses: DWord): HResult; stdcall;
begin
  Result := RealDevice.ValidateDevice (pNumPasses);
end;
function TMyD3DDevice.GetInfo(DevInfoID: DWord; out pDevInfoStruct; DevInfoStructSize: DWord): HResult; stdcall;
begin
  Result := RealDevice.GetInfo (DevInfoID, pDevInfoStruct, DevInfoStructSize);
end;
function TMyD3DDevice.SetPaletteEntries(PaletteNumber: LongWord; pEntries: pPaletteEntry): HResult; stdcall;
begin
  Result := RealDevice.SetPaletteEntries (PaletteNumber, pEntries);
end;
function TMyD3DDevice.GetPaletteEntries(PaletteNumber: LongWord; pEntries: pPaletteEntry): HResult; stdcall;
begin
  Result := RealDevice.GetPaletteEntries (PaletteNumber, pEntries);
end;
function TMyD3DDevice.SetCurrentTexturePalette(PaletteNumber: LongWord): HResult; stdcall;
begin
  Result := RealDevice.SetCurrentTexturePalette (PaletteNumber);
end;
function TMyD3DDevice.GetCurrentTexturePalette(out PaletteNumber: LongWord): HResult; stdcall;
begin
  Result := RealDevice.GetCurrentTexturePalette (PaletteNumber);
end;
function TMyD3DDevice.DrawPrimitive(PrimitiveType: TD3DPrimitiveType; StartVertex, PrimitiveCount: LongWord): HResult; stdcall;
begin
  Result := RealDevice.DrawPrimitive (PrimitiveType, StartVertex, PrimitiveCount);
end;
function TMyD3DDevice.DrawIndexedPrimitive(_Type: TD3DPrimitiveType; minIndex, NumVertices, startIndex, primCount: LongWord): HResult; stdcall;
begin
  Result := RealDevice.DrawIndexedPrimitive (_Type, minIndex, NumVertices, startIndex, primCount);
end;
function TMyD3DDevice.DrawPrimitiveUP(PrimitiveType: TD3DPrimitiveType; PrimitiveCount: LongWord; const pVertexStreamZeroData; VertexStreamZeroStride: LongWord): HResult; stdcall;
begin
  Result := RealDevice.DrawPrimitiveUP (PrimitiveType, PrimitiveCount, pVertexStreamZeroData, VertexStreamZeroStride);
end;
function TMyD3DDevice.DrawIndexedPrimitiveUP(PrimitiveType: TD3DPrimitiveType; MinVertexIndex, NumVertexIndices, PrimitiveCount: LongWord; const pIndexData; IndexDataFormat: TD3DFormat; const pVertexStreamZeroData; VertexStreamZeroStride: LongWord): HResult; stdcall;
begin
  Result := RealDevice.DrawIndexedPrimitiveUP (PrimitiveType, MinVertexIndex, NumVertexIndices, PrimitiveCount, pIndexData, IndexDataFormat, pVertexStreamZeroData, VertexStreamZeroStride);
end;
function TMyD3DDevice.ProcessVertices(SrcStartIndex, DestIndex, VertexCount: LongWord; pDestBuffer: IDirect3DVertexBuffer8; Flags: DWord): HResult; stdcall;
begin
  Result := RealDevice.ProcessVertices (SrcStartIndex, DestIndex, VertexCount, pDestBuffer, Flags);
end;
function TMyD3DDevice.CreateVertexShader(pDeclaration, pFunction: PDWord; out pHandle: DWord; Usage: DWord): HResult; stdcall;
begin
  Result := RealDevice.CreateVertexShader (pDeclaration, pFunction, pHandle, Usage);
end;
function TMyD3DDevice.SetVertexShader(Handle: DWord): HResult; stdcall;
begin
  Result := RealDevice.SetVertexShader (Handle);
end;
function TMyD3DDevice.GetVertexShader(out pHandle: DWord): HResult; stdcall;
begin
  Result := RealDevice.GetVertexShader (pHandle);
end;
function TMyD3DDevice.DeleteVertexShader(Handle: DWord): HResult; stdcall;
begin
  Result := RealDevice.DeleteVertexShader (Handle);
end;
function TMyD3DDevice.SetVertexShaderConstant(_Register: DWord; const pConstantData; ConstantCount: DWord): HResult; stdcall;
begin
  Result := RealDevice.SetVertexShaderConstant (_Register, pConstantData, ConstantCount);
end;
function TMyD3DDevice.GetVertexShaderConstant(_Register: DWord; out pConstantData; ConstantCount: DWord): HResult; stdcall;
begin
  Result := RealDevice.GetVertexShaderConstant (_Register, pConstantData, ConstantCount);
end;
function TMyD3DDevice.GetVertexShaderDeclaration(Handle: DWord; pData: Pointer; out pSizeOfData: DWord): HResult; stdcall;
begin
  Result := RealDevice.GetVertexShaderDeclaration (Handle, pData, pSizeOfData);
end;
function TMyD3DDevice.GetVertexShaderFunction(Handle: DWord; pData: Pointer; out pSizeOfData: DWord): HResult; stdcall;
begin
  Result := RealDevice.GetVertexShaderFunction (Handle, pData, pSizeOfData);
end;
function TMyD3DDevice.SetStreamSource(StreamNumber: LongWord; pStreamData: IDirect3DVertexBuffer8; Stride: LongWord): HResult; stdcall;
begin
  Result := RealDevice.SetStreamSource (StreamNumber, pStreamData, Stride);
end;
function TMyD3DDevice.GetStreamSource(StreamNumber: LongWord; out ppStreamData: IDirect3DVertexBuffer8; out pStride: LongWord): HResult; stdcall;
begin
  Result := RealDevice.GetStreamSource (StreamNumber, ppStreamData, pStride);
end;
function TMyD3DDevice.SetIndices(pIndexData: IDirect3DIndexBuffer8; BaseVertexIndex: LongWord): HResult; stdcall;
begin
  Result := RealDevice.SetIndices (pIndexData, BaseVertexIndex);
end;
function TMyD3DDevice.GetIndices(out ppIndexData: IDirect3DIndexBuffer8; out pBaseVertexIndex: LongWord): HResult; stdcall;
begin
  Result := RealDevice.GetIndices (ppIndexData, pBaseVertexIndex);
end;
function TMyD3DDevice.CreatePixelShader(pFunction: PDWord; out pHandle: DWord): HResult; stdcall;
begin
  Result := RealDevice.CreatePixelShader (pFunction, pHandle);
end;
function TMyD3DDevice.SetPixelShader(Handle: DWord): HResult; stdcall;
begin
  Result := RealDevice.SetPixelShader (Handle);
end;
function TMyD3DDevice.GetPixelShader(out Handle: DWord): HResult; stdcall;
begin
  Result := RealDevice.GetPixelShader (Handle);
end;
function TMyD3DDevice.DeletePixelShader(Handle: DWord): HResult; stdcall;
begin
  Result := RealDevice.DeletePixelShader (Handle);
end;
function TMyD3DDevice.SetPixelShaderConstant(_Register: DWord; const pConstantData; ConstantCount: DWord): HResult; stdcall;
begin
  Result := RealDevice.SetPixelShaderConstant (_Register, pConstantData, ConstantCount);
end;
function TMyD3DDevice.GetPixelShaderConstant(_Register: DWord; out pConstantData; ConstantCount: DWord): HResult; stdcall;
begin
  Result := RealDevice.GetPixelShaderConstant (_Register,pConstantData,ConstantCount);
end;
function TMyD3DDevice.GetPixelShaderFunction(Handle: DWord; pData: Pointer; var pSizeOfData: DWord): HResult; stdcall;
begin
  Result := RealDevice.GetPixelShaderFunction (Handle,pData,pSizeOfData);
end;
function TMyD3DDevice.DrawRectPatch(Handle: LongWord; pNumSegs: PSingle; pTriPatchInfo: PD3DRectPatchInfo): HResult; stdcall;
begin
  Result := RealDevice.DrawRectPatch (Handle, pNumSegs, pTriPatchInfo);
end;
function TMyD3DDevice.DrawTriPatch(Handle: LongWord; pNumSegs: PSingle; pTriPatchInfo: PD3DTriPatchInfo): HResult; stdcall;
begin
  Result := RealDevice.DrawTriPatch (Handle, pNumSegs, pTriPatchInfo);
end;
function TMyD3DDevice.DeletePatch(Handle: LongWord): HResult; stdcall;
begin
  Result := RealDevice.DeletePatch (Handle);
end;







end.
