unit u_3D_engine;

interface
uses classes,windows,sysutils,directxGraphics;

type

  E3DError=class(Exception);

  TEngineParam=record
     Handle:HWND;
     Fullscreen:boolean;
     ResolutionWidth:integer;
     ResolutionHeight:integer;
     TotalBackBuffer:integer;
     ColorDepth:integer;
     InitDeviceOnCreate:boolean;
  end;

  TEngine=class(TObject)
  private
    FDirect3DObj:IDirect3D8;
    FDirect3DDevice:IDirect3DDevice8;

    function CreateD3D:IDirect3D8;
    function CreateDevice(const d3d:IDirect3D8):IDirect3DDevice8;
    function HwVertexProcSupported:boolean;
  protected
    FEngineParam:TEngineParam;
    FPresentParam:TD3DPresent_Parameters;

    procedure BeginDraw;
    procedure Draw;virtual;abstract;
    procedure EndDraw;
  public
    constructor Create(Param:TEngineParam);virtual;
    destructor Destroy;override;

    procedure InitDevice;

    procedure Clear(const ClearCol:TD3DColor);
    procedure Render;
  published
    property Direct3DObj:IDirect3D8 read FDirect3DObj;
    property Direct3DDevice:IDirect3DDevice8 read FDirect3DDevice;
  end;

  T3DCollection=class(TCollection)
  private
    FEngine: TEngine;
    procedure SetEngine(const Value: TEngine);
  published
    property Engine:TEngine read FEngine write SetEngine;
  end;

  T3DObject=class(TCollectionItem)
  private
  protected
    FVertexBuff:IDirect3DVertexBuffer8;
    FIndexBuff:IDirect3DIndexBuffer8;
  public
    procedure Init;virtual;abstract;
    procedure Draw;virtual;abstract;
  end;

  TTexture=class(TCollectionItem)
  private
    FTextureObj: IDirect3DTexture8;
    FColorKey: TD3DColor;
    procedure SetColorKey(const Value: TD3DColor);
  public
    destructor Destroy;override;
    procedure LoadFromFile(const filename:string);
  published
    property TextureObj:IDirect3DTexture8 read FTextureObj;
    property ColorKey:TD3DColor read FColorKey write SetColorKey;
  end;

  TTextureCollection=class(T3DCollection)
  end;

implementation
uses d3dx8;

{ TEngine }

procedure TEngine.BeginDraw;
begin
  if FDirect3DDevicenil then
    FDirect3DDevice.BeginScene;
end;

procedure TEngine.Clear(const ClearCol:TD3DColor);
begin
  if FDirect3DDevicenil then
    FDirect3DDevice.Clear(0,nil,D3DCLEAR_TARGET,ClearCol,0,0);
end;

constructor TEngine.Create(Param: TEngineParam);
begin
  FEngineParam:=Param;
  FDirect3DObj:=CreateD3D;
  if FEngineParam.InitDeviceOnCreate then
    InitDevice;
end;

function TEngine.CreateD3D: IDirect3D8;
begin
  result:=Direct3DCreate8(D3D_SDK_VERSION);
end;

function TEngine.CreateDevice(const d3d: IDirect3D8): IDirect3DDevice8;
var hr:HResult;
   vertexProcessing:cardinal;
begin
  if d3dnil then
  begin
    ZeroMemory(@FPresentParam,sizeof(TD3DPresent_Parameters));

    FPresentParam.Windowed:=not FEngineParam.Fullscreen;
    FPresentParam.BackBufferWidth:=FEngineParam.ResolutionWidth;
    FPresentParam.BackBufferHeight:=FEngineParam.ResolutionHeight;
    FPresentParam.BackBufferCount:=FEngineParam.TotalBackBuffer;
    FPresentParam.BackBufferFormat:=FEngineParam.ColorDepth;
    FPresentParam.SwapEffect:=D3DSWAPEFFECT_DISCARD;

    //3D hardware acceleration tersedia?
    if HwVertexProcSupported then
      vertexProcessing:=D3DCREATE_HARDWARE_VERTEXPROCESSING
    else
      vertexProcessing:=D3DCREATE_SOFTWARE_VERTEXPROCESSING;

    hr:=d3d.CreateDevice(D3DADAPTER_DEFAULT,
                   D3DDEVTYPE_HAL,
                   FEngineParam.Handle,
                   vertexProcessing,
                   FPresentParam,
                   result);
    if result=nil then
      raise E3DError.Create(D3DXErrorString(hr));
  end else
   result:=nil;

end;

destructor TEngine.Destroy;
begin
  FDirect3DDevice:=nil;
  FDirect3DObj:=nil;
  inherited;
end;

procedure TEngine.EndDraw;
begin
  if FDirect3DDevicenil then
    FDirect3DDevice.EndScene;
end;

function TEngine.HwVertexProcSupported: boolean;
var caps:TD3DCaps8;
begin
  FDirect3DObj.GetDeviceCaps(D3DADAPTER_DEFAULT,
                    D3DDEVTYPE_HAL,caps);

  result:=((caps.DevCaps and D3DDEVCAPS_HWTRANSFORMANDLIGHT)0);
end;

procedure TEngine.InitDevice;
begin
  FDirect3DDevice:=CreateDevice(FDirect3DObj);
end;

procedure TEngine.Render;
begin
  BeginDraw;
  try
    Draw;
    if FDirect3DDevicenil then
      FDirect3DDevice.Present(nil,nil,0,nil);
  finally
    EndDraw;
  end;
end;

{ T3DCollection }

procedure T3DCollection.SetEngine(const Value: TEngine);
begin
  FEngine := Value;
end;

{ TTexture }

destructor TTexture.Destroy;
begin
  FTextureObj:=nil;
  inherited;
end;

procedure TTexture.LoadFromFile(const filename: string);
var atexcollection:TTextureCollection;
    hr:HResult;
begin
  if Collection is TTextureCollection then
  begin
    aTexCollection:=Collection as TTextureCollection;
    if (aTexCollection.Engine.Direct3DDevicenil) and
       fileExists(filename) then
    begin
      hr:=D3DXCreateTextureFromFileEx(aTexCollection.Engine.Direct3DDevice,
                        PChar(filename),
                        D3DX_DEFAULT,
                        D3DX_DEFAULT,
                        D3DX_DEFAULT,
                        0,
                        D3DFMT_UNKNOWN,
                        D3DPOOL_MANAGED,
                        D3DX_FILTER_NONE,
                        D3DX_FILTER_NONE,
                        FColorKey,
                        nil,nil,
                        FTextureObj);
       if FTextureObj=nil then
        raise E3DError.Create(D3DXErrorString(hr));
    end;
  end;
end;

procedure TTexture.SetColorKey(const Value: TD3DColor);
begin
  FColorKey := Value;
end;

end.
