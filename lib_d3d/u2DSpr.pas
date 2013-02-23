unit u2DSpr;

interface

uses classes,directxgraphics,d3dx8,u_3D_engine;

type
  TSpriteCollection=class(T3DCollection)
  private
     FSprite:ID3DXSprite;
     procedure Init;
  public
     destructor Destroy;override;
     procedure BeginDraw;
     procedure EndDraw;
  published
     property SpriteObj:ID3DXSprite read FSprite;
  end;

  TSprite=class(T3DObject)
  private
    FSprite:ID3DXSprite;
    FTexture: TTexture;
    FRotation: single;
    FColor: TD3DColor;
    FScaling: TD3DXVector2;
    FRotationCenter: TD3DXVector2;
    FTranslation: TD3DXVector2;
    FAlpha: byte;

    procedure SetTexture(const Value: TTexture);
    procedure SetColor(const Value: TD3DColor);
    procedure SetRotation(const Value: single);
    procedure SetRotationCenter(const Value: TD3DXVector2);
    procedure SetScaling(const Value: TD3DXVector2);
    procedure SetTranslation(const Value: TD3DXVector2);
    procedure SetAlpha(const Value: byte);
  public
    constructor Create(ACollection: TCollection);override;
    destructor Destroy;override;
    procedure Init;override;
    procedure Draw;override;

    procedure DrawEx(const translVect,
                              scaleVect,
                              rotCenter:PD3DXVector2;
                        const rot:single;
                        const Acolor:TD3DColor;
                        const alphaValue:byte);

    published
       property Texture:TTexture read FTexture write SetTexture;
       property Translation:TD3DXVector2 read FTranslation write SetTranslation;
       property X:single read FTranslation.x write FTranslation.x;
       property Y:single read FTranslation.y write FTranslation.y;
       property RotationCenter:TD3DXVector2 read FRotationCenter write SetRotationCenter;
       property Scaling:TD3DXVector2 read FScaling write SetScaling;
       property Rotation:single read FRotation write SetRotation;
       property Color:TD3DColor read FColor write SetColor;
       property Alpha:byte read FAlpha write SetAlpha;
    end;

implementation

{ TSprite }
constructor TSprite.Create(ACollection: TCollection);
var asprCollect:TSpriteCollection;
begin
  inherited;
  FSprite:=nil;
  if ACollection is TSpriteCollection then
  begin
    asprCollect:=ACollection as TSpriteCollection;
    if asprCollect.SpriteObj=nil then
      asprCollect.Init;

    FSprite:=asprCollect.SpriteObj;
  end;

  FTranslation.x:=0;
  FTranslation.Y:=0;

  FRotationCenter.x:=0;
  FRotationCenter.y:=0;
  FRotation:=0;

  FScaling.x:=1;
  FScaling.y:=1;
  FColor:=$FFFFFFFF;
end;

destructor TSprite.Destroy;
begin
  FSprite:=nil;
  inherited;
end;

procedure TSprite.Draw;
begin
  DrawEx(@FTranslation,
         @FScaling,
         @FRotationCenter,
         FRotation,
         FColor,
         FAlpha);
end;

procedure TSprite.DrawEx(const translVect, scaleVect,
  rotCenter: PD3DXVector2; const rot: single;
  const AColor:TD3DColor;const AlphaValue:byte);
begin
  if (FSpritenil) and
     (FTexturenil) and
     (FTexture.TextureObjnil) then
  begin
    FSprite.Draw(FTexture.TextureObj,nil,
                 scaleVect,
                 rotCenter,rot,
                 translVect,
                 (AlphaValue shl 24) or AColor);
  end;
end;

procedure TSprite.Init;
begin
end;

procedure TSprite.SetAlpha(const Value: byte);
begin
  FAlpha := Value;
end;

procedure TSprite.SetColor(const Value: TD3DColor);
begin
  FColor := Value;
end;

procedure TSprite.SetRotation(const Value: single);
begin
  FRotation := Value;
end;

procedure TSprite.SetRotationCenter(const Value: TD3DXVector2);
begin
  FRotationCenter := Value;
end;

procedure TSprite.SetScaling(const Value: TD3DXVector2);
begin
  FScaling := Value;
end;

procedure TSprite.SetTexture(const Value: TTexture);
begin
  FTexture := Value;
end;

procedure TSprite.SetTranslation(const Value: TD3DXVector2);
begin
  FTranslation := Value;
end;

{ TSpriteCollection }

procedure TSpriteCollection.BeginDraw;
begin
  if FSpritenil then
    FSprite._Begin;
end;

destructor TSpriteCollection.Destroy;
begin
  FSprite:=nil;
  inherited;
end;

procedure TSpriteCollection.EndDraw;
begin
  if FSpritenil then
    FSprite._End;
end;

procedure TSpriteCollection.Init;
begin
  if (Enginenil) and (Engine.Direct3DDevicenil) then
    D3DXCreateSprite(Engine.Direct3DDevice,FSprite)
  else
    FSprite:=nil;
end;

end.