unit TypeRecordList;

interface
uses windows, classes, sysUtils;


const
   BUF_SIZE=51200;

Type
  PPackageUnit=^TPackageUnit;
  TPackageUnit= packed record
    BeenCancel :wordbool;
    len:word;
    buf:array[0..BUF_SIZE-1] of char;
  end;

  THowToClassifyProc = procedure(Buffer:Pointer; Length:Integer; var BufferOut:Pointer; var LengthOut:Integer) of object;
  TOnNewTypeProc = procedure(MarkBuffer:Pointer; MarkLength:Integer) of object;

  TTypeRecordList = class(TObject)
  private
    FTypeList :TList;
    FHowToClassify :THowToClassifyProc;
    FOnNewType :TOnNewTypeProc;
    function IsIn(Buffer:Pointer; Length:Integer):Integer;
  public
    Constructor Create(HowToClassify:THowToClassifyProc; OnNewType:TOnNewTypeProc);
    Destructor Destroy; override;
    function CheckPoint(Buffer:Pointer; Length:Integer):LongBool;
    function SetAllow(MarkBuffer:Pointer; MarkLength:Integer; Allow:LongBool=True):LongBool;
  end;

implementation

Constructor TTypeRecordList.Create(HowToClassify:THowToClassifyProc; OnNewType:TOnNewTypeProc);
begin
  FTypeList := TList.Create;
  FHowToClassify := HowToClassify;
  FOnNewType := OnNewType;
end;

Destructor TTypeRecordList.Destroy;
var
  I :Integer;
begin
  for I := 0 to FTypeList.Count - 1 do
    FreeMem(FTypeList[I]);
  FTypeList.Clear;
end;

function TTypeRecordList.IsIn(Buffer:Pointer; Length:Integer):Integer;
var
  Package :PPackageUnit;
  I:Integer;
begin
  result := -1;
  for I := 0 to FTypeList.Count - 1 do
  begin
    Package :=FTypeList[I];
    if Package.len <> Length then continue;
    if Not CompareMem(@Package.Buf[0], Buffer, Length) then continue;
    result := I;
    break;
  end;
end;

function TTypeRecordList.SetAllow(MarkBuffer:Pointer; MarkLength:Integer; Allow:LongBool=True):LongBool;
var
  Package :PPackageUnit;
  Index :Integer;
begin
  result := false;
  Index := IsIn(MarkBuffer, markLength);
  if Index = -1 then exit;
  Package := FTypeList[Index];
  Package.BeenCancel := Allow;
  result := true;
end;

function TTypeRecordList.CheckPoint(Buffer:Pointer; Length:Integer):LongBool;
var
  Buf:Pointer;
  Len:Integer;
  Package :PPackageUnit;
  Index :Integer;
begin
  result := true;

  //获取特征标签
  FHowToClassify(Buffer, Length, Buf, Len);

  //维护列表
  Index := IsIn(Buf, Len);
  if Index = -1 then
  begin
    GetMem(Package, Len + SizeOf(TPackageUnit) - SizeOf(Package.buf));
    Package.len := Len;
    Package.BeenCancel := False;
    Copymemory(@Package.buf[0], Buf, Len);
    FTypeList.Add(Package);
    FOnNewType(Buf, Len);
    exit;
  end;

  if PPackageUnit(FTypeList[Index]).BeenCancel then
    result := False;
end;



end.
