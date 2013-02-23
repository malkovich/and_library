unit TPacketSampleList;

interface
uses
  windows, classes, stdctrls, FormatFunction, SysUtils;

const
   BUF_SIZE=51200;

Type
  PPackageUnit=^TPackageUnit;
  TPackageUnit= packed record
      len:word;
      buf:array[0..BUF_SIZE-1] of char;
  end;

  TPacketSampleLister = class(TObject)
  private
    FListBox :TListBox;
    FList    :TList;
  public
    Constructor Create(ListBox:TListbox);
    Destructor Destroy;override;

    function Store(Caption:string; Buffer:Pointer; Length:Integer):LongBool;
    function CaptionToIndex(Caption:string):Integer;
    function Get(Index:Integer; var Buffer:Pointer; var Length:Integer):LongBool;
    procedure Delete(Index:Integer);
    function ReName(Caption, NewCaption:string):Longbool;
    procedure Clear;
    function SaveToFile(FileName:string):LongBool;
    function LoadFromFile(FileName:String):LongBool;
  end;

implementation

Constructor TPacketSampleLister.Create(ListBox:TListbox);
begin
  inherited Create;
  FListbox := ListBox;
  FList := TList.Create;
end;
Destructor TPacketSampleLister.Destroy;
begin
  Clear;
  FList.Free;
end;

function TPacketSampleLister.Store(Caption:string; Buffer:Pointer; Length:Integer):LongBool;
var
  Package :PPackageUnit;
begin
  GetMem(Package, Length + SizeOf(WORD));
  CopyMemory(@Package.buf[0], Buffer, Length);
  Package.len := Length;
                               
  while FListbox.Items.IndexOf(Caption) <> -1 do
  begin
    Caption := Caption + '[' +TimeToStr(Now)+ ']';
  end;

  FList.Add(Package);
  FListBox.Items.Append(Caption);
  
  Result := Assigned(@Package.buf[0]);
end;

function TPacketSampleLister.CaptionToIndex(Caption:string):Integer;
begin
  result := -1;
  if Caption = '' then exit;
  result := FListbox.Items.IndexOf(Caption);
end;

procedure TPacketSampleLister.Delete(Index:Integer);
begin
  if Index > FList.Count -1 then exit;
  if Index < 0 then exit;
  
  FListbox.Items.Delete(Index);
  FList.Delete(Index);
end;

function TPacketSampleLister.Get(Index:Integer; var Buffer:Pointer; var Length:Integer):LongBool;
var
  Package :PPackageUnit;
begin
  Result := FALSE;
  
  if Index = -1 then EXIT;
  if Index >= FList.Count then EXIT;

  Package := FList[Index];
  Buffer := @Package.buf[0];
  Length := Package.len;

  result := Length >0 ;
end;
function TPacketSampleLister.ReName(Caption, NewCaption:string):Longbool;
var
  Index :Integer;
begin
  RESULT := FALSE;
  Index := FListbox.Items.IndexOf(Caption);
  if Index = -1 then EXIT;

  FListbox.Items[Index] := NewCaption;
  Result := TRUE;
end;

procedure TPacketSampleLister.Clear;
var
  I:Integer;
begin
  FListBox.Clear;
  for I := 0 to Flist.Count - 1 do
    FreeMem(FList[I]);
end;

function TPacketSampleLister.SaveToFile(FileName:string):LongBool;
var
  Index :Integer;
  tmpString :string;
  Package :PPackageUnit;
begin
  DeleteFile(FileName);
  With TStringList.Create() do
  begin
    for Index := 0 to FList.Count - 1 do
    begin
      Append(FListBox.Items[Index]);
      Package := FList[Index];
      tmpString := PcharToPacketStr(@Package.buf[0], Package.len);
      Append(tmpString);
    end;
    SaveToFile(FileName);
  end;
  RESULT := FileExists(FileName);
end;
function TPacketSampleLister.LoadFromFile(FileName:String):LongBool;
var
  strCaption, strBuffer:string;
  isCaption :boolean;
  Index, Count, len :Integer;
  sl:TStringList;
  Buffer:Pointer;
begin
  result := false;
  if not FileExists(FileName) then exit;

  Clear;   
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    Count := sl.Count;
    if Count < 2 then exit;

    isCaption := true;
    for Index := 0 to Count - 1 do
    begin
      if isCaption then
      begin
        strCaption := sl[Index];
        isCaption := False;
      end else
      begin
        strBuffer := sl[Index];
        Buffer := PacketStrToPchar(strBuffer,len);
        isCaption := true;
        Store(strCaption, Buffer, len);
      end;
    end;
  finally
    sl.Free;
  end;
  result := FList.Count > 0;
end;

end.
