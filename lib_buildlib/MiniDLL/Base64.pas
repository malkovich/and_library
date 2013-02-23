unit Base64;

interface

function EncodeBase64(InputBuffer: PChar; InputLength:Integer; var Output):Integer;
function DecodeBase64(InputBuffer: PChar; InputLength:Integer; var Output):Integer;

implementation

const
  Base64Table : Array[0..65] of char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';

function FindInTable(CSource:Char):integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to 64 do
    if Base64Table[I] = CSource then
    begin
      Result := I;
      Exit;
    end;
end;

function DecodeBase64(InputBuffer: PChar; InputLength:Integer; var Output):Integer;
var
  Times,i:integer;
  x1,x2,x3,x4,xt:byte;
  OutputStr: PChar;
  OutputIndex: Integer;
begin
  Result := -1;
  if not assigned(InputBuffer) then exit;
  if not (InputLength>0) then exit;

  Times := InputLength div 4;

  OutputStr := @Output;
  OutputIndex := 0;
  for i:=0 to Times-1 do
  begin
    x1 := FindInTable(InputBuffer[i*4]);
    x2 := FindInTable(InputBuffer[1+i*4]);
    x3 := FindInTable(InputBuffer[2+i*4]);
    x4 := FindInTable(InputBuffer[3+i*4]);
    x1 := x1 shl 2;
    xt := x2 shr 4;
    x1 := x1 or xt;
    x2 := x2 shl 4;
    OutputStr[OutputIndex] := chr(x1);
    Inc(OutputIndex);

    if x3= 64 then break;
    xt := x3 shr 2;
    x2 := x2 or xt;
    x3 := x3 shl 6;
    OutputStr[OutputIndex] := chr(x2);
    Inc(OutputIndex);

    if x4=64 then break;
    x3 := x3 or x4;
    OutputStr[OutputIndex] := chr(x3);
    Inc(OutputIndex);
  end;
  Result := OutputIndex;
end;

function EncodeBase64(InputBuffer: PChar; InputLength:Integer; var Output):Integer;
var
  Times,i:integer;
  x1,x2,x3,x4:char;
  xt:byte;
  OutputStr: PChar;
  OutputIndex: Integer;
begin
  Result := -1;
  if not assigned(InputBuffer) then exit;
  if not (InputLength>0) then exit;

  if InputLength mod 3 = 0 then
     Times:=InputLength div 3
  else
     Times:=InputLength div 3 + 1;

  OutputStr := @Output;
  OutputIndex := 0;
  for i:=0 to times-1 do
  begin
    if InputLength >= (3+i*3) then
    begin
      x1:=Base64Table[(ord(InputBuffer[i*3]) shr 2)];
      xt:=(ord(InputBuffer[i*3]) shl 4) and 48;
      xt:=xt or (ord(InputBuffer[1+i*3]) shr 4);
      x2:=Base64Table[xt];
      xt:=(Ord(InputBuffer[1+i*3]) shl 2) and 60;
      xt:=xt or (ord(InputBuffer[2+i*3]) shr 6);
      x3:=Base64Table[xt];
      xt:=(ord(InputBuffer[2+i*3]) and 63);
      x4:=Base64Table[xt];
    end
    else if InputLength>=(2+i*3) then
    begin
      x1:=Base64Table[(ord(InputBuffer[i*3]) shr 2)];
      xt:=(ord(InputBuffer[i*3]) shl 4) and 48;
      xt:=xt or (ord(InputBuffer[1+i*3]) shr 4);
      x2:=Base64Table[xt];
      xt:=(ord(InputBuffer[1+i*3]) shl 2) and 60;
      x3:=Base64Table[xt];
      x4:='=';
    end else
    begin
      x1:=Base64Table[(ord(InputBuffer[i*3]) shr 2)];
      xt:=(ord(InputBuffer[i*3]) shl 4) and 48;
      x2:=Base64Table[xt];
      x3:='=';
      x4:='=';
    end;
    OutputStr[OutputIndex]     := x1;
    OutputStr[OutputIndex + 1] := x2;
    OutputStr[OutputIndex + 2] := x3;
    OutputStr[OutputIndex + 3] := x4;

    Inc(OutputIndex, 4);
  end;
  OutputStr[OutputIndex] := #0;
  Result := OutputIndex;
end;

end.
