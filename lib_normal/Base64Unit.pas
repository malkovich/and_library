unit Base64Unit;

interface
uses
  windows, SysUtils;

function EncodeBase64(Source:string):string; overload;
function DecodeBase64(Source:string):string; overload;
function EncodeBase64(const Source; SourceLen: Integer):string; overload;
function DecodeBase64(Source:string; var Destin):Integer; overload;

function EncodeBase64(InputBuffer: PChar; InputLength:Integer; var Output):Integer; overload;
function DecodeBase64(InputBuffer: PChar; InputLength:Integer; var Output):Integer; overload;

implementation

const
  BaseTable:string='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';

function FindInTable(CSource:char):integer; 
begin
 result:=Pos(string(CSource),BaseTable)-1;
end;

function EncodeBase64(const Source; SourceLen: Integer):string;
var
  SourceStr: String;
begin
  SetLength (SourceStr, SourceLen);
  Move (Source, SourceStr[1], SourceLen);
  Result := EncodeBase64 (SourceStr);
  SetLength (SourceStr, 0);
end;

function DecodeBase64(Source:string; var Destin):Integer;
var
 SrcLen,Times,i:integer;
 x1,x2,x3,x4,xt:byte;
 tmpResult:string;
 tmpLength:Integer;
begin
 tmpResult := '';
// tmpLength := 0;
 Source := Trim (Source);
 SrcLen:=Length(Source);
 Times:=SrcLen div 4;
 for i:=0 to Times-1 do
 begin
  x1:=FindInTable(Source[1+i*4]);
  x2:=FindInTable(Source[2+i*4]);
  x3:=FindInTable(Source[3+i*4]);
  x4:=FindInTable(Source[4+i*4]);
  x1:=x1 shl 2;
  xt:=x2 shr 4;
  x1:=x1 or xt;
  x2:=x2 shl 4;
  tmpResult:=tmpResult+chr(x1);
//  inc(tmpLength);
  if x3= 64 then break;
  xt:=x3 shr 2;
  x2:=x2 or xt;
  x3:=x3 shl 6;
  tmpResult:=tmpResult+chr(x2);
//  inc(tmpLength);
  if x4=64 then break;
  x3:=x3 or x4;
  tmpResult:=tmpResult+chr(x3);
//  inc(tmpLength);
 end;
 tmpLength := Length (tmpResult);
 CopyMemory(@Destin, @tmpResult[1], tmpLength);
 result := tmpLength;
end;


function DecodeBase64(Source:string):string;
var
 SrcLen,Times,i:integer;
 x1,x2,x3,x4,xt:byte;
begin
 result:='';
 Source := Trim (Source);
 SrcLen:=Length(Source);
 Times:=SrcLen div 4;
 for i:=0 to Times-1 do
 begin
  x1:=FindInTable(Source[1+i*4]);
  x2:=FindInTable(Source[2+i*4]);
  x3:=FindInTable(Source[3+i*4]);
  x4:=FindInTable(Source[4+i*4]);
  x1:=x1 shl 2;
  xt:=x2 shr 4;
  x1:=x1 or xt;
  x2:=x2 shl 4;
  result:=result+chr(x1);
  if x3= 64 then break;
  xt:=x3 shr 2;
  x2:=x2 or xt;
  x3:=x3 shl 6;
  result:=result+chr(x2);
  if x4=64 then break;
  x3:=x3 or x4;
  result:=result+chr(x3);
 end;    
end;


function EncodeBase64(Source:string):string;
var
 Times,LenSrc,i:integer;
 x1,x2,x3,x4:char;
 xt:byte;
begin
 result:='';
 LenSrc:=length(Source);
 if LenSrc mod 3 =0 then Times:=LenSrc div 3
 else Times:=LenSrc div 3 + 1;
 for i:=0 to times-1 do
 begin
  if LenSrc >= (3+i*3) then
  begin
   x1:=BaseTable[(ord(Source[1+i*3]) shr 2)+1];
   xt:=(ord(Source[1+i*3]) shl 4) and 48;
   xt:=xt or (ord(Source[2+i*3]) shr 4);
   x2:=BaseTable[xt+1];
   xt:=(Ord(Source[2+i*3]) shl 2) and 60;
   xt:=xt or (ord(Source[3+i*3]) shr 6);
   x3:=BaseTable[xt+1];
   xt:=(ord(Source[3+i*3]) and 63);
   x4:=BaseTable[xt+1];
  end
  else if LenSrc>=(2+i*3) then
  begin
   x1:=BaseTable[(ord(Source[1+i*3]) shr 2)+1];
   xt:=(ord(Source[1+i*3]) shl 4) and 48;
   xt:=xt or (ord(Source[2+i*3]) shr 4);
   x2:=BaseTable[xt+1];
   xt:=(ord(Source[2+i*3]) shl 2) and 60;
   x3:=BaseTable[xt+1];
   x4:='=';
  end else
  begin
   x1:=BaseTable[(ord(Source[1+i*3]) shr 2)+1];
   xt:=(ord(Source[1+i*3]) shl 4) and 48;
   x2:=BaseTable[xt+1];
   x3:='=';
   x4:='=';
  end;
  result:=result+x1+x2+x3+x4;
 end;

end;

const
  Base64Table : Array[0..65] of char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';

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
