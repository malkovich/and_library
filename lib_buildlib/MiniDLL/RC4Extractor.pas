unit RC4Extractor;

interface

const
  SAMPLE_GUID = '{26D7A28F-8F72-40CC-8749-08B8838CAF60}';

type
  TGUIDStr = array[0..Length(SAMPLE_GUID) - 1]of char;

  TRC4Context = packed record
    D:        array[Byte] of Byte;
    I,J:      Byte;
    Offset:   Integer;
    Reserved: WORD;
  end;


procedure RC4Init(var RC4: TRC4Context; const Key: TGUIDStr);
function  RC4KeyExtract(var RC4: TRC4Context; Count: Integer;
                        OutBuffer: POINTER = NIL; Offset: Integer = -1):Integer;

implementation


type
  PByteArray = ^TByteArray;
  TByteArray = array[WORD] of Byte;


procedure RC4Init(var RC4: TRC4Context; const Key: TGUIDStr);
var
  R,S,T,K: Byte;
  U,L: Integer;
begin
{$R-}
{$Q-}
  L := SizeOf(Key);
  with RC4 do
  begin
    I := 0;
    J := 0;
    Offset := 0;
    for S := 0 to 255 do D[S] := S;
    R := 0;
    U := 0;
    for S := 0 to 255 do
    begin
      if U < L then K := PByteArray(@Key[0])[U] else K := 0;
      Inc(U);
      if U >= L then U := 0;

      Inc(R, D[S] + K);
      T    := D[S];
      D[S] := D[R];
      D[R] := T;
    end;
    Offset := 0;
  end;
end;

function _RC4KeyExtract(var RC4: TRC4Context; Count: Integer; OutBuffer: POINTER):Integer;
var
  S: Integer;
  T: Byte;
begin
  with RC4 do
  begin
    Result := Offset;
    for S := 0 to Count -1 do
    begin
      Inc(I);
      T := D[I];
      Inc(J, T);
      D[I] := D[J];
      D[J] := T;
      Inc(T, D[I]);
      if Assigned(OutBuffer) then
        PByteArray(OutBuffer)[S] :=  D[T];
      Inc(Offset);
    end;
  end;
end;

function  RC4KeyExtract(var RC4: TRC4Context; Count: Integer;
                        OutBuffer: POINTER = NIL; 
                        Offset: Integer = -1):Integer;
var
  SkipLength :Integer;
begin
  if Offset <> -1 then
  begin
    if Offset < RC4.Offset then
    begin
      Result := -1;
      Exit;
    end;

    SkipLength := Offset - RC4.Offset;

    if SkipLength > 0 then
      _RC4KeyExtract(RC4, SkipLength, NIL);
  end;

  Result := _RC4KeyExtract(RC4, Count, OutBuffer); 
end;


end.
