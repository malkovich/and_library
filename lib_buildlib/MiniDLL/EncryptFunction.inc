
function GetEncryptMSG(var RC4:TRC4Context; Buffer: Pointer; Size: Integer):PChar;
type
  LPByteArray = ^TByteArray;
  TByteArray = array[word] of byte;
var
  OutStrLen, Base64Length, I: Integer;
  EncryptBuffer: LPByteArray;
  OutputBuffer: PChar;
begin
  EncryptBuffer := AllocMem(Size);

  RC4KeyExtract(RC4, Size, EncryptBuffer);
  for I := 0 to Size - 1 do
    EncryptBuffer[I] := LPByteArray(Buffer)[I] xor EncryptBuffer[I];

  OutputBuffer := AllocMem(Size * 2);
  OutStrLen := 0;

  Base64Length := EncodeBase64(@EncryptBuffer[0], Size, OutputBuffer[OutStrLen]);
  Inc(OutStrLen, Base64Length);
  OutputBuffer[OutStrLen] := #0;

  FreeMem(Pointer(EncryptBuffer));
  Result := @OutputBuffer[0];
end;

function GetDecryptMSG(var RC4:TRC4Context; Base64Str: PChar; var Size: Integer):PChar;
type
  LPByteArray = ^TByteArray;
  TByteArray = array[word] of byte;
var
  BaseLen, I: Integer;
  DecryptBuffer, KeyBuffer: LPByteArray;
begin
  BaseLen := StrLen (Base64Str);

  DecryptBuffer := AllocMem (BaseLen);
  Size := DecodeBase64 (Base64Str, BaseLen, DecryptBuffer[0]);
  DecryptBuffer[Size] := 0;

  KeyBuffer := AllocMem (Size);
  RC4KeyExtract(RC4, Size, KeyBuffer);

  for I := 0 to Size - 1 do
    DecryptBuffer[I] := KeyBuffer[I] xor DecryptBuffer[I];

  FreeMem (Pointer(KeyBuffer));
  Result := Pointer(DecryptBuffer);
end;
