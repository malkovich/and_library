library Algorithm;

uses
  Windows,
  SysUtils, comObj,
  base64unit,PasswordMD5Time,
  Classes;

function Base64Encode(InputBuffer: PChar; InputLength:Integer; Output: PChar): Integer; stdcall;
begin
  Result := EncodeBase64 (InputBuffer, InputLength, Output^);
end;

function Base64Decode(InputBase64: PChar; Output: PChar): Integer; stdcall;
var
  InputStr: String;
begin
  InputStr := StrPas (InputBase64);
  InputStr := Trim (InputStr);
  Result := DecodeBase64 (PChar(InputStr), Length (InputStr), Output^);
end;

//int WINAPI MakeGuid (char* OutBuffer);
function MakeGuid (OutBuffer: PChar): Integer; Stdcall;
var
  GuidStr: String;
begin
  GuidStr := CreateClassID;
  Result := Length (GuidStr);
  CopyMemory (OutBuffer, @GuidStr[1], Result);
  OutBuffer[Result] := #0;
end;


exports
  Base64Encode,
  Base64Decode,
  MakeGuid,
  GetNowTimeStr,
  GetUserPassMD5_Client,
  GetUserPassMD5_Server;

begin
end.
