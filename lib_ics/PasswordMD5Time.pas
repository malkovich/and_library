unit PasswordMD5Time;

interface
uses windows, sysUtils, CompressBase64;

function GetNowTimeStr (OutBuffer: PChar): Integer; Stdcall;
function GetUserPassMD5_Server (UserName, Password: PChar; OutFirstMD5: PChar; OutSecondMD5: PChar): BOOL; stdcall;
function GetUserPassMD5_Client (ServerTime: TDateTime; UserName, Password: PChar; OutFirstMD5: PChar; OutSecondMD5: PChar): BOOL; stdcall;

implementation


function GetUserPassMD5_Client (ServerTime: TDateTime; UserName, Password: PChar; OutFirstMD5: PChar; OutSecondMD5: PChar): BOOL; stdcall;
var
  BaseStr: String;
  Md5_1, Md5_2: String;
  Md5Size: Integer;
begin
  Result := False;
  if UserName = nil then exit;
  if Password = nil then exit;
  BaseStr := StrPas(UserName) + StrPas(Password);

  if MakeDateTimeSerialMd5 (ServerTime, BaseStr, Md5_1, Md5_2) then
  begin
    Md5Size := Length(Md5_1);
    CopyMemory (OutFirstMD5, PChar(Md5_1), Md5Size);
    OutFirstMD5 [Md5Size] := #0;

    Md5Size := Length(Md5_2);
    CopyMemory (OutSecondMD5, PChar(Md5_2), Md5Size);
    OutSecondMD5 [Md5Size] := #0;

    Result := True;
  end;
end;

function GetUserPassMD5_Server (UserName, Password: PChar; OutFirstMD5: PChar; OutSecondMD5: PChar): BOOL; stdcall;
begin
  Result := GetUserPassMD5_Client (NOW, UserName, Password, OutFirstMD5, OutSecondMD5);
end;

function GetNowTimeStr (OutBuffer: PChar): Integer; Stdcall;
var
  TimeStr: String;
begin
  TimeStr := DateTimeToStr (NOW);
  Result := Length (TImeStr);
  CopyMemory (OutBuffer, PChar(TimeStr), Result);
  OutBuffer [Result] := #0;
end;

end.
