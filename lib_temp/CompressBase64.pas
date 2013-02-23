unit CompressBase64;

interface
uses Windows, Classes, TDLZCmpr, Base64Unit;

function MakeBase64Package(InStream: TMemoryStream): String; overload;
function MakeBase64Package(Buffer: Pointer; Size: Integer): String; overload;
function UnMakeBase64Package(Base64Str: String; OutStream: TMemoryStream): Integer;

function MakeDateTimeMd5 (ServerTime: TDateTime; BaseStr: String): String;
function MakeDateTimeSerialMd5 (ServerTime: TDateTime; BaseStr: String; Out Md5_1, Md5_2: String): BOOL;

implementation
uses Md5, DateUtils, SysUtils;

function ZeroDateTimeMinute (SrcTime: TDateTime): TDateTime;
var
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
begin
  DecodeDateTime (SrcTime, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
  ASecond := 0;
  AMilliSecond := 0;
  Result := EncodeDateTime (AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
end;

function MakeDateTimeMd5 (ServerTime: TDateTime; BaseStr: String): String;
var
  TempTime: TDateTime;
  TimeStr, CalcuStr: String;
begin
  TempTime := ZeroDateTimeMinute (ServerTime);
  TimeStr := DateTimeToStr (TempTime);
  CalcuStr := BaseStr + TimeStr;
  Result := StringMD5Str (CalcuStr);
end;

function MakeDateTimeSerialMd5 (ServerTime: TDateTime; BaseStr: String; Out Md5_1, Md5_2: String): BOOL;
var
  TempTime: TDateTime;
  TimeStr, CalcuStr: String;
begin
  TempTime := ZeroDateTimeMinute (ServerTime);
  TimeStr := DateTimeToStr (TempTime);
  CalcuStr := BaseStr + TimeStr;
  Md5_1 := StringMD5Str (CalcuStr);

  TempTime := IncMinute (TempTime);
  TimeStr := DateTimeToStr (TempTime);
  CalcuStr := BaseStr + TimeStr;
  Md5_2 := StringMD5Str (CalcuStr);

  Result := True;
end;

function MakeBase64Package(Buffer: Pointer; Size: Integer): String;
var
  InStream: TMemoryStream;
begin
  InStream := TMemoryStream.Create;
  try
    InStream.Seek(0, soFromBeginning);
    InStream.Write(Buffer^, Size);
    Result := MakeBase64Package (InStream);
  finally
    InStream.Free;
  end;
end;

function MakeBase64Package(InStream: TMemoryStream): String;
var
  OutMM: TMemoryStream;
  OutSize: Integer;
begin
  OutMM := TMemoryStream.Create;
  try
    InStream.Seek(0, soFromBeginning);
    TDLZCompress(InStream, OutMM);

    SetLength(Result, OutMM.Size * 2);
    OutSize := EncodeBase64(OutMM.Memory, OutMM.Size, Result[1]);
    SetLength(Result, OutSize);
  finally
    OutMM.Free;
  end;
end;


function UnMakeBase64Package(Base64Str: String; OutStream: TMemoryStream): Integer;
var
  TmpMM: TMemoryStream;
  TmpSize: Integer;
  Base64Len: Integer;
begin
  TmpMM := TMemoryStream.Create;
  try
    Base64Len := Length(Base64Str);
    TmpSize := Base64Len * 2;
    TmpMM.SetSize(TmpSize);
    TmpSize := DecodeBase64(PChar(Base64Str), Base64Len, TmpMM.Memory^);
    TmpMM.SetSize(TmpSize);

    TmpMM.Seek(0, soFromBeginning);
    TDLZDecompress(TmpMM, OutStream);

    Result := OutStream.Size;
  finally
    TmpMM.Free;
  end;
end;


end.
