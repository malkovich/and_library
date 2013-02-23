unit CodeEncrypt;

interface
uses
  windows, classes, SysUtils, RC4Extractor;


function KeyModuleEncrypt(InStream: TStream; out OutBuff; out VerStr: String): Integer;
function KeyModuleDecrypt(InStream: TMemoryStream; OutStream: TStream): Integer;overload;
function KeyModuleDecrypt(InputFile, OutputFile: String): LongBool; overload;

function DynamicCodeEncrypt(InStream: TMemoryStream; RC4Key: TGUIDStr): String;
function DynamicCodeDecrypt(Base64Str: String; OutStream: TMemoryStream): Integer; overload;
function DynamicCodeDecrypt(Base64Str: String): TMemoryStream; overload;

function GetRandomGUID: TGUIDStr;

implementation

uses
  MakeRC4Code, Base64Unit, MD5, TDLZCmpr, ComObj;


function GetRandomGUID: TGUIDStr;
var
  guid: string;
begin
  guid := CreateClassID;
  CopyMemory(@result, @guid[1], GUID_SAMPLE_LENGTH);
  result[GUID_SAMPLE_LENGTH] := #0;
end;

function KeyModuleEncrypt(InStream: TStream; out OutBuff; out VerStr: String): Integer;
var
  OutStream: TMemoryStream;
begin
  OutStream := TMemoryStream.Create;
  try
    InStream.Seek(0, soFromBeginning);
    OutStream.Seek(0, soFromBeginning);
    TDLZCompress(InStream, OutStream);

    result := EncodeBase64(OutStream.Memory, OutStream.Size, OutBuff);

    VerStr := BufferMD5Str(@OutBuff, result);
  finally
    OutStream.Free;
  end;
end;

// GWS和GwDaemon.exe用这个
function KeyModuleDecrypt(InStream: TMemoryStream; OutStream: TStream): Integer;
var
  SrcBuff: PChar;
  SrcSize, RetSize: Integer;
  TemMM: TMemoryStream;
begin
  SrcSize := InStream.Size * 2;
  SrcBuff := AllocMem(SrcSize);
  TemMM := TMemoryStream.Create;
  try
    RetSize := DecodeBase64(InStream.Memory, InStream.Size, SrcBuff[0]);

    TemMM.Seek(0, soFromBeginning);
    TemMM.Write(SrcBuff[0], RetSize);

    TemMM.Seek(0, soFromBeginning);
    OutStream.Seek(0, soFromBeginning);
    TDLZDecompress(TemMM, OutStream);
  finally
    FreeMem(SrcBuff);
    TemMM.Free;
  end;
  result := OutStream.Size;
end;

// RootModule用这个
function KeyModuleDecrypt(InputFile, OutputFile: String): LongBool;
var
  InputMM: TMemoryStream;
  OutputFS: TFileStream;
  RetSize: Integer;
begin
  InputMM := TMemoryStream.Create;
  OutputFS := TFileStream.Create(OutputFile, fmOpenWrite or fmCreate);
  try
    InputMM.LoadFromFile(InputFile);

    RetSize := KeyModuleDecrypt(InputMM, OutputFS);

    Result := OutputFS.Size = RetSize;
  finally
    InputMM.Free;
    OutputFS.Free;
  end;
end;

function DynamicCodeEncrypt(InStream: TMemoryStream; RC4Key: TGUIDStr): String;
var
  OutMM: TMemoryStream;
  OutSize: Integer;
begin
  MakeRC4ToCode(InStream.Memory, InStream.Size, RC4Key);

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

function DynamicCodeDecrypt(Base64Str: String; OutStream: TMemoryStream): Integer;
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

function DynamicCodeDecrypt(Base64Str: String): TMemoryStream;
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
    Result := TMemoryStream.Create;
    Result.Seek(0, soFromBeginning);
    TDLZDecompress(TmpMM, Result); 
  finally
    TmpMM.Free;
  end;
end;


end.
