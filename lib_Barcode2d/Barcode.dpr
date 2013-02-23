program Barcode;

{$APPTYPE CONSOLE}

{$R 'PtDlls.res' 'PtDlls.rc'}

uses
  SysUtils,
  classes,
  TrapApiHook,
  Windows,
  DLLLoader,
  PtBarcodeDec in 'PtBarcodeDec.pas',
  PtBarcodeEnc in 'PtBarcodeEnc.pas',
  PtBarcodeImage in 'PtBarcodeImage.pas',
  barcodeUnit in 'barcodeUnit.pas';

//bmp,tiff,jpg,jpeg,gif,tga,png,pcx,ioc,wmf,jp2
const
  ImageName : array[0..10] of string = (
    '.BMP','.TIFF','.JPG','.JPEG','.GIF','.TGA','.PNG','.PCX','.ICO','.WMF','.JP2'
  );

function IsOKImageName (FileName: String): BOOL;
var
  FileExt: String;
  Index: Integer;
begin
  Result := False;
  FileExt := ExtractFileExt (FileName);
  FileExt := UpperCase (FileExt);
  for Index := 0 to High(ImageName) do
    if ImageName[Index] = FileExt then
    begin
      Result := True;
      Exit;
    end;
end;

Function CheckInputParams (ParamSL: TStringList): BOOL;
var
  IterStr, FromStr, ToStr, TempStr: String;
begin
  Result := True;
  
  //在没有显式知道Form参数的时候，将会查找参数列表中第一个文件代替
  FromStr := ParamSL.Values['FROM'];
  if FromStr = '' then
    for IterStr in ParamSL do
      if FileExists (IterStr) then
      begin
        FromStr := IterStr;
        ParamSL.Values['FROM'] := FromStr;
        Break;
      end;

  if FromStr = '' then
  begin
    Result := False;
    Exit;
  end;

  //输出文件名，只支持指定的格式
  ToStr := ParamSL.Values['TO'];
  if ToStr = '' then
    for IterStr in ParamSL do
      if FromStr <> IterStr then
      begin
        TempStr := ExtractFilePath(IterStr);
        if DirectoryExists (TempStr) then
        begin
          ToStr := IterStr;
          ParamSL.Values['TO'] := ToStr;
          Break;
        end;
      end;

  TempStr := ParamSL.Values['TYPE'];
  if TempStr = '' then
  begin
    if IsOKImageName (FromStr) then
      ParamSL.Values['TYPE'] := 'QRDECODE'
    else
      ParamSL.Values['TYPE'] := 'QRENCODE';
  end;

  if ToStr = '' then
  begin
    TempStr := UpperCase (ParamSL.Values['TYPE']);
    if Pos ('ENCODE', TempStr) > 0 then
    begin
        ToStr := ChangeFileExt (FromStr, '.PNG');
        ParamSL.Values['TO'] := ToStr;
    end else
    begin
        ToStr := ChangeFileExt (FromStr, '.TXT');
        ParamSL.Values['TO'] := ToStr;
    end;
  end;
end;


function MakeInputParamSL: TStringList;
var
  Index, PosIndex: Integer;
  ParamSte, Key: String;
begin
  Result := TStringList.Create;
  for Index := 1 to ParamCount do
  begin
    ParamSte := ParamStr(Index);
    PosIndex := Pos ('=', ParamSte);
    if PosIndex > 1 then
    begin
      Key := Copy (ParamSte, 1, PosIndex - 1);
      Key := UpperCase (Key);
      Move (Key[1], ParamSte[1], PosIndex - 1);
    end;
    Result.Append (ParamSte);
  end;
end;


var
  Lib_Kernal32: THandle;
  Addr_GetModuleFileNameA: Pointer;
  Real_GetModuleFileNameA: function (hModule: HINST; lpFilename: PAnsiChar; nSize: DWORD): DWORD; stdcall;
  ImageList: TList;

const
  QrEncodeFile = 'C:\kernel32.dll';
  RegistKey = 'DemoKey';

function MeGetModuleFileNameA(hModule: HINST; lpFilename: PAnsiChar; nSize: DWORD): DWORD; stdcall;
begin
//  WriteLn ('$', IntToHex(hModule,8));

  if Assigned (ImageList) then
  if ImageList.IndexOf(Pointer(hModule)) >= 0 then
  begin
    StrCopy (lpFilename, QrEncodeFile);
    Result := Length(QrEncodeFile);
//    WriteLn (lpFilename);
    Exit;
  end;

  Result := Real_GetModuleFileNameA (hModule, lpFilename, nSize);
//  WriteLn (lpFilename);
end;

var
  ParamSL: TStringList;
  DLL_PtImageRW       :TDLLLoader;
  DLL_PtDMDecode      :TDLLLoader;
  DLL_PtDMEncode      :TDLLLoader;
  DLL_PtPDF417Decode  :TDLLLoader;
  DLL_PtPDF417Encode  :TDLLLoader;
  DLL_PtQRDecode      :TDLLLoader;
  DLL_PtQREncode      :TDLLLoader;
  OpType: String;

PROCEDURE OnBeforeAttach(Sender: Pointer; ImageBase: Pointer; ImageSize: Integer);
begin
  if not Assigned (ImageList) then
    ImageList := TList.Create;
  ImageList.Add(ImageBase);
end;


FUNCTION NewLoadLibrary (lpLibFileName: PChar): HMODULE; stdcall;
begin
  Repeat
    if not Assigned (DLL_PtImageRW) then break;
    if StrPas (lpLibFileName) <> 'PtImageRW.dll' then break;

    Result := THandle (DLL_PtImageRW.ImageBase);
    Exit;
  until True;
  Result := LoadLibrary (lpLibFileName);
end;


//ProcessExports PtInitImage      0       00D81000
//ProcessExports PtLoadImage      1       00D81240
//ProcessExports PtSaveImage      2       00D814E0
//ProcessExports PtShowImage      3       00D81800
//ProcessExports PtCreateImage    4       00D81020
//ProcessExports PtFreeImage      5       00D81090
//ProcessExports PtGetImageFrames 6       00D810D0

var
  ImageDLLIndex: TStringList;

Procedure SetEncodeImageDLL;
begin
  if not Assigned (ImageDLLIndex) then
    ImageDLLIndex := TStringList.Create;

  ImageDLLIndex.Values['5'] := 'PtCreateImage';
  ImageDLLIndex.Values['1'] := 'PtInitImage';
  ImageDLLIndex.Values['3'] := 'PtSaveImage';
  ImageDLLIndex.Values['6'] := 'PtFreeImage';
end;

Procedure SetDecodeImageDLL;
begin
  if not Assigned (ImageDLLIndex) then
    ImageDLLIndex := TStringList.Create;

  ImageDLLIndex.Values['1'] := 'PtInitImage';
  ImageDLLIndex.Values['2'] := 'PtLoadImage';
  ImageDLLIndex.Values['6'] := 'PtFreeImage';
  ImageDLLIndex.Values['7'] := 'PtGetImageFrames';
end;

FUNCTION NewGetProcAddress (hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;
var
  FuncIndex: Integer;
  FuncName: String;
begin
  Repeat
    if not Assigned (DLL_PtImageRW) then break;
    if hModule <> THandle (DLL_PtImageRW.ImageBase) then break;

    FuncIndex := Integer (lpProcName);
    if FuncIndex < 100 then
    begin
//      Write ('NewGetProcAddress ', FuncIndex, ' ');
      FuncName := ImageDLLIndex.Values[IntToStr(FuncIndex)];
    end else
      FuncName := StrPas(lpProcName);

//    WriteLn (FuncName);
    Result := DLL_PtImageRW.FindExport(FuncName);
    Exit;
  until True;
  Result := GetProcAddress (hModule, lpProcName);
end;

function LoadDLLFromRES (ResName: String): TDLLLoader;
var
  RunResult: TDLLLoaderEx;
begin
  RunResult := TDLLLoaderEx.Create;
  RunResult.NewLoadLibrary := NewLoadLibrary;
  RunResult.NewGetProcAddress := NewGetProcAddress;  
  RunResult.OnBeforeAttach := OnBeforeAttach;
  RunResult.LoadRES('PE_FILE', ResName);
  Result := RunResult;
end;

Procedure LoadRESPtImageRW;
begin
  DLL_PtImageRW := LoadDLLFromRES ('PtImageRW');
  PtInitImage := DLL_PtImageRW.FindExport('PtInitImage');
  PtLoadImage := DLL_PtImageRW.FindExport('PtLoadImage');
  PtSaveImage := DLL_PtImageRW.FindExport('PtSaveImage');
  PtShowImage := DLL_PtImageRW.FindExport('PtShowImage');
  PtCreateImage := DLL_PtImageRW.FindExport('PtCreateImage');
  PtFreeImage := DLL_PtImageRW.FindExport('PtFreeImage');
  PtGetImageFrames := DLL_PtImageRW.FindExport('PtGetImageFrames');
end;


Procedure LoadRESPtDMDecode;
begin
    DLL_PtDMDecode := LoadDLLFromRES ('PtDMDecode');
    PtDMDecodeRegister := DLL_PtDMDecode.FindExport('PtDMDecodeRegister');
    PtDMDecodeInit := DLL_PtDMDecode.FindExport('PtDMDecodeInit');
    PtDMDecode := DLL_PtDMDecode.FindExport('PtDMDecode');
    PtDMDecodeFromFile := DLL_PtDMDecode.FindExport('PtDMDecodeFromFile');
    PtDMDecodeFromBitmap := DLL_PtDMDecode.FindExport('PtDMDecodeFromBitmap');
    PtDMDecodeFree := DLL_PtDMDecode.FindExport('PtDMDecodeFree');
end;

Procedure LoadRESPtDMEncode;
begin
    DLL_PtDMEncode := LoadDLLFromRES ('PtDMEncode');
    PtDMEncodeRegister := DLL_PtDMEncode.FindExport('PtDMEncodeRegister');
    PTDMEncodeInit := DLL_PtDMEncode.FindExport('PtDMEncodeInit');
    PtDMEncode := DLL_PtDMEncode.FindExport('PtDMEncode');
    PtDMEncodeToImage := DLL_PtDMEncode.FindExport('PtDMEncodeToImage');
end;

Procedure LoadRESPtPDF417Decode;
begin
    DLL_PtPDF417Decode := LoadDLLFromRES ('PtPDF417Decode');
    PtPDF417DecodeRegister := DLL_PtPDF417Decode.FindExport('PtPDF417DecodeRegister');
    PtPDF417DecodeInit := DLL_PtPDF417Decode.FindExport('PtPDF417DecodeInit');
    PtPDF417Decode := DLL_PtPDF417Decode.FindExport('PtPDF417Decode');
    PtPDF417DecodeFromFile := DLL_PtPDF417Decode.FindExport('PtPDF417DecodeFromFile');
    PtPDF417DecodeFromBitmap := DLL_PtPDF417Decode.FindExport('PtPDF417DecodeFromBitmap');
    PtPDF417DecodeFree := DLL_PtPDF417Decode.FindExport('PtPDF417DecodeFree');
end;

Procedure LoadRESPtPDF417Encode;
begin
    DLL_PtPDF417Encode := LoadDLLFromRES ('PtPDF417Encode');
    PtPDF417EncodeRegister := DLL_PtPDF417Encode.FindExport('PtPDF417EncodeRegister');
    PTPDF417EncodeInit := DLL_PtPDF417Encode.FindExport('PtPDF417EncodeInit');
    PtPDF417Encode := DLL_PtPDF417Encode.FindExport('PtPDF417Encode');
    PtPDF417EncodeToImage := DLL_PtPDF417Encode.FindExport('PtPDF417EncodeToImage');
end;           

Procedure LoadRESPtQRDecode;
begin
    DLL_PtQRDecode := LoadDLLFromRES ('PtQRDecode');
    PtQRDecodeRegister := DLL_PtQRDecode.FindExport('PtQRDecodeRegister');
    PtQRDecodeInit := DLL_PtQRDecode.FindExport('PtQRDecodeInit');
    PtQRDecode := DLL_PtQRDecode.FindExport('PtQRDecode');
    PtQRDecodeFromFile := DLL_PtQRDecode.FindExport('PtQRDecodeFromFile');
    PtQRDecodeFromBitmap := DLL_PtQRDecode.FindExport('PtQRDecodeFromBitmap');
    PtQRDecodeFree := DLL_PtQRDecode.FindExport('PtQRDecodeFree');
end;

Procedure LoadRESPtQREncode;
begin
    DLL_PtQREncode := LoadDLLFromRES ('PtQREncode');
    PtQREncodeRegister := DLL_PtQREncode.FindExport('PtQREncodeRegister');
    PTQREncodeInit := DLL_PtQREncode.FindExport('PtQREncodeInit');
    PtQREncode := DLL_PtQREncode.FindExport('PtQREncode');
    PtQREncodeToImage := DLL_PtQREncode.FindExport('PtQREncodeToImage');
end;

function PrintErrorCode (ErrNo: Integer): String;
begin
  case ErrNo of
  $00000000: Result := 'An operation is Failed.';
  $00000001: Result := 'An operation is successful.';
  $00000100: Result := 'Error while allocating memory.';
  $00000101: Result := 'The image format unsupported.';
  $00000200: Result := 'Error while allocating the memory.';
  $00000201: Result := 'Data to be encoded is too big.';
  $00000202: Result := 'The size of image to be pasted the symbol is too small.';
  $00000203: Result := 'The image to be pasted is invalid.';
  $00000300: Result := 'Error while allocating the memory.';
  $00000301: Result := 'The image to be decode is invalid.';
  $00000302: Result := 'The parameters input to a function are invalid.';
  ELSE Result := 'Know error';
  end;              
end;


procedure ExtraBarcodeInfo(pBarCodeInfo : pPTTOTALBARCODEINFOSTRUCT; ToFile: String);
var
  MM: TMemoryStream;
  pInfo: pPTBARCODEINFOSTRUCT;
  Index:     DWORD ;
  ToMakeFile: String;
begin
  if  pBarCodeInfo.dwTotalCount<=0 then
  begin
    WriteLn ('没有发现二维码');
    exit
  end;

  MM := TMemoryStream.Create;

  Index := 0;
  pInfo:=pBarCodeInfo.pInfoList;
  Repeat
    ToMakeFile := ToFile;
    if Index > 0 then
    begin
      SetLength (ToMakeFile, Length(ToMakeFile) - Length(ExtractFileExt (ToMakeFile)));
      ToMakeFile := ToMakeFile + ' ('+ IntToStr(Index) +')' + ExtractFileExt (ToFile);
    end;

    MM.Clear;
    MM.Seek(0, soFromBeginning);
    MM.Write(pInfo.pData^, pInfo.dwDataLen);
    MM.SaveToFile(ToMakeFile);
    Inc(pInfo);

    Inc (Index);
  until Index = pBarCodeInfo.dwTotalCount;

  MM.Free;
end;

Procedure HandleDMDecode (ParamSL: TStringList);
var
  Ret : integer;
  FromFile, ToFile: String;
  DecodePara :  PTDECODEPARASTRUCT;
  BarCodeInfo : PTTOTALBARCODEINFOSTRUCT;
begin
  FromFile := ParamSL.Values['FROM'];
  ToFile   := ParamSL.Values['TO'];
  //search the whole image
  DecodePara.dwStartX   := 0;
  DecodePara.dwEndX     := 0;
  DecodePara.dwStartY   := 0;
  DecodePara.dwEndY     := 0;
  //search the all symbols in the image
  DecodePara.dwMaxCount := 0;

  PtDMDecodeRegister (RegistKey);
  PtDMDecodeInit( @BarCodeInfo );

  ret := PtDMDecodeFromFile( pChar(FromFile), @DecodePara, @BarCodeInfo );
  if ret<>PT_DMDECODE_SUCCESS  then
    WriteLn('识别错误： ', PrintErrorCode(Ret))
  else
    ExtraBarcodeInfo(@BarCodeInfo, ToFile);

  PtDMDecodeFree(@BarCodeInfo);
end;


Procedure HandlePDF417Decode (ParamSL: TStringList);
var
  Ret : integer;
  FromFile, ToFile: String;
  DecodePara :  PTDECODEPARASTRUCT;
  BarCodeInfo : PTTOTALBARCODEINFOSTRUCT;
begin
  FromFile := ParamSL.Values['FROM'];
  ToFile   := ParamSL.Values['TO'];
  //search the whole image
  DecodePara.dwStartX   := 0;
  DecodePara.dwEndX     := 0;
  DecodePara.dwStartY   := 0;
  DecodePara.dwEndY     := 0;
  //search the all symbols in the image
  DecodePara.dwMaxCount := 0;

  PtPDF417DecodeRegister (RegistKey);
  PtPDF417DecodeInit( @BarCodeInfo );

  ret := PtPDF417DecodeFromFile( pChar(FromFile), @DecodePara, @BarCodeInfo );
  if ret<>PT_PDF417DECODE_SUCCESS  then
    WriteLn('识别错误： ', PrintErrorCode(Ret))
  else
    ExtraBarcodeInfo(@BarCodeInfo, ToFile);
  PtPDF417DecodeFree(@BarCodeInfo);
end;


Procedure HandleQRDecode (ParamSL: TStringList);
var
  Ret : integer;
  FromFile, ToFile: String;
  DecodePara :  PTDECODEPARASTRUCT;
  BarCodeInfo : PTTOTALBARCODEINFOSTRUCT;
begin
  FromFile := ParamSL.Values['FROM'];
  ToFile   := ParamSL.Values['TO'];

  //search the whole image
  DecodePara.dwStartX   := 0;
  DecodePara.dwEndX     := 0;
  DecodePara.dwStartY   := 0;
  DecodePara.dwEndY     := 0;
  //search the all symbols in the image
  DecodePara.dwMaxCount := 0;

  PtQRDecodeRegister (RegistKey);
  PtQRDecodeInit( @BarCodeInfo );
  ret := PtQRDecodeFromFile( pChar(FromFile), @DecodePara, @BarCodeInfo );
  if ret<>PT_QRDECODE_SUCCESS  then
    WriteLn('识别错误： ', PrintErrorCode(Ret))
  else
    ExtraBarcodeInfo(@BarCodeInfo, ToFile);

  PtQRDecodeFree(@BarCodeInfo);
end;


Procedure HandleDMEncode (ParamSL: TStringList);
var
  ret : integer;
  m_encode : PTDMENCODESTRUCT;
  m_image : PTIMAGESTRUCT;
  FromFile, ToFile: String;
  MM: TMemoryStream;
begin
  FromFile := ParamSL.Values['FROM'];
  ToFile   := ParamSL.Values['TO'];

  PtDMEncodeRegister (RegistKey);
  PtInitImage(@m_image);
  PtDMEncodeInit(@m_encode);

  MM := TMemoryStream.Create;
  Try
    MM.LoadFromFile(FromFile);
    m_encode.pData := MM.Memory;
    m_encode.nDataLength := MM.Size;

    ret := PtDMEncode(@m_encode, @m_image);
    If ret <> PT_DMENCODE_SUCCESS Then
    begin
      WriteLn('编码错误： ', PrintErrorCode(Ret));
      Exit;
    End;

    ret := PtSaveImage( ToFile, @m_image);
    If ret <> PT_IMAGERW_SUCCESS Then
    begin
      WriteLn ('保存文件时发生错误：', PrintErrorCode(Ret));
      WriteLn ('保存文件名是：', ToFile);
    end;
      
  finally
    PtFreeImage(@m_image);
    MM.Free;
  end;
end;

Procedure HandlePDF417Encode (ParamSL: TStringList);
var
  ret : integer;
  m_encode : PTPDF417ENCODESTRUCT;
  m_image : PTIMAGESTRUCT;
  FromFile, ToFile: String;
  MM: TMemoryStream;
begin
  FromFile := ParamSL.Values['FROM'];
  ToFile   := ParamSL.Values['TO'];

  PtPDF417EncodeRegister (RegistKey);
  PtPDF417EncodeInit(@m_encode);
  PtInitImage(@m_image);

  MM := TMemoryStream.Create;
  Try
    MM.LoadFromFile(FromFile);
    m_encode.pData := MM.Memory;
    m_encode.nDataLength := MM.Size;

    ret := PtPDF417Encode(@m_encode, @m_image);
    If ret <> PT_PDF417ENCODE_SUCCESS Then
    begin
      WriteLn('编码错误： ', PrintErrorCode(Ret));
      Exit;
    End;

    ret := PtSaveImage( ToFile, @m_image);
    If ret <> PT_IMAGERW_SUCCESS Then
    begin
      WriteLn ('保存文件时发生错误：', PrintErrorCode(Ret));
      WriteLn ('保存文件名是：', ToFile);
    end;
      
  finally
    PtFreeImage(@m_image);
    MM.Free;
  end;
end;


Procedure HandleQREncode (ParamSL: TStringList);
var
  Ret : integer;
  m_encode : PTQRENCODESTRUCT;
  m_image : PTIMAGESTRUCT;
  FromFile, ToFile: String;
  MM: TMemoryStream;
begin
  FromFile := ParamSL.Values['FROM'];
  ToFile   := ParamSL.Values['TO'];

  PtQREncodeRegister (RegistKey);
  PtInitImage(@m_image);
  PtQREncodeInit(@m_encode);

  MM := TMemoryStream.Create;
  Try
    MM.LoadFromFile(FromFile);
    m_encode.pData := MM.Memory;
    m_encode.nDataLength := MM.Size;

    Ret := PtQREncode(@m_encode, @m_image);
    If ret <> PT_QRENCODE_SUCCESS Then
    begin
      WriteLn('编码错误： ', PrintErrorCode(Ret));
      Exit;
    End;

    Ret := PtSaveImage (ToFile, @m_image);
    If ret <> PT_IMAGERW_SUCCESS Then
    begin
      WriteLn ('保存文件时发生错误：', PrintErrorCode(Ret));
      WriteLn ('保存文件名是：', ToFile);
    end;

  finally
    PtFreeImage(@m_image);
    MM.Free;
  end;
end;


begin
  ParamSL := MakeInputParamSL;

//输入barcode c:\dss.txt，将默认使用QR，输出同名.png后缀文件
//输入barcode c:\dss.png，将默认使用QR，输出同名.txt后缀文件
  if not CheckInputParams (ParamSL) then
  begin
    WriteLn ('输入参数有误：');
    WriteLn (ParamSL.Text);
    ParamSL.free;
    Exit;
  end;

//  WriteLn (ParamSL.Text);

  Lib_Kernal32 := LoadLibrary ('kernel32.dll');
  Addr_GetModuleFileNameA := GetProcAddress (Lib_Kernal32, 'GetModuleFileNameA');
  Real_GetModuleFileNameA := SMHookApi (Addr_GetModuleFileNameA, @MeGetModuleFileNameA);

  LoadRESPtImageRW;
  OpType := UpperCase(ParamSL.Values['TYPE']);
                            
  if OpType = 'DMDECODE' then
  begin
    SetDecodeImageDLL;
    LoadRESPtDMDecode;
    HandleDMDecode (ParamSL);
  end else
  if OpType = 'DMENCODE' then
  begin
    SetEncodeImageDLL;
    LoadRESPtDMEncode;
    HandleDMEncode (ParamSL);
  end else
  if OpType = 'PDF417DECODE' then
  begin
    SetDecodeImageDLL;
    LoadRESPtPDF417Decode;
    HandlePDF417Decode (ParamSL);
  end else
  if OpType = 'PDF417ENCODE' then
  begin
    SetEncodeImageDLL;
    LoadRESPtPDF417Encode;
    HandlePDF417Encode (ParamSL);
  end else
  if OpType = 'QRDECODE' then
  begin
    SetDecodeImageDLL;
    LoadRESPtQRDecode;
    HandleQRDecode (ParamSL);
  end else
  if OpType = 'QRENCODE' then
  begin
    SetEncodeImageDLL;
    LoadRESPtQREncode;
    HandleQREncode (ParamSL);
  end;

  ParamSL.free;
end.

