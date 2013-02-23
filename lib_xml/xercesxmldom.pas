
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{       Xerces XML DOM Implementation Wrapper           }
{                                                       }
{ Copyright (c) 2002 Borland Software Corporation       }
{                                                       }
{*******************************************************}

unit xercesxmldom;

interface

uses
  xmldom;

{$IFDEF MSWINDOWS}
{$HPPEMIT '#pragma link "xercesxmldom.obj"'}
{$ENDIF}

{$IFDEF LINUX}
{$HPPEMIT '#pragma link "xercesxmldom.o"'}
{$ENDIF}

const
  SXercesXML = 'Xerces XML';  { Do not localize }

type

{ TXercesDOMImplementationFactory }

  TXercesDOMImplementationFactory = class(TDOMVendor)
  private
    FLibHandle: Integer;
    FDescription: string;
    FLibName: string;
  protected
    procedure InitLibrary;
  public
    constructor Create(const ADesc, ALibName: string);
    destructor Destroy; override;
    function DOMImplementation: IDOMImplementation; override;
    function Description: String; override;
    procedure EnablePreserveWhitespace(Value: Boolean);
  end;

var
  XercesDOM: TXercesDOMImplementationFactory;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows, ComObj, ActiveX,
{$ENDIF}
{$IFDEF LINUX}
  types, Libc,
{$ENDIF}
  XMLConst, Classes, SysUtils;

const

  { Wrapper specific error codes }
  ERR_XMLDOM_TRANSFORMARGS = $0101;
  ERR_XMLDOM_WRITEFILE     = $0102;
  ERR_XMLDOM_NOTSUPPORTED  = $0103;

  { Standard DOM message }
  DOMMessages: array[INDEX_SIZE_ERR..INVALID_ACCESS_ERR] of string =
  (SINDEX_SIZE_ERR, SDOMSTRING_SIZE_ERR,  SHIERARCHY_REQUEST_ERR,
   SWRONG_DOCUMENT_ERR, SINVALID_CHARACTER_ERR, SNO_DATA_ALLOWED_ERR,
   SNO_MODIFICATION_ALLOWED_ERR, SNOT_FOUND_ERR, SNOT_SUPPORTED_ERR,
   SINUSE_ATTRIBUTE_ERR, SINVALID_STATE_ERR, SSYNTAX_ERR,
   SINVALID_MODIFICATION_ERR, SNAMESPACE_ERR, SINVALID_ACCESS_ERR);

  { Library file names }
{$IFDEF MSWINDOWS}
  SXercesLibName = 'xercesxmldom.dll'; 		        { Do not localize }
{$ENDIF}
{$IFDEF LINUX}
  SXercesLibName = 'libxercesxmldom.so.1'; 		{ Do not localize }
{$ENDIF}

  { Library export names }
  SDoneXML = 'DoneXML';					{ Do not localize }
  SGetDOMImpl = 'GetDOMImplementation';			{ Do not localize }
  SSetCallback = 'SetCallback';				{ Do not localize }
  SSetOption = 'SetOption';				{ Do not localize }

type
  { Library export types & signatures }
  TCallbackType = (cbExceptionMsg, cbNoVendorSppt{deprecated}, cbWStrAlloc, cbGetIStrAdapt);
  TOptionType = (opEnablePreserveWhitespace);
  TGetDOMProc = procedure (out DOMImplementation: IDOMImplementation); stdcall;
  TSetCallbackProc = procedure (cbType: TCallbackType; Proc: Pointer); stdcall;
  TSetOptionProc = procedure (opType: TOptionType; Value: ULONG); stdcall;
  TDoneXMLProc = procedure; stdcall;

{$IFDEF MSWINDOWS}
function LoadXMLLibrary(const LibName: string): Integer;
begin
  Result := LoadLibrary(PChar(LibName));
  if Result = 0 then
    RaiseLastOSError;
end;

procedure SetSafeCallExceptionMsg(const Msg: WideString);
var
  ErrorInfo: ICreateErrorInfo;
begin
  OleCheck(CreateErrorInfo(ErrorInfo));
  OleCheck(ErrorInfo.SetDescription(PWideChar(Msg)));
  OleCheck(SetErrorInfo(0, (ErrorInfo as IErrorInfo)));
end;
{$ENDIF}

{$IFDEF LINUX}
function LoadXMLLibrary(const LibName: string): Integer;
begin
  Result := LoadLibrary(PChar(LibName));
  if Result = 0 then
    raise Exception.CreateFmt(SErrorLoadingLib, [LibName, dlerror]);
end;
{$ENDIF}

function GetErrorMessage(const Id: LongWord; const Param: string): WideString;
begin
  if (id >= INDEX_SIZE_ERR) and (id <= INVALID_ACCESS_ERR) then
    Result := SDOMError + DOMMessages[id]
  else
  begin
    case Id of
      ERR_XMLDOM_TRANSFORMARGS: Result := SBadTransformArgs;
      ERR_XMLDOM_WRITEFILE:     Result := Format(SErrorWritingFile, [Param]);
      ERR_XMLDOM_NOTSUPPORTED:  Result := Format(SDOMNotSupported, [Param, SXercesXML]);
    else
      if Param <> '' then
        Result := Param
      else
        Result := Format(SUnhandledXercesErr, [id]);
    end;
  end;
end;

{ Callbacks }

procedure WideStringAlloc(var Dest: WideString; Source: PWideChar; CharLength: Integer) stdcall;
begin
  SetString(Dest, Source, CharLength);
end;

procedure GetIStreamAdapter(const Stream: TStream; var Adapter: IStream); stdcall;
begin
  Adapter := TStreamAdapter.Create(Stream);
end;

procedure SetExceptionMessage(const Msg: WideString; const Id: LongWord); stdcall;
begin
  SetSafeCallExceptionMsg(GetErrorMessage(Id, Msg))
end;

{ TXercesDOMImplementationFactory }

constructor TXercesDOMImplementationFactory.Create(const ADesc,
  ALibName: string);
begin
  FDescription := ADesc;
  FLibName := ALibName;
end;

destructor TXercesDOMImplementationFactory.Destroy;
var
  DoneXML: TDoneXMLProc;
begin
  if FLibHandle > 0 then
  begin
    DoneXML := GetProcAddress(FLibHandle, SDoneXML);
    DoneXML;
    FreeLibrary(FLibHandle);
    FLibHandle := 0;
  end;
end;

function TXercesDOMImplementationFactory.Description: String;
begin
  Result := FDescription;
end;

procedure TXercesDOMImplementationFactory.InitLibrary;
var
  SetCallbackProc: TSetCallbackProc;
begin
  if FLibHandle = 0 then
  begin
    FLibHandle := LoadXMLLibrary(FLibName);
    SetCallbackProc := GetProcAddress(FLibHandle, SSetCallback);
    SetCallbackProc(cbExceptionMsg, @SetExceptionMessage);
    SetCallbackProc(cbWStrAlloc, @WideStringAlloc);
    SetCallbackProc(cbGetIStrAdapt, @GetIStreamAdapter);
  end;
end;

function TXercesDOMImplementationFactory.DOMImplementation: IDOMImplementation;
var
  GetDOMProc: TGetDOMProc;
begin
  InitLibrary;
  GetDOMProc := GetProcAddress(FLibHandle, SGetDOMImpl);
  GetDOMProc(Result);
end;

procedure TXercesDOMImplementationFactory.EnablePreserveWhitespace(Value: Boolean);
var
  SetOptionProc: TSetOptionProc;
begin
  InitLibrary;
  SetOptionProc := GetProcAddress(FLibHandle, SSetOption);
  SetOptionProc(opEnablePreserveWhitespace, Abs(Ord(Value)));
end;


initialization
  XercesDOM := TXercesDOMImplementationFactory.Create(SXercesXML, SXercesLibName);
  RegisterDOMVendor(XercesDOM);
finalization
  UnRegisterDOMVendor(XercesDOM);
  XercesDOM.Free;
end.
