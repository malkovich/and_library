{*******************************************************}
{                                                       }
{ Borland Delphi Visual Component Library               }
{ v2.4 http://www.philo.de                              }
{                                                       }
{ Copyright (c) 2001-2002 Borland Software Corporation  }
{ Contributions by Keith Wood (kbwood@compuserve.com)   }
{                                                       }
{*******************************************************}

unit oxmldom;

interface

uses
{$IFDEF MSWINDOWS}
  ComObj, ActiveX, Windows,
{$ENDIF}
{$IFDEF LINUX}
  Types, Libc, IdHTTP,
{$ENDIF}
  SysUtils, Classes, Variants, xmldom, xdom;

{$IF DOMWrapperVersion > 1.0}
{ This define is used for the changes to the calling conventions of the IDOMxxx
  interfaces after the release of Delphi 6 (safecall was added in many places) }
{$DEFINE WRAPVER1.1}
{$IFEND}

{$IFDEF MSWINDOWS}
{$HPPEMIT '#pragma link "oxmldom.obj"'}
{$ENDIF}

{$IFDEF LINUX}
{$HPPEMIT '#pragma link "oxmldom.o"'}
{$ENDIF}

const
  SOpenXML  = 'Open XML'; { Do not localize }

type

{ IOXDOMNodeRef }

  IOXDOMNodeRef = interface
    ['{921E0E60-BC17-11D4-9ABD-9039FDE1134E}']
    function GetDOMNode: TdomNode;
  end;

{ TOXDOMInterface }

  TOXDOMInterface = class(TInterfacedObject)
  public
    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HRESULT; override;
  end;

{ TOXDOMImplementation }


   PParseErrorInfo = ^TParseErrorInfo;
   TParseErrorInfo = record
     errorCode: Integer;
     url: WideString;
     reason: WideString;
     srcText: WideString;
     line: Integer;
     linePos: Integer;
     filePos: Integer;
   end;

  TOXDOMDocument = class;

  TOXDOMImplementation = class(TOXDOMInterface, IDOMImplementation)
  private
    FDOMImpl: TDomImplementation;
    FParseError: PParseErrorInfo;
    FDocBuilder1: TXmlDocBuilder;
    FDocBuilder2: TXmlDocBuilder;
    FXMLDomReader: TxmlStandardDomReader;
    FXMLAgent: TXmlStandardProcessorAgent;
    FTempDoc: TdomDocument;
    function GetDOMImpl: TdomImplementation;
  protected
    { IDOMImplementation }
    function hasFeature(const feature, version: DOMString): WordBool;
    function createDocumentType(const qualifiedName, publicId,
      systemId: DOMString): IDOMDocumentType; safecall;
    function createDocument(const namespaceURI, qualifiedName: DOMString;
      doctype: IDOMDocumentType): IDOMDocument; safecall;
    procedure OnXMLDeclaration(processorAgent: TXmlCustomProcessorAgent;
      locator: TdomStandardLocator; var verDl, encDl, sdDl: wideString);
  public
    constructor Create;
    destructor Destroy; override;
    { Parsing Helpers for IDOMPersist }
    procedure FreeDocument(var Doc: TdomDocument);
    procedure InitParserAgent;
    function loadFromStream(const stream: TStream; const Doc: TOXDOMDocument;
      var ParseError: TParseErrorInfo): WordBool;
    function loadxml(const Value: DOMString; const Doc: TdomDocument;
      var ParseError: TParseErrorInfo): WordBool;
    procedure ParseErrorHandler(processorAgent: TXmlCustomProcessorAgent;
      e: TXmlParserError);
    property DOMImpl: TdomImplementation read GetDOMImpl;
  end;

{ TOXDOMNode }

  TOXDOMNode = class;
  TOXDOMNodeClass = class of TOXDOMNode;

  TOXDOMNode = class(TOXDOMInterface, IOXDOMNodeRef,
    IDOMNode, IDOMNodeEx, IDOMNodeSelect)
  private
    FNode: TdomNode;
    FOXDOMDocument: TOXDOMDocument;
    FChildNodes: IDOMNodeList;
    FAttributes: IDOMNamedNodeMap;
    FOwnerDocument: IDOMDocument;
  protected
    { IOXDOMNodeRef }
    function GetDOMNode: TdomNode;
    { IDOMNode }
    function get_nodeName: DOMString; safecall;
    function get_nodeValue: DOMString; safecall;
    procedure set_nodeValue(value: DOMString); safecall; 
    function get_nodeType: DOMNodeType; safecall;
    function get_parentNode: IDOMNode; safecall;
    function get_childNodes: IDOMNodeList; virtual; safecall;
    function get_firstChild: IDOMNode; safecall;
    function get_lastChild: IDOMNode; safecall;
    function get_previousSibling: IDOMNode; safecall;
    function get_nextSibling: IDOMNode; safecall;
    function get_attributes: IDOMNamedNodeMap; safecall;
    function get_ownerDocument: IDOMDocument; safecall;
    function get_namespaceURI: DOMString; safecall;
    function get_prefix: DOMString; safecall;
    function get_localName: DOMString; safecall;
    function insertBefore(const newChild, refChild: IDOMNode): IDOMNode; safecall;
    function replaceChild(const newChild, oldChild: IDOMNode): IDOMNode; safecall;
    function removeChild(const childNode: IDOMNode): IDOMNode; safecall;
    function appendChild(const newChild: IDOMNode): IDOMNode; safecall;
    function hasChildNodes: WordBool; virtual; safecall;
    function cloneNode(deep: WordBool): IDOMNode; safecall;
    procedure normalize; safecall; 
    function supports(const feature, version: DOMString): WordBool;
    { IDOMNodeEx }
    function get_text: DOMString; safecall;
    function get_xml: DOMString; safecall;
    procedure set_text(const Value: DOMString); safecall;
    procedure transformNode(const stylesheet: IDOMNode; var output: WideString); overload; safecall; 
    procedure transformNode(const stylesheet: IDOMNode; const output: IDOMDocument); overload; safecall; 
    { IDOMNodeSelect }
    function selectNode(const nodePath: WideString): IDOMNode; safecall;
    function selectNodes(const nodePath: WideString): IDOMNodeList; safecall;
  public
    constructor Create(ANode: TdomNode; OXDOMDocument: TOXDOMDocument); virtual;
    destructor Destroy; override;
    property Node: TdomNode read FNode;
    property OXDOMDocument: TOXDOMDocument read FOXDOMDocument;
  end;

{ TOXDOMNodeList }

  TOXDOMNodeList = class(TOXDOMInterface, IDOMNodeList)
  private
     FNodeList: TdomNodeList;
     FOXDOMNode: TOXDOMNode;
  protected
    { IDOMNodeList }
    function get_item(index: Integer): IDOMNode; safecall;
    function get_length: Integer; safecall;
  public
    constructor Create(ANodeList: TdomNodeList; OXDOMNode: TOXDOMNode);
    property NodeList: TdomNodeList read FNodeList;
  end;

{ TOXDOMNamedNodeMap }

  TOXDOMNamedNodeMap = class(TOXDOMInterface, IDOMNamedNodeMap)
  private
    FNamedNodeMap: TdomNamedNodeMap;
    FOXDOMNode: TOXDOMNode;
    procedure CheckNamespaceAware;
  protected
    { IDOMNamedNodeMap }
    function get_item(index: Integer): IDOMNode; safecall;
    function get_length: Integer; safecall; 
    function getNamedItem(const name: DOMString): IDOMNode; safecall;
    function setNamedItem(const arg: IDOMNode): IDOMNode; safecall;
    function removeNamedItem(const name: DOMString): IDOMNode; safecall;
    function getNamedItemNS(const namespaceURI, localName: DOMString): IDOMNode;
      safecall;
    function setNamedItemNS(const arg: IDOMNode): IDOMNode; safecall;
    function removeNamedItemNS(const namespaceURI, localName: DOMString):
      IDOMNode; safecall;
  public
    constructor Create(ANamedNodeMap: TdomNamedNodeMap; OXDOMNode: TOXDOMNode);
    property NamedNodeMap: TdomNamedNodeMap read FNamedNodeMap;
  end;

{ TOXDOMCharacterData }

  TOXDOMCharacterData = class(TOXDOMNode, IDOMCharacterData)
  private
    function GetCharacterData: TdomCharacterData;
  protected
    { IDOMCharacterData }
    function get_data: DOMString; safecall;
    procedure set_data(const data: DOMString); safecall; 
    function get_length: Integer; safecall; 
    function substringData(offset, count: Integer): DOMString; safecall; 
    procedure appendData(const data: DOMString); safecall; 
    procedure insertData(offset: Integer; const data: DOMString); safecall; 
    procedure deleteData(offset, count: Integer); safecall; 
    procedure replaceData(offset, count: Integer; const data: DOMString); safecall; 
  public
    property CharacterData: TdomCharacterData read GetCharacterData;
  end;

{ TOXDOMAttr }

  TOXDOMAttr = class(TOXDOMNode, IDOMAttr)
  private
    function GetAttribute: TdomAttr;
  protected
    { Property Get/Set }
    function get_name: DOMString; safecall; 
    function get_specified: WordBool; safecall; 
    function get_value: DOMString; safecall; 
    procedure set_value(const attributeValue: DOMString); safecall; 
    function get_ownerElement: IDOMElement; safecall; 
    { Properties }
    property name: DOMString read get_name;
    property specified: WordBool read get_specified;
    property value: DOMString read get_value write set_value;
    property ownerElement: IDOMElement read get_ownerElement;
  public
    property Attribute: TdomAttr read GetAttribute;
  end;

{ TOXDOMElement }

  TOXDOMElement = class(TOXDOMNode, IDOMElement)
  private
    function GetElement: TdomElement;
    procedure CheckNamespaceAware;
  protected
    { IDOMElement }
    function get_tagName: DOMString; safecall;
    function getAttribute(const name: DOMString): DOMString; safecall;
    procedure setAttribute(const name, value: DOMString); safecall; 
    procedure removeAttribute(const name: DOMString); safecall; 
    function getAttributeNode(const name: DOMString): IDOMAttr; safecall;
    function setAttributeNode(const newAttr: IDOMAttr): IDOMAttr; safecall;
    function removeAttributeNode(const oldAttr: IDOMAttr): IDOMAttr; safecall;
    function getElementsByTagName(const name: DOMString): IDOMNodeList;
      safecall;
    function getAttributeNS(const namespaceURI, localName: DOMString):
      DOMString; safecall;
    procedure setAttributeNS(const namespaceURI, qualifiedName,
      value: DOMString); safecall; 
    procedure removeAttributeNS(const namespaceURI, localName: DOMString); safecall; 
    function getAttributeNodeNS(const namespaceURI, localName: DOMString):
      IDOMAttr; safecall;
    function setAttributeNodeNS(const newAttr: IDOMAttr): IDOMAttr; safecall;
    function getElementsByTagNameNS(const namespaceURI,
      localName: DOMString): IDOMNodeList; safecall;
    function hasAttribute(const name: DOMString): WordBool; safecall;
    function hasAttributeNS(const namespaceURI, localName: DOMString): WordBool; safecall; 
    procedure normalize; safecall; 
  public
    property Element: TdomElement read GetElement;
  end;

{ TOXDOMText }

  TOXDOMText = class(TOXDOMCharacterData, IDOMText)
  protected
    function splitText(offset: Integer): IDOMText; safecall;
  end;

{ TOXDOMComment }

  TOXDOMComment = class(TOXDOMCharacterData, IDOMComment)
  end;

{ TOXDOMCDATASection }

  TOXDOMCDATASection = class(TOXDOMText, IDOMCDATASection)
  end;

{ TOXDOMDocumentType }

  TOXDOMDocumentTypeChildren = class;

  TOXDOMDocumentType = class(TOXDOMNode, IDOMDocumentType)
  private
    FDocumentTypeChildren: TOXDOMDocumentTypeChildren;
    FEntities: IDOMNamedNodeMap;
    FNotations: IDOMNamedNodeMap;
    function GetDocumentType: TdomDocumentType;
  protected
    function get_childNodes: IDOMNodeList; override; safecall;
    function hasChildNodes: WordBool; override; safecall;
    { IDOMDocumentType }
    function get_name: DOMString; safecall;
    function get_entities: IDOMNamedNodeMap; safecall;
    function get_notations: IDOMNamedNodeMap; safecall;
    function get_publicId: DOMString; safecall;
    function get_systemId: DOMString; safecall;
    function get_internalSubset: DOMString; safecall;
  public
    constructor Create(ANode: TdomNode; OXDOMDocument: TOXDOMDocument); override;
    destructor Destroy; override;
    property DocumentType: TdomDocumentType read GetDocumentType;
  end;

  TOXDOMDocumentTypeChildren = class(TInterfacedObject, IDOMNodeList)
  private
    FDocumentType: TOXDOMDocumentType;
  protected
    { IDOMNodeList }
    function get_item(index: Integer): IDOMNode; safecall;
    function get_length: Integer; safecall;
  public
    constructor Create(DocumentType: TOXDOMDocumentType);
  end;

{ TOXDOMNotation }

  TOXDOMNotation = class(TOXDOMNode, IDOMNotation)
  private
    function GetNotation: TdomNotation;
  protected
    { IDOMNotation }
    function get_publicId: DOMString; safecall;
    function get_systemId: DOMString; safecall;
  public
    property Notation: TdomNotation read GetNotation;
  end;

{ TOXDOMEntity }

  TOXDOMEntity = class(TOXDOMNode, IDOMEntity)
  private
    function GetEntity: TdomEntity;
  protected
    { IDOMEntity }
    function get_publicId: DOMString; safecall;
    function get_systemId: DOMString; safecall;
    function get_notationName: DOMString; safecall;
  public
    property Entity: TdomEntity read GetEntity;
  end;

{ TOXDOMEntityReference }

  TOXDOMEntityReference = class(TOXDOMNode, IDOMEntityReference)
  end;

{ TOXDOMProcessingInstruction }

  TOXDOMProcessingInstruction = class(TOXDOMNode, IDOMProcessingInstruction)
  private
    function GetProcessingInstruction: TdomProcessingInstruction;
  protected
    { IDOMProcessingInstruction }
    function get_target: DOMString; safecall;
    function get_data: DOMString; safecall;
    procedure set_data(const value: DOMString); safecall; 
  public
    property ProcessingInstruction: TdomProcessingInstruction
      read GetProcessingInstruction;
  end;

{ TOXDOMDocumentFragment }

  TOXDOMDocumentFragment = class(TOXDOMNode, IDOMDocumentFragment)
  end;

{ TOXDOMDocument }

  TOXDOMDocument = class(TOXDOMNode, IDOMDocument, IDOMParseOptions,
    IDOMPersist, IDOMParseError, IDOMXMLProlog)
  private
    FDOMImpl: TOXDOMImplementation;
    FDocIsOwned: Boolean;
    FParseError: TParseErrorInfo;
    FPreserveWhitespace: Boolean;
    FDocumentElement: IDOMElement;
    FDocElementSource: TdomElement;
    function GetDocument: TdomDocument;
  protected
    { IDOMDocument }
    function get_doctype: IDOMDocumentType; safecall;
    function get_domImplementation: IDOMImplementation; safecall;
    function get_documentElement: IDOMElement; safecall;
    procedure set_documentElement(const DOMElement: IDOMElement); safecall; 
    function createElement(const tagName: DOMString): IDOMElement; safecall;
    function createDocumentFragment: IDOMDocumentFragment; safecall;
    function createTextNode(const data: DOMString): IDOMText; safecall;
    function createComment(const data: DOMString): IDOMComment; safecall;
    function createCDATASection(const data: DOMString): IDOMCDATASection;
      safecall;
    function createProcessingInstruction(const target,
      data: DOMString): IDOMProcessingInstruction; safecall;
    function createAttribute(const name: DOMString): IDOMAttr; safecall;
    function createEntityReference(const name: DOMString): IDOMEntityReference;
      safecall;
    function getElementsByTagName(const tagName: DOMString): IDOMNodeList;
      safecall;
    function importNode(importedNode: IDOMNode; deep: WordBool): IDOMNode;
      safecall;
    function createElementNS(const namespaceURI,
      qualifiedName: DOMString): IDOMElement; safecall;
    function createAttributeNS(const namespaceURI,
      qualifiedName: DOMString): IDOMAttr; safecall;
    function getElementsByTagNameNS(const namespaceURI,
      localName: DOMString): IDOMNodeList; safecall;
    function getElementById(const elementId: DOMString): IDOMElement; safecall; 
    { IDOMParseOptions }
    function get_async: Boolean; safecall; 
    function get_preserveWhiteSpace: Boolean; safecall; 
    function get_resolveExternals: Boolean; safecall; 
    function get_validate: Boolean; safecall; 
    procedure set_async(Value: Boolean); safecall; 
    procedure set_preserveWhiteSpace(Value: Boolean); safecall; 
    procedure set_resolveExternals(Value: Boolean); safecall; 
    procedure set_validate(Value: Boolean); safecall; 
    { IDOMPersist }
    function get_xml: DOMString; safecall;
    function asyncLoadState: Integer; safecall;
    function load(source: OleVariant): WordBool; safecall;
    function loadFromStream(const stream: TStream): WordBool; overload; safecall;
    function loadxml(const Value: DOMString): WordBool; safecall;
    procedure save(destination: OleVariant); safecall;
    procedure saveToStream(const stream: TStream); overload; safecall;
    procedure set_OnAsyncLoad(const Sender: TObject;
      EventHandler: TAsyncEventHandler); safecall;
{$IFDEF WRAPVER1.1}
    function loadFromStream(const stream: IStream): WordBool; overload; safecall;
    procedure saveToStream(const stream: IStream); overload; safecall;
{$ENDIF}
    { IDOMParseError }
    function get_errorCode: Integer; safecall; 
    function get_url: WideString; safecall;
    function get_reason: WideString; safecall;
    function get_srcText: WideString; safecall;
    function get_line: Integer; safecall; 
    function get_linepos: Integer; safecall; 
    function get_filepos: Integer; safecall; 
    { IDOMXMLProlog }
    function get_Encoding: DOMString; safecall;
    function get_Standalone: DOMString; safecall;
    function get_Version: DOMString; safecall;
    procedure set_Encoding(const Value: DOMString); safecall;
    procedure set_Standalone(const Value: DOMString); safecall;
    procedure set_Version(const Value: DOMString); safecall;
  public
    constructor Create(OXDOMImpl: TOXDOMImplementation; ADoc: TdomDocument;
      DocIsOwned: Boolean); reintroduce;
    destructor Destroy; override;
    property Document: TdomDocument read GetDocument;
    property PreserveWhitespace: Boolean read FPreserveWhitespace;
  end;

{ TOXDOMImplementationFactory }

  TOXDOMImplementationFactory = class(TDOMVendor)
  private
    FGlobalDOMImpl: IDOMImplementation;
  public
    function DOMImplementation: IDOMImplementation; override;
    function Description: String; override;
  end;

var
  OpenXMLFactory: TOXDOMImplementationFactory;

implementation

uses XMLConst;

var
  ParserLanguage: TXmlParserLanguage;

threadvar
  GlobalOXDOM: TOXDOMImplementation;

{ Utility Functions }

function MakeNode(Node: TdomNode; OXDOMDocument: TOXDOMDocument): IDOMNode;
const
  NodeClasses: array [xdom.TDomNodeType] of TOXDOMNodeClass =
    (TOXDOMNode, TOXDOMElement, TOXDOMAttr, TOXDOMText, TOXDOMCDataSection,
     TOXDOMEntityReference, TOXDOMEntity, TOXDOMProcessingInstruction,
     TOXDOMComment, TOXDOMDocument, TOXDOMDocumentType, TOXDOMDocumentFragment,
     TOXDOMNotation);
begin
  if Node <> nil then
    Result := NodeClasses[Node.NodeType].Create(Node, OXDOMDocument)
  else
    Result := nil;
end;

function MakeNodeList(const NodeList: TdomNodeList; OwnerNode: TOXDOMNode): IDOMNodeList;
begin
  Result := TOXDOMNodeList.Create(NodeList, OwnerNode);
end;

function MakeNamedNodeMap(const NamedNodeMap: TdomNamedNodeMap; OwnerNode: TOXDOMNode):
  IDOMNamedNodeMap;
begin
  Result := TOXDOMNamedNodeMap.Create(NamedNodeMap, OwnerNode);
end;

function GetOXNode(const Node: IDOMNode): TdomNode;
begin
  if not Assigned(Node) then
    raise DOMException.Create(SNodeExpected);
  Result := (Node as IOXDOMNodeRef).GetDOMNode;
end;

function TOXDOMInterface.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HRESULT;
const
  E_FAIL = HRESULT($80004005);
{$IFDEF MSWINDOWS}
var
  HelpFile: string;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if ExceptObject is EOleException then
    HelpFile := (ExceptObject as EOleException).HelpFile;
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IDOMNode, '', Helpfile);
{$ELSE}
{$IFDEF LINUX}
  if ExceptObject is Exception then
    SysUtils.SetSafeCallExceptionMsg(Exception(ExceptObject).Message);
  Result := E_FAIL;
{$ELSE}
  DOMVendorNotSupported('SafeCallException', SOpenXML); { Do not localize }
{$ENDIF}
{$ENDIF}
end;

procedure InitParserLanguage;

  function GetLanguageCode: string;
  {$IFDEF MSWINDOWS}
  var
    Buffer: array [0..255] of Char;
  begin
    if GetLocaleInfo(GetThreadLocale, LOCALE_SISO639LANGNAME, @Buffer, SizeOf(Buffer)) > 0 then
      Result := LowerCase(Buffer);
  end;
  {$ENDIF}
  {$IFDEF LINUX}
  var
    LangCode: PChar;
  begin
    LangCode := getenv('LANG');
    if not Assigned(LangCode) or (LangCode^ = #0) then
      LangCode := setlocale(LC_ALL, '');
    if Assigned(LangCode) and (LangCode^ <> #0) then
    begin
      SetString(Result, LangCode, 2);
      Result := LowerCase(Result);
    end;
  end;
  {$ENDIF}
  
var
  LangCode: string;
begin
  LangCode := GetLanguageCode;
  if LangCode = 'de' then          { Do not localize }
    ParserLanguage := iso639_de
  else if LangCode = 'es' then     { Do not localize }
    ParserLanguage := iso639_es
  else if LangCode = 'fr' then     { Do not localize }
    ParserLanguage := iso639_fr
  else if LangCode = 'it' then     { Do not localize }
    ParserLanguage := iso639_it
  else if LangCode = 'nl' then     { Do not localize }
    ParserLanguage := iso639_nl
  else if LangCode = 'pl' then     { Do not localize }
    ParserLanguage := iso639_pl
  else
    { Default to English if we can't determine the language }
    ParserLanguage := iso639_en;
end;

{ TDOMIStreamAdapter }

type
  TDOMIStreamAdapter = class(TStream)
  private
    FStream: IStream;
  public
    constructor Create(const Stream: IStream);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

constructor TDOMIStreamAdapter.Create(const Stream: IStream);
begin
  inherited Create;
  FStream := Stream;
end;

function TDOMIStreamAdapter.Read(var Buffer; Count: Longint): Longint;
begin
  FStream.Read(@Buffer, Count, @Result)
end;

function TDOMIStreamAdapter.Write(const Buffer; Count: Longint): Longint;
begin
  FStream.Write(@Buffer, Count, @Result);
end;

function TDOMIStreamAdapter.Seek(Offset: Longint; Origin: Word): Longint;
var
  Pos: Largeint;
begin
  FStream.Seek(Offset, Origin, Pos);
  Result := Longint(Pos);
end;

{ TOXDOMImplementation }

constructor TOXDOMImplementation.Create;
begin
  FDOMImpl := TDomImplementation.Create(nil);
  inherited;
end;

function TOXDOMImplementation.createDocument(const namespaceURI,
  qualifiedName: DOMString; doctype: IDOMDocumentType): IDOMDocument;
var
  domDoc: TdomDocument;
begin
  if (qualifiedName = '') and (namespaceURI = '') then
  begin
    domDoc := DomImpl.CreateDocument('x', nil);
    domDoc.clear;
  end
  else
    domDoc := DomImpl.CreateDocumentNS(namespaceURI, qualifiedName, nil);
  Result := TOXDOMDocument.Create(Self, domDoc, True);
end;

function TOXDOMImplementation.createDocumentType(const qualifiedName,
  publicId, systemId: DOMString): IDOMDocumentType;
begin
  DOMVendorNotSupported('createDocumentType', SOpenXML); { Do not localize }
end;

destructor TOXDOMImplementation.Destroy;
begin
  inherited;
  FreeAndNil(FXMLAgent);
end;

procedure TOXDOMImplementation.FreeDocument(var Doc: TdomDocument);
begin
  FDOMImpl.freeDocument(Doc);
end;

function TOXDOMImplementation.GetDOMImpl: TdomImplementation;
begin
  Result := FDOMImpl;
end;

function TOXDOMImplementation.hasFeature(const feature, version: DOMString):
  WordBool;
begin
  Result := DomImpl.hasFeature(feature, version);
end;

procedure TOXDOMImplementation.InitParserAgent;
var
  XMLDocReader: TxmlStandardDocReader;
begin
  if not Assigned(FXMLAgent) then
  begin
    FTempDoc := FDOMImpl.createDocument('x', nil);
    FXMLAgent := TXmlStandardProcessorAgent.Create(nil);
    XMLDocReader := TxmlStandardDocReader.create(FXMLAgent);
    FDocBuilder1 := TXmlDocBuilder.create(XMLDocReader);
    FDocBuilder1.BuildNamespaceTree := False;
    FDocBuilder1.OnXmlDeclaration := OnXMLDeclaration;
    XMLDocReader.NextHandler := FDocBuilder1;
    FXMLAgent.Reader := XMLDocReader;
    XMLDocReader.OnFatalError := ParseErrorHandler;
    XMLDocReader.OnError := ParseErrorHandler;
    XMLDocReader.OnWarning := ParseErrorHandler;

    FXMLDomReader := TxmlStandardDomReader.create(FXMLAgent);
    FDocBuilder2 := TXmlDocBuilder.create(FXMLDomReader);
    FXMLDomReader.NextHandler := FDocBuilder2;
    FDocBuilder2.BuildNamespaceTree := True;
    FXMLDomReader.PrefixMapping := True;
    FXMLDomReader.OnFatalError := ParseErrorHandler;
    FXMLDomReader.OnError := ParseErrorHandler;
    FXMLDomReader.OnWarning := ParseErrorHandler;
  end;
  FTempDoc.clear;
  FDocBuilder1.referenceNode := FTempDoc;
end;

function TOXDOMImplementation.loadFromStream(const stream: TStream;
  const Doc: TOXDOMDocument; var ParseError: TParseErrorInfo): WordBool;
begin
  FParseError := @ParseError;
  ParseError.errorCode := 0;
  InitParserAgent;
  FDocBuilder1.preserveWhitespace := Doc.PreserveWhitespace;
  Doc.Document.clear;
  FDocBuilder2.referenceNode := Doc.Document;
  try
    FXMLAgent.processStream(stream, '', '');
    if ParseError.errorCode = 0 then
      FXMLDomReader.parse(nil, FTempDoc);
    Result := (ParseError.errorCode = 0);
  except
    on e: Exception do
    begin
      Doc.Document.clear;
      ParseError.reason := e.Message;
      Result := False;
    end;
  end;
end;

function TOXDOMImplementation.loadxml(const Value: DOMString;
  const Doc: TdomDocument; var ParseError: TParseErrorInfo): WordBool;
begin
  FParseError := @ParseError;
  ParseError.errorCode := 0;
  InitParserAgent;
  Doc.clear;
  FDocBuilder2.referenceNode := Doc;
  try
    FXMLAgent.processWideString(Value, '', '');
    if ParseError.errorCode = 0 then
      FXMLDomReader.parse(nil, FTempDoc);
    Result := (ParseError.errorCode = 0);
  except
    on e: Exception do
    begin
      Doc.clear;
      ParseError.reason := e.Message;
      Result := False;
    end;
  end;
end;

procedure TOXDOMImplementation.OnXMLDeclaration(
  processorAgent: TXmlCustomProcessorAgent; locator: TdomStandardLocator;
  var verDl, encDl, sdDl: wideString);
begin
  with FDocBuilder2.referenceNode.ownerDocument do
  begin
    version := verDl;
    encoding := encDl;
    standalone := sdDl;
  end;
end;

procedure TOXDOMImplementation.ParseErrorHandler(
  processorAgent: TXmlCustomProcessorAgent; e: TXmlParserError);

  function get_errorCode: Integer;
  var
    ErrorClass: string;
  begin
    try
      ErrorClass := e.errorClass;
    except
      ErrorClass := '';
    end;
    if ErrorClass = 'EIndex_Size_Err' then
      Result := INDEX_SIZE_ERR
    else if ErrorClass = 'EDomstring_Size_Err' then
      Result := DOMSTRING_SIZE_ERR
    else if ErrorClass = 'EHierarchy_Request_Err' then
      Result := HIERARCHY_REQUEST_ERR
    else if ErrorClass = 'EWrong_Document_Err' then
      Result := WRONG_DOCUMENT_ERR
    else if ErrorClass = 'EInvalid_Character_Err' then
      Result := INVALID_CHARACTER_ERR
    else if ErrorClass = 'ENo_Data_Allowed_Err' then
      Result := NO_DATA_ALLOWED_ERR
    else if ErrorClass = 'ENo_Modification_Allowed_Err' then
      Result := NO_MODIFICATION_ALLOWED_ERR
    else if ErrorClass = 'ENot_Found_Err' then
      Result := NOT_FOUND_ERR
    else if ErrorClass = 'ENot_Supported_Err' then
      Result := NOT_SUPPORTED_ERR
    else if ErrorClass = 'EInuse_Attribute_Err' then
      Result := INUSE_ATTRIBUTE_ERR
    else if ErrorClass = 'EInvalid_State_Err' then
      Result := INVALID_STATE_ERR
    else if ErrorClass = 'ESyntax_Err' then
      Result := SYNTAX_ERR
    else if ErrorClass = 'EInvalid_Modification_Err' then
      Result := INVALID_MODIFICATION_ERR
    else if ErrorClass = 'ENamespace_Err' then
      Result := NAMESPACE_ERR
    else if ErrorClass = 'EInvalid_Access_Err' then
      Result := INVALID_ACCESS_ERR
    else
      Result := -1;
  end;

begin
  if (e.errorType = xetFatalError) or (FParseError.errorCode = 0) then
    with FParseError^ do
    begin
      errorCode := get_errorCode;
      reason := e.getErrorStr(ParserLanguage);
      srcText := e.code;
      line := e.startLine;
      linePos := e.startColumn;
      filePos := -1;
      url := e.systemId;
    end;
end;

{ TOXDOMNode }

constructor TOXDOMNode.Create(ANode: TdomNode; OXDOMDocument: TOXDOMDocument);
begin
  FNode := ANode;
  FOXDOMDocument := OXDOMDocument;
  inherited Create;
end;

destructor TOXDOMNode.Destroy;
begin
  inherited;
  FNode := nil;
end;

function TOXDOMNode.appendChild(const newChild: IDOMNode): IDOMNode;
var
  NewOXChild,
  ReturnedChild: TdomNode;
begin
  NewOXChild := GetOXNode(newChild);
  ReturnedChild := Node.appendChild(NewOXChild);
  if ReturnedChild = NewOXChild then
    Result := newChild else
    Result := MakeNode(ReturnedChild, FOXDOMDocument);
end;

function TOXDOMNode.cloneNode(deep: WordBool): IDOMNode;
begin
  Result := MakeNode(Node.CloneNode(deep), FOXDOMDocument);
end;

function TOXDOMNode.get_attributes: IDOMNamedNodeMap;
begin
  if not Assigned(FAttributes) and Assigned(Node.Attributes) then
    FAttributes := MakeNamedNodeMap(Node.Attributes, Self);
  Result := FAttributes;
end;

function TOXDOMNode.get_childNodes: IDOMNodeList;
begin
  if not Assigned(FChildNodes) then
    FChildNodes := MakeNodeList(Node.ChildNodes, Self);
  Result := FChildNodes;
end;

function TOXDOMNode.get_firstChild: IDOMNode;
begin
  Result := MakeNode(Node.FirstChild, FOXDOMDocument);
end;

function TOXDOMNode.get_lastChild: IDOMNode;
begin
  Result := MakeNode(Node.LastChild, FOXDOMDocument);
end;

function TOXDOMNode.get_localName: DOMString;
begin
  Result := Node.LocalName;
end;

function TOXDOMNode.get_namespaceURI: DOMString;
begin
  Result := Node.NamespaceURI;
end;

function TOXDOMNode.get_nextSibling: IDOMNode;
begin
  Result := MakeNode(Node.NextSibling, FOXDOMDocument);
end;

function TOXDOMNode.get_nodeName: DOMString;
begin
  Result := Node.NodeName;
end;

function TOXDOMNode.get_nodeType: DOMNodeType;
const
  NodeTypes: array [TDomNodeType] of Word =
   (0, ELEMENT_NODE, ATTRIBUTE_NODE, TEXT_NODE, CDATA_SECTION_NODE,
   ENTITY_REFERENCE_NODE, ENTITY_NODE, PROCESSING_INSTRUCTION_NODE,
   COMMENT_NODE, DOCUMENT_NODE, DOCUMENT_TYPE_NODE, DOCUMENT_FRAGMENT_NODE,
   NOTATION_NODE);
begin
  Result := NodeTypes[Node.NodeType];
end;

function TOXDOMNode.get_nodeValue: DOMString;
begin
  Result := Node.NodeValue;
end;

function TOXDOMNode.get_ownerDocument: IDOMDocument;
begin
  if not Assigned(FOwnerDocument) then
    FOwnerDocument := TOXDOMDocument.Create(GlobalOXDOM, Node.OwnerDocument, False);
  Result := FOwnerDocument;
end;

function TOXDOMNode.get_parentNode: IDOMNode;
begin
  Result := MakeNode(Node.ParentNode, FOXDOMDocument);
end;

function TOXDOMNode.get_prefix: DOMString;
begin
  Result := Node.Prefix;
end;

function TOXDOMNode.get_previousSibling: IDOMNode;
begin
  Result := MakeNode(Node.PreviousSibling, FOXDOMDocument);
end;

function TOXDOMNode.GetDOMNode: TdomNode;
begin
  Result := Node;
end;

function TOXDOMNode.hasChildNodes: WordBool;
begin
  Result := Node.HasChildNodes;
end;

function TOXDOMNode.insertBefore(const newChild, refChild: IDOMNode): IDOMNode;
begin
  Result := MakeNode(Node.InsertBefore(GetOXNode(newChild),
    GetOXNode(refChild)), FOXDOMDocument);
end;

procedure TOXDOMNode.normalize;
begin
  Node.normalize;
end;

function TOXDOMNode.removeChild(const childNode: IDOMNode): IDOMNode;
begin
  Result := MakeNode(Node.RemoveChild(GetOXNode(childNode)), FOXDOMDocument);
end;

function TOXDOMNode.replaceChild(const newChild, oldChild: IDOMNode): IDOMNode;
begin
  Result := MakeNode(Node.ReplaceChild(GetOXNode(newChild),
    GetOXNode(oldChild)), FOXDOMDocument);
end;

procedure TOXDOMNode.set_nodeValue(value: DOMString);
begin
  Node.NodeValue := value;
end;

function TOXDOMNode.supports(const feature, version: DOMString): WordBool;
begin
  Result := Node.supports(feature, version);
end;

{ IDOMNodeSelect }

function TOXDOMNode.selectNode(const nodePath: WideString): IDOMNode;
begin
  DOMVendorNotSupported('selectNode', SOpenXML); { Do not localize }
end;

function TOXDOMNode.selectNodes(const nodePath: WideString): IDOMNodeList;
begin
  DOMVendorNotSupported('selectNodes', SOpenXML); { Do not localize }
end;

{ IDOMNodeEx Interface }

function TOXDOMNode.get_text: DOMString;
begin
  Result := Node.TextContent;
end;

procedure TOXDOMNode.set_text(const Value: DOMString);
var
  Index: Integer;
begin
  for Index := 0 to Node.ChildNodes.Length - 1 do
    Node.RemoveChild(Node.ChildNodes.Item(Index));
  Node.AppendChild(Node.OwnerDocument.CreateTextNode(Value))
end;

function TOXDOMNode.get_xml: DOMString;
begin
  Result := Node.Code;
end;

procedure TOXDOMNode.transformNode(const stylesheet: IDOMNode;
  var output: WideString);
begin
  DOMVendorNotSupported('transformNode', SOpenXML); { Do not localize }
end;

procedure TOXDOMNode.transformNode(const stylesheet: IDOMNode;
  const output: IDOMDocument);
begin
  DOMVendorNotSupported('transformNode', SOpenXML); { Do not localize }
end;

{ TOXDOMNodeList }

constructor TOXDOMNodeList.Create(ANodeList: TdomNodeList; OXDOMNode: TOXDOMNode);
begin
  inherited Create;
  FNodeList := ANodeList;
  FOXDOMNode := OXDOMNode;
end;

function TOXDOMNodeList.get_item(index: Integer): IDOMNode;
begin
  Result := MakeNode(NodeList.Item(index), FOXDOMNode.OXDOMDocument);
end;

function TOXDOMNodeList.get_length: Integer;
begin
  Result := NodeList.Length;
end;

{ TOXDOMNamedNodeMap }

constructor TOXDOMNamedNodeMap.Create(ANamedNodeMap: TdomNamedNodeMap; OXDOMNode: TOXDOMNode);
begin
  inherited Create;
  FNamedNodeMap := ANamedNodeMap;
  FOXDOMNode := OXDOMNode;
end;

procedure TOXDOMNamedNodeMap.CheckNamespaceAware;
begin
  if not NamedNodeMap.namespaceAware then
    NamedNodeMap.namespaceAware := True;
end;

function TOXDOMNamedNodeMap.get_item(index: Integer): IDOMNode;
begin
  Result := MakeNode(NamedNodeMap.Item(index), FOXDOMNode.OXDOMDocument);
end;

function TOXDOMNamedNodeMap.get_length: Integer;
begin
  Result := NamedNodeMap.Length;
end;

function TOXDOMNamedNodeMap.getNamedItem(const name: DOMString): IDOMNode;
begin
  CheckNamespaceAware;
  Result := MakeNode(NamedNodeMap.GetNamedItemNS('', name), FOXDOMNode.OXDOMDocument);
end;

function TOXDOMNamedNodeMap.getNamedItemNS(const namespaceURI,
  localName: DOMString): IDOMNode;
begin
  CheckNamespaceAware;
  Result := MakeNode(NamedNodeMap.GetNamedItemNS(namespaceURI, localName), FOXDOMNode.OXDOMDocument);
end;

function TOXDOMNamedNodeMap.removeNamedItem(const name: DOMString): IDOMNode;
begin
  CheckNamespaceAware;
  Result := MakeNode(NamedNodeMap.RemoveNamedItemNS('', name), FOXDOMNode.OXDOMDocument);
end;

function TOXDOMNamedNodeMap.removeNamedItemNS(const namespaceURI,
  localName: DOMString): IDOMNode;
begin
  CheckNamespaceAware;
  Result := MakeNode(NamedNodeMap.RemoveNamedItemNS(namespaceURI, localName), FOXDOMNode.OXDOMDocument);
end;

function TOXDOMNamedNodeMap.setNamedItem(const arg: IDOMNode): IDOMNode;
begin
  CheckNamespaceAware;
  Result := MakeNode(NamedNodeMap.SetNamedItemNS(GetOXNode(arg)), FOXDOMNode.OXDOMDocument);
end;

function TOXDOMNamedNodeMap.setNamedItemNS(const arg: IDOMNode): IDOMNode;
begin
  CheckNamespaceAware;
  Result := MakeNode(NamedNodeMap.SetNamedItemNS(GetOXNode(arg)), FOXDOMNode.OXDOMDocument);
end;

{ TOXDOMCharacterData }

function TOXDOMCharacterData.GetCharacterData: TdomCharacterData;
begin
  Result := Node as TdomCharacterData;
end;

procedure TOXDOMCharacterData.appendData(const data: DOMString);
begin
  CharacterData.AppendData(data);
end;

procedure TOXDOMCharacterData.deleteData(offset, count: Integer);
begin
  CharacterData.DeleteData(offset, count);
end;

function TOXDOMCharacterData.get_data: DOMString;
begin
  Result := CharacterData.Data;
end;

function TOXDOMCharacterData.get_length: Integer;
begin
  Result := CharacterData.length;
end;

procedure TOXDOMCharacterData.insertData(offset: Integer;
  const data: DOMString);
begin
  CharacterData.InsertData(offset, data);
end;

procedure TOXDOMCharacterData.replaceData(offset, count: Integer;
  const data: DOMString);
begin
  CharacterData.ReplaceData(offset, count, data);
end;

procedure TOXDOMCharacterData.set_data(const data: DOMString);
begin
  CharacterData.Data := data;
end;

function TOXDOMCharacterData.substringData(offset, count: Integer): DOMString;
begin
  Result := CharacterData.SubstringData(offset, count);
end;

{ TOXDOMAttr }

function TOXDOMAttr.GetAttribute: TdomAttr;
begin
  Result := Node as TdomAttr;
end;

function TOXDOMAttr.get_name: DOMString;
begin
  Result := Attribute.Name;
end;

function TOXDOMAttr.get_ownerElement: IDOMElement;
begin
  Result := MakeNode(Attribute.OwnerElement, Self.OXDOMDocument) as IDOMElement;
end;

function TOXDOMAttr.get_specified: WordBool;
begin
  Result := Attribute.Specified;
end;

function TOXDOMAttr.get_value: DOMString;
begin
  Result := Attribute.Value;
end;

procedure TOXDOMAttr.set_value(const attributeValue: DOMString);
begin
  Attribute.Value := attributeValue;
end;

{ TOXDOMElement }

procedure TOXDOMElement.CheckNamespaceAware;
begin
  if not Element.attributes.namespaceAware then
    Element.attributes.namespaceAware := True;
end;

function TOXDOMElement.GetElement: TdomElement;
begin
  Result := Node as TdomElement;
end;

function TOXDOMElement.get_tagName: DOMString;
begin
  Result := Element.TagName;
end;

function TOXDOMElement.getAttribute(const name: DOMString): DOMString;
begin
  Result := Element.getAttributeNS('', name);
end;

function TOXDOMElement.getAttributeNode(const name: DOMString): IDOMAttr;
begin
  Result := MakeNode(Element.GetAttributeNodeNS('', name), Self.OXDOMDocument) as IDOMAttr;
end;

function TOXDOMElement.getAttributeNodeNS(const namespaceURI,
  localName: DOMString): IDOMAttr;
begin
  Result := MakeNode(Element.GetAttributeNodeNS(namespaceURI, localName), Self.OXDOMDocument)
    as IDOMAttr;
end;

function TOXDOMElement.getAttributeNS(const namespaceURI,
  localName: DOMString): DOMString;
begin
  Result := Element.GetAttributeNS(namespaceURI, localName);
end;

function TOXDOMElement.getElementsByTagName(const name: DOMString):
  IDOMNodeList;
begin
  Result := MakeNodeList(Element.GetElementsByTagName(name), Self.OXDOMDocument);
end;

function TOXDOMElement.getElementsByTagNameNS(const namespaceURI,
  localName: DOMString): IDOMNodeList;
begin
  Result := MakeNodeList(Element.GetElementsByTagNameNS(
    namespaceURI, localName), Self);
end;

function TOXDOMElement.hasAttribute(const name: DOMString): WordBool;
begin
  Result := Element.hasAttributeNS('', name);
end;

function TOXDOMElement.hasAttributeNS(const namespaceURI,
  localName: DOMString): WordBool;
begin
  Result := Element.hasAttributeNS(namespaceURI, localName);
end;

procedure TOXDOMElement.removeAttribute(const name: DOMString);
begin
  Element.RemoveAttributeNS('', name);
end;

function TOXDOMElement.removeAttributeNode(const oldAttr: IDOMAttr): IDOMAttr;
begin
  Result := MakeNode(Element.RemoveAttributeNode(
    GetOXNode(oldAttr) as TdomAttr), Self.OXDOMDocument) as IDOMAttr;
end;

procedure TOXDOMElement.removeAttributeNS(const namespaceURI,
  localName: DOMString);
begin
  Element.RemoveAttributeNS(namespaceURI, localName);
end;

procedure TOXDOMElement.setAttribute(const name, value: DOMString);
begin
  CheckNamespaceAware;
  if IsPrefixed(name) and (ExtractPrefix(name) = SXML) then
    Element.setAttributeNS(SXMLPrefixNamespaceURI, name, value)
  else
    Element.setAttributeNS('', name, value);
end;

function TOXDOMElement.setAttributeNode(const newAttr: IDOMAttr): IDOMAttr;
begin
  CheckNamespaceAware;
  Result := MakeNode(Element.SetAttributeNodeNS(GetOXNode(newAttr) as TdomAttr), Self.OXDOMDocument) as IDOMAttr;
end;

function TOXDOMElement.setAttributeNodeNS(const newAttr: IDOMAttr): IDOMAttr;
begin
  CheckNamespaceAware;
  Result := MakeNode(Element.SetAttributeNodeNS(
    GetOXNode(newAttr) as TdomAttr), Self.OXDOMDocument) as IDOMAttr;
end;

procedure TOXDOMElement.setAttributeNS(const namespaceURI, qualifiedName,
  value: DOMString);
begin
  CheckNamespaceAware;
  Element.SetAttributeNS(namespaceURI, qualifiedName, value);
end;

procedure TOXDOMElement.normalize;
begin
  Element.normalize;
end;

{ TOXDOMText }

function TOXDOMText.splitText(offset: Integer): IDOMText;
begin
  Result := MakeNode((Node as TdomText).SplitText(offset), Self.OXDOMDocument) as IDOMText;
end;

{ TOXDOMDocumentType }

constructor TOXDOMDocumentType.Create(ANode: TdomNode; OXDOMDocument: TOXDOMDocument);
begin
  inherited Create(ANode, OXDOMDocument);
  FDocumentTypeChildren := TOXDOMDocumentTypeChildren.Create(Self);
  FDocumentTypeChildren._AddRef;
end;

destructor TOXDOMDocumentType.Destroy;
begin
  FDocumentTypeChildren._Release;
  inherited Destroy;
end;

function TOXDOMDocumentType.GetDocumentType: TdomDocumentType;
begin
  Result := Node as TdomDocumentType;
end;

function TOXDOMDocumentType.get_childNodes: IDOMNodeList;
begin
  Result := FDocumentTypeChildren;
end;

function TOXDOMDocumentType.get_entities: IDOMNamedNodeMap;
begin
  if not Assigned(FEntities) then
    FEntities := MakeNamedNodeMap(DocumentType.Entities, Self);
  Result := FEntities;
end;

function TOXDOMDocumentType.get_internalSubset: DOMString;
begin
  Result := DocumentType.InternalSubset;
end;

function TOXDOMDocumentType.get_name: DOMString;
begin
  Result := DocumentType.Name;
end;

function TOXDOMDocumentType.get_notations: IDOMNamedNodeMap;
begin
  if not Assigned(FNotations) then
    FNotations := MakeNamedNodeMap(DocumentType.Notations, Self);
  Result := FNotations;
end;

function TOXDOMDocumentType.get_publicId: DOMString;
begin
  Result := DocumentType.PublicId;
end;

function TOXDOMDocumentType.get_systemId: DOMString;
begin
  Result := DocumentType.SystemId;
end;

function TOXDOMDocumentType.hasChildNodes: WordBool;
begin
  Result := (get_childNodes.Length > 0);
end;

{ TOXDOMDocumentTypeChildren }

constructor TOXDOMDocumentTypeChildren.Create(DocumentType: TOXDOMDocumentType);
begin
  inherited Create;
  FDocumentType := DocumentType;
end;

function TOXDOMDocumentTypeChildren.get_item(index: Integer): IDOMNode;
var
  Len: Integer;
begin
  Len := FDocumentType.get_entities.length;
  if index < Len then
    Result := FDocumentType.get_entities.item[index]
  else if index < Len + FDocumentType.get_notations.length then
    Result := FDocumentType.get_notations.item[index - len]
  else
    Result := nil;
end;

function TOXDOMDocumentTypeChildren.get_length: Integer;
begin
  Result :=
    FDocumentType.get_entities.length + FDocumentType.get_notations.length;
end;

{ TOXDOMNotation }

function TOXDOMNotation.GetNotation: TdomNotation;
begin
  Result := Node as TdomNotation;
end;

function TOXDOMNotation.get_publicId: DOMString;
begin
  Result := Notation.PublicId;
end;

function TOXDOMNotation.get_systemId: DOMString;
begin
  Result := Notation.SystemId;
end;

{ TOXDOMEntity }

function TOXDOMEntity.GetEntity: TdomEntity;
begin
  Result := Node as TdomEntity;
end;

function TOXDOMEntity.get_notationName: DOMString;
begin
  Result := Entity.NotationName;
end;

function TOXDOMEntity.get_publicId: DOMString;
begin
  Result := Entity.PublicId;
end;

function TOXDOMEntity.get_systemId: DOMString;
begin
  Result := Entity.SystemId;
end;

{ TOXDOMProcessingInstruction }

function TOXDOMProcessingInstruction.GetProcessingInstruction:
  TdomProcessingInstruction;
begin
  Result := Node as TdomProcessingInstruction;
end;

function TOXDOMProcessingInstruction.get_data: DOMString;
begin
  Result := ProcessingInstruction.Data;
end;

function TOXDOMProcessingInstruction.get_target: DOMString;
begin
  Result := ProcessingInstruction.Target;
end;

procedure TOXDOMProcessingInstruction.set_data(const value: DOMString);
begin
  ProcessingInstruction.Data := value;
end;

{ TOXDOMDocument }

constructor TOXDOMDocument.Create(OXDOMImpl: TOXDOMImplementation;
  ADoc: TdomDocument; DocIsOwned: Boolean);
begin
  FDocIsOwned := DocIsOwned;
  FDOMImpl := OXDOMImpl;
  FPreserveWhitespace := True;
  inherited Create(ADoc, Self);
end;

destructor TOXDOMDocument.Destroy;
begin
  if FDocIsOwned and Assigned(FDOMImpl) and Assigned(FNode) then
    FDOMImpl.FreeDocument(TdomDocument(FNode));
  inherited Destroy;
end;

function TOXDOMDocument.GetDocument: TdomDocument;
begin
  Result := Node as TdomDocument;
end;

function TOXDOMDocument.createAttribute(const name: DOMString): IDOMAttr;
begin
  Result := TOXDOMAttr.Create(Document.CreateAttributeNS('', name), Self);
end;

function TOXDOMDocument.createAttributeNS(const namespaceURI,
  qualifiedName: DOMString): IDOMAttr;
begin
  Result := TOXDOMAttr.Create(Document.CreateAttributeNS(
    namespaceURI, qualifiedName), Self);
end;

function TOXDOMDocument.createCDATASection(const data: DOMString):
  IDOMCDATASection;
begin
  Result := TOXDOMCDATASection.Create(Document.CreateCDATASection(data), Self);
end;

function TOXDOMDocument.createComment(const data: DOMString): IDOMComment;
begin
  Result := TOXDOMComment.Create(Document.CreateComment(data), Self);
end;

function TOXDOMDocument.createDocumentFragment: IDOMDocumentFragment;
begin
  Result := TOXDOMDocumentFragment.Create(Document.CreateDocumentFragment, Self);
end;

function TOXDOMDocument.createElement(const tagName: DOMString): IDOMElement;
begin
  Result := TOXDOMElement.Create(Document.CreateElement(tagName), Self);
end;

function TOXDOMDocument.createElementNS(const namespaceURI,
  qualifiedName: DOMString): IDOMElement;
begin
  Result := TOXDOMElement.Create(
    Document.CreateElementNS(namespaceURI, qualifiedName), Self);
end;

function TOXDOMDocument.createEntityReference(const name: DOMString):
  IDOMEntityReference;
begin
  Result := TOXDOMEntityReference.Create(
    Document.CreateEntityReference(name), Self);
end;

function TOXDOMDocument.createProcessingInstruction(const target,
  data: DOMString): IDOMProcessingInstruction;
begin
  Result := TOXDOMProcessingInstruction.Create(
    Document.CreateProcessingInstruction(target, data), Self);
end;

function TOXDOMDocument.createTextNode(const data: DOMString): IDOMText;
begin
  Result := TOXDOMText.Create(Document.CreateTextNode(data), Self);
end;

function TOXDOMDocument.get_doctype: IDOMDocumentType;
begin
  Result := TOXDOMDocumentType.Create(Document.docType, Self);
end;

function TOXDOMDocument.get_documentElement: IDOMElement;
begin
  if not Assigned(FDocumentElement) or
    (FDocElementSource <> Document.documentElement) then { Test if underlying document element changed }
  begin
    FDocElementSource := Document.documentElement;
    FDocumentElement := MakeNode(FDocElementSource, Self) as IDOMElement;
  end;
  Result := FDocumentElement;
end;

function TOXDOMDocument.get_domImplementation: IDOMImplementation;
begin
  Result := FDOMImpl;
end;

function TOXDOMDocument.getElementById(const elementId: DOMString): IDOMElement;
begin
  Result := TOXDOMElement.Create(Document.GetElementById(elementId), Self);
end;

function TOXDOMDocument.getElementsByTagName(const tagName: DOMString): IDOMNodeList;
begin
  Result := MakeNodeList(Document.GetElementsByTagName(tagName), Self);
end;

function TOXDOMDocument.getElementsByTagNameNS(const namespaceURI,
  localName: DOMString): IDOMNodeList;
begin
  Result := MakeNodeList(Document.GetElementsByTagNameNS(
    namespaceURI, localName), Self);
end;

function TOXDOMDocument.importNode(importedNode: IDOMNode; deep: WordBool):
  IDOMNode;
begin
  Result := MakeNode(Document.ImportNode(GetOXNode(importedNode), deep), Self);
end;

procedure TOXDOMDocument.set_documentElement(const DOMElement: IDOMElement);
begin
  if Assigned(DOMElement) then
  begin
    if Assigned(Document.documentElement) then
      Document.replaceChild(GetOXNode(DOMElement), Document.documentElement)
    else
      Document.appendChild(GetOXNode(DOMElement));
  end
  else if Assigned(Document.documentElement) then
      Document.removeChild(Document.documentElement);
  FDocumentElement := nil;
end;

{ IDOMParseOptions Interface }

function TOXDOMDocument.get_async: Boolean;
begin
  Result := False;
end;

function TOXDOMDocument.get_preserveWhiteSpace: Boolean;
begin
  Result := True;
end;

function TOXDOMDocument.get_resolveExternals: Boolean;
begin
  Result := False;
end;

function TOXDOMDocument.get_validate: Boolean;
begin
  Result := False;
end;

procedure TOXDOMDocument.set_async(Value: Boolean);
begin
  if Value then
    DOMVendorNotSupported('set_async(True)', SOpenXML); { Do not localize }
end;

procedure TOXDOMDocument.set_preserveWhiteSpace(Value: Boolean);
begin
  FPreserveWhitespace := Value;
end;

procedure TOXDOMDocument.set_resolveExternals(Value: Boolean);
begin
  if Value then
    DOMVendorNotSupported('set_resolveExternals(True)', SOpenXML); { Do not localize }
end;

procedure TOXDOMDocument.set_validate(Value: Boolean);
begin
  if Value then
    DOMVendorNotSupported('set_validate(True)', SOpenXML); { Do not localize }
end;

{ IDOMPersist interface }

function TOXDOMDocument.get_xml: DOMString;
var
  EncodingSave: WideString;
begin
  EncodingSave := Document.encoding;
  try
    { We want to be like MSXML and leave the encoding out completely since it's
      going to be unicode because we are returning a unicode string }
    Document.encoding := '';
    Result := Document.Code;
  finally
    Document.encoding := EncodingSave;
  end;
end;

{$IFDEF LINUX}
procedure LoadFromURL(URL: string; Stream: TStream);
var
  IndyHTTP: TIDHttp;
begin
  IndyHTTP := TIDHttp.Create(Nil);
  try
    IndyHttp.Request.Accept := 'text/xml, text/html, application/octet-stream'; { Do not localize }
    IndyHttp.Request.UserAgent := 'Mozilla/3.0 (compatible; Indy Library)'; { Do not localize }
    IndyHttp.Request.ContentType := 'text/xml';   { Do not localize }
//    IndyHttp.Request.Location := URL;
    IndyHttp.Request.Connection := URL;
    IndyHttp.Intercept := Nil;
    IndyHTTP.Get(Url, Stream);
  finally
    IndyHTTP.Free;
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
var
  UrlMonHandle: HMODULE;
  URLDownloadToCacheFile: function(Caller: IUnknown; URL: PAnsiChar;
  FileName: PAnsiChar; FileNameBufLen: DWORD; Reserved: DWORD;
    StatusCB: IInterface {IBindStatusCallback}): HResult; stdcall = nil;

procedure LoadFromURL(URL: string; Stream: TMemoryStream);

  procedure InitURLMon;
  const
    UrlMonLib = 'URLMON.DLL';                             { Do not localize }
    sURLDownloadToCacheFileA = 'URLDownloadToCacheFileA'; { Do not localize }
  begin
    if not Assigned(URLDownloadToCacheFile) then
    begin
      UrlMonHandle := LoadLibrary(UrlMonLib);
      if UrlMonHandle = 0 then
        raise Exception.CreateResFmt(@SUrlMonDllMissing, [UrlMonLib]);
      URLDownloadToCacheFile := GetProcAddress(UrlMonHandle, sURLDownloadToCacheFileA);
    end;
  end;

var
  FileName: array[0..MAX_PATH] of AnsiChar;
begin
  InitURLMon;
  if URLDownloadToCacheFile(nil, PChar(URL), FileName, SizeOf(FileName), 0, nil) <> S_OK then
    raise Exception.CreateResFmt(@SErrorDownloadingURL, [URL]);
  Stream.LoadFromFile(FileName);
end;
{$ENDIF}

function TOXDOMDocument.load(source: OleVariant): WordBool;
var
  Stream: TMemoryStream;
begin
  if VarType(source) = varOleStr then
  begin
    Stream:= TMemoryStream.create;
    try
      if LowerCase(Copy(Source, 1, 7)) = 'http://' then   { do not localize }
        LoadFromURL(Source, Stream)
      else
        Stream.LoadFromFile(Source);
      result:= loadFromStream(Stream);
    finally
      Stream.free;
    end;
  end
  else
    DOMVendorNotSupported('load(object)', SOpenXML); { Do Not Localize }
end;

function TOXDOMDocument.loadxml(const Value: DOMString): WordBool;
begin
  Result := FDOMImpl.loadxml(Value, Document, FParseError);
end;

function TOXDOMDocument.loadFromStream(const stream: TStream): WordBool;
begin
  Result := FDOMImpl.loadFromStream(stream, Self, FParseError);
end;

procedure TOXDOMDocument.save(destination: OleVariant);
var
  FStream: TFileStream;
begin
  if VarType(destination) = varOleStr then
  begin
    FStream := TFileStream.Create(destination, fmCreate);
    try
      saveToStream(FStream);
    finally
      FStream.Free;
    end;
  end
  else
    DOMVendorNotSupported('save(object)', SOpenXML); { Do Not Localize }
end;

procedure TOXDOMDocument.saveToStream(const stream: TStream);
begin
  if (Document.encoding = '') or (Document.encoding = 'UTF-8') then
    Document.writeCodeAsUTF8(stream)
  else
    Document.writeCodeAsUTF16(stream);
end;

//{$IFDEF WRAPVER1.1}
function TOXDOMDocument.loadFromStream(const stream: IStream): WordBool;
var
  LStream: TStream;
begin
  LStream := TDOMIStreamAdapter.Create(stream);
  try
    Result := loadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TOXDOMDocument.saveToStream(const stream: IStream);
var
  LStream: TStream;
begin
  LStream := TDOMIStreamAdapter.Create(stream);
  try
    SaveToStream(LStream);
  finally
    LStream.Free;
  end;
end;
//{$ENDIF}

{ IDOMParseError }

function TOXDOMDocument.get_errorCode: Integer;
begin
  Result := FParseError.errorCode;
end;

function TOXDOMDocument.get_filepos: Integer;
begin
  Result := FParseError.filePos;
end;

function TOXDOMDocument.get_line: Integer;
begin
  Result := FParseError.line;
end;

function TOXDOMDocument.get_linepos: Integer;
begin
  Result := FParseError.linePos;
end;

function TOXDOMDocument.get_reason: WideString;
begin
  Result := FParseError.reason;
end;

function TOXDOMDocument.get_srcText: WideString;
begin
  Result := FParseError.srcText;
end;

function TOXDOMDocument.get_url: WideString;
begin
  Result := FParseError.url;
end;

function TOXDOMDocument.asyncLoadState: Integer;
begin
  result := 0; { Not Supported }
end;

procedure TOXDOMDocument.set_OnAsyncLoad(const Sender: TObject;
  EventHandler: TAsyncEventHandler);
begin
  DOMVendorNotSupported('set_OnAsyncLoad', SOpenXML); { Do Not Localize }
end;

{ IDOMXMLProlog }

function TOXDOMDocument.get_Encoding: DOMString;
begin
  Result := Document.encoding;
end;

procedure TOXDOMDocument.set_Encoding(const Value: DOMString);
begin
  Document.encoding := Value;
end;

function TOXDOMDocument.get_Standalone: DOMString;
begin
  Result := Document.standalone;
end;

procedure TOXDOMDocument.set_Standalone(const Value: DOMString);
begin
  Document.standalone := Value;
end;

function TOXDOMDocument.get_Version: DOMString;
begin
  Result := Document.version;
end;

procedure TOXDOMDocument.set_Version(const Value: DOMString);
begin
  Document.version := Value;
end;

{ TOXDOMImplementationFactory }

function TOXDOMImplementationFactory.DOMImplementation: IDOMImplementation;
begin
  if not Assigned(GlobalOXDOM) then
  begin
    GlobalOXDOM := TOXDOMImplementation.Create;
    FGlobalDOMImpl := GlobalOXDOM;
  end;
  Result := FGlobalDOMImpl;
end;

function TOXDOMImplementationFactory.Description: String;
begin
  Result := SOpenXML;
end;

{ TOXDOMInterface }

initialization
  OpenXMLFactory := TOXDOMImplementationFactory.Create;
  RegisterDOMVendor(OpenXMLFactory);
  InitParserLanguage;
finalization
{$IFDEF MSWINDOWS}
  URLDownloadToCacheFile := nil;
  if UrlMonHandle <> 0 then
    FreeLibrary(UrlMonHandle);
{$ENDIF}
  UnRegisterDOMVendor(OpenXMLFactory);
  OpenXMLFactory.Free;
end.
