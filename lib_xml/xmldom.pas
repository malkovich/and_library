
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{       XML DOM 2.0 Interfaces                          }
{       Translated from dom.idl                         }
{                                                       }
{ Copyright (c) 2001-2004 Borland Software Corporation  }
{                                                       }
{*******************************************************}

unit xmldom;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, ActiveX,
{$ENDIF}
{$IFDEF LINUX}
  Types,
{$ENDIF}
  SysUtils, Variants, Classes, XMLConst;

const

{ Wrapper Versioning }

  DOMWrapperVersion              = 1.2;
  
{ NodeType Values }

  ELEMENT_NODE                   = 1;
  ATTRIBUTE_NODE                 = 2;
  TEXT_NODE                      = 3;
  CDATA_SECTION_NODE             = 4;
  ENTITY_REFERENCE_NODE          = 5;
  ENTITY_NODE                    = 6;
  PROCESSING_INSTRUCTION_NODE    = 7;
  COMMENT_NODE                   = 8;
  DOCUMENT_NODE                  = 9;
  DOCUMENT_TYPE_NODE             = 10;
  DOCUMENT_FRAGMENT_NODE         = 11;
  NOTATION_NODE                  = 12;

{ ExceptionCode Values }

  INDEX_SIZE_ERR                 = 1;
  DOMSTRING_SIZE_ERR             = 2;
  HIERARCHY_REQUEST_ERR          = 3;
  WRONG_DOCUMENT_ERR             = 4;
  INVALID_CHARACTER_ERR          = 5;
  NO_DATA_ALLOWED_ERR            = 6;
  NO_MODIFICATION_ALLOWED_ERR    = 7;
  NOT_FOUND_ERR                  = 8;
  NOT_SUPPORTED_ERR              = 9;
  INUSE_ATTRIBUTE_ERR            = 10;
  INVALID_STATE_ERR              = 11; { DOM Level 2 }
  SYNTAX_ERR                     = 12; { DOM Level 2 }
  INVALID_MODIFICATION_ERR       = 13; { DOM Level 2 }
  NAMESPACE_ERR                  = 14; { DOM Level 2 }
  INVALID_ACCESS_ERR             = 15; { DOM Level 2 }

const
  NSDelim = ':';
  SXML = 'xml';
  SVersion = 'version';
  SEncoding = 'encoding';
  SStandalone = 'standalone';
  SXMLNS = 'xmlns';
  SHttp = 'http:/';
  SXMLNamespaceURI = SHttp+'/www.w3.org/2000/xmlns/';
  SXMLPrefixNamespaceURI = SHttp+'/www.w3.org/XML/1998/namespace';

type

{ Misc. Types }

  DOMNodeType = Word;
  {$EXTERNALSYM DOMNodeType}
  DOMString = WideString;
  {$EXTERNALSYM DOMString}
  DOMTimeStamp = Int64;

{ Forward Declarations }

  IDOMImplementation = interface;
  IDOMNode = interface;
  IDOMNodeList = interface;
  IDOMNamedNodeMap = interface;
  IDOMCharacterData = interface;
  IDOMAttr = interface;
  IDOMElement = interface;
  IDOMText = interface;
  IDOMComment = interface;
  IDOMCDATASection = interface;
  IDOMDocumentType = interface;
  IDOMNotation = interface;
  IDOMEntity = interface;
  IDOMEntityReference = interface;
  IDOMProcessingInstruction = interface;
  IDOMDocumentFragment = interface;
  IDOMDocument = interface;
  { DOM Extensions }
  IDOMNodeEx = interface;
  IDOMPersist = interface;
  IDOMParseError = interface;

{ DOMException }

  DOMException = class(Exception)
  public
    code: Word;
  end;

{ EDOMParseError }

  EDOMParseError = class(Exception)
  private
    FParseError: IDOMParseError;
    function GetFilePos: Integer;
    function GetLine: Integer;
    function GetLinePos: Integer;
    function GetReason: DOMString;
    function GetSrcText: DOMString;
    function GetURL: DOMString;
    function GetErrorCode: Integer;
  protected
    property ParseError: IDOMParseError read FParseError;
  public
    constructor Create(const ParseError: IDOMParseError; const Msg: string);
    property ErrorCode: Integer read GetErrorCode;
    property URL: DOMString read GetURL;
    property Reason: DOMString read GetReason;
    property SrcText: DOMString read GetSrcText;
    property Line: Integer read GetLine;
    property LinePos: Integer read GetLinePos;
    property FilePos: Integer read GetFilePos;
  end;

{ IDOMImplementation }

  IDOMImplementation = interface
    ['{2BF4C0E0-096E-11D4-83DA-00C04F60B2DD}']
    function hasFeature(const feature, version: DOMString): WordBool;
    function createDocumentType(const qualifiedName, publicId,           { DOM Level 2 }
      systemId: DOMString): IDOMDocumentType; safecall;
    function createDocument(const namespaceURI, qualifiedName: DOMString;{ DOM Level 2 }
      doctype: IDOMDocumentType): IDOMDocument; safecall;
  end;

{ IDOMNode }

  IDOMNode = interface(IInterface)
    ['{2BF4C0E1-096E-11D4-83DA-00C04F60B2DD}']
    { Property Acessors }
    function get_nodeName: DOMString; safecall;
    function get_nodeValue: DOMString; safecall;
    procedure set_nodeValue(value: DOMString); safecall;
    function get_nodeType: DOMNodeType; safecall;
    function get_parentNode: IDOMNode; safecall;
    function get_childNodes: IDOMNodeList; safecall;
    function get_firstChild: IDOMNode; safecall;
    function get_lastChild: IDOMNode; safecall;
    function get_previousSibling: IDOMNode; safecall;
    function get_nextSibling: IDOMNode; safecall;
    function get_attributes: IDOMNamedNodeMap; safecall;
    function get_ownerDocument: IDOMDocument; safecall;
    function get_namespaceURI: DOMString; safecall;
    function get_prefix: DOMString; safecall;
    function get_localName: DOMString; safecall;
    { Methods }
    function insertBefore(const newChild, refChild: IDOMNode): IDOMNode; safecall;
    function replaceChild(const newChild, oldChild: IDOMNode): IDOMNode; safecall;
    function removeChild(const childNode: IDOMNode): IDOMNode; safecall;
    function appendChild(const newChild: IDOMNode): IDOMNode; safecall;
    function hasChildNodes: WordBool; safecall;
    function cloneNode(deep: WordBool): IDOMNode; safecall;
    procedure normalize; safecall;                                  { DOM Level 2 }
    function supports(const feature, version: DOMString): WordBool; { DOM Level 2 }
    { Properties }
    property nodeName: DOMString read get_nodeName;
    property nodeValue: DOMString read get_nodeValue write set_nodeValue;
    property nodeType: DOMNodeType read get_nodeType;
    property parentNode: IDOMNode read get_parentNode;
    property childNodes: IDOMNodeList read get_childNodes;
    property firstChild: IDOMNode read get_firstChild;
    property lastChild: IDOMNode read get_lastChild;
    property previousSibling: IDOMNode read get_previousSibling;
    property nextSibling: IDOMNode read get_nextSibling;
    property attributes: IDOMNamedNodeMap read get_attributes;
    property ownerDocument: IDOMDocument read get_ownerDocument;
    property namespaceURI: DOMString read get_namespaceURI;         { DOM Level 2 }
    property prefix: DOMString read get_prefix;                     { DOM Level 2 }
    property localName: DOMString read get_localName;               { DOM Level 2 }
  end;

{ IDOMNodeList }

  IDOMNodeList = interface
    ['{2BF4C0E2-096E-11D4-83DA-00C04F60B2DD}']
    { Property Acessors }
    function get_item(index: Integer): IDOMNode; safecall;
    function get_length: Integer; safecall;
    { Properties }
    property item[index: Integer]: IDOMNode read get_item; default;
    property length: Integer read get_length;
  end;

{ IDOMNamedNodeMap }

  IDOMNamedNodeMap = interface
    ['{2BF4C0E3-096E-11D4-83DA-00C04F60B2DD}']
    { Property Acessors }
    function get_item(index: Integer): IDOMNode; safecall;
    function get_length: Integer; safecall;
    { Methods }
    function getNamedItem(const name: DOMString): IDOMNode; safecall;
    function setNamedItem(const arg: IDOMNode): IDOMNode; safecall;
    function removeNamedItem(const name: DOMString): IDOMNode; safecall;
    function getNamedItemNS(const namespaceURI, localName: DOMString): IDOMNode; safecall;    { DOM Level 2 }
    function setNamedItemNS(const arg: IDOMNode): IDOMNode; safecall;                          { DOM Level 2 }
    function removeNamedItemNS(const namespaceURI, localName: DOMString): IDOMNode; safecall; { DOM Level 2 }
    { Properties }
    property item[index: Integer]: IDOMNode read get_item; default;
    property length: Integer read get_length;
  end;

{ IDOMCharacterData }

  IDOMCharacterData = interface(IDOMNode)
    ['{2BF4C0E4-096E-11D4-83DA-00C04F60B2DD}']
    { Property Acessors }
    function get_data: DOMString; safecall;
    procedure set_data(const data: DOMString); safecall;
    function get_length: Integer; safecall;
    { Methods }
    function substringData(offset, count: Integer): DOMString; safecall;
    procedure appendData(const data: DOMString); safecall;
    procedure insertData(offset: Integer; const data: DOMString); safecall;
    procedure deleteData(offset, count: Integer); safecall;
    procedure replaceData(offset, count: Integer; const data: DOMString); safecall;
    { Properties }
    property data: DOMString read get_data write set_data;
    property length: Integer read get_length;
  end;

{ IDOMAttr }

  IDOMAttr = interface(IDOMNode)
    ['{2BF4C0E5-096E-11D4-83DA-00C04F60B2DD}']
    { Property Acessors }
    function get_name: DOMString; safecall;
    function get_specified: WordBool; safecall;
    function get_value: DOMString; safecall;
    procedure set_value(const attributeValue: DOMString); safecall;
    function get_ownerElement: IDOMElement; safecall;
    { Properties }
    property name: DOMString read get_name;
    property specified: WordBool read get_specified;
    property value: DOMString read get_value write set_value;
    property ownerElement: IDOMElement read get_ownerElement;     { DOM Level 2 }
  end;

{ IDOMElement }

  IDOMElement = interface(IDOMNode)
    ['{2BF4C0E6-096E-11D4-83DA-00C04F60B2DD}']
    { Property Acessors }
    function get_tagName: DOMString; safecall;
    { Methods }
    function getAttribute(const name: DOMString): DOMString; safecall;
    procedure setAttribute(const name, value: DOMString); safecall;
    procedure removeAttribute(const name: DOMString); safecall;
    function getAttributeNode(const name: DOMString): IDOMAttr; safecall;
    function setAttributeNode(const newAttr: IDOMAttr): IDOMAttr; safecall;
    function removeAttributeNode(const oldAttr: IDOMAttr): IDOMAttr; safecall;
    function getElementsByTagName(const name: DOMString): IDOMNodeList; safecall;
    function getAttributeNS(const namespaceURI, localName: DOMString): DOMString; safecall;    { DOM Level 2 }
    procedure setAttributeNS(const namespaceURI, qulifiedName, value: DOMString); safecall;    { DOM Level 2 }
    procedure removeAttributeNS(const namespaceURI, localName: DOMString); safecall;           { DOM Level 2 }
    function getAttributeNodeNS(const namespaceURI, localName: DOMString): IDOMAttr; safecall; { DOM Level 2 }
    function setAttributeNodeNS(const newAttr: IDOMAttr): IDOMAttr; safecall;                  { DOM Level 2 }
    function getElementsByTagNameNS(const namespaceURI,
      localName: DOMString): IDOMNodeList; safecall;                                           { DOM Level 2 }
    function hasAttribute(const name: DOMString): WordBool; safecall;                          { DOM Level 2 }
    function hasAttributeNS(const namespaceURI, localName: DOMString): WordBool; safecall;     { DOM Level 2 }
    { Properties }
    property tagName: DOMString read get_tagName;
  end;

{ IDOMText }

  IDOMText = interface(IDOMCharacterData)
    ['{2BF4C0E7-096E-11D4-83DA-00C04F60B2DD}']
    function splitText(offset: Integer): IDOMText; safecall;
  end;

{ IDOMComment }

  IDOMComment = interface(IDOMCharacterData)
    ['{2BF4C0E8-096E-11D4-83DA-00C04F60B2DD}']
  end;

{ IDOMCDATASection }

  IDOMCDATASection = interface(IDOMText)
    ['{2BF4C0E9-096E-11D4-83DA-00C04F60B2DD}']
  end;

{ IIDOMDocumentType }

  IDOMDocumentType = interface(IDOMNode)
    ['{2BF4C0EA-096E-11D4-83DA-00C04F60B2DD}']
    { Property Acessors }
    function get_name: DOMString; safecall;
    function get_entities: IDOMNamedNodeMap; safecall;
    function get_notations: IDOMNamedNodeMap; safecall;
    function get_publicId: DOMString; safecall;
    function get_systemId: DOMString; safecall;
    function get_internalSubset: DOMString; safecall;
    { Properties }
    property name: DOMString read get_name;
    property entities: IDOMNamedNodeMap read get_entities;
    property notations: IDOMNamedNodeMap read get_notations;
    property publicId: DOMString read get_publicId;             { DOM Level 2 }
    property systemId: DOMString read get_systemId;             { DOM Level 2 }
    property internalSubset: DOMString read get_internalSubset; { DOM Level 2 }
  end;

{ IDOMNotation }

  IDOMNotation = interface(IDOMNode)
    ['{2BF4C0EB-096E-11D4-83DA-00C04F60B2DD}']
    { Property Acessors }
    function get_publicId: DOMString; safecall;
    function get_systemId: DOMString; safecall;
    { Properties }
    property publicId: DOMString read get_publicId;
    property systemId: DOMString read get_systemId;
  end;

{ DOmEntity }

  IDOMEntity = interface(IDOMNode)
    ['{2BF4C0EC-096E-11D4-83DA-00C04F60B2DD}']
    { Property Acessors }
    function get_publicId: DOMString; safecall;
    function get_systemId: DOMString; safecall;
    function get_notationName: DOMString; safecall;
    { Properties }
    property publicId: DOMString read get_publicId;
    property systemId: DOMString read get_systemId;
    property notationName: DOMString read get_notationName;
  end;

{ IDOMEntityReference }

  IDOMEntityReference = interface(IDOMNode)
    ['{2BF4C0ED-096E-11D4-83DA-00C04F60B2DD}']
  end;

{ IDOMProcessingInstruction }

  IDOMProcessingInstruction = interface(IDOMNode)
    ['{2BF4C0EE-096E-11D4-83DA-00C04F60B2DD}']
    { Property Acessors }
    function get_target: DOMString; safecall;
    function get_data: DOMString; safecall;
    procedure set_data(const value: DOMString); safecall;
    { Properties }
    property target: DOMString read get_target;
    property data: DOMString read get_data write set_data;
  end;

{ IDOMDocumentFragment }

  IDOMDocumentFragment = interface(IDOMNode)
    ['{2BF4C0EF-096E-11D4-83DA-00C04F60B2DD}']
  end;

{ IDOMDocument }

  IDOMDocument = interface(IDOMNode)
    ['{2BF4C0F0-096E-11D4-83DA-00C04F60B2DD}']
    { Property Acessors }
    function get_doctype: IDOMDocumentType; safecall;
    function get_domImplementation: IDOMImplementation; safecall;
    function get_documentElement: IDOMElement; safecall;
    procedure set_documentElement(const Element: IDOMElement); safecall;
    { Methods }
    function createElement(const tagName: DOMString): IDOMElement; safecall;
    function createDocumentFragment: IDOMDocumentFragment; safecall;
    function createTextNode(const data: DOMString): IDOMText; safecall;
    function createComment(const data: DOMString): IDOMComment; safecall;
    function createCDATASection(const data: DOMString): IDOMCDATASection; safecall;
    function createProcessingInstruction(const target,
      data: DOMString): IDOMProcessingInstruction; safecall;
    function createAttribute(const name: DOMString): IDOMAttr; safecall;
    function createEntityReference(const name: DOMString): IDOMEntityReference; safecall;
    function getElementsByTagName(const tagName: DOMString): IDOMNodeList; safecall;
    function importNode(importedNode: IDOMNode; deep: WordBool): IDOMNode; safecall; { DOM Level 2 }
    function createElementNS(const namespaceURI,                                     { DOM Level 2 }
      qualifiedName: DOMString): IDOMElement; safecall;
    function createAttributeNS(const namespaceURI,                                   { DOM Level 2 }
      qualifiedName: DOMString): IDOMAttr; safecall;
    function getElementsByTagNameNS(const namespaceURI,                              { DOM Level 2 }
      localName: DOMString): IDOMNodeList; safecall;
    function getElementById(const elementId: DOMString): IDOMElement; safecall;      { DOM Level 2 }
    { Properties }
    property doctype: IDOMDocumentType read get_doctype;
    property domImplementation: IDOMImplementation read get_domImplementation;
    property documentElement: IDOMElement read get_documentElement write set_documentElement;
  end;

{*********************************************************}
{ DOM Extension Interfaces                                }
{ The following interfaces are NOT a part of the DOM spec }
{*********************************************************}

{ IDOMNodeEx }

  IDOMNodeEx = interface(IDOMNode)
    ['{B06BFFDD-337B-48DA-980B-6F7AA8ADE85C}']
    { Property Acessors }
    function get_text: DOMString; safecall;
    function get_xml: DOMString; safecall;
    procedure set_text(const Value: DOMString); safecall;
    { Methods }
    procedure transformNode(const stylesheet: IDOMNode; var output: WideString); safecall; overload;
    procedure transformNode(const stylesheet: IDOMNode; const output: IDOMDocument); safecall; overload;
    { Properties }
    property text: DOMString read get_text write set_text;
    property xml: DOMString read get_xml;
  end;

{ IDOMNodeSelect }

  IDOMNodeSelect = interface(IInterface)
    ['{2A3602E0-2B39-11D4-83DA-00C04F60B2DD}']
    function selectNode(const nodePath: WideString): IDOMNode; safecall;
    function selectNodes(const nodePath: WideString): IDOMNodeList; safecall;
  end;

{ IDOMXSLProcessor }

  IDOMXSLProcessor = interface(IInterface)
    ['{2BF4C0F4-096E-11D4-83DA-00C04F60B2DD}']
    { Property Acessors }
    function Get_input: OleVariant; safecall;
    function Get_output: OleVariant; safecall;
    function Get_stylesheet: IDOMNode; safecall;
    procedure Set_input(const value: OleVariant); safecall;
    procedure Set_output(const value: OleVariant); safecall;
    { Methods }
    procedure setParameter(const Name: DOMString; Value: OleVariant;
      const namespaceURI: DOMString); safecall;
    procedure reset; safecall;
    function transform: WordBool; safecall;
    { Properties }
    property input: OleVariant read Get_input write Set_input;
    property output: OleVariant read Get_output write Set_output;
    property stylesheet: IDOMNode read Get_stylesheet;
  end;

{ IDOMPersist }

  TAsyncEventHandler = procedure(Sender: TObject; AsyncLoadState: Integer) of Object;

  IDOMPersist = interface
    ['{2BF4C0F1-096E-11D4-83DA-00C04F60B2DD}']
    { Property Acessors }
    function get_xml: DOMString; safecall;
    { Methods }
    function asyncLoadState: Integer; safecall;
    function load(source: OleVariant): WordBool; safecall;
    function loadFromStream(const stream: TStream): WordBool; overload; safecall;
    function loadxml(const Value: DOMString): WordBool; safecall;
    procedure save(destination: OleVariant); safecall;
    procedure saveToStream(const stream: TStream); overload; safecall;
    procedure set_OnAsyncLoad(const Sender: TObject;
      EventHandler: TAsyncEventHandler); safecall;
    function loadFromStream(const stream: IStream): WordBool; overload; safecall;
    procedure saveToStream(const stream: IStream); overload; safecall;
    { Properties }
    property xml: DOMString read get_xml;
  end;

{ IDOMParseError }

  IDOMParseError = interface
    ['{2BF4C0F2-096E-11D4-83DA-00C04F60B2DD}']
    { Property Acessors }
    function get_errorCode: Integer; safecall;
    function get_url: DOMString; safecall;
    function get_reason: DOMString; safecall;
    function get_srcText: DOMString; safecall;
    function get_line: Integer; safecall;
    function get_linepos: Integer; safecall;
    function get_filepos: Integer; safecall;
    { Properties }
    property errorCode: Integer read get_errorCode;
    property url: DOMString read get_url;
    property reason: DOMString read get_reason;
    property srcText: DOMString read get_srcText;
    property line: Integer read get_line;
    property linePos: Integer read get_linepos;
    property filePos: Integer read get_filepos;
  end;

{ IDOMParseOptions }

  IDOMParseOptions = interface
    ['{2BF4C0F3-096E-11D4-83DA-00C04F60B2DD}']
    { Property Acessors }
    function get_async: Boolean; safecall;
    function get_preserveWhiteSpace: Boolean; safecall;
    function get_resolveExternals: Boolean; safecall;
    function get_validate: Boolean; safecall;
    procedure set_async(Value: Boolean); safecall;
    procedure set_preserveWhiteSpace(Value: Boolean); safecall;
    procedure set_resolveExternals(Value: Boolean); safecall;
    procedure set_validate(Value: Boolean); safecall;
    { Properties }
    property async: Boolean read get_async write set_async;
    property preserveWhiteSpace: Boolean read get_preserveWhiteSpace write set_preserveWhiteSpace;
    property resolveExternals: Boolean read get_resolveExternals write set_resolveExternals;
    property validate: Boolean read get_validate write set_validate;
  end;

{ IDOMPrologNode }

  IDOMXMLProlog = interface
    ['{7C192633-C267-483C-B0D5-89289A14D522}']
    { Property Acessors }
    function get_Encoding: DOMString; safecall;
    function get_Standalone: DOMString; safecall;
    function get_Version: DOMString; safecall;
    procedure set_Encoding(const Value: DOMString); safecall;
    procedure set_Standalone(const Value: DOMString); safecall;
    procedure set_Version(const Value: DOMString); safecall;
    { Properties }
    property Encoding: DOMString read get_Encoding write set_Encoding;
    property Standalone: DOMString read get_Standalone write set_Standalone;
    property Version: DOMString read get_Version write set_Version;
  end;

{$M+}

{ TDOMVendor }

  TDOMVendor = class
  public
    function Description: string; virtual; abstract;
    function DOMImplementation: IDOMImplementation; virtual; abstract;
  end;

{$M-}

{ TDOMVendorList }

  TDOMVendorArray = array of TDOMVendor;

  TDOMVendorList = class
  private
    FVendors: TDOMVendorArray;
  protected
    function GetVendors(Index: Integer): TDOMVendor;
  public
    procedure Add(const Vendor: TDOMVendor);
    function Count: Integer;
    function Find(const VendorDesc: string): TDOMVendor;
    procedure Remove(const Vendor: TDOMVendor);
    property Vendors[Index: Integer]: TDOMVendor read GetVendors; default;
  end;

var
  DefaultDOMVendor: string;
  DOMVendors: TDOMVendorList;

{ Global Helper Functions }

function IsPrefixed(const AName: DOMString): Boolean;
function ExtractLocalName(const AName: DOMString): DOMString;
function ExtractPrefix(const AName: DOMString): DOMString;
function MakeNodeName(const Prefix, LocalName: DOMString): DOMString;
function SameNamespace(const Node: IDOMNode; const namespaceURI: WideString): Boolean; overload;
function SameNamespace(const URI1, URI2: WideString): Boolean; overload;
function NodeMatches(const Node: IDOMNode; const TagName, NamespaceURI: DOMString): Boolean; overload;

function GetDOMNodeEx(const Node: IDOMNode): IDOMNodeEx;

{ DOM Vendor Registration & Selection }

procedure RegisterDOMVendor(const Vendor: TDOMVendor);
procedure UnRegisterDOMVendor(const Vendor: TDOMVendor);
function GetDOMVendor(VendorDesc: string): TDOMVendor;
function GetDOM(const VendorDesc: string = ''): IDOMImplementation;
procedure DOMVendorNotSupported(const PropOrMethod, VendorName: string);

implementation

{$IFDEF MSWINDOWS}
uses msxmldom;
{$ENDIF}
{$IFDEF LINUX}
uses xercesxmldom;
{$ENDIF}

{ Global Helper Functions }

function IsPrefixed(const AName: DOMString): Boolean;
begin
  Result := Pos(NSDelim, AName) > 0;
end;

function ExtractLocalName(const AName: DOMString): DOMString;
var
  SepPos: Integer;
begin
  SepPos := Pos(NSDelim, AName);
  if SepPos > 0 then
    Result := Copy(AName, SepPos+1, High(Integer))
  else
    Result := AName;
end;

function ExtractPrefix(const AName: DOMString): DOMString;
var
  SepPos: Integer;
begin
  SepPos := Pos(NSDelim, AName);
  if SepPos > 0 then
    Result := Copy(AName, 1, SepPos-1)
  else
    Result := '';
end;

function MakeNodeName(const Prefix, LocalName: DOMString): DOMString;
begin
  if Prefix <> '' then
    Result := Prefix + NSDelim + LocalName
  else
    Result := LocalName;
end;

function SameNamespace(const Node: IDOMNode; const namespaceURI: WideString): Boolean;
begin
  Result := SameText(Node.namespaceURI, namespaceURI);
end;

function SameNamespace(const URI1, URI2: WideString): Boolean; overload;
begin
  Result := SameText(URI1, URI2);
end;

function NodeMatches(const Node: IDOMNode; const TagName, NamespaceURI: DOMString): Boolean;
begin
  Result := ((NamespaceURI = '') or (SameNamespace(Node, NamespaceURI))) and
    ((Node.NodeName = TagName) or (Node.LocalName = TagName));
end;

function GetDOMNodeEx(const Node: IDOMNode): IDOMNodeEx;
begin
  if not Supports(Node, IDOMNodeEx, Result) then
    raise DOMException.Create(SNoDOMNodeEx);
end;

{ Registration }

function DOMVendorList: TDOMVendorList;
begin
  if not Assigned(DOMVendors) then
    DOMVendors := TDOMVendorList.Create;
  Result := DOMVendors;
end;

procedure RegisterDOMVendor(const Vendor: TDOMVendor);
begin
  DOMVendorList.Add(Vendor);
end;

procedure UnRegisterDOMVendor(const Vendor: TDOMVendor);
begin
  if Assigned(DOMVendors) then
    DOMVendorList.Remove(Vendor);
end;

function GetDOMVendor(VendorDesc: string): TDOMVendor;
begin
  if VendorDesc = '' then
    VendorDesc := DefaultDOMVendor;
  if (VendorDesc = '') and (DOMVendorList.Count > 0) then
    Result := DOMVendorList[0]
  else
    Result := DOMVendorList.Find(VendorDesc);
  if not Assigned(Result) then
    raise Exception.CreateFmt(SNoMatchingDOMVendor, [VendorDesc]);
end;

function GetDOM(const VendorDesc: string = ''): IDOMImplementation;
begin
  Result := GetDOMVendor(VendorDesc).DOMImplementation;
end;

procedure DOMVendorNotSupported(const PropOrMethod, VendorName: string);
begin
  raise DOMException.CreateFmt(SDOMNotSupported, [PropOrMethod, VendorName]);
end;

{ TDOMVendorList }

function TDOMVendorList.Count: Integer;
begin
  Result := Length(FVendors);
end;

function TDOMVendorList.Find(const VendorDesc: string): TDOMVendor;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if VendorDesc = FVendors[I].Description then
    begin
      Result := FVendors[I];
      Break;
    end;
end;

procedure TDOMVendorList.Add(const Vendor: TDOMVendor);
var
  Index: Integer;
begin
  if Find(Vendor.Description) <> nil then
    raise Exception.CreateFmt(SDuplicateRegistration, [Vendor.Description]);
  Index := Length(FVendors);
  SetLength(FVendors, Index + 1);
  FVendors[Index] := Vendor;
end;

procedure TDOMVendorList.Remove(const Vendor: TDOMVendor);
var
  I, Index: Integer;
begin
  Index := Count;
  for I := 0 to Count - 1 do
    if Vendor = FVendors[I] then
    begin
      Index := I;
      Break;
    end;
  if Index >= Count then Exit;
  for I := Index to Count - 2 do
    FVendors[I] := FVendors[I+1];
  SetLength(FVendors, Count-1);
end;

function TDOMVendorList.GetVendors(Index: Integer): TDOMVendor;
begin
  Result := FVendors[Index];
end;

{ EDOMParseError }

constructor EDOMParseError.Create(const ParseError: IDOMParseError;
  const Msg: string);
begin
  FParseError := ParseError;
  inherited Create(Msg);
end;

function EDOMParseError.GetErrorCode: Integer;
begin
  Result := ParseError.ErrorCode;
end;

function EDOMParseError.GetFilePos: Integer;
begin
  Result := ParseError.filePos;
end;

function EDOMParseError.GetLine: Integer;
begin
  Result := ParseError.line;
end;

function EDOMParseError.GetLinePos: Integer;
begin
  Result := ParseError.linePos;
end;

function EDOMParseError.GetReason: DOMString;
begin
  Result := ParseError.reason;
end;

function EDOMParseError.GetSrcText: DOMString;
begin
  Result := ParseError.srcText;
end;

function EDOMParseError.GetURL: DOMString;
begin
  Result := ParseError.url;
end;

initialization
finalization
  FreeAndNil(DOMVendors);
end.
