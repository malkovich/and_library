
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{       MSXML DOM Implementation Wrapper                }
{                                                       }
{ Copyright (c) 2001-2002 Borland Software Corporation  }
{                                                       }
{*******************************************************}

unit msxmldom;

interface

uses ActiveX, Windows, Variants, ComObj, msxml, xmldom, Classes;

{$IF DOMWrapperVersion >= 1.1}
{ This define is used for the changes to the calling conventions of the IDOMxxx
  interfaces after the release of Delphi 6 (safecall was added in many places) }
{$DEFINE WRAP1.1}
{$IFEND}

const

  SMSXML = 'MSXML';  { Do not localize }

type

{ IXMLDOMNodeRef }

  IXMLDOMNodeRef = interface
    ['{5EF5DAA1-2729-11D4-83DA-00C04F60B2DD}']
    function GetXMLDOMNode: IXMLDOMNode;
  end;

{ TMSDOMInterface }

  TMSDOMInterface = class(TInterfacedObject)
  public
    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HRESULT; override;
  end;

{ TMSDOMImplementation }

  TMSDOMImplementation = class(TMSDOMInterface, IDOMImplementation)
  private
    FMSDOMImpl: IXMLDOMImplementation;
  protected
     { IDOMImplementation }
    function hasFeature(const feature, version: DOMString): WordBool;
    function createDocumentType(const qualifiedName, publicId,
      systemId: DOMString): IDOMDocumentType; safecall;
    function createDocument(const namespaceURI, qualifiedName: DOMString;
      doctype: IDOMDocumentType): IDOMDocument; safecall;
  public
    constructor Create(DOMImpl: IXMLDOMImplementation);
    property MSDOMImpl: IXMLDOMImplementation read FMSDOMImpl;
  end;

{ TMSDOMNode }

  TMSDOMNode = class;
  TMSDOMNodeClass = class of TMSDOMNode;

  TMSDOMNode = class(TMSDOMInterface, IXMLDOMNodeRef, IDOMNode, IDOMNodeEx,
    IDOMNodeSelect)
  private
    FMSNode: IXMLDOMNode;
    FChildNodes: IDOMNodeList;
    FAttributes: IDOMNamedNodeMap;
    FOwnerDocument: IDOMDocument;
  protected
    { IXMLDOMNodeRef }
    function GetXMLDOMNode: IXMLDOMNode;
    { IDOMNode }
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
    function insertBefore(const newChild, refChild: IDOMNode): IDOMNode; safecall;
    function replaceChild(const newChild, oldChild: IDOMNode): IDOMNode; safecall;
    function removeChild(const childNode: IDOMNode): IDOMNode; safecall;
    function appendChild(const newChild: IDOMNode): IDOMNode; safecall;
    function hasChildNodes: WordBool; safecall;
    function cloneNode(deep: WordBool): IDOMNode; safecall;
    procedure normalize; safecall;
    function supports(const feature, version: DOMString): WordBool;
    { IDOMNodeEx }
    function get_text: DOMString; safecall;
    function get_xml: DOMString; safecall;
    procedure transformNode(const stylesheet: IDOMNode; var output: WideString); overload; safecall;
    procedure transformNode(const stylesheet: IDOMNode; const output: IDOMDocument); overload; safecall;
    { IDOMNodeSelect }
    function selectNode(const nodePath: WideString): IDOMNode; safecall;
    function selectNodes(const nodePath: WideString): IDOMNodeList; safecall;
    procedure set_text(const Value: DOMString); safecall;
  public
    constructor Create(ANode: IXMLDOMNode);
    property MSNode: IXMLDOMNode read FMSNode;
  end;

{ TMSDOMNodeList }

  TMSDOMNodeList = class(TMSDOMInterface, IDOMNodeList)
  private
     FMSNodeList: IXMLDOMNodeList;
  protected
    { IDOMNodeList }
    function get_item(index: Integer): IDOMNode; safecall;
    function get_length: Integer; safecall;
  public
    constructor Create(ANodeList: IXMLDOMNodeList);
    property MSNodeList: IXMLDOMNodeList read FMSNodeList;
  end;

{ TMSDOMNamedNodeMap }

  TMSDOMNamedNodeMap = class(TMSDOMInterface, IDOMNamedNodeMap)
  private
    FMSNamedNodeMap: IXMLDOMNamedNodeMap;
  protected
    { IDOMNamedNodeMap }
    function get_item(index: Integer): IDOMNode; safecall;
    function get_length: Integer; safecall;
    function getNamedItem(const name: DOMString): IDOMNode; safecall;
    function setNamedItem(const newItem: IDOMNode): IDOMNode; safecall;
    function removeNamedItem(const name: DOMString): IDOMNode; safecall;
    function getNamedItemNS(const namespaceURI, localName: DOMString): IDOMNode; safecall;
    function setNamedItemNS(const arg: IDOMNode): IDOMNode; safecall;
    function removeNamedItemNS(const namespaceURI, localName: DOMString): IDOMNode; safecall;
  public
    constructor Create(ANamedNodeMap: IXMLDOMNamedNodeMap);
    property MSNamedNodeMap: IXMLDOMNamedNodeMap read FMSNamedNodeMap;
  end;

{ TMSDOMCharacterData }

  TMSDOMCharacterData = class(TMSDOMNode, IDOMCharacterData)
  private
    function GetMSCharacterData: IXMLDOMCharacterData;
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
    property MSCharacterData: IXMLDOMCharacterData read GetMSCharacterData;
  end;

{ TMSDOMAttr }

  TMSDOMAttr = class(TMSDOMNode, IDOMAttr)
  private
    function GetMSAttribute: IXMLDOMAttribute;
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
    property MSAttribute: IXMLDOMAttribute read GetMSAttribute;
  end;

{ TMSDOMElement }

  TMSDOMElement = class(TMSDOMNode, IDOMElement)
  private
    function GetMSElement: IXMLDOMElement;
  protected
    { IDOMElement }
    function get_tagName: DOMString; safecall;
    function getAttribute(const name: DOMString): DOMString; safecall;
    procedure setAttribute(const name, value: DOMString); safecall;
    procedure removeAttribute(const name: DOMString); safecall;
    function getAttributeNode(const name: DOMString): IDOMAttr; safecall;
    function setAttributeNode(const newAttr: IDOMAttr): IDOMAttr; safecall;
    function removeAttributeNode(const oldAttr: IDOMAttr): IDOMAttr; safecall;
    function getElementsByTagName(const name: DOMString): IDOMNodeList; safecall;
    function getAttributeNS(const namespaceURI, localName: DOMString): DOMString; safecall;
    procedure setAttributeNS(const namespaceURI, qualifiedName, value: DOMString); safecall;
    procedure removeAttributeNS(const namespaceURI, localName: DOMString); safecall;
    function getAttributeNodeNS(const namespaceURI, localName: DOMString): IDOMAttr; safecall;
    function setAttributeNodeNS(const newAttr: IDOMAttr): IDOMAttr; safecall;
    function getElementsByTagNameNS(const namespaceURI,
      localName: DOMString): IDOMNodeList; safecall;
    function hasAttribute(const name: DOMString): WordBool; safecall;
    function hasAttributeNS(const namespaceURI, localName: DOMString): WordBool; safecall;
    procedure normalize; safecall;
  public
    property MSElement: IXMLDOMElement read GetMSElement;
  end;

{ TMSDOMText }

  TMSDOMText = class(TMSDOMCharacterData, IDOMText)
  protected
    function splitText(offset: Integer): IDOMText; safecall;
  end;

{ TMSDOMComment }

  TMSDOMComment = class(TMSDOMCharacterData, IDOMComment)
  end;

{ TMSDOMCDATASection }

  TMSDOMCDATASection = class(TMSDOMText, IDOMCDATASection)
  end;

{ TMSDOMDocumentType }

  TMSDOMDocumentType = class(TMSDOMNode, IDOMDocumentType)
  private
    FEntities: IDOMNamedNodeMap;
    FNotations: IDOMNamedNodeMap;
    function GetMSDocumentType: IXMLDOMDocumentType;
  protected
    { IDOMDocumentType }
    function get_name: DOMString; safecall;
    function get_entities: IDOMNamedNodeMap; safecall;
    function get_notations: IDOMNamedNodeMap; safecall;
    function get_publicId: DOMString; safecall;
    function get_systemId: DOMString; safecall;
    function get_internalSubset: DOMString; safecall;
  public
    property MSDocumentType: IXMLDOMDocumentType read GetMSDocumentType;
  end;

{ TMSDOMNotation }

  TMSDOMNotation = class(TMSDOMNode, IDOMNotation)
  private
    function GetMSNotation: IXMLDOMNotation;
  protected
    { IDOMNotation }
    function get_publicId: DOMString; safecall;
    function get_systemId: DOMString; safecall;
  public
    property MSNotation: IXMLDOMNotation read GetMSNotation;
  end;

{ TMSDOMEntity }

  TMSDOMEntity = class(TMSDOMNode, IDOMEntity)
  private
    function GetMSEntity: IXMLDOMEntity;
  protected
    { IDOMEntity }
    function get_publicId: DOMString; safecall;
    function get_systemId: DOMString; safecall;
    function get_notationName: DOMString; safecall;
  public
    property MSEntity: IXMLDOMEntity read GetMSEntity;
  end;

{ TMSDOMEntityReference }

  TMSDOMEntityReference = class(TMSDOMNode, IDOMEntityReference)
  end;

{ TMSDOMProcessingInstruction }

  TMSDOMProcessingInstruction = class(TMSDOMNode, IDOMProcessingInstruction)
  private
    function GetMSProcessingInstruction: IXMLDOMProcessingInstruction;
  protected
    { IDOMProcessingInstruction }
    function get_target: DOMString; safecall;
    function get_data: DOMString; safecall;
    procedure set_data(const value: DOMString); safecall;
  public
    property MSProcessingInstruction: IXMLDOMProcessingInstruction read GetMSProcessingInstruction;
  end;

{ TMSDOMDocumentFragment }

  TMSDOMDocumentFragment = class(TMSDOMNode, IDOMDocumentFragment)
  end;

{ TMSDOMEventHandler }

  TMSDOMDocument = class;

  TMSDOMEventHandler = class(TInterfacedObject, IDispatch)
  private
    FSender: TObject;
    FDocument: TMSDOMDocument;
    FEventHandler: TAsyncEventHandler;
  protected
    { IDispatch }
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; virtual; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; virtual; stdcall;
    function GetTypeInfoCount(out Count: Integer): HResult; virtual; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; virtual; stdcall;
  public
    constructor Create(const Document: TMSDOMDocument;
      AsyncEventHandler: TAsyncEventHandler; Sender: TObject);
  end;

{ TMSDOMDocument }

  TMSDOMDocument = class(TMSDOMNode, IDOMDocument, IDOMParseOptions, IDOMPersist,
    IDOMParseError)
  private
    function GetMSDocument: IXMLDOMDocument;
  protected
    { IDOMDocument }
    function get_doctype: IDOMDocumentType; safecall;
    function get_domImplementation: IDOMImplementation; safecall;
    function get_documentElement: IDOMElement; safecall;
    procedure set_documentElement(const IDOMElement: IDOMElement); safecall;
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
    function importNode(importedNode: IDOMNode; deep: WordBool): IDOMNode; safecall;
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
    function loadFromStream(const stream: TStream): WordBool; overload;safecall;
    function loadxml(const Value: DOMString): WordBool; safecall;
    procedure save(destination: OleVariant); safecall;
    procedure saveToStream(const stream: TStream); overload;safecall; 
    procedure set_OnAsyncLoad(const Sender: TObject;
      EventHandler: TAsyncEventHandler); safecall;
    function loadFromStream(const stream: IStream): WordBool; overload; safecall;
    procedure saveToStream(const stream: IStream); overload; safecall;
    { IDOMParseError }
    function get_errorCode: Integer; safecall;
    function get_url: WideString; safecall;
    function get_reason: WideString; safecall;
    function get_srcText: WideString; safecall;
    function get_line: Integer; safecall;
    function get_linepos: Integer; safecall;
    function get_filepos: Integer; safecall;
  public
    property MSDocument: IXMLDOMDocument read GetMSDocument;
  end;

{ TMSDOMImplementationFactory }

  TMSDOMImplementationFactory = class(TDOMVendor)
  public
    function DOMImplementation: IDOMImplementation; override;
    function Description: String; override;
  end;

function CreateDOMDocument: IXMLDOMDocument;
function LoadFromXMLString(const Doc: IXMLDOMDocument; const XML: string): Boolean;

type
 TMSXMLDOMDocumentCreateFunc = function: IXMLDOMDocument;
var
  MSXML_DOM: TMSDOMImplementationFactory;
  MSXMLDOMDocumentCreate: TMSXMLDOMDocumentCreateFunc = CreateDOMDocument;

implementation

uses XMLConst;

{ Utility Functions }

function MakeNode(Node: IXMLDOMNode): IDOMNode;
const
  NodeClasses: array[ELEMENT_NODE..NOTATION_NODE] of TMSDOMNodeClass =
    (TMSDOMElement, TMSDOMAttr, TMSDOMText, TMSDOMCDataSection,
     TMSDOMEntityReference, TMSDOMEntity, TMSDOMProcessingInstruction,
     TMSDOMComment, TMSDOMDocument, TMSDOMDocumentType, TMSDOMDocumentFragment,
     TMSDOMNotation);
begin
  if Node <> nil then
    Result := NodeClasses[Node.nodeType].Create(Node) else
    Result := nil;
end;

function MakeNodeList(const NodeList: IXMLDOMNodeList): IDOMNodeList;
begin
  Result := TMSDOMNodeList.Create(NodeList);
end;

function MakeNamedNodeMap(const NamedNodeMap: IXMLDOMNamedNodeMap): IDOMNamedNodeMap;
begin
  Result := TMSDOMNamedNodeMap.Create(NamedNodeMap);
end;

function GetMSNode(const Node: IDOMNode): IXMLDOMNode;
begin
  if not Assigned(Node) then
    raise DOMException.Create(SNodeExpected);
  Result := (Node as IXMLDOMNodeRef).GetXMLDOMNode;
end;

function LoadFromXMLString(const Doc: IXMLDOMDocument; const XML: string): Boolean;
begin
  Result := Doc.load(TStreamAdapter.Create(TStringStream.Create(XML), soOwned) as IStream);
end;

{ TMSDOMInterface }

function TMSDOMInterface.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HRESULT;
var
  HelpFile: string;
begin
  if ExceptObject is EOleException then
    HelpFile := (ExceptObject as EOleException).HelpFile;
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IDOMNode, '', Helpfile);
end;

{ TMSDOMImplementation }

const
  { GUID's from MSXML2_TLB.pas }
  CLASS_DOMDocument26: TGUID = '{F5078F1B-C551-11D3-89B9-0000F81FE221}';
  CLASS_DOMDocument30: TGUID = '{F5078F32-C551-11D3-89B9-0000F81FE221}';
  CLASS_DOMDocument40: TGUID = '{88D969C0-F192-11D4-A65F-0040963251E5}';

function TryObjectCreate(const GuidList: array of TGuid): IUnknown;
var
  I: Integer;
  Status: HResult;
begin
  Status := S_OK;
  for I := Low(GuidList) to High(GuidList) do
  begin
    Status := CoCreateInstance(GuidList[I], nil, CLSCTX_INPROC_SERVER or
      CLSCTX_LOCAL_SERVER, IDispatch, Result);
    if Status = S_OK then Exit;
  end;
  OleCheck(Status);
end;

function CreateDOMDocument: IXMLDOMDocument;
begin
  Result := TryObjectCreate([CLASS_DOMDocument40, CLASS_DOMDocument30,
    CLASS_DOMDocument26, msxml.CLASS_DOMDocument]) as IXMLDOMDocument;
  if not Assigned(Result) then
    raise DOMException.Create(SMSDOMNotInstalled);
end;

constructor TMSDOMImplementation.Create(DOMImpl: IXMLDOMImplementation);
begin
  inherited Create;
  FMSDOMImpl := DOMImpl;
end;

function TMSDOMImplementation.createDocument(const namespaceURI,
  qualifiedName: DOMString; doctype: IDOMDocumentType): IDOMDocument;
begin
  Result := TMSDOMDocument.Create(MSXMLDOMDocumentCreate);
end;

function TMSDOMImplementation.createDocumentType(const qualifiedName,
  publicId, systemId: DOMString): IDOMDocumentType;
begin
  DOMVendorNotSupported('createDocumentType', SMSXML); { Do not localize }
end;

function TMSDOMImplementation.hasFeature(const feature,
  version: DOMString): WordBool;
begin
  if Assigned(MSDOMImpl) then
    Result := MSDOMImpl.hasFeature(feature, version) else
    Result := MSXMLDOMDocumentCreate.implementation_.HasFeature(feature, version);
end;

{ TMSDOMNode }

constructor TMSDOMNode.Create(ANode: IXMLDOMNode);
begin
  Assert(Assigned(ANode));
  FMSNode := ANode;
  inherited Create;
end;

function TMSDOMNode.appendChild(const newChild: IDOMNode): IDOMNode;
var
  NewMSChild,
  ReturnedChild: IXMLDOMNode;
begin
  NewMSChild := GetMSNode(newChild);
  ReturnedChild := MSNode.appendChild(NewMSChild);
  if ReturnedChild = NewMSChild then
    Result := newChild else
    Result := MakeNode(ReturnedChild);
end;

function TMSDOMNode.cloneNode(deep: WordBool): IDOMNode;
begin
  Result := MakeNode(MSNode.cloneNode(deep));
end;

function TMSDOMNode.get_attributes: IDOMNamedNodeMap;
begin
  if not Assigned(FAttributes) and Assigned(MSNode.attributes) then
    FAttributes := MakeNamedNodeMap(MSNode.attributes);
  Result := FAttributes;
end;

function TMSDOMNode.get_childNodes: IDOMNodeList;
begin
  if not Assigned(FChildNodes) then
    FChildNodes := MakeNodeList(MSNode.childNodes);
  Result := FChildNodes;
end;

function TMSDOMNode.get_firstChild: IDOMNode;
begin
  Result := MakeNode(MSNode.firstChild);
end;

function TMSDOMNode.get_lastChild: IDOMNode;
begin
  Result := MakeNode(MSNode.lastChild);
end;

function TMSDOMNode.get_localName: DOMString;
begin
  Result := MSNode.baseName; 
end;

function TMSDOMNode.get_namespaceURI: DOMString;
begin
  Result := MSNode.namespaceURI;
end;

function TMSDOMNode.get_nextSibling: IDOMNode;
begin
  Result := MakeNode(MSNode.nextSibling);
end;

function TMSDOMNode.get_nodeName: DOMString;
begin
  Result := MSNode.nodeName;
end;

function TMSDOMNode.get_nodeType: DOMNodeType;
begin
  Result := MSNode.nodeType;
end;

function TMSDOMNode.get_nodeValue: DOMString;
var
  NodeValue: OleVariant;
begin
  NodeValue := MSNode.nodeValue;
  if not VarIsNull(NodeValue) then
    Result := NodeValue
  else
    Result := '';
end;

function TMSDOMNode.get_ownerDocument: IDOMDocument;
begin
  if not Assigned(FOwnerDocument) then
    FOwnerDocument := TMSDOMDocument.Create(MSNode.ownerDocument);
  Result := FOwnerDocument;
end;

function TMSDOMNode.get_parentNode: IDOMNode;
begin
  Result := MakeNode(MSNode.parentNode);
end;

function TMSDOMNode.get_prefix: DOMString;
begin
  Result := MSNode.prefix;
end;

function TMSDOMNode.get_previousSibling: IDOMNode;
begin
  Result := MakeNode(MSNode.previousSibling);
end;

function TMSDOMNode.hasChildNodes: WordBool;
begin
  Result := MSNode.hasChildNodes;
end;

function TMSDOMNode.insertBefore(const newChild,
  refChild: IDOMNode): IDOMNode;
begin
  Result := MakeNode(MSNode.insertBefore(GetMSNode(newChild), GetMSNode(refChild)));
end;

procedure TMSDOMNode.normalize;
begin
  DOMVendorNotSupported('normalize', SMSXML); { Do not localize }
end;

function TMSDOMNode.removeChild(const childNode: IDOMNode): IDOMNode;
begin
  Result := MakeNode(MSNode.removeChild(GetMSNode(childNode)));
end;

function TMSDOMNode.replaceChild(const newChild,
  oldChild: IDOMNode): IDOMNode;
begin
  Result := MakeNode(MSNode.replaceChild(GetMSNode(newChild), GetMSNode(oldChild)));
end;

procedure TMSDOMNode.set_nodeValue(value: DOMString);
begin
  MSNode.nodeValue := value;
end;

function TMSDOMNode.supports(const feature, version: DOMString): WordBool;
begin
  DOMVendorNotSupported('supports', SMSXML); { Do not localize }
  Result := False;
end;

function TMSDOMNode.GetXMLDOMNode: IXMLDOMNode;
begin
  Result := MSNode;
end;

function TMSDOMNode.selectNode(const nodePath: WideString): IDOMNode;
var
  Node: IXMLDOMNode;
begin
  Node := MSNode.selectSingleNode(nodePath);
  if Assigned(Node) then
    Result := MakeNode(Node) else
    Result := nil;
end;

function TMSDOMNode.selectNodes(const nodePath: WideString): IDOMNodeList;
var
  NodeList: IXMLDOMNodeList;
begin
  NodeList := MSNode.selectNodes(nodePath);
  if Assigned(NodeList) then
    Result := MakeNodeList(NodeList) else
    Result := nil;
end;

{ IDOMNodeEx Interface }

function TMSDOMNode.get_text: DOMString;
begin
  Result := MSNode.text;
end;

procedure TMSDOMNode.set_text(const Value: DOMString);
begin
  MSNode.text := Value;
end;

function TMSDOMNode.get_xml: DOMString;
begin
  Result := MSNode.xml;
end;

procedure TMSDOMNode.transformNode(const stylesheet: IDOMNode; var output: WideString);
begin
  output := MSNode.transformNode(GetMSNode(stylesheet));
end;

procedure TMSDOMNode.transformNode(const stylesheet: IDOMNode; const output: IDOMDocument);
begin
  MSNode.transformNodeToObject(GetMSNode(stylesheet), GetMSNode(output));
end;

{ TMSDOMNodeList }

constructor TMSDOMNodeList.Create(ANodeList: IXMLDOMNodeList);
begin
  inherited Create;
  FMSNodeList := ANodeList;
end;

function TMSDOMNodeList.get_item(index: Integer): IDOMNode;
begin
  Result := MakeNode(MSNodeList.Item[index]);
end;

function TMSDOMNodeList.get_length: Integer;
begin
  Result := MSNodeList.Length;
end;

{ TMSDOMNamedNodeMap }

constructor TMSDOMNamedNodeMap.Create(ANamedNodeMap: IXMLDOMNamedNodeMap);
begin
  inherited Create;
  FMSNamedNodeMap := ANamedNodeMap;
end;

function TMSDOMNamedNodeMap.get_item(index: Integer): IDOMNode;
begin
  Result := MakeNode(MSNamedNodeMap.item[index]);
end;

function TMSDOMNamedNodeMap.get_length: Integer;
begin
  Result := MSNamedNodeMap.length;
end;

function TMSDOMNamedNodeMap.getNamedItem(const name: DOMString): IDOMNode;
begin
  Result := MakeNode(MSNamedNodeMap.getNamedItem(name));
end;

function TMSDOMNamedNodeMap.getNamedItemNS(const namespaceURI,
  localName: DOMString): IDOMNode;
begin
  Result := MakeNode(MSNamedNodeMap.getQualifiedItem(localName, namespaceURI));
end;

function TMSDOMNamedNodeMap.removeNamedItem(const name: DOMString): IDOMNode;
begin
  Result := MakeNode(MSNamedNodeMap.removeNamedItem(name));
end;

function TMSDOMNamedNodeMap.removeNamedItemNS(const namespaceURI,
  localName: DOMString): IDOMNode;
begin
  Result := MakeNode(MSNamedNodeMap.removeQualifiedItem(localName, namespaceURI));
end;

function TMSDOMNamedNodeMap.setNamedItem(const newItem: IDOMNode): IDOMNode;
begin
  Result := MakeNode(MSNamedNodeMap.setNamedItem(GetMSNode(newItem)));
end;

function TMSDOMNamedNodeMap.setNamedItemNS(const arg: IDOMNode): IDOMNode;
begin
  Result := MakeNode(MSNamedNodeMap.setNamedItem(GetMSNode(arg)));
end;

{ TMSDOMCharacterData }

function TMSDOMCharacterData.GetMSCharacterData: IXMLDOMCharacterData;
begin
  Result := MSNode as IXMLDOMCharacterData;
end;

procedure TMSDOMCharacterData.appendData(const data: DOMString);
begin
  MSCharacterData.appendData(data);
end;

procedure TMSDOMCharacterData.deleteData(offset, count: Integer);
begin
  MSCharacterData.deleteData(offset, count);
end;

function TMSDOMCharacterData.get_data: DOMString;
begin
  Result := MSCharacterData.data;
end;

function TMSDOMCharacterData.get_length: Integer;
begin
  Result := MSCharacterData.length;
end;

procedure TMSDOMCharacterData.insertData(offset: Integer;
  const data: DOMString);
begin
  MSCharacterData.insertData(offset, data);
end;

procedure TMSDOMCharacterData.replaceData(offset, count: Integer;
  const data: DOMString);
begin
  MSCharacterData.replaceData(offset, count, data);
end;

procedure TMSDOMCharacterData.set_data(const data: DOMString);
begin
  MSCharacterData.data := data;
end;

function TMSDOMCharacterData.substringData(offset,
  count: Integer): DOMString;
begin
  Result := MSCharacterData.substringData(offset, count);
end;

{ TMSDOMAttr }

function TMSDOMAttr.GetMSAttribute: IXMLDOMAttribute;
begin
  Result := MSNode as IXMLDOMAttribute;
end;

function TMSDOMAttr.get_name: DOMString;
begin
  Result := MSAttribute.name;
end;

function TMSDOMAttr.get_ownerElement: IDOMElement;
begin
  DOMVendorNotSupported('get_ownerElement', SMSXML); { Do not localize }
  Result := nil;
end;

function TMSDOMAttr.get_specified: WordBool;
begin
  Result := MSAttribute.specified;
end;

function TMSDOMAttr.get_value: DOMString;
begin
  Result := MSAttribute.value;
end;

procedure TMSDOMAttr.set_value(const attributeValue: DOMString);
begin
  MSAttribute.value := attributeValue;
end;

{ TMSDOMElement }

function TMSDOMElement.GetMSElement: IXMLDOMElement;
begin
  Result := MSNode as IXMLDOMElement;
end;

function TMSDOMElement.get_tagName: DOMString;
begin
  Result := MSElement.tagName;
end;

function TMSDOMElement.getAttribute(const name: DOMString): DOMString;
begin
  Result := VarToWideStr(MSElement.getAttribute(name));
end;

function TMSDOMElement.getAttributeNS(const namespaceURI,
  localName: DOMString): DOMString;
var
  AttrNode: IDOMAttr;
begin
  AttrNode := getAttributeNodeNS(namespaceURI, localName);
  if Assigned(AttrNode) then
    Result := VarToWideStr(AttrNode.NodeValue)
  else
    Result := '';
end;

function TMSDOMElement.getAttributeNode(const name: DOMString): IDOMAttr;
begin
  Result := MakeNode(MSElement.getAttributeNode(name)) as IDOMAttr;
end;

function TMSDOMElement.getAttributeNodeNS(const namespaceURI,
  localName: DOMString): IDOMAttr;
begin
  Result := MakeNode(MSElement.Attributes.getQualifiedItem(localName, namespaceURI)) as IDOMAttr;
end;

function TMSDOMElement.getElementsByTagName(const name: DOMString): IDOMNodeList;
begin
  Result := MakeNodeList(MSElement.getElementsByTagName(name));
end;

function TMSDOMElement.getElementsByTagNameNS(const namespaceURI,
  localName: DOMString): IDOMNodeList;
begin
  DOMVendorNotSupported('getElementsByTagNameNS', SMSXML); { Do not localize }
end;

function TMSDOMElement.hasAttribute(const name: DOMString): WordBool;
begin
  { Note: use MS DOM function when/if it is added. }
  try
    Result := MSElement.getAttributeNode(name) <> nil;
  except
    Result := False;
  end;
end;

function TMSDOMElement.hasAttributeNS(const namespaceURI,
  localName: DOMString): WordBool;
begin
  { Note: use MS DOM function when/if it is added. }
  try
    Result := getAttributeNodeNS(namespaceURI, localName) <> nil;
  except
    Result := False;
  end;
end;

procedure TMSDOMElement.removeAttribute(const name: DOMString);
begin
  MSElement.removeAttribute(name);
end;

function TMSDOMElement.removeAttributeNode(const oldAttr: IDOMAttr): IDOMAttr;
begin
  Result := MakeNode(MSElement.removeAttributeNode(
    GetMSNode(oldAttr) as IXMLDOMAttribute)) as IDOMAttr;
end;

procedure TMSDOMElement.removeAttributeNS(const namespaceURI,
  localName: DOMString);
begin
  MSElement.attributes.removeQualifiedItem(localName, namespaceURI);
end;

procedure TMSDOMElement.setAttribute(const name, value: DOMString);
begin
  MSElement.setAttribute(name, value);
end;

function TMSDOMElement.setAttributeNode(const newAttr: IDOMAttr): IDOMAttr;
begin
  Result := MakeNode(MSElement.setAttributeNode(
    GetMSNode(newAttr) as IXMLDOMAttribute)) as IDOMAttr;
end;

function TMSDOMElement.setAttributeNodeNS(const newAttr: IDOMAttr): IDOMAttr;
begin
  Result := setAttributeNode(newAttr);
end;

procedure TMSDOMElement.setAttributeNS(const namespaceURI, qualifiedName,
  value: DOMString);
var
  AttrNode: IXMLDOMAttribute;
begin
  AttrNode := MSNode.ownerDocument.createNode(NODE_ATTRIBUTE, qualifiedName,
    namespaceURI) as IXMLDOMAttribute;
  AttrNode.nodeValue := value;
  MSElement.setAttributeNode(AttrNode);
end;

procedure TMSDOMElement.normalize;
begin
  MSElement.normalize;
end;

{ TMSDOMText }

function TMSDOMText.splitText(offset: Integer): IDOMText;
begin
  Result := MakeNode((MSNode as IXMLDOMText).splitText(offset)) as IDOMText;
end;

{ TMSDOMDocumentType }

function TMSDOMDocumentType.GetMSDocumentType: IXMLDOMDocumentType;
begin
  Result := MSNode as IXMLDOMDocumentType;
end;

function TMSDOMDocumentType.get_entities: IDOMNamedNodeMap;
begin
  if not Assigned(FEntities) then
    FEntities := MakeNamedNodeMap(MSDocumentType.get_entities);
  Result := FEntities;
end;

function TMSDOMDocumentType.get_internalSubset: DOMString;
begin
  DOMVendorNotSupported('get_internalSubset', SMSXML); { Do not localize }
end;

function TMSDOMDocumentType.get_name: DOMString;
begin
  Result := MSDocumentType.name;
end;

function TMSDOMDocumentType.get_notations: IDOMNamedNodeMap;
begin
  if not Assigned(FNotations) then
    FNotations := MakeNamedNodeMap(MSDocumentType.notations);
  Result := FNotations;
end;

function TMSDOMDocumentType.get_publicId: DOMString;
begin
  DOMVendorNotSupported('get_publicId', SMSXML); { Do not localize }
end;

function TMSDOMDocumentType.get_systemId: DOMString;
begin
  DOMVendorNotSupported('get_systemId', SMSXML); { Do not localize }
end;

{ TMSDOMNotation }

function TMSDOMNotation.GetMSNotation: IXMLDOMNotation;
begin
  Result := MSNode as IXMLDOMNotation;
end;

function TMSDOMNotation.get_publicId: DOMString;
begin
  Result := MSNotation.publicId;
end;

function TMSDOMNotation.get_systemId: DOMString;
begin
  Result := MSNotation.systemId;
end;

{ TMSDOMEntity }

function TMSDOMEntity.GetMSEntity: IXMLDOMEntity;
begin
  Result := MSNode as IXMLDOMEntity;
end;

function TMSDOMEntity.get_notationName: DOMString;
begin
  Result := MSEntity.notationName;
end;

function TMSDOMEntity.get_publicId: DOMString;
begin
  Result := MSEntity.publicId;
end;

function TMSDOMEntity.get_systemId: DOMString;
begin
  Result := MSEntity.systemId;
end;

{ TMSDOMProcessingInstruction }

function TMSDOMProcessingInstruction.GetMSProcessingInstruction: IXMLDOMProcessingInstruction;
begin
  Result := MSNode as IXMLDOMProcessingInstruction;
end;

function TMSDOMProcessingInstruction.get_data: DOMString;
begin
  Result := MSProcessingInstruction.data;
end;

function TMSDOMProcessingInstruction.get_target: DOMString;
begin
  Result := MSProcessingInstruction.target;
end;

procedure TMSDOMProcessingInstruction.set_data(const value: DOMString);
begin
  MSProcessingInstruction.data := value;
end;

{ TMSDOMEventHandler }

constructor TMSDOMEventHandler.Create(const Document: TMSDOMDocument;
  AsyncEventHandler: TAsyncEventHandler; Sender: TObject);
begin
  FDocument := Document; 
  FEventHandler := AsyncEventHandler;
  FSender := Sender;
end;

function TMSDOMEventHandler.GetIDsOfNames(const IID: TGUID;
  Names: Pointer; NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL
end;

function TMSDOMEventHandler.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
begin
  Result := E_NOTIMPL
end;

function TMSDOMEventHandler.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := E_NOTIMPL
end;

function TMSDOMEventHandler.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  try
    FEventHandler(FSender, FDocument.asyncLoadState);
  except
  end;
  Result := S_OK;
end;

{ TMSDOMDocument }

function TMSDOMDocument.GetMSDocument: IXMLDOMDocument;
begin
  Result := MSNode as IXMLDOMDocument;
end;

function TMSDOMDocument.createAttribute(const name: DOMString): IDOMAttr;
begin
  Result := MakeNode(MSDocument.createAttribute(name)) as IDOMAttr;
end;

function TMSDOMDocument.createAttributeNS(const namespaceURI,
  qualifiedName: DOMString): IDOMAttr;
begin
  Result := MakeNode(MSDocument.createNode(NODE_ATTRIBUTE, qualifiedName, namespaceURI)) as IDOMAttr;
end;

function TMSDOMDocument.createCDATASection(const data: DOMString): IDOMCDATASection;
begin
  Result := TMSDOMCDATASection.Create(MSDocument.createCDATASection(data)) as IDOMCDATASection;
end;

function TMSDOMDocument.createComment(const data: DOMString): IDOMComment;
begin
  Result := TMSDOMComment.Create(MSDocument.createComment(data)) as IDOMComment;
end;

function TMSDOMDocument.createDocumentFragment: IDOMDocumentFragment;
begin
  Result := TMSDOMDocumentFragment.Create(MSDocument.createDocumentFragment) as IDOMDocumentFragment;
end;

function TMSDOMDocument.createElement(const tagName: DOMString): IDOMElement;
begin
  Result := MakeNode(MSDocument.createElement(tagName)) as IDOMElement;
end;

function TMSDOMDocument.createElementNS(const namespaceURI, qualifiedName: DOMString): IDOMElement;
begin
  Result := MakeNode(MSDocument.createNode(NODE_ELEMENT, qualifiedName, namespaceURI)) as IDOMElement;
end;

function TMSDOMDocument.createEntityReference(const name: DOMString): IDOMEntityReference;
begin
  Result := IDOMEntityReference(TMSDOMEntityReference.Create(MSDocument.createEntityReference(name)));
end;

function TMSDOMDocument.createProcessingInstruction(const target, data: DOMString): IDOMProcessingInstruction;
begin
  Result := IDOMProcessingInstruction(TMSDOMProcessingInstruction.Create(MSDocument.createProcessingInstruction(target, data)));
end;

function TMSDOMDocument.createTextNode(const data: DOMString): IDOMText;
begin
  Result := MakeNode(MSDocument.createTextNode(data)) as IDOMText;
end;

function TMSDOMDocument.get_doctype: IDOMDocumentType;
begin
  Result := IDOMDocumentType(TMSDOMDocumentType.Create(MSDocument.docType));
end;

function TMSDOMDocument.get_documentElement: IDOMElement;
begin
  Result := MakeNode(MSDocument.documentElement) as IDOMElement;
end;

function TMSDOMDocument.get_domImplementation: IDOMImplementation;
begin
  Result := xmldom.IDOMImplementation(TMSDOMImplementation.Create(MSDocument.Implementation_));
end;

function TMSDOMDocument.getElementById(const elementId: DOMString): IDOMElement;
begin
  DOMVendorNotSupported('getElementById', SMSXML); { Do not localize }
end;

function TMSDOMDocument.getElementsByTagName(const tagName: DOMString): IDOMNodeList;
begin
  Result := MakeNodeList(MSDocument.getElementsByTagName(tagName));
end;

function TMSDOMDocument.getElementsByTagNameNS(const namespaceURI, localName: DOMString): IDOMNodeList;
begin
  DOMVendorNotSupported('getElementsByTagNameNS', SMSXML); { Do not localize }
end;

function TMSDOMDocument.importNode(importedNode: IDOMNode; deep: WordBool): IDOMNode;
begin
  DOMVendorNotSupported('importNode', SMSXML); { Do not localize }
end;

procedure TMSDOMDocument.set_documentElement(const IDOMElement: IDOMElement);
begin
  if Assigned(IDOMElement) then
    MSDocument.documentElement := GetMSNode(IDOMElement) as IXMLDOMElement
  else if Assigned(MSDocument.documentElement) then
    MSDocument.removeChild(MSDocument.documentElement);
end;

{ IDOMParseOptions Interface }

function TMSDOMDocument.get_async: Boolean;
begin
  Result := MSDocument.async;
end;

procedure TMSDOMDocument.set_async(Value: Boolean);
begin
  MSDocument.async := Value
end;

function TMSDOMDocument.get_preserveWhiteSpace: Boolean;
begin
  Result := MSDocument.Get_preserveWhiteSpace;
end;

function TMSDOMDocument.get_resolveExternals: Boolean;
begin
  Result := MSDocument.Get_resolveExternals;
end;

function TMSDOMDocument.get_validate: Boolean;
begin
  Result := MSDocument.Get_validateOnParse;
end;

procedure TMSDOMDocument.set_preserveWhiteSpace(Value: Boolean);
begin
  MSDocument.Set_preserveWhiteSpace(Value);
end;

procedure TMSDOMDocument.set_resolveExternals(Value: Boolean);
begin
  MSDocument.Set_resolveExternals(Value);
end;

procedure TMSDOMDocument.set_validate(Value: Boolean);
begin
  MSDocument.Set_validateOnParse(Value);
end;

{ IDOMPersist interface }

function TMSDOMDocument.asyncLoadState: Integer;
begin
  Result := MSDocument.readyState;
end;

function TMSDOMDocument.get_xml: DOMString;
begin
  Result := MSDocument.xml;
end;

function TMSDOMDocument.load(source: OleVariant): WordBool;
begin
  Result := MSDocument.load(source);
end;

function TMSDOMDocument.loadFromStream(const stream: TStream): WordBool;
var
  OleStream: IStream;
begin
  OleStream := TStreamAdapter.Create(stream);
  Result := MSDocument.load(OleStream);
end;

procedure TMSDOMDocument.save(destination: OleVariant);
begin
  MSDocument.save(destination);
end;

procedure TMSDOMDocument.saveToStream(const stream: TStream);
var
  OleStream: IStream;
begin
  OleStream := TStreamAdapter.Create(stream);
  MSDocument.save(OleStream);
end;

function TMSDOMDocument.loadxml(const Value: DOMString): WordBool;
begin
  Result := MSDocument.loadXML(Value);
end;

procedure TMSDOMDocument.set_OnAsyncLoad(const Sender: TObject;
  EventHandler: TAsyncEventHandler);
begin
  if Assigned(EventHandler) then
    MSDocument.onreadystatechange := TMSDOMEventHandler.Create(Self,
      EventHandler, Sender) as IDispatch
  else
    MSDocument.onreadystatechange := VarNull;
end;

function TMSDOMDocument.loadFromStream(const stream: IStream): WordBool;
begin
  Result := MSDocument.load(stream);
end;

procedure TMSDOMDocument.saveToStream(const stream: IStream);
begin
  MSDocument.save(stream);
end;

{ IDOMParseError }

function TMSDOMDocument.get_errorCode: Integer;
begin
  Result := MSDocument.parseError.get_errorCode;
end;

function TMSDOMDocument.get_filepos: Integer;
begin
  Result := MSDocument.parseError.get_filepos;
end;

function TMSDOMDocument.get_line: Integer;
begin
  Result := MSDocument.parseError.get_line;
end;

function TMSDOMDocument.get_linepos: Integer;
begin
  Result := MSDocument.parseError.get_linepos;
end;

function TMSDOMDocument.get_reason: WideString;
begin
  Result := MSDocument.parseError.get_reason;
end;

function TMSDOMDocument.get_srcText: WideString;
begin
  Result := MSDocument.parseError.get_srcText;
end;

function TMSDOMDocument.get_url: WideString;
begin
  Result := MSDocument.parseError.get_url;
end;

{ TMSDOMImplementationFactory }

function TMSDOMImplementationFactory.DOMImplementation: IDOMImplementation;
begin
  Result := TMSDOMImplementation.Create(nil);
end;

function TMSDOMImplementationFactory.Description: String;
begin
  Result := SMSXML;
end;

initialization
  MSXML_DOM := TMSDOMImplementationFactory.Create;
  RegisterDOMVendor(MSXML_DOM);
finalization
  UnRegisterDOMVendor(MSXML_DOM);
  MSXML_DOM.Free;
end.
