
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{       XML Schema Support                              }
{                                                       }
{ Copyright (c) 2001-2005 Borland Software Corporation  }
{                                                       }
{*******************************************************}

unit XMLSchema;

interface

uses SysUtils, Variants, Classes, xmldom, XMLDoc, XMLIntf;

const
  SXMLSchemaExt = '.xsd';   { Default }
  SXMLSchemaPrefix = 'xs';  { Default }
  SXMLSchemaLocation = 'schemaLocation';
  SXMLNoNSSchemaLocation = 'noNamespaceSchemaLocation';
  { Working Drafts - 5 November 1999 to 7 April 2000  }
  SXMLSchemaURI_1999 = SHttp+'/www.w3.org/1999/XMLSchema';
  SXMLSchemaInstURI_1999 = SHttp+'/www.w3.org/1999/XMLSchema-instance';
  { Working Draft 22 September 2000 to Candidate Rec. 10 October 2000 }
  SXMLSchemaURI_2000_10 = SHttp+'/www.w3.org/2000/10/XMLSchema';
  SXMLSchemaInstURI_2000_10 = SHttp+'/www.w3.org/2000/10/XMLSchema-instance';
  { Proposed Rec. 16 March 2001 to Final Rec. 02 May 2001  }
  SXMLSchemaURI_2001 = SHttp+'/www.w3.org/2001/XMLSchema';
  SXMLSchemaInstURI = SHttp+'/www.w3.org/2001/XMLSchema-instance';

type

{ Exceptions }

  ESchemaError = class(EXMLDocError);
  ESchemaValidation = class(ESchemaError);
  ESchemaParse = class(ESchemaError);

{ Misc. types }

  TFormChoice = (fcQualified, fcUnqualified);
  TCompositorType = (ctAll, ctChoice, ctSequence);
  TContentModel = (cmALL, cmChoice, cmSequence, cmGroupRef, cmEmpty);
  TDerivationMethod = (dmNone, dmComplexExtension, dmComplexRestriction,
    dmSimpleExtension, dmSimpleRestriction);
  TSimpleDerivationMethod = (sdmNone, sdmRestriction, sdmList, sdmUnion);

{ Interface Forwards }

  IXMLSchemaImport = interface;
  IXMLSchemaInclude = interface;
  IXMLTypeDef = interface;
  IXMLSimpleTypeDef = interface;
  IXMLComplexTypeDef = interface;
  IXMLAttributeDef = interface;
  IXMLElementDef = interface;
  IXMLElementCompositor = interface;
  IXMLAttributeGroup = interface;
  IXMLElementGroup = interface;
  IXMLNotation = interface;
  IXMLAnnotation = interface;
  IXMLAppInfo = interface;
  IXMLDocumentation = interface;
  IXMLEnumeration = interface;
  IXMLSchemaDef = interface;
  IXMLSchemaDoc = interface;

{ Collection Interface Forwards }

  IXMLSchemaItems = interface;
  IXMLComplexTypeDefs = interface;
  IXMLSimpleTypeDefs = interface;
  IXMLAttributeDefs = interface;
  IXMLElementDefs = interface;
  IXMLElementCompositors = interface;
  IXMLAttributeGroups = interface;
  IXMLElementGroups = interface;
  IXMLNotationDefs = interface;
  IXMLAnnotationItems = interface;
  IXMLAppInfoCollection = interface;
  IXMLDocumentationCollection = interface;
  IXMLEnumerationCollection = interface;

{ IXMLElementDefList }

  IXMLElementDefList = interface(IInterfaceList)
    ['{BA009DE0-8372-11D4-8E37-00C0F056BC66}']
    function GetElementDef(const Index: Integer): IXMLElementDef;
    property ElementDefs[const Index: Integer]: IXMLElementDef read GetElementDef; default;
  end;

{ IXMLAttributeDefList }

  IXMLAttributeDefList = interface(IInterfaceList)
    ['{6C15F622-846C-11D4-8E37-00C0F056BC66}']
    function GetAttributeDef(const Index: Integer): IXMLAttributeDef;
    property AttributeDefs[const Index: Integer]: IXMLAttributeDef read GetAttributeDef; default;
  end;

{ IXMLSchemaNode }

  IXMLSchemaNode = interface(IXMLNode)
   ['{2DF0AEF7-6A69-45DB-8F4D-C00D6BE3F0AA}']
    { Property Accessors }
    function GetSchemaDef: IXMLSchemaDef;
    { Public Properties }
    property SchemaDef: IXMLSchemaDef read GetSchemaDef;
  end;

{ IXMLAnnotatedItem }

  IXMLAnnotatedItem = interface(IXMLSchemaNode)
   ['{7EDFC120-4C43-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetAppInfoCollection: IXMLAppInfoCollection;
    function GetDocumentationCollection: IXMLDocumentationCollection;
    { Public Methods }
    function HasAnnotation: Boolean;
    procedure RemoveAnnotation(OnlyIfEmpty: Boolean = True);
    { Public Properties }
    property AppInfo: IXMLAppInfoCollection read GetAppInfoCollection;
    property Documentation: IXMLDocumentationCollection read GetDocumentationCollection;
  end;

{ IXMLSchemaDocRef }

  IXMLSchemaDocRef = interface(IXMLAnnotatedItem)
    ['{501411A9-74B8-46AF-A145-12E3A46A7DA4}']
    { Property Accessors }
    function GetSchemaLocation: DOMString;
    function GetSchemaRef: IXMLSchemaDef;
    procedure SetSchemaLocation(const Value: DOMString);
    { Public Properties }
    property SchemaRef: IXMLSchemaDef read GetSchemaRef;
    property SchemaLocation: DOMString read GetSchemaLocation write SetSchemaLocation;
  end;

{ IXMLSchemaImport }

  IXMLSchemaImport = interface(IXMLSchemaDocRef)
    ['{C15D7ABA-CAB6-4230-A732-BFC3C95F7B1A}']
  end;

{ IXMLSchemaInclude }

  IXMLSchemaInclude = interface(IXMLSchemaDocRef)
    ['{9C3A2E64-8CA5-403D-B557-1E28A9570720}']
  end;

{ IXMLSchemaItem }

  IXMLSchemaItem = interface(IXMLAnnotatedItem)
   ['{7EDFC120-4C43-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetName: DOMString;
    procedure SetName(const Value: DOMString);
    { Public Methods }
    function IsGlobal: Boolean;
    function ItemLocalName: DOMString;
    { Public Properties }
    property Name: DOMString read GetName write SetName;
  end;

{ IXMLTypeDef }

  IXMLTypeDef = interface(IXMLSchemaItem)
    ['{7EDFC121-4C43-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetBaseType: IXMLTypeDef;
    function GetBaseTypeName: DOMString;
    function GetBounded: Variant;
    function GetCardinality: Variant;
    function GetEnumerations: IXMLEnumerationCollection;
    function GetFractionalDigits: Variant;
    function GetLength: Variant;
    function GetMaxExclusive: Variant;
    function GetMaxInclusive: Variant;
    function GetMaxLength: Variant;
    function GetMinExclusive: Variant;
    function GetMinInclusive: Variant;
    function GetMinLength: Variant;
    function GetNumeric: Variant;
    function GetOrdered: Variant;
    function GetPattern: Variant;
    function GetTotalDigits: Variant;
    function GetWhitespace: Variant;
    procedure SetBaseTypeName(const Value: DOMString);
    procedure SetBounded(const Value: Variant);
    procedure SetCardinality(const Value: Variant);
    procedure SetFractionalDigits(const Value: Variant);
    procedure SetLength(const Value: Variant);
    procedure SetMaxExclusive(const Value: Variant);
    procedure SetMaxInclusive(const Value: Variant);
    procedure SetMaxLength(const Value: Variant);
    procedure SetMinExclusive(const Value: Variant);
    procedure SetMinInclusive(const Value: Variant);
    procedure SetMinLength(const Value: Variant);
    procedure SetNumeric(const Value: Variant);
    procedure SetOrdered(const Value: Variant);
    procedure SetPattern(const Value: Variant);
    procedure SetTotalDigits(const Value: Variant);
    procedure SetWhitespace(const Value: Variant);
    { Public Methods & Properties }
    function IsAnonymous: Boolean;
    function IsComplex: Boolean;
    property BaseType: IXMLTypeDef read GetBaseType;
    property BaseTypeName: DOMString read GetBaseTypeName write SetBaseTypeName;
    property Enumerations: IXMLEnumerationCollection read GetEnumerations;
    { Facets (in spec order) }
    property Ordered: Variant read GetOrdered write SetOrdered;
    property Bounded: Variant read GetBounded write SetBounded;
    property Cardinality: Variant read GetCardinality write SetCardinality;
    property Numeric: Variant read GetNumeric write SetNumeric;
    property Length: Variant read GetLength write SetLength;
    property MinLength: Variant read GetMinLength write SetMinLength;
    property MaxLength: Variant read GetMaxLength write SetMaxLength;
    property Pattern : Variant read GetPattern write SetPattern;
    property Whitespace: Variant read GetWhitespace write SetWhitespace;
    property MaxInclusive: Variant read GetMaxInclusive write SetMaxInclusive;
    property MaxExclusive: Variant read GetMaxExclusive write SetMaxExclusive;
    property MinInclusive: Variant read GetMinInclusive write SetMinInclusive;
    property MinExclusive: Variant read GetMinExclusive write SetMinExclusive;
    property TotalDigits: Variant read GetTotalDigits write SetTotalDigits;
    property FractionalDigits: Variant read GetFractionalDigits write SetFractionalDigits;
  end;

{ IXMLSimpleTypeContent }

  IXMLSimpleContent = interface(IXMLSchemaNode)
    ['{D355AA00-A9C9-4945-B97D-219A6E7E026F}']
  end;

{ IXMLSimpleTypeRestriction }

  IXMLSimpleTypeRestriction = interface(IXMLSimpleContent)
    ['{7BE665E6-9D59-4E27-AF37-587A8CCA6306}']
    { Property Accessors }
    function GetBaseName: DOMString;
    procedure SetBaseName(const Value: DOMString);
    { Public Properties }
    property BaseName: DOMString read GetBaseName write SetBasename;
  end;

{ IXMLSimpleTypeList }

  IXMLSimpleTypeList = interface(IXMLSimpleContent)
    ['{0E61C20B-9773-4716-AF81-1833000F15BC}']
    { Property Accessors }
    function GetItemType: DOMString;
    function GetSimpleTypeNode: IXMLSimpleTypeDef;
    procedure SetItemType(const Value: DOMString);
    { Public Properties }
    property ItemType: DOMString read GetItemType write SetItemType;
    property SimpleTypeNode: IXMLSimpleTypeDef read GetSimpleTypeNode;
  end;

{ IXMLSimpleTypeUnion }

  IXMLSimpleTypeUnion = interface(IXMLSimpleContent)
    ['{EA433957-14EE-4ACA-8B95-65060923EB56}']
    { Property Accessors }
    function GetMemberTypes: DOMString;
    function GetSimpleTypes: IXMLSimpleTypeDefs;
    procedure SetMemberTypes(const Value: DOMString);
    { Public Properties }
    property MemberTypes: DOMString read GetMemberTypes write SetMemberTypes;
    property SimpleTypes: IXMLSimpleTypeDefs read GetSimpleTypes;
  end;


{ IXMLSimpleTypeDef }

  IXMLSimpleTypeDef = interface(IXMLTypeDef)
    ['{7EDFC122-4C43-11D4-83DA-00C04F60B2DD}']
    function GetFacetParentNode(const Required: Boolean=True): IXMLNode;
    { Property Accessors }
    function GetFinal: DOMString;
    function GetContentNode: IXMLSimpleContent;
    function GetDerivationMethod: TSimpleDerivationMethod;
    function GetRestrictionNode: IXMLSimpleTypeRestriction;
    procedure SetFinal(const Value: DOMString);
    { Public Methods & Properties }
    function IsBuiltInType: Boolean;
    property ContentNode: IXMLSimpleContent read GetContentNode;
    property DerivationMethod: TSimpleDerivationMethod read GetDerivationMethod;
    property Final: DOMString read GetFinal write SetFinal;
    property RestrictionNode: IXMLSimpleTypeRestriction read GetRestrictionNode;
  end;

{ IXMLBaseTypeIndicator }

  IXMLBaseTypeIndicator = interface(IXMLSchemaNode)
    ['{3DD57E4A-43AC-4C12-B878-B10C7BFE19D4}']
    { Property Accessors }
    function GetMixed: Variant;
    procedure SetMixed(const Value: Variant);
    { Public Properties }
    property Mixed: Variant read GetMixed write SetMixed;
  end;

{ IXMLComplexTypeDef }

  IXMLComplexTypeDef = interface(IXMLTypeDef)
    ['{7EDFC123-4C43-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetAbstract: Boolean;
    function GetAttributeDefList: IXMLAttributeDefList;
    function GetAttributeDefs: IXMLAttributeDefs;
    function GetAttributeGroups: IXMLAttributeGroups;
    function GetBaseTypeIndicator: IXMLBaseTypeIndicator;
    function GetCompositorNode: IXMLElementCompositor;
    function GetCompositors: IXMLElementCompositors;
    function GetContentModel: TContentModel;
    function GetDerivationMethod: TDerivationMethod;
    function GetElementDefList: IXMLElementDefList;
    function GetElementDefs: IXMLElementDefs;
    function GetElementGroups: IXMLElementGroups;
    function GetGroupRef: IXMLElementGroup;
    procedure SetAbstract(const Value: Boolean);
    procedure SetContentModel(const Value: TContentModel);
    procedure SetDerivationMethod(const Value: TDerivationMethod);
    { Public Properties }
    property AbstractType: Boolean read GetAbstract write SetAbstract;
    property AttributeDefs: IXMLAttributeDefs read GetAttributeDefs;
    property AttributeDefList: IXMLAttributeDefList read GetAttributeDefList;
    property AttributeGroups: IXMLAttributeGroups read GetAttributeGroups;
    property BaseTypeIndicator: IXMLBaseTypeIndicator read GetBaseTypeIndicator;
    property CompositorNode: IXMLElementCompositor read GetCompositorNode;
    property ContentModel: TContentModel read GetContentModel write SetContentModel;
    property DerivationMethod: TDerivationMethod read GetDerivationMethod
      write SetDerivationMethod;
    property ElementCompositors: IXMLElementCompositors read GetCompositors;
    property ElementDefList: IXMLElementDefList read GetElementDefList;
    property ElementDefs: IXMLElementDefs read GetElementDefs;
    property ElementGroups: IXMLElementGroups read GetElementGroups;
    property GroupRef: IXMLElementGroup read GetGroupRef;
  end;

{ IXMLSchemaRefItem }

  IXMLSchemaRefItem = interface(IXMLSchemaItem)
   ['{41465201-D39A-4C39-ACBE-C3AF64F0720D}']
    { Property Accessors }
    function GetRefName: DOMString;
    procedure SetRefName( const Value: DOMString);
    { Public Properties and Methods }
    function GlobalCollection: IXMLSchemaItems;
    property RefName: DOMString read GetRefName write SetRefName;
  end;

{ IXMLTypedSchemaItem }

  IXMLTypedSchemaItem = interface(IXMLSchemaRefItem)
    ['{75D91C7E-6DFF-44CA-91E0-34E4314AE9D4}']
    { Property Accessors }
    function GetDataType: IXMLTypeDef;
    function GetDataTypeName: DOMString;
    procedure SetDataType(const Value: IXMLTypeDef);
    procedure SetDataTypeName(const Value: DOMString);
    { Public Properties }
    property DataType: IXMLTypeDef read GetDataType write SetDataType;
    property DataTypeName: DOMString read GetDataTypeName write SetDataTypeName;
  end;

{ IXMLAttributeDef }

  IXMLAttributeDef = interface(IXMLTypedSchemaItem)
    ['{7EDFC125-4C43-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetRef: IXMLAttributeDef;
    function GetUse: Variant;
    function GetDefault: Variant;
    function GetFixed: Variant;
    procedure SetRef(const Value: IXMLAttributeDef);
    procedure SetUse(const Value: Variant);
    procedure SetDefault(const Value: Variant);
    procedure SetFixed(const Value: Variant);
    { Public Properties }
    property Ref: IXMLAttributeDef read GetRef write SetRef;
    property Use: Variant read GetUse write SetUse;
    property Default: Variant read GetDefault write SetDefault;
    property Fixed: Variant read GetFixed write SetFixed;
  end;

{ IXMLElementDef }

  IXMLElementDef = interface(IXMLTypedSchemaItem)
    ['{7EDFC124-4C43-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetAttributeDefList: IXMLAttributeDefList;
    function GetChildElementList: IXMLElementDefList;
    function GetMaxOccurs: Variant;
    function GetMinOccurs: Variant;
    function GetRef: IXMLElementDef;
    procedure SetMaxOccurs(const Value: Variant);
    procedure SetMinOccurs(const Value: Variant);
    procedure SetRef(const Value: IXMLElementDef);
    { Public Methods & Properties }
    function IsRepeating: Boolean;
    property AttributeDefs: IXMLAttributeDefList read GetAttributeDefList;
    property ChildElements: IXMLElementDefList read GetChildElementList;
    property MaxOccurs: Variant read GetMaxOccurs write SetMaxOccurs;
    property MinOccurs: Variant read GetMinOccurs write SetMinOccurs;
    property Ref: IXMLElementDef read GetRef write SetRef;
  end;

{ IXMLElementCompositor }

  IXMLElementCompositor = interface(IXMLAnnotatedItem)
    ['{0B007C80-648B-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetCompositors: IXMLElementCompositors;
    function GetCompositorType: TCompositorType;
    function GetElementDefs: IXMLElementDefs;
    function GetElementGroups: IXMLElementGroups;
    function GetMaxOccurs: Variant;
    function GetMinOccurs: Variant;
    procedure SetMaxOccurs(const Value: Variant);
    procedure SetMinOccurs(const Value: Variant);
    { Public Properties }
    property Compositors: IXMLElementCompositors read GetCompositors;
    property CompositorType: TCompositorType read GetCompositorType;
    property ElementDefs: IXMLElementDefs read GetElementDefs;
    property ElementGroups: IXMLElementGroups read GetElementGroups;
    property MaxOccurs: Variant read GetMaxOccurs write SetMaxOccurs;
    property MinOccurs: Variant read GetMinOccurs write SetMinOccurs;
  end;

{ IXMLAttributeGroup }

  IXMLAttributeGroup = interface(IXMLSchemaRefItem)
    ['{8E5DB500-63C5-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetAttributeDefs: IXMLAttributeDefs;
    function GetAttributeGroups: IXMLAttributeGroups;
    function GetRef: IXMLAttributeGroup;
    procedure SetRef(const Value: IXMLAttributeGroup);
    { Public Properties }
    property AttributeDefs: IXMLAttributeDefs read GetAttributeDefs;
    property AttributeGroups: IXMLAttributeGroups read GetAttributeGroups;
    property Ref: IXMLAttributeGroup read GetRef write SetRef;
  end;

{ IXMLElementGroup }

  IXMLElementGroup = interface(IXMLSchemaRefItem)
    ['{53F27080-4DAF-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetCompositorNode: IXMLElementCompositor;
    function GetElementCompositors: IXMLElementCompositors;
    function GetElementDefs: IXMLElementDefs;
    function GetElementGroups: IXMLElementGroups;
    function GetRef: IXMLElementGroup;
    procedure SetRef(const Value: IXMLElementGroup);
    { Public Properties }
    property CompositorNode: IXMLElementCompositor read GetCompositorNode;
    property ElementCompositors: IXMLElementCompositors read GetElementCompositors;
    property ElementDefs: IXMLElementDefs read GetElementDefs;
    property ElementGroups: IXMLElementGroups read GetElementGroups;
    property Ref: IXMLElementGroup read GetRef write SetRef;
  end;

{ IXMLNotation }

  IXMLNotation = interface(IXMLSchemaItem)
    ['{6BC9D168-3AA1-4F00-A025-DAD2CD02E5B3}']
    { Property Accessors }
    procedure SetPublicID(const Value: Variant);
    procedure SetSystemID(const Value: Variant);
    function GetPublicID: Variant;
    function GetSystemID: Variant;
    { Public Properties }
    property PublicID: Variant read GetPublicID write SetPublicID;
    property SystemID: Variant read GetSystemID write SetSystemID;
  end;

{ IXMLAnnotation }

  IXMLAnnotation = interface(IXMLSchemaNode)
  ['{F3A9316D-4AF3-4CD7-8C5B-DBFEF236A8DA}']
    function GetAppInfoCollection: IXMLAppInfoCollection;
    function GetDocumentationCollection: IXMLDocumentationCollection;
    function HasAppInfo: Boolean;
    function HasDocumentation: Boolean;
    property AppInfo: IXMLAppInfoCollection read GetAppInfoCollection;
    property Documentation: IXMLDocumentationCollection read GetDocumentationCollection;
  end;

{ IXMLAppInfo }

  IXMLAppInfo = interface(IXMLSchemaNode)
    ['{FE7435C8-806F-441F-9195-A23F777D1954}']
    function GetSource: Variant;
    procedure SetSource(const Value: Variant);
    property Source: Variant read GetSource write SetSource;
  end;

{ IXMLDocumentation }

  IXMLDocumentation = interface(IXMLSchemaNode)
    ['{A1562492-BC31-4727-A690-17631848C2AF}']
    function GetSource: Variant;
    procedure SetSource(const Value: Variant);
    property Source: Variant read GetSource write SetSource;
  end;

{ IXMLEnumeration }

  IXMLEnumeration = interface(IXMLSchemaNode)
    ['{FBF0F414-AA54-4ADD-BD64-EA15D5AD380E}']
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
    property Value: Variant read GetValue write SetValue;
  end;

{ *** Collections *** }

{ IXMLSchemaRefs }

  IXMLSchemaDocRefs = interface(IXMLNodeCollection)
    ['{3C32E7BD-5CEE-4757-9C1D-266BB11983FE}']
    { Property Accessors }
    function GetSchemaDocRef(Index: Integer): IXMLSchemaDocRef;
    { Public Properties }
    property SchemaDocRef[Index: Integer]: IXMLSchemaDocRef read GetSchemaDocRef; default;
  end;

{ IXMLSchemaImports }

  IXMLSchemaImports = interface(IXMLSchemaDocRefs)
    ['{7EDFC126-4C43-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetItem(Index: Integer): IXMLSchemaImport;
    { Public Methods & Properties }
    function Add(const SchemaLocation: DOMString): IXMLSchemaImport; overload;
    function Insert(Index: Integer): IXMLSchemaImport;
    property Items[Index: Integer]: IXMLSchemaImport read GetItem; default;
  end;

{ IXMLSchemaIncludes }

  IXMLSchemaIncludes = interface(IXMLSchemaDocRefs)
    ['{7D9A30D4-007E-47C1-9BC1-AF8A1919777B}']
    { Property Accessors }
    function GetItem(Index: Integer): IXMLSchemaInclude;
    { Public Methods & Properties }
    function Add(const SchemaLocation: DOMString): IXMLSchemaInclude; overload;
    function Insert(Index: Integer): IXMLSchemaInclude;
    property Items[Index: Integer]: IXMLSchemaInclude read GetItem; default;
  end;

{ IXMLSchemaItems }

  IXMLSchemaItems = interface(IXMLNodeCollection)
    ['{7EDFC126-4C43-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetSchemaItem(Index: Integer): IXMLSchemaItem;
    { Public Methods & Properties }
    function FindItem(const Name: DOMString): IXMLSchemaItem;
    function IndexOfItem(const Name: DOMString): Integer;
    property Items[Index: Integer]: IXMLSchemaItem read GetSchemaItem; default;
  end;

{ IXMLSimpleTypeDefs }

  IXMLSimpleTypeDefs = interface(IXMLSchemaItems)
    ['{7EDFC127-4C43-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetItem(Index: Integer): IXMLSimpleTypeDef;
    { Public Methods & Properties }
    function Add(const Name: DOMString; BaseTypeName: DOMString = ''): IXMLSimpleTypeDef; overload;
    function Find(const Name: DOMString): IXMLSimpleTypeDef;
    function Insert(Index: Integer): IXMLSimpleTypeDef;
    property Items[Index: Integer]: IXMLSimpleTypeDef read GetItem; default;
  end;

{ IXMLComplexTypeDefs }

  IXMLComplexTypeDefs = interface(IXMLSchemaItems)
    ['{7EDFC128-4C43-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetItem(Index: Integer): IXMLComplexTypeDef;
    { Public Methods & Properties }
    function Add(const Name: DOMString;
      ContentModel: TContentModel = cmSequence): IXMLComplexTypeDef; overload;
    function Add(const Name, BaseTypeName: DOMString;
      DerivationMethod: TDerivationMethod = dmComplexExtension;
      ContentModel: TContentModel = cmSequence): IXMLComplexTypeDef;  overload;
    function Find(const Name: DOMString): IXMLComplexTypeDef;
    function Insert(Index: Integer): IXMLComplexTypeDef;
    property Items[Index: Integer]: IXMLComplexTypeDef read GetItem; default;
  end;

{ IXMLAttributeDefs }

  IXMLAttributeDefs = interface(IXMLSchemaItems)
    ['{7EDFC131-4C43-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetItem(Index: Integer): IXMLAttributeDef;
    { Public Methods & Properties }
    function Add(const RefName: DOMString): IXMLAttributeDef; overload;
    function Add(const Name, DataType: DOMString): IXMLAttributeDef; overload;
    function Add(const Name: DOMString; LocalSimpleType: Boolean;
      BaseTypeName: DOMString = ''): IXMLAttributeDef; overload;
    function Find(const Name: DOMString): IXMLAttributeDef;
    function Insert(Index: Integer): IXMLAttributeDef;
    property Items[Index: Integer]: IXMLAttributeDef read GetItem; default;
  end;

{ IXMLElementDefs }

  IXMLElementDefs = interface(IXMLSchemaItems)
    ['{7EDFC130-4C43-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetItem(Index: Integer): IXMLElementDef;
    { Public Methods & Properties }
    function Add(const RefName: DOMString): IXMLElementDef; overload;
    function Add(const Name, DataType: DOMString): IXMLElementDef; overload;
    function Add(const Name: DOMString; ComplexLocalType: Boolean;
      BaseTypeName: DOMString = ''): IXMLElementDef; overload;
    function Find(const Name: DOMString): IXMLElementDef;
    function Insert(Index: Integer): IXMLElementDef;
    property Items[Index: Integer]: IXMLElementDef read GetItem; default;
  end;

{ IXMLAttributeGroups }

  IXMLAttributeGroups = interface(IXMLSchemaItems)
    ['{8E5DB501-63C5-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetItem(Index: Integer): IXMLAttributeGroup;
    { Public Methods & Properties }
    function Add: IXMLAttributeGroup;
    function Find(const Name: DOMString): IXMLAttributeGroup;
    function Insert(Index: Integer): IXMLAttributeGroup;
    property Items[Index: Integer]: IXMLAttributeGroup read GetItem; default;
  end;

{ IXMLElementGroups }

  IXMLElementGroups = interface(IXMLSchemaItems)
    ['{7EDFC129-4C43-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetItem(Index: Integer): IXMLElementGroup;
    { Public Methods & Properties }
    function Add(const NameOrRef: DOMString): IXMLElementGroup; overload;
    function Find(const Name: DOMString): IXMLElementGroup;
    function Insert(Index: Integer): IXMLElementGroup;
    property Items[Index: Integer]: IXMLElementGroup read GetItem; default;
  end;

{ IXMLElementCompositors }

  IXMLElementCompositors = interface(IXMLSchemaItems)
    ['{0B007C81-648B-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetItem(Index: Integer): IXMLElementCompositor;
    { Public Methods & Properties }
    function Add(CompositorType: TCompositorType = ctSequence): IXMLElementCompositor;
    function Insert(Index: Integer): IXMLElementCompositor;
    property Items[Index: Integer]: IXMLElementCompositor read GetItem; default;
  end;

{ IXMLElementCompositors }

  IXMLNotationDefs = interface(IXMLSchemaItems)
    ['{6388BD4A-9E83-4249-A7F0-C8438F47D0AD}']
    { Property Accessors }
    function GetItem(Index: Integer): IXMLNotation;
    { Public Methods & Properties }
    function Add(const Name: DOMString; const APublicID: DOMString = '';
      const ASystemID: DOMString = ''): IXMLNotation; overload;
    function Insert(Index: Integer): IXMLNotation;
    property Items[Index: Integer]: IXMLNotation read GetItem; default;
  end;

{ IXMLAnnotations }

  IXMLAnnotationItems = interface(IXMLNodeCollection)
    ['{C6414FE7-A7AD-495B-9302-2E65537FE0A1}']
    function Add: IXMLAnnotation;
    function GetItem(Index: Integer): IXMLAnnotation;
    function Insert(Index: Integer): IXMLAnnotation;
  end;

{ IXMLAppInfoCollection }

  IXMLAppInfoCollection = interface(IXMLNodeCollection)
    ['{6C5C9808-FE59-4A7E-A38A-A17F710C0BE7}']
    function Add: IXMLAppInfo;
    function GetAppInfo(const AppInfoType: TGuid;
      AppInfoClassType: TXMLNodeClass = nil): IXMLAppInfo;
    function GetItem(Index: Integer): IXMLAppInfo;
    function Insert(Index: Integer): IXMLAppInfo;
    property Items[Index: Integer]: IXMLAppInfo read GetItem; default;
  end;

{ IXMLDocumentationCollection }

  IXMLDocumentationCollection = interface(IXMLNodeCollection)
    ['{F7820DA4-7C00-40D0-9F2C-AF726D8D4353}']
    function Add: IXMLDocumentation;
    function GetItem(Index: Integer): IXMLDocumentation;
    function Insert(Index: Integer): IXMLDocumentation;
    property Items[Index: Integer]: IXMLDocumentation read GetItem; default;
  end;

{ IXMLEnumerationCollection }

  IXMLEnumerationCollection = interface(IXMLNodeCollection)
    ['{3EB8EB97-1343-40E1-9E85-5E29C4EDD8C4}']
    function Add(const Value: DOMString): IXMLEnumeration;
    function GetItem(Index: Integer): IXMLEnumeration;
    function Insert(Index: Integer): IXMLEnumeration;
    property Items[Index: Integer]: IXMLEnumeration read GetItem; default;
  end;

{ IXMLSchemaDef }

  IXMLSchemaDef = interface(IXMLNOde)
    ['{7EDFC132-4C43-11D4-83DA-00C04F60B2DD}']
    { Property Accessors }
    function GetAnnotations: IXMLAnnotationItems;
    function GetAttributeDefs: IXMLAttributeDefs;
    function GetAttributeFormDefault: TFormChoice;
    function GetAttributeGroups: IXMLAttributeGroups;
    function GetBuiltInTypes: IXMLSimpleTypeDefs;
    function GetComplexTypes: IXMLComplexTypeDefs;
    function GetDocElementDef: IXMLElementDef;
    function GetElementDefs: IXMLElementDefs;
    function GetElementFormDefault: TFormChoice;
    function GetElementGroups: IXMLElementGroups;
    function GetNotationDefs: IXMLNotationDefs;
    function GetSchemaDoc: IXMLSchemaDoc;
    function GetSchemaImports: IXMLSchemaImports;
    function GetSchemaIncludes: IXMLSchemaIncludes;
    function GetSimpleTypes: IXMLSimpleTypeDefs;
    function GetStrictFormat: Boolean;
    function GetTargetNamespace: OleVariant;
    function GetTargetNSPrefix: DOMString;
    function GetVersion: DOMString;
    procedure SetAttributeformDefault(const Value: TFormChoice);
    procedure SetDocElementDef(const Value: IXMLElementDef);
    procedure SetElementformDefault(const Value: TFormChoice);
    procedure SetStrictFormat(const Value: Boolean);
    procedure SetVersion(const Value: DOMString);
    { Public Methods }
    function PrefixedTypeName(const TypeName: DOMString): DOMString;
    procedure RegisterAppInfo(const NamespaceURI, Prefix: DOMString;
      NodeClass: TXMLNodeClass);
    procedure SetTargetNamespace(const NamespaceURI, NSPrefix: DOMString);
    { Public Properties }
    property Annotations: IXMLAnnotationItems read GetAnnotations;
    property AttributeDefs: IXMLAttributeDefs read GetAttributeDefs;
    property AttributeFormDefault: TFormChoice read GetAttributeFormDefault
      write SetAttributeformDefault;
    property AttributeGroups: IXMLAttributeGroups read GetAttributeGroups;
    property BuiltInTypes: IXMLSimpleTypeDefs read GetBuiltInTypes;
    property ComplexTypes: IXMLComplexTypeDefs read GetComplexTypes;
    property DocElementDef: IXMLElementDef read GetDocElementDef
      write SetDocElementDef;
    property ElementDefs: IXMLElementDefs read GetElementDefs;
    property ElementFormDefault: TFormChoice read GetElementFormDefault
      write SetElementformDefault;
    property ElementGroups: IXMLElementGroups read GetElementGroups;
    property NotationDefs: IXMLNotationDefs read GetNotationDefs;
    property SchemaDoc: IXMLSchemaDoc read GetSchemaDoc;
    property SchemaImports: IXMLSchemaImports read GetSchemaImports;
    property SchemaIncludes: IXMLSchemaIncludes read GetSchemaIncludes;
    property SimpleTypes: IXMLSimpleTypeDefs read GetSimpleTypes;
    property StrictFormat: Boolean read GetStrictFormat write SetStrictFormat;
    property TargetNamespace: OleVariant read GetTargetNamespace;
    property TargetNSPrefix: DOMString read GetTargetNSPrefix;
    property Version: DOMString read GetVersion write SetVersion;
  end;

{ IXMLSchemaDoc }

// To disambiguiate between IXMLDocument in msxml.h and the
// one defined in Xmlintf.pas
{$IFDEF MSWINDOWS}
(*$HPPEMIT 'namespace Xmlschema {'         *)
(*$HPPEMIT 'using Xmlintf::IXMLDocument;'  *)
(*$HPPEMIT '};'                            *)
{$ENDIF}

  IXMLSchemaDoc = interface(IXMLDocument)
    ['{4E6B683C-15F4-4F62-9B4B-DF3130420F62}']
    { Property Accessors }
    function GetSchemaDef: IXMLSchemaDef;
    function GetWasImported: Boolean;
    { Public Properties }
    property SchemaDef: IXMLSchemaDef read GetSchemaDef;
    property WasImported: Boolean read GetWasImported;
  end;

{ IXMLSchemaTranslator }

  IXMLSchemaTranslator = interface(IInterface)
    ['{9C4DB79C-1D60-4BF2-95EE-63FDF98237D0}']
    procedure Translate(const FileName: DOMString; const SchemaDef: IXMLSchemaDef);
  end;

{ IXMLSchemaTranslatorFactory }

  IXMLSchemaTranslatorFactory = interface(IInterface)
    ['{AAB226EE-51C1-4902-B883-FE538EB1F27E}']
    { Public Methods }
    function CanExportFile(const FileName: WideString): Boolean;
    function CanImportFile(const FileName: WideString): Boolean;
    function GetExportTranslator: IXMLSchemaTranslator;
    function GetImportTranslator: IXMLSchemaTranslator;
    function GetTranslatorDescription: WideString;
  end;


{ ======================================================================== }
{                          Implementing Classes                            }
{ ======================================================================== }

{ Class Forwards }

  TXMLSchemaItems = class;
  TXMLComplexTypeDefs = class;
  TXMLSimpleTypeDefs = class;
  TXMLAttributeDefs = class;
  TXMLElementDefs = class;
  TXMLElementCompositors = class;
  TXMLAttributeGroups = class;
  TXMLElementGroups = class;
  TXMLNotationDefs = class;
  TXMLAppInfoCollection = class;
  TXMLDocumentationCollection = class;
  TXMLEnumerationCollection = class;

{ TXMLAttributeDefList }

  TXMLAttributeDefList = class(TInterfaceList, IXMLAttributeDefList)
    function GetAttributeDef(const Index: Integer): IXMLAttributeDef;
  end;

{ TXMLElementDefList }

  TXMLElementDefList = class(TInterfaceList, IXMLElementDefList)
    function GetElementDef(const Index: Integer): IXMLElementDef;
  end;

{ TXMLSchemaNode }

  TXMLSchemaDef = class;

  TXMLSchemaNode = class(TXMLNode, IXMLSchemaNode)
  protected
    function SchemaObject: TXMLSchemaDef;
    { IXMLSchemaNode interface }
    function GetSchemaDef: IXMLSchemaDef;
    property SchemaDef: IXMLSchemaDef read GetSchemaDef;
  end;

{ TXMLSchemaItem }

  TXMLSchemaItem = class(TXMLSchemaNode, IXMLAnnotatedItem, IXMLSchemaItem, IXMLSchemaRefItem)
  private
    FRefItem: IXMLSchemaRefItem;
    FAnnotation: IXMLAnnotation;
  protected
    { IXMLSchemaRefItem interface }
    function GlobalCollection: IXMLSchemaItems; virtual; 
    function GetRefItem: IXMLSchemaRefItem;
    procedure SetRefItem(const Value: IXMLSchemaRefItem); virtual;
    function IsGlobal: Boolean;
    { IXMLSchemaItem interface }
    function GetAnnotation: IXMLAnnotation;
    function GetAppInfoCollection: IXMLAppInfoCollection;
    function GetDocumentationCollection: IXMLDocumentationCollection;
    function GetName: DOMString; virtual;
    function GetRefName: DOMString;
    procedure SetName(const Value: DOMString); virtual;
    procedure SetRefName(const Value: DOMString);
    function HasAnnotation: Boolean;
    function ItemLocalName: DOMString;
    procedure RemoveAnnotation(OnlyIfEmpty: Boolean = True);
  protected
    property Annotation: IXMLAnnotation read GetAnnotation;
    property RefItem: IXMLSchemaRefItem read GetRefItem write SetRefItem;
    property SchemaDef: IXMLSchemaDef read GetSchemaDef;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSchemaDocRef }

  TXMLSchemaDocRef = class(TXMLSchemaItem, IXMLSchemaDocRef)
  private
    FSchemaRef: IXMLSchemaDef;
  protected
    { IXMLSchemaDocRef }
    function GetSchemaLocation: DOMString;
    function GetSchemaRef: IXMLSchemaDef;
    procedure SetSchemaLocation(const Value: DOMString);
  end;

{ TXMLSchemaImport }

  TXMLSchemaImport = class(TXMLSchemaDocRef, IXMLSchemaImport)
  end;

{ TXMLSchemaInclude }

  TXMLSchemaInclude = class(TXMLSchemaDocRef, IXMLSchemaInclude)
  end;

{ TXMLTypeDef }

  TXMLTypeDef = class(TXMLSchemaItem, IXMLTypeDef)
  private
    FEnumerations: IXMLEnumerationCollection;
  protected
    function ReadFacet(const FacetName: string): Variant; virtual;
    function GetFacetParentNode(const Required: Boolean=True): IXMLNode; virtual; abstract;
  protected
    { IXMLSchemaItem overrides }
    function GetName: DOMString; override;
    procedure SetName(const Value: DOMString); override;
    { IXMLTypeDef }
    function GetBaseType: IXMLTypeDef; virtual; abstract;
    function GetBaseTypeName: DOMString;
    function GetBounded: Variant;
    function GetCardinality: Variant;
    function GetEnumerations: IXMLEnumerationCollection;
    function GetFractionalDigits: Variant;
    function GetLength: Variant;
    function GetMaxExclusive: Variant;
    function GetMaxInclusive: Variant;
    function GetMaxLength: Variant;
    function GetMinExclusive: Variant;
    function GetMinInclusive: Variant;
    function GetMinLength: Variant;
    function GetNumeric: Variant;
    function GetOrdered: Variant;
    function GetPattern: Variant;
    function GetTotalDigits: Variant;
    function GetWhitespace: Variant;
    function IsAnonymous: Boolean;
    function IsComplex: Boolean; virtual;
    procedure SetBaseTypeName(const Value: DOMString); virtual; abstract;
    procedure SetBounded(const Value: Variant);
    procedure SetCardinality(const Value: Variant);
    procedure SetFractionalDigits(const Value: Variant);
    procedure SetLength(const Value: Variant);
    procedure SetMaxExclusive(const Value: Variant);
    procedure SetMaxInclusive(const Value: Variant);
    procedure SetMaxLength(const Value: Variant);
    procedure SetMinExclusive(const Value: Variant);
    procedure SetMinInclusive(const Value: Variant);
    procedure SetMinLength(const Value: Variant);
    procedure SetNumeric(const Value: Variant);
    procedure SetOrdered(const Value: Variant);
    procedure SetPattern(const Value: Variant);
    procedure SetTotalDigits(const Value: Variant);
    procedure SetWhitespace(const Value: Variant);
    procedure WriteFacet(const FacetName: string; const Value: Variant);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSimpleTypeContent }

  TXMLSimpleTypeContent = class(TXMLSchemaNode, IXMLSimpleContent)
  end;

{ TXMLTypeRestriction }

  TXMLSimpleTypeRestriction = class(TXMLSimpleTypeContent, IXMLSimpleTypeRestriction)
  protected
    { IXMLSimpleTypeRestriction }
    function GetBaseName: DOMString;
    procedure SetBaseName(const Value: DOMString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSimpleTypeList }

  TXMLSimpleTypeList = class(TXMLSimpleTypeContent, IXMLSimpleTypeList)
  protected
    { IXMLSimpleTypeList }
    function GetItemType: DOMString;
    function GetSimpleTypeNode: IXMLSimpleTypeDef;
    procedure SetItemType(const Value: DOMString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSimpleTypeUnion }

  TXMLSimpleTypeUnion = class(TXMLSimpleTypeContent, IXMLSimpleTypeUnion)
  private
    FSimpleTypes: IXMLSimpleTypeDefs;
  protected
    { IXMLSimpleTypeUnion }
    function GetMemberTypes: DOMString;
    function GetSimpleTypes: IXMLSimpleTypeDefs;
    procedure SetMemberTypes(const Value: DOMString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSimpleTypeDef }

  TXMLSimpleTypeDef = class(TXMLTypeDef, IXMLSimpleTypeDef)
  protected
    { IXMLTypeDef }
    function GetBaseType: IXMLTypeDef; override;
    function IsAnonymous: Boolean;
    function IsBuiltInType: Boolean;
    procedure SetBaseTypeName(const Value: DOMString); override;
    { IXMLSimpleTypeDef }
    function GetFinal: DOMString;
    function GetContentNode: IXMLSimpleContent;
    function GetDerivationMethod: TSimpleDerivationMethod;
    function GetFacetParentNode(const Required: Boolean=True): IXMLNode; override;
    function GetRestrictionNode: IXMLSimpleTypeRestriction;
    procedure SetFinal(const Value: DOMString);
  public
    procedure AfterConstruction; override;
  end;

{ IXMLComplexContent }

  IXMLComplexContent = interface(IXMLSchemaNode)
    ['{33A54303-05F7-4BF3-B2BB-61F9CD3C1CBB}']
    function GetAttributeDefs: IXMLAttributeDefs;
    function GetAttributeGroups: IXMLAttributeGroups;
    function GetCompositorNode: IXMLElementCompositor;
    function GetCompositors: IXMLElementCompositors;
    function GetContentModel: TContentModel;
    function GetElementDefs: IXMLElementDefs;
    function GetElementGroups: IXMLElementGroups;
    function GetGroupRef: IXMLElementGroup;
    procedure SetContentModel(const Value: TContentModel);
  end;

{ TXMLComplexContent }

  TXMLComplexContent = class(TXMLSchemaNode, IXMLComplexContent)
  private
    FAttributeDefs: IXMLAttributeDefs;
    FAttributeGroups: IXMLAttributeGroups;
    FCompositorNode: IXMLElementCompositor;
    FContentModel: TContentModel;
    FGroupRef: IXMLElementGroup;
  protected
    { IXMLComplexContent }
    function GetAttributeDefs: IXMLAttributeDefs;
    function GetAttributeGroups: IXMLAttributeGroups;
    function GetCompositorNode: IXMLElementCompositor;
    function GetCompositors: IXMLElementCompositors;
    function GetContentModel: TContentModel;
    function GetElementDefs: IXMLElementDefs;
    function GetElementGroups: IXMLElementGroups;
    function GetGroupRef: IXMLElementGroup;
    procedure SetContentModel(const Value: TContentModel);
  public
    procedure AfterConstruction; override;
    property CompositorNode: IXMLElementCompositor read GetCompositorNode;
  end;

{ TXMLBaseTypeIndicator }

  TXMLBaseTypeIndicator = class(TXMLSchemaNode, IXMLBaseTypeIndicator)
  protected
    { IXMLBaseTypeIndicator }
    function GetMixed: Variant;
    procedure SetMixed(const Value: Variant);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLComplexTypeDef }

  TXMLComplexTypeDef = class(TXMLTypeDef, IXMLComplexTypeDef)
  private
    FContentNode: IXMLComplexContent;
    FDerivationMethod: TDerivationMethod;
    FAttributeDefList: IXMLAttributeDefList;
    FElementDefList: IXMLElementDefList;
  protected
    procedure AttributesChanged;
    procedure ChildElementsChanged;
    procedure CheckContent;
    function GetContentNode: IXMLComplexContent;
    function GetFacetParentNode(const Required: Boolean=True): IXMLNode; override;
    { IXMLComplexTypeDef interface }
    function GetAbstract: Boolean;
    function GetAttributeDefList: IXMLAttributeDefList;
    function GetAttributeDefs: IXMLAttributeDefs;
    function GetAttributeGroups: IXMLAttributeGroups;
    function GetBaseType: IXMLTypeDef; override;
    function GetBaseTypeIndicator: IXMLBaseTypeIndicator;
    function GetCompositorNode: IXMLElementCompositor;
    function GetCompositors: IXMLElementCompositors;
    function GetContentModel: TContentModel;
    function GetDerivationMethod: TDerivationMethod;
    function GetElementDefs: IXMLElementDefs;
    function GetElementDefList: IXMLElementDefList;
    function GetElementGroups: IXMLElementGroups;
    function GetGroupRef: IXMLElementGroup;
    function IsComplex: Boolean; override;
    procedure SetAbstract(const Value: Boolean);
    procedure SetBaseTypeName(const Value: DOMString); override;
    procedure SetContentModel(const Value: TContentModel);
    procedure SetDerivationMethod(const Value: TDerivationMethod);
  public
    procedure AfterConstruction; override;
    property ContentNode: IXMLComplexContent read GetContentNode;
  end;

{ TXMLTypedSchemaItem }

  TXMLTypedSchemaItem = class(TXMLSchemaItem, IXMLTypedSchemaItem)
  private
    FTypeDef: IXMLTypeDef;
  protected
    procedure RemoveTypeInfo;
    procedure SetRefItem(const Value: IXMLSchemaRefItem); override;
    { IXMLTypedSchemaItem }
    function GetDataType: IXMLTypeDef;
    function GetDataTypeName: DOMString;
    procedure SetDataType(const Value: IXMLTypeDef);
    procedure SetDataTypeName(const Value: DOMString);
    property DataType: IXMLTypeDef read GetDataType;
  end;

{ TXMLAttributeDef }

  TXMLAttributeDef = class(TXMLTypedSchemaItem, IXMLAttributeDef)
  protected
    { IXMLSchemaRefItem }
    function GlobalCollection: IXMLSchemaItems; override;
    { IXMLAttributeDef interface }
    function GetRef: IXMLAttributeDef;
    function GetUse: Variant;
    function GetDefault: Variant;
    function GetFixed: Variant;
    procedure SetRef(const Value: IXMLAttributeDef);
    procedure SetUse(const Value: Variant);
    procedure SetDefault(const Value: Variant);
    procedure SetFixed(const Value: Variant);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLElementDef }

  TXMLElementDef = class(TXMLTypedSchemaItem, IXMLElementDef)
  protected
    { IXMLSchemaRefItem }
    function GlobalCollection: IXMLSchemaItems; override;
    { IXMLElementDef interface }
    function GetAttributeDefList: IXMLAttributeDefList;
    function GetChildElementList: IXMLElementDefList;
    function GetMaxOccurs: Variant;
    function GetMinOccurs: Variant;
    function GetRef: IXMLElementDef;
    function IsRepeating: Boolean;
    procedure SetMaxOccurs(const Value: Variant);
    procedure SetMinOccurs(const Value: Variant);
    procedure SetRef(const Value: IXMLElementDef);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLElementCompositor }

  TXMLElementCompositor = class(TXMLSchemaItem, IXMLElementCompositor)
  private
    FCompositorType: TCompositorType;
    FCompositors: IXMLElementCompositors;
    FElementDefs: IXMLElementDefs;
    FElementGroups: IXMLElementGroups;
  protected
    { IXMLElementCompositor interface }
    function GetCompositors: IXMLElementCompositors;
    function GetCompositorType: TCompositorType;
    function GetElementDefs: IXMLElementDefs;
    function GetElementGroups: IXMLElementGroups;
    function GetMaxOccurs: Variant;
    function GetMinOccurs: Variant;
    procedure SetMaxOccurs(const Value: Variant);
    procedure SetMinOccurs(const Value: Variant);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLAttributeGroup }

  TXMLAttributeGroup = class(TXMLSchemaItem, IXMLAttributeGroup)
  private
    FAttributeDefs: IXMLAttributeDefs;
    FAttributeGroups: IXMLAttributeGroups;
  protected
    { IXMLSchemaRefItem }
    function GlobalCollection: IXMLSchemaItems; override;
    { IXMLAttributeGroup interface }
    function GetAttributeDefs: IXMLAttributeDefs;
    function GetAttributeGroups: IXMLAttributeGroups;
    function GetRef: IXMLAttributeGroup;
    procedure SetRef(const Value: IXMLAttributeGroup);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLElementGroup }

  TXMLElementGroup = class(TXMLSchemaItem, IXMLElementGroup)
  private
    FContentNode: IXMLElementCompositor;
  protected
    { IXMLSchemaRefItem }
    function GlobalCollection: IXMLSchemaItems; override;
    { IXMLElementGroup interface }
    function GetCompositorNode: IXMLElementCompositor;
    function GetElementCompositors: IXMLElementCompositors;
    function GetElementDefs: IXMLElementDefs;
    function GetElementGroups: IXMLElementGroups;
    function GetRef: IXMLElementGroup;
    procedure SetRef(const Value: IXMLElementGroup);
  public
    procedure AfterConstruction; override;
    property ContentNode: IXMLElementCompositor read GetCompositorNode;
  end;

{ TXMLNotationDef }

  TXMLNotationDef = class(TXMLSchemaItem, IXMLNotation)
  protected
    { IXMLNotation interface }
    procedure SetPublicID(const Value: Variant);
    procedure SetSystemID(const Value: Variant);
    function GetPublicID: Variant;
    function GetSystemID: Variant;
    property PublicID: Variant read GetPublicID write SetPublicID;
    property SystemID: Variant read GetSystemID write SetSystemID;
  end;

{ TXMLAnnotationItem }

  TXMLAnnotationItem = class(TXMLSchemaNode, IXMLAppInfo, IXMLDocumentation)
  protected
    { IXMLAppInfo & IXMLDocumentation }
    function GetSource: Variant;
    procedure SetSource(const Value: Variant);
    property Source: Variant read GetSource write SetSource;
  end;

{ TXMLAnnotation }

  TXMLAnnotation = class(TXMLSchemaNode, IXMLAnnotation)
  private
    FAppInfo: IXMLAppInfoCollection;
    FDocumentation: IXMLDocumentationCollection;
  protected
    function CreateChildNode(const ADOMNode: IDOMNode): IXMLNode; override;
    { IXMLAnnotation interface }
    function HasAppInfo: Boolean;
    function HasDocumentation: Boolean;
    function GetAppInfoCollection: IXMLAppInfoCollection;
    function GetDocumentationCollection: IXMLDocumentationCollection;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLEnumeration }

  TXMLEnumeration = class(TXMLSchemaNode, IXMLEnumeration)
  protected
    { IXMLEnumeration interface }
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
  end;

{ *** Collections *** }

{ TXMLSchemaDocRefs }

  TXMLSchemaDocRefs = class(TXMLNodeCollection, IXMLSchemaDocRefs)
  protected
    function GetSchemaDocRef(Index: Integer): IXMLSchemaDocRef;
  end;

{ TXMLSchemaImports }

  TXMLSchemaImports = class(TXMLSchemaDocRefs, IXMLSchemaImports)
  protected
    { IXMLSchemaImports }
    function Add(const SchemaLocation: DOMString): IXMLSchemaImport; overload;
    function Insert(Index: Integer): IXMLSchemaImport;
    function GetItem(Index: Integer): IXMLSchemaImport;
  end;

{ TXMLSchemaIncludes }

  TXMLSchemaIncludes = class(TXMLSchemaDocRefs, IXMLSchemaIncludes)
  protected
    { IXMLSchemaIncludes }
    function Add(const SchemaLocation: DOMString): IXMLSchemaInclude; overload;
    function Insert(Index: Integer): IXMLSchemaInclude;
    function GetItem(Index: Integer): IXMLSchemaInclude;
  end;

{ TXMLSchemaItems }

  TXMLSchemaItems = class(TXMLNodeCollection, IXMLSchemaItems)
  private
    FIsGlobal: Boolean;
    FSchemaDef: TXMLNode;
    FCollectionClassType: TXMLNodeCollectionClass;
  protected
    { IXMLSchemaItems interface }
    function GetSchemaItem(Index: Integer): IXMLSchemaItem;
    function FindItem(const Name: DOMString): IXMLSchemaItem;
    function IndexOfItem(const Name: DOMString): Integer;
  protected
    function AddItem(Index: Integer): IXMLNode; override;
    function GetSchemaDef: IXMLSchemaDef;
    procedure SetSchemaDef(const Value: IXMLSchemaDef);
    procedure CheckParent;
    property SchemaDef: IXMLSchemaDef read GetSchemaDef write SetSchemaDef;
    property IsGlobal: Boolean read FIsGlobal;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSimpleTypeDefs }

  TXMLSimpleTypeDefs = class(TXMLSchemaItems, IXMLSimpleTypeDefs)
  protected
    { IXMLSimpleTypeDefs interface }
    function Add(const Name: DOMString; BaseTypeName: DOMString = ''): IXMLSimpleTypeDef;
    function Find(const Name: DOMString): IXMLSimpleTypeDef;
    function GetItem(Index: Integer): IXMLSimpleTypeDef;
    function Insert(Index: Integer): IXMLSimpleTypeDef;
  end;

{ TXMLBuiltInTypeDefs }

  TXMLBuiltInTypes = class(TXMLSimpleTypeDefs, IXMLSimpleTypeDefs)
  protected
    function IndexOfItem(const Name: DOMString): Integer;
    function IsBuiltInTypeName(const Name: DOMString): Boolean;
    { IXMLSimpleTypeDefs interface }
    function Add(const Name: DOMString; BaseTypeName: DOMString = ''): IXMLSimpleTypeDef;
    function Find(const Name: DOMString): IXMLSimpleTypeDef;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLComplexTypeDefs }

  TXMLComplexTypeDefs = class(TXMLSchemaItems, IXMLComplexTypeDefs)
  protected
    { IXMLComplexTypeDefs interface }
    function Add(const Name: DOMString;
      ContentModel: TContentModel = cmSequence): IXMLComplexTypeDef; overload;
    function Add(const Name, BaseTypeName: DOMString;
      DerivationMethod: TDerivationMethod = dmComplexExtension;
      ContentModel: TContentModel = cmSequence): IXMLComplexTypeDef;  overload;
    function Find(const Name: DOMString): IXMLComplexTypeDef;
    function GetItem(Index: Integer): IXMLComplexTypeDef;
    function Insert(Index: Integer): IXMLComplexTypeDef;
  end;

{ TXMLAttributeDefs }

  TXMLAttributeDefs = class(TXMLSchemaItems, IXMLAttributeDefs)
  protected
    procedure ChildListNotify(Operation: TNodeListOperation;
      var Node: IXMLNode; const IndexOrName: OleVariant; BeforeOperation: Boolean); override;
    { IXMLAttributeDefs interface }
    function Add(const RefName: DOMString): IXMLAttributeDef; overload;
    function Add(const Name, DataType: DOMString): IXMLAttributeDef; overload;
    function Add(const Name: DOMString; LocalSimpleType: Boolean;
      BaseTypeName: DOMString = ''): IXMLAttributeDef; overload;
    function Find(const Name: DOMString): IXMLAttributeDef;
    function GetItem(Index: Integer): IXMLAttributeDef;
    function Insert(Index: Integer): IXMLAttributeDef;
  end;

{ TXMLElementDefs }

  TXMLElementDefs = class(TXMLSchemaItems, IXMLElementDefs)
  protected
    procedure ChildListNotify(Operation: TNodeListOperation;
      var Node: IXMLNode; const IndexOrName: OleVariant; BeforeOperation: Boolean); override;
    procedure InternalAdd(const AElementDef: IXMLElementDef);
    { IXMLElementDefs interface }
    function Add(const RefName: DOMString): IXMLElementDef; overload;
    function Add(const Name, DataType: DOMString): IXMLElementDef; overload;
    function Add(const Name: DOMString; ComplexLocalType: Boolean;
      BaseTypeName: DOMString = ''): IXMLElementDef; overload;
    function Find(const Name: DOMString): IXMLElementDef;
    function GetItem(Index: Integer): IXMLElementDef;
    function Insert(Index: Integer): IXMLElementDef;
  end;

{ TXMLElementCompositors }

  TXMLElementCompositors = class(TXMLSchemaItems, IXMLElementCompositors)
  protected
    { IXMLElementCompositors interface }
    function Add(CompositorType: TCompositorType = ctSequence): IXMLElementCompositor;
    function GetItem(Index: Integer): IXMLElementCompositor;
    function Insert(Index: Integer): IXMLElementCompositor;
  end;

{ TXMLAttributeGroups }

  TXMLAttributeGroups = class(TXMLSchemaItems, IXMLAttributeGroups)
  protected
    { IXMLAttributeGroups interface }
    function Add: IXMLAttributeGroup;
    function Find(const Name: DOMString): IXMLAttributeGroup;
    function GetItem(Index: Integer): IXMLAttributeGroup;
    function Insert(Index: Integer): IXMLAttributeGroup;
  end;

{ TXMLElementGroups }

  TXMLElementGroups = class(TXMLSchemaItems, IXMLElementGroups)
  protected
    { IXMLElementGroups interface }
    function Add(const NameOrRef: DOMString): IXMLElementGroup;
    function Find(const Name: DOMString): IXMLElementGroup;
    function GetItem(Index: Integer): IXMLElementGroup;
    function Insert(Index: Integer): IXMLElementGroup;
  end;

{ TXMLNotationDefs }

  TXMLNotationDefs = class(TXMLSchemaItems, IXMLNotationDefs)
  protected
    { IXMLNotationDefs interface }
    function Add(const Name: DOMString; const APublicID: DOMString = '';
      const ASystemID: DOMString = ''): IXMLNotation;
    function GetItem(Index: Integer): IXMLNotation;
    function Insert(Index: Integer): IXMLNotation;
  end;

{ TXMLAnnotationCollection }

  TXMLAnnotationCollection = class(TXMLNodeCollection, IXMLAnnotationItems)
  protected
    { IXMLAnnotations interface }
    function Add: IXMLAnnotation;
    function GetItem(Index: Integer): IXMLAnnotation;
    function Insert(Index: Integer): IXMLAnnotation;
  end;

{ TXMLAppInfoCollection }

  TXMLAppInfoCollection = class(TXMLNodeCollection, IXMLAppInfoCollection)
  protected
    { IXMLAppInfoCollection interface }
    function GetAppInfo(const AppInfoType: TGuid;
      AppInfoClassType: TXMLNodeClass = nil): IXMLAppInfo;
    function Add: IXMLAppInfo;
    function GetItem(Index: Integer): IXMLAppInfo;
    function Insert(Index: Integer): IXMLAppInfo;
  end;

{ TXMLDocumentationCollection }

  TXMLDocumentationCollection = class(TXMLNodeCollection, IXMLDocumentationCollection)
  protected
    { IXMLDocumentationCollection interface }
    function Add: IXMLDocumentation;
    function GetItem(Index: Integer): IXMLDocumentation;
    function Insert(Index: Integer): IXMLDocumentation;
  end;

{ TXMLEnumerationCollection }

  TXMLEnumerationCollection = class(TXMLNodeCollection, IXMLEnumerationCollection)
  protected
    { IXMLEnumerationCollection interface }
    function Add(const Value: DOMString): IXMLEnumeration;
    function GetItem(Index: Integer): IXMLEnumeration;
    function Insert(Index: Integer): IXMLEnumeration;
  end;

{ TXMLSchemaDef }

  TAppInfoClassInfo = record
    Prefix: DOMString;
    NamespaceURI: DOMString;
    NodeClass: TXMLNodeClass;
  end;
  TAppInfoClassArray = array of TAppInfoClassInfo;

  TXMLSchemaDef = class(TXMLNode, IXMLSchemaDef)
  private
    FAnnotations: IXMLAnnotationItems;
    FAttributeDefs: IXMLAttributeDefs;
    FAttributeGroups: IXMLAttributeGroups;
    FBuiltInTypes: IXMLSimpleTypeDefs;
    FComplexTypes: IXMLComplexTypeDefs;
    FDocElementDef: IXMLElementDef;
    FElementDefs: IXMLElementDefs;
    FElementGroups: IXMLElementGroups;
    FSimpleTypes: IXMLSimpleTypeDefs;
    FNotationDefs: IXMLNotationDefs;
    FAppInfoClasses: TAppInfoClassArray;
    FSchemaImports: IXMLSchemaImports;
    FSchemaIncludes: IXMLSchemaIncludes;
    FTargetNSPrefix: DOMString;
    FStrictFormat: Boolean;
    FLegacyFormat: Boolean;
  protected
    function FindAppInfoClass(const NamespaceURI: DOMString): TXMLNodeClass;
    function GetFormChoice(const ChoiceName: DOMString): TFormChoice;
    function LoadSchemaRef(SchemaLoc: DOMString): IXMLSchemaDef;
    function LocateAppInfo(const NamespaceURI: DOMString;
      var AppInfoClassInfo: TAppInfoClassInfo): Boolean;
    procedure SetFormChoice(const ChoiceName: DOMString; const Value: TFormChoice);
    { IXMLSchemaDef inteface }
    function GetAnnotations: IXMLAnnotationItems;
    function GetAttributeDefs: IXMLAttributeDefs;
    function GetAttributeFormDefault: TFormChoice;
    function GetAttributeGroups: IXMLAttributeGroups;
    function GetBuiltInTypes: IXMLSimpleTypeDefs;
    function GetComplexTypes: IXMLComplexTypeDefs;
    function GetDocElementDef: IXMLElementDef;
    function GetElementDefs: IXMLElementDefs;
    function GetElementFormDefault: TFormChoice;
    function GetElementGroups: IXMLElementGroups;
    function GetNotationDefs: IXMLNotationDefs;
    function GetSchemaDoc: IXMLSchemaDoc;
    function GetSchemaImports: IXMLSchemaImports;
    function GetSchemaIncludes: IXMLSchemaIncludes;
    function GetSimpleTypes: IXMLSimpleTypeDefs;
    function GetStrictFormat: Boolean;
    function GetTargetNamespace: OleVariant;
    function GetTargetNSPrefix: DOMString;
    function GetVersion: DOMString;
    function PrefixedTypeName(const TypeName: DOMString): DOMString;
    procedure RegisterAppInfo(const NamespaceURI, Prefix: DOMString;
      NodeClass: TXMLNodeClass);
    procedure SetAttributeformDefault(const Value: TFormChoice);
    procedure SetDocElementDef(const Value: IXMLElementDef);
    procedure SetElementformDefault(const Value: TFormChoice);
    procedure SetStrictFormat(const Value: Boolean);
    procedure SetTargetNamespace(const NamespaceURI, NSPrefix: DOMString);
    procedure SetVersion(const Value: DOMString);
  public
    procedure AfterConstruction; override;
    property AttributeFormDefault: TFormChoice read GetAttributeFormDefault write SetAttributeformDefault;
    property DocElementDef: IXMLElementDef read GetDocElementDef
      write SetDocElementDef;
    property ElementFormDefault: TFormChoice read GetElementFormDefault write SetElementformDefault;
    property IsLegacyFormat: Boolean read FLegacyFormat;
    property TargetNamespace: OleVariant read GetTargetNamespace;
    property TargetNSPrefix: DOMString read GetTargetNSPrefix;
    property Version: DOMString read GetVersion write SetVersion;
  end;

{ TXMLSchemaDoc }

  TXMLSchemaDoc = class(TXMLDocument, IXMLSchemaDoc)
  private
    FWasImported: Boolean;
    FEnableTranslation: Boolean;
  protected
    { IXMLSchemaDoc }
    function GetWasImported: Boolean;
    function GetSchemaDef: IXMLSchemaDef;
  protected
    procedure CheckSchemaVersion; virtual;
    procedure DoAfterOpen; override;
    procedure LoadData; override;
    procedure PrepareNewSchema; virtual;
    property EnableTranslation: Boolean read FEnableTranslation write FEnableTranslation;
  public
    procedure AfterConstruction; override;
    procedure SaveToFile(const AFileName: WideString = ''); override;
  end;

{ TXMLSchemaTranslator }

  TXMLSchemaTranslator = class(TInterfacedObject, IXMLSchemaTranslator)
  private
    FFileName: WideString;
    FSchemaDef: IXMLSchemaDef;
  protected
    { IXMLSchemaTranslator }
    procedure Translate(const FileName: WideString; const SchemaDef: IXMLSchemaDef); virtual;
    { Data member access }
    property FileName: WideString read FFileName;
    property SchemaDef: IXMLSchemaDef read FSchemaDef;
  end;

{ TXMLSchemaTranslatorFactory }

  TSchemaTranslatorClass = class of TXMLSchemaTranslator;

  TXMLSchemaTranslatorFactory = class(TInterfacedObject, IXMLSchemaTranslatorFactory)
  private
    FDescription: WideString;
    FExtension: WideString;
    FExportTranslatorClass: TSchemaTranslatorClass;
    FImportTranslatorClass: TSchemaTranslatorClass;
  protected
    { IXMLSchemaTranslatorFactory }
    function CanExportFile(const FileName: WideString): Boolean; virtual;
    function CanImportFile(const FileName: WideString): Boolean; virtual;
    function GetExportTranslator: IXMLSchemaTranslator;
    function GetImportTranslator: IXMLSchemaTranslator;
    function GetTranslatorDescription: WideString;
  public
    constructor Create(ImportClass, ExportClass: TSchemaTranslatorClass;
      const Extension, Description: WideString);
  end;


{ Schema Helpers }

function GetXMLSchema(DOMDocument: IDOMDocument): IXMLSchemaDoc;
function LoadXMLSchema(const FileName: DOMString): IXMLSchemaDoc;
function LoadXMLSchemaStr(const XML: DOMString): IXMLSchemaDoc;
function NewXMLSchema(const TargetNamespace: DOMString = '';
  const TNSPrefix: DOMString = '';
  const XSDPrefix: DOMString = SXMLSchemaPrefix): IXMLSchemaDoc;

{ Translation }

function FindSchemaExporter(const FileName: string): IXMLSchemaTranslator;
function FindSchemaImporter(const FileName: string): IXMLSchemaTranslator;
procedure RegisterSchemaTranslator(const TranslatorFactory: IXMLSchemaTranslatorFactory);
procedure UnRegisterSchemaTranslator(const TranslatorFactory: IXMLSchemaTranslatorFactory);

{ Utility Routines }

function GetDocumentation(SchemaItem: IXMLSchemaItem; AllEntries: Boolean = False): string;
procedure SchemaValidationError(const Msg: string; const Args: array of const);
procedure SchemaParseError(const Msg: string; const Args: array of const);
function FindItemNS(SchemaItems: TXMLSchemaItems; const Name, ItemNS: DOMString): IXMLSchemaItem;

var
  TranslatorList: IInterfaceList;
  XMLSchemaURI: DOMString = SXMLSchemaURI_2001;

implementation

uses XMLSchemaTags, XMLConst;


{ Utility routines }

function ArrayContains(const Value: DOMString; const Values: array of DOMString): Boolean;
var
  I: Integer;
begin
  for I := Low(Values) to High(Values) do
    if Value = Values[I] then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

function IsHosted(const Node: IXMLNode): Boolean;
begin
  Result := (Node as IXMLNodeAccess).HostNode <> nil;
end;

function GetDocumentation(SchemaItem: IXMLSchemaItem;
  AllEntries: Boolean = False): string;
var
  I: Integer;
begin
  Result := '';
  if SchemaItem.HasAnnotation then
    for I := 0 to SchemaItem.Documentation.Count - 1 do
    begin
      Result := Result + SchemaItem.Documentation[I].Text;
      if not AllEntries then Break;
    end;
end;

function GetXMLSchema(DOMDocument: IDOMDocument): IXMLSchemaDoc;
begin
  Result := TXMLSchemaDoc.Create(nil);
  Result.DOMDocument := DOMDocument;
end;

function LoadXMLSchema(const FileName: DOMString): IXMLSchemaDoc;
begin
  Result := TXMLSchemaDoc.Create(nil);
  Result.LoadFromFile(FileName);
end;

function LoadXMLSchemaStr(const XML: DOMString): IXMLSchemaDoc;
begin
  Result := TXMLSchemaDoc.Create(nil);
  Result.LoadFromXML(XML);
end;

function NewXMLSchema(const TargetNamespace: DOMString = '';
  const TNSPrefix: DOMString = ''; const XSDPrefix: DOMString = SXMLSchemaPrefix): IXMLSchemaDoc;
begin
  Result := TXMLSchemaDoc.Create(nil);
  Result.Active := True;
  Result.AddChild(MakeNodeName(XSDPrefix, SSchema), XMLSchemaURI);
  if TargetNamespace <> '' then
    Result.SchemaDef.SetTargetNamespace(TargetNamespace, TNSPrefix);
end;

procedure SchemaValidationError(const Msg: string; const Args: array of const);
begin
  raise ESchemaValidation.CreateFmt(Msg, Args);
end;

procedure SchemaParseError(const Msg: string; const Args: array of const);
begin
  raise ESchemaParse.CreateFmt(Msg, Args);
end;

{ Translator Registration }

procedure RegisterSchemaTranslator(const TranslatorFactory: IXMLSchemaTranslatorFactory);
begin
  if TranslatorList = nil then TranslatorList := TInterfaceList.Create;
  if TranslatorList.IndexOf(TranslatorFactory) < 0 then
    TranslatorList.Add(TranslatorFactory);
end;

procedure UnRegisterSchemaTranslator(const TranslatorFactory: IXMLSchemaTranslatorFactory);
begin
  if TranslatorList <> nil then
    TranslatorList.Remove(TranslatorFactory);
end;

function FindSchemaExporter(const FileName: string): IXMLSchemaTranslator;
var
  I: Integer;
begin
  Result := nil;
  if not Assigned(TranslatorList) then Exit;
  for I := TranslatorList.Count - 1 downto 0 do
  begin
    if (TranslatorList[I] as IXMLSchemaTranslatorFactory).CanExportFile(FileName) then
    begin
      Result := (TranslatorList[I] as IXMLSchemaTranslatorFactory).GetExportTranslator;
      Break;
    end;
  end;
end;

function FindSchemaImporter(const FileName: string): IXMLSchemaTranslator;
var
  I: Integer;
begin
  Result := nil;
  if not Assigned(TranslatorList) then Exit;
  for I := TranslatorList.Count - 1 downto 0 do
  begin
    if (TranslatorList[I] as IXMLSchemaTranslatorFactory).CanImportFile(FileName) then
    begin
      Result := (TranslatorList[I] as IXMLSchemaTranslatorFactory).GetImportTranslator;
      Break;
    end;
  end;
end;

{ TXMLAttributeDefList }

function TXMLAttributeDefList.GetAttributeDef(
  const Index: Integer): IXMLAttributeDef;
begin
  Result := Get(Index) as IXMLAttributeDef;
end;

{ TXMLElementDefList }

function TXMLElementDefList.GetElementDef(const Index: Integer): IXMLElementDef;
begin
  Result := Get(Index) as IXMLElementDef;
end;

{ TXMLSchemaNode }

function TXMLSchemaNode.GetSchemaDef: IXMLSchemaDef;
begin
  Result := SchemaObject;
end;

function TXMLSchemaNode.SchemaObject: TXMLSchemaDef;
var
  Node: TXMLNode;
begin
  Node := Self;
  while Assigned(Node) and not (Node is TXMLSchemaDef) do
    Node := TXMLSchemaNode(Node).ParentNode;
  Result := Node as TXMLSchemaDef;
end;

{ TXMLSchemaItem }

procedure TXMLSchemaItem.AfterConstruction;
begin
  RegisterChildNode(SAnnotation, TXMLAnnotation);
  inherited;
end;

function TXMLSchemaItem.GetName: DOMString;
begin
  if Assigned(RefItem) then
    Result := RefItem.GetName
  else if HasAttribute(SName) then
    Result := VarToWideStr(GetAttribute(SName))
  else if HasAttribute(SRef) then
    { If there is no name, and we can't resolve the ref, then just return
      the name of the ref }
    Result := VarToWideStr(GetAttribute(SRef))
  else
    Result := '';
end;

procedure TXMLSchemaItem.SetName(const Value: DOMString);
begin
  if Assigned(RefItem) then
    SchemaValidationError(SNoNameOnRef, []);
  SetAttribute(SName, Value);
end;

function TXMLSchemaItem.ItemLocalName: DOMString;
begin
  Result := ExtractLocalName(GetName);
end;

function TXMLSchemaItem.GetRefName: DOMString;
begin
  Result := VarToWideStr(GetAttribute(SRef));
end;

procedure TXMLSchemaItem.SetRefName(const Value: DOMString);
begin
  if Value = '' then
    SetAttribute(SRef, Null)
  else if (SchemaDef.TargetNSPrefix <> '') and not IsPrefixed(Value) then
    SetAttribute(SRef, MakeNodeName(SchemaDef.TargetNSPrefix, Value))
  else
    SetAttribute(SRef, Value);
  FRefItem := nil;
end;

function TXMLSchemaItem.GlobalCollection: IXMLSchemaItems;
begin
  Result := nil; { Descendent types should override to point to collection }
end;

function TXMLSchemaItem.GetRefItem: IXMLSchemaRefItem;
begin
  if not Assigned(FRefItem) and HasAttribute(SRef) and (GlobalCollection <> nil) then
    FRefItem := GlobalCollection.FindItem(GetAttribute(SRef)) as IXMLSchemaRefItem;
  Result := FRefItem;
end;

procedure TXMLSchemaItem.SetRefItem(const Value: IXMLSchemaRefItem);
begin
  if IsGlobal then
    SchemaValidationError(SNoGlobalRef, []);
  if not Assigned(Value) then
    SetRefName('')
  else
  begin
    SetAttribute(SName, Null);
    SetRefName(Value.Name);
  end;
end;

function TXMLSchemaItem.GetAnnotation: IXMLAnnotation;
begin
  if not Assigned(FAnnotation) then
  begin
    FAnnotation := ChildNodes.FindNode(SAnnotation) as IXMLAnnotation;
    if not Assigned(FAnnotation) then
      FAnnotation := AddChild(SAnnotation, 0) as IXMLAnnotation;
  end;
  Result := FAnnotation;
end;

function TXMLSchemaItem.HasAnnotation: Boolean;
begin
  Result := HasChildNode(SAnnotation);
end;

procedure TXMLSchemaItem.RemoveAnnotation(OnlyIfEmpty: Boolean);
begin
  if Assigned(FAnnotation) and not OnlyIfEmpty or not FAnnotation.HasChildNodes then
  begin
    ChildNodes.Remove(FAnnotation);
    FAnnotation := nil;
  end;
end;

function TXMLSchemaItem.GetAppInfoCollection: IXMLAppInfoCollection;
begin
  Result := Annotation.AppInfo;
end;

function TXMLSchemaItem.GetDocumentationCollection: IXMLDocumentationCollection;
begin
  Result := Annotation.Documentation;
end;

function TXMLSchemaItem.IsGlobal: Boolean;
begin
  Result := (ParentNode as IXMLNode).GetLocalName = SSchema;
end;

{ TXMLSchemaDocRef }

function TXMLSchemaDocRef.GetSchemaLocation: DOMString;
begin
  Result := VarToWideStr(GetAttribute(SSchemaLocation));
end;

function TXMLSchemaDocRef.GetSchemaRef: IXMLSchemaDef;
begin
  if not Assigned(FSchemaRef) then
    FSchemaRef := SchemaObject.LoadSchemaRef(GetSchemaLocation);
  Result := FSchemaRef;
end;

procedure TXMLSchemaDocRef.SetSchemaLocation(const Value: DOMString);
begin
  SetAttribute(SSchemaLocation, Value);
end;


{ TXMLTypeDef }

procedure TXMLTypeDef.AfterConstruction;
begin
  RegisterChildNodes([SAttribute, SAttributeGroup],
    [TXMLAttributeDef, TXMLAttributeGroup]);
  inherited;
end;

function TXMLTypeDef.GetName: DOMString;
begin
  { For unnamed local types, default to the name of the owning element }
  if Assigned(ParentNode) and (ParentNode is TXMLTypedSchemaItem) then
    Result := (ParentNode as TXMLTypedSchemaItem).GetName
  else
  begin
    if not HasAttribute(SName) then
      SchemaParseError(SMissingName, []);
    Result := inherited GetName;
  end;
end;

procedure TXMLTypeDef.SetName(const Value: DOMString);
begin
  if Assigned(ParentNode) and (ParentNode is TXMLElementDef) then
    SchemaValidationError(SNoLocalTypeName, [GetName]);
  SetAttribute(SName, Value);
end;

function TXMLTypeDef.GetBaseTypeName: DOMString;
begin
  if GetBaseType <> nil then
    Result := GetBaseType.Name
  else
    Result := '';
end;

function TXMLTypeDef.IsComplex: Boolean;
begin
  Result := False;
end;

function TXMLTypeDef.IsAnonymous: Boolean;
begin
  Result := Assigned(ParentNode) and not (ParentNode is TXMLSchemaDef);
end;

function TXMLTypeDef.GetBounded: Variant;
begin
  Result := ReadFacet(xsfBounded);
end;

function TXMLTypeDef.GetCardinality: Variant;
begin
  Result := ReadFacet(xsfCardinality);
end;

function TXMLTypeDef.GetEnumerations: IXMLEnumerationCollection;
var
  ReadOnly: Boolean;
  FacetParentNode: IXMLNode;
begin
  if not Assigned(FEnumerations) then
  begin
    FacetParentNode := GetFacetParentNode(False);
    { If this is a built in type, then we just use the datatype node as the
      host of the collection.  This allows us to get Enumerations.Count even if
      it's not valid to have any facets. }
    ReadOnly := not Assigned(FacetParentNode);
    if ReadOnly then
      FacetParentNode := Self;
    FEnumerations := (FacetParentNode as IXMLNodeAccess).CreateCollection(
      TXMLEnumerationCollection, IXMLEnumeration, xsfEnumeration) as IXMLEnumerationCollection;
    FEnumerations.ReadOnly := ReadOnly; 
  end;
  Result := FEnumerations;
end;

function TXMLTypeDef.GetFractionalDigits: Variant;
begin
  Result := ReadFacet(xsfFractionalDigits);
end;

function TXMLTypeDef.GetLength: Variant;
begin
  Result := ReadFacet(xsfLength);
end;

function TXMLTypeDef.GetMaxExclusive: Variant;
begin
  Result := ReadFacet(xsfMaxExclusive);
end;

function TXMLTypeDef.GetMaxInclusive: Variant;
begin
  Result := ReadFacet(xsfMaxInclusive);
end;

function TXMLTypeDef.GetMaxLength: Variant;
begin
  Result := ReadFacet(xsfMaxLength);
end;

function TXMLTypeDef.GetMinExclusive: Variant;
begin
  Result := ReadFacet(xsfMinExclusive);
end;

function TXMLTypeDef.GetMinInclusive: Variant;
begin
  Result := ReadFacet(xsfMinInclusive);
end;

function TXMLTypeDef.GetMinLength: Variant;
begin
  Result := ReadFacet(xsfMinLength);
end;

function TXMLTypeDef.GetNumeric: Variant;
begin
  Result := ReadFacet(xsfNumeric);
end;

function TXMLTypeDef.GetOrdered: Variant;
begin
  Result := ReadFacet(xsfOrdered);
end;

function TXMLTypeDef.GetPattern: Variant;
begin
  Result := ReadFacet(xsfPattern);
end;

function TXMLTypeDef.GetTotalDigits: Variant;
begin
  Result := ReadFacet(xsfTotalDigits);
end;

function TXMLTypeDef.GetWhitespace: Variant;
begin
  Result := ReadFacet(xsfWhitespace);
end;

function TXMLTypeDef.ReadFacet(const FacetName: string): Variant;
var
  FacetParent: IXMLNode;
  FacetNode: IXMLNode;
begin
  Result := Null;
  FacetParent := GetFacetParentNode(False);
  if Assigned(FacetParent) then
  begin
    FacetNode := FacetParent.ChildNodes.FindNode(FacetName);
    if Assigned(FacetNode) then
      Result := FacetNode.Attributes[SValue];
  end;
end;

procedure TXMLTypeDef.WriteFacet(const FacetName: string;
  const Value: Variant);
begin
  with GetFacetParentNode do
    if HasChildNode(FacetName) then
      ChildNodes[FacetName].Attributes[SValue] := Value else
      AddChild(FacetName).Attributes[SValue] := Value;
end;

procedure TXMLTypeDef.SetBounded(const Value: Variant);
begin
  WriteFacet(xsfBounded, Value);
end;

procedure TXMLTypeDef.SetCardinality(const Value: Variant);
begin
  WriteFacet(xsfCardinality, Value);
end;

procedure TXMLTypeDef.SetFractionalDigits(const Value: Variant);
begin
  WriteFacet(xsfFractionalDigits, Value);
end;

procedure TXMLTypeDef.SetLength(const Value: Variant);
begin
  WriteFacet(xsfLength, Value);
end;

procedure TXMLTypeDef.SetMaxExclusive(const Value: Variant);
begin
  WriteFacet(xsfMaxExclusive, Value);
end;

procedure TXMLTypeDef.SetMaxInclusive(const Value: Variant);
begin
  WriteFacet(xsfMaxInclusive, Value);
end;

procedure TXMLTypeDef.SetMaxLength(const Value: Variant);
begin
  WriteFacet(xsfMaxLength, Value);
end;

procedure TXMLTypeDef.SetMinExclusive(const Value: Variant);
begin
  WriteFacet(xsfMinExclusive, Value);
end;

procedure TXMLTypeDef.SetMinInclusive(const Value: Variant);
begin
  WriteFacet(xsfMinInclusive, Value);
end;

procedure TXMLTypeDef.SetMinLength(const Value: Variant);
begin
  WriteFacet(xsfMinLength, Value);
end;

procedure TXMLTypeDef.SetNumeric(const Value: Variant);
begin
  WriteFacet(xsfNumeric, Value);
end;

procedure TXMLTypeDef.SetOrdered(const Value: Variant);
begin
  WriteFacet(xsfOrdered, Value);
end;

procedure TXMLTypeDef.SetPattern(const Value: Variant);
begin
  WriteFacet(xsfPattern, Value);
end;

procedure TXMLTypeDef.SetTotalDigits(const Value: Variant);
begin
  WriteFacet(xsfTotalDigits, Variant(Value));
end;

procedure TXMLTypeDef.SetWhitespace(const Value: Variant);
begin
  WriteFacet(xsfWhitespace, Value);
end;

{ TXMLSimpleTypeRestriction }

procedure TXMLSimpleTypeRestriction.AfterConstruction;
begin
  RegisterChildNode(xsfEnumeration, TXMLEnumeration);
  inherited;
end;

function TXMLSimpleTypeRestriction.GetBaseName: DOMString;
begin
  Result := VarToWideStr(GetAttribute(SBase));
end;

procedure TXMLSimpleTypeRestriction.SetBaseName(const Value: DOMString);
begin
  SetAttribute(SBase, SchemaDef.PrefixedTypeName(Value));
end;

{ TXMLSimpleTypeList }

procedure TXMLSimpleTypeList.AfterConstruction;
begin
  RegisterChildNode(SSimpleType, TXMLSimpleTypeDef);
  inherited;
end;

function TXMLSimpleTypeList.GetSimpleTypeNode: IXMLSimpleTypeDef;
begin
  Result := ChildNodes.FindNode(IXMLSimpleTypeDef) as IXMLSimpleTypeDef;
end;

function TXMLSimpleTypeList.GetItemType: DOMString;
begin
  Result := VarToWideStr(GetAttribute(SItemType));
end;

procedure TXMLSimpleTypeList.SetItemType(const Value: DOMString);
begin
  SetAttribute(SItemType, SchemaDef.PrefixedTypeName(Value));
end;

{ TXMLSimpleTypeUnion }

procedure TXMLSimpleTypeUnion.AfterConstruction;
begin
  FSimpleTypes := CreateCollection(TXMLSimpleTypeDefs, IXMLSimpleTypeDef,
    SSimpleType) as IXMLSimpleTypeDefs;
  inherited;
end;

function TXMLSimpleTypeUnion.GetMemberTypes: DOMString;
begin
  Result := VarToWideStr(GetAttribute(SMemberTypes));
end;

procedure TXMLSimpleTypeUnion.SetMemberTypes(const Value: DOMString);
begin
  SetAttribute(SMemberTypes, SchemaDef.PrefixedTypeName(Value));
end;

function TXMLSimpleTypeUnion.GetSimpleTypes: IXMLSimpleTypeDefs;
begin
  Result := FSimpleTypes;
end;

{ TXMLSimpleTypeDef }

procedure TXMLSimpleTypeDef.AfterConstruction;
begin
  RegisterChildNode(SRestriction, TXMLSimpleTypeRestriction);
  RegisterChildNode(SList, TXMLSimpleTypeList);
  RegisterChildNode(SUnion, TXMLSimpleTypeUnion);
  RegisterChildNode(xsfEnumeration, TXMLEnumeration); { Legacy }
  inherited;
end;

function TXMLSimpleTypeDef.IsAnonymous: Boolean;
begin
  Result := not IsBuiltInType and inherited IsAnonymous;
end;

function TXMLSimpleTypeDef.IsBuiltInType: Boolean;
begin
  Result := Assigned(ParentNode) and (ParentNode is TXMLBuiltInTypes);
end;

function TXMLSimpleTypeDef.GetBaseType: IXMLTypeDef;
var
  RestNode: IXMLSimpleTypeRestriction;
begin
  Result := nil;
  if not Assigned(SchemaDef) then Exit;
  if not SchemaObject.IsLegacyFormat then
  begin
    RestNode := GetRestrictionNode;
    if Assigned(RestNode) and (RestNode.BaseName <> '') then
      Result := SchemaDef.SimpleTypes.Find(RestNode.BaseName);
  end
  else if HasAttribute(SBase) then
    Result := SchemaDef.SimpleTypes.Find(GetAttribute(SBase));
end;

procedure TXMLSimpleTypeDef.SetBaseTypeName(const Value: DOMString);
var
  RestNode: IXMLSimpleTypeRestriction;
begin
  if not SchemaObject.IsLegacyFormat then
  begin
    RestNode := GetRestrictionNode;
    if not Assigned(RestNode) then
      RestNode := AddChild(SRestriction) as IXMLSimpleTypeRestriction;
    RestNode.BaseName := SchemaDef.PrefixedTypeName(Value);
  end else
    SetAttribute(SBase, SchemaDef.PrefixedTypeName(Value));
end;

function TXMLSimpleTypeDef.GetFacetParentNode(const Required: Boolean=True): IXMLNode;
begin
  if Assigned(SchemaDef) and SchemaObject.IsLegacyFormat then
    Result := Self
  else if GetDerivationMethod = sdmList then
    Result := (GetContentNode as IXMLSimpleTypeList).SimpleTypeNode.GetFacetParentNode
  else
    Result := ChildNodes.FindNode(IXMLSimpleTypeRestriction);
  if Required and not Assigned(Result) then
    SchemaValidationError(SNoFacetsAllowed, [GetName]);
end;

function TXMLSimpleTypeDef.GetRestrictionNode: IXMLSimpleTypeRestriction;
begin
  Result := ChildNodes.FindNode(IXMLSimpleTypeRestriction) as IXMLSimpleTypeRestriction;
end;

function TXMLSimpleTypeDef.GetContentNode: IXMLSimpleContent;
begin
  Result := ChildNodes.FindNode(IXMLSimpleContent) as IXMLSimpleContent;
end;

function TXMLSimpleTypeDef.GetDerivationMethod: TSimpleDerivationMethod;
var
  ContentNode: IXMLSimpleContent;
begin
  Result := sdmNone;
  ContentNode := GetContentNode;
  if Assigned(ContentNode) then
  begin
    if Supports(ContentNode, IXMLSimpleTypeRestriction) then
      Result := sdmRestriction
    else if Supports(ContentNode, IXMLSimpleTypeList) then
      Result := sdmList
    else if Supports(ContentNode, IXMLSimpleTypeUnion) then
      Result := sdmUnion;
  end;
end;

function TXMLSimpleTypeDef.GetFinal: DOMString;
begin
  Result := VarToWideStr(GetAttribute(SFinal));
end;

procedure TXMLSimpleTypeDef.SetFinal(const Value: DOMString);
begin
  SetAttribute(SFinal, SchemaDef.PrefixedTypeName(Value));
end;

{ TXMLComplexContent }

procedure TXMLComplexContent.AfterConstruction;
begin
  RegisterChildNodes([SGroup, SAll, SChoice, SSequence, SAttribute, SAttributeGroup],
    [TXMLElementGroup, TXMLElementCompositor, TXMLElementCompositor,
     TXMLElementCompositor, TXMLAttributeDef, TXMLAttributeGroup]);
  FAttributeDefs := CreateCollection(TXMLAttributeDefs, IXMLAttributeDef,
    SAttribute) as IXMLAttributeDefs;
  FAttributeGroups := CreateCollection(TXMLAttributeGroups,
    IXMLAttributeGroup, SAttributeGroup) as IXMLAttributeGroups;
  inherited;
end;

function TXMLComplexContent.GetCompositorNode: IXMLElementCompositor;
const
  CompositorMap: array[TCompositorType] of TContentModel = (cmAll, cmChoice,
    cmSequence);
begin
  if not Assigned(FCompositorNode) then
  begin
    if SchemaObject.IsLegacyFormat then
    begin
      FContentModel := cmSequence;
      FCompositorNode := TXMLElementCompositor.CreateHosted(Self);
    end
    else
    begin
      FCompositorNode := ChildNodes.FindNode(IXMLElementCompositor) as IXMLElementCompositor;
      if Assigned(FCompositorNode) then
        FContentModel := CompositorMap[FCompositorNode.CompositorType]
      else
      begin
        FGroupRef := ChildNodes.FindNode(IXMLElementGroup) as IXMLElementGroup;
        if Assigned(FGroupRef) then
        begin
          FCompositorNode := FGroupRef.CompositorNode;
          FContentModel := cmGroupRef;
        end
        else
        begin
          { Create a dummy node so we don't AV on access of ElementDefs, etc }
          FContentModel := cmEmpty;
          FCompositorNode := TXMLElementCompositor.CreateHosted(Self);
        end;
      end;
    end;
  end;
  Result := FCompositorNode;
end;

function TXMLComplexContent.GetAttributeDefs: IXMLAttributeDefs;
begin
  Result := FAttributeDefs;
end;

function TXMLComplexContent.GetAttributeGroups: IXMLAttributeGroups;
begin
  Result := FAttributeGroups;
end;

function TXMLComplexContent.GetCompositors: IXMLElementCompositors;
begin
  Result := CompositorNode.GetCompositors;
end;

function TXMLComplexContent.GetElementDefs: IXMLElementDefs;
begin
  Result := CompositorNode.GetElementDefs;
end;

function TXMLComplexContent.GetElementGroups: IXMLElementGroups;
begin
  Result := CompositorNode.GetElementGroups;
end;

function TXMLComplexContent.GetContentModel: TContentModel;
begin
  if not Assigned(FCompositorNode) then
    GetCompositorNode;
  Result := FContentModel;
end;

procedure TXMLComplexContent.SetContentModel(const Value: TContentModel);
const
  CompositorNames: array[cmAll..cmSequence] of DOMString = (SAll, SChoice, SSequence);
begin
  if Value <> GetContentModel then
  begin
    if Assigned(FCompositorNode) then
    begin
      { Delete any existing compositor, but make sure it's not a dummy hosted node first }
      if not IsHosted(FCompositorNode) and Assigned(CompositorNode.ParentNode) then
        CompositorNode.ParentNode.ChildNodes.Remove(CompositorNode);
      FCompositorNode := nil;
    end;
    if Value <> cmEmpty then
      FCompositorNode := AddChild(CompositorNames[Value], GetNamespaceURI)
        as IXMLElementCompositor;
    FContentModel := Value;
  end;
end;


function TXMLComplexContent.GetGroupRef: IXMLElementGroup;
begin
  Result := FGroupRef;
end;

{ TXMLBaseTypeIndicator }

procedure TXMLBaseTypeIndicator.AfterConstruction;
begin
  RegisterChildNodes([SRestriction, SExtension],
    [TXMLComplexContent, TXMLComplexContent]);
  inherited;
end;

function TXMLBaseTypeIndicator.GetMixed: Variant;
begin
  if GetLocalName = SComplexContent then
    Result := GetAttribute(SMixed)
  else
    Result := Null;
end;

procedure TXMLBaseTypeIndicator.SetMixed(const Value: Variant);
begin
  if GetLocalName = SComplexContent then
    SetAttribute(SMixed, Value);
end;

{ TXMLComplexTypeDef }

procedure TXMLComplexTypeDef.AfterConstruction;
begin
  RegisterChildNodes([SGroup, SAll, SChoice, SSequence, SAttribute,
    SAttributeGroup], [TXMLElementGroup, TXMLElementCompositor,
    TXMLElementCompositor, TXMLElementCompositor,  TXMLAttributeDef,
    TXMLAttributeGroup]);
  if not SchemaObject.IsLegacyFormat then
    RegisterChildNodes([SComplexContent, SSimpleContent],
      [TXMLBaseTypeIndicator, TXMLBaseTypeIndicator])
  else
    RegisterChildNode(SElement, TXMLElementDef);
  inherited;
end;

procedure TXMLComplexTypeDef.CheckContent;

  function SetDerivationMeth(const Node: IXMLNode;
    DervMeth: TDerivationMethod): Boolean;
  begin
    Result := Assigned(Node);
    if Result then
    begin
      FDerivationMethod := DervMeth;
      FContentNode := Node as IXMLComplexContent;
    end;
  end;

  function FindDerivation(const DerivMeth: WideString;
    ExtType, RestType: TDerivationMethod): Boolean;
  begin
    Result := HasChildNode(DerivMeth, GetNamespaceURI);
    if Result then
      with ChildNodes.FindNode(DerivMeth, GetNamespaceURI).ChildNodes do
      begin
        if SetDerivationMeth(FindNode(SExtension, GetNamespaceURI), ExtType) then Exit;
        if SetDerivationMeth(FindNode(SRestriction, GetNamespaceURI), RestType) then Exit;
        SchemaParseError(SInvalidDerivation, [GetName]);
      end;
  end;

  function FindLegacyDerivation: TDerivationMethod;
  begin
    Result := dmNone;
    if HasAttribute(SBase) then
      if HasAttribute(SDerivedBy) and (GetAttribute(SDerivedBy) = SExtension) then
        Result := dmComplexExtension
      else
        Result := dmComplexRestriction;
  end;

begin
  if not SchemaObject.IsLegacyFormat then
  begin
    if FindDerivation(SComplexContent, dmComplexExtension, dmComplexRestriction) then Exit;
    if FindDerivation(SSimpleContent, dmSimpleExtension, dmSimpleRestriction) then Exit;
    SetDerivationMeth(TXMLComplexContent.CreateHosted(Self), dmNone);
  end
  else
    SetDerivationMeth(TXMLComplexContent.CreateHosted(Self), FindLegacyDerivation);
end;

procedure TXMLComplexTypeDef.AttributesChanged;
begin
  FAttributeDefList := nil;
end;

procedure TXMLComplexTypeDef.ChildElementsChanged;
begin
  FElementDefList := nil;
end;

function TXMLComplexTypeDef.GetAttributeDefs: IXMLAttributeDefs;
begin
  Result := ContentNode.GetAttributeDefs;
end;

function TXMLComplexTypeDef.GetAttributeGroups: IXMLAttributeGroups;
begin
  Result := ContentNode.GetAttributeGroups;
end;

function TXMLComplexTypeDef.GetAttributeDefList: IXMLAttributeDefList;

  procedure FindAttributeDefs(NodeList: IXMLNodeList);
  var
    I: Integer;
    AttributeDef: IXMLAttributeDef;
    AttributeGroup: IXMLAttributeGroup;
    Node: IXMLNode;
  begin
    for I := 0 to NodeList.Count - 1 do
    begin
      Node := NodeList[I];
      if Supports(Node, IXMLAttributeDef, AttributeDef) then
      begin
        FAttributeDefList.Add(AttributeDef);
      end
      else if Supports(Node, IXMLAttributeGroup, AttributeGroup) then
        FindAttributeDefs(AttributeGroup.Ref.ChildNodes);
    end;
  end;

begin
  if not Assigned(FAttributeDefList) then
  begin
    FAttributeDefList := TXMLAttributeDefList.Create;
    FindAttributeDefs(ContentNode.ChildNodes);
  end;
  Result := FAttributeDefList;
end;

function TXMLComplexTypeDef.GetElementDefList: IXMLElementDefList;

  procedure FindElementDefs(NodeList: IXMLNodeList);
  var
    I: Integer;
    Node: IXMLNode;
    ElementDef: IXMLElementDef;
    ElementGroup: IXMLElementGroup;
    ElementCompositor: IXMLElementCompositor;
  begin
    for I := 0 to NodeList.Count - 1 do
    begin
      Node := NodeList[I];
      if Supports(Node, IXMLElementDef, ElementDef) then
        FElementDefList.Add(ElementDef)
      else if Supports(Node, IXMLElementGroup, ElementGroup) then
      begin
        if Assigned(ElementGroup.Ref) then
          FindElementDefs(ElementGroup.Ref.ChildNodes)
        else
          SchemaParseError(SInvalidGroupDecl, [GetName]);
      end
      else if Supports(Node, IXMLElementCompositor, ElementCompositor) then
      begin
        FindElementDefs(ElementCompositor.ChildNodes)
      end
      else
        FindElementDefs(Node.GetChildNodes);
    end;
  end;

begin
  if not Assigned(FElementDefList) then
  begin
    FElementDefList := TXMLElementDefList.Create;
    if GetContentModel <> cmEmpty then
      FindElementDefs(ContentNode.ChildNodes);
  end;
  Result := FElementDefList;
end;

procedure TXMLComplexTypeDef.SetBaseTypeName(const Value: DOMString);
begin
  ContentNode.SetAttribute(SBase, SchemaDef.PrefixedTypeName(Value));
end;

function TXMLComplexTypeDef.GetCompositors: IXMLElementCompositors;
begin
  Result := ContentNode.GetCompositors;
end;

function TXMLComplexTypeDef.GetElementDefs: IXMLElementDefs;
begin
  Result := ContentNode.GetElementDefs;
end;

function TXMLComplexTypeDef.GetElementGroups: IXMLElementGroups;
begin
  Result := ContentNode.GetElementGroups;
end;

function TXMLComplexTypeDef.IsComplex: Boolean;
begin
  Result := True;
end;

function TXMLComplexTypeDef.GetBaseType: IXMLTypeDef;
var
  BaseName: DOMString;
begin
  Result := nil;
  if not SchemaObject.IsLegacyFormat then
  begin
    if FDerivationMethod <> dmNone then
    begin
      BaseName := VarToStr(ContentNode.GetAttribute(SBase));
      if (BaseName <> '') then
        if FDerivationMethod in [dmComplexExtension, dmComplexRestriction] then
          Result := SchemaDef.ComplexTypes.Find(BaseName) else
          Result := SchemaDef.SimpleTypes.Find(BaseName);
    end;
  end
  else if HasAttribute(SBase) then
    Result := SchemaDef.ComplexTypes.Find(GetAttribute(SBase));
end;

function TXMLComplexTypeDef.GetContentModel: TContentModel;
begin
  Result := ContentNode.GetContentModel;
end;

procedure TXMLComplexTypeDef.SetContentModel(const Value: TContentModel);
begin
  if Value = cmGroupRef then
    SchemaValidationError(SSetGroupRefProp, []);
  ContentNode.SetContentModel(Value);
end;

function TXMLComplexTypeDef.GetCompositorNode: IXMLElementCompositor;
begin
  ContentNode.GetCompositorNode;
end;

function TXMLComplexTypeDef.GetContentNode: IXMLComplexContent;
begin
  if not Assigned(FContentNode) then
    CheckContent;
  Result := FContentNode;
end;

function TXMLComplexTypeDef.GetDerivationMethod: TDerivationMethod;
begin
  if not Assigned(FContentNode) then
    CheckContent;
  Result := FDerivationMethod;
end;

procedure TXMLComplexTypeDef.SetDerivationMethod(const Value: TDerivationMethod);
const
  BaseTypeIndMap: array[TDerivationMethod] of DOMString =
    ('', SComplexContent, SComplexContent, SSimpleContent, SSimpleContent);
  ExtRestTypeMap: array[TDerivationMethod] of DOMString =
    ('', SExtension, SRestriction, SExtension, SRestriction);
var
  I: Integer;
  WasHosted: Boolean;
  BaseTypeIndicatorNode: IXMLNode;
  OldAttributes: IXMLAttributeDefs;
begin
  OldAttributes := nil;
  WasHosted := False;
  if Value <> GetDerivationMethod then
  begin
    { If there are existing AttributesDefs, we need to move them }
    if GetAttributeDefs.Count > 0 then
    begin
      OldAttributes := GetAttributeDefs;
      { Here we touch the attributes datatype to cache it, otherwise we can't read them later }
      for I := 0 to OldAttributes.Count - 1 do
        OldAttributes[I].DataTypeName;
    end;
    { Delete any existing "complexContent" or "simpleContent" node }
    if not IsHosted(FContentNode) then
    begin
      WasHosted := True;
      { Delete the existing "simpleContent" or "complexContent" node }
      BaseTypeIndicatorNode := ChildNodes.FindNode(IXMLBaseTypeIndicator);
      if Assigned(BaseTypeIndicatorNode) then
        ChildNodes.Remove(BaseTypeIndicatorNode);
      FContentNode := nil;
    end;
    if Value <> dmNone then
    begin
      if not SchemaObject.IsLegacyFormat then
      begin
        BaseTypeIndicatorNode := AddChild(BaseTypeIndMap[Value]);
        FContentNode := BaseTypeIndicatorNode.AddChild(ExtRestTypeMap[Value]) as IXMLComplexContent;
      end
      else
        SetAttribute(SDerivedBy, ExtRestTypeMap[Value]);
    end;
    if FContentNode = nil then
      CheckContent;
    { Copy the attributes over now }
    if Assigned(OldAttributes) then
    begin
      for I := 0 to OldAttributes.Count - 1 do
        FContentNode.GetAttributeDefs.Add(OldAttributes[I].Name, OldAttributes[I].DataTypeName);
      { When the AttributeDefs are parented directly to the typedef, we will need to remove them }
      if not WasHosted then
        OldAttributes.Clear;
    end;
    FDerivationMethod := Value;
  end;
end;

function TXMLComplexTypeDef.GetGroupRef: IXMLElementGroup;
begin
  Result := ContentNode.GetGroupRef;
end;

function TXMLComplexTypeDef.GetAbstract: Boolean;
var
  AttrVal: OleVariant;
begin
  AttrVal := GetAttribute(SAbstract);
  Result := (not VarIsNull(AttrVal)) and AttrVal;
end;

procedure TXMLComplexTypeDef.SetAbstract(const Value: Boolean);
begin
  SetAttribute(SAbstract, Variant(Value));
end;

function TXMLComplexTypeDef.GetFacetParentNode(const Required: Boolean=True): IXMLNode;
begin
  if GetDerivationMethod in [dmSimpleRestriction, dmComplexRestriction] then
    Result := ContentNode;
  if Required and not Assigned(Result) then
    SchemaValidationError(SNoFacetsAllowed, [GetName]);
end;

function TXMLComplexTypeDef.GetBaseTypeIndicator: IXMLBaseTypeIndicator;
begin
  Result := ChildNodes.FindNode(IXMLBaseTypeIndicator) as IXMLBaseTypeIndicator;
end;

{ TXMLTypedSchemaItem }

procedure TXMLTypedSchemaItem.RemoveTypeInfo;
var
  ExistingType: IXMLTypeDef;
begin
  if ChildNodes.Count > 0 then
  begin
    ExistingType := GetDataType;
    if Assigned(ExistingType) and ExistingType.IsAnonymous then
      ChildNodes.Remove(ExistingType)
  end
  else if HasAttribute(SType) then
    SetAttribute(SType, Null);
  FTypeDef := nil;
end;

procedure TXMLTypedSchemaItem.SetRefItem(const Value: IXMLSchemaRefItem);
begin
  inherited;
  RemoveTypeInfo;
end;

function TXMLTypedSchemaItem.GetDataType: IXMLTypeDef;
var
  TypeNS,
  TypeName: DOMString;
begin
  if Assigned(RefItem) then
    Result := (GetRefItem as IXMLTypedSchemaItem).DataType
  else
  begin
    if not Assigned(FTypeDef) then
    begin
      TypeName := VarToStr(GetAttribute(SType));
      if TypeName = '' then
      begin
        { Check for an anonymous type definition }
        FTypeDef := ChildNodes.FindNode(IXMLTypeDef) as IXMLTypeDef;
        if not Assigned(FTypeDef) then
        begin
          { If no anonymous type, then default to string }
          FTypeDef := SchemaDef.SimpleTypes.Find(SchemaDef.PrefixedTypeName(xsdString));
          Assert(Assigned(FTypeDef));
        end;
      end
      else
      begin
        { Otherwise look for the type in the global collections }
        if IsPrefixed(TypeName) then
          TypeNS := FindNamespaceURI(TypeName) else
          TypeNS := VarToStr(SchemaDef.TargetNamespace);
        FTypeDef := FindItemNS(((SchemaDef.ComplexTypes as IXMLNodeAccess).GetNodeObject as TXMLSchemaItems), TypeName, TypeNS) as IXMLTypeDef;
        if not Assigned(FTypeDef) then
        begin
          FTypeDef := FindItemNS(((SchemaDef.SimpleTypes as IXMLNodeAccess).GetNodeObject as TXMLSchemaItems), TypeName, TypeNS) as IXMLTypeDef;
          if not Assigned(FTypeDef) then
          begin
            FTypeDef := SchemaDef.SimpleTypes.Find(TypeName);
            if not Assigned(FTypeDef) then
              SchemaParseError(SUnknownDataType, [TypeName]);
          end;
        end;
      end;
    end;
    Result := FTypeDef;
  end;
end;


function TXMLTypedSchemaItem.GetDataTypeName: DOMString;
begin
  Result := GetDataType.Name;
end;

procedure TXMLTypedSchemaItem.SetDataType(const Value: IXMLTypeDef);
begin
  if Assigned(Value) and (Value.Name <> '') then
    SetDataTypeName(Value.Name);
end;

procedure TXMLTypedSchemaItem.SetDataTypeName(const Value: DOMString);
begin
  if Assigned(RefItem) then
    SchemaValidationError(SNoRefPropSet, ['DataType']); { Do not localize }
  RemoveTypeInfo;
  SetAttribute(SType, SchemaDef.PrefixedTypeName(Value));
end;

{ TXMLAttributeDef }

procedure TXMLAttributeDef.AfterConstruction;
begin
  RegisterChildNode(SSimpleType, TXMLSimpleTypeDef);
  inherited;
end;

function TXMLAttributeDef.GlobalCollection: IXMLSchemaItems;
begin
  if not IsGlobal and (SchemaObject <> nil) then
    Result := SchemaObject.GetAttributeDefs;
end;

function TXMLAttributeDef.GetRef: IXMLAttributeDef;
begin
  Result := GetRefItem as IXMLAttributeDef;
end;

procedure TXMLAttributeDef.SetRef(const Value: IXMLAttributeDef);
begin
  SetRefItem(Value);
end;

function TXMLAttributeDef.GetUse: Variant;
begin
  Result := GetAttribute(SUse);
end;

procedure TXMLAttributeDef.SetUse(const Value: Variant);
begin
  SetAttribute(SUse, Value);
end;

function TXMLAttributeDef.GetDefault: Variant;
begin
  Result := GetAttribute(SDefault);
end;

procedure TXMLAttributeDef.SetDefault(const Value: Variant);
begin
  if Value <> '' then
    SetAttribute(SUse, Null);
  SetAttribute(SDefault, Value);
end;

function TXMLAttributeDef.GetFixed: Variant;
begin
  Result := GetAttribute(SFixed);
end;

procedure TXMLAttributeDef.SetFixed(const Value: Variant);
begin
  if Value <> '' then
    SetAttribute(SUse, Null);
  SetAttribute(SFixed, Value);
end;

{ TXMLElementDef }

procedure TXMLElementDef.AfterConstruction;
begin
  RegisterChildNodes([SSimpleType, SComplextype],
    [TXMLSimpleTypeDef, TXMLComplexTypeDef]);
  inherited;
end;

function TXMLElementDef.GlobalCollection: IXMLSchemaItems;
begin
  if not IsGlobal and (SchemaObject <> nil) then
    Result := SchemaObject.GetElementDefs;
end;

function TXMLElementDef.GetAttributeDefList: IXMLAttributeDefList;
begin
  if DataType.IsComplex then
    Result := (DataType as IXMLComplexTypeDef).AttributeDefList else
    Result := TXMLAttributeDefList.Create; { Return empty list to avoid AV on use }
end;

function TXMLElementDef.GetChildElementList: IXMLElementDefList;
begin
  if DataType.IsComplex then
    Result := (DataType as IXMLComplexTypeDef).ElementDefList else
    Result := TXMLElementDefList.Create; { Return empty list to avoid AV on use }
end;

function TXMLElementDef.IsRepeating: Boolean;

  function ParentRepeats(const Node: IXMLNode): Boolean;
  var
    CompositorNode: IXMLElementCompositor;
  begin
    if Assigned(Node) and Supports(Node, IXMLElementCompositor, CompositorNode) then
    begin
      Result := not (string(CompositorNode.GetMaxOccurs)[1] in ['0', '1']);
      if not Result then
        Result := ParentRepeats(CompositorNode.ParentNode);
    end
    else
      Result := False;
  end;

begin
  Result := not (string(GetMaxOccurs)[1] in ['0', '1']);
  if not Result then
    Result := ParentRepeats(ParentNode);
end;

function TXMLElementDef.GetMaxOccurs: Variant;
begin
  Result := GetAttribute(SMaxOccurs);
  if VarIsNull(Result) then
    Result := DOMString('1');
end;

procedure TXMLElementDef.SetMaxOccurs(const Value: Variant);
begin
  SetAttribute(SMaxOccurs, Value);
end;

function TXMLElementDef.GetMinOccurs: Variant;
begin
  Result := GetAttribute(SMinOccurs);
  if VarIsNull(Result) then
    Result := DOMString('1');
end;

procedure TXMLElementDef.SetMinOccurs(const Value: Variant);
begin
  SetAttribute(SMinOccurs, Value);
end;

function TXMLElementDef.GetRef: IXMLElementDef;
begin
  Result := GetRefItem as IXMLElementDef;
end;

procedure TXMLElementDef.SetRef(const Value: IXMLElementDef);
begin
  SetRefItem(Value);
end;

{ TXMLElementCompositor }

procedure TXMLElementCompositor.AfterConstruction;
begin
  RegisterChildNodes([SElement, SGroup, SAll, SChoice, SSequence],
    [TXMLElementDef, TXMLElementGroup, TXMLElementCompositor,
     TXMLElementCompositor, TXMLElementCompositor]);
  if GetLocalName = SAll then
    FCompositorType := ctAll
  else if GetLocalName = SChoice then
    FCompositorType := ctChoice
  else
    FCompositorType := ctSequence;
  FCompositors := CreateCollection(TXMLElementCompositors,
    IXMLElementCompositor, SSequence) as IXMLElementCompositors;
  FElementDefs := CreateCollection(TXMLElementDefs, IXMLElementDef,
    SElement) as IXMLElementDefs;
  FElementGroups := CreateCollection(TXMLElementGroups, IXMLElementGroup,
    SGroup) as IXMLElementGroups;
  inherited;
end;

function TXMLElementCompositor.GetCompositors: IXMLElementCompositors;
begin
  Result := FCompositors;
end;

function TXMLElementCompositor.GetCompositorType: TCompositorType;
begin
  Result := FCompositorType;
end;

function TXMLElementCompositor.GetElementDefs: IXMLElementDefs;
begin
  Result := FElementDefs;
end;

function TXMLElementCompositor.GetElementGroups: IXMLElementGroups;
begin
  Result := FElementGroups;
end;

function TXMLElementCompositor.GetMaxOccurs: Variant;
begin
  Result := GetAttribute(SMaxOccurs);
  if VarIsNull(Result) then
    Result := DOMString('1');
end;

procedure TXMLElementCompositor.SetMaxOccurs(const Value: Variant);
begin
  SetAttribute(SMaxOccurs, Value);
end;

function TXMLElementCompositor.GetMinOccurs: Variant;
begin
  Result := GetAttribute(SMinOccurs);
  if VarIsNull(Result) then
    Result := DOMString('1');
end;

procedure TXMLElementCompositor.SetMinOccurs(const Value: Variant);
begin
  SetAttribute(SMinOccurs, Value);
end;

{ TXMLAttributeGroup }

procedure TXMLAttributeGroup.AfterConstruction;
begin
  RegisterChildNodes([SAttribute, SAttributeGroup],
    [TXMLAttributeDef, TXMLAttributeGroup]);
  inherited;
end;

function TXMLAttributeGroup.GetAttributeDefs: IXMLAttributeDefs;
begin
  if Assigned(RefItem) then
    Result := GetRef.AttributeDefs
  else
  begin
    if not Assigned(FAttributeDefs) then
      FAttributeDefs := CreateCollection(TXMLAttributeDefs, IXMLAttributeDef,
        SAttribute) as IXMLAttributeDefs;
    Result := FAttributeDefs;
  end;
end;

function TXMLAttributeGroup.GetAttributeGroups: IXMLAttributeGroups;
begin
  if Assigned(RefItem) then
    Result := GetRef.AttributeGroups
  else
  begin
    if not assigned(FAttributeGroups) then
      FAttributeGroups := CreateCollection(TXMLAttributeGroups,
        IXMLAttributeGroup, SAttributeGroup) as IXMLAttributeGroups;
    Result := FAttributeGroups;
  end;
end;

function TXMLAttributeGroup.GlobalCollection: IXMLSchemaItems;
begin
  if not IsGlobal and (SchemaObject <> nil) then
    Result := SchemaObject.GetAttributeGroups;
end;

function TXMLAttributeGroup.GetRef: IXMLAttributeGroup;
begin
  Result := GetRefItem as IXMLAttributeGroup;
end;

procedure TXMLAttributeGroup.SetRef(const Value: IXMLAttributeGroup);
begin
  SetRefItem(Value);
end;

{ TXMLElementGroup }

procedure TXMLElementGroup.AfterConstruction;
begin
  RegisterChildNodes([SAll, SChoice, SSequence],
    [TXMLElementCompositor, TXMLElementCompositor, TXMLElementCompositor]);
  if SchemaObject.IsLegacyFormat then
    RegisterChildNodes([SElement, SGroup], [TXMLElementDef, TXMLElementGroup]);
  inherited;
end;

function TXMLElementGroup.GetCompositorNode: IXMLElementCompositor;
var
  GroupNode: IXMLNode;
begin
  if not Assigned(FContentNode) then
  begin
    if not SchemaObject.IsLegacyFormat then
    begin
      if Assigned(RefItem) then
        GroupNode := RefItem
      else
        GroupNode := Self;
      FContentNode := GroupNode.ChildNodes.FindNode(IXMLElementCompositor)
        as IXMLElementCompositor;
    end
    else
      FContentNode := TXMLElementCompositor.CreateHosted(Self)
  end;
  Result := FContentNode;
end;

function TXMLElementGroup.GetElementCompositors: IXMLElementCompositors;
begin
  Result := ContentNode.Compositors;
end;

function TXMLElementGroup.GetElementDefs: IXMLElementDefs;
begin
  Result := ContentNode.ElementDefs;
end;

function TXMLElementGroup.GetElementGroups: IXMLElementGroups;
begin
  Result := ContentNode.ElementGroups;
end;

function TXMLElementGroup.GlobalCollection: IXMLSchemaItems;
begin
  if not IsGlobal and (SchemaObject <> nil) then
    Result := SchemaObject.GetElementGroups;
end;

function TXMLElementGroup.GetRef: IXMLElementGroup;
begin
  Result := GetRefItem as IXMLElementGroup;
end;

procedure TXMLElementGroup.SetRef(const Value: IXMLElementGroup);
begin
  SetRefItem(Value);
end;

{ TXMLNotationDef }

function TXMLNotationDef.GetPublicID: Variant;
begin
  Result := GetAttribute(SPublic);
end;

function TXMLNotationDef.GetSystemID: Variant;
begin
  Result := GetAttribute(SSystem);
end;

procedure TXMLNotationDef.SetPublicID(const Value: Variant);
begin
  SetAttribute(SPublic, Value);
end;

procedure TXMLNotationDef.SetSystemID(const Value: Variant);
begin
  SetAttribute(SSystem, Value);
end;

{ TXMLAnnotation }

procedure TXMLAnnotation.AfterConstruction;
begin
  RegisterChildNodes([SDocumentation, SAppInfo],
    [TXMLAnnotationItem, TXMLAnnotationItem]);
  inherited;
end;

function TXMLAnnotation.CreateChildNode(const ADOMNode: IDOMNode): IXMLNode;
var
  Element: IDOMElement;
  NodeClass: TXMLNodeClass;
begin
  if ADOMNode.NodeType = ELEMENT_NODE then
  begin
    Element := ADOMNode as IDOMElement;
    if (Length(SchemaObject.FAppInfoClasses) > 0) and (Element.attributes.length > 0)
       and (Element.attributes[0].namespaceURI <> '') then
    begin
      NodeClass := SchemaObject.FindAppInfoClass(Element.attributes[0].namespaceURI);
      if Assigned(NodeClass) then
      begin
        Result := NodeClass.Create(ADOMNode, Self, OwnerDocument);
        Exit;
      end;
    end;
  end;
  Result := inherited CreateChildNode(ADOMNode);
end;

function TXMLAnnotation.HasAppInfo: Boolean;
begin
  Result := HasChildNode(SAppInfo);
end;

function TXMLAnnotation.HasDocumentation: Boolean;
begin
  Result := HasChildNode(SDocumentation);
end;

function TXMLAnnotation.GetAppInfoCollection: IXMLAppInfoCollection;
begin
  if not Assigned(FAppInfo) then
    FAppInfo := CreateCollection(TXMLAppInfoCollection, IXMLAppInfo,
      SAppInfo) as IXMLAppInfoCollection;
  Result := FAppInfo;
end;

function TXMLAnnotation.GetDocumentationCollection: IXMLDocumentationCollection;
begin
  if not Assigned(FDocumentation) then
    FDocumentation := CreateCollection(TXMLDocumentationCollection,
      IXMLDocumentation, SDocumentation) as IXMLDocumentationCollection;
  Result := FDocumentation;
end;

{ TXMLAnnotationItem }

function TXMLAnnotationItem.GetSource: Variant;
begin
  Result := GetAttribute(SSource);
end;

procedure TXMLAnnotationItem.SetSource(const Value: Variant);
begin
  SetAttribute(SSource, Value);
end;

{ TXMLEnumeration }

function TXMLEnumeration.GetValue: Variant;
begin
  Result := GetAttribute(SValue);
end;

procedure TXMLEnumeration.SetValue(const Value: Variant);
begin
  SetAttribute(SValue, Value);
end;

{ *** Collections *** }

{ TXMLSchemaDocRefs }

function TXMLSchemaDocRefs.GetSchemaDocRef(Index: Integer): IXMLSchemaDocRef;
begin
  Result := List[Index] as IXMLSchemaDocRef;
end;

{ TXMLSchemaImports }

function TXMLSchemaImports.Add(const SchemaLocation: DOMString): IXMLSchemaImport;
begin
  Result := AddItem(-1) as IXMLSchemaImport;
  Result.SchemaLocation := SchemaLocation;
end;

function TXMLSchemaImports.GetItem(Index: Integer): IXMLSchemaImport;
begin
  Result := List[Index] as IXMLSchemaImport;
end;

function TXMLSchemaImports.Insert(Index: Integer): IXMLSchemaImport;
begin
  Result := AddItem(Index) as IXMLSchemaImport;
end;

{ TXMLSchemaIncludes }

function TXMLSchemaIncludes.Add(const SchemaLocation: DOMString): IXMLSchemaInclude;
begin
  Result := AddItem(-1) as IXMLSchemaInclude;
  Result.SchemaLocation := SchemaLocation;
end;

function TXMLSchemaIncludes.GetItem(Index: Integer): IXMLSchemaInclude;
begin
  Result := List[Index] as IXMLSchemaInclude;
end;

function TXMLSchemaIncludes.Insert(Index: Integer): IXMLSchemaInclude;
begin
  Result := AddItem(Index) as IXMLSchemaInclude;
end;

{ TXMLSchemaItems }

procedure TXMLSchemaItems.AfterConstruction;
begin
  FIsGlobal := GetLocalName = SSchema;
  FCollectionClassType := TXMLNodeCollectionClass(ClassType);
  inherited;
end;

function TXMLSchemaItems.GetSchemaDef: IXMLSchemaDef;
var
  Node: IXMLNode;
  SchemaDef: IXMLSchemaDef;
begin
  if not Assigned(FSchemaDef) then
  begin
    Node := HostNode;
    while Assigned(Node) and not Supports(Node, IXMLSchemaDef, SchemaDef) do
      Node := Node.ParentNode;
    if Assigned(SchemaDef) then
      SetSchemaDef(SchemaDef);
  end;
  Result := FSchemaDef as IXMLSchemaDef;
end;

procedure TXMLSchemaItems.SetSchemaDef(const Value: IXMLSchemaDef);
begin
  FSchemaDef := (Value as IXMLNodeAccess).GetNodeObject;
end;

function FindItemNS(SchemaItems: TXMLSchemaItems; const Name, ItemNS: DOMString): IXMLSchemaItem;

  function SearchLinkedSchemas(const SchemaDocRefs: IXMLSchemaDocRefs;
    const ItemNS: DOMString): IXMLSchemaItem;
  var
    I: Integer;
    SchemaObj: IXMLNodeAccess;
    ImportCollection: IXMLSchemaItems;
  begin
    for I := 0 to SchemaDocRefs.Count - 1 do
      if Assigned(SchemaDocRefs[I].SchemaRef) and
         SameNamespace(VarToWideStr(SchemaDocRefs[I].SchemaRef.TargetNamespace), ItemNS) then
      begin
        SchemaObj := (SchemaDocRefs[I].SchemaRef as IXMLNodeAccess);
        ImportCollection := SchemaObj.FindHostedNode(SchemaItems.FCollectionClassType) as IXMLSchemaItems;
        Result := ImportCollection.FindItem(Name);
        if Assigned(Result) then Break;
      end;
  end;

  function SearchSiblingSchemas(const ParentNode: IXMLNode;
    const ItemNS: DOMString): IXMLSchemaItem;
  var
    I: Integer;
    SchemaObj: IXMLNodeAccess;
    SchemaDef: IXMLSchemaDef;
    ImportCollection: IXMLSchemaItems;
  begin
    for I := 0 to ParentNode.ChildNodes.Count - 1 do
      if Supports(ParentNode.ChildNodes[I], IXMLSchemaDef, SchemaDef) and
         (SchemaDef <> SchemaItems.SchemaDef) and
        SameNamespace(VarToWideStr(SchemaDef.TargetNamespace), ItemNS) then
      begin
        SchemaObj := (SchemaDef as IXMLNodeAccess);
        ImportCollection := SchemaObj.FindHostedNode(SchemaItems.FCollectionClassType) as IXMLSchemaItems;
        Result := ImportCollection.FindItem(ExtractLocalName(Name));
        if Assigned(Result) then Break;
      end;
  end;

var
  Index: Integer;
begin
  Result := nil;
  Index := SchemaItems.IndexOfItem(Name);
  if Index >= 0 then
    Result := SchemaItems.List[Index] as IXMLSchemaItem
  else if SchemaItems.IsGlobal then
  begin
    Result := SearchLinkedSchemas(SchemaItems.SchemaDef.SchemaIncludes, ItemNS);
    if not Assigned(Result) then
    begin
      Result := SearchLinkedSchemas(SchemaItems.SchemaDef.SchemaImports, ItemNS);
      if not Assigned(Result) and (SchemaItems.SchemaDef.ParentNode.NodeType = ntElement) then
        Result := SearchSiblingSchemas(SchemaItems.SchemaDef.ParentNode, ItemNS);
    end;
  end;
end;

function TXMLSchemaItems.FindItem(const Name: DOMString): IXMLSchemaItem;
var
  ItemNS: DOMString;
begin
  if IsPrefixed(Name) then
    ItemNS := SchemaDef.FindNamespaceURI(Name)
  else
    ItemNS := VarToStr(SchemaDef.TargetNamespace);
  Result := FindItemNS(Self, Name, ItemNS);
end;

function TXMLSchemaItems.GetSchemaItem(Index: Integer): IXMLSchemaItem;
begin
  Result := List[Index] as IXMLSchemaItem;
end;

function TXMLSchemaItems.IndexOfItem(const Name: DOMString): Integer;
var
  ItemNS: DOMString;
  LocalName: DOMString;
begin
  if IsPrefixed(Name) then
  begin
    ItemNS := SchemaDef.FindNamespaceURI(Name);
    if SameNamespace(ItemNS, VarToStr(SchemaDef.TargetNamespace)) then
      LocalName := ExtractLocalName(Name)
    else
    begin
      Result := -1;
      Exit;
    end;
  end
  else
    LocalName := Name;
  for Result := 0 to Count - 1 do
    if (List[Result] as IXMLSchemaItem).Name = LocalName then Exit;
  Result := -1;
end;

function TXMLSchemaItems.AddItem(Index: Integer): IXMLNode;
begin
  CheckParent;
  Result := inherited AddItem(Index);
end;

procedure TXMLSchemaItems.CheckParent;
begin
  if not Assigned(ParentNode) then
    SchemaValidationError(SNoContentModel, []);
end;

{ TXMLSimpleTypeDefs }

function TXMLSimpleTypeDefs.Add(const Name: DOMString; BaseTypeName: DOMString = ''): IXMLSimpleTypeDef;
begin
  Result := AddItem(-1) as IXMLSimpleTypeDef;
  Result.Name := Name;
  if BaseTypeName <> '' then
    Result.SetBaseTypeName(BaseTypeName);
end;

function TXMLSimpleTypeDefs.Find(const Name: DOMString): IXMLSimpleTypeDef;
begin
  Result := FindItem(Name) as IXMLSimpleTypeDef;
  if not Assigned(Result) then
    Result := SchemaDef.BuiltInTypes.Find(Name);
end;

function TXMLSimpleTypeDefs.GetItem(Index: Integer): IXMLSimpleTypeDef;
begin
  Result := List[Index] as IXMLSimpleTypeDef; { No Refs }
end;

function TXMLSimpleTypeDefs.Insert(Index: Integer): IXMLSimpleTypeDef;
begin
  Result := AddItem(Index) as IXMLSimpleTypeDef;
end;

{ TXMLBuiltInTypes }

procedure TXMLBuiltInTypes.AfterConstruction;
begin
  RegisterChildNode(SSimpleType, TXMLSimpleTypeDef);
  inherited;
end;

function TXMLBuiltInTypes.Add(const Name: DOMString; BaseTypeName: DOMString): IXMLSimpleTypeDef;
begin
  if not IsBuiltInTypeName(Name) then
    SchemaValidationError(SNotBuiltInType, [Name]);
  Result := AddChild(SSimpleType) as IXMLSimpleTypeDef;
  Result.Name := Name;
  { BaseTypeName is ignored }
end;

function TXMLBuiltInTypes.IndexOfItem(const Name: DOMString): Integer;
var
  TypeDef: IXMLSimpleTypeDef;
  Namespace, LocalName: DOMString;
begin
  if not IsPrefixed(Name) then
    Namespace := SchemaDef.NamespaceURI
  else
  begin
    Namespace := SchemaDef.FindNamespaceURI(Name);
    LocalName := ExtractLocalName(Name);
  end;
  for Result := 0 to Count - 1 do
  begin
    TypeDef := List[Result] as IXMLSimpleTypeDef;
    if (TypeDef.Name = LocalName) and
      SameNamespace(TypeDef.DOMNode, Namespace) then Exit;
  end;
  Result := -1;
end;

function TXMLBuiltInTypes.IsBuiltInTypeName(const Name: DOMString): Boolean;

  function IsSchemaNamespace(const Namespace: DOMString): Boolean;
  begin
    Result := SameNamespace(SchemaDef.DOMNode, Namespace) or
              SameNamespace(Namespace, SXMLSchemaURI_1999) or
              SameNamespace(Namespace, SXMLSchemaURI_2000_10) or
              SameNamespace(Namespace, SXMLSchemaURI_2001);
  end;

var
  LocalName: DOMString;
begin
  if not IsPrefixed(Name) or IsSchemaNamespace(SchemaDef.FindNamespaceURI(Name)) then
  begin
    LocalName := ExtractLocalName(Name);
    Result := ArrayContains(LocalName, BuiltInTypeNames) or (LocalName = 'ur-type') or
      (not SchemaDef.StrictFormat and ArrayContains(LocalName, LegacyTypeNames));
  end else
    Result := False;
end;

function TXMLBuiltInTypes.Find(const Name: DOMString): IXMLSimpleTypeDef;
var
  Index: Integer;
begin
  Index := IndexOfItem(Name);
  if Index >= 0 then
    Result := List[Index] as IXMLSimpleTypeDef
  else if IsBuiltInTypeName(Name) then
  begin
    if IsPrefixed(Name) then
    begin
      { This is basically a hack to support documents which use built in types
        from different XML Schema namespaces (other than the one associated with
        this document). }
      Result := AddChild(SSimpleType, SchemaDef.FindNamespaceURI(Name), TXMLSimpleTypeDef) as IXMLSimpleTypeDef;
      Result.Name := ExtractLocalName(Name);
    end else
      Result := Add(Name);
  end;
end;

{ TXMLComplexTypeDefs }

function TXMLComplexTypeDefs.Add(const Name: DOMString;
  ContentModel: TContentModel): IXMLComplexTypeDef;
begin
  Result := AddItem(-1) as IXMLComplexTypeDef;
  Result.Name := Name;
  Result.ContentModel := ContentModel;
end;

function TXMLComplexTypeDefs.Add(const Name, BaseTypeName: DOMString;
  DerivationMethod: TDerivationMethod;
  ContentModel: TContentModel): IXMLComplexTypeDef;
begin
  Result := AddItem(-1) as IXMLComplexTypeDef;
  Result.Name := Name;
  Result.DerivationMethod := DerivationMethod;
  Result.BaseTypeName := BaseTypeName;
  if ContentModel <> cmEmpty then
    Result.ContentModel := ContentModel;
end;

function TXMLComplexTypeDefs.Find(const Name: DOMString): IXMLComplexTypeDef;
begin
  Result := FindItem(Name) as IXMLComplexTypeDef;
end;

function TXMLComplexTypeDefs.GetItem(Index: Integer): IXMLComplexTypeDef;
begin
  Result := GetSchemaItem(Index) as IXMLComplexTypeDef;
end;

function TXMLComplexTypeDefs.Insert(Index: Integer): IXMLComplexTypeDef;
begin
  Result := AddItem(Index) as IXMLComplexTypeDef;
end;

{ TXMLAttributeDefs }

procedure TXMLAttributeDefs.ChildListNotify(Operation: TNodeListOperation;
  var Node: IXMLNode; const IndexOrName: OleVariant; BeforeOperation: Boolean);
var
  TypeNode: TXMLNode;
begin
  inherited;
  if BeforeOperation then
  begin
    TypeNode := HostNode;
    while Assigned(TypeNode) and not (TypeNode is TXMLComplexTypeDef) do
      TypeNode := TXMLSchemaItem(HostNode).HostNode;
    if Assigned(TypeNode) then
      TXMLComplexTypeDef(TypeNode).AttributesChanged;
  end;
end;

function TXMLAttributeDefs.Add(const RefName: DOMString): IXMLAttributeDef;
begin
  Result := AddItem(-1) as IXMLAttributeDef;
  Result.RefName := RefName;
end;

function TXMLAttributeDefs.Add(const Name, DataType: DOMString): IXMLAttributeDef;
begin
  Result := AddItem(-1) as IXMLAttributeDef;
  Result.Name := Name;
  Result.DataTypeName := DataType;
end;

function TXMLAttributeDefs.Add(const Name: DOMString; LocalSimpleType: Boolean;
  BaseTypeName: DOMString = ''): IXMLAttributeDef;
begin
  Result := AddItem(-1) as IXMLAttributeDef;
  Result.Name := Name;
  if LocalSimpleType then
  begin
    Result.AddChild(SSimpleType);
    Result.DataType.BaseTypeName := BaseTypeName;
  end else
    Result.DataTypeName := BaseTypeName;
end;

function TXMLAttributeDefs.Find(const Name: DOMString): IXMLAttributeDef;
begin
  Result := FindItem(Name) as IXMLAttributeDef;
end;

function TXMLAttributeDefs.GetItem(Index: Integer): IXMLAttributeDef;
begin
  Result := GetSchemaItem(Index) as IXMLAttributeDef;
end;

function TXMLAttributeDefs.Insert(Index: Integer): IXMLAttributeDef;
begin
  Result := AddItem(Index) as IXMLAttributeDef;
end;

{ TXMLElementDefs }

procedure TXMLElementDefs.ChildListNotify(Operation: TNodeListOperation;
  var Node: IXMLNode; const IndexOrName: OleVariant; BeforeOperation: Boolean);
var
  TypeNode: TXMLNode;
begin
  inherited;
  if BeforeOperation then
  begin
    TypeNode := HostNode;
    while Assigned(TypeNode) and not (TypeNode is TXMLComplexTypeDef) do
      TypeNode := TXMLSchemaItem(HostNode).HostNode;
    if Assigned(TypeNode) then
      TXMLComplexTypeDef(TypeNode).ChildElementsChanged;
  end;
end;

function TXMLElementDefs.GetItem(Index: Integer): IXMLElementDef;
begin
  Result := GetSchemaItem(Index) as IXMLElementDef;
end;

function TXMLElementDefs.Add(const Name, DataType: DOMString): IXMLElementDef;
begin
  Result := AddItem(-1) as IXMLElementDef;
  Result.Name := Name;
  if DataType <> '' then
    Result.SetDataTypeName(DataType);
end;

function TXMLElementDefs.Add(const RefName: DOMString): IXMLElementDef;
begin
  Result := AddItem(-1) as IXMLElementDef;
  Result.RefName := RefName;
end;

function TXMLElementDefs.Add(const Name: DOMString;
  ComplexLocalType: Boolean; BaseTypeName: DOMString = ''): IXMLElementDef;
begin
  Result := AddItem(-1) as IXMLElementDef;
  Result.Name := Name;
  if ComplexLocalType then
  begin
    Result.AddChild(SComplexType);
    (Result.DataType as IXMLComplexTypeDef).ContentModel := cmSequence;
  end
  else
    Result.AddChild(SSimpleType);
  if BaseTypeName <> '' then
    Result.DataType.BaseTypeName := BaseTypeName;
end;

function TXMLElementDefs.Insert(Index: Integer): IXMLElementDef;
begin
  Result := AddItem(Index) as IXMLElementDef;
end;

procedure TXMLElementDefs.InternalAdd(const AElementDef: IXMLElementDef);
begin
  InsertInCollection(AElementDef, -1);
end;

function TXMLElementDefs.Find(const Name: DOMString): IXMLElementDef;
begin
  Result := FindItem(Name) as IXMLElementDef;
end;

{ TXMLElementCompositors }

function TXMLElementCompositors.Add(CompositorType: TCompositorType = ctSequence): IXMLElementCompositor;
const 
  CompositorNames: array[TCompositorType] of DOMString = (SAll, SChoice, SSequence);
begin
  CheckParent;
  Result := AddChild(CompositorNames[CompositorType]) as IXMLElementCompositor;
end;

function TXMLElementCompositors.Insert(Index: Integer): IXMLElementCompositor;
begin
  Result := AddItem(Index) as IXMLElementCompositor;
end;

function TXMLElementCompositors.GetItem(Index: Integer): IXMLElementCompositor;
begin
  Result := List[Index] as IXMLElementCompositor;  { No Refs }
end;

{ TXMLAttributeGroups }

function TXMLAttributeGroups.Add: IXMLAttributeGroup;
begin
  Result := AddItem(-1) as IXMLAttributeGroup;
end;

function TXMLAttributeGroups.Insert(Index: Integer): IXMLAttributeGroup;
begin
  Result := AddItem(Index) as IXMLAttributeGroup;
end;

function TXMLAttributeGroups.Find(const Name: DOMString): IXMLAttributeGroup;
begin
  Result := FindItem(Name) as IXMLAttributeGroup;
end;

function TXMLAttributeGroups.GetItem(Index: Integer): IXMLAttributeGroup;
begin
  Result := GetSchemaItem(Index) as IXMLAttributeGroup;
end;

{ TXMLElementGroups }

function TXMLElementGroups.Add(const NameOrRef: DOMString): IXMLElementGroup;
begin
  Result := AddItem(-1) as IXMLElementGroup;
  if ParentNode is TXMLSchemaDef then
    Result.Name := NameOrRef
  else
    Result.RefName := NameOrRef;
end;

function TXMLElementGroups.Insert(Index: Integer): IXMLElementGroup;
begin
  Result := AddItem(Index) as IXMLElementGroup;
end;

function TXMLElementGroups.Find(const Name: DOMString): IXMLElementGroup;
begin
  Result := FindItem(Name) as IXMLElementGroup;
end;

function TXMLElementGroups.GetItem(Index: Integer): IXMLElementGroup;
begin
  Result := GetSchemaItem(Index) as IXMLElementGroup;
end;

{ TXMLNotationDefs }

function TXMLNotationDefs.Add(const Name: DOMString;
  const APublicID: DOMString = ''; const ASystemID: DOMString = ''): IXMLNotation;
begin
  Result := AddItem(-1) as IXMLNotation;
  Result.Name := Name;
  if APublicID <> '' then
    Result.PublicID := APublicID
  else if ASystemID <> '' then
    Result.SystemID := ASystemID;
end;

function TXMLNotationDefs.GetItem(Index: Integer): IXMLNotation;
begin
  Result := List[Index] as IXMLNotation;
end;

function TXMLNotationDefs.Insert(Index: Integer): IXMLNotation;
begin
  Result := AddItem(Index) as IXMLNotation;
end;

{ TXMLAnnotationCollection }

function TXMLAnnotationCollection.Add: IXMLAnnotation;
begin
  Result := AddItem(-1) as IXMLAnnotation;
end;

function TXMLAnnotationCollection.GetItem(Index: Integer): IXMLAnnotation;
begin
  Result := List[Index] as IXMLAnnotation;
end;

function TXMLAnnotationCollection.Insert(Index: Integer): IXMLAnnotation;
begin
  Result := AddItem(Index) as IXMLAnnotation;
end;

{ TXMLAppInfoCollection }

function TXMLAppInfoCollection.Add: IXMLAppInfo;
begin
  Result := AddItem(-1) as IXMLAppInfo;
end;

function TXMLAppInfoCollection.GetAppInfo(const AppInfoType: TGuid;
  AppInfoClassType: TXMLNodeClass = nil): IXMLAppInfo;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Supports(GetItem(I), AppInfoType, Result) then Exit;
  if Assigned(AppInfoClasstype) then
    Result := AddChild(MakeNodeName(GetPrefix, SAppInfo), GetNamespaceURI, AppInfoClassType) as IXMLAppInfo
  else
    Result := nil;
end;


function TXMLAppInfoCollection.GetItem(Index: Integer): IXMLAppInfo;
begin
  Result := List[Index] as IXMLAppInfo;
end;


function TXMLAppInfoCollection.Insert(Index: Integer): IXMLAppInfo;
begin
  Result := AddItem(Index) as IXMLAppInfo;
end;

{ TXMLDocumentationCollection }

function TXMLDocumentationCollection.Add: IXMLDocumentation;
begin
  Result := AddItem(-1) as IXMLDocumentation;
end;

function TXMLDocumentationCollection.GetItem(Index: Integer): IXMLDocumentation;
begin
  Result := List[Index] as IXMLDocumentation;
end;

function TXMLDocumentationCollection.Insert(Index: Integer): IXMLDocumentation;
begin
  Result := AddItem(Index) as IXMLDocumentation;
end;

{ TXMLEnumerationCollection }

function TXMLEnumerationCollection.Add(const Value: DOMString): IXMLEnumeration;
begin
  Result := AddItem(-1) as IXMLEnumeration;
  Result.Value := Value;
end;

function TXMLEnumerationCollection.Insert(Index: Integer): IXMLEnumeration;
begin
  Result := AddItem(Index) as IXMLEnumeration;
end;

function TXMLEnumerationCollection.GetItem(Index: Integer): IXMLEnumeration;
begin
  Result := List[Index] as IXMLEnumeration;
end;

{ TXMLSchemaDef }

procedure TXMLSchemaDef.AfterConstruction;
begin
  RegisterChildNodes([SImport, SInclude, SAnnotation, SSimpleType,
     SComplexType, SElement, SGroup, SAttribute, SAttributeGroup, SNotation],
    [TXMLSchemaImport, TXMLSchemaInclude, TXMLAnnotation, TXMLSimpleTypeDef,
     TXMLComplexTypeDef, TXMLElementDef, TXMLElementGroup, TXMLAttributeDef,
     TXMLAttributeGroup, TXMLNotationDef]);
  FComplexTypes := CreateCollection(TXMLComplexTypeDefs, IXMLComplexTypeDef,
    SComplexType) as IXMLComplexTypeDefs;
  FSimpleTypes := CreateCollection(TXMLSimpleTypeDefs, IXMLSimpleTypeDef,
    SSimpleType) as IXMLSimpleTypeDefs;
  FAttributeDefs := CreateCollection(TXMLAttributeDefs, IXMLAttributeDef,
    SAttribute) as IXMLAttributeDefs;
  FAttributeGroups := CreateCollection(TXMLAttributeGroups, IXMLAttributeGroup,
    SAttributeGroup) as IXMLAttributeGroups;
  FElementDefs := CreateCollection(TXMLElementDefs, IXMLElementDef,
    SElement) as IXMLElementDefs;
  FElementGroups := CreateCollection(TXMLElementGroups, IXMLElementGroup,
    SGroup) as IXMLElementGroups;
  FAnnotations := CreateCollection(TXMLAnnotationCollection, IXMLAnnotation,
    SAnnotation) as IXMLAnnotationItems;
  FSchemaImports := CreateCollection(TXMLSchemaImports, IXMLSchemaImport,
    SImport) as IXMLSchemaImports;
  FSchemaIncludes := CreateCollection(TXMLSchemaIncludes, IXMLSchemaInclude,
    SInclude) as IXMLSchemaIncludes;
  FLegacyFormat := SameNamespace(GetNamespaceURI, SXMLSchemaURI_1999);
  inherited;
end;

function TXMLSchemaDef.LocateAppInfo(const NamespaceURI: DOMString;
  var AppInfoClassInfo: TAppInfoClassInfo): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Length(FAppInfoClasses) - 1 do
    if SameNamespace(NamespaceURI, FAppInfoClasses[I].NamespaceURI) then
    begin
      Result := True;
      AppInfoClassInfo := FAppInfoClasses[I];
      Break;
    end;
end;

procedure TXMLSchemaDef.RegisterAppInfo(const NamespaceURI, Prefix: DOMString;
  NodeClass: TXMLNodeClass);
var
  OldLength: Integer;
  AppInfoClassInfo: TAppInfoClassInfo;
begin
  { Check if there is already a tag registered, and replace the class if so }
  if LocateAppInfo(NamespaceURI, AppInfoClassInfo) then
    AppInfoClassInfo.NodeClass := NodeClass
  else
  begin
    { Othwise append the entry }
    OldLength := Length(FAppInfoClasses);
    SetLength(FAppInfoClasses,  OldLength + 1);
    FAppInfoClasses[OldLength].NamespaceURI := NamespaceURI;
    FAppInfoClasses[OldLength].Prefix := Prefix;
    FAppInfoClasses[OldLength].NodeClass := NodeClass;
    if FindNamespaceDecl(NamespaceURI) = nil then
      DeclareNamespace(Prefix, NamespaceURI);
  end;
end;

function TXMLSchemaDef.FindAppInfoClass(const NamespaceURI: DOMString): TXMLNodeClass;
var
  AppInfoClassInfo: TAppInfoClassInfo;
begin
  if LocateAppInfo(NamespaceURI, AppInfoClassInfo) then
    Result := AppInfoClassInfo.NodeClass
  else
    Result := nil;
end;

function TXMLSchemaDef.GetAnnotations: IXMLAnnotationItems;
begin
  Result := FAnnotations;
end;

function TXMLSchemaDef.GetAttributeDefs: IXMLAttributeDefs;
begin
  Result := FAttributeDefs;
end;

function TXMLSchemaDef.GetAttributeGroups: IXMLAttributeGroups;
begin
  Result := FAttributeGroups;
end;

function TXMLSchemaDef.GetBuiltInTypes: IXMLSimpleTypeDefs;
var
  BuiltInTypes: TXMLBuiltInTypes;
begin
  if not Assigned(FBuiltInTypes) then
  begin
    { Built-in schema types (like string) are stored in a separate collection
      and are part of the current document, but the nodes which represent them
      are not parented and do not appear in the document if it is saved }
    BuiltInTypes := TXMLBuiltInTypes.Create(CreateDOMNode(
      OwnerDocument.DOMDocument, MakeNodeName(GetPrefix, 'schemaTypes'), { Do not localize }
      ntElement, GetNamespaceURI), nil, OwnerDocument);
    BuiltInTypes.ItemInterface := IXMLSimpleTypeDef;
    BuiltInTypes.ItemTag := SSimpleType;
    BuiltInTypes.ItemNS := GetNamespaceURI;
    FBuiltInTypes := BuiltInTypes as IXMLSimpleTypeDefs;
    BuiltInTypes.SchemaDef := Self;
  end;
  Result := FBuiltInTypes;
end;

function TXMLSchemaDef.GetComplexTypes: IXMLComplexTypeDefs;
begin
  Result := FComplexTypes;
end;

function TXMLSchemaDef.GetSimpleTypes: IXMLSimpleTypeDefs;
begin
  Result := FSimpleTypes;
end;

function TXMLSchemaDef.GetElementDefs: IXMLElementDefs;
begin
  Result := FElementDefs;
end;

function TXMLSchemaDef.GetElementGroups: IXMLElementGroups;
begin
  Result := FElementGroups;
end;

function TXMLSchemaDef.GetNotationDefs: IXMLNotationDefs;
begin
  Result := FNotationDefs;
end;

function TXMLSchemaDef.GetSchemaImports: IXMLSchemaImports;
begin
  Result := FSchemaImports;
end;

function TXMLSchemaDef.GetSchemaIncludes: IXMLSchemaIncludes;
begin
  Result := FSchemaIncludes;
end;

function TXMLSchemaDef.GetStrictFormat: Boolean;
begin
  Result := FStrictFormat;
end;

procedure TXMLSchemaDef.SetStrictFormat(const Value: Boolean);
begin
  FStrictFormat := Value;
end;

function TXMLSchemaDef.GetTargetNamespace: OleVariant;
begin
  Result := GetAttribute(STargetNamespace);
end;

procedure TXMLSchemaDef.SetTargetNamespace(const NamespaceURI, NSPrefix: DOMString);
begin
  SetAttribute(STargetnamespace, NamespaceURI);
  FTargetNSPrefix := NSPrefix;
  DeclareNamespace(NSPrefix, NamespaceURI);
end;

function TXMLSchemaDef.GetFormChoice(const ChoiceName: DOMString): TFormChoice;
begin
  if HasAttribute(ChoiceName) and (GetAttribute(ChoiceName) = SQualified) then
    Result := fcQualified
  else
    Result := fcUnqualified;
end;

procedure TXMLSchemaDef.SetFormChoice(const ChoiceName: DOMString;
  const Value: TFormChoice);
const
  ChoiceTypeMap: array[TFormChoice] of DOMString = (SQualified, SUnqualified);
begin
  SetAttribute(ChoiceName, ChoiceTypeMap[Value]);
end;

function TXMLSchemaDef.GetAttributeFormDefault: TFormChoice;
begin
  Result := GetFormChoice(SAttributeFormDefault);
end;

procedure TXMLSchemaDef.SetAttributeformDefault(const Value: TFormChoice);
begin
  SetFormChoice(SAttributeFormDefault, Value);
end;

function TXMLSchemaDef.GetElementFormDefault: TFormChoice;
begin
  Result := GetFormChoice(SElementFormDefault);
end;

procedure TXMLSchemaDef.SetElementformDefault(const Value: TFormChoice);
begin
  SetFormChoice(SElementFormDefault, Value);
end;

function TXMLSchemaDef.GetVersion: DOMString;
begin
  Result := VarToWideStr(GetAttribute(SVersion));
end;

procedure TXMLSchemaDef.SetVersion(const Value: DOMString);
begin
  SetAttribute(SVersion, Value);
end;

function TXMLSchemaDef.LoadSchemaRef(SchemaLoc: DOMString): IXMLSchemaDef;
var
  I: Integer;
  FilePath, ImportPath: string;
begin
  if SchemaLoc <> '' then
  begin
    ImportPath := ExtractFilePath(SchemaLoc);
    FilePath := ExtractFilePath(OwnerDocument.FileName);
    if (FilePath <> '') and (ImportPath = '') then
      SchemaLoc := ExpandFileName(FilePath + SchemaLoc);
    Result := LoadXMLSchema(SchemaLoc).SchemaDef;
    for I := 0 to Length(FAppInfoClasses) - 1 do
      with FAppInfoClasses[I] do
        Result.RegisterAppInfo(NamespaceURI, Prefix, NodeClass);
  end;
end;

function TXMLSchemaDef.GetTargetNSPrefix: DOMString;
begin
  Result := FTargetNSPrefix;
end;

function TXMLSchemaDef.GetSchemaDoc: IXMLSchemaDoc;
begin
  Result := GetOwnerDocument as IXMLSchemaDoc;
end;

function TXMLSchemaDef.GetDocElementDef: IXMLElementDef;
begin
  Result := FDocElementDef;
end;

procedure TXMLSchemaDef.SetDocElementDef(const Value: IXMLElementDef);
begin
  FDocElementDef := Value;
end;

function TXMLSchemaDef.PrefixedTypeName(const TypeName: DOMString): DOMString;
begin
  Result := TypeName;
  if not IsPrefixed(TypeName) then
  begin
    { First check if the name matches one of the built in simple types }
    if (GetPrefix <> '') and
       (GetSimpleTypes.Find(MakeNodeName(GetPrefix, TypeName)) <> nil) then
      Result := MakeNodeName(GetPrefix, TypeName)
    { Otherwise assume this is a type defined locally and use the target
      namespace prefix if we have one }
    else if (TargetNSPrefix <> '') then
      Result := MakeNodeName(TargetNSPrefix, TypeName);
  end;
end;

{ TXMLSchemaDoc }

procedure TXMLSchemaDoc.AfterConstruction;
begin
  FEnableTranslation := True;
  RegisterDocBinding(SSchema, TXMLSchemaDef, SXMLSchemaURI_2001);
  RegisterDocBinding(SSchema, TXMLSchemaDef, SXMLSchemaURI_2000_10);
  RegisterDocBinding(SSchema, TXMLSchemaDef, SXMLSchemaURI_1999);
  inherited;
  Options := Options + [doNodeAutoIndent];
end;

procedure TXMLSchemaDoc.CheckSchemaVersion;
begin
  if not Supports(DocumentElement, IXMLSchemaDef) then
    SchemaParseError(SInvalidSchema, []);
end;

procedure TXMLSchemaDoc.DoAfterOpen;
begin
  if not IsEmptyDoc then
    CheckSchemaVersion;
end;

procedure TXMLSchemaDoc.PrepareNewSchema;
begin
  Version := '1.0';
  GetDocBinding(MakeNodeName(SXMLSchemaPrefix, SSchema),
    TXMLSchemaDef, XMLSchemaURI);
end;

procedure TXMLSchemaDoc.LoadData;
var
  Translator: IXMLSchemaTranslator;
begin
  FWasImported := False;
  if (FileName <> '') and EnableTranslation then
  begin
    Translator := FindSchemaImporter(FileName);
    if Assigned(Translator) then
    begin
      PrepareNewSchema;
      Translator.Translate(FileName, GetSchemaDef);
      FWasImported := True;
    end;
  end;
  if not FWasImported then
    inherited;
end;

procedure TXMLSchemaDoc.SaveToFile(const AFileName: WideString);
var
  Translator: IXMLSchemaTranslator;
begin
  if EnableTranslation then
    Translator := FindSchemaExporter(AFileName)
  else
    Translator := nil;
  if Assigned(Translator) then
    Translator.Translate(AFileName, GetSchemaDef)
  else
    inherited;
end;

function TXMLSchemaDoc.GetSchemaDef: IXMLSchemaDef;
begin
  Result := DocumentElement as IXMLSchemaDef;
end;

function TXMLSchemaDoc.GetWasImported: Boolean;
begin
  Result := FWasImported;
end;

{ TXMLSchemaTranslator }

procedure TXMLSchemaTranslator.Translate(const FileName: WideString;
  const SchemaDef: IXMLSchemaDef);
begin
  FSchemaDef := SchemaDef;
  FFileName := FileName;
end;

{ TXMLSchemaTranslatorFactory }

function TXMLSchemaTranslatorFactory.CanExportFile(const FileName: WideString): Boolean;
begin
  Result := Assigned(FExportTranslatorClass) and
    (AnsiCompareText(ExtractFileExt(FileName), FExtension) = 0);
end;

function TXMLSchemaTranslatorFactory.CanImportFile(const FileName: WideString): Boolean;
begin
  Result := Assigned(FImportTranslatorClass) and
    (AnsiCompareText(ExtractFileExt(FileName), FExtension) = 0);
end;

constructor TXMLSchemaTranslatorFactory.Create(ImportClass, ExportClass: TSchemaTranslatorClass; const
  Extension, Description: WideString);
begin
  inherited Create;
  FImportTranslatorClass := ImportClass;
  FExportTranslatorClass := ExportClass;
  FExtension := Extension;
  FDescription := Description;
end;

function TXMLSchemaTranslatorFactory.GetExportTranslator: IXMLSchemaTranslator;
begin
  Result := FExportTranslatorClass.Create;
end;

function TXMLSchemaTranslatorFactory.GetImportTranslator: IXMLSchemaTranslator;
begin
  Result := FImportTranslatorClass.Create;
end;

function TXMLSchemaTranslatorFactory.GetTranslatorDescription: WideString;
begin
  Result := FDescription;
end;

end.
