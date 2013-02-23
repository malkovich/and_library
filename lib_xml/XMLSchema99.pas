
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{       Importer for legacy XML Schema files            }
{                                                       }
{ Copyright (c) 2001-2002 Borland Software Corporation  }
{                                                       }
{*******************************************************}

unit XMLSchema99;

interface

uses SysUtils, XMLSchema, xmldom, XMLDoc, XMLIntf;

type

{ TXMLSchema1999TranslatorFactory }

  TXMLSchema1999TranslatorFactory = class(TXMLSchemaTranslatorFactory)
  protected
    function CanImportFile(const FileName: WideString): Boolean; override;
  end;

{ TXMLSchema1999Translator }

  TXMLSchema1999Translator = class(TXMLSchemaTranslator)
  private
    FOldSchema: IXMLSchemaDef;
  protected
    procedure CopyAttrNodes(const SourceNode, DestNode: IXMLNode);
    procedure CopyChildNodes(const SourceNode, DestNode: IXMLNode);
    procedure CopyComplexType(const ComplexTypeDef: IXMLComplexTypeDef;
      const DestNode: IXMLNode);
    procedure CopySimpleType(const SimpleTypeDef: IXMLSimpleTypeDef;
      const DestNode: IXMLNode);
    { Data member access }
    property OldSchema: IXMLSchemaDef read FOldSchema;
  public
    procedure Translate(const FileName: WideString; const SchemaDef: IXMLSchemaDef); override;
  end;

{ TXMLComplexTypeDef99 }

  TXMLComplexTypeDef99 = class(TXMLComplexTypeDef)
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSchemaDoc99 }

  TXMLSchemaDoc99 = class(TXMLSchemaDoc)
  protected
    function GetChildNodeClass(const Node: IDOMNode): TXMLNodeClass; override;
    procedure CheckSchemaVersion; override;
    procedure LoadData; override;
  public
    procedure AfterConstruction; override;
  end;

implementation

uses XMLSchemaTags, XMLConst;

var
  TranslatorFactory: IXMLSchemaTranslatorFactory;

{ TXMLSchema1999TranslatorFactory }

function TXMLSchema1999TranslatorFactory.CanImportFile(
  const FileName: WideString): Boolean;
var
  Doc: IXMLDocument;
begin
  Result := inherited CanImportFile(FileName);
  if Result then
  try
    Doc := LoadXMLDocument(FileName);
    Result := not Doc.IsEmptyDoc and (Doc.DocumentElement.FindNamespaceDecl(SXMLSchemaURI_1999) <> nil);
  except
    Result := False;
  end;
end;

{ TXMLSchema1999Translator }

procedure TXMLSchema1999Translator.CopyAttrNodes(const SourceNode,
  DestNode: IXMLNode);
var
  I: Integer;
  TypeName, AttrName: DOMString;
begin
  for I := 0 to SourceNode.AttributeNodes.Count - 1 do
  begin
    AttrName := SourceNode.AttributeNodes[I].NodeName;
    { Don't copy attribute if it's name is 'base' or if it's already there }
    if (AttrName <> SBase) and not DestNode.HasAttribute(AttrName) then
      DestNode.AttributeNodes.Add(SourceNode.AttributeNodes[I].CloneNode(True));
    { Fixup and "type" attributes that point to built in types }
    if AttrName = SType then
    begin
      TypeName := SourceNode.Attributes[SType];
      if ExtractPrefix(TypeName) = SourceNode.Prefix then
        DestNode.Attributes[SType] := MakeNodeName(DestNode.Prefix, ExtractLocalName(TypeName));
    end;
  end;
end;

procedure TXMLSchema1999Translator.CopyChildNodes(const SourceNode,
  DestNode: IXMLNode);
var
  I: Integer;
  DestChild, ChildNode: IXMLNode;
  ComplexTypeDef: IXMLComplexTypeDef;
  SimpleTypeDef: IXMLSimpleTypeDef;
begin
  for I := 0 to SourceNode.ChildNodes.Count - 1 do
  begin
    ChildNode := SourceNode.ChildNodes[I];
    if Supports(ChildNode, IXMLComplexTypeDef, ComplexTypeDef) then
      { ComplexTypes have to be copied specially }
      CopyComplexType(ComplexTypeDef, DestNode)
    else if Supports(ChildNode, IXMLSimpleTypeDef, SimpleTypeDef) then
      { as do SimpleTypes }
      CopySimpleType(SimpleTypeDef, DestNode)
    else
    begin
      if ChildNode.NodeType = ntElement then
      begin
        { Need to copy elements this way to avoid getting the source namespace }
        DestChild := DestNode.AddChild(ChildNode.LocalName);
        CopyAttrNodes(ChildNode, DestChild);
      end else
        { Everything else we can just clone }
        DestNode.ChildNodes.Add(ChildNode.CloneNode(False));
      if ChildNode.HasChildNodes then
        { Going down... }
        CopyChildNodes(ChildNode, DestChild);
    end;
  end;
end;

procedure TXMLSchema1999Translator.CopyComplexType(
  const ComplexTypeDef: IXMLComplexTypeDef; const DestNode: IXMLNode);
var
  I: Integer;
  NewChild, ChildNode: IXMLNode;
  ElementGroup: IXMLElementGroup;
  ElementDef: IXMLElementDef;
  NewComplexType: IXMLComplexTypeDef;
  ContentModel: TContentModel;
  Annotation: IXMLAnnotation;
begin
  NewComplexType := DestNode.AddChild(SComplexType) as IXMLComplexTypeDef;
  ContentModel := cmSequence;
  { Content Model is based on existing compositor when there is only one }
  if (ComplexTypeDef.ElementCompositors.Count = 1) then
    case ComplexTypeDef.ElementCompositors[0].CompositorType of
      ctAll: ContentModel := cmAll;
      ctChoice: ContentModel := cmChoice;
    end
  { If no compositor and no direct elements and one group then use the group }
  else if (ComplexTypeDef.ElementCompositors.Count = 0) and
          (ComplexTypeDef.ElementDefs.Count = 0) and
          (ComplexTypeDef.ElementGroups.Count = 1) then
    ContentModel := cmGroupRef;
  { Set the new content model }
  NewcomplexType.ContentModel := ContentModel;
  if not ComplexTypeDef.IsAnonymous then
    NewcomplexType.Name := ComplexTypeDef.Name;
  { Copy the contents }
  for I := 0 to ComplexTypeDef.ChildNodes.Count - 1 do
  begin
    ChildNode := ComplexTypeDef.ChildNodes[I];
    if Supports(ChildNode, IXMLElementDef, ElementDef) then
    begin
      { Copy Elements appearing directly below the type }
      if ElementDef.RefName <> '' then
        NewChild := NewComplexType.ElementDefs.Add(ElementDef.RefName)
      else if ElementDef.DataType.IsAnonymous then
        NewChild := NewComplexType.ElementDefs.Add(ElementDef.Name, '')
      else
        NewChild := NewComplexType.ElementDefs.Add(ElementDef.Name,
          ExtractLocalName(ElementDef.DataTypeName));
    end
    else if Supports(ChildNode, IXMLElementGroup, ElementGroup) then
      { Groups need to be copied by hand as well }
      NewChild := NewComplexType.ElementGroups.Add(ElementGroup.RefName)
    else if Supports(ChildNode, IXMLAnnotation, Annotation) then
      { Annotations must go first into the list }
      NewChild := NewComplexType.AddChild(SAnnotation, 0)
    else
      { All other child node we can copy directly }
      NewChild := NewComplexType.AddChild(ChildNode.LocalName);
    CopyAttrNodes(ChildNode, NewChild);
    if ChildNode.HasChildNodes then
      CopyChildNodes(ChildNode, NewChild);
  end;
end;

procedure TXMLSchema1999Translator.CopySimpleType(
  const SimpleTypeDef: IXMLSimpleTypeDef; const DestNode: IXMLNode);
var
  NewSimpleType: IXMLSimpleTypeDef;
begin
  NewSimpleType := DestNode.AddChild(SSimpleType) as IXMLSimpleTypeDef;
  NewSimpleType.BaseTypeName := ExtractLocalName(SimpleTypeDef.Attributes[SBase]);
  if not SimpleTypeDef.IsAnonymous then
    NewSimpleType.Name := SimpleTypeDef.Name;
  { Copy Enumerations and Facets }
  CopyChildNodes(SimpleTypeDef, NewSimpleType.RestrictionNode);
end;

procedure TXMLSchema1999Translator.Translate(const FileName: WideString;
  const SchemaDef: IXMLSchemaDef);
var
  OldSchemaDoc: IXMLSchemaDoc;
begin
  inherited;
  OldSchemaDoc := TXMLSchemaDoc99.Create(FileName);
  FOldSchema := OldSchemaDoc.SchemaDef;
  CopyChildNodes(FOldSchema, SchemaDef);
end;

{ TXMLComplexTypeDef99 }

procedure TXMLComplexTypeDef99.AfterConstruction;
begin
  inherited;
  { Unregister ComplexContent and SimpleContent that are not valid in this version }
  RegisterChildNodes([SComplexContent, SSimpleContent], [nil, nil]);
  { Complextypes in this version can have direct child elements }
  RegisterChildNode(SElement, TXMLElementDef);
end;

{ TXMLSchemaDoc99 }

procedure TXMLSchemaDoc99.AfterConstruction;
begin
  inherited;
end;

procedure TXMLSchemaDoc99.CheckSchemaVersion;
begin
  { Disable validation }
end;

function TXMLSchemaDoc99.GetChildNodeClass(const Node: IDOMNode): TXMLNodeClass;
begin
  if NodeMatches(Node, SComplexType, SXMLSchemaURI_1999) then
    Result := TXMLComplexTypeDef99
  else
    Result := nil;
end;

procedure TXMLSchemaDoc99.LoadData;
begin
  { Prevent recursion when we load the old schema document }
  EnableTranslation := False;
  inherited;
  { Register the default class, but with the older namespace }
  RegisterDocBinding(SSchema, TXMLSchemaDef, SXMLSchemaURI_1999);
end;

initialization
  TranslatorFactory := TXMLSchema1999TranslatorFactory.Create(
    TXMLSchema1999Translator, nil, SXMLSchemaExt, S99TransDesc);
  RegisterSchemaTranslator(TranslatorFactory);
finalization
  UnRegisterSchemaTranslator(TranslatorFactory);
end.

