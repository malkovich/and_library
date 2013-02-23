
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{       XDR Translator for XML Schema                   }
{                                                       }
{ Copyright (c) 2001-2002 Borland Software Corporation  }
{                                                       }
{*******************************************************}

unit XDRSchema;

interface

uses SysUtils, Classes, XMLSchema, XMLDOM, XMLDoc, XMLIntf;

type

{ TXDRImportTranslator }

  TXDRImportTranslator = class(TXMLSchemaTranslator)
  protected
    XDRDoc: IXMLDocument;
    SchemaPrefix: string;
    DataTypePrefix: string;
    ns: string;
    procedure AddAttribute(XdrAttributedef: IDOMNode; XsdAttributeDefs: IXMLAttributeDefs);
    procedure AddChildElement(XdrElement: IDOMNode; XsdElementDefs: IXMLElementDefs; Order: string);
    procedure AddExtends(Extends: IDOMNode; XsdElementDefs: IXMLElementDefs; Order: string);
    procedure Parse(XdrDom: IDomDocument);
    procedure ParseAttributeDefinition(Parent: string; XdrAttributeDef: IDOMNode; XsdAttributeDefs: IXMLAttributeDefs);
    procedure ParseElementDefinition(Parent: string; XdrElementDef: IDOMNode; XsdElementDefs: IXMLElementDefs);
    procedure ParseGroup(Group: IDOMNode; XsdElementCompositors: IXMLElementCompositors);
    procedure Translate(const FileName: WideString; const SchemaDef: IXMLSchemaDef); override;
  end;

const
  XDRExtension = '.xdr';
  XDR_DataTypes = 'urn:w3-org:xmldatatypes';
  XDR_Schema = 'urn:w3-org:xmlschema';
  MSXDR_DataTypes =  'urn:schemas-microsoft-com:datatypes';
  MSXDR_Schema =  'urn:schemas-microsoft-com:xml-data';

  xdrElementType='ElementType';
  xdrAttributeType='AttributeType';
  xdrDescription='Description';
  xdrElement='element';
  xdrAttribute='attribute';
  xdrGroup='group';
  xdrExtends='extends';

  xdrName='name';
  xdrType='type';
  xdrContent='content';
  xdrContent_default='mixed';
  xdrMixed='mixed';
  xdrEmpty='empty';
  xdrTextOnly='textOnly';
  xdrEltOnly='eltOnly';

  xdrModel='model';
  xdrOpen='open';
  xdrClosed='closed';
  xdrOrder='order';
  xdrSeq='seq';
  xdrOne='one';
  xdrAll='all';
  xdrMany='many';

  xdrRequired='required';
  xdrRequired_default='no';
  xdrYes='yes';
  xdrNo='no';

  xdrDefault='default';

  xdrOccurs='occurs';
  xdrMinOccurs='minOccurs';
  xdrMaxOccurs='maxOccurs';
  xdrOptional='optional';
  xdrOneOrMore='oneOrMore';
  xdrZeroOrMore='zeroOrMore';

  xdrDatatype='datatype'; //only if no dt:type, and content=textonly
  xdrMaxLength='maxLength';
  xdrValues='values';
  xdrMax='max';
  xdrMin='min';
  xdrMinExclusive='minExclusive';
  xdrMaxExclusive='maxExclusive';

  xdrString='string';
  xdrId='id';
  xdrIdref='idref';
  xdrIdrefs='idrefs';
  xdrEntity='entity';
  xdrEntities='entities';
  xdrNmtoken='nmtoken';
  xdrNmtokens='nmtokens';
  xdrNumber='number';
  xdrInt='int';
  xdrEnumeration='enumeration';
  xdrNotation='notation';
  xdrFixed='fixed';
  xdrBoolean='boolean';
  xdrDateTime='dateTime';
  xdrDateTimeTz='dateTime.tz';
  xdrDate='date';
  xdrTime='time';
  xdrTimetz='time.tz';
  xdrI1='i1';
  xdrByte='byte';
  xdrI2='i2';
  xdrI4='i4';
  xdrI8='i8';
  xdrUi1='ui1';
  xdrUi2='ui2';
  xdrUi4='ui4';
  xdrUi8='ui8';
  xdrR4='r4';
  xdrR8='r8';
  xdrFloat='float';
  xdrChar='char';
  xdrUuid='uuid';
  xdrBinhex='bin.hex';
  xdrBinbase64='bin.base64';

implementation

uses XMLSchemaTags, XMLConst;

{ TXDRImportTranslator }

procedure TXDRImportTranslator.Translate(const FileName: WideString;
  const SchemaDef: IXMLSchemaDef);
begin
  inherited;
  XDRDoc := LoadXMLDocument(FileName);
  Parse(XDRDoc.DOMDocument);
end;


function Split(Str: string;  SubStr: string): TStringList;
var
  I: Integer;
  S, Tmp: string;
  List: TStringList;
begin
  List := TStringList.Create;
  S := Str;
  while Length(S) > 0 do
  begin
    I := Pos(SubStr, S);
    if I = 0 then
    begin
      List.Add(S);
      S := '';
    end
    else
    begin
      if I = 1 then
      begin
        List.Add('');
        Delete(S, 1, Length(SubStr));
      end
      else
      begin
        Tmp := S;
        Delete(Tmp, I, Length(Tmp));
        List.Add(Tmp);
        Delete(S, 1, I + Length(SubStr) - 1);
        if Length(S) = 0 then
          List.Add('');
      end;
    end;
  end;
  Result := List;
end;

function Tail(Str: string; SubStr: string): string;
var
  I: Integer;
begin
  I:= pos(SubStr, Str);
  if  i = 0 then
    Result:= ''
  else
  begin
    delete(Str,1,i+Length(SubStr)-1);
    Result:= Str;
  end;
end;

function Head(Str: string; SubStr: string; var TailStr: string): string;
var
  I: Integer;
begin
  i:= pos(SubStr, Str);
  if I = 0 then
  begin
    Result:= Str;
    TailStr:= '';
  end
  else
  begin
    TailStr:= Str;
    delete(TailStr, 1, I+Length(SubStr)-1);
    delete(Str, I, Length(Str));
    Result:= Str;
  end;
end;


function GetAttribute(Node:IDOMNode; Name:string):string;
var
  attr:IDOMNode;
begin
  Result:= '';
  if (Node<>nil) and (Node.attributes <> nil) then
  begin
    attr:= Node.attributes.GetNamedItem(name);
    if attr<>nil then
      Result:= attr.nodeValue;
  end;
end;

function XdrTypeToXsdType(DtType: string):string;
begin
  if DtType = xdrId then Result:= xsdID else
  if DtType = xdrIdRef then Result:= xsdIDREF else
  if DtType = xdrIdRefs then Result:= xsdIDREFS else
  if DtType = xdrEntity then Result:= xsdENTITY else
  if DtType = xdrEntities then Result:= xsdENTITIES else
  if DtType = xdrNmToken then Result:= xsdNMTOKEN else
  if DtType = xdrNmTokens then Result:= xsdNMTOKENS else
  if DtType = xdrNotation then Result:= xsdNOTATION else

 // if DtType = xdrDateTimeTz then Result:= xsdInt else
 // if DtType = xdrTimeTz then Result:= xsdInteger else
  if DtType = xdrUuid then Result:= xsdAnyURI else
  if DtType = xdrBinHex then Result:= xsdHexBinary else
  if DtType = xdrBinBase64 then Result:= xsdBase64Binary else

  if DtType = xdrString then Result:= xsdString else
  if DtType = xdrChar then Result:= xsdByte else
  if DtType = xdrI1 then Result:= xsdByte else
  if DtType = xdrByte then Result:= xsdByte else
  if DtType = xdrI2 then Result:= xsdShort else
  if DtType = xdrI4 then Result:= xsdInteger else
  if DtType = xdrInt then Result:= xsdInteger else
  if DtType = XdrI8 then Result:= xsdLong else
  if DtType = xdrUi1 then Result:= xsdUnsignedByte else
  if DtType = xdrUi2 then Result:= xsdUnsignedShort else
  if DtType = xdrUi4 then Result:= xsdUnsignedInt else
  if DtType = xdrUi8 then Result:= xsdUnsignedLong else
//  if DtType = xdrR4      then Result:= xsd else
  if DtType = xdrR8 then Result:= xsdDouble else
  if DtType = xdrFloat then Result:= xsdFloat else

  if DtType = xdrNumber then Result:= xsdFloat else
  if DtType = xdrFixed then Result:= xsdDecimal else
  if DtType = xdrBoolean then Result:= xsdBoolean else
  if DtType = xdrDate then Result:= xsdDate else
  if DtType = xdrDateTime then Result:= xdrDateTime else
  if DtType = xdrTime then Result:= xsdTime else
//  if DtType = dt_options  then Result:= Xsd_Enumerations else
//  if DtType = dt_Unknown then Result:= xsdString else
    Result:= xsdString;
end;

procedure DetermineOccurence(Occurs: string; var MinOccurs: string; var MaxOccurs: string; Order: string);
begin
  if (Occurs <> '') then
  begin
    if (pos(':', Occurs) > 0) then
    begin
      MinOccurs:= Head(Occurs,':', MaxOccurs);
    end
    else
    if Occurs=xdroneorMore then
    begin
       MinOccurs:= '1';
       MaxOccurs:= SUnbounded;
    end
    else
    if Occurs= xdrZeroOrMore then
    begin
       MinOccurs:= '0';
       MaxOccurs:= SUnbounded;
    end
    else
    if Occurs=xdrOptional then
    begin
       MinOccurs:= '0';
       MaxOccurs:= '1';
    end;
  end;
  if Order=xdrMany then
  begin
    MinOccurs:= '0';
    MaxOccurs:= SUnbounded;
  end;

  if (MaxOccurs='*') or (MaxOccurs='+') then
    MaxOccurs:= SUnbounded;
end;

//parse an expanded xdr-string into a nodeinfo-list
procedure TXDRImportTranslator.Parse(XdrDom: IDomDocument);
var
  I: integer;
  DtTypes, DtValues: string;
  Root, SchemaNode, AttributeNode, ChildNode: IDOMNode;
  AttributeName, AttributeValue, ChildName: string;
  SchemaName, Prefix, RootName, Ns: string;
begin
  try
    SchemaPrefix:= '';
    DataTypePrefix:= 'dt:';
    DtTypes:= 'dt:type';
    DtValues:= 'dt:values';
    Ns:= '';
    Root:= XdrDom.DocumentElement;
    if (Root = nil)  then exit;
      SchemaName:= Root.LocalName;
      if pos('Schema', SchemaName) > 0 then
      begin
        SchemaNode:= Root;
        //Get any prefix
        if pos(':', SchemaName) > 0 then
          SchemaPrefix:= Head(RootName, ':', SchemaName) + ':'
        else
          SchemaPrefix:= '';
        RootName:= GetAttribute(Root, xdrName);

        for I:= 0 to Root.attributes.length-1 do
        begin
          AttributeNode:= Root.Attributes.item[I];
          AttributeName:= AttributeNode.NodeName;
          if pos('xmlns', AttributeName)> 0 then
          begin
            if pos(':', AttributeName) > 0 then
            begin
              Prefix:= Tail(AttributeName, ':') +':';
            end
            else
              Prefix:= '';
              AttributeValue:= AttributeNode.NodeValue;
              if AttributeValue=MSXDR_Schema then
                SchemaPrefix:= Prefix;
              if AttributeValue=MSXDR_DataTypes then
                DataTypePrefix:= Prefix;
          end;
        end;

        //Process all children
        for I:= 0 to SchemaNode.ChildNodes.Length-1 do
        begin
          ChildNode:= SchemaNode.ChildNodes.item[I];
          ChildName:= ChildNode.LocalName;

          if ChildName = xdrElementType then
          begin
            ParseElementDefinition('', ChildNode, SchemaDef.ElementDefs);
          end else
          if ChildName = xdrAttributeType then
          begin
            ParseAttributeDefinition('', ChildNode, SchemaDef.AttributeDefs);
          end else
          if ChildName = xdrNotation then
          begin
          end else
          if ChildName = xdrEntity then
          begin
          end else
          if ChildName = xdrDescription then
          begin
          end;
        end;
    end;
  finally
  end;
end;

procedure TXDRImportTranslator.ParseElementDefinition(Parent:string;
          XdrElementDef: IDOMNode; XsdElementDefs: IXMLElementDefs);
var
  I, J: Integer;
  Name, Content, Order, Model: string;
  DtType, DtValues, DtMaxLength, DtMin, DtMax: string;
  DtNode, XdrChildNode : IDOMNode;
  ChildName: string;
  XsdType: string;
  ComplexTypeDef: IXMLComplexTypeDef;
  SimpleTypeDef: IXMLSimpleTypeDef;
  Compositor: IXMLElementCompositor;
begin
  Name    := GetAttribute(XdrElementDef, xdrname);
  if Name = '' then exit;

  Content:= GetAttribute(XdrElementDef, xdrcontent);
  Order:= GetAttribute(XdrElementDef, xdrorder);
  Model:= GetAttribute(XdrElementDef, xdrmodel);

  if Content = xdrTextOnly then
  begin
    DtNode:= nil;
    DtType:= GetAttribute(XdrElementDef, DataTypePrefix+'type');
    if DtType<>'' then
      DtNode:= XdrElementDef
    else
    if XdrElementDef.ChildNodes.Length>0 then
    begin //contains datatype element
      for J:= 0 to XdrElementDef.ChildNodes.Length-1 do
      begin
        XdrChildNode:= XdrElementDef.ChildNodes.item[J];
        if XdrChildNode.LocalName = xdrDatatype then
        begin
          DtNode:= XdrChildNode;
          DtType:= GetAttribute(DtNode, DataTypePrefix+'type');
          if DtType='' then
            DtNode:= nil;
          break;
        end;
      end;
    end;
    if DtNode <> nil then
    begin //defaults
      DtValues:= GetAttribute(DtNode, DataTypePrefix+xdrvalues);
      DtMaxLength:= GetAttribute(DtNode, DataTypePrefix+xdrMaxLength);
      DtMin:= GetAttribute(DtNode, DataTypePrefix+xdrMin);
      DtMax:= GetAttribute(DtNode, DataTypePrefix+xdrMax);
      XsdType:= XdrTypeToXsdType(DtType);  //XdrTypeToCdsType(pInfo, DtType, DtMaxLength, DtValues);
    end
    else
      XsdType:= xsdString; //convert DtType;
    XsdElementDefs.Add(Name, Name+'Type');
    SimpleTypeDef:= SchemaDef.SimpleTypes.Add(Name+'Type', XsdType);
    with SimpleTypeDef do
    begin
      if DtMaxLength <> '' then
        MaxLength:= DtMaxLength;
      if DtMin <> '' then
        MinInclusive:= DtMin;
      if DtMax <> '' then
        MaxInclusive:= DtMax;
      if DtValues <> '' then
      begin
        //enumeration ? Value:= DtValues;
      end;
    end;
  end//textOnly
  else
  begin //Elements-only
    XsdElementDefs.Add(Name, Name+'Type');
    ComplexTypeDef:= SchemaDef.ComplexTypes.Add(Name+'Type');
    if Order = xdrOne then
      Compositor:= ComplexTypeDef.ElementCompositors.Add(ctChoice)
    else Compositor:= nil;

    for I:= 0 to XdrElementdef.ChildNodes.Length-1 do
    begin
      XdrChildNode:= XdrElementDef.childNodes.item[I];
      ChildName:= XdrChildNode.LocalName;

      if ChildName = xdrElementType then
      begin
        if Compositor <> nil then
          ParseElementDefinition(Name, XdrChildNode, Compositor.ElementDefs)
        else
          ParseElementDefinition(Name, XdrChildNode, ComplexTypeDef.ElementDefs);
      end
      else
      if ChildName = xdrAttributeType then
      begin
        ParseAttributeDefinition(Name, XdrChildNode, ComplexTypeDef.AttributeDefs);
      end
      else
      if ChildName = xdrElement then
      begin
        if Compositor <> nil then
          AddChildElement(XdrChildNode, Compositor.ElementDefs, Order)
        else
          AddChildElement(XdrChildNode, ComplexTypeDef.ElementDefs, Order)
      end
      else
      if ChildName = xdrAttribute  then
      begin
        AddAttribute(XdrChildNode, ComplexTypeDef.AttributeDefs);
      end
      else
      if ChildName = xdrGroup then
      begin
        if Compositor <> nil then
          ParseGroup(XdrChildNode, Compositor.Compositors)
        else
          ParseGroup(XdrChildNode, ComplexTypeDef.ElementCompositors);
      end
      else
      if ChildName = xdrExtends then
      begin
        if Compositor <> nil then
          AddExtends(XdrChildNode, Compositor.ElementDefs, Order)
        else
          AddExtends(XdrChildNode, ComplexTypeDef.ElementDefs, Order)
      end;
    end;
  end;//Elementsonly
end;

procedure TXDRImportTranslator.ParseAttributeDefinition(Parent: string;
          XdrAttributeDef: IDOMNode; XsdAttributeDefs: IXMLAttributeDefs);
var
  I: Integer;
  Name : string;
  Required, DefValue, DtType, DtValues, DtMaxLength, DtMin, DtMax: string;
  DtNode, ChildNode: IDOMNode;
  OList: TStringList;
  XdrChildName: string;
  XsdType: string;
  AttributeDef: IXMLAttributeDef;
begin
  Name:= GetAttribute(XdrAttributeDef, xdrName);
  Dttype:= GetAttribute(XdrAttributeDef, DataTypePrefix+'type');
  DtNode:= nil;
  if DtType<> '' then
    DtNode:= XdrAttributeDef
  else
  begin
    for I:= 0 to XdrAttributeDef.ChildNodes.Length-1 do
    begin
      ChildNode:= XdrAttributeDef.Childnodes.Item[I];
      XdrChildName:= ChildNode.LocalName;
      if XdrChildName = xdrDatatype then
      begin
        DtNode:= ChildNode;
        break;
      end
    end;
  end;

  if DtNode<>nil then
  begin
    DtType:= GetAttribute(DtNode, DataTypePrefix+'type');
    DtValues:= GetAttribute(DtNode, DataTypePrefix+xdrvalues);
    DtMaxLength:= GetAttribute(DtNode, DataTypePrefix+xdrmaxLength);
    DtMin:= GetAttribute(DtNode, DataTypePrefix+xdrMin);
    DtMax:= GetAttribute(DtNode, DataTypePrefix+xdrMax);
    XsdType:= XdrTypeToXsdType(DtType);
  end
  else
     XsdType:= xsdString;

  Required := GetAttribute(XdrAttributeDef, SchemaPrefix+xdrRequired);
  DefValue := GetAttribute(XdrAttributeDef, SchemaPrefix+xdrDefault);

  if DtValues<> '' then
    AttributeDef := xsdAttributeDefs.Add(Name, true, XsdType)
  else
    AttributeDef := xsdAttributeDefs.Add(Name, XsdType);
  with AttributeDef do
  begin
    if DefValue <> '' then
    begin
      Default := DefValue;
    end;
    if Required = 'yes' then //yes ?
    begin
      Use := SRequired;
    end;
    //FixedValue
    if (DtType = XdrEnumeration) and (DtValues <> '') then
    begin
      OList:= Split(DtValues, ' ');
      try
        for I:= 0 to OList.Count-1 do
        begin
          DataType.Enumerations.Add(OList[I]);
        end;
      finally
        OList.Free;
      end;
    end;
  end;
end;

procedure TXDRImportTranslator.ParseGroup(Group: IDOMNode; XsdElementCompositors: IXMLElementCompositors);
var
  I: integer;
  ChildNode: IDOMNode;
  ChildName, GroupOrder, groupOccurs, GroupMinOccurs, GroupMaxOccurs: string;
  CompType: TCompositorType;
  Compositor: IXMLElementCompositor;
begin
  GroupOrder:= GetAttribute(Group, xdrOrder);
  GroupOccurs:= GetAttribute(Group, xdrOccurs);
  GroupMinOccurs:= GetAttribute(Group, xdrMinOccurs);
  GroupMaxOccurs:= GetAttribute(Group, xdrMaxOccurs);

  if GroupOrder = xdrSeq then
    CompType:= ctSequence
  else
  if GroupOrder = xdrOne then
    CompType:= ctChoice
  else
  if GroupOrder = xdrAll then
    CompType:= ctAll
  else
  if GroupOrder = xdrMany then
    CompType:= ctSequence
  else
    CompType:= ctSequence;

  Compositor:= XsdElementCompositors.Add(CompType);
  DetermineOccurence(GroupOccurs, GroupMinOccurs, GroupMaxOccurs, GroupOrder);
  if GroupMinOccurs = '' then GroupMinOccurs:= '1';
  if GroupMaxOccurs = '' then GroupMaxOccurs:= '1';
  Compositor.MinOccurs:= GroupMinOccurs;
  Compositor.MaxOccurs:= GroupMaxOccurs;

  for I:= 0 to Group.ChildNodes.Length-1 do
  begin
     ChildNode:= Group.ChildNodes.item[I];
     ChildName:= ChildNode.LocalName;
     if ChildName = xdrElement then
     begin
       AddChildElement(ChildNode, Compositor.ElementDefs, GroupOrder);
     end
     else
     if ChildName = xdrGroup then
     begin
        ParseGroup(ChildNode, Compositor.Compositors);
     end
     else
     if ChildName = xdrExtends then
     begin
        AddExtends(ChildNode, Compositor.ElementDefs, GroupOrder);
     end;
  end;
end;


procedure TXDRImportTranslator.AddChildElement(XdrElement: IDOMNode; XsdElementDefs: IXMLElementDefs; Order: string);
var
  Name: string;
  MinOccurs, MaxOccurs, Occurs: string;
  ChildElement: IXMLElementDef;
begin
  Name:= GetAttribute(XdrElement, 'type');
  MinOccurs:= GetAttribute(XdrElement, xdrMinOccurs);
  MaxOccurs:= GetAttribute(XdrElement, xdrMaxOccurs);
  Occurs:= GetAttribute(XdrElement, xdrOccurs);
  DetermineOccurence(Occurs, MinOccurs, MaxOccurs, Order);
  if MinOccurs = '' then MinOccurs:= '1';
  if MaxOccurs = '' then MaxOccurs:= '1';
  ChildElement:= XsdElementdefs.Add(Name);
  ChildElement.MinOccurs:= MinOccurs;
  ChildElement.MaxOccurs:= MaxOccurs;
end;

procedure TXDRImportTranslator.AddAttribute(XdrAttributeDef: IDOMNode; XsdAttributeDefs: IXMLAttributeDefs);
var
  Name: string;
  DefValue, Required: string;
begin
  Name:= GetAttribute(XdrAttributeDef, Stype);
  DefValue:= GetAttribute(XdrAttributeDef, xdrDefault);
  Required:= GetAttribute(XdrAttributeDef, xdrRequired);

  with XsdAttributeDefs.Add(Name) do
  begin
    if DefValue <> '' then
    begin
      Default := DefValue;
    end;
    if Required <> '' then
    begin
      Use:= SRequired;
    end;
    //Fixed
  end;
end;

procedure TXDRImportTranslator.AddExtends(Extends: IDOMNode; XsdElementDefs: IXMLElementDefs; Order: string);
var
  Name: string;
begin
  Name:= GetAttribute(Extends, Stype);
  XsdElementDefs.Add(Name);
end;

var
  TranslatorFactory: IXMLSchemaTranslatorFactory;
initialization
  TranslatorFactory:= TXMLSchemaTranslatorFactory.Create(TXDRImportTranslator,
    nil, XDRExtension, SXDRTransDesc);
  RegisterSchemaTranslator(TranslatorFactory);
finalization
  UnRegisterSchemaTranslator(TranslatorFactory);
end.

