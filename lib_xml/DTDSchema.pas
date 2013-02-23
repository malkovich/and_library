
{*******************************************************}
{                                                       }
{    Borland Delphi Visual Component Library            }
{    DTD Translator for XML Schema                      }
{                                                       }
{ Copyright (c) 2001-2002 Borland Software Corporation  }
{                                                       }
{*******************************************************}

unit DTDSchema;

interface

uses SysUtils, Classes, XMLSchema, XMLDOM;

type

{ TDTDBaseTranslator }

  TDTDBaseTranslator = class(TXMLSchemaTranslator)
  private
    FDTDLines: TStrings;
  protected
    { Data member access }
    property DTDLines: TStrings read FDTDLines;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

{ TDTDImportTranslator }

  TDTDImportTranslator = class(TDTDBaseTranslator)
  protected
    function  Expand(Dtd: string; var RootName: string; SrcPath: string): string;
    procedure ExtractDtdParts(Str: string; StrBeg: string; StrEnd: string; Parts: TStrings);
    procedure Parse(Dtd: string);
    procedure ParseAttributeDefinitions(AttributeList: TStrings);
    procedure ParseAttributeDefinition(AttrStr: string);
    procedure ParseChildElements(ComplexDef: IXMLComplexTypeDef; ParentCompositors: IXMLElementCompositors; var Str: string);
    procedure ParseElementDefinitions(ElementList: TStrings; ElementsWithAttributes: TStrings);
    procedure  ParseTokens(Str: string; Tokens: TStrings);
    function  PreParseChildElements(Str: string; var Suffix: string; var GroupType: string): string;
    procedure  PreparseAttributeDefinitions(AttributeList: TStrings; AttrDefs: TStrings);
    procedure Translate(const FileName: WideString; const SchemaDef: IXMLSchemaDef); override;
  end;

const
  DTDExtension = '.dtd';

implementation

uses XMLSchemaTags, XMLConst;

{ Utility Functions }

function LoadFile(const Name: string): string;
begin
  with TStringList.Create do
  try
    LoadFromFile(Name);
    Result:= Text;
  finally
    Free;
  end;
end;

function Join(List: TStrings; Str: string): string;
var
  I: Integer;
begin
  if List.Count = 0 then
    Result := Str
  else
    Result:= List[0];
  for I := 1 to List.Count - 1 do
     Result := Result + Str + List[I];
end;

procedure Split(Str: string;  SubStr: string; List: TStrings);
var
  I: Integer;
  S, Tmp: string;
begin
  List.Clear;
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
end;

function Head(Str: string; SubStr: string; var TailStr: string): string;
var
  I: Integer;
begin
  I := Pos(SubStr, Str);
  if I = 0 then
  begin
    Result := Str;
    TailStr := '';
  end
  else
  begin
    TailStr := Str;
    Delete(TailStr, 1, I + Length(SubStr) - 1);
    Delete(Str, I, Length(Str));
    Result := Str;
  end;
end;

function Tail(Str: string; SubStr: string): string;
var
  I: Integer;
begin
  I := Pos(SubStr, Str);
  if I = 0 then
    Result := ''
  else
  begin
    Delete(Str, 1, I + Length(SubStr) - 1);
    Result:= Str;
  end;
end;

function Trim(Str: string): string;
var
  Tokens: TStrings;
begin
  if Length(Str) > 0 then
  begin
    Tokens := TStringList.Create;
    try
      // Replace end-of-line chars
      Split(Str, #13, Tokens);
      Str := Join(Tokens, ' ');
      Split(Str, #10, Tokens);
      Str := Join(Tokens, ' ');
      Split(Str, #9, Tokens);
      Str := Join(Tokens, ' ');
    finally
      Tokens.Free;
    end;
    while (Length(Str) > 0) and (Str[1] = ' ') do
      Str := Tail(Str, ' '); //remove leading spaces
    while (Length(Str) > 0) and (Str[Length(Str)]=' ') do
      SetLength(Str, Length(Str) - 1);
    while (Length(Str) > 0) and (Str[Length(Str)] = #0) do
      SetLength(Str, Length(Str) - 1);
    Result := Str;
  end
  else
    Result := '';
end;

function ReadName(Str: string; var TailStr: string): string;
var
  I: Integer;
  Ch: Char;
begin
  Str := Trim(Str);
  Ch := ' ';
  for I := 1 to Length(Str) do
  begin
    if Str[I] in ['(','#',' '] then
    begin
      Ch := Str[I];
      Break;
    end
  end;
  Str := Head(Str, Ch, TailStr);
  if Ch <> ' ' then
    TailStr := Ch + TailStr;
  TailStr := Trim(TailStr);
  Result := Trim(Str);
end;

function Replace(Str: string; FindStr: string; ReplaceStr: string): string;
begin
  Result:= '';
  while Pos(FindStr, Str) > 0 do
    Result := Result + Head(Str, FindStr, Str) + ReplaceStr;
  Result:= Result+ Str;
end;

function UnQuote(Str: string; const QuoteChar: string = '"'): string;
begin
  Str:= Tail(Str, QuoteChar);
  Result:= Head(Str, QuoteChar, Str);
end;

{ TDTDBaseTranslator }

procedure  TDTDBaseTranslator.AfterConstruction;
begin
  inherited;
  FDTDLines := TStringList.Create;
end;

destructor TDTDBaseTranslator.Destroy;
begin
  inherited;
  FreeAndNil(FDTDLines);
end;

{ TDTDImportTranslator }

procedure TDTDImportTranslator.Translate(const FileName: WideString;
  const SchemaDef: IXMLSchemaDef);
var
  Dtd: string;
  Pathname, RootName: string;
  Path: TStrings;
begin
  inherited;
  if FileName = '' then Exit;
  DTDLines.LoadFromFile(FileName);
  Dtd:= DTDLines.Text;
  if Dtd = '' then exit;
  Path := TStringList.Create;
  try
    Split(Filename, '\', Path);
    Path[Path.count-1] := '';
    PathName:= Join(Path, '\');
  finally
    Path.Free;
  end;
  Dtd:= Expand(Dtd, RootName, PathName);
  Parse(Dtd);
end;

procedure TDTDImportTranslator.ExtractDtdParts(Str: string; StrBeg: string;
  StrEnd: string; Parts: TStrings);
var
  E, X: string;
begin
  Parts.Clear;
  while Length(Str) > 0 do
  begin
    X := Head(Str, StrBeg, Str); //  s: "...<!ELEMENT xx yy > ... "  -> " xx yy > ..."
    if Length(Str) > 0 then
    begin
      E := Head(Str, StrEnd, Str); //  e: " xx yy " , s: "...."
      Trim(E);
      if Length(E) > 0 then
        Parts.Add(E);
    end;
  end;
end;

function TDTDImportTranslator.Expand(Dtd: string; var RootName:
  string; SrcPath: string): string;

  function SubstituteEntities(Str: string; EntityList: TStrings;
    var IncFile: Boolean; SrcPath: string): string;
  var
    I: Integer;
    Entity, EntityName: string;
    IncludeFile, ReplaceString, Tmp: string;
  begin
    for I := 0 to EntityList.Count-1 do
    begin
      Entity := EntityList[I];
      Entity := Tail(Entity, '%');
      EntityName := ReadName(Entity, ReplaceString);

      if Pos('SYSTEM', Entity) > 0 then
      begin
        Includefile := Tail(EntityList[I], 'SYSTEM');
        Includefile := Tail(IncludeFile,'"');
        Includefile := Head(IncludeFile,'"', Tmp);
        ReplaceString := LoadFile(SrcPath + IncludeFile);
        Str := Replace(Str, EntityList[I], ' ');
        Incfile := True;
      end
      else
      begin
        ReplaceString := Tail(ReplaceString, '"');
        ReplaceString := Head(ReplaceString, '"', Tmp);
     end;
     Str := Replace(Str, '%'+EntityName+';', ' '+ReplaceString+' ');
    end;
    Result := Str;
  end;

var
  EntityList, ElementList, DocList: TStrings;
  Str, Tmp : string;
  More : Boolean;
begin
  try
    Str := Dtd;
    DocList := TStringList.Create;
    try
      ExtractDtdParts(Dtd, '<!DOCTYPE', ']>', DocList);
      if (DocList.Count = 0) and (Rootname= '') then
      begin
        Str := Dtd;
        //Assume first element is the root
        ElementList := TStringList.Create;
        try
          ExtractDtdParts(Str, '<!ELEMENT', '>', ElementList);
          if ElementList.count > 0 then
            RootName :=ReadName(ElementList[0], Tmp);
        finally
          ElementList.Free;
        end;
      end
      else
      begin
        if DocList.Count <> 0 then
        begin
          Str := DocList[0];
          RootName := Trim(Head(Str, '[', Str));
          RootName := Trim(RootName);
        end;
      end;
      More := True;
      while More do
      begin
        More := False;
        EntityList := TStringList.Create;
        try
          ExtractDtdParts(Str, '<!ENTITY', '>', EntityList);
          if EntityList.count > 0 then //substitute
          repeat
            Tmp := Str;
            Str := SubstituteEntities(Str, EntityList, More, Srcpath);
          until Tmp = Str;
        finally
          EntityList.Free;
        end;
      end;
      Result := Str;
    finally
      DocList.Free;
    end;
  except
    Result := '';
  end;
end;

//parse an expanded dtd-string into a nodeinfo-list
procedure TDTDImportTranslator.Parse(Dtd: string);
var
  ElementDefinitionList,
  AttributeDefinitionList,
  ElementsWithAttributes: TStrings;
begin
  AttributeDefinitionList := nil;
  ElementsWithAttributes := nil;
  ElementDefinitionList := nil;
  try
    ElementDefinitionList := TStringList.Create;
    ElementsWithAttributes := TStringList.Create;
    AttributeDefinitionList := TStringList.Create;
    Extractdtdparts(Dtd, '<!ELEMENT', '>', ElementDefinitionList);
    Extractdtdparts(Dtd, '<!ATTLIST', '>', AttributeDefinitionList);
    PreparseAttributeDefinitions(AttributeDefinitionList, ElementsWithAttributes);
    ParseElementDefinitions(ElementDefinitionList, ElementsWithAttributes);
    ParseAttributeDefinitions(AttributeDefinitionList);
  finally
    ElementDefinitionList.Free;
    AttributeDefinitionList.Free;
    ElementsWithAttributes.Free;
  end;
end;

function TDTDImportTranslator.PreParseChildElements(Str: string; var Suffix: string; var GroupType: string ): string;
var
  I: integer;
  Ch: Char;
  More: boolean;
  Suffix1, GroupType1: string;
begin
  GroupType := '';
  Suffix := '';

  Str := Tail(Str, '('); //Remove beginning parenthesis
  More := True;
  while More do
  begin
    for I := 1 to Length(Str) do
    begin
      Ch := Str[I];
      if Ch = ')' then
      begin  //Terminating
        Str := Tail(Str, Ch);
        if Length(Str)> 0 then
        begin
           Ch := Str[1]; //Should ignore whitespaces
           if Ch in ['?','*','+'] then
           begin
              Suffix := Ch;
              Str := Tail(Str, Ch);
           end;
        end;
        More := False;
        break;
      end else
      if Ch = '(' then
      begin
        Str := PreParseChildElements(Str, Suffix1, GroupType1);
        break;
      end else
      if Ch = '|' then
      begin
        GroupType := '|';
        Str := Tail(Str, '|');
        break;
      end else
      if Ch = ',' then
      begin
        GroupType := ',';
        Str := Tail(Str, ',');
        break;
      end;
    end; //for I
    if Length(Str) = 0 then
       More := False;
  end; //while
  Result := Str;
end;

procedure TDTDImportTranslator.ParseElementDefinitions(ElementList: TStrings;
  ElementsWithAttributes: TStrings);
var
  I: Integer;
  ElementStr: string;
  Name, XsdType: string;
  PcData: Boolean;
  ComplexType: IXMLComplexTypeDef;
  HasAttributes: Boolean;
  IsEmpty, IsAny: Boolean;
begin
  for I := 0 to ElementList.Count-1 do
  begin
    ElementStr := ElementList[I];
    //ElementStr := Replace(ElementStr, '(', ' (');
    Name := ReadName(ElementStr, ElementStr);
    if ElementsWithAttributes.IndexOf(Name) <> -1 then
      HasAttributes := True else HasAttributes := False;
    if Name='' then
      continue
    else
    begin
      if Pos('EMPTY', ElementStr)> 0 then IsEmpty := True else IsEmpty := False;
      if Pos('ANY', ElementStr)> 0 then IsAny := True else IsAny := False;
      if (Pos('(', ElementStr) > 0) or (IsEmpty) or (IsAny) then
      begin
        if Pos('#PCDATA', ElementStr) > 0 then PcData := True else PcData := False;
        if ((PcData) or (IsEmpty)) and (not HasAttributes) then    //and no attributes
        begin
          SchemaDef.ElementDefs.Add(Name, Name+'Type');
          XsdType := xsdString;
          SchemaDef.SimpleTypes.Add(Name+'Type', XsdType);
        end
        else
        begin
          SchemaDef.ElementDefs.Add(Name, Name+'Type');
          ComplexType := SchemaDef.ComplexTypes.Add(Name+'Type');
          ParseChildElements(ComplexType, nil, ElementStr);
        end;
      end
      else
      begin
        SchemaDef.ElementDefs.Add(Name);
      end;
    end;
  end;
end;

procedure TDTDImportTranslator.ParseTokens(Str: string; Tokens: TStrings);
var
  I: Integer;
begin
  Tokens.Clear;
  Split(Str, ''#10, Tokens); // Replace end-of-line chars
  Str := Join(Tokens, ' ');
  Split(Str, ''#13, Tokens);
  Str := Join(tokens, ' ');
  Split(Str, ''#09, Tokens);
  Str := Join(Tokens, ' ');
  Split(Str, ' ', Tokens); //Divide using spaces
  for I :=0 to Tokens.Count-1 do
    Tokens[I] := Trim(Tokens[I]); // Trim for preceding/succeding spaces
  for I := Tokens.Count-1 downto 0 do        //Remove empty strings
    if Tokens[I]='' then
      Tokens.Delete(I);
end;

//Parse an <!ATTLIST ..> definition (including multiple definitions)
procedure TDTDImportTranslator.ParseAttributeDefinition(AttrStr: string);
var
  K, I, CurToken: Integer;
  ElementName, AttributeName, AttributeSample: string;
  AttributeType, AttrType, AttributeDefault, AttributeFixed, AttributeOptions: string;
  AttrToken, X: string;
  Tokens: TStrings;
  OList: TStrings;
  AttributeRequired, More: Boolean;
  MaxLength: Integer;
  ComplexType: IXMLComplexTypeDef;
  AttributeDef: IXMLAttributeDef;
begin
  Tokens := TStringList.Create;
  try
    AttrStr := Head(AttrStr, '>', X);
    AttrStr := Replace(AttrStr, '(', ' (');
    ParseTokens(AttrStr, Tokens);
    ElementName := Tokens[0];

    ComplexType := SchemaDef.ComplexTypes.FindItem(ElementName+'Type') as IXMLComplexTypeDef;
    if ComplexType = nil then Exit; ///Should not happen

    CurToken := 1;
    More := True;
    while More do
    begin
      AttributeRequired := False;
      AttributeFixed := '';
      AttributeDefault := '';
      AttributeOptions := '';
      AttributeType := 'string';
      if curtoken < tokens.count then
        AttributeName := tokens[curtoken];
      AttributeSample := '';
      MaxLength := 0;
      CurToken := CurToken+1;
      if Tokens.Count > CurToken then
      begin
        AttrType := Tokens[CurToken];
        CurToken := CurToken+1;
        if Pos('(', AttrType)>0 then
        begin
          AttributeOptions := Tail(AttrType, '(');
          while (Pos(')', AttrType) = 0) and (Tokens.Count> CurToken) do
          begin  //Reassemble options string
            AttrType := Trim(Tokens[CurToken]);
            AttributeOptions := AttributeOptions+ AttrType;
            CurToken := CurToken+1;
          end;
          AttributeOptions := Head(AttributeOptions, ')', X);
          OList := TStringList.Create;
          try
            Split(AttributeOptions, '|', OList);
            for I := 0 to OList.Count-1 do
            begin
              AttrType := Trim(OList[I]);
              if AttributeSample = '' then
                AttributeSample := AttrType;
              if Length(AttrType) > MaxLength then MaxLength := Length(AttrType);
              OList[I] := AttrType;
            end;
            AttributeOptions := Join(OList,' ');
          finally
            OList.Free;
          end;
        end
        else
        if AttrType='CDATA' then
          AttributeType := xsdString
        else if AttrType='NOTATION' then
          AttributeType := AttrType
        else
         //could be followed by options-list
          AttributeType := AttrType;
      end
      else
        AttributeType := xsdString;
      if Tokens.Count > CurToken then
      begin
        AttrToken := Tokens[CurToken];
        if AttrToken[1]= '#' then
        begin
          CurToken := CurToken+1;
          if AttrToken= '#REQUIRED' then
            AttributeRequired := True;
          if AttrToken = '#FIXED' then
            AttributeFixed := UnQuote(AttrToken);
          if Tokens.Count > CurToken then
            AttrToken := Tokens[CurToken];
        end;
        if AttrToken[1] in ['"', ''''] then
        begin
          CurToken := CurToken+1;
          AttributeDefault := UnQuote(AttrToken, AttrToken[1]);
        end;
      end;
      if AttributeOptions <> '' then
      begin
        AttributeDef := ComplexType.AttributeDefs.Add(AttributeName, True, AttributeType);
        OList := TStringList.Create;
        try
          Split(AttributeOptions, ' ', OList);
          for K := 0 to OList.Count-1 do
            AttributeDef.DataType.Enumerations.Add(OList[K]);
        finally
          OList.Free;
        end;
      end
      else
        AttributeDef := ComplexType.AttributeDefs.Add(AttributeName, AttributeType);
      if AttributeDefault<> '' then
      begin
        AttributeDef.Default := AttributeDefault;
      end;
      if AttributeRequired then
        AttributeDef.Use := SRequired;
      if AttributeFixed <> '' then
      begin
        AttributeDef.Fixed := AttributeFixed;
      end;
      if Tokens.Count > CurToken then
      begin //special case: Multiple attributes in one declaration
      end
      else
        More := False;
    end; //end while
  finally
    Tokens.Free;
  end;
end;

procedure TDTDImportTranslator.ParseAttributeDefinitions(AttributeList: TStrings);
var
  I: Integer;
begin
  for I := 0 to AttributeList.Count-1 do
     ParseAttributeDefinition(AttributeList[I]);
end;

procedure TDTDImportTranslator.PreparseAttributeDefinitions(AttributeList: TStrings; AttrDefs: TStrings);
var
  I: Integer;
  AttrStr: String;
  Tokens: TStrings;
  ElementName, X: String;
begin
  AttrDefs.Clear;
  for I := 0 to AttributeList.Count -1 do
  begin
    Tokens := TStringList.Create;
    try
      AttrStr := Head(AttributeList[I], '>', X);
      ParseTokens(AttrStr, Tokens);
      ElementName := Tokens[0];
      if AttrDefs.IndexOf(ElementName) <> -1 then Continue;
      AttrDefs.Add(ElementName);
    finally
      Tokens.Free;
    end;
  end;
end;

//Eats the string s, and generates a list of child-element names
procedure TDTDImportTranslator.ParseChildElements(ComplexDef: IXMLComplexTypeDef;
  ParentCompositors: IXMLElementCompositors; var Str: string);

  function RemoveSuffix(var name: string; RemoveAll: Boolean): Char;
  var
    X: string;
    Ch: Char;
  begin
    if Name = '' then
    begin
      Result:= ' ';
      exit;
    end;

    Ch := Name[Length(Name)];
    if Ch in ['?','*','+','!', ')'] then
    begin
      if (Ch= ')') and (RemoveAll) then
        Name := Trim(Head(Name, '(', X))
      else
        Name := Trim(Head(Name, Ch, X));
    end
    else if Pos('#', Name) > 0 then
       Name := Trim(Head(Name, '#', X))
    else if Pos('[', Name) > 0 then
       Name := Trim(Head(Name, '#', X))
    else
      Ch := ' ';
    Result := Ch;
  end;

  function IncStrProp(const Val: string): string;
  begin
    if Val[1] in ['0'..'9'] then
      Result := IntToStr(StrToInt(Val) + 1)
    else
      Result := Val;
  end;

var
  I: integer;
  Name: string;
  Ch: Char;
  More: boolean;
  ElementDefs: IXMLElementDefs;
  ElementDef: IXMLElementDef;
  Compositor: IXMLElementCompositor;
  Suffix, GroupType: string;
  NameSuffix: Char;
  CompType: TCompositorType;
  CountChildNames: Integer;
begin
  CountChildNames := 0;
  PreParseChildElements(Str, Suffix, GroupType);
  if GroupType = '|' then
    CompType := ctChoice else
    CompType := ctSequence;

  if (GroupType <> '') and ((Suffix <> '') or (GroupType = '|') or
     (ComplexDef = nil)) then
  begin
    if ComplexDef = nil then
      Compositor := ParentCompositors.Add(CompType)
    else
      Compositor := ComplexDef.ElementCompositors.Add(CompType);
    if (Suffix <> '') and (Compositor <> nil) then
    begin
      if (Suffix = '?') or (Suffix = '*') then
         Compositor.MinOccurs := '0';
      if (Suffix = '*') or (Suffix = '+') then
         Compositor.MaxOccurs := 'unbounded';
    end;
    ElementDefs := Compositor.ElementDefs;
  end
  else
  begin
    if ComplexDef <> nil then
    begin
      ElementDefs := ComplexDef.ElementDefs;
      Compositor := nil;
    end;
  end;

  Str := Tail(Str, '('); //Remove beginning parenthesis
  More := True;
  while More do
  begin
    for I := 1 to Length(Str) do
    begin
      Ch := Str[I];
      if (Ch = ')') or (Ch = '|') or (Ch = ',') then
      begin  //Terminating
        Name := Trim(Head(Str, Ch, Str));
        NameSuffix := RemoveSuffix(Name, True);
        if (Name <> '') and (Name <> '#PCDATA') and Assigned(ElementDefs) then
        begin ///
          ElementDef := ElementDefs.Find(Name);
          if not Assigned(ElementDef) then
          begin
            ElementDef := ElementDefs.Add(Name);
            CountChildNames := CountChildNames + 1;
            if (Ch = ')') and (CountChildNames = 1) and ( not (NameSuffix in ['?','*','+'])) then
            begin  // (x)*
              if Length(Str) > 0 then
              begin
                NameSuffix := Str[1];
                Str := Tail(Str, NameSuffix);
              end
              else NameSuffix := ' ';
            end;
            if NameSuffix in ['?','*','+'] then
            begin
              if (NameSuffix = '?') or (NameSuffix = '*') then
                ElementDef.MinOccurs := '0';
              if (NameSuffix = '*') or (NameSuffix = '+') then
                ElementDef.MaxOccurs := 'unbounded';
            end;
          end
          else
          begin
            // Duplicate entries i.e. (StreetAddress, StreetAddress?, City ...
            if NameSuffix = '' then
            begin
              ElementDef.MinOccurs := IncStrProp(ElementDef.MinOccurs);
              ElementDef.MaxOccurs := IncStrProp(ElementDef.MaxOccurs);
            end
            else if (NameSuffix = '*') or (NameSuffix = '+') then
            begin
              ElementDef.MinOccurs := IncStrProp(ElementDef.MinOccurs);
              ElementDef.MaxOccurs := 'unbounded';
            end
            else if (NameSuffix = '?') then
              ElementDef.MaxOccurs := IncStrProp(ElementDef.MaxOccurs);
          end;
        end; ///
        if Ch = ')' then
          More := False;
        break;
      end else
      if Ch = '(' then
      begin
        if Compositor <> nil then
          ParseChildElements(nil, Compositor.Compositors, Str)
        else
          ParseChildElements(ComplexDef, nil, Str);
        break;
      end;
    end; //for I
    if Length(Str) = 0 then
      More := False;
  end; //while
end;

var
  TranslatorFactory: IXMLSchemaTranslatorFactory;
initialization
  TranslatorFactory := TXMLSchemaTranslatorFactory.Create(TDTDImportTranslator,
    nil, DTDExtension, SDTDTransDesc);
  RegisterSchemaTranslator(TranslatorFactory);
finalization
  UnRegisterSchemaTranslator(TranslatorFactory);
end.

