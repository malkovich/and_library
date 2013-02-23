
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{       XML Schema Tags                                 }
{                                                       }
{ Copyright (c) 2001-2002 Borland Software Corporation  }
{                                                       }
{*******************************************************}

unit XMLSchemaTags;

interface

const

{ XML Schema Tag/Attribute names and values (do not localize) }

  SAbstract = 'abstract';
  SAll = 'all';
  SAnnotation = 'annotation';
  SAny = 'any';
  SAppInfo = 'appinfo';
  SAttribute = 'attribute';
  SAttributeFormDefault = 'attributeFormDefault';
  SAttributeGroup = 'attributeGroup';
  SBase = 'base';
  SBase64 = 'base64';
  SBlockDefault = 'blockDefault';
  SChoice = 'choice';
  SComplexContent = 'complexContent';
  SComplexType = 'complexType';
  SDefault = 'default';
  SDerivedBy = 'derivedBy';
  SDocumentation = 'documentation';
  SElement = 'element';
  SElementFormDefault = 'elementFormDefault';
  SExtension = 'extension';
  SFinal = 'final';
  SFinalDefault = 'finalDefault';
  SFixed = 'fixed';
  SGroup = 'group';
  SHex = 'hex';
  SImport = 'import';
  SInclude = 'include';
  SItemType = 'itemType';
  SList = 'list';
  SMaxOccurs = 'maxOccurs';
  SMemberTypes = 'memberTypes';
  SMinOccurs = 'minOccurs';
  SMixed = 'mixed';
  SName = 'name';
  SNotation = 'notation';
  SOptional = 'optional';
  SPublic = 'public';
  SProhibited = 'prohibited';
  SQualified = 'qualified';
  SRef = 'ref';
  SRequired = 'required';
  SRestriction = 'restriction';
  SSchema = 'schema';
  SSchemaLocation = 'schemaLocation';
  SSequence = 'sequence';
  SSimpleContent = 'simpleContent';
  SSimpleType = 'simpleType';
  SSource = 'source';
  SSystem = 'system';
  STargetNamespace = 'targetNamespace';
  SType = 'type';
  SUnbounded = 'unbounded';
  SUnqualified = 'unqualified';
  SUse = 'use';
  SUnion = 'union';
  SValue = 'value';
  SVersion = 'version';

{ Built-in Data Types (Primative) }

  xsdString = 'string';
  xsdBoolean = 'boolean';
  xsdDecimal = 'decimal';
  xsdFloat = 'float';
  xsdDouble = 'double';
  xsdDuration = 'duration';
  xsdDateTime = 'dateTime';
  xsdTime = 'time';
  xsdDate = 'date';
  xsdGYearMonth = 'gYearMonth';
  xsdGYear = 'gYear';
  xsdGMonthDay = 'gMonthDay';
  xsdGDay = 'gDay';
  xsdGMonth = 'gMonth';
  xsdHexBinary = 'hexBinary';
  xsdBase64Binary = 'base64Binary';
  xsdAnyUri = 'anyURI';
  xsdQName = 'QName';
  xsdNOTATION = 'NOTATION';

{ Built-in Data Types (Derived) }

  xsdNormalizedString = 'normalizedString';
  xsdToken = 'token';
  xsdLanguage = 'language';
  xsdIDREFS = 'IDREFS';
  xsdENTITIES = 'ENTITIES';
  xsdNMTOKEN = 'NMTOKEN';
  xsdNMTOKENS = 'NMTOKENS';
  xsdName = 'Name';
  xsdNCName = 'NCName';
  xsdID = 'ID';
  xsdIDREF = 'IDREF';
  xsdENTITY = 'ENTITY';
  xsdInteger = 'integer';
  xsdNonPositiveInteger = 'nonPositiveInteger';
  xsdNegativeInteger = 'negativeInteger';
  xsdLong = 'long';
  xsdInt = 'int';
  xsdShort = 'short';
  xsdByte = 'byte';
  xsdNonNegativeInteger = 'nonNegativeInteger';
  xsdUnsignedLong = 'unsignedLong';
  xsdUnsignedInt = 'unsignedInt';
  xsdUnsignedShort = 'unsignedShort';
  xsdUnsignedByte = 'unsignedByte';
  xsdPositiveInteger = 'positiveInteger';

{ Legacy Built-in Data Types (pre 2001) }

  xsdTimeDuration = 'timeDuration';             { duration }
  xsdBinary = 'binary';                         { hexBinary }
  xsdUriReference = 'uriReference';             { anyURI }
  xsdTimeInstant = 'timeInstant';               { dateTime }
  xsdRecurringDate= 'recurringDate';            { gMonthDay }
  xsdRecurringDay = 'recurringDay';             { gDay }
  xsdMonth = 'month';                           { gMonth }
  xsdYear = 'year';                             { gYear }
  xsdTimePeriod = 'timePeriod';                 { removed }
  xsdRecurringDuration = 'recurringDuration';   { removed }
  xsdCentury = 'century';                       { removed }

  xsdanySimpleType = 'anySimpleType';
  xsdanyType = 'anyType';

{ Datatype Facets }

  xsfOrdered = 'ordered';
  xsfBounded = 'bounded';
  xsfCardinality = 'cardinality';
  xsfNumeric = 'numeric';

  xsfLength = 'length';
  xsfMinLength = 'minLength';
  xsfMaxLength = 'maxLength';
  xsfPattern = 'pattern';
  xsfEnumeration = 'enumeration';
  xsfMaxInclusive = 'maxInclusive';
  xsfMaxExclusive = 'maxExclusive';
  xsfMinInclusive = 'minInclusive';
  xsfMinExclusive = 'minExclusive';
  xsfWhitespace = 'whiteSpace';
  xsfTotalDigits = 'totalDigits';
  xsfFractionalDigits = 'fractionalDigits';

{ Other strings }

  BuiltInTypeNames: array[0..45] of WideString = (xsdString, xsdBoolean,
    xsdDecimal, xsdFloat, xsdDouble, xsdDuration, xsdDateTime, xsdTime,
    xsdDate, xsdGYearMonth, xsdGYear, xsdGMonthDay, xsdGDay, xsdGMonth,
    xsdHexBinary, xsdBase64Binary, xsdAnyUri, xsdQName, xsdNOTATION,
    xsdNormalizedString, xsdToken, xsdLanguage, xsdIDREFS, xsdENTITIES,
    xsdNMTOKEN, xsdNMTOKENS, xsdName, xsdNCName, xsdID, xsdIDREF, xsdENTITY,
    xsdInteger, xsdNonPositiveInteger, xsdNegativeInteger, xsdLong, xsdInt,
    xsdShort, xsdByte, xsdNonNegativeInteger, xsdUnsignedLong, xsdUnsignedInt,
    xsdUnsignedShort, xsdUnsignedByte, xsdPositiveInteger, xsdanySimpleType,
    xsdanyType);

  LegacyTypeNames: array[0..10] of WideString = (xsdTimeDuration, xsdBinary,
    xsdUriReference, xsdTimeInstant, xsdRecurringDate, xsdRecurringDay, xsdMonth,
    xsdYear, xsdTimePeriod, xsdRecurringDuration, xsdCentury);

implementation

end.

