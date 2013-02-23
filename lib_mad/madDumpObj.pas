// ***************************************************************
//  madDumpObj.pas            version:  1.0   ·  date: 2005-07-09
//  -------------------------------------------------------------
//  converts an object into a tree formatted string
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2005 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2005-07-09 1.0  initial release

unit madDumpObj;

{$I mad.inc}

interface

// ***************************************************************

// returns a tree structure of all properties of the specified object
// the object must be a descendant of a class which was declared with {$M+}
// e.g. TPersistent
function DumpObj (obj: TObject) : string;

// ***************************************************************

implementation

uses TypInfo, SysUtils;

// ***************************************************************

function DumpObj(obj: TObject) : string;

  function DumpObject_(obj: TObject; indent: string) : string;

    function DumpProp(propInfo: PPropInfo) : string;

      function GetSetProp(obj: TObject; propInfo: PPropInfo) : string;
      type TIntegerSet = set of 0..sizeOf(integer) * 8 - 1;
      var s  : TIntegerSet;
          ti : PTypeInfo;
          i1 : Integer;
      begin
        integer(s) := GetOrdProp(obj, propInfo);
        ti := GetTypeData(PropInfo^.PropType^)^.CompType^;
        for i1 := 0 to sizeOf(integer) * 8 - 1 do
          if i1 in s then
            result := result + ', ' + GetEnumName(ti, i1);
        Delete(result, 1, 2);
      end;

    const CTypeKindNames : array [TTypeKind] of string =
      ('Unknown', 'Integer', 'Char', 'Enum', 'Float',
       'ShortString', 'Set', 'Class', 'Method', 'WChar', 'String', 'WideString',
       'Variant', 'Array', 'Record', 'Interface', 'Int64', 'DynArray');
    const CMethodKindNames : array [TMethodKind] of string =
      ('procedure', 'function', 'constructor', 'destructor',
       'class procedure', 'class function',
       {$ifndef ver120}{$ifndef ver130}{$ifndef ver140}{$ifndef ver150}{$ifndef ver160}
         'class constructor', 'operator overload',
       {$endif}{$endif}{$endif}{$endif}{$endif}
       'procedure', 'function');
    var tk    : TTypeKind;
        obj1  : TObject;
        i1    : integer;
        buf   : pointer;
        flags : TParamFlags;
        s1    : string;
        td    : PTypeData;
    begin
      result := '- ' + propInfo^.Name + ': ';
      tk := propInfo^.PropType^^.Kind;
      if tk = tkClass then begin
        td := GetTypeData(propInfo^.PropType^);
        if td <> nil then begin
          obj1 := TObject(GetOrdProp(obj, propInfo));
          if obj1 <> nil then begin
            result[1] := '+';
            result := result + td^.ClassType.ClassName + DumpObject_(obj1, indent + '  ');
          end else
            result := result + td^.ClassType.ClassName + ' = nil';
        end else
          result := result + '???';
      end else
        if tk = tkMethod then begin
          td := GetTypeData(propInfo^.PropType^);
          if td <> nil then begin
            result := result + CMethodKindNames[td^.MethodKind];
            buf := @td^.ParamList;
            result := result + ' (';
            for i1 := 0 to td^.ParamCount - 1 do begin
              flags := TParamFlags(buf^);
              if      pfVar   in flags then result := result + 'var '
              else if pfOut   in flags then result := result + 'out '
              else if pfConst in flags then result := result + 'const ';
              inc(integer(buf));
              result := result + ShortString(buf^);
              if pfArray in flags then
                result := result + 'array of ';
              inc(integer(buf), byte(buf^) + 1);
              s1 := ShortString(buf^);
              if s1 <> '' then
                result := result + ': ' + s1;
              inc(integer(buf), byte(buf^) + 1);
              if i1 < td^.ParamCount - 1 then
                result := result + '; ';
            end;
            result := result + ')';
            s1 := ShortString(buf^);
            if s1 <> '' then
              result := result + ' : ' + s1;
            result := result + ';';
          end else
            result := result + '???';
        end else begin
          result := result + CTypeKindNames[tk];
          case tk of
            tkInteger, tkChar, tkWChar: result := result + ' = '  + IntToStr(GetOrdProp(obj, propInfo));
            tkEnumeration:              result := result + ' = '  + GetEnumName(propInfo^.PropType^, GetOrdProp(obj, propInfo));
            tkSet:                      result := result + ' = [' + GetSetProp(obj, propInfo) + ']';
            tkFloat:                    result := result + ' = '  + FloatToStr(GetFloatProp(obj, propInfo));
            tkString, tkLString:        result := result + ' = "' + GetStrProp(obj, propInfo) + '"';
            {$ifndef ver120}{$ifndef ver130}
              tkWString:                  result := result + ' = "' + GetWideStrProp(obj, propInfo) + '"';
            {$endif}{$endif}
            tkVariant:                  result := result + ' = '  + GetVariantProp(obj, propInfo);
            tkInt64:                    result := result + ' = '  + IntToStr(GetInt64Prop(obj, propInfo));
          end;
        end;
    end;

  var i1       : integer;
      propList : PPropList;
  begin
    result := '';
    if (obj <> nil) and (obj.ClassInfo <> nil) then begin
      i1 := GetPropList(obj.ClassInfo, [low(TTypeKind)..high(TTypeKind)], nil);
      if i1 <> 0 then begin
        GetMem(propList, i1 * sizeOf(pointer));
        for i1 := 0 to GetPropList(obj.ClassInfo, [low(TTypeKind)..high(TTypeKind)], propList) - 1 do
          result := result + #$D#$A + indent + DumpProp(propList[i1]);
        FreeMem(propList);
      end;
    end;
  end;

begin
  result := DumpObject_(obj, '');
  Delete(result, 1, 2);
end;

// ***************************************************************

end.
