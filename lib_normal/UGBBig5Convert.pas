unit UGBBig5Convert;
interface

uses
  Classes, Windows;

var
  conlanguage:boolean;   //true:非gb2312简体中文 false为简体中文

type
  TGBBIG5Convert = class(TObject)
  public
    class function BIG5ToGB(BIG5Str : String): AnsiString;
    class function GBToBIG5(GBStr : String): AnsiString;
  end;
 

implementation

{
******************************** TGBBIG5Convert ********************************
}
class function TGBBIG5Convert.BIG5ToGB(BIG5Str : String): AnsiString;
var
  Len: Integer;
  pBIG5Char: PChar;
  pGBCHSChar: PChar;
  pGBCHTChar: PChar;
  pUniCodeChar: PWideChar;
begin
  //String -> PChar
  pBIG5Char := PChar(BIG5Str);
  Len := MultiByteToWideChar(950,0,pBIG5Char,-1,nil,0);
  GetMem(pUniCodeChar,Len*2);
  ZeroMemory(pUniCodeChar,Len*2);
  //Big5 -> UniCode
  MultiByteToWideChar(950,0,pBIG5Char,-1,pUniCodeChar,Len);
  Len := WideCharToMultiByte(936,0,pUniCodeChar,-1,nil,0,nil,nil);
  GetMem(pGBCHTChar,Len*2);
  GetMem(pGBCHSChar,Len*2);
  ZeroMemory(pGBCHTChar,Len*2);
  ZeroMemory(pGBCHSChar,Len*2);
  //UniCode->GB CHT
  WideCharToMultiByte(936,0,pUniCodeChar,-1,pGBCHTChar,Len,nil,nil);
  //GB CHT -> GB CHS
  LCMapString($804,LCMAP_SIMPLIFIED_CHINESE,pGBCHTChar,-1,pGBCHSChar,Len);
  Result := String(pGBCHSChar);
  FreeMem(pGBCHTChar);
  FreeMem(pGBCHSChar);
  FreeMem(pUniCodeChar);
end;

class function TGBBIG5Convert.GBToBIG5(GBStr : String): AnsiString;
var
  Len: Integer;
  pGBCHTChar: PChar;
  pGBCHSChar: PChar;
  pUniCodeChar: PWideChar;
  pBIG5Char: PChar;
begin
  pGBCHSChar := PChar(GBStr);
  Len := MultiByteToWideChar(936,0,pGBCHSChar,-1,nil,0);
  GetMem(pGBCHTChar,Len*2+1);
  ZeroMemory(pGBCHTChar,Len*2+1);
  //GB CHS -> GB CHT
  LCMapString($804,LCMAP_TRADITIONAL_CHINESE,pGBCHSChar,-1,pGBCHTChar,Len*2);
  GetMem(pUniCodeChar,Len*2);
  ZeroMemory(pUniCodeChar,Len*2);
  //GB CHT -> UniCode
  MultiByteToWideChar(936,0,pGBCHTChar,-1,pUniCodeChar,Len*2);
  Len := WideCharToMultiByte(950,0,pUniCodeChar,-1,nil,0,nil,nil);
  GetMem(pBIG5Char,Len*2);
  ZeroMemory(pBIG5Char,Len*2);
  //UniCode -> Big5
  WideCharToMultiByte(950,0,pUniCodeChar,-1,pBIG5Char,Len,nil,nil);
  Result := String(pBIG5Char);
  FreeMem(pBIG5Char);
  FreeMem(pGBCHTChar);
  FreeMem(pUniCodeChar);
end;

initialization
  conLanguage:=GetUserDefaultLangID<>2052;

finalization

end.


