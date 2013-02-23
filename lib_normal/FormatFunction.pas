unit FormatFunction;

interface
uses
   Windows,SysUtils,StrUtils,dialogs,classes;


function FormatPacketSLToPChar (Input: TStrings; Out OutBuffer: PChar): Integer;   

procedure FormatToPchar(inputStringList:TStrings; var OuputPchar:pchar;var lens:integer;var Isok:boolean);
Procedure PcharToFormatedViewNull (Buffer: PChar; BufLen: Integer; Var PrintSL: TStringList);
Procedure PcharToFormatedViewSimple (Buffer: PChar; BufLen: Integer; BeginCount: DWORD; Var PrintSL: TStringList);
Procedure PcharToFormatedView (Buffer: PChar; BufLen: Integer; BeginCount: DWORD; Var PrintSL: TStringList);
Procedure PcharToFormatedViewUtf8 (Buffer: PChar; BufLen: Integer; BeginCount: DWORD; Var PrintSL: TStringList);
Procedure PcharToFormatedViewBoth (Buffer: PChar; BufLen: Integer; BeginCount: DWORD; Var PrintSL: TStringList);
function PcharToPacketViewStr(buf:pchar; len:word; BeginLine: Integer = -1):string;

//==============================================================
function FormatIt(str:string; var IsOk:boolean):string;

function FormatItLeft(finalstr:string):string;
function FormatItRight(finalstr:string):string;
function CheckConstructure(finalstr:string):integer;
function CutBlank(finalstr:string):string;

function  PacketStrToPchar(s:string;var len:integer):pchar; //记住释放内存
function  PcharToPacketStr(buf:pchar; len:word):string;

implementation



function FormatPacketSLToPChar (Input: TStrings; Out OutBuffer: PChar): Integer;
var
  ModifySL: TStringList;
  LineStr, LineItar: String;
  TabIndex: Integer;
  IsOk: Boolean;
begin
  Result := 0;
  ModifySL := TStringList.Create;

  for LineItar in Input do
  begin
     LineStr := Trim(LineItar);
     if Length (LineStr) < 2 then continue;
     TabIndex := Pos (#9, LineStr);

     if TabIndex > 1 then
       LineStr := Copy (LineStr, TabIndex+1, Length(LineStr)-TabIndex);
     LineStr := Trim (LineStr);

     TabIndex := Pos (#9, LineStr);
     if TabIndex > 1 then
     begin
       LineStr := Copy (LineStr, 1, TabIndex - 1);
       LineStr := Trim (LineStr);
     end;

     ModifySL.Add(LineStr);
  end;

  FormatToPchar (ModifySL, OutBuffer, Result, IsOK);

  ModifySL.Free;
end;

function PcharToPacketViewStr(buf:pchar; len:word; BeginLine: Integer = -1):string;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  PcharToFormatedView (buf, Len, BeginLine, SL);
  Result := SL.Text;
  SL.Free;
end;

function PcharToPacketStr(buf:pchar; len:word):string;
var
  i:integer;
begin
  result :='';
  for i:=0 to len-1 do
    result:=result+inttohex(ord(buf[i]), 2)+' ';
end;

function IsHex(s:string):boolean;
begin
  s:=UpperCase(s);
  if ((s='0') or (s='1') or (s='2') or (s='3') or (s='4') or (s='5') or
      (s='6') or (s='7') or (s='8') or (s='9') or (s='A') or (s='B') or
      (s='C') or (s='D') or (s='E') or (s='F')) then
      result:=true
  else
      result:=false;
end;

function FormatItLeft(finalstr:string):string;
var
  i:integer;
begin
  finalstr := Trim (finalstr);
  if finalstr='' then exit;

  if (length(finalstr)>2) then
  begin
     finalstr:=UpperCase(finalstr);
     if ((finalstr[1]<>' ') and (IsHex(finalstr[1])) and
         (finalstr[2]<>' ') and (IsHex(finalstr[2])) and  (finalstr[3]=' ')) then
        begin
           finalstr:=trim(finalstr);
        end
     else
       begin
           if strpos(pchar(finalstr),' ')<>nil then
              begin
                finalstr:=trim( strpas(strpos(pchar(finalstr),' ')));
                if not ((finalstr[1]<>' ') and (IsHex(finalstr[1])) and
                       (finalstr[2]<>' ') and (IsHex(finalstr[2])) and  (finalstr[3]=' ')) then
                    begin
                      finalstr:=FormatItLeft(finalstr);
                    end;
              end
          else
             begin
               result:=' ';
               exit;
             end;
       end;
  end
  else
  begin
      finalstr:=UpperCase(finalstr);
      for i:=1 to length(finalstr) do
         if not (IsHex(finalstr[i])) then
             finalstr:=' ';

  end;

  result:=finalstr;
end;


function FormatItRight(finalstr:string):string;  //检查字符串右侧最多8个字符的合法性
var                                             //不合法则切割之。
   s:pchar;
   sCut:string;
   len:integer;
   function CutTheLen( str:string;len:integer):string;
   begin
      Setlength( str,(length(str)-len));
      str:=trim(str);
      result:=str;
   end;
begin
    if finalstr='' then exit;
    result:=' ';
    if (finalstr<>' ') then
    begin
       finalstr:=UpperCase(trim(finalstr));  //全部转为大写
       s:=strpos(pchar(finalstr), ' ');   // 切割第一个空格前
       if s=nil then exit;
       while s <> nil do              //循环切割到最后一个空格
       begin
          s:=pchar(trim(  strpas(s)  ));
          sCut:=strpas(s);            //保存最后一段的字符
          s := strpos(pchar(s), ' ');
       end;
       sCut:=trim(sCut);
       len:=length(sCut);      //取最后一段字符长度以便定位
       if len=2  then
            if ( (IsHex(sCut[1])) and (IsHex(sCut[2]))) then
               begin

                  if length(finalstr)>4 then
                    begin

                       if ( ( finalstr[length(finalstr)-2]=' ')        and
                            ( IsHex(finalstr[length(finalstr)-3]) )  and
                            ( IsHex(finalstr[length(finalstr)-4]) )    ) then
                          begin

                             if length(finalstr)>7 then
                               begin

                                  if ( ( finalstr[length(finalstr)-5]=' ')        and
                                      ( IsHex(finalstr[length(finalstr)-6]) )  and
                                      ( IsHex(finalstr[length(finalstr)-7]) )    ) then
                                     begin
                                       result:=trim(finalstr);
                                       exit;
                                     end
                                  else
                                     begin
                                       finalstr:=CutTheLen(finalstr,8);
                                       result:=FormatItRight(finalstr);
                                     end;

                               end
                             else
                               begin
                                 result:=trim(finalstr);
                                 exit;
                               end;

                          end
                       else
                          begin
                             finalstr:=CutTheLen(finalstr,5);
                             result:=FormatItRight(finalstr);
                          end;

                     end
                   else
                     begin
                             result:=trim(finalstr);
                             exit;
                     end;
               end
            else
               begin
                  finalstr:=CutTheLen(finalstr,2);
                  result:=FormatItRight(finalstr);
               end;


       if len<>2 then
             begin
                  finalstr:=CutTheLen(finalstr,len);
                  result:=FormatItRight(finalstr);
             end;
    end;
end;

function CutBlank(finalstr:string):string;
var
   i:integer;
begin
   i:=1;
   if finalstr<>'' then
   begin
     repeat
        if (finalstr[i]=' ') and (finalstr[i+1]=' ') then
           finalstr:=trim(leftstr(finalstr,i))+' '+trim(rightstr(finalstr,length(finalstr)-i));
        inc(i);
     until i>=length(finalstr);
   end;
   result:=finalstr;
end;

function CheckConstructure(finalstr:string):integer;
var
   i,j:integer;
begin
   result:=0;
   for i:=1 to length(finalstr) do
       if not (IsHex(finalstr[i]) or (finalstr[i]=' ')) then
       begin
          result:=1;
          exit;
       end;
       
   finalstr:=trim(finalstr);
   finalstr:=CutBlank(finalstr);

   if ((length(finalstr)+1) mod 3 )<>0 then
   begin
       result:=2;
       exit;
   end;

   i:=1;
   repeat
       for j:=0 to 1 do
           if not IsHex(finalstr[i+j]) then
           begin
              result:=2;
              exit;
           end;
       inc(i);
       if (i=length(finalstr)) then
          begin
              result:=0;
              exit;
          end;
       inc(i);
       if not (finalstr[i]=' ') then
          begin
              result:=2;
              exit;
          end;
       inc(i);
   until i>=length(finalstr);

end;

function FormatIt(str:string; var IsOk:boolean):string;
var
   s:string;
begin
   result:='';
   s:=trim(str);
   if s='' then exit;
   s:=FormatItLeft(s);
   s:=FormatItRight(s);

   case CheckConstructure(s) of
   0: begin
      s:=CutBlank(s);
      result:=s;
      IsOk:=true;
      end;
   1: begin
      showmessage('有非法字符,请检查！');
      result:=str;
      IsOk:=false;
      end;
   2: begin
      showmessage('关键内容:'+#13+'    "'+str+'"'+#13+'不符合封包格式，请检查并除去！');
      result:=str;
      IsOk:=false;
      end;
   end;

end;
//=================================================
function Hexstrtoint(str: string):byte;    //将读到的更新地址字符变成整型
    function Todword(charletter:Char ):byte;
    begin
      result:=0;
      case charletter of
       '0':result:=0;     '1':result:=1;     '2':result:=2;
       '3':result:=3;     '4':result:=4;     '5':result:=5;
       '6':result:=6;     '7':result:=7;     '8':result:=8;
       '9':result:=9;     'a':result:=10;     'A':result:=10;
       'b':result:=11;     'B':result:=11;     'c':result:=12;
       'C':result:=12;     'd':result:=13;     'D':result:=13;
       'e':result:=14;     'E':result:=14;     'f':result:=15;
       'F':result:=15;
      end;
    end;
begin
    result:=Todword(str[1])*16
           +Todword(str[2]);
end;

function HexStrtobyte(s:string):byte;
begin
   s:=trim(s);
   result:= Hexstrtoint( s[1]+s[2] );
end;

function PacketStrToPchar(s:string;var len:integer):pchar;
var
   i:integer;
   pcr:pchar;
begin
   s:=trim(s);
   len:= (length(s)+1) div 3;
   pcr := AllocMem (len + 4);
   for i:=0 to (len-1) do
       pcr[i]:=char(HexStrtobyte( s[i*3+1]+ s[i*3+2] ));
   result:=pcr;
end;

procedure FormatToPchar(inputStringList:TStrings;
                        var OuputPchar:pchar;var lens:integer;var Isok:boolean);
var
   ss1:TstringList;
   i,k,len,lencount:integer;
   _isok:boolean;
   pcharval:pchar;
   resultpcharBuffer: array[0..$4000-1] of Char;
begin
   Isok:=true;
   ss1:=TstringList.Create;

   //将inputStringList字符串数组格式化后放进ss1字符串数组中
   i:=0;
   repeat
      if trim(inputStringList[i])<>'' then
      begin
         ss1.Add(FormatIt(inputStringList[i],_isok));
         if _isok=false then
            Isok:=false;
      end;
      inc(i);
   until i=inputStringList.Count;

   //将ss1字符串数组的字符串，保存进pchar中
   lencount:=0;
   for i:=0 to (ss1.Count-1) do
   begin
       pcharval:=PacketStrToPchar(ss1[i],len);

       if ((lencount+len)<=length(resultpcharBuffer)) then
       begin
         for k:=0 to (len-1) do
           resultpcharBuffer[k+lencount]:=pcharval[k];
         lencount:=lencount+len;
       end;
   end;
   //输出
   lens:=lencount;
   getmem(OuputPchar,lencount);
   for i:=0 to lencount-1 do
       OuputPchar[i]:=resultpcharBuffer[i];
   ss1.Free;
end;


Type
  THexLine = Array[0..15] of Byte;
  THexLineArray = Array[word] of THexLine;
  LPTHexLineArray = ^THexLineArray;


Procedure PcharToFormatedView (Buffer: PChar; BufLen: Integer; BeginCount: DWORD; Var PrintSL: TStringList);
var
  ToPrint: Byte;
  Index, Index2, LineCount, ModCount, TabCount: Integer;
  ViewLine: DWORD;
  Msg, Ref: String;
  Source: LPTHexLineArray absolute Buffer;
begin
  LineCount := BufLen div SizeOf(THexLine);
  ModCount  := BufLen mod SizeOf(THexLine);

  for Index := 0 to LineCount - 1 do
  begin
    Msg := '';
    Ref := '';
    for Index2 := 0 to SizeOf(THexLine) - 1 do
    begin
      ToPrint := Source[Index][Index2];
      if ToPrint > 32 then
        Ref := Ref + Char(ToPrint)
      else
        Ref := Ref + '.';
      Msg := Msg + IntToHex (ToPrint, 2) + ' ';
    end;
    ViewLine := BeginCount + DWORD(Index shl 4);
    Msg := format('%.8X'#9'%s'#9'%s',[ViewLine, Msg, Ref]);
    PrintSL.Add(Msg);
  end;


  if ModCount > 0 then
  begin
    Msg := '';
    Ref := '';
    for Index2 := 0 to ModCount - 1 do
    begin
        ToPrint := Source[LineCount][Index2];
        if ToPrint > 32 then
          Ref := Ref + Char(ToPrint)
        else
          Ref := Ref + '.';
        Msg := Msg + IntToHex (ToPrint, 2) + ' ';
    end;

    TabCount := 6 - ModCount * 3 div 8;
    if ModCount * 3 mod 8 > 0 then
      Inc (TabCount);
    for Index2 := 0 to TabCount - 1 do
      Msg := Msg + #9;

    ViewLine := BeginCount + DWORD(LineCount shl 4);
    Msg := format('%.8X'#9'%s%s',[ViewLine, Msg, Ref]);
    PrintSL.Add(Msg);
  end;
end;

Procedure PcharToFormatedViewBoth (Buffer: PChar; BufLen: Integer; BeginCount: DWORD; Var PrintSL: TStringList);
var
  ToPrint: Byte;
  Index, Index2, LineCount, ModCount, TabCount: Integer;
  ViewLine: DWORD;
  Msg, RefUtf8, RefAnsi: String;
  Source: LPTHexLineArray absolute Buffer;
begin
  LineCount := BufLen div SizeOf(THexLine);
  ModCount  := BufLen mod SizeOf(THexLine);

  for Index := 0 to LineCount - 1 do
  begin
    Msg := '';
    RefUtf8 := '';
    for Index2 := 0 to SizeOf(THexLine) - 1 do
    begin
      ToPrint := Source[Index][Index2];
      if ToPrint > 32 then
        RefUtf8 := RefUtf8 + Char(ToPrint)
      else
        RefUtf8 := RefUtf8 + '.';
      Msg := Msg + IntToHex (ToPrint, 2) + ' ';
    end;
    ViewLine := BeginCount + DWORD(Index shl 4);
    RefAnsi := Utf8ToAnsi (RefUtf8);
    Msg := IntToHex(ViewLine, 8) + '    ' + Msg + '   ' + RefUtf8 + ' '#9 + RefAnsi;
    PrintSL.Add(Msg);
  end;


  if ModCount > 0 then
  begin
    Msg := '';
    RefUtf8 := '';
    for Index2 := 0 to ModCount - 1 do
    begin
        ToPrint := Source[LineCount][Index2];
        if ToPrint > 32 then
          RefUtf8 := RefUtf8 + Char(ToPrint)
        else
          RefUtf8 := RefUtf8 + '.';
        Msg := Msg + IntToHex (ToPrint, 2) + ' ';
    end;

    TabCount := 5 - ModCount * 3 div 8;
    if ModCount * 3 mod 8 > 0 then
      Inc (TabCount);
    for Index2 := 0 to TabCount - 1 do
      Msg := Msg + #9;

    ViewLine := BeginCount + DWORD(LineCount shl 4);
    RefAnsi := Utf8ToAnsi (RefUtf8);
    Msg := IntToHex(ViewLine, 8) + '    ' + Msg + '   ' + RefUtf8 + ' '#9 + RefAnsi;
    PrintSL.Add(Msg);
  end;
end;

Procedure PcharToFormatedViewUtf8 (Buffer: PChar; BufLen: Integer; BeginCount: DWORD; Var PrintSL: TStringList);
var
  ToPrint: Byte;
  Index, Index2, LineCount, ModCount, TabCount: Integer;
  ViewLine: DWORD;
  Msg, RefUtf8, RefAnsi: String;
  Source: LPTHexLineArray absolute Buffer;
begin
  LineCount := BufLen div SizeOf(THexLine);
  ModCount  := BufLen mod SizeOf(THexLine);

  for Index := 0 to LineCount - 1 do
  begin
    Msg := '';
    RefUtf8 := '';
    for Index2 := 0 to SizeOf(THexLine) - 1 do
    begin
      ToPrint := Source[Index][Index2];
      if ToPrint > 32 then
        RefUtf8 := RefUtf8 + Char(ToPrint)
      else
        RefUtf8 := RefUtf8 + '.';
      Msg := Msg + IntToHex (ToPrint, 2) + ' ';
    end;
    ViewLine := BeginCount + DWORD(Index shl 4);
    RefAnsi := Utf8ToAnsi (RefUtf8);
    Msg := IntToHex(ViewLine, 4) + '    ' + Msg + '   ' + RefAnsi;
    PrintSL.Add(Msg);
  end;


  if ModCount > 0 then
  begin
    Msg := '';
    RefUtf8 := '';
    for Index2 := 0 to ModCount - 1 do
    begin
        ToPrint := Source[LineCount][Index2];
        if ToPrint > 32 then
          RefUtf8 := RefUtf8 + Char(ToPrint)
        else
          RefUtf8 := RefUtf8 + '.';
        Msg := Msg + IntToHex (ToPrint, 2) + ' ';
    end;

    TabCount := 5 - ModCount * 3 div 8;
    if ModCount * 3 mod 8 > 0 then
      Inc (TabCount);
    for Index2 := 0 to TabCount - 1 do
      Msg := Msg + #9;

    ViewLine := BeginCount + DWORD(LineCount shl 4);
    RefAnsi := Utf8ToAnsi (RefUtf8);
    Msg := IntToHex(ViewLine, 4) + '    ' + Msg + '   ' + RefAnsi;
    PrintSL.Add(Msg);
  end;
end;


Procedure PcharToFormatedViewSimple (Buffer: PChar; BufLen: Integer; BeginCount: DWORD; Var PrintSL: TStringList);
var
  ToPrint: Byte;
  Index, Index2, LineCount, ModCount: Integer;
  ViewLine: DWORD;
  Msg: String;
  Source: LPTHexLineArray absolute Buffer;
begin
  LineCount := BufLen div SizeOf(THexLine);
  ModCount  := BufLen mod SizeOf(THexLine);

  for Index := 0 to LineCount - 1 do
  begin
    Msg := '';
    for Index2 := 0 to SizeOf(THexLine) - 1 do
    begin
      ToPrint := Source[Index][Index2];
      Msg := Msg + IntToHex (ToPrint, 2) + ' ';
    end;
    ViewLine := BeginCount + DWORD(Index shl 4);
    Msg := IntToHex(ViewLine, 4) + '    ' + Msg;
    PrintSL.Add(Msg);
  end;


  if ModCount > 0 then
  begin
    Msg := '';
    for Index2 := 0 to ModCount - 1 do
    begin
        ToPrint := Source[LineCount][Index2];
        Msg := Msg + IntToHex (ToPrint, 2) + ' ';
    end;

    ViewLine := BeginCount + DWORD(LineCount shl 4);
    Msg := IntToHex(ViewLine, 4) + '    ' + Msg;
    PrintSL.Add(Msg);
  end;
end;

Procedure PcharToFormatedViewNull (Buffer: PChar; BufLen: Integer; Var PrintSL: TStringList);
var
  ToPrint: Byte;
  Index, Index2, LineCount, ModCount: Integer;
  Msg: String;
  Source: LPTHexLineArray absolute Buffer;
begin
  LineCount := BufLen div SizeOf(THexLine);
  ModCount  := BufLen mod SizeOf(THexLine);

  for Index := 0 to LineCount - 1 do
  begin
    Msg := '';
    for Index2 := 0 to SizeOf(THexLine) - 1 do
    begin
      ToPrint := Source[Index][Index2];
      Msg := Msg + IntToHex (ToPrint, 2) + ' ';
    end;
    PrintSL.Add(Trim(Msg));
  end;          

  if ModCount > 0 then
  begin
    Msg := '';
    for Index2 := 0 to ModCount - 1 do
    begin
        ToPrint := Source[LineCount][Index2];
        Msg := Msg + IntToHex (ToPrint, 2) + ' ';
    end;
    PrintSL.Add(Trim(Msg));
  end;
end;


end.

