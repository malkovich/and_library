{===============================================================}
{ 函数 : RESULTSTRING = HexToBin(HEXSTRING)
{ 目的 : 把十六进制字符串转换为二进制字符串
{
{===============================================================}
{ 函数 : RESULTINTEGER = HexCharToInt(HEXCHAR)
{ 目的 : 转换一个十六进制字符为整数
{===============================================================}
{ 函数 : RESULTSTRING = HexCharToBin(HEXCHAR)
{ 目的 : 转换一个十六进制字符为二进制字符串
{===============================================================}
{ 函数 : RESULTINTEGER = Pow(BASE,POWER)
{ 目的 : 指数函数
{===============================================================}
{ 函数 : RESULTINTEGER = BinStrToInt(BINSTRING)
{ 目的 : 把二进制字符串转换为整数
{===============================================================}
{ 函数 : RESULTSTRING = DecodeSMS7Bit (PDUSTRING)
{ 目的 : 解码一个7-bit SMS (GSM 03.38) 为ASCII码
{===============================================================}
{ 函数 : RESULTSTRING = ReverseStr (SOURCESTRING)
{ 目的 : 反转一个字符串
{===============================================================}
{===============================================================}
{ 函数 : RESULTSTRING = UniCode2Gb (SOURCESTRING)
{ 目的 : 将UniCode字符串转换为GB
{===============================================================}
{===============================================================}
{ 函数 : RESULTSTRING = GB2UniCode (SOURCESTRING)
{ 目的 : 将GB字符串转换为UniCode
{===============================================================}

unit BinHexTools;
interface

function HexToBin(HexNr : String): String;
function HexCharToInt(HexToken : char): Integer;
function HexCharToBin(HexToken : char): String;
function pow(base, power: integer): integer;
function BinStrToInt(BinStr : String) : Integer;
function DecodeSMS7Bit(PDU : String): String;
function ReverseStr(SourceStr : String) : String;
function GB2UniCode(GB:string): String;
function UniCode2GB(S : String): String;

implementation

uses sysutils, dialogs;


function HexCharToInt(HexToken : char):Integer;
begin
//if HexToken>#97 then HexToken:=Chr(Ord(HexToken)-32);
{ 将小写字母转换成大写 }

Result:=0;

if (HexToken>#47) and (HexToken<#58) then { chars 0....9 }
Result:=Ord(HexToken)-48
else if (HexToken>#64) and (HexToken<#71) then { chars A....F }
Result:=Ord(HexToken)-65 + 10;
end;


function HexCharToBin(HexToken : char): string;
var DivLeft : integer;
begin
DivLeft:=HexCharToInt(HexToken); { first HexChar->Int }
Result:='';
{ Use reverse dividing }
repeat { Trick; divide by 2 }
if Odd(DivLeft) then { result = odd ? then bit = 1 }
Result:='1'+Result { result = even ? then bit = 0 }
else
Result:='0'+Result;

DivLeft:=DivLeft div 2; { keep dividing till 0 left and length = 4 }
until (DivLeft=0) and (length(Result)=4); { 1 token = nibble = 4 bits }
end;


function HexToBin(HexNr : string): string;
{ only stringsize is limit of binnr }
var Counter : integer;
begin
Result:='';

for Counter:=1 to length(HexNr) do
Result:=Result+HexCharToBin(HexNr[Counter]);
end;


function pow(base, power: integer): integer; //指数base^power
var counter : integer;
begin
Result:=1;

for counter:=1 to power do
Result:=Result*base;
end;


function BinStrToInt(BinStr : string) : integer;
var counter : integer;
begin
if length(BinStr)>16 then
raise ERangeError.Create(#13+BinStr+#13+
'不是一个有效的16Bit二进制单元'+#13);

Result:=0;

for counter:=1 to length(BinStr) do
if BinStr[Counter]='1' then
Result:=Result+pow(2,length(BinStr)-counter);
end;


function DecodeSMS7Bit(PDU : string):string;
var OctetStr : string;
OctetBin : string;
Charbin : string;
PrevOctet: string;
Counter : integer;
Counter2 : integer;
begin
PrevOctet:='';
Result:='';

for Counter:=1 to length(PDU) do
begin
if length(PrevOctet)>=7 then { if 7 Bit overflow on previous }
begin
if BinStrToInt(PrevOctet)<>0 then
Result:=Result+Chr(BinStrToInt(PrevOctet))
else Result:=Result+' ';

PrevOctet:='';
end;

if Odd(Counter) then { only take two nibbles at a time }
begin
OctetStr:=Copy(PDU,Counter,2);
OctetBin:=HexToBin(OctetStr);

Charbin:='';
for Counter2:=1 to length(PrevOctet) do
Charbin:=Charbin+PrevOctet[Counter2];

for Counter2:=1 to 7-length(PrevOctet) do
Charbin:=OctetBin[8-Counter2+1]+Charbin;

if BinStrToInt(Charbin)<>0 then Result:=Result+Chr(BinStrToInt(CharBin))
else Result:=Result+' ';

PrevOctet:=Copy(OctetBin,1,length(PrevOctet)+1);
end;
end;
end;


function ReverseStr(SourceStr : string) : string;
var Counter : integer;
begin
Result:='';

for Counter:=1 to length(SourceStr) do
Result:=SourceStr[Counter]+Result;
end;


function GB2UniCode(GB:string):string;
var
s: string;
i, j, k: integer;
a: array [1..160] of char;
begin
s:='';
StringToWideChar(GB, @(a[1]), 500);
i:=1;
while ((a[i]<>#0) or (a[i+1]<>#0)) do begin
j:=Integer(a[i]);
k:=Integer(a[i+1]);
s:=s+Copy(Format('%X ',[k*$100+j+$10000]) ,2,4);
//S := S + Char(k)+Char(j);
i:=i+2;
end;
Result:=s;
end;


function UniCode2GB(S : String):String;
Var I: Integer;
begin
I := Length(S);
while I >=4 do begin
try
Result :=WideChar(StrToInt('$'+S[I-3]+S[I-2]+S[I-1]+S[I]))+ Result;
except end;
I := I - 4;
end;
end;


end.
