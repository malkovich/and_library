unit BuildInFunctions;

interface

//内存块处理
procedure FillChar(var Dest; count : Integer; Value : Char);
procedure ZeroMemory (Destination: Pointer; Length: DWORD);
procedure CopyMemory(Dest, Source : Pointer; count : Integer);
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
procedure Move(const Source; var Dest; count : Integer);

//字符处理
function UpCase(ch :Char):Char;
function StrLower(Str: PChar): PChar; assembler;
function StrUpper(Str: PChar): PChar; assembler;
function StrPos(const Str1, Str2: PChar): PChar; assembler;
function StrLCopy(Dest : PChar; const Source : PChar; MaxLen : Cardinal) : PChar; inline;
function StrCopy(Dest: PChar; const Source: PChar): PChar; inline;
function StrSame(Str1, Str2 : PChar) : LongBool;
function StrLen(const Str: PChar): Cardinal; inline;
function StrEnd(const Str: PChar): PChar; inline;
function StrCat(Dest: PChar; const Source: PChar): PChar; inline;
procedure Trim(S:PChar; D:PChar);
function NumStrToInt(Str:PChar):Integer;
procedure LongIntToStr(N: DWORD; var Dist);
function LongIntToStrig (N: DWORD): PChar;
procedure IntToHexStr(Value:DWORD; var Dist; digits:Integer = -1);

//网络函数
function ntohs(netShort :WORD):WORD;
function ntohl(netLong :LongWord):LongWord;
function IsValidEmail(EMail :Pchar): Boolean;


implementation

    
procedure LongIntToStr(N: DWORD; var Dist);
var
  m, Len, I, K: DWORD;
  D: PChar;
  BufA: Array[byte] of char;
begin
  D := @Dist;
  Len := 0;
  While N div 10 > 0 do
  begin
    m := N mod 10;
    BufA[Len] := Char(m + Ord('0'));
    N   :=   N   div   10;
    inc(Len);
  end;
  m := N mod 10;
  BufA[Len] := Char(m + Ord('0'));
  inc(Len);
  BufA[Len] := #0;

  K := 0;
  for I := Len - 1 downto 0 do
  begin
    D[K] := BufA[I];
    INC(K);
  end;
  D[K] := #0;
end;

var
  IntToStrBuff: array[0..15] of char;

function LongIntToStrig (N: DWORD): PChar;
begin
  LongIntToStr (N, IntToStrBuff[0]);
  Result := @IntToStrBuff[0];
end;

const
  c_HexStr :array[0..$F] of char = '0123456789ABCDEF';

function ByteToChr(b: byte): char;
begin
  result := c_HexStr[b mod 16];
end;

procedure MakeIntToHex(value:DWORD; var Dist);
var
  I: Integer;
  val: byte;
  Output: PChar;
begin
  Output := @Dist;
  for I := 7 downto 0 do
  begin
    val := Value and $0000000F;
    Output[I] := ByteToChr(val);
    value := value shr 4;
  end;
  Output[8] := #0;
end;

procedure IntToHexStr(Value:DWORD; var Dist; digits:Integer=-1);
var
  I, Index: Integer;
  Output: PChar;
begin
  Output := @Dist;
  if digits = -1 then
  begin
    MakeIntToHex(Value, Output[0]);

    Index := 0;
    for I := 0 to 7 do
      if Output[I] <> '0' then
      begin
        Index := I;
        Break;
      end;

    if Index > 0 then
      for I := Index to 8 do
        Output[I - Index] := Output[I];

    exit;
  end;

  if digits > 8 then exit;
  MakeIntToHex(Value, Output[0]);
  if digits > 0 then
  begin
    digits := 8 - digits;
    for I := digits to 8 do
      Output[I - digits] := Output[I];
  end;
end;

function NumStrToInt(Str:PChar):Integer;
var
  Length: Integer;
  I, K, Value, Count: Integer;
begin
  Result := 0;
  Length := StrLen(Str);
  Count := 0;
  for I := Length - 1 downto 0 do
  begin
    Value := ORD(Str[I]) - $30;
    for K := 1  to Count do
      Value := Value * 10; 
    Result := Result + Value;
    INC(Count);
  end;
end;

function StrPos(const Str1, Str2: PChar): PChar; assembler;
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        OR      EAX,EAX
        JE      @@2
        OR      EDX,EDX
        JE      @@2
        MOV     EBX,EAX
        MOV     EDI,EDX
        XOR     AL,AL
        MOV     ECX,0FFFFFFFFH
        REPNE   SCASB
        NOT     ECX
        DEC     ECX
        JE      @@2
        MOV     ESI,ECX
        MOV     EDI,EBX
        MOV     ECX,0FFFFFFFFH
        REPNE   SCASB
        NOT     ECX
        SUB     ECX,ESI
        JBE     @@2
        MOV     EDI,EBX
        LEA     EBX,[ESI-1]
@@1:    MOV     ESI,EDX
        LODSB
        REPNE   SCASB
        JNE     @@2
        MOV     EAX,ECX
        PUSH    EDI
        MOV     ECX,EBX
        REPE    CMPSB
        POP     EDI
        MOV     ECX,EAX
        JNE     @@1
        LEA     EAX,[EDI-1]
        JMP     @@3
@@2:    XOR     EAX,EAX
@@3:    POP     EBX
        POP     ESI
        POP     EDI
end;

function StrUpper(Str: PChar): PChar; assembler;
asm
        PUSH    ESI
        MOV     ESI,Str
        MOV     EDX,Str
@@1:    LODSB
        OR      AL,AL
        JE      @@2
        CMP     AL,'a'
        JB      @@1
        CMP     AL,'z'
        JA      @@1
        SUB     AL,20H
        MOV     [ESI-1],AL
        JMP     @@1
@@2:    XCHG    EAX,EDX
        POP     ESI
end;

function StrLower(Str: PChar): PChar; assembler;
asm
        PUSH    ESI
        MOV     ESI,Str
        MOV     EDX,Str
@@1:    LODSB
        OR      AL,AL
        JE      @@2
        CMP     AL,'A'
        JB      @@1
        CMP     AL,'Z'
        JA      @@1
        ADD     AL,20H
        MOV     [ESI-1],AL
        JMP     @@1
@@2:    XCHG    EAX,EDX
        POP     ESI
end;


function CheckAllowed(s:PChar): Boolean;
var
  i:Integer;
  Length :Integer;
begin
  result := False;
  Length := StrLen(s);
  for i := 0 to Length -1 do
    if not (s[i] in ['a'..'z','A'..'Z','0'..'9','_','-','.']) then Exit;
  result := True;
end;

function IsValidEmail(EMail :Pchar): Boolean;
var
  Find :Pchar;
  NamePart, ServerPart :array[0..15] of char;
  AimStr :array[0..1] of char;
begin
  result :=  False;
  AimStr[0] := '@';
  AimStr[1] := #0;       
  Find := StrPos(EMail, @AimStr[0]);
  if Find = NIL then Exit;
  INC(Find);

  ZeroMemory(@NamePart[0], 16);
  ZeroMemory(@ServerPart[0], 16);

  StrLCopy(@NamePart[0], EMail, DWORD(Find) - DWORD(EMail) - 1);
  StrCopy(@ServerPart[0], Find);

  if (StrLen(NamePart) = 0) or ((StrLen(ServerPart) < 5)) then Exit;

  AimStr[0] := '.';
  AimStr[1] := #0;    
  Find := StrPos(@ServerPart[0], @AimStr[0]);
  if Find = NIL then Exit;
  if (DWORD(Find) - DWORD(@ServerPart[0])) > (StrLen(ServerPart) - 2) then exit;

  result := CheckAllowed(NamePart) and CheckAllowed(ServerPart);
end;

function ntohs(netShort :WORD):WORD;
asm
  movzx eax, netShort
  xor ecx,ecx
  mov ch,al
  shr eax,$08
  or ecx,eax
  mov ax,cx
end;

function ntohl(netLong :LongWord):LongWord;
asm
  mov ecx, netLong
  mov eax,ecx
  mov edx,ecx
  shl edx,$10
  and eax,$0000ff00
  or eax,edx
  mov edx,ecx
  and edx,$00ff0000
  shr ecx,$10
  or edx,ecx
  shl eax,$08
  shr edx,$08
  or eax,edx
end;

function UpCase(ch :Char):Char;
begin
  Result := ch;
  case Result of
    'a'..'z':  Dec(Result, Ord('a') - Ord('A'));
  end;
end;

function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
asm
   add   eax, ecx
   add   edx, ecx
   xor   ecx, -1
   add   eax, -8
   add   edx, -8
   add   ecx, 9
   push  ebx
   jg    @Dword
   mov   ebx, [eax+ecx]
   cmp   ebx, [edx+ecx]
   jne   @Ret0
   lea   ebx, [eax+ecx]
   add   ecx, 4
   and   ebx, 3
   sub   ecx, ebx
   jg    @Dword
@DwordLoop:
   mov   ebx, [eax+ecx]
   cmp   ebx, [edx+ecx]
   jne   @Ret0
   mov   ebx, [eax+ecx+4]
   cmp   ebx, [edx+ecx+4]
   jne   @Ret0
   add   ecx, 8
   jg    @Dword
   mov   ebx, [eax+ecx]
   cmp   ebx, [edx+ecx]
   jne   @Ret0
   mov   ebx, [eax+ecx+4]
   cmp   ebx, [edx+ecx+4]
   jne   @Ret0
   add   ecx, 8
   jle   @DwordLoop
@Dword:
   cmp   ecx, 4
   jg    @Word
   mov   ebx, [eax+ecx]
   cmp   ebx, [edx+ecx]
   jne   @Ret0
   add   ecx, 4
@Word:
   cmp   ecx, 6
   jg    @Byte
   movzx ebx, word ptr [eax+ecx]
   cmp   bx, [edx+ecx]
   jne   @Ret0
   add   ecx, 2
@Byte:
   cmp   ecx, 7
   jg    @Ret1
   movzx ebx, byte ptr [eax+7]
   cmp   bl, [edx+7]
   jne   @Ret0
@Ret1:
   mov   eax, 1
   pop   ebx
   ret
@Ret0:
   xor   eax, eax
   pop   ebx
end;


procedure ZeroMemory(Destination: Pointer; Length: DWORD);
begin
  FillChar(Destination^, Length, #0);
end;

procedure Move(const Source; var Dest; count : Integer);
var
  S, D: PChar;
  I: Integer;
begin
  S := PChar(@Source);
  D := PChar(@Dest);
  if S = D then Exit;
  if Cardinal(D) > Cardinal(S) then
    for I := count-1 downto 0 do
      D[I] := S[I]
  else
    for I := 0 to count-1 do
      D[I] := S[I];
end;

function StrLen(const Str: PChar): Cardinal;
var
  Scan: PChar;
begin
  Result := 0;
  Scan := Str;
  while Scan^ <> #0 do
  begin
    Inc(Result);
    Scan := @Str[Result];
  end;
end;


procedure Trim(S:PChar; D:PChar);
var
  LowIndex, MaxIndex, Length: Integer;
begin
  MaxIndex := StrLen(S) - 1;
  LowIndex := 0;
  D[0] := #0;
  
  while (LowIndex <= MaxIndex) and (S[LowIndex] <= ' ') do Inc(LowIndex);
  if LowIndex > MaxIndex then Exit;

  while S[MaxIndex] <= ' ' do Dec(MaxIndex);
  if LowIndex > MaxIndex then Exit;

  Length := MaxIndex - LowIndex + 1;
  CopyMemory(D, @S[LowIndex], Length);
  D[Length] := #0;
end;

procedure CopyMemory(Dest, Source : Pointer; count : Integer);
var
  S, D                                  : PChar;
  I                                     : Integer;
begin
  S := Source;
  D := Dest;
  if S = D then Exit;
  if Cardinal(D) > Cardinal(S) then
    for I := count - 1 downto 0 do
      D[I] := S[I]
  else
    for I := 0 to count - 1 do
      D[I] := S[I];
end;


procedure FillChar(var Dest; count : Integer; Value : Char);
inline;
var
  I                                     : Integer;
  P                                     : PChar;
begin
  P := PChar(@Dest);
  for I := count - 1 downto 0 do
    P[I] := Value;
end;


function StrSame(Str1, Str2 : PChar) : LongBool;
inline;
begin
  result := false;
  while Str1^ = Str2^ do
  begin
    if Str1^ = #0 then
    begin
      result := True;
      exit;
    end;
    Inc(Str1);
    Inc(Str2);
  end;
end;

function StrLCopy(Dest : PChar; const Source : PChar; MaxLen : Cardinal) : PChar;
inline;
var
  I                                     : INTEGER;
begin
  for I := 0 to MaxLen - 1 do
  begin
    Dest[I] := Source[I];
    if Source[I] = #0 then break;
  end;
  Result := Dest;
end;

function StrCopy(Dest: PChar; const Source: PChar): PChar;
var
  I: Integer;
begin
  I := 0;
  Repeat
    Dest[I] := Source[I];
    Inc (I);
  until Source[I] = #0;
  Result := Dest;
end;

function StrEnd(const Str: PChar): PChar;
var
  I: Integer;
begin
  I := 0;
  Repeat
    Result := @Str[I];
    Inc (I);
  until Result^ = #0;
end;

function StrCat(Dest: PChar; const Source: PChar): PChar;
begin
  StrCopy(StrEnd(Dest), Source);
  Result := Dest;
end;

end.
