unit BitUnit;

interface

uses
    Classes,SysUtils;
type

    TBitsString = class(TBits)
    protected
        procedure SetBitsString(Value :String);
        function GetBitsString:String;
        procedure SetBCDString(Value :String);
        function GetBCDString:String;
        procedure SetHexString(Value :String);
        function GetHexString:String;
        function HexChar(c: Char): Byte;

        function GetString:String;
        procedure SetString(Value :String);
    public

    published
        property AsBitsString :String Read GetBitsString Write SetBitsString;
        property AsBCDString :String Read GetBCDString Write SetBCDString;
        property AsHexString :String Read GetHexString Write SetHexString;
        property AsString :String Read GetString Write SetString;
    end;

  TBitBools = class(TObject)
  private
    FFlags:Cardinal;
    function GetString: String;
    function GetFlags(Index:Integer):Boolean;
    procedure SetFlags(Index:Integer;Value:Boolean);
  public
    property Flags[Index:Integer]:Boolean read GetFlags write SetFlags;
    property AsLongWord: Cardinal read FFlags write FFlags;
    property AsHexString: String read GetString;
  end;
              

implementation

function TBitBools.GetString: String;
begin
  Result := IntToHex (FFlags, 8);
end;

function TBitBools.GetFlags(Index:Integer):Boolean;
begin
  Result:=(FFlags and (1 shl Index) > 0);
end;
procedure TBitBools.SetFlags(Index:Integer;Value:Boolean);
begin
  FFlags:=(FFlags xor ($7FFFFFFF and (1 shl Index))) or (ord(Value) shl Index);
end;

function TBitsString.GetString:String;
var
    i,iLen,j,iChr:integer;
    sStr:String;
begin
    sStr:='';
    if (Size mod 8 ) = 0 then begin
    iLen := Size div 8;
    for i := 0 to iLen - 1 do begin
        iChr := 0;
        for j := 0 to 7 do begin
        if Bits[i*8 +j] then begin
        iChr := iChr * 2 + 1;
        end else iChr := iChr * 2 + 0;
        end;
        sStr := sStr + chr(ichr);
    end;
    end;
    Result := sStr;
end;

procedure TBitsString.SetString(Value :String);
var        //A -> 0010101
    i,iLen,j,ichr,imod:integer;
begin
    iLen := Length(Value);
    Size := 0;
    if iLen <=0 then exit;
    Size := Length(Value)*8;
    for i := 1 to iLen do begin
    ichr := ord(Value[i]);
    for j := 1 to 8 do begin
        imod := ichr mod 2;
        ichr := ichr div 2;
        if imod = 0 then begin bits[i*8-j] := false;
        end else bits[i*8-j] := true;
    end;
    end;
end;

function TBitsString.HexChar(c: Char): Byte;
begin
    case c of
        '0'..'9': Result := Byte(c) - Byte('0');
        'a'..'f': Result := (Byte(c) - Byte('a')) + 10;
        'A'..'F': Result := (Byte(c) - Byte('A')) + 10;
    else
        Result := 0;
    end;
end;

procedure TBitsString.SetHexString(Value :String);
var        //625 -> 110010101
    i,iLen,j,ichr,imod:integer;
begin
    iLen := Length(Value);
    Size := 0;
    if iLen <=0 then exit;
    for i := 1 to iLen do begin
    if not ((Value[i] in ['0'..'9']) or (Value[i] in ['A'..'F'])) then begin
        exit;
    end;
    end;
    Size := Length(Value)*4;

    for i := 1 to iLen do begin
    ichr := HexChar(Value[i]);
    for j := 1 to 4 do begin
        imod := ichr mod 2;
        ichr := ichr div 2;
        if imod = 0 then begin bits[i*4-j] := false;
        end else bits[i*4-j] := true;
    end;
    end;
end;

function TBitsString.GetHexString:String;
var
    i,iLen,j,iChr:integer;
    sStr:String;
begin
    sStr:='';
    if (Size mod 4 ) = 0 then begin
    iLen := Size div 4;
    for i := 0 to iLen - 1 do begin
        iChr := 0;
        for j := 0 to 3 do begin
        if Bits[i*4 +j] then begin
        iChr := iChr * 2 + 1;
        end else iChr := iChr * 2 + 0;
        end;
        sStr := sStr + IntToHex(ichr,1);
    end;
    end;
    Result := sStr;
end;

procedure TBitsString.SetBCDString(Value :String);
var        //625 -> 110010101
    i,iLen,j,ichr,imod:integer;
begin
    iLen := Length(Value);
    Size := 0;
    if iLen <=0 then exit;
    for i := 1 to iLen do begin
    if not (Value[i] in ['0'..'7']) then begin
        exit;
    end;
    end;
    Size := Length(Value)*3;

    for i := 1 to iLen do begin
    ichr := strToint(Value[i]);
    for j := 1 to 3 do begin
        imod := ichr mod 2;
        ichr := ichr div 2;
        if imod = 0 then begin bits[i*3-j] := false;
        end else bits[i*3-j] := true;
    end;
    end;
end;

function TBitsString.GetBCDString:String;    ///110 010 101 = 625
var
    i,iLen,j,iChr:integer;
    sStr:String;
begin
    sStr:='';
    if (Size mod 3 ) = 0 then begin
    iLen := Size div 3;
    for i := 0 to iLen - 1 do begin
        iChr := 0;
        for j := 0 to 2 do begin
        if Bits[i*3 +j] then begin
        iChr := iChr * 2 + 1;
        end else iChr := iChr * 2 + 0;
        end;
        sStr := sStr + intToStr(ichr);
    end;
    end;
    Result := sStr;
end;

function TBitsString.GetBitsString:String;
var
    i : integer;
    iStr:String;
begin
    iStr :='';
    for i := 0 to size - 1 do begin
    if Bits[i] then iStr := iStr + '1'
        else iStr := iStr + '0';
    end;
    Result := iStr;
end;

procedure TBitsString.SetBitsString(Value :String);
var
    i,iLen:integer;
begin
// if Length(Value) <> Size then exit;
    Size := Length(Value);
    for i := 0 to size - 1 do begin
    if not (Value[i + 1] in ['1','0']) then exit;
    if Value[i] = '1' then Bits[i] := True;
    if Value[i] = '0' then Bits[i] := False;
    end;
end;

end.

