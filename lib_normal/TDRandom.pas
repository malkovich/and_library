(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDRandom                                                         *)
(* Random number generator classes                                  *)
(********************************************************************)

unit TDRandom;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  {$IFDEF Delphi1}
  WinTypes, WinProcs,
  {$ENDIF}
  {$IFDEF Delphi2Plus}
  Windows,
  {$ENDIF}
  {$IFDEF Kylix1Plus}
  Types, Libc,
  {$ENDIF}
  TDBasics;

const
  tdcAdditivePRNGSize = 55;
  tdcShufflePRNGSize = 97;

type
  TtdBasePRNG = class
    {base random number generator class}
    private
      FName : TtdNameString;
    protected
      procedure bError(aErrorCode  : integer;
                 const aMethodName : TtdNameString);
    public
      function AsDouble : double; virtual; abstract;
        {-returns a random number between 0 inclusive and 1 exclusive}
      function AsLimitedDouble(aLower, aUpper : double) : double;
        {-returns a random number between aLower inclusive and aUpper
          exclusive}
      function AsInteger(aUpper : integer) : integer;
        {-returns a random integer between 0 inclusive and aUpper
          exclusive}

      property Name : TtdNameString read FName write FName;
  end;

  TtdMinStandardPRNG = class(TtdBasePRNG)
    {Minimal Standard random number generator class}
    private
      FSeed : longint;
    protected
      procedure msSetSeed(aValue : longint);
    public
      constructor Create(aSeed : longint);

      function AsDouble  : double; override;
      property Seed : longint read FSeed write msSetSeed;
  end;

  TtdSystemPRNG = class(TtdBasePRNG)
    {Delphi's random number generator as a class}
    private
      FSeed : longint;
    protected
    public
      constructor Create(aSeed : longint);

      function AsDouble  : double; override;
      procedure Randomize;
      property Seed : longint read FSeed write FSeed;
  end;

  TtdCombinedPRNG = class(TtdBasePRNG)
    {Combined generator}
    private
      FSeed1 : longint;
      FSeed2 : longint;
    protected
      procedure cpSetSeed1(aValue : longint);
      procedure cpSetSeed2(aValue : longint);
    public
      constructor Create(aSeed1, aSeed2 : longint);

      function AsDouble  : double; override;
      property Seed1 : longint read FSeed1 write cpSetSeed1;
      property Seed2 : longint read FSeed2 write cpSetSeed2;
  end;

  TtdAdditiveGenerator = class(TtdBasePRNG)
    {Additive random number generator}
    private
      FInx1  : integer;
      FInx2  : integer;
      FPRNG  : TtdMinStandardPRNG;
      FTable : array [0..pred(tdcAdditivePRNGSize)] of double;
    protected
      procedure agSetSeed(aValue : longint);
      procedure agInitTable;
    public
      constructor Create(aSeed : longint);
      destructor Destroy; override;

      function AsDouble  : double; override;
      property Seed : longint write agSetSeed;
  end;

  TtdShuffleGenerator = class(TtdBasePRNG)
    {Shuffle random number generator}
    private
      FAux   : double;
      FPRNG  : TtdMinStandardPRNG;
      FTable : array [0..pred(tdcShufflePRNGSize)] of double;
    protected
      procedure sgSetSeed(aValue : longint);
      procedure sgInitTable;
    public
      constructor Create(aSeed : longint);
      destructor Destroy; override;

      function AsDouble  : double; override;
      property Seed : longint write sgSetSeed;
  end;

function NormalRandomNumber(aPRNG   : TtdBasePRNG;
                            aMean   : double;
                            aStdDev : double) : double;

function ExponentialRandomNumber(aPRNG   : TtdBasePRNG;
                                 aMean   : double) : double;

implementation

const
  UnitName = 'TDRandom';

{====================================================================}
function GetTimeAsLong : longint;
{$IFDEF Delphi1}
assembler;
asm
  mov ah, $2C
  call DOS3Call
  mov ax, cx
end;
{$ENDIF}
{$IFDEF Delphi2Plus}
begin
  Result := longint(GetTickCount);
end;
{$ENDIF}
{$IFDEF Kylix1Plus}
var
  T : TTime_t;
begin
  __time(@T);
  Result := longint(T);
end;
{$ENDIF}
{====================================================================}


{===TtdBasePRNG======================================================}
function TtdBasePRNG.AsLimitedDouble(aLower, aUpper : double) : double;
begin
  if (aLower < 0.0) or (aUpper < 0.0) or (aLower >= aUpper) then
    bError(tdeRandRangeError, 'AsLimitedDouble');
  Result := (AsDouble * (aUpper - aLower)) + aLower;
end;
{--------}
function TtdBasePRNG.AsInteger(aUpper : integer) : integer;
begin
  if (aUpper <= 0) then
    bError(tdeRandRangeError, 'AsInteger');
  Result := Trunc(AsDouble * aUpper);
end;
{--------}
procedure TtdBasePRNG.bError(aErrorCode  : integer;
                       const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdRandGenException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{====================================================================}


{===TtdMinStandardPRNG===============================================}
const
  m = 2147483647;
{--------}
constructor TtdMinStandardPRNG.Create(aSeed : longint);
begin
  inherited Create;
  Seed := aSeed;
end;
{--------}
function TtdMinStandardPRNG.AsDouble  : double;
const
  a = 16807;
  q = 127773; {equals m div a}
  r = 2836;   {equals m mod a}
  OneOverM : double = 1.0 / m;
var
  k : longint;
begin
  k := FSeed div q;
  FSeed := (a * (FSeed - (k * q))) - (k * r);
  if (FSeed < 0) then
    inc(FSeed, m);
  Result := FSeed * OneOverM;
end;
{--------}
procedure TtdMinStandardPRNG.msSetSeed(aValue : longint);
begin
  if (aValue > 0) then
    FSeed := aValue
  else
    FSeed := GetTimeAsLong;
  {make sure that the seed is between 1 and m-1 inclusive}
  if (FSeed >= m-1) then
    FSeed := FSeed - (m - 1) + 1;
end;
{====================================================================}


{===TtdSystemPRNG====================================================}
constructor TtdSystemPRNG.Create(aSeed : longint);
begin
  inherited Create;
  Seed := System.RandSeed;
end;
{--------}
function TtdSystemPRNG.AsDouble  : double;
var
  OldSeed : longint;
begin
  OldSeed := System.RandSeed;
  System.RandSeed := Seed;
  Result := System.Random;
  Seed := System.RandSeed;
  System.RandSeed := OldSeed;
end;
{--------}
procedure TtdSystemPRNG.Randomize;
begin
  Seed := GetTimeAsLong;
end;
{====================================================================}


{===TtdCombinedPRNG===============================================}
const
  m1 = 2147483563;
  m2 = 2147483399;
{--------}
constructor TtdCombinedPRNG.Create(aSeed1, aSeed2 : longint);
begin
  inherited Create;
  Seed1 := aSeed1;
  Seed2 := aSeed2;
end;
{--------}
function TtdCombinedPRNG.AsDouble  : double;
const
  a1 = 40014;
  q1 = 53668;  {equals m1 div a1}
  r1 = 12211;  {equals m1 mod a1}

  a2 = 40692;
  q2 = 52774;  {equals m2 div a2}
  r2 = 3791;   {equals m2 mod a2}

  OneOverM1 : double = 1.0 / m1;
var
  k : longint;
  Z : longint;
begin
  {advance first PRNG}
  k := FSeed1 div q1;
  FSeed1 := (a1 * (FSeed1 - (k * q1))) - (k * r1);
  if (FSeed1 < 0) then
    inc(FSeed1, m1);
  {advance second PRNG}
  k := FSeed2 div q2;
  FSeed2 := (a2 * (FSeed2 - (k * q2))) - (k * r2);
  if (FSeed2 < 0) then
    inc(FSeed2, m2);
  {combine the two seeds}
  Z := FSeed1 - FSeed2;
  if (Z <= 0) then
    Z := Z + m1 - 1;
  Result := Z * OneOverM1;
end;
{--------}
procedure TtdCombinedPRNG.cpSetSeed1(aValue : longint);
begin
  if (aValue > 0) then
    FSeed1 := aValue
  else
    FSeed1 := GetTimeAsLong;
  {make sure that the seed is between 1 and m-1 inclusive}
  if (FSeed1 >= m1-1) then
    FSeed1 := FSeed1 - (m1 - 1) + 1;
end;
{--------}
procedure TtdCombinedPRNG.cpSetSeed2(aValue : longint);
begin
  if (aValue > 0) then
    FSeed2 := aValue
  else
    FSeed2 := GetTimeAsLong;
  {make sure that the seed is between 1 and m-1 inclusive}
  if (FSeed2 >= m2-1) then
    FSeed2 := FSeed2 - (m2 - 1) + 1;
end;
{====================================================================}


{===TtdAdditiveGenerator=============================================}
constructor TtdAdditiveGenerator.Create(aSeed : longint);
begin
  inherited Create;
  FPRNG := TtdMinStandardPRNG.Create(aSeed);
  agInitTable;
  FInx1 := pred(tdcAdditivePRNGSize);
  FInx2 := 23;
end;
{--------}
destructor TtdAdditiveGenerator.Destroy;
begin
  FPRNG.Free;
  inherited Destroy;
end;
{--------}
procedure TtdAdditiveGenerator.agSetSeed(aValue : longint);
begin
  FPRNG.Seed := aValue;
  agInitTable;
end;
{--------}
procedure TtdAdditiveGenerator.agInitTable;
var
  i : integer;
begin
  for i := pred(tdcAdditivePRNGSize) downto 0 do
    FTable[i] := FPRNG.AsDouble;
end;
{--------}
function TtdAdditiveGenerator.AsDouble  : double;
begin
  Result := FTable[FInx1] + FTable[FInx2];
  if (Result >= 1.0) then
    Result := Result - 1.0;
  FTable[FInx1] := Result;
  inc(FInx1);
  if (FInx1 >= tdcAdditivePRNGSize) then
    FInx1 := 0;
  inc(FInx2);
  if (FInx2 >= tdcAdditivePRNGSize) then
    FInx2 := 0;
end;
{====================================================================}


{===TtdShuffleGenerator==============================================}
constructor TtdShuffleGenerator.Create(aSeed : longint);
begin
  inherited Create;
  FPRNG := TtdMinStandardPRNG.Create(aSeed);
  sgInitTable;
end;
{--------}
destructor TtdShuffleGenerator.Destroy;
begin
  FPRNG.Free;
  inherited Destroy;
end;
{--------}
function TtdShuffleGenerator.AsDouble  : double;
var
  Inx : integer;
begin
  Inx := Trunc(FAux * tdcShufflePRNGSize);
  Result := FTable[Inx];
  FAux := Result;
  FTable[Inx] := FPRNG.AsDouble;
end;
{--------}
procedure TtdShuffleGenerator.sgSetSeed(aValue : longint);
begin
  FPRNG.Seed := aValue;
  sgInitTable;
end;
{--------}
procedure TtdShuffleGenerator.sgInitTable;
var
  i : integer;
begin
  for i := pred(tdcShufflePRNGSize) downto 0 do
    FTable[i] := FPRNG.AsDouble;
  FAux := FPRNG.AsDouble;
end;
{====================================================================}


{===Other distributions==============================================}
var
  NRGNextNumber : double;
  NRGNextIsSet  : boolean;
{--------}
function NormalRandomNumber(aPRNG   : TtdBasePRNG;
                            aMean   : double;
                            aStdDev : double) : double;
var
  R1, R2     : double;
  RadiusSqrd : double;
  Factor     : double;
begin
  if NRGNextIsSet then begin
    Result := NRGNextNumber;
    NRGNextIsSet := false;
  end
  else begin
    {get two random numbers that define a point in the unit circle}
    repeat
      R1 := (2.0 * aPRNG.AsDouble) - 1.0;
      R2 := (2.0 * aPRNG.AsDouble) - 1.0;
      RadiusSqrd := sqr(R1) + sqr(R2);
    until (RadiusSqrd < 1.0) and (RadiusSqrd > 0.0);
    {apply Box-Muller transformation}
    Factor := sqrt(-2.0 * ln(RadiusSqrd) / RadiusSqrd);
    Result := R1 * Factor;
    NRGNextNumber := R2 * Factor;
    NRGNextIsSet := true;
  end;
end;
{--------}
function ExponentialRandomNumber(aPRNG   : TtdBasePRNG;
                                 aMean   : double) : double;
var
  R : double;
begin
  repeat
    R := aPRNG.AsDouble;
  until (R <> 0.0);
  Result := -aMean * ln(R);
end;
{====================================================================}

initialization
  NRGNextIsSet := false; 

end.

