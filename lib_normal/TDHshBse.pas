(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDHshBse                                                         *)
(* Base unit to support hash tables                                 *)
(********************************************************************)

unit TDHshBse;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  TDBasics;

type
  TtdHashFunc = function (const aKey       : string;
                                aTableSize : integer) : integer;
    {-Function type for a hash function mod table size}

  TtdHashFuncEx = function (const aKey : string) : longint;
    {-Function type for a hash function returning a longint}

{---Prime numbers---}
function TDGetClosestPrime(N : integer) : integer;
  {-returns the closest prime number to N}

{---Example hash functions---}
function TDSimpleHash(const aKey       : string;
                            aTableSize : integer) : integer;
  {-simple hash function}
function TDPJWHash(const aKey       : string;
                         aTableSize : integer) : integer;
  {-PJW hash function}
function TDPJWHashEx(const aKey : string) : longint;
  {-Extended PJW hash function}

implementation

{===Interfaced routines==============================================}
function TDGetClosestPrime(N : integer) : integer;
{$I TDPrimes.inc}
const
  Forever = true;
var
  L, R, M : integer;
  RootN   : integer;
  IsPrime : boolean;
  DivisorIndex : integer;
begin
  {treat 2 as a special case}
  if (N = 2) then begin
    Result := N;
    Exit;
  end;
  {make the result equal to N, and if it's even, the next odd number}
  if Odd(N) then
    Result := N
  else
    Result := succ(N);
  {if the result is within our prime number table, use binary search
   to find the equal or next highest prime number}
  if (Result <= MaxPrime) then begin
    L := 0;
    R := pred(PrimeCount);
    while (L <= R) do begin
      M := (L + R) div 2;
      if (Result = Primes[M]) then
        Exit
      else if (Result < Primes[M]) then
        R := pred(M)
      else
        L := succ(M);
    end;
    Result := Primes[L];
    Exit;
  end;
  {the result is outside our prime number table range, use the
   standard method for testing primality (do any of the primes up to
   the root of the number divide it exactly?) and continue
   incrementing the result by 2 until it is prime}
  if (Result <= (MaxPrime * MaxPrime)) then begin
    while Forever do begin
      RootN := round(Sqrt(Result));
      DivisorIndex := 1; {ignore the prime number 2}
      IsPrime := true;
      while (DivisorIndex < PrimeCount) and (RootN > Primes[DivisorIndex]) do begin
        if ((Result div Primes[DivisorIndex]) * Primes[DivisorIndex] = Result) then begin
          IsPrime := false;
          Break;
        end;
        inc(DivisorIndex);
      end;
      if IsPrime then
        Exit;
      inc(Result, 2);
    end;
  end;
end;
{--------}
function TDPJWHash(const aKey       : string;
                         aTableSize : integer) : integer;
var
  G : longint;
  i : integer;
  Hash : longint;
begin
  Hash := 0;
  for i := 1 to length(aKey) do begin
    Hash := (Hash shl 4) + ord(aKey[i]);
    G := Hash and $F0000000;
    if (G <> 0) then
      Hash := (Hash xor (G shr 24)) xor G;
  end;
  Result := Hash mod aTableSize;
end;
{--------}
function TDPJWHashEx(const aKey : string) : longint;
var
  G : longint;
  i : integer;
  Hash : longint;
begin
  Hash := 0;
  for i := 1 to length(aKey) do begin
    Hash := (Hash shl 4) + ord(aKey[i]);
    G := Hash and $F0000000;
    if (G <> 0) then
      Hash := (Hash xor (G shr 24)) xor G;
  end;
  Result := Hash;
end;
{--------}
function TDSimpleHash(const aKey       : string;
                            aTableSize : integer) : integer;
var
  i : integer;
  Hash : longint;
begin
  Hash := 0;
  for i := 1 to length(aKey) do
    Hash := ((Hash * 17) + ord(aKey[i])) mod aTableSize;
  Result := Hash;
  if (Result < 0) then
    inc(Result, aTableSize);
end;
{====================================================================}

end.
