(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDRecLst                                                         *)
(* Record list class                                                *)
(********************************************************************)

unit TDRecLst;

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

type
  TtdRecordList = class
    private
      FActElemSize : integer;
      FArray       : PAnsiChar;
      FCount       : integer;
      FCapacity    : integer;
      FElementSize : integer;
      FIsSorted    : boolean;
      FMaxElemCount: integer;
      FName        : TtdNameString;
    protected
      function rlGetItem(aIndex : integer) : pointer;
      procedure rlSetCapacity(aCapacity : integer);
      procedure rlSetCount(aCount : integer);

      function rlBinarySearch(aItem    : pointer;
                              aCompare : TtdCompareFunc;
                          var aInx     : integer) : boolean;
      procedure rlError(aErrorCode : integer;
                  const aMethodName : TtdNameString;
                        aIndex      : integer);
      procedure rlExpand;
    public
      constructor Create(aElementSize : integer);
      destructor Destroy; override;

      function Add(aItem : pointer) : integer;
      procedure Clear;
      procedure Delete(aIndex : integer);
      procedure Exchange(aIndex1, aIndex2 : integer);
      function First : pointer;
      function IndexOf(aItem    : pointer;
                       aCompare : TtdCompareFunc) : integer;
      procedure Insert(aIndex : integer; aItem : pointer);
      function InsertSorted(aItem : pointer;
                            aCompare : TtdCompareFunc) : integer;
      function Last : pointer;
      procedure Move(aCurIndex, aNewIndex : integer);
      function Remove(aItem    : pointer;
                      aCompare : TtdCompareFunc) : integer;
      procedure Sort(aCompare : TtdCompareFunc);

      property Capacity : integer
         read FCapacity write rlSetCapacity;
      property Count : integer
         read FCount write rlSetCount;
      property ElementSize : integer
         read FActElemSize;
      property IsSorted : boolean
         read FIsSorted;
      property Items[aIndex : integer] : pointer
         read rlGetItem; default;
      property MaxCount : integer
         read FMaxElemCount;
      property Name : TtdNameString
         read FName write FName;
  end;

implementation

const
  UnitName = 'TDRecLst';

{===TtdRecordList===================================================}
constructor TtdRecordList.Create(aElementSize : integer);
begin
  inherited Create;
  {save the actual element size}
  FActElemSize := aElementSize;
  {round the actual size to the nearest 4 bytes}
  FElementSize := ((aElementSize + 3) shr 2) shl 2;
  {calculate the maximum number of elements}
  {$IFDEF Delphi1}
  FMaxElemCount := 65535 div FElementSize;
  {$ELSE}
  FMaxElemCount := MaxInt div integer(FElementSize);
  {$ENDIF}
  FIsSorted := true;
end;
{--------}
destructor TtdRecordList.Destroy;
begin
  Capacity := 0;
  inherited Destroy;
end;
{--------}
function TtdRecordList.Add(aItem : pointer) : integer;
begin
  Result := Count;
  Insert(Count, aItem);
end;
{--------}
procedure TtdRecordList.Clear;
begin
  Count := 0;
  FIsSorted := true;
end;
{--------}
procedure TtdRecordList.Delete(aIndex : integer);
begin
  if (aIndex < 0) or (aIndex >= Count) then
    rlError(tdeIndexOutOfBounds, 'Delete', aIndex);
  dec(FCount);
  if (aIndex < Count) then
    System.Move((FArray + (succ(aIndex) * FElementSize))^,
                (FArray + (aIndex * FElementSize))^,
                (Count - aIndex) * FElementSize);
  if (Count <= 1) then
    FIsSorted := true;
end;
{--------}
procedure TtdRecordList.Exchange(aIndex1, aIndex2 : integer);
var
  i       : integer;
  Temp    : longint;
  FromPtr : PAnsiChar;
  ToPtr   : PAnsiChar;
begin
  if (aIndex1 < 0) or (aIndex1 > Count) then
    rlError(tdeIndexOutOfBounds, 'Exchange', aIndex1);
  if (aIndex2 < 0) or (aIndex2 > Count) then
    rlError(tdeIndexOutOfBounds, 'Exchange', aIndex2);
  FromPtr := FArray + (aIndex1 * FElementSize);
  ToPtr := FArray + (aIndex2 * FElementSize);
  for i := 0 to pred(FElementSize div 4) do begin
    Temp := PLongint(FromPtr)^;
    PLongint(FromPtr)^ := PLongint(ToPtr)^;
    PLongint(ToPtr)^ := Temp;
    inc(FromPtr, 4);
    inc(ToPtr, 4);
  end;
  FIsSorted := false;
end;
{--------}
function TtdRecordList.First : pointer;
begin
  Result := pointer(FArray);
end;
{--------}
function TtdRecordList.IndexOf(aItem    : pointer;
                               aCompare : TtdCompareFunc) : integer;
var
  ElementPtr : PAnsiChar;
  i          : integer;
begin
  if IsSorted then begin
    if rlBinarySearch(aItem, aCompare, Result) then
      Exit;
  end
  else begin {do sequential search}
    ElementPtr := FArray;
    for i := 0 to pred(Count) do begin
      if (aCompare(aItem, ElementPtr) = 0) then begin
        Result := i;
        Exit;
      end;
      inc(ElementPtr, FElementSize);
    end;
  end;
  {if we get here the item was not found}
  Result := -1;
end;
{--------}
procedure TtdRecordList.Insert(aIndex : integer; aItem : pointer);
begin
  if (aItem = nil) then
    rlError(tdeNilItem, 'Insert', aIndex);
  if (aIndex < 0) or (aIndex > Count) then
    rlError(tdeIndexOutOfBounds, 'Insert', aIndex);
  if (Count = Capacity) then
    rlExpand;
  if (aIndex < Count) then
    System.Move((FArray + (aIndex * FElementSize))^,
                (FArray + (succ(aIndex) * FElementSize))^,
                (Count - aIndex) * FElementSize);
  System.Move(aItem^,
              (FArray + (aIndex * FElementSize))^,
              FActElemSize);
  inc(FCount);
  FIsSorted := (Count = 1);
end;
{--------}
function TtdRecordList.InsertSorted(aItem : pointer;
                                    aCompare : TtdCompareFunc) : integer;
begin
  if not IsSorted then
    rlError(tdeListIsNotSorted, 'InsertSorted', 0);
  if not rlBinarySearch(aItem, aCompare, Result) then begin
    Insert(Result, aItem);
    FIsSorted := true;
  end;
end;
{--------}
function TtdRecordList.Last : pointer;
begin
  Result := pointer(FArray + (pred(Count) * FElementSize));
end;
{--------}
procedure TtdRecordList.Move(aCurIndex, aNewIndex : integer);
var
  Temp  : PByteArray;
begin
  if (aCurIndex < 0) or (aCurIndex >= Count) then
    rlError(tdeIndexOutOfBounds, 'Move', aCurIndex);
  if (aNewIndex < 0) or (aNewIndex >= Count) then
    rlError(tdeIndexOutOfBounds, 'Move', aNewIndex);
  if (aCurIndex <> aNewIndex) then begin
    {perform the move, by (1) saving the element at the current index,
     (2) deleting that element from the array and (3) inserting it
     into the array at the new index}
    GetMem(Temp, FElementSize);
    try
      System.Move((FArray + (aCurIndex * FElementSize))^,
                  Temp^,
                  FElementSize);
      Delete(aCurIndex);
      Insert(aNewIndex, Temp);
      FIsSorted := false;
    finally
      FreeMem(Temp, FElementSize);
    end;
  end;
end;
{--------}
function TtdRecordList.rlBinarySearch(aItem    : pointer;
                                      aCompare : TtdCompareFunc;
                                  var aInx     : integer) : boolean;
var
  L, R, M : integer;
  MPtr    : pointer;
  CompareResult : integer;
begin
  L := 0;
  R := pred(Count);
  while L <= R do begin
    M := (L + R) div 2;
    MPtr := pointer(FArray + (M * FElementSize));
    CompareResult := aCompare(MPtr, aItem);
    if (CompareResult < 0) then
      L := succ(M)
    else if (CompareResult > 0) then
      R := pred(M)
    else begin
      aInx := M;
      Result := true;
      Exit;
    end;
  end;
  aInx := L;
  Result := false;
end;
{--------}
procedure TtdRecordList.rlError(aErrorCode  : integer;
                          const aMethodName : TtdNameString;
                                aIndex      : integer);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdArrayException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name, aIndex]));
end;
{--------}
procedure TtdRecordList.rlExpand;
var
  NewCapacity : integer;
begin
  {if current capacity is 0, make new capacity 4 elements}
  if (Capacity = 0) then
    NewCapacity := 4
  {if current capacity is less than 64, increase it by 16 elements}
  else if (Capacity < 64) then
    NewCapacity := Capacity + 16
  {if current capacity is 64 or more, increase it by a quarter}
  else
    NewCapacity := Capacity + (Capacity div 4);
  {make sure we don't grow beyond the array's upper limit}
  if (NewCapacity > FMaxElemCount) then begin
    NewCapacity := FMaxElemCount;
    if (NewCapacity = Capacity) then
      rlError(tdeAtMaxCapacity, 'rlExpand', 0);
  end;
  {set the new capacity}
  Capacity := NewCapacity;
end;
{--------}
function TtdRecordList.rlGetItem(aIndex : integer) : pointer;
begin
  if (aIndex < 0) or (aIndex >= Count) then
    rlError(tdeIndexOutOfBounds, 'rlGetItem', aIndex);
  Result := pointer(FArray + (aIndex * FElementSize));
end;
{--------}
procedure TtdRecordList.rlSetCapacity(aCapacity : integer);
begin
  if (aCapacity <> FCapacity) then begin
    {don't go over the maximum number of elements possible}
    if (aCapacity > FMaxElemCount) then
      rlError(tdeCapacityTooLarge, 'rlSetCapacity', 0);
    {reallocate the array, or free it if the capacity is being reduced
     to zero}
    {$IFDEF Delphi1}
    if (aCapacity = 0) then begin
      FreeMem(FArray, word(FCapacity) * FElementSize);
      FArray := nil;
    end
    else begin
      if (FCapacity = 0) then
        GetMem(FArray, word(aCapacity) * FElementSize)
      else
        FArray := ReallocMem(FArray,
                             word(FCapacity) * FElementSize,
                             word(aCapacity) * FElementSize);
    end;
    {$ELSE}
    ReallocMem(FArray, aCapacity * FElementSize);
    {$ENDIF}
    {are we shrinking the capacity? if so check the count}
    if (aCapacity < FCapacity) then begin
      if (Count > aCapacity) then
        Count := aCapacity;
    end;
    {save the new capacity}
    FCapacity := aCapacity;
  end;
end;
{--------}
procedure TtdRecordList.rlSetCount(aCount : integer);
begin
  if (aCount <> FCount) then begin
    {if the new count is greater than the capacity, grow the array}
    if (aCount > Capacity) then
      Capacity := aCount;
    {if the new count is greater than the old count, set new elements
     to binary zeros}
    if (aCount > FCount) then begin
      FillChar((FArray + (FCount * FElementSize))^,
               (aCount - FCount) * FElementSize,
               0);
      FIsSorted := false;
    end;
    {save the new count}
    FCount := aCount;
  end;
end;
{--------}
function TtdRecordList.Remove(aItem    : pointer;
                              aCompare : TtdCompareFunc) : integer;
begin
  Result := IndexOf(aItem, aCompare);
  if (Result <> -1) then
    Delete(Result);
end;
{--------}
procedure TtdRecordList.Sort(aCompare : TtdCompareFunc);
begin
  {!!!!requires the sort unit}
end;
{====================================================================}

end.
