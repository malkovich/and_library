(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDTList                                                          *)
(* Routines that act on TList                                       *)
(********************************************************************)

unit TDTList;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  Classes,
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

{---Sequential search and binary search for TList---}
function TDTListIndexOf(aList : TList; aItem : pointer;
                        aCompare : TtdCompareFunc) : integer;
function TDTListSortedIndexOf(aList : TList; aItem : pointer;
                              aCompare : TtdCompareFunc) : integer;

{---Helper routines for the sorts}
procedure TDValidateListRange(aList : TList;
                              aStart, aEnd : integer;
                              aMessage : string);
  {-validate a range for a list:
    aList is non-nil and
    0 <= aStart,aEnd < aList.Count; aStart <= aEnd}

procedure TDSimpleListShuffle(aList : TList;
                              aStart, aEnd : integer);
  {-simple shuffle of the items between aStart and aEnd in aList}

procedure TDListShuffle(aList : TList;
                        aStart, aEnd : integer);
  {-permutation shuffle of the items between aStart and aEnd in aList}

procedure TDListReverse(aList : TList;
                        aStart, aEnd : integer);
  {-reverse the items between aStart and aEnd in aList}

procedure TDListMerge(aList1, aList2, aTargetList : TList;
                      aCompare : TtdCompareFunc);
  {-merge two *sorted* lists into a third target list, also sorted;
    the two source lists are not validated to be sorted}

implementation


{===Sequential search and binary search for TList====================}
function TDTListIndexOf(aList : TList; aItem : pointer;
                        aCompare : TtdCompareFunc) : integer;
var
  Inx : integer;
begin
  for Inx := 0 to pred(aList.Count) do
    if (aCompare(aList.List^[Inx], aItem) = 0) then begin
      Result := Inx;
      Exit;
    end;
  Result := -1;
end;
{--------}
function TDTListSortedIndexOf(aList : TList; aItem : pointer;
                              aCompare : TtdCompareFunc) : integer;
var
  L, R, M : integer;
  CompareResult : integer;
begin
  {set the values of the left and right indexes}
  L := 0;
  R := pred(aList.Count);
  while (L <= R) do begin
    {calculate the middle index}
    M := (L + R) div 2;
    {compare the middle element against the given item}
    CompareResult := aCompare(aList.List^[M], aItem);
    {if middle element is less than the given item, move the left
     index to just after the middle index}
    if (CompareResult < 0) then
      L := succ(M)
    {if middle element is greater than the given item, move the right
     index to just before the middle index}
    else if (CompareResult > 0) then
      R := pred(M)
    {otherwise we found the item}
    else begin
      Result := M;
      Exit;
    end;
  end;
  Result := -1;
end;
{====================================================================}


{===Interfaced routines==============================================}
procedure TDValidateListRange(aList : TList;
                              aStart, aEnd : integer;
                              aMessage : string);
begin
  if (aList = nil) then
    raise EtdTListException.Create(
       Format(LoadStr(tdeTListIsNil), [aMessage]));
  if (aStart < 0) or (aStart >= aList.Count) or
     (aEnd   < 0) or (aEnd   >= aList.Count) or
     (aStart > aEnd) then
    raise EtdTListException.Create(
       Format(LoadStr(tdeTListInvalidRange), [aStart, aEnd, aMessage]));
end;
{--------}
procedure TDSimpleListShuffle(aList : TList;
                              aStart, aEnd : integer);
var
  Range     : integer;
  Inx       : integer;
  RandomInx : integer;
  TempPtr   : pointer;
begin
  TDValidateListRange(aList, aStart, aEnd, 'TDSimpleListShuffle');
  Range := succ(aEnd - aStart);
  for Inx := aStart to aEnd do begin
    RandomInx := aStart + Random(Range);
    TempPtr := aList.List^[Inx];
    aList.List^[Inx] := aList.List^[RandomInx];
    aList.List^[RandomInx] := TempPtr;
  end;
end;
{--------}
procedure TDListShuffle(aList : TList;
                        aStart, aEnd : integer);
var
  Inx       : integer;
  RandomInx : integer;
  TempPtr   : pointer;
begin
  TDValidateListRange(aList, aStart, aEnd, 'TDListShuffle');
  {for each element, counting from the right..,.}
  for Inx := (aEnd - aStart) downto aStart + 1 do begin
    {generate a random number from aStart to the index of the element
     we're currently at}
    RandomInx := aStart + Random(Inx-aStart+1);
    {if the random index does not equal our index, swap the items}
    if (RandomInx <> Inx) then begin
      TempPtr := aList.List^[Inx];
      aList.List^[Inx] := aList.List^[RandomInx];
      aList.List^[RandomInx] := TempPtr;
    end;
  end;
end;
{--------}
procedure TDListReverse(aList : TList;
                        aStart, aEnd : integer);
var
  Inx     : integer;
  ToInx   : integer;
  TempPtr : pointer;
begin
  TDValidateListRange(aList, aStart, aEnd, 'TDListReverse');
  {initialize the to index to the last item}
  ToInx := aEnd;
  {for each element...}
  for Inx := aStart to ((aEnd + aStart) div 2) do begin
    {swap the items from the front to the end}
    TempPtr := aList.List^[Inx];
    aList.List^[Inx] := aList.List^[ToInx];
    aList.List^[ToInx] := TempPtr;
    dec(ToInx);
  end;
end;
{--------}
procedure TDListMerge(aList1, aList2, aTargetList : TList;
                      aCompare : TtdCompareFunc);
var
  Inx1, Inx2, Inx3 : integer;
begin
  {set up the target list}
  aTargetList.Clear;
  aTargetList.Capacity := aList1.Count + aList2.Count;
  {initialize the counters}
  Inx1 := 0;
  Inx2 := 0;
  Inx3 := 0;
  {do until one of the source lists is exhausted...}
  while (Inx1 < aList1.Count) and (Inx2 < aList2.Count) do begin
    {find the smaller item from both lists and copy it over to the
     target list; increment the indexes}
    if aCompare(aList1.List^[Inx1], aList2.List^[Inx2]) <= 0 then begin
      aTargetList.List^[Inx3] := aList1.List^[Inx1];
      inc(Inx1);
    end
    else begin
      aTargetList.List^[Inx3] := aList2.List^[Inx2];
      inc(Inx2);
    end;
    inc(Inx3);
  end;
  {the loop ends if one of the source lists is exhausted; if there are
   any remaining items in the first source list, copy them over}
  {Note: the calls to Move are OK since we've ensured that the target
         list has enough room to accept the items from both lists}
  if (Inx1 < aList1.Count) then
    Move(aList1.List^[Inx1], aTargetList.List^[Inx3],
         (aList1.Count - Inx1) * sizeof(pointer))
  {otherwise copy over the remaining items in the second list}
  else
    Move(aList2.List^[Inx2], aTargetList.List^[Inx3],
         (aList2.Count - Inx2) * sizeof(pointer));
end;
{====================================================================}


end.
