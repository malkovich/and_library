(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDSorts                                                          *)
(* Sort routines                                                    *)
(********************************************************************)

unit TDSorts;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  Classes,
  TDBasics,
  TDTList;

procedure TDBubbleSort(aList    : TList;
                       aFirst   : integer;
                       aLast    : integer;
                       aCompare : TtdCompareFunc);
  {-Standard bubble sort}

procedure TDShakerSort(aList    : TList;
                       aFirst   : integer;
                       aLast    : integer;
                       aCompare : TtdCompareFunc);
  {-Standard shaker sort--double ended bubble sort}

procedure TDCombSort(aList    : TList;
                     aFirst   : integer;
                     aLast    : integer;
                     aCompare : TtdCompareFunc);
  {-Comb sort}

procedure TDSelectionSort(aList    : TList;
                          aFirst   : integer;
                          aLast    : integer;
                          aCompare : TtdCompareFunc);
  {-Standard selection sort}

procedure TDInsertionSortStd(aList    : TList;
                             aFirst   : integer;
                             aLast    : integer;
                             aCompare : TtdCompareFunc);
  {-Standard insertion sort}

procedure TDInsertionSort(aList    : TList;
                          aFirst   : integer;
                          aLast    : integer;
                          aCompare : TtdCompareFunc);
  {-Optimized insertion sort}

procedure TDShellSort(aList    : TList;
                      aFirst   : integer;
                      aLast    : integer;
                      aCompare : TtdCompareFunc);
  {-Shellsort using Knuth's sequence}

procedure TDMergeSortStd(aList    : TList;
                         aFirst   : integer;
                         aLast    : integer;
                         aCompare : TtdCompareFunc);
  {-Mergesort using N/2 extra memory}

procedure TDMergeSort(aList    : TList;
                         aFirst   : integer;
                         aLast    : integer;
                         aCompare : TtdCompareFunc);
  {-Optimized mergesort using N/2 extra memory}

procedure TDQuickSortStd(aList    : TList;
                         aFirst   : integer;
                         aLast    : integer;
                         aCompare : TtdCompareFunc);
  {-Standard quicksort}

procedure TDQuickSortNoRecurse(aList    : TList;
                               aFirst   : integer;
                               aLast    : integer;
                               aCompare : TtdCompareFunc);
  {-Non-recursive quicksort}

procedure TDQuickSortRandom(aList    : TList;
                            aFirst   : integer;
                            aLast    : integer;
                            aCompare : TtdCompareFunc);
  {-Quicksort with random selection of pivot}

procedure TDQuickSortMedian(aList    : TList;
                            aFirst   : integer;
                            aLast    : integer;
                            aCompare : TtdCompareFunc);
  {-Quicksort with selection of pivot using median-of-3 algorithm}

procedure TDQuickSort(aList    : TList;
                      aFirst   : integer;
                      aLast    : integer;
                      aCompare : TtdCompareFunc);
  {-Optimized quicksort: non-recursive, median-of-3, cutoff for small
                         subfiles, optimized insertion sort to finish}

procedure TDHeapSort(aList    : TList;
                     aFirst   : integer;
                     aLast    : integer;
                     aCompare : TtdCompareFunc);
  {-Heapsort}

implementation


{===implemented routines (except advanced sorts)=====================}
procedure TDBubbleSort(aList    : TList;
                       aFirst   : integer;
                       aLast    : integer;
                       aCompare : TtdCompareFunc);
var
  i, j : integer;
  Temp : pointer;
  Done : boolean;
begin
  TDValidateListRange(aList, aFirst, aLast, 'TDBubbleSort');
  for i := aFirst to pred(aLast) do begin
    Done := true;
    for j := aLast downto succ(i) do
      if (aCompare(aList.List^[j], aList.List^[j-1]) < 0) then begin
        {swap jth and (j-1)th elements}
        Temp := aList.List^[j];
        aList.List^[j] := aList.List^[j-1];
        aList.List^[j-1] := Temp;
        Done := false;
      end;
    if Done then
      Exit;
  end;
end;
{--------}
procedure TDCombSort(aList    : TList;
                     aFirst   : integer;
                     aLast    : integer;
                     aCompare : TtdCompareFunc);
var
  i, j : integer;
  Temp : pointer;
  Done : boolean;
  Gap  : integer;
begin
  TDValidateListRange(aList, aFirst, aLast, 'TDCombSort');
  {start off with a gap equal to the number of elements}
  Gap := succ(aLast - aFirst);
  repeat
    {assume we'll finish this time around}
    Done := true;
    {calculate the new gap}
    Gap := (longint(Gap) * 10) div 13; {Gap := Trunc(Gap / 1.3);}
    if (Gap < 1) then
      Gap := 1
    else if (Gap = 9) or (Gap = 10) then
      Gap := 11;
    {order every item with its sibling Gap items along}
    for i := aFirst to (aLast - Gap) do begin
      j := i + Gap;
      if (aCompare(aList.List^[j], aList.List^[i]) < 0) then begin
        {swap jth and (j-Gap)th elements}
        Temp := aList.List^[j];
        aList.List^[j] := aList.List^[i];
        aList.List^[i] := Temp;
        {we swapped, so we didn't finish}
        Done := false;
      end;
    end;
  until Done and (Gap = 1);
end;
{--------}
procedure TDInsertionSort(aList    : TList;
                          aFirst   : integer;
                          aLast    : integer;
                          aCompare : TtdCompareFunc);
var
  i, j       : integer;
  IndexOfMin : integer;
  Temp       : pointer;
begin
  TDValidateListRange(aList, aFirst, aLast, 'TDInsertionSort');
  {find the smallest element and put it in the first position}
  IndexOfMin := aFirst;
  for i := succ(aFirst) to aLast do
    if (aCompare(aList.List^[i], aList.List^[IndexOfMin]) < 0) then
      IndexOfMin := i;
  if (aFirst <> IndexOfMin) then begin
    Temp := aList.List^[aFirst];
    aList.List^[aFirst] := aList.List^[IndexOfMin];
    aList.List^[IndexOfMin] := Temp;
  end;
  {now sort via insertion method}
  for i := aFirst+2 to aLast do begin
    Temp := aList.List^[i];
    j := i;
    while (aCompare(Temp, aList.List^[j-1]) < 0) do begin
      aList.List^[j] := aList.List^[j-1];
      dec(j);
    end;
    aList.List^[j] := Temp;
  end;
end;
{--------}
procedure TDInsertionSortStd(aList    : TList;
                             aFirst   : integer;
                             aLast    : integer;
                             aCompare : TtdCompareFunc);
var
  i, j : integer;
  Temp : pointer;
begin
  TDValidateListRange(aList, aFirst, aLast, 'TDInsertionSortStd');
  for i := succ(aFirst) to aLast do begin
    Temp := aList.List^[i];
    j := i;
    while (j > aFirst) and
          (aCompare(Temp, aList.List^[j-1]) < 0) do begin
      aList.List^[j] := aList.List^[j-1];
      dec(j);
    end;
    aList.List^[j] := Temp;
  end;
end;
{--------}
procedure TDSelectionSort(aList    : TList;
                          aFirst   : integer;
                          aLast    : integer;
                          aCompare : TtdCompareFunc);
var
  i, j       : integer;
  IndexOfMin : integer;
  Temp       : pointer;
begin
  TDValidateListRange(aList, aFirst, aLast, 'TDSelectionSort');
  for i := aFirst to pred(aLast) do begin
    IndexOfMin := i;
    for j := succ(i) to aLast do
      if (aCompare(aList.List^[j], aList.List^[IndexOfMin]) < 0) then
        IndexOfMin := j;
    Temp := aList.List^[i];
    aList.List^[i] := aList.List^[IndexOfMin];
    aList.List^[IndexOfMin] := Temp;
  end;
end;
{--------}
procedure TDShakerSort(aList    : TList;
                       aFirst   : integer;
                       aLast    : integer;
                       aCompare : TtdCompareFunc);
var
  i    : integer;
  Temp : pointer;
begin
  TDValidateListRange(aList, aFirst, aLast, 'TDShakerSort');
  while (aFirst < aLast) do begin
    for i := aLast downto succ(aFirst) do
      if (aCompare(aList.List^[i], aList.List^[i-1]) < 0) then begin
        Temp := aList.List^[i];
        aList.List^[i] := aList.List^[i-1];
        aList.List^[i-1] := Temp;
      end;
    inc(aFirst);
    for i := succ(aFirst) to aLast do
      if (aCompare(aList.List^[i], aList.List^[i-1]) < 0) then begin
        Temp := aList.List^[i];
        aList.List^[i] := aList.List^[i-1];
        aList.List^[i-1] := Temp;
      end;
    dec(aLast);
  end;
end;
{--------}
procedure TDShellSort(aList    : TList;
                      aFirst   : integer;
                      aLast    : integer;
                      aCompare : TtdCompareFunc);
var
  i, j : integer;
  h    : integer;
  Temp : pointer;
  Ninth: integer;
begin
  {Note: Shellsort was invented by Donald Shell in 1959.
         This Shellsort implementation uses Knuth's sequence: 1, 4,
         13, 40, 121, ...}
  TDValidateListRange(aList, aFirst, aLast, 'TDShellSort');
  {firstly calculate the first h value we shall use: it'll be about
   one ninth of the number of the elements}
  h := 1;
  Ninth := (aLast - aFirst) div 9;
  while (h <= Ninth) do
    h := (h * 3) + 1;
  {start a loop that'll decrement h by one third each time through}
  while (h > 0) do begin
    {now insertion sort each h-subfile}
    for i := (aFirst + h) to aLast do begin
      Temp := aList.List^[i];
      j := i;
      while (j >= (aFirst+h)) and
            (aCompare(Temp, aList.List^[j-h]) < 0) do begin
        aList.List^[j] := aList.List^[j-h];
        dec(j, h);
      end;
      aList.List^[j] := Temp;
    end;
    {decrease h by a third}
    h := h div 3;
  end;
end;
{====================================================================}


{===Standard mergesort===============================================}
procedure MSS(aList    : TList;
              aFirst   : integer;
              aLast    : integer;
              aCompare : TtdCompareFunc;
              aTempList: PPointerList);
var
  Mid   : integer;
  i, j  : integer;
  ToInx : integer;
  FirstCount : integer;
begin
  {calculate the midpoint}
  Mid := (aFirst + aLast) div 2;
  {recursively mergesort the 1st half and the 2nd half of the list}
  if (aFirst < Mid) then
    MSS(aList, aFirst, Mid, aCompare, aTempList);
  if (succ(Mid) < aLast) then
    MSS(aList, succ(Mid), aLast, aCompare, aTempList);
  {copy the first half of the list to our temporary list}
  FirstCount := succ(Mid - aFirst);
  Move(aList.List^[aFirst], aTempList^[0], FirstCount * sizeof(pointer));
  {set up the indexes: i is the index for the temporary list (ie the
   first half of the list), j is the index for the second half of the
   list, ToInx is the index in the merged where items will be copied}
  i := 0;
  j := succ(Mid);
  ToInx := aFirst;
  {now merge the two lists}
  {repeat until one of the lists empties...}
  while (i < FirstCount) and (j <= aLast) do begin
    {calculate the smaller item from the next items in both lists and
     copy it over; increment the relevant index}
    if (aCompare(aTempList^[i], aList.List^[j]) <= 0) then begin
      aList.List^[ToInx] := aTempList^[i];
      inc(i);
    end
    else begin
      aList.List^[ToInx] := aList.List^[j];
      inc(j);
    end;
    {there's one more item in the merged list}
    inc(ToInx);
  end;
  {if there are any more items in the first list, copy them back over}
  if (i < FirstCount) then
    Move(aTempList^[i], aList.List^[ToInx], (FirstCount - i) * sizeof(pointer));
  {if there are any more items in the second list then they're already
   in place and we're done; if there aren't, we're still done}
end;
{--------}
procedure TDMergeSortStd(aList    : TList;
                         aFirst   : integer;
                         aLast    : integer;
                         aCompare : TtdCompareFunc);
var
  TempList : PPointerList;
  ItemCount: integer;
begin
  TDValidateListRange(aList, aFirst, aLast, 'TDMergeSortStd');
  {if there is at least two items to sort}
  if (aFirst < aLast) then begin
    {create a temporary pointer list}
    ItemCount := succ(aLast - aFirst);
    GetMem(TempList, (succ(ItemCount) div 2) * sizeof(pointer));
    try
      MSS(aList, aFirst, aLast, aCompare, TempList);
    finally
      FreeMem(TempList, (succ(ItemCount) div 2) * sizeof(pointer));
    end;
  end;
end;
{====================================================================}


{===Optimized mergesort==============================================}
const
  MSCutOff = 15;
{--------}
procedure MSInsertionSort(aList    : TList;
                          aFirst   : integer;
                          aLast    : integer;
                          aCompare : TtdCompareFunc);
var
  i, j       : integer;
  IndexOfMin : integer;
  Temp       : pointer;
begin
  {find the smallest element in the list}
  IndexOfMin := aFirst;
  for i := succ(aFirst) to aLast do
    if (aCompare(aList.List^[i], aList.List^[IndexOfMin]) < 0) then
      IndexOfMin := i;
  if (aFirst <> IndexOfMin) then begin
    Temp := aList.List^[aFirst];
    aList.List^[aFirst] := aList.List^[IndexOfMin];
    aList.List^[IndexOfMin] := Temp;
  end;
  {now sort via fast insertion method}
  for i := aFirst+2 to aLast do begin
    Temp := aList.List^[i];
    j := i;
    while (aCompare(Temp, aList.List^[j-1]) < 0) do begin
      aList.List^[j] := aList.List^[j-1];
      dec(j);
    end;
    aList.List^[j] := Temp;
  end;
end;
{--------}
procedure MS(aList    : TList;
             aFirst   : integer;
             aLast    : integer;
             aCompare : TtdCompareFunc;
             aTempList: PPointerList);
var
  Mid   : integer;
  i, j  : integer;
  ToInx : integer;
  FirstCount : integer;
begin
  {calculate the midpoint}
  Mid := (aFirst + aLast) div 2;
  {sort the 1st half of the list, either with mergesort, or, if there
   are few enough items, with insertion sort}
  if (aFirst < Mid) then
    if (Mid-aFirst) <= MSCutOff then
      MSInsertionSort(aList, aFirst, Mid, aCompare)
    else
      MS(aList, aFirst, Mid, aCompare, aTempList);
  {sort the 2nd half of the list likewise}
  if (succ(Mid) < aLast) then
    if (aLast-succ(Mid)) <= MSCutOff then
      MSInsertionSort(aList, succ(Mid), aLast, aCompare)
    else
      MS(aList, succ(Mid), aLast, aCompare, aTempList);
  {copy the first half of the list to our temporary list}
  FirstCount := succ(Mid - aFirst);
  Move(aList.List^[aFirst], aTempList^[0], FirstCount * sizeof(pointer));
  {set up the indexes: i is the index for the temporary list (ie the
   first half of the list), j is the index for the second half of the
   list, ToInx is the index in the merged where items will be copied}
  i := 0;
  j := succ(Mid);
  ToInx := aFirst;
  {now merge the two lists}
  {repeat until one of the lists empties...}
  while (i < FirstCount) and (j <= aLast) do begin
    {calculate the smaller item from the next items in both lists and
     copy it over; increment the relevant index}
    if (aCompare(aTempList^[i], aList.List^[j]) <= 0) then begin
      aList.List^[ToInx] := aTempList^[i];
      inc(i);
    end
    else begin
      aList.List^[ToInx] := aList.List^[j];
      inc(j);
    end;
    {there's one more item in the merged list}
    inc(ToInx);
  end;
  {if there are any more items in the first list, copy them back over}
  if (i < FirstCount) then
    Move(aTempList^[i], aList.List^[ToInx], (FirstCount - i) * sizeof(pointer));
  {if there are any more items in the second list then they're already
   in place and we're done; if there aren't, we're still done}
end;
{--------}
procedure TDMergeSort(aList    : TList;
                      aFirst   : integer;
                      aLast    : integer;
                      aCompare : TtdCompareFunc);
var
  TempList : PPointerList;
  ItemCount: integer;
begin
  TDValidateListRange(aList, aFirst, aLast, 'TDMergeSort');
  {if there is at least two items to sort}
  if (aFirst < aLast) then begin
    {create a temporary pointer list}
    ItemCount := succ(aLast - aFirst);
    GetMem(TempList, (succ(ItemCount) div 2) * sizeof(pointer));
    try
      MS(aList, aFirst, aLast, aCompare, TempList);
    finally
      FreeMem(TempList, (succ(ItemCount) div 2) * sizeof(pointer));
    end;
  end;
end;
{====================================================================}


{===Standard quicksort===============================================}
procedure QSS(aList    : TList;
              aFirst   : integer;
              aLast    : integer;
              aCompare : TtdCompareFunc);
var
  L, R  : integer;
  Pivot : pointer;
  Temp  : pointer;
begin
  {while there are at least two items to sort}
  while (aFirst < aLast) do begin
    {the pivot is the middle item}
    Pivot := aList.List^[(aFirst+aLast) div 2];
    {set indexes and partition}
    L := pred(aFirst);
    R := succ(aLast);
    while true do begin
      repeat dec(R); until (aCompare(aList.List^[R], Pivot) <= 0);
      repeat inc(L); until (aCompare(aList.List^[L], Pivot) >= 0);
      if (L >= R) then Break;
      Temp := aList.List^[L];
      aList.List^[L] := aList.List^[R];
      aList.List^[R] := Temp;
    end;
    {quicksort the first subfile}
    if (aFirst < R) then
      QSS(aList, aFirst, R, aCompare);
    {quicksort the second subfile - recursion removal}
    aFirst := succ(R);
  end;
end;
{--------}
procedure TDQuickSortStd(aList    : TList;
                         aFirst   : integer;
                         aLast    : integer;
                         aCompare : TtdCompareFunc);
begin
  TDValidateListRange(aList, aFirst, aLast, 'TDQuickSortStd');
  QSS(aList, aFirst, aLast, aCompare);
end;
{====================================================================}



{===Non-recursive quicksort==========================================}
procedure QSNR(aList    : TList;
               aFirst   : integer;
               aLast    : integer;
               aCompare : TtdCompareFunc);
var
  L, R  : integer;
  Pivot : pointer;
  Temp  : pointer;
  Stack : array [0..63] of integer; {allows for 2 billion items}
  SP    : integer;
begin
  {initialize stack}
  Stack[0] := aFirst;
  Stack[1] := aLast;
  SP := 2;
  while (SP <> 0) do begin
    {pop off the top subfile}
    dec(SP, 2);
    aFirst := Stack[SP];
    aLast := Stack[SP+1];
    {while there are at least two items to sort}
    while (aFirst < aLast) do begin
      {the pivot is the middle item}
      Pivot := aList.List^[(aFirst+aLast) div 2];
      {set indexes and partition}
      L := pred(aFirst);
      R := succ(aLast);
      while true do begin
        repeat dec(R); until (aCompare(aList.List^[R], Pivot) <= 0);
        repeat inc(L); until (aCompare(aList.List^[L], Pivot) >= 0);
        if (L >= R) then Break;
        Temp := aList.List^[L];
        aList.List^[L] := aList.List^[R];
        aList.List^[R] := Temp;
      end;
      {push the larger subfile onto the stack, go round loop again
       with the smaller subfile}
      if (R - aFirst) < (aLast - R) then begin
        Stack[SP] := succ(R);
        Stack[SP+1] := aLast;
        inc(SP, 2);
        aLast := R;
      end
      else begin
        Stack[SP] := aFirst;
        Stack[SP+1] := R;
        inc(SP, 2);
        aFirst := succ(R);
      end;
    end;
  end;
end;
{--------}
procedure TDQuickSortNoRecurse(aList    : TList;
                               aFirst   : integer;
                               aLast    : integer;
                               aCompare : TtdCompareFunc);
begin
  TDValidateListRange(aList, aFirst, aLast, 'TDQuickSortNoRecurse');
  QSNR(aList, aFirst, aLast, aCompare);
end;
{====================================================================}


{===Randomized quicksort=============================================}
procedure QSR(aList    : TList;
              aFirst   : integer;
              aLast    : integer;
              aCompare : TtdCompareFunc);
var
  L, R  : integer;
  Pivot : pointer;
  Temp  : pointer;
begin
  while (aFirst < aLast) do begin
    {choose a random item, swap with middle to become pivot}
    R := aFirst + Random(aLast - aFirst + 1);
    L := (aFirst + aLast) div 2;
    Pivot := aList.List^[R];
    aList.List^[R] := aList.List^[L];
    aList.List^[L] := Pivot;
    {set indexes and partition about the pivot}
    L := pred(aFirst);
    R := succ(aLast);
    while true do begin
      repeat dec(R); until (aCompare(aList.List^[R], Pivot) <= 0);
      repeat inc(L); until (aCompare(aList.List^[L], Pivot) >= 0);
      if (L >= R) then Break;
      Temp := aList.List^[L];
      aList.List^[L] := aList.List^[R];
      aList.List^[R] := Temp;
    end;
    {quicksort the first subfile}
    if (aFirst < R) then
      QSR(aList, aFirst, R, aCompare);
    {quicksort the second subfile - recursion removal}
    aFirst := succ(R);
  end;
end;
{--------}
procedure TDQuickSortRandom(aList    : TList;
                            aFirst   : integer;
                            aLast    : integer;
                            aCompare : TtdCompareFunc);
begin
  TDValidateListRange(aList, aFirst, aLast, 'TDQuickSortRandom');
  QSR(aList, aFirst, aLast, aCompare);
end;
{====================================================================}


{===Median of 3 quicksort============================================}
procedure QSM(aList    : TList;
              aFirst   : integer;
              aLast    : integer;
              aCompare : TtdCompareFunc);
var
  L, R  : integer;
  Pivot : pointer;
  Temp  : pointer;
begin
  while (aFirst < aLast) do begin
    {if there are three or more items, select the pivot as being the
     median of the first, last and middle items and store it in the
     middle}
    if (aLast - aFirst) >= 2 then begin
      R := (aFirst + aLast) div 2;
      if (aCompare(aList.List^[aFirst], aList.List^[R]) > 0) then begin
        Temp := aList.List^[aFirst];
        aList.List^[aFirst] := aList.List^[R];
        aList.List^[R] := Temp;
      end;
      if (aCompare(aList.List^[aFirst], aList.List^[aLast]) > 0) then begin
        Temp := aList.List^[aFirst];
        aList.List^[aFirst] := aList.List^[aLast];
        aList.List^[aLast] := Temp;
      end;
      if (aCompare(aList.List^[R], aList.List^[aLast]) > 0) then begin
        Temp := aList.List^[R];
        aList.List^[R] := aList.List^[aLast];
        aList.List^[aLast] := Temp;
      end;
      Pivot := aList.List^[R];
    end
    {otherwise, there are only 2 items, so choose the first item as
     the pivot}
    else
      Pivot := aList.List^[aFirst];
    {set indexes and partition about the pivot}
    L := pred(aFirst);
    R := succ(aLast);
    while true do begin
      repeat dec(R); until (aCompare(aList.List^[R], Pivot) <= 0);
      repeat inc(L); until (aCompare(aList.List^[L], Pivot) >= 0);
      if (L >= R) then Break;
      Temp := aList.List^[L];
      aList.List^[L] := aList.List^[R];
      aList.List^[R] := Temp;
    end;
    {quicksort the first subfile}
    if (aFirst < R) then
      QSM(aList, aFirst, R, aCompare);
    {quicksort the second subfile - recursion removal}
    aFirst := succ(R);
  end;
end;
{--------}
procedure TDQuickSortMedian(aList    : TList;
                            aFirst   : integer;
                            aLast    : integer;
                            aCompare : TtdCompareFunc);
begin
  TDValidateListRange(aList, aFirst, aLast, 'TDQuickSortMedian');
  QSM(aList, aFirst, aLast, aCompare);
end;
{====================================================================}


{===Optimized quicksort==============================================}
const
  QSCutOff = 15;
{--------}
procedure QSInsertionSort(aList    : TList;
                          aFirst   : integer;
                          aLast    : integer;
                          aCompare : TtdCompareFunc);
var
  i, j       : integer;
  IndexOfMin : integer;
  Temp       : pointer;
begin
  {find the smallest element in the first QSCutOff items and put it in
   the first position}
  IndexOfMin := aFirst;
  j := QSCutOff;
  if (j > aLast) then
    j := aLast;
  for i := succ(aFirst) to j do
    if (aCompare(aList.List^[i], aList.List^[IndexOfMin]) < 0) then
      IndexOfMin := i;
  if (aFirst <> IndexOfMin) then begin
    Temp := aList.List^[aFirst];
    aList.List^[aFirst] := aList.List^[IndexOfMin];
    aList.List^[IndexOfMin] := Temp;
  end;
  {now sort via fast insertion method}
  for i := aFirst+2 to aLast do begin
    Temp := aList.List^[i];
    j := i;
    while (aCompare(Temp, aList.List^[j-1]) < 0) do begin
      aList.List^[j] := aList.List^[j-1];
      dec(j);
    end;
    aList.List^[j] := Temp;
  end;
end;
{--------}
procedure QS(aList    : TList;
             aFirst   : integer;
             aLast    : integer;
             aCompare : TtdCompareFunc);
var
  L, R  : integer;
  Pivot : pointer;
  Temp  : pointer;
  Stack : array [0..63] of integer; {allows for 2 billion items}
  SP    : integer;
begin
  {initialize stack}
  Stack[0] := aFirst;
  Stack[1] := aLast;
  SP := 2;

  {while there are subfiles on the stack...}
  while (SP <> 0) do begin

    {pop off the top subfile}
    dec(SP, 2);
    aFirst := Stack[SP];
    aLast := Stack[SP+1];

    {repeat while there are sufficient items in the subfile...}
    while ((aLast - aFirst) > QSCutOff) do begin

      {sort the first, middle and last items, then set the pivot to
       the middle one - the median-of-3 method}
      R := (aFirst + aLast) div 2;
      if (aCompare(aList.List^[aFirst], aList.List^[R]) > 0) then begin
        Temp := aList.List^[aFirst];
        aList.List^[aFirst] := aList.List^[R];
        aList.List^[R] := Temp;
      end;
      if (aCompare(aList.List^[aFirst], aList.List^[aLast]) > 0) then begin
        Temp := aList.List^[aFirst];
        aList.List^[aFirst] := aList.List^[aLast];
        aList.List^[aLast] := Temp;
      end;
      if (aCompare(aList.List^[R], aList.List^[aLast]) > 0) then begin
        Temp := aList.List^[R];
        aList.List^[R] := aList.List^[aLast];
        aList.List^[aLast] := Temp;
      end;
      Pivot := aList.List^[R];

      {set indexes and partition}
      L := aFirst;
      R := aLast;
      while true do begin
        repeat dec(R); until (aCompare(aList.List^[R], Pivot) <= 0);
        repeat inc(L); until (aCompare(aList.List^[L], Pivot) >= 0);
        if (L >= R) then Break;
        Temp := aList.List^[L];
        aList.List^[L] := aList.List^[R];
        aList.List^[R] := Temp;
      end;

      {push the larger subfile onto the stack, go round loop again
       with the smaller subfile}
      if (R - aFirst) < (aLast - R) then begin
        Stack[SP] := succ(R);
        Stack[SP+1] := aLast;
        inc(SP, 2);
        aLast := R;
      end
      else begin
        Stack[SP] := aFirst;
        Stack[SP+1] := R;
        inc(SP, 2);
        aFirst := succ(R);
      end;
    end;
  end;
end;
{--------} (****
procedure QS(aList    : TList;
             aFirst   : integer;
             aLast    : integer;
             aCompare : TtdCompareFunc);
var
  L, R  : integer;
  Pivot : pointer;
  Temp  : pointer;
begin
  while ((aLast - aFirst) > QSCutOff) do begin
    {sort the first, middle and last items, then set the pivot to the
     middle one - the median-of-3 method}
    R := (aFirst + aLast) div 2;
    if (aCompare(aList.List^[aFirst], aList.List^[R]) > 0) then begin
      Temp := aList.List^[aFirst];
      aList.List^[aFirst] := aList.List^[R];
      aList.List^[R] := Temp;
    end;
    if (aCompare(aList.List^[aFirst], aList.List^[aLast]) > 0) then begin
      Temp := aList.List^[aFirst];
      aList.List^[aFirst] := aList.List^[aLast];
      aList.List^[aLast] := Temp;
    end;
    if (aCompare(aList.List^[R], aList.List^[aLast]) > 0) then begin
      Temp := aList.List^[R];
      aList.List^[R] := aList.List^[aLast];
      aList.List^[aLast] := Temp;
    end;
    Pivot := aList.List^[R];
    {set indexes and partition}
    L := aFirst;
    R := aLast;
    while true do begin
      repeat dec(R); until (aCompare(aList.List^[R], Pivot) <= 0);
      repeat inc(L); until (aCompare(aList.List^[L], Pivot) >= 0);
      if (L >= R) then Break;
      Temp := aList.List^[L];
      aList.List^[L] := aList.List^[R];
      aList.List^[R] := Temp;
    end;
    {quicksort the first subfile}
    QS(aList, aFirst, R, aCompare);
    {quicksort the second subfile - recursion removal}
    aFirst := succ(R);
  end;
end; ***)
{--------}
procedure TDQuickSort(aList    : TList;
                      aFirst   : integer;
                      aLast    : integer;
                      aCompare : TtdCompareFunc);
begin
  TDValidateListRange(aList, aFirst, aLast, 'TDQuickSort');
  QS(aList, aFirst, aLast, aCompare);
  QSInsertionSort(aList, aFirst, aLast, aCompare);
end;
{====================================================================}


{===Heapsort=========================================================}
procedure HSTrickleDown(aList    : PPointerList;
                        aFromInx : integer;
                        aCount   : integer;
                        aCompare : TtdCompareFunc);
var
  Item     : pointer;
  ChildInx : integer;
  ParentInx: integer;
begin
  {first do a simple trickle down continually replacing parent with
   larger child until we reach the bottom level of the heap}
  Item := aList^[aFromInx];
  ChildInx := (aFromInx * 2) + 1;
  while (ChildInx < aCount) do begin
    if (succ(ChildInx) < aCount) and
       (aCompare(aList^[ChildInx], aList^[succ(ChildInx)]) < 0) then
      inc(ChildInx);
    aList^[aFromInx] := aList^[ChildInx];
    aFromInx := ChildInx;
    ChildInx := (aFromInx * 2) + 1;
  end;
  {now bubble up from where we ended up}
  ParentInx := (aFromInx - 1) div 2;
  while (aFromInx > 0) and
        (aCompare(Item, aList^[ParentInx]) > 0) do begin
    aList^[aFromInx] := aList^[ParentInx];
    aFromInx := ParentInx;
    ParentInx := (aFromInx - 1) div 2;
  end;
  {save the item where we ended up after the bubble up}
  aList^[aFromInx] := Item;
end;
{--------}
procedure HSTrickleDownStd(aList    : PPointerList;
                           aFromInx : integer;
                           aCount   : integer;
                           aCompare : TtdCompareFunc);
var
  Item     : pointer;
  ChildInx : integer;
begin
  Item := aList^[aFromInx];
  ChildInx := (aFromInx * 2) + 1;
  while (ChildInx < aCount) do begin
    if (succ(ChildInx) < aCount) and
       (aCompare(aList^[ChildInx], aList^[succ(ChildInx)]) < 0) then
      inc(ChildInx);
    if aCompare(Item, aList^[ChildInx]) >= 0 then
      Break;
    aList^[aFromInx] := aList^[ChildInx];
    aFromInx := ChildInx;
    ChildInx := (aFromInx * 2) + 1;
  end;
  aList^[aFromInx] := Item;
end;
{--------}
procedure TDHeapSort(aList    : TList;
                     aFirst   : integer;
                     aLast    : integer;
                     aCompare : TtdCompareFunc);
var
  ItemCount : integer;
  Inx       : integer;
  Temp      : pointer;
begin
  TDValidateListRange(aList, aFirst, aLast, 'TDHeapSort');
  {convert the list into a heap}
  ItemCount := aLast - aFirst + 1;
  for Inx := pred(ItemCount div 2) downto 0 do
    HSTrickleDownStd(@aList.List^[aFirst], Inx, ItemCount, aCompare);
  {now remove the items one at a time from the heap, placing them at
   the end of the array}
  for Inx := pred(ItemCount) downto 0 do begin
    Temp := aList.List^[aFirst];
    aList.List^[aFirst] := aList.List^[aFirst+Inx];
    aList.List^[aFirst+Inx] := Temp;
    HSTrickleDown(@aList.List^[aFirst], 0, Inx, aCompare);
  end;
end;
{====================================================================}


end.
