//*****************************************************************//
//                                                                 //
//  Sorting Algorithms in Delphi                                   //
//  Copyright© BrandsPatch LLC                                     //
//  http://www.explainth.at                                        //
//                                                                 //
//  All Rights Reserved                                            //
//                                                                 //
//  Permission is granted to use, modify and redistribute          //
//  the code in this Delphi unit on the condition that this        //
//  notice is retained unchanged.                                  //
//                                                                 //
//  BrandsPatch declines all responsibility for any losses,        //
//  direct or indirect, that may arise  as a result of using       //
//  this code.                                                     //
//                                                                 //
//*****************************************************************//
unit Sorts;

interface

uses Windows,SysUtils;

type TSortArray = array[0..8191] of Double;

type TSortProc = procedure(var Vals:TSortArray;ACount:Integer);

procedure BubbleSort(var Vals:TSortArray;ACount:Integer);
procedure SelSort(var Vals:TSortArray;ACount:Integer);
procedure InsertSort(var Vals:TSortArray;ACount:Integer);
procedure HeapSort(var Vals:TSortArray;ACount:Integer);
procedure MergeSort(var Vals:TSortArray;ACount:Integer);
procedure QuickSort(var Vals:TSortArray;ACount:Integer);
procedure ShellSort(var Vals:TSortArray;ACount:Integer);

implementation

//******************************
//
// O(n^2) Sorts - time required roughly propotional to n^2
// where n is the number of items to be sorted
//
//******************************

procedure BubbleSort(var Vals:TSortArray;ACount:Integer);
var i,j,k:Integer;
    Hold:Double;
begin
  for i:=ACount - 1 downto 0 do
  begin
    for j:=1 to i do
    begin
      k:=j - 1;
      if (Vals[k] <= Vals[j]) then Continue;
      Hold:=Vals[k];
      Vals[k]:=Vals[j];
      Vals[j]:=Hold;
      {Starting from the bottom of the array each time we
       find a smaller value lower down the array we cause
       it to "bubble-up". Conversely bigger values "sink".
       The problem is that both sinking nor bubbling
       occur in steps of one index shift so the process
       is slow with multiple value exchanges at each step}
    end;
  end;
end;

procedure SelSort(var Vals:TSortArray;ACount:Integer);
var i,j,AMin:Integer;
    Hold:Double;
begin
  for i:=0 to ACount - 2 do
  begin
    AMin:=i;
    for j:=i + 1 to ACount - 1 do
    if (Vals[j] < Vals[AMin]) then AMin:=j;
    //find smallest Vals in subsequent members of the array
    Hold:=Vals[i];
    Vals[i]:=Vals[AMin];
    Vals[AMin]:=i;
    //move the smallest value to its rightful position
    {If AMin has not changed, still = i, we end up doing a pointless
     swap. However, it turns out more economical than running an if
     test and issuing a Continue

     As a general rule there is no good reason for using this sort.
     Consider the Inertion sort instead}
  end;
end;

procedure InsertSort(var Vals:TSortArray;ACount:Integer);
var i,j,k:Integer;
    Hold:Double;
begin
  for i:=1 to ACount - 1 do
  begin
    Hold:=Vals[i];
    j:=i;k:=j - 1;
    while ((j > 0) and (Vals[k] > Hold)) do
    begin
      Vals[j]:=Vals[k];
      dec(j);
      dec(k);
    end;
    Vals[j]:=Hold;
  end;
  {Better than the bubble sort because we move one
  value at a time to its correct position in the array.
  There are no value exchanges and only one value
  assignment per iteratin of the inner loop

  As a general rule the best of the O(n^2) sorts}
end;
//***********************************************************
//
// O(nlog2n) sorts - time required roughly proportional to
// n*log2n where n is the number of items to be sorted
//
//***********************************************************
procedure HeapSort(var Vals:TSortArray;ACount:Integer);
var i,ACountM,HalfCount,L:Integer;
    Hold,M:Double;

  procedure BuildHeap(p:Integer);
  begin
    M:=Vals[p];
    //the value in the (P)arent node
    while (p <= HalfCount) do
    {the lower half of the array represents the bottom of the heap
     which will get ordered anyway}
    begin
      L:=p shl 1 + 1;
      //for a 0 based array the children are at 2p + 1 and 2p + 2
      inc(L,ord((L < ACountM) and (Vals[L + 1] > Vals[L])));
      //L is the (L)eft child. L + 1, if  it exists, is the right child
      if ((M >= Vals[L]) or (L >= ACount)) then break;
      //parent bigger than both children so subtree is in order
      Vals[p]:=Vals[L];
      Vals[L]:=M;
      //swap parent with bigger child, L or L + 1
      p:=L;
      {now examine the subtree for that child, if it exists

       Note that we do need to update M - the original parent
       moved down the heap and became a child and we are now
       testing ITS children}
    end;
  end;

  procedure RestoreHeap(p,n:Integer);
  begin
    M:=Vals[p];
    {As before, p = Parent, L = Left Child & M = parent value
     However, we need to leave out array elements from Vals[n+1]
     onwards since they are already ordered and should not go
     back onto the heap.}
    while (p <= n shr 1) do
    begin
      L:=p shl 1 + 1;
      inc(L,ord((L < n) and (Vals[L + 1] > Vals[L])));
      //if the right child exists & is the bigger of the two
      //arithmetic on L is faster than doing an if test here
      if ((M >= Vals[L]) or (L > n)) then break;
      {here we risk finding a "child" that actually belongs to
       already ordered portion of the array. To avoid this we
       check L > n}
      Vals[p]:=Vals[L];
      Vals[L]:=M;
      p:=L;
    end;
  end;

begin
  HalfCount:=ACount shr 1;
  ACountM:=ACount - 1;
  for i:=HalfCount downto 0 do BuildHeap(i);
  {at this point we have a heap. The heap is in an array
   which itself is unordered}
  for i:=ACountM downto 1 do
  begin
    Hold:=Vals[i];
    Vals[i]:=Vals[0];
    Vals[0]:=Hold;
    {at each iteration Vals[0] contains the top of the heap. We move
     it to the end of the heap - for starters, the end of the array
     and subsequently one position up at each iteration

     With this done what is left of the array is no longer a valid
     heap - we just orphaned the child nodes at Vals[2] and Vals[3]
     by taking away their parrent - so we rebuild the heap. However,
     we should not do this for a heap with just one element or else
     we would disorder the already ordered array.

     This sort does not use recursion - the other O(nlog2n) sorts,
     except the Shell sort, do. This can be a consideration with
     large datasets since recursion places a considerable performance
     overhead}
    if (i > 1) then RestoreHeap(0,i-1);
  end;
end;

procedure MergeSort(var Vals:TSortArray;ACount:Integer);
var AVals:TSortArray;

  procedure Merge(ALo,AMid,AHi:Integer);
  var i,j,k,m:Integer;
  begin
    i:=0;
    for j:=ALo to AMid do
    begin
      AVals[i]:=Vals[j];
      inc(i);
      //copy lower half or Vals into temporary array AVals
    end;

    i:=0;j:=AMid + 1;k:=ALo;//j could be undefined after the for loop!
    while ((k < j) and (j <= AHi)) do
    if (AVals[i] <= Vals[j]) then
    begin
      Vals[k]:=AVals[i];
      inc(i);inc(k);
    end else
    begin
      Vals[k]:=Vals[j];
      inc(k);inc(j);
    end;
    {locate next greatest value in Vals or AVals and copy it to the
     right position.}

    for m:=k to j - 1 do
    begin
      Vals[m]:=AVals[i];
      inc(i);
    end;
    //copy back any remaining, unsorted, elements
  end;

  procedure PerformMergeSort(ALo,AHi:Integer);
  var AMid:Integer;
  begin
    if (ALo < AHi) then
    begin
      AMid:=(ALo + AHi) shr 1;
      PerformMergeSort(ALo,AMid);
      PerformMergeSort(AMid + 1,AHi);
      Merge(ALo,AMid,AHi);
    end;
  end;

begin
  PerformMergeSort(0,ACount - 1);
end;

procedure QuickSort(var Vals:TSortArray;ACount:Integer);

  procedure PerformQuickSort(ALo,AHi:Integer);
  var i,j,k,m:Integer;
      Hold:Double;
  begin
    i:=ALo;j:=AHi;
    Hold:=Vals[(ALo + AHi) shr 1];
    //for starters take the midpoint value as the comparison value CV

    repeat
      repeat
        m:=ord(Vals[i] < Hold) or (ord(Vals[j] > Hold) shl 1);
        inc(i,ord(m and 1 > 0));
        dec(j,ord(m and 2 > 0));
        {Most implementations use two while loops. For moderately
         large values of ACount there is a significant benefit in
         using a single repeat loop and some math as we have done
         here.}
      until (m = 0);
      {We do the following

       a. Increase i until Vals[i] >= CV
       b. Decrease j until Vals[j] <= CV
      }


      if (i <= j) then
      begin
        Hold:=Vals[i];
        Vals[i]:=Vals[j];
        Vals[j]:=Hold;
        inc(i);dec(j);
      {If i is to the left of, or same as, j then we swap Vals
       at i & j. Incidentally this gives us a new CV}
      end;
    until (i > j);
    //keep repeating until i > j

    if (ALo < j) then PerformQuickSort(ALo,j);
    if (i < AHi) then PerformQuickSort(i,AHi);
  end;

begin
  PerformQuickSort(0,ACount - 1);
end;

type TShellCols = array[0..15] of Integer;

procedure ShellSort(var Vals:TSortArray;ACount:Integer);
var i,j,k,h,v:Integer;
    Hold:Double;
const ShellCols:TShellCols = (1391376,463792,198768,86961,
                              33936,13776,4592,1968,861,336,
                              112,48,21,7,3,1);
//predefine the column sequence to be used
begin
  for k:=0 to 15 do
  begin
    h:=ShellCols[k];
    {the inner loop will not execute if the number of columns is
     greater than ACount.}
    for i:=h to ACount - 1 do
    begin
      Hold:=Vals[i];
      j:=i;
      while ((j >= h) and (Vals[j-h] > HOld)) do
      begin
        Vals[j]:=Vals[j-h];
        dec(j,h);
      end;
      {In the inner loop we do a simplified insertion sort}
      Vals[j]:=Hold;
    end;
  end;
end;

end.
