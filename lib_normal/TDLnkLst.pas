(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDLnkLst                                                         *)
(* Linked list classes                                              *)
(********************************************************************)

unit TDLnkLst;

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

{$DEFINE UseNodeManager}

type
  PslNode = ^TslNode;      {a node with one link}
  TslNode = packed record
    slnNext : PslNode;
    slnData : pointer;
  end;

  PdlNode = ^TdlNode;      {a node with two links}
  TdlNode = packed record
    dlnNext  : PdlNode;
    dlnPrior : PdlNode;
    dlnData  : pointer;
  end;

type
  TtdSingleLinkList = class
    private
      FCount   : longint;
      FCursor  : PslNode;
      FCursorIx: longint;
      FDispose : TtdDisposeProc;
      FHead    : PslNode;
      FIsSorted: boolean;
      FName    : TtdNameString;
      FParent  : PslNode;
    protected
      function sllGetItem(aIndex : longint) : pointer;
      procedure sllSetItem(aIndex : longint; aItem  : pointer);

      procedure sllError(aErrorCode  : integer;
                   const aMethodName : TtdNameString);
      class procedure sllGetNodeManager;
      function sllMerge(aCompare    : TtdCompareFunc;
                        aPriorNode1 : PslNode; aCount1 : longint;
                        aPriorNode2 : PslNode; aCount2 : longint)
                                                            : PslNode;
      function sllMergesort(aCompare   : TtdCompareFunc;
                            aPriorNode : PslNode;
                            aCount     : longint) : PslNode;
      procedure sllPositionAtNth(aIndex : longint);
    public
      constructor Create(aDispose : TtdDisposeProc);
      destructor Destroy; override;

      function Add(aItem : pointer) : longint;
      procedure Clear;
      procedure Delete(aIndex : longint);
      procedure DeleteAtCursor;
      function Examine : pointer;
      function First : pointer;
      function IndexOf(aItem : pointer) : longint;
      procedure Insert(aIndex : longint; aItem : pointer);
      procedure InsertAtCursor(aItem : pointer);
      procedure InsertSorted(aItem : pointer;
                             aCompare : TtdCompareFunc);
      function IsAfterLast : boolean;
      function IsBeforeFirst : boolean;
      function IsEmpty : boolean;
      function Last : pointer;
      function Locate(aItem : pointer;
                      aCompare : TtdCompareFunc) : longint;
      procedure MoveBeforeFirst;
      procedure MoveNext;
      procedure Remove(aItem : pointer);

      procedure InsertionSort(aCompare : TtdCompareFunc);
      procedure Sort(aCompare : TtdCompareFunc);

      property Count : longint
         read FCount;
      property IsSorted : boolean
         read FIsSorted;
      property Items[aIndex : longint] : pointer
         read sllGetItem write sllSetItem;
         default;
      property Name : TtdNameString
         read FName write FName;
  end;

type
  TtdDoubleLinkList = class
    private
      FCount   : longint;
      FCursor  : PdlNode;
      FCursorIx: longint;
      FDispose : TtdDisposeProc;
      FHead    : PdlNode;
      FIsSorted: boolean;
      FName    : TtdNameString;
      FTail    : PdlNode;
    protected
      function dllGetItem(aIndex : longint) : pointer;
      procedure dllSetItem(aIndex : longint; aItem  : pointer);

      procedure dllError(aErrorCode  : integer;
                   const aMethodName : TtdNameString);
      class procedure dllGetNodeManager;
      function dllMerge(aCompare    : TtdCompareFunc;
                        aPriorNode1 : PdlNode; aCount1 : longint;
                        aPriorNode2 : PdlNode; aCount2 : longint)
                                                            : PdlNode;
      function dllMergesort(aCompare   : TtdCompareFunc;
                            aPriorNode : PdlNode;
                            aCount     : longint) : PdlNode;
      procedure dllPositionAtNth(aIndex : longint);
    public
      constructor Create(aDispose : TtdDisposeProc);
      destructor Destroy; override;

      function Add(aItem : pointer) : longint;
      procedure Clear;
      procedure Delete(aIndex : longint);
      procedure DeleteAtCursor;
      function Examine : pointer;
      function First : pointer;
      function IndexOf(aItem : pointer) : longint;
      procedure Insert(aIndex : longint; aItem : pointer);
      procedure InsertAtCursor(aItem : pointer);
      procedure InsertSorted(aItem : pointer;
                             aCompare : TtdCompareFunc);
      function IsAfterLast : boolean;
      function IsBeforeFirst : boolean;
      function IsEmpty : boolean;
      function Last : pointer;
      function Locate(aItem : pointer;
                      aCompare : TtdCompareFunc) : longint;
      procedure MoveAfterLast;
      procedure MoveBeforeFirst;
      procedure MoveNext;
      procedure MovePrior;
      procedure Remove(aItem : pointer);

      procedure InsertionSort(aCompare : TtdCompareFunc);
      procedure Sort(aCompare : TtdCompareFunc);

      property Count : longint
         read FCount;
      property IsSorted : boolean
         read FIsSorted;
      property Items[aIndex : longint] : pointer
         read dllGetItem write dllSetItem;
         default;
      property Name : TtdNameString
         read FName write FName;
  end;

function TDSLLSearch(aList : TtdSingleLinkList;
                     aItem : pointer;
                     aCompare : TtdCompareFunc) : boolean;

function TDSLLSortedSearch(aList : TtdSingleLinkList;
                           aItem : pointer;
                           aCompare : TtdCompareFunc) : boolean;

function TDDLLSearch(aList : TtdSingleLinkList;
                     aItem : pointer;
                     aCompare : TtdCompareFunc) : boolean;

function TDDLLSortedSearch(aList : TtdSingleLinkList;
                           aItem : pointer;
                           aCompare : TtdCompareFunc) : boolean;


implementation

uses
  TDNdeMgr;

const
  UnitName = 'TDLnkLst';

var
  SLNodeManager : TtdNodeManager; {nodemanager for singlylinked lists}
  DLNodeManager : TtdNodeManager; {nodemanager for doublylinked lists}

{===TtdSingleLinkList================================================}
constructor TtdSingleLinkList.Create(aDispose : TtdDisposeProc);
begin
  inherited Create;
  {save the dispose procedure}
  FDispose := aDispose;
  {get the node manager}
  sllGetNodeManager;
  {allocate a head node}
  FHead := PslNode(SLNodeManager.AllocNode);
  FHead^.slnNext := nil;
  FHead^.slnData := nil;
  {set the cursor}
  MoveBeforeFirst;
  FIsSorted := true;
end;
{--------}
destructor TtdSingleLinkList.Destroy;
begin
  {delete all the nodes, including the head node}
  if (Count <> 0) then
    Clear;
  SLNodeManager.FreeNode(FHead);
  inherited Destroy;
end;
{--------}
function TtdSingleLinkList.Add(aItem : pointer) : longint;
var
  WorkCursor : PslNode;
  WorkParent : PslNode;
begin
  {use work variables for speed}
  WorkCursor := FCursor;
  WorkParent := FParent;
  {move to the very end of the linked list}
  while (WorkCursor <> nil) do begin
    WorkParent := WorkCursor;
    WorkCursor := WorkCursor^.slnNext;
  end;
  {set the real cursor}
  FParent := WorkParent;
  FCursor := nil;
  FCursorIx := Count;
  Result := Count;
  {insert at the cursor}
  InsertAtCursor(aItem);
  FIsSorted := false;
end;
{--------}
procedure TtdSingleLinkList.Clear;
var
  Temp : PslNode;
begin
  {delete all the nodes, except the head node; if we can dispose of
   data, do so}
  Temp := FHead^.slnNext;
  while (Temp <> nil) do begin
    FHead^.slnNext := Temp^.slnNext;
    if Assigned(FDispose) then
      FDispose(Temp^.slnData);
    SLNodeManager.FreeNode(Temp);
    Temp := FHead^.slnNext;
  end;
  FCount := 0;
  MoveBeforeFirst;
  FIsSorted := true;
end;
{--------}
procedure TtdSingleLinkList.Delete(aIndex : longint);
begin
  {position the cursor}
  sllPositionAtNth(aIndex);
  {delete the item at the cursor}
  DeleteAtCursor;
end;
{--------}
procedure TtdSingleLinkList.DeleteAtCursor;
begin
  if (FCursor = nil) or (FCursor = FHead) then
    sllError(tdeListCannotDelete, 'Delete');
  {dispose of its contents}
  if Assigned(FDispose) then
    FDispose(FCursor^.slnData);
  {unlink the node and free it}
  FParent^.slnNext := FCursor^.slnNext;
  SLNodeManager.FreeNode(FCursor);
  FCursor := FParent^.slnNext;
  dec(FCount);
  if (Count <= 1) then
    FIsSorted := true;
end;
{--------}
function TtdSingleLinkList.Examine : pointer;
begin
  if (FCursor = nil) or (FCursor = FHead) then
    sllError(tdeListCannotExamine, 'Examine');
  {return the data part of the cursor}
  Result := FCursor^.slnData;
end;
{--------}
function TtdSingleLinkList.First : pointer;
begin
  {position the cursor}
  sllPositionAtNth(0);
  {return the data}
  Result := FCursor^.slnData;
end;
{--------}
function TtdSingleLinkList.IndexOf(aItem : pointer) : longint;
var
  WorkCursor   : PslNode;
  WorkParent   : PslNode;
  WorkCursorIx : longint;
begin
  {set the work cursor to the first node (if it exists)}
  WorkParent := FHead;
  WorkCursor := WorkParent^.slnNext;
  WorkCursorIx := 0;
  {walk the linked list looking for the item}
  while (WorkCursor <> nil) do begin
    if (WorkCursor^.slnData = aItem) then begin
      {we found it; set the result; set the real cursor}
      Result := WorkCursorIx;
      FCursor := WorkCursor;
      FParent := WorkParent;
      FCursorIx := WorkCursorIx;
      Exit;
    end;
    {advance to the next node}
    WorkParent := WorkCursor;
    WorkCursor := WorkCursor^.slnNext;
    inc(WorkCursorIx);
  end;
  {didn't find it}
  Result := -1;
end;
{--------}
procedure TtdSingleLinkList.Insert(aIndex : longint; aItem : pointer);
begin
  {position the cursor}
  sllPositionAtNth(aIndex);
  {insert the item at the cursor}
  InsertAtCursor(aItem);
  FIsSorted := false;
end;
{--------}
procedure TtdSingleLinkList.InsertAtCursor(aItem : pointer);
var
  NewNode : PslNode;
begin
  {make sure we aren't trying to insert at the before first position;
   if we're there, move on one position}
  if (FCursor = FHead) then
    MoveNext;
  {allocate a new node and insert at the cursor}
  NewNode := PslNode(SLNodeManager.AllocNode);
  NewNode^.slnData := aItem;
  NewNode^.slnNext := FCursor;
  FParent^.slnNext := NewNode;
  FCursor := NewNode;
  inc(FCount);
end;
{--------}
procedure TtdSingleLinkList.InsertionSort(aCompare : TtdCompareFunc);
var
  Walker : PslNode;
  Temp   : PslNode;
  WalkerParent : PslNode;
  TempParent   : PslNode;
begin
  {if there are zero (or one) items the list is already sorted}
  if (Count <= 1) then begin
    FIsSorted := true;
    Exit;
  end;
  {perform an insertion sort from the second item onwards}
  WalkerParent := FHead^.slnNext;
  Walker := WalkerParent^.slnNext;
  while (Walker <> nil) do begin
    {find where the walker item should be in the sorted list to its
     left - we walk the sorted sublist making a note of the parent as
     we go so that we can insert properly. Note that the loop below
     will terminate in the worst case by the walker node itself - we
     won't run off the end of the list}
    TempParent := FHead;
    Temp := TempParent^.slnNext;
    while (aCompare(Temp^.slnData, Walker^.slnData) < 0) do begin
      TempParent := Temp;
      Temp := TempParent^.slnNext;
    end;
    {did we find the walker node? If so, it's in the right place so
     move the walker's parent on by one link}
    if (Temp = Walker) then
      WalkerParent := Walker
    {otherwise, move the walker node into the correct place in the
     sorted sublist; leave the walker's parent where it is}
    else begin
      {disconnect the walker node}
      WalkerParent^.slnNext := Walker^.slnNext;
      {connect the walker node in the correct place}
      Walker^.slnNext := Temp;
      TempParent^.slnNext := Walker;
    end;
    {set the walker node}
    Walker := WalkerParent^.slnNext;
  end;
  MoveBeforeFirst;
  FIsSorted := true;
end;
{--------}
procedure TtdSingleLinkList.InsertSorted(aItem : pointer;
                                         aCompare : TtdCompareFunc);
begin
  if not IsSorted then
    sllError(tdeListIsNotSorted, 'InsertSorted');
  if Locate(aItem, aCompare) = -1 then
    InsertAtCursor(aItem);
end;
{--------}
function TtdSingleLinkList.IsBeforeFirst : boolean;
begin
  Result := FCursor = FHead;
end;
{--------}
function TtdSingleLinkList.IsEmpty : boolean;
begin
  Result := (Count = 0);
end;
{--------}
function TtdSingleLinkList.IsAfterLast : boolean;
begin
  Result := FCursor = nil;
end;
{--------}
function TtdSingleLinkList.Last : pointer;
begin
  {position the cursor}
  sllPositionAtNth(pred(Count));
  {return the data}
  Result := FCursor^.slnData;
end;
{--------}
function TtdSingleLinkList.Locate(aItem    : pointer;
                                  aCompare : TtdCompareFunc) : longint;
var
  BLCursor : PslNode;
  BLCursorIx : longint;
  WorkCursor : PslNode;
  WorkParent : PslNode;
  WorkCursorIx : longint;
  ListCount    : longint;
  MidPoint     : longint;
  i            : integer;
  CompareResult: integer;
begin
  WorkParent := nil;
  {there are two different ways of doing this, depending on whether
   the list is sorted or not; if sorted, we perform a binary search,
   if not, we perform a sequential search}
  {if sorted, do a binary search}
  if IsSorted then begin
    {prepare}
    BLCursor := FHead;
    BLCursorIx := -1;
    ListCount := Count;
    {while there are still nodes to check...}
    while (ListCount <> 0) do begin
      {calculate the midpoint; it will be at least 1}
      MidPoint := (ListCount + 1) div 2;
      {move that many nodes along}
      WorkCursor := BLCursor;
      WorkCursorIx := BLCursorIx;
      for i := 1 to MidPoint do begin
        WorkParent := WorkCursor;
        WorkCursor := WorkCursor^.slnNext;
        inc(WorkCursorIx);
      end;
      {compare this node's data with the given item}
      CompareResult := aCompare(WorkCursor^.slnData, aItem);
      {if the node's data is less than the item, shrink the list, and
       try again from where we're at}
      if (CompareResult < 0) then begin
        dec(ListCount, MidPoint);
        BLCursor := WorkCursor;
        BLCursorIx := WorkCursorIx;
      end
      {if the node's data is greater than the item, shrink the list,
       and try again}
      else if (CompareResult > 0) then begin
        ListCount := MidPoint - 1;
      end
      {otherwise we found it; set the real cursor}
      else begin
        FCursor := WorkCursor;
        {NOTE: the 32-bit Delphis flag the following line with
         "Variable 'WorkParent' might not have been initialized."
         In reality it will have, since MidPoint >= 1 and hence the
         for loop above will have been executed at least once, setting
         WorkParent}
        FParent := WorkParent;
        FCursorIx := WorkCursorIx;
        Result := WorkCursorIx;
        Exit;
      end;
    end;
    {we didn't find it, but set the real cursor to where the item
     should be inserted}
    FCursor := BLCursor^.slnNext;
    FParent := BLCursor;
    FCursorIx := succ(BLCursorIx);
  end
  {otherwise do a sequential search}
  else begin
    {set the work cursor to the first node (if it exists)}
    WorkParent := FHead;
    WorkCursor := WorkParent^.slnNext;
    WorkCursorIx := 0;
    {walk the linked list looking for the item}
    while (WorkCursor <> nil) do begin
      if (aCompare(WorkCursor^.slnData, aItem) = 0) then begin
        {we found it; set the result; set the real cursor}
        Result := WorkCursorIx;
        FCursor := WorkCursor;
        FParent := WorkParent;
        FCursorIx := WorkCursorIx;
        Exit;
      end;
      {advance to the next node}
      WorkParent := WorkCursor;
      WorkCursor := WorkCursor^.slnNext;
      inc(WorkCursorIx);
    end;
  end;
  {didn't find it}
  Result := -1;
end;
{--------}
procedure TtdSingleLinkList.MoveBeforeFirst;
begin
  {set the cursor to the head node}
  FCursor := FHead;
  FParent := nil;
  FCursorIx := -1;
end;
{--------}
procedure TtdSingleLinkList.MoveNext;
begin
  {advance the cursor to its own next pointer, ignore attempts to move
   beyond the end of the list}
  if (FCursor <> nil) then begin
    FParent := FCursor;
    FCursor := FCursor^.slnNext;
    inc(FCursorIx);
  end;
end;
{--------}
procedure TtdSingleLinkList.Remove(aItem : pointer);
begin
  if (IndexOf(aItem) <> -1) then
    DeleteAtCursor;
end;
{--------}
procedure TtdSingleLinkList.sllError(aErrorCode  : integer;
                               const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdLinkListException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
function TtdSingleLinkList.sllGetItem(aIndex : longint) : pointer;
begin
  {position the cursor}
  sllPositionAtNth(aIndex);
  {return the data}
  Result := FCursor^.slnData;
end;
{--------}
class procedure TtdSingleLinkList.sllGetNodeManager;
begin
  {if the node manager hasn't been allocated yet, do so}
  if (SLNodeManager = nil) then
    SLNodeManager := TtdNodeManager.Create(sizeof(TslNode));
end;
{--------}
function TtdSingleLinkList.sllMerge(
                  aCompare    : TtdCompareFunc;
                  aPriorNode1 : PslNode; aCount1 : longint;
                  aPriorNode2 : PslNode; aCount2 : longint) : PslNode;
var
  i : integer;
  Node1    : PslNode;
  Node2    : PslNode;
  LastNode : PslNode;
  Temp     : PslNode;
begin
  {note: when this method is called the linked list looks like this:
            aPriorNode1 -> sublist1 -> sublist2 -> rest of list
         with the last node of sublist1 being aPriorNode2. After the
         merge, the list will look like this
            aPriorNode1 -> merged sublists -> rest of list
         and we'll return the last node in the merged sublist.}
  LastNode := aPriorNode1;
  {get the two top nodes}
  Node1 := aPriorNode1^.slnNext;
  Node2 := aPriorNode2^.slnNext;
  {repeat until one of the lists empties}
  while (aCount1 <> 0) and (aCount2 <> 0) do begin
    if (aCompare(Node1^.slnData, Node2^.slnData) <= 0) then begin
      LastNode := Node1;
      Node1 := Node1^.slnNext;
      dec(aCount1);
    end
    else begin
      Temp := Node2^.slnNext;
      Node2^.slnNext := Node1;
      LastNode^.slnNext := Node2;
      LastNode := Node2;
      Node2 := Temp;
      dec(aCount2);
    end;
  end;
  {if it was the first list that emptied, link the last node up to the
   remaining part of the second list, and walk it to get the very last
   node}
  if (aCount1 = 0) then begin
    LastNode^.slnNext := Node2;
    for i := 0 to pred(aCount2) do
      LastNode := LastNode^.slnNext;
  end
  {if it was the second list that emptied, Node2 is the first node of
   the remaining list; walk the remaining part of the first list and
   link it up to Node2}
  else begin
    for i := 0 to pred(aCount1) do
      LastNode := LastNode^.slnNext;
    LastNode^.slnNext := Node2;
  end;
  {return the last node}
  Result := LastNode;
end;
{--------}
function TtdSingleLinkList.sllMergesort(aCompare   : TtdCompareFunc;
                                        aPriorNode : PslNode;
                                        aCount     : longint)
                                                            : PslNode;
var
  Count2     : longint;
  PriorNode2 : PslNode;
  {$IFDEF Windows}
  DummyNode  : PslNode;
  {$ENDIF}
begin
  {easy case first: if there is only one item in the sublist, it must
   be sorted, so return}
  if (aCount = 1) then begin
    Result := aPriorNode^.slnNext;
    Exit;
  end;
  {split the list into two parts}
  Count2 := aCount div 2;
  aCount := aCount - Count2;
  {mergesort the first half: this'll return the head node for the
   second half}
  PriorNode2 := sllMergeSort(aCompare, aPriorNode, aCount);
  {mergesort the second half}
  {$IFDEF Windows}
  DummyNode :=
  {$ENDIF}
  sllMergeSort(aCompare, PriorNode2, Count2);
  {now merge the two halves, return the final node}
  Result := sllMerge(aCompare, aPriorNode, aCount, PriorNode2, Count2);
end;
{--------}
procedure TtdSingleLinkList.sllPositionAtNth(aIndex : longint);
var
  WorkCursor   : PslNode;
  WorkParent   : PslNode;
  WorkCursorIx : longint;
begin
  {check for a valid index}
  if (aIndex < 0) or (aIndex >= Count) then
    sllError(tdeListInvalidIndex, 'sllPositionAtNth');
  {take care of easy case}
  if (aIndex = FCursorIx) then
    Exit;
  {--now use local variables for speed--}
  {if the index wanted is before the cursor's index, move work cursor
   before all of the nodes}
  if (aIndex < FCursorIx) then begin
    WorkCursor := FHead;
    WorkParent := nil;
    WorkCursorIx := -1;
  end
  {otherwise set work cursor to current cursor}
  else begin
    WorkCursor := FCursor;
    WorkParent := FParent;
    WorkCursorIx := FCursorIx;
  end;
  {while the work cursor index is less than the index required,
   advance the work cursor}
  while (WorkCursorIx < aIndex) do begin
    WorkParent := WorkCursor;
    WorkCursor := WorkCursor^.slnNext;
    inc(WorkCursorIx);
  end;
  {set the real cursor equal to the work cursor}
  FCursor := WorkCursor;
  FParent := WorkParent;
  FCursorIx := WorkCursorIx;
end;
{--------}
procedure TtdSingleLinkList.sllSetItem(aIndex : longint;
                                       aItem  : pointer);
begin
  {position the cursor}
  sllPositionAtNth(aIndex);
  {if we can dispose of the data about to be replaced, do so}
  if Assigned(FDispose) and (aItem <> FCursor^.slnData) then
    FDispose(FCursor^.slnData);
  {replace the data}
  FCursor^.slnData := aItem;
  FIsSorted := false;
end;
{--------}
procedure TtdSingleLinkList.Sort(aCompare : TtdCompareFunc);
begin
  {perform a mergesort if there are more than one items in the list}
  if (Count > 1) then
    sllMergesort(aCompare, FHead, Count);
  MoveBeforeFirst;
  FIsSorted := true;
end;
{====================================================================}


{===TtdDoubleLinkList================================================}
constructor TtdDoubleLinkList.Create;
begin
  inherited Create;
  {save the dispose procedure}
  FDispose := aDispose;
  {get the node manager}
  dllGetNodeManager;
  {allocate a head and a tail node and link them together}
  FHead := PdlNode(DLNodeManager.AllocNode);
  FTail := PdlNode(DLNodeManager.AllocNode);
  FHead^.dlnNext := FTail;
  FHead^.dlnPrior := nil;
  FHead^.dlnData := nil;
  FTail^.dlnNext := nil;
  FTail^.dlnPrior := FHead;
  FTail^.dlnData := nil;
  {set the cursor to the head node}
  FCursor := FHead;
  FCursorIx := -1;
  FIsSorted := true;
end;
{--------}
destructor TtdDoubleLinkList.Destroy;
begin
  if (Count <> 0) then
    Clear;
  DLNodeManager.FreeNode(FHead);
  DLNodeManager.FreeNode(FTail);
  inherited Destroy;
end;
{--------}
function TtdDoubleLinkList.Add(aItem : pointer) : longint;
begin
  {move to the very end of the linked list}
  FCursor := FTail;
  FCursorIx := Count;
  {return the index of the new node}
  Result := Count;
  {insert at the cursor}
  InsertAtCursor(aItem);
  FIsSorted := false;
end;
{--------}
procedure TtdDoubleLinkList.Clear;
var
  Temp : PdlNode;
begin
  {delete all the nodes, except the head and tail nodes; if we can
   dispose of nodes, do so}
  Temp := FHead^.dlnNext;
  while (Temp <> FTail) do begin
    FHead^.dlnNext := Temp^.dlnNext;
    if Assigned(FDispose) then
      FDispose(Temp^.dlnData);
    DLNodeManager.FreeNode(Temp);
    Temp := FHead^.dlnNext;
  end;
  {patch up the linked list}
  FTail^.dlnPrior := FHead;
  FCount := 0;
  {set the cursor to the head of the list}
  FCursor := FHead;
  FCursorIx := -1;
  FIsSorted := true;
end;
{--------}
procedure TtdDoubleLinkList.Delete(aIndex : longint);
begin
  {position the cursor}
  dllPositionAtNth(aIndex);
  {delete the item at the cursor}
  DeleteAtCursor;
end;
{--------}
procedure TtdDoubleLinkList.DeleteAtCursor;
var
  Temp : PdlNode;
begin
  {let Temp equal the node we are to delete}
  Temp := FCursor;
  if (Temp = FHead) or (Temp = FTail) then
    dllError(tdeListCannotDelete, 'Delete');
  {dispose of its contents}
  if Assigned(FDispose) then
    FDispose(Temp^.dlnData);
  {unlink the node and free it; the cursor moves to the next node}
  Temp^.dlnPrior^.dlnNext := Temp^.dlnNext;
  Temp^.dlnNext^.dlnPrior := Temp^.dlnPrior;
  FCursor := Temp^.dlnNext;
  DLNodeManager.FreeNode(Temp);
  dec(FCount);
  if (Count <= 1) then
    FIsSorted := true;
end;
{--------}
procedure TtdDoubleLinkList.dllError(aErrorCode  : integer;
                               const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdLinkListException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
function TtdDoubleLinkList.dllGetItem(aIndex : longint) : pointer;
begin
  {position the cursor}
  dllPositionAtNth(aIndex);
  {return the data}
  Result := FCursor^.dlnData;
end;
{--------}
class procedure TtdDoubleLinkList.dllGetNodeManager;
begin
  {if the node manager hasn't been allocated yet, do so}
  if (DLNodeManager = nil) then
    DLNodeManager := TtdNodeManager.Create(sizeof(TdlNode));
end;
{--------}
function TtdDoubleLinkList.dllMerge(
                  aCompare    : TtdCompareFunc;
                  aPriorNode1 : PdlNode; aCount1 : longint;
                  aPriorNode2 : PdlNode; aCount2 : longint) : PdlNode;
var
  i : integer;
  Node1    : PdlNode;
  Node2    : PdlNode;
  LastNode : PdlNode;
  Temp     : PdlNode;
begin
  {note: when this method is called the linked list looks like this:
            aPriorNode1 -> sublist1 -> sublist2 -> rest of list
         with the last node of sublist1 being aPriorNode2. After the
         merge, the list will look like this
            aPriorNode1 -> merged sublists -> rest of list
         and we'll return the last node in the merged sublist.}
  LastNode := aPriorNode1;
  {get the two top nodes}
  Node1 := aPriorNode1^.dlnNext;
  Node2 := aPriorNode2^.dlnNext;
  {repeat until one of the lists empties}
  while (aCount1 <> 0) and (aCount2 <> 0) do begin
    if (aCompare(Node1^.dlnData, Node2^.dlnData) <= 0) then begin
      LastNode := Node1;
      Node1 := Node1^.dlnNext;
      dec(aCount1);
    end
    else begin
      Temp := Node2^.dlnNext;
      Node2^.dlnNext := Node1;
      LastNode^.dlnNext := Node2;
      LastNode := Node2;
      Node2 := Temp;
      dec(aCount2);
    end;
  end;
  {if it was the first list that emptied, link the last node up to the
   remaining part of the second list, and walk it to get the very last
   node}
  if (aCount1 = 0) then begin
    LastNode^.dlnNext := Node2;
    for i := 0 to pred(aCount2) do
      LastNode := LastNode^.dlnNext;
  end
  {if it was the second list that emptied, Node2 is the first node of
   the remaining list; walk the remaining part of the first list and
   link it up to Node2}
  else begin
    for i := 0 to pred(aCount1) do
      LastNode := LastNode^.dlnNext;
    LastNode^.dlnNext := Node2;
  end;
  {return the last node}
  Result := LastNode;
end;
{--------}
function TtdDoubleLinkList.dllMergesort(aCompare   : TtdCompareFunc;
                                        aPriorNode : PdlNode;
                                        aCount     : longint)
                                                            : PdlNode;
var
  Count2     : longint;
  PriorNode2 : PdlNode;
  {$IFDEF Windows}
  DummyNode  : PdlNode;
  {$ENDIF}
begin
  {easy case first: if there is only one item in the sublist, it must
   be sorted, so return}
  if (aCount = 1) then begin
    Result := aPriorNode^.dlnNext;
    Exit;
  end;
  {split the list into two parts}
  Count2 := aCount div 2;
  aCount := aCount - Count2;
  {mergesort the first half: this'll return the head node for the
   second half}
  PriorNode2 := dllMergeSort(aCompare, aPriorNode, aCount);
  {mergesort the second half}
  {$IFDEF Windows}
  DummyNode :=
  {$ENDIF}
  dllMergeSort(aCompare, PriorNode2, Count2);
  {now merge the two halves, return the final node}
  Result := dllMerge(aCompare, aPriorNode, aCount, PriorNode2, Count2);
end;
{--------}
procedure TtdDoubleLinkList.dllPositionAtNth(aIndex : longint);
var
  WorkCursor   : PdlNode;
  WorkCursorIx : longint;
begin
  {check for a valid index}
  if (aIndex < 0) or (aIndex >= Count) then
    dllError(tdeListInvalidIndex, 'dllPositionAtNth');
  {use local varaibles for speed}
  WorkCursor := FCursor;
  WorkCursorIx := FCursorIx;
  {take care of easy case}
  if (aIndex = WorkCursorIx) then
    Exit;
  {the desired index is either before the current cursor or after it;
   in either case the required index is either closer to the cursor or
   closer to the relevant end; work out the shortest route}
  if (aIndex < WorkCursorIx) then begin
    if ((aIndex - 0) < (WorkCursorIx - aIndex)) then begin
      {start at front and work forwards towards aIndex}
      WorkCursor := FHead;
      WorkCursorIx := -1;
    end;
  end
  else {aIndex > FCursorIx} begin
    if ((aIndex - WorkCursorIx) < (Count - aIndex)) then begin
      {start at end and work back towards aIndex}
      WorkCursor := FTail;
      WorkCursorIx := Count;
    end;
  end;
  {while the work cursor index is less than the index required,
   advance the work cursor}
  while (WorkCursorIx < aIndex) do begin
    WorkCursor := WorkCursor^.dlnNext;
    inc(WorkCursorIx);
  end;
  {while the work cursor index is greater than the index required,
   move the work cursor backwards}
  while (WorkCursorIx > aIndex) do begin
    WorkCursor := WorkCursor^.dlnPrior;
    dec(WorkCursorIx);
  end;
  {set the real cursor equal to the work cursor}
  FCursor := WorkCursor;
  FCursorIx := WorkCursorIx;
end;
{--------}
procedure TtdDoubleLinkList.dllSetItem(aIndex : longint;
                                       aItem  : pointer);
begin
  {position the cursor}
  dllPositionAtNth(aIndex);
  {if we can dispose of the data about to be replaced, do so}
  if Assigned(FDispose) and (aItem <> FCursor^.dlnData) then
    FDispose(FCursor^.dlnData);
  {replace the data}
  FCursor^.dlnData := aItem;
  FIsSorted := false;
end;
{--------}
function TtdDoubleLinkList.Examine : pointer;
begin
  if (FCursor = nil) or (FCursor = FHead) then
    dllError(tdeListCannotExamine, 'Examine');
  {return the data part of the cursor}
  Result := FCursor^.dlnData;
end;
{--------}
function TtdDoubleLinkList.First : pointer;
begin
  {position the cursor}
  dllPositionAtNth(0);
  {return the data}
  Result := FCursor^.dlnData;
end;
{--------}
function TtdDoubleLinkList.IndexOf(aItem : pointer) : longint;
var
  WorkCursor   : PdlNode;
  WorkCursorIx : longint;
begin
  {set the work cursor to the first node (if it exists)}
  WorkCursor := FHead^.dlnNext;
  WorkCursorIx := 0;
  {walk the linked list looking for the item}
  while (WorkCursor <> FTail) do begin
    if (WorkCursor^.dlnData = aItem) then begin
      {we found it; set the result; set the real cursor}
      Result := WorkCursorIx;
      FCursor := WorkCursor;
      FCursorIx := WorkCursorIx;
      Exit;
    end;
    {advance to the next node}
    WorkCursor := WorkCursor^.dlnNext;
    inc(WorkCursorIx);
  end;
  {didn't find it}
  Result := -1;
end;
{--------}
procedure TtdDoubleLinkList.Insert(aIndex : longint; aItem : pointer);
begin
  {position the cursor}
  dllPositionAtNth(aIndex);
  {insert the item at the cursor}
  InsertAtCursor(aItem);
  FIsSorted := false;
end;
{--------}
procedure TtdDoubleLinkList.InsertAtCursor(aItem : pointer);
var
  NewNode : PdlNode;
begin
  {if the cursor is at the head, rather than raise an exception, move
   it forwards one node}
  if (FCursor = FHead) then
    MoveNext;
  {allocate a new node and insert before the cursor}
  NewNode := PdlNode(DLNodeManager.AllocNode);
  NewNode^.dlnData := aItem;
  NewNode^.dlnNext := FCursor;
  NewNode^.dlnPrior := FCursor^.dlnPrior;
  NewNode^.dlnPrior^.dlnNext := NewNode;
  FCursor^.dlnPrior := NewNode;
  FCursor := NewNode;
  inc(FCount);
end;
{--------}
procedure TtdDoubleLinkList.InsertSorted(aItem : pointer;
                                         aCompare : TtdCompareFunc);
begin
  if not IsSorted then
    dllError(tdeListIsNotSorted, 'InsertSorted');
  if Locate(aItem, aCompare) = -1 then
    InsertAtCursor(aItem);
end;
{--------}
function TtdDoubleLinkList.IsAfterLast : boolean;
begin
  Result := FCursor = FTail;
end;
{--------}
function TtdDoubleLinkList.IsBeforeFirst : boolean;
begin
  Result := FCursor = FHead;
end;
{--------}
function TtdDoubleLinkList.IsEmpty : boolean;
begin
  Result := (Count = 0);
end;
{--------}
function TtdDoubleLinkList.Last : pointer;
begin
  {position the cursor}
  dllPositionAtNth(pred(Count));
  {return the data}
  Result := FCursor^.dlnData;
end;
{--------}
function TtdDoubleLinkList.Locate(aItem    : pointer;
                                  aCompare : TtdCompareFunc)
                                           : longint;
var
  BLCursor : PdlNode;
  BLCursorIx : longint;
  WorkCursor : PdlNode;
  WorkCursorIx : longint;
  ListCount    : longint;
  MidPoint     : longint;
  i            : integer;
  CompareResult: integer;
begin
  {there are two different ways of doing this, depending on whether
   the list is sorted or not; if sorted, we perform a binary search,
   if not, we perform a sequential search}
  {if sorted, do a binary search}
  if IsSorted then begin
    {prepare}
    BLCursor := FHead;
    BLCursorIx := -1;
    ListCount := Count;
    {while there are still nodes to check...}
    while (ListCount <> 0) do begin
      {calculate the midpoint; it will be at least 1}
      MidPoint := (ListCount + 1) div 2;
      {move that many nodes along}
      WorkCursor := BLCursor;
      WorkCursorIx := BLCursorIx;
      for i := 1 to MidPoint do begin
        WorkCursor := WorkCursor^.dlnNext;
        inc(WorkCursorIx);
      end;
      {compare this node's data with the given item}
      CompareResult := aCompare(WorkCursor^.dlnData, aItem);
      {if the node's data is less than the item, shrink the list, and
       try again from where we're at}
      if (CompareResult < 0) then begin
        dec(ListCount, MidPoint);
        BLCursor := WorkCursor;
        BLCursorIx := WorkCursorIx;
      end
      {if the node's data is greater than the item, shrink the list,
       and try again}
      else if (CompareResult > 0) then begin
        ListCount := MidPoint - 1;
      end
      {otherwise we found it; set the real cursor}
      else begin
        FCursor := WorkCursor;
        FCursorIx := WorkCursorIx;
        Result := WorkCursorIx;
        Exit;
      end;
    end;
    {we didn't find it, but set the real cursor to where the item
     should be inserted}
    FCursor := BLCursor^.dlnNext;
    FCursorIx := succ(BLCursorIx);
  end
  {otherwise do a sequential search}
  else begin
    {set the work cursor to the first node (if it exists)}
    WorkCursor := FHead^.dlnNext;
    WorkCursorIx := 0;
    {walk the linked list looking for the item}
    while (WorkCursor <> nil) do begin
      if (aCompare(WorkCursor^.dlnData, aItem) = 0) then begin
        {we found it; set the result; set the real cursor}
        Result := WorkCursorIx;
        FCursor := WorkCursor;
        FCursorIx := WorkCursorIx;
        Exit;
      end;
      {advance to the next node}
      WorkCursor := WorkCursor^.dlnNext;
      inc(WorkCursorIx);
    end;
  end;
  {didn't find it}
  Result := -1;
end;
{--------}
procedure TtdDoubleLinkList.MoveAfterLast;
begin
  {set the cursor to the tail node}
  FCursor := FTail;
  FCursorIx := Count;
end;
{--------}
procedure TtdDoubleLinkList.MoveBeforeFirst;
begin
  {set the cursor to the head node}
  FCursor := FHead;
  FCursorIx := -1;
end;
{--------}
procedure TtdDoubleLinkList.MoveNext;
begin
  {advance the cursor to its own next pointer}
  if (FCursor <> FTail) then begin
    FCursor := FCursor^.dlnNext;
    inc(FCursorIx);
  end;
end;
{--------}
procedure TtdDoubleLinkList.MovePrior;
begin
  {move the cursor back to its own previous pointer}
  if (FCursor <> FHead) then begin
    FCursor := FCursor^.dlnPrior;
    dec(FCursorIx);
  end;
end;
{--------}
procedure TtdDoubleLinkList.Remove(aItem : pointer);
begin
  if (IndexOf(aItem) <> -1) then
    DeleteAtCursor;
end;
{--------}
procedure TtdDoubleLinkList.InsertionSort(aCompare : TtdCompareFunc);
var
  Walker : PdlNode;
  Temp   : PdlNode;
  WalkerParent : PdlNode;
  TempParent   : PdlNode;
begin
  {if there are zero (or one) items the list is already sorted}
  if (Count <= 1) then begin
    FIsSorted := true;
    Exit;
  end;
  {perform an insertion sort from the second item onwards}
  WalkerParent := FHead^.dlnNext;
  Walker := WalkerParent^.dlnNext;
  while (Walker <> FTail) do begin
    {find where the walker item should be in the sorted list to its
     left - we walk the sorted sublist making a note of the parent as
     we go so that we can insert properly. Note that the loop below
     will terminate in the worst case by the walker node itself - we
     won't run off the end of the list}
    TempParent := FHead;
    Temp := TempParent^.dlnNext;
    while (aCompare(Temp^.dlnData, Walker^.dlnData) < 0) do begin
      TempParent := Temp;
      Temp := TempParent^.dlnNext;
    end;
    {did we find the walker node? If so, move the walker's parent on
     by one link}
    if (Temp = Walker) then begin
      WalkerParent := Walker;
    end
    {otherwise, move the walker node into the correct place in the
     sorted sublist}
    else begin
      {disconnect the walker node}
      WalkerParent^.dlnNext := Walker^.dlnNext;
      {connect the walker node in the correct place}
      Walker^.dlnNext := Temp;
      TempParent^.dlnNext := Walker;
    end;
    {set the walker node}
    Walker := WalkerParent^.dlnNext;
  end;
  {now patch up all of the previous links}
  WalkerParent := FHead;
  Walker := WalkerParent^.dlnNext;
  while (WalkerParent <> FTail) do begin
    Walker^.dlnPrior := WalkerParent;
    WalkerParent := Walker;
    Walker := WalkerParent^.dlnNext;
  end;
  MoveBeforeFirst;
  FIsSorted := true;
end;
{--------}
procedure TtdDoubleLinkList.Sort(aCompare : TtdCompareFunc);
var
  Dad, Walker : PdlNode;
begin
  {perform a singly linked mergesort if there are more than one item
   in the list; then patch up the prior links}
  if (Count > 1) then begin
    dllMergesort(aCompare, FHead, Count);
    Dad := FHead;
    Walker := FHead^.dlnNext;
    while (Walker <> nil) do begin
      Walker^.dlnPrior := Dad;
      Dad := Walker;
      Walker := Dad^.dlnNext;
    end;
  end;
  MoveBeforeFirst;
  FIsSorted := true;
end;
{====================================================================}


{===Interfaced routines==============================================}
function TDSLLSearch(aList : TtdSingleLinkList;
                     aItem : pointer;
                     aCompare : TtdCompareFunc) : boolean;
begin
  with aList do begin
    MoveBeforeFirst;
    MoveNext;
    while not IsAfterLast do begin
      if (aCompare(Examine, aItem) = 0) then begin
        Result := true;
        Exit;
      end;
      MoveNext;
    end;
  end;
  Result := false;
end;
{--------}
function TDSLLSortedSearch(aList : TtdSingleLinkList;
                           aItem : pointer;
                           aCompare : TtdCompareFunc) : boolean;
var
  Compare : integer;
begin
  with aList do begin
    MoveBeforeFirst;
    MoveNext;
    while not IsAfterLast do begin
      Compare := aCompare(Examine, aItem);
      if (Compare >= 0) then begin
        Result := (Compare = 0);
        Exit;
      end;
      MoveNext;
    end;
  end;
  Result := false;
end;
{--------}
function TDDLLSearch(aList : TtdSingleLinkList;
                     aItem : pointer;
                     aCompare : TtdCompareFunc) : boolean;
begin
  with aList do begin
    MoveBeforeFirst;
    MoveNext;
    while not IsAfterLast do begin
      if (aCompare(Examine, aItem) = 0) then begin
        Result := true;
        Exit;
      end;
      MoveNext;
    end;
  end;
  Result := false;
end;
{--------}
function TDDLLSortedSearch(aList : TtdSingleLinkList;
                           aItem : pointer;
                           aCompare : TtdCompareFunc) : boolean;
var
  Compare : integer;
begin
  with aList do begin
    MoveBeforeFirst;
    MoveNext;
    while not IsAfterLast do begin
      Compare := aCompare(Examine, aItem);
      if (Compare >= 0) then begin
        Result := (Compare = 0);
        Exit;
      end;
      MoveNext;
    end;
  end;
  Result := false;
end;
{====================================================================}


procedure FinalizeUnit; far;
begin
  SLNodeManager.Free;
  DLNodeManager.Free;
end;

initialization
  SLNodeManager := nil;
  DLNodeManager := nil;
  {$IFDEF Delphi1}
  AddExitProc(FinalizeUnit);
  {$ENDIF}

{$IFNDEF Delphi1}
finalization
  FinalizeUnit;
{$ENDIF}

end.
