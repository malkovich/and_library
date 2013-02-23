(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDPriQue                                                         *)
(* Priority queues                                                  *)
(********************************************************************)

unit TDPriQue;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  Classes,
  TDBasics,
  TDNdeMgr;

type
  TtdSimplePriQueue1 = class
    {-A priority queue that is fast at insertion, slow at retrieval}
    private
      FCompare : TtdCompareFunc;
      FList    : TList;
    protected
      function pqGetCount : integer;
    public
      constructor Create(aCompare : TtdCompareFunc);
      destructor Destroy; override;

      function Dequeue : pointer;
      procedure Enqueue(aItem : pointer);

      property Count : integer read pqGetCount;
  end;

type
  TtdSimplePriQueue2 = class
    {-A priority queue that is slow at insertion, fast at retrieval}
    private
      FCompare : TtdCompareFunc;
      FList    : TList;
    protected
      function pqGetCount : integer;
    public
      constructor Create(aCompare : TtdCompareFunc);
      destructor Destroy; override;

      function Dequeue : pointer;
      procedure Enqueue(aItem : pointer);

      property Count : integer read pqGetCount;
  end;

type
  TtdPriorityQueue = class
    {-A priority queue that uses the heap algorithm}
    private
      FCompare : TtdCompareFunc;
      FDispose : TtdDisposeProc;
      FList    : TList;
      FName    : TtdNameString;
    protected
      function pqGetCount : integer;

      procedure pqError(aErrorCode  : integer;
                  const aMethodName : TtdNameString);
      procedure pqBubbleUp(aFromInx : integer);
      procedure pqTrickleDown;
      procedure pqTrickleDownStd;
    public
      constructor Create(aCompare : TtdCompareFunc;
                         aDispose : TtdDisposeProc);
      destructor Destroy; override;

      procedure Clear;
      function Dequeue : pointer;
      procedure Enqueue(aItem : pointer);
      function Examine : pointer;
      function IsEmpty : boolean;

      property Count : integer read pqGetCount;

      property Name : TtdNameString
         read FName write FName;

      property List : TList read FList;
  end;

type
  TtdPQHandle = pointer;

  TtdPriorityQueueEx = class
    {-A priority queue that uses the heap algorithm and that allows
      deletion and reprioritisation of arbitrary items}
    private
      FCompare : TtdCompareFunc;
      FHandles : pointer;
      FList    : TList;
      FName    : TtdNameString;
    protected
      function pqGetCount : integer;

      procedure pqError(aErrorCode  : integer;
                  const aMethodName : TtdNameString);
      procedure pqBubbleUp(aHandle : TtdPQHandle);
      procedure pqTrickleDown(aHandle : TtdPQHandle);

      {$IFDEF DebugMode}
      procedure pqVerifyIndirection;
      {$ENDIF}
    public
      constructor Create(aCompare : TtdCompareFunc);
      destructor Destroy; override;

      procedure ChangePriority(aHandle : TtdPQHandle);
      procedure Clear;
      function Dequeue : pointer;
      function Enqueue(aItem : pointer) : TtdPQHandle;
      function Examine : pointer;
      function IsEmpty : boolean;
      function Remove(aHandle : TtdPQHandle) : pointer;

      property Count : integer read pqGetCount;
      property Name : TtdNameString
         read FName write FName;

      property List : TList read FList;
  end;

implementation

const
  UnitName = 'TDPriQue';

{===TtdSimplePriQueue1===============================================}
constructor TtdSimplePriQueue1.Create(aCompare : TtdCompareFunc);
begin
  inherited Create;
  FCompare := aCompare;
  FList := TList.Create;
end;
{--------}
destructor TtdSimplePriQueue1.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;
{--------}
function TtdSimplePriQueue1.Dequeue : pointer;
var
  Inx     : integer;
  PQCount : integer;
  MaxInx  : integer;
  MaxItem : pointer;
begin
  PQCount := Count;
  if (PQCount = 0) then
    Result := nil
  else if (PQCount = 1) then begin
    Result := FList.List^[0];
    FList.Clear;
  end
  else begin
    MaxItem := FList.List^[0];
    MaxInx := 0;
    for Inx := 1 to pred(PQCount) do
      if (FCompare(FList.List^[Inx], MaxItem) > 0) then begin
        MaxItem := FList.List^[Inx];
        MaxInx := Inx;
      end;
    Result := MaxItem;
    FList.List^[MaxInx] := FList.Last;
    FList.Count := FList.Count - 1;
  end;
end;
{--------}
procedure TtdSimplePriQueue1.Enqueue(aItem : pointer);
begin
  FList.Add(aItem);
end;
{--------}
function TtdSimplePriQueue1.pqGetCount : integer;
begin
  Result := FList.Count;
end;
{====================================================================}


{===TtdSimplePriQueue2===============================================}
constructor TtdSimplePriQueue2.Create(aCompare : TtdCompareFunc);
begin
  inherited Create;
  FCompare := aCompare;
  FList := TList.Create;
end;
{--------}
destructor TtdSimplePriQueue2.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;
{--------}
function TtdSimplePriQueue2.Dequeue : pointer;
begin
  Result := FList.Last;
  FList.Count := FList.Count - 1;
end;
{--------}
procedure TtdSimplePriQueue2.Enqueue(aItem : pointer);
var
  Inx : integer;
begin
  {increment the number of items in the list}
  FList.Count := FList.Count + 1;
  {find where to put our new item}
  Inx := FList.Count - 2;
  while (Inx >= 0) and
        (FCompare(FList.List^[Inx], aItem) > 0) do begin
    FList.List^[Inx+1] := FList.List^[Inx];
    dec(Inx);
  end;
  {put it there}
  FList.List^[Inx+1] := aItem;
end;
{--------}
function TtdSimplePriQueue2.pqGetCount : integer;
begin
  Result := FList.Count;
end;
{====================================================================}


{===TtdPriorityQueue=================================================}
constructor TtdPriorityQueue.Create(aCompare : TtdCompareFunc;
                                    aDispose : TtdDisposeProc);
begin
  inherited Create;
  if not Assigned(aCompare) then
    pqError(tdePriQueueNoCompare, 'Create');
  FCompare := aCompare;
  FDispose := aDispose;
  FList := TList.Create;
end;
{--------}
destructor TtdPriorityQueue.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;
{--------}
procedure TtdPriorityQueue.Clear;
var
  Inx : integer;
begin
  if Assigned(FDispose) then
    for Inx := 0 to pred(FList.Count) do
      FDispose(FList.List^[Inx]);
  FList.Count := 0;
end;
{--------}
function TtdPriorityQueue.Dequeue : pointer;
begin
  {make sure we have an item to dequeue}
  if (FList.Count = 0) then
    pqError(tdeQueueIsEmpty, 'Dequeue');
  {return the item at the root}
  Result := FList.List^[0];
  {if there was only one item in the queue, it's now empty}
  if (FList.Count = 1) then
    FList.Count := 0
  {if there were two, just replace the root with the one remaining
   child; the heap property is obviously satisfied}
  else if (FList.Count = 2) then begin
    FList.List^[0] := FList.List^[1];
    FList.Count := 1;
  end
  {otherwise we have to restore the heap property}
  else begin
    {replace the root with the child at the lowest, rightmost
     position, shrink the list, and finally trickle down the root item
     as far as it will go}
    FList.List^[0] := FList.Last;
    FList.Count := FList.Count - 1;
    pqTrickleDown;
  end;
end;
{--------}
procedure TtdPriorityQueue.Enqueue(aItem : pointer);
begin
  {add the item to the end of the list and bubble it up as far as it
   will go}
  FList.Add(aItem);
  pqBubbleUp(pred(FList.Count));
end;
{--------}
function TtdPriorityQueue.Examine : pointer;
begin
  if (FList.Count = 0) then
    pqError(tdeQueueIsEmpty, 'Examine');
  Result := FList.List^[0];
end;
{--------}
function TtdPriorityQueue.IsEmpty : boolean;
begin
  Result := FList.Count = 0;
end;
{--------}
procedure TtdPriorityQueue.pqBubbleUp(aFromInx : integer);
var
  ParentInx : integer;
  Item      : pointer;
begin
  Item := FList.List^[aFromInx];
  {while the item under consideration is larger than its parent, swap
   it with its parent and continue from its new position}
  {Note: the parent for the child at index N is at N-1 div 2}
  ParentInx := (aFromInx - 1) div 2;
  {while our item has a parent, and it's greater than the parent...}
  while (aFromInx > 0) and
        (FCompare(Item, FList.List^[ParentInx]) > 0) do begin
    {move our parent down the tree}
    FList.List^[aFromInx] := FList.List^[ParentInx];
    aFromInx := ParentInx;
    ParentInx := (aFromInx - 1) div 2;
  end;
  {store our item in the correct place}
  FList.List^[aFromInx] := Item;
end;
{--------}
procedure TtdPriorityQueue.pqError(aErrorCode  : integer;
                             const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdQueueException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
function TtdPriorityQueue.pqGetCount : integer;
begin
  Result := FList.Count;
end;
{--------}
procedure TtdPriorityQueue.pqTrickleDown;
var
  FromInx  : integer;
  ChildInx : integer;
  MaxInx   : integer;
  Item     : pointer;
begin
  FromInx := 0;
  Item := FList.List^[0];
  MaxInx := pred(FList.Count);
  {swap the item under consideration with its larger child until it
   has no children}
  {Note: the children for the parent at index N are at 2N+1 and 2N+2}
  ChildInx := (FromInx * 2) + 1;
  {while there is at least a left child...}
  while (ChildInx <= MaxInx) do begin
    {if there is a right child as well, calculate the index of the
     larger child}
    if (succ(ChildInx) <= MaxInx) and
       (FCompare(FList.List^[ChildInx],
                 FList.List^[succ(ChildInx)]) < 0) then
      inc(ChildInx);
    {move the larger child up the tree, and move our item
     down the tree and repeat}
    FList.List^[FromInx] := FList.List^[ChildInx];
    FromInx := ChildInx;
    ChildInx := (FromInx * 2) + 1;
  end;
  {store our item where we end up}
  FList.List^[FromInx] := Item;
  {now bubble this item up the tree}
  pqBubbleUp(FromInx);
end;
{--------}
procedure TtdPriorityQueue.pqTrickleDownStd;
var
  FromInx  : integer;
  ChildInx : integer;
  MaxInx   : integer;
  Item     : pointer;
begin
  FromInx := 0;
  Item := FList.List^[0];
  MaxInx := FList.Count - 1;
  {while the item under consideration is smaller than one of its
   children, swap it with the larger child and continue from its new
   position}
  {Note: the children for the parent at index N are at 2N+1 and 2N+2}
  ChildInx := (FromInx * 2) + 1;
  {while there is at least a left child...}
  while (ChildInx <= MaxInx) do begin
    {if there is a right child as well, calculate the index of the
     larger child}
    if (succ(ChildInx) <= MaxInx) and
       (FCompare(FList.List^[ChildInx],
                 FList.List^[succ(ChildInx)]) < 0) then
      inc(ChildInx);
    {if our item is greater or equal to the larger child, we're done}
    if (FCompare(Item, FList.List^[ChildInx]) >= 0) then
      Break;
    {otherwise move the larger child up the tree, and move our item
     down the tree and repeat}
    FList.List^[FromInx] := FList.List^[ChildInx];
    FromInx := ChildInx;
    ChildInx := (FromInx * 2) + 1;
  end;
  {store our item in the correct place}
  FList.List^[FromInx] := Item;
end;
{====================================================================}


{===Linked list routines for extended priority queue=================}
type
  PpqexNode = ^TpqexNode;
  TpqexNode = packed record
    peNext : PpqexNode;
    pePrev : PpqexNode;
    peItem : pointer;
    peInx  : integer;
  end;
var
  LLNodeManager : TtdNodeManager;
{--------}
function CreateLinkedList : PpqexNode;
begin
  Result := LLNodeManager.AllocNode;
  Result^.peNext := LLNodeManager.AllocNode;
  Result^.peNext^.pePrev := Result;
  Result^.peNext^.peNext := nil;
end;
{--------}
procedure ClearLinkedList(aLinkedList : PpqexNode);
var
  Walker, Temp : PpqexNode;
begin
  Walker := aLinkedList^.peNext;
  while (Walker^.peNext <> nil) do begin
    Temp := Walker;
    Walker := Walker^.peNext;
    LLNodeManager.FreeNode(Temp);
  end;
end;
{--------}
procedure DestroyLinkedList(aLinkedList : PpqexNode);
var
  Temp : PpqexNode;
begin
  while (aLinkedList <> nil) do begin
    Temp := aLinkedList;
    aLinkedList := aLinkedList^.peNext;
    LLNodeManager.FreeNode(Temp);
  end;
end;
{--------}
function AddLinkedListNode(aLinkedList : PpqexNode; aItem : pointer) : PpqexNode;
begin
  Result := LLNodeManager.AllocNode;
  Result^.peNext := aLinkedList^.peNext;
  Result^.pePrev := aLinkedList;
  aLinkedList^.peNext^.pePrev := Result;
  aLinkedList^.peNext := Result;
  Result^.peItem := aItem;
end;
{--------}
procedure DeleteLinkedListNode(aLinkedList : PpqexNode; aNode : PpqexNode);
begin
  aNode^.pePrev^.peNext := aNode^.peNext;
  aNode^.peNext^.pePrev := aNode^.pePrev;
  LLNodeManager.FreeNode(aNode);
end;
{====================================================================}


{===TtdPriorityQueueEx===============================================}
constructor TtdPriorityQueueEx.Create(aCompare : TtdCompareFunc);
begin
  inherited Create;
  if (LLNodeManager = nil) then
    LLNodeManager := TtdNodeManager.Create(sizeof(TpqexNode));
  if not Assigned(aCompare) then
    pqError(tdePriQueueNoCompare, 'Create');
  FCompare := aCompare;
  FList := TList.Create;
  FHandles := CreateLinkedList;
end;
{--------}
destructor TtdPriorityQueueEx.Destroy;
begin
  Clear;
  FList.Free;
  DestroyLinkedList(FHandles);
  inherited Destroy;
end;
{--------}
procedure TtdPriorityQueueEx.ChangePriority(aHandle : TtdPQHandle);
var
  Handle : PpqexNode absolute aHandle;
  ParentInx    : integer;
  ParentHandle : PpqexNode;
begin
  {check to see whether we can bubble up}
  if (Handle^.peInx > 0) then begin
    ParentInx := (Handle^.peInx - 1) div 2;
    ParentHandle := PpqexNode(FList[ParentInx]);
    if (FCompare(Handle^.peItem, ParentHandle^.peItem) > 0) then begin
      pqBubbleUp(Handle);
      {$IFDEF DebugMode}
      pqVerifyIndirection;
      {$ENDIF}
      Exit;
    end;
  end;
  {otherwise trickle down}
  pqTrickleDown(Handle);
  {$IFDEF DebugMode}
  pqVerifyIndirection;
  {$ENDIF}
end;
{--------}
procedure TtdPriorityQueueEx.Clear;
begin
  ClearLinkedList(FHandles);
  FList.Clear;
end;
{--------}
function TtdPriorityQueueEx.Dequeue : pointer;
var
  Handle : PpqexNode;
begin
  {make sure we have an item to dequeue}
  if (FList.Count = 0) then
    pqError(tdeQueueIsEmpty, 'Dequeue');
  {return the item at the root, remove it from the handles list}
  Handle := FList.List^[0];
  Result := Handle^.peItem;
  DeleteLinkedListNode(FHandles, Handle);
  {if there was only one item in the queue, it's now empty}
  if (FList.Count = 1) then
    FList.Count := 0
  {if there were two, just replace the root with the one remaining
   child; the heap property is obviously satisfied}
  else if (FList.Count = 2) then begin
    Handle := FList.List^[1];
    FList.List^[0] := Handle;
    FList.Count := 1;
    Handle^.peInx := 0;
  end
  {otherwise we have to restore the heap property}
  else begin
    {replace the root with the child at the lowest, rightmost
     position, and shrink the list; then trickle down the root item as
     far as it will go}
    Handle := FList.Last;
    FList.List^[0] := Handle;
    Handle^.peInx := 0;
    FList.Count := FList.Count - 1;
    pqTrickleDown(Handle);
    {$IFDEF DebugMode}
    pqVerifyIndirection;
    {$ENDIF}
  end;
end;
{--------}
function TtdPriorityQueueEx.Enqueue(aItem : pointer) : TtdPQHandle;
var
  Handle : PpqexNode;
begin
  {create a new node for the linked list}
  Handle := AddLinkedListNode(FHandles, aItem);
  {add the handle to the end of the queue}
  FList.Add(Handle);
  Handle^.peInx := pred(FList.Count);
  {now bubble it up as far as it will go}
  if (FList.Count > 1) then
    pqBubbleUp(Handle);
  {return the handle}
  Result := Handle;
  {$IFDEF DebugMode}
  pqVerifyIndirection;
  {$ENDIF}
end;
{--------}
function TtdPriorityQueueEx.Examine : pointer;
begin
  if (FList.Count = 0) then
    pqError(tdeQueueIsEmpty, 'Examine');
  Result := PpqexNode(FList.List^[0])^.peItem;
end;
{--------}
function TtdPriorityQueueEx.IsEmpty : boolean;
begin
  Result := FList.Count = 0;
end;
{--------}
procedure TtdPriorityQueueEx.pqBubbleUp(aHandle : pointer);
var
  FromInx      : integer; 
  ParentInx    : integer;
  ParentHandle : PpqexNode;
  Handle       : PpqexNode absolute aHandle;
begin
  {while the handle under consideration is larger than its parent,
   swap it with its parent and continue from its new position}
  {Note: the parent for the child at index N is at (N-1) div 2}
  FromInx := Handle^.peInx;
  if (FromInx > 0) then begin
    ParentInx := (FromInx - 1) div 2;
    ParentHandle := PpqexNode(FList.List^[ParentInx]);
    {while our item has a parent, and it's greater than the parent...}
    while (FromInx > 0) and
          (FCompare(Handle^.peItem, ParentHandle^.peItem) > 0) do begin
      {move our parent down the tree}
      FList.List^[FromInx] := ParentHandle;
      ParentHandle^.peInx := FromInx;
      FromInx := ParentInx;
      ParentInx := (FromInx - 1) div 2;
      ParentHandle := PpqexNode(FList.List^[ParentInx]);
    end;
  end;
  {store our item in the correct place}
  FList.List^[FromInx] := Handle;
  Handle^.peInx := FromInx;
end;
{--------}
procedure TtdPriorityQueueEx.pqError(aErrorCode  : integer;
                               const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdQueueException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
function TtdPriorityQueueEx.pqGetCount : integer;
begin
  Result := FList.Count;
end;
{--------}
procedure TtdPriorityQueueEx.pqTrickleDown(aHandle : TtdPQHandle);
var
  FromInx     : integer;                                          
  MaxInx      : integer;
  ChildInx    : integer;
  ChildHandle : PpqexNode;
  Handle      : PpqexNode absolute aHandle;
begin
  {while the item under consideration is smaller than one of its
   children, swap it with the larger child and continue from its new
   position}
  {Note: the children for the parent at index N are at (2N+1) and
         2N+2}
  FromInx := Handle^.peInx;      
  MaxInx := pred(FList.Count);
  {calculate the left child index}
  ChildInx := succ(FromInx * 2);
  {while there is at least a left child...}
  while (ChildInx <= MaxInx) do begin
    {if there is a right child, calculate the index of the larger
     child}
    if ((ChildInx+1) <= MaxInx) and
       (FCompare(PpqexNode(FList.List^[ChildInx])^.peItem,
                 PpqexNode(FList.List^[ChildInx+1])^.peItem) < 0) then
      inc(ChildInx);
    {if our item is greater or equal to the larger child, we're done}
    ChildHandle := PpqexNode(FList.List^[ChildInx]);
    if (FCompare(Handle^.peItem, ChildHandle^.peItem) >= 0) then
      Break;
    {otherwise move the larger child up the tree, and move our item
     down the tree and repeat}
    FList.List^[FromInx] := ChildHandle;
    ChildHandle^.peInx := FromInx;
    FromInx := ChildInx;
    ChildInx := succ(FromInx * 2);
  end;
  {store our item in the correct place}
  FList.List^[FromInx] := Handle;
  Handle^.peInx := FromInx;
end;
{--------}
{$IFDEF DebugMode}
procedure TtdPriorityQueueEx.pqVerifyIndirection;
var
  i : integer;
  Handle : PpqexNode;
begin
  for i := 0 to pred(FList.Count) do begin
    Handle := PpqexNode(FList.List^[i]);
    if (Handle^.peInx <> i) then begin
      writeln('ERROR: Handle at ', i, ' doesn''t point to it');
      readln;
    end;
  end;
end;
{$ENDIF}
{--------}
function TtdPriorityQueueEx.Remove(aHandle : TtdPQHandle) : pointer;
var
  Handle    : PpqexNode absolute aHandle;
  NewHandle : PpqexNode;
  HeapInx   : integer;
begin
  {return the item, then delete the handle}
  Result := Handle^.peItem;
  HeapInx := Handle^.peInx;
  DeleteLinkedListNode(FHandles, Handle);
  {check to see whether we deleted the last item, if so just shrink
   the heap - the heap property will still apply}
  if (HeapInx = pred(FList.Count)) then
    FList.Count := FList.Count - 1
  else begin
    {replace the heap element with the child at the lowest, rightmost
     position, and shrink the list}
    NewHandle := FList.Last;
    FList.List^[HeapInx] := NewHandle;
    NewHandle^.peInx := HeapInx;
    FList.Count := FList.Count - 1;
    {now treat it as a change priority operation}
    ChangePriority(NewHandle);
  end;
  {$IFDEF DebugMode}
  pqVerifyIndirection;
  {$ENDIF}
end;
{====================================================================}

procedure FinalizeUnit; far;
begin
  LLNodeManager.Free;
end;

initialization
  LLNodeManager := nil;
  {$IFDEF Delphi1}
  AddExitProc(FinalizeUnit);
  {$ENDIF}

{$IFNDEF Delphi1}
finalization
  FinalizeUnit;
{$ENDIF}

end.
