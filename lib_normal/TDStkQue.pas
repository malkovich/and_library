(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDStkQue                                                         *)
(* Stacks and queues                                                *)
(********************************************************************)

unit TDStkQue;

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
  Classes,
  TDBasics,
  TDLnkLst;

type
  TtdStack = class
    private
      FCount   : longint;
      FDispose : TtdDisposeProc;
      FHead    : PslNode;
      FName    : TtdNameString;
    protected
      procedure sError(aErrorCode  : integer;
                 const aMethodName : TtdNameString);
      class procedure sGetNodeManager;
    public
      constructor Create(aDispose : TtdDisposeProc);
      destructor Destroy; override;

      procedure Clear;
      function Examine : pointer;
      function IsEmpty : boolean;
      function Pop : pointer;
      procedure Push(aItem : pointer);

      property Count : longint
         read FCount;
      property Name : TtdNameString
         read FName write FName;
  end;

  TtdArrayStack = class
    private
      FCount   : longint;
      FDispose : TtdDisposeProc;
      FList    : TList;
      FName    : TtdNameString;
    protected
      procedure asError(aErrorCode  : integer;
                  const aMethodName : TtdNameString);
      procedure asGrow;
    public
      constructor Create(aDispose  : TtdDisposeProc;
                         aCapacity : integer);
      destructor Destroy; override;

      procedure Clear;
      function Examine : pointer;
      function IsEmpty : boolean;
      function Pop : pointer;
      procedure Push(aItem : pointer);

      property Count : longint
         read FCount;
      property Name : TtdNameString
         read FName write FName;
  end;

type
  TtdQueue = class
    private
      FCount   : longint;
      FDispose : TtdDisposeProc;
      FHead    : PslNode;
      FName    : TtdNameString;
      FTail    : PslNode;
    protected
      procedure qError(aErrorCode  : integer;
                 const aMethodName : TtdNameString);
      class procedure qGetNodeManager;
    public
      constructor Create(aDispose : TtdDisposeProc);
      destructor Destroy; override;

      procedure Clear;
      function Dequeue : pointer;
      procedure Enqueue(aItem : pointer);
      function Examine : pointer;
      function IsEmpty : boolean;

      property Count : longint
         read FCount;
      property Name : TtdNameString
         read FName write FName;
  end;

  TtdArrayQueue = class
    private
      FCount   : integer;
      FDispose : TtdDisposeProc;
      FHead    : integer;
      FList    : TList;
      FName    : TtdNameString;
      FTail    : integer;
    protected
      procedure aqError(aErrorCode  : integer;
                 const aMethodName : TtdNameString);
      procedure aqGrow;
    public
      constructor Create(aDispose  : TtdDisposeProc;
                         aCapacity : integer);
      destructor Destroy; override;

      procedure Clear;
      function Dequeue : pointer;
      procedure Enqueue(aItem : pointer);
      function Examine : pointer;
      function IsEmpty : boolean;

      property Count : integer
         read FCount;
      property Name : TtdNameString
         read FName write FName;
  end;

implementation

uses
  TDNdeMgr;

const
  UnitName = 'TDStkQue';

var
  SLNodeManager : TtdNodeManager; {nodemanager for stacks and queues}


{===TtdStack=========================================================}
constructor TtdStack.Create(aDispose : TtdDisposeProc);
begin
  inherited Create;
  {save the dispose procedure}
  FDispose := aDispose;
  {get the node manager}
  sGetNodeManager;
  {allocate a head node}
  FHead := PslNode(SLNodeManager.AllocNode);
  FHead^.slnNext := nil;
  FHead^.slnData := nil;
end;
{--------}
destructor TtdStack.Destroy;
begin
  {remove all the remaining nodes; free the head node}
  if (Count <> 0) then
    Clear;
  SLNodeManager.FreeNode(FHead);
  inherited Destroy;
end;
{--------}
procedure TtdStack.Clear;
var
  Temp : PslNode;
begin
  {delete all the nodes, except the head node; if we can dispose of
   the nodes' data, do so}
  Temp := FHead^.slnNext;
  while (Temp <> nil) do begin
    FHead^.slnNext := Temp^.slnNext;
    if Assigned(FDispose) then
      FDispose(Temp^.slnData);
    SLNodeManager.FreeNode(Temp);
    Temp := FHead^.slnNext;
  end;
  FCount := 0;
end;
{--------}
function TtdStack.Examine : pointer;
begin
  if (Count = 0) then
    sError(tdeStackIsEmpty, 'Examine');
  Result := FHead^.slnNext^.slnData;
end;
{--------}
function TtdStack.IsEmpty : boolean;
begin
  Result := (Count = 0);
end;
{--------}
function TtdStack.Pop : pointer;
var
  Temp : PslNode;
begin
  if (Count = 0) then
    sError(tdeStackIsEmpty, 'Pop');
  {note that, even if we could, we don't dispose of the top node's
   data; this routine returns it}
  Temp := FHead^.slnNext;
  Result := Temp^.slnData;
  FHead^.slnNext := Temp^.slnNext;
  SLNodeManager.FreeNode(Temp);
  dec(FCount);
end;
{--------}
procedure TtdStack.Push(aItem : pointer);
var
  Temp : PslNode;
begin
  {allocate a new node and put it at the top of the list}
  Temp := PslNode(SLNodeManager.AllocNode);
  Temp^.slnData := aItem;
  Temp^.slnNext := FHead^.slnNext;
  FHead^.slnNext := Temp;
  inc(FCount);
end;
{--------}
procedure TtdStack.sError(aErrorCode  : integer;
                    const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdStackException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
class procedure TtdStack.sGetNodeManager;
begin
  {if the node manager hasn't been allocated yet, do so}
  if (SLNodeManager = nil) then
    SLNodeManager := TtdNodeManager.Create(sizeof(TslNode));
end;
{====================================================================}


{===TtdListStack=====================================================}
constructor TtdArrayStack.Create(aDispose  : TtdDisposeProc;
                                aCapacity : integer);
begin
  inherited Create;
  {save the dispose procedure}
  FDispose := aDispose;
  {create the internal TList and make it have aCapacity elements}
  FList := TList.Create;
  if (aCapacity <= 1) then
    aCapacity := 16;
  FList.Count := aCapacity;
end;
{--------}
destructor TtdArrayStack.Destroy;
begin
  if (Count <> 0) then
    Clear;
  FList.Free;
  inherited Destroy;
end;
{--------}
procedure TtdArrayStack.Clear;
var
  i : integer;
begin
  if (Count > 0) then begin
    if Assigned(FDispose) then
      for i := 0 to pred(Count) do
        FDispose(FList[i]);
    FCount := 0;
  end;
end;
{--------}
function TtdArrayStack.Examine : pointer;
begin
  if (Count = 0) then
    asError(tdeStackIsEmpty, 'Examine');
  Result := FList[pred(Count)];
end;
{--------}
function TtdArrayStack.IsEmpty : boolean;
begin
  Result := (Count = 0);
end;
{--------}
procedure TtdArrayStack.asError(aErrorCode  : integer;
            const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdStackException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
procedure TtdArrayStack.asGrow;
begin
  FList.Count := (FList.Count * 3) div 2;
end;
{--------}
function TtdArrayStack.Pop : pointer;
begin
  {make sure we have an item to pop}
  if (Count = 0) then
    asError(tdeStackIsEmpty, 'Pop');
  {decrement the count}
  dec(FCount);
  {the item to pop is at the end of the list}
  Result := FList[Count];
end;
{--------}
procedure TtdArrayStack.Push(aItem : pointer);
begin
  {check to see whether the stack is currently full; if so, grow the
   list}
  if (Count = FList.Count) then
    asGrow;
  {add the item to the end of the stack}
  FList[Count] := aItem;
  {increment the count}
  inc(FCount);
end;
{====================================================================}


{===TtdQueue=========================================================}
constructor TtdQueue.Create(aDispose : TtdDisposeProc);
begin
  inherited Create;
  {save the dispose procedure}
  FDispose := aDispose;
  {get the node manager}
  qGetNodeManager;
  {allocate a head node}
  FHead := PslNode(SLNodeManager.AllocNode);
  FHead^.slnNext := nil;
  FHead^.slnData := nil;
  {make the tail pointer point to the head node}
  FTail := FHead;
end;
{--------}
destructor TtdQueue.Destroy;
begin
  {remove all the remaining nodes; free the head node}
  if (Count <> 0) then
    Clear;
  SLNodeManager.FreeNode(FHead);
  inherited Destroy;
end;
{--------}
procedure TtdQueue.Clear;
var
  Temp : PslNode;
begin
  {delete all the nodes, except the head node; if we can dispose of
   the nodes' data, do so}
  Temp := FHead^.slnNext;
  while (Temp <> nil) do begin
    FHead^.slnNext := Temp^.slnNext;
    if Assigned(FDispose) then
      FDispose(Temp^.slnData);
    SLNodeManager.FreeNode(Temp);
    Temp := FHead^.slnNext;
  end;
  FCount := 0;
  {the queue is now empty so make the tail pointer point to the head
   node}
  FTail := FHead;
end;
{--------}
function TtdQueue.Dequeue : pointer;
var
  Temp : PslNode;
begin
  if (Count = 0) then
    qError(tdeQueueIsEmpty, 'Dequeue');
  Temp := FHead^.slnNext;
  Result := Temp^.slnData;
  FHead^.slnNext := Temp^.slnNext;
  SLNodeManager.FreeNode(Temp);
  dec(FCount);
  {if we've managed to empty the queue, the tail pointer is now
   invalid, so reset it to point to the head node}
  if (Count = 0) then
    FTail := FHead;
end;
{--------}
procedure TtdQueue.Enqueue(aItem : pointer);
var
  Temp : PslNode;
begin
  Temp := PslNode(SLNodeManager.AllocNode);
  Temp^.slnData := aItem;
  Temp^.slnNext := nil;
  {add the new node to the tail of the list and make sure the tail
   pointer points to the newly added node}
  FTail^.slnNext := Temp;
  FTail := Temp;
  inc(FCount);
end;
{--------}
function TtdQueue.Examine : pointer;
begin
  if (Count = 0) then
    qError(tdeQueueIsEmpty, 'Examine');
  Result := FHead^.slnNext^.slnData;
end;
{--------}
function TtdQueue.IsEmpty : boolean;
begin
  Result := (Count = 0);
end;
{--------}
procedure TtdQueue.qError(aErrorCode  : integer;
                    const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdQueueException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
class procedure TtdQueue.qGetNodeManager;
begin
  {if the node manager hasn't been allocated yet, do so}
  if (SLNodeManager = nil) then
    SLNodeManager := TtdNodeManager.Create(sizeof(TslNode));
end;
{====================================================================}


{===TtdArrayQueue=====================================================}
constructor TtdArrayQueue.Create(aDispose  : TtdDisposeProc;
                                aCapacity : integer);
begin
  inherited Create;
  {save the dispose procedure}
  FDispose := aDispose;
  {create the internal TList and make it have aCapacity elements}
  FList := TList.Create;
  if (aCapacity <= 1) then
    aCapacity := 16;
  FList.Count := aCapacity;
end;
{--------}
destructor TtdArrayQueue.Destroy;
begin
  if (Count <> 0) then
    Clear;
  FList.Free;
  inherited Destroy;
end;
{--------}
procedure TtdArrayQueue.aqError(aErrorCode  : integer;
                         const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdQueueException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
procedure TtdArrayQueue.aqGrow;
var
  i     : integer;
  ToInx : integer;
begin
  {grow the list}
  FList.Count := (FList.Count * 3) div 2;
  {the items are now down at the bottom of the list, we need to make
   them into a proper circular queue again}
  if (FHead = 0) then
    FTail := Count
  else begin
    ToInx := FList.Count;
    for i := pred(Count) downto FHead do begin
      dec(ToInx);
      FList[ToInx] := FList[i];
    end;
    FHead := ToInx;
  end;
end;
{--------}
procedure TtdArrayQueue.Clear;
begin
  if (Count > 0) and Assigned(FDispose) then begin
    while (FHead <> FTail) do begin
      FDispose(FList[FHead]);
      FHead := (FHead + 1) mod FList.Count;
    end;
  end;
  {reset the head and tail indexes equal}
  FHead := 0;
  FTail := 0;
  FCount := 0;
end;
{--------}
function TtdArrayQueue.Dequeue : pointer;
begin
  {make sure we have an item to dequeue}
  if (Count = 0) then
    aqError(tdeQueueIsEmpty, 'Dequeue');
  {the item to dequeue is at the head of the queue}
  Result := FList[FHead];
  {move the head index, making sure it's still a valid index;
   decrement the count}
  FHead := (FHead + 1) mod FList.Count;
  dec(FCount);
end;
{--------}
procedure TtdArrayQueue.Enqueue(aItem : pointer);
begin
  {add the item to the tail of the queue}
  FList[FTail] := aItem;
  {move the tail index, making sure it's still a valid index;
   increment the count}
  FTail := (FTail + 1) mod FList.Count;
  inc(FCount);
  {if, having added another item, we find that the tail and head
   indexes are equal, we need to grow the array in size}
  if (FTail = FHead) then
    aqGrow;
end;
{--------}
function TtdArrayQueue.Examine : pointer;
begin
  {make sure we have an item to examine}
  if (Count = 0) then
    aqError(tdeQueueIsEmpty, 'Dequeue');
  {return the item at the head of the queue without dequeuing it}
  Result := FList[FHead];
end;
{--------}
function TtdArrayQueue.IsEmpty : boolean;
begin
  Result := (Count = 0);
end;
{====================================================================}


procedure FinalizeUnit; far;
begin
  SLNodeManager.Free;
end;

initialization
  SLNodeManager := nil;
  {$IFDEF Delphi1}
  AddExitProc(FinalizeUnit);
  {$ENDIF}

{$IFNDEF Delphi1}
finalization
  FinalizeUnit;
{$ENDIF}

end.
