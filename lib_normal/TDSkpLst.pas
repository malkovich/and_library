(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDSkpLst                                                         *)
(* Skip list class                                                  *)
(********************************************************************)

unit TDSkpLst;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  TDBasics,
  TDRandom;

const
  tdcMaxSkipLevels = 12;

type
  PskNode = ^TskNode;

  TskNodeArray = array [0..pred(tdcMaxSkipLevels)] of PskNode;

  TskNode = packed record
    sknData  : pointer;
    sknLevel : longint;
    sknPrev  : PskNode;
    sknNext  : TskNodeArray;
  end;

  TtdSkipList = class
    private
      FCompare  : TtdCompareFunc;
      FCount    : integer;
      FCursor   : PskNode;
      FDispose  : TtdDisposeProc;
      FHead     : PskNode;
      FMaxLevel : integer;
      FName     : TtdNameString;
      FPRNG     : TtdMinStandardPRNG;
      FTail     : PskNode;
    protected
      class function slAllocNode(aLevel : integer) : PskNode;
      procedure slError(aErrorCode  : integer;
                  const aMethodName : TtdNameString);
      procedure slFreeNode(aNode : PskNode);
      class procedure slGetNodeManagers;
      function slSearchPrim(aItem : pointer;
                        var aBeforeNodes : TskNodeArray) : boolean;
    public
      constructor Create(aCompare : TtdCompareFunc;
                         aDispose : TtdDisposeProc);
      destructor Destroy; override;

      procedure Add(aItem : pointer);
      procedure Clear;
      procedure Delete;
      function Examine : pointer;
      function IsAfterLast : boolean;
      function IsBeforeFirst : boolean;
      function IsEmpty : boolean;
      procedure MoveAfterLast;
      procedure MoveBeforeFirst;
      procedure MoveNext;
      procedure MovePrior;
      procedure Remove(aItem : pointer);
      function Search(aItem : pointer) : boolean;

      {$IFDEF DebugMode}
      procedure Print;
      {$ENDIF}

      property Count : integer
                  read FCount;
      property MaxLevel : integer
                  read FMaxLevel;
      property Name : TtdNameString
                  read FName write FName;
  end;

implementation

uses
  TDNdeMgr;

const
  UnitName = 'TDSkpLst';

var
  NodeSize : array [0..pred(tdcMaxSkipLevels)] of integer;
  SLNodeManager : array [0..pred(tdcMaxSkipLevels)] of TtdNodeManager;

{===TtdSkipList======================================================}
constructor TtdSkipList.Create(aCompare : TtdCompareFunc;
                               aDispose : TtdDisposeProc);
var
  i : integer;
begin
  inherited Create;
  {the compare function cannot be nil}
  if not Assigned(aCompare) then
    slError(tdeSkpLstNoCompare, 'Create');
  {get the node managers}
  slGetNodeManagers;
  {allocate a head node}
  FHead := slAllocNode(pred(tdcMaxSkipLevels));
  FHead^.sknData := nil;
  {allocate a tail node}
  FTail := slAllocNode(0);
  FTail^.sknData := nil;
  {set the forward and back links in both the head and tail nodes}
  for i := 0 to pred(tdcMaxSkipLevels) do
    FHead^.sknNext[i] := FTail;
  FHead^.sknPrev := nil;
  FTail^.sknNext[0] := nil;
  FTail^.sknPrev := FHead;
  {set the cursor to the head node}
  FCursor := FHead;
  {save the compare function and the dispose procedure}
  FCompare := aCompare;
  FDispose := aDispose;
  {create a random number generator}
  FPRNG := TtdMinStandardPRNG.Create(0);
end;
{--------}
destructor TtdSkipList.Destroy;
begin
  Clear;
  slFreeNode(FHead);
  slFreeNode(FTail);
  FPRNG.Free;
  inherited Destroy;
end;
{--------}
procedure TtdSkipList.Add(aItem : pointer);
var
  i, Level    : integer;
  NewNode     : PskNode;
  BeforeNodes : TskNodeArray;
begin
  {search for the item and initialize the BeforeNodes array}
  if slSearchPrim(aItem, BeforeNodes) then
    slError(tdeSkpLstDupItem, 'Add');
  {calculate the level for the new node}
  Level := 0;
  while (Level <= MaxLevel) and (FPRNG.AsDouble < 0.25) do
    inc(Level);
  {if we've gone beyond the maximum level, save it}
  if (Level > MaxLevel) then
    inc(FMaxLevel);
  {allocate the new node}
  NewNode := slAllocNode(Level);
  NewNode^.sknData := aItem;
  {patch up the links on level 0 - a doubly linked list}
  NewNode^.sknPrev := BeforeNodes[0];
  NewNode^.sknNext[0] := BeforeNodes[0]^.sknNext[0];
  BeforeNodes[0]^.sknNext[0] := NewNode;
  NewNode^.sknNext[0]^.sknPrev := NewNode;
  {patch up the links on the other levels - all singly linked lists}
  for i := 1 to Level do begin
    NewNode^.sknNext[i] := BeforeNodes[i]^.sknNext[i];
    BeforeNodes[i]^.sknNext[i] := NewNode;
  end;
  {we now have one more node in the skip list}
  inc(FCount);
end;
{--------}
procedure TtdSkipList.Clear;
var
  i : integer;
  Walker, Temp : PskNode;
begin
  {walk level 0, freeing all the nodes}
  Walker := FHead^.sknNext[0];
  while (Walker <> FTail) do begin
    Temp := Walker;
    Walker := Walker^.sknNext[0];
    slFreeNode(Temp);
  end;
  {patch up the head and tail nodes}
  for i := 0 to pred(tdcMaxSkipLevels) do
    FHead^.sknNext[i] := FTail;
  FTail^.sknPrev := FHead;
  FCount := 0;
end;
{--------}
procedure TtdSkipList.Delete;
begin
  {we can't delete at the head or tail}
  if (FCursor = FHead) or (FCursor = FTail) then
    slError(tdeListCannotDelete, 'Delete');
  {remove the cursor's item}
  Remove(FCursor^.sknData);
end;
{--------}
function TtdSkipList.Examine : pointer;
begin
  Result := FCursor^.sknData;
end;
{--------}
function TtdSkipList.IsAfterLast : boolean;
begin
  Result := FCursor = FTail;
end;
{--------}
function TtdSkipList.IsBeforeFirst : boolean;
begin
  Result := FCursor = FHead;
end;
{--------}
function TtdSkipList.IsEmpty : boolean;
begin
  Result := Count = 0;
end;
{--------}
procedure TtdSkipList.MoveAfterLast;
begin
  FCursor := FTail;
end;
{--------}
procedure TtdSkipList.MoveBeforeFirst;
begin
  FCursor := FHead;
end;
{--------}
procedure TtdSkipList.MoveNext;
begin
  if (FCursor <> FTail) then
    FCursor := FCursor^.sknNext[0];
end;
{--------}
procedure TtdSkipList.MovePrior;
begin
  if (FCursor <> FHead) then
    FCursor := FCursor^.sknPrev;
end;
{--------}
{$IFDEF DebugMode}
procedure TtdSkipList.Print;
var
  BeforeNodes : TskNodeArray;
  i           : integer;
  Temp        : PskNode;
  TempLevel   : integer;
begin
  {set the BeforeNodes array to point to the head node}
  for i := 0 to pred(tdcMaxSkipLevels) do
    BeforeNodes[i] := FHead;
  Temp := FHead;
  TempLevel := Temp^.sknLevel;
  for i := 0 to TempLevel do
    write('*');
  writeln;
  Temp := Temp^.sknNext[0];
  while Temp <> FTail do begin
    TempLevel := Temp^.sknLevel;
    if (BeforeNodes[TempLevel]^.sknNext[TempLevel] <> Temp) then begin
      writeln('---Wrong pointer from before');
      readln;
    end;
    for i := 0 to TempLevel do
      BeforeNodes[i] := Temp;
    for i := 0 to TempLevel do
      write('*');
    writeln;
    Temp := Temp^.sknNext[0];
  end;
end;
{$ENDIF}
{--------}
procedure TtdSkipList.Remove(aItem : pointer);
var
  i, Level    : integer;
  Temp        : PskNode;
  BeforeNodes : TskNodeArray;
begin
  {search for the item and initialize the BeforeNodes array}
  if not slSearchPrim(aItem, BeforeNodes) then
    slError(tdeSkpLstItemMissing, 'Remove');
  {the only valid before nodes are from the skip list's maximum level
   down to this node's level; we need to get the before nodes for the
   others}
  Level := FCursor^.sknLevel;
  if (Level > 0) then begin
    for i := pred(Level) downto 0 do begin
      BeforeNodes[i] := BeforeNodes[i+1];
      while (BeforeNodes[i]^.sknNext[i] <> FCursor) do
        BeforeNodes[i] := BeforeNodes[i]^.sknNext[i];
    end;
  end;
  {patch up the links on level 0 - doubly linked list}
  BeforeNodes[0]^.sknNext[0] := FCursor^.sknNext[0];
  FCursor^.sknNext[0]^.sknPrev := BeforeNodes[0];
  {patch up the links on the other levels - all singly linked lists}
  for i := 1 to Level do
    BeforeNodes[i]^.sknNext[i] := FCursor^.sknNext[i];
  {reset cursor, dispose of the node}
  Temp := FCursor;
  FCursor := FCursor^.sknNext[0];
  slFreeNode(Temp);
  {we now have one less node in the skip list}
  dec(FCount);
end;
{--------}
function TtdSkipList.Search(aItem : pointer) : boolean;
var
  BeforeNodes : TskNodeArray;
begin
  Result := slSearchPrim(aItem, BeforeNodes);
end;
{--------}
class function TtdSkipList.slAllocNode(aLevel : integer) : PskNode;
begin
  Result := SLNodeManager[aLevel].AllocNode;
  Result^.sknLevel := aLevel;
end;
{--------}
procedure TtdSkipList.slError(aErrorCode  : integer;
                        const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdSkipListException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
procedure TtdSkipList.slFreeNode(aNode : PskNode);
begin
  if (aNode <> nil) then begin
    if Assigned(FDispose) then
      FDispose(aNode^.sknData);
    SLNodeManager[aNode^.sknLevel].FreeNode(aNode);
  end;
end;
{--------}
class procedure TtdSkipList.slGetNodeManagers;
var
  i : integer;
begin
  {if the node managers haven't been allocated yet, do so}
  if (SLNodeManager[0] = nil) then
    for i := 0 to pred(tdcMaxSkipLevels) do
      SLNodeManager[i] := TtdNodeManager.Create(NodeSize[i]);
end;
{--------}
function TtdSkipList.slSearchPrim(aItem : pointer;
                              var aBeforeNodes : TskNodeArray) : boolean;
var
  Level  : integer;
  Walker : PskNode;
  Temp   : PskNode;
  CompareResult : integer;
begin
  {set the entire BeforeNodes array to refer to the head node}
  for Level := 0 to pred(tdcMaxSkipLevels) do
    aBeforeNodes[Level] := FHead;
  {initialize}
  Walker := FHead;
  Level := MaxLevel;
  {start zeroing in on the item we want}
  while (Level >= 0) do begin
    {get the next node at this level}
    Temp := Walker^.sknNext[Level];
    {if the next node is the tail, pretend that it is greater than the
     item we're looking for}
    if (Temp = FTail) then
      CompareResult := 1
    {otherwise, compare the next node's data with our item}
    else
      CompareResult := FCompare(Temp^.sknData, aItem);
    {if the node's data and item are equal, we found it; exit now,
     there's no need to go any further }
    if (CompareResult = 0) then begin
      aBeforeNodes[Level] := Walker;
      FCursor := Temp;
      Result := true;
      Exit;
    end;
    {if less than, then advance the walker node}
    if (CompareResult < 0) then begin
      Walker := Temp;
    end
    {if greater than, save the before node, drop down a level}
    else begin
      aBeforeNodes[Level] := Walker;
      dec(Level);
    end;
  end;
  {reaching this point means that the item was not found}
  Result := false;
end;
{====================================================================}

procedure InitializeUnit;
var
  i : integer;
begin
  {set up the node sizes}
  for i := 0 to pred(tdcMaxSkipLevels) do
    NodeSize[i] := (i + 3) * sizeof(pointer) + sizeof(longint);
  {setting the first node manager to nil is a signal that none of the
   node managers have been created}
  SLNodeManager[0] := nil;
end;

procedure FinalizeUnit; far;
var
  i : integer;
begin
  if (SLNodeManager[0] <> nil) then
    for i := 0 to pred(tdcMaxSkipLevels) do
      SLNodeManager[i].Free;
end;

initialization
  InitializeUnit;
  {$IFDEF Delphi1}
  AddExitProc(FinalizeUnit);
  {$ENDIF}

{$IFNDEF Delphi1}
finalization
  FinalizeUnit;
{$ENDIF}

end.
