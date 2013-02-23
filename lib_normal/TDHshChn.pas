(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 2000                *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDHshChn                                                         *)
(* Dynamic Hash table using chaining                                *)
(********************************************************************)

unit TDHshChn;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  Classes,
  TDBasics,
  TDNdeMgr,
  TDHshBse;

type
  TtdHashChainUsage = (       {Usage of hash table chains...}
                  hcuFirst,   {..insert at the beginning}
                  hcuLast);   {..insert at the end}

type
  TtdHashTableChained = class
    {-a hash table that uses chaining to resolve collisions}
    private
      FChainUsage : TtdHashChainUsage;
      FCount      : integer;
      FDispose    : TtdDisposeProc;
      FHashFunc   : TtdHashFunc;
      FName       : TtdNameString;
      FTable      : TList;
      FNodeMgr    : TtdNodeManager;
      FMaxLoadFactor : integer;
    protected
      procedure htcSetMaxLoadFactor(aMLF : integer);

      procedure htcAllocHeads(aTable : TList);
      procedure htcAlterTableSize(aNewTableSize : integer);
      procedure htcError(aErrorCode : integer;
                   const aMethodName : TtdNameString);
      function htcFindPrim(const aKey    : string;
                             var aInx    : integer;
                             var aParent : pointer) : boolean;
      procedure htcFreeHeads(aTable : TList);
      procedure htcGrowTable;
    public
      constructor Create(aTableSize : integer;
                         aHashFunc  : TtdHashFunc;
                         aDispose   : TtdDisposeProc);
      destructor Destroy; override;

      procedure Delete(const aKey : string);
      procedure Clear;
      function Find(const aKey  : string;
                      var aItem : pointer) : boolean;
      procedure Insert(const aKey : string; aItem : pointer);
      function Visit(aVisitProc : TtdVisitProc;
                     aExtraData : pointer) : pointer;

      property Count : integer
         read FCount;
      property MaxLoadFactor : integer
         read FMaxLoadFactor write htcSetMaxLoadFactor;
      property Name : TtdNameString
         read FName write FName;
      property ChainUsage : TtdHashChainUsage
         read FChainUsage write FChainUsage;

      {$IFDEF DebugMode}
      procedure debugPrint(const aFileName : string;
                                 aDetailed : boolean);
      {$ENDIF}
  end;

implementation

const
  UnitName = 'TDHshChn';

type
  PHashedItem = ^THashedItem;
  THashedItem = packed record
    hiNext : PHashedItem;
    hiItem : pointer;
    {$IFDEF Delphi1}
    hiKey  : PString;
    {$ELSE}
    hiKey  : string;
    {$ENDIF}
  end;


{===TtdHashTableChained==============================================}
constructor TtdHashTableChained.Create(aTableSize : integer;
                                       aHashFunc  : TtdHashFunc;
                                       aDispose   : TtdDisposeProc);
begin
  inherited Create;
  FDispose := aDispose;
  if not Assigned(aHashFunc) then
    htcError(tdeHashTblNoHashFunc, 'Create');
  FHashFunc := aHashFunc;
  FTable := TList.Create;
  FTable.Count := TDGetClosestPrime(aTableSize);
  FNodeMgr := TtdNodeManager.Create(sizeof(THashedItem));
  htcAllocHeads(FTable);
  FMaxLoadFactor := 5;
end;
{--------}
destructor TtdHashTableChained.Destroy;
begin
  if (FTable <> nil) then begin
    Clear;
    htcFreeHeads(FTable);
    FTable.Destroy;
  end;
  FNodeMgr.Free;
  inherited Destroy;
end;
{--------}
{$IFDEF DebugMode}
procedure TtdHashTableChained.debugPrint(const aFileName : string;
                                              aDetailed : boolean);
var
  Inx    : integer;
  Walker : PHashedItem;
  TotLen : integer;
  F      : System.Text;
begin
  System.Assign(F, aFileName);
  System.Rewrite(F);
  try
    writeln(F, 'Hash Table (Chained) Debug Print [', aFileName, ']');
    writeln(F, '--------------------------------');
    if aDetailed then
      writeln(F);
    for Inx := 0 to pred(FTable.Count) do begin
      writeln(F, 'Slot ', Inx);
      TotLen := 0;
      Walker := PHashedItem(FTable.List^[Inx])^.hiNext;
      while (Walker <> nil) do begin
        if aDetailed then begin
          {$IFDEF Delphi1}
          writeln(F, '  - ', Walker^.hiKey^);
          {$ELSE}
          writeln(F, '  - ', Walker^.hiKey);
          {$ENDIF}
        end;
        inc(TotLen);
        Walker := Walker^.hiNext;
      end;
      writeln(F, '  Count of items: ', TotLen);
    end;
    writeln(F);
    writeln(F, 'The hash table has ', FCount,
               ' element(s) in ', FTable.Count,
               ' slots');
    writeln(F, '..an average of ', FCount / FTable.Count:7:2,
               ' items per slot');
  finally
    System.Close(F);
  end;
end;
{$ENDIF}
{--------}
procedure TtdHashTableChained.Delete(const aKey : string);
var
  Inx  : integer;
  Parent : pointer;
  Temp : PHashedItem;
begin
  {find the key}
  if not htcFindPrim(aKey, Inx, Parent) then
    htcError(tdeHashTblKeyNotFound, 'Delete');
  {delete the item and the key in this node}
  Temp := PHashedItem(Parent)^.hiNext;
  if Assigned(FDispose) then
    FDispose(Temp^.hiItem);
  {$IFDEF Delphi1}
  DisposeStr(Temp^.hiKey);
  {$ELSE}
  Temp^.hiKey := '';
  {$ENDIF}
  {unlink the node and free it}
  PHashedItem(Parent)^.hiNext := Temp^.hiNext;
  FNodeMgr.FreeNode(Temp);
  dec(FCount);
end;
{--------}
procedure TtdHashTableChained.Clear;
var
  Inx : integer;
  Temp, Walker : PHashedItem;
begin
  for Inx := 0 to pred(FTable.Count) do begin
    Walker := PHashedItem(FTable.List^[Inx])^.hiNext;
    while (Walker <> nil) do begin
      if Assigned(FDispose) then
        FDispose(Walker^.hiItem);
      {$IFDEF Delphi1}
      DisposeStr(Walker^.hiKey);
      {$ELSE}
      Walker^.hiKey := '';
      {$ENDIF}
      Temp := Walker;
      Walker := Walker^.hiNext;
      FNodeMgr.FreeNode(Temp);
    end;
    PHashedItem(FTable.List^[Inx])^.hiNext := nil;
  end;
  FCount := 0;
end;
{--------}
function TtdHashTableChained.Find(const aKey  : string;
                                    var aItem : pointer) : boolean;
var
  Inx    : integer;
  Parent : pointer;
begin
  if htcFindPrim(aKey, Inx, Parent) then begin
    Result := true;
    aItem := PHashedItem(Parent)^.hiNext^.hiItem;
  end
  else begin
    Result := false;
    aItem := nil;
  end;
end;
{--------}
procedure TtdHashTableChained.htcAllocHeads(aTable : TList);
var
  Inx  : integer;
begin
  for Inx := 0 to pred(aTable.Count) do 
    aTable.List^[Inx] := FNodeMgr.AllocNodeClear;
end;
{--------}
procedure TtdHashTableChained.htcAlterTableSize(aNewTableSize : integer);
var
  Inx      : integer;
  OldTable : TList;
  Walker, Temp : PHashedItem;
begin
  {save the old table}
  OldTable := FTable;
  {allocate a new table}
  FTable := TList.Create;
  try
    FTable.Count := aNewTableSize;
    htcAllocHeads(FTable);
    {read through the old table and transfer over the keys & items to
     the new table by inserting them}
    FCount := 0;
    for Inx := 0 to pred(OldTable.Count) do begin
      Walker := PHashedItem(OldTable.List^[Inx])^.hiNext;
      while (Walker <> nil) do begin
        {$IFDEF Delphi1}
        Insert(Walker^.hiKey^, Walker^.hiItem);
        {$ELSE}
        Insert(Walker^.hiKey, Walker^.hiItem);
        {$ENDIF}
        Walker := Walker^.hiNext;
      end;
    end;
  except
    {if we get an exception, try to clean up and leave the hash
     table in a consistent state}
    Clear;
    htcFreeHeads(FTable);
    FTable.Free;
    FTable := OldTable;
    raise;
  end;
  {the new table is now fully populated with all the items and their
   keys, so destroy the old table and its linked lists}
  for Inx := 0 to pred(OldTable.Count) do begin
    Walker := PHashedItem(OldTable.List^[Inx])^.hiNext;
    while (Walker <> nil) do begin
      {$IFDEF Delphi1}
      DisposeStr(Walker^.hiKey);
      {$ELSE}
      Walker^.hiKey := '';
      {$ENDIF}
      Temp := Walker;
      Walker := Walker^.hiNext;
      FNodeMgr.FreeNode(Temp);
    end;
    PHashedItem(OldTable.List^[Inx])^.hiNext := nil;
  end;
  htcFreeHeads(OldTable);
  OldTable.Free;
end;
{--------}
procedure TtdHashTableChained.htcError(aErrorCode  : integer;
                                const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdHashTableException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
function TtdHashTableChained.htcFindPrim(const aKey    : string;
                                           var aInx    : integer;
                                           var aParent : pointer) : boolean;
var
  Inx  : integer;
  Head, Walker, Parent : PHashedItem;
begin
  {calculate the hash for the string}
  Inx := FHashFunc(aKey, FTable.Count);
  {assume there's a linked list at the Inx'th slot}
  Head := PHashedItem(FTable.List^[Inx]);
  {start walking the linked list looking for the key}
  Parent := Head;
  Walker := Head^.hiNext;
  while (Walker <> nil) do begin
    {$IFDEF Delphi1}
    if (Walker^.hiKey^ = aKey) then begin
    {$ELSE}
    if (Walker^.hiKey = aKey) then begin
    {$ENDIF}
      if (ChainUsage = hcuFirst) and (Parent <> Head) then begin
        Parent^.hiNext := Walker^.hiNext;
        Walker^.hiNext := Head^.hiNext;
        Head^.hiNext := Walker;
        Parent := Head;
      end;
      aInx := Inx;
      aParent := Parent;
      Result := true;
      Exit;
    end;
    Parent := Walker;
    Walker := Walker^.hiNext;
  end;
  {if we reach here, the key was not found}
  aInx := Inx;
  if ChainUsage = hcuLast then
    aParent := Parent
  else
    aParent := Head;
  Result := false;
end;
{--------}
procedure TtdHashTableChained.htcFreeHeads(aTable : TList);
var
  Inx  : integer;
begin
  for Inx := 0 to pred(aTable.Count) do
    FNodeMgr.FreeNode(aTable.List^[Inx]);
end;
{--------}
procedure TtdHashTableChained.htcGrowTable;
begin
  {make the table roughly twice as large as before}
  htcAlterTableSize(TDGetClosestPrime(succ(FTable.Count * 2)));
end;
{--------}
procedure TtdHashTableChained.htcSetMaxLoadFactor(aMLF : integer);
begin
  if (aMLF <> FMaxLoadFactor) and
     (aMLF > 1) then begin
    FMaxLoadFactor := aMLF;
    while (FCount > (aMLF * FTable.Count)) do
      htcGrowTable;
  end;
end;
{--------}
procedure TtdHashTableChained.Insert(const aKey  : string;
                                           aItem : pointer);
var
  Inx    : integer;
  Parent : pointer;
  NewNode : PHashedItem;
begin
  if htcFindPrim(aKey, Inx, Parent) then
    htcError(tdeHashTblKeyExists, 'Insert');
  NewNode := FNodeMgr.AllocNodeClear;
  {$IFDEF Delphi1}
  NewNode^.hiKey := NewStr(aKey);
  {$ELSE}
  NewNode^.hiKey := aKey;
  {$ENDIF}
  NewNode^.hiItem := aItem;
  NewNode^.hiNext := PHashedItem(Parent)^.hiNext;
  PHashedItem(Parent)^.hiNext := NewNode;
  inc(FCount);
  {grow the table if we're over the maximum load factor}
  if (FCount  > (FMaxLoadFactor * FTable.Count)) then
    htcGrowTable;
end;
{--------}
function TtdHashTableChained.Visit(aVisitProc : TtdVisitProc;
                                   aExtraData : pointer) : pointer;
var
  Inx     : integer;
  Walker  : PHashedItem;
  StopNow : boolean;
begin
  StopNow := false;
  for Inx := 0 to pred(FTable.Count) do begin
    Walker := PHashedItem(FTable.List^[Inx])^.hiNext;
    while (Walker <> nil) do begin
      aVisitProc(Walker^.hiItem, aExtraData, StopNow);
      if StopNow then begin
        Result := Walker^.hiItem;
        Exit;
      end;
      Walker := Walker^.hiNext;
    end;
  end;
  Result := nil;
end;
{====================================================================}

end.
