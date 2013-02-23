(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDHshLnP                                                         *)
(* Dynamic Hash table using linear probing                          *)
(********************************************************************)

unit TDHshLnP;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  TDBasics,
  TDRecLst,
  TDHshBse;

type
  TtdHashTableLinear = class
    {-a hash table that uses linear probing to resolve collisions}
    private
      FCount    : integer;
      FDispose  : TtdDisposeProc;
      FHashFunc : TtdHashFunc;
      FName     : TtdNameString;
      FTable    : TtdRecordList;
      {$IFDEF DebugMode}
      htlDebugSeeks: integer;
      htlDebugHash : integer;
      {$ENDIF}
    protected
      procedure htlAlterTableSize(aNewTableSize : integer);
      procedure htlError(aErrorCode : integer;
                   const aMethodName : TtdNameString);
      procedure htlGrowTable;
      function htlIndexOf(const aKey  : string;
                            var aSlot : pointer) : integer;
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
      property Name : TtdNameString
         read FName write FName;

      {$IFDEF DebugMode}
      procedure debugPrint(const aFileName : string;
                                 aDetailed : boolean);
      {$ENDIF}
  end;

implementation

const
  UnitName = 'TDHshLnP';

type
  PHashSlot = ^THashSlot;
  THashSlot = packed record
    {$IFDEF Delphi1}
    hsKey  : PString;
    {$ELSE}
    hsKey  : string;
    {$ENDIF}
    hsItem : pointer;
    hsInUse: boolean;
  end;

{===TtdHashTableLinear===============================================}
constructor TtdHashTableLinear.Create(aTableSize : integer;
                                      aHashFunc  : TtdHashFunc;
                                      aDispose   : TtdDisposeProc);
begin
  inherited Create;
  FDispose := aDispose;
  if not Assigned(aHashFunc) then
    htlError(tdeHashTblNoHashFunc, 'Create');
  FHashFunc := aHashFunc;
  FTable := TtdRecordList.Create(sizeof(THashSlot));
  FTable.Name := ClassName + ': hash table';
  FTable.Count := TDGetClosestPrime(aTableSize);
  {$IFDEF DebugMode}
  htlDebugHash := -1;
  {$ENDIF}
end;
{--------}
destructor TtdHashTableLinear.Destroy;
begin
  if (FTable <> nil) then begin
    Clear;
    FTable.Destroy;
  end;
  inherited Destroy;
end;
{--------}
{$IFDEF DebugMode}
procedure TtdHashTableLinear.debugPrint(const aFileName : string;
                                              aDetailed : boolean);
const
  StateStrs : array [boolean] of string[9] =
              ('<empty>  ', '<in use> ');
var
  Inx         : integer;
  discardSlot : pointer;
  TotSeeks    : integer;
  F           : System.Text;
begin
  System.Assign(F, aFileName);
  System.Rewrite(F);
  try
    writeln(F, 'Hash Table (Linear Probe) Debug Print [', aFileName, ']');
    writeln(F, '-------------------------------------');
    if aDetailed then
      writeln(F);
    TotSeeks := 0;
    for Inx := 0 to pred(FTable.Count) do begin
      with PHashSlot(FTable[Inx])^ do begin
        if aDetailed then
          write(F, Inx:4, ': ', StateStrs[hsInUse]);
        if hsInUse then begin
          {$IFDEF Delphi1}
          htlIndexOf(hsKey^, discardSlot);
          {$ELSE}
          htlIndexOf(hsKey, discardSlot);
          {$ENDIF}
          inc(TotSeeks, htlDebugSeeks);
          if aDetailed then
            {$IFDEF Delphi1}
            writeln(F, '  ', hsKey^, '  (seekcount: ', htlDebugSeeks, ')')
            {$ELSE}
            writeln(F, '  ', hsKey, '  (seekcount: ', htlDebugSeeks, ')')
            {$ENDIF}
        end
        else
          if aDetailed then
            writeln(F);
      end;
    end;
    writeln(F);
    writeln(F, 'The table has ', FCount,
               ' element(s) (of ', FTable.Count,
               ') and is ', (100.0 * FCount / FTable.Count):0:2,
               '% full');
    if (FCount > 0) then
      writeln(F, 'The average path length for a hit is ',
                 (TotSeeks / FCount):0:2, ' probes');
    TotSeeks := 0;
    for Inx := 0 to pred(FTable.Count) do begin
      htlDebugHash := Inx;
      htlIndexOf('%%%%%debug', discardSlot);
      inc(TotSeeks, htlDebugSeeks);
    end;
    writeln(F, 'The average path length for a miss is ',
               (TotSeeks / FTable.Count):0:2, ' probes');
  finally
    System.Close(F);
  end;
  htlDebugHash := -1;
end;
{$ENDIF}
{--------}
procedure TtdHashTableLinear.Delete(const aKey : string);
var
  Inx  : integer;
  ItemSlot : pointer;
  Slot : PHashSlot;
  Key  : string;
  Item : pointer;
begin
  {find the key}
  Inx := htlIndexOf(aKey, ItemSlot);
  if (Inx = -1) then
    htlError(tdeHashTblKeyNotFound, 'Delete');
  {delete the item and the key in this slot}
  with PHashSlot(ItemSlot)^ do begin
    if Assigned(FDispose) then
      FDispose(hsItem);
    {$IFDEF Delphi1}
    DisposeStr(hsKey);
    {$ELSE}
    hsKey := '';
    {$ENDIF}
    hsInUse := false;
  end;
  dec(FCount);
  {now reinsert all subsequent items until we reach an empty slot}
  inc(Inx);
  if (Inx = FTable.Count) then
    Inx := 0;
  Slot := PHashSlot(FTable[Inx]);
  while Slot^.hsInUse do begin
    {save the item and key; remove key from slot}
    Item := Slot^.hsItem;
    {$IFDEF Delphi1}
    Key := Slot^.hsKey^;
    DisposeStr(Slot^.hsKey);
    {$ELSE}
    Key := Slot^.hsKey;
    Slot^.hsKey := '';
    {$ENDIF}
    {mark the slot as empty}
    Slot^.hsInUse := false;
    dec(FCount);
    {reinsert the item and its key}
    Insert(Key, Item);
    {move to the next slot}
    inc(Inx);
    if (Inx = FTable.Count) then
      Inx := 0;
    Slot := PHashSlot(FTable[Inx]);
  end;
end;
{--------}
procedure TtdHashTableLinear.Clear;
var
  Inx : integer;
begin
  for Inx := 0 to pred(FTable.Count) do begin
    with PHashSlot(FTable[Inx])^ do begin
      if hsInUse then begin
        if Assigned(FDispose) then
          FDispose(hsItem);
        {$IFDEF Delphi1}
        DisposeStr(hsKey);
        {$ELSE}
        hsKey := '';
        {$ENDIF}
      end;
      hsInUse := false;
    end;
  end;
  FCount := 0;
end;
{--------}
function TtdHashTableLinear.Find(const aKey  : string;
                                   var aItem : pointer) : boolean;
var
  Slot : pointer;
begin
  if (htlIndexOf(aKey, Slot) <> -1) then begin
    Result := true;
    aItem := PHashSlot(Slot)^.hsItem;
  end
  else begin
    Result := false;
    aItem := nil;
  end;
end;
{--------}
procedure TtdHashTableLinear.htlAlterTableSize(aNewTableSize : integer);
var
  Inx          : integer;
  OldTable     : TtdRecordList;
begin
  {save the old table}
  OldTable := FTable;
  {allocate a new table}
  FTable := TtdRecordList.Create(sizeof(THashSlot));
  try
    FTable.Count := aNewTableSize;
    {read through the old table and transfer over the keys & items}
    FCount := 0;
    for Inx := 0 to pred(OldTable.Count) do
      with PHashSlot(OldTable[Inx])^ do
        if hsInUse then begin
          {$IFDEF Delphi1}
          Insert(hsKey^, hsItem);
          DisposeStr(hsKey);
          {$ELSE}
          Insert(hsKey, hsItem);
          hsKey := '';
          {$ENDIF}
        end;
  except
    {if we get an exception, try to clean up and leave the hash
     table in a consistent state}
    FTable.Free;
    FTable := OldTable;
    raise;
  end;
  {finally free the old table}
  OldTable.Free;
end;
{--------}
procedure TtdHashTableLinear.htlError(aErrorCode  : integer;
                                const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdHashTableException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
procedure TtdHashTableLinear.htlGrowTable;
begin
  {make the table roughly twice as large as before}
  htlAlterTableSize(TDGetClosestPrime(succ(FTable.Count * 2)));
end;
{--------}
function TtdHashTableLinear.htlIndexOf(const aKey  : string;
                                         var aSlot : pointer)
                                                   : integer;
var
  Inx      : integer;
  CurSlot  : PHashSlot;
  FirstInx : integer;
begin
  {calculate the hash for the string, make a note of it so we can
   find out when (if) we wrap around the table completely}
  {$IFDEF DebugMode}
  htlDebugSeeks := 1;
  if (htlDebugHash <> -1) then
    Inx := htlDebugHash
  else
  {$ENDIF}
  Inx := FHashFunc(aKey, FTable.Count);
  FirstInx := Inx;
  {do forever - we'll be exiting out of the loop when needed}
  while true do begin
    {with the current slot...}
    CurSlot := PHashSlot(FTable[Inx]);
    with CurSlot^ do begin
      if not hsInUse then begin
        {the slot is 'empty', we must stop the linear
         probe and return this slot}
        aSlot := CurSlot;
        Result := -1;
        Exit;
      end
      else begin
        {the slot is 'in use', we check to see if it's our
         key, if it is, exit returning the index and slot}
        {$IFDEF Delphi1}
        if (hsKey^ = aKey) then begin
        {$ELSE}
        if (hsKey = aKey) then begin
        {$ENDIF}
          aSlot := CurSlot;
          Result := Inx;
          Exit;
        end;
      end;
    end;
    {we didn't find the key or an empty slot this time around, so
     increment the index (taking care of the wraparound) and exit if
     we've got back to the start again}
    inc(Inx);
    if (Inx = FTable.Count) then
      Inx := 0;
    if (Inx = FirstInx) then begin
      aSlot := nil; {this signifies the table is full}
      Result := -1;
      Exit;
    end;
    {$IFDEF DebugMode}
    inc(htlDebugSeeks);
    {$ENDIF}
  end;{forever loop}
end;
{--------}
procedure TtdHashTableLinear.Insert(const aKey  : string;
                                          aItem : pointer);
var
  Slot : pointer;
begin
  if (htlIndexOf(aKey, Slot) <> -1) then
    htlError(tdeHashTblKeyExists, 'Insert');
  if (Slot = nil) then
    htlError(tdeHashTblIsFull, 'Insert');
  with PHashSlot(Slot)^ do begin
    {$IFDEF Delphi1}
    hsKey := NewStr(aKey);
    {$ELSE}
    hsKey := aKey;
    {$ENDIF}
    hsItem := aItem;
    hsInUse := true;
  end;
  inc(FCount);
  {grow the table if we're over 2/3 full}
  if ((FCount * 3) > (FTable.Count * 2)) then
    htlGrowTable;
end;
{--------}
function TtdHashTableLinear.Visit(aVisitProc : TtdVisitProc;
                                  aExtraData : pointer) : pointer;
var
  Inx     : integer;
  Walker  : PHashSlot;
  StopNow : boolean;
begin
  for Inx := 0 to pred(FTable.Count) do begin
    Walker := PHashSlot(FTable[Inx]);
    if Walker^.hsInUse then begin
      aVisitProc(Walker^.hsItem, aExtraData, StopNow);
      if StopNow then begin
        Result := Walker^.hsItem;
        Exit;
      end;
    end;
  end;
  Result := nil;
end;
{====================================================================}

end.
