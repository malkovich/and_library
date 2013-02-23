(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDHshExt                                                         *)
(* Extendible hashing for disk-based hash table                     *)
(********************************************************************)

unit TDHshExt;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  Classes,
  TDBasics,
  TDRecFil,
  TDHshBse;

const
  tdcBucketItemCount = 64;
  tdcBucketSize = (tdcBucketItemCount + 1) * 2 * sizeof(longint);

type
  TtdCompareRecordKey = function (var aRecord;
                                const aKey : string) : boolean;
type
  TtdHashDirectory = class
    private
      FCount  : integer;
      FDepth  : integer;
      FList   : TList;
      FName   : TtdNameString;
      FStream : TStream;
    protected
      function hdGetItem(aInx : integer) : longint;
      procedure hdSetItem(aInx : integer; aValue : longint);

      function hdErrorMsg(aErrorCode  : integer;
                    const aMethodName : TtdNameString;
                          aIndex      : integer) : string;

      procedure hdLoadFromStream;
      procedure hdStoreToStream;
    public
      constructor Create(aStream : TStream);
      destructor Destroy; override;

      procedure DoubleCount;

      property Count : integer read FCount;
      property Depth : integer read FDepth;
      property Items[aInx : integer] : longint
                  read hdGetItem write hdSetItem; default;
      property Name : TtdNameString
         read FName write FName;
  end;

type
  TtdHashTableExtendible = class
    {-a hash table that uses extendible hashing}
    private
      FCompare  : TtdCompareRecordKey;
      FCount    : longint;
      FDirectory: TtdHashDirectory;
      FHashFunc : TtdHashFuncEx;
      FName     : TtdNameString;
      FBuckets  : TtdRecordStream;
      FRecords  : TtdRecordStream;
      FRecord   : pointer;
    protected
      procedure hteCreateNewHashTable;
      procedure hteError(aErrorCode : integer;
                   const aMethodName : TtdNameString);
      function hteErrorMsg(aErrorCode : integer;
                     const aMethodName : TtdNameString) : string;
      function hteFindBucket(const aKey : string;
                               var aFindInfo) : boolean;
      procedure hteSplitBucket(var aFindInfo);
    public
      constructor Create(aHashFunc     : TtdHashFuncEx;
                         aCompare      : TtdCompareRecordKey;
                         aDirStream    : TStream;
                         aBucketStream : TtdRecordStream;
                         aRecordStream : TtdRecordStream);
      destructor Destroy; override;

      function Find(const aKey   : string;
                      var aRecord) : boolean;
      procedure Insert(const aKey : string; var aRecord);

      property Count : longint
         read FCount;
      property Name : TtdNameString
         read FName write FName;
  end;

implementation

const
  UnitName = 'TDHshExt';

type
  PLongintArray = ^TLongintArray;
  TLongintArray =
     array [0..pred(MaxInt div sizeof(longint))] of longint;

  THashElement = packed record
    heHash : longint;
    heItem : longint;
  end;

  PBucket = ^TBucket;
  TBucket = packed record
    bkDepth  : longint;
    bkCount  : longint;
    bkHashes : array [0..pred(tdcBucketItemCount)] of THashElement;
  end;

  PFindItemInfo = ^TFindItemInfo;
  TFindItemInfo = packed record
    fiiHash      : longint;      {hash of key parameter}
    fiiDirEntry  : integer;      {directory entry}
    fiiSlot      : integer;      {slot in bucket}
    fiiBucketNum : longint;      {bucket number in stream}
    fiiBucket    : TBucket;      {bucket}
  end;


{===TtdHashDirectory=================================================}
constructor TtdHashDirectory.Create(aStream : TStream);
begin
  Assert(sizeof(pointer) = sizeof(longint),
         hdErrorMsg(tdePointerLongSize, 'Create', 0));
  Assert(aStream <> nil,
         hdErrorMsg(tdeHashTblNoDir, 'Create', 0));

  {create the ancestor}
  inherited Create;
  {create the directory as a TList}
  FList := TList.Create;
  FStream := aStream;
  {if there's nothing in the stream, initialize the directory to
   have one entry and be of depth 0}
  if (FStream.Size = 0) then begin
    FList.Count := 1;
    FCount := 1;
    FDepth := 0;
  end
  {otherwise load from the stream}
  else
    hdLoadFromStream;
end;
{--------}
destructor TtdHashDirectory.Destroy;
begin
  hdStoreToStream;
  FList.Free;
  inherited Destroy;
end;
{--------}
procedure TtdHashDirectory.DoubleCount;
var
  Inx : integer;
begin
  {double the count, increment the depth}
  FList.Count := FList.Count * 2;
  FCount := FCount * 2;
  inc(FDepth);
  {each entry in the original directory is now doubled up in the new
   one; for example, the value in the old dir entry 0 is now the value
   for the new dir entries 0 and 1}
  for Inx := pred(FList.Count) downto 1 do
    FList.List^[Inx] := FList.List^[Inx div 2];
end;
{--------}
function TtdHashDirectory.hdErrorMsg(aErrorCode  : integer;
                               const aMethodName : TtdNameString;
                                     aIndex      : integer) : string;
begin
  if (Name = '') then
    Name := '-unnamed-';
  Result := FmtLoadStr(aErrorCode,
                    [UnitName, ClassName, aMethodName, Name, aIndex]);
end;
{--------}
function TtdHashDirectory.hdGetItem(aInx : integer) : longint;
begin
  Assert((0 <= aInx) and (aInx < FList.Count),
         hdErrorMsg(tdeIndexOutOfBounds, 'hdGetItem', aInx));
  Result := longint(FList.List^[aInx]);
end;
{--------}
procedure TtdHashDirectory.hdLoadFromStream;
begin
  FStream.Seek(0, soFromBeginning);
  FStream.ReadBuffer(FDepth, sizeof(FDepth));
  FStream.ReadBuffer(FCount, sizeof(FCount));
  FList.Count := FCount;
  FStream.ReadBuffer(FList.List^, FCount * sizeof(longint));
end;
{--------}
procedure TtdHashDirectory.hdSetItem(aInx : integer; aValue : longint);
begin
  Assert((0 <= aInx) and (aInx < FList.Count),
         hdErrorMsg(tdeIndexOutOfBounds, 'hdGetItem', aInx));
  FList.List^[aInx] := pointer(aValue);
end;
{--------}
procedure TtdHashDirectory.hdStoreToStream;
begin
  FStream.Seek(0, soFromBeginning);
  FStream.WriteBuffer(FDepth, sizeof(FDepth));
  FStream.WriteBuffer(FCount, sizeof(FCount));
  FStream.WriteBuffer(FList.List^, FCount * sizeof(longint));
end;
{====================================================================}


{===Helper routines==================================================}
function ReverseBits(aValue : longint; aBitCount : integer) : longint;
var
  i : integer;
begin
  Result := 0;
  for i := 0 to pred(aBitCount) do begin
    Result := (Result shl 1) or (aValue and 1);
    aValue := aValue shr 1;
  end;
end;
{====================================================================}


{===TtdHashTableExtendible===========================================}
constructor TtdHashTableExtendible.Create(
                                  aHashFunc     : TtdHashFuncEx;
                                  aCompare      : TtdCompareRecordKey;
                                  aDirStream    : TStream;
                                  aBucketStream : TtdRecordStream;
                                  aRecordStream : TtdRecordStream);
begin
  Assert(Assigned(aHashFunc),
         hteErrorMsg(tdeHashTblNoHashFunc, 'Create'));
  Assert(Assigned(aCompare),
         hteErrorMsg(tdeHashTblNoCompare, 'Create'));
  Assert(Assigned(aDirStream),
         hteErrorMsg(tdeHashTblNoDir, 'Create'));
  Assert(Assigned(aBucketStream),
         hteErrorMsg(tdeHashTblNoBuckets, 'Create'));
  Assert(Assigned(aRecordStream),
         hteErrorMsg(tdeHashTblNoRecords, 'Create'));

  {create the ancestor}
  inherited Create;

  {create the directory}
  FDirectory := TtdHashDirectory.Create(aDirStream);

  {save parameters}
  FHashFunc := aHashFunc;
  FCompare := aCompare;
  FBuckets := aBucketStream;
  FRecords := aRecordStream;

  {get a buffer for any records we have to read}
  GetMem(FRecord, FRecords.RecordLength);

  {if the bucket stream is empty, create the first bucket}
  if (FBuckets.Count = 0) then
    hteCreateNewHashTable;
end;
{--------}
destructor TtdHashTableExtendible.Destroy;
begin
  FDirectory.Free;
  if (FRecord <> nil) then
    FreeMem(FRecord, FRecords.RecordLength);
  inherited Destroy;
end;
{--------}
function TtdHashTableExtendible.Find(const aKey : string;
                                       var aRecord) : boolean;
var
  FindInfo  : TFindItemInfo;
begin
  if hteFindBucket(aKey, FindInfo) then begin
    Result := true;
    Move(FRecord^, aRecord, FRecords.RecordLength);
  end
  else
    Result := false;
end;
{--------}
procedure TtdHashTableExtendible.hteCreateNewHashTable;
var
  NewBucket : TBucket;
begin
  FillChar(NewBucket, sizeof(NewBucket), 0);
  FDirectory[0] := FBuckets.Add(NewBucket);
end;
{--------}
procedure TtdHashTableExtendible.hteError(aErrorCode  : integer;
                                const aMethodName : TtdNameString);
begin
  raise EtdHashTableException.Create(
           hteErrorMsg(aErrorCode, aMethodName));
end;
{--------}
function TtdHashTableExtendible.hteErrorMsg(
                                aErrorCode : integer;
                          const aMethodName : TtdNameString) : string;
begin
  if (Name = '') then
    Name := '-unnamed-';
  Result := FmtLoadStr(aErrorCode,
                       [UnitName, ClassName, aMethodName, Name]);
end;
{--------}
function TtdHashTableExtendible.hteFindBucket(
                                           const aKey : string;
                                             var aFindInfo) : boolean;
var
  FindInfo  : PFindItemInfo;
  Inx       : integer;
  IsDeleted : boolean;
begin
  FindInfo := PFindItemInfo(@aFindInfo);
  with FindInfo^ do begin
    {calculate the hash for the string}
    fiiHash := FHashFunc(aKey);
    {calculate the entry in the directory for this hash, which gives
     us the bucket number}
    fiiDirEntry := ReverseBits(fiiHash, FDirectory.Depth);
    fiiBucketNum := FDirectory[fiiDirEntry];
    {retrieve the bucket}
    FBuckets.Read(fiiBucketNum, fiiBucket, IsDeleted);
    if IsDeleted then
      hteError(tdeHashTblDeletedBkt, 'hteFindBucket');
    {search for the hash value in the bucket, assume we won't succeed}
    Result := false;
    with fiiBucket do begin
      for Inx := 0 to pred(bkCount) do begin
        {if the hash matches...}
        if (bkHashes[Inx].heHash = fiiHash) then begin
          {read the record}
          FRecords.Read(bkHashes[Inx].heItem, FRecord^, IsDeleted);
          if IsDeleted then
            hteError(tdeHashTblDeletedRec, 'hteFindBucket');
          {compare the record to the key}
          if FCompare(FRecord^, aKey) then begin
            Result := true;
            fiiSlot := Inx;
            Exit;
          end;
        end;
      end;
    end;
  end;
end;
{--------}
procedure TtdHashTableExtendible.hteSplitBucket(var aFindInfo);
var
  FindInfo  : PFindItemInfo;
  Inx       : integer;
  NewBucket : TBucket;
  NewBucketNum : longint;
  Mask      : longint;
  OldValue  : longint;
  OldInx    : integer;
  NewInx    : integer;
  StartDirEntry : longint;
  NewStartDirEntry : longint;
  EndDirEntry   : longint;
  Test : integer;
begin
  FindInfo := PFindItemInfo(@aFindInfo);

  {if the bucket we are splitting has the same bit depth as the
   directory, then we need to double the capacity of the directory}
  if (FindInfo^.fiiBucket.bkDepth = FDirectory.Depth) then begin
    FDirectory.DoubleCount;
    {update the directory entry for the bucket we're splitting}
    FindInfo^.fiiDirEntry := FindInfo^.fiiDirEntry * 2;
  end;

  {calculate the range of directory entries pointing to the original
   bucket, and the range for the new}
  StartDirEntry := FindInfo^.fiiDirEntry;
  while (StartDirEntry >= 0) and
        (FDirectory[StartDirEntry] = FindInfo^.fiiBucketNum) do
    dec(StartDirEntry);
  inc(StartDirEntry);
  EndDirEntry := FindInfo^.fiiDirEntry;
  while (EndDirEntry < FDirectory.Count) and
        (FDirectory[EndDirEntry] = FindInfo^.fiiBucketNum) do
    inc(EndDirEntry);
  dec(EndDirEntry);
  NewStartDirEntry := (StartDirEntry + EndDirEntry + 1) div 2;

  {increase the bit depth of the bucket being split}
  inc(FindInfo^.fiiBucket.bkDepth);
  {initialize the new bucket; it will have the same bucket depth as
   the bucket we're splitting}
  FillChar(NewBucket, sizeof(NewBucket), 0);
  NewBucket.bkDepth := FindInfo^.fiiBucket.bkDepth;

  {calculate the AND mask we'll use to identify where hash entries go}
  Mask := (1 shl NewBucket.bkDepth) - 1;
  {calculate the ANDed value for hash entries for the old bucket}
  OldValue := ReverseBits(StartDirEntry, FDirectory.Depth) and Mask;
  {read through the old bucket and transfer hashes that belong to the
   new bucket over to it}
  OldInx := 0;
  NewInx := 0;
  with FindInfo^.fiiBucket do
    for Inx := 0 to pred(bkCount) do begin
      if (bkHashes[Inx].heHash and Mask) = OldValue then begin
        bkHashes[OldInx] := bkHashes[Inx];
        inc(OldInx);
      end
      else begin
        NewBucket.bkHashes[NewInx] := bkHashes[Inx];
        inc(NewInx);
      end;
    end;

  {set the counts for both buckets}
  FindInfo^.fiiBucket.bkCount := OldInx;
  NewBucket.bkCount := NewInx;

  {add the new bucket to the bucket stream, update the old bucket}
  NewBucketNum := FBuckets.Add(NewBucket);
  FBuckets.Write(FindInfo^.fiiBucketNum, FindInfo^.fiiBucket);

  {set all the entries in the new directory range to the new bucket}
  for Inx := NewStartDirEntry to EndDirEntry do
    FDirectory[Inx] := NewBucketNum;

  {$IFDEF DebugMode}
  {verify all hashes to be in correct place}
  with FindInfo^.fiiBucket do
    for Inx := 0 to pred(bkCount) do begin
      Test := ReverseBits(bkHashes[Inx].heHash, FDirectory.Depth);
      if not ((StartDirEntry <= Test) and
              (Test < NewStartDirEntry)) then begin
        writeln('error');
        readln;
      end;
    end;
  with NewBucket do
    for Inx := 0 to pred(bkCount) do begin
      Test := ReverseBits(bkHashes[Inx].heHash, FDirectory.Depth);
      if not ((NewStartDirEntry <= Test) and
              (Test <= EndDirEntry)) then begin
        writeln('error');
        readln;
      end;
    end;
  {$ENDIF}
end;
{--------}
procedure TtdHashTableExtendible.Insert(const aKey : string;
                                          var aRecord);
var
  FindInfo  : TFindItemInfo;
  RRN       : longint;
begin
  if hteFindBucket(aKey, FindInfo) then
    hteError(tdeHashTblKeyExists, 'Insert');
  {check to see if there's enough room in this bucket, if not we'll
   split the bucket, and re-find where to insert the item; continue
   until the bucket found has enough room}
  while (FindInfo.fiiBucket.bkCount >= tdcBucketItemCount) do begin
    hteSplitBucket(FindInfo);
    if hteFindBucket(aKey, FindInfo) then
      hteError(tdeHashTblKeyExists, 'Insert');
  end;
  {add the record to the record stream to get the record number}
  RRN := FRecords.Add(aRecord);
  {add the hash to the end of the hash list, update the bucket}
  with FindInfo, FindInfo.fiiBucket do begin
    bkHashes[bkCount].heHash := fiiHash;
    bkHashes[bkCount].heItem := RRN;
    inc(bkCount);
    FBuckets.Write(fiiBucketNum, fiiBucket);
  end;
  {we have one more record}
  inc(FCount);
end;
{====================================================================}

end.
