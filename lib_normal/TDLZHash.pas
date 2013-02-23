(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDLZHash                                                         *)
(* Chained hash table for LZ77 compression                          *)
(********************************************************************)

unit TDLZHash;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  Classes,
  TDBasics,
  TDLZBase;

{Notes: Although a pretty standard hash table with chaining, this
        particular version has several changes to make it more
        applicable to LZ compression.
        Firstly all keys (signatures) are 3 characters long. The hash
        function is the three characters of the signature as the three
        lowest bytes of a longint, with the upper byte set to zero.
        Secondly the items in the hash table comprise the signature,
        together with the input stream offset where the string
        appears.
        Thirdly the offset is used as a least recently used indicator:
        when a signature is searched for, older nodes in the chains
        are freed automatically. Older nodes equate to lower offsets.

        All nodes in the hash table are 12 bytes long (next pointer,
        signature and offset) and so a node manager is used for
        space and speed efficiency reasons.
        }

type
  TtdLZSigEnumProc = procedure (aExtraData : pointer;
                          const aSignature : TtdLZSignature;
                                aOffset    : longint);

  PtdLZHashNode = ^TtdLZHashNode;
  TtdLZHashNode = packed record
    hnNext   : PtdLZHashNode;
    hnSig    : TtdLZSignature;
    hnOffset : longint;
  end;

  TtdLZHashTable = class
    {-a hash table for LZ77 compression}
    private
      FHashTable : TList;
      FName    : TtdNameString;
    protected
      procedure htError(aErrorCode  : integer;
                  const aMethodName : TtdNameString);
      procedure htFreeChain(aParentNode : PtdLZHashNode);
    public
      constructor Create;
      destructor Destroy; override;

      procedure Empty;
      function EnumMatches(const aSignature : TtdLZSignature;
                                 aCutOffset : longint;
                                 aAction    : TtdLZSigEnumProc;
                                 aExtraData : pointer) : boolean;
      procedure Insert(const aSignature : TtdLZSignature;
                             aOffset    : longint);

      property Name : TtdNameString
                  read FName write FName;
  end;

implementation

uses
  TDNdeMgr;

const
  UnitName = 'TDLZHash';
  LZHashTableSize = 521; {a prime}

var
  LZHashNodeManager : TtdNodeManager;

{===TtdLZHashTable===============================================}
constructor TtdLZHashTable.Create;
var
  Inx : integer;
begin
  inherited Create;
  if (LZHashNodeManager = nil) then begin
    LZHashNodeManager := TtdNodeManager.Create(sizeof(TtdLZHashNode));
    LZHashNodeManager.Name := 'LZ77 node manager';
  end;
  {create the hash table, make all elements linked lists with a dummy
   head node}
  FHashTable := TList.Create;
  FHashTable.Count := LZHashTableSize;
  for Inx := 0 to pred(LZHashTableSize) do
    FHashTable.List^[Inx] := LZHashNodeManager.AllocNodeClear;
end;
{--------}
destructor TtdLZHashTable.Destroy;
var
  Inx : integer;
begin
  {destroy the hash table completely, including dummy head nodes}
  if (FHashTable <> nil) then begin
    Empty;
    for Inx := 0 to pred(FHashTable.Count) do
      LZHashNodeManager.FreeNode(FHashTable.List^[Inx]);
    FHashTable.Free;
  end;
  inherited Destroy;
end;
{--------}
procedure TtdLZHashTable.Empty;
var
  Inx : integer;
begin
  for Inx := 0 to pred(FHashTable.Count) do
    htFreeChain(PtdLZHashNode(FHashTable.List^[Inx]));
end;
{--------}
function TtdLZHashTable.EnumMatches(const aSignature : TtdLZSignature;
                                    aCutOffset : longint;
                                    aAction    : TtdLZSigEnumProc;
                                    aExtraData : pointer) : boolean;
var
  Inx : integer;
  Temp : PtdLZHashNode;
  Dad  : PtdLZHashNode;
begin
  {if we have no action, it's an error}
  if not Assigned(aAction) then
    htError(tdeLZNoAction, 'EnumMatches');
  {assume we don't find any}
  Result := false;
  {calculate the hash table index for this signature}
  Inx := (aSignature.AsLong shr 8) mod LZHashTableSize;
  {walk the chain at this index}
  Dad := PtdLZHashNode(FHashTable.List^[Inx]);
  Temp := Dad^.hnNext;
  while (Temp <> nil) do begin
    {if this node has an offset that is less than the cutoff offset,
     then remove the rest of this chain and exit}
    if (Temp^.hnOffset < aCutOffset) then begin
      htFreeChain(Dad);
      Exit;
    end;
    {if the node's signature matches ours, call the action routine}
    if (Temp^.hnSig.AsLong = aSignature.AsLong) then begin
      Result := true;
      aAction(aExtraData, aSignature, Temp^.hnOffset);
    end;
    {advance to the next node}
    Dad := Temp;
    Temp := Dad^.hnNext;
  end;
end;
{--------}
procedure TtdLZHashTable.htError(aErrorCode  : integer;
                           const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdLZException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
procedure TtdLZHashTable.htFreeChain(aParentNode : PtdLZHashNode);
var
  Walker, Temp : PtdLZHashNode;
begin
  Walker := aParentNode^.hnNext;
  aParentNode^.hnNext := nil;
  while (Walker <> nil) do begin
    Temp := Walker;
    Walker := Walker^.hnNext;
    LZHashNodeManager.FreeNode(Temp);
  end;
end;
{--------}
procedure TtdLZHashTable.Insert(const aSignature : TtdLZSignature;
                                      aOffset    : longint);
var
  Inx      : integer;
  NewNode  : PtdLZHashNode;
  HeadNode : PtdLZHashNode;
begin
  {calculate the hash table index for this signature}
  Inx := (aSignature.AsLong shr 8) mod LZHashTableSize;
  {allocate a new node and insert at the head of the chain at this
   index in the hash table; this ensures that the nodes in the chain
   are in reverse order of offset value}
  NewNode := LZHashNodeManager.AllocNode;
  NewNode^.hnSig := aSignature;
  NewNode^.hnOffset := aOffset;
  HeadNode := PtdLZHashNode(FHashTable.List^[Inx]);
  NewNode^.hnNext := HeadNode^.hnNext;
  HeadNode^.hnNext := NewNode;
end;
{====================================================================}

procedure FinalizeUnit; far;
begin
  LZHashNodeManager.Free;
end;

initialization
  LZHashNodeManager := nil;
  {$IFDEF Delphi1}
  AddExitProc(FinalizeUnit);
  {$ENDIF}

{$IFNDEF Delphi1}
finalization
  FinalizeUnit;
{$ENDIF}

end.
