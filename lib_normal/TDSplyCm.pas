(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDSplyCm                                                         *)
(* Splay compression and decompression                              *)
(********************************************************************)

unit TDSplyCm;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  Classes,
  TDBasics;

procedure TDSplayCompress(aInStream, aOutStream : TStream);
  {Compress input stream to output stream using splay tree encoding}

procedure TDSplayDecompress(aInStream, aOutStream : TStream);
  {Decompress input stream to output stream using splay tree encoding}

implementation

uses
  TDStrms,
  TDPriQue;

const
  UnitName = 'TDSplyCm';
  TDSplayHeader = $54534454; {the header for the compressed result}
                             { = 'TDST'}
  SplayBufferSize = 4096;

type
  PSplayNode = ^TSplayNode;
  TSplayNode = packed record
    hnParentInx: longint;
    hnLeftInx  : longint;
    hnRightInx : longint;
    hnIndex    : longint;
  end;

  PSplayNodeArray = ^TSplayNodeArray;
  TSplayNodeArray = array [0..510] of TSplayNode;

type
  TSplayTree = class
    private
      FTree : TSplayNodeArray;
    protected
      procedure stConvertCodeStr(const aRevCodeStr : ShortString;
                                   var aBitString  : TtdBitString);
      procedure stInitialize;
      procedure stSplay(aNodeInx : integer);
    public
      constructor Create;
      destructor Destroy; override;

      procedure EncodeByte(aBitStream : TtdOutputBitStream;
                           aValue     : byte);
      function DecodeByte(aBitStream : TtdInputBitStream) : byte;
  end;


{===Helper routines==================================================}
procedure DoSplayCompression(aInStream  : TStream;
                             aBitStream : TtdOutputBitStream;
                             aTree      : TSplayTree);
var
  i         : integer;
  Buffer    : PByteArray;
  BytesRead : longint;
begin
  GetMem(Buffer, SplayBufferSize);
  try
    {reset the input stream to the start}
    aInStream.Position := 0;
    {read the first block from the input stream}
    BytesRead := aInStream.Read(Buffer^, SplayBufferSize);
    while (BytesRead <> 0) do begin
      {for each character in the block, write out its bit string}
      for i := 0 to pred(BytesRead) do
        aTree.EncodeByte(aBitStream, Buffer^[i]);
      {read the nextblock from the input stream}
      BytesRead := aInStream.Read(Buffer^, SplayBufferSize);
    end;
  finally
    FreeMem(Buffer, SplayBufferSize);
  end;
end;
{--------}
procedure DoSplayDecompression(aBitStream : TtdInputBitStream;
                               aOutStream : TStream;
                               aTree      : TSplayTree;
                               aSize      : longint);
var
  CharCount : longint;
  Buffer    : PByteArray;
  BufEnd    : integer;
begin
  GetMem(Buffer, SplayBufferSize);
  try
    {preset the loop variables}
    BufEnd := 0;
    CharCount := 0;
    {repeat until all the characters have been decompressed}
    while (CharCount < aSize) do begin
      {read the next byte}
      Buffer^[BufEnd] := aTree.DecodeByte(aBitStream);
      inc(BufEnd);
      inc(CharCount);
      {if we've filled the buffer, write it out}
      if (BufEnd = SplayBufferSize) then begin
        aOutStream.WriteBuffer(Buffer^, SplayBufferSize);
        BufEnd := 0;
      end;
    end;
    {if there's anything left in the buffer, write it out}
    if (BufEnd <> 0) then
      aOutStream.WriteBuffer(Buffer^, BufEnd);
  finally
    FreeMem(Buffer, SplayBufferSize);
  end;
end;
{====================================================================}


{===TSplayTree=====================================================}
constructor TSplayTree.Create;
begin
  inherited Create;
  stInitialize;
end;
{--------}
destructor TSplayTree.Destroy;
begin
  inherited Destroy;
end;
{--------}
procedure TSplayTree.EncodeByte(aBitStream : TtdOutputBitStream;
                                aValue     : byte);
var
  NodeInx    : integer;
  ParentInx  : integer;
  RevCodeStr : ShortString;
  BitString  : TtdBitString;
begin
  {starting at the node for aValue, work our way up the tree, saving
   whether we moved up a left link (0) or a right link (1) at every
   step}
  RevCodeStr := '';
  NodeInx := aValue + 255;
  while (NodeInx <> 0) do begin
    ParentInx := FTree[NodeInx].hnParentInx;
    inc(RevCodeStr[0]);
    if (FTree[ParentInx].hnLeftInx = NodeInx) then
      RevCodeStr[length(RevCodeStr)] := '0'
    else
      RevCodeStr[length(RevCodeStr)] := '1';
    NodeInx := ParentInx;
  end;
  {convert the string code into a bit string}
  stConvertCodeStr(RevCodeStr, BitString);
  {write the bit string to the bit stream}
  aBitStream.WriteBits(BitString);
  {now splay the node}
  stSplay(aValue + 255);
end;
{--------}
function TSplayTree.DecodeByte(aBitStream : TtdInputBitStream) : byte;
var
  NodeInx : integer;
begin
  {starting at the root, walk down the tree as dictated by the bits
   from the bit stream}
  NodeInx := 0;
  while NodeInx < 255 do begin
    if not aBitStream.ReadBit then
      NodeInx := FTree[NodeInx].hnLeftInx
    else
      NodeInx := FTree[NodeInx].hnRightInx;
  end;
  {calculate the byte from the final node index}
  Result := NodeInx - 255;
  {now splay the node}
  stSplay(NodeInx);
end;
{--------}
procedure TSplayTree.stConvertCodeStr(const aRevCodeStr : ShortString;
                                      var aBitString  : TtdBitString);
var
  ByteNum : integer;
  i       : integer;
  Mask    : byte;
  Accum   : byte;
begin
  {prepare for the conversion loop}
  ByteNum := 0;
  Mask := 1;
  Accum := 0;
  {convert the bits in reverse}
  for i := length(aRevCodeStr) downto 1 do begin
    if (aRevCodeStr[i] = '1') then
      Accum := Accum or Mask;
    Mask := Mask shl 1;
    if (Mask = 0) then begin
      aBitString.bsBits[ByteNum] := Accum;
      inc(ByteNum);
      Mask := 1;
      Accum := 0;
    end;
  end;
  {if there are some bits left over, store them}
  if (Mask <> 1) then
    aBitString.bsBits[ByteNum] := Accum;
  {store binary code in the codes array}
  aBitString.bsCount := length(aRevCodeStr);
end;
{--------}
procedure TSplayTree.stInitialize;
var
  i : integer;
begin
  {create a perfectly balanced tree, the root will be at element 0;
   for node N, its parent is at (N-1) div 2 and its children are at
   2N+1 and 2N+2}
  FillChar(FTree, sizeof(FTree), 0);
  for i := 0 to 254 do begin
    FTree[i].hnLeftInx := (2 * i) + 1;
    FTree[i].hnRightInx := (2 * i) + 2;
  end;
  for i := 1 to 510 do
    FTree[i].hnParentInx := (i - 1) div 2;
end;
{--------}
procedure TSplayTree.stSplay(aNodeInx : integer);
var
  Dad      : integer;
  GrandDad : integer;
  Uncle    : integer;
begin
  {splay the node}
  repeat
    {get the parent of the node}
    Dad := FTree[aNodeInx].hnParentInx;
    {if the parent is the root, we're done}
    if (Dad = 0) then
      aNodeInx := 0
    {otherwise, semi-rotate the node up the tree}
    else begin
      {get the parent of the parent}
      GrandDad := FTree[Dad].hnParentInx;
      {semi-rotate (ie, swap the node with its uncle)}
      if (FTree[GrandDad].hnLeftInx = Dad) then begin
        Uncle := FTree[GrandDad].hnRightInx;
        FTree[GrandDad].hnRightInx := aNodeInx;
      end
      else begin
        Uncle := FTree[GrandDad].hnLeftInx;
        FTree[GrandDad].hnLeftInx := aNodeInx;
      end;
      if (FTree[Dad].hnLeftInx = aNodeInx) then
        FTree[Dad].hnLeftInx := Uncle
      else
        FTree[Dad].hnRightInx := Uncle;
      FTree[Uncle].hnParentInx := Dad;
      FTree[aNodeInx].hnParentInx := GrandDad;
      {start again at the grandparent}
      aNodeInx := GrandDad;
    end;
  until (aNodeInx = 0);
end;
{====================================================================}


{===Interfaced routines==============================================}
procedure TDSplayCompress(aInStream, aOutStream : TStream);
var
  STree     : TSplayTree;
  BitStrm   : TtdOutputBitStream;
  Signature : longint;
  Size      : longint;
begin
  {output the header information (the signature and the size of the
   uncompressed data)}
  Signature := TDSplayHeader;
  aOutStream.WriteBuffer(Signature, sizeof(longint));
  Size := aInStream.Size;
  aOutStream.WriteBuffer(Size, sizeof(longint));
  {if there's nothing to compress, exit now}
  if (Size = 0) then
    Exit;
  {prepare}
  STree := nil;
  BitStrm := nil;
  try
    {create the compressed bit stream}
    BitStrm := TtdOutputBitStream.Create(aOutStream);
    BitStrm.Name := 'Splay compressed stream';
    {create the Splay tree}
    STree := TSplayTree.Create;
    {compress the characters in the input stream to the bit stream}
    DoSplayCompression(aInStream, BitStrm, STree);
  finally
    BitStrm.Free;
    STree.Free;
  end;
end;
{--------}
procedure TDSplayDecompress(aInStream, aOutStream : TStream);
var
  Signature : longint;
  Size      : longint;
  STree     : TSplayTree;
  BitStrm   : TtdInputBitStream;
begin
  {make sure that the input stream is a valid Splay encoded stream}
  aInStream.Seek(0, soFromBeginning);
  aInStream.ReadBuffer(Signature, sizeof(Signature));
  if (Signature <> TDSplayHeader) then
    raise EtdSplayException.Create(
       FmtLoadStr(tdeSplyBadEncodedStrm,
                 [UnitName, 'TDSplayDecompress']));
  aInStream.ReadBuffer(Size, sizeof(longint));
  {if there's nothing to decompress, exit now}
  if (Size = 0) then
    Exit;
  {prepare for the decompression}
  STree := nil;
  BitStrm := nil;
  try
    {create the bit stream}
    BitStrm := TtdInputBitStream.Create(aInStream);
    BitStrm.Name := 'Splay compressed stream';
    {create the Splay tree}
    STree := TSplayTree.Create;
    {using the Splay tree, decompress the characters in the input
     stream}
    DoSplayDecompression(BitStrm, aOutStream, STree, Size);
  finally
    BitStrm.Free;
    STree.Free;
  end;
end;
{====================================================================}

end.
