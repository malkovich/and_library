(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDHuffmn                                                         *)
(* Huffman compression and decompression                            *)
(********************************************************************)

unit TDHuffmn;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  Classes,
  TDBasics;

procedure TDHuffmanCompress(aInStream, aOutStream : TStream);
  {Compress input stream to output stream using Huffman encoding}

procedure TDHuffmanDecompress(aInStream, aOutStream : TStream);
  {Decompress input stream to output stream using Huffman encoding}

implementation

uses
  TDStrms,
  TDPriQue;

const
  UnitName = 'TDHuffmn';
  TDHuffHeader = $46484454; {the header for the compressed result}
                            { = 'TDHF'}
  HuffmanBufferSize = 4096;

type
  PHuffmanNode = ^THuffmanNode;
  THuffmanNode = packed record
    hnCount    : longint;
    hnLeftInx  : longint;
    hnRightInx : longint;
    hnIndex    : longint;
  end;

  PHuffmanNodeArray = ^THuffmanNodeArray;
  THuffmanNodeArray = array [0..510] of THuffmanNode;

type
  THuffmanCodeStr = string[255];

type
  PHuffmanCodes = ^THuffmanCodes;
  THuffmanCodes = array [0..255] of TtdBitString;

type
  THuffmanTree = class
    private
      FTree : THuffmanNodeArray;
      FRoot : integer;
    protected
      procedure htBuild;
      procedure htCalcCodesPrim(aNodeInx : integer;
                            var aCodeStr : THuffmanCodeStr;
                            var aCodes   : THuffmanCodes);
      function htLoadNode(aBitStream : TtdInputBitStream) : integer;
      procedure htSaveNode(aBitStream : TtdOutputBitStream;
                           aNode : integer);
    public
      constructor Create;

      procedure CalcCharDistribution(aStream : TStream);
      procedure CalcCodes(var aCodes : THuffmanCodes);
      function DecodeNextByte(aBitStream : TtdInputBitStream) : byte;
      procedure LoadFromBitStream(aBitStream : TtdInputBitStream);
      function RootIsLeaf : boolean;
      procedure SaveToBitStream(aBitStream : TtdOutputBitStream);

      property Root : integer read FRoot;
  end;


{===Helper routines==================================================}
function CompareHuffmanNodes(aData1, aData2 : pointer) : integer; far;
var
  Node1 : PHuffmanNode absolute aData1;
  Node2 : PHuffmanNode absolute aData2;
begin
  {NOTE: this comparison routine is for the Huffman priority queue,
         which is a *min heap*, therefore this comparison routine
         should return the reverse of what we expect}
  if (Node1^.hnCount) > (Node2^.hnCount) then
    Result := -1
  else if (Node1^.hnCount) = (Node2^.hnCount) then
    Result := 0
  else
    Result := 1;
end;
{--------}
procedure ConvertCodeStr(const aCodeStr : THuffmanCodeStr;
                           var aCode    : TtdBitString);
{-convert a code string into binary; store in codes array}
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
  {fill the bits from the left in the binary code}
  for i := 1 to length(aCodeStr) do begin
    if (aCodeStr[i] = '1') then
      Accum := Accum or Mask;
    Mask := Mask shl 1;
    if (Mask = 0) then begin
      aCode.bsBits[ByteNum] := Accum;
      inc(ByteNum);
      Mask := 1;
      Accum := 0;
    end;
  end;
  {if there are some bits left over, store them}
  if (Mask <> 1) then
    aCode.bsBits[ByteNum] := Accum;
  {store binary code in the codes array}
  aCode.bsCount := length(aCodeStr);
end;
{--------}
procedure DoHuffmanCompression(aInStream  : TStream;
                               aBitStream : TtdOutputBitStream;
                           var aCodes     : THuffmanCodes);
var
  i         : integer;
  Buffer    : PByteArray;
  BytesRead : longint;
begin
  GetMem(Buffer, HuffmanBufferSize);
  try
    {reset the input stream to the start}
    aInStream.Position := 0;
    {read the first block from the input stream}
    BytesRead := aInStream.Read(Buffer^, HuffmanBufferSize);
    while (BytesRead <> 0) do begin
      {for each character in the block, write out its bit string}
      for i := 0 to pred(BytesRead) do
        aBitStream.WriteBits(aCodes[Buffer^[i]]);
      {read the nextblock from the input stream}
      BytesRead := aInStream.Read(Buffer^, HuffmanBufferSize);
    end;
  finally
    FreeMem(Buffer, HuffmanBufferSize);
  end;
end;
{--------}
procedure DoHuffmanDecompression(aBitStream : TtdInputBitStream;
                                 aOutStream : TStream;
                                 aHTree     : THuffmanTree;
                                 aSize      : longint);
var
  CharCount : longint;
  Ch        : byte;
  Buffer    : PByteArray;
  BufEnd    : integer;
begin
  GetMem(Buffer, HuffmanBufferSize);
  try
    {preset the loop variables}
    BufEnd := 0;
    CharCount := 0;
    {repeat until all the characters have been decompressed}
    while (CharCount < aSize) do begin
      {read the next byte}
      Ch := aHTree.DecodeNextByte(aBitStream);
      Buffer^[BufEnd] := Ch;
      inc(BufEnd);
      inc(CharCount);
      {if we've filled the buffer, write it out}
      if (BufEnd = HuffmanBufferSize) then begin
        aOutStream.WriteBuffer(Buffer^, HuffmanBufferSize);
        BufEnd := 0;
      end;
    end;
    {if there's anything left in the buffer, write it out}
    if (BufEnd <> 0) then
      aOutStream.WriteBuffer(Buffer^, BufEnd);
  finally
    FreeMem(Buffer, HuffmanBufferSize);
  end;
end;
{--------}
procedure WriteMultipleChars(aStream : TStream;
                             aCh     : AnsiChar;
                             aCount  : longint);
var
  Buffer       : PByteArray;
  BytesToWrite : integer;
begin
  GetMem(Buffer, HuffmanBufferSize);
  try
    FillChar(Buffer^, HuffmanBufferSize, aCh);
    while (aCount > 0) do begin
      if (aCount < HuffmanBufferSize) then
        BytesToWrite := aCount
      else
        BytesToWrite := HuffmanBufferSize;
      aStream.WriteBuffer(Buffer^, BytesToWrite);
      dec(aCount, BytesToWrite);
    end;
  finally
    FreeMem(Buffer, HuffmanBufferSize);
  end;
end;
{====================================================================}


{===THuffmanTree=====================================================}
constructor THuffmanTree.Create;
var
  i : integer;
begin
  inherited Create;
  FillChar(FTree, sizeof(FTree), 0);
  for i := 0 to 510 do
    FTree[i].hnIndex := i;
end;
{--------}
procedure THuffmanTree.CalcCharDistribution(aStream : TStream);
var
  i         : integer;
  Buffer    : PByteArray;
  BytesRead : integer;
begin
  {starting at the beginning of the stream, read all the bytes,
   maintain counts for each byte value}
  aStream.Position := 0;
  GetMem(Buffer, HuffmanBufferSize);
  try
    BytesRead := aStream.Read(Buffer^, HuffmanBufferSize);
    while (BytesRead <> 0) do begin
      for i := pred(BytesRead) downto 0 do
        inc(FTree[Buffer^[i]].hnCount);
      BytesRead := aStream.Read(Buffer^, HuffmanBufferSize);
    end;
  finally
    FreeMem(Buffer, HuffmanBufferSize);
  end;
  {now build the tree}
  htBuild;
end;
{--------}
procedure THuffmanTree.CalcCodes(var aCodes : THuffmanCodes);
var
  CodeStr : THuffmanCodeStr;
begin
  {clear the codes array}
  FillChar(aCodes, sizeof(aCodes), 0);
  {to calculate the codes we have to visit every leaf and for each
   leaf we'll have accumulated a series of bits (going left from a
   parent node to a child node is a 0 bit, going right is a 1 bit); we
   use an inorder recursive routine}
  CodeStr := '';
  htCalcCodesPrim(FRoot, CodeStr, aCodes);
end;
{--------}
function THuffmanTree.DecodeNextByte(aBitStream : TtdInputBitStream)
                                                               : byte;
var
  NodeInx : integer;
begin
  NodeInx := FRoot;
  while (NodeInx >= 256) do begin
    if not aBitStream.ReadBit then
      NodeInx := FTree[NodeInx].hnLeftInx
    else
      NodeInx := FTree[NodeInx].hnRightInx;
  end;
  Result := NodeInx;
end;
{--------}
procedure THuffmanTree.htBuild;
var
  i        : integer;
  PQ       : TtdPriorityQueue;
  Node1    : PHuffmanNode;
  Node2    : PHuffmanNode;
  RootNode : PHuffmanNode;
begin
  {create a priority queue}
  PQ := TtdPriorityQueue.Create(CompareHuffmanNodes, nil);
  try
    PQ.Name := 'Huffman tree minheap'; 
    {add all the non-zero nodes to the queue}
    for i := 0 to 255 do
      if (FTree[i].hnCount <> 0) then
        PQ.Enqueue(@FTree[i]);
    {SPECIAL CASE: there is only one non-zero node, ie the input
     stream consisted of just one character, repeated one or more
     times; set the root to the index of the single character node}
    if (PQ.Count = 1) then begin
      RootNode := PQ.Dequeue;
      FRoot := RootNode^.hnIndex;
    end
    {otherwise we have the normal, many different chars, case}
    else begin
      {while there is more than one item in the queue, remove the two
       smallest, join them to a new parent, and add the parent to the
       queue}
      FRoot := 255;
      while (PQ.Count > 1) do begin
        Node1 := PQ.Dequeue;
        Node2 := PQ.Dequeue;
        inc(FRoot);
        RootNode := @FTree[FRoot];
        with RootNode^ do begin
          hnLeftInx := Node1^.hnIndex;
          hnRightInx := Node2^.hnIndex;
          hnCount := Node1^.hnCount + Node2^.hnCount;
        end;
        PQ.Enqueue(RootNode);
      end;
    end;
  finally
    PQ.Free;
  end;
end;
{--------}
procedure THuffmanTree.htCalcCodesPrim(aNodeInx : integer;
                                   var aCodeStr : THuffmanCodeStr;
                                   var aCodes   : THuffmanCodes);
begin
  {if the current node is *not* a leaf, then visit the left subtree
   followed by the right subtree}
  if (aNodeInx >= 256) then begin
    {add a 0 bit on the end of the code string}
    inc(aCodeStr[0]);
    aCodeStr[length(aCodeStr)] := '0';
    {visit the left subtree}
    htCalcCodesPrim(FTree[aNodeInx].hnLeftInx, aCodeStr, aCodes);
    {add a 1 bit on the end of the code string}
    aCodeStr[length(aCodeStr)] := '1';
    {visit the right subtree}
    htCalcCodesPrim(FTree[aNodeInx].hnRightInx, aCodeStr, aCodes);
    dec(aCodeStr[0]);
  end
  {otherwise the current node is a leaf, so record the current code in
   the codes array}
  else begin
    ConvertCodeStr(aCodeStr, aCodes[aNodeInx]);
  end;
end;
{--------}
function THuffmanTree.htLoadNode(aBitStream : TtdInputBitStream)
                                                            : integer;
var
  ThisNode : integer;
begin
  {if the next bit is true, this is a leaf node, so get the character}
  if aBitStream.ReadBit then
    Result := aBitStream.ReadByte
  {otherwise, this is an internal node, so get the two child trees}
  else begin
    inc(FRoot);
    ThisNode := FRoot;
    FTree[ThisNode].hnLeftInx := htLoadNode(aBitStream);
    FTree[ThisNode].hnRightInx := htLoadNode(aBitStream);
    Result := ThisNode;
  end;
end;
{--------}
procedure THuffmanTree.htSaveNode(aBitStream : TtdOutputBitStream;
                                  aNode : integer);
begin
  {if this is an internal node, write a clear bit, and then the left
   child tree followed by the right child tree}
  if (aNode >= 256) then begin
    aBitStream.WriteBit(false);
    htSaveNode(aBitStream, FTree[aNode].hnLeftInx);
    htSaveNode(aBitStream, FTree[aNode].hnRightInx);
  end
  {otherwise it is a leaf, write a set bit, and the character}
  else begin
    aBitStream.WriteBit(true);
    aBitStream.WriteByte(aNode); {aNode equals the char value}
  end;
end;
{--------}
procedure THuffmanTree.LoadFromBitStream(aBitStream : TtdInputBitStream);
begin
  FRoot := 255; {the first parent node will be one more than this}
  FRoot := htLoadNode(aBitStream);
end;
{--------}
function THuffmanTree.RootIsLeaf : boolean;
begin
  Result := FRoot < 256;
end;
{--------}
procedure THuffmanTree.SaveToBitStream(aBitStream : TtdOutputBitStream);
begin
  htSaveNode(aBitStream, FRoot);
end;
{====================================================================}


{===Interfaced routines==============================================}
procedure TDHuffmanCompress(aInStream, aOutStream : TStream);
var
  HTree     : THuffmanTree;
  HCodes    : PHuffmanCodes;
  BitStrm   : TtdOutputBitStream;
  Signature : longint;
  Size      : longint;
begin
  {output the header information (the signature and the size of the
   uncompressed data)}
  Signature := TDHuffHeader;
  aOutStream.WriteBuffer(Signature, sizeof(longint));
  Size := aInStream.Size;
  aOutStream.WriteBuffer(Size, sizeof(longint));
  {if there's nothing to compress, exit now}
  if (Size = 0) then
    Exit;
  {prepare}
  HTree := nil;
  HCodes := nil;
  BitStrm := nil;
  try
    {create the compressed bit stream}
    BitStrm := TtdOutputBitStream.Create(aOutStream);
    BitStrm.Name := 'Huffman compressed stream';
    {allocate the Huffman tree}
    HTree := THuffmanTree.Create;
    {get the distribution of characters in the input stream, and build
    the Huffman tree from the bottom up}
    HTree.CalcCharDistribution(aInStream);
    {output the tree to the bit stream to aid the decompressor}
    HTree.SaveToBitStream(BitStrm);
    {if the root of the Huffman tree is a leaf, the input stream just
     consisted of repetitions of one character, so we're done; other-
     wise we compress the input stream}
    if not HTree.RootIsLeaf then begin
      {allocate the codes array}
      New(HCodes);
      {calculate all the codes}
      HTree.CalcCodes(HCodes^);
      {compress the characters in the input stream to the bit stream}
      DoHuffmanCompression(aInStream, BitStrm, HCodes^);
    end;
  finally
    BitStrm.Free;
    HTree.Free;
    if (HCodes <> nil) then
      Dispose(HCodes);
  end;
end;
{--------}
procedure TDHuffmanDecompress(aInStream, aOutStream : TStream);
var
  Signature : longint;
  Size      : longint;
  HTree     : THuffmanTree;
  BitStrm   : TtdInputBitStream;
begin
  {make sure that the input stream is a valid Huffman encoded stream}
  aInStream.Seek(0, soFromBeginning);
  aInStream.ReadBuffer(Signature, sizeof(Signature));
  if (Signature <> TDHuffHeader) then
    raise EtdHuffmanException.Create(
       FmtLoadStr(tdeHuffBadEncodedStrm,
                 [UnitName, 'TDHuffmanDecompress']));
  aInStream.ReadBuffer(Size, sizeof(longint));
  {if there's nothing to decompress, exit now}
  if (Size = 0) then
    Exit;
  {prepare for the decompression}
  HTree := nil;
  BitStrm := nil;
  try
    {create the bit stream}
    BitStrm := TtdInputBitStream.Create(aInStream);
    BitStrm.Name := 'Huffman compressed stream';
    {create the Huffman tree}
    HTree := THuffmanTree.Create;
    {read the tree data from the input stream}
    HTree.LoadFromBitStream(BitStrm);
    {if the root of the Huffman tree is a leaf, the original stream
     just consisted of repetitions of one character}
    if HTree.RootIsLeaf then
      WriteMultipleChars(aOutStream, AnsiChar(HTree.Root), Size)
    {otherwise, using the Huffman tree, decompress the characters in
     the input stream}
    else
      DoHuffmanDecompression(BitStrm, aOutStream, HTree, Size);
  finally
    BitStrm.Free;
    HTree.Free;
  end;
end;
{====================================================================}

end.
