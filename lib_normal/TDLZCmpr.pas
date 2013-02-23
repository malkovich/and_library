(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDLZCmpr                                                         *)
(* LZ77 compression and decompression                               *)
(********************************************************************)

unit TDLZCmpr;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  Classes,
  TDLZBase;

procedure TDLZCompress(aInStream, aOutStream : TStream);
  {Compress input stream to output stream using LZ77 algorithm}

procedure TDLZDecompress(aInStream, aOutStream : TStream);
  {Decompress input stream to output stream using LZ77 algorithm}

implementation

{Notes: A distance/length encoding consists of two parts: the distance
        and the length. The distance varies from 1 to 8192, and the
        length from 3 to 10. To store both these values into a 16-bit
        entity, we subtract 1 from the distance value (to force it in
        the range 0 to 8191, or $0000 to $1FFF) and shift it left by 3
        bits. We subtract 3 from the length (to force it in the range
        0 to 7) and add it to the modified distance value. The
        resulting 16-bit value is then written.
        To unpack the two values on reading, perform the opposite. AND
        $7 to the 16-bit value, and add 3 for the length. Shift the
        16-bit value right by 3 bits and add 1 for the distance.}

uses
  TDBasics,
  TDLZSWin,
  TDLZHash;

const
  UnitName = 'TDLZCmpr';
  TDLZHeader = $5A4C4454; {the header for the compressed result}
                          { = 'TDLZ'}

type
  PEnumExtraData = ^TEnumExtraData;    {extra data record for the    }
  TEnumExtraData = packed record       {  hash table's FindAll method}
    edSW          : TtdLZSlidingWindow;{..sliding window object}
    edMaxLen      : integer;           {..maximum match length so far}
    edDistMaxMatch: integer;           {..distance of max match}
  end;

type
  TEncoding = packed record
    AsDistLen : cardinal;
    AsChar    : AnsiChar;
    IsChar    : boolean;
    {$IFDEF Win32}
    Filler    : word;
    {$ENDIF}
  end;
  TEncodingArray = packed record
    eaData : array [0..7] of TEncoding;
    eaCount: integer;
  end;

{===Exception raising================================================}
procedure RaiseError(aErrorCode   : integer;
               const aRoutineName : TtdNameString);
begin
  raise EtdLZException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, aRoutineName]));
end;
{====================================================================}


{===Helper routines==================================================}
procedure MatchLongest(aExtraData : pointer;
                 const aSignature : TtdLZSignature;
                       aOffset    : longint); far;
var
  Len  : integer;
  Dist : integer;
begin
  with PEnumExtraData(aExtraData)^ do begin
    Len := edSW.Compare(aOffset, Dist);
    if (Len > edMaxLen) then begin
      edMaxLen := Len;
      edDistMaxMatch := Dist;
    end;
  end;
end;
{--------}
procedure WriteEncodings(aStream    : TSTream;
                     var aEncodings : TEncodingArray);
var
  i : integer;
  FlagByte : byte;
  Mask     : byte;
begin
  {build flag byte, write it to the stream}
  FlagByte := 0;
  Mask := 1;
  for i := 0 to pred(aEncodings.eaCount) do begin
    if not aEncodings.eaData[i].IsChar then
      FlagByte := FlagByte or Mask;
    Mask := Mask shl 1;
  end;
  aStream.WriteBuffer(FlagByte, sizeof(FlagByte));
  {write out the encodings}
  for i := 0 to pred(aEncodings.eaCount) do begin
    if aEncodings.eaData[i].IsChar then
      aStream.WriteBuffer(aEncodings.eaData[i].AsChar, 1)
    else
      aStream.WriteBuffer(aEncodings.eaData[i].AsDistLen, 2);
  end;
  aEncodings.eaCount := 0;
end;
{--------}
procedure AddCharToEncodings(aStream : TStream;
                                 aCh : AnsiChar;
                      var aEncodings : TEncodingArray);
begin
  with aEncodings do begin
    eaData[eaCount].AsChar := aCh;
    eaData[eaCount].IsChar := true;
    inc(eaCount);
    if (eaCount = 8) then
      WriteEncodings(aStream, aEncodings);
  end;
end;
{--------}
procedure AddCodeToEncodings(aStream   : TStream;
                             aDistance : integer;
                             aLength   : integer;
                        var aEncodings : TEncodingArray);
begin
  with aEncodings do begin
    eaData[eaCount].AsDistLen :=
       (pred(aDistance) shl tdcLZDistanceShift) + (aLength - 3);
    eaData[eaCount].IsChar := false;
    inc(eaCount);
    if (eaCount = 8) then
      WriteEncodings(aStream, aEncodings);
  end;
end;
{====================================================================}


{===Interfaced routines==============================================}
procedure TDLZCompress(aInStream, aOutStream : TStream);
var
  HashTable : TtdLZHashTable;
  SlideWin  : TtdLZSlidingWindow;
  Signature : TtdLZSignature;
  Offset    : longint;
  Encodings : TEncodingArray;
  EnumData  : TEnumExtraData;
  LongValue : longint;
  i         : integer;
begin
  HashTable := nil;
  SlideWin := nil;
  try
    HashTable := TtdLZHashTable.Create;
    HashTable.Name := 'LZ77 Compression hash table';
    SlideWin := TtdLZSlidingWindow.Create(aInStream, true);
    SlideWin.Name := 'LZ77 Compression sliding window';
    {write the header to the stream: 'TDLZ' followed by uncompressed
     size of input stream}
    LongValue := TDLZHeader;
    aOutStream.WriteBuffer(LongValue, sizeof(LongValue));
    LongValue := aInStream.Size;
    aOutStream.WriteBuffer(LongValue, sizeof(LongValue));
    {prepare for the compression}
    Encodings.eaCount := 0;
    EnumData.edSW := SlideWin;
    {get the first signature}
    SlideWin.GetNextSignature(Signature, Offset);
    {while the signature is three characters long...}
    while (length(Signature.AsString) = 3) do begin
      {find the longest match in the sliding window using the hash
       table to identify matches}
      EnumData.edMaxLen := 0;
      if HashTable.EnumMatches(Signature,
                               Offset - tdcLZSlidingWindowSize,
                               MatchLongest,
                               @EnumData) then begin
        {we have at least one match: save the distance/length pair
         of the longest match and advance the sliding window by its
         length}
        AddCodeToEncodings(aOutStream,
                           EnumData.edDistMaxMatch,
                           EnumData.edMaxLen,
                           Encodings);
        SlideWin.Advance(EnumData.edMaxLen);
      end
      else begin
        {we don't have a match: save the current character and
         advance by 1}
        AddCharToEncodings(aOutStream,
                           Signature.AsString[1],
                           Encodings);
        SlideWin.Advance(1);
      end;
      {now add this signature to the hash table}
      HashTable.Insert(Signature, Offset);
      {get the next signature}
      SlideWin.GetNextSignature(Signature, Offset);
    end;
    {if the last signature of all was at most two characters, save
     them as literal character encodings}
    if (length(Signature.AsString) > 0) then begin
      for i := 1 to length(Signature.AsString) do
        AddCharToEncodings(aOutStream,
                           Signature.AsString[i],
                           Encodings);
    end;
    {make sure we write out the final encodings}
    if (Encodings.eaCount > 0) then
      WriteEncodings(aOutStream, Encodings);
  finally
    SlideWin.Free;
    HashTable.Free;
  end;{try..finally}
end;
{--------}
procedure TDLZDecompress(aInStream, aOutStream : TStream);
type
  TDecodeState = (dsGetFlagByte, dsGetChar, dsGetDistLen);
var
  SlideWin      : TtdLZSlidingWindow;
  BytesUnpacked : longint;
  TotalSize     : longint;
  LongValue     : longint;
  DecodeState   : TDecodeState;
  FlagByte      : byte;
  FlagMask      : byte;
  NextChar      : AnsiChar;
  NextDistLen   : longint;
  CodeCount     : integer;
  Len           : integer;
begin
  SlideWin := TtdLZSlidingWindow.Create(aOutStream, false);
  try
    SlideWin.Name := 'LZ77 Decompress sliding window';
    {read the header from the stream: 'TDLZ' followed by uncompressed
     size of input stream}
    aInStream.ReadBuffer(LongValue, sizeof(LongValue));
    if (LongValue <> TDLZHeader) then
      RaiseError(tdeLZBadEncodedStream, 'TDLZDecompress');
    aInStream.ReadBuffer(TotalSize, sizeof(TotalSize));
    {prepare for the decompression}
    BytesUnpacked := 0;
    NextDistLen := 0;
    DecodeState := dsGetFlagByte;
    CodeCount := 0;
    FlagMask := 1;
    {while there are still bytes to decompress...}
    while (BytesUnpacked < TotalSize) do begin
      {given the current decode state, read the next item}
      case DecodeState of
        dsGetFlagByte :
          begin
            aInStream.ReadBuffer(FlagByte, 1);
            CodeCount := 0;
            FlagMask := 1;
          end;
        dsGetChar :
          begin
            aInStream.ReadBuffer(NextChar, 1);
            SlideWin.AddChar(NextChar);
            inc(BytesUnpacked);
          end;
        dsGetDistLen :
          begin
            aInStream.ReadBuffer(NextDistLen, 2);
            Len := (NextDistLen and tdcLZLengthMask) + 3;
            SlideWin.AddCode((NextDistLen shr tdcLZDistanceShift) + 1, Len);
            inc(BytesUnpacked, Len);
          end;
      end;
      {calculate the next decode state}
      inc(CodeCount);
      if (CodeCount > 8) then
        DecodeState := dsGetFlagByte
      else begin
        if ((FlagByte and FlagMask) = 0) then
          DecodeState := dsGetChar
        else
          DecodeState := dsGetDistLen;
        FlagMask := FlagMask shl 1;
      end;
    end;
  finally
    SlideWin.Free;
  end;{try..finally}
end;
{====================================================================}


end.
