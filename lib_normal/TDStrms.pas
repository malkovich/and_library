(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDStrms                                                          *)
(* Different types of streams                                       *)
(********************************************************************)

unit TDStrms;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  Classes,
  TDBasics;

{---Simple buffered streams---}
type
  TtdBufferedInputStream = class(TStream)
    private
      FBufEnd : integer;
      FBuffer : PAnsiChar;
      FBufOfs : longint;
      FBufPos : integer;
      FName   : TtdNameString;
      FSize   : longint;
      FStream : TStream;
    protected
      procedure bisError(aErrorCode  : integer;
                   const aMethodName : TtdNameString);
      procedure bisReadBuffer;
    public
      constructor Create(aStream : TStream);
      destructor Destroy; override;

      function Read(var Buffer; Count : longint) : longint; override;
      function Seek(Offset : longint;
                    Origin : word) : longint; override;
      function Write(const Buffer;
                           Count : longint) : longint; override;

      property Name : TtdNameString
                  read FName write FName;
  end;

  TtdBufferedOutputStream = class(TStream)
    private
      FBuffer : PAnsiChar;
      FBufOfs : longint;
      FBufPos : integer;
      FName   : TtdNameString;
      FStream : TStream;
    protected
      procedure bosError(aErrorCode  : integer;
                   const aMethodName : TtdNameString);
      function bosWriteBuffer : boolean;
    public
      constructor Create(aStream : TStream);
      destructor Destroy; override;

      function Read(var Buffer; Count : longint) : longint; override;
      function Seek(Offset : longint;
                    Origin : word) : longint; override;
      function Write(const Buffer;
                           Count : longint) : longint; override;

      property Name : TtdNameString
                  read FName write FName;
  end;

{---Bit streams---}
type
  TtdBitString = packed record
    bsCount : integer; {actually the value is only 1..256}
    bsBits  : array [0..31] of byte;
  end;

type
  TtdInputBitStream = class
    private
      FAccum      : byte;
      FBufEnd     : integer;
      FBuffer     : PAnsiChar;
      FBufPos     : integer;
      FMask       : byte;
      FName       : TtdNameString;
      FStream     : TStream;
    protected
      procedure ibsError(aErrorCode  : integer;
                   const aMethodName : TtdNameString);
      procedure ibsReadBuffer;
    public
      constructor Create(aStream : TStream);
      destructor Destroy; override;

      function ReadBit : boolean;
      procedure ReadBits(var aBitString : TtdBitString;
                             aBitCount  : integer);
      function ReadByte : byte;

      property Name : TtdNameString
                  read FName write FName;
  end;

  TtdOutputBitStream = class
    private
      FAccum      : byte;
      FBuffer     : PAnsiChar;
      FBufPos     : integer;
      FMask       : byte;
      FName       : TtdNameString;
      FStream     : TStream;
      FStrmBroken : boolean;
    protected
      procedure obsError(aErrorCode  : integer;
                   const aMethodName : TtdNameString);
      procedure obsWriteBuffer;
    public
      constructor Create(aStream : TStream);
      destructor Destroy; override;

      procedure WriteBit(aBit : boolean);
      procedure WriteBits(const aBitString : TtdBitString);
      procedure WriteByte(aByte : byte);

      property Name : TtdNameString
                  read FName write FName;
  end;

function CompareStreams(aStream1, aStream2 : TStream) : boolean;
  {-compare to streams for equality}

implementation

const
  UnitName = 'TDStrms';
  StreamBufferSize = 4096;

{===TtdBufferedInputStream===========================================}
constructor TtdBufferedInputStream.Create(aStream : TStream);
begin
  inherited Create;
  FStream := aStream;
  FSize := FStream.Size;
  GetMem(FBuffer, StreamBufferSize);
end;
{--------}
destructor TtdBufferedInputStream.Destroy;
begin
  if (FBuffer <> nil) then
    FreeMem(FBuffer, StreamBufferSize);
  inherited Destroy;
end;
{--------}
procedure TtdBufferedInputStream.bisError(aErrorCode  : integer;
                                    const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdStreamException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
procedure TtdBufferedInputStream.bisReadBuffer;
begin
  if (FStream.Seek(FBufOfs, 0) >= 0) then
    FBufEnd := FStream.Read(FBuffer^, StreamBufferSize);
end;
{--------}
function TtdBufferedInputStream.Read(var Buffer;
                                         Count : longint) : longint;
var
  BufAsBytes  : TByteArray absolute Buffer;
  BufInx      : Longint;
  BytesToGo   : Longint;
  BytesToRead : integer;
begin
  {$IFDEF Delphi1}
  {in Delphi 1 we do not support reads greater than 65535 bytes}
  if (Count > $FFFF) then
    bisError(tdeInStreamRead, 'Read');
  {$ENDIF}

  {calculate the actual number of bytes we can read - this depends on
   the current position and size of the stream as well as the number
   of bytes requested}
  BytesToGo := Count;
  if (FSize < (FBufOfs + FBufPos + Count)) then
    BytesToGo := FSize - (FBufOfs + FBufPos);
  if (BytesToGo <= 0) then begin
    Result := 0;
    Exit;
  end;
  {remember to return the result of our calculation}
  Result := BytesToGo;

  {initialise the byte index for the caller's buffer}
  BufInx := 0;
  {is there anything in the buffer? if not, go read something from
   the actual stream}
  if (FBufEnd = 0) then
    bisReadBuffer;
  {calculate the number of bytes we can read prior to the loop}
  BytesToRead := FBufEnd - FBufPos;
  if (BytesToRead > BytesToGo) then
    BytesToRead := BytesToGo;
  {copy from the stream buffer to the caller's buffer}
  Move(FBuffer[FBufPos], BufAsBytes[BufInx], BytesToRead);
  {calculate the number of bytes still to read}
  dec(BytesToGo, BytesToRead);

  {while we have bytes to read, read them}
  while (BytesToGo > 0) do begin
    {advance the byte index for the caller's buffer}
    inc(BufInx, BytesToRead);
    {as we've exhausted this buffer-full, advance to the next}
    inc(FBufOfs, StreamBufferSize);
    FBufPos := 0;
    bisReadBuffer;
    {calculate the number of bytes we can read in this cycle}
    BytesToRead := FBufEnd;
    if (BytesToRead > BytesToGo) then
      BytesToRead := BytesToGo;
    {copy from the stream buffer to the caller's buffer}
    Move(FBuffer^, BufAsBytes[BufInx], BytesToRead);
    {calculate the number of bytes still to read}
    dec(BytesToGo, BytesToRead);
  end;
  {remember our new position}
  inc(FBufPos, BytesToRead);
  if (FBufPos = FBufEnd) then begin
    inc(FBufOfs, StreamBufferSize);
    FBufPos := 0;
    FBufEnd := 0;
  end;
end;
{--------}
function TtdBufferedInputStream.Write(const Buffer;
                                            Count : longint) : longint;
begin
  bisError(tdeInStreamWrite, 'Write');
  Result := 0;
end;
{--------}
function TtdBufferedInputStream.Seek(Offset : longint;
                                     Origin : word) : longint;
var
  NewPageStart : longint;
  NewPos       : longint;
begin
  {calculate the new position}
  case Origin of
    soFromBeginning : NewPos := Offset;
    soFromCurrent   : NewPos := FBufOfs + FBufPos + Offset;
    soFromEnd       : NewPos := FSize + Offset;
  else
    NewPos := 0;
    bisError(tdeStreamBadOrigin, 'Seek');
  end;
  if (NewPos < 0) then
    NewPos := 0
  else if (NewPos > FSize) then
    NewPos := FSize;
  {calculate which page of the file we need to be at}
  NewPageStart := NewPos and not 4095;
  {if the new page is different than the old, mark the buffer as being
   ready to be replenished}
  if (NewPageStart <> FBufOfs) then begin
    FBufOfs := NewPageStart;
    FBufEnd := 0;
  end;
  {save the new position}
  FBufPos := NewPos - NewPageStart;
  Result := NewPos;
end;
{====================================================================}


{===TtdBufferedOutputStream==========================================}
constructor TtdBufferedOutputStream.Create(aStream : TStream);
begin
  inherited Create;
  FStream := aStream;
  GetMem(FBuffer, StreamBufferSize);
end;
{--------}
destructor TtdBufferedOutputStream.Destroy;
begin
  if (FBuffer <> nil) then begin
    if (FBufPos > 0) then
      bosWriteBuffer;
    FreeMem(FBuffer, StreamBufferSize);
  end;
  inherited Destroy;
end;
{--------}
procedure TtdBufferedOutputStream.bosError(aErrorCode  : integer;
                                     const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdStreamException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
function TtdBufferedOutputStream.bosWriteBuffer : boolean;
begin
  Result := false;
  if (FStream.Seek(FBufOfs, 0) >= 0) then begin
    Result := (FStream.Write(FBuffer^, FBufPos) = FBufPos);
    FBufPos := 0;
  end;
end;
{--------}
function TtdBufferedOutputStream.Read(var Buffer;
                                          Count : longint) : longint;
begin
  bosError(tdeOutStreamRead, 'Read');
  Result := 0;
end;
{--------}
function TtdBufferedOutputStream.Seek(Offset : longint;
                                      Origin : word) : longint;
begin
  {the only Seek call we allow is a seek to the final position to give
   the current size of the stream; for any other signal an exception}
  if (Offset <> 0) or
     ((Origin <> soFromCurrent) and (Origin <> soFromEnd)) then
    bosError(tdeOutStreamSeek, 'Seek');

  {return the current position}
  Result := FBufOfs + FBufPos;
end;
{--------}
function TtdBufferedOutputStream.Write(const Buffer;
                                             Count : longint) : longint;
var
  BufAsBytes  : TByteArray absolute Buffer;
  BufInx      : Longint;
  BytesToGo   : Longint;
  BytesToWrite: integer;
begin
  {$IFDEF Delphi1}
  {in Delphi 1 we do not support writes greater than 65535 bytes}
  if (Count > $FFFF) then
    bosError(tdeOutStreamWrite, 'Write');
  {$ENDIF}

  {when we write to this stream we always assume that we can write the
   requested number of bytes: if we can't (eg, the disk is full) we'll
   get an exception somewhere eventually}
  BytesToGo := Count;
  {remember to return the result of our calculation}
  Result := BytesToGo;
  if (BytesToGo <= 0) then begin
    Result := 0;
    Exit;
  end;
  {initialise the byte index for the caller's buffer}
  BufInx := 0;
  {calculate the number of bytes we can write prior to the loop}
  BytesToWrite := StreamBufferSize - FBufPos;
  if (BytesToWrite > BytesToGo) then
    BytesToWrite := BytesToGo;
  {copy from the caller's buffer to the stream buffer}
  Move(BufAsBytes[BufInx], FBuffer[FBufPos], BytesToWrite);
  inc(FBufPos, BytesToWrite);
  {calculate the number of bytes still to write}
  dec(BytesToGo, BytesToWrite);

  {while we have bytes to write, write them}
  while (BytesToGo > 0) do begin
    {advance the byte index for the caller's buffer}
    inc(BufInx, BytesToWrite);
    {as we've filled this buffer, write it out to the actual stream
     and advance to the next buffer, reading it if required}
    if not bosWriteBuffer then begin
      {if we couldn't write to the underlying stream, we need to
       signal that something went wrong, so return zero bytes written}
      Result := 0;
      Exit;
    end;
    inc(FBufOfs, StreamBufferSize);
    {calculate the number of bytes we can write in this cycle}
    BytesToWrite := StreamBufferSize;
    if (BytesToWrite > BytesToGo) then
      BytesToWrite := BytesToGo;
    {copy from the caller's buffer to our buffer}
    Move(BufAsBytes[BufInx], FBuffer^, BytesToWrite);
    FBufPos := BytesToWrite;
    {calculate the number of bytes still to write}
    dec(BytesToGo, BytesToWrite);
  end;
end;
{====================================================================}


{===TtdInputBitStream================================================}
constructor TtdInputBitStream.Create(aStream : TStream);
begin
  inherited Create;
  FStream := aStream;
  GetMem(FBuffer, StreamBufferSize);
 {FMask := 0;}
 {FAccum := 0;}
 {FBufPos := 0;}
 {FBufEnd := 0:}
end;
{--------}
destructor TtdInputBitStream.Destroy;
begin
  if (FBuffer <> nil) then
    FreeMem(FBuffer, StreamBufferSize);
  inherited Destroy;
end;
{--------}
procedure TtdInputBitStream.ibsError(aErrorCode  : integer;
                               const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdStreamException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
procedure TtdInputBitStream.ibsReadBuffer;
begin
  FBufEnd := FStream.Read(FBuffer^, StreamBufferSize);
  if (FBufEnd = 0) then
    ibsError(tdeInBitStreamRead, 'ibsReadBuffer');
  FBufPos := 0;  
end;
{--------}
function TtdInputBitStream.ReadBit : boolean;
begin
  {if we have no bits left in the current accumulator, read another
   accumulator byte and reset the mask}
  if (FMask = 0) then begin
    if (FBufPos >= FBufEnd) then
      ibsReadBuffer;
    FAccum := byte(FBuffer[FBufPos]);
    inc(FBufPos);
    FMask := 1;
  end;
  {take the next bit}
  Result := (FAccum and FMask) <> 0;
  FMask := FMask shl 1;          {overflow required on this statement}
end;
{--------}
procedure TtdInputBitStream.ReadBits(var aBitString : TtdBitString;
                                         aBitCount  : integer);
var
  Mask     : byte;
  Accum    : byte;
  StrInx   : integer;
  StrMask  : byte;
  StrAccum : byte;
begin
  {to speed up this process, we shall take copies of the object's
   fields; at the end we'll copy them back}
  Mask := FMask;
  Accum := FAccum;
  {force the bit count in range (we can only read up to 256 bits)}
  if (aBitCount <= 0) then
    Exit;
  if (aBitCount > 256) then
    aBitCount := 256;
  aBitString.bsCount := aBitCount;
  {prepare for the loop(s)}
  StrInx := 0;
  StrMask := 1;
  StrAccum := 0;
  {extract as many bits from the accumulator as we can, refilling as
   necessary}
  while (aBitCount > 0) do begin
    {if the accumulator is empty, refill it and reset the mask}
    if (Mask = 0) then begin
      if (FBufPos >= FBufEnd) then
        ibsReadBuffer;
      Accum := byte(FBuffer[FBufPos]);
      inc(FBufPos);
      Mask := 1;
    end;
    {get the next bit}
    if ((Accum and Mask) <> 0) then
      StrAccum := StrAccum or StrMask;
    Mask := Mask shl 1;          {overflow required on this statement}
    StrMask := StrMask shl 1;    {overflow required on this statement}
    {write the next byte to the bit string if required}
    if (StrMask = 0) then begin
      aBitString.bsBits[StrInx] := StrAccum;
      inc(StrInx);
      StrMask := 1;
      StrAccum := 0;
    end;
    {that's one less bit}
    dec(aBitCount);
  end;
  {write out the final partial byte, if there is one}
  if (StrMask <> 1) then
    aBitString.bsBits[StrInx] := StrAccum;
  {save the new values of the accumulator and the mask}
  FMask := Mask;
  FAccum := Accum;
end;
{--------}
function TtdInputBitStream.ReadByte : byte;
var
  Mask   : byte;
  Accum  : byte;
  ByteMask : byte;
begin
  {to speed up this process, we shall take copies of the object's
   fields; at the end we'll copy them back}
  Mask := FMask;
  Accum := FAccum;
  {prepare for the loop(s)}
  ByteMask := 1;
  Result := 0;
  {extract as many bits from the accumulator as we can, refilling as
   necessary}
  while (ByteMask <> 0) do begin
    {if the accumulator is empty, refill it and reset the mask}
    if (Mask = 0) then begin
      if (FBufPos >= FBufEnd) then
        ibsReadBuffer;
      Accum := byte(FBuffer[FBufPos]);
      inc(FBufPos);
      Mask := 1;
    end;
    {get the next bit}
    if ((Accum and Mask) <> 0) then
      Result := Result or ByteMask;
    Mask := Mask shl 1;          {overflow required on this statement}
    ByteMask := ByteMask shl 1;  {overflow required on this statement}
  end;
  {save the new values of the accumulator and the mask}
  FMask := Mask;
  FAccum := Accum;
end;
{====================================================================}


{===TtdOutputBitStream===============================================}
constructor TtdOutputBitStream.Create(aStream : TStream);
begin
  inherited Create;
  FStream := aStream;
  GetMem(FBuffer, StreamBufferSize);
  FMask := 1; {ready for the first bit to be written}
 {FAccum := 0;}
 {FBufPos := 0;}
 {FBufEnd := 0:}
end;
{--------}
destructor TtdOutputBitStream.Destroy;
begin
  if (FBuffer <> nil) then begin
    {if Mask is not equal to 1, it means that there are some bits in
     the accumulator that need to be written to the buffer; make sure
     the buffer is written to the underlying stream}
    if not FStrmBroken then begin
      if (FMask <> 1) then begin
        byte(FBuffer[FBufPos]) := FAccum;
        inc(FBufPos);
      end;
      if (FBufPos > 0) then
        obsWriteBuffer;
    end;
    FreeMem(FBuffer, StreamBufferSize);
  end;
  inherited Destroy;
end;
{--------}
procedure TtdOutputBitStream.obsError(aErrorCode  : integer;
                                const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdStreamException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
procedure TtdOutputBitStream.obsWriteBuffer;
var
  BytesWrit : longint;
begin
  BytesWrit := FStream.Write(FBuffer^, FBufPos);
  if (BytesWrit <> FBufPos) then begin
    {we had a problem writing the buffer to the stream; raiuse an
     exception to say so, but first make sure so that we don't trigger
     the same exception in the Destroy as well}
    FStrmBroken := true;
    obsError(tdeOutBitStreamWrite, 'obsWriteBuffer');
  end;
  FBufPos := 0;
end;
{--------}
procedure TtdOutputBitStream.WriteBit(aBit : boolean);
begin
  {set the next spare bit}
  if aBit then
    FAccum := (FAccum or FMask);
  FMask := FMask shl 1;           {require overflow on this statement}
  {if we have no spare bits left in the current accumulator, write it
   to the buffer, and reset the accumulator and the mask}
  if (FMask = 0) then begin
    byte(FBuffer[FBufPos]) := FAccum;
    inc(FBufPos);
    if (FBufPos >= StreamBufferSize) then
      obsWriteBuffer;
    FAccum := 0;
    FMask := 1;
  end;
end;
{--------}
procedure TtdOutputBitStream.WriteBits(const aBitString : TtdBitString);
var
  Mask   : byte;
  Accum  : byte;
  BitCount : integer;
  StrInx   : integer;
  StrMask  : byte;
  StrAccum : byte;
begin
  {to speed up this process, we shall take copies of the object's
   fields; at the end we'll copy them back}
  Mask := FMask;
  Accum := FAccum;
  {prepare for the loop}
  BitCount := aBitString.bsCount;
  StrInx := 0;
  StrMask := 0;
  StrAccum := 0; {to fool the warning analyzer of the compiler} 
  {store as many bits to the accumulator as we can, writing it out and
   clearing it as necessary}
  while (BitCount > 0) do begin
    {if we need another byte from the bit string, get it and reset}
    if (StrMask = 0) then begin
      StrAccum := aBitString.bsBits[StrInx];
      inc(StrInx);
      StrMask := 1;
    end;
    {store the next bit}
    if ((StrAccum and StrMask) <> 0) then
      Accum := Accum or Mask;
    Mask := Mask shl 1;          {overflow required on this statement}
    StrMask := StrMask shl 1;    {overflow required on this statement}
    {if needed, write out the accumulator & reset}
    if (Mask = 0) then begin
      byte(FBuffer[FBufPos]) := Accum;
      inc(FBufPos);
      if (FBufPos >= StreamBufferSize) then
        obsWriteBuffer;
      Accum := 0;
      Mask := 1;
    end;
    {that's one more bit stored}
    dec(BitCount);
  end;
  {save the new values of the accumulator and the mask}
  FMask := Mask;
  FAccum := Accum;
end;
{--------}
procedure TtdOutputBitStream.WriteByte(aByte : byte);
var
  Mask   : byte;
  Accum  : byte;
  ByteMask : byte;
begin
  {to speed up this process, we shall take copies of the object's
   fields; at the end we'll copy them back}
  Mask := FMask;
  Accum := FAccum;
  {prepare for the loop}
  ByteMask := 1;
  {store as many bits to the accumulator as we can, writing it out and
   clearing it as necessary}
  while (ByteMask <> 0) do begin
    {store the next bit}
    if ((aByte and ByteMask) <> 0) then
      Accum := Accum or Mask;
    Mask := Mask shl 1;          {overflow required on this statement}
    ByteMask := ByteMask shl 1;  {overflow required on this statement}
    {if needed, write out the accumulator & reset}
    if (Mask = 0) then begin
      byte(FBuffer[FBufPos]) := Accum;
      inc(FBufPos);
      if (FBufPos >= StreamBufferSize) then
        obsWriteBuffer;
      Accum := 0;
      Mask := 1;
    end;
  end;
  {save the new values of the accumulator and the mask}
  FMask := Mask;
  FAccum := Accum;
end;
{====================================================================}


{===Interfaced routines==============================================}
function CompareStreams(aStream1, aStream2 : TStream) : boolean;
var
  Buf1, Buf2 : PByteArray;
  BytesRead  : integer;
begin
  {assume the streams are not equal}
  Result := false;

  {if the stream sizes are not the same, the streams cannot be equal}
  if (aStream1.Size <> aStream2.Size) then
    Exit;

  {get ready for the loop: position both streams at the start}
  aStream1.Position := 0;
  aStream2.Position := 0;

  Buf1 := nil;
  Buf2 := nil;
  try
   GetMem(Buf1, StreamBufferSize);
   GetMem(Buf2, StreamBufferSize);

   {read blocks from both streams and then compare them for equality;
    continue until both streams are exhausted}
   BytesRead := aStream1.Read(Buf1^, StreamBufferSize);
   while (BytesRead <> 0) do begin
     aStream2.Read(Buf2^, StreamBufferSize);
     if not CompareMem(Buf1, Buf2, BytesRead) then
       Exit;
     BytesRead := aStream1.Read(Buf1^, StreamBufferSize);
   end;
  finally
    FreeMem(Buf2, StreamBufferSize);
    FreeMem(Buf1, StreamBufferSize);
  end;

  {if we reach this point, the streams are equal}
  Result := true;
end;
{====================================================================}

end.
