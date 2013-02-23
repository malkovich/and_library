(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDLZSWin                                                         *)
(* Sliding Window buffer for LZ77 compression                       *)
(********************************************************************)

unit TDLZSWin;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  Classes,
  TDBasics,
  TDLZBase;

{Notes: TtdLZSlidingWindow implements a sliding window on data for
        the LZ77 implementation. The sliding window class can be used
        in two ways: for compression and for decompression.

        For compression, the stream passed to the class is
        automatically read when required to keep the internal buffer
        fully loaded. The procedure GetNextSignature returns the 3
        character string at the current position ready for the hashing
        routine. (Note that right at the end of the input stream a two
        or less character string will be returned - this is a signal
        to end the compression phase.) Advance will move the sliding
        window on by one character (or as many that are specified -
        depends on the match). Compare will compare the string at the
        current position with that at the given offset (note that this
        is an offset into the input stream and not the position in the
        sliding window) and will return the number of matched
        characters.

        For decompression, the stream passed to the class is the
        output stream (the decompressed stream). The decompressor will
        either call AddChar to add a literal character at the current
        position and advance by one, or will call AddCode to decode
        a distance/length pair and advance the stream on by as many
        characters as is decoded. The class will periodically update
        the stream from the internal buffer.

        The DebugMode compiler define adds some extra checking at the
        start of each interfaced method to ensure that the sliding
        window is used in the correct mode.}

type
  TtdLZSlidingWindow = class
    private
      FBuffer       : PAnsiChar;     {the circular buffer}
      FBufferEnd    : PAnsiChar;     {the end point of the buffer}
      FCompressing  : boolean;       {true=for compressing data}
      FCurrent      : PAnsiChar;     {current character}
      FLookAheadEnd : PAnsiChar;     {end of the look-ahead}
      FMidPoint     : PAnsiChar;     {mid-point of the buffer}
      FName         : TtdNameString; {name of the sliding window}
      FStart        : PAnsiChar;     {start of the sliding window}
      FStartOffset  : longint;       {stream offset for FStart}
      FStream       : TStream;       {underlying stream}
    protected
      procedure swAdvanceAfterAdd(aCount : integer);
      procedure swError(aErrorCode  : integer;
                  const aMethodName : TtdNameString);
      {$IFDEF DebugMode}
      procedure swInvalidUseError(
                            const aMethodName : TtdNameString);
      {$ENDIF}
      procedure swReadFromStream;
      procedure swSetCapacity(aValue : longint);
      procedure swWriteToStream(aFinalBlock : boolean);
    public
      constructor Create(aStream      : TStream;
                         aCompressing : boolean);
      destructor Destroy; override;

      {methods used for both compression and decompression}
      procedure Clear;

      {methods used during compression}
      procedure Advance(aCount : integer);
      function Compare(aOffset   : longint;
                   var aDistance : integer) : integer;
      procedure GetNextSignature(var aMS     : TtdLZSignature;
                                 var aOffset : longint);

      {methods used during decompression}
      procedure AddChar(aCh : AnsiChar);
      procedure AddCode(aDistance : integer; aLength : integer);

      property Name : TtdNameString
         read FName write FName;
  end;

implementation

const
  UnitName = 'TDLZSWin';

{Notes:
        Meaning of the internal pointers:

        |----------+===================+==+--------------------------|
        |          |                   |  |                          |
        FBuffer    FStart       FCurrent  FLookAheadEnd     FBufferEnd

        The valid data is between FStart and FLookAheadEnd when
        compressing, and between FStart and FCurrent when
        decompressing (FLookAheadEnd not being used in this latter
        case). Usually the difference between FStart and FCurrent is
        8192, the size of the sliding window.
        }


{===TtdLZSlidingWindow=============================================}
constructor TtdLZSlidingWindow.Create(aStream      : TStream;
                                      aCompressing : boolean);
begin
  inherited Create;
  {save parameters}
  FCompressing := aCompressing;
  FStream := aStream;
  {set capacity of sliding window: by definition this is 8192 bytes of
   sliding window and 10 bytes of lookahead}
  swSetCapacity(tdcLZSlidingWindowSize + tdcLZLookAheadSize);
  {reset the buffer and, if we're compressing, read some data from the
   stream to be compressed}
  Clear;
  if aCompressing then
    swReadFromStream;
end;
{--------}
destructor TtdLZSlidingWindow.Destroy;
begin
  if Assigned(FBuffer) then begin
    {finish writing to the output stream if we're decompressing}
    if not FCompressing then
      swWriteToStream(true);
    {free the buffer}
    FreeMem(FBuffer, FBufferEnd - FBuffer);
  end;
  inherited Destroy;
end;
{--------}
procedure TtdLZSlidingWindow.AddChar(aCh : AnsiChar);
begin
  {$IFDEF DebugMode}
  {this is a decompression routine only}
  if FCompressing then
    swInvalidUseError('AddChar');
  {$ENDIF}
  {add the character to the buffer}
  FCurrent^ := aCh;
  {advance the sliding window by one character}
  swAdvanceAfterAdd(1);
end;
{--------}
procedure TtdLZSlidingWindow.AddCode(aDistance : integer; aLength : integer);
var
  FromChar : PAnsiChar;
  ToChar   : PAnsiChar;
  i        : integer;
begin
  {$IFDEF DebugMode}
  {this is a decompression routine only}
  if FCompressing then
    swInvalidUseError('AddCode');
  {$ENDIF}
  {set up the pointers to do the data copy; note we cannot use Move
   since part of the data we are copying may be set up by the actual
   copying of the data}
  FromChar := FCurrent - aDistance;
  ToChar := FCurrent;
  for i := 1 to aLength do begin
    ToChar^ := FromChar^;
    inc(FromChar);
    inc(ToChar);
  end;
  {advance the start of the sliding window}
  swAdvanceAfterAdd(aLength);
end;
{--------}
procedure TtdLZSlidingWindow.Advance(aCount : integer);
var
  ByteCount : integer;
begin
  {$IFDEF DebugMode}
  {this is a compression routine only}
  if not FCompressing then
    swInvalidUseError('Advance');
  {$ENDIF}
  {advance the start of the sliding window, if required}
  if ((FCurrent - FStart) >= tdcLZSlidingWindowSize) then begin
    inc(FStart, aCount);
    inc(FStartOffset, aCount);
  end;
  {advance the current pointer}
  inc(FCurrent, aCount);
  {check to see if we have advanced into the overflow zone}
  if (FStart >= FMidPoint) then begin
    {move current data back to the start of the buffer}
    ByteCount := FLookAheadEnd - FStart;
    Move(FStart^, FBuffer^, ByteCount);
    {reset the various pointers}
    ByteCount := FStart - FBuffer;
    FStart := FBuffer;
    dec(FCurrent, ByteCount);
    dec(FLookAheadEnd, ByteCount);
    {read some more data from the stream}
    swReadFromStream;
  end;
end;
{--------}
procedure TtdLZSlidingWindow.Clear;
begin
  FStart := FBuffer;
  FCurrent := FBuffer;
  FLookAheadEnd := FBuffer;
  FStartOffset := 0;
end;
{--------}
function TtdLZSlidingWindow.Compare(aOffset   : longint;
                                var aDistance : integer) : integer;
var
  MatchStr  : PAnsiChar;
  CurrentCh : PAnsiChar;
begin
  {Note: when this routine is called it is assumed that at least three
         characters will match between the passed position and the
         current position}
  {$IFDEF DebugMode}
  {this is a compression routine only}
  if not FCompressing then
    swInvalidUseError('Compare');
  {$ENDIF}
  {calculate the position in the sliding window for the passed offset
   and its distance from the current position}
  MatchStr := FStart + (aOffset - FStartOffset);
  aDistance := FCurrent - MatchStr;
  inc(MatchStr, 3);
  {calculate the length of the matching characters between this and
   the current position. Don't go above the maximum length. Have a
   special case for the end of the input stream}
  Result := 3;
  CurrentCh := FCurrent + 3;
  if (CurrentCh <> FLookAheadEnd) then begin
    while (Result < tdcLZMaxMatchLength) and
          (MatchStr^ = CurrentCh^) do begin
      inc(Result);
      inc(MatchStr);
      inc(CurrentCh);
      if (CurrentCh = FLookAheadEnd) then
        Break;
    end;
  end;
end;
{--------}
procedure TtdLZSlidingWindow.GetNextSignature(var aMS     : TtdLZSignature;
                                              var aOffset : longint);
var
  P : PAnsiChar;
  i : integer;
begin
  {$IFDEF DebugMode}
  {this is a compression routine only}
  if not FCompressing then
    swInvalidUseError('GetNextSignature');
  {$ENDIF}
  {calculate the length of the match string; usually it's 3, but at
   the end of the input stream it could be 2 or less.}
  if ((FLookAheadEnd - FCurrent) < 3) then
    aMS.AsString[0] := AnsiChar(FLookAheadEnd - FCurrent)
  else
    aMS.AsString[0] := #3;
  P := FCurrent;
  for i := 1 to length(aMS.AsString) do begin
    aMS.AsString[i] := P^;
    inc(P);
  end;
  aOffset := FStartOffset + (FCurrent - FStart);
end;
{--------}
procedure TtdLZSlidingWindow.swAdvanceAfterAdd(aCount : integer);
begin
  {advance the start of the sliding window, if required}
  if ((FCurrent - FStart) >= tdcLZSlidingWindowSize) then begin
    inc(FStart, aCount);
    inc(FStartOffset, aCount);
  end;
  {advance the current pointer}
  inc(FCurrent, aCount);
  {check to see if we have advanced into the overflow zone}
  if (FStart >= FMidPoint) then begin
    {write some more data to the stream (from FBuffer to FStart)}
    swWriteToStream(false);
    {move current data back to the start of the buffer}
    Move(FStart^, FBuffer^, FCurrent - FStart);
    {reset the various pointers}
    dec(FCurrent, FStart - FBuffer);
    FStart := FBuffer;
  end;
end;
{--------}
procedure TtdLZSlidingWindow.swError(aErrorCode  : integer;
                               const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdLZException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
{$IFDEF DebugMode}
procedure TtdLZSlidingWindow.swInvalidUseError(
                                   const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  if FCompressing then
    raise EtdLZException.Create(
       FmtLoadStr(tdeLZEncodeBadMethod,
                  [UnitName, ClassName, aMethodName, Name]))
  else
    raise EtdLZException.Create(
       FmtLoadStr(tdeLZDecodeBadMethod,
                  [UnitName, ClassName, aMethodName, Name]));
end;
{$ENDIF}
{--------}
procedure TtdLZSlidingWindow.swReadFromStream;
var
  BytesRead   : longint;
  BytesToRead : longint;
begin
  {read some more data into the look ahead zone}
  BytesToRead := FBufferEnd - FLookAheadEnd;
  BytesRead := FStream.Read(FLookAheadEnd^, BytesToRead);
  inc(FLookAheadEnd, BytesRead);
end;
{--------}
procedure TtdLZSlidingWindow.swSetCapacity(aValue : longint);
var
  NewQueue  : PAnsiChar;
begin
  {round the requested capacity to nearest 64 bytes}
  aValue := (aValue + 63) and $7FFFFFC0;
  {get a new buffer}
  GetMem(NewQueue, aValue * 2);
  {destroy the old buffer}
  if (FBuffer <> nil) then
    FreeMem(FBuffer, FBufferEnd - FBuffer);
  {set the head/tail and other pointers}
  FBuffer := NewQueue;
  FStart := NewQueue;
  FCurrent := NewQueue;
  FLookAheadEnd := NewQueue;
  FBufferEnd := NewQueue + (aValue * 2);
  FMidPoint := NewQueue + aValue;
end;
{--------}
procedure TtdLZSlidingWindow.swWriteToStream(aFinalBlock : boolean);
var
  BytesToWrite : longint;
begin
  {write the data before the current sliding window}
  if aFinalBlock then
    BytesToWrite := FCurrent - FBuffer
  else
    BytesToWrite := FStart - FBuffer;
  FStream.WriteBuffer(FBuffer^, BytesToWrite);
end;
{====================================================================}

end.

