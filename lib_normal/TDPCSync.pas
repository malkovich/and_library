(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDPCSync                                                         *)
(* Producers/consumers synchronization classes                      *)
(********************************************************************)

unit TDPCSync;

{$I TDDefine.inc}

interface

{$IFNDEF Win32}
!! Error - this unit is for 32-bit Windows only
{$ENDIF}

uses
  Windows, SysUtils, Classes, TDBasics;

type
  TtdProduceConsumeSync = class
    private
      FHasData   : THandle; {a semaphore}
      FNeedsData : THandle; {a semaphore}
    protected
    public
      constructor Create(aBufferCount : integer);
      destructor Destroy; override;

      procedure StartConsuming;
      procedure StartProducing;
      procedure StopConsuming;
      procedure StopProducing;
  end;

  TtdProduceManyConsumeSync = class
    private
      FBufferCount   : integer; {count of data buffers}
      FBufferInfo    : TList;   {a circular queue of buffer info}
      FBufferTail    : integer; {tail of the buffer circular queue}
      FConsumerCount : integer; {count of consumers}
      FConsumerInfo  : TList;   {info for each consumer}
      FNeedsData     : THandle; {a semaphore}
    protected
    public
      constructor Create(aBufferCount   : integer;
                         aConsumerCount : integer);
      destructor Destroy; override;

      procedure StartConsuming(aId : integer);
      procedure StartProducing;
      procedure StopConsuming(aId : integer);
      procedure StopProducing;
  end;

implementation

const
  MaxBuffers = 127;

{===Helper routines==================================================}
procedure GetRandomObjName(aDest : PChar; const aRootName : string);
var
  Len : integer;
  i   : integer;
begin
  Len := length(aRootName);
  StrCopy(aDest, PChar(aRootName));
  inc(aDest, Len);
  aDest^ := '/';
  inc(aDest);
  for i := 1 to 10 do begin
    aDest^ := chr(Random(26) + ord('A'));
    inc(aDest);
  end;
  aDest^ := #0;
end;
{====================================================================}


{===TtdProduceConsumeSync============================================}
constructor TtdProduceConsumeSync.Create(aBufferCount : integer);
var
  NameZ : array [0..MAX_PATH] of AnsiChar;
begin
  inherited Create;
  {create the primitive synchronization objects; note that the "needs
   data" semaphore is set up with the required count so that the
   producer can start immediately}
  FNeedsData := INVALID_HANDLE_VALUE;
  GetRandomObjName(NameZ, 'tdPC.HasData');
  FHasData := CreateSemaphore(nil, 0, aBufferCount, NameZ);
  if (FHasData = INVALID_HANDLE_VALUE) then
    RaiseLastWin32Error;
  GetRandomObjName(NameZ, 'tdPC.NeedsData');
  FNeedsData := CreateSemaphore(nil, aBufferCount, aBufferCount, NameZ);
  if (FNeedsData = INVALID_HANDLE_VALUE) then
    RaiseLastWin32Error;
end;
{--------}
destructor TtdProduceConsumeSync.Destroy;
begin
  if (FHasData <> INVALID_HANDLE_VALUE) then
    CloseHandle(FHasData);
  if (FNeedsData <> INVALID_HANDLE_VALUE) then
    CloseHandle(FNeedsData);
  inherited Destroy;
end;
{--------}
procedure TtdProduceConsumeSync.StartConsuming;
begin
  {to start consuming, the "has data" semaphore needs to be signalled}
  WaitForSingleObject(FHasData, INFINITE);
end;
{--------}
procedure TtdProduceConsumeSync.StartProducing;
begin
  {to start producing, the "needs data" semaphore needs to be
   signalled}
  WaitForSingleObject(FNeedsData, INFINITE);
end;
{--------}
procedure TtdProduceConsumeSync.StopConsuming;
begin
  {if we've consumed some data, we should signal the
   producer to generate some more}
  ReleaseSemaphore(FNeedsData, 1, nil);
end;
{--------}
procedure TtdProduceConsumeSync.StopProducing;
begin
  {if we've produced some more data, we should signal the
   consumer to use it up}
  ReleaseSemaphore(FHasData, 1, nil);
end;
{====================================================================}


{===TtdProduceManyConsumeSync========================================}
type
  PBufferInfo = ^TBufferInfo;
  TBufferInfo = packed record
    biToUseCount : integer; {count of consumers still to use buffer}
  end;
{--------}
type
  PConsumerInfo = ^TConsumerInfo;
  TConsumerInfo = packed record
    ciHasData : THandle; {a semaphore}
    ciHead    : integer; {head pointer into the buffer queue}
  end;
{--------}
constructor TtdProduceManyConsumeSync.Create(aBufferCount   : integer;
                                             aConsumerCount : integer);
var
  NameZ : array [0..MAX_PATH] of AnsiChar;
  i     : integer;
  BufInfo : PBufferInfo;
  ConsumerInfo : PConsumerInfo;
begin
  inherited Create;
  {create the "needs data" semaphore}
  GetRandomObjName(NameZ, 'tdPMC.NeedsData');
  FNeedsData := CreateSemaphore(nil, aBufferCount, aBufferCount, NameZ);
  if (FNeedsData = INVALID_HANDLE_VALUE) then
    RaiseLastWin32Error;
  {create the buffer circular queue and populate it}
  FBufferCount := aBufferCount;
  FBufferInfo := TList.Create;
  FBufferInfo.Count := aBufferCount;
  for i := 0 to pred(aBufferCount) do begin
    New(BufInfo);
    BufInfo^.biToUseCount := 0;
    FBufferInfo[i] := BufInfo;
  end;
  {create the consumer info list and populate it}
  FConsumerCount := aConsumerCount;
  FConsumerInfo := TList.Create;
  FConsumerInfo.Count := aConsumerCount;
  for i := 0 to pred(aConsumerCount) do begin
    New(ConsumerInfo);
    FConsumerInfo[i] := ConsumerInfo;
    GetRandomObjName(NameZ, 'tdPMC.HasData');
    ConsumerInfo^.ciHasData :=
       CreateSemaphore(nil, 0, aBufferCount, NameZ);
    if (ConsumerInfo^.ciHasData = INVALID_HANDLE_VALUE) then
      RaiseLastWin32Error;
    ConsumerInfo^.ciHead := 0;
  end;
end;
{--------}
destructor TtdProduceManyConsumeSync.Destroy;
var
  i : integer;
  BufInfo : PBufferInfo;
  ConsumerInfo : PConsumerInfo;
begin
  {destroy the "needs data" semaphore}
  if (FNeedsData <> INVALID_HANDLE_VALUE) then
    CloseHandle(FNeedsData);
  {destroy the consumer info list}
  if (FConsumerInfo <> nil) then begin
    for i := 0 to pred(FConsumerCount) do begin
      ConsumerInfo := PConsumerInfo(FConsumerInfo[i]);
      if (ConsumerInfo <> nil) then begin
        if (ConsumerInfo^.ciHasData <> INVALID_HANDLE_VALUE) then
          CloseHandle(ConsumerInfo^.ciHasData);
        Dispose(ConsumerInfo);
      end;
    end;
    FConsumerInfo.Free;
  end;
  {destroy the buffer info list}
  if (FBufferInfo <> nil) then begin
    for i := 0 to pred(FBufferCount) do begin
      BufInfo := PBufferInfo(FBufferInfo[i]);
      if (BufInfo <> nil) then
        Dispose(BufInfo);
    end;
    FBufferInfo.Free;
  end;
  inherited Destroy;
end;
{--------}
procedure TtdProduceManyConsumeSync.StartConsuming(aId : integer);
var
  ConsumerInfo : PConsumerInfo;
begin
  {to start consuming, the "has data" semaphore needs to be signalled
   for that particular consumer id}
  ConsumerInfo := PConsumerInfo(FConsumerInfo[aId]);
  WaitForSingleObject(ConsumerInfo^.ciHasData, INFINITE);
end;
{--------}
procedure TtdProduceManyConsumeSync.StartProducing;
begin
  {to start producing, the "needs data" semaphore needs to be
   signalled}
  WaitForSingleObject(FNeedsData, INFINITE);
end;
{--------}
procedure TtdProduceManyConsumeSync.StopConsuming(aId : integer);
var
  BufInfo      : PBufferInfo;
  ConsumerInfo : PConsumerInfo;
  NumToRead    : integer;
begin
  {we've consumed the data in the buffer at our head pointer}
  ConsumerInfo := PConsumerInfo(FConsumerInfo[aId]);
  BufInfo := PBufferInfo(FBufferInfo[ConsumerInfo^.ciHead]);
  NumToRead := InterlockedDecrement(BufInfo^.biToUseCount);
  {advance our head pointer}
  inc(ConsumerInfo^.ciHead);
  if (ConsumerInfo^.ciHead >= FBufferCount) then
    ConsumerInfo^.ciHead := 0;
  {if we were the last to use this buffer, we should signal the
   producer to generate some more}
  if (NumToRead = 0) then
    ReleaseSemaphore(FNeedsData, 1, nil);
end;
{--------}
procedure TtdProduceManyConsumeSync.StopProducing;               
var
  i : integer;
  BufInfo      : PBufferInfo;
  ConsumerInfo : PConsumerInfo;
begin
  {if we've produced some more data, set the count of consumers for
   the buffer at the tail to cover all of them}
  BufInfo := PBufferInfo(FBufferInfo[FBufferTail]);
  BufInfo^.biToUseCount := FConsumerCount;
  inc(FBufferTail);
  if (FBufferTail >= FBufferCount) then
    FBufferTail := 0;
  {now signal all the consumers that there is more data}
  for i := 0 to pred(FConsumerCount) do begin
    ConsumerInfo := PConsumerInfo(FConsumerInfo[i]);
    ReleaseSemaphore(ConsumerInfo^.ciHasData, 1, nil);
  end;
end;
{====================================================================}


end.
