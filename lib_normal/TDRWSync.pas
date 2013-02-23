(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDRWSync                                                         *)
(* Readers/writers synchronization class                            *)
(********************************************************************)

unit TDRWSync;

{$I TDDefine.inc}

interface

{$IFNDEF Win32}
!! Error - this unit is for 32-bit Windows only
{$ENDIF}

uses
  Windows, SysUtils;

type
  TtdReadWriteSync = class
    private
      FActiveReaders  : integer;
      FActiveWriter   : boolean;
      FBlockedReaders : THandle; {a semaphore}
      FBlockedWriters : THandle; {a semaphore}
      FController     : TRTLCriticalSection;
      FWaitingReaders : integer;
      FWaitingWriters : integer;
    protected
    public
      constructor Create;
      destructor Destroy; override;

      procedure StartReading;
      procedure StartWriting;
      procedure StopReading;
      procedure StopWriting;
  end;

implementation

const
  MaxReaders = 127;

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


{====================================================================}
constructor TtdReadWriteSync.Create;
var
  NameZ : array [0..MAX_PATH] of AnsiChar;
begin
  inherited Create;
  {create the primitive synchronization objects}
  GetRandomObjName(NameZ, 'tdRW.BlockedReaders');
  FBlockedReaders := CreateSemaphore(nil, 0, MaxReaders, NameZ);
  GetRandomObjName(NameZ, 'tdRW.BlockedWriters');
  FBlockedWriters := CreateSemaphore(nil, 0, 1, NameZ);
  InitializeCriticalSection(FController);
end;
{--------}
destructor TtdReadWriteSync.Destroy;
begin
  CloseHandle(FBlockedReaders);
  CloseHandle(FBlockedWriters);
  DeleteCriticalSection(FController);
  inherited Destroy;
end;
{--------}
procedure TtdReadWriteSync.StartReading;
var
  HaveToWait : boolean;
begin
  {acquire the controlling critical section}
  EnterCriticalSection(FController);

  {if there is a writer executing or there is at least one writer
   waiting, add ourselves as a waiting reader, make sure we wait}
  if FActiveWriter or (FWaitingWriters <> 0) then begin
    Assert(FWaitingReaders < MaxReaders,
           'TtdReadWriteSync: maximum waiting readers reached');
    inc(FWaitingReaders);
    HaveToWait := true;
  end

  {otherwise, add ourselves as another executing reader,
   and make sure we don't wait}
  else begin
    inc(FActiveReaders);
    HaveToWait := false;
  end;

  {release the controlling critical section}
  LeaveCriticalSection(FController);

  {if we have to wait, then do so}
  if HaveToWait then
    WaitForSingleObject(FBlockedReaders, INFINITE);
end;
{--------}
procedure TtdReadWriteSync.StartWriting;
var
  HaveToWait : boolean;
begin
  {acquire the controlling critical section}
  EnterCriticalSection(FController);

  {if there is another writer running or there are active readers, add
   ourselves as a waiting writer, and make sure we wait}
  if FActiveWriter or (FActiveReaders <> 0) then begin
    inc(FWaitingWriters);
    HaveToWait := true;
  end

  {otherwise, add ourselves as another executing writer, and make sure
   we don't wait}
  else begin
    FActiveWriter := true;
    HaveToWait := false;
  end;

  {release the controlling critical section}
  LeaveCriticalSection(FController);

  {if we have to wait, then do so}
  if HaveToWait then
    WaitForSingleObject(FBlockedWriters, INFINITE);
end;
{--------}
procedure TtdReadWriteSync.StopReading;
begin
  {acquire the controlling critical section}
  EnterCriticalSection(FController);

  {we've finished reading}
  dec(FActiveReaders);

  {if we are the last reader reading and there is at
   least one writer waiting, then release it}
  if (FActiveReaders = 0) and (FWaitingWriters <> 0) then begin
    dec(FWaitingWriters);
    FActiveWriter := true;
    ReleaseSemaphore(FBlockedWriters, 1, nil);
  end;

  {release the controlling critical section}
  LeaveCriticalSection(FController);
end;
{--------}
procedure TtdReadWriteSync.StopWriting;
var
  i : integer;
begin
  {acquire the controlling critical section}
  EnterCriticalSection(FController);

  {we've finished writing}
  FActiveWriter := false;

  {if there is at least one reader waiting then release them all}
  if (FWaitingReaders <> 0) then begin
    FActiveReaders := FWaitingReaders;
    FWaitingReaders := 0;
    ReleaseSemaphore(FBlockedReaders, FActiveReaders, nil);
  end

  {otherwise, if there is at least one waiting writer, release one}
  else if (FWaitingWriters <> 0) then begin
    dec(FWaitingWriters);
    FActiveWriter := true;
    ReleaseSemaphore(FBlockedWriters, 1, nil);
  end;

  {release the controlling critical section}
  LeaveCriticalSection(FController);
end;
{====================================================================}

end.
