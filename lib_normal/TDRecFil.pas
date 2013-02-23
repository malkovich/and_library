(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDRecFil                                                         *)
(* Class encapsulating a file of fixed length records               *)
(********************************************************************)

unit TDRecFil;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  {$IFDEF Delphi1}
  WinTypes, WinProcs,
  {$ENDIF}
  {$IFDEF Delphi2Plus}
  Windows,
  {$ENDIF}
  {$IFDEF Kylix1Plus}
  Types, Libc,
  {$ENDIF}
  Classes,
  TDBasics;

type
  PtdRSHeaderRec = ^TtdRSHeaderRec;
  TtdRSHeaderRec = packed record
    hrSignature : longint;
    hrVersion   : longint;
    hrHeaderLen : longint;
    hrRecordLen : longint;
    hrCapacity  : longint;
    hrCount     : longint;
    hr1stDelRec : longint;
    heFiller    : array [0..8] of longint; {for expansion}
  end;

type
  TtdRecordStream = class
    private
      FStream      : TStream;
      FCount       : longint;
      FCapacity    : longint;
      FHeaderRec   : PtdRSHeaderRec;
      FName        : TtdNameString;
      FRecord      : PByteArray;
      FRecordLen   : integer;
      FRecordLen4  : integer;
      FZeroPosition: longint;
    protected
      procedure rsSetCapacity(aCapacity : longint);

      procedure rsError(aErrorCode  : integer;
                  const aMethodName : TtdNameString;
                        aNumValue   : longint);
      function rsErrorMsg(aErrorCode  : integer;
                    const aMethodName : TtdNameString;
                          aNumValue   : longint) : string;

      function rsCalcRecordOffset(aIndex : longint) : longint;
      procedure rsCreateHeaderRec(aRecordLen : integer);
      procedure rsReadHeaderRec;
      procedure rsReadStream(var aBuffer; aBufLen : integer);
      procedure rsWriteStream(var aBuffer; aBufLen : integer);
      procedure rsSeekStream(aOffset : longint);
    public
      constructor Create(aStream       : TStream;
                         aRecordLength : integer);
      destructor Destroy; override;

      procedure Flush; virtual;

      function Add(var aRecord) : longint;
      procedure Clear;
      procedure Delete(aIndex : longint);
      procedure Read(aIndex : longint;
                     var aRecord;
                     var aIsDeleted : boolean);
      procedure Write(aIndex : longint;
                      var aRecord);

      property Capacity : longint
         read FCapacity write rsSetCapacity;
      property Count : longint
         read FCount;
      property RecordLength : integer
         read FRecordLen;
      property Name : TtdNameString
         read FName write FName;
  end;

  TtdRecordFile = class(TtdRecordStream)
    private
      FEnsureSafe : boolean;
      FFileName   : string;
      FMode       : word;
      FStream     : TFileStream;
    public
      constructor Create(const aFileName : string;
                               aMode     : word;
                               aRecordLength : integer);
      destructor Destroy; override;

      procedure Flush; override;

      property EnsureSafe : boolean
                  read FEnsureSafe write FEnsureSafe;
  end;


implementation

const
  UnitName = 'TDRecFil';

const
  cRSSignature = $1A524454;
  cActiveRecord = -1;
  cEndOfDeletedChain = -2;

{===TtdRecordStream===================================================}
constructor TtdRecordStream.Create(aStream       : TStream;
                                   aRecordLength : integer);
begin
  Assert(aStream <> nil,
         rsErrorMsg(tdeNoStream, 'Create', 0));

  {create the ancestor}
  inherited Create;
  {save the stream, and its current position}
  FStream := aStream;
  FZeroPosition := aStream.Position;
  {if the stream size is zero we have to create the header record}
  if (aStream.Size - FZeroPosition = 0) then
    rsCreateHeaderRec(aRecordLength)
  {otherwise, check to see if it has a valid header record, read it
   and set up our fields}
  else
    rsReadHeaderRec;
  {allocate a work record}
  FRecordLen4 := FRecordLen + sizeof(longint);
  GetMem(FRecord, FRecordLen4);
end;
{--------}
destructor TtdRecordStream.Destroy;
begin
  if (FHeaderRec <> nil) then
    FreeMem(FHeaderRec, FHeaderRec^.hrHeaderLen);
  if (FRecord <> nil) then
    FreeMem(FRecord, FRecordLen4);
  inherited Destroy;
end;
{--------}
function TtdRecordStream.Add(var aRecord) : longint;
begin
  {if the deleted record chain is empty, we'll be adding the record
   to the end of the stream}
  if (FHeaderRec^.hr1stDelRec = cEndOfDeletedChain) then begin
    Result := FCapacity;
    inc(FCapacity);
    inc(FHeaderRec^.hrCapacity);
  end
  {otherwise, use the first deleted record, update the header record's
   deleted record chain to start at the next deleted record}
  else begin
    Result := FHeaderRec^.hr1stDelRec;
    rsSeekStream(rsCalcRecordOffset(FHeaderRec^.hr1stDelRec));
    rsReadStream(FHeaderRec^.hr1stDelRec, sizeof(longint));
  end;
  {seek to the record offset and write the new record}
  rsSeekStream(rsCalcRecordOffset(Result));
  PLongint(FRecord)^ := cActiveRecord;
  Move(aRecord, FRecord^[sizeof(longint)], FRecordLen);
  rsWriteStream(FRecord^, FRecordLen4);
  {we have one more record}
  inc(FCount);
  inc(FHeaderRec^.hrCount);
  {now update the header record}
  rsSeekStream(FZeroPosition);
  rsWriteStream(FHeaderRec^, sizeof(TtdRSHeaderRec));
end;
{--------}
procedure TtdRecordStream.Clear;
var
  Inx : longint;
  DeletedFlag : longint;
begin
  {visit all records and join them to the deleted record chain}
  for Inx := 0 to pred(FCapacity) do begin
    rsSeekStream(rsCalcRecordOffset(Inx));
    rsReadStream(DeletedFlag, sizeof(longint));
    if (DeletedFlag = cActiveRecord) then begin
      {write the first deleted record number to the first 4 bytes of
       the record we're deleting}
      rsSeekStream(rsCalcRecordOffset(Inx));
      rsWriteStream(FHeaderRec^.hr1stDelRec, sizeof(longint));
      {update the header record's deleted record chain to start at the
       record we're deleting}
      FHeaderRec^.hr1stDelRec := Inx;
    end;
  end;
  {we have no records}
  FCount := 0;
  FHeaderRec^.hrCount := 0;
  {now update the header record}
  rsSeekStream(FZeroPosition);
  rsWriteStream(FHeaderRec^, sizeof(TtdRSHeaderRec));
end;
{--------}
procedure TtdRecordStream.Delete(aIndex : longint);
var
  DeletedFlag : longint;
begin
  {check the record number to be valid}
  if (aIndex < 0) or (aIndex >= Capacity) then
    rsError(tdeRSOutOfBounds, 'Delete', aIndex);
  {check to see that the record is not already deleted}
  rsSeekStream(rsCalcRecordOffset(aIndex));
  rsReadStream(DeletedFlag, sizeof(longint));
  if (DeletedFlag <> cActiveRecord) then
    rsError(tdeRSAlreadyDeleted, 'Delete', aIndex);
  {write the first deleted record number to the first 4 bytes of the
   record we're deleting}
  rsSeekStream(rsCalcRecordOffset(aIndex));
  rsWriteStream(FHeaderRec^.hr1stDelRec, sizeof(longint));
  {update the header record's deleted record chain to start at the
   record we're deleting}
  FHeaderRec^.hr1stDelRec := aIndex;
  {we have one less record}
  dec(FCount);
  dec(FHeaderRec^.hrCount);
  {now update the header record}
  rsSeekStream(FZeroPosition);
  rsWriteStream(FHeaderRec^, sizeof(TtdRSHeaderRec));
end;
{--------}
procedure TtdRecordStream.Flush;
begin
  {do nothing at this level}
end;
{--------}
procedure TtdRecordStream.Read(aIndex : longint;
                           var aRecord;
                           var aIsDeleted : boolean);
begin
  {check the record number to be valid}
  if (aIndex < 0) or (aIndex >= Capacity) then
    rsError(tdeRSOutOfBounds, 'Read', aIndex);
  {seek to the record offset and read the record}
  rsSeekStream(rsCalcRecordOffset(aIndex));
  rsReadStream(FRecord^, FRecordLen4);
  if (PLongint(FRecord)^ = cActiveRecord) then begin
    aIsDeleted := false;
    Move(FRecord^[sizeof(longint)], aRecord, FRecordLen);
  end
  else begin
    aIsDeleted := true;
    FillChar(aRecord, FRecordLen, 0);
  end;
end;
{--------}
function TtdRecordStream.rsCalcRecordOffset(aIndex : longint) : longint;
begin
  Result := FZeroPosition + FHeaderRec^.hrHeaderLen +
            (aIndex * FRecordLen4);
end;
{--------}
procedure TtdRecordStream.rsCreateHeaderRec(aRecordLen : integer);
begin
  {allocate a header record}
  if ((aRecordLen + sizeof(longint)) < sizeof(TtdRSHeaderRec)) then begin
    FHeaderRec := AllocMem(sizeof(TtdRSHeaderRec));
    FHeaderRec^.hrHeaderLen := sizeof(TtdRSHeaderRec);
  end
  else begin
    FHeaderRec := AllocMem(aRecordLen + sizeof(longint));
    FHeaderRec^.hrHeaderLen := aRecordLen + sizeof(longint);
  end;
  {set other standard fields}
  with FHeaderRec^ do begin
    hrSignature := cRSSignature;
    hrVersion := $00010000; {Major=1; Minor=0}
    hrRecordLen := aRecordLen;
    hrCapacity := 0;
    hrCount := 0;
    hr1stDelRec := cEndOfDeletedChain;
  end;
  {now update the header record}
  rsSeekStream(FZeroPosition);
  rsWriteStream(FHeaderRec^, FHeaderRec^.hrHeaderLen);
  {set the record length field}
  FRecordLen := aRecordLen;
end;
{--------}
procedure TtdRecordStream.rsError(aErrorCode  : integer;
                            const aMethodName : TtdNameString;
                                  aNumValue   : longint);
begin
  raise EtdArrayException.Create(
                      rsErrorMsg(aErrorCode, aMethodName, aNumValue));
end;
{--------}
function TtdRecordStream.rsErrorMsg(aErrorCode  : integer;
                              const aMethodName : TtdNameString;
                                    aNumValue   : longint) : string;
begin
  if (Name = '') then
    Name := '-unnamed-';
  Result := FmtLoadStr(aErrorCode,
                 [UnitName, ClassName, aMethodName, Name, aNumValue]);
end;
{--------}
procedure TtdRecordStream.rsReadHeaderRec;
var
  StreamSize    : longint;
  TempHeaderRec : TtdRSHeaderRec;
begin
  {if the stream size is not at least the size of the header record,
   it can't be one of ours}
  StreamSize := FStream.Size - FZeroPosition;
  if (StreamSize < sizeof(TtdRSHeaderRec)) then
    rsError(tdeRSNoHeaderRec, 'rsReadHeaderRec', 0);
  {read the header record}
  rsSeekStream(FZeroPosition);
  rsReadStream(TempHeaderRec, sizeof(TtdRSHeaderRec));
  {first sanity check: the signature and count/capacity}
  with TempHeaderRec do begin
    if (hrSignature <> cRSSignature) or
       (hrCount > hrCapacity) then
      rsError(tdeRSBadHeaderRec, 'rsReadHeaderRec', 0);
  end;
  {allocate the true header record, copy the data already read}
  FHeaderRec := AllocMem(TempHeaderRec.hrHeaderLen);
  Move(TempHeaderRec, FHeaderRec^, TempHeaderRec.hrHeaderLen);
  {second sanity check: check the record info}
  with FHeaderRec^ do begin
    FRecordLen4 := hrRecordLen + 4; {for rsCalcRecordOffset}
    if (StreamSize <> rsCalcRecordOffset(hrCapacity)) then
      rsError(tdeRSBadHeaderRec, 'rsReadHeaderRec', 0);
    {set up the class fields}
    FCount := hrCount;
    FCapacity := hrCapacity;
    FRecordLen := hrRecordLen;
  end;
end;
{--------}
procedure TtdRecordStream.rsReadStream(var aBuffer; aBufLen : integer);
var
  BytesRead : longint;
begin
  BytesRead := FStream.Read(aBuffer, aBufLen);
  if (BytesRead <> aBufLen) then
    rsError(tdeRSReadError, 'rsReadStream', aBufLen);
end;
{--------}
procedure TtdRecordStream.rsSeekStream(aOffset : longint);
var
  NewOffset : longint;
begin
  NewOffset := FStream.Seek(aOffset, soFromBeginning);
  if (NewOffset <> aOffset) then
    rsError(tdeRSSeekError, 'rsSeekStream', aOffset);
end;
{--------}
procedure TtdRecordStream.rsSetCapacity(aCapacity : longint);
var
  Inx : longint;
begin
  {we only accept increases in capacity}
  if (aCapacity > FCapacity) then begin
    {fill the work record with zeros}
    FillChar(FRecord^, FRecordLen4, 0);
    {seek to the end of the file}
    rsSeekStream(rsCalcRecordOffset(FCapacity));
    {write out the extra records, remembering to add them to the
     deleted record chain}
    for Inx := FCapacity to pred(aCapacity) do begin
      PLongint(FRecord)^ := FHeaderRec^.hr1stDelRec;
      rsWriteStream(FRecord^, FRecordLen4);
      FHeaderRec^.hr1stDelRec := Inx;
    end;
    {save the new capacity}
    FCapacity := aCapacity;
    FHeaderRec^.hrCapacity := aCapacity;
    {now update the header record}
    rsSeekStream(FZeroPosition);
    rsWriteStream(FHeaderRec^, sizeof(TtdRSHeaderRec));
  end;
end;
{--------}
procedure TtdRecordStream.rsWriteStream(var aBuffer; aBufLen : integer);
var
  BytesWritten : longint;
begin
  BytesWritten := FStream.Write(aBuffer, aBufLen);
  if (BytesWritten <> aBufLen) then
    rsError(tdeRSWriteError, 'rsWriteStream', aBufLen);
  Flush;
end;
{--------}
procedure TtdRecordStream.Write(aIndex : longint;
                            var aRecord);
var
  DeletedFlag : longint;
begin
  {check the record number to be valid}
  if (aIndex < 0) or (aIndex >= Capacity) then
    rsError(tdeIndexOutOfBounds, 'Write', aIndex);
  {check to see that the record is not already deleted}
  rsSeekStream(rsCalcRecordOffset(aIndex));
  rsReadStream(DeletedFlag, sizeof(longint));
  if (DeletedFlag <> cActiveRecord) then
    rsError(tdeRSRecIsDeleted, 'Write', aIndex);
  {write the record}
  rsWriteStream(aRecord, FRecordLen);
end;
{====================================================================}


{===TtdRecordFile====================================================}
constructor TtdRecordFile.Create(const aFileName : string;
                                       aMode     : word;
                                       aRecordLength : integer);
begin
  FStream := TFileStream.Create(aFileName, aMode);
  inherited Create(FStream, aRecordLength);
  FFileName := aFileName;
  FMode := aMode;
end;
{--------}
destructor TtdRecordFile.Destroy;
begin
  inherited Destroy;
  FStream.Free;
end;
{--------}
procedure TtdRecordFile.Flush;
{$IFDEF Windows}
var
  DosError : word;
  Handle   : THandle;
begin
  if EnsureSafe then begin
    Handle := FStream.Handle;
    asm
      mov ah, $68
      mov bx, Handle
      call DOS3Call
      jc @@Error
      xor ax, ax
    @@Error:
      mov DosError, ax
    end;
    if (DosError <> 0) then
      rsError(tdeRSFlushError, 'Flush', DosError)
  end;
end;
{$ENDIF}
{$IFDEF Delphi2Plus}
begin
  if EnsureSafe then begin
    if not FlushFileBuffers(FStream.Handle) then
      rsError(tdeRSFlushError, 'Flush', GetLastError)
  end;
end;
{$ENDIF}
{$IFDEF Kylix1Plus}
begin
  {nothing to do?}
end;
{$ENDIF}
{====================================================================}


end.
