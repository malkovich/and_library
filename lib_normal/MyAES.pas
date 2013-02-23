unit MyAES;

interface
uses
  windows,Classes,Forms,SysUtils,UnitAES;

type
  TFootInfo = record
    dwBaseInternalSize :dword;
    dwBaseInternalCount  :dword;
    dwTotalInteralCount :dword;
  end;

const
  DEFAULT_AES_BLOCK_COUNT = 128 * 1024;
  AES_BLOCK_SIZE  = SizeOf(TAESBuffer);
  AES_HANDLE_SIZE = SizeOf(TAESBuffer);

procedure GetRandomKey256(var Key256:TAESKey256);
function FmtKeyAsGUID(Key256:TAESKey256): String;

function EncryptAESBuferECB( Buf: pchar; Count: dword; const Key: string):dword; overload;
function EncryptAESBuferECB( Buf: pchar; Count: dword; const Key256:TAESKey256):dword; overload;
function EncryptAESBuferECB( Buf: TStream; Count: dword; const Key256:TAESKey256):dword; overload;
function EncryptAESBuferECB( Buf: TStream; FootInfo:TFootInfo; const Key256:TAESKey256):dword; overload;

function DecryptAESBufferECB( Buf: pchar; Count: dword; const Key: string):dword; overload;
function DecryptAESBufferECB( Buf: pchar; Count: dword; const Key256:TAESKey256):dword; overload;
function DecryptAESBufferECB( Buf: TStream; Count: dword; const Key256:TAESKey256):dword; overload;
function DecryptAESBufferECB( Buf: TStream; FootInfo:TFootInfo; const Key256:TAESKey256):dword; overload;

function GetFootInfo(var FootInfo:TFootInfo; dwSize:dword; dwAESBlockCount:dword=DEFAULT_AES_BLOCK_COUNT):boolean;

implementation

uses md5;


function FmtKeyAsGUID(Key256:TAESKey256): String;
var
  g : TGUID;
begin
  move(Key256, g, sizeof(TGUID));
  Result:=GUIDToString(g);
  move(Key256[16], g, sizeof(TGUID));
  Result:=Result+GUIDToString(g);
end;

procedure GetRandomKey256(var Key256:TAESKey256);
var
  g : TGUID;
begin
  if S_OK=CreateGuid(g) then
    move(g, Key256, sizeof(TGUID));
  if S_OK=CreateGuid(g) then
    move(g, Key256[16], sizeof(TGUID));
end;


function GetAESKey256(Key: string):TAESKey256;
var
  szMD5,szMD52:MD5Digest;
begin
  szMD5:=MD5String(Key);
  szMD52:=MD5String(Key+'and');
  FillChar( result, SizeOf(TAESKey256), 0 );
  Move( szMD5, result, SizeOf(MD5Digest));
  Move( szMD52, result[SizeOf(MD5Digest)], SizeOf(MD5Digest));
end;

function EncryptAESBuferECB( Buf: pchar; Count: dword; const Key: string):dword; overload;
var
  AESKey256:TAESKey256;
begin
  AESKey256:= GetAESKey256(Key);
  result:=EncryptAESBuferECB( Buf, Count, AESKey256); 
end;

function EncryptAESBuferECB( Buf: pchar; Count: dword; const Key256:TAESKey256):dword; overload;
var
  TempIn, TempOut: TAESBuffer;
  Done: dword;
  ExpandedKey: TAESExpandedKey256;
begin
  result:=0;
  if Count = 0 then exit;

  ExpandAESKeyForEncryption(Key256,ExpandedKey);

  Done:=0;
  while Count >= SizeOf(TAESBuffer) do
  begin
    Move(Buf[Done],  TempIn, SizeOf(TAESBuffer));
    EncryptAES(TempIn, ExpandedKey, TempOut);
    Move(TempOut, Buf[Done], SizeOf(TAESBuffer));
    Dec(Count, SizeOf(TAESBuffer));
    inc(Done, SizeOf(TAESBuffer));
  end;
  if Count > 0 then
  begin
    Move(Buf[Done],  TempIn, Count);
    FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);
    EncryptAES(TempIn, ExpandedKey, TempOut);
    Move(TempOut, Buf[Done], SizeOf(TAESBuffer));
    inc(Done, SizeOf(TAESBuffer));
  end;
  result:=Done;
end;

function EncryptAESBuferECB( Buf: TStream; Count: dword; const Key256:TAESKey256):dword; overload;
var
  TempIn, TempOut: TAESBuffer;
  ExpandedKey: TAESExpandedKey256;
  Done:cardinal;
begin
  result:=0;
  if Count = 0 then
  begin
    Buf.Position := 0;
    Count := Buf.Size;
  end
  else Count := Min(Count, Buf.Size - Buf.Position);
  if Count = 0 then exit;

  ExpandAESKeyForEncryption(Key256,ExpandedKey);

  while Count >= SizeOf(TAESBuffer) do
  begin
    Done := Buf.Read(TempIn, SizeOf(TAESBuffer));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SReadError);

    EncryptAES(TempIn, ExpandedKey, TempOut);

    Buf.Seek(Buf.Position-SizeOf(TAESBuffer), soFromBeginning);
    Done := Buf.Write(TempOut, SizeOf(TAESBuffer));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);
    Dec(Count, SizeOf(TAESBuffer));
    application.ProcessMessages;
  end;
  if Count > 0 then
  begin
    Done := Buf.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SReadError);
    FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);

    EncryptAES(TempIn, ExpandedKey, TempOut);

    Buf.Seek(Buf.Position-Count, soFromBeginning);
    Done := Buf.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);
  end;
  result:=Buf.Size;
end;

function DecryptAESBufferECB( Buf: TStream; Count: dword; const Key256:TAESKey256):dword; overload;
var
  TempIn, TempOut: TAESBuffer;
  ExpandedKey: TAESExpandedKey256;
  Done: cardinal;
begin
  result:=0;
  if Count = 0 then
  begin
    Buf.Position := 0;
    Count := Buf.Size;
  end
  else Count := Min(Count, Buf.Size - Buf.Position);
  if Count = 0 then exit;
  if (Count mod SizeOf(TAESBuffer)) > 0 then
    raise EAESError.Create(SInvalidInBufSize);

  ExpandAESKeyForDecryption(Key256,ExpandedKey);

  while Count >= SizeOf(TAESBuffer) do
  begin
    Done := Buf.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SReadError);

    DecryptAES(TempIn, ExpandedKey, TempOut);

    Buf.Seek(Buf.Position-SizeOf(TAESBuffer), soFromBeginning);
    Done := Buf.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);
    Dec(Count, SizeOf(TAESBuffer));
    application.ProcessMessages;
  end;
  result:=Buf.Size;
end;

function DecryptAESBufferECB( Buf: pchar; Count: dword; const Key: string):dword; overload;
var
  AESKey256:TAESKey256;
begin
  AESKey256:= GetAESKey256(Key);
  result:=DecryptAESBufferECB( Buf, Count, AESKey256);
end;

function DecryptAESBufferECB( Buf: pchar; Count: dword; const Key256:TAESKey256):dword; overload;
var
  TempIn, TempOut: TAESBuffer;
  Done: cardinal;
  ExpandedKey: TAESExpandedKey256;
begin
  result:=0;
  if Count = 0 then exit;

  ExpandAESKeyForDecryption(Key256,ExpandedKey);

  if (Count mod SizeOf(TAESBuffer)) > 0 then
    raise EAESError.Create(SInvalidInBufSize);

  Done:=0;
  while Count >= SizeOf(TAESBuffer) do
  begin
    Move(Buf[Done],  TempIn, SizeOf(TAESBuffer));
    DecryptAES(TempIn, ExpandedKey, TempOut);
    Move(TempOut, Buf[Done], SizeOf(TAESBuffer));
    Dec(Count, SizeOf(TAESBuffer));
    inc(Done, SizeOf(TAESBuffer));
  end;
  result:=Done;
end;

/////////////////////////////////////////////////


function GetFootInfo(var FootInfo:TFootInfo; dwSize:dword; dwAESBlockCount:dword=DEFAULT_AES_BLOCK_COUNT):boolean;
var
  nMod, nDiv:dword;
  dwAESSize, dwFillSize :dword;
  dwTotalInteralCount :dword;
begin
  result:=false;
  FootInfo.dwBaseInternalSize:=0;
  FootInfo.dwBaseInternalCount:=0;
  FootInfo.dwTotalInteralCount:=0;
  dwAESSize :=dwAESBlockCount * AES_BLOCK_SIZE;   
  if dwSize <= dwAESSize then exit;
  
  dwFillSize :=dwSize - dwAESSize;
  dwTotalInteralCount :=dwAESBlockCount-1;

  nMod :=dwFillSize mod dwTotalInteralCount;
  nDiv :=dwFillSize div dwTotalInteralCount;

  FootInfo.dwBaseInternalSize := nDiv;
  FootInfo.dwBaseInternalCount  := dwTotalInteralCount - nMod;
  FootInfo.dwTotalInteralCount := dwTotalInteralCount;

  result := true;                
end;


function EncryptAESBuferECB( Buf: TStream; FootInfo:TFootInfo;  const Key256:TAESKey256):dword; overload;
var
  TempIn, TempOut: TAESBuffer;
  ExpandedKey: TAESExpandedKey256;
  Done, CountDone:dword;
begin
  result:=0;
  if Buf.Size = 0 then exit;

  if FootInfo.dwTotalInteralCount = 0 then
  begin
    result := EncryptAESBuferECB(Buf, 0, Key256);
    exit;
  end;

  ExpandAESKeyForEncryption(Key256,ExpandedKey);

  CountDone := 0;
  while CountDone < FootInfo.dwTotalInteralCount do
  begin
    Done := Buf.Read(TempIn, SizeOf(TAESBuffer));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SReadError);

    EncryptAES(TempIn, ExpandedKey, TempOut);

    Buf.Seek(Buf.Position-SizeOf(TAESBuffer), soFromBeginning);
    Done := Buf.Write(TempOut, SizeOf(TAESBuffer));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);

    inc(CountDone);

    if CountDone > FootInfo.dwBaseInternalCount then
      Buf.Seek(Buf.Position+FootInfo.dwBaseInternalSize+1, soFromBeginning)
    else
      Buf.Seek(Buf.Position+FootInfo.dwBaseInternalSize, soFromBeginning);

    application.ProcessMessages;
  end;  
  result:=Buf.Size;
end;


function DecryptAESBufferECB( Buf: TStream; FootInfo:TFootInfo; const Key256:TAESKey256):dword; overload;
var
  TempIn, TempOut: TAESBuffer;
  ExpandedKey: TAESExpandedKey256;
  Done, CountDone:cardinal;
begin
  result:=0;
  if Buf.Size = 0 then exit;

  if FootInfo.dwTotalInteralCount = 0 then
  begin
    result := DecryptAESBufferECB(Buf, 0, Key256);
    exit;
  end;

  ExpandAESKeyForDecryption(Key256,ExpandedKey);

  CountDone := 0;
  while CountDone < FootInfo.dwTotalInteralCount do
  begin
    Done := Buf.Read(TempIn, SizeOf(TAESBuffer));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SReadError);

    DecryptAES(TempIn, ExpandedKey, TempOut);

    Buf.Seek(Buf.Position-SizeOf(TAESBuffer), soFromBeginning);
    Done := Buf.Write(TempOut, SizeOf(TAESBuffer));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);

    inc(CountDone);

    if CountDone > FootInfo.dwBaseInternalCount then
      Buf.Seek(Buf.Position+FootInfo.dwBaseInternalSize+1, soFromBeginning)
    else
      Buf.Seek(Buf.Position+FootInfo.dwBaseInternalSize, soFromBeginning);

    application.ProcessMessages;
  end;
  result:=Buf.Size;
end;


end.
