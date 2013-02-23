unit MyRC4;

interface
uses
  SysUtils,RC4,windows,classes,GlobalUnit;

type
  TKeySize = (Key128,Key256);
  PTRC4Context=^TRC4Context;

////////////////////////////////////////////////////////////////////////////////
function RC4Initial:dword;
procedure RC4CodeBuffer( id:dword; Buf:pchar; Len:dword); overload;
procedure RC4CodeBuffer(var RC4Context:TRC4Context; Buf:pchar; Len:dword); overload;
procedure RC4CodeBuffer(var RC4Context:TRC4Context; Stream:TStream); overload;
procedure RC4Finally(id:dword);
////////////////////////////////////////////////////////////////////////////////

procedure RC4InitRandomKey(var RC4: TRC4Context);
function GetRandomKeyString(size:TKeySize):string;   

procedure SaveKeyToFile(RC4: TRC4Context;szFileName:string);
function LoadKeyFormFile(szFileName:string):TRC4Context;

resourcestring
  SInvalidInBufSize = 'Invalid buffer size for decryption';
  SReadError = 'Stream read error';
  SWriteError = 'Stream write error';


implementation

procedure SaveKeyToFile(RC4: TRC4Context;szFileName:string);
var
  fileStm:TFileStream;
begin
  fileStm:=TFileStream.Create(szFileName, fmCreate OR fmShareDenyWrite);
  try
    fileStm.Seek(0,soFromBeginning);
    fileStm.Write(RC4, sizeof(TRC4Context));
  finally
    filestm.Free;
  end;
end;

function LoadKeyFormFile(szFileName:string):TRC4Context;
var
  fileStm:TFileStream;
begin
  fileStm:=TFileStream.Create(szFileName, fmOpenRead);
  try
    fileStm.Seek(0,soFromBeginning);
    fileStm.Read(result, sizeof(TRC4Context));
  finally
    filestm.Free;
  end;
end;

function RC4Initial:dword;
var
 pRC4KeyRecord:PTRC4KeyRecord;
begin
  GetMem(pRC4KeyRecord, sizeof(TRC4KeyRecord));
  RC4InitRandomKey(pRC4KeyRecord.RC4Key2048);
  pRC4KeyRecord.RC4Key2048_bak:=pRC4KeyRecord.RC4Key2048;
  result:=dword(pRC4KeyRecord);
end;

procedure RC4CodeBuffer( id:dword; Buf:pchar; Len:dword);
begin
  RC4Code(PTRC4Context(id)^, Buf[0], Buf[0], Len);
end;

procedure RC4CodeBuffer(var RC4Context:TRC4Context; Stream:TStream); overload;
const
  DEFAULT_BUFFER_SIZE = 8*1024*1024;
var
  pBuf:pchar;
  nStreamSize, nPosition:int64;
  nDiv, nMod, nRet, i :integer;
begin
  GetMem(pBuf, DEFAULT_BUFFER_SIZE);
  try
    nStreamSize :=Stream.Size;
    nDiv := nStreamSize div DEFAULT_BUFFER_SIZE;
    nMod := nStreamSize mod DEFAULT_BUFFER_SIZE;

    nPosition :=0;
    if nDiv > 0 then
    begin
      for i:=1 to nDiv do
      begin
        Stream.Seek(nPosition, soFromBeginning);
        nRet:= Stream.Read(pBuf[0], DEFAULT_BUFFER_SIZE);
        if nRet < DEFAULT_BUFFER_SIZE then
          raise EStreamError.Create(SReadError);

        RC4CodeBuffer(RC4Context, pBuf, DEFAULT_BUFFER_SIZE);

        Stream.Seek(nPosition, soFromBeginning);
        nRet:= Stream.Write(pBuf[0], DEFAULT_BUFFER_SIZE);
        if nRet < DEFAULT_BUFFER_SIZE then
          raise EStreamError.Create(SWriteError);

        inc(nPosition, DEFAULT_BUFFER_SIZE);
      end;
    end;

    if nMod > 0 then
    begin
      Stream.Seek(nPosition, soFromBeginning);
      nRet:= Stream.Read(pBuf[0], nMod);

      RC4CodeBuffer(RC4Context, pBuf, nRet);
      
      Stream.Seek(nPosition, soFromBeginning);
      Stream.Write(pBuf[0], nRet);
    end;

  finally
    FreeMem(pBuf, DEFAULT_BUFFER_SIZE);
  end;
end;

procedure RC4CodeBuffer(var RC4Context:TRC4Context; Buf:pchar; Len:dword);
begin
  RC4Code(RC4Context, Buf[0], Buf[0], Len);
end;

procedure RC4Finally(id:dword);
begin
  FreeMem(PTRC4KeyRecord(id), sizeof(TRC4KeyRecord));
end;


function GetRandomKeyString(size:TKeySize):string;
var
  g : TGUID;
begin
  case size of
    Key128:
      begin
        setlength(result, 16);
        if S_OK=CreateGuid(g) then
          move(g, result[1], sizeof(TGUID));
      end;
    Key256:
      begin
         setlength(result, 32);
         if S_OK=CreateGuid(g) then
           move(g, result[1], sizeof(TGUID));
         if S_OK=CreateGuid(g) then
           move(g, result[17], sizeof(TGUID));
      end;
  end;
end;

procedure RC4InitRandomKey(var RC4: TRC4Context);
var
  g : TGUID;
  U: Integer;
begin
{$R-}
{$Q-}
  with RC4 do
  begin
    I := 0;
    J := 0;    
    for U:=0 to 15 do
    begin
      if S_OK=CreateGuid(g) then
        move(g, D[U*16], sizeof(TGUID));
    end;
  end;
end;


end.
