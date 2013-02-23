unit MemoryStream;

interface
uses
  GlobalType;

const
  soFromBeginning = 0;
  soFromCurrent = 1;
  soFromEnd = 2;

type
  PStreamData = ^TStreamData;
  TStreamData = record
    RelocFnTable :LPRelocFunctionTable;
    Memory:Pointer;
    Size :Longint;
    Position :Longint;
    Capacity:LongInt;   
  end;

function MemStream_Seek(This:PStreamData; Offset: Longint; Origin: Word): Longint; InLine;
function MemStream_Read(This:PStreamData; var Buffer; Count: Longint): Longint; Inline;
function MemStream_Write(This:PStreamData; const Buffer; Count: Longint): Longint; Inline;
function MemStream_Create(RelocFnTable :LPRelocFunctionTable):PStreamData; Inline;
procedure MemStream_Destroy(This:PStreamData); Inline;

implementation

function MemStream_Seek(This:PStreamData; Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: This.Position := Offset;
    soFromCurrent: Inc(This.Position, Offset);
    soFromEnd: This.Position := This.Size + Offset;
  end;
  Result := This.Position;
end;

function MemStream_Read(This:PStreamData; var Buffer; Count: Longint): Longint;
begin
  if (This.Position >= 0) and (Count >= 0) then
  begin
    Result := This.Size - This.Position;
    if Result > 0 then
    begin
      if Result > Count then Result := Count;
      CopyMemory(@Buffer, Pointer(Longint(This.Memory) + This.Position), Result);
      Inc(This.Position, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

function MemStream_Write(This:PStreamData; const Buffer; Count: Longint): Longint;
var
  Pos: Longint;
begin
  if (This.Position >= 0) and (Count >= 0) then
  begin
    Pos := This.Position + Count;
    if Pos > 0 then
    begin
      if Pos > This.Size then
      begin
        result := 0;
        exit;
      end;
      CopyMemory(Pointer(Longint(This.Memory) + This.Position), @Buffer, Count);
      This.Position := Pos;
      Result := Count;
      Exit;
    end;
  end;
  Result := 0;
end;

function MemStream_Create(RelocFnTable :LPRelocFunctionTable):PStreamData;
begin
  Result := RelocFnTable.VirtualAlloc(nil,  SizeOf(TStreamData), MEM_COMMIT,PAGE_READWRITE);
  Result.Memory := nil;
  Result.Size := 0;
  Result.Position := 0;
  Result.RelocFnTable := RelocFnTable;
end;
procedure MemStream_Destroy(This:PStreamData);
begin
  This.RelocFnTable.VirtualFree(This,0,MEM_FREE);
end;

end.
