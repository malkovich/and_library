unit SimArray;

interface
uses
   GlobalType;

Type
  PSimArrayObject = ^TSimArrayObject;
  TSimArrayObject = packed record
    RelocFnTable :LPRelocFunctionTable;
    Base :Pointer;
    Count :LongInt;
    MaxCount :LongInt;
    ItemSize  :LongInt;
    CapacitySize :LongInt; 
  end;

function Array_Create(RelocFnTable :LPRelocFunctionTable; ItemSize, MaxItemCount :LongInt): PSimArrayObject; Inline;
function Array_Append(SimArray :PSimArrayObject):Pointer; Inline;
function Array_Item(SimArray :PSimArrayObject; Index :LongInt):Pointer; Inline;
procedure Array_Destroy(SimArray :PSimArrayObject); Inline;

implementation

function Array_Create(RelocFnTable :LPRelocFunctionTable; ItemSize, MaxItemCount :LongInt): PSimArrayObject;
begin
  Result := RelocFnTable.VirtualAlloc(nil,  SizeOf(TSimArrayObject), MEM_COMMIT,PAGE_READWRITE);
  Result.CapacitySize := MaxItemCount * ItemSize;
  Result.MaxCount := MaxItemCount;
  Result.Base := RelocFnTable.VirtualAlloc(nil,  Result.CapacitySize, MEM_COMMIT,PAGE_READWRITE);
  FillChar(Result.Base^, Result.CapacitySize, #0);
  Result.Count := 0;
  Result.ItemSize := ItemSize;
  Result.RelocFnTable := RelocFnTable;
end;

procedure Array_Destroy(SimArray :PSimArrayObject); inline;
begin
  SimArray.RelocFnTable.VirtualFree(SimArray.Base,0,MEM_RELEASE);
  SimArray.RelocFnTable.VirtualFree(SimArray,0,MEM_RELEASE);
end;

procedure Array_Grow(SimArray :PSimArrayObject); inline
var
  NewBase :Pointer;
begin
  NewBase := SimArray.RelocFnTable.VirtualAlloc(nil,  SimArray.CapacitySize * 2, MEM_COMMIT,PAGE_READWRITE);
  FillChar(NewBase^, SimArray.CapacitySize * 2, #0);
  CopyMemory(NewBase, SimArray.Base, SimArray.Count * SimArray.ItemSize);
  SimArray.RelocFnTable.VirtualFree(SimArray.Base,0,MEM_RELEASE);

  SimArray.Base := NewBase;
  SimArray.MaxCount := SimArray.MaxCount * 2;
  SimArray.CapacitySize := SimArray.MaxCount * SimArray.ItemSize;
end;

function Array_Append(SimArray :PSimArrayObject):Pointer;
begin
  if SimArray.Count >= SimArray.MaxCount then
  begin
    Array_Grow(SimArray);
  end;
  Result := Pointer(LongInt(SimArray.Base) + SimArray.Count * SimArray.ItemSize);
  FillChar(Result^, SimArray.ItemSize, #0);
  Inc(SimArray.count);
end;

function Array_Item(SimArray :PSimArrayObject; Index :LongInt):Pointer;
begin
  Result := NIL;
  if Index >= SimArray.Count then exit;

  Result := Pointer( LongInt(SimArray.Base) + (Index * SimArray.ItemSize));
end;

end.
