unit HashTblUnit;

interface
uses  SysUtils;             

function  HashTbl_Create (aTableSize : integer): THandle; stdcall;
procedure HashTbl_Destroy(HashTbl: THandle);stdcall;
procedure HashTbl_Delete (HashTbl: THandle; aKey: PChar);stdcall;
procedure HashTbl_Clear  (HashTbl: THandle);stdcall;
function  HashTbl_Find   (HashTbl: THandle; aKey: PChar; var aItem : pointer): LongBool; stdcall;
procedure HashTbl_Insert (HashTbl: THandle; aKey: PChar; aItem : pointer); stdcall;
function  HashTbl_Count  (HashTbl: THandle): integer; stdcall;
                         
implementation

uses TDHshChn, TDHshBse, TDStkQue;

function  HashTbl_Create (aTableSize : integer): THandle; stdcall;
var
  HashTbl: TtdHashTableChained;
begin
  HashTbl := TtdHashTableChained.Create(aTableSize, TDPJWHash, NIL);
  Result := THandle(HashTbl);
end;

procedure HashTbl_Destroy(HashTbl: THandle);stdcall;
var
  HashTable: TtdHashTableChained absolute HashTbl;
begin
  HashTable.Destroy;
end;

procedure HashTbl_Delete (HashTbl: THandle; aKey: PChar);stdcall;
var
  HashTable: TtdHashTableChained absolute HashTbl;
begin
  HashTable.Delete(StrPas(aKey));
end;

procedure HashTbl_Clear  (HashTbl: THandle);stdcall;
var
  HashTable: TtdHashTableChained absolute HashTbl;
begin
  HashTable.Clear;
end;

function  HashTbl_Find   (HashTbl: THandle; aKey: PChar; var aItem : pointer): LongBool; stdcall;
var
  HashTable: TtdHashTableChained absolute HashTbl;
begin
  Result := HashTable.Find(StrPas(aKey), aItem);
end;

procedure HashTbl_Insert (HashTbl: THandle; aKey: PChar; aItem : pointer); stdcall;
var
  HashTable: TtdHashTableChained absolute HashTbl;
  Item : Pointer;
begin
  if HashTable.Find (aKey, Item) then
    HashTable.Delete (aKey);

  HashTable.Insert(StrPas(aKey), aItem);
end;

function  HashTbl_Count  (HashTbl: THandle): integer; stdcall;
var
  HashTable: TtdHashTableChained absolute HashTbl;
begin
  Result := HashTable.Count;
end;


end.

