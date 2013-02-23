(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDObjLst                                                         *)
(* Object list class                                                *)
(********************************************************************)
               
unit TDObjLst;

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
  TtdObjectList = class
    private
      FClass     : TClass;
      FDataOwner : boolean;
      FList      : TList;
      FName      : TtdNameString;
    protected
      function olGetCapacity : integer;
      function olGetCount : integer;
      function olGetItem(aIndex : integer) : TObject;

      procedure olSetCapacity(aCapacity : integer);
      procedure olSetCount(aCount : integer);
      procedure olSetItem(aIndex : integer;
                          aItem  : TObject);

      procedure olError(aErrorCode  : integer;
                  const aMethodName : TtdNameString;
                        aIndex      : integer);
    public
      constructor Create(aClass : TClass; aDataOwner : boolean);
      destructor Destroy; override;

      function Add(aItem : TObject) : integer;
      procedure Clear;
      procedure Delete(aIndex : integer);
      procedure Exchange(aIndex1, aIndex2 : integer);
      function First : TObject;
      function IndexOf(aItem : TObject) : integer;
      procedure Insert(aIndex : integer; aItem : TObject);
      function Last : TObject;
      procedure Move(aCurIndex, aNewIndex : integer);
      function Remove(aItem : TObject) : integer;
      procedure Sort(aCompare : TtdCompareFunc);

      property Capacity : integer
         read olGetCapacity write olSetCapacity;
      property Count : integer
         read olGetCount write olSetCount;
      property DataOwner : boolean
         read FDataOwner;
      property Items[Index : integer] : TObject
         read olGetItem write olSetItem; default;
      property List : TList
         read FList;
      property Name : TtdNameString
         read FName write FName;
  end;

implementation

const
  UnitName = 'TDObjLst';

{===TtdObjectList===================================================}
constructor TtdObjectList.Create(aClass     : TClass;
                                 aDataOwner : boolean);
begin
  inherited Create;
  {save the class and the data owner flag}
  FClass := aClass;
  FDataOwner := aDataOwner;
  {create the internal list}
  FList := TList.Create;
end;
{--------}
destructor TtdObjectList.Destroy;
begin
  {if the list is assigned, clear it and destroy it}
  if (FList <> nil) then begin
    Clear;
    FList.Destroy;
  end;
  inherited Destroy;
end;
{--------}
function TtdObjectList.Add(aItem : TObject) : integer;
begin
  {test for item class}
  if (aItem = nil) then
    olError(tdeNilItem, 'Add', FList.Count);
  if not (aItem is FClass) then
    olError(tdeInvalidClassType, 'Add', FList.Count);
  {insert the new item at the end of the list}
  Result := FList.Count;
  FList.Insert(Result, aItem);
end;
{--------}
procedure TtdObjectList.Clear;
var
  i : integer;
begin
  {if we own the items present, free them before clearing the list}
  if DataOwner then
    for i := 0 to pred(FList.Count) do
      TObject(FList[i]).Free;
  FList.Clear;
end;
{--------}
procedure TtdObjectList.Delete(aIndex : integer);
begin
  {instead of the list doing it, we'll check the index}
  if (aIndex < 0) or (aIndex >= FList.Count) then
    olError(tdeIndexOutOfBounds, 'Delete', aIndex);
  {if we own the objects, then free the one we're about to delete}
  if DataOwner then
    TObject(FList[aIndex]).Free;
  {delete the item from the list}
  FList.Delete(aIndex);
end;
{--------}
procedure TtdObjectList.Exchange(aIndex1, aIndex2 : integer);
begin
  {instead of the list doing it, we'll check the indexes}
  if (aIndex1 < 0) or (aIndex1 >= FList.Count) then
    olError(tdeIndexOutOfBounds, 'Exchange', aIndex1);
  if (aIndex2 < 0) or (aIndex2 >= FList.Count) then
    olError(tdeIndexOutOfBounds, 'Exchange', aIndex2);
  FList.Exchange(aIndex1, aIndex2);
end;
{--------}
function TtdObjectList.First : TObject;
begin
  Result := TObject(FList.First);
end;
{--------}
function TtdObjectList.IndexOf(aItem : TObject) : integer;
begin
  Result := FList.IndexOf(aItem);
end;
{--------}
procedure TtdObjectList.Insert(aIndex : integer; aItem : TObject);
begin
  {test for item class}
  if (aItem = nil) then
    olError(tdeNilItem, 'Insert', aIndex);
  if not (aItem is FClass) then
    olError(tdeInvalidClassType, 'Insert', aIndex);
  {instead of the list doing it, we'll check the index}
  if (aIndex < 0) or (aIndex > FList.Count) then
    olError(tdeIndexOutOfBounds, 'Insert', aIndex);
  {insert into the list}
  FList.Insert(aIndex, aItem);
end;
{--------}
function TtdObjectList.Last : TObject;
begin
  Result := TObject(FList.Last);
end;
{--------}
procedure TtdObjectList.Move(aCurIndex, aNewIndex : integer);
begin
  {instead of the list doing it, we'll check the indexes}
  if (aCurIndex < 0) or (aCurIndex >= FList.Count) then
    olError(tdeIndexOutOfBounds, 'Move', aCurIndex);
  if (aNewIndex < 0) or (aNewIndex >= FList.Count) then
    olError(tdeIndexOutOfBounds, 'Move', aNewIndex);
  {move the items}
  FList.Move(aCurIndex, aNewIndex);
end;
{--------}
procedure TtdObjectList.olError(aErrorCode  : integer;
                          const aMethodName : TtdNameString;
                                aIndex      : integer);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdArrayException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name, aIndex]));
end;
{--------}
function TtdObjectList.olGetCapacity : integer;
begin
  Result := FList.Capacity;
end;
{--------}
function TtdObjectList.olGetCount : integer;
begin
  Result := FList.Count;
end;
{--------}
function TtdObjectList.olGetItem(aIndex : integer) : TObject;
begin
  {instead of the list doing it, we'll check the index}
  if (aIndex < 0) or (aIndex >= FList.Count) then
    olError(tdeIndexOutOfBounds, 'olGetItem', aIndex);
  {get the item}
  Result := TObject(FList[aIndex]);
end;
{--------}
procedure TtdObjectList.olSetCapacity(aCapacity : integer);
begin
  FList.Capacity := aCapacity;
end;
{--------}
procedure TtdObjectList.olSetCount(aCount : integer);
begin
  FList.Count := aCount;
end;
{--------}
procedure TtdObjectList.olSetItem(aIndex : integer;
                                  aItem  : TObject);
begin
  {test for item class}
  if (aItem = nil) then
    olError(tdeNilItem, 'olSetItem', aIndex);
  if not (aItem is FClass) then
    olError(tdeInvalidClassType, 'olSetItem', aIndex);
  {instead of the list doing it, we'll check the index}
  if (aIndex < 0) or (aIndex >= FList.Count) then
    olError(tdeIndexOutOfBounds, 'olSetItem', aIndex);
  {if we own the objects and we're about to replace the current object
   at this index with another different one, then free the current one
   first}
  if DataOwner and (aItem <> FList[aIndex]) then
    TObject(FList[aIndex]).Free;
  {set the new item}
  FList[aIndex] := aItem;
end;
{--------}
function TtdObjectList.Remove(aItem : TObject) : integer;
begin
  {find the item}
  Result := IndexOf(aItem);
  {if we found it...}
  if (Result <> -1) then begin
    {if we own the objects, free the one about to be deleted}
    if DataOwner then
      TObject(FList[Result]).Free;
    {delete the item}
    FList.Delete(Result);
  end;
end;
{--------}
procedure TtdObjectList.Sort(aCompare : TtdCompareFunc);
begin
  {!!!!requires the sort unit}
end;
{====================================================================}

end.
