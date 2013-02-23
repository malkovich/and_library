//*****************************************************************//
//                                                                 //
//  TIntList Class                                                 //
//  Copyright?BrandsPatch LLC                                     //
//  http://www.explainth.at                                        //
//                                                                 //
//  All Rights Reserved                                            //
//                                                                 //
//  Permission is granted to use, modify and redistribute          //
//  the code in this Delphi unit on the condition that this        //
//  notice is retained unchanged.                                  //
//                                                                 //
//  BrandsPatch  declines all responsibility for any losses,       //
//  direct or indirect, that may arise  as a result of using       //
//  this code.                                                     //
//                                                                 //
//*****************************************************************//
{
Delphi:TIntList
TIntList is a very easy class to create. We take advantage of the fact that the Delphi TList object is designed to store 32 bit pointers and cheat it into storing 32 bit integers instead - after all, internally the two are exactly identical. TIntList has two properties

IntCount: Integer - The number of integers in the list.
Integers[Index: Integer] :Integer - The array of integer values stored in the list. This is the default property.
The principal methods of TIntList are listed below

constructor CreateEx - This is the object constructor. Internally, it creates an instance of TList used to store integer values after typecasting them to a pointer.
procedure ReadFromStream( AStream: TStream ) - Reads in integer values previously streamed out to memory or file.
procedure WriteToStream( AStream: TStream ) - Writes integer values to memory or file.
function Add( Value: Integer ) :Integer - Add the specified integer value to the list. Returns the number of entries in the list after addition.
procedure ClearEx( ACount: Integer ) - Empties the list and prepares it to accept ACount new entries.
function Decrement( Index: Integer ) :Boolean - Decrements the integer stored at Index by unity. Returns True if the post decrement value is zero. Raises an EListError exception if Index is invalid.
procedure Delete( Index: Integer ) - Deletes the entry at Index. Does nothing if Index is invalid.
function Discard( Value: Integer ) :Boolean - Finds and discards the first entry for Value in the list. Returns False if Value does not exist.
function Find( Value: Integer ) :Integer - Finds the first occurrence of Value in the list and returns its index. Returns -1 if the list does not contain Value.
procedure Increment( Index: Integer ) - Increments the value at Index by unity. Does nothing if Index is invalid.
procedure Replicate( AList: TIntList ) - Discards the current list contents and replaces them with a copy of those in AList.
Usage: Create an instance of TIntList whenever you need to store a sequence of related integers. Use TIntList methods and properties to manage your entries. Remember to free the instance once you are done.
}
unit IntList2;

interface

uses Windows,SysUtils,Classes;

type WordInt = SmallInt;

type TIntList = class(TObject)
private
  FList:TList;
  function GetCount:Integer;
  function GetInteger(Index:Integer):Integer;
  procedure SetInteger(Index,Value:Integer);
public
  property IntCount:Integer read GetCount;
  property Integers[Index:Integer]:Integer read GetInteger write SetInteger;default;
  constructor CreateEx;
  destructor Destroy;override;
  procedure ReadFromStream(AStream:TStream);
  procedure WriteToStream(AStream:TStream);
  function Add(Value:Integer):Integer;
  procedure ClearEx(ACount:Integer);
  function Decrement(Index:Integer):Boolean;
  procedure Delete(Index:Integer);
  function Discard(Value:Integer):Integer;
  procedure Exchange(Index1,Index2:Integer);
  function Find(Value:Integer):Integer;
  procedure Increment(Index:Integer);
  function Insert(Index,Value:Integer):Integer;
  procedure Pack;
  function Remove(Index:Integer):Boolean;
  procedure Replicate(AList:TIntList);
  procedure Sort;
end;

implementation

constructor TIntList.CreateEx;
begin
  inherited Create;
  FList:=TList.Create;
end;

destructor TIntList.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TIntList.ReadFromStream(AStream:TStream);
var ACount:WordInt;
    i,j:Integer;
begin
  with FList,AStream do
  begin
    Clear;//empty the current contents of the list
    ReadBuffer(ACount,sizeof(ACount));
    //first read the number of entries in the list
    Capacity:=ACount;
    for i:=1 to ACount do
    begin
      ReadBuffer(j,sizeof(Integer));
      Add(Pointer(j));
      //read in each entry and add it to the list
    end;
  end;
end;

procedure TIntList.WriteToStream(AStream:TStream);
var i:WordInt;
    j:Integer;
begin
  i:=FList.Count;
  with AStream,FList do
  begin
    i:=Count;
    WriteBuffer(i,sizeof(i));
    //first write the number of entries in the list
    for i:=0 to Count - 1 do
    begin
      j:=Integer(Items[i]);
      WriteBuffer(j,sizeof(j));
      //then write out each entry
    end;
  end;
end;

function TIntList.Add(Value:Integer):Integer;
begin
  Result:=FList.Add(Pointer(Value));
end;

procedure TIntList.ClearEx(ACount:Integer);
begin
  with FList do
  begin
    Clear;
    Capacity:=ACount;
  end;
end;

function TIntList.Decrement(Index:Integer):Boolean;
begin
  with FList do if ((Index >= 0) and (Index < Count)) then
  begin
    Items[Index]:=Pointer(Integer(Items[Index]) - 1);
    Result:=(Integer(FList[Index]) = 0);
  end else raise EListError.Create('Invalid List Index');
end;

procedure TIntList.Delete(Index:Integer);
begin
  with FList do
  begin
    if (Index < 0) or (Index >= Count) then exit;
    Delete(Index);
  end;
end;

function TIntList.Discard(Value:Integer):Integer;
begin
  Result:=Find(Value);
  if (Result >= 0) then Delete(Result);
end;

procedure TIntList.Exchange(Index1,Index2:Integer);
begin
  FList.Exchange(Index1,Index2);
end;

function TIntList.Find(Value:Integer):Integer;
begin
  with FList do
  for Result:=0 to Count - 1 do if (Integer(Items[Result]) = Value) then exit;
  Result:=-1;
end;

procedure TIntList.Increment(Index:Integer);
begin
  with FList do
  if ((Index >= 0) and (Index < Count)) then
  Items[Index]:=Pointer(Integer(Items[Index]) + 1);
end;

function TIntList.Insert(Index,Value:Integer):Integer;
begin
  with FList do
  if ((Index >= 0) and (Index < Count)) then
  begin
    Result:=Index;
    Insert(Index,Pointer(Value));
  end else Result:=Add(Pointer(Value));
end;

procedure TIntList.Pack;
begin
  FList.Pack;
end;

function TIntList.Remove(Index:Integer):Boolean;
begin
  with FList do if ((Index >= 0) and (Index < Count)) then
  begin
    Delete(Index);
    Result:=True;
  end else Result:=False;
end;

procedure TIntList.Replicate(AList:TIntList);
var AStream:TStream;
begin
  AStream:=TMemoryStream.Create;
  with AStream do
  try
    AList.WriteToStream(AStream);
    Position:=0;
    ReadFromStream(AStream);
  finally Free end;
  //replicate the contents of AList in the current list
end;


function SortIntegers(P1,P2:Pointer):Integer;
var i:Integer absolute P1;
    j:Integer absolute P2;
begin
  Result:=(i - j);//return -ve, 0 or +ve
end;

procedure TIntList.Sort;
begin
  FList.Sort(SortIntegers);
end;

function TIntList.GetCount:Integer;
begin
  Result:=FList.Count;
end;

function TIntList.GetInteger(Index:Integer):Integer;
begin
  with FList do
  if ((Index >= 0) and (Index < Count)) then Result:=Integer(Items[Index])
  else raise EListError.Create('Invalid list index.');
end;

procedure TIntList.SetInteger(Index,Value:Integer);
begin
  with FList do
  if ((Index >= 0) and (Index < Count)) then Items[Index]:=Pointer(Value) else
  raise EListError.Create('Invalid list index.');
end;

end.
