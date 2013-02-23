(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDIntDeq                                                         *)
(* An integer deque class                                           *)
(********************************************************************)

unit TDIntDeq;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  Classes,
  TDBasics;

type
  TtdIntDeque = class
    private
      FList : TList;
      FHead : integer;
      FTail : integer;
      FName : TtdNameString;
    protected
      procedure idGrow;
      procedure idError(aErrorCode  : integer;
                  const aMethodName : TtdNameString);
    public
      constructor Create(aCapacity : integer);
      destructor Destroy; override;

      function IsEmpty : boolean;

      procedure Enqueue(aValue : integer);
      procedure Push(aValue : integer);
      function Pop : integer;

      property Name : TtdNameString
         read FName write FName;
  end;

implementation

const
  UnitName = 'TDIntDeq';

{===TtdIntDeque======================================================}
constructor TtdIntDeque.Create(aCapacity : integer);
begin
  inherited Create;
  FList := TList.Create;
  FList.Count := aCapacity;
  {let's help out the user of the deque by putting the head and
   tail pointers in the middle: it's probably more efficient}
  FHead := aCapacity div 2;
  FTail := FHead;
end;
{--------}
destructor TtdIntDeque.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;
{--------}
procedure TtdIntDeque.Enqueue(aValue : integer);
begin
  FList.List^[FTail] := pointer(aValue);
  inc(FTail);
  if (FTail = FList.Count) then
    FTail := 0;
  if (FTail = FHead) then
    idGrow;
end;
{--------}
procedure TtdIntDeque.idError(aErrorCode  : integer;
                        const aMethodName : TtdNameString);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdDequeException.Create(
     FmtLoadStr(aErrorCode,
                [UnitName, ClassName, aMethodName, Name]));
end;
{--------}
procedure TtdIntDeque.idGrow;
var
  OldCount : integer;
  i, j     : integer;
begin
  {grow the list by 50%}
  OldCount := FList.Count;
  FList.Count := (OldCount * 3) div 2;
  {expand the data into the increased space, maintaining the deque}
  if (FHead = 0) then
    FTail := OldCount
  else begin
    j := FList.Count;
    for i := pred(OldCount) downto FHead do begin
      dec(j);
      FList.List^[j] := FList.List^[i]
    end;
    FHead := j;
  end;
end;
{--------}
function TtdIntDeque.IsEmpty : boolean;
begin
  Result := FHead = FTail;
end;
{--------}
procedure TtdIntDeque.Push(aValue : integer);
begin
  if (FHead = 0) then
    FHead := FList.Count;
  dec(FHead);
  FList.List^[FHead] := pointer(aValue);
  if (FTail = FHead) then
    idGrow;
end;
{--------}
function TtdIntDeque.Pop : integer;
begin
  if FHead = FTail then
    idError(tdeDequeIsEmpty, 'Pop');
  Result := integer(FList.List^[FHead]);
  inc(FHead);
  if (FHead = FList.Count) then
    FHead := 0;
end;
{====================================================================}

end.
