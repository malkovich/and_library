(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDLCS                                                            *)
(* Longest common subsequence                                       *)
(********************************************************************)

unit TDLCS;

{$I TDDefine.inc}

{define this if you want to use the debug facilities}
{$DEFINE Debug}

interface

uses
  SysUtils,
  Classes,
  TDBasics;

type
  TtdLCSDir = (
     ldNorth,
     ldNorthWest,
     ldWest);

  PtdLCSData = ^TtdLCSData;
  TtdLCSData = packed record
    ldLen  : integer;
    ldPrev : TtdLCSDir;
  end;

  TtdLCSMatrix = class
    private
      FCols     : integer;
      FMatrix   : TList;
      FRows     : integer;
    protected
      function mxGetItem(aRow, aCol : integer) : PtdLCSData;
      procedure mxSetItem(aRow, aCol : integer;
                          aValue : PtdLCSData);
    public
      constructor Create(aRowCount, aColCount : integer);
      destructor Destroy; override;

      procedure Clear;

      {$IFDEF Debug}
      procedure Dump;
      {$ENDIF}

      property Items[aRow, aCol : integer] : PtdLCSData
                  read mxGetItem write mxSetItem; default;

      property RowCount : integer read FRows;
      property ColCount : integer read FCols;
  end;

  TtdStringLCS = class
    private
      FFromStr : string;
      FMatrix  : TtdLCSMatrix;
      FToStr   : string;
    protected
      procedure slFillMatrix;
      function slGetCell(aFromInx, aToInx : integer) : integer;
      procedure slWriteChange(var F : System.Text;
                                  aFromInx, aToInx : integer);
    public
      constructor Create(const aFromStr, aToStr : string);
      destructor Destroy; override;

      procedure WriteChanges(const aFileName : string);
  end;

  TtdFileLCS = class
    private
      FFromFile : TStringList;
      FMatrix   : TtdLCSMatrix;
      FToFile   : TStringList;
    protected
      function slGetCell(aFromInx, aToInx : integer) : integer;
      procedure slWriteChange(var F : System.Text;
                                  aFromInx, aToInx : integer);
    public
      constructor Create(const aFromFile, aToFile : string);
      destructor Destroy; override;

      procedure WriteChanges(const aFileName : string);
  end;

implementation

{===TtdLCSMatrix=====================================================}
constructor TtdLCSMatrix.Create(aRowCount, aColCount : integer);
var
  Row     : integer;
  ColList : TList;
begin
  {create the ancestor}
  inherited Create;

  {simple validation}
  if (aRowCount <= 0) or (aColCount < 0) then
    raise Exception.Create(
                  'TtdLCSMatrix.Create: Invalid Row or column count');
  FRows := aRowCount;
  FCols := aColCount;

  {create the matrix: it'll be a TList of TLists in row order}
  FMatrix := TList.Create;
  FMatrix.Count := aRowCount;
  for Row := 0 to pred(aRowCount) do begin
    ColList := TList.Create;
    ColList.Count := aColCount;
    TList(FMatrix.List^[Row]) := ColList;
  end;
end;
{--------}
destructor TtdLCSMatrix.Destroy;
var
  Row : integer;
begin
  {destroy the matrix}
  if (FMatrix <> nil) then begin
    Clear;
    for Row := 0 to pred(FRows) do
      TList(FMatrix.List^[Row]).Free;
    FMatrix.Free;
  end;

  {destroy the ancestor}
  inherited Destroy;
end;
{--------}
procedure TtdLCSMatrix.Clear;
var
  Row, Col : integer;
  ColList  : TList;
begin
  for Row := 0 to pred(FRows) do begin
    ColList := TList(FMatrix.List^[Row]);
    if (ColList <> nil) then
      for Col := 0 to pred(FCols) do begin
        if (ColList.List^[Col] <> nil) then
          Dispose(PtdLCSData(ColList.List^[Col]));
        ColList.List^[Col] := nil;
      end;
  end;
end;
{--------}
{$IFDEF Debug}
procedure TtdLCSMatrix.Dump;
var
  Row, Col : integer;
  LCSData  : PtdLCSData;
begin
  for Row := 0 to pred(FRows) do begin
    for Col := 0 to pred(FCols) do begin
      LCSData := Items[Row, Col];
      if (LCSData = nil) then begin
        write('  ? 0');
      end
      else begin
        case LCSData^.ldPrev of
          ldNorth     : write('  |');
          ldNorthWest : write('  \');
          ldWest      : write('  -');
        end;
        write(LCSData^.ldLen:2);
      end;
    end;
    writeln;
  end;
end;
{$ENDIF}
{--------}
function TtdLCSMatrix.mxGetItem(aRow, aCol : integer) : PtdLCSData;
begin
  if not ((0 <= aRow) and (aRow < RowCount) and
          (0 <= aCol) and (aCol < ColCount)) then
    raise Exception.Create(
         'TtdLCSMatrix.mxGetItem: Row or column index out of bounds');
  Result := PtdLCSData(TList(FMatrix.List^[aRow]).List^[aCol]);
end;
{--------}
procedure TtdLCSMatrix.mxSetItem(aRow, aCol : integer;
                    aValue : PtdLCSData);
begin
  if not ((0 <= aRow) and (aRow < RowCount) and
          (0 <= aCol) and (aCol < ColCount)) then
    raise Exception.Create(
         'TtdLCSMatrix.mxSetItem: Row or column index out of bounds');
  TList(FMatrix.List^[aRow]).List^[aCol] := aValue;
end;
{====================================================================}


{===TtdStringLCS=====================================================}
constructor TtdStringLCS.Create(const aFromStr, aToStr : string);
begin
  {create the ancestor}
  inherited Create;

  {save the strings}
  FFromStr := aFromStr;
  FToStr := aToStr;

  {create the matrix}
  FMatrix := TtdLCSMatrix.Create(succ(length(aFromStr)),
                                 succ(length(aToStr)));

  {now fill in the matrix}
  slGetCell(length(aFromStr), length(aToStr));
(*slFillMatrix;*)

  {$IFDEF Debug}
  writeln('Matrix for ', aFromStr, ' -> ', aToStr);
  FMatrix.Dump;
  {$ENDIF}
end;
{--------}
destructor TtdStringLCS.Destroy;
begin
  {destroy the matrix}
  FMatrix.Free;

  {destroy the ancestor}
  inherited Destroy;
end;
{--------}
procedure TtdStringLCS.slFillMatrix;
var
  FromInx : integer;
  ToInx   : integer;
  NorthLen: integer;
  WestLen : integer;
  LCSData : PtdLCSData;
begin
  {Create the empty items along the top and left sides}
  for ToInx := 0 to length(FToStr) do begin
    New(LCSData);
    LCSData^.ldLen := 0;
    LCSData^.ldPrev := ldWest;
    FMatrix[0, ToInx] := LCSData;
  end;
  for FromInx := 1 to length(FFromStr) do begin
    New(LCSData);
    LCSData^.ldLen := 0;
    LCSData^.ldPrev := ldNorth;
    FMatrix[FromInx, 0] := LCSData;
  end;

  {fill in the matrix, row by row, from left to right}
  for FromInx := 1 to length(FFromStr) do begin
    for ToInx := 1 to length(FToStr) do begin
      {create the new item}
      New(LCSData);

      {if the two current chars are equal, increment the count
      from the northwest, that's our previous item}
      if (FFromStr[FromInx] = FToStr[ToInx]) then begin
        LCSData^.ldPrev := ldNorthWest;
        LCSData^.ldLen := succ(FMatrix[FromInx-1, ToInx-1]^.ldLen);
      end

      {otherwise the current characters are different: use the
      maximum of the north or west (west preferred}
      else begin
        NorthLen := FMatrix[FromInx-1, ToInx]^.ldLen;
        WestLen := FMatrix[FromInx, ToInx-1]^.ldLen;
        if (NorthLen > WestLen) then begin
          LCSData^.ldPrev := ldNorth;
          LCSData^.ldLen := NorthLen;
        end
        else begin
          LCSData^.ldPrev := ldWest;
          LCSData^.ldLen := WestLen;
        end;
      end;

      {set the item in the matrix}
      FMatrix[FromInx, ToInx] := LCSData;
    end;
  end;
  {at this point the item in the bottom right hand corner has
   the length of the LCS and the calculation is complete}
end;
{--------}
function TtdStringLCS.slGetCell(aFromInx, aToInx : integer) : integer;
var
  LCSData : PtdLCSData;
  NorthLen: integer;
  WestLen : integer;
begin
  if (aFromInx = 0) or (aToInx = 0) then
    Result := 0
  else begin
    LCSData := FMatrix[aFromInx, aToInx];
    if (LCSData <> nil) then
      Result := LCSData^.ldLen
    else begin
      {create the new item}
      New(LCSData);

      {if the two current chars are equal, increment the count
      from the northwest, that's our previous item}
      if (FFromStr[aFromInx] = FToStr[aToInx]) then begin
        LCSData^.ldPrev := ldNorthWest;
        LCSData^.ldLen := slGetCell(aFromInx-1, aToInx-1) + 1;
      end

      {otherwise the current characters are different: use the
      maximum of the north or west (west preferred}
      else begin
        NorthLen := slGetCell(aFromInx-1, aToInx);
        WestLen := slGetCell(aFromInx, aToInx-1);
        if (NorthLen > WestLen) then begin
          LCSData^.ldPrev := ldNorth;
          LCSData^.ldLen := NorthLen;
        end
        else begin
          LCSData^.ldPrev := ldWest;
          LCSData^.ldLen := WestLen;
        end;
      end;

      {set the item in the matrix}
      FMatrix[aFromInx, aToInx] := LCSData;

      {return the length of this LCS}
      Result := LCSData^.ldLen;
    end;
  end;
end;
{--------}
procedure TtdStringLCS.slWriteChange(var F : System.Text;
                                         aFromInx, aToInx : integer);
var
  Cell : PtdLCSData;
begin
  {if both indexes are zero, this is the first
   cell of the LCS matrix, so just exit}
  if (aFromInx = 0) and (aToInx = 0) then
    Exit;

  {if the from index is zero, we're flush against the left
   hand side of the matrix, so go up; this'll be a deletion}
  if (aFromInx = 0) then begin
    slWriteChange(F, aFromInx, aToInx-1);
    writeln(F, '-> ', FToStr[aToInx]);
  end
  {if the to index is zero, we're flush against the top side
   of the matrix, so go left; this'll be an insertion}
  else if (aToInx = 0) then begin
    slWriteChange(F, aFromInx-1, aToInx);
    writeln(F, '<- ', FFromStr[aFromInx]);
  end
  {otherwise see what the cell says to do}
  else begin
    Cell := FMatrix[aFromInx, aToInx];
    case Cell^.ldPrev of
      ldNorth :
        begin
          slWriteChange(F, aFromInx-1, aToInx);
          writeln(F, '<- ', FFromStr[aFromInx]);
        end;
      ldNorthWest :
        begin
          slWriteChange(F, aFromInx-1, aToInx-1);
          writeln(F, '   ', FFromStr[aFromInx]);
        end;
      ldWest :
        begin
          slWriteChange(F, aFromInx, aToInx-1);
          writeln(F, '-> ', FToStr[aToInx]);
        end;
    end;
  end;
end;
{--------}
procedure TtdStringLCS.WriteChanges(const aFileName : string);
var
  F : System.Text;
begin
  System.Assign(F, aFileName);
  System.Rewrite(F);
  try
    slWriteChange(F, length(FFromStr), length(FToStr));
  finally
    System.Close(F);
  end;
end;
{====================================================================}


{===TtdFileLCS=====================================================}
constructor TtdFileLCS.Create(const aFromFile, aToFile : string);
begin
  {create the ancestor}
  inherited Create;

  {read the files}
  FFromFile := TStringList.Create;
  FFromFile.LoadFromFile(aFromFile);
  FToFile := TStringList.Create;
  FToFile.LoadFromFile(aToFile);

  {create the matrix}
  FMatrix := TtdLCSMatrix.Create(FFromFile.Count, FToFile.Count);

  {now fill in the matrix}
  slGetCell(pred(FFromFile.Count), pred(FToFile.Count));
end;
{--------}
destructor TtdFileLCS.Destroy;
begin
  {destroy the matrix}
  FMatrix.Free;

  {free the string lists}
  FFromFile.Free;
  FToFile.Free;

  {destroy the ancestor}
  inherited Destroy;
end;
{--------}
function TtdFileLCS.slGetCell(aFromInx, aToInx : integer) : integer;
var
  LCSData : PtdLCSData;
  NorthLen: integer;
  WestLen : integer;
begin
  if (aFromInx = -1) or (aToInx = -1) then
    Result := 0
  else begin
    LCSData := FMatrix[aFromInx, aToInx];
    if (LCSData <> nil) then
      Result := LCSData^.ldLen
    else begin
      {create the new item}
      New(LCSData);

      {if the two current lines are equal, increment the count
      from the northwest, that's our previous item}
      if (FFromFile[aFromInx] = FToFile[aToInx]) then begin
        LCSData^.ldPrev := ldNorthWest;
        LCSData^.ldLen := slGetCell(aFromInx-1, aToInx-1) + 1;
      end

      {otherwise the current lines are different: use the
      maximum of the north or west (west preferred}
      else begin
        NorthLen := slGetCell(aFromInx-1, aToInx);
        WestLen := slGetCell(aFromInx, aToInx-1);
        if (NorthLen > WestLen) then begin
          LCSData^.ldPrev := ldNorth;
          LCSData^.ldLen := NorthLen;
        end
        else begin
          LCSData^.ldPrev := ldWest;
          LCSData^.ldLen := WestLen;
        end;
      end;

      {set the item in the matrix}
      FMatrix[aFromInx, aToInx] := LCSData;

      {return the length of this LCS}
      Result := LCSData^.ldLen;
    end;
  end;
end;
{--------}
procedure TtdFileLCS.slWriteChange(var F : System.Text;
                                         aFromInx, aToInx : integer);
var
  Cell : PtdLCSData;
begin
  {if both indexes are less than zero, this is the first
   cell of the LCS matrix, so just exit}
  if (aFromInx = -1) and (aToInx = -1) then
    Exit;

  {if the from index is less than zero, we're flush against the
   left hand side of the matrix, so go up; this'll be a deletion}
  if (aFromInx = -1) then begin
    slWriteChange(F, aFromInx, aToInx-1);
    writeln(F, '-> ', FToFile[aToInx]);
  end
  {if the to index is less than zero, we're flush against the
   top side of the matrix, so go left; this'll be an insertion}
  else if (aToInx = -1) then begin
    slWriteChange(F, aFromInx-1, aToInx);
    writeln(F, '<- ', FFromFile[aFromInx]);
  end
  {otherwise see what the cell says to do}
  else begin
    Cell := FMatrix[aFromInx, aToInx];
    case Cell^.ldPrev of
      ldNorth :
        begin
          slWriteChange(F, aFromInx-1, aToInx);
          writeln(F, '<- ', FFromFile[aFromInx]);
        end;
      ldNorthWest :
        begin
          slWriteChange(F, aFromInx-1, aToInx-1);
          writeln(F, '   ', FFromFile[aFromInx]);
        end;
      ldWest :
        begin
          slWriteChange(F, aFromInx, aToInx-1);
          writeln(F, '-> ', FToFile[aToInx]);
        end;
    end;
  end;
end;
{--------}
procedure TtdFileLCS.WriteChanges(const aFileName : string);
var
  F : System.Text;
begin
  System.Assign(F, aFileName);
  System.Rewrite(F);
  try
    slWriteChange(F, pred(FFromFile.Count), pred(FToFile.Count));
  finally
    System.Close(F);
  end;
end;
{====================================================================}

end.
