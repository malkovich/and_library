(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDStates                                                         *)
(* Different types of state machines                                *)
(********************************************************************)

unit TDStates;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  Classes,
  TDBasics;

{---Extract words from string---}
procedure TDExtractWords(const S : string; aList : TStrings);

{---Extract field values from CSV string---}
procedure TDExtractFields(const S : string; aList : TStrings);

{---Validate a string to be a number---}
function IsValidNumberNFA(const S : string) : boolean;
function IsValidNumber(const S : string) : boolean;

implementation

uses
  TDStkQue;

const
  UnitName = 'TDStates';

{===Extract words from string========================================}
procedure TDExtractWords(const S : string; aList : TStrings);
type
  TStates = (ScanNormal, ScanQuoted, ScanPunctuation);
const
  WordDelim = ' !<>[]{}(),./?;:-+=*&';
var
  State   : TStates;
  Inx     : integer;
  Ch      : AnsiChar;
  CurWord : string;
begin
  {initialize by clearing the string list, and
   starting in ScanNormal state}
  Assert(aList <> nil, 'TDExtractWords: list is nil');
  aList.Clear;
  State := ScanNormal;
  CurWord := '';
  {read through all the characters in the string}
  for Inx := 1 to length(S) do begin
    {get the next character}
    Ch := S[Inx];
    {switch processing on the state}
    case State of
      ScanNormal :
        begin
          if (Ch = '"') then begin
            if (CurWord <> '') then
              aList.Add(CurWord);
            CurWord := '"';
            State := ScanQuoted;
          end
          else if (TDPosCh(Ch, WordDelim) <> 0) then begin
            if (CurWord <> '') then begin
              aList.Add(CurWord);
              CurWord := '';
            end;
            State := ScanPunctuation;
          end
          else
            CurWord := CurWord + Ch;
        end;
      ScanQuoted :
        begin
          CurWord := CurWord + Ch;
          if (Ch = '"') then begin
            aList.Add(CurWord);
            CurWord := '';
            State := ScanNormal;
          end;
        end;
      ScanPunctuation :
        begin
          if (Ch = '"') then begin
            CurWord := '"';
            State := ScanQuoted;
          end
          else if (TDPosCh(Ch, WordDelim) = 0) then begin
            CurWord := Ch;
            State := ScanNormal;
          end
        end;
    end;
  end;
  {if we are in the ScanQUoted state at the end of the
   string, there was a mismatched double quote}
  if (State = ScanQuoted) then
    raise EtdStateException.Create(
       FmtLoadStr(tdeStateMisMatchQuote,
                  [UnitName, 'TDExtractWords']));
  {if the current word is not empty, add it to the list}
  if (CurWord <> '') then
    aList.Add(CurWord);
end;
{====================================================================}


{===Extract field values from CSV string=============================}
procedure TDExtractFields(const S : string; aList : TStrings);
type
  TStates = (FieldStart, ScanField, ScanQuoted, EndQuoted, GotError);
var
  State   : TStates;
  Inx     : integer;
  Ch      : AnsiChar;
  CurField: string;
begin
  {initialize by clearing the string list, and
   starting in FieldStart state}
  Assert(aList <> nil, 'TDExtractFields: list is nil');
  aList.Clear;
  State := FieldStart;
  CurField := '';
  {read through all the characters in the string}
  for Inx := 1 to length(S) do begin
    {get the next character}
    Ch := S[Inx];
    {switch processing on the state}
    case State of
      FieldStart :
        begin
          case Ch of
            '"' :
              begin
                State := ScanQuoted;
              end;
            ',' :
              begin
                aList.Add('');
              end;
          else
            CurField := Ch;
            State := ScanField;
          end;
        end;
      ScanField :
        begin
          if (Ch = ',') then begin
            aList.Add(CurField);
            CurField := '';
            State := FieldStart;
          end
          else
            CurField := CurField + Ch;
        end;
      ScanQuoted :
        begin
          if (Ch = '"') then
            State := EndQuoted
          else
            CurField := CurField + Ch;
        end;
      EndQuoted :
        begin
          if (Ch = ',') then begin
            aList.Add(CurField);
            CurField := '';
            State := FieldStart;
          end
          else
            State := GotError;
        end;
      GotError :
        begin
          raise EtdStateException.Create(
             FmtLoadStr(tdeStateBadCSV,
                        [UnitName, 'TDExtractFields']));
        end;
    end;
  end;
  {if we are in the ScanQUoted or GotError state at the end
   of the string, there was a problem with a closing quote}
  if (State = ScanQuoted) or (State = GotError) then
    raise EtdStateException.Create(
       FmtLoadStr(tdeStateBadCSV,
                  [UnitName, 'TDExtractFields']));
  {if the current field is not empty, add it to the list}
  if (CurField <> '') then
    aList.Add(CurField);
end;
{====================================================================}


{===Validating a string to be a number===============================}
function IsValidNumber(const S : string) : boolean;
type
  TStates = (StartState, GotSign,
             GotInitDigit, GotInitDecPt, ScanDigits);
var
  State   : TStates;
  Inx     : integer;
  Ch      : AnsiChar;
begin
  {assume the string is not a valid number}
  Result := false;
  {prepare for the scan loop}
  State := StartState;
  {read all the characters in the string}
  for Inx := 1 to length(S) do begin
    {get the current character}
    Ch := S[Inx];
    {switch processing on state}
    case State of
      StartState :
        begin
          if (Ch = '+') or (Ch = '-') then
            State := GotSign
          else if (Ch = DecimalSeparator) then
            State := GotInitDecPt
          else if TDIsdigit(Ch) then
            State := GotInitDigit
          else
            Exit;
        end;
      GotSign :
        begin
          if (Ch = DecimalSeparator) then
            State := GotInitDecPt
          else if TDIsDigit(Ch) then
            State := GotInitDigit
          else
            Exit;
        end;
      GotInitDigit :
        begin
          if (Ch = DecimalSeparator) then
            State := ScanDigits
          else if not TDIsDigit(Ch) then
            Exit;
        end;
      GotInitDecPt :
        begin
          if TDIsDigit(Ch) then
            State := ScanDigits
          else
            Exit;
        end;
      ScanDigits :
        begin
          if not TDIsDigit(Ch) then
            Exit;
        end;
    end;
  end;
  {if we reach this point, the number is valid if we're in a
   terminating state}
  if (State = GotInitDigit) or
     (State = ScanDigits) then
    Result := true;
end;
{--------}
type
  TnfaState = (StartScanning,               {state A in figure}
            ScannedSign,                 {state B in figure}
            ScanInteger,                 {state C in figure}
            ScanLeadDigits,              {state D in figure}
            ScannedDecPoint,             {state E in figure}
            ScanLeadDecPoint,            {state F in figure}
            ScanDecimalDigits);          {state G in figure}

  PnfaChoice = ^TnfaChoice;
  TnfaChoice = packed record
    chInx   : integer;
    chMove  : integer;
    chState : TnfaState;
  end;
{--------}
procedure DisposeChoice(aData : pointer); far;
begin
  if (aData <> nil) then
    Dispose(PnfaChoice(aData));
end;
{--------}
procedure PushChoice(aStack : TtdStack;
                     aInx   : integer;
                     aMove  : integer;
                     aState : TnfaState);
var
  Choice : PnfaChoice;
begin
  New(Choice);
  Choice^.chInx := aInx;
  Choice^.chMove := aMove;
  Choice^.chState := aState;
  aStack.Push(Choice);
end;
{--------}
procedure PopChoice(aStack : TtdStack;
                var aInx   : integer;
                var aMove  : integer;
                var aState : TnfaState);
var
  Choice : PnfaChoice;
begin
  Choice := PnfaChoice(aStack.Pop);
  aInx := Choice^.chInx;
  aMove := Choice^.chMove;
  aState := Choice^.chState;
  Dispose(Choice);
end;
{--------}
function IsValidNumberNFA(const S : string) : boolean;
var
  StrInx: integer;
  State : TnfaState;
  Ch    : AnsiChar;
  Move  : integer;
  ChoiceStack : TtdStack;
begin
  {assume the number is invalid}
  Result := false;
  {initialize the choice stack}
  ChoiceStack := TtdStack.Create(DisposeChoice);
  try
    {prepare for scanning}
    Move := 0;
    StrInx := 1;
    State := StartScanning;
    {read through all the characters in the string}
    while StrInx <= length(S) do begin
      {get the current character}
      Ch := S[StrInx];
      {switch processing based on state}
      case State of
        StartScanning :
          begin
            case Move of
              0 : {move to ScannedSign with +}
                begin
                  if (Ch = '+') then begin
                    PushChoice(ChoiceStack, StrInx, Move, State);
                    State := ScannedSign;
                    Move := 0;
                    inc(StrInx);
                  end
                  else
                    inc(Move);
                end;
              1 : {move to ScannedSign with -}
                begin
                  if (Ch = '-') then begin
                    PushChoice(ChoiceStack, StrInx, Move, State);
                    State := ScannedSign;
                    Move := 0;
                    inc(StrInx);
                  end
                  else
                    inc(Move);
                end;
              2 : {no-cost move to ScannedSign}
                begin
                  PushChoice(ChoiceStack, StrInx, Move, State);
                  State := ScannedSign;
                  Move := 0;
                end;
            else
              {we've run out of moves for this state}
              Move := -1;
            end;
          end;
        ScannedSign :
          begin
            case Move of
              0 : {move to ScanInteger with digit}
                begin
                  if TDIsDigit(Ch) then begin
                    PushChoice(ChoiceStack, StrInx, Move, State);
                    State := ScanInteger;
                    Move := 0;
                    inc(StrInx);
                  end
                  else
                    inc(Move);
                end;
              1 : {move to ScanLeadDigits with digit}
                begin
                  if TDIsDigit(Ch) then begin
                    PushChoice(ChoiceStack, StrInx, Move, State);
                    State := ScanLeadDigits;
                    Move := 0;
                    inc(StrInx);
                  end
                  else
                    inc(Move);
                end;
              2 : {move to ScanLeadDigits with decimal separator}
                begin
                  if (Ch = DecimalSeparator) then begin
                    PushChoice(ChoiceStack, StrInx, Move, State);
                    State := ScanLeadDecPoint;
                    Move := 0;
                    inc(StrInx);
                  end
                  else
                    inc(Move);
                end;
            else
              {we've run out of moves for this state}
              Move := -1;
            end;
          end;
        ScanInteger :
          begin
            case Move of
              0 : {stay in same state with digit}
                begin
                  if TDIsDigit(Ch) then
                    inc(StrInx)
                  else
                    inc(Move);
                end;
            else
              {we've run out of moves for this state}
              Move := -1;
            end;
          end;
        ScanLeadDigits :
          begin
            case Move of
              0 : {stay in same state with digit}
                begin
                  if TDIsDigit(Ch) then
                    inc(StrInx)
                  else
                    inc(Move);
                end;
              1 : {move to ScannedDecPoint with decimal separator}
                begin
                  if (Ch = DecimalSeparator) then begin
                    PushChoice(ChoiceStack, StrInx, Move, State);
                    State := ScannedDecPoint;
                    Move := 0;
                    inc(StrInx);
                  end
                  else
                    inc(Move);
                end;
            else
              {we've run out of moves for this state}
              Move := -1;
            end;
          end;
        ScannedDecPoint :
          begin
            case Move of
              0 : {stay in same state with digit}
                begin
                  if TDIsDigit(Ch) then
                    inc(StrInx)
                  else
                    inc(Move);
                end;
            else
              {we've run out of moves for this state}
              Move := -1;
            end;
          end;
        ScanLeadDecPoint :
          begin
            case Move of
              0 : {move to ScannedDecPoint with digit}
                begin
                  if TDIsDigit(Ch) then begin
                    PushChoice(ChoiceStack, StrInx, Move, State);
                    State := ScanDecimalDigits;
                    Move := 0;
                    inc(StrInx);
                  end
                  else
                    inc(Move);
                end;
            else
              {we've run out of moves for this state}
              Move := -1;
            end;
          end;
        ScanDecimalDigits :
          begin
            case Move of
              0 : {stay in same state with digit}
                begin
                  if TDIsDigit(Ch) then
                    inc(StrInx)
                  else
                    inc(Move);
                end;
            else
              {we've run out of moves for this state}
              Move := -1;
            end;
          end;
      end;
      {if we've run out of moves for a particular state, backtrack by
       popping off the topmost choice, and incrementing the move}
      if (Move = -1) then begin
        {if the stack is empty, there is no more backtracking}
        if ChoiceStack.IsEmpty then
          Exit;
        {pop the top choice, advance on by one move}
        PopChoice(ChoiceStack, StrInx, Move, State);
        inc(Move);
      end;
    end;
    {if we reach this point, the number is valid if we're in a
     terminating state}
    if (State = ScanInteger) or
       (State = ScannedDecPoint) or
       (State = ScanDecimalDigits) then
      Result := true;
  finally
    ChoiceStack.Free;
  end;
end;
{====================================================================}

end.
