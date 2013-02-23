(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDRegex                                                          *)
(* Regular expression parser and compiler                           *)
(********************************************************************)

unit TDRegex;

{$I TDDefine.inc}

interface

{Notes:
  these classes parse regular expressions that follow this grammar:

    <anchorexpr> ::= <expr> |
                     '^' <expr> |
                     <expr> '$' |
                     '^' <expr> '$'
    <expr> ::= <term> |
               <term> '|' <expr>                 - alternation
    <term> ::= <factor> |
               <factor><term>                    - concatenation
    <factor> ::= <atom> |
                 <atom> '?' |                    - zero or one
                 <atom> '*' |                    - zero or more
                 <atom> '+'                      - one or more
    <atom> ::= <char> |
               '.' |                             - any char
               '(' <expr> ') |                   - parentheses
               '[' <charclass> ']' |             - normal class
               '[^' <charclass> ']'              - negated class
    <charclass> ::= <charrange> |
                    <charrange><charclass>
    <charrange> ::= <ccchar> |
                    <ccchar> '-' <ccchar>
    <char> ::= <any character except metacharacters> |
               '\' <any character at all>
    <ccchar> ::= <any character except '-' and ']'> |
                 '\' <any character at all>

  This means that parentheses have maximum precedence, followed
  by square brackets, followed by the closure operators,
  followed by concatenation, finally followed by alternation.
}

{turn this compiler define on to log the parsing progress and the
 final transition table; file is regexparse.log in the current folder}
{$DEFINE LogParse}

uses
  SysUtils,
  Classes,
  TDBasics,
  TDRecLst,
  TDIntDeq;

type
  TtdRegexParser = class
    private
      FRegexStr : string;
      {$IFDEF Delphi1}
      FRegexStrZ: PAnsiChar;
      {$ENDIF}
      FPosn     : PAnsiChar;
    protected
      procedure rpParseAtom;
      procedure rpParseCCChar;
      procedure rpParseChar;
      procedure rpParseCharClass;
      procedure rpParseCharRange;
      procedure rpParseExpr;
      procedure rpParseFactor;
      procedure rpParseTerm;
    public
      constructor Create(const aRegexStr : string);
      destructor Destroy; override;

      function Parse(var aErrorPos : integer) : boolean;
  end;

type
  TtdNFAMatchType = (  {types of matching performed...}
     mtNone,           {..no match (an epsilon no-cost move)}
     mtAnyChar,        {..any character}
     mtChar,           {..a particular character}
     mtClass,          {..a character class}
     mtNegClass,       {..a negated character class}
     mtTerminal,       {..the final state--no matching}
     mtUnused);        {..an unused state--no matching}

  TtdRegexError = (    {error codes for invalid regex strings}
     recNone,          {..no error}
     recSuddenEnd,     {..unexpected end of string}
     recMetaChar,      {..read metacharacter, but needed normal char}
     recNoCloseParen,  {..expected close paren, but not there}
     recExtraChars     {..not at end of string after parsing regex}
     );

  TtdRegexEngine = class
    private
      FAnchorEnd  : boolean;
      FAnchorStart: boolean;
      FErrorCode  : TtdRegexError;
      FIgnoreCase : boolean;
      FName        : TtdNameString;
      FPosn       : PAnsiChar;
      FRegexStr   : string;
      {$IFDEF Delphi1}
      FRegexStrZ  : PAnsiChar;
      {$ENDIF}
      FStartState : integer;
      FTable      : TtdRecordList;
      FUpcase     : TtdUpcaseFunc;
      {$IFDEF LogParse}
      Log : System.Text;
      {$ENDIF}
    protected
      procedure rcSetIgnoreCase(aValue : boolean);
      procedure rcSetRegexStr(const aRegexStr : string);
      procedure rcSetUpcase(aValue : TtdUpcaseFunc);

      procedure rcClear;
      procedure rcError(aErrorCode  : integer;
                  const aMethodName : TtdNameString;
                        aIndex      : integer);
      procedure rcLevel1Optimize;
      function rcMatchSubString(const S   : string;
                                StartPosn : integer) : boolean;
      function rcAddState(aMatchType : TtdNFAMatchType;
                          aChar      : AnsiChar;
                          aCharClass : PtdCharSet;
                          aNextState1: integer;
                          aNextState2: integer) : integer;
      function rcSetState(aState     : integer;
                          aNextState1: integer;
                          aNextState2: integer) : integer;

      function rcParseAnchorExpr : integer;
      function rcParseAtom : integer;
      function rcParseCCChar : AnsiChar;
      function rcParseChar : integer;
      function rcParseCharClass(aClass : PtdCharSet) : boolean;
      function rcParseCharRange(aClass : PtdCharSet) : boolean;
      function rcParseExpr : integer;
      function rcParseFactor : integer;
      function rcParseTerm : integer;

      {$IFDEF LogParse}
      procedure rcDumpTable;
      {$ENDIF}
    public
      constructor Create(const aRegexStr : string);
      destructor Destroy; override;

      function Parse(var aErrorPos : integer;
                     var aErrorCode: TtdRegexError) : boolean;
      function MatchString(const S : string) : integer;


      property IgnoreCase : boolean
                  read FIgnoreCase write rcSetIgnoreCase;
      property Name : TtdNameString
                  read FName write FName;
      property RegexString : string
                  read FRegexStr write rcSetRegexStr;
      property Upcase : TtdUpcaseFunc
                  read FUpcase write rcSetUpcase;
  end;

implementation

const
  UnitName = 'TDRegex';

const
  MetaCharacters : set of AnsiChar =
                   ['[', ']', '(', ')', '|', '*', '+', '?', '-', '.',
                    '^', '$'];
  {some handy constants}
  UnusedState = -1;
  NewFinalState = -2;
  CreateNewState = -3;
  ErrorState = -4;
  MustScan = -5;

type
  PNFAState = ^TNFAState;
  TNFAState = packed record
    sdNextState1: integer;
    sdNextState2: integer;
    sdClass     : PtdCharSet;
    sdMatchType : TtdNFAMatchType;
    sdChar      : AnsiChar;
    sdFiller    : array [0..1] of byte;
  end;

{===Helper routines==================================================}
function SystemUpcase(aCh : AnsiChar) : AnsiChar; far;
begin
  Result := System.Upcase(aCh);
end;
{====================================================================}


{===TtdRegexParser===================================================}
constructor TtdRegexParser.Create(const aRegexStr : string);
begin
  inherited Create;
  FRegexStr := aRegexStr;
  {$IFDEF Delphi1}
  FRegexStrZ := StrAlloc(succ(length(aRegexStr)));
  StrPCopy(FRegexStrZ, aRegexStr);
  {$ENDIF}
end;
{--------}
destructor TtdRegexParser.Destroy;
begin
  {$IFDEF Delphi1}
  StrDispose(FRegexStrZ);
  {$ENDIF}
  inherited Destroy;
end;
{--------}
function TtdRegexParser.Parse(var aErrorPos : integer) : boolean;
begin
  Result := true;
  aErrorPos := 0;
  {$IFDEF Delphi1}
  FPosn := FRegexStrZ;
  {$ELSE}
  FPosn := PAnsiChar(FRegexStr);
  {$ENDIF}
  try
    rpParseExpr;
    if (FPosn^ <> #0) then begin
      Result := false;
      {$IFDEF Delphi1}
      aErrorPos := FPosn - FRegexStrZ + 1;
      {$ELSE}
      aErrorPos := FPosn - PAnsiChar(FRegexStr) + 1;
      {$ENDIF}
    end;
  except
    on E:Exception do begin
      Result := false;
      {$IFDEF Delphi1}
      aErrorPos := FPosn - FRegexStrZ + 1;
      {$ELSE}
      aErrorPos := FPosn - PAnsiChar(FRegexStr) + 1;
      {$ENDIF}
    end;
  end;
end;
{--------}
procedure TtdRegexParser.rpParseAtom;
begin
  case FPosn^ of
    '(' : begin
            inc(FPosn);
            writeln('open paren');
            rpParseExpr;
            if (FPosn^ <> ')') then
              raise Exception.Create('Regex error: expecting a closing parenthesis');
            inc(FPosn);
            writeln('close paren');
          end;
    '[' : begin
            inc(FPosn);
            if (FPosn^ = '^') then begin
              inc(FPosn);
              writeln('negated char class');
              rpParseCharClass;
            end
            else begin
              writeln('normal char class');
              rpParseCharClass;
            end;
            inc(FPosn);
          end;
    '.' : begin
            inc(FPosn);
            writeln('any character');
          end;
  else
    rpParseChar;
  end;{case}
end;
{--------}
procedure TtdRegexParser.rpParseCCChar;
begin
  if (FPosn^ = #0) then
    raise Exception.Create('Regex error: expecting a normal character, found null terminator');
  if FPosn^ in [']', '-'] then
    raise Exception.Create('Regex error: expecting a normal character, found a metacharacter');
  if (FPosn^ = '\') then begin
    inc(FPosn);
    writeln('escaped ccchar ', FPosn^);
    inc(FPosn);
  end
  else begin
    writeln('ccchar ', FPosn^);
    inc(FPosn);
  end;
end;
{--------}
procedure TtdRegexParser.rpParseChar;
begin
  if (FPosn^ = #0) then
    raise Exception.Create('Regex error: expecting a normal character, found null terminator');
  if FPosn^ in MetaCharacters then
    raise Exception.Create('Regex error: expecting a normal character, found a metacharacter');
  if (FPosn^ = '\') then begin
    inc(FPosn);
    writeln('escaped char ', FPosn^);
    inc(FPosn);
  end
  else begin
    writeln('char ', FPosn^);
    inc(FPosn);
  end;
end;
{--------}
procedure TtdRegexParser.rpParseCharClass;
begin
  rpParseCharRange;
  if (FPosn^ <> ']') then
    rpParseCharClass;
end;
{--------}
procedure TtdRegexParser.rpParseCharRange;
begin
  rpParseCCChar;
  if (FPosn^ = '-') then begin
    inc(FPosn);
    writeln('--range to--');
    rpParseCCChar;
  end;
end;
{--------}
procedure TtdRegexParser.rpParseExpr;
begin
  rpParseTerm;
  if (FPosn^ = '|') then begin
    inc(FPosn);
    writeln('alternation');
    rpParseExpr;
  end;
end;
{--------}
procedure TtdRegexParser.rpParseFactor;
begin
  rpParseAtom;
  case FPosn^ of
    '?' : begin
            inc(FPosn);
            writeln('zero or one');
          end;
    '*' : begin
            inc(FPosn);
            writeln('zero or more');
          end;
    '+' : begin
            inc(FPosn);
            writeln('one or more');
          end;
  end;{case}
end;
{--------}
procedure TtdRegexParser.rpParseTerm;
begin
  rpParseFactor;
  {Note: we have to "break the grammar" here. We've parsed a regular
         subexpression and we're possibly following on with another
         regular subexpression. There's no nice operator to key off
         for concatenation: we just have to know that for
         concatenating two subexpressions, the current character will
         be
           - an open parenthesis
           - an open square bracket
           - an any char operator
           - a character that's not a metacharacter
         i.e., the three possibilities for the start of an "atom" in
         our grammar}
  if (FPosn^ = '(') or
     (FPosn^ = '[') or
     (FPosn^ = '.') or
     ((FPosn^ <> #0) and not (FPosn^ in MetaCharacters)) then
    rpParseTerm;
end;
{====================================================================}


{===TtdRegexEngine===================================================}
constructor TtdRegexEngine.Create(const aRegexStr : string);
begin
  inherited Create;
  FRegexStr := aRegexStr;
  {$IFDEF Delphi1}
  FRegexStrZ := StrAlloc(succ(length(aRegexStr)));
  StrPCopy(FRegexStrZ, aRegexStr);
  {$ENDIF}
  FIgnoreCase := true;
  FUpcase := SystemUpcase;
  Assert((sizeof(TNFAState) mod 4) = 0,
         'Alignment problem: TNFAState should be multiple of 4 bytes long');
  FTable := TtdRecordList.Create(sizeof(TNFAState));
  FTable.Capacity := 64;
end;
{--------}
destructor TtdRegexEngine.Destroy;
begin
  if (FTable <> nil) then begin
    rcClear;
    FTable.Free;
  end;
  {$IFDEF Delphi1}
  StrDispose(FRegexStrZ);
  {$ENDIF}
  inherited Destroy;
end;
{--------}
function TtdRegexEngine.MatchString(const S : string) : integer;
var
  i : integer;
  ErrorPos  : integer;
  ErrorCode : TtdRegexError;
begin
  {if the regex string hasn't been parsed yet, do so}
  if (FTable.Count = 0) then begin
    if not Parse(ErrorPos, ErrorCode) then
      rcError(tdeRegexParseError, 'MatchString', ErrorPos);
  end;
  {now try and see if the string matches (empty strings don't)}
  Result := 0;
  if (S <> '') then
    {if the regex specified a start anchor, then we only need to check
     the string starting at the first position}
    if FAnchorStart then begin
      if rcMatchSubString(S, 1) then
        Result := 1;
    end
    {otherwise we try and match the string at every position and
     return at the first success}
    else begin
      for i := 1 to length(S) do
        if rcMatchSubString(S, i) then begin
          Result := i;
          Break;
        end;
    end;
end;
{--------}
function TtdRegexEngine.Parse(var aErrorPos : integer;
                              var aErrorCode: TtdRegexError)
                                                            : boolean;
  {$IFDEF LogParse}
  procedure WriteError(aErrorPos : integer;
                       aErrorCode: TtdRegexError);
  begin
    writeln(Log, '***parse error found at ', aErrorPos);
    case aErrorCode of
      recNone         : writeln(Log, '-->no error');
      recSuddenEnd    : writeln(Log, '-->unexpected end of regex');
      recMetaChar     : writeln(Log, '-->found metacharacter in wrong place');
      recNoCloseParen : writeln(Log, '-->missing close paren');
      recExtraChars   : writeln(Log, '-->extra chars after valid regex');
    end;
    writeln(Log, '"', FRegexStr, '"');
    writeln(Log, '^':succ(aErrorPos));
  end;
  {$ENDIF}
begin
  {$IFDEF LogParse}
  System.Assign(Log, 'regexparse.log');
  System.Rewrite(Log);
  try
    writeln(Log, 'Parsing regex: "', FRegexStr, '"');
  {$ENDIF}

  {clear the current transition table}
  rcClear;
  {empty regex strings are not allowed}
  if (FRegexStr = '') then begin
    Result := false;
    aErrorPos := 1;
    aErrorCode := recSuddenEnd;

    {$IFDEF LogParse}
    WriteError(aErrorPos, aErrorCode);
    {$ENDIF}

    Exit;
  end;
  {parse the regex string}
  {$IFDEF Delphi1}
  FPosn := FRegexStrZ;
  {$ELSE}
  FPosn := PAnsiChar(FRegexStr);
  {$ENDIF}
  FStartState := rcParseAnchorExpr;
  {if an error occurred or we're not at the end of the regex string,
   clear the transition table, return false and the error position}
  if (FStartState = ErrorState) or (FPosn^ <> #0) then begin
    if (FStartState <> ErrorState) and (FPosn^ <> #0) then
      FErrorCode := recExtraChars;
    rcClear;
    Result := false;
    {$IFDEF Delphi1}
    aErrorPos := succ(FPosn - FRegexStrZ);
    {$ELSE}
    aErrorPos := succ(FPosn - PAnsiChar(FRegexStr));
    {$ENDIF}
    aErrorCode := FErrorCode;

    {$IFDEF LogParse}
    WriteError(aErrorPos, aErrorCode);
    {$ENDIF}
  end
  {otherwise add a terminal state, optimize, return true}
  else begin
    rcAddState(mtTerminal, #0, nil, UnusedState, UnusedState);
    rcLevel1Optimize;
    Result := true;
    aErrorPos := 0;
    aErrorCode := recNone;

    {$IFDEF LogParse}
    rcDumpTable;
    {$ENDIF}
  end;

  {$IFDEF LogParse}
  finally
    System.Close(Log);
  end;
  {$ENDIF}
end;
{--------}
function TtdRegexEngine.rcAddState(aMatchType : TtdNFAMatchType;
                                   aChar      : AnsiChar;
                                   aCharClass : PtdCharSet;
                                   aNextState1: integer;
                                   aNextState2: integer) : integer;
var
  StateData : TNFAState;
begin
  {set up the fields in the state record}
  if (aNextState1 = NewFinalState) then
    StateData.sdNextState1 := succ(FTable.Count)
  else
    StateData.sdNextState1 := aNextState1;
  StateData.sdNextState2 := aNextState2;
  StateData.sdMatchType := aMatchType;
  if (aMatchType = mtChar) then
    StateData.sdChar := aChar
  else if (aMatchType = mtClass) or (aMatchType = mtNegClass) then
    StateData.sdClass := aCharClass;
  {add the new state}
  Result := FTable.Count;
  FTable.Add(@StateData);
end;
{--------}
procedure TtdRegexEngine.rcClear;
var
  i : integer;
  StateData : PNFAState;
begin
  {free all items in the state transition table}
  for i := 0 to pred(FTable.Count) do begin
    StateData := PNFAState(FTable[i]);
    if (StateData <> nil) then begin
      with StateData^ do begin
        if (sdMatchType = mtClass) or
           (sdMatchType = mtNegClass) then
          if (sdClass <> nil) then
            FreeMem(StateData^.sdClass, sizeof(TtdCharSet));
      end;
    end;
  end;
  {clear the state transition table}
  FTable.Clear;
  FTable.Capacity := 64;
  FAnchorStart := false;
  FAnchorEnd := false;
end;
{--------}
{$IFDEF LogParse}
procedure TtdRegexEngine.rcDumpTable;
var
  i : integer;
begin
  writeln(Log);
  if (FTable.Count = 0) then
    writeln(Log, 'No transition table to dump!')
  else begin
    writeln(Log, 'Transition table dump for "', FRegexStr, '"');
    if FAnchorStart then
      writeln(Log, 'anchored at start of string');
    if FAnchorEnd then
      writeln(Log, 'anchored at end of string');
    writeln(Log, 'start state: ', FStartState:3);
    for i := 0 to pred(FTable.Count) do begin
      write(Log, i:3);
      with PNFAState(FTable[i])^ do begin
        case sdMatchType of
          mtNone    : write(Log, '  no match');
          mtAnyChar : write(Log, '  any char');
          mtChar    : write(Log, '    char:', sdChar);
          mtClass   : write(Log, '     class');
          mtNegClass: write(Log, ' neg class');
          mtTerminal: write(Log, '*******END');
          mtUnused  : write(Log, '        --');
        else
          write(Log, ' **error**');
        end;
        if (sdMatchType <> mtTerminal) and
           (sdMatchType <> mtUnused) then
          write(Log, ' next1: ', sdNextState1:3);
        if (sdNextState2 <> UnusedState) then
          write(Log, ' next2: ', sdNextState2:3);
      end;
      writeln(Log);
    end;
  end;
end;
{$ENDIF}
{--------}
procedure TtdRegexEngine.rcError(aErrorCode  : integer;
                           const aMethodName : TtdNameString;
                                 aIndex      : integer);
begin
  if (Name = '') then
    Name := '-unnamed-';
  raise EtdRegexException.Create(
     FmtLoadStr(aErrorCode,
                [aIndex, UnitName, ClassName, aMethodName, Name]));
end;
{--------}
procedure TtdRegexEngine.rcLevel1Optimize;
var
  i : integer;
  Walker : PNFAState;
begin
  {level 1 optimization removes all states that have only a single
   no-cost move to another state}

  {cycle through all the state records, except for the last one}
  for i := 0 to (FTable.Count - 2) do begin
    {get this state}
    with PNFAState(FTable[i])^ do begin
      {walk the chain pointed to by the first next state, unlinking
       the states that are simple single no-cost moves}
      Walker := PNFAState(FTable[sdNextState1]);
      while (Walker^.sdMatchType = mtNone) and
            (Walker^.sdNextState2 = UnusedState) do begin
        sdNextState1 := Walker^.sdNextState1;
        Walker := PNFAState(FTable[sdNextState1]);
      end;
      {walk the chain pointed to by the second next state, unlinking
       the states that are simple single no-cost moves}
      if (sdNextState2 <> UnusedState) then begin
        Walker := PNFAState(FTable[sdNextState2]);
        while (Walker^.sdMatchType = mtNone) and
              (Walker^.sdNextState2 = UnusedState) do begin
          sdNextState2 := Walker^.sdNextState1;
          Walker := PNFAState(FTable[sdNextState2]);
        end;
      end;
    end;
  end;

  {cycle through all the state records, except for the last one,
   marking unused ones--not strictly necessary but good for debugging}
  for i := 0 to (FTable.Count - 2) do begin
    with PNFAState(FTable[i])^ do begin
      if (sdMatchType = mtNone) and
         (sdNextState2 = UnusedState) then
        sdMatchType := mtUnused;
    end;
  end;
end;
{--------}
function TtdRegexEngine.rcMatchSubString(const S   : string;
                                         StartPosn : integer)
                                                            : boolean;
var
  Ch     : AnsiChar;
  State  : integer;
  Deque  : TtdIntDeque;
  StrInx : integer;
begin
  {assume we fail to match}
  Result := false;
  {create the deque}
  Deque := TtdIntDeque.Create(64);
  try
    {enqueue the special value to start scanning}
    Deque.Enqueue(MustScan);
    {enqueue the first state}
    Deque.Enqueue(FStartState);
    {prepare the string index}
    StrInx := StartPosn - 1;
    Ch := #0; {just to fool the compiler}
    {loop until the deque is empty or we run out of string}
    while (StrInx <= length(S)) and not Deque.IsEmpty do begin
      {pop the top state from the deque}
      State := Deque.Pop;
      {process the "must scan" state first}
      if (State = MustScan) then begin
        {if the deque is empty at this point, we might as well give up
         since there are no states left to process new characters}
        if not Deque.IsEmpty then begin
          {if we haven't run out of string, get the character, and
           enqueue the "must scan" state again}
          inc(StrInx);
          if (StrInx <= length(S)) then begin
            if IgnoreCase then
              Ch := Upcase(S[StrInx])
            else
              Ch := S[StrInx];
            Deque.Enqueue(MustScan);
          end;
        end;
      end
      {otherwise, process the state}
      else with PNFAState(FTable[State])^ do begin
        case sdMatchType of
          mtNone :
            begin
              {for free moves, push the next states onto the deque}
              Assert(sdNextState2 <> UnusedState,
                     'optimization should remove all states with one no-cost move');
              Deque.Push(sdNextState2);
              Deque.Push(sdNextState1);
            end;
          mtAnyChar :
            begin
              {for a match of any character, enqueue the next state}
              Deque.Enqueue(sdNextState1);
            end;
          mtChar :
            begin
              {for a match of a character, enqueue the next state}
              if (Ch = sdChar) then
                Deque.Enqueue(sdNextState1);
            end;
          mtClass :
            begin
              {for a match within a class, enqueue the next state}
              if (Ch in sdClass^) then
                Deque.Enqueue(sdNextState1);
            end;
          mtNegClass :
            begin
              {for a match not within a class, enqueue the next state}
              if not (Ch in sdClass^) then
                Deque.Enqueue(sdNextState1);
            end;
          mtTerminal :
            begin
              {for a terminal state, the string successfully matched
               if the regex had no end anchor, or we're at the end
               of the string}
              if (not FAnchorEnd) or (StrInx > length(S)) then begin
                Result := true;
                Exit;
              end;
            end;
          mtUnused :
            begin
              Assert(false, 'unused states shouldn''t be seen');
            end;
        end;
      end;
    end;
    {if we reach this point we've either exhausted the deque or we've
     run out of string; if the former, the substring did not match
     since there are no more states. If the latter, we need to check
     the states left on the deque to see if one is the terminating
     state; if so the string matched the regular expression defined by
     the transition table}
    while not Deque.IsEmpty do begin
      State := Deque.Pop;
      with PNFAState(FTable[State])^ do begin
        case sdMatchType of
          mtNone :
            begin
              {for free moves, push the next states onto the deque}
              Assert(sdNextState2 <> UnusedState,
                     'optimization should remove all states with one no-cost move');
              Deque.Push(sdNextState2);
              Deque.Push(sdNextState1);
            end;
          mtTerminal :
            begin
              {for a terminal state, the string successfully matched
               if the regex had no end anchor, or we're at the end
               of the string}
              if (not FAnchorEnd) or (StrInx > length(S)) then begin
                Result := true;
                Exit;
              end;
            end;
        end;{case}
      end;
    end;
  finally
    Deque.Free;
  end;
end;
{--------}
function TtdRegexEngine.rcParseAnchorExpr : integer;
begin
  {check for an initial '^'}
  if (FPosn^ = '^') then begin
    FAnchorStart := true;
    inc(FPosn);

    {$IFDEF LogParse}
    writeln(Log, 'parsed start anchor');
    {$ENDIF}
  end;

  {parse an expression}
  Result := rcParseExpr;

  {if we were successful, check for the final '$'}
  if (Result <> ErrorState) then begin
    if (FPosn^ = '$') then begin
      FAnchorEnd := true;
      inc(FPosn);

      {$IFDEF LogParse}
      writeln(Log, 'parsed end anchor');
      {$ENDIF}
    end;
  end;
end;
{--------}
function TtdRegexEngine.rcParseAtom : integer;
var
  MatchType : TtdNFAMatchType;
  CharClass : PtdCharSet;
begin
  case FPosn^ of
    '(' :
      begin
        {move past the open parenthesis}
        inc(FPosn);

        {$IFDEF LogParse}
        writeln(Log, 'parsed open paren');
        {$ENDIF}

        {parse a complete regex between the parentheses}
        Result := rcParseExpr;
        if (Result = ErrorState) then
          Exit;
        {if the current character is not a close parenthesis,
         there's an error}
        if (FPosn^ <> ')') then begin
          FErrorCode := recNoCloseParen;
          Result := ErrorState;
          Exit;
        end;
        {move past the close parenthesis}
        inc(FPosn);

        {$IFDEF LogParse}
        writeln(Log, 'parsed close paren');
        {$ENDIF}
      end;
    '[' :
      begin
        {move past the open square bracket}
        inc(FPosn);

        {$IFDEF LogParse}
        writeln(Log, 'parsed open square bracket (start of class)');
        {$ENDIF}

        {if the first character in the class is a '^' then the
         class if negated, otherwise it's a normal one}
        if (FPosn^ = '^') then begin
          inc(FPosn);
          MatchType := mtNegClass;

          {$IFDEF LogParse}
          writeln(Log, 'it is a negated class');
          {$ENDIF}
        end
        else begin
          MatchType := mtClass;

          {$IFDEF LogParse}
          writeln(Log, 'it is a normal class');
          {$ENDIF}
        end;
        {allocate the class character set and parse the character
         class; this will return either with an error, or when the
         closing square bracket is encountered}
        New(CharClass);
        CharClass^ := [];
        if not rcParseCharClass(CharClass) then begin
          Dispose(CharClass);
          Result := ErrorState;
          Exit;
        end;
        {move past the closing square bracket}
        Assert(FPosn^ = ']',
               'the rcParseCharClass terminated without finding a "]"');
        inc(FPosn);

        {$IFDEF LogParse}
        writeln(Log, 'parsed close square bracket (end of class)');
        {$ENDIF}

        {add a new state for the character class}
        Result := rcAddState(MatchType, #0, CharClass,
                             NewFinalState, UnusedState);
      end;
    '.' :
      begin
        {move past the period metacharacter}
        inc(FPosn);

        {$IFDEF LogParse}
        writeln(Log, 'parsed anychar operator "."');
        {$ENDIF}

        {add a new state for the 'any character' token}
        Result := rcAddState(mtAnyChar, #0, nil,
                             NewFinalState, UnusedState);
      end;
  else
    {otherwise parse a single character}
    Result := rcParseChar;
  end;{case}
end;
{--------}
function TtdRegexEngine.rcParseCCChar : AnsiChar;
begin
  {if we hit the end of the string, it's an error}
  if (FPosn^ = #0) then begin
    FErrorCode := recSuddenEnd;
    Result := #0;
    Exit;
  end;
  {if the current char is a metacharacter (at least in terms of a
   character class), it's an error}
  if FPosn^ in [']', '-'] then begin
    FErrorCode := recMetaChar;
    Result := #0;
    Exit;
  end;
  {otherwise return the character and advance past it}
  if (FPosn^ = '\') then
    {..it's an escaped character: get the next character instead}
    inc(FPosn);
  Result := FPosn^;
  inc(FPosn);

  {$IFDEF LogParse}
  writeln(Log, 'parsed charclass char: "', Result, '"');
  {$ENDIF}
end;
{--------}
function TtdRegexEngine.rcParseChar : integer;
var
  Ch : AnsiChar;
begin
  {if we hit the end of the string, it's an error}
  if (FPosn^ = #0) then begin
    Result := ErrorState;
    FErrorCode := recSuddenEnd;
    Exit;
  end;
  {if the current char is one of the metacharacters, it's an error}
  if FPosn^ in MetaCharacters then begin
    Result := ErrorState;
    FErrorCode := recMetaChar;
    Exit;
  end;
  {otherwise add a state for the character}
  {..if it's an escaped character: get the next character instead}
  if (FPosn^ = '\') then
    inc(FPosn);
  if IgnoreCase then
    Ch := Upcase(FPosn^)
  else
    Ch := FPosn^;
  Result := rcAddState(mtChar, Ch, nil, NewFinalState, UnusedState);
  inc(FPosn);

  {$IFDEF LogParse}
  writeln(Log, 'parsed char: "', Ch, '"');
  {$ENDIF}
end;
{--------}
function TtdRegexEngine.rcParseCharClass(aClass : PtdCharSet) : boolean;
begin
  {assume we can't parse a character class properly}
  Result := false;
  {parse a character range; if we can't there was an error and the
   caller will take care of it}
  if not rcParseCharRange(aClass) then
    Exit;
  {if the current character was not the right bracket, parse another
   character class (note: we're removing the tail recursion here)}
  while (FPosn^ <> ']') do begin
    if not rcParseCharRange(aClass) then
      Exit;
  end;
  {if we reach here we were successful}
  Result := true;
end;
{--------}
function TtdRegexEngine.rcParseCharRange(aClass : PtdCharSet) : boolean;
var
  StartChar : AnsiChar;
  EndChar   : AnsiChar;
  Ch        : AnsiChar;
begin
  {assume we can't parse a character range properly}
  Result := false;
  {parse a single character; if it's null there was an error}
  StartChar := rcParseCCChar;
  if (StartChar = #0) then
    Exit;
  {if the current character is not a dash, the range consisted of a
   single character}
  if (FPosn^ <> '-') then begin
    if IgnoreCase then
      Include(aClass^, Upcase(StartChar))
    else
      Include(aClass^, StartChar)
  end
  {otherwise it's a real range, so get the character at the end of the
   range; if that's null, there was an error}
  else begin

    {$IFDEF LogParse}
    writeln(Log, '-range to-');
    {$ENDIF}

    inc(FPosn); {move past the '-'}
    EndChar := rcParseCCChar;
    if (EndChar = #0) then
      Exit;
    {build the range as a character set}
    if (StartChar > EndChar) then begin
      Ch := StartChar;
      StartChar := EndChar;
      EndChar := Ch;
    end;
    for Ch := StartChar to EndChar do begin
      Include(aClass^, Ch);
      if IgnoreCase then
        Include(aClass^, Upcase(Ch));
    end;
  end;
  {if we reach here we were successful}
  Result := true;
end;
{--------}
function TtdRegexEngine.rcParseExpr : integer;
var
  StartState1 : integer;
  StartState2 : integer;
  EndState1   : integer;
  OverallStartState : integer;
begin
  {assume the worst}
  Result := ErrorState;
  {parse an initial term}
  StartState1 := rcParseTerm;
  if (StartState1 = ErrorState) then
    Exit;
  {if the current character is *not* a pipe character, no alternation
   is present so return the start state of the initial term as our
   start state}
  if (FPosn^ <> '|') then
    Result := StartState1
  {otherwise, we need to parse another expr and join the two together
   in the transition table}
  else begin

    {$IFDEF LogParse}
    writeln(Log, 'OR (alternation)');
    {$ENDIF}

    {advance past the pipe}
    inc(FPosn);
    {the initial term's end state does not exist yet (although there
     is a state in the term that points to it), so create it}
    EndState1 := rcAddState(mtNone, #0, nil, UnusedState, UnusedState);
    {for the OR construction we need a new initial state: it will
     point to the initial term and the second just-about-to-be-parsed
     expr}
    OverallStartState := rcAddState(mtNone, #0, nil,
                                    UnusedState, UnusedState);
    {parse another expr}
    StartState2 := rcParseExpr;
    if (StartState2 = ErrorState) then
      Exit;
    {alter the state state for the overall expr so that the second
     link points to the start of the second expr}
    Result := rcSetState(OverallStartState, StartState1, StartState2);
    {now set the end state for the initial term to point to the final
     end state for the second expr and the overall expr}
    rcSetState(EndState1, FTable.Count, UnusedState);
  end;
end;
{--------}
function TtdRegexEngine.rcParseFactor : integer;
var
  StartStateAtom : integer;
  EndStateAtom   : integer;
begin
  {assume the worst}
  Result := ErrorState;
  {first parse an atom}
  StartStateAtom := rcParseAtom;
  if (StartStateAtom = ErrorState) then
    Exit;
  {check for a closure operator}
  case FPosn^ of
    '?' : begin
            {$IFDEF LogParse}
            writeln(Log, 'zero or one closure');
            {$ENDIF}

            {move past the ? operator}
            inc(FPosn);
            {the atom's end state doesn't exist yet, so create one}
            EndStateAtom := rcAddState(mtNone, #0, nil,
                                       UnusedState, UnusedState);
            {create a new start state for the overall regex}
            Result := rcAddState(mtNone, #0, nil,
                                 StartStateAtom, EndStateAtom);
            {make sure the new end state points to the next unused
             state}
            rcSetState(EndStateAtom, FTable.Count, UnusedState);
          end;
    '*' : begin
            {$IFDEF LogParse}
            writeln(Log, 'zero or more closure');
            {$ENDIF}

            {move past the * operator}
            inc(FPosn);
            {the atom's end state doesn't exist yet, so create one;
             it'll be the start of the overall regex subexpression}
            Result := rcAddState(mtNone, #0, nil,
                                 NewFinalState, StartStateAtom);
          end;
    '+' : begin
            {$IFDEF LogParse}
            writeln(Log, 'one or more closure');
            {$ENDIF}

            {move past the + operator}
            inc(FPosn);
            {the atom's end state doesn't exist yet, so create one}
            rcAddState(mtNone, #0, nil, NewFinalState, StartStateAtom);
            {the start of the overall regex subexpression will be the
             atom's start state}
            Result := StartStateAtom;
          end;
  else
    Result := StartStateAtom;
  end;{case}
end;
{--------}
function TtdRegexEngine.rcParseTerm : integer;
var
  StartState2 : integer;
  EndState1   : integer;
begin
  {parse an initial factor, the state number returned will also be our
   return state number}
  Result := rcParseFactor;
  if (Result = ErrorState) then
    Exit;
  {Note: we have to "break the grammar" here. We've parsed a regular
         subexpression and we're possibly following on with another
         regular subexpression. There's no nice operator to key off
         for concatenation: we just have to know that for
         concatenating two subexpressions, the current character will
         be
           - an open parenthesis
           - an open square bracket
           - an any char operator
           - a character that's not a metacharacter
         i.e., the three possibilities for the start of an "atom" in
         our grammar}
  if (FPosn^ = '(') or
     (FPosn^ = '[') or
     (FPosn^ = '.') or
     ((FPosn^ <> #0) and not (FPosn^ in MetaCharacters)) then begin
    {$IFDEF LogParse}
    writeln(Log, 'concatenation');
    {$ENDIF}

    {the initial factor's end state does not exist yet (although there
     is a state in the term that points to it), so create it}
    EndState1 := rcAddState(mtNone, #0, nil, UnusedState, UnusedState);
    {parse another term}
    StartState2 := rcParseTerm;
    if (StartState2 = ErrorState) then begin
      Result := ErrorState;
      Exit;
    end;
    {join the first factor to the second term}
    rcSetState(EndState1, StartState2, UnusedState);
  end;
end;
{--------}
procedure TtdRegexEngine.rcSetIgnoreCase(aValue : boolean);
begin
  if (aValue <> FIgnoreCase) then begin
    rcClear;
    FIgnoreCase := aValue;
  end;
end;
{--------}
procedure TtdRegexEngine.rcSetRegexStr(const aRegexStr : string);
begin
  if (aRegexStr <> FRegexStr) then begin
    rcClear;
    FRegexStr := aRegexStr;
  end;
end;
{--------}
function TtdRegexEngine.rcSetState(aState     : integer;
                                   aNextState1: integer;
                                   aNextState2: integer) : integer;
var
  StateData : PNFAState;
begin
  Assert((0 <= aState) and (aState < FTable.Count),
         'trying to change an invalid state');

  {get the state record and change the transition information}
  StateData := PNFAState(FTable[aState]);
  StateData^.sdNextState1 := aNextState1;
  StateData^.sdNextState2 := aNextState2;
  Result := aState;
end;
{--------}
procedure TtdRegexEngine.rcSetUpcase(aValue : TtdUpcaseFunc);
begin
  if not Assigned(aValue) then
    FUpcase := SystemUpcase
  else
    FUpcase := aValue;
end;
{====================================================================}

end.
