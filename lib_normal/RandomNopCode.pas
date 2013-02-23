unit RandomNopCode;

interface
uses windows;

procedure FillRandomCode (Start, Stop: Pointer);

implementation

procedure FillRandomCode (Start, Stop: Pointer);
const
  TEMPLATE_6 = chr($8b)+chr($ff)+chr($55)+chr($8b)+chr($ec)+chr($c9);
var
  CodeLen: Integer;
  ScanPtr: PChar;
begin
  CodeLen := Integer(Stop) - Integer(Start);
  if CodeLen < 1 then exit;

  ScanPtr := Start;
  repeat
    ScanPtr^ := Char($90);
    inc(ScanPtr);
  until ScanPtr = Stop;

  if CodeLen > 6 then
    CopyMemory (Start, PChar(TEMPLATE_6), 6);
end;

end.
