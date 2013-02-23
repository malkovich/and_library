program GetStringHash;

{$APPTYPE CONSOLE}

uses
  SysUtils, windows, DLLDatabase;

var
  GetLine, ResultStr, PrintStr: String;
  HashValue: DWORD;
  
begin
  repeat
    Write ('Input string to hash: ');
    ReadLn (GetLine);
    GetLine := Trim (GetLine);
    if GetLine = 'exit' then Exit;

    HashValue := DLLDatabase.GetUpperNameHash (PChar(GetLine));
    ResultStr := '$' + IntToHex (HashValue, 8);
    PrintStr := UpperCase(GetLine) + ' = ' + ResultStr;
                            
    WriteLn (PrintStr);
    WriteLn ('');
  until false;

end.
