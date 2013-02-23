unit TraceLastError;

interface
uses windows, sysutils, inifiles;

procedure And_SetLastError (ErrorType: String; ErrorMsg: String);
function  And_GetLastError (ErrorType: String): String;

function  And_GetAllLastError : String;
procedure And_CleanError (ToFileName: String = '');

implementation

var
  ErrorList: THashedStringList;

procedure And_SetLastError (ErrorType: String; ErrorMsg: String);
begin
  ErrorList.Add (ErrorType + '=' + ErrorMsg);
end;

function  And_GetLastError (ErrorType: String): String;
begin
  result := ErrorList.Values[ErrorType];
end;

function  And_GetAllLastError : String;
begin
  Result := ErrorList.Text;
end;

procedure And_CleanError (ToFileName: String = '');
begin
  if ToFileName <> '' then
    ErrorList.SaveToFile(ToFileName);
  ErrorList.Clear;
end;

initialization
  ErrorList := THashedStringList.Create;

end.
