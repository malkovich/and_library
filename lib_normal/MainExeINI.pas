unit MainExeINI;

interface
uses windows, Classes, SysUtils;

procedure WriteConfig (const Section, Ident, ToWrite: string; FinishFree: BOOL = False);
function ReadConfig (const Section, Ident, Default: string; FinishFree: BOOL = False): string;

procedure WriteConfBool (const Section, Ident: string; Value: BOOL; FinishFree: BOOL = False);
function ReadConfBool (const Section, Ident: String; Default: BOOL; FinishFree: BOOL = False): BOOL;

function ReadConfValue (const Section, Ident: String; Default: Integer; FinishFree: BOOL = False): Integer;
procedure WriteConfValue (const Section, Ident: string; Value: Integer; FinishFree: BOOL = False);

Procedure EraseSection (const Section: String; FinishFree: BOOL = False);
function ReadSectionValues (const Section: String; FinishFree: BOOL = False): TStringList;

            
var
  SubDirectory: String;

function MakeFileNameByExt (Ext: String): String;
function GetSplitBlankList (InputStr: String; Separate: TSysCharSet = [' ']): TStringList;

implementation

uses iniFiles;


function GetSplitBlankList (InputStr: String; Separate: TSysCharSet = [' ']): TStringList;
begin
  Result := TStringList.Create;
  ExtractStrings (Separate,[' '],PChar(InputStr),Result);
end;

//MakeFileNameByExt('.ini');
function MakeFileNameByExt (Ext: String): String;
var
  temp: string;
  path: String;
begin
  Result := GetModuleName(0);
  temp := ExtractFileExt (Result);
  setlength (Result, Length(result)-Length(temp));
  result := result + Ext;

  if SubDirectory <> '' then
  begin
    path := ExtractFilePath (Result);
    temp := ExtractFileName (Result);
    Result := path + SubDirectory + temp;
  end;
end;


var
  IniFile: TIniFile;

Procedure BeginIni;
var
  iniFileName: String;
begin
  if not assigned (IniFile) then
  Begin
    iniFileName := MakeFileNameByExt('.ini');
    if not FileExists (iniFileName) then
      FileClose (FileCreate (iniFileName));
    iniFile:= TIniFile.Create(iniFileName);
  end;
end;

Procedure EndIni (FinishFree: BOOL = True);
begin
  if FinishFree then
    FreeAndNil (iniFile);
end;

function ReadConfig (const Section, Ident, Default: string; FinishFree: BOOL = False): string;
begin
  BeginIni;

  Result := iniFile.ReadString (Section, Ident, Default);
  Result := Trim(Result);

  EndIni (FinishFree);
end;

procedure WriteConfig (const Section, Ident, ToWrite: string; FinishFree: BOOL = False);
begin
  BeginIni;
              
  iniFile.WriteString (Section, Ident, ToWrite);

  EndIni (FinishFree);
end;

function ReadConfBool (const Section, Ident: String; Default: BOOL; FinishFree: BOOL = False): BOOL;
begin
  BeginIni;

  Result := iniFile.ReadBool (Section, Ident, Default);

  EndIni (FinishFree);
end;

procedure WriteConfBool (const Section, Ident: string; Value: BOOL; FinishFree: BOOL = False);
begin
  BeginIni;
              
  iniFile.WriteBool (Section, Ident, Value);

  EndIni (FinishFree);
end;

procedure WriteConfValue (const Section, Ident: string; Value: Integer; FinishFree: BOOL = False);
begin
  BeginIni;

  IniFile.WriteInteger(Section, Ident, Value);

  EndIni (FinishFree);
end;

function ReadConfValue (const Section, Ident: String; Default: Integer; FinishFree: BOOL = False): Integer;
begin
  BeginIni;

  Result := IniFile.ReadInteger(Section, Ident, Default);

  EndIni (FinishFree);
end;


Procedure EraseSection (const Section: String; FinishFree: BOOL = False);
begin
  BeginIni;

  IniFile.EraseSection(Section);

  EndIni (FinishFree);
end;

function ReadSectionValues (const Section: String; FinishFree: BOOL = False): TStringList;
begin
  BeginIni;

  Result := TStringList.Create;
  IniFile.ReadSectionValues(Section, Result);
    
  EndIni (FinishFree);
end;

end.
