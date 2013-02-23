program DbgLoger;

{$R 'DBInclude.res' 'DBInclude.rc'}

uses
  Forms,
  LogViewForm in 'LogViewForm.pas' {Form4},
  LogDatabaseUnit in 'LogDatabaseUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
