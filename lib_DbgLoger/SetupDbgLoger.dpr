program SetupDbgLoger;

{$R 'Setup.res' 'Setup.rc'}

uses
  Forms,
  SetupForm in 'SetupForm.pas' {Form7};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
