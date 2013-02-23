program MakeMachineDetails;

uses
  Forms,
  MakeMachineForm in 'MakeMachineForm.pas' {Form2},
  HardInfosAndComputerIDUnit in '..\lib\lib_normal\HardInfosAndComputerIDUnit.pas',
  WMIUnit in '..\lib\lib_wmi\WMIUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
