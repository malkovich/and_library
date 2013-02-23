library VerifyCode;


{$R 'VerifyModule.res' 'VerifyModule.rc'}

uses
  LogHost,
  VerifyCodeUnit in 'VerifyCodeUnit.pas';

Exports
  SetUser,
  SetMachine,
  DeleteUser,
  DeleteMachine,
  GetVerifyCode,
  IsMachineValid,
  GetMachineMaxCount,
  GetMachineList,
  GetMachineDetail;


begin
  Default_LogType := 'VerifyCode';
  Initial ('VerifyCode.DB');
end.

