unit SendEmailUnit;

interface
uses windows, classes;

function SimpleSendEmail (BaseParam, MailMessage, EmailFiles: TStringList): BOOL; overload;
function SimpleSendEmail (Base64Param: String): BOOL; overload;

implementation

uses OverbyteIcsSmtpProt, Base64Unit;

{
Host=
Port=
UserName=
Password=
FromName=
HdrFrom=
HdrTo=
HdrSubject=
ToList=
CcList=
BccList=
MailMessage=
EmailFiles=
}

function SimpleSendEmail (Base64Param: String): BOOL;
var
  BaseParam, MailMessage, EmailFiles: TStringList;
  sMailMessage, sEmailFiles: String;
  DelAttachWhenDone: BOOL;
begin
  BaseParam := TStringList.Create;
  MailMessage := TStringList.Create;
  EmailFiles := TStringList.Create;

  Base64Param := DecodeBase64 (Base64Param);
  BaseParam.Text := Base64Param;

  DelAttachWhenDone := BaseParam.Values['DelAttachWhenDone'] = 'True';
  sMailMessage := BaseParam.Values['MailMessage'];
  sMailMessage := DecodeBase64 (sMailMessage);
  MailMessage.Text := sMailMessage;

  sEmailFiles := BaseParam.Values['EmailFiles'];
  sEmailFiles := DecodeBase64 (sEmailFiles);
  EmailFiles.Text := sEmailFiles;

  Result := SimpleSendEmail (BaseParam, MailMessage, EmailFiles);
                
  if DelAttachWhenDone then
    for sEmailFiles in EmailFiles do
      DeleteFile (PChar(sEmailFiles));

  BaseParam.Free;
  MailMessage.Free;
  EmailFiles.Free;
end;

function SimpleSendEmail (BaseParam, MailMessage, EmailFiles: TStringList): BOOL;
var
  SyncSmtp: TSyncSmtpCli;
begin
  Result := False;
  SyncSmtp := TSyncSmtpCli.Create(nil);
  SyncSmtp.Host := BaseParam.Values['Host'];
  SyncSmtp.Port := BaseParam.Values['Port'];
  SyncSmtp.Username := BaseParam.Values['UserName'];
  SyncSmtp.Password := BaseParam.Values['Password'];
  SyncSmtp.FromName := BaseParam.Values['FromName'];
  SyncSmtp.HdrFrom := BaseParam.Values['HdrFrom'];
  SyncSmtp.HdrTo := BaseParam.Values['HdrTo'];
  SyncSmtp.HdrSubject := BaseParam.Values['HdrSubject'];
  SyncSmtp.HdrCc     := BaseParam.Values['CcList'];
  SyncSmtp.RcptNameAdd(SyncSmtp.HdrTo, SyncSmtp.HdrCc, BaseParam.Values['BccList']);

  SyncSmtp.MailMessage.AddStrings (MailMessage);
  SyncSmtp.EmailFiles.AddStrings(EmailFiles);

  SyncSmtp.AuthType := smtpAuthLogin;
  SyncSmtp.HdrPriority := smtpPriorityNormal;
  SyncSmtp.DefaultEncoding := smtpEncBase64;
  SyncSmtp.SendMode := smtpToSocket;

  Repeat
    if not SyncSmtp.ConnectSync then break;
    if not SyncSmtp.EhloSync then break;
    if not SyncSmtp.AuthSync then break;
    if not SyncSmtp.MailFromSync then break;
    if not SyncSmtp.RcptToSync then break;
    if not SyncSmtp.DataSync then break;
    if not SyncSmtp.QuitSync then break;
    Result := True;
  until True;

  SyncSmtp.Free;
end;

end.
