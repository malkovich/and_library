// ***************************************************************
//  madExceptPatcher.pas      version:  1.1b  ·  date: 2006-05-26
//  -------------------------------------------------------------
//  patch dlls/exes to enable full madExcept support
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2006 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2006-05-26 1.1b (1) undocumented MES file option "MadExcept2Only" added
//                 (2) minDebugInfo + hideUglyItems don't like each other
//                 (3) UPX fix modification for BPLs
// 2005-11-28 1.1a (1) "show please wait box" setting was incorrectly stored
//                 (2) "AbortBtn" renamed to "CancelBtn"
//                 (3) "include min. debug info" forces "show addr offset" on
//                 (4) superfluous "icon" logging removed
//                 (5) settings are now stored even if madExcept is not enabled
//                 (6) "mes" file redirection implemented (mes content = path)
// 2005-07-17 1.1  lots of changes for madExcept 3.0
// 2005-07-17 1.0g (1) stupid me, dbisamm -> dbisammm
//                 (2) "unit init order" option no longer limited to 260 chars
//                 (3) defaults for FastMM3, RecyclerMM, SmartHeapMM added
//                 (4) changed several default settings
//                 (5) locating "_memcpy" failed sometimes (bcb support)
//                 (6) unit init order patching removed completely
// 2005-01-30 1.0f dbisam mem manager added to unit init order patching option
// 2004-10-22 1.0e extended support for several kind of BCB exceptions
// 2004-08-15 1.0d (1) support for "ListHardware" option added
//                 (2) memory leak: loaded map file was not freed again
//                 (3) unit init order patching: support for NexusMM2 added
// 2004-03-28 1.0c (1) BCB: unit init order patching made problems again (argh)
//                 (2) "try/except" added when searching for exported functions
//                 (3) ExceptMsg + FrozenMsg max length increased to 255 chars
// 2004-03-07 1.0b (1) BCB unit init order patching warning removed
//                 (2) showRelativeLines option added
//                 (3) MailAsSMTPServer not default anymore
//                 (4) ScreenShot support added
//                 (5) unit init order patching supports package units now
//                 (6) unit init order patching can be disabled now
//                 (7) automated support for CGI and ISAPI added
//                 (8) "dontExportUglyItems" define added, disabled by default
// 2003-11-16 1.0a (1) order of patching changed: resources come last
//                 (2) NexusDB memory manager support added to default options
//                 (3) unit init order patching is skipped for BCB binary files
//                 (4) dynamically loaded bpls: handling of init exceptions
//                 (5) ExceptMsg string size increased to 200 characters
//                 (6) bug in init order patching fixed
//                 (7) support for TThread.Synchronize exception catching
//                 (8) madExcept "Synchronize" option removed
//                 (9) autoMail + autoClip + autoContinue options added
//                 (a) showRelativeAddrs option added
//                 (b) a whole bunch of new mailing options added
// 2003-06-09 1.0  needed by madExceptWizard, *Patch and *FinalBuilder

unit madExceptPatcher;

{$I mad.inc}

{$define bcb}  // always compile with BCB support for now

interface

uses Windows, SysUtils, madExcept, madTypes;

// ***************************************************************

// when defined *and* when the option "hideUglyItems" is selected by the user
// public names of VCL/RTL functions are killed from the map file info stream
{ $define dontExportUglyItems}

// ***************************************************************

type
  TProject = record
    FPrj                : IUnknown;
    FCurrentFileName    : string;
    FModified           : boolean;
    FSaveNotifierIndex  : integer;
    FLastBinary         : string;
    FLastBinaryTime     : int64;
    FLastBinarySize     : dword;
    MadExcept2Only      : boolean;
    // SETTINGS
    Enabled             : boolean;
    // BASIC SETTINGS
    MinDebugInfoOnly    : boolean;
    NoOwnSettings       : boolean;
    CheckFileCrc        : boolean; // default true
    CheckForFreeze      : boolean; // default false
    FreezeTimeout       : dword; // default 60
    // ON EXCEPTION AUTO ACTIONS
    AutoSave            : boolean;
    AutoSaveIfNotSent   : boolean;
    AutoSend            : boolean;
    AutoSendPrgrBox     : boolean;
    AutoClipboard       : boolean;
    SuspendThreads      : boolean;
    ShowPleaseWaitBox   : boolean;
    AutoContinue        : boolean;
    AutoRestart         : dword;
    AutoClose           : dword;
    AutoDelay           : dword;
    // EXCEPTION FILTER
    Filter1Classes      : string;
    Filter2Classes      : string;
    Filter1NoBugReport  : boolean;
    Filter2NoBugReport  : boolean;
    GeneralNoBugReport  : boolean;
    Filter1NoScreenShot : boolean;
    Filter2NoScreenShot : boolean;
    GeneralNoScreenShot : boolean;
    Filter1NoSuspend    : boolean;
    Filter2NoSuspend    : boolean;
    GeneralNoSuspend    : boolean;
    Filter1NoHandlers   : boolean;
    Filter2NoHandlers   : boolean;
    GeneralNoHandlers   : boolean;
    Filter1ShowSetting  : TMEShowSetting;
    Filter2ShowSetting  : TMEShowSetting;
    GeneralShowSetting  : TMEShowSetting;
    Filter1Assis        : string;
    Filter2Assis        : string;
    GeneralAssis        : string;
    // EXCEPTION BOX SETTINGS
    SendBtnVisible      : boolean;
    SaveBtnVisible      : boolean;
    PrintBtnVisible     : boolean;
    ShowBtnVisible      : boolean;
    ContinueBtnVisible  : boolean;
    RestartBtnVisible   : boolean;
    CloseBtnVisible     : boolean;
    SendBtnIcon         : string;
    SaveBtnIcon         : string;
    PrintBtnIcon        : string;
    ShowBtnIcon         : string;
    ContinueBtnIcon     : string;
    CantContinueBtnIcon : string;
    RestartBtnIcon      : string;
    CloseBtnIcon        : string;
    BigIcon             : string;
    Send32Icon          : string;
    PleaseWaitIcon      : string;
    FocusedButton       : TMEButton;
    SendAssistant       : string;
    SaveAssistant       : string;
    PrintAssistant      : string;
    AutoShowBugReport   : boolean;
    NoOwnerDrawButtons  : boolean;
    // EMAIL SETTINGS
    MailAddr            : string;
    SendInBackground    : boolean;
    MailAsSmtpServer    : boolean;
    MailAsSmtpClient    : boolean;
    UploadViaHttp       : boolean;
    MailViaMapi         : boolean;
    MailViaMailto       : boolean;
    SmtpServer          : string;
    SmtpPort            : dword;
    SmtpAccount         : string;
    SmtpPassword        : string;
    HttpServer          : string;
    HttpPort            : dword;
    HttpAccount         : string;
    HttpPassword        : string;
    AttachBugReport     : boolean;
    AttachBugReportFile : boolean;
    DeleteBugReportFile : boolean;
    BugReportSendAs     : string;
    BugReportZip        : string;
    ScreenShotDepth     : integer;
    ScreenShotAppOnly   : boolean;
    ScreenShotSendAs    : string;
    ScreenShotZip       : string;
    AdditionalAttachs   : string;
    MailFrom            : string;
    // SAVE SETTINGS
    BugReportFile       : string;
    AppendBugReports    : boolean;
    BugReportFileSize   : dword;
    NoDupExcepts        : boolean;
    NoDupFreezes        : boolean;
    DupExceptDef        : TMEDupDef;
    DupFreezeDef        : TMEDupDef;
    // BUG REPORT SETTINGS
    ListThreads         : boolean;
    ShowCpuRegisters    : boolean;
    ShowStackDump       : boolean;
    ShowDisAsm          : boolean;
    HideUglyItems       : boolean;
    ShowRelativeAddrs   : boolean;
    ShowRelativeLines   : boolean;
    FormatDisassembly   : boolean;
    LimitDisassembly    : integer;
    EnabledPlugins      : string;
    VersionVar          : string;
    // ASSISTANT CREATOR
    Assistants          : TDAString;
    Forms               : TDAString;
    // CUSTOM STRINGS
    TitleBar            : string;
    ExceptMsg           : string;
    FrozenMsg           : string;
    BitFaultMsg         : string;
    SendBtnCaption      : string;
    SaveBtnCaption      : string;
    PrintBtnCaption     : string;
    ShowBtnCaption      : string;
    ContinueBtnCaption  : string;
    RestartBtnCaption   : string;
    CloseBtnCaption     : string;
    OkBtnCaption        : string;
    DetailsBtnCaption   : string;
    PleaseWaitTitle     : string;
    PleaseWaitText      : string;
    MailSubject         : string;
    MailBody            : string;
    SendBoxTitle        : string;
    PrepareAttachMsg    : string;
    MxLookupMsg         : string;
    ConnectMsg          : string;
    AuthMsg             : string;
    SendMailMsg         : string;
    FieldsMsg           : string;
    SendAttachMsg       : string;
    SendFinalizeMsg     : string;
    SendFailureMsg      : string;
  end;

procedure   indentLog;
procedure unindentLog;
procedure log (str: string);

procedure SetDefaultSettings  (var project: TProject);
procedure LoadSettingsFromIni (fileName: string; var project: TProject);
procedure SaveSettingsToIni   (fileName: string; const project: TProject);
function  LoadDefaultSettings (var project: TProject) : boolean;
procedure SaveDefaultSettings (const project: TProject);

function GetProcInfo (proc: pointer; var unitName, procName: string) : boolean;

function PatchBinary (var project: TProject; binary, root, map: string;
                      compileSucceeded: boolean; var infos: string) : integer;

// ***************************************************************

implementation

uses madMapFile, madStrings, madDisAsm, madTools, madRes, madCrypt;

// ***************************************************************

var indentStr : string;
procedure   indentLog; begin indentStr := indentStr + '  ' end;
procedure unindentLog; begin Delete(indentStr, 1, 2)       end;

var logFileExists : boolean = false;
    logFileCheck  : dword = 0;
procedure log(str: string);
var c1, c2 : dword;
    st     : TSystemTime;
    time   : string;
begin
  if (logFileCheck = 0) or (GetTickCount < logFileCheck) or (GetTickCount - logFileCheck > 3000) then begin
    logFileExists := GetFileAttributes('c:\madExceptWizard.txt') <> dword(-1);
    logFileCheck := GetTickCount;
  end;
  if logFileExists then begin
    c1 := CreateFile('c:\madExceptWizard.txt', GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, 0);
    if c1 <> INVALID_HANDLE_VALUE then begin
      SetFilePointer(c1, 0, nil, FILE_END);
      GetLocalTime(st);
      time := IntToStrEx(dword(st.wHour),         2) + ':' +
              IntToStrEx(dword(st.wMinute),       2) + ':' +
              IntToStrEx(dword(st.wSecond),       2) + '-' +
              IntToStrEx(dword(st.wMilliseconds), 3) + ' ';
      ReplaceStr(str, #$D#$A, #$D#$A + time + indentStr);
      if str = '' then str := time + indentStr + 'InterceptExceptionHandler' + #$D#$A
      else             str := time + indentStr + str                         + #$D#$A;
      WriteFile(c1, pointer(str)^, length(str), c2, nil);
      CloseHandle(c1);
    end else
      logFileExists := false;
  end;
end;

// ***************************************************************

function LoadStrFromFile(file_: wideString) : string;
var fh, c1 : dword;
begin
  fh := CreateFileW(pwidechar(file_), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if fh <> INVALID_HANDLE_VALUE then begin
    SetLength(result, GetFileSize(fh, nil));
    if (result <> '') and (not ReadFile(fh, pointer(result)^, Length(result), c1, nil)) and (c1 = dword(Length(result))) then
      result := '';
    CloseHandle(fh);
  end else
    result := '';
end;

procedure SetDefaultSettings(var project: TProject);
begin
  with project do begin
    MadExcept2Only      := false;
    Enabled             := false;
    MinDebugInfoOnly    := false;
    NoOwnSettings       := false;
    CheckFileCrc        := true;
    CheckForFreeze      := false;
    FreezeTimeout       := 60000;
    AutoSave            := true;
    AutoSaveIfNotSent   := true;
    AutoSend            := false;
    AutoSendPrgrBox     := false;
    AutoClipboard       := false;
    SuspendThreads      := false;
    ShowPleaseWaitBox   := true;
    AutoContinue        := false;
    AutoRestart         := 0;
    AutoClose           := 0;
    SendBtnVisible      := true;
    SaveBtnVisible      := false;
    PrintBtnVisible     := false;
    ShowBtnVisible      := true;
    ContinueBtnVisible  := true;
    RestartBtnVisible   := true;
    CloseBtnVisible     := true;
    SendBtnIcon         := '';
    SaveBtnIcon         := '';
    PrintBtnIcon        := '';
    ShowBtnIcon         := '';
    ContinueBtnIcon     := '';
    CantContinueBtnIcon := '';
    RestartBtnIcon      := '';
    CloseBtnIcon        := '';
    FocusedButton       := bSendBugReport;
    SendAssistant       := 'SendAssistant';
    SaveAssistant       := 'SaveAssistant';
    PrintAssistant      := 'PrintAssistant';
    AutoShowBugReport   := false;
    NoOwnerDrawButtons  := false;
    BigIcon             := '';
    Send32Icon          := '';
    PleaseWaitIcon      := '';
    MailAddr            := '';
    SendInBackground    := true;
    MailAsSmtpServer    := false;
    MailAsSmtpClient    := false;
    UploadViaHttp       := false;
    MailViaMapi         := true;
    MailViaMailto       := true;
    SmtpServer          := '';
    SmtpPort            := 25;
    SmtpAccount         := '';
    SmtpPassword        := '';
    HttpServer          := '';
    HttpPort            := 80;
    HttpAccount         := '';
    HttpPassword        := '';
    AttachBugReport     := true;
    AttachBugReportFile := true;
    DeleteBugReportFile := true;
    BugReportSendAs     := 'bugreport.txt';
    BugReportZip        := '';
    ScreenShotDepth     := 8;
    ScreenShotAppOnly   := false;
    ScreenShotSendAs    := 'screenshot.png';
    ScreenShotZip       := '';
    AdditionalAttachs   := '';
    BugReportFile       := 'bugreport.txt';
    AppendBugReports    := true;
    BugReportFileSize   := 100000;
    NoDupExcepts        := true;
    NoDupFreezes        := true;
    DupExceptDef        := ddCrashStackIdentical;
    DupFreezeDef        := ddAllStacksIdentical;
    ListThreads         := true;
    ShowCpuRegisters    := true;
    ShowStackDump       := true;
    ShowDisAsm          := true;
    HideUglyItems       := false;
    ShowRelativeAddrs   := true;
    ShowRelativeLines   := true;
    FormatDisassembly   := false;
    LimitDisassembly    := 5;
    EnabledPlugins      := 'modules|processes|hardware';
    Filter1Classes      := 'EDBEditError';
    Filter1NoBugReport  := true;
    Filter1NoScreenshot := true;
    Filter1NoSuspend    := true;
    Filter1NoHandlers   := true;
    Filter1ShowSetting  := ssSimpleBox;
    Filter1Assis        := '';
    Filter2Classes      := '';
    Filter2NoBugReport  := false;
    Filter2NoScreenshot := false;
    Filter2NoSuspend    := false;
    Filter2NoHandlers   := false;
    Filter2ShowSetting  := ssFullBox;
    Filter2Assis        := '';
    GeneralNoBugReport  := false;
    GeneralNoScreenshot := false;
    GeneralNoSuspend    := false;
    GeneralNoHandlers   := false;
    GeneralShowSetting  := ssFullBox;
    GeneralAssis        := '';
    TitleBar            := '%appname%';
    ExceptMsg           := 'An error occurred in the application.';
    FrozenMsg           := 'The application seems to be frozen.';
    BitFaultMsg         := 'The file "%modname%" seems to be corrupt!';
    SendBtnCaption      := 'send bug report';
    SaveBtnCaption      := 'save bug report';
    PrintBtnCaption     := 'print bug report';
    ShowBtnCaption      := 'show bug report';
    ContinueBtnCaption  := 'continue application';
    RestartBtnCaption   := 'restart application';
    CloseBtnCaption     := 'close application';
    OkBtnCaption        := '&OK';
    DetailsBtnCaption   := '&Details';
    PleaseWaitTitle     := 'Information';
    PleaseWaitText      := 'Please wait a moment...';
    MailSubject         := 'bug report';
    MailBody            := 'please find the bug report attached';
    SendBoxTitle        := 'Sending bug report...';
    PrepareAttachMsg    := 'Preparing attachments...';
    MxLookupMsg         := 'Searching for mail server...';
    ConnectMsg          := 'Connecting to server...';
    AuthMsg             := 'Authentication...';
    SendMailMsg         := 'Sending mail...';
    FieldsMsg           := 'Setting fields...';
    SendAttachMsg       := 'Sending attachments...';
    SendFinalizeMsg     := 'Finalizing...';
    SendFailureMsg      := 'Sorry, sending the bug report didn''t work.';
    VersionVar          := '';
    SetLength(Forms, 3);
    Forms[0] := 'TPF0' + #$0e + 'TMEContactForm' + #$0b + 'ContactForm' +
                         #$07 + 'Message'        + #$0c + #$13#$00#$00#$00 + 'Contact Information' +
                         #$08 + 'OnAction'       + #$0c + #$1b#$00#$00#$00 + 'madExcept.HandleContactForm' +
                         #$00 + #$09 + 'INVButton'   + #$0b + 'ContinueBtn' +
                                #$00 +
                         #$00 + #$09 + 'INVButton'   + #$07 + 'SkipBtn' +
                                #$07 + 'Enabled'     + #$08 +
                                #$00 +
                         #$00 + #$09 + 'INVButton'   + #$09 + 'CancelBtn' +
                                #$00 +
                         #$00 + #$08 + 'INVLabel'    + #$06 + 'Label1' +
                                #$07 + 'Caption'     + #$0c + #$0a#$00#$00#$00 + 'your name:' +
                                #$00 +
                         #$00 + #$07 + 'INVEdit'     + #$08 + 'NameEdit' +
                                #$08 + 'Optional'    + #$09 +
                                #$0a + 'OutputName'  + #$0c + #$0c#$00#$00#$00 + 'contact name' +
                                #$00 +
                         #$00 + #$08 + 'INVLabel'    + #$06 + 'Label2' +
                                #$07 + 'Caption'     + #$0c + #$0b#$00#$00#$00 + 'your email:' +
                                #$00 +
                         #$00 + #$07 + 'INVEdit'     + #$09 + 'EmailEdit' +
                                #$0a + 'OutputName'  + #$0c + #$0d#$00#$00#$00 + 'contact email' +
                                #$00 +
                         #$00 + #$0b + 'INVCheckBox' + #$08 + 'MemCheck' +
                                #$07 + 'Caption'     + #$0c + #$0b#$00#$00#$00 + 'remember me' +
                                #$00 +
                         #$00 +
                #$00;
    Forms[1] := 'TPF0' + #$0e + 'TMEDetailsForm' + #$0b + 'DetailsForm' +
                         #$07 + 'Message'        + #$0c + #$0d#$00#$00#$00 + 'Error Details' +
                         #$00 + #$09 + 'INVButton'   + #$0b + 'ContinueBtn' +
                                #$00 +
                         #$00 + #$09 + 'INVButton'   + #$07 + 'SkipBtn' +
                                #$00 +
                         #$00 + #$09 + 'INVButton'   + #$09 + 'CancelBtn' +
                                #$00 +
                         #$00 + #$08 + 'INVLabel'   + #$06 + 'Label1' +
                                #$07 + 'Caption'    + #$0C + #$27#$00#$00#$00 + 'in which situation did the error occur?' +
                                #$00 +
                         #$00 + #$07 + 'INVEdit'    + #$0b + 'DetailsMemo' +
                                #$05 + 'Lines'      + #$04 + #$09#$00#$00#$00 +
                                #$0a + 'OutputName' + #$0c + #$0d#$00#$00#$00 + 'error details' +
                                #$0a + 'OutputType' + #$07 + #$0d +             'nvoOwnSection' +
                                #$00 +
                         #$00 +
                #$00;
    Forms[2] := 'TPF0' + #$0e + 'TMEScrShotForm' + #$0b + 'ScrShotForm' +
                         #$0d + 'ActiveControl'  + #$07 + #$0b             + 'ContinueBtn' +
                         #$05 + 'Timer'          + #$04 + #$fa#$00#$00#$00 +
                         #$07 + 'Message'        + #$0c + #$18#$00#$00#$00 + 'Screenshot Configuration' +
                         #$08 + 'OnAction'       + #$0c + #$1e#$00#$00#$00 + 'madExcept.HandleScreenshotForm' +
                         #$00 + #$09 + 'INVButton'   + #$0b + 'ContinueBtn' +
                                #$00 +
                         #$00 + #$09 + 'INVButton'   + #$07 + 'SkipBtn' +
                                #$07 + 'Enabled'     + #$08 +
                                #$00 +
                         #$00 + #$09 + 'INVButton'   + #$09 + 'CancelBtn' +
                                #$00 +
                         #$00 + #$0b + 'INVCheckBox' + #$0b + 'AttachCheck' +
                                #$07 + 'Checked'     + #$09 +
                                #$07 + 'Caption'     + #$0c + #$25#$00#$00#$00 + 'attach a screenshot to the bug report' +
                                #$00 +
                         #$00 + #$08 + 'INVImage'    + #$0a + 'ScrShotImg' +
                                #$06 + 'Border'      + #$09 +
                                #$09 + 'Clickable'   + #$09 +
                                #$00 +
                         #$00 + #$08 + 'INVLabel'    + #$06 + 'Label1' +
                                #$07 + 'Caption'     + #$0C + #$15#$00#$00#$00 + '(click to edit image)' +
                                #$00 +
                         #$00 +
                #$00;
    UniqueString(Forms[0]);
    UniqueString(Forms[1]);
    UniqueString(Forms[2]);
    SetLength(Assistants, 3);
    Assistants[0] := 'SendAssistant|Send Assistant|ContactForm|DetailsForm|ScrShotForm';
    Assistants[1] := 'SaveAssistant|Save Assistant|ContactForm|DetailsForm';
    Assistants[2] := 'PrintAssistant|Print Assistant|ContactForm|DetailsForm';
    UniqueString(Assistants[0]);
    UniqueString(Assistants[1]);
    UniqueString(Assistants[2]);
  end;
  log('SetDefaultSettings');
end;

function EncodeStr(str: string) : string;

  function FourBitToHex(fourBit: integer) : char;
  begin
    if fourBit < $a then
         result := chr(ord('0') - $0 + fourBit)
    else result := chr(ord('a') - $a + fourBit);
  end;

var i1, i2 : integer;
    trim1, trim2 : integer;
begin
  i2 := Length(str);
  trim1 := 0;
  for i1 := 1 to length(str) do
    if str[i1] = ' ' then
      trim1 := i1
    else
      break;
  trim2 := length(str) + 1;
  for i1 := length(str) downto i1 do
    if str[i1] = ' ' then
      trim2 := i1
    else
      break;
  for i1 := 1 to length(str) do
    if (i1 <= trim1) or (i1 >= trim2) or (not (str[i1] in [' '..'$', '&'..'~'])) then
      inc(i2, 2);
  SetLength(result, i2);
  i2 := 1;
  for i1 := 1 to Length(str) do
    if (i1 <= trim1) or (i1 >= trim2) or (not (str[i1] in [' '..'$', '&'..'~'])) then begin
      result[i2    ] := '%';
      result[i2 + 1] := FourBitToHex(ord(str[i1]) shr  4);
      result[i2 + 2] := FourBitToHex(ord(str[i1]) and $F);
      inc(i2, 3);
    end else begin
      result[i2] := str[i1];
      inc(i2);
    end;
end;

function DecodeStr(str: string) : string;
var pc1, pc2 : pchar;
    ch1      : char;
begin
  result := str;
  if result <> '' then begin
    UniqueString(result);
    pc1 := pchar(str);
    pc2 := pchar(result);
    while true do begin
      ch1 := pc1^;
      if (ch1 = '%') and ((pc1 + 1)^ in ['0'..'9', 'a'..'f', 'A'..'F']) and
                         ((pc1 + 2)^ in ['0'..'9', 'a'..'f', 'A'..'F']) then begin
        byte(pc2^) := StrToIntEx(true, pc1 + 1, 2);
        inc(pc1, 3);
        inc(pc2);
      end else begin
        pc2^ := ch1;
        inc(pc1);
        inc(pc2);
      end;
      if ch1 = #0 then
        break;
    end;
    SetLength(result, dword(pc2) - dword(result) - 1);
  end;
end;

function RegGetStr(hk: HKEY; name: string; var value: string) : boolean;
var s1  : string;
    len : dword;
begin
  result := false;
  len := 0;
  if RegQueryValueEx(hk, pchar(name), nil, nil, nil, @len) = 0 then begin
    SetLength(s1, len);
    if RegQueryValueEx(hk, pchar(name), nil, nil, pointer(s1), @len) = 0 then begin
      value := Copy(s1, 1, len - 1);
      result := true;
      log(name + ' = "' + value + '"');
    end;
  end;
end;

procedure LoadSettingsFromIni(fileName: string; var project: TProject);

  procedure IniGetDword(section, key: string; var value: dword);
  var arrCh : array [0..MAX_PATH] of char;
  begin
    if (GetPrivateProfileString(pchar(section), pchar(key), '´', arrCh, MAX_PATH, pchar(fileName)) <> 1) or
       (arrCh[0] <> '´') then begin
      value := StrToIntDef(arrCh, value);
      log('[' + section + ']\' + key + ' = ' + IntToStr(value));
    end;
  end;

  function IniGetBoolean(section, key: string; var value: boolean) : boolean;
  var arrCh : array [0..MAX_PATH] of char;
  begin
    result := false;
    if (GetPrivateProfileString(pchar(section), pchar(key), '´', arrCh, MAX_PATH, pchar(fileName)) <> 1) or
       (arrCh[0] <> '´') then begin
      result := true;
      value := StrToIntDef(arrCh, ord(value)) <> 0;
      log('[' + section + ']\' + key + ' = ' + booleanToChar(value));
    end;
  end;

  function IniGetStr(section, key: string; var value: string) : boolean;
  var pc : pchar;
  begin
    GetMem(pc, 32 * 1024);
    if (GetPrivateProfileString(pchar(section), pchar(key), '´', pc, 32 * 1024, pchar(fileName)) <> 1) or
       (pc[0] <> '´') then begin
      result := true;
      value := pc;
      if PosText('Assistent', key) > 0 then
        ReplaceStr(value, 'Assistent', 'Assistant');
      log('[' + section + ']\' + key + ' = "' + value + '"');
      value := DecodeStr(value);
    end else
      result := false;
    FreeMem(pc);
  end;

var arrCh : array [0..MAX_PATH] of char;
    c1    : dword;
    b1    : boolean;
    hk    : HKEY;
    s1    : string;
    i1    : integer;
begin
  log('LoadSettingsFromIni(' + fileName + '):'); indentLog;
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, 'Software\madshi\madExcept', 0, KEY_QUERY_VALUE, hk) = 0 then begin
    if RegGetStr(hk, 'centralMesFile', s1) and (s1 <> '') then begin
      if ExpandEnvironmentStrings(pchar(s1), arrCh, MAX_PATH) > 0 then
        s1 := arrCh;
      if GetFileAttributes(pchar(s1)) <> maxCard then begin
        log('Central MES file "' + s1 + '" shall be used!');
        DeleteR(s1, Length(ExtractFileExt(s1)));
        fileName := s1;
      end else
        log('Central MES file "' + s1 + '" not found!');
    end;
    RegCloseKey(hk);
  end;
  if RegOpenKeyEx(HKEY_CURRENT_USER, 'Software\madshi\madExcept', 0, KEY_QUERY_VALUE, hk) = 0 then begin
    if RegGetStr(hk, 'centralMesFile', s1) and (s1 <> '') then begin
      if ExpandEnvironmentStrings(pchar(s1), arrCh, MAX_PATH) > 0 then
        s1 := arrCh;
      if GetFileAttributes(pchar(s1)) <> maxCard then begin
        log('Central MES file "' + s1 + '" shall be used!');
        DeleteR(s1, Length(ExtractFileExt(s1)));
        fileName := s1;
      end else
        log('Central MES file "' + s1 + '" not found!');
    end;
    RegCloseKey(hk);
  end;
  if fileName <> '' then
    with project do begin
      if (GetPrivateProfileString('AppendMapFile', 'Enabled', '´', arrCh, MAX_PATH, pchar(fileName + '.amf')) <> 1) or
         (arrCh[0] <> '´') then begin
        FModified         := true;
        Enabled           := StrToIntDef(arrCh, ord(Enabled)) <> 0;
        if Enabled then begin
          MinDebugInfoOnly   := false;
          NoOwnSettings      := false;
          GeneralShowSetting := ssFullBox;
        end;
        log(fileName + '.amf converted, AppendMapFile = ' + booleanToChar(Enabled));
      end;
      fileName := fileName + '.mes';
      s1 := LoadStrFromFile(fileName);
      if (s1 <> '') and (Length(s1) < MAX_PATH) and (PosStr(#$D#$A, s1) = 0) then begin
        if (Length(s1) = 1) or
           ( ((s1[2] <> ':')                  ) and
             ((s1[1] <> '\') or (s1[2] <> '\'))     ) then
          if s1[1] = '\' then
                 s1 := ExtractFileDrive(fileName) + s1
          else   s1 := ExtractFilePath (fileName) + s1;
        if GetFileAttributes(pchar(s1)) <> dword(-1) then begin
          log('MES file redirection to "' + s1 + '"');
          fileName := s1;
        end;
      end;
      IniGetBoolean('GeneralSettings', 'MadExcept2Only',                   MadExcept2Only       );
      IniGetBoolean('GeneralSettings', 'HandleExceptions',                 Enabled              );
      b1 := not MinDebugInfoOnly;
      IniGetBoolean('GeneralSettings', 'AppendMapFileToBinary',            b1                   );
      MinDebugInfoOnly := not b1;
      IniGetBoolean('GeneralSettings', 'NoOwnMadExceptSettings',           NoOwnSettings        );
      IniGetBoolean('GeneralSettings', 'CheckFileCrc',                     CheckFileCrc         );
      IniGetBoolean('GeneralSettings', 'CheckForFrozenMainThread',         CheckForFreeze       );
      IniGetDword  ('GeneralSettings', 'FreezeTimeout',                    FreezeTimeout        );
      IniGetBoolean('GeneralSettings', 'AutomaticallySaveBugReport',       AutoSave             );
      IniGetBoolean('GeneralSettings', 'AutoSaveBugReportIfNotSent',       AutoSaveIfNotSent    );
      IniGetBoolean('GeneralSettings', 'AutomaticallyMailBugReport',       AutoSend             );
      IniGetBoolean('GeneralSettings', 'AutoMailProgressBox',              AutoSendPrgrBox      );
      IniGetBoolean('GeneralSettings', 'CopyBugReportToClipboard',         AutoClipboard        );
      IniGetBoolean('GeneralSettings', 'SuspendAllRunningThreads',         SuspendThreads       );
      IniGetBoolean('GeneralSettings', 'ShowPleaseWaitBox',                ShowPleaseWaitBox    );
      IniGetStr    ('GeneralSettings', 'PleaseWaitIcon',                   PleaseWaitIcon       );
      IniGetBoolean('GeneralSettings', 'AutomaticallyContinueApplication', AutoContinue         );
      IniGetDword  ('GeneralSettings', 'AutomaticallyRestartApplication',  AutoRestart          );
      IniGetDword  ('GeneralSettings', 'AutomaticallyCloseApplication',    AutoClose            );
      IniGetBoolean('ExceptionBox',    'ShowButtonMailBugReport',          SendBtnVisible       );
      IniGetBoolean('ExceptionBox',    'ShowButtonSaveBugReport',          SaveBtnVisible       );
      IniGetBoolean('ExceptionBox',    'ShowButtonPrintBugReport',         PrintBtnVisible      );
      IniGetBoolean('ExceptionBox',    'ShowButtonShowBugReport',          ShowBtnVisible       );
      IniGetBoolean('ExceptionBox',    'ShowButtonContinueApplication',    ContinueBtnVisible   );
      IniGetBoolean('ExceptionBox',    'ShowButtonRestartApplication',     RestartBtnVisible    );
      IniGetBoolean('ExceptionBox',    'ShowButtonCloseApplication',       CloseBtnVisible      );
      IniGetStr    ('ExceptionBox',    'IconButtonSendBugReport',          SendBtnIcon          );
      IniGetStr    ('ExceptionBox',    'IconButtonSaveBugReport',          SaveBtnIcon          );
      IniGetStr    ('ExceptionBox',    'IconButtonPrintBugReport',         PrintBtnIcon         );
      IniGetStr    ('ExceptionBox',    'IconButtonShowBugReport',          ShowBtnIcon          );
      IniGetStr    ('ExceptionBox',    'IconButtonContinueApplication',    ContinueBtnIcon      );
      IniGetStr    ('ExceptionBox',    'IconButtonCantContinueApplication',CantContinueBtnIcon  );
      IniGetStr    ('ExceptionBox',    'IconButtonRestartApplication',     RestartBtnIcon       );
      IniGetStr    ('ExceptionBox',    'IconButtonCloseApplication',       CloseBtnIcon         );
      c1 := ord(FocusedButton);
      IniGetDword  ('ExceptionBox',    'FocusedButton',                    c1                   );
      FocusedButton := TMEButton(c1);
      if not IniGetStr    ('ExceptionBox',    'SendAssistant',                    SendAssistant        ) then
             IniGetStr    ('ExceptionBox',    'SendAssistent',                    SendAssistant        );
      if not IniGetStr    ('ExceptionBox',    'SaveAssistant',                    SaveAssistant        ) then
             IniGetStr    ('ExceptionBox',    'SaveAssistent',                    SaveAssistant        );
      if not IniGetStr    ('ExceptionBox',    'PrintAssistant',                   PrintAssistant       ) then
             IniGetStr    ('ExceptionBox',    'PrintAssistent',                   PrintAssistant       );
      IniGetBoolean('ExceptionBox',    'AutomaticallyShowBugReport',       AutoShowBugReport    );
      IniGetBoolean('ExceptionBox',    'NoOwnerDrawButtons',               NoOwnerDrawButtons   );
      IniGetStr    ('ExceptionBox',    'BigExceptionIcon',                 BigIcon              );
      IniGetStr    ('GeneralSettings', 'MailAddress',                      MailAddr             );
      IniGetBoolean('GeneralSettings', 'SendInBackground',                 SendInBackground     );
      IniGetStr    ('GeneralSettings', 'Send32Icon',                       Send32Icon           );
      IniGetBoolean('GeneralSettings', 'MailAsSmtpServer',                 MailAsSmtpServer     );
      IniGetBoolean('GeneralSettings', 'MailAsSmtpClient',                 MailAsSmtpClient     );
      IniGetBoolean('GeneralSettings', 'UploadViaHttp',                    UploadViaHttp        );
      IniGetBoolean('GeneralSettings', 'MailViaMapi',                      MailViaMapi          );
      IniGetBoolean('GeneralSettings', 'MailViaMailto',                    MailViaMailto        );
      IniGetStr    ('GeneralSettings', 'SmtpServer',                       SmtpServer           );
      IniGetDword  ('GeneralSettings', 'SmtpPort',                         dword(SmtpPort)      );
      IniGetStr    ('GeneralSettings', 'SmtpAccount',                      SmtpAccount          );
      IniGetStr    ('GeneralSettings', 'SmtpPassword',                     SmtpPassword         );
      IniGetStr    ('GeneralSettings', 'HttpServer',                       HttpServer           );
      IniGetDword  ('GeneralSettings', 'HttpPort',                         dword(HttpPort)      );
      IniGetStr    ('GeneralSettings', 'HttpAccount',                      HttpAccount          );
      IniGetStr    ('GeneralSettings', 'HttpPassword',                     HttpPassword         );
      IniGetBoolean('GeneralSettings', 'AttachBugReport',                  AttachBugReport      );
      IniGetBoolean('GeneralSettings', 'AttachBugReportFile',              AttachBugReportFile  );
      IniGetBoolean('GeneralSettings', 'DeleteBugReportFile',              DeleteBugReportFile  );
      IniGetStr    ('GeneralSettings', 'BugReportSendAs',                  BugReportSendAs      );
      IniGetStr    ('GeneralSettings', 'BugReportZip',                     BugReportZip         );
      IniGetDword  ('GeneralSettings', 'ScreenShotDepth',                  dword(ScreenShotDepth));
      IniGetBoolean('GeneralSettings', 'ScreenShotAppOnly',                ScreenShotAppOnly    );
      IniGetStr    ('GeneralSettings', 'ScreenShotSendAs',                 ScreenShotSendAs     );
      IniGetStr    ('GeneralSettings', 'ScreenShotZip',                    ScreenShotZip        );
      IniGetStr    ('GeneralSettings', 'AdditionalAttachments',            AdditionalAttachs    );
      IniGetStr    ('GeneralSettings', 'BugReportFile',                    BugReportFile        );
      IniGetBoolean('GeneralSettings', 'AppendBugReports',                 AppendBugReports     );
      c1 := BugReportFileSize;
      BugReportFileSize := maxCard;
      IniGetDword  ('GeneralSettings', 'BugReportFileSize',                BugReportFileSize    );
      b1 := BugReportFileSize = maxCard;
      if b1 then begin
        BugReportFileSize := c1;
        if ord(FocusedButton) >= 2 then
          FocusedButton := TMEButton(ord(FocusedButton) + 1);
      end;
      IniGetBoolean('GeneralSettings', 'DontSaveDuplicateExceptions',      NoDupExcepts         );
      IniGetBoolean('GeneralSettings', 'DontSaveDuplicateFreezings',       NoDupFreezes         );
      c1 := dword(DupExceptDef);
      IniGetDword  ('GeneralSettings', 'DuplicateExceptionDefinition',     c1                   );
      DupExceptDef := TMEDupDef(c1);
      c1 := dword(DupFreezeDef);
      IniGetDword  ('GeneralSettings', 'DuplicateFreezeDefinition',        c1                   );
      DupFreezeDef := TMEDupDef(c1);
      IniGetBoolean('BugReport',       'ListThreads',                      ListThreads          );
      IniGetBoolean('BugReport',       'ShowCpuRegisters',                 ShowCpuRegisters     );
      IniGetBoolean('BugReport',       'ShowStackDump',                    ShowStackDump        );
      IniGetBoolean('BugReport',       'Disassembly',                      ShowDisAsm           );
      IniGetBoolean('BugReport',       'HideUglyItems',                    HideUglyItems        );
      IniGetBoolean('BugReport',       'ShowRelativeAddrs',                ShowRelativeAddrs    );
      IniGetBoolean('BugReport',       'ShowRelativeLines',                ShowRelativeLines    );
      IniGetBoolean('BugReport',       'FormatDisassembly',                FormatDisassembly    );
      IniGetDword  ('BugReport',       'LimitDisassembly',                 dword(LimitDisassembly));
      IniGetStr    ('BugReport',       'EnabledPlugins',                   EnabledPlugins       );
      if IniGetBoolean('BugReport',       'ListModules',                      b1                   ) and b1 and
         (not SubTextExists(EnabledPlugins, 'modules')) then
        if EnabledPlugins = '' then
             EnabledPlugins := 'modules'
        else EnabledPlugins := EnabledPlugins + '|modules';
      if IniGetBoolean('BugReport',       'ListHardware',                     b1                   ) and b1 and
         (not SubTextExists(EnabledPlugins, 'hardware')) then
        if EnabledPlugins = '' then
             EnabledPlugins := 'hardware'
        else EnabledPlugins := EnabledPlugins + '|hardware';
      IniGetStr    ('Filters',         'Filter1ExceptionClasses',          Filter1Classes       );
      IniGetBoolean('Filters',         'Filter1DontCreateBugReport',       Filter1NoBugReport   );
      IniGetBoolean('Filters',         'Filter1DontCreateScreenshot',      Filter1NoScreenshot  );
      IniGetBoolean('Filters',         'Filter1DontSuspendThreads',        Filter1NoSuspend     );
      IniGetBoolean('Filters',         'Filter1DontCallHandlers',          Filter1NoHandlers    );
      c1 := ord(Filter1ShowSetting);
      IniGetDword  ('Filters',         'Filter1ShowBox',                   c1                   );
      Filter1ShowSetting := TMEShowSetting(c1);
      IniGetStr    ('Filters',         'Filter1Assis',                     Filter1Assis         );
      IniGetStr    ('Filters',         'Filter2ExceptionClasses',          Filter2Classes       );
      IniGetBoolean('Filters',         'Filter2DontCreateBugReport',       Filter2NoBugReport   );
      IniGetBoolean('Filters',         'Filter2DontCreateScreenshot',      Filter2NoScreenshot  );
      IniGetBoolean('Filters',         'Filter2DontSuspendThreads',        Filter2NoSuspend     );
      IniGetBoolean('Filters',         'Filter2DontCallHandlers',          Filter2NoHandlers    );
      c1 := ord(Filter2ShowSetting);
      IniGetDword  ('Filters',         'Filter2ShowBox',                   c1                   );
      Filter2ShowSetting := TMEShowSetting(c1);
      IniGetStr    ('Filters',         'Filter2Assis',                     Filter2Assis         );
      IniGetBoolean('Filters',         'GeneralDontCreateBugReport',       GeneralNoBugReport   );
      IniGetBoolean('Filters',         'GeneralDontCreateScreenshot',      GeneralNoScreenshot  );
      IniGetBoolean('Filters',         'GeneralDontSuspendThreads',        GeneralNoSuspend     );
      IniGetBoolean('Filters',         'GeneralDontCallHandlers',          GeneralNoHandlers    );
      if IniGetBoolean('GeneralSettings', 'ShowExceptionBox',                 b1                  ) then
        if b1 then
             GeneralShowSetting := ssFullBox
        else GeneralShowSetting := ssNothing;
      c1 := ord(GeneralShowSetting);
      IniGetDword  ('Filters',         'GeneralShowBox',                   c1                   );
      GeneralShowSetting := TMEShowSetting(c1);
      IniGetStr    ('Filters',         'GeneralAssis',                     GeneralAssis         );
      if IniGetStr('Assistants', 'Assistant1', s1) then begin
        Assistants := nil;
        i1 := 0;
        while true do begin
          s1 := '';
          IniGetStr    ('Assistants',      'Assistant' + IntToStr(i1 + 1),     s1                   );
          if s1 = '' then
            break;
          SetLength(Assistants, i1 + 1);
          Assistants[i1] := s1;
          inc(i1);
        end;
      end else
        if IniGetStr('Assistents', 'Assistent1', s1) then begin
          Assistants := nil;
          i1 := 0;
          while true do begin
            s1 := '';
            IniGetStr    ('Assistents',      'Assistent' + IntToStr(i1 + 1),     s1                   );
            if s1 = '' then
              break;
            SetLength(Assistants, i1 + 1);
            Assistants[i1] := s1;
            inc(i1);
          end;
        end;
      if IniGetStr('Assistants', 'Forms1', s1) then begin
        Forms := nil;
        i1 := 0;
        while true do begin
          s1 := '';
          IniGetStr    ('Assistants',      'Forms' + IntToStr(i1 + 1),         s1                   );
          if s1 = '' then
            break;
          SetLength(Forms, i1 + 1);
          Forms[i1] := s1;
          inc(i1);
        end;
      end else
        if IniGetStr('Assistents', 'Forms1', s1) then begin
          Forms := nil;
          i1 := 0;
          while true do begin
            s1 := '';
            IniGetStr    ('Assistents',      'Forms' + IntToStr(i1 + 1),         s1                   );
            if s1 = '' then
              break;
            SetLength(Forms, i1 + 1);
            Forms[i1] := s1;
            inc(i1);
          end;
        end;
      IniGetStr    ('ExceptionBox',    'TitleBar',                         TitleBar             );
      IniGetStr    ('ExceptionBox',    'ExceptionMessage',                 ExceptMsg            );
      IniGetStr    ('ExceptionBox',    'FrozenMessage',                    FrozenMsg            );
      IniGetStr    ('ExceptionBox',    'BitFaultMsg',                      BitFaultMsg          );
      IniGetStr    ('ExceptionBox',    'MailBugReportText',                SendBtnCaption       );
      IniGetStr    ('ExceptionBox',    'SaveBugReportText',                SaveBtnCaption       );
      IniGetStr    ('ExceptionBox',    'PrintBugReportText',               PrintBtnCaption      );
      IniGetStr    ('ExceptionBox',    'ShowBugReportText',                ShowBtnCaption       );
      IniGetStr    ('ExceptionBox',    'ContinueApplicationText',          ContinueBtnCaption   );
      IniGetStr    ('ExceptionBox',    'RestartApplicationText',           RestartBtnCaption    );
      IniGetStr    ('ExceptionBox',    'CloseApplicationText',             CloseBtnCaption      );
      IniGetStr    ('GeneralSettings', 'OkBtnText',                        OkBtnCaption         );
      IniGetStr    ('GeneralSettings', 'DetailsBtnText',                   DetailsBtnCaption    );
      IniGetStr    ('GeneralSettings', 'PleaseWaitTitle',                  PleaseWaitTitle      );
      IniGetStr    ('GeneralSettings', 'PleaseWaitText',                   PleaseWaitText       );
      IniGetStr    ('GeneralSettings', 'MailSubject',                      MailSubject          );
      IniGetStr    ('GeneralSettings', 'MailBody',                         MailBody             );
      IniGetStr    ('GeneralSettings', 'SendBoxTitle',                     SendBoxTitle         );
      IniGetStr    ('GeneralSettings', 'PrepareAttachMsg',                 PrepareAttachMsg     );
      IniGetStr    ('GeneralSettings', 'MxLookupMsg',                      MxLookupMsg          );
      IniGetStr    ('GeneralSettings', 'ConnectMsg',                       ConnectMsg           );
      IniGetStr    ('GeneralSettings', 'AuthMsg',                          AuthMsg              );
      IniGetStr    ('GeneralSettings', 'SendMailMsg',                      SendMailMsg          );
      IniGetStr    ('GeneralSettings', 'FieldsMsg',                        FieldsMsg            );
      IniGetStr    ('GeneralSettings', 'SendAttachMsg',                    SendAttachMsg        );
      IniGetStr    ('GeneralSettings', 'SendFinalizeMsg',                  SendFinalizeMsg      );
      IniGetStr    ('GeneralSettings', 'MailFailureMsg',                   SendFailureMsg       );
      IniGetStr    ('GeneralSettings', 'VersionVariable',                  VersionVar           );
    end;
  if project.MinDebugInfoOnly then begin
    project.ShowRelativeAddrs := true;
    project.HideUglyItems     := false;
  end;
  unindentLog;
end;

procedure SaveSettingsToIni(fileName: string; const project: TProject);

  procedure IniSetDword(section, key: string; value: dword);
  begin
    WritePrivateProfileString(pchar(section), pchar(key), pchar(IntToStr(value)), pchar(fileName));
    log('[' + section + ']\' + key + ' := ' + IntToStr(value));
  end;

  procedure IniSetBoolean(section, key: string; value: boolean);
  begin
    WritePrivateProfileString(pchar(section), pchar(key), pchar(IntToStr(ord(value))), pchar(fileName));
    log('[' + section + ']\' + key + ' := ' + booleanToChar(value));
  end;

  procedure IniSetStr(section, key, value: string);
  begin
    if value <> '' then
      value := EncodeStr(value);
    WritePrivateProfileString(pchar(section), pchar(key), pchar(value), pchar(fileName));
    log('[' + section + ']\' + key + ' := "' + value + '"');
  end;

  procedure IniGetStr(section, key: string; var value: string);
  var pc : pchar;
  begin
    GetMem(pc, 32 * 1024);
    if (GetPrivateProfileString(pchar(section), pchar(key), '´', pc, 32 * 1024, pchar(fileName)) <> 1) or
       (pc[0] <> '´') then begin
      value := pc;
      log('[' + section + ']\' + key + ' = "' + value + '"');
      value := DecodeStr(value);
    end;
    FreeMem(pc);
  end;

  function IniDelVal(section, key: string) : boolean;
  var s1 : string;
  begin
    IniGetStr(section, key, s1);
    result := (s1 <> '') and WritePrivateProfileString(pchar(section), pchar(key), nil, pchar(fileName));
    log('del [' + section + ']\' + key + ' -> ' + booleanToChar(result));
  end;

  procedure IniDelSection(section: string);
  begin
    WritePrivateProfileString(pchar(section), nil, nil, pchar(fileName));
    log('del [' + section + ']\*');
  end;

var i1 : integer;
    s1 : string;
begin
  log('SaveSettingsToIni(' + fileName + '):'); indentLog;
  if (fileName <> '') and (not project.MadExcept2Only) then
    with project do begin
      SetFileAttributes(pchar(fileName + '.amf'), 0);
      DeleteFile(fileName + '.amf');
      fileName := fileName + '.mes';
      s1 := LoadStrFromFile(fileName);
      if (s1 <> '') and (Length(s1) < MAX_PATH) and (PosStr(#$D#$A, s1) = 0) then begin
        if (Length(s1) = 1) or
           ( ((s1[2] <> ':')                  ) and
             ((s1[1] <> '\') or (s1[2] <> '\'))     ) then
          if s1[1] = '\' then
                 s1 := ExtractFileDrive(fileName) + s1
          else   s1 := ExtractFilePath (fileName) + s1;
        if GetFileAttributes(pchar(s1)) <> dword(-1) then begin
          log('MES file redirection to "' + s1 + '"');
          fileName := s1;
        end;
      end;
      IniSetBoolean('GeneralSettings', 'HandleExceptions',                 Enabled              );
      IniSetBoolean('GeneralSettings', 'AppendMapFileToBinary',            not MinDebugInfoOnly );
      IniSetBoolean('GeneralSettings', 'NoOwnMadExceptSettings',           NoOwnSettings        );
      IniSetBoolean('GeneralSettings', 'CheckFileCrc',                     CheckFileCrc         );
      IniSetBoolean('GeneralSettings', 'CheckForFrozenMainThread',         CheckForFreeze       );
      IniSetDword  ('GeneralSettings', 'FreezeTimeout',                    FreezeTimeout        );
      IniSetBoolean('GeneralSettings', 'AutomaticallySaveBugReport',       AutoSave             );
      IniSetBoolean('GeneralSettings', 'AutoSaveBugReportIfNotSent',       AutoSaveIfNotSent    );
      IniSetBoolean('GeneralSettings', 'AutomaticallyMailBugReport',       AutoSend             );
      IniSetBoolean('GeneralSettings', 'AutoMailProgressBox',              AutoSendPrgrBox      );
      IniSetBoolean('GeneralSettings', 'CopyBugReportToClipboard',         AutoClipboard        );
      IniSetBoolean('GeneralSettings', 'SuspendAllRunningThreads',         SuspendThreads       );
      IniSetBoolean('GeneralSettings', 'ShowPleaseWaitBox',                ShowPleaseWaitBox    );
      IniSetStr    ('GeneralSettings', 'PleaseWaitIcon',                   PleaseWaitIcon       );
      IniSetBoolean('GeneralSettings', 'AutomaticallyContinueApplication', AutoContinue         );
      IniSetDword  ('GeneralSettings', 'AutomaticallyRestartApplication',  AutoRestart          );
      IniSetDword  ('GeneralSettings', 'AutomaticallyCloseApplication',    AutoClose            );
      IniSetBoolean('ExceptionBox',    'ShowButtonMailBugReport',          SendBtnVisible       );
      IniSetBoolean('ExceptionBox',    'ShowButtonSaveBugReport',          SaveBtnVisible       );
      IniSetBoolean('ExceptionBox',    'ShowButtonPrintBugReport',         PrintBtnVisible      );
      IniSetBoolean('ExceptionBox',    'ShowButtonShowBugReport',          ShowBtnVisible       );
      IniSetBoolean('ExceptionBox',    'ShowButtonContinueApplication',    ContinueBtnVisible   );
      IniSetBoolean('ExceptionBox',    'ShowButtonRestartApplication',     RestartBtnVisible    );
      IniSetBoolean('ExceptionBox',    'ShowButtonCloseApplication',       CloseBtnVisible      );
      IniSetStr    ('ExceptionBox',    'IconButtonSendBugReport',          SendBtnIcon          );
      IniSetStr    ('ExceptionBox',    'IconButtonSaveBugReport',          SaveBtnIcon          );
      IniSetStr    ('ExceptionBox',    'IconButtonPrintBugReport',         PrintBtnIcon         );
      IniSetStr    ('ExceptionBox',    'IconButtonShowBugReport',          ShowBtnIcon          );
      IniSetStr    ('ExceptionBox',    'IconButtonContinueApplication',    ContinueBtnIcon      );
      IniSetStr    ('ExceptionBox',    'IconButtonCantContinueApplication',CantContinueBtnIcon  );
      IniSetStr    ('ExceptionBox',    'IconButtonRestartApplication',     RestartBtnIcon       );
      IniSetStr    ('ExceptionBox',    'IconButtonCloseApplication',       CloseBtnIcon         );
      IniSetDword  ('ExceptionBox',    'FocusedButton',                    dword(FocusedButton) );
      IniSetStr    ('ExceptionBox',    'SendAssistant',                    SendAssistant        );
      IniSetStr    ('ExceptionBox',    'SaveAssistant',                    SaveAssistant        );
      IniSetStr    ('ExceptionBox',    'PrintAssistant',                   PrintAssistant       );
      IniDelVal    ('ExceptionBox',    'SendAssistent');
      IniDelVal    ('ExceptionBox',    'SaveAssistent');
      IniDelVal    ('ExceptionBox',    'PrintAssistent');
      IniSetBoolean('ExceptionBox',    'AutomaticallyShowBugReport',       AutoShowBugReport    );
      IniSetBoolean('ExceptionBox',    'NoOwnerDrawButtons',               NoOwnerDrawButtons   );
      IniSetStr    ('ExceptionBox',    'BigExceptionIcon',                 BigIcon              );
      IniSetStr    ('GeneralSettings', 'MailAddress',                      MailAddr             );
      IniSetBoolean('GeneralSettings', 'SendInBackground',                 SendInBackground     );
      IniSetStr    ('GeneralSettings', 'Send32Icon',                       Send32Icon           );
      IniSetBoolean('GeneralSettings', 'MailAsSmtpServer',                 MailAsSmtpServer     );
      IniSetBoolean('GeneralSettings', 'MailAsSmtpClient',                 MailAsSmtpClient     );
      IniSetBoolean('GeneralSettings', 'UploadViaHttp',                    UploadViaHttp        );
      IniSetBoolean('GeneralSettings', 'MailViaMapi',                      MailViaMapi          );
      IniSetBoolean('GeneralSettings', 'MailViaMailto',                    MailViaMailto        );
      IniSetStr    ('GeneralSettings', 'SmtpServer',                       SmtpServer           );
      IniSetDword  ('GeneralSettings', 'SmtpPort',                         dword(SmtpPort)      );
      IniSetStr    ('GeneralSettings', 'SmtpAccount',                      SmtpAccount          );
      IniSetStr    ('GeneralSettings', 'SmtpPassword',                     SmtpPassword         );
      IniSetStr    ('GeneralSettings', 'HttpServer',                       HttpServer           );
      IniSetDword  ('GeneralSettings', 'HttpPort',                         dword(HttpPort)      );
      IniSetStr    ('GeneralSettings', 'HttpAccount',                      HttpAccount          );
      IniSetStr    ('GeneralSettings', 'HttpPassword',                     HttpPassword         );
      IniSetStr    ('GeneralSettings', 'BugReportFile',                    BugReportFile        );
      IniSetBoolean('GeneralSettings', 'AttachBugReport',                  AttachBugReport      );
      IniSetBoolean('GeneralSettings', 'AttachBugReportFile',              AttachBugReportFile  );
      IniSetBoolean('GeneralSettings', 'DeleteBugReportFile',              DeleteBugReportFile  );
      IniSetStr    ('GeneralSettings', 'BugReportSendAs',                  BugReportSendAs      );
      IniSetStr    ('GeneralSettings', 'BugReportZip',                     BugReportZip         );
      IniSetDword  ('GeneralSettings', 'ScreenShotDepth',                  dword(ScreenShotDepth));
      IniSetBoolean('GeneralSettings', 'ScreenShotAppOnly',                ScreenShotAppOnly    );
      IniSetStr    ('GeneralSettings', 'ScreenShotSendAs',                 ScreenShotSendAs     );
      IniSetStr    ('GeneralSettings', 'ScreenShotZip',                    ScreenShotZip        );
      IniSetStr    ('GeneralSettings', 'AdditionalAttachments',            AdditionalAttachs    );
      IniSetBoolean('GeneralSettings', 'AppendBugReports',                 AppendBugReports     );
      IniSetDword  ('GeneralSettings', 'BugReportFileSize',                BugReportFileSize    );
      IniSetBoolean('GeneralSettings', 'DontSaveDuplicateExceptions',      NoDupExcepts         );
      IniSetBoolean('GeneralSettings', 'DontSaveDuplicateFreezings',       NoDupFreezes         );
      IniSetDword  ('GeneralSettings', 'DuplicateExceptionDefinition',     dword(DupExceptDef)  );
      IniSetDword  ('GeneralSettings', 'DuplicateFreezeDefinition',        dword(DupFreezeDef)  );
      IniSetBoolean('BugReport',       'ListThreads',                      ListThreads          );
      IniSetBoolean('BugReport',       'ListModules',                      SubTextExists(EnabledPlugins, 'modules' ));
      IniSetBoolean('BugReport',       'ListHardware',                     SubTextExists(EnabledPlugins, 'hardware'));
      IniSetBoolean('BugReport',       'ShowCpuRegisters',                 ShowCpuRegisters     );
      IniSetBoolean('BugReport',       'ShowStackDump',                    ShowStackDump        );
      IniSetBoolean('BugReport',       'Disassembly',                      ShowDisAsm           );
      IniSetBoolean('BugReport',       'HideUglyItems',                    HideUglyItems        );
      IniSetBoolean('BugReport',       'ShowRelativeAddrs',                ShowRelativeAddrs    );
      IniSetBoolean('BugReport',       'ShowRelativeLines',                ShowRelativeLines    );
      IniSetBoolean('BugReport',       'FormatDisassembly',                FormatDisassembly    );
      IniSetDword  ('BugReport',       'LimitDisassembly',                 dword(LimitDisassembly));
      IniSetStr    ('BugReport',       'EnabledPlugins',                   EnabledPlugins       );
      IniSetStr    ('Filters',         'Filter1ExceptionClasses',          Filter1Classes       );
      IniSetBoolean('Filters',         'Filter1DontCreateBugReport',       Filter1NoBugReport   );
      IniSetBoolean('Filters',         'Filter1DontCreateScreenshot',      Filter1NoScreenshot  );
      IniSetBoolean('Filters',         'Filter1DontSuspendThreads',        Filter1NoSuspend     );
      IniSetBoolean('Filters',         'Filter1DontCallHandlers',          Filter1NoHandlers    );
      IniSetDword  ('Filters',         'Filter1ShowBox',                   dword(Filter1ShowSetting));
      IniSetStr    ('Filters',         'Filter1Assis',                     Filter1Assis         );
      IniSetStr    ('Filters',         'Filter2ExceptionClasses',          Filter2Classes       );
      IniSetBoolean('Filters',         'Filter2DontCreateBugReport',       Filter2NoBugReport   );
      IniSetBoolean('Filters',         'Filter2DontCreateScreenshot',      Filter2NoScreenshot  );
      IniSetBoolean('Filters',         'Filter2DontSuspendThreads',        Filter2NoSuspend     );
      IniSetBoolean('Filters',         'Filter2DontCallHandlers',          Filter2NoHandlers    );
      IniSetDword  ('Filters',         'Filter2ShowBox',                   dword(Filter2ShowSetting));
      IniSetStr    ('Filters',         'Filter2Assis',                     Filter2Assis         );
      IniSetBoolean('Filters',         'GeneralDontCreateBugReport',       GeneralNoBugReport   );
      IniSetBoolean('Filters',         'GeneralDontCreateScreenshot',      GeneralNoScreenshot  );
      IniSetBoolean('Filters',         'GeneralDontSuspendThreads',        GeneralNoSuspend     );
      IniSetBoolean('Filters',         'GeneralDontCallHandlers',          GeneralNoHandlers    );
      IniSetDword  ('Filters',         'GeneralShowBox',                   dword(GeneralShowSetting));
      IniSetBoolean('GeneralSettings', 'ShowExceptionBox',                 GeneralShowSetting <> ssNothing);
      IniSetStr    ('Filters',         'GeneralAssis',                     GeneralAssis         );
      for i1 := 0 to high(Assistants) do
        IniSetStr    ('Assistants',      'Assistant' + IntToStr(i1 + 1),     Assistants[i1]       );
      for i1 := Length(Assistants) to maxInt - 1 do
        if not IniDelVal('Assistants', 'Assistant' + IntToStr(i1 + 1)) then
          break;
      if Assistants = nil then
        IniSetStr    ('Assistants',      'Assistant1',                       ''                   );
      for i1 := 0 to high(Forms) do
        IniSetStr    ('Assistants',      'Forms' + IntToStr(i1 + 1),         Forms[i1]            );
      for i1 := Length(Forms) to maxInt - 1 do
        if not IniDelVal('Assistants', 'Forms' + IntToStr(i1 + 1)) then
          break;
      if Forms = nil then
        IniSetStr    ('Assistants',      'Forms1',                           ''                   );
      IniDelSection('Assistents');
      IniSetStr    ('ExceptionBox',    'TitleBar',                         TitleBar             );
      IniSetStr    ('ExceptionBox',    'ExceptionMessage',                 ExceptMsg            );
      IniSetStr    ('ExceptionBox',    'FrozenMessage',                    FrozenMsg            );
      IniSetStr    ('ExceptionBox',    'BitFaultMsg',                      BitFaultMsg          );
      IniSetStr    ('ExceptionBox',    'MailBugReportText',                SendBtnCaption       );
      IniSetStr    ('ExceptionBox',    'SaveBugReportText',                SaveBtnCaption       );
      IniSetStr    ('ExceptionBox',    'PrintBugReportText',               PrintBtnCaption      );
      IniSetStr    ('ExceptionBox',    'ShowBugReportText',                ShowBtnCaption       );
      IniSetStr    ('ExceptionBox',    'ContinueApplicationText',          ContinueBtnCaption   );
      IniSetStr    ('ExceptionBox',    'RestartApplicationText',           RestartBtnCaption    );
      IniSetStr    ('ExceptionBox',    'CloseApplicationText',             CloseBtnCaption      );
      IniSetStr    ('GeneralSettings', 'OkBtnText',                        OkBtnCaption         );
      IniSetStr    ('GeneralSettings', 'DetailsBtnText',                   DetailsBtnCaption    );
      IniSetStr    ('GeneralSettings', 'PleaseWaitTitle',                  PleaseWaitTitle      );
      IniSetStr    ('GeneralSettings', 'PleaseWaitText',                   PleaseWaitText       );
      IniSetStr    ('GeneralSettings', 'MailSubject',                      MailSubject          );
      IniSetStr    ('GeneralSettings', 'MailBody',                         MailBody             );
      IniSetStr    ('GeneralSettings', 'SendBoxTitle',                     SendBoxTitle         );
      IniSetStr    ('GeneralSettings', 'PrepareAttachMsg',                 PrepareAttachMsg     );
      IniSetStr    ('GeneralSettings', 'MxLookupMsg',                      MxLookupMsg          );
      IniSetStr    ('GeneralSettings', 'ConnectMsg',                       ConnectMsg           );
      IniSetStr    ('GeneralSettings', 'AuthMsg',                          AuthMsg              );
      IniSetStr    ('GeneralSettings', 'SendMailMsg',                      SendMailMsg          );
      IniSetStr    ('GeneralSettings', 'FieldsMsg',                        FieldsMsg            );
      IniSetStr    ('GeneralSettings', 'SendAttachMsg',                    SendAttachMsg        );
      IniSetStr    ('GeneralSettings', 'SendFinalizeMsg',                  SendFinalizeMsg      );
      IniSetStr    ('GeneralSettings', 'MailFailureMsg',                   SendFailureMsg       );
      IniSetStr    ('GeneralSettings', 'VersionVariable',                  VersionVar           );
      WritePrivateProfileString(nil, nil, nil, pchar(fileName));
    end;
  unindentLog;
end;

function LoadDefaultSettings(var project: TProject) : boolean;
var hk : HKEY;

  function RegGetBinary(len: dword; name: string; var value) : boolean;
  begin
    result := RegQueryValueEx(hk, pchar(name), nil, nil, @value, @len) = 0;
    if result then
      if len = 1 then log(name + ' = ' + booleanToChar(boolean(value)))
      else            log(name + ' = ' + IntToStr     (integer(value)));
  end;

  function RegGetDword(name: string; var value: dword) : boolean;
  begin
    result := RegGetBinary(4, name, value);
  end;

  function RegGetBoolean(name: string; var value: boolean) : boolean;
  begin
    result := RegGetBinary(1, name, value);
  end;

  function RegGetStr(name: string; var value: string) : boolean;
  begin
    result := madExceptPatcher.RegGetStr(hk, name, value);
    if PosText('Assistent', name) > 0 then
      ReplaceStr(value, 'Assistent', 'Assistant');
  end;

var c1 : dword;
    b1 : boolean;
    i1 : integer;
    s1 : string;
begin
  SetDefaultSettings(project);
  log('LoadDefaultSettings:'); indentLog;
  result := RegOpenKeyEx(HKEY_CURRENT_USER, 'Software\madshi\madExcept', 0, KEY_QUERY_VALUE, hk) = 0;
  if result then
    try
      with project do begin
        result := RegGetBoolean('HandleExceptions',                 Enabled              );
        if result then begin
          b1 := not minDebugInfoOnly;
          RegGetBoolean('AppendMapFileToBinary',            b1                   );
          minDebugInfoOnly := not b1;
          RegGetBoolean('NoOwnMadExceptSettings',           NoOwnSettings        );
          RegGetBoolean('CheckFileCrc',                     CheckFileCrc         );
          RegGetBoolean('CheckForFrozenMainThread',         CheckForFreeze       );
          RegGetDword  ('FreezeTimeout',                    FreezeTimeout        );
          RegGetBoolean('AutomaticallySaveBugReport',       AutoSave             );
          RegGetBoolean('AutoSaveBugReportIfNotSent',       AutoSaveIfNotSent    );
          RegGetBoolean('AutomaticallyMailBugReport',       AutoSend             );
          RegGetBoolean('AutoMailProgressBox',              AutoSendPrgrBox      );
          RegGetBoolean('CopyBugReportToClipboard',         AutoClipboard        );
          RegGetBoolean('SuspendAllRunningThreads',         SuspendThreads       );
          RegGetBoolean('ShowPleaseWaitBox',                ShowPleaseWaitBox    );
          RegGetStr    ('PleaseWaitIcon',                   PleaseWaitIcon       );
          RegGetBoolean('AutomaticallyContinueApplication', AutoContinue         );
          RegGetDword  ('AutomaticallyRestartApplication',  AutoRestart          );
          RegGetDword  ('AutomaticallyCloseApplication',    AutoClose            );
          RegGetBoolean('ShowButtonMailBugReport',          SendBtnVisible       );
          RegGetBoolean('ShowButtonSaveBugReport',          SaveBtnVisible       );
          RegGetBoolean('ShowButtonPrintBugReport',         PrintBtnVisible      );
          RegGetBoolean('ShowButtonShowBugReport',          ShowBtnVisible       );
          RegGetBoolean('ShowButtonContinueApplication',    ContinueBtnVisible   );
          RegGetBoolean('ShowButtonRestartApplication',     RestartBtnVisible    );
          RegGetBoolean('ShowButtonCloseApplication',       CloseBtnVisible      );
          RegGetStr    ('IconButtonSendBugReport',          SendBtnIcon          );
          RegGetStr    ('IconButtonSaveBugReport',          SaveBtnIcon          );
          RegGetStr    ('IconButtonPrintBugReport',         PrintBtnIcon         );
          RegGetStr    ('IconButtonShowBugReport',          ShowBtnIcon          );
          RegGetStr    ('IconButtonContinueApplication',    ContinueBtnIcon      );
          RegGetStr    ('IconButtonCantContinueApplication',CantContinueBtnIcon  );
          RegGetStr    ('IconButtonRestartApplication',     RestartBtnIcon       );
          RegGetStr    ('IconButtonCloseApplication',       CloseBtnIcon         );
          c1 := dword(FocusedButton);
          RegGetDword  ('FocusedButton',                    c1                   );
          FocusedButton := TMEButton(c1);
          if not RegGetStr    ('SendAssistant',                    SendAssistant        ) then
                 RegGetStr    ('SendAssistent',                    SendAssistant        );
          if not RegGetStr    ('SaveAssistant',                    SaveAssistant        ) then
                 RegGetStr    ('SaveAssistent',                    SaveAssistant        );
          if not RegGetStr    ('PrintAssistant',                   PrintAssistant       ) then
                 RegGetStr    ('PrintAssistent',                   PrintAssistant       );
          RegGetBoolean('AutomaticallyShowBugReport',       AutoShowBugReport    );
          RegGetBoolean('NoOwnerDrawButtons',               NoOwnerDrawButtons   );
          RegGetStr    ('BigExceptionIcon',                 BigIcon              );
          RegGetStr    ('MailAddress',                      MailAddr             );
          RegGetBoolean('SendInBackground',                 SendInBackground     );
          RegGetStr    ('Send32Icon',                       Send32Icon           );
          RegGetBoolean('MailAsSmtpServer',                 MailAsSmtpServer     );
          RegGetBoolean('MailAsSmtpClient',                 MailAsSmtpClient     );
          RegGetBoolean('UploadViaHttp',                    UploadViaHttp        );
          RegGetBoolean('MailViaMapi',                      MailViaMapi          );
          RegGetBoolean('MailViaMailto',                    MailViaMailto        );
          RegGetStr    ('SmtpServer',                       SmtpServer           );
          RegGetDword  ('SmtpPort',                         dword(SmtpPort)      );
          RegGetStr    ('SmtpAccount',                      SmtpAccount          );
          RegGetStr    ('SmtpPassword',                     SmtpPassword         );
          RegGetStr    ('HttpServer',                       HttpServer           );
          RegGetDword  ('HttpPort',                         dword(HttpPort)      );
          RegGetStr    ('HttpAccount',                      HttpAccount          );
          RegGetStr    ('HttpPassword',                     HttpPassword         );
          RegGetBoolean('AttachBugReport',                  AttachBugReport      );
          RegGetBoolean('AttachBugReportFile',              AttachBugReportFile  );
          RegGetBoolean('DeleteBugReportFile',              DeleteBugReportFile  );
          RegGetStr    ('BugReportSendAs',                  BugReportSendAs      );
          RegGetStr    ('BugReportZip',                     BugReportZip         );
          RegGetDword  ('ScreenShotDepth',                  dword(ScreenShotDepth));
          RegGetBoolean('ScreenShotAppOnly',                ScreenShotAppOnly    );
          RegGetStr    ('ScreenShotSendAs',                 ScreenShotSendAs     );
          RegGetStr    ('ScreenShotZip',                    ScreenShotZip        );
          RegGetStr    ('AdditionalAttachments',            AdditionalAttachs    );
          RegGetStr    ('BugReportFile',                    BugReportFile        );
          RegGetBoolean('AppendBugReports',                 AppendBugReports     );
          c1 := BugReportFileSize;
          BugReportFileSize := maxCard;
          RegGetDword  ('BugReportFileSize',                BugReportFileSize    );
          b1 := BugReportFileSize = maxCard;
          if b1 then begin
            BugReportFileSize := c1;
            if ord(FocusedButton) >= 2 then
              FocusedButton := TMEButton(ord(FocusedButton) + 1);
          end;
          RegGetBoolean('DontSaveDuplicateExceptions',      NoDupExcepts         );
          RegGetBoolean('DontSaveDuplicateFreezings',       NoDupFreezes         );
          c1 := dword(DupExceptDef);
          RegGetDword  ('DuplicateExceptionDefinition',     c1                   );
          DupExceptDef := TMEDupDef(c1);
          c1 := dword(DupFreezeDef);
          RegGetDword  ('DuplicateFreezeDefinition',        c1                   );
          DupFreezeDef := TMEDupDef(c1);
          RegGetBoolean('ListThreads',                      ListThreads          );
          RegGetBoolean('ShowCpuRegisters',                 ShowCpuRegisters     );
          RegGetBoolean('ShowStackDump',                    ShowStackDump        );
          RegGetBoolean('Disassembly',                      ShowDisAsm           );
          RegGetBoolean('HideUglyItems',                    HideUglyItems        );
          RegGetBoolean('ShowRelativeAddrs',                ShowRelativeAddrs    );
          RegGetBoolean('ShowRelativeLines',                ShowRelativeLines    );
          RegGetBoolean('FormatDisassembly',                FormatDisassembly    );
          RegGetDword  ('LimitDisassembly',                 dword(LimitDisassembly));
          RegGetStr    ('EnabledPlugins',                   EnabledPlugins       );
          if RegGetBoolean('ListModules',                      b1                   ) and b1 and
             (not SubTextExists(EnabledPlugins, 'modules')) then
            if EnabledPlugins = '' then
                 EnabledPlugins := 'modules'
            else EnabledPlugins := EnabledPlugins + '|modules';
          if RegGetBoolean('ListHardware',                     b1                   ) and b1 and
             (not SubTextExists(EnabledPlugins, 'hardware')) then
            if EnabledPlugins = '' then
                 EnabledPlugins := 'hardware'
            else EnabledPlugins := EnabledPlugins + '|hardware';
          RegGetStr    ('Filter1ExceptionClasses',          Filter1Classes       );
          RegGetBoolean('Filter1DontCreateBugReport',       Filter1NoBugReport   );
          RegGetBoolean('Filter1DontCreateScreenshot',      Filter1NoScreenshot  );
          RegGetBoolean('Filter1DontSuspendThreads',        Filter1NoSuspend     );
          RegGetBoolean('Filter1DontCallHandlers',          Filter1NoHandlers    );
          c1 := ord(Filter1ShowSetting);
          RegGetDword  ('Filter1ShowBox',                   c1                   );
          Filter1ShowSetting := TMEShowSetting(c1);
          RegGetStr    ('Filter1Assis',                     Filter1Assis         );
          RegGetStr    ('Filter2ExceptionClasses',          Filter2Classes       );
          RegGetBoolean('Filter2DontCreateBugReport',       Filter2NoBugReport   );
          RegGetBoolean('Filter2DontCreateScreenshot',      Filter2NoScreenshot  );
          RegGetBoolean('Filter2DontSuspendThreads',        Filter2NoSuspend     );
          RegGetBoolean('Filter2DontCallHandlers',          Filter2NoHandlers    );
          c1 := ord(Filter2ShowSetting);
          RegGetDword  ('Filter2ShowBox',                   c1                   );
          Filter2ShowSetting := TMEShowSetting(c1);
          RegGetStr    ('Filter2Assis',                     Filter2Assis         );
          RegGetBoolean('GeneralDontCreateBugReport',       GeneralNoBugReport   );
          RegGetBoolean('GeneralDontCreateScreenshot',      GeneralNoScreenshot  );
          RegGetBoolean('GeneralDontSuspendThreads',        GeneralNoSuspend     );
          RegGetBoolean('GeneralDontCallHandlers',          GeneralNoHandlers    );
          if RegGetBoolean('ShowExceptionBox',                 b1                   ) then
            if b1 then
                 GeneralShowSetting := ssFullBox
            else GeneralShowSetting := ssNothing;
          c1 := ord(GeneralShowSetting);
          RegGetDword  ('GeneralShowBox',                   c1                   );
          GeneralShowSetting := TMEShowSetting(c1);
          RegGetStr    ('GeneralAssis',                     GeneralAssis         );
          if RegGetStr('Assistant1', s1) then begin
            Assistants := nil;
            i1 := 0;
            while true do begin
              s1 := '';
              RegGetStr    ('Assistant' + IntToStr(i1 + 1),     s1                   );
              if s1 = '' then
                break;
              SetLength(Assistants, i1 + 1);
              Assistants[i1] := s1;
              inc(i1);
            end;
          end else
            if RegGetStr('Assistent1', s1) then begin
              Assistants := nil;
              i1 := 0;
              while true do begin
                s1 := '';
                RegGetStr    ('Assistent' + IntToStr(i1 + 1),     s1                   );
                if s1 = '' then
                  break;
                SetLength(Assistants, i1 + 1);
                Assistants[i1] := s1;
                inc(i1);
              end;
            end;
          if RegGetStr('Forms1', s1) then begin
            Forms := nil;
            i1 := 0;
            while true do begin
              s1 := '';
              RegGetStr    ('Forms' + IntToStr(i1 + 1),         s1                   );
              if s1 = '' then
                break;
              SetLength(Forms, i1 + 1);
              Forms[i1] := s1;
              inc(i1);
            end;
          end;
          RegGetStr    ('TitleBar',                         TitleBar             );
          RegGetStr    ('ExceptionMessage',                 ExceptMsg            );
          RegGetStr    ('FrozenMessage',                    FrozenMsg            );
          RegGetStr    ('BitFaultMsg',                      BitFaultMsg          );
          RegGetStr    ('MailBugReportText',                SendBtnCaption       );
          RegGetStr    ('SaveBugReportText',                SaveBtnCaption       );
          RegGetStr    ('PrintBugReportText',               PrintBtnCaption      );
          RegGetStr    ('ShowBugReportText',                ShowBtnCaption       );
          RegGetStr    ('ContinueApplicationText',          ContinueBtnCaption   );
          RegGetStr    ('RestartApplicationText',           RestartBtnCaption    );
          RegGetStr    ('CloseApplicationText',             CloseBtnCaption      );
          RegGetStr    ('OkBtnText',                        OkBtnCaption         );
          RegGetStr    ('DetailsBtnText',                   DetailsBtnCaption    );
          RegGetStr    ('PleaseWaitTitle',                  PleaseWaitTitle      );
          RegGetStr    ('PleaseWaitText',                   PleaseWaitText       );
          RegGetStr    ('MailSubject',                      MailSubject          );
          RegGetStr    ('MailBody',                         MailBody             );
          RegGetStr    ('SendBoxTitle',                     SendBoxTitle         );
          RegGetStr    ('PrepareAttachMsg',                 PrepareAttachMsg     );
          RegGetStr    ('MxLookupMsg',                      MxLookupMsg          );
          RegGetStr    ('ConnectMsg',                       ConnectMsg           );
          RegGetStr    ('AuthMsg',                          AuthMsg              );
          RegGetStr    ('SendMailMsg',                      SendMailMsg          );
          RegGetStr    ('FieldsMsg',                        FieldsMsg            );
          RegGetStr    ('SendAttachMsg',                    SendAttachMsg        );
          RegGetStr    ('SendFinalizeMsg',                  SendFinalizeMsg      );
          RegGetStr    ('MailFailureMsg',                   SendFailureMsg       );
          RegGetStr    ('VersionVariable',                  VersionVar           );
        end;
      end;
    finally RegCloseKey(hk) end;
  if project.MinDebugInfoOnly then begin
    project.ShowRelativeAddrs := true;
    project.HideUglyItems     := false;
  end;
  unindentLog;
end;

procedure SaveDefaultSettings(const project: TProject);
var hk : HKEY;

  procedure RegSetBin(len: dword; name: string; const value);
  var typ : dword;
  begin
    if len <> 4 then typ := REG_BINARY
    else             typ := REG_DWORD;
    RegSetValueEx(hk, pchar(name), 0, typ, @value, len);
    case len of
      1  : log(name + ' := ' + booleanToChar(boolean(value)));
      4  : log(name + ' := ' + IntToStr(integer(value)));
      else log(name + ' := <' + IntToStr(len) + ' bytes of binary data>');
    end;
  end;

  procedure RegSetDword(name: string; const value: dword);
  begin
    RegSetBin(4, name, value);
  end;

  procedure RegSetBoolean(name: string; const value: boolean);
  begin
    RegSetBin(1, name, value);
  end;

  procedure RegSetStr(name, value: string);
  begin
    RegSetValueEx(hk, pchar(name), 0, REG_SZ, pchar(value), Length(value) + 1);
    log(name + ' := "' + value + '"');
  end;

  function RegDelVal(name: string) : boolean;
  begin
    result := RegDeleteValue(hk, pchar(name)) = 0;
    log('del ' + name + ' -> ' + booleanToChar(result));
  end;

var c1 : dword;
    i1 : integer;
begin
  log('SaveDefaultSettings'); indentLog;
  c1 := RegCreateKeyEx(HKEY_CURRENT_USER, 'Software\madshi\madExcept', 0, nil, 0, KEY_ALL_ACCESS, nil, hk, @c1);
  if c1 = 0 then
    try
      with project do begin
        RegSetBoolean('HandleExceptions',                 Enabled              );
        RegSetBoolean('AppendMapFileToBinary',            not MinDebugInfoOnly );
        RegSetBoolean('NoOwnMadExceptSettings',           NoOwnSettings        );
        RegSetBoolean('CheckFileCrc',                     CheckFileCrc         );
        RegSetBoolean('CheckForFrozenMainThread',         CheckForFreeze       );
        RegSetDword  ('FreezeTimeout',                    FreezeTimeout        );
        RegSetBoolean('AutomaticallySaveBugReport',       AutoSave             );
        RegSetBoolean('AutoSaveBugReportIfNotSent',       AutoSaveIfNotSent    );
        RegSetBoolean('AutomaticallyMailBugReport',       AutoSend             );
        RegSetBoolean('AutoMailProgressBox',              AutoSendPrgrBox      );
        RegSetBoolean('CopyBugReportToClipboard',         AutoClipboard        );
        RegSetBoolean('SuspendAllRunningThreads',         SuspendThreads       );
        RegSetBoolean('ShowPleaseWaitBox',                ShowPleaseWaitBox    );
        RegSetStr    ('PleaseWaitIcon',                   PleaseWaitIcon       );
        RegSetBoolean('AutomaticallyContinueApplication', AutoContinue         );
        RegSetDword  ('AutomaticallyRestartApplication',  AutoRestart          );
        RegSetDword  ('AutomaticallyCloseApplication',    AutoClose            );
        RegSetBoolean('ShowButtonMailBugReport',          SendBtnVisible       );
        RegSetBoolean('ShowButtonSaveBugReport',          SaveBtnVisible       );
        RegSetBoolean('ShowButtonPrintBugReport',         PrintBtnVisible      );
        RegSetBoolean('ShowButtonShowBugReport',          ShowBtnVisible       );
        RegSetBoolean('ShowButtonContinueApplication',    ContinueBtnVisible   );
        RegSetBoolean('ShowButtonRestartApplication',     RestartBtnVisible    );
        RegSetBoolean('ShowButtonCloseApplication',       CloseBtnVisible      );
        RegSetStr    ('IconButtonSendBugReport',          SendBtnIcon          );
        RegSetStr    ('IconButtonSaveBugReport',          SaveBtnIcon          );
        RegSetStr    ('IconButtonPrintBugReport',         PrintBtnIcon         );
        RegSetStr    ('IconButtonShowBugReport',          ShowBtnIcon          );
        RegSetStr    ('IconButtonContinueApplication',    ContinueBtnIcon      );
        RegSetStr    ('IconButtonCantContinueApplication',CantContinueBtnIcon  );
        RegSetStr    ('IconButtonRestartApplication',     RestartBtnIcon       );
        RegSetStr    ('IconButtonCloseApplication',       CloseBtnIcon         );
        RegSetDword  ('FocusedButton',                    dword(FocusedButton) );
        RegSetStr    ('SendAssistant',                    SendAssistant        );
        RegSetStr    ('SaveAssistant',                    SaveAssistant        );
        RegSetStr    ('PrintAssistant',                   PrintAssistant       );
        RegDelVal    ('SendAssistent');
        RegDelVal    ('SaveAssistent');
        RegDelVal    ('PrintAssistent');
        RegSetBoolean('AutomaticallyShowBugReport',       AutoShowBugReport    );
        RegSetBoolean('NoOwnerDrawButtons',               NoOwnerDrawButtons   );
        RegSetStr    ('BigExceptionIcon',                 BigIcon              );
        RegSetStr    ('MailAddress',                      MailAddr             );
        RegSetBoolean('SendInBackground',                 SendInBackground     );
        RegSetStr    ('Send32Icon',                       Send32Icon           );
        RegSetBoolean('MailAsSmtpServer',                 MailAsSmtpServer     );
        RegSetBoolean('MailAsSmtpClient',                 MailAsSmtpClient     );
        RegSetBoolean('UploadViaHttp',                    UploadViaHttp        );
        RegSetBoolean('MailViaMapi',                      MailViaMapi          );
        RegSetBoolean('MailViaMailto',                    MailViaMailto        );
        RegSetStr    ('SmtpServer',                       SmtpServer           );
        RegSetDword  ('SmtpPort',                         dword(SmtpPort)      );
        RegSetStr    ('SmtpAccount',                      SmtpAccount          );
        RegSetStr    ('SmtpPassword',                     SmtpPassword         );
        RegSetStr    ('HttpServer',                       HttpServer           );
        RegSetDword  ('HttpPort',                         dword(HttpPort)      );
        RegSetStr    ('HttpAccount',                      HttpAccount          );
        RegSetStr    ('HttpPassword',                     HttpPassword         );
        RegSetBoolean('AttachBugReport',                  AttachBugReport      );
        RegSetBoolean('AttachBugReportFile',              AttachBugReportFile  );
        RegSetBoolean('DeleteBugReportFile',              DeleteBugReportFile  );
        RegSetStr    ('BugReportSendAs',                  BugReportSendAs      );
        RegSetStr    ('BugReportZip',                     BugReportZip         );
        RegSetDword  ('ScreenShotDepth',                  dword(ScreenShotDepth));
        RegSetBoolean('ScreenShotAppOnly',                ScreenShotAppOnly    );
        RegSetStr    ('ScreenShotSendAs',                 ScreenShotSendAs     );
        RegSetStr    ('ScreenShotZip',                    ScreenShotZip        );
        RegSetStr    ('AdditionalAttachments',            AdditionalAttachs    );
        RegSetStr    ('BugReportFile',                    BugReportFile        );
        RegSetBoolean('AppendBugReports',                 AppendBugReports     );
        RegSetDword  ('BugReportFileSize',                BugReportFileSize    );
        RegSetBoolean('DontSaveDuplicateExceptions',      NoDupExcepts         );
        RegSetBoolean('DontSaveDuplicateFreezings',       NoDupFreezes         );
        RegSetDword  ('DuplicateExceptionDefinition',     dword(DupExceptDef)  );
        RegSetDword  ('DuplicateFreezeDefinition',        dword(DupFreezeDef)  );
        RegSetBoolean('ListThreads',                      ListThreads          );
        RegSetBoolean('ListModules',                      SubTextExists(EnabledPlugins, 'modules' ));
        RegSetBoolean('ListHardware',                     SubTextExists(EnabledPlugins, 'hardware'));
        RegSetBoolean('ShowCpuRegisters',                 ShowCpuRegisters     );
        RegSetBoolean('ShowStackDump',                    ShowStackDump        );
        RegSetBoolean('Disassembly',                      ShowDisAsm           );
        RegSetBoolean('HideUglyItems',                    HideUglyItems        );
        RegSetBoolean('ShowRelativeAddrs',                ShowRelativeAddrs    );
        RegSetBoolean('ShowRelativeLines',                ShowRelativeLines    );
        RegSetBoolean('FormatDisassembly',                FormatDisassembly    );
        RegSetDword  ('LimitDisassembly',                 dword(LimitDisassembly));
        RegSetStr    ('EnabledPlugins',                   EnabledPlugins       );
        RegSetStr    ('Filter1ExceptionClasses',          Filter1Classes       );
        RegSetBoolean('Filter1DontCreateBugReport',       Filter1NoBugReport   );
        RegSetBoolean('Filter1DontCreateScreenshot',      Filter1NoScreenshot  );
        RegSetBoolean('Filter1DontSuspendThreads',        Filter1NoSuspend     );
        RegSetBoolean('Filter1DontCallHandlers',          Filter1NoHandlers    );
        RegSetDword  ('Filter1ShowBox',                   dword(Filter1ShowSetting));
        RegSetStr    ('Filter1Assis',                     Filter1Assis         );
        RegSetStr    ('Filter2ExceptionClasses',          Filter2Classes       );
        RegSetBoolean('Filter2DontCreateBugReport',       Filter2NoBugReport   );
        RegSetBoolean('Filter2DontCreateScreenshot',      Filter2NoScreenshot  );
        RegSetBoolean('Filter2DontSuspendThreads',        Filter2NoSuspend     );
        RegSetBoolean('Filter2DontCallHandlers',          Filter2NoHandlers    );
        RegSetDword  ('Filter2ShowBox',                   dword(Filter2ShowSetting));
        RegSetStr    ('Filter2Assis',                     Filter2Assis         );
        RegSetBoolean('GeneralDontCreateBugReport',       GeneralNoBugReport   );
        RegSetBoolean('GeneralDontCreateScreenshot',      GeneralNoScreenshot  );
        RegSetBoolean('GeneralDontSuspendThreads',        GeneralNoSuspend     );
        RegSetBoolean('GeneralDontCallHandlers',          GeneralNoHandlers    );
        RegSetDword  ('GeneralShowBox',                   dword(GeneralShowSetting));
        RegSetBoolean('ShowExceptionBox',                 GeneralShowSetting <> ssNothing);
        RegSetStr    ('GeneralAssis',                     GeneralAssis         );
        for i1 := 0 to high(Assistants) do
          RegSetStr    ('Assistant' + IntToStr(i1 + 1),     Assistants[i1]       );
        for i1 := Length(Assistants) to maxInt - 1 do
          if not RegDelVal('Assistant' + IntToStr(i1 + 1)) then
            break;
        if Assistants = nil then
          RegSetStr    ('Assistant1',                       ''                   );
        for i1 := 1 to maxInt do
          if not RegDelVal('Assistent' + IntToStr(i1)) then
            break;
        for i1 := 0 to high(Forms) do
          RegSetBin    (Length(Forms[i1]), 'Forms' + IntToStr(i1 + 1), Forms[i1][1]);
        for i1 := Length(Forms) to maxInt - 1 do
          if not RegDelVal('Forms' + IntToStr(i1 + 1)) then
            break;
        if Forms = nil then
          RegSetStr    ('Forms1',                         ''                   );
        RegSetStr    ('TitleBar',                         TitleBar             );
        RegSetStr    ('ExceptionMessage',                 ExceptMsg            );
        RegSetStr    ('FrozenMessage',                    FrozenMsg            );
        RegSetStr    ('BitFaultMsg',                      BitFaultMsg          );
        RegSetStr    ('MailBugReportText',                SendBtnCaption       );
        RegSetStr    ('SaveBugReportText',                SaveBtnCaption       );
        RegSetStr    ('PrintBugReportText',               PrintBtnCaption      );
        RegSetStr    ('ShowBugReportText',                ShowBtnCaption       );
        RegSetStr    ('ContinueApplicationText',          ContinueBtnCaption   );
        RegSetStr    ('RestartApplicationText',           RestartBtnCaption    );
        RegSetStr    ('CloseApplicationText',             CloseBtnCaption      );
        RegSetStr    ('OkBtnText',                        OkBtnCaption         );
        RegSetStr    ('DetailsBtnText',                   DetailsBtnCaption    );
        RegSetStr    ('PleaseWaitTitle',                  PleaseWaitTitle      );
        RegSetStr    ('PleaseWaitText',                   PleaseWaitText       );
        RegSetStr    ('MailSubject',                      MailSubject          );
        RegSetStr    ('MailBody',                         MailBody             );
        RegSetStr    ('SendBoxTitle',                     SendBoxTitle         );
        RegSetStr    ('PrepareAttachMsg',                 PrepareAttachMsg     );
        RegSetStr    ('MxLookupMsg',                      MxLookupMsg          );
        RegSetStr    ('ConnectMsg',                       ConnectMsg           );
        RegSetStr    ('AuthMsg',                          AuthMsg              );
        RegSetStr    ('SendMailMsg',                      SendMailMsg          );
        RegSetStr    ('FieldsMsg',                        FieldsMsg            );
        RegSetStr    ('SendAttachMsg',                    SendAttachMsg        );
        RegSetStr    ('SendFinalizeMsg',                  SendFinalizeMsg      );
        RegSetStr    ('MailFailureMsg',                   SendFailureMsg       );
        RegSetStr    ('VersionVariable',                  VersionVar           );
      end;
    finally RegCloseKey(hk) end;
  unindentLog;
end;

// ***************************************************************

function GetProcInfo(proc: pointer; var unitName, procName: string) : boolean;
var i1     : integer;
    module : dword;
    s1     : string;
    c1, c2 : dword;
    pc1    : pchar;
begin
  result := false;
  if FindModule(proc, module, s1) then begin
    s1 := GetImageProcName(module, proc, true);
    unitName := SubStr(s1, 1, '.');
    procName := SubStr(s1, 2, '.');
    result := (unitName <> '') and (procName <> '');
    if result then begin
      c1 := FindResource(module, 'PACKAGEINFO', RT_RCDATA);
      if c1 <> 0 then begin
        c2 := LoadResource(module, c1);
        if c2 <> 0 then begin
          pc1 := LockResource(c2);
          i1 := PosPChar(pchar(unitName), pc1, Length(unitName), SizeOfResource(module, c1), true);
          if i1 >= 0 then
            Move(pc1[i1], unitName[1], Length(unitName));
          UnlockResource(c2);
          FreeResource(c2);
        end;
      end;
    end;
  end;
end;

// ***************************************************************

function PatchBinary(var project: TProject; binary, root, map: string;
                     compileSucceeded: boolean; var infos: string) : integer;

  procedure info(info: string);
  begin
    log('Info: ' + info);
    infos := infos + '|i' + info;
  end;

  procedure warning(warning: string);
  begin
    log('Warning: ' + warning);
    infos := infos + '|w' + warning;
  end;

  function VirtualToRaw(nh: PImageNtHeaders; virt: cardinal) : cardinal;
  type TImageSectionHeaderArray = packed array [0..9999] of TImageSectionHeader;
  var i1  : integer;
      ish : ^TImageSectionHeaderArray;
  begin
    result := virt;
    ish := pointer(cardinal(nh) + sizeOf(TImageNtHeaders));
    for i1 := 0 to nh^.FileHeader.NumberOfSections - 1 do
      if (virt >= ish[i1].VirtualAddress) and
         ((i1 = nh^.FileHeader.NumberOfSections - 1) or (virt < ish[i1 + 1].VirtualAddress)) then begin
        result := virt - ish[i1].VirtualAddress + ish[i1].PointerToRawData;
        break;
      end;
  end;

  function VirtualGetImportEntry(nh: PImageNtHeaders; buf: pointer; module, proc: string) : cardinal;
  type
    TImageImportDirectory = packed record
      HintNameArray  : cardinal;
      TimeDateStamp  : cardinal;
      ForwarderChain : cardinal;
      Name_          : cardinal;
      ThunkArray     : cardinal;
    end;
  var pimp : ^TImageImportDirectory;
      pc1  : pchar;
      pdw  : ^dword;
      c1   : cardinal;
  begin
    result := 0;
    dword(pimp) := VirtualToRaw(nh, nh^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress) + dword(buf);
    while (pimp <> nil) and (pimp^.Name_ <> 0) do begin
      dword(pc1) := VirtualToRaw(nh, pimp^.Name_) + dword(buf);
      if ((module = '') or IsTextEqual(pc1, module)) and (pimp^.ThunkArray <> 0) then begin
        dword(pdw) := VirtualToRaw(nh, pimp^.ThunkArray) + dword(buf);
        c1 := 0;
        while pdw^ <> 0 do begin
          if pdw^ and $80000000 = 0 then begin
            dword(pc1) := VirtualToRaw(nh, pdw^) + dword(buf) + 2;
            if IsTextEqual(pc1, proc) then begin
              result := pimp^.ThunkArray + c1 * 4;
              exit;
            end;
          end;
          inc(pdw);
          inc(c1);
        end;
      end;
      inc(pimp);
    end;
  end;

  procedure logMapEntry(var address; name: string);
  begin
    if dword(address) <> 0 then
         log(name + ': ' + IntToHexEx(dword(address), 8))
    else log(name + ' not found');
  end;

  function AdjustIcon(root: wideString; name_: string; handle, size: dword; icon: string) : boolean;

    function GetResIcon(bmp: string) : string;
    var c1, c2 : dword;
    begin
      result := '';
      c1 := FindResource(HInstance, pchar(UpStr(bmp)), RT_BITMAP);
      if (c1 <> 0) and (SizeOfResource(HInstance, c1) = 40 + size * size * 4) then begin
        c2 := LoadResource(HInstance, c1);
        if c2 <> 0 then begin
          SetString(result, pchar(LockResource(c2)), 40 + size * size * 4);
          UnlockResource(c2);
          FreeResource(c2);
        end;
      end;
    end;

  var name    : wideString;
      icofile : wideString;
      bmpfile : wideString;
      s1      : string;
      b1      : boolean;
      i1      : integer;
      pbih    : PBitmapInfoHeader;
  begin
    name    := 'MEI' + UpStr(name_);
    icofile := root + name + '.ICO';
    bmpfile := root + name + '.BMP';
    result := (GetFileAttributesW(pwidechar(icofile)) <> dword(-1)) or
              (GetFileAttributesW(pwidechar(bmpfile)) <> dword(-1)) or
              (icon <> '-');
    if result and (handle > 0) then begin
      if icon <> '-' then begin
        if icon = '' then
          icon := name_ + '1';
        if Length(icon) < 200 then begin
          s1 := icon;
          icon := GetResIcon(icon);
          if icon <> '' then
            //info('resource "mei' + name_ + '" set to default icon "' + s1 + '"')
          else
            warning('default icon "' + s1 + '" not found.');
        end else
          ;//info('resource "mei' + name_ + '" set to custom icon');
        if icon <> '' then
          UpdateResourceW(handle, PWideChar(RT_BITMAP), PWideChar(name), 0, pointer(icon), 40 + size * size * 4);
      end else
        if GetFileAttributesW(pwidechar(icofile)) <> dword(-1) then begin
          s1 := LoadStrFromFile(icofile);
          if s1 <> '' then begin
            b1 := false;
            with TPIcoHeader(s1)^ do
              for i1 := 0 to itemCount - 1 do
                with items[i1] do
                  if (width = size) and ((height = size) or (height = size * 2)) then begin
                    b1 := true;
                    if (bitCount = 32) and (imageSize > size * size * 4) and (imageSize < size * size * 2 * 4) then begin
                      pbih := pointer(dword(s1) + items[i1].offset);
                      pbih^.biHeight    := size;
                      pbih^.biSizeImage := size * size * 4;
                      UpdateResourceW(handle, PWideChar(RT_BITMAP), PWideChar(name), 0, pbih, 40 + size * size * 4);
                      //info('resource "mei' + name_ + '" replaced with 32 bit bitmap from ico file');
                      exit;
                    end;
                  end;
            if b1 then begin
              UpdateResourceW(handle, PWideChar(RT_BITMAP), PWideChar(name), 0, nil, 0);
              LoadIconGroupResourceW(handle, PWideChar(name), 0, PWideChar(icofile));
              //info('resource "mei' + name_ + '" replaced with ico file');
            end else
              warning('The file "mei' + name_ + '.ico" doesn''t contain a ' + IntToStr(size) + 'x' + IntToStr(size) + ' sized image.');
          end else begin
            UpdateResourceW(handle, PWideChar(RT_BITMAP), PWideChar(name), 0, nil, 0);
            //info('resource "mei' + name_ + '" deleted');
          end;
        end else begin
          s1 := LoadStrFromFile(bmpfile);
          if s1 <> '' then begin
            pbih := pointer(dword(s1) + sizeOf(TBitmapFileHeader));
            if (pbih^.biWidth = integer(size)) and (pbih^.biHeight = integer(size)) then begin
              if pbih^.biBitCount = 32 then begin
                LoadBitmapResourceW(handle, PWideChar(name), 0, PWideChar(bmpfile));
                //info('resource "mei' + name_ + '" replaced with bmp file');
              end else
                warning('The bmp file "mei' + name_ + '" doesn''t have the required color format (32 bits per pixel).');
            end else
              warning('The bmp file "mei' + name_ + '.bmp" doesn''t have the required size (' + IntToStr(size) + 'x' + IntToStr(size) + ').');
          end else begin
            UpdateResourceW(handle, PWideChar(RT_BITMAP), PWideChar(name), 0, nil, 0);
            //info('resource "mei' + name_ + '" deleted');
          end;
        end;
    end;
  end;

  function FindExport(module: dword; nh: PImageNtHeaders; proc: string) : dword;
  var ed : PImageExportDirectory;
      i1 : integer;
      c1 : dword;
  begin
    result := 0;
    try
      dword(ed) := module + VirtualToRaw(nh, nh^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress);
      with ed^ do
        for i1 := 0 to NumberOfNames - 1 do
          if proc = pchar(module + VirtualToRaw(nh, TPACardinal(module + VirtualToRaw(nh, AddressOfNames))^[i1])) then begin
            c1 := TPAWord(module + VirtualToRaw(nh, AddressOfNameOrdinals))^[i1];
            result := module + VirtualToRaw(nh, AddressOfFunctions) + c1 * 4;
            break;
          end;
    except result := 0 end;
  end;

{  function Align(value, align: dword) : dword;
  begin
    result := ((value + align - 1) div align) * align;
  end; }

  function CreateMadExceptDfm(const mapFile: TMapFile; var error: integer; versionVarPtr: dword; maxDebugInfo: string) : string;

    procedure AddDword(const name: string; value: dword);
    begin
      result := result + chr(Length(name)) + name + #$4 + #0#0#0#0;
      TPCardinal(dword(result) + dword(Length(result)) - 4)^ := value;
    end;

    procedure AddBoolean(const name: string; value: boolean);
    begin
      result := result + chr(Length(name)) + name + chr(8 + ord(value));
    end;

    procedure AddStr(const name, value: string);
    begin
      result := result + chr(Length(name)) + name;
      result := result + #$c + #0#0#0#0 + value;
      TPInteger(dword(result) + dword(Length(result) - Length(value)) - 4)^ := Length(value);
    end;

    procedure AddDADword(const name: string; const value: TDACardinal);
    begin
      result := result + chr(Length(name)) + name + #$a + #0#0#0#0;
      TPInteger(dword(result) + dword(Length(result)) - 4)^ := Length(value) * 4;
      SetLength(result, Length(result) + Length(value) * 4);
      Move(value[0], (pchar(result) + Length(result) - Length(value) * 4)^, Length(value) * 4);
    end;

  var i1 : integer;
  begin
    result := 'TPF0' + #$a + 'TMadExcept' + #$9 + 'madExcept';
    with project do begin
      AddBoolean('Enabled',          Enabled              );
      if MinDebugInfoOnly then
        AddStr    ('MinDebugInfo',     maxDebugInfo         );
      AddBoolean('NoSettings',       NoOwnSettings        );
      if not NoOwnSettings then begin
        AddBoolean('CheckFileCrc',      CheckFileCrc         );
        AddBoolean('CheckFreeze',       CheckForFreeze       );
        AddDword  ('FreezeTimeout',     FreezeTimeout        );
        AddBoolean('AutoSave',          AutoSave             );
        AddBoolean('AutoSaveIfNotSent', AutoSaveIfNotSent    );
        AddBoolean('AutoSend',          AutoSend             );
        AddBoolean('AutoSendBox',       AutoSendPrgrBox      );
        AddBoolean('AutoClip',          AutoClipboard        );
        AddBoolean('PauseThreads',      SuspendThreads       );
        AddBoolean('PlWaitBox',         ShowPleaseWaitBox    );
        AddBoolean('AutoContinue',      AutoContinue         );
        AddDword  ('AutoRestart',       AutoRestart          );
        AddDword  ('AutoClose',         AutoClose            );
        AddBoolean('SendBtnVis',        SendBtnVisible       );
        AddBoolean('SaveBtnVis',        SaveBtnVisible       );
        AddBoolean('PrintBtnVis',       PrintBtnVisible      );
        AddBoolean('ShowBtnVis',        ShowBtnVisible       );
        AddBoolean('ContinueBtnVis',    ContinueBtnVisible   );
        AddBoolean('RestartBtnVis',     RestartBtnVisible    );
        AddBoolean('CloseBtnVis',       CloseBtnVisible      );
        AddDword  ('FocusedBtn',        dword(FocusedButton) );
        AddStr    ('SendAssis',         SendAssistant        );
        AddStr    ('SaveAssis',         SaveAssistant        );
        AddStr    ('PrintAssis',        PrintAssistant       );
        AddBoolean('AutoShowBugRep',    AutoShowBugReport    );
        AddBoolean('UglyBtns',          NoOwnerDrawButtons   );
        AddStr    ('MailAddr',          MailAddr             );
        AddBoolean('SendInBackgr',      SendInBackground     );
        AddBoolean('MailAsSmtpServer',  MailAsSmtpServer     );
        AddBoolean('MailAsSmtpClient',  MailAsSmtpClient     );
        AddBoolean('UploadViaHttp',     UploadViaHttp        );
        AddBoolean('MailViaMapi',       MailViaMapi          );
        AddBoolean('MailViaMailto',     MailViaMailto        );
        AddStr    ('SmtpServer',        SmtpServer           );
        AddDword  ('SmtpPort',          dword(SmtpPort)      );
        AddStr    ('SmtpAccount',       SmtpAccount          );
        AddStr    ('SmtpPassword',      SmtpPassword         );
        AddStr    ('HttpServer',        HttpServer           );
        AddDword  ('HttpPort',          dword(HttpPort)      );
        AddStr    ('HttpAccount',       HttpAccount          );
        AddStr    ('HttpPassword',      HttpPassword         );
        AddBoolean('AttachBugRep',      AttachBugReport      );
        AddBoolean('AttachBugRepFile',  AttachBugReportFile  );
        AddBoolean('DelBugRepFile',     DeleteBugReportFile  );
        AddStr    ('BugRepSendAs',      BugReportSendAs      );
        AddStr    ('BugRepZip',         BugReportZip         );
        AddDword  ('ScrShotDepth',      dword(ScreenShotDepth));
        AddBoolean('ScrShotAppOnly',    ScreenShotAppOnly    );
        AddStr    ('ScrShotSendAs',     ScreenShotSendAs     );
        AddStr    ('ScrShotZip',        ScreenShotZip        );
        AddStr    ('AddAttachs',        AdditionalAttachs    );
        AddStr    ('BugRepFile',        BugReportFile        );
        AddBoolean('AppendBugReps',     AppendBugReports     );
        AddDword  ('BugRepFileSize',    BugReportFileSize    );
        AddBoolean('NoDupExcepts',      NoDupExcepts         );
        AddBoolean('NoDupFreezes',      NoDupFreezes         );
        AddDword  ('DupExceptDef',      dword(DupExceptDef)  );
        AddDword  ('DupFreezeDef',      dword(DupFreezeDef)  );
        AddBoolean('ListThreads',       ListThreads          );
        AddBoolean('CpuRegs',           ShowCpuRegisters     );
        AddBoolean('StackDump',         ShowStackDump        );
        AddBoolean('ShowDisAsm',        ShowDisAsm           );
        AddBoolean('HideUglyItems',     HideUglyItems        );
        AddBoolean('ShowRelAddrs',      ShowRelativeAddrs    );
        AddBoolean('ShowRelLines',      ShowRelativeLines    );
        AddBoolean('FormatDisAsm',      FormatDisassembly    );
        AddDword  ('LimitDisAsm',       dword(LimitDisassembly));
        AddStr    ('Plugins',           EnabledPlugins       );
        AddStr    ('F1Classes',         Filter1Classes       );
        AddBoolean('F1NoBugRep',        Filter1NoBugReport   );
        AddBoolean('F1NoScrShot',       Filter1NoScreenshot  );
        AddBoolean('F1NoHandlers',      Filter1NoHandlers    );
        AddBoolean('F1NoSuspend',       Filter1NoSuspend     );
        AddDword  ('F1ShowCfg',         dword(Filter1ShowSetting));
        AddStr    ('F1Assis',           Filter1Assis         );
        AddStr    ('F2Classes',         Filter2Classes       );
        AddBoolean('F2NoBugRep',        Filter2NoBugReport   );
        AddBoolean('F2NoScrShot',       Filter2NoScreenshot  );
        AddBoolean('F2NoHandlers',      Filter2NoHandlers    );
        AddBoolean('F2NoSuspend',       Filter2NoSuspend     );
        AddDword  ('F2ShowCfg',         dword(Filter2ShowSetting));
        AddStr    ('F2Assis',           Filter2Assis         );
        AddBoolean('GnNoBugRep',        GeneralNoBugReport   );
        AddBoolean('GnNoScrShot',       GeneralNoScreenshot  );
        AddBoolean('GnNoHandlers',      GeneralNoHandlers    );
        AddBoolean('GnNoSuspend',       GeneralNoSuspend     );
        AddDword  ('GnShowCfg',         dword(GeneralShowSetting));
        AddStr    ('GnAssis',           GeneralAssis         );
        for i1 := 0 to high(Assistants) do
          AddStr    ('Assistant' + IntToStr(i1 + 1),     Assistants[i1]       );
        AddStr    ('TitleBar',          TitleBar             );
        AddStr    ('ExceptMsg',         ExceptMsg            );
        AddStr    ('FrozenMsg',         FrozenMsg            );
        AddStr    ('BitFaultMsg',       BitFaultMsg          );
        AddStr    ('SendBtnTxt',        SendBtnCaption       );
        AddStr    ('SaveBtnTxt',        SaveBtnCaption       );
        AddStr    ('PrintBtnTxt',       PrintBtnCaption      );
        AddStr    ('ShowBtnTxt',        ShowBtnCaption       );
        AddStr    ('ContinueBtnTxt',    ContinueBtnCaption   );
        AddStr    ('RestartBtnTxt',     RestartBtnCaption    );
        AddStr    ('CloseBtnTxt',       CloseBtnCaption      );
        AddStr    ('OkBtnTxt',          OkBtnCaption         );
        AddStr    ('DetailsBtnTxt',     DetailsBtnCaption    );
        AddStr    ('PlWaitTitle',       PleaseWaitTitle      );
        AddStr    ('PlWaitText',        PleaseWaitText       );
        AddStr    ('MailSubj',          MailSubject          );
        AddStr    ('MailBody',          MailBody             );
        AddStr    ('SendBoxTitle',      SendBoxTitle         );
        AddStr    ('PrepAttMsg',        PrepareAttachMsg     );
        AddStr    ('MxLookMsg',         MxLookupMsg          );
        AddStr    ('ConnMsg',           ConnectMsg           );
        AddStr    ('AuthMsg',           AuthMsg              );
        AddStr    ('SendMailMsg',       SendMailMsg          );
        AddStr    ('FieldMsg',          FieldsMsg            );
        AddStr    ('SendAttMsg',        SendAttachMsg        );
        AddStr    ('SendFinalMsg',      SendFinalizeMsg      );
        AddStr    ('SendFailMsg',       SendFailureMsg       );
        if versionVarPtr <> 0 then
          AddDword  ('VersionVar',        versionVarPtr        );
      end;
      result := result + #0#0;
    end;
  end;

  function PatchFuncs(nh: PImageNtHeaders; buf: pointer; mapFile: TMapFile) : boolean;
  type TMapInfo = record
                    code  : boolean;
                    var_  : string;
                    unit_ : string;
                    func  : string;
                  end;
  const CMapInfos : array [0.. {$ifdef bcb}19{$else}13{$endif}] of TMapInfo = (
    {$ifdef bcb}
      (code: true;  var_: 'BcbInitExceptBlockLDTC';                          unit_: '';              func: '__InitExceptBlockLDTC'                 ),
      (code: true;  var_: 'BcbExceptionHandler';                             unit_: '';              func: '____ExceptionHandler'                  ),
      (code: true;  var_: 'BcbThrowExceptionLDTC';                           unit_: '';              func: '_ThrowExceptionLDTC'                   ),
      (code: true;  var_: 'BcbOrgMalloc';                                    unit_: '';              func: '___org_malloc'                         ),
      (code: true;  var_: 'BcbMemcpy';                                       unit_: '';              func: '_memcpy|memcpy'                        ),
      (code: true;  var_: 'BcbCallTerminate';                                unit_: '';              func: '___call_terminate'                     ),
    {$endif}
    (code: true;  var_: 'Forms_TApplication_HandleException';              unit_: 'Forms';         func: 'TApplication.HandleException'          ),
    (code: true;  var_: 'Forms_TApplication_ShowException';                unit_: 'Forms';         func: 'TApplication.ShowException'            ),
    (code: true;  var_: 'Qforms_TApplication_HandleException';             unit_: 'Qforms';        func: 'TApplication.HandleException'          ),
    (code: true;  var_: 'Qforms_TApplication_ShowException';               unit_: 'Qforms';        func: 'TApplication.ShowException'            ),
    (code: true;  var_: 'SysUtils_ShowException';                          unit_: 'SysUtils';      func: 'ShowException'                         ),
    (code: true;  var_: 'SysUtils_LoadPackage';                            unit_: 'SysUtils';      func: 'LoadPackage'                           ),
    (code: true;  var_: 'SysUtils_InitializePackage';                      unit_: 'SysUtils';      func: 'InitializePackage'                     ),
    (code: true;  var_: 'Classes_CheckSynchronize';                        unit_: 'Classes';       func: 'CheckSynchronize'                      ),
    (code: true;  var_: 'CGIApp_TCGIApplication_CGIHandleException';       unit_: 'CGIApp';        func: 'TCGIApplication.CGIHandleException'    ),
    (code: true;  var_: 'ISAPIApp_TISAPIApplication_ISAPIHandleException'; unit_: 'ISAPIApp';      func: 'TISAPIApplication.ISAPIHandleException'),
    (code: true;  var_: 'System_InitUnits';                                unit_: 'System';        func: 'InitUnits'                             ),
    (code: true;  var_: 'System_FinalizeUnits';                            unit_: 'System';        func: 'FinalizeUnits|FInitUnits'              ),
    (code: true;  var_: 'System_ExceptionHandler';                         unit_: 'System';        func: '@ExceptionHandler'                     ),
    (code: false; var_: 'System_runErrMsg';                                unit_: 'System';        func: 'runErrMsg'                             ));
  var i1, i2 : integer;
      c1, c2 : dword;
  begin
    result := false;
    with project, mapFile do 
      for i1 := 0 to high(CMapInfos) do
        with CMapInfos[i1] do begin
          c1 := FindPublic(false, 'madExcept', var_);
          logMapEntry(c1, 'madExcept.' + var_);
          for i2 := 1 to SubStrCount(func) do begin
            c2 := FindPublic(code, unit_, SubStr(func, i2));
            if c2 <> 0 then
              break;
          end;
          logMapEntry(c2, unit_ + '.' + func);
          if (c1 <> 0) and (c2 <> 0) then begin
            with nh^.OptionalHeader do
              if code then
                   TPCardinal(VirtualToRaw(nh, c1 + BaseOfData) + dword(buf))^ := c2 + BaseOfCode
              else TPCardinal(VirtualToRaw(nh, c1 + BaseOfData) + dword(buf))^ := c2 + BaseOfData;
            result := true;
          end;
        end;
  end;

var madFile                   : string;
    ico                       : wideString;
    s1, s2, s3, s4            : string;
    c1, c2, c3                : cardinal;
    cutFile                   : dword;
    wfd                       : TWin32FindData;
    buf                       : pointer;
    nh                        : PImageNtHeaders;
    HttpExtensionProc         : TPCardinal;
    HttpExtensionProcCallback : dword;
    HttpExtensionProcNext     : dword;
    versionVarPtr             : dword;
    b1                        : boolean;
    mapFile                   : TMapFile;
    i1, i2                    : integer;
    p1                        : pointer;
    boc, bod, soc, soid, soud : dword;
    psh                       : PImageSectionHeader;
begin
  result := 0;
  log('PatchBinary, compileSucceeded: ' + booleanToChar(compileSucceeded) + ', binary: "' + binary + '", root: "' + root + '"'); indentLog;
  infos := '';
  boc := 0;
  bod := 0;
  soc := 0;
  soid := 0;
  soud := 0;
  versionVarPtr := 0;
  try
    if map = '' then
      map := binary;
    with project do begin
      mapFile := LoadMapFile(map);
      with mapFile do
        if IsValid then begin
          log('map file successfully loaded/parsed');
          c1 := CreateFile(pchar(binary), GENERIC_READ or GENERIC_WRITE, 0, nil,
                           OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
          if c1 <> INVALID_HANDLE_VALUE then begin
            try
              log('binary successfully opened');
              cutFile := 0;
              c2 := CreateFileMapping(c1, nil, PAGE_READWRITE, 0, 0, nil);
              if c2 <> 0 then
                try
                  buf := MapViewOfFile(c2, FILE_MAP_ALL_ACCESS, 0, 0, 0);
                  if buf <> nil then
                    try
                      log('binary opened as memory mapped file');
                      nh := GetImageNtHeaders(dword(buf));
                      if nh <> nil then begin
                        boc := nh^.OptionalHeader.BaseOfCode;
                        bod := nh^.OptionalHeader.BaseOfData;
                        soc := nh^.OptionalHeader.SizeOfCode;
                        dword(psh) := dword(nh) + sizeOf(nh^);
                        if (psh^.Characteristics and IMAGE_SCN_CNT_CODE <> 0) and
                           (pchar(@psh^.Name) = '.text') and (soc = psh^.SizeOfRawData) then begin
                          inc(psh);
                          if (psh^.Characteristics and IMAGE_SCN_CNT_CODE <> 0) and (pchar(@psh^.Name) = '.itext') then
                            // we have an exe with an extra initialization section here
                            soc := psh^.VirtualAddress - boc + psh^.SizeOfRawData;
                        end;
                        soid := nh^.OptionalHeader.SizeOfInitializedData;
                        soud := nh^.OptionalHeader.SizeOfUninitializedData;
                        if Enabled then begin
                          log('patch binary');
                          b1 := false;
                          with nh^, OptionalHeader do begin
                            HttpExtensionProc         := pointer(FindExport(dword(buf), nh, 'HttpExtensionProc'        ));
                            HttpExtensionProcCallback := FindPublic(true,  'madExcept',     'InterceptHttpExtensionProc');
                            HttpExtensionProcNext     := FindPublic(false, 'madExcept',     'HttpExtensionProcNext'     );
                            if (HttpExtensionProc <> nil) and (HttpExtensionProcCallback <> 0) and
                               (HttpExtensionProcNext <> 0) then begin
                              TPInteger(VirtualToRaw(nh, HttpExtensionProcNext + BaseOfData) + dword(buf))^ := HttpExtensionProc^;
                              HttpExtensionProc^ := HttpExtensionProcCallback + BaseOfCode;
                              b1 := true;
                            end;
                            if versionVar <> '' then
                              if SubStrCount(versionVar, '.') = 2 then begin
                                versionVarPtr := FindPublic(false, SubStr(versionVar, 1, '.'), SubStr(versionVar, 2, '.'));
                                if versionVarPtr <> 0 then begin
                                  versionVarPtr := versionVarPtr + BaseOfData;
                                end else begin
                                  warning('The specified version variable was not found!');
                                  result := 6;
                                end;
                              end else begin
                                warning('The format of the version variable is incorrect!');
                                result := 6;
                              end;
                            if PatchFuncs(nh, buf, mapFile) then
                              b1 := true;
                          end;
                          if b1 then
                            info('Binary patched.');
                        end;
                        // remove the debug section
                        // I thought I need it for BCB, but I don't
                        // anyway, nice code, don't wanna kill it
                        // perhaps I need it later again
                        (*
                          if (OldTD32Info = 0) and (FPrj.ProjectOptions.Values['DebugInfo'] <> 0) then begin
                            dword(psh) := dword(nh) + sizeOf(nh^);
                            inc(psh, integer(nh^.FileHeader.NumberOfSections) - 1);
                            if (psh^.VirtualAddress <> 0) and
                               (psh^.VirtualAddress = nh^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress) then begin
                              cutFile := psh^.PointerToRawData;
                              dec(nh^.OptionalHeader.SizeOfInitializedData, psh^.SizeOfRawData);
                              dec(nh^.OptionalHeader.SizeOfImage,           Align(psh^.Misc.VirtualSize, nh^.OptionalHeader.SectionAlignment));
                              ZeroMemory(psh, sizeOf(psh^.Name));
                              psh^.SizeOfRawData    := 0;
                              psh^.Misc.VirtualSize := 0;
                              with nh^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG] do begin
                                VirtualAddress := 0;
                                Size           := 0;
                              end;
                              dec(nh^.FileHeader.NumberOfSections);
                            end;
                          end;
                        *)
                      end;
                    finally UnmapViewOfFile(buf) end;
                finally CloseHandle(c2) end;
              if cutFile > 0 then begin
                SetFilePointer(c1, cutFile, nil, FILE_BEGIN);
                SetEndOfFile(c1);
              end;
            finally CloseHandle(c1) end;
            ico := ExtractFilePath(root);
            c1 := BeginUpdateResourceW(PWideChar(wideString(binary)), false);
            log('append map file to binary');
            if GetResourceW(c1, 'MAD', 'EXCEPT', 0, buf, c2) then begin
              info('map file is already appended');
              EndUpdateResourceW(c1, true);
              exit;
            end;
            log('map file is not yet appended');
            s1 := Export(true, MinDebugInfoOnly, {$ifdef dontExportUglyItems} hideUglyItems {$else} false {$endif} );
            log('append map file now');
            UpdateResourceW(c1, 'MAD', 'EXCEPT', 0, pchar(s1), Length(s1));
            if boc <> 0 then begin
              SetLength(s1, 20);
              TPACardinal(s1)^[0] := FindPublic(true,  'madExcept', 'CalibrateCode');
              TPACardinal(s1)^[1] := FindPublic(false, 'madExcept', 'CalibrateData');
              TPACardinal(s1)^[2] := soc;
              TPACardinal(s1)^[3] := soid;
              TPACardinal(s1)^[4] := soud;
              if TPACardinal(s1)^[0] = 0 then
                TPACardinal(s1)^[0] := boc or $80000000;
              if TPACardinal(s1)^[1] = 0 then
                TPACardinal(s1)^[1] := bod or $80000000;
              UpdateResourceW(c1, 'MAD', 'CALIBRATE', 0, pchar(s1), Length(s1));
            end;
            info('map file appended');
            madFile := binary;
            Delete(madFile, PosStr('.', madFile, maxInt, 1), maxInt);
            madFile := madFile + '.mad';
            SetFileAttributes(pchar(madFile), 0);
            s2 := '';
            if MinDebugInfoOnly then begin
              log('create "mad" file');
              c2 := CreateFile(pchar(madFile), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0);
              if c2 <> INVALID_HANDLE_VALUE then begin
                s1 := Export(true, false, {$ifdef dontExportUglyItems} hideUglyItems {$else} false {$endif} );
                if WriteFile(c2, pointer(s1)^, Length(s1), c3, nil) and (c3 = dword(Length(s1))) then begin
                  s2 := IntToHexEx(Length(s1), 8) + ', ' +
                        IntToHexEx(TPInteger(dword(s1) + dword(Length(CMapFileStreamDescriptor)) + 08)^, 8) + ', ' +
                        IntToHexEx(TPInteger(dword(s1) + dword(Length(CMapFileStreamDescriptor)) + 12)^, 8);
                  info('map file compressed into "mad" file');
                end else
                  warning('Error writing "mad" file.');
                CloseHandle(c2);
              end else warning('Error creating "mad" file.');
            end else
              DeleteFile(madFile);
//            if Enabled then begin
              s1 := CreateMadExceptDfm(mapFile, result, versionVarPtr, s2);
              UpdateResourceW(c1, PWideChar(RT_RCDATA), 'TMADEXCEPT', 0, pointer(s1), Length(s1));
              if not NoOwnSettings then begin
                AdjustIcon(ico, 'Send',         c1, 16,         SendBtnIcon);
                AdjustIcon(ico, 'Save',         c1, 16,         SaveBtnIcon);
                AdjustIcon(ico, 'Print',        c1, 16,        PrintBtnIcon);
                AdjustIcon(ico, 'Show',         c1, 16,         ShowBtnIcon);
                AdjustIcon(ico, 'Continue',     c1, 16,     ContinueBtnIcon);
                AdjustIcon(ico, 'Restart',      c1, 16,      RestartBtnIcon);
                AdjustIcon(ico, 'Close',        c1, 16,        CloseBtnIcon);
                AdjustIcon(ico, 'CantContinue', c1, 16, CantContinueBtnIcon);
                AdjustIcon(ico, 'Big',          c1, 32,             BigIcon);
                AdjustIcon(ico, 'Send32',       c1, 32,          Send32Icon);
                AdjustIcon(ico, 'PlWait',       c1, 32,      PleaseWaitIcon);
                for i1 := 0 to high(Forms) do
                  if PosStrIs1('TPF0', Forms[i1]) then begin
                    SetString(s1, pchar(Forms[i1]) + 5, ord(Forms[i1, 5]));
                    s2 := Forms[i1];
                    i2 := PosStr(#8 + 'OnAction' + #$c, s2);
                    if i2 > 0 then begin
                      inc(i2, 10);
                      SetString(s3, pchar(s2) + i2 + 3, TPInteger(@s2[i2])^);
                      if s3 <> '' then begin
                        s4 := SubStr(s3, 2, '.');
                        if s4 = '' then begin
                          s4 := s3;
                          s3 := '';
                        end else
                          s3 := SubStr(s3, 1, '.');
                        if (not IsTextEqual(s3, 'madExcept')) or
                           ( (not IsTextEqual(s4, 'HandleContactForm'   )) and
                             (not IsTextEqual(s4, 'HandleScreenshotForm'))     ) then
                          if FindPublic(true, s3, s4) = 0 then
                            warning(s1 + '.OnAction handler "' + s3 + '.' + s4 + '" not found. Maybe the smart linker removed it?');
                      end;
                    end;
                    s1 := UpStr(s1);
                    if GetResourceW(c1, PWideChar(RT_RCDATA), PWideChar(wideString(s1)), 0, p1, c2) then
                      warning('Duplicate resource "' + s1 + '".')
                    else
                      UpdateResourceW(c1, PWideChar(RT_RCDATA), PWideChar(wideString(s1)), 0, pointer(s2), Length(s2));
                  end else
                    warning('Invalid madExcept form data.');
              end;
//            end;
            EndUpdateResourceW(c1, false);
            c1 := FindFirstFile(pchar(binary), wfd);
            if c1 <> INVALID_HANDLE_VALUE then begin
              Windows.FindClose(c1);
              FLastBinaryTime := int64(wfd.ftLastWriteTime);
              FLastBinarySize :=       wfd.nFileSizeLow;
            end;
          end else begin
            c1 := GetLastError;
            log('can''t open the binary, error: "' + errorCodeToStr(c1) + '"');
            if compileSucceeded then
              if c1 in [5, 32] then
                   warning('The binary couldn''t be patched, because it''s currently in use.')
              else warning('The binary couldn''t be patched ("' + errorCodeToStr(c1) + '").');
            result := 8;
          end;
        end else begin
          Free;
          if compileSucceeded then warning('No map file found!');
          result := 9;
        end;
    end;
    Delete(infos, 1, 1);
    log('PatchBinary done');
  finally unindentLog end;
end;

// ***************************************************************

begin
  if @PatchBinary <> nil then ;
end.
