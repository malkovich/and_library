// ***************************************************************
//  madExcept.pas             version:  3.0c  ·  date: 2006-05-26
//  -------------------------------------------------------------
//  exception handling
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2006 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2006-05-26 3.0c (1) bug in IMEAttachments.Delete fixed
//                 (2) minor change to improve stack overflow catching
//                 (3) some http uploading bugs fixed
//                 (4) "MESettings.Reload" reloads the madExcept settings
//                 (5) %appversion% and %modversion% added
//                 (6) support for C++ Builder 2006 dynamic RTL added
//                 (7) IDE exception catching sometimes used incorrect settings
//                 (8) "save bug report" dialog default extension is now set
//                 (9) little bug in CloseHandleExceptionThread fixed
//                 (a) Registered Owner/Organzation added to bug report header
//                 (b) UPX fix is applied to madExcept enabled BPLs now, too
// 2006-01-30 3.0b (1) smtp and http password storage error fixed
//                 (2) support for QueueUserWorkItem threads added
//                 (3) plugin crashes are now logged to the bug report
//                 (4) "IException.GetBugReport" could output incomplete report
//                 (5) manually asking bug reports no longer uses extra threads
//                 (6) two different plugin callbacks supported now
//                 (7) uploading bug fixed (IIS is quite sensitive)
//                 (8) always save bug report -> sent report incomplete
//                 (9) slightly improved thread frame logic
//                 (a) exception box now also shows "public%" and "segment%"
// 2005-11-28 3.0a (1) send assistant: email syntax is verified now
//                 (2) line breaks in except message are not removed, anymore
//                 (3) canContinue can be reset to "true" if it once was "true"
//                 (4) ExceptActionHandler: "handled := true" didn't work
//                 (5) ExceptActionHandler was called for "stDontSync", only
//                 (6) sometimes an outdated bug report was mailed
//                 (7) "%exceptMsg%": exception message is nicer formatted now
//                 (8) memory leak fixed
//                 (9) dll init/final exception -> incomplete bug report
//                 (a) undocumented global variable "ShowBugReportKey" added
//                 (b) HandleContact/ScreenshotForm OnAction handlers exported
//                 (c) http upload redirection support added
//                 (d) some minor http upload bugs fixed
//                 (e) "exception number" added to bug report header
//                 (f) TThread.Synchronize exception catcher didn't like EAbort
//                 (g) thread exception frame more forgiving now
//                 (h) IMEAttachments.Clear added
//                 (i) madTraceProcess always lists all threads now
//                 (j) bcb support for runtime packages improved
//                 (k) undocumented OnExceptBoxCreate callback added
//                 (l) Close/RestartApplication now waits until mailing is done
//                 (m) wide string settings support added (needed for Passolo)
//                 (n) SMTP client password was read out incorrectly
//                 (o) memory status bigger than 2 GB was returned incorrectly
// 2005-07-17 3.0  this is just a list of the most important changes:
//                 (1) redesigned settings dialog
//                 (2) thread safe assistants
//                 (3) exception box variations
//                 (4) exception filters
//                 (5) resizable exception box with tabs
//                 (6) hyper jump functionality
//                 (7) threaded bug report creation
//                 (8) extended runtime package support
//                 (9) bug report plugins
//                 (a) madExcept settings stored in the resource section
//                 (b) settings accessible at runtime through an IInterface
//                 (c) exception information encapsulated in an IInterface
//                 (d) exception interface is descendent of settings interface
//                 (e) duplicate bug report detection
//                 (f) HTTP upload functionality
//                 (g) new attachment options
//                 (h) madExcept configuration dialog
//                 (i) additional exception context information
//                 (j) send & please wait progress boxes
//                 (k) no own settings for some modules
//                 (l) limit screen shot to our own app windows
//                 (m) bug report sending in background thread
//                 (n) auto save only if bug report doesn't get sent
//                 (o) include minimal debug info only
//                 (p) cpu registers & stack dump
//                 (q) support for UPX and Shrinker
//                 (r) out of memory protection
//                 (s) safecall exception handling
//                 (t) list of running processes
//                 (u) new icons, now adjustable
//                 (v) pausing exception catching
//                 (w) "mail bug report" -> "send bug report"
//                 (x) check whether binary file is corrupt (via crc)
// 2005-07-15 2.7h (1) bugs fixed in "CheckExceptParams.IsCorrectExceptAddr"
//                 (2) MailAddr supports %appname% now, too
//                 (3) OpenThread doesn't need OS loader lock, anymore
//                 (4) NameThread now also shows effect on the debugger (D7+)
//                 (5) InterceptFinalizeUnits improved (ignores AVs)
//                 (6) DeleteBugReportFile failed when screenShot was enabled
//                 (7) possible race condition in InterceptHttpExtensionProc
//                 (8) custom rtl/vcl packages are automatically supported, now
//                 (9) %exceptMsg% -> Unknown exception -> "Unknown"
//                 (a) "invalid import table" warning only shown in debugger
//                 (b) CreateBugReport now always gets stack for own thread
//                 (c) support for InitializePackage overload (2 params) added
//                 (d) ProcessMainThreadId was not always set correctly
//                 (e) "Abort" handling slightly improved
//                 (f) stTrySyncCallAlways didn't work for freezes
//                 (g) user impersonation problems fixed
// 2005-02-05 2.7g (1) another bug fixed in hidden exception handling
//                 (2) our process' "allocated memory" added to report header
// 2005-01-30 2.7f (1) "MainThreadId" in a DLL might not be process' main tid
//                 (2) bug fixed in hidden exception catching
//                 (3) exception seconds and ms added to bug report header
//                 (4) removed spaces from CPU name
// 2004-11-15 2.7e "Unhandled OS exceptions" catching off by default in Delphi
// 2004-11-08 2.7d (1) mapi mails get sent to "SMTP:" now
//                 (2) improved situation when exceptObject is invalid/wild
//                 (3) mail subject supports %appname% now
//                 (4) pressed "save" button is hidden only if it makes sense
//                 (5) check for invalid ExceptObject improved
//                 (6) now all dns ips reported by the OS are used
//                 (7) some info functions like "GetHardwareList" exported now
//                 (8) GetThreadName exported now
//                 (9) except callback sync parameters changed
//                 (a) except callback "phase" parameter added
//                 (b) directory for bug report file is created (if necessary)
//                 (c) PauseFreezeCheck is now stackable
//                 (d) pause running delphi/bcb threads totally rewritten
//                 (e) "PauseMeEventually" function added
//                 (f) minor changes for delphi 9 support (only win32)
//                 (g) CreateBugReport reports all it can even if crashes occur
//                 (h) madTraceProcess works for other sessions now, too
//                 (i) new BCB patches added to enhance exception catching
//                 (j) "AddMailAttachments" option added (runtime only for now)
//                 (k) "Message-ID" added to SMTP header
// 2004-08-15 2.7c (1) hardware list optionally added to bug report
//                 (2) CPU info added to bug report header (w2k and newer)
//                 (3) display mode added to bug report header
//                 (4) improved html output (layout, w3 validity, etc)
//                 (5) suspended threads are marked as such in the bug report
//                 (6) bug report contains information about thread priorities
//                 (7) bde version added to bug report header (if bde is used)
//                 (8) stack not accessible: at least except location is shown
//                 (9) fixed a resource leak in SuspendThreadEx
//                 (a) undocumented "HyperJumpCallback" API added (for Viewer)
//                 (b) only Delphi threads are suspended by madExcept now
//                 (c) MX lookup uses the OS' DNS ip address now (-> firewalls)
//                 (d) callback synchronization sometimes failed unexpectedly
//                 (e) MailSubject now supports %exceptMsg%, too
//                 (f) MAPI: some mail clients didn't accept the receiver
//                 (g) undocumented HookSafeCallExceptions function added
//                 (h) exceptBox: pressing ESC when continue impossible: freeze
//                 (i) if "canContinue" is false, madExcept behaves better now
// 2004-04-25 2.7b HandleException "currentEbp" param can improve trace quality
// 2004-04-18 2.7a (1) AMD64 NX: LocalAlloc -> VirtualAlloc (HookedCreateThread)
//                 (2) eaClose/RestartApp: setting "handled" didn't work
//                 (3) MAPI mailing: screenShot.png isn't deleted anymore
//                 (4) %LF% (line feed) added for exception message (ExceptMsg)
//                 (5) TExceptActionHandler: bugReport parameter is "var" now
//                 (6) screenShot + crashedThreadId params added to handlers
//                 (7) support for mailing the bug report to multiple receivers
//                 (8) window menu: resize/minimize/maximize/restore removed
//                 (9) ExceptMsg + FrozenMsg max length increased to 255 chars
//                 (a) added "Content-Type" to the smtp multipart text part
//                 (b) SMTP: responses may come in multiple small chunks now
//                 (c) GetTickCount doesn't overrun now, anymore
//                 (d) ISAPI problems with ISAPIThreadPool fixed
//                 (e) rtl/vcl/clx are only hooked, if they're not hooked yet
//                 (f) MAPI availability check is less strict now
// 2004-03-08 2.7  (1) BugReportFile: "%appname%" sometimes didn't work
//                 (2) strange color schemes are better supported now
//                 (3) %LF% (line feed) added for mail success/failure messages
//                 (4) "pause all running threads" off -> messages get handled
//                 (5) "sync" handlers only get called in main thread context
//                 (6) FPU exceptions can be reenabled after each exception
//                 (7) handling of sync/non-sync events improved a bit
//                 (8) program up time added to bug report header
//                 (9) SetFreezeTimeout function added
//                 (a) during synced handlers: ignore main thread exceptions
//                 (b) ignore exceptions of our private except. handling thread
//                 (c) improved support for EAbort
//                 (d) undocumented "ResetFpuMode" option added
//                 (e) CreateBugReport: additional parameter "showProgressBar"
//                 (f) hidden exceptions: no progress bar is shown anymore
//                 (g) MailAsSMTPServer not default anymore
//                 (h) control chars in the exception message made problems
//                 (i) final. exception sometimes resulted in program restart
//                 (j) console exception handling improved
//                 (k) exception box' title bar now always shows a [x] button
//                 (l) screen shot functionality added
//                 (m) SMTP mailing function now fills the date time field
//                 (n) bug in GetTSClientName fixed
//                 (o) automated support for CGI + ISAPI + IntraWeb 5 - 7 added
//                 (p) exceptions in madExcept's finalization are ignored now
//                 (q) AutoMailBugReport function added for silent mail sending
//                 (r) got rid of non ascii chars in bug report
// 2003-11-18 2.6a (1) timeout added for SMTP functions
//                 (2) runtime option "AddCmdLineToBugRep" added
//                 (3) critical bug in OpenThread fixed
// 2003-11-16 2.6  (1) Armadillo related bug in CreateThread hook fixed
//                 (2) AutoClipboard property added
//                 (3) AutoMail property reintroduced
//                 (4) due to a bug printing was limited to one page only
//                 (5) HandleHiddenException crashed when "exceptObject = nil"
//                 (6) improved thread naming, this also fixes a memory leak
//                 (7) if callback synchronization failed, the program froze
//                 (8) "AutoDelay := 0" made madExcept crash
//                 (9) invalid "ExceptObject" made madExcept crash
//                 (a) process id and command line added to bug report
//                 (b) dynamically loaded bpls: handling of init exceptions
//                 (c) "created by thread" improved when using runtime packages
//                 (d) "DetectConsole" runtime option added
//                 (e) %exceptMsg% was not expanded for console applications
//                 (f) ExceptMsg string size increased to 200 characters
//                 (g) support for exceptObject classes <> SysUtils.Exception
//                 (h) some changes to avoid freezes while handling exceptions
//                 (i) now each event handler can be synchronized or not
//                 (j) ImNotYetFrozen -> ImNotFrozen
//                 (k) AmOnline + SendSmtpMail functions added
//                 (l) SendMapiMail supports an attachment now
//                 (m) a lot of new mailing options added
//                 (n) TThread.Synchronize exceptions get caught now (D6+D7)
//                 (o) exceptions get counted now -> automatic restart/close
//                 (p) Save+Print now refuse to run during DLL initializion
//                 (q) Save+Print+Mail: button flashes to indicate action
//                 (r) Save+Print+Mail: hourglass mouse cursor while busy
//                 (s) Save+Print+Mail: sound indicates success/failure
//                 (t) "SillyExceptions" don't get bug reports (saves time)
//                 (u) SendMapiMail improved (no receiver, user abort, logon)
// 2003-07-26 2.5  (1) disassembling output now added to bug report
//                 (2) available/total physical memory added to bug report
//                 (3) free disk size is now shown for OS and our start drive
//                 (4) TThread hook was not thread safe (ouch), now it is
//                 (5) some bug report specific options added
//                 (6) address for external exceptions was 1 asm instr. too far
//                 (7) madExcept version is shown in the bug report
//                 (8) bug report print functionality
//                 (9) handled/hidden exceptions can now optionally be caught
//                 (a) ImNotYetFrozen resets the freeze check timer
//                 (b) new options SuspendThreads, AutoClose, BugReportFileSize
//                 (c) automatic exception catching for clx projects
//                 (d) the save dialog doesn't freeze anymore
//                 (e) clipboard functionality didn't work reliably in win2k
//                 (f) box main msg var %exceptMsg% added (autowrap enabled)
//                 (g) box auto layout improved, big icon support "meiBigIcon"
//                 (h) box plays windows default error sound
//                 (i) D7 named threads are supported (name is extracted)
//                 (j) some minor changes in order to support Armadillo
//                 (k) option vars are now short strings instead of char arrays
//                 (l) only 1 close button available -> esc + [x] now supported
//                 (m) madExcept threads run on a higher priority now
//                 (n) all button action functions exports added
//                 (o) exception notification now uses pipe instead of window
//                 (p) system up time added to bug report
//                 (q) AutoContinue + AutoDelay options added
//                 (r) terminal server client name added to bug report
//                 (s) OwnerDrawButtons option added
//                 (t) support for FinalBuilder added (in form of a FB action)
// 2002-12-05 2.3a (1) complain if memory manager changed from init. -> final.
//                 (2) user name added to bug report header
//                 (3) "currentEsp" added to HandleException and CreateBugReport
//                     this should hopefully improve the stack trace quality
//                 (4) avoiding of handled exceptions when running inside of IDE
//                 (5) don't show exception box: continue/close were swapped
//                 (6) except box layout inconsistencies fixed
//                 (7) BugReportFile now also supports the %appname% variable
// 2002-11-14 2.3  (1) several changes to integrate the rewritten madStackTrace
//                 (2) ICriticalSection -> TRTLCriticalSection reduces footprint
//                 (3) CloseCreateThreadHook was not complete
//                 (4) no further unhandled exceptions during exception handling
//                 (5) system hooks are undone during finalization now
//                 (6) apptype console: no GUI box anymore, WriteLn instead
//                 (7) function exported for madTraceProcess tool
//                 (8) Automatically save bug report without path -> start path
//                 (9) unthreaded MAPI mailing again to improve fail detection
// 2002-10-22 2.2b SaveBugReport no go in 95/98, API structure was too new/long
// 2002-10-18 2.2a (1) forgot to remove test code from SendShellMail (dumb me)
//                 (2) got rid of LoadImage, it returned a shifted image in NT4
//                 (3) escape eventually destroyed the bug report detail memo
// 2002-10-12 2.2  (1) delphi 7 support
//                 (2) automatic exception catching in TThread works fine now
//                 (3) leak fixes stopped madTraceProcess from working
//                 (4) tweaked the problem "can we continue after an exception?"
//                 (5) the exception box's title bar is now freely adjustable
//                 (6) class name of TThread threads added to bug report
//                 (7) thread creator added to stack trace as bottom stack item
//                 (8) fixed some little leaks
//                 (9) bug report reformatted, module list added
//                 (a) exception box gets much bigger, if the report is shown
//                 (b) sometimes the except box didn't show up in finalization
//                     because Windows destroyed our HandleExceptionWnd window
//                 (c) mailto now uses CreateProcess instead of ShellExecute
//                     as a result the mail body length is not limited anymore
//                 (d) some tricks to avoid freezing when mailing with MAPI
//                 (e) CreateThread is now hooked in all modules/packages
//                 (f) new true color button bitmaps with 8 bit alpha channel
//                 (g) you can now store custom icons for the exception box
// 2002-08-02 2.1d Init/CloseAntiFreeze exported for use in NT services
// 2002-07-16 2.1c CloseHandleExceptionThread froze dynamic dlls during unload
// 2002-06-14 2.1b (1) AntiFreezeCheck is turned off, if window can't be created
//                 (2) little NT4 bug workaround, see TExceptionBox.Create
//                 (3) support for runtime package projects with only rtl (D6+)
//                 (4) resource/memory leaks fixed
//                 (5) instead of entry point patching the initialization order
//                     of the units SysUtils/madTools/madExcept is now changed
//                     this moves some hacks from the binary into the IDE wizard
//                     and more exceptions get caught now during finalization
//                 (6) when a thread closes, its name gets automatically del'ed
// 2002-02-24 2.1a (1) ShowException exported (needed by madTraceProcess)
//                 (2) bug report suppresses madTraceProcess.dll
//                 (2) mail button is now also available without email address
//                 (3) CreateThread hook was installed too often
// 2002-02-08 2.1  (1) madExcept now also runs 100% fine with runtime packages
//                 (2) "save bug report" button added + auto save option
//                 (3) email address max length increased to 75
//                 (4) email subject max length increased to 50
// 2002-01-22 2.0f kernel32.OpenThread is now used when available
// 2001-11-13 2.0e (1) new threads (CreateThread) are now hooked automatically
//                 (2) the new GetThreadList-NT worked on XP, but failed on NT4
// 2001-10-13 2.0d (1) GetThreadList-NT failed if too many threads were running
//                 (2) if GetThreadList failed, the initialization crashed
// 2001-08-13 2.0c time calculations corrected (time zones etc)
// 2001-08-09 2.0b WM_SETFONT font converted to integer to avoid range errors
// 2001-08-03 2.0a (1) serious bug in NameThread fixed (wrong allocation size)
//                 (2) SendShellMail now supports line breaks (thanks, Raymond!)
//                     so we can totally get rid of the "please press Ctrl+V"
// 2001-07-22 2.0  (1) new exception box with customizable buttons & messages
//                 (2) extended bug report
//                 (3) restart button or even automatic restart possible
//                 (4) callbacks for every exception action
//                 (5) integrated mail functionality (both mapi & ShellExecute)
//                 (6) exception handlers can be synchronized
//                 (7) main thread freeze detection
//                 (8) everything can be configured by the IDE wizard
//                 (9) during exception handling the other threads are paused
// 2001-06-10 1.5a "InitExceptionHandling" got faster
// 2001-04-30 1.5  (1) minor changes in order to get rid of SysUtils
//                 (2) "InitExceptionHandling" now called in initialization
//                 (3) automatic initialization as early as possible
//                 (4) unhandled runtime errors are finally also caught (!!)
// 2000-11-24 1.4e bugfix in 1.4d was faulty, refix, now it really works
// 2000-11-13 1.4d bug with ExceptProcAddr fixed

unit madExcept;

{$I mad.inc} {$W-}

{ $define log}

interface

uses Windows, madStackTrace, madDisAsm, madTypes, madNVAssistant, madNVBitmap;

{$ifndef ver120}{$ifndef ver130}{$define d6}{$ifndef ver140}{$define d7}{$endif}{$endif}{$endif}

// ***************************************************************

const CMadExceptVersion = '3.0c beta 5';

// ***************************************************************
// settings interfaces

type
  // forward 
  IMEFields      = interface;
  IMEAttachments = interface;

  // types needed by "IMESettings" properties
  TMEButton = (bSendBugReport, bSaveBugReport, bPrintBugReport, bShowBugReport,
               bContinueApplication, bRestartApplication, bCloseApplication);
  TMEDupDef = (ddExceptAddrIdentical, ddCrashStackIdentical, ddAllStacksIdentical);
  TMEShowSetting = (ssFullBox, ssAssistant, ssDetailBox, ssSimpleBox, ssNothing);

  // this interface is the basic settings interface
  // it's used for global settings (per module)
  // also it's part of the "IMEException" exception interface
  IMESettings = interface ['{27DBC590-890F-463E-8D05-32E33584119F}']
    function  Module  : dword;

    // ON EXCEPTION AUTO ACTIONS

    // various actions - automatically save bug report
    function  GetAutoSave : boolean;
    procedure SetAutoSave (value: boolean);
    property  AutoSave : boolean read GetAutoSave write SetAutoSave; // default true
    // various actions - only if bug report doesn't get sent
    function  GetAutoSaveIfNotSent : boolean;
    procedure SetAutoSaveIfNotSent (value: boolean);
    property  AutoSaveIfNotSent : boolean read GetAutoSaveIfNotSent write SetAutoSaveIfNotSent; // default true
    // various actions - automatically send bug report
    function  GetAutoSend : boolean;
    procedure SetAutoSend (value: boolean);
    property  AutoSend : boolean read GetAutoSend write SetAutoSend; // default false
    // various actions - show progress box during auto send
    function  GetAutoSendPrgrBox : boolean;
    procedure SetAutoSendPrgrBox (value: boolean);
    property  AutoSendPrgrBox : boolean read GetAutoSendPrgrBox write SetAutoSendPrgrBox; // default false
    // various actions - copy bug report to clipboard
    function  GetAutoClipboard : boolean;
    procedure SetAutoClipboard (value: boolean);
    property  AutoClipboard : boolean read GetAutoClipboard write SetAutoClipboard; // default false
    // various actions - pause all running delphi/bcb threads
    function  GetSuspendThreads : boolean;
    procedure SetSuspendThreads (value: boolean);
    property  SuspendThreads : boolean read GetSuspendThreads write SetSuspendThreads; // default false

    // if madExcept is busy while no window is visible - show "please wait" box
    function  GetShowPleaseWaitBox : boolean;
    procedure SetShowPleaseWaitBox (value: boolean);
    property  ShowPleaseWaitBox : boolean read GetShowPleaseWaitBox write SetShowPleaseWaitBox; // default true

    // application control - automatically continue application
    function  GetAutoContinue : boolean;
    procedure SetAutoContinue (value: boolean);
    property  AutoContinue : boolean read GetAutoContinue write SetAutoContinue; // default false
    // application control - automatically restart application
    // 0 = off; 1 = at once; xxx = after 10 crashes in xxx seconds
    function  GetAutoRestart : dword;
    procedure SetAutoRestart (value: dword);
    property  AutoRestart : dword read GetAutoRestart write SetAutoRestart; // default 0
    // application control - automatically close application
    // 0 = off; 1 = at once; xxx = after 10 crashes in xxx seconds
    function  GetAutoClose : dword;
    procedure SetAutoClose (value: dword);
    property  AutoClose : dword read GetAutoClose write SetAutoClose; // default 0

    // application control - progress delay  (runtime only option)
    function  GetAutoDelay : dword;
    procedure SetAutoDelay (value: dword);
    property  AutoDelay : dword read GetAutoDelay write SetAutoDelay; // default 60

    // EXCEPTION BOX SETTINGS

    // button configuration - "send bug report" visible
    // default: everything visible except SaveBugReport and PrintBugReport
    function      GetSendBtnVisible : boolean;
    function      GetSaveBtnVisible : boolean;
    function     GetPrintBtnVisible : boolean;
    function      GetShowBtnVisible : boolean;
    function  GetContinueBtnVisible : boolean;
    function   GetRestartBtnVisible : boolean;
    function     GetCloseBtnVisible : boolean;
    procedure     SetSendBtnVisible (value: boolean);
    procedure     SetSaveBtnVisible (value: boolean);
    procedure    SetPrintBtnVisible (value: boolean);
    procedure     SetShowBtnVisible (value: boolean);
    procedure SetContinueBtnVisible (value: boolean);
    procedure  SetRestartBtnVisible (value: boolean);
    procedure    SetCloseBtnVisible (value: boolean);
    property     SendBtnVisible : boolean read     GetSendBtnVisible write     SetSendBtnVisible;
    property     SaveBtnVisible : boolean read     GetSaveBtnVisible write     SetSaveBtnVisible;
    property    PrintBtnVisible : boolean read    GetPrintBtnVisible write    SetPrintBtnVisible;
    property     ShowBtnVisible : boolean read     GetShowBtnVisible write     SetShowBtnVisible;
    property ContinueBtnVisible : boolean read GetContinueBtnVisible write SetContinueBtnVisible;
    property  RestartBtnVisible : boolean read  GetRestartBtnVisible write  SetRestartBtnVisible;
    property    CloseBtnVisible : boolean read    GetCloseBtnVisible write    SetCloseBtnVisible;
    // button configuration - button focus
    function  GetFocusedButton : TMEButton;
    procedure SetFocusedButton (value: TMEButton);
    property  FocusedButton : TMEButton read GetFocusedButton write SetFocusedButton; // default mebSendBugReport
    // button configuration - send assistant
    function  GetSendAssistant : string;
    procedure SetSendAssistant (value: string);
    property  SendAssistant : string read GetSendAssistant write SetSendAssistant; // default 'SendAssistant'
    // button configuration - save assistant
    function  GetSaveAssistant : string;
    procedure SetSaveAssistant (value: string);
    property  SaveAssistant : string read GetSaveAssistant write SetSaveAssistant; // default 'SaveAssistant'
    // button configuration - print assistant
    function  GetPrintAssistant : string;
    procedure SetPrintAssistant (value: string);
    property  PrintAssistant : string read GetPrintAssistant write SetPrintAssistant; // default 'PrintAssistant'

    // various options - automatically show bug report
    function  GetAutoShowBugReport : boolean;
    procedure SetAutoShowBugReport (value: boolean);
    property  AutoShowBugReport : boolean read GetAutoShowBugReport write SetAutoShowBugReport; // default false
    // various options - show standard buttons instead of fading owner draw buttons
    function  GetNoOwnerDrawButtons : boolean;
    procedure SetNoOwnerDrawButtons (value: boolean);
    property  NoOwnerDrawButtons : boolean read GetNoOwnerDrawButtons write SetNoOwnerDrawButtons; // default false

    // EMAIL & UPLOAD SETTINGS

    // basic send settings - mail receiver(s)
    function  GetMailAddr : string;
    procedure SetMailAddr (value: string);
    property  MailAddr : string read GetMailAddr write SetMailAddr; // default ''
    // basic send settings - send bug report in background
    function  GetSendInBackground : boolean;
    procedure SetSendInBackground (value: boolean);
    property  SendInBackground : boolean read GetSendInBackground write SetSendInBackground; // default true

    // which methods may be used to send the bug report - act as a SMTP mail server
    // works only if the PC is already online
    function  GetMailAsSmtpServer : boolean;
    procedure SetMailAsSmtpServer (value: boolean);
    property  MailAsSmtpServer : boolean read GetMailAsSmtpServer write SetMailAsSmtpServer; // default false
    // which methods may be used to send the bug report - act as a SMTP mail client
    // either with or without authentification
    // works only if the PC is already online
    function  GetMailAsSmtpClient : boolean;
    procedure SetMailAsSmtpClient (value: boolean);
    property  MailAsSmtpClient : boolean read GetMailAsSmtpClient write SetMailAsSmtpClient; // default false
    // which methods may be used to send the bug report - upload to HTTP web server
    // either with or without authentification
    // works only if the PC is already online
    function  GetUploadViaHttp : boolean;
    procedure SetUploadViaHttp (value: boolean);
    property  UploadViaHttp : boolean read GetUploadViaHttp write SetUploadViaHttp; // default false
    // which methods may be used to send the bug report - contact the mail client via MAPI
    function  GetMailViaMapi : boolean;
    procedure SetMailViaMapi (value: boolean);
    property  MailViaMapi : boolean read GetMailViaMapi write SetMailViaMapi; // default true
    // which methods may be used to send the bug report - contact the mail client via mailto
    function  GetMailViaMailto : boolean;
    procedure SetMailViaMailto (value: boolean);
    property  MailViaMailto : boolean read GetMailViaMailto write SetMailViaMailto; // default true

    // SMTP client settings - SMTP server
    function  GetSmtpServer : string;
    procedure SetSmtpServer (value: string);
    property  SmtpServer : string read GetSmtpServer write SetSmtpServer; // default ''
    // SMTP client settings - SMTP port
    function  GetSmtpPort : dword;
    procedure SetSmtpPort (value: dword);
    property  SmtpPort : dword read GetSmtpPort write SetSmtpPort; // default 25
    // SMTP client settings - authentification user  (optional)
    function  GetSmtpAccount : string;
    procedure SetSmtpAccount (value: string);
    property  SmtpAccount : string read GetSmtpAccount write SetSmtpAccount; // default ''
    // SMTP client settings - authentification password  (optional)
    function  GetSmtpPassword : string;
    procedure SetSmtpPassword (value: string);
    property  SmtpPassword : string read GetSmtpPassword write SetSmtpPassword; // default ''

    // HTTP client settings - upload url
    function  GetHttpServer : string;
    procedure SetHttpServer (value: string);
    property  HttpServer : string read GetHttpServer write SetHttpServer; // default ''
    // HTTP client settings - HTTP port
    function  GetHttpPort : dword;
    procedure SetHttpPort (value: dword);
    property  HttpPort : dword read GetHttpPort write SetHttpPort; // default 80
    // HTTP client settings - authentification user  (optional)
    function  GetHttpAccount : string;
    procedure SetHttpAccount (value: string);
    property  HttpAccount : string read GetHttpAccount write SetHttpAccount; // default ''
    // HTTP client settings - authentification password  (optional)
    function  GetHttpPassword : string;
    procedure SetHttpPassword (value: string);
    property  HttpPassword : string read GetHttpPassword write SetHttpPassword; // default ''

    // runtime only settings - SMTP mail sender
    function  GetMailFrom : string;
    procedure SetMailFrom (value: string);
    property  MailFrom : string read GetMailFrom write SetMailFrom; // default ''
    // runtime only settings - additional fields for http uploads
    function  GetAdditionalFields : IMEFields;
    property  AdditionalFields : IMEFields read GetAdditionalFields;

    // ATTACHMENTS SETTINGS

    // bug report & screen shot attachments - attach bug report
    function  GetAttachBugReport : boolean;
    procedure SetAttachBugReport (value: boolean);
    property  AttachBugReport : boolean read GetAttachBugReport write SetAttachBugReport; // default true
    // bug report & screen shot attachments - attach bug report file
    function  GetAttachBugReportFile : boolean;
    procedure SetAttachBugReportFile (value: boolean);
    property  AttachBugReportFile : boolean read GetAttachBugReportFile write SetAttachBugReportFile; // default true
    // bug report & screen shot attachments - empty bug report file afterwards
    function  GetDeleteBugReportFile : boolean;
    procedure SetDeleteBugReportFile (value: boolean);
    property  DeleteBugReportFile : boolean read GetDeleteBugReportFile write SetDeleteBugReportFile; // default true
    // file names & zip configuration - bug report send as
    function  GetBugReportSendAs : string;
    procedure SetBugReportSendAs (value: string);
    property  BugReportSendAs : string read GetBugReportSendAs write SetBugReportSendAs; // default 'bugreport.txt'
    // file names & zip configuration - bug report zip
    function  GetBugReportZip : string;
    procedure SetBugReportZip (value: string);
    property  BugReportZip : string read GetBugReportZip write SetBugReportZip; // default ''
    // bug report file & screen shot attachments - attach screen shot
    // 0 = none; 4 = 16 grays; 8 = 256 cols; xx * 1024 = try to keep the size < xx KB
    function  GetScreenShotDepth : integer;
    procedure SetScreenShotDepth (value: integer);
    property  ScreenShotDepth : integer read GetScreenShotDepth write SetScreenShotDepth; // default 8
    // bug report file & screen shot attachments - capture windows of the current application only
    function  GetScreenShotAppOnly : boolean;
    procedure SetScreenShotAppOnly (value: boolean);
    property  ScreenShotAppOnly : boolean read GetScreenShotAppOnly write SetScreenShotAppOnly; // default false
    // file names & zip configuration - screen shot send as
    function  GetScreenShotSendAs : string;
    procedure SetScreenShotSendAs (value: string);
    property  ScreenShotSendAs : string read GetScreenShotSendAs write SetScreenShotSendAs; // default 'screenshot.png'
    // file names & zip configuration - screen shot zip
    function  GetScreenShotZip : string;
    procedure SetScreenShotZip (value: string);
    property  ScreenShotZip : string read GetScreenShotZip write SetScreenShotZip; // default ''
    // file names & zip configuration - also attach this
    function  GetAdditionalAttachments : IMEAttachments;
    property  AdditionalAttachments : IMEAttachments read GetAdditionalAttachments;

    // SAVE SETTINGS

    // basic bug report file settings - bug report file
    function  GetBugReportFile : string;
    procedure SetBugReportFile (value: string);
    property  BugReportFile : string read GetBugReportFile write SetBugReportFile; // default 'bugreport.txt'
    // basic bug report file settings - append bug reports
    function  GetAppendBugReports : boolean;
    procedure SetAppendBugReports (value: boolean);
    property  AppendBugReports : boolean read GetAppendBugReports write SetAppendBugReports; // default true
    // basic bug report file settings - bug report bug max file size
    function  GetBugReportFileSize : dword;
    procedure SetBugReportFileSize (value: dword);
    property  BugReportFileSize : dword read GetBugReportFileSize write SetBugReportFileSize; // default 100000
    // basic bug report file settings - don't save duplicate exceptions
    function  GetNoDupExcepts : boolean;
    procedure SetNoDupExcepts (value: boolean);
    property  NoDupExcepts : boolean read GetNoDupExcepts write SetNoDupExcepts; // default true
    // basic bug report file settings - don't save duplicate main thread freezings
    function  GetNoDupFreezes : boolean;
    procedure SetNoDupFreezes (value: boolean);
    property  NoDupFreezes : boolean read GetNoDupFreezes write SetNoDupFreezes; // default true

    // "duplicate exceptions" definition
    function  GetDupExceptDef : TMEDupDef;
    procedure SetDupExceptDef (value: TMEDupDef);
    property  DupExceptDef : TMEDupDef read GetDupExceptDef write SetDupExceptDef; // default meddCrashStackIdentical
    // "duplicate main thread freezings" definition
    function  GetDupFreezeDef : TMEDupDef;
    procedure SetDupFreezeDef (value: TMEDupDef);
    property  DupFreezeDef : TMEDupDef read GetDupFreezeDef write SetDupFreezeDef; // default meddAllStacksIdentical

    // BUG REPORT SETTINGS

    // what to you want to be contained in the bug report - callstack of all running threads
    function  GetListThreads : boolean;
    procedure SetListThreads (value: boolean);
    property  ListThreads : boolean read GetListThreads write SetListThreads; // default true
    // what to you want to be contained in the bug report - cpu registers
    function  GetShowCpuRegisters : boolean;
    procedure SetShowCpuRegisters (value: boolean);
    property  ShowCpuRegisters : boolean read GetShowCpuRegisters write SetShowCpuRegisters; // default true
    // what to you want to be contained in the bug report - stack dump
    function  GetShowStackDump : boolean;
    procedure SetShowStackDump (value: boolean);
    property  ShowStackDump : boolean read GetShowStackDump write SetShowStackDump; // default true
    // what to you want to be contained in the bug report - exception location disassembly
    function  GetShowDisAsm : boolean;
    procedure SetShowDisAsm (value: boolean);
    property  ShowDisAsm : boolean read GetShowDisAsm write SetShowDisAsm; // default true

    // callstack options - hide callstack items which have no line number information
    function  GetHideUglyItems : boolean;
    procedure SetHideUglyItems (value: boolean);
    property  HideUglyItems : boolean read GetHideUglyItems write SetHideUglyItems; // default false
    // callstack options - show address offset to the beginning of the procedure
    function  GetShowRelativeAddrs : boolean;
    procedure SetShowRelativeAddrs (value: boolean);
    property  ShowRelativeAddrs : boolean read GetShowRelativeAddrs write SetShowRelativeAddrs; // default true
    // callstack options - show line number offset to the beginning of the procedure
    function  GetShowRelativeLines : boolean;
    procedure SetShowRelativeLines (value: boolean);
    property  ShowRelativeLines : boolean read GetShowRelativeLines write SetShowRelativeLines; // default true

    // disassembly fine tuning - format disassembly output
    function  GetFormatDisassembly : boolean;
    procedure SetFormatDisassembly (value: boolean);
    property  FormatDisassembly : boolean read GetFormatDisassembly write SetFormatDisassembly; // default false
    // disassembly fine tuning - limit number of asm instructions  (above & below exception location)
    function  GetLimitDisassembly : integer;
    procedure SetLimitDisassembly (value: integer);
    property  LimitDisassembly : integer read GetLimitDisassembly write SetLimitDisassembly; // default 5

    // registered plugins
    function  GetPluginEnabled (plugin: string) : boolean;
    procedure SetPluginEnabled (plugin: string; value: boolean);
    property  PluginEnabled [plugin: string] : boolean read GetPluginEnabled write SetPluginEnabled;

    // still supported, but not adjustable in settings GUI, anymore
    function  VersionVar : string; // default ''

    // Assistant CREATOR

    // create an instance of the assistant with the specified name
    function  GetAssistant (name: string) : INVAssistant;

    // CUSTOM STRINGS

    // except box - title bar
    function  GetTitleBar : string;
    procedure SetTitleBar (value: string);
    property  TitleBar : string read GetTitleBar write SetTitleBar; // default '%appname%'
    // except box - exception message
    // default: 'An error occurred in the application.'
    function  GetExceptMsg : string;
    procedure SetExceptMsg (value: string);
    property  ExceptMsg : string read GetExceptMsg write SetExceptMsg;
    // except box - frozen message
    // default: 'The application seems to be frozen.'
    function  GetFrozenMsg : string;
    procedure SetFrozenMsg (value: string);
    property  FrozenMsg : string read GetFrozenMsg write SetFrozenMsg;
    // except box - bit fault message
    // default: 'The file "%modname%" seems to be corrupt!'
    function  GetBitFaultMsg : string;
    procedure SetBitFaultMsg (value: string);
    property  BitFaultMsg : string read GetBitFaultMsg write SetBitFaultMsg;
    // except box - button captions
    // defaults: 'send/save/print/show bug report', 'continue/restart/close application'
    function      GetSendBtnCaption : string;
    function      GetSaveBtnCaption : string;
    function     GetPrintBtnCaption : string;
    function      GetShowBtnCaption : string;
    function  GetContinueBtnCaption : string;
    function   GetRestartBtnCaption : string;
    function     GetCloseBtnCaption : string;
    procedure     SetSendBtnCaption (value: string);
    procedure     SetSaveBtnCaption (value: string);
    procedure    SetPrintBtnCaption (value: string);
    procedure     SetShowBtnCaption (value: string);
    procedure SetContinueBtnCaption (value: string);
    procedure  SetRestartBtnCaption (value: string);
    procedure    SetCloseBtnCaption (value: string);
    property     SendBtnCaption : string read     GetSendBtnCaption write     SetSendBtnCaption;
    property     SaveBtnCaption : string read     GetSaveBtnCaption write     SetSaveBtnCaption;
    property    PrintBtnCaption : string read    GetPrintBtnCaption write    SetPrintBtnCaption;
    property     ShowBtnCaption : string read     GetShowBtnCaption write     SetShowBtnCaption;
    property ContinueBtnCaption : string read GetContinueBtnCaption write SetContinueBtnCaption;
    property  RestartBtnCaption : string read  GetRestartBtnCaption write  SetRestartBtnCaption;
    property    CloseBtnCaption : string read    GetCloseBtnCaption write    SetCloseBtnCaption;

    // message box - button captions
    // defaults: 'OK/Details'
    function       GetOkBtnCaption : string;
    function  GetDetailsBtnCaption : string;
    procedure      SetOkBtnCaption (value: string);
    procedure SetDetailsBtnCaption (value: string);
    property          OkBtnCaption : string read      GetOkBtnCaption write      SetOkBtnCaption;
    property     DetailsBtnCaption : string read GetDetailsBtnCaption write SetDetailsBtnCaption;

    // please wait box - strings
    // defaults: 'Information/Please wait a moment...'
    function  GetPleaseWaitTitle : string;
    function  GetPleaseWaitText  : string;
    procedure SetPleaseWaitTitle (value: string);
    procedure SetPleaseWaitText  (value: string);
    property     PleaseWaitTitle : string read GetPleaseWaitTitle write SetPleaseWaitTitle;
    property     PleaseWaitText  : string read GetPleaseWaitText  write SetPleaseWaitText;

    // send - mail subject
    // default: 'bug report'
    function  GetMailSubject : string;
    procedure SetMailSubject (value: string);
    property  MailSubject : string read GetMailSubject write SetMailSubject; // default 'bug report'
    // send - mail body
    // default: 'please find the bug report attached'
    function  GetMailBody : string;
    procedure SetMailBody (value: string);
    property  MailBody : string read GetMailBody write SetMailBody;
    // send - send box title
    // default: 'Sending bug report...'
    function  GetSendBoxTitle : string;
    procedure SetSendBoxTitle (value: string);
    property  SendBoxTitle : string read GetSendBoxTitle write SetSendBoxTitle;
    // send - prepare attachments
    // default: 'Preparing attachments...'
    function  GetPrepareAttachMsg : string;
    procedure SetPrepareAttachMsg (value: string);
    property  PrepareAttachMsg : string read GetPrepareAttachMsg write SetPrepareAttachMsg;
    // send - mx lookup
    // default: 'Searching for mail server...'
    function  GetMxLookupMsg : string;
    procedure SetMxLookupMsg (value: string);
    property  MxLookupMsg : string read GetMxLookupMsg write SetMxLookupMsg;
    // send - server connect
    // default: 'Connecting to server...'
    function  GetConnectMsg : string;
    procedure SetConnectMsg (value: string);
    property  ConnectMsg : string read GetConnectMsg write SetConnectMsg;
    // send - authentication
    // default: 'Authentication...'
    function  GetAuthMsg : string;
    procedure SetAuthMsg (value: string);
    property  AuthMsg : string read GetAuthMsg write SetAuthMsg;
    // send - send mail
    // default: 'Sending mail...'
    function  GetSendMailMsg : string;
    procedure SetSendMailMsg (value: string);
    property  SendMailMsg : string read GetSendMailMsg write SetSendMailMsg;
    // send - set http fields
    // default: 'Setting fields...'
    function  GetFieldsMsg : string;
    procedure SetFieldsMsg (value: string);
    property  FieldsMsg : string read GetFieldsMsg write SetFieldsMsg;
    // send - sending attachments
    // default: 'Sending attachments...'
    function  GetSendAttachMsg : string;
    procedure SetSendAttachMsg (value: string);
    property  SendAttachMsg : string read GetSendAttachMsg write SetSendAttachMsg;
    // send - finalization
    // default: 'Finalizing...'
    function  GetSendFinalizeMsg : string;
    procedure SetSendFinalizeMsg (value: string);
    property  SendFinalizeMsg : string read GetSendFinalizeMsg write SetSendFinalizeMsg;
    // send - failure message
    // default: 'Sorry, sending the bug report didn''t work.'
    function  GetSendFailureMsg : string;
    procedure SetSendFailureMsg (value: string);
    property  SendFailureMsg : string read GetSendFailureMsg write SetSendFailureMsg;
  end;

  // this interface adds some more settings, which are only available globally
  IMEModuleSettings = interface (IMESettings) ['{E2196B25-D73A-4F69-B79B-D65539763B31}']
    function  IsValid : boolean;
    function  Enabled : boolean;

    // BASIC SETTINGS

    // general options - include minimal debug info only
    function  MinDebugInfoOnly : boolean; // default false
    // general options - this module gets no own madExcept settings
    function  NoOwnSettings : boolean; // default false

    // runtime checks - check whether binary file is corrupt (via crc)
    function  CheckFileCrc : boolean; // default true
    // runtime checks - check for frozen main thread
    function  CheckForFreeze : boolean; // default false
    // runtime checks - freeze timeout (sec)
    function  FreezeTimeout : dword; // default 60

    // EXCEPTION FILTER

    // exception class filter - classes
    function  GetFilter1Classes : string;
    function  GetFilter2Classes : string;
    procedure SetFilter1Classes (value: string);
    procedure SetFilter2Classes (value: string);
    property  Filter1Classes : string read GetFilter1Classes write SetFilter1Classes; // default 'EDBEditError'
    property  Filter2Classes : string read GetFilter2Classes write SetFilter2Classes; // default ''
    // exception class filter - don't create a bug report
    function  GetFilter1NoBugReport : boolean;
    function  GetFilter2NoBugReport : boolean;
    function  GetGeneralNoBugReport : boolean;
    procedure SetFilter1NoBugReport (value: boolean);
    procedure SetFilter2NoBugReport (value: boolean);
    procedure SetGeneralNoBugReport (value: boolean);
    property  Filter1NoBugReport : boolean read GetFilter1NoBugReport write SetFilter1NoBugReport; // default true
    property  Filter2NoBugReport : boolean read GetFilter2NoBugReport write SetFilter2NoBugReport; // default false
    property  GeneralNoBugReport : boolean read GetGeneralNoBugReport write SetGeneralNoBugReport; // default false
    // exception class filter - don't create a screen shot
    function  GetFilter1NoScreenShot : boolean;
    function  GetFilter2NoScreenShot : boolean;
    function  GetGeneralNoScreenShot : boolean;
    procedure SetFilter1NoScreenShot (value: boolean);
    procedure SetFilter2NoScreenShot (value: boolean);
    procedure SetGeneralNoScreenShot (value: boolean);
    property  Filter1NoScreenShot : boolean read GetFilter1NoScreenShot write SetFilter1NoScreenShot; // default true
    property  Filter2NoScreenShot : boolean read GetFilter2NoScreenShot write SetFilter2NoScreenShot; // default false
    property  GeneralNoScreenShot : boolean read GetGeneralNoScreenShot write SetGeneralNoScreenShot; // default false
    // exception class filter - don't pause the running threads
    function  GetFilter1NoSuspend : boolean;
    function  GetFilter2NoSuspend : boolean;
    function  GetGeneralNoSuspend : boolean;
    procedure SetFilter1NoSuspend (value: boolean);
    procedure SetFilter2NoSuspend (value: boolean);
    procedure SetGeneralNoSuspend (value: boolean);
    property  Filter1NoSuspend : boolean read GetFilter1NoSuspend write SetFilter1NoSuspend; // default true
    property  Filter2NoSuspend : boolean read GetFilter2NoSuspend write SetFilter2NoSuspend; // default false
    property  GeneralNoSuspend : boolean read GetGeneralNoSuspend write SetGeneralNoSuspend; // default false
    // exception class filter - don't call the exception handlers
    function  GetFilter1NoHandlers : boolean;
    function  GetFilter2NoHandlers : boolean;
    function  GetGeneralNoHandlers : boolean;
    procedure SetFilter1NoHandlers (value: boolean);
    procedure SetFilter2NoHandlers (value: boolean);
    procedure SetGeneralNoHandlers (value: boolean);
    property  Filter1NoHandlers : boolean read GetFilter1NoHandlers write SetFilter1NoHandlers; // default true
    property  Filter2NoHandlers : boolean read GetFilter2NoHandlers write SetFilter2NoHandlers; // default false
    property  GeneralNoHandlers : boolean read GetGeneralNoHandlers write SetGeneralNoHandlers; // default false
    // exception class filter - show setting
    function  GetFilter1ShowSetting : TMEShowSetting;
    function  GetFilter2ShowSetting : TMEShowSetting;
    function  GetGeneralShowSetting : TMEShowSetting;
    procedure SetFilter1ShowSetting (value: TMEShowSetting);
    procedure SetFilter2ShowSetting (value: TMEShowSetting);
    procedure SetGeneralShowSetting (value: TMEShowSetting);
    property  Filter1ShowSetting : TMEShowSetting read GetFilter1ShowSetting write SetFilter1ShowSetting; // default fssSimpleBox
    property  Filter2ShowSetting : TMEShowSetting read GetFilter2ShowSetting write SetFilter2ShowSetting; // default fssFullBox
    property  GeneralShowSetting : TMEShowSetting read GetGeneralShowSetting write SetGeneralShowSetting; // default fssFullBox
    // exception class filter - assistant
    function  GetFilter1Assistant : string;
    function  GetFilter2Assistant : string;
    function  GetGeneralAssistant : string;
    procedure SetFilter1Assistant (value: string);
    procedure SetFilter2Assistant (value: string);
    procedure SetGeneralAssistant (value: string);
    property  Filter1Assistant : string read GetFilter1Assistant write SetFilter1Assistant; // default ''
    property  Filter2Assistant : string read GetFilter2Assistant write SetFilter2Assistant; // default ''
    property  GeneralAssistant : string read GetGeneralAssistant write SetGeneralAssistant; // default ''

    // RELOAD - for use in combination with 3rd party translators

    procedure Reload;
  end;

  // used for mail & upload attachments
  IMEAttachments = interface ['{7DE1DA9C-28D7-4527-954F-F951B88BEE41}']
    procedure Lock;
    procedure Unlock;

    function  GetItemCount : integer;
    property  ItemCount : integer read GetItemCount;

    function  GetItem (index: integer;
                       var originalFile   : string;
                       var sendAsFileName : string;
                       var zipFile        : string;
                       var fieldName      : string) : boolean;

    procedure Add (originalFile   : string;
                   sendAsFileName : string = '';
                   zipFile        : string = '';
                   fieldName      : string = '');

    function  Delete (originalFile: string) : boolean;
    procedure Clear;

    function  Clone : IMEAttachments;
  end;

  // used for various purposes, e.g. bug report header and bug report sections
  IMEFields = interface ['{AF0EA7A7-2B36-46FD-BF2E-245ABA2740E2}']
    procedure Lock;
    procedure Unlock;

    function  GetItemCount : integer;
    property  ItemCount : integer read GetItemCount;

    function  GetItem (index: integer) : string;
    procedure SetItem (index: integer; value: string);
    property  Items   [index: integer] : string read GetItem write SetItem;

    function  FindItem (item: string) : integer;

    function  GetContent (item: string) : string;
    procedure SetContent (item: string; value: string);
    property  Contents [item: string] : string read GetContent write SetContent; default;

    procedure Add    (                item, content: string);
    procedure Insert (index: integer; item, content: string);
    procedure Delete (index: integer                       ); overload;
    procedure Delete (                item         : string); overload;

    function  Clone : IMEFields;
  end;

// you can access the settings of any module which is compiled with madExcept
function MESettings                    : IMEModuleSettings; overload;
function MESettings (module : dword  ) : IMEModuleSettings; overload;
function MESettings (addr   : pointer) : IMEModuleSettings; overload;

// ***************************************************************
// exception interface

type
  // forward
  IMEException = interface;

  // what kind of exception is this?
  TExceptType = (etNormal, etFrozen, etHidden);

  // who initiated madExcept exception handling?
  TExceptSource = (esManual,                         // you called HandleException manually
                   esAntiFreezeCheck,                // the anti freeze check gave alarm
                   esBcbAbnormalProgramTermination,  // BCB: abnormal program termination detected
                   esSysUtilsShowException,          // SysUtils.ShowException was called
                   esApplicationShowException,       // Application.ShowException was called
                   esApplicationHandleException,     // Application.HandleException was called
                   esCGI,                            // TCGIApplication.HandleException was called
                   esISAPI,                          // TISAPIApplication.HandleException was called
                   esHttpExtension,                  // exception inside of our HttpExtensionProc
                   esIntraweb,                       // exception inside of IntraWeb client session
                   esTThreadSynchronize,             // exception inside of TThread.Synchronize
                   esRuntimeError,                   // runtime error detected
                   esInitializePackage,              // init. of dynamically loaded package crashed
                   esExceptProc,                     // System.ExceptProc was called
                   esSystemExceptionHandler,         // System.ExceptionHandler caught exception
                   esHidden,                         // hidden exception was detected
                   esThreadFrame,                    // win32 thread function crashed
                   esTThreadExecute,                 // TThread.Execute crashed
                   esUnhandledFilter);               // unhandled exception filter was called

  // in what phase of exception handling shall this handler get called?
  TExceptPhase = (epNotHandledYet,   // for manually created exception only
                                     // see "NewException"
                  epQuickFiltering,  // called before bugReport/screenShot are created
                                     // main purpose: fast exception filtering
                  epMainPhase,       // you can do everything here, e.g. showing your
                                     // own exception box, filtering, whatever
                  epPostProcessing,  // you can't filter exceptions (= abort exception
                                     // handling) during this phase, anymore
                  epCompleteReport   // gets called when the rendering of the bug
                                     // report is complete
                 );

  // this callback is called when the bug report changes
  TBugReportCallback   = procedure (complete: boolean; bugReport: string; exceptIntf: IMEException);
  TBugReportCallbackOO = procedure (complete: boolean; bugReport: string; exceptIntf: IMEException) of object;

  // the one and only exception interface
  IMEException = interface (IMESettings) ['{5EE7E49B-571F-4504-B9E6-1EEF6587C51F}']

    // STATE INFORMATION

    function  GetPhase : TExceptPhase;
    property  Phase : TExceptPhase read GetPhase;

    function  GetCanContinue : boolean;
    procedure SetCanContinue (value: boolean);
    property  CanContinue : boolean read GetCanContinue write SetCanContinue;

    // EXCEPTION INFORMATION

    function  GetExceptType : TExceptType;
    property  ExceptType : TExceptType read GetExceptType;

    function  GetExceptObject : TObject;
    property  ExceptObject : TObject read GetExceptObject;

    function  GetExceptClass : string;
    property  ExceptClass : string read GetExceptClass;

    function  GetExceptMessage : string;
    property  ExceptMessage : string read GetExceptMessage;

    function  GetExceptAddr : pointer;
    property  ExceptAddr : pointer read GetExceptAddr;

    function  GetCrashedThreadId : dword;
    property  CrashedThreadId : dword read GetCrashedThreadId;

    function  GetCallstackCrc (index: integer) : dword;
    property  CallstackCrc [index: integer] : dword read GetCallstackCrc;

    function  GetContext : PContext;
    property  Context : PContext read GetContext;

    function  GetSource : TExceptSource;
    property  Source : TExceptSource read GetSource;

    function  GetRelatedObject : TObject;
    property  RelatedObject : TObject read GetRelatedObject;

    function  GetPackage : dword;
    property  Package : dword read GetPackage;

    function  GetBugReportHeader : IMEFields;
    property  BugReportHeader : IMEFields read GetBugReportHeader;

    function  GetBugReportSections : IMEFields;
    property  BugReportSections : IMEFields read GetBugReportSections;

    function  GetBugReport_ : string;
    procedure SetBugReport (value: string);
    function  GetBugReport (mustBeComplete: boolean = true) : string;
    property  BugReport : string read GetBugReport_ write SetBugReport;

    function  GetScreenShot : INVBitmap;
    procedure SetScreenShot (value: INVBitmap);
    property  ScreenShot : INVBitmap read GetScreenShot write SetScreenShot;

    // CALLBACKS AND RELATED

    procedure BeginUpdate;
    procedure   EndUpdate;

    procedure   RegisterBugReportCallback (bugReportCallback: TBugReportCallback;   critical: boolean); overload;
    procedure   RegisterBugReportCallback (bugReportCallback: TBugReportCallbackOO; critical: boolean); overload;
    procedure UnregisterBugReportCallback (bugReportCallback: TBugReportCallback  ); overload;
    procedure UnregisterBugReportCallback (bugReportCallback: TBugReportCallbackOO); overload;

    // PRE PHASE SETTINGS - ONLY VALID DURING EXCEPTION HANDLER PRE PHASE

    function  GetCreateBugReport : boolean;
    procedure SetCreateBugReport (value: boolean);
    property  CreateBugReport : boolean read GetCreateBugReport write SetCreateBugReport;

    function  GetCreateScreenShot : boolean;
    procedure SetCreateScreenShot (value: boolean);
    property  CreateScreenShot : boolean read GetCreateScreenShot write SetCreateScreenShot;

    function  GetCallHandlers : boolean;
    procedure SetCallHandlers (value: boolean);
    property  CallHandlers : boolean read GetCallHandlers write SetCallHandlers;

    // NON-VISIBLE SETTINGS - VALID DURING ALL EXCEPTION HANDLER PHASES

    function  GetAppendScreenShot : boolean;
    procedure SetAppendScreenShot (value: boolean);
    property  AppendScreenShot : boolean read GetAppendScreenShot write SetAppendScreenShot;

    // VISIBLE SETTINGS - VALID DURING ALL EXCEPTION HANDLER PHASES

    function  GetShowSetting : TMEShowSetting;
    procedure SetShowSetting (value: TMEShowSetting);
    property  ShowSetting : TMEShowSetting read GetShowSetting write SetShowSetting;

    function  GetShowAssistant : string;
    procedure SetShowAssistant (value: string);
    property  ShowAssistant : string read GetShowAssistant write SetShowAssistant;

    // ACTION

    procedure Show;
  end;

// creates a new  IMEException interface
// you can e.g. call its "Show" and "BugReport" methods
function NewException (exceptType      : TExceptType       = etFrozen;
                       exceptObject    : TObject           = nil;
                       exceptAddr      : pointer           = nil;
                       canContinue     : boolean           = true;
                       crashedThreadId : dword             = 0;
                       currentEsp      : dword             = 0;
                       currentEbp      : dword             = 0;
                       context         : PContext          = nil;
                       settings        : IMEModuleSettings = nil;
                       source          : TExceptSource     = esManual;
                       relatedObject   : TObject           = nil;
                       package         : dword             = 0;
                       preparedStack   : pointer           = nil     ) : IMEException;

// do you want to be notified when a madExcept exception box was created?
var OnExceptBoxCreate : procedure (exceptBox    : dword;
                                   simpleMsgBox : boolean) = nil;

// ***************************************************************
// callbacks

type
  // type for all handlers
  // shall we try to synchronize the handler?
  // if yes, how shall we behave if synchronization fails?
  TSyncType = (stDontSync, stTrySyncCallOnSuccess, stTrySyncCallAlways);

  // types for RegisterExceptionHandler
  // set "handled" to true, if you have completely handled the exception
  TExceptEvent   = procedure (const exceptIntf : IMEException;
                              var handled      : boolean);
  TExceptEventOO = procedure (const exceptIntf : IMEException;
                              var handled      : boolean) of object;

// installs custom exception handlers
procedure  RegisterExceptionHandler (exceptHandler: TExceptEvent;   sync: TSyncType; phase: TExceptPhase = epMainPhase); overload;
procedure  RegisterExceptionHandler (exceptHandler: TExceptEventOO; sync: TSyncType; phase: TExceptPhase = epMainPhase); overload;
function UnregisterExceptionHandler (exceptHandler: TExceptEvent  ) : boolean; overload;
function UnregisterExceptionHandler (exceptHandler: TExceptEventOO) : boolean; overload;

type
  // button enumeration for the exception action events
  TExceptAction = (eaSendBugReport, eaSaveBugReport, eaPrintBugReport, eaShowBugReport,
                   eaContinueApplication, eaRestartApplication, eaCloseApplication);

  // types for RegisterExceptActionHandler
  // set "handled" to true, if you have done all the sending/saving work
  // this has meaning only, though, if the action is eaSend/Save/PrintBugReport
  // if frozen is set, exceptObject and exceptAddr are both nil
  TExceptActionEvent   = procedure (action           : TExceptAction;
                                    const exceptIntf : IMEException;
                                    var handled      : boolean      );
  TExceptActionEventOO = procedure (action           : TExceptAction;
                                    const exceptIntf : IMEException;
                                    var handled      : boolean      ) of object;

// these handlers are called whenever action happens as a result of an exception
procedure  RegisterExceptActionHandler (actionHandler: TExceptActionEvent;   sync: TSyncType); overload;
procedure  RegisterExceptActionHandler (actionHandler: TExceptActionEventOO; sync: TSyncType); overload;
function UnregisterExceptActionHandler (actionHandler: TExceptActionEvent  ) : boolean; overload;
function UnregisterExceptActionHandler (actionHandler: TExceptActionEventOO) : boolean; overload;

// exceptions which are handled by a try..except block are normally hidden
// these events make them available to you, nevertheless
procedure  RegisterHiddenExceptionHandler (hiddenHandler: TExceptEvent;   sync: TSyncType); overload;
procedure  RegisterHiddenExceptionHandler (hiddenHandler: TExceptEventOO; sync: TSyncType); overload;
function UnregisterHiddenExceptionHandler (hiddenHandler: TExceptEvent  ) : boolean; overload;
function UnregisterHiddenExceptionHandler (hiddenHandler: TExceptEventOO) : boolean; overload;

// ***************************************************************

// this calls our exception handler, you can e.g. call this from a try..except
procedure HandleException (exceptType    : TExceptType   = etNormal;
                           exceptObject  : TObject       = nil;
                           exceptAddr    : pointer       = nil;
                           canContinue   : boolean       = true;
                           currentEsp    : dword         = 0;
                           currentEbp    : dword         = 0;
                           context       : PContext      = nil;
                           source        : TExceptSource = esManual;
                           relatedObject : TObject       = nil;
                           package       : dword         = 0;
                           showReport    : TPString      = nil     );

// ***************************************************************

// when using the option "pause running delphi/bcb threads" all threads which
// are supposed to be suspended get suspended by madExcept in the moment when
// they call DispatchMessageA
// if a thread doesn't call DispatchMessageA, but you want it to be suspended
// during exception handling, you have to call "PauseMeEventually" yourself in that
// code location where your thread should ideally be suspended (e.g. main loop)
procedure PauseMeEventually;

// ***************************************************************

// you can pause madExcept's exception catching powers, if you like
// but please don't call this from multiple threads at the same time
// also please don't turn it on and off again all the time
// it should work ok, but it's not fully thread safe, so be careful
// each PauseMadExcept(true) must have its own PauseMadExcept(false)
procedure PauseMadExcept (pause: boolean = true);

// ***************************************************************
// main thread freeze checking

// with this function you can manually pause & resume freeze checking
// you should pause it for code blocks where the main thread is doing time
// consuming tasks without handling messages to avoid a false frozen alarm
// each PauseFreezeCheck(true) must have its own PauseFreezeCheck(false)
procedure PauseFreezeCheck (pause: boolean = true);

// reset the freeze detection timer
procedure ImNotFrozen;

// set a new freeze timeout time (in seconds)
procedure SetFreezeTimeout (newTimeout: dword);

// initializing the anti freeze check manually makes e.g. sense for NT services
// because the worker thread of a NT service is *not* the main thread
procedure  InitAntiFreeze;
procedure CloseAntiFreeze;

// ***************************************************************
// thread names

// to make the bug report more readable, you can name your secondardy threads
procedure NameThread (threadId: dword; threadName: string);

// ask the name of a specific thread (0 = current thread)
// if the thread has no name, "thread %id%" is returned
function GetThreadName (threadId: dword = 0) : string;

// ***************************************************************
// manually getting bug report information

// creates a full bug report including the callstack of all threads
function CreateBugReport (exceptType        : TExceptType       = etFrozen;
                          exceptObject      : TObject           = nil;
                          exceptAddr        : pointer           = nil;
                          callingThreadId   : dword             = 0;
                          currentEsp        : dword             = 0;
                          currentEbp        : dword             = 0;
                          context           : PContext          = nil;
                          showPleaseWaitBox : boolean           = false;
                          settings          : IMEModuleSettings = nil;
                          source            : TExceptSource     = esManual;
                          relatedObject     : TObject           = nil;
                          package           : dword             = 0;
                          preparedStack     : pointer           = nil     ) : string;

// some functions which return system information
function IsUserAdmin          : boolean;  // does the current user have admin rights?
function GetTSClientName      : string;   // client session name (terminal server)
function GetOsVersionString   : string;   // e.g. "Windows XP Service Pack 1 build 2600"
function GetOsLanguageString  : string;   // e.g. "English"
function GetSystemUpTime      : string;   // e.g. "9 hours 52 minutes"
function GetProgramUpTime     : string;   // e.g. "15 seconds"
function GetCpuName           : string;   // e.g. "Intel(R) Pentium(R) 4 CPU 2.80GHz"
function GetCpuCount          : integer;  // number of installed CPUs (HT-CPUs count as 2)
function GetMemoryStatus      : string;   // e.g. "327/511 MB (free/total)"
function Get9xResourceReport  : string;   // e.g. "99/97 (gdi/user)"
function GetDisplayModeString : string;   // e.g. "1024x768, 32 bit"
function GetAllocatedMemory   : string;   // e.g. "3.52 MB"

// ***************************************************************
// here all the action functionality is exported

// if you have a window, give it in as the parentWindow, otherwise leave it 0
function  SendBugReport (bugReport    : string;
                         screenShot   : INVBitmap;
                         parentWindow : dword       = 0;
                         settings     : IMESettings = nil) : TExtBool;
function  SaveBugReport (bugReport    : string;
                         parentWindow : dword       = 0;
                         settings     : IMESettings = nil) : boolean;
function PrintBugReport (bugReport    : string;
                         parentWindow : dword       = 0;
                         settings     : IMESettings = nil) : boolean;

procedure RestartApplication;
procedure CloseApplication;

// this is for secretly saving/sending the bug report (without UI)
function AutoSendBugReport (bugReport  : string;
                            screenShot : INVBitmap;
                            settings   : IMESettings = nil) : TExtBool;
function AutoSaveBugReport (bugReport  : string;
                            settings   : IMESettings = nil) : boolean;

// ***************************************************************
// additional tool functions

// are we online right now?
function AmOnline : boolean;

// which mx server is responsible for the specified email address?
function MxLookup (email: string) : string;

// setup an IMEAttachments interface
// can be used in SendMapiMail, SendSmtpMail and HttpUpload
function NewAttachments : IMEAttachments;

// setup an IMEFields interface
// can be used in HttpUpload
function NewFields : IMEFields;

// madExcept knows three different ways to send a mail
function SendMapiMail  (rcptTo        : string;
                        subject, body : string;
                        attachments   : IMEAttachments = nil;
                        parentWindow  : dword          = 0;
                        hidden        : boolean        = false;
                        background    : boolean        = false;
                        settings      : IMESettings    = nil  ) : TExtBool;
function SendShellMail (rcptTo        : string;
                        subject, body : string                ) : boolean;
function SendSmtpMail  (mailFrom      : string;
                        rcptTo        : string;
                        subject, body : string;
                        attachments   : IMEAttachments = nil;
                        server        : string         = '';
                        authUserName  : string         = '';
                        authPassword  : string         = '';
                        port          : dword          = 25;
                        parentWindow  : dword          = 0;
                        hidden        : boolean        = false;
                        background    : boolean        = false;
                        settings      : IMESettings    = nil  ) : boolean;

// madExcept can also upload fields and attachments to a HTTP website
function HttpUpload    (httpUrl       : string;
                        attachments   : IMEAttachments;
                        fields        : IMEFields      = nil;
                        authUserName  : string         = '';
                        authPassword  : string         = '';
                        port          : dword          = 80;
                        mailFrom      : string         = '';
                        parentWindow  : dword          = 0;
                        hidden        : boolean        = false;
                        background    : boolean        = false;
                        settings      : IMESettings    = nil  ) : boolean;

// ***************************************************************
// bug report plugins

type
  TBugReportPlugin   = function : string;
  TBugReportPluginEx = function (const exceptIntf: IMEException) : string;

// call this from a the "Register" function of a design time package
// the madExcept wizard will then offer the programmer the possibility to
// enable the plugin in the madExcept settings dialog
// Example:
// RegisterBugReportPlugin('hardware', 'list of installed hardware', GetHardwareList);
procedure RegisterBugReportPlugin (name        : string;
                                   description : string;
                                   proc        : TBugReportPlugin;
                                   ownSection  : boolean = true    ); overload;
procedure RegisterBugReportPlugin (name        : string;
                                   description : string;
                                   proc        : TBugReportPluginEx;
                                   ownSection  : boolean = true    ); overload;

// please call this in the finalization of your plugin unit
procedure UnregisterBugReportPlugin (name: string);

// ***************************************************************

var
  // id of the process' main thread (filled in "madExcept.initialization")
  // in dlls this can eventually be different to "System.MainThreadId"
  ProcessMainThreadId : dword = 0;

// ***************************************************************
// undocumented options, accessible at runtime only

var
  AddCmdLineToBugRep  : boolean = true;   // false: don't add the command line to the bug report
  DetectConsole       : boolean = true;   // if the app is a console, the bug report is shown there
  ResetFpuMode        : boolean = false;  // enable FPU exceptions after each exception?
  DumbStackTrace      : boolean = false;  // use dumb stack tracing (= faster, but more noise)
  HttpUploadTimeout   : integer = 7;      // web server upload timeout value (default 7 seconds)
  ShowBugReportKey    : integer = 0;      // press Ctrl+Alt+Shift + xxx to show the bug report
                                          // can be e.g. ord('S') or VK_F1

// ***************************************************************
// please don't use, for internal purposes only

procedure CheckExceptParams(var exceptObject: TObject; var exceptAddr: pointer;
                            var preparedStack: pointer; var context: PContext);
procedure InstallUnhandledExceptionFilter;
procedure FillClipboard(str: string);
procedure HandleContactForm(form: INVForm; action: TNVAction; item: INVItem; exceptIntf: IMEException);
procedure HandleScreenshotForm(form: INVForm; action: TNVAction; item: INVItem; exceptIntf: IMEException);
procedure HyperJumpCallstack (callstackItem: string);
function PatchJmp(aim, new_: pointer) : boolean;
function PatchInt(aim: pointer; new_: integer) : boolean;
procedure ExpandVars(module: dword; var str: string; exceptMsg, bugReport: string);
function FormatExceptMessage(msg: string) : string;
function madTraceProcess(mapSize: integer) : integer; stdcall;
function DecryptPassword(password, account, server: string) : string;
function RegReadStr(key: dword; path: string; name: string = '') : string;
function Esp : dword;
function Ebp : dword;
function DefaultBugReportHtml(bugReport: string; settings: IMESettings) : string;
procedure CalibrateCode;
var Plugins        : array of record name, description: string; procOld: TBugReportPlugin; procNew: TBugReportPluginEx; ownSection: boolean end;
    CalibrateData  : dword = 0;
    AmHttpServer   : boolean = false;
    BugReportHtml  : function (bugReport: string; settings: IMESettings) : string = DefaultBugReportHtml;
    DisAsmFunc     : function (func: pointer; var disAsm: string; exceptAddr: pointer;
                               maxLines: integer; autoDelimiters: boolean) : TFunctionInfo = nil;

var
  // these variables are initialized by the IDE wizard
  Forms_TApplication_HandleException              : pointer   = nil;
  Forms_TApplication_ShowException                : pointer   = nil;
  Qforms_TApplication_HandleException             : pointer   = nil;
  Qforms_TApplication_ShowException               : pointer   = nil;
  SysUtils_ShowException                          : pointer   = nil;
  SysUtils_LoadPackage                            : pointer   = nil;
  SysUtils_InitializePackage                      : procedure (module, dummy: dword) = nil;
  Classes_CheckSynchronize                        : pointer   = nil;
  CGIApp_TCGIApplication_CGIHandleException       : pointer   = nil;
  ISAPIApp_TISAPIApplication_ISAPIHandleException : pointer   = nil;
  HttpExtensionProcNext                           : function (ecb: pointer) : dword stdcall = nil;
  System_InitUnits                                : procedure = nil;
  System_FinalizeUnits                            : procedure = nil;
  System_ExceptionHandler                         : pointer   = nil;
  System_runErrMsg                                : pointer   = nil;
  {$ifdef bcb}
    BcbExceptionHandler                             : pointer   = nil;
    BcbThrowExceptionLDTC                           : pointer   = nil;
    BcbInitExceptBlockLDTC                          : pointer   = nil;
    BcbOrgMalloc                                    : pointer   = nil;
    BcbMemcpy                                       : pointer   = nil;
    BcbCallTerminate                                : pointer   = nil;
    BcbPTerminate                                   : TPPointer = nil;
  {$endif}

// ***************************************************************

implementation

uses Messages, ActiveX, CommCtrl, CommDlg, ShellAPI, SysConst, WinSock,
     madMapFile, madStrings, madTools, madCrypt, madNVPrgrAlert, madZip;

// ***************************************************************

// needed to find code section (UPX/Shrinker support)
procedure CalibrateCode; begin end;

// ***************************************************************

{$ifdef log}
var indentStr : string;
procedure   indentLog; begin indentStr := indentStr + '  ' end;
procedure unindentLog; begin Delete(indentStr, 1, 2)       end;

procedure log(str: string);
var c1, c2 : dword;
    st     : TSystemTime;
    time   : string;
begin
  c1 := CreateFile('c:\madExcept.txt', GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_ALWAYS, 0, 0);
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
  end;
end;
{$endif}

// ***************************************************************

var
  StartTime : int64;            // at which time got madExcept initialized?
  AmMeBpl   : boolean = false;  // are we madExcept_.bpl or another bpl containing madExcept.pas?

const
  cDelphiException = $0EEDFADE;
  cCppException    = $0EEFFACE;

type
  // internal type to get access to the current ExceptAddr and ExceptObject
  TRaiseFrame = record
    NextRaise       : pointer;
    ExceptAddr      : pointer;
    ExceptObject    : TObject;
    ExceptionRecord : PExceptionRecord;
  end;

type
  // registered exception handler storage
  TDAExceptHandler   = array of record sync: TSyncType; phase: TExceptPhase; handler: TExceptEvent;         end;
  TDAExceptHandlerOO = array of record sync: TSyncType; phase: TExceptPhase; handler: TExceptEventOO;       end;
  TDAActionHandler   = array of record sync: TSyncType; phase: TExceptPhase; handler: TExceptActionEvent;   end;
  TDAActionHandlerOO = array of record sync: TSyncType; phase: TExceptPhase; handler: TExceptActionEventOO; end;

var
  HandlerSection   : PRTLCriticalSection = nil;
  ExceptHandlers   : TDAExceptHandler;
  ExceptHandlersOO : TDAExceptHandlerOO;
  ActionHandlers   : TDAActionHandler;
  ActionHandlersOO : TDAActionHandlerOO;
  HiddenHandlers   : TDAExceptHandler;
  HiddenHandlersOO : TDAExceptHandlerOO;

type
  // process global buffer used by madExcept
  TPThreadName = ^TThreadName;
  TThreadName = record
    id          : dword;
    name        : pchar;
    next        : TPThreadName;
    class_      : pchar;
    creatorAddr : pointer;
    creatorTid  : dword;
    handle      : dword;
  end;
  TMadExceptBuf = packed record
    antiFreezeMutex  : dword;
    threadNamesMutex : dword;
    threadNamesList  : TPThreadName;
    referenceCount   : integer;
    mapHandle        : dword;
  end;

var
  madExceptBuf : ^TMadExceptBuf = nil;

type
  // extended exception access
  IMEExceptionEx = interface ['{0BCABCF1-B70F-4952-B907-A129A9BD19FD}']
    function  CriticalBugReportCallbackExists : boolean;
    function  GetCompleteThreadId : dword;
    procedure SetPhase (value: TExceptPhase);
    function  GetMailWasSent : boolean;
    procedure SetMailWasSent (value: boolean);
    function  GetMailWasSaved : boolean;
    procedure SetMailWasSaved (value: boolean);
    function  FirstCompleteCallback : boolean;
    function  FirstAutoSave : boolean;
    function  PSendThread : TPCardinal;
  end;

// ***************************************************************
// some thread functions, a lot of undocumented stuff

function GetThreadList : TDACardinal;
// get a list of thread ids (the NT part is undocumented)
type TThreadEntry32 = record
       size          : cardinal;
       d1            : cardinal;
       thread        : cardinal;
       ownerProcess  : cardinal;
       d2            : array [4..6] of integer;
     end;
     TNtProcessInfo = record
       offset     : cardinal;
       numThreads : cardinal;
       d1         : array [2..16] of cardinal;
       pid        : cardinal;
       d3         : array [18..42] of cardinal;
       threads    : array [0..maxInt shr 7 - 1] of record
                      tidNt4 : cardinal;
                      d5     : array [44..54] of cardinal;
                      tidNt5 : cardinal;
                      d6     : array [56..58] of cardinal;
                    end;
     end;
var
  Thread32First             : function (snap: cardinal; var te: TThreadEntry32) : LongBool; stdcall;
  Thread32Next              : function (snap: cardinal; var te: TThreadEntry32) : LongBool; stdcall;
  CreateToolhelp32Snapshot  : function (flags, processId: cardinal ) : cardinal; stdcall;
  NtQuerySystemInformation  : function (infoClass: cardinal; buffer: pointer; bufSize: cardinal;
                                        returnSize: TPCardinal) : cardinal; stdcall;
var c1, c2 : cardinal;
    te     : TThreadEntry32;
    p1     : pointer;
    npi    : ^TNtProcessInfo;
    i1     : integer;
begin
  result := nil;
  if OS.win9x then begin
    CreateToolhelp32Snapshot := GetProcAddress(GetModuleHandle(kernel32), 'CreateToolhelp32Snapshot');
    Thread32First            := GetProcAddress(GetModuleHandle(kernel32), 'Thread32First'           );
    Thread32Next             := GetProcAddress(GetModuleHandle(kernel32), 'Thread32Next'            );
    c1 := CreateToolHelp32Snapshot(4, 0);  // TH32CS_SnapThread = 4
    if c1 <> INVALID_HANDLE_VALUE then
      try
        te.size := sizeOf(TThreadEntry32);
        if Thread32First(c1, te) then
          repeat
            if te.ownerProcess = GetCurrentProcessId then begin
              SetLength(result, Length(result) + 1);
              result[high(result)] := te.thread;
            end;
          until not Thread32Next(c1, te);
      finally CloseHandle(c1) end;
  end else begin
    NtQuerySystemInformation := GetProcAddress(GetModuleHandle('ntdll.dll'), 'NtQuerySystemInformation');
    c1 := 0;
    NtQuerySystemInformation(5, nil, 0, @c1);
    p1 := nil;
    try
      if c1 = 0 then begin
        c1 := $10000;
        repeat
          c1 := c1 * 2;
          LocalFree(dword(p1));
          dword(p1) := LocalAlloc(LPTR, c1);
          c2 := NtQuerySystemInformation(5, p1, c1, nil);
        until (c2 = 0) or (c1 = $400000);
      end else begin
        c1 := c1 * 2;
        dword(p1) := LocalAlloc(LPTR, c1);
        c2 := NtQuerySystemInformation(5, p1, c1, nil);
      end;
      if c2 = 0 then begin
        npi := p1;
        while true do begin
          if npi^.pid = GetCurrentProcessId then
            for i1 := 0 to npi^.numThreads - 1 do begin
              if OS.winNtEnum > osWinNt4 then
                   c1 := npi^.threads[i1].tidNt5
              else c1 := npi^.threads[i1].tidNt4;
              SetLength(result, Length(result) + 1);
              result[high(result)] := c1;
            end;
          if npi^.offset = 0 then break;
          npi := pointer(cardinal(npi) + npi^.offset);
        end;
      end;
    finally LocalFree(dword(p1)) end;
  end;
end;

var
  OpenThreadProc   : function (access: cardinal; inheritHandle: bool;
                               threadId: cardinal) : cardinal; stdcall = nil;
  NtOpenThreadProc : function (var hThread: cardinal; access: cardinal;
                               objAttr, clientId: pointer) : cardinal; stdcall = nil;

// this opens a thread in any OS (completely undocumented!)
function OpenThread(tid, access: dword) : dword;
type T9xHandleItem = packed record
       access    : cardinal;
       objAddr   : pointer;
     end;
     T9xHandleTable = packed record
       itemCount : integer;
       items     : array [0..maxInt shr 3 - 1] of T9xHandleItem;
     end;
const CNtObjAttr : array [0..5] of cardinal = ($18, 0, 0, 0, 0, 0);
var clientId     : array [0..1] of cardinal;
    c1, c2, c3   : integer;
    oldObjAddr   : pointer;
begin
  result := 0;
  try
    if @OpenThreadProc <> nil then begin
      result := OpenThreadProc(access, true, tid);
    end else
      if OS.winNT then begin
        if @NtOpenThreadProc <> nil then begin
          clientId[0] := 0;
          clientId[1] := tid;
          if NtOpenThreadProc(result, access, @CNtObjAttr, @clientId) <> 0 then
            result := 0;
        end;
      end else begin
        c1 := GetCurrentProcess;
        if DuplicateHandle(c1, c1, c1, @c2, 0, false, DUPLICATE_SAME_ACCESS) then
          try
            if Magic95 then c3 := c2
            else            c3 := c2 div 4;
            with T9xHandleTable(TPAPointer(GetCurrentProcessId xor Magic)^[17]^).items[c3] do begin
              oldObjAddr := objAddr;
              objAddr := pointer(tid xor Magic);
            end;
            if not DuplicateHandle(c1, c2, c1, @result, access, false, 0) then
              result := 0;
            T9xHandleTable(TPAPointer(GetCurrentProcessId xor Magic)^[17]^).items[c3].objAddr := oldObjAddr;
          finally CloseHandle(c2) end;
      end;
  except end;
end;

// ***************************************************************
// some general purpose helper functions, don't wanna import SysUtils

function ModuleName(module: dword) : string;
var arrCh : array [0..MAX_PATH] of char;
begin
  GetModuleFileName(module, arrCh, MAX_PATH);
  result := arrCh;
end;

function ExtractFileName(str: string) : string;
var i1 : integer;
begin
  result := str;
  for i1 := Length(result) downto 1 do
    if result[i1] = '\' then begin
      Delete(result, 1, i1);
      break;
    end;
end;

function ExtractFilePath(str: string) : string;
var i1 : integer;
begin
  result := str;
  for i1 := Length(result) downto 1 do
    if result[i1] = '\' then begin
      Delete(result, i1 + 1, maxInt);
      break;
    end;
end;

function KillExt(str: string) : string;
var i1 : integer;
begin
  result := str;
  for i1 := Length(result) downto 1 do
    if result[i1] = '.' then begin
      Delete(result, i1, maxInt);
      break;
    end;
end;

function GetTempPath : string;
var arrCh : array [0..MAX_PATH] of char;
begin
  if windows.GetTempPath(MAX_PATH, arrCh) > 0 then begin
    result := arrCh;
    if result <> '' then begin
      CreateDirectory(pchar(result), nil);
      if result[Length(result)] <> '\' then
        result := result + '\';
      result := result + KillExt(ExtractFileName(ModuleName(0))) + '.madExcept';
      CreateDirectory(pchar(result), nil);
      result := result + '\';
    end;
  end else
    result := '';
end;

procedure ClearTempPath;
var tp  : string;
    wfd : TWin32FindData;
    fh  : dword;
begin
  tp := GetTempPath;
  if (tp <> '') and (GetFileAttributes(pchar(tp)) <> dword(-1)) then begin
    fh := FindFirstFile(pchar(tp + '*.*'), wfd);
    if fh <> INVALID_HANDLE_VALUE then begin
      repeat
        DeleteFile(pchar(tp + wfd.cFileName));
      until not FindNextFile(fh, wfd);
      windows.FindClose(fh);
    end;
    RemoveDirectory(pchar(tp));
  end;
end;

function CompName : string;
var c1    : dword;
    arrCh : array [0..MAX_PATH] of char;
begin
  c1 := MAX_PATH;
  GetComputerName(arrCh, c1);
  result := arrCh;
end;

function RegReadStr(key: dword; path: string; name: string = '') : string;
var key2   : HKEY;
    c1, c2 : dword;
    p1     : pchar;
begin
  result := '';
  if RegOpenKeyEx(key, pchar(path), 0, KEY_READ, key2) = 0 then begin
    c1 := 0;
    RegQueryValueEx(key2, pchar(name), nil, nil, nil, @c1);
    if c1 <> 0 then begin
      c1 := c1 * 2;
      p1 := pointer(LocalAlloc(LPTR, c1));
      if RegQueryValueEx(key2, pchar(name), nil, @c2, pointer(p1), @c1) = 0 then
        if c2 = REG_SZ then
             result := p1
        else SetString(result, p1, c1);
      LocalFree(dword(p1));
    end;
    RegCloseKey(key2);
  end;
end;

function RegWriteStr(key: dword; path, name, value: string) : boolean;
var key2 : HKEY;
    c1   : dword;
begin
  if RegCreateKeyEx(key, pchar(path), 0, nil, 0, KEY_ALL_ACCESS, nil, key2, @c1) = 0 then begin
    result := RegSetValueEx(key2, pchar(name), 0, REG_SZ, pchar(value), Length(value) + 1) = 0;
    RegCloseKey(key2);
  end else
    result := false;
end;

function RegDelVal(key: dword; path: string; name: string = '') : boolean;
var key2 : HKEY;
begin
  if RegOpenKeyEx(key, pchar(path), 0, KEY_ALL_ACCESS, key2) = 0 then begin
    result := RegDeleteValue(key2, pchar(name)) = 0;
    RegCloseKey(key2);
  end else
    result := false;
end;

function GetSeconds : int64;
var st : TSystemTime;
    ft : TFileTime;
begin
  GetSystemTime(st);
  SystemTimeToFileTime(st, ft);
  result := int64(ft) div 10000000;
end;

procedure InitSecAttr(var sa: TSecurityAttributes; var sd: TSecurityDescriptor);
begin
  sa.nLength := sizeOf(sa);
  sa.lpSecurityDescriptor := @sd;
  sa.bInheritHandle := false;
  InitializeSecurityDescriptor(@sd, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@sd, true, nil, false);
end;

// ***************************************************************
// some module functions

type
  TModule = record
    handle   : dword;
    fileName : string;
  end;
  TDAModule = array of TModule;

// returns all modules of the current process
function GetModuleList : TDAModule;
var p1, p2 : pointer;
    mbi    : TMemoryBasicInformation;
    arrCh  : array [0..MAX_PATH] of char;
    i1     : integer;
begin
  SetLength(result, 10);
  i1 := 0;
  p1 := nil;
  p2 := nil;
  while VirtualQuery(p1, mbi, sizeOf(mbi)) = sizeOf(mbi) do begin
    if (mbi.State = MEM_COMMIT) and
       (mbi.AllocationBase <> p2) and (mbi.AllocationBase = mbi.BaseAddress) and
       (GetModuleFileName(dword(mbi.AllocationBase), arrCh, MAX_PATH) > 0) then begin
      if i1 = Length(result) then
        SetLength(result, i1 * 2);
      with result[i1] do begin
        handle   := dword(mbi.AllocationBase);
        fileName := ExtractFileName(arrCh);
      end;
      inc(i1);
    end;
    p2 := mbi.AllocationBase;
    dword(p1) := dword(p1) + mbi.RegionSize;
  end;
  SetLength(result, i1);
end;

procedure PatchImportTable(module: dword; old, new: pointer);

  procedure InvalidImportTable;
  begin
    if (module = HInstance) and (DebugHook <> 0) then
      MessageBox(0, 'The import table is invalid.' + #$D#$A + #$D#$A +
                    'This way madExcept can''t install the thread hooks.',
                 pchar(ExtractFileName(ModuleName(HInstance))), 0);
  end;

var pinh : PImageNtHeaders;
    pid  : ^TImageImportDirectory;
    p1   : TPPointer;
    c1   : dword;
    eis  : dword;  // end of import section
begin
  pinh := GetImageNtHeaders(module);
  if pinh <> nil then begin
    with pinh^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT] do begin
      pid := pointer(module + VirtualAddress);
      eis := VirtualAddress + Size;
    end;
    if pid <> nil then
      while pid^.Name_ <> 0 do begin
        if pid^.ThunkArray > eis then begin
          InvalidImportTable;
          break;
        end;
        if pid^.ThunkArray <> 0 then begin
          p1 := pointer(module + pid^.ThunkArray);
          while p1^ <> nil do begin
            if (p1^ = old) and VirtualProtect(p1, 4, PAGE_EXECUTE_READWRITE, @c1) then begin
              p1^ := new;
              VirtualProtect(p1, 4, c1, @c1);
            end;
            inc(p1);
          end;
        end;
        inc(pid);
      end;
  end;
end;

procedure PatchImportTables(const modules: TDAModule; old, new: pointer);
var i1 : integer;
begin
  for i1 := 0 to high(modules) do
    if OS.winNt or (modules[i1].handle < $80000000) then
      PatchImportTable(modules[i1].handle, old, new);
end;

// ***************************************************************
// some madExcept specific tool functions

procedure SetThreadInfo(threadId, threadHandle: dword; threadName, threadClass: string;
                        threadCreator: pointer; creatorTid: dword);
var pptn : ^TPThreadName;
    ptn  : TPThreadName;
    new  : boolean;
    th   : dword;
begin
  if madExceptBuf <> nil then begin
    WaitForSingleObject(madExceptBuf^.threadNamesMutex, INFINITE);
    try
      new := (threadName <> '') or (threadCreator <> nil);
      pptn := @madExceptBuf^.threadNamesList;
      while (pptn^ <> nil) and (pptn^^.id <> threadId) do
        pptn := @pptn^^.next;
      th := 0;
      if pptn^ <> nil then begin
        ptn := pptn^;
        pptn^ := pptn^^.next;
        if new then begin
          if threadName = '' then threadName := ptn^.name;
          if LocalSize(dword(ptn)) > 12 then begin
            if threadClass   = ''  then threadClass   := ptn^.class_;
            if threadCreator = nil then threadCreator := ptn^.creatorAddr;
            if creatorTid    = 0   then creatorTid    := ptn^.creatorTid;
          end;
        end;
        if LocalSize(dword(ptn)) > 24 then
          th := ptn^.handle;
        LocalFree(dword(ptn^.name));
        if LocalSize(dword(ptn)) > 12 then
          LocalFree(dword(ptn^.class_));
        LocalFree(dword(ptn));
      end;
      if new then begin
        if th = 0 then
          if threadHandle = 0 then
               th := OpenThread(threadId, SYNCHRONIZE or $40)
          else DuplicateHandle(GetCurrentProcess, threadHandle, GetCurrentProcess, @th, SYNCHRONIZE or $40, false, 0);
        if th <> 0 then begin
          ptn := pointer(LocalAlloc(LPTR, sizeOf(TThreadName)));
          ptn^.id := threadId;
          ptn^.handle := th;
          dword(ptn^.name  ) := LocalAlloc(LPTR, Length(threadName ) + 1);
          dword(ptn^.class_) := LocalAlloc(LPTR, Length(threadClass) + 1);
          Move(pchar(threadName )^, ptn^.name^,   Length(threadName ) + 1);
          Move(pchar(threadClass)^, ptn^.class_^, Length(threadClass) + 1);
          ptn^.creatorAddr := threadCreator;
          ptn^.creatorTid  := creatorTid;
          ptn^.next := madExceptBuf^.threadNamesList;
          madExceptBuf^.threadNamesList := ptn;
        end;
      end else
        if th <> 0 then
          CloseHandle(th);
      // check all threads, remove all which are already terminated
      ptn := madExceptBuf^.threadNamesList;
      while ptn <> nil do begin
        if (LocalSize(dword(ptn)) > 24) and (WaitForSingleObject(ptn^.handle, 0) = WAIT_OBJECT_0) then begin
          SetThreadInfo(ptn^.id, 0, '', '', nil, 0);
          break;
        end;
        ptn := ptn^.next;
      end;
    finally ReleaseMutex(madExceptBuf^.threadNamesMutex) end;
  end;
end;

type
  TThreadNameInfo = record
    FType     : LongWord;  // has to be 0x1000
    FName     : PChar;     // thread name
    FThreadId : LongWord;  // thread ID ("-1" indicates caller thread)
    FFlags    : LongWord;  // reserved, must be 0
  end;

procedure NameThread(threadId: dword; threadName: string);
{$ifdef d7} var tni : TThreadNameInfo; {$endif}
begin
  SetThreadInfo(threadID, 0, threadName, '', nil, 0);
  {$ifdef d7}
    if (threadName <> '') and (DebugHook <> 0) then begin
      tni.FType     := $1000;
      tni.FName     := pchar(threadName);
      tni.FThreadID := threadID;
      tni.FFlags    := 0;
      try
        RaiseException($406D1388, 0, 4, @tni);
      except end;
    end;
  {$endif}
end;

function GetThreadInfoStr(tid: dword; forceShow: boolean;
                          var creatorAddr: pointer; var creatorTid: dword) : string;
// get/create the name of the specified thread
var ptn : TPThreadName;
begin
  if (madExceptBuf <> nil) and
     (WaitForSingleObject(madExceptBuf^.threadNamesMutex, 50) = WAIT_OBJECT_0) then begin
    try
      result := '';
      creatorAddr := nil;
      ptn := madExceptBuf^.threadNamesList;
      while (ptn <> nil) and (ptn^.id <> tid) do
        ptn := ptn^.next;
      if ptn <> nil then begin
        if ptn^.name^ <> #0 then
          result := ptn^.name;
        if LocalSize(dword(ptn)) > 12 then
          try
            creatorAddr := ptn^.creatorAddr;
            creatorTid  := ptn^.creatorTid;
          except end;
      end;
      if forceShow or (result <> '-') then begin
        if (result = '') or (result = '-') then begin
          result := 'thread ' + IntToHexEx(tid);
          try
            if (ptn <> nil) and (LocalSize(dword(ptn)) > 12) and (ptn^.class_^ <> #0) then
              result := result + ' (' + ptn^.class_ + ')';
          except end;
        end else result := result + ' (' + IntToHexEx(tid) + ')';
      end else result := '';
    finally ReleaseMutex(madExceptBuf^.threadNamesMutex) end;
  end else begin
    result      := 'thread ' + IntToHexEx(tid);
    creatorAddr := nil;
  end;
end;

function GetThreadName(threadId: dword = 0) : string;
var addr    : pointer;
    creator : dword;
begin
  if threadId = 0 then
    threadId := GetCurrentThreadId;
  result := GetThreadInfoStr(threadId, true, addr, creator);
end;

function IsProcessBlocked(wait: integer = 0) : boolean;

  function GetTebIndex(index: dword) : dword;
  asm
    mov ecx, fs:[$18]
    mov ecx, [ecx + $30]
    add ecx, index
    mov eax, [ecx]
  end;

  function IsSectionLocked(section: dword) : boolean;
  begin
    result := (section <> 0) and (TPInteger(section + 4)^ > -1);
  end;

begin
  repeat
    if GetVersion and $80000000 = 0 then
      result := IsSectionLocked(GetTebIndex($a0)) or  // loader lock -> initialization
                IsSectionLocked(GetTebIndex($1c))     //    peb lock -> finalization
    else
      result := PInt(GetCurrentProcessId xor Magic + $64)^ <> 0;  // 9x thread section
    if not result then
      break;
    if wait <= 0 then
      break;
    dec(wait, 10);
    Sleep(10);
  until false;
end;

function IsValidIdent(const Ident: string) : boolean;
var i1 : integer;
begin
  result := (Ident <> '') and (Ident[1] in ['A'..'Z', 'a'..'z', '_']);
  if result then
    for i1 := 2 to Length(Ident) do
      if not (Ident[i1] in ['A'..'Z', 'a'..'z', '_', '0'..'9']) then begin
        result := false;
        break;
      end;
end;

function IsValidObject(obj: TObject) : boolean;

  function InRangeOrZero(value, min, max: dword) : boolean;
  begin
    result := (value = 0) or ((value >= min) and (value <= max));
  end;

var module    : dword;
    NTHeader  : PImageNtHeaders;
    codeBegin : dword;
    codeEnd   : dword;
    c1, c2    : dword;
    buf       : dword;
    s1        : string;
    sh        : PImageSectionHeader;
begin
  result := false;
  try
    buf := dword(obj.ClassType) - dword(-vmtSelfPtr);
    if FindModule(pointer(buf), module, s1) then begin
      NTHeader := GetImageNtHeaders(module);
      if NTHeader <> nil then begin
        dword(sh) := dword(NTHeader) + sizeOf(NTHeader^);
        if sh^.Characteristics and IMAGE_SCN_CNT_CODE <> 0 then begin
          codeBegin := module    + sh^.VirtualAddress;
          codeEnd   := codeBegin + sh^.Misc.VirtualSize - 3;
          inc(sh);
          if sh^.Characteristics and IMAGE_SCN_CNT_CODE <> 0 then
            codeEnd := module + sh^.VirtualAddress + sh^.Misc.VirtualSize - 3;
        end else begin
          codeBegin := module    + NTHeader.OptionalHeader.BaseOfCode;
          codeEnd   := codeBegin + NTHeader.OptionalHeader.SizeOfCode - 3;
        end;
        c1 := TPCardinal(buf)^;
        if c1 >= module + NTHeader.OptionalHeader.BaseOfData then
          // BCB likes to store its own classes in the data section
          codeEnd := module +
                     NTHeader.OptionalHeader.BaseOfData +
                     NTHeader.OptionalHeader.SizeOfInitializedData - 3;
        if (c1 = buf + dword(-vmtSelfPtr)) and
           InRangeOrZero(TPCardinal(c1 - dword(-vmtClassName   ))^, c1,        codeEnd) and
           InRangeOrZero(TPCardinal(c1 - dword(-vmtDynamicTable))^, c1,        codeEnd) and
           InRangeOrZero(TPCardinal(c1 - dword(-vmtMethodTable ))^, c1,        codeEnd) and
           InRangeOrZero(TPCardinal(c1 - dword(-vmtFieldTable  ))^, c1,        codeEnd) and
           InRangeOrZero(TPCardinal(c1 - dword(-vmtTypeInfo    ))^, codeBegin, codeEnd) and
           InRangeOrZero(TPCardinal(c1 - dword(-vmtInitTable   ))^, codeBegin, codeEnd) and
           InRangeOrZero(TPCardinal(c1 - dword(-vmtAutoTable   ))^, codeBegin, codeEnd) and
           InRangeOrZero(TPCardinal(c1 - dword(-vmtIntfTable   ))^, codeBegin, codeEnd) then begin
          s1 := ShortString(pointer(TPPointer(c1 - dword(-vmtClassName))^)^);
          c2 := TPCardinal(c1 - dword(-vmtTypeInfo))^;
          if IsValidIdent(s1) and ((c2 = 0) or (ShortString(pointer(c2 + 1)^) = s1)) then
            result := true;
        end;
      end;
    end;
  except end;
end;

var System_FpuInit : procedure = nil;
procedure _FpuInit;
begin
  if @System_FpuInit <> nil then
    System_FpuInit;
end;

var Last10Exceptions : array [0..9] of int64 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
function CheckAutoBtn(auto: dword) : boolean;
var i64 : int64;
    i1  : integer;
begin
  result := auto <> 0;
  if auto > 1 then begin
    i64 := GetSeconds;
    for i1 := 9 downto 0 do
      if (Last10Exceptions[i1] = 0) or (i64 - Last10Exceptions[i1] > auto) then begin
        result := false;
        break;
      end;
  end;
end;

procedure ExpandVars(module: dword; var str: string; exceptMsg, bugReport: string);

  procedure ReplaceVersion(modStr: string; module: dword);
  var s1  : string;
      i64 : int64;
  begin
    if PosText('%' + modStr + 'version%', str) > 0 then begin
      s1 := MESettings(module).VersionVar;
      if s1 = '' then begin
        i64 := GetFileVersion(ModuleName(module));
        if i64 <> 0 then
          s1 := FileVersionToStr(i64)
        else
          s1 := '?';
      end;
      ReplaceText(str, '%' + modStr + 'version%', s1);
    end;
  end;

var sysTime : TSystemTime;
    date, time, datetime : string;
begin
  ReplaceText(str, '%appname%', ExtractFileName(ModuleName(0     )));
  ReplaceText(str, '%modname%', ExtractFileName(ModuleName(module)));
  ReplaceVersion('app', GetModuleHandle(nil));
  ReplaceVersion('mod', module);
  if exceptMsg = '' then
    exceptMsg := 'Unknown';
  ReplaceText(str, '%exceptMsg%', exceptMsg);
  ReplaceText(str, '%bugReport%', bugReport);
  ReplaceText(str, '%LF%', #$D#$A);
  GetLocalTime(sysTime);
  date := IntToStrEx(dword(sysTime.wYear        ), 4) + '-'  +
          IntToStrEx(dword(sysTime.wMonth       ), 2) + '-'  +
          IntToStrEx(dword(sysTime.wDay         ), 2);
  time := IntToStrEx(dword(sysTime.wHour        ), 2) + ':'  +
          IntToStrEx(dword(sysTime.wMinute      ), 2) + ':'  +
          IntToStrEx(dword(sysTime.wSecond      ), 2) + ', ' +
          IntToStrEx(dword(sysTime.wMilliseconds), 1) + 'ms';
  dateTime := date + ', ' + time;
  ReplaceText(str, '%date%',     date);
  ReplaceText(str, '%time%',     time);
  ReplaceText(str, '%datetime%', dateTime);
end;

function IsClass(obj: TObject; className: string) : boolean;
var clss : TClass;
begin
  result := false;
  try
    if obj <> nil then begin
      clss := obj.ClassType;
      while (clss <> nil) and (clss.ClassName <> className) do
        clss := clss.ClassParent;
      result := clss <> nil;
    end else
      result := false;
  except end;
end;

function GetExceptClass_(exceptObject: TObject) : string;
begin
  result := '';
  if exceptObject <> nil then
    try
      result := exceptObject.ClassName;
    except end;
  if result = '' then
    result := 'Unknown';
end;

function GetExceptMessage_(exceptObject: TObject) : string;
begin
  result := '';
  if exceptObject <> nil then
    try
      if IsClass(exceptObject, 'MadException') or IsClass(exceptObject, 'Exception') then
        result := MadException(exceptObject).Message
      else
        result := exceptObject.ClassName;
    except end;
  KillChars(result, [#0..#31] - [#$D, #$A]);
  TrimStr(result);
  if result = '' then
    result := 'Unknown';
end;

function FormatExceptMessage(msg: string) : string;
begin
  result := msg;
  Delete(result, PosChars([#1..#255] - ['.', '!'], result, maxInt, 1) + 1, maxInt);
  result := result + '.';
end;

procedure FillClipboard(str: string);
// fill the clipboard with a specific string
var mem : dword;
    buf : pointer;
begin
  if OpenClipboard(0) then begin
    // w2k needs this, the other OSs don't...  :-(
    EmptyClipboard;
    CloseClipboard;
  end;
  if OpenClipboard(0) then begin
    mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, Length(str) + 1);
    buf := GlobalLock(mem);
    if buf <> nil then begin
      Move(pchar(str)^, buf^, Length(str) + 1);
      GlobalUnlock(mem);
      SetClipboardData(CF_TEXT, mem);
    end;
    CloseClipboard;
  end;
end;

// ***************************************************************
// additional stuff for BCB only

{$ifdef bcb}

const
  // used for validity checking
  CBcbXorCheck : dword = $9BDF9BDF;
var
  // at which TLS offset is the BCB exception list stored?
  BcbExceptTlsOffset : dword = 0;

  // how can we get the BCB TLS root?
  BcbGetTls : function : dword = nil;

  // original BCB functions, stored for hooking
  BcbMallocNext             : function (size: dword) : pointer; cdecl;
  BcbGetExceptionObjectNext : function (var er: TExceptionRecord; d1, d2, d3: dword) : pointer; cdecl;
  BcbRaiseExceptionNext     : procedure (code, flags, argCount: dword; args: TPACardinal); stdcall;
  BcbInitCatchArgPascalNext : procedure (d1, d2: dword; var er: TExceptionRecord; d3, d4, d5: dword); cdecl;

function BcbMallocCallback(size: dword) : pointer; cdecl;
// there's an "allocExceptMem" function in BCB
// which allocates a record for each exception in the internal exception list
// this function patches BCB, so that some more space is allocated
// we leave space so that later the exceptAddr can be stored here
// also we put a first raw stack trace into the record
var pac      : TPACardinal;
    stack    : TDAPreStackItem;
    stackLen : dword;
begin
  stackLen := PrepareStackTrace(Ebp, 0, 0, nil, true, stack);
  result := BcbMallocNext(size + 12 + stackLen * sizeOf(TPreStackItem));
  if result <> nil then begin
    // allocation succeeded, now let's fill the structure
    // the additional 2 dwords contain the exceptAddr and a "validifier"
    ZeroMemory(result, size);
    dword(pac) := dword(result) + size;
    pac[0] := 0;
    pac[1] := pac[0] xor CBcbXorCheck;
    pac[2] := stackLen;
    if stackLen > 0 then
      Move(stack[1], pac[3], stackLen * sizeOf(TPreStackItem));
  end;
  try
    stack := nil;
  except integer(stack) := 0 end;
end;

function BcbGetExceptionObjectCallback(var er: TExceptionRecord; d1, d2, d3: dword) : pointer; cdecl;
// for OS exceptions (access violation etc) BCB calls "GetExceptionObject"
// which allocates and initializes an exception record for the OS exception
// finally "GetExceptionObject" returns the record
// we are here adding the exception address to our private field in the record
var pac : TPACardinal;
begin
  result := BcbGetExceptionObjectNext(er, d1, d2, d3);
  if result <> nil then 
    try
      dword(pac) := dword(result) + $52 + dword(pointer(dword(result) + $10)^);
      if pac[0] = pac[1] xor CBcbXorCheck then begin
        // the record has our private field, so we store the exceptAddr there
        pac[0] := dword(er.ExceptionAddress);
        pac[1] := pac[0] xor CBcbXorCheck;
      end;
    except end;
end;

procedure BcbRaiseExceptionCallback(code, flags, argCount: dword; args: TPACardinal); stdcall;
// for C++ exceptions (e.g. "throw 777") BCB calls "tossAnException"
// which in turn calls "RaiseException"
// we are again adding the exception address to our private field in the record
var pac : TPACardinal;
begin
  if (code = cCppException) and (argCount = 3) then
    try
      pac := pointer(args[2] + $52 + dword(pointer(args[2] + $10)^));
      if pac[0] = pac[1] xor CBcbXorCheck then begin
        // validity check succeeded, so we store the exceptAddr again
        pac[0] := args[1];
        pac[1] := pac[0] xor CBcbXorCheck;
      end;
    except end;
  BcbRaiseExceptionNext(code, flags, argCount, args);
end;

procedure BcbInitCatchArgPascalCallback(d1, d2: dword; var er: TExceptionRecord; d3, d4, d5: dword); cdecl;
// for Delphi exceptions BCB calls "initCatchArgPascal"
// which allocates and initializes an exception record for the Delphi exception
// also the record gets filled into the internal exception list
// once again we are adding the exception address to the record
var bcbTlsRoot   : dword;
    bcbExceptRec : dword;
    pac          : TPACardinal;
begin
  BcbInitCatchArgPascalNext(d1, d2, er, d3, d4, d5);
  if @BcbGetTls <> nil then
    try
      bcbTlsRoot := BcbGetTls;
      if bcbTlsRoot <> 0 then begin
        // we found the BCB TLS root, now we look for the exception list
        bcbExceptRec := TPCardinal(bcbTlsRoot + BcbExceptTlsOffset)^;
        if bcbExceptRec <> 0 then begin
          // there's an exception record in the list, so we do the usual work
          pac := pointer(bcbExceptRec + $52 + dword(pointer(bcbExceptRec + $10)^));
          if pac[0] = pac[1] xor CBcbXorCheck then begin
            pac[0] := er.ExceptionInformation[0];
            if er.NumberParameters = 8 then
              // for some strange reasons when BCB throws Delphi exceptions
              // the exception address is always 4 too low
              inc(pac[0], 4);
            pac[1] := pac[0] xor CBcbXorCheck;
          end;
        end;
      end;
    except end;
end;

procedure HookHandleAnyException;
asm
  mov edx, [esp]
  add edx, 4
  mov [esp], edx
  mov edx, eax
  mov eax, [esp+4+4]
  mov ecx, [eax].TExceptionRecord.ExceptionCode
  cmp ecx, cCppException
  je @cppException
  // this is the normal solution, no change to Borland default behaviour
  mov ecx, [eax].TExceptionRecord.ExceptionAddress
  jmp @done
 @cppException:
  // cpp exceptions have the exceptAddr stored in ExceptionInformation[1]
  // why does Borland not read this in by default? I don't understand it...
  mov ecx, [eax+$18]  //.TExceptionRecord.ExceptionInformation[1]
 @done:
end;

procedure BcbTerminateCallback;
// BCB exception which are raised outside of any try..whatever blocks
// can end up in a "abnormal program termination"
// we hook that, so that we can show our exception box instead
begin
  HandleException(etNormal, BcbTermination.Create('Abnormal program termination'),
                  nil, false, Esp, Ebp, nil, esBcbAbnormalProgramTermination);
end;

function PatchCall(aim, new_: pointer) : boolean; forward;

procedure HookBcbExceptions(hookPackages: boolean);
// here comes the hard part: we have to disasm some parts of BCB and
// patch the code so that our hook functions (see above) are installed

  function GetHandleAnyExceptionAddr : pointer;
  asm
    mov eax, offset System.@HandleAnyException
    cmp word ptr [eax], $25ff
    jnz @done
    mov eax, [eax + 2]
    mov eax, [eax]
   @done:
  end;

var ci             : TCodeInfo;
    dynamicRtl     : dword;
    allocExceptMem : pointer;
    fi1, fi2       : TFunctionInfo;
    i1, i2         : integer;
    p1             : pointer;
    b1             : boolean;
begin
  if BcbExceptionHandler = nil then begin
    // BCB's exception handler was not found
    // probably we're using the "dynamic RTL" (see linker options)
    // so let's try to locate the dynamic RTL functions
    {$ifdef ver180}
      dynamicRtl := GetModuleHandle('cc3270mt.dll');
      if dynamicRtl = 0 then
        dynamicRtl := GetModuleHandle('cc3270.dll');
    {$else}
      {$ifdef ver130}
        dynamicRtl := GetModuleHandle('cc3250mt.dll');
        if dynamicRtl = 0 then
          dynamicRtl := GetModuleHandle('cc3250.dll');
      {$else}
        dynamicRtl := GetModuleHandle('cc3260mt.dll');
        if dynamicRtl = 0 then
          dynamicRtl := GetModuleHandle('cc3260.dll');
      {$endif}
    {$endif}
    BcbExceptionHandler    := GetProcAddress(dynamicRtl, '____ExceptionHandler'                     );
    BcbThrowExceptionLDTC  := GetProcAddress(dynamicRtl, '@_ThrowExceptionLDTC$qpvt1t1t1uiuiuipuct1');
    BcbOrgMalloc           := GetProcAddress(dynamicRtl, '_malloc'                                  );
    BcbMemcpy              := GetProcAddress(dynamicRtl, '_memcpy'                                  );
    BcbPTerminate          := GetProcAddress(dynamicRtl, '___terminatePTR'                          );
  end;
  if (BcbExceptionHandler <> nil) and (BcbThrowExceptionLDTC <> nil) and
     (*(BcbInitExceptBlockLDTC <> nil) and *)(BcbOrgMalloc <> nil) and (BcbMemcpy <> nil) and
     ( (BcbPTerminate <> nil) or (BcbCallTerminate <> nil) ) then begin
    // first tell madDisAsm which address InitExceptBlockLDTC has
    madDisAsm.BcbInitExceptBlockLDTC := BcbInitExceptBlockLDTC;
    // BCB has an exception list stored in TLS
    // we disassemble BCB's except handler to get the TLS index of that list
    ci := ParseCode(BcbExceptionHandler);
    p1 := nil;
    while not (ci.Opcode in [$C2, $C3]) do begin
      if (p1 <> nil) and (ci.Opcode = $8b) and (ci.ModRm = $80) then begin
        BcbExceptTlsOffset := TPCardinal(dword(ci.This) + 2)^;
        BcbGetTls := p1;
        break;
      end;
      if ci.Call then
           p1 := ci.Target
      else p1 := nil;
      ci := ParseCode(ci.Next);
    end;
    if @BcbGetTls <> nil then begin
      // ThrowExceptionLDTC does nothing but call "tossAnException"
      fi1 := ParseFunction(BcbThrowExceptionLDTC);
      if Length(fi1.FarCalls) = 1 then begin
        // we found the address of "tossAnException", let's disasm it
        fi1 := ParseFunction(fi1.FarCalls[0].Target);
        p1 := @RaiseException;
        if word(p1^) = $25FF then
          p1 := pointer(TPPointer(dword(p1) + 2)^^);
        allocExceptMem := nil;
        for i1 := 0 to high(fi1.FarCalls) do
          if fi1.FarCalls[i1].Target = p1 then begin
            // here we hook "tossAnException"'s call to "RaiseException"
            if TPCardinal(dword(fi1.FarCalls[i1].CodeAddr1) - 4)^ = cCppException then begin
              ci := ParseCode(fi1.FarCalls[i1].CodeAddr1);
              if (ci.TargetSize = 4) and ci.RelTarget then begin
                {$ifdef log}log('hook BCB.tossAnException');{$endif}
                BcbRaiseExceptionNext := ci.Target;
                PatchInt(ci.PTarget, integer(@BcbRaiseExceptionCallback) - integer(ci.Next));
              end;
            end;
          end else begin
            ci := ParseCode(fi1.FarCalls[i1].Target);
            while not (ci.Opcode in [$C2, $C3]) do begin
              if ci.Target = BcbOrgMalloc then begin
                // seems we've found "allocExceptMem"
                allocExceptMem := fi1.FarCalls[i1].Target;
                // now we hook "allocExceptMem"'s call to "___org_malloc"
                if (ci.TargetSize = 4) and ci.RelTarget then begin
                  {$ifdef log}log('hook BCB.allocExceptMem');{$endif}
                  BcbMallocNext := ci.Target;
                  PatchInt(ci.PTarget, integer(@BcbMallocCallback) - integer(ci.Next));
                end;
                break;
              end;
              ci := ParseCode(ci.Next);
            end;
          end;
        if allocExceptMem <> nil then begin
          // let's analyze which functions BCB's ExceptionHandler calls
          // two of those are calling "allocExceptMem", we want to hook both
          fi1 := ParseFunction(BcbExceptionHandler);
          for i1 := 0 to high(fi1.FarCalls) do begin
            fi2 := ParseFunction(fi1.FarCalls[i1].Target);
            p1 := nil;
            b1 := false;
            for i2 := 0 to high(fi2.FarCalls) do
              if fi2.FarCalls[i2].Target = allocExceptMem then
                // looks good, but we don't know yet which of the 2 this is
                p1 := fi1.FarCalls[i1].Target
              else
                if fi2.FarCalls[i2].Target = BcbMemcpy then
                  // okay, must be "GetExceptionObject"
                  b1 := true;
            if p1 <> nil then begin
              // we've found one of those 2 functions
              ci := ParseCode(fi1.FarCalls[i1].CodeAddr1);
              if (ci.TargetSize = 4) and ci.RelTarget then
                if b1 then begin
                  // we hook "ExceptionHandler"'s call to "GetExceptionObject"
                  {$ifdef log}log('hook BCB.getExceptionObject');{$endif}
                  BcbGetExceptionObjectNext := ci.Target;
                  PatchInt(ci.PTarget, integer(@BcbGetExceptionObjectCallback) - integer(ci.Next));
                end else begin
                  // we hook "ExceptionHandler"'s call to "initCatchArgPascal"
                  {$ifdef log}log('hook BCB.initCatchArgPascal');{$endif}
                  BcbInitCatchArgPascalNext := ci.Target;
                  PatchInt(ci.PTarget, integer(@BcbInitCatchArgPascalCallback) - integer(ci.Next));
                end;
            end;
          end;
        end;
      end;
    end;
    if hookPackages then begin
      ci := ParseCode(GetHandleAnyExceptionAddr);
      while ci.IsValid and (not (ci.Opcode in [$c2, $c3])) and
            ( (ci.Opcode <> $89) or (TPCardinal(dword(ci.This) + 1)^ <> $24448bc2) or
                                    (TPCardinal(dword(ci.This) + 5)^ <> $0c488b04)    ) do
        ci := ParseCode(ci.Next);
      if ci.IsValid and (not (ci.Opcode in [$c2, $c3])) then begin
        p1 := ci.This;
        ci := ParseCode(ci.Next);
        while ci.IsValid and (not (ci.Opcode in [$c2, $c3])) and
              ( (ci.Opcode <> $89) or (TPCardinal(dword(ci.This) + 1)^ <> $24448bc2) or
                                      (TPCardinal(dword(ci.This) + 5)^ <> $0c488b04)    ) do
          ci := ParseCode(ci.Next);
        if ci.IsValid and (not (ci.Opcode in [$c2, $c3])) then
          p1 := ci.This;
        if p1 <> nil then
          PatchCall(p1, @HookHandleAnyException);
      end;
    end;
    // now let's hook BCB's "terminate" call (= abnormal program termination)
    if BcbPTerminate <> nil then
      // when using the dynamic RTL we have access to the function pointer
      BcbPTerminate^ := @BcbTerminateCallback
    else
      // else we can only patch the "___call_terminate" function
      PatchJmp(BcbCallTerminate, @BcbTerminateCallback);
  end;
end;

procedure GetBcbExceptInfo(var exceptAddr: pointer; var exceptObject: TObject; var preparedStack: pointer);
// finally we can profit from all the bad stuff we've done (see above)
var bcbTlsRoot   : dword;
    bcbExceptRec : dword;
    xdSize       : dword;
    xdArgBuff    : byte;
    xdValue      : TPPointer;
    xdPac        : TPACardinal;
begin
  if @BcbGetTls <> nil then
    try
      bcbTlsRoot := BcbGetTls;
      if bcbTlsRoot <> 0 then begin
        // we found the BCB TLS root, now we look for the exception list
        bcbExceptRec := TPCardinal(bcbTlsRoot + BcbExceptTlsOffset)^;
        if bcbExceptRec <> 0 then begin
          // there's an exception record in the list
          xdSize := TPCardinal(bcbExceptRec + $10)^;
          if exceptObject = nil then
            try
              xdArgBuff := TPByte   (bcbExceptRec + $44)^;
              xdValue   := TPPointer(bcbExceptRec + $52)^;
              if (xdArgBuff > 0) and (xdSize = 4) then begin
                exceptObject := TObject(xdValue);
                if (exceptObject <> nil) and (not IsValidObject(exceptObject)) then
                  exceptObject := nil;
              end;
            except end;
          if exceptAddr = nil then begin
            xdPac := pointer(bcbExceptRec + $52 + xdSize);
            if xdPac[0] = xdPac[1] xor CBcbXorCheck then begin
              exceptAddr := pointer(xdPac[0]);
              if xdPac[2] > 0 then
                preparedStack := @xdPac[2];
            end;
          end;
        end;
      end;
    except end;
end;

{$endif}

// ***************************************************************
// complete/correct exception base parameters

threadvar
  LastExceptObject : TObject;
  LastExceptAddr   : pointer;
  LastContext      : TContext;

procedure CheckExceptParams(var exceptObject: TObject; var exceptAddr: pointer;
                            var preparedStack: pointer; var context: PContext);
var p1 : pointer;
begin
  try
    if (exceptObject = nil) and (RaiseList <> nil) then
      exceptObject := TRaiseFrame(RaiseList^).ExceptObject;
  except end;
  try
    if (exceptAddr = nil) and (RaiseList <> nil) then
      exceptAddr := TRaiseFrame(RaiseList^).ExceptAddr;
  except end;
  if (exceptObject <> nil) and (not IsValidObject(exceptObject)) then
    exceptObject := nil;
  {$ifdef bcb}
    if (exceptAddr = nil) or (exceptObject = nil) then
      GetBcbExceptInfo(exceptAddr, exceptObject, preparedStack);
  {$endif}
  if (exceptObject <> nil) and (LastExceptObject = exceptObject) and (LastExceptAddr = exceptAddr) then begin
    // Delphi doesn't compile "context := @LastContext"
    p1 := @LastContext;
    context := p1;
  end;
end;

procedure CheckExceptAddr(exceptObject: TObject; var exceptAddr: pointer);

  function IsCorrectExceptAddr(const fi: TFunctionInfo; exceptAddr: pointer) : boolean;
  var i1 : integer;
      ci : TCodeInfo;
  begin
    result := false;
    for i1 := 0 to high(fi.FarCalls) do
      if fi.FarCalls[i1].CodeAddr2 = exceptAddr then begin
        result := true;
        exit;
      end;
    for i1 := 0 to high(fi.UnknownTargets) do
      if fi.UnknownTargets[i1].CodeAddr2 = exceptAddr then begin
        result := true;
        exit;
      end;
    for i1 := 0 to high(fi.CodeAreas) do
      if (dword(exceptAddr) >= dword(fi.CodeAreas[i1].AreaBegin)    ) and
         (dword(exceptAddr) <= dword(fi.CodeAreas[i1].AreaEnd  ) + 1) then begin
        // the item does lie inside of our function's code areas
        // so we check whether the item fits to the code
        ci.Next := fi.CodeAreas[i1].AreaBegin;
        while dword(ci.Next) < dword(exceptAddr) do
          ci := ParseCode(ci.Next);
        result := ci.Next = exceptAddr;
        break;
      end;
  end;

var mf : TMapFile;
    fi : TFunctionInfo;
begin
  if (exceptAddr <> nil) and ((exceptObject = nil) or (not IsClass(exceptObject, 'EAbort'))) then
    try
      mf := FindMapFile(exceptAddr);
      if mf.IsValid then begin
        with mf.FindPublic(exceptAddr) do
          if IsValid then begin
            fi := ParseFunction(Address);
            if (not IsCorrectExceptAddr(fi,               exceptAddr      )) and
               (    IsCorrectExceptAddr(fi, pointer(dword(exceptAddr) + 4))) then
              // BCB sometimes reports an exception address which is 4 too low
              inc(dword(exceptAddr), 4);
          end;
      end else
        mf.Free;
    except end;
end;

// ***************************************************************
// settings properties

type
  TPropertyType = (ptString, ptInteger, ptBoolean, ptBinary);
  TMEProperty = packed record
    name  : pchar;
    typ   : TPropertyType;
    vBool : boolean;
    vInt  : integer;
    vStr  : pchar;
  end;
  TPMEProperty = ^TMEProperty;
  TAMEProperty = array [0..maxInt shr 4 - 1] of TMEProperty;
  TPAMEProperty = ^TAMEProperty;

const
  CMEProperties : array [0..120] of TMEProperty = (
    (name: 'Enabled';           typ: ptBoolean; vBool : false                                      ),
    (name: 'NoSettings';        typ: ptBoolean; vBool : true                                       ),
    (name: 'CheckFileCrc';      typ: ptBoolean; vBool : true                                       ),
    (name: 'CheckFreeze';       typ: ptBoolean; vBool : false                                      ),
    (name: 'FreezeTimeout';     typ: ptInteger; vInt  : 60                                         ),
    (name: 'AutoSave';          typ: ptBoolean; vBool : true                                       ),
    (name: 'AutoSaveIfNotSent'; typ: ptBoolean; vBool : true                                       ),
    (name: 'AutoSend';          typ: ptBoolean; vBool : false                                      ),
    (name: 'AutoSendBox';       typ: ptBoolean; vBool : false                                      ),
    (name: 'AutoClip';          typ: ptBoolean; vBool : true                                       ),
    (name: 'PauseThreads';      typ: ptBoolean; vBool : false                                      ),
    (name: 'PlWaitBox';         typ: ptBoolean; vBool : true                                       ),
    (name: 'AutoContinue';      typ: ptBoolean; vBool : false                                      ),
    (name: 'AutoRestart';       typ: ptInteger; vInt  : 0                                          ),
    (name: 'AutoClose';         typ: ptInteger; vInt  : 0                                          ),
    (name: 'AutoDelay';         typ: ptInteger; vInt  : 60                                         ),
    (name: 'SendBtnVis';        typ: ptBoolean; vBool : true                                       ),
    (name: 'SaveBtnVis';        typ: ptBoolean; vBool : false                                      ),
    (name: 'PrintBtnVis';       typ: ptBoolean; vBool : false                                      ),
    (name: 'ShowBtnVis';        typ: ptBoolean; vBool : true                                       ),
    (name: 'ContinueBtnVis';    typ: ptBoolean; vBool : true                                       ),
    (name: 'RestartBtnVis';     typ: ptBoolean; vBool : true                                       ),
    (name: 'CloseBtnVis';       typ: ptBoolean; vBool : true                                       ),
    (name: 'FocusedBtn';        typ: ptBoolean; vInt  : integer(bSendBugReport)                    ),
    (name: 'SendAssis';         typ: ptString;  vStr  : ''                                         ),
    (name: 'SaveAssis';         typ: ptString;  vStr  : ''                                         ),
    (name: 'PrintAssis';        typ: ptString;  vStr  : ''                                         ),
    (name: 'AutoShowBugRep';    typ: ptBoolean; vBool : false                                      ),
    (name: 'UglyBtns';          typ: ptBoolean; vBool : false                                      ),
    (name: 'MailAddr';          typ: ptString;  vStr  : ''                                         ),
    (name: 'SendInBackgr';      typ: ptBoolean; vBool : true                                       ),
    (name: 'MailAsSmtpServer';  typ: ptBoolean; vBool : false                                      ),
    (name: 'MailAsSmtpClient';  typ: ptBoolean; vBool : false                                      ),
    (name: 'UploadViaHttp';     typ: ptBoolean; vBool : false                                      ),
    (name: 'MailViaMapi';       typ: ptBoolean; vBool : true                                       ),
    (name: 'MailViaMailto';     typ: ptBoolean; vBool : true                                       ),
    (name: 'SmtpServer';        typ: ptString;  vStr  : ''                                         ),
    (name: 'SmtpPort';          typ: ptInteger; vInt  : 25                                         ),
    (name: 'SmtpAccount';       typ: ptString;  vStr  : ''                                         ),
    (name: 'SmtpPassword';      typ: ptString;  vStr  : ''                                         ),
    (name: 'HttpServer';        typ: ptString;  vStr  : ''                                         ),
    (name: 'HttpPort';          typ: ptInteger; vInt  : 80                                         ),
    (name: 'HttpAccount';       typ: ptString;  vStr  : ''                                         ),
    (name: 'HttpPassword';      typ: ptString;  vStr  : ''                                         ),
    (name: 'AttachBugRep';      typ: ptBoolean; vBool : true                                       ),
    (name: 'AttachBugRepFile';  typ: ptBoolean; vBool : true                                       ),
    (name: 'DelBugRepFile';     typ: ptBoolean; vBool : true                                       ),
    (name: 'BugRepSendAs';      typ: ptString;  vStr  : 'bugreport.txt'                            ),
    (name: 'BugRepZip';         typ: ptString;  vStr  : ''                                         ),
    (name: 'ScrShotDepth';      typ: ptInteger; vInt  : 0                                          ),
    (name: 'ScrShotAppOnly';    typ: ptBoolean; vBool : true                                       ),
    (name: 'ScrShotSendAs';     typ: ptString;  vStr  : 'screenshot.png'                           ),
    (name: 'ScrShotZip';        typ: ptString;  vStr  : ''                                         ),
    (name: 'AddAttachs';        typ: ptString;  vStr  : ''                                         ),
    (name: 'MailFrom';          typ: ptString;  vStr  : ''                                         ),
    (name: 'AddFields';         typ: ptString;  vStr  : ''                                         ),
    (name: 'BugRepFile';        typ: ptString;  vStr  : 'bugreport.txt'                            ),
    (name: 'AppendBugReps';     typ: ptBoolean; vBool : true                                       ),
    (name: 'BugRepFileSize';    typ: ptInteger; vInt  : 100000                                     ),
    (name: 'NoDupExcepts';      typ: ptBoolean; vBool : true                                       ),
    (name: 'NoDupFreezes';      typ: ptBoolean; vBool : true                                       ),
    (name: 'DupExceptDef';      typ: ptBoolean; vInt  : integer(ddCrashStackIdentical)             ),
    (name: 'DupFreezeDef';      typ: ptBoolean; vInt  : integer(ddAllStacksIdentical)              ),
    (name: 'ListThreads';       typ: ptBoolean; vBool : true                                       ),
    (name: 'CpuRegs';           typ: ptBoolean; vBool : true                                       ),
    (name: 'StackDump';         typ: ptBoolean; vBool : true                                       ),
    (name: 'ShowDisAsm';        typ: ptBoolean; vBool : true                                       ),
    (name: 'HideUglyItems';     typ: ptBoolean; vBool : false                                      ),
    (name: 'ShowRelAddrs';      typ: ptBoolean; vBool : true                                       ),
    (name: 'ShowRelLines';      typ: ptBoolean; vBool : true                                       ),
    (name: 'FormatDisAsm';      typ: ptBoolean; vBool : false                                      ),
    (name: 'LimitDisAsm';       typ: ptInteger; vInt  : 5                                          ),
    (name: 'Plugins';           typ: ptString;  vStr  : 'modules|processes|hardware'               ),
    (name: 'F1Classes';         typ: ptString;  vStr  : ''                                         ),
    (name: 'F1NoBugRep';        typ: ptBoolean; vBool : false                                      ),
    (name: 'F1NoScrShot';       typ: ptBoolean; vBool : false                                      ),
    (name: 'F1NoHandlers';      typ: ptBoolean; vBool : false                                      ),
    (name: 'F1NoSuspend';       typ: ptBoolean; vBool : false                                      ),
    (name: 'F1ShowCfg';         typ: ptBoolean; vInt  : integer(ssFullBox)                         ),
    (name: 'F1Assis';           typ: ptString;  vStr  : ''                                         ),
    (name: 'F2Classes';         typ: ptString;  vStr  : ''                                         ),
    (name: 'F2NoBugRep';        typ: ptBoolean; vBool : false                                      ),
    (name: 'F2NoScrShot';       typ: ptBoolean; vBool : false                                      ),
    (name: 'F2NoHandlers';      typ: ptBoolean; vBool : false                                      ),
    (name: 'F2NoSuspend';       typ: ptBoolean; vBool : false                                      ),
    (name: 'F2ShowCfg';         typ: ptBoolean; vInt  : integer(ssFullBox)                         ),
    (name: 'F2Assis';           typ: ptString;  vStr  : ''                                         ),
    (name: 'GnNoBugRep';        typ: ptBoolean; vBool : false                                      ),
    (name: 'GnNoScrShot';       typ: ptBoolean; vBool : false                                      ),
    (name: 'GnNoHandlers';      typ: ptBoolean; vBool : false                                      ),
    (name: 'GnNoSuspend';       typ: ptBoolean; vBool : false                                      ),
    (name: 'GnShowCfg';         typ: ptBoolean; vInt  : integer(ssFullBox)                         ),
    (name: 'GnAssis';           typ: ptString;  vStr  : ''                                         ),
    (name: 'TitleBar';          typ: ptString;  vStr  : '%appname%'                                ),
    (name: 'ExceptMsg';         typ: ptString;  vStr  : 'An error occurred in the application.'    ),
    (name: 'FrozenMsg';         typ: ptString;  vStr  : 'The application seems to be frozen.'      ),
    (name: 'BitFaultMsg';       typ: ptString;  vStr  : 'The file "%modname%" seems to be corrupt!'),
    (name: 'SendBtnTxt';        typ: ptString;  vStr  : 'send bug report'                          ),
    (name: 'SaveBtnTxt';        typ: ptString;  vStr  : 'save bug report'                          ),
    (name: 'PrintBtnTxt';       typ: ptString;  vStr  : 'print bug report'                         ),
    (name: 'ShowBtnTxt';        typ: ptString;  vStr  : 'show bug report'                          ),
    (name: 'ContinueBtnTxt';    typ: ptString;  vStr  : 'continue bug report'                      ),
    (name: 'RestartBtnTxt';     typ: ptString;  vStr  : 'restart bug report'                       ),
    (name: 'CloseBtnTxt';       typ: ptString;  vStr  : 'close bug report'                         ),
    (name: 'OkBtnTxt';          typ: ptString;  vStr  : '&OK'                                      ),
    (name: 'DetailsBtnTxt';     typ: ptString;  vStr  : '&Details'                                 ),
    (name: 'PlWaitTitle';       typ: ptString;  vStr  : 'Information'                              ),
    (name: 'PlWaitText';        typ: ptString;  vStr  : 'Please wait a moment...'                  ),
    (name: 'MailSubj';          typ: ptString;  vStr  : 'bug report'                               ),
    (name: 'MailBody';          typ: ptString;  vStr  : 'please find the bug report attached'      ),
    (name: 'SendBoxTitle';      typ: ptString;  vStr  : 'Sending bug report...'                    ),
    (name: 'PrepAttMsg';        typ: ptString;  vStr  : 'Prepare attachments...'                   ),
    (name: 'MxLookMsg';         typ: ptString;  vStr  : 'Searching for mail server...'             ),
    (name: 'ConnMsg';           typ: ptString;  vStr  : 'Connecting to server...'                  ),
    (name: 'AuthMsg';           typ: ptString;  vStr  : 'Authentication...'                        ),
    (name: 'SendMailMsg';       typ: ptString;  vStr  : 'Sending mail...'                          ),
    (name: 'FieldMsg';          typ: ptString;  vStr  : 'Setting fields...'                        ),
    (name: 'SendAttMsg';        typ: ptString;  vStr  : 'Sending attachments...'                   ),
    (name: 'SendFinalMsg';      typ: ptString;  vStr  : 'Finalizing...'                            ),
    (name: 'SendFailMsg';       typ: ptString;  vStr  : 'Sorry, sending the bug report didn''t work.'),
    (name: 'VersionVar';        typ: ptInteger; vInt  : 0                                          ) );

function GetMeSettingsRes(module: dword) : dword;
begin
  result := FindResource(module, 'TMADEXCEPT', pchar(RT_RCDATA));
end;

procedure SetMEPropertyStr(var prop: TMEProperty; vStrBuf: pchar; vStrLen: integer);
begin
  if (vStrBuf <> nil) and (vStrLen > 0) then begin
    prop.vStr := pointer(LocalAlloc(LPTR, vStrLen + 1));
    Move(vStrBuf^, prop.vStr^, vStrLen);
  end else
    prop.vStr := nil;
end;

function AddMEProperty(var propCap, propCount: integer; var props: TPAMEProperty;
                       nameBuf: pchar; nameLen: integer; typ: TPropertyType;
                       vBool: boolean = false; vInt: integer = 0;
                       vStrBuf: pchar = nil; vStrLen: integer = 0) : integer;
var p1 : TPAMEProperty;
begin
  if propCount = propCap then begin
    p1 := props;
    propCap := propCap + 20;
    props := pointer(LocalAlloc(LPTR, sizeOf(TMEProperty) * propCap));
    Move(p1^, props^, sizeOf(TMEProperty) * propCount);
    LocalFree(dword(p1));
  end;
  result := propCount;
  props[result].name := pointer(LocalAlloc(LPTR, nameLen + 1));
  Move(nameBuf^, props[result].name^, nameLen);
  props[result].typ   := typ;
  props[result].vBool := vBool;
  props[result].vInt  := vInt;
  SetMEPropertyStr(props[result], vStrBuf, vStrLen);
  inc(propCount);
end;

function ParseMEProperties(module: dword; var propCap, propCount: integer) : TPAMEProperty;
const CDfmHeader = 'TPF0' + #$a + 'TMadExcept' + #$9 + 'madExcept';
var res     : dword;
    pc1     : pchar;
    nameLen : integer;
    nameBuf : pchar;
    ws      : wideString;
    c1      : dword;
begin
  result := nil;
  propCap := 0;
  propCount := 0;
  c1 := FindResourceHInstance(module);
  res := GetMeSettingsRes(c1);
  if res <> 0 then
    module := c1
  else
    // didn't find the madExcept resource in the language dll
    res := GetMeSettingsRes(module);
  if res <> 0 then begin
    res := LoadResource(module, res);
    if res <> 0 then begin
      pc1 := LockResource(res);
      if PosPChar(pc1, CDfmHeader, Length(CDfmHeader), Length(CDfmHeader)) = 0 then begin
        inc(pc1, Length(CDfmHeader));
        propCap := Length(CMEProperties) + 10;
        result := pointer(LocalAlloc(LPTR, sizeOf(TMEProperty) * propCap));
        while pc1^ <> #0 do begin
          nameLen := byte(pc1^);
          nameBuf := pc1 + 1;
          if nameLen = 0 then
            break;
          inc(pc1, 1 + byte(pc1^) + 1);
          case (pc1 - 1)^ of
            #$2      : begin
                         AddMEProperty(propCap, propCount, result, nameBuf, nameLen, ptInteger, false, TPShortInt(pc1)^);
                         inc(pc1, 1);
                       end;
            #$3      : begin
                         AddMEProperty(propCap, propCount, result, nameBuf, nameLen, ptInteger, false, TPSmallInt(pc1)^);
                         inc(pc1, 2);
                       end;
            #$4      : begin
                         AddMEProperty(propCap, propCount, result, nameBuf, nameLen, ptInteger, false, TPInteger(pc1)^);
                         inc(pc1, 4);
                       end;
            #$6      : begin
                         AddMEProperty(propCap, propCount, result, nameBuf, nameLen, ptString, false, 0, pc1 + 1, TPByte(pc1)^);
                         inc(pc1, 1 + TPByte(pc1)^);
                       end;
            #$8, #$9 : AddMEProperty(propCap, propCount, result, nameBuf, nameLen, ptBoolean, (pc1 - 1)^ = #$9);
            #$c      : begin
                         AddMEProperty(propCap, propCount, result, nameBuf, nameLen, ptString, false, 0, pc1 + 4, TPInteger(pc1)^);
                         inc(pc1, 4 + TPInteger(pc1)^);
                       end;
            #$12     : begin
                         SetLength(ws, TPInteger(pc1)^);
                         inc(pc1, 4);
                         if ws <> '' then
                           Move(pc1^, ws[1], Length(ws) * 2);
                         AddMEProperty(propCap, propCount, result, nameBuf, nameLen, ptString, false, 0, pchar(string(ws)), Length(ws));
                         inc(pc1, Length(ws) * 2);
                       end;
            else       begin
                         MessageBox(0, pchar('Internal error: Invalid settings resource (' + IntToHexEx(dword((pc1 - 1)^)) + ').'), 'madExcept', 0);
                         break;
                       end;
          end;
        end;
      end;
      UnlockResource(res);
      FreeResource(res);
    end;
  end;
end;

function FindMEProperty(count: integer; props: TPAMEProperty; name: string) : integer;
var i1 : integer;
begin
  result := -1;
  for i1 := 0 to count - 1 do
    if props[i1].name = name then begin
      result := i1;
      exit;
    end;
end;

// ***************************************************************
// madExcept settings buffer

type
  TPMESettingsModule = ^TMESettingsModule;
  TMESettingsModule = record
    refCount  : integer;
    module    : dword;
    propCap   : integer;
    propCount : integer;
    props     : TPAMEProperty;
    next      : TPMESettingsModule;
  end;
  TMESettingsBuf = packed record
    refCount  : integer;
    mapHandle : dword;
    section   : TRTLCriticalSection;
    settings  : TPMESettingsModule;
    reserved  : pointer;
  end;

var
  meSettingsBuf : ^TMESettingsBuf = nil;
  mySettings    : IMEModuleSettings = nil;

function EnterSettingsMutex : dword;
var sa  : TSecurityAttributes;
    sd  : TSecurityDescriptor;
begin
  InitSecAttr(sa, sd);
  result := CreateMutex(@sa, false, pchar('madExceptSettingsMtx' + IntToHexEx(GetCurrentProcessId)));
  WaitForSingleObject(result, INFINITE);
end;

procedure LeaveSettingsMutex(mutex: dword);
begin
  ReleaseMutex(mutex);
  CloseHandle(mutex);
end;

function ReferenceMeSettingsBuf : boolean;
var mesb : ^TMESettingsBuf;
    map  : dword;
    mtx  : dword;
    sa   : TSecurityAttributes;
    sd   : TSecurityDescriptor;
begin
  result := false;
  mtx := EnterSettingsMutex;
  try
    if meSettingsBuf = nil then begin
      InitSecAttr(sa, sd);
      map := CreateFileMapping(maxCard, @sa, PAGE_READWRITE, 0, sizeOf(TMESettingsBuf),
                               pchar('madExceptSettingsBuf' + IntToHexEx(GetCurrentProcessId)));
      if map = 0 then
        map := OpenFileMapping(FILE_MAP_READ, false, pchar('madExceptSettingsBuf' + IntToHexEx(GetCurrentProcessId)));
      if GetLastError = 0 then begin
        {$ifdef log}log('new settings buffer');{$endif}
        mesb := MapViewOfFile(map, FILE_MAP_ALL_ACCESS, 0, 0, 0);
        mesb^.refCount  := 0;
        mesb^.mapHandle := map;
        InitializeCriticalSection(mesb^.section);
        mesb^.settings  := nil;
        mesb^.reserved  := nil;
      end else
        if map <> 0 then begin
          {$ifdef log}log('open settings buffer: success');{$endif}
          mesb := MapViewOfFile(map, FILE_MAP_ALL_ACCESS, 0, 0, 0);
          CloseHandle(map);
        end else begin
          {$ifdef log}log('open settings buffer: error');{$endif}
          mesb := nil;
          MessageBoxA(0, 'internal error (opening settings buffer)', 'madExcept', 0);
        end;
      if mesb <> nil then
        meSettingsBuf := pointer(mesb);
    end;
    if meSettingsBuf <> nil then begin
      {$ifdef log}log('increment settings buffer refcount');{$endif}
      inc(meSettingsBuf^.refCount);
      result := true;
    end;
  finally LeaveSettingsMutex(mtx) end;
end;

procedure DereferenceMeSettingsBuf;
var mtx : dword;
    map : dword;
begin
  {$ifdef log}log('decrement settings buffer refcount');{$endif}
  mtx := EnterSettingsMutex;
  try
    if meSettingsBuf <> nil then
      if meSettingsBuf.refCount = 1 then begin
        {$ifdef log}log('free settings buffer');{$endif}
        DeleteCriticalSection(meSettingsBuf.section);
        map := meSettingsBuf.mapHandle;
        UnmapViewOfFile(meSettingsBuf);
        meSettingsBuf := nil;
        CloseHandle(map);
      end else
        dec(meSettingsBuf.refCount);
  finally LeaveSettingsMutex(mtx) end;
end;

procedure FillProps(module: dword; mes: TPMESettingsModule);

  function GetPropStr(var mes: TPMESettingsModule; prop: string) : string;
  var i1 : integer;
  begin
    i1 := FindMEProperty(mes^.propCount, mes^.props, prop);
    if i1 <> -1 then
         result := mes^.props[i1].vStr
    else result := '';
  end;

  function SetPropStr(var mes: TPMESettingsModule; prop, value: string) : string;
  var i1 : integer;
  begin
    i1 := FindMEProperty(mes^.propCount, mes^.props, prop);
    SetMEPropertyStr(mes^.props[i1], pchar(value), length(value));
  end;
  
var s1 : string;
begin
  mes^.props := ParseMEProperties(module, mes^.propCap, mes^.propCount);
  s1 := GetPropStr(mes, 'SmtpPassword');
  if s1 <> '' then begin
    s1 := DecryptPassword(Decode(s1), GetPropStr(mes, 'SmtpAccount'), GetPropStr(mes, 'SmtpServer'));
    SetPropStr(mes, 'SmtpPassword', s1);
  end;
  s1 := GetPropStr(mes, 'HttpPassword');
  if s1 <> '' then begin
    s1 := DecryptPassword(Decode(s1), GetPropStr(mes, 'HttpAccount'), GetPropStr(mes, 'HttpServer'));
    SetPropStr(mes, 'HttpPassword', s1);
  end;
end;

function ReferenceMeSettingsModule(module: dword) : TPMESettingsModule;
var mes : ^TPMESettingsModule;
begin
  result := nil;
  if (GetMeSettingsRes(module) <> 0) and ReferenceMeSettingsBuf then begin
    EnterCriticalSection(meSettingsBuf^.section);
    try
      mes := @meSettingsBuf^.settings;
      while (mes^ <> nil) and (mes^^.module <> module) do
        mes := @mes^^.next;
      if mes^ = nil then begin
        mes^ := pointer(LocalAlloc(LPTR, sizeOf(mes^^)));
        mes^^.module := module;
        FillProps(module, mes^);
      end;
      inc(mes^^.refCount);
      result := mes^;
    finally LeaveCriticalSection(meSettingsBuf^.section) end;
  end;
end;

procedure UnlistMeSettingsModule(module: dword);
var mes : ^TPMESettingsModule;
begin
  EnterCriticalSection(meSettingsBuf^.section);
  try
    mes := @meSettingsBuf^.settings;
    while (mes^ <> nil) and (mes^^.module <> module) do
      mes := @mes^^.next;
    if mes^ <> nil then
      mes^ := mes^^.next;
  finally LeaveCriticalSection(meSettingsBuf^.section) end;
end;

procedure FreeProps(var settings: TMESettingsModule);
var i1 : integer;
begin
  for i1 := 0 to settings.propCount - 1 do begin
    if settings.props[i1].name <> nil then
      LocalFree(dword(settings.props[i1].name));
    if settings.props[i1].vStr <> nil then
      LocalFree(dword(settings.props[i1].vStr));
  end;
  LocalFree(dword(settings.props));
  settings.props := nil;
end;

procedure DereferenceMeSettingsModule(settings: TPMESettingsModule);
begin
  EnterCriticalSection(meSettingsBuf^.section);
  try
    if settings.refCount = 1 then begin
      UnlistMeSettingsModule(settings.module);
      if settings.props <> nil then
        FreeProps(settings^);
      LocalFree(dword(settings));
    end else
      dec(settings.refCount);
  finally LeaveCriticalSection(meSettingsBuf^.section) end;
  DereferenceMeSettingsBuf;
end;

procedure RefreshProps(module: dword; settings: TPMESettingsModule);
begin
  EnterCriticalSection(meSettingsBuf^.section);
  try
    if settings.props <> nil then
      FreeProps(settings^);
    FillProps(module, settings);
  finally LeaveCriticalSection(meSettingsBuf^.section) end;
end;

// ***************************************************************
// madExcept settings & exception classes

type
  // forward
  TIMESettings = class;
  TIMEException = class;

  // extended settings interface access
  IMESettingsEx = interface ['{8C612485-CD17-4C66-8C62-D6D94D54EA50}']
    function GetPropBool (name: string) : boolean;
    function GetPropInt  (name: string) : integer;
    function GetPropStr  (name: string) : string;
    function GetPropDw   (name: string) : dword;

    procedure FillFormCache (assistant: INVAssistant);
    procedure ReadFormCache (assistant: INVAssistant);
    
    function GetSelf : TIMESettings;
  end;

  // settings base class
  TIMESettings = class (TInterfacedObject, IMESettings, IMEModuleSettings, IMESettingsEx)
    FValid      : boolean;
    FModule     : dword;
    FSettings   : TPMESettingsModule;
    FVersionVar : string;
    FAddAttachs : IMEAttachments;
    FAddFields  : IMEFields;
    FFormCache  : IMEFields;

    constructor Create (module: dword; init: boolean);
    destructor Destroy; override;

    function  IsValid : boolean;
    function  Module : dword;

    function  Enabled : boolean;
    function  MinDebugInfoOnly : boolean;
    function  NoOwnSettings : boolean;
    function  CheckFileCrc : boolean;
    function  CheckForFreeze : boolean; // false
    function  FreezeTimeout : dword; // default 60
    // ON EXCEPTION AUTO ACTIONS
    function  GetAutoSave : boolean;
    procedure SetAutoSave (value: boolean);
    function  GetAutoSaveIfNotSent : boolean;
    procedure SetAutoSaveIfNotSent (value: boolean);
    function  GetAutoSend : boolean;
    procedure SetAutoSend (value: boolean);
    function  GetAutoSendPrgrBox : boolean;
    procedure SetAutoSendPrgrBox (value: boolean);
    function  GetAutoClipboard : boolean;
    procedure SetAutoClipboard (value: boolean);
    function  GetSuspendThreads : boolean;
    procedure SetSuspendThreads (value: boolean);
    function  GetShowPleaseWaitBox : boolean;
    procedure SetShowPleaseWaitBox (value: boolean);
    function  GetAutoContinue : boolean;
    procedure SetAutoContinue (value: boolean);
    function  GetAutoRestart : dword;
    procedure SetAutoRestart (value: dword);
    function  GetAutoClose : dword;
    procedure SetAutoClose (value: dword);
    function  GetAutoDelay : dword;
    procedure SetAutoDelay (value: dword);
    // EXCEPTION FILTER
    function  GetFilter1Classes : string;
    function  GetFilter2Classes : string;
    procedure SetFilter1Classes (value: string);
    procedure SetFilter2Classes (value: string);
    function  GetFilter1NoBugReport : boolean;
    function  GetFilter2NoBugReport : boolean;
    function  GetGeneralNoBugReport : boolean;
    procedure SetFilter1NoBugReport (value: boolean);
    procedure SetFilter2NoBugReport (value: boolean);
    procedure SetGeneralNoBugReport (value: boolean);
    function  GetFilter1NoScreenShot : boolean;
    function  GetFilter2NoScreenShot : boolean;
    function  GetGeneralNoScreenShot : boolean;
    procedure SetFilter1NoScreenShot (value: boolean);
    procedure SetFilter2NoScreenShot (value: boolean);
    procedure SetGeneralNoScreenShot (value: boolean);
    function  GetFilter1NoSuspend : boolean;
    function  GetFilter2NoSuspend : boolean;
    function  GetGeneralNoSuspend : boolean;
    procedure SetFilter1NoSuspend (value: boolean);
    procedure SetFilter2NoSuspend (value: boolean);
    procedure SetGeneralNoSuspend (value: boolean);
    function  GetFilter1NoHandlers : boolean;
    function  GetFilter2NoHandlers : boolean;
    function  GetGeneralNoHandlers : boolean;
    procedure SetFilter1NoHandlers (value: boolean);
    procedure SetFilter2NoHandlers (value: boolean);
    procedure SetGeneralNoHandlers (value: boolean);
    function  GetFilter1ShowSetting : TMEShowSetting;
    function  GetFilter2ShowSetting : TMEShowSetting;
    function  GetGeneralShowSetting : TMEShowSetting;
    procedure SetFilter1ShowSetting (value: TMEShowSetting);
    procedure SetFilter2ShowSetting (value: TMEShowSetting);
    procedure SetGeneralShowSetting (value: TMEShowSetting);
    function  GetFilter1Assistant : string;
    function  GetFilter2Assistant : string;
    function  GetGeneralAssistant : string;
    procedure SetFilter1Assistant (value: string);
    procedure SetFilter2Assistant (value: string);
    procedure SetGeneralAssistant (value: string);
    // EXCEPTION BOX SETTINGS
    function      GetSendBtnVisible : boolean;
    function      GetSaveBtnVisible : boolean;
    function     GetPrintBtnVisible : boolean;
    function      GetShowBtnVisible : boolean;
    function  GetContinueBtnVisible : boolean;
    function   GetRestartBtnVisible : boolean;
    function     GetCloseBtnVisible : boolean;
    procedure     SetSendBtnVisible (value: boolean);
    procedure     SetSaveBtnVisible (value: boolean);
    procedure    SetPrintBtnVisible (value: boolean);
    procedure     SetShowBtnVisible (value: boolean);
    procedure SetContinueBtnVisible (value: boolean);
    procedure  SetRestartBtnVisible (value: boolean);
    procedure    SetCloseBtnVisible (value: boolean);
    function  GetFocusedButton : TMEButton;
    procedure SetFocusedButton (value: TMEButton);
    function  GetSendAssistant : string;
    procedure SetSendAssistant (value: string);
    function  GetSaveAssistant : string;
    procedure SetSaveAssistant (value: string);
    function  GetPrintAssistant : string;
    procedure SetPrintAssistant (value: string);
    function  GetAutoShowBugReport : boolean;
    procedure SetAutoShowBugReport (value: boolean);
    function  GetNoOwnerDrawButtons : boolean;
    procedure SetNoOwnerDrawButtons (value: boolean);
    // EMAIL SETTINGS
    function  GetMailAddr : string;
    procedure SetMailAddr (value: string);
    function  GetSendInBackground : boolean;
    procedure SetSendInBackground (value: boolean);
    function  GetMailAsSmtpServer : boolean;
    procedure SetMailAsSmtpServer (value: boolean);
    function  GetMailAsSmtpClient : boolean;
    procedure SetMailAsSmtpClient (value: boolean);
    function  GetUploadViaHttp : boolean;
    procedure SetUploadViaHttp (value: boolean);
    function  GetMailViaMapi : boolean;
    procedure SetMailViaMapi (value: boolean);
    function  GetMailViaMailto : boolean;
    procedure SetMailViaMailto (value: boolean);
    function  GetSmtpServer : string;
    procedure SetSmtpServer (value: string);
    function  GetSmtpPort : dword;
    procedure SetSmtpPort (value: dword);
    function  GetSmtpAccount : string;
    procedure SetSmtpAccount (value: string);
    function  GetSmtpPassword : string;
    procedure SetSmtpPassword (value: string);
    function  GetHttpServer : string;
    procedure SetHttpServer (value: string);
    function  GetHttpPort : dword;
    procedure SetHttpPort (value: dword);
    function  GetHttpAccount : string;
    procedure SetHttpAccount (value: string);
    function  GetHttpPassword : string;
    procedure SetHttpPassword (value: string);
    function  GetAttachBugReport : boolean;
    procedure SetAttachBugReport (value: boolean);
    function  GetAttachBugReportFile : boolean;
    procedure SetAttachBugReportFile (value: boolean);
    function  GetDeleteBugReportFile : boolean;
    procedure SetDeleteBugReportFile (value: boolean);
    function  GetBugReportSendAs : string;
    procedure SetBugReportSendAs (value: string);
    function  GetBugReportZip : string;
    procedure SetBugReportZip (value: string);
    function  GetScreenShotDepth : integer;
    procedure SetScreenShotDepth (value: integer);
    function  GetScreenShotAppOnly : boolean;
    procedure SetScreenShotAppOnly (value: boolean);
    function  GetScreenShotSendAs : string;
    procedure SetScreenShotSendAs (value: string);
    function  GetScreenShotZip : string;
    procedure SetScreenShotZip (value: string);
    function  GetAdditionalAttachments : IMEAttachments;
    function  GetMailFrom : string;
    procedure SetMailFrom (value: string);
    function  GetAdditionalFields : IMEFields;
    // SAVE SETTINGS
    function  GetBugReportFile : string;
    procedure SetBugReportFile (value: string);
    function  GetAppendBugReports : boolean;
    procedure SetAppendBugReports (value: boolean);
    function  GetBugReportFileSize : dword;
    procedure SetBugReportFileSize (value: dword);
    function  GetNoDupExcepts : boolean;
    procedure SetNoDupExcepts (value: boolean);
    function  GetNoDupFreezes : boolean;
    procedure SetNoDupFreezes (value: boolean);
    function  GetDupExceptDef : TMEDupDef;
    procedure SetDupExceptDef (value: TMEDupDef);
    function  GetDupFreezeDef : TMEDupDef;
    procedure SetDupFreezeDef (value: TMEDupDef);
    // BUG REPORT SETTINGS
    function  GetListThreads : boolean;
    procedure SetListThreads (value: boolean);
    function  GetShowCpuRegisters : boolean;
    procedure SetShowCpuRegisters (value: boolean);
    function  GetShowStackDump : boolean;
    procedure SetShowStackDump (value: boolean);
    function  GetShowDisAsm : boolean;
    procedure SetShowDisAsm (value: boolean);
    function  GetHideUglyItems : boolean;
    procedure SetHideUglyItems (value: boolean);
    function  GetShowRelativeAddrs : boolean;
    procedure SetShowRelativeAddrs (value: boolean);
    function  GetShowRelativeLines : boolean;
    procedure SetShowRelativeLines (value: boolean);
    function  GetFormatDisassembly : boolean;
    procedure SetFormatDisassembly (value: boolean);
    function  GetLimitDisassembly : integer;
    procedure SetLimitDisassembly (value: integer);
    function  GetPluginEnabled (plugin: string) : boolean;
    procedure SetPluginEnabled (plugin: string; value: boolean);
    function  VersionVar : string;
    // Assistant CREATOR
    function  GetAssistant (name: string) : INVAssistant;
    // CUSTOM STRINGS
    function  GetTitleBar : string;
    procedure SetTitleBar (value: string);
    function  GetExceptMsg : string;
    procedure SetExceptMsg (value: string);
    function  GetFrozenMsg : string;
    procedure SetFrozenMsg (value: string);
    function  GetBitFaultMsg : string;
    procedure SetBitFaultMsg (value: string);
    function      GetSendBtnCaption : string;
    function      GetSaveBtnCaption : string;
    function     GetPrintBtnCaption : string;
    function      GetShowBtnCaption : string;
    function  GetContinueBtnCaption : string;
    function   GetRestartBtnCaption : string;
    function     GetCloseBtnCaption : string;
    procedure     SetSendBtnCaption (value: string);
    procedure     SetSaveBtnCaption (value: string);
    procedure    SetPrintBtnCaption (value: string);
    procedure     SetShowBtnCaption (value: string);
    procedure SetContinueBtnCaption (value: string);
    procedure  SetRestartBtnCaption (value: string);
    procedure    SetCloseBtnCaption (value: string);
    function       GetOkBtnCaption : string;
    function  GetDetailsBtnCaption : string;
    procedure      SetOkBtnCaption (value: string);
    procedure SetDetailsBtnCaption (value: string);
    function  GetPleaseWaitTitle : string;
    function  GetPleaseWaitText  : string;
    procedure SetPleaseWaitTitle (value: string);
    procedure SetPleaseWaitText  (value: string);
    function  GetMailSubject : string;
    procedure SetMailSubject (value: string);
    function  GetMailBody : string;
    procedure SetMailBody (value: string);
    function  GetSendBoxTitle : string;
    procedure SetSendBoxTitle (value: string);
    function  GetPrepareAttachMsg : string;
    procedure SetPrepareAttachMsg (value: string);
    function  GetMxLookupMsg : string;
    procedure SetMxLookupMsg (value: string);
    function  GetConnectMsg : string;
    procedure SetConnectMsg (value: string);
    function  GetAuthMsg : string;
    procedure SetAuthMsg (value: string);
    function  GetSendMailMsg : string;
    procedure SetSendMailMsg (value: string);
    function  GetFieldsMsg : string;
    procedure SetFieldsMsg (value: string);
    function  GetSendAttachMsg : string;
    procedure SetSendAttachMsg (value: string);
    function  GetSendFinalizeMsg : string;
    procedure SetSendFinalizeMsg (value: string);
    function  GetSendFailureMsg : string;
    procedure SetSendFailureMsg (value: string);

    function  FindProp    (name: string; allowDefault, write: boolean; createTyp: TPropertyType) : TPMEProperty; virtual;
    function  GetPropBool (name: string) : boolean;
    function  GetPropInt  (name: string) : integer;
    function  GetPropStr  (name: string) : string;
    function  GetPropDw   (name: string) : dword;
    function  SetPropBool (name: string; value: boolean) : boolean;
    function  SetPropInt  (name: string; value: integer) : boolean;
    function  SetPropStr  (name: string; value: string ) : boolean;
    function  SetPropDw   (name: string; value: dword  ) : boolean;

    procedure Reload;

    procedure FillFormCache (assistant: INVAssistant);
    procedure ReadFormCache (assistant: INVAssistant);
    
    function  GetSelf : TIMESettings;
  end;

  // extended attachments access
  IMEAttachmentsEx = interface ['{1E8B96CB-8314-4C9E-A327-7CDA83A55982}']
    procedure ClearParent;
  end;

  // attachments class
  TIMEAttachments = class (TInterfacedObject, IMEAttachments, IMEAttachmentsEx)
    FParent    : TIMESettings;
    FItemCount : integer;
    FItems     : array of record orgFile, sendAs, zip, field: string end;
    FSection   : TRTLCriticalSection;
    procedure Lock;
    procedure Unlock;
    function  GetItemCount : integer;
    function  GetItem (index: integer;
                       var originalFile   : string;
                       var sendAsFileName : string;
                       var zipFile        : string;
                       var fieldName      : string) : boolean;
    procedure Add (originalFile   : string;
                   sendAsFileName : string = '';
                   zipFile        : string = '';
                   fieldName      : string = '');
    function  Delete (originalFile: string) : boolean;
    procedure Clear;
    function  Clone : IMEAttachments;
    procedure ClearParent;
    procedure Modified;
    constructor Create;
    destructor Destroy; override;
  end;

  // extended fields access
  IMEFieldsEx = interface ['{D9EED670-C965-403D-92E0-BF11D854911D}']
    procedure ClearExcParent;
    procedure ClearMesParent;
    procedure SetAddIndex(addIndex: integer);
    procedure AcceptData;
  end;

  // fields base class
  TIMEFields = class (TInterfacedObject, IMEFields, IMEFieldsEx)
    FExcParent  : TIMEException;
    FMesParent  : TIMESettings;
    FItemCount  : integer;
    FItems      : array of record item, content: string end;
    FAddIndex   : integer;
    FAcceptData : boolean;
    FSection    : TRTLCriticalSection;
    procedure Lock;
    procedure Unlock;
    function  GetItemCount : integer;
    function  GetItem (index: integer) : string;
    procedure SetItem (index: integer; value: string);
    function  FindItem (item: string) : integer;
    function  GetContent (item: string) : string;
    procedure SetContent (item: string; value: string);
    procedure Add    (                item, content: string);
    procedure Insert (index: integer; item, content: string);
    procedure Delete (index: integer                       ); overload;
    procedure Delete (                item         : string); overload;
    function  Clone : IMEFields;
    procedure ClearExcParent;
    procedure ClearMesParent;
    procedure SetAddIndex(addIndex: integer);
    procedure AcceptData;
    procedure Modified;
    constructor Create;
    destructor Destroy; override;
  end;

  // internally needed by "TIMEException" for background stack tracing
  TThreadStack = record
    threadName      : string;
    exceptAddr      : pointer;
    exceptThread    : boolean;
    extException    : boolean;
    stackBottom     : dword;
    stackTop        : dword;
    creatorAddr     : pointer;
    ebp             : dword;
    bcbTermination  : boolean;
    preparedStackP  : pointer;
    preparedStackDA : TDAPreStackItem;
    creatorTid      : dword;
  end;

  // needed for bug report callback storage
  TDABugReportCallback   = array of record callback: TBugReportCallback;   critical: boolean end;
  TDABugReportCallbackOO = array of record callback: TBugReportCallbackOO; critical: boolean end;

  // exception base class
  TIMEException = class (TIMESettings, IMEException, IMEExceptionEx)
    FModuleSettings        : IMEModuleSettings;
    FSettingsBuf           : TMESettingsModule;
    FPhase                 : TExceptPhase;
    FCanContinue           : boolean;
    FCanContinue2          : boolean;
    FExceptType            : TExceptType;
    FExceptObject          : TObject;
    FExceptAddr            : pointer;
    FCrashedThreadId       : dword;
    FCallstackCrc          : array [0..2] of dword;
    FCurrentEsp            : dword;
    FCurrentEbp            : dword;
    FContext               : TContext;
    FPContext              : PContext;
    FSource                : TExceptSource;
    FRelatedObject         : TObject;
    FPackage               : dword;
    FPreparedStack         : pointer;
    FBugReportHeader       : IMEFields;
    FBugReportSections     : IMEFields;
    FBugReport             : string;
    FComplete              : boolean;
    FScreenShot            : INVBitmap;
    FUpdating              : integer;
    FDirty                 : boolean;
    FBugReportCallbacks    : TDABugReportCallback;
    FBugReportCallbacksOO  : TDABugReportCallbackOO;
    FCreateBugReport       : boolean;
    FCreateScreenShot      : boolean;
    FCallHandlers          : boolean;
    FAppendScreenShot      : boolean;
    FShowSetting           : TMEShowSetting;
    FShowAssis             : string;
    FThreadStacks          : array of TThreadStack;
    FSection               : TRTLCriticalSection;
    FCorrectBugReportNo    : int64;
    FCompleteTh            : dword;
    FCompleteTid           : dword;
    FAbortCompleteThread   : boolean;
    FProgressAlert         : IProgressAlert;
    FMailWasSent           : boolean;
    FMailWasSaved          : boolean;
    FFirstCompleteCallback : integer;
    FFirstAutoSave         : integer;
    FSendThread            : dword;

    // STATE INFORMATION
    function  GetPhase : TExceptPhase;
    procedure SetPhase (value: TExceptPhase);
    function  GetCanContinue : boolean;
    procedure SetCanContinue (value: boolean);
    // EXCEPTION INFORMATION
    function  GetExceptType : TExceptType;
    function  GetExceptObject : TObject;
    function  GetExceptClass : string;
    function  GetExceptMessage : string;
    function  GetExceptAddr : pointer;
    function  GetCrashedThreadId : dword;
    function  GetCallstackCrc (index: integer) : dword;
    function  GetContext : PContext;
    function  GetSource : TExceptSource;
    function  GetRelatedObject : TObject;
    function  GetPackage : dword;
    function  GetBugReportHeader : IMEFields;
    function  GetBugReportSections : IMEFields;
    function  GetBugReport_ : string;
    procedure SetBugReport (value: string);
    function  GetBugReport (mustBeComplete: boolean = true) : string;
    function  GetScreenShot : INVBitmap;
    procedure SetScreenShot (value: INVBitmap);
    // CALLBACKS
    procedure BeginUpdate;
    procedure   EndUpdate;
    procedure   RegisterBugReportCallback (bugReportCallback: TBugReportCallback;   critical: boolean); overload;
    procedure   RegisterBugReportCallback (bugReportCallback: TBugReportCallbackOO; critical: boolean); overload;
    procedure UnregisterBugReportCallback (bugReportCallback: TBugReportCallback  ); overload;
    procedure UnregisterBugReportCallback (bugReportCallback: TBugReportCallbackOO); overload;
    // PRE PHASE SETTINGS - ONLY VALID DURING EXCEPTION HANDLER PRE PHASE
    function  GetCreateBugReport : boolean;
    procedure SetCreateBugReport (value: boolean);
    function  GetCreateScreenShot : boolean;
    procedure SetCreateScreenShot (value: boolean);
    function  GetCallHandlers : boolean;
    procedure SetCallHandlers (value: boolean);
    // NON-VISIBLE SETTINGS - VALID DURING ALL EXCEPTION HANDLER PHASES
    function  GetAppendScreenShot : boolean;
    procedure SetAppendScreenShot (value: boolean);
    // VISIBLE SETTINGS - VALID DURING ALL EXCEPTION HANDLER PHASES
    function  GetShowSetting : TMEShowSetting;
    procedure SetShowSetting (value: TMEShowSetting);
    function  GetShowAssistant : string;
    procedure SetShowAssistant (value: string);
    // ACTION
    procedure Show;

    // internal
    procedure SetModified;
    procedure FireChangeEvent;
    function  CompleteBugReport : integer; stdcall;
    function  GetCompleteThreadId : dword;
    function  CriticalBugReportCallbackExists : boolean;
    function  FindProp (name: string; allowDefault, write: boolean; createTyp: TPropertyType) : TPMEProperty; override;
    function  GetMailWasSent : boolean;
    procedure SetMailWasSent (value: boolean);
    function  GetMailWasSaved : boolean;
    procedure SetMailWasSaved (value: boolean);
    function  FirstCompleteCallback : boolean;
    function  FirstAutoSave : boolean;
    function  PSendThread : TPCardinal;
    constructor Create (module: dword);
    destructor Destroy; override;
  end;

// ***************************************************************
// synchronize the execution of registered exception handlers

type
  TFireHandlersParams = record
    eventType   : byte;
    action      : TExceptAction;
    exc         : IMEException;
    handled     : boolean;
    callSync    : boolean;  // call event handlers, which want to be synced
    syncFailed  : boolean;  // synchronization failed
  end;

var
  FireHandlersWnd   : dword = 0;
  FireHandlersMsg   : dword = 0;
  AmSyncingHandlers : boolean = false;

function FireHandlers(var fhp: TFireHandlersParams) : boolean;
var eh   : TDAExceptHandler;
    ehoo : TDAExceptHandlerOO;
    ah   : TDAActionHandler;
    ahoo : TDAActionHandlerOO;
    hh   : TDAExceptHandler;
    hhoo : TDAExceptHandlerOO;
    i1   : integer;
begin
  {$ifdef log}log('FireHandlers');{$endif}
  result := false;
  eh := nil; ehoo := nil;
  ah := nil; ahoo := nil;
  hh := nil; hhoo := nil;
  if GetCurrentThreadId = MainThreadId then
    AmSyncingHandlers := true;
  with fhp do begin
    case eventType of
      0 : begin
            if ExceptHandlers   <> nil then eh   := Copy(ExceptHandlers);
            if ExceptHandlersOO <> nil then ehoo := Copy(ExceptHandlersOO);
            for i1 := 0 to high(ehoo) do begin
              try
                if (exc.Phase = ehoo[i1].phase) and
                   (callSync = (ehoo[i1].sync > stDontSync)) and ((not syncFailed) or (ehoo[i1].sync = stTrySyncCallAlways)) then begin
                  result := true;
                  ehoo[i1].handler(exc, handled);
                end;
              except end;
              if (exc.Phase < epPostProcessing) and handled then break;
            end;
            if not handled then
              for i1 := 0 to high(eh) do begin
                try
                  if (exc.Phase = eh[i1].phase) and
                     (callSync = (eh[i1].sync > stDontSync)) and ((not syncFailed) or (eh[i1].sync = stTrySyncCallAlways)) then begin
                    result := true;
                    eh[i1].handler(exc, handled);
                  end;
                except end;
                if (exc.Phase < epPostProcessing) and handled then break;
              end;
            if exc.Phase >= epPostProcessing then
              handled := false;
          end;
      1 : begin
            if ActionHandlers   <> nil then ah   := Copy(ActionHandlers);
            if ActionHandlersOO <> nil then ahoo := Copy(ActionHandlersOO);
            for i1 := 0 to high(ahoo) do begin
              try
                if (callSync = (ahoo[i1].sync > stDontSync)) and ((not syncFailed) or (ahoo[i1].sync = stTrySyncCallAlways)) then begin
                  result := true;
                  ahoo[i1].handler(action, exc, handled);
                end;
              except end;
              if handled then break;
            end;
            if not handled then
              for i1 := 0 to high(ah) do begin
                try
                  if (callSync = (ah[i1].sync > stDontSync)) and ((not syncFailed) or (ah[i1].sync = stTrySyncCallAlways)) then begin
                    result := true;
                    ah[i1].handler(action, exc, handled);
                  end;
                except end;
                if handled then break;
              end;
          end;
      2 : begin
            if HiddenHandlers   <> nil then hh   := Copy(HiddenHandlers);
            if HiddenHandlersOO <> nil then hhoo := Copy(HiddenHandlersOO);
            for i1 := 0 to high(hhoo) do begin
              try
                if (callSync = (hhoo[i1].sync > stDontSync)) and ((not syncFailed) or (hhoo[i1].sync = stTrySyncCallAlways)) then begin
                  result := true;
                  hhoo[i1].handler(exc, handled);
                end;
              except end;
              if (exc.Phase < epPostProcessing) and handled then break;
            end;
            if (not handled) or (hhoo = nil) then
              for i1 := 0 to high(hh) do begin
                try
                  if (callSync = (hh[i1].sync > stDontSync)) and ((not syncFailed) or (hh[i1].sync = stTrySyncCallAlways)) then begin
                    result := true;
                    hh[i1].handler(exc, handled);
                  end;
                except end;
                if (exc.Phase < epPostProcessing) and handled then break;
              end;
            if exc.Phase >= epPostProcessing then
              handled := false;
          end;
    end;
  end;
  if GetCurrentThreadId = MainThreadId then
    AmSyncingHandlers := false;
end;

procedure ReceiveFireHandlers(window, msg: cardinal; wParam, lParam: integer; var result: integer);
begin
  {$ifdef log}log('ReceiveFireHandlers');{$endif}
  PauseMeEventually;
  if wParam = 0 then
    FireHandlers(TFireHandlersParams(pointer(lParam)^));
  result := 777;
end;

function ResumeMainThread: boolean; forward;
procedure SuspendMainThread; forward;

procedure PutAssisIntoBugReport(assis: INVAssistant; exc: IMEException);
var i1, i2 : integer;
    nve    : INVEdit;
    nvc    : INVCheckBox;
begin
  with assis do
    for i1 := 0 to FormCount - 1 do
      with Forms[i1] do
        if ModalResult = nvmOk then
          for i2 := 0 to ItemCount - 1 do
            if Items[i2].QueryInterface(INVEdit, nve) = 0 then begin
              if (nve.OutputName <> '') and (nve.Text <> '') then
                if (nve.OutputType = otOwnSection) and (nve.Lines > 1) then
                     exc.BugReportSections[nve.OutputName] := nve.Text
                else exc.BugReportHeader  [nve.OutputName] := nve.Text;
            end else
              if Items[i2].QueryInterface(INVCheckBox, nvc) = 0 then
                if nvc.OutputName <> '' then
                  if nvc.Checked then
                       exc.BugReportHeader[nvc.OutputName] := 'yes'
                  else exc.BugReportHeader[nvc.OutputName] := 'no';
end;

function CompletePhaseHandlerExists : boolean; forward;

procedure DoFireHandlers(parentWindow: dword; eventType: byte; action: TExceptAction;
                         exc: IMEException; var handled: boolean);

  function ContainsSyncHandler(thisType: integer; handlers: TDAExceptHandler; handlersOO: TDAExceptHandlerOO) : boolean;
  var i1 : integer;
  begin
    result := false;
    if (thisType = eventType) and (HandlerSection <> nil) then begin
      EnterCriticalSection(HandlerSection^);
      try
        for i1 := 0 to high(handlers) do
          if (handlers[i1].sync > stDontSync) and ((thisType <> 0) or (handlers[i1].phase = exc.Phase)) then begin
            result := true;
            exit;
          end;
        if not result then
          for i1 := 0 to high(handlersOO) do
            if (handlersOO[i1].sync > stDontSync) and ((thisType <> 0) or (handlersOO[i1].phase = exc.Phase)) then begin
              result := true;
              exit;
            end;
      finally LeaveCriticalSection(HandlerSection^) end;
    end;
  end;

var fhp    : TFireHandlersParams;
    c1     : dword;
    b1, b2 : boolean;
    s1     : string;
    assis  : INVAssistant;
    succ   : boolean;
begin
  {$ifdef log}log('DoFireHandlers, eventType: ' + IntToStrEx(dword(eventType)) + ', action: ' + IntToStrEx(ord(action)) + ', handled: ' + booleanToChar(handled));{$endif}
  if (eventType = 1) and (action in [eaRestartApplication, eaCloseApplication]) and
     ( (exc as IMEExceptionEx).CriticalBugReportCallbackExists or
       ( CompletePhaseHandlerExists and exc.CallHandlers )        ) then
    exc.BugReport;
  if exc.CallHandlers and
     ( ((eventType = 0) and ((ExceptHandlers <> nil) or (ExceptHandlersOO <> nil))) or
       ((eventType = 1) and ((ActionHandlers <> nil) or (ActionHandlersOO <> nil))) or
       ((eventType = 2) and ((HiddenHandlers <> nil) or (HiddenHandlersOO <> nil)))    ) then begin
    fhp.eventType       := eventType;
    fhp.action          := action;
    fhp.exc             := exc;
    fhp.handled         := handled;
    fhp.callSync        := false;
    fhp.syncFailed      := false;
    succ := FireHandlers(fhp);
    if ((not fhp.handled) or (not succ)) and (FireHandlersWnd <> 0) and
       ( ContainsSyncHandler(0,                  ExceptHandlers,                     ExceptHandlersOO)  or
         ContainsSyncHandler(1, TDAExceptHandler(ActionHandlers), TDAExceptHandlerOO(ActionHandlersOO)) or
         ContainsSyncHandler(2, TDAExceptHandler(HiddenHandlers), TDAExceptHandlerOO(HiddenHandlersOO))    ) then begin
      fhp.callSync := true;
      if exc.ExceptType <> etFrozen then begin
        b1 := exc.SuspendThreads and ResumeMainThread;
        b2 := (SendMessageTimeout(FireHandlersWnd, FireHandlersMsg, 1, integer(@fhp), SMTO_NORMAL, 1500,       c1) <> 0) and (c1 = 777) and
              (SendMessageTimeout(FireHandlersWnd, FireHandlersMsg, 0, integer(@fhp), SMTO_NORMAL, maxInt - 1, c1) <> 0) and (c1 = 777);
        if b1 then
          SuspendMainThread;
      end else
        b2 := false;
      if not b2 then begin
        // synchronization didn't work, so we call those handlers, which would
        // like to be synchronized, but nevertheless can live without it
        fhp.syncFailed := true;
        FireHandlers(fhp);
      end;
    end;
    handled := fhp.handled;
  end;
  if (not handled) and (eventType = 1) and (parentWindow <> 0) then begin
    case action of
      eaSendBugReport  : s1 := exc.SendAssistant;
      eaSaveBugReport  : s1 := exc.SaveAssistant;
      eaPrintBugReport : s1 := exc.PrintAssistant;
      else               s1 := '';
    end;
    if s1 <> '' then begin
      assis := exc.GetAssistant(s1);
      if assis <> nil then begin
        b1 := (parentWindow <> 0) and (GetWindowLong(parentWindow, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0);
        if b1 then
          SetWindowPos(parentWindow, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
        b2 := assis.ShowModal(parentWindow) = nvmOk;
        if b1 then
          SetWindowPos(parentWindow, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
        if b2 then PutAssisIntoBugReport(assis, exc)
        else       handled := true;
        (exc as IMESettingsEx).FillFormCache(assis);
      end;
    end;
  end;
  {$ifdef log}log('DoFireHandlers end, handled: ' + booleanToChar(handled));{$endif}
end;

procedure InitFireHandlers;
begin
  {$ifdef log}log('InitFireHandlers');{$endif}
  FireHandlersMsg := AddMsgHandler(ReceiveFireHandlers);
  FireHandlersWnd := MsgHandlerWindow;
end;

procedure CloseFireHandlers;
begin
  {$ifdef log}log('CloseFireHandlers');{$endif}
  DelMsgHandler(ReceiveFireHandlers);
end;

// ***************************************************************
// functions for "pause running delphi/bcb threads"

var
  SuspendThreadIds : TDACardinal;
  SuspendSection   : PRTLCriticalSection = nil;
  MainThreadSE     : dword = 0;  // main thread suspend event
  OtherThreadsSE   : dword = 0;

procedure InitMessageHooks; forward;

procedure SuspendDelphiThreads(suspend: boolean; dontSuspend: dword);
// prepare suspension of all threads which we want to have suspended
// we save them into the "SuspendThreadIds" array
// furthermore we prepare two events (main thread/other threads)
// the array and the events will later be used by "PauseMeEventually"

  function IsDelphiThread(tid: dword) : boolean;
  var ptn     : TPThreadName;
      creator : pointer;
      module  : dword;
      s1      : string;
  begin
    result := false;
    creator := nil;
    if (madExceptBuf <> nil) and
       (WaitForSingleObject(madExceptBuf^.threadNamesMutex, 50) = WAIT_OBJECT_0) then begin
      try
        ptn := madExceptBuf^.threadNamesList;
        while (ptn <> nil) and (ptn^.id <> tid) do
          ptn := ptn^.next;
        if (ptn <> nil) and (LocalSize(dword(ptn)) > 12) then
          creator := ptn^.creatorAddr;
      finally ReleaseMutex(madExceptBuf^.threadNamesMutex) end;
    end;
    if (creator <> nil) and FindModule(creator, module, s1) and
       (FindResource(module, 'EXCEPT', 'MAD') <> 0) then
      result := true;
  end;

var i1, i2 : integer;
    tl     : TDACardinal;
begin
  {$ifdef log}log('SuspendAllOtherThreads');{$endif}
  tl := nil;
  if (SuspendSection = nil) and suspend then begin
    New(SuspendSection);
    InitializeCriticalSection(SuspendSection^);
    InitMessageHooks;
  end;
  if SuspendSection <> nil then begin
    EnterCriticalSection(SuspendSection^);
    try
      SuspendThreadIds := nil;
      if suspend then begin
        if MainThreadSE = 0 then
             MainThreadSE := CreateEvent(nil, true, false, nil)
        else ResetEvent(MainThreadSE);
        if OtherThreadsSE = 0 then
             OtherThreadsSE := CreateEvent(nil, true, false, nil)
        else ResetEvent(OtherThreadsSE);
      end;
    finally LeaveCriticalSection(SuspendSection^) end;
    if suspend then begin
      tl := GetThreadList;
      for i1 := 0 to high(tl) do
        if (tl[i1] <> GetCurrentThreadId) and (tl[i1] <> MainThreadId) and
           (tl[i1] <> DontSuspend) and IsDelphiThread(tl[i1]) then begin
          EnterCriticalSection(SuspendSection^);
          try
            i2 := Length(SuspendThreadIds);
            SetLength(SuspendThreadIds, i2 + 1);
            SuspendThreadIds[i2] := tl[i1];
          finally LeaveCriticalSection(SuspendSection^) end;
        end;
    end else begin
      if MainThreadSE <> 0 then
        SetEvent(MainThreadSE);
      if OtherThreadsSE <> 0 then
        SetEvent(OtherThreadsSE);
    end;
  end;
  PostMessage(FireHandlersWnd, FireHandlersMsg, 1, 0);
end;

procedure PauseMeEventually;
// check whether the current thread is supposed to be suspended
// if yes, we wait until the suspension event gets signaled
var b1 : boolean;
    i1 : integer;
    le : dword;
begin
  le := GetLastError;
  if SuspendSection <> nil then
    if GetCurrentThreadId = MainThreadId then begin
      if MainThreadSE <> 0 then
        WaitForSingleObject(MainThreadSE, INFINITE);
    end else
      if OtherThreadsSE <> 0 then begin
        b1 := false;
        EnterCriticalSection(SuspendSection^);
        try
          for i1 := 0 to high(SuspendThreadIds) do
            if SuspendThreadIds[i1] = GetCurrentThreadId then begin
              b1 := true;
              break;
            end;
        finally LeaveCriticalSection(SuspendSection^) end;
        if b1 then
          WaitForSingleObject(OtherThreadsSE, INFINITE);
      end;
  SetLastError(le);
end;

function ResumeMainThread : boolean;
begin
  result := (MainThreadSE <> 0) and (WaitForSingleObject(MainThreadSE, 0) = WAIT_TIMEOUT);
  if result then
    SetEvent(MainThreadSE);
end;

procedure SuspendMainThread;
begin
  if MainThreadSE <> 0 then begin
    ResetEvent(MainThreadSE);
    PostMessage(FireHandlersWnd, FireHandlersMsg, 1, 0);
  end;
end;

// ***************************************************************
// this section checks for a frozen main thread

var AntiFreezeTid   : dword   = $FFFFFFFF;
    AntiFreezeTh    : dword   = 0;
    AntiFreezeWnd   : dword   = 0;
    AntiFreezeMsg   : dword   = 0;
    AntiFreezeCnt   : dword   = 0;
    AntiFreezePause : integer = -1;  // counter for PauseFreezeCheck
    AntiFreezeEvent : dword   = 0;   // event   for PauseFreezeCheck
    FreezeTimeout   : dword   = 60000;

procedure ReceiveAntiFreeze(window, msg: cardinal; wParam, lParam: integer; var result: integer);
begin
  {$ifdef log}log('ReceiveAntiFreeze');{$endif}
  if wParam <> 0 then
       PostMessage      (wParam,        WM_NULL, 777, 777)
  else PostThreadMessage(AntiFreezeTid, WM_NULL, 777, 777);
  result := 777;
end;

function AntiFreezeThread(dummy: pointer) : dword; stdcall;
var timeOut : dword;
    msg     : TMsg;
    b1      : boolean;
begin
  {$ifdef log}log('AntiFreezeThread');{$endif}
  WaitForSingleObject(madExceptBuf^.antiFreezeMutex, INFINITE);
  try
    timeOut := FreezeTimeout div 10;
    while true do begin
      if AntiFreezeEvent <> 0 then
        WaitForSingleObject(AntiFreezeEvent, INFINITE);
      PostMessage(AntiFreezeWnd, AntiFreezeMsg, 0, 0);
      Sleep(timeOut);
      b1 := true;
      while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do
        if (msg.hwnd = 0) and (msg.message = WM_NULL) and (msg.wParam = 777) and (msg.lParam = 777) then
          b1 := false;
      if b1 then begin
        inc(AntiFreezeCnt);
        if AntiFreezeCnt = 10 then begin
          HandleException(etFrozen, nil, nil, true, 0, 0, nil, esAntiFreezeCheck);
          AntiFreezeCnt := 0;
          timeOut := timeOut + timeOut div 2;
        end;
      end else begin
        AntiFreezeCnt := 0;
        timeOut := FreezeTimeout div 10;
      end;
    end;
  finally ReleaseMutex(madExceptBuf^.antiFreezeMutex) end;
end;

procedure InitAntiFreeze;
begin
  {$ifdef log}log('InitAntiFreeze, FreezeTimeout: ' + IntToStrEx(FreezeTimeout));{$endif}
  if AntiFreezeWnd = 0 then begin
    AntiFreezeWnd := MsgHandlerWindow;
    if AntiFreezeWnd <> 0 then begin
      AntiFreezeMsg := AddMsgHandler(ReceiveAntiFreeze);
      {$ifdef log}log('AntiFreezeWnd: ' + IntToStrEx(AntiFreezeWnd) +
                    '; AntiFreezeMsg: ' + IntToStrEx(AntiFreezeMsg));{$endif}
      AntiFreezeTh := CreateThread(nil, 0, @AntiFreezeThread, nil, 0, AntiFreezeTid);
      if AntiFreezeTh <> 0 then begin
        SetThreadInfo(AntiFreezeTid, AntiFreezeTh, '-', '', nil, 0);
        SetThreadPriority(AntiFreezeTh, THREAD_PRIORITY_HIGHEST);
      end;
    end;
  end;
end;

procedure CloseAntiFreeze;
begin
  {$ifdef log}log('CloseAntiFreeze, was installed: ' + booleanToChar(AntiFreezeWnd <> 0));{$endif}
  if AntiFreezeWnd <> 0 then begin
    if AntiFreezeEvent <> 0 then begin
      CloseHandle(AntiFreezeEvent);
      AntiFreezeEvent := 0;
    end;
    AntiFreezePause := -1;
    DelMsgHandler(ReceiveAntiFreeze);
    TerminateThread(AntiFreezeTh, 0);
    CloseHandle(AntiFreezeTh);
    SetThreadInfo(AntiFreezeTid, 0, '', '', nil, 0);
    AntiFreezeTh  := 0;
    AntiFreezeTid := 0;
    AntiFreezeWnd := 0;
  end;
end;

procedure PauseFreezeCheck(pause: boolean = true);
begin
  {$ifdef log}log('PauseFreezeCheck, pause: ' + booleanToChar(pause));{$endif}
  if (AntiFreezeTh <> 0) and (GetCurrentThreadId <> AntiFreezeTid) then
    if pause then begin
      AntiFreezeCnt := 0;
      if InterlockedIncrement(AntiFreezePause) = 0 then
        AntiFreezeEvent := CreateEvent(nil, true, false, nil);
    end else begin
      if (InterlockedDecrement(AntiFreezePause) < 0) and (AntiFreezeEvent <> 0) then begin
        SetEvent(AntiFreezeEvent);
        CloseHandle(AntiFreezeEvent);
        AntiFreezeEvent := 0;
      end;
      AntiFreezeCnt := 0;
    end;
end;

procedure ImNotFrozen;
begin
  AntiFreezeCnt := 0;
  PostThreadMessage(AntiFreezeTid, WM_NULL, 777, 777);
end;

procedure SetFreezeTimeout(newTimeOut: dword);
begin
  FreezeTimeout := newTimeOut * 1000;
  ImNotFrozen;
end;

// ***************************************************************
// predefined assistant on action handlers

procedure HandleContactForm(form: INVForm; action: TNVAction; item: INVItem; exceptIntf: IMEException);

  function CheckEMailAddress(email: string) : boolean;
  const CValidChars = ['a'..'z', 'A'..'Z', '0'..'9', '+', '-', '_'];
  var i1, i2 : integer;
      b1     : boolean;
  begin
    result := (email <> '') and (RetTrimStr(email) = email);
    b1 := false;
    i2 := -1;
    for i1 := 1 to Length(email) do
      if (i1 > 1) and (email[i1] = '@') then
        i2 := i1
      else
        if (i2 <> -1) and (email[i1] = '.') then
          b1 := true;
    result := result and b1 and
              (email[            1] <> '.') and
              (email[length(email)] <> '.') and
              (email[       i2 + 1] <> '.') and
              KillChars(email, CValidChars) and
              KillChar(email, '.') and
              (email = '@');
  end;

var s1, s2 : string;
    edit   : INVEdit;
    check  : INVCheckBox;
begin
  case action of
    nvaFormCreate : begin
                      s1 := RegReadStr(HKEY_CURRENT_USER, 'Software\madshi\ContactForm', 'RememberName');
                      s2 := RegReadStr(HKEY_CURRENT_USER, 'Software\madshi\ContactForm', 'RememberEmail');
                      if (s1 <> '') or (s2 <> '') then begin
                        edit := form.nvEdit('NameEdit');
                        if edit <> nil then
                          edit.Text := s1;
                        edit := form.nvEdit('EmailEdit');
                        if edit <> nil then
                          edit.Text := s2;
                        check := form.nvCheckBox('MemCheck');
                        if check <> nil then
                          check.Checked := true;
                        if form.ContinueButton.Enabled then
                          form.ActiveControl := form.ContinueButton.Name;
                      end;
                    end;
    nvaFormClose  : if form.ModalResult = nvmOk then begin
                      check := form.nvCheckBox('MemCheck');
                      if check <> nil then
                        if check.Checked then begin
                          edit := Form.nvEdit('NameEdit');
                          if edit <> nil then
                            RegWriteStr(HKEY_CURRENT_USER, 'Software\madshi\ContactForm', 'RememberName', edit.Text);
                          edit := Form.nvEdit('EmailEdit');
                          if edit <> nil then
                            RegWriteStr(HKEY_CURRENT_USER, 'Software\madshi\ContactForm', 'RememberEmail', edit.Text);
                        end else begin
                          RegDelVal(HKEY_CURRENT_USER, 'Software\madshi\ContactForm', 'RememberName');
                          RegDelVal(HKEY_CURRENT_USER, 'Software\madshi\ContactForm', 'RememberEmail');
                        end;
                      if exceptIntf <> nil then begin
                        edit := Form.nvEdit('NameEdit');
                        if edit <> nil then
                             s1 := edit.Text
                        else s1 := '';
                        edit := Form.nvEdit('EmailEdit');
                        if edit <> nil then
                             s2 := edit.Text
                        else s2 := '';
                        if s1 <> '' then
                          exceptIntf.MailFrom := s1 + ' ' + '<' + s2 + '>';
                      end;
                    end;
    nvaItemEvent  : if (item <> nil) and (item.Name = 'EmailEdit') then begin
                      edit := item as INVEdit;
                      edit.Valid := CheckEMailAddress(edit.Text);
                    end;
  end;
end;

procedure HandleScreenshotForm(form: INVForm; action: TNVAction; item: INVItem; exceptIntf: IMEException);
type
  TScreenShotEditRec = record
    time   : dword;  // screenShot.bmp file time
    editor : dword;  // bitmap editor process handle
  end;

  function GetFileTime(file_: string) : dword;
  var wfd : TWin32FindData;
      c1  : dword;
      date, time : word;
  begin
    c1 := FindFirstFile(pchar(file_), wfd);
    if c1 <> INVALID_HANDLE_VALUE then begin
      windows.FindClose(c1);
      FileTimeToDosDateTime(wfd.ftLastWriteTime, date, time);
      result := dword(date) shl 16 + dword(time);
    end else
      result := 0;
  end;

  function GetScreenShotFile(var file_: string) : boolean;
  begin
    file_ := GetTempPath;
    result := file_ <> '';
    if result then
      file_ := file_ + 'screenShot.bmp';
  end;

var s1    : string;
    img   : INVImage;
    check : INVCheckBox;
    bmp   : INVBitmap;
    sei   : TShellExecuteInfo;
    ser   : ^TScreenShotEditRec;
begin
  if exceptIntf <> nil then
    case action of
      nvaFormCreate : begin
                        img := form.nvImage('ScrShotImg');
                        if img <> nil then
                          img.Bitmap := exceptIntf.ScreenShot;
                        check := form.nvCheckBox('AttachCheck');
                        if check <> nil then
                          check.Checked := exceptIntf.AppendScreenShot;
                      end;
      nvaItemEvent  : if GetScreenShotFile(s1) then begin
                        ser := form.Data;
                        if item = nil then begin
                          // Timer.OnTimer
                          if (ser <> nil) and (ser.time <> GetFileTime(s1)) then begin
                            img := form.nvImage('ScrShotImg');
                            if img <> nil then begin
                              bmp := LoadBitmap(s1);
                              if bmp <> nil then begin
                                ser.time := GetFileTime(s1);
                                exceptIntf.ScreenShot := bmp;
                                img.Bitmap := exceptIntf.ScreenShot;
                              end;
                            end;
                          end;
                        end else
                          if  (item.QueryInterface(INVImage, img) = 0) and (img.Bitmap <> nil) then begin
                            // ScrShotImg.OnClick
                            if (ser = nil) and
                               (exceptIntf.ScreenShot <> nil) and
                               exceptIntf.ScreenShot.SaveBmp(s1) then begin
                              // first OnClick event, screen shotting available
                              New(ser);
                              ser.editor := 0;
                              ser.time := GetFileTime(s1);
                              form.Data := ser;
                            end;
                            if ser <> nil then begin
                              if (ser.editor <> 0) and (WaitForSingleObject(ser.editor, 0) = WAIT_OBJECT_0) then begin
                                CloseHandle(ser.editor);
                                ser.editor := 0;
                              end;
                              if ser.editor = 0 then begin
                                ZeroMemory(@sei, sizeOf(sei));
                                sei.cbSize := sizeOf(sei);
                                sei.fMask := SEE_MASK_NOCLOSEPROCESS;
                                sei.nShow := SW_SHOWMAXIMIZED;
                                sei.lpVerb := 'edit';
                                sei.lpFile := pchar(s1);
                                if ShellExecuteEx(@sei) then
                                  ser.editor := sei.hProcess
                                else
                                  windows.beep(60, 60);
                              end;
                            end;
                          end;
                      end;
      nvaFormClose  : begin
                        ser := form.Data;
                        if ser <> nil then begin
                          form.Data := nil;
                          if ser.editor <> 0 then
                            CloseHandle(ser.editor);
                          Dispose(ser);
                          if GetScreenShotFile(s1) then
                            DeleteFile(pchar(s1));
                        end;
                        check := form.nvCheckBox('AttachCheck');
                        if check <> nil then
                          exceptIntf.AppendScreenShot := check.Checked;
                      end;
    end;
end;

// ***************************************************************
// settings class implementation

function TIMESettings.IsValid                : boolean;        begin result :=                             FValid                end;
function TIMESettings.Module                 : dword;          begin result :=                             FModule               end;
function TIMESettings.Enabled                : boolean;        begin result :=                GetPropBool ('Enabled'          )  end;
function TIMESettings.MinDebugInfoOnly       : boolean;        begin result :=                GetPropStr  ('MinDebugInfo'     ) <> '' end;
function TIMESettings.NoOwnSettings          : boolean;        begin result :=                GetPropBool ('NoSettings'       )  end;
function TIMESettings.CheckFileCrc           : boolean;        begin result :=                GetPropBool ('CheckFileCrc'     )  end;
function TIMESettings.CheckForFreeze         : boolean;        begin result :=                GetPropBool ('CheckFreeze'      )  end;
function TIMESettings.FreezeTimeout          : dword;          begin result :=                GetPropDw   ('FreezeTimeout'    )  end;
function TIMESettings.GetAutoSave            : boolean;        begin result :=                GetPropBool ('AutoSave'         )  end;
function TIMESettings.GetAutoSaveIfNotSent   : boolean;        begin result :=                GetPropBool ('AutoSaveIfNotSent')  end;
function TIMESettings.GetAutoSend            : boolean;        begin result :=                GetPropBool ('AutoSend'         )  end;
function TIMESettings.GetAutoSendPrgrBox     : boolean;        begin result :=                GetPropBool ('AutoSendBox'      )  end;
function TIMESettings.GetAutoClipboard       : boolean;        begin result :=                GetPropBool ('AutoClip'         )  end;
function TIMESettings.GetSuspendThreads      : boolean;        begin result :=                GetPropBool ('PauseThreads'     )  end;
function TIMESettings.GetShowPleaseWaitBox   : boolean;        begin result :=                GetPropBool ('PlWaitBox'        )  end;
function TIMESettings.GetAutoContinue        : boolean;        begin result :=                GetPropBool ('AutoContinue'     )  end;
function TIMESettings.GetAutoRestart         : dword;          begin result :=                GetPropDw   ('AutoRestart'      )  end;
function TIMESettings.GetAutoClose           : dword;          begin result :=                GetPropDw   ('AutoClose'        )  end;
function TIMESettings.GetAutoDelay           : dword;          begin result :=                GetPropDw   ('AutoDelay'        )  end;
function TIMESettings.GetFilter1Classes      : string;         begin result :=                GetPropStr  ('F1Classes'        )  end;
function TIMESettings.GetFilter2Classes      : string;         begin result :=                GetPropStr  ('F2Classes'        )  end;
function TIMESettings.GetFilter1NoBugReport  : boolean;        begin result :=                GetPropBool ('F1NoBugRep'       )  end;
function TIMESettings.GetFilter2NoBugReport  : boolean;        begin result :=                GetPropBool ('F2NoBugRep'       )  end;
function TIMESettings.GetGeneralNoBugReport  : boolean;        begin result :=                GetPropBool ('GnNoBugRep'       )  end;
function TIMESettings.GetFilter1NoScreenShot : boolean;        begin result :=                GetPropBool ('F1NoScrShot'      )  end;
function TIMESettings.GetFilter2NoScreenShot : boolean;        begin result :=                GetPropBool ('F2NoScrShot'      )  end;
function TIMESettings.GetGeneralNoScreenShot : boolean;        begin result :=                GetPropBool ('GnNoScrShot'      )  end;
function TIMESettings.GetFilter1NoSuspend    : boolean;        begin result :=                GetPropBool ('F1NoSuspend'      )  end;
function TIMESettings.GetFilter2NoSuspend    : boolean;        begin result :=                GetPropBool ('F2NoSuspend'      )  end;
function TIMESettings.GetGeneralNoSuspend    : boolean;        begin result :=                GetPropBool ('GnNoSuspend'      )  end;
function TIMESettings.GetFilter1NoHandlers   : boolean;        begin result :=                GetPropBool ('F1NoHandlers'     )  end;
function TIMESettings.GetFilter2NoHandlers   : boolean;        begin result :=                GetPropBool ('F2NoHandlers'     )  end;
function TIMESettings.GetGeneralNoHandlers   : boolean;        begin result :=                GetPropBool ('GnNoHandlers'     )  end;
function TIMESettings.GetFilter1ShowSetting  : TMEShowSetting; begin result := TMEShowSetting(GetPropDw   ('F1ShowCfg'        )) end;
function TIMESettings.GetFilter2ShowSetting  : TMEShowSetting; begin result := TMEShowSetting(GetPropDw   ('F2ShowCfg'        )) end;
function TIMESettings.GetGeneralShowSetting  : TMEShowSetting; begin result := TMEShowSetting(GetPropDw   ('GnShowCfg'        )) end;
function TIMESettings.GetFilter1Assistant    : string;         begin result :=                GetPropStr  ('F1Assis'          )  end;
function TIMESettings.GetFilter2Assistant    : string;         begin result :=                GetPropStr  ('F2Assis'          )  end;
function TIMESettings.GetGeneralAssistant    : string;         begin result :=                GetPropStr  ('GnAssis'          )  end;
function TIMESettings.GetSendBtnVisible      : boolean;        begin result :=                GetPropBool ('SendBtnVis'       )  end;
function TIMESettings.GetSaveBtnVisible      : boolean;        begin result :=                GetPropBool ('SaveBtnVis'       )  end;
function TIMESettings.GetPrintBtnVisible     : boolean;        begin result :=                GetPropBool ('PrintBtnVis'      )  end;
function TIMESettings.GetShowBtnVisible      : boolean;        begin result :=                GetPropBool ('ShowBtnVis'       )  end;
function TIMESettings.GetContinueBtnVisible  : boolean;        begin result :=                GetPropBool ('ContinueBtnVis'   )  end;
function TIMESettings.GetRestartBtnVisible   : boolean;        begin result :=                GetPropBool ('RestartBtnVis'    )  end;
function TIMESettings.GetCloseBtnVisible     : boolean;        begin result :=                GetPropBool ('CloseBtnVis'      )  end;
function TIMESettings.GetFocusedButton       : TMEButton;      begin result :=      TMEButton(GetPropDw   ('FocusedBtn'       )) end;
function TIMESettings.GetSendAssistant       : string;         begin result :=                GetPropStr  ('SendAssis'        )  end;
function TIMESettings.GetSaveAssistant       : string;         begin result :=                GetPropStr  ('SaveAssis'        )  end;
function TIMESettings.GetPrintAssistant      : string;         begin result :=                GetPropStr  ('PrintAssis'       )  end;
function TIMESettings.GetAutoShowBugReport   : boolean;        begin result :=                GetPropBool ('AutoShowBugRep'   )  end;
function TIMESettings.GetNoOwnerDrawButtons  : boolean;        begin result :=                GetPropBool ('UglyBtns'         )  end;
function TIMESettings.GetMailAddr            : string;         begin result :=                GetPropStr  ('MailAddr'         )  end;
function TIMESettings.GetSendInBackground    : boolean;        begin result :=                GetPropBool ('SendInBackgr'     )  end;
function TIMESettings.GetMailAsSmtpServer    : boolean;        begin result :=                GetPropBool ('MailAsSmtpServer' )  end;
function TIMESettings.GetMailAsSmtpClient    : boolean;        begin result :=                GetPropBool ('MailAsSmtpClient' )  end;
function TIMESettings.GetUploadViaHttp       : boolean;        begin result :=                GetPropBool ('UploadViaHttp'    )  end;
function TIMESettings.GetMailViaMapi         : boolean;        begin result :=                GetPropBool ('MailViaMapi'      )  end;
function TIMESettings.GetMailViaMailto       : boolean;        begin result :=                GetPropBool ('MailViaMailto'    )  end;
function TIMESettings.GetSmtpServer          : string;         begin result :=                GetPropStr  ('SmtpServer'       )  end;
function TIMESettings.GetSmtpPort            : dword;          begin result :=                GetPropDw   ('SmtpPort'         )  end;
function TIMESettings.GetSmtpAccount         : string;         begin result :=                GetPropStr  ('SmtpAccount'      )  end;
function TIMESettings.GetSmtpPassword        : string;         begin result :=                GetPropStr  ('SmtpPassword'     )  end;
function TIMESettings.GetHttpServer          : string;         begin result :=                GetPropStr  ('HttpServer'       )  end;
function TIMESettings.GetHttpPort            : dword;          begin result :=                GetPropDw   ('HttpPort'         )  end;
function TIMESettings.GetHttpAccount         : string;         begin result :=                GetPropStr  ('HttpAccount'      )  end;
function TIMESettings.GetHttpPassword        : string;         begin result :=                GetPropStr  ('HttpPassword'     )  end;
function TIMESettings.GetAttachBugReport     : boolean;        begin result :=                GetPropBool ('AttachBugRep'     )  end;
function TIMESettings.GetAttachBugReportFile : boolean;        begin result :=                GetPropBool ('AttachBugRepFile' )  end;
function TIMESettings.GetDeleteBugReportFile : boolean;        begin result :=                GetPropBool ('DelBugRepFile'    )  end;
function TIMESettings.GetBugReportSendAs     : string;         begin result :=                GetPropStr  ('BugRepSendAs'     )  end;
function TIMESettings.GetBugReportZip        : string;         begin result :=                GetPropStr  ('BugRepZip'        )  end;
function TIMESettings.GetScreenShotDepth     : integer;        begin result :=                GetPropInt  ('ScrShotDepth'     )  end;
function TIMESettings.GetScreenShotAppOnly   : boolean;        begin result :=                GetPropBool ('ScrShotAppOnly'   )  end;
function TIMESettings.GetScreenShotSendAs    : string;         begin result :=                GetPropStr  ('ScrShotSendAs'    )  end;
function TIMESettings.GetScreenShotZip       : string;         begin result :=                GetPropStr  ('ScrShotZip'       )  end;
function TIMESettings.GetMailFrom            : string;         begin result :=                GetPropStr  ('MailFrom'         )  end;
function TIMESettings.GetBugReportFile       : string;         begin result :=                GetPropStr  ('BugRepFile'       )  end;
function TIMESettings.GetAppendBugReports    : boolean;        begin result :=                GetPropBool ('AppendBugReps'    )  end;
function TIMESettings.GetBugReportFileSize   : dword;          begin result :=                GetPropDw   ('BugRepFileSize'   )  end;
function TIMESettings.GetNoDupExcepts        : boolean;        begin result :=                GetPropBool ('NoDupExcepts'     )  end;
function TIMESettings.GetNoDupFreezes        : boolean;        begin result :=                GetPropBool ('NoDupFreezes'     )  end;
function TIMESettings.GetDupExceptDef        : TMEDupDef;      begin result :=      TMEDupDef(GetPropDw   ('DupExceptDef'     )) end;
function TIMESettings.GetDupFreezeDef        : TMEDupDef;      begin result :=      TMEDupDef(GetPropDw   ('DupFreezeDef'     )) end;
function TIMESettings.GetListThreads         : boolean;        begin result :=                GetPropBool ('ListThreads'      )  end;
function TIMESettings.GetShowCpuRegisters    : boolean;        begin result :=                GetPropBool ('CpuRegs'          )  end;
function TIMESettings.GetShowStackDump       : boolean;        begin result :=                GetPropBool ('StackDump'        )  end;
function TIMESettings.GetShowDisAsm          : boolean;        begin result :=                GetPropBool ('ShowDisAsm'       )  end;
function TIMESettings.GetHideUglyItems       : boolean;        begin result :=                GetPropBool ('HideUglyItems'    )  end;
function TIMESettings.GetShowRelativeAddrs   : boolean;        begin result :=                GetPropBool ('ShowRelAddrs'     )  end;
function TIMESettings.GetShowRelativeLines   : boolean;        begin result :=                GetPropBool ('ShowRelLines'     )  end;
function TIMESettings.GetFormatDisassembly   : boolean;        begin result :=                GetPropBool ('FormatDisAsm'     )  end;
function TIMESettings.GetLimitDisassembly    : integer;        begin result :=                GetPropInt  ('LimitDisAsm'      )  end;
function TIMESettings.VersionVar             : string;         begin result :=                             FVersionVar           end;
function TIMESettings.GetTitleBar            : string;         begin result :=                GetPropStr  ('TitleBar'         )  end;
function TIMESettings.GetExceptMsg           : string;         begin result :=                GetPropStr  ('ExceptMsg'        )  end;
function TIMESettings.GetFrozenMsg           : string;         begin result :=                GetPropStr  ('FrozenMsg'        )  end;
function TIMESettings.GetBitFaultMsg         : string;         begin result :=                GetPropStr  ('BitFaultMsg'      )  end;
function TIMESettings.GetSendBtnCaption      : string;         begin result :=                GetPropStr  ('SendBtnTxt'       )  end;
function TIMESettings.GetSaveBtnCaption      : string;         begin result :=                GetPropStr  ('SaveBtnTxt'       )  end;
function TIMESettings.GetPrintBtnCaption     : string;         begin result :=                GetPropStr  ('PrintBtnTxt'      )  end;
function TIMESettings.GetShowBtnCaption      : string;         begin result :=                GetPropStr  ('ShowBtnTxt'       )  end;
function TIMESettings.GetContinueBtnCaption  : string;         begin result :=                GetPropStr  ('ContinueBtnTxt'   )  end;
function TIMESettings.GetRestartBtnCaption   : string;         begin result :=                GetPropStr  ('RestartBtnTxt'    )  end;
function TIMESettings.GetCloseBtnCaption     : string;         begin result :=                GetPropStr  ('CloseBtnTxt'      )  end;
function TIMESettings.GetOkBtnCaption        : string;         begin result :=                GetPropStr  ('OkBtnTxt'         )  end;
function TIMESettings.GetDetailsBtnCaption   : string;         begin result :=                GetPropStr  ('DetailsBtnTxt'    )  end;
function TIMESettings.GetPleaseWaitTitle     : string;         begin result :=                GetPropStr  ('PlWaitTitle'      )  end;
function TIMESettings.GetPleaseWaitText      : string;         begin result :=                GetPropStr  ('PlWaitText'       )  end;
function TIMESettings.GetMailSubject         : string;         begin result :=                GetPropStr  ('MailSubj'         )  end;
function TIMESettings.GetMailBody            : string;         begin result :=                GetPropStr  ('MailBody'         )  end;
function TIMESettings.GetSendBoxTitle        : string;         begin result :=                GetPropStr  ('SendBoxTitle'     )  end;
function TIMESettings.GetPrepareAttachMSg    : string;         begin result :=                GetPropStr  ('PrepAttMsg'       )  end;
function TIMESettings.GetMxLookupMsg         : string;         begin result :=                GetPropStr  ('MxLookMsg'        )  end;
function TIMESettings.GetConnectMsg          : string;         begin result :=                GetPropStr  ('ConnMsg'          )  end;
function TIMESettings.GetAuthMsg             : string;         begin result :=                GetPropStr  ('AuthMsg'          )  end;
function TIMESettings.GetSendMailMsg         : string;         begin result :=                GetPropStr  ('SendMailMsg'      )  end;
function TIMESettings.GetFieldsMsg           : string;         begin result :=                GetPropStr  ('FieldMsg'         )  end;
function TIMESettings.GetSendAttachMsg       : string;         begin result :=                GetPropStr  ('SendAttMsg'       )  end;
function TIMESettings.GetSendFinalizeMsg     : string;         begin result :=                GetPropStr  ('SendFinalMsg'     )  end;
function TIMESettings.GetSendFailureMsg      : string;         begin result :=                GetPropStr  ('SendFailMsg'      )  end;

function TIMESettings.GetAdditionalAttachments : IMEAttachments;
var s1, s2 : string;
    i1     : integer;
    mea    : TIMEAttachments;
begin
  if FAddAttachs = nil then begin
    mea := TIMEAttachments.Create;
    FAddAttachs := mea;
    s1 := GetPropStr('AddAttachs');
    for i1 := 1 to SubStrCount(s1) do begin
      s2 := SubStr(s1, i1);
      FAddAttachs.Add(SubStr(s2, 1, '>'), SubStr(s2, 2, '>'), SubStr(s2, 3, '>'), SubStr(s2, 4, '>'));
    end;
    mea.FParent := self;
  end;
  result := FAddAttachs;
end;

function TIMESettings.GetAdditionalFields : IMEFields;
var s1, s2 : string;
    i1     : integer;
    mef    : TIMEFields;
begin
  if FAddFields = nil then begin
    mef := TIMEFields.Create;
    mef.AcceptData;
    FAddFields := mef;
    s1 := GetPropStr('AddFields');
    for i1 := 1 to SubStrCount(s1) do begin
      s2 := SubStr(s1, i1);
      FAddFields[SubStr(s2, 1, '=')] := SubStr(s2, 2, '=');
    end;
    mef.FMesParent := self;
  end;
  result := FAddFields;
end;

function TIMESettings.GetPluginEnabled(plugin: string) : boolean;
begin
  result := SubTextExists(GetPropStr('Plugins'), plugin);
end;

procedure TIMESettings.SetPluginEnabled(plugin: string; value: boolean);
var s1 : string;
begin
  s1 := GetPropStr('Plugins');
  if SubTextExists(s1, plugin) <> value then
    if value then begin
      if s1 = '' then
           SetPropStr('Plugins', plugin)
      else SetPropStr('Plugins', s1 + '|' + plugin);
    end else begin
      s1 := '|' + s1 + '|';
      ReplaceText(s1, '|' + plugin + '|', '|', true);
      Delete(s1, Length(s1), 1);
      Delete(s1, 1, 1);
      SetPropStr('Plugins', s1);
    end;
end;

procedure TIMESettings.SetAutoSave               (value: boolean       ); begin  SetPropBool ('AutoSave',               value ) end;
procedure TIMESettings.SetAutoSaveIfNotSent      (value: boolean       ); begin  SetPropBool ('AutoSaveIfNotSent',      value ) end;
procedure TIMESettings.SetAutoSend               (value: boolean       ); begin  SetPropBool ('AutoSend',               value ) end;
procedure TIMESettings.SetAutoSendPrgrBox        (value: boolean       ); begin  SetPropBool ('AutoSendBox',            value ) end;
procedure TIMESettings.SetAutoClipboard          (value: boolean       ); begin  SetPropBool ('AutoClip',               value ) end;
procedure TIMESettings.SetSuspendThreads         (value: boolean       ); begin  SetPropBool ('PauseThreads',           value ) end;
procedure TIMESettings.SetShowPleaseWaitBox      (value: boolean       ); begin  SetPropBool ('PlWaitBox',              value ) end;
procedure TIMESettings.SetAutoContinue           (value: boolean       ); begin  SetPropBool ('AutoContinue',           value ) end;
procedure TIMESettings.SetAutoRestart            (value: dword         ); begin  SetPropDw   ('AutoRestart',            value ) end;
procedure TIMESettings.SetAutoClose              (value: dword         ); begin  SetPropDw   ('AutoClose',              value ) end;
procedure TIMESettings.SetAutoDelay              (value: dword         ); begin  SetPropDw   ('AutoDelay',              value ) end;
procedure TIMESettings.SetFilter1Classes         (value: string        ); begin  SetPropStr  ('F1Classes',              value ) end;
procedure TIMESettings.SetFilter2Classes         (value: string        ); begin  SetPropStr  ('F2Classes',              value ) end;
procedure TIMESettings.SetFilter1NoBugReport     (value: boolean       ); begin  SetPropBool ('F1NoBugRep',             value ) end;
procedure TIMESettings.SetFilter2NoBugReport     (value: boolean       ); begin  SetPropBool ('F2NoBugRep',             value ) end;
procedure TIMESettings.SetGeneralNoBugReport     (value: boolean       ); begin  SetPropBool ('GnNoBugRep',             value ) end;
procedure TIMESettings.SetFilter1NoScreenShot    (value: boolean       ); begin  SetPropBool ('F1NoScrShot',            value ) end;
procedure TIMESettings.SetFilter2NoScreenShot    (value: boolean       ); begin  SetPropBool ('F2NoScrShot',            value ) end;
procedure TIMESettings.SetGeneralNoScreenShot    (value: boolean       ); begin  SetPropBool ('GnNoScrShot',            value ) end;
procedure TIMESettings.SetFilter1NoSuspend       (value: boolean       ); begin  SetPropBool ('F1NoSuspend',            value ) end;
procedure TIMESettings.SetFilter2NoSuspend       (value: boolean       ); begin  SetPropBool ('F2NoSuspend',            value ) end;
procedure TIMESettings.SetGeneralNoSuspend       (value: boolean       ); begin  SetPropBool ('GnNoSuspend',            value ) end;
procedure TIMESettings.SetFilter1NoHandlers      (value: boolean       ); begin  SetPropBool ('F1NoHandlers',           value ) end;
procedure TIMESettings.SetFilter2NoHandlers      (value: boolean       ); begin  SetPropBool ('F2NoHandlers',           value ) end;
procedure TIMESettings.SetGeneralNoHandlers      (value: boolean       ); begin  SetPropBool ('GnNoHandlers',           value ) end;
procedure TIMESettings.SetFilter1ShowSetting     (value: TMEShowSetting); begin  SetPropDw   ('F1ShowCfg',        dword(value)) end;
procedure TIMESettings.SetFilter2ShowSetting     (value: TMEShowSetting); begin  SetPropDw   ('F2ShowCfg',        dword(value)) end;
procedure TIMESettings.SetGeneralShowSetting     (value: TMEShowSetting); begin  SetPropDw   ('GnShowCfg',        dword(value)) end;
procedure TIMESettings.SetFilter1Assistant       (value: string        ); begin  SetPropStr  ('F1Assis',                value ) end;
procedure TIMESettings.SetFilter2Assistant       (value: string        ); begin  SetPropStr  ('F2Assis',                value ) end;
procedure TIMESettings.SetGeneralAssistant       (value: string        ); begin  SetPropStr  ('GnAssis',                value ) end;
procedure TIMESettings.SetSendBtnVisible         (value: boolean       ); begin  SetPropBool ('SendBtnVis',             value ) end;
procedure TIMESettings.SetSaveBtnVisible         (value: boolean       ); begin  SetPropBool ('SaveBtnVis',             value ) end;
procedure TIMESettings.SetPrintBtnVisible        (value: boolean       ); begin  SetPropBool ('PrintBtnVis',            value ) end;
procedure TIMESettings.SetShowBtnVisible         (value: boolean       ); begin  SetPropBool ('ShowBtnVis',             value ) end;
procedure TIMESettings.SetContinueBtnVisible     (value: boolean       ); begin  SetPropBool ('ContinueBtnVis',         value ) end;
procedure TIMESettings.SetRestartBtnVisible      (value: boolean       ); begin  SetPropBool ('RestartBtnVis',          value ) end;
procedure TIMESettings.SetCloseBtnVisible        (value: boolean       ); begin  SetPropBool ('CloseBtnVis',            value ) end;
procedure TIMESettings.SetFocusedButton          (value: TMEButton     ); begin  SetPropDw   ('FocusedBtn',       dword(value)) end;
procedure TIMESettings.SetSendAssistant          (value: string        ); begin  SetPropStr  ('SendAssis',              value ) end;
procedure TIMESettings.SetSaveAssistant          (value: string        ); begin  SetPropStr  ('SaveAssis',              value ) end;
procedure TIMESettings.SetPrintAssistant         (value: string        ); begin  SetPropStr  ('PrintAssis',             value ) end;
procedure TIMESettings.SetAutoShowBugReport      (value: boolean       ); begin  SetPropBool ('AutoShowBugRep',         value ) end;
procedure TIMESettings.SetNoOwnerDrawButtons     (value: boolean       ); begin  SetPropBool ('UglyBtns',               value ) end;
procedure TIMESettings.SetMailAddr               (value: string        ); begin  SetPropStr  ('MailAddr',               value ) end;
procedure TIMESettings.SetSendInBackground       (value: boolean       ); begin  SetPropBool ('SendInBackgr',           value ) end;
procedure TIMESettings.SetMailAsSmtpServer       (value: boolean       ); begin  SetPropBool ('MailAsSmtpServer',       value ) end;
procedure TIMESettings.SetMailAsSmtpClient       (value: boolean       ); begin  SetPropBool ('MailAsSmtpClient',       value ) end;
procedure TIMESettings.SetUploadViaHttp          (value: boolean       ); begin  SetPropBool ('UploadViaHttp',          value ) end;
procedure TIMESettings.SetMailViaMapi            (value: boolean       ); begin  SetPropBool ('MailViaMapi',            value ) end;
procedure TIMESettings.SetMailViaMailto          (value: boolean       ); begin  SetPropBool ('MailViaMailto',          value ) end;
procedure TIMESettings.SetSmtpServer             (value: string        ); begin  SetPropStr  ('SmtpServer',             value ) end;
procedure TIMESettings.SetSmtpPort               (value: dword         ); begin  SetPropDw   ('SmtpPort',               value ) end;
procedure TIMESettings.SetSmtpAccount            (value: string        ); begin  SetPropStr  ('SmtpAccount',            value ) end;
procedure TIMESettings.SetSmtpPassword           (value: string        ); begin  SetPropStr  ('SmtpPassword',           value ) end;
procedure TIMESettings.SetHttpServer             (value: string        ); begin  SetPropStr  ('HttpServer',             value ) end;
procedure TIMESettings.SetHttpPort               (value: dword         ); begin  SetPropDw   ('HttpPort',               value ) end;
procedure TIMESettings.SetHttpAccount            (value: string        ); begin  SetPropStr  ('HttpAccount',            value ) end;
procedure TIMESettings.SetHttpPassword           (value: string        ); begin  SetPropStr  ('HttpPassword',           value ) end;
procedure TIMESettings.SetAttachBugReport        (value: boolean       ); begin  SetPropBool ('AttachBugRep',           value ) end;
procedure TIMESettings.SetAttachBugReportFile    (value: boolean       ); begin  SetPropBool ('AttachBugRepFile',       value ) end;
procedure TIMESettings.SetDeleteBugReportFile    (value: boolean       ); begin  SetPropBool ('DelBugRepFile',          value ) end;
procedure TIMESettings.SetBugReportSendAs        (value: string        ); begin  SetPropStr  ('BugRepSendAs',           value ) end;
procedure TIMESettings.SetBugReportZip           (value: string        ); begin  SetPropStr  ('BugRepZip',              value ) end;
procedure TIMESettings.SetScreenShotDepth        (value: integer       ); begin  SetPropInt  ('ScrShotDepth',           value ) end;
procedure TIMESettings.SetScreenShotAppOnly      (value: boolean       ); begin  SetPropBool ('ScrShotAppOnly',         value ) end;
procedure TIMESettings.SetScreenShotSendAs       (value: string        ); begin  SetPropStr  ('ScrShotSendAs',          value ) end;
procedure TIMESettings.SetScreenShotZip          (value: string        ); begin  SetPropStr  ('ScrShotZip',             value ) end;
procedure TIMESettings.SetMailFrom               (value: string        ); begin  SetPropStr  ('MailFrom',               value ) end;
procedure TIMESettings.SetBugReportFile          (value: string        ); begin  SetPropStr  ('BugRepFile',             value ) end;
procedure TIMESettings.SetAppendBugReports       (value: boolean       ); begin  SetPropBool ('AppendBugReps',          value ) end;
procedure TIMESettings.SetBugReportFileSize      (value: dword         ); begin  SetPropDw   ('BugRepFileSize',         value ) end;
procedure TIMESettings.SetNoDupExcepts           (value: boolean       ); begin  SetPropBool ('NoDupExcepts',           value ) end;
procedure TIMESettings.SetNoDupFreezes           (value: boolean       ); begin  SetPropBool ('NoDupFreezes',           value ) end;
procedure TIMESettings.SetDupExceptDef           (value: TMEDupDef     ); begin  SetPropDw   ('DupExceptDef',     dword(value)) end;
procedure TIMESettings.SetDupFreezeDef           (value: TMEDupDef     ); begin  SetPropDw   ('DupFreezeDef',     dword(value)) end;
procedure TIMESettings.SetListThreads            (value: boolean       ); begin  SetPropBool ('ListThreads',            value ) end;
procedure TIMESettings.SetShowCpuRegisters       (value: boolean       ); begin  SetPropBool ('CpuRegs',                value ) end;
procedure TIMESettings.SetShowStackDump          (value: boolean       ); begin  SetPropBool ('StackDump',              value ) end;
procedure TIMESettings.SetShowDisAsm             (value: boolean       ); begin  SetPropBool ('ShowDisAsm',             value ) end;
procedure TIMESettings.SetHideUglyItems          (value: boolean       ); begin  SetPropBool ('HideUglyItems',          value ) end;
procedure TIMESettings.SetShowRelativeAddrs      (value: boolean       ); begin  SetPropBool ('ShowRelAddrs',           value ) end;
procedure TIMESettings.SetShowRelativeLines      (value: boolean       ); begin  SetPropBool ('ShowRelLines',           value ) end;
procedure TIMESettings.SetFormatDisassembly      (value: boolean       ); begin  SetPropBool ('FormatDisAsm',           value ) end;
procedure TIMESettings.SetLimitDisassembly       (value: integer       ); begin  SetPropInt  ('LimitDisAsm',            value ) end;
procedure TIMESettings.SetTitleBar               (value: string        ); begin  SetPropStr  ('TitleBar',               value ) end;
procedure TIMESettings.SetExceptMsg              (value: string        ); begin  SetPropStr  ('ExceptMsg',              value ) end;
procedure TIMESettings.SetFrozenMsg              (value: string        ); begin  SetPropStr  ('FrozenMsg',              value ) end;
procedure TIMESettings.SetBitFaultMsg            (value: string        ); begin  SetPropStr  ('BitFaultMsg',            value ) end;
procedure TIMESettings.SetSendBtnCaption         (value: string        ); begin  SetPropStr  ('SendBtnTxt',             value ) end;
procedure TIMESettings.SetSaveBtnCaption         (value: string        ); begin  SetPropStr  ('SaveBtnTxt',             value ) end;
procedure TIMESettings.SetPrintBtnCaption        (value: string        ); begin  SetPropStr  ('PrintBtnTxt',            value ) end;
procedure TIMESettings.SetShowBtnCaption         (value: string        ); begin  SetPropStr  ('ShowBtnTxt',             value ) end;
procedure TIMESettings.SetContinueBtnCaption     (value: string        ); begin  SetPropStr  ('ContinueBtnTxt',         value ) end;
procedure TIMESettings.SetRestartBtnCaption      (value: string        ); begin  SetPropStr  ('RestartBtnTxt',          value ) end;
procedure TIMESettings.SetCloseBtnCaption        (value: string        ); begin  SetPropStr  ('CloseBtnTxt',            value ) end;
procedure TIMESettings.SetOkBtnCaption           (value: string        ); begin  SetPropStr  ('OkBtnTxt',               value ) end;
procedure TIMESettings.SetDetailsBtnCaption      (value: string        ); begin  SetPropStr  ('DetailsBtnTxt',          value ) end;
procedure TIMESettings.SetPleaseWaitTitle        (value: string        ); begin  SetPropStr  ('PlWaitTitle',            value ) end;
procedure TIMESettings.SetPleaseWaitText         (value: string        ); begin  SetPropStr  ('PlWaitText',             value ) end;
procedure TIMESettings.SetMailSubject            (value: string        ); begin  SetPropStr  ('MailSubj',               value ) end;
procedure TIMESettings.SetMailBody               (value: string        ); begin  SetPropStr  ('MailBody',               value ) end;
procedure TIMESettings.SetSendBoxTitle           (value: string        ); begin  SetPropStr  ('SendBoxTitle',           value )  end;
procedure TIMESettings.SetPrepareAttachMsg       (value: string        ); begin  SetPropStr  ('PrepAttMsg',             value )  end;
procedure TIMESettings.SetMxLookupMsg            (value: string        ); begin  SetPropStr  ('MxLookMsg',              value )  end;
procedure TIMESettings.SetConnectMsg             (value: string        ); begin  SetPropStr  ('ConnMsg',                value )  end;
procedure TIMESettings.SetAuthMsg                (value: string        ); begin  SetPropStr  ('AuthMsg',                value )  end;
procedure TIMESettings.SetSendMailMsg            (value: string        ); begin  SetPropStr  ('SendMailMsg',            value )  end;
procedure TIMESettings.SetFieldsMsg              (value: string        ); begin  SetPropStr  ('FieldMsg',               value )  end;
procedure TIMESettings.SetSendAttachMsg          (value: string        ); begin  SetPropStr  ('SendAttMsg',             value )  end;
procedure TIMESettings.SetSendFinalizeMsg        (value: string        ); begin  SetPropStr  ('SendFinalMsg',           value )  end;
procedure TIMESettings.SetSendFailureMsg         (value: string        ); begin  SetPropStr  ('SendFailMsg',            value ) end;

function TIMESettings.GetAssistant(name: string) : INVAssistant;
var s1    : string;
    i1    : integer;
    forms : TDAString;
begin
  result := nil;
  i1 := 1;
  repeat
    s1 := GetPropStr('Assistant' + IntToStrEx(i1));
    inc(i1);
  until (s1 = '') or IsTextEqual(SubStr(s1, 1), name);
  if s1 <> '' then begin
    if ( ( (self is TIMEException) and (not TIMEException(Self).FCreateScreenShot) ) or
         ( (self is TIMESettings ) and (GetScreenShotDepth = 0                   ) )    ) and
       SubTextExists(s1, 'ScrShotForm') then begin
      // auto hide screen shot form, if screen shotting is disabled
      s1 := '|' +  s1 + '|';
      ReplaceText(s1, '|ScrShotForm|', '|');
      FormatSubStrs(s1);
    end;
    SetLength(forms, SubStrCount(s1) - 2);
    for i1 := 0 to high(forms) do
      forms[i1] := SubStr(s1, i1 + 3);
    if self is TIMEException then
         result := LoadAssistant(FModule, SubStr(s1, 2), forms, IMEException(TIMEException(Self)))
    else result := LoadAssistant(FModule, SubStr(s1, 2), forms, nil);
  end;
  if result <> nil then
    ReadFormCache(result);
end;

function TIMESettings.FindProp(name: string; allowDefault, write: boolean; createTyp: TPropertyType) : TPMEProperty; 
var i1 : integer;
begin
  if FSettings <> nil then begin
    i1 := FindMEProperty(FSettings.propCount, FSettings.props, name);
    if (i1 = -1) and write then
      i1 := AddMEProperty(FSettings.propCap, FSettings.propCount, FSettings.props, pchar(name), length(name), createTyp);
  end else
    i1 := -1;
  if i1 = -1 then begin
    if allowDefault then
      i1 := FindMEProperty(Length(CMEProperties), @CMEProperties, name);
    if i1 = -1 then
         result := nil
    else result := @CMEProperties[i1];
  end else
    result := @FSettings.props[i1];
end;

function TIMESettings.GetPropBool(name: string) : boolean;
var b1   : boolean;
    prop : TPMEProperty;
begin
  b1 := (mySettings = nil) or (mySettings.Module = FModule);
  prop := FindProp(name, b1, false, ptBoolean);
  if prop <> nil then
    result := prop.vBool
  else
    if b1 then
      result := false
    else
      result := (mySettings as IMESettingsEx).GetPropBool(name);
end;

function TIMESettings.GetPropInt(name: string) : integer;
var b1   : boolean;
    prop : TPMEProperty;
begin
  b1 := (mySettings = nil) or (mySettings.Module = FModule);
  prop := FindProp(name, b1, false, ptBoolean);
  if prop <> nil then
    result := prop.vInt
  else
    if b1 then
      result := 0
    else
      result := (mySettings as IMESettingsEx).GetPropInt(name);
end;

function TIMESettings.GetPropStr(name: string) : string;
var b1   : boolean;
    prop : TPMEProperty;
begin
  b1 := (mySettings = nil) or (mySettings.Module = FModule);
  prop := FindProp(name, b1, false, ptBoolean);
  if prop <> nil then
    result := prop.vStr
  else
    if b1 then
      result := ''
    else
      result := (mySettings as IMESettingsEx).GetPropStr(name);
  ExpandVars(FModule, result, '%exceptMsg%', '%bugReport%');
end;

function TIMESettings.GetPropDw(name: string) : dword;
begin
  result := dword(GetPropInt(name));
end;

function TIMESettings.SetPropBool(name: string; value: boolean) : boolean;
var prop : TPMEProperty;
begin
  if FValid then begin
    prop := FindProp(name, false, true, ptBoolean);
    result := (prop <> nil) and (prop.typ = ptBoolean);
    if result then
      prop.vBool := value;
  end else
    result := false;
end;

function TIMESettings.SetPropInt(name: string; value: integer) : boolean;
var prop : TPMEProperty;
begin
  if FValid then begin
    prop := FindProp(name, false, true, ptInteger);
    result := (prop <> nil) and (prop.typ = ptInteger);
    if result then
      prop.vInt := value;
  end else
    result := false;
end;

function TIMESettings.SetPropStr(name: string; value: string) : boolean;
var prop : TPMEProperty;
begin
  if FValid then begin
    prop := FindProp(name, false, true, ptString);
    result := (prop <> nil) and (prop.typ = ptString);
    if result then begin
      if prop.vStr <> nil then begin
        LocalFree(dword(prop.vStr));
        prop.vStr := nil;
      end;
      if value <> '' then
        SetMEPropertyStr(prop^, pchar(value), Length(value));
    end;
  end else
    result := false;
end;

function TIMESettings.SetPropDw(name: string; value: dword) : boolean;
begin
  result := SetPropInt(name, integer(value));
end;

procedure TIMESettings.Reload;
begin
  RefreshProps(FModule, FSettings);
end;

procedure TIMESettings.FillFormCache(assistant: INVAssistant);
var i1, i2 : integer;
    edit   : INVEdit;
    check  : INVCheckBox;
begin
  if FFormCache = nil then begin
    FFormCache := TIMEFields.Create;
    (FFormCache as IMEFieldsEx).AcceptData;
  end;
  for i1 := 0 to assistant.FormCount - 1 do
    with assistant.Forms[i1] do
      if ModalResult = nvmOk then
        for i2 := 0 to ItemCount - 1 do
          if Items[i2].QueryInterface(INVEdit, edit) = 0 then
            FFormCache[edit.Name] := edit.Text
          else
            if Items[i2].QueryInterface(INVCheckBox, check) = 0 then
              FFormCache[check.Name] := booleanToChar(check.Checked);
end;

procedure TIMESettings.ReadFormCache(assistant: INVAssistant);
var i1, i2 : integer;
    edit   : INVEdit;
    check  : INVCheckBox;
    b1, b2 : boolean;
begin
  if FFormCache <> nil then
    for i1 := 0 to assistant.FormCount - 1 do
      with assistant.Forms[i1] do begin
        b1 := false;
        b2 := true;
        for i2 := 0 to ItemCount - 1 do
          if Items[i2].QueryInterface(INVEdit, edit) = 0 then begin
            if FFormCache.FindItem(edit.Name) <> -1 then begin
              b1 := true;
              edit.Text := FFormCache[edit.Name];
            end else
              b2 := false;
          end else
            if Items[i2].QueryInterface(INVCheckBox, check) = 0 then
              if FFormCache.FindItem(check.Name) <> -1 then begin
                b1 := true;
                check.Checked := FFormCache[check.Name] = '+';
              end else
                b2 := false;
        if b1 and b2 and ContinueButton.Enabled then
          ActiveControl := ContinueButton.Name;
      end;
end;

function TIMESettings.GetSelf : TIMESettings;
begin
  result := self;
end;

constructor TIMESettings.Create(module: dword; init: boolean);
var c1 : dword;
begin
  inherited Create;
  if init then begin
    FSettings := ReferenceMeSettingsModule(module);
    FValid := FSettings <> nil;
    if FValid then begin
      FModule := module;
      c1 := GetPropDw('VersionVar');
      if c1 <> 0 then
        try
          FVersionVar := pchar(TPPointer(module + c1)^);
        except end;
    end;
  end;
end;

destructor TIMESettings.Destroy;
begin
  if FAddAttachs <> nil then
    (FAddAttachs as IMEAttachmentsEx).ClearParent;
  if FAddFields <> nil then
    (FAddFields as IMEFieldsEx).ClearMesParent;
  if FSettings <> nil then
    DereferenceMeSettingsModule(FSettings);
  inherited;
end;

function FindMeSettings : IMEModuleSettings;
var mbi    : TMemoryBasicInformation;
    mes    : IMEModuleSettings;
    p1, p2 : pointer;
    c1     : dword;
begin
  result := nil;
  if GetMeSettingsRes(HInstance) <> 0 then
    result := TIMESettings.Create(HInstance, true);
  if ((result = nil) or result.NoOwnSettings) then begin
    c1 := GetModuleHandle('madExceptIde_.bpl');
    if (c1 <> 0) and (GetMeSettingsRes(c1) <> 0) then
      result := TIMESettings.Create(c1, true);
  end;
  p1 := nil;
  p2 := nil;
  while ((result = nil) or result.NoOwnSettings) and
        (VirtualQuery(p1, mbi, sizeOf(mbi)) = sizeOf(mbi)) do begin
    if (mbi.State = MEM_COMMIT) and
       (mbi.AllocationBase <> p2) and
       (mbi.AllocationBase = mbi.BaseAddress) and
       (GetMeSettingsRes(dword(mbi.AllocationBase)) <> 0) then begin
      mes := TIMESettings.Create(dword(mbi.AllocationBase), true);
      if (result = nil) or (not mes.NoOwnSettings) then
        result := mes;
    end;
    p2 := mbi.AllocationBase;
    dword(p1) := dword(p1) + mbi.RegionSize;
  end;
end;

function MESettings(module: dword) : IMEModuleSettings;
var mes : IMEModuleSettings;
begin
  result := TIMESettings.Create(module, true);
  if (      result.IsValid  and result.NoOwnSettings ) or          // -> madExcept enabled, but no own settings
     ( (not result.IsValid) and              AmMeBpl ) then begin  // -> this is a bpl containing madExcept.pas
    mes := FindMeSettings;
    if (mes <> nil) and mes.IsValid and (not mes.NoOwnSettings) then
      result := mes;
  end;
end;

function MESettings(addr: pointer) : IMEModuleSettings;
var mbi    : TMemoryBasicInformation;
    mes    : IMEModuleSettings;
begin
  result := nil;
  if (VirtualQuery(addr, mbi, sizeOf(mbi)) = sizeOf(mbi)) and
     (mbi.State = MEM_COMMIT) and (mbi.AllocationBase <> nil) and
     (dword(mbi.AllocationBase) <> HInstance) and
     (GetMeSettingsRes(dword(mbi.AllocationBase)) <> 0) then
    result := TIMESettings.Create(dword(mbi.AllocationBase), true);
  if (result = nil) or (result.IsValid and result.NoOwnSettings) then begin
    mes := FindMeSettings;
    if (result = nil) or (mes.IsValid and (not mes.NoOwnSettings)) then
      result := mes;
  end;
  if result = nil then
    result := TIMESettings.Create(HInstance, true);
end;

{$W+}
function MESettings : IMEModuleSettings;
var ebp_ : dword;
begin
  asm
    mov ebp_, ebp
  end;
  result := MESettings(TPPointer(ebp_ + 4)^);
end;

function MESettingsEx(level: integer) : IMEModuleSettings;
var ebp_ : dword;
    i1   : integer;
begin
  asm
    mov ebp_, ebp
  end;
  for i1 := 1 to level do
    ebp_ := TPCardinal(ebp_)^;
  result := MESettings(TPPointer(ebp_ + 4)^);
end;
{$W-}

// ***************************************************************
// fields class implementation

procedure TIMEFields.Lock;
begin
  if FExcParent <> nil then
    FExcParent.BeginUpdate;
  EnterCriticalSection(FSection);
end;

procedure TIMEFields.Unlock;
begin
  LeaveCriticalSection(FSection);
  if FExcParent <> nil then
    FExcParent.EndUpdate;
end;

function TIMEFields.GetItemCount : integer;
begin
  result := FItemCount;
end;

function TIMEFields.GetItem(index: integer) : string;
begin
  Lock;
  try
    if (index >= 0) and (index < FItemCount) then
         result := FItems[index].item
    else result := '';
  finally Unlock end;
end;

procedure TIMEFields.SetItem(index: integer; value: string);
begin
  Lock;
  try
    if (index >= 0) and (index < FItemCount) and (FItems[index].item <> value) then begin
      FItems[index].item := value;
      Modified;
    end;
  finally Unlock end;
end;

function TIMEFields.FindItem(item: string) : integer;
var i1 : integer;
begin
  result := -1;
  Lock;
  try
    for i1 := 0 to FItemCount - 1 do
      if IsTextEqual(FItems[i1].item, item) then begin
        result := i1;
        break;
      end;
  finally Unlock end;
end;

function TIMEFields.GetContent(item: string) : string;
var index : integer;
begin
  Lock;
  try
    index := FindItem(item);
    if index >= 0 then
      result := FItems[index].content;
  finally Unlock end;
end;

procedure TIMEFields.SetContent(item, value: string);
var index : integer;
begin
  if FAcceptData then begin
    TrimStr(value);
    if value <> '' then begin
      Lock;
      try
        index := FindItem(item);
        if index >= 0 then begin
          if (FItems[index].content <> value) or (FItems[index].item <> item) then begin
            FItems[index].item    := item;
            FItems[index].content := value;
            Modified;
          end;
        end else
          if FAddIndex > 0 then
               Insert(FAddIndex,  item, value)
          else Insert(FItemCount, item, value);
      finally Unlock end;
    end else
      Delete(item);
  end;
end;

procedure TIMEFields.Add(item, content: string);
begin
  SetContent(item, content);
end;

procedure TIMEFields.Insert(index: integer; item, content: string);
var i1 : integer;
begin
  TrimStr(content);
  if (content <> '') and FAcceptData then begin
    Lock;
    try
      if index < 0 then
        index := 0
      else
        if index > FItemCount then
          index := FItemCount;
      i1 := FindItem(item);
      if i1 > 0 then begin
        if (i1 = index) and (FItems[i1].content = content) then
          exit;
        Delete(i1);
        if i1 < index then
          dec(index);
      end;
      if Length(FItems) = FItemCount then
        if FItemCount = 0 then
             SetLength(FItems, 16)
        else SetLength(FItems, FItemCount * 3 div 2);
      for i1 := FItemCount - 1 downto index do
        FItems[i1 + 1] := FItems[i1];
      FItems[index].item    := item;
      FItems[index].content := content;
      inc(FItemCount);
      if (FAddIndex > 0) and (index <= FAddIndex) then
        inc(FAddIndex);
      Modified;
    finally Unlock end;
  end;
end;

procedure TIMEFields.Delete(index: integer);
var i1 : integer;
begin
  Lock;
  try
    if (index >= 0) and (index < FItemCount) then begin
      for i1 := index to FItemCount - 2 do
        FItems[i1] := FItems[i1 + 1];
      dec(FItemCount);
      if (FAddIndex > 0) and (index < FAddIndex) then
        dec(FAddIndex);
      Modified;
    end;
  finally Unlock end;
end;

procedure TIMEFields.Delete(item: string);
begin
  Delete(FindItem(item));
end;

function TIMEFields.Clone : IMEFields;
var i1 : integer;
begin
  result := TIMEFields.Create;
  (result as IMEFieldsEx).AcceptData;
  Lock;
  try
    for i1 := 0 to FItemCount - 1 do
      result.Add(FItems[i1].item, FItems[i1].content);
  finally Unlock end;
end;

procedure TIMEFields.ClearExcParent;
begin
  FExcParent := nil;
end;

procedure TIMEFields.ClearMesParent;
begin
  FMesParent := nil;
end;

procedure TIMEFields.Modified;
var s1  : string;
    i1  : integer;
    mes : TIMESettings;
begin
  if FMesParent <> nil then begin
    s1 := '';
    Lock;
    try
      for i1 := 0 to FItemCount - 1 do
        s1 := s1 + '|' + FItems[i1].item + '=' + FItems[i1].content;
    finally Unlock end;
    System.Delete(s1, 1, 1);
    mes := FMesParent;
    if mes <> nil then
      // not really thread safe, but better bad protection than none
      mes.SetPropStr('AddFields', s1);
  end;
  if FExcParent <> nil then
    FExcParent.SetModified;
end;

procedure TIMEFields.SetAddIndex(addIndex: integer);
begin
  FAddIndex := addIndex;
end;

procedure TIMEFields.AcceptData;
begin
  FAcceptData := true;
end;

constructor TIMEFields.Create;
begin
  inherited;
  InitializeCriticalSection(FSection);
end;

destructor TIMEFields.Destroy;
begin
  DeleteCriticalSection(FSection);
  inherited;
end;

function NewFields : IMEFields;
begin
  result := TIMEFields.Create;
end;

// ***************************************************************
// attachments class implementation

procedure TIMEAttachments.Lock;
begin
  EnterCriticalSection(FSection);
end;

procedure TIMEAttachments.Unlock;
begin
  LeaveCriticalSection(FSection);
end;

function TIMEAttachments.GetItemCount : integer;
begin
  result := FItemCount;
end;

function TIMEAttachments.GetItem(index: integer;
                                 var originalFile   : string;
                                 var sendAsFileName : string;
                                 var zipFile        : string;
                                 var fieldName      : string) : boolean;
begin
  Lock;
  try
    result := (index >= 0) and (index < FItemCount);
    if result then begin
      originalFile   := FItems[index].orgFile;
      sendAsFileName := FItems[index].sendAs;
      zipFile        := FItems[index].zip;
      fieldName      := FItems[index].field;
    end;
  finally Unlock end;
end;

procedure TIMEAttachments.Add(originalFile   : string;
                              sendAsFileName : string = '';
                              zipFile        : string = '';
                              fieldName      : string = '');
begin
  if originalFile <> '' then begin
    if sendAsFileName = '' then
      sendAsFileName := ExtractFileName(originalFile);
    if fieldName = '' then begin
      fieldName := sendAsFileName;
      System.Delete(fieldName, PosStr('.', fieldName, maxInt, 1), maxInt);
    end;
    Lock;
    try
      if Length(FItems) = FItemCount then
        if FItemCount = 0 then
             SetLength(FItems, 16)
        else SetLength(FItems, FItemCount * 3 div 2);
      FItems[FItemCount].orgFile := originalFile;
      FItems[FItemCount].sendAs  := sendAsFileName;
      FItems[FItemCount].zip     := zipFile;
      FItems[FItemCount].field   := fieldName;
      inc(FItemCount);
    finally Unlock end;
    Modified;
  end;
end;

function TIMEAttachments.Delete(originalFile: string) : boolean;
var i1, i2 : integer;
begin
  result := false;
  Lock;
  try
    for i1 := 0 to FItemCount - 1 do
      if IsTextEqual(FItems[i1].orgFile, originalFile) then begin
        for i2 := i1 to FItemCount - 2 do
          FItems[i2] := FItems[i2 + 1];
        Finalize(FItems[FItemCount - 1]);
        dec(FItemCount);
        result := true;
        break;
      end;
  finally Unlock end;
  if result then
    Modified;
end;

procedure TIMEAttachments.Clear;
var b1 : boolean;
begin
  Lock;
  try
    b1 := FItemCount > 0;
    if b1 then begin
      FItems := nil;
      FItemCount := 0;
    end;
  finally Unlock end;
  if b1 then
    Modified;
end;

function TIMEAttachments.Clone : IMEAttachments;
var i1 : integer;
begin
  result := TIMEAttachments.Create;
  Lock;
  try
    for i1 := 0 to FItemCount - 1 do
      result.Add(FItems[i1].orgFile, FItems[i1].sendAs, FItems[i1].field);
  finally Unlock end;
end;

procedure TIMEAttachments.ClearParent;
begin
  FParent := nil;
end;

procedure TIMEAttachments.Modified;
var s1  : string;
    i1  : integer;
    mes : TIMESettings;
begin
  if FParent <> nil then begin
    s1 := '';
    Lock;
    try
      for i1 := 0 to FItemCount - 1 do
        s1 := s1 + '|' + FItems[i1].orgFile + '>' + FItems[i1].sendAs + '>' +
                         FItems[i1].zip     + '>' + FItems[i1].field;
    finally Unlock end;
    System.Delete(s1, 1, 1);
    mes := FParent;
    if mes <> nil then
      // not really thread safe, but better bad protection than none
      mes.SetPropStr('AddAttachs', s1);
  end;
end;

constructor TIMEAttachments.Create;
begin
  inherited;
  InitializeCriticalSection(FSection);
end;

destructor TIMEAttachments.Destroy;
begin
  DeleteCriticalSection(FSection);
  inherited;
end;

function NewAttachments : IMEAttachments;
begin
  result := TIMEAttachments.Create;
end;

// ***************************************************************
// (mostly) exported madExcept tool functions

function MailAddrCount(var mail: string) : integer;
begin
  ReplaceStr(mail, ';', ',');
  FormatSubStrs(mail, ',');
  result := SubStrCount(mail, ',');
end;

function FormatMailAddr(mail: string; allowFullName: boolean) : string;
var i1 : integer;
begin
  i1 := PosStr('<', mail);
  if i1 = 0 then
    result := '<' + mail + '>'
  else
    if allowFullName then
      result := mail
    else
      result := Copy(mail, i1, maxInt);
end;

function PrepareAttachments(settings: IMESettings; attach: IMEAttachments;
                            var files, sendAs, fields: TDAString;
                            parentWindow: dword; hidden, background: boolean) : IProgressAlert;
var zipHis, toZip, zipAs : TDAString;
    i1, i2, i3           : integer;
    b1                   : boolean;
    s1, s2, s3, s4       : string;
begin
  result := NewProgressAlert(settings.SendBoxTitle, settings.Module, 'MEISEND32');
  result.AddArea(100, settings.PrepareAttachMsg);
  if settings.MailAsSmtpServer or settings.MailAsSmtpClient or settings.UploadViaHttp then begin
    if settings.MailAsSmtpServer then
      result.AddArea(100, settings.MxLookupMsg);
    result.AddArea(100, settings.ConnectMsg);
    if settings.MailAsSmtpServer or settings.MailAsSmtpClient then begin
      result.AddArea(200, settings.AuthMsg);
      result.AddArea(100, settings.SendMailMsg);
    end;
    if settings.UploadViaHttp then
      result.AddArea(100, settings.FieldsMsg);
    if (attach <> nil) and (attach.ItemCount > 0) then
      result.AddArea(100, settings.SendAttachMsg);
    result.AddArea(100, settings.SendFinalizeMsg);
  end;
  if (not hidden) or settings.AutoSendPrgrBox then begin
    if background then
      parentWindow := 0;
    result.Show(parentWindow, not background);
  end;
  files  := nil;
  sendAs := nil;
  fields := nil;
  zipHis := nil;
  toZip  := nil;
  zipAs  := nil;
  if attach <> nil then
    for i1 := 0 to attach.ItemCount - 1 do begin
      attach.GetItem(i1, s1, s2, s3, s4);
      b1 := s3 <> '';
      if b1 then begin
        for i2 := 0 to high(zipHis) do
          if IsTextEqual(zipHis[i2], s3) then begin
            b1 := false;
            break;
          end;
        if b1 then begin
          SetLength(zipHis, Length(zipHis) + 1);
          zipHis[high(zipHis)] := s3;
          toZip := nil;
          zipAs := nil;
          i3 := 0;
          for i2 := i1 to attach.ItemCount - 1 do begin
            attach.GetItem(i2, s1, s2, s3, s4);
            if IsTextEqual(s3, zipHis[high(zipHis)]) then begin
              SetLength(toZip, i3 + 1);
              SetLength(zipAs, i3 + 1);
              toZip[i3] := s1;
              zipAs[i3] := s2;
              inc(i3);
            end;
          end;
          attach.GetItem(i1, s1, s2, s3, s4);
          s1 := GetTempPath;
          if (s1 <> '') and Zip(s1 + s3, toZip, zipAs) then begin
            i2 := Length(files);
            SetLength(files,  i2 + 1);
            SetLength(sendAs, i2 + 1);
            SetLength(fields, i2 + 1);
            files [i2] := s1 + s3;
            sendAs[i2] := s3;
            fields[i2] := s3;
          end;
        end;
      end else begin
        i2 := Length(files);
        SetLength(files,  i2 + 1);
        SetLength(sendAs, i2 + 1);
        SetLength(fields, i2 + 1);
        files [i2] := s1;
        sendAs[i2] := s2;
        fields[i2] := s4;
      end;
    end;
end;

function AmOnline : boolean;
var igcs    : function (var flags: dword; reserved: dword = 0) : bool; stdcall;
    dll, c1 : dword;
begin
  dll := LoadLibrary('wininet.dll');
  igcs := GetProcAddress(dll, 'InternetGetConnectedState');
  result := (@igcs <> nil) and igcs(c1);
  FreeLibrary(dll);
end;

function SendMapiMailEx(rcptTo, subject, body: string; attachFiles, attachSendAs: TDAString;
                        parentWindow: dword; hidden: boolean) : TExtBool;
// send a mail via Mapi

  procedure SplitEMailAddr(email: string; var name, address: string);
  var i1, i2 : integer;
  begin
    i1 := PosStr('<', email);
    if i1 > 0 then begin
      i2 := PosStr('>', email, i1 + 1);
      if i2 > 0 then
        Delete(email, i2, maxInt);
      name    := Copy(email, 1, i1 - 1);
      address := Copy(email, i1 + 1, maxInt);
    end else begin
      name    := email;
      address := email;
    end;
    address := 'SMTP:' + address;
  end;

type
  TMapiReceiver = packed record
    d1        : dword;
    class_    : dword;
    name      : pchar;
    address   : pchar;
    d2        : array [0..1] of dword;
  end;
  TMapiAttach = packed record
    d1        : dword;
    d2        : dword;
    position  : dword;  // must be -1
    pathName  : pchar;
    fileName  : pchar;
    d4        : dword;
  end;
  TMapiMail = packed record
    d1        : dword;
    subject   : pchar;
    body      : pchar;
    d2        : array [0..4] of dword;
    noOfRec   : dword;
    receiver  : ^TMapiReceiver;
    attachCnt : dword;
    attachs   : ^TMapiAttach;
  end;
var //key      : HKEY;
    //len      : dword;
    //buf      : array [0..8] of char;
    mail     : TMapiMail;
    rec1     : array of array [0..1] of string;
    rec2     : array of TMapiReceiver;
    att      : array of TMapiAttach;
    flags    : dword;
    sendproc : function (session, uiParam: dword; var msg: TMapiMail; flags, reserved: dword) : dword; stdcall;
    b1, b2   : boolean;
    i1       : integer;
begin
  result := no;
  if not IsProcessBlocked(250) then begin
//    len := sizeOf(buf);
//    if RegOpenKeyEx(HKEY_LOCAL_MACHINE, 'Software\Microsoft\Windows Messaging Subsystem', 0, KEY_READ, key) = 0 then begin
//      if (RegQueryValueEx(key, 'MAPI', nil, nil, @buf, @len) = 0) and (buf[0] = '1') and (buf[1] = #0) then begin
        sendproc := GetProcAddress(LoadLibrary('mapi32.dll'), 'MAPISendMail');
        if @sendproc <> nil then begin
          ZeroMemory(@mail, sizeOf(mail));
          rec2 := nil;
          SetLength(rec1, MailAddrCount(rcptTo));
          SetLength(rec2, length(rec1));
          for i1 := 0 to high(rec1) do begin
            SplitEMailAddr(SubStr(rcptTo, i1 + 1, ','), rec1[i1][0], rec1[i1][1]);
            rec2[i1].class_  := 1;  // MAPI_TO = 1
            rec2[i1].name    := pchar(rec1[i1][0]);
            rec2[i1].address := pchar(rec1[i1][1]);
          end;
          mail.subject  := pchar(subject);
          mail.body     := pchar(body   );
          mail.noOfRec  := length (rec2);
          mail.receiver := pointer(rec2);
          att := nil;
          if attachFiles <> nil then begin
            SetLength(att, length(attachFiles));
            for i1 := 0 to high(att) do begin
              att[i1].position := dword(-1);
              att[i1].pathName := pchar(attachFiles [i1]);
              att[i1].fileName := pchar(attachSendAs[i1]);
            end;
          end;
          mail.attachCnt := length (att);
          mail.attachs   := pointer(att);
          if hidden and (rcptTo <> '') then
               flags := 0
          else flags := $9;  // MAPI_DIALOG = 8; MAPI_LOGON_UI = 1;
          b1 := (parentWindow <> 0) and (GetWindowLong(parentWindow, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0);
          if b1 then
            SetWindowPos(parentWindow, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
          b2 := ResumeMainThread;
          case sendProc(0, parentWindow, mail, flags, 0) of
            0 : result := yes;    // sending succeeded
            1 : result := other;  // user aborted mailing
          end;
          if b2 then
            SuspendMainThread;
          if b1 then
            SetWindowPos(parentWindow, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
        end;
//      end;
//      RegCloseKey(key);
//    end;
  end;
end;

function SendMapiMail(rcptTo, subject, body: string; attachments: IMEAttachments = nil;
                      parentWindow: dword = 0; hidden: boolean = false; background: boolean = false;
                      settings: IMESettings = nil) : TExtBool;
var attachFiles, attachSendAs, attachFields : TDAString;
begin
  if settings = nil then
    settings := MESettingsEx(1);
  PrepareAttachments(settings, attachments, attachFiles, attachSendAs, attachFields,
                     parentWindow, hidden, background).Close;
  result := SendMapiMailEx(rcptTo, subject, body, attachFiles, attachSendAs, parentWindow, hidden);
end;

const CEncodeMailto : TSChar = [#$D, #$A, ' ', '#', '<', '>', '"', '%', '?', '+'];
      CEncodeSmtp   : TSChar = [#0..#255] - ['a'..'z', 'A'..'Z', '0'..'9'];
function EncodeStr(str: string; encodeChars: TSChar; encodeChar: char) : string;

  function FourBitToHex(fourBit: integer) : char;
  begin
    if fourBit < $a then
         result := chr(ord('0') - $0 + fourBit)
    else result := chr(ord('A') - $a + fourBit);
  end;

var i1, i2 : integer;
begin
  i2 := Length(str);
  for i1 := length(str) downto 1 do
    if str[i1] in encodeChars then
      inc(i2, 2);
  SetLength(result, i2);
  i2 := 1;
  for i1 := 1 to Length(str) do
    if str[i1] in encodeChars then begin
      result[i2    ] := encodeChar;
      result[i2 + 1] := FourBitToHex(ord(str[i1]) shr  4);
      result[i2 + 2] := FourBitToHex(ord(str[i1]) and $F);
      inc(i2, 3);
    end else begin
      result[i2] := str[i1];
      inc(i2);
    end;
end;

function SendShellMail(rcptTo, subject, body: string) : boolean;
// send a mail via CreateProcess('mailto:')
const CMaxCmdLine = 32 * 1024 - 2;
var s1, s2 : string;
    key    : HKEY;
    len    : dword;
    arrCh  : array [0..MAX_PATH] of char;
    si     : TStartupInfo;
    pi     : TProcessInformation;
begin
  result := false;
  len := MAX_PATH;
  if RegOpenKeyEx(HKEY_CLASSES_ROOT, 'mailto\shell\open\command', 0, KEY_READ, key) = 0 then begin
    if RegQueryValueEx(key, '', nil, nil, @arrCh, @len) = 0 then begin
      arrCh[len] := #0;
      s1 := arrCh;
      ExpandEnvironmentStrings(pchar(s1), arrCh, MAX_PATH);
      s1 := arrCh;
      if s1 <> '' then begin
        subject := EncodeStr(subject, CEncodeMailto, '%');
        body    := EncodeStr(body,    CEncodeMailto, '%');
        MailAddrCount(rcptTo);
        s2 := 'mailto:' + rcptTo + '?subject=' + subject + '&body=' + body;
        if not ReplaceStr(s1, '%1', s2) then
          if s1[Length(s1)] <> ' ' then
               s1 := s1 + ' ' + s2
          else s1 := s1       + s2;
        // the max supported parameter length for CreateProcess in win9x is
        // unlimited, however in NT based systems it is 32kb
        if (GetVersion and $80000000 = 0) and (length(s1) > CMaxCmdLine) then
          if      s1[CMaxCmdLine    ] = '%' then SetLength(s1, CMaxCmdLine - 1)
          else if s1[CMaxCmdLine - 1] = '%' then SetLength(s1, CMaxCmdLine - 2)
          else                                   SetLength(s1, CMaxCmdLine    );
        ZeroMemory(@si, sizeOf(si));
        si.cb := sizeOf(si);
        result := CreateProcess(nil, pchar(s1), nil, nil, false, 0, nil, nil, si, pi);
        if result then begin
          if pi.hThread  <> 0 then CloseHandle(pi.hThread );
          if pi.hProcess <> 0 then CloseHandle(pi.hProcess);
        end;
      end;
    end;
    RegCloseKey(key);
  end;
end;

function MxLookup(email: string) : string;

  function GetDnsIps : TDAString;
  type
    TDns = packed record
      next   : pointer;
      ipAddr : array [0..15] of char;
      ipMask : array [0..15] of char;
      ctxt   : dword;
    end;
    TFixedInfo = packed record
      hostName      : array [1..132] of char;
      domainName    : array [1..132] of char;
      currentDns    : ^TDns;
      dnsList       : TDns;
      nodeType      : dword;
      scopeId       : array [1..260] of char;
      enableRouting : dword;
      enableProxy   : dword;
      enableDns     : dword;
    end;
  var dll : dword;
      gnp : function (buf: pointer; var len: integer) : dword; stdcall;
      len : integer;
      buf : ^TFixedInfo;
      s1  : string;
      ch1 : char;
      i1  : integer;
      dns : ^TDns;
  begin
    result := nil;
    if OS.Enum in [osWin95, osWin95osr2, osWinNTOld, osWinNT4] then begin
      if GetVersion and $80000000 = 0 then
           s1 := 'Tcpip\Parameters'
      else s1 := 'VxD\MSTCP';
      s1 := 'System\CurrentControlSet\Services\' + s1;
      s1 := RegReadStr(HKEY_LOCAL_MACHINE, s1, 'DhcpNameServer');
      if s1 = '' then
        s1 := RegReadStr(HKEY_LOCAL_MACHINE, s1, 'NameServer');
      if s1 <> '' then begin
        if PosStr(',', s1) > 0 then
             ch1 := ','
        else ch1 := ' ';
        SetLength(result, SubStrCount(s1, ch1));
        for i1 := 0 to high(result) do
          result[i1] := SubStr(s1, i1 + 1, ch1);
      end;
    end else begin
      dll := LoadLibrary('IpHlpApi.dll');
      if dll <> 0 then begin
        gnp := GetProcAddress(dll, 'GetNetworkParams');
        if @gnp <> nil then begin
          len := 0;
          gnp(nil, len);
          if len <> 0 then begin
            len := len shl 1;
            GetMem(buf, len);
            if gnp(buf, len) = 0 then begin
              if buf^.currentDns <> nil then begin
                SetLength(result, 1);
                result[0] := buf^.currentDns^.ipAddr;
              end;
              dns := @buf^.dnsList;
              repeat
                s1 := dns.ipAddr;
                if (s1 <> '') and ((result = nil) or (s1 <> result[0])) then begin
                  SetLength(result, Length(result) + 1);
                  result[high(result)] := s1;
                end;
                dns := dns^.Next;
              until dns = nil;
            end;
            FreeMem(buf);
          end;
        end;
        FreeLibrary(dll);
      end;
    end;
  end;

  procedure nslookup(email: string; const querys: array of string; type_: byte; var as1, as2: TDAString);
  var buf : array [0..511] of byte;

    function GetLabel(var pos: integer; len: integer) : string;
    var i1 : integer;
    begin
      result := '';
      while pos < len do
        if buf[pos] and $c0 = $c0 then begin
          i1 := (buf[pos + 1] + buf[pos] shl 8) and $3fff;
          result := result + '.' + GetLabel(i1, len);
          inc(pos, 2);
          break;
        end else
          if buf[pos] > 0 then begin
            result := result + '.' + PShortString(@buf[pos])^;
            inc(pos, buf[pos] + 1);
          end else begin
            inc(pos);
            break;
          end;
      Delete(result, 1, 1);
    end;

    function GetServers(len: integer; var pos: integer) : TDAString;
    var i1, i2 : integer;
    begin
      SetLength(result, len);
      for i1 := 0 to high(result) do begin
        GetLabel(pos, maxInt);
        if type_ = 15 then
             i2 := pos + 12
        else i2 := pos + 10;
        inc(pos, buf[pos + 9] + buf[pos + 8] shl 8 + 10);
        result[i1] := GetLabel(i2, pos);
      end;
    end;

    function StrToSAddr(str: string) : integer;
    var phe : PHostEnt;
    begin
      result := integer(inet_addr(pchar(str)));
      if result = integer(INADDR_NONE) then begin
        phe := gethostbyname(pchar(str));
        if phe <> nil then
          result := TPInteger(phe^.h_addr_list^)^;
      end;
    end;

  var sock   : TSocket;
      sin    : TSockAddrIn;
      addr   : integer;
      pos    : integer;
      i1, i2 : integer;
      s1     : string;
      count  : integer;
      fds    : TFDSet;
      tv     : TTimeVal;
  begin
    as1 := nil;
    as2 := nil;
    sock := socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
    if sock <> INVALID_SOCKET then begin
      sin.sin_family      := AF_INET;
      sin.sin_addr.s_addr := INADDR_ANY;
      sin.sin_port        := 0;
      if bind(sock, sin, sizeOf(sin)) <> SOCKET_ERROR then begin
        ZeroMemory(@buf, sizeOf(buf));
        buf[1] := 1;
        buf[2] := 1;  // recursive lookup allowed
        buf[5] := 1;
        s1 := '.' + email;
        i1 := 1;
        while true do begin
          i2 := PosStr('.', s1, i1 + 1);
          if (i2 > 0) and (i2 < Length(s1)) then begin
            byte(s1[i1]) := i2 - i1 - 1;
            i1 := i2;
          end else begin
            byte(s1[i1]) := Length(s1) - i1;
            break;
          end;
        end;
        Move(s1[1], buf[12], Length(s1));
        pos := 12 + Length(s1);
        buf[pos + 2] := type_;
        buf[pos + 4] := 1;
        inc(pos, 5);
        count := 0;
        for i1 := 0 to high(querys) do
          if querys[i1] <> '' then begin
            addr := StrToSAddr(querys[i1]);
            if addr <> integer(INADDR_NONE) then begin
              sin.sin_family      := AF_INET;
              sin.sin_port        := htons(53);
              sin.sin_addr.s_addr := addr;
              if sendto(sock, buf, pos, 0, sin, sizeOf(sin)) <> SOCKET_ERROR then
                inc(count);
            end;
          end;
        fds.fd_count := 1;
        fds.fd_array[0] := sock;
        tv.tv_sec := 5;
        tv.tv_usec := 0;
        while (count > 0) and (select(0, @fds, nil, nil, @tv) > 0) and
              (recvfrom(sock, buf, 512, 0, TSockAddrIn(nil^), integer(nil^)) <> SOCKET_ERROR) do begin
          if buf[4] and $0f = 0 then begin
            as1 := GetServers(TPWord(@buf[7])^, pos);
            if type_ = 2 then
              as2 := GetServers(TPWord(@buf[9])^, pos);
            if (as1 <> nil) or (as2 <> nil) then
              break;
          end;
          dec(count);
        end;
      end;
      closesocket(sock);
    end;
  end;

var wd            : TWsaData;
    as1, as2, as3 : TDAString;
    subEmail      : string;
    i1, i2        : integer;
    dnsIps        : TDAString;
{ $define oneServerOnly}
begin
  result := '';
  dnsIps := nil;
  Delete(email, 1, PosStr('@', email));
  if email[Length(email)] = '>' then
    Delete(email, Length(email), 1);
  subEmail := email;
  i2 := 0;
  for i1 := Length(subEmail) downto 1 do
    if subEmail[i1] = '.' then
      if i2 = 2 then begin
        Delete(subEmail, 1, i1);
        break;
      end else
        inc(i2);
	if WSAStartup($101, wd) = 0 then begin
    dnsIps := GetDnsIps;
    if dnsIps <> nil then begin
      {$ifdef oneServerOnly}
        nslookup(email, [dnsIps[0]], 15, as2, as3);  // mx
      {$else}
        nslookup(email, dnsIps, 15, as2, as3);  // mx
      {$endif}
      if as2 <> nil then
        result := as2[0];
      if (result = '') and (email <> subEmail) then begin
        {$ifdef oneServerOnly}
          nslookup(subEmail, [dnsIps[0]], 15, as2, as3);  // mx
        {$else}
          nslookup(subEmail, dnsIps, 15, as2, as3);  // mx
        {$endif}
        if as2 <> nil then
          result := as2[0];
      end;
    end else begin
      nslookup(subEmail, ['A.ROOT-SERVERS.NET', 'K.ROOT-SERVERS.NET'], 2, as1, as2);  // nameserver
      if as2 <> nil then begin
        {$ifdef oneServerOnly}
          nslookup(subEmail, [as2[0]], 2, as1, as3);  // nameserver
        {$else}
          nslookup(subEmail, as2, 2, as1, as3);  // nameserver
        {$endif}
        for i1 := 1 to 10 do
          if (as1 = nil) and (as3 <> nil) then begin
            {$ifdef oneServerOnly}
              nslookup(subEmail, [as3[0]], 2, as1, as2); // nameserver
            {$else}
              nslookup(subEmail, as3, 2, as1, as2); // nameserver
            {$endif}
            as3 := as2;
          end else
            break;
        if (as1 = nil) and (as2 <> nil) then
          as1 := as2;
        {$ifdef oneServerOnly}
          for i1 := high(as1) downto 0 do begin
            nslookup(email, [as1[i1]], 15, as2, as3);  // mx
            if as2 <> nil then begin
              result := as2[0];
              break;
            end;
          end;
          if (result = '') and (email <> subEmail) then
            for i1 := 0 to high(as1) do begin
              nslookup(subEmail, [as1[i1]], 15, as2, as3);  // mx
              if as2 <> nil then begin
                result := as2[0];
                break;
              end;
            end;
        {$else}
          nslookup(email, as1, 15, as2, as3);  // mx
          if as2 <> nil then
            result := as2[0];
          if (result = '') and (email <> subEmail) then begin
            nslookup(subEmail, as1, 15, as2, as3);  // mx
            if as2 <> nil then
              result := as2[0];
          end;
        {$endif}
      end;
    end;
    WSACleanup;
  end;
end;

function SmtpOrUpload(httpUrl           : string;
                      mailFrom, rcptTo  : string;
                      subject,  body    : string;
                      attachFiles       : TDAString;
                      attachSendAs      : TDAString;
                      attachFields      : TDAString;
                      fields            : IMEFields;
                      server            : string;
                      authUserName      : string;
                      authPassword      : string;
                      port              : dword;
                      var progressAlert : IProgressAlert;
                      settings          : IMESettings   ) : boolean;
const CBoundary = 'www.madshi.net_multipart_boundary';

  function SendStr(sock: TSocket; sendBuf: string) : boolean;
  var i1, i2 : integer;
  begin
    result := false;
    i1 := 0;
    while i1 < length(sendBuf) do begin
      i2 := length(sendBuf) - i1;
      if i2 > 1024 then
        i2 := 1024;
      if send(sock, sendBuf[i1 + 1], i2, 0) < 0 then
        exit;
      inc(i1, i2);
    end;
    result := true;
  end;

  function GetAttachSize(fileName: string) : integer;
  var fh : dword;
  begin
    fh := CreateFile(pchar(fileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
    if fh <> INVALID_HANDLE_VALUE then begin
      result := GetFileSize(fh, nil);
      CloseHandle(fh);
    end else
      result := 0;
  end;

  function Mail_PrepareProgress(pa: IProgressAlert; withMxLookup: boolean) : integer;
  var i1, i2 : integer;
  begin
    if withMxLookup then begin
      pa.AddArea(500, settings.MxLookupMsg);
      result := 1;
    end else
      result := 0;
    pa.AddArea(100, settings.ConnectMsg);
    pa.AddArea(100);
    pa.AddArea(200);
    pa.AddArea(200, settings.AuthMsg);
    pa.AddArea(100, settings.SendMailMsg);
    pa.AddArea(100);
    pa.AddArea(100);
    pa.AddArea(100);
    pa.AddArea(100);
    for i1 := 0 to high(attachFiles) do begin
      i2 := GetAttachSize(attachFiles[i1]);
      if i2 > 0 then begin
        pa.AddArea((i2 + 56) div 57 * 3, settings.SendAttachMsg);
        inc(result);
      end;
    end;
    pa.AddArea(500, settings.SendFinalizeMsg);
    pa.AddArea(100);
    inc(result, 11);
  end;

  function ConnectToServer(pa: IProgressAlert; server: string; out sock: TSocket) : boolean;
  var addr : integer;
      sin  : TSockAddrIn;
      phe  : PHostEnt;
  begin
    result := false;
    if server <> '' then begin
      sock := socket(AF_INET, SOCK_STREAM, 0);
      if sock <> INVALID_SOCKET then begin
        addr := integer(inet_addr(pchar(server)));
        if addr = integer(INADDR_NONE) then begin
          phe := gethostbyname(pchar(server));
          if phe <> nil then
            addr := TPInteger(phe^.h_addr_list^)^;
        end;
        if addr <> integer(INADDR_NONE) then begin
          pa.AreaDone;
          sin.sin_family      := AF_INET;
          sin.sin_port        := htons(port);
          sin.sin_addr.s_addr := addr;
          if (connect(sock, sin, sizeOf(sin)) <> SOCKET_ERROR) then begin
            result := true;
            pa.AreaDone;
          end;
        end;
      end;
    end;
  end;

  function MailIt(pa: IProgressAlert; rcptTo: string) : boolean;

    function GetDateTimeStr : string;
    const CMonths : array [1..12] of string =
            ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
    var st : TSystemTime;
    begin
      GetSystemTime(st);
      with st do
        result := IntToStrEx(dword(wDay)) + ' ' + CMonths[wMonth] + ' ' +
                  IntToStrEx(dword(wYear)) + ' ' + IntToStrEx(dword(wHour), 2) + ':' +
                  IntToStrEx(dword(wMinute), 2) + ':' + IntToStrEx(dword(wSecond), 2) + ' UT';
    end;

    function GetMessageId : string;
    var st : TSystemTime;
        ft : TFileTime;
    begin
      GetSystemTime(st);
      SystemTimeToFileTime(st, ft);
      result := IntToHexEx(int64(ft), 16) + '@madExcept>';
      result[1] := '<';
    end;

    function RecvStr(sock: TSocket; wantedReply: string; auth: TPBoolean = nil) : boolean;
    var buf    : array [0..1023] of char;
        i1     : integer;
        s1, s2 : string;
        fds    : TFDSet;
        tv     : TTimeVal;
        stop   : boolean;
    begin
      s1 := '';
      repeat
        stop := true;
        fds.fd_count := 1;
        fds.fd_array[0] := sock;
        tv.tv_sec := 7;
        tv.tv_usec := 0;
        if select(0, @fds, nil, nil, @tv) > 0 then begin
          i1 := recv(sock, buf, 1024, 0);
          SetString(s2, buf, i1);
          s1 := s1 + s2;
          result := Length(s1) > Length(wantedReply);
          if result then begin
            for i1 := 1 to Length(wantedReply) do
              if s1[i1] <> wantedReply[i1] then begin
                result := false;
                break;
              end;
            if result then begin
              for i1 := 1 to Length(s1) do
                if s1[i1] = '=' then
                  s1[i1] := ' ';
              repeat
                stop := s1[4] <> '-';
                i1 := PosStr(#$D#$A, s1);
                if i1 = 0 then
                  i1 := maxInt - 1;
                if (auth <> nil) and (not auth^) then begin
                  s2 := Copy(s1, 5, i1 - 5);
                  if PosTextIs1('AUTH ', s2) and (PosText(' LOGIN ', Copy(s2, 5, maxInt) + ' ') > 0) then
                    auth^ := true;
                end;
                Delete(s1, 1, i1 + 1);
              until stop or (Length(s1) < 4);
            end;
          end;
        end else
          result := false;
      until stop;
    end;

    function SendRecv(sock: TSocket; sendBuf, wantedReply: string; auth: TPBoolean = nil) : boolean;
    begin
      result := ( (sendBuf = '') or SendStr(sock, sendBuf + #$D#$A) ) and
                RecvStr(sock, wantedReply, auth);
    end;

    function SendFile(sock: TSocket; filePath, fileName, field: string) : boolean;
    var fh, c1, c2, c3 : dword;
        c4             : dword;
        arrCh          : array [0..56] of char;
        first          : boolean;
        fileType       : string;
    begin
      fh := CreateFile(pchar(filePath), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
      result := fh <> INVALID_HANDLE_VALUE;
      if result then begin
        c1 := GetFileSize(fh, nil);
        if c1 > 0 then begin
          c4 := c1;
          first := true;
          while c1 > 0 do begin
            if c1 > 57 then c2 := 57
            else            c2 := c1;
            if (not ReadFile(fh, arrCh, c2, c3, nil)) or (c2 <> c3) then begin
              if not first then
                SendStr(sock, #$D#$A);
              result := false;
              break;
            end;
            if IsTextEqual(CopyR(filePath, 4), '.png') then
                 fileType := 'image/png'
            else fileType := 'application/octet-stream';
            if first then
              SendStr(sock, '--' + CBoundary + #$D#$A +
                            'Content-Type: ' + fileType + '; ' +
// so sad: some email clients don't like this (e.g. the Bat)                            
//                            'name="' + field + '"' + #$D#$A +
                            'name="' + fileName + '"' + #$D#$A +
                            'Content-Transfer-Encoding: base64' + #$D#$A +
                            'Content-Disposition: attachment; ' +
                            'filename="' + fileName + '"' + #$D#$A +
                            #$D#$A);
            SendStr(sock, Encode(arrCh, c2) + #$D#$A);
            dec(c1, c2);
            first := false;
            pa.Position := (c4 - c1) * 1000 div c4;
          end;
          pa.AreaDone;
        end;
        CloseHandle(fh);
      end;
    end;

  var sock : TSocket;
      auth : boolean;
      i1   : integer;
      b1   : boolean;
  begin
    result := false;
    if server = '' then begin
      server := MxLookup(rcptTo);
      pa.AreaDone;
    end;
    if ConnectToServer(pa, server, sock) then begin
      if RecvStr(sock, '220') and
         ( SendRecv(sock, 'EHLO ' + CompName, '250', @auth) or
           SendRecv(sock, 'HELO ' + CompName, '250'       )    ) then begin
        pa.AreaDone;
        if mailFrom = '' then
          mailFrom := rcptTo;
        if ( ( (authUserName = '') and
               (authPassword = '')     ) or
             ( auth and
               SendRecv(sock, 'AUTH LOGIN',         '334') and
               SendRecv(sock, Encode(authUserName), '334') and
               SendRecv(sock, Encode(authPassword), '235')     ) ) then begin
          pa.AreaDone;
          if SendRecv(sock, 'MAIL FROM:' + FormatMailAddr(mailFrom, false), '250') then begin
            pa.AreaDone;
            b1 := false;
            for i1 := 1 to MailAddrCount(rcptTo) do
              b1 := SendRecv(sock, 'RCPT TO:' + FormatMailAddr(SubStr(rcptTo, i1, ','), false), '250') or b1;
            pa.AreaDone;
            if b1 then
              if (subject <> '') or (body <> '') then begin
                ReplaceStr(body, #$D#$A + '.' + #$D#$A, #$D#$A + '. ' + #$D#$A);
                subject := '=?ISO-8859-1?Q?' + EncodeStr(subject, CEncodeSmtp, '=') + '?=';
                if SendRecv(sock, 'DATA', '354') then begin
                  pa.AreaDone;
                  result := SendStr(sock, 'To: '         + FormatMailAddr(rcptTo,   true) + #$D#$A +
                                          'From: '       + FormatMailAddr(mailFrom, true) + #$D#$A +
                                          'Subject: '    + subject + #$D#$A +
                                          'Date: '       + GetDateTimeStr + #$D#$A +
                                          'Message-ID: ' + GetMessageId + #$D#$A);
                  if result then begin
                    pa.AreaDone;
                    if attachFiles <> nil then begin
                      result := SendStr(sock, 'MIME-Version: 1.0' + #$D#$A +
                                              'Content-Type: multipart/mixed; boundary="' + CBoundary + '"' + #$D#$A +
                                              #$D#$A +
                                              'This is a multi-part message in MIME format.' + #$D#$A +
                                              #$D#$A +
                                              '--' + CBoundary + #$D#$A +
                                              'Content-Type: text/plain; charset="iso-8859-1"' + #$D#$A +
                                              'Content-Transfer-Encoding: 8bit' + #$D#$A +
                                              #$D#$A +
                                              body + #$D#$A);
                      pa.AreaDone;
                      if result then
                        for i1 := 0 to high(attachFiles) do
                          if (GetFileAttributes(pchar(attachFiles[i1])) <> dword(-1)) and
                             (not SendFile(sock, attachFiles[i1], attachSendAs[i1], attachFields[i1])) then begin
                            result := false;
                            break;
                          end;
                      if result then
                        result := SendStr(sock, '--' + CBoundary + '--' + #$D#$A);
                    end else begin
                      result := SendStr(sock, #$D#$A + body);
                      pa.AreaDone;
                    end;
                  end;
                  result := SendRecv(sock, #$D#$A + '.', '250') and result;
                  pa.AreaDone;
                end;
              end else
                result := SendRecv(sock, 'RSET', '250');
          end;
        end;
        result := SendRecv(sock, 'QUIT', '221') and result;
        pa.AreaDone;
      end;
      closesocket(sock);
    end;
  end;

  function UploadIt(pa: IProgressAlert) : boolean;

    function RecvStr(sock: TSocket) : boolean;
    var buf    : array [0..5120] of char;
        s1     : string;
        fds    : TFDSet;
        tv     : TTimeVal;
        i1, i2 : integer;
    begin
      result := false;
      fds.fd_count := 1;
      fds.fd_array[0] := sock;
      tv.tv_sec := HttpUploadTimeout;
      tv.tv_usec := 0;
      if select(0, @fds, nil, nil, @tv) > 0 then begin
        SetString(s1, buf, recv(sock, buf, 5120, 0));
        if SubStr(s1, 2, ' ') = '100' then
          result := RecvStr(sock)
        else
          if SubStr(s1, 2, ' ') = '302' then begin
            i1 := PosText('Location: ', s1);
            if i1 > 0 then begin
              i2 := PosStr(#$D#$A, s1, i1 + 10);
              if i2 > 0 then begin
                Delete(s1, i2, maxInt);
                Delete(s1, 1, i1 + 9);
                ShellExecute(0, nil, pchar(s1), nil, nil, SW_SHOWNORMAL);
                result := true;
              end;
            end;
          end else
            result := SubStr(s1, 2, ' ') = '200';
      end;
    end;

    function SendFile(sock: TSocket; filePath, fileName, field: string) : boolean;
    var fh, c1, c2, c3 : dword;
        c4             : dword;
        buf            : string;
    begin
      fh := CreateFile(pchar(filePath), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
      result := fh <> INVALID_HANDLE_VALUE;
      if result then begin
        c1 := GetFileSize(fh, nil);
        if c1 > 0 then begin
          c4 := c1;
          SendStr(sock, #$D#$A + '--' + CBoundary + #$D#$A +
                        'Content-Disposition: form-data; name="' + field + '"; filename="' + fileName + '"' + #$D#$A +
                        'Content-Type: application/octet-stream' + #$D#$A + #$D#$A);
          SetLength(buf, 1024);
          while result and (c1 > 0) do begin
            if c1 > 1024 then c2 := 1024
            else              c2 := c1;
            SetLength(buf, c2);
            if (not ReadFile(fh, pointer(buf)^, c2, c3, nil)) or (c2 <> c3) then begin
              result := false;
              break;
            end;
            result := SendStr(sock, buf);
            dec(c1, c2);
            pa.Position := (c4 - c1) * 1000 div c4;
          end;
          pa.AreaDone;
        end;
        CloseHandle(fh);
      end;
    end;

    function GetProxyServer : string;
    type TInternetProxyInfo = packed record
           accessType  : dword;
           proxy       : pchar;
           proxyBypass : pchar;
         end;
    const INTERNET_OPTION_PROXY = 38;
          PROXY_TYPE_PROXY      = 2;
    var iqo    : function (internet, option: dword; buf: pointer; var bufLen: dword) : bool; stdcall;
        bufLen : dword;
        ipi    : ^TInternetProxyInfo;
    begin
      result := '';
      iqo := GetProcAddress(LoadLibrary('wininet.dll'), 'InternetQueryOptionA');
      if @iqo <> nil then begin
        bufLen := 0;
        iqo(0, INTERNET_OPTION_PROXY, nil, bufLen);
        if bufLen > 0 then begin
          ipi := pointer(LocalAlloc(LPTR, bufLen));
          if iqo(0, INTERNET_OPTION_PROXY, ipi, bufLen) then
            result := ipi.proxy;
          LocalFree(dword(ipi));
        end;
      end;
    end;

    procedure RemoveProtocol(var str: string);
    var i1 : integer;
    begin
      i1 := PosText('://', str);
      if i1 > 0 then
        Delete(str, 1, i1 + 2);
      i1 := PosText('http=', str);
      if i1 > 0 then begin
        Delete(str, 1, i1 + 4);
        str := SubStr(str, 1, ' ');
      end;
    end;

  var server  : string;
      host    : string;
      site    : string;
      i1, i2  : integer;
      size    : integer;
      sock    : TSocket;
      s1, s2  : string;
      fstr    : string;
      orgPort : dword;
  begin
    result := false;
    pa.AddArea(100, settings.ConnectMsg);
    pa.AddArea(100);
    fstr := '';
    if fields <> nil then
      for i1 := 0 to fields.ItemCount - 1 do
        fstr := fstr + #$D#$A + '--' + CBoundary + #$D#$A +
                'Content-Disposition: form-data; name="' + fields.Items[i1] + '"' + #$D#$A +
                #$D#$A +
                fields[fields.Items[i1]];
    pa.AddArea(100 + length(fstr), settings.FieldsMsg);
    size := length(fstr);
    for i1 := 0 to high(attachFiles) do begin
      i2 := GetAttachSize(attachFiles[i1]);
      if i2 > 0 then begin
        i2 := i2 + 102 + Length(CBoundary) + Length(attachSendAs[i1]) + Length(attachFields[i1]);
        pa.AddArea(i2 div 16, settings.SendAttachMsg);
        inc(size, i2);
      end;
    end;
    pa.AddArea(100);
    pa.AddArea(500, settings.SendFinalizeMsg);
    progressAlert.AreaDone;
    host := httpUrl;
    site := httpUrl;
    RemoveProtocol(site);
    for i1 := 1 to Length(site) do
      if site[i1] = '/' then begin
        host := Copy(site, 1, i1 - 1);
        Delete(site, 1, i1 - 1);
        break;
      end;
    orgPort := port;
    server := GetProxyServer;
    if server <> '' then begin
      RemoveProtocol(server);
      s2 := SubStr(server, 2, ':');
      if s2 <> '' then begin
        i1 := StrToIntEx(false, pchar(s2), Length(s2));
        if i1 <> -1 then begin
          server := SubStr(server, 1, ':');
          port := i1;
        end;
      end;
      site := httpUrl;
    end else
      server := host;
    if ConnectToServer(pa, server, sock) then begin
      s1 := 'POST ' + site + ' HTTP/1.1' + #$D#$A +
            'Host: ' + host + ':' + IntToStrEx(orgPort) + #$D#$A +
            'Connection: close' + #$D#$A +
            'Pragma: no-cache' + #$D#$A +
            'User-Agent: madExcept/3.0' + #$D#$A +
            'Content-Type: multipart/form-data; boundary=' + CBoundary + #$D#$A +
            'Content-Length: ' + IntToStrEx(size + 4 + Length(CBoundary) + 2) + #$D#$A;
      if mailFrom <> '' then
        s1 := s1 + 'From: ' + FormatMailAddr(mailFrom, false) + #$D#$A;
      if (authUserName <> '') or (authPassword <> '') then
        s1 := s1 + 'Authorization: Basic ' + Encode(authUserName + ':' + authPassword) + #$D#$A;
      s1 := s1 + fstr;
      if SendStr(sock, s1) then begin
        result := true;
        pa.AreaDone;
        for i1 := 0 to high(attachFiles) do
          if (GetFileAttributes(pchar(attachFiles[i1])) <> dword(-1)) and
             (not SendFile(sock, attachFiles[i1], attachSendAs[i1], attachFields[i1])) then begin
            result := false;
            break;
          end;
        SendStr(sock, #$D#$A + '--' + CBoundary + '--' + #$D#$A);
        pa.AreaDone;
        result := result and RecvStr(sock);
        pa.AreaDone;
      end;
      closesocket(sock);
    end;
  end;

var wd     : TWSAData;
    i1, i2 : integer;
begin
  result := false;
  if WSAStartup($101, wd) = 0 then begin
    progressAlert.ClearAreas;
    progressAlert.AddArea(1000, 'Prepare attachments...');
    if httpUrl <> '' then
      result := UploadIt(progressAlert)
    else
      if rcptTo <> '' then begin
        i2 := 0;
        if server = '' then begin
          for i1 := 1 to MailAddrCount(rcptTo) do
            i2 := Mail_PrepareProgress(progressAlert, true);
          progressAlert.AreaDone;
          for i1 := 1 to MailAddrCount(rcptTo) do begin
            result := MailIt(progressAlert, SubStr(rcptTo, i1, ',')) or result;
            progressAlert.CurrentArea := i2 * i1;
          end;
        end else begin
          i2 := Mail_PrepareProgress(progressAlert, false);
          progressAlert.AreaDone;
          result := MailIt(progressAlert, rcptTo);
          progressAlert.CurrentArea := i2;
        end;
      end;
    WSACleanup;
    if progressAlert <> nil then
      progressAlert.Close;
  end;
end;

function SendSmtpMail(mailFrom, rcptTo : string;
                      subject,  body   : string;
                      attachments      : IMEAttachments = nil;
                      server           : string         = '';
                      authUserName     : string         = '';
                      authPassword     : string         = '';
                      port             : dword          = 25;
                      parentWindow     : dword          = 0;
                      hidden           : boolean        = false;
                      background       : boolean        = false;
                      settings         : IMESettings    = nil  ) : boolean;
var attachFiles, attachSendAs, attachFields : TDAString;
    pa : IProgressAlert;
begin
  if settings = nil then
    settings := MESettingsEx(1);
  pa := PrepareAttachments(settings, attachments, attachFiles, attachSendAs, attachFields,
                           parentWindow, hidden, background);
  result := SmtpOrUpload('', mailFrom, rcptTo, subject, body,
                         attachFiles, attachSendAs, attachFields, nil,
                         server, authUserName, authPassword, port,
                         pa, settings);
end;

//function VerifySmtpAddress(email: string) : boolean;
//begin
//  result := SendSmtpMail(email, email, '', '');
//end;

function DecryptPassword(password, account, server: string) : string;
begin
  result := password;
  Decrypt(result, server,  $12345678);
  Decrypt(result, account, $77777777);
end;

function HttpUpload(httpUrl       : string;
                    attachments   : IMEAttachments;
                    fields        : IMEFields      = nil;
                    authUserName  : string         = '';
                    authPassword  : string         = '';
                    port          : dword          = 80;
                    mailFrom      : string         = '';
                    parentWindow  : dword          = 0;
                    hidden        : boolean        = false;
                    background    : boolean        = false;
                    settings      : IMESettings    = nil  ) : boolean;
var attachFiles, attachSendAs, attachFields : TDAString;
    pa : IProgressAlert;
begin
  if settings = nil then
    settings := MESettingsEx(1);
  pa := PrepareAttachments(settings, attachments, attachFiles, attachSendAs, attachFields,
                           parentwindow, hidden, background);
  result := SmtpOrUpload(httpUrl, mailFrom, '', '', '',
                         attachFiles, attachSendAs, attachFields, fields,
                         '', authUserName, authPassword, port,
                         pa, settings);
end;

// ***************************************************************
// action functions

function GetBugReportFile(settings: IMESettings) : string;
begin
  result := settings.BugReportFile;
  ReplaceText(result, '%appname%', KillExt(ExtractFileName(ModuleName(0              ))));
  ReplaceText(result, '%modname%', KillExt(ExtractFileName(ModuleName(settings.Module))));
end;

// save the bug report to a file, eventually show a "save as" dialog
function SaveBugReportEx(bugReportFile, bugReport: string;
                         parentWindow: dword; noDialog: boolean;
                         settings: IMESettings) : boolean;

  function ForceDir(path: string) : boolean;
  begin
    path := ExtractFilePath(path);
    if path <> '' then begin
      if path[Length(path)] = '\' then
        Delete(path, Length(path), 1);
      result := (GetFileAttributes(pchar(path)) <> dword(-1)) or
                ( ForceDir(path) and CreateDirectory(pchar(path), nil) );
    end else
      result := false;
  end;

  function ExtractHeaderInfo(bugReport, item: string) : string;
  var i1, i2 : integer;
  begin
    result := '';
    i1 := PosStr(#$D#$A + item, bugReport);
    if i1 > 0 then begin
      i2 := PosStr(#$D#$A, bugReport, i1 + 2 + Length(item));
      if i2 = 0 then
        i2 := maxInt;
      result := Copy(bugReport, i1 + 2 + Length(item), i2 - i1 - 2 - Length(item));
      for i1 := 1 to Length(result) do
        case result[i1] of
          ':' : begin
                  Delete(result, 1, i1 + 1);
                  break;
                end;
          ' ' : ;
          else  begin
                  result := '';
                  break;
                end;
        end;
    end;
  end;

  procedure AddHeaderInfo(var bugReport: string; item, afterItem, content: string);
  var i1, i2, i3 : integer;
  begin
    i1 := PosStr(#$D#$A + afterItem, bugReport);
    if i1 > 0 then begin
      i2 := PosStr(#$D#$A, bugReport, i1 + 2 + Length(item));
      if i2 > 0 then begin
        i3 := PosStr(':', bugReport, i1 + 2 + Length(item), i2);
        if i3 > 0 then
          Insert(#$D#$A + FillStr(item, - i3 + i1 + 2) + ': ' + content, bugReport, i2);
      end;
    end;
  end;

  function CompareHeaderInfo(br1, br2, item: string) : boolean;
  begin
    result := ExtractHeaderInfo(br1, item) = ExtractHeaderInfo(br2, item);
  end;

type TOpenFileName = packed record
       lStructSize: DWORD;
       hWndOwner: HWND;
       hInstance: HINST;
       lpstrFilter: PAnsiChar;
       lpstrCustomFilter: PAnsiChar;
       nMaxCustFilter: DWORD;
       nFilterIndex: DWORD;
       lpstrFile: PAnsiChar;
       nMaxFile: DWORD;
       lpstrFileTitle: PAnsiChar;
       nMaxFileTitle: DWORD;
       lpstrInitialDir: PAnsiChar;
       lpstrTitle: PAnsiChar;
       Flags: DWORD;
       nFileOffset: Word;
       nFileExtension: Word;
       lpstrDefExt: PAnsiChar;
       lCustData: LPARAM;
       lpfnHook: function(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): UINT stdcall;
       lpTemplateName: PAnsiChar;
     end;
var c1, c2     : dword;
    ofn        : TOpenFileName;
    s1, s2, s3 : string;
    i1, i2, i3 : integer;
    map        : dword;
    buf        : pchar;
    def        : TMEDupDef;
    b1         : boolean;
    exc        : IMEExceptionEx;
begin
  result := false;
  if bugReportFile = '' then begin
    s1 := GetBugReportFile(settings);
    if (Length(s1) <= 2) or ((s1[1] <> '\') and (s1[2] <> ':')) then begin
      s2 := ExtractFilePath(ModuleName(0));
      if not noDialog then begin
        if IsProcessBlocked(250) then begin
          MessageBeep(0);
          exit;
        end;
        s1 := s1 + #0;
        SetLength(s1, MAX_PATH + 1);
        ZeroMemory(@ofn, sizeOf(ofn));
        ofn.lStructSize     := sizeOf(ofn);
        ofn.hWndOwner       := parentWindow;
        ofn.lpstrInitialDir := pchar(s2);
        ofn.lpstrFilter     := '*.txt' + #0 + '*.txt' + #0;
        ofn.nFilterIndex    := 1;
        ofn.lpstrFile       := pchar(s1);
        ofn.nMaxFile        := MAX_PATH;
        s3 := settings.SaveBtnCaption;
        ofn.lpstrTitle      := pchar(s3);
        ofn.lpstrDefExt     := '.txt';
        ofn.Flags           := OFN_NOREADONLYRETURN or OFN_HIDEREADONLY;
        if not GetSaveFileName(POpenFileName(@ofn)^) then
          exit;
        s1 := ofn.lpstrFile;
      end else
        s1 := s2 + s1;
    end;
  end else
    s1 := bugReportFile;
  ForceDir(s1);
  c1 := CreateFile(pchar(s1), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_ALWAYS, 0, 0);
  if c1 <> INVALID_HANDLE_VALUE then begin
    if settings.AppendBugReports then begin
      c2 := GetFileSize(c1, nil);
      s1 := '';
      if c2 > 0 then begin
        s1 := ExtractHeaderInfo(bugReport, 'callstack crc');
        if Length(s1) = 31 then begin
          if ExtractHeaderInfo(bugReport, 'exception class') <> '' then begin
            b1  := settings.NoDupExcepts;
            def := settings.DupExceptDef;
          end else begin
            b1  := settings.NoDupFreezes;
            def := settings.DupFreezeDef;
          end;
          if not b1 then
            s1 := ''
          else
            case def of
              ddExceptAddrIdentical : SetLength(s1, 9);
              ddCrashStackIdentical : SetLength(s1, 20);
            end;
        end else
          s1 := '';
      end;
      b1 := (settings.BugReportFileSize > 0) and (c2 > settings.BugReportFileSize);
      if b1 or (s1 <> '') then begin
        map := CreateFileMapping(c1, nil, PAGE_READWRITE, 0, 0, nil);
        if map <> 0 then begin
          buf := MapViewOfFile(map, FILE_MAP_ALL_ACCESS, 0, 0, 0);
          if buf <> nil then begin
            if s1 <> '' then begin
              i1 := c2;
              repeat
                i1 := PosPChar(pchar(': ' + s1), buf, Length(s1) + 2, c2, false, i1 - 1, 0);
                if i1 >= 0 then begin
                  i2 := PosPChar(#$D#$A + 'date/time', buf, 11, c2, false, i1, 0);
                  if i2 < 0 then
                       i2 := PosPChar('date/time', buf, 9, c2, false, 0, 0)
                  else inc(i2, 2);
                  if i2 >= 0 then begin
                    i3 := PosPChar(#$D#$A + 'date/time', buf, 11, c2, false, i1, maxInt);
                    if i3 < 0 then
                         i3 := c2
                    else inc(i3, 2);
                    SetString(s2, pchar(buf) + i2, i3 - i2);
                    if CompareHeaderInfo(s2, bugReport, 'executable'     ) and
                       CompareHeaderInfo(s2, bugReport, 'current module' ) and
                       CompareHeaderInfo(s2, bugReport, 'exception class') and
                       CompareHeaderInfo(s2, bugReport, 'version'        ) and
                       ( (ExtractHeaderInfo(s2, 'version') <> '') or
                         ( CompareHeaderInfo(s2, bugReport,  'exec. date/time') and
                           CompareHeaderInfo(s2, bugReport, 'module date/time')     ) ) then begin
                      s1 := ExtractHeaderInfo(s2, 'count');
                      if s1 <> '' then
                           i1 := StrToIntEx(false, pchar(s1), Length(s1))
                      else i1 := 1;
                      AddHeaderInfo(bugReport, 'count', 'callstack crc', IntToStrEx(i1 + 1));
                      if dword(i3) < c2 then
                        Move((buf + i3)^, (buf + i2)^, c2 - dword(i3));
                      c2 := c2 - dword(i3 - i2);
                      break;
                    end;
                  end;
                end;
              until i1 = -1;
            end;
            if b1 then begin
              SetFilePointer(c1, 0, nil, FILE_BEGIN);
              i1 := 0;
              repeat
                i1 := PosPChar(#$D#$A + 'date/time', buf, 11, c2, false, i1 + 1);
              until (i1 = -1) or (c2 - dword(i1) < settings.BugReportFileSize - settings.BugReportFileSize div 4);
              if i1 <> -1 then begin
                inc(i1, 2);
                c2 := c2 - dword(i1);
                Move((buf + i1)^, buf^, c2);
              end;
            end;
            UnmapViewOfFile(buf);
          end;
          CloseHandle(map);
        end;
      end;
      SetFilePointer(c1, c2, nil, FILE_BEGIN);
    end;
    bugReport := bugReport + #$D#$A + #$D#$A;
    WriteFile(c1, pointer(bugReport)^, length(bugReport), c2, nil);
    SetEndOfFile(c1);
    CloseHandle(c1);
    if (bugReportFile = '') and (settings.QueryInterface(IMEExceptionEx, exc) = 0) then
      exc.SetMailWasSaved(true);
    result := true;
  end;
end;

function SaveBugReport(bugReport: string; parentWindow: dword; settings: IMESettings) : boolean;
begin
  if settings = nil then
    settings := MESettingsEx(1);
  result := SaveBugReportEx('', bugReport, parentWindow, false, settings);
end;

function AutoSaveBugReport(bugReport: string; settings: IMESettings) : boolean;
begin
  if settings = nil then
    settings := MESettingsEx(1);
  result := SaveBugReportEx('', bugReport, 0, true, settings);
end;

function SendBugReportEx(bugReport: string; screenShot: INVBitmap;
                         parentWindow: dword; hidden, background: boolean;
                         settings: IMESettings; inThread: boolean; pth: TPCardinal = nil) : TExtBool;
type
  TSendBugReportExRec = record
    bugReport  : string;
    screenShot : INVBitmap;
    hidden     : boolean;
    settings   : IMESettings;
  end;

  function SendBugReportExThread(var params: TSendBugReportExRec) : integer; stdcall;
  begin
    result := 0;
    with params do
      SendBugReportEx(bugReport, screenShot, 0, hidden, true, settings, true);
    Finalize(params);
    LocalFree(dword(@params));
  end;

var s1, s2, s3, s4   : string;
    brf              : string;
    dbrf             : boolean;
    i1, i2           : integer;
    subject          : string;
    body             : string;
    mailFrom         : string;
    rcptTo           : string;
    pf               : TPngFormat;
    fields           : IMEFields;
    exc              : IMEExceptionEx;
    attach           : IMEAttachments;
    das1, das2, das3 : TDAString;
    pa               : IProgressAlert;
    params           : ^TSendBugReportExRec;
    th, tid          : dword;
begin
  das1 := nil;
  das2 := nil;
  das3 := nil;
  if pth <> nil then
    pth^ := 0;
  if background and (not inThread) and (not IsProcessBlocked(250)) then begin
    if parentWindow <> 0 then
      SetWindowPos(parentWindow, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
    params := pointer(LocalAlloc(LPTR, sizeOf(TSendBugReportExRec)));
    params.bugReport  := bugReport;
    params.screenShot := screenShot;
    params.hidden     := hidden;
    params.settings   := settings;
    th := CreateThread(nil, 0, @SendBugReportExThread, params, 0, tid);
    if th <> 0 then begin
      result := yes;
      if pth <> nil then
           pth^ := th
      else CloseHandle(th);
      exit;
    end;
    Finalize(params^);
    LocalFree(dword(params));
  end;
  subject := settings.MailSubject;
  body    := settings.MailBody;
  rcptTo  := settings.MailAddr;
  if (settings.MailFrom = '') and (settings.MailAddr <> '') then begin
    mailFrom := settings.MailAddr;
    if MailAddrCount(mailFrom) > 1 then
      mailFrom := SubStr(mailFrom, 1, ',');
    mailFrom := 'madExcept ' + FormatMailAddr(mailFrom, false);
  end else
    mailFrom := settings.MailFrom;
  s1 := '';
  i1 := PosStr(#$D#$A + 'exception message ', bugReport);
  if i1 > 0 then begin
    i2 := PosStr(#$D#$A, bugReport, i1 + 10);
    if i2 > 0 then begin
      s1 := Copy(bugReport, i1, i2 - i1);
      i1 := PosStr(':', s1);
      Delete(s1, 1, i1);
      TrimStr(s1);
    end;
  end;
  ExpandVars(settings.Module, subject, s1, bugReport);
  ExpandVars(settings.Module, body,    s1, bugReport);
  attach := NewAttachments;
  dbrf := false;
  brf := '';                     
  if settings.MailAsSmtpServer or settings.MailAsSmtpClient or settings.UploadViaHttp or settings.MailViaMapi then begin
    if settings.AttachBugReport then begin
      brf := GetBugReportFile(settings);
      if (Length(brf) <= 2) or ((brf[1] <> '\') and (brf[2] <> ':')) then
        brf := ExtractFilePath(ModuleName(settings.Module)) + brf;
      s2 := GetTempPath;
      if s2 <> '' then begin
        s2 := s2 + ExtractFileName(brf);
        if settings.AttachBugReportFile and CopyFile(pchar(brf), pchar(s2), false) then
          dbrf := settings.DeleteBugReportFile
        else
          DeleteFile(pchar(s2));
        if (not settings.AttachBugReportFile) or
           (settings.QueryInterface(IMEExceptionEx, exc) <> 0) or
           (not exc.GetMailWasSaved) then
          SaveBugReportEx(s2, bugReport, 0, true, settings);
        if GetFileAttributes(pchar(s2)) <> dword(-1) then
          attach.Add(s2, settings.BugReportSendAs, settings.BugReportZip, 'BugReport');
      end;
    end;
    if screenShot <> nil then begin
      s2 := GetTempPath;
      if s2 <> '' then begin
        s2 := s2 + 'screenShot.png';
        case settings.ScreenShotDepth of
          4          : pf := pf16Grays;
          50  * 1024 : pf := pf50kb;
          100 * 1024 : pf := pf100kb;
          200 * 1024 : pf := pf200kb;
          300 * 1024 : pf := pf300kb;
          else         pf := pf256Colors;
        end;
        if screenShot.SavePng(s2, pf) then
          attach.Add(s2, settings.ScreenShotSendAs, settings.ScreenShotZip, 'ScreenShot');
      end;
    end;
    for i1 := 0 to settings.AdditionalAttachments.ItemCount - 1 do begin
      settings.AdditionalAttachments.GetItem(i1, s1, s2, s3, s4);
      attach.Add(s1, s2, s3, s4);
    end;
    pa := PrepareAttachments(settings, attach, das1, das2, das3, parentWindow, hidden, inThread);
  end else
    pa := nil;
  fields := settings.AdditionalFields.Clone;
  if mailFrom <> '' then
    fields['MailFrom'] := mailFrom;
  if settings.MailAsSmtpServer then
    boolean(result) := (rcptTo <> '') and AmOnline and
                       SmtpOrUpload('', mailFrom, rcptTo, subject, body, das1, das2, das3, nil,
                                    '', '', '', 25,
                                    pa, settings)
  else
    if settings.MailAsSmtpClient then
      boolean(result) := (rcptTo <> '') and (settings.SmtpServer <> '') and AmOnline and
                         SmtpOrUpload('', mailFrom, rcptTo, subject, body, das1, das2, das3, nil,
                                      settings.SmtpServer, settings.SmtpAccount, settings.SmtpPassword, settings.SmtpPort,
                                      pa, settings)
    else
      if settings.UploadViaHttp then
        boolean(result) := (settings.HttpServer <> '') and AmOnline and
                           SmtpOrUpload(settings.HttpServer, mailFrom, '', '', '', das1, das2, das3, fields,
                                        '', settings.HttpAccount, settings.HttpPassword, settings.HttpPort,
                                        pa, settings)
      else
        result := no;
  if pa <> nil then
    pa.Close;
  if result = no then begin
    if settings.MailViaMapi then
      result := SendMapiMailEx(rcptTo, subject, body, das1, das2, parentWindow, hidden);
    if (result = no) and settings.MailViaMailto and (not hidden) then begin
      dbrf := false;  // don't delete the bug report file, because SendShellMail can't send attachments
      if parentWindow <> 0 then
        SetWindowPos(parentWindow, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
      boolean(result) := SendShellMail(rcptTo, subject, bugReport);
    end;
    if (result = no) and (not hidden) then
      MessageBox(parentWindow, pchar(settings.SendFailureMsg), nil, MB_ICONERROR);
  end;
  if result = yes then begin
    if settings.QueryInterface(IMEExceptionEx, exc) = 0 then
      exc.SetMailWasSent(true);
    if dbrf and (brf <> '') and settings.DeleteBugReportFile then begin
      SetFileAttributes(pchar(brf), 0);
      DeleteFile(pchar(brf));
    end;
  end;
end;

function SendBugReport(bugReport: string; screenShot: INVBitmap;
                       parentWindow: dword; settings: IMESettings) : TExtBool;
begin
  if settings = nil then
    settings := MESettingsEx(1);
  result := SendBugReportEx(bugReport, screenShot, parentWindow, false, settings.SendInBackground, settings, false);
end;

function AutoSendBugReport(bugReport: string; screenShot: INVBitmap; settings: IMESettings) : TExtBool;
begin
  if settings = nil then
    settings := MESettingsEx(1);
  result := SendBugReportEx(bugReport, screenShot, 0, true, true, settings, false);
end;

function PrintBugReport(bugReport: string; parentWindow: dword; settings: IMESettings) : boolean;
var pd       : TPrintDlg;
    doc      : TDocInfo;
    s1, s2   : string;
    sxm, sym : integer;
    sxp, syp : integer;
    ix1, iy1 : integer;
    iy       : integer;
    i1       : integer;
    tm       : TTextMetric;
    newFont  : dword;
    oldFont  : dword;
    r1       : TRect;
begin
  if IsProcessBlocked(250) then begin
    result := false;
    MessageBeep(0);
    exit;
  end;
  if settings = nil then
    settings := MESettingsEx(1);
  ZeroMemory(@pd, sizeOf(pd));
  pd.lStructSize := sizeOf(pd);
  pd.hWndOwner := parentWindow;
  pd.Flags := PD_RETURNDC or PD_HIDEPRINTTOFILE or PD_NOPAGENUMS or PD_NOSELECTION or
              PD_NOWARNING or PD_NONETWORKBUTTON or PD_USEDEVMODECOPIESANDCOLLATE;
  if PrintDlg(pd) then begin
    SetMapMode(pd.hDC, MM_TEXT);
    syp := GetDeviceCaps(pd.hDC, VERTRES);   // device units
    sym := GetDeviceCaps(pd.hDC, VERTSIZE);  // mm
    iy1 := (10 * syp + sym div 2) div sym - GetDeviceCaps(pd.hDC, PHYSICALOFFSETY);
    syp := syp - iy1 * 2;
    sxp := GetDeviceCaps(pd.hDC, HORZRES);   // device units
    sxm := GetDeviceCaps(pd.hDC, HORZSIZE);  // mm
    ix1 := (20 * sxp + sxm div 2) div sxm - GetDeviceCaps(pd.hDC, PHYSICALOFFSETX);
    sxp := sxp - ix1 - ((10 * sxp + sxm div 2) div sxm - GetDeviceCaps(pd.hDC, PHYSICALOFFSETX));
    newFont := CreateFont(-(8 * GetDeviceCaps(pd.hDC, LOGPIXELSY) div 72), 0, 0, 0, 400, 0, 0, 0, DEFAULT_CHARSET,
                          OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                          DEFAULT_PITCH or FF_DONTCARE, 'Courier New');
    if newFont <> 0 then
         oldFont := SelectObject(pd.hDC, newFont)
    else oldFont := 0;
    GetTextMetrics(pd.hDC, tm);
    ZeroMemory(@doc, sizeOf(doc));
    doc.cbSize := sizeOf(doc);
    s1 := settings.PrintBtnCaption;
    doc.lpszDocName := pchar(s1);
    if StartDoc(pd.hDC, doc) > 0 then begin
      if StartPage(pd.hDC) > 0 then begin
        s1 := bugReport;
        iy := iy1;
        while s1 <> '' do begin
          i1 := PosStr(#$D#$A, s1);
          if i1 > 0 then begin
            s2 := Copy(s1, 1, i1 - 1);
            Delete(s1, 1, i1 + 1);
          end else begin
            s2 := s1;
            s1 := '';
          end;
          r1.Left   := 0;
          r1.Top    := 0;
          r1.Right  := sxp;
          r1.Bottom := syp;
          if s2 = '' then
            s2 := ' ';
          DrawText(pd.hDC, pchar(s2), Length(s2), r1, DT_CALCRECT or DT_WORDBREAK or DT_EXTERNALLEADING);
          if iy + r1.Bottom > syp then begin
            iy := iy1;
            EndPage(pd.hDC);
            StartPage(pd.hDC);
          end;
          OffsetRect(r1, ix1, iy);
          DrawText(pd.hDC, pchar(s2), Length(s2), r1, DT_WORDBREAK or DT_EXTERNALLEADING);
          iy := r1.Bottom;
        end;
        EndPage(pd.hDC);
      end;
      EndDoc(pd.hDC);
    end;
    if oldFont <> 0 then
      SelectObject(pd.hDC, oldFont);
    if newFont <> 0 then
      DeleteObject(newFont);
    DeleteDC(pd.hDC);
  end;
  result := false;
end;

// restart ourself with the same command line
procedure RestartApplication;
var sa  : TSecurityAttributes;
    sd  : TSecurityDescriptor;
    si  : TStartupInfo;
    pi  : TProcessInformation;
    map : dword;
    buf : TPACardinal;
begin
  ZeroMemory(@si, sizeOf(si));
  si.cb := sizeOf(si);
  if CreateProcess(nil, GetCommandLine, nil, nil, false, CREATE_SUSPENDED, nil, nil, si, pi) then begin
    InitSecAttr(sa, sd);
    map := CreateFileMapping(maxCard, @sa, PAGE_READWRITE, 0, 8 + 80,
                             pchar('madExceptRestart' + IntToHexEx(pi.dwProcessId)));
    buf := MapViewOfFile(map, FILE_MAP_ALL_ACCESS, 0, 0, 0);
    if buf <> nil then begin
      buf^[0] := GetCurrentProcessId;
      DuplicateHandle(GetCurrentProcess, map, pi.hProcess, @buf^[1], 0, false, DUPLICATE_SAME_ACCESS);
      Move(Last10Exceptions, buf^[2], 80);
    end;
    ResumeThread(pi.hThread);
  end;
  TerminateProcess(GetCurrentProcess, 2);
end;

procedure CloseApplication;
begin
  TerminateProcess(GetCurrentProcess, 1);
end;

// ***************************************************************
// exception dialog stuff

const
  // width of the exception box when the bug report is visible
  CFullWidth = 88 * 7 + 48;

type
  // types for our exception box list view
  TColumn = record
    caption   : string;
    format    : dword;  // LVCFMT_LEFT / LVCFMT_RIGHT / LVCFMT_CENTER
    upperCase : boolean;
  end;
  TColumns = array of TColumn;
  TItem = record
    captions   : string;
    forceColor : dword;
    title      : boolean;
  end;
  TItems = array of TItem;

  // this is the class for our sweet little exception box
  TExceptionBox = class
    FDialogWndProc                  : pointer;
    FTmpDC, FTmpBmp                 : dword;
    FFont1, FFont2, FFont3          : dword;
    FTmpWidth                       : integer;
    FTmpBits                        : TPCardinal;
    FOldBmp, FOldFont               : dword;
    FMainWnd, FTabs, FEdit          : dword;
    FSend, FSave, FPrint, FDetails  : dword;
    FClose, FRestart, FContinue     : dword;
    FTimerBtn                       : dword;
    FLastFocusTime                  : dword;
    FDefaultMsgBox                  : boolean;
    FReshow                         : boolean;
    FBtnHeight                      : integer;
    FTime, FTimer                   : dword;
    FOldX                           : integer;
    FIcoWnd                         : dword;
    FExc                            : IMEException;
    FMinWidth                       : integer;
    FMinHeight                      : integer;
    FSizingDone                     : boolean;
    FColumns                        : TColumns;
    FItems                          : TItems;
    FImageList                      : dword;
    FListViewWndProc                : pointer;
    FOldListViewWndProc             : pointer;
    FCurrentTab                     : integer;
    FCurrentTabContent              : string;
    FShuttingDown                   : boolean;
    {$ifdef noncommercial}
      FNCLabelX, FNCLabelY, FNCLabelW : integer;
      FNCLabelIn                      : boolean;
    {$endif}
    constructor Create (exc: IMEException; defaultMsgBox, detailsBtn, makeSound: boolean);
    destructor Destroy; override;
    function  GetTextWidth     (add: integer; str: string; vis: boolean; lastW: TPInteger = nil) : integer;
    function  DrawGraphic      (hDC, offset, module, window, backColor, size: dword; bits: TPCardinal) : boolean;
    procedure DrawButton       (textColor, backColor, decColor, window, hDC: dword;
                                newX: integer = -1; pr: PRect = nil;
                                pushed: boolean = false; focused: boolean = false);
    procedure FlashButton      (f1, b1, d1, f2, b2, d2, window, hDC: dword);
    function  DialogWndMethod  (window, msg: dword; wParam, lParam: integer) : integer; stdcall;
    function  ListViewWndProc  (window, msg: dword; wParam, lParam: integer) : integer; stdcall;
    procedure BugReportChanged (complete: boolean; bugReport: string; exceptIntf: IMEException);
    procedure CloseMainWnd;
    procedure DoDetails;
    procedure CheckTabs;
    procedure CheckCurrentTab;
    property  Handle : dword read FMainWnd;
  end;

// helper function for the exception box layout calculation
function TExceptionBox.GetTextWidth(add: integer; str: string; vis: boolean; lastW: TPInteger = nil) : integer;
var size : TSize;
begin
  result := 0;
  if vis and GetTextExtentPoint32(FTmpDC, pchar(str), Length(str), size) then
    result := size.cx + add;
  if (lastW <> nil) and (result > lastW^) then lastW^ := result;
end;

function CheckColDec(col, dif: dword) : boolean;
// shall we make our exception box buttons lighter or darker than the window?
var i1 : integer;
begin
  i1 := 0;
  if col and $0000ff + dif and $0000ff > $0000ff then inc(i1);
  if col and $00ff00 + dif and $00ff00 > $00ff00 then inc(i1);
  if col and $ff0000 + dif and $ff0000 > $ff0000 then inc(i1);
  result := i1 <= 1;
end;

procedure DecCol(var col: dword; dif: dword; dec: boolean);
// calculate a ligher (dec = true) or darker (dec = false) color
var b1, b2, b3 : byte;
    d1, d2, d3 : byte;
begin
  b1 := byte(col       ); d1 := byte(dif       );
  b2 := byte(col shr  8); d2 := byte(dif shr  8);
  b3 := byte(col shr 16); d3 := byte(dif shr 16);
  if dec then begin
    if dword(b1) + dword(d1) > $ff then b1 := $ff else b1 := b1 + d1;
    if dword(b2) + dword(d2) > $ff then b2 := $ff else b2 := b2 + d2;
    if dword(b3) + dword(d3) > $ff then b3 := $ff else b3 := b3 + d3;
  end else begin
    if b1 < d1 then b1 := 0 else b1 := b1 - d1;
    if b2 < d2 then b2 := 0 else b2 := b2 - d2;
    if b3 < d3 then b3 := 0 else b3 := b3 - d3;
  end;
  col := b3 shl 16 + b2 shl 8 + b1;
end;

function TExceptionBox.DrawGraphic(hDC, offset, module, window, backColor, size: dword; bits: TPCardinal) : boolean;
const CRes : array [0..8] of pchar = ('send', 'save', 'print', 'show', 'continue', 'restart', 'close', 'cantContinue', 'big');
var graphic  : dword;     // icon or bitmap handle
    bdc, obh : dword;     // bitmap dc, old bitmap handle
    i1       : cardinal;
    alpha    : dword;
    name     : string;
    c1, c2   : dword;
    p1       : pointer;
    bi       : TBitmapInfo;
    pbih     : PBitmapInfoHeader;
    buf      : TDACardinal;
begin
  buf := nil;
  name := 'mei' + CRes[GetWindowLong(window, GWL_USERDATA)];
  result := false;
  c1 := FindResource(module, pchar(name), RT_BITMAP);
  if (c1 <> 0) and (SizeOfResource(module, c1) = sizeOf(pbih^) + size * size * 4) then begin
    c2 := LoadResource(module, c1);
    if c2 <> 0 then begin
      SetLength(buf, size * size);
      pbih := LockResource(c2); Move(pbih^, bi, sizeOf(pbih^));
      inc(pbih);                Move(pbih^, buf[0], size * size * 4);
      UnlockResource(c2);
      FreeResource(c2);
      backColor := (backColor and $0000FF) shl 16 +    // swap rgb to bgr
                   (backColor and $00FF00)        +
                   (backColor and $FF0000) shr 16;
      if bits <> nil then
        inc(bits, integer(offset) * FTmpWidth + integer(offset));
      for i1 := 0 to size * size - 1 do begin
        alpha := buf[i1] shr 24 + 1;
        if bits <> nil then begin
          if (i1 > 0) and (i1 mod size = 0) then
            inc(bits, FTmpWidth - integer(size));
          backColor := bits^;
          inc(bits);
        end;
        buf[i1] := ( (      alpha  * (buf[i1]   and $00FF00FF) +
                     (256 - alpha) * (backColor and $00FF00FF)   ) shr 8) and $00FF00FF +
                   ( (      alpha  * (buf[i1]   and $0000FF00) +
                     (256 - alpha) * (backColor and $0000FF00)   ) shr 8) and $0000FF00;
      end;
      bdc := CreateCompatibleDC(0);
      if bdc <> 0 then begin
        p1 := nil;
        graphic := CreateDIBSection(bdc, bi, DIB_RGB_COLORS, p1, 0, 0);
        if (graphic <> 0) and (p1 <> nil) then begin
          GdiFlush;
          Move(buf[0], p1^, size * size * 4);
          GdiFlush;
          obh := SelectObject(bdc, graphic);
          BitBlt(hDC, offset, offset, size, size, bdc, 0, 0, SRCCOPY);
          SelectObject(bdc, obh);
          DeleteObject(graphic);
          result := true;
        end;
        DeleteDC(bdc);
      end;
    end;
  end;
end;

// draw an exception box button in the specified colors
procedure TExceptionBox.DrawButton(textColor, backColor, decColor, window, hDC: dword;
                                   newX: integer = -1; pr: PRect = nil;
                                   pushed: boolean = false; focused: boolean = false);

  procedure DrawLine(x, h, col, offset: integer);
  var pen, oldPen : dword;
  begin
    pen := CreatePen(PS_SOLID, 1, col);
    oldPen := SelectObject(FTmpDC, pen);
    if MoveToEx(FTmpDC, x, offset * 2, nil) then
      LineTo(FTmpDC, x, h - offset * 2);
    SelectObject(FTmpDC, oldPen);
    DeleteObject(pen);
  end;

var r1, r2 : TRect;
    brush  : dword;
    arrCh  : array [0..30] of char;
    len    : dword;
    ix     : integer;
    i1     : integer;
    b1     : boolean;
    offset : integer;
    col1, col2, col3, col4, col5 : integer;
begin
  if (decColor <> 0) and (GetDeviceCaps(FTmpDC, BitsPixel) >= 15) then begin
    b1 := CheckColDec(backColor, decColor);
    if IsWindowEnabled(window) then DecCol(backColor, decColor,       b1)
    else                            DecCol(backColor, decColor div 2, b1);
  end;
  if pr <> nil then r1 := pr^
  else              GetClientRect(window, r1);
  if FExc.NoOwnerDrawButtons then begin
    offset := 2;
    if pushed then
         DrawFrameControl(FTmpDC, r1, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_PUSHED)
    else DrawFrameControl(FTmpDC, r1, DFC_BUTTON, DFCS_BUTTONPUSH);
    col1 := GetSysColor(COLOR_3DDKSHADOW);
  end else begin
    offset := 0;
    brush := CreateSolidBrush(backColor);
    FillRect(FTmpDC, r1, brush);
    DeleteObject(brush);
    col1 := GetSysColor(COLOR_BTNFACE);
  end;
  if (window = FTimerBtn) and (FTime <> 0) then begin
    if newX <> -1 then
      FOldX := newX;
    ix := FOldX;
    DrawLine(ix, r1.Bottom, col1, offset);
    if GetDeviceCaps(FTmpDC, BitsPixel) >= 15 then begin
      col2 := col1      and $00FF00; col1 := col1      and $FF00FF;
      col4 := backColor and $00FF00; col3 := backColor and $FF00FF;
      for i1 := 0 to 3 do begin
        col5 := ((((4 - i1) * col1 + i1 * col3) shr 2) and $FF00FF) +
                ((((4 - i1) * col2 + i1 * col4) shr 2) and $00FF00);
        DrawLine(ix + i1, r1.Bottom, col5, offset);
        DrawLine(ix - i1, r1.Bottom, col5, offset);
      end;
    end;
  end;
  if FExc.NoOwnerDrawButtons and focused then begin
    r2 := r1;
    InflateRect(r2, -3, -3);
    DrawFocusRect(FTmpDC, r2);
  end;
  DrawGraphic(FTmpDC, 3 + offset, FExc.Module, window, backColor, 16, FTmpBits);
  len := GetWindowText(window, arrCh, 30);
  ix := (r1.Right - r1.Left - GetTextWidth(0, arrCh, true) - 27 - 8) div 2;
  if not IsWindowEnabled(window) then begin
    SetTextColor(FTmpDC, $FFFFFF);
    TextOut(FTmpDC, 28 + ix, 5 + offset, arrCh, len);
    SetTextColor(FTmpDC, GetSysColor(COLOR_3DDKSHADOW));
  end else
    SetTextColor(FTmpDC, textColor);
  TextOut(FTmpDC, 27 + ix, 4 + offset, arrCh, len);
  BitBlt(hDC, 0, 0, r1.Right, r1.Bottom, FTmpDC, 0, 0, SRCCOPY);
end;

// alpha blend an exception box button
procedure TExceptionBox.FlashButton(f1, b1, d1, f2, b2, d2, window, hDC: dword);
var b11, b12, b21, b22 : integer;
    f11, f12, f21, f22 : integer;
    i1                 : integer;
    r1                 : TRect;
begin
  GetClientRect(window, r1);
  if GetDeviceCaps(FTmpDC, BitsPixel) >= 15 then
    if d1 <> 0 then
         DecCol(b1, d1, CheckColDec(b1, d1))
    else DecCol(b2, d2, CheckColDec(b2, d2));
  b11 := b1 and $FF00FF; b12 := b1 and $00FF00;
  b21 := b2 and $FF00FF; b22 := b2 and $00FF00;
  f11 := f1 and $FF00FF; f12 := f1 and $00FF00;
  f21 := f2 and $FF00FF; f22 := f2 and $00FF00;
  for i1 := 7 downto 0 do begin
    if i1 <> 7 then Sleep(10);
    DrawButton(((f11 * i1 + f21 * (8 - i1)) shr 3) and $FF00FF + ((f12 * i1 + f22 * (8 - i1)) shr 3) and $00FF00,
               ((b11 * i1 + b21 * (8 - i1)) shr 3) and $FF00FF + ((b12 * i1 + b22 * (8 - i1)) shr 3) and $00FF00,
               0, window, hDC, -1, @r1);
  end;
  FLastFocusTime := GetTickCount;
end;

procedure CheckAutoStuff(const exceptIntf: IMEException; waitForAutoSend: boolean); forward;

procedure TExceptionBox.CloseMainWnd;
begin
  FShuttingDown := true;
  CheckCurrentTab;
  DestroyWindow(FMainWnd);
end;

procedure TExceptionBox.DoDetails;
var b1 : boolean;
begin
  {$ifdef log}log('show bug report');{$endif}
  b1 := false;
  DoFireHandlers(FMainWnd, 1, eaShowBugReport, FExc, b1);
  FReshow := true;
  CloseMainWnd;
end;

// the window proc of our exception box
function TExceptionBox.DialogWndMethod(window, msg: dword; wParam, lParam: integer) : integer; stdcall;

  procedure DoClose;
  var b1 : boolean;
  begin
    {$ifdef log}log('close application');{$endif}
    b1 := false;
    DoFireHandlers(FMainWnd, 1, eaCloseApplication, FExc, b1);
    if not b1 then begin
      CloseMainWnd;
      CheckAutoStuff(FExc, true);
      CloseApplication;
    end;
  end;

  procedure DoRestart;
  var b1 : boolean;
  begin
    {$ifdef log}log('restart application');{$endif}
    b1 := false;
    DoFireHandlers(FMainWnd, 1, eaRestartApplication, FExc, b1);
    if not b1 then begin
      CloseMainWnd;
      CheckAutoStuff(FExc, true);
      RestartApplication;
    end;
  end;

  procedure DoContinue;
  var b1 : boolean;
  begin
    {$ifdef log}log('continue application');{$endif}
    b1 := false;
    DoFireHandlers(FMainWnd, 1, eaContinueApplication, FExc, b1);
    CloseMainWnd;
  end;

  procedure DoSend;
  var b1 : boolean;
      ss : INVBitmap;
  begin
    {$ifdef log}log('send bug report');{$endif}
    b1 := false;
    DoFireHandlers(FMainWnd, 1, eaSendBugReport, FExc, b1);
    if not b1 then begin
      if FExc.AppendScreenShot then
           ss := FExc.ScreenShot
      else ss := nil;
      with FExc as IMEExceptionEx do
        if PSendThread^ <> 0 then
          CloseHandle(PSendThread^);
      SendBugReportEx(FExc.GetBugReport(true), ss, window, false, FExc.SendInBackground, FExc, false, (FExc as IMEExceptionEx).PSendThread);
    end;
    SetCursor(LoadCursor(0, IDC_ARROW));
  end;

  procedure DoSave;
  var b1 : boolean;
  begin
    {$ifdef log}log('save bug report');{$endif}
    b1 := false;
    DoFireHandlers(FMainWnd, 1, eaSaveBugReport, FExc, b1);
    if not b1 then
      if SaveBugReportEx('', FExc.GetBugReport(true), window, false, FExc) then
        if (Length(FExc.BugReportFile) > 2) and
           ((FExc.BugReportFile[1] = '\') or (FExc.BugReportFile[2] = ':')) then
          DestroyWindow(FSave);
    SetCursor(LoadCursor(0, IDC_ARROW));
  end;

  procedure DoPrint;
  var b1 : boolean;
  begin
    {$ifdef log}log('print bug report');{$endif}
    b1 := false;
    DoFireHandlers(FMainWnd, 1, eaPrintBugReport, FExc, b1);
    if not b1 then
      PrintBugReport(FExc.GetBugReport(true), window);
    SetCursor(LoadCursor(0, IDC_ARROW));
  end;

  {$ifdef noncommercial}
    procedure DrawLineEx(dc: dword; x1, x2, y, col1, col2: integer);
    var pen, oldPen : dword;
        r, g, b     : integer;
        i1, i2, i3  : integer;
    begin
      MoveToEx(dc, x1, y, nil);
      r := (col2 and $ff0000) shr 16 - (col1 and $ff0000) shr 16;
      g := (col2 and $00ff00) shr  8 - (col1 and $00ff00) shr  8;
      b := (col2 and $0000ff) shr  0 - (col1 and $0000ff) shr  0;
      i2 := 0;
      i3 := x2 - x1 + 1;
      for i1 := x1 to x2 do begin
        pen := CreatePen(PS_SOLID, 1, col1 + (r * i2) div i3 shl 16 + (g * i2) div i3 shl 8 + (b * i2) div i3);
        inc(i2);
        oldPen := SelectObject(dc, pen);
        LineTo(dc, i1, y);
        SelectObject(dc, oldPen);
        DeleteObject(pen);
      end;
    end;

    function GetClientCursorPos : TPoint;
    begin
      GetCursorPos(result);
      ScreenToClient(window, result);
    end;

    function CheckNcLabel(ppos: PPoint = nil) : boolean;
    var pos : TPoint;
    begin
      if ppos <> nil then
           pos := ppos^
      else pos := GetClientCursorPos;
      result := (pos.X > FNCLabelX) and (pos.X < FNCLabelX + FNCLabelW) and
                (pos.Y > FNCLabelY) and (pos.Y < FNCLabelY + 10       );
    end;
  {$endif}

var dc, wnd : dword;
    r1      : TRect;
    wp      : TWindowPlacement;
    i1, i2  : integer;
    {$ifdef noncommercial}
      ps      : TPaintStruct;
      c1      : dword;
      s1      : string;
      size    : TSize;
      p1      : TPoint;
    {$endif}
begin
  case msg of
    WM_CLOSE             : begin
                             result := 0;
                             case FExc.FocusedButton of
                               bContinueApplication : DoContinue;
                               bRestartApplication  : DoRestart;
                               bCloseApplication    : DoClose;
                               else if     (FContinue <> 0) and FExc.CanContinue then DoContinue
                                    else if FClose    <> 0                       then DoClose
                                    else if FRestart  <> 0                       then DoRestart
                                    else if FDefaultMsgBox                       then DoClose;
                             end;
                           end;
    WM_SYSCOMMAND        : if wParam and $FFF = SC_CLOSE then
                                result := 0
                           else result := DefWindowProc(window, msg, wParam, lParam);
    {$ifdef noncommercial}
      WM_SETCURSOR         : begin
                               if CheckNcLabel then begin
                                 SetCursor(LoadCursor(0,IDC_HAND));
                                 result := 1;
                               end else
                                 result := DefWindowProc(window, msg, wParam, lParam);
                             end;
      WM_PAINT             : begin
                               BeginPaint(window, ps);
                               GetClientRect(window, r1);
                               DrawLineEx(ps.hdc, 0, r1.Right - 6, FNCLabelY + 5, GetSysColor(COLOR_BTNFACE), GetSysColor(COLOR_3DDKSHADOW));
                               c1 := SelectObject(ps.hdc, FFont3);
                               s1 := DecryptStr(#$75#$38#$34#$31#$10#$2D#$36#$30#$25#$21#$75#$66#$7B#$65#$75#$E2#$75#$3B#$3A#$3B#$78#$36#$3A#$38#$38#$30#$27#$36#$3C#$34#$39#$75#$30#$31#$3C#$21#$3C#$3A#$3B#$75);
                               GetTextExtentPoint32(ps.hdc, pchar(s1), Length(s1), size);
                               FNCLabelW := size.cx;
                               FNCLabelX := r1.Right - FNCLabelW - 15;
                               FNCLabelIn := CheckNcLabel;
                               if FNCLabelIn then
                                    SetTextColor(ps.hdc, $900080)
                               else SetTextColor(ps.hdc, GetSysColor(COLOR_3DDKSHADOW));
                               SetBkColor(ps.hdc, GetSysColor(COLOR_BTNFACE));
                               TextOut(ps.hdc, FNCLabelX, FNCLabelY, pchar(s1), Length(s1));
                               SelectObject(ps.hdc, c1);
                               EndPaint(window, ps);
                               result := 0;
                             end;
      WM_MOUSEMOVE         : begin
                               result := DefWindowProc(window, msg, wParam, lParam);
                               if FNCLabelIn <> CheckNcLabel then begin
                                 FNCLabelIn := not FNCLabelIn;
                                 r1.Left := FNCLabelX;
                                 r1.Top  := FNCLabelY;
                                 r1.Right := FNCLabelX + FNCLabelW;
                                 r1.Bottom := FNCLabelY + 10;
                                 InvalidateRect(window, @r1, false);
                               end;
                             end;
      WM_LBUTTONDOWN       : begin
                               result := DefWindowProc(window, msg, wParam, lParam);
                               p1.X := word(lParam);
                               p1.Y := dword(lParam) shr 16;
                               if CheckNcLabel(@p1) then
                                 ShellExecute(0, nil, 'http://madExcept.com', nil, nil, SW_SHOWNORMAL);
                             end;
    {$endif}
    WM_NULL              : begin
                             if (wParam = 777) and (lParam = 777) then
                               CloseMainWnd;
                             result := 0;
                           end;
    WM_NOTIFY            : begin
                             if PNmHdr(lParam)^.code = TCN_SELCHANGE then
                               CheckCurrentTab;
                             result := DefWindowProc(window, msg, wParam, lParam);
                           end;
    WM_USER + 777        : begin
                             if FTabs <> 0 then begin
                               CheckTabs;
                               CheckCurrentTab;
                             end;
                             result := 0;
                           end;
    WM_COMMAND           : begin
                             result := DefWindowProc(window, msg, wParam, lParam);
                             wnd := dword(lParam);
                             if (wnd = 0) and (wParam = 1) then
                               wnd := GetFocus;
                             if (wnd <> 0) and ((wnd = FSend) or (wnd = FSave) or (wnd = FPrint) or (wnd = FDetails) or
                                                (wnd = FContinue) or (wnd = FRestart) or (wnd = FClose)) then begin
                               if FTime <> 0 then begin
                                 KillTimer(FMainWnd, FTimer);
                                 FTime := 0;
                                 InvalidateRect(FRestart, nil, false);
                               end;
                               if (not FDefaultMsgBox) and (not FExc.NoOwnerDrawButtons) then
                                 if (wnd = FDetails) or (TickDif(FLastFocusTime) > 200) then begin
                                   dc := GetWindowDC(wnd);
                                   if wnd <> FDetails then begin
                                     FlashButton($FFFFFF, GetSysColor(COLOR_3DDKSHADOW), $000000,
                                                 $000000, GetSysColor(COLOR_BTNFACE   ), $101010, wnd, dc);
                                     Sleep(10);
                                     FlashButton($000000, GetSysColor(COLOR_BTNFACE   ), $101010,
                                                 $FFFFFF, GetSysColor(COLOR_3DDKSHADOW), $000000, wnd, dc);
                                   end else
                                     FlashButton($FFFFFF,                    GetSysColor(COLOR_3DDKSHADOW), 0,
                                                 GetSysColor(COLOR_BTNFACE), GetSysColor(COLOR_BTNFACE   ), 0, wnd, dc);
                                   ReleaseDC(wnd, dc);
                                 end;
                               if wnd = FDetails then
                                 DoDetails
                               else if (wnd = FClose) or ((wnd = FContinue) and (not FExc.CanContinue)) then
                                 DoClose
                               else if wnd = FRestart then
                                 DoRestart
                               else if wnd = FContinue then
                                 DoContinue
                               else if wnd = FSend then
                                 DoSend
                               else if wnd = FSave then
                                 DoSave
                               else if wnd = FPrint then
                                 DoPrint;
                             end;
                           end;
    WM_TIMER             : begin
                             GetClientRect(FTimerBtn, r1);
                             if FExc.AutoDelay <> 0 then begin
                               if FExc.NoOwnerDrawButtons then
                                    i1 := 5 + (dword(r1.Right - 11) * TickDif(FTime) + FExc.AutoDelay * 500) div (FExc.AutoDelay * 1000)
                               else i1 := 0 + (dword(r1.Right -  0) * TickDif(FTime) + FExc.AutoDelay * 500) div (FExc.AutoDelay * 1000);
                               if i1 <> FOldX then begin
                                 if (      FExc.NoOwnerDrawButtons  and (i1 > r1.Right - 6) ) or
                                    ( (not FExc.NoOwnerDrawButtons) and (i1 > r1.Right - 0) ) then
                                   PostMessage(window, WM_COMMAND, 0, FTimerBtn);
                                 dc := GetWindowDC(FTimerBtn);
                                 if FExc.NoOwnerDrawButtons then
                                      DrawButton($000000, GetSysColor(COLOR_BTNFACE   ), $101010, FTimerBtn, dc, i1, @r1, false, true )
                                 else DrawButton($FFFFFF, GetSysColor(COLOR_3DDKSHADOW), $000000, FTimerBtn, dc, i1, @r1, false, false);
                                 ReleaseDC(FTimerBtn, dc);
                               end;
                             end;
                             result := 0;
                           end;
    WM_DRAWITEM          : with PDrawItemStruct(lParam)^ do begin
                             if hwndItem <> FIcoWnd then begin
                               if (itemAction and ODA_FOCUS <> 0) and (FTime <> 0) then begin
                                 KillTimer(FMainWnd, FTimer);
                                 FTime := 0;
                               end;
                               if FExc.NoOwnerDrawButtons then
                                 DrawButton($000000, GetSysColor(COLOR_BTNFACE), $101010, hwndItem, hDC, -1, nil,
                                            itemState and ODS_SELECTED <> 0, itemState and ODS_FOCUS <> 0)
                               else
                                 if itemState and ODS_FOCUS <> 0 then begin
                                   if itemAction and ODA_FOCUS <> 0 then
                                       FlashButton($000000, GetSysColor(COLOR_BTNFACE   ), $101010,
                                                   $FFFFFF, GetSysColor(COLOR_3DDKSHADOW), $000000, hwndItem, hDC)
                                   else DrawButton($FFFFFF, GetSysColor(COLOR_3DDKSHADOW), $000000, hwndItem, hDC);
                                 end else
                                   if itemAction and ODA_FOCUS <> 0 then
                                       FlashButton($FFFFFF, GetSysColor(COLOR_3DDKSHADOW), $000000,
                                                   $000000, GetSysColor(COLOR_BTNFACE   ), $101010, hwndItem, hDC)
                                   else DrawButton($000000, GetSysColor(COLOR_BTNFACE   ), $101010, hwndItem, hDC);
                             end else
                               DrawGraphic(hDC, 0, FExc.Module, FIcoWnd, GetSysColor(COLOR_BTNFACE), 32, nil);
                             result := 1;
                           end;
    WM_GETMINMAXINFO     : begin
                             PMinMaxInfo(lParam)^.ptMinTrackSize.X := FMinWidth;
                             PMinMaxInfo(lParam)^.ptMinTrackSize.Y := FMinHeight;
                             result := 0;
                           end;
    WM_WINDOWPOSCHANGING : begin
                             result := DefWindowProc(window, msg, wParam, lParam);
                             if FSizingDone and
                                (PWindowPos(lParam)^.flags and SWP_NOSIZE     = 0) and
                                (PWindowPos(lParam)^.flags and SWP_SHOWWINDOW = 0) and
                                (PWindowPos(lParam)^.flags and SWP_HIDEWINDOW = 0) then begin
                               GetWindowRect(window, r1);
                               if (PWindowPos(lParam)^.cy >= FMinHeight) and (r1.Bottom - r1.Top >= FMinHeight) then begin
                                 i1 := PWindowPos(lParam)^.cx - (r1.Right  - r1.Left);
                                 i2 := PWindowPos(lParam)^.cy - (r1.Bottom - r1.Top );
                                 if (i1 <> 0) or (i2 <> 0) then begin
                                   with wp, rcNormalPosition do begin
                                     length := sizeOf(wp);
                                     if GetWindowPlacement(FContinue, @wp) then
                                       SetWindowPos(FContinue, 0, Left + i1, Top, 0, 0, SWP_NOSIZE);
                                     if GetWindowPlacement(FRestart, @wp) then
                                       SetWindowPos(FRestart, 0, Left + i1, Top, 0, 0, SWP_NOSIZE);
                                     if GetWindowPlacement(FClose, @wp) then
                                       SetWindowPos(FClose, 0, Left + i1, Top, 0, 0, SWP_NOSIZE);
                                     if GetWindowPlacement(FTabs, @wp) then
                                       SetWindowPos(FTabs, 0, 0, 0, Right - Left + i1, Bottom - Top + i2, SWP_NOMOVE);
                                     if GetWindowPlacement(FEdit, @wp) then
                                       SetWindowPos(FEdit, 0, 0, 0, Right - Left + i1, Bottom - Top + i2, SWP_NOMOVE);
                                   end;
                                   {$ifdef noncommercial}
                                     GetClientRect(window, r1);
                                     r1.Top := FNCLabelY;
                                     r1.Bottom := FNCLabelY + 10;
                                     InvalidateRect(window, @r1, false);
                                   {$endif}
                                 end;
                               end;
                             end;
                           end;
    else                   result := DefWindowProc(window, msg, wParam, lParam);
  end;
end;

procedure TExceptionBox.BugReportChanged(complete: boolean; bugReport: string; exceptIntf: IMEException);
begin
  PostMessage(FMainWnd, WM_USER + 777, 1357, 2468);
end;

function TExceptionBox.ListViewWndProc(window, msg: dword; wParam, lParam: integer) : integer; stdcall;
var lvi    : TLvItem;
    brush  : dword;
    r1     : TRect;
    i1, i2 : integer;
    ix     : integer;
    c1     : dword;
    s1     : string;
    arrCh  : array [0..MAX_PATH] of char;
    tm     : TTextMetric;
    font   : dword;
begin
  if msg = WM_CTLCOLORSTATIC then begin
    result := GetStockObject(WHITE_BRUSH);
    exit;
  end;
  result := 0;
  if msg <> WM_DRAWITEM then
    result := CallWindowProc(FOldListViewWndProc, window, msg, wParam, lParam)
  else
    with PDrawItemStruct(lParam)^ do
      if itemID < dword(Length(FItems)) then begin
        ZeroMemory(@lvi, sizeOf(lvi));
        lvi.mask       := LVIF_STATE;
        lvi.iItem      := itemID;
        lvi.iSubitem   := 0;
        lvi.stateMask  := LVIS_SELECTED or LVIS_FOCUSED;
        if (FItems[itemID].forceColor = $ffffff) or
           (SendMessage(hwndItem, LVM_GETITEM, 0, integer(@lvi)) <> 0) then begin
          ix := rcItem.Left;
          for i1 := 0 to high(FColumns) do begin
            i2 := SendMessage(hwndItem, LVM_GETCOLUMNWIDTH, i1, 0);
            if FItems[itemID].forceColor <> 0 then
              c1 := FItems[itemID].forceColor
            else
              if odd(i1) or (Length(FColumns) = 1) then
                c1 := $ffffff
              else
                if i1 mod 4 = 0 then
                  c1 := $fff8f0
                else
                  c1 := $f4f4ff;
            if lvi.state and LVIS_SELECTED <> 0 then
              brush := CreateSolidBrush(c1 - $101010)
            else
              brush := CreateSolidBrush(c1);
            r1 := rcItem;
            r1.Left := ix;
            r1.Right := ix + i2;
            FillRect(hDC, r1, brush);
            DeleteObject(brush);
            ix := ix + i2;
          end;
          if lvi.state and LVIS_SELECTED(*FOCUSED*) <> 0 then begin
            brush := CreateSolidBrush($b0b0b0);
            FrameRect(hDC, rcItem, brush);
            DeleteObject(brush);
          end;
          if (lvi.state and LVIS_SELECTED = 0) and (*(lvi.state and LVIS_FOCUSED = 0) and*)
             ( (itemID = dword(high(FItems))) or (not FItems[itemID + 1].title) ) then begin
            brush := CreateSolidBrush($e0e0e0);
            r1 := rcItem;
            r1.Top := r1.Bottom - 1;
            FrameRect(hDC, r1, brush);
            DeleteObject(brush);
          end;
          ix := rcItem.Left;
          for i1 := 0 to high(FColumns) do begin
            if i1 > 0 then
              i2 := ix
            else
              i2 := ix + 2;
            ix := ix + SendMessage(hwndItem, LVM_GETCOLUMNWIDTH, i1, 0);
            r1 := rcItem;
            r1.Left  := i2 + 4;
            r1.Right := ix - 4;
            r1.Top   := r1.Top + 2;
            ZeroMemory(@arrCh, sizeOf(arrCh));
            lvi.mask       := LVIF_TEXT;
            lvi.iItem      := itemID;
            lvi.iSubitem   := i1;
            lvi.pszText    := arrCh;
            lvi.cchTextMax := MAX_PATH;
            if SendMessage(hwndItem, LVM_GETITEMTEXT, itemID, integer(@lvi)) <> 0 then begin
              s1 := arrCh;
              if s1 = '>>' then begin
                s1 := RetDelete(FItems[itemID].captions, 1, 3);
                font := 0;
                if FItems[itemID].title then begin
                  if lvi.state and LVIS_SELECTED(*FOCUSED*) = 0 then
                    brush := CreateSolidBrush($c0c0c0)
                  else
                    brush := CreateSolidBrush($a0a0a0);
                  r1 := rcItem;
                  r1.Top := r1.Bottom - 2;
                  FrameRect(hDC, r1, brush);
                  DeleteObject(brush);
                  if (GetTextFace(hDC, MAX_PATH, arrCh) > 0) and GetTextMetrics(hDC, tm) then
                    font := CreateFont(tm.tmHeight, 0, 0, 0, 700, 0, 0, 0, DEFAULT_CHARSET,
                                       OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                                       DEFAULT_PITCH or FF_DONTCARE, arrCh);
                end else
                  SetTextColor(hDC, $606060);
                r1 := rcItem;
                r1.Left  := r1.Left + 6;
                r1.Top   := r1.Top  + 2;
                r1.Right := r1.Right - 4;
                if font <> 0 then begin
                  c1 := SelectObject(hDC, font);
                  DrawText(hDC, pchar(s1), Length(s1), r1, DT_END_ELLIPSIS or DT_SINGLELINE);
                  SelectObject(hDC, c1);
                  DeleteObject(font);
                end else
                  DrawText(hDC, pchar(s1), Length(s1), r1, DT_END_ELLIPSIS or DT_SINGLELINE);
                SetTextColor(hDC, $000000);
              end else begin
                if FColumns[i1].format and LVCFMT_RIGHT <> 0 then
                  c1 := DT_RIGHT
                else
                  if FColumns[i1].format and LVCFMT_CENTER <> 0 then
                    c1 := DT_CENTER
                  else
                    c1 := 0;
                if (Length(s1) > 2) and (s1[1] in ['a'..'z', 'A'..'Z']) and (s1[2] = ':') and (s1[3] = '\') then
                     c1 := c1 or DT_PATH_ELLIPSIS
                else c1 := c1 or DT_END_ELLIPSIS;
                DrawText(hDC, pchar(s1), Length(s1), r1, c1 or DT_SINGLELINE);
              end;
            end;
          end;
        end;
      end;
  if (FCurrentTab = 1) and (msg = WM_NOTIFY) and (PNmHdr(lParam)^.code = NM_DBLCLK) then begin
    i1 := SendMessage(FEdit, LVM_GETNEXTITEM, -1, LVNI_SELECTED);
    if (i1 > 0) and (i1 < Length(FItems)) then begin
      s1 := FItems[i1].captions;
      if (s1 <> '') and (s1 <> '>') then begin
        ReplaceStr(s1, #9, ' ');
        SetWindowPos(FMainWnd, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
        HyperJumpCallstack(s1);
      end;
    end;
  end;
end;

procedure TExceptionBox.CheckCurrentTab;

  type
    TDALine = array of record
      start : integer;
      len   : integer;
    end;

  function FindLines(str: string) : TDALine;
  var i1, i2 : integer;
  begin
    SetLength(result, SubStrCount(str, #$D));
    if result <> nil then begin
      i2 := 0;
      result[i2].start := 1;
      for i1 := 1 to Length(str) do
        if str[i1] = #$D then begin
          if (str[result[i2].start] = '>') and (str[result[i2].start + 1] = '>') then
               // lines beginning with ">>" get a special treatment
               result[i2].len := -1
          else result[i2].len := i1 - result[i2].start;
          inc(i2);
          result[i2].start := i1 + 2;
        end;
      if (str[result[i2].start] = '>') and (str[result[i2].start + 1] = '>') then
           result[i2].len := -1
      else result[i2].len := Length(str) + 1 - result[i2].start;
      SetLength(result, i2 + 1);
    end;
  end;

  function FindColumns(var str: string; lines: TDALine = nil; ignoreSingleLines: boolean = true) : TDAInteger;
  var i1, i2, i3 : integer;
      lineCnt    : integer;  // number of lines (excluding ">> bla" lines)
      index      : integer;  // index of longest line
      b1         : boolean;  // is there a seperator in column "i1" in all lines?
      pc1        : pchar;
  begin
    result := nil;
    if lines = nil then
      lines := FindLines(str);
    if lines <> nil then begin
      index := 0;
      lineCnt := ord(lines[0].len > 0);
      for i1 := 1 to high(lines) do begin
        if lines[i1].len > 0 then
          inc(lineCnt);
        if lines[i1].len > lines[index].len then
          index := i1;
      end;
      for i1 := 1 to lines[index].len - 2 do begin
        // loop through all columns
        pc1 := @str[lines[index].start + i1];
        b1 := (pc1[0] in [' ', '=', '|']) or ( (pc1[0] = ':') and (pc1[1] = ' ') );
        i3 := 0;
        if b1 then
          for i2 := 0 to high(lines) do
            if (i2 <> index) and (i1 < lines[i2].len) then
              if str[lines[i2].start + i1] <> pc1[0] then begin
                // "i1" evidentyl is no seperation column
                b1 := false;
                break;
              end else
                if (pc1[0] <> ' ') or ( (i1 + 1 < lines[i2].len) and (str[lines[i2].start + i1 + 1] <> ' ') ) then
                  // found a line with a valid column seperator
                  inc(i3);
        if b1 and ( (not ignoreSingleLines) or (lineCnt = 1) or (i3 > 1) ) then begin
          for i2 := 0 to high(lines) do
            if i1 < lines[i2].len then
              str[lines[i2].start + i1] := ' ';
          if (result = nil) or (result[high(result)] <> i1) then
            SetLength(result, Length(result) + 1);
          result[high(result)] := i1 + 1;
        end;
      end;
    end;
  end;

  procedure PrepareListView_Columns(str: string);
  var i1 : integer;
  begin
    SetLength(FColumns, SubStrCount(str));
    for i1 := 0 to high(FColumns) do
      with FColumns[i1] do begin
        caption := SubStr(str, i1 + 1);
        if (caption <> '') and (caption[1] = '^') then begin
          upperCase := true;
          Delete(caption, 1, 1);
        end;
        if (caption <> '') and (caption[1] = ' ') then begin
          format := LVCFMT_RIGHT;
          Delete(caption, 1, 1);
        end;
      end;
  end;

  procedure PrepareListView_Items(str: string; const ai: array of integer);

    procedure TrimStr9(var str: string);
    var c1, c2 : cardinal;
    begin
      c1 := PosChars([#9, #33..#255], str);
      if c1 <> 0 then begin
        c2 := PosChars([#9, #33..#255], str, maxInt, 1);
        Keep(str, c1, c2 - c1 + 1);
      end else str := '';
    end;

  var i1, i2 : integer;
      s1     : string;
  begin
    SetLength(FItems, SubStrCount(str, #$D));
    for i1 := 0 to high(FItems) do begin
      s1 := SubStr(str, i1 + 1, #$D);
      TrimStr9(s1);
      for i2 := 0 to high(ai) do
        if (i2 < high(FColumns)) and (Length(s1) > ai[i2]) then
          s1[ai[i2]] := #9;
      FItems[i1].captions := s1;
    end;
  end;

  procedure PrepareListView_Header(s1: string; var mono, showColumnHeaders: boolean; var seperator: char);
  begin
    PrepareListView_Columns('description|value');
    PrepareListView_Items(s1, [maxInt]);
  end;

  procedure PrepareListView_CpuRegs(s1: string; var mono, showColumnHeaders: boolean; var seperator: char);
  begin
    mono := true;
    PrepareListView_Columns('reg.|content');
    PrepareListView_Items(s1, FindColumns(s1));
  end;

  procedure PrepareListView_StackDump(s1: string; var mono, showColumnHeaders: boolean; var seperator: char);
  begin
    mono := true;
    PrepareListView_Columns('address|data - hexadecimal|data - chars');
    PrepareListView_Items(s1, [10, 61]);
  end;

  procedure PrepareListView_Modules(s1: string; var mono, showColumnHeaders: boolean; var seperator: char);
  begin
    PrepareListView_Columns('^handle|file name| version|path');
    PrepareListView_Items(s1, FindColumns(s1));
  end;

  procedure PrepareListView_Processes(s1: string; var mono, showColumnHeaders: boolean; var seperator: char);
  begin
    PrepareListView_Columns('^id|file name|priority|path');
    PrepareListView_Items(s1, FindColumns(s1));
  end;

  procedure PrepareListView_DisAsm(s1: string; var mono, showColumnHeaders: boolean; var seperator: char);
  var lines  : TDALine;
      ai     : TDAInteger;
      index  : integer;
      i1, i2 : integer;
      s2     : string;
  begin
    mono  := true;
    lines := FindLines(s1);
    ai    := nil;
    index := 0;
    s2    := '';
    for i1 := 0 to high(lines) do
      if lines[i1].len > 0 then begin
        if lines[i1].len > 25 then
             i2 := 25
        else i2 := lines[i1].len - 1;
        index := PosStr('>', s1, lines[i1].start, lines[i1].start + i2);
        if index > 0 then begin
          index := index + 1 - lines[i1].start;
          if index > 10 then begin
            SetLength(ai, 2);
            ai[1] := index - 1;
            s2 := 'line|';
          end else
            SetLength(ai, 1);
          ai[0] := 9;
          break;
        end;
      end;
    PrepareListView_Columns('address|' + s2 + 'asm code|comments');
    ReplaceStr(s1, '; --', '# --');
    ReplaceStr(s1, ';', #9 + ';');
    ReplaceStr(s1, '# --', '; --');
    SetLength(FItems, SubStrCount(s1, #$D));
    for i1 := 0 to high(FItems) do begin
      s2 := SubStr(s1, i1 + 1, #$D);
      TrimStr(s2);
      for i2 := 0 to high(ai) do
        if Length(s2) > ai[i2] then
          s2[ai[i2]] := #9;
      if (index > 0) and (length(s2) > index) and (s2[index] in [' ', '>']) then begin
        if s2[index] = '>' then
          FItems[i1].forceColor := $d0d0ff;
        s2[index] := ' ';
      end;
      if PosStr('; --', s2) > 0 then
        Delete(s2, PosStr('; --', s2) + 2, 28);
      FItems[i1].captions := s2;
    end;
  end;

  procedure PrepareListView_Thread(s1: string; var mono, showColumnHeaders: boolean; var seperator: char);

    type
      TPiece = record
        str        : string;
        cols       : TDAInteger;
        moduleCol  : integer;
        relAddrCol : integer;
        unitCol    : integer;
        lineCol    : integer;
        relLineCol : integer;
        funcCol    : integer;
      end;
      TDAPiece = array of TPiece;

    function CutInPieces(var pieces: TDAPiece) : boolean;
    // step 1: divide the whole string into good pieces
    // because if multiple callstacks are contained, they may differ in format
    var lines  : TDALine;
        b1     : boolean;
        i1, i2 : integer;
    begin
      lines := FindLines(s1);
      result := lines <> nil;
      if result then begin
        pieces := nil;
        i2 := 0;
        b1 := lines[high(lines)].len > 0;
        for i1 := high(lines) - 1 downto 0 do
          if (lines[i1].len > 0) <> b1 then begin
            if (not b1) and (Copy(s1, lines[i1 + 1].start, 10) <> '>> created') then begin
              SetLength(pieces, i2 + 1);
              pieces[i2].str := Copy(s1, lines[i1 + 1].start, maxInt);
              inc(i2);
              Delete(s1, lines[i1 + 1].start - 2, maxInt);
            end;
            b1 := not b1;
          end;
        SetLength(pieces, i2 + 1);
        pieces[i2].str := s1;
      end;
    end;

    procedure AnalyzePiece(var pi: TPiece);
    // step 2: try to find out how the columns of this piece are structured
    var lines      : TDALine;
        b1, b2     : boolean;
        i1, i2, i4 : integer;
    begin
      with pi do begin
        lines := FindLines(pi.str);
        cols := FindColumns(str, lines, false);
        moduleCol  := 1;
        relAddrCol := -1;
        unitCol    := -1;
        lineCol    := -1;
        relLineCol := -1;
        funcCol    := -1;
        b1 := Length(cols) > 1;
        if b1 then begin
          // try to find relative address column
          for i1 := 0 to high(lines) do
            if lines[i1].len > cols[1] - 2 then begin
              b1 := str[lines[i1].start + cols[0]] = '+';
              if b1 then
                for i2 := lines[i1].start + cols[0] + 1 to lines[i1].start + cols[1] - 2 do
                  if not (str[i2] in ['0'..'9', 'a'..'f', 'A'..'F']) then begin
                    b1 := false;
                    break;
                  end;
              if not b1 then
                break;
            end;
          if b1 then begin
            // the relative address column can only be the 2nd one
            relAddrCol := 1;
            inc(moduleCol);
          end;
        end;
        if moduleCol > length(cols) then
          moduleCol := -1;
        if Length(cols) >= 3 + ord(relAddrCol <> -1) then
          // is there a line number column?
          for i4 := 2 + ord(ord(relAddrCol <> -1)) to Length(cols) - 1 do begin
            b1 := true;
            for i1 := 0 to high(lines) do
              if (lines[i1].len > cols[i4]) and
                 ( (i4 = high(cols)) or (lines[i1].len > cols[i4 + 1] - 2) ) then begin
                b2 := false;
                if i4 = high(cols) then
                     i2 := lines[i1].len
                else i2 := cols[i4 + 1];
                for i2 := lines[i1].start + cols[i4] to lines[i1].start + i2 - 2 do
                  if str[i2] in ['0'..'9'] then
                    b2 := true
                  else
                    if (str[i2] <> ' ') or b2 then begin
                      b1 := false;
                      break;
                    end;
                if not b1 then
                  break;
              end;
            if b1 then begin
              lineCol := i4 + 1;
              unitCol := i4;
              if Length(cols) - lineCol >= 2 then begin
                relLineCol := i4 + 2;
                funcCol := i4 + 3;
              end else
                if Length(cols) - lineCol = 1 then
                  funcCol := i4 + 2;
              break;
            end;
          end;
        if (lineCol = -1) and (Length(cols) > 1 + ord(relAddrCol <> -1)) then
          // no line column is available
          // so we search the 1st column which contains valid identifiers only
          // this will become our "unit" column then
          for i4 := 1 + ord(relAddrCol <> -1) to Length(cols) - 1 do begin
            b1 := true;
            for i1 := 0 to high(lines) do
              if (lines[i1].len > cols[i4]) and
                 ( (i4 = high(cols)) or (lines[i1].len > cols[i4 + 1] - 2) ) then begin
                if i4 = high(cols) then
                     i2 := lines[i1].len
                else i2 := cols[i4 + 1];
                s1 := Copy(str, lines[i1].start + cols[i4], i2 - cols[i4]);
                TrimStr(s1);
                if (s1 <> '') and (not (IsValidIdent(s1) or PosTextIs1('public%', s1) or PosTextIs1('segment%', s1))) then begin
                  b1 := false;
                  break;
                end;
              end;
            if b1 then begin
              if Length(cols) - i4 = 2 then begin
                unitCol := i4 + 1;
                funcCol := i4 + 2;
              end else
                funcCol := i4 + 1;
              break;
            end;
          end;
      end;
    end;

    function ColumnExists(var pieces: TDAPiece; offset: pointer; var result2: boolean) : boolean;
    // returns whether a specific column integer is set
    var i1 : integer;
        c1 : dword;
    begin
      result := false;
      result2 := false;
      c1 := dword(offset) - dword(@pieces[0]);
      for i1 := 0 to high(pieces) do
        if integer(pointer(dword(@pieces[i1]) + c1)^) <> -1 then begin
          result := true;
          result2 := true;
          break;
        end;
    end;

    procedure AddColumnStr(doAdd: boolean; var s1: string; s2: string; const cols: TDAInteger; col1, col2: integer);
    // extract the specified column from the line string "s2" and add it to "s1"
    var i1 : integer;
    begin
      if doAdd then
        if col1 <> -1 then begin
          if col1 < length(cols) then
               i1 := cols[col1] - cols[col2]
          else i1 := maxInt;
          s1 := s1 + #9 + Copy(s2, cols[col2], i1);
        end else
          s1 := s1 + #9;
    end;

  var i1, i2, i3, i4 : integer;
      s2             : string;
      pieces         : TDAPiece;
      moduleColumn   : boolean;
      relAddrColumn  : boolean;
      unitColumn     : boolean;
      lineColumn     : boolean;
      relLineColumn  : boolean;
      funcColumn     : boolean;
      b1             : boolean;
  begin
    if CutInPieces(pieces) then begin
      for i3 := high(pieces) downto 0 do
        AnalyzePiece(pieces[i3]);
      // step 3: set up the overall column structure
      s1 := '^address';
      if ColumnExists(pieces, @pieces[0].relAddrCol, relAddrColumn) then s1 := s1 + '|^ rel.';
      if ColumnExists(pieces, @pieces[0]. moduleCol,  moduleColumn) then s1 := s1 + '|module';
      if ColumnExists(pieces, @pieces[0].   unitCol,    unitColumn) then s1 := s1 + '|unit';
      if ColumnExists(pieces, @pieces[0].   lineCol,    lineColumn) then s1 := s1 + '| line';
      if ColumnExists(pieces, @pieces[0].relLineCol, relLineColumn) then s1 := s1 + '| rel.';
      if ColumnExists(pieces, @pieces[0].   funcCol,    funcColumn) then s1 := s1 + '|function';
      PrepareListView_Columns(s1);
      // step 4: build up the lines one by one
      b1 := not PosStrIs1('>> internal error', pieces[high(pieces)].str);
      for i3 := high(pieces) downto 0 do
        with pieces[i3] do begin
          i2 := Length(FItems);
          SetLength(FItems, i2 + SubStrCount(str, #$D));
          for i1 := i2 to high(FItems) do begin
            s2 := SubStr(str, i1 + 1 - i2, #$D);
            TrimStr(s2);
            if (s2 = '') or ((Length(s2) > 1) and (s2[1] = '>') and (s2[2] = '>')) then begin
              // a line beginning with ">>" gets special treatment
              FItems[i1].forceColor := $ffffff;
              FItems[i1].title := b1;
              b1 := s2 = '';
            end else begin
              b1 := false;
              if cols <> nil then begin
                // we have a typical callstack line here
                // we build up the final line for our list view
                // by extracting all the columns which are available
                // and by leaving the other columns empty 
                s1 := s2;
                Delete(s2, cols[0], maxInt);
                if unitCol <> -1 then
                     i4 := unitCol - 1
                else i4 := moduleCol;
                AddColumnStr(relAddrColumn, s2, s1, cols, relAddrCol, relAddrCol - 1);
                AddColumnStr( moduleColumn, s2, s1, cols,         i4,  moduleCol - 1);
                AddColumnStr(   unitColumn, s2, s1, cols,    unitCol,    unitCol - 1);
                AddColumnStr(   lineColumn, s2, s1, cols,    lineCol,    lineCol - 1);
                AddColumnStr(relLineColumn, s2, s1, cols, relLineCol, relLineCol - 1);
                AddColumnStr(   funcColumn, s2, s1, cols,    funcCol,    funcCol - 1);
              end;
            end;
            FItems[i1].captions := s2;
          end;
        end;
    end;
  end;

  procedure PrepareListView_AutoFormat(s1: string; var mono, showColumnHeaders: boolean; var seperator: char);
  var i1, i2, i3, i4 : integer;
      lines          : TDALine;
      ai             : TDAInteger;
  begin
    ai := nil;
    lines := FindLines(s1);
    i2 := 0;  // number of lines with >  80 chars
    i3 := 0;  // number of lines with <= 80 chars
    mono := true;
    for i1 := 0 to high(lines) do begin
      if lines[i1].len > 80 then
        inc(i2)
      else
        inc(i3);
      if mono then
        for i4 := lines[i1].start to lines[i1].start + lines[i1].len - 1 do
          if s1[i4] <> ' ' then begin
            // mono is set to true, if all lines begin with "+" or "-"
            mono := s1[i4] in ['+', '-'];
            break;
          end;
    end;
    if (i2 + i3 > 1) and (i2 * 5 < i3) then begin
      // we have lots more lines with <= 80 chars than lines with > 80 chars
      // so we use a ListView component to show this information
      showColumnHeaders := false;
      if mono then begin
        ai := nil;
        SetLength(FColumns, 1);
        for i1 := 0 to high(lines) do
          if s1[lines[i1].start] = ' ' then
            s1[lines[i1].start] := ' ';
      end else begin
        if PosStr(#9, s1) > 0 then begin
          ai := nil;
          i4 := 1;
          for i1 := 0 to high(lines) do begin
            i3 := 1;
            for i2 := lines[i1].start to lines[i1].start + lines[i1].len - 1 do
              if s1[i2] = #9 then
                inc(i3);
            if i3 > i4 then
              i4 := i3;
          end;
          SetLength(FColumns, i4);
        end else begin
          ai := FindColumns(s1, lines);
          SetLength(FColumns, Length(ai) + 1);
        end;
      end;
      PrepareListView_Items(s1, ai);
      if mono then
        for i1 := 0 to high(FItems) do
          if (FItems[i1].captions <> '') and (FItems[i1].captions[1] = '+') then
            FItems[i1].forceColor := $fff8f0;
    end;
  end;

  function GetTabClientRect : TRect;
  begin
    GetClientRect(FTabs, result);
    SendMessage(FTabs, TCM_ADJUSTRECT, 0, integer(@result));
    dec(result.Left);
    inc(result.Top, 2);
    InflateRect(result, -3, -3);
    result.Right  := result.Right  - result.Left;
    result.Bottom := result.Bottom - result.Top;
  end;

  function CreateListView(mono, showColumnHeaders: boolean; seperator: char) : dword;
  var lvc        : TLvColumn;
      lvi        : TLvItem;
      i1, i2, i3 : integer;
      s1, s2     : string;
      b1         : boolean;
      c1         : dword;
      r1         : TRect;
  begin
    InitCommonControls;
    r1 := GetTabClientRect;
    if showColumnHeaders then
         c1 := 0
    else c1 := LVS_NOCOLUMNHEADER;
    result := CreateWindow(WC_LISTVIEW, nil,
                           c1 or WS_CHILD or WS_VSCROLL or WS_HSCROLL or WS_BORDER or
                           LVS_REPORT or LVS_NOSORTHEADER or LVS_SINGLESEL or LVS_OWNERDRAWFIXED,
                           r1.Left, r1.Top, r1.Right, r1.Bottom, FTabs, 0, HInstance, nil);
    if mono then
         c1 := FFont2
    else c1 := FFont1;
    SendMessage(result, WM_SETFONT, integer(c1), 0);
    SendMessage(result, LVM_SETIMAGELIST, LVSIL_SMALL, FImageList);
    ZeroMemory(@lvc, sizeOf(lvc));
    for i1 := 0 to high(FColumns) do begin
      lvc.mask    := LVCF_FMT or LVCF_TEXT or LVCF_WIDTH;
      lvc.fmt     := FColumns[i1].format;
      lvc.pszText := pchar(FColumns[i1].caption);
      lvc.cx      := -1;
      SendMessage(result, LVM_INSERTCOLUMN, i1, integer(@lvc));
    end;
    for i1 := 0 to high(FItems) do begin
      lvi.mask  := LVIF_TEXT;
      lvi.iItem := i1;
      s1 := FItems[i1].captions;
      for i2 := 0 to high(FColumns) do begin
        if (Length(s1) > 1) and (s1[1] = '>') and (s1[2] = '>') then
          Delete(s1, 3, maxInt);
        b1 := true;
        if i2 < high(FColumns) then
          for i3 := 1 to Length(s1) do
            if s1[i3] = seperator then begin
              s2 := Copy(s1, 1, i3 - 1);
              b1 := false;
              break;
            end;
        if b1 then
          s2 := s1;
        Delete(s1, 1, Length(s2) + 1);
        TrimStr(s2);
        if FColumns[i2].upperCase then
          s2 := UpStr(s2);
        lvi.iSubitem := i2;
        lvi.pszText := pchar(s2);
        if i2 = 0 then
             SendMessage(result, LVM_INSERTITEM, 0, integer(@lvi))
        else SendMessage(result, LVM_SETITEM,    0, integer(@lvi));
      end;
    end;
    for i1 := 0 to high(FColumns) do
      SendMessage(result, LVM_SETCOLUMNWIDTH, i1, -1);
    ShowWindow(result, SW_SHOW);
  end;

var tab    : integer;
    b1, b2 : boolean;
    i1, i2 : integer;
    sep    : char;
    s1, s2 : string;
    r1     : TRect;
begin
  if FShuttingDown then
       tab := -1
  else tab := SendMessage(FTabs, TCM_GETCURSEL, 0, 0);
  b1 := false;
  b2 := true;
  sep := #9;
  if tab >= 0 then
    if tab > 0 then begin
      if FExc.BugReportSections.ItemCount > 1 then begin
        for i2 := 1 to FExc.BugReportSections.ItemCount - 1 do
          if PosStr('$', FExc.BugReportSections.Items[i2]) = 0 then
            break;
      end else
        i2 := 1;
      if tab = 1 then begin
        s2 := '';
        for i1 := 0 to i2 - 1 do
          s2 := s2 + #$D#$A + #$D#$A +
                '>> ' + FExc.BugReportSections.Items[i1] + #$D#$A +
                        FExc.BugReportSections[FExc.BugReportSections.Items[i1]];
        Delete(s2, 1, 4);
      end else begin
        i1 := tab + i2 - 2;
        s1 := FExc.BugReportSections.Items[i1];
        s2 := FExc.BugReportSections[s1];
      end;
    end else begin
      s2 := '';
      for i1 := 0 to FExc.BugReportHeader.ItemCount - 1 do begin
        s1 := FExc.BugReportHeader.Items[i1];
        s2 := s2 + #$D#$A + s1 + #9 + FExc.BugReportHeader.Contents[s1];
      end;
      Delete(s2, 1, 2);
    end;
  if (tab <> FCurrentTab) or (s2 <> FCurrentTabContent) then begin
    if FEdit <> 0 then begin
      SendMessage(FEdit, LVM_SETIMAGELIST, LVSIL_SMALL, 0);
      DestroyWindow(FEdit);
      FEdit := 0;
    end;
    FColumns := nil;
    FItems := nil;
    if tab >= 0 then begin
      if      PosTextIs1('>> will be',  s2) then PrepareListView_AutoFormat(s2, b1, b2, sep)
      else if PosTextIs1('>> internal', s2) then PrepareListView_Thread    (s2, b1, b2, sep)
      else if tab = 0                       then PrepareListView_Header    (s2, b1, b2, sep)
      else if tab = 1                       then PrepareListView_Thread    (s2, b1, b2, sep)
      else if s1 = 'modules'                then PrepareListView_Modules   (s2, b1, b2, sep)
      else if s1 = 'cpu registers'          then PrepareListView_CpuRegs   (s2, b1, b2, sep)
      else if s1 = 'stack dump'             then PrepareListView_StackDump (s2, b1, b2, sep)
      else if s1 = 'processes'              then PrepareListView_Processes (s2, b1, b2, sep)
      else if s1 = 'disassembling'          then PrepareListView_DisAsm    (s2, b1, b2, sep)
      else                                       PrepareListView_AutoFormat(s2, b1, b2, sep);
      if (FColumns = nil) or (FItems = nil) then begin
        r1 := GetTabClientRect;
        FEdit := CreateWindow('Edit', pchar(s2),
                              WS_VISIBLE or WS_CHILD or WS_VSCROLL or WS_BORDER or
                              ES_READONLY or ES_MULTILINE or ES_WANTRETURN,
                              r1.Left, r1.Top, r1.Right, r1.Bottom, FTabs, 0, HInstance, nil);
        SendMessage(FEdit, WM_SETFONT, integer(FFont1), 0);
      end else
        FEdit := CreateListView(b1, b2, sep);
    end;
    FCurrentTab := tab;
    FCurrentTabContent := s2;
  end;
end;

procedure TExceptionBox.CheckTabs;
var tci   : TTcItem;
    i1    : integer;
    b1    : boolean;
    s1    : string;
    tabs1 : string;
    tabs2 : string;
    arrCh : array [0..MAX_PATH] of char;
begin
  tabs1 := 'general|call stack';
  if (FExc.BugReportSections.ItemCount > 1) and
     (PosStr('$', FExc.BugReportSections.Items[1]) > 0) then
    tabs1 := tabs1 + 's';
  b1 := true;
  for i1 := 1 to FExc.BugReportSections.ItemCount - 1 do begin
    s1 := FExc.BugReportSections.Items[i1];
    if (not b1) or (PosStr('$', s1) = 0) then begin
      b1 := false;
      if      s1 = 'cpu registers' then s1 := 'cpu regs'
      else if s1 = 'disassembling' then s1 := 'disasm';
      tabs1 := tabs1 + '|' + s1;
    end;
  end;
  b1 := false;
  ZeroMemory(@tci, sizeOf(tci));
  tci.mask := TCIF_TEXT;
  tci.pszText := arrCh;
  tabs2 := '';
  for i1 := SendMessage(FTabs, TCM_GETITEMCOUNT, 0, 0) - 1 downto 0 do begin
    tci.cchTextMax := MAX_PATH;
    if (SendMessage(FTabs, TCM_GETITEM, i1, integer(@tci)) = 0) or
       (not SubStrExists(tabs1, arrCh)) then begin
      SendMessage(FTabs, TCM_DELETEITEM, i1, 0);
      b1 := true;
    end else
      tabs2 := tabs2 + '|' + arrCh;
  end;
  for i1 := 1 to SubStrCount(tabs1) do begin
    s1 := SubStr(tabs1, i1);
    if not SubStrExists(tabs2, s1) then begin
      tci.pszText := pchar(s1);
      SendMessage(FTabs, TCM_INSERTITEM, i1 - 1, integer(@tci));
      b1 := true;
    end;
  end;
  if b1 then
    InvalidateRect(FTabs, nil, false);
end;

constructor TExceptionBox.Create(exc: IMEException; defaultMsgBox, detailsBtn, makeSound: boolean);
const CRatio = 14;
var doFocus : boolean;
    view2   : boolean;
    lastW   : integer;

  function CreateWindow_(class_, text: string; style: dword; ix, iy, iw, ih, parent: dword) : dword;
  begin
    result := CreateWindow(pchar(class_), pchar(text), WS_VISIBLE or WS_CHILD or style,
                           ix, iy, iw, ih, parent, 0, HInstance, nil);
    SendMessage(result, WM_SETFONT, integer(FFont1), 0);
  end;

  function CreateButton(caption: string; vis: boolean; image: integer; var ix: integer; iy, ii, iw: integer) : dword;
  var style : dword;
  begin
    if vis then begin
      if image >= 4 then
        if view2 then
             iw := lastW
        else ii := 1;
      style := WS_TABSTOP;
      if not FDefaultMsgBox then
        style := style or BS_OWNERDRAW;
      if ((image = ord(FExc.FocusedButton)) or ((image = 4) and FDefaultMsgBox)) and doFocus then
        style := style or BS_DEFPUSHBUTTON;
      result := CreateWindow_('Button', pchar(caption), style, ix, iy + (FBtnHeight + 4) * ii, iw - 4, FBtnHeight, FMainWnd);
      if (not view2) or (image < 4) then
        inc(ix, iw);
      SetWindowLong(result, GWL_USERDATA, image);
      if ((image = ord(FExc.FocusedButton)) or ((image = 4) and FDefaultMsgBox)) and doFocus then begin
        SetFocus(result);
        doFocus := false;
      end;
    end else
      result := 0;
  end;

var wndClass : TWndClass;
    i1       : integer;
    iw, ih   : integer;
    ix, iy   : integer;
    bw       : integer;  // buttonwidth
    fw       : integer;  // fullwidth
    errorTxt : string;
    s1       : string;
    c1       : dword;
    rect     : TRect;
    bi       : TBitmapInfo;
    sendW, saveW, printW, detailsW, continueW, restartW, closeW : integer;
    sendV, saveV, printV, detailsV, continueV, restartV, closeV : boolean;
                          detailsC, continueC                   : string;
begin
  {$ifdef log}log('TExceptionBox.Create'); indentLog;{$endif}
  inherited Create;
  FExc := exc;
  FDefaultMsgBox := defaultMsgBox;
  FCurrentTab := -1;
  FImageList := ImageList_Create(16, 16, ILC_COLOR32 or ILC_MASK, 1, 1);
  if (not defaultMsgBox) and FExc.AutoShowBugReport then FExc.ShowBtnVisible := false;
  if      CheckAutoBtn(FExc.AutoClose  ) and FExc.  CloseBtnVisible             then FExc.FocusedButton :=    bCloseApplication
  else if CheckAutoBtn(FExc.AutoRestart) and FExc.RestartBtnVisible             then FExc.FocusedButton :=  bRestartApplication
  else if FExc.AutoContinue and FExc.ContinueBtnVisible and FExc.CanContinue then FExc.FocusedButton := bContinueApplication;
  s1 := FExc.ExceptMessage;
  for i1 := length(s1) - 10 downto 20 do
    if (s1[i1] = '.') and (s1[i1 + 1] = ' ') then begin
      s1[i1 + 1] := #$D;
      Insert(#$A, s1, i1 + 2);
    end;
  if defaultMsgBox then begin
    if FExc.ExceptType = etFrozen then begin
      errorTxt := FExc.FrozenMsg;
      ExpandVars(FExc.Module, errorTxt, s1, '');
    end else
      errorTxt := s1;
    s1 := KillExt(ExtractFileName(ModuleName(0)));
  end else begin
    if FExc.ExceptType = etFrozen then
         errorTxt := FExc.FrozenMsg
    else errorTxt := FExc.ExceptMsg;
    ExpandVars(FExc.Module, errorTxt, s1, '');
    s1 := FExc.TitleBar;
  end;
  ZeroMemory(@wndClass, sizeOf(TWndClass));
  with wndClass do begin
    style         := CS_CLASSDC or CS_PARENTDC;
    lpfnWndProc   := @DefWindowProc;
    hInstance     := SysInit.HInstance;
    hbrBackground := COLOR_BTNFACE + 1;
    lpszClassname := 'madExceptWndClass';
    hCursor       := LoadCursor(0, IDC_ARROW);
  end;
  RegisterClass(wndClass);
  if (not defaultMsgBox) and FExc.AutoShowBugReport then
       c1 := WS_SIZEBOX or WS_MAXIMIZEBOX or WS_MINIMIZEBOX
  else c1 := 0;
  // in NT4 you sometimes have to give exactly the same *pointer*
  // which you used in RegisterClass, the same *string* sometimes fails
  FMainWnd := CreateWindowEx(WS_EX_DLGMODALFRAME or WS_EX_TOPMOST,
                             wndClass.lpszClassname, pchar(s1),
                             WS_CAPTION or WS_SYSMENU or c1,
                             0, 0, 100, 100, 0, 0, HInstance, nil);
  FDialogWndProc := MethodToProcedure(self, @TExceptionBox.DialogWndMethod);
  SetWindowLong(FMainWnd, GWL_WNDPROC, integer(FDialogWndProc));
  if defaultMsgBox or (not FExc.AutoShowBugReport) then begin
    c1 := GetSystemMenu(FMainWnd, false);
    RemoveMenu(c1, SC_MINIMIZE, MF_BYCOMMAND);
    RemoveMenu(c1, SC_MAXIMIZE, MF_BYCOMMAND);
    RemoveMenu(c1, SC_RESTORE,  MF_BYCOMMAND);
    RemoveMenu(c1, SC_SIZE,     MF_BYCOMMAND);
  end;
  FFont1 := CreateFont(-11, 0, 0, 0, 400, 0, 0, 0, DEFAULT_CHARSET,
                       OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                       DEFAULT_PITCH or FF_DONTCARE, 'Tahoma');
  if FFont1 = 0 then
    FFont1 := CreateFont(-12, 0, 0, 0, 400, 0, 0, 0, DEFAULT_CHARSET,
                         OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                         DEFAULT_PITCH or FF_DONTCARE, 'MS Sans Serif');
  FFont2 := CreateFont(-11, 0, 0, 0, 400, 0, 0, 0, DEFAULT_CHARSET,
                       OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                       DEFAULT_PITCH or FF_DONTCARE, 'Courier New');
  FFont3 := CreateFont(-8, 0, 0, 0, 400, 0, 0, 0, DEFAULT_CHARSET,
                       OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                       DEFAULT_PITCH or FF_DONTCARE, 'Small Fonts');
  FTmpDC := CreateCompatibleDC(0);
  FOldFont := SelectObject(FTmpDC, FFont1);
  SetBkMode(FTmpDC, TRANSPARENT);
  lastW     := 0;
  sendV     :=  FExc.SendBtnVisible     and (not defaultMsgBox);
  saveV     :=  FExc.SaveBtnVisible     and (not defaultMsgBox);
  printV    :=  FExc.PrintBtnVisible    and (not defaultMsgBox);
  detailsV  := (FExc.ShowBtnVisible     and (not defaultMsgBox)) or (defaultMsgBox and detailsBtn);
  continueV :=  FExc.ContinueBtnVisible                          or  defaultMsgBox;
  restartV  :=  FExc.RestartBtnVisible  and (not defaultMsgBox);
  closeV    :=  FExc.CloseBtnVisible    and (not defaultMsgBox);
  if defaultMsgBox then begin
    FBtnHeight := 23;
    continueC := FExc.OkBtnCaption;
    detailsC := FExc.DetailsBtnCaption;
  end else begin
    if FExc.NoOwnerDrawButtons then
         FBtnHeight := 26
    else FBtnHeight := 22;
    continueC := FExc.ContinueBtnCaption;
    detailsC := FExc.ShowBtnCaption;
  end;
  sendW     := GetTextWidth(4 + 27 + 8, FExc.   SendBtnCaption,     sendV);
  saveW     := GetTextWidth(4 + 27 + 8, FExc.   SaveBtnCaption,     saveV);
  printW    := GetTextWidth(4 + 27 + 8, FExc.  PrintBtnCaption,    printV);
  detailsW  := GetTextWidth(4 + 27 + 8,               detailsC,  detailsV);
  continueW := GetTextWidth(4 + 27 + 8,              continueC, continueV, @lastW);
  restartW  := GetTextWidth(4 + 27 + 8, FExc.RestartBtnCaption,  restartV, @lastW);
  closeW    := GetTextWidth(4 + 27 + 8, FExc.  CloseBtnCaption,    closeV, @lastW);
  if defaultMsgBox then begin
    if continueW < 75 then continueW := 79;
    if  detailsW < 75 then  detailsW := 79;
  end;
  if lastW > sendW then i1 := lastW
  else                  i1 := sendW;
  if detailsW > i1 then i1 := detailsW;
  if saveW    > i1 then i1 := saveW;
  if printW   > i1 then i1 := printW;
  ZeroMemory(@bi, sizeOf(bi));
  FTmpWidth := i1 - 4;
  bi.bmiHeader.biSize     := sizeOf(bi.bmiHeader);
  bi.bmiHeader.biWidth    := FTmpWidth;
  bi.bmiHeader.biHeight   := FBtnHeight;
  bi.bmiHeader.biPlanes   := 1;
  bi.bmiHeader.biBitCount := 32;
  FTmpBmp := CreateDIBSection(FTmpDC, bi, DIB_RGB_COLORS, pointer(FTmpBits), 0, 0);
  FOldBmp := SelectObject(FTmpDC, FTmpBmp);
  doFocus := true;
  ih := ord(continueV) + ord(restartV) + ord(closeV);
  view2 := (ih > 1) and ( sendV or saveV or printV or detailsV );
  if ih = 1 then
    ih := 2;
  if defaultMsgBox then begin
    bw := 12 + continueW + 14;
    if detailsBtn then
      bw := bw + 2 + detailsW;
    iw := 11 + 11 + 4;
  end else
    if view2 then begin
      bw := 6 + sendW + saveW + printW + detailsW + lastW + 8;
      iw := 11 + 11 + 10 + lastW + 8;
    end else begin
      bw := 6 + sendW + saveW + printW + detailsW + continueW + restartW + closeW + 8;
      iw := 11 + 11 + 4;
    end;
  if FindResource(FExc.Module, 'MEIBIG', PChar(RT_BITMAP)) <> 0 then begin
    inc(iw, 32 + 8);
    FIcoWnd := CreateWindow_('Button', '', BS_OWNERDRAW or WS_DISABLED, 9, 10, 32, 32, FMainWnd);
    SetWindowLong(FIcoWnd, GWL_USERDATA, 8);
  end else
    FIcoWnd := 0;
  if CFullWidth > bw then
       SetRect(rect, 0, 0, CFullWidth - iw, 0)
  else SetRect(rect, 0, 0, bw         - iw, 0);
  DrawText(FTmpDC, pchar(errorTxt), -1, rect, DT_CALCRECT or DT_WORDBREAK);
  if rect.Right + iw > bw then
       iw := rect.Right + iw
  else iw := bw;
  fw := CFullWidth;
  if fw > GetSystemMetrics(SM_CXSCREEN) - GetSystemMetrics(SM_CYCAPTION) * 5 then
    fw := GetSystemMetrics(SM_CXSCREEN) - GetSystemMetrics(SM_CYCAPTION) * 5;
  if (not defaultMsgBox) and FExc.AutoShowBugReport and (fw > iw) then
    iw := fw;
  if (FIcoWnd <> 0) and (rect.Bottom < 26) then
       iy := 5 + 26
  else iy := 5 + rect.Bottom;
  if ih = 3 then
    if view2 then begin
      dec(iy, FBtnHeight + 4);
      if iy < 7 then
        iy := 7;
    end else
      ih := 2;
  if defaultMsgBox then begin
    if detailsBtn then
      ix := (iw - continueW + 4 - detailsW + 4 - 6) div 2 - 3
    else
      ix := (iw - continueW + 4) div 2 - 3;
    FContinue := CreateButton(continueC, continueV, 4, ix, iy, 1, continueW);
    inc(ix, 2);
    FDetails  := CreateButton( detailsC,  detailsV, 3, ix, iy, 1, detailsW);
    ih := iy + (FBtnHeight + 4) * 2 + 13;
  end else begin
    ix := 6;
    FSend     := CreateButton(FExc. SendBtnCaption,    sendV, 0, ix, iy, ih - 1,    sendW);
    FSave     := CreateButton(FExc. SaveBtnCaption,    saveV, 1, ix, iy, ih - 1,    saveW);
    FPrint    := CreateButton(FExc.PrintBtnCaption,   printV, 2, ix, iy, ih - 1,   printW);
    FDetails  := CreateButton(            detailsC, detailsV, 3, ix, iy, ih - 1, detailsW);
    if view2 then
         ix := iw - lastW - 8
    else ix := iw - continueW - restartW - closeW - 8;
    FContinue := CreateButton(             continueC, continueV, 4, ix, iy, 0,   continueW);
    FRestart  := CreateButton(FExc.RestartBtnCaption,  restartV, 5, ix, iy, ord(FExc.ContinueBtnVisible), restartW);
    FClose    := CreateButton(FExc.  CloseBtnCaption,    closeV, 6, ix, iy, ih - 1, closeW);
    ih := iy + (FBtnHeight + 4) * ih + 8;
  end;
  if continueV and (not FExc.CanContinue) and (restartV or closeV) then begin
    EnableWindow(FContinue, false);
    SetWindowLong(FContinue, GWL_USERDATA, 7);
  end;
  rect.Left := 11;
  rect.Top  := 12;
  if FIcoWnd <> 0 then begin
    rect.Left := 11 + 32 + 8;
    if rect.Bottom < 26 then
      rect.Top := 12 + (26 - rect.Bottom) div 2;
  end;
  CreateWindow_('Static', errorTxt, SS_LEFT, rect.Left, rect.Top, rect.Right, rect.Bottom, FMainWnd);
  {$ifdef noncommercial}
    if (not defaultMsgBox) or detailsBtn then begin
      FNCLabelY := ih - 8;
      if (not defaultMsgBox) and FExc.AutoShowBugReport then
           inc(ih, 9)
      else inc(ih, 11);
    end;
  {$endif}
  iy := ih;
  if (not defaultMsgBox) and FExc.AutoShowBugReport then
    inc(ih, CFullWidth * 10 div CRatio);
  if ih > GetSystemMetrics(SM_CYSCREEN) - GetSystemMetrics(SM_CYCAPTION) * 5 then
    ih := GetSystemMetrics(SM_CYSCREEN) - GetSystemMetrics(SM_CYCAPTION) * 5;
  if (not defaultMsgBox) and FExc.AutoShowBugReport then begin
    FTabs := CreateWindow_(WC_TABCONTROL, '', 0, 5, iy - 4, iw - 15, ih - iy - 6, FMainWnd);
    FListViewWndProc := MethodToProcedure(self, @TExceptionBox.ListViewWndProc);
    FOldListViewWndProc := pointer(SetWindowLong(FTabs, GWL_WNDPROC, integer(FListViewWndProc)));
    CheckTabs;
    CheckCurrentTab;
  end;
  inc(ih, GetSystemMetrics(SM_CYCAPTION));
  if (not defaultMsgBox) and FExc.AutoShowBugReport then begin
    iw := iw + (GetSystemMetrics(SM_CXSIZEFRAME) - GetSystemMetrics(SM_CXFIXEDFRAME)) * 2;
    ih := ih + (GetSystemMetrics(SM_CYSIZEFRAME) - GetSystemMetrics(SM_CYFIXEDFRAME)) * 2;
  end;
  SetWindowPos(FMainWnd, 0, (GetSystemMetrics(SM_CXSCREEN) - iw) div 2,
                            (GetSystemMetrics(SM_CYSCREEN) - ih) div 2,
                            iw, ih, SWP_NOZORDER);
  GetWindowRect(FMainWnd, rect);
  FMinWidth  := rect.Right  - rect.Left;
  FMinHeight := rect.Bottom - rect.Top;
  FSizingDone := true;
  if not defaultMsgBox then
    if      CheckAutoBtn(FExc.AutoClose  ) and closeV                         then FTimerBtn := FClose
    else if CheckAutoBtn(FExc.AutoRestart) and restartV                       then FTimerBtn := FRestart
    else if FExc.AutoContinue              and continueV and FExc.CanContinue then FTimerBtn := FContinue;
  if FTimerBtn <> 0 then begin
    FTime  := GetTickCount;
    FTimer := SetTimer(FMainWnd, 777, 10, nil);
  end;
  if @OnExceptBoxCreate <> nil then
    OnExceptBoxCreate(FMainWnd, defaultMsgBox);
  if makeSound then
    MessageBeep(MB_ICONHAND);
  ShowWindow(FMainWnd, SW_SHOWNORMAL);
  if IsIconic(FMainWnd) then
    ShowWindow(FMainWnd, SW_RESTORE);
  BringWindowToTop(FMainWnd);
  SetForegroundWindow(FMainWnd);
  if FExc.ExceptType = etFrozen then
    PostMessage(AntiFreezeWnd, AntiFreezeMsg, FMainWnd, 0);
  FExc.RegisterBugReportCallback(BugReportChanged, false);
end;

destructor TExceptionBox.Destroy;
begin
  {$ifdef log}unindentLog; log('TExceptionBox.Destroy');{$endif}
  if FImageList <> 0 then
    ImageList_Destroy(FImageList);
  FExc.UnregisterBugReportCallback(BugReportChanged);
  VirtualFree(FDialogWndProc, 0, MEM_RELEASE);
  if FListViewWndProc <> nil then VirtualFree(FListViewWndProc, 0, MEM_RELEASE);
  SelectObject(FTmpDC, FOldBmp );
  SelectObject(FTmpDC, FOldFont);
  DeleteDC    (FTmpDC);
  DeleteObject(FTmpBmp);
  DeleteObject(FFont1);
  DeleteObject(FFont2);
  DeleteObject(FFont3);
  inherited;
end;

function DefaultBugReportHtml(bugReport: string; settings: IMESettings) : string;
var s1, s2 : string;
    i1, i2 : integer;
begin
  result := '<html><head><title>' + settings.TitleBar + '</title></head><body>' + #$D#$A +
            '<table border="0" width="100%" height="100%">' + #$D#$A +
            '<tr><td colspan=4 style="height:2pt;"></td></tr>' + #$D#$A +
            '<tr><td colspan=4 valign="top">' +
            '<p style="font-family:Tahoma,''MS Sans Serif'';font-size:13pt;margin-left:4px;">';
  s1 := settings.ExceptMsg;
  i1 := PosStr(#$D#$A + 'exception message ', bugReport);
  s2 := '';
  if i1 > 0 then begin
    i2 := PosStr(#$D#$A, bugReport, i1 + 10);
    if i2 > 0 then begin
      s2 := Copy(bugReport, i1, i2 - i1);
      i1 := PosStr(':', s2);
      Delete(s2, 1, i1);
      TrimStr(s2);
    end;
  end;
  ReplaceText(s1, '%LF%', '<br>' + #$D#$A);
  ExpandVars(settings.Module, s1, s2, bugReport);
  result := result + s1 + '</p></td></tr>' + #$D#$A +
            '<tr><td colspan=4 style="height:8pt;"></td></tr>' + #$D#$A +
            '<tr><td><button onClick="history.back();" style="height:19.5pt;">&nbsp;';
  if settings.ContinueBtnVisible then
       s1 := settings.ContinueBtnCaption
  else s1 := 'continue application';
  result := result + s1 + '&nbsp;</button></td>' + #$D#$A +
            '<td><p>&nbsp;&nbsp;</p></td>' + #$D#$A +
            '<td>';
  if (not settings.AutoShowBugReport) and settings.ShowBtnVisible then
    s1 := '<button onClick="document.getElementById(''bugReport'').style.' +
          'visibility=''visible'';this.style.visibility=''hidden'';" ' +
          'style="height:19.5pt;">&nbsp;' + settings.ShowBtnCaption + '&nbsp;</button>'
  else
    s1 := '&nbsp;';
  result := result + s1 + '</td>' + #$D#$A +
            '<td width="100%"></td></tr>' + #$D#$A +
            '<tr><td colspan=4 style="height:8pt;"></td></tr>' + #$D#$A +
            '<tr><td colspan=4 height="100%">' + #$D#$A +
            '<textarea id="bugReport" readonly cols="80" rows="20" style="width:100%;height:100%;';
  if not settings.AutoShowBugReport then
    result := result + 'visibility:hidden;';
  result := result + '">' + #$D#$A +
            bugReport + '</textarea></td></tr></table></body></html>';
end;

procedure ShowException(exceptIntf: IMEException);

  procedure ShowExceptBox(showSetting: TMEShowSetting; makeSound: boolean = true);
  var eb  : TExceptionBox;
      msg : TMsg;
      b1  : boolean;
  begin
    eb := TExceptionBox.Create(exceptIntf, showSetting <> ssFullBox, showSetting = ssDetailBox, makeSound);
    while IsWindow(eb.Handle) and (dword(integer(GetMessage(msg, 0, 0, 0)) + 1) > 1) do begin
      if (msg.message = WM_KEYDOWN) and (msg.wParam = VK_ESCAPE) then begin
        msg.hwnd    := eb.Handle;
        msg.message := WM_CLOSE;
        msg.wParam  := 0;
        msg.lParam  := 0;
      end else
        if (msg.message = WM_KEYDOWN) and (msg.wParam = ShowBugReportKey) and
           (GetKeyState(VK_SHIFT  ) and $80 = $80) and
           (GetKeyState(VK_CONTROL) and $80 = $80) and
           (GetKeyState(VK_MENU   ) and $80 = $80) and
           (showSetting in [ssDetailBox, ssFullBox]) and
           (not exceptIntf.AutoShowBugReport) then
          eb.DoDetails;
      if (msg.hwnd = eb.FEdit) or (not IsDialogMessage(eb.Handle, msg)) then begin
        TranslateMessage(msg);
        DispatchMessage(msg);
      end;
    end;
    b1 := (showSetting in [ssDetailBox, ssFullBox]) and eb.FReshow;
    eb.Free;
    if b1 then begin
      exceptIntf.AutoShowBugReport := true;
      exceptIntf.AutoContinue      := false;
      exceptIntf.AutoRestart       := 0;
      exceptIntf.AutoClose         := 0;
      ShowExceptBox(ssFullBox, false);
    end;
  end;

var s1    : string;
    assis : INVAssistant;
begin
  {$ifdef log}log('ShowException, frozen: ' + booleanToChar(exceptIntf.ExceptType = etFrozen) + ', exceptObject: ' + IntToHexEx(dword(exceptIntf.ExceptObject)) + ', exceptAddr: ' + IntToHexEx(dword(exceptIntf.ExceptAddr)) + ', canContinue: ' + booleanToChar(exceptIntf.CanContinue));{$endif}
  if DetectConsole and IsConsole then begin
    if CGIApp_TCGIApplication_CGIHandleException = nil then begin
      if exceptIntf.ExceptType = etFrozen then
           s1 := exceptIntf.FrozenMsg
      else s1 := exceptIntf.ExceptMsg;
      ExpandVars(exceptIntf.Module, s1, exceptIntf.ExceptMessage, '');
      WriteLn(s1);
      if exceptIntf.AutoShowBugReport or exceptIntf.ShowBtnVisible then begin
        exceptIntf.ShowPleaseWaitBox := false;
        WriteLn;
        WriteLn(exceptIntf.GetBugReport(true));
      end;
    end else
      WriteLn('Content-Type: text/html' + #$D#$A + #$D#$A +
              BugReportHtml(exceptIntf.GetBugReport(true), exceptIntf));
  end else
    case exceptIntf.ShowSetting of
      ssFullBox   : ShowExceptBox(ssFullBox);
      ssAssistant : begin
                      assis := exceptIntf.GetAssistant(exceptIntf.ShowAssistant);
                      if assis <> nil then begin
                        if assis.ShowModal = nvmOk then
                          PutAssisIntoBugReport(assis, exceptIntf);
                        (exceptIntf as IMESettingsEx).FillFormCache(assis);
                      end else
                        ShowExceptBox(ssFullBox);
                    end;
      ssDetailBox : ShowExceptBox(ssDetailBox);
      ssSimpleBox : ShowExceptBox(ssSimpleBox);
    end;
end;

// ***************************************************************
// move exceptions to (and then handle them in) a seperate thread

type
  THandleExceptionParams = record
    event           : dword;
    callingThreadId : dword;
    exceptType      : TExceptType;
    exceptObject    : TObject;
    exceptAddr      : pointer;
    canContinue     : boolean;
    currentEsp      : dword;
    currentEbp      : dword;
    context         : PContext;
    showReport      : TPString;
    source          : TExceptSource;
    relatedObject   : TObject;
    package         : dword;
    preparedStack   : pointer;
  end;

var
  HandleExceptionTid : dword = $FFFFFFFF;
  HandleExceptionTh  : dword = 0;
  HandleExceptionRP  : dword = 0;  //  read pipe
  HandleExceptionWP  : dword = 0;  // write pipe

threadvar
  // bug report cache for ISAPI dlls
  isapiBugReport  : string;
  isapiExceptAddr : pointer;

procedure CheckAutoSave(const exceptIntf: IMEException; final: boolean);
var b1 : boolean;
begin
  if exceptIntf.AutoSave and
     ( (not exceptIntf.AutoSaveIfNotSent) or
       (final and (not (exceptIntf as IMEExceptionEx).GetMailWasSent) ) ) and
     (exceptIntf as IMEExceptionEx).FirstAutoSave then begin
    b1 := false;
    DoFireHandlers(0, 1, eaSaveBugReport, exceptIntf, b1);
    if not b1 then                                                    
      SaveBugReportEx('', exceptIntf.BugReport, 0, true, exceptIntf);
  end;
end;

procedure DoExceptionAutoStuff(complete: boolean; bugReport: string; exceptIntf: IMEException);
var b1 : boolean;
    ss : INVBitmap;
begin
  if complete and (exceptIntf as IMEExceptionEx).FirstCompleteCallback then begin
//    CheckAutoSave(exceptIntf, false);
    if exceptIntf.AutoSend then begin
      b1 := false;
      DoFireHandlers(0, 1, eaSendBugReport, exceptIntf, b1);
      if not b1 then begin
        if exceptIntf.AppendScreenShot then
             ss := exceptIntf.ScreenShot
        else ss := nil;
        with exceptIntf as IMEExceptionEx do
          if PSendThread^ <> 0 then
            CloseHandle(PSendThread^);
        SendBugReportEx(bugReport, ss, 0, true, true, exceptIntf, false, (exceptIntf as IMEExceptionEx).PSendThread);
      end;
    end;
    if exceptIntf.AutoClipboard then
      FillClipboard(bugReport)
    else
      exceptIntf.UnregisterBugReportCallback(DoExceptionAutoStuff);
  end;
end;

procedure CheckAutoStuff(const exceptIntf: IMEException; waitForAutoSend: boolean);
begin
  if (exceptIntf as IMEExceptionEx).CriticalBugReportCallbackExists or
     ( CompletePhaseHandlerExists and exceptIntf.CallHandlers ) then
    exceptIntf.BugReport;
  CheckAutoSave(exceptIntf, true);
  if waitForAutoSend then
    with exceptIntf as IMEExceptionEx do
      if PSendThread^ <> 0 then begin
        WaitForSingleObject(PSendThread^, INFINITE);
        CloseHandle(PSendThread^);
        PSendThread^ := 0;
      end;
end;

procedure ReceiveHandleException(var hep: THandleExceptionParams);
var handled  : boolean;
    b1       : boolean;
    tl1, tl2 : TDACardinal;
    exc      : IMEException;
begin
  {$ifdef log}log('ReceiveHandleException');{$endif}
  tl1 := nil;
  tl2 := nil;
  try
    with hep do begin
      {$ifdef log}log('  frozen: ' + booleanToChar(exceptType = etFrozen) + ', exceptObject: ' + IntToHexEx(dword(exceptObject)) + ', exceptAddr: ' + IntToHexEx(dword(exceptAddr)) + ', canContinue: ' + booleanToChar(canContinue));{$endif}
      ClearTempPath;
      CheckExceptAddr(exceptObject, exceptAddr);
      exc := NewException(exceptType, exceptObject, exceptAddr, canContinue,
                          callingThreadId, currentEsp, currentEbp, context,
                          MESettings(exceptAddr), source, relatedObject, package, preparedStack);
      (exc as IMEExceptionEx).SetPhase(epQuickFiltering);
      if exceptType = etHidden then begin
        handled := true;
        DoFireHandlers(0, 2, eaSendBugReport, exc, handled);
      end else begin
        handled := false;
        Move(Last10Exceptions[0], Last10Exceptions[1], 9 * 8);
        Last10Exceptions[0] := GetSeconds;
      end;
      if not handled then
        DoFireHandlers(0, 0, eaSendBugReport, exc, handled);
      (exc as IMEExceptionEx).SetPhase(epMainPhase);
      if not handled then begin
        if (exc.ExceptType <> etFrozen) and exc.SuspendThreads and
           ( (HInstance = GetModuleHandle(nil)) or AmMeBpl) then
          // only the application should suspend the running threads
          // if e.g. an ISAPI dll would suspend the whole process -> bad!
          // the madExcept bpl may suspend, too, though
          SuspendDelphiThreads(true, (exc as IMEExceptionEx).GetCompleteThreadID);
        try
          if not handled then begin
            if exc.CreateScreenShot then
              exc.ScreenShot;
            if exc.CreateBugReport then
              exc.GetBugReport(false);
            DoFireHandlers(0, 0, eaSendBugReport, exc, handled);
          end;
          (exc as IMEExceptionEx).SetPhase(epPostProcessing);
          if not handled then
            DoFireHandlers(0, 0, eaSendBugReport, exc, handled);
          if (not handled) or (not exc.CanContinue) then begin
            if not handled then begin
              if exc.AutoSave or exc.AutoSend or exc.AutoClipboard then
                exc.RegisterBugReportCallback(DoExceptionAutoStuff, true);
              if AmHttpServer or (DetectConsole and IsConsole) then begin
                if exc.ShowSetting <> ssNothing then
                  if not AmHttpServer then
                    exc.Show
                  else
                    if showReport <> nil then
                      showReport^ := exc.GetBugReport(true);
              end else
                exc.Show;
            end;
            if (not exc.CanContinue) or (exc.ShowSetting = ssNothing) or AmHttpServer or (DetectConsole and IsConsole) then
              if CheckAutoBtn(exc.AutoClose) or ((not exc.CanContinue) and (not CheckAutoBtn(exc.AutoRestart))) then begin
                b1 := false;
                DoFireHandlers(0, 1, eaCloseApplication, exc, b1);
                CheckAutoStuff(exc, true);
                CloseApplication;
              end else
                if CheckAutoBtn(exc.AutoRestart) then begin
                  b1 := false;
                  DoFireHandlers(0, 1, eaRestartApplication, exc, b1);
                  CheckAutoStuff(exc, true); 
                  RestartApplication;
                end else begin
                  b1 := false;
                  DoFireHandlers(0, 1, eaContinueApplication, exc, b1);
                end;
            if not handled then
              CheckAutoStuff(exc, false);
          end;
          AntiFreezeCnt := 0;
        finally
          if (exc.ExceptType <> etFrozen) and exc.SuspendThreads and
             ( (HInstance = GetModuleHandle(nil)) or AmMeBpl ) then
            SuspendDelphiThreads(false, 0);
        end;
      end;
    end;
  except end;
end;

function HandleExceptionThread(dummy: pointer) : dword; stdcall;
var hep : THandleExceptionParams;
    c1  : dword;
begin
  {$ifdef log}log('HandleExceptionThread');{$endif}
  result := 0;
  while ReadFile(HandleExceptionRP, hep, sizeOf(hep), c1, nil) and (hep.event <> 0) do
    if c1 = sizeOf(hep) then begin
      ReceiveHandleException(hep);
      SetEvent(hep.event);
      PostThreadMessage(hep.callingThreadId, WM_NULL, 0, 0);
    end;
end;

procedure InitHandleExceptionThread;
begin
  {$ifdef log}log('InitHandleExceptionThread');{$endif}
  if CreatePipe(HandleExceptionRP, HandleExceptionWP, nil, 0) then begin
    HandleExceptionTh := CreateThread(nil, 0, @HandleExceptionThread, nil, 0, HandleExceptionTid);
    if HandleExceptionTh <> 0 then begin
      SetThreadInfo(HandleExceptionTid, HandleExceptionTh, '-', '', nil, 0);
      SetThreadPriority(HandleExceptionTh, THREAD_PRIORITY_HIGHEST);
    end;
  end;
end;

procedure CloseHandleExceptionThread;
var hep : THandleExceptionParams;
    c1  : dword;
begin
  {$ifdef log}log('CloseHandleExceptionThread');{$endif}
  if HandleExceptionTid <> $FFFFFFFF then begin
    ResumeThread(HandleExceptionTh);  // just in case...
    if not IsProcessBlocked then begin
      ZeroMemory(@hep, sizeOf(hep));
      WriteFile(HandleExceptionWP, hep, sizeOf(hep), c1, nil);
      CloseHandle(HandleExceptionWP);
      WaitForSingleObject(HandleExceptionTh, 500);
    end else
      CloseHandle(HandleExceptionWP);
    if WaitForSingleObject(HandleExceptionTh, 0) <> WAIT_OBJECT_0 then
      TerminateThread(HandleExceptionTh, 0);
    CloseHandle(HandleExceptionTh);
    CloseHandle(HandleExceptionRP);
    SetThreadInfo(HandleExceptionTid, 0, '', '', nil, 0);
  end;
end;

// ***************************************************************
// exported functions

var OutOfMemoryInsurance : array [boolean] of array [1..4] of pointer;

procedure HandleException(exceptType    : TExceptType   = etNormal;
                          exceptObject  : TObject       = nil;
                          exceptAddr    : pointer       = nil;
                          canContinue   : boolean       = true;
                          currentEsp    : dword         = 0;
                          currentEbp    : dword         = 0;
                          context       : PContext      = nil;
                          source        : TExceptSource = esManual;
                          relatedObject : TObject       = nil;
                          package       : dword         = 0;
                          showReport    : TPString      = nil     );
var hep           : THandleExceptionParams;
    b1            : boolean;
    c1            : dword;
    msg           : TMsg;
    preparedStack : pointer;
begin
  {$ifdef log}log('HandleException, frozen: ' + booleanToChar(exceptType = etFrozen) + ', exceptObject: ' + IntToHexEx(dword(exceptObject)) + ', exceptAddr: ' + IntToHexEx(dword(exceptAddr)) + ', canContinue: ' + booleanToChar(canContinue));{$endif}
  try
    if GetCurrentThreadId = HandleExceptionTid then
      // we ignore exceptions of our private exception handling thread
      exit;
    if AmSyncingHandlers and (GetCurrentThreadId = MainThreadId) then
      // this exception occurred in the main thread
      // but the main thread is currently executing an exception handler
      // so in order to avoid conflicts, we ignore this exception
      exit;
    for b1 := low(boolean) to high(boolean) do
      for c1 := low(OutOfMemoryInsurance[b1]) to high(OutOfMemoryInsurance[b1]) do
        if OutOfMemoryInsurance[b1, c1] <> nil then begin
          VirtualFree(OutOfMemoryInsurance[b1, c1], 0, MEM_RELEASE);
          OutOfMemoryInsurance[b1, c1] := nil;
        end;
    preparedStack := nil;
    CheckExceptParams(exceptObject, exceptAddr, preparedStack, context);
    if (exceptObject <> nil) and IsClass(exceptObject, 'EAbort') then begin
      LastExceptObject := nil;
      LastExceptAddr := nil;
      if not canContinue then
        CloseApplication;
      exit;
    end;
    if currentEsp = 0 then currentEsp := Esp;
    if currentEbp = 0 then currentEbp := Ebp;
    if (context <> nil) and (dword(exceptAddr) >= context.Eip) and (dword(exceptAddr) - context.Eip <= 4) then begin
      currentEsp := context.Esp;
      currentEbp := context.Ebp;
    end;
    hep.callingThreadId := GetCurrentThreadId;
    hep.exceptType      := exceptType;
    hep.exceptObject    := exceptObject;
    hep.exceptAddr      := exceptAddr;
    hep.canContinue     := canContinue;
    hep.currentEsp      := currentEsp;
    hep.currentEbp      := currentEbp;
    hep.context         := context;
    hep.showReport      := showReport;
    hep.source          := source;
    hep.relatedObject   := relatedObject;
    hep.package         := package;
    hep.preparedStack   := preparedStack;
    if IsProcessBlocked(250) then
         hep.event := 0
    else hep.event := CreateEvent(nil, true, false, nil);
    PauseFreezeCheck(true);
    try
      if hep.event <> 0 then begin
        if WriteFile(HandleExceptionWP, hep, sizeOf(hep), c1, nil) and (c1 = sizeOf(hep)) then
          if GetCurrentThreadId = MainThreadId then begin
            while WaitForSingleObject(hep.event, 0) = WAIT_TIMEOUT do
              if dword(integer(GetMessage(msg, 0, 0, 0)) + 1) > 1 then begin
                TranslateMessage(msg);
                DispatchMessage(msg);
              end else begin
                PostQuitMessage(msg.wParam);
                break;
              end;
          end else
            WaitForSingleObject(hep.event, INFINITE);
        CloseHandle(hep.event);
      end else
        ReceiveHandleException(hep);
    finally PauseFreezeCheck(false) end;
    LastExceptObject := nil;
    LastExceptAddr := nil;
    if ResetFpuMode then
      _FpuInit;
    if ((HiddenHandlers <> nil) or (HiddenHandlersOO <> nil)) and (exceptObject <> nil) then
      // the hidden handler is not supposed to see exceptions we've already handled
      if IsClass(exceptObject, 'MadException') or IsClass(exceptObject, 'Exception') then
        MadException(exceptObject).Message := #0;
    for c1 := low(OutOfMemoryInsurance[false]) to high(OutOfMemoryInsurance[false]) do begin
      OutOfMemoryInsurance[false, c1] := VirtualAlloc(nil, 4096 * 1024, MEM_RESERVE, PAGE_NOACCESS);
      OutOfMemoryInsurance[true,  c1] := VirtualAlloc(nil,  512 * 1024, MEM_COMMIT,  PAGE_NOACCESS);
    end;
  except end;
end;

procedure RegisterHandler(var list: TDAExceptHandler; handler: pointer; sync: TSyncType; phase: TExceptPhase);
var i1, i2 : integer;
begin
  if HandlerSection = nil then begin
    New(HandlerSection);
    InitializeCriticalSection(HandlerSection^);
  end;
  EnterCriticalSection(HandlerSection^);
  try
    i2 := Length(list);
    for i1 := 0 to i2 - 1 do
      if @list[i1].handler = handler then begin
        list[i1].sync  := sync;
        list[i1].phase := phase;
        exit;
      end;
    SetLength(list, i2 + 1);
    @list[i2].handler := handler;
    list[i2].sync  := sync;
    list[i2].phase := phase;
  finally LeaveCriticalSection(HandlerSection^) end;
end;

procedure RegisterHandlerOO(var list: TDAExceptHandlerOO; handler: int64; sync: TSyncType; phase: TExceptPhase);
var i1, i2 : integer;
begin
  if HandlerSection = nil then begin
    New(HandlerSection);
    InitializeCriticalSection(HandlerSection^);
  end;
  EnterCriticalSection(HandlerSection^);
  try
    i2 := Length(list);
    for i1 := 0 to i2 - 1 do
      if int64(TMethod(list[i1].handler)) = handler then begin
        list[i1].sync  := sync;
        list[i1].phase := phase;
        exit;
      end;
    SetLength(list, i2 + 1);
    int64(TMethod(list[i2].handler)) := handler;
    list[i2].sync  := sync;
    list[i2].phase := phase;
  finally LeaveCriticalSection(HandlerSection^) end;
end;

function UnregisterHandler(var list: TDAExceptHandler; handler: pointer) : boolean;
var i1, i2 : integer;
begin
  result := false;
  if HandlerSection <> nil then begin
    EnterCriticalSection(HandlerSection^);
    try
      i1 := high(list);
      for i2 := i1 downto 0 do
        if @list[i2].handler = handler then begin
          list[i2] := list[i1];
          dec(i1);
          result := true;
        end;
      SetLength(list, i1 + 1);
    finally LeaveCriticalSection(HandlerSection^) end;
  end;
end;

function UnregisterHandlerOO(var list: TDAExceptHandlerOO; handler: int64) : boolean;
var i1, i2 : integer;
begin
  result := false;
  if HandlerSection <> nil then begin
    EnterCriticalSection(HandlerSection^);
    try
      i1 := high(list);
      for i2 := i1 downto 0 do
        if int64(TMethod(list[i2].handler)) = handler then begin
          list[i2] := list[i1];
          dec(i1);
          result := true;
        end;
      SetLength(list, i1 + 1);
    finally LeaveCriticalSection(HandlerSection^) end;
  end;
end;

procedure RegisterExceptionHandler(exceptHandler: TExceptEvent; sync: TSyncType; phase: TExceptPhase);
begin
  {$ifdef log}log('RegisterExceptionHandler, exceptHandler: ' + IntToHexEx(dword(@exceptHandler)));{$endif}
  RegisterHandler(ExceptHandlers, @exceptHandler, sync, phase);
end;

procedure RegisterExceptionHandler(exceptHandler: TExceptEventOO; sync: TSyncType; phase: TExceptPhase);
begin
  {$ifdef log}log('RegisterExceptionHandlerOO, exceptHandler: ' + IntToHexEx(int64(TMethod(exceptHandler))));{$endif}
  RegisterHandlerOO(ExceptHandlersOO, int64(TMethod(exceptHandler)), sync, phase);
end;

function UnregisterExceptionHandler(exceptHandler: TExceptEvent) : boolean;
begin
  {$ifdef log}log('UnregisterExceptionHandler, exceptHandler: ' + IntToHexEx(dword(@exceptHandler)));{$endif}
  result := UnregisterHandler(ExceptHandlers, @exceptHandler);
end;

function UnregisterExceptionHandler(exceptHandler: TExceptEventOO) : boolean;
begin
  {$ifdef log}log('UnregisterExceptActionHandlerOO, exceptHandler: ' + IntToHexEx(int64(TMethod(exceptHandler))));{$endif}
  result := UnregisterHandlerOO(ExceptHandlersOO, int64(TMethod(exceptHandler)));
end;

procedure RegisterExceptActionHandler(actionHandler: TExceptActionEvent; sync: TSyncType);
begin
  {$ifdef log}log('RegisterExceptActionHandler, actionHandler: ' + IntToHexEx(dword(@actionHandler)));{$endif}
  RegisterHandler(TDAExceptHandler(ActionHandlers), @actionHandler, sync, epMainPhase);
end;

procedure RegisterExceptActionHandler(actionHandler: TExceptActionEventOO; sync: TSyncType);
begin
  {$ifdef log}log('RegisterExceptActionHandlerOO, actionHandler: ' + IntToHexEx(int64(TMethod(actionHandler))));{$endif}
  RegisterHandlerOO(TDAExceptHandlerOO(ActionHandlersOO), int64(TMethod(actionHandler)), sync, epMainPhase);
end;

function UnregisterExceptActionHandler(actionHandler: TExceptActionEvent) : boolean;
begin
  {$ifdef log}log('UnregisterExceptActionHandler, actionHandler: ' + IntToHexEx(dword(@actionHandler)));{$endif}
  result := UnregisterHandler(TDAExceptHandler(ActionHandlers), @actionHandler);
end;

function UnregisterExceptActionHandler(actionHandler: TExceptActionEventOO) : boolean;
begin
  {$ifdef log}log('UnregisterExceptActionHandlerOO, actionHandler: ' + IntToHexEx(int64(TMethod(actionHandler))));{$endif}
  result := UnregisterHandlerOO(TDAExceptHandlerOO(ActionHandlersOO), int64(TMethod(actionHandler)));
end;

procedure RegisterHiddenExceptionHandler(hiddenHandler: TExceptEvent; sync: TSyncType);
begin
  {$ifdef log}log('RegisterHiddenExceptionHandler, hiddenHandler: ' + IntToHexEx(dword(@hiddenHandler)));{$endif}
  RegisterHandler(TDAExceptHandler(HiddenHandlers), @hiddenHandler, sync, epQuickFiltering);
end;

procedure RegisterHiddenExceptionHandler(hiddenHandler: TExceptEventOO; sync: TSyncType);
begin
  {$ifdef log}log('RegisterHiddenExceptionHandlerOO, hiddenHandler: ' + IntToHexEx(int64(TMethod(hiddenHandler))));{$endif}
  RegisterHandlerOO(TDAExceptHandlerOO(HiddenHandlersOO), int64(TMethod(hiddenHandler)), sync, epQuickFiltering);
end;

function UnregisterHiddenExceptionHandler(hiddenHandler: TExceptEvent) : boolean;
begin
  {$ifdef log}log('UnregisterHiddenExceptionHandler, hiddenHandler: ' + IntToHexEx(dword(@hiddenHandler)));{$endif}
  result := UnregisterHandler(TDAExceptHandler(HiddenHandlers), @hiddenHandler);
end;

function UnregisterHiddenExceptionHandler(hiddenHandler: TExceptEventOO) : boolean;
begin
  {$ifdef log}log('UnregisterHiddenExceptionHandlerOO, hiddenHandler: ' + IntToHexEx(int64(TMethod(hiddenHandler))));{$endif}
  result := UnregisterHandlerOO(TDAExceptHandlerOO(HiddenHandlersOO), int64(TMethod(hiddenHandler)));
end;

function CompletePhaseHandlerExists : boolean;
var i1 : integer;
begin
  result := false;
  if HandlerSection <> nil then begin
    EnterCriticalSection(HandlerSection^);
    try
      for i1 := 0 to high(ExceptHandlers) do
        if ExceptHandlers[i1].phase = epCompleteReport then begin
          result := true;
          break;
        end;
      if not result then
        for i1 := 0 to high(ExceptHandlersOO) do
          if ExceptHandlersOO[i1].phase = epCompleteReport then begin
            result := true;
            break;
          end;
    finally LeaveCriticalSection(HandlerSection^) end;
  end;
end;

// ***************************************************************
// madExcept bug report plugins

procedure RegisterBugReportPlugin(name, description: string; proc: TBugReportPlugin; ownSection: boolean); overload;
var i1 : integer;
begin
  i1 := Length(Plugins);
  SetLength(Plugins, i1 + 1);
  Plugins[i1].name        := name;
  Plugins[i1].description := description;
  Plugins[i1].procOld     := proc;
  Plugins[i1].ownSection  := ownSection;
end;

procedure RegisterBugReportPlugin(name, description: string; proc: TBugReportPluginEx; ownSection: boolean); overload;
var i1 : integer;
begin
  i1 := Length(Plugins);
  SetLength(Plugins, i1 + 1);
  Plugins[i1].name        := name;
  Plugins[i1].description := description;
  Plugins[i1].procNew     := proc;
  Plugins[i1].ownSection  := ownSection;
end;

procedure UnregisterBugReportPlugin(name: string);
var i1, i2 : integer;
begin
  for i1 := 0 to high(Plugins) do
    if Plugins[i1].name = name then begin
      for i2 := i1 to high(Plugins) - 1 do
        Plugins[i2] := Plugins[i2 + 1];
      SetLength(Plugins, Length(Plugins) - 1);
      break;
    end;
end;

// ***************************************************************

function CreateBugReport(exceptType        : TExceptType       = etFrozen;
                         exceptObject      : TObject           = nil;
                         exceptAddr        : pointer           = nil;
                         callingThreadId   : dword             = 0;
                         currentEsp        : dword             = 0;
                         currentEbp        : dword             = 0;
                         context           : PContext          = nil;
                         showPleaseWaitBox : boolean           = false;
                         settings          : IMEModuleSettings = nil;
                         source            : TExceptSource     = esManual;
                         relatedObject     : TObject           = nil;
                         package           : dword             = 0;
                         preparedStack     : pointer           = nil     ) : string;
var exc : IMEException;
begin
  if settings = nil then
    settings := MESettingsEx(1);
  exc := NewException(exceptType, exceptObject, exceptAddr, true, callingThreadId,
                      currentEsp, currentEbp, context,
                      settings, source, relatedObject, package, preparedStack);
  exc.ShowPleaseWaitBox := showPleaseWaitBox;
  result := exc.GetBugReport;
end;

function IsUserAdmin : boolean;
const CAdminSia : TSidIdentifierAuthority = (value: (0, 0, 0, 0, 0, 5));
var sid : PSid;
    ctm : function (token: dword; sid: pointer; var isMember: bool) : bool; stdcall;
    b1  : bool;
begin
  result := false;
  ctm := GetProcAddress(LoadLibrary('advapi32.dll'), 'CheckTokenMembership');
  if (@ctm <> nil) and AllocateAndInitializeSid(CAdminSia, 2, $20, $220, 0, 0, 0, 0, 0, 0, sid) then begin
    result := ctm(0, sid, b1) and b1;
    FreeSid(sid);
  end;
end;

function GetTSClientName : string;
const SM_REMOTESESSION = $1000;
var dll : dword;
    buf : pointer;
    len : dword;
    qsi : function (server, session, infoClass: dword; var buf: pointer; var len: dword) : bool; stdcall;
    fm  : procedure (buf: pointer); stdcall;
begin
  result := '';
  try
    if (GetVersion and $80000000 = 0) and (GetSystemMetrics(SM_REMOTESESSION) <> 0) then begin
      dll := LoadLibrary('wtsapi32.dll');
      qsi := GetProcAddress(dll, 'WTSQuerySessionInformationA');
      fm  := GetProcAddress(dll, 'WTSFreeMemory');
      if (@qsi <> nil) and (@fm <> nil) then begin
        if qsi(0, dword(-1), 10, buf, len) and (len > 1) then begin
          SetString(result, pchar(buf), len - 1);
          fm(buf);
        end;
      end;
    end;
  except end;
end;

function TimeToStr(seconds: int64) : string;
var c1, c2 : dword;
    s1, s2 : string;
begin
  result := '';
  if seconds < 0 then
    seconds := 0;
  c1 := seconds div (60*60*24);
  c2 := (seconds div (60*60)) mod 24;
  s1 := 'day';
  s2 := 'hour';
  if c1 = 0 then begin
    c1 := c2;
    c2 := (seconds div 60) mod 60;
    s1 := s2;
    s2 := 'minute';
    if c1 = 0 then begin
      c1 := c2;
      c2 := seconds mod 60;
      s1 := s2;
      s2 := 'second';
    end;
  end;
  case c1 of
    0  : ;
    1  : result := '1'            + ' ' + s1 + ' ';
    else result := IntToStrEx(c1) + ' ' + s1 + 's ';
  end;
  case c2 of
    0  : ;
    1  : result := result + '1'            + ' ' + s2 + ' ';
    else result := result + IntToStrEx(c2) + ' ' + s2 + 's ';
  end;
end;

function GetOsVersionString : string;
begin
  result := OS.description + ' build ' + IntToStrEx(OS.build);
end;

function GetOsLanguageString : string;

(*  function GetKernel32Language : string;
  var lang  : ^word;
      arrCh : array [0..MAX_PATH] of char;
      s1    : string;
      c1    : dword;
  begin
    s1 := ResToStr(GetModuleHandle(kernel32), pchar(RT_VERSION), pchar(1));
    if (s1 <> '') and
       VerQueryValue(pointer(s1), '\VarFileInfo\Translation', pointer(lang), c1) and
       (GetLocaleInfo(lang^, LOCALE_SENGLANGUAGE, arrCh, MAX_PATH) > 0) then
         result := arrCh
    else result := '';
  end; *)

var arrCh : array [0..MAX_PATH] of char;
begin
  if GetLocaleInfo(LOCALE_SYSTEM_DEFAULT, LOCALE_SENGLANGUAGE, arrCh, MAX_PATH) > 0 then
       result := arrCh
  else result := '';
end;

function GetSystemUpTime : string;
var i64, i65 : int64;
begin
  if QueryPerformanceCounter(i64) and QueryPerformanceFrequency(i65) then begin
    i64 := i64 div i65;
    result := TimeToStr(i64);
  end else
    result := '';
end;

function GetProgramUpTime : string;
var i64, i65 : int64;
begin
  if (StartTime <> 0) and QueryPerformanceCounter(i64) and QueryPerformanceFrequency(i65) then begin
    i64 := i64 div (i65 div 1000) - StartTime;
    result := TimeToStr(i64 div 1000);
    if result = '' then
      result := IntToStrEx(i64) + ' milliseconds';
  end;
end;

function GetCpuName : string;
begin
  result := RegReadStr(HKEY_LOCAL_MACHINE, 'hardware\description\system\centralProcessor\0',
                       'ProcessorNameString');
  TrimStr(result);
end;

function GetCpuCount : integer;
var i1 : integer;
begin
  result := 16;
  for i1 := 1 to 15 do
    if RegReadStr(HKEY_LOCAL_MACHINE, 'hardware\description\system\centralProcessor\' + IntToStrEx(i1),
                  'ProcessorNameString') = '' then begin
      result := i1;
      break;
    end;
end;

function GetMemoryStatus : string;
type
  TMemoryStatusEx = record
    dwLength                : dword;
    dwMemoryLoad            : dword;
    ullTotalPhys            : int64;
    ullAvailPhys            : int64;
    ullTotalPageFile        : int64;
    ullAvailPageFile        : int64;
    ullTotalVirtual         : int64;
    ullAvailVirtual         : int64;
    ullAvailExtendedVirtual : int64;
  end;
var gmse : function (var mse: TMemoryStatusEx) : bool; stdcall;
    ms   : TMemoryStatus;
    mse  : TMemoryStatusEx;
begin
  gmse := GetProcAddress(GetModuleHandle(kernel32), 'GlobalMemoryStatusEx');
  if @gmse <> nil then begin
    mse.dwLength := sizeOf(mse);
    gmse(mse);
  end else begin
    ms.dwLength := sizeOf(ms);
    GlobalMemoryStatus(ms);
    mse.ullAvailPhys := ms.dwAvailPhys;
    mse.ullTotalPhys := ms.dwTotalPhys;
  end;
  result := IntToStrEx((mse.ullAvailPhys + $80000) div $100000) + '/' +
            IntToStrEx((mse.ullTotalPhys + $80000) div $100000) +
            ' MB (free/total)';
end;

function Get9xResourceReport : string;
begin
  result := IntToStrEx(dword(GetFreeSystemResources(1)), 2) + '/' +
            IntToStrEx(dword(GetFreeSystemResources(2)), 2) + ' (gdi/user)';
end;

function GetDisplayModeString : string;
var c1 : dword;
begin
  result := IntToStrEx(GetSystemMetrics(SM_CXScreen)) + 'x' + IntToStrEx(GetSystemMetrics(SM_CYScreen));
  c1 := GetDC(0);
  result := result + ', ' + IntToStrEx(GetDeviceCaps(c1, BitsPixel) * GetDeviceCaps(c1, Planes)) + ' bit';
  ReleaseDC(0, c1);
end;

procedure GetProcessMemInfo(var committed, largestFreeBlock: dword);
var currentPos     : pointer;
    mbi            : TMemoryBasicInformation;
    freeBlockBegin : cardinal;
    lastAllocBase  : pointer;
    noModule       : boolean;
    dummy          : dword;
begin
  committed := 0;
  largestFreeBlock := 0;
  currentPos := nil;
  freeBlockBegin := 0;
  lastAllocBase := nil;
  noModule := false;
  while VirtualQuery(currentPos, mbi, sizeOf(mbi)) = sizeOf(mbi) do begin
    if mbi.State = MEM_FREE then
      freeBlockBegin := dword(currentPos)
    else
      if freeBlockBegin > 0 then begin
        if dword(currentPos) - freeBlockBegin > largestFreeBlock then
          largestFreeBlock := dword(currentPos) - freeBlockBegin;
        freeBlockBegin := 0;
      end;
    if mbi.State = MEM_COMMIT then begin
      if mbi.AllocationBase <> lastAllocBase then begin
        lastAllocBase := mbi.AllocationBase;
        noModule := GetModuleFileName(dword(lastAllocBase), @dummy, 3) = 0;
      end;
      if noModule then
        inc(committed, mbi.RegionSize);
    end else
      lastAllocBase := nil;
    dword(currentPos) := dword(currentPos) + mbi.RegionSize;
  end;
  if freeBlockBegin > 0 then
    if dword(currentPos) - freeBlockBegin > largestFreeBlock then
      largestFreeBlock := dword(currentPos) - freeBlockBegin;
end;

function GetAllocatedMemory : string;
var c1, c2 : dword;
begin
  GetProcessMemInfo(c1, c2);
  result := SizeToStr(c1);
end;

// ***************************************************************

var
  // stores all hooks, so we can cleanly unhook at finalization
  PatchList : array of record
    mem      : pointer;
    old, new : string;
  end;

function AddPatch(mem: pointer; var new; len: integer) : boolean;
// hook the memory location and (if successful) add the hook to our list
var s1, s2 : string;
    c1     : dword;
    i1     : integer;
begin
  result := false;
  s1 := '';
  try
    SetString(s1, pchar( mem), len);
    SetString(s2, pchar(@new), len);
    if (Length(s1) = len) and (Length(s2) = len) and (s1 <> s2) and
       VirtualProtect(mem, len, PAGE_EXECUTE_READWRITE, @c1) then begin
      Move(new, mem^, len);
      VirtualProtect(mem, len, c1, @c1);
      i1 := Length(PatchList);
      SetLength(PatchList, i1 + 1);
      PatchList[i1].mem := mem;
      PatchList[i1].old := s1;
      PatchList[i1].new := s2;
      {$ifdef log}log('  Patch ' + IntToHexEx(dword(mem)) + ' succeeded');{$endif}
      result := true;
    end;
  except end;
end;

procedure UndoPatches;
var i1 : integer;
    s1 : string;
    c1 : dword;
begin
  for i1 := 0 to high(PatchList) do
    with PatchList[i1] do
      try
        SetString(s1, pchar(mem), Length(new));
        if (s1 = new) and VirtualProtect(mem, Length(old), PAGE_EXECUTE_READWRITE, @c1) then begin
          Move(pointer(old)^, pointer(mem)^, Length(old));
          VirtualProtect(mem, Length(old), c1, @c1);
        end;
      except end;
end;

procedure RedoPatches;
var i1 : integer;
    s1 : string;
    c1 : dword;
begin
  for i1 := 0 to high(PatchList) do
    with PatchList[i1] do
      try
        SetString(s1, pchar(mem), Length(old));
        if (s1 = old) and VirtualProtect(mem, Length(new), PAGE_EXECUTE_READWRITE, @c1) then begin
          Move(pointer(new)^, pointer(mem)^, Length(new));
          VirtualProtect(mem, Length(new), c1, @c1);
        end;
      except end;
end;

function PatchJmp(aim, new_: pointer) : boolean;
// overwrite the code at address "aim" with a "jmp new" assembler instruction
var jmp : packed record
            opcode   : byte;     // $E9
            distance : integer;  // distance - 5
          end;
begin
  if word(aim^) = $25ff then
    aim := TPPointer(TPPointer(dword(aim) + 2)^)^;
  jmp.opcode   := $E9;
  jmp.distance := integer(new_) - integer(aim) - 5;
  result := AddPatch(aim, jmp, sizeOf(jmp));
end;

{$ifdef bcb}
  function PatchCall(aim, new_: pointer) : boolean;
  // overwrite the code at address "aim" with a "call new" assembler instruction
  var call : packed record
               opcode   : byte;     // $E8
               distance : integer;  // distance - 5
             end;
  begin
    call.opcode   := $E8;
    call.distance := integer(new_) - integer(aim) - 5;
    result := AddPatch(aim, call, sizeOf(call));
  end;
{$endif}

function PatchInt(aim: pointer; new_: integer) : boolean;
// overwrite the address which is stored at address "aim" with the "new" address
begin
  result := AddPatch(aim, new_, 4);
end;

// ***************************************************************

var MadExceptPause : integer = -1;
procedure PauseMadExcept(pause: boolean = true);
begin
  {$ifdef log}log('PauseMadExcept, pause: ' + booleanToChar(pause));{$endif}
  if (MySettings <> nil) and MySettings.Enabled then
    if pause then begin
      if InterlockedIncrement(MadExceptPause) = 0 then begin
        PauseFreezeCheck;
        UndoPatches;
      end;
    end else
      if InterlockedDecrement(MadExceptPause) < 0 then begin
        PauseFreezeCheck(false);
        RedoPatches;
      end;
end;

// ***************************************************************
// exception class implementation

function TIMEException.GetPhase             : TExceptPhase;      begin result := FPhase;            end;
function TIMEException.GetCanContinue       : boolean;           begin result := FCanContinue;      end;
function TIMEException.GetExceptType        : TExceptType;       begin result := FExceptType;       end;
function TIMEException.GetExceptObject      : TObject;           begin result := FExceptObject;     end;
function TIMEException.GetExceptClass       : string;            begin result := GetExceptClass_(FExceptObject) end;
function TIMEException.GetExceptMessage     : string;            begin result := FormatExceptMessage(GetExceptMessage_(FExceptObject)) end;
function TIMEException.GetExceptAddr        : pointer;           begin result := FExceptAddr;       end;
function TIMEException.GetCrashedThreadId   : dword;             begin result := FCrashedThreadId;  end;
function TIMEException.GetSource            : TExceptSource;     begin result := FSource;           end;
function TIMEException.GetRelatedObject     : TObject;           begin result := FRelatedObject;    end;
function TIMEException.GetPackage           : dword;             begin result := FPackage;          end;
function TIMEException.GetBugReport_        : string;            begin result := GetBugReport;      end;
function TIMEException.GetCreateBugReport   : boolean;           begin result := FCreateBugReport;  end;
function TIMEException.GetCreateScreenShot  : boolean;           begin result := FCreateScreenShot; end;
function TIMEException.GetCallHandlers      : boolean;           begin result := FCallHandlers;     end;
function TIMEException.GetAppendScreenShot  : boolean;           begin result := FAppendScreenShot; end;
function TIMEException.GetShowSetting       : TMEShowSetting;    begin result := FShowSetting;      end;
function TIMEException.GetShowAssistant     : string;            begin result := FShowAssis;        end;

function TIMEException.GetCallstackCrc(index: integer) : dword;
begin
  if (index >= 0) and (index <= high(FCallstackCrc)) then
       result := FCallstackCrc[index]
  else result := 0;
end;

function TIMEException.GetContext : PContext;
begin
  if FPContext <> nil then
       result := @FContext
  else result := nil;
end;

procedure TIMEException.SetPhase(value: TExceptPhase);
begin
  if (value = epPostProcessing) and FComplete then
       FPhase := epCompleteReport
  else FPhase := value;
end;

procedure TIMEException.SetCanContinue(value: boolean);
begin
  if not value then
    FCanContinue := false
  else
    if FCanContinue2 then
      FCanContinue := true;
end;

procedure TIMEException.SetCreateBugReport  (value: boolean);        begin FCreateBugReport  := value; end;
procedure TIMEException.SetCreateScreenShot (value: boolean);        begin FCreateScreenShot := value; end;
procedure TIMEException.SetCallHandlers     (value: boolean);        begin FCallHandlers     := value; end;
procedure TIMEException.SetAppendScreenShot (value: boolean);        begin FAppendScreenShot := value; end;
procedure TIMEException.SetShowSetting      (value: TMEShowSetting); begin FShowSetting      := value; end;
procedure TIMEException.SetShowAssistant    (value: string);         begin FShowAssis        := value; end;

var exceptionNumber : int64 = 0;
function TIMEException.GetBugReportHeader : IMEFields;
var GetDiskFreeSpaceEx : function (dir: pchar; var available, diskSize, free: int64) : bool; stdcall;
    itms   : array of record name, content: string end;
    itmCnt : integer;

  function GetDiskStr(drive: char) : string;
  var i64, i65, i66  : int64;
      c1, c2, c3, c4 : dword;
  begin
    if drive <> '\' then begin
      if @GetDiskFreeSpaceEx <> nil then
        GetDiskFreeSpaceEx(pchar(drive + ':\'), i66, i65, i64)
      else
        if GetDiskFreeSpace(pchar(drive + ':\'), c1, c2, c3, c4) then
          i64 := int64(c1) * int64(c2) * int64(c3);
      result := '(' + UpStr(drive) + ':) ' + SizeToStr(i64);
    end;
  end;

  procedure AddItem(name, content: string);
  begin
    if itmCnt = Length(itms) then
      if itmCnt = 0 then
           SetLength(itms, 64)
      else SetLength(itms, itmCnt * 3 div 2);
    itms[itmCnt].name    := name;
    itms[itmCnt].content := content;
    inc(itmCnt);
  end;

var s1, s2 : string;
    arrCh  : array [0..MAX_PATH] of char;
    len    : dword;
    wfd    : TWin32FindData;
    time   : TSystemTime;
    c1     : dword;
    i64    : int64;
    i1, i2 : integer;
    bri    : TIMEFields;
begin
  if FBugReportHeader = nil then begin
    bri := TIMEFields.Create;
    bri.FExcParent := Self;
    FBugReportHeader := bri;
  end;
  result := FBugReportHeader;
  if FCreateBugReport and (FBugReportHeader.ItemCount = 0) then
    with FBugReportHeader do begin
      (FBugReportHeader as IMEFieldsEx).AcceptData;
      itmCnt := 0;
      try
        GetLocalTime(time);
        AddItem('date/time', IntToStrEx(dword(time.wYear        ), 4) + '-'  +
                             IntToStrEx(dword(time.wMonth       ), 2) + '-'  +
                             IntToStrEx(dword(time.wDay         ), 2) + ', ' +
                             IntToStrEx(dword(time.wHour        ), 2) + ':'  +
                             IntToStrEx(dword(time.wMinute      ), 2) + ':'  +
                             IntToStrEx(dword(time.wSecond      ), 2) + ', ' +
                             IntToStrEx(dword(time.wMilliseconds), 1) + 'ms');
      except end;
      try
        AddItem('computer name', CompName);
      except end;
      try
        AddItem('wts client name', GetTSClientName);
      except end;
      try
        len := MAX_PATH;
        GetUserName(arrCh, len);
        s1 := arrCh;
        if IsUserAdmin then
          s1 := s1 + '  <admin>';
        AddItem('user name', s1);
      except end;
      try
        if GetVersion and $80000000 = 0 then
             s1 := ' NT'
        else s1 := '';
        s2 := RegReadStr(HKEY_LOCAL_MACHINE, 'Software\Microsoft\Windows' + s1 + '\CurrentVersion', 'RegisteredOrganization');
        s1 := RegReadStr(HKEY_LOCAL_MACHINE, 'Software\Microsoft\Windows' + s1 + '\CurrentVersion', 'RegisteredOwner');
        if s2 <> '' then
          s1 := s1 + ' / ' + s2;
        AddItem('registered owner', s1);
      except end;
      try
        AddItem('operating system', GetOsVersionString);
      except end;
      try
        AddItem('system language', GetOsLanguageString);
      except end;
      try
        AddItem('system up time', GetSystemUpTime);
      except end;
      try
        AddItem('program up time', GetProgramUpTime);
      except end;
      try
        s1 := GetCpuName;
        if s1 <> '' then begin
          i1 := GetCpuCount;
          if i1 = 1 then
               AddItem('processor',  s1)
          else AddItem('processors', IntToStrEx(i1) + 'x ' + s1);
        end;
      except end;
      try
        AddItem('physical memory', GetMemoryStatus);
      except end;
      if OS.win9x then
        try
          AddItem('system resources', Get9xResourceReport);
        except end;
      try
        GetDiskFreeSpaceEx := GetProcAddress(GetModuleHandle(kernel32), 'GetDiskFreeSpaceExA');
        GetWindowsDirectory(arrCh, MAX_PATH);
        s1 := ModuleName(0);
        if UpChar(s1[1]) <> UpChar(arrCh[0]) then
             AddItem('free disk space', GetDiskStr(arrCh[0]) + ' ' + GetDiskStr(s1[1]))
        else AddItem('free disk space', GetDiskStr(arrCh[0]));
      except end;
      try
        AddItem('display mode', GetDisplayModeString);
      except end;
      try
        AddItem('process id', IntToHexEx(GetCurrentProcessId));
      except end;
      try
        AddItem('allocated memory', GetAllocatedMemory);
      except end;
      try
        if AddCmdLineToBugRep and (ParamCount > 0) then
          AddItem('command line', GetCommandLine);
      except end;
      try
        AddItem('executable', ExtractFileName(ModuleName(0)));
      except end;
      try
        if HInstance <> GetModuleHandle(nil) then
          AddItem('current module', ExtractFileName(ModuleName(HInstance)));
      except end;
      try
        if AmMeBpl then
             s1 := ModuleName(0)
        else s1 := ModuleName(HInstance);
        c1 := FindFirstFile(pchar(s1), wfd);
        if c1 <> INVALID_HANDLE_VALUE then begin
          windows.FindClose(c1);
          try
            FileTimeToLocalFileTime(wfd.ftLastWriteTime, wfd.ftLastWriteTime);
            FileTimeToSystemTime(wfd.ftLastWriteTime, time);
            if AmMeBpl or (HInstance = GetModuleHandle(nil)) then
                 s2 := 'exec.'
            else s2 := 'module';
            AddItem(s2 + ' date/time', IntToStrEx(dword(time.wYear  ), 4) + '-' +
                                       IntToStrEx(dword(time.wMonth ), 2) + '-' +
                                       IntToStrEx(dword(time.wDay   ), 2) + ' ' +
                                       IntToStrEx(dword(time.wHour  ), 2) + ':' +
                                       IntToStrEx(dword(time.wMinute), 2) + #$D#$A);
          except end;
          if VersionVar = '' then begin
            i64 := GetFileVersion(s1);
            if i64 <> 0 then
              AddItem('version', FileVersionToStr(i64));
          end else
            AddItem('version', VersionVar);
        end;
      except end;
      try
        c1 := GetModuleHandle('idapi32.dll');
        if (c1 <> 0) and (GetModuleFileName(c1, arrCh, MAX_PATH) > 0) then begin
          i64 := GetFileVersion(arrCh);
          if i64 <> 0 then
            AddItem('bde version', FileVersionToStr(i64));
        end;
      except end;
      try
        AddItem('madExcept version', CMadExceptVersion);
      except end;
      s2 := GetPropStr('Plugins');
      for i1 := 1 to SubStrCount(s2) do
        try
          s1 := SubStr(s2, i1);
          for i2 := 0 to high(Plugins) do
            if IsTextEqual(Plugins[i2].name, s1) then begin
              if not Plugins[i2].ownSection then
                if @Plugins[i2].procOld <> nil then
                     AddItem(Plugins[i2].name, Plugins[i2].procOld      )
                else AddItem(Plugins[i2].name, Plugins[i2].procNew(self));
              break;
            end;
        except end;
      i2 := itmCnt - 1;
      try
        AddItem('callstack crc', 'will be calculated soon');
      except end;
      try
        inc(exceptionNumber);
        AddItem('exception number', IntToStrEx(exceptionNumber));
      except end;
      try
        if FExceptType <> etFrozen then begin
          s1 := GetExceptMessage_(FExceptObject);
          s2 := GetExceptClass;
          AddItem('exception class', s2);
          if s1 <> s2 then
            AddItem('exception message', FormatExceptMessage(s1));
        end else
          AddItem('exception message', GetFrozenMsg);
      except end;
      Lock;
      try
        for i1 := 0 to itmCnt - 1 do
          Contents[itms[i1].name] := itms[i1].content;
        (FBugReportHeader as IMEFieldsEx).SetAddIndex(i2);
      except end;
      Unlock;
    end;
end;

function TIMEException.CompleteBugReport : integer; stdcall;
var count : integer;
    mdis  : TDAString;

  procedure UpdateSection(oldSection, newSection, content: string);
  var i1 : integer;
      s1 : string;
  begin
    if not FAbortCompleteThread then begin
      BeginUpdate;
      FBugReportSections[oldSection] := content;
      if (content <> '') and (newSection <> '') then begin
        i1 := FBugReportSections.FindItem(oldSection);
        FBugReportSections.Items[i1] := newSection;
      end;
      dec(count);
      if count = 0 then begin
        FBugReportHeader['callstack crc'] := IntToHexEx(FCallstackCrc[0], 8) + ', ' +
                                             IntToHexEx(FCallstackCrc[1], 8) + ', ' +
                                             IntToHexEx(FCallstackCrc[2], 8);
        for i1 := 0 to high(mdis) do begin
          s1 := SubStr(mdis[i1], 1);
          FBugReportHeader[s1 + '.mad'] := SubStr(mdis[i1], 2);
        end;
        FComplete := true;
      end;
      EndUpdate;
    end;
  end;

  function GetThreadReport(index: integer; var crc1, crc2: dword; var delphiThread: boolean;
                           ea: TPPointer = nil; ef: TPPointer = nil; efa: TPPointer = nil) : string;
  var i1 : integer;
  begin
    try
      with FThreadStacks[index] do begin
        crc1 := 0;
        crc2 := 0;
        delphiThread := false;
        result := StackTrace(GetHideUglyItems, GetShowRelativeAddrs, GetShowRelativeLines, nil,
                             exceptAddr, exceptThread, extException, stackBottom, stackTop,
                             creatorAddr, ea, ef, efa, FProgressAlert, ebp,
                             DumbStackTrace, bcbTermination, preparedStackP, @FAbortCompleteThread,
                             @crc1, @crc2, @delphiThread, @mdis);
        FProgressAlert.AreaDone;
        preparedStackP := nil;
        preparedStackDA := nil;
        if (creatorAddr <> nil) and (not FAbortCompleteThread) and (result <> '') and (result[1] <> '>') then
          for i1 := Length(result) downto 1 do
            if result[i1] = #$D then begin
              Insert(#$D#$A + '>> created by ' +
                     GetThreadInfoStr(creatorTid, true, creatorAddr, creatorTid) + ' at:',
                     result, i1);
              break;
            end;
      end;
    except result := InternalError('GetThreadReport', GetShowRelativeAddrs, GetShowRelativeLines) end;
  end;

  function GetOdbcDriverList : string;
  var key1   : HKEY;
      s1, s2 : string;
      c1, c2 : dword;
      p1     : pchar;
  begin
    result := '';
    try
      if RegOpenKeyEx(HKEY_LOCAL_MACHINE, 'Software\ODBC\ODBCINST.INI', 0, KEY_READ, key1) = 0 then begin
        if RegQueryInfoKey(key1, nil, nil, nil, @c1, @c2, nil, nil, nil, nil, nil, nil) = 0 then begin
          c2 := c2 * 2;
          p1 := pointer(LocalAlloc(LPTR, c2 * 2));
          for c1 := c1 - 1 downto 0 do
            if RegEnumKey(key1, c1, p1, c2) = 0 then begin
              s1 := RegReadStr(key1, p1, 'Driver');
              s2 := RegReadStr(key1, p1, 'DriverODBCVer');
              if (s1 <> '') and (s2 <> '') then
                result := result + #$D#$A + '- ' + FillStr(ExtractFileName(s1), 12) + ' ' + s2 + ' ' + p1;
            end else
              break;
          LocalFree(dword(p1));
        end;
        RegCloseKey(key1);
      end;
    except result := InternalError('GetOdbcDriverList', GetShowRelativeAddrs, GetShowRelativeLines) end;
  end;

  function GetCpuRegisters(const context: TContext) : string;
  begin
    try
      result := 'eax = ' + RetDelete(IntToHexEx(context.Eax, 8), 1, 1) + #$D#$A +
                'ebx = ' + RetDelete(IntToHexEx(context.Ebx, 8), 1, 1) + #$D#$A +
                'ecx = ' + RetDelete(IntToHexEx(context.Ecx, 8), 1, 1) + #$D#$A +
                'edx = ' + RetDelete(IntToHexEx(context.Edx, 8), 1, 1) + #$D#$A +
                'esi = ' + RetDelete(IntToHexEx(context.Esi, 8), 1, 1) + #$D#$A +
                'edi = ' + RetDelete(IntToHexEx(context.Edi, 8), 1, 1) + #$D#$A +
                'eip = ' + RetDelete(IntToHexEx(context.Eip, 8), 1, 1) + #$D#$A +
                'esp = ' + RetDelete(IntToHexEx(context.Esp, 8), 1, 1) + #$D#$A +
                'ebp = ' + RetDelete(IntToHexEx(context.Ebp, 8), 1, 1);
    except result := InternalError('GetCpuRegisters', GetShowRelativeAddrs, GetShowRelativeLines) end;
  end;

  function GetStackDump(const context: TContext) : string;
  var esp      : TPAByte;
      i1, i2   : integer;
      mbi      : TMemoryBasicInformation;
      stackTop : dword;
  begin
    try
      result := '';
      dword(esp) := context.Esp;
      if VirtualQuery(esp, mbi, sizeOf(mbi)) = sizeOf(mbi) then
           stackTop := dword(mbi.BaseAddress) + mbi.RegionSize
      else stackTop := $ffffffff;
      for i1 := 0 to 19 do begin
        result := result + #$D#$A + RetDelete(IntToHexEx(dword(esp), 8), 1, 1) + '  ';
        for i2 := 0 to 15 do
          if dword(@esp[i2]) >= stackTop then begin
            if i2 = 8 then
              result := result + '  ';
            result := result + '   ';
          end else begin
            if i2 = 8 then
              result := result + '- ';
            result := result + RetDelete(IntToHexEx(integer(esp[i2]), 2), 1, 1) + ' ';
          end;
        result := result + ' ';
        for i2 := 0 to 15 do begin
          if dword(@esp[i2]) >= stackTop then
            break;
          if esp[i2] in [$21..$7e] then
               result := result + chr(esp[i2])
          else result := result + '.';
        end;
        inc(dword(esp), 16);
        if dword(esp) >= stackTop then
          break;
      end;
      Delete(result, 1, 2);
    except result := InternalError('GetStackDump', GetShowRelativeAddrs, GetShowRelativeLines) end;
  end;

  function GetCleartextDisAsm(func, exceptAddr: pointer) : string;
  begin
    try
      if func <> nil then
           DisAsmFunc(func, result, exceptAddr, GetLimitDisassembly, GetFormatDisassembly)
      else result := '';
    except result := InternalError('GetCleartextDisAsm', GetShowRelativeAddrs, GetShowRelativeLines) end;
  end;

var i1, i2  : integer;
    ef, efa : pointer;
    b1      : boolean;
    exts    : string;
    s1      : string;
    c1, c2  : dword;
begin
  result := 0;
  mdis := nil;
  try
    for i1 := 0 to high(FThreadStacks) do
      with FThreadStacks[i1] do
        if preparedStackP = nil then begin
          i2 := PrepareStackTrace(ebp, stackTop, stackBottom, exceptAddr, exceptThread, preparedStackDA);
          if i2 > 0 then begin
            integer(preparedStackDA[0].afterInstr) := i2;
            preparedStackP := @preparedStackDA[0].afterInstr;
          end;
        end;
    count := Length(FThreadStacks) +
             ord(GetShowCpuRegisters and (FPContext <> nil)) +
             ord(GetShowStackDump and (FPContext <> nil)) +
             ord(GetShowDisAsm and (@DisAsmFunc <> nil)) (*+
             ord(GetListOdbcDrivers)*);
    exts := GetPropStr('Plugins');
    for i1 := 1 to SubStrCount(exts) do begin
      s1 := SubStr(exts, i1);
      for i2 := 0 to high(Plugins) do
        if IsTextEqual(Plugins[i2].name, s1) then begin
          if Plugins[i2].ownSection then
            inc(count);
          break;
        end;
    end;
    if FAbortCompleteThread then exit;
    ef := nil;
    if FThreadStacks <> nil then begin
      s1 := GetThreadReport(0, FCallstackCrc[0], FCallstackCrc[1], b1, @FExceptAddr, @ef, @efa);
      FCallstackCrc[2] := FCallstackCrc[1];
      UpdateSection(FThreadStacks[0].threadName, '', s1);
    end;
    if FAbortCompleteThread then exit;
    if GetShowCpuRegisters and (FPContext <> nil) then
      UpdateSection('cpu registers', '', GetCpuRegisters(FContext));
    if GetShowStackDump and (FPContext <> nil) then
      UpdateSection('stack dump', '', GetStackDump(FContext));
    if GetShowDisAsm and (@DisAsmFunc <> nil) then
      UpdateSection('disassembling', '', GetCleartextDisAsm(ef, efa));  
    if FAbortCompleteThread then exit;
//    if FAbortCompleteThread then exit;
//    if ListOdbcDrivers then
//      UpdateSection('odbc drivers', '', GetOdbcDriverList);
    for i1 := 1 to SubStrCount(exts) do
      try
        if FAbortCompleteThread then exit;
        s1 := SubStr(exts, i1);
        for i2 := 0 to high(Plugins) do
          if IsTextEqual(Plugins[i2].name, s1) then begin
            if Plugins[i2].ownSection then begin
              try
                if @Plugins[i2].procOld <> nil then
                     s1 := Plugins[i2].procOld
                else s1 := Plugins[i2].procNew(self);
              except s1 := InternalError('plugin', GetShowRelativeAddrs, GetShowRelativeLines) end;
              UpdateSection(Plugins[i2].name, '', s1);
            end;
            break;
          end;
      except end;
    for i1 := 1 to high(FThreadStacks) do begin
      if FAbortCompleteThread then exit;
      s1 := GetThreadReport(i1, c1, c2, b1);
      if b1 then
        FCallstackCrc[2] := UpdateCrc32(FCallstackCrc[2], c2, 4);
      UpdateSection(FThreadStacks[i1].threadName, '', s1);
    end;
    if FAbortCompleteThread then exit;
  finally FProgressAlert.Close end;
  if (FRefCount > 0) and (FPhase = epPostProcessing) then begin
    FPhase := epCompleteReport;
    b1 := false;
    DoFireHandlers(0, 0, eaSendBugReport, self, b1);
  end;
end;

function TIMEException.GetCompleteThreadId : dword;
begin
  result := FCompleteTid;
end;

function TIMEException.GetBugReportSections : IMEFields;
var itms   : array of record name, content: string end;
    itmCnt : integer;

  procedure AddItem(name, content: string);
  begin
    if itmCnt = Length(itms) then
      if itmCnt = 0 then
           SetLength(itms, 64)
      else SetLength(itms, itmCnt * 3 div 2);
    itms[itmCnt].name    := name;
    itms[itmCnt].content := content;
    inc(itmCnt);
  end;

  procedure PrepareThreadReport;

    // do a stack trace of the specified thread and return the result as a string
    procedure GetThreadStack(tid: dword; extException, bcbTermination, exceptThread, forceShow: boolean;
                             exceptAddr: pointer; pa: IProgressAlert; preparedStack: pointer);

      // collect some important informations about the specified thread
      function GetThreadInfos(tid: dword; var stackBottom, stackTop, eip, ebp: dword;
                              var priority: integer; var suspended: boolean) : boolean;

        function IsThreadSuspended(tid, th: dword) : boolean;
        var i1 : integer;
        begin
          result := false;
          if tid <> GetCurrentThreadId then begin
            i1 := integer(SuspendThread(th));
            if i1 <> -1 then begin
              ResumeThread(th);
              result := i1 > 0;
            end;
          end;
        end;

      var th   : dword;
          ctxt : TContext;
          mi   : TMemoryBasicInformation;
      begin
        result := false;
        if tid <> GetCurrentThreadId then begin
          th := OpenThread(tid, $4a);  // THREAD_GET_CONTEXT = 8; THREAD_SUSPEND_RESUME = 2; THREAD_QUERY_INFORMATION = $40;
          if th = 0 then
            th := OpenThread(tid, $8);
        end else
          if not DuplicateHandle(GetCurrentProcess, GetCurrentThread, GetCurrentProcess, @th, 0, false, DUPLICATE_SAME_ACCESS) then
            th := 0;
        if th <> 0 then begin
          ctxt.ContextFlags := CONTEXT_FULL;
          if GetThreadContext(th, ctxt) and (VirtualQuery(pointer(ctxt.esp), mi, sizeOf(mi)) = sizeOf(mi)) then begin
            stackBottom := ctxt.Esp;
            stackTop    := dword(mi.BaseAddress) + mi.RegionSize;
            eip         := ctxt.Eip;
            ebp         := ctxt.Ebp;
            result      := true;
          end;
          priority := GetThreadPriority(th);
          if priority = maxInt then
            priority := 0;
          suspended := IsThreadSuspended(tid, th);
          CloseHandle(th);
        end;
      end;

    var s1, s2           : string;
        creatorAddr      : pointer;
        creatorTid       : dword;
        i1               : integer;
        sb, st, eip, ebp : dword;
        b1               : boolean;
        preparedStackDA  : TDAPreStackItem;
    begin
      preparedStackDA := nil;
      try
        s1 := GetThreadInfoStr(tid, forceShow, creatorAddr, creatorTid);
      except s1 := '' end;
      if s1 <> '' then begin
        s1 := s1 + ':';
        try
          s2 := '>> stack not accessible';
          if exceptThread then
            s2 := s2 + ', exception location: ' + #$D#$A +
                  StackAddrToStr(exceptAddr, GetShowRelativeAddrs, GetShowRelativeLines);
          if GetThreadInfos(tid, sb, st, eip, ebp, i1, b1) then begin
            if b1 then
              s1 := s1 + ' <suspended>';
            if i1 <> 0 then
              s1 := s1 + ' <priority:' + IntToStrEx(i1) + '>';
            if not exceptThread then
              exceptAddr := pointer(eip)
            else begin
              if FCurrentEsp <> 0 then
                sb := FCurrentEsp;
              if FCurrentEbp <> 0 then
                ebp := FCurrentEbp;
            end;
            FProgressAlert.AddArea((st - sb) * 100, GetPleaseWaitText);
            i1 := Length(FThreadStacks);
            SetLength(FThreadStacks, i1 + 1);
            FThreadStacks[i1].threadName        := s1;
            FThreadStacks[i1].exceptAddr        := exceptAddr;
            FThreadStacks[i1].exceptThread      := exceptThread;
            FThreadStacks[i1].extException      := extException;
            FThreadStacks[i1].stackBottom       := sb;
            FThreadStacks[i1].stackTop          := st;
            FThreadStacks[i1].creatorAddr       := creatorAddr;
            FThreadStacks[i1].ebp               := ebp;
            FThreadStacks[i1].bcbTermination    := bcbTermination;
            FThreadStacks[i1].preparedStackP    := preparedStack;
            FThreadStacks[i1].preparedStackDA   := preparedStackDA;
            FThreadStacks[i1].creatorTid        := creatorTid;
            s2 := StackAddrToStr(exceptAddr, GetShowRelativeAddrs, GetShowRelativeLines) + #$D#$A + '>> stack ';
            s2 := s2 + 'will be calculated soon';
            AddItem(s1, s2);
          end else
            AddItem(s1, s2);
        except
          AddItem(s1, InternalError('GetThreadStack', GetShowRelativeAddrs, GetShowRelativeLines));
        end;
      end;
    end;

  var tl : TDACardinal;
      i1 : integer;
  begin
    tl := nil;
    try
      if GetListThreads then
        tl := GetThreadList;
      if FExceptType <> etFrozen then begin
        GetThreadStack(FCrashedThreadId, IsClass(FExceptObject, 'EExternal'),
                       IsClass(FExceptObject, 'BcbTermination'),
                       true, true, FExceptAddr, nil, FPreparedStack);
        if FCrashedThreadId <> ProcessMainThreadId then
          GetThreadStack(ProcessMainThreadId, false, false, false, false, nil, nil, nil);
      end else
        GetThreadStack(ProcessMainThreadId, false, false, false, false, nil, nil, nil);
      if GetListThreads then
        for i1 := 0 to high(tl) do
          if ((FExceptType = etFrozen) or (tl[i1] <> FCrashedThreadId)) and (tl[i1] <> ProcessMainThreadId) and
             (tl[i1] <> AntiFreezeTid) and (tl[i1] <> HandleExceptionTid) then
            GetThreadStack(tl[i1], false, false, false, false, nil, nil, nil);
    except end;
  end;

var bri    : TIMEFields;
    s1     : string;
    exts   : string;
    i1, i2 : integer;
begin
  if FBugReportSections = nil then begin
    bri := TIMEFields.Create;
    bri.FExcParent := Self;
    FBugReportSections := bri;
  end;
  result := FBugReportSections;
  if FCreateBugReport and (FBugReportSections.ItemCount = 0) then
    with FBugReportSections do begin
      (FBugReportSections as IMEFieldsEx).AcceptData;
      itmCnt := 0;
      PrepareThreadReport;
//      if ListOdbcDrivers then
//        AddItem('odbc drivers', '>> will be calculated soon');
      exts := GetPropStr('Plugins');
      for i1 := 1 to SubStrCount(exts) do
        try
          s1 := SubStr(exts, i1);
          for i2 := 0 to high(Plugins) do
            if IsTextEqual(Plugins[i2].name, s1) then begin
              if Plugins[i2].ownSection then
                AddItem(Plugins[i2].name, '>> will be calculated soon');
              break;
            end;
        except end;
      if GetShowCpuRegisters and (FPContext <> nil) then
        AddItem('cpu registers', '>> will be calculated soon');
      if GetShowStackDump and (FPContext <> nil) then
        AddItem('stack dump', '>> will be calculated soon');
      if GetShowDisAsm and (@DisAsmFunc <> nil) then
        AddItem('disassembling', '>> will be calculated soon');
      Lock;
      try
        for i1 := 0 to itmCnt - 1 do
          Contents[itms[i1].name] := itms[i1].content;
      except end;
      Unlock;
      if (GetCurrentThreadID = HandleExceptionTid) and (not IsProcessBlocked(250)) then
        FCompleteTh := CreateThread(nil, 0, @TIMEException.CompleteBugReport, Self, 0, FCompleteTid);
      if FCompleteTh = 0 then begin
        if GetShowPleaseWaitBox then
          FProgressAlert.Show;
        CompleteBugReport;
      end else
        SetThreadPriority(FCompleteTh, THREAD_PRIORITY_ABOVE_NORMAL);
    end;
end;

procedure TIMEException.SetBugReport(value: string);
begin
  FBugReport := value;
  FireChangeEvent;
end;

function TIMEException.GetBugReport(mustBeComplete: boolean = true) : string;
var i1, i2, i3 : integer;
    s1, s2, s3 : string;
    currentNo  : int64;
begin
  EnterCriticalSection(FSection);
  try
    currentNo := FCorrectBugReportNo;
    result := FBugReport;
  finally LeaveCriticalSection(FSection) end;
  if FCreateBugReport and ((result = '') or (mustBeComplete and (not FComplete))) then begin
    GetBugReportHeader;
    GetBugReportSections;
    if mustBeComplete and (FCompleteTh <> 0) and (not IsProcessBlocked(250)) then 
      if GetShowPleaseWaitBox and FProgressAlert.Show then begin
        SetThreadPriority(FCompleteTh, THREAD_PRIORITY_HIGHEST);
        while WaitForSingleObject(FCompleteTh, 50) = WAIT_TIMEOUT do
          FProgressAlert.ProcessMessages;
        FProgressAlert.ProcessMessages;
      end else
        WaitForSingleObject(FCompleteTh, INFINITE);
    with GetBugReportHeader do begin
      Lock;
      try
        i2 := 0;
        for i1 := 0 to ItemCount - 1 do begin
          i3 := Length(Items[i1]);
          if i3 > i2 then
            i2 := i3;
        end;
        inc(i2);
        for i1 := 0 to ItemCount - 1 do begin
          s1 := Items[i1];
          if i1 > 0 then
            result := result + #$D#$A;
          result := result + FillStr(s1, -i2) + ': ';
          s2 := Contents[s1];
          ReplaceStr(s2, #$D#$A, ' ');
          ReplaceStr(s2, #$D, ' ');
          ReplaceStr(s2, #$A, ' ');
          ReplaceStr(s2, '  ', ' ', true);
          result := result + s2;
        end;
      finally Unlock end;
    end;
    with GetBugReportSections do begin
      Lock;
      try
        for i1 := 0 to ItemCount - 1 do begin
          s1 := Items[i1];
          if PosStr(':', s1) = 0 then
               s2 := s1 + ':'
          else s2 := s1;
          result := result + #$D#$A + #$D#$A + s2;
          s2 := Contents[s1];
          i2 := 1;
          repeat
            i3 := PosChars([#$D, #$A], s2, i2);
            if i3 > 0 then begin
              s3 := Copy(s2, i2, i3 - i2);
              if (s2[i3] = #$D) and (i3 < Length(s2)) and (s2[i3 + 1] = #$A) then
                inc(i3);
              i2 := i3 + 1;
            end else
              s3 := Copy(s2, i2, maxInt);
            s3 := '.' + s3;
            TrimStr(s3);
            system.Delete(s3, 1, 1);
            if s3 = '' then
              s3 := '.';
            result := result + #$D#$A + s3;
          until i3 = 0;
        end;
      finally Unlock end;
    end;
    EnterCriticalSection(FSection);
    try
      if currentNo = FCorrectBugReportNo then
        FBugReport := result;
    finally LeaveCriticalSection(FSection) end;
  end;
end;

function TIMEException.GetScreenShot : INVBitmap;
begin
  if (FScreenShot = nil) and FCreateScreenShot then begin
    FScreenShot := ScreenShot(GetScreenShotAppOnly);
    if FScreenShot = nil then
      FCreateScreenShot := false;
  end;
  result := FScreenShot;
end;

procedure TIMEException.SetScreenShot(value: INVBitmap);
begin
  FScreenShot := value;
end;

procedure TIMEException.BeginUpdate;
begin
  InterlockedIncrement(FUpdating);
end;

procedure TIMEException.EndUpdate;
begin
  if (InterlockedDecrement(FUpdating) = 0) and FDirty then
    SetModified;
end;

procedure TIMEException.RegisterBugReportCallback(bugReportCallback: TBugReportCallback; critical: boolean);
var i1, i2 : integer;
    b1     : boolean;
begin
  if HandlerSection = nil then begin
    New(HandlerSection);
    InitializeCriticalSection(HandlerSection^);
  end;
  EnterCriticalSection(HandlerSection^);
  try
    b1 := true;
    i2 := Length(FBugReportCallbacks);
    for i1 := 0 to i2 - 1 do
      if @FBugReportCallbacks[i1].callback = @bugReportCallback then begin
        FBugReportCallbacks[i1].critical := critical;
        b1 := false;
        break;
      end;
    if b1 then begin
      SetLength(FBugReportCallbacks, i2 + 1);
      FBugReportCallbacks[i2].callback := bugReportCallback;
      FBugReportCallbacks[i2].critical := critical;
    end;
  finally LeaveCriticalSection(HandlerSection^) end;
  if b1 then
    bugReportCallback(FComplete, GetBugReport(false), self);
end;

procedure TIMEException.RegisterBugReportCallback(bugReportCallback: TBugReportCallbackOO; critical: boolean);
var i1, i2 : integer;
    b1     : boolean;
begin
  if HandlerSection = nil then begin
    New(HandlerSection);
    InitializeCriticalSection(HandlerSection^);
  end;
  EnterCriticalSection(HandlerSection^);
  try
    b1 := true;
    i2 := Length(FBugReportCallbacksOO);
    for i1 := 0 to i2 - 1 do
      if int64(TMethod(FBugReportCallbacksOO[i1].callback)) = int64(TMethod(bugReportCallback)) then begin
        FBugReportCallbacksOO[i1].critical := critical;
        b1 := false;
        break;
      end;
    if b1 then begin
      SetLength(FBugReportCallbacksOO, i2 + 1);
      FBugReportCallbacksOO[i2].callback := bugReportCallback;
      FBugReportCallbacksOO[i2].critical := critical;
    end;
  finally LeaveCriticalSection(HandlerSection^) end;
  if b1 then
    bugReportCallback(FComplete, GetBugReport(false), self);
end;

procedure TIMEException.UnregisterBugReportCallback(bugReportCallback: TBugReportCallback);
var i1 : integer;
begin
  if HandlerSection <> nil then begin
    EnterCriticalSection(HandlerSection^);
    try
      for i1 := 0 to high(FBugReportCallbacks) do
        if @FBugReportCallbacks[i1].callback = @bugReportCallback then begin
          Move(TPAPointer(FBugReportCallbacks)^[i1 + 1], TPAPointer(FBugReportCallbacks)[i1], (high(FBugReportCallbacks) - i1) * sizeOf(FBugReportCallbacks[0]));
          SetLength(FBugReportCallbacks, Length(FBugReportCallbacks) - 1);
          break;
        end;
    finally LeaveCriticalSection(HandlerSection^) end;
  end;
end;

procedure TIMEException.UnregisterBugReportCallback(bugReportCallback: TBugReportCallbackOO);
var i1 : integer;
begin
  if HandlerSection <> nil then begin
    EnterCriticalSection(HandlerSection^);
    try
      for i1 := 0 to high(FBugReportCallbacksOO) do
        if int64(TMethod(FBugReportCallbacksOO[i1].callback)) = int64(TMethod(bugReportCallback)) then begin
          Move(TPAInt64(FBugReportCallbacksOO)^[i1 + 1], TPAInt64(FBugReportCallbacksOO)[i1], (high(FBugReportCallbacksOO) - i1) * sizeOf(FBugReportCallbacksOO[0]));
          SetLength(FBugReportCallbacksOO, Length(FBugReportCallbacksOO) - 1);
          break;
        end;
    finally LeaveCriticalSection(HandlerSection^) end;
  end;
end;

function TIMEException.CriticalBugReportCallbackExists : boolean;
var i1 : integer;
begin
  result := false;
  if HandlerSection <> nil then begin
    EnterCriticalSection(HandlerSection^);
    try
      for i1 := 0 to high(FBugReportCallbacks) do
        if FBugReportCallbacks[i1].critical then begin
          result := true;
          break;
        end;
      if not result then
        for i1 := 0 to high(FBugReportCallbacksOO) do
          if FBugReportCallbacksOO[i1].critical then begin
            result := true;
            break;
          end;
    finally LeaveCriticalSection(HandlerSection^) end;
  end;
end;

procedure TIMEException.SetModified;
begin
  if FUpdating = 0 then begin
    FDirty := false;
    EnterCriticalSection(FSection);
    try
      inc(FCorrectBugReportNo);
      FBugReport := '';
    finally LeaveCriticalSection(FSection) end;
    FireChangeEvent;
  end else
    FDirty := true;
end;

procedure TIMEException.FireChangeEvent;
var i1    : integer;
    brc   : TDABugReportCallback;
    brcOO : TDABugReportCallbackOO;
    br    : string;
begin
  brc := nil;
  brcOO := nil;
  if HandlerSection <> nil then begin
    EnterCriticalSection(HandlerSection^);
    try
      brc   := FBugReportCallbacks;
      brcOO := FBugReportCallbacksOO;
    finally LeaveCriticalSection(HandlerSection^) end;
  end;
  if (brc <> nil) or (brcOO <> nil) then begin
    br := GetBugReport(false);
    for i1 := 0 to high(brc) do
      brc[i1].callback(FComplete, br, self);
    for i1 := 0 to high(brcOO) do
      brcOO[i1].callback(FComplete, br, self);
  end;
end;

procedure TIMEException.Show;
begin
  ShowException(Self);
end;

function TIMEException.FindProp(name: string; allowDefault, write: boolean; createTyp: TPropertyType) : TPMEProperty;
// "FindProp" was originally implemented in TIMESettings (parent class of ours)
// however, we need to override this method to realize custom settings for exceptions
var i1 : integer;
begin
  // read access:
  // search through custom settings stored for this exception only
  // the list of custom settings is usually empty
  // it gets filled only if you call "IMEException.SomeSetting := SomeValue"
  // write access:
  // the settings is stored to the private settings list of our exception
  result := inherited FindProp(name, false, write, createTyp);
  if (result = nil) and (not write) then
    // the setting is not specifically set for this exception
    // so we search the global settings, where we should usually find it
    with (FModuleSettings as IMESettingsEx).GetSelf do
      if FSettings <> nil then begin
        i1 := FindMEProperty(FSettings.propCount, FSettings.props, name);
        if i1 <> -1 then
          result := @FSettings.props[i1];
      end;
  if (result = nil) and (not write) then
    // we still didn't find the wanted setting
    // so we search through the default settings this time
    result := inherited FindProp(name, true, false, createTyp);
end;

function TIMEException.GetMailWasSent : boolean;
begin
  result := FMailWasSent;
end;

procedure TIMEException.SetMailWasSent(value: boolean);
begin
  FMailWasSent := value;
end;

function TIMEException.GetMailWasSaved : boolean;
begin
  result := FMailWasSaved;
end;

procedure TIMEException.SetMailWasSaved(value: boolean);
begin
  FMailWasSaved := value;
end;

function TIMEException.FirstCompleteCallback : boolean;
begin
  result := InterlockedIncrement(FFirstCompleteCallback) = 0;
end;

function TIMEException.FirstAutoSave : boolean;
begin
  result := InterlockedIncrement(FFirstAutoSave) = 0;
end;

function TIMEException.PSendThread : TPCardinal;
begin
  result := @FSendThread;
end;

constructor TIMEException.Create(module: dword);
begin
  inherited Create(module, false);
  FValid := true;
  FCanContinue := true;
  FCanContinue2 := true;
  FModule := module;
  FFirstCompleteCallback := -1;
  FFirstAutoSave := -1;
  InitializeCriticalSection(FSection);
end;

destructor TIMEException.Destroy;
begin
  FAbortCompleteThread := true;
  if FCompleteTh <> 0 then begin
    if IsProcessBlocked(250) then
         TerminateThread(FCompleteTh, 0)
    else WaitForSingleObject(FCompleteTh, INFINITE);
    CloseHandle(FCompleteTh);
    FCompleteTh  := 0;
    FCompleteTid := 0;
  end;
  if FBugReportHeader <> nil then
    (FBugReportHeader as IMEFieldsEx).ClearExcParent;
  if FBugReportSections <> nil then
    (FBugReportSections as IMEFieldsEx).ClearExcParent;
  DeleteCriticalSection(FSection);
  FSettings := nil;
  FreeProps(FSettingsBuf);
  if FSendThread <> 0 then begin
    CloseHandle(FSendThread);
    FSendThread := 0;
  end;
  inherited;
end;

function NewException(exceptType      : TExceptType       = etFrozen;
                      exceptObject    : TObject           = nil;
                      exceptAddr      : pointer           = nil;
                      canContinue     : boolean           = true;
                      crashedThreadId : dword             = 0;
                      currentEsp      : dword             = 0;
                      currentEbp      : dword             = 0;
                      context         : PContext          = nil;
                      settings        : IMEModuleSettings = nil;
                      source          : TExceptSource     = esManual;
                      relatedObject   : TObject           = nil;
                      package         : dword             = 0;
                      preparedStack   : pointer           = nil     ) : IMEException;

  function StrLen(str: pchar) : integer;
  begin
    result := 0;
    if str <> nil then
      while str[result] <> #0 do
        inc(result);
  end;

  procedure CheckFilters(Self: TIMEException;
                         const classes      : array of string;
                         const noBugReport  : array of boolean;
                         const noScreenShot : array of boolean;
                         const noSuspend    : array of boolean;
                         const noHandlers   : array of boolean;
                         const showSetting  : array of TMEShowSetting;
                         const assistant    : array of string);

    function FindClass(exceptObject: TObject; classes: string) : boolean;
    var i1 : integer;
    begin
      result := false;
      FormatSubStrs(classes, ',');
      for i1 := 1 to SubStrCount(classes, ',') do
        if IsClass(exceptObject, SubStr(classes, i1, ',')) then begin
          result := true;
          break;
        end;
    end;

  var i1 : integer;
  begin
    for i1 := 0 to high(classes) do
      if FindClass(exceptObject, classes[i1]) then
        break;
    with Self do
      begin
        FCreateBugReport  := not noBugReport[i1];
        FCreateScreenShot := (not noScreenShot[i1]) and (GetScreenShotDepth > 0);
        SetSuspendThreads ((not noSuspend[i1]) and GetSuspendThreads);
        FCallHandlers     := not noHandlers[i1];
        FShowSetting      := showSetting[i1];
        FShowAssis        := assistant[i1];
      end;
  end;

var Self : TIMEException;
begin
  if settings = nil then
    settings := MESettingsEx(1);
  Self := TIMEException.Create(settings.Module);
  result := Self;
  with Self do begin
    ZeroMemory(@FSettingsBuf, sizeOf(FSettingsBuf));
    FSettings := @FSettingsBuf;
    FVersionVar := settings.VersionVar;
    CheckExceptParams(exceptObject, exceptAddr, preparedStack, context);
    CheckExceptAddr(exceptObject, exceptAddr);
    FModuleSettings  := settings;
    FExceptType      := exceptType;
    FExceptObject    := exceptObject;
    FExceptAddr      := exceptAddr;
    FCanContinue     := canContinue;
    FCanContinue2    := canContinue;
    if crashedThreadId <> 0 then
         FCrashedThreadId := crashedThreadId
    else FCrashedThreadId := GetCurrentThreadId;
    if currentEsp <> 0 then
         FCurrentEsp := currentEsp
    else FCurrentEsp := Esp;
    if currentEbp <> 0 then
         FCurrentEbp := currentEbp
    else FCurrentEbp := Ebp;
    FPContext        := context;
    if FPContext <> nil then
      FContext         := FPContext^;
    FSource          := source;
    FRelatedObject   := relatedObject;
    FPackage         := package;
    FPreparedStack   := preparedStack;
    FProgressAlert   := NewProgressAlert(GetPleaseWaitTitle, FModule, 'MEIPLWAIT');
    if AmHttpServer then
      SetShowPleaseWaitBox(false);
    with FModuleSettings do
      CheckFilters(Self,
                   [Filter1Classes,      Filter2Classes],
                   [Filter1NoBugReport,  Filter2NoBugReport,  GeneralNoBugReport ],
                   [Filter1NoScreenShot, Filter2NoScreenShot, GeneralNoScreenShot],
                   [Filter1NoSuspend,    Filter2NoSuspend,    GeneralNoSuspend   ],
                   [Filter1NoHandlers,   Filter2NoHandlers,   GeneralNoHandlers  ],
                   [Filter1ShowSetting,  Filter2ShowSetting,  GeneralShowSetting ],
                   [Filter1Assistant,    Filter2Assistant,    GeneralAssistant   ]);
    FAppendScreenShot := FCreateScreenShot;
  end;
end;

// ***************************************************************
// intercepting of Delphis normal exception handling

var
  // access to ExceptObjProc at compile time is not possible in a package
  // we have to access it at runtime
  PExceptObjProc : pointer = nil; // @ExceptObjProc;

function Esp : dword;
// return the current esp value (stack bottom)
asm
  mov eax, esp
  sub eax, 4
end;

function Ebp : dword;
// return the current ebp value (stack frame chain pointer)
asm
  mov eax, ebp
end;

function MyExceptObjProc(er: PExceptionRecord; esp: dword) : TObject;
type TExceptObjProc = function (er: PExceptionRecord) : TObject;
begin
  if ExceptObjProc <> nil then
       result := TExceptObjProc(ExceptObjProc)(er)
  else result := MadException.Create('Unknown exception. If you want to know more, ' +
                                     'you have to add SysUtils to your project.');
  LastExceptObject := result;
  LastExceptAddr   := er.ExceptionAddress;
  LastContext      := PContext(TPCardinal(esp + 4 + $c)^)^;
end;

procedure MyExceptObjProc1;
asm
  mov edx, esp
  jmp MyExceptObjProc
end;

procedure MyExceptObjProc2;
asm
  mov edx, esp
  add edx, 4*4
  jmp MyExceptObjProc
end;

procedure MyRaiseExceptProc;
asm
  push edx
  push ecx
  push eax
  call System.@GetTls
  pop ecx
  mov dword ptr [eax+LastExceptObject], ecx
  mov dword ptr [eax+LastContext+176], ecx
  pop ecx
  pop edx
  mov dword ptr [eax+LastExceptAddr], edx
  mov dword ptr [eax+LastContext+156], edi
  mov dword ptr [eax+LastContext+160], esi
  mov dword ptr [eax+LastContext+164], ebx
  mov dword ptr [eax+LastContext+168], edx
  mov dword ptr [eax+LastContext+172], ecx
  mov dword ptr [eax+LastContext+180], ebp
  mov dword ptr [eax+LastContext+184], edx
  mov dword ptr [eax+LastContext+196], esp
  jmp RaiseException
end;

var PMyExceptObjProc1  : pointer = @MyExceptObjProc1;
    PMyExceptObjProc2  : pointer = @MyExceptObjProc2;
    {$ifdef d6}
      PMyRaiseExceptProc : pointer = @MyRaiseExceptProc;
    {$endif}

// intercept SysUtils.ShowException
procedure InterceptSShowExcept(exceptObject: TObject; exceptAddr: pointer);
begin
  {$ifdef log}log('InterceptSShowExcept');{$endif}
  HandleException(etNormal, exceptObject, exceptAddr, true, Esp, Ebp, nil, esSysUtilsShowException);
end;

// intercept Application.ShowException
procedure InterceptAShowExcept(self: TObject; E: MadException);
begin
  {$ifdef log}log('InterceptAShowExcept');{$endif}
  HandleException(etNormal, E, nil, true, Esp, Ebp, nil, esApplicationShowException);
end;

// intercept Application.HandleException
procedure InterceptAHandleExcept(self, Sender: TObject);
begin
  {$ifdef log}log('InterceptAHandleExcept');{$endif}
  if GetCapture <> 0 then SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
  HandleException(etNormal, nil, nil, true, Esp, Ebp, nil, esApplicationHandleException, Sender);
end;

// intercept TCGIApplication.CGIHandleException
procedure InterceptCGIHandleException(self, Sender: TObject);
begin
  {$ifdef log}log('InterceptCGIHandleException');{$endif}
  HandleException(etNormal, nil, nil, true, Esp, Ebp, nil, esCGI, Sender);
end;

// intercept TISAPIApplication.ISAPIHandleException
procedure InterceptISAPIHandleException(self, Sender: TObject);
var exceptObject  : TObject;
    exceptAddr    : pointer;
    preparedStack : pointer;
    context       : PContext;
begin
  {$ifdef log}log('InterceptISAPIHandleException');{$endif}
  exceptObject  := nil;
  exceptAddr    := nil;
  preparedStack := nil;
  context       := nil;
  CheckExceptParams(exceptObject, exceptAddr, preparedStack, context);
  CheckExceptAddr(exceptObject, exceptAddr);
  HandleException(etNormal, exceptObject, exceptAddr, true, Esp, Ebp, context, esISAPI, Sender, 0, @isapiBugReport);
  isapiExceptAddr := exceptAddr;
end;

type
  // ISAPI specific type
  TExtensionControlBlock = packed record
    size, version, connID, statusCode          : dword;
    logData                                    : array [0..79] of char;
    method, queryStr, pathInfo, pathTranslated : pchar;
    totalBytes, available                      : dword;
    data, contentType                          : pointer;
    GetServerVariable     : pointer;
    WriteClient           : function (connID: dword; buf: pointer; var len: dword; reserved: dword) : bool; stdcall;
    ReadClient            : pointer;
    ServerSupportFunction : function (hConn, request: dword; buf, size, dataType: pchar) : bool; stdcall;
  end;

const
  // ISAPI specific constants
  HSE_REQ_SEND_RESPONSE_HEADER    = 3;
  HSE_REQ_DONE_WITH_SESSION       = 4;
  HSE_REQ_SEND_RESPONSE_HEADER_EX = 1016;

type
  TIsapiRequest = record
    ServerSupportFunctionNext : function (hConn, request: dword; buf, size, dataType: pchar) : bool; stdcall;
    ResponseHeaderSent        : boolean;
    ECB                       : ^TExtensionControlBlock;
  end;

var
  IRSection : PRTLCriticalSection = nil;
  IRList    : array of TIsapiRequest;
  IRCount   : integer = 0;

function RemoveIR(var ecb: TExtensionControlBlock; index: integer) : boolean;
var b1 : boolean;
    i1 : integer;
begin
  result := false;
  EnterCriticalSection(IRSection^);
  try
    // let's remove this isapi request from our internal list again
    // has the index stayed the same? that would save us looping = performance
    b1 := (index < IRCount) and (IRList[index].ECB = @ecb);
    if b1 then
      i1 := index
    else
      // no, the index changed, <sigh>, so we need to loop to find the new index
      for i1 := 0 to IRCount - 1 do
        if IRList[i1].ECB = @ecb then begin
          b1 := true;
          break;
        end;
    if b1 then begin
      // we found the isapi request in the internal list
      // now let's store whether somebody already sent the response header
      result := IRList[i1].ResponseHeaderSent;
      // and finally clean up the list
      dec(IRCount);
      IRList[i1] := IRList[IRCount];
      IRList[IRCount].ServerSupportFunctionNext := nil;
      IRList[IRCount].ResponseHeaderSent        := false;
      IRList[IRCount].ECB                       := nil;
    end;
  finally LeaveCriticalSection(IRSection^) end;
  // we don't need the copied ECB, anymore
  Dispose(@ecb);
end;

function InterceptServerSupportFunction(hConn, request: dword; buf, size, dataType: pchar) : bool; stdcall;
// intercept the ISAPI function "ServerSupportFunction"
var i1   : integer;
    ecb  : ^TExtensionControlBlock;
    ssfn : function (hConn, request: dword; buf, size, dataType: pchar) : bool; stdcall;
begin
  ecb := nil;
  ssfn := nil;
  EnterCriticalSection(IRSection^);
  try
    // first let's find out which address the original "ServerSupportFunction" has
    // in order to do that we need to loop through the internal list of isapi requests
    for i1 := 0 to IRCount - 1 do
      if IRList[i1].ECB^.connID = hConn then begin
        ecb  := pointer(IRList[i1].ECB);
        ssfn := IRList[i1].ServerSupportFunctionNext;
        break;
      end;
  finally LeaveCriticalSection(IRSection^) end;
  // now call the original "ServerSupportFunction" (if we found the address)
  result := (@ssfn <> nil) and ssfn(hConn, request, buf, size, dataType);
  if result and
     (request = HSE_REQ_SEND_RESPONSE_HEADER   ) or
     (request = HSE_REQ_SEND_RESPONSE_HEADER_EX) then begin
    // we don't want to send the response header twice
    // so if somebody already called it, we won't do it again later...
    EnterCriticalSection(IRSection^);
    try
      if (i1 < IRCount) and (IRList[i1].ECB^.connID = hConn) then
        // has the index of our isapi request in the internal list stayed the same?
        // most of the time this will be true, so we save the looping
        IRList[i1].ResponseHeaderSent := true
      else
        // the index has changed, must be because of multiple threads
        // well, no problem, we can deal with that, but it costs a bit of performance
        for i1 := 0 to IRCount - 1 do
          if IRList[i1].ECB^.connID = hConn then begin
            IRList[i1].ResponseHeaderSent := true;
            break;
          end;
    finally LeaveCriticalSection(IRSection^) end;
  end;
  if (ecb <> nil) and (request = HSE_REQ_DONE_WITH_SESSION) then
    RemoveIR(ecb^, i1);
end;

function InterceptHttpExtensionProc(var ecb: TExtensionControlBlock) : dword; stdcall;
// intercept exported HttpExtensionProc (for ISAPI dlls)
const HSE_STATUS_PENDING = 3;
      HSE_STATUS_ERROR   = 4;
var c1   : dword;
    ecb2 : ^TExtensionControlBlock;
    i1   : integer;
    b1   : boolean;
    s1   : string;
    exceptObject  : TObject;
    exceptAddr    : pointer;
    preparedStack : pointer;
    context       : PContext;
begin
  // we want to intercept all calls to the "ServerSupportFunction"
  // so we copy the ECB and store our hook function into the copied instance
  New(ecb2);
  ecb2^ := ecb;
  ecb2^.ServerSupportFunction := InterceptServerSupportFunction;
  EnterCriticalSection(IRSection^);
  try
    // now we store this isapi request into our internal list
    if IRCount = Length(IRList) then
      if IRCount = 0 then
           SetLength(IRList, 64)
      else SetLength(IRList, IRCount * 2);
    i1 := IRCount;
    inc(IRCount);
    IRList[i1].ServerSupportFunctionNext := ecb.ServerSupportFunction;
    IRList[i1].ResponseHeaderSent        := false;
    IRList[i1].ECB                       := pointer(ecb2);
  finally LeaveCriticalSection(IRSection^) end;
  // now we call the original HttpExtensionProc function
  isapiBugReport  := '';
  isapiExceptAddr := nil;
  exceptAddr      := nil;
  exceptObject    := nil;
  preparedStack   := nil;
  context         := nil;
  s1 := '';
  try
    result := HttpExtensionProcNext(ecb2);
    s1 := isapiBugReport;
    exceptAddr := isapiExceptAddr;
  except
    result := HSE_STATUS_ERROR;
    CheckExceptParams(exceptObject, exceptAddr, preparedStack, context);
    CheckExceptAddr(exceptObject, exceptAddr);
    HandleException(etNormal, exceptObject, exceptAddr, true, Esp, Ebp, context, esHttpExtension, nil, 0, @s1);
  end;
  isapiBugReport := '';
  isapiExceptAddr := nil;
  b1 := (result <> HSE_STATUS_PENDING) and RemoveIR(ecb2^, i1);
  if (result = HSE_STATUS_ERROR) and (s1 <> '') then begin
    if not b1 then
      // we send the response header only if it wasn't sent yet
      ecb.ServerSupportFunction(ecb.ConnID, HSE_REQ_SEND_RESPONSE_HEADER, nil, nil,
                                'Content-Type: text/html' + #$D#$A + #$D#$A);
    s1 := BugReportHtml(s1, MESettings(exceptAddr));
    c1 := length(s1);
    ecb.WriteClient(ecb.ConnID, pchar(s1), c1, 0);
  end;
end;

{$ifdef d6}
  // intercept TThread.Synchronize's exception catching
  function InterceptSynchronize : TObject;
  begin
    if (exceptObject = nil) or (not IsClass(exceptObject, 'EAbort')) then begin
      HandleException(etNormal, nil, nil, true, Esp, Ebp, nil, esTThreadSynchronize);
      result := nil;
    end else
      result := AcquireExceptionObject;
  end;

  // intercept runtime errors in D6
  procedure InterceptWriteErrorMessage;
  var exc : MadException;
  begin
    {$ifdef log}log('InterceptWriteErrorMessage');{$endif}
    if System_runErrMsg <> nil then
         exc := MadException.Create(pchar(System_runErrMsg))
    else exc := MadException.Create('Runtime error');
    HandleException(etNormal, exc, ErrorAddr, false, Esp, Ebp, nil, esRuntimeError);
    exc.Free;
  end;
{$else}
  // intercept runtime errors in D4-D5, part I
  procedure InterceptHalt0WriteLn(dummy: integer; error: pchar);
  var exc : MadException;
  begin
    {$ifdef log}log('InterceptHalt0WriteLn, error: "' + error + '", ErrorAddr: ' + IntToHexEx(dword(ErrorAddr)));{$endif}
    exc := MadException.Create(error);
    HandleException(etNormal, exc, ErrorAddr, false, Esp, Ebp, nil, esRuntimeError);
    exc.Free;
  end;

  // intercept runtime errors in D4-D5, part II
  function InterceptHalt0MessageBox(dummy1: dword; error, dummy2: pchar; dummy3: dword) : integer; stdcall;
  var exc : MadException;
  begin
    {$ifdef log}log('InterceptHalt0MessageBox, error: "' + error + '", ErrorAddr: ' + IntToHexEx(dword(ErrorAddr)));{$endif}
    result := 0;
    exc := MadException.Create(error);
    HandleException(etNormal, exc, ErrorAddr, false, Esp, Ebp, nil, esRuntimeError);
    exc.Free;
  end;
{$endif}

procedure InterceptInitializePackage(module, dummy: dword);
// support for 2nd parameter added
begin
  {$ifdef log}log('InterceptInitializePackage, module: ' + IntToHexEx(module));{$endif}
  try
    SysUtils_InitializePackage(module, dummy);
  except HandleException(etNormal, nil, nil, true, Esp, Ebp, nil, esInitializePackage, nil, module) end;
end;

// intercept finalization of units, if an error occurred during initialiation
procedure InterceptFinalizeUnits;

  function RealStuff : boolean;

    procedure Log1;
    begin
      {$ifdef log}log('InterceptFinalizeUnits, RaiseList: ' + IntToHexEx(dword(RaiseList)));{$endif}
    end;

  begin
    // we have to be careful with allocations in this function
    // because after System_FinalizeUnits the allocator doesn't work anymore
    // so if Delphi tries to free some of our local strings variables at the end
    // of this function, this will result in a crash
    // so we have to do everything which can result in allocations in local funcs
    Log1;
    result := (GetCurrentThreadId <> MainThreadId) or (RaiseList = nil);
  end;

asm
  // System.Halt0 calls FinalizeUnits even for DLL_THREAD_DETACH events
  // the TLS is already freed at that moment, so accessing "RaiseList" may AV
  // using try..except may not work at this time, either
  // so we're using a low level asm try..except block

  // first: set up the exception handler
  lea eax, @except
  push eax
  mov eax, fs:[0]
  push eax
  mov fs:[0], esp 

  // now do the real work
  call RealStuff

  // if we reach this, there was no exception
  jmp @cleanup

 @except:
  // ouch, RaiseList evidently crashed
  // let's simply ignore the exception and continue at @failure
  mov eax, [esp+$c]
  lea ecx, @failure
  mov [eax+$b8], ecx
  mov ecx, [esp+$8]
  mov [eax+$c4], ecx
  mov eax, 0
  ret

 @failure:
  // there was an exception, so we will call System_FinalizeUnits later
  mov eax, 1

 @cleanup:
  // final cleanup: remove the exception handler again
  mov ecx, [esp]
  mov fs:[0], ecx
  add esp, 8

  // if eax then System_FinalizeUnits;
  cmp eax, 0
  jz @quit
  call System_FinalizeUnits

 @quit: 
end; 

// intercept the using of ExceptProc
procedure InterceptExceptProc(exceptObject: TObject; exceptAddr: pointer; esp: dword);
begin
  {$ifdef log}log('InterceptExceptProc, exceptObject: ' + IntToHexEx(dword(exceptObject)) + ', exceptAddr: ' + IntToHexEx(dword(exceptAddr)));{$endif}
  // these kind of exception can normally not be continued
  // however, if they occur inside of a thread, we can kill only that thread
  HandleException(etNormal, exceptObject, exceptAddr, GetCurrentThreadId <> ProcessMainThreadId, esp, 0, TPPointer(esp + $c)^, esExceptProc);
  // evidently the exception occurred inside of a thread and
  // the program shall continue to run, so we kill the current thread
  SetThreadInfo(GetCurrentThreadId, 0, '', '', nil, 0);
  ExitThread(0);
end;

// get full exception descriptions even without SysUtils
procedure MapToRunError(P: PExceptionRecord); stdcall;

  function CreateAVStr : string;

    procedure Replace1Str(sub, str: string);
    var i1 : integer;
    begin
      i1 := PosStr(sub, result);
      Delete(result, i1, 2);
      Insert(str, result, i1);
    end;

  var mi    : TMemoryBasicInformation;
      arrCh : array [0..MAX_PATH] of char;
  begin
    with P^ do begin
      VirtualQuery(ExceptionAddress, mi, sizeOf(mi));
      if (mi.State = MEM_COMMIT) and (GetModuleFileName(dword(mi.AllocationBase), arrCh, MAX_PATH) <> 0) then begin
        result := SModuleAccessViolation;
        Replace1Str('%s', ExtractFileName(arrCh));
      end else
        {$ifdef d7}
          result := 'Access Violation';
        {$else}
          result := SAccessViolation;
        {$endif}
      Replace1Str('%p', IntToHexEx(dword(ExceptionAddress)));
      if ExceptionInformation[0] = 0 then
           Replace1Str('%s',  SReadAccess)
      else Replace1Str('%s', SWriteAccess);
      Replace1Str('%p', IntToHexEx(ExceptionInformation[1]));
    end;
  end;

var s1   : string;
    exc  : MadException;
    ebp_ : dword;
begin
  asm
    mov ebp_, ebp
  end;
  case P.ExceptionCode of
    $C0000094                       : s1 := SDivByZero;
    $C000008C                       : s1 := SRangeError;
    $C0000095                       : s1 := SIntOverflow;
    $C000008F, $C0000090, $C0000092 : s1 := SInvalidOp;
    $C000008E                       : s1 := SZeroDivide;
    $C0000091                       : s1 := SOverflow;
    $C0000093, $C000008D            : s1 := SUnderflow;
    $C0000005                       : s1 := CreateAVStr;
    $C0000096                       : s1 := SPrivilege;
    $C000013A                       : s1 := SControlC;
    $C00000FD                       : s1 := SStackOverflow;
    else                              s1 := SExternalException;
  end;
  exc := MadException.Create(s1);
  HandleException(etNormal, exc, P.ExceptionAddress, false, 0, 0, TPPointer(ebp_ + 4 + $c)^, esSystemExceptionHandler);
  exc.Free;
end;

// needed by our InterceptExceptionHandler
procedure RtlUnwind; stdcall; external kernel32 name 'RtlUnwind';

// intercept Delphi's main exception handling procedure
procedure InterceptExceptionHandler;
asm
        {$ifdef log}
          MOV     EAX, 0
          CALL    log
        {$endif}

        MOV     EAX,[ESP+4]

        TEST    [EAX].TExceptionRecord.ExceptionFlags,6
        JNE     @@exit

        MOV     EAX,[ESP+4]
        CLD
        CALL    _FpuInit
        MOV     EDX,[ESP+8]

        PUSH    0
        PUSH    EAX
        PUSH    offset @@returnAddress
        PUSH    EDX
        CALL    RtlUnwind

@@returnAddress:
        MOV     EBX,[ESP+4]
        CMP     [EBX].TExceptionRecord.ExceptionCode,cDelphiException
        MOV     EDX,[EBX+$14] //.TExceptionRecord.ExceptAddr
        MOV     EAX,[EBX+$18] //.TExceptionRecord.ExceptObject
        JE      @@DelphiException2

        MOV     EDX,PExceptObjProc
        MOV     EDX,[EDX]
        TEST    EDX,EDX
        JE      MapToRunError
        MOV     EAX,EBX
        CALL    EDX
        TEST    EAX,EAX
        JE      MapToRunError
        MOV     EDX,[EBX].TExceptionRecord.ExceptionAddress

@@DelphiException2:
        MOV     ECX,ESP
        CALL    InterceptExceptProc

@@exit:
        XOR     EAX,EAX
end;

procedure InterceptHandleAutoException;
asm
        { ->    [ESP+ 4] excPtr: PExceptionRecord       }
        {       [ESP+ 8] errPtr: PExcFrame              }
        {       [ESP+12] ctxPtr: Pointer                }
        {       [ESP+16] dspPtr: Pointer                }
        { <-    EAX return value - always one           }

        MOV     EAX,[ESP+4]
        TEST    [EAX].TExceptionRecord.ExceptionFlags,6
        JNE     @@exit

        CLD
        CALL    _FpuInit

        OR      [EAX].TExceptionRecord.ExceptionFlags,2

        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EDX,[ESP+8+3*4]

        PUSH    0
        PUSH    EAX
        PUSH    offset @@returnAddress
        PUSH    EDX
        CALL    RtlUnwind

@@returnAddress:
        POP     EBP
        POP     EDI
        POP     ESI
        MOV     EAX,[ESP+4]
        MOV     EBX,8000FFFFH
        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        JE      @@DelphiException

        MOV     EDX,PExceptObjProc
        MOV     EDX,[EDX]
        TEST    EDX,EDX
        JE      MapToRunError
        MOV     EAX,[ESP+4]
        CALL    EDX
        TEST    EAX,EAX
        JE      MapToRunError
        MOV     EDX,EAX
        MOV     EAX,[ESP+4]
        MOV     [EAX+$18],EDX
        MOV     EDX,[EAX].TExceptionRecord.ExceptionAddress
        MOV     [EAX+$14],EDX

@@DelphiException:
        MOV     EDX,[EAX+$18] //.TExceptionRecord.ExceptObject
        MOV     ECX,[EAX+$14] //.TExceptionRecord.ExceptAddr
        MOV     EAX,[ESP+8]
        MOV     EAX,[EAX+$0c] //.TExcFrame.SelfOfMethod
        TEST    EAX,EAX
        JZ      @@freeException
        MOV     EBX,[EAX]
        CALL    [EBX].vmtSafeCallException.Pointer
        MOV     EBX,EAX
@@freeException:
        MOV     EAX,[ESP+4]
        MOV     EAX,[EAX+$18] //.TExceptionRecord.ExceptObject
        CALL    TObject.Free
@@done:
        XOR     EAX,EAX
        MOV     ESP,[ESP+8]
        POP     ECX
        MOV     FS:[EAX],ECX
        POP     EDX
        POP     EBP
        ADD     EDX,5
        POP     ECX
        JMP     EDX
@@exit:
        MOV     EAX,1
end;

type
  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord = record
    ExceptionCode: Cardinal;
    ExceptionFlags: Cardinal;
    ExceptionRecord: PExceptionRecord;
    ExceptionAddress: Pointer;
    NumberParameters: Cardinal;
    ExceptionInformation: TThreadNameInfo;
  end;
  EExternalException = class
    FMessage: string;
    FHelpContext: Integer;
    ExceptionRecord: PExceptionRecord;
  end;

procedure HandleHiddenException(exceptObject: TObject; RaiseList: pointer);
begin
  {$ifdef log}log('HandleHiddenException');{$endif}
  try
    if (exceptObject <> nil) and (exceptObject.ClassName = 'EExternalException') and
       (EExternalException(exceptObject).ExceptionRecord <> nil) then
      with EExternalException(exceptObject).ExceptionRecord^, ExceptionInformation do
        if (ExceptionCode = $406D1388) and (FType = $1000) and (FFlags = 0) then begin
          if FThreadId = $ffffffff then
               SetThreadInfo(GetCurrentThreadId, 0, FName, '', nil, 0)
          else SetThreadInfo(FThreadId,          0, FName, '', nil, 0);
          exit;
        end;
  except end;
  if (GetCurrentThreadId <> HandleExceptionTid) and
     ((HiddenHandlers <> nil) or (HiddenHandlersOO <> nil)) and (RaiseList <> nil) and
     (exceptObject <> nil) and IsValidObject(exceptObject) and (MadException(exceptObject).Message <> #0) then
    HandleException(etHidden, exceptObject, TRaiseFrame(RaiseList^).ExceptAddr, true, Esp, Ebp, nil, esHidden);
  LastExceptObject := nil;
  LastExceptAddr := nil;
end;

// intercept "System._DoneExcept"'s call to exceptObject.Free
procedure InterceptFreeExceptObject;
asm
  pushad
  pushfd
  call HandleHiddenException
  popfd
  popad
  call TObject.Free
end;

// ***************************************************************
// general API hooking stuff

function PatchAPI(dll, api: string; org: pointer; var next: pointer; callback: pointer) : boolean;
var p1 : pointer;
    c1 : dword;
begin
  if next = nil then begin
    next := GetImageProcAddress(GetModuleHandle(pchar(dll)), api);
    if (org <> nil) and (TPWord(org)^ = $25ff) then
         p1 := pointer(pointer(pointer(dword(org) + 2)^)^)
    else p1 := nil;
    // only the application module may hook all modules
    // dlls/packages may not, because they might come and go
    // however, the madExcept bpl may hook all modules, too
    if (HInstance = GetModuleHandle(nil)) or AmMeBpl then
         PatchImportTables(GetModuleList, next, callback)
    else PatchImportTable (HInstance,     next, callback);
    result := (p1 <> nil) and (p1 = pointer(pointer(pointer(dword(org) + 2)^)^));
    if result then begin
      // the import table patching didn't change our own API import
      // this can happen with exe compressors/protectors
      // so we patch our own API import, additionally
      p1 := pointer(pointer(dword(org) + 2)^);
      if VirtualProtect(p1, 4, PAGE_EXECUTE_READWRITE, @c1) then begin
        pointer(p1^) := callback;
        VirtualProtect(p1, 4, c1, @c1);
      end;
    end;
  end else
    result := false;
end;

procedure UnpatchAPI(org, next, callback: pointer);
var p1 : pointer;
    c1 : dword;
begin
  if next <> nil then begin
    if (org <> nil) and (TPWord(org)^ = $25ff) then
         p1 := pointer(pointer(pointer(dword(org) + 2)^)^)
    else p1 := nil;
    if (HInstance = GetModuleHandle(nil)) or AmMeBpl then
         PatchImportTables(GetModuleList, callback, next)
    else PatchImportTable (HInstance,     callback, next);
    if (p1 <> nil) and (p1 = pointer(pointer(pointer(dword(org) + 2)^)^)) then begin
      p1 := pointer(pointer(dword(org) + 2)^);
      if VirtualProtect(p1, 4, PAGE_EXECUTE_READWRITE, @c1) then begin
        pointer(p1^) := next;
        VirtualProtect(p1, 4, c1, @c1);
      end;
    end;
  end;
end;

// ***************************************************************
// intercepting of CreateThread & TThread

type
  // needed to transport parameters to our ThreadExceptFrame
  TThreadInfo = record
    threadProc : pointer;
    param      : pointer;
  end;
  TPThreadInfo = ^TThreadInfo;

  // needed to intercept TThread.Execute
  TThreadInterceptor = packed record
    movEdx  : byte;     // $ba
    execute : pointer;
    movEcx  : byte;     // $b9
    frame   : pointer;
    jmp     : word;     // $ff d1
  end;
  TPThreadInterceptor = ^TThreadInterceptor;

function CallThreadProcSafe(threadProc, param: pointer) : dword; stdcall;
// protect the stack, just in case the thread function is incorrect
asm
  push ebx
  mov ebx, esp
  mov eax, [ebp+$c]
  push eax
  mov eax, [ebp+$8]
  call eax
  // if everything is alright, we should have "esp = ebp" here
  cmp ebx, esp
  jz @done

  // something is wrong with either esp or ebp !!
  ja @ebxBigger

  // ebx < esp
  // e.g. stdcall thread function with more than 1 parameters
  push eax
  mov eax, ebx
  mov ebx, esp
  add ebx, 4
  sub ebx, eax
  pop eax
  cmp ebx, 12    // we accept ebx only if "esp - ebx <= 12" and
  ja @done
  test ebx, 3    // we accept ebx only if "ebx and $3 = 0"
  jnz @done
  sub esp, ebx
  jmp @done

  // ebx > esp
  // e.g. delphi thread function, "stdcall" forgotten 
 @ebxBigger:
  sub ebx, esp
  cmp ebx, 4     // we accept ebx only if "ebx - esp = 4"
  jnz @done
  add esp, ebx

 @done:
  pop ebx
end;

function ThreadExceptFrame(ti: TPThreadInfo) : dword; stdcall;
// our frame around each and every new thread
type TThreadProc = function (param: pointer) : dword; stdcall;
var threadProc : pointer;
    param      : pointer;
begin
  try
    threadProc := ti^.threadProc;
    param      := ti^.param;
    Dispose(ti);
    try
      result := CallThreadProcSafe(threadProc, param);
    except
      result := maxCard;
      if MadExceptPause >= 0 then
        raise;
      HandleException(etNormal, nil, nil, true, Esp, Ebp, nil, esThreadFrame);
    end;
  finally SetThreadInfo(GetCurrentThreadId, 0, '', '', nil, 0) end;
end;

procedure HookedTThreadExecute(self, execute: pointer);
// intercept TThread.Execute
type TThreadExecute = procedure (self: pointer);
begin
  try
    try
      TThreadExecute(execute)(self);
    except
      if MadExceptPause >= 0 then
        raise;
      HandleException(etNormal, nil, nil, true, Esp, Ebp, nil, esTThreadExecute, self);
    end;
  finally SetThreadInfo(GetCurrentThreadId, 0, '', '', nil, 0) end;
end;

var Armadillo : boolean = false;
procedure HandleCreateThreadHook(ebpFrameSkip: integer; threadProc, param: pointer;
                                 var ti: TPThreadInfo; var class_: string; var threadCreator: pointer);
// handle CreateThread hook events
const vmtClassName = 44;
      vmtParent    = 36;
var c1, c2, c3 : dword;
    i1         : integer;
    name       : string[7];
    tti        : ^TThreadInterceptor;
    creator    : TPAPointer;
    fi         : TFunctionInfo;
    mutex      : dword;
    p1         : pointer;
    sa         : TSecurityAttributes;
    sd         : TSecurityDescriptor;
begin
  asm
    mov creator, ebp
  end;
  try
    for i1 := 1 to ebpFrameSkip do
      creator := creator^[0];
    if Armadillo then
      try
        if TPAPointer(creator^[0])^[1] <> nil then
          creator := creator^[0];
      except end;
    p1 := @BeginThread;
    if word(p1^) = $25ff then
      p1 := TPPointer(TPPointer(dword(p1) + 2)^)^;
    if dword(creator^[1]) > dword(p1) then begin
      fi := ParseFunction(p1);
      if fi.IsValid and (dword(creator^[1]) <= dword(fi.CodeBegin) + dword(fi.CodeLen)) then
        creator := creator^[0];
    end;
    class_ := '';
    if (param <> nil) and
       TryRead(@TPThreadInfo(param)^.param, @c1, 4) and
       TryRead(pointer(c1),                 @c1, 4) then begin
      // is this CreateThread resulting from a TThread.Create?
      c2 := 0;
      i1 := 0;
      while (c1 > vmtParent) and (i1 < 15) and
            TryRead(pointer(c1 - vmtParent), @c3, 4) and
            TryRead(pointer(c3),             @c3, 4) do begin
        c2 := c1;
        c1 := c3;
        inc(i1);
      end;
      if (c2 > vmtClassName) and
         TryRead(TPPointer(c2 - vmtClassName), @c3,   4) and
         TryRead(pointer(c3),                  @name, 8) and
         (name = 'TThread') then begin
        // yes! we have a TThread here!
        // first we ask the class name of the TThread
        class_ := TObject(TPThreadInfo(param)^.param).ClassName;
        // the creator would now fire in TThread.Create, let's go up one level:
        creator := creator^[0];
        // did we already hook this TThread class?
        tti := TPAPointer(TPPointer(TPThreadInfo(param)^.param)^)^[1];
        if tti^.frame <> @HookedTThreadExecute then begin
          // it's not hooked, let's enter a mutex to make this thread safe
          InitSecAttr(sa, sd);
          mutex := CreateMutex(@sa, false, pchar('HookTThread' + IntToHexEx(GetCurrentProcessId)));
          if mutex <> 0 then
            try
              WaitForSingleObject(mutex, INFINITE);
              try
                // let's check again to avoid double hooking
                if tti^.frame <> @HookedTThreadExecute then begin
                  // no, we really didn't hook it yet, so we do it now!
                  // we replace the "Execute" function pointer with a bit of asm code
                  // which jumps to our "HookedTThreadExecute" interceptor
                  tti := VirtualAlloc(nil, sizeOf(tti^), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
                  tti^.movEdx  := $ba;
                  tti^.execute := TPAPointer(TPPointer(TPThreadInfo(param)^.param)^)^[1];
                  tti^.movEcx  := $b9;
                  tti^.frame   := @HookedTThreadExecute;
                  tti^.jmp     := $e1ff;
                  p1 := @TPAPointer(TPPointer(TPThreadInfo(param)^.param)^)^[1];
                  if VirtualProtect(p1, 4, PAGE_EXECUTE_READWRITE, @c1) then begin
                    pointer(p1^) := tti;
                    VirtualProtect(p1, 4, c1, @c1);
                  end;
                end;
              finally ReleaseMutex(mutex) end;
            finally CloseHandle(mutex) end;
        end;
      end;
    end;
  except end;
  threadCreator := creator^[1];
  New(ti);
  ti^.threadProc := threadProc;
  ti^.param      := param;
end;

{$W+}

var NextCreateThread : function (threadAttr, stackSize, threadProc, param,
                                 creationFlags, threadId: pointer) : dword stdcall = nil;
function HookedCreateThread(threadAttr, stackSize, threadProc, param,
                            creationFlags, threadId: pointer) : dword; stdcall;
// intercept CreateThread calls hooked through IAT patching
var ti            : TPThreadInfo;
    threadCreator : pointer;
    class_        : string;
    tid           : dword;
begin
  HandleCreateThreadHook(1, threadProc, param, ti, class_, threadCreator);
  result := NextCreateThread(threadAttr, stackSize, @ThreadExceptFrame, ti, creationFlags, @tid);
  if threadId <> nil then
    dword(threadId^) := tid;
  if result <> 0 then
    SetThreadInfo(tid, result, '', class_, threadCreator, GetCurrentThreadId);
  {$ifdef log}log('Hook new thread id (1) ' + IntToHexEx(dword(tid)));{$endif}
end;

function HookedCreateRemoteThread(process: dword; threadAttr: pointer; stackSize: dword;
                                  threadProc, param: pointer; creationFlags: dword; var threadId: dword) : dword; stdcall;
// intercept CreateThread calls hooked through CreateThread code manipulation
var ti            : TPThreadInfo;
    threadCreator : pointer;
    class_        : string;
    tid           : dword;
begin
  HandleCreateThreadHook(2, threadProc, param, ti, class_, threadCreator);
  result := CreateRemoteThread(process, threadAttr, stackSize, @ThreadExceptFrame, ti, creationFlags, tid);
  if @threadId <> nil then
    dword(threadId) := tid;
  if result <> 0 then
    SetThreadInfo(tid, result, '', class_, threadCreator, GetCurrentThreadId);
  {$ifdef log}log('Hook new thread id (2) ' + IntToHexEx(dword(tid)));{$endif}
end;

{$W-}

function UserWorkItemExceptFrame(ti: TPThreadInfo) : dword; stdcall;
// our frame around each and every new user work item
type TThreadProc = function (param: pointer) : dword; stdcall;
var threadProc : pointer;
    param      : pointer;
begin
  try
    threadProc := ti^.threadProc;
    param      := ti^.param;
    Dispose(ti);
    try
      result := CallThreadProcSafe(threadProc, param);
    except
      result := maxCard;
      if MadExceptPause >= 0 then
        raise;
      HandleException(etNormal, nil, nil, true, Esp, Ebp, nil, esThreadFrame);
    end;
  finally end;
end;

var NextQueueUserWorkItem : function (threadProc, context: pointer; flags: dword) : bool; stdcall = nil;
function HookedQueueUserWorkItem(threadProc, context: pointer; flags: dword) : bool; stdcall;
// intercept QueueUserWorkItem calls hooked through IAT patching
var ti : TPThreadInfo;
begin
  New(ti);
  ti^.threadProc := threadProc;
  ti^.param      := context;
  result := NextQueueUserWorkItem(@UserWorkItemExceptFrame, ti, flags);
  {$ifdef log}log('Hook queue user work item');{$endif}
end;

function PatchRelCall(api, old, new: pointer) : boolean;
// if "api" internally calls "old", we detour this call instruction to "new"
// this is an usual kind of API hooking, but *very* effective!
var fi : TFunctionInfo;
    i1 : integer;
    c1 : dword;
begin
  result := false;
  fi := ParseFunction(api);
  if fi.IsValid then
    for i1 := 0 to high(fi.FarCalls) do
      with fi.FarCalls[i1] do
        if RelTarget and (Target = old) then begin
          if VirtualProtect(PTarget, 4, PAGE_EXECUTE_READWRITE, @c1) then begin
            dword(PTarget^) := dword(new) - dword(CodeAddr2);
            VirtualProtect(PTarget, 4, c1, @c1);
            result := true;
          end;
          break;
        end;
end;

procedure InitCreateThreadHook(const settings: IMESettings);
var nh : PImageNtHeaders;
begin
  {$ifdef log}log('InitCreateThreadHook: +');{$endif}
  nh := GetImageNtHeaders(settings.Module);
  // we can check for Armadillo by looking at the linker fields in the header
  Armadillo := (nh <> nil) and
               (chr(nh^.OptionalHeader.MajorLinkerVersion) = 'S') and
               (chr(nh^.OptionalHeader.MinorLinkerVersion) = 'R');
  if (GetVersion and $80000000 <> 0) or
     (not PatchRelCall(GetProcAddress(GetModuleHandle(kernel32), 'CreateThread'),
                       GetProcAddress(GetModuleHandle(kernel32), 'CreateRemoteThread'),
                       @HookedCreateRemoteThread)) then
    PatchAPI(kernel32, 'CreateThread', @CreateThread, @NextCreateThread, @HookedCreateThread);
  PatchAPI(kernel32, 'QueueUserWorkItem', nil, @NextQueueUserWorkItem, @HookedQueueUserWorkItem);
end;

procedure CloseCreateThreadHook;
begin
  {$ifdef log}log('CloseCreateThreadHook');{$endif}
  if (GetVersion and $80000000 <> 0) or
     (not PatchRelCall(GetProcAddress(GetModuleHandle(kernel32), 'CreateThread'),
                       @HookedCreateRemoteThread,
                       GetProcAddress(GetModuleHandle(kernel32), 'CreateRemoteThread'))) then
    UnpatchAPI(@CreateThread, @NextCreateThread, @HookedCreateThread);
  UnpatchAPI(nil, @NextQueueUserWorkItem, @HookedQueueUserWorkItem);
end;

// ***************************************************************
// intercepting of GetMessage and PeekMessage

var
  NextGetMessageA  : function (var msg: TMsg; wnd, filterMin, filterMax           : dword) : bool stdcall = nil;
  NextPeekMessageA : function (var msg: TMsg; wnd, filterMin, filterMax, removeMsg: dword) : bool stdcall = nil;
  NextGetMessageW  : function (var msg: TMsg; wnd, filterMin, filterMax           : dword) : bool stdcall = nil;
  NextPeekMessageW : function (var msg: TMsg; wnd, filterMin, filterMax, removeMsg: dword) : bool stdcall = nil;

function GetMessageCallbackA(var msg: TMsg; wnd, filterMin, filterMax: dword) : bool; stdcall;
begin
  PauseMeEventually;
  result := NextGetMessageA(msg, wnd, filterMin, filterMax);
end;

function PeekMessageCallbackA(var msg: TMsg; wnd, filterMin, filterMax, removeMsg: dword) : bool; stdcall;
begin
  PauseMeEventually;
  result := NextPeekMessageA(msg, wnd, filterMin, filterMax, removeMsg);
end;

function GetMessageCallbackW(var msg: TMsg; wnd, filterMin, filterMax: dword) : bool; stdcall;
begin
  PauseMeEventually;
  result := NextGetMessageW(msg, wnd, filterMin, filterMax);
end;

function PeekMessageCallbackW(var msg: TMsg; wnd, filterMin, filterMax, removeMsg: dword) : bool; stdcall;
begin
  PauseMeEventually;
  result := NextPeekMessageW(msg, wnd, filterMin, filterMax, removeMsg);
end;

procedure InitMessageHooks;
begin
  {$ifdef log}log('InitMessageHooks: +');{$endif}
  PatchAPI(user32,  'GetMessageA', @ GetMessageA, @ NextGetMessageA, @ GetMessageCallbackA);
  PatchAPI(user32, 'PeekMessageA', @PeekMessageA, @NextPeekMessageA, @PeekMessageCallbackA);
  PatchAPI(user32,  'GetMessageW', @ GetMessageW, @ NextGetMessageW, @ GetMessageCallbackW);
  PatchAPI(user32, 'PeekMessageW', @PeekMessageW, @NextPeekMessageW, @PeekMessageCallbackW);
end;

procedure CloseMessageHooks;
begin
  {$ifdef log}log('CloseMessageHooks, was installed: ' + booleanToChar(@NextGetMessageA <> nil));{$endif}
  UnpatchAPI(@ GetMessageA, @ NextGetMessageA, @ GetMessageCallbackA);
  UnpatchAPI(@PeekMessageA, @NextPeekMessageA, @PeekMessageCallbackA);
  UnpatchAPI(@ GetMessageW, @ NextGetMessageW, @ GetMessageCallbackW);
  UnpatchAPI(@PeekMessageW, @NextPeekMessageW, @PeekMessageCallbackW);
end;

// ***************************************************************
// specials

function madTraceProcess(mapSize: integer) : integer; stdcall;
// thread function for the tool madTraceProcess

  function GetBugReport : string;
  var exc : IMEException;
  begin
    exc := NewException(etFrozen, nil, nil, true, 0, 0, 0, nil, MESettings, esManual, nil, 0, nil);
    exc.ShowPleaseWaitBox := false;
    exc.ListThreads := true;
    result := exc.GetBugReport;
  end;

var s1  : string;
    map : dword;
    buf : pointer;
begin
  result := 0;
  NameThread(GetCurrentThreadId, '-');
  s1 := GetBugReport;
  if s1 <> '' then
    if mapSize > 0 then begin
      if GetVersion and $80000000 = 0 then
           map := OpenFileMapping(FILE_MAP_ALL_ACCESS, false, 'Global\madTraceProcessMap')
      else map := 0;
      if map = 0 then
        map := OpenFileMapping(FILE_MAP_ALL_ACCESS, false, 'madTraceProcessMap');
      if map <> 0 then begin
        buf := MapViewOfFile(map, FILE_MAP_ALL_ACCESS, 0, 0, 0);
        if buf <> nil then begin
          if Length(s1) >= mapSize then
            SetLength(s1, mapSize - 1);
          Move(pointer(s1)^, buf^, Length(s1) + 1);
          UnmapViewOfFile(buf);
        end;
        CloseHandle(map);
      end;
    end else
      FillClipboard(s1);
  NameThread(GetCurrentThreadId, '');
end;

exports madTraceProcess;

procedure HyperJumpCallstack(callstackItem: string);
type TEnumWindowRec = record windowName, callstackItem: string end;

  function EnumWindowProc(window: cardinal; var ewr: TEnumWindowRec) : bool; stdcall;
  var arrCh : array [0..MAX_PATH] of char;
      c1    : dword;
  begin
    arrCh[0] := #0;
    GetClassName(window, arrCh, MAX_PATH);
    result := not PosStrIs1(ewr.windowName, arrCh);
    if not result then
      SendMessageTimeout(window, RegisterWindowMessage('madExceptIdeJumpCommand'),
                         integer(GetCurrentProcessId), integer(ewr.callstackItem), SMTO_ABORTIFHUNG, 1000, c1);
  end;

var ewr    : TEnumWindowRec;
    window : dword;
begin
  window := FindWindow('TAppBuilder', nil);
  if window <> 0 then begin
    ewr.windowName := 'madToolsMsgHandlerWindow' + IntToHexEx(GetWindowThreadProcessId(window, nil));
    ewr.callstackItem := callstackItem;
    EnumWindows(@EnumWindowProc, integer(@ewr));
  end;
end;

// ***************************************************************
// unhandled exception filter stuff

var OldExceptionFilter : pointer = pointer(maxCard);

function NewExceptionFilter(var xp: TExceptionPointers) : integer; stdcall;
// this is meant for BCB mainly, but shouldn't harm to Delphi, either
const cCppException : dword = $0EEFFACE;
var exceptAddr, exceptObject : pointer;
begin
  exceptAddr   := nil;
  exceptObject := nil;
  try
    if (xp.ExceptionRecord.ExceptionCode = cCppException) and
       (xp.ExceptionRecord.NumberParameters = 3) then begin
      exceptAddr   := pointer(xp.ExceptionRecord.ExceptionInformation[1]);
      exceptObject := MadException.Create('Unhandled C++ exception');
    end else
      if xp.ExceptionRecord.ExceptionCode = cDelphiException then begin
        exceptAddr   := pointer(xp.ExceptionRecord.ExceptionInformation[0]);
        exceptObject := pointer(xp.ExceptionRecord.ExceptionInformation[1]);
        if xp.ExceptionRecord.NumberParameters = 8 then
          // raised by BCB -> exceptAddr is 4 too low
          inc(dword(exceptAddr), 4);
      end else begin
        exceptAddr   := xp.ExceptionRecord.ExceptionAddress;
        exceptObject := MadException.Create('Unhandled OS exception');
      end;
  except end;
  HandleException(etNormal, exceptObject, exceptAddr, false,
                  dword(xp.ContextRecord.Esp), dword(xp.ContextRecord.Ebp),
                  xp.ContextRecord, esUnhandledFilter);
  result := 0;
end;

procedure InstallUnhandledExceptionFilter;
begin
  OldExceptionFilter := SetUnhandledExceptionFilter(@NewExceptionFilter);
end;

procedure UninstallUnhandledExceptionFilter;
begin
  if dword(OldExceptionFilter) <> maxCard then begin
    SetUnhandledExceptionFilter(OldExceptionFilter);
    OldExceptionFilter := pointer(maxCard);
  end;
end;

// ***************************************************************
// initialization / finalization

procedure CheckRestart;
var map : dword;
    buf : TPACardinal;
    c1  : dword;
begin
  {$ifdef log}log('CheckRestart');{$endif}
  map := OpenFileMapping(FILE_MAP_READ, false, pchar('madExceptRestart' + IntToHexEx(GetCurrentProcessId)));
  if map <> 0 then begin
    {$ifdef log}log('restart!');{$endif}
    buf := MapViewOfFile(map, FILE_MAP_READ, 0, 0, 0);
    if buf <> nil then begin
      CloseHandle(buf^[1]);
      c1 := OpenProcess(SYNCHRONIZE, false, buf^[0]);
      if c1 <> 0 then begin
        {$ifdef log}log('wait for process id ' + IntToHexEx(buf^[0]));{$endif}
        WaitForSingleObject(c1, INFINITE);
        CloseHandle(c1);
      end;
      Move(buf^[2], Last10Exceptions, 80);
      UnmapViewOfFile(buf);
    end;
    CloseHandle(map);
  end;
end;

procedure Init;
type TProcedure = procedure;

  procedure AddPtr(var ptr: pointer; add: dword);
  begin
    if ptr <> nil then dword(ptr) := dword(ptr) + add;
  end;

  function System_DoneExcept : pointer;
  asm
    mov eax, offset System.@DoneExcept
    cmp word ptr [eax], $25ff
    jnz @done
    mov eax, [eax + 2]
    mov eax, [eax]
   @done:
  end;

  function System_Write0CString : pointer;
  asm
    mov eax, offset System.@Write0CString
    cmp word ptr [eax], $25ff
    jnz @done
    mov eax, [eax + 2]
    mov eax, [eax]
   @done:
  end;

  function System_Halt0 : pointer;
  asm
    mov eax, offset System.@Halt0
    cmp word ptr [eax], $25ff
    jnz @done
    mov eax, [eax + 2]
    mov eax, [eax]
   @done:
  end;

  function System_HandleAutoException : pointer;
  asm
    mov eax, offset System.@HandleAutoException
    cmp word ptr [eax], $25ff
    jnz @done
    mov eax, [eax + 2]
    mov eax, [eax]
   @done:
  end;

  function System_HandleAnyException : pointer;
  asm
    mov eax, offset System.@HandleAnyException
    cmp word ptr [eax], $25ff
    jnz @done
    mov eax, [eax + 2]
    mov eax, [eax]
   @done:
  end;

  function System_HandleOnException : pointer;
  asm
    mov eax, offset System.@HandleOnException
    cmp word ptr [eax], $25ff
    jnz @done
    mov eax, [eax + 2]
    mov eax, [eax]
   @done:
  end;

  function System_RaiseExcept : pointer;
  asm
    mov eax, offset System.@RaiseExcept
    cmp word ptr [eax], $25ff
    jnz @done
    mov eax, [eax + 2]
    mov eax, [eax]
   @done:
  end;

  function CheckFileCrc(module: dword) : boolean;

    function CalcCheckSum(buf: pointer; size, checkSum: dword) : dword;
    var i1 : integer;
    begin
      result := checkSum;
      for i1 := 0 to size div 2 - 1 do begin
        result := result + word(buf^);
        if result and $ffff0000 <> 0 then
          result := result and $ffff + result shr 16;
        inc(dword(buf), 2);
      end;
      if odd(size) then begin
        result := result + byte(buf^);
        result := result and $ffff + result shr 16;
      end;
      result := word(result and $ffff + result shr 16);
    end;

  var arrCh   : array [0..MAX_PATH] of char;
      fh, map : dword;
      buf     : pointer;
      nh      : PImageNtHeaders;
      ignore  : dword;
      size    : dword;
      s1      : string;
  begin
    result := false;
    GetModuleFileName(module, arrCh, MAX_PATH);
    fh := CreateFile(arrCh, GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if fh <> INVALID_HANDLE_VALUE then begin
      map := CreateFileMapping(fh, nil, PAGE_READONLY, 0, 0, nil);
      if map <> 0 then begin
        buf := MapViewOfFile(map, FILE_MAP_READ, 0, 0, 0);
        if buf <> nil then begin
          nh := GetImageNtHeaders(dword(buf));
          if (nh <> nil) and (nh^.OptionalHeader.CheckSum <> 0) then begin
            ignore := dword(@nh^.OptionalHeader.CheckSum);
            size := GetFileSize(fh, nil);
            if CalcCheckSum(pointer(ignore + 4), size - (ignore - dword(buf)) - 4,
                            CalcCheckSum(buf, ignore - dword(buf), 0)) + size <> nh^.OptionalHeader.CheckSum then
              result := true;
          end;
          UnmapViewOfFile(buf);
        end;
        CloseHandle(map);
      end;
      CloseHandle(fh);
    end;
    if result then begin
      MessageBoxA(0, pchar(mySettings.BitFaultMsg), nil, MB_ICONERROR);
      if HInstance <> GetModuleHandle(nil) then begin
        mySettings := nil;
        raise MadException.Create(s1);
      end else
        ExitProcess(0);
    end;
  end;

  function FindBpl(suggestion: dword; name: string; version: integer; api: string) : dword;
  var modules : TDAModule;
      i1      : integer;
  begin
    modules := nil;
    if (suggestion = 0) or (GetProcAddress(suggestion, pchar(api)) = nil) then begin
      suggestion := GetModuleHandle(pchar(name + IntToStrEx(version) + '.bpl'));
      if GetProcAddress(suggestion, pchar(api)) = nil then begin
        result := 0;
        modules := GetModuleList;
        for i1 := 0 to high(modules) do
          if IsTextEqual(CopyR(modules[i1].fileName, 4), '.bpl') and
             (GetProcAddress(modules[i1].handle, pchar(api)) <> nil) then begin
            result := modules[i1].handle;
            break;
          end;
      end else
        result := suggestion;
    end else
      result := suggestion;
  end;

  procedure CalibrateModule(module: dword);
  var s1 : string;
      nh : PImageNtHEaders;
      c1 : dword;
  begin
    s1 := ResToStr(module, 'MAD', 'CALIBRATE');
    if (Length(s1) = 20) and ((module = HInstance) = (TPACardinal(s1)^[0] and $80000000 = 0)) then begin
      nh := GetImageNtHeaders(module);
      VirtualProtect(@nh^.OptionalHeader, sizeOf(nh^.OptionalHeader), PAGE_EXECUTE_READWRITE, @c1);
      if module = HInstance then begin
        nh^.OptionalHeader.BaseOfCode := dword(@CalibrateCode) - TPACardinal(s1)^[0] - module;
        nh^.OptionalHeader.BaseOfData := dword(@CalibrateData) - TPACardinal(s1)^[1] - module;
      end else begin
        nh^.OptionalHeader.BaseOfCode := TPACardinal(s1)^[0] and $7fffffff;
        nh^.OptionalHeader.BaseOfData := TPACardinal(s1)^[1] and $7fffffff;
      end; 
      nh^.OptionalHeader.SizeOfCode := TPACardinal(s1)^[2];
      nh^.OptionalHeader.SizeOfInitializedData := TPACardinal(s1)^[3];
      nh^.OptionalHeader.SizeOfUninitializedData := TPACardinal(s1)^[4];
      VirtualProtect(@nh^.OptionalHeader, sizeOf(nh^.OptionalHeader), c1, @c1);
    end;
  end;

  procedure CalibrateModules;
  var exe : dword;
      ml  : TDAModule;
      i1  : integer;
  begin
    exe := GetModuleHandle(nil);
    CalibrateModule(exe);
    if exe <> HInstance then
      CalibrateModule(HInstance);
    ml := GetModuleList;
    for i1 := 0 to high(ml) do
      if (ml[i1].handle <> exe) and (ml[i1].handle <> HInstance) and
         IsTextEqual(CopyR(ml[i1].fileName, 4), '.bpl') then
        CalibrateModule(ml[i1].handle);
  end;

var ci, ci2      : TCodeInfo;
    fi           : TFunctionInfo;
    b1           : boolean;
    p1           : pointer;
    {$ifndef d6}
      b2           : boolean;
      MessageBox   : pointer;
    {$endif}
    map          : dword;
    tl           : TDACardinal;
    vcl, rtl     : dword;
    clx          : dword;
    hookPackages : boolean;
    s1           : string;
    ptn          : TPThreadName;
    c1           : dword;
    i64, i65     : int64;
    sa           : TSecurityAttributes;
    sd           : TSecurityDescriptor;
begin
  {$ifdef log}
    log('Init, executable: ' + ModuleName(0) + ', current module: ' + ModuleName(HInstance));
    indentLog;
  {$endif}
  tl := nil;
  if QueryPerformanceCounter(i64) and QueryPerformanceFrequency(i65) then
    StartTime := i64 div (i65 div 1000);
  AmMeBpl := GetProcAddress(HInstance, '@Madexcept@initialization$qqrv') <> nil;
  madNVAssistant.HandleContactFormProc    := @HandleContactForm;
  madNVAssistant.HandleScreenshotFormProc := @HandleScreenshotForm;
  mySettings := MESettings(HInstance);
  if mySettings.IsValid and mySettings.Enabled and mySettings.CheckFileCrc then begin
    CheckFileCrc(HInstance);
    if mySettings.Module <> HInstance then
      CheckFileCrc(mySettings.Module);
  end;
  OpenThreadProc := GetProcAddress(GetModuleHandle(kernel32), 'OpenThread');
  NtOpenThreadProc := GetProcAddress(GetModuleHandle('ntdll.dll'), 'NtOpenThread');
  CalibrateModules;
  if mySettings.IsValid then begin
    InitSecAttr(sa, sd);
    map := CreateFileMapping(maxCard, @sa, PAGE_READWRITE, 0, sizeOf(TMadExceptBuf),
                             pchar('madExceptThreadNameBuf' + IntToHexEx(GetCurrentProcessId)));
    if GetLastError = 0 then begin
      {$ifdef log}log('new thread name buffer');{$endif}
      madExceptBuf := MapViewOfFile(map, FILE_MAP_ALL_ACCESS, 0, 0, 0);
      madExceptBuf^.referenceCount   := 1;
      madExceptBuf^.antiFreezeMutex  := CreateMutex(nil, false, nil);
      madExceptBuf^.threadNamesMutex := CreateMutex(nil, false, nil);
      madExceptBuf^.threadNamesList  := nil;
      madExceptBuf^.mapHandle        := map;
      tl := GetThreadList;
      if tl <> nil then begin
        ProcessMainThreadId := tl[0];
        SetThreadInfo(tl[0], 0, 'main thread', '', nil, 0);
      end else
        ProcessMainThreadId := MainThreadId;
    end else
      if map <> 0 then begin
        madExceptBuf := MapViewOfFile(map, FILE_MAP_ALL_ACCESS, 0, 0, 0);
        CloseHandle(map);
        InterlockedIncrement(madExceptBuf^.referenceCount);
        {$ifdef log}log('open thread name buffer');{$endif}
      end else begin
        {$ifdef log}log('thread name buffer map failure!');{$endif}
        MessageBoxA(0, 'internal error (opening global buffer)', 'madExcept', 0);
        ExitProcess(0);
      end;
    if madExceptBuf = nil then begin
      {$ifdef log}log('internal error!');{$endif}
      MessageBoxA(0, 'internal error. please notify bug@madshi.net', 'madExcept', 0);
      ExitProcess(0);
    end;
    if ProcessMainThreadId = 0 then begin
      WaitForSingleObject(madExceptBuf^.threadNamesMutex, INFINITE);
      try
        ptn := madExceptBuf^.threadNamesList;
        while ptn <> nil do begin
          ProcessMainThreadId := ptn^.id;
          ptn := ptn^.next;
        end;
      finally ReleaseMutex(madExceptBuf^.threadNamesMutex) end;
      if ProcessMainThreadId = 0 then
        ProcessMainThreadId := MainThreadId;
    end;
    {$ifdef log}log('current module valid, base address: ' + IntToHexEx(HInstance));{$endif}
    // when dealing with runtime packages, only the application module
    // should hook into the packages, because the dlls might come and go
    // alternatively we may also hook, when we are the madExcept bpl
    hookPackages := (HInstance = GetModuleHandle(nil)) or AmMeBpl;
    if mySettings.Enabled then begin
      {$ifdef log}log('System_ExceptionHandler: ' + IntToHexEx(dword(System_ExceptionHandler)));{$endif}
      AddPtr(Forms_TApplication_HandleException,              HInstance);
      AddPtr(Forms_TApplication_ShowException,                HInstance);
      AddPtr(Qforms_TApplication_HandleException,             HInstance);
      AddPtr(Qforms_TApplication_ShowException,               HInstance);
      AddPtr(SysUtils_ShowException,                          HInstance);
      AddPtr(SysUtils_LoadPackage,                            HInstance);
      AddPtr(@SysUtils_InitializePackage,                     HInstance);
      AddPtr(Classes_CheckSynchronize,                        HInstance);
      AddPtr(CGIApp_TCGIApplication_CGIHandleException,       HInstance);
      AddPtr(ISAPIApp_TISAPIApplication_ISAPIHandleException, HInstance);
      AddPtr(@HttpExtensionProcNext,                          HInstance);
      AddPtr(@System_InitUnits,                               HInstance);
      AddPtr(@System_FinalizeUnits,                           HInstance);
      AddPtr(System_ExceptionHandler,                         HInstance);
      AddPtr(System_runErrMsg,                                HInstance);
      {$ifdef bcb}
        AddPtr(BcbExceptionHandler,                             HInstance);
        AddPtr(BcbThrowExceptionLDTC,                           HInstance);
        AddPtr(BcbInitExceptBlockLDTC,                          HInstance);
        AddPtr(BcbOrgMalloc,                                    HInstance);
        AddPtr(BcbMemcpy,                                       HInstance);
        AddPtr(BcbCallTerminate,                                HInstance);
      {$endif}
      if ((System_ExceptionHandler             = nil) or         // rtlxx.bpl?
          (Forms_TApplication_HandleException  = nil) or         // vclxx.bpl?
          (Qforms_TApplication_HandleException = nil)    ) and   // visualclxxx.bpl?
         hookPackages then begin
        p1 := @FreeMemory;
        if word(p1^) = $25ff then begin
          p1 := TPPointer(TPPointer(dword(p1) + 2)^)^;
          FindModule(p1, rtl, s1);
          if rtl = HInstance then
            rtl := 0;
        end;
        {$ifdef ver120}
          vcl := FindBpl(rtl, 'vcl', 40, '@Forms@TApplication@HandleException$qqrp14System@TObject');
          if rtl = 0 then
            rtl := FindBpl(vcl, 'rtl', 40, '@System@FreeMem$qpv');
          clx := 0;
        {$else}
          {$ifdef ver130}
            vcl := FindBpl(rtl, 'vcl', 50, '@Forms@TApplication@HandleException$qqrp14System@TObject');
            if rtl = 0 then
              rtl := FindBpl(vcl, 'rtl', 50, '@System@FreeMem$qpv');
            clx := 0;
          {$else}
            {$ifdef ver140}
              vcl := FindBpl(rtl, 'vcl', 60, '@Forms@TApplication@HandleException$qqrp14System@TObject');
              if rtl = 0 then
                rtl := FindBpl(vcl, 'rtl', 60, '@System@FreeMem$qpv');
              clx := FindBpl(0, 'visualclx', 60, '@Qforms@TApplication@HandleException$qqrp14System@TObject');
            {$else}
              {$ifdef ver150}
                vcl := FindBpl(rtl, 'vcl', 70, '@Forms@TApplication@HandleException$qqrp14System@TObject');
                if rtl = 0 then
                  rtl := FindBpl(vcl, 'rtl', 70, '@System@FreeMem$qpv');
                clx := FindBpl(0, 'visualclx', 70, '@Qforms@TApplication@HandleException$qqrp14System@TObject');
              {$else}
                {$ifdef ver170}
                  vcl := FindBpl(rtl, 'vcl', 90, '@Forms@TApplication@HandleException$qqrp14System@TObject');
                  if rtl = 0 then
                    rtl := FindBpl(vcl, 'rtl', 90, '@System@FreeMem$qpv');
                  clx := FindBpl(0, 'visualclx', 90, '@Qforms@TApplication@HandleException$qqrp14System@TObject');
                {$else}
                  vcl := FindBpl(rtl, 'vcl', 100, '@Forms@TApplication@HandleException$qqrp14System@TObject');
                  if rtl = 0 then
                    rtl := FindBpl(vcl, 'rtl', 100, '@System@FreeMem$qpv');
                  clx := FindBpl(0, 'visualclx', 100, '@Qforms@TApplication@HandleException$qqrp14System@TObject');
                {$endif}
              {$endif}
            {$endif}
          {$endif}
        {$endif}
        {$ifdef log}log('vcl: ' + IntToHexEx(vcl) + ', rtl: ' + IntToHexEx(rtl) +  ', clx: ' + IntToHexEx(clx) + ', ProcessModule: ' + IntToHexEx(GetModuleHandle(nil)));{$endif}
        if (System_ExceptionHandler = nil) and (rtl <> 0) then begin
          SysUtils_ShowException := GetProcAddress(rtl, '@Sysutils@ShowException$qqrp14System@TObjectpv');
          // is the RTL already hooked?
          if (SysUtils_ShowException <> nil) and (byte(SysUtils_ShowException^) <> $E9) then begin
            // no, it's not hooked yet
            SysUtils_LoadPackage := GetProcAddress(rtl, '@Sysutils@LoadPackage$qqrx17System@AnsiString');
            {$ifdef ver120}
              SysUtils_InitializePackage := GetProcAddress(rtl, '@Sysutils@InitializePackage$qqri');
            {$else}
              SysUtils_InitializePackage := GetProcAddress(rtl, '@Sysutils@InitializePackage$qqrui');
            {$endif}
            {$ifdef d6}
              {$ifdef d7}
                Classes_CheckSynchronize := GetProcAddress(rtl, '@Classes@CheckSynchronize$qqri');
              {$else}
                Classes_CheckSynchronize := GetProcAddress(rtl, '@Classes@CheckSynchronize$qqrv');
              {$endif}
            {$endif}
            {$ifdef d6}
              ci := ParseCode(GetProcAddress(rtl, '@System@@StartExe$qqrp23System@PackageInfoTablep17System@TLibModule'));
            {$else}
              ci := ParseCode(GetProcAddress(rtl, '@System@@StartExe$qqrv'));
            {$endif}
            b1 := true;
            while ci.IsValid and (ci.Opcode <> $C3) do begin
              if (ci.Target <> nil) and ci.RelTarget and (ci.TargetSize = 4) then
                if b1 then begin
                  b1 := false;
                  ci2 := ParseCode(ci.Target);
                  while ci2.IsValid and (ci2.Opcode <> $C3) do begin
                    if (ci2.Opcode = $C7) and (ci2.ModRm = $40) and
                       (TPByte(dword(ci2.Next) - 5)^ = $04) then begin  // mov dword ptr [eax+$04], xxxx
                      System_ExceptionHandler := TPPointer(dword(ci2.Next) - 4)^;
                      break;
                    end;
                    ci2 := ParseCode(ci2.Next);
                  end;
                end else begin
                  System_InitUnits := ci.Target;
                  ci := ParseCode(@System_InitUnits);
                  while ci.IsValid and (ci.Opcode <> $C3) do begin
                    if (ci.Target <> nil) and (ci.Opcode = $E8) and ci.RelTarget and (ci.TargetSize = 4) then begin
                      System_FinalizeUnits := ci.Target;
                      break;
                    end;
                    ci := ParseCode(ci.Next);
                  end;
                  break;
                end;
              ci := ParseCode(ci.Next);
            end;
          end else begin
            // the RTL is already hooked, so we don't do it again
            hookPackages := false;
            SysUtils_ShowException := nil;
          end;
        end;
        if hookPackages then begin
          if (Forms_TApplication_HandleException = nil) and (vcl <> 0) then begin
            Forms_TApplication_HandleException := GetProcAddress(vcl, '@Forms@TApplication@HandleException$qqrp14System@TObject');
            Forms_TApplication_ShowException   := GetProcAddress(vcl, '@Forms@TApplication@ShowException$qqrp18Sysutils@Exception');
          end;
          if (Qforms_TApplication_HandleException = nil) and (clx <> 0) then begin
            Qforms_TApplication_HandleException := GetProcAddress(clx, '@Qforms@TApplication@HandleException$qqrp14System@TObject');
            Qforms_TApplication_ShowException   := GetProcAddress(clx, '@Qforms@TApplication@ShowException$qqrp18Sysutils@Exception');
          end;
        end;
      end;
      {$ifdef log}
        log('Forms_TApplication_HandleException: '              + IntToHexEx(dword(Forms_TApplication_HandleException             )));
        log('Forms_TApplication_ShowException: '                + IntToHexEx(dword(Forms_TApplication_ShowException               )));
        log('Qforms_TApplication_HandleException: '             + IntToHexEx(dword(Qforms_TApplication_HandleException            )));
        log('Qforms_TApplication_ShowException: '               + IntToHexEx(dword(Qforms_TApplication_ShowException              )));
        log('SysUtils_ShowException: '                          + IntToHexEx(dword(SysUtils_ShowException                         )));
        log('SysUtils_LoadPackage: '                            + IntToHexEx(dword(SysUtils_LoadPackage                           )));
        log('SysUtils_InitializePackage: '                      + IntToHexEx(dword(@SysUtils_InitializePackage                    )));
        log('Classes_CheckSynchronize: '                        + IntToHexEx(dword(Classes_CheckSynchronize                       )));
        log('CGIApp_TCGIApplication_CGIHandleException: '       + IntToHexEx(dword(CGIApp_TCGIApplication_CGIHandleException      )));
        log('ISAPIApp_TISAPIApplication_ISAPIHandleException: ' + IntToHexEx(dword(ISAPIApp_TISAPIApplication_ISAPIHandleException)));
        log('HttpExtensionProcNext: '                           + IntToHexEx(dword(@HttpExtensionProcNext                         )));
        log('@System_InitUnits: '                               + IntToHexEx(dword(@System_InitUnits                              )));
        log('@System_FinalizeUnits: '                           + IntToHexEx(dword(@System_FinalizeUnits                          )));
        log('System_ExceptionHandler: '                         + IntToHexEx(dword(System_ExceptionHandler                        )));
        log('System_runErrMsg: '                                + IntToHexEx(dword(System_runErrMsg                               )));
        {$ifdef bcb}
          log('BcbExceptionHandler: '                             + IntToHexEx(dword(BcbExceptionHandler                            )));
          log('BcbThrowExceptionLDTC: '                           + IntToHexEx(dword(BcbThrowExceptionLDTC                          )));
          log('BcbInitExceptBlockLDTC: '                          + IntToHexEx(dword(BcbInitExceptBlockLDTC                         )));
          log('BcbOrgMalloc: '                                    + IntToHexEx(dword(BcbOrgMalloc                                   )));
          log('BcbMemcpy: '                                       + IntToHexEx(dword(BcbMemcpy                                      )));
          log('BcbCallTerminate: '                                + IntToHexEx(dword(BcbCallTerminate                               )));
        {$endif}
      {$endif}
      {$ifdef log}log('hook system exception procedures');{$endif}
      PExceptObjProc := @ExceptObjProc;
      if System_ExceptionHandler <> nil then
        PatchJmp(System_ExceptionHandler, @InterceptExceptionHandler);
      if Forms_TApplication_HandleException <> nil then
        PatchJmp(Forms_TApplication_HandleException, @InterceptAHandleExcept);
      if Forms_TApplication_ShowException <> nil then
        PatchJmp(Forms_TApplication_ShowException, @InterceptAShowExcept);
      if Qforms_TApplication_HandleException <> nil then
        PatchJmp(Qforms_TApplication_HandleException, @InterceptAHandleExcept);
      if Qforms_TApplication_ShowException <> nil then
        PatchJmp(Qforms_TApplication_ShowException, @InterceptAShowExcept);
      if SysUtils_ShowException <> nil then
        PatchJmp(SysUtils_ShowException, @InterceptSShowExcept);
      if CGIApp_TCGIApplication_CGIHandleException <> nil then
        PatchJmp(CGIApp_TCGIApplication_CGIHandleException, @InterceptCGIHandleException);
      if ISAPIApp_TISAPIApplication_ISAPIHandleException <> nil then
        PatchJmp(ISAPIApp_TISAPIApplication_ISAPIHandleException, @InterceptISAPIHandleException);
      if @HttpExtensionProcNext <> nil then begin
        AmHttpServer := true;
        New(IRSection);
        InitializeCriticalSection(IRSection^);
      end;
      if (SysUtils_LoadPackage <> nil) and (@SysUtils_InitializePackage <> nil) then begin
        fi := ParseFunction(SysUtils_LoadPackage);
        if length(fi.FarCalls) = 1 then
          SysUtils_LoadPackage := fi.FarCalls[0].Target;
        fi := ParseFunction(@SysUtils_InitializePackage);
        if length(fi.FarCalls) = 1 then
          SysUtils_InitializePackage := fi.FarCalls[0].Target;
        ci := ParseCode(SysUtils_LoadPackage);
        while not (ci.Opcode in [$C2, $C3]) do begin
          if (ci.Target <> nil) and (ci.Target = @SysUtils_InitializePackage) then begin
            if (ci.TargetSize = 4) and ci.RelTarget then begin
              {$ifdef log}log('hook SysUtils.LoadPackage''s call to InitializePackage');{$endif}
              PatchInt(ci.PTarget, integer(@InterceptInitializePackage) - integer(ci.Next));
            end;
            break;
          end;
          ci := ParseCode(ci.Next);
        end;
      end;
      {$ifdef d6}
        if Classes_CheckSynchronize <> nil then begin
          p1 := @AcquireExceptionObject;
          if word(p1^) = $25ff then
            p1 := TPPointer(TPPointer(dword(p1) + 2)^)^;
          ci := ParseCode(Classes_CheckSynchronize);
          while not (ci.Opcode in [$C2, $C3]) do begin
            if (ci.Target <> nil) and (ci.Target = p1) then begin
              if (ci.TargetSize = 4) and ci.RelTarget then begin
                {$ifdef log}log('hook Classes.CheckSynchronize''s call to AcquireExceptionObject');{$endif}
                PatchInt(ci.PTarget, integer(@InterceptSynchronize) - integer(ci.Next));
              end;
              break;
            end;
            ci := ParseCode(ci.Next);
          end;
        end;
      {$endif}
      {$ifdef d6}
        ci := ParseCode(System_Halt0);
        b1 := false;
        while not (ci.Opcode in [$C2, $C3]) do begin
          if ci.RelTarget and (ci.TargetSize = 4) and (ci.Target <> nil) then
            if b1 then begin
              {$ifdef log}log('hook System.Halt0''s call to WriteErrorMessage');{$endif}
              PatchInt(ci.PTarget, integer(@InterceptWriteErrorMessage) - integer(ci.Next));
              break;
            end else
              b1 := true;
          ci := ParseCode(ci.Next);
        end;
      {$else}
        MessageBox := SolveW9xDebugMode(ParseCode(@MessageBoxA).Target);
        b1 := false;
        b2 := false;
        ci := ParseCode(System_Halt0);
        while not (ci.Opcode in [$C2, $C3]) do begin
          if ci.Target <> nil then
            if (ci.Target = System_Write0CString) and (not b1) then begin
              if (ci.TargetSize = 4) and ci.RelTarget then begin
                {$ifdef log}log('hook System.Halt0''s call to WriteLn');{$endif}
                PatchInt(ci.PTarget, integer(@InterceptHalt0WriteLn) - integer(ci.Next));
              end;
              if b2 then break;
              b1 := true;
            end else
              if not b2 then begin
                p1 := ParseCode(ci.Target).Target;
                if p1 <> nil then begin
                  if (GetVersion and $80000000 <> 0) and (dword(p1) > $80000000) and (TPByte(p1)^ = $68) then  // w9x debug mode?
                    p1 := TPPointer(dword(p1) + 1)^;
                  if p1 = MessageBox then begin
                    if (ci.TargetSize = 4) and ci.RelTarget then begin
                      {$ifdef log}log('hook System.Halt0''s call to MessageBox');{$endif}
                      PatchInt(ci.PTarget, integer(@InterceptHalt0MessageBox) - integer(ci.Next));
                    end;
                    if b1 then break;
                    b2 := true;
                  end;
                end;
              end;
          ci := ParseCode(ci.Next);
        end;
      {$endif}
      ci := ParseCode(System_Halt0);
      while not (ci.Opcode in [$C2, $C3]) do begin
        if (ci.Target <> nil) and (ci.Target = @System_FinalizeUnits) then begin
          if (ci.TargetSize = 4) and ci.RelTarget then begin
            {$ifdef log}log('hook System.Halt0''s call to FinalizeUnits');{$endif}
            PatchInt(ci.PTarget, integer(@InterceptFinalizeUnits) - integer(ci.Next));
          end;
          break;
        end;
        ci := ParseCode(ci.Next);
      end;
      if (@System_InitUnits <> nil) and (@System_FinalizeUnits <> nil) then begin
        ci := ParseCode(@System_InitUnits);
        while not (ci.Opcode in [$C2, $C3]) do begin
          if ci.Target = @System_FinalizeUnits then begin
            if (ci.TargetSize = 4) and ci.RelTarget then begin
              {$ifdef log}log('hook System.InitUnits'' call to FinalizeUnits');{$endif}
              PatchInt(ci.PTarget, integer(@InterceptFinalizeUnits) - integer(ci.Next));
            end;
            break;
          end;
          ci := ParseCode(ci.Next);
        end;
      end;
      if @System_FinalizeUnits <> nil then begin
        ci := ParseCode(@System_FinalizeUnits);
        while not (ci.Opcode in [$C2, $C3]) do begin
          if ci.Target = @System_FinalizeUnits then begin
            if (ci.TargetSize = 4) and ci.RelTarget then begin
              {$ifdef log}log('hook System.FinalizeUnits'' call to FinalizeUnits');{$endif}
              PatchInt(ci.PTarget, integer(@InterceptFinalizeUnits) - integer(ci.Next));
            end;
            break;
          end;
          ci := ParseCode(ci.Next);
        end;
      end;
      ci := ParseCode(@TObject.Free);
      if ci.Target <> nil then
           p1 := ci.Target
      else p1 := @TObject.Free;
      ci := ParseCode(System_DoneExcept);
      while not (ci.Opcode in [$C2, $C3]) do begin
        if ci.Target = p1 then begin
          if (ci.TargetSize = 4) and ci.RelTarget then begin
            {$ifdef log}log('hook System.DoneExcept''s call to TObject.Free');{$endif}
            PatchInt(ci.PTarget, integer(@InterceptFreeExceptObject) - integer(ci.Next));
          end;
          break;
        end;
        ci := ParseCode(ci.Next);
      end;
      ci := ParseCode(System_HandleAnyException);
      while ci.IsValid and
            ((ci.Opcode <> $8b) or (ci.ModRm <> $15) or (TPPointer(dword(ci.This) + 2)^ <> @ExceptObjProc)) and
            (not (ci.Opcode in [$c2, $c3])) do
        ci := ParseCode(ci.Next);
      if ci.Opcode = $8b then begin
        {$ifdef log}log('hook System.HandleAnyException''s call to ExceptObjProc');{$endif}
        PatchInt(pointer(dword(ci.This) + 2), integer(@PMyExceptObjProc1));
      end;
      ci := ParseCode(System_HandleOnException);
      while ci.IsValid and
            ((ci.Opcode <> $ff) or (ci.ModRm <> $15) or (TPPointer(dword(ci.This) + 2)^ <> @ExceptObjProc)) and
            (not (ci.Opcode in [$c2, $c3])) do
        ci := ParseCode(ci.Next);
      if ci.Opcode = $ff then begin
        {$ifdef log}log('hook System.HandleOnException''s call to ExceptObjProc');{$endif}
        PatchInt(pointer(dword(ci.This) + 2), integer(@PMyExceptObjProc2));
      end;
      ci := ParseCode(System_RaiseExcept);
      {$ifdef d6}
        while ci.IsValid and
              ((ci.Opcode <> $ff) or (ci.ModRm <> $25)) and
              (not (ci.Opcode in [$c2, $c3])) do
          ci := ParseCode(ci.Next);
        if ci.Opcode = $ff then begin
          {$ifdef log}log('hook System.RaiseExcept''s call to RaiseExceptionProc');{$endif}
          PatchInt(pointer(dword(ci.This) + 2), integer(@PMyRaiseExceptProc));
        end;
      {$else}
        while ci.IsValid and (ci.Opcode <> $e9) and (not (ci.Opcode in [$c2, $c3])) do
          ci := ParseCode(ci.Next);
        if ci.Opcode = $e9 then begin
          {$ifdef log}log('hook System.RaiseExcept''s call to RaiseException');{$endif}
          PatchInt(pointer(dword(ci.This) + 1), integer(dword(@MyRaiseExceptProc) - dword(ci.Next)));
        end;
      {$endif}
      p1 := System_HandleAutoException;
      ci := ParseCode(p1);
      while ci.IsValid and (not ci.Call) do
        ci := ParseCode(ci.Next);
      System_FpuInit := ci.Target;
      PatchJmp(p1, @InterceptHandleAutoException);
      InitHandleExceptionThread;
      InitFireHandlers;
      InitCreateThreadHook(mySettings);
      {$ifdef bcb}HookBcbExceptions(hookPackages);{$endif}
      if mySettings.CheckForFreeze then begin
        FreezeTimeout := mySettings.FreezeTimeout;
        InitAntiFreeze;
      end;
      for c1 := low(OutOfMemoryInsurance[false]) to high(OutOfMemoryInsurance[false]) do begin
        OutOfMemoryInsurance[false, c1] := VirtualAlloc(nil, 4096 * 1024, MEM_RESERVE, PAGE_NOACCESS);
        OutOfMemoryInsurance[true,  c1] := VirtualAlloc(nil,  512 * 1024, MEM_COMMIT,  PAGE_NOACCESS);
      end;
    end;
    if @InterceptHttpExtensionProc = nil then ;
  end else begin
    // madExcept is not enabled, but let's at least initialize a bit
    tl := GetThreadList;
    if tl <> nil then
         ProcessMainThreadId := tl[0]
    else ProcessMainThreadId := MainThreadId;
    mySettings := nil;
  end;
  {$ifdef log}unindentLog;{$endif}
end;

procedure Close;
var section : PRTLCriticalSection;
    c1      : dword;
    b1      : boolean;
begin
  {$ifdef log}
    if madExceptBuf <> nil then
         log('Close, module: ' + ModuleName(HInstance) + ', ' + 'refCount: ' + IntToStrEx(madExceptBuf^.referenceCount))
    else log('Close, module: ' + ModuleName(HInstance));
    indentLog;
  {$endif}
  if meSettingsBuf <> nil then begin
    UninstallUnhandledExceptionFilter;
    try
      UndoPatches;
      PatchList := nil;
    except end;
    CloseAntiFreeze;
    CloseMessageHooks;
    CloseCreateThreadHook;
    CloseFireHandlers;
    CloseHandleExceptionThread;
    if InterlockedDecrement(madExceptBuf^.referenceCount) = 0 then begin
      SetThreadInfo(GetCurrentThreadId, 0, '', '', nil, 0);
      CloseHandle(madExceptBuf^.mapHandle       );
      CloseHandle(madExceptBuf^.antiFreezeMutex );
      CloseHandle(madExceptBuf^.threadNamesMutex);
    end;
    UnmapViewOfFile(madExceptBuf);
    madExceptBuf := nil;
    if MainThreadSE <> 0 then begin
      c1 := MainThreadSE;
      MainThreadSE := 0;
      SetEvent(c1);
      CloseHandle(c1);
    end;
    if OtherThreadsSE <> 0 then begin
      c1 := OtherThreadsSE;
      OtherThreadsSE := 0;
      SetEvent(c1);
      CloseHandle(c1);
    end;
    if SuspendSection <> nil then begin
      section := SuspendSection;
      SuspendSection := nil;
      DeleteCriticalSection(section^);
      Dispose(section);
    end;
    for b1 := low(boolean) to high(boolean) do
      for c1 := low(OutOfMemoryInsurance[b1]) to high(OutOfMemoryInsurance[b1]) do
        if OutOfMemoryInsurance[b1, c1] <> nil then begin
          VirtualFree(OutOfMemoryInsurance[b1, c1], 0, MEM_RELEASE);
          OutOfMemoryInsurance[b1, c1] := nil;
        end;
  end;
  if HandlerSection <> nil then begin
    section := HandlerSection;
    HandlerSection := nil;
    DeleteCriticalSection(section^);
    Dispose(section);
  end;
  if IRSection <> nil then begin
    section := IRSection;
    IRSection := nil;
    DeleteCriticalSection(section^);
    Dispose(section);
  end;
  if meSettingsBuf <> nil then
    UnlistMeSettingsModule(HInstance);
  {$ifdef log}unindentLog;{$endif}
end;

// ***************************************************************

initialization
  IsMultiThread := true;
  {$ifdef log}log('initialization'); indentLog;{$endif}
  InitTryExceptFinally;
  CheckRestart;
  ClearTempPath;
  Init;
  {$ifdef log}unindentLog;{$endif}
finalization
  {$ifdef log}log('finalization'); indentLog;{$endif}
  try
    Close;
  except end;
  {$ifdef log}unindentLog;{$endif}
end.
