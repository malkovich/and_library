// ***************************************************************
//  madExceptWizard.pas       version:  3.0c  ·  date: 2006-05-21
//  -------------------------------------------------------------
//  IDE integration
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2006 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2006-05-21 3.0c (1) BCB2006 define "madExcept" is set now
//                 (2) little improvement for BCB2006 DLL projects
//                 (3) undocumented MES file option "MadExcept2Only" added
//                 (4) BCB2006 MES options sometimes didn't get saved
// 2006-01-05 3.0b support for BCB2006's "Build Configuration" added
// 2005-11-12 3.0a (1) assistant checkboxes incorrectly showed "some text"
//                 (2) assistant creator: testing forms sometimes crashed
//                 (3) multiple attachments were not saved correctly
//                 (4) several changes for big fonts
//                 (5) "AbortBtn" renamed to "CancelBtn"
//                 (6) "Visible" property added for main buttons
//                 (7) "include min. debug info" forces "show addr offset" on
//                 (8) support for recursive environment variables added
//                 (9) some minor improvements for better BCB support
// 2005-07-17 3.0  countless changes for madExcept 3.0
// 2005-07-17 2.7f (1) ".cpp" now also accepted as a project name (bcb support)
//                 (2) output dir for package projects was handled incorrectly
//                 (3) unit init order patching removed completely
//                 (4) mad* units are added after MM units in project uses
// 2005-01-30 2.7e superfluous Delphi 2005 warning removed
// 2004-11-15 2.7d warnings are shown now, if uses clause modification fails
// 2004-10-03 2.7c various changes for Delphi 9 support (win32 only)
// 2004-07-11 2.7b (1) support for "ListHardware" option added
//                 (2) ".bat" in project group made problems in D4 and D5
//                 (3) optional "relative" bug report infos renamed to "offset"
//                 (4) external apps can issue a "jump to unit/line" command
//                 (5) pause all running thread -> pause all delphi/bcb threads
//                 (6) patch errors weren't posted to compiler message window
// 2004-04-12 2.7a (1) parsing of very big project "uses" clauses failed
//                 (2) BCB: library BPL output dir was incorrectly ignored
//                 (3) special handling to not destroy $(BCB) in include path
//                 (4) ExceptMsg + FrozenMsg max length increased to 255 chars
//                 (5) BCB: madExcept now doesn't add "#include"s anymore
// 2004-03-07 2.7  (1) "ShowExceptionBox" off: mail options were not available
//                 (2) showRelativeLines option added
//                 (3) BCB: unit links are added after "#pragma hdrstop" now
//                 (4) ScreenShot support added
//                 (5) unit init order patching can be disabled now
//                 (6) special case: compilation of "pas" file without project
//                 (7) automated support for IntraWeb 5 - 7 added
// 2003-11-16 2.6  (1) project close event gets handled more safely now
//                 (2) madExcept is automatically added to the bpr file in BCB
//                 (3) project include/library paths in BCB are adjusted
//                 (4) additional BCB project options are controlled now
//                 (5) "Local Symbols" compiler option is controlled now
//                 (6) settings are not restored to the old state anymore
//                 (7) BCB binary file name detection (e.g. project1.dll) added
//                 (8) the wizard package exports a "ReloadMesFile" API now
//                 (9) ExceptMsg string size increased to 200 characters
//                 (a) madExcept "Synchronize" option removed
//                 (b) autoMail + autoClip + autoContinue options added
//                 (c) showRelativeAddrs option added
//                 (d) a whole bunch of new mailing options added
//                 (e) help window is not shown maximized anymore
//                 (f) HKxx\Software\madshi\madExcept\dontTouchUses + Defines
// 2003-06-09 2.5  (1) settings dialog: bug report tab with new options added
//                 (2) little initialization change makes D4/5 IDE more stable
//                 (3) the wizard now takes full control of the option
//                     "project options -> compiler -> debug infos" (D5+)
//                 (4) settings dialog now shows version number
//                 (5) project groups now don't open all forms anymore
//                 (6) some problems with the auto "uses" adjustments fixed
//                 (7) freeze timeout changed from ms to sec
// 2002-12-03 2.3a unit initialization order option added in order to fix
//                 problems with memory management replacements like ShareMem
// 2002-11-05 2.3  (1) "madExceptIde" renamed to "madExceptWizard"
//                 (2) madExcept gets added to project's uses clause
//                 (3) some first steps of BCB support (not ready yet)
//                 (4) D4: handling of project's map file setting changed
// 2002-10-22 2.2b AfterCompile "succeeded" flag seems to not be reliable in D5
// 2002-10-19 2.2a (1) settings dialog: "Abort" button renamed to "Cancel"
//                 (2) $(EnvironmentStrings) are correctly handled now
//                 (3) old "try..except" statement killed from "AfterCompile"
// 2002-10-12 2.2  (1) delphi 7 support
//                 (2) the exception box's title bar is now freely adjustable
//                 (3) logging of unit initialization order index was wrong
//                 (4) no automatic integration question at first start, anymore
//                 (5) perfect support of project groups, was weak until now
//                 (6) got totally rid of the old toolsAPI interfaces ->
//                     so in D5 we know now have the IsCodeInsight flag
//                 (7) activate logging now by creating "c:\madExceptIde.txt"
//                 (8) FLastBinarySize added to solve fast Ctrl-F9 + F9 problem
//                 (9) the map file stream is now added as a resource
//                     -> this way exe packers don't trash the map file stream
//                     -> the stream can be loaded through resource APIs
//                 (a) uncompressed "map" file -> compressed "mad" file
//                 (b) only "append map file to binary" enabled for dpk projects
//                 (c) ico/bmp files like "meiMail.ico" in the project's root
//                     folder automatically replace madExcept's default images
//                 (d) wizard exceptions are now shown in the madExcept box
// 2002-06-14 2.1d instead of entry point patching the initialization order of
//                 the units SysUtils/madTools/madExcept is now manipulated
//                 this moves some hacks out of the binary into the IDE wizard
//                 and more exceptions get caught now during finalization
// 2002-03-15 2.1c just another bug in CloseProjectSaveNotifier fixed <sigh>
// 2002-03-08 2.1b bug in CloseMadExceptIde/CloseProjectSaveNotifier fixed
// 2002-02-24 2.1a (1) map file behaviour changed:
//                     D4  : detailed map file always enabled, file NOT deleted
//                     D5+ : map file behaves as if madExceptIde would not exist
//                 (2) mes file is only saved/created when it makes sense
// 2002-02-08 2.1  (1) madExcept now also runs 100% fine with runtime packages
//                 (2) "save bug report" button added + auto save option
//                 (3) email address max length increased to 75
//                 (4) email subject max length increased to 50
// 2001-12-12 2.0c project option PackageDLLOutputDir is now supported
// 2001-09-27 2.0b turning freeze check off didn't work
// 2001-08-03 2.0a got rid of mailBody ("please press Ctrl+V" message)
// 2001-07-22 2.0  (1) menu item opens settings dialog
//                 (2) *.amf -> *.mes ([m]ad[E]xcept[S]ettings)
//                 (3) all madExcept options can be configured in the dialog
// 2001-07-10 1.7c (1) this is a new design package featuring the except wizard
//                 (2) delphi 6 compatible
// madExceptVcl history:
// 2001-06-10 1.7b expert now initializes "madExcept" function vars
// 2001-05-31 1.7a (1) expert now automatically changes map file settings
//                 (2) "append" setting gets saved together with project options
// 2001-04-30 1.7  entry point manipulation: automatic madExcept initialization

unit madExceptWizard;

{$I mad.inc}

{$ifndef ver120}{$ifndef ver130}{$define d6}{$endif}{$endif}

{ $define TestProjectOptions}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, ComCtrls,
  {$ifdef TestProjectOptions} TypInfo, {$endif} ShellAPI, Dialogs, Buttons,
  CheckLst, madSideTabs, madExceptPatcher, madTypes, meAddComp,
  ImgList, meGlobalCfg, Menus;

// ***************************************************************

{$define td32}  // support td32 debug infos? enabled by default (bcb support)

// ***************************************************************

type
  TDfmTyp = (dtInt, dtBool, dtStr, dtEnum);
  TDfmValue = record
    name  : string;
    typ   : TDfmTyp;
    vInt  : integer;
    vBool : boolean;
    vStr  : string;
    list  : string;  // list of options for dtEnum
    prep  : string;  // prepend string for dtEnum
  end;
  TDfmObject = record
    name  : string;
    typ   : string;
    props : array of TDfmValue;
  end;
  TDfm = array of TDfmObject;

  TFMadExceptProjectSettings = class(TForm)
    EnableMeCheck: TCheckBox;
    BasicPanel: TPanel;
    OkBtn: TButton;
    CancelBtn: TButton;
    MainShape: TShape;
    TabPanel: TPanel;
    Label4: TLabel;
    FreezeTimeoutEdit: TEdit;
    AntiFreezeCheck: TCheckBox;
    BitFaultCheck: TCheckBox;
    AutoPanel: TPanel;
    Shape3: TShape;
    Label2: TLabel;
    Shape4: TShape;
    Label5: TLabel;
    AutoClipCheck: TCheckBox;
    SuspendCheck: TCheckBox;
    AutoCloseCheck: TCheckBox;
    AutoRestartCheck: TCheckBox;
    AutoContinueCheck: TCheckBox;
    RestartCombo: TComboBox;
    CloseCombo: TComboBox;
    AutoSaveCheck: TCheckBox;
    AutoSendCheck: TCheckBox;
    PleaseWaitCheck: TCheckBox;
    Shape5: TShape;
    Label3: TLabel;
    Shape8: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape9: TShape;
    Shape10: TShape;
    Shape11: TShape;
    Shape12: TShape;
    Shape13: TShape;
    Shape14: TShape;
    Shape15: TShape;
    Shape16: TShape;
    Shape17: TShape;
    Shape2: TShape;
    Shape18: TShape;
    Shape19: TShape;
    Shape20: TShape;
    Shape21: TShape;
    Shape26: TShape;
    Shape27: TShape;
    Shape30: TShape;
    Shape31: TShape;
    Shape32: TShape;
    Shape33: TShape;
    Shape34: TShape;
    Shape35: TShape;
    LayoutPanel: TPanel;
    Shape38: TShape;
    Shape39: TShape;
    Shape41: TShape;
    Label7: TLabel;
    Shape42: TShape;
    Shape43: TShape;
    Shape44: TShape;
    Shape45: TShape;
    Label8: TLabel;
    SendFocus: TRadioButton;
    AutoShowBugRepCheck: TCheckBox;
    SendBugRepCheck: TCheckBox;
    SaveBugRepCheck: TCheckBox;
    PrintBugRepCheck: TCheckBox;
    ShowBugRepCheck: TCheckBox;
    ContinueAppCheck: TCheckBox;
    RestartAppCheck: TCheckBox;
    CloseAppCheck: TCheckBox;
    SaveFocus: TRadioButton;
    PrintFocus: TRadioButton;
    ShowFocus: TRadioButton;
    ContinueFocus: TRadioButton;
    RestartFocus: TRadioButton;
    CloseFocus: TRadioButton;
    SendImg: TPaintBox;
    SaveImg: TPaintBox;
    PrintImg: TPaintBox;
    ShowImg: TPaintBox;
    ContinueImg: TPaintBox;
    RestartImg: TPaintBox;
    CloseImg: TPaintBox;
    BigImg: TPaintBox;
    ShowUglyBtnsCheck: TCheckBox;
    SendPanel: TPanel;
    Shape50: TShape;
    Shape51: TShape;
    Shape52: TShape;
    Label16: TLabel;
    Shape53: TShape;
    Shape54: TShape;
    Shape55: TShape;
    Shape56: TShape;
    MailAddrLabel: TLabel;
    SendServerLabel: TLabel;
    SendAccountLabel: TLabel;
    SendPasswordLabel: TLabel;
    SendPortLabel: TLabel;
    MailAddrEdit: TEdit;
    DirectCheck: TCheckBox;
    MailToCheck: TCheckBox;
    MapiCheck: TCheckBox;
    SendServerEdit: TEdit;
    UseAuthCheck: TCheckBox;
    SendAccountEdit: TEdit;
    SendPasswordEdit: TEdit;
    SendPortEdit: TEdit;
    Shape57: TShape;
    Shape60: TShape;
    Shape61: TShape;
    Label17: TLabel;
    Shape62: TShape;
    Shape63: TShape;
    Shape64: TShape;
    Shape65: TShape;
    Shape58: TShape;
    Shape59: TShape;
    Shape66: TShape;
    Label20: TLabel;
    Shape67: TShape;
    Shape68: TShape;
    Shape69: TShape;
    Shape70: TShape;
    SavePanel: TPanel;
    Shape71: TShape;
    Shape72: TShape;
    Shape73: TShape;
    Label21: TLabel;
    Shape74: TShape;
    Shape75: TShape;
    Shape76: TShape;
    Shape77: TShape;
    BugRepFileLabel: TLabel;
    BugRepFileEdit: TEdit;
    BugRepFileSizeEdit: TEdit;
    AppendBugRepCheck: TCheckBox;
    Shape78: TShape;
    Shape79: TShape;
    Shape80: TShape;
    Label27: TLabel;
    Shape81: TShape;
    Shape82: TShape;
    Shape83: TShape;
    Shape84: TShape;
    NoDupExceptCheck: TCheckBox;
    DupExceptLabel: TLabel;
    NoDupFreezeCheck: TCheckBox;
    ReportPanel: TPanel;
    Shape85: TShape;
    Shape86: TShape;
    Shape87: TShape;
    Label29: TLabel;
    Shape88: TShape;
    Shape89: TShape;
    Shape90: TShape;
    Shape91: TShape;
    ListThreadsCheck: TCheckBox;
    DisassemblyCheck: TCheckBox;
    FormatDisCheck: TCheckBox;
    LimitDisCheck: TCheckBox;
    LimitDisCombo: TComboBox;
    HideUglyItemsCheck: TCheckBox;
    ShowRelAddrsCheck: TCheckBox;
    ShowRelLinesCheck: TCheckBox;
    Shape92: TShape;
    Shape95: TShape;
    Shape96: TShape;
    Label30: TLabel;
    Shape97: TShape;
    Shape98: TShape;
    Shape99: TShape;
    Shape100: TShape;
    Shape94: TShape;
    Shape101: TShape;
    Shape102: TShape;
    Label32: TLabel;
    Shape103: TShape;
    Shape104: TShape;
    Shape105: TShape;
    Shape106: TShape;
    Shape93: TShape;
    Shape107: TShape;
    Shape108: TShape;
    Label31: TLabel;
    Shape109: TShape;
    Shape110: TShape;
    Shape111: TShape;
    Shape112: TShape;
    PluginList: TCheckListBox;
    Shape113: TShape;
    Shape114: TShape;
    Shape115: TShape;
    Label6: TLabel;
    Shape116: TShape;
    Shape117: TShape;
    Shape118: TShape;
    Shape119: TShape;
    DupFreezeLabel: TLabel;
    Panel1: TPanel;
    DupExcept1Radio: TRadioButton;
    DupExcept2Radio: TRadioButton;
    DupExcept3Radio: TRadioButton;
    Panel2: TPanel;
    DupFreeze1Radio: TRadioButton;
    DupFreeze2Radio: TRadioButton;
    DupFreeze3Radio: TRadioButton;
    SendAssisCombo: TComboBox;
    CantContinueImg: TPaintBox;
    SaveAssisCombo: TComboBox;
    PrintAssisCombo: TComboBox;
    BigIconLabel: TLabel;
    FilterPanel: TPanel;
    Shape120: TShape;
    Shape121: TShape;
    Shape122: TShape;
    Label35: TLabel;
    Shape123: TShape;
    Shape124: TShape;
    Shape125: TShape;
    Shape126: TShape;
    Filter1Label: TLabel;
    Filter1Edit: TEdit;
    Filter1NoBugRepCheck: TCheckBox;
    Filter1NoScreenshotCheck: TCheckBox;
    Filter1NoHandlersCheck: TCheckBox;
    Panel3: TPanel;
    Shape127: TShape;
    Shape128: TShape;
    Shape129: TShape;
    Label37: TLabel;
    Shape130: TShape;
    Shape131: TShape;
    Shape132: TShape;
    Shape133: TShape;
    Filter3NoBugRepCheck: TCheckBox;
    Filter3NoScreenshotCheck: TCheckBox;
    Filter3NoHandlersCheck: TCheckBox;
    Shape36: TShape;
    Shape37: TShape;
    Shape40: TShape;
    Shape46: TShape;
    Shape47: TShape;
    Shape48: TShape;
    Shape49: TShape;
    Shape134: TShape;
    Shape135: TShape;
    Shape136: TShape;
    Label38: TLabel;
    Shape137: TShape;
    Shape138: TShape;
    Shape139: TShape;
    Shape140: TShape;
    Filter2Label: TLabel;
    Filter2Edit: TEdit;
    Filter2NoBugRepCheck: TCheckBox;
    Filter2NoScreenshotCheck: TCheckBox;
    Filter2NoHandlersCheck: TCheckBox;
    AssisPanel: TPanel;
    Shape141: TShape;
    Shape142: TShape;
    Shape143: TShape;
    Label40: TLabel;
    Shape144: TShape;
    Shape145: TShape;
    Shape146: TShape;
    Shape147: TShape;
    AssisList: TListView;
    AssisAddBtn: TSpeedButton;
    AssisDelBtn: TSpeedButton;
    AssisImpBtn: TSpeedButton;
    AssisExpBtn: TSpeedButton;
    Shape148: TShape;
    Shape149: TShape;
    Shape150: TShape;
    Label41: TLabel;
    Shape151: TShape;
    Shape152: TShape;
    Shape153: TShape;
    Shape154: TShape;
    FormList: TListView;
    FormAddBtn: TSpeedButton;
    FormDelBtn: TSpeedButton;
    FormImpBtn: TSpeedButton;
    FormExpBtn: TSpeedButton;
    CompList: TListView;
    CompAddBtn: TSpeedButton;
    CompDelBtn: TSpeedButton;
    AssisPlayBtn: TSpeedButton;
    StringPanel: TPanel;
    Shape155: TShape;
    Shape156: TShape;
    Shape157: TShape;
    Label42: TLabel;
    Shape158: TShape;
    Shape159: TShape;
    Shape160: TShape;
    Shape161: TShape;
    StringList: TListView;
    Shape162: TShape;
    Shape163: TShape;
    Shape164: TShape;
    Label43: TLabel;
    Shape165: TShape;
    Shape166: TShape;
    Shape167: TShape;
    Shape168: TShape;
    StringMemo: TMemo;
    Shape169: TShape;
    Shape170: TShape;
    Shape171: TShape;
    Label44: TLabel;
    Shape172: TShape;
    Shape173: TShape;
    Shape174: TShape;
    Shape175: TShape;
    NoMesCheck: TCheckBox;
    IncludeMinDebugCheck: TCheckBox;
    DefaultCheck: TCheckBox;
    FormPlayBtn: TSpeedButton;
    PropBox: TScrollBox;
    PropPaint: TPaintBox;
    PropEdit: TEdit;
    PropBtn: TSpeedButton;
    CompImgs: TImageList;
    CompPopup: TPopupMenu;
    CompAddMen: TMenuItem;
    FormPopup: TPopupMenu;
    FormAddMen: TMenuItem;
    CompDownBtn: TSpeedButton;
    CompUpBtn: TSpeedButton;
    AssisPopup: TPopupMenu;
    AssisAddMen: TMenuItem;
    AssisChangeBtn: TSpeedButton;
    AssisDelMen: TMenuItem;
    N3: TMenuItem;
    AssisImpMen: TMenuItem;
    AssisExpMen: TMenuItem;
    N4: TMenuItem;
    AssisChangeMen: TMenuItem;
    N5: TMenuItem;
    AssisPlayMen: TMenuItem;
    FormDelMen: TMenuItem;
    N2: TMenuItem;
    FormImpMen: TMenuItem;
    FormExpMen: TMenuItem;
    N6: TMenuItem;
    FormPlayMen: TMenuItem;
    CompDelMen: TMenuItem;
    N1: TMenuItem;
    CompUpMen: TMenuItem;
    CompDownMen: TMenuItem;
    Filter1ShowCombo: TComboBox;
    Filter1AssisCombo: TComboBox;
    Panel4: TPanel;
    Filter2ShowCombo: TComboBox;
    Filter2AssisCombo: TComboBox;
    Panel5: TPanel;
    Filter3ShowCombo: TComboBox;
    Filter3AssisCombo: TComboBox;
    Filter1NoSuspendCheck: TCheckBox;
    Filter2NoSuspendCheck: TCheckBox;
    Filter3NoSuspendCheck: TCheckBox;
    PlWaitImg: TPaintBox;
    PleaseWaitIconLabel: TLabel;
    Send32Img: TPaintBox;
    Send32IconLabel: TLabel;
    AttachPanel: TPanel;
    Shape1: TShape;
    Shape22: TShape;
    Shape23: TShape;
    Label1: TLabel;
    Shape24: TShape;
    Shape25: TShape;
    Shape28: TShape;
    Shape29: TShape;
    AttachBugRepFileCheck: TCheckBox;
    DelBugRepFileCheck: TCheckBox;
    AttachScreenShotCheck: TCheckBox;
    ScreenShotCombo: TComboBox;
    SmtpServerRadio: TRadioButton;
    SmtpClientRadio: TRadioButton;
    HttpUploadRadio: TRadioButton;
    AutoSendPrgrBoxCheck: TCheckBox;
    Shape176: TShape;
    Shape177: TShape;
    Shape178: TShape;
    Label10: TLabel;
    Shape179: TShape;
    Shape180: TShape;
    Shape181: TShape;
    Shape182: TShape;
    AttachList: TListView;
    CaptureCurrentAppCheck: TCheckBox;
    DelAttachBtn: TSpeedButton;
    AddAttachBtn: TSpeedButton;
    EditAttachBtn: TSpeedButton;
    AutoSaveIfNotSentCheck: TCheckBox;
    BackgroundCheck: TCheckBox;
    AttachBugRepCheck: TCheckBox;
    CpuRegsCheck: TCheckBox;
    StackDumpCheck: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImgsPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckControls(Sender: TObject);
    procedure StringListCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure StringListDblClick(Sender: TObject);
    procedure StringMemoChange(Sender: TObject);
    procedure ImgsClick(Sender: TObject);
    procedure FormAddBtnClick(Sender: TObject);
    procedure FormListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure CompAddBtnClick(Sender: TObject);
    procedure CompListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure PropPaintPaint(Sender: TObject);
    procedure PropBtnClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure AcceptPropEdit(Sender: TObject = nil);
    procedure PropEditKeyPress(Sender: TObject; var Key: Char);
    procedure PropPaintMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PropEditDblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CompDelBtnClick(Sender: TObject);
    procedure CompListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CompListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDelBtnClick(Sender: TObject);
    procedure FormListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormPlayBtnClick(Sender: TObject);
    procedure CompUpBtnClick(Sender: TObject);
    procedure CompDownBtnClick(Sender: TObject);
    procedure AssisAddBtnClick(Sender: TObject);
    procedure AssisDelBtnClick(Sender: TObject);
    procedure AssisListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure AssisListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AssisListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AssisPlayBtnClick(Sender: TObject);
    procedure AssisChangeBtnClick(Sender: TObject);
    procedure SendAssisComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure Filter1ShowComboChange(Sender: TObject);
    procedure SmtpServerRadioClick(Sender: TObject);
    procedure StringListKeyPress(Sender: TObject; var Key: Char);
    procedure StringListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure AttachListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure DelAttachBtnClick(Sender: TObject);
    procedure AddAttachBtnClick(Sender: TObject);
    procedure EditAttachBtnClick(Sender: TObject);
    procedure AttachListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AttachListDblClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FSideTabs         : TSideTabs;
    FForms            : array of TDfm;
    FAssis            : TDAString;
    FSelProp          : string;
    FActiveProp       : array [0..2] of integer;
    FPropList         : TListBox;
    FCurIcon          : TPaintBox;
    FIcons            : array [0..10] of string;
    FHook             : dword;
    FIconPanel        : TPanel;
    FHookWndProc      : pointer;
    FSmtpServer       : string;
    FSmtpPort         : dword;
    FSmtpAccount      : string;
    FSmtpPassword     : string;
    FHttpServer       : string;
    FHttpPort         : dword;
    FHttpAccount      : string;
    FHttpPassword     : string;
    FBugReportSendAs  : string;
    FBugReportZip     : string;
    FScreenShotSendAs : string;
    FScreenShotZip    : string;
    FPropCol          : integer;
    procedure ReleaseIconPanel(var Message: TMessage); message WM_USER + 777;
    procedure ActivateWnd(var Message: TMessage); message WM_ACTIVATE;
    function  MouseHook(code, wParam, lParam: integer) : integer; stdcall;
    procedure IconPanelExit(Sender: TObject);
    procedure IconPanelClick(Sender: TObject);
    function  EditAssistant(var assis: string) : boolean;
    procedure CheckAssisButtons;
    procedure PropListKeyPress(Sender: TObject; var Key: Char);
    procedure ClosePropList(Sender: TObject = nil);
    procedure AcceptPropList(Sender: TObject = nil; Button: TMouseButton = mbLeft; Shift: TShiftState = []; X: Integer = 0; Y: Integer = 0);
  public
    { Public-Deklarationen }
    procedure FillAssisCombo (combo: TComboBox; assis: string = '*'; renameFrom: string = ''; renameTo: string = '');
    procedure FillAssisCombos (renameFrom: string = ''; renameTo: string = '');
    procedure SettingsToForm (const settings: TProject);
    procedure FormToSettings (var   settings: TProject);
    procedure CheckControlColor (parent: TWinControl);
  end;

// ***************************************************************

procedure Register;

implementation

{$R *.dfm}

uses madExcept, madStackTrace, madMapFile, madStrings, madDisAsm,
     CommCtrl, ToolsApi, madTools, madRes, madCrypt, madNVAssistant, meEditAssis,
     meEditAttach, madGraphics;

// ***************************************************************

var BCB : boolean = false;

procedure DelphiOrBCB;
var arrCh : array [0..MAX_PATH] of char;
begin
  GetModuleFileName(0, arrCh, MAX_PATH);
  BCB := PosText('bcb.exe', arrCh) > 0;
end;

// ***************************************************************

type
  TSettingsString = record
    name        : string;
    description : string;
    content     : string;
    offset      : integer;
  end;

var
  SettingsStrings : array [0..26] of TSettingsString =
    ( (name: 'TitleBar';                description: 'except box - title bar'),
      (name: 'ExceptionMessage';        description: 'except box - exception message'),
      (name: 'FrozenMessage';           description: 'except box - frozen message'),
      (name: 'BitFaultMsg';             description: 'except box - bit fault message'),
      (name: 'SendBugReportText';       description: 'except box - send button'),
      (name: 'SaveBugReportText';       description: 'except box - save button'),
      (name: 'PrintBugReportText';      description: 'except box - print button'),
      (name: 'ShowBugReportText';       description: 'except box - show button'),
      (name: 'ContinueApplicationText'; description: 'except box - continue button'),
      (name: 'RestartApplicationText';  description: 'except box - restart button'),
      (name: 'CloseApplicationText';    description: 'except box - close button'),
      (name: 'OkBtnText';               description: 'message boxes - ok button'),
      (name: 'DetailsBtnText';          description: 'message boxes - details button'),
      (name: 'PleaseWaitTitle';         description: 'please wait box - title bar'),
      (name: 'PleaseWaitText';          description: 'please wait box - text'),
      (name: 'MailSubject';             description: 'send - mail subject'),
      (name: 'MailBody';                description: 'send - mail body'),
      (name: 'SendBoxTitle';            description: 'send - send box title'),
      (name: 'PrepareAttachMsg';        description: 'send - prepare attachments'),
      (name: 'MxLookupMsg';             description: 'send - mx lookup'),
      (name: 'ConnectMsg';              description: 'send - server connect'),
      (name: 'AuthMsg';                 description: 'send - authentication'),
      (name: 'SendMailMsg';             description: 'send - send mail'),
      (name: 'FieldsMsg';               description: 'send - set http fields'),
      (name: 'SendAttachMsg';           description: 'send - sending attachments'),
      (name: 'SendFinalizeMsg';         description: 'send - finalization'),
      (name: 'MailFailureMsg';          description: 'send - failure message') );

type
  TFloatingListBox = class (TListBox)
    procedure CreateParams(var Params: TCreateParams); override;
  end;

procedure TFLoatingListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
//  Params.Style := (Params.Style and (not WS_CHILD)) or WS_POPUP;
//  Params.WndParent := TForm(Owner).Handle;
end;

procedure TFMadExceptProjectSettings.FormCreate(Sender: TObject);
var i1 : integer;
begin
  FPropCol := 80 * Screen.PixelsPerInch div 96;
  FHookWndProc := MethodToProcedure(Self, @TFMadExceptProjectSettings.MouseHook);
  FPropList := TFloatingListBox.Create(self);
  with FPropList do begin
    Visible := false;
    Parent := self;
    Left := ScreenToClient(PropBox.ClientToScreen(Point(FPropCol, 0))).X + 2;
    Ctl3D := false;
    Width := PropBox.Width - FPropCol - 4;
    OnKeyPress := PropListKeyPress;
    OnExit := ClosePropList;
    OnMouseUp := AcceptPropList;
  end;
  for i1 := 0 to high(Plugins) do
    PluginList.Items.Add(Plugins[i1].description);
end;

procedure TFMadExceptProjectSettings.FormDestroy(Sender: TObject);
begin
  if FHook <> 0 then begin
    UnhookWindowsHookEx(FHook);
    FHook := 0;
  end;
  if FHookWndProc <> nil then
    VirtualFree(FHookWndProc, 0, MEM_RELEASE);
  FSideTabs.Free;
end;

procedure TFMadExceptProjectSettings.ImgsPaint(Sender: TObject);

  function DrawGraphic(bmp: string; size, dc, backCol: dword; enabled: boolean) : boolean;
  var graphic  : dword;     // icon or bitmap handle
      bdc, obh : dword;     // bitmap dc, old bitmap handle
      i1       : cardinal;
      alpha    : dword;
      c1, c2   : dword;
      p1       : pointer;
      bi       : TBitmapInfo;
      pbih     : PBitmapInfoHeader;
      buf      : TDACardinal;
      pixel    : dword;
  begin
    result := false;
    if size >= 32 then
         size := 32
    else size := 16;
    SetLength(buf, size * size);
    if Length(bmp) < 200 then begin
      bmp := UpStr(bmp);
      c1 := FindResource(HInstance, pchar(bmp), RT_BITMAP);
      if (c1 <> 0) and (SizeOfResource(HInstance, c1) = sizeOf(pbih^) + size * size * 4) then begin
        c2 := LoadResource(HInstance, c1);
        if c2 <> 0 then begin
          pbih := LockResource(c2); Move(pbih^, bi, sizeOf(pbih^));
          inc(pbih);                Move(pbih^, buf[0], size * size * 4);
          UnlockResource(c2);
          FreeResource(c2);
          result := true;
        end;
      end;
    end else
      if Length(bmp) = sizeOf(pbih^) + integer(size * size * 4) then begin
        Move(bmp[1], bi, sizeOf(pbih^));
        Move(bmp[1 + sizeOf(pbih^)], buf[0], size * size * 4);
        result := true;
      end;
    if result then begin
      backCol := (backCol and $0000FF) shl 16 +    // swap rgb to bgr
                 (backCol and $00FF00)        +
                 (backCol and $FF0000) shr 16;
      for i1 := 0 to size * size - 1 do begin
        alpha := buf[i1] shr 24 + 1;
        pixel := buf[i1];
        if not enabled then begin
          pixel := (pixel and $00FF0000) shr 16 + (pixel and $0000FF00) shr 8 + pixel and $000000FF;
          pixel := pixel div 3;
          if pixel > $FF then
            pixel := $FF;
          pixel := pixel shl 16 + pixel shl 8 + pixel;
        end;
        buf[i1] := ( (      alpha  * (pixel   and $00FF00FF) +
                     (256 - alpha) * (backCol and $00FF00FF)   ) shr 8) and $00FF00FF +
                   ( (      alpha  * (pixel   and $0000FF00) +
                     (256 - alpha) * (backCol and $0000FF00)   ) shr 8) and $0000FF00;
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
          BitBlt(dc, 0, 0, size, size, bdc, 0, 0, SRCCOPY);
          SelectObject(bdc, obh);
          DeleteObject(graphic);
          result := true;
        end;
        DeleteDC(bdc);
      end;
    end;
  end;

begin
  with Sender as TPaintBox do
    if not DrawGraphic(FIcons[Tag], Width, Canvas.Handle, DecCol(GetSysColor(COLOR_BTNFACE), $080808, true), Enabled) then begin
      Canvas.Pen.Color   := DecCol(GetSysColor(COLOR_BTNFACE), $101010, false);
      Canvas.Brush.Color := DecCol(GetSysColor(COLOR_BTNFACE), $101010, true);
      Canvas.Rectangle(0, 0, Width, Height);
    end;
end;

procedure FillProps(var dfmObj: TDfmObject; defaultButton: boolean);

  procedure AddProp(var dfmObj: TDfmObject; name: string; typ: TDfmTyp;
                    vInt: integer; vBool: boolean; vStr, list: string; prep: string = '');
  var i1, i2, i3 : integer;
  begin
    i1 := Length(dfmObj.props);
    SetLength(dfmObj.props, i1 + 1);
    i3 := i1;
    for i1 := 0 to high(dfmObj.props) - 1 do
      if CompareText(name, dfmObj.props[i1].name) <= 0 then begin
        i3 := i1;
        break;
      end;
    for i2 := high(dfmObj.props) downto i3 + 1 do
      dfmObj.props[i2] := dfmObj.props[i2 - 1];
    dfmObj.props[i3].name  := name;
    dfmObj.props[i3].typ   := typ;
    dfmObj.props[i3].vInt  := vInt;
    dfmObj.props[i3].vBool := vBool;
    dfmObj.props[i3].vStr  := vStr;
    dfmObj.props[i3].list  := list;
    dfmObj.props[i3].prep  := prep;
  end;

var b1 : boolean;
begin
  b1 := true;
  dfmObj.props := nil;
  if not defaultButton then
    AddProp(dfmObj, 'Name',          dtStr,  0, false, dfmObj.name, '');
  if defaultButton and (dfmObj.typ <> 'INVButton') then begin
    b1 := false;
    AddProp(dfmObj, 'Name',          dtStr,  0, false, dfmObj.name, '');
    AddProp(dfmObj, 'Message',       dtStr,  0, false, 'some text', '');
    AddProp(dfmObj, 'MinWidth',      dtInt,  0, false, '', '');
    AddProp(dfmObj, 'ActiveControl', dtEnum, 0, false, '', '');
    AddProp(dfmObj, 'Timer',         dtInt,  0, false, '', '');
    AddProp(dfmObj, 'OnAction',      dtStr,  0, false, '', '');
  end else if dfmObj.typ = 'INVLabel' then begin
    AddProp(dfmObj, 'Caption',       dtStr,  0, false, 'some text', '');
  end else if dfmObj.typ = 'INVButton' then begin
    if defaultButton then begin
      b1 := false;
      AddProp(dfmObj, 'Enabled',       dtBool, 0, true,  '', '');
      AddProp(dfmObj, 'Caption',       dtStr,  0, false, Copy(dfmObj.name, 1, Length(dfmObj.name) - 3), '');
      AddProp(dfmObj, 'NoOwnerDraw',   dtBool, 0, false, '', '');
      AddProp(dfmObj, 'Visible',       dtBool, 0, true,  '', '');
    end else begin
      AddProp(dfmObj, 'Caption',       dtStr,  0, false, 'some text', '');
      AddProp(dfmObj, 'MinWidth',      dtInt,  0, false, '', '');
    end;
  end else if dfmObj.typ = 'INVEdit' then begin
    AddProp(dfmObj, 'Text',          dtStr,  0, false, '', '');
    AddProp(dfmObj, 'Lines',         dtInt,  1, false, '', '');
    AddProp(dfmObj, 'Optional',      dtBool, 0, false, '', '');
    AddProp(dfmObj, 'Colored',       dtBool, 0, true,  '', '');
    AddProp(dfmObj, 'Valid',         dtBool, 0, true,  '', '');
    AddProp(dfmObj, 'OutputType',    dtEnum, 0, false, 'nvoHeader', 'nvoHeader|nvoOwnSection', 'nvo');
    AddProp(dfmObj, 'OutputName',    dtStr,  0, false, '', '', '');
  end else if dfmObj.typ = 'INVCheckBox' then begin
    AddProp(dfmObj, 'Caption',       dtStr,  0, false, 'some text', '');
    AddProp(dfmObj, 'Checked',       dtBool, 0, false, '', '');
    AddProp(dfmObj, 'OutputName',    dtStr,  0, false, '', '', '');
  end else if dfmObj.typ = 'INVImage' then begin
    AddProp(dfmObj, 'Width',         dtInt,  0, false, '', '');
    AddProp(dfmObj, 'Height',        dtInt,  0, false, '', '');
    AddProp(dfmObj, 'File',          dtStr,  0, false, '', '');
    AddProp(dfmObj, 'Clickable',     dtBool, 0, false, '', '');
    AddProp(dfmObj, 'Border',        dtBool, 0, false, '', '');
  end;
  if b1 then begin
    AddProp(dfmObj, 'Enabled',       dtBool, 0, true,  '', '');
    AddProp(dfmObj, 'Spacing',       dtInt,  0, true,  '', '');
  end;
end;

function DfmToStruct(dfm: string) : TDfm;
var index : integer;

  procedure ConvertObject;

    procedure ConvertProperty(var dfmObj: TDfmObject);
    var i1, i2 : integer;
        s1     : string;
    begin
      SetString(s1, pchar(dfm) + index, ord(dfm[index]));
      inc(index, ord(dfm[index]) + 2);
      i2 := -1;
      for i1 := 0 to high(dfmObj.props) do
        if IsTextEqual(dfmObj.props[i1].name, s1) then begin
          i2 := i1;
          break;
        end;
      case dfm[index - 1] of
        #$4      : begin
                     if i2 <> -1 then begin
                       dfmObj.props[i2].typ  := dtInt;
                       dfmObj.props[i2].vInt := TPInteger(@dfm[index])^;
                     end;
                     inc(index, 4);
                   end;
        #$8, #$9 : if i2 <> -1 then begin
                     dfmObj.props[i1].typ   := dtBool;
                     dfmObj.props[i1].vBool := dfm[index - 1] = #$9;
                   end;
        #$c      : begin
                     if i2 <> -1 then begin
                       dfmObj.props[i1].typ := dtStr;
                       SetString(dfmObj.props[i1].vStr, pchar(dfm) + index + 3, TPInteger(@dfm[index])^);
                     end;
                     inc(index, 4 + TPInteger(@dfm[index])^);
                   end;
        #$7      : begin
                     if i2 <> -1 then begin
                       dfmObj.props[i1].typ := dtEnum;
                       SetString(dfmObj.props[i1].vStr, pchar(dfm) + index, ord(dfm[index]));
                     end;
                     inc(index, 1 + ord(dfm[index]));
                   end;
        else       raise MadException.Create('Invalid assistant form!');
      end;
    end;

  var i1 : integer;
  begin
    i1 := Length(result);
    SetLength(result, i1 + 1);
    SetString(result[i1].typ,  pchar(dfm) + index, ord(dfm[index]));
    inc(index, ord(dfm[index]) + 1);
    SetString(result[i1].name,  pchar(dfm) + index, ord(dfm[index]));
    if result[i1].name = 'AbortBtn' then
      result[i1].name := 'CancelBtn';
    inc(index, ord(dfm[index]) + 1);
    FillProps(result[i1], i1 < 4);
    while dfm[index] <> #0 do
      ConvertProperty(result[i1]);
    inc(index);
    while dfm[index] <> #0 do
      ConvertObject;
    inc(index);
  end;

begin
  result := nil;
  index := 5;
  if PosStrIs1('TPF0', dfm) then
    ConvertObject;
end;

function StructToDfm(const dfm: TDfm) : string;
var i1, i2 : integer;
    s1     : string;
begin
  result := 'TPF0';
  for i1 := 0 to high(dfm) do begin
    if i1 = 0 then
         s1 := 'TME' + dfm[i1].name
    else s1 := dfm[i1].typ;
    result := result + chr(Length(s1          )) + s1 +
                       chr(Length(dfm[i1].name)) + dfm[i1].name;
    for i2 := 0 to high(dfm[i1].props) do
      with dfm[i1].props[i2] do
        if (name <> 'Name') and ((typ <> dtEnum) or (vStr <> '')) then begin
          result := result + chr(Length(name)) + name;
          case typ of
            dtInt  : begin
                       result := result + #$4 + #0#0#0#0;
                       TPInteger(@result[Length(result) - 3])^ := vInt;
                     end;
            dtBool : if vBool then
                          result := result + #$9
                     else result := result + #$8;
            dtStr  : begin
                       result := result + #$c + #0#0#0#0;
                       TPInteger(@result[Length(result) - 3])^ := Length(vStr);
                       result := result + vStr;
                     end;
            dtEnum : result := result + #$7 + chr(Length(vStr)) + vStr;
          end;
        end;
    result := result + #0;
    if i1 > 0 then
      result := result + #0;
  end;
  result := result + #0;
end;

procedure TFMadExceptProjectSettings.FillAssisCombo(combo: TComboBox; assis: string = '*'; renameFrom: string = ''; renameTo: string = '');
var i1 : integer;
begin
  if (assis = '*') and (combo.ItemIndex > 0) then begin
    assis := combo.Items[combo.ItemIndex];
    if (renameFrom <> '') and IsTextEqual(renameFrom, assis) then
      assis := renameTo;
  end;
  combo.Clear;
  combo.Items.Add('--- no assistant ---');
  combo.ItemIndex := 0;
  for i1 := 0 to high(FAssis) do begin
    combo.Items.Add(SubStr(FAssis[i1], 1));
    if IsTextEqual(SubStr(FAssis[i1], 1), assis) then
      combo.ItemIndex := i1 + 1;
  end;
end;

procedure TFMadExceptProjectSettings.FillAssisCombos(renameFrom: string = ''; renameTo: string = '');
var b2 : boolean;
begin
  FillAssisCombo(   sendAssisCombo, '*', renameFrom, renameTo);
  FillAssisCombo(   saveAssisCombo, '*', renameFrom, renameTo);
  FillAssisCombo(  printAssisCombo, '*', renameFrom, renameTo);
  FillAssisCombo(filter1AssisCombo, '*', renameFrom, renameTo);
  FillAssisCombo(filter2AssisCombo, '*', renameFrom, renameTo);
  FillAssisCombo(filter3AssisCombo, '*', renameFrom, renameTo);
  b2 := EnableMECheck.Checked and (not noMesCheck.Checked) and (FAssis <> nil);
     sendAssisCombo.Enabled := b2 and sendBugRepCheck.Checked;
     saveAssisCombo.Enabled := b2 and saveBugRepCheck.Checked;
    printAssisCombo.Enabled := b2 and printBugRepCheck.Checked;
  filter1AssisCombo.Enabled := b2 and (filter1Edit.Text <> '') and (filter1ShowCombo.ItemIndex = 1);
  filter2AssisCombo.Enabled := b2 and (filter2Edit.Text <> '') and (filter2ShowCombo.ItemIndex = 1);
  filter3AssisCombo.Enabled := b2 and                              (filter3ShowCombo.ItemIndex = 1);
  CheckControlColor(self);
end;

procedure TFMadExceptProjectSettings.SettingsToForm(const settings: TProject);
var i1, i2 : integer;
    s1     : string;
begin
  with settings do begin
     defaultCheck.Checked := FPrj =  nil;
     defaultCheck.Enabled := (FPrj <> nil) and (not MadExcept2Only);
    enableMECheck.Checked := Enabled;
    enableMECheck.Enabled := not MadExcept2Only;
    FBugReportSendAs  := BugReportSendAs;
    FBugReportZip     := BugReportZip;
    FScreenShotSendAs := ScreenShotSendAs;
    FScreenShotZip    := ScreenShotZip;
    FSmtpServer   := SmtpServer;
    FSmtpPort     := SmtpPort;
    FSmtpAccount  := SmtpAccount;
    FSmtpPassword := SmtpPassword;
    FHttpServer   := HttpServer;
    FHttpPort     := HttpPort;
    FHttpAccount  := HttpAccount;
    FHttpPassword := HttpPassword;
    if FHttpServer <> '' then
      HttpUploadRadio.Checked := true
    else
      if FSmtpServer <> '' then
        SmtpClientRadio.Checked := true
      else
        SmtpServerRadio.Checked := true;
    SetLength(FAssis, Length(Assistants));
    for i1 := 0 to high(FAssis) do begin
      FAssis[i1] := Assistants[i1];
      with AssisList.Items.Add do begin
        Caption := SubStr(FAssis[i1], 1);
        SubItems.Add(SubStr(FAssis[i1], 2));
        s1 := '';
        for i2 := 3 to SubStrCount(FAssis[i1]) do
          s1 := s1 + ', ' + SubStr(FAssis[i1], i2);
        System.Delete(s1, 1, 2);
        SubItems.Add(s1);
      end;
    end;
    SetLength(FForms, Length(Forms));
    for i1 := 0 to high(FForms) do begin
      FForms[i1] := DfmToStruct(Forms[i1]);
      with FormList.Items.Add do begin
        Caption := FForms[i1][0].name;
        ImageIndex := 1;
      end;
    end;
    FillAssisCombo(   sendAssisCombo, SendAssistant);
    FillAssisCombo(   saveAssisCombo, SaveAssistant);
    FillAssisCombo(  printAssisCombo, PrintAssistant);
    FillAssisCombo(filter1AssisCombo, Filter1Assis);
    FillAssisCombo(filter2AssisCombo, Filter2Assis);
    FillAssisCombo(filter3AssisCombo, GeneralAssis);
     includeMinDebugCheck.Checked := MinDebugInfoOnly;
               noMesCheck.Checked := NoOwnSettings;
            bitFaultCheck.Checked := CheckFileCrc;
          antiFreezeCheck.Checked := CheckForFreeze;
        freezeTimeoutEdit.Text    := IntToStr((FreezeTimeout + 500) div 1000);
            autoSaveCheck.Checked := AutoSave;
   autoSaveIfNotSentCheck.Checked := AutoSaveIfNotSent;
            autoSendCheck.Checked := AutoSend;
     autoSendPrgrBoxCheck.Checked := AutoSendPrgrBox;
            autoClipCheck.Checked := AutoClipboard;
             suspendCheck.Checked := SuspendThreads;
          pleaseWaitCheck.Checked := ShowPleaseWaitBox;
        autoContinueCheck.Checked := AutoContinue;
         autoRestartCheck.Checked := AutoRestart <> 0;
           autoCloseCheck.Checked := AutoClose <> 0;
          sendBugRepCheck.Checked := SendBtnVisible;
          saveBugRepCheck.Checked := SaveBtnVisible;
         printBugRepCheck.Checked := PrintBtnVisible;
          showBugRepCheck.Checked := ShowBtnVisible;
         continueAppCheck.Checked := ContinueBtnVisible;
          restartAppCheck.Checked := RestartBtnVisible;
            closeAppCheck.Checked := CloseBtnVisible;
      autoShowBugRepCheck.Checked := AutoShowBugReport;
        showUglyBtnsCheck.Checked := NoOwnerDrawButtons;
             mailAddrEdit.Text    := MailAddr;
        attachBugRepCheck.Checked := AttachBugReport;
    attachBugRepFileCheck.Checked := AttachBugReportFile;
       delBugRepFileCheck.Checked := DeleteBugReportFile;
    attachScreenShotCheck.Checked := ScreenShotDepth > 0;
   captureCurrentAppCheck.Checked := ScreenShotAppOnly;
              directCheck.Checked := MailAsSmtpServer or MailAsSmtpClient or UploadViaHttp;
  if directCheck.Checked then begin
          smtpServerRadio.Checked := MailAsSmtpServer;
          smtpClientRadio.Checked := MailAsSmtpClient;
          httpUploadRadio.Checked := UploadViaHttp;
  end;
          backgroundCheck.Checked := SendInBackground;        
                mapiCheck.Checked := MailViaMapi;
              mailtoCheck.Checked := MailViaMailto;
           bugRepFileEdit.Text    := BugReportFile;
        appendBugRepCheck.Checked := AppendBugReports;
       bugRepFileSizeEdit.Text    := IntToStr((BugReportFileSize + 512) div 1000);
         noDupExceptCheck.Checked := NoDupExcepts;
         noDupFreezeCheck.Checked := NoDupFreezes;
         listThreadsCheck.Checked := ListThreads;
         disassemblyCheck.Checked := ShowDisAsm;
             cpuRegsCheck.Checked := ShowCpuRegisters;
           stackDumpCheck.Checked := ShowStackDump;
       hideUglyItemsCheck.Checked := HideUglyItems;
        showRelAddrsCheck.Checked := ShowRelativeAddrs or MinDebugInfoOnly;
        showRelLinesCheck.Checked := ShowRelativeLines;
           formatDisCheck.Checked := FormatDisassembly;
            limitDisCheck.Checked := LimitDisassembly <> 0;
              filter1Edit.Text    := Filter1Classes;
              filter2Edit.Text    := Filter2Classes;
     filter1NoBugRepCheck.Checked := Filter1NoBugReport;
     filter2NoBugRepCheck.Checked := Filter2NoBugReport;
     filter3NoBugRepCheck.Checked := GeneralNoBugReport;
 filter1NoScreenshotCheck.Checked := Filter1NoScreenshot;
 filter2NoScreenshotCheck.Checked := Filter2NoScreenshot;
 filter3NoScreenshotCheck.Checked := GeneralNoScreenshot;
    filter1NoSuspendCheck.Checked := Filter1NoSuspend;
    filter2NoSuspendCheck.Checked := Filter2NoSuspend;
    filter3NoSuspendCheck.Checked := GeneralNoSuspend;
   filter1NoHandlersCheck.Checked := Filter1NoHandlers;
   filter2NoHandlersCheck.Checked := Filter2NoHandlers;
   filter3NoHandlersCheck.Checked := GeneralNoHandlers;
    case AutoRestart of
             0           : restartCombo.ItemIndex := 3;
             1           : restartCombo.ItemIndex := 0;
             2 ..  03*60 : restartCombo.ItemIndex := 1;
        3*60+1 ..  15*60 : restartCombo.ItemIndex := 2;
       15*60+1 .. 120*60 : restartCombo.ItemIndex := 3;
      120*60+1 .. 720*60 : restartCombo.ItemIndex := 4;
      else                 restartCombo.ItemIndex := 5;
    end;
    case AutoClose of
             0           : closeCombo.ItemIndex := 1;
             1           : closeCombo.ItemIndex := 0;
             2 ..  03*60 : closeCombo.ItemIndex := 1;
        3*60+1 ..  15*60 : closeCombo.ItemIndex := 2;
       15*60+1 .. 120*60 : closeCombo.ItemIndex := 3;
      120*60+1 .. 720*60 : closeCombo.ItemIndex := 4;
      else                 closeCombo.ItemIndex := 5;
    end;
    if              SendBtnIcon = ''  then FIcons[        sendImg.Tag] := 'send1'
    else if         SendBtnIcon = '-' then FIcons[        sendImg.Tag] := ''
    else                                   FIcons[        sendImg.Tag] := SendBtnIcon;
    if              SaveBtnIcon = ''  then FIcons[        saveImg.Tag] := 'save1'
    else if         SaveBtnIcon = '-' then FIcons[        saveImg.Tag] := ''
    else                                   FIcons[        saveImg.Tag] := SaveBtnIcon;
    if             PrintBtnIcon = ''  then FIcons[       printImg.Tag] := 'print1'
    else if        PrintBtnIcon = '-' then FIcons[       printImg.Tag] := ''
    else                                   FIcons[       printImg.Tag] := PrintBtnIcon;
    if              ShowBtnIcon = ''  then FIcons[        showImg.Tag] := 'show1'
    else if         ShowBtnIcon = '-' then FIcons[        showImg.Tag] := ''
    else                                   FIcons[        showImg.Tag] := ShowBtnIcon;
    if          ContinueBtnIcon = ''  then FIcons[    continueImg.Tag] := 'continue1'
    else if     ContinueBtnIcon = '-' then FIcons[    continueImg.Tag] := ''
    else                                   FIcons[    continueImg.Tag] := ContinueBtnIcon;
    if      CantContinueBtnIcon = ''  then FIcons[cantContinueImg.Tag] := 'cantContinue1'
    else if CantContinueBtnIcon = '-' then FIcons[cantContinueImg.Tag] := ''
    else                                   FIcons[cantContinueImg.Tag] := CantContinueBtnIcon;
    if           RestartBtnIcon = ''  then FIcons[     restartImg.Tag] := 'restart1'
    else if      RestartBtnIcon = '-' then FIcons[     restartImg.Tag] := ''
    else                                   FIcons[     restartImg.Tag] := RestartBtnIcon;
    if             CloseBtnIcon = ''  then FIcons[       closeImg.Tag] := 'close1'
    else if        CloseBtnIcon = '-' then FIcons[       closeImg.Tag] := ''
    else                                   FIcons[       closeImg.Tag] := CloseBtnIcon;
    if                  BigIcon = ''  then FIcons[         bigImg.Tag] := 'big1'
    else if             BigIcon = '-' then FIcons[         bigImg.Tag] := ''
    else                                   FIcons[         bigImg.Tag] := BigIcon;
    if               Send32Icon = ''  then FIcons[      send32Img.Tag] := 'send321'
    else if          Send32Icon = '-' then FIcons[      send32Img.Tag] := ''
    else                                   FIcons[      send32Img.Tag] := Send32Icon;
    if           PleaseWaitIcon = ''  then FIcons[      plWaitImg.Tag] := 'plwait1'
    else if      PleaseWaitIcon = '-' then FIcons[      plWaitImg.Tag] := ''
    else                                   FIcons[      plWaitImg.Tag] := PleaseWaitIcon;
    case FocusedButton of
      bSendBugReport       :     sendFocus.Checked := true;
      bSaveBugReport       :     saveFocus.Checked := true;
      bPrintBugReport      :    printFocus.Checked := true;
      bShowBugReport       :     showFocus.Checked := true;
      bContinueApplication : continueFocus.Checked := true;
      bRestartApplication  :  restartFocus.Checked := true;
      bCloseApplication    :    closeFocus.Checked := true;
    end;
    case ScreenShotDepth of
               4 : screenShotCombo.ItemIndex := 1;
       50 * 1024 : screenShotCombo.ItemIndex := 2;
      100 * 1024 : screenShotCombo.ItemIndex := 3;
      200 * 1024 : screenShotCombo.ItemIndex := 4;
      300 * 1024 : screenShotCombo.ItemIndex := 5;
      else         screenShotCombo.ItemIndex := 0;
    end;
    case DupExceptDef of
      ddExceptAddrIdentical : dupExcept1Radio.Checked := true;
      ddAllStacksIdentical  : dupExcept3Radio.Checked := true;
      else                    dupExcept2Radio.Checked := true;
    end;
    case DupFreezeDef of
      ddExceptAddrIdentical : dupFreeze1Radio.Checked := true;
      ddCrashStackIdentical : dupFreeze2Radio.Checked := true;
      else                    dupFreeze3Radio.Checked := true;
    end;
    case LimitDisassembly of
      003 : limitDisCombo.ItemIndex := 0;
      005 : limitDisCombo.ItemIndex := 1;
      007 : limitDisCombo.ItemIndex := 2;
      010 : limitDisCombo.ItemIndex := 3;
      020 : limitDisCombo.ItemIndex := 4;
      050 : limitDisCombo.ItemIndex := 5;
      100 : limitDisCombo.ItemIndex := 6;
      200 : limitDisCombo.ItemIndex := 7;
      500 : limitDisCombo.ItemIndex := 8;
      else  limitDisCombo.ItemIndex := 1;
    end;
    for i1 := 1 to SubStrCount(EnabledPlugins) do
      for i2 := 0 to high(Plugins) do
        if IsTextEqual(SubStr(EnabledPlugins, i1), Plugins[i2].name) then begin
          PluginList.Checked[i2] := true;
          break;
        end;
    Filter1ShowCombo.ItemIndex := ord(Filter1ShowSetting);
    Filter2ShowCombo.ItemIndex := ord(Filter2ShowSetting);
    Filter3ShowCombo.ItemIndex := ord(GeneralShowSetting);
    for i1 := 0 to high(SettingsStrings) do
      with StringList.Items.Add do begin
        Caption := SettingsStrings[i1].description;
        s1 := TPString(integer(@settings) + SettingsStrings[i1].offset)^;
        ReplaceStr(s1, #$D#$A, '%LF%');
        SubItems.Add(s1);
      end;
    for i1 := 1 to SubStrCount(AdditionalAttachs) do begin
      s1 := SubStr(AdditionalAttachs, i1);
      with AttachList.Items.Add do begin
        ImageIndex := 9;
        Caption := SubStr(s1, 1, '>');
        SubItems.Add(SubStr(s1, 2, '>'));
        SubItems.Add(SubStr(s1, 3, '>'));
      end;
    end;
  end;
end;

procedure TFMadExceptProjectSettings.FormToSettings(var settings: TProject);

  function GetAuto(check: TCheckBox; combo: TComboBox) : dword;
  begin
    if check.checked then begin
      case combo.ItemIndex of
        0 :  result := 1;
        1 :  result := 1 * 60;
        2 :  result := 7 * 60;
        3 :  result := 30 * 60;
        4 :  result := 5 * 60 * 60;
        else result := 24 * 60 * 60;
      end;
    end else
      result := 0;
  end;

  function CorrectFilterClasses(classes: string) : string;
  begin
    result := classes;
    ReplaceStr(result, ';', ',');
    ReplaceStr(result, '|', ',');
    ReplaceStr(result, '/', ',');
    ReplaceStr(result, '+', ',');
    ReplaceStr(result, '-', ',');
    ReplaceStr(result, ' ', ',');
    FormatSubStrs(result, ',');
    ReplaceStr(result, ',', ', ');
  end;

var i1, i2 : integer;
    s1     : string;
    li     : TListItem;
begin
  with settings do begin
    Enabled               :=                 enableMECheck.Checked;
    MinDebugInfoOnly      :=          includeMinDebugCheck.Checked;
    NoOwnSettings         :=                    noMesCheck.Checked;
    CheckFileCrc          :=                 bitFaultCheck.Checked;
    CheckForFreeze        :=               antiFreezeCheck.Checked;
    FreezeTimeout         := StrToIntDef(freezeTimeoutEdit.Text, 60) * 1000;
    AutoSave              :=                 autoSaveCheck.Checked;
    AutoSaveIfNotSent     :=        autoSaveIfNotSentCheck.Checked;
    AutoSend              :=                 autoSendCheck.Checked;
    AutoSendPrgrBox       :=          autoSendPrgrBoxCheck.Checked;
    AutoClipboard         :=                 autoClipCheck.Checked;
    SuspendThreads        :=                  suspendCheck.Checked;
    ShowPleaseWaitBox     :=               pleaseWaitCheck.Checked;
    AutoContinue          :=             autoContinueCheck.Checked;
    AutoRestart           := GetAuto(autoRestartCheck, restartCombo);
    AutoClose             := GetAuto(  autoCloseCheck,   closeCombo);
    SendBtnVisible        :=               sendBugRepCheck.Checked;
    SaveBtnVisible        :=               saveBugRepCheck.Checked;
    PrintBtnVisible       :=              printBugRepCheck.Checked;
    ShowBtnVisible        :=               showBugRepCheck.Checked;
    ContinueBtnVisible    :=              continueAppCheck.Checked;
    RestartBtnVisible     :=               restartAppCheck.Checked;
    CloseBtnVisible       :=                 closeAppCheck.Checked;
    if not (ContinueBtnVisible or RestartBtnVisible or CloseBtnVisible) then
      CloseBtnVisible := true;
    if FIcons[        sendImg.Tag] = '' then         SendBtnIcon := '-'
    else                                             SendBtnIcon := FIcons[        sendImg.Tag];
    if FIcons[        saveImg.Tag] = '' then         SaveBtnIcon := '-'
    else                                             SaveBtnIcon := FIcons[        saveImg.Tag];
    if FIcons[       printImg.Tag] = '' then        PrintBtnIcon := '-'
    else                                            PrintBtnIcon := FIcons[       printImg.Tag];
    if FIcons[        showImg.Tag] = '' then         ShowBtnIcon := '-'
    else                                             ShowBtnIcon := FIcons[        showImg.Tag];
    if FIcons[    continueImg.Tag] = '' then     ContinueBtnIcon := '-'
    else                                         ContinueBtnIcon := FIcons[    continueImg.Tag];
    if FIcons[cantContinueImg.Tag] = '' then CantContinueBtnIcon := '-'
    else                                     CantContinueBtnIcon := FIcons[cantContinueImg.Tag];
    if FIcons[     restartImg.Tag] = '' then      RestartBtnIcon := '-'
    else                                          RestartBtnIcon := FIcons[     restartImg.Tag];
    if FIcons[       closeImg.Tag] = '' then        CloseBtnIcon := '-'
    else                                            CloseBtnIcon := FIcons[       closeImg.Tag];
    if FIcons[         bigImg.Tag] = '' then             BigIcon := '-'
    else                                                 BigIcon := FIcons[         bigImg.Tag];
    if FIcons[      send32Img.Tag] = '' then          Send32Icon := '-'
    else                                              Send32Icon := FIcons[      send32Img.Tag];
    if FIcons[      plWaitImg.Tag] = '' then      PleaseWaitIcon := '-'
    else                                          PleaseWaitIcon := FIcons[      plWaitImg.Tag];
    if          sendFocus.Checked then FocusedButton := bSendBugReport
    else if     saveFocus.Checked then FocusedButton := bSaveBugReport
    else if    printFocus.Checked then FocusedButton := bPrintBugReport
    else if     showFocus.Checked then FocusedButton := bShowBugReport
    else if continueFocus.Checked then FocusedButton := bContinueApplication
    else if  restartFocus.Checked then FocusedButton := bRestartApplication
    else if    closeFocus.Checked then FocusedButton := bCloseApplication;
    if sendAssisCombo.ItemIndex > 0 then
         SendAssistant    := sendAssisCombo.Items[sendAssisCombo.ItemIndex]
    else SendAssistant    := '';
    if saveAssisCombo.ItemIndex > 0 then
         SaveAssistant    := saveAssisCombo.Items[saveAssisCombo.ItemIndex]
    else SaveAssistant    := '';
    if printAssisCombo.ItemIndex > 0 then
         PrintAssistant   := printAssisCombo.Items[printAssisCombo.ItemIndex]
    else PrintAssistant   := '';
    AutoShowBugReport     :=           autoShowBugRepCheck.Checked;
    NoOwnerDrawButtons    :=             showUglyBtnsCheck.Checked;
    MailAddr              :=                  mailAddrEdit.Text;
    AttachBugReport       :=             attachBugRepCheck.Checked;
    AttachBugReportFile   :=         attachBugRepFileCheck.Checked;
    DeleteBugReportFile   :=            delBugRepFileCheck.Checked;
    if attachScreenShotCheck.Checked then begin
      case screenShotCombo.ItemIndex of
        0 :  ScreenShotDepth := 8;
        1 :  ScreenShotDepth := 4;
        2 :  ScreenShotDepth :=  50 * 1024;
        3 :  ScreenShotDepth := 100 * 1024;
        4 :  ScreenShotDepth := 200 * 1024;
        else ScreenShotDepth := 300 * 1024;
      end;
    end else
      ScreenShotDepth := 0;
    ScreenShotAppOnly     :=        captureCurrentAppCheck.Checked;
    MailAsSmtpServer      := directCheck.Checked and smtpServerRadio.Checked;
    MailAsSmtpClient      := directCheck.Checked and smtpClientRadio.Checked;
    UploadViaHttp         := directCheck.Checked and httpUploadRadio.Checked;
    SendInBackground      :=               backgroundCheck.Checked;
    MailViaMapi           :=                     mapiCheck.Checked;
    MailViaMailto         :=                   mailtoCheck.Checked;
    BugReportFile         :=                bugRepFileEdit.Text;
    AppendBugReports      :=             appendBugRepCheck.Checked;
    BugReportFileSize     := StrToIntDef(bugRepFileSizeEdit.Text, 50) * 1000;
    NoDupExcepts          :=              noDupExceptCheck.Checked;
    NoDupFreezes          :=              noDupFreezeCheck.Checked;
    if      dupExcept1Radio.Checked then DupExceptDef := ddExceptAddrIdentical
    else if dupExcept2Radio.Checked then DupExceptDef := ddCrashStackIdentical
    else                                 DupExceptDef := ddAllStacksIdentical;
    if      dupFreeze1Radio.Checked then DupFreezeDef := ddExceptAddrIdentical
    else if dupFreeze2Radio.Checked then DupFreezeDef := ddCrashStackIdentical
    else                                 DupFreezeDef := ddAllStacksIdentical;
    ListThreads           :=              listThreadsCheck.Checked;
    ShowDisAsm            :=              disassemblyCheck.Checked;
    ShowCpuRegisters      :=                  cpuRegsCheck.Checked;
    ShowStackDump         :=                stackDumpCheck.Checked;
    HideUglyItems         :=            hideUglyItemsCheck.Checked;
    ShowRelativeAddrs     :=             showRelAddrsCheck.Checked;
    ShowRelativeLines     :=             showRelLinesCheck.Checked;
    FormatDisassembly     :=                formatDisCheck.Checked;
    if limitDisCheck.Checked then begin
      case limitDisCombo.ItemIndex of
        0 :  LimitDisassembly := 3;
        1 :  LimitDisassembly := 5;
        2 :  LimitDisassembly := 7;
        3 :  LimitDisassembly := 10;
        4 :  LimitDisassembly := 20;
        5 :  LimitDisassembly := 50;
        6 :  LimitDisassembly := 100;
        7 :  LimitDisassembly := 200;
        8 :  LimitDisassembly := 500;
        else LimitDisassembly := 5;
      end;
    end else
      LimitDisassembly := 0;
    EnabledPlugins := '';
    for i1 := 0 to PluginList.Items.Count - 1 do
      if PluginList.Checked[i1] then
        EnabledPlugins := EnabledPlugins + '|' + Plugins[i1].name;
    Delete(EnabledPlugins, 1, 1);
    Filter1Classes      := CorrectFilterClasses(filter1Edit.Text);
    Filter2Classes      := CorrectFilterClasses(filter2Edit.Text);
    Filter1NoBugReport  :=          filter1NoBugRepCheck.Checked;
    Filter2NoBugReport  :=          filter2NoBugRepCheck.Checked;
    GeneralNoBugReport  :=          filter3NoBugRepCheck.Checked;
    Filter1NoScreenshot :=      filter1NoScreenshotCheck.Checked;
    Filter2NoScreenshot :=      filter2NoScreenshotCheck.Checked;
    GeneralNoScreenshot :=      filter3NoScreenshotCheck.Checked;
    Filter1NoSuspend    :=         filter1NoSuspendCheck.Checked;
    Filter2NoSuspend    :=         filter2NoSuspendCheck.Checked;
    GeneralNoSuspend    :=         filter3NoSuspendCheck.Checked;
    Filter1NoHandlers   :=        filter1NoHandlersCheck.Checked;
    Filter2NoHandlers   :=        filter2NoHandlersCheck.Checked;
    GeneralNoHandlers   :=        filter3NoHandlersCheck.Checked;
    Filter1ShowSetting  := TMEShowSetting(Filter1ShowCombo.ItemIndex);
    Filter2ShowSetting  := TMEShowSetting(Filter2ShowCombo.ItemIndex);
    GeneralShowSetting  := TMEShowSetting(Filter3ShowCombo.ItemIndex);
    if Filter1AssisCombo.ItemIndex > 0 then
         Filter1Assis := Filter1AssisCombo.Items[Filter1AssisCombo.ItemIndex]
    else Filter1Assis := '';
    if Filter2AssisCombo.ItemIndex > 0 then
         Filter2Assis := Filter2AssisCombo.Items[Filter2AssisCombo.ItemIndex]
    else Filter2Assis := '';
    if Filter3AssisCombo.ItemIndex > 0 then
         GeneralAssis := Filter3AssisCombo.Items[Filter3AssisCombo.ItemIndex]
    else GeneralAssis := '';
    SetLength(Assistants, Length(FAssis));
    for i1 := 0 to high(Assistants) do
      Assistants[i1] := FAssis[i1];
    SetLength(Forms, Length(FForms));
    for i1 := 0 to high(Forms) do
      Forms[i1] := StructToDfm(FForms[i1]);
    for i1 := 0 to high(SettingsStrings) do begin
      li := StringList.Items[i1];
      if li <> nil then begin
        s1 := li.SubItems[0];
        ReplaceStr(s1, '%LF%', #$D#$A);
        TPString(integer(@settings) + SettingsStrings[i1].offset)^ := s1;
      end;
    end;
    i2 := 0;
    li := AttachList.FindCaption(0, '<bug report>', false, true, false);
    if li <> nil then begin
      FBugReportSendAs := li.SubItems[0];
      FBugReportZip    := li.SubItems[1];
      inc(i2);
    end;
    li := AttachList.FindCaption(0, '<screen shot>', false, true, false);
    if li <> nil then begin
      FScreenShotSendAs := li.SubItems[0];
      FScreenShotZip    := li.SubItems[1];
      inc(i2);
    end;
    BugReportSendAs  := FBugReportSendAs;
    BugReportZip     := FBugReportZip;
    ScreenShotSendAs := FScreenShotSendAs;
    ScreenShotZip    := FScreenShotZip;
    AdditionalAttachs := '';
    for i1 := i2 to AttachList.Items.Count - 1 do
      with AttachList.Items[i1] do
        AdditionalAttachs := AdditionalAttachs + '|' + Caption + '>' + SubItems[0] + '>' + SubItems[1];
    Delete(AdditionalAttachs, 1, 1);
    SmtpServerRadioClick(nil);
    SmtpServer   := FSmtpServer;
    SmtpPort     := FSmtpPort;
    SmtpAccount  := FSmtpAccount;
    SmtpPassword := FSmtpPassword;
    HttpServer   := FHttpServer;
    HttpPort     := FHttpPort;
    HttpAccount  := FHttpAccount;
    HttpPassword := FHttpPassword;
  end;
end;

// ***************************************************************

function CompareProjectSettings(const p1, p2: TProject) : boolean;

  function DAStrEqual(const das1, das2: TDAString) : boolean;
  var i1 : integer;
  begin
    result := Length(das1) = Length(das2);
    if result then
      for i1 := 0 to high(das1) do
        if das1[i1] <> das2[i1] then begin
          result := false;
          break;
        end;
  end;

begin
  result := (p1.Enabled               = p2.Enabled              ) and
            (p1.MinDebugInfoOnly      = p2.MinDebugInfoOnly     ) and
            (p1.NoOwnSettings         = p2.NoOwnSettings        ) and
            (p1.CheckFileCrc          = p2.CheckFileCrc         ) and
            (p1.CheckForFreeze        = p2.CheckForFreeze       ) and
            (p1.FreezeTimeout         = p2.FreezeTimeout        ) and
            (p1.AutoSave              = p2.AutoSave             ) and
            (p1.AutoSaveIfNotSent     = p2.AutoSaveIfNotSent    ) and
            (p1.AutoSend              = p2.AutoSend             ) and
            (p1.AutoSendPrgrBox       = p2.AutoSendPrgrBox      ) and
            (p1.AutoClipboard         = p2.AutoClipboard        ) and
            (p1.SuspendThreads        = p2.SuspendThreads       ) and
            (p1.ShowPleaseWaitBox     = p2.ShowPleaseWaitBox    ) and
            (p1.PleaseWaitIcon        = p2.PleaseWaitIcon       ) and
            (p1.AutoContinue          = p2.AutoContinue         ) and
            (p1.AutoRestart           = p2.AutoRestart          ) and
            (p1.AutoClose             = p2.AutoClose            ) and
            (p1.SendBtnVisible        = p2.SendBtnVisible       ) and
            (p1.SaveBtnVisible        = p2.SaveBtnVisible       ) and
            (p1.PrintBtnVisible       = p2.PrintBtnVisible      ) and
            (p1.ShowBtnVisible        = p2.ShowBtnVisible       ) and
            (p1.ContinueBtnVisible    = p2.ContinueBtnVisible   ) and
            (p1.RestartBtnVisible     = p2.RestartBtnVisible    ) and
            (p1.CloseBtnVisible       = p2.CloseBtnVisible      ) and
            (p1.SendBtnIcon           = p2.SendBtnIcon          ) and
            (p1.SaveBtnIcon           = p2.SaveBtnIcon          ) and
            (p1.PrintBtnIcon          = p2.PrintBtnIcon         ) and
            (p1.ShowBtnIcon           = p2.ShowBtnIcon          ) and
            (p1.ContinueBtnIcon       = p2.ContinueBtnIcon      ) and
            (p1.CantContinueBtnIcon   = p2.CantContinueBtnIcon  ) and
            (p1.RestartBtnIcon        = p2.RestartBtnIcon       ) and
            (p1.CloseBtnIcon          = p2.CloseBtnIcon         ) and
            (p1.FocusedButton         = p2.FocusedButton        ) and
            (p1.SendAssistant         = p2.SendAssistant        ) and
            (p1.SaveAssistant         = p2.SaveAssistant        ) and
            (p1.PrintAssistant        = p2.PrintAssistant       ) and
            (p1.AutoShowBugReport     = p2.AutoShowBugReport    ) and
            (p1.NoOwnerDrawButtons    = p2.NoOwnerDrawButtons   ) and
            (p1.BigIcon               = p2.BigIcon              ) and
            (p1.MailAddr              = p2.MailAddr             ) and
            (p1.AttachBugReport       = p2.AttachBugReport      ) and
            (p1.AttachBugReportFile   = p2.AttachBugReportFile  ) and
            (p1.DeleteBugReportFile   = p2.DeleteBugReportFile  ) and
            (p1.BugReportSendAs       = p2.BugReportSendAs      ) and
            (p1.BugReportZip          = p2.BugReportZip         ) and
            (p1.ScreenShotDepth       = p2.ScreenShotDepth      ) and
            (p1.ScreenShotAppOnly     = p2.ScreenShotAppOnly    ) and
            (p1.ScreenShotSendAs      = p2.ScreenShotSendAs     ) and
            (p1.ScreenShotZip         = p2.ScreenShotZip        ) and
            (p1.AdditionalAttachs     = p2.AdditionalAttachs    ) and
            (p1.MailAsSmtpServer      = p2.MailAsSmtpServer     ) and
            (p1.MailAsSmtpClient      = p2.MailAsSmtpClient     ) and
            (p1.UploadViaHttp         = p2.UploadViaHttp        ) and
            (p1.SendInBackground      = p2.SendInBackground     ) and
            (p1.MailViaMapi           = p2.MailViaMapi          ) and
            (p1.MailViaMailto         = p2.MailViaMailto        ) and
            (p1.SmtpServer            = p2.SmtpServer           ) and
            (p1.SmtpPort              = p2.SmtpPort             ) and
            (p1.SmtpAccount           = p2.SmtpAccount          ) and
            (p1.SmtpPassword          = p2.SmtpPassword         ) and
            (p1.HttpServer            = p2.HttpServer           ) and
            (p1.HttpPort              = p2.HttpPort             ) and
            (p1.HttpAccount           = p2.HttpAccount          ) and
            (p1.HttpPassword          = p2.HttpPassword         ) and
            (p1.Send32Icon            = p2.Send32Icon           ) and
            (p1.BugReportFile         = p2.BugReportFile        ) and
            (p1.AppendBugReports      = p2.AppendBugReports     ) and
            (p1.BugReportFileSize     = p2.BugReportFileSize    ) and
            (p1.NoDupExcepts          = p2.NoDupExcepts         ) and
            (p1.NoDupFreezes          = p2.NoDupFreezes         ) and
            (p1.DupExceptDef          = p2.DupExceptDef         ) and
            (p1.DupFreezeDef          = p2.DupFreezeDef         ) and
            (p1.ListThreads           = p2.ListThreads          ) and
            (p1.ShowDisAsm            = p2.ShowDisAsm           ) and
            (p1.ShowCpuRegisters      = p2.ShowCpuRegisters     ) and
            (p1.ShowStackDump         = p2.ShowStackDump        ) and
            (p1.HideUglyItems         = p2.HideUglyItems        ) and
            (p1.ShowRelativeAddrs     = p2.ShowRelativeAddrs    ) and
            (p1.ShowRelativeLines     = p2.ShowRelativeLines    ) and
            (p1.FormatDisassembly     = p2.FormatDisassembly    ) and
            (p1.LimitDisassembly      = p2.LimitDisassembly     ) and
            (p1.EnabledPlugins        = p2.EnabledPlugins       ) and
            (p1.Filter1Classes        = p2.Filter1Classes       ) and
            (p1.Filter2Classes        = p2.Filter2Classes       ) and
            (p1.Filter1NoBugReport    = p2.Filter1NoBugReport   ) and
            (p1.Filter2NoBugReport    = p2.Filter2NoBugReport   ) and
            (p1.GeneralNoBugReport    = p2.GeneralNoBugReport   ) and
            (p1.Filter1NoScreenshot   = p2.Filter1NoScreenshot  ) and
            (p1.Filter2NoScreenshot   = p2.Filter2NoScreenshot  ) and
            (p1.GeneralNoScreenshot   = p2.GeneralNoScreenshot  ) and
            (p1.Filter1NoSuspend      = p2.Filter1NoSuspend     ) and
            (p1.Filter2NoSuspend      = p2.Filter2NoSuspend     ) and
            (p1.GeneralNoSuspend      = p2.GeneralNoSuspend     ) and
            (p1.Filter1NoHandlers     = p2.Filter1NoHandlers    ) and
            (p1.Filter2NoHandlers     = p2.Filter2NoHandlers    ) and
            (p1.GeneralNoHandlers     = p2.GeneralNoHandlers    ) and
            (p1.Filter1ShowSetting    = p2.Filter1ShowSetting   ) and
            (p1.Filter2ShowSetting    = p2.Filter2ShowSetting   ) and
            (p1.GeneralShowSetting    = p2.GeneralShowSetting   ) and
            (p1.Filter1Assis          = p2.Filter1Assis         ) and
            (p1.Filter2Assis          = p2.Filter2Assis         ) and
            (p1.GeneralAssis          = p2.GeneralAssis         ) and
            DAStrEqual(p1.Assistants,   p2.Assistants           ) and
            DAStrEqual(p1.Forms,        p2.Forms                ) and
            (p1.TitleBar              = p2.TitleBar             ) and
            (p1.ExceptMsg             = p2.ExceptMsg            ) and
            (p1.FrozenMsg             = p2.FrozenMsg            ) and
            (p1.BitFaultMsg           = p2.BitFaultMsg          ) and
            (p1.SendBtnCaption        = p2.SendBtnCaption       ) and
            (p1.SaveBtnCaption        = p2.SaveBtnCaption       ) and
            (p1.PrintBtnCaption       = p2.PrintBtnCaption      ) and
            (p1.ShowBtnCaption        = p2.ShowBtnCaption       ) and
            (p1.ContinueBtnCaption    = p2.ContinueBtnCaption   ) and
            (p1.RestartBtnCaption     = p2.RestartBtnCaption    ) and
            (p1.CloseBtnCaption       = p2.CloseBtnCaption      ) and
            (p1.OkBtnCaption          = p2.OkBtnCaption         ) and
            (p1.DetailsBtnCaption     = p2.DetailsBtnCaption    ) and
            (p1.PleaseWaitTitle       = p2.PleaseWaitTitle      ) and
            (p1.PleaseWaitText        = p2.PleaseWaitText       ) and
            (p1.MailSubject           = p2.MailSubject          ) and
            (p1.MailBody              = p2.MailBody             ) and
            (p1.SendBoxTitle          = p2.SendBoxTitle         ) and
            (p1.PrepareAttachMsg      = p2.PrepareAttachMsg     ) and
            (p1.MxLookupMsg           = p2.MxLookupMsg          ) and
            (p1.ConnectMsg            = p2.ConnectMsg           ) and
            (p1.AuthMsg               = p2.AuthMsg              ) and
            (p1.SendMailMsg           = p2.SendMailMsg          ) and
            (p1.FieldsMsg             = p2.FieldsMsg            ) and
            (p1.SendAttachMsg         = p2.SendAttachMsg        ) and
            (p1.SendFinalizeMsg       = p2.SendFinalizeMsg      ) and
            (p1.SendFailureMsg        = p2.SendFailureMsg       ) and
            (p1.VersionVar            = p2.VersionVar           );
end;

{$ifdef TestProjectOptions}
  procedure ListAllProjectOptions(project: IOTAProject); forward;
{$endif}

function GetVersionFromResources : string;
var arrCh      : array [0..MAX_PATH] of char;
    c1, c2, c3 : dword;
    s1         : string;
begin
  result := '';
  GetModuleFileName(HInstance, arrCh, MAX_PATH);
  s1 := arrCh;
  KillStr(s1, 'Wizard');
  c1 := GetModuleHandle(pchar(s1));
  if c1 = 0 then
    c1 := GetModuleHandle(arrCh);
  if c1 <> 0 then begin
    c2 := FindResource(c1, 'DESCRIPTION', RT_RCDATA);
    if c2 <> 0 then begin
      c3 := LoadResource(c1, c2);
      if c3 <> 0 then begin
        result := wideString(PWideChar(LockResource(c3)));
        UnlockResource(c3);
        FreeResource(c3);
      end;
    end;
  end;
  if result <> '' then begin
    Delete(result, 1, Pos(' ', result));
    Delete(result, Pos('-', result), maxInt);
    TrimStr(result);
    result := result + ' ';
  end;
end;

function EditProjectSettings(var project: TProject) : boolean;

  function EncryptPassword(password, account, server: string) : string;
  begin
    result := password;
    Encrypt(result, account, $77777777);
    Encrypt(result, server,  $12345678);
  end;

var s1         : string;
    tmpProject : TProject;
begin
  result := false;
  {$ifdef TestProjectOptions}
    ListAllProjectOptions(IOTAProject(project.FPrj));
  {$endif}
  with TFMadExceptProjectSettings.Create(nil), project do
    try
      SettingsStrings[ 0].offset := integer(@TitleBar          ) - integer(@project);
      SettingsStrings[ 1].offset := integer(@ExceptMsg         ) - integer(@project);
      SettingsStrings[ 2].offset := integer(@FrozenMsg         ) - integer(@project);
      SettingsStrings[ 3].offset := integer(@BitFaultMsg       ) - integer(@project);
      SettingsStrings[ 4].offset := integer(@SendBtnCaption    ) - integer(@project);
      SettingsStrings[ 5].offset := integer(@SaveBtnCaption    ) - integer(@project);
      SettingsStrings[ 6].offset := integer(@PrintBtnCaption   ) - integer(@project);
      SettingsStrings[ 7].offset := integer(@ShowBtnCaption    ) - integer(@project);
      SettingsStrings[ 8].offset := integer(@ContinueBtnCaption) - integer(@project);
      SettingsStrings[ 9].offset := integer(@RestartBtnCaption ) - integer(@project);
      SettingsStrings[10].offset := integer(@CloseBtnCaption   ) - integer(@project);
      SettingsStrings[11].offset := integer(@OkBtnCaption      ) - integer(@project);
      SettingsStrings[12].offset := integer(@DetailsBtnCaption ) - integer(@project);
      SettingsStrings[13].offset := integer(@PleaseWaitTitle   ) - integer(@project);
      SettingsStrings[14].offset := integer(@PleaseWaitText    ) - integer(@project);
      SettingsStrings[15].offset := integer(@MailSubject       ) - integer(@project);
      SettingsStrings[16].offset := integer(@MailBody          ) - integer(@project);
      SettingsStrings[17].offset := integer(@SendBoxTitle      ) - integer(@project);
      SettingsStrings[18].offset := integer(@PrepareAttachMsg  ) - integer(@project);
      SettingsStrings[19].offset := integer(@MxLookupMsg       ) - integer(@project);
      SettingsStrings[20].offset := integer(@ConnectMsg        ) - integer(@project);
      SettingsStrings[21].offset := integer(@AuthMsg           ) - integer(@project);
      SettingsStrings[22].offset := integer(@SendMailMsg       ) - integer(@project);
      SettingsStrings[23].offset := integer(@FieldsMsg         ) - integer(@project);
      SettingsStrings[24].offset := integer(@SendAttachMsg     ) - integer(@project);
      SettingsStrings[25].offset := integer(@SendFinalizeMsg   ) - integer(@project);
      SettingsStrings[26].offset := integer(@SendFailureMsg    ) - integer(@project);
      smtpPassword := DecryptPassword(Decode(smtpPassword), smtpAccount, smtpServer);
      httpPassword := DecryptPassword(Decode(httpPassword), httpAccount, httpServer);
      try
        SettingsToForm(project);
        CheckControls(nil);
        s1 := 'madExcept ' + GetVersionFromResources;
        if FPrj = nil then
             Caption := s1 + 'default settings...'
        else Caption := s1 + 'settings  ·  ' + ExtractFileName(IOTAProject(FPrj).FileName);
        if ShowModal = mrOk then begin
          tmpProject := project;
          FormToSettings(tmpProject);
          if not CompareProjectSettings(tmpProject, project) then begin
            project := tmpProject;
            result := true;
          end;
          if defaultCheck.Checked then
            SaveDefaultSettings(project);
        end;
      finally
        smtpPassword := Encode(EncryptPassword(smtpPassword, smtpAccount, smtpServer));
        httpPassword := Encode(EncryptPassword(httpPassword, httpAccount, httpServer));
      end;
    finally Free end;
end;

procedure TFMadExceptProjectSettings.CheckControlColor(parent: TWinControl);
var i1 : integer;
begin
  for i1 := 0 to parent.ControlCount - 1 do
    if parent.Controls[i1] is TEdit then begin
      with TEdit(parent.Controls[i1]) do
        if Enabled then Color := clWindow
        else            Color := DecCol(GetSysColor(COLOR_BTNFACE), $080808, true);
    end else
      if parent.Controls[i1] is TMemo then begin
        with TMemo(parent.Controls[i1]) do
          if Enabled then Color := clWindow
          else            Color := DecCol(GetSysColor(COLOR_BTNFACE), $080808, true);
      end else
        if parent.Controls[i1] is TComboBox then begin
          with TComboBox(parent.Controls[i1]) do
            if Enabled then Color := clWindow
            else            Color := DecCol(GetSysColor(COLOR_BTNFACE), $080808, true);
        end else
          if parent.Controls[i1] is TListView then begin
            with TListView(parent.Controls[i1]) do
              if Enabled then Font.Color := clWindowText
              else            Font.Color := clGrayText;
          end else
            if parent.Controls[i1] is TPanel then
              CheckControlColor(TPanel(parent.Controls[i1]));
end;

procedure TFMadExceptProjectSettings.CheckControls(Sender: TObject);
var b1, b2   : boolean;
    li1, li2 : TListItem;
begin
  // work around a Delphi 2005
  AttachList.HandleNeeded;
  StringList.HandleNeeded;
  AssisList.HandleNeeded;
  FormList.HandleNeeded;
  CompList.HandleNeeded;
  b1 := enableMECheck.Checked;
  b2 := b1 and (not noMesCheck.Checked);
     includeMinDebugCheck.Enabled := b1;
               noMesCheck.Enabled := b1;
            bitFaultCheck.Enabled := b2;
          antiFreezeCheck.Enabled := b2;
        freezeTimeoutEdit.Enabled := b2 and antiFreezeCheck.Checked;
            autoSaveCheck.Enabled := b2;
   autoSaveIfNotSentCheck.Enabled := b2 and autoSaveCheck.Checked;
            autoSendCheck.Enabled := b2;
     autoSendPrgrBoxCheck.Enabled := b2 and autoSendCheck.Checked;
            autoClipCheck.Enabled := b2;
             suspendCheck.Enabled := b2;
          pleaseWaitCheck.Enabled := b2;
      pleaseWaitIconLabel.Enabled := b2;
                plWaitImg.Enabled := b2;
        autoContinueCheck.Enabled := b2;
         autoRestartCheck.Enabled := b2;
           autoCloseCheck.Enabled := b2;
             restartCombo.Enabled := b2 and autoRestartCheck.Checked;
               closeCombo.Enabled := b2 and autoCloseCheck.Checked;
          sendBugRepCheck.Enabled := b2;
          saveBugRepCheck.Enabled := b2;
         printBugRepCheck.Enabled := b2;
          showBugRepCheck.Enabled := b2;
         continueAppCheck.Enabled := b2;
          restartAppCheck.Enabled := b2;
            closeAppCheck.Enabled := b2;
                  sendImg.Enabled := b2;
                  saveImg.Enabled := b2;
                 printImg.Enabled := b2;
                  showImg.Enabled := b2;
              continueImg.Enabled := b2;
          cantContinueImg.Enabled := b2;
               restartImg.Enabled := b2;
                 closeImg.Enabled := b2;
                sendFocus.Enabled := b2 and sendBugRepCheck.Checked;
                saveFocus.Enabled := b2 and saveBugRepCheck.Checked;
               printFocus.Enabled := b2 and printBugRepCheck.Checked;
                showFocus.Enabled := b2 and showBugRepCheck.Checked;
            continueFocus.Enabled := b2 and continueAppCheck.Checked;
             restartFocus.Enabled := b2 and restartAppCheck.Checked;
               closeFocus.Enabled := b2 and closeAppCheck.Checked;
           sendAssisCombo.Enabled := b2 and sendBugRepCheck.Checked and (sendAssisCombo.Items.Count > 1);
           saveAssisCombo.Enabled := b2 and saveBugRepCheck.Checked and (saveAssisCombo.Items.Count > 1);
          printAssisCombo.Enabled := b2 and printBugRepCheck.Checked and (printAssisCombo.Items.Count > 1);
      autoShowBugRepCheck.Enabled := b2;
        showUglyBtnsCheck.Enabled := b2;
             bigIconLabel.Enabled := b2;
                   bigImg.Enabled := b2;
            mailAddrLabel.Enabled := b2;
             mailAddrEdit.Enabled := b2;
          backgroundCheck.Enabled := b2;
          send32IconLabel.Enabled := b2;
                send32Img.Enabled := b2;
              directCheck.Enabled := b2;
          smtpServerRadio.Enabled := b2 and directCheck.Checked;
          smtpClientRadio.Enabled := b2 and directCheck.Checked;
          httpUploadRadio.Enabled := b2 and directCheck.Checked;
                mapiCheck.Enabled := b2;
              mailtoCheck.Enabled := b2;
          sendServerLabel.Enabled := b2 and directCheck.Checked and (smtpClientRadio.Checked or httpUploadRadio.Checked);
           sendServerEdit.Enabled := b2 and directCheck.Checked and (smtpClientRadio.Checked or httpUploadRadio.Checked);
            sendPortLabel.Enabled := b2 and directCheck.Checked and (smtpClientRadio.Checked or httpUploadRadio.Checked);
             sendPortEdit.Enabled := b2 and directCheck.Checked and (smtpClientRadio.Checked or httpUploadRadio.Checked);
             useAuthCheck.Enabled := b2 and directCheck.Checked and (smtpClientRadio.Checked or httpUploadRadio.Checked);
         sendAccountLabel.Enabled := b2 and directCheck.Checked and (smtpClientRadio.Checked or httpUploadRadio.Checked);
          sendAccountEdit.Enabled := b2 and directCheck.Checked and (smtpClientRadio.Checked or httpUploadRadio.Checked) and useAuthCheck.Checked;
        sendPasswordLabel.Enabled := b2 and directCheck.Checked and (smtpClientRadio.Checked or httpUploadRadio.Checked);
         sendPasswordEdit.Enabled := b2 and directCheck.Checked and (smtpClientRadio.Checked or httpUploadRadio.Checked) and useAuthCheck.Checked;
        attachBugRepCheck.Enabled := b2;
    attachBugRepFileCheck.Enabled := b2 and attachBugRepCheck.Checked;
       delBugRepFileCheck.Enabled := b2 and attachBugRepCheck.Checked and attachBugRepFileCheck.Checked;
    attachScreenShotCheck.Enabled := b2;
   captureCurrentAppCheck.Enabled := b2 and attachScreenShotCheck.Checked;
          screenShotCombo.Enabled := b2 and attachScreenShotCheck.Checked;
               attachList.Enabled := b2;
             addAttachBtn.Enabled := b2;
          bugRepFileLabel.Enabled := b2;
           bugRepFileEdit.Enabled := b2;
        appendBugRepCheck.Enabled := b2;
       bugRepFileSizeEdit.Enabled := b2 and appendBugRepCheck.Checked;
         noDupExceptCheck.Enabled := b2 and appendBugRepCheck.Checked;
         noDupFreezeCheck.Enabled := b2 and appendBugRepCheck.Checked;
           dupExceptLabel.Enabled := b2 and appendBugRepCheck.Checked and noDupExceptCheck.Checked;
          dupExcept1Radio.Enabled := b2 and appendBugRepCheck.Checked and noDupExceptCheck.Checked;
          dupExcept2Radio.Enabled := b2 and appendBugRepCheck.Checked and noDupExceptCheck.Checked;
          dupExcept3Radio.Enabled := b2 and appendBugRepCheck.Checked and noDupExceptCheck.Checked;
           dupFreezeLabel.Enabled := b2 and appendBugRepCheck.Checked and noDupFreezeCheck.Checked;
          dupFreeze1Radio.Enabled := b2 and appendBugRepCheck.Checked and noDupFreezeCheck.Checked;
          dupFreeze2Radio.Enabled := b2 and appendBugRepCheck.Checked and noDupFreezeCheck.Checked;
          dupFreeze3Radio.Enabled := b2 and appendBugRepCheck.Checked and noDupFreezeCheck.Checked;
         listThreadsCheck.Enabled := b2;
         disassemblyCheck.Enabled := b2;
       hideUglyItemsCheck.Enabled := b2;
        showRelAddrsCheck.Enabled := b2 and (not includeMinDebugCheck.Checked);
        showRelLinesCheck.Enabled := b2;
           formatDisCheck.Enabled := b2 and disassemblyCheck.Checked;
            limitDisCheck.Enabled := b2 and disassemblyCheck.Checked;
            limitDisCombo.Enabled := b2 and disassemblyCheck.Checked and limitDisCheck.Checked;
               PluginList.Enabled := b2;
             filter1Label.Enabled := b2;
              filter1Edit.Enabled := b2;
             filter2Label.Enabled := b2;
              filter2Edit.Enabled := b2;
     filter1NoBugRepCheck.Enabled := b2 and (filter1Edit.Text <> '');
     filter2NoBugRepCheck.Enabled := b2 and (filter2Edit.Text <> '');
     filter3NoBugRepCheck.Enabled := b2;
 filter1NoScreenshotCheck.Enabled := b2 and (filter1Edit.Text <> '');
 filter2NoScreenshotCheck.Enabled := b2 and (filter2Edit.Text <> '');
 filter3NoScreenshotCheck.Enabled := b2;
    filter1NoSuspendCheck.Enabled := b2 and (filter1Edit.Text <> '');
    filter2NoSuspendCheck.Enabled := b2 and (filter2Edit.Text <> '');
    filter3NoSuspendCheck.Enabled := b2;
   filter1NoHandlersCheck.Enabled := b2 and (filter1Edit.Text <> '');
   filter2NoHandlersCheck.Enabled := b2 and (filter2Edit.Text <> '');
   filter3NoHandlersCheck.Enabled := b2;
         filter1ShowCombo.Enabled := b2 and (filter1Edit.Text <> '');
         filter2ShowCombo.Enabled := b2 and (filter2Edit.Text <> '');
         filter3ShowCombo.Enabled := b2;
        filter1AssisCombo.Enabled := b2 and (filter1Edit.Text <> '') and (filter1AssisCombo.Items.Count > 1) and (filter1ShowCombo.ItemIndex = 1);
        filter2AssisCombo.Enabled := b2 and (filter2Edit.Text <> '') and (filter2AssisCombo.Items.Count > 1) and (filter2ShowCombo.ItemIndex = 1);
        filter3AssisCombo.Enabled := b2 and                              (filter3AssisCombo.Items.Count > 1) and (filter3ShowCombo.ItemIndex = 1);
               StringList.Enabled := b2;
               StringMemo.Enabled := b2 and (StringList.Selected <> nil);
                 FormList.Enabled := b2;
                AssisList.Enabled := b2;
                 CompList.Enabled := b2;
  if includeMinDebugCheck.Checked then
    showRelAddrsCheck.Checked := true;
  if (sendFocus.Checked and (not sendBugRepCheck.Checked)) then
    sendFocus.Checked := false;
  if (saveFocus.Checked and (not saveBugRepCheck.Checked)) then
    saveFocus.Checked := false;
  if (printFocus.Checked and (not printBugRepCheck.Checked)) then
    printFocus.Checked := false;
  if (showFocus.Checked and (not showBugRepCheck.Checked)) then
    showFocus.Checked := false;
  if (continueFocus.Checked and (not continueAppCheck.Checked)) then
    continueFocus.Checked := false;
  if (restartFocus.Checked and (not restartAppCheck.Checked)) then
    restartFocus.Checked := false;
  if (closeFocus.Checked and (not closeAppCheck.Checked)) then
    closeFocus.Checked := false;
  if (not sendFocus.Checked) and (not saveFocus.Checked) and (not printFocus.Checked) and (not showFocus.Checked) and
     (not continueFocus.Checked) and (not restartFocus.Checked) and (not closeFocus.Checked) then
    if      sendBugRepCheck.Checked  then sendFocus.Checked := true
    else if saveBugRepCheck.Checked  then saveFocus.Checked := true
    else if printBugRepCheck.Checked then printFocus.Checked := true
    else if showBugRepCheck.Checked  then showFocus.Checked := true
    else if continueAppCheck.Checked then continueFocus.Checked := true
    else if restartAppCheck.Checked  then restartFocus.Checked := true
    else if closeAppCheck.Checked    then closeFocus.Checked := true;
  li1 := AttachList.FindCaption(0, '<bug report>', false, true, false);
  li2 := AttachList.FindCaption(0, '<screen shot>', false, true, false);
  if attachBugRepCheck.Checked <> (li1 <> nil) then
    if not attachBugRepCheck.Checked then begin
      FBugReportSendAs := li1.SubItems[0];
      FBugReportZip    := li1.SubItems[1];
      li1.Delete;
    end else
      with AttachList.Items.Insert(0) do begin
        ImageIndex := 7;
        Caption := '<bug report>';
        SubItems.Add(FBugReportSendAs);
        SubItems.Add(FBugReportZip);
      end;
  if attachScreenShotCheck.Checked <> (li2 <> nil) then
    if not attachScreenShotCheck.Checked then begin
      FScreenShotSendAs := li2.SubItems[0];
      FScreenShotZip    := li2.SubItems[1];
      li2.Delete;
    end else
      with AttachList.Items.Insert(ord(li1 <> nil)) do begin
        ImageIndex := 8;
        Caption := '<screen shot>';
        SubItems.Add(FScreenShotSendAs);
        SubItems.Add(FScreenShotZip);
      end;
  FillAssisCombos;
  CheckAssisButtons;
  AttachListSelectItem(nil, nil, false);
  CheckControlColor(self);
  if LayoutPanel.Visible then begin
          sendImg.Invalidate;
          saveImg.Invalidate;
         printImg.Invalidate;
          showImg.Invalidate;
      continueImg.Invalidate;
  cantContinueImg.Invalidate;
       restartImg.Invalidate;
         closeImg.Invalidate;
           bigImg.Invalidate;
  end;
  if AutoPanel.Visible then
    plWaitImg.Invalidate;
  if SendPanel.Visible then
    send32Img.Invalidate;
end;

procedure TFMadExceptProjectSettings.Filter1ShowComboChange(Sender: TObject);

  procedure CheckFilter(combo: TComboBox; noBugRep, noScreenShot, noSuspend, noHandlers: TCheckBox);
  begin
    case combo.ItemIndex of
      0 : begin
            noBugRep    .Checked := false;
            noScreenShot.Checked := false;
            noSuspend   .Checked := false;
            noHandlers  .Checked := false;
          end;
      1 : begin
            noBugRep    .Checked := false;
            noScreenShot.Checked := false;
            noSuspend   .Checked := true;
            noHandlers  .Checked := false;
          end;
      2 : begin
            noBugRep    .Checked := false;
            noScreenShot.Checked := false;
            noSuspend   .Checked := true;
            noHandlers  .Checked := false;
          end;
      3 : begin
            noBugRep    .Checked := true;
            noScreenShot.Checked := true;
            noSuspend   .Checked := true;
            noHandlers  .Checked := true;
          end;
      4 : begin
            noBugRep    .Checked := false;
            noScreenShot.Checked := false;
            noSuspend   .Checked := true;
            noHandlers  .Checked := false;
          end;
    end;
  end;

begin
  CheckControls(Sender);
  if      Sender = Filter1ShowCombo then CheckFilter(Filter1ShowCombo, Filter1NoBugRepCheck, Filter1NoScreenShotCheck, Filter1NoSuspendCheck, Filter1NoHandlersCheck)
  else if Sender = Filter2ShowCombo then CheckFilter(Filter2ShowCombo, Filter2NoBugRepCheck, Filter2NoScreenShotCheck, Filter2NoSuspendCheck, Filter2NoHandlersCheck)
  else if Sender = Filter3ShowCombo then CheckFilter(Filter3ShowCombo, Filter3NoBugRepCheck, Filter3NoScreenShotCheck, Filter3NoSuspendCheck, Filter3NoHandlersCheck);
end;

procedure TFMadExceptProjectSettings.StringListDblClick(Sender: TObject);
begin
  if (Sender as TListView).Selected <> nil then begin
    StringMemo.SetFocus;
    StringMemo.SelectAll;
  end;
end;

procedure TFMadExceptProjectSettings.StringMemoChange(Sender: TObject);
var li : TListItem;
    s1 : string;
begin
  li := StringList.Selected;
  if li <> nil then begin
    s1 := StringMemo.Text;
    ReplaceStr(s1, #$D#$A, '%LF%');
    li.SubItems[0] := s1;
  end;
end;

procedure TFMadExceptProjectSettings.ReleaseIconPanel(var Message: TMessage);

  function LoadBitmap(fileName: string; size: dword) : string;
  var i1     : cardinal;
      c1, c2 : dword;
      pbih   : PBitmapInfoHeader;
      s1     : string;
      b1     : boolean;
  begin
    result := '';
    c1 := CreateFile(pchar(fileName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if c1 <> INVALID_HANDLE_VALUE then begin
      SetLength(s1, GetFileSize(c1, nil));
      if (s1 <> '') and ReadFile(c1, pointer(s1)^, Length(s1), c2, nil) and (c2 = dword(Length(s1))) then begin
        if IsTextEqual(ExtractFileExt(fileName), '.ico') then begin
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
                    SetString(result, pchar(pbih), sizeOf(pbih^) + size * size * 4);
                    b1 := false;
                    break;
                  end;
                end;
          if result = '' then
            if b1 then
              ShowMessage('Icon of correct size found, but it doesn''t contain ' +
                          'an 8 bit alpha channel.')
            else
              ShowMessage('No icon of the needed size found');
//            icon := LoadImage(0, pchar(fileName), IMAGE_ICON, size, size, LR_LOADFROMFILE);
        end else begin
          pbih := pointer(dword(s1) + sizeOf(TBitmapFileHeader));
          if (pbih^.biWidth = integer(size)) and (pbih^.biHeight = integer(size)) and
             (pbih^.biBitCount = 32) then
            SetString(result, pchar(pbih), sizeOf(pbih^) + size * size * 4)
          else
            ShowMessage('Bitmap has to be 32x32 pixels TrueColor plus an 8 bit alpha channel.');
        end;
      end else
        ShowMessage('Couldn''t read the specified file.');
      CloseHandle(c1);
    end else
      ShowMessage('Couldn''t open the specified file.');
  end;

var s1 : string;
begin
  if FHook <> 0 then begin
    UnhookWindowsHookEx(FHook);
    FHook := 0;
  end;
  if FIconPanel <> nil then begin
    FIconPanel.Free;
    FIconPanel := nil;
  end;
  if Message.wParam = -2 then
    with TOpenDialog.Create(nil) do begin
      Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
      Filter := 'bitmap & icon files (*.bmp; *.ico)|*.bmp;*.ico';
      if Execute then begin
        s1 := LoadBitmap(FileName, FCurIcon.Width);
        if s1 <> '' then begin
          FIcons[FCurIcon.Tag] := s1;
          FCurIcon.Invalidate;
        end;
      end;
      Free;
    end;
end;

procedure TFMadExceptProjectSettings.ActivateWnd(var Message: TMessage);
begin
  if Visible then begin
    PostMessage(Handle, WM_USER + 777, 0, 0);
    ClosePropList;
  end;
end;

procedure TFMadExceptProjectSettings.IconPanelExit(Sender: TObject);
begin
  if Visible then
    PostMessage(Handle, WM_USER + 777, 0, 0);
end;

procedure TFMadExceptProjectSettings.IconPanelClick(Sender: TObject);
var i1 : integer;
    s1 : string;
begin
  i1 := (Sender as TSpeedButton).Tag;
  s1 := (Sender as TSpeedButton).Name;
  case i1 of
    -1 : FIcons[FCurIcon.Tag] := '';
    -2 : ;
    else FIcons[FCurIcon.Tag] := s1;
  end;
  FCurIcon.Invalidate;
  PostMessage(Handle, WM_USER + 777, i1, 0);
end;

function TFMadExceptProjectSettings.MouseHook(code, wParam, lParam: integer) : integer; stdcall;
begin
  if (code >= 0) and
     ( (wParam =   WM_LBUTTONDOWN) or (wParam =   WM_RBUTTONDOWN) or (wParam =   WM_MBUTTONDOWN) or
       (wParam = WM_NCLBUTTONDOWN) or (wParam = WM_NCRBUTTONDOWN) or (wParam = WM_NCMBUTTONDOWN)    ) then
    if (FIconPanel <> nil) and (PMouseHookStruct(lParam)^.hwnd <> FIconPanel.Handle) then
      PostMessage(Handle, WM_USER + 777, 0, 0)
    else
      if FPropList.Visible and (PMouseHookStruct(lParam)^.hwnd <> FPropList.Handle) then
        ClosePropList;
  result := CallNextHookEx(FHook, code, wParam, lParam);
end;

procedure TFMadExceptProjectSettings.ImgsClick(Sender: TObject);

  function GetResIcon(name: string; var bmp: TBitmap; size: integer) : boolean;
  var graphic : dword;     // icon or bitmap handle
      bdc     : dword;     // bitmap dc, old bitmap handle
      i1      : cardinal;
      alpha   : dword;
      c1, c2  : dword;
      p1      : pointer;
      bi      : TBitmapInfo;
      pbih    : PBitmapInfoHeader;
      buf     : TDACardinal;
      backCol : dword;
  begin
    result := false;
    if size >= 32 then
         size := 32
    else size := 16;
    SetLength(buf, size * size);
    c1 := FindResource(HInstance, pchar(name), RT_BITMAP);
    if (c1 <> 0) and (SizeOfResource(HInstance, c1) = sizeOf(pbih^) + dword(size * size * 4)) then begin
      c2 := LoadResource(HInstance, c1);
      if c2 <> 0 then begin
        pbih := LockResource(c2); Move(pbih^, bi,  sizeOf(pbih^));
        inc(pbih);                Move(pbih^, buf[0], size * size * 4);
        UnlockResource(c2);
        FreeResource(c2);
        backCol := GetSysColor(COLOR_BTNFACE);
        backCol := (backCol and $0000FF) shl 16 +    // swap rgb to bgr
                   (backCol and $00FF00)        +
                   (backCol and $FF0000) shr 16;
        for i1 := 0 to size * size - 1 do begin
          alpha := buf[i1] shr 24 + 1;
          buf[i1] := ( (      alpha  * (buf[i1] and $00FF00FF) +
                       (256 - alpha) * (backCol and $00FF00FF)   ) shr 8) and $00FF00FF +
                     ( (      alpha  * (buf[i1] and $0000FF00) +
                       (256 - alpha) * (backCol and $0000FF00)   ) shr 8) and $0000FF00;
        end;
        bdc := CreateCompatibleDC(0);
        if bdc <> 0 then begin
          p1 := nil;
          graphic := CreateDIBSection(bdc, bi, DIB_RGB_COLORS, p1, 0, 0);
          if (graphic <> 0) and (p1 <> nil) then begin
            GdiFlush;
            Move(buf[0], p1^, size * size * 4);
            GdiFlush;
            bmp := TBitmap.Create;
            bmp.Handle := graphic;
            result := true;
          end;
          DeleteDC(bdc);
        end;
      end;
    end;
  end;

var i1, iw : integer;
    img    : string;
    bmp    : TBitmap;
begin
  FCurIcon := Sender as TPaintBox;
  if FIconPanel = nil then begin
    FIconPanel := TPanel.Create(FCurIcon.Parent);
    with FIconPanel do begin
      iw := (Sender as TPaintBox).Width;
      img := (Sender as TPaintBox).Name;
      Delete(img, Length(img) - 2, 3);
      Left := (Sender as TPaintBox).Left + iw;
      Top := (Sender as TPaintBox).Top - 6;
      Height := 2 + (iw + 8) + 2;
      Parent := FCurIcon.Parent;
      TabStop := true;
      SetFocus;
      OnExit := IconPanelExit;
      i1 := 0;
      while GetResIcon(img + IntToStr(i1 + 1), bmp, FCurIcon.Width) do
        with TSpeedButton.Create(FIconPanel) do begin
          Name := img + IntToStr(i1 + 1);
          Tag := i1;
          Left := 2 + i1 * (iw + 8);
          Top := 2;
          Width := iw + 8;
          Height := iw + 8;
          Flat := true;
          Glyph.Assign(bmp);
          bmp.Free;
          Parent := FIconPanel;
          inc(i1);
          OnClick := IconPanelClick;
        end;
      if FIcons[FCurIcon.Tag] <> '' then begin
        Width := 2 + (iw + 8) * i1 + 40 * 2 + 2;
        with TSpeedButton.Create(FIconPanel) do begin
          Tag := -1;
          Left := 2 + (iw + 8) * i1;
          Top := 2;
          Width := 40;
          Height := iw + 8;
          Caption := 'clear';
          Layout := blGlyphBottom;
          Flat := true;
          Parent := FIconPanel;
          OnClick := IconPanelClick;
        end;
      end else
        Width := 2 + (iw + 8) * i1 + 40 + 2;
      with TSpeedButton.Create(FIconPanel) do begin
        Tag := -2;
        if FIcons[FCurIcon.Tag] <> '' then
             Left := 2 + (iw + 8) * i1 + 40
        else Left := 2 + (iw + 8) * i1;
        Top := 2;
        Width := 40;
        Height := iw + 8;
        Caption := 'load';
        Layout := blGlyphBottom;
        Flat := true;
        Parent := FIconPanel;
        OnClick := IconPanelClick;
      end;
    end;
    if FHook = 0 then
      FHook := SetWindowsHookEx(WH_MOUSE, FHookWndProc, 0, GetCurrentThreadId);
  end else  
    PostMessage(Handle, WM_USER + 777, 0, 0);
end;

// ***************************************************************

procedure WizardHandleException(ExceptObject: TObject);
var tb, em, ma, ms : string;
    asbr, pbv, sbv : boolean;
begin
  with MESettings do begin
    tb   := TitleBar;
    em   := ExceptMsg;
    ma   := MailAddr;
    ms   := MailSubject;
    pbv  := PrintBtnVisible;
    sbv  := SendBtnVisible;
    asbr := AutoShowBugReport;
    TitleBar          := 'madExcept wizard exception';
    ExceptMsg         := 'An error occurred in madExceptWizard.';
    MailAddr          := 'bugreport@madshi.net';
    MailSubject       := 'madExceptWizard bug report';
    PrintBtnVisible   := false;
    SendBtnVisible    := true;
    AutoShowBugReport := false;
    HandleException(etNormal, ExceptObject);
    TitleBar          := tb;
    ExceptMsg         := em;
    MailAddr          := ma;
    MailSubject       := ms;
    PrintBtnVisible   := pbv;
    SendBtnVisible    := sbv;
    AutoShowBugReport := asbr;
  end;
end;

// ***************************************************************

type
  TIDENotifier = class(TInterfacedObject, IOTAIDENotifier {$ifndef ver120}, IOTAIDENotifier50 {$endif})
    procedure FileNotification (notifyCode: TOTAFileNotification; const fileName: string; var cancel: boolean);
    procedure BeforeCompile (const project: IOTAProject; var cancel: boolean); overload;
    procedure AfterCompile  (succeeded: boolean); overload;
    {$ifndef ver120}
      procedure BeforeCompile (const project: IOTAProject; isCodeInsight: boolean; var cancel: Boolean); overload;
      procedure AfterCompile  (succeeded: boolean; isCodeInsight: boolean); overload;
    {$endif}
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  end;

  TProjectSaveNotifier = class(TInterfacedObject, IOTAModuleNotifier, IOTANotifier)
  private
    FPrj : IOTAProject;
  protected
    procedure AfterSave;
    procedure BeforeSave;
    procedure Modified;
    procedure Destroyed;
    function  CheckOverwrite : boolean;
    procedure ModuleRenamed (const newName: string);
  public
    constructor Create (var prj: IOTAProject);
  end;

var
  FMenuItem1        : TMenuItem = nil;
  FMenuItem2        : TMenuItem = nil;
  FIDENotifier      : IOTAIDENotifier;
  FIDENotifierIndex : integer;
  FCurrentPrj       : integer = -1;
  {$ifndef ver120}{$ifndef ver130}{$ifndef ver140}{$ifndef ver150}{$ifndef ver160}
    FSplashBmp        : dword;
  {$endif}{$endif}{$endif}{$endif}{$endif}
  FAmCompiling      : boolean = false;

var Projects : array of TProject;

function CutFileExt(var path: string) : string;
var i1 : integer;
begin
  i1 := PosStr('.', path, maxInt, 1);
  if i1 <> 0 then begin
    result := Copy(path, i1, maxInt);
    Delete(path, i1, maxInt);
  end;
end;

function PrjFileName(const prj: IUnknown) : string;
begin
  try
    result := IOTAProject(prj).FileName;
    if IsTextEqual(ExtractFileExt(result), '.bdsproj') then
      try
        result := IOTAProject(prj).GetModuleFileEditor(0).GetFileName;
      except
      end;
  except result := '' end;
end;

procedure GetProjectRoot(const project: TProject; var root: string);
begin
  root := PrjFileName(project.FPrj);
  CutFileExt(root);
end;

procedure SetProjectModified;
begin
  if FCurrentPrj > -1 then
    with IOTAProject(Projects[FCurrentPrj].FPrj).ProjectOptions do begin
      log('current project: ' + Projects[FCurrentPrj].FCurrentFileName + '/' + IOTAProject(Projects[FCurrentPrj].FPrj).FileName);
      Values['MapFile'] := Values['MapFile'];
      {$ifdef ver180}
        // C++ 2006 needs more than resetting 'MapFile'
        ModifiedState := true;
      {$endif}
    end;
  log('SetProjectModified');
end;

// ***************************************************************

procedure ExpandVars(var str: string);
var i1, i2 : integer;
    s1, s2 : string;
begin
  s1 := str;
  repeat
    str := s1;
    i1 := PosStr('$(', s1);
    while i1 > 0 do begin
      for i2 := i1 + 2 to Length(s1) do
        if s1[i2] = ')' then begin
          s1[i2] := '%';
          Delete(s1, i1, 1);
          s1[i1] := '%';
          break;
        end;
      i1 := PosStr('$(', s1, i1);
    end;
    i1 := ExpandEnvironmentStrings(pchar(s1), nil, 0);
    if i1 > 0 then begin
      SetLength(s2, i1);
      if ExpandEnvironmentStrings(pchar(s1), pchar(s2), i1) > 0 then
        s1 := pchar(s2);
    end;
  until str = s1;
end;

function GetLibraryStr(str: string) : string;
var s1     : string;
    hk1    : HKEY;
    c1, c2 : dword;
begin
  result := '';
  s1 := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\Library';
  if (s1 <> '') and (s1[1] = '\') then
    Delete(s1, 1, 1);
  if RegOpenKeyEx(HKEY_CURRENT_USER, pchar(s1), 0, KEY_QUERY_VALUE, hk1) = 0 then
    try
      c2 := 0;
      if RegQueryValueEx(hk1, pchar(str), nil, nil, nil, @c2) = 0 then begin
        c1 := REG_SZ;
        SetLength(result, c2 + 1);
        if (RegQueryValueEx(hk1, pchar(str), nil, @c1, pointer(result), @c2) = 0) and (c2 > 1) then
             SetLength(result, c2 - 1)
        else result := '';
      end;
    finally RegCloseKey(hk1) end;
end;

function GetRootDir : string;
var s3     : string;
    hk1    : HKEY;
    c1, c2 : dword;
    arrCh  : array [0..MAX_PATH] of char;
begin
  result := '';
  s3 := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey;
  Delete(s3, 1, 1);
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, pchar(s3), 0, KEY_QUERY_VALUE, hk1) = 0 then
    try
      c2 := MAX_PATH + 1;
      if (RegQueryValueEx(hk1, 'RootDir', nil, @c1, @arrCh, @c2) = 0) and (c2 > 1) then begin
        arrCh[c2] := #0;
        result := arrCh;
      end;
    finally RegCloseKey(hk1) end;
  if (result <> '') and (result[Length(result)] = '\') then
    DeleteR(result, 1);
end;

function FindUnitPath(const project: IOTAProject; unitName: string) : string;

  function CheckPaths(paths: string) : string;
  var i1     : integer;
      s1, s2 : string;
  begin
    result := '';
    FormatSubStrs(paths, ';');
    for i1 := 1 to SubStrCount(paths, ';') do begin
      s1 := SubStr(paths, i1, ';');
      if s1 <> '' then begin
        if (Length(s1) = 1) or
           ( ((s1[2] <> ':')                      ) and
             ((s1[1] <> '$') or (s1[2] <> '(')) and
             ((s1[1] <> '\') or (s1[2] <> '\'))     ) then
          if s1[1] = '\' then
                 s1 := ExtractFileDrive(PrjFileName(project)) + s1
          else   s1 := ExtractFilePath (PrjFileName(project)) + s1;
        ExpandVars(s1);
        s2 := GetRootDir;
        if s2 <> '' then begin
          ReplaceText(s1, '%delphi%', s2);
          ReplaceText(s1, '%bcb%',    s2);
        end;
        if s1[Length(s1)] <> '\' then
          s1 := s1 + '\';
        s1 := s1 + unitName;
        if GetFileAttributes(pchar(s1)) <> dword(-1) then begin
          result := s1;
          exit;
        end;
        if GetFileAttributes(pchar(s1 + '.dpr')) <> dword(-1) then begin
          result := s1 + '.dpr';
          exit;
        end;
        if GetFileAttributes(pchar(s1 + '.pas')) <> dword(-1) then begin
          result := s1 + '.pas';
          exit;
        end;
        if GetFileAttributes(pchar(s1 + '.inc')) <> dword(-1) then begin
          result := s1 + '.inc';
          exit;
        end;
        if GetFileAttributes(pchar(s1 + '.asm')) <> dword(-1) then begin
          result := s1 + '.cpp';
          exit;
        end;
        if GetFileAttributes(pchar(s1 + '.bpr')) <> dword(-1) then begin
          result := s1 + '.bpr';
          exit;
        end;
        if GetFileAttributes(pchar(s1 + '.cpp')) <> dword(-1) then begin
          result := s1 + '.cpp';
          exit;
        end;
        if GetFileAttributes(pchar(s1 + '.c')) <> dword(-1) then begin
          result := s1 + '.c';
          exit;
        end;
        if GetFileAttributes(pchar(s1 + '.h')) <> dword(-1) then begin
          result := s1 + '.h';
          exit;
        end;
      end;
    end;
  end;

var i1 : integer;
begin
  result := '';
  if IsTextEqual(ExtractFileName(PrjFileName(project)), unitName + '.bpr') and
     (GetFileAttributes(pchar(PrjFileName(project))) <> dword(-1)) then begin
    result := PrjFileName(project);
    exit;
  end;
  for i1 := 0 to project.GetModuleCount - 1 do
    try
      with project.GetModule(i1) do
        if IsTextEqual(Name, unitName) and (GetFileAttributes(pchar(FileName)) <> dword(-1)) then begin
          result := FileName;
          exit;
        end;
    except end;
  result := CheckPaths(ExtractFilePath(PrjFileName(project)));
  if result = '' then begin
    result := CheckPaths(project.ProjectOptions.Values['UnitDir']);
    if result = '' then begin
      result := CheckPaths(GetLibraryStr('Search Path'));
      if result = '' then
        result := CheckPaths(GetLibraryStr('Browsing Path'));
    end;
  end;
end;

function OpenUnitLine(const project: IOTAProject; unitName: string; line: integer) : boolean;

  function HandleSourceEditor(const editor: IOTASourceEditor) : boolean;

    function InvalidateEditControl(window, dummy: dword) : bool; stdcall;
    var arrCh : array [0..MAX_PATH] of char;
    begin
      result := (GetClassName(window, arrCh, MAX_PATH) = 0) or (arrCh <> 'TEditControl');
      if not result then
        InvalidateRect(window, nil, true);
    end;

  var pos : TOTAEditPos;
      wnd : dword;
      tid : dword;
  begin
    result := false;
    editor.Show;
    if editor.GetEditViewCount > 0 then
      with editor.GetEditView(0) do begin
        pos.Col  := 1;
        pos.Line := line;
        SetCursorPos(pos);
        if pos.Line > 10 then
             dec(pos.Line, 10)
        else pos.Line := 1;
        SetTopPos(pos);
        wnd := FindWindow('TEditWindow', nil);
        if wnd = 0 then
          wnd := FindWindow('TAppBuilder', nil);
        EnumChildWindows(wnd, @InvalidateEditControl, 0);
        tid := GetWindowThreadProcessId(GetForegroundWindow, nil);
        if tid <> 0 then
          AttachThreadInput(GetCurrentThreadId, tid, true);
        SetForegroundWindow(wnd);
        if tid <> 0 then
          AttachThreadInput(GetCurrentThreadId, tid, false);
        result := true;
      end;
  end;

  function HandleModule(const module: IOTAModule) : boolean;
  var i1     : integer;
      editor : IOTASourceEditor;
      s1     : string;
  begin
    result := false;
    if module <> nil then begin
      s1 := module.GetFileName;
      if IsTextEqual(ExtractFileExt(s1), '.bdsproj') then
        try
          s1 := module.GetModuleFileEditor(0).GetFileName;
        except
        end;
      if IsTextEqual(unitName, s1) then
        for i1 := 0 to module.GetModuleFileCount - 1 do
          if module.GetModuleFileEditor(i1).QueryInterface(IOTASourceEditor, editor) = 0 then begin
            result := HandleSourceEditor(editor);
            if result then
              break;
          end;
    end;
  end;

  function HandleModuleInfo(const moduleInfo: IOTAModuleInfo) : boolean;
  begin
    result := IsTextEqual(unitName, moduleInfo.FileName) and
              HandleModule(moduleInfo.OpenModule);
  end;

var i1 : integer;
begin
  result := false;
  log('OpenUnitLine "' + unitName + '", ' + IntToStrEx(line));
  unitName := FindUnitPath(project, unitName);
  log('  FindUnitPath -> "' + unitName + '"');
  if unitName <> '' then begin
    try
      if HandleModule(project) then begin
        log('  HandleModule(project) -> +');
        result := true;
        exit;
      end;
    except end;
    try
      if HandleModule((BorlandIDEServices as IOTAModuleServices).FindModule(unitName)) then begin
        log('  ModuleServices.FindModule(unitName) -> +');
        result := true;
        exit;
      end;
    except end;
    try
      for i1 := 0 to project.GetModuleCount - 1 do
        if HandleModuleInfo(project.GetModule(i1)) then begin
          log('  project.GetModule(' + IntToStrEx(i1) + ') -> +');
          result := true;
          exit;
        end;
    except end;
    try
      if (BorlandIDEServices as IOTAActionServices).OpenFile(unitName) then begin
        log('  ActionServices.OpenFile(unitName) -> +');
        result := HandleModule((BorlandIDEServices as IOTAModuleServices).FindModule(unitName));
        log('  ModuleServices.FindModule(unitName) -> ' + booleanToChar(result));
      end else
        log('  ActionServices.OpenFile(unitName) -> -');
    except end;
  end;
end;

procedure CheckCurrentProject(shutdown: boolean = false); forward;

procedure ExtractExtFromDpr(s1: string; var ext: string; noDpk: boolean);
var i1, i2 : integer;
    s2     : string;
begin
  i1 := 1;
  while i1 < Length(s1) do
    case s1[i1] of
      '''': begin
              i2 := PosStr('''', s1, i1 + 1);
              if i2 = 0 then
                i2 := maxInt;
              Delete(s1, i1, i2 - i1 + 1);
            end;
      '/' : if s1[i1 + 1] = '/' then begin
              i2 := PosChars([#$D, #$A], s1, i1 + 2);
              if i2 = 0 then
                i2 := maxInt;
              Delete(s1, i1, i2 - i1);
            end else
              inc(i1);
      '{' : begin
              i2 := PosStr('}', s1, i1 + 1);
              if i2 = 0 then
                i2 := maxInt;
              s2 := Copy(s1, i1, i2 - i1 + 1);
              if (Length(s2) > 6) and (s2[2] = '$') and (UpChar(s2[3]) = 'E') and (s2[4] = ' ') then begin
                s2 := Copy(s2, 5, Length(s2) - 5);
                TrimStr(s2);
                if (s2 <> '') and (s2[1] = '.') then
                  Delete(s2, 1, 1);
                if s2 <> '' then begin
                  ext := s2;
                  exit;
                end;
              end;
              Delete(s1, i1, i2 - i1 + 1);
            end;
      '(' : if s1[i1 + 1] = '*' then begin
              i2 := PosStr('*)', s1, i1 + 2);
              if i2 > 0 then
                   inc(i2)
              else i2 := maxInt;
              Delete(s1, i1, i2 - i1 + 1);
            end else
              inc(i1);
      else  inc(i1);
    end;
  if noDpk then begin
    i1 := PosText('library ', s1);
    i2 := PosText('program ', s1);
    if (i1 > 0) and ((i2 = 0) or (i1 < i2)) then
      ext := 'dll';
  end;
end;

function FindMapFile : string;

  function RetAddBackslash(const path: string) : string;
  begin
    if (path <> '') and (path[Length(path)] <> '\') then
         result := path + '\'
    else result := path;
  end;

var root    : string;
    output  : string;
    mapFile : string;
    ext     : string;
    binary  : string;
    s1      : string;
    i1, i2  : integer;
    c1, c2  : cardinal;
    hk1     : HKEY;
    isDpk   : boolean;
    arrCh   : array [0..MAX_PATH] of char;
    bcb     : boolean;
begin
  result := '';
  log('FindMapFile'); indentLog;
  try
    if (FCurrentPrj > -1) and (Projects[FCurrentPrj].FPrj <> nil) then
      with Projects[FCurrentPrj] do begin
        GetProjectRoot(Projects[FCurrentPrj], root);
        log('root: "' + root + '"');
        ext := ExtractFileExt(PrjFileName(FPrj));
        isDpk := IsTextEqual(ext, '.dpk') or IsTextEqual(ext, '.bpk');
        bcb := (Length(ext) > 1) and (ext[2] in ['b'..'c', 'B'..'C']);
        log('isDpk: ' + booleanToChar(isDpk));
        binary := '';
        {$ifndef ver120}{$ifndef ver130}{$ifndef ver140}{$ifndef ver150}
          binary := IOTAProject(FPrj).ProjectOptions.GetTargetName;
        {$endif}{$endif}{$endif}{$endif}
        if binary <> '' then begin
          log('binary: "' + binary + '"');
        end else begin
          if isDpk and (not BCB) then begin
            output := IOTAProject(FPrj).ProjectOptions.Values['PkgDllDir'];
            log('ProjectOption.PkgDllDir: "' + output + '"');
            if (output = '0') and
               (GetPrivateProfileString('Directories', 'PackageDLLOutputDir', '0',
                                        arrCh, MAX_PATH, pchar(root + '.dof')) > 0) then begin
              log('dof.PackageDLLOutputDir: "' + arrCh + '"');
              output := arrCh;
            end;
          end else begin
            output := IOTAProject(FPrj).ProjectOptions.Values['OutputDir'];
            log('ProjectOption.OutputDir: "' + output + '"');
          end;
          if isDpk then begin
            s1 := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\Library';
            Delete(s1, 1, 1);
            if ((output = '') or (output = '0')) and
               (RegOpenKeyEx(HKEY_CURRENT_USER, pchar(s1), 0, KEY_QUERY_VALUE, hk1) = 0) then
              try
                c2 := MAX_PATH + 1;
                if (RegQueryValueEx(hk1, 'Package DPL Output', nil, @c1, @arrCh, @c2) = 0) and (c2 > 1) then begin
                  arrCh[c2] := #0;
                  log('registry\Package DPL Output: "' + arrCh + '"');
                  output := arrCh;
                end;
              finally RegCloseKey(hk1) end;
          end;
          if (output <> '') and (output <> '0') then begin
            if (Length(output) = 1) or
               ( ((output[2] <> ':')                      ) and
                 ((output[1] <> '$') or (output[2] <> '(')) and
                 ((output[1] <> '\') or (output[2] <> '\'))     ) then begin
              if output[1] = '\' then
                     output := RetAddBackslash(ExtractFileDrive(root) + output) + ExtractFileName(root)
              else   output := RetAddBackslash(ExtractFilePath (root) + output) + ExtractFileName(root);
            end else output := RetAddBackslash(                         output) + ExtractFileName(root);
          end else   output :=                                                                    root;
          log('output: "' + output + '"');
          ExpandVars(output);
          log('output (after expanding): "' + output + '"');
          s1 := GetRootDir;
          if s1 <> '' then begin
            log('registry\RootDir: "' + s1 + '"');
            ReplaceText(output, '%delphi%', s1);
            ReplaceText(output, '%bcb%',    s1);
          end;
          if BCB then
               ext := IOTAProject(FPrj).ProjectOptions.Values['AppFileExt']
          else ext := '';
          if ext = '' then begin
            if isDpk then
                 ext := 'bpl'
            else ext := 'exe';
            c1 := CreateFile(pchar(PrjFileName(FPrj)),
                             GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
            if c1 <> INVALID_HANDLE_VALUE then
              try
                SetLength(s1, GetFileSize(c1, nil));
                if ReadFile(c1, pointer(s1)^, Length(s1), c2, nil) and (c2 = dword(Length(s1))) then begin
                  if not isDpk then
                    if BCB then begin
                      i1 := PosText('<PROJECT value="', s1);
                      if i1 > 0 then begin
                        i2 := PosStr('"/>', s1, i1);
                        if i2 > 0 then begin
                          if s1[i1 + 17] in [':', '\'] then
                               output := ''
                          else output := ExtractFilePath(output);
                          if (output <> '') and (s1[i1 + 16] = '\') then
                            inc(i1);
                          output := output + Copy(s1, i1 + 16, i2 - i1 - 16);
                          ext := CutFileExt(output);
                          Delete(ext, 1, 1);
                        end;
                      end;
                    end else
                      ExtractExtFromDpr(s1, ext, true);
                  if not BCB then
                    ExtractExtFromDpr(s1, ext, false);
                end;
              finally CloseHandle(c1) end;
          end;
          if ext <> '' then begin
            {$ifdef d6}
              s1 := IOTAProject(FPrj).ProjectOptions.Values['SOSuffix'];
              if s1 <> '' then
                output := output + s1;
            {$endif}
            binary := output + '.' + ext;
            {$ifdef d6}
              s1 := IOTAProject(FPrj).ProjectOptions.Values['SOVersion'];
              if s1 <> '' then begin
                if s1[1] <> '.' then
                  s1 := '.' + s1;
                binary := binary + s1;
              end;
              s1 := IOTAProject(FPrj).ProjectOptions.Values['SOPrefix'];
              if s1 <> '' then
                binary := ExtractFilePath(binary) + s1 + ExtractFileName(binary);
            {$endif}
            log('binary: "' + binary + '"');
          end;
        end;
        if binary <> '' then begin
          mapFile := binary;
          Delete(mapFile, PosStr('.', mapFile, maxInt, 1), maxInt);
          mapFile := mapFile + '.map';
          log('map file: "' + mapFile + '"');
          if GetFileAttributes(pchar(mapFile)) <> dword(-1) then
            result := mapFile;
        end;
      end;
    log('FindMapFile done');
  finally unindentLog end;
end;

procedure JumpToBugReportLine(line: string);

  function IsOrdinal(str: string) : boolean;
  var i1 : integer;
  begin
    if (str <> '') and (str[1] = '+') then
      Delete(str, 1, 1);
    result := str <> '';
    for i1 := 1 to Length(str) do
      if not (str[i1] in ['0'..'9', 'a'..'f', 'A'..'F']) then begin
        result := false;
        break;
      end;
  end;

var addrAbs, addrOff        : pointer;
    lineAbs, lineOff        : integer;
    sModule, sUnit, sPublic : string;
    i1                      : integer;
    s1                      : string;
begin
  CheckCurrentProject;
  if FCurrentPrj = -1 then
    exit;
  addrAbs := nil;
  addrOff := pointer(-1);
  lineAbs := 0;
  lineOff := -1;
  sModule := '';
  sUnit   := '';
  sPublic := '';
  FormatSubStrs(line, ' ');
  for i1 := 1 to SubStrCount(line, ' ') do begin
    s1 := SubStr(line, i1, ' ');
    if s1 <> '' then begin
      if IsOrdinal(s1) then begin
        if s1[1] = '+' then begin
          Delete(s1, 1, 1);
          if lineAbs <> 0 then
               lineOff :=         StrToIntDef(      s1, -1)
          else addrOff := pointer(StrToIntDef('$' + s1, -1));
        end else
          if addrAbs <> nil then
               lineAbs :=         StrToIntDef(      s1, -1)
          else addrAbs := pointer(StrToIntDef('$' + s1, -1));
      end else
        if sModule = '' then
          sModule := s1
        else if sUnit = '' then
          sUnit := s1
        else
          sPublic := s1;
    end;
  end;
  if sUnit <> '' then begin
    if (sPublic <> '') and ((lineOff <> -1) or (addrOff <> pointer(-1))) then begin
      s1 := FindMapFile;
      if s1 <> '' then
        with LoadMapFileEx(s1, true) do begin
          if IsValid then begin
            addrAbs := pointer(FindPublic(true, sUnit, sPublic));
            if addrAbs <> nil then
              if addrOff <> pointer(-1) then begin
                if FindPublic(pointer(dword(addrAbs) + dword(addrOff))).Address = addrAbs then begin
                  i1 := FindLine(pointer(dword(addrAbs) + dword(addrOff)));
                  if i1 > 0 then
                    lineAbs := i1;
                end;
              end else begin
                i1 := FindLine(addrAbs);
                if i1 > 0 then
                  lineAbs := i1 + lineOff;
              end;
          end;
          Free;
        end;
    end;
    if lineAbs = 0 then
      lineAbs := 1;
    OpenUnitLine(IOTAProject(Projects[FCurrentPrj].FPrj), sUnit, lineAbs);
  end;
end;

{$ifdef TestProjectOptions}
  procedure PostWarning(project, warning: string; logWarning: boolean = true); forward;
  procedure ListAllProjectOptions(project: IOTAProject);
  var i1    : integer;
      names : TOTAOptionNameArray;
      ss    : string[255];
  begin
    names := nil;
    if project <> nil then begin
      names := project.ProjectOptions.GetOptionNames;
      for i1 := 0 to high(names) do
        try
          case names[i1].Kind of
            tkInteger, tkInt64, tkEnumeration, tkSet:
              try
                PostWarning('', names[i1].Name + ' (integer = ' + IntToStr(ord(names[i1].Kind)) + ') -> ' + IntToStr(project.ProjectOptions.Values[names[i1].Name]));
              except
                PostWarning('', names[i1].Name + ' (integer = ' + IntToStr(ord(names[i1].Kind)) + ') -> exception');
              end;
            tkLString:
              try
                PostWarning('', names[i1].Name + ' (string = ' + IntToStr(ord(names[i1].Kind)) + ') -> "' + project.ProjectOptions.Values[names[i1].Name] + '"');
              except
                PostWarning('', names[i1].Name + ' (string = ' + IntToStr(ord(names[i1].Kind)) + ') -> exception');
              end;
            tkString:
              try
                ss := project.ProjectOptions.Values[names[i1].Name];
                PostWarning('', names[i1].Name + ' (string = ' + IntToStr(ord(names[i1].Kind)) + ') -> "' + ss + '"');
              except
                PostWarning('', names[i1].Name + ' (string = ' + IntToStr(ord(names[i1].Kind)) + ') -> exception');
              end;
            else
              PostWarning('', names[i1].Name + ' (' + IntToStr(ord(names[i1].Kind)) + ') -> unknown');
          end;
  //  TTypeKind = (tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
  //               tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
  //               tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray);
        except
          PostWarning('', names[i1].Name + ' -> exception');
        end;
    end;
  end;
{$endif}

// ***************************************************************

function ShowDelphiForm(className: string) : boolean;
var i1 : integer;
begin
  result := false;
  for i1 := 0 to Screen.FormCount - 1 do
    if Screen.Forms[i1].ClassNameIs(className) then begin
      if not Screen.Forms[i1].Visible then
        Screen.Forms[i1].Show;
      result := true;
      break;
    end;
end;

procedure PostWarning(project, warning: string; logWarning: boolean = true);
begin
  if logWarning then
    log('warning "' + warning + '"');
  if (project <> '') and (Length(Projects) > 1) then
    warning := ExtractFileName(project) + ': ' + warning;
  (BorlandIDEServices as IOTAMessageServices).AddToolMessage('', warning, 'madExcept', 0, 0);
  if not ShowDelphiForm('TMsgWindow') then
    ShowDelphiForm('TMessageViewForm');
end;

procedure CheckCurrentProject(shutdown: boolean = false);

  function UnknownQueryInterface(const unknown: IUnknown; const IID: TGUID; var obj) : boolean;
  begin
    try // D4 crashes here sometimes
      result := (unknown <> nil) and (unknown.QueryInterface(IID, obj) = 0);
    except result := false end;
  end;

var prjs : array of IOTAProject;

  procedure AddProject(prj: IOTAProject);
  var i1 : integer;
  begin
    for i1 := 0 to high(prjs) do
      if prjs[i1] = prj then
        exit;
    SetLength(prjs, Length(prjs) + 1);
    prjs[high(prjs)] := prj;
  end;

var ap, p1 : IOTAProject;
    prjg   : IOTAProjectGroup;
    i1, i2 : Integer;
    b1     : boolean;
    ab     : TDABoolean;
    s1, s2 : string;
begin
  prjs := nil;
  ap   := nil;
  if not shutdown then
    with BorlandIDEServices as IOTAModuleServices do
      for i1 := 0 to ModuleCount - 1 do
        if UnknownQueryInterface(Modules[i1], IOTAProjectGroup, prjg) then begin
          log('[' + IntToStrEx(i1) + '] project group');
          for i2 := 0 to prjg.ProjectCount - 1 do
            try // D4+5 crash here for ".bat" projects
              s1 := '    [' + IntToStrEx(i2) + '] project "' + prjg.Projects[i2].FileName + '"';
              try
                s1 := s1 + '; moduleEditor[0]: "' + prjg.Projects[i2].GetModuleFileEditor(0).GetFileName + '"';
              except end;
              log(s1);
              AddProject(prjg.Projects[i2]);
              ap := prjg.Projects[i2];
            except end;
          try // D4+5 crash here for ".bat" projects
            ap := prjg.ActiveProject;
          except end;
          break;
        end else
          if UnknownQueryInterface(Modules[i1], IOTAProject, p1) then begin
            s1 := '[' + IntToStrEx(i1) + '] project "' + p1.FileName + '"';
            try
              s1 := s1 + '; moduleEditor[0]: "' + p1.GetModuleFileEditor(0).GetFileName + '"';
            except end;
            log(s1);
            AddProject(p1);
            if ap = nil then
              ap := p1;
          end else
            log('[' + IntToStrEx(i1) + '] module "' + Modules[i1].FileName + '"');
  SetLength(ab, Length(Projects));
  for i1 := 0 to high(prjs) do begin
    b1 := true;
    for i2 := 0 to high(Projects) do
      if prjs[i1] = Projects[i2].FPrj then begin
        b1 := false;
        ab[i2] := true;
      end;
    if b1 then begin
      i2 := Length(Projects);
      SetLength(Projects, i2 + 1);
      with Projects[i2] do begin
        log('New project detected: "' + PrjFileName(prjs[i1]) + '"');
        FPrj               := prjs[i1];
        FCurrentFileName   := PrjFileName(FPrj);
        FModified          := false;
        FSaveNotifierIndex := IOTAProject(FPrj).AddNotifier(TProjectSaveNotifier.Create(IOTAProject(FPrj)));
        FLastBinaryTime    := -1;
        FLastBinarySize    := 0;
        LoadDefaultSettings(Projects[i2]);
        GetProjectRoot(Projects[i2], s1);
        s2 := ExtractFileExt(PrjFileName(FPrj));
        if IsTextEqual(s2, '.dpr') or IsTextEqual(s2, '.dpk') or
           IsTextEqual(s2, '.bpr') or IsTextEqual(s2, '.bpk') or IsTextEqual(s2, '.cpp') or IsTextEqual(s2, '.bpf') then begin
          if s1 <> '' then begin
            LoadSettingsFromIni(s1, Projects[i2]);
            if MadExcept2Only then begin
              Enabled := false;
              MessageBox(0, 'This project may only be used with madExcept 2.', nil, MB_ICONERROR);
            end;
          end;
        end else
          Enabled := false;
      end;
    end;
  end;
  for i1 := high(ab) downto 0 do
    if not ab[i1] then begin
      log('Project closed: "' + Projects[i1].FCurrentFileName + '"');
      try
        try
          if Projects[i1].FPrj <> nil then
            IOTAProject(Projects[i1].FPrj).RemoveNotifier(Projects[i1].FSaveNotifierIndex);
        except end;
        Projects[i1] := Projects[high(Projects)];
        SetLength(Projects, high(Projects));
      except end;
    end;
  if Projects <> nil then begin
    FCurrentPrj := 0;
    for i1 := 0 to high(Projects) do
      if Projects[i1].FPrj = ap then begin
        FCurrentPrj := i1;
        break;
      end;
  end else
    FCurrentPrj := -1;
end;

function Reg_ValExists(key: HKEY; const path, name: string) : boolean;
var len : dword;
begin
  result := false;
  if RegOpenKeyEx(key, pchar(path), 0, KEY_QUERY_VALUE, key) = 0 then begin
    result := (RegQueryValueEx(key, pchar(name), nil, nil, nil, @len) = 0) and (len > 1);
    RegCloseKey(key);
  end;
end;

function ExtractUsesClause(var project: TProject) : string;

  procedure GetReader(const ed: IOTASourceEditor; var reader: IOTAEditReader);
  begin
    reader := ed.CreateReader;
  end;

var s1     : string;
    ed     : IOTASourceEditor;
    reader : IOTAEditReader;
    i1, i2 : integer;
begin
  result := '';
  try
    ed := IOTAProject(project.FPrj).GetModuleFileEditor(0) as IOTASourceEditor;
    SetLength(s1, $8000);
    GetReader(ed, reader);
    SetLength(s1, reader.GetText(0, pchar(s1), $8000 - 1));
    reader := nil;
    i1 := 1;
    while i1 < Length(s1) do
      case s1[i1] of
        '''': begin
                i2 := PosStr('''', s1, i1 + 1);
                if i2 = 0 then
                  i2 := maxInt;
                Delete(s1, i1, i2 - i1 + 1);
              end;
        '/' : if s1[i1 + 1] = '/' then begin
                i2 := PosChars([#$D, #$A], s1, i1 + 2);
                if i2 = 0 then
                  i2 := maxInt;
                Delete(s1, i1, i2 - i1);
              end else
                inc(i1);
        '{' : begin
                i2 := PosStr('}', s1, i1 + 1);
                if i2 = 0 then
                  i2 := maxInt;
                Delete(s1, i1, i2 - i1 + 1);
              end;
        '(' : if s1[i1 + 1] = '*' then begin
                i2 := PosStr('*)', s1, i1 + 2);
                if i2 > 0 then
                     inc(i2)
                else i2 := maxInt;
                Delete(s1, i1, i2 - i1 + 1);
              end else
                inc(i1);
        #$D, #$A:
              begin
                s1[i1] := ' ';
                inc(i1);
              end;
        else  inc(i1);
      end;
    KillChars(s1, [#0..#255] - ['a'..'z', 'A'..'Z', '_', '1'..'9', '0', ',', ';', ' ', #$D, #$A]);
    i1 := PosText(' uses ', s1);
    i2 := PosText(';uses ', s1);
    if (i1 = 0) or ((i2 > 0) and (i2 < i1)) then
      i1 := i2;
    if i1 > 0 then begin
      Delete(s1, 1, i1 + 5);
      i1 := PosStr(';', s1);
      if i1 > 0 then
        Delete(s1, i1, maxInt);
      KillStr(s1, ' in ');
      KillStr(s1, ' in,');
      KillStr(s1, ' in;');
      KillChar(s1, ' ');
      result := s1;
    end;
    log('extracted unit clause: "' + result + '"');
    result := ',' + result + ',';
  except end;
end;

function ModifyProjectUses(var project: TProject; unitName, afterUnitName: string; addUnit: boolean) : boolean;

  procedure GetReader(const ed: IOTASourceEditor; var reader: IOTAEditReader);
  begin
    reader := ed.CreateReader;
  end;

var s1     : string;
    i1, i2 : integer;
    b2     : boolean;
    ed     : IOTASourceEditor;
    reader : IOTAEditReader;
    pos    : integer;
begin
  result := false;
  try
    ed := IOTAProject(project.FPrj).GetModuleFileEditor(0) as IOTASourceEditor;
    SetLength(s1, $8000);
    GetReader(ed, reader);
    SetLength(s1, reader.GetText(0, pchar(s1), $8000 - 1));
    reader := nil;
    if addUnit and (PosText('{%DelphiDotNetAssemblyCompiler', s1) > 0) then begin
      if project.Enabled then begin
        PostWarning(PrjFileName(project.FPrj), 'madExcept doesn''t support DotNet yet, so it has turned itself off for this project');
        project.Enabled := false;
      end;
      exit;
    end;
    try
      log('we have to adjust the project''s uses clause (' + unitName + ')');
      if (not Reg_ValExists(HKEY_CURRENT_USER,  'Software\madshi\madExcept', 'dontTouchUses')) and
         (not Reg_ValExists(HKEY_LOCAL_MACHINE, 'Software\madshi\madExcept', 'dontTouchUses')) and
         (PosText('dontTouchUses', s1) = 0) and
         (not project.MadExcept2Only) and
         AdjustUses then begin
        i1 := 1;
        pos := 0;
        b2 := false;
        while (i1 > 0) and (i1 <= Length(s1)) do
          case UpChar(s1[i1]) of
            '{' : i1 := PosStr('}', s1, i1 + 1);
            '''': begin
                    i1 := PosStr('''', s1, i1 + 1);
                    if i1 > 0 then
                      inc(i1);
                  end;
            ';', 'B', 'E', 'P':
                  if (pos > 0) and ((s1[i1] = ';') or (not b2)) then begin
                    if addUnit then begin
                      with ed.CreateWriter do begin
                        CopyTo(pos);
                        Insert(pchar(#$D#$A + #$D#$A + 'uses ' + unitName + ';'));
                      end;
                      result := true;
                    end;
                    exit;
                  end else begin
                    if s1[i1] = ';' then
                      pos := i1;
                    inc(i1);
                  end;
            ',' : begin
                    if b2 then
                      pos := i1;
                    inc(i1);
                  end;
            'U' : if (pos > 0) and (i1 + 4 < Length(s1)) and (PosText('uses', s1, i1) = i1) and
                     (s1[i1 + 4] in [' ', '(', '/', '{', #$D, #$A]) then begin
                    if addUnit and (afterUnitName = '') then begin
                      with ed.CreateWriter do begin
                        if s1[i1 + 4] in [#$D, #$A] then begin
                          CopyTo(i1 + 5);
                          Insert(pchar('  ' + unitName + ',' + #$D#$A));
                        end else begin
                          CopyTo(i1 + 3);
                          Insert(pchar(' ' + unitName + ','));
                        end;
                      end;
                      result := true;
                      exit;
                    end else begin
                      pos := i1;
                      b2 := true;
                      inc(i1);
                    end;
                  end else
                    inc(i1);
            else  if (s1[i1] = '(') and (i1 < Length(s1)) and (s1[i1 + 1] = '*') then
                    i1 := PosStr('*)', s1, i1 + 2)
                  else
                    if (s1[i1] = '/') and (i1 < Length(s1)) and (s1[i1 + 1] = '/') then
                      i1 := PosChars([#$D, #$A], s1, i1 + 2)
                    else
                      if (s1[i1] in ['a'..'z', 'A'..'Z', '_']) and b2 and
                         (pos > 0) and (i1 + Length(afterUnitName) < Length(s1)) and
                         (PosText(afterUnitName, s1, i1) = i1) and
                         (s1[i1 + Length(afterUnitName)] in [' ', ',', ';', '(', '/', '{', #$D, #$A]) then begin
                        i2 := i1 + Length(afterUnitName);
                        while (i2 > 0) and (i2 < Length(s1)) do
                          case s1[i2] of
                            '{'      : i2 := PosStr('}', s1, i2 + 1);
                            ''''     : begin
                                         i2 := PosStr('''', s1, i2 + 1);
                                         if i2 > 0 then
                                           inc(i2);
                                       end;
                            ',', ';' : if addUnit then begin
                                         with ed.CreateWriter do begin
                                           CopyTo(i2 - 1);
                                           if (i1 > 4) and
                                              (s1[i1 - 1] = ' ') and (s1[i1 - 2] = ' ') and
                                              (s1[i1 - 3] = #$A) and (s1[i1 - 4] = #$D) then
                                             Insert(pchar(',' + #$D#$A + '  ' + unitName))
                                           else
                                             Insert(pchar(', ' + unitName));
                                         end;
                                         result := true;
                                         exit;
                                       end else begin
                                         if (s1[pos] <> ',') and (s1[i2] = ';') then begin
                                           i1 := pos;
                                           if (i1 > 2) and (s1[i1 - 1] = #$A) and (s1[i1 - 2] = #$D) then
                                             dec(i1, 2);
                                           if (i2 + 2 < Length(s1)) and (s1[i2 + 1] = #$D) and (s1[i2 + 2] = #$A) then
                                             inc(i2, 2);
                                         end else
                                           if s1[pos] <> ',' then begin
                                             while s1[i1 - 1] = ' ' do
                                               dec(i1);
                                             if (i2 + 2 < Length(s1)) and (s1[i2 + 1] = #$D) and (s1[i2 + 2] = #$A) then
                                               inc(i2, 2);
                                           end else begin
                                             i1 := pos;
                                             dec(i2);
                                           end;
                                         with ed.CreateWriter do begin
                                           CopyTo(i1 - 1);
                                           DeleteTo(i2);
                                         end;
                                         result := true;
                                         exit;
                                       end;
                            else       if (s1[i2] = '(') and (i2 < Length(s1)) and (s1[i2 + 1] = '*') then
                                         i2 := PosStr('*)', s1, i2 + 2)
                                       else
                                         if (s1[i2] = '/') and (i2 < Length(s1)) and (s1[i2 + 1] = '/') then
                                           i2 := PosChars([#$D, #$A], s1, i2 + 2)
                                         else
                                           inc(i2);
                          end;
                        exit;
                      end else
                        inc(i1);
          end;
      end else
        if addUnit then
          PostWarning(PrjFileName(project.FPrj), 'Please add "' + unitName + '" to the project''s uses clause', true);
    except
      if addUnit then
        PostWarning(PrjFileName(project.FPrj), 'Adding "' + unitName + '" to the project''s uses clause failed', true)
      else
        PostWarning(PrjFileName(project.FPrj), 'Deleting "' + unitName + '" from the project''s uses clause failed', true);
    end;
  except end;
end;

function CheckProjectLinks(var project: TProject; unitName: string; checkOnly, addUnit: boolean) : boolean;

  procedure GetReader(const ed: IOTASourceEditor; var reader: IOTAEditReader);
  begin
    reader := ed.CreateReader;
  end;

  function IsUnitContained(s1: string) : boolean;
  var i1, i2 : integer;
  begin
    i1 := 1;
    while i1 < Length(s1) do
      case s1[i1] of
        '''': begin
                i2 := PosStr('''', s1, i1 + 1);
                if i2 = 0 then
                  i2 := maxInt;
                Delete(s1, i1, i2 - i1 + 1);
              end;
        '/' : if s1[i1 + 1] = '/' then begin
                i2 := PosChars([#$D, #$A], s1, i1 + 2);
                if i2 = 0 then
                  i2 := maxInt;
                Delete(s1, i1, i2 - i1);
              end else
                if s1[i1 + 1] = '*' then begin
                  i2 := PosStr('*/', s1, i1 + 2);
                  if i2 > 0 then
                       inc(i2)
                  else i2 := maxInt;
                  Delete(s1, i1, i2 - i1 + 1);
                end else
                  inc(i1);
        '{' : begin
                i2 := PosStr('}', s1, i1 + 1);
                if i2 = 0 then
                  i2 := maxInt;
                Delete(s1, i1, i2 - i1 + 1);
              end;
        #$D, #$A:
              begin
                s1[i1] := ' ';
                inc(i1);
              end;
        else  inc(i1);
      end;
    KillChars(s1, [#0..#255] - ['a'..'z', 'A'..'Z', '_', '1'..'9', '0', ',', ';', '#', '"', '<', '>', '.']);
    if ExtractFileExt(unitName) <> '' then
      result := (PosText('#include"' + unitName + '"', s1) > 0) or
                (PosText('#include<' + unitName + '>', s1) > 0)
    else
      result := PosText('#pragmalink"' + unitName + '"', s1) > 0;
  end;

var s1     : string;
    ed     : IOTASourceEditor;
    reader : IOTAEditReader;
    i1, i2 : integer;
    b1     : boolean;
begin
  result := false;
  try
    ed := IOTAProject(project.FPrj).GetModuleFileEditor(0) as IOTASourceEditor;
    SetLength(s1, $8000);
    GetReader(ed, reader);
    SetLength(s1, reader.GetText(0, pchar(s1), $8000 - 1));
    reader := nil;
    b1 := not IsUnitContained(s1);
    if checkOnly then
      result := not b1
    else
      try
        if b1 = addUnit then begin
          log('we have to adjust the project file (' + unitName + ')');
          if (not Reg_ValExists(HKEY_CURRENT_USER,  'Software\madshi\madExcept', 'dontTouchUses')) and
             (not Reg_ValExists(HKEY_LOCAL_MACHINE, 'Software\madshi\madExcept', 'dontTouchUses')) and
             (PosText('dontTouchUses', s1) = 0) and
             (not project.MadExcept2Only) and
             AdjustUses then begin
            if addUnit then begin
              with ed.CreateWriter do begin
                i1 := PosText('#pragma hdrstop' + #$D#$A, s1);
                if i1 > 0 then
                  CopyTo(i1 + Length('#pragma hdrstop' + #$D#$A) - 1);
                if ExtractFileExt(unitName) <> '' then
                     Insert(pchar('#include <'     + unitName + '>' + #$D#$A))
                else Insert(pchar('#pragma link "' + unitName + '"' + #$D#$A));
              end;
              result := true;
            end else begin
              if ExtractFileExt(unitName) <> '' then
                   i1 := PosText('#include <'     + unitName + '>', s1)
              else i1 := PosText('#pragma link "' + unitName + '"', s1);
              if i1 > 0 then begin
                with ed.CreateWriter do begin
                  if i1 > 1 then
                    CopyTo(i1 - 1);
                  if s1[i1 + 1] in ['i', 'I'] then
                       i2 := i1 + 10 + Length(unitName)
                  else i2 := i1 + 14 + Length(unitName);
                  if (i2 + 2 < Length(s1)) and (s1[i2 + 1] = #$D) and (s1[i2 + 2] = #$A) then
                    inc(i2, 2);
                  DeleteTo(i2);
                end;
                result := true;
              end;
            end;
          end else
            if addUnit then
              PostWarning(PrjFileName(project.FPrj), 'Please add "' + unitName + '" to the project file', true);
        end else
          log('the project file is alright (' + unitName + ')');
      except
        if not checkOnly then
          if addUnit then
            PostWarning(PrjFileName(project.FPrj), 'Adding "' + unitName + '" links to the project file failed', true)
          else
            PostWarning(PrjFileName(project.FPrj), 'Deleting "' + unitName + '" links from the project file failed', true);
      end;
  except end;
end;

procedure CheckProjectUses(var project: TProject);

  procedure CheckPathOption(option, s2: string);
  var s1, s3 : string;
      b1     : boolean;
      i1     : integer;
  begin
    s1 := IOTAProject(project.FPrj).ProjectOptions.Values[option];
    s3 := GetRootDir;
    if s3 <> '' then
      ReplaceText(s1, '$(BCB)', s3);
    b1 := false;
    for i1 := 1 to SubStrCount(s2, ';') do
      if (not SubTextExists(s1,       SubStr(s2, i1, ';'),        ';')) and
         (not SubTextExists(s1, '"' + SubStr(s2, i1, ';') + '"',  ';')) and
         (not SubTextExists(s1,       SubStr(s2, i1, ';') + '\',  ';')) and
         (not SubTextExists(s1, '"' + SubStr(s2, i1, ';') + '\"', ';')) then begin
        s1 := s1 + ';' + SubStr(s2, i1, ';');
        b1 := true;
      end;
    if b1 then begin
      if s1[1] = ';' then
        Delete(s1, 1, 1);
      if s3 <> '' then
        ReplaceText(s1, s3, '$(BCB)');
      IOTAProject(project.FPrj).ProjectOptions.Values[option] := s1;
    end;
  end;

  procedure CheckUsesClause(uss, unit_: string; addUnit: boolean);
  const CMMs = 'ShareMem|SimpleShareMem|HPMM|MultiMM|nxReplacementMemoryManager|dbisammm|FastMM3|FastMM4|RecyclerMM|SmartHeapMM';

    function FindFirstNonMM : string;
    var i1 : integer;
        s1 : string;
    begin
      result := '';
      for i1 := 1 to SubStrCount(uss, ',') do begin
        s1 := SubStr(uss, i1, ',');
        if (s1 <> '') and SubTextExists(CMMs, s1) then
          result := s1;
      end;
    end;

  var i1, i2 : integer;
      s1     : string;
  begin
    i1 := PosText(',' + unit_ + ',', uss);
    if (i1 > 0) <> addUnit then begin
      if addUnit then
           s1 := FindFirstNonMM
      else s1 := unit_;
      if ModifyProjectUses(project, unit_, s1, addUnit) then
        SetProjectModified;
    end else
      if i1 > 0 then
        for i2 := 1 to SubStrCount(CMMs) do
          if PosText(',' + SubStr(CMMs, i2) + ',', uss) > i1 then begin
            if ModifyProjectUses(project, unit_, unit_, false) then begin
              SetProjectModified;
              if ModifyProjectUses(project, unit_, FindFirstNonMM, true) then
                SetProjectModified;
            end;
            break;
          end;
  end;

var s1, s2 : string;
    i1, i2 : integer;
    b1, b2 : boolean;
    uss    : string;
begin
  if IsTextEqual(ExtractFileExt(PrjFileName(project.FPrj)), '.dpk') or
     IsTextEqual(ExtractFileExt(PrjFileName(project.FPrj)), '.bpk') then
    exit;
  s1 := ExtractFileExt(PrjFileName(project.FPrj));
  if (s1 <> '') and (s1[2] in ['b'..'c', 'B'..'C']) then begin
    for i1 := 0 to high(Plugins) do
      if ( (@Plugins[i1].procOld <> nil) and GetProcInfo(@Plugins[i1].procOld, s1, s2) ) or
         ( (@Plugins[i1].procNew <> nil) and GetProcInfo(@Plugins[i1].procNew, s1, s2) ) then begin
        b1 := false;
        for i2 := 1 to SubStrCount(project.EnabledPlugins) do
          if IsTextEqual(Plugins[i1].name, SubStr(project.EnabledPlugins, i2)) then begin
            b1 := true;
            break;
          end;
        if CheckProjectLinks(project, s1, false, b1 and project.Enabled) then
          SetProjectModified;
      end;
    b1 := project.Enabled and
          ( CheckProjectLinks(project, 'IWMain',           true, false) or
            CheckProjectLinks(project, 'IWInitISAPI',      true, false) or
            CheckProjectLinks(project, 'IWInitApache',     true, false) or
            CheckProjectLinks(project, 'IWInitApache2',    true, false) or
            CheckProjectLinks(project, 'IWInitService',    true, false) or
            CheckProjectLinks(project, 'IWInitStandAlone', true, false)    );
    b2 := false;
    if CheckProjectLinks(project, 'madIWSupport',  false, b1                                               ) then begin
      b2 := true;
      SetProjectModified;
    end;
    if CheckProjectLinks(project, 'madScreenShot', false, false                                            ) then begin
      b2 := true;
      SetProjectModified;
    end;
    if CheckProjectLinks(project, 'madLinkDisAsm', false, project.Enabled and  project.ShowDisAsm          ) then begin
      b2 := true;
      SetProjectModified;
    end;
    if CheckProjectLinks(project, 'madExcept',     false, project.Enabled                                  ) then begin
      b2 := true;
      SetProjectModified;
    end;
    if b2 and project.Enabled and IsTextEqual(ExtractFileExt(PrjFileName(project.FPrj)), '.bpf') then
      PostWarning(PrjFileName(project.FPrj), 'please copy the "#pragma link" lines to the cpp file which contains the DllEntryPoint', false);
    {$ifdef ver130}{$define AdjustIncludePath}{$endif}  // bcb5
    {$ifdef ver140}{$define AdjustIncludePath}{$endif}  // bcb6
    {$ifdef AdjustIncludePath}
      if AdjustOptions and (not project.MadExcept2Only) then begin
        s2 := GetLibraryStr('Include Path');
        CheckPathOption('IncludePath', s2);
        CheckPathOption('LibPath',     s2);
      end;
    {$endif}
  end else begin
    uss := ExtractUsesClause(project);
    for i1 := 0 to high(Plugins) do
      if ( (@Plugins[i1].procOld <> nil) and GetProcInfo(@Plugins[i1].procOld, s1, s2) ) or
         ( (@Plugins[i1].procNew <> nil) and GetProcInfo(@Plugins[i1].procNew, s1, s2) ) then begin
        b1 := false;
        for i2 := 1 to SubStrCount(project.EnabledPlugins) do
          if IsTextEqual(Plugins[i1].name, SubStr(project.EnabledPlugins, i2)) then begin
            b1 := true;
            break;
          end;
        CheckUsesClause(uss, s1, b1 and project.Enabled);
      end;
    b1 := project.Enabled and
          ( SubTextExists(uss, 'IWMain',           ',') or
            SubTextExists(uss, 'IWInitISAPI',      ',') or
            SubTextExists(uss, 'IWInitApache',     ',') or
            SubTextExists(uss, 'IWInitApache2',    ',') or
            SubTextExists(uss, 'IWInitService',    ',') or
            SubTextExists(uss, 'IWInitStandAlone', ',')    );
    CheckUsesClause(uss, 'madIWSupport',  b1);
    CheckUsesClause(uss, 'madScreenShot', false);
    CheckUsesClause(uss, 'madLinkDisAsm', project.Enabled and project.ShowDisAsm);
    CheckUsesClause(uss, 'madExcept',     project.Enabled);
  end;
  if (not Reg_ValExists(HKEY_CURRENT_USER,  'Software\madshi\madExcept', 'dontTouchDefines')) and
     (not Reg_ValExists(HKEY_LOCAL_MACHINE, 'Software\madshi\madExcept', 'dontTouchDefines')) and
     (not project.MadExcept2Only) and
     AdjustDefines then begin
    {$ifdef ver180}
      s2 := ExtractFileExt(PrjFileName(project.FPrj));
      if (s2 <> '') and (s2[2] in ['b'..'c', 'B'..'C']) then
           s1 := IOTAProject(project.FPrj).ProjectOptions.Values['bcc32.D.value']
      else s1 := IOTAProject(project.FPrj).ProjectOptions.Values['Defines'];
    {$else}
      s1 := IOTAProject(project.FPrj).ProjectOptions.Values['Defines'];
    {$endif}
    if SubTextExists(s1, 'madExcept', ';') <> project.Enabled then begin
      if project.Enabled then begin
        if s1 <> '' then
          s1 := s1 + ';';
        s1 := s1 + 'madExcept';
      end else begin
        s1 := ';' + s1 + ';';
        i1 := PosText(';madExcept;', s1);
        if i1 <> 0 then
          Delete(s1, i1, 10);
        if (s1 <> '') and (s1[1] = ';') then
          Delete(s1, 1, 1);
        if (s1 <> '') and (s1[Length(s1)] = ';') then
          SetLength(s1, Length(s1) - 1);
      end;
      {$ifdef ver180}
        s2 := ExtractFileExt(PrjFileName(project.FPrj));
        if (s2 <> '') and (s2[2] in ['b'..'c', 'B'..'C']) then 
             IOTAProject(project.FPrj).ProjectOptions.Values['bcc32.D.value'] := s1
        else IOTAProject(project.FPrj).ProjectOptions.Values['Defines'      ] := s1;
      {$else}
        IOTAProject(project.FPrj).ProjectOptions.Values['Defines'] := s1;
      {$endif}
    end;
  end;
end;

// ***************************************************************

constructor TProjectSaveNotifier.Create(var prj: IOTAProject);
begin
  FPrj := prj;
  inherited Create;
end;

procedure TProjectSaveNotifier.BeforeSave;
var i1 : integer;
begin
  try
    for i1 := 0 to high(Projects) do
      if Projects[i1].FPrj = FPrj then
        with Projects[i1] do begin
          log('BeforeSave event "' + FCurrentFileName + '"');
          indentLog;
          CheckProjectUses(Projects[i1]);
          unindentLog;
          break;
        end;
  except on E: Exception do WizardHandleException(E) end;
end;

procedure TProjectSaveNotifier.AfterSave;
var i1 : integer;
    s1 : string;
begin
  try
    for i1 := 0 to high(Projects) do
      if Projects[i1].FPrj = FPrj then
        with Projects[i1] do begin
          log('BeforeSave event "' + FCurrentFileName + '", modified: ' + booleanToChar(FModified));
          indentLog;
          if FModified then begin
            GetProjectRoot(Projects[i1], s1);
            if s1 <> '' then
              SaveSettingsToIni(s1, Projects[i1]);
            FModified := false;
          end;
          unindentLog;
          break;
        end;
  except on E: Exception do WizardHandleException(E) end;
end;

procedure TProjectSaveNotifier.Modified;   begin end;
procedure TProjectSaveNotifier.Destroyed;  begin end;

function TProjectSaveNotifier.CheckOverwrite : boolean;
begin
  result := true;
end;

procedure TProjectSaveNotifier.ModuleRenamed(const newName: string);
var i1, i2 : integer;
begin
  try
    for i1 := 0 to high(Projects) do
      if Projects[i1].FPrj = FPrj then
        with Projects[i1] do begin
          log('ModuleRenamed event, newName: "' + newName + '"');
          i2 := PosStr('.', FCurrentFileName, maxInt, 1);
          if i2 <> 0 then
            Delete(FCurrentFileName, i2, maxInt);
          if GetFileAttributes(pchar(FCurrentFileName + '.mes')) <> maxCard then
            FModified := true;
          FCurrentFileName := newName;
          break;
        end;
  except on E: Exception do WizardHandleException(E) end;
end;

procedure MenuItemClick(Self, Sender: TObject);
var prj : TProject;
begin
  try
    if Self = FMenuItem1 then begin
      log('madSettings menu item was clicked'); indentLog;
      CheckCurrentProject;
      if FCurrentPrj = -1 then begin
        prj.FPrj := nil;
        LoadDefaultSettings(prj);
        if EditProjectSettings(prj) then
          log('default settings were modified');
      end else
        if EditProjectSettings(Projects[FCurrentPrj]) then begin
          log('settings were modified');
          CheckProjectUses(Projects[FCurrentPrj]);
          Projects[FCurrentPrj].FModified := true;
          SetProjectModified;
        end;
      unindentLog;
    end else begin
      log('madExcept configuration menu item was clicked'); indentLog;
      EditGlobalCfg;
      unindentLog;
    end;
  except on E: Exception do WizardHandleException(E) end;
end;

procedure TIDENotifier.FileNotification(notifyCode: TOTAFileNotification; const fileName: string; var cancel: boolean);
begin
  try
    if notifyCode in [ofnFileClosing, {$ifdef d6}ofnActiveProjectChanged{$else}ofnFileOpened{$endif}] then begin
      case notifyCode of
        ofnFileClosing : log('FileClosing: "' + fileName + '"');
        {$ifdef d6}
          ofnActiveProjectChanged : log('ActiveProjectChanged: "' + fileName + '"');
        {$else}
          ofnFileOpened : log( 'FileOpened: "' + fileName + '"');
        {$endif}
      end;
{      if notifyCode = ofnFileClosing then begin
        indentLog;
        CheckCurrentProject;
        unindentLog;
      end; }
    end;
  except on E: Exception do WizardHandleException(E) end;
end;

procedure BeforeCompile;
var bcb : boolean;
    ext : string;
//{$ifndef ver120}
//var b1 : boolean;
//{$endif}
begin
  log('BeforeCompile'); indentLog;
  if (FCurrentPrj > -1) and (not Projects[FCurrentPrj].MadExcept2Only) then begin
    CheckProjectUses(Projects[FCurrentPrj]);
    if AdjustOptions then
      with Projects[FCurrentPrj] do
        if Enabled then begin
          with IOTAProject(FPrj).ProjectOptions do begin
            ext := ExtractFileExt(PrjFileName(FPrj));
            bcb := (Length(ext) > 1) and (ext[2] in ['b'..'c', 'B'..'C']);
            if ((BCB or (Values['MapFile'] <> 3)) {$ifdef td32} and ((not BCB) or (Values['DebugInfo'] <> -1)) {$endif} ) or
               (BCB and ((Values['CppDebugInfo'] <> -1) or (Values['LineNumbers'] <> -1)))
               {$ifndef ver120} or (Values['UnitDebugInfo'] <> 1) or (Values['LocalSymbols'] <> 1) {$endif} then begin
              if BCB then begin
                Values['DebugInfo'    ] := -1;
                Values['CppDebugInfo' ] := -1;
                Values['LineNumbers'  ] := -1;
              end else
                Values['MapFile'      ] := 3;
              {$ifndef ver120}
                Values['UnitDebugInfo'] := 1;
                Values['LocalSymbols' ] := 1;
              {$endif}
            end;
          end;
        end;
  end;
  unindentLog;
end;

procedure AfterCompile(succeeded: boolean);

  function RetAddBackslash(const path: string) : string;
  begin
    if (path <> '') and (path[Length(path)] <> '\') then
         result := path + '\'
    else result := path;
  end;

var root             : string;
    output           : string;
    mapFile, madFile : string;
    ext              : string;
    binary           : string;
    s1, s2           : string;
    i1, i2           : integer;
    c1, c2           : cardinal;
    wfd              : TWin32FindData;
    hk1              : HKEY;
    isDpk            : boolean;
//    {$ifndef ver120}
//      b1               : boolean;
//    {$endif}
    arrCh            : array [0..MAX_PATH] of char;
    bcb              : boolean;
begin
  log('AfterCompile, succeeded: ' + booleanToChar(succeeded)); indentLog;
  try
    if FCurrentPrj > -1 then
      with Projects[FCurrentPrj] do begin
        GetProjectRoot(Projects[FCurrentPrj], root);
        log('root: "' + root + '"');
        ext := ExtractFileExt(PrjFileName(FPrj));
        isDpk := IsTextEqual(ext, '.dpk') or IsTextEqual(ext, '.bpk');
        bcb := (Length(ext) > 1) and (ext[2] in ['b'..'c', 'B'..'C']);
        log('isDpk: ' + booleanToChar(isDpk));
        binary := '';
        {$ifndef ver120}{$ifndef ver130}{$ifndef ver140}{$ifndef ver150}
          binary := IOTAProject(FPrj).ProjectOptions.GetTargetName;
        {$endif}{$endif}{$endif}{$endif}
        if binary <> '' then begin
          log('binary: "' + binary + '"');
        end else begin
          if isDpk and (not BCB) then begin
            output := IOTAProject(FPrj).ProjectOptions.Values['PkgDllDir'];
            log('ProjectOption.PkgDllDir: "' + output + '"');
            if (output = '0') and
               (GetPrivateProfileString('Directories', 'PackageDLLOutputDir', '0',
                                        arrCh, MAX_PATH, pchar(root + '.dof')) > 0) then begin
              log('dof.PackageDLLOutputDir: "' + arrCh + '"');
              output := arrCh;
            end;
          end else begin
            output := IOTAProject(FPrj).ProjectOptions.Values['OutputDir'];
            log('ProjectOption.OutputDir: "' + output + '"');
          end;
          if isDpk then begin
            s1 := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey + '\Library';
            Delete(s1, 1, 1);
            if ((output = '') or (output = '0')) and
               (RegOpenKeyEx(HKEY_CURRENT_USER, pchar(s1), 0, KEY_QUERY_VALUE, hk1) = 0) then
              try
                c2 := MAX_PATH + 1;
                if (RegQueryValueEx(hk1, 'Package DPL Output', nil, @c1, @arrCh, @c2) = 0) and (c2 > 1) then begin
                  arrCh[c2] := #0;
                  log('registry\Package DPL Output: "' + arrCh + '"');
                  output := arrCh;
                end;
              finally RegCloseKey(hk1) end;
          end;
          if (output <> '') and (output <> '0') then begin
            if (Length(output) = 1) or
               ( ((output[2] <> ':')                      ) and
                 ((output[1] <> '$') or (output[2] <> '(')) and
                 ((output[1] <> '\') or (output[2] <> '\'))     ) then begin
              if output[1] = '\' then
                     output := RetAddBackslash(ExtractFileDrive(root) + output) + ExtractFileName(root)
              else   output := RetAddBackslash(ExtractFilePath (root) + output) + ExtractFileName(root);
            end else output := RetAddBackslash(                         output) + ExtractFileName(root);
          end else   output :=                                                                    root;
          log('output: "' + output + '"');
          ExpandVars(output);
          log('output (after expanding): "' + output + '"');
          s1 := GetRootDir;
          if s1 <> '' then begin
            log('registry\RootDir: "' + s1 + '"');
            ReplaceText(output, '%delphi%', s1);
            ReplaceText(output, '%bcb%',    s1);
          end;
          if BCB then
               ext := IOTAProject(FPrj).ProjectOptions.Values['AppFileExt']
          else ext := '';
          if ext = '' then begin
            if isDpk then
                 ext := 'bpl'
            else ext := 'exe';
            c1 := CreateFile(pchar(PrjFileName(FPrj)), GENERIC_READ, FILE_SHARE_READ, nil,
                             OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
            if c1 <> INVALID_HANDLE_VALUE then
              try
                SetLength(s1, GetFileSize(c1, nil));
                if ReadFile(c1, pointer(s1)^, Length(s1), c2, nil) and (c2 = dword(Length(s1))) then begin
                  if not isDpk then
                    if BCB then begin
                      i1 := PosText('<PROJECT value="', s1);
                      if i1 > 0 then begin
                        i2 := PosStr('"/>', s1, i1);
                        if i2 > 0 then begin
                          if s1[i1 + 17] in [':', '\'] then
                               output := ''
                          else output := ExtractFilePath(output);
                          if (output <> '') and (s1[i1 + 16] = '\') then
                            inc(i1);
                          output := output + Copy(s1, i1 + 16, i2 - i1 - 16);
                          ext := CutFileExt(output);
                          Delete(ext, 1, 1);
                        end;
                      end;
                    end else
                      ExtractExtFromDpr(s1, ext, true);
                  if not BCB then
                    ExtractExtFromDpr(s1, ext, false);
                end;
              finally CloseHandle(c1) end;
          end;
          if ext <> '' then begin
            {$ifdef d6}
              s1 := IOTAProject(FPrj).ProjectOptions.Values['SOSuffix'];
              if s1 <> '' then
                output := output + s1;
            {$endif}
            binary := output + '.' + ext;
            {$ifdef d6}
              s1 := IOTAProject(FPrj).ProjectOptions.Values['SOVersion'];
              if s1 <> '' then begin
                if s1[1] <> '.' then
                  s1 := '.' + s1;
                binary := binary + s1;
              end;
              s1 := IOTAProject(FPrj).ProjectOptions.Values['SOPrefix'];
              if s1 <> '' then
                binary := ExtractFilePath(binary) + s1 + ExtractFileName(binary);
            {$endif}
            log('binary: "' + binary + '"');
          end;
        end;
        if binary <> '' then begin
          if Enabled then begin
            mapFile := binary;
            Delete(mapFile, PosStr('.', mapFile, maxInt, 1), maxInt);
            mapFile := mapFile + '.map';
            log('map file: "' + mapFile + '"');
            c1 := FindFirstFile(pchar(binary), wfd);
            if c1 <> INVALID_HANDLE_VALUE then begin
              Windows.FindClose(c1);
              if IsTextEqual(FLastBinary, binary) then begin
                if (int64(wfd.ftLastWriteTime) = FLastBinaryTime) and
                   (      wfd.nFileSizeLow     = FLastBinarySize) then begin
                  log('binary date/time and size unchanged -> exit');
                  exit;
                end;
              end else
                FLastBinary := binary;
              s1 := '';
              PatchBinary(Projects[FCurrentPrj], binary, root, '', succeeded, s1);
              for i1 := 1 to SubStrCount(s1) do begin
                s2 := SubStr(s1, i1);
                if (s2 <> '') and (s2[1] = 'w') then
                  PostWarning(PrjFileName(FPrj), Copy(s2, 2, maxInt), false);
              end;
            end else if succeeded then PostWarning(PrjFileName(FPrj), 'Binary not found!');
          end else begin
            madFile := output + '.mad';
            SetFileAttributes(pchar(madFile), 0);
            DeleteFile(madFile);
          end;
        end else if succeeded then PostWarning(PrjFileName(FPrj), 'Binary not found, unknown extension!');
      end;
    log('AfterCompile done');
  finally unindentLog end;
end;

procedure TIDENotifier.BeforeCompile(const project: IOTAProject; var cancel: boolean);
begin
  {$ifdef ver120}
    try
      log('BeforeCompile event'); indentLog;
      FAmCompiling := true;
      CheckCurrentProject;
      madExceptWizard.BeforeCompile;
      unindentLog;
    except on E: Exception do WizardHandleException(E) end;
  {$endif}
end;

procedure TIDENotifier.AfterCompile(succeeded: boolean);
begin
  {$ifdef ver120}
    try
      log('AfterCompile event, succeeded: ' + booleanToChar(succeeded)); indentLog;
      CheckCurrentProject;
      madExceptWizard.AfterCompile(succeeded);
      FAmCompiling := false;
      unindentLog;
    except on E: Exception do WizardHandleException(E) end;
  {$endif}
end;

{$ifndef ver120}
  procedure TIDENotifier.BeforeCompile(const project: IOTAProject; isCodeInsight: boolean; var cancel: Boolean);
  begin
    try
      log('BeforeCompile event, isCodeInsight: ' + booleanToChar(isCodeInsight)); indentLog;
      FAmCompiling := true;
      CheckCurrentProject;
      if not isCodeInsight then
        madExceptWizard.BeforeCompile;
      unindentLog;
    except on E: Exception do WizardHandleException(E) end;
  end;

  procedure TIDENotifier.AfterCompile(succeeded: boolean; isCodeInsight: boolean);
  begin
    try
      log('AfterCompile event, succeeded: ' + booleanToChar(succeeded) + ', isCodeInsight: ' + booleanToChar(isCodeInsight)); indentLog;
      CheckCurrentProject;
      if not isCodeInsight then
        madExceptWizard.AfterCompile(succeeded);
      FAmCompiling := false;
      unindentLog;
    except on E: Exception do WizardHandleException(E) end;
  end;
{$endif}

procedure TIDENotifier.AfterSave;  begin end;
procedure TIDENotifier.BeforeSave; begin end;
procedure TIDENotifier.Destroyed;  begin end;
procedure TIDENotifier.Modified;   begin end;

procedure AfterCompileHack(package: string);
var ext : string;
begin
  log('AfterCompileHack, package: "' + package + '"'); indentLog;
  try
    if FAmCompiling and (FCurrentPrj > -1) then
      with Projects[FCurrentPrj] do begin
        ext := ExtractFileExt(PrjFileName(FPrj));
        if IsTextEqual(ext, '.dpk') or IsTextEqual(ext, '.bpk') then
          AfterCompile(false);
      end;
  finally unindentLog end;
end;

var LoadPackageNext : pointer = nil;
procedure LoadPackageCallback;
asm
  pushad
  pushfd
  call AfterCompileHack
  popfd
  popad
  jmp LoadPackageNext
end;

procedure HookLoadPackage;
// Delphi's "AfterCompile" notifications comes too late for us
// it is fired *after* the newly compiled package is reloaded by the IDE
// so we hack into the IDE to get a better (earlier) notification
// tested with D4 - Delphi 2005
var s1 : string;
    c1 : dword;
    p1 : pointer;
    fi : TFunctionInfo;
begin
  {$ifdef ver120}
    s1 := 'coride40.bpl';
  {$else}
    {$ifdef ver130}
      s1 := 'coride50.bpl';
    {$else}
      {$ifdef ver140}
        s1 := 'coreide60.bpl';
      {$else}
        {$ifdef ver150}
          s1 := 'coreide70.bpl';
        {$else}
          {$ifdef ver170}
            s1 := 'coreide90.bpl';
          {$else}
            s1 := 'coreide100.bpl';
          {$endif}
        {$endif}
      {$endif}
    {$endif}
  {$endif}
  c1 := GetModuleHandle(pchar(s1));
  if c1 <> 0 then begin
    p1 := GetProcAddress(c1, '@Pakload@TPackage@DoLoadPackage$qqrx17System@AnsiString');
    if p1 <> nil then begin
      fi := ParseFunction(p1);
      if Length(fi.FarCalls) = 1 then
        with fi.FarCalls[0] do
          if RelTarget and (dword(CodeAddr2) - dword(CodeAddr1) = 5) and
             VirtualProtect(CodeAddr1, 5, PAGE_EXECUTE_READWRITE, @c1) then begin
            p1 := pointer(dword(CodeAddr1) + 1);
            if LoadPackageNext = nil then begin
              dword(LoadPackageNext) := dword(CodeAddr2) + dword(p1^);
              dword(p1^) := dword(@LoadPackageCallback) - dword(CodeAddr2);
            end else
              dword(p1^) := dword(LoadPackageNext) - dword(CodeAddr2);
            VirtualProtect(CodeAddr1, 5, c1, @c1);
          end;
    end;
  end;
end;

// ***************************************************************

procedure EnableAttachMapFile;
var s1     : string;
    i1, i2 : integer;
begin
  try
    log('EnableAttachMapFile was called from outside'); indentLog;
    CheckCurrentProject;
    with (BorlandIDEServices as IOTAModuleServices).CurrentModule do
      for i1 := 0 to OwnerCount - 1 do
        for i2 := 0 to high(Projects) do
          if Owners[i1] = Projects[i2].FPrj then
            with Projects[i2] do begin
              GetProjectRoot(Projects[i2], s1);
              if (not FModified) and
                 (GetFileAttributes(pchar(s1 + '.amf')) = maxCard) and
                 (GetFileAttributes(pchar(s1 + '.mes')) = maxCard) then begin
                Enabled := true;
                FModified := true;
                SetProjectModified;
              end;
            end;
    unindentLog;
  except on E: Exception do WizardHandleException(E) end;
end;

exports EnableAttachMapFile;

// ***************************************************************

procedure ReloadMesFile;
var i1 : integer;
    s1 : string;
begin
  try
    log('ReloadMesFile was called from outside'); indentLog;
    CheckCurrentProject;
    for i1 := 0 to high(Projects) do
      with Projects[i1] do begin
        log('reloading mes file for project "' + PrjFileName(FPrj) + '"');
        LoadDefaultSettings(Projects[i1]);
        GetProjectRoot(Projects[i1], s1);
        if s1 <> '' then
          LoadSettingsFromIni(s1, Projects[i1]);
      end;
    unindentLog;
  except on E: Exception do WizardHandleException(E) end;
end;

exports ReloadMesFile;

// ***************************************************************

procedure ReceiveJumpCommand(window, msg: cardinal; wParam, lParam: integer; var result: integer);
var ph    : dword;
    arrCh : array [0..MAX_PATH] of char;
begin
  try
    ph := OpenProcess(PROCESS_ALL_ACCESS, false, dword(wParam));
    if ph <> 0 then
      try
        if ReadProcessMemory(ph, pointer(lParam), @arrCh, MAX_PATH, dword(nil^)) then
          JumpToBugReportLine(arrCh);
      finally CloseHandle(ph) end;
  except end;
  result := 0;
end;

// ***************************************************************

var pluginList : TStringList = nil;
procedure CheckPlugins(window, msg: cardinal; wParam, lParam: integer; var result: integer);
var wfd : TWin32FindData;
    fh  : dword;
    s1  : string;
    oem : dword;
begin
  s1 := RegReadStr(HKEY_CURRENT_USER, 'Software\madshi\madExcept', 'plugin root');
  if s1 <> '' then begin
    if pluginList = nil then
      pluginList := TStringList.Create;
    oem := SetErrorMode(SEM_NOOPENFILEERRORBOX or SEM_FAILCRITICALERRORS);
    {$ifdef ver120}fh := FindFirstFile(pchar(s1 + '\*40.bpl'), wfd);{$endif}
    {$ifdef ver130}fh := FindFirstFile(pchar(s1 + '\*50.bpl'), wfd);{$endif}
    {$ifdef ver140}fh := FindFirstFile(pchar(s1 + '\*60.bpl'), wfd);{$endif}
    {$ifdef ver150}fh := FindFirstFile(pchar(s1 + '\*70.bpl'), wfd);{$endif}
    {$ifdef ver170}fh := FindFirstFile(pchar(s1 + '\*90.bpl'), wfd);{$endif}
    {$ifdef ver180}fh := FindFirstFile(pchar(s1 + '\*100.bpl'), wfd);{$endif}
    if fh <> INVALID_HANDLE_VALUE then begin
      repeat
        if pluginList.IndexOf(wfd.cFileName) = -1 then
          try
            if LoadPackage(s1 + '\' + wfd.cFileName) <> 0 then
              pluginList.Add(wfd.cFileName);
          except end;
      until not FindNextFile(fh, wfd);
      windows.FindClose(fh);
    end;
    SetErrorMode(oem);
  end;
end;

// ***************************************************************

procedure Register;
var i1, i2 : integer;
    method : TMethod;
    c1     : dword;
begin
  try
    log('Registering'); indentLog;

    {$ifndef ver120}{$ifndef ver130}{$ifndef ver140}{$ifndef ver150}{$ifndef ver160}
      // Delphi 2005 and up...
      FSplashBmp := LoadImage(HInstance, 'MAD', IMAGE_BITMAP, 24, 24, 0);
      SplashScreenServices.AddPluginBitmap('madExcept ' + GetVersionFromResources, FSplashBmp);
    {$endif}{$endif}{$endif}{$endif}{$endif}

    with (BorlandIDEServices as INTAServices).MainMenu do
      for i1 := 0 to Items.Count - 1 do
        with Items[i1] do
          if Name = 'ProjectMenu' then begin
            log('ProjectMenu found');
            for i2 := 0 to Count - 1 do
              if Items[i2].Name = 'ProjectOptionsItem' then begin
                log('ProjectOptionsItem found, adding "madExcept settings..."');
                FMenuItem1 := TMenuItem.Create(Items[i2].Owner);
                FMenuItem1.Name := 'ProjectMadExceptSettingsItem';
                FMenuItem1.Caption := 'madExcept settings...';
                c1 := LoadImage(HInstance, 'MENUBMP', IMAGE_BITMAP, 0, 0, LR_SHARED);
                {$ifndef ver120}
                  FMenuItem1.ImageIndex := ImageList_AddMasked(Items[i2].GetImageList.Handle, c1, clSilver);
                {$else}
                  FMenuItem1.ImageIndex := -1;
                  FMenuItem1.Bitmap := TBitmap.Create;
                  FMenuItem1.Bitmap.Handle := c1;
                {$endif}
                method.Data := FMenuItem1;
                method.Code := @MenuItemClick;
                FMenuItem1.OnClick := TNotifyEvent(method);
                Insert(i2, FMenuItem1);
                System.break;
              end;
          end else
            if Name = 'ToolsMenu' then begin
              log('ToolsMenu found');
              for i2 := 0 to Count - 1 do
                if (Items[i2].Caption = '-') or (Items[i2].Name = 'ToolsToolsItem') then begin
                  log('seperator found, adding "madExcept configuration..."');
                  FMenuItem2 := TMenuItem.Create(Items[0].Owner);
                  FMenuItem2.Name := 'GlobalMadExceptConfigurationItem';
                  FMenuItem2.Caption := 'madExcept configuration...';
                  c1 := LoadImage(HInstance, 'MENUBMP', IMAGE_BITMAP, 0, 0, LR_SHARED);
                  {$ifndef ver120}
                    FMenuItem2.ImageIndex := ImageList_AddMasked(Items[0].GetImageList.Handle, c1, clSilver);
                  {$else}
                    FMenuItem2.ImageIndex := -1;
                    FMenuItem2.Bitmap := TBitmap.Create;
                    FMenuItem2.Bitmap.Handle := c1;
                  {$endif}
                  method.Data := FMenuItem2;
                  method.Code := @MenuItemClick;
                  FMenuItem2.OnClick := TNotifyEvent(method);
                  Insert(i2, FMenuItem2);
                  System.break;
                end;
            end;

    log('creating notifiers');
    FIDENotifier      := TIDENotifier.Create;
    FIDENotifierIndex := (BorlandIDEServices as IOTAServices).AddNotifier(FIDENotifier);

    // unfortunately this confuses D4/5 slightly, if a project group is loaded at startup
    // as a result the first project in the group is activated instead of the last
    // CheckCurrentProject;

    log('current project: ' + booleanToChar(FCurrentPrj > -1));
    AddMsgHandler(ReceiveJumpCommand, RegisterWindowMessage('madExceptIdeJumpCommand'));
    AddMsgHandler(CheckPlugins,       RegisterWindowMessage('madExceptIdeCheckPlugins'));
    log('jump command handler registered');
    CheckPlugins(0, 0, 0, 0, i1);
    log('plugins dynamically loaded');
    HookLoadPackage;
    log('AfterCompile hack installed');
    unindentLog;
  except on E: Exception do WizardHandleException(E) end;
end;

// ***************************************************************

procedure CloseMadExceptWizard;
var i1 : integer;
begin
  {$ifndef ver120}{$ifndef ver130}{$ifndef ver140}{$ifndef ver150}{$ifndef ver160}
    DeleteObject(FSplashBmp);
    FSplashBmp := 0;
  {$endif}{$endif}{$endif}{$endif}{$endif}

  HookLoadPackage;
  log('AfterCompile hack removed');
  DelMsgHandler(CheckPlugins);
  if pluginList <> nil then begin
    for i1 := pluginList.Count - 1 downto 0 do
      UnloadPackage(GetModuleHandle(pchar(pluginList[i1])));
    pluginList.Free;
    pluginList := nil;
  end;
  DelMsgHandler(ReceiveJumpCommand);
  log('jump command handler removed');
  CheckCurrentProject(true);
  log('remove notifier');
  (BorlandIDEServices as IOTAServices).RemoveNotifier(FIDENotifierIndex);
  FIDENotifier := nil;
  log('remove menu item');
  {$ifndef ver120}
    if FMenuItem1.ImageIndex <> -1 then begin
      ImageList_Remove(FMenuItem1.GetImageList.Handle, FMenuItem1.ImageIndex);
      FMenuItem1.ImageIndex := -1;
    end;
    if FMenuItem2.ImageIndex <> -1 then begin
      ImageList_Remove(FMenuItem2.GetImageList.Handle, FMenuItem2.ImageIndex);
      FMenuItem2.ImageIndex := -1;
    end;
  {$endif}
  FMenuItem1.Free;
  FMenuItem2.Free;
  log('done.');
end;

// ***************************************************************

procedure TFMadExceptProjectSettings.FormAddBtnClick(Sender: TObject);
var astr : TDAString;
    i1   : integer;
    s1   : string;
begin
  SetLength(astr, Length(FForms));
  for i1 := 0 to high(astr) do
    astr[i1] := FForms[i1][0].name;
  if AddForm(s1, astr, CompImgs) then begin
    i1 := Length(FForms);
    SetLength(FForms, i1 + 1);
    SetLength(FForms[i1], 4);
    FForms[i1][0].name := s1;
    FForms[i1][0].typ  := 'INVForm';
    FForms[i1][1].name := 'ContinueBtn';
    FForms[i1][1].typ  := 'INVButton';
    FForms[i1][2].name := 'SkipBtn';
    FForms[i1][2].typ  := 'INVButton';
    FForms[i1][3].name := 'CancelBtn';
    FForms[i1][3].typ  := 'INVButton';
    FillProps(FForms[i1][0], true);
    FillProps(FForms[i1][1], true);
    FillProps(FForms[i1][2], true);
    FillProps(FForms[i1][3], true);
    with FormList.Items.Add do begin
      Caption := s1;
      ImageIndex := 1;
    end;
  end;
  CheckAssisButtons;
end;

function TypToImageIndex(typ: string) : integer;
begin
  if      typ = 'INVLabel'    then result :=  2
  else if typ = 'INVEdit'     then result :=  3
  else if typ = 'INVCheckBox' then result :=  4
  else if typ = 'INVButton'   then result :=  5
  else if typ = 'INVImage'    then result :=  6
  else                             result := -1;
end;

procedure TFMadExceptProjectSettings.FormListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var i1 : integer;
begin
  CompList.Items.Clear;
  PropPaint.Repaint;
  if Selected and (Item <> nil) then begin
    with CompList.Items.Add do begin
      Caption := FForms[Item.Index, 0].name;
      ImageIndex := 1;
    end;
    for i1 := 1 to high(FForms[Item.Index]) do
      with CompList.Items.Add do begin
        Caption := FForms[Item.Index][i1].name;
        ImageIndex := TypToImageIndex(FForms[Item.Index][i1].typ);
      end;
  end;
  CheckAssisButtons;
end;

procedure TFMadExceptProjectSettings.CompAddBtnClick(Sender: TObject);
var form   : TListItem;
    i1     : integer;
    s1, s2 : string;
    astr   : TDAString;
begin
  form := FormList.Selected;
  if (form <> nil) and (form.Index > -1) then begin
    SetLength(astr, Length(FForms[form.Index]));
    for i1 := 0 to high(astr) do
      astr[i1] := FForms[form.Index, i1].name;
    if AddComponent(s1, s2, astr, CompImgs) then begin
      i1 := Length(FForms[form.Index]);
      SetLength(FForms[form.Index], i1 + 1);
      with FForms[form.Index][i1] do begin
        name := s2;
        typ  := 'INV' + s1;
        FillProps(FForms[form.Index][i1], false);
        with CompList.Items.Add do begin
          Caption := name;
          ImageIndex := TypToImageIndex(typ);
        end;
      end;
    end;
  end;
  CheckAssisButtons;
  FActiveProp[0] := -1;
  PropPaint.Repaint;
end;

procedure TFMadExceptProjectSettings.CompListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  PropPaint.Repaint;
  CheckAssisButtons;
end;

procedure TFMadExceptProjectSettings.PropPaintPaint(Sender: TObject);

  function PropToText(i1, i2, i3: integer) : string;
  begin
    with FForms[i1, i2].props[i3] do
      case typ of
        dtInt  : result := IntToStrEx(vInt);
        dtBool : if vBool then
                      result := 'True'
                 else result := 'False';
        dtStr  : result := vStr;
        dtEnum : result := Copy(vStr, 1 + Length(prep), maxInt);
        else     result := '';
      end;
  end;

var formItem, compItem : TListItem;
    i1, i2             : integer;
    b1                 : boolean;
    activeProp         : array [0..2] of integer;
    lineHeight         : integer;
begin
  activeProp[0] := -1;
  activeProp[1] := -1;
  activeProp[2] := -1;
  lineHeight := 16 * Screen.PixelsPerInch div 96;
  formItem := FormList.Selected;
  compItem := CompList.Selected;
  if (formItem <> nil) and (compItem <> nil) then
    with FForms[formItem.Index, compItem.Index], PropPaint.Canvas do begin
      b1 := true;
      if FSelProp <> '' then
        for i1 := 0 to high(props) do
          if FSelProp = props[i1].name then begin
            b1 := false;
            break;
          end;
      if b1 then
        FSelProp := props[0].name;
      for i1 := 0 to high(props) do begin
        if FSelProp <> props[i1].name then begin
          Pen.Color := clBtnShadow;
          for i2 := 0 to PropBox.Width do
            if not odd(i2) then begin
              MoveTo(i2, i1 * lineHeight + lineHeight - 1);
              LineTo(i2, i1 * lineHeight + lineHeight);
            end;
          Pen.Color := clBtnShadow;
          MoveTo(FPropCol, i1 * lineHeight);
          LineTo(FPropCol, i1 * lineHeight + lineHeight);
          Pen.Color := clBtnHighlight;
          MoveTo(FPropCol + 1, i1 * lineHeight);
          LineTo(FPropCol + 1, i1 * lineHeight + lineHeight);
          Font.Color := clNavy;
          TextOut(FPropCol + 3, i1 * lineHeight + 1, PropToText(formItem.Index, compItem.Index, i1));
        end else begin
          DrawFrameControl(Handle, Rect(0, i1 * lineHeight - 2, PropPaint.ClientWidth + 3, i1 * lineHeight + lineHeight), DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_PUSHED);
          Pen.Color := clBtnShadow;
          MoveTo(FPropCol, i1 * lineHeight);
          LineTo(FPropCol, i1 * lineHeight + lineHeight - 2);
          Pen.Color := clBtnHighlight;
          MoveTo(FPropCol + 1, i1 * lineHeight);
          LineTo(FPropCol + 1, i1 * lineHeight + lineHeight - 2);
          MoveTo(FPropCol + 2, i1 * lineHeight);
          LineTo(FPropCol + 2, i1 * lineHeight + lineHeight - 2);
          MoveTo(FPropCol + 1, i1 * lineHeight);
          LineTo(PropBox.Width, i1 * lineHeight);
          activeProp[0] := formItem.Index;
          activeProp[1] := compItem.Index;
          activeProp[2] := i1;
        end;
        Font.Color := clBlack;
        Brush.Style := bsClear;
        TextRect(Rect(3, i1 * lineHeight + 1, FPropCol, i1 * lineHeight + lineHeight + 1), 3, i1 * lineHeight + 1, props[i1].name);
      end;
    end;
  if (activeProp[0] <> FActiveProp[0]) or (activeProp[1] <> FActiveProp[1]) or (activeProp[2] <> FActiveProp[2]) then begin
    PropEdit.Visible := false;
    PropBtn.Visible := false;
    FPropList.Visible := false;
    if FHook <> 0 then begin
      UnhookWindowsHookEx(FHook);
      FHook := 0;
    end;
    FActiveProp[0] := activeProp[0];
    FActiveProp[1] := activeProp[1];
    FActiveProp[2] := activeProp[2];
    if activeProp[0] <> -1 then begin
      PropEdit.Visible := true;
      PropEdit.Left := FPropCol + 3;
      PropEdit.Top := activeProp[2] * lineHeight + 1;
      PropEdit.Height := lineHeight - 3;
      PropEdit.Text := PropToText(activeProp[0], activeProp[1], activeProp[2]);
      PropEdit.Visible := true;
      with FForms[activeProp[0], activeProp[1]].props[activeProp[2]] do
        if typ in [dtBool, dtEnum] then begin
          PropEdit.Width := PropBox.Width - FPropCol - 7 - PropBtn.Width;
          PropBtn.Top := activeProp[2] * lineHeight;
          PropBtn.Visible := true;
          FPropList.Items.Clear;
          if typ = dtBool then begin
            FPropList.Items.Add('False');
            FPropList.Items.Add('True');
          end else
            if list <> '' then begin
              for i1 := 1 to SubStrCount(list) do
                FPropList.Items.Add(Copy(SubStr(list, i1), 1 + Length(prep), maxInt));
            end else
              for i1 := 1 to high(FForms[activeProp[0]]) do
                FPropList.Items.Add(FForms[activeProp[0], i1].name);
          if FPropList.Items.Count <= 15 then
               FPropList.Height := FPropList.Items.Count * 13 * Screen.PixelsPerInch div 96 + 2
          else FPropList.Height := 15 * 13 * Screen.PixelsPerInch div 96 + 2;
          FPropList.Top := ScreenToClient(PropBox.ClientToScreen(Point(0, activeProp[2] * lineHeight + lineHeight - 1))).Y - 1;
          if FPropList.Top + FPropList.Height >= ClientHeight then
            FPropList.Top := ScreenToClient(PropBox.ClientToScreen(Point(0, activeProp[2] * lineHeight - FPropList.Height))).Y - 1;
        end else begin
          PropEdit.Width := PropBox.Width - FPropCol - 7;
          PropBtn.Visible := false;
        end;
      PropEdit.SelectAll;
    end;
  end;
end;

procedure TFMadExceptProjectSettings.PropListKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then begin
    Key := #0;
    ClosePropList;
  end else
    if Key = #13 then begin
      Key := #0;
      AcceptPropList;
    end;
end;

procedure TFMadExceptProjectSettings.ClosePropList(Sender: TObject);
begin
  if FHook <> 0 then begin
    UnhookWindowsHookEx(FHook);
    FHook := 0;
  end;
  if FPropList <> nil then
    FPropList.Visible := false;
  if AssisPanel.Visible and (PropEdit <> nil) and PropEdit.Visible and PropEdit.Enabled then
    try
      PropEdit.SetFocus;
    except end;
end;

procedure TFMadExceptProjectSettings.PropBtnClick(Sender: TObject);
var i1 : integer;
begin
  FPropList.Visible := true;
  FPropList.SetFocus;
  i1 := FPropList.Items.IndexOf(PropEdit.Text);
  if i1 = -1 then
    i1 := 0;
  SendMessage(FPropList.Handle, LB_SETCARETINDEX, i1, 1);
  SendMessage(FPropList.Handle, LB_SETCURSEL,     i1, 0);
  if FHook = 0 then
    FHook := SetWindowsHookEx(WH_MOUSE, FHookWndProc, 0, GetCurrentThreadId);
end;

procedure TFMadExceptProjectSettings.AcceptPropList(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i1 : integer;
begin
  i1 := SendMessage(FPropList.Handle, LB_GETCURSEL, 0, 0);
  if i1 <> -1 then begin
    PropEdit.Text := FPropList.Items[i1];
    AcceptPropEdit;
  end;
  ClosePropList;
end;

procedure TFMadExceptProjectSettings.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #27) and (GetFocus <> FPropList.Handle) then begin
    Key := #0;
    if FIconPanel <> nil then 
      PostMessage(Handle, WM_USER + 777, 0, 0)
    else
      Close;
  end;
end;

procedure TFMadExceptProjectSettings.AcceptPropEdit(Sender: TObject);
var i1, i2, i3 : integer;
    b1         : boolean;
    li         : TListItem;
    s1         : string;
begin
  if (FActiveProp[0] <> -1) and (FActiveProp[1] <> -1) and (FActiveProp[2] <> -1) and
     (FActiveProp[0] < Length(FForms)) and (FActiveProp[1] < Length(FForms[FActiveProp[0]])) and
     (FActiveProp[2] < Length(FForms[FActiveProp[0], FActiveProp[1]].props)) then
    with FForms[FActiveProp[0], FActiveProp[1]].props[FActiveProp[2]] do
      case typ of
        dtInt  : begin
                   if StrToIntDef(PropEdit.Text, -777) <> -777 then begin
                     vInt := StrToIntDef(PropEdit.Text, -777);
                     if (vInt <= 1) and (name = 'Lines') then
                       with FForms[FActiveProp[0], FActiveProp[1]] do
                         for i1 := 0 to high(props) do
                           if (props[i1].typ = dtEnum) and (props[i1].name = 'OutputType') then begin
                             if props[i1].vStr = 'nvoOwnSection' then begin
                               props[i1].vStr := 'nvoHeader';
                               PropPaint.Repaint;
                             end;
                             break;
                           end;
                   end else
                     MessageBox(Handle, 'Invalid integer value.', nil, MB_ICONERROR);
                   PropEdit.Text := IntToStrEx(vInt);
                 end;
        dtBool : begin
                   if IsTextEqual(PropEdit.Text, 'true') then
                     vBool := true
                   else
                     if IsTextEqual(PropEdit.Text, 'false') then
                       vBool := false
                     else
                       MessageBox(Handle, 'Invalid boolean value.', nil, MB_ICONERROR);
                   if vBool then
                        PropEdit.Text := 'True'
                   else PropEdit.Text := 'False';
                 end;
        dtStr  : begin
                   if (name <> 'Name') or IsValidIdent(PropEdit.Text) then begin
                     b1 := true;
                     for i1 := 0 to high(FForms[FActiveProp[0]]) do
                       if (i1 <> FActiveProp[1]) and IsTextEqual(FForms[FActiveProp[0], i1].name, PropEdit.Text) then begin
                         MessageBox(Handle, 'Identifier already in use.', nil, MB_ICONERROR);
                         b1 := false;
                         break;
                       end;
                     if b1 then begin
                       if FActiveProp[1] = 0 then
                         for i1 := 0 to high(FForms) do
                           if (i1 <> FActiveProp[0]) and IsTextEqual(FForms[i1, 0].name, PropEdit.Text) then begin
                             MessageBox(Handle, 'Identifier already in use.', nil, MB_ICONERROR);
                             b1 := false;
                             break;
                           end;
                       if b1 and (PropEdit.Text <> vStr) then begin
                         vStr := PropEdit.Text;
                         if name = 'Name' then begin
                           li := CompList.Selected;
                           if (li <> nil) and (li.Caption = FForms[FActiveProp[0], FActiveProp[1]].name) then begin
                             li.Caption := vStr;
                             if FActiveProp[1] = 0 then begin
                               li := FormList.Selected;
                               if (li <> nil) and (li.Caption = FForms[FActiveProp[0], FActiveProp[1]].name) then
                                 li.Caption := vStr;
                               for i2 := 0 to high(FAssis) do begin
                                 FAssis[i2] := FAssis[i2] + '|';
                                 ReplaceText(FAssis[i2], '|' + FForms[FActiveProp[0], FActiveProp[1]].name + '|', '|' + vStr + '|');
                                 Delete(FAssis[i2], Length(FAssis[i2]), 1);
                                 if i2 < AssisList.Items.Count then begin
                                   s1 := '';
                                   for i3 := 3 to SubStrCount(FAssis[i2]) do
                                     s1 := s1 + ', ' + SubStr(FAssis[i2], i3);
                                   Delete(s1, 1, 2);
                                   AssisList.Items[i2].SubItems[1] := s1;
                                 end;
                               end;
                             end else
                               for i2 := 0 to high(FForms[FActiveProp[0]]) do
                                 for i3 := 0 to high(FForms[FActiveProp[0], i2].props) do
                                   with FForms[FActiveProp[0], i2].props[i3] do
                                     if (typ = dtEnum) and (list = '') and IsTextEqual(vStr, FForms[FActiveProp[0], FActiveProp[1]].name) then
                                       vStr := PropEdit.Text;
                             FForms[FActiveProp[0], FActiveProp[1]].name := vStr;
                           end;
                         end;
                       end;
                     end;
                   end else
                     MessageBox(Handle, 'Invalid identifier.', nil, MB_ICONERROR);
                   PropEdit.Text := vStr;
                 end;
        dtEnum : begin
                   if list <> '' then begin
                     if SubTextExists(list, prep + PropEdit.Text) then begin
                       for i1 := 1 to SubStrCount(list) do
                         if IsTextEqual(prep + PropEdit.Text, SubStr(list, i1)) then
                           vStr := SubStr(list, i1);
                       if vStr = 'nvoOwnSection' then
                         with FForms[FActiveProp[0], FActiveProp[1]] do
                           for i1 := 0 to high(props) do
                             if (props[i1].typ = dtInt) and (props[i1].name = 'Lines') then begin
                               if props[i1].vInt <= 1 then begin
                                 props[i1].vInt := 5;
                                 PropPaint.Repaint;
                               end;
                               break;
                             end;
                     end else
                       MessageBox(Handle, 'Invalid enumeration value.', nil, MB_ICONERROR);
                     PropEdit.Text := Copy(vStr, 1 + Length(prep), maxInt);
                   end else begin
                     if PropEdit.Text <> '' then begin
                       b1 := false;
                       for i1 := 1 to high(FForms[FActiveProp[0]]) do
                         if IsTextEqual(FForms[FActiveProp[0], i1].name, PropEdit.Text) then begin
                           b1 := true;
                           vStr := FForms[FActiveProp[0], i1].name;
                           break;
                         end;
                       if not b1 then
                         MessageBox(Handle, 'Invalid component name.', nil, MB_ICONERROR);
                     end else
                       vStr := '';
                     PropEdit.Text := vStr;
                   end;
                 end;
      end;
end;

procedure TFMadExceptProjectSettings.PropEditKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    Key := #0;
    AcceptPropEdit;
    PropEdit.SelectAll;
  end;
end;

procedure TFMadExceptProjectSettings.PropPaintMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Y := Y div (16 * Screen.PixelsPerInch div 96);
  if (FActiveProp[0] <> -1) and (FActiveProp[1] <> -1) and
     (FActiveProp[0] < Length(FForms)) and (FActiveProp[1] < Length(FForms[FActiveProp[0]])) and
     (Y < Length(FForms[FActiveProp[0], FActiveProp[1]].props)) then begin
    AcceptPropEdit;
    FSelProp := FForms[FActiveProp[0], FActiveProp[1]].props[Y].name;
    PropPaint.Repaint;
    ClosePropList;
  end;
end;

procedure TFMadExceptProjectSettings.PropEditDblClick(Sender: TObject);
var s1 : string;
begin
  s1 := PropEdit.Text;
  AcceptPropEdit;
  if IsTextEqual(s1, PropEdit.Text) and
     (FActiveProp[0] <> -1) and (FActiveProp[1] <> -1) and (FActiveProp[2] <> -1) and
     (FActiveProp[0] < Length(FForms)) and (FActiveProp[1] < Length(FForms[FActiveProp[0]])) and
     (FActiveProp[2] < Length(FForms[FActiveProp[0], FActiveProp[1]].props)) then
    with FForms[FActiveProp[0], FActiveProp[1]].props[FActiveProp[2]] do
      if typ = dtBool then begin
        vBool := not vBool;
        if vBool then
             PropEdit.Text := 'True'
        else PropEdit.Text := 'False';
      end;
  PropEdit.SelectAll;
end;

procedure TFMadExceptProjectSettings.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  procedure ShowContextHelp;
  var arrCh : array [0..MAX_PATH] of char;
      s1    : string;
  begin
    log('show help');
    GetModuleFileName(HInstance, arrCh, MAX_PATH);
    s1 := ExtractFilePath(arrCh);
    if s1 <> '' then
      s1 := ExtractFilePath(Copy(s1, 1, Length(s1) - 1));
    if s1 <> '' then
      s1 := ExtractFilePath(Copy(s1, 1, Length(s1) - 1));
    s1 := s1 + 'madBasic\Help\Data\madExceptSettings.htm';
    if (GetFileAttributes(pchar(s1)) <> maxCard) and
       (FindExecutable(pchar(s1), nil, arrCh) > 32) then
      ShellExecute(0, nil, arrCh, pchar('"' + s1 + '"'), nil, SW_SHOWNORMAL);
  end;

begin
  if ((Key = VK_DOWN) or (Key = VK_UP)) and PropEdit.Focused then begin
    if (FActiveProp[0] <> -1) and (FActiveProp[1] <> -1) and
       (FActiveProp[0] < Length(FForms)) and (FActiveProp[1] < Length(FForms[FActiveProp[0]])) then
      if ( (Key = VK_DOWN) and (FActiveProp[2] + 1 < Length(FForms[FActiveProp[0], FActiveProp[1]].props)) ) or
         ( (Key = VK_UP  ) and (FActiveProp[2] > 0)                                                        ) then begin
        AcceptPropEdit;
        if Key = VK_DOWN then
             FSelProp := FForms[FActiveProp[0], FActiveProp[1]].props[FActiveProp[2] + 1].name
        else FSelProp := FForms[FActiveProp[0], FActiveProp[1]].props[FActiveProp[2] - 1].name;
        PropPaint.Repaint;
        ClosePropList;
      end;
    Key := 0;
  end else
    if Key = VK_F1 then
      ShowContextHelp;
end;

procedure TFMadExceptProjectSettings.CheckAssisButtons;
var b1, b2 : boolean;
    li     : TListItem;
begin
  b2 := EnableMeCheck.Checked;
  li := AssisList.Selected;
  AssisAddBtn   .Enabled := b2 and (FForms <> nil);
  AssisAddMen   .Enabled := b2 and (FForms <> nil);
  AssisDelBtn   .Enabled := b2 and (li <> nil);
  AssisDelMen   .Enabled := b2 and (li <> nil);
//  AssisImpBtn   .Enabled := b2;
//  AssisImpMen   .Enabled := b2;
//  AssisExpBtn   .Enabled := b2 and (li <> nil);
//  AssisExpMen   .Enabled := b2 and (li <> nil);
  AssisChangeBtn.Enabled := b2 and (li <> nil);
  AssisChangeMen.Enabled := b2 and (li <> nil);
  AssisPlayBtn  .Enabled := b2 and (li <> nil) and (li.Index < Length(FAssis)) and (SubStrCount(FAssis[li.Index]) > 1);
  AssisPlayMen  .Enabled := b2 and (li <> nil) and (li.Index < Length(FAssis)) and (SubStrCount(FAssis[li.Index]) > 1);
  b1 := FormList.Selected <> nil;
  FormAddBtn    .Enabled := b2;
  FormAddMen    .Enabled := b2;
  FormDelBtn    .Enabled := b2 and b1;
  FormDelMen    .Enabled := b2 and b1;
//  FormImpBtn    .Enabled := b2;
//  FormImpMen    .Enabled := b2;
//  FormExpBtn    .Enabled := b2 and b1;
//  FormExpMen    .Enabled := b2 and b1;
  FormPlayBtn   .Enabled := b2 and b1;
  FormPlayMen   .Enabled := b2 and b1;
  CompAddBtn    .Enabled := b2 and b1;
  CompAddMen    .Enabled := b2 and b1;
  li := CompList.Selected;
  CompDelBtn    .Enabled := b2 and (li <> nil) and (li.Index > 3);
  CompDelMen    .Enabled := b2 and (li <> nil) and (li.Index > 3);
  CompUpBtn     .Enabled := b2 and (li <> nil) and (li.Index > 4);
  CompUpMen     .Enabled := b2 and (li <> nil) and (li.Index > 4);
  CompDownBtn   .Enabled := b2 and (li <> nil) and (li.Index > 3) and (li.Index < CompList.Items.Count - 1);
  CompDownMen   .Enabled := b2 and (li <> nil) and (li.Index > 3) and (li.Index < CompList.Items.Count - 1);
end;

procedure TFMadExceptProjectSettings.CompDelBtnClick(Sender: TObject);
var formItem, compItem : TListItem;
    i1, i2, i3         : integer;
begin
  formItem := FormList.Selected;
  compItem := CompList.Selected;
  if (formItem <> nil) and (compItem <> nil) and
     (MessageBox(Handle, pchar('Do you really want to delete the component "' + compItem.Caption + '"?'),
                 'Question...', MB_YESNO or MB_ICONQUESTION) = IDYES) then
    for i1 := 4 to high(FForms[formItem.Index]) do
      if IsTextEqual(FForms[formItem.Index, i1].name, compItem.Caption) then begin
        for i2 := i1 to high(FForms[formItem.Index]) - 1 do
          FForms[formItem.Index, i2] := FForms[formItem.Index, i2 + 1];
        SetLength(FForms[formItem.Index], Length(FForms[formItem.Index]) - 1);
        for i2 := 0 to high(FForms[formItem.Index]) do
          for i3 := 0 to high(FForms[formItem.Index, i2].props) do
            with FForms[formItem.Index, i2].props[i3] do
              if (typ = dtEnum) and (list = '') and IsTextEqual(vStr, compItem.Caption) then
                vStr := ''; 
        compItem.Delete;
        break;
      end;
end;

procedure TFMadExceptProjectSettings.CompListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var p1 : TPoint;
begin
  if Button = mbRight then begin
    p1 := CompList.ClientToScreen(Point(X, Y));
    CompPopup.Popup(p1.X, p1.Y);
  end;
end;

procedure TFMadExceptProjectSettings.CompListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) and CompDelBtn.Enabled then
    CompDelBtnClick(nil)
  else
    if (Key = VK_INSERT) and CompAddBtn.Enabled then
      CompAddBtnClick(nil)
    else
      if (Key = VK_DOWN) and (ssCtrl in Shift) and (not (ssShift in Shift)) then begin
        if CompDownBtn.Enabled then
          CompDownBtnClick(nil);
        Key := 0;
      end else
        if (Key = VK_UP) and (ssCtrl in Shift) and (not (ssShift in Shift)) then begin
          if CompUpBtn.Enabled then
            CompUpBtnClick(nil);
          Key := 0;
        end;
end;

procedure TFMadExceptProjectSettings.FormDelBtnClick(Sender: TObject);
var formItem   : TListItem;
    i1, i2, i3 : integer;
    s1         : string;
begin
  formItem := FormList.Selected;
  if (formItem <> nil) and
     (MessageBox(Handle, pchar('Do you really want to delete the form "' + formItem.Caption + '"?'),
                 'Question...', MB_YESNO or MB_ICONQUESTION) = IDYES) then
    for i1 := 0 to high(FForms) do
      if IsTextEqual(FForms[i1, 0].name, formItem.Caption) then begin
        for i2 := 0 to high(FAssis) do begin
          FAssis[i2] := FAssis[i2] + '|';
          ReplaceText(FAssis[i2], '|' + formItem.Caption + '|', '|');
          Delete(FAssis[i2], Length(FAssis[i2]), 1);
          if i2 < AssisList.Items.Count then begin
            s1 := '';
            for i3 := 3 to SubStrCount(FAssis[i2]) do
              s1 := s1 + ', ' + SubStr(FAssis[i2], i3);
            Delete(s1, 1, 2);
            AssisList.Items[i2].SubItems[1] := s1;
          end;
        end;
        for i2 := i1 to high(FForms) - 1 do
          FForms[i2] := FForms[i2 + 1];
        SetLength(FForms, Length(FForms) - 1);
        formItem.Delete;
        break;
      end;
end;

procedure TFMadExceptProjectSettings.FormListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) and FormDelBtn.Enabled then
    FormDelBtnClick(nil)
  else
    if (Key = VK_INSERT) and FormAddBtn.Enabled then
      FormAddBtnClick(nil);
end;

procedure TFMadExceptProjectSettings.FormListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var p1 : TPoint;
begin
  if Button = mbRight then begin
    p1 := FormList.ClientToScreen(Point(X, Y));
    FormPopup.Popup(p1.X, p1.Y);
  end;
end;

procedure TFMadExceptProjectSettings.FormPlayBtnClick(Sender: TObject);
var formItem : TListItem;
    i1       : integer;
begin
  formItem := FormList.Selected;
  if formItem <> nil then
    for i1 := 0 to high(FForms) do
      if IsTextEqual(FForms[i1, 0].name, formItem.Caption) then begin
        Enabled := false;
        CreateAssistant('Unnamed Assistant', [StructToDfm(FForms[i1])]).ShowModal(Handle);
        Enabled := true;
        BringToFront;
        break;
      end;
end;

procedure TFMadExceptProjectSettings.CompUpBtnClick(Sender: TObject);
var formItem, compItem : TListItem;
    dfmObj             : TDfmObject;
    s1                 : string;
    img, idx           : integer;
begin
  formItem := FormList.Selected;
  compItem := CompList.Selected;
  if (formItem <> nil) and (compItem <> nil) and (compItem.Index > 4) and
     (formItem.Index < Length(FForms)) and (compItem.Index < Length(FForms[formItem.Index])) then begin
    dfmObj := FForms[formItem.Index, compItem.Index - 1];
    FForms[formItem.Index, compItem.Index - 1] := FForms[formItem.Index, compItem.Index];
    FForms[formItem.Index, compItem.Index] := dfmObj;
    s1 := compItem.Caption;
    img := compItem.ImageIndex;
    idx := compItem.Index;
    compItem.Delete;
    with compList.Items.Insert(idx - 1) do begin
      Caption := s1;
      ImageIndex := img;
      Selected := true;
      Focused := true;
      MakeVisible(false);
    end;
  end;
end;

procedure TFMadExceptProjectSettings.CompDownBtnClick(Sender: TObject);
var formItem, compItem : TListItem;
    dfmObj             : TDfmObject;
    s1                 : string;
    img, idx           : integer;
begin
  formItem := FormList.Selected;
  compItem := CompList.Selected;
  if (formItem <> nil) and (compItem <> nil) and (compItem.Index > 3) and
     (formItem.Index < Length(FForms)) and (compItem.Index < Length(FForms[formItem.Index]) - 1) then begin
    dfmObj := FForms[formItem.Index, compItem.Index + 1];
    FForms[formItem.Index, compItem.Index + 1] := FForms[formItem.Index, compItem.Index];
    FForms[formItem.Index, compItem.Index] := dfmObj;
    s1 := compItem.Caption;
    img := compItem.ImageIndex;
    idx := compItem.Index;
    compItem.Delete;
    with compList.Items.Insert(idx + 1) do begin
      Caption := s1;
      ImageIndex := img;
      Selected := true;
      Focused := true;
      MakeVisible(false);
    end;
  end;
end;

function TFMadExceptProjectSettings.EditAssistant(var assis: string) : boolean;
var astr1, astr2 : TDAString;
    i1, i2       : integer;
begin
  SetLength(astr1, Length(FAssis));
  i2 := 0;
  for i1 := 0 to high(astr1) do begin
    astr1[i2] := SubStr(FAssis[i1], 1);
    if not IsTextEqual(astr1[i1], SubStr(assis, 1)) then
      inc(i2);
  end;
  SetLength(astr1, i2);
  SetLength(astr2, Length(FForms));
  for i1 := 0 to high(astr2) do
    astr2[i1] := FForms[i1, 0].name;
  result := EditAssis(assis, astr1, astr2);
end;

procedure TFMadExceptProjectSettings.AssisAddBtnClick(Sender: TObject);
var s1, s2 : string;
    i1     : integer;
begin
  s1 := '';
  if EditAssistant(s1) then begin
    i1 := Length(FAssis);
    SetLength(FAssis, i1 + 1);
    FAssis[i1] := s1;
    with AssisList.Items.Add do begin
      Caption := SubStr(s1, 1);
      SubItems.Add(SubStr(s1, 2));
      s2 := '';
      for i1 := 3 to SubStrCount(s1) do
        s2 := s2 + ', ' + SubStr(s1, i1);
      System.Delete(s2, 1, 2);
      SubItems.Add(s2);
      ImageIndex := 0;
    end;
    FillAssisCombos;
  end;
end;

procedure TFMadExceptProjectSettings.AssisDelBtnClick(Sender: TObject);
var li : TListItem;
    i1 : integer;
begin
  li := AssisList.Selected;
  if (li <> nil) and (li.Index < Length(FAssis)) and
     (MessageBox(Handle, pchar('Do you really want to delete the assistant "' + li.Caption + '"?'),
                 'Question...', MB_YESNO or MB_ICONQUESTION) = IDYES) then begin
    for i1 := li.Index to high(FAssis) - 1 do
      FAssis[i1] := FAssis[i1 + 1];
    SetLength(FAssis, Length(FAssis) - 1);
    li.Delete;
    FillAssisCombos;
  end;
end;

procedure TFMadExceptProjectSettings.AssisListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  CheckAssisButtons;
end;

procedure TFMadExceptProjectSettings.AssisListMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var p1 : TPoint;
begin
  if Button = mbRight then begin
    p1 := AssisList.ClientToScreen(Point(X, Y));
    AssisPopup.Popup(p1.X, p1.Y);
  end;
end;

procedure TFMadExceptProjectSettings.AssisListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) and AssisDelBtn.Enabled then
    AssisDelBtnClick(nil)
  else
    if (Key = VK_INSERT) and AssisAddBtn.Enabled then
      AssisAddBtnClick(nil)
    else
      if ((Key = VK_RETURN) or (Key = VK_F2)) and AssisChangeBtn.Enabled then
        AssisChangeBtnClick(nil);
end;

procedure TFMadExceptProjectSettings.AssisPlayBtnClick(Sender: TObject);
var li     : TListItem;
    i1, i2 : integer;
    astr   : TDAString;
    s1     : string;
begin
  li := AssisList.Selected;
  if (li <> nil) and (li.Index < Length(FAssis)) then begin
    astr := nil;
    for i1 := 3 to SubStrCount(FAssis[li.Index]) do begin
      s1 := SubStr(FAssis[li.Index], i1);
      for i2 := 0 to high(FForms) do
        if IsTextEqual(FForms[i2, 0].name, s1) then begin
          SetLength(astr, Length(astr) + 1);
          astr[high(astr)] := StructToDfm(FForms[i2]);
        end;
    end;
    if astr <> nil then begin
      Enabled := false;
      CreateAssistant(SubStr(FAssis[li.Index], 2), astr).ShowModal(Handle);
      Enabled := true;
      BringToFront;
    end;
  end;
end;

procedure TFMadExceptProjectSettings.AssisChangeBtnClick(Sender: TObject);
var li     : TListItem;
    i1     : integer;
    s1, s2 : string;
begin
  li := AssisList.Selected;
  if (li <> nil) and (li.Index < Length(FAssis)) then begin
    s1 := FAssis[li.Index];
    if EditAssistant(s1) then begin
      s2 := FAssis[li.Index];
      FAssis[li.Index] := s1;
      FillAssisCombos(SubStr(s2, 1), SubStr(s1, 1));
      li.Caption := SubStr(s1, 1);
      li.SubItems[0] := SubStr(s1, 2);
      s2 := '';
      for i1 := 3 to SubStrCount(s1) do
        s2 := s2 + ', ' + SubStr(s1, i1);
      System.Delete(s2, 1, 2);
      li.SubItems[1] := s2;
    end;
  end;
end;

procedure TFMadExceptProjectSettings.SendAssisComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var i1 : integer;
begin
  with TComboBox(Control) do begin
    Canvas.FillRect(Rect);
    if Items[Index][1] <> '-' then begin
      ImageList_Draw(CompImgs.Handle, 0, Canvas.Handle, Rect.Left + 1, Rect.Top, ILD_NORMAL);
      i1 := 20;
    end else
      i1 := 8;
    if Enabled then
      Font.Color := clBtnText
    else
      Font.Color := clBtnShadow;
    try
      Canvas.TextOut(Rect.Left + i1, Rect.Top + 1, Items[Index]);
    except end;
  end;
end;

procedure TFMadExceptProjectSettings.StringListCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if not Sender.Enabled then
    Sender.Canvas.Brush.Color := GetSysColor(COLOR_WINDOW);
end;

procedure TFMadExceptProjectSettings.FormShow(Sender: TObject);
begin
  if FSideTabs = nil then begin
    FSideTabs := TSideTabs.Create(TabPanel);
    with FSideTabs do begin
      AddTab( BasicPanel, 'Basic',      'basic|settings'            );
      AddTab(  AutoPanel, 'Auto',       'on exception|auto actions' );
      AddTab(FilterPanel, 'Filter',     'exception|filter'          );
      AddTab(LayoutPanel, 'Layout',     'exception|box settings'    );
      AddTab(  SendPanel, 'Send',       'email & upload|settings'   );
      AddTab(AttachPanel, 'Attach',     'attachment|settings'       );
      AddTab(  SavePanel, 'Save',       'save|settings'             );
      AddTab(ReportPanel, 'BugReport',  'bug report|settings'       );
      AddTab( AssisPanel, 'Assistant',  'assistant|creator'         );
      AddTab(StringPanel, 'Locale',     'custom|strings'            );
    end;
  end;
end;

procedure TFMadExceptProjectSettings.SmtpServerRadioClick(Sender: TObject);
begin
  CheckControls(nil);
  case directCheck.Tag of  // old radio state
    0 : ;  // no radio button set
    1 : ;  // smtp server
    2 : begin
          FSmtpServer     :=           sendServerEdit.Text;
          FSmtpPort       := StrToIntDef(sendPortEdit.Text, 25);
          if useAuthCheck.Checked then begin
            FSmtpAccount  :=          sendAccountEdit.Text;
            FSmtpPassword :=         sendPasswordEdit.Text;
          end else begin
            FSmtpAccount  := '';
            FSmtpPassword := '';
          end;
        end;
    3 : begin
          FHttpServer     :=           sendServerEdit.Text;
          FHttpPort       := StrToIntDef(sendPortEdit.Text, 25);
          if useAuthCheck.Checked then begin
            FHttpAccount  :=          sendAccountEdit.Text;
            FHttpPassword :=         sendPasswordEdit.Text;
          end else begin
            FHttpAccount  := '';
            FHttpPassword := '';
          end;
        end;
  end;
  if      smtpServerRadio.Checked then directCheck.Tag := 1
  else if smtpClientRadio.Checked then directCheck.Tag := 2
  else if httpUploadRadio.Checked then directCheck.Tag := 3
  else                                 directCheck.Tag := 0;
  case directCheck.Tag of
    0 : ;
    1 : ;
    2 : begin
          sendServerLabel.Caption := 'SMTP server:';
            sendServerEdit.Text    := FSmtpServer;
              sendPortEdit.Text    := IntToStrEx(FSmtpPort);
              useAuthCheck.Checked := (FSmtpAccount <> '') or (FSmtpPassword <> '');
           sendAccountEdit.Text    := FSmtpAccount;
          sendPasswordEdit.Text    := FSmtpPassword;
        end;
    3 : begin
          sendServerLabel.Caption := 'HTTP server:';
            sendServerEdit.Text    := FHttpServer;
              sendPortEdit.Text    := IntToStrEx(FHttpPort);
              useAuthCheck.Checked := (FHttpAccount <> '') or (FHttpPassword <> '');
           sendAccountEdit.Text    := FHttpAccount;
          sendPasswordEdit.Text    := FHttpPassword;
        end;
  end;
end;

procedure TFMadExceptProjectSettings.StringListKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    if (Sender as TListView).Selected <> nil then begin
      StringMemo.SetFocus;
      StringMemo.SelectAll;
    end;
    Key := #0;
  end;
end;

procedure TFMadExceptProjectSettings.StringListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var li : TListItem;
    s1 : string;
begin
  li := (Sender as TListView).Selected;
  if li <> nil then begin
    s1 := li.SubItems[0];
    ReplaceStr(s1, '%LF%', #$D#$A);
    StringMemo.Enabled := true;
    StringMemo.Color := GetSysColor(COLOR_WINDOW);
    StringMemo.Text := s1;
  end else begin
    StringMemo.Clear;
    StringMemo.Enabled := false;
    StringMemo.Color := DecCol(GetSysColor(COLOR_BTNFACE), $080808, true);
  end;
end;

procedure TFMadExceptProjectSettings.AttachListSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var b2 : boolean;
begin
  if AttachList.Selected <> nil then begin
    b2 := enableMECheck.Checked and (not noMesCheck.Checked);
    DelAttachBtn.Enabled := b2;
    EditAttachBtn.Enabled := b2;
  end else begin
    DelAttachBtn.Enabled := false;
    EditAttachBtn.Enabled := false;
  end;
end;

procedure TFMadExceptProjectSettings.DelAttachBtnClick(Sender: TObject);
var li : TListItem;
begin
  if not DelAttachBtn.Enabled then begin
    beep;
    exit;
  end;
  li := AttachList.Selected;
  if li <> nil then begin
    if li.ImageIndex = 7 then
      attachBugRepCheck.Checked := false
    else
      if li.ImageIndex = 8 then
        attachScreenShotCheck.Checked := false
      else
        li.Delete;
  end else
    beep;
end;

procedure TFMadExceptProjectSettings.AddAttachBtnClick(Sender: TObject);
var s1, s2, s3 : string;
begin
  if not AddAttachBtn.Enabled then begin
    beep;
    exit;
  end;
  if EditAttach(s1, s2, s3) then
    with AttachList.Items.Add do begin
      ImageIndex := 9;
      Caption := s1;
      SubItems.Add(s2);
      SubItems.Add(s3);
    end;
end;

procedure TFMadExceptProjectSettings.EditAttachBtnClick(Sender: TObject);
var li         : TListItem;
    s1, s2, s3 : string;
begin
  if not EditAttachBtn.Enabled then begin
    beep;
    exit;
  end;
  li := AttachList.Selected;
  if li <> nil then begin
    s1 := li.Caption;
    s2 := li.SubItems[0];
    s3 := li.SubItems[1];
    if EditAttach(s1, s2, s3) then begin
      li.Caption := s1;
      li.SubItems[0] := s2;
      li.SubItems[1] := s3;
    end;
  end else
    beep;
end;

procedure TFMadExceptProjectSettings.AttachListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) and DelAttachBtn.Enabled then
    DelAttachBtnClick(nil)
  else
    if (Key = VK_INSERT) and AddAttachBtn.Enabled then
      AddAttachBtnClick(nil)
    else
      if (Key = VK_RETURN) and EditAttachBtn.Enabled then
        EditAttachBtnClick(nil);
end;

procedure TFMadExceptProjectSettings.AttachListDblClick(Sender: TObject);
begin
  if EditAttachBtn.Enabled then
    EditAttachBtnClick(nil);
end;

initialization
  DelphiOrBCB;
finalization
  try
    log('finalization'); indentLog;
    CloseMadExceptWizard;
    unindentLog;
  except {on E: Exception do WizardHandleException(E)} end;
end.
 