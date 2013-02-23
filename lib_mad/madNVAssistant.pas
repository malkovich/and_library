// ***************************************************************
//  madNVAssistant.pas        version:  1.0a  ·  date: 2006-02-23
//  -------------------------------------------------------------
//  thread safe "non VCL" forms / assistants
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2006 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2006-02-23 1.0b INVAssistant.Title property added
// 2005-10-09 1.0a (1) memory leak fixed
//                 (2) INVForm.Name added
//                 (3) INVForm.Skip added
//                 (4) INVAssistant.Form renamed to "Forms" (index search)
//                 (5) INVAssistant.Form added (name search)
//                 (6) global OnAssistantCreate handler added
//                 (7) INVEdit.Colored added
//                 (8) INVEdit.Valid added
//                 (9) pressing [X] on assistant forms behaved incorrectly
//                 (a) "Abort" renamed to "Cancel" everywhere
//                 (b) "Visible" property added for main buttons
//                 (c) font "Tahoma" used now instead of "MS Sans Serif"
//                 (d) main buttons in "NoOwnerDraw" style are now smaller
//                 (e) ContinueButton now said "Next >" instead of "Continue"
//                 (f) NoOwnerDraw + hidden SkipBtn -> ContinueBtn pos. changed
//                 (g) INVLabel/Button/CheckBox.Caption is now writable
//                 (h) INVForm.Title + INVForm.Message now writable
// 2005-07-09 1.0  initial release

unit madNVAssistant;

{$I mad.inc}

interface

uses Windows, madNVBitmap;

// ***************************************************************

type
  TOutputType = (otHeader, otOwnSection);

  INVItem = interface ['{36EE4E77-B6B3-41D0-ABA7-3D9B90847EC0}']
    function  GetName    : string;
    function  GetHandle  : dword;
    function  GetTabStop : boolean;
    function  GetSpacing : integer;
    function  GetEnabled : boolean;
    procedure SetEnabled (value: boolean);

    property  Name    : string  read GetName;
    property  Handle  : dword   read GetHandle;
    property  TabStop : boolean read GetTabStop;
    property  Spacing : integer read GetSpacing;
    property  Enabled : boolean read GetEnabled write SetEnabled;
    procedure SetFocus;
  end;

  INVButton = interface (INVItem) ['{20E07644-4788-4FDF-8F90-B62C0E61E20B}']
    function  GetCaption  : string;
    procedure SetCaption  (value: string);
    function  GetMinWidth : integer;

    property  Caption  : string  read GetCaption  write SetCaption;
    property  MinWidth : integer read GetMinWidth;
  end;

  INVLabel = interface (INVItem) ['{EA968486-7DA2-486F-9D40-DE0B7BDECBEC}']
    function  GetCaption : string;
    procedure SetCaption (value: string);

    property  Caption : string read GetCaption write SetCaption;
  end;

  INVEdit = interface (INVItem) ['{22AC205D-0FED-425B-BEBB-9FBAB850C1DC}']
    function  GetLines      : integer;
    function  GetOptional   : boolean;
    function  GetText       : string;
    procedure SetText       (value: string);
    function  GetColored    : boolean;
    procedure SetColored    (value: boolean);
    function  GetValid      : boolean;
    procedure SetValid      (value: boolean);
    function  GetOutputType : TOutputType;
    procedure SetOutputType (value: TOutputType);
    function  GetOutputName : string;
    procedure SetOutputName (value: string);

    property  Lines      : integer     read GetLines;
    property  Optional   : boolean     read GetOptional;
    property  Text       : string      read GetText       write SetText;
    property  Colored    : boolean     read GetColored    write SetColored;
    property  Valid      : boolean     read GetValid      write SetValid;
    property  OutputType : TOutputType read GetOutputType write SetOutputType;
    property  OutputName : string      read GetOutputName write SetOutputName;
  end;

  INVCheckBox = interface (INVItem) ['{34E76977-C2B5-4819-B8F6-B6170089A03C}']
    function  GetCaption    : string;
    procedure SetCaption    (value: string);
    function  GetChecked    : boolean;
    procedure SetChecked    (value: boolean);
    function  GetOutputName : string;
    procedure SetOutputName (value: string);

    property  Caption    : string  read GetCaption    write SetCaption;
    property  Checked    : boolean read GetChecked    write SetChecked;
    property  OutputName : string  read GetOutputName write SetOutputName;
  end;

  INVImage = interface (INVItem) ['{A0DE7AC1-F790-4BDB-B6B2-453AF25A6B7B}']
    function  GetFile      : string;
    function  GetBitmap    : INVBitmap;
    procedure SetBitmap    (value: INVBitmap);
    function  GetClickable : boolean;
    function  GetBorder    : boolean;
    function  GetWidth     : integer;
    function  GetHeight    : integer;

    property  File_     : string    read GetFile;
    property  Bitmap    : INVBitmap read GetBitmap write SetBitmap;
    property  Clickable : boolean   read GetClickable;
    property  Border    : boolean   read GetBorder;
    property  Width     : integer   read GetWidth;
    property  Height    : integer   read GetHeight;
  end;

  INVForm = interface;
  INVAssistant = interface;

  TNVModalResult = (nvmInvalid, nvmOk, nvmSkip, nvmCancel);
  TNVAction = (nvaFormCreate, nvaFormShow, nvaFormClose, nvaItemEvent);
  TNVActionHandler = procedure (form: INVForm; action: TNVAction; item: INVItem; exception: IUnknown);
                                                                              // exception: IMEException);  <-  doesn't compile because madExcept is not in our uses clause 

  INVForm = interface ['{9C62303F-3C4A-4E2F-9C14-74C518871995}']
    function  GetAssistant      : INVAssistant;
    function  GetName           : string;
    function  GetTitle          : string;
    procedure SetTitle          (value: string);
    function  GetMessage_       : string;
    procedure SetMessage        (value: string);
    function  GetItemCount      : integer;
    function  GetItem           (index: integer) : INVItem;
    function  GetContinueButton : INVButton;
    function  GetSkipButton     : INVButton;
    function  GetCancelButton   : INVButton;
    function  GetActiveControl  : string;
    procedure SetActiveControl  (value: string);
    function  GetTimer          : dword;
    function  GetHandle         : dword;
    function  GetModalResult    : TNVModalResult;
    function  GetOnAction       : TNVActionHandler;
    procedure SetOnAction       (value: TNVActionHandler);

    property  Assistant      : INVAssistant             read GetAssistant;
    property  Name           : string                   read GetName;
    property  Title          : string                   read GetTitle         write SetTitle;
    property  Message        : string                   read GetMessage_      write SetMessage;
    property  ItemCount      : integer                  read GetItemCount;
    property  Items          [index: integer] : INVItem read GetItem;
    property  ContinueButton : INVButton                read GetContinueButton;
    property  SkipButton     : INVButton                read GetSkipButton;
    property  CancelButton   : INVButton                read GetCancelButton;
    property  ActiveControl  : string                   read GetActiveControl write SetActiveControl;
    property  Timer          : dword                    read GetTimer;
    property  Handle         : dword                    read GetHandle;
    property  ModalResult    : TNVModalResult           read GetModalResult;
    property  OnAction       : TNVActionHandler         read GetOnAction      write SetOnAction;

    function  nvEdit     (item: string) : INVEdit;
    function  nvLabel    (item: string) : INVLabel;
    function  nvButton   (item: string) : INVButton;
    function  nvCheckBox (item: string) : INVCheckBox;
    function  nvImage    (item: string) : INVImage;

    procedure Skip;

    function  ShowModal (parentWindow: dword = 0) : TNVModalResult;

    function  GetException : IUnknown;
    procedure SetException (value: IUnknown);
    property  Exception : IUnknown read GetException write SetException;

    function  GetData : pointer;
    procedure SetData (value: pointer);
    property  Data : pointer read GetData write SetData;
  end;

  INVAssistant = interface ['{14A3AFE4-B39F-4745-8F85-08FFABE8771C}']
    function  GetTitle       : string;
    procedure SetTitle       (value: string);
    function  GetFormCount   : integer;
    function  GetForms       (index : integer) : INVForm;
    function  GetForm        (name  : string ) : INVForm;
    function  GetModalResult : TNVModalResult;
    function  GetOnAction    : TNVActionHandler;
    procedure SetOnAction    (value: TNVActionHandler);

    property  Title       : string                    read GetTitle    write SetTitle;
    property  FormCount   : integer                   read GetFormCount;
    property  Forms       [index : integer] : INVForm read GetForms;
    property  Form        [name  : string ] : INVForm read GetForm;
    property  ModalResult : TNVModalResult            read GetModalResult;
    property  OnAction    : TNVActionHandler          read GetOnAction write SetOnAction;

    function  ShowModal (parentWindow: dword = 0) : TNVModalResult;

    function  GetException : IUnknown;
    procedure SetException (value: IUnknown);
    property  Exception : IUnknown read GetException write SetException;

    function  GetData : pointer;
    procedure SetData (value: pointer);
    property  Data : pointer  read GetData write SetData;
  end;

var
  OnAssistantCreate : procedure (const assistant: INVAssistant; const exception: IUnknown) = nil;

// ***************************************************************
// for internal use only - please ignore
function CreateAssistant (title      : string;
                          const dfms : array of string;
                          exception  : IUnknown = nil ) : INVAssistant;
function LoadAssistant (module      : dword;
                        title       : string;
                        const forms : array of string;
                        exception   : IUnknown = nil ) : INVAssistant;
var
  HandleContactFormProc    : pointer = nil;
  HandleScreenshotFormProc : pointer = nil;

implementation

uses Messages, madDisAsm, madMapFile, madTools, madStrings, madTypes;

// ***************************************************************

type
  TNVItem      = class;
  TNVEdit      = class;
  TNVButton    = class;
  TNVLabel     = class;
  TNVCheckBox  = class;
  TNVImage     = class;
  TNVForm      = class;
  TNVAssistant = class;

  // ***************************************************************

  INVItemEx = interface (INVItem) ['{94D23487-7861-4FD8-93FA-E62680ECAC99}']
    function GetTNVItem : TNVItem;
  end;

  // ***************************************************************

  TNVItem = class (TInterfacedObject, INVItem, INVItemEx)
  private
    FParent        : TNVForm;
    FName          : string;
    FHandle        : dword;
    FTabStop       : boolean;
    FEnabled       : boolean;
    FForceDisabled : boolean;
    FSpacing       : integer;
    FNext          : array [boolean] of INVItemEx;
    FIndex         : integer;
    FRect          : TRect;
    FOutputType    : TOutputType;
    FOutputName    : string;
    constructor Create (parent: TNVForm; name: string);
    function  GetTNVItem : TNVItem;
    procedure SetProperty (prop, value: string); virtual;
    procedure Show (ix: integer; var iy: integer; fontDC: dword); virtual;
    procedure FormPaint (dc: dword); virtual;
    procedure OwnerDraw (dc, itemState: dword; rect: TRect); virtual;
    function  MayEnableContinueButton : boolean; virtual;
    procedure CreateWindow (ix, iy: integer; clss, text: string; style: dword; tabStop: boolean);
    procedure SetForceDisabled (forceDisabled: boolean);
    function  GetOutputType : TOutputType;
    procedure SetOutputType (value: TOutputType);
    function  GetOutputName : string;
    procedure SetOutputName (value: string);
    function  GetName    : string;
    function  GetHandle  : dword;
    function  GetTabStop : boolean;
    function  GetSpacing : integer;
    function  GetEnabled : boolean;
    procedure SetEnabled (value: boolean);
    procedure SetFocus;
  end;

  TNVButton = class (TNVItem, INVButton)
  private
    FMinWidth    : integer;
    FCaption     : string;
    FNoOwnerDraw : boolean;
    FInvisible   : boolean;
    constructor Create (parent: TNVForm; name, caption: string);
    procedure SetProperty (prop, value: string); override;
    procedure Show (ix: integer; var iy: integer; fontDC: dword); override;
    procedure OwnerDraw (dc, itemState: dword; rect: TRect); override;
    function  GetCaption : string;
    procedure SetCaption (value: string);
    function  GetMinWidth : integer;
  end;

  TNVLabel = class (TNVItem, INVLabel)
  private
    FCaption : string;
    procedure SetProperty (prop, value: string); override;
    procedure Show (ix: integer; var iy: integer; fontDC: dword); override;
    function CalcRect (fontDC: dword) : TRect;
    function IsHeadLabel (fontDC: dword) : boolean;
    function GetCaption : string;
    procedure SetCaption (value: string);
  end;

  TNVEdit = class (TNVItem, INVEdit)
  private
    FLines    : integer;
    FOptional : boolean;
    FText     : string;
    FNColored : boolean;
    FNValid   : boolean;
    procedure SetProperty (prop, value: string); override;
    procedure Show (ix: integer; var iy: integer; fontDC: dword); override;
    function  MayEnableContinueButton : boolean; override;
    procedure FormPaint (dc: dword); override;
    function  GetLines    : integer;
    function  GetOptional : boolean;
    function  GetText     : string;
    procedure SetText     (value: string);
    function  GetColored  : boolean;
    procedure SetColored  (value: boolean);
    function  GetValid    : boolean;
    procedure SetValid    (value: boolean);
  end;

  TNVCheckBox = class (TNVItem, INVCheckBox)
  private
    FCaption : string;
    FChecked : boolean;
    procedure SetProperty (prop, value: string); override;
    procedure Show (ix: integer; var iy: integer; fontDC: dword); override;
    procedure OwnerDraw(dc, itemState: dword; rect: TRect); override;
    function  GetCaption : string;
    procedure SetCaption (value: string);
    function  GetChecked : boolean;
    procedure SetChecked (value: boolean);
  end;

  TNVImage = class (TNVItem, INVImage)
  private
    FFile      : string;
    FBitmap    : INVBitmap;
    FClickable : boolean;
    FBorder    : boolean;
    procedure SetProperty (prop, value: string); override;
    procedure Show (ix: integer; var iy: integer; fontDC: dword); override;
    procedure FormPaint (dc: dword); override;
    function  GetFile      : string;
    function  GetBitmap    : INVBitmap;
    procedure SetBitmap    (value: INVBitmap);
    function  GetClickable : boolean;
    function  GetBorder    : boolean;
    function  GetWidth     : integer;
    function  GetHeight    : integer;
  end;

  // ***************************************************************

  TNVForm = class (TInterfacedObject, INVForm)
  private
    FParent         : TNVAssistant;
    FName           : string;
    FTitle          : string;
    FMessage        : string;
    FStep, FSteps   : integer;
    FItems          : array of INVItemEx;
    FOnAction       : TNVActionHandler;
    Handle          : dword;
    FWidth, FHeight : integer;
    FFont           : dword;
    MinWidth        : integer;
    LastFocus       : dword;
    FActiveControl  : string;
    FTimer          : dword;
    FCap            : INVItem;
    FModalResult    : TNVModalResult;
    FException      : IUnknown;
    FData           : pointer;
    FSkip           : boolean;
    constructor Create (parent: TNVAssistant; title, dfm: string;
                        exception: IUnknown; currentStep, maxStep: integer);
    destructor Destroy; override;
    function  DialogWndMethod (window, msg: dword; wParam, lParam: integer) : integer; stdcall;
    procedure CheckContinueButton;
    function  FindClass  (item: string; guid: TGuid) : INVItem;
    function  FindHandle (handle: dword) : INVItemEx;
    procedure FocusNext  (item: INVItemEx; down: boolean);
    procedure FireOnAction (form: INVForm; action: TNVAction; item: INVItem);
    function  GetAssistant      : INVAssistant;
    function  GetName           : string;
    function  GetTitle          : string;
    procedure SetTitle          (value: string);
    function  GetMessage_       : string;
    procedure SetMessage        (value: string);
    function  GetItemCount      : integer;
    function  GetItem           (index: integer) : INVItem;
    function  GetContinueButton : INVButton;
    function  GetSkipButton     : INVButton;
    function  GetCancelButton   : INVButton;
    function  GetActiveControl  : string;
    procedure SetActiveControl  (value: string);
    function  GetTimer          : dword;
    function  GetHandle         : dword;
    function  GetModalResult    : TNVModalResult;
    function  GetOnAction       : TNVActionHandler;
    procedure SetOnAction       (value: TNVActionHandler);
    function  nvEdit     (item: string) : INVEdit;
    function  nvLabel    (item: string) : INVLabel;
    function  nvButton   (item: string) : INVButton;
    function  nvCheckBox (item: string) : INVCheckBox;
    function  nvImage    (item: string) : INVImage;
    procedure Skip;
    function  ShowModal (parentWindow: dword = 0) : TNVModalResult;
    function  GetException : IUnknown;
    procedure SetException (value: IUnknown);
    function  GetData : pointer;
    procedure SetData (value: pointer);
  end;

  // ***************************************************************

  TNVAssistant = class (TInterfacedObject, INVAssistant)
  private
    FTitle       : string;
    FForms       : array of INVForm;
    FModalResult : TNVModalResult;
    FOnAction    : TNVActionHandler;
    FException   : IUnknown;
    FData        : pointer;
    constructor Create (title: string; const dfms: array of string; exception: IUnknown);
    function  GetTitle : string;
    procedure SetTitle (value: string);
    function  GetFormCount : integer;
    function  GetForms (index: integer) : INVForm;
    function  GetForm (name: string) : INVForm;
    function  GetModalResult : TNVModalResult;
    function  GetOnAction : TNVActionHandler;
    procedure SetOnAction (value: TNVActionHandler);
    function  ShowModal (parentWindow: dword = 0) : TNVModalResult;
    function  GetException : IUnknown;
    procedure SetException (value: IUnknown);
    function  GetData : pointer;
    procedure SetData (value: pointer);
  end;

// ***************************************************************
// tool functions

function BooleanValue(value: string) : boolean;
begin
  result := IsTextEqual(value, 'yes') or IsTextEqual(value, 'true') or (value = '1');
end;

function IntegerValue(value: string) : integer;
begin
  result := StrToIntEx(false, pchar(value), Length(value));
end;

function DecCol(col: dword; dif: dword; dec: boolean) : dword;
// calculate a ligher (dec = true) or darker (dec = false) color
var b1, b2, b3 : byte;
    d1, d2, d3 : byte;
    i1         : integer;
begin
  b1 := byte(col       ); d1 := byte(dif       );
  b2 := byte(col shr  8); d2 := byte(dif shr  8);
  b3 := byte(col shr 16); d3 := byte(dif shr 16);
  i1 := 0;
  if dec then begin
    if dword(b1) + dword(d1) > $ff then begin inc(i1); b1 := $ff end else b1 := b1 + d1;
    if dword(b2) + dword(d2) > $ff then begin inc(i1); b2 := $ff end else b2 := b2 + d2;
    if dword(b3) + dword(d3) > $ff then begin inc(i1); b3 := $ff end else b3 := b3 + d3;
  end else begin
    if b1 < d1 then begin inc(i1); b1 := 0 end else b1 := b1 - d1;
    if b2 < d2 then begin inc(i1); b2 := 0 end else b2 := b2 - d2;
    if b3 < d3 then begin inc(i1); b3 := 0 end else b3 := b3 - d3;
  end;
  if i1 > 1 then
       result := DecCol(col, dif, not dec)
  else result := b3 shl 16 + b2 shl 8 + b1;
end;

function GetFont(font: string; size: integer; bold: boolean) : dword;
// get a font handle with the specified attributes
var weight : dword;
begin
  if bold then
       weight := 700
  else weight := 400;
  result := CreateFont(-size, 0, 0, 0, weight, 0, 0, 0, DEFAULT_CHARSET,
                       OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                       DEFAULT_PITCH or FF_DONTCARE, pchar(font));
  if result = 0 then
    result := CreateFont(-size, 0, 0, 0, weight, 0, 0, 0, DEFAULT_CHARSET,
                         OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                         DEFAULT_PITCH or FF_DONTCARE, 'MS Sans Serif');
end;

function GetTextExtent(dc: dword; text: string; defaultX: integer = 0; defaultY: integer = 0) : TPoint;
// calculate the extent of a specific text on the specified dc
begin
  if not GetTextExtentPoint32(dc, pchar(text), Length(text), TSize(result)) then begin
    result.X := defaultX;
    result.Y := defaultY;
  end;
end;

procedure FillRect(dc: dword; rect: TRect; inflate: integer; col: dword); overload;
// draw a filled rectangle to the specified dc with the specific attributes 
var c1 : dword;
begin
  if inflate <> 0 then
    InflateRect(rect, inflate, inflate);
  c1 := CreateSolidBrush(col);
  windows.FillRect(dc, rect, c1);
  DeleteObject(c1);
end;

procedure FillRect(dc, ix1, iy1, ix2, iy2, col: dword); overload;
var r1 : TRect;
begin
  r1.Left   := ix1;
  r1.Top    := iy1;
  r1.Right  := ix2 + 1;
  r1.Bottom := iy2 + 1;
  FillRect(dc, r1, 0, col);
end;

procedure LineRect(dc: dword; ix1, iy1, ix2, iy2: integer; width: integer; style, backCol, penCol: dword); overload;
// draw a rectangle border to the specific dc with the specific attributes
// dotted lines of any width are supported
var pen, oldPen : dword;
    i1          : integer;
    oldCol      : dword;
begin
  // setup the pen color and mode
  pen := CreatePen(style, 1, penCol);
  oldPen := SelectObject(dc, pen);
  oldCol := SetBkColor(dc, backCol);
  // draw the lines one by one, only this way we can draw fat dotted lines
  for i1 := 0 to width - 1 do begin
    if MoveToEx(dc, ix1,      iy1 + i1, nil) then LineTo(dc, ix2 +  1, iy1 + i1);
    if MoveToEx(dc, ix1 + i1, iy1,      nil) then LineTo(dc, ix1 + i1, iy2 +  1);
    if MoveToEx(dc, ix2,      iy2 - i1, nil) then LineTo(dc, ix1 -  1, iy2 - i1);
    if MoveToEx(dc, ix2 - i1, iy2,      nil) then LineTo(dc, ix2 - i1, iy1 -  1);
  end;
  // clear up the pen handle mess
  SetBkColor(dc, oldCol);
  SelectObject(dc, oldPen);
  DeleteObject(pen);
end;

procedure LineRect(dc: dword; rect: TRect; inflate, width: integer; style, backCol, penCol: dword); overload;
begin
  if inflate <> 0 then
    InflateRect(rect, inflate, inflate);
  LineRect(dc, rect.Left, rect.Top, rect.Right - 1, rect.Bottom - 1, width, style, backCol, penCol);
end;

procedure WrText(dc: dword; ix, iy: integer; text, font: string; size: integer;
                 bold, whiteUnderground: boolean; color: dword;
                 iw: integer = 0; ih: integer = 0);
// write text to the specific dc with the specified attributes
// if "iw" and "ih" are defined, the text is automatically centered
var newfont, oldfont : dword;
    te               : TSize;
    ix2, iy2         : integer;
begin
  // first create and select the font
  newfont := GetFont(font, size, bold);
  oldfont := SelectObject(dc, newfont);
  // next calculate the size - if we are supposed to center the text
  if (iw <> 0) and GetTextExtentPoint32(dc, pchar(text), Length(text), te) then begin
    inc(ix, (iw - te.cx) div 2);
    inc(iy, (ih - te.cy) div 2);
  end;
  // now set the mode and color
  SetBkMode(dc, TRANSPARENT);
  SetTextColor(dc, color);
  // finally print the text out
  if whiteUnderground then begin
    SetTextColor(dc, $ffffff);
    for ix2 := -1 to +1 do
      for iy2 := -1 to +1 do
        if (ix2 <> 0) or (iy2 <> 0) then
          TextOut(dc, ix + ix2, iy + iy2, pchar(text), length(text));
    SetTextColor(dc, color);
  end;
  TextOut(dc, ix, iy, pchar(text), length(text));
  // clear up the font handle mess
  SelectObject(dc, oldfont);
  DeleteObject(newfont);
end;

// ***************************************************************

constructor TNVItem.Create(parent: TNVForm; name: string);
begin
  inherited Create;
  FParent  := parent;
  FName    := name;
  FEnabled := true;
end;

function TNVItem.GetTNVItem : TNVItem;
begin
  result := self;
end;

procedure TNVItem.SetProperty(prop, value: string);

  function StrToOutputType(value: string) : TOutputType;
  begin
    if PosText('ownSection', value) > 0 then
         result := otOwnSection
    else result := otHeader;
  end;

begin
  if      prop = 'Spacing'    then FSpacing    := IntegerValue(value)
  else if prop = 'Enabled'    then FEnabled    := BooleanValue(value)
  else if prop = 'OutputType' then FOutputType := StrToOutputType(value)
  else if prop = 'OutputName' then FOutputName := value;
end;

procedure TNVItem.Show(ix: integer; var iy: integer; fontDC: dword);
begin
  iy := FRect.Bottom;
  if FHandle <> 0 then begin
    SendMessage(FHandle, WM_SETFONT, integer(FParent.FFont), 0);
    ShowWindow(FHandle, SW_SHOW);
  end;
  if FSpacing <> 0 then
    // manual spacing
    inc(iy, FSpacing)
  else
    // automated spacing
    with FParent do
      if Self <> FItems[high(FItems)].GetTNVItem then
        // this is not the last item
        if (Self is TNVLabel) and TNVLabel(Self).IsHeadLabel(fontDC) then
          // a head label (e.g. "your name:"), followed by something else
          // so we use a quite small spacing
          inc(iy, 4)
        else
          if ClassName = FItems[FIndex + 1].GetTNVItem.ClassName then begin
            // we have two controls of the same type
            // the auto spacing between those depends on the control type
            if      Self is TNVButton   then inc(iy, 4)
            else if Self is TNVEdit     then inc(iy, 6)
            else if Self is TNVCheckBox then inc(iy, 8)
            else if Self is TNVLabel    then inc(iy, 8)
            else if Self is TNVImage    then inc(iy, 8);
          end else
            if (FItems[FIndex + 1].GetTNVItem is TNVLabel) and TNVLabel(FItems[FIndex + 1].GetTNVItem).IsHeadLabel(fontDC) and
               (FItems[FIndex - 1].GetTNVItem is TNVLabel) and TNVLabel(FItems[FIndex - 1].GetTNVItem).IsHeadLabel(fontDC) and
               (FIndex + 2 < length(FItems)) and (FItems[FIndex + 2].GetTNVItem.ClassName = ClassName) then
              // two controls of the same type, each with a head label above it
              inc(iy, 8)
            else
              // two different controls
              inc(iy, 12);
end;

procedure TNVItem.FormPaint(dc: dword); begin end;
procedure TNVItem.OwnerDraw(dc, itemState: dword; rect: TRect); begin end;
function  TNVItem.MayEnableContinueButton : boolean; begin result := true end;

procedure TNVItem.CreateWindow(ix, iy: integer; clss, text: string; style: dword; tabStop: boolean);
// create the window handle with the specified attributes
// plus offset "FRect" to the specified "ix" and "iy" position
begin
  FTabStop := tabStop;
  OffsetRect(FRect, ix, iy);
  if FTabStop then
    style := style or WS_TABSTOP;
  with FRect do
    FHandle := windows.CreateWindow(pchar(clss), pchar(text), style + WS_CHILD,
                                    Left, Top, Right - Left, Bottom - Top,
                                    FParent.Handle, 0, HInstance, nil);
  if (FHandle <> 0) and (not FEnabled) then
    EnableWindow(FHandle, false);
end;

procedure TNVItem.SetForceDisabled(forceDisabled: boolean);
// only used for the "continue" main button
// automatically disabled that button, if mandatory controls are not filled yet
begin
  FForceDisabled := forceDisabled;
  if FHandle <> 0 then
    EnableWindow(FHandle, FEnabled and (not FForceDisabled));
end;

function TNVItem.GetOutputType : TOutputType; begin result := FOutputType end;
function TNVItem.GetOutputName : string;      begin result := FOutputName end;
function TNVItem.GetName       : string;      begin result := FName       end;
function TNVItem.GetHandle     : dword;       begin result := FHandle     end;
function TNVItem.GetTabStop    : boolean;     begin result := FTabStop    end;
function TNVItem.GetSpacing    : integer;     begin result := FSpacing    end;
function TNVItem.GetEnabled    : boolean;     begin result := FEnabled    end;

procedure TNVItem.SetOutputType(value: TOutputType);
begin
  if (value = otHeader) or ((self is TNVEdit) and (TNVEdit(self).FLines > 1)) then
    FOutputType := value;
end;

procedure TNVItem.SetOutputName(value: string);
begin
  FOutputName := value;
end;

procedure TNVItem.SetEnabled(value: boolean);
begin
  FEnabled := value;
  if FHandle <> 0 then
    EnableWindow(FHandle, FEnabled and (not FForceDisabled));
end;

procedure TNVItem.SetFocus;
begin
  if FHandle <> 0 then begin
    windows.SetFocus(FHandle);
    if Self is TNVEdit then
      SendMessage(FHandle, EM_SETSEL, 0, -1);
  end;
end;                                  

// ***************************************************************

constructor TNVButton.Create(parent: TNVForm; name, caption: string);
begin
  inherited Create(parent, name);
  FCaption := caption;
end;

procedure TNVButton.SetProperty(prop, value: string);
begin
  if      prop = 'MinWidth'    then FMinWidth    := IntegerValue(value)
  else if prop = 'Caption'     then FCaption     := value
  else if prop = 'NoOwnerDraw' then FNoOwnerDraw := BooleanValue(value)
  else if prop = 'Visible'     then FInvisible   := not BooleanValue(value)
  else                              inherited;
end;

procedure TNVButton.Show(ix: integer; var iy: integer; fontDC: dword);
// not called for our 3 main buttons, only for the "normal" buttons
begin
  PPoint(@FRect.Right)^ := GetTextExtent(fontDC, GetCaption, 0, 13);
  inc(FRect.Right,  32);
  if FRect.Right < 70 then
    FRect.Right := 70;
  inc(FRect.Bottom, 10);
  if FMinWidth > FRect.Right then FRect.Right := FMinWidth;
  CreateWindow(ix, iy, 'Button', GetCaption, BS_NOTIFY, true);
  inherited;
end;

procedure TNVButton.OwnerDraw(dc: dword; itemState: dword; rect: TRect);
// not called for "normal" buttons, only called for our 3 main buttons
var c1, c2, c3 : dword;
    i1         : integer;
begin
  if      FHandle = FParent.GetContinueButton.Handle then begin c2 := $80a080; c3 := $020102 end
  else if FHandle = FParent.    GetSkipButton.Handle then begin c2 := $a08080; c3 := $010202 end
  else                                                    begin c2 := $8080a0; c3 := $020201 end;
  if itemState and ODS_DISABLED = 0 then begin
    if itemState and ODS_FOCUS <> 0 then begin
      FillRect(dc, rect,  0, GetSysColor(COLOR_WINDOWTEXT));
      c1 := 0;
      for i1 := rect.Bottom - 3 downto rect.Top + 2 do begin
        FillRect(dc, rect.Left + 2, i1, rect.Right - 2, i1 + 1, c1);
        c1 := c1 + ($020202 - c3) * 3;
      end;
      LineRect(dc, rect, -3, 1, PS_DOT, c1, c2);
      WrText(dc, rect.Left, rect.Top, GetCaption, 'Arial', 15, false, false,
             GetSysColor(COLOR_WINDOW), rect.Right, rect.Bottom);
    end else begin
      FillRect(dc, rect,  0, GetSysColor(COLOR_BTNSHADOW));
      FillRect(dc, rect, -2, GetSysColor(COLOR_BTNFACE));
      c1 := $ffffff;
      for i1 := rect.Top + 3 to rect.Bottom - 4 do begin
        FillRect(dc, rect.Left + 3, i1, rect.Right - 3, i1 + 1, c1);
        if odd(i1) then
          c1 := c1 - c3;
      end;
      WrText(dc, rect.Left, rect.Top, GetCaption, 'Arial', 15, false, true,
             GetSysColor(COLOR_WINDOWTEXT), rect.Right, rect.Bottom);
    end;
  end else begin
    FillRect(dc, rect,  0, GetSysColor(COLOR_BTNFACE));
    FillRect(dc, rect, -2, DecCol(GetSysColor(COLOR_BTNFACE), $101010, true));
    WrText(dc, rect.Left, rect.Top, GetCaption, 'Arial', 15, false, false,
           DecCol(GetSysColor(COLOR_BTNFACE), $141414, false), rect.Right, rect.Bottom);
  end;
end;

function TNVButton.GetCaption : string;
begin
  if FCaption = '' then
       result := FName
  else result := FCaption;
end;

procedure TNVButton.SetCaption(value: string);
begin
  FCaption := value;
end;

function TNVButton.GetMinWidth : integer;
begin
  result := FMinWidth;
end;

// ***************************************************************

procedure TNVLabel.SetProperty(prop, value: string);
begin
  if prop = 'Caption' then FCaption := value
  else                     inherited;
end;

procedure TNVLabel.Show(ix: integer; var iy: integer; fontDC: dword);
begin
  dec(iy, 2);
  FRect := CalcRect(fontDC);
  CreateWindow(ix, iy, 'Static', GetCaption, SS_LEFT, false);
  inherited;
end;

function TNVLabel.CalcRect(fontDC: dword) : TRect;
begin
  ZeroMemory(@result, sizeOf(result));
  result.Right := FParent.FWidth - 46;
  DrawText(fontDC, pchar(GetCaption), length(GetCaption), result, DT_CALCRECT or DT_WORDBREAK);
end;

function TNVLabel.IsHeadLabel(fontDC: dword) : boolean;
// a head label is a label which fulfills two rules:
// (1) the caption ends with ":" or "?"
// (2) the caption fits into one line
begin
  result := ((CopyR(GetCaption, 1) = ':') or (CopyR(GetCaption, 1) = '?')) and
            (CalcRect(fontDC).Bottom = GetTextExtent(fontDC, GetCaption).Y);
end;

function TNVLabel.GetCaption : string;
begin
  if FCaption = '' then
       result := FName
  else result := FCaption;
end;

procedure TNVLabel.SetCaption(value: string);
begin
  FCaption := value;
end;

// ***************************************************************

procedure TNVEdit.SetProperty(prop, value: string);
begin
  if      prop = 'Text'     then begin
                                   FText     := value;
                                   ReplaceText(FText, '%lf%', #$D#$A);
                                 end
  else if prop = 'Lines'    then FLines    := IntegerValue(value)
  else if prop = 'Optional' then FOptional := BooleanValue(value)
  else if prop = 'Colored'  then FNColored := not BooleanValue(value)
  else if prop = 'Valid'    then FNValid   := not BooleanValue(value)
  else                           inherited;
end;

procedure TNVEdit.Show(ix: integer; var iy: integer; fontDC: dword);
var style : dword;
begin
  FRect.Right  := FParent.FWidth - 46;
  FRect.Bottom := GetTextExtent(fontDC, 'Gg', 0, 13).Y;
  if FLines > 1 then
    FRect.Bottom := FRect.Bottom * FLines;
  inc(FRect.Bottom, 2);
  if FLines > 1 then
       style := WS_VSCROLL or ES_MULTILINE or ES_WANTRETURN or ES_AUTOVSCROLL
  else style := 0;
  CreateWindow(ix + 2, iy + 2, 'Edit', FText, style, true);
  InflateRect(FRect, 2, 2);
  inherited;
end;

procedure TNVEdit.FormPaint(dc: dword);
// we draw around the real edit control to make it look nicer
var r1 : TRect;
begin
  r1 := FRect;
  if GetFocus = FHandle then begin
    FillRect(dc, r1,  1, GetSysColor(COLOR_BTNSHADOW));
    FillRect(dc, r1.Left - 1, r1.Bottom,  r1.Right, r1.Bottom, GetSysColor(COLOR_WINDOW));
    FillRect(dc, r1.Right,    r1.Top - 1, r1.Right, r1.Bottom, GetSysColor(COLOR_WINDOW));
    FillRect(dc, r1,  0, GetSysColor(COLOR_BTNFACE));
    FillRect(dc, r1, -1, GetSysColor(COLOR_WINDOW));
  end else
    FillRect(dc, FRect, 0, $ffffff - ((($ffffff - GetSysColor(COLOR_BTNFACE)) and $fefefe) shr 1));
end;

function TNVEdit.MayEnableContinueButton : boolean;
// the continue button may only be enabled if all mandatory edits are filled
begin
  result := FOptional or ((not FNValid) and (GetText <> ''));
end;

function TNVEdit.GetLines    : integer; begin result := FLines    end;
function TNVEdit.GetOptional : boolean; begin result := FOptional end;

function TNVEdit.GetText : string;
begin
  if FHandle <> 0 then begin
    SetLength(FText, SendMessage(FHandle, WM_GETTEXTLENGTH, 0, 0) + 1);
    SetLength(FText, SendMessage(FHandle, WM_GETTEXT, Length(FText), integer(FText)));
  end;
  result := FText;
end;

procedure TNVEdit.SetText(value: string);
begin
  FText := value;
  if (value <> GetText) and (FHandle <> 0) then
    SendMessage(FHandle, WM_SETTEXT, 0, integer(value));
  if FParent <> nil then
    FParent.FireOnAction(FParent, nvaItemEvent, INVEdit(self));
end;

function TNVEdit.GetColored : boolean;
begin
  result := not FNColored;
end;

procedure TNVEdit.SetColored(value: boolean);
begin
  FNColored := not value;
  InvalidateRect(FHandle, nil, false);
end;

function TNVEdit.GetValid : boolean;
begin
  result := not FNValid;
end;

procedure TNVEdit.SetValid(value: boolean);
begin
  FNValid := not value;
  InvalidateRect(FHandle, nil, false);
  if FParent <> nil then
    FParent.CheckContinueButton;
end;

// ***************************************************************

procedure TNVCheckBox.SetProperty(prop, value: string);
begin
  if      prop = 'Caption' then FCaption := value
  else if prop = 'Text'    then FCaption := value
  else if prop = 'Checked' then FChecked := BooleanValue(value)
  else                          inherited;
end;

procedure TNVCheckBox.Show(ix: integer; var iy: integer; fontDC: dword);
begin
  PPoint(@FRect.Right)^ := GetTextExtent(fontDC, FCaption, 0, 13);
  inc(FRect.Right, GetSystemMetrics(SM_CXMENUCHECK) + 8);
  if GetSystemMetrics(SM_CYMENUCHECK) > FRect.Bottom then
    FRect.Bottom := GetSystemMetrics(SM_CXMENUCHECK);
  CreateWindow(ix, iy, 'Button', pchar(FCaption), BS_NOTIFY or BS_OWNERDRAW, true);
  if FChecked then
    SendMessage(FHandle, BM_SETCHECK, BST_CHECKED, 0);
  inherited;
end;

procedure TNVCheckBox.OwnerDraw(dc: dword; itemState: dword; rect: TRect);
// we completely draw the checkbox manually
// the win32 default checkbox simply doesn't fit into our window outfit
var r2 : TRect;
    c1 : dword;
    i1 : integer;
begin
  FillRect(dc, rect, 0, GetSysColor(COLOR_BTNFACE));
  r2 := rect;
  rect.Right := rect.Bottom;
  r2.Left := rect.Right + 4;
  dec(r2.Right, 2);
  FillRect(dc, rect, 0, $ffffff - ((($ffffff - GetSysColor(COLOR_BTNFACE)) and $fefefe) shr 1));
  if (itemState and ODS_SELECTED <> 0) or (itemState and ODS_DISABLED <> 0) then
       c1 := GetSysColor(COLOR_BTNFACE)
  else c1 := GetSysColor(COLOR_WINDOW );
  FillRect(dc, rect, -2, c1);
  if itemState and ODS_DISABLED <> 0 then
       c1 := GetSysColor(COLOR_BTNSHADOW)
  else c1 := GetSysColor(COLOR_WINDOWTEXT);
  if FChecked then begin
    i1 := rect.Top;
    if OS.winNtEnum >= osWinXP then
      dec(i1);
    WrText(dc, rect.Left, i1, 'a', 'Marlett', 13, false, false, c1, rect.Right - rect.Left, rect.Bottom - rect.Top);
  end;
  if itemState and ODS_DISABLED <> 0 then
    WrText(dc, r2.Left + 2, r2.Top + 1, pchar(FCaption), 'Tahoma', 11, false, false, GetSysColor(COLOR_WINDOW));
  WrText(dc, r2.Left + 1, r2.Top, pchar(FCaption), 'Tahoma', 11, false, false, c1);
  if itemState and ODS_FOCUS <> 0 then
    DrawFocusRect(dc, r2);
end;

function  TNVCheckBox.GetCaption : string;        begin result := FCaption end;
procedure TNVCheckBox.SetCaption (value: string); begin FCaption := value  end;
function  TNVCheckBox.GetChecked : boolean;       begin result := FChecked end;

procedure TNVCheckBox.SetChecked(value: boolean);
begin
  FChecked := value;
  if FHandle <> 0 then
    InvalidateRect(FHandle, nil, true);
end;

// ***************************************************************

procedure TNVImage.SetProperty(prop, value: string);
begin
  if      prop = 'Width'     then FRect.Right  := IntegerValue(value)
  else if prop = 'Height'    then FRect.Bottom := IntegerValue(value)
  else if prop = 'File'      then FFile        := value
  else if prop = 'Clickable' then FClickable   := BooleanValue(value)
  else if prop = 'Border'    then FBorder      := BooleanValue(value)
  else                            inherited;
end;

procedure TNVImage.Show(ix: integer; var iy: integer; fontDC: dword);
// the image control has multiple size options
// as a result the calculation of the final dimensions is a bit complicated
// the image is the only control which has no own window handle
var i1 : integer;
begin
  i1 := 0;
  if FClickable then inc(i1, 6);
  if FBorder    then inc(i1, 2);
  if (FBitmap = nil) and (FFile <> '') then
    FBitmap := LoadBitmap(FFile);
  // if no image dimensions were set we simply use the size of the bitmap
  if FRect.Right = 0 then
    if FBitmap <> nil then
         FRect.Right := FBitmap.Width
    else FRect.Right := 32;
  if FRect.Bottom = 0 then
    if FBitmap <> nil then
         FRect.Bottom := FBitmap.Height
    else FRect.Bottom := 32;
  // however, there are maximum dimensions which we won't supercede
  if FRect.Right > FParent.FWidth - 42 - i1 then
    FRect.Right := FParent.FWidth - 42 - i1;
  if FRect.Bottom > GetSystemMetrics(SM_CYSCREEN) div 3 then
    FRect.Bottom := GetSystemMetrics(SM_CYSCREEN) div 3;
  // are the final image dimensions different than the original bitmap size?
  if (FBitmap <> nil) and ((FRect.Right <> FBitmap.Width) or (FRect.Bottom <> FBitmap.Height)) then begin
    // yes, they are - so we have to zoom the bitmap
    FBitmap := FBitmap.Zoom(FRect.Right, FRect.Bottom);
    if FBitmap <> nil then begin
      FRect.Right  := FBitmap.Width;
      FRect.Bottom := FBitmap.Height;
    end;
  end;
  inc(FRect.Right,  i1);
  inc(FRect.Bottom, i1);
  OffsetRect(FRect, ix, iy);
  inherited;
end;

procedure TNVImage.FormPaint(dc: dword);
// we paint the bitmap manually onto the form dc in order to reduce flickering
var r1 : TRect;
begin
  r1 := FRect;
  if FClickable then begin
    LineRect(dc, r1, 0, 2, PS_SOLID, 0, $ffffff - ((($ffffff - GetSysColor(COLOR_BTNFACE)) and $fefefe) shr 1));
    InflateRect(r1, -3, -3);
  end;
  if FBitmap <> nil then begin
    if FBorder then begin
      LineRect(dc, r1, 0, 1, PS_SOLID, 0, GetSysColor(COLOR_WINDOWTEXT));
      InflateRect(r1, -1, -1);
    end;
    FillRect(dc, r1.Left + FBitmap.Width, r1.Top, r1.Right - 1, r1.Top + FBitmap.Height - 1, GetSysColor(COLOR_BTNFACE));
    FillRect(dc, r1.Left, r1.Top + FBitmap.Height, r1.Right - 1, r1.Bottom - 1, GetSysColor(COLOR_BTNFACE));
    FBitmap.Draw(dc, r1.Left, r1.Top);
  end else
    FillRect(dc, r1, 0, GetSysColor(COLOR_BTNFACE));
end;

function TNVImage.GetFile      : string;    begin result := FFile                     end;
function TNVImage.GetBitmap    : INVBitmap; begin result := FBitmap                   end;
function TNVImage.GetClickable : boolean;   begin result := FClickable                end;
function TNVImage.GetBorder    : boolean;   begin result := FBorder                   end;
function TNVImage.GetWidth     : integer;   begin result := FRect.Right  - FRect.Left end;
function TNVImage.GetHeight    : integer;   begin result := FRect.Bottom - FRect.Top  end;

procedure TNVImage.SetBitmap(value: INVBitmap);
var r1 : TRect;
    i1 : integer;
begin
  if FRect.Left > 0 then begin
    if value <> nil then begin
      r1 := FRect;
      OffsetRect(r1, -r1.Left, -r1.Top);
      i1 := 0;
      if FClickable then inc(i1, 6);
      if FBorder    then inc(i1, 2);
      dec(r1.Right,  i1);
      dec(r1.Bottom, i1);
      if (value.Width > r1.Right) or (value.Height > r1.Bottom) then
        FBitmap := value.Zoom(r1.Right, r1.Bottom)
      else
        FBitmap := value;
    end else
      FBitmap := nil;
    if FParent <> nil then
      InvalidateRect(FParent.Handle, @FRect, false);
  end else
    FBitmap := value;
end;

// ***************************************************************

constructor TNVForm.Create(parent: TNVAssistant; title, dfm: string;
                           exception: IUnknown; currentStep, maxStep: integer);

  function AddItem(item: INVItemEx) : INVItemEx; overload;
  // add a new item to our list of items
  var i1 : integer;
  begin
    // first we add the item into the item array
    i1 := Length(FItems);
    SetLength(FItems, i1 + 1);
    FItems[i1] := item;
    // then we set up cross links to the neighbour items
    // we need this later for tab simulating
    if i1 > 0 then begin
      FItems[i1].GetTNVItem.FNext[true ] := FItems[0     ]; FItems[0     ].GetTNVItem.FNext[false] := item;
      FItems[i1].GetTNVItem.FNext[false] := FItems[i1 - 1]; FItems[i1 - 1].GetTNVItem.FNext[true ] := item;
    end;
    item.GetTNVItem.FIndex := i1;
    result := item;
  end;

  function ConvertObject(var index: integer) : boolean;

    function ConvertProperty(const item: INVItemEx; var index: integer) : boolean;

      procedure ReadProperty(const item: INVItemEx; prop, val: string);
      var fi : TFunctionInfo;
          s1 : string;
      begin
        if item = nil then begin
          // this is a property of the dialog itself
          if      prop = 'MinWidth'      then MinWidth       := IntegerValue(val)
          else if prop = 'Message'       then FMessage       := val
          else if prop = 'ActiveControl' then FActiveControl := val
          else if prop = 'Timer'         then FTimer         := dword(IntegerValue(val))
          else if prop = 'OnAction'      then begin
            if      IsTextEqual(val, 'madExcept.HandleContactForm'   ) then FOnAction := HandleContactFormProc
            else if IsTextEqual(val, 'madExcept.HandleScreenshotForm') then FOnAction := HandleScreenshotFormProc
            else begin
              s1 := SubStr(val, 2, '.');
              if s1 = '' then begin
                s1 := val;
                val := '';
              end else
                val := SubStr(val, 1, '.');
              FOnAction := pointer(FindMapFile.FindPublic(true, val, s1));
            end;
            if @FOnAction <> nil then begin
              fi := ParseFunction(@FOnAction);
              if (not fi.IsValid) or
                 (TPCardinal(dword(fi.CodeBegin) + dword(fi.CodeLen) - 4)^ and $ffffff00 <> $0004c200) then
                // this function does not end with "ret $0008"
                // so the parameter list of the function is most likely incorrect
                FOnAction := nil;
            end;
          end;
        end else
          // this is a property of the currently described control
          item.GetTNVItem.SetProperty(prop, val);
      end;

    var s1, s2 : string;
    begin
      result := true;
      SetString(s1, pchar(dfm) + index, ord(dfm[index]));
      inc(index, ord(dfm[index]) + 2);
      case dfm[index - 1] of
        #$2      : begin
                     ReadProperty(item, s1, IntToStrEx(TPShortInt(@dfm[index])^));
                     inc(index, 1);
                   end;
        #$3      : begin
                     ReadProperty(item, s1, IntToStrEx(TPSmallInt(@dfm[index])^));
                     inc(index, 2);
                   end;
        #$4      : begin
                     ReadProperty(item, s1, IntToStrEx(TPInteger(@dfm[index])^));
                     inc(index, 4);
                   end;
        #$6      : begin
                     SetString(s2, pchar(dfm) + index, TPByte(@dfm[index])^);
                     ReadProperty(item, s1, s2);
                     inc(index, 1 + TPByte(@dfm[index])^);
                   end;
        #$7      : begin
                     SetString(s2, pchar(dfm) + index, ord(dfm[index]));
                     ReadProperty(item, s1, s2);
                     inc(index, 1 + ord(dfm[index]));
                   end;
        #$8, #$9 : ReadProperty(item, s1, IntToStrEx(ord(dfm[index - 1] = #$9)));
        #$c      : begin
                     SetString(s2, pchar(dfm) + index + 3, TPInteger(@dfm[index])^);
                     ReadProperty(item, s1, s2);
                     inc(index, 4 + TPInteger(@dfm[index])^);
                   end;
        else       begin
                     MessageBox(0, pchar('Internal error: Invalid assistant resource (' + IntToHexEx(integer(dfm[index - 1])) + ').'), 'madExcept', 0);
                     result := false;
                   end;
      end;
    end;

  var typ, name : string;
      item      : INVItemEx;
  begin
    SetString(typ, pchar(dfm) + index, ord(dfm[index]));
    inc(index, ord(dfm[index]) + 1);
    SetString(name, pchar(dfm) + index, ord(dfm[index]));
    if name = 'AbortBtn' then
      name := 'CancelBtn';
    inc(index, ord(dfm[index]) + 1);
    item := nil;
    typ[1] := 'T';
    if typ = TNVButton.ClassName then begin
      // special case for buttons (because of the 3 main buttons)
      if      name = FItems[0].Name then item := FItems[0]
      else if name = FItems[1].Name then item := FItems[1]
      else if name = FItems[2].Name then item := FItems[2]
      else                               item := AddItem(TNVButton.Create(self, name, ''));
    end
      // all other classes are handled here
      else if typ = TNVLabel   .ClassName then item := AddItem(TNVLabel   .Create(self, name))
      else if typ = TNVEdit    .ClassName then item := AddItem(TNVEdit    .Create(self, name))
      else if typ = TNVCheckBox.ClassName then item := AddItem(TNVCheckBox.Create(self, name))
      else if typ = TNVImage   .ClassName then item := AddItem(TNVImage   .Create(self, name));
    if (FName = '') and (item = nil) then
      FName := name;
    result := true;
    while result and (dfm[index] <> #0) do
      result := ConvertProperty(item, index);
    inc(index);
    while result and (dfm[index] <> #0) do
      result := ConvertObject(index);
    inc(index);
  end;

var index : integer;
begin
  inherited Create;
  FParent    := parent;
  FTitle     := title;
  FException := exception;
  FStep      := currentStep;
  FSteps     := maxStep;
  // create our main buttons - they are always there!
  AddItem(TNVButton.Create(self, 'ContinueBtn', 'Next >'));
  AddItem(TNVButton.Create(self, 'SkipBtn',     'Skip'  ));
  AddItem(TNVButton.Create(self, 'CancelBtn',   'Cancel' ));
  // parse the form string
  index := 5;
  if PosStrIs1('TPF0', dfm) then
    ConvertObject(index);
  inc(FRefCount);
  // notify the action event handler
  FireOnAction(self, nvaFormCreate, nil);
  dec(FRefCount);
end;

destructor TNVForm.Destroy;
var i1 : integer;
begin
  DeleteObject(FFont);
  for i1 := 0 to high(FItems) do begin
    FItems[i1].GetTNVItem.FNext[true ] := nil;
    FItems[i1].GetTNVItem.FNext[false] := nil;
  end;
  inherited;
end;

function TNVForm.DialogWndMethod(window, msg: dword; wParam, lParam: integer) : integer; stdcall;

  function FindImagePos(p1: TPoint) : INVItemEx;
  var i1 : integer;
  begin
    result := nil;
    for i1 := 0 to high(FItems) do
      if FItems[i1].GetTNVItem is TNVImage then
        with TNVImage(FItems[i1].GetTNVItem) do
          if FClickable and
             (p1.X > FRect.Left) and (p1.X <= FRect.Right ) and
             (p1.Y > FRect.Top ) and (p1.Y <= FRect.Bottom) then begin
            result := FItems[i1];
            break;
          end;
  end;

var dc     : dword;
    r1, r2 : TRect;
    c1     : dword;
    i1, i2 : integer;
    ps     : TPaintStruct;
    item   : INVItemEx;
    p1     : TPoint;
    check  : INVCheckBox;
begin
  case msg of
    WM_SETCURSOR  : begin
                      GetCursorPos(p1);
                      ScreenToClient(Handle, p1);
                      if (FindImagePos(p1) <> nil) and IsWindowEnabled(Handle) then
                           c1 := LoadCursor(0, IDC_HAND)
                      else c1 := 0;
                      if c1 <> 0 then begin
                        SetCursor(c1);
                        result := 1;
                      end else
                        result := DefWindowProc(window, msg, wParam, lParam);
                    end;
    WM_LBUTTONDOWN : begin
                      p1.X := word(lParam);
                      p1.Y := dword(lParam) shr 16;
                      item := FindImagePos(p1);
                      if item <> nil then begin
                        FCap := item;
                        SetCapture(Handle);
                      end;
                      result := 0;
                    end;
    WM_LBUTTONUP  : begin
                      if FCap <> nil then begin
                        ReleaseCapture;
                        p1.X := word(lParam);
                        p1.Y := dword(lParam) shr 16;
                        if FCap = FindImagePos(p1) then
                          FireOnAction(self, nvaItemEvent, FCap);
                        FCap := nil;
                      end;
                      result := 0;
                    end;
    WM_ERASEBKGND : begin
                      dc := dword(wParam);
                      FillRect(dc, 0,  0, FWidth, FHeight, DecCol(GetSysColor(COLOR_BTNFACE), $080808, true));
                      FillRect(dc, 8, 25, FWidth - 9, (GetContinueButton as INVItemEx).GetTNVItem.FRect.Top - 5 - 5, GetSysColor(COLOR_BTNFACE));
                      LineRect(dc, 8, 25, FWidth - 9, (GetContinueButton as INVItemEx).GetTNVItem.FRect.Top - 5 - 5, 2, PS_DOT, GetSysColor(COLOR_BTNFACE), DecCol(GetSysColor(COLOR_BTNFACE), $101010, false));
                      WrText(dc, 11, 6, FMessage, 'Arial', 15, true, true, GetSysColor(COLOR_3DDKSHADOW));
                      i1 := FWidth - 29 - (FSteps - FStep) * 19;
                      FillRect(dc, i1,     2, i1 + 20, 22, GetSysColor(COLOR_BTNSHADOW));
                      FillRect(dc, i1 + 2, 4, i1 + 18, 07, GetSysColor(COLOR_WINDOW   ));
                      for i2 := 7 to 19 do
                        FillRect(dc, i1 + 2, i2, i1 + 18, i2 + 1, GetSysColor(COLOR_WINDOW) - $020202 * (dword(i2) - 7));
                      i2 := FWidth - 10 + 7 - 19 * FSteps;
                      for i1 := 1 to FSteps do begin
                        if i1 = FStep then
                             WrText(dc, i2, 4, IntToStrEx(i1), 'Arial', 15, true, true,  GetSysColor(COLOR_WINDOWTEXT))
                        else WrText(dc, i2, 4, IntToStrEx(i1), 'Arial', 15, true, false, GetSysColor(COLOR_BTNSHADOW ));
                        inc(i2, 19);
                      end;
                      result := 0;
                    end;
    WM_PAINT      : begin
                      BeginPaint(Handle, ps);
                      for i1 := 0 to high(FItems) do
                        FItems[i1].GetTNVItem.FormPaint(ps.hdc);
                      EndPaint(Handle, ps);
                      result := 0;
                    end;
    WM_CTLCOLOREDIT : begin
                      dc := dword(wParam);
                      item := FindHandle(dword(lParam));
                      if (item <> nil) and (item.GetTNVItem is TNVEdit) then begin
                        with TNVEdit(item.GetTNVItem) do
                          if not FNColored then begin
                            if not FNValid then
                                 SetTextColor(dc, $008000)
                            else SetTextColor(dc, $000080);
                          end else
                            SetTextColor(dc, $000000);
                      end else
                        SetTextColor(dc, $008000);
                      result := GetSysColorBrush(COLOR_WINDOW);
                    end;
    WM_DRAWITEM   : with PDrawItemStruct(lParam)^ do begin
                      item := FindHandle(hwndItem);
                      if item <> nil then begin
                        GetClientRect(hwndItem, r1);
                        item.GetTNVItem.OwnerDraw(hDC, itemState, r1);
                      end;
                      result := 1;
                    end;
    WM_ACTIVATE   : begin
                      if wParam and $ffff = 0 then
                        LastFocus := GetFocus
                      else
                        if LastFocus <> 0 then
                          SetFocus(LastFocus);
                      result := 0;
                    end;
    WM_TIMER      : begin
                      if wParam = 777 then
                        FireOnAction(self, nvaItemEvent, nil);
                      result := 0;
                    end;
    WM_CLOSE      : begin
                      if FTimer <> 0 then begin
                        KillTimer(Handle, 777);
                        FTimer := 0;
                      end;
                      FModalResult := nvmCancel;
                      FireOnAction(self, nvaFormClose, nil);
                      result := DefWindowProc(window, msg, wParam, lParam);
                    end;
    WM_COMMAND    : begin
                      result := DefWindowProc(window, msg, wParam, lParam);
                      if (dword(wParam) shr 16 = EN_SETFOCUS) or (dword(wParam) shr 16 = EN_KILLFOCUS) then begin
                        GetWindowRect(dword(lParam), r1);
                        ScreenToClient(Handle, PPoint(@r1.Left )^);
                        ScreenToClient(Handle, PPoint(@r1.Right)^);
                        InflateRect(r1, 3, 3);
                        r2 := r1; r2.Bottom := r2.Top    + 3; InvalidateRect(Handle, @r2, true);
                        r2 := r1; r2.Right  := r2.Left   + 3; InvalidateRect(Handle, @r2, true);
                        r2 := r1; r2.Left   := r2.Right  - 3; InvalidateRect(Handle, @r2, true);
                        r2 := r1; r2.Top    := r2.Bottom - 3; InvalidateRect(Handle, @r2, true);
                      end;
                      if dword(wParam) shr 16 = BN_SETFOCUS then
                        PostMessage(dword(lParam), BM_SETSTYLE, GetWindowLong(dword(lParam), GWL_STYLE) or BS_DEFPUSHBUTTON, 1);
                      if dword(wParam) shr 16 = EN_CHANGE then begin
                        FireOnAction(self, nvaItemEvent, FindHandle(dword(lParam)));
                        CheckContinueButton;
                      end;
                      if ((dword(wParam) shr 16 = BN_CLICKED) and (lParam <> 0)) or (wParam = 1) then begin
                        if lParam = 0 then begin
                          item := FindHandle(GetFocus);
                          if item.GetTNVItem.FIndex > 2 then
                            item := nil;
                        end else
                          item := FindHandle(dword(lParam));
                        if item <> nil then begin
                          if item.QueryInterface(INVCheckBox, check) = 0 then
                            check.Checked := not check.Checked;
                          FireOnAction(self, nvaItemEvent, item);
                          if item.GetTNVItem.FIndex <= 2 then begin
                            if FTimer <> 0 then begin
                              KillTimer(Handle, 777);
                              FTimer := 0;
                            end;
                            case item.GetTNVItem.FIndex of
                              0 : FModalResult := nvmOk;
                              1 : FModalResult := nvmSkip;
                              2 : FModalResult := nvmCancel;
                            end;
                            if FModalResult = nvmOk then
                              for i1 := 0 to high(FItems) do
                                if FItems[i1].GetTNVItem is TNVEdit then
                                  with TNVEdit(FItems[i1].GetTNVItem) do begin
                                    FText := GetText;
                                    FHandle := 0;
                                  end;
                            FireOnAction(self, nvaFormClose, nil);
                            DestroyWindow(Handle);
                          end;
                        end;
                      end;
                    end;
    else result := DefWindowProc(window, msg, wParam, lParam);
  end;
end;

procedure TNVForm.CheckContinueButton;
// check whether the continue button may be enabled or not
// it may be enabled only if all controls agree
var i1 : integer;
    b1 : boolean;
begin
  b1 := false;
  for i1 := 3 to high(FItems) do
    if not FItems[i1].GetTNVItem.MayEnableContinueButton then begin
      b1 := true;
      break;
    end;
  (GetContinueButton as INVItemEx).GetTNVItem.SetForceDisabled(b1);
end;

function TNVForm.FindClass(item: string; guid: TGuid) : INVItem;
// find an item which has a specific name and supports a specific interface
var i1 : integer;
begin
  for i1 := 0 to high(FItems) do
    if (FItems[i1].Name = item) and (FItems[i1].QueryInterface(guid, result) = 0) then
      exit;
  result := nil;
end;

function TNVForm.FindHandle(handle: dword) : INVItemEx;
// find an item which has a specific window handle
var i1 : integer;
begin
  result := nil;
  if handle <> 0 then
    for i1 := 0 to high(FItems) do
      if FItems[i1].Handle = handle then begin
        result := FItems[i1];
        break;
      end;
end;

procedure TNVForm.FocusNext(item: INVItemEx; down: boolean);
// focus the next or previous item
var item2 : INVItemEx;
begin
  if item <> nil then begin
    item2 := item;
    repeat
      // loop until we find an item which can get the focus
      // or until the loop is run through without success
      item2 := item2.GetTNVItem.FNext[down];
    until (item2.Enabled and item2.TabStop and IsWindowEnabled(item2.Handle)) or (item2 = item);
    if item2 <> item then
      // we found a focusable item, so let's focus it
      item2.SetFocus;
  end;
end;

procedure TNVForm.FireOnAction(form: INVForm; action: TNVAction; item: INVItem);
begin
  if @FOnAction <> nil then
    FOnAction(form, action, item, FException);
  if (FParent <> nil) and (@FParent.FOnAction <> nil) then
    FParent.FOnAction(form, action, item, FParent.FException);
end;

function  TNVForm.GetAssistant           : INVAssistant;      begin result := FParent                end;
function  TNVForm.GetName                : string;            begin result := FName                  end;
function  TNVForm.GetTitle               : string;            begin result := FTitle                 end;
procedure TNVForm.SetTitle         (value: string);           begin FTitle := value                  end;
function  TNVForm.GetMessage_            : string;            begin result := FMessage               end;
procedure TNVForm.SetMessage       (value: string);           begin FMessage := value                end;
function  TNVForm.GetItemCount           : integer;           begin result := Length(FItems)         end;
function  TNVForm.GetContinueButton      : INVButton;         begin result := FItems[0] as INVButton end;
function  TNVForm.GetSkipButton          : INVButton;         begin result := FItems[1] as INVButton end;
function  TNVForm.GetCancelButton        : INVButton;         begin result := FItems[2] as INVButton end;
function  TNVForm.GetActiveControl       : string;            begin result := FActiveControl         end;
procedure TNVForm.SetActiveControl (value: string);           begin FActiveControl := value          end;
function  TNVForm.GetTimer               : dword;             begin result := FTimer                 end;
function  TNVForm.GetHandle              : dword;             begin result := Handle                 end;
function  TNVForm.GetModalResult         : TNVModalResult;    begin result := FModalResult           end;
function  TNVForm.GetOnAction            : TNVActionHandler;  begin result := FOnAction              end;
procedure TNVForm.SetOnAction      (value: TNVActionHandler); begin FOnAction := value               end;

function TNVForm.GetItem(index: integer) : INVItem;
begin
  if (index >= 0) and (index < Length(FItems)) then
       result := FItems[index]
  else result := nil;
end;

function TNVForm.nvEdit    (item: string) : INVEdit;     begin result := INVEdit    (FindClass(item, INVEdit    )) end;
function TNVForm.nvLabel   (item: string) : INVLabel;    begin result := INVLabel   (FindClass(item, INVLabel   )) end;
function TNVForm.nvButton  (item: string) : INVButton;   begin result := INVButton  (FindClass(item, INVButton  )) end;
function TNVForm.nvCheckBox(item: string) : INVCheckBox; begin result := INVCheckBox(FindClass(item, INVCheckBox)) end;
function TNVForm.nvImage   (item: string) : INVImage;    begin result := INVImage   (FindClass(item, INVImage   )) end;

procedure TNVForm.Skip;
begin
  FSkip := true;
end;

function TNVForm.ShowModal(parentWindow: dword) : TNVModalResult;
var fontDC, oldFont : dword;
    wndProc         : pointer;

  procedure CreateMainWindow;
  var wndClass : TWndClass;
      c1       : dword;
  begin
    // register the window class
    ZeroMemory(@wndClass, sizeOf(TWndClass));
    with wndClass do begin
      style         := CS_CLASSDC or CS_PARENTDC;
      lpfnWndProc   := @DefWindowProc;
      hInstance     := SysInit.HInstance;
      hbrBackground := COLOR_BTNFACE + 1;
      lpszClassname := 'madNVAssistantWnd';
      hCursor       := LoadCursor(0, IDC_ARROW);
    end;
    RegisterClass(wndClass);
    // in NT4 you sometimes have to give exactly the same *pointer*
    // which you used in RegisterClass, the same *string* sometimes fails
    Handle := CreateWindowEx(WS_EX_DLGMODALFRAME, wndClass.lpszClassname, pchar(FTitle),
                             WS_CAPTION or WS_SYSMENU, 0, 0, 200, 200, parentWindow, 0, HInstance, nil);
    // now we install our special class window method
    wndProc := MethodToProcedure(self, @TNVForm.DialogWndMethod);
    SetWindowLong(Handle, GWL_WNDPROC, integer(wndProc));
    // finally we remove the "minimize" etc options from the window's sys menu
    c1 := GetSystemMenu(Handle, false);
    RemoveMenu(c1, SC_MINIMIZE, MF_BYCOMMAND);
    RemoveMenu(c1, SC_MAXIMIZE, MF_BYCOMMAND);
    RemoveMenu(c1, SC_RESTORE,  MF_BYCOMMAND);
    RemoveMenu(c1, SC_SIZE,     MF_BYCOMMAND);
  end;

  procedure SelectFont(font: string; size: integer; bold: boolean);
  begin
    if FFont <> 0 then begin
      // free the old font
      SelectObject(fontDC, oldFont);
      DeleteObject(FFont);
    end;
    // create the new font and select it into the font dc
    FFont   := GetFont(font, size, bold);
    oldFont := SelectObject(fontDC, FFont);
  end;

  procedure PrepareMainButton(button: INVButton);
  // size calculations
  begin
    with TNVButton((button as INVItemEx).GetTNVItem) do begin
      if FNoOwnerDraw then
           FRect.Bottom := 23
      else FRect.Bottom := 32;
      if FNoOwnerDraw then begin
        SelectFont('Tahoma', 11, false);
        FRect.Right := GetTextExtent(fontDC, GetCaption).X + 20;
        if FRect.Right < 75 then FRect.Right := 75;
      end else begin
        SelectFont('Arial', 15, false);
        FRect.Right := GetTextExtent(fontDC, GetCaption).X + 20;
        if FRect.Right < 85 then FRect.Right := 85;
      end;
      if FInvisible then
        FRect.Right := FRect.Left - 8;
    end;
  end;

  procedure CreateMainButton(button: INVButton; left: integer);
  var c1 : dword;
  begin
    with TNVButton((button as INVItemEx).GetTNVItem) do begin
      if FNoOwnerDraw then
           c1 := 0
      else c1 := BS_OWNERDRAW;
      if not FInvisible then
        c1 := c1 or WS_VISIBLE;
      CreateWindow(left, FHeight - FRect.Bottom - 9, 'Button', GetCaption, BS_NOTIFY or c1, true);
      SendMessage(FHandle, WM_SETFONT, integer(FParent.FFont), 0);
    end;
  end;

var msg    : TMsg;
    i1, i2 : integer;
    iy     : integer;
    r1     : TRect;
    item   : INVItem;
    enableParent : boolean;
begin
  if FSkip then begin
    result := nvmSkip;
    exit;
  end;
  enableParent := (parentWindow <> 0) and IsWindowEnabled(parentWindow);
  if enableParent then
    EnableWindow(parentWindow, false);
  // first let's create the main window
  CreateMainWindow;
  // now we create a font dc for text size calculations
  fontDC := CreateCompatibleDC(0);
  // next we prepare our 3 main buttons
  PrepareMainButton(GetContinueButton);
  PrepareMainButton(    GetSkipButton);
  PrepareMainButton(  GetCancelButton);
  // now we calculate the width of the whole dialog
  SelectFont('Arial', 15, true);
  i1 := 11 + GetTextExtent(fontDC, FMessage).X + 16 + FSteps * 19 + 9;
  FWidth := (GetContinueButton as INVItemEx).GetTNVItem.FRect.Right +
            (    GetSkipButton as INVItemEx).GetTNVItem.FRect.Right +
            (  GetCancelButton as INVItemEx).GetTNVItem.FRect.Right + 32;
  if FWidth < 315      then FWidth := 315;
  if FWidth < MinWidth then FWidth := MinWidth;
  if FWidth < i1       then FWidth := i1;
  // then we add in all controls and calculate the height of the dialog
  SelectFont('Tahoma', 11, false);
  iy := 40;
  for i1 := 3 to high(FItems) do
    FItems[i1].GetTNVItem.Show(21, iy, fontDC);
  FHeight := iy + (GetContinueButton as INVItemEx).GetTNVItem.FRect.Bottom + 33;
  // we don't need our font dc anymore
  SelectObject(fontDC, oldFont);
  DeleteDC(fontDC);
  // now we can create our main buttons at the correct position
  if TNVButton((GetContinueButton as INVItemEx).GetTNVItem).FNoOwnerDraw and
     TNVButton((    GetSkipButton as INVItemEx).GetTNVItem).FInvisible   and
     TNVButton((  GetCancelButton as INVItemEx).GetTNVItem).FNoOwnerDraw then begin
    CreateMainButton(  GetCancelButton, FWidth - 8 - (GetCancelButton as INVItemEx).GetTNVItem.FRect.Right);
    CreateMainButton(GetContinueButton, (  GetCancelButton as INVItemEx).GetTNVItem.FRect.Left  -
                                        (GetContinueButton as INVItemEx).GetTNVItem.FRect.Right - 8);
    CreateMainButton(    GetSkipButton, 8);
  end else begin
    CreateMainButton(GetContinueButton, 8);
    CreateMainButton(    GetSkipButton, (GetContinueButton as INVItemEx).GetTNVItem.FRect.Right + 8);
    CreateMainButton(  GetCancelButton, FWidth - 8 - (GetCancelButton as INVItemEx).GetTNVItem.FRect.Right);
  end;
  CheckContinueButton;
  // which control shall we focus?
  if FActiveControl <> '' then
       item := FindClass(FActiveControl, INVItem)
  else item := nil;
  if item <> nil then
       item.SetFocus
  else FocusNext(GetCancelButton as INVItemEx, true);
  // next we size and center the main dialog
  GetWindowRect(Handle, r1);
  i1 := r1.Right  - r1.Left;
  i2 := r1.Bottom - r1.Top;
  GetClientRect(Handle, r1);
  i1 := i1 - r1.Right;
  i2 := i2 - r1.Bottom;
  SetWindowPos(Handle, 0, (GetSystemMetrics(SM_CXScreen) - FWidth  - i1) div 2,
                          (GetSystemMetrics(SM_CYScreen) - FHeight - i2) div 2,
               FWidth + i1, FHeight + i2, SWP_NOZORDER);
  // notify the action event handler
  FireOnAction(self, nvaFormShow, nil);
  if FSkip then begin
    result := nvmSkip;
    exit;
  end;
  // all preparation is done, we can now show the dialog
  ShowWindow(Handle, SW_SHOWNORMAL);
  if IsIconic(Handle) then
    ShowWindow(Handle, SW_RESTORE);
  BringWindowToTop(Handle);
  SetForegroundWindow(Handle);
  if FTimer <> 0 then
    SetTimer(Handle, 777, FTimer, nil);
  // here comes the dialog message loop
  while IsWindow(Handle) and (dword(integer(GetMessage(msg, 0, 0, 0)) + 1) > 1) do begin
    if (msg.message = WM_KEYDOWN) and (msg.wParam = VK_ESCAPE) and GetCancelButton.Enabled then begin
      FModalResult := nvmCancel;
      msg.hwnd     := Handle;
      msg.message  := WM_CLOSE;
      msg.wParam   := 0;
      msg.lParam   := 0;
    end;
    if (msg.message = WM_CHAR) and (msg.wParam = 9) then
      // someone pressed tab
      // we need to handle this manually, because the memo box doesn't do it
      FocusNext(FindHandle(msg.hwnd), GetKeyState(VK_SHIFT) and $8000 = 0)
    else
      if (msg.message = WM_KEYDOWN) and (msg.wParam = VK_RETURN) and
         (FindHandle(msg.hwnd) <> nil) and
         (not (FindHandle(msg.hwnd).GetTNVItem is TNVButton)) and
         ( (not (FindHandle(msg.hwnd).GetTNVItem is TNVEdit)) or
           (TNVEdit(FindHandle(msg.hwnd).GetTNVItem).FLines <= 1) ) then
        // in most cases we do "enter = tab", except on buttons and memos
        FocusNext(FindHandle(msg.hwnd), true)
      else
        // any other message goes the default way
        if not IsDialogMessage(Handle, msg) then begin
          TranslateMessage(msg);
          DispatchMessage(msg);
        end;
  end;
  VirtualFree(wndProc, 0, MEM_RELEASE);
  result := FModalResult;
  if (parentWindow <> 0) and enableParent then begin
    EnableWindow(parentWindow, true);
    SetActiveWindow(parentWindow);
  end;
end;

function  TNVForm.GetException       : IUnknown;  begin result     := FException end;
procedure TNVForm.SetException (value: IUnknown); begin FException := value      end;
function  TNVForm.GetData            : pointer;   begin result     := FData      end;
procedure TNVForm.SetData      (value: pointer);  begin FData      := value      end;

// ***************************************************************

constructor TNVAssistant.Create(title: string; const dfms: array of string; exception: IUnknown);
var i1 : integer;
begin
  inherited Create;
  FTitle     := title;
  FException := exception;
  SetLength(FForms, Length(dfms));
  for i1 := 0 to high(FForms) do
    FForms[i1] := TNVForm.Create(self, title, dfms[i1], exception, i1 + 1, Length(FForms));
end;

function TNVAssistant.GetTitle : string;
begin
  result := FTitle;
end;

procedure TNVAssistant.SetTitle(value: string);
var i1 : integer;
begin
  FTitle := value;
  for i1 := 0 to high(FForms) do
    FForms[i1].SetTitle(value);
end;

function TNVAssistant.GetFormCount : integer;
begin
  result := length(FForms);
end;

function TNVAssistant.GetForms(index: integer) : INVForm;
begin
  if (index >= 0) and (index < length(FForms)) then
       result := FForms[index]
  else result := nil;
end;

function TNVAssistant.GetForm(name: string) : INVForm;
var i1 : integer;
begin
  result := nil;
  for i1 := 0 to high(FForms) do
    if IsTextEqual(FForms[i1].Name, name) then
      result := FForms[i1];
end;

function  TNVAssistant.GetModalResult    : TNVModalResult;    begin result := FModalResult end;
function  TNVAssistant.GetOnAction       : TNVActionHandler;  begin result := FOnAction    end;
procedure TNVAssistant.SetOnAction (value: TNVActionHandler); begin FOnAction := value     end;

function TNVAssistant.ShowModal(parentWindow: dword = 0) : TNVModalResult;
var i1 : integer;
begin
  if parentWindow <> 0 then
    EnableWindow(parentWindow, false);
  result := nvmOk;
  for i1 := 0 to high(FForms) do
    if FForms[i1].ShowModal(parentWindow) = nvmCancel then begin
      result := nvmCancel;
      break;
    end;
  if parentWindow <> 0 then
    EnableWindow(parentWindow, true);
  FModalResult := result;
end;

function  TNVAssistant.GetException       : IUnknown;  begin result     := FException end;
procedure TNVAssistant.SetException (value: IUnknown); begin FException := value      end;
function  TNVAssistant.GetData            : pointer;   begin result     := FData      end;
procedure TNVAssistant.SetData      (value: pointer);  begin FData      := value      end;

function CreateAssistant(title: string; const dfms: array of string; exception: IUnknown) : INVAssistant;
begin
  result := TNVAssistant.Create(title, dfms, exception);
  if @OnAssistantCreate <> nil then 
    OnAssistantCreate(result, exception);
end;

function LoadAssistant(module: dword; title: string; const forms: array of string; exception: IUnknown) : INVAssistant;
var dfms : TDAString;
    i1   : integer;
    c1   : dword;
begin
  SetLength(dfms, Length(forms));
  if dfms <> nil then begin
    c1 := FindResourceHInstance(module);
    if FindResource(c1, pchar('TME' + UpStr(forms[0])), pchar(RT_RCDATA)) <> 0 then
      // switch to the language dll, but only if it contains the resources we need
      module := c1;
    for i1 := 0 to high(dfms) do
      dfms[i1] := ResToStr(module, pchar(RT_RCDATA), pchar('TME' + UpStr(forms[i1])));
  end;
  result := CreateAssistant(title, dfms, exception);
end;

// ***************************************************************

end.
