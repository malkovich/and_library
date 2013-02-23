// ***************************************************************
//  madExceptVcl.pas          version:  2.0c  ·  date: 2004-10-22
//  -------------------------------------------------------------
//  VCL exception component
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2004 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2004-10-22 2.0c (1) BCB: the problems are gone, so we can reenable this unit
//                 (2) the visual component got its own package "madExceptVcl_"
// 2003-10-10 2.0b (1) this unit doesn't well in BCB, so we don't use it there
//                 (2) removed the "hidden" event, please install it manually
// 2003-06-09 2.0a deleted the confusing TMadExceptionHandler.Enabled property
// 2001-07-22 2.0  (1) dropping a TMadExceptionHandler component on your main
//                     form now enables the 3 main tasks of madExcept 2.0
//                 (2) OnExceptAction property introduced
// 2001-07-10 1.7c moved expert stuff to new design package madExceptIde
// 2000-11-14 1.6e dropping TMadExceptionHandler didn't check menu item

unit madExceptVcl;

{$I mad.inc}

interface

uses Classes, madExcept, madStackTrace;

// ***************************************************************

type
  // avoid compiler errors if madExceptVcl is in the uses, but not madExcept
  {$ifndef bcb}
    TExceptAction = madExcept.TExceptAction;
    IMEException  = madExcept.IMEException;
  {$endif}
  
  // component to make your life a bit easier
  TMadExceptionHandler = class (TComponent)
  private
    FExceptEvent : TExceptEventOO;
    FActionEvent : TExceptActionEventOO;
    procedure SetExceptEvent (value: TExceptEventOO);
    procedure SetActionEvent (value: TExceptActionEventOO);
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnException    : TExceptEventOO       read FExceptEvent write SetExceptEvent; 
    property OnExceptAction : TExceptActionEventOO read FActionEvent write SetActionEvent;
  end;

// ***************************************************************

procedure Register;

implementation

uses Windows, SysUtils, madMapFile, madTypes;

// ***************************************************************

constructor TMadExceptionHandler.Create(AOwner: TComponent);
var p1 : procedure;
begin
  inherited;
  if csDesigning in ComponentState then begin
    p1 := GetProcAddress(GetModuleHandle('madExceptWizard_.bpl'), 'EnableAttachMapFile');
    if @p1 <> nil then p1;
  end;
end;

destructor TMadExceptionHandler.Destroy;
begin
  if not (csDesigning in ComponentState) then begin
    if @FExceptEvent <> nil then UnregisterExceptionHandler    (FExceptEvent);
    if @FActionEvent <> nil then UnregisterExceptActionHandler (FActionEvent);
  end;
  inherited;
end;

procedure TMadExceptionHandler.SetExceptEvent(value: TExceptEventOO);
begin
  if int64(TMethod(value)) <> int64(TMethod(FExceptEvent)) then begin
    if not (csDesigning in ComponentState) then begin
      if @FExceptEvent <> nil then
        UnregisterExceptionHandler(FExceptEvent);
      if @value <> nil then
        RegisterExceptionHandler(value, stTrySyncCallOnSuccess);
    end;
    FExceptEvent := value;
  end;
end;

procedure TMadExceptionHandler.SetActionEvent(value: TExceptActionEventOO);
begin
  if int64(TMethod(value)) <> int64(TMethod(FActionEvent)) then begin
    if not (csDesigning in ComponentState) then begin
      if @FActionEvent <> nil then
        UnregisterExceptActionHandler(FActionEvent);
      if @value <> nil then
        RegisterExceptActionHandler(value, stTrySyncCallOnSuccess);
    end;
    FActionEvent := value;
  end;
end;

// ***************************************************************

procedure Register;
begin
  RegisterComponents('madshi', [TMadExceptionHandler]);
end;

// ***************************************************************

end.