// ***************************************************************
//  meAddComp.pas             version:  1.0a  ·  date: 2005-09-18
//  -------------------------------------------------------------
//  madExcept IDE wizard: add components to an assistant
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2005 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2005-09-18 1.0a minor change for big fonts
// 2005-07-09 1.0  initial release

unit meAddComp;

{$I mad.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, madTypes;

// ***************************************************************

type
  TFNewComp = class(TForm)
    Bevel1: TBevel;
    AbortBtn: TButton;
    OkBtn: TButton;
    Label1: TLabel;
    CompCombo: TComboBox;
    Label2: TLabel;
    CompEdit: TEdit;
    procedure CompEditChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure CompComboKeyPress(Sender: TObject; var Key: Char);
    procedure CompComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  private
    { Private-Deklarationen }
    FCompImgs : TImageList;
    FAlreadyUsedNames : TDAString;
  public
    { Public-Deklarationen }
  end;

function AddComponent (var comp, name_: string; const alreadyUsedNames: TDAString; compImgs: TImageList) : boolean;
function AddForm      (var name_: string; const alreadyUsedNames: TDAString; compImgs: TImageList) : boolean;

// ***************************************************************

implementation

{$R *.dfm}

uses madStrings, CommCtrl;

// ***************************************************************

procedure TFNewComp.CompEditChange(Sender: TObject);
var s1 : string;
    i1 : integer;
    b1 : boolean;
begin
  s1 := CompEdit.Text;
  if (Length(s1) < 255) and IsValidIdent(s1) then begin
    b1 := true;
    if b1 then
      for i1 := 0 to high(FAlreadyUsedNames) do
        if IsTextEqual(FAlreadyUsedNames[i1], s1) then begin
          b1 := false;
          break;
        end;
    OkBtn.Enabled := b1;
  end else
    OkBtn.Enabled := false;
end;

function AddComponent(var comp, name_: string; const alreadyUsedNames: TDAString; compImgs: TImageList) : boolean;
begin
  with TFNewComp.Create(nil) do begin
    CompCombo.Height     := CompEdit.Height + 1;
    CompCombo.ItemHeight := CompEdit.Height - 5;
    FAlreadyUsedNames := alreadyUsedNames;
    FCompImgs := compImgs;
    result := ShowModal = mrOk;
    if result then begin
      comp := CompCombo.Items[CompCombo.ItemIndex];
      name_ := CompEdit.Text;
      TrimStr(name_);
    end else begin
      comp := '';
      name_ := '';
    end;
    Free;
  end;
end;

function AddForm(var name_: string; const alreadyUsedNames: TDAString; compImgs: TImageList) : boolean;
begin
  with TFNewComp.Create(nil) do begin
    FAlreadyUsedNames := alreadyUsedNames;
    FCompImgs := compImgs;
    Caption := 'Add form...';
    CompCombo.Items.Clear;
    CompCombo.Items.Add('Form');
    CompCombo.ItemIndex := 0;
    CompCombo.Enabled := false;
    result := ShowModal = mrOk;
    if result then begin
      name_ := CompEdit.Text;
      TrimStr(name_);
    end else
      name_ := '';
    Free;
  end;
end;

procedure TFNewComp.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) and CompEdit.Focused then begin
    if OkBtn.Enabled then
         OkBtn.SetFocus
    else AbortBtn.SetFocus;
    Key := #0;
  end;
end;

procedure TFNewComp.CompComboKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then begin
    CompEdit.SetFocus;
    Key := #0;
  end;
end;

procedure TFNewComp.CompComboDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var i1 : integer;
begin
  with TComboBox(Control) do begin
    Canvas.FillRect(Rect);
    if Items.Count > 1 then
      i1 := Index + 2
    else
      i1 := 1;
    ImageList_Draw(FCompImgs.Handle, i1, Canvas.Handle, Rect.Left + 1, Rect.Top, ILD_NORMAL);
    Canvas.TextOut(Rect.Left + 20, Rect.Top + 2, Items[Index]);
  end;
end;

// ***************************************************************

end.
