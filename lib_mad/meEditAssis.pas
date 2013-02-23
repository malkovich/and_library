// ***************************************************************
//  meEditAssis.pas           version:  1.0   ·  date: 2005-07-09
//  -------------------------------------------------------------
//  madExcept IDE wizard: edit assistant properties
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2005 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2005-07-09 1.0  initial release

unit meEditAssis;

{$I mad.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, madTypes;

// ***************************************************************

type
  TFEditAssis = class(TForm)
    Bevel1: TBevel;
    AbortBtn: TButton;
    OkBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    FormsEdit: TEdit;
    AssisName: TEdit;
    Label3: TLabel;
    AssisTitle: TEdit;
    procedure FormsEditChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private-Deklarationen }
    FAlreadyUsedNames : TDAString;
    FAvailableForms   : TDAString;
  public
    { Public-Deklarationen }
  end;

function EditAssis (var assis: string; const alreadyUsedNames, availableForms: TDAString) : boolean;

// ***************************************************************

implementation

{$R *.dfm}

uses madStrings;

// ***************************************************************

procedure TFEditAssis.FormsEditChange(Sender: TObject);
var s1, s2 : string;
    i1, i2 : integer;
    b1, b2 : boolean;
begin
  s1 := AssisName.Text;
  if (Length(s1) < 255) and IsValidIdent(s1) then begin
    b1 := true;
    if b1 then
      for i1 := 0 to high(FAlreadyUsedNames) do
        if IsTextEqual(FAlreadyUsedNames[i1], s1) then begin
          b1 := false;
          break;
        end;
    if b1 then begin
      s1 := FormsEdit.Text;
      FormatSubStrs(s1, ',');
      b1 := s1 <> '';
      if b1 then
        for i1 := 1 to SubStrCount(s1, ',') do begin
          s2 := SubStr(s1, i1, ',');
          b2 := false;
          for i2 := 0 to high(FAvailableForms) do
            if IsTextEqual(s2, FAvailableForms[i2]) then begin
              b2 := true;
              break;
            end;
          if not b2 then begin
            b1 := false;
            break;
          end;
        end;
    end;
    OkBtn.Enabled := b1;
  end else
    OkBtn.Enabled := false;
end;

function EditAssis(var assis: string; const alreadyUsedNames, availableForms: TDAString) : boolean;
var i1, i2 : integer;
    s1, s2 : string;
begin
  with TFEditAssis.Create(nil) do begin
    FAlreadyUsedNames := alreadyUsedNames;
    FAvailableForms := availableForms;
    AssisName.Text := SubStr(assis, 1);
    AssisTitle.Text := SubStr(assis, 2);
    s1 := '';
    for i1 := 3 to SubStrCount(assis) do
      s1 := s1 + ', ' + SubStr(assis, i1);
    Delete(s1, 1, 2);
    FormsEdit.Text := s1;
    result := ShowModal = mrOk;
    if result then begin
      assis := AssisName.Text + '|' + AssisTitle.Text + '|';
      s1 := FormsEdit.Text;
      FormatSubStrs(s1, ',');
      for i1 := 1 to SubStrCount(s1, ',') do begin
        s2 := SubStr(s1, i1, ',');
        for i2 := 0 to high(FAvailableForms) do
          if IsTextEqual(s2, FAvailableForms[i2]) then begin
            assis := assis + FAvailableForms[i2] + '|';
            break;
          end;
      end;
      Delete(assis, Length(assis), 1);
    end else
      assis := '';
    Free;
  end;
end;

procedure TFEditAssis.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) and AssisName.Focused then begin
    AssisTitle.SetFocus;
    Key := #0;
  end else
    if (Key = #13) and AssisTitle.Focused then begin
      FormsEdit.SetFocus;
      Key := #0;
    end else
      if (Key = #13) and FormsEdit.Focused then begin
        if OkBtn.Enabled then
             OkBtn.SetFocus
        else AbortBtn.SetFocus;
        Key := #0;
      end;
end;

// ***************************************************************

end.
 