// ***************************************************************
//  meEditAttach.pas          version:  1.0   ·  date: 2005-07-09
//  -------------------------------------------------------------
//  madExcept IDE wizard: edit attachment properties
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2005 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2005-07-09 1.0  initial release

unit meEditAttach;

{$I mad.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, madTypes;

// ***************************************************************

type
  TFEditAttach = class(TForm)
    Bevel1: TBevel;
    AbortBtn: TButton;
    OkBtn: TButton;
    Label1: TLabel;
    Label2: TLabel;
    FileEdit: TEdit;
    SendAsEdit: TEdit;
    Label3: TLabel;
    ZipEdit: TEdit;
    procedure FileEditChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

function EditAttach (var file_, sendAs, zip: string) : boolean;

// ***************************************************************

implementation

{$R *.dfm}

uses madStrings;

// ***************************************************************

function IsValidFilePath(file_: string) : boolean;
var i1, i2 : integer;
    b1     : boolean;
begin
  result := false;
  file_ := UpStr(file_);
  if file_ = '' then
    exit;
  if PosChars(['/', '<', '>', '|', '"', '*', '?'], file_) > 0 then
    exit;
  if (Length(file_) = 1) or ((Copy(file_, 1, 2) <> '\\') and (file_[2] <> ':')) then
    if file_[1] = '\' then
      file_ := 'C:' + file_
    else
      file_  := 'C:\1\2\3\4\5\6\7\8\9\' + file_;
  ReplaceStr(file_, '\.\', '\', true);
  while true do begin
    i1 := PosStr('\..\', file_);
    if i1 = 0 then
      break;
    b1 := true;
    for i2 := i1 - 1 downto 1 do
      if file_[i2] = '\' then begin
        b1 := false;
        Delete(file_, i2 + 1, i1 - i2 + 3);
        break;
      end;
    if b1 then
      exit;
  end;
  if (file_[1] = ':') or (PosStr(':', file_, 3) > 0) then
    exit;
  if PosStr('\\', file_, 2) > 0 then
    exit;
  if length(file_) < 3 then
    exit;
  if file_[2] = ':' then begin
    if not (file_[1] in ['A'..'Z']) then
      exit;
    if file_[3] <> '\' then
      exit;
    Delete(file_, 1, 3);
  end else
    if (file_[1] = '\') and (file_[2] = '\') then begin
      if file_[3] = '\' then
        exit;
      b1 := true;
      i2 := 0;
      for i1 := 4 to Length(file_) do
        if file_[i1] = '\' then
          if i2 > 0 then begin
            b1 := false;
            Delete(file_, 1, i1);
            break;
          end else
            i2 := 1;
      if b1 then
        exit;
    end else
      exit;
  if file_ = '' then
    exit;
  if file_[Length(file_)] = '\' then
    exit;
  result := true;
end;

function IsValidFileName(file_: string) : boolean;
begin
  result := PosChars(['/', '<', '>', '|', '"', '*', '?', ':', '\'], file_) = 0;
end;

procedure TFEditAttach.FileEditChange(Sender: TObject);
begin
  OkBtn.Enabled := ((not FileEdit.Enabled) or IsValidFilePath(FileEdit.Text)) and
                   IsValidFileName(SendAsEdit.Text) and
                   IsValidFileName(ZipEdit.Text);
end;

function EditAttach(var file_, sendAs, zip: string) : boolean;
begin
  with TFEditAttach.Create(nil) do begin
    if (file_ <> '') and (file_[1] = '<') then begin
      FileEdit.Enabled := false;
      FileEdit.Color := clBtnFace;
    end;
    FileEdit.Text := file_;
    SendAsEdit.Text := sendAs;
    ZipEdit.Text := zip;
    result := ShowModal = mrOk;
    if result then begin
      file_  := FileEdit.Text;
      sendAs := SendAsEdit.Text;
      zip    := ZipEdit.Text;
      if sendAs = '' then
        sendAs := ExtractFileName(file_);
    end;
    Free;
  end;
end;

procedure TFEditAttach.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) and FileEdit.Focused then begin
    SendAsEdit.SetFocus;
    Key := #0;
  end else
    if (Key = #13) and SendAsEdit.Focused then begin
      ZipEdit.SetFocus;
      Key := #0;
    end else
      if (Key = #13) and ZipEdit.Focused then begin
        if OkBtn.Enabled then
             OkBtn.SetFocus
        else AbortBtn.SetFocus;
        Key := #0;
      end;
end;

// ***************************************************************

end.
 