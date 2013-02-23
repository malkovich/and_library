// ***************************************************************
//  meGlobalCfg.pas           version:  1.0   ·  date: 2005-07-09
//  -------------------------------------------------------------
//  madExcept IDE wizard: madExcept configuration dialog
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2005 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2005-07-09 1.0  initial release

unit meGlobalCfg;

{$I mad.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, madTypes;

// ***************************************************************

type
  TFGlobalCfg = class(TForm)
    Bevel1: TBevel;
    AbortBtn: TButton;
    OkBtn: TButton;
    Shape1: TShape;
    Shape22: TShape;
    Shape23: TShape;
    Label2: TLabel;
    Shape24: TShape;
    Shape25: TShape;
    Shape28: TShape;
    Shape29: TShape;
    IdeExc: TCheckBox;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Label3: TLabel;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    AdjOptions: TCheckBox;
    AdjUses: TCheckBox;
    AdjDefines: TCheckBox;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

procedure EditGlobalCfg;

var
  IdeExceptionCatching : boolean = true;
  AdjustUses           : boolean = true;
  AdjustOptions        : boolean = true;
  AdjustDefines        : boolean = true;

// ***************************************************************

implementation

{$R *.dfm}

uses madExcept, ShellAPI;

// ***************************************************************

function LoadDefaultSettings : boolean;
var hk : HKEY;

  function RegGetBoolean(name: string; var value: boolean) : boolean;
  var len : dword;
  begin
    len := 1;
    result := RegQueryValueEx(hk, pchar(name), nil, nil, @value, @len) = 0;
  end;

begin
  result := RegOpenKeyEx(HKEY_CURRENT_USER, 'Software\madshi\madExcept', 0, KEY_QUERY_VALUE, hk) = 0;
  if result then begin
    RegGetBoolean('IdeExceptionCatching', IdeExceptionCatching);
    RegGetBoolean('AdjustUses',           AdjustUses);
    RegGetBoolean('AdjustOptions',        AdjustOptions);
    RegGetBoolean('AdjustDefines',        AdjustDefines);
    RegCloseKey(hk);
    if not IdeExceptionCatching then
      PauseMadExcept;
  end;
end;

function SaveDefaultSettings : boolean;
var hk : HKEY;

  procedure RegSetBoolean(name: string; value: boolean);
  begin
    RegSetValueEx(hk, pchar(name), 0, REG_BINARY, @value, 1);
  end;

begin
  result := RegCreateKeyEx(HKEY_CURRENT_USER, 'Software\madshi\madExcept', 0, nil, 0, KEY_ALL_ACCESS, nil, hk, nil) = 0;
  if result then begin
    RegSetBoolean('IdeExceptionCatching', IdeExceptionCatching);
    RegSetBoolean('AdjustUses',           AdjustUses);
    RegSetBoolean('AdjustOptions',        AdjustOptions);
    RegSetBoolean('AdjustDefines',        AdjustDefines);
    RegCloseKey(hk);
  end;
end;

procedure EditGlobalCfg;
begin
  with TFGlobalCfg.Create(nil) do begin
    if GetModuleHandle('madExceptIde_.bpl') = 0 then begin
      IdeExc.Checked := false;
      IdeExc.Enabled := false;
    end else
      IdeExc.Checked := IdeExceptionCatching;
    AdjUses.Checked    := AdjustUses;
    AdjOptions.Checked := AdjustOptions;
    AdjDefines.Checked := AdjustDefines;
    if ShowModal = mrOk then begin
      AdjustUses    := AdjUses.Checked;
      AdjustOptions := AdjOptions.Checked;
      AdjustDefines := AdjDefines.Checked;
      if IdeExceptionCatching <> IdeExc.Checked then begin
        IdeExceptionCatching := IdeExc.Checked;
        PauseMadExcept(not IdeExceptionCatching);
      end;
      SaveDefaultSettings;
    end;
    Free;
  end;
end;

// ***************************************************************

procedure TFGlobalCfg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  procedure ShowContextHelp;
  var arrCh : array [0..MAX_PATH] of char;
      s1    : string;
  begin
    GetModuleFileName(HInstance, arrCh, MAX_PATH);
    s1 := ExtractFilePath(arrCh);
    if s1 <> '' then
      s1 := ExtractFilePath(Copy(s1, 1, Length(s1) - 1));
    if s1 <> '' then
      s1 := ExtractFilePath(Copy(s1, 1, Length(s1) - 1));
    s1 := s1 + 'madBasic\Help\Data\madExceptConfiguration.htm';
    if (GetFileAttributes(pchar(s1)) <> maxCard) and
       (FindExecutable(pchar(s1), nil, arrCh) > 32) then
      ShellExecute(0, nil, arrCh, pchar('"' + s1 + '"'), nil, SW_SHOWNORMAL);
  end;

begin
  if Key = VK_F1 then
    ShowContextHelp;
end;

initialization
  LoadDefaultSettings;
end.
