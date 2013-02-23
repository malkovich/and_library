object Form2: TForm2
  Left = 0
  Top = 0
  Caption = #26426#22120#29305#24449#37319#38598#31243#24207
  ClientHeight = 433
  ClientWidth = 403
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 50
    Width = 84
    Height = 13
    Caption = #26426#22120#25551#36848#21517#31216#65306
  end
  object Label2: TLabel
    Left = 32
    Top = 21
    Width = 48
    Height = 13
    Caption = #29992#25143#21517#65306
  end
  object Edit1: TEdit
    Left = 122
    Top = 21
    Width = 241
    Height = 21
    Hint = #35831#36755#20837#19968#20010#21807#19968#30340#21517#31216#65281
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    Text = 'Edit1'
  end
  object Memo1: TMemo
    Left = 8
    Top = 77
    Width = 387
    Height = 209
    Color = clMedGray
    Lines.Strings = (
      'Memo1')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object Button2: TButton
    Left = 32
    Top = 304
    Width = 75
    Height = 25
    Caption = #29983#25104
    TabOrder = 2
    Visible = False
    OnClick = Button2Click
  end
  object Button4: TButton
    Left = 208
    Top = 304
    Width = 84
    Height = 25
    Caption = #29983#25104#29305#24449#30721
    TabOrder = 3
    OnClick = Button4Click
  end
  object Edit2: TEdit
    Left = 122
    Top = 48
    Width = 241
    Height = 21
    TabOrder = 4
    Text = 'Edit2'
  end
  object Button1: TButton
    Left = 295
    Top = 304
    Width = 87
    Height = 25
    Caption = #22797#21046#21040#21098#20999#26495
    TabOrder = 5
    OnClick = Button1Click
  end
  object Memo2: TMemo
    Left = 8
    Top = 335
    Width = 387
    Height = 89
    Lines.Strings = (
      'Memo2')
    TabOrder = 6
  end
end
