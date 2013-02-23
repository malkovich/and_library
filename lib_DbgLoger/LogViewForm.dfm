object Form4: TForm4
  Left = 0
  Top = 0
  Caption = #35843#35797#26085#24535#26597#30475
  ClientHeight = 583
  ClientWidth = 657
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 177
    Top = 0
    Height = 564
    ExplicitLeft = 232
    ExplicitTop = 120
    ExplicitHeight = 100
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 177
    Height = 564
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 0
      Top = 514
      Width = 177
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitLeft = 4
      ExplicitTop = 323
      ExplicitWidth = 169
    end
    object ListBox1: TListBox
      Left = 0
      Top = 21
      Width = 177
      Height = 493
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = ListBox1Click
    end
    object Panel2: TPanel
      Left = 0
      Top = 517
      Width = 177
      Height = 47
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object Panel6: TPanel
        Left = 0
        Top = 25
        Width = 177
        Height = 22
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        object Panel7: TPanel
          Left = 0
          Top = 0
          Width = 23
          Height = 22
          Align = alLeft
          BevelOuter = bvNone
          Caption = #21040
          TabOrder = 0
          OnDblClick = Panel7DblClick
        end
        object DateTimePicker2: TDateTimePicker
          Left = 23
          Top = 0
          Width = 84
          Height = 22
          Align = alClient
          Date = 39754.653892881940000000
          Format = 'yyyy-MM-dd'
          Time = 39754.653892881940000000
          DateFormat = dfLong
          DateMode = dmUpDown
          TabOrder = 1
        end
        object DateTimePicker4: TDateTimePicker
          Left = 107
          Top = 0
          Width = 70
          Height = 22
          Align = alRight
          Date = 39781.064321365740000000
          Format = 'HH:mm:ss'
          Time = 39781.064321365740000000
          DateFormat = dfLong
          Kind = dtkTime
          TabOrder = 2
        end
      end
      object Panel8: TPanel
        Left = 0
        Top = 0
        Width = 177
        Height = 22
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object Panel9: TPanel
          Left = 0
          Top = 0
          Width = 23
          Height = 22
          Align = alLeft
          BevelOuter = bvNone
          Caption = #20174
          TabOrder = 0
        end
        object DateTimePicker1: TDateTimePicker
          Left = 23
          Top = 0
          Width = 84
          Height = 22
          Align = alClient
          Date = 39754.653892881940000000
          Format = 'yyyy-MM-dd'
          Time = 39754.653892881940000000
          DateMode = dmUpDown
          TabOrder = 1
        end
        object DateTimePicker3: TDateTimePicker
          Left = 107
          Top = 0
          Width = 70
          Height = 22
          Align = alRight
          Date = 39781.064321365740000000
          Format = 'HH:mm:ss'
          Time = 39781.064321365740000000
          DateFormat = dfLong
          Kind = dtkTime
          TabOrder = 2
        end
      end
    end
    object Panel4: TPanel
      Left = 0
      Top = 0
      Width = 177
      Height = 21
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 32
        Height = 21
        Align = alLeft
        BevelOuter = bvNone
        Caption = #26174#31034
        TabOrder = 0
      end
      object ComboBox1: TComboBox
        Left = 32
        Top = 0
        Width = 145
        Height = 21
        Align = alClient
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
        OnChange = ComboBox1Change
        Items.Strings = (
          #26368#36817'1'#22825#27963#36291#30340#31867#22411
          #26368#36817'3'#22825#27963#36291#30340#31867#22411
          #26368#36817'7'#22825#27963#36291#30340#31867#22411
          #25152#26377#27963#36291#30340#31867#22411)
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 564
    Width = 657
    Height = 19
    Panels = <>
  end
  object Panel3: TPanel
    Left = 180
    Top = 0
    Width = 477
    Height = 564
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object Memo1: TMemo
      Left = 0
      Top = 0
      Width = 477
      Height = 564
      Align = alClient
      BevelOuter = bvNone
      ScrollBars = ssVertical
      TabOrder = 0
      WordWrap = False
    end
  end
  object MainMenu1: TMainMenu
    Left = 208
    Top = 160
    object N3: TMenuItem
      Caption = #23548#20986#26085#24535
      object N4: TMenuItem
        Caption = #21478#23384#20026
        OnClick = N4Click
      end
      object Email1: TMenuItem
        Caption = #20316#20026#38468#20214#21457#36865'Email'#21040
      end
    end
    object N7: TMenuItem
      Caption = #26174#31034
      object ViewTime: TMenuItem
        Caption = #26102#38388
        Checked = True
        OnClick = ViewTimeClick
      end
      object ViewProcess: TMenuItem
        Caption = #36827#31243
        OnClick = ViewTimeClick
      end
      object ViewProcessID: TMenuItem
        Caption = #36827#31243'ID'
        OnClick = ViewTimeClick
      end
      object ViewModule: TMenuItem
        Caption = #27169#22359
        Checked = True
        OnClick = ViewTimeClick
      end
    end
    object N1: TMenuItem
      Caption = #32500#25252#26085#24535
      object N71: TMenuItem
        Caption = #28165#31354'7'#22825#21069#25152#26377#30340#35760#24405
        OnClick = N71Click
      end
      object N31: TMenuItem
        Caption = #24773#31354'3'#22825#21069#25152#26377#30340#35760#24405
        OnClick = N31Click
      end
      object N11: TMenuItem
        Caption = #28165#31354'1'#22825#21069#25152#26377#30340#35760#24405
        OnClick = N11Click
      end
      object N2: TMenuItem
        Caption = #28165#31354#25968#25454#24211#25152#26377#30340#35760#24405
        OnClick = N2Click
      end
    end
    object N5: TMenuItem
      Caption = #35774#32622
      object N151: TMenuItem
        Caption = #33258#21160#28165#38500'15'#22825#21069#30340#26085#24535
        OnClick = N151Click
      end
    end
    object N6: TMenuItem
      Caption = #24110#21161
      object DbgLogerDLLdll1: TMenuItem
        Caption = 'log.dll'#20351#29992#35828#26126
        OnClick = DbgLogerDLLdll1Click
      end
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 80
    Top = 72
  end
end
