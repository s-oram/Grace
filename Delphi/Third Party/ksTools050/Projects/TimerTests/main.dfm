object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 623
  ClientWidth = 731
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    731
    623)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 176
    Top = 576
    Width = 94
    Height = 13
    Caption = 'Timer Interval, ms :'
  end
  object Label2: TLabel
    Left = 40
    Top = 16
    Width = 96
    Height = 13
    Caption = 'TTimer Performance'
  end
  object Label3: TLabel
    Left = 368
    Top = 16
    Width = 106
    Height = 13
    Caption = 'TksTimer Performance'
  end
  object btnRun: TButton
    Left = 392
    Top = 571
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Run'
    TabOrder = 0
    OnClick = btnRunClick
  end
  object Memo1: TMemo
    Left = 40
    Top = 40
    Width = 313
    Height = 481
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Fixedsys'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 368
    Top = 40
    Width = 321
    Height = 481
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Fixedsys'
    Font.Style = []
    Lines.Strings = (
      'Memo2')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object cmbInterval: TComboBox
    Left = 280
    Top = 573
    Width = 57
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 4
    TabOrder = 3
    Text = '20'
    Items.Strings = (
      '1'
      '2'
      '5'
      '10'
      '20'
      '50'
      '100')
  end
end
