object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Lucidity Key Generator'
  ClientHeight = 552
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    500
    552)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 27
    Height = 13
    Caption = 'Name'
  end
  object Label2: TLabel
    Left = 8
    Top = 36
    Width = 24
    Height = 13
    Caption = 'Email'
  end
  object KeyInfoText: TLabel
    Left = 8
    Top = 522
    Width = 480
    Height = 25
    Alignment = taCenter
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 'KEY IS VALID'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ExplicitTop = 443
    ExplicitWidth = 433
  end
  object EmailInput: TEdit
    Left = 38
    Top = 32
    Width = 450
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = #1102#1079#1077#1088'@'#1077#1082#1079#1072#1084#1087#1083'.'#1082#1086#1084
  end
  object NameInput: TEdit
    Left = 38
    Top = 5
    Width = 450
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'Joe Smith'
  end
  object Memo1: TMemo
    Left = 8
    Top = 103
    Width = 480
    Height = 413
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 3
  end
  object GoButton: TButton
    Left = 228
    Top = 64
    Width = 83
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Create Key'
    TabOrder = 2
    OnClick = CreateKeyButtonClick
  end
  object OpenKeyButton: TButton
    Left = 406
    Top = 64
    Width = 83
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Open Key'
    TabOrder = 4
    OnClick = OpenKeyButtonClick
  end
  object ClearButton: TButton
    Left = 8
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 5
    OnClick = ClearButtonClick
  end
  object ExportKeyButton: TButton
    Left = 317
    Top = 64
    Width = 83
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Export Key'
    TabOrder = 6
    OnClick = ExportKeyButtonClick
  end
  object FileSaveDialog1: TFileSaveDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 100
    Top = 64
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 136
    Top = 64
  end
end
