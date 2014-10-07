object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Lucidity Patch Updater'
  ClientHeight = 144
  ClientWidth = 614
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 27
    Width = 33
    Height = 13
    Caption = 'Source'
  end
  object Label2: TLabel
    Left = 8
    Top = 54
    Width = 22
    Height = 13
    Caption = 'Dest'
  end
  object SourceEdit: TEdit
    Left = 47
    Top = 24
    Width = 522
    Height = 21
    TabOrder = 0
    Text = 'SourceEdit'
  end
  object DestEdit: TEdit
    Left = 48
    Top = 51
    Width = 521
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
  end
  object SourceSelectButton: TButton
    Left = 575
    Top = 24
    Width = 26
    Height = 21
    Caption = '...'
    TabOrder = 2
    OnClick = SourceSelectButtonClick
  end
  object DestSelectButton: TButton
    Left = 575
    Top = 51
    Width = 26
    Height = 21
    Caption = '...'
    TabOrder = 3
    OnClick = DestSelectButtonClick
  end
  object ConvertPatchButton: TButton
    Left = 477
    Top = 96
    Width = 129
    Height = 25
    Caption = 'Convert'
    TabOrder = 4
    OnClick = ConvertPatchButtonClick
  end
  object OpenSourceButton: TButton
    Left = 8
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Open Source'
    TabOrder = 5
  end
  object OpenDestButton: TButton
    Left = 89
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Open Dest'
    TabOrder = 6
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 216
    Top = 88
  end
  object FileSaveDialog1: TFileSaveDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 280
    Top = 88
  end
end
