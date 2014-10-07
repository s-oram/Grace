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
  object Edit1: TEdit
    Left = 47
    Top = 24
    Width = 522
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 48
    Top = 51
    Width = 521
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 575
    Top = 22
    Width = 26
    Height = 25
    Caption = '...'
    TabOrder = 2
  end
  object Button2: TButton
    Left = 575
    Top = 49
    Width = 26
    Height = 25
    Caption = '...'
    TabOrder = 3
  end
  object Button3: TButton
    Left = 472
    Top = 96
    Width = 129
    Height = 25
    Caption = 'Convert'
    TabOrder = 4
  end
  object Button4: TButton
    Left = 8
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Open Source'
    TabOrder = 5
  end
  object Button5: TButton
    Left = 89
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Open Dest'
    TabOrder = 6
  end
  object SaveDialog1: TSaveDialog
    Left = 304
    Top = 88
  end
  object OpenDialog1: TOpenDialog
    Left = 224
    Top = 88
  end
end
