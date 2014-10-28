object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 642
  ClientWidth = 1386
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 876
    Top = 168
    Width = 118
    Height = 52
    Caption = 'Label1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -43
    Font.Name = 'Tekton Pro'
    Font.Style = []
    ParentFont = False
  end
  object Button1: TButton
    Left = 56
    Top = 548
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
  end
  object Button2: TButton
    Left = 148
    Top = 548
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
  end
  object Button3: TButton
    Left = 229
    Top = 548
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 2
  end
  object RedFoxContainer1: TRedFoxContainer
    Left = 80
    Top = 64
    Width = 685
    Height = 433
    Color = '$FFFFFFFF'
    object VamMemo1: TVamMemo
      Left = 76
      Top = 84
      Width = 377
      Height = 233
      Text.Strings = (
        'Test1'
        'Test2'
        'Test3')
      HitTest = True
      Color = '$FF000000'
      ColorMouseOver = '$FF000000'
      TextAlign = AlignNear
      TextVAlign = AlignNear
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Visible = True
    end
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 44
    Top = 112
  end
end
