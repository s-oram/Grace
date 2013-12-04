object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 300
  ClientWidth = 635
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
  object Button1: TButton
    Left = 8
    Top = 267
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object RedFoxContainer1: TRedFoxContainer
    Left = 48
    Top = 16
    Width = 505
    Height = 185
    Color = '$FFEEEEEE'
    object SampleDisplay: TVamSampleDisplay
      Left = 48
      Top = 32
      Width = 417
      Height = 129
      Text = 'SampleDisplay'
      HitTest = True
      LineColor = '$FF000000'
      Visible = True
    end
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 184
    Top = 224
  end
end
