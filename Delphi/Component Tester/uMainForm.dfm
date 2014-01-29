object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 501
  ClientWidth = 1005
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
    Top = 371
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
  end
  object Memo1: TMemo
    Left = 368
    Top = 32
    Width = 537
    Height = 265
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object RedFoxContainer1: TRedFoxContainer
    Left = 72
    Top = 72
    Width = 377
    Height = 289
    Color = '$FFEEEEEE'
  end
end
