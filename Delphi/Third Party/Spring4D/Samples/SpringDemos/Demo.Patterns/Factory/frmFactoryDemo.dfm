object Form29: TForm29
  Left = 0
  Top = 0
  Caption = 'Factory Pattern Demo'
  ClientHeight = 385
  ClientWidth = 691
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
    Left = 11
    Top = 16
    Width = 195
    Height = 25
    Caption = 'Create a new Edit Box'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 11
    Top = 47
    Width = 195
    Height = 25
    Caption = 'Create a new Edit Box with red text'
    TabOrder = 1
    OnClick = Button2Click
  end
end
