object Form28: TForm28
  Left = 0
  Top = 0
  Caption = 'Observer Pattern Demo'
  ClientHeight = 124
  ClientWidth = 276
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
    Left = 32
    Top = 19
    Width = 66
    Height = 13
    Caption = 'Current Time:'
  end
  object Label2: TLabel
    Left = 32
    Top = 46
    Width = 68
    Height = 13
    Caption = 'GetTickCount:'
  end
  object Edit1: TEdit
    Left = 104
    Top = 16
    Width = 153
    Height = 21
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 104
    Top = 43
    Width = 153
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 184
    Top = 70
    Width = 73
    Height = 31
    Caption = 'All Done'
    ModalResult = 8
    TabOrder = 2
    OnClick = Button1Click
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 32
    Top = 72
  end
end
