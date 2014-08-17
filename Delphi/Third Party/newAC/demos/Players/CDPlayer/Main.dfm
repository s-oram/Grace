object Form1: TForm1
  Left = 206
  Top = 234
  Caption = 'CD Player'
  ClientHeight = 175
  ClientWidth = 188
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 28
    Height = 13
    Caption = 'Track'
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 25
    Height = 13
    Caption = 'Drive'
  end
  object Button1: TButton
    Left = 8
    Top = 64
    Width = 65
    Height = 25
    Caption = 'Play'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 80
    Top = 64
    Width = 65
    Height = 25
    Caption = 'Pause'
    TabOrder = 1
    OnClick = Button2Click
  end
  object ComboBox2: TComboBox
    Left = 40
    Top = 32
    Width = 121
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnDropDown = ComboBox2DropDown
    OnEnter = ComboBox2Enter
    OnSelect = ComboBox2Select
  end
  object Button3: TButton
    Left = 8
    Top = 128
    Width = 65
    Height = 25
    Caption = 'Eject'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 80
    Top = 128
    Width = 65
    Height = 25
    Caption = 'Close Tray'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 96
    Width = 65
    Height = 25
    Caption = 'Stop'
    TabOrder = 5
    OnClick = Button5Click
  end
  object ComboBox1: TComboBox
    Left = 40
    Top = 8
    Width = 49
    Height = 21
    Style = csDropDownList
    TabOrder = 6
    OnSelect = ComboBox1Select
  end
  object CDPlayer1: TCDPlayer
    CurrentDrive = 0
    LVolume = 0
    RVolume = 0
    Left = 144
    Top = 64
  end
end
