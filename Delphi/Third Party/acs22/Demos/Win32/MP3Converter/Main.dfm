object Form1: TForm1
  Left = 192
  Top = 109
  Caption = 'MP3 Converter'
  ClientHeight = 115
  ClientWidth = 241
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 48
    Width = 82
    Height = 13
    Caption = 'Ogg compression'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Input...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Output...'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 168
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Convert'
    TabOrder = 2
    OnClick = Button3Click
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 80
    Width = 233
    Height = 17
    Smooth = True
    Step = 2
    TabOrder = 3
  end
  object Edit1: TEdit
    Left = 104
    Top = 48
    Width = 41
    Height = 21
    TabOrder = 4
    Text = '0,2'
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 96
    Width = 241
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 105
    ExplicitWidth = 249
  end
  object OpenDialog1: TOpenDialog
    Filter = 'MP3 files|*.mp3'
    Left = 184
    Top = 40
  end
  object SaveDialog1: TSaveDialog
    Filter = 'Wave|*.wav|Ogg Vorbis|*.ogg'
    Left = 216
    Top = 40
  end
end
