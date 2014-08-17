object Form1: TForm1
  Left = 106
  Top = 69
  Caption = 'Wav2TTA Converter'
  ClientHeight = 198
  ClientWidth = 235
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 10
    Top = 43
    Width = 23
    Height = 13
    Caption = 'Artist'
  end
  object Label5: TLabel
    Left = 10
    Top = 67
    Width = 29
    Height = 13
    Caption = 'Album'
  end
  object Label6: TLabel
    Left = 10
    Top = 91
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object Label7: TLabel
    Left = 10
    Top = 115
    Width = 22
    Height = 13
    Caption = 'Year'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Select...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 143
    Width = 235
    Height = 17
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 134
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 179
    Width = 235
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object Button2: TButton
    Left = 96
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 50
    Top = 40
    Width = 177
    Height = 21
    TabOrder = 4
  end
  object Edit2: TEdit
    Left = 50
    Top = 64
    Width = 177
    Height = 21
    TabOrder = 5
  end
  object Edit3: TEdit
    Left = 50
    Top = 88
    Width = 177
    Height = 21
    TabOrder = 6
  end
  object Edit4: TEdit
    Left = 50
    Top = 112
    Width = 177
    Height = 21
    TabOrder = 7
  end
  object Panel1: TPanel
    Left = 0
    Top = 160
    Width = 235
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 8
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 144
    Top = 128
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 72
    Top = 128
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'tta'
    FilterIndex = 0
    Left = 112
    Top = 128
  end
  object TTAOut1: TTTAOut
    Input = WaveIn1
    OnDone = TTAOut1Done
    OnProgress = TTAOut1Progress
    OnThreadException = TTAOut1ThreadException
    ShareMode = 0
    Id3v1Tags.GenreId = 0
    Left = 184
    Top = 128
  end
end
