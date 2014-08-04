object Form1: TForm1
  Left = 192
  Top = 109
  Width = 237
  Height = 182
  Caption = 'Audio Mixer Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Input1...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Input2...'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 16
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Do Ouput...'
    TabOrder = 2
    OnClick = Button3Click
  end
  object RadioButton1: TRadioButton
    Left = 120
    Top = 16
    Width = 57
    Height = 17
    Caption = 'Mix'
    Checked = True
    TabOrder = 3
    TabStop = True
    OnClick = RadioButton1Click
  end
  object RadioButton2: TRadioButton
    Left = 120
    Top = 40
    Width = 89
    Height = 17
    Caption = 'Concatenate'
    TabOrder = 4
    OnClick = RadioButton2Click
  end
  object WaveIn1: TWaveIn
    Loop = False
    Left = 16
    Top = 104
  end
  object WaveIn2: TWaveIn
    Loop = False
    Left = 48
    Top = 104
  end
  object AudioMixer1: TAudioMixer
    Input1 = WaveIn1
    Input2 = WaveIn2
    Mode = amMix
    Volume1 = 255
    Volume2 = 255
    Left = 80
    Top = 104
  end
  object WaveOut1: TWaveOut
    Input = AudioMixer1
    SuspendWhenIdle = True
    OnDone = WaveOut1Done
    Left = 112
    Top = 104
  end
  object OpenDialog1: TOpenDialog
    Filter = 'WAV files|*.wav'
    Left = 144
    Top = 104
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'wav'
    Left = 176
    Top = 104
  end
end
