object Form1: TForm1
  Left = 194
  Top = 110
  Caption = 'Mixer Demo'
  ClientHeight = 166
  ClientWidth = 377
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
  object Label1: TLabel
    Left = 104
    Top = 16
    Width = 44
    Height = 13
    Caption = 'Volume 1'
  end
  object Label2: TLabel
    Left = 104
    Top = 40
    Width = 44
    Height = 13
    Caption = 'Volume 2'
  end
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
    Caption = 'Go!'
    TabOrder = 2
    OnClick = Button3Click
  end
  object TrackBar1: TTrackBar
    Left = 152
    Top = 16
    Width = 150
    Height = 25
    Max = 65535
    TabOrder = 3
    ThumbLength = 16
    TickStyle = tsNone
    OnChange = TrackBar1Change
  end
  object TrackBar2: TTrackBar
    Left = 152
    Top = 40
    Width = 150
    Height = 25
    Max = 65535
    TabOrder = 4
    ThumbLength = 16
    TickStyle = tsNone
    OnChange = TrackBar2Change
  end
  object RadioGroup1: TRadioGroup
    Left = 112
    Top = 64
    Width = 105
    Height = 57
    Caption = 'Action'
    ItemIndex = 0
    Items.Strings = (
      'Mix'
      'Concatenate')
    TabOrder = 5
  end
  object RadioGroup2: TRadioGroup
    Left = 232
    Top = 64
    Width = 105
    Height = 57
    Caption = 'Output'
    ItemIndex = 0
    Items.Strings = (
      'File'
      'Speakers')
    TabOrder = 6
  end
  object Button4: TButton
    Left = 16
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 7
    OnClick = Button4Click
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 16
    Top = 128
  end
  object WaveIn2: TWaveIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 48
    Top = 128
  end
  object OpenDialog1: TOpenDialog
    Filter = 'WAV files|*.wav'
    Left = 272
    Top = 128
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'wav'
    Left = 312
    Top = 128
  end
  object DXAudioOut1: TDXAudioOut
    Input = AudioMixer1
    OnDone = WaveOut1Done
    DeviceNumber = 0
    Latency = 100
    PrefetchData = True
    PollingInterval = 200
    FramesInBuffer = 65536
    SpeedFactor = 1.000000000000000000
    Left = 168
    Top = 128
  end
  object WaveOut1: TWaveOut
    Input = AudioMixer1
    OnDone = WaveOut1Done
    ShareMode = 0
    WavType = wtPCM
    BlockSize = 512
    CreateNonMsHeaders = False
    FileMode = foRewrite
    Left = 128
    Top = 128
  end
  object AudioMixer1: TAudioMixer
    Input1 = WaveIn1
    Input2 = WaveIn2
    Mode = amMix
    Input2StartSample = 0
    Volume1 = 0
    Volume2 = 0
    Left = 88
    Top = 128
  end
end
