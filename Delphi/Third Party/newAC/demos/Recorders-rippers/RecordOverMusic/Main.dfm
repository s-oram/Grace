object Form15: TForm15
  Left = 0
  Top = 0
  Caption = 'Record voice over music'
  ClientHeight = 180
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 149
    Top = 19
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 8
    Top = 96
    Width = 25
    Height = 13
    Caption = 'Voice'
  end
  object Label3: TLabel
    Left = 8
    Top = 129
    Width = 56
    Height = 13
    Caption = 'Background'
  end
  object Label4: TLabel
    Left = 8
    Top = 19
    Width = 26
    Height = 13
    Caption = 'Input'
  end
  object Button1: TButton
    Left = 8
    Top = 49
    Width = 75
    Height = 25
    Caption = 'Start...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object SpinEdit1: TSpinEdit
    Left = 48
    Top = 16
    Width = 81
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 0
    OnChange = SpinEdit1Change
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 161
    Width = 360
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 249
    ExplicitWidth = 458
  end
  object Button2: TButton
    Left = 89
    Top = 49
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 3
    OnClick = Button2Click
  end
  object ProgressBar1: TProgressBar
    Left = 80
    Top = 93
    Width = 273
    Height = 16
    BarColor = clLime
    BackgroundColor = clBlack
    TabOrder = 4
  end
  object ProgressBar2: TProgressBar
    Left = 80
    Top = 126
    Width = 273
    Height = 16
    BarColor = clYellow
    BackgroundColor = clBlack
    TabOrder = 5
  end
  object DXAudioIn1: TDXAudioIn
    Latency = 80
    SamplesToRead = -1
    DeviceNumber = 1
    InBitsPerSample = 8
    InChannels = 1
    InSampleRate = 8000
    RecTime = -1
    EchoRecording = False
    FramesInBuffer = 24576
    PollingInterval = 100
    Left = 240
    Top = 16
  end
  object WaveOut1: TWaveOut
    Input = RealTimeMixer1
    OnDone = WaveOut1Done
    ShareMode = 0
    WavType = wtPCM
    BlockSize = 512
    CreateNonMsHeaders = True
    FileMode = foRewrite
    Left = 328
    Top = 64
  end
  object WaveIn1: TWaveIn
    Loop = True
    EndSample = -1
    StartSample = 0
    Left = 240
    Top = 64
  end
  object RealTimeMixer1: TRealTimeMixer
    Input1 = FastGainIndicator1
    Input2 = FastGainIndicator2
    OutSampleRate = 0
    OutBitsPerSample = 0
    OutChannels = 0
    Volume1 = 65535
    Volume2 = 32000
    Left = 288
    Top = 64
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'wav'
    Filter = 'Wave files|*.wav'
    Title = 'Select background audio'
    Left = 200
    Top = 64
  end
  object FastGainIndicator1: TFastGainIndicator
    Input = DXAudioIn1
    Interval = 100
    OnGainData = GainIndicator1GainData
    Left = 280
    Top = 16
  end
  object FastGainIndicator2: TFastGainIndicator
    Input = WaveIn1
    Interval = 100
    OnGainData = GainIndicator2GainData
    Left = 320
    Top = 16
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'wav'
    Filter = 'Wave Files|*.wav'
    Title = 'Save result as'
    Left = 200
    Top = 16
  end
end
