object Form1: TForm1
  Left = 150
  Top = 88
  Caption = 'Sound Recorder'
  ClientHeight = 269
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    369
    269)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 136
    Top = 140
    Width = 53
    Height = 13
    Caption = 'Ogg quality'
  end
  object Label3: TLabel
    Left = 8
    Top = 56
    Width = 34
    Height = 13
    Caption = 'Device'
  end
  object Label4: TLabel
    Left = 107
    Top = 56
    Width = 48
    Height = 13
    Caption = 'DevName'
  end
  object Label5: TLabel
    Left = 8
    Top = 85
    Width = 53
    Height = 13
    Caption = 'Samplerate'
  end
  object SelectFileButton: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Select File...'
    TabOrder = 0
    OnClick = SelectFileButtonClick
  end
  object RecordButton: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Record'
    TabOrder = 1
    OnClick = RecordButtonClick
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 250
    Width = 369
    Height = 19
    Panels = <
      item
        Width = 240
      end
      item
        Width = 120
      end
      item
        Width = 50
      end>
  end
  object StopButton: TButton
    Left = 248
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 3
    OnClick = StopButtonClick
  end
  object SpinEdit1: TSpinEdit
    Left = 223
    Top = 137
    Width = 41
    Height = 22
    MaxValue = 10
    MinValue = 1
    TabOrder = 4
    Value = 2
  end
  object SpinEdit2: TSpinEdit
    Left = 48
    Top = 53
    Width = 41
    Height = 22
    MaxValue = 10
    MinValue = 1
    TabOrder = 5
    Value = 2
    OnChange = SpinEdit2Change
  end
  object SREdit: TEdit
    Left = 84
    Top = 81
    Width = 57
    Height = 21
    TabOrder = 6
    Text = '44100'
  end
  object StereoCheckBox: TCheckBox
    Left = 157
    Top = 83
    Width = 57
    Height = 17
    Caption = 'Stereo'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 108
    Width = 105
    Height = 57
    Caption = 'Bits per sample'
    ItemIndex = 0
    Items.Strings = (
      '16'
      '24')
    TabOrder = 8
  end
  object PauseButton: TButton
    Left = 168
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Pause'
    TabOrder = 9
    OnClick = PauseButtonClick
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 189
    Width = 352
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    MarqueeInterval = 2
    BarColor = clLime
    BackgroundColor = clBlack
    TabOrder = 10
  end
  object CheckBox2: TCheckBox
    Left = 136
    Top = 111
    Width = 97
    Height = 17
    Caption = 'Skip Silence'
    TabOrder = 11
    OnClick = CheckBox2Click
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.wav'
    Filter = 'Wave|*.wav|Ogg Vorbis|*.ogg|FLAC|*.flac'
    OnTypeChange = SaveDialog1TypeChange
    Left = 296
    Top = 152
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 296
    Top = 104
  end
  object VorbisOut1: TVorbisOut
    Input = GainProcessor1
    OnDone = OutputDone
    ShareMode = 0
    Compression = 0.200000002980232200
    Comments.Track = '0'
    DesiredMaximumBitrate = brAutoSelect
    DesiredNominalBitrate = brAutoSelect
    FileMode = foRewrite
    MinimumBitrate = brAutoSelect
    Serial = 0
    Left = 136
    Top = 208
  end
  object WaveOut1: TWaveOut
    Input = GainProcessor1
    OnDone = OutputDone
    ShareMode = 0
    WavType = wtPCM
    BlockSize = 512
    CreateNonMsHeaders = False
    FileMode = foRewrite
    Left = 80
    Top = 208
  end
  object DXAudioIn1: TDXAudioIn
    Latency = 70
    SamplesToRead = -1
    DeviceNumber = 0
    InBitsPerSample = 8
    InChannels = 1
    InSampleRate = 44100
    RecTime = -1
    EchoRecording = False
    FramesInBuffer = 12000
    PollingInterval = 80
    Left = 16
    Top = 208
  end
  object FLACOut1: TFLACOut
    Input = GainProcessor1
    OnDone = OutputDone
    ShareMode = 0
    BestModelSearch = False
    BlockSize = 4608
    CompressionLevel = -1
    EnableMidSideStereo = True
    EnableLooseMidSideStereo = False
    MaxLPCOrder = 0
    MaxResidualPartitionOrder = 0
    MinResidualPartitionOrder = 0
    QLPCoeffPrecision = 0
    QLPCoeffPrecisionSearch = False
    Tags.Track = '0'
    Verify = False
    Left = 192
    Top = 208
  end
  object GainProcessor1: TGainProcessor
    Input = FastGainIndicator1
    SkipSilenceEnabled = False
    MinSilenceInterval = 1
    SilenceThreshold = 5
    Left = 296
    Top = 208
  end
  object FastGainIndicator1: TFastGainIndicator
    Input = DXAudioIn1
    Interval = 100
    OnGainData = GainIndicator1GainData
    Left = 240
    Top = 208
  end
end
