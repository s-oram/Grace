object Form1: TForm1
  Left = 540
  Top = 394
  Caption = 'RadioRecorder'
  ClientHeight = 273
  ClientWidth = 465
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
    465
    273)
  PixelsPerInch = 96
  TextHeight = 13
  object Label14: TLabel
    Left = 339
    Top = 75
    Width = 102
    Height = 13
  end
  object Label15: TLabel
    Left = 120
    Top = 75
    Width = 35
    Height = 13
    Caption = 'Volume'
  end
  object Label16: TLabel
    Left = 8
    Top = 75
    Width = 30
    Height = 13
    Caption = 'Bitrate'
  end
  object TLabel
    Left = 8
    Top = 109
    Width = 54
    Height = 13
    Caption = 'Signal level'
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 40
    Width = 65
    Height = 25
    Caption = 'Play'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 88
    Top = 40
    Width = 49
    Height = 25
    Caption = 'Pause'
    NumGlyphs = 2
    TabOrder = 1
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 151
    Top = 40
    Width = 49
    Height = 26
    Caption = 'Stop'
    NumGlyphs = 2
    TabOrder = 2
    OnClick = BitBtn3Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 254
    Width = 465
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ParentColor = True
    ExplicitWidth = 466
  end
  object Panel1: TPanel
    Left = 0
    Top = 173
    Width = 465
    Height = 81
    Align = alBottom
    BevelOuter = bvNone
    Color = 2240534
    TabOrder = 4
    ExplicitWidth = 466
    DesignSize = (
      465
      81)
    object Label2: TLabel
      Left = 8
      Top = 8
      Width = 23
      Height = 13
      Caption = 'Artist'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label3: TLabel
      Left = 48
      Top = 8
      Width = 14
      Height = 13
      Caption = 'No'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label5: TLabel
      Left = 8
      Top = 24
      Width = 29
      Height = 13
      Caption = 'Album'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label9: TLabel
      Left = 48
      Top = 24
      Width = 14
      Height = 13
      Caption = 'No'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label10: TLabel
      Left = 8
      Top = 40
      Width = 20
      Height = 13
      Caption = 'Title'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label11: TLabel
      Left = 48
      Top = 40
      Width = 257
      Height = 13
      AutoSize = False
      Caption = 'No'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label12: TLabel
      Left = 8
      Top = 56
      Width = 22
      Height = 13
      Caption = 'Year'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label13: TLabel
      Left = 48
      Top = 56
      Width = 14
      Height = 13
      Caption = 'No'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label4: TLabel
      Left = 332
      Top = 8
      Width = 8
      Height = 13
      Anchors = [akTop, akRight]
      Caption = '0'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      ExplicitLeft = 333
    end
    object Label1: TLabel
      Left = 332
      Top = 24
      Width = 73
      Height = 17
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = '0'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      ExplicitLeft = 320
    end
    object Label6: TLabel
      Left = 332
      Top = 56
      Width = 57
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Total time'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      ExplicitLeft = 320
    end
    object Label7: TLabel
      Left = 396
      Top = 56
      Width = 8
      Height = 13
      Anchors = [akTop, akRight]
      Caption = '0'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      ExplicitLeft = 397
    end
    object Label8: TLabel
      Left = 332
      Top = 40
      Width = 31
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'mono'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      ExplicitLeft = 320
    end
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 162
    Width = 465
    Height = 11
    Align = alBottom
    Anchors = [akBottom]
    TabOrder = 5
    ExplicitLeft = 16
    ExplicitTop = 164
    ExplicitWidth = 466
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 8
    Width = 400
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    ExplicitWidth = 401
  end
  object RecordButton: TButton
    Left = 216
    Top = 40
    Width = 105
    Height = 25
    Caption = 'Start/Stop record'
    TabOrder = 7
    OnClick = RecordButtonClick
  end
  object PauseRecordButton: TButton
    Left = 328
    Top = 40
    Width = 81
    Height = 25
    Caption = 'Pause Record'
    TabOrder = 8
    OnClick = PauseRecordButtonClick
  end
  object TrackBar1: TTrackBar
    Left = 160
    Top = 72
    Width = 161
    Height = 24
    Max = 0
    Min = -4000
    Frequency = 4
    TabOrder = 9
    ThumbLength = 12
    TickStyle = tsNone
    OnChange = TrackBar1Change
  end
  object Edit1: TEdit
    Left = 48
    Top = 72
    Width = 57
    Height = 21
    TabOrder = 10
    Text = '256'
  end
  object ProgressBar2: TProgressBar
    Left = 8
    Top = 128
    Width = 449
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    MarqueeInterval = 5
    BarColor = clLime
    BackgroundColor = clBlack
    TabOrder = 11
    ExplicitWidth = 450
  end
  object OpenDialog1: TOpenDialog
    Filter = 'WMA files|*.wma'
    Left = 256
    Top = 208
  end
  object WMStreamedIn1: TWMStreamedIn
    Loop = False
    BufferingTime = 2
    EnableHTTP = False
    EnableTCP = False
    EnableUDP = False
    MaxWaitMilliseconds = 10000
    ProxyPort = 0
    StretchFactor = 1.000000000000000000
    OnStreamOpened = WMStreamedIn1StreamOpened
    OnStartedPlaying = WMStreamedIn1StartedPlaying
    Left = 112
    Top = 112
  end
  object WMATap1: TWMATap
    Input = FastResampler1
    DesiredBitrate = 0
    Lossless = False
    VBR = False
    VBRQuality = 0
    Left = 224
    Top = 112
  end
  object FastGainIndicator1: TFastGainIndicator
    Input = WMATap1
    Interval = 60
    OnGainData = GainIndicator1GainData
    Left = 272
    Top = 112
  end
  object FastResampler1: TFastResampler
    Input = WMStreamedIn1
    OutSampleRate = 22050
    Left = 168
    Top = 112
  end
  object DXAudioOut1: TDXAudioOut
    Input = FastGainIndicator1
    DeviceNumber = 0
    Latency = 100
    PrefetchData = True
    PollingInterval = 100
    FramesInBuffer = 24576
    SpeedFactor = 1.000000000000000000
    Left = 336
    Top = 112
  end
end
