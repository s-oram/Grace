object Form1: TForm1
  Left = 71
  Top = 141
  Caption = 'Sinc Filter Demo'
  ClientHeight = 367
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    400
    367)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 56
    Width = 21
    Height = 13
    Caption = 'Mag'
  end
  object Label2: TLabel
    Left = 8
    Top = 209
    Width = 21
    Height = 13
    Caption = 'Freq'
  end
  object Label3: TLabel
    Left = 8
    Top = 228
    Width = 54
    Height = 13
    Caption = 'Low Cut-off'
  end
  object Label4: TLabel
    Left = 8
    Top = 275
    Width = 56
    Height = 13
    Caption = 'High Cut-off'
  end
  object Button1: TButton
    Left = 8
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Play...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 312
    Width = 400
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Step = 2
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 38
    Top = 56
    Width = 201
    Height = 145
    BevelOuter = bvLowered
    Color = clWhite
    TabOrder = 2
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 199
      Height = 143
      Align = alClient
    end
  end
  object RadioGroup1: TRadioGroup
    Left = 264
    Top = 56
    Width = 121
    Height = 121
    Caption = ' Filter Type '
    ItemIndex = 0
    Items.Strings = (
      'Low pass'
      'High pass'
      'Band pass'
      'Band reject'
      'All pass')
    TabOrder = 3
    OnClick = RadioGroup1Click
  end
  object TrackBar1: TTrackBar
    Left = 80
    Top = 224
    Width = 297
    Height = 41
    Max = 50
    Min = 1
    PageSize = 1
    Position = 15
    TabOrder = 4
    OnChange = TrackBar1Change
  end
  object TrackBar2: TTrackBar
    Left = 80
    Top = 271
    Width = 297
    Height = 34
    Max = 50
    Min = 1
    PageSize = 1
    Position = 25
    TabOrder = 5
    OnChange = TrackBar2Change
  end
  object Button2: TButton
    Left = 88
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 6
    OnClick = Button2Click
  end
  object Panel2: TPanel
    Left = 0
    Top = 329
    Width = 400
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 7
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 348
    Width = 400
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object SincFilter1: TSincFilter
    Input = WaveIn1
    FilterType = ftLowPass
    HighFreq = 12000
    KernelWidth = 257
    LowFreq = 4000
    WindowType = fwHann
    Left = 288
    Top = 8
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wave files|*.wav'
    Left = 216
    Top = 8
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 256
    Top = 8
  end
  object DXAudioOut1: TDXAudioOut
    Input = SincFilter1
    OnDone = AudioOut1Done
    OnProgress = AudioOut1Progress
    DeviceNumber = 0
    Latency = 100
    PrefetchData = True
    PollingInterval = 100
    FramesInBuffer = 24576
    SpeedFactor = 1.000000000000000000
    Left = 336
    Top = 8
  end
end
