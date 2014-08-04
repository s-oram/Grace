object Form1: TForm1
  Left = 194
  Top = 185
  Width = 382
  Height = 207
  Caption = 'Sound Recorder'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 89
    Height = 13
    Caption = 'Duration (seconds)'
  end
  object Button1: TButton
    Left = 128
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Record ...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 152
    Width = 313
    Height = 17
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object Button2: TButton
    Left = 216
    Top = 120
    Width = 75
    Height = 25
    Caption = 'Abort'
    TabOrder = 2
    OnClick = Button2Click
  end
  object SpinEdit1: TSpinEdit
    Left = 104
    Top = 8
    Width = 49
    Height = 22
    Increment = 10
    MaxValue = 600
    MinValue = 30
    TabOrder = 3
    Value = 30
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 40
    Width = 105
    Height = 105
    Caption = 'Sample Rate (Hz)'
    TabOrder = 4
    object RadioButton1: TRadioButton
      Left = 8
      Top = 24
      Width = 49
      Height = 17
      Caption = '11025'
      TabOrder = 0
    end
    object RadioButton2: TRadioButton
      Left = 8
      Top = 48
      Width = 57
      Height = 17
      Caption = '22050'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
    object RadioButton3: TRadioButton
      Left = 8
      Top = 72
      Width = 57
      Height = 17
      Caption = '44100'
      TabOrder = 2
    end
  end
  object GroupBox2: TGroupBox
    Left = 120
    Top = 40
    Width = 97
    Height = 73
    Caption = 'Bits per Sample'
    TabOrder = 5
    object RadioButton4: TRadioButton
      Left = 8
      Top = 24
      Width = 41
      Height = 17
      Caption = '8'
      TabOrder = 0
    end
    object RadioButton5: TRadioButton
      Left = 8
      Top = 48
      Width = 41
      Height = 17
      Caption = '16'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
  end
  object GroupBox3: TGroupBox
    Left = 224
    Top = 40
    Width = 97
    Height = 73
    Caption = 'Channels'
    TabOrder = 6
    object RadioButton6: TRadioButton
      Left = 8
      Top = 24
      Width = 57
      Height = 17
      Caption = 'Mono'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object RadioButton7: TRadioButton
      Left = 8
      Top = 48
      Width = 57
      Height = 17
      Caption = 'Stereo'
      TabOrder = 1
    end
  end
  object ProgressBar2: TProgressBar
    Left = 344
    Top = 8
    Width = 17
    Height = 161
    Min = 0
    Max = 40
    Orientation = pbVertical
    Step = 1
    TabOrder = 7
  end
  object AudioIn1: TAudioIn
    BaseChannel = 0
    InBitsPerSample = 16
    InChannels = 2
    InSampleRate = 44100
    RecTime = 180
    Left = 168
    Top = 8
  end
  object WaveOut1: TWaveOut
    Input = SoundIndicator1
    SuspendWhenIdle = True
    OnDone = WaveOut1Done
    OnProgress = WaveOut1Progress
    FileMode = foRewrite
    FileName = 'mozart.wav'
    Left = 232
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'wav'
    Left = 264
    Top = 8
  end
  object SoundIndicator1: TSoundIndicator
    Input = AudioIn1
    Left = 200
    Top = 8
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 296
    Top = 8
  end
end
