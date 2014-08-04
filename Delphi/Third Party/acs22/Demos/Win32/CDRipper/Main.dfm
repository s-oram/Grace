object Form1: TForm1
  Left = 155
  Top = 234
  Width = 366
  Height = 214
  Caption = 'Ripper'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 25
    Height = 13
    Caption = 'Drive'
  end
  object Label2: TLabel
    Left = 176
    Top = 8
    Width = 28
    Height = 13
    Caption = 'Track'
  end
  object Label3: TLabel
    Left = 288
    Top = 8
    Width = 28
    Height = 13
    Caption = 'Rip to'
  end
  object Button1: TButton
    Left = 8
    Top = 88
    Width = 73
    Height = 25
    Caption = 'Rip!'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 56
    Width = 345
    Height = 17
    Min = 0
    Max = 100
    ParentShowHint = False
    Smooth = True
    Step = 5
    ShowHint = True
    TabOrder = 1
  end
  object Button2: TButton
    Left = 88
    Top = 88
    Width = 73
    Height = 25
    Caption = 'Abort'
    TabOrder = 2
    OnClick = Button2Click
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 24
    Width = 161
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnSelect = ComboBox1Select
  end
  object ComboBox2: TComboBox
    Left = 176
    Top = 24
    Width = 105
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    OnDropDown = ComboBox2DropDown
    OnEnter = ComboBox2Enter
    OnSelect = ComboBox2Select
  end
  object Button3: TButton
    Left = 168
    Top = 88
    Width = 73
    Height = 25
    Caption = 'Eject'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 248
    Top = 88
    Width = 73
    Height = 25
    Caption = 'Close Tray'
    TabOrder = 6
    OnClick = Button4Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 166
    Width = 358
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object ComboBox3: TComboBox
    Left = 288
    Top = 24
    Width = 65
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 8
    Text = 'Ogg'
    Items.Strings = (
      'Ogg'
      'Wav'
      'Ape'
      'FLAC')
  end
  object CDIn1: TCDIn
    CurrentDrive = 0
    StartTrack = 1
    EndTrack = 1
    Left = 8
    Top = 128
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ogg'
    Filter = 'Vorbis files|*.ogg'
    Left = 168
    Top = 128
  end
  object VorbisOut1: TVorbisOut
    Input = CDIn1
    SuspendWhenIdle = True
    OnDone = VorbisOut1Done
    OnProgress = VorbisOut1Progress
    OnThreadException = VorbisOut1ThreadException
    FileMode = foRewrite
    Compression = 0.600000023841858
    DesiredMaximumBitrate = brAutoSelect
    DesiredNominalBitrate = brAutoSelect
    MinimumBitrate = brAutoSelect
    Serial = 0
    Left = 40
    Top = 128
  end
  object WaveOut1: TWaveOut
    Input = CDIn1
    SuspendWhenIdle = True
    OnDone = VorbisOut1Done
    OnProgress = VorbisOut1Done
    OnThreadException = VorbisOut1ThreadException
    FileMode = foRewrite
    WavType = wtPCM
    BlockSize = 512
    Left = 72
    Top = 128
  end
  object MACOut1: TMACOut
    Input = CDIn1
    SuspendWhenIdle = True
    OnDone = VorbisOut1Done
    OnProgress = VorbisOut1Progress
    OnThreadException = VorbisOut1ThreadException
    FileMode = foRewrite
    CompressionLevel = 2000
    MaxAudioBytes = -1
    Left = 104
    Top = 128
  end
  object FLACOut1: TFLACOut
    Input = CDIn1
    SuspendWhenIdle = True
    OnDone = VorbisOut1Done
    OnProgress = VorbisOut1Progress
    OnThreadException = VorbisOut1ThreadException
    FileMode = foRewrite
    BestModelSearch = False
    BlockSize = 4608
    EnableMidSideStereo = True
    EnableLooseMidSideStereo = False
    MaxLPCOrder = 0
    MaxResidualPartitionOrder = 0
    MinResidualPartitionOrder = 0
    QLPCoeffPrecision = 0
    QLPCoeffPrecisionSearch = False
    Verify = False
    Left = 136
    Top = 128
  end
end
