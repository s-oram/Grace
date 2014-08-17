object Form1: TForm1
  Left = 229
  Top = 225
  Caption = 'Rip and Listen'
  ClientHeight = 180
  ClientWidth = 358
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
    TabOrder = 3
    OnSelect = ComboBox1Select
  end
  object ComboBox2: TComboBox
    Left = 176
    Top = 24
    Width = 105
    Height = 21
    Style = csDropDownList
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
    Top = 161
    Width = 358
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object ComboBox3: TComboBox
    Left = 288
    Top = 24
    Width = 65
    Height = 21
    Style = csDropDownList
    ItemIndex = 3
    TabOrder = 8
    Text = 'FLAC'
    Items.Strings = (
      'Ogg'
      'Wav'
      'Ape'
      'FLAC'
      'WMA')
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 128
    Width = 97
    Height = 17
    Caption = 'Mute'
    TabOrder = 9
    OnClick = CheckBox1Click
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ogg'
    Filter = 'Vorbis files|*.ogg'
    Left = 288
    Top = 120
  end
  object WaveOut1: TWaveOut
    Input = AudioPass1
    OnDone = OutputDone
    OnProgress = Progress
    ShareMode = 0
    WavType = wtPCM
    BlockSize = 512
    CreateNonMsHeaders = True
    FileMode = foRewrite
    Left = 192
    Top = 120
  end
  object VorbisOut1: TVorbisOut
    Input = AudioPass1
    OnDone = OutputDone
    OnProgress = Progress
    ShareMode = 0
    Compression = 0.200000002980232200
    Comments.Track = '0'
    DesiredMaximumBitrate = brAutoSelect
    DesiredNominalBitrate = brAutoSelect
    FileMode = foRewrite
    MinimumBitrate = brAutoSelect
    Serial = 0
    Left = 224
    Top = 120
  end
  object CDIn1: TCDIn
    EnableJitterCorrection = False
    LockTray = False
    MultiReadCount = 0
    Paranoid = False
    ParanoiaMode = 0
    Left = 64
    Top = 120
  end
  object AudioPass1: TAudioPass
    Input = CDIn1
    DeviceNumber = 0
    Mute = False
    Left = 96
    Top = 120
  end
  object MACOut1: TMACOut
    Input = AudioPass1
    OnDone = OutputDone
    OnProgress = Progress
    ShareMode = 0
    CompressionLevel = 2000
    MaxAudioBytes = -1
    Left = 128
    Top = 120
  end
  object FLACOut1: TFLACOut
    Input = AudioPass1
    OnDone = OutputDone
    OnProgress = Progress
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
    Left = 160
    Top = 120
  end
  object WMAOut1: TWMAOut
    Input = AudioPass1
    OnDone = OutputDone
    OnProgress = Progress
    ShareMode = 0
    DesiredBitrate = 0
    Lossless = False
    VBR = False
    VBRQuality = 0
    Left = 256
    Top = 120
  end
end
