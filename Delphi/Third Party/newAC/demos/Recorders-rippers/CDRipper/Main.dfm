object Form1: TForm1
  Left = 229
  Top = 225
  Caption = 'Ripper'
  ClientHeight = 195
  ClientWidth = 358
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
  object Label4: TLabel
    Left = 8
    Top = 151
    Width = 38
    Height = 13
    Caption = 'No error'
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
    Top = 176
    Width = 358
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 168
  end
  object ComboBox3: TComboBox
    Left = 288
    Top = 24
    Width = 65
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 8
    Text = 'Ogg'
    Items.Strings = (
      'Ogg'
      'Wav'
      'Ape'
      'FLAC')
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 128
    Width = 145
    Height = 17
    Caption = 'Enable jitter correction'
    TabOrder = 9
    OnClick = CheckBox1Click
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ogg'
    Filter = 'Vorbis files|*.ogg'
    Left = 304
    Top = 128
  end
  object WaveOut1: TWaveOut
    Input = CDIn1
    OnDone = OutputDone
    OnProgress = Progress
    ShareMode = 0
    WavType = wtPCM
    BlockSize = 512
    CreateNonMsHeaders = False
    FileMode = foRewrite
    Left = 240
    Top = 128
  end
  object VorbisOut1: TVorbisOut
    Input = CDIn1
    OnDone = OutputDone
    OnProgress = Progress
    ShareMode = 0
    Compression = 0.300000011920929000
    Comments.Track = '0'
    DesiredMaximumBitrate = brAutoSelect
    DesiredNominalBitrate = brAutoSelect
    FileMode = foRewrite
    MinimumBitrate = brAutoSelect
    Serial = 0
    Left = 272
    Top = 128
  end
  object FLACOut1: TFLACOut
    Input = CDIn1
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
    Left = 176
    Top = 128
  end
  object MACOut1: TMACOut
    Input = CDIn1
    OnDone = OutputDone
    OnProgress = Progress
    ShareMode = 0
    CompressionLevel = 2000
    MaxAudioBytes = -1
    Left = 208
    Top = 128
  end
  object CDIn1: TCDIn
    EnableJitterCorrection = False
    LockTray = False
    MultiReadCount = 0
    Paranoid = False
    ParanoiaMode = 0
    Left = 144
    Top = 128
  end
end
