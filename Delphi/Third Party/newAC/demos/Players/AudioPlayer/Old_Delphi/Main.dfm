object Form1: TForm1
  Left = 246
  Top = 304
  Width = 598
  Height = 461
  ActiveControl = AddtoPLButton
  Caption = 'Audio Player'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    590
    427)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 64
    Top = 40
    Width = 53
    Height = 13
    Caption = 'Total Time:'
  end
  object Label2: TLabel
    Left = 120
    Top = 40
    Width = 3
    Height = 13
  end
  object PlayButton: TButton
    Left = 120
    Top = 8
    Width = 41
    Height = 25
    Caption = 'Play'
    TabOrder = 0
    OnClick = PlayButtonClick
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 285
    Width = 590
    Height = 17
    Align = alBottom
    Min = 0
    Max = 100
    Smooth = True
    Step = 2
    TabOrder = 1
  end
  object StopButton: TButton
    Left = 216
    Top = 8
    Width = 41
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = StopButtonClick
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 38
    Width = 49
    Height = 17
    Caption = 'Loop'
    TabOrder = 3
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 408
    Width = 590
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object AddtoPLButton: TButton
    Left = 8
    Top = 8
    Width = 105
    Height = 25
    Caption = 'Add To PlayList...'
    Default = True
    TabOrder = 5
    OnClick = AddtoPLButtonClick
  end
  object ListBox1: TListBox
    Left = 0
    Top = 319
    Width = 590
    Height = 89
    Align = alBottom
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 6
    OnClick = ListBox1Click
    OnKeyDown = ListBox1KeyDown
  end
  object SkipButton: TButton
    Left = 168
    Top = 8
    Width = 41
    Height = 25
    Caption = '>>|'
    TabOrder = 7
    OnClick = SkipButtonClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 302
    Width = 590
    Height = 17
    Align = alBottom
    TabOrder = 8
  end
  object ForwardButton: TButton
    Left = 319
    Top = 8
    Width = 33
    Height = 25
    Caption = '>>'
    TabOrder = 9
    OnClick = ForwardButtonClick
  end
  object BackwardButton: TButton
    Left = 358
    Top = 8
    Width = 33
    Height = 25
    Caption = '<<'
    TabOrder = 10
    OnClick = BackwardButtonClick
  end
  object Memo1: TMemo
    Left = 0
    Top = 61
    Width = 363
    Height = 219
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      'Memo1')
    ReadOnly = True
    TabOrder = 11
  end
  object Panel2: TPanel
    Left = 369
    Top = 64
    Width = 215
    Height = 215
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Panel2'
    Color = clMaroon
    TabOrder = 12
    DesignSize = (
      215
      215)
    object ProgressBar2: TProgressBar
      Left = 16
      Top = 16
      Width = 17
      Height = 193
      Anchors = [akLeft, akTop, akBottom]
      Min = 0
      Max = 100
      Orientation = pbVertical
      TabOrder = 0
    end
    object ProgressBar3: TProgressBar
      Left = 39
      Top = 16
      Width = 17
      Height = 193
      Anchors = [akLeft, akTop, akBottom]
      Min = 0
      Max = 100
      Orientation = pbVertical
      TabOrder = 1
    end
    object ProgressBar4: TProgressBar
      Left = 62
      Top = 16
      Width = 17
      Height = 193
      Anchors = [akLeft, akTop, akBottom]
      Min = 0
      Max = 100
      Orientation = pbVertical
      TabOrder = 2
    end
    object ProgressBar5: TProgressBar
      Left = 85
      Top = 16
      Width = 17
      Height = 193
      Anchors = [akLeft, akTop, akBottom]
      Min = 0
      Max = 100
      Orientation = pbVertical
      TabOrder = 3
    end
    object ProgressBar6: TProgressBar
      Left = 108
      Top = 16
      Width = 17
      Height = 193
      Anchors = [akLeft, akTop, akBottom]
      Min = 0
      Max = 100
      Orientation = pbVertical
      TabOrder = 4
    end
    object ProgressBar7: TProgressBar
      Left = 131
      Top = 16
      Width = 17
      Height = 193
      Anchors = [akLeft, akTop, akBottom]
      Min = 0
      Max = 100
      Orientation = pbVertical
      TabOrder = 5
    end
    object ProgressBar8: TProgressBar
      Left = 154
      Top = 16
      Width = 17
      Height = 193
      Anchors = [akLeft, akTop, akBottom]
      Min = 0
      Max = 100
      Orientation = pbVertical
      TabOrder = 6
    end
    object ProgressBar9: TProgressBar
      Left = 177
      Top = 16
      Width = 17
      Height = 193
      Anchors = [akLeft, akTop, akBottom]
      Min = 0
      Max = 100
      Orientation = pbVertical
      TabOrder = 7
    end
  end
  object Button1: TButton
    Left = 263
    Top = 8
    Width = 50
    Height = 25
    Caption = 'Pause'
    TabOrder = 13
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 408
    Top = 8
    Width = 174
    Height = 25
    Caption = 'Remove Selected from Playlist'
    TabOrder = 14
    OnClick = Button2Click
  end
  object VorbisIn1: TVorbisIn
    FileName = 'D:\Program Files\Borland\Delphi6\Projects\mozart.ogg'
    Loop = False
    EndSample = -1
    Left = 8
    Top = 160
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'All files|*.*|Ogg Vorbis files|*.ogg|MP3 files|*.mp3|FLAC files|' +
      '*.flac|Monkey Audio|*.ape|Wavpack|*.wv|Wave files|*.wav'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 392
    Top = 160
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    Left = 56
    Top = 160
  end
  object FLACIn1: TFLACIn
    Loop = False
    CheckMD5Signature = False
    EndSample = -1
    Left = 168
    Top = 160
  end
  object MP3In1: TMP3In
    Loop = False
    EndSample = -1
    HighPrecision = False
    OutputChannels = cnMonoOrStereo
    Left = 232
    Top = 160
  end
  object DXAudioOut1: TDXAudioOut
    Input = SpectrumIndicator1
    OnDone = AudioOut1Done
    OnProgress = AudioOut1Progress
    DeviceNumber = 0
    FramesInBuffer = 24576
    Latency = 60
    PollingInterval = 100
    PrefetchData = True
    Left = 328
    Top = 160
  end
  object MACIn1: TMACIn
    Loop = False
    EndSample = -1
    Left = 104
    Top = 160
  end
  object WVIn1: TWVIn
    Loop = False
    EndSample = -1
    Left = 8
    Top = 120
  end
  object SpectrumIndicator1: TSpectrumIndicator
    Interval = 80
    OnGainData = SpectrumIndicator1GainData
    Left = 280
    Top = 160
  end
end
