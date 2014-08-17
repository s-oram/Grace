object Form1: TForm1
  Left = 246
  Top = 190
  Width = 357
  Height = 224
  Caption = 'MP3 Player'
  Color = clSilver
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object BitBtn1: TBitBtn
    Left = 16
    Top = 8
    Width = 65
    Height = 25
    Caption = 'Play...'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 96
    Top = 8
    Width = 49
    Height = 25
    Caption = 'Pause'
    TabOrder = 1
    OnClick = BitBtn2Click
    NumGlyphs = 2
  end
  object BitBtn3: TBitBtn
    Left = 160
    Top = 8
    Width = 49
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = BitBtn3Click
    NumGlyphs = 2
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 171
    Width = 349
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 90
    Width = 349
    Height = 81
    Align = alBottom
    BevelOuter = bvNone
    Color = clBlack
    TabOrder = 4
    DesignSize = (
      349
      81)
    object Label2: TLabel
      Left = 8
      Top = 8
      Width = 23
      Height = 13
      Caption = 'Artist'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label3: TLabel
      Left = 48
      Top = 8
      Width = 32
      Height = 13
      Caption = 'Label3'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
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
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label9: TLabel
      Left = 48
      Top = 24
      Width = 32
      Height = 13
      Caption = 'Label9'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
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
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label11: TLabel
      Left = 48
      Top = 40
      Width = 38
      Height = 13
      Caption = 'Label11'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
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
      Font.Color = clYellow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label13: TLabel
      Left = 48
      Top = 56
      Width = 38
      Height = 13
      Caption = 'Label13'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object Label4: TLabel
      Left = 230
      Top = 8
      Width = 62
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'samplerate'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object Label1: TLabel
      Left = 232
      Top = 24
      Width = 73
      Height = 17
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'bitrate'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object Label6: TLabel
      Left = 232
      Top = 56
      Width = 57
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Total time'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object Label7: TLabel
      Left = 296
      Top = 56
      Width = 39
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Label7'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object Label8: TLabel
      Left = 232
      Top = 40
      Width = 31
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'mono'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 79
    Width = 349
    Height = 11
    Align = alBottom
    Min = 0
    Max = 100
    TabOrder = 5
  end
  object ForwardButton: TButton
    Left = 224
    Top = 8
    Width = 33
    Height = 25
    Caption = '>>'
    TabOrder = 6
    OnClick = ForwardButtonClick
  end
  object BackwardButton: TButton
    Left = 272
    Top = 8
    Width = 33
    Height = 25
    Caption = '<<'
    TabOrder = 7
    OnClick = BackwardButtonClick
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 48
    Width = 81
    Height = 17
    Caption = 'Loop'
    TabOrder = 8
    OnClick = CheckBox1Click
  end
  object OpenDialog1: TOpenDialog
    Filter = 'MP3 Files|*.mp3'
    Left = 80
    Top = 152
  end
  object MP3In1: TMP3In
    Loop = False
    EndSample = -1
    HighPrecision = False
    OutputChannels = cnMonoOrStereo
    Left = 8
    Top = 152
  end
  object DXAudioOut1: TDXAudioOut
    Input = MP3In1
    OnDone = AudioOut1Done
    OnProgress = AudioOut1Progress
    DeviceNumber = 0
    FramesInBuffer = 24576
    Latency = 80
    PollingInterval = 100
    PrefetchData = True
    Left = 48
    Top = 152
  end
end
