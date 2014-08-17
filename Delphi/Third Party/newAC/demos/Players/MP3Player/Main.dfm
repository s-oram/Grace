object Form1: TForm1
  Left = 246
  Top = 190
  Caption = 'MP3 Player'
  ClientHeight = 190
  ClientWidth = 349
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
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
    NumGlyphs = 2
    TabOrder = 1
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 160
    Top = 8
    Width = 49
    Height = 25
    Caption = 'Stop'
    NumGlyphs = 2
    TabOrder = 2
    OnClick = BitBtn3Click
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
      Width = 3
      Height = 13
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
      Left = 43
      Top = 21
      Width = 3
      Height = 13
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
      Width = 3
      Height = 13
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
      Width = 3
      Height = 13
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
      Left = 232
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
    end
    object Label1: TLabel
      Left = 232
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
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object Label7: TLabel
      Left = 296
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
      Font.Color = clBlack
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
    Left = 240
    Top = 40
  end
  object MP3In1: TMP3In
    Loop = False
    EndSample = -1
    StartSample = 0
    HighPrecision = False
    OutputChannels = cnMonoOrStereo
    Left = 168
    Top = 40
  end
  object DXAudioOut1: TDXAudioOut
    Input = MP3In1
    OnDone = AudioOut1Done
    OnProgress = AudioOut1Progress
    DeviceNumber = 0
    Latency = 80
    PrefetchData = True
    PollingInterval = 100
    FramesInBuffer = 24576
    SpeedFactor = 1.000000000000000000
    Left = 208
    Top = 40
  end
end
