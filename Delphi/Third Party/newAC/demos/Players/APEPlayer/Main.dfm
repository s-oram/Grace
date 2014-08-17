object Form1: TForm1
  Left = 488
  Top = 378
  Caption = 'Monkey'#39's Audio Player'
  ClientHeight = 182
  ClientWidth = 326
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
  object Label4: TLabel
    Left = 168
    Top = 8
    Width = 3
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 232
    Top = 8
    Width = 46
    Height = 13
    Caption = 'Total time'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label7: TLabel
    Left = 280
    Top = 8
    Width = 3
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label8: TLabel
    Left = 96
    Top = 8
    Width = 3
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label1: TLabel
    Left = 8
    Top = 72
    Width = 23
    Height = 13
    Caption = 'Artist'
  end
  object Label2: TLabel
    Left = 8
    Top = 88
    Width = 29
    Height = 13
    Caption = 'Album'
  end
  object Label3: TLabel
    Left = 8
    Top = 104
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 40
    Width = 65
    Height = 25
    Caption = 'Play...'
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
    Left = 152
    Top = 40
    Width = 49
    Height = 25
    Caption = 'Stop'
    NumGlyphs = 2
    TabOrder = 2
    OnClick = BitBtn3Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 163
    Width = 326
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 149
    Width = 326
    Height = 14
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 138
    Width = 326
    Height = 11
    Align = alBottom
    TabOrder = 5
  end
  object ForwardButton: TButton
    Left = 216
    Top = 40
    Width = 33
    Height = 25
    Caption = '>>'
    TabOrder = 6
    OnClick = ForwardButtonClick
  end
  object BackwardButton: TButton
    Left = 256
    Top = 40
    Width = 33
    Height = 25
    Caption = '<<'
    TabOrder = 7
    OnClick = BackwardButtonClick
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 8
    Width = 73
    Height = 17
    Caption = 'Loop'
    TabOrder = 8
    OnClick = CheckBox1Click
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Monkey'#39's Audio Files|*.ape'
    Left = 80
    Top = 120
  end
  object MACIn1: TMACIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 8
    Top = 120
  end
  object DXAudioOut1: TDXAudioOut
    Input = MACIn1
    OnDone = AudioOut1Done
    OnProgress = AudioOut1Progress
    OnThreadException = DXAudioOut1ThreadException
    DeviceNumber = 0
    Latency = 80
    PrefetchData = True
    PollingInterval = 200
    FramesInBuffer = 65536
    SpeedFactor = 1.000000000000000000
    Left = 48
    Top = 120
  end
end
