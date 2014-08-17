object Form1: TForm1
  Left = 505
  Top = 393
  Caption = 'DTS Player'
  ClientHeight = 263
  ClientWidth = 341
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  DesignSize = (
    341
    263)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 73
    Width = 113
    Height = 13
    AutoSize = False
    Caption = 'bitrate'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Label4: TLabel
    Left = 8
    Top = 54
    Width = 113
    Height = 13
    Caption = 'samplerate'
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
    Left = 8
    Top = 92
    Width = 113
    Height = 13
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
  object BitBtn1: TBitBtn
    Left = 89
    Top = 8
    Width = 65
    Height = 25
    Caption = 'Play'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 160
    Top = 8
    Width = 49
    Height = 25
    Caption = 'Pause'
    NumGlyphs = 2
    TabOrder = 1
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 215
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
    Top = 244
    Width = 341
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Add...'
    TabOrder = 4
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 8
    Top = 115
    Width = 325
    Height = 123
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ItemHeight = 13
    ParentFont = False
    TabOrder = 5
    OnClick = ListBox1Click
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'dts'
    Filter = 'DTS-wav files|*.wav|DTS-dts files|*.dts|VOB files|*.vob'
    Left = 224
    Top = 192
  end
  object DTSIn1: TDTSIn
    Loop = False
    OutputChannels = dts5dot1
    Left = 16
    Top = 192
  end
  object AudioPlayList1: TAudioPlayList
    Input = DTSIn1
    CurrentItem = 0
    OnPlayItemChanged = AudioPlayList1PlayItemChanged
    Left = 80
    Top = 192
  end
  object DXAudioOut1: TDXAudioOut
    Input = AudioPlayList1
    OnDone = AudioOut1Done
    DeviceNumber = 0
    Latency = 100
    PrefetchData = True
    PollingInterval = 100
    FramesInBuffer = 32768
    SpeedFactor = 1.000000000000000000
    Left = 152
    Top = 192
  end
end
