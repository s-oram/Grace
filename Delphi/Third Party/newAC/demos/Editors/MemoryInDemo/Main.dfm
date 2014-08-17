object Form1: TForm1
  Left = 192
  Top = 114
  Caption = 'Sine Wave Generator'
  ClientHeight = 97
  ClientWidth = 309
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
  object Label1: TLabel
    Left = 16
    Top = 27
    Width = 50
    Height = 13
    Caption = 'Frequency'
  end
  object Label2: TLabel
    Left = 148
    Top = 27
    Width = 40
    Height = 13
    Caption = 'Duration'
  end
  object SpinEdit1: TSpinEdit
    Left = 72
    Top = 24
    Width = 57
    Height = 22
    Increment = 50
    MaxValue = 4000
    MinValue = 50
    TabOrder = 0
    Value = 400
  end
  object SpinEdit2: TSpinEdit
    Left = 194
    Top = 24
    Width = 57
    Height = 22
    MaxValue = 10000
    MinValue = 1
    TabOrder = 1
    Value = 1
  end
  object Button1: TButton
    Left = 72
    Top = 64
    Width = 121
    Height = 25
    Caption = 'Play'
    TabOrder = 2
    OnClick = Button1Click
  end
  object DXAudioOut1: TDXAudioOut
    Input = MemoryIn1
    OnDone = DXAudioOut1Done
    DeviceNumber = 0
    Latency = 100
    PrefetchData = True
    PollingInterval = 100
    FramesInBuffer = 24576
    SpeedFactor = 1.000000000000000000
    Left = 136
  end
  object MemoryIn1: TMemoryIn
    InBitsPerSample = 16
    InChannels = 1
    InSampleRate = 8000
    RepeatCount = 1
    Left = 184
  end
end
