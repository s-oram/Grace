object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'AudioProcessor Demo'
  ClientHeight = 49
  ClientWidth = 240
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 120
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 1
    OnClick = Button2Click
  end
  object AudioProcessor1: TAudioProcessor
    Input = WaveIn1
    OnFlush = AudioProcessor1Flush
    OnGetData = AudioProcessor1GetData
    OnInit = AudioProcessor1Init
    Left = 40
    Top = 8
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 8
    Top = 8
  end
  object DXAudioOut1: TDXAudioOut
    Input = AudioProcessor1
    OnDone = DXAudioOut1Done
    DeviceNumber = 0
    Latency = 80
    PrefetchData = True
    PollingInterval = 100
    FramesInBuffer = 32768
    SpeedFactor = 1.000000000000000000
    Left = 80
    Top = 8
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'wav'
    Filter = 'Wave Files|*.wav'
    Left = 120
    Top = 8
  end
end
