object Form1: TForm1
  Left = 196
  Top = 150
  Width = 397
  Height = 180
  Caption = 'Speach Client'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 56
    Height = 13
    Caption = 'Receiver IP'
  end
  object Button1: TButton
    Left = 16
    Top = 64
    Width = 81
    Height = 25
    Caption = 'Start recording'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 104
    Top = 64
    Width = 81
    Height = 25
    Caption = 'Stop recording'
    Enabled = False
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 280
    Top = 64
    Width = 81
    Height = 25
    Caption = 'Send Message'
    Enabled = False
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 192
    Top = 64
    Width = 81
    Height = 25
    Caption = 'Test Message'
    Enabled = False
    TabOrder = 3
    OnClick = Button4Click
  end
  object Edit1: TEdit
    Left = 80
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 4
    Text = '127.0.0.1'
  end
  object AudioIn1: TAudioIn
    BaseChannel = 0
    InBitsPerSample = 8
    InChannels = 1
    InSampleRate = 8000
    RecTime = 6000
    Left = 24
    Top = 104
  end
  object WaveOut1: TWaveOut
    Input = AudioIn1
    SuspendWhenIdle = True
    OnDone = WaveOut1Done
    Left = 56
    Top = 104
  end
  object WaveIn1: TWaveIn
    Loop = False
    Left = 120
    Top = 104
  end
  object AudioOut1: TAudioOut
    Input = WaveIn1
    SuspendWhenIdle = True
    OnDone = AudioOut1Done
    BaseChannel = 0
    Volume = 255
    Left = 152
    Top = 104
  end
  object IdTrivialFTP1: TIdTrivialFTP
    Host = '127.0.0.1'
    Port = 69
    ReceiveTimeout = 4000
    OnWorkEnd = IdTrivialFTP1WorkEnd
    Left = 88
    Top = 104
  end
end
