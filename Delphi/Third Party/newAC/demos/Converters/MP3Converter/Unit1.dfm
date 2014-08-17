object Form1: TForm1
  Left = 192
  Top = 114
  Caption = 'mp3 -> wav converter'
  ClientHeight = 109
  ClientWidth = 391
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 54
    Width = 391
    Height = 17
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 90
  end
  object Panel1: TPanel
    Left = 0
    Top = 71
    Width = 391
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = -55
    ExplicitTop = 112
    ExplicitWidth = 446
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 90
    Width = 391
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitLeft = -55
    ExplicitTop = 112
    ExplicitWidth = 446
  end
  object MP3In1: TMP3In
    Loop = False
    EndSample = -1
    StartSample = 0
    HighPrecision = False
    OutputChannels = cnMonoOrStereo
    Left = 160
    Top = 8
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'mp3'
    Filter = 'mp3|*.mp3'
    Left = 224
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'wav'
    Filter = 'Wav files|*.wav'
    Left = 264
    Top = 8
  end
  object WaveOut1: TWaveOut
    Input = MP3In1
    OnDone = WaveOut1Done
    OnProgress = WaveOut1Progress
    ShareMode = 0
    WavType = wtPCM
    BlockSize = 512
    CreateNonMsHeaders = True
    FileMode = foRewrite
    Left = 192
    Top = 8
  end
end
