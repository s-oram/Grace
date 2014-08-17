object Form1: TForm1
  Left = 192
  Top = 114
  Caption = 'mp3 -> wma converter'
  ClientHeight = 112
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
  object Label1: TLabel
    Left = 104
    Top = 23
    Width = 69
    Height = 13
    Caption = 'Desired Bitrate'
  end
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
    Top = 57
    Width = 391
    Height = 17
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 114
  end
  object Edit1: TEdit
    Left = 184
    Top = 20
    Width = 73
    Height = 21
    TabOrder = 2
    Text = '256'
  end
  object Panel1: TPanel
    Left = 0
    Top = 74
    Width = 391
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitLeft = -55
    ExplicitTop = 112
    ExplicitWidth = 446
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 93
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
  object WMAOut1: TWMAOut
    Input = MP3In1
    OnDone = WMAOut1Done
    OnProgress = WMAOut1Progress
    ShareMode = 0
    DesiredBitrate = 0
    Lossless = False
    VBR = False
    VBRQuality = 0
    Left = 304
    Top = 16
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'mp3'
    Filter = 'mp3|*.mp3'
    Left = 336
    Top = 16
  end
  object MP3In1: TMP3In
    Loop = False
    EndSample = -1
    StartSample = 0
    HighPrecision = False
    OutputChannels = cnMonoOrStereo
    Left = 272
    Top = 16
  end
end
