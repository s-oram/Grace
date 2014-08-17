object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Cue-sheet Splitter'
  ClientHeight = 195
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    446
    195)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Select...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 39
    Width = 430
    Height = 148
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    TabOrder = 1
  end
  object MACIn1: TMACIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 32
    Top = 56
  end
  object FLACIn1: TFLACIn
    Loop = False
    CheckMD5Signature = False
    EndSample = -1
    StartSample = 0
    Left = 88
    Top = 56
  end
  object WVIn1: TWVIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 144
    Top = 56
  end
  object CueSplitter1: TCueSplitter
    Left = 200
    Top = 56
  end
  object MACOut1: TMACOut
    Input = CueSplitter1
    OnDone = MACOut1Done
    ShareMode = 0
    CompressionLevel = 2000
    MaxAudioBytes = -1
    Left = 32
    Top = 112
  end
  object FLACOut1: TFLACOut
    Input = CueSplitter1
    OnDone = FLACOut1Done
    ShareMode = 0
    BestModelSearch = False
    BlockSize = 4608
    CompressionLevel = -1
    EnableMidSideStereo = True
    EnableLooseMidSideStereo = False
    MaxLPCOrder = 0
    MaxResidualPartitionOrder = 0
    MinResidualPartitionOrder = 0
    QLPCoeffPrecision = 0
    QLPCoeffPrecisionSearch = False
    Verify = False
    Left = 88
    Top = 112
  end
  object WVOut1: TWVOut
    Input = CueSplitter1
    OnDone = WVOut1Done
    ShareMode = 0
    Left = 144
    Top = 112
  end
  object OpenDialog1: TOpenDialog
    Filter = 'WavPack files|*.wv|Monkey Audio files|*.ape|FLAC files|*.flac'
    Left = 200
    Top = 112
  end
  object OpenDialog2: TOpenDialog
    DefaultExt = 'cue'
    Filter = 'Cue sheets|*.cue'
    Left = 256
    Top = 112
  end
end
