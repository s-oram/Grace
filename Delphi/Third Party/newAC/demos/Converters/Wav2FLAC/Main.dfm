object Form1: TForm1
  Left = 207
  Top = 205
  Caption = 'Wav2FLAC Converter'
  ClientHeight = 256
  ClientWidth = 305
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
    305
    256)
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 184
    Top = 14
    Width = 60
    Height = 13
    Caption = 'Compression'
  end
  object Label1: TLabel
    Left = 16
    Top = 51
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object Label2: TLabel
    Left = 16
    Top = 75
    Width = 29
    Height = 13
    Caption = 'Album'
  end
  object Label3: TLabel
    Left = 16
    Top = 99
    Width = 23
    Height = 13
    Caption = 'Artist'
  end
  object Label5: TLabel
    Left = 16
    Top = 123
    Width = 23
    Height = 13
    Caption = 'Date'
  end
  object Label6: TLabel
    Left = 16
    Top = 147
    Width = 29
    Height = 13
    Caption = 'Genre'
  end
  object Label7: TLabel
    Left = 16
    Top = 171
    Width = 28
    Height = 13
    Caption = 'Track'
  end
  object Button1: TButton
    Left = 96
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object SpinEdit1: TSpinEdit
    Left = 256
    Top = 11
    Width = 41
    Height = 22
    MaxValue = 8
    MinValue = 0
    TabOrder = 1
    Value = 2
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Select...'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 64
    Top = 48
    Width = 161
    Height = 21
    TabOrder = 3
  end
  object Edit2: TEdit
    Left = 64
    Top = 72
    Width = 161
    Height = 21
    TabOrder = 4
  end
  object Edit3: TEdit
    Left = 64
    Top = 96
    Width = 161
    Height = 21
    TabOrder = 5
  end
  object Edit4: TEdit
    Left = 64
    Top = 120
    Width = 161
    Height = 21
    TabOrder = 6
  end
  object Edit5: TEdit
    Left = 64
    Top = 144
    Width = 161
    Height = 21
    TabOrder = 7
  end
  object Edit6: TEdit
    Left = 64
    Top = 168
    Width = 73
    Height = 21
    TabOrder = 8
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 237
    Width = 305
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitLeft = 8
    ExplicitTop = 195
  end
  object Panel2: TPanel
    Left = 0
    Top = 218
    Width = 305
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 10
    ExplicitTop = 233
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 203
    Width = 305
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Step = 2
    TabOrder = 11
    ExplicitTop = 243
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 232
    Top = 48
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 168
    Top = 48
  end
  object SaveDialog1: TSaveDialog
    Left = 200
    Top = 48
  end
  object FLACOut1: TFLACOut
    Input = WaveIn1
    OnDone = FLACOut1Done
    OnProgress = FLACOut1Progress
    OnThreadException = FLACOut1ThreadException
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
    Left = 264
    Top = 48
  end
end
