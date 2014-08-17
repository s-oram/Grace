object Form1: TForm1
  Left = 192
  Top = 109
  Caption = 'Wav2Ape'
  ClientHeight = 227
  ClientWidth = 281
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
    281
    227)
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 120
    Top = 16
    Width = 60
    Height = 13
    Caption = 'Compression'
  end
  object Label5: TLabel
    Left = 8
    Top = 51
    Width = 23
    Height = 13
    Caption = 'Artist'
  end
  object Label6: TLabel
    Left = 8
    Top = 75
    Width = 29
    Height = 13
    Caption = 'Album'
  end
  object Label7: TLabel
    Left = 8
    Top = 123
    Width = 29
    Height = 13
    Caption = 'Genre'
  end
  object Label8: TLabel
    Left = 8
    Top = 99
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object Label9: TLabel
    Left = 8
    Top = 147
    Width = 23
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Year'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 175
    Width = 281
    Height = 14
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 1
    ExplicitTop = 191
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 208
    Width = 281
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 200
    ExplicitWidth = 280
  end
  object SpinEdit1: TSpinEdit
    Left = 192
    Top = 8
    Width = 57
    Height = 22
    Increment = 1000
    MaxValue = 5000
    MinValue = 1000
    TabOrder = 3
    Value = 2000
  end
  object Edit1: TEdit
    Left = 56
    Top = 48
    Width = 213
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 4
    ExplicitWidth = 212
  end
  object Edit2: TEdit
    Left = 56
    Top = 72
    Width = 213
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    ExplicitWidth = 212
  end
  object Edit3: TEdit
    Left = 56
    Top = 96
    Width = 213
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    ExplicitWidth = 212
  end
  object Edit4: TEdit
    Left = 56
    Top = 120
    Width = 213
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    ExplicitWidth = 212
  end
  object Edit5: TEdit
    Left = 56
    Top = 144
    Width = 213
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
    ExplicitWidth = 212
  end
  object Panel2: TPanel
    Left = 0
    Top = 189
    Width = 281
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 9
    ExplicitLeft = 8
    ExplicitTop = 191
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 168
    Top = 56
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 104
    Top = 56
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ape'
    Left = 136
    Top = 56
  end
  object MACOut1: TMACOut
    Input = WaveIn1
    OnDone = MACOut1Done
    OnProgress = MACOut1Progress
    OnThreadException = MACOut1ThreadException
    ShareMode = 0
    CompressionLevel = 2000
    MaxAudioBytes = -1
    Left = 208
    Top = 56
  end
end
