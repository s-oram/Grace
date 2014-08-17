object Form1: TForm1
  Left = 192
  Top = 114
  Caption = 'WMA to Ogg converter'
  ClientHeight = 283
  ClientWidth = 305
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    305
    283)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 214
    Top = 56
    Width = 55
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Ogg Quality'
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 64
    Height = 13
    Caption = 'Input Formats'
  end
  object ListBox1: TListBox
    Left = 8
    Top = 75
    Width = 191
    Height = 147
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object SpinEdit1: TSpinEdit
    Left = 214
    Top = 80
    Width = 65
    Height = 22
    Anchors = [akTop, akRight]
    MaxValue = 10
    MinValue = 1
    TabOrder = 1
    Value = 5
  end
  object Button1: TButton
    Left = 8
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Select...'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 89
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 3
    OnClick = Button2Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 264
    Width = 305
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 249
  end
  object CheckBox1: TCheckBox
    Left = 214
    Top = 120
    Width = 89
    Height = 17
    Anchors = [akTop, akRight]
    Caption = 'High Precision'
    TabOrder = 5
    OnClick = CheckBox1Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 245
    Width = 305
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 6
    ExplicitTop = 230
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 228
    Width = 305
    Height = 17
    Align = alBottom
    TabOrder = 7
    ExplicitTop = 218
  end
  object OpenDialog1: TOpenDialog
    Filter = 'WMA files|*.wma'
    Left = 32
    Top = 160
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ogg'
    Filter = 'Ogg Vorbis Files|*.ogg'
    Left = 64
    Top = 160
  end
  object WMIn1: TWMIn
    Loop = False
    EndSample = -1
    StartSample = 0
    HighPrecision = False
    OutputChannels = cnMonoOrStereo
    Left = 104
    Top = 160
  end
  object VorbisOut1: TVorbisOut
    Input = WMIn1
    OnDone = VorbisOut1Done
    OnProgress = VorbisOut1Progress
    ShareMode = 0
    Compression = 0.200000002980232200
    DesiredMaximumBitrate = brAutoSelect
    DesiredNominalBitrate = brAutoSelect
    FileMode = foRewrite
    MinimumBitrate = brAutoSelect
    Serial = 0
    Left = 144
    Top = 160
  end
end
