object Form1: TForm1
  Left = 192
  Top = 109
  Caption = 'Wav2Ogg Converter'
  ClientHeight = 137
  ClientWidth = 247
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 8
    Top = 51
    Width = 30
    Height = 13
    Caption = 'Bitrate'
  end
  object Button1: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 82
    Width = 247
    Height = 17
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 79
  end
  object ComboBox1: TComboBox
    Left = 44
    Top = 48
    Width = 81
    Height = 21
    Style = csDropDownList
    ItemIndex = 4
    TabOrder = 2
    Text = '192'
    OnSelect = ComboBox1Select
    Items.Strings = (
      '24'
      '32'
      '64'
      '128'
      '192'
      '256'
      '320'
      '499')
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 118
    Width = 247
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Select...'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Panel1: TPanel
    Left = 0
    Top = 99
    Width = 247
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 5
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 168
    Top = 48
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 104
    Top = 48
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ogg'
    Left = 136
    Top = 48
  end
  object VorbisOut1: TVorbisOut
    Input = WaveIn1
    OnDone = VorbisOut1Done
    OnProgress = VorbisOut1Progress
    OnThreadException = VorbisOut1ThreadException
    ShareMode = 0
    Compression = 0.200000002980232200
    DesiredMaximumBitrate = brAutoSelect
    DesiredNominalBitrate = brAutoSelect
    FileMode = foRewrite
    MinimumBitrate = brAutoSelect
    Serial = 0
    Left = 200
    Top = 48
  end
end
