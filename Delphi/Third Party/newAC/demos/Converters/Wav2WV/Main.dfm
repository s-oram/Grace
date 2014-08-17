object Form1: TForm1
  Left = 106
  Top = 69
  Caption = 'Wav2WavPack Converter'
  ClientHeight = 292
  ClientWidth = 246
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
    246
    292)
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 8
    Top = 137
    Width = 23
    Height = 13
    Caption = 'Artist'
  end
  object Label5: TLabel
    Left = 8
    Top = 161
    Width = 29
    Height = 13
    Caption = 'Album'
  end
  object Label6: TLabel
    Left = 8
    Top = 185
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object Label7: TLabel
    Left = 8
    Top = 209
    Width = 22
    Height = 13
    Caption = 'Year'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Select...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 237
    Width = 246
    Height = 17
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 246
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 273
    Width = 246
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 280
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 39
    Width = 105
    Height = 89
    Caption = 'Compression'
    ItemIndex = 0
    Items.Strings = (
      'Fast'
      'High'
      'Very High')
    TabOrder = 3
  end
  object CheckBox1: TCheckBox
    Left = 120
    Top = 71
    Width = 89
    Height = 17
    Caption = 'Joint Stereo'
    TabOrder = 4
  end
  object CheckBox2: TCheckBox
    Left = 120
    Top = 48
    Width = 89
    Height = 17
    Caption = 'Hybrid Mode'
    TabOrder = 5
  end
  object Button2: TButton
    Left = 96
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 6
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 48
    Top = 134
    Width = 185
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
  end
  object Edit2: TEdit
    Left = 48
    Top = 158
    Width = 185
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
  end
  object Edit3: TEdit
    Left = 48
    Top = 182
    Width = 185
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 9
  end
  object Edit4: TEdit
    Left = 48
    Top = 206
    Width = 185
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 10
  end
  object Panel1: TPanel
    Left = 0
    Top = 254
    Width = 246
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 11
    ExplicitTop = 261
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 157
    Top = 87
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 93
    Top = 87
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'wv'
    FilterIndex = 0
    Left = 125
    Top = 87
  end
  object WVOut1: TWVOut
    Input = WaveIn1
    OnDone = WVOut1Done
    OnProgress = WVOut1Progress
    OnThreadException = WVOut1ThreadException
    ShareMode = 0
    Left = 189
    Top = 87
  end
end
