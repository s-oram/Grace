object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'MP3 Converter'
  ClientHeight = 185
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
    185)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 127
    Top = 59
    Width = 17
    Height = 13
    Caption = 'bps'
  end
  object Label2: TLabel
    Left = 8
    Top = 59
    Width = 32
    Height = 13
    Caption = 'Bitrate'
  end
  object Label3: TLabel
    Left = 248
    Top = 59
    Width = 54
    Height = 13
    Caption = 'VBR quality'
  end
  object Label4: TLabel
    Left = 204
    Top = 97
    Width = 26
    Height = 13
    Caption = 'Mode'
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
  object ComboBox1: TComboBox
    Left = 49
    Top = 56
    Width = 72
    Height = 21
    Style = csDropDownList
    ItemIndex = 9
    TabOrder = 1
    Text = '128'
    Items.Strings = (
      'Auto'
      '32'
      '40'
      '48'
      '56'
      '64'
      '80'
      '96'
      '112'
      '128'
      '144'
      '160'
      '192'
      '224'
      '256'
      '320')
  end
  object CheckBox1: TCheckBox
    Left = 160
    Top = 58
    Width = 81
    Height = 17
    Caption = 'Enable VBR'
    TabOrder = 2
  end
  object ComboBox2: TComboBox
    Left = 308
    Top = 56
    Width = 56
    Height = 21
    Style = csDropDownList
    ItemIndex = 3
    TabOrder = 3
    Text = '3'
    Items.Strings = (
      '0'
      '1'
      '2'
      '3'
      '4'
      '5'
      '6'
      '7'
      '8'
      '9')
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 96
    Width = 97
    Height = 17
    Caption = 'Use bit reservoir'
    Checked = True
    State = cbChecked
    TabOrder = 4
  end
  object ComboBox3: TComboBox
    Left = 248
    Top = 94
    Width = 70
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 5
    Text = 'Stereo'
    Items.Strings = (
      'Stereo'
      'Joint Stereo'
      'Dual '
      'Mono')
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 129
    Width = 446
    Height = 16
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 6
    ExplicitTop = 166
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 166
    Width = 446
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 203
  end
  object CheckBox3: TCheckBox
    Left = 111
    Top = 96
    Width = 65
    Height = 17
    Caption = 'Strict ISO'
    TabOrder = 8
  end
  object Panel1: TPanel
    Left = 0
    Top = 147
    Width = 446
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 9
    ExplicitTop = 159
  end
  object MACIn1: TMACIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 144
    Top = 8
  end
  object FLACIn1: TFLACIn
    Loop = False
    CheckMD5Signature = False
    EndSample = -1
    StartSample = 0
    Left = 184
    Top = 8
  end
  object WVIn1: TWVIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 224
    Top = 8
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'WavPack files|*.wv|Monkey Audio files|*.ape|FLAC files|*.flac|Wa' +
      've files|*.wav'
    Left = 328
    Top = 8
  end
  object MP3Out1: TMP3Out
    OnDone = MP3Out1Done
    OnProgress = MP3Out1Progress
    ShareMode = 0
    BitRate = mbr128
    EnableBitReservoir = False
    StrictISO = False
    VBRQuality = mp3ql0
    EnableVBR = False
    AverageBitrate = mbrAuto
    MaximumBitrate = mbrAuto
    Left = 272
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'mp3'
    Filter = 'MP3 files|*.mp3'
    Left = 384
    Top = 8
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 96
    Top = 8
  end
end
