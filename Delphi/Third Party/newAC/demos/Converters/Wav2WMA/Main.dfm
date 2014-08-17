object Form1: TForm1
  Left = 395
  Top = 226
  Caption = 'Wav2WMA Converter'
  ClientHeight = 273
  ClientWidth = 296
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel
    Left = 8
    Top = 66
    Width = 29
    Height = 13
    Caption = 'Album'
  end
  object Label7: TLabel
    Left = 8
    Top = 90
    Width = 23
    Height = 13
    Caption = 'Artist'
  end
  object Label8: TLabel
    Left = 8
    Top = 114
    Width = 23
    Height = 13
    Caption = 'Date'
  end
  object Label9: TLabel
    Left = 8
    Top = 138
    Width = 29
    Height = 13
    Caption = 'Genre'
  end
  object Label10: TLabel
    Left = 8
    Top = 162
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object Label11: TLabel
    Left = 8
    Top = 186
    Width = 28
    Height = 13
    Caption = 'Track'
  end
  object Label1: TLabel
    Left = 8
    Top = 42
    Width = 69
    Height = 13
    Caption = 'Desired Bitrate'
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
    Top = 218
    Width = 296
    Height = 17
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 214
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 254
    Width = 296
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 245
  end
  object Panel1: TPanel
    Left = 0
    Top = 235
    Width = 296
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitTop = 226
  end
  object AlbumEdit: TEdit
    Left = 83
    Top = 63
    Width = 174
    Height = 21
    TabOrder = 4
  end
  object ArtistEdit: TEdit
    Left = 83
    Top = 87
    Width = 174
    Height = 21
    TabOrder = 5
  end
  object DateEdit: TEdit
    Left = 83
    Top = 111
    Width = 174
    Height = 21
    TabOrder = 6
  end
  object GenreEdit: TEdit
    Left = 83
    Top = 135
    Width = 174
    Height = 21
    TabOrder = 7
  end
  object TitleEdit: TEdit
    Left = 83
    Top = 159
    Width = 174
    Height = 21
    TabOrder = 8
  end
  object TrackSpinEdit: TSpinEdit
    Left = 83
    Top = 183
    Width = 49
    Height = 22
    MaxValue = 99
    MinValue = 0
    TabOrder = 9
    Value = 0
  end
  object Edit1: TEdit
    Left = 83
    Top = 39
    Width = 174
    Height = 21
    TabOrder = 10
    Text = '128'
  end
  object Button2: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 11
    OnClick = Button2Click
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 192
    Top = 175
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 128
    Top = 175
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'wma'
    Left = 160
    Top = 175
  end
  object WMAOut1: TWMAOut
    Input = WaveIn1
    OnDone = WMAOut1Done
    OnProgress = WMAOut1Progress
    OnThreadException = WMAOut1ThreadException
    ShareMode = 0
    DesiredBitrate = 0
    Lossless = False
    VBR = False
    VBRQuality = 0
    Left = 224
    Top = 175
  end
end
