object Form1: TForm1
  Left = 372
  Top = 218
  Caption = 'Wav2Ogg Converter'
  ClientHeight = 279
  ClientWidth = 268
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
  object Label4: TLabel
    Left = 11
    Top = 50
    Width = 32
    Height = 13
    Caption = 'Quality'
  end
  object Label6: TLabel
    Left = 11
    Top = 75
    Width = 29
    Height = 13
    Caption = 'Album'
  end
  object Label7: TLabel
    Left = 11
    Top = 99
    Width = 23
    Height = 13
    Caption = 'Artist'
  end
  object Label8: TLabel
    Left = 11
    Top = 123
    Width = 23
    Height = 13
    Caption = 'Date'
  end
  object Label9: TLabel
    Left = 11
    Top = 147
    Width = 29
    Height = 13
    Caption = 'Genre'
  end
  object Label10: TLabel
    Left = 11
    Top = 171
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object Label11: TLabel
    Left = 11
    Top = 195
    Width = 28
    Height = 13
    Caption = 'Track'
  end
  object Button1: TButton
    Left = 104
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 224
    Width = 268
    Height = 17
    Align = alBottom
    TabOrder = 1
    ExplicitTop = 223
  end
  object SpinEdit1: TSpinEdit
    Left = 59
    Top = 47
    Width = 33
    Height = 22
    MaxValue = 10
    MinValue = 1
    TabOrder = 2
    Value = 2
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 260
    Width = 268
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 256
  end
  object AlbumEdit: TEdit
    Left = 59
    Top = 72
    Width = 201
    Height = 21
    TabOrder = 4
  end
  object ArtistEdit: TEdit
    Left = 59
    Top = 96
    Width = 201
    Height = 21
    TabOrder = 5
  end
  object DateEdit: TEdit
    Left = 59
    Top = 120
    Width = 201
    Height = 21
    TabOrder = 6
  end
  object GenreEdit: TEdit
    Left = 59
    Top = 144
    Width = 201
    Height = 21
    TabOrder = 7
  end
  object TitleEdit: TEdit
    Left = 59
    Top = 168
    Width = 201
    Height = 21
    TabOrder = 8
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Select...'
    TabOrder = 9
    OnClick = Button2Click
  end
  object TrackEdit: TEdit
    Left = 59
    Top = 192
    Width = 97
    Height = 21
    TabOrder = 10
  end
  object Panel1: TPanel
    Left = 0
    Top = 241
    Width = 268
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 11
    ExplicitTop = 237
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 179
    Top = 208
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 115
    Top = 208
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ogg'
    Left = 147
    Top = 208
  end
  object VorbisOut1: TVorbisOut
    Input = WaveIn1
    OnDone = VorbisOut1Done
    OnProgress = VorbisOut1Progress
    OnThreadException = VorbisOut1ThreadException
    ShareMode = 0
    Compression = 0.400000005960464400
    DesiredMaximumBitrate = brAutoSelect
    DesiredNominalBitrate = brAutoSelect
    FileMode = foRewrite
    MinimumBitrate = brAutoSelect
    Serial = 0
    Left = 211
    Top = 208
  end
end
