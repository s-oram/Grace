object Form1: TForm1
  Left = 395
  Top = 226
  Caption = 'DTS2WMA Converter'
  ClientHeight = 383
  ClientWidth = 467
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
  DesignSize = (
    467
    383)
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel
    Left = 8
    Top = 176
    Width = 29
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Album'
  end
  object Label7: TLabel
    Left = 8
    Top = 200
    Width = 23
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Artist'
  end
  object Label8: TLabel
    Left = 8
    Top = 224
    Width = 23
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Date'
  end
  object Label9: TLabel
    Left = 8
    Top = 248
    Width = 29
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Genre'
  end
  object Label10: TLabel
    Left = 8
    Top = 272
    Width = 20
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Title'
  end
  object Label11: TLabel
    Left = 8
    Top = 296
    Width = 28
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Track'
  end
  object Label1: TLabel
    Left = 8
    Top = 40
    Width = 36
    Height = 13
    Caption = 'Codecs'
  end
  object Label2: TLabel
    Left = 272
    Top = 40
    Width = 37
    Height = 13
    Caption = 'Formats'
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
    Top = 327
    Width = 467
    Height = 18
    Align = alBottom
    TabOrder = 1
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 364
    Width = 467
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 345
    Width = 467
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
  end
  object AlbumEdit: TEdit
    Left = 56
    Top = 176
    Width = 201
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 4
  end
  object ArtistEdit: TEdit
    Left = 56
    Top = 200
    Width = 201
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 5
  end
  object DateEdit: TEdit
    Left = 56
    Top = 224
    Width = 201
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 6
  end
  object GenreEdit: TEdit
    Left = 56
    Top = 248
    Width = 201
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 7
  end
  object TitleEdit: TEdit
    Left = 56
    Top = 272
    Width = 201
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 8
  end
  object TrackSpinEdit: TSpinEdit
    Left = 56
    Top = 296
    Width = 49
    Height = 22
    Anchors = [akLeft, akBottom]
    MaxValue = 99
    MinValue = 0
    TabOrder = 9
    Value = 0
  end
  object Button2: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 10
    OnClick = Button2Click
  end
  object VBR: TCheckBox
    Left = 169
    Top = 12
    Width = 65
    Height = 17
    Caption = 'VBR'
    TabOrder = 11
    OnClick = VBRClick
  end
  object ListBox1: TListBox
    Left = 8
    Top = 64
    Width = 249
    Height = 97
    ItemHeight = 13
    TabOrder = 12
    OnClick = ListBox1Click
  end
  object ListBox2: TListBox
    Left = 272
    Top = 64
    Width = 185
    Height = 257
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 13
  end
  object OpenDialog1: TOpenDialog
    Filter = 'DTS files|*.dts|VOB files|*.vob'
    Left = 8
    Top = 320
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'wma'
    Left = 40
    Top = 320
  end
  object WMAOut1: TWMAOut
    Input = DTSIn1
    OnDone = WMAOut1Done
    OnProgress = WMAOut1Progress
    ShareMode = 0
    DesiredBitrate = 0
    Lossless = False
    VBR = False
    VBRQuality = 0
    Left = 104
    Top = 320
  end
  object DTSIn1: TDTSIn
    Loop = False
    OutputChannels = dtsStereo
    Left = 72
    Top = 320
  end
end
