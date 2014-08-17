object Form1: TForm1
  Left = 395
  Top = 226
  Caption = 'AC3 -> WMA Converter'
  ClientHeight = 492
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
    492)
  PixelsPerInch = 96
  TextHeight = 13
  object Label6: TLabel
    Left = 8
    Top = 317
    Width = 29
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Album'
    ExplicitTop = 324
  end
  object Label7: TLabel
    Left = 8
    Top = 341
    Width = 23
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Artist'
    ExplicitTop = 348
  end
  object Label8: TLabel
    Left = 8
    Top = 365
    Width = 23
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Date'
    ExplicitTop = 372
  end
  object Label9: TLabel
    Left = 8
    Top = 389
    Width = 29
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Genre'
    ExplicitTop = 396
  end
  object Label10: TLabel
    Left = 8
    Top = 413
    Width = 20
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Title'
    ExplicitTop = 420
  end
  object Label11: TLabel
    Left = 8
    Top = 437
    Width = 28
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Track'
    ExplicitTop = 444
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
  object StatusBar1: TStatusBar
    Left = 0
    Top = 473
    Width = 467
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 499
  end
  object AlbumEdit: TEdit
    Left = 56
    Top = 317
    Width = 201
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    ExplicitTop = 343
  end
  object ArtistEdit: TEdit
    Left = 56
    Top = 341
    Width = 201
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    ExplicitTop = 367
  end
  object DateEdit: TEdit
    Left = 56
    Top = 365
    Width = 201
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 3
    ExplicitTop = 391
  end
  object GenreEdit: TEdit
    Left = 56
    Top = 389
    Width = 201
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 4
    ExplicitTop = 415
  end
  object TitleEdit: TEdit
    Left = 56
    Top = 413
    Width = 201
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 5
    ExplicitTop = 439
  end
  object TrackSpinEdit: TSpinEdit
    Left = 56
    Top = 437
    Width = 49
    Height = 22
    Anchors = [akLeft, akBottom]
    MaxValue = 99
    MinValue = 0
    TabOrder = 6
    Value = 0
    ExplicitTop = 463
  end
  object Button2: TButton
    Left = 88
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 7
    OnClick = Button2Click
  end
  object VBR: TCheckBox
    Left = 169
    Top = 12
    Width = 65
    Height = 17
    Caption = 'VBR'
    TabOrder = 8
    OnClick = VBRClick
  end
  object ListBox1: TListBox
    Left = 8
    Top = 64
    Width = 249
    Height = 97
    ItemHeight = 13
    TabOrder = 9
    OnClick = ListBox1Click
  end
  object ListBox2: TListBox
    Left = 272
    Top = 64
    Width = 185
    Height = 386
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 10
    ExplicitHeight = 412
  end
  object ListBox3: TListBox
    Left = 8
    Top = 176
    Width = 249
    Height = 124
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 11
    OnClick = ListBox1Click
    ExplicitHeight = 150
  end
  object Button3: TButton
    Left = 7
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Add File...'
    TabOrder = 12
    OnClick = Button3Click
  end
  object OpenDialog1: TOpenDialog
    Filter = 'DTS Files|*.wav|DTS Files|*.dts|Vob files|*.vob'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 40
    Top = 256
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'wma'
    Left = 72
    Top = 256
  end
  object WMAOut1: TWMAOut
    Input = AudioPlayList1
    OnDone = WMAOut1Done
    OnThreadException = WMAOut1ThreadException
    FileName = 'aqua.wma'
    ShareMode = 0
    Id3v2Tags.Album = 'Aqua cosmos'
    DesiredBitrate = 0
    Lossless = False
    VBR = False
    VBRQuality = 0
    Left = 168
    Top = 256
  end
  object AC3In1: TAC3In
    Loop = False
    VobAudioSubstream = acvStreamFirst
    OutputChannels = acc5dot1
    Left = 104
    Top = 256
  end
  object AudioPlayList1: TAudioPlayList
    Input = AC3In1
    CurrentItem = 0
    Left = 136
    Top = 256
  end
end
