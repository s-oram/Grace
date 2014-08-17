object Form5: TForm5
  Left = 0
  Top = 0
  Caption = 'NetStation'
  ClientHeight = 128
  ClientWidth = 390
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  DesignSize = (
    390
    128)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 185
    Height = 41
    TabOrder = 0
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 89
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Stop'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object Panel2: TPanel
    Left = 8
    Top = 55
    Width = 374
    Height = 66
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    ExplicitWidth = 494
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 62
      Height = 13
      Align = alCustom
      Caption = 'Now Playing:'
    end
    object Label2: TLabel
      Left = 89
      Top = 8
      Width = 42
      Height = 13
      Caption = 'No Artist'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 89
      Top = 25
      Width = 36
      Height = 13
      Caption = 'No Title'
    end
    object Label4: TLabel
      Left = 89
      Top = 43
      Width = 45
      Height = 13
      Caption = 'No Album'
    end
  end
  object Panel3: TPanel
    Left = 199
    Top = 8
    Width = 183
    Height = 41
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    ExplicitWidth = 185
    object Label5: TLabel
      Left = 8
      Top = 13
      Width = 20
      Height = 13
      Caption = 'Port'
    end
    object Label6: TLabel
      Left = 87
      Top = 13
      Width = 32
      Height = 13
      Caption = 'Bitrate'
    end
    object Edit1: TEdit
      Left = 34
      Top = 10
      Width = 47
      Height = 21
      TabOrder = 0
      Text = '8886'
    end
    object Edit2: TEdit
      Left = 125
      Top = 10
      Width = 52
      Height = 21
      TabOrder = 1
      Text = '256'
    end
  end
  object MP3In1: TMP3In
    Loop = False
    EndSample = -1
    StartSample = 0
    HighPrecision = False
    OutputChannels = cnMonoOrStereo
    Left = 208
    Top = 56
  end
  object AudioPlayList1: TAudioPlayList
    Input = MP3In1
    CurrentItem = 0
    OnPlayItemChanged = AudioPlayList1PlayItemChanged
    Left = 256
    Top = 56
  end
  object WMStreamedOut1: TWMStreamedOut
    Input = AudioPlayList1
    OnDone = WMStreamedOut1Done
    ShareMode = 0
    Id3v2Tags.Artist = 'Home Radio'
    DesiredBitrate = 256
    Lossless = False
    VBR = False
    VBRQuality = 0
    MaxClients = 0
    Port = 8886
    Left = 312
    Top = 56
  end
  object OpenDialog1: TOpenDialog
    Filter = 'MP3|*.mp3'
    Left = 168
    Top = 56
  end
end
