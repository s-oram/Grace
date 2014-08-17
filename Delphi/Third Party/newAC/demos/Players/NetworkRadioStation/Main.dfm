object Form1: TForm1
  Left = 246
  Top = 304
  Caption = 'Home Station'
  ClientHeight = 212
  ClientWidth = 339
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
    339
    212)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 64
    Top = 40
    Width = 53
    Height = 13
    Caption = 'Total Time:'
  end
  object Label2: TLabel
    Left = 120
    Top = 40
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 224
    Top = 8
    Width = 19
    Height = 13
    Caption = 'Port'
  end
  object Label4: TLabel
    Left = 224
    Top = 32
    Width = 30
    Height = 13
    Caption = 'Bitrate'
  end
  object PlayButton: TButton
    Left = 8
    Top = 8
    Width = 41
    Height = 25
    Caption = 'Play'
    TabOrder = 0
    OnClick = PlayButtonClick
  end
  object ProgressBar1: TProgressBar
    Left = 0
    Top = 70
    Width = 339
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Smooth = True
    Step = 2
    TabOrder = 1
  end
  object StopButton: TButton
    Left = 64
    Top = 8
    Width = 41
    Height = 25
    Caption = 'Stop'
    TabOrder = 2
    OnClick = StopButtonClick
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 38
    Width = 49
    Height = 17
    Caption = 'Loop'
    TabOrder = 3
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 193
    Width = 339
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 87
    Width = 339
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Clients:'
    TabOrder = 5
  end
  object Edit1: TEdit
    Left = 264
    Top = 8
    Width = 49
    Height = 21
    TabOrder = 6
    Text = '8088'
  end
  object Memo1: TMemo
    Left = 0
    Top = 104
    Width = 339
    Height = 89
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      '')
    TabOrder = 7
  end
  object Edit2: TEdit
    Left = 264
    Top = 32
    Width = 49
    Height = 21
    TabOrder = 8
    Text = '128'
  end
  object VorbisIn1: TVorbisIn
    FileName = 'D:\Program Files\Borland\Delphi6\Projects\mozart.ogg'
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 8
    Top = 160
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'Wave files|*.wav|Ogg Vorbis files|*.ogg|MP3 files|*.mp3|FLAC fil' +
      'es|*.flac|WMA Files|*.wma'
    Left = 200
    Top = 160
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 40
    Top = 160
  end
  object FLACIn1: TFLACIn
    Loop = False
    CheckMD5Signature = False
    EndSample = -1
    StartSample = 0
    Left = 72
    Top = 160
  end
  object MP3In1: TMP3In
    Loop = False
    EndSample = -1
    StartSample = 0
    HighPrecision = False
    OutputChannels = cnMonoOrStereo
    Left = 104
    Top = 160
  end
  object WMStreamedOut1: TWMStreamedOut
    OnDone = WMStreamedOut1Done
    OnProgress = WMStreamedOut1Progress
    ShareMode = 0
    Id3v2Tags.Title = 'Home Radio Station'
    DesiredBitrate = 0
    Lossless = False
    VBR = False
    VBRQuality = 0
    MaxClients = 0
    Port = 0
    OnClientConnected = WMStreamedOut1ClientConnected
    OnClientDisconnected = WMStreamedOut1ClientDisconnected
    Left = 168
    Top = 160
  end
  object WMIn1: TWMIn
    Loop = False
    EndSample = -1
    StartSample = 0
    HighPrecision = False
    OutputChannels = cnMonoOrStereo
    Left = 136
    Top = 160
  end
end
