object Form10: TForm10
  Left = 0
  Top = 0
  Caption = 'ReplayGain'
  ClientHeight = 254
  ClientWidth = 405
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    405
    254)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 104
    Top = 20
    Width = 193
    Height = 13
    Caption = 'Select several files  you want to analyze'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Analyze...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 8
    Top = 39
    Width = 389
    Height = 207
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
  object GainAnalysis1: TGainAnalysis
    Input = MP3In1
    Left = 136
    Top = 88
  end
  object NULLOut1: TNULLOut
    Input = GainAnalysis1
    OnDone = NULLOut1Done
    Left = 200
    Top = 88
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'mp3'
    Filter = 'MP3 files|*.mp3'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 24
    Top = 88
  end
  object MP3In1: TMP3In
    Loop = False
    EndSample = -1
    StartSample = 0
    HighPrecision = False
    OutputChannels = cnMonoOrStereo
    Left = 80
    Top = 88
  end
end
