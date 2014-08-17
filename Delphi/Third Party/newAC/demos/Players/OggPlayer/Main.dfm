object Form1: TForm1
  Left = 484
  Top = 384
  Caption = 'Ogg Player'
  ClientHeight = 346
  ClientWidth = 291
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
    291
    346)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 48
    Top = 56
    Width = 50
    Height = 13
    AutoSize = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 16
    Top = 56
    Width = 25
    Height = 13
    Caption = 'BPS'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 16
    Top = 8
    Width = 68
    Height = 13
    Caption = 'Sample rate'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 96
    Top = 8
    Width = 3
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 16
    Top = 80
    Width = 57
    Height = 13
    Caption = 'Total time'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label7: TLabel
    Left = 96
    Top = 80
    Width = 3
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label8: TLabel
    Left = 16
    Top = 32
    Width = 5
    Height = 13
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label9: TLabel
    Left = 16
    Top = 184
    Width = 58
    Height = 13
    Caption = 'Comments'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label10: TLabel
    Left = 144
    Top = 136
    Width = 47
    Height = 13
    Caption = 'Balance'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 0
    Top = 333
    Width = 291
    Height = 13
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 3
  end
  object ScrollBar1: TScrollBar
    Left = 16
    Top = 112
    Width = 265
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Enabled = False
    PageSize = 0
    TabOrder = 0
    OnScroll = ScrollBar1Scroll
  end
  object BitBtn1: TBitBtn
    Left = 16
    Top = 136
    Width = 25
    Height = 25
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      33333333333333333333EEEEEEEEEEEEEEE333FFFFFFFFFFFFF3E00000000000
      00E337777777777777F3E0F77777777770E337F33333333337F3E0F333333333
      70E337F3333F333337F3E0F33303333370E337F3337FF33337F3E0F333003333
      70E337F33377FF3337F3E0F33300033370E337F333777FF337F3E0F333000033
      70E337F33377773337F3E0F33300033370E337F33377733337F3E0F333003333
      70E337F33377333337F3E0F33303333370E337F33373333337F3E0F333333333
      70E337F33333333337F3E0FFFFFFFFFFF0E337FFFFFFFFFFF7F3E00000000000
      00E33777777777777733EEEEEEEEEEEEEEE33333333333333333}
    NumGlyphs = 2
    TabOrder = 1
    OnClick = BitBtn1Click
  end
  object BitBtn2: TBitBtn
    Left = 48
    Top = 136
    Width = 25
    Height = 25
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      33333333333333333333EEEEEEEEEEEEEEE333FFFFFFFFFFFFF3E00000000000
      00E337777777777777F3E0F77777777770E337F33333333337F3E0F333333333
      70E337F33333333337F3E0F33333333370E337F333FF3FF337F3E0F330030033
      70E337F3377F77F337F3E0F33003003370E337F3377F77F337F3E0F330030033
      70E337F3377F77F337F3E0F33003003370E337F3377F77F337F3E0F330030033
      70E337F33773773337F3E0F33333333370E337F33333333337F3E0F333333333
      70E337F33333333337F3E0FFFFFFFFFFF0E337FFFFFFFFFFF7F3E00000000000
      00E33777777777777733EEEEEEEEEEEEEEE33333333333333333}
    NumGlyphs = 2
    ParentFont = False
    TabOrder = 2
    OnClick = BitBtn2Click
  end
  object BitBtn3: TBitBtn
    Left = 80
    Top = 136
    Width = 25
    Height = 25
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000130B0000130B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      33333333333333333333EEEEEEEEEEEEEEE333FFFFFFFFFFFFF3E00000000000
      00E337777777777777F3E0F77777777770E337F33333333337F3E0F333333333
      70E337F33333333337F3E0F33333333370E337F333FFFFF337F3E0F330000033
      70E337F3377777F337F3E0F33000003370E337F3377777F337F3E0F330000033
      70E337F3377777F337F3E0F33000003370E337F3377777F337F3E0F330000033
      70E337F33777773337F3E0F33333333370E337F33333333337F3E0F333333333
      70E337F33333333337F3E0FFFFFFFFFFF0E337FFFFFFFFFFF7F3E00000000000
      00E33777777777777733EEEEEEEEEEEEEEE33333333333333333}
    NumGlyphs = 2
    TabOrder = 3
    OnClick = BitBtn3Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 208
    Width = 273
    Height = 105
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelInner = bvNone
    BevelOuter = bvNone
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 4
  end
  object TrackBar1: TTrackBar
    Left = 136
    Top = 152
    Width = 80
    Height = 30
    PageSize = 1
    Position = 5
    TabOrder = 5
    OnChange = TrackBar1Change
  end
  object VorbisIn1: TVorbisIn
    FileName = 'E:\Music\Valkyries.ogg'
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 40
    Top = 232
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Ogg Vorbis Files|*.ogg'
    Left = 232
    Top = 232
  end
  object Timer2: TTimer
    Interval = 500
    OnTimer = Timer2Timer
    Left = 200
    Top = 232
  end
  object DXAudioOut1: TDXAudioOut
    Input = StereoBalance1
    OnDone = AudioOut1Done
    OnProgress = AudioOut1Progress
    OnThreadException = DXAudioOut1ThreadException
    DeviceNumber = 0
    Latency = 80
    PrefetchData = True
    PollingInterval = 200
    FramesInBuffer = 65536
    SpeedFactor = 1.000000000000000000
    Left = 120
    Top = 232
  end
  object StereoBalance1: TStereoBalance
    Input = VorbisIn1
    Balance = 0.500000000000000000
    Left = 80
    Top = 232
  end
end
