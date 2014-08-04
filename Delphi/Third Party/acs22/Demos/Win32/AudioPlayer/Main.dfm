object Form1: TForm1
  Left = 182
  Top = 201
  Width = 207
  Height = 177
  Caption = 'Audio Player'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 72
    Top = 64
    Width = 53
    Height = 13
    Caption = 'Total Time:'
  end
  object Label2: TLabel
    Left = 128
    Top = 64
    Width = 3
    Height = 13
  end
  object Label3: TLabel
    Left = 8
    Top = 8
    Width = 19
    Height = 13
    Caption = 'File:'
  end
  object Label4: TLabel
    Left = 32
    Top = 8
    Width = 3
    Height = 13
  end
  object Button1: TButton
    Left = 8
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Play...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 88
    Width = 185
    Height = 17
    Min = 0
    Max = 100
    Smooth = True
    Step = 5
    TabOrder = 1
  end
  object Button2: TButton
    Left = 96
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Abort'
    TabOrder = 2
    OnClick = Button2Click
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 62
    Width = 49
    Height = 17
    Caption = 'Loop'
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 129
    Width = 199
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object VorbisIn1: TVorbisIn
    EndSample = -1
    FileName = 'D:\Program Files\Borland\Delphi6\Projects\mozart.ogg'
    Loop = False
    StartSample = 0
    Left = 8
    Top = 112
  end
  object AudioOut1: TAudioOut
    Input = VorbisIn1
    SuspendWhenIdle = True
    OnDone = AudioOut1Done
    OnProgress = AudioOut1Progress
    OnThreadException = AudioOut1ThreadException
    BaseChannel = 0
    Volume = 255
    Left = 136
    Top = 112
  end
  object OpenDialog1: TOpenDialog
    Filter = 
      'Wave files|*.wav|Ogg Vorbis files|*.ogg|FLAC files|*.flac|Ape fi' +
      'les|*.ape'
    Left = 168
    Top = 112
  end
  object WaveIn1: TWaveIn
    EndSample = -1
    Loop = False
    StartSample = 0
    Left = 40
    Top = 112
  end
  object MACIn1: TMACIn
    EndSample = -1
    Loop = False
    StartSample = 0
    Left = 72
    Top = 112
  end
  object FLACIn1: TFLACIn
    EndSample = -1
    Loop = False
    StartSample = 0
    Left = 104
    Top = 112
  end
end
