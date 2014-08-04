object Form1: TForm1
  Left = 210
  Top = 206
  Width = 471
  Height = 318
  Caption = 'Wave File Converter'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ProgressBar1: TProgressBar
    Left = 97
    Top = 222
    Width = 327
    Height = 15
    Min = 0
    Max = 100
    Smooth = True
    Step = 1
    TabOrder = 0
  end
  object GroupBox1: TGroupBox
    Left = 7
    Top = 55
    Width = 202
    Height = 147
    Caption = 'Input File Parameters'
    TabOrder = 1
    object Label1: TLabel
      Left = 7
      Top = 42
      Width = 47
      Height = 13
      Caption = 'Channels:'
    end
    object Label2: TLabel
      Left = 7
      Top = 69
      Width = 64
      Height = 13
      Caption = 'Sample Rate:'
    end
    object Label3: TLabel
      Left = 7
      Top = 21
      Width = 47
      Height = 13
      Caption = 'File Name'
    end
    object Label4: TLabel
      Left = 7
      Top = 90
      Width = 77
      Height = 13
      Caption = 'Bits Per Sample:'
    end
    object Label5: TLabel
      Left = 91
      Top = 21
      Width = 104
      Height = 19
      AutoSize = False
    end
    object Label6: TLabel
      Left = 90
      Top = 42
      Width = 43
      Height = 19
      AutoSize = False
    end
    object Label7: TLabel
      Left = 91
      Top = 70
      Width = 42
      Height = 19
      AutoSize = False
    end
    object Label8: TLabel
      Left = 91
      Top = 91
      Width = 42
      Height = 19
      AutoSize = False
    end
    object Label14: TLabel
      Left = 7
      Top = 111
      Width = 45
      Height = 13
      Caption = 'Encoding'
    end
    object Label15: TLabel
      Left = 91
      Top = 111
      Width = 3
      Height = 13
    end
  end
  object Button1: TButton
    Left = 7
    Top = 21
    Width = 70
    Height = 21
    Caption = 'Input File...'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 222
    Top = 21
    Width = 77
    Height = 21
    Caption = 'Output File...'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 7
    Top = 215
    Width = 65
    Height = 22
    Caption = 'Convert!'
    TabOrder = 4
    OnClick = Button3Click
  end
  object GroupBox2: TGroupBox
    Left = 216
    Top = 56
    Width = 233
    Height = 145
    Caption = ' Output File Parameters '
    TabOrder = 5
    object Label9: TLabel
      Left = 7
      Top = 21
      Width = 47
      Height = 13
      Caption = 'File Name'
    end
    object Label10: TLabel
      Left = 91
      Top = 21
      Width = 104
      Height = 19
      AutoSize = False
    end
    object Label11: TLabel
      Left = 7
      Top = 69
      Width = 64
      Height = 13
      Caption = 'Sample Rate:'
    end
    object Label12: TLabel
      Left = 7
      Top = 42
      Width = 47
      Height = 13
      Caption = 'Channels:'
    end
    object Label13: TLabel
      Left = 7
      Top = 90
      Width = 77
      Height = 13
      Caption = 'Bits Per Sample:'
    end
    object Label16: TLabel
      Left = 7
      Top = 125
      Width = 45
      Height = 13
      Caption = 'Encoding'
    end
    object SpinEdit1: TSpinEdit
      Left = 90
      Top = 42
      Width = 29
      Height = 22
      MaxValue = 2
      MinValue = 1
      TabOrder = 0
      Value = 1
    end
    object SpinEdit3: TSpinEdit
      Left = 90
      Top = 88
      Width = 36
      Height = 22
      Increment = 8
      MaxValue = 16
      MinValue = 8
      TabOrder = 1
      Value = 8
    end
    object Edit1: TEdit
      Left = 90
      Top = 69
      Width = 50
      Height = 21
      TabOrder = 2
      Text = '22050'
    end
    object ComboBox1: TComboBox
      Left = 90
      Top = 118
      Width = 105
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 3
      Text = 'RAW PCM'
      Items.Strings = (
        'RAW PCM'
        'DVI IMA ADPCM')
    end
  end
  object RateConverter1: TRateConverter
    FilterWindow = fwBlackman
    KernelWidth = 30
    OutSampleRate = 22050
    Left = 304
    Top = 248
  end
  object SampleConverter1: TSampleConverter
    Left = 240
    Top = 248
  end
  object MSConverter1: TMSConverter
    Mode = msmMonoToBoth
    Left = 272
    Top = 248
  end
  object SampleConverter2: TSampleConverter
    Left = 336
    Top = 248
  end
  object WaveIn1: TWaveIn
    EndSample = -1
    Loop = False
    StartSample = 0
    Left = 208
    Top = 248
  end
  object WaveOut1: TWaveOut
    SuspendWhenIdle = True
    OnDone = WaveOut1Done
    OnProgress = WaveOut1Progress
    FileMode = foRewrite
    WavType = wtPCM
    BlockSize = 512
    Left = 368
    Top = 248
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'wav'
    Filter = 'Wave Files|*.wav'
    FilterIndex = 0
    Title = 'Open'
    Left = 136
    Top = 248
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'wav'
    Filter = 'Wave Files|*.wav'
    FilterIndex = 0
    Title = 'Save As'
    Left = 168
    Top = 248
  end
end
