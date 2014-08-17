object Form1: TForm1
  Left = 10
  Top = 91
  Caption = 'Filter Test'
  ClientHeight = 507
  ClientWidth = 1107
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 392
    Top = 440
    Width = 81
    Height = 13
    Caption = 'Frequency 1 (Hz)'
  end
  object Label2: TLabel
    Left = 392
    Top = 464
    Width = 81
    Height = 13
    Caption = 'Frequency 2 (Hz)'
  end
  object Label3: TLabel
    Left = 568
    Top = 408
    Width = 82
    Height = 13
    Caption = 'Chebyshev Poles'
  end
  object Label4: TLabel
    Left = 568
    Top = 432
    Width = 98
    Height = 13
    Caption = 'Chebyshev ripple (%)'
  end
  object Label5: TLabel
    Left = 568
    Top = 456
    Width = 89
    Height = 13
    Caption = 'Sinc  Kernel Points'
  end
  object Label6: TLabel
    Left = 392
    Top = 408
    Width = 83
    Height = 13
    Caption = 'Sample Rate( Hz)'
  end
  object Label7: TLabel
    Left = 496
    Top = 408
    Width = 18
    Height = 13
    Caption = '000'
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 1041
    Height = 369
    TabOrder = 0
    object Image1: TImage
      Left = 8
      Top = 8
      Width = 1024
      Height = 353
    end
  end
  object Button1: TButton
    Left = 16
    Top = 400
    Width = 75
    Height = 25
    Caption = 'Open...'
    TabOrder = 1
    OnClick = Button1Click
  end
  object RadioGroup1: TRadioGroup
    Left = 104
    Top = 392
    Width = 129
    Height = 65
    Caption = ' Filter '
    ItemIndex = 0
    Items.Strings = (
      'Windowed Sinc'
      'Chebyshev')
    TabOrder = 2
  end
  object Edit1: TEdit
    Left = 496
    Top = 432
    Width = 49
    Height = 21
    TabOrder = 3
    Text = '0'
  end
  object Edit2: TEdit
    Left = 496
    Top = 456
    Width = 49
    Height = 21
    TabOrder = 4
    Text = '0'
  end
  object RadioGroup2: TRadioGroup
    Left = 240
    Top = 392
    Width = 137
    Height = 105
    Caption = ' Filter type '
    ItemIndex = 0
    Items.Strings = (
      'Low-pass'
      'High-pass'
      'Band-pass'
      'Band-reject')
    TabOrder = 5
  end
  object SpinEdit1: TSpinEdit
    Left = 672
    Top = 400
    Width = 41
    Height = 22
    Increment = 2
    MaxValue = 16
    MinValue = 2
    TabOrder = 6
    Value = 6
  end
  object Edit3: TEdit
    Left = 672
    Top = 424
    Width = 49
    Height = 21
    TabOrder = 7
    Text = '5'
  end
  object SpinEdit2: TSpinEdit
    Left = 672
    Top = 448
    Width = 49
    Height = 22
    Increment = 32
    MaxValue = 512
    MinValue = 32
    TabOrder = 8
    Value = 32
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 776
    Top = 400
  end
  object SincFilter1: TSincFilter
    Input = WaveIn1
    FilterType = ftBandPass
    HighFreq = 4000
    KernelWidth = 63
    LowFreq = 8000
    WindowType = fwBlackman
    Left = 808
    Top = 400
  end
  object ChebyshevFilter1: TChebyshevFilter
    Input = WaveIn1
    FilterType = ftLowPass
    NumberOfPoles = 8
    HighFreq = 8000
    LowFreq = 4000
    Ripple = 0.500000000000000000
    Left = 808
    Top = 432
  end
  object FrequencyAnalysis1: TFrequencyAnalysis
    Input = ChebyshevFilter1
    OnDone = FrequencyAnalysis1Done
    N = 2048
    Window = fwHamming
    StartSample = 0
    EndSample = -1
    Left = 848
    Top = 400
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wave|*.wav'
    Left = 776
    Top = 432
  end
  object FrequencyAnalysis2: TFrequencyAnalysis
    Input = WaveIn1
    OnDone = FrequencyAnalysis2Done
    N = 2048
    Window = fwHamming
    StartSample = 0
    EndSample = -1
    Left = 848
    Top = 432
  end
end
