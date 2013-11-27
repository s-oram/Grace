object PerformanceCounterForm: TPerformanceCounterForm
  Left = 0
  Top = 0
  Caption = 'PerformanceCounterForm'
  ClientHeight = 200
  ClientWidth = 160
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 49
    Height = 13
    Caption = 'Last Time:'
  end
  object Label2: TLabel
    Left = 8
    Top = 64
    Width = 49
    Height = 13
    Caption = 'Max Time:'
  end
  object Label3: TLabel
    Left = 82
    Top = 64
    Width = 63
    Height = 13
    Caption = '(Milliseconds)'
  end
  object Label4: TLabel
    Left = 82
    Top = 8
    Width = 63
    Height = 13
    Caption = '(Milliseconds)'
  end
  object LastTimeDisplay: TEdit
    Left = 8
    Top = 24
    Width = 137
    Height = 21
    TabOrder = 0
    Text = 'LastTimeDisplay'
  end
  object MaxTimeDisplay: TEdit
    Left = 8
    Top = 80
    Width = 137
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 40
    Top = 107
    Width = 75
    Height = 25
    Caption = 'Reset'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Timer1: TTimer
    Interval = 200
    OnTimer = OnUpdateDisplay
    Left = 8
    Top = 112
  end
end
