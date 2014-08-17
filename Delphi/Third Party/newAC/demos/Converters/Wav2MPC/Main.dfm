object Form1: TForm1
  Left = 97
  Top = 152
  Caption = 'Wav2MPC Converter'
  ClientHeight = 248
  ClientWidth = 242
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
    242
    248)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 46
    Width = 32
    Height = 13
    Caption = 'Quality'
  end
  object Label2: TLabel
    Left = 10
    Top = 71
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object Label3: TLabel
    Left = 10
    Top = 95
    Width = 29
    Height = 13
    Caption = 'Album'
  end
  object Label4: TLabel
    Left = 10
    Top = 119
    Width = 23
    Height = 13
    Caption = 'Artist'
  end
  object Label5: TLabel
    Left = 10
    Top = 143
    Width = 22
    Height = 13
    Caption = 'Year'
  end
  object Label6: TLabel
    Left = 10
    Top = 167
    Width = 28
    Height = 13
    Caption = 'Track'
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
    Top = 193
    Width = 242
    Height = 17
    Align = alBottom
    Anchors = [akLeft, akRight]
    TabOrder = 1
    ExplicitTop = 196
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 229
    Width = 242
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 226
    ExplicitWidth = 235
  end
  object Button2: TButton
    Left = 96
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Convert...'
    TabOrder = 3
    OnClick = Button2Click
  end
  object SpinEdit1: TSpinEdit
    Left = 50
    Top = 43
    Width = 41
    Height = 22
    MaxValue = 10
    MinValue = 1
    TabOrder = 4
    Value = 5
  end
  object Edit1: TEdit
    Left = 50
    Top = 68
    Width = 184
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    ExplicitWidth = 177
  end
  object Edit2: TEdit
    Left = 50
    Top = 92
    Width = 184
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    ExplicitWidth = 177
  end
  object Edit3: TEdit
    Left = 50
    Top = 116
    Width = 184
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 7
    ExplicitWidth = 177
  end
  object Edit4: TEdit
    Left = 50
    Top = 140
    Width = 184
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
    ExplicitWidth = 177
  end
  object Edit5: TEdit
    Left = 50
    Top = 164
    Width = 184
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 9
    ExplicitWidth = 177
  end
  object Panel1: TPanel
    Left = 0
    Top = 210
    Width = 242
    Height = 19
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 10
    ExplicitTop = 207
    ExplicitWidth = 235
  end
  object WaveIn1: TWaveIn
    Loop = False
    EndSample = -1
    StartSample = 0
    Left = 168
    Top = 48
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Wav files|*.wav'
    Left = 96
    Top = 48
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'mpc'
    FilterIndex = 0
    Left = 128
    Top = 48
  end
  object MPCOut1: TMPCOut
    Input = WaveIn1
    OnDone = MPCOut1Done
    OnProgress = MPCOut1Progress
    OnThreadException = MPCOut1ThreadException
    ShareMode = 0
    Quality = 5.000000000000000000
    Left = 200
    Top = 48
  end
end
