object Form1: TForm1
  Left = 186
  Top = 177
  Width = 290
  Height = 152
  Caption = 'Speach Server'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 72
    Width = 11
    Height = 13
  end
  object Label2: TLabel
    Left = 16
    Top = 8
    Width = 39
    Height = 13
    Caption = 'Local IP'
  end
  object Button1: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 96
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Disconnect'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 64
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '127.0.0.1'
  end
  object AudioOut1: TAudioOut
    Input = WaveIn1
    SuspendWhenIdle = True
    BaseChannel = 0
    Volume = 255
    Left = 184
    Top = 72
  end
  object WaveIn1: TWaveIn
    Loop = False
    Left = 144
    Top = 72
  end
  object IdTrivialFTPServer1: TIdTrivialFTPServer
    Bindings = <>
    DefaultPort = 69
    OnWriteFile = IdTrivialFTPServer1WriteFile
    OnTransferComplete = IdTrivialFTPServer1TransferComplete
    Left = 104
    Top = 72
  end
end
