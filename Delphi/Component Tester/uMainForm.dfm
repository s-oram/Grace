object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 536
  ClientWidth = 1005
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object Button1: TButton
    Left = 25
    Top = 462
    Width = 139
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 185
    Top = 462
    Width = 139
    Height = 25
    Caption = 'Button2'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 330
    Top = 462
    Width = 139
    Height = 25
    Caption = 'Button3'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Panel1: TPanel
    Left = 25
    Top = 48
    Width = 320
    Height = 185
    Caption = 'Panel1'
    TabOrder = 3
    object Edit1: TEdit
      Left = 64
      Top = 32
      Width = 89
      Height = 22
      TabOrder = 0
      Text = 'Edit1'
    end
    object Edit2: TEdit
      Left = 50
      Top = 60
      Width = 89
      Height = 22
      TabOrder = 1
      Text = 'Edit1'
    end
  end
  object Panel2: TPanel
    Left = 377
    Top = 48
    Width = 320
    Height = 185
    Caption = 'Panel1'
    TabOrder = 4
    object Edit3: TEdit
      Left = 34
      Top = 44
      Width = 89
      Height = 22
      TabOrder = 0
      Text = 'Edit1'
    end
  end
  object Edit4: TEdit
    Left = 34
    Top = 396
    Width = 487
    Height = 22
    TabOrder = 5
    Text = 'Edit1'
  end
  object RedFoxContainer1: TRedFoxContainer
    Left = 288
    Top = 96
    Width = 625
    Height = 265
    Color = '$FFEEEEEE'
    object VamPanel1: TVamPanel
      Left = 42
      Top = 40
      Width = 409
      Height = 193
      Opacity = 255
      Text = 'VamPanel1'
      HitTest = True
      Color = '$FFCCCCCC'
      Transparent = False
      Visible = True
      object VamLabel1: TVamLabel
        Left = 40
        Top = 48
        Width = 345
        Height = 121
        Opacity = 255
        Text = 'Test'
        HitTest = True
        AutoTrimText = False
        AutoSize = False
        TextAlign = AlignCenter
        TextVAlign = AlignCenter
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        Visible = True
      end
    end
  end
end
