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
  object RedFoxContainer1: TRedFoxContainer
    Left = 80
    Top = 24
    Width = 377
    Height = 393
    Color = '$FFCCCCCC'
    object VamTextBox2: TVamTextBox
      Left = 32
      Top = 72
      Width = 217
      Height = 113
      Opacity = 255
      HitTest = True
      AutoTrimText = False
      Color = '$FF3E3E3E'
      ColorMouseOver = '$FF3E3E3E'
      ColorBorder = '$00000000'
      ShowBorder = False
      TextAlign = AlignNear
      TextVAlign = AlignNear
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ImageOverlayVertAlign = AlignCenter
      ImageOverlayHorzAlign = AlignCenter
      ImageOverlayOffsetX = 0
      ImageOverlayOffsetY = 0
      Visible = True
    end
  end
  object Button2: TButton
    Left = 23
    Top = 503
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 3
  end
  object Button3: TButton
    Left = 104
    Top = 503
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 4
  end
  object Button4: TButton
    Left = 185
    Top = 503
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 5
  end
  object Button5: TButton
    Left = 185
    Top = 463
    Width = 139
    Height = 25
    Caption = 'Button1'
    TabOrder = 6
    OnClick = Button5Click
  end
  object Memo1: TMemo
    Left = 640
    Top = 83
    Width = 353
    Height = 265
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 640
    Top = 55
    Width = 401
    Height = 22
    TabOrder = 7
    Text = '<region> sample=tom one.wav lokey=12'
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 816
    Top = 368
  end
end
