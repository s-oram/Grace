object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 501
  ClientWidth = 1005
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 371
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 368
    Top = 32
    Width = 537
    Height = 265
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object RedFoxContainer1: TRedFoxContainer
    Left = 32
    Top = 44
    Width = 617
    Height = 309
    Color = '$FFEEEEEE'
    object Knob1: TVamNumericKnob
      Left = 48
      Top = 56
      Width = 201
      Height = 25
      Text = 'Knob1'
      HitTest = True
      TextAlign = AlignCenter
      TextVAlign = AlignCenter
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      KnobMin = 0
      KnobMax = 100
      NumericStyle = nsFloat
      DecimalPlaces = 2
      OnChanged = VamNumericKnob1Changed
      Visible = True
    end
    object Knob2: TVamNumericKnob
      Left = 48
      Top = 112
      Width = 201
      Height = 25
      Text = 'VamNumericKnob1'
      HitTest = True
      TextAlign = AlignCenter
      TextVAlign = AlignCenter
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      KnobMin = 0
      KnobMax = 100
      NumericStyle = nsFloat
      DecimalPlaces = 2
      Visible = True
    end
    object DropBoxSelector1: TDropBoxSelector
      Left = 168
      Top = 192
      Width = 125
      Height = 25
      Text = 'DropBoxSelector1'
      HitTest = True
      ColorTextA = '$FFCCCCCC'
      ColorTextB = '$FFFFFFFF'
      Color = '$FF3E3E3E'
      ColorMouseOver = '$FF3E3E3E'
      ColorBorder = '$00000000'
      ShowBorder = False
      TextA = 'Text A'
      TextB = 'Text B'
      TextPadding.Left = 6
      TextPadding.Top = 3
      TextPadding.Right = 6
      TextPadding.Bottom = 3
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Visible = True
    end
  end
  object Button2: TButton
    Left = 89
    Top = 371
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 3
  end
  object Button3: TButton
    Left = 170
    Top = 411
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 250
    Top = 451
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 5
    OnClick = Button3Click
  end
end
