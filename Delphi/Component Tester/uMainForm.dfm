object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 536
  ClientWidth = 1005
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 25
    Top = 461
    Width = 139
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object RedFoxContainer1: TRedFoxContainer
    Left = 25
    Top = 8
    Width = 849
    Height = 425
    Color = '$FFCCCCCC'
    object VamPanel1: TVamPanel
      Left = 48
      Top = 80
      Width = 505
      Height = 313
      Opacity = 255
      Text = 'VamPanel1'
      HitTest = True
      Color = '$FFCCCCCC'
      Transparent = False
      Visible = True
      object VamKnob1: TVamKnob
        Left = 64
        Top = 136
        Width = 65
        Height = 65
        Opacity = 255
        Text = 'VamKnob1'
        HitTest = True
        ModLineDist = 17.000000000000000000
        ModLineWidth = 3.000000000000000000
        ModLineColor = '$FFFF0000'
        ModLineOffColor = '$FFC0C0C0'
        IndicatorSize = 2.500000000000000000
        IndicatorDist = 9.000000000000000000
        IsBipolarKnob = False
        KnobMode = PositionEdit
        IsKnobEnabled = True
        VisibleSteps = 0
        ParameterIndex = 0
        OnKnobPosChanged = VamKnob1KnobPosChanged
        Visible = True
      end
      object VamLabel1: TVamLabel
        Left = 152
        Top = 200
        Width = 193
        Height = 65
        Opacity = 255
        Text = 'Test'
        HitTest = True
        AutoSize = False
        TextAlign = AlignCenter
        TextVAlign = AlignCenter
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Visible = True
      end
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
    Left = 592
    Top = 463
    Width = 353
    Height = 265
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
end
