object FmAggPasControlsdemo: TFmAggPasControlsdemo
  Left = 0
  Top = 0
  Caption = 'AggPas Controls Demo'
  ClientHeight = 88
  ClientWidth = 259
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    259
    88)
  PixelsPerInch = 96
  TextHeight = 13
  object AggLabel: TAggLabel
    Left = 111
    Top = 31
    Width = 140
    Height = 22
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    ParentColor = False
    TextColor = clBlack
    TextSize = 16.000000000000000000
    Text = 'Test 123'
    TextWidth = 2.000000000000000000
  end
  object AggSlider: TAggSlider
    Left = 8
    Top = 8
    Width = 243
    Height = 17
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Test 123'
    TextThickness = 1.500000000000000000
    Value = 0.600000000000000000
    BackgroundColor = -3348737
    TriangleColor = -6710862
    TextColor = -16777216
    PointerPreviewColor = 1717986969
    PointerColor = -1728053044
  end
  object AggCheckBox: TAggCheckBox
    Left = 111
    Top = 60
    Width = 73
    Height = 14
    Anchors = [akLeft, akBottom]
    TextColor = -16777216
    InactiveColor = -16777216
    ActiveColor = -16777114
  end
  object AggRadioBox: TAggRadioBox
    Left = 8
    Top = 31
    Width = 97
    Height = 49
    Anchors = [akLeft, akBottom]
    Items.Strings = (
      'Item 1'
      'Item 2')
    BackgroundColor = -3348737
    BorderColor = -16777216
    TextColor = -16777216
    InactiveColor = 1717986969
    ActiveColor = -1728053044
  end
end
