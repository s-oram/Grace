object InfoBarFrame: TInfoBarFrame
  Left = 0
  Top = 0
  Width = 626
  Height = 228
  TabOrder = 0
  object Panel: TRedFoxContainer
    Left = 0
    Top = 0
    Width = 626
    Height = 228
    Color = '$FFEEEEEE'
    Align = alClient
    object BackgroundPanel: TVamPanel
      Left = 0
      Top = 0
      Width = 626
      Height = 228
      Text = 'BackgroundPanel'
      HitTest = True
      Color = '$FFCCCCFF'
      CornerRadius1 = 3.000000000000000000
      CornerRadius2 = 3.000000000000000000
      CornerRadius3 = 3.000000000000000000
      CornerRadius4 = 3.000000000000000000
      Transparent = False
      Align = alClient
      Visible = True
      object InfoTextBox: TVamTextBox
        Left = 40
        Top = 52
        Width = 493
        Height = 49
        Text = 'Plugin Info Here!'
        HitTest = True
        Color = '$FF3E3E3E'
        ColorMouseOver = '$FF3E3E3E'
        TextAlign = AlignNear
        TextVAlign = AlignCenter
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        Visible = True
      end
    end
  end
end
