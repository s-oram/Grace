object ModSystemFrame: TModSystemFrame
  Left = 0
  Top = 0
  Width = 689
  Height = 240
  TabOrder = 0
  object Panel: TRedFoxContainer
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 689
    Height = 238
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 2
    Color = '$FFEEEEEE'
    Align = alClient
    object BackgroundPanel: TVamPanel
      Left = 0
      Top = 0
      Width = 689
      Height = 238
      Opacity = 255
      HitTest = True
      Color = '$FFCCCCCC'
      Transparent = False
      Align = alClient
      Visible = True
    end
  end
end
