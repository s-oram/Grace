object AboutFrame: TAboutFrame
  Left = 0
  Top = 0
  Width = 549
  Height = 369
  TabOrder = 0
  object Panel: TRedFoxContainer
    Left = 0
    Top = 0
    Width = 549
    Height = 369
    Color = '$FFEEEEEE'
    Align = alClient
    object BackgroundPanel: TVamPanel
      Left = 0
      Top = 0
      Width = 549
      Height = 369
      Text = 'BackgroundPanel'
      HitTest = True
      Color = '$FFCCCCCC'
      Transparent = False
      Align = alClient
      Visible = True
    end
  end
end
