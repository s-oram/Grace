object VoiceSetupFrame: TVoiceSetupFrame
  Left = 0
  Top = 0
  Width = 1102
  Height = 708
  TabOrder = 0
  object Panel: TRedFoxContainer
    AlignWithMargins = True
    Left = 0
    Top = 0
    Width = 1102
    Height = 706
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 2
    Color = '$FFEEEEEE'
    Align = alClient
    ExplicitWidth = 320
    ExplicitHeight = 238
    object BackgroundPanel: TVamPanel
      Left = 0
      Top = 0
      Width = 1102
      Height = 706
      Opacity = 255
      HitTest = True
      Color = '$FFCCCCCC'
      Transparent = False
      Align = alClient
      Visible = True
      ExplicitLeft = 208
      ExplicitTop = 64
    end
  end
end
