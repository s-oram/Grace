object SequencerFrame: TSequencerFrame
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  TabOrder = 0
  object Panel: TRedFoxContainer
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Color = '$FFEEEEEE'
    Align = alClient
    object BackgroundPanel: TVamPanel
      Left = 0
      Top = 0
      Width = 320
      Height = 240
      Text = 'BackgroundPanel'
      HitTest = True
      Color = '$FFCCCCCC'
      Transparent = False
      Align = alClient
      Visible = True
      object SeqBackPanel: TVamPanel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 312
        Height = 232
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Text = 'SeqBackPanel'
        HitTest = True
        Color = '$FFCCCCCC'
        CornerRadius1 = 3.000000000000000000
        CornerRadius2 = 3.000000000000000000
        CornerRadius3 = 3.000000000000000000
        CornerRadius4 = 3.000000000000000000
        Transparent = False
        Align = alClient
        Visible = True
      end
    end
  end
end
