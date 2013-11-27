object XYPadsFrame: TXYPadsFrame
  Left = 0
  Top = 0
  Width = 863
  Height = 404
  TabOrder = 0
  object Panel: TRedFoxContainer
    Left = 0
    Top = 0
    Width = 863
    Height = 404
    Color = '$FFEEEEEE'
    Align = alClient
    object BackgroundPanel: TVamPanel
      Left = 0
      Top = 0
      Width = 863
      Height = 404
      HitTest = True
      Color = '$FFCCCCCC'
      Transparent = True
      Align = alClient
      Visible = True
      object VamLabel1: TVamLabel
        Left = 0
        Top = 0
        Width = 863
        Height = 18
        Text = 'XY PADS'
        HitTest = True
        AutoSize = False
        TextAlign = AlignCenter
        TextVAlign = AlignCenter
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        Align = alTop
        Visible = True
      end
      object VamDiv1: TVamDiv
        Left = 0
        Top = 363
        Width = 863
        Height = 41
        Text = 'VamDiv1'
        HitTest = True
        Align = alBottom
        Visible = True
        object CpuText: TVamLabel
          Left = 120
          Top = 0
          Width = 137
          Height = 41
          Text = 'CPU: 00%'
          HitTest = True
          AutoSize = False
          TextAlign = AlignNear
          TextVAlign = AlignCenter
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Align = alLeft
          Visible = True
        end
        object VoiceCountText: TVamLabel
          Left = 0
          Top = 0
          Width = 120
          Height = 41
          Text = 'Voices: 0'
          HitTest = True
          AutoSize = False
          TextAlign = AlignNear
          TextVAlign = AlignCenter
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          Align = alLeft
          Visible = True
        end
      end
    end
  end
end
