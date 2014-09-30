object TestDialogForm: TTestDialogForm
  Left = 0
  Top = 0
  Caption = 'Test Dialog'
  ClientHeight = 312
  ClientWidth = 573
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object RedFoxContainer1: TRedFoxContainer
    Left = 0
    Top = 0
    Width = 573
    Height = 312
    Color = '$FFEEEEEE'
    Align = alClient
    object VamPanel1: TVamPanel
      Left = 0
      Top = 0
      Width = 573
      Height = 312
      Opacity = 255
      Text = 'VamPanel1'
      HitTest = True
      Color = '$FF000000'
      Transparent = False
      Align = alClient
      Visible = True
      object VamPanel2: TVamPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 567
        Height = 306
        Opacity = 255
        Text = 'VamPanel2'
        HitTest = True
        Color = '$FFCCCCCC'
        CornerRadius1 = 3.000000000000000000
        CornerRadius2 = 3.000000000000000000
        CornerRadius3 = 3.000000000000000000
        CornerRadius4 = 3.000000000000000000
        Transparent = False
        Align = alClient
        Visible = True
        object Label1: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 107
          Height = 16
          Align = alTop
          Alignment = taCenter
          Caption = 'Jump up to the top'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object VamDiv1: TVamDiv
          AlignWithMargins = True
          Left = 8
          Top = 264
          Width = 551
          Height = 34
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Opacity = 255
          Text = 'VamDiv1'
          HitTest = True
          Align = alBottom
          Visible = True
          object Button1: TButton
            Left = 476
            Top = 0
            Width = 75
            Height = 34
            Align = alRight
            Caption = 'Button1'
            TabOrder = 0
          end
          object Button2: TButton
            Left = 401
            Top = 0
            Width = 75
            Height = 34
            Align = alRight
            Caption = 'Button1'
            TabOrder = 1
          end
        end
      end
    end
  end
end
