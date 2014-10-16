object SampleFinderDialogForm: TSampleFinderDialogForm
  Left = 0
  Top = 0
  Caption = 'SampleFinderDialogForm'
  ClientHeight = 392
  ClientWidth = 638
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
    Width = 638
    Height = 392
    Color = '$FFEEEEEE'
    Align = alClient
    object BackPanel1: TVamPanel
      Left = 0
      Top = 0
      Width = 638
      Height = 392
      Opacity = 255
      Text = 'BackPanel1'
      HitTest = True
      Color = '$FF000000'
      Transparent = False
      Align = alClient
      Visible = True
      object BackPanel2: TVamPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 632
        Height = 386
        Opacity = 255
        Text = 'BackPanel2'
        HitTest = True
        Color = '$FFCCCCCC'
        CornerRadius1 = 3.000000000000000000
        CornerRadius2 = 3.000000000000000000
        CornerRadius3 = 3.000000000000000000
        CornerRadius4 = 3.000000000000000000
        Transparent = False
        Align = alClient
        Visible = True
        object ButtonDiv: TVamDiv
          AlignWithMargins = True
          Left = 8
          Top = 296
          Width = 616
          Height = 82
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Opacity = 255
          Text = 'ButtonDiv'
          HitTest = True
          Align = alBottom
          Visible = True
        end
        object MainDialogArea: TVamDiv
          AlignWithMargins = True
          Left = 8
          Top = 8
          Width = 616
          Height = 272
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Opacity = 255
          Text = 'ButtonDiv'
          HitTest = True
          Align = alClient
          Visible = True
          OnResize = MainDialogAreaResize
          object DialogTextControl: TLabel
            Left = 16
            Top = 16
            Width = 86
            Height = 13
            Caption = 'DialogTextControl'
          end
          object FilenameLabel: TLabel
            Left = 16
            Top = 72
            Width = 53
            Height = 13
            Caption = 'Missing File'
          end
          object FullPathLabel: TLabel
            Left = 16
            Top = 128
            Width = 78
            Height = 13
            Caption = 'Missing File Path'
          end
          object FilenameEdit: TEdit
            Left = 16
            Top = 45
            Width = 569
            Height = 21
            TabOrder = 0
            Text = 'FilenameEdit'
          end
          object FullPathEdit: TEdit
            Left = 16
            Top = 101
            Width = 569
            Height = 21
            TabOrder = 1
            Text = 'Edit1'
          end
        end
      end
    end
  end
end
