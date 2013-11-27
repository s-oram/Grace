object FmAgg2D: TFmAgg2D
  Left = 0
  Top = 0
  Caption = 'TAgg2D Demo'
  ClientHeight = 477
  ClientWidth = 643
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnClick = FormClick
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Agg2DControl: TAgg2DControl
    Left = 0
    Top = 0
    Width = 643
    Height = 477
    Align = alClient
  end
  object MainMenu: TMainMenu
    Left = 24
    Top = 16
    object MiFile: TMenuItem
      Caption = '&File'
      object MiSaveAs: TMenuItem
        Caption = 'Save as...'
        OnClick = MiSaveAsClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MiExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MiExitClick
      end
    end
    object MiExample: TMenuItem
      Caption = '&Example'
      object MiDemo: TMenuItem
        Caption = 'Demo'
        OnClick = MiDemoClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MiLines: TMenuItem
        Caption = '&Lines'
        object MiLinesRandom: TMenuItem
          Caption = '&Random'
          OnClick = MiLinesRandomClick
        end
        object MiLinesCircle: TMenuItem
          Caption = '&Circle'
          OnClick = MiLinesCircleClick
        end
        object MiLinesGradient: TMenuItem
          Caption = '&Gradient'
          OnClick = MiLinesGradientClick
        end
        object MiLinesAnimation: TMenuItem
          Caption = '&Animation'
          OnClick = MiLinesAnimationClick
        end
      end
      object MiCurves: TMenuItem
        Caption = '&Curves'
        object MiCurvesRandom: TMenuItem
          Caption = '&Random'
          OnClick = MiCurvesRandomClick
        end
        object MiCurvesCircle: TMenuItem
          Caption = '&Circle'
          OnClick = MiCurvesCircleClick
        end
        object MiCurvesAnimation: TMenuItem
          Caption = '&Animation'
          OnClick = MiCurvesAnimationClick
        end
      end
      object MiArcs: TMenuItem
        Caption = '&Arcs'
        object MiArcsRandom: TMenuItem
          Caption = '&Random'
          OnClick = MiArcsRandomClick
        end
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MiEllipses: TMenuItem
        Caption = '&Ellipses'
        object MiEllipsesRandom: TMenuItem
          Caption = '&Random'
          OnClick = MiEllipsesRandomClick
        end
        object MiEllipsesCircles: TMenuItem
          Caption = '&Circles'
          OnClick = MiEllipsesCirclesClick
        end
        object MiEllipsesGradient: TMenuItem
          Caption = '&Gradient'
          OnClick = MiEllipsesGradientClick
        end
        object MiCircleAnimation: TMenuItem
          Caption = '&Animation'
          OnClick = MiCircleAnimationClick
        end
      end
      object MiTriangles: TMenuItem
        Caption = '&Triangles'
        object MiTrianglesRandom: TMenuItem
          Caption = '&Random'
          OnClick = MiTrianglesRandomClick
        end
        object MiTrianglesAnimation: TMenuItem
          Caption = '&Animation'
          OnClick = MiTrianglesAnimationClick
        end
      end
      object MiRectangles: TMenuItem
        Caption = '&Rectangles'
        object MiRectanglesRandom: TMenuItem
          Caption = '&Random'
          OnClick = MiRectanglesRandomClick
        end
        object MiRectanglesArbitrary: TMenuItem
          Caption = 'Ar&bitrary'
          OnClick = MiRectanglesArbitraryClick
        end
        object MiRectanglesAnimation: TMenuItem
          Caption = '&Animation'
          OnClick = MiRectanglesAnimationClick
        end
      end
      object MiStars: TMenuItem
        Caption = '&Stars'
        object MiStarsRandom: TMenuItem
          Caption = '&Random'
          OnClick = MiStarsRandomClick
        end
        object MiStarAnimation: TMenuItem
          Caption = '&Animation'
          OnClick = MiStarAnimationClick
        end
      end
      object MiPolygons: TMenuItem
        Caption = 'Polygons'
        object MiPolygonsRandom: TMenuItem
          Caption = '&Random'
          OnClick = MiPolygonsRandomClick
        end
        object MiPolygonsAnimation: TMenuItem
          Caption = '&Animation'
          OnClick = MiPolygonsAnimationClick
        end
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MiText: TMenuItem
        Caption = 'Text'
        object MiTextRandom: TMenuItem
          Caption = '&Random'
          OnClick = MiTextRandomClick
        end
        object MiTextLoremIpsum: TMenuItem
          Caption = '&Lorem Ipsum'
          OnClick = MiTextLoremIpsumClick
        end
        object MiTextAnimation: TMenuItem
          Caption = '&Animation'
          OnClick = MiTextAnimationClick
        end
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MiTransformations: TMenuItem
        Caption = '&Transformations'
        Visible = False
        object MiRotate: TMenuItem
          Caption = '&Rotate'
          OnClick = MiRotateClick
        end
      end
      object MiColors: TMenuItem
        Caption = '&Colors'
        object MiColorsRed: TMenuItem
          Caption = '&Red'
          OnClick = MiColorsRedClick
        end
        object MiColorsGreen: TMenuItem
          Caption = '&Green'
          OnClick = MiColorsGreenClick
        end
        object MiColorsBlue: TMenuItem
          Caption = '&Blue'
          OnClick = MiColorsBlueClick
        end
      end
    end
    object MiSettings: TMenuItem
      Caption = '&Settings'
      object MiAlwaysClear: TMenuItem
        Caption = 'Always Clear'
        Checked = True
        OnClick = MiAlwaysClearClick
      end
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.bmp'
    Filter = 'Bitmap (*.bmp)|*.bmp'
    Left = 112
    Top = 16
  end
  object Timer: TTimer
    Enabled = False
    Interval = 10
    Left = 192
    Top = 16
  end
end
