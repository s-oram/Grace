object FmSvgViewer: TFmSvgViewer
  Left = 0
  Top = 0
  Caption = 'SVG Viewer'
  ClientHeight = 272
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object AggSVG: TAggSVG
    Left = 0
    Top = 0
    Width = 378
    Height = 272
    Align = alClient
    OnMouseDown = AggSVGMouseDown
    OnMouseMove = AggSVGMouseMove
    Scale = 1.000000000000000000
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.svg'
    Filter = 'Scalable Vector Graphic (*.svg)|*.svg'
    Left = 48
    Top = 40
  end
end
