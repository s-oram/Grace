object FmClock: TFmClock
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Clock'
  ClientHeight = 400
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Agg2DControl: TAgg2DControl
    Left = 0
    Top = 0
    Width = 400
    Height = 400
    Align = alClient
    OnClick = Agg2DControlClick
    OnPaint = Agg2DControlPaint
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 184
    Top = 184
  end
end
