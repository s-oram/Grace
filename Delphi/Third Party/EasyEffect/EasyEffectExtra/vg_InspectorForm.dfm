object InspectorForm: TInspectorForm
  Left = 0
  Top = 0
  Caption = 'Inspector'
  ClientHeight = 682
  ClientWidth = 563
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object vgScene1: TvgScene
    Left = 0
    Top = 0
    Width = 563
    Height = 682
    Align = alClient
    DesignSnapGridShow = False
    DesignSnapToGrid = False
    DesignSnapToLines = True
    object Root1: TvgBackground
      Width = 563.000000000000000000
      Height = 682.000000000000000000
      object Inspector1: TvgInspector
        Align = vaClient
        Width = 563.000000000000000000
        Height = 682.000000000000000000
        Resource = 'treeviewstyle'
        TabOrder = 0
        HideSelectionUnfocused = False
      end
    end
  end
end
