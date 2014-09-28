object TestDialogForm: TTestDialogForm
  Left = 0
  Top = 0
  Caption = 'Test Dialog'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 0
    Top = 256
    Width = 635
    Height = 36
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 8
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object CancelButton: TButton
      AlignWithMargins = True
      Left = 520
      Top = 3
      Width = 105
      Height = 30
      Margins.Right = 10
      Align = alRight
      Caption = 'Cancel'
      TabOrder = 0
    end
    object OkButton: TButton
      AlignWithMargins = True
      Left = 409
      Top = 3
      Width = 105
      Height = 30
      Align = alRight
      Caption = 'OK'
      TabOrder = 1
      OnClick = OkButtonClick
    end
  end
end
