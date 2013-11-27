object ProfillerForm: TProfillerForm
  Left = 0
  Top = 0
  Caption = 'ProfillerForm'
  ClientHeight = 192
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    465
    192)
  PixelsPerInch = 96
  TextHeight = 13
  object ReportDisplayBox: TMemo
    Left = 4
    Top = 4
    Width = 457
    Height = 153
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'ReportDisplayBox')
    TabOrder = 0
  end
  object ResetButton: TButton
    Left = 4
    Top = 163
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Reset'
    TabOrder = 1
    OnClick = ResetButtonClick
  end
  object UpdateTimer: TTimer
    OnTimer = UpdateTimerStep
    Left = 212
    Top = 12
  end
end
