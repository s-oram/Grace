object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 349
  ClientWidth = 595
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
    595
    349)
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 6
    Top = 8
    Width = 581
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'Edit1'
  end
  object Memo1: TMemo
    Left = 6
    Top = 35
    Width = 581
    Height = 276
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object RenameAllSampleButton: TButton
    Left = 8
    Top = 317
    Width = 169
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Rename All Samples'
    TabOrder = 2
    OnClick = RenameAllSampleButtonClick
  end
  object RenameProgramFileButton: TButton
    Left = 183
    Top = 316
    Width = 169
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Rename Program File'
    TabOrder = 3
    OnClick = RenameProgramFileButtonClick
  end
end
