object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 349
  ClientWidth = 851
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
    851
    349)
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 6
    Top = 8
    Width = 837
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'Edit1'
    ExplicitWidth = 581
  end
  object Memo1: TMemo
    Left = 6
    Top = 35
    Width = 837
    Height = 276
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
    ExplicitWidth = 581
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
  object RenameProgramFileOnlyButton: TButton
    Left = 391
    Top = 316
    Width = 169
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Rename Program File Only'
    TabOrder = 3
    OnClick = RenameProgramFileOnlyButtonClick
  end
  object RenameProgramFileAndSampleDirButton: TButton
    Left = 183
    Top = 317
    Width = 202
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Rename Program File + Sample Dir'
    TabOrder = 4
    OnClick = RenameProgramFileAndSampleDirButtonClick
  end
end
