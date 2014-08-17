object Form12: TForm12
  Left = 0
  Top = 0
  Caption = 'Tags Editor'
  ClientHeight = 282
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    420
    282)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 64
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object Label2: TLabel
    Left = 8
    Top = 93
    Width = 29
    Height = 13
    Caption = 'Album'
  end
  object Label3: TLabel
    Left = 8
    Top = 122
    Width = 26
    Height = 13
    Caption = 'Artist'
  end
  object Label4: TLabel
    Left = 8
    Top = 151
    Width = 29
    Height = 13
    Caption = 'Genre'
  end
  object Label5: TLabel
    Left = 8
    Top = 180
    Width = 26
    Height = 13
    Caption = 'Track'
  end
  object Label6: TLabel
    Left = 8
    Top = 209
    Width = 22
    Height = 13
    Caption = 'Year'
  end
  object Edit1: TEdit
    Left = 43
    Top = 56
    Width = 369
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Load...'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 89
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Save'
    TabOrder = 2
    OnClick = Button2Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 263
    Width = 420
    Height = 19
    Panels = <
      item
        Width = 200
      end
      item
        Width = 100
      end
      item
        Width = 50
      end>
  end
  object Edit2: TEdit
    Left = 43
    Top = 85
    Width = 369
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    Text = 'Edit1'
  end
  object Edit3: TEdit
    Left = 43
    Top = 114
    Width = 369
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
    Text = 'Edit1'
  end
  object Edit4: TEdit
    Left = 43
    Top = 143
    Width = 369
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
    Text = 'Edit1'
  end
  object Edit5: TEdit
    Left = 43
    Top = 172
    Width = 369
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 7
    Text = 'Edit1'
  end
  object Edit6: TEdit
    Left = 43
    Top = 201
    Width = 369
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Times New Roman'
    Font.Style = []
    ParentFont = False
    TabOrder = 8
    Text = 'Edit1'
  end
  object TagEditor1: TTagEditor
    Left = 288
    Top = 8
  end
  object OpenDialog1: TOpenDialog
    Left = 216
    Top = 8
  end
end
