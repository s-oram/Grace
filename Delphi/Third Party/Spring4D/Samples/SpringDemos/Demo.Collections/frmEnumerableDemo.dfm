object EnumerationDemoForm: TEnumerationDemoForm
  Left = 0
  Top = 0
  Caption = 'Enumeration Demo Form'
  ClientHeight = 512
  ClientWidth = 579
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    579
    512)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 243
    Top = 8
    Width = 328
    Height = 496
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 218
    Height = 25
    Caption = 'Run Regular Enumerable'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 39
    Width = 218
    Height = 25
    Caption = 'Run WhereEnumerable (evens only)'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 70
    Width = 218
    Height = 25
    Caption = 'Run SkipEnumerable'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 101
    Width = 218
    Height = 25
    Caption = 'Run SkipWhile less than five'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 132
    Width = 218
    Height = 25
    Caption = 'Run TakeEnumerable'
    TabOrder = 5
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 8
    Top = 163
    Width = 218
    Height = 25
    Caption = 'Run TakeWhileEnumerable'
    TabOrder = 6
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 8
    Top = 194
    Width = 218
    Height = 25
    Caption = 'Concat two lists'
    TabOrder = 7
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 8
    Top = 225
    Width = 218
    Height = 25
    Caption = 'Get first item'
    TabOrder = 8
    OnClick = Button8Click
  end
  object Button9: TButton
    Left = 8
    Top = 256
    Width = 218
    Height = 25
    Caption = 'Get last item'
    TabOrder = 9
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 8
    Top = 287
    Width = 218
    Height = 25
    Caption = 'Get the fifth item'
    TabOrder = 10
    OnClick = Button10Click
  end
  object Button11: TButton
    Left = 8
    Top = 318
    Width = 218
    Height = 25
    Caption = 'Get the minimum valued item'
    TabOrder = 11
    OnClick = Button11Click
  end
  object Button12: TButton
    Left = 8
    Top = 349
    Width = 218
    Height = 25
    Caption = 'Get the maximum valued item'
    TabOrder = 12
    OnClick = Button12Click
  end
  object Button13: TButton
    Left = 8
    Top = 380
    Width = 218
    Height = 25
    Caption = 'Get items in reverse order'
    TabOrder = 13
    OnClick = Button13Click
  end
  object Button14: TButton
    Left = 8
    Top = 411
    Width = 218
    Height = 25
    Caption = 'Create string with ForEach'
    TabOrder = 14
    OnClick = Button14Click
  end
  object Button15: TButton
    Left = 8
    Top = 442
    Width = 218
    Height = 25
    Caption = 'Move to set back to list'
    TabOrder = 15
    OnClick = Button15Click
  end
end
