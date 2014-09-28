object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 536
  ClientWidth = 1005
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 14
  object Button1: TButton
    Left = 25
    Top = 462
    Width = 139
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object RedFoxContainer1: TRedFoxContainer
    Left = 25
    Top = 16
    Width = 609
    Height = 440
    Color = '$FFCCCCCC'
    object VamPanel1: TVamPanel
      AlignWithMargins = True
      Left = 8
      Top = 8
      Width = 593
      Height = 424
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Opacity = 255
      Text = 'VamPanel1'
      HitTest = True
      Color = '$FFCCCCCC'
      Transparent = False
      Align = alClient
      Visible = True
      object VamTreeView1: TVamTreeView
        Left = 50
        Top = 3
        Width = 471
        Height = 406
        Opacity = 255
        Text = 'VamTreeView1'
        HitTest = True
        SelectedNodeColor = clBlack
        SelectedNodeAlpha = 35
        ChildIndent = 12
        DefaultNodeHeight = 16
        Options = []
        RootIndent = 4
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        Visible = True
      end
    end
  end
  object Button2: TButton
    Left = 185
    Top = 462
    Width = 139
    Height = 25
    Caption = 'Button2'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 640
    Top = 83
    Width = 353
    Height = 265
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 640
    Top = 55
    Width = 401
    Height = 22
    TabOrder = 4
    Text = '<region> sample=tom one.wav lokey=12'
  end
  object Button3: TButton
    Left = 330
    Top = 462
    Width = 139
    Height = 25
    Caption = 'Button3'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Panel1: TPanel
    Left = 656
    Top = 8
    Width = 137
    Height = 177
    Caption = 'Panel1'
    TabOrder = 6
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 816
    Top = 368
  end
end
