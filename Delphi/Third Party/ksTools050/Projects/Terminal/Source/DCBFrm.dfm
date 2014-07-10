object frmDCB: TfrmDCB
  Left = 303
  Top = 185
  Width = 359
  Height = 315
  Caption = 'Device Control Block'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 351
    Height = 240
    Align = alClient
    TabOrder = 0
    object Label1: TLabel
      Left = 24
      Top = 24
      Width = 46
      Height = 13
      Caption = 'Baud rate'
    end
    object Label6: TLabel
      Left = 192
      Top = 24
      Width = 44
      Height = 13
      Caption = 'Byte Size'
    end
    object Label7: TLabel
      Left = 24
      Top = 80
      Width = 26
      Height = 13
      Caption = 'Parity'
    end
    object Label8: TLabel
      Left = 192
      Top = 80
      Width = 41
      Height = 13
      Caption = 'Stop bits'
    end
    object Label2: TLabel
      Left = 24
      Top = 136
      Width = 111
      Height = 13
      Caption = 'DTR Input Flow Control'
    end
    object Label3: TLabel
      Left = 24
      Top = 184
      Width = 110
      Height = 13
      Caption = 'RTS Input Flow Control'
    end
    object cbBaudRate: TComboBox
      Left = 24
      Top = 40
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
    end
    object edtByteSize: TEdit
      Left = 192
      Top = 40
      Width = 73
      Height = 21
      TabOrder = 1
      Text = 'edtByteSize'
    end
    object cbParity: TComboBox
      Left = 24
      Top = 96
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
    end
    object cbStopBits: TComboBox
      Left = 192
      Top = 96
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
    end
    object cbfDsrSensitivity: TCheckBox
      Left = 192
      Top = 200
      Width = 145
      Height = 17
      Caption = 'DSR Sensitivity'
      TabOrder = 4
    end
    object cbDtrControl: TComboBox
      Left = 24
      Top = 152
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
    end
    object cbRTSControl: TComboBox
      Left = 24
      Top = 200
      Width = 129
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 6
    end
    object cbfParity: TCheckBox
      Left = 192
      Top = 136
      Width = 97
      Height = 17
      Caption = 'Parity Check'
      TabOrder = 7
    end
    object cbfOutxCtsFlow: TCheckBox
      Left = 192
      Top = 160
      Width = 137
      Height = 17
      Caption = 'CTS Output Control'
      TabOrder = 8
    end
    object cbfOutxDsrFlow: TCheckBox
      Left = 192
      Top = 176
      Width = 137
      Height = 17
      Caption = 'DSR Output Control'
      TabOrder = 9
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 240
    Width = 351
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      351
      41)
    object BitBtn1: TBitBtn
      Left = 87
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
    end
    object BitBtn2: TBitBtn
      Left = 175
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333333333333000033338833333333333333333F333333333333
        0000333911833333983333333388F333333F3333000033391118333911833333
        38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
        911118111118333338F3338F833338F3000033333911111111833333338F3338
        3333F8330000333333911111183333333338F333333F83330000333333311111
        8333333333338F3333383333000033333339111183333333333338F333833333
        00003333339111118333333333333833338F3333000033333911181118333333
        33338333338F333300003333911183911183333333383338F338F33300003333
        9118333911183333338F33838F338F33000033333913333391113333338FF833
        38F338F300003333333333333919333333388333338FFF830000333333333333
        3333333333333333333888330000333333333333333333333333333333333333
        0000}
      NumGlyphs = 2
    end
    object BitBtn3: TBitBtn
      Left = 264
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Help'
      TabOrder = 2
      OnClick = BitBtn3Click
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333336633
        3333333333333FF3333333330000333333364463333333333333388F33333333
        00003333333E66433333333333338F38F3333333000033333333E66333333333
        33338FF8F3333333000033333333333333333333333338833333333300003333
        3333446333333333333333FF3333333300003333333666433333333333333888
        F333333300003333333E66433333333333338F38F333333300003333333E6664
        3333333333338F38F3333333000033333333E6664333333333338F338F333333
        0000333333333E6664333333333338F338F3333300003333344333E666433333
        333F338F338F3333000033336664333E664333333388F338F338F33300003333
        E66644466643333338F38FFF8338F333000033333E6666666663333338F33888
        3338F3330000333333EE666666333333338FF33333383333000033333333EEEE
        E333333333388FFFFF8333330000333333333333333333333333388888333333
        0000}
      NumGlyphs = 2
    end
  end
end
