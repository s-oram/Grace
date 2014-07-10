unit DCBFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TfrmDCB = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    cbBaudRate: TComboBox;
    Label1: TLabel;
    edtByteSize: TEdit;
    Label6: TLabel;
    cbParity: TComboBox;
    Label7: TLabel;
    cbStopBits: TComboBox;
    Label8: TLabel;
    cbfDsrSensitivity: TCheckBox;
    cbDtrControl: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    cbRTSControl: TComboBox;
    BitBtn3: TBitBtn;
    cbfParity: TCheckBox;
    cbfOutxCtsFlow: TCheckBox;
    cbfOutxDsrFlow: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function ExecDCB( var DCB: TDCB): Boolean;
  end;

var
  frmDCB: TfrmDCB;

implementation

{$R *.DFM}

procedure TfrmDCB.FormCreate(Sender: TObject);
begin
  with cbBaudRate.Items do begin
    Clear;
    AddObject( '110', TObject(CBR_110));
    AddObject( '300', TObject(CBR_300));
    AddObject( '600', TObject(CBR_600));
    AddObject( '1200', TObject(CBR_1200));
    AddObject( '2400', TObject(CBR_2400));
    AddObject( '4800', TObject(CBR_4800));
    AddObject( '9600', TObject(CBR_9600));
    AddObject( '14400', TObject(CBR_14400));
    AddObject( '19200', TObject(CBR_19200));
    AddObject( '38400', TObject(CBR_38400));
    AddObject( '56000', TObject(CBR_56000));
    AddObject( '57600', TObject(CBR_57600));
    AddObject( '115200', TObject(CBR_115200));
    AddObject( '128000', TObject(CBR_128000));
    AddObject( '256000', TObject(CBR_256000));
  end;
  with cbDtrControl.Items do begin
    Clear;
    AddObject( 'DISABLE', TObject(DTR_CONTROL_DISABLE));
    AddObject( 'ENABLE', TObject(DTR_CONTROL_ENABLE));
    AddObject( 'HANDSHAKE', TObject(DTR_CONTROL_HANDSHAKE));
  end;
  with cbRtsControl.Items do begin
    Clear;
    AddObject('DISABLE', TObject(RTS_CONTROL_DISABLE));
    AddObject('ENABLE', TObject(RTS_CONTROL_ENABLE));
    AddObject('HANDSHAKE', TObject(RTS_CONTROL_HANDSHAKE));
    AddObject('TOGGLE', TObject(RTS_CONTROL_TOGGLE));
  end;
  with cbParity.Items do begin
    Clear;
    AddObject('EVEN PARITY', TObject(EVENPARITY));
    AddObject('MARK PARITY', TObject(MARKPARITY));
    AddObject('NO PARITY', TObject(NOPARITY));
    AddObject('ODD PARITY', TObject(ODDPARITY));
    AddObject('SPACE PARITY', TObject(SPACEPARITY));
  end;
  with cbStopBits.Items do begin
    Clear;
    AddObject('1 Stop Bit', TObject(ONESTOPBIT));
    AddObject('1.5 Stop Bits', TObject(ONE5STOPBITS));
    AddObject('2 Stop Bits', TObject(TWOSTOPBITS));
  end;
end;

function TfrmDCB.ExecDCB(var DCB: TDCB): Boolean;
var
  I: Integer;
  DtrControl: DWORD;
  RtsControl: DWORD;

begin
                               // Load Baud Rate
  cbBaudRate.ItemIndex:= -1;
  for I:= 0 to cbBaudRate.Items.Count - 1 do begin
    if DWORD( cbBaudRate.Items.Objects[I]) = DCB.BaudRate then begin
      cbBaudRate.ItemIndex:= I;
      Break;
    end;
  end;
                               // Load Flags
  cbfParity.Checked:= (DCB.Flags and $0002) <> 0;
  cbfOutxCtsFlow.Checked:= (DCB.Flags and $0004) <> 0;
  cbfOutxDsrFlow.Checked:= (DCB.Flags and $0008) <> 0;
  cbfDsrSensitivity.Checked:= (DCB.Flags and $0040) <> 0;
//  cbfTXContinueOnXOff.Checked:= (DCB.Flags and $0080) <> 0;
//  cbfOutX.Checked:= (DCB.Flags and $0100) <> 0;
//  cbfInX.Checked:= (DCB.Flags and $0200) <> 0;
//  cbfErrorChar.Checked:= (DCB.Flags and $0400) <> 0;
//  cbfNull.Checked:= (DCB.Flags and $0800) <> 0;
//  cbfAbortOnError.Checked:= (DCB.Flags and $4000) <> 0;

                               // Load DTR Input Flow Control
  DTRControl:= (DCB.Flags shr 4) and $0003;
  cbDtrControl.ItemIndex:= -1;
  for I:= 0 to cbDtrControl.Items.Count - 1 do begin
    if DWORD( cbDtrControl.Items.Objects[I]) = DTRControl then begin
      cbDtrControl.ItemIndex:= I;
      Break;
    end;
  end;
                               // Load RTS Input Flow Control
  RTSControl:= (DCB.Flags shr 12) and $0003;
  cbRtsControl.ItemIndex:= -1;
  for I:= 0 to cbRtsControl.Items.Count - 1 do begin
    if DWORD( cbRtsControl.Items.Objects[I]) = RTSControl then begin
      cbRtsControl.ItemIndex:= I;
      Break;
    end;
  end;
                               // Load Parity
  cbParity.ItemIndex:= -1;
  for I:= 0 to cbParity.Items.Count - 1 do begin
    if Byte(cbParity.Items.Objects[I]) = DCB.Parity then begin
      cbParity.ItemIndex:= I;
      Break;
    end;
  end;
                               // Load Stop Bits
  cbStopBits.ItemIndex:= -1;
  for I:= 0 to cbStopBits.Items.Count - 1 do begin
    if Byte(cbStopBits.Items.Objects[I]) = DCB.StopBits then begin
      cbStopBits.ItemIndex:= I;
      Break;
    end;
  end;

  edtByteSize.Text:= IntToStr(DCB.ByteSize);
//  edtXonLim.Text:= IntToStr(DCB.XonLim);
//  edtXoffLim.Text:= IntToStr(DCB.XoffLim);
//  edtXonChar.Text:= IntToHex(Byte( DCB.XonChar), 2);
//  edtXoffChar.Text:= IntToHex(Byte( DCB.XoffChar), 2);
//  edtErrorChar.Text:= IntToHex( Byte( DCB.ErrorChar), 2);
//  edtEofChar.Text:= IntToHex( Byte( DCB.EofChar), 2);
//  edtEvtChar.Text:= IntToHex( Byte( DCB.EvtChar), 2);

  Result:= ShowModal = mrOK;
  if not Result then Exit;

  I:= cbBaudRate.ItemIndex;
  if I <> -1 then
    DCB.BaudRate:= DWORD( cbBaudRate.Items.Objects[I]);

  DCB.Flags:= DCB.Flags and $FFFF8001;

  I:= cbDtrControl.ItemIndex;
  if I <> -1 then
    DCB.Flags:= DCB.Flags or (LongInt(cbDtrControl.Items.Objects[I]) shl 4);

  I:= cbRtsControl.ItemIndex;
  if I <> -1 then
    DCB.Flags:= DCB.Flags or (LongInt(cbRtsControl.Items.Objects[I]) shl 12);

  if cbfParity.Checked then DCB.Flags:= DCB.Flags or $0002;
  if cbfOutxCtsFlow.Checked then DCB.Flags:= DCB.Flags or $0004;
  if cbfOutxDsrFlow.Checked then DCB.Flags:= DCB.Flags or $0008;
  if cbfDsrSensitivity.Checked then DCB.Flags:= DCB.Flags or $0040;
//  if cbfTXContinueOnXOff.Checked then DCB.Flags:= DCB.Flags or $0080;
//  if cbfOutX.Checked then DCB.Flags:= DCB.Flags or $0100;
//  if cbfInX.Checked then DCB.Flags:= DCB.Flags or $0200;
//  if cbfErrorChar.Checked then DCB.Flags:= DCB.Flags or $0400;
//  if cbfNull.Checked then DCB.Flags:= DCB.Flags or $0800;
//  if cbfAbortOnError.Checked then DCB.Flags:= DCB.Flags or $4000;

  I:= cbParity.ItemIndex;
  if I <> -1 then DCB.Parity:= Byte(cbParity.Items.Objects[I]);

  I:= cbStopBits.ItemIndex;
  if I <> -1 then DCB.StopBits:= Byte(cbStopBits.Items.Objects[I]);

  DCB.ByteSize:= StrToInt(edtByteSize.Text);
//  DCB.XonLim:= StrToInt(edtXonLim.Text);
//  DCB.XoffLim:= StrToInt(edtXoffLim.Text);

//  DCB.XonChar:= Char( StrToInt('$'+Trim( edtXonChar.Text)));
//  DCB.XoffChar:= Char( StrToInt('$'+Trim( edtXoffChar.Text)));
  DCB.ErrorChar:= Chr(0); //Char( StrToInt('$'+Trim( edtErrorChar.Text)));
  DCB.EofChar:= Chr(0);   //Char( StrToInt('$'+Trim( edtEofChar.Text)));
  DCB.EvtChar:= Chr(0);   //Char( StrToInt('$'+Trim( edtEvtChar.Text)));
end;

procedure TfrmDCB.BitBtn3Click(Sender: TObject);
begin
//  Application.HelpContext(PortSettings_ID);
end;

end.
