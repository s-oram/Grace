unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, ksComm, ActnList, ImgList, Menus,
  ToolWin;

type
  TfrmMain = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    PageControl1: TPageControl;
    tsText: TTabSheet;
    tsHex: TTabSheet;
    Splitter1: TSplitter;
    MemoText: TMemo;
    MemoHex: TMemo;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ImageList1: TImageList;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ActionList1: TActionList;
    acConnect: TAction;
    acDisconnect: TAction;
    acSetup: TAction;
    cmbPort: TComboBox;
    ToolButton6: TToolButton;
    tsLog: TTabSheet;
    MemoLog: TMemo;
    MemoCommand: TMemo;
    chbCR: TCheckBox;
    ToolButton7: TToolButton;
    chbSendHex: TCheckBox;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure acConnectExecute(Sender: TObject);
    procedure acConnectUpdate(Sender: TObject);
    procedure acDisconnectExecute(Sender: TObject);
    procedure acSetupExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ToolButton8Click(Sender: TObject);
  private
    { Private declarations }
    FPort: TksComPort;
    FTextBuf: TBytes;
    FTextPos: Integer;
    FHexBuf: TBytes;
    FHexPos: Integer;
    FMemoLimit: Integer;
//    FTextLimit: Integer;
//    FHexLimit: Integer;
    procedure DumpHex;
    procedure MemoAdd(Target: TMemo; const Value: string);
    procedure PortMessage(Sender: TObject; var Msg: TMessage);
    procedure PortEvent(Value: LongWord);
    procedure PortError(Value: LongWord);
    procedure PortRead(Value: LongWord);
    procedure ReadOverflow(Value: LongWord);
    procedure FatalError(Value: LongWord);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses DCBFrm;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FMemoLimit:= 16 * 1024;
  SetLength(FTextBuf, 64);
  SetLength(FHexBuf, 16);
//  FTextLimit:= 64;
//  FHexLimit:= 16;
  FPort:= TksComPort.Create(Self);
  FPort.OnMessage:= PortMessage;
end;

function Strings2Bytes(Strings: TStrings; Bytes: TBytes): Integer;
var
  I, J, K, L: Integer;
  S, S1: string;
  B: Byte;

begin
  Result:= 0;
  for I:= 0 to Strings.Count - 1 do begin
    S:= Strings[I];
    L:= Length(S);
    J:= 1;
    while J <= L do begin
      while (J <= L) and (S[J] <= ' ') do Inc(J);
      K:= J;
      while (K <= L) and (S[K] > ' ') do Inc(K);
      if K > J then begin
        S1:= Copy(S, J, K - J);
        if S1[1] <> '$' then S1:= '$' + S1;
        B:= StrToInt(S1);
        if Assigned(Bytes) then Bytes[Result]:= B;
        Inc(Result);
      end;
      J:= K;
    end;
  end;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift:
    TShiftState);
var
  S: string;
  I, J, Count: Integer;
  Command: TBytes;

begin
// MemoCommand.WantReturns = False, so Ctrl+Enter ignored
  if (ActiveControl = MemoCommand) and (Key = VK_RETURN)
    and not (ssCtrl in Shift) then begin
//      ShowMessage('VK_RETURN');
    if chbSendHex.Checked then begin
      Count:= Strings2Bytes(MemoCommand.Lines, nil);
      SetLength(Command, Count);
      Strings2Bytes(MemoCommand.Lines, Command);
    end
    else begin

// Calculate the byte size of the command buffer
      I:= 0;
      Count:= 0;
      while I < MemoCommand.Lines.Count do begin
        Inc(Count, Length(MemoCommand.Lines[I]));
        Inc(I);
      end;

// Additional byte is for CR {13}
      if chbCR.Checked then Inc(Count);

// Additional byte is for LF {10}
      SetLength(Command, Count + 1);

// Fill the command buffer
      I:= 0;
      Count:= 0;
      while I < MemoCommand.Lines.Count do begin
        S:= MemoCommand.Lines[I];
        J:= 0;
        while (J < Length(S)) do begin
          Inc(J);
          Command[Count]:= Byte(S[J]);
          Inc(Count);
        end;
        Inc(I);
      end;

      if chbCR.Checked then begin
        Command[Count]:= 13;
        Inc(Count);
      end;

      Command[Count]:= 10;
      Inc(Count);
    end;

    MemoCommand.Lines.Clear;

    FPort.Write(Command[0], Count);
  end;
end;

procedure TfrmMain.MemoAdd(Target: TMemo; const Value: string);
var
  SL: TStrings;
  I, N: Integer;

begin
  if (Target.GetTextLen > FMemoLimit) and (FMemoLimit > 0) then begin
    SL:= TStringList.Create;
    try
      N:= Target.Lines.Count;
      I:= N shr 1;
      while I < N do begin
        SL.Add(Target.Lines[I]);
        Inc(I);
      end;
      Target.Lines.Assign(SL);
    finally
      SL.Free;
    end;
  end;
  Target.Lines.Add(Value);
end;

procedure TfrmMain.DumpHex;
var
  S: string;
  I: Integer;

begin
  S:= '';
  for I:= 0 to Length(FHexBuf) - 1 do begin
    if I < FHexPos then
      S:= S + IntToHex(FHexBuf[I], 2) + ' '
    else
      S:= S + '.. ';
  end;
  MemoAdd(MemoHex, S);
  FHexPos:= 0;
end;

procedure AppendS(var S: string; const Value: string);
begin
  if Length(S) > 0 then S:= S + ',';
  S:= S + Value;
end;

procedure TfrmMain.acConnectExecute(Sender: TObject);
begin
  if not FPort.Connected then begin
//    Port1.ReadBufSize:= 64 * 1024;       // DEBUG!
//    Port1.WriteBufSize:= 64;
    FPort.PortName:= cmbPort.Text;
    if FPort.Open then (Sender as TAction).Checked:= True;
  end;
end;

procedure TfrmMain.acConnectUpdate(Sender: TObject);
var
  Connected: Boolean;

begin
  Connected:= FPort.Connected;
  acConnect.Enabled:= not Connected;
  acDisconnect.Enabled:= Connected;
  acSetup.Enabled:= Connected;
end;

procedure TfrmMain.acDisconnectExecute(Sender: TObject);
begin
  if FPort.Connected then begin
    FPort.Close;
    (Sender as TAction).Checked:= True;
  end;
end;

procedure TfrmMain.acSetupExecute(Sender: TObject);
var
  DCB: TDCB;

begin
  if FPort.Connected then begin
    FPort.GetPortState(DCB);
    if frmDCB.ExecDCB(DCB) then FPort.SetPortState(DCB);
  end;
end;

procedure TfrmMain.PortEvent(Value: LongWord);
var
  S: string;

begin
  S:= '';
  if Value and EV_CTS <> 0 then AppendS(S, 'CTS');
  if Value and EV_DSR <> 0 then AppendS(S, 'DSR');
  if Value and EV_RLSD <> 0 then AppendS(S, 'RLSD');
  if Value and EV_RING <> 0 then AppendS(S, 'RING');
  if Value and EV_ERR <> 0 then AppendS(S, 'ERR');
  if Value and EV_BREAK <> 0 then AppendS(S, 'BREAK');
  MemoAdd(MemoLog, Format('Comm Event %.8x [%s]', [Value, S]));
end;

procedure TfrmMain.PortError(Value: LongWord);
var
  S: string;

begin
  S:= '';
  if Value and CE_BREAK <> 0 then AppendS(S, 'BREAK');
  if Value and CE_FRAME <> 0 then AppendS(S, 'FRAME');
  if Value and CE_OVERRUN <> 0 then AppendS(S, 'OVERRUN');
  if Value and CE_RXOVER <> 0 then AppendS(S, 'RXOVER');
  if Value and CE_RXPARITY <> 0 then AppendS(S, 'RXPARITY');
  MemoAdd(MemoLog, Format('Comm Error %.8x [%s]', [Value, S]));
end;

procedure TfrmMain.PortMessage(Sender: TObject; var Msg: TMessage);
begin
  case Msg.WParam of
    TksComPort.etCommEvent: PortEvent(Msg.LParam);
    TksComPort.etCommError: PortError(Msg.LParam);
    TksComPort.etCommDataReady: PortRead(Msg.LParam);
    TksComPort.etWriteBufEmpty: ;
    TksComPort.etReadOverflow: ReadOverflow(Msg.LParam);
    TksComPort.etFatalError: FatalError(Msg.LParam);
  end;
  Msg.Result:= 0;
end;

procedure TfrmMain.PortRead(Value: LongWord);
var
  Buf: array[0..127] of Byte;
  NextByte: Byte;
  I, J, Count: Integer;
  S: string;

begin
  repeat
    Count:= FPort.Read(Buf, SizeOf(Buf));
    if Count = 0 then Break;
    I:= 0;
    repeat
      NextByte:= Buf[I];
      if NextByte = 10 {LF} then begin
        if (FTextPos > 0) and (FTextBuf[FTextPos - 1] = 13 {CR} )
          then Dec(FTextPos);
      end
      else begin
        FTextBuf[FTextPos]:= NextByte;
        Inc(FTextPos);
      end;
      if (NextByte = 10) or (FTextPos >= Length(FTextBuf)) then begin
        SetLength(S, FTextPos);
        for J:= 0 to FTextPos - 1 do begin
          S[J+1]:= Char(FTextBuf[J]);
        end;
        MemoAdd(MemoText, S);
        FTextPos:= 0;
      end;
      FHexBuf[FHexPos]:= NextByte;
      Inc(FHexPos);
      Inc(I);
      if (FHexPos >= Length(FHexBuf)) or (I = Count) then DumpHex;
    until I = Count;
  until Count < SizeOf(Buf);
end;

procedure TfrmMain.ReadOverflow(Value: LongWord);
begin
  MemoAdd(MemoLog, Format('Read Queue Overflow %d', [Value]));
end;

procedure TfrmMain.ToolButton8Click(Sender: TObject);
var
  CommConfig: TCommConfig;

begin
  CommConfigDialog(PChar(FPort.PortName), 0, CommConfig);
end;

procedure TfrmMain.FatalError(Value: LongWord);
begin
  MemoAdd(MemoLog, Format('Fatal Error %d', [Value]));
end;

end.
