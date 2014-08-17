unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACS_Classes, ACS_Misc, ACS_DXAudio, StdCtrls, Spin, Math, ACS_Types;

const
  SampleRate = 16000;
  BitsPerSample = 16;

type
  TForm1 = class(TForm)
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Button1: TButton;
    DXAudioOut1: TDXAudioOut;
    MemoryIn1: TMemoryIn;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure DXAudioOut1Done(Sender: TComponent);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    MemBlock : Pointer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  B16 : PBuffer16;
  i : Integer;
  Freq : Integer;
begin
  GetMem(MemBlock, SampleRate*2); // 1 second, 16 bit samples
  B16 := MemBlock;
  Freq := SpinEdit1.Value;
  for i := 0 to SampleRate - 1 do
    B16[i] := Round(Sin(2*Pi*i*Freq/SampleRate)*High(SmallInt)*0.4);
  MemoryIn1.InChannels := 1;
  MemoryIn1.InSampleRate := SampleRate;
  MemoryIn1.InBitsPerSample := BitsPerSample; 
  MemoryIn1.DataBuffer := MemBlock;
  MemoryIn1.DataSize := SampleRate*2;
  MemoryIn1.RepeatCount := SpinEdit2.Value;
  Button1.Enabled := False;
  DXAudioOut1.Run;
end;

procedure TForm1.DXAudioOut1Done(Sender: TComponent);
begin
  FreeMem(MemBlock);
  MemBlock := nil;
  Button1.Enabled := True;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DXAudioOut1.Stop(False);
  if Assigned(MemBlock) then
    FreeMem(MemBlock);
end;


end.
