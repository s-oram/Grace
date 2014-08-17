(*
   TAudioProcessor component demo. This demo uses TAudioProcessor for swapping input channels:
   AudioData --L-R-L-R-L-R-->[AudioProcessor]--R-L-R-L-R-L-->
   So on the output the left channel becomes right and vice-versa.
 *)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACS_Classes, ACS_DXAudio, ACS_Wave, ACS_Misc, ACS_Types, StdCtrls;

type
  TForm1 = class(TForm)
    AudioProcessor1: TAudioProcessor;
    WaveIn1: TWaveIn;
    DXAudioOut1: TDXAudioOut;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    Button2: TButton;
    procedure AudioProcessor1GetData(Sender: TComponent; var Buffer: Pointer;
      var Bytes: Cardinal);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure DXAudioOut1Done(Sender: TComponent);
    procedure AudioProcessor1Init(Sender: TComponent; var TotalSize: Int64);
    procedure AudioProcessor1Flush(Sender: TComponent);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

(*
  This OnGetData event handler is where the processing occures.
  The Sender variable points to the AudioProcessor object for which the handler is called.
  The Buffer points to the block of data to be processed. You should never delete this block.
  The Bytes variable holds the number of bytes in the block.
  There are two ways to return the results of the processing.
  First you can change the contents of the block pointed by Buffer (just as we do in this example).
  The next component in the chain will get the modified data.
  Second you can replace the pointer in the Buffer variable itself, making it point to the new data block.
  Of cource if the new number of data bytes differs from that before processing, you should change the Bytes value as well.
  Returning 0 in the Bytes variable means that data processing should stop.
*)

procedure TForm1.AudioProcessor1GetData(Sender: TComponent;
  var Buffer: Pointer; var Bytes: Cardinal);
var
 B32 : PBuffer32;
 B16 : PBuffer16;
 Tmp : Integer;
 i : Integer;
begin
  AudioProcessor1.Input.GetData(Buffer, Bytes);
  if Buffer = nil then
    Exit;
  case AudioProcessor1.Input.BitsPerSample of
    16 :
    begin
      B16 := Buffer;
      for i := 0 to (Bytes div 4) - 1 do
      begin
        Tmp := B16[i*2];
        B16[i*2] := B16[i*2 + 1];
        B16[i*2 + 1] := Tmp;
      end;
    end;
    32 :
    begin
      B32 := Buffer;
      for i := 0 to (Bytes div 8) - 1 do
      begin
        Tmp := B32[i*2];
        B32[i*2] := B32[i*2 + 1];
        B32[i*2 + 1] := Tmp;
      end;
    end;
  end;
end;

(*
  This OnInit event handler is called before the processing starts.
  The Sender variable points to the AudioProcessor object for which the handler is called and we use it to access the AudioProcessor object properties.
  The mimum thing we should do here is to initialize processor's input which we do by calling
  TAudioProcessor(Sender).Input.Init;
  We can also do whatever is needed to initialize the audio processor itself.
*)

procedure TForm1.AudioProcessor1Init(Sender: TComponent; var TotalSize: Int64);
begin
  TAudioProcessor(Sender).Input.Init;
  TotalSize := TAudioProcessor(Sender).Input.Size
end;


(*
  This OnFlush event handler is called after the processing has finished
  The Sender variable points to the AudioProcessor object for which the handler is called and we use it to access the AudioProcessor object properties.
  The mimum thing we should do here is to uninitialize processor's input which we do by calling
  TAudioProcessor(Sender).Input.Flush;
  We can also do whatever is needed to uninitialize the audio processor itself.
*)

procedure TForm1.AudioProcessor1Flush(Sender: TComponent);
begin
  TAudioProcessor(Sender).Input.Flush;
end;


procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    Button1.Enabled := False;
    WaveIn1.FileName := OpenDialog1.FileName;
    DXAudioOut1.Run;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  DXAudioOut1.Stop;
end;


procedure TForm1.DXAudioOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
end;

end.
