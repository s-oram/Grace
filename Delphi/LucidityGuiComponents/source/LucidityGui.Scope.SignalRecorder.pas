unit LucidityGui.Scope.SignalRecorder;

interface

uses
  Types,
  RedFoxColor,
  RedFoxImageBuffer,
  VamLib.MoreTypes;

type
  IScopeSignalRecorder = interface
    ['{A01F1BD5-F4DD-48A2-A466-5AD7C9195647}']
    procedure GetReadPointer(const MaxReadSampleFrames : integer; out ActualReadSampleFrames, ReadIndex, BufferSize : integer; out Buffer : PArrayOfSingle);
  end;

  TSignalDisplay = class
  private
    BackBuffer: TRedFoxImageBuffer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetSize(w,h:integer);


    procedure DrawSignal(Dest : TRedFoxImageBuffer; const DestRect:TRect; const Source: IScopeSignalRecorder);
  end;

implementation

{ TSignalDisplay }

constructor TSignalDisplay.Create;
begin
  BackBuffer := TRedFoxImageBuffer.Create;
end;

destructor TSignalDisplay.Destroy;
begin
  BackBuffer.Free;
  inherited;
end;

procedure TSignalDisplay.SetSize(w, h: integer);
begin
  BackBuffer.SetSize(w, h);
end;

procedure TSignalDisplay.DrawSignal(Dest : TRedFoxImageBuffer; const DestRect:TRect; const Source: IScopeSignalRecorder);
begin
  BackBuffer.BufferInterface.ClearAll(255,0,0,255);
  BackBuffer.RedFoxInterface.BlendTo(Dest.RedFoxInterface, DestRect.Left, DestRect.Top);
end;



end.
