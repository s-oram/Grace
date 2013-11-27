{
  InProcessTimer is a timer class that is synced to the audio processing routines.
  - It can be useful because events can be triggered from the audio thread
  - Events are timed relative to the processing.
  - WARNING: This timer isn't designed to be perfectly accurate. When using 44100 sampling rate, there
    will be issues.. 


}

unit eeInProcessTimer;

interface

uses
  Classes;

type
  TInProcessTimer = class
  private
    fTime: cardinal;
    fOnTime: TNotifyEvent;
    fActive: boolean;
    fSampleRate: integer;
    procedure SetSampleRate(const Value: integer);
  protected
    SliceSize:integer;
    SampleFrameCounter:integer;
    TimeCounter:cardinal;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure Start;
    procedure Stop;

    procedure Process(SampleFrames:integer); inline;

    property Active     :boolean  read fActive     write fActive;
    property Time       :cardinal read fTime       write fTime;        //milliseconds. 
    property SampleRate :integer  read fSampleRate write SetSampleRate;

    property OnTime:TNotifyEvent read fOnTime write fOnTime;
  end;

implementation

{ TInProcessTimer }

constructor TInProcessTimer.Create;
begin
  fTime := 1000;     //default to 1000 milliseconds.

  SampleFrameCounter := 0;
  TimeCounter        := 0;
  SampleRate         := 44100;
end;

destructor TInProcessTimer.Destroy;
begin

  inherited;
end;

procedure TInProcessTimer.SetSampleRate(const Value: integer);
begin
  fSampleRate := Value;
  SliceSize   := Value div 1000
end;

procedure TInProcessTimer.Start;
begin
  Active := true;
  SampleFrameCounter := 0;
  TimeCounter        := 0;
end;

procedure TInProcessTimer.Stop;
begin
  Active := false;
end;

procedure TInProcessTimer.Process(SampleFrames: integer);
var
  ms:integer;
begin
  if Active then
  begin
    inc(SampleFrameCounter, SampleFrames);

    ms := SampleFrameCounter mod SliceSize;
    if ms > 0 then
    begin
      SampleFrameCounter := SampleFrameCounter - (ms * SliceSize);
      inc(TimeCounter, ms);

      if TimeCounter >= Time then
      begin
        TimeCounter := 0;
        if assigned(OnTime) then OnTime(self);
      end;
    end;
  end;

end;



end.
