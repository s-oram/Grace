unit eeSyncKeeper;

interface

uses
  eeSyncEvents,
  SysUtils, eeMidiEvents;

type
  PSyncKeeper = ^TSyncKeeper;
  TSyncKeeper = class
  private
    fProcessReplacingOffset: integer;
    fOverSampleFactor: integer;
    fSyncEvents: TSyncEventController;
  protected
    MidiOutput : PeeMidiEventList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Setup_MidiOutput(aMidiOutput : PeeMidiEventList);


    // SendMidiEvent() allows MIDI events to be sent at any time without the complexity
    // of having to calculate the correct MIDI deltaframe value. (Calculating the correct
    // deltaframe value is a little difficult because generally effect's process methods
    // are decoupled from the VST ProcessReplacing() method.
    procedure SendMidiEvent(Status, Channel, Data1, Data2: longint; OffsetFromNow:longint = 0);


    // Important: Keep these properties up to date. The SendMidi() methods use them
    // to calculate the midi event deltaframes value.
    property ProcessReplacingOffset : integer read fProcessReplacingOffset write fProcessReplacingOffset;
    property OverSampleFactor        : integer read fOverSampleFactor       write fOverSampleFactor;

    property SyncEvents : TSyncEventController read fSyncEvents write fSyncEvents;
  end;

implementation

{ TSyncKeeper }

constructor TSyncKeeper.Create;
begin
  OverSampleFactor := 1;
  SyncEvents       := TSyncEventController.Create;
end;

destructor TSyncKeeper.Destroy;
begin
  SyncEvents.Free;
  inherited;
end;

procedure TSyncKeeper.SendMidiEvent(Status, Channel, Data1, Data2: longint; OffsetFromNow:longint = 0);
var
  DeltaFrames : integer;
begin
  DeltaFrames := (OffsetFromNow + ProcessReplacingOffset div OverSampleFactor);
  MidiOutput^.AddEvent(Status, Channel, Data1, Data2, DeltaFrames);
end;

procedure TSyncKeeper.Setup_MidiOutput(aMidiOutput: PeeMidiEventList);
begin
  MidiOutput := aMidiOutput;
end;

end.
