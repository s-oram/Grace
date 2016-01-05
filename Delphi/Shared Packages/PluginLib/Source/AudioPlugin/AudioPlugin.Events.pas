unit AudioPlugin.Events;

interface

uses
  Helm.Message;

type
  // This event should only be trigger by parameter changes sent from the host.
  // Parameter changes from the GUI will need to be propagated using another event
  // or other techniques.
  TVstParameterChanged = class(TAppMessage)
  public
    class function Create(const VstParId : integer; const ParValue : single):THelmMessage;
    property VstParID : integer index 0 read GetDataAsInteger;
    property ParValue : single  index 1 read GetDataAsSingle;
  end;

  THostTempoChangedEvent = class(TAppMessage)
  public
    class function Create(const BPM : single):THelmMessage;
    property BPM : single index 0 read GetDataAsSingle;
  end;

  THostTransportChangedEvent = class(TAppMessage)
  public
    class function Create(const IsPlaying, IsRecording, IsCycling : boolean):THelmMessage;
    property IsPlaying   : boolean index 0 read GetDataAsBoolean;
    property IsRecording : boolean index 1 read GetDataAsBoolean;
    property IsCycling   : boolean index 2 read GetDataAsBoolean;
  end;

implementation

type
  TProtectedMessageHack = class(TAppMessage);

{ TVstParameterChanged }

class function TVstParameterChanged.Create(const VstParId: integer; const ParValue: single): THelmMessage;
var
  msg : THelmMessage;
begin
  assert(VstParId >= 0);
  assert(ParValue >= 0);
  assert(ParValue <= 1);

  msg := THelmMessage.Create(TVstParameterChanged);

  TProtectedMessageHack(msg).SetDataAsInteger(0, VstParId);
  TProtectedMessageHack(msg).SetDataAsSingle(1, ParValue);

  result := msg;
end;


{ TTempoChanged }

class function THostTempoChangedEvent.Create(const BPM: single): THelmMessage;
var
  msg : THelmMessage;
begin
  msg := THelmMessage.Create(THostTempoChangedEvent);
  TProtectedMessageHack(msg).SetDataAsSingle(0, BPM);
  result := msg;
end;

{ THostTransportChangedEvent }

class function THostTransportChangedEvent.Create(const IsPlaying, IsRecording, IsCycling: boolean): THelmMessage;
var
  msg : THelmMessage;
begin
  msg := THelmMessage.Create(THostTransportChangedEvent);
  TProtectedMessageHack(msg).SetDataAsBoolean(0, IsPlaying);
  TProtectedMessageHack(msg).SetDataAsBoolean(1, IsRecording);
  TProtectedMessageHack(msg).SetDataAsBoolean(2, IsCycling);
  result := msg;
end;

end.
