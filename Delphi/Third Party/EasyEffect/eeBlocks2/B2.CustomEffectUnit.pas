unit B2.CustomEffectUnit;

interface

uses
  eeGlobals;

type
  TCustomEffectUnit = class
  private
  protected
    Globals : TGlobals;
    procedure EventHandler_SampleRateChanged(Sender:TObject); virtual;
    procedure EventHandler_BlockSizeChanged(Sender:TObject); virtual;
    procedure EventHandler_TempoChanged(Sender:TObject); virtual;
    procedure EventHandler_VstSuspendEvent(Sender:TObject); virtual;
    procedure EventHandler_VstResumeEvent(Sender:TObject); virtual;
  public
    constructor Create(aGlobals:TGlobals); virtual;
    destructor Destroy; override;
  end;

implementation

{ TCustomEffectUnit }

constructor TCustomEffectUnit.Create(aGlobals: TGlobals);
begin
  Globals := aGlobals;
  Globals.AddEventListener(TPluginEvent.SampleRateChanged, EventHandler_SampleRateChanged);
  Globals.AddEventListener(TPluginEvent.BlockSizeChanged,  EventHandler_BlockSizeChanged);
  Globals.AddEventListener(TPluginEvent.TempoChanged,      EventHandler_TempoChanged);
  Globals.AddEventListener(TPluginEvent.VstSuspendEvent,   EventHandler_VstSuspendEvent);
  Globals.AddEventListener(TPluginEvent.VstResumeEvent,    EventHandler_VstResumeEvent);
end;

destructor TCustomEffectUnit.Destroy;
begin
  Globals.RemoveEventListener(TPluginEvent.SampleRateChanged, EventHandler_SampleRateChanged);
  Globals.RemoveEventListener(TPluginEvent.BlockSizeChanged,  EventHandler_BlockSizeChanged);
  Globals.RemoveEventListener(TPluginEvent.TempoChanged,      EventHandler_TempoChanged);
  Globals.RemoveEventListener(TPluginEvent.VstSuspendEvent,   EventHandler_VstSuspendEvent);
  Globals.RemoveEventListener(TPluginEvent.VstResumeEvent,    EventHandler_VstResumeEvent);
  inherited;
end;

procedure TCustomEffectUnit.EventHandler_BlockSizeChanged(Sender: TObject);
begin

end;

procedure TCustomEffectUnit.EventHandler_SampleRateChanged(Sender: TObject);
begin

end;

procedure TCustomEffectUnit.EventHandler_TempoChanged(Sender: TObject);
begin

end;

procedure TCustomEffectUnit.EventHandler_VstResumeEvent(Sender: TObject);
begin

end;

procedure TCustomEffectUnit.EventHandler_VstSuspendEvent(Sender: TObject);
begin

end;

end.
