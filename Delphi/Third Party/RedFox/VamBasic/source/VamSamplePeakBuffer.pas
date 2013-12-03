unit VamSamplePeakBuffer;

interface

type
  TPeakBuffer = class;
  IPeakBuffer = interface;

  IPeakBuffer = interface
    ['{E925DFD8-A07D-403A-A0FD-7D4AFE96C27F}']
  end;

  TPeakBuffer = class(TInterfacedObject, IPeakBuffer)
  private
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TPeakBuffer }

constructor TPeakBuffer.Create;
begin

end;

destructor TPeakBuffer.Destroy;
begin

  inherited;
end;

end.
