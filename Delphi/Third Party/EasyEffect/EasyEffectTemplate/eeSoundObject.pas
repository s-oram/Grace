unit eeSoundObject;

interface

uses
  eeGlobals;

type

  {
    SoundObject is the base class for all sound and modulation obects.
    Oscilators, filters, envelopes etc.
  }
  ISoundObject = interface
    ['{90661E87-6654-4470-A480-069285E54E69}']

  end;

  TSoundObject = class(TInterfacedObject)
  strict private
    fGlobals : TGlobals;
    fObjectName: string;
  private
  strict protected
    property Globals : TGlobals read fGlobals;
  protected
  public
    constructor Create(aObjectName : string; aGlobals : TGlobals); virtual;
    destructor Destroy; override;

    property ObjectName : string read fObjectName;
  end;

implementation

{ TSoundObject }

constructor TSoundObject.Create(aObjectName : string; aGlobals : TGlobals);
begin
  fGlobals    := aGlobals;
  fObjectName := aObjectName;
end;

destructor TSoundObject.Destroy;
begin

  inherited;
end;

end.
