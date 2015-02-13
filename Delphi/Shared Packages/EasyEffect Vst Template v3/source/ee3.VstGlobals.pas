unit ee3.VstGlobals;

interface

uses
  DAEffect,
  DAEffectX,
  DAudioEffectX,
  VamLib.ZeroObject;

type
  TVstMsgID = record
  const
    SampleRateChanged = 1;
    BlockSizeChanged  = 2;
    GuiOpening        = 3; // called before the GUI is visible.
    GuiOpened         = 4; // called after the GUI is vislble.
    GuiClosed         = 5;
    Suspend           = 6;
    Resume            = 7;
    UserMsgID = 1000;
  end;

  TVstGlobals = class
  private
    FMotherShip: TMotherShip;
  protected
    Effect : AudioEffectX;
  public
    constructor Create(aEffect : AudioEffectX);
    destructor Destroy; override;

    property MotherShip : TMotherShip read FMotherShip;
  end;

implementation

{ TPluginGlobals }

constructor TVstGlobals.Create(aEffect : AudioEffectX);
begin
  Effect := aEffect;
  FMotherShip := TMotherShip.Create;
end;

destructor TVstGlobals.Destroy;
begin
  FMotherShip.Free;
  inherited;
end;

end.
