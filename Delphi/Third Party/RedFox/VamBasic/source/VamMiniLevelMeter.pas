unit VamMiniLevelMeter;

interface

uses
  VamGuiControlInterfaces,
  Types, Controls, Classes, Graphics, AggColor,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamGraphicControl, VamWinControl;

type
  TVamMiniLevelMeter = class(TVamWinControl)
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{ TVamMiniLevelMeter }

constructor TVamMiniLevelMeter.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TVamMiniLevelMeter.Destroy;
begin

  inherited;
end;

end.
