unit RedFoxBasicWinControl;

interface

uses
  RedFoxWinControl;

type
  TRedFoxBasicWinControl = class(TRedFoxWinControl)
  private
  public
    procedure Paint; override;
  end;

implementation

{ TRedFoxBasicWinControl }

procedure TRedFoxBasicWinControl.Paint;
begin
  inherited;
  BackBuffer.BufferInterface.ClearAll(255,0,0,125);
  //BackBuffer.BufferInterface.ClearAll(Random(255),Random(255),Random(255),100);
end;

end.
