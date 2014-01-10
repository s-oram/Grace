unit eeGuiStandard.RedFoxKnob;

interface

uses
  Classes, Controls;

type
  TRedFoxKnobHandler = class
  public

  published
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Changed(Sender: TObject);
  end;

implementation

{ TRedFoxKnobHandler }

procedure TRedFoxKnobHandler.Changed(Sender: TObject);
begin

end;

procedure TRedFoxKnobHandler.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TRedFoxKnobHandler.MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

end.
