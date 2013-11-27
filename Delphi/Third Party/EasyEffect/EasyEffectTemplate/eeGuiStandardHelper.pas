unit eeGuiStandardHelper;

interface

uses
  eeGuiStandard;

type
  TGuiStandardHelper = class
  private
  public
    procedure AssignEventHandlers(GuiStandard : TGuiStandard; c:TObject);
  end;


var
  GuiStandardHelper : TGuiStandardHelper;

implementation

uses
  vam_Knobs, vam_BitmapButton;


{ TGuiStandardHelper }

procedure TGuiStandardHelper.AssignEventHandlers(GuiStandard : TGuiStandard; c:TObject);
begin
  if (c is TvgVamBitmapButton) then
  begin
     (c as TvgVamBitmapButton).OnMouseDown := GuiStandard.vgButtonMouseDown;
  end;

  if (c is TvgVamBitmapKnob) then
  begin
    (c as TvgVamBitmapKnob).OnChange    := GuiStandard.vgKnobChange;
    (c as TvgVamBitmapKnob).OnMouseDown := GuiStandard.vgKnobMouseDown;
    (c as TvgVamBitmapKnob).OnMouseUp   := GuiStandard.vgKnobMouseUp;
  end;
  
  
end;

initialization
  GuiStandardHelper := TGuiStandardHelper.Create;

finalization
  GuiStandardHelper.Free;
end.
