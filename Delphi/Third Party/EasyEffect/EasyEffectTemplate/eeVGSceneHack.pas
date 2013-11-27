unit eeVGSceneHack;

interface

procedure InitGDIP;
procedure FreeGDIP;

implementation

uses
  vg_canvas_gdip;

procedure InitGDIP;
begin
  vg_canvas_gdip.InitGDIP;
end;

procedure FreeGDIP;
begin
  vg_canvas_gdip.FreeGDIP;
end;

end.
