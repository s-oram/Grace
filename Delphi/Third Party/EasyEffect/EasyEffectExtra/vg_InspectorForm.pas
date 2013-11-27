unit vg_InspectorForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, vg_controls, vg_layouts, vg_treeview, vg_inspector, vg_scene;

type
  TInspectorForm = class(TForm)
    vgScene1: TvgScene;
    Root1: TvgBackground;
    Inspector1: TvgInspector;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
