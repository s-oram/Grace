unit djfxExtra;

interface

uses
  Classes, Menus, djfxBase;

procedure BuildFxSelectionMenu(Menu:TPopupMenu; CurrentFx:TDJFXType; OnFxClick:TNotifyEvent);

implementation

procedure BuildFxSelectionMenu(Menu:TPopupMenu; CurrentFx:TDJFXType; OnFxClick:TNotifyEvent);
var
  mi:TMenuItem;
begin
  Menu.Items.Clear;

  mi := TMenuItem.Create(Menu);
  mi.OnClick := OnFxClick;
  mi.Caption := 'None';
  mi.Tag     := TagDXNone;
  if CurrentFx = dxNone then mi.Checked := true;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.OnClick := OnFxClick;
  mi.Caption := 'Lofi A';
  mi.Tag     := TagDXLofiA;
  if CurrentFx = dxLofiA then mi.Checked := true;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.OnClick := OnFxClick;
  mi.Caption := 'Auto Pan';
  mi.Tag     := TagDXAutoPan;
  if CurrentFx = dxAutoPan then mi.Checked := true;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.OnClick := OnFxClick;
  mi.Caption := 'Amp Mod';
  mi.Tag     := TagDXAmpMod;
  if CurrentFx = dxAmpMod then mi.Checked := true;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.OnClick := OnFxClick;
  mi.Caption := 'Pitch Mod';
  mi.Tag     := TagDXPitchMod;
  if CurrentFx = dxPitchMod then mi.Checked := true;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.OnClick := OnFxClick;
  mi.Caption := 'One Pole LP';
  mi.Tag     := TagDXOnePoleLP;
  if CurrentFx = dxOnePoleLP then mi.Checked := true;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.OnClick := OnFxClick;
  mi.Caption := 'Two Pole LP';
  mi.Tag     := TagDXTwoPoleLP;
  if CurrentFx = dxTwoPoleLP then mi.Checked := true;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.OnClick := OnFxClick;
  mi.Caption := 'Moog LP';
  mi.Tag     := TagDXMoogLP;
  if CurrentFx = dxMoogLP then mi.Checked := true;
  Menu.Items.Add(mi);
end;

end.
