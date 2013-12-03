unit eeGuiContainer;

interface

uses
  {$IFDEF VER230}
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  {$ELSE}
  Windows, Messages,
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
  {$ENDIF}
  eePluginGUI;

type
  TGuiContainer = class(TForm)
    procedure Button1Click(Sender: TObject);
  private

  public
    PluginGUI : TPluginGUI;
  end;


implementation

{$R *.dfm}

procedure TGuiContainer.Button1Click(Sender: TObject);
begin
  //PluginGUI := TPluginGUI.Create(nil);
end;

end.
