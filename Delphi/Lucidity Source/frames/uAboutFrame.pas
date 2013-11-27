unit uAboutFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer;

type


  TAboutFrame = class(TFrame)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;





implementation

{$R *.dfm}

{ TAboutFrame }

constructor TAboutFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TAboutFrame.Destroy;
begin

  inherited;
end;


end.
