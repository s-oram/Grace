unit uVoiceSetupFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer;

type
  TVoiceSetupFrame = class(TFrame)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
