unit uScrollPanelFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VamDiv, VamScrollBox,
  RedFoxWinControl, VamWinControl, VamPanel, RedFoxContainer;

type
  TScrollPanelFrame = class(TFrame)
    Panel: TRedFoxContainer;
    ScrollPanel: TVamPanel;
    MainPanelScrollBox: TVamScrollBox;
    MainPanelOuterDiv: TVamDiv;
    MainPanelInnerDiv: TVamDiv;
  private
    fScrollPosY: single;
    procedure SetScrollPosY(const Value: single);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame;

    procedure UpdateGui(Sender:TObject);

    property ScrollPosY : single read fScrollPosY write SetScrollPosY;
  end;

implementation

uses
  Vcl.StdCtrls;

{$R *.dfm}

{ TScrollPanelFrame }

constructor TScrollPanelFrame.Create(AOwner: TComponent);
begin
  inherited;


end;

destructor TScrollPanelFrame.Destroy;
begin

  inherited;
end;

procedure TScrollPanelFrame.InitializeFrame;
begin
  MainPanelScrollBox.ScrollBars := ssNone;

  MainPanelInnerDiv.Top := 0;
  MainPanelInnerDiv.Left := 0;
  MainPanelInnerDiv.Constraints.MinHeight := MainPanelScrollBox.Height;
  MainPanelInnerDiv.Constraints.MinWidth  := MainPanelInnerDiv.Parent.ClientRect.Width;
  MainPanelInnerDiv.AutoSize := true;
end;

procedure TScrollPanelFrame.SetScrollPosY(const Value: single);
begin
  if Value <> fScrollPosY then
  begin
    fScrollPosY := Value;
    MainPanelInnerDiv.Top := round(-((MainPanelInnerDiv.Height - MainPanelScrollBox.Height) * (1-Value)));
  end;
end;

procedure TScrollPanelFrame.UpdateGui(Sender: TObject);
begin
  if ScrollPosY <> MainPanelScrollBox.ScrollYPos
    then ScrollPosY := MainPanelScrollBox.ScrollYPos;
end;

end.
