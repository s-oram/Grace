unit uModSystem2Frame;

interface

uses
  uConstants,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, VamModSelector, VamTextBox;

type
  TModSystem2Frame = class(TFrame)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
  private
    ModSelectors : array[0..kModSlots-1] of TVamModSelector;
    MainSelector : TVamTextBox;
    ForwardSelector : TVamTextBox;
    BackwardSelector : TVamTextBox;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  RedFox, RedFoxColor;

{$R *.dfm}

{ TModSystem2Frame }

constructor TModSystem2Frame.Create(AOwner: TComponent);
var
  c1: Integer;
  PosX, PosY : integer;
  OffsetX : integer;

begin
  inherited;


  //560 - width of mod slots

  OffsetX := 64 + 4 + 4;
  PosY := 4;


  for c1 := 0 to kModSlots-1 do
  begin
    ModSelectors[c1] := TVamModSelector.Create(AOwner);
    ModSelectors[c1].Parent := BackgroundPanel;
    ModSelectors[c1].Visible := true;

    PosX := OffsetX + c1 * (62 + 4);

    ModSelectors[c1].Layout.SetSize(62,62);
    ModSelectors[c1].Layout.SetPos(PosX, PosY);
  end;


  MainSelector     := TVamTextBox.Create(AOwner);
  MainSelector.Parent := BackgroundPanel;
  MainSelector.Visible := true;
  MainSelector.Layout.SetSize(64, 28);
  MainSelector.Layout.SetPos(4,4);

  BackwardSelector := TVamTextBox.Create(AOwner);
  BackwardSelector.Parent := BackgroundPanel;
  BackwardSelector.Visible := true;
  BackwardSelector.Layout.SetSize(29, 29);
  BackwardSelector.Layout.SetPos(4,36);

  ForwardSelector  := TVamTextBox.Create(AOwner);
  ForwardSelector.Parent := BackgroundPanel;
  ForwardSelector.Visible := true;
  ForwardSelector.Layout.SetSize(29, 29);
  ForwardSelector.Layout.SetPos(38,36);



  MainSelector.Text := 'Main';
  MainSelector.TextAlign := TRedFoxAlign.AlignCenter;
  MainSelector.TextVAlign := TRedFoxAlign.AlignCenter;

  ForwardSelector.Text := '>';
  ForwardSelector.TextAlign := TRedFoxAlign.AlignCenter;
  ForwardSelector.TextVAlign := TRedFoxAlign.AlignCenter;

  BackwardSelector.Text := '<';
  BackwardSelector.TextAlign := TRedFoxAlign.AlignCenter;
  BackwardSelector.TextVAlign := TRedFoxAlign.AlignCenter;


  MainSelector.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  BackwardSelector.Font.Color := GetRedFoxColor(kColor_LcdDark5);
  ForwardSelector.Font.Color  := GetRedFoxColor(kColor_LcdDark5);

  MainSelector.Color     := kColor_LcdDark1;
  BackwardSelector.Color := kColor_LcdDark1;
  ForwardSelector.Color  := kColor_LcdDark1;

  MainSelector.ColorMouseOver     := kColor_ButtonMouseOver;
  BackwardSelector.ColorMouseOver := kColor_ButtonMouseOver;
  ForwardSelector.ColorMouseOver  := kColor_ButtonMouseOver;



end;

destructor TModSystem2Frame.Destroy;
begin

  inherited;
end;

end.
