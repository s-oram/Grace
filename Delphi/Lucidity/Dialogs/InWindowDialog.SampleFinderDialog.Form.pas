unit InWindowDialog.SampleFinderDialog.Form;

interface

uses
  InWindowDialog.Prototypes,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, VamDiv, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer;

type
  TSampleFinderDialogForm = class(TPluginDialogForm)
    RedFoxContainer1: TRedFoxContainer;
    BackPanel1: TVamPanel;
    BackPanel2: TVamPanel;
    ButtonDiv: TVamDiv;
    MainDialogArea: TVamDiv;
    DialogTextControl: TLabel;
    FilenameLabel: TLabel;
    FullPathLabel: TLabel;
    FilenameEdit: TEdit;
    FullPathEdit: TEdit;
    procedure MainDialogAreaResize(Sender: TObject);
  private

    SkipButton : TButton;
    SkipAllButton : TButton;
    LocateButton  : TButton;
    SearchInButton : TButton;
    AutoSearchButton : TButton;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  VamLib.VclLayout,
  RedFoxColor;

{$R *.dfm}

{ TSampleFinderDialogForm }

constructor TSampleFinderDialogForm.Create(AOwner: TComponent);
const
  kButtonHeight = 28;
begin
  inherited;

  BackPanel1.Color := GetRedfoxColor(clWindowText);
  BackPanel2.Color := GetRedfoxColor(cl3DLight);
  DialogTextControl.Color := GetRedfoxColor(cl3DLight);


  FileNameLabel.Color := GetRedfoxColor(cl3DLight);
  FullPathLabel.Color := GetRedfoxColor(cl3DLight);


  SkipButton := TButton.Create(self);
  SkipButton.Caption := 'Skip';
  SkipButton.Parent := ButtonDiv;
  SkipButton.Visible := true;
  SkipButton.Height  := kButtonHeight;

  SkipAllButton := TButton.Create(self);
  SkipAllButton.Caption := 'Skip All';
  SkipAllButton.Parent := ButtonDiv;
  SkipAllButton.Visible := true;
  SkipAllButton.Height  := kButtonHeight;

  LocateButton  := TButton.Create(self);
  LocateButton.Caption := 'Locate...';
  LocateButton.Parent := ButtonDiv;
  LocateButton.Visible := true;
  LocateButton.Height  := kButtonHeight;

  SearchInButton := TButton.Create(self);
  SearchInButton.Caption := 'Search In...';
  SearchInButton.Parent := ButtonDiv;
  SearchInButton.Visible := true;
  SearchInButton.Height  := kButtonHeight;

  AutoSearchButton := TButton.Create(self);
  AutoSearchButton.Caption := 'Auto-Search';
  AutoSearchButton.Parent := ButtonDiv;
  AutoSearchButton.Visible := true;
  AutoSearchButton.Height  := kButtonHeight;

  ButtonDiv.Height := kButtonHeight;
end;

destructor TSampleFinderDialogForm.Destroy;
begin

  inherited;
end;

procedure TSampleFinderDialogForm.MainDialogAreaResize(Sender: TObject);
begin
  DialogTextControl.Top := 0;
  DialogTextControl.Left := 0;

  FileNameEdit.Width := MainDialogArea.Width;
  FileNameEdit.Left := 0;
  VclLayout(FileNameEdit, DialogTextControl).SnapToBottomEdge.Move(0, 16);

  FileNameLabel.Left := 2;
  VclLayout(FileNameLabel, FileNameEdit).SnapToBottomEdge.Move(0, 2);


  FullPathEdit.Width := MainDialogArea.Width;
  FullPathEdit.Left := 0;
  VclLayout(FullPathEdit, FileNameLabel).SnapToBottomEdge.Move(0, 16);

  FullPathLabel.Left := 2;
  VclLayout(FullPathLabel, FullPathEdit).SnapToBottomEdge.Move(0, 2);


  VclLayout([SkipButton, SkipAllButton, LocateButton, SearchInButton, AutoSearchButton]).FitToParentWidth(16);

  SkipButton.Top := 0;
  SkipAllButton.Top := 0;
  LocateButton.Top := 0;
  SearchInButton.Top := 0;
  AutoSearchButton.Top := 0;

end;

end.
