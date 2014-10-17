unit InWindowDialog.SampleFinderDialog.Form;

interface

uses
  InWindowDialog.Prototypes, InWindowDialog.SampleFinderDialog.Brain,
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
    MissingFileCountLabel: TLabel;
    procedure MainDialogAreaResize(Sender: TObject);
  private
    SkipButton : TButton;
    SkipAllButton : TButton;
    LocateButton  : TButton;
    SearchInButton : TButton;
    AutoSearchButton : TButton;

    Brain : TSampleFinderBrain;

    procedure EventHandle_SearchFinished(Sender : TObject);
    procedure EventHandle_UpdateAllControls(Sender : TObject);
    procedure EventHandle_ButtonClick(Sender : TObject);
    procedure EventHandle_SearchPathChanged(Sender : TObject; NewPath : string);
  public
    constructor Create(AOwner: TComponent; var MissingFiles, SearchPaths : TStringList); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  VamLib.Utils,
  VamLib.VclLayout,
  RedFoxColor;

{$R *.dfm}

{ TSampleFinderDialogForm }

constructor TSampleFinderDialogForm.Create(AOwner: TComponent; var MissingFiles, SearchPaths : TStringList);
const
  kButtonHeight = 28;
var
  obj : TObject;
begin
  inherited Create(AOwner);

  Brain := TSampleFinderBrain.Create(MissingFiles, SearchPaths);
  Brain.OnUpdateMainView := EventHandle_UpdateAllControls;
  Brain.OnSearchFinished := EventHandle_SearchFinished;
  Brain.OnSearchPathChanged := EventHandle_SearchPathChanged;

  BackPanel1.Color := GetRedfoxColor(clWindowText);
  BackPanel2.Color := GetRedfoxColor(cl3DLight);
  DialogTextControl.Color := GetRedfoxColor(cl3DLight);

  FileNameLabel.Color := GetRedfoxColor(cl3DLight);
  FullPathLabel.Color := GetRedfoxColor(cl3DLight);
  MissingFileCountLabel.Color := GetRedfoxColor(cl3DLight);

  MissingFileCountLabel.AutoSize := false;
  MissingFileCountLabel.Width    := 300;

  SkipButton := TButton.Create(self);
  SkipButton.Caption := 'Skip';

  SkipAllButton := TButton.Create(self);
  SkipAllButton.Caption := 'Skip All';

  LocateButton  := TButton.Create(self);
  LocateButton.Caption := 'Locate...';

  SearchInButton := TButton.Create(self);
  SearchInButton.Caption := 'Search In...';

  AutoSearchButton := TButton.Create(self);
  AutoSearchButton.Caption := 'Auto-Search';

  //== set common properties for all buttons ==
  for obj in ObjectArray([SkipButton, SkipAllButton, LocateButton, SearchInButton, AutoSearchButton]) do
  begin
    (obj as TControl).Parent  := ButtonDiv;
    (obj as TControl).Visible := true;
    (obj as TControl).Height  := kButtonHeight;
    (obj as TButton).OnClick := EventHandle_ButtonClick;
  end;

  ButtonDiv.Height := kButtonHeight;

  EventHandle_UpdateAllControls(self);
end;

destructor TSampleFinderDialogForm.Destroy;
begin
  Brain.Free;
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

  MissingFileCountLabel.Left := MainDialogArea.Width - MissingFileCountLabel.Width;
  VclLayout(MissingFileCountLabel, FileNameEdit).SnapToBottomEdge.Move(0, 2);

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

procedure TSampleFinderDialogForm.EventHandle_UpdateAllControls(Sender: TObject);
begin
  if Brain.CurrentMissingFileCount = 1
    then MissingFileCountLabel.Caption := '1 file remaining.'
    else MissingFileCountLabel.Caption := IntToStr(Brain.CurrentMissingFileCount) + ' files remaing.';

  FileNameEdit.Text := Brain.CurrentMissingFileName;
  FullPathEdit.Text := ExtractFilePath(Brain.CurrentMissingFileFullPath);
end;

procedure TSampleFinderDialogForm.EventHandle_ButtonClick(Sender: TObject);
begin
  if Sender = SkipButton then
  begin
    Brain.Skip;
  end else
  if Sender = SkipAllButton then
  begin
    self.CloseDialog;
  end else
  if Sender = LocateButton then
  begin
    Brain.LocateFile;
  end else
  if Sender = SearchInButton then
  begin
    Brain.SearchIn;
  end else
  if Sender = AutoSearchButton then
  begin

  end;
end;

procedure TSampleFinderDialogForm.EventHandle_SearchFinished(Sender: TObject);
begin
  CloseDialog;
end;

procedure TSampleFinderDialogForm.EventHandle_SearchPathChanged(Sender: TObject; NewPath: string);
begin
  FullPathLabel.Caption := NewPath;
end;

end.
