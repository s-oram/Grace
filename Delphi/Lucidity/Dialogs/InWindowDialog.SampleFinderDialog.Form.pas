unit InWindowDialog.SampleFinderDialog.Form;

interface

uses
  InWindowDialog.Prototypes, InWindowDialog.SampleFinderDialog.Brain,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, VamDiv, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer;

type
  TSampleFinderFileFoundEvent = procedure(Sender : TObject; const MissingIndex : integer; const OldFileName, NewFileName : string; var Accept : boolean; var AcceptMessage : string) of object;

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
    StatusLabel1: TLabel;
    StatusLabel2: TLabel;
    procedure MainDialogAreaResize(Sender: TObject);
    procedure ButtonDivResize(Sender: TObject);
  private
    SkipButton : TButton;
    LocateButton  : TButton;
    SearchInButton : TButton;
    AutoSearchButton : TButton;
    CloseDialogButton : TButton;
    Brain : TSampleFinderBrain;
    fOnFileFound: TSampleFinderFileFoundEvent;

    procedure EventHandle_SearchFinished(Sender : TObject);
    procedure EventHandle_UpdateAllControls(Sender : TObject);
    procedure EventHandle_ButtonClick(Sender : TObject);
    procedure EventHandle_SearchPathChanged(Sender : TObject; NewPath : string);
    procedure EventHandle_SearchFinished_FileNotFound(Sender : TObject);
    procedure EventHandle_FileFound(Sender : TObject; const MissingIndex : integer; const OldFileName, NewFileName : string; var Accept : boolean);
  public
    constructor Create(AOwner: TComponent; var MissingFiles, SearchPaths : TStringList); reintroduce;
    destructor Destroy; override;

    property OnFileFound : TSampleFinderFileFoundEvent read fOnFileFound write fOnFileFound;
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
  Brain.OnFinished := EventHandle_SearchFinished;
  Brain.OnSearchPathChanged := EventHandle_SearchPathChanged;
  Brain.OnSearchFinished_FileNotFound := EventHandle_SearchFinished_FileNotFound;
  Brain.OnFileFound := EventHandle_FileFound;

  BackPanel1.Color := GetRedfoxColor(clWindowText);
  BackPanel2.Color := GetRedfoxColor(cl3DLight);
  DialogTextControl.Color := GetRedfoxColor(cl3DLight);
  DialogTextControl.AutoSize := false;
  DialogTextControl.Alignment := TAlignment.taCenter;
  DialogTextControl.Font.Style := [fsBold];

  FileNameLabel.Color := GetRedfoxColor(cl3DLight);
  FullPathLabel.Color := GetRedfoxColor(cl3DLight);
  MissingFileCountLabel.Color := GetRedfoxColor(cl3DLight);
  StatusLabel1.Color := GetRedfoxColor(cl3DLight);
  StatusLabel2.Color := GetRedfoxColor(cl3DLight);

  StatusLabel1.Left := 0;
  StatusLabel2.Left := 0;
  StatusLabel1.AutoSize := false;
  StatusLabel2.AutoSize := false;

  MissingFileCountLabel.AutoSize := false;
  MissingFileCountLabel.Width    := 300;

  SkipButton := TButton.Create(self);
  SkipButton.Caption := 'Skip';

  LocateButton  := TButton.Create(self);
  LocateButton.Caption := 'Locate...';

  SearchInButton := TButton.Create(self);
  SearchInButton.Caption := 'Search In...';

  AutoSearchButton := TButton.Create(self);
  AutoSearchButton.Caption := 'Auto-Search';
  AutoSearchButton.Enabled := false;
  //TODO:HIGH AutoSearch button needs to be enabled.

  CloseDialogButton := TButton.Create(self);
  CloseDialogButton.Caption := 'Close';

  //== set common properties for all buttons ==
  for obj in ObjectArray([SkipButton, LocateButton, SearchInButton, AutoSearchButton, CloseDialogButton]) do
  begin
    (obj as TControl).Parent  := ButtonDiv;
    (obj as TControl).Visible := true;
    (obj as TControl).Height  := kButtonHeight;
    (obj as TButton).OnClick := EventHandle_ButtonClick;
  end;

  AutoSearchButton.Visible := false; //TODO:HIGH delete this when auto search is implemented.

  ButtonDiv.Height := kButtonHeight;

  EventHandle_UpdateAllControls(self);

  //StatusLabel1.Caption := '';
  //StatusLabel2.Caption := '';
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
  DialogTextControl.Width := MainDialogArea.Width;

  FileNameLabel.Left := 0;
  VclLayout(FileNameLabel, DialogTextControl).SnapToBottomEdge.Move(0, 16);

  MissingFileCountLabel.Left := MainDialogArea.Width - MissingFileCountLabel.Width;
  VclLayout(MissingFileCountLabel, DialogTextControl).SnapToBottomEdge.Move(0, 16);

  FileNameEdit.Width := MainDialogArea.Width;
  FileNameEdit.Left := 0;
  VclLayout(FileNameEdit, FileNameLabel).SnapToBottomEdge.Move(0, 4);

  FullPathLabel.Left := 0;
  VclLayout(FullPathLabel, FileNameEdit).SnapToBottomEdge.Move(0, 16);

  FullPathEdit.Width := MainDialogArea.Width;
  FullPathEdit.Left := 0;
  VclLayout(FullPathEdit, FullPathLabel).SnapToBottomEdge.Move(0, 4);

  VclLayout(StatusLabel1, FullPathEdit).SnapToBottomEdge.Move(0, 16);
  VclLayout(StatusLabel2, StatusLabel1).SnapToBottomEdge.Move(0, 2);

  StatusLabel1.Width := MainDialogArea.Width;
  StatusLabel2.Width := MainDialogArea.Width;
end;

procedure TSampleFinderDialogForm.ButtonDivResize(Sender: TObject);
begin
  //VclLayout([SkipButton, LocateButton, SearchInButton, AutoSearchButton, CloseDialogButton]).FitToParentWidth(16);
  VclLayout([SkipButton, LocateButton, SearchInButton, CloseDialogButton]).FitToParentWidth(16);
  SkipButton.Top := 0;
  LocateButton.Top := 0;
  SearchInButton.Top := 0;
  AutoSearchButton.Top := 0;
  CloseDialogButton.Top := 0;
end;



procedure TSampleFinderDialogForm.EventHandle_UpdateAllControls(Sender: TObject);
begin
  if Brain.CurrentMissingFileCount = 1
    then MissingFileCountLabel.Caption := '1 file remaining.'
    else MissingFileCountLabel.Caption := IntToStr(Brain.CurrentMissingFileCount) + ' files remaing.';

  FileNameEdit.Text := Brain.CurrentMissingFileName;
  FullPathEdit.Text := ExtractFilePath(Brain.CurrentMissingFileFullPath);

  // HACK: I'm just assuming this is a good place to have some status labels updated.
  // what probably needs to happen is to have status information provided by the brain
  // instead of just guessing from the outside.
  StatusLabel1.Caption := '';
  StatusLabel2.Caption := '';
end;

procedure TSampleFinderDialogForm.EventHandle_ButtonClick(Sender: TObject);
begin
  if Sender = SkipButton then
  begin
    Brain.Skip;
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
    //TODO:HIGH
  end else
  if Sender = CloseDialogButton then
  begin
    CloseDialog;
  end;
end;

procedure TSampleFinderDialogForm.EventHandle_SearchFinished(Sender: TObject);
begin
  EventHandle_UpdateAllControls(self);

  SkipButton.Enabled := false;
  LocateButton.Enabled := false;
  SearchInButton.Enabled := false;
  AutoSearchButton.Enabled := false;

  CloseDialogButton.SetFocus;

  StatusLabel1.Caption := 'Finished';
  StatusLabel2.Caption := '';

  // TODO:MED it would look better if the finished status message said all files found
  // or 7 of 7 files found.
end;

procedure TSampleFinderDialogForm.EventHandle_SearchFinished_FileNotFound(Sender: TObject);
begin
  StatusLabel1.Caption := 'Search finished. File not found.';
  StatusLabel2.Caption := '';
end;

procedure TSampleFinderDialogForm.EventHandle_SearchPathChanged(Sender: TObject; NewPath: string);
begin
  StatusLabel1.Caption := 'Searching...';
  StatusLabel2.Caption := NewPath;
end;

procedure TSampleFinderDialogForm.EventHandle_FileFound(Sender: TObject; const MissingIndex: integer; const OldFileName, NewFileName: string; var Accept: boolean);
var
  AcceptMessage : string;
begin
  AcceptMessage := '';
  if assigned(OnFileFound) then OnFileFound(self, MissingIndex, OldFileName, NewFileName, Accept, AcceptMessage);

  if Accept = false then
  begin
    StatusLabel1.Caption := AcceptMessage;
    StatusLabel2.Caption := '';
  end;

end;



end.
