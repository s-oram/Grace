unit eeGlobals;

interface

{$INCLUDE Defines.inc}

uses
  Lucidity.Interfaces,
  eeCpuMonitor,
  SysUtils,
  ExtCtrls,
  eeStoredActionList,
  VamLib.ManagedObject,
  uGuiState,
  Lucidity.Options, eeTaskRunner,
  eeCustomGlobals, Lucidity.CopyProtection, eeSkinImageLoader.VCL;

type
  TPatchInfo = class
  private
    fPatchFileName: string;
  public
    property PatchFileName : string read fPatchFileName write fPatchFileName; //full path patch file name.
  end;


  TGlobals = class(TCustomGlobals)
  private
    fKeyData: TLucidityKey;
    fSkinImageLoader: TSkinImageLoader;
    fOptions: TOptions;
    fSelectedModSlot: integer;
    fIsMouseOverModSlot: boolean;
    fMouseOverModSlot: integer;
    fIsGuiOpen: boolean;
    fGuiState: TGuiState;
    fAudioActions: TStoredActions;
    fSampleMapReference: TObject;
    fKeyGroupsReference: TObject;
    fCpuMonitor: TCpuMonitor;
    fCpuSampleFrames: integer;
    fCpuSampleRate: integer;
    fUserConfigDir: string;
    fDefaultConfigDir: string;
    fPatchInfo: TPatchInfo;
    procedure SetSelectedModSlot(const Value: integer);
    procedure SetIsGuiOpen(const Value: boolean);
  protected
  protected
    VclTaskRunner : TTaskRunner;
    VclTaskTimer  : TTimer;
    procedure OnVclTimer(Sender : TObject);
  public
    constructor Create; override;
	  destructor Destroy; override;

    function FindConfigFile(const ConfigFilename : string; Out FullPathLocation : string):boolean;

    procedure AddVclTask(aTask : TProc);

    procedure LoadRegistrationKeyFile(FileName : string);

    property KeyData : TLucidityKey read fKeyData;

    property DefaultConfigDir : string read fDefaultConfigDir;
    property UserConfigDir    : string read fUserConfigDir;

    property SkinImageLoader : TSkinImageLoader read fSkinImageLoader;

    property Options : TOptions read fOptions;


    // TODO:MED the selected stuff below probably show go into the GUI state.
    // SelectedModSlot shows which mod slot is active. valid range is -1..7.
    //   -1 indicates that the "Main" is selected and no mod slot is being edited.
    //   0..7 indicates that the X mod slot is being edited. It should be displayed.
    property SelectedModSlot    : integer read fSelectedModSlot    write SetSelectedModSlot;
    property IsMouseOverModSlot : boolean read fIsMouseOverModSlot write fIsMouseOverModSlot;
    property MouseOverModSlot   : integer read fMouseOverModSlot   write fMouseOverModSlot; //valid range is -1..7, same as SelectedModSlot.

    property IsGuiOpen : boolean read fIsGuiOpen write SetIsGuiOpen;

    property GuiState : TGuiState read fGuiState;
    property PatchInfo : TPatchInfo read fPatchInfo;


    // TODO:MED AudioActions isn't being used at all. The role of audio actions,
    // being able to pass events to be processed at the end of the audio processing
    // block does seem useful however. I won't delete it right away.
    property AudioActions : TStoredActions read fAudioActions;

    //=== some object references =====
    property SampleMapReference : TObject read fSampleMapReference write fSampleMapReference;
    property KeyGroupsReference : TObject read fKeyGroupsReference write fKeyGroupsReference;
    //================================

    //HACK: including this here is very hacky.
    property CpuMonitor : TCpuMonitor read fCpuMonitor write fCpuMonitor;

    property CpuSampleFrames : integer read fCpuSampleFrames write fCpuSampleFrames;
    property CpuSampleRate   : integer read fCpuSampleRate   write fCpuSampleRate;
  end;

implementation

uses
  VamLib.ZeroObject,
  uConstants,
  eePluginDataDir,
  eeFunctions;


{ TGlobals }

constructor TGlobals.Create;
  procedure LoadSkinImage(const SkinImageName : string);
  begin
    if fSkinImageLoader.LoadImage(SkinImageName) = false then raise Exception.Create('Skin image not found. (' + SkinImageName + ')');
  end;
var
  DataDir : string;
  fn : string;
begin
  inherited;

  fAudioActions := TStoredActions.Create;

  fGuiState := TGuiState.Create;
  fPatchInfo := TPatchInfo.Create;

  VclTaskRunner := TTaskRunner.Create;
  VclTaskTimer  := TTimer.Create(nil);
  VclTaskTimer.Enabled := false;
  VclTaskTimer.Interval := 25;
  VclTaskTimer.OnTimer := OnVclTimer;

  fSelectedModSlot := -1;

  fKeyData.Clear;

  fDefaultConfigDir := '';
  fUserConfigDir    := '';

  if (PluginDataDir^.Exists) then
  begin
    // Find or Create User data directory.
    DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + 'Config Default';
    if (DirectoryExists(DataDir) = false) then CreateDir(DataDir);

    if DirectoryExists(DataDir)
      then fDefaultConfigDir := DataDir
      else fDefaultConfigDir := '';


    // find or create factory data directory.
    DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + 'Config User Override';
    if (DirectoryExists(DataDir) = false) then CreateDir(DataDir);

    if DirectoryExists(DataDir)
      then fUserConfigDir := DataDir
      else fUserConfigDir := '';
  end;


  fOptions := TOptions.Create;
  if DirectoryExists(UserConfigDir) then
  begin
    fn := IncludeTrailingPathDelimiter(UserConfigDir) + 'Lucidity Options.xml';
    if FileExists(fn) then
    begin
      Options.ReadFromFile(fn);
    end;
    Options.AutoSaveFileName := fn;
  end;



  fSkinImageLoader := TSkinImageLoader.Create;

  LoadSkinImage('Background');
  LoadSkinImage('Small_Knob');
  LoadSkinImage('Knob_Upper');
  LoadSkinImage('Knob_Lower');
  LoadSkinImage('Knob_UniPolar');
  LoadSkinImage('Knob_BiPolar');
  LoadSkinImage('Knob_PlaceHolder');
  LoadSkinImage('ModMatrix_Knob');
  LoadSkinImage('ButtonOff');
  LoadSkinImage('ButtonOn');
  LoadSkinImage('Browser_FolderIcon');
  LoadSkinImage('Browser_AudioIcon');
  LoadSkinImage('Browser_ProgramIcon');
  LoadSkinImage('Menu_SampleEditIcon');
  LoadSkinImage('Menu_ProgramIcon');
  LoadSkinImage('Locked_Icon');
  LoadSkinImage('Unlocked_Icon');
  LoadSkinImage('Preview_Icon');
  LoadSkinImage('Switch_Background');
  LoadSkinImage('Switch_Index');

end;

destructor TGlobals.Destroy;
begin
  fSkinImageLoader.Free;
  fOptions.Free;
  VclTaskTimer.Free;
  VclTaskRunner.Free;
  fGuiState.Free;
  fPatchInfo.Free;
  fAudioActions.Free;
  inherited;
end;

function TGlobals.FindConfigFile(const ConfigFilename: string; out FullPathLocation: string): boolean;
var
  TestFilename : string;
begin
  // First, check for the config in the user overrides directory
  if UserConfigDir <> '' then
  begin
    TestFileName := IncludeTrailingPathDelimiter(UserConfigDir) + ConfigFileName;
    if FileExists(TestFileName) then
    begin
      FullPathLocation := TestFileName;
      exit(true); //============================================================>> exit >>=============>>
    end;
  end;

  // Then check in the default config directory.
  if DefaultConfigDir <> '' then
  begin
    TestFileName := IncludeTrailingPathDelimiter(DefaultConfigDir) + ConfigFileName;
    if FileExists(TestFileName) then
    begin
      FullPathLocation := TestFileName;
      exit(true); //============================================================>> exit >>=============>>
    end;
  end;


  // if still not found, it doesn't exist;
  FullPathLocation := '';
  result := false;
end;

procedure TGlobals.LoadRegistrationKeyFile(FileName: string);
var
  SourceFileName : string;
  DestFileName   : string;
begin
  fKeyData.Clear;

  if FileExists(FileName) then
  begin
    fKeyData.LoadFromFile(FileName);
    if fKeyData.IsKeyChecksumValid then
    begin
      if (UserConfigDir <> '') then
      begin
        SourceFileName := FileName;
        DestFileName   := IncludeTrailingPathDelimiter(UserConfigDir) + kKeyFileName;
        CopyFile(SourceFileName, DestFileName);
      end;
    end else
    begin
      fKeyData.Clear;
    end;
  end;
end;

procedure TGlobals.SetIsGuiOpen(const Value: boolean);
begin
  fIsGuiOpen := Value;

  if fIsGuiOpen = true then
  begin
    VclTaskTimer.Enabled := true;

    MotherShip.SetIsGuiOpen(true);
  end else
  begin
    VclTaskTimer.Enabled := false;
    VclTaskRunner.Clear;

    MotherShip.SetIsGuiOpen(false);
  end;
end;

procedure TGlobals.SetSelectedModSlot(const Value: integer);
begin
  assert(Value >= -1);
  assert(Value <= kModSlotCount-1);

  if Value <> fSelectedModSlot then
  begin
    fSelectedModSlot := Value;
  end;
end;

procedure TGlobals.OnVclTimer(Sender: TObject);
begin
  VclTaskRunner.RunTasks;
end;

procedure TGlobals.AddVclTask(aTask: TProc);
begin
  if IsGuiOpen
    then VclTaskRunner.AddTask(aTask)
    else aTask := nil;
end;




end.
