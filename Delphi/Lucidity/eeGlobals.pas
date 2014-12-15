unit eeGlobals;

interface

{$INCLUDE Defines.inc}

uses
  Lucidity.Interfaces,
  eeCpuMonitor,
  SysUtils,
  ExtCtrls,
  eeStoredActionList,
  Lucidity.GuiState,
  Lucidity.Options,
  eeCustomGlobals, Lucidity.CopyProtection, eeSkinImageLoader.VCL;

type
  TPatchInfo = class
  private
    fPatchFileName: string;
  public
    property PatchFileName : string read fPatchFileName write fPatchFileName; //full path patch file name.
  end;

  TCopyProtection = class
  private
    fKeyData: TLucidityKey;
    fIsRegistered: boolean;
  public
    constructor Create;

    procedure LoadRegistrationKeyFile(FileName : string);
    property KeyData : TLucidityKey read fKeyData;

    property IsRegistered : boolean read fIsRegistered;
  end;


  TGlobals = class(TCustomGlobals)
  private
    fSkinImageLoader: TSkinImageLoader;
    fOptions: TOptions;
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
    fCopyProtection: TCopyProtection;
    procedure SetIsGuiOpen(const Value: boolean);
    function GetLastProgramLoadDir: string;
    function GetLastProgramSaveDir: string;
    procedure SetLastProgramLoadDir(const Value: string);
    procedure SetLastProgramSaveDir(const Value: string);
  protected
  protected
  public
    constructor Create; override;
	  destructor Destroy; override;

    function FindConfigFile(const ConfigFilename : string; Out FullPathLocation : string):boolean;

    property CopyProtection : TCopyProtection read fCopyProtection;

    property DefaultConfigDir : string read fDefaultConfigDir;
    property UserConfigDir    : string read fUserConfigDir;

    property SkinImageLoader : TSkinImageLoader read fSkinImageLoader;

    property Options : TOptions read fOptions;

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

    property LastProgramLoadDir : string read GetLastProgramLoadDir write SetLastProgramLoadDir;
    property LastProgramSaveDir : string read GetLastProgramSaveDir write SetLastProgramSaveDir;
  end;




implementation

uses
  VamLib.ZeroObject,
  uConstants,
  eePluginDataDir;

var
  Global_LastProgramLoadDir : string;
  Global_LastProgramSaveDir : string;



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

  fCopyProtection := TCopyProtection.Create;

  fAudioActions := TStoredActions.Create;

  fGuiState := TGuiState.Create;
  fPatchInfo := TPatchInfo.Create;

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
  fGuiState.Free;
  fPatchInfo.Free;
  fAudioActions.Free;
  fCopyProtection.Free;
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

function TGlobals.GetLastProgramLoadDir: string;
begin
  result := Global_LastProgramLoadDir;
end;

function TGlobals.GetLastProgramSaveDir: string;
begin
  result := Global_LastProgramSaveDir;
end;

procedure TGlobals.SetLastProgramLoadDir(const Value: string);
begin
  Global_LastProgramLoadDir := Value;
end;

procedure TGlobals.SetLastProgramSaveDir(const Value: string);
begin
  Global_LastProgramSaveDir := Value;
end;

procedure TGlobals.SetIsGuiOpen(const Value: boolean);
begin
  fIsGuiOpen := Value;

  if fIsGuiOpen = true then
  begin
    MotherShip.SetIsGuiOpen(true);
  end else
  begin
    MotherShip.SetIsGuiOpen(false);
  end;
end;

{ TCopyProtection }

constructor TCopyProtection.Create;
begin
  fIsRegistered := false;
end;

procedure TCopyProtection.LoadRegistrationKeyFile(FileName: string);
begin
  fKeyData.Clear;

  if FileExists(FileName) then
  begin
    fKeyData.LoadFromFile(FileName);
    if fKeyData.IsKeyChecksumValid then
    begin
      fIsRegistered := true;
    end else
    begin
      fKeyData.Clear;
      fIsRegistered := false;
    end;
  end;
end;



initialization
  Global_LastProgramLoadDir := '';
  Global_LastProgramSaveDir := '';

finalization


end.
