unit eeGlobals;

interface

uses
  Lucidity.Options,
  eeCustomGlobals, Lucidity.CopyProtection, eeSkinImageLoader.VCL,
  eeGlobals.InfoBarReceiver;

type
  TGlobals = class(TCustomGlobals)
  private
    fKeyData: TLucidityKey;
    fFactoryDataDir: string;
    fUserDataDir: string;
    fSkinImageLoader: TSkinImageLoader;
    fOptions: TOptions;
    fInfoBarReceiver: TInfoBarReceiver;
    fSelectedModSlot: integer;
    fIsMouseOverModSlot: boolean;
    fMouseOverModSlot: integer;
    fSelectedLfo: integer;
    procedure SetSelectedModSlot(const Value: integer);
    procedure SetSelectedLfo(const Value: integer);
  protected
  protected
  public
    constructor Create; override;
	  destructor Destroy; override;

    procedure LoadRegistrationKeyFile(FileName : string);

    property KeyData : TLucidityKey read fKeyData;

    property FactoryDataDir : string read fFactoryDataDir;
    property UserDataDir    : string read fUserDataDir;

    property SkinImageLoader : TSkinImageLoader read fSkinImageLoader;

    property Options : TOptions read fOptions;

    // InfoBarReceiver is a central point for controls to send messages for the info bar to.
    // The info bar can then respond to changes and display the message.
    property InfoBarReceiver : TInfoBarReceiver read fInfoBarReceiver write fInfoBarReceiver;

    // SelectedModSlot shows which mod slot is active. valid range is -1..7.
    //   -1 indicates that the "Main" is selected and no mod slot is being edited.
    //   0..7 indicates that the X mod slot is being edited. It should be displayed.
    property SelectedModSlot    : integer read fSelectedModSlot    write SetSelectedModSlot;
    property IsMouseOverModSlot : boolean read fIsMouseOverModSlot write fIsMouseOverModSlot;
    property MouseOverModSlot   : integer read fMouseOverModSlot   write fMouseOverModSlot; //valid range is -1..7, same as SelectedModSlot.

    property SelectedLfo        : integer read fSelectedLfo        write SetSelectedLfo;

  end;

implementation

uses
  VamLib.Throttler,
  SysUtils,
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

  fSelectedModSlot := -1;

  fInfoBarReceiver := TInfoBarReceiver.Create;

  fKeyData.Clear;

  fFactoryDataDir := '';
  fUserDataDir    := '';

  if (PluginDataDir^.Exists) then
  begin
    // Find or Create User data directory.
    DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + 'User';
    if (DirectoryExists(DataDir) = false) then CreateDir(DataDir);

    if DirectoryExists(DataDir)
      then fUserDataDir := DataDir
      else fUserDataDir := '';


    // find or create factory data directory.
    DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + 'Factory';
    if (DirectoryExists(DataDir) = false) then CreateDir(DataDir);

    if DirectoryExists(DataDir)
      then fFactoryDataDir := DataDir
      else fFactoryDataDir := '';

  end;


  fOptions := TOptions.Create;


  if DirectoryExists(UserDataDir) then
  begin
    fn := IncludeTrailingPathDelimiter(UserDataDir) + 'Lucidity Options.xml';
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
  fInfoBarReceiver.Free;
  fSkinImageLoader.Free;
  fOptions.Free;
  inherited;
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
      if (UserDataDir <> '') then
      begin
        SourceFileName := FileName;
        DestFileName   := IncludeTrailingPathDelimiter(UserDataDir) + kKeyFileName;
        CopyFile(sourceFileName, DestFileName);
      end;
    end else
    begin
      fKeyData.Clear;
    end;
  end;
end;

procedure TGlobals.SetSelectedLfo(const Value: integer);
begin
  assert((Value >= 0) and (Value <= 1));

  if Value <> fSelectedLfo then
  begin
    fSelectedLfo := Value;
    MotherShip.SendMessageUsingGuiThread(TLucidMsgID.LfoChanged);
  end;
end;

procedure TGlobals.SetSelectedModSlot(const Value: integer);
begin
  assert(Value >= -1);
  assert(Value <= kModSlotCount-1);

  if Value <> fSelectedModSlot then
  begin
    fSelectedModSlot := Value;
    MotherShip.SendMessageUsingGuiThread(TLucidMsgID.ModSlotChanged);
  end;
end;

end.
