unit eeVoiceLogic;

interface

uses
  Classes;

type
  IVoiceStateInfo = Interface(IInterface)
    function GetObject:TObject;
    procedure Release;
    procedure QuickRelease;
    procedure Kill;
    procedure GetVoiceState(out aIsActive, aHasBeenReleased, aHasBeenQuickReleased: boolean;  out aAmpLevel: single);
  end;

  TVoiceControl = class
  private
    fMaxActiveVoiceCount: integer;
  protected
    LastVoiceIndex : integer;
    VoiceList:TInterfaceList;
    function FindActiveVoiceCount:integer;
    function FindReleasedVoice:IVoiceStateInfo;
  public
    constructor Create;
	  destructor Destroy; override;

    function GetInactiveVoice:TObject;

    procedure AddVoice(aVoice:IVoiceStateInfo);

    //NOTE: I'm not sure if this MaxActiveVoiceCount is a good idea anymore...
    property MaxActiveVoiceCount:integer read fMaxActiveVoiceCount write fMaxActiveVoiceCount;
  end;



implementation

{ TVoiceControl }

constructor TVoiceControl.Create;
begin
  LastVoiceIndex := 0;
  VoiceList := TInterfaceList.Create;
  MaxActiveVoiceCount := 3;
end;

destructor TVoiceControl.Destroy;
begin
  VoiceList.Free;
  inherited;
end;

function TVoiceControl.GetInactiveVoice: TObject;
var
  c1:integer;
  aVoice:IVoiceStateInfo;
  IsActive:boolean;
  HasBeenReleased:boolean;
  HasBeenQuickReleased:boolean;
  AmpLevel:single;
  VoiceFound:boolean;
  VoiceToKill:IVoiceStateInfo;
  avc:integer;
begin
  result := nil;
  VoiceFound := false;

  if LastVoiceIndex < VoiceList.Count then
  begin
    aVoice := IVoiceStateInfo(VoiceList[LastVoiceIndex]);
    aVoice.GetVoiceState(IsActive, HasBeenReleased, HasBeenQuickReleased, AmpLevel);
    if IsActive = false then
    begin
      VoiceFound := true;
      result := aVoice.GetObject;
    end;

    inc(LastVoiceIndex);
    if LastVoiceIndex >= VoiceList.Count then LastVoiceIndex := 0;
  end else
  begin
    LastVoiceIndex := 0;
  end;


  if VoiceFound = false then
  begin
    for c1 := 0 to VoiceList.Count - 1 do
    begin
      aVoice := IVoiceStateInfo(VoiceList[c1]);
      aVoice.GetVoiceState(IsActive, HasBeenReleased, HasBeenQuickReleased, AmpLevel);
      if IsActive = false then
      begin
        VoiceFound := true;
        result := aVoice.GetObject;
        break;
      end;
    end;
  end;

  {
  // TODO: NOTE: If the current active Voice count is bigger then
  // the MaxActiveVoiceCount, then this section will look for
  // a voice to kill. Current MaxActiveVoice count is
  // set at a default value regardless of how many voices
  // are present. Perhaps it would be better to automatically calculate
  // the MaxActiveVoice count. As it currently is the application
  // developer is likely to forget to set MaxActiveVoiceCount to an
  // appropiate value.
  avc := FindActiveVoiceCount;
  for c1 := 0 to avc - MaxActiveVoiceCount - 1 do
  begin
    VoiceToKill := FindReleasedVoice;
    if VoiceToKill <> nil then
    begin
      VoiceToKill.QuickRelease;
    end;
  end;
  }

  if VoiceFound = false then
  begin
    result := nil;
    exit;
  end;
end;

procedure TVoiceControl.AddVoice(aVoice: IVoiceStateInfo);
begin
  VoiceList.Add(aVoice);
end;

function TVoiceControl.FindActiveVoiceCount: integer;
var
  c1:integer;
  aVoice:IVoiceStateInfo;
  IsActive:boolean;
  HasBeenReleased:boolean;
  HasBeenQuickReleased:boolean;
  AmpLevel:single;
  _ActiveVoiceCount:integer;
begin
  _ActiveVoiceCount := 0;
  for c1 := 0 to VoiceList.Count - 1 do
  begin
    aVoice := IVoiceStateInfo(VoiceList[c1]);
    aVoice.GetVoiceState(IsActive, HasBeenReleased, HasBeenQuickReleased, AmpLevel);
    if (IsActive) and (HasBeenQuickReleased = false) then
    begin
      inc(_ActiveVoiceCount);
    end;
  end;
  result := _ActiveVoiceCount;
end;

function TVoiceControl.FindReleasedVoice: IVoiceStateInfo;
var
  c1:integer;
  aVoice:IVoiceStateInfo;
  IsActive:boolean;
  HasBeenReleased:boolean;
  HasBeenQuickReleased:boolean;
  AmpLevel:single;
  LastIndex:integer;
  LastLevel:single;
begin
  LastIndex := -1;
  LastLevel := 0;
  for c1 := 0 to VoiceList.Count - 1 do
  begin
    aVoice := IVoiceStateInfo(VoiceList[c1]);
    aVoice.GetVoiceState(IsActive, HasBeenReleased, HasBeenQuickReleased, AmpLevel);

    if (IsActive) and (HasBeenReleased) and (HasBeenQuickReleased = false) then
    begin
      if LastIndex = -1 then
      begin
        LastIndex := c1;
        LastLevel := AmpLevel;
      end else
      begin
        if (AmpLevel < LastLevel) then
        begin
          LastIndex := c1;
          LastLevel := AmpLevel;
        end;
      end;
    end;
  end;

  if LastIndex = -1
    then result := nil
    else result := IVoiceStateInfo(VoiceList[LastIndex]);

end;





end.

