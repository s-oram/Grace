unit eeTypes;

interface

uses
  Generics.Collections;

type
  // NOTE: These aren't all vst host, just the ones I've had access to for testing purposes.
  TVstHost = (vhUnKnown,
              vhEnergyXT,
              vhEnergyXT2,
              vhCubaseSX2,
              vhCubase5,
              vhReaper,
              vhReaperBridge32, //32bit bridge for using 32 bit plugins in Reaper x64
              vhTracktion3,
              vhFLStudio,
              vhPodium,
              vhAbletonLive,
              vhAcidPro6,
              vhSonar,
              vhMultiTrackStudio,
              vhSamplitude11,
              vhStudioOne,
              vhJBridge);


  TBeginParameterEdit    = function(Index:longint):boolean of object;
  TEndParameterEdit      = function(Index:longint):boolean of object;
  TSetParameterProcedure = procedure(Index:longint; Value:single) of object;
  TGetParameterFunction  = function(Index:longint):single of object;
  TSizeWindowMethod      = function(Width, Height:longint): boolean of object;

  TChangePinCountFunction = function(NewPinCount:integer):integer of object;

  PPinProperties = ^TPinProperties;
  TPinProperties = record
    Name:string;
    ShortName:string;
    IsStereo:boolean;
  end;

  THostPlayState = (psHostIsStopped, psHostIsPlaying);

  TVstAudioEffect = class
  public
    BeginEdit :TBeginParameterEdit;
    EndEdit   :TEndParameterEdit;
  end;


  // A non-reference-counted IInterface implementation.
  TPureInterfacedObject = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;



  TIntegerList  = TList<integer>;
  TCardinalList = TList<cardinal>;


  TPluginParameterID = integer;

  // TParChangeScope = Parameter Change Scope.
  // Setting/Getting a plugin parameters value becomes complicated
  // when a parameter can be dynmaically mapped to different 'focused'
  // elements of a multi-timbral type patch. For Example: The ADSR
  // envelope knobs might be applied to the focused sample, not all
  // loaded samples.
  // By controst parameter changes sometimes need to be global and
  // applied to all 'layers' of a patch.
  // Generally parameter changes from MIDI and Published VST parameters
  // are "Global" while the GUI normally triggers "Focused" parameter changes.
  TParChangeScope = (psGlobal, psFocused);




implementation




function TPureInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TPureInterfacedObject._AddRef: Integer;
begin
  Result := -1;
end;

function TPureInterfacedObject._Release: Integer;
begin
  Result := -1;
end;


end.
