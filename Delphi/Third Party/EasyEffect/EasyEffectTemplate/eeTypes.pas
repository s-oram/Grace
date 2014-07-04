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
