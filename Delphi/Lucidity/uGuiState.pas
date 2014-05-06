unit uGuiState;

interface

uses
  uConstants, uLucidityEnums, Classes, Controls;

type
  TGuiState = class
  private
    fLowerTabState: TLowerTabOptions;
    fIsSampleMapVisible: boolean;
    fMouseOverRegionID: TGUID;
    fCurrentModDestTarget: TModDest;
    fFocusedControl: TControl;
    fIsModDestAutoSelectEnabled: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property IsSampleMapVisible : boolean          read fIsSampleMapVisible write fIsSampleMapVisible;
    property LowerTabState      : TLowerTabOptions read fLowerTabState      write fLowerTabState;

    property MouseOverRegionID : TGUID read fMouseOverRegionID write fMouseOverRegionID;

    property ModDestTarget : TModDest read fCurrentModDestTarget write fCurrentModDestTarget;
    property IsModDestAutoSelectEnabled : boolean read fIsModDestAutoSelectEnabled write fIsModDestAutoSelectEnabled;

    property FocusedControl : TControl read fFocusedControl write fFocusedControl;
  end;

implementation

uses
  GuidEx;

{ TGuiState }

constructor TGuiState.Create;
begin
  fLowerTabState := TLowerTabOptions.TabMain; // shows the main tab by default.

  MouseOverRegionID := TGuidEx.EmptyGuid;

  fIsModDestAutoSelectEnabled := true;
end;

destructor TGuiState.Destroy;
begin

  inherited;
end;

end.
