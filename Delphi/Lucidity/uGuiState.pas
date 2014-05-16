unit uGuiState;

interface

uses
  uConstants, uLucidityEnums, Classes, Controls;

type
  TGuiState = class
  private
    fLowerTabState: TLowerTabOptions;

    fMouseOverRegionID: TGUID;
    fCurrentModDestTarget: TModDest;
    fFocusedControl: TControl;
    fIsModDestAutoSelectEnabled: boolean;
    fMainGuiLayout: TMainGuiLayout;
  public
    constructor Create;
    destructor Destroy; override;

    property MouseOverRegionID : TGUID read fMouseOverRegionID write fMouseOverRegionID;

    property ModDestTarget : TModDest read fCurrentModDestTarget write fCurrentModDestTarget;
    property IsModDestAutoSelectEnabled : boolean read fIsModDestAutoSelectEnabled write fIsModDestAutoSelectEnabled;

    property FocusedControl : TControl read fFocusedControl write fFocusedControl;

    property MainGuiLayout : TMainGuiLayout read fMainGuiLayout write fMainGuiLayout;
    property LowerTabState      : TLowerTabOptions read fLowerTabState      write fLowerTabState;
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
  fMainGuiLayout := TMainGuiLayout.Default;
end;

destructor TGuiState.Destroy;
begin

  inherited;
end;

end.
