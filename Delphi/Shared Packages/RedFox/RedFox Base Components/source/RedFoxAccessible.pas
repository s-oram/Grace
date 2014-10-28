unit RedFoxAccessible;

interface

uses
  Controls, Classes, WinApi.Oleacc, Windows, System.Win.ComObj;


{
  NOTES:

  IAccessible
  http://msdn.microsoft.com/en-us/library/windows/desktop/dd318466%28v=vs.85%29.aspx
}


type
  TRedFoxAccessibleProperties = class;

  TRedFoxAccessibleWrapper = class(TAutoObject, IAccessible)
  private
    //== IAccessible ==
    function Get_accParent(out ppdispParent: IDispatch): HResult; stdcall;
    function Get_accChildCount(out pcountChildren: Integer): HResult; stdcall;
    function Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult; stdcall;
    function Get_accName(varChild: OleVariant; out pszName: WideString): HResult; stdcall;
    function Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult; stdcall;
    function Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult; stdcall;
    function Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult; stdcall;
    function Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult; stdcall;
    function Get_accHelp(varChild: OleVariant; out pszHelp: WideString): HResult; stdcall;
    function Get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant; out pidTopic: Integer): HResult; stdcall;
    function Get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HResult; stdcall;
    function Get_accFocus(out pvarChild: OleVariant): HResult; stdcall;
    function Get_accSelection(out pvarChildren: OleVariant): HResult; stdcall;
    function Get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HResult; stdcall;
    function accSelect(flagsSelect: Integer; varChild: OleVariant): HResult; stdcall;
    function accLocation(out pxLeft: Integer; out pyTop: Integer; out pcxWidth: Integer; out pcyHeight: Integer; varChild: OleVariant): HResult; stdcall;
    function accNavigate(navDir: Integer; varStart: OleVariant; out pvarEndUpAt: OleVariant): HResult; stdcall;
    function accHitTest(xLeft: Integer; yTop: Integer; out pvarChild: OleVariant): HResult; stdcall;
    function accDoDefaultAction(varChild: OleVariant): HResult; stdcall;
    function Set_accName(varChild: OleVariant; const pszName: WideString): HResult; stdcall;
    function Set_accValue(varChild: OleVariant; const pszValue: WideString): HResult; stdcall;
  protected
    Anchor : TControl; //control that the accessibility functionality will be added to.
    AnchorProperties:TRedFoxAccessibleProperties;
  public
    constructor Create(const aAnchor : TControl; const aAnchorProperties:TRedFoxAccessibleProperties);
    destructor Destroy; override;
  end;

  TRedFoxAccessibleProperties = class(TPersistent)
  private
    fName: string;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignTo(Dest: TPersistent); override;
  published
    property Name : string read fName write fName;
  end;

implementation

uses
  {$IFDEF Logging}
  SmartInspectLogging,
  MadListModules,
  MadListProcesses,
  MadListHardware,
  MadLinkDisAsm,
  MadExcept,
  {$ENDIF}
  WinApi.ActiveX;

{ TVamAccessible }

constructor TRedFoxAccessibleWrapper.Create(const aAnchor : TControl; const aAnchorProperties:TRedFoxAccessibleProperties);
begin
  assert(assigned(aAnchor));
  assert(assigned(aAnchorProperties));
  Anchor := aAnchor;
  AnchorProperties := aAnchorProperties;
end;

destructor TRedFoxAccessibleWrapper.Destroy;
begin

  inherited;
end;

function TRedFoxAccessibleWrapper.accDoDefaultAction(varChild: OleVariant): HResult;
begin
  {$IFDEF Logging}LogMain.logMessage('accDoDefaultAction');{$ENDIF}


  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.accHitTest(xLeft, yTop: Integer; out pvarChild: OleVariant): HResult;
var
  p : TPoint;
  c : TControl;
begin
  {$IFDEF Logging}LogMain.logMessage('accHitTest');{$ENDIF}

  // http://msdn.microsoft.com/en-us/library/windows/desktop/dd318471%28v=vs.85%29.aspx

  p := Anchor.ScreenToClient(Point(xLeft, yTop));


  if (Anchor is TWinControl) then
  begin
    c := (Anchor as TWinControl).ControlAtPos(p, true, true, true);

    if (not assigned(c)) and (Anchor.BoundsRect.Contains(p)) then
    begin
      pvarChild := CHILDID_SELF;
    end else
    if assigned(c)  then
    begin
      // TODO: return child ID. for now return empty.
      pvarChild := VT_Empty;
    end else
    begin
      pvarChild := VT_Empty;
    end;
  end else
  begin
    if Anchor.BoundsRect.Contains(p)
      then pvarChild := CHILDID_SELF
      else pvarChild := VT_Empty;
  end;


  result := S_OK;


  //result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.accLocation(out pxLeft, pyTop, pcxWidth, pcyHeight: Integer; varChild: OleVariant): HResult;
begin
  {$IFDEF Logging}LogMain.logMessage('accLocation');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.accNavigate(navDir: Integer; varStart: OleVariant; out pvarEndUpAt: OleVariant): HResult;
begin
  {$IFDEF Logging}LogMain.logMessage('accNavigate');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.accSelect(flagsSelect: Integer; varChild: OleVariant): HResult;
begin
  {$IFDEF Logging}LogMain.logMessage('accSelect');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.Get_accChild(varChild: OleVariant; out ppdispChild: IDispatch): HResult;
begin
  {$IFDEF Logging}LogMain.logMessage('Get_accChild');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.Get_accChildCount(out pcountChildren: Integer): HResult;
begin
  {$IFDEF Logging}LogMain.logMessage('Get_accChildCount');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.Get_accDefaultAction(varChild: OleVariant; out pszDefaultAction: WideString): HResult;
begin
  {$IFDEF Logging}LogMain.LogMessage('Get_accDefaultAction');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.Get_accDescription(varChild: OleVariant; out pszDescription: WideString): HResult;
begin
  {$IFDEF Logging}LogMain.LogMessage('Get_accDescription');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.Get_accFocus(out pvarChild: OleVariant): HResult;
begin
  {$IFDEF Logging}LogMain.LogMessage('Get_accFocus');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.Get_accHelp(varChild: OleVariant; out pszHelp: WideString): HResult;
begin
  {$IFDEF Logging}LogMain.LogMessage('Get_accHelp');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.Get_accHelpTopic(out pszHelpFile: WideString; varChild: OleVariant; out pidTopic: Integer): HResult;
begin
  {$IFDEF Logging}LogMain.LogMessage('Get_accHelpTopic');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.Get_accKeyboardShortcut(varChild: OleVariant; out pszKeyboardShortcut: WideString): HResult;
begin
  {$IFDEF Logging}LogMain.LogMessage('Get_accKeyboardShortcut');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.Get_accName(varChild: OleVariant; out pszName: WideString): HResult;
begin
  {$IFDEF Logging}LogMain.LogMessage('Get_accName');{$ENDIF}
  //http://msdn.microsoft.com/en-us/library/0dcdsy6s%28v=vs.71%29.aspx

  if varChild = CHILDID_SELF then
  begin
    if (AnchorProperties.Name <> '') then
    begin
      pszName := AnchorProperties.Name;
      result := S_OK;
    end else
    if (Anchor.Name <> '') then
    begin
      pszName := Anchor.Name;
      result := S_OK;
    end else
    begin
      pszName := '';
      result := S_FALSE;
    end;
  end else
  begin
    result := S_FALSE;
  end;
end;

function TRedFoxAccessibleWrapper.Get_accParent(out ppdispParent: IDispatch): HResult;
begin
  {$IFDEF Logging}LogMain.LogMessage('Get_accParent');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.Get_accRole(varChild: OleVariant; out pvarRole: OleVariant): HResult;
begin
  {$IFDEF Logging}LogMain.LogMessage('Get_accRole');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.Get_accSelection(out pvarChildren: OleVariant): HResult;
begin
  {$IFDEF Logging}LogMain.LogMessage('Get_accSelection');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.Get_accState(varChild: OleVariant; out pvarState: OleVariant): HResult;
begin
  {$IFDEF Logging}LogMain.LogMessage('Get_accState');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.Get_accValue(varChild: OleVariant; out pszValue: WideString): HResult;
begin
  {$IFDEF Logging}LogMain.LogMessage('Get_accValue');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.Set_accName(varChild: OleVariant; const pszName: WideString): HResult;
begin
  {$IFDEF Logging}LogMain.LogMessage('Set_accName');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

function TRedFoxAccessibleWrapper.Set_accValue(varChild: OleVariant; const pszValue: WideString): HResult;
begin
  {$IFDEF Logging}LogMain.LogMessage('Set_accValue');{$ENDIF}
  result := DISP_E_MEMBERNOTFOUND;
end;

{ TVamAccessibleProperties }

procedure TRedFoxAccessibleProperties.AssignTo(Dest: TPersistent);
begin
  if Dest is TRedFoxAccessibleProperties then
  begin
    (Dest as TRedFoxAccessibleProperties).Name := self.Name;
  end else
  begin
    inherited AssignTo(Dest);
  end;
end;

constructor TRedFoxAccessibleProperties.Create;
begin
end;

destructor TRedFoxAccessibleProperties.Destroy;
begin

  inherited;
end;



end.
