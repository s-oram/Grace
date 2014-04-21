unit VamGraphicControl;

interface

uses
  Classes, Types,
  RedFoxGraphicControl, VamVisibleControl, VamLayoutWizard;

type
  TVamGraphicControl = class(TRedFoxGraphicControl, IVamVisibleControl, IVamLayoutWizard)
  private
    fUpdatingCount : integer;
    fLayout: TVamLayoutWizard;
    function GetIsUpdating: boolean;
  protected
    function GetObject : TObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // AlignToParent() resizes the control to the same size as the containing parent control.
    procedure AlignToParent(const UsingMargins : boolean = false);

    // BeginUpdate() / EndUpdate() calls can be nested. A BeginUpdate() call must
    // always be followed by a EndUpdate() call.
    procedure BeginUpdate;
    procedure EndUpdate;
    property IsUpdating : boolean read GetIsUpdating;

    // Resize calls SetBounds() then fires the OnResize() event.
    procedure Resize(aLeft, aTop, aWidth, aHeight: Integer); reintroduce;


    //TODO: These methods need to be made private and should be accessed via the interface.
    procedure OleDragOver(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData); virtual;
    procedure OleDragDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData); virtual;
    procedure OleDragEnter(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData); virtual;
    procedure OleDragLeave(Sender: TObject); virtual;


    property Layout : TVamLayoutWizard read fLayout implements IVamLayoutWizard;
  published
    property HitTest;
  end;

implementation

uses
  SysUtils;

{ TVamGraphicControl }

constructor TVamGraphicControl.Create(AOwner: TComponent);
begin
  inherited;
  fUpdatingCount := 0;
  fLayout := TVamLayoutWizard.Create(self);
end;

destructor TVamGraphicControl.Destroy;
begin
  fLayout.Free;
  inherited;
end;

procedure TVamGraphicControl.AlignToParent(const UsingMargins: boolean);
begin
  if not assigned(Parent) then exit;

  BeginUpdate;
  try
    if UsingMargins then
    begin
      Top    := Margins.Top  + Parent.Padding.Top;
      Left   := Margins.Left + Parent.Padding.Left;
      Width  := Parent.Width  - (Margins.Left + Margins.Right)  - (Parent.Padding.Left + Parent.Padding.Right);
      Height := Parent.Height - (Margins.Top  + Margins.Bottom) - (Parent.Padding.Top  + Parent.Padding.Bottom);
    end else
    begin
      Top    := 0;
      Left   := 0;
      Width  := Parent.Width  - (Parent.Padding.Left + Parent.Padding.Right);
      Height := Parent.Height - (Parent.Padding.Top  + Parent.Padding.Bottom);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TVamGraphicControl.BeginUpdate;
begin
  inc(fUpdatingCount);
end;

procedure TVamGraphicControl.EndUpdate;
begin
  dec(fUpdatingCount);
  if fUpdatingCount = 0 then self.Invalidate;
  if fUpdatingCount < 0 then raise Exception.Create('Begin/End Update mismatch.');
end;

function TVamGraphicControl.GetIsUpdating: boolean;
begin
  if fUpdatingCount > 0
    then result := true
    else result := false;
end;

function TVamGraphicControl.GetObject: TObject;
begin
  result := self;
end;

procedure TVamGraphicControl.OleDragDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
begin

end;

procedure TVamGraphicControl.OleDragEnter(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer;  Data:IVamDragData);
begin

end;

procedure TVamGraphicControl.OleDragLeave(Sender: TObject);
begin

end;

procedure TVamGraphicControl.OleDragOver(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
begin

end;

procedure TVamGraphicControl.Resize(aLeft, aTop, aWidth, aHeight: Integer);
begin
  // NOTE: Resize calls SetBounds() then fires the OnResize() event.
  // I'm not sure why the OnResize() event isn't called after SetBounds().
  // I considered overriding the SetBounds() method so that the OnResize()
  // event is fired, but I'm guessing there must be a reason the VCL designers
  // didn't do this.
  // Adding the Resize() method allows the control to be resized and the Resize()
  // event to be triggered where appropiate.

  if (aLeft <> Left) or (aTop <> Top) or (aWidth <> Width) or (aHeight <> aHeight) then
  begin
    BeginUpdate;
    try
      SetBounds(aLeft, aTop, aWidth, aHeight);
      if assigned(OnResize) then OnResize(self);
    finally
      EndUpdate;
    end;
  end;
end;

end.
