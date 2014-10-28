unit RedFoxPaintBox;

interface

uses
  Types, Classes, Controls,
  RedFox,
  RedFoxCustomControl;

type
  TRedFoxPaintBox = class(TRedFoxCustomControl)
  private
    fOnPaintBuffer: TNotifyEvent;
  protected
  public
    procedure DoPaintBuffer; override;
  published
    property OnPaintBuffer : TNotifyEvent read fOnPaintBuffer write fOnPaintBuffer;

    //-------------
    property Align;
    property AlignWithMargins;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Anchors;
    property Constraints;
    property Visible;

    property Font;
    property ParentFont;

    //TControl Events.
    property OnCanResize;
		property OnClick;
		property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    //end TControlEvents.
  end;

implementation

{ TRedFoxPaintBox }

procedure TRedFoxPaintBox.DoPaintBuffer;
begin
  inherited;

  if csDesigning in ComponentState then
  begin
    BackBuffer.BufferInterface.ClearAll(255,255,255,0);
  end else
  begin
    if assigned(OnPaintBuffer) then OnPaintBuffer(self);
  end;
end;

end.
