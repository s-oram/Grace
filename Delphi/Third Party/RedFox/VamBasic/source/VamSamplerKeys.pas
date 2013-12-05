unit VamSamplerKeys;

interface

uses
  Types, Classes, Controls,
  RedFox, AggColor, RedFoxColor,
  VamWinControl, VamKeyStateTracker;

const
  kKeyCount = 128;

type
  TRootKeyChangingEvent = procedure(Sender : TObject; const NewRootKey    : integer) of object;
  TRootKeyChangedEvent  = procedure(Sender : TObject; const RootKeyOffset : integer) of object;
  TMIDIKeyEvent         = procedure(Sender : TObject; const KeyIndex      : integer) of object;

  TVamSamplerKeyDisplayElement = record
    Bounds     : TRectF;
    IsBlackKey : boolean;
    Note       : byte; //range 0..127
    IsRootNote : boolean;
  end;

  TVamSamplerKeys = class(TVamWinControl)
  private
    fOffset: single;
    fZoom: single;
    fOnRootKeyChanged: TRootKeyChangedEvent;
    fOnRootKeyChanging: TRootKeyChangingEvent;
    fOnMidiKeyDown: TMIDIKeyEvent;
    fOnMidiKeyUp: TMIDIKeyEvent;
    fMouseDownKeyIndex : integer;
    procedure SetOffset(const Value: single);
    procedure SetZoom(const Value: single);
    function GetKeyIsRootNote(Index: integer): boolean;
    procedure SetKeyIsRootNote(Index: integer; const Value: boolean);
    procedure SetMouseDownKeyIndex(const Value: integer);
  protected
    KeyStateData : TKeyStateData;
    NumberOfKeysToShow : integer;
    KeyBedPixelWidth   : integer;
    KeyBedPixelOffset  : integer;
    KeyPixelWidth      : integer;
    KeyBuffer : array [0..kKeyCount-1] of TVamSamplerKeyDisplayElement;
    MouseOverKeyIndex : integer;
    IsGrabbed : boolean;

    IsMouseDownActive : boolean;

    IsDraggingRootKey : boolean;
    DraggedRootKeyIndex : integer;
    RootKeyDragActive : boolean;
    RootKeyDragIndex  : integer;
    RootKeyDragOffset : integer;
    procedure ZoomOffsetChanged;

    function IsMidiKeyDown(const MidiNote : integer): boolean;
    procedure Paint; override;

    function CalcKeyColor(const KeyIndex:integer):TRedFoxColor;

    procedure DrawWhiteKey(const KeyBounds : TRectF; const KeyColor : TRedFoxColor);
    procedure DrawBlackKey(const KeyBounds : TRectF; const KeyColor : TRedFoxColor);

    function GetKeyIndexAt(PixelPosX, PixelPosY:integer):integer;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure MouseEnter; override;
    procedure MouseLeave; override;

    property MouseDownKeyIndex : integer read fMouseDownKeyIndex write SetMouseDownKeyIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetKeyStateData : PKeyStateData;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property Key_IsRootNote[Index:integer] : boolean read GetKeyIsRootNote write SetKeyIsRootNote;
  published
    property Zoom   : single read fZoom   write SetZoom;   //Range 0..1. 0=Show all keys, 1=Show minimum keys.
    property Offset : single read fOffset write SetOffset; //Range 0..1  0=Show lowest keys, 1=show highest keys.

    {$INCLUDE TControlProperties.inc}

    property OnRootKeyChanging : TRootKeyChangingEvent read fOnRootKeyChanging write fOnRootKeyChanging;
    property OnRootKeyChanged  : TRootKeyChangedEvent  read fOnRootKeyChanged  write fOnRootKeyChanged;

    // MIDIKeyDown/Up events are triggered with the mouse.
    property OnMidiKeyDown : TMIDIKeyEvent read fOnMidiKeyDown write fOnMidiKeyDown;
    property OnMidiKeyUp   : TMIDIKeyEvent read fOnMidiKeyUp   write fOnMidiKeyUp;

    //pro
  end;

implementation

uses
  Math, AggPixelFormat,
  Graphics;

const
  kMinimumKeys = 24;
  kMaximumKeys = kKeyCount;

{ TRedFoxMidiKeysControl }

constructor TVamSamplerKeys.Create(AOwner: TComponent);
var
  c1: Integer;
begin
  inherited;
  fZoom := 0;
  fOffset := 0;

  for c1 := 0 to kMaximumKeys-1 do
  begin
    KeyBuffer[c1].Note := c1;
    case c1 mod 12 of
    0:  KeyBuffer[c1].IsBlackKey := false;  //c
    1:  KeyBuffer[c1].IsBlackKey := true;   //c#
    2:  KeyBuffer[c1].IsBlackKey := false;  //d
    3:  KeyBuffer[c1].IsBlackKey := true;   //d#
    4:  KeyBuffer[c1].IsBlackKey := false;  //e
    5:  KeyBuffer[c1].IsBlackKey := false;  //f
    6:  KeyBuffer[c1].IsBlackKey := true;   //f#
    7:  KeyBuffer[c1].IsBlackKey := false;  //g
    8:  KeyBuffer[c1].IsBlackKey := true;   //g#
    9:  KeyBuffer[c1].IsBlackKey := false;  //a
    10: KeyBuffer[c1].IsBlackKey := true;   //a#
    11: KeyBuffer[c1].IsBlackKey := false;  //b
    end;

  end;

  MouseOverKeyIndex := -1;
end;

destructor TVamSamplerKeys.Destroy;
begin

  inherited;
end;

procedure TVamSamplerKeys.MouseEnter;
begin
  inherited;

end;

procedure TVamSamplerKeys.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
begin
  inherited;


  if (Button = TMouseButton.mbLeft) then
  begin
    IsGrabbed := true;

    //=== reset variables ====
    IsMouseDownActive   := false;
    MouseOverKeyIndex   := -1;
    MouseDownKeyIndex   := -1;

    IsDraggingRootKey   := false;
    DraggedRootKeyIndex := -1;
    RootKeyDragActive   := false;
    RootKeyDragIndex    := -1;
    RootKeyDragOffset   := 0;
    //========================

    Index := GetKeyIndexAt(X, Y);

    if (Index >= 0) and (Index <= kKeyCount) and (KeyBuffer[Index].IsRootNote) then
    begin
      IsDraggingRootKey   := true;
      DraggedRootKeyIndex := Index;

      RootKeyDragActive := true;
      RootKeyDragIndex  := Index;
      RootKeyDragOffset := 0;
    end else
    begin
      IsMouseDownActive := true;
      MouseDownKeyIndex := Index;
    end;



    Invalidate;
  end;

end;

procedure TVamSamplerKeys.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
begin
  inherited;

  Index := GetKeyIndexAt(X, Y);

  if (IsGrabbed = false) and (Index <> MouseOverKeyIndex) then
  begin
    MouseOverKeyIndex := Index;
    Invalidate;
  end;

  if (IsGrabbed) and (IsDraggingRootKey) and (Index <> DraggedRootKeyIndex) then
  begin
    DraggedRootKeyIndex := Index;

    if (DraggedRootKeyIndex <> - 1) then
    begin
      RootKeyDragActive := true;
      RootKeyDragOffset := Index - RootKeyDragIndex;
    end else
    begin
      RootKeyDragActive := false;
      RootKeyDragOffset := 0;
    end;

    Invalidate;

    if assigned(OnRootKeyChanging) then OnRootKeyChanging(Self, DraggedRootKeyIndex);
  end;

  if (IsGrabbed) and (IsMouseDownActive) and (Index <> MouseDownKeyIndex) then
  begin
    MouseDownKeyIndex := Index;
    Invalidate;
  end;





end;

procedure TVamSamplerKeys.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
begin
  inherited;



  if (Button = TMouseButton.mbLeft) and (IsGrabbed = true) then
  begin
    Index := GetKeyIndexAt(X, Y);

    if (IsDraggingRootKey) and (Index <> -1) and (assigned(OnRootKeyChanged)) then
    begin
      RootKeyDragOffset := Index - RootKeyDragIndex;
      OnRootKeyChanged(self, RootKeyDragOffset);
    end;

    IsGrabbed := false;

    IsMouseDownActive   := false;
    MouseDownKeyIndex   := -1;
    DraggedRootKeyIndex := -1;
    RootKeyDragActive   := false;

    //DraggedRootKeyIndex
    Invalidate;
  end;
end;

procedure TVamSamplerKeys.MouseLeave;
begin
  inherited;

  if (MouseOverKeyIndex <> -1) then
  begin
    MouseOverKeyIndex := -1;
    Invalidate;
  end;


end;



function TVamSamplerKeys.GetKeyStateData: PKeyStateData;
begin
  result := @KeyStateData;
end;

procedure TVamSamplerKeys.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  ZoomOffsetChanged;
end;

procedure TVamSamplerKeys.SetKeyIsRootNote(Index: integer; const Value: boolean);
begin
  KeyBuffer[Index].IsRootNote := Value;
end;

procedure TVamSamplerKeys.SetMouseDownKeyIndex(const Value: integer);
begin
  if (Value <> fMouseDownKeyIndex) then
  begin
    if (fMouseDownKeyIndex <> -1) and (assigned(OnMidiKeyUp)) then OnMidiKeyUp(self,fMouseDownKeyIndex);
    fMouseDownKeyIndex := Value;
    if (fMouseDownKeyIndex <> -1) and (assigned(OnMidiKeyDown)) then OnMidiKeyDown(self,fMouseDownKeyIndex);
  end;
end;

function TVamSamplerKeys.GetKeyIndexAt(PixelPosX, PixelPosY: integer): integer;
var
  c1: Integer;
begin
  //search black keys
  for c1 := 0 to kKeyCount-1 do
  begin
    if (KeyBuffer[c1].IsBlackKey) and (InRect(PixelPosX, PixelPosY, KeyBuffer[c1].Bounds)) then
    begin
      exit(c1); //==============>>exit>>=================>>
    end;
  end;

  //search white keys
  for c1 := 0 to kKeyCount-1 do
  begin
    if (KeyBuffer[c1].IsBlackKey = false) and (InRect(PixelPosX, PixelPosY, KeyBuffer[c1].Bounds)) then
    begin
      exit(c1); //==============>>exit>>=================>>
    end;
  end;

  //if we've made it this far, it is not in any key.
  result := -1;
end;

function TVamSamplerKeys.GetKeyIsRootNote(Index: integer): boolean;
begin
  result := KeyBuffer[Index].IsRootNote;
end;

procedure TVamSamplerKeys.SetOffset(const Value: single);
begin
  if value <> fOffset then
  begin
    fOffset := Value;
    ZoomOffsetChanged;
  end;
end;

procedure TVamSamplerKeys.SetZoom(const Value: single);
begin
  if Value <> fZoom then
  begin
    fZoom := Value;
    ZoomOffsetChanged;
  end;
end;

procedure TVamSamplerKeys.ZoomOffsetChanged;
var
  c1: Integer;
begin
  NumberOfKeysToShow := round((kMaximumKeys * (1 - fZoom)) + (kMinimumKeys * fZoom));

  //== Calculate the key left & right bounds ==
  if fZoom = 0 then
  begin
    KeyBedPixelWidth  := self.Width;
    KeyPixelWidth     := ceil(KeyBedPixelWidth / kMaximumKeys);
    KeyBedPixelOffset := 0;

    for c1 := 0 to kMaximumKeys-1 do
    begin
      KeyBuffer[c1].Bounds.Left := (c1 / kMaximumKeys) * KeyBedPixelWidth;
      KeyBuffer[c1].Bounds.Right := KeyBuffer[c1].Bounds.Left + KeyPixelWidth;
    end;
  end else
  begin
    KeyPixelWidth     := ceil(self.Width / NumberOfKeysToShow);
    KeyBedPixelWidth  := KeyPixelWidth * kMaximumKeys;
    KeyBedPixelOffset := round((KeyBedPixelWidth - self.Width) * Offset);

    for c1 := 0 to kMaximumKeys-1 do
    begin
      KeyBuffer[c1].Bounds.Left := c1 * KeyPixelWidth - KeyBedPixelOffset;
      KeyBuffer[c1].Bounds.Right := KeyBuffer[c1].Bounds.Left + KeyPixelWidth;
    end;
  end;


  //=== set key top and bottom bounds ===
  for c1 := 0 to kMaximumKeys-1 do
  begin
    KeyBuffer[c1].Bounds.Top := 0;
    if KeyBuffer[c1].IsBlackKey = false
      then KeyBuffer[c1].Bounds.Bottom := Height
      else KeyBuffer[c1].Bounds.Bottom := Height * 2/3;
  end;



  //=== adjust white key bounds ===
  for c1 := 0 to kMaximumKeys-1 do
  begin
    case KeyBuffer[c1].Note mod 12 of
      0,2,5,7,9:
      begin
        KeyBuffer[c1].Bounds.Right := KeyBuffer[c1].Bounds.Right + (KeyPixelWidth * 0.5);
      end;
    end;
  end;

  for c1 := 0 to kMaximumKeys-1 do
  begin
    case KeyBuffer[c1].Note mod 12 of
      2,4,7,9,11:
      begin
        KeyBuffer[c1].Bounds.Left := KeyBuffer[c1].Bounds.Left - (KeyPixelWidth * 0.5);
      end;
    end;
  end;




  //========================
  // Adjust keys for pixel drawing reasons

  for c1 := 0 to kMaximumKeys-1 do
  begin
    if KeyBuffer[c1].IsBlackKey = true then
    begin
      KeyBuffer[c1].Bounds.Left  := floor(KeyBuffer[c1].Bounds.Left);
      KeyBuffer[c1].Bounds.Right := KeyBuffer[c1].Bounds.Left + KeyPixelWidth;

      KeyBuffer[c1].Bounds.Left  := KeyBuffer[c1].Bounds.Left  - (KeyPixelWidth div 5);
      KeyBuffer[c1].Bounds.Right := KeyBuffer[c1].Bounds.Right + (KeyPixelWidth div 5);
    end;
  end;


  for c1 := 0 to kMaximumKeys-1 do
  begin
    case KeyBuffer[c1].Note mod 12 of
      0,5,9:
      begin
        KeyBuffer[c1].Bounds.Left := floor(KeyBuffer[c1].Bounds.Left) + 0.5;
      end;

      4:
      begin
        KeyBuffer[c1].Bounds.Left := floor(KeyBuffer[c1].Bounds.Left) - 0.5;
      end;

      11:
      begin
        KeyBuffer[c1].Bounds.Left := round(KeyBuffer[c1].Bounds.Left) - 0.5;
      end;

      2,7:
      begin
        KeyBuffer[c1].Bounds.Left := ceil(KeyBuffer[c1].Bounds.Left) + 0.5;
      end;
    end;

    if KeyBuffer[c1].IsBlackKey = false then
    begin

    end;
  end;

  for c1 := 0 to kMaximumKeys-2 do
  begin
    case KeyBuffer[c1].Note mod 12 of
      0,2,5,7,9:
      begin
        KeyBuffer[c1].Bounds.Right:= KeyBuffer[c1+2].Bounds.Left;
      end;

      4,11:
      begin
        KeyBuffer[c1].Bounds.Right:= KeyBuffer[c1+1].Bounds.Left;
      end;
    end;
  end;

  {
  for c1 := 0 to kMaximumKeys-2 do
  begin
    case KeyBuffer[c1].Note mod 12 of
      0:
      begin
        KeyBuffer[c1].Bounds.Left  := round(KeyBuffer[c1].Bounds.Left) + 0.5;
        KeyBuffer[c1].Bounds.Right := floor(KeyBuffer[c1].Bounds.Right) + 0.5;
      end;
    end;
  end;


  for c1 := 0 to kMaximumKeys-2 do
  begin
    case KeyBuffer[c1].Note mod 12 of
      2:
      begin
        KeyBuffer[c1].Bounds.Left  := KeyBuffer[c1-2].Bounds.Right
        //KeyBuffer[c1].Bounds.Right := ceil(KeyBuffer[c1].Bounds.Right) + 0.5;
      end;

      11:
      begin
        KeyBuffer[c1].Bounds.Right := KeyBuffer[c1 + 1].Bounds.Left;
      end;
    end;
  end;
  }


  Invalidate;
end;

function TVamSamplerKeys.IsMidiKeyDown(const MidiNote: integer): boolean;
var
  c1 : integer;
begin
  for c1 := 0 to Length(KeyStateData)-1 do
  begin
    if KeyStateData[c1].Note = MidiNote then exit(true);
  end;

  // if we've made it this far, no matching key has been found. exit false.
  result := false;
end;






procedure TVamSamplerKeys.Paint;
const
  c2 : TAggRgba8 = (
    R : $44;
    G : $44;
    B : $44;
    A : $FF;
  );
var
  c1: Integer;
  aColor : TRedFoxColor;
begin
  inherited;

  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;
  BackBuffer.BufferInterface.ClearAll(c2);
  BackBuffer.BufferInterface.NoLine;

  for c1 := 0 to kMaximumKeys-1 do
  begin
    if KeyBuffer[c1].IsBlackKey = false then
    begin
      aColor := self.CalcKeyColor(c1);
      DrawWhiteKey(KeyBuffer[c1].Bounds, aColor);
    end;
  end;

  for c1 := 0 to kMaximumKeys-1 do
  begin
    if KeyBuffer[c1].IsBlackKey = true then
    begin
      aColor := self.CalcKeyColor(c1);
      DrawBlackKey(KeyBuffer[c1].Bounds, aColor);
    end;
  end;

  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;
end;

procedure TVamSamplerKeys.DrawWhiteKey(const KeyBounds: TRectF; const KeyColor : TRedFoxColor);
const
  c1 : TAggRgba8 = (
    R : $aa;
    G : $aa;
    B : $aa;
    A : $FF;
  );
var
  x1, y1, x2, y2 : single;
begin
  x1 := KeyBounds.Left;
  x2 := KeyBounds.Right;
  y1 := KeyBounds.Top;
  y2 := KeyBounds.Bottom;

  BackBuffer.BufferInterface.FillColor := KeyColor;
  BackBuffer.BufferInterface.LineColor := c1;
  BackBuffer.BufferInterface.LineWidth := 1;
  BackBuffer.BufferInterface.RoundedRectEx(x1, y1, x2, y2, 0,0,6,6);
end;

procedure TVamSamplerKeys.DrawBlackKey(const KeyBounds: TRectF; const KeyColor : TRedFoxColor);
var
  x1, y1, x2, y2 : single;
begin
  x1 := KeyBounds.Left;
  x2 := KeyBounds.Right;
  y1 := KeyBounds.Top;
  y2 := KeyBounds.Bottom;

  BackBuffer.BufferInterface.FillColor := KeyColor;
  BackBuffer.BufferInterface.LineColor := KeyColor;
  BackBuffer.BufferInterface.LineWidth := 1;
  BackBuffer.BufferInterface.RoundedRectEx(x1, y1, x2, y2, 0,0,6,6);
end;

function TVamSamplerKeys.CalcKeyColor(const KeyIndex: integer): TRedFoxColor;
const
  kBrightRootKey = '$FF226CEA';
  kMouseOverColor = '$FFFF0000';
  kMouseDownColor = '$FFFF0000';

  White_c1 : TAggRgba8 = (
    R : $aa;
    G : $aa;
    B : $aa;
    A : $FF;
  );

  White_c2 : TAggRgba8 = (
    R : $FF;
    G : $FF;
    B : $FF;
    A : $FF;
  );

  White_c3 : TAggRgba8 = (
    R : $FF;
    G : $cc;
    B : $cc;
    A : $FF;
  );

  White_c4 : TAggRgba8 = (
    R : $22;
    G : $6C;
    B : $EA;
    A : $FF;
  );



  Black_c1 : TAggRgba8 = (
    R : $CC;
    G : $44;
    B : $44;
    A : $FF;
  );

  Black_c2 : TAggRgba8 = (
    R : $44;
    G : $44;
    B : $44;
    A : $FF;
  );

  Black_c4 : TAggRgba8 = (
    R : $22;
    G : $6C;
    B : $EA;
    A : $FF;
  );
var
  aColor : TRedFoxColor;


  rkIndex : integer;
  IsRoot : boolean;


begin
  if (RootKeyDragActive) then
  begin
    rkIndex := KeyIndex - RootKeyDragOffset;
  end else
  begin
    rkIndex := KeyIndex;
  end;

  if (rkIndex >= 0) and (rkIndex <= kKeyCount) and (KeyBuffer[rkIndex].IsRootNote)
    then IsRoot := true
    else IsRoot := false;

  if KeyBuffer[KeyIndex].IsBlackKey = false then
  begin
    //=== White Keys ===
    if IsMidiKeyDown(KeyIndex)
      then aColor := White_c3
      else aColor := White_c2;

    if (KeyIndex = MouseOverKeyIndex) then
    begin
      aColor := ColorFadeF(aColor, kMouseOverColor, 0.4);
    end;

    if (KeyIndex = MouseDownKeyIndex) and (IsMouseDownActive) then
    begin
      aColor := ColorFadeF(aColor, kMouseDownColor, 0.7);
    end;

    if (IsRoot) and (RootKeyDragActive = false) then
    begin
      aColor := ColorFadeF(aColor, White_C4, 0.7);
    end;

    if (IsRoot) and (RootKeyDragActive) then
    begin
      aColor := ColorFadeF(aColor, kBrightRootKey, 0.8);
    end;

  end else
  begin
    //=== Black Keys ===
    if IsMidiKeyDown(KeyIndex)
      then aColor := Black_c1
      else aColor := Black_c2;

    if (KeyIndex = MouseOverKeyIndex) then
    begin
      aColor := ColorFadeF(aColor, kMouseOverColor, 0.7);
    end;

    if (KeyIndex = MouseDownKeyIndex) and (IsMouseDownActive) then
    begin
      aColor := ColorFadeF(aColor, kMouseDownColor, 0.7);
    end;

    if (IsRoot) and (RootKeyDragActive = false) then
    begin
      aColor := ColorFadeF(aColor, Black_C4, 0.4);
    end;

    if (IsRoot) and (RootKeyDragActive) then
    begin
      aColor := ColorFadeF(aColor, kBrightRootKey, 0.8);
    end;
  end;

  result := aColor;
end;








end.
