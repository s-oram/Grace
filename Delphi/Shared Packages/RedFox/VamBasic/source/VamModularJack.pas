unit VamModularJack;

interface

uses
  Contnrs, Types, Controls, Classes, Graphics, Generics.Collections,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamGraphicControl, VamWinControl;

type
  //==== forward declarations ====
  TVamModularJack = class;
  IVamModularConnection = interface;
  TVamModularCableOverlay = class;
  TVamModularConnections = class;
  //===============================

  TNewModularConnectionEvent    = procedure(Sender:TObject; const OutputJack, InputJack : TVamModularJack) of object;
  TDeleteModularConnectionEvent = procedure(Sender:TObject; const OutputJack, InputJack : TVamModularJack) of object;

  TJackType = (jtInput, jtOutput);

  TVamModularJack = class(TVamWinControl)
  strict private
    fInputColor : TRedFoxColor;
    fOutputColor : TRedFoxColor;
    fBackgroundColor : TRedFoxColor;
    fJackType: TJackType;
    fJackSize: integer;
    fCableOverlay: TVamModularCableOverlay;
    function GetInputColor: TRedFoxColorString;
    function GetOutputColor: TRedFoxColorString;
    procedure SetInputColor(const Value: TRedFoxColorString);
    procedure SetJackType(const Value: TJackType);
    procedure SetOutputColor(const Value: TRedFoxColorString);
    procedure SetJackSize(const Value: integer);
    function GetBackgroundColor: TRedFoxColorString;
    procedure SetBackgroundColor(const Value: TRedFoxColorString);
  private
    procedure SetCableOverlay(const Value: TVamModularCableOverlay);
  strict protected
    IsGrabbed : boolean;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;



  published
    property BackgroundColor : TRedFoxColorString read GetBackgroundColor write SetBackgroundColor;
    property InputColor : TRedFoxColorString read GetInputColor write SetInputColor;
    property OutputColor : TRedFoxColorString read GetOutputColor write SetOutputColor;

    property JackType : TJackType read fJackType write SetJackType;
    property JackSize : integer read fJackSize write SetJackSize;

    property CableOverlay : TVamModularCableOverlay read fCableOverlay write SetCableOverlay;

    {$INCLUDE TControlProperties.inc}
  end;


  TVamModularJackList = TObjectList<TVamModularJack>;

  PPendingConnection = ^TPendingConnection;
  TPendingConnection = record
    IsActive : boolean;
    SourceX : single;
    SourceY : single;
    DestX   : single;
    DestY   : single;
    Color   : TRedFoxColor;
  end;

  TVamModularCableOverlayOptions = class(TPersistent)
  strict private
    fLimitInputConnections: boolean;
  private
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    property LimitInputConnections : boolean read fLimitInputConnections write fLimitInputConnections;
  end;

  TVamModularCableOverlay = class(TVamWinControl)
  strict private
    fPendingConnection: TPendingConnection;
    fJackList : TVamModularJackList;
  strict
  private
    fConnections: TVamModularConnections;
  private
    fOnNewConnection: TNewModularConnectionEvent;
    fOnDeleteConnection: TDeleteModularConnectionEvent;
    fOptions: TVamModularCableOverlayOptions;
    procedure SetOptions(const Value: TVamModularCableOverlayOptions); protected
    function CalcJackCenterPoint(aJack : TVamModularJack):TPoint;
    function GetJackNear(const X, Y:integer; const Margin: integer = 10):TVamModularJack;
    function CheckForConnectionNear(const Source: TVamModularJack; const X, Y:integer; const Margin: integer = 10):TVamModularJack;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DeleteConnectionsToInputJack(const aInputJack : TVamModularJack);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;

    procedure AddJack(aJack : TVamModularJack);
    procedure RemoveJack(aJack : TVamModularJack);

    procedure UpdatePendingConnection(const Source : TVamModularJack; EndPointX, EndPointY : integer);
    procedure ResolvePendingConnection(const Source : TVamModularJack; EndPointX, EndPointY : integer);
    procedure ClearPendingConnection;

    property PendingConnection : TPendingConnection read fPendingConnection;

    property Connections : TVamModularConnections read fConnections;
  published
    {$INCLUDE TControlProperties.inc}

    property Options : TVamModularCableOverlayOptions read fOptions write SetOptions;

    property OnNewConnection    : TNewModularConnectionEvent    read fOnNewConnection    write fOnNewConnection;
    property OnDeleteConnection : TDeleteModularConnectionEvent read fOnDeleteConnection write fOnDeleteConnection;
  end;

  IVamModularConnection = interface
    ['{EBB95562-DB98-4CE4-B62E-4CA5209E57A5}']
    function GetColor : TRedFoxColor;
    function GetInputJack : TVamModularJack;
    function GetOutputJack : TVamModularJack;
    procedure SetColor(aColor : TRedFoxColor);
    procedure SetInputJack(aJack : TVamModularJack);
    procedure SetOutputJack(aJack : TVamModularJack);
  end;

  TVamModularConnection = class(TInterfacedObject, IVamModularConnection)
  strict private
    fColor: TRedFoxColor;
    fOutputJack: TVamModularJack;
    fInputJack: TVamModularJack;

    function GetColor : TRedFoxColor;
    function GetInputJack : TVamModularJack;
    function GetOutputJack : TVamModularJack;
    procedure SetColor(aColor : TRedFoxColor);
    procedure SetInputJack(aJack : TVamModularJack);
    procedure SetOutputJack(aJack : TVamModularJack);
  private
  public
    constructor Create;
    destructor Destroy; override;

    property Color      : TRedFoxColor    read fColor      write fColor;
    property InputJack  : TVamModularJack read fInputJack write fInputJack;
    property OutputJack : TVamModularJack read fOutputJack write fOutputJack;
  end;

  TNewConnectionInfo = record
    Color : TRedFoxColor;
    InputJack : TVamModularJack;
    OutputJack : TVamModularJack;
  end;

  TVamModularConnections = class
  strict private
  private
    function GetConnectionCount: integer;
    function GetConnection(Index: integer): IVamModularConnection;
  strict protected
    fConnectionsList : TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure NewConnection(CreateInfo : TNewConnectionInfo);
    procedure Remove(aConnection : IVamModularConnection);

    procedure ClearConnectionsToJack(aJack : TVamModularJack);

    function FindConnection(aOutputJack, aInputJack : TVamModularJack):IVamModularConnection;

    property Count : integer read GetConnectionCount;
    property Connections[Index:integer] : IVamModularConnection read GetConnection; default;
  end;







implementation

uses
  SysUtils,
  Math,
  AggColor;

{ TVamModularJack }

constructor TVamModularJack.Create(AOwner: TComponent);
begin
  inherited;

  fBackgroundColor.SetColor('$FF3A3A3A');
  fOutputColor.SetColor('$FFF93912');
  fInputColor.SetColor('$FFB1B1B1');
  fJackType := jtOutput;
  fJackSize := 20;

  fCableOverlay := nil;
end;

destructor TVamModularJack.Destroy;
begin

  inherited;
end;

function TVamModularJack.GetBackgroundColor: TRedFoxColorString;
begin
  result := fBackgroundColor.AsString;
end;

function TVamModularJack.GetInputColor: TRedFoxColorString;
begin
  result := fInputColor.AsString;
end;

function TVamModularJack.GetOutputColor: TRedFoxColorString;
begin
  result := fOutputColor.AsString;
end;

procedure TVamModularJack.SetBackgroundColor(const Value: TRedFoxColorString);
begin
  if Value <> fBackgroundColor.AsString then
  begin
    fBackgroundColor.SetColor(Value);
    Invalidate;
  end;
end;

procedure TVamModularJack.SetCableOverlay(const Value: TVamModularCableOverlay);
begin
  // remove the jack from the previous connection manager...
  if assigned(fCableOverlay) then
  begin
    fCableOverlay.RemoveJack(self);
    fCableOverlay.RemoveFreeNotification(self);
  end;

  fCableOverlay := Value;

  // add the jack to the new connection manager...
  if assigned(fCableOverlay) then
  begin
    fCableOverlay.AddJack(self);
    fCableOverlay.FreeNotification(self);
  end;
end;

procedure TVamModularJack.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (AComponent = fCableOverlay) and (Operation = opRemove) then
  begin
    fCableOverlay := nil;
  end;
end;

procedure TVamModularJack.SetInputColor(const Value: TRedFoxColorString);
begin
  if Value <> fInputColor.AsString then
  begin
    fInputColor.SetColor(Value);
    Invalidate;
  end;
end;

procedure TVamModularJack.SetJackSize(const Value: integer);
begin
  if Value <> fJackSize then
  begin
    fJackSize := Value;
    Invalidate;
  end;
end;

procedure TVamModularJack.SetJackType(const Value: TJackType);
begin
  if Value <> fJackType then
  begin
    fJackType := Value;
    Invalidate;
  end;
end;

procedure TVamModularJack.SetOutputColor(const Value: TRedFoxColorString);
begin
  if Value <> fOutputColor.AsString then
  begin
    fOutputColor.SetColor(Value);
    Invalidate;
  end;
end;


procedure TVamModularJack.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not assigned(CableOverlay) then exit;

  if (Button = mbLeft) then
  begin
    IsGrabbed := true;
    CableOverlay.UpdatePendingConnection(self,X,Y);
  end;

end;

procedure TVamModularJack.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not assigned(CableOverlay) then exit;


  if IsGrabbed then
  begin
    CableOverlay.UpdatePendingConnection(self,X,Y);
  end;
end;

procedure TVamModularJack.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not assigned(CableOverlay) then exit;


  if (Button = mbLeft) and (IsGrabbed = true) then
  begin
    IsGrabbed := false;
    CableOverlay.ResolvePendingConnection(self, X, Y);
  end;

end;





procedure TVamModularJack.Paint;
var
  cx, cy, cr : single;
begin
  inherited;

  BackBuffer.BufferInterface.ClearAll(0,0,0,0);

  BackBuffer.BufferInterface.NoLine;

  BackBuffer.BufferInterface.FillColor := fBackgroundColor.AsAggRgba8;

  cx := Width / 2;
  cy := Height / 2;
  cr := fJackSize / 2;

  BackBuffer.BufferInterface.Circle(cx, cy, cr);

  case JackType of
    jtInput:  BackBuffer.BufferInterface.FillColor := fInputColor.AsAggRgba8;
    jtOutput: BackBuffer.BufferInterface.FillColor := fOutputColor.AsAggRgba8;
  end;


  cr := cr - 0.7;
  BackBuffer.BufferInterface.Circle(cx, cy, cr);


  BackBuffer.BufferInterface.FillColor := fBackgroundColor.AsAggRgba8;
  cr := cr - 3.5;
  BackBuffer.BufferInterface.Circle(cx, cy, cr);
end;










{ TVamModularCableOverlay }

constructor TVamModularCableOverlay.Create(AOwner: TComponent);
begin
  inherited;

  fOptions := TVamModularCableOverlayOptions.Create;

  fJackList := TVamModularJackList.Create;
  fJackList.OwnsObjects := false;

  HitTest := false;

  fConnections := TVamModularConnections.Create;
end;

destructor TVamModularCableOverlay.Destroy;
begin
  FreeAndNil(fOptions);
  fJackList.Free;
  fConnections.Free;
  inherited;
end;

procedure TVamModularCableOverlay.AddJack(aJack: TVamModularJack);
begin
  if fJackList.IndexOf(aJack) = -1 then
  begin
    fJackList.Add(aJack);
    aJack.FreeNotification(Self);
  end;
end;


procedure TVamModularCableOverlay.RemoveJack(aJack: TVamModularJack);
begin
  fJackList.Remove(aJack);
  aJack.RemoveFreeNotification(aJack);
  Connections.ClearConnectionsToJack(aJack);
  Invalidate;
end;

procedure TVamModularCableOverlay.DeleteConnectionsToInputJack(const aInputJack: TVamModularJack);
var
  c1: Integer;
  ModCon : IVamModularConnection;
begin
  assert(aInputJack.JackType = TJackType.jtInput);

  for c1 := Connections.Count-1 downto 0 do
  begin
    ModCon := Connections[c1];
    if ModCon.GetInputJack = aInputJack then
    begin
      if assigned(OnDeleteConnection) then OnDeleteConnection(self, ModCon.GetOutputJack, ModCon.GetInputJack);
      Connections.Remove(ModCon);
    end;
  end;
end;

procedure TVamModularCableOverlay.Paint;
var
  pt1, pt2 : TPoint;
  x1, y1, x2,y2 : single;
  c1: Integer;
  aConnection : IVamModularConnection;
begin
  inherited;

  BackBuffer.BufferInterface.ClearAll(0,0,0,0);


  if (csDesigning in Self.ComponentState) then
  begin
    BackBuffer.BufferInterface.NoLine;
    BackBuffer.BufferInterface.FillColor := GetAggColor(clRed, 55);
    BackBuffer.BufferInterface.Rectangle(0,0,Width, Height);
  end;

  BackBuffer.BufferInterface.LineWidth := 6;
  for c1 := 0 to Connections.Count-1 do
  begin
    aConnection := Connections[c1];

    BackBuffer.BufferInterface.LineColor := aConnection.GetColor.AsAggRgba8;

    pt1 := CalcJackCenterPoint(aConnection.GetInputJack);
    pt2 := CalcJackCenterPoint(aConnection.GetOutputJack);

    BackBuffer.BufferInterface.Line(pt1.x, pt1.y, pt2.x, pt2.y);
  end;

  if PendingConnection.IsActive then
  begin
    x1 := PendingConnection.SourceX;
    y1 := PendingConnection.SourceY;
    x2 := PendingConnection.DestX;
    y2 := PendingConnection.DestY;

    BackBuffer.BufferInterface.LineWidth := 6;
    BackBuffer.BufferInterface.LineColor := PendingConnection.Color.AsAggRgba8;

    BackBuffer.BufferInterface.Line(x1, y1, x2, y2);
  end;


end;



procedure TVamModularCableOverlay.UpdatePendingConnection(const Source: TVamModularJack; EndPointX, EndPointY: integer);
var
  aPoint : TPoint;
  R, G, B : byte;
  SourcePos, DestPos : TPoint;
  DestX, DestY : integer;
  DestJack : TVamModularJack;
begin
  if fPendingConnection.IsActive = false then
  begin
    R := 50 + Random(205);
    G := 50 + Random(205);
    B := 50 + Random(205);
    fPendingConnection.Color.SetColor(255, R,G,B);
  end;

  SourcePos := Source.GetAbsoluteBoundsRect.TopLeft;
  DestPos   := GetAbsoluteBoundsRect.TopLeft;

  if DestPos.X > 0
    then DestX := SourcePos.X + EndPointX - DestPos.X
    else DestX := SourcePos.X + EndPointX;

  if DestPos.Y > 0
    then DestY := SourcePos.Y + EndPointY - DestPos.Y
    else DestY := SourcePos.Y + EndPointY;

  // NOTE: when the cable over component has a negative top or left postion, the
  // cable will draw to the correct location despite not accounting for the
  // negative offset. (This comment isn't making much sense cause I am tired.)


  DestJack := self.CheckForConnectionNear(Source, DestX, DestY, 15);
  if assigned(DestJack) then
  begin
    aPoint := CalcJackCenterPoint(DestJack);
    DestX := aPoint.X;
    DestY := aPoint.Y;
  end;

  fPendingConnection.IsActive := true;
  fPendingConnection.SourceX := CalcJackCenterPoint(Source).X + 0.5;
  fPendingConnection.SourceY := CalcJackCenterPoint(Source).Y + 0.5;
  fPendingConnection.DestX := DestX;
  fPendingConnection.DestY := DestY;

  Invalidate;
end;

procedure TVamModularCableOverlay.ResolvePendingConnection(const Source: TVamModularJack; EndPointX, EndPointY: integer);
var
  //Allowed : boolean;
  R, G, B : byte;
  SourcePos, DestPos : TPoint;
  DestX, DestY : integer;
  DestJack : TVamModularJack;
  NewConnectionInfo : TNewConnectionInfo;
  ExistingConnection : IVamModularConnection;
begin
  if fPendingConnection.IsActive = false then
  begin
    R := 50 + Random(205);
    G := 50 + Random(205);
    B := 50 + Random(205);
    fPendingConnection.Color.SetColor(255, R,G,B);
  end;

  SourcePos := Source.GetAbsoluteBoundsRect.TopLeft;
  DestPos   := GetAbsoluteBoundsRect.TopLeft;

  if DestPos.X > 0
    then DestX := SourcePos.X + EndPointX - DestPos.X
    else DestX := SourcePos.X + EndPointX;

  if DestPos.Y > 0
    then DestY := SourcePos.Y + EndPointY - DestPos.Y
    else DestY := SourcePos.Y + EndPointY;

  // NOTE: when the cable over component has a negative top or left postion, the
  // cable will draw to the correct location despite not accounting for the
  // negative offset. (This comment isn't making much sense cause I am tired.)


  DestJack := self.CheckForConnectionNear(Source, DestX, DestY, 15);
  if assigned(DestJack) then
  begin
    assert(DestJack.JackType <> Source.JackType);

    NewConnectionInfo.Color := fPendingConnection.Color;

    if Source.JackType = jtInput then
    begin
      NewConnectionInfo.InputJack  := Source;
      NewConnectionInfo.OutputJack := DestJack;
    end else
    begin
      NewConnectionInfo.InputJack  := DestJack;
      NewConnectionInfo.OutputJack := Source;
    end;

    ExistingConnection := Connections.FindConnection(NewConnectionInfo.OutputJack, NewConnectionInfo.InputJack);

    if assigned(ExistingConnection) = false then
    begin
      //== Create new connection ==
      if Options.LimitInputConnections then DeleteConnectionsToInputJack(NewConnectionInfo.InputJack);
      if assigned(OnNewConnection) then OnNewConnection(self, NewConnectionInfo.OutputJack, NewConnectionInfo.InputJack);
      Connections.NewConnection(NewConnectionInfo);
    end else
    begin
      //== delete existing connection ==
      if assigned(OnDeleteConnection) then OnDeleteConnection(self, NewConnectionInfo.OutputJack, NewConnectionInfo.InputJack);
      Connections.Remove(ExistingConnection);
    end;
  end;

  fPendingConnection.IsActive := false;

  Invalidate;
end;



procedure TVamModularCableOverlay.SetOptions(const Value: TVamModularCableOverlayOptions);
begin
  fOptions.Assign(Value);
end;

function TVamModularCableOverlay.CalcJackCenterPoint(aJack: TVamModularJack): TPoint;
var
  SourcePos, DestPos : TPoint;
  DestX, DestY : integer;
begin
  SourcePos := aJack.GetAbsoluteBoundsRect.TopLeft;
  DestPos   := self.GetAbsoluteBoundsRect.TopLeft;

  if DestPos.X > 0
    then DestX := (aJack.Width div 2) + SourcePos.X - DestPos.X
    else DestX := (aJack.Width div 2) + SourcePos.X;

  if DestPos.Y > 0
    then DestY := (aJack.Height div 2) + SourcePos.Y - DestPos.Y
    else DestY := (aJack.Height div 2) + SourcePos.Y;

  result := Point(DestX, DestY);
end;

function TVamModularCableOverlay.CheckForConnectionNear(const Source: TVamModularJack; const X, Y,  Margin: integer): TVamModularJack;
var
  BestIndex : integer;
  BestDistance : single;
  c1: Integer;
  aPoint : TPoint;
  Dist : single;
begin
  BestIndex := -1;
  BestDistance := Margin + 1;
  result := nil;

  for c1 := 0 to fJackList.Count-1 do
  begin
    if (source.JackType) <> (fJackList[c1].JackType) then
    begin
      aPoint := CalcJackCenterPoint(fJackList[c1]);
      Dist := max(abs(x - aPoint.X), abs(y - aPoint.Y));
      if (Dist <= Margin) then
      begin
        if (BestIndex = -1) or (Dist < BestDistance) then
        begin
          BestIndex    := c1;
          BestDistance := Dist;
          result := fJackList[c1];
        end;
      end;
    end;
  end;
end;

function TVamModularCableOverlay.GetJackNear(const X, Y:integer; const Margin: integer): TVamModularJack;
var
  aPoint : TPoint;
  c1: Integer;
  Dist : single;
begin
  result := nil;

  //TODO: The function should return the closest jack to the point, not the first it finds.
  for c1 := 0 to fJackList.Count-1 do
  begin
    aPoint := CalcJackCenterPoint(fJackList[c1]);
    Dist := max(abs(x - aPoint.X), abs(y - aPoint.Y));

    if (Dist <= Margin) then
    begin
      result := fJackList[c1];
      exit; //===================>> exit >>=============>>
    end;
  end;
end;



procedure TVamModularCableOverlay.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (AComponent is TVamModularJack) and (Operation = opRemove) then
  begin
    fJackList.Remove(AComponent as TVamModularJack);
    Connections.ClearConnectionsToJack(AComponent as TVamModularJack);
    Invalidate;
  end;

end;

procedure TVamModularCableOverlay.ClearPendingConnection;
begin
  if fPendingConnection.IsActive <> false then
  begin
    fPendingConnection.IsActive := false;
    Invalidate;
  end;
end;

{ TVamModularConnection }

constructor TVamModularConnection.Create;
begin

end;

destructor TVamModularConnection.Destroy;
begin

  inherited;
end;

function TVamModularConnection.GetColor: TRedFoxColor;
begin
  result := fColor;
end;

function TVamModularConnection.GetInputJack: TVamModularJack;
begin
  result := fInputJack;
end;

function TVamModularConnection.GetOutputJack: TVamModularJack;
begin
  result := fOutputJack;
end;

procedure TVamModularConnection.SetColor(aColor: TRedFoxColor);
begin
  fColor := aColor;
end;

procedure TVamModularConnection.SetInputJack(aJack: TVamModularJack);
begin
  fInputJack := aJack;
end;

procedure TVamModularConnection.SetOutputJack(aJack: TVamModularJack);
begin
  fOutputJack := aJack;
end;

{ TVamModularConnections }

procedure TVamModularConnections.ClearConnectionsToJack(aJack: TVamModularJack);
var
  c1: Integer;
  aConnection : IVamModularConnection;
begin
  for c1 := Count-1 downto 0 do
  begin
    aConnection := Connections[c1];
    if (aConnection.GetOutputJack = aJack) or (aConnection.GetInputJack = aJack) then
    begin
      fConnectionsList.Remove(aConnection);
    end;
  end;
end;

constructor TVamModularConnections.Create;
begin
  fConnectionsList := TInterfaceList.Create;
end;

destructor TVamModularConnections.Destroy;
begin
  fConnectionsList.Free;
  inherited;
end;

function TVamModularConnections.FindConnection(aOutputJack, aInputJack: TVamModularJack): IVamModularConnection;
var
  c1: Integer;
  aConnection : IVamModularConnection;
begin
  result := nil;
  for c1 := 0 to fConnectionsList.Count-1 do
  begin
    aConnection := fConnectionsList[c1] as IVamModularConnection;
    if (aConnection.GetOutputJack = aOutputJack) and (aConnection.GetInputJack = aInputJack) then
    begin
      result := aConnection;
      exit; //=====================>> exit >>================>>
    end;
  end;
end;

function TVamModularConnections.GetConnection(Index: integer): IVamModularConnection;
begin
  result := fConnectionsList.Items[Index] as IVamModularConnection;
end;

function TVamModularConnections.GetConnectionCount: integer;
begin
  result := fConnectionsList.Count;
end;

procedure TVamModularConnections.NewConnection(CreateInfo: TNewConnectionInfo);
var
  aConnection : IVamModularConnection;
begin
  aConnection := TVamModularConnection.Create;

  aConnection.SetColor(CreateInfo.Color);
  aConnection.SetInputJack(CreateInfo.InputJack);
  aConnection.SetOutputJack(CreateInfo.OutputJack);

  fConnectionsList.Add(aConnection);
end;

procedure TVamModularConnections.Remove(aConnection: IVamModularConnection);
begin
  fConnectionsList.Remove(aConnection);
end;

{ TVamModularCableOverlayOptions }

procedure TVamModularCableOverlayOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TVamModularCableOverlay then
  begin
    with TVamModularCableOverlay(Dest) do
    begin
      fLimitInputConnections := self.fLimitInputConnections;
    end;
  end else
  begin
    inherited;
  end;

end;

end.
