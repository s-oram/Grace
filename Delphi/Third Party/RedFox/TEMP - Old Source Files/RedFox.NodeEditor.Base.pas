unit RedFox.NodeEditor.Base;

interface

uses
  Types, Contnrs;

type
  TRedFoxNode = class
  private
    fPosition: TPointF;
    function GetX: single;
    function GetY: single;
    procedure SetX(const Value: single);
    procedure SetY(const Value: single);
  public
    constructor Create;
    destructor Destroy; override;

    property X : single read GetX write SetX;
    property Y : single read GetY write SetY;

    property Position : TPointF read fPosition write fPosition;
  end;

  TRedFoxNodeList = class(TObjectList)
  private
    function GetRedFoxNode(Index: integer): TRedFoxNode;
  protected
  public
    property Nodes[Index:integer]:TRedFoxNode read GetRedFoxNode; default;

    function InsetNodeAt(x, y : single):integer;
    procedure AddNode(x, y:single);

    procedure AssignFrom(Source:TRedFoxNodeList);
  end;


implementation

{ TRedFoxNode }

constructor TRedFoxNode.Create;
begin

end;

destructor TRedFoxNode.Destroy;
begin

  inherited;
end;

function TRedFoxNode.GetX: single;
begin
  result := fPosition.X;
end;

function TRedFoxNode.GetY: single;
begin
  result := fPosition.Y;
end;

procedure TRedFoxNode.SetX(const Value: single);
begin
  fPosition.X := Value;
end;

procedure TRedFoxNode.SetY(const Value: single);
begin
  fPosition.Y := Value;
end;

{ TRedFoxNodeList }


{ TRedFoxNodeList }

function TRedFoxNodeList.GetRedFoxNode(Index: integer): TRedFoxNode;
begin
  result := inherited Items[Index] as TRedFoxNode;
end;

function TRedFoxNodeList.InsetNodeAt(x, y: single):integer;
var
  aNode : TRedFoxNode;
  c1: Integer;
begin

  aNode := TRedFoxNode.Create;
  aNode.X := X;
  aNode.Y := Y;

  if Count = 0 then
  begin
    result := self.Add(aNode);
    exit; //===================>>exit>>=========>>
  end else
  begin
    if x < Nodes[0].X then
    begin
      self.Insert(0, aNode);
      result := 0;
      exit; //===================>>exit>>=========>>
    end;

    for c1 := Count-1 downto 0 do
    begin
      if Nodes[c1].X < x then
      begin
        self.Insert(c1 + 1, aNode);
        result := c1 + 1;
        exit; //===================>>exit>>=========>>
      end;
    end;
  end;

  //we should never make it this far.


end;

procedure TRedFoxNodeList.AddNode(x, y: single);
var
  aNode : TRedFoxNode;
begin
  aNode := TRedFoxNode.Create;
  aNode.Position := PointF(x,y);
  self.Add(aNode);
end;

procedure TRedFoxNodeList.AssignFrom(Source: TRedFoxNodeList);
var
  c1: Integer;
begin
  if Source.Count <> Self.Count then
  begin
    self.Clear;
    for c1 := 0 to Source.Count-1 do
    begin
      Self.AddNode(Source[c1].X, Source[c1].Y);
    end;
  end else
  begin
    for c1 := 0 to Source.Count-1 do
    begin
      Self.Nodes[c1].Position := Source.Nodes[c1].Position;
    end;
  end;
end;







end.
