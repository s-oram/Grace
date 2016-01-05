unit FarScape.Color;

interface

uses
  Graphics;

type
  TFarScapeColor = record
  public
    class operator Implicit(a : TFarScapeColor):string;
    class operator Implicit(a : string):TFarScapeColor;

    class operator Implicit(a : TColor):TFarScapeColor;
  public
    procedure NoColor;
    procedure Black;
    procedure RandomColor;
    case byte of
    0: (R, G, B, A: Byte);
    1: (FColor : integer);
  end;

implementation

uses
  SysUtils;

{ TFarScapeColor }

class operator TFarScapeColor.Implicit(a: TColor): TFarScapeColor;
var
  Temp : Longint;
begin
  Temp := ColorToRGB(a);
  result.FColor := Temp;
  result.A := 255;
end;

class operator TFarScapeColor.Implicit(a: TFarScapeColor): string;
var
  Temp : TFarScapeColor;
begin
  Temp.R := a.B;
  Temp.G := a.G;
  Temp.B := a.R;
  Temp.A := a.A;
  result := '$' + IntToHex(Temp.FColor,8);
end;

class operator TFarScapeColor.Implicit(a: string): TFarScapeColor;
var
  Temp : TFarScapeColor;
begin
  Temp.FColor := StrToInt(a);
  result.R := Temp.B;
  result.G := Temp.G;
  result.B := Temp.R;
  result.A := Temp.A;
end;

procedure TFarScapeColor.NoColor;
begin
  self.FColor := 0;
end;

procedure TFarScapeColor.RandomColor;
begin
  self.FColor := Random($FFFFFFFF);
end;

procedure TFarScapeColor.Black;
begin
  self.FColor := 0;
  self.A := 255;
end;





end.
