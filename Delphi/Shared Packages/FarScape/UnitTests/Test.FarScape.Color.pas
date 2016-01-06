unit Test.FarScape.Color;

interface

uses
  FarScape.Color,
  WatchTower;

type
  TFarScapeColorTest = class(TWatchTowerTest)
  private
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure StrToColorConversion;

    [Test]
    procedure ColorToStrConversion;

    [Test]
    procedure GraphicsColorConversion;
  end;

implementation

uses
  WatchTower.Confirm,
  Vcl.Graphics;

{ TFarScapeColorTest }

procedure TFarScapeColorTest.Setup;
begin
  inherited;

end;

procedure TFarScapeColorTest.TearDown;
begin
  inherited;

end;

procedure TFarScapeColorTest.StrToColorConversion;
var
  CA : TFarScapeColor;
begin
  CA := '$FF000000';
  Confirm.IsTrue(  CA.A = 255  );
  Confirm.IsTrue(  CA.R = 0  );
  Confirm.IsTrue(  CA.G = 0  );
  Confirm.IsTrue(  CA.B = 0  );

  CA := '$00FF0000';
  Confirm.IsTrue(  CA.A = 0  );
  Confirm.IsTrue(  CA.R = 255  );
  Confirm.IsTrue(  CA.G = 0  );
  Confirm.IsTrue(  CA.B = 0  );

  CA := '$0000FF00';
  Confirm.IsTrue(  CA.A = 0  );
  Confirm.IsTrue(  CA.R = 0  );
  Confirm.IsTrue(  CA.G = 255  );
  Confirm.IsTrue(  CA.B = 0  );

  CA := '$000000FF';
  Confirm.IsTrue(  CA.A = 0  );
  Confirm.IsTrue(  CA.R = 0  );
  Confirm.IsTrue(  CA.G = 0  );
  Confirm.IsTrue(  CA.B = 255  );
end;

procedure TFarScapeColorTest.ColorToStrConversion;
var
  CA : TFarScapeColor;
  str : string;
begin
  CA.NoColor;
  CA.A := 255;
  str := CA;
  Confirm.IsTrue( str = '$FF000000'  );

  CA.NoColor;
  CA.R := 255;
  str := CA;
  Confirm.IsTrue( str = '$00FF0000'  );

  CA.NoColor;
  CA.G := 255;
  str := CA;
  Confirm.IsTrue( str = '$0000FF00'  );

  CA.NoColor;
  CA.B := 255;
  str := CA;
  Confirm.IsTrue( str = '$000000FF'  );
end;


procedure TFarScapeColorTest.GraphicsColorConversion;
var
  CA : TFarScapeColor;
begin
   CA := clBlack;
  Confirm.IsTrue(  CA.A = 255  );
  Confirm.IsTrue(  CA.R = 0  );
  Confirm.IsTrue(  CA.G = 0  );
  Confirm.IsTrue(  CA.B = 0  );

  CA := clRed;
  Confirm.IsTrue(  CA.A = 255  );
  Confirm.IsTrue(  CA.R = 255  );
  Confirm.IsTrue(  CA.G = 0  );
  Confirm.IsTrue(  CA.B = 0  );

  CA := clLime;
  Confirm.IsTrue(  CA.A = 255  );
  Confirm.IsTrue(  CA.R = 0  );
  Confirm.IsTrue(  CA.G = 255  );
  Confirm.IsTrue(  CA.B = 0  );

  CA := clBlue;
  Confirm.IsTrue(  CA.A = 255  );
  Confirm.IsTrue(  CA.R = 0  );
  Confirm.IsTrue(  CA.G = 0  );
  Confirm.IsTrue(  CA.B = 255  );
end;


end.
