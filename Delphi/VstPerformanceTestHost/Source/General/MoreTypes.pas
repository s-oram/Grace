unit MoreTypes;

interface


type
  PObject = ^TObject;

  TComplex = record
    Real:single;
    Imag:single;
  end;

  TArrayOfComplex = array of TComplex;

  PSinglePoint = ^TSinglePoint;
  TSinglePoint = record
    X:single;
    Y:single;
  end;

  //TODO should check if this works as it should....
  TSingleRect = packed record
    case Integer of
    0: (Left, Top, Right, Bottom: Single);

  TSingleTriangle = record
    x1,y1,x2,y2,x3,y3:single;
  end;


  PPSingle = ^PSingle;
  PSingle  = ^Single;

  PArrayOfPSingle  = ^TArrayOfPSingle;
  TArrayOfPSingle  = array of PSingle;
  PArrayOfSingle   = ^TArrayOfSingle;
  TArrayOfSingle   = array of single;
  TArrayOfDouble   = array of double;
  TArrayOfInteger  = array of integer;
  TArrayOfSmallInt = array of smallInt;
  TArrayOfString   = array of string;
  TArrayOfBoolean  = array of boolean;

  T2dArrayOfSingle   = array of TArrayOfSingle;
  T2dArrayOfDouble   = array of TArrayOfDouble;
  T2dArrayOfSmallInt = array of TArrayOfSmallInt;
  T2dArrayOfBoolean  = array of TArrayOfBoolean;

function SinglePoint(X,Y:single):TSinglePoint;
function SingleRect(x1,y1,x2,y2:single):TSingleRect;


implementation

function SinglePoint(X,Y:single):TSinglePoint;
begin
  result.X := X;
  result.Y := Y;
end;

function SingleRect(x1,y1,x2,y2:single):TSingleRect;
begin
  result.Left := x1;
  result.Top := y1;
  result.Right := x2;
  result.Bottom := y2;
end;


end.