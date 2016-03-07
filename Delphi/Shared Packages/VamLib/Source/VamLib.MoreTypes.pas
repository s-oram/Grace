unit VamLib.MoreTypes;

interface

const
  // http://stackoverflow.com/q/15993428/395461
  kMaxArrayOfSingleLength = MaxInt div SizeOf(single);

type
  PObject = ^TObject;

  TComplex = record
    Real:single;
    Imag:single;
  end;

  TArrayOfComplex = array of TComplex;

  PSinglePoint = ^TSinglePoint;
  TSinglePoint = packed record
    X:single;
    Y:single;
  end;

  TSingleRect = packed record
    case Integer of
    0: (Left, Top, Right, Bottom: Single);    1: (TopLeft, BottomRight: TSinglePoint);  end;

  TSingleTriangle = record
    x1,y1,x2,y2,x3,y3:single;
  end;

  PPDouble = ^PDouble;
  PDouble = ^Double;

  PPSingle = ^PSingle;
  PSingle  = ^Single;

  // TODO:MED Instead of using custom definitions, I should
  // use the dynamic array types in System.Types.
  // These declarations should alias the System.Types.
  PArrayOfPSingle  = ^TArrayOfPSingle;
  TArrayOfPSingle  = array of PSingle;
  PArrayOfSingle   = ^TArrayOfSingle;
  TArrayOfSingle   = array of single;
  TArrayOfDouble   = array of double;
  TArrayOfInteger  = array of integer;
  TArrayOfSmallInt = array of smallInt;
  TArrayOfString   = array of string;
  TArrayOfBoolean  = array of boolean;

  PStaticArrayOfSingle = ^TStaticArrayOfSingle;
  TStaticArrayOfSingle = array[0..kMaxArrayOfSingleLength-1] of single;

  T2dArrayOfSingle   = array of TArrayOfSingle;
  T2dArrayOfDouble   = array of TArrayOfDouble;
  T2dArrayOfSmallInt = array of TArrayOfSmallInt;
  T2dArrayOfBoolean  = array of TArrayOfBoolean;

  TTuple<T> = record
  private
    FValue: T;
  public
    constructor Create(const AVal: T);
    property Value1: T read FValue write FValue;
  end;

  TTuple<T1, T2> = record
  private
    FValue1: T1;
    FValue2: T2;
  public
    constructor Create(const AVal1: T1; const AVal2: T2);
    property Value1: T1 read FValue1 write FValue1;
    property Value2: T2 read FValue2 write FValue2;
  end;

  TTuple<T1, T2, T3> = record
  private
    FValue1: T1;
    FValue2: T2;
    FValue3: T3;
  public
    constructor Create(const AVal1: T1; const AVal2: T2; const AVal3: T3);
    property Value1: T1 read FValue1 write FValue1;
    property Value2: T2 read FValue2 write FValue2;
    property Value3: T3 read FValue3 write FValue3;
  end;


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

{ TTuple<T> }

constructor TTuple<T>.Create(const AVal: T);
begin
  FValue := AVal;
end;

{ TTuple<T1, T2> }

constructor TTuple<T1, T2>.Create(const AVal1: T1; const AVal2: T2);
begin
  FValue1 := AVal1;
  FValue2 := AVal2;
end;

{ TTuple<T1, T2, T3> }

constructor TTuple<T1, T2, T3>.Create(const AVal1: T1; const AVal2: T2; const AVal3: T3);
begin
  FValue1 := AVal1;
  FValue2 := AVal2;
  FValue3 := AVal3;
end;

end.
