unit AggMath;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
//    Maintained by Christian-W. Budde (Christian@savioursofsoul.de)          //
//    Copyright (c) 2012                                                      //
//                                                                            //
//  Based on:                                                                 //
//    Pascal port by Milan Marusinec alias Milano (milan@marusinec.sk)        //
//    Copyright (c) 2005-2006, see http://www.aggpas.org                      //
//                                                                            //
//  Original License:                                                         //
//    Anti-Grain Geometry - Version 2.4 (Public License)                      //
//    Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)     //
//    Contact: McSeem@antigrain.com / McSeemAgg@yahoo.com                     //
//                                                                            //
//  Permission to copy, use, modify, sell and distribute this software        //
//  is granted provided this copyright notice appears in all copies.          //
//  This software is provided "as is" without express or implied              //
//  warranty, and with no claim as to its suitability for any purpose.        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Bessel function (besj) was adapted for use in AGG library by Andy Wilk    //
//  Contact: castor.vulgaris@gmail.com                                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}
{$Q-}
{$R-}

uses
  Math,
  AggBasics,
  AggVertexSequence;

type
  PAggPolyXY = ^TAggPolyXY;
  TAggPolyXY = array [0..0] of TPointDouble;

  PAggStorageXY = ^TAggStorageXY;
  TAggStorageXY = record
    Poly: PAggPolyXY;
    Size: Cardinal;
  end;

const
  CAggIntersectionEpsilon: Double = 1.0E-30;

// Tables for fast sqrt
const
  CAggSqrtTable: array [0..1023] of Int16u = (0, 2048, 2896, 3547, 4096, 4579,
    5017, 5418, 5793, 6144, 6476, 6792, 7094, 7384, 7663, 7932, 8192, 8444,
    8689, 8927, 9159, 9385, 9606, 9822, 10033, 10240, 10443, 10642, 10837,
    11029, 11217, 11403, 11585, 11765, 11942, 12116, 12288, 12457, 12625, 12790,
    12953, 13114, 13273, 13430, 13585, 13738, 13890, 14040, 14189, 14336, 14482,
    14626, 14768, 14910, 15050, 15188, 15326, 15462, 15597, 15731, 15864, 15995,
    16126, 16255, 16384, 16512, 16638, 16764, 16888, 17012, 17135, 17257, 17378,
    17498, 17618, 17736, 17854, 17971, 18087, 18203, 18318, 18432, 18545, 18658,
    18770, 18882, 18992, 19102, 19212, 19321, 19429, 19537, 19644, 19750, 19856,
    19961, 20066, 20170, 20274, 20377, 20480, 20582, 20684, 20785, 20886, 20986,
    21085, 21185, 21283, 21382, 21480, 21577, 21674, 21771, 21867, 21962, 22058,
    22153, 22247, 22341, 22435, 22528, 22621, 22713, 22806, 22897, 22989, 23080,
    23170, 23261, 23351, 23440, 23530, 23619, 23707, 23796, 23884, 23971, 24059,
    24146, 24232, 24319, 24405, 24491, 24576, 24661, 24746, 24831, 24915, 24999,
    25083, 25166, 25249, 25332, 25415, 25497, 25580, 25661, 25743, 25824, 25905,
    25986, 26067, 26147, 26227, 26307, 26387, 26466, 26545, 26624, 26703, 26781,
    26859, 26937, 27015, 27092, 27170, 27247, 27324, 27400, 27477, 27553, 27629,
    27705, 27780, 27856, 27931, 28006, 28081, 28155, 28230, 28304, 28378, 28452,
    28525, 28599, 28672, 28745, 28818, 28891, 28963, 29035, 29108, 29180, 29251,
    29323, 29394, 29466, 29537, 29608, 29678, 29749, 29819, 29890, 29960, 30030,
    30099, 30169, 30238, 30308, 30377, 30446, 30515, 30583, 30652, 30720, 30788,
    30856, 30924, 30992, 31059, 31127, 31194, 31261, 31328, 31395, 31462, 31529,
    31595, 31661, 31727, 31794, 31859, 31925, 31991, 32056, 32122, 32187, 32252,
    32317, 32382, 32446, 32511, 32575, 32640, 32704, 32768, 32832, 32896, 32959,
    33023, 33086, 33150, 33213, 33276, 33339, 33402, 33465, 33527, 33590, 33652,
    33714, 33776, 33839, 33900, 33962, 34024, 34086, 34147, 34208, 34270, 34331,
    34392, 34453, 34514, 34574, 34635, 34695, 34756, 34816, 34876, 34936, 34996,
    35056, 35116, 35176, 35235, 35295, 35354, 35413, 35472, 35531, 35590, 35649,
    35708, 35767, 35825, 35884, 35942, 36001, 36059, 36117, 36175, 36233, 36291,
    36348, 36406, 36464, 36521, 36578, 36636, 36693, 36750, 36807, 36864, 36921,
    36978, 37034, 37091, 37147, 37204, 37260, 37316, 37372, 37429, 37485, 37540,
    37596, 37652, 37708, 37763, 37819, 37874, 37929, 37985, 38040, 38095, 38150,
    38205, 38260, 38315, 38369, 38424, 38478, 38533, 38587, 38642, 38696, 38750,
    38804, 38858, 38912, 38966, 39020, 39073, 39127, 39181, 39234, 39287, 39341,
    39394, 39447, 39500, 39553, 39606, 39659, 39712, 39765, 39818, 39870, 39923,
    39975, 40028, 40080, 40132, 40185, 40237, 40289, 40341, 40393, 40445, 40497,
    40548, 40600, 40652, 40703, 40755, 40806, 40857, 40909, 40960, 41011, 41062,
    41113, 41164, 41215, 41266, 41317, 41368, 41418, 41469, 41519, 41570, 41620,
    41671, 41721, 41771, 41821, 41871, 41922, 41972, 42021, 42071, 42121, 42171,
    42221, 42270, 42320, 42369, 42419, 42468, 42518, 42567, 42616, 42665, 42714,
    42763, 42813, 42861, 42910, 42959, 43008, 43057, 43105, 43154, 43203, 43251,
    43300, 43348, 43396, 43445, 43493, 43541, 43589, 43637, 43685, 43733, 43781,
    43829, 43877, 43925, 43972, 44020, 44068, 44115, 44163, 44210, 44258, 44305,
    44352, 44400, 44447, 44494, 44541, 44588, 44635, 44682, 44729, 44776, 44823,
    44869, 44916, 44963, 45009, 45056, 45103, 45149, 45195, 45242, 45288, 45334,
    45381, 45427, 45473, 45519, 45565, 45611, 45657, 45703, 45749, 45795, 45840,
    45886, 45932, 45977, 46023, 46069, 46114, 46160, 46205, 46250, 46296, 46341,
    46386, 46431, 46477, 46522, 46567, 46612, 46657, 46702, 46746, 46791, 46836,
    46881, 46926, 46970, 47015, 47059, 47104, 47149, 47193, 47237, 47282, 47326,
    47370, 47415, 47459, 47503, 47547, 47591, 47635, 47679, 47723, 47767, 47811,
    47855, 47899, 47942, 47986, 48030, 48074, 48117, 48161, 48204, 48248, 48291,
    48335, 48378, 48421, 48465, 48508, 48551, 48594, 48637, 48680, 48723, 48766,
    48809, 48852, 48895, 48938, 48981, 49024, 49067, 49109, 49152, 49195, 49237,
    49280, 49322, 49365, 49407, 49450, 49492, 49535, 49577, 49619, 49661, 49704,
    49746, 49788, 49830, 49872, 49914, 49956, 49998, 50040, 50082, 50124, 50166,
    50207, 50249, 50291, 50332, 50374, 50416, 50457, 50499, 50540, 50582, 50623,
    50665, 50706, 50747, 50789, 50830, 50871, 50912, 50954, 50995, 51036, 51077,
    51118, 51159, 51200, 51241, 51282, 51323, 51364, 51404, 51445, 51486, 51527,
    51567, 51608, 51649, 51689, 51730, 51770, 51811, 51851, 51892, 51932, 51972,
    52013, 52053, 52093, 52134, 52174, 52214, 52254, 52294, 52334, 52374, 52414,
    52454, 52494, 52534, 52574, 52614, 52654, 52694, 52734, 52773, 52813, 52853,
    52892, 52932, 52972, 53011, 53051, 53090, 53130, 53169, 53209, 53248, 53287,
    53327, 53366, 53405, 53445, 53484, 53523, 53562, 53601, 53640, 53679, 53719,
    53758, 53797, 53836, 53874, 53913, 53952, 53991, 54030, 54069, 54108, 54146,
    54185, 54224, 54262, 54301, 54340, 54378, 54417, 54455, 54494, 54532, 54571,
    54609, 54647, 54686, 54724, 54762, 54801, 54839, 54877, 54915, 54954, 54992,
    55030, 55068, 55106, 55144, 55182, 55220, 55258, 55296, 55334, 55372, 55410,
    55447, 55485, 55523, 55561, 55599, 55636, 55674, 55712, 55749, 55787, 55824,
    55862, 55900, 55937, 55975, 56012, 56049, 56087, 56124, 56162, 56199, 56236,
    56273, 56311, 56348, 56385, 56422, 56459, 56497, 56534, 56571, 56608, 56645,
    56682, 56719, 56756, 56793, 56830, 56867, 56903, 56940, 56977, 57014, 57051,
    57087, 57124, 57161, 57198, 57234, 57271, 57307, 57344, 57381, 57417, 57454,
    57490, 57527, 57563, 57599, 57636, 57672, 57709, 57745, 57781, 57817, 57854,
    57890, 57926, 57962, 57999, 58035, 58071, 58107, 58143, 58179, 58215, 58251,
    58287, 58323, 58359, 58395, 58431, 58467, 58503, 58538, 58574, 58610, 58646,
    58682, 58717, 58753, 58789, 58824, 58860, 58896, 58931, 58967, 59002, 59038,
    59073, 59109, 59144, 59180, 59215, 59251, 59286, 59321, 59357, 59392, 59427,
    59463, 59498, 59533, 59568, 59603, 59639, 59674, 59709, 59744, 59779, 59814,
    59849, 59884, 59919, 59954, 59989, 60024, 60059, 60094, 60129, 60164, 60199,
    60233, 60268, 60303, 60338, 60373, 60407, 60442, 60477, 60511, 60546, 60581,
    60615, 60650, 60684, 60719, 60753, 60788, 60822, 60857, 60891, 60926, 60960,
    60995, 61029, 61063, 61098, 61132, 61166, 61201, 61235, 61269, 61303, 61338,
    61372, 61406, 61440, 61474, 61508, 61542, 61576, 61610, 61644, 61678, 61712,
    61746, 61780, 61814, 61848, 61882, 61916, 61950, 61984, 62018, 62051, 62085,
    62119, 62153, 62186, 62220, 62254, 62287, 62321, 62355, 62388, 62422, 62456,
    62489, 62523, 62556, 62590, 62623, 62657, 62690, 62724, 62757, 62790, 62824,
    62857, 62891, 62924, 62957, 62991, 63024, 63057, 63090, 63124, 63157, 63190,
    63223, 63256, 63289, 63323, 63356, 63389, 63422, 63455, 63488, 63521, 63554,
    63587, 63620, 63653, 63686, 63719, 63752, 63785, 63817, 63850, 63883, 63916,
    63949, 63982, 64014, 64047, 64080, 64113, 64145, 64178, 64211, 64243, 64276,
    64309, 64341, 64374, 64406, 64439, 64471, 64504, 64536, 64569, 64601, 64634,
    64666, 64699, 64731, 64763, 64796, 64828, 64861, 64893, 64925, 64957, 64990,
    65022, 65054, 65086, 65119, 65151, 65183, 65215, 65247, 65279, 65312, 65344,
    65376, 65408, 65440, 65472, 65504);

  CAggElderBitTable: array [0..255] of Int8 = (0, 0, 1, 1, 2, 2, 2, 2, 3, 3,
    3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7);

function CalculatePointLocation(X1, Y1, X2, Y2, X, Y: Double): Double; overload;
function CalculatePointLocation(Point1, Point2: TPointDouble; X, Y: Double)
  : Double; overload;

function PointInTriangle(X1, Y1, X2, Y2, X3, Y3, X, Y: Double): Boolean;
  overload;
function PointInTriangle(Point1, Point2, Point3: TPointDouble; X, Y: Double)
  : Boolean; overload;

function CalculateDistance(X1, Y1, X2, Y2: Double): Double; overload;
function CalculateDistance(Point1, Point2: TPointDouble): Double; overload;

function CalculateLinePointDistance(X1, Y1, X2, Y2, X, Y: Double): Double;
function CalculateIntersection(Ax, Ay, Bx, By, Cx, Cy, Dx, Dy: Double;
  X, Y: PDouble): Boolean;

function IntersectionExists(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double): Boolean;

procedure CalculateOrthogonal(Thickness, X1, Y1, X2, Y2: Double; X, Y: PDouble);
procedure DilateTriangle(X1, Y1, X2, Y2, X3, Y3: Double; X, Y: PDouble;
  D: Double);

function CalculateTriangleArea(X1, Y1, X2, Y2, X3, Y3: Double): Double;
function CalculatePolygonArea(St: PAggStorageXY): Double;
function CalculatePolygonAreaVertexSequence(St: TAggVertexSequence): Double;
function FastSqrt(Val: Cardinal): Cardinal;
function Besj(X: Double; N: Integer): Double;
function CrossProduct(X1, Y1, X2, Y2, X, Y: Double): Double;

function Hypot(X, Y: Double): Double;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure SinCos(Theta: Double; out Sin, Cos: Double);
  {$IFDEF SUPPORTS_INLINE} {$IFNDEF AGG_CPU_386} inline; {$ENDIF} {$ENDIF}
procedure SinCosScale(Theta: Double; out Sin, Cos: Double; Scale: Double);
  {$IFDEF SUPPORTS_INLINE} {$IFNDEF AGG_CPU_386} inline; {$ENDIF} {$ENDIF}
  overload;
procedure SinCosScale(Theta: Double; out Sin, Cos: Double; ScaleSin,
  ScaleCos: Double); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

implementation

function CalculatePointLocation(X1, Y1, X2, Y2, X, Y: Double): Double;
begin
  Result := (X - X2) * (Y2 - Y1) - (Y - Y2) * (X2 - X1);
end;

function CalculatePointLocation(Point1, Point2: TPointDouble;
  X, Y: Double): Double;
begin
  Result := (X - Point2.X) * (Point2.Y - Point1.Y) -
    (Y - Point2.Y) * (Point2.X - Point1.X);
end;

function PointInTriangle(X1, Y1, X2, Y2, X3, Y3, X, Y: Double): Boolean;
var
  PntLoc: array [0..2] of Boolean;
begin
  PntLoc[0] := CalculatePointLocation(X1, Y1, X2, Y2, X, Y) < 0.0;
  PntLoc[1] := CalculatePointLocation(X2, Y2, X3, Y3, X, Y) < 0.0;
  PntLoc[2] := CalculatePointLocation(X3, Y3, X1, Y1, X, Y) < 0.0;

  Result := (PntLoc[0] = PntLoc[1]) and (PntLoc[1] = PntLoc[2]) and
    (PntLoc[2] = PntLoc[0]);
end;

function PointInTriangle(Point1, Point2, Point3: TPointDouble; X, Y: Double)
  : Boolean; overload;
var
  PntLoc: array [0..2] of Boolean;
begin
  PntLoc[0] := CalculatePointLocation(Point1, Point2, X, Y) < 0.0;
  PntLoc[1] := CalculatePointLocation(Point2, Point3, X, Y) < 0.0;
  PntLoc[2] := CalculatePointLocation(Point3, Point1, X, Y) < 0.0;

  Result := (PntLoc[0] = PntLoc[1]) and (PntLoc[1] = PntLoc[2]) and
    (PntLoc[2] = PntLoc[0]);
end;

function CalculateDistance(X1, Y1, X2, Y2: Double): Double;
var
  Delta: TPointDouble;
begin
  Delta.X := X2 - X1;
  Delta.Y := Y2 - Y1;

  Result := Hypot(Delta.X, Delta.Y);
end;

function CalculateDistance(Point1, Point2: TPointDouble): Double;
var
  Delta: TPointDouble;
begin
  Delta.X := Point2.X - Point1.X;
  Delta.Y := Point2.Y - Point1.Y;

  Result := Hypot(Delta.X, Delta.Y);
end;

function CalculateLinePointDistance(X1, Y1, X2, Y2, X, Y: Double): Double;
var
  D: Double;
  Delta: TPointDouble;
begin
  Delta.X := X2 - X1;
  Delta.Y := Y2 - Y1;
  D := Hypot(Delta.X, Delta.Y);

  if D < CAggIntersectionEpsilon then
    Result := CalculateDistance(X1, Y1, X, Y)
  else
    Result := ((X - X2) * Delta.Y - (Y - Y2) * Delta.X) / D;
end;

function CalculateIntersection(Ax, Ay, Bx, By, Cx, Cy, Dx, Dy: Double;
  X, Y: PDouble): Boolean;
var
  R, Num, Den: Double;
begin
  Num := (Ay - Cy) * (Dx - Cx) - (Ax - Cx) * (Dy - Cy);
  Den := (Bx - Ax) * (Dy - Cy) - (By - Ay) * (Dx - Cx);

  if Abs(Den) < CAggIntersectionEpsilon then
    Result := False

  else
  begin
    R := Num / Den;
    X^ := Ax + R * (Bx - Ax);
    Y^ := Ay + R * (By - Ay);

    Result := True;
  end;
end;

function IntersectionExists(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Double): Boolean;
var
  Delta: array [0..1] of TPointDouble;
begin
  Delta[0].X := X2 - X1;
  Delta[0].Y := Y2 - Y1;
  Delta[1].X := X4 - X3;
  Delta[1].Y := Y4 - Y3;

  Result := (((X3 - X2) * Delta[0].Y - (Y3 - Y2) * Delta[0].X < 0.0) <>
    ((X4 - X2) * Delta[0].Y - (Y4 - Y2) * Delta[0].X < 0.0)) and
    (((X1 - X4) * Delta[1].Y - (Y1 - Y4) * Delta[1].X < 0.0) <>
    ((X2 - X4) * Delta[1].Y - (Y2 - Y4) * Delta[1].X < 0.0));
end;

procedure CalculateOrthogonal(Thickness, X1, Y1, X2, Y2: Double; X, Y: PDouble);
var
  D: Double;
  Delta: TPointDouble;
begin
  Delta.X := X2 - X1;
  Delta.Y := Y2 - Y1;
  D := Thickness / Hypot(Delta.X, Delta.Y);
  X^ := Delta.Y * D;
  Y^ := Delta.X * D;
end;

procedure DilateTriangle(X1, Y1, X2, Y2, X3, Y3: Double; X, Y: PDouble;
  D: Double);
var
  Loc: Double;
  Delta: array [0..3] of TPointDouble;
begin
  Delta[0] := PointDouble(0);
  Delta[1] := PointDouble(0);
  Delta[2] := PointDouble(0);
  Loc := CalculatePointLocation(X1, Y1, X2, Y2, X3, Y3);

  if Abs(Loc) > CAggIntersectionEpsilon then
  begin
    if CalculatePointLocation(X1, Y1, X2, Y2, X3, Y3) > 0.0 then
      D := -D;

    CalculateOrthogonal(D, X1, Y1, X2, Y2, @Delta[0].X, @Delta[0].Y);
    CalculateOrthogonal(D, X2, Y2, X3, Y3, @Delta[1].X, @Delta[1].Y);
    CalculateOrthogonal(D, X3, Y3, X1, Y1, @Delta[2].X, @Delta[2].Y);
  end;

  X^ := X1 + Delta[0].X;
  Inc(X);
  Y^ := Y1 - Delta[0].Y;
  Inc(Y);
  X^ := X2 + Delta[0].X;
  Inc(X);
  Y^ := Y2 - Delta[0].Y;
  Inc(Y);
  X^ := X2 + Delta[1].X;
  Inc(X);
  Y^ := Y2 - Delta[1].Y;
  Inc(Y);
  X^ := X3 + Delta[1].X;
  Inc(X);
  Y^ := Y3 - Delta[1].Y;
  Inc(Y);
  X^ := X3 + Delta[2].X;
  Inc(X);
  Y^ := Y3 - Delta[2].Y;
  Inc(Y);
  X^ := X1 + Delta[2].X;
  Inc(X);
  Y^ := Y1 - Delta[2].Y;
  Inc(Y);
end;

function CalculateTriangleArea(X1, Y1, X2, Y2, X3, Y3: Double): Double;
begin
  Result := (X1 * Y2 - X2 * Y1 + X2 * Y3 - X3 * Y2 + X3 * Y1 - X1 * Y3) * 0.5;
end;

function CalculatePolygonArea(St: PAggStorageXY): Double;
var
  I: Cardinal;
  V: PPointDouble;
  X, Y, Sum, Xs, Ys: Double;
begin
  Sum := 0.0;
  X := St.Poly[0].X;
  Y := St.Poly[0].Y;
  Xs := X;
  Ys := Y;

  if St.Size > 0 then
    for I := 1 to St.Size - 1 do
    begin
      V := @St.Poly[I];

      Sum := Sum + (X * V.Y - Y * V.X);

      X := V.X;
      Y := V.Y;
    end;

  Result := (Sum + X * Ys - Y * Xs) * 0.5;
end;

function CalculatePolygonAreaVertexSequence(St: TAggVertexSequence): Double;
var
  I: Cardinal;
  V: PAggVertexDistance;
  Pos: TPointDouble;
  Sum, Xs, Ys: Double;
begin
  Sum := 0.0;
  Pos := PAggVertexDistance(St[0]).Pos;
  Xs := Pos.X;
  Ys := Pos.Y;

  if St.Size > 0 then
    for I := 1 to St.Size - 1 do
    begin
      V := St[I];

      Sum := Sum + (Pos.X * V.Pos.Y - Pos.Y * V.Pos.X);

      Pos := V.Pos;
    end;

  Result := (Sum + Pos.X * Ys - Pos.Y * Xs) * 0.5;
end;

function FastSqrt(Val: Cardinal): Cardinal;
var
  Bit: Integer;
  T, Shift: Cardinal;
begin
  T := Val;
  Bit := 0;

  Shift := 11;

  // The following piece of code is just an emulation of the
  // Ix86 assembler command "bsr" (see below). However on old
  // Intels (like Intel MMX 233MHz) this code is about twice
  // faster (sic!) then just one "bsr". On PIII and PIV the
  // bsr is optimized quite well.
  Bit := T shr 24;

  if Bit <> 0 then
    Bit := CAggElderBitTable[Bit] + 24
  else
  begin
    Bit := (T shr 16) and $FF;

    if Bit <> 0 then
      Bit := CAggElderBitTable[Bit] + 16
    else
    begin
      Bit := (T shr 8) and $FF;

      if Bit <> 0 then
        Bit := CAggElderBitTable[Bit] + 8
      else
        Bit := CAggElderBitTable[T];
    end;
  end;

  // This is calculation sqrt itself.
  Bit := Bit - 9;

  if Bit > 0 then
  begin
    Bit := (ShrInt32(Bit, 1)) + (Bit and 1);
    Shift := Shift - Bit;
    Val := Val shr (Bit shl 1);
  end;

  Result := CAggSqrtTable[Val] shr Shift;
end;

// --------------------------------------------------------------------besj
// Function BESJ calculates Bessel function of first kind of order n
// Arguments:
// n - an integer (>=0), the order
// x - value at which the Bessel function is required
// --------------------
// C++ Mathematical Library
// Convereted from equivalent FORTRAN library
// Converetd by Gareth Walker for use by course 392 computational project
// All functions tested and yield the same results as the corresponding
// FORTRAN versions.
//
// If you have any problems using these functions please report them to
// M.Muldoon@UMIST.ac.uk
//
// Documentation available on the web
// http://www.ma.umist.ac.uk/mrm/Teaching/392/libs/392.html
// Version 1.0   8/98
// 29 October, 1999
// --------------------
// Adapted for use in AGG library by Andy Wilk (castor.vulgaris@gmail.com)
// ------------------------------------------------------------------------
function Besj(X: Double; N: Integer): Double;
var
  I, M1, M2, M8, Imax: Integer;
  D, B, B1, C2, C3, C4, C6: Double;
begin
  if N < 0 then
  begin
    Result := 0;

    Exit;
  end;

  D := 1E-6;
  B := 0;

  if Abs(X) <= D then
  begin
    if N <> 0 then
      Result := 0
    else
      Result := 1;

    Exit;
  end;

  B1 := 0; // b1 is the value from the previous iteration

  // Set up a starting order for recurrence
  M1 := Trunc(Abs(X) + 6);

  if Abs(X) > 5 then
    M1 := Trunc(Abs(1.4 * X + 60 / X));

  M2 := Trunc(N + 2 + Abs(X) * 0.25);

  if M1 > M2 then
    M2 := M1;

  // Apply recurrence down from curent max order
  repeat
    C3 := 0;
    C2 := 1E-30;
    C4 := 0;
    M8 := 1;

    if M2 div 2 * 2 = M2 then
      M8 := -1;

    Imax := M2 - 2;

    for I := 1 to Imax do
    begin
      C6 := 2 * (M2 - I) * C2 / X - C3;
      C3 := C2;
      C2 := C6;

      if M2 - I - 1 = N then
        B := C6;

      M8 := -1 * M8;

      if M8 > 0 then
        C4 := C4 + 2 * C6;
    end;

    C6 := 2 * C2 / X - C3;

    if N = 0 then
      B := C6;

    C4 := C4 + C6;
    B := B / C4;

    if Abs(B - B1) < D then
    begin
      Result := B;
      Exit;
    end;

    B1 := B;

    Inc(M2, 3);
  until False;
end;

function CrossProduct(X1, Y1, X2, Y2, X, Y: Double): Double;
begin
  Result := (X - X2) * (Y2 - Y1) - (Y - Y2) * (X2 - X1);
end;

{$IFDEF AGG_CPU_386}
procedure SinCosDouble(Theta: Double; out Sin, Cos: Double);
  {$IFDEF PUREPASCAL} {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Math.SinCos(Theta, Sin, Cos);
{$ELSE}
asm
        FLD     Theta.Double
        FSINCOS
        FSTP    [EDX].Double
        FSTP    [EAX].Double
        FWAIT
{$ENDIF}
end;
{$ENDIF}

procedure SinCos(Theta: Double; out Sin, Cos: Double);
{$IFNDEF AGG_CPU_386}
begin
  Math.SinCos(Theta, Sin, Cos);
{$ELSE}
{$IFDEF PUREPASCAL}
begin
  Math.SinCos(Theta, Sin, Cos);
{$ELSE}
asm
        FLD     Theta.Double
        FSINCOS
        FSTP    [EDX].Double
        FSTP    [EAX].Double
        FWAIT
{$ENDIF}
{$ENDIF}
end;

function Hypot(X, Y: Double): Double;
begin
  Result := Math.Hypot(X, Y);
end;

procedure SinCosScale(Theta: Double; out Sin, Cos: Double; Scale: Double);
{$IFNDEF AGG_CPU_386}
begin
  SinCos(Theta, Sin, Cos);
  Sin := Sin * Scale;
  Cos := Cos * Scale;
{$ELSE}
{$IFDEF PUREPASCAL}
begin
  SinCos(Theta, Sin, Cos);
  Sin := Sin * Scale;
  Cos := Cos * Scale;
{$ELSE}
asm
        FLD     Theta.Double
        FSINCOS
        FMUL    Scale.Double;
        FSTP    [EDX].Double
        FMUL    Scale.Double;
        FSTP    [EAX].Double
        FWAIT
{$ENDIF}
{$ENDIF}
end;

procedure SinCosScale(Theta: Double; out Sin, Cos: Double; ScaleSin,
  ScaleCos: Double);
begin
  SinCos(Theta, Sin, Cos);
  Sin := Sin * ScaleSin;
  Cos := Cos * ScaleCos;
end;

end.
