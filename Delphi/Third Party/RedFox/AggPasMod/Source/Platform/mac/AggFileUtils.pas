unit AggFileUtils;

//
// AggPas 2.4 RM3 demo framework file utility library
// Milan Marusinec alias Milano (c) 2006
//

interface

{$I AggCompiler.inc}
{$I- }

uses
  AggBasics,
  Carbon;

type
  PApiFile = ^TApiFile;
  TApiFile = record
    FileName: ShortString;
    IsOpened: Boolean;

    FSize, FRead: SInt64;

    // FSOpenFork parameters
    FFSRef: FSRef;
    FName, FFork: HFSUniStr255;
    FRef: SInt16;
  end;

function CutString(S: ShortString): ShortString;
function UpString(S: ShortString): ShortString;
function CompString(S: ShortString): ShortString;

function Str_dir(S: ShortString): ShortString;
function Dir_str(S: ShortString): ShortString;

function Str_disk(Fn: ShortString): ShortString;
function Str_path(Fn: ShortString): ShortString;
function Str_name(Fn: ShortString): ShortString;
function Str_ext(Fn: ShortString): ShortString;

function FoldName(P, N, X: ShortString): ShortString;
procedure SpreadName(Fn: ShortString; var P, N, X: ShortString);

function FileExists(Fn: ShortString): Boolean;

procedure Display(Msg: PAnsiChar);
function Pasc(Msg: ShortString): PAnsiChar;

function ApiOpenFile(var Af: TApiFile; Fname: ShortString): Boolean;
function ApiReadFile(var Af: TApiFile; Buff: Pointer; Aloc: Integer;
  var Read: Integer): Boolean;
function ApiCloseFile(var Af: TApiFile): Boolean;

function Param_Count: Integer;
function Param_str(I: Integer): ShortString;

implementation


type
  TSCAN = (
    SCAN_0, SCAN_1, SCAN_2, SCAN_3, SCAN_4, SCAN_5, SCAN_6, SCAN_7, SCAN_8,
    SCAN_9, SCAN_A, SCAN_B, SCAN_C, SCAN_D, SCAN_E, SCAN_F, SCAN_G, SCAN_H,
    SCAN_I, SCAN_J, SCAN_K, SCAN_L, SCAN_M, SCAN_N, SCAN_O, SCAN_P, SCAN_Q,
    SCAN_R, SCAN_S, SCAN_T, SCAN_U, SCAN_V, SCAN_W, SCAN_X, SCAN_Y, SCAN_Z
    );

  TITEM = (
    ITEM_0, ITEM_1, ITEM_2, ITEM_3, ITEM_4, ITEM_5, ITEM_6, ITEM_7, ITEM_8,
    ITEM_9, ITEM_A, ITEM_B, ITEM_C, ITEM_D, ITEM_E, ITEM_F, ITEM_G, ITEM_H,
    );

const
  Dir_slash = '/';

  PageEqHigh: ShortString = #1#2#3#4#5#6#7#8#9#10#11#12#13#14#15#16 +
    #17#18#19#20#21#22#23#24#25#26#27#28#29#30#31#32 +
    #33#34#35#36#37#38#39#40#41#42#43#44#45#46#47#48 +
    #49#50#51#52#53#54#55#56#57#58#59#60#61#62#63#64 +
    #65#66#67#68#69#70#71#72#73#74#75#76#77#78#79#80 +
    #81#82#83#84#85#86#87#88#89#90#91#92#93#94#95#96 +
    #65#66#67#68#69#70#71#72#73#74#75#76#77#78#79#80 +
    #81#82#83#84#85#86#87#88#89#90#123#124#125#126#127#128 +
    #129#130#131#132#133#134#135#136#137#138#139#140#141#142#143#144 +
    #145#146#147#148#149#150#151#152#153#154#155#156#157#158#159#160 +
    #161#162#163#164#165#166#167#168#169#170#171#172#173#174#175#176 +
    #177#178#179#180#181#182#183#184#185#186#187#188#189#190#191#192 +
    #193#194#195#196#197#198#199#200#201#202#203#204#205#206#207#208 +
    #209#210#211#212#213#214#215#216#217#218#219#220#221#222#223#224 +
    #225#226#227#228#229#230#231#232#233#234#235#236#237#238#239#240 +
    #241#242#243#244#245#246#247#248#249#250#251#252#253#254#255;

  
function CutString;
var
  Fcb: Byte;
  Scn: TSCAN;

begin
  Result := '';

  Scn := SCAN_1;

  if Length(S) > 0 then
    for Fcb := Length(S) downto 1 do
      case Scn of
        SCAN_1:
          case S[Fcb] of
            ' ':
            else
            begin
              Result := S[Fcb];

              Scn := SCAN_2;

            end;

          end;

        SCAN_2:
          Result := S[Fcb] + Result;
      end;
end;

function CompString;
begin
  CompString := UpString(CutString(S));
end;

function UpString;
var
  Fcb: Byte;

begin
  if Length(S) > 0 then
    for Fcb := 1 to Length(S) do
      if Byte(S[Fcb]) > 0 then
        S[Fcb] := PageEqHigh[Byte(S[Fcb])];

  Result := S;
end;

function Str_dir;
begin
  S := CutString(S);

  if Length(S) > 0 then
    if S[Length(S)] <> Dir_slash then
      S := S + Dir_slash;

  Result := S;
end;

function Dir_str;
begin
  S := CutString(S);

  if Length(S) > 0 then
    if S[Length(S)] = Dir_slash then
      Dec(Byte(S[0]));

  Result := S;
end;

function Str_disk;
var
  Fcb: Byte;
  Str: ShortString;
  Itm: TITEM;

begin
  Str := '';
  Itm := ITEM_1;

  if Length(Fn) > 0 then
    for Fcb := 1 to Length(Fn) do
      case Itm of
        ITEM_1:
          case Fn[Fcb] of
            'a'..'z', 'A'..'Z':
              begin
                Str := Fn[Fcb];
                Itm := ITEM_2;
              end;

            '\', '/':
              begin
                Str := Fn[Fcb];
                Itm := ITEM_3;
              end;

          else
            Break;
          end;

        ITEM_2:
          case Fn[Fcb] of
            ':':
              begin
                Str := Str + Fn[Fcb];
                Itm := ITEM_F;

                Break;
              end;

          else
            Break;
          end;

        ITEM_3:
          case Fn[Fcb] of
            '\', '/':
              begin
                Str := Str + Fn[Fcb];
                Itm := ITEM_4;
              end;

          else
            Break;
          end;

        ITEM_4:
          case Fn[Fcb] of
            '\', '/', ':', '<', '>', '.', '"', '|', #0..#31:
              Break;

          else
            begin
              Str := Str + Fn[Fcb];
              Itm := ITEM_F;
            end;
          end;

        ITEM_F:
          case Fn[Fcb] of
            '\', '/':
              Break;

          else
            Str := Str + Fn[Fcb];
          end;
      end;

  if Itm = ITEM_F then
    Result := Str
  else
    Result := '';
end;

function Str_path;
var
  Fcb     : Byte;
  Pth, Str: ShortString;
  Itm     : TITEM;
begin
  Pth := '';
  Str := '';
  Itm := ITEM_1;

  if Length(Fn) > 0 then
    for Fcb := 1 to Length(Fn) do
      case Itm of
        ITEM_1:
          case Fn[Fcb] of
            '\', '/':
              begin
                Str := Fn[Fcb];
                Itm := ITEM_2;
              end;

          else
            begin
              Str := Fn[Fcb];
              Itm := ITEM_3;
            end;
          end;

        ITEM_2:
          case Fn[Fcb] of
            '\', '/':
              begin
                Str := Str + Fn[Fcb];
                Itm := ITEM_3;
              end;

          else
            begin
              Pth := Str;
              Str := Fn[Fcb];
              Itm := ITEM_A;
            end;
          end;

        ITEM_3:
          case Fn[Fcb] of
            '\', '/':
              begin
                Pth := Fn[Fcb];
                Str := '';
                Itm := ITEM_A;
              end;

          else
            Str := Str + Fn[Fcb];
          end;

        ITEM_A:
          case Fn[Fcb] of
            '\', '/':
              begin
                Pth := Pth + Str + Fn[Fcb];
                Str := '';
              end;

          else
            Str := Str + Fn[Fcb];
          end;
      end;

  Result := Pth;
end;

function Str_name;
var
  Fcb     : Byte;
  Str, Ext: ShortString;
  Itm     : TITEM;
begin
  Str := '';
  Ext := '';
  Itm := ITEM_1;

  if Length(Fn) > 0 then
    for Fcb := 1 to Length(Fn) do
      case Itm of
        ITEM_1:
          case Fn[Fcb] of
            '\', '/':
              Itm := ITEM_2;

            'a'..'z', 'A'..'Z':
              begin
                Ext := Fn[Fcb];
                Itm := ITEM_4;
              end;

            '.':
              begin
                Str := '';
                Ext := Fn[Fcb];
                Itm := ITEM_B;
              end;

          else
            begin
              Str := Fn[Fcb];
              Itm := ITEM_A;
            end;
          end;

        ITEM_2:
          case Fn[Fcb] of
            '\', '/':
              Itm := ITEM_3;

            '.':
              begin
                Str := '';
                Ext := Fn[Fcb];
                Itm := ITEM_B;
              end;

          else
            begin
              Str := Fn[Fcb];
              Itm := ITEM_A;
            end;
          end;

        ITEM_3:
          case Fn[Fcb] of
            '\', '/':
              begin
                Str := '';
                Itm := ITEM_A;
              end;
          end;

        ITEM_4:
          case Fn[Fcb] of
            '\', '/':
              begin
                Str := '';
                Itm := ITEM_A;
              end;

            ':':
              Itm := ITEM_5;

            '.':
              begin
                Str := Ext;
                Ext := Fn[Fcb];
                Itm := ITEM_B;
              end;

          else
            begin
              Str := Ext + Fn[Fcb];
              Ext := '';
              Itm := ITEM_A;
            end;
          end;

        ITEM_5:
          case Fn[Fcb] of
            '\', '/':
              begin
                Str := '';
                Itm := ITEM_A;
              end;

            '.':
              begin
                Str := '';
                Ext := Fn[Fcb];
                Itm := ITEM_B;
              end;

          else
            begin
              Str := Fn[Fcb];
              Itm := ITEM_A;
            end;
          end;

        ITEM_A:
          case Fn[Fcb] of
            '\', '/':
              begin
                Str := '';
                Ext := '';
              end;

            '.':
              begin
                Ext := Fn[Fcb];
                Itm := ITEM_B;
              end;

          else
            Str := Str + Fn[Fcb];
          end;

        ITEM_B:
          case Fn[Fcb] of
            '\', '/':
              begin
                Str := '';
                Ext := '';
                Itm := ITEM_A;
              end;

            '.':
              begin
                Str := Str + Ext;
                Ext := Fn[Fcb];
              end;
          end;
      end;

  Result := Str;
end;

function Str_ext;
var
  Fcb: Byte;
  Ext: ShortString;
  Itm: TITEM;
begin
  Ext := '';
  Itm := ITEM_1;

  if Length(Fn) > 0 then
    for Fcb := 1 to Length(Fn) do
      case Itm of
        ITEM_1:
          case Fn[Fcb] of
            '\', '/':
              Itm := ITEM_2;

            '.':
              begin
                Ext := Fn[Fcb];
                Itm := ITEM_B;
              end;

          else
            Itm := ITEM_A;
          end;

        ITEM_2:
          case Fn[Fcb] of
            '\', '/':
              Itm := ITEM_3;

            '.':
              begin
                Ext := Fn[Fcb];
                Itm := ITEM_B;
              end;

          else
            Itm := ITEM_A;
          end;

        ITEM_3:
          case Fn[Fcb] of
            '\', '/':
              Itm := ITEM_A;
          end;

        ITEM_A:
          case Fn[Fcb] of
            '.':
              begin
                Ext := Fn[Fcb];
                Itm := ITEM_B;
              end;
          end;

        ITEM_B:
          case Fn[Fcb] of
            '\', '/':
              begin
                Ext := '';
                Itm := ITEM_A;
              end;

            '.':
              Ext := Fn[Fcb];

          else
            Ext := Ext + Fn[Fcb];
          end;
      end;

  Result := CutString(Ext);

  if Result = '.' then
    Result := '';
end;

function FoldName;
var
  Dsk, Nme, Pth, Ext: ShortString;
begin
  Dsk := Str_disk(P);
  Pth := Str_dir(Str_path(P));
  Nme := Str_name(N);
  Ext := Str_ext(X);

  Result := Dsk + Pth + Nme + Ext;
end;

procedure SpreadName;
begin
  P := Str_disk(Fn) + Str_dir(Str_path(Fn));
  N := Str_name(Fn);
  X := Str_ext(Fn);
end;

function FileExists;
var
  F: file;

begin
  AssignFile(F, Fn);
  Reset(F);

  if IOResult = 0 then
  begin
    Close(F);

    Result := True;

  end
  else
    Result := False;
end;

procedure Display;
var
  Dlg: DialogRef;
  Itm: DialogItemIndex;

begin
  CreateStandardAlert(KAlertPlainAlert, CFStringCreateWithCStringNoCopy(nil,
    'AGG Message', KCFStringEncodingASCII, nil),
    CFStringCreateWithCStringNoCopy(nil, Msg, KCFStringEncodingASCII, nil),
    nil, Dlg);

  RunStandardAlert(Dlg, nil, Itm);
end;

var
  Mout: ShortString;

function Pasc;
begin
  if Length(Msg) = 255 then
    Dec(Byte(Msg[0]));

  Mout := Msg + #0;
  Result := PAnsiChar(@Mout[1]);
end;

function ApiOpenFile;
var
  I: Cardinal;

  OssError : OSErr;
  OutStatus: OSStatus;
  FileSpecs: FSSpec;

begin
  Result := False;

  FillChar(Af, SizeOf(TApiFile), 0);

  Af.FileName := Fname;
  Af.IsOpened := False;

  { Fill In Unicode Name }
  for I := 1 to Length(Fname) do
    Af.FName.Unicode[I - 1] := Byte(Fname[I]);

  Af.FName.Length := Length(Fname);

  { Create FSRef }
  OutStatus := FSMakeFSSpec(0, 0, Fname, FileSpecs);

  if OutStatus <> NoErr then
    Exit;

  OutStatus := FSpMakeFSRef(FileSpecs, Af.FFSRef);

  if OutStatus <> NoErr then
    Exit;

  { Open Fork }
  FSGetDataForkName(Af.FFork);

  OssError := FSOpenFork(Af.FFSRef, Af.FFork.Length, Af.FFork.Unicode[0],
    FsRdPerm, Af.FRef);

  if OssError = NoErr then
  begin
    Af.IsOpened := True;

    FSGetForkSize(Af.FRef, Af.FSize);

    Af.FRead := 0;
  end;

  Result := Af.IsOpened;
end;

function ApiReadFile;
var
  OssError: OSStatus;
  ForkLoad: ByteCount;

begin
  Result := False;
  read := 0;

  if Af.IsOpened then
  begin
    if Aloc > Af.FSize - Af.FRead then
      Aloc := Af.FSize - Af.FRead;

    OssError := FSReadFork(Af.FRef, FsAtMark + NoCacheMask, Af.FRead, Aloc,
      Buff, ForkLoad);

    if OssError = NoErr then
    begin
      read := ForkLoad;

      Inc(Af.FRead, read);

      Result := True;
    end;
  end;
end;

function ApiCloseFile;
begin
  Result := False;

  if Af.IsOpened then
  begin
    FSCloseFork(Af.FRef);

    Af.IsOpened := False;

    Result := True;
  end;
end;

function Param_Count;
begin
  Result := 0;
end;

function Param_str;
begin
  Result := '';
end;

end.
