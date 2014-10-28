program FindCompilersLinux;

{ target:linux }
{ linux_console_app }
//
// AggPas 2.4 RM3 Helper utility application
// Milan Marusinec alias Milano (c) 2006 - 2008
//

uses
  SysUtils,
  AggBasics,
  AggFileUtils,
  Libc;

{$I AggCompiler.inc}
{$- }

type
  TSourceKey = record
    Key, Val: string[99];
  end;

const
  Cardinal(kcMax = 99;
  CPoolMax = 65536;
  CMakeMax = 99;

  FpcComp = 'ppc386';
  FpcLibs = '-Fu"src;src/ctrl;src/platform/linux;src/util;src/svg;expat-wrap"';
  FpcIncl = '-Fisrc';
  FpcOutd = '-FU_debug';
  FpcConf = '-Mdelphi -Tlinux -Sg -Se3 -XX -Xs -B -v0i';
  FpcGapp = '-WG';
  FpcCapp = '-WC';

var
  GKeyArray: array [0..CKeyMax - 1] of TSourceKey;
  GKeyCount: Cardinal;
  GKeyLastX: Cardinal;
  GKeyScanX: ShortString;

  GPoolBuff: Pointer;
  GPoolAloc: Cardinal;
  GPoolSize: Cardinal;

  GMakeArray: array [0..CMakeMax - 1] of string[99];
  GMakeCount: Cardinal;

procedure WrPool(Str: ShortString; Crlf: Boolean = False);
begin
  if Crlf then
    Str := Str + #10;

  if GPoolSize + Length(Str) < GPoolAloc then
  begin
    System.Move(Str[1], Pointer(PtrComp(GPoolBuff) + GPoolSize)^, Length(Str));

    Inc(GPoolSize, Length(Str));
  end;
end;

function WrFile(Fname: ShortString): Boolean;
var
  Df: file;
  Wr: Integer;

begin
  Result := False;

  AssignFile(Df, Fname);
  Rewrite(Df, 1);

  if IOResult = 0 then
  begin
    Blockwrite(Df, GPoolBuff^, GPoolSize, Wr);
    Close(Df);

    Fname := Fname + #0;

    Libc.Chmod(PAnsiChar(@Fname[1]), S_IRWXU or S_IRWXG or S_IROTH or S_IWOTH);

    if GPoolSize = Wr then
      Result := True;
  end;
end;

function NextKey(var Val: ShortString): Boolean;
begin
  Result := False;

  while GKeyLastX < GKeyCount do
  begin
    Inc(GKeyLastX);

    if Cmp_str(GKeyArray[GKeyLastX - 1].Key) = GKeyScanX then
    begin
      Val := GKeyArray[GKeyLastX - 1].Val;
      Result := True;

      Break;
    end;
  end;
end;

function FirstKey(Key: ShortString; var Val: ShortString): Boolean;
begin
  GKeyLastX := 0;
  GKeyScanX := Cmp_str(Key);

  Result := NextKey(Val);
end;

procedure LoadKeys(Buff: PAnsiChar; Size: Integer);
type
  E_scan = (Expect_lp, Load_key, Load_val, Next_ln, Expect_crlf);

var
  Scan    : E_scan;
  Key, Val: ShortString;

  procedure Add_key;
  begin
    if GKeyCount < Cardinal(kcMax then
    begin
      GKeyArray[GKeyCount].Key := Key;
      GKeyArray[GKeyCount].Val := Val;

      Inc(GKeyCount);
    end;

    Key := '';
    Val := '';
  end;

begin
  GKeyCount := 0;

  Scan := Expect_lp;
  Key := '';
  Val := '';

  while Size > 0 do
  begin
    case Scan of
      Expect_lp:
        case Buff^ of
          '{':
            Scan := Load_key;

        else
          Break;
        end;

      Load_key:
        case Buff^ of
          #13, #10:
            Break;

          ':':
            Scan := Load_val;

          '}':
            begin
              Add_key;

              Scan := Next_ln;
            end;

        else
          Key := Key + Buff^;
        end;

      Load_val:
        case Buff^ of
          #13, #10:
            Break;

          '}':
            begin
              Add_key;

              Scan := Next_ln;
            end;

        else
          Val := Val + Buff^;
        end;

      Next_ln:
        case Buff^ of
          #13, #10:
            Scan := Expect_crlf;

          ' ':
          else
            Break;
        end;

      Expect_crlf:
        case Buff^ of
          '{':
            Scan := Load_key;

          #13, #10:
          else
            Break;
        end;
    end;

    Dec(Size);
    Inc(PtrComp(Buff));
  end;
end;

function WriteCompileScript(Name, Ext: ShortString): Boolean;
var
  Cp: ShortString;

begin
  Result := False;

  // Create the script in memory
  GPoolSize := 0;

  WrPool(FpcComp + ' ');
  WrPool(FpcLibs + ' ');
  WrPool(FpcIncl + ' ');
  WrPool(FpcOutd + ' ');
  WrPool(FpcConf + ' ');

  if FirstKey('linux_console_app', Cp) then
    WrPool(FpcCapp + ' ')
  else
    WrPool(FpcGapp + ' ');

  WrPool(name + Ext, True);

  // WriteFile
  name := 'compile-' + name;

  if WrFile(name) then
  begin
    if GMakeCount < CMakeMax then
    begin
      GMakeArray[GMakeCount] := name;

      Inc(GMakeCount);
    end;

    Result := True;
  end;
end;

procedure CreateCompileScript(Name, Ext: ShortString);
var
  Loaded: Boolean;

  Target, Value: ShortString;

  Lf    : file;
  Fs, Ls: Integer;
  Bf    : Pointer;

begin
  write(' ', name, Ext, ' ... ');

  // Open Source .DPR file
  AssignFile(Lf, name + Ext);
  Reset(Lf, 1);

  if IOResult = 0 then
  begin
    Loaded := False;

    // Load DPR keys
    Fs := Filesize(Lf);

    if (Fs > 0) and AggGetMem(Bf, Fs) then
    begin
      Blockread(Lf, Bf^, Fs, Ls);

      if Fs = Ls then
      begin
        Loaded := True;

        LoadKeys(Bf, Fs);
      end;

      AggFreeMem(Bf, Fs);
    end;

    // Close DPR
    Close(Lf);

    // Create compilation script
    if Loaded then
    begin
      if FirstKey('skip', Value) then
        Writeln('to be not included -> skipped')
      else
      begin
        Target := 'linux';

        FirstKey('target', Target);

        if Cmp_str(Target) = Cmp_str('linux') then
          if WriteCompileScript(name, Ext) then
            Writeln('OK')
          else
            Writeln('Failed to generate compile script !')
        else
          Writeln('different target (', Target, ') -> skipped');
      end;

    end
    else
      Writeln('Failed to read the source file !');

  end
  else
    Writeln('Failed to open !');
end;

procedure ProcessObject(Found: ShortString);
var
  File_path, File_name, File_ext: ShortString;

begin
  SpreadName(Found, File_path, File_name, File_ext);

  if Cmp_str(File_ext) = Cmp_str('.dpr') then
    CreateCompileScript(File_name, File_ext);
end;

procedure IterateFolder(InFolder: ShortString);
var
  Dp: Libc.PDIR;
  Ep: Libc.Pdirent;

begin
  InFolder := InFolder + #0;

  Dp := Libc.Opendir(PAnsiChar(@InFolder[1]));

  if Dp <> nil then
  begin
    repeat
      Ep := Libc.Readdir(Dp);

      if Ep <> nil then
        ProcessObject(Strpas(Ep.D_name));

    until Ep = nil;

    Libc.Closedir(Dp);
  end;
end;

procedure CreateMakeFile;
var
  I: Cardinal;

begin
  GPoolSize := 0;

  I := 0;

  while I < GMakeCount do
  begin
    WrPool('./' + GMakeArray[I], True);

    Inc(I);
  end;

  WrFile('compile_make_all');
end;

procedure ScanDemos;
begin
  IterateFolder('./');
  Writeln;

  if GMakeCount > 0 then
  begin
    CreateMakeFile;

    Writeln('SUCCESS: FPC compilation script files were created');
    Writeln('         for the AggPas demos listed above.');
    Writeln;
    Writeln('         To compile the demos, run Terminal, change to the current');
    Writeln('         directory and type "./compile_make_all"');
    Writeln('         or "./compile-xxx", where "xxx" is the name of the demo.');

  end
  else
    Writeln('MESSAGE: No AggPas demo files were found in current folder !');

  Writeln;
end;

begin
  Writeln;
  Writeln('*************************************************************');
  Writeln('* Welcome to the AggPas 2.4 RM3 vector graphics library.    *');
  Writeln('*************************************************************');
  Writeln('*                                                           *');
  Writeln('* This helper utility will generate the compilation script  *');
  Writeln('* files with current paths and options needed to compile    *');
  Writeln('* properly all the AggPas demos on your Linux station.      *');
  Writeln('*                                                           *');
  Writeln('* Currently the Free Pascal compiler is supported.          *');
  Writeln('* (www.freepascal.org)                                      *');
  Writeln('*                                                           *');
  Writeln('*************************************************************');
  Writeln;
  Writeln('[Press ENTER key to continue ...]');
  Writeln;
  Readln;

  if AggGetMem(GPoolBuff, CPoolMax) then
  begin
    GPoolAloc := CPoolMax;
    GPoolSize := 0;
    GMakeCount := 0;

    ScanDemos;

    AggFreeMem(GPoolBuff, GPoolAloc);

  end
  else
    Writeln('ERROR: Not enough memory for the pool buffer !');
end.
