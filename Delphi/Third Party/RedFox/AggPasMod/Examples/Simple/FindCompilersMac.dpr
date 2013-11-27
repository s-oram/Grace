program FindCompilersMac;

{ target:mac }
{ mac_console_app }
//
// AggPas 2.4 RM3 Helper utility application
// Milan Marusinec alias Milano (c) 2006 - 2008
//

uses
  SysUtils,
  AggBasics,
  File_utils_,
  Carbon;

{$I AggCompiler.inc}

type
  TSourceKey = record
    Key, Val: string[99];
  end;

const
  CKeyMax = 99;
  CPoolMax = 65536;
  CMakeMax = 99;

  CFpcComp = '/usr/local/bin/ppcppc';
  CFpcLibs =
    '-Fu"src;src/ctrl;src/platform/mac;src/util;src/svg;upi;expat-wrap"';
  CFpcIncl = '-Fisrc';
  CFpcOutd = '-FU_debug';
  CFpcFramework = '-k"-framework Carbon -framework QuickTime"';
  CFpcConf = '-Mdelphi -Tdarwin -Sg -Se3 -XX -Xs -B -v0i';
  CFpcCApp = '-WC';
  CFpcGApp = '-WG';

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

function WrFile(DestDir: FSRefPtr; Name: ShortString): Boolean;
var
  I: Cardinal;

  OssState          : OSStatus;
  OssError          : OSErr;
  ForkName, DestName: HFSUniStr255;
  DstFSRef          : FSRef;
  ForkRef           : SInt16;
  Written           : ByteCount;
  CatInfo           : FSCatalogInfo;
begin
  Result := False;

  // Fill in Unicode name
  for I := 1 to Length(name) do
    DestName.Unicode[I - 1] := Byte(name[I]);

  DestName.Length := Length(name);

  // Write the script to file
  OssError := FSCreateFileUnicode(DestDir^, DestName.Length,
    DestName.Unicode[0], KFSCatInfoNone, nil, @DstFSRef, nil);

  if OssError = NoErr then
  begin
    FSGetDataForkName(ForkName);

    OssError := FSOpenFork(DstFSRef, ForkName.Length, ForkName.Unicode[0],
      FsWrPerm, ForkRef);

    if OssError = NoErr then
    begin
      OssError := FSWriteFork(ForkRef, FsFromStart + NoCacheBit, 0, GPoolSize,
        GPoolBuff, Written);

      FSCloseFork(ForkRef);

      if (OssError = NoErr) and (GPoolSize = Written) then

      else
        Exit;
    end
    else
    begin
      Write('[FSOpenFork:', OssError, '] ');
      Exit;
    end;
  end
  else if OssError = DupFNErr then
  else
  begin
    Write('[FSCreateFileUnicode:', OssError, '] ');
    Exit;
  end;

  // Set The File permissions
  CatInfo.Permissions[0] := 0;
  CatInfo.Permissions[1] := 0;
  CatInfo.Permissions[2] := 0;
  CatInfo.Permissions[3] := 0;

  FSPermissionInfoPtr(@CatInfo.Permissions).Mode := 999;

  OssError := FSSetCatalogInfo(DstFSRef, KFSCatInfoPermissions, CatInfo);

  // OK
  Result := True;
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

procedure LoadKeys(Buff: PAnsiChar; Size: SInt64);
type
  E_scan = (Expect_lp, Load_key, Load_val, Next_ln, Expect_crlf);

var
  Scan    : E_scan;
  Key, Val: ShortString;

  procedure Add_key;
  begin
    if GKeyCount < CKeyMax then
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

function WriteCompileScript(DestDir: FSRefPtr; Name, Ext: ShortString): Boolean;
var
  Cp, Fp, Fn, Fx: ShortString;
begin
  Result := False;

  // Create the script in memory
  GPoolSize := 0;

  WrPool(CFpcComp + ' ');
  WrPool(CFpcLibs + ' ');
  WrPool(CFpcIncl + ' ');
  WrPool(CFpcOutd + ' ');
  WrPool(CFpcFramework + ' ');
  WrPool(CFpcConf + ' ');

  if FirstKey('mac_console_app', Cp) then
    WrPool(CFpcCApp + ' ')
  else
    WrPool(CFpcGApp + ' ');

  WrPool(name + Ext, True);

  if not FirstKey('mac_console_app', Cp) then
  begin
    WrPool('mkdir -p ' + name + '.app/Contents/MacOS', True);
    WrPool('mv -f ' + name + ' ' + name + '.app/Contents/MacOS/' + name, True);
  end;

  if FirstKey('mac_copy', Cp) then
    repeat
      SpreadName(Cp, Fp, Fn, Fx);

      if Cmp_str(Fx) = Cmp_str('.bmp') then
        WrPool('cp -f bmp/' + Cp + ' ' + name + '.app/Contents/MacOS/'
          + Cp, True)
      else if Cmp_str(Fx) = Cmp_str('.svg') then
        WrPool('cp -f svg/' + Cp + ' ' + name + '.app/Contents/MacOS/' +
          Cp, True);
    until not NextKey(Cp);

  // WriteFile
  name := 'compile-' + name;

  if WrFile(DestDir, name) then
  begin
    if GMakeCount < CMakeMax then
    begin
      GMakeArray[GMakeCount] := name;

      Inc(GMakeCount);
    end;

    Result := True;
  end;
end;

procedure CreateCompileScript(DestDir: FSRefPtr; Name, Ext: ShortString;
  InRef: FSRefPtr);
var
  Loaded: Boolean;

  OssError: OSStatus;
  ForkName: HFSUniStr255;
  ForkSize: SInt64;
  ForkRef : SInt16;
  ForkBuff: Pointer;
  ForkLoad: ByteCount;

  Target, Value: ShortString;
begin
  Write(' ', name, Ext, ' ... ');

  // Open Source .DPR file
  FSGetDataForkName(ForkName);

  OssError := FSOpenFork(InRef^, ForkName.Length, ForkName.Unicode[0],
    FsRdPerm, ForkRef);

  if OssError = NoErr then
  begin
    Loaded := False;

    // Load DPR keys
    FSGetForkSize(ForkRef, ForkSize);

    if (ForkSize > 0) and AggGetMem(ForkBuff, ForkSize) then
    begin
      OssError := FSReadFork(ForkRef, FsAtMark + NoCacheMask, 0, ForkSize,
        ForkBuff, ForkLoad);

      if (OssError = NoErr) and (ForkSize = ForkLoad) then
      begin
        Loaded := True;

        LoadKeys(ForkBuff, ForkSize);
      end;

      AggFreeMem(ForkBuff, ForkSize);
    end;

    // Close DPR
    FSCloseFork(ForkRef);

    // Create compilation script
    if Loaded then
    begin
      if FirstKey('skip', Value) then
        Writeln('to be not included -> skipped')
      else
      begin
        Target := 'mac';

        FirstKey('target', Target);

        if Cmp_str(Target) = Cmp_str('mac') then
          if WriteCompileScript(DestDir, name, Ext) then
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

procedure ProcessObject(DestDir: FSRefPtr; InCatInfo: FSCatalogInfoPtr;
  InRef: FSRefPtr; InSpec: FSSpecPtr);
var
  FilePath, FileName, FileExt: ShortString;
begin
  if InCatInfo.NodeFlags and KFSNodeIsDirectoryMask = KFSNodeIsDirectoryMask
  then
  else
  begin
    SpreadName(InSpec.Name, FilePath, FileName, FileExt);

    if Cmp_str(FileExt) = Cmp_str('.dpr') then
      CreateCompileScript(DestDir, FileName, FileExt, InRef);
  end;
end;

function IterateFolder(var InFolder: FSRef): OSStatus;
var
  KRequestCountPerIteration: Size_t;

  OutStatus: OSStatus;

  KCatalogInfoBitmap: FSCatalogInfoBitmap;

  Iterator: FSIterator;

  CatalogInfoArray: FSCatalogInfoPtr;

  FSRefArray : FSRefPtr;
  FSSpecArray: FSSpecPtr;

  ActualCount: ItemCount;

  Index: UInt32;

  Changed: Boolean;
begin
  KRequestCountPerIteration := ((4096 * 4) div SizeOf(FSCatalogInfo));

  // Get permissions and node flags and Finder info
  //
  // For maximum performance, specify in the catalog
  // bitmap only the information you need to know
  KCatalogInfoBitmap := KFSCatInfoNodeFlags or KFSCatInfoFinderInfo;

  // On each iteration of the do-while loop, retrieve this
  // number of catalog infos
  //
  // We use the number of FSCatalogInfos that will fit in
  // exactly four VM pages (#113). This is a good balance
  // between the iteration I/O overhead and the risk of
  // incurring additional I/O from additional memory
  // allocation

  // Create an iterator
  OutStatus := FSOpenIterator(InFolder, KFSIterateFlat, Iterator);

  if OutStatus = NoErr then
  begin
    // Allocate storage for the returned information
    AggGetMem(Pointer(CatalogInfoArray), SizeOf(FSCatalogInfo) *
      KRequestCountPerIteration);
    AggGetMem(Pointer(FSRefArray), SizeOf(FSRef) * KRequestCountPerIteration);
    AggGetMem(Pointer(FSSpecArray), SizeOf(FSSpec) *
      KRequestCountPerIteration);

    if CatalogInfoArray = nil then
      OutStatus := MemFullErr
    else
    begin
      // Request information about files in the given directory,
      // until we get a status code back from the File Manager
      repeat
        Changed := False;

        OutStatus := FSGetCatalogInfoBulk(Iterator, KRequestCountPerIteration,
          ActualCount, Changed, KCatalogInfoBitmap, CatalogInfoArray,
          FSRefArray, FSSpecArray, nil);

        // Process all items received
        if (OutStatus = NoErr) or (OutStatus = ErrFSNoMoreItems) then
          for index := 0 to ActualCount - 1 do
            ProcessObject(@InFolder, FSCatalogInfoPtr(PtrComp(CatalogInfoArray)
              + index * SizeOf(FSCatalogInfo)),
              FSRefPtr(PtrComp(FSRefarray) + index * SizeOf(FSRef)),
              FSSpecPtr(PtrComp(FSSpecArray) + index * SizeOf(FSSpec)));
      until OutStatus <> NoErr;

      // errFSNoMoreItems tells us we have successfully processed all
      // items in the directory -- not really an error
      if OutStatus = ErrFSNoMoreItems then
        OutStatus := NoErr;

      // Free the array memory
      AggFreeMem(Pointer(CatalogInfoArray), SizeOf(FSCatalogInfo) *
        KRequestCountPerIteration);
      AggFreeMem(Pointer(FSRefArray),
        SizeOf(FSRef) * KRequestCountPerIteration);
      AggFreeMem(Pointer(FSSpecArray), SizeOf(FSSpec) *
        KRequestCountPerIteration);
    end;
  end;

  FSCloseIterator(Iterator);

  Result := OutStatus;
end;

procedure CreateMakeFile(DestDir: FSRefPtr);
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

  WrFile(DestDir, 'compile_make_all');
end;

procedure ScanDemos;
var
  OutStatus: OSStatus;
  FolderRef: FSRef;
  FileSpecs: FSSpec;
begin
  OutStatus := FSMakeFSSpec(0, 0, '', FileSpecs);

  if OutStatus = NoErr then
  begin
    OutStatus := FSpMakeFSRef(FileSpecs, FolderRef);

    if OutStatus = NoErr then
    begin
      OutStatus := IterateFolder(FolderRef);

      Writeln;

      if GMakeCount > 0 then
      begin
        CreateMakeFile(@FolderRef);

        Writeln('SUCCESS: FPC compilation script files were created');
        Writeln('         for the AggPas demos listed above.');
        Writeln;
        Writeln('         To compile the demos, run Terminal, change to the current');
        Writeln('         directory and type "./compile_make_all"');
        Writeln('         or "./compile-xxx", where "xxx" is the name of the demo.');
      end
      else
        Writeln('MESSAGE: No AggPas demo files were found in current folder !');
    end
    else
      Writeln('ERROR: Failed to create FSRef structure for the current folder !');
  end
  else
    Writeln('ERROR: Failed to search for files in the current folder !');

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
  Writeln('* properly all the AggPas demos on your Mac.                *');
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
