unit Lucidity.ProgramFileUtils;

interface

uses
  Types, Classes;

function GetProgramFileFormatVersion(const ProgramFileName : string):integer;




// Takes a sample file path and a program file name and attempts to figure out the absolute sample path.
// The sample filename may have been saved as an absolute path, a relative path or no path at all
// depending on how the program file was saved and where the samples files were in relation to
// the program file.
function FindAbsoluteSamplePath(const ProgramFileName : string; const SampleFileName : string; out AbsoluteSamplePath : string):boolean;

// takes a sample file name and makes it relative to the program file.
function MakeRelativeSamplePath(const ProgramFileName : string; const SampleFileName : string):string;


// rename a sample file used in an existing program file. Use with caution. The target sample file
// may be used in multiple program files.
procedure RenameUsedSampleFile(const ProgramFileName, OldSampleFileName, NewSampleFileName : string);


// rename all sample files used in a program file. Use with caution. The target sample files
// may be used in multiple program files.
procedure RenameAllUsedSampleFiles(const ProgramFileName, NewSampleFileNameRoot : string);

// get all the sample file names referenced in the program file. Sample files may or may not
// exist on disk.
procedure GetSampleFileNameReferences(const ProgramFileName : string; var SampleFileNames : TStringList);


// RenameProgramFileOnly() renames the program file but doesn't change the name of the sample directory.
// This is useful if the program file of an existing drum kit needs to change without breaking
// user song projects that may be using the sample file paths in saved projects.
procedure RenameProgramFileOnly(const NewProgramFileName, OldProgramFileName : string);

// After using the RenameProgramFileOnly() procedure, the program file will contain
// the old samples directory in XMLDoc/Root/AlternateSampleDirectory
// GetAlternateSamplesDirectory() reads that node value. It returns a full path
// string if the directory exists on disk, else it ignores the <AlternateSampleDirectory>
// node value and returns an empty string.
function GetAlternateSamplesDirectory(const ProgramFileName : string):string;

function ConvertProgramFileNameToSampleDirName(const ProgramFileName : string):string;

function RenameProgramFile(const CurrentProgramFileName, NewProgramFileName : string):boolean;

implementation

uses
  StrUtils,
  SysUtils,
  VamLib.Utils,
  NativeXML, NativeXMLEx;

function GetProgramFileFormatVersion(const ProgramFileName : string):integer;
var
  xml : TNativeXML;
  RootNode : TXMLNode;
  aNode : TXmlNode;
  PatchFormatVersion : integer;
begin
  xml := TNativeXML.Create(nil);
  autoFree(@xml);
  xml.LoadFromFile(ProgramFileName);

  RootNode := xml.Root;
  assert(assigned(RootNode));

  aNode := RootNode.FindNode('PatchFileFormatVersion');
  if assigned(aNode)
    then PatchFormatVersion := DataIO_StrToInt(aNode.ValueUnicode, -1)
    else PatchFormatVersion := -1;

 result := PatchFormatVersion;
end;

procedure GetSampleFileNameReferences(const ProgramFileName : string; var SampleFileNames : TStringList);
var
  xml : TNativeXML;
  RootNode : TXMLNode;
  aNode : TXmlNode;
  PatchFormatVersion : integer;
  nd : TXmlNode;
  Nodes : TList;
  c1: Integer;
  p : Pointer;
  fn : string;
begin
  xml := TNativeXML.Create(nil);
  autoFree(@xml);
  xml.LoadFromFile(ProgramFileName);

  Nodes := TList.Create;
  AutoFree(@Nodes);

  RootNode := xml.Root;
  assert(assigned(RootNode));

  FindNodes(RootNode, 'SampleFileName', Nodes);

  for c1 := 0 to Nodes.Count-1 do
  begin
    nd := TXmlNode(Nodes.Items[c1]);
    fn := nd.ValueUnicode;
    if SampleFileNames.IndexOf(fn) = -1
      then SampleFileNames.Add(fn);
  end;
end;

procedure RenameUsedSampleFile(const ProgramFileName, OldSampleFileName, NewSampleFileName : string);
var
  xml : TNativeXML;
  RootNode : TXMLNode;
  aNode : TXmlNode;
  PatchFormatVersion : integer;
  nd : TXmlNode;
  Nodes : TList;
  c1: Integer;
  p : Pointer;
  fn : string;
  NewFN : string;
  OldAbsFileName : string;
  NewAbsFileName : string;
  IsFileRenameSuccess : boolean;
begin
  xml := TNativeXML.Create(nil);
  autoFree(@xml);
  xml.LoadFromFile(ProgramFileName);

  Nodes := TList.Create;
  AutoFree(@Nodes);

  RootNode := xml.Root;
  assert(assigned(RootNode));

  FindNodes(RootNode, 'SampleFileName', Nodes);

  IsFileRenameSuccess := false;

  for c1 := 0 to Nodes.Count-1 do
  begin
    nd := TXmlNode(Nodes.Items[c1]);
    fn := nd.ValueUnicode;
    if EndsText(OldSampleFileName, fn) then
    begin
      if (FindAbsoluteSamplePath(ProgramFileName, fn, OldAbsFileName)) and (not IsFileRenameSuccess) then
      begin
        assert(FileExists(OldAbsFileName));
        NewAbsFileName := StringReplace(OldAbsFileName, OldSampleFileName, NewSampleFileName, [rfReplaceAll, rfIgnoreCase]);
        if (not FileExists(NewAbsFilename)) then
        begin
          IsFileRenameSuccess := RenameFile(OldAbsFileName, NewAbsFileName);
        end;
      end;

      if IsFileRenameSuccess then
      begin
        NewFN := StringReplace(fn, OldSampleFileName, NewSampleFileName, [rfReplaceAll, rfIgnoreCase]);
        nd.ValueUnicode := NewFN;
      end;
    end;
  end;

  xml.XmlFormat := xfReadable;
  xml.SaveToFile(ProgramFileName);
end;


function FindAbsoluteSamplePath(const ProgramFileName : string; const SampleFileName : string; out AbsoluteSamplePath : string):boolean;
var
  fn : string;
  path : string;
begin

  // Check if the sample file name is an absolute file name and exists on disk already.
  if FileExists(SampleFileName) then
  begin
    AbsoluteSamplePath := SampleFileName;
    exit(true); //===============================>> exit >>====================>>
  end;


  // check in a child 'samples' directory.
  path := ExtractFilePath(ProgramFileName);
  path := IncludeTrailingPathDelimiter(path) + RemoveFileExt(ProgramFileName) + ' Samples';
  path := IncludeTrailingPathDelimiter(path);
  fn   := ExtractFileName(SampleFileName);
  if FileExists(path + fn) then
  begin
    AbsoluteSamplePath := (path + fn);
    exit(true); //===============================>> exit >>====================>>
  end;



  // check if the sample is relative to the program file.
  path := ExtractFilePath(ProgramFileName);
  fn   := IncludeTrailingPathDelimiter(path) + SampleFileName;
  fn   := ExpandFileName(fn);
  if FileExists(fn) then
  begin
    AbsoluteSamplePath := fn;
    exit(true); //===============================>> exit >>====================>>
  end;


  // Check if the drum kit is using an alternately named samples directory.
  Path := GetAlternateSamplesDirectory(ProgramFileName);
  fn := IncludeTrailingPathDelimiter(path) + SampleFileName;
  if FileExists(fn) then
  begin
    AbsoluteSamplePath := fn;
    exit(true); //===============================>> exit >>====================>>
  end;


  // the file has not been found.
  AbsoluteSamplePath := '';
  exit(false); //===============================>> exit >>====================>>
end;


function MakeRelativeSamplePath(const ProgramFileName : string; const SampleFileName : string):string;
var
  fn : string;
  ProgramPath : string;
  SamplesDir : string;
begin
  ProgramPath := ExtractFilePath(ProgramFileName);
  SamplesDir  := IncludeTrailingPathDelimiter(ProgramPath) + RemoveFileExt(ProgramFileName) + ' Samples';
  SamplesDir  := IncludeTrailingPathDelimiter(SamplesDir); // IMPORTANT: include the trailing path delimiter so that it is stripped away from the filename later.

  fn := SampleFileName;

  if StartsText(SamplesDir, fn)
      then fn := ReplaceText(fn, SamplesDir, '')
      else fn := ExtractRelativePath(ProgramPath, fn);

  result := fn;
end;


procedure RenameAllUsedSampleFiles(const ProgramFileName, NewSampleFileNameRoot : string);
var
  c1 : integer;
  SampleFileNames : TStringList;
  OldFileName : string;
  NewFileName : string;
begin
  SampleFileNames := TStringList.Create;
  AutoFree(@SampleFileNames);

  GetSampleFileNameReferences(ProgramFileName, SampleFileNames);

  for c1 := 0 to SampleFileNames.Count-1 do
  begin
    OldFileName := SampleFileNames[c1];
    NewFileName := NewSampleFileNameRoot + ' ' + IntToStrB(c1 + 1, 2) + ExtractFileExt(OldFileName);
    RenameUsedSampleFile(ProgramFileName, OldFileName, NewFileName);
  end;

end;

procedure RenameProgramFileOnly(const NewProgramFileName, OldProgramFileName : string);
var
  xml : TNativeXML;
  RootNode : TXMLNode;
  ProgramPath : string;
  SamplesDir : string;
begin
  xml := TNativeXML.Create(nil);
  autoFree(@xml);
  xml.LoadFromFile(OldProgramFileName);

  RootNode := xml.Root;
  if not assigned(RootNode) then raise Exception.Create('Root node doesn''t exist in file.');

  if NodeWiz(RootNode).Exists('AlternateSampleDirectory') = false then
  begin
    ProgramPath := ExtractFilePath(OldProgramFileName);
    SamplesDir  := IncludeTrailingPathDelimiter(ProgramPath) + RemoveFileExt(OldProgramFileName) + ' Samples';

    if DirectoryExists(SamplesDir) then
    begin
      SamplesDir := ExtractFileName(SamplesDir);
      NodeWiz(RootNode).CreateNode('AlternateSampleDirectory').ValueUnicode := SamplesDir;
    end;
  end;

  xml.XmlFormat := xfReadable;
  xml.SaveToFile(OldProgramFileName);

  RenameFile(OldProgramFilename, NewProgramFileName);
end;

function GetAlternateSamplesDirectory(const ProgramFileName : string):string;
var
  xml : TNativeXML;
  RootNode : TXMLNode;
  ProgramPath : string;
  SamplesDir : string;
begin
  xml := TNativeXML.Create(nil);
  autoFree(@xml);
  xml.LoadFromFile(ProgramFileName);

  RootNode := xml.Root;
  if not assigned(RootNode) then raise Exception.Create('Root node doesn''t exist in file.');

  if NodeWiz(RootNode).Exists('AlternateSampleDirectory') then
  begin
    ProgramPath := IncludeTrailingPathDelimiter(ExtractFilePath(ProgramFileName));
    SamplesDir := NodeWiz(RootNode).Child('AlternateSampleDirectory').ValueUnicode;
    SamplesDir := ProgramPath + IncludeTrailingPathDelimiter(SamplesDir);
    result := SamplesDir;
  end else
  begin
    result := '';
  end;
end;


function ConvertProgramFileNameToSampleDirName(const ProgramFileName : string):string;
var
  fn : string;
  dir : string;
begin
  fn := TrimFileExt(ProgramFileName);
  dir := ExtractFilePath(ProgramFileName);
  dir := IncludeTrailingPathDelimiter(dir) + IncludeTrailingPathDelimiter(fn + ' Samples');
  result := dir;
end;

function RenameProgramFile(const CurrentProgramFileName, NewProgramFileName : string):boolean;
var
  OldDirName : string;
  NewDirName : string;
  r1, r2 : boolean;
begin
  OldDirName := ConvertProgramFileNameToSampleDirName(CurrentProgramFileName);
  NewDirName := ConvertProgramFileNameToSampleDirName(NewProgramFileName);

  if (FileExists(CurrentProgramFileName)) and (not FileExists(NewProgramFileName)) and (not DirectoryExists(NewDirName)) then
  begin
    r1 := RenameFile(CurrentProgramFileName, NewProgramFileName);
    if (DirectoryExists(OldDirName)) then
    begin
      r2 := RenameFile(ExcludeTrailingPathDelimiter(OldDirName), ExcludeTrailingPathDelimiter(NewDirName));
    end;
  end;

  if (r1 = true) and (r2 = true)
    then result := true
    else result := false;
end;


end.
