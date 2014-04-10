unit Lucidity.StateHelpers;

interface

uses
  Lucidity.Interfaces,
  Lucidity.SampleMap,
  NativeXml,
  NativeXmlEx;

procedure SaveSamplesToDisk(const ProgramFilename : string; const SampleMap : TSampleMap);


procedure MakeSampleFileNamesRelative(const RootNode : TXmlNode; const ProgramFileName : string);
procedure MakeSampleFileNamesAbsolute(const RootNode : TXmlNode; const ProgramFileName : string);



implementation

uses
  uAutoFree,
  eeFunctions,
  StrUtils,
  SysUtils;

procedure SaveSamplesToDisk(const ProgramFilename : string; const SampleMap : TSampleMap);
var
  xSampleDir : string;
  c1: Integer;
  Region : IRegion;
  SourceAudioFile, DestAudioFile : string;
begin
  xSampleDir := ExtractFileDir(ProgramFileName);

  if DirectoryExists(xSampleDir) = false
    then raise Exception.Create('Program directory doesn''t exist. Can not save samples.');

  xSampleDir := IncludeTrailingPathDelimiter(xSampleDir);

  xSampleDir := xSampleDir + RemoveFileExt(ProgramFileName) + ' Samples';

  xSampleDir := IncludeTrailingPathDelimiter(xSampleDir);

  if DirectoryExists(xSampleDir) = false then
  begin
    if CreateDir(xSampleDir) = false
      then raise Exception.Create('Error: Cannot create sample directory.');
  end;


  for c1 := 0 to SampleMap.RegionCount-1 do
  begin
    Region := SampleMap.Regions[c1];

    SourceAudioFile := Region.GetProperties^.SampleFileName;

    DestAudioFile := IncludeTrailingPathDelimiter(xSampleDir) + ExtractFileName(SourceAudioFile);

    if (FileExists(SourceAudioFile)) and (SourceAudioFile <> DestAudioFile) then
    begin
      if CopyFile(SourceAudioFile, DestAudioFile) then
      begin
        // File copied. Update region with new sample data.
        Region.GetProperties^.SampleFileName := DestAudioFile;
      end else
      begin
        // TODO: Unable to copy file.. do something about it...
      end;
    end;
  end;


  // TODO: This method has only been sketched out. It needs to handle
  // what happens when the samples directory contains foiegn files.
  // Should they be deleted? possibly.

end;


procedure MakeSampleFileNamesRelative(const RootNode : TXmlNode; const ProgramFileName : string);
var
  NodeList : TsdNodeList;
  c1: Integer;
  fn : string;
  ProgramPath : string;
  SamplesDir : string;
begin
  NodeList := TsdNodeList.Create(false);
  AutoFree(@NodeList);

  ProgramPath := ExtractFilePath(ProgramFileName);
  SamplesDir  := IncludeTrailingPathDelimiter(ProgramPath) + RemoveFileExt(ProgramFileName) + ' Samples';
  SamplesDir  := IncludeTrailingPathDelimiter(SamplesDir); // IMPORTANT: include the trailing path delimiter so that it is stripped away from the filename later.

  FindNodes(RootNode,'/Region/SampleProperties/SampleFileName', NodeList);

  for c1 := 0 to NodeList.Count-1 do
  begin
    fn := NodeList[c1].ValueUnicode;

    if StartsText(SamplesDir, fn)
      then fn := ReplaceText(fn, SamplesDir, '')
      else fn := ExtractRelativePath(ProgramPath, fn);

    NodeList[c1].ValueUnicode := fn;
  end;
end;

procedure MakeSampleFileNamesAbsolute(const RootNode : TXmlNode; const ProgramFileName : string);
var
  NodeList : TsdNodeList;
  c1: Integer;
  fn : string;
  ProgramPath : string;
  SamplesDir : string;
begin
  NodeList := TsdNodeList.Create(false);
  AutoFree(@NodeList);

  ProgramPath := ExtractFilePath(ProgramFileName);
  SamplesDir  := IncludeTrailingPathDelimiter(ProgramPath) + RemoveFileExt(ProgramFileName) + ' Samples';

  FindNodes(RootNode,'/Region/SampleProperties/SampleFileName', NodeList);

  for c1 := 0 to NodeList.Count-1 do
  begin
    // First attempt: look for sample in "program samples" dir
    fn := NodeList[c1].ValueUnicode;
    if (FileExists(fn) = false) then
    begin
      fn := IncludeTrailingPathDelimiter(SamplesDir) + ExtractFileName(fn);
    end;

    // second attempt: if the file doesn't exists, check if it's relative to the program directory.
    if (FileExists(fn) = false) and (IsRelativePath(NodeList[c1].ValueUnicode)) then
    begin
      fn := NodeList[c1].ValueUnicode;
      fn := IncludeTrailingPathDelimiter(ProgramPath) + fn;
      fn := ExpandFileName(fn);
    end;

    // If the file exists, update the node with the absolute filename.
    if FileExists(fn) then
    begin
      NodeList[c1].ValueUnicode := fn;
    end;
  end;
end;

end.
