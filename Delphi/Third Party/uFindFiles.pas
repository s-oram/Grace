unit uFindFiles;

interface

uses SysUtils, Classes;

type
  TSearchPathChanged = procedure(Sender:TObject; aSearchPath:string) of object;



procedure FindOnlyFiles(Dir:string; Results:TStringList; LimitType:string = '*.*');
procedure FindOnlyFolders(Dir:string; Results:TStringList);

procedure FindOnlyFilesRecursive(Dir:string; Results : TStringList; LimitType:string = '*.*');

procedure GetDirContents(Dir:string; Results:TStringList; FileTypes:TStringList);

function AbFindPrevFile(FileName:string; LimitType:string = '*.*'):string;
function AbFindNextFile(FileName:string; LimitType:string = '*.*'):string;

function AbFindFile(Dir, FileName:string; SearchSubfolders:boolean; out FullPath:string; OnChangeSearchPath:TSearchPathChanged = nil):boolean;

implementation

procedure FindOnlyFiles(Dir:string; Results:TStringList; LimitType:string = '*.*');
var
  sr:TSearchRec;
begin
  Dir := IncludeTrailingPathDelimiter(Dir) + LimitType;

  if FindFirst(Dir, 0,sr) = 0 then
  begin
    repeat Results.Add(sr.Name);
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

procedure FindOnlyFolders(Dir:string; Results:TStringList);
var
  sr:TSearchRec;
  Path:string;
begin
  Path := IncludeTrailingPathDelimiter(Dir) + '*.*';
  //find all the folders first.
  if FindFirst(Path, faDirectory,sr) = 0 then
  begin
    repeat
    begin
      if (sr.Attr and faDirectory <> 0)
        and (sr.Attr and faHidden = 0)
        and (sr.Name <> '.')
        and (sr.Name <> '..')
        then Results.Add(sr.Name);

    end
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;

procedure FindOnlyFilesRecursive(Dir:string; Results : TStringList; LimitType:string = '*.*');
var
  DirList : TStringList;
  CurDir  : string;
  s       : string;
  sr:TSearchRec;

begin
  DirList := TStringList.Create;

  try
    Dir := IncludeTrailingPathDelimiter(Dir);
    DirList.Add(Dir);
    while DirList.Count > 0 do
    begin
      CurDir := DirList[0];
      DirList.Delete(0);

      //Find the files...
      if FindFirst(CurDir + LimitType, 0,sr) = 0 then
      begin
        repeat Results.Add(CurDir + sr.Name);
        until FindNext(sr) <> 0;
        FindClose(sr);
      end;

      //Find the child directories...
      if FindFirst(CurDir + '*.*', faDirectory, sr) = 0 then
      begin
        repeat
        begin
          if (sr.Attr and faDirectory <> 0)
            and (sr.Attr and faHidden = 0)
            and (sr.Name <> '.')
            and (sr.Name <> '..')
            then
            begin
              s := CurDir + sr.Name;
              s := IncludeTrailingPathDelimiter(s);
              DirList.Add(s);
            end;
        end
        until FindNext(sr) <> 0;
        FindClose(sr);
      end;

    end;
  finally
    DirList.Free;
  end;
end;

{
  GetDircontents is an Improvement of FindOnlyFiles.
  It fills the Reults stringlist with a list of folders, followed
  by any files matching those found in the FileTypes string list.
  To find all files only use of FileType string, "*.*".
}
procedure GetDirContents(Dir:string; Results:TStringList; FileTypes:TStringList);
var
  c1:integer;
  sr:TSearchRec;
  Path:string;
begin
  assert(FileTypes.Count > 0, 'No fileypes specified.');

  Path := IncludeTrailingPathDelimiter(Dir) + '*.*';

  //find all the folders first.
  if FindFirst(Path, faDirectory,sr) = 0 then
  begin
    repeat
    begin
      if (sr.Attr and faDirectory = 0)
        and (sr.Attr and faHidden = 0)
        and (sr.Name <> '.')
        and (sr.Name <> '..')
        then Results.Add(sr.Name);
    end
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;

  //then find the files.
  for c1 := 0 to FileTypes.Count-1 do
  begin
    Path := IncludeTrailingPathDelimiter(Dir) + FileTypes.Strings[c1];
    if FindFirst(Path, 0,sr) = 0 then
    begin
      repeat
      begin
        if (sr.Attr and faHidden <> sr.Attr) and (sr.Attr and faSysFile <> sr.Attr)
          then Results.Add(sr.Name);
      end
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
  end;
end;



function AbFindPrevFile(FileName:string; LimitType:string = '*.*'):string;
// FileName must be full path.
// - Will return the input fileName if no previous file name is found.
//   That is odd., but maybe too late to change this behaviour.
var
  Path,Fn,LastFile:string;
  c1:integer;
  sr:TSearchRec;
begin
  Path := ExtractFilePath(FileName);
  Fn := ExtractFileName(FileName);
  LastFile := fn;

  Result := Filename;

  if FindFirst(Path + LimitType, faAnyFile,sr) = 0 then
  begin
    if sr.Name = Fn then result := FileName
    else
    begin
      repeat
      begin
        LastFile := sr.Name;
        c1 := FindNext(sr);
      end
      until (c1 <> 0) or (sr.Name = fn);

      if c1 = 0
        then Result := Path + LastFile
        else Result := Filename;
    end;
  end;
  FindClose(sr);
end;


function AbFindNextFile(FileName:string; LimitType:string = '*.*'):string;
// FileName must be full path.
// - Will return the input fileName if no next file name is found.
//   That is odd.. but maybe too late to change this behaviour.
var
  Path,Fn:string;
  c1:integer;
  sr:TSearchRec;
begin
  Path := ExtractFilePath(FileName);
  Fn := ExtractFileName(FileName);

  Result := Filename;

  if FindFirst(Path + LimitType, faAnyFile,sr) = 0 then
  begin

    if sr.Name <> Fn then
      repeat c1 := FindNext(sr);
      until (c1 <> 0) or (sr.Name = fn);

    if (sr.Name <> Fn) or (FindNext(sr) <> 0)
      then Result := Filename
      else Result := Path + Sr.Name;
  end;
  FindClose(sr);

end;

//Use AbFindFile to find the location of a particular file.
function AbFindFile(Dir, FileName:string; SearchSubfolders:boolean; out FullPath:string; OnChangeSearchPath:TSearchPathChanged = nil):boolean;
var
  sr:TSearchRec;
  c1:integer;
  LocalDir:string;
begin
  result := false;

  // Call the OnChangeSearchPath event. (Useful for dialogs and stuff to show the search is active).
  if assigned(OnChangeSearchPath) then OnChangeSearchPath(nil, Dir);                                


  Dir := IncludeTrailingPathDelimiter(Dir);

  //Attempt to find the file in the given directory.
  if FindFirst(Dir + Filename, faAnyFile, sr) = 0 then
  begin
    if sr.Name <> FileName then
      repeat c1 := FindNext(sr);
      until (c1 <> 0) or (sr.Name = FileName);

    if sr.Name = FileName then
    begin
      FullPath := Dir + sr.Name;
      result := true;
    end;
  end;
  FindClose(sr);

  //If the file isn't found, do we need to search subfolders?
  if (result = false) and (SearchSubfolders) then
  begin
    //Seach for folders. For some reason this just picks up all files. not just directories... ???  :(
    c1 := FindFirst(Dir  + '*.*', faDirectory, sr);
    while c1 = 0 do
    begin

      //If it is a valid folder, search that folder for the same file. This method gets recursive here.
      if ((sr.Attr and faDirectory) = faDirectory) and (sr.Name <> '.') and (sr.Name <> '..') then
      begin
        LocalDir := IncludeTrailingPathDelimiter(Dir) + sr.Name;
        result := AbFindFile(LocalDir, Filename, true, FullPath, OnChangeSearchPath);
      end;

      //If the file was found, we can finish looking, else continue search subfolders.
      if Result = true
        then c1 := -1
        else c1 := FindNext(sr);

    end;
    FindClose(sr);
  end;


end;




end.
