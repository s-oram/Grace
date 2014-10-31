unit uMainForm;

interface

uses
  ShellApi,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm4 = class(TForm)
    Edit1: TEdit;
    Memo1: TMemo;
    RenameAllSampleButton: TButton;
    RenameProgramFileOnlyButton: TButton;
    RenameProgramFileAndSampleDirButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RenameAllSampleButtonClick(Sender: TObject);
    procedure RenameProgramFileOnlyButtonClick(Sender: TObject);
    procedure RenameProgramFileAndSampleDirButtonClick(Sender: TObject);
  private
    CurrentProgramFile : string;

    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;

    procedure FilesDropped(Files : TStringList);

    procedure RefreshDetails;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

uses
  XPlat.Dialogs,
  Lucidity.ProgramFileUtils,
  Lucidity.Utils,
  VamLib.Utils;



{$R *.dfm}

procedure TForm4.FormCreate(Sender: TObject);
begin
  RefreshDetails;
  DragAcceptFiles(self.Handle, true);
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(self.Handle, false);
end;



procedure TForm4.WMDropFiles(var Msg: TWMDropFiles);
var
  Files : TStringList;
  DropH: HDROP;               // drop handle
  DroppedFileCount: Integer;  // number of files dropped
  FileNameLength: Integer;    // length of a dropped file name
  FileName: string;           // a dropped file name
  I: Integer;                 // loops thru all dropped files
  DropPoint: TPoint;          // point where files dropped
begin
  inherited;

  Files := TStringList.Create;
  AutoFree(@Files);

  // Store drop handle from the message
  DropH := Msg.Drop;
  try
    // Get count of files dropped
    DroppedFileCount := DragQueryFile(DropH, $FFFFFFFF, nil, 0);
    // Get name of each file dropped and process it
    for I := 0 to DroppedFileCount-1 do
    begin
      // get length of file name
      FileNameLength := DragQueryFile(DropH, I, nil, 0);
      // create string large enough to store file
      // (Delphi allows for #0 terminating character automatically)
      SetLength(FileName, FileNameLength);
      // get the file name
      DragQueryFile(DropH, I, PChar(FileName), FileNameLength + 1);
      // process file name (application specific)
      Files.Add(FileName);
    end;
    // Optional: Get point at which files were dropped
    DragQueryPoint(DropH, DropPoint);
    // ... do something with drop point here

    FilesDropped(Files);
  finally
    // Tidy up - release the drop handle
    // don't use DropH again after this
    DragFinish(DropH);
  end;
  // Note we handled message
  Msg.Result := 0;
end;

procedure TForm4.FilesDropped(Files: TStringList);
var
  s, fn : string;
begin
  fn := Files[0];
  if IsLucidityProgramFile(fn)
    then CurrentProgramFile := fn
    else CurrentProgramFile := '';

  RefreshDetails;
end;

procedure TForm4.RefreshDetails;
var
  s : string;
  c1 : integer;
  Samples : TStringList;
begin
  Samples := TStringList.Create;
  AutoFree(@Samples);

  if CurrentProgramFile = '' then
  begin
    Edit1.Text := CurrentProgramFile;
    Memo1.Clear;
  end;


  if CurrentProgramFile <> '' then
  begin
    Edit1.Text := CurrentProgramFile;
    Memo1.Clear;

    s := 'Lucidity Program Format = ' + IntToStr(GetProgramFileFormatVersion(CurrentProgramFile));
    Memo1.Lines.Add(s);

    s := '';
    Memo1.Lines.Add(s);

    s := 'Samples';
    Memo1.Lines.Add(s);
    s := '=======';
    Memo1.Lines.Add(s);

    GetSampleFileNameReferences(CurrentProgramFile, Samples);

    for c1 := 0 to Samples.Count-1 do
    begin
      Memo1.Lines.Add(Samples[c1]);
    end;
  end;
end;

procedure TForm4.RenameAllSampleButtonClick(Sender: TObject);
var
  ProgramFileName : string;
  NewFileName : string;
  FileNameRoot : string;
begin
  ProgramFileName := CurrentProgramFile;
  if FileExists(CurrentProgramFile) then
  begin
    FileNameRoot := InputBox('Sample Name Root', 'Root', '');
    if FileNameRoot <> ''
      then RenameAllUsedSampleFiles(ProgramFileName, FileNameRoot);
    RefreshDetails;
  end;
end;

procedure TForm4.RenameProgramFileOnlyButtonClick(Sender: TObject);
var
  NewProgramFileName : string;
  fn : string;
  Dir : string;
begin
  if FileExists(CurrentProgramFile) = false then exit;

  fn := RemoveFileExt(CurrentProgramFile);
  Dir := ExtractFilePath(CurrentProgramFile);

  NewProgramFileName := InputBox('Rename Program File Only', 'New Program File Name', fn);

  if NewProgramFileName <> fn then
  begin
    NewProgramFileName := IncludeTrailingPathDelimiter(Dir) + NewProgramFileName + '.lpg';
    RenameProgramFileOnly(NewProgramFileName, CurrentProgramFile);

    if FileExists(NewProgramFileName) then
    begin
      CurrentProgramFile := NewProgramFileName;
      RefreshDetails;
    end;
  end;
end;

procedure TForm4.RenameProgramFileAndSampleDirButtonClick(Sender: TObject);
var
  NewProgramFileName : string;
  fn : string;
  Dir : string;
begin
  if FileExists(CurrentProgramFile) = false then exit;

  fn := RemoveFileExt(CurrentProgramFile);
  Dir := ExtractFilePath(CurrentProgramFile);

  NewProgramFileName := InputBox('Rename Program File & Samples Directory', 'New Program File Name', fn);

  if NewProgramFileName <> fn then
  begin
    NewProgramFileName := IncludeTrailingPathDelimiter(Dir) + NewProgramFileName + '.lpg';
    RenameProgramFile(NewProgramFileName, CurrentProgramFile);

    if FileExists(NewProgramFileName) then
    begin
      CurrentProgramFile := NewProgramFileName;
      RefreshDetails;
    end;
  end;
end;



end.
