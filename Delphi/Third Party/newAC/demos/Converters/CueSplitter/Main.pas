(* This demo splits long audio files convrted from CD/DVD imagess into separate
  compositions using the supplied cue-sheets *)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACS_WavPack, ACS_FLAC, ACS_Classes, ACS_MAC, ACS_Misc, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    MACIn1: TMACIn;
    FLACIn1: TFLACIn;
    WVIn1: TWVIn;
    CueSplitter1: TCueSplitter;
    MACOut1: TMACOut;
    FLACOut1: TFLACOut;
    WVOut1: TWVOut;
    OpenDialog1: TOpenDialog;
    Memo1: TMemo;
    OpenDialog2: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure WVOut1Done(Sender: TComponent);
    procedure MACOut1Done(Sender: TComponent);
    procedure FLACOut1Done(Sender: TComponent);
  private
    { Private declarations }
  public
    { Public declarations }
    Output : TAuTaggedFileOut;
    procedure FillList;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FillList;
var
  i, min, sec : Integer;
  S : String;
  sep : String;
begin
  Memo1.Clear;
  for i := 0 to CueSplitter1.ItemsCount - 1 do
  begin
    CueSplitter1.CurrentItem := i;
    Min := CueSplitter1.Time div 60;
    Sec := CueSplitter1.Time mod 60;
    if Sec < 10 then sep := ':0'
    else
    sep := ':';
    S := Format('[ %s ]  / %s /  %s - %d%s%d', [CueSplitter1.Performer, CueSplitter1.Album, CueSplitter1.Title, min, sep, sec]);
    Memo1.Lines.Add(S);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Ext : WideString;
  FileName : String;
  SL : TStringList;
  i : Integer;
begin
  OpenDialog1.Title := 'Open Source';
  if OpenDialog1.Execute then
  begin
    FileName := OpenDialog1.FileName;
    Ext := ExtractFileExt(FileName);
    Ext := CharLower(@Ext[1]);
    if Ext = '.wv' then
    begin
     // Output := TAuFileOut(WVOut1);
      CueSplitter1.Input := WVIn1;
      WVIn1.FileName := FileName;
      if WVIn1.Cuesheet <> '' then
      begin
        SL := TStringList.Create;
        SL.Text := WVIn1.Cuesheet;
        SL.SaveToFile(FileName + '.cue');
        SL.Free;
        CueSplitter1.CueFile := FileName + '.cue';
      end else
      begin
        if OpenDialog2.Execute then
        begin
          CueSplitter1.CueFile := OpenDialog2.FileName;
        end;
      end;
      if CueSplitter1.CueFile <> '' then
      begin
        FillList;
        CueSplitter1.CurrentItem := 0;
        WVOut1.FileName := Format('xTrack-%d.wv', [1]);
        WVOut1.APEv2Tags.Album := CueSplitter1.Album;
        WVOut1.APEv2Tags.Artist := CueSplitter1.Performer;
        WVOut1.APEv2Tags.Title := CueSplitter1.Title;
        WVOut1.APEv2Tags.Year := CueSplitter1.Year;
        WVOut1.APEv2Tags.Genre := CueSplitter1.Genre;
        WVOut1.APEv2Tags.Track := '0';
        Button1.Enabled := False;
        WVOut1.Run;
      end;
    end else
    if Ext = '.ape' then
    begin
      CueSplitter1.Input := MacIn1;
      MacIn1.FileName := FileName;
      if OpenDialog2.Execute then
      begin
        CueSplitter1.CueFile := OpenDialog2.FileName;
      end;
      if CueSplitter1.CueFile <> '' then
      begin
        FillList;
        CueSplitter1.CurrentItem := 0;
        MacOut1.FileName := Format('xTrack-%d.ape', [1]);
        MacOut1.APEv2Tags.Album := CueSplitter1.Album;
        MacOut1.APEv2Tags.Artist := CueSplitter1.Performer;
        MacOut1.APEv2Tags.Title := CueSplitter1.Title;
        MacOut1.APEv2Tags.Year := CueSplitter1.Year;
        MacOut1.APEv2Tags.Genre := CueSplitter1.Genre;
        MacOut1.APEv2Tags.Track := '0';
        Button1.Enabled := False;
        MacOut1.Run;
      end;
    end else
    if Ext = '.flac' then
    begin
      CueSplitter1.Input := FLACIn1;
      FLACIn1.FileName := FileName;
      if OpenDialog2.Execute then
      begin
        CueSplitter1.CueFile := OpenDialog2.FileName;
      end;
      if CueSplitter1.CueFile <> '' then
      begin
        FillList;
        CueSplitter1.CurrentItem := 0;
        FLACOut1.FileName := Format('xTrack-%d.flac', [1]);
        FLACOut1.Tags.Album := CueSplitter1.Album;
        FLACOut1.Tags.Artist := CueSplitter1.Performer;
        FLACOut1.Tags.Title := CueSplitter1.Title;
        FLACOut1.Tags.Date := CueSplitter1.Year;
        FLACOut1.Tags.Genre := CueSplitter1.Genre;
        FLACOut1.Tags.Track := '1';
        Button1.Enabled := False;
        FLACOut1.Run;
      end;
    end;

  end;

end;

procedure TForm1.FLACOut1Done(Sender: TComponent);
begin
  if CueSplitter1.CurrentItem < CueSplitter1.ItemsCount - 1 then
  begin
    CueSplitter1.CurrentItem := CueSplitter1.CurrentItem + 1;
    FLACOut1.FileName := Format('xTrack-%d.flac', [CueSplitter1.CurrentItem + 1]);
    FLACOut1.Tags.Album := CueSplitter1.Album;
    FLACOut1.Tags.Artist := CueSplitter1.Performer;
    FLACOut1.Tags.Title := CueSplitter1.Title;
    FLACOut1.Tags.Date := CueSplitter1.Year;
    FLACOut1.Tags.Genre := CueSplitter1.Genre;
    FLACOut1.Tags.Track := IntToStr(CueSplitter1.CurrentItem + 1);
    FLACOut1.Run;
  end else
    Button1.Enabled := True;
end;

procedure TForm1.MACOut1Done(Sender: TComponent);
begin
  if CueSplitter1.CurrentItem < CueSplitter1.ItemsCount - 1 then
  begin
    CueSplitter1.CurrentItem := CueSplitter1.CurrentItem + 1;
    MacOut1.FileName := Format('xTrack-%d.ape', [CueSplitter1.CurrentItem + 1]);
    MacOut1.APEv2Tags.Album := CueSplitter1.Album;
    MacOut1.APEv2Tags.Artist := CueSplitter1.Performer;
    MacOut1.APEv2Tags.Title := CueSplitter1.Title;
    MacOut1.APEv2Tags.Year := CueSplitter1.Year;
    MacOut1.APEv2Tags.Genre := CueSplitter1.Genre;
    MacOut1.APEv2Tags.Track := IntToStr(CueSplitter1.CurrentItem + 1);
    MacOut1.Run;
  end else
    Button1.Enabled := True;
end;

procedure TForm1.WVOut1Done(Sender: TComponent);
begin
  if CueSplitter1.CurrentItem < CueSplitter1.ItemsCount - 1 then
  begin
    CueSplitter1.CurrentItem := CueSplitter1.CurrentItem + 1;
    WVOut1.FileName := Format('xTrack-%d.wv', [CueSplitter1.CurrentItem + 1]);
    WVOut1.APEv2Tags.Album := CueSplitter1.Album;
    WVOut1.APEv2Tags.Artist := CueSplitter1.Performer;
    WVOut1.APEv2Tags.Title := CueSplitter1.Title;
    WVOut1.APEv2Tags.Year := CueSplitter1.Year;
    WVOut1.APEv2Tags.Genre := CueSplitter1.Genre;
    WVOut1.APEv2Tags.Track := IntToStr(CueSplitter1.CurrentItem + 1);
    WVOut1.Run;
  end else
    Button1.Enabled := True;
end;

end.
