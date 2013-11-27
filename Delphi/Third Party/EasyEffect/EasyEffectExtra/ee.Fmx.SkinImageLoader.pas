unit ee.Fmx.SkinImageLoader;

interface

uses
  Windows, Fmx.Types, Contnrs;

type
  TSkinImageLoader = class
  private
    fSkinItemList: TObjectList;
    fSkinDirectory: string;
  protected
    procedure LoadFromResource(ResourceName:string; var Dest:TBitmap);
    procedure LoadFromSkinDir(const SkinDir, ImageFileName:string; var Dest:TBitmap);

    property SkinItemList:TObjectList read fSkinItemList write fSkinItemList;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure LoadImage(const FinalImageID, ResourceName:string; const SkinDir:string = ''; const ImageFileName:string = ''); overload;
    procedure LoadImage(const FinalImageID:string); overload;

    function Exists(ImageID:string):boolean;
    function GetImage(ImageID:string):TBitmap;

    property SkinDirectory:string read fSkinDirectory write fSkinDirectory;
  end;

implementation

uses
  SysUtils, Classes;

type
  TSkinItem = class
  private
    fImageID: string;
    fImage: TBitmap;
  public
    constructor Create;
	  destructor Destroy; override;

    property ImageID :string    read fImageID write fImageID;
    property Image   :TBitmap read fImage   write fImage;
  end;

{ TSkinItem }

constructor TSkinItem.Create;
begin
  //Image := TBitmap
  Image := TBitmap.Create(1,1);
end;

destructor TSkinItem.Destroy;
begin
  Image.Free;
  inherited;
end;


{ TSkinImageLoader }

constructor TSkinImageLoader.Create;
begin
  SkinItemList := TObjectList.Create;
  SkinItemList.OwnsObjects := true;
end;

destructor TSkinImageLoader.Destroy;
begin
  SkinItemList.Free;
  inherited;
end;

function TSkinImageLoader.Exists(ImageID: string): boolean;
var
  c1: Integer;
begin
  for c1 := 0 to SkinItemList.Count - 1 do
  begin
    if (SkinItemList[c1] as TSkinItem).ImageID = ImageID then
    begin
      result := true;
      exit; //===============================>>
    end;
  end;
  result := false;
end;

function TSkinImageLoader.GetImage(ImageID: string): TBitmap;
var
  c1: Integer;
begin
  for c1 := 0 to SkinItemList.Count - 1 do
  begin
    if (SkinItemList[c1] as TSkinItem).ImageID = ImageID then
    begin
      result := (SkinItemList[c1] as TSkinItem).Image;
      exit; //===============================>>
    end;
  end;
  result := nil;
end;

procedure TSkinImageLoader.LoadImage(const FinalImageID, ResourceName:string; const SkinDir:string = ''; const ImageFileName:string = '');
var
  SkinItem:TSkinItem;
  DestBitmap:TBitmap;
begin
  if Exists(FinalImageID) then exit;

  SkinItem := TSkinItem.Create;
  try
    SkinItem.ImageID := FinalImageID;
    DestBitmap := SkinItem.Image;

    if (SkinDir <> '') and (ImageFileName <> '') and (FileExists(SkinDir + ImageFileName)) then
    begin
      LoadFromSkinDir(SkinDir, ImageFileName, DestBitmap);
    end else
    begin
      LoadFromResource(ResourceName, DestBitmap);
    end;
  finally
    SkinItemList.Add(SkinItem);
  end;
end;




procedure TSkinImageLoader.LoadFromResource(ResourceName: string; var Dest: TBitmap);
var
  rs:TResourceStream;
begin
  ResourceName := UpperCase(ResourceName);
  rs := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  try
    Dest.LoadFromStream(rs);
  finally
    rs.Free
  end;
end;


procedure TSkinImageLoader.LoadFromSkinDir(const SkinDir, ImageFileName: string; var Dest: TBitmap);
var
  FileName:String;
begin
  FileName := SkinDir + ImageFileName;
  Dest.LoadFromFile(FileName);
end;

procedure TSkinImageLoader.LoadImage(const FinalImageID: string);
begin
  LoadImage(FinalImageID, FinalImageID, SkinDirectory, FinalImageID + '.png')
end;



end.
