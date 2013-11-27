unit eeSkinImageLoader;

interface

uses
  {$IFDEF VER230}
  {$ELSE}
  GraphicEx,
  {$ENDIF}
  vg_scene, Contnrs;

type
  TSkinImageLoader = class
  private
    fSkinItemList: TObjectList;
    fSkinDirectory: string;
  protected
    procedure LoadFromResource(ResourceName:string; var Dest:TvgBitmap);
    procedure LoadFromSkinDir(const SkinDir, ImageFileName:string; var Dest:TvgBitmap);

    property SkinItemList:TObjectList read fSkinItemList write fSkinItemList;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure LoadImage(const FinalImageID, ResourceName:string; const SkinDir:string = ''; const ImageFileName:string = ''); overload;
    procedure LoadImage(const FinalImageID:string); overload; 

    function Exists(ImageID:string):boolean;
    function GetImage(ImageID:string):TvgBitmap;

    property SkinDirectory:string read fSkinDirectory write fSkinDirectory;
  end;

implementation

uses
  SysUtils, Classes, Windows;

var
  // NOTE: The GlobalInstanceCount variable is used to keep track of how many TSkinImageLoader instances there are.
  // Once the last instance is free'ed the globalSkinItemList is cleared of any skin items. This needs to be done
  // because the skin items use TvgBitmap's from VGScene. VGScene is a bit fussy with the way it is used by DLL's.
  // Because of VGScene, all VGSCene objects must be free'ed before the application begins to process the "finalization"
  // unit sections.
  GlobalInstanceCount :integer;
  GlobalSkinItemList  :TObjectList;

type
  TSkinItem = class
  private
    fImageID: string;
    fImage: TvgBitmap;
  public
    constructor Create;
	  destructor Destroy; override;

    property ImageID :string    read fImageID write fImageID;
    property Image   :TvgBitmap read fImage   write fImage;
  end;

{ TSkinItem }

constructor TSkinItem.Create;
begin
  Image := TvgBitmap.Create(1,1,true);
end;

destructor TSkinItem.Destroy;
begin
  Image.Free;
  inherited;
end;


{ TSkinImageLoader }

constructor TSkinImageLoader.Create;
begin
  inc(GlobalInstanceCount);
  SkinItemList := GlobalSkinItemList;
end;

destructor TSkinImageLoader.Destroy;
begin
  dec(GlobalInstanceCount);
  if GlobalInstanceCount <= 0 then SkinItemList.Clear;  
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

function TSkinImageLoader.GetImage(ImageID: string): TvgBitmap;
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
  DestBitmap:TvgBitmap;
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




procedure TSkinImageLoader.LoadFromResource(ResourceName: string; var Dest: TvgBitmap);
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


procedure TSkinImageLoader.LoadFromSkinDir(const SkinDir, ImageFileName: string; var Dest: TvgBitmap);
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


initialization
  GlobalInstanceCount := 0;

  GlobalSkinItemList  := TObjectList.Create;
  GlobalSkinItemList.OwnsObjects := true;

finalization
  GlobalSkinItemList.Free;

end.
