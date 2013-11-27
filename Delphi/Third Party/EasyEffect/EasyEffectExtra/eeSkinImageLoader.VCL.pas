unit eeSkinImageLoader.VCL;

interface

uses
  Windows, Graphics, Contnrs;

type
  TSkinImageLoader = class
  private
    fSkinItemList: TObjectList;
    fSkinDirectory: string;
    fShowWarningDialogs: boolean;
  protected
    function LoadFromResource(ResourceName:string; var Dest:TBitmap):boolean;
    function LoadFromSkinDir(const SkinDir, ImageFileName:string; var Dest:TBitmap):boolean;

    property SkinItemList:TObjectList read fSkinItemList write fSkinItemList;
  public
    constructor Create;
	  destructor Destroy; override;

    function LoadImage(const FinalImageID:string):boolean; overload;
    procedure LoadImage(const FinalImageID, ResourceName:string; const SkinDir:string = ''; const ImageFileName:string = ''); overload; deprecated;

    function Exists(ImageID:string):boolean;
    function GetImage(ImageID:string):TBitmap;

    property SkinDirectory:string read fSkinDirectory write fSkinDirectory;

    property ShowWarningDialogs : boolean read fShowWarningDialogs write fShowWarningDialogs;
  end;

implementation

uses
  Vcl.Imaging.PngImage, SysUtils, Classes, uAutoFree,
  Dialogs;

var
  GlobalInstanceCount :integer;
  GlobalSkinItemList  :TObjectList;

type
  TSkinItem = class
  private
    fImageID: string;
    fImage: TBitmap;
  public
    constructor Create;
	  destructor Destroy; override;

    property ImageID :string  read fImageID write fImageID;
    property Image   :TBitmap read fImage   write fImage;
  end;

{ TSkinItem }

constructor TSkinItem.Create;
begin
  Image := TBitmap.Create;
end;

destructor TSkinItem.Destroy;
begin
  Image.Free;
  inherited;
end;

procedure ResetAlphaMask(aBitmap:TBitmap);
type
  // Define a generic 4 byte pixel so we can access the individual bytes of
  // each 4 byte pixel.
  PPixel = ^TPixel;
  TPixel = record
    b1, b2, b3, b4 : byte;
  end;
var
  c1: Integer;
  c2: Integer;
  SrcPixel  : PPixel;
begin
  for c1 := 0 to aBitmap.Height-1 do
  begin
    SrcPixel := aBitmap.ScanLine[c1];
    for c2 := 0 to aBitmap.Width-1 do
    begin
      SrcPixel^.b4 := 255;
      inc(SrcPixel);
    end;
  end;
end;



{ TSkinImageLoader }

constructor TSkinImageLoader.Create;
begin
  inc(GlobalInstanceCount);
  SkinItemList := GlobalSkinItemList;
  ShowWarningDialogs := false;
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

function TSkinImageLoader.LoadImage(const FinalImageID: string):boolean;
var
  SkinItem:TSkinItem;
  DestBitmap:TBitmap;
  ImageFileName : string;
  ImageFileFullPath : string;
  ResourceName : string;
  LoadResult : boolean;
begin
  if Exists(FinalImageID) then exit;

  LoadResult := false;

  SkinItem := TSkinItem.Create;
  try
    SkinItem.ImageID := FinalImageID;
    DestBitmap       := SkinItem.Image;
    ImageFileName    := FinalImageID + '.png';
    ImageFileFullPath := IncludeTrailingPathDelimiter(SkinDirectory) + ImageFileName;
    ResourceName     := Uppercase(FinalImageID);

    if (SkinDirectory <> '') and (ImageFileName <> '') and (FileExists(ImageFileFullPath)) then
    begin
      LoadResult := LoadFromSkinDir(SkinDirectory, ImageFileName, DestBitmap);
    end else
    if (FindResource(hInstance, PWideChar(WideString(ResourceName)), MakeIntResource(RT_RCDATA)) <> 0) then
    begin
      LoadResult := LoadFromResource(ResourceName, DestBitmap);
    end else
    begin
      LoadResult := false;
    end;
  finally
    if LoadResult = true then
    begin
      SkinItemList.Add(SkinItem)
    end else
    begin
      SkinItem.Free;
      if ShowWarningDialogs then ShowMessage('Could not find skin image \"' + ImageFileName + '\".');
    end;
  end;

  result := LoadResult;
end;


procedure TSkinImageLoader.LoadImage(const FinalImageID, ResourceName:string; const SkinDir:string = ''; const ImageFileName:string = '');
begin
  assert(false); //method deprecated.
end;




function TSkinImageLoader.LoadFromResource(ResourceName: string; var Dest: TBitmap): boolean;
var
  //rs:TResourceStream;
  PngImage : TPngImage;
  //x : integer;
begin
  PngImage := TPngImage.Create;
  AutoFree(@PngImage);

  ResourceName := UpperCase(ResourceName);

  try
    PngImage.LoadFromResourceName(HInstance, ResourceName);
  except
    on EPNGCouldNotLoadResource do
    begin
      result := false;
      exit; //================>>exit>>================>>
    end;
  end;

  PngImage.AssignTo(Dest);

  if Dest.PixelFormat <> TPixelFormat.pf32bit then
  begin
    Dest.PixelFormat := TPixelFormat.pf32bit;
    ResetAlphaMask(Dest);
  end;

  //if we've made it this far, the image must be succesfully loaded.
  result := true;
end;




function TSkinImageLoader.LoadFromSkinDir(const SkinDir, ImageFileName: string; var Dest: TBitmap):boolean;
var
  FileName:String;
  PngImage : TPngImage;
begin
  PngImage := TPngImage.Create;
  AutoFree(@PngImage);

  FileName := IncludeTrailingPathDelimiter(SkinDir) + ImageFileName;
  PngImage.LoadFromFile(FileName);
  PngImage.AssignTo(Dest);

  if Dest.PixelFormat <> TPixelFormat.pf32bit then
  begin
    Dest.PixelFormat := TPixelFormat.pf32bit;
    ResetAlphaMask(Dest);
  end;

  //if we've made it this far, the image must be succesfully loaded.
  result := true;
end;



initialization
  GlobalInstanceCount := 0;

  GlobalSkinItemList  := TObjectList.Create;
  GlobalSkinItemList.OwnsObjects := true;

finalization
  GlobalSkinItemList.Free;

end.
