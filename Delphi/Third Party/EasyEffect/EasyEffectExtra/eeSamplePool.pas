{
  AudioFilePool manges loading and unloading of audio files.
}

unit eeSamplePool;

interface

{$INCLUDE Defines.inc}

uses
  SyncObjs, Contnrs, eeCustomSample, eeSampleInt, eeSampleFloat;

type
  TSampleClassType = (eeFloat, eeInt);

  TSamplePool = class
  private
    function GetSample(Index: integer): TCustomSample;
  protected
    Lock:TMutex;

    // Contains all samples currently in the pool, except for samples in the DeleteQueue list.
    // OwnsObjects = true
    SampleList:TObjectList;

    // Contains references to samples in use by the application. These samples should not be deleted.
    // OwnsObjects = false
    InUseList:TObjectList;

    // If the application requests a sample to be deleted but is already in use, it will not be deleted and moved
    // to the DeleteQueue instead.
    // OwnsObjects = true
    DeleteQueue:TObjectList;
    
  public
    constructor Create;
	  destructor Destroy; override;

    function LoadSample(FileName:string; SampleClassType:TSampleClassType = eeFloat):TCustomSample;

    procedure Add(aSample:TCustomSample);
    procedure Delete(aSample:TCustomSample);

    function MarkSampleInUse(aSample:TCustomSample):TCustomSample; //Call to say sample is being used and shouldn't be deleted.
    procedure MarkSampleFree(aSample:TCustomSample);              //Call when sample no longer being used.

    function IndexOf(aSample:TCustomSample):integer;

    property Samples[Index:integer]:TCustomSample read GetSample;
  end;

implementation

uses
  Windows, SysUtils;

{ TAudioFilePool }

constructor TSamplePool.Create;
begin
  SampleList := TObjectList.Create;
  SampleList.OwnsObjects := true;

  InUseList := TObjectList.Create;
  InUseList.OwnsObjects := false;

  DeleteQueue := TObjectList.Create;
  DeleteQueue.OwnsObjects := true;

  Lock := TMutex.Create;
end;

destructor TSamplePool.Destroy;
begin
  {$IFDEF DebugMessages}
  OutputDebugString('TSamplePool: Destroy Start.');
  {$ENDIF}

  SampleList.Free;
  InUseList.Free;
  DeleteQueue.Free;


  Lock.Acquire;
  Lock.Release;
  Lock.Free;

  {$IFDEF DebugMessages}
  OutputDebugString('TSamplePool: Destroy End.');
  {$ENDIF}

  inherited;
end;

procedure TSamplePool.Add(aSample: TCustomSample);
begin
  SampleList.Add(aSample);
end;                      

function TSamplePool.GetSample(Index: integer): TCustomSample;
begin
  result := SampleList[Index] as TCustomSample;
end;                      

function TSamplePool.IndexOf(aSample: TCustomSample):integer;
begin
  result := SampleList.IndexOf(aSample);
end;

function TSamplePool.LoadSample(FileName: string; SampleClassType: TSampleClassType): TCustomSample;
var
  aSample:TCustomSample;
begin
  // WARNING: LoadSample will return NIL if unable to load the sample for any reason.


  case SampleClassType of
    eeFloat: aSample := TSampleFloat.Create;
    eeInt:   aSample := TSampleInt.Create;
  else
    raise Exception.Create('Unknow sample class type.');
  end;

  if aSample.LoadFromFile(FileName) then
  begin
    SampleList.Add(aSample);
    result := aSample;
  end else
  begin
    aSample.Free;
    result := nil;
  end;
end;

function TSamplePool.MarkSampleInUse(aSample: TCustomSample):TCustomSample;
var
  Index:integer;
begin
  Lock.Acquire;
  try
    Index := SampleList.IndexOf(aSample);
    if Index = -1
      then result := nil      // Sample not in Pool, so it can not be marked in use.
      else result := aSample; // sample will successfully be marked as in use, so return a reference to it.

    InUseList.Add(aSample);
  finally
    Lock.Release;
  end;
end;


procedure TSamplePool.MarkSampleFree(aSample: TCustomSample);
var
  Index:integer;
begin
  Lock.Acquire;
  try
    //Delete from InUseList.
    Index := InUseList.IndexOf(aSample);
    if Index <> -1 then InUseList.Delete(Index);

    //Check if sample is in the delete queue.
    Index := DeleteQueue.IndexOf(aSample);
    if Index <> -1 then DeleteQueue.Delete(Index);

  finally
    Lock.Release;
  end;
end;



procedure TSamplePool.Delete(aSample: TCustomSample);
var
  Index:integer;
begin
  Lock.Acquire;

  Index := InUseList.IndexOf(aSample);
  if Index = -1 then
  begin
    Index := SampleList.IndexOf(aSample);
    SampleList.Delete(Index);
  end else
  begin
    SampleList.Extract(aSample);
    DeleteQueue.Add(aSample);
  end;                       

  Lock.Release;
end;




end.
