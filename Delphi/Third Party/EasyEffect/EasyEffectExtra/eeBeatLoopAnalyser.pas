{
  Beat Loop Analyser finds slice points in a loop. Several different algorthms are included.

}

unit eeBeatLoopAnalyser;

interface

uses
  Contnrs, MoreTypes, eeSampleFloat,
  eeBeatLoopAnalyser.CoreTypes,
  eeBeatLoopAnalyser.GenericChopperStrategy,
  eeBeatLoopAnalyser.EqualLengthChopperStrategy,
  eeBeatLoopAnalyser.BeatChopperStrategy;

type
  TBeatLoopAnalyser = class
  private
    fEqualLengthChopper: TEqualLengthChopper;
    fBeatChopperA: TBeatChopperA;
    fEnv2: TSampleFloat;
    fEnv1: TSampleFloat;
    function GetSlice(Index: integer): TSliceInfo;
    function GetSliceCount: integer;
  protected
    SliceList : TSliceList;
  public
    constructor Create;
    destructor Destroy; override;

    //======================================
    // The chopping strategies
    //======================================
    // Chops a sample into X number of equal length slices.
    property EqualLengthChopper : TEqualLengthChopper read fEqualLengthChopper write fEqualLengthChopper;
    // BeatChopper slices the sample into X number of slices, each roughly the same
    // length. Slice points will be shifted to the strongest nearby hitpoint. This
    // method has been designed for chopping drum loops into a set number of slices.
    property BeatChopperA        : TBeatChopperA        read fBeatChopperA        write fBeatChopperA;


    // The chopping results can be access with SliceCount and Slices[].
    property SliceCount : integer read GetSliceCount;
    property Slices[Index : integer]:TSliceInfo read GetSlice; default;

    property Env1 : TSampleFloat read fEnv1 write fEnv1;
    property Env2 : TSampleFloat read fEnv2 write fEnv2;
  end;

implementation


{ TBeatLoopAnalyser }

constructor TBeatLoopAnalyser.Create;
begin
  Env1 := TSampleFloat.Create;
  Env2 := TSampleFloat.Create;

  SliceList := TSliceList.Create;

  EqualLengthChopper := TEqualLengthChopper.Create(SliceList, Env1, Env2);
  BeatChopperA        := TBeatChopperA.Create(SliceList, Env1, Env2);
end;

destructor TBeatLoopAnalyser.Destroy;
begin
  EqualLengthChopper.Free;
  BeatChopperA.Free;
  SliceList.Free;
  Env1.Free;
  Env2.Free;
  inherited;
end;

function TBeatLoopAnalyser.GetSlice(Index: integer): TSliceInfo;
begin
  result := SliceList[Index];
end;

function TBeatLoopAnalyser.GetSliceCount: integer;
begin
  result := SliceList.Count;
end;





end.
