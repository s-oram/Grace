{
  GridFeedback is provides a unified way to send 'visual feedback cues' to grid controllers such as the Monome or the
  LaunchPad.


  Grid coordinates always start in the top left.


   Example:   0..1..2..3..4
              .           .
              1           .
              .           .
              2           .
              .           .
              3............


}


unit eeGridFeedback;

interface

{$INCLUDE Defines.inc}

uses
  Contnrs, eeGlobals, MoreTypes, eeMidiPorts;

type
  // umNativeMidi - Midi to grid via the vst->host-->midi interface.
  // umDirectMidi - Midi to grid by connecting directly to the midi port.
  // umOsc        - Open Sound Control. To be implemented. :( Theres' work in that one.
  TUpdateMethod = (umNativeMidi, umDirectMidi, umOSC);

  TGridDevice = (gdNone, gdMonome64);


  TGridScene = class
  private
    fButtonLightState: T2dArrayOfBoolean;
  public
    destructor Destroy; override;
    procedure SetGridSize(Width, Height:integer);
    property ButtonLightState:T2dArrayOfBoolean read fButtonLightState write fButtonLightState;
  end;


  TGridScenes = array of TGridScene;



  TGridFeedback = class
  private
    fSceneCount: integer;
    fGridHeight: integer;
    fGridWidth: integer;
    fCurrentScene: integer;
    fUpdateMethod: TUpdateMethod;
    fGridDevice: TGridDevice;
    fDirectMidiOutput: TMidiOutput;

  protected
    Globals:TGlobals;
    fScenes:TGridScenes;

    //Call to actually update the grid.
    procedure SetGrid(GridX, GridY: integer; LightState:boolean); inline;
    // NOTE: More SetGrid proceduers may be needed depending on performance, but as it stands, the lights can
    // be set one by one for now.


    //Override to respond to scene changes. Typically will involve updating lights.
    procedure SetCurrentScene(const NewSceneIndex: integer); virtual;
  public
    constructor Create(aGlobals:TGlobals; aSceneCount, aGridWidth, aGridHeight:integer); virtual;
	  destructor Destroy; override;

    procedure MidiNoteToGrid(const Note:byte; out x, y:integer); inline;
    function GridToMidiNote(const x, y:integer):byte;            inline;



    // NOTE: RefreshGrid & RefreshScene are similar in function.
    // RefreshGrid will force the grid to be redrawn regardless.
    // RefreshScene will only force the grid/scene to be redrawn if the Scene is currently visible.
    procedure RefreshGrid;
    procedure RefreshScene(SceneIndex:integer);


    // The 'Update' methods update the state of scene button lights, and if the scene is currently visible,
    // the grid will be updated also.
    procedure Update(GridX, GridY:integer; LightState:boolean; SceneIndex:integer);
    procedure UpdateRow(RowIndex:integer; LightState:boolean; SceneIndex:integer);
    procedure UpdateColumn(ColumnIndex:integer; LightState:boolean; SceneIndex:integer);
    procedure UpdateAll(LightState:boolean; SceneIndex:integer);

    property GridWidth :integer read fGridWidth;
    property GridHeight:integer read fGridHeight;

    property Scene:TGridScenes read fScenes write fScenes;
    property SceneCount:integer read fSceneCount;

    property CurrentScene:integer read fCurrentScene write SetCurrentScene;


    property UpdateMethod :TUpdateMethod read fUpdateMethod write fUpdateMethod;
    property GridDevice   :TGridDevice   read fGridDevice   write fGridDevice;

    property DirectMidiOutput:TMidiOutput read fDirectMidiOutput write fDirectMidiOutput;
  end;

implementation

uses
  Windows, SysUtils, eeMidiEvents;

{ TGridScene }

destructor TGridScene.Destroy;
begin
  SetGridSize(0,0);
  inherited;
end;

procedure TGridScene.SetGridSize(Width, Height: integer);
var
  OldGridWidth:integer;
  c1: Integer;
begin
  //Free the old grid memory.
  OldGridWidth := Length(ButtonLightState);
  for c1 := 0 to OldGridWidth - 1 do
  begin
    SetLength(fButtonLightState[c1], 0);
  end;
  SetLength(fButtonLightState, 0);

  //Initialise the new grid size memory.
  if (Width > 0) and (Height > 0) then
  begin
    SetLength(fButtonLightState, Width);
    for c1 := 0 to Width - 1 do
    begin
      SetLength(fButtonLightState[c1], Height);
    end;
  end;
end;



{ TGridFeedback }

constructor TGridFeedback.Create(aGlobals: TGlobals; aSceneCount, aGridWidth, aGridHeight:integer);
var
  c1: Integer;
begin
  Globals := aGlobals;

  fSceneCount := aSceneCount;
  fGridWidth  := aGridWidth;
  fGridHeight := aGridHeight;

  setLength(fScenes, aSceneCount);
  for c1 := 0 to aSceneCount - 1 do
  begin
    fScenes[c1] := TGridScene.Create;
    fScenes[c1].SetGridSize(aGridWidth, aGridHeight);
  end;

  fCurrentScene := 0;

  UpdateMethod := umNativeMidi;
  GridDevice   := gdMonome64;

  DirectMidiOutput := TMidiOutput.Create;
end;

destructor TGridFeedback.Destroy;
var
  c1: Integer;
begin
  {$IFDEF DebugMessages}
  OutputDebugString('TGridFeedback: Destroy Start.');
  {$ENDIF}


  for c1 := 0 to SceneCount - 1 do
  begin
    fScenes[c1].Free;
  end;
  DirectMidiOutput.Free;


  {$IFDEF DebugMessages}
  OutputDebugString('TGridFeedback: Destroy Finished.');
  {$ENDIF}

  inherited;
end;


function TGridFeedback.GridToMidiNote(const x, y: integer): byte;
begin
  case GridDevice of
    gdNone:
    begin
      result := 0;
      raise Exception.Create('No grid device selected.');
    end;

    gdMonome64:
    begin
      //Valid for the monome 8x8 in MIDI mode.
      result := (y * 16) + 8 + x;
    end;
  else
    raise Exception.Create('Grid device not implemented.');
    result := 0;
  end;
end;

procedure TGridFeedback.MidiNoteToGrid(const Note: byte; out x, y: integer); 
begin
  case GridDevice of
    gdNone:
    begin
      x := -1;
      y := -1;
    end;

    gdMonome64:
    begin
      //Valid for the monome 8x8 in MIDI mode.
      x := Note mod 16 - 8;
      y := Note div 16;
      if (x < 0) or (x > 7) then x := -1;
      if (y < 0) or (y > 7) then y := -1;
    end;
  else
    raise Exception.Create('Grid device not implemented.');
    x := -1;
    y := -1;
  end;
end;

procedure TGridFeedback.RefreshGrid;
var
  c1, c2:integer;
  LightState:boolean;
begin
  for c1 := 0 to GridWidth - 1 do
  begin
    for c2 := 0 to GridHeight - 1 do
    begin
      LightState := Scene[CurrentScene].ButtonLightState[c1,c2];
      SetGrid(c1, c2, LightState);
    end;
  end;
end;

procedure TGridFeedback.RefreshScene(SceneIndex: integer);
begin
  if (SceneIndex < 0) or (SceneIndex >= SceneCount) then Exception.Create('SceneIndex out of range.');
  if SceneIndex = CurrentScene then RefreshGrid;
end;

procedure TGridFeedback.Update(GridX, GridY: integer; LightState: boolean; SceneIndex: integer);
begin
  if (GridX < 0) or (GridX >= GridWidth)  then raise Exception.Create('GridX out of range.');
  if (GridY < 0) or (GridY >= GridHeight) then raise Exception.Create('GridY out of range.');
  if (SceneIndex < 0) or (SceneIndex >= SceneCount) then Exception.Create('SceneIndex out of range.');

  Scene[SceneIndex].ButtonLightState[GridX, GridY] := LightState;

  if SceneIndex = CurrentScene then SetGrid(GridX, GridY, LightState);
end;

procedure TGridFeedback.UpdateColumn(ColumnIndex: integer; LightState: boolean; SceneIndex: integer);
var
  c1: Integer;
begin
  if (ColumnIndex < 0) or (ColumnIndex >= GridWidth)  then raise Exception.Create('ColumnIndex out of range.');
  if (SceneIndex < 0)  or (SceneIndex >= SceneCount)  then Exception.Create('SceneIndex out of range.');

  for c1 := 0 to GridHeight - 1 do
  begin
    Scene[SceneIndex].ButtonLightState[ColumnIndex, c1] := LightState;
  end;

  if SceneIndex = CurrentScene then
  begin
    for c1 := 0 to GridHeight - 1 do
    begin
      SetGrid(ColumnIndex, c1, LightState);
    end;
  end;
end;

procedure TGridFeedback.UpdateRow(RowIndex: integer; LightState: boolean; SceneIndex: integer);
var
  c1:integer;
begin
  if (RowIndex < 0) or (RowIndex >= GridHeight) then raise Exception.Create('RowIndex out of range.');
  if (SceneIndex < 0) or (SceneIndex >= SceneCount) then Exception.Create('SceneIndex out of range.');

  for c1 := 0 to GridHeight - 1 do
  begin
    Scene[SceneIndex].ButtonLightState[c1, RowIndex] := LightState;
  end;

  if SceneIndex = CurrentScene then
  begin
    for c1 := 0 to GridHeight - 1 do
    begin
      SetGrid(c1, RowIndex, LightState);
    end;
  end;
end;

procedure TGridFeedback.UpdateAll(LightState: boolean; SceneIndex: integer);
var
  c1: Integer;
  c2: Integer;
begin
  if (SceneIndex < 0) or (SceneIndex >= SceneCount) then Exception.Create('SceneIndex out of range.');

  for c1 := 0 to GridWidth - 1 do
  begin
    for c2 := 0 to GridHeight - 1 do
    begin
      Scene[SceneIndex].ButtonLightState[c1, c2] := LightState;
    end;
  end;

  if SceneIndex = CurrentScene then
  begin
    for c1 := 0 to GridWidth - 1 do
    begin
      for c2 := 0 to GridHeight - 1 do
      begin
        SetGrid(c1, c2, LightState);
      end;
    end;
  end;
end;


procedure TGridFeedback.SetCurrentScene(const NewSceneIndex: integer);
begin
  if NewSceneIndex <> fCurrentScene then
  begin
    fCurrentScene := NewSceneIndex;
    RefreshGrid;
  end;
end;

procedure TGridFeedback.SetGrid(GridX, GridY: integer; LightState:boolean);
var
  Status, Channel, Data1, Data2, Delta:byte;
begin
  //Raw button light update method. Should only be used internally.

  case UpdateMethod of
    umNativeMidi:
    begin
      if LightState = true
        then Status := kNoteOn
        else Status := kNoteOff;

      Channel := 0;
      Data1 := GridToMidiNote(GridX, GridY);
      Data2 := 50;
      Delta := 0; //send grid update ASAP.
      Globals.MidiOutput.AddEvent(Status, Channel, Data1, Data2, Delta);
    end;

    umDirectMidi:
    begin
      assert(DirectMidiOutput.IsPortOpen);

      if LightState = true
        then Status := kNoteOn
        else Status := kNoteOff;

      Channel := 0;
      Data1 := GridToMidiNote(GridX, GridY);
      Data2 := 50;
      Delta := 0; //send grid update ASAP.

      DirectMidiOutput.Send(Status, Data1, Data2);

    end;

    umOSC:
    begin
    end;
  else
    raise Exception.Create('Update method not implemented.');
  end;

end;





end.
