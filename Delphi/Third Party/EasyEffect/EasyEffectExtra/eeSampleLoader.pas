{
  TSampleLoader

  Loading large samples can be slow. TSampleLoader loads a sample in a seperate thread. It can be
  used to avoid blocking audio or GUI threads.


  NOTE: This code was written while I was still learning how to use OmniThreadLibrary effectively.



  NOTE: This code assumes there are two threads:
  - MainThread
  - WorkerThread
  It is assumed MainThread is the audio/GUI thread that is using TSampleLoader..



  NOTE: TSampleLoader does not use a task loop, but I do think it would make more sense with on.
  My first implementation used a task loop, but it would hang on some user machines.
  The hang was probably my fault, but I didn't figure out the cause.
  Switching to the current non-loop style process fixed the bug, however, I don't think
  the intent of the code is as clear.

}

unit eeSampleLoader;

interface

uses
  eeCustomSample, eeSampleInt, eeSampleFloat, OTLComm, OTLTask, OTLTaskControl, OTLEventMonitor, SyncObjs;

type
  TLoadSampleResult = class
  private
    fSampleID: cardinal;
    fSampleFormat: TSampleDataFormat;
    fSample: TCustomSample;
    fIsSuccess: boolean;
  public
    constructor Create;
	  destructor Destroy; override;
    property IsSuccess    :boolean           read fIsSuccess    write fIsSuccess;
    property Sample       :TCustomSample     read fSample       write fSample;
    property SampleID     :cardinal          read fSampleID     write fSampleID;
    property SampleFormat :TSampleDataFormat read fSampleFormat write fSampleFormat;
  end;

  TOnLoadSampleEvent = procedure(Sender:TObject; var Data:TLoadSampleResult) of object;

  TSampleLoader = class
  private
    fOnLoadSample   : TOnLoadSampleEvent;
  protected
    LoadSampleTask  : IOmniTaskControl;

    IsBusyLock:TCriticalSection;

    //--Tempory variable storage---
    NextFileName   :string;
    NextFormat     :TSampleDataFormat;
    NextLoadDelay  :integer;
    NextSampleID   :integer;
    //---------------------------------

    procedure Task_LoadSample(const task: IOmniTask);
    procedure Task_MessageHandler(const task: IOmniTaskControl; const msg: TOmniMessage);
    procedure FreeTaskControl(const task: IOmniTaskControl);
  public
    constructor Create;
	  destructor Destroy; override;

    procedure LoadSample(FileName: String; Format: TSampleDataFormat; LoadDelay_ms:integer = 0; SampleID:cardinal = 0);
    procedure CancelLoadSample;

    // NOTE: When handling OnLoadSample, the Data.Sample object will be free'ed, if not set to nil, when
    // control passes back to the TSampleLoader instance.
    property OnLoadSample : TOnLoadSampleEvent read fOnLoadSample write fOnLoadSample;
  end;



implementation

uses
  SiAuto, SysUtils, OTLCommon;

const
  mID_LoadSample       = 1;
  mID_CancelLoadSample = 2;
  mID_LoadFinished     = 3;

{ TLoadSampleResult }

constructor TLoadSampleResult.Create;
begin

end;

destructor TLoadSampleResult.Destroy;
begin
  if assigned(fSample) then FreeAndNil(fSample);
  inherited;
end;










{ TSampleLoader }

constructor TSampleLoader.Create;
begin
  NextFileName := '';
  IsBusyLock := TCriticalSection.Create;
end;

destructor TSampleLoader.Destroy;
begin
  if LoadSampleTask <> nil then
  begin
    LoadSampleTask.Terminate;
    LoadSampleTask := nil;
  end;

  IsBusyLock.Free;
  inherited;
end;

procedure TSampleLoader.LoadSample(FileName: String; Format: TSampleDataFormat; LoadDelay_ms:integer = 0; SampleID:cardinal = 0);
var
  FormatAsString : string;
begin
  IsBusyLock.Enter;
  try
    if LoadSampleTask = nil then
    begin
      NextFileName := '';

      case Format of
        dfFloat: FormatAsString := 'dfFloat';
        dfInt:   FormatAsString := 'dfInt';
      else
        FormatAsString := 'error';
        raise Exception.Create('Format type not handled.');
      end;

      LoadSampleTask := CreateTask(Task_LoadSample)
                        .SetParameter('FileName', FileName)
                        .SetParameter('LoadDelay', LoadDelay_ms)
                        .SetParameter('SampleID', SampleID)
                        .SetParameter('Format', FormatAsString)
                        .OnMessage(Task_MessageHandler)
                        .OnTerminated(FreeTaskControl)
                        .Run;

      siAuto.SiMain.LogMessage('Do_LoadSample');
    end else
    begin
      // NOTE: TSampleLoader has a very basic load queue mechanism. If NextFileName doesn't equal '', then
      // TSampleLoader will attempt to load it. 
      NextFileName   := FileName;
      NextFormat     := Format;
      NextLoadDelay  := LoadDelay_ms;
      NextSampleID   := SampleID;

      //Load sample task is already doing something, so cancel that load request.
      LoadSampleTask.Comm.Send(mID_CancelLoadSample);

      siAuto.SiMain.LogMessage('Do_CancelLoadSample');
    end;

  finally
    IsBusyLock.Leave;
  end;
end;

procedure TSampleLoader.CancelLoadSample;
begin
  IsBusyLock.Enter;
  try
    if LoadSampleTask <> nil then
    begin
      LoadSampleTask.Comm.Send(mID_CancelLoadSample);
    end;
  finally
    IsBusyLock.Leave;
  end;
end;




procedure TSampleLoader.FreeTaskControl(const task: IOmniTaskControl);
// NOTE: FreeTaskControl() is excuted by the main thread.
// This method is called once Task_LoadSample() finishes. It clears
// the task control variable (LoadSampleTask) so that more samples may be loaded. 
begin
  IsBusyLock.Enter;
  try
    LoadSampleTask := nil;
  finally
    IsBusyLock.Leave;
  end;


  //Check for new file in the load queue. 
  if NextFileName <> '' then
  begin
    self.LoadSample(NextFileName, NextFormat, NextLoadDelay, NextSampleID);
  end;


  SiAuto.SiMain.LogMessage('FreeTaskControl_finish');

end;

procedure TSampleLoader.Task_LoadSample(const task: IOmniTask);
// NOTE: Task_LoadSample() is always executed by the worker thread.
var
  CancelLoad : boolean;
  msg:TOmniMessage;
  LoadSampleResult:TLoadSampleResult;
  IsSuccess : boolean;

  Local_FileName  : TOmniValue;
  Local_Format    : TSampleDataFormat;
  Local_SampleID  : integer;
  Local_LoadDelay : integer;
  FormatAsString  : string;
begin

  siAuto.SiMain.LogMessage('msg_TaskLoadSample_started');
  //Init variables...
  IsSuccess        := false;
  CancelLoad       := false;
  LoadSampleResult := nil;


  Local_FileName  := Task.ParamByName['FileName'];
  Local_LoadDelay := Task.ParamByName['LoadDelay'];
  Local_SampleID  := Task.ParamByName['SampleID'];
  FormatAsString  := Task.ParamByName['Format'];

  if FormatAsString = 'dfFloat' then Local_Format := dfFloat
  else
  if FormatAsString = 'dfInt'   then Local_Format := dfInt
  else
  begin
    raise Exception.Create('Error: Format type not handled.');
  end;


  //== Pause before loading the sample ====
  Sleep(Local_LoadDelay + 5);

  //== After pausing, check if sample loading has been canceled. ====
  while (Task.Comm.Receive(msg)) do
  begin
   if (msg.MsgID = mID_CancelLoadSample) then CancelLoad := true;
  end;



  //=== if not canceled, load the sample.... ================
  if CancelLoad = false then
  begin
    LoadSampleResult := TLoadSampleResult.Create;
    try
      if Local_Format = dfInt
        then LoadSampleResult.Sample := TSampleInt.Create
        else LoadSampleResult.Sample := TSampleFloat.Create;

      LoadSampleResult.SampleFormat := Local_Format;
      LoadSampleResult.SampleID     := Local_SampleID;

      IsSuccess := LoadSampleResult.Sample.LoadFromFile(Local_FileName);

      LoadSampleResult.IsSuccess := IsSuccess;
    except
      //if an exception is raised during sample load, free LoadSampleResult class and make moves to get out...
      IsSuccess := false;
      FreeAndNil(LoadSampleResult);
    end;
  end;


  //Check if any cancel load messages have been sent whilst loading the sample.
  while (Task.Comm.Receive(msg)) do
  begin
   if (msg.MsgID = mID_CancelLoadSample) then CancelLoad := true;
  end;




  if (CancelLoad = false) and (IsSuccess) and (assigned(OnLoadSample)) then
  begin
    // If sample loading was successful, report so the calling code can do something..
    // WARNING: This passes ownership of the LoadSampleResult class to the other thread.
    //OnLoadSample(self, LoadSampleResult);
    msg.MsgID            := mID_LoadFinished;
    msg.MsgData.AsObject := LoadSampleResult;
    Task.Comm.Send(msg);

  end else
  begin
    //=== Clean up.... =====
    if assigned(LoadSampleResult) then FreeAndNil(LoadSampleResult);
  end;


  siAuto.SiMain.LogMessage('msg_TaskLoadSample_finished.');
end;

procedure TSampleLoader.Task_MessageHandler(const task: IOmniTaskControl;  const msg: TOmniMessage);
// NOTE: Task_MessageHandler is always executed in the Main thread, not the worker thread.
// This is important, because from here we can communicate with the Main thread while maintaining thread saftey.
var
  LoadSampleResult:TLoadSampleResult;
begin
  SiAuto.SiMain.LogMessage('Task_MessageHandler_start');
  if msg.MsgID = mID_LoadFinished then
  begin
    LoadSampleResult := msg.MsgData.AsObject as TLoadSampleResult;

    if assigned(OnLoadSample) then OnLoadSample(self, LoadSampleResult);

    //IMPORTANT: Need to free the LoadSampleResult to finish cleaning up.
    if assigned(LoadSampleResult) then FreeAndNil(LoadSampleResult);
  end;
  SiAuto.SiMain.LogMessage('Task_MessageHandler_finish');
end;

end.
