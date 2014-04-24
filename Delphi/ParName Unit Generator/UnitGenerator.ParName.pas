unit UnitGenerator.ParName;

interface

type
  TVstParName = record
  const
    VoiceMode = 'Voice Mode';
  end;

  TVstParIndex = record
  const
    VoiceMode = 0;
    VoiceGlide = 1;
    PitchTracking = 2;
    SamplePlaybackType = 3;
    SampleResetClockSource = 4;
    SamplerLoopBounds = 5;
    SamplerLoopMode = 6;
    GrainLoop = 7;
    GrainLength = 8;
    GrainRate = 9;
    GrainPosition = 10;
    OutputGain = 11;
    OutputPan = 12;
    VoicePitchOne = 13;
    VoicePitchTwo = 14;
    AuxALevel = 15;
    AuxBLevel = 16;
    OscShape = 17;
    OscPulseWidth = 18;
    NoiseLevel = 19;
    SampleStart = 20;
    SampleEnd = 21;
    LoopStart = 22;
    LoopEnd = 23;
    AmpAttack = 24;
    AmpHold = 25;
    AmpDecay = 26;
    AmpSustain = 27;
    AmpRelease = 28;
    AmpVelocity = 29;
    FilterAttack = 30;
    FilterHold = 31;
    FilterDecay = 32;
    FilterSustain = 33;
    FilterRelease = 34;
    FilterVelocity = 35;
    FilterRouting = 36;
    FilterOutputBlend = 37;
    Filter1Type = 38;
    Filter2Type = 39;
    Filter1KeyFollow = 40;
    Filter2KeyFollow = 41;
    Filter1Par1 = 42;
    Filter1Par2 = 43;
    Filter1Par3 = 44;
    Filter1Par4 = 45;
    Filter2Par1 = 46;
    Filter2Par2 = 47;
    Filter2Par3 = 48;
    Filter2Par4 = 49;
    Lfo1Shape = 50;
    Lfo2Shape = 51;
    Lfo1FreqMode = 52;
    Lfo2FreqMode = 53;
    Lfo1Par1 = 54;
    Lfo1Par2 = 55;
    Lfo1Par3 = 56;
    Lfo2Par1 = 57;
    Lfo2Par2 = 58;
    Lfo2Par3 = 59;
    Seq1Clock = 60;
    Seq1Direction = 61;
    Seq1Length = 62;
    Seq2Clock = 63;
    Seq2Direction = 64;
    Seq2Length = 65;
    PreviewVolume = 66;
    Preview = 67;
  end;


  //function ParNameToParIndex(const Name : string):integer;
  //function ParIndexToParName(const Name : string):integer;

procedure Generate;

implementation

uses
  SysUtils,
  Classes,
  Dialogs,
  UnitGen.Tools,
  VamLib.Utils;

procedure Generate;
const
  SourceFN : string = 'S:\Delphi\ParName Unit Generator\Source_ParName.txt';
  DestFN1  : string = 'S:\Delphi\ParName Unit Generator\Constants.ParNames.txt';
  DestFN2  : string = 'S:\Delphi\Lucidity\Constants.ParNames.pas';
var
  c1 : integer;
  Names : TStringList;
  Text : TStringList;
  fn : string;
  s : string;
  QuotedStr : string;
begin
  Names := TStringList.Create;
  AutoFree(@Names);

  Text  := TStringList.Create;
  AutoFree(@Text);

  Names.LoadFromFile(SourceFN);

  fn := TrimFileExt(DestFN1);

  TUnitGen.AddUnitHeader(Text, fn);

  //============================================
  //  Add some constants
  Text.Add('const');
  Text.Add('  kParNameCount = ' + IntToStr(Names.Count) + ';');
  TUnitGen.AddEmptyLine(Text);


  //============================================
  Text.Add('type');
  //=== Generate TVSTParIndex constants record ====
  Text.Add('  TVstParIndex = record');
  Text.Add('  const');

  for c1 := 0 to Names.Count-1 do
  begin
    s := '    ' + Names[c1] + ' = ' + IntToStr(c1) + ';';
    Text.Add(s);
  end;

  Text.Add('  end;');
  //============================================
  TUnitGen.AddEmptyLine(Text);


  //=== Generate TVSTParIndex constants record ====
  Text.Add('  TVstParName = record');
  Text.Add('  const');

  for c1 := 0 to Names.Count-1 do
  begin
    s := '    ' + Names[c1] + ' = ''' + Names[c1] + ''';';
    Text.Add(s);
  end;

  Text.Add('  end;');
  //============================================
  TUnitGen.AddEmptyLine(Text);

  Text.Add('function ParNameToParIndex(const Name : string):integer;');
  Text.Add('function ParIndexToParName(const Index : integer):string;');
  TUnitGen.AddEmptyLine(Text);


  //============================================
  //============================================
  TUnitGen.AddImplementationHeader(Text);
  TUnitGen.AddEmptyLine(Text);
  //============================================
  //============================================

  Text.Add('function ParNameToParIndex(const Name : string):integer;');
  Text.Add('begin');

  for c1 := 0 to Names.Count-1 do
  begin
    QuotedStr := '''' + Names[c1] + '''';
    s := '  if Name = ' + QuotedStr + ' then exit(' + IntToStr(c1) + ');';
    Text.Add(s);
  end;
  s := '  result := -1;';
  Text.Add(s);

  Text.Add('end;');

  TUnitGen.AddEmptyLine(Text);

  //============================================

  Text.Add('function ParIndexToParName(const Index : integer):string;');
  Text.Add('begin');

  Text.Add('  case Index of');
  for c1 := 0 to Names.Count-1 do
  begin
    QuotedStr := '''' + Names[c1] + '''';
    s := '    ' + IntToStr(c1) + ': result := ' + QuotedStr + ';';
    Text.Add(s);
  end;
  Text.Add('  else');
  Text.Add('    result := ''''');
  Text.Add('  end;');

  Text.Add('end;');
  TUnitGen.AddEmptyLine(Text);

  TUnitGen.AddUnitFooter(Text);




  //ShowMessage(Text.Text);
  Text.SaveToFile(DestFN1);
  Text.SaveToFile(DestFN2);


end;

end.
