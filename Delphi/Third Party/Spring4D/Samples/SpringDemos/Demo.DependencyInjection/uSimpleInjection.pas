unit uSimpleInjection;

interface

type
  IMakeNoise = interface
    ['{2CBCF71D-C5C0-4642-B47C-E0FC56857DE8}']
    procedure MakeNoise;
  end;

  IPetNoiseMaker = interface
    ['{156FEFE7-1CD1-4D58-995C-3B9D69A05931}']
    procedure MakePetNoises; overload;
    procedure MakePetNoises(aAnimalName: string); overload;
  end;

implementation

uses
  Spring.Container,
  Spring.Services;

type
  TPetNoiseProvider = class(TInterfacedObject, IPetNoiseMaker)
  private
    [Inject('Dog')]
    FPet: IMakeNoise;
  public
    procedure MakePetNoises; overload;
    procedure MakePetNoises(aAnimalName: string); overload;
  end;

  TDog = class(TInterfacedObject, IMakeNoise)
    procedure MakeNoise;
  end;

  TCat = class(TInterfacedObject, IMakeNoise)
    procedure MakeNoise;
  end;

  TCow = class(TInterfacedObject, IMakeNoise)
    procedure MakeNoise;
  end;

{ TDog }

procedure TDog.MakeNoise;
begin
  WriteLn('Woof!!');
end;

{ TCat }

procedure TCat.MakeNoise;
begin
  WriteLn('Meow!!');
end;

{ TCow }

procedure TCow.MakeNoise;
begin
  WriteLn('Moo!');
end;

{ TPetNoiseProvider }

procedure TPetNoiseProvider.MakePetNoises;
begin
  FPet.MakeNoise;
end;

procedure TPetNoiseProvider.MakePetNoises(aAnimalName: string);
begin
  FPet := ServiceLocator.GetService<IMakeNoise>(aAnimalName);
  FPet.MakeNoise;
end;

initialization
  GlobalContainer.RegisterType<TCat>.Implements<IMakeNoise>('Cat');
  GlobalContainer.RegisterType<TDog>.Implements<IMakeNoise>('Dog');
  GlobalContainer.RegisterType<TCow>.Implements<IMakeNoise>('Cow');

  GlobalContainer.RegisterType<TPetNoiseProvider>.Implements<IPetNoiseMaker>;

end.
