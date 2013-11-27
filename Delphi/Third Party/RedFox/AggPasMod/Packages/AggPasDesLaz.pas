{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit AggPasDesLaz;

interface

uses
  AggDesignTimeColor, AggDesignTimeRegistration, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AggDesignTimeRegistration', @AggDesignTimeRegistration.Register
    );
end;

initialization
  RegisterPackage('AggPasDesLaz', @Register);
end.
