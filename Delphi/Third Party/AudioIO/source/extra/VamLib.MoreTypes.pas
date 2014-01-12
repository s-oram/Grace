unit VamLib.MoreTypes;

interface

// VamLib.MoreTypes.pas is shared between a few of my projects. It defines a few
// basic types that all my projects use.

type
  PSingle  = ^Single;

  PArrayOfPSingle  = ^TArrayOfPSingle;
  TArrayOfPSingle  = array of PSingle;
  PArrayOfSingle   = ^TArrayOfSingle;
  TArrayOfSingle   = array of single;
  TArrayOfInteger  = array of integer;
  TArrayOfSmallInt = array of smallInt;
  TArrayOfString   = array of string;
  TArrayOfBoolean  = array of boolean;



implementation

end.
