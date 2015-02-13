{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Times;

interface

uses
  SysUtils;

type
  Times = packed record
  private
    fEvaluator: TFunc<Integer, Boolean>;
    fMin, fMax: Integer;
    fMessageFormat: string;
    constructor Create(const evaluator: TFunc<Integer, Boolean>;
      min, max: Integer; const messageFormat: string);
  public
    function Equals(const value: Times): Boolean;
    function ToString(count: Integer): string;
    function Verify(count: Integer): Boolean;

    class function Any: Times; static;
    class function AtLeast(count: Integer): Times; static;
    class function AtLeastOnce: Times; static;
    class function AtMost(count: Integer): Times; static;
    class function AtMostOnce: Times; static;
    class function Between(min, max: Integer): Times; static;
    class function Exactly(count: Integer): Times; static;
    class function Never: Times; static;
    class function Once: Times; static;

    class operator Equal(const left, right: Times): Boolean;
    class operator NotEqual(const left, right: Times): Boolean;
  end;

implementation

uses
  Spring,
  Spring.ResourceStrings;


{$REGION 'Times'}

constructor Times.Create(const evaluator: TFunc<Integer, Boolean>;
  min, max: Integer; const messageFormat: string);
begin
  fEvaluator := evaluator;
  fMin := min;
  fMax := max;
  fMessageFormat := messageFormat;
end;

function Times.Equals(const value: Times): Boolean;
begin
  Result := (Self.fMin = value.fMin) and (Self.fMax = value.fMax);
end;

class operator Times.Equal(const left, right: Times): Boolean;
begin
  Result := left.Equals(right);
end;

class operator Times.NotEqual(const left, right: Times): Boolean;
begin
  Result := not left.Equals(right);
end;

class function Times.Any: Times;
begin
  Result := Times.Create(
    function(n: Integer): Boolean
    begin
      Result := n >= 0;
    end, 0, MaxInt, SNoMatchAny);
end;

class function Times.AtLeast(count: Integer): Times;
begin
  Guard.CheckRangeInclusive(count, 1, MaxInt);
  Result := Times.Create(
    function(n: Integer): Boolean
    begin
      Result := n >= count;
    end, count, MaxInt, SNoMatchAtLeast);
end;

class function Times.AtLeastOnce: Times;
begin
  Result := Times.Create(
    function(n: Integer): Boolean
    begin
      Result := n >= 1;
    end, 1, MaxInt, SNoMatchAtLeastOnce);
end;

class function Times.AtMost(count: Integer): Times;
begin
  Guard.CheckRangeInclusive(count, 0, MaxInt);
  Result := Times.Create(
    function(n: Integer): Boolean
    begin
      Result := (n >= 0) and (n <= count);
    end, 0, count, SNoMatchAtMost);
end;

class function Times.AtMostOnce: Times;
begin
  Result := Times.Create(
    function(n: Integer): Boolean
    begin
      Result := (n >= 0) and (n <= 1);
    end, 0, 1, SNoMatchAtMostOnce);
end;

class function Times.Between(min, max: Integer): Times;
begin
  Guard.CheckRangeInclusive(max, 0, MaxInt);
  Guard.CheckRangeInclusive(min, 0, max);
  Result := Times.Create(
    function(n: Integer): Boolean
    begin
      Result := (n >= min) and (n <= max);
    end, min, max, SNoMatchBetween);
end;

class function Times.Exactly(count: Integer): Times;
begin
  Guard.CheckRangeInclusive(count, 0, MaxInt);
  Result := Times.Create(
    function(n: Integer): Boolean
    begin
      Result := n = count;
    end, count, count, SNoMatchExactly);
end;

class function Times.Never: Times;
begin
  Result := Times.Create(
    function(n: Integer): Boolean
    begin
      Result := n = 0;
    end, 0, 0, SNoMatchNever);
end;

class function Times.Once: Times;
begin
  Result := Times.Create(
    function(n: Integer): Boolean
    begin
      Result := n = 1;
    end, 1, 1, SNoMatchOnce);
end;

function Times.ToString(count: Integer): string;
begin
  Result := Format(fMessageFormat, [fMin, fMax, count]);
end;

function Times.Verify(count: Integer): Boolean;
begin
  Result := fEvaluator(count);
end;

{$ENDREGION}


end.
