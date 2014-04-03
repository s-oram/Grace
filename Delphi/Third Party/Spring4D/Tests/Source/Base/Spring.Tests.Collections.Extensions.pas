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

unit Spring.Tests.Collections.Extensions;

{$I Spring.inc}
{$IFDEF DELPHIXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  TestFramework,
  Generics.Defaults,
  Generics.Collections,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Lists;

type
  TThrowingEnumerable = class sealed(TEnumerableBase<Integer>)
  public
    function GetEnumerator: IEnumerator<Integer>; override;
  end;

  TNonEnumerableList<T> = class(TList<T>)
  public
    function GetEnumerator: IEnumerator<T>; override;
  end;

  TTestCaseHelper = class helper for TTestCase
  protected
    procedure CheckException(AExceptionClass: ExceptionClass; const AMethod: TProc; const msg: string = ''); overload;
    procedure CheckExceptionDeferred(deferredFunction: TFunc<IEnumerable<Integer>, IEnumerable<Integer>>); overload;
  end;

  TTestWhere = class(TTestCase)
  published
    procedure NilSourceThrowsNilArgumentException;
    procedure NilPredicateThrowsNilArgumentException;

    procedure WithIndexNilSourceThrowsNilArgumentException;
    procedure WithIndexNilPredicateThrowsNilArgumentException;

    procedure SimpleFiltering;
    procedure EmptySource;
    procedure ExecutionIsDeferred;

    procedure WithIndexSimpleFiltering;
    procedure WithIndexEmptySource;
    procedure WithIndexExecutionIsDeferred;

    procedure IteratorIsClonedWhenAlreadyStarted;
  end;

  TTestSelect = class(TTestCase)
  published
    procedure NilSourceThrowsNilArgumentException;
    procedure NilPredicateThrowsNilArgumentException;

    procedure WithIndexNilSourceThrowsNilArgumentException;
    procedure WithIndexNilPredicateThrowsNilArgumentException;

    procedure SimpleProjection;
    procedure SimpleProjectionToDifferentType;
    procedure EmptySource;
    procedure ExecutionIsDeferred;

    procedure WithIndexSimpleProjection;
    procedure WithIndexEmptySource;
    procedure WithIndexExecutionIsDeferred;

    procedure SideEffectsInProjection;
  end;

  TTestRange = class(TTestCase)
  published
    procedure NegativeCount;
    procedure CountTooLarge;
    procedure LargeButValidCount;

    procedure ValidRange;
    procedure NegativeRange;
    procedure EmptyRange;
    procedure SingleValueOfMaxInt32;
    procedure EmptyRangeStartingAtMinInt32;
  end;

  TTestConcat = class(TTestCase)
  published
    procedure SimpleConcatenation;

    procedure NullFirstThrowsNullArgumentException;
    procedure NullSecondThrowsNullArgumentException;

    procedure FirstSequenceIsntAccessedBeforeFirstUse;
    procedure SecondSequenceIsntAccessedBeforeFirstUse;
  end;

  TTestSelectMany = class(TTestCase)
  private
    function ToCharArray(const s: string): IEnumerable<Char>;
  published
    procedure SimpleFlatten;
    procedure SimpleFlattenWithIndex;

    procedure FlattenWithProjection;
    procedure FlattenWithProjectionAndIndex;
  end;

  TTestAny = class(TTestCase)
  published
//    procedure NilSourceWithoutPredicate;
//    procedure NilSourceWithPredicate;
    procedure NilPredicate;

    procedure EmptySequenceWithoutPredicate;
    procedure EmptySequenceWithPredicate;

    procedure NonEmptySequenceWithoutPredicate;
    procedure NonEmptySequenceWithPredicateMatchingElement;
    procedure NonEmptySequenceWithPredicateNotMatchingElement;

    procedure SequenceIsNotEvaluatedAfterFirstMatch;
  end;

  TTestAll = class(TTestCase)
  published
//    procedure NilSource;
    procedure NilPredicate;

    procedure EmptySequenceReturnsTrue;

    procedure PredicateMatchingNoElements;
    procedure PredicateMatchingSomeElements;
    procedure PredicateMatchingAllElements;

    procedure SequenceIsNotEvaluatedAfterFirstNonMatch;
  end;

  TTestFirst = class(TTestCase)
  published
//    procedure NilSourceWithoutPredicate;
//    procedure NilSourceWithPredicate;
    procedure NilPredicate;

    procedure EmptySequenceWithoutPredicate;

    procedure SingleElementSequenceWithoutPredicate;
    procedure MultipleElementSequenceWithoutPredicate;
    procedure EmptySequenceWithPredicate;

    procedure SingleElementSequenceWithMatchingPredicate;
    procedure SingleElementSequenceWithNonMatchingPredicate;

    procedure MultipleElementSequenceWithNoPredicateMatches;
    procedure MultipleElementSequenceWithSinglePredicateMatch;
    procedure MultipleElementSequenceWithMultiplePredicateMatches;

    procedure EarlyOutWithoutPredicate;
    procedure EarlyOutWithPredicate;

    procedure ListWithoutPredicateDoesntIterate;
    procedure ListWithPredicateStillIterates;
  end;

  TTestSingle = class(TTestCase)
  published
//    procedure NilSourceWithoutPredicate;
//    procedure NilSourceWithPredicate;
    procedure NilPredicate;

    procedure EmptySequenceWithoutPredicate;

    procedure SingleElementSequenceWithoutPredicate;
    procedure MultipleElementSequenceWithoutPredicate;
    procedure EmptySequenceWithPredicate;

    procedure SingleElementSequenceWithMatchingPredicate;
    procedure SingleElementSequenceWithNonMatchingPredicate;

    procedure MultipleElementSequenceWithNoPredicateMatches;
    procedure MultipleElementSequenceWithSinglePredicateMatch;
    procedure MultipleElementSequenceWithMultiplePredicateMatches;

    procedure EarlyOutWithoutPredicate;
    procedure EarlyOutWithPredicate;

    procedure ListWithoutPredicateDoesntIterate;
    procedure ListWithPredicateStillIterates;
  end;

  TTestLast = class(TTestCase)
  published
//    procedure NilSourceWithoutPredicate;
//    procedure NilSourceWithPredicate;
    procedure NilPredicate;

    procedure EmptySequenceWithoutPredicate;

    procedure SingleElementSequenceWithoutPredicate;
    procedure MultipleElementSequenceWithoutPredicate;
    procedure EmptySequenceWithPredicate;

    procedure MultipleElementSequenceWithNoPredicateMatches;
    procedure MultipleElementSequenceWithSinglePredicateMatch;
    procedure MultipleElementSequenceWithMultiplePredicateMatches;

    procedure ListWithoutPredicateDoesntIterate;
    procedure ListWithPredicateStillIterates;
  end;

  TTestFirstOrDefault = class(TTestCase)
  published
//    procedure NilSourceWithoutPredicate;
//    procedure NilSourceWithPredicate;
    procedure NilPredicate;

    procedure EmptySequenceWithoutPredicate;

    procedure SingleElementSequenceWithoutPredicate;
    procedure MultipleElementSequenceWithoutPredicate;
    procedure EmptySequenceWithPredicate;

    procedure SingleElementSequenceWithMatchingPredicate;
    procedure SingleElementSequenceWithNonMatchingPredicate;

    procedure MultipleElementSequenceWithNoPredicateMatches;
    procedure MultipleElementSequenceWithSinglePredicateMatch;
    procedure MultipleElementSequenceWithMultiplePredicateMatches;

    procedure EarlyOutWithoutPredicate;
    procedure EarlyOutWithPredicate;

    procedure ListWithoutPredicateDoesntIterate;
    procedure ListWithPredicateStillIterates;
  end;

  TTestSingleOrDefault = class(TTestCase)
  published
//    procedure NilSourceWithoutPredicate;
//    procedure NilSourceWithPredicate;
    procedure NilPredicate;

    procedure EmptySequenceWithoutPredicate;

    procedure SingleElementSequenceWithoutPredicate;
    procedure MultipleElementSequenceWithoutPredicate;
    procedure EmptySequenceWithPredicate;

    procedure SingleElementSequenceWithMatchingPredicate;
    procedure SingleElementSequenceWithNonMatchingPredicate;

    procedure MultipleElementSequenceWithNoPredicateMatches;
    procedure MultipleElementSequenceWithSinglePredicateMatch;
    procedure MultipleElementSequenceWithMultiplePredicateMatches;

    procedure EarlyOutWithoutPredicate;
    procedure EarlyOutWithPredicate;

    procedure ListWithoutPredicateDoesntIterate;
    procedure ListWithPredicateStillIterates;
  end;

  TTestLastOrDefault = class(TTestCase)
  published
//    procedure NilSourceWithoutPredicate;
//    procedure NilSourceWithPredicate;
    procedure NilPredicate;

    procedure EmptySequenceWithoutPredicate;

    procedure SingleElementSequenceWithoutPredicate;
    procedure MultipleElementSequenceWithoutPredicate;
    procedure EmptySequenceWithPredicate;

    procedure SingleElementSequenceWithMatchingPredicate;
    procedure SingleElementSequenceWithNonMatchingPredicate;

    procedure MultipleElementSequenceWithNoPredicateMatches;
    procedure MultipleElementSequenceWithSinglePredicateMatch;
    procedure MultipleElementSequenceWithMultiplePredicateMatches;

    procedure ListWithoutPredicateDoesntIterate;
    procedure ListWithPredicateStillIterates;
  end;

  TTestDefaultIfEmpty = class(TTestCase)
  published
    procedure NilSourceNoDefaultValue;
    procedure NilSourceWithDefaultValue;

    procedure EmptySequenceNoDefaultValue;
    procedure EmptySequenceWithDefaultValue;

    procedure NonEmptySequenceNoDefaultValue;
    procedure NonEmptySequenceWithDefaultValue;

    procedure ExecutionIsDeferred;
  end;
(*
  TTestAggregate = class(TTestCase)
  published
    procedure NilSourceUnseeded;
    procedure NilFuncUnseeded;
    procedure UnseededAggregation;

    procedure NilSourceSeeded;
    procedure NilFuncSeeded;
    procedure SeededAggregation;

    procedure NilSourceSeededWithResultSelector;
    procedure NilFuncSeededWithResultSelector;
    procedure NilProjectionSeededWithResultSelector;
    procedure SeededAggregationWithResultSelector;

    procedure DifferentSourceAndAccumulatorTypes;
    procedure EmptySequenceUnseeded;
    procedure EmptySequenceSeeded;
    procedure EmptySequenceSeededWithResultSelector;
    procedure FirstElementOfInputIsUsedAsSeedForUnseededOverload;
  end;
*)
  TTestDistinct = class(TTestCase)
  published
    procedure NilSourceNoComparer;
    procedure NilSourceWithComparer;
//    procedure NilElementsArePassedToComparer;
//    procedure HashSetCopesWithNilElementsIfComparerDoes;

    procedure NoComparerSpecifiedUsesDefault;
    procedure NilComparerUsesDefault;
//    procedure DistinctStringsWithCaseInsensitiveComparer;
//    procedure DistinctStringsCustomComparer;

    procedure ExecutionIsDeferred;
  end;

  TTestUnion = class(TTestCase)
  published
    procedure NilFirstWithoutComparer;
    procedure NilSecondWithoutComparer;
    procedure NilFirstWithComparer;
    procedure NilSecondWithComparer;

    procedure NoComparerSpecified;
    procedure NilComparerSpecified;
    procedure CaseInsensitiveComparerSpecified;

    procedure EmptyFirstSequence;
    procedure EmptySecondSequence;
    procedure TwoEmptySequences;

    procedure FirstSequenceIsNotUsedUntilQueryIsIterated;
    procedure SecondSequenceIsNotUsedUntilFirstIsExhausted;
  end;

  TTestIntersect = class(TTestCase)
  published
    procedure NilFirstWithoutComparer;
    procedure NilSecondWithoutComparer;
    procedure NilFirstWithComparer;
    procedure NilSecondWithComparer;

    procedure NoComparerSpecified;
    procedure NilComparerSpecified;
    procedure CaseInsensitiveComparerSpecified;

    procedure NoSequencesUsedBeforeIteration;

    procedure FirstSequenceOnlyReadAsResultsAreRead;
    procedure SecondSequenceReadFullyOnFirstResultIteration;
  end;

  TTestExcept = class(TTestCase)
  published
    procedure NilFirstWithoutComparer;
    procedure NilSecondWithoutComparer;
    procedure NilFirstWithComparer;
    procedure NilSecondWithComparer;

    procedure NoComparerSpecified;
    procedure NilComparerSpecified;
    procedure CaseInsensitiveComparerSpecified;

    procedure NoSequencesUsedBeforeIteration;

    procedure FirstSequenceOnlyReadAsResultsAreRead;
    procedure SecondSequenceReadFullyOnFirstResultIteration;
  end;

  TTestToLookup = class(TTestCase)
  published
    procedure SourceSequenceIsReadEagerly;

    procedure ChangesToSourceSequenceAfterToLookupAreNotNoticed;

    procedure LookupWithNoComparerOrElementSelector;
    procedure LookupWithComparerButNoElementSelector;
    procedure LookupWithNilComparerButNoElementSelector;
    procedure LookupWithElementSelectorButNoComparer;
    procedure LookupWithComparerAndElementSelector;
  end;

  TTestJoin = class(TTestCase)
  published
    procedure ExecutionIsDeferred;
    procedure OuterSequenceIsStreamed;
    procedure InnerSequenceIsBuffered;

    procedure SimpleJoin;
    procedure DifferentSourceTypes;

//    procedure NilKeys;
  end;

  TTestGroupBy = class(TTestCase)
  published
    procedure ExecutionIsPartiallyDeferred;
    procedure SequenceIsReadFullyBeforeFirstResultReturned;

    procedure SimpleGroupBy;
    procedure GroupByWithElementProjection;
    procedure GroupByWithCollectionProjection;
    procedure GroupByWithElementProjectionAndCollectionProjection;
    procedure ChangesToSourceAreIgnoredInWhileIteratingOverResultsAfterFirstElementRetrieved;

//    procedure NilKeys;
  end;

  TTestGroupJoin = class(TTestCase)
  published
    procedure ExecutionIsDeferred;

    procedure SimpleGroupJoin;
    procedure CustomComparer;
    procedure DifferentSourceTypes;

//    procedure NilKeys;
  end;

  TTestTake = class(TTestCase)
  published
    procedure ExecutionIsDeferred;

//    procedure NilSource;

    procedure NegativeCount;
    procedure ZeroCount;
    procedure CountShorterThanSourceLength;
    procedure CountEqualToSourceLength;
    procedure CountGreaterThanSourceLength;

    procedure OnlyEnumerateTheGivenNumberOfElements;
  end;

  TTestSkip = class(TTestCase)
  published
    procedure ExecutionIsDeferred;

//    procedure NilSource;

    procedure NegativeCount;
    procedure ZeroCount;
    procedure CountShorterThanSourceLength;
    procedure CountEqualToSourceLength;
    procedure CountGreaterThanSourceLength;
  end;

  TTestTakeWhile = class(TTestCase)
  published
    procedure ExecutionIsDeferred;

//    procedure NilSourceNoIndex;
//    procedure NilSourceUsingIndex;
    procedure NilPredicateNoIndex;
    procedure NilPredicateUsingIndex;

    procedure PredicateFailingFirstElement;
    procedure PredicateWithIndexFailingFirstElement;
    procedure PredicateMatchingSomeElements;
    procedure PredicateWithIndexMatchingSomeElements;
    procedure PredicateMatchingAllElements;
    procedure PredicateWithIndexMatchingAllElements;
  end;

  TTestSkipWhile = class(TTestCase)
  published
    procedure ExecutionIsDeferred;

//    procedure NilSourceNoIndex;
//    procedure NilSourceUsingIndex;
    procedure NilPredicateNoIndex;
    procedure NilPredicateUsingIndex;

    procedure PredicateFailingFirstElement;
    procedure PredicateWithIndexFailingFirstElement;
    procedure PredicateMatchingSomeElements;
    procedure PredicateWithIndexMatchingSomeElements;
    procedure PredicateMatchingAllElements;
    procedure PredicateWithIndexMatchingAllElements;
  end;

  TTestOrderBy = class(TTestCase)
  published
    procedure ExecutionIsDeferred;

    procedure NilSourceNoComparer;
    procedure NilKeySelectorNoComparer;
    procedure NilSourceWithComparer;
    procedure NilKeySelectorWithComparer;

    procedure SimpleUniqueKeys;
    procedure OrderingIsStable;

    procedure NilComparerIsDefault;
    procedure CustomComparer;
    procedure KeySelectorIsCalledExactlyOncePerElement;
  end;

  TTestOrderByDescending = class(TTestCase)
  published
    procedure ExecutionIsDeferred;

    procedure NilSourceNoComparer;
    procedure NilKeySelectorNoComparer;
    procedure NilSourceWithComparer;
    procedure NilKeySelectorWithComparer;

    procedure SimpleUniqueKeys;
    procedure OrderingIsStable;

    procedure NilComparerIsDefault;
    procedure CustomComparer;

    procedure CustomExtremeComparer;
  end;

  TTestReverse = class(TTestCase)
  published
    procedure ExecutionIsDeferred;

//    procedure NilSource;
    procedure InputIsBuffered;
    procedure ArraysAreBuffered;
    procedure ReversedRange;
    procedure ReversedList;
    procedure EmptyInput;
  end;
(*
  TTestSumInt32 = class(TTestCase)
  published
    procedure NilSourceNoSelector;
    procedure NilSourceNullableNoSelector;
    procedure NilSourceWithSelector;
    procedure NilSourceNullableWithSelector;
    procedure NilSelector;
    procedure NilSelectorNullable;

    procedure EmptySequence;
//    procedure EmptySequenceNullable;
//    procedure SequenceOfNullsNullable;
    procedure EmptySequenceWithSelector;
//    procedure EmptySequenceNullableWithSelector;
//    procedure SequenceOfNullsNullableWithSelector;

    procedure SimpleSum;
//    procedure SimpleSumNullable;
//    procedure SimpleSumNullableIncludingNulls;
    procedure SimpleSumWithSelector;
//    procedure SimpleSumNullableWithSelector;
//    procedure SimpleSumNullableWithSelectorIncludingNulls;

    procedure NegativeOverflow;
    procedure Overflow;
    procedure OverflowWithSelector;
//    procedure OverflowNullable;
//    procedure OverflowNullableWithSelector;
    procedure OverflowOfComputableSum;
  end;

  TTestSumInt64 = class(TTestCase)
  published
    procedure NilSourceNoSelector;
    procedure NilSourceNullableNoSelector;
    procedure NilSourceWithSelector;
    procedure NilSourceNullableWithSelector;
    procedure NilSelector;
    procedure NilSelectorNullable;

    procedure EmptySequence;
//    procedure EmptySequenceNullable;
//    procedure SequenceOfNullsNullable;
    procedure EmptySequenceWithSelector;
//    procedure EmptySequenceNullableWithSelector;
//    procedure SequenceOfNullsNullableWithSelector;

    procedure SimpleSum;
//    procedure SimpleSumNullable;
//    procedure SimpleSumNullableIncludingNulls;
    procedure SimpleSumWithSelector;
//    procedure SimpleSumNullableWithSelector;
//    procedure SimpleSumNullableWithSelectorIncludingNulls;

    procedure NegativeOverflow;
    procedure Overflow;
    procedure OverflowWithSelector;
//    procedure OverflowNullable;
//    procedure OverflowNullableWithSelector;
    procedure OverflowOfComputableSum;
  end;

  TTestSumSingle = class(TTestCase)
  published
    procedure NilSourceNoSelector;
    procedure NilSourceNullableNoSelector;
    procedure NilSourceWithSelector;
    procedure NilSourceNullableWithSelector;
    procedure NilSelector;
    procedure NilSelectorNullable;

    procedure EmptySequence;
//    procedure EmptySequenceNullable;
//    procedure SequenceOfNullsNullable;
    procedure EmptySequenceWithSelector;
//    procedure EmptySequenceNullableWithSelector;
//    procedure SequenceOfNullsNullableWithSelector;

    procedure SimpleSum;
//    procedure SimpleSumNullable;
//    procedure SimpleSumNullableIncludingNulls;
    procedure SimpleSumWithSelector;
//    procedure SimpleSumNullableWithSelector;
//    procedure SimpleSumNullableWithSelectorIncludingNulls;

    procedure OverflowToNegativeInfinity;
    procedure OverflowToInfinity;
    procedure OverflowToInfinitySingleWithSelector;
//    procedure OverflowToInfinityNullable;
//    procedure OverflowToInfinityNullableWithSelector;
//    procedure NonOverflowOfComputableSum;
//    procedure AccumulatorAccuracyForSingle;
  end;

  TTestSumDouble = class(TTestCase)
  published
    procedure NilSourceNoSelector;
    procedure NilSourceNullableNoSelector;
    procedure NilSourceWithSelector;
    procedure NilSourceNullableWithSelector;
    procedure NilSelector;
    procedure NilSelectorNullable;
  end;

  TTestMinBy = class(TTestCase)
  published
    procedure NilSource;
    procedure NilKeySelector;

    procedure ExecutionIsDeferred;

    procedure EmptySequence;

    procedure SimpleMinBy;
  end;
*)
  TTestMaxBy = class(TTestCase)
  published
    procedure SimpleMaxBy;
  end;

implementation

uses
  Math,
  StrUtils,
  Spring.Collections.Extensions;

const
  MinInt = -2147483648;
  MinInt64 = -9223372036854775808;
  MaxInt64 = 9223372036854775807;

{ TThrowingEnumerable }

function TThrowingEnumerable.GetEnumerator: IEnumerator<Integer>;
begin
  raise EInvalidOperationException.Create('');
end;

{ TNonEnumerableList<T> }

function TNonEnumerableList<T>.GetEnumerator: IEnumerator<T>;
begin
  raise ENotSupportedException.Create('');
end;

{ TTestCaseHelper }

procedure TTestCaseHelper.CheckException(AExceptionClass: ExceptionClass;
  const AMethod: TProc; const msg: string);
begin
  FCheckCalled := True;
  try
    AMethod();
  except
    on e: Exception do
    begin
      if not Assigned(AExceptionClass) then
        raise
      else if not e.ClassType.InheritsFrom(AExceptionClass) then
        FailNotEquals(AExceptionClass.ClassName, e.ClassName, msg, ReturnAddress)
      else
        AExceptionClass := nil;
    end;
  end;
  if Assigned(AExceptionClass) then
    FailNotEquals(AExceptionClass.ClassName, '', msg, ReturnAddress);
end;

procedure TTestCaseHelper.CheckExceptionDeferred(
  deferredFunction: TFunc<IEnumerable<Integer>, IEnumerable<Integer>>);
var
  source: TThrowingEnumerable;
  result: IEnumerable<Integer>;
  iterator: IEnumerator<Integer>;
begin
  source := TThrowingEnumerable.Create;
  result := deferredFunction(source);
  iterator := result.GetEnumerator;

  CheckException(EInvalidOperationException,
    procedure
    begin
      iterator.MoveNext;
    end);
end;

{ TTestWhere }

procedure TTestWhere.EmptySource;
var
  source: IEnumerable<Integer>;
  result: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  result := source.Where(
    function(const x: Integer): Boolean
    begin
      Result := x < 4;
    end);

  CheckTrue(result.EqualsTo([]));
end;

procedure TTestWhere.ExecutionIsDeferred;
begin
  CheckExceptionDeferred(
    function(source: IEnumerable<Integer>): IEnumerable<Integer>
    begin
      Result := source.Where(
        function(const x: Integer): Boolean
        begin
          Result := x > 0;
        end);
    end);
end;

procedure TTestWhere.IteratorIsClonedWhenAlreadyStarted;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
  enumerator1, enumerator2: IEnumerator<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3, 7, 9, 10]);
  query := source.Where(
    function(const x: Integer): Boolean
    begin
      Result := x > 0;
    end);
  enumerator1 := query.GetEnumerator;
  CheckTrue(enumerator1.MoveNext);
  CheckEquals(1, enumerator1.Current);
  enumerator2 := query.GetEnumerator;
  CheckTrue(enumerator2.MoveNext);
  CheckEquals(1, enumerator2.Current);
end;

procedure TTestWhere.NilPredicateThrowsNilArgumentException;
var
  source: IEnumerable<Integer>;
  predicate: TPredicate<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3, 7, 9, 10]);
  predicate := nil;

  CheckException(EArgumentNilException,
    procedure
    begin
      source.Where(predicate);
    end);
end;

procedure TTestWhere.NilSourceThrowsNilArgumentException;
var
  source: IEnumerable<Integer>;
begin
  source := nil;

  CheckException(EArgumentNilException,
    procedure
    begin
      source := TWhereIterator<Integer>.Create(source,
        function(const x: Integer): Boolean
        begin
          Result := x > 5;
        end);
    end);
end;

procedure TTestWhere.SimpleFiltering;
var
  source: IEnumerable<Integer>;
  result: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3, 4, 2, 8, 1]);
  result := source.Where(
    function(const x: Integer): Boolean
    begin
      Result := x < 4;
    end);

  CheckTrue(result.EqualsTo([1, 3, 2, 1]));
end;

procedure TTestWhere.WithIndexEmptySource;
var
  source: IEnumerable<Integer>;
  result: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  result := TWhereIndexIterator<Integer>.Create(source,
    function(x, index: Integer): Boolean
    begin
      Result := x < 4;
    end);

  CheckTrue(result.EqualsTo([]));
end;

procedure TTestWhere.WithIndexExecutionIsDeferred;
begin
  CheckExceptionDeferred(
    function(source: IEnumerable<Integer>): IEnumerable<Integer>
    begin
      Result := TWhereIndexIterator<Integer>.Create(source,
        function(x, index: Integer): Boolean
        begin
          Result := x > 0;
        end);
    end);
end;

procedure TTestWhere.WithIndexNilPredicateThrowsNilArgumentException;
var
  source: IEnumerable<Integer>;
  predicate: TFunc<Integer, Integer, Boolean>;
begin
  source := TCollections.CreateList<Integer>([1, 3, 7, 9, 10]);
  predicate := nil;

  CheckException(EArgumentNilException,
    procedure
    begin
      TWhereIndexIterator<Integer>.Create(source, predicate);
    end);
end;

procedure TTestWhere.WithIndexNilSourceThrowsNilArgumentException;
var
  source: IEnumerable<Integer>;
begin
  source := nil;

  CheckException(EArgumentNilException,
    procedure
    begin
      TWhereIndexIterator<Integer>.Create(source,
        function(x, index: Integer): Boolean
        begin
          Result := x > 5;
        end);
    end);
end;

procedure TTestWhere.WithIndexSimpleFiltering;
var
  source: IEnumerable<Integer>;
  result: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3, 4, 2, 8, 1]);
  result := TWhereIndexIterator<Integer>.Create(source,
    function(x, index: Integer): Boolean
    begin
      Result := x < 4;
    end);

  CheckTrue(result.EqualsTo([1, 3, 2, 1]));
end;

{ TTestSelect }

procedure TTestSelect.EmptySource;
var
  source: IEnumerable<Integer>;
  result: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  result := TSelectIterator<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := x * 2;
    end);

  CheckTrue(result.EqualsTo([]));
end;

procedure TTestSelect.ExecutionIsDeferred;
begin
  CheckExceptionDeferred(
    function(source: IEnumerable<Integer>): IEnumerable<Integer>
    begin
      Result := TSelectIterator<Integer, Integer>.Create(source,
        function(x: Integer): Integer
        begin
          Result := x * 2;
        end);
    end);
end;

procedure TTestSelect.NilPredicateThrowsNilArgumentException;
var
  source: IEnumerable<Integer>;
  projection: TFunc<Integer, Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3, 7, 9, 10]);
  projection  := nil;

  CheckException(EArgumentNilException,
    procedure
    begin
      TSelectIterator<Integer, Integer>.Create(source, projection);
    end);
end;

procedure TTestSelect.NilSourceThrowsNilArgumentException;
var
  source: IEnumerable<Integer>;
begin
  source := nil;

  CheckException(EArgumentNilException,
    procedure
    begin
      TSelectIterator<Integer, Integer>.Create(source,
        function(x: Integer): Integer
        begin
          Result := x + 1;
        end);
    end);
end;

procedure TTestSelect.SideEffectsInProjection;
var
  source: IEnumerable<Integer>;
  count: Integer;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 3]);
  count := 0;
  query := TSelectIterator<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := count;
      Inc(count);
    end);

  CheckTrue(query.EqualsTo([0, 1, 2]));
  CheckTrue(query.EqualsTo([3, 4, 5]));
  count := 10;
  CheckTrue(query.EqualsTo([10, 11, 12]));
end;

procedure TTestSelect.SimpleProjection;
var
  source: IEnumerable<Integer>;
  result: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 5, 2]);
  result := TSelectIterator<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := x * 2;
    end);

  CheckTrue(result.EqualsTo([2, 10, 4]));
end;

procedure TTestSelect.SimpleProjectionToDifferentType;
var
  source: IEnumerable<Integer>;
  result: IEnumerable<string>;
begin
  source := TCollections.CreateList<Integer>([1, 5, 2]);
  result := TSelectIterator<Integer, string>.Create(source,
    function(x: Integer): string
    begin
      Result := IntToStr(x);
    end);

  CheckTrue(result.EqualsTo(['1', '5', '2']));
end;

procedure TTestSelect.WithIndexEmptySource;
var
  source: IEnumerable<Integer>;
  result: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  result := TSelectIndexIterator<Integer, Integer>.Create(source,
    function(x, index: Integer): Integer
    begin
      Result := x + index;
    end);

  CheckTrue(result.EqualsTo([]));
end;

procedure TTestSelect.WithIndexExecutionIsDeferred;
begin
  CheckExceptionDeferred(
    function(source: IEnumerable<Integer>): IEnumerable<Integer>
    begin
      Result := TSelectIndexIterator<Integer, Integer>.Create(source,
        function(x, index: Integer): Integer
        begin
          Result := x + index;
        end);
    end);
end;

procedure TTestSelect.WithIndexNilPredicateThrowsNilArgumentException;
var
  source: IEnumerable<Integer>;
  projection: TFunc<Integer, Integer, Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3, 7, 9, 10]);
  projection  := nil;

  CheckException(EArgumentNilException,
    procedure
    begin
      TSelectIndexIterator<Integer, Integer>.Create(source, projection);
    end);
end;

procedure TTestSelect.WithIndexNilSourceThrowsNilArgumentException;
var
  source: IEnumerable<Integer>;
begin
  source := nil;

  CheckException(EArgumentNilException,
    procedure
    begin
      TSelectIndexIterator<Integer, Integer>.Create(source,
        function(x, index: Integer): Integer
        begin
          Result := x + index;
        end);
    end);
end;

procedure TTestSelect.WithIndexSimpleProjection;
var
  source: IEnumerable<Integer>;
  result: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 5, 2]);
  result := TSelectIndexIterator<Integer, Integer>.Create(source,
    function(x, index: Integer): Integer
    begin
      Result := x + index * 10;
    end);

  CheckTrue(result.EqualsTo([1, 15, 22]));
end;

{ TTestRange }

procedure TTestRange.CountTooLarge;
begin
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      TCollections.Range(MaxInt, 2);
    end);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      TCollections.Range(2, MaxInt);
    end);
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      TCollections.Range(MaxInt div 2, (MaxInt div 2) + 3);
    end);
end;

procedure TTestRange.EmptyRange;
begin
  CheckTrue(TCollections.Range(100, 0).EqualsTo([]));
end;

procedure TTestRange.EmptyRangeStartingAtMinInt32;
begin
  CheckTrue(TCollections.Range(MinInt, 0).EqualsTo([]));
end;

procedure TTestRange.LargeButValidCount;
begin
  TCollections.Range(MaxInt, 1);
  TCollections.Range(1, MaxInt);
  TCollections.Range(MaxInt div 2, (MaxInt div 2) + 2);
end;

procedure TTestRange.NegativeCount;
begin
  CheckException(EArgumentOutOfRangeException,
    procedure
    begin
      TCollections.Range(10, -1);
    end);
end;

procedure TTestRange.NegativeRange;
begin
  CheckTrue(TCollections.Range(-2, 5).EqualsTo([-2, -1, 0, 1, 2]));
end;

procedure TTestRange.SingleValueOfMaxInt32;
begin
  CheckTrue(TCollections.Range(MaxInt, 1).EqualsTo([MaxInt]));
end;

procedure TTestRange.ValidRange;
begin
  CheckTrue(TCollections.Range(5, 3).EqualsTo([5, 6, 7]));
end;

{ TTestConcat }

procedure TTestConcat.FirstSequenceIsntAccessedBeforeFirstUse;
var
  first: IEnumerable<Integer>;
  second: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
  iterator: IEnumerator<Integer>;
begin
  first := TThrowingEnumerable.Create;
  second := TCollections.CreateList<Integer>([5]);
  query := first.Concat(second);
  iterator := query.GetEnumerator;
  CheckException(EInvalidOperationException,
    procedure
    begin
      iterator.MoveNext;
    end);
end;

procedure TTestConcat.NullFirstThrowsNullArgumentException;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
begin
  first := nil;
  second := TCollections.CreateList<string>(['hello']);

  CheckException(EArgumentNilException,
    procedure
    begin
      TConcatIterator<string>.Create(first, second);
    end);
end;

procedure TTestConcat.NullSecondThrowsNullArgumentException;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>(['hello']);
  second := nil;

  CheckException(EArgumentNilException,
    procedure
    begin
      first.Concat(second);
    end);
end;

procedure TTestConcat.SecondSequenceIsntAccessedBeforeFirstUse;
var
  first: IEnumerable<Integer>;
  second: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
  iterator: IEnumerator<Integer>;
begin
  first := TCollections.CreateList<Integer>([5]);
  second := TThrowingEnumerable.Create;
  query := first.Concat(second);
  iterator := query.GetEnumerator;
  CheckTrue(iterator.MoveNext);
  CheckEquals(5, iterator.Current);
  CheckException(EInvalidOperationException,
    procedure
    begin
      iterator.MoveNext;
    end);
end;

procedure TTestConcat.SimpleConcatenation;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>(['a', 'b']);
  second := TCollections.CreateList<string>(['c', 'd']);

  CheckTrue(first.Concat(second).EqualsTo(['a', 'b', 'c', 'd']));
end;

{ TTestSelectMany }

procedure TTestSelectMany.FlattenWithProjection;
var
  numbers: IEnumerable<Integer>;
  query: IEnumerable<string>;
begin
  numbers := TCollections.CreateList<Integer>([3, 5, 20, 15]);
  query := TSelectManyIterator<Integer, Char, string>.Create(numbers,
    function(x: Integer): IEnumerable<Char>
    begin
      Result := ToCharArray(IntToStr(x));
    end,
    function(x: Integer; c: Char): string
    begin
      Result := IntToStr(x) + ': ' + c;
    end);

  CheckTrue(query.EqualsTo(['3: 3', '5: 5', '20: 2', '20: 0', '15: 1', '15: 5']));
end;

procedure TTestSelectMany.FlattenWithProjectionAndIndex;
var
  numbers: IEnumerable<Integer>;
  query: IEnumerable<string>;
begin
  numbers := TCollections.CreateList<Integer>([3, 5, 20, 15]);
  query := TSelectManyIndexIterator<Integer, Char, string>.Create(numbers,
    function(x, index: Integer): IEnumerable<Char>
    begin
      Result := ToCharArray(IntToStr(x + index));
    end,
    function(x: Integer; c: Char): string
    begin
      Result := IntToStr(x) + ': ' + c;
    end);

  CheckTrue(query.EqualsTo(['3: 3', '5: 6', '20: 2', '20: 2', '15: 1', '15: 8']));
end;

procedure TTestSelectMany.SimpleFlatten;
var
  numbers: IEnumerable<Integer>;
  query: IEnumerable<Char>;
begin
  numbers := TCollections.CreateList<Integer>([3, 5, 20, 15]);
  query := TSelectManyIterator<Integer, Char>.Create(numbers,
    function(x: Integer): IEnumerable<Char>
    begin
      Result := ToCharArray(IntToStr(x));
    end);

  CheckTrue(query.EqualsTo(['3', '5', '2', '0', '1', '5']));
end;

procedure TTestSelectMany.SimpleFlattenWithIndex;
var
  numbers: IEnumerable<Integer>;
  query: IEnumerable<Char>;
begin
  numbers := TCollections.CreateList<Integer>([3, 5, 20, 15]);
  query := TSelectManyIndexIterator<Integer, Char>.Create(numbers,
    function(x, index: Integer): IEnumerable<Char>
    begin
      Result := ToCharArray(IntToStr(x + index));
    end);

  CheckTrue(query.EqualsTo(['3', '6', '2', '2', '1', '8']));
end;

function TTestSelectMany.ToCharArray(const s: string): IEnumerable<Char>;
var
  values: TArray<Char>;
  i: Integer;
begin
  SetLength(values, Length(s));
  for i := 0 to Length(s) - 1 do
    values[i] := s[i + 1];
  Result := TArrayIterator<Char>.Create(values);
end;

{ TTestAny }

procedure TTestAny.EmptySequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>();
  CheckFalse(source.Any);
end;

procedure TTestAny.EmptySequenceWithPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>();
  CheckFalse(source.Any(
    function(const x: Integer): Boolean
    begin
      Result := x > 10;
    end));
end;

procedure TTestAny.NilPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3, 5]);
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Any(nil);
    end);
end;

(*
procedure TTestAny.NilSourceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.Any;
    end);
end;

procedure TTestAny.NilSourceWithPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.Any(
        function(const x: Integer): Boolean
        begin
          Result := x > 10;
        end);
    end);
end;
*)

procedure TTestAny.NonEmptySequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1]);
  CheckTrue(source.Any);
end;

procedure TTestAny.NonEmptySequenceWithPredicateMatchingElement;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 5, 20, 30]);
  CheckTrue(source.Any(
    function(const x: Integer): Boolean
    begin
      Result := x > 10;
    end));
end;

procedure TTestAny.NonEmptySequenceWithPredicateNotMatchingElement;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 5, 8, 9]);
  CheckFalse(source.Any(
    function(const x: Integer): Boolean
    begin
      Result := x > 10;
    end));
end;

procedure TTestAny.SequenceIsNotEvaluatedAfterFirstMatch;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([10, 2, 0, 3]);
  query := TSelectIterator<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  CheckTrue(query.Any(
    function(const y: Integer): Boolean
    begin
      Result := y > 2;
    end));
end;

{ TTestAll }

procedure TTestAll.EmptySequenceReturnsTrue;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckTrue(source.All(
    function(const x: Integer): Boolean
    begin
      Result := x > 0;
    end));
end;

procedure TTestAll.NilPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3, 5]);
  CheckException(EArgumentNilException,
    procedure
    begin
      source.All(nil);
    end);
end;

(*
procedure TTestAll.NilSource;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.All(
        function(const x: Integer): Boolean
        begin
          Result := x > 10;
        end);
    end);
end;
*)

procedure TTestAll.PredicateMatchingAllElements;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 5, 8, 9]);
  CheckTrue(source.All(
    function(const x: Integer): Boolean
    begin
      Result := x > 0;
    end));
end;

procedure TTestAll.PredicateMatchingNoElements;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 5, 20, 30]);
  CheckFalse(source.All(
    function(const x: Integer): Boolean
    begin
      Result := x < 0;
    end));
end;

procedure TTestAll.PredicateMatchingSomeElements;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 5, 8, 9]);
  CheckFalse(source.All(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestAll.SequenceIsNotEvaluatedAfterFirstNonMatch;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([2, 10, 0, 3]);
  query := TSelectIterator<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  CheckFalse(query.All(
    function(const y: Integer): Boolean
    begin
      Result := y > 2;
    end));
end;

{ TTestFirst }

procedure TTestFirst.EarlyOutWithoutPredicate;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([15, 1, 0, 3]);
  query := TSelectIterator<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  CheckEquals(0, query.First);
end;

procedure TTestFirst.EarlyOutWithPredicate;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([15, 1, 0, 3]);
  query := TSelectIterator<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  CheckEquals(10, query.First(
    function(const y: Integer): Boolean
    begin
      Result := y > 5;
    end));
end;

procedure TTestFirst.EmptySequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckException(EInvalidOperationException,
    procedure
    begin
      source.First;
    end);
end;

procedure TTestFirst.EmptySequenceWithPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckException(EInvalidOperationException,
    procedure
    begin
      source.First(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;

procedure TTestFirst.ListWithoutPredicateDoesntIterate;
var
  source: IEnumerable<Integer>;
begin
  source := TNonEnumerableList<Integer>.Create([1, 5, 10, 3]);
  CheckEquals(1, source.First);
end;

procedure TTestFirst.ListWithPredicateStillIterates;
var
  source: IEnumerable<Integer>;
begin
  source := TNonEnumerableList<Integer>.Create([1, 5, 10, 3]);
  CheckException(ENotSupportedException,
    procedure
    begin
      source.First(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;

procedure TTestFirst.MultipleElementSequenceWithMultiplePredicateMatches;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 5, 10, 2, 1]);
  CheckEquals(5, source.First(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestFirst.MultipleElementSequenceWithNoPredicateMatches;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 2, 1]);
  CheckException(EInvalidOperationException,
    procedure
    begin
      source.First(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;

procedure TTestFirst.MultipleElementSequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5, 10]);
  CheckEquals(5, source.First);
end;

procedure TTestFirst.MultipleElementSequenceWithSinglePredicateMatch;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 5, 2, 1]);
  CheckEquals(5, source.First(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestFirst.NilPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3, 5]);
  CheckException(EArgumentNilException,
    procedure
    begin
      source.First(nil);
    end);
end;

(*
procedure TTestFirst.NilSourceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.First;
    end);
end;

procedure TTestFirst.NilSourceWithPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.First(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;
*)

procedure TTestFirst.SingleElementSequenceWithMatchingPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5]);
  CheckEquals(5, source.First(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestFirst.SingleElementSequenceWithNonMatchingPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([2]);
  CheckException(EInvalidOperationException,
    procedure
    begin
      source.First(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;

procedure TTestFirst.SingleElementSequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5]);
  CheckEquals(5, source.First);
end;

{ TTestSingle }

procedure TTestSingle.EarlyOutWithoutPredicate;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 0]);
  query := TSelectIterator<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  CheckException(EInvalidOperationException,
    procedure
    begin
      query.Single;
    end);
  query := nil;
end;

procedure TTestSingle.EarlyOutWithPredicate;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 0]);
  query := TSelectIterator<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  CheckException(EInvalidOperationException,
    procedure
    begin
      query.Single(
        function(const x: Integer): Boolean
        begin
          Result := True;
        end);
    end);
  query := nil;
end;

procedure TTestSingle.EmptySequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckException(EInvalidOperationException,
    procedure
    begin
      source.Single;
    end);
end;

procedure TTestSingle.EmptySequenceWithPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckException(EInvalidOperationException,
    procedure
    begin
      source.Single(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;

procedure TTestSingle.ListWithoutPredicateDoesntIterate;
var
  source: IEnumerable<Integer>;
begin
  source := TNonEnumerableList<Integer>.Create([1]);
  CheckEquals(1, source.Single);
end;

procedure TTestSingle.ListWithPredicateStillIterates;
var
  source: IEnumerable<Integer>;
begin
  source := TNonEnumerableList<Integer>.Create([1, 5, 10, 3]);
  CheckException(ENotSupportedException,
    procedure
    begin
      source.Single(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;

procedure TTestSingle.MultipleElementSequenceWithMultiplePredicateMatches;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 5, 10, 2, 1]);
  CheckException(EInvalidOperationException,
    procedure
    begin
      source.Single(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end)
    end);
end;

procedure TTestSingle.MultipleElementSequenceWithNoPredicateMatches;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 2, 1]);
  CheckException(EInvalidOperationException,
    procedure
    begin
      source.Single(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;

procedure TTestSingle.MultipleElementSequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5, 10]);
  CheckException(EInvalidOperationException,
    procedure
    begin
      source.Single;
    end);
end;

procedure TTestSingle.MultipleElementSequenceWithSinglePredicateMatch;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 5, 2, 1]);
  CheckEquals(5, source.Single(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestSingle.NilPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3, 5]);
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Single(nil);
    end);
end;

(*
procedure TTestSingle.NilSourceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.Single;
    end);
end;

procedure TTestSingle.NilSourceWithPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.Single(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;
*)

procedure TTestSingle.SingleElementSequenceWithMatchingPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5]);
  CheckEquals(5, source.Single(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestSingle.SingleElementSequenceWithNonMatchingPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([2]);
  CheckException(EInvalidOperationException,
    procedure
    begin
      source.Single(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;

procedure TTestSingle.SingleElementSequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5]);
  CheckEquals(5, source.Single);
end;

{ TTestLast }

procedure TTestLast.EmptySequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckException(EInvalidOperationException,
    procedure
    begin
      source.Last;
    end);
end;

procedure TTestLast.EmptySequenceWithPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckException(EInvalidOperationException,
    procedure
    begin
      source.Last(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;

procedure TTestLast.ListWithoutPredicateDoesntIterate;
var
  source: IEnumerable<Integer>;
begin
  source := TNonEnumerableList<Integer>.Create([1, 5, 10, 3]);
  CheckEquals(3, source.Last);
end;

procedure TTestLast.ListWithPredicateStillIterates;
var
  source: IEnumerable<Integer>;
begin
  source := TNonEnumerableList<Integer>.Create([1, 5, 10, 3]);
  CheckException(ENotSupportedException,
    procedure
    begin
      source.Last(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;

procedure TTestLast.MultipleElementSequenceWithMultiplePredicateMatches;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 5, 10, 2, 1]);
  CheckEquals(10, source.Last(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestLast.MultipleElementSequenceWithNoPredicateMatches;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 2, 1]);
  CheckException(EInvalidOperationException,
    procedure
    begin
      source.Last(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;

procedure TTestLast.MultipleElementSequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5, 10]);
  CheckEquals(10, source.Last);
end;

procedure TTestLast.MultipleElementSequenceWithSinglePredicateMatch;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 5, 2, 1]);
  CheckEquals(5, source.Last(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestLast.NilPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3, 5]);
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Last(nil);
    end);
end;

(*
procedure TTestLast.NilSourceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.Last;
    end);
end;

procedure TTestLast.NilSourceWithPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.Last(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;
*)

procedure TTestLast.SingleElementSequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5]);
  CheckEquals(5, source.Last);
end;

{ TTestFirstOrDefault }

procedure TTestFirstOrDefault.EarlyOutWithoutPredicate;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([15, 1, 0, 3]);
  query := TSelectIterator<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  CheckEquals(0, query.FirstOrDefault);
end;

procedure TTestFirstOrDefault.EarlyOutWithPredicate;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([15, 1, 0, 3]);
  query := TSelectIterator<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  CheckEquals(10, query.FirstOrDefault(
    function(const y: Integer): Boolean
    begin
      Result := y > 5;
    end));
end;

procedure TTestFirstOrDefault.EmptySequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckEquals(0, source.FirstOrDefault);
end;

procedure TTestFirstOrDefault.EmptySequenceWithPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckEquals(0, source.FirstOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestFirstOrDefault.ListWithoutPredicateDoesntIterate;
var
  source: IEnumerable<Integer>;
begin
  source := TNonEnumerableList<Integer>.Create([1, 5, 10, 3]);
  CheckEquals(1, source.FirstOrDefault);
end;

procedure TTestFirstOrDefault.ListWithPredicateStillIterates;
var
  source: IEnumerable<Integer>;
begin
  source := TNonEnumerableList<Integer>.Create([1, 5, 10, 3]);
  CheckException(ENotSupportedException,
    procedure
    begin
      source.FirstOrDefault(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;

procedure TTestFirstOrDefault.MultipleElementSequenceWithMultiplePredicateMatches;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 5, 10, 2, 1]);
  CheckEquals(5, source.FirstOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestFirstOrDefault.MultipleElementSequenceWithNoPredicateMatches;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 2, 1]);
  CheckEquals(0, source.FirstOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestFirstOrDefault.MultipleElementSequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5, 10]);
  CheckEquals(5, source.FirstOrDefault);
end;

procedure TTestFirstOrDefault.MultipleElementSequenceWithSinglePredicateMatch;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 5, 2, 1]);
  CheckEquals(5, source.FirstOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestFirstOrDefault.NilPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3, 5]);
  CheckException(EArgumentNilException,
    procedure
    begin
      source.FirstOrDefault(nil);
    end);
end;

(*
procedure TTestFirstOrDefault.NilSourceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.FirstOrDefault;
    end);
end;

procedure TTestFirstOrDefault.NilSourceWithPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.FirstOrDefault(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;
*)

procedure TTestFirstOrDefault.SingleElementSequenceWithMatchingPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5]);
  CheckEquals(5, source.FirstOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestFirstOrDefault.SingleElementSequenceWithNonMatchingPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([2]);
  CheckEquals(0, source.FirstOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestFirstOrDefault.SingleElementSequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5]);
  CheckEquals(5, source.FirstOrDefault);
end;

{ TTestSingleOrDefault }

procedure TTestSingleOrDefault.EarlyOutWithoutPredicate;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 0]);
  query := TSelectIterator<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  CheckException(EInvalidOperationException,
    procedure
    begin
      query.SingleOrDefault;
    end);
  query := nil;
end;

procedure TTestSingleOrDefault.EarlyOutWithPredicate;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 0]);
  query := TSelectIterator<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  CheckException(EInvalidOperationException,
    procedure
    begin
      query.SingleOrDefault(
        function(const x: Integer): Boolean
        begin
          Result := True;
        end);
    end);
  query := nil;
end;

procedure TTestSingleOrDefault.EmptySequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckEquals(0, source.SingleOrDefault);
end;

procedure TTestSingleOrDefault.EmptySequenceWithPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckEquals(0, source.SingleOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestSingleOrDefault.ListWithoutPredicateDoesntIterate;
var
  source: IEnumerable<Integer>;
begin
  source := TNonEnumerableList<Integer>.Create([1]);
  CheckEquals(1, source.SingleOrDefault);
end;

procedure TTestSingleOrDefault.ListWithPredicateStillIterates;
var
  source: IEnumerable<Integer>;
begin
  source := TNonEnumerableList<Integer>.Create([1, 5, 10, 3]);
  CheckException(ENotSupportedException,
    procedure
    begin
      source.SingleOrDefault(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;

procedure TTestSingleOrDefault.MultipleElementSequenceWithMultiplePredicateMatches;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 5, 10, 2, 1]);
  CheckException(EInvalidOperationException,
    procedure
    begin
      source.SingleOrDefault(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end)
    end);
end;

procedure TTestSingleOrDefault.MultipleElementSequenceWithNoPredicateMatches;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 2, 1]);
  CheckEquals(0, source.SingleOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestSingleOrDefault.MultipleElementSequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5, 10]);
  CheckException(EInvalidOperationException,
    procedure
    begin
      source.SingleOrDefault;
    end);
end;

procedure TTestSingleOrDefault.MultipleElementSequenceWithSinglePredicateMatch;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 5, 2, 1]);
  CheckEquals(5, source.SingleOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestSingleOrDefault.NilPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3, 5]);
  CheckException(EArgumentNilException,
    procedure
    begin
      source.SingleOrDefault(nil);;
    end);
end;

(*
procedure TTestSingleOrDefault.NilSourceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.SingleOrDefault;
    end);
end;

procedure TTestSingleOrDefault.NilSourceWithPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.SingleOrDefault(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;
*)

procedure TTestSingleOrDefault.SingleElementSequenceWithMatchingPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5]);
  CheckEquals(5, source.SingleOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestSingleOrDefault.SingleElementSequenceWithNonMatchingPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([2]);
  CheckEquals(0, source.SingleOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestSingleOrDefault.SingleElementSequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5]);
  CheckEquals(5, source.SingleOrDefault);
end;

{ TTestLastOrDefault }

procedure TTestLastOrDefault.EmptySequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckEquals(0, source.LastOrDefault);
end;

procedure TTestLastOrDefault.EmptySequenceWithPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckEquals(0, source.LastOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestLastOrDefault.ListWithoutPredicateDoesntIterate;
var
  source: IEnumerable<Integer>;
begin
  source := TNonEnumerableList<Integer>.Create([1, 5, 10, 3]);
  CheckEquals(3, source.LastOrDefault);
end;

procedure TTestLastOrDefault.ListWithPredicateStillIterates;
var
  source: IEnumerable<Integer>;
begin
  source := TNonEnumerableList<Integer>.Create([1, 5, 10, 3]);
  CheckException(ENotSupportedException,
    procedure
    begin
      source.LastOrDefault(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;

procedure TTestLastOrDefault.MultipleElementSequenceWithMultiplePredicateMatches;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 5, 10, 2, 1]);
  CheckEquals(10, source.LastOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestLastOrDefault.MultipleElementSequenceWithNoPredicateMatches;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 2, 1]);
  CheckEquals(0, source.LastOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestLastOrDefault.MultipleElementSequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5, 10]);
  CheckEquals(10, source.LastOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestLastOrDefault.MultipleElementSequenceWithSinglePredicateMatch;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 5, 2, 1]);
  CheckEquals(5, source.LastOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestLastOrDefault.NilPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3, 5]);
  CheckException(EArgumentNilException,
    procedure
    begin
      source.LastOrDefault(nil);
    end);
end;

(*
procedure TTestLastOrDefault.NilSourceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.LastOrDefault;
    end);
end;

procedure TTestLastOrDefault.NilSourceWithPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.LastOrDefault(
        function(const x: Integer): Boolean
        begin
          Result := x > 3;
        end);
    end);
end;
*)

procedure TTestLastOrDefault.SingleElementSequenceWithMatchingPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5]);
  CheckEquals(5, source.LastOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestLastOrDefault.SingleElementSequenceWithNonMatchingPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([2]);
  CheckEquals(0, source.LastOrDefault(
    function(const x: Integer): Boolean
    begin
      Result := x > 3;
    end));
end;

procedure TTestLastOrDefault.SingleElementSequenceWithoutPredicate;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5]);
  CheckEquals(5, source.LastOrDefault);
end;

{ TTestDefaultIfEmpty }

procedure TTestDefaultIfEmpty.NilSourceWithDefaultValue;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      TDefaultIfEmptyIterator<Integer>.Create(source, 5);
    end);
end;

procedure TTestDefaultIfEmpty.EmptySequenceNoDefaultValue;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  query := TDefaultIfEmptyIterator<Integer>.Create(source, Default(Integer));
  CheckTrue(query.EqualsTo([0]));
end;

procedure TTestDefaultIfEmpty.EmptySequenceWithDefaultValue;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  query := TDefaultIfEmptyIterator<Integer>.Create(source, 5);
  CheckTrue(query.EqualsTo([5]));
end;

procedure TTestDefaultIfEmpty.ExecutionIsDeferred;
begin
  CheckExceptionDeferred(
    function(source: IEnumerable<Integer>): IEnumerable<Integer>
    begin
      Result := TDefaultIfEmptyIterator<Integer>.Create(source, Default(Integer));
    end);
end;

procedure TTestDefaultIfEmpty.NonEmptySequenceNoDefaultValue;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([3, 1, 4]);
  query := TDefaultIfEmptyIterator<Integer>.Create(source, Default(Integer));
  CheckTrue(query.EqualsTo(source));
end;

procedure TTestDefaultIfEmpty.NonEmptySequenceWithDefaultValue;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([3, 1, 4]);
  query := TDefaultIfEmptyIterator<Integer>.Create(source, 5);
  CheckTrue(query.EqualsTo(source));
end;

procedure TTestDefaultIfEmpty.NilSourceNoDefaultValue;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      TDefaultIfEmptyIterator<Integer>.Create(source, Default(Integer));
    end);
end;

{ TTestAggregate }
(*
procedure TTestAggregate.DifferentSourceAndAccumulatorTypes;
const
  largeValue = 2000000000;
var
  source: IEnumerable<Integer>;
  sum: Int64;
begin
  source := TCollections.CreateList<Integer>([largeValue, largeValue, largeValue]);
  sum := source.Aggregate<Int64>(0,
    function(acc: Int64; value: Integer): Int64
    begin
      Result := acc + value;
    end);
  CheckEquals(6000000000, sum);
  CheckTrue(sum > MaxInt);
end;

procedure TTestAggregate.EmptySequenceSeeded;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckEquals(5, source.Aggregate<Integer>(5,
    function(x, y: Integer): Integer
    begin
      Result := x + y;
    end));
end;

procedure TTestAggregate.EmptySequenceSeededWithResultSelector;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckEquals('5', source.Aggregate<Integer, string>(5,
    function(x, y: Integer): Integer
    begin
      Result := x + y;
    end,
    function(x: Integer): string
    begin
      Result := IntToStr(x);
    end));
end;

procedure TTestAggregate.EmptySequenceUnseeded;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckException(EInvalidOperationException,
    procedure
    begin
      source.Aggregate(
        function(x, y: Integer): Integer
        begin
          Result := x + y;
        end);
    end);
end;

procedure TTestAggregate.FirstElementOfInputIsUsedAsSeedForUnseededOverload;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([5, 3, 2]);
  CheckEquals(30, source.Aggregate(
    function(acc, value: Integer): Integer
    begin
      Result := acc * value;
    end));
end;

procedure TTestAggregate.NilFuncSeeded;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3]);
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Aggregate<Integer>(5, nil);
    end);
end;

procedure TTestAggregate.NilFuncSeededWithResultSelector;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3]);
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Aggregate<Integer, string>(5, nil,
        function(x: Integer): string
        begin
          Result := IntToStr(x);
        end);
    end);
end;

procedure TTestAggregate.NilFuncUnseeded;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3]);
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Aggregate(nil);
    end);
end;

procedure TTestAggregate.NilProjectionSeededWithResultSelector;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3]);
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Aggregate<Integer, string>(5,
        function(x, y: Integer): Integer
        begin
          Result := x + y;
        end, nil);
    end);
end;

procedure TTestAggregate.NilSourceSeeded;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Aggregate<Integer>(3,
        function(x, y: Integer): Integer
        begin
          Result := x + y;
        end);
    end);
end;

procedure TTestAggregate.NilSourceSeededWithResultSelector;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Aggregate<Integer, string>(3,
        function(x, y: Integer): Integer
        begin
          Result := x + y;
        end,
        function(x: Integer): string
        begin
          Result := IntToStr(x);
        end);
    end);
end;

procedure TTestAggregate.NilSourceUnseeded;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Aggregate(
        function(x, y: Integer): Integer
        begin
          Result := x + y;
        end);
    end);
end;

procedure TTestAggregate.SeededAggregation;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 4, 5]);
  // First iteration: 5 * 2 + 1 = 11
  // Second iteration: 11 * 2 + 4 = 26
  // Third iteration: 26 * 2 + 5 = 57
  CheckEquals(57, source.Aggregate<Integer>(5,
    function(current, value: Integer): Integer
    begin
      Result := current * 2 + value;
    end));
end;

procedure TTestAggregate.SeededAggregationWithResultSelector;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 4, 5]);
  // First iteration: 5 * 2 + 1 = 11
  // Second iteration: 11 * 2 + 4 = 26
  // Third iteration: 26 * 2 + 5 = 57
  CheckEquals('57', source.Aggregate<Integer, string>(5,
    function(current, value: Integer): Integer
    begin
      Result := current * 2 + value;
    end,
    function(x: Integer): string
    begin
      Result := IntToStr(x);
    end));
end;

procedure TTestAggregate.UnseededAggregation;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 4, 5]);
  // First iteration: 0 * 2 + 1 = 1
  // Second iteration: 1 * 2 + 4 = 6
  // Third iteration: 6 * 2 + 5 = 17
  CheckEquals(17, source.Aggregate(
    function(current, value: Integer): Integer
    begin
      Result := current * 2 + value;
    end));
end;
*)
{ TTestDistinct }

procedure TTestDistinct.ExecutionIsDeferred;
begin
  CheckExceptionDeferred(
    function(source: IEnumerable<Integer>): IEnumerable<Integer>
    begin
      Result := TDistinctIterator<Integer>.Create(source, nil);
    end);
end;

procedure TTestDistinct.NilComparerUsesDefault;
var
  source: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['xyz', 'test', 'XYZ', 'test', 'def']);
  query := TDistinctIterator<string>.Create(source, nil);
  CheckTrue(query.EqualsTo(['xyz', 'test', 'XYZ', 'def']));
end;

procedure TTestDistinct.NilSourceNoComparer;
var
  source: IEnumerable<string>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      TDistinctIterator<string>.Create(source, nil);
    end);
end;

procedure TTestDistinct.NilSourceWithComparer;
var
  source: IEnumerable<string>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      TDistinctIterator<string>.Create(source, TStringComparer.Ordinal);
    end);
end;

procedure TTestDistinct.NoComparerSpecifiedUsesDefault;
var
  source: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['xyz', 'test', 'XYZ', 'test', 'def']);
  query := TDistinctIterator<string>.Create(source, nil);
  CheckTrue(query.EqualsTo(['xyz', 'test', 'XYZ', 'def']));
end;

{ TTestUnion }

procedure TTestUnion.CaseInsensitiveComparerSpecified;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>(['a', 'b', 'B', 'c', 'b']);
  second := TCollections.CreateList<string>(['d', 'e', 'd', 'a']);
  query := TUnionIterator<string>.Create(first, second, TStringComparer.OrdinalIgnoreCase);
  CheckTrue(query.EqualsTo(['a', 'b', 'c', 'd', 'e']));
end;

procedure TTestUnion.EmptyFirstSequence;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>;
  second := TCollections.CreateList<string>(['d', 'e', 'd', 'a']);
  query := TUnionIterator<string>.Create(first, second);
  CheckTrue(query.EqualsTo(['d', 'e', 'a']));
end;

procedure TTestUnion.EmptySecondSequence;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>(['a', 'b', 'B', 'c', 'b']);
  second := TCollections.CreateList<string>;
  query := TUnionIterator<string>.Create(first, second);
  CheckTrue(query.EqualsTo(['a', 'b', 'B', 'c']));
end;

procedure TTestUnion.FirstSequenceIsNotUsedUntilQueryIsIterated;
var
  first: IEnumerable<Integer>;
  second: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
  iterator: IEnumerator<Integer>;
begin
  first := TThrowingEnumerable.Create;
  second := TCollections.CreateList<Integer>([2]);
  query := TUnionIterator<Integer>.Create(first, second);
  iterator := query.GetEnumerator;
  CheckException(EInvalidOperationException,
    procedure
    begin
      iterator.MoveNext;
    end);
end;

procedure TTestUnion.NilComparerSpecified;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>(['a', 'b', 'B', 'c', 'b']);
  second := TCollections.CreateList<string>(['d', 'e', 'd', 'a']);
  query := TUnionIterator<string>.Create(first, second, nil);
  CheckTrue(query.EqualsTo(['a', 'b', 'B', 'c', 'd', 'e']));
end;

procedure TTestUnion.NilFirstWithComparer;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
begin
  first := nil;
  second := TCollections.CreateList<string>;
  CheckException(EArgumentNilException,
    procedure
    begin
      TUnionIterator<string>.Create(first, second, TStringComparer.Ordinal);
    end);
end;

procedure TTestUnion.NilFirstWithoutComparer;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
begin
  first := nil;
  second := TCollections.CreateList<string>;
  CheckException(EArgumentNilException,
    procedure
    begin
      TUnionIterator<string>.Create(first, second);
    end);
end;

procedure TTestUnion.NilSecondWithComparer;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>;
  second := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      TUnionIterator<string>.Create(first, second, TStringComparer.Ordinal);
    end);
end;

procedure TTestUnion.NilSecondWithoutComparer;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>;
  second := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      TUnionIterator<string>.Create(first, second);
    end);
end;

procedure TTestUnion.NoComparerSpecified;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>(['a', 'b', 'B', 'c', 'b']);
  second := TCollections.CreateList<string>(['d', 'e', 'd', 'a']);
  query := TUnionIterator<string>.Create(first, second);
  CheckTrue(query.EqualsTo(['a', 'b', 'B', 'c', 'd', 'e']));
end;

procedure TTestUnion.SecondSequenceIsNotUsedUntilFirstIsExhausted;
var
  first: IEnumerable<Integer>;
  second: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
  iterator: IEnumerator<Integer>;
begin
  first := TCollections.CreateList<Integer>([3, 5, 3]);
  second := TThrowingEnumerable.Create;
  query := TUnionIterator<Integer>.Create(first, second);
  iterator := query.GetEnumerator;
  CheckTrue(iterator.MoveNext);
  CheckEquals(3, iterator.Current);
  CheckTrue(iterator.MoveNext);
  CheckEquals(5, iterator.Current);
  CheckException(EInvalidOperationException,
    procedure
    begin
      iterator.MoveNext;
    end);
end;

procedure TTestUnion.TwoEmptySequences;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>;
  second := TCollections.CreateList<string>;
  query := TUnionIterator<string>.Create(first, second);
  CheckTrue(query.EqualsTo([]));
end;

{ TTestIntersect }

procedure TTestIntersect.CaseInsensitiveComparerSpecified;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>(['A', 'a', 'b', 'c', 'b']);
  second := TCollections.CreateList<string>(['b', 'a', 'd', 'a']);
  query := TIntersectIterator<string>.Create(first, second, TStringComparer.OrdinalIgnoreCase);
  CheckTrue(query.EqualsTo(['A', 'b']));
end;

procedure TTestIntersect.FirstSequenceOnlyReadAsResultsAreRead;
var
  first: IEnumerable<Integer>;
  second: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
  iterator: IEnumerator<Integer>;
begin
  first := TCollections.CreateList<Integer>([10, 2, 0, 2]);
  first := TSelectIterator<Integer, Integer>.Create(first,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  second := TCollections.CreateList<Integer>([1]);
  query := TIntersectIterator<Integer>.Create(first, second);
  iterator := query.GetEnumerator;
  CheckTrue(iterator.MoveNext);
  CheckEquals(1, iterator.Current);
  CheckException(EDivByZero,
    procedure
    begin
      iterator.MoveNext;
    end);
  iterator := nil;
end;

procedure TTestIntersect.NilComparerSpecified;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>(['A', 'a', 'b', 'c', 'b']);
  second := TCollections.CreateList<string>(['b', 'a', 'd', 'a']);
  query := TIntersectIterator<string>.Create(first, second, nil);
  CheckTrue(query.EqualsTo(['a', 'b']));
end;

procedure TTestIntersect.NilFirstWithComparer;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
begin
  first := nil;
  second := TCollections.CreateList<string>;
  CheckException(EArgumentNilException,
    procedure
    begin
      TIntersectIterator<string>.Create(first, second, TStringComparer.Ordinal);
    end);
end;

procedure TTestIntersect.NilFirstWithoutComparer;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
begin
  first := nil;
  second := TCollections.CreateList<string>;
  CheckException(EArgumentNilException,
    procedure
    begin
      TIntersectIterator<string>.Create(first, second);
    end);
end;

procedure TTestIntersect.NilSecondWithComparer;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>;
  second := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      TIntersectIterator<string>.Create(first, second, TStringComparer.Ordinal);
    end);
end;

procedure TTestIntersect.NilSecondWithoutComparer;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>;
  second := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      TIntersectIterator<string>.Create(first, second);
    end);
end;

procedure TTestIntersect.NoComparerSpecified;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>(['A', 'a', 'b', 'c', 'b']);
  second := TCollections.CreateList<string>(['b', 'a', 'd', 'a']);
  query := TIntersectIterator<string>.Create(first, second);
  CheckTrue(query.EqualsTo(['a', 'b']));
end;

procedure TTestIntersect.NoSequencesUsedBeforeIteration;
var
  first: IEnumerable<Integer>;
  second: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
  iterator: IEnumerator<Integer>;
begin
  first := TThrowingEnumerable.Create;
  second := TThrowingEnumerable.Create;
  query := TIntersectIterator<Integer>.Create(first, second);
  iterator := query.GetEnumerator;
end;

procedure TTestIntersect.SecondSequenceReadFullyOnFirstResultIteration;
var
  first: IEnumerable<Integer>;
  second: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
  iterator: IEnumerator<Integer>;
begin
  first := TCollections.CreateList<Integer>([1]);
  second := TCollections.CreateList<Integer>([10, 2, 0]);
  second := TSelectIterator<Integer, Integer>.Create(second,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  query := TIntersectIterator<Integer>.Create(first, second);
  iterator := query.GetEnumerator;
  CheckException(EDivByZero,
    procedure
    begin
      iterator.MoveNext;
    end);
  iterator := nil;
end;

{ TTestExcept }

procedure TTestExcept.CaseInsensitiveComparerSpecified;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>(['A', 'a', 'b', 'c', 'b']);
  second := TCollections.CreateList<string>(['b', 'a', 'd', 'a']);
  query := TExceptIterator<string>.Create(first, second, TStringComparer.OrdinalIgnoreCase);
  CheckTrue(query.EqualsTo(['c']));
end;

procedure TTestExcept.FirstSequenceOnlyReadAsResultsAreRead;
var
  first: IEnumerable<Integer>;
  second: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
  iterator: IEnumerator<Integer>;
begin
  first := TCollections.CreateList<Integer>([10, 2, 0, 2]);
  first := TSelectIterator<Integer, Integer>.Create(first,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  second := TCollections.CreateList<Integer>([1]);
  query := TExceptIterator<Integer>.Create(first, second);
  iterator := query.GetEnumerator;
  CheckTrue(iterator.MoveNext);
  CheckEquals(5, iterator.Current);
  CheckException(EDivByZero,
    procedure
    begin
      iterator.MoveNext;
    end);
  iterator := nil;
end;

procedure TTestExcept.NilComparerSpecified;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>(['A', 'a', 'b', 'c', 'b', 'c']);
  second := TCollections.CreateList<string>(['b', 'a', 'd', 'a']);
  query := TExceptIterator<string>.Create(first, second, nil);
  CheckTrue(query.EqualsTo(['A', 'c']));
end;

procedure TTestExcept.NilFirstWithComparer;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
begin
  first := nil;
  second := TCollections.CreateList<string>;
  CheckException(EArgumentNilException,
    procedure
    begin
      TExceptIterator<string>.Create(first, second, TStringComparer.Ordinal);
    end);
end;

procedure TTestExcept.NilFirstWithoutComparer;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
begin
  first := nil;
  second := TCollections.CreateList<string>;
  CheckException(EArgumentNilException,
    procedure
    begin
      TExceptIterator<string>.Create(first, second);
    end);
end;

procedure TTestExcept.NilSecondWithComparer;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>;
  second := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      TExceptIterator<string>.Create(first, second, TStringComparer.Ordinal);
    end);
end;

procedure TTestExcept.NilSecondWithoutComparer;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>;
  second := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      TExceptIterator<string>.Create(first, second);
    end);
end;

procedure TTestExcept.NoComparerSpecified;
var
  first: IEnumerable<string>;
  second: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  first := TCollections.CreateList<string>(['A', 'a', 'b', 'c', 'b', 'c']);
  second := TCollections.CreateList<string>(['b', 'a', 'd', 'a']);
  query := TExceptIterator<string>.Create(first, second);
  CheckTrue(query.EqualsTo(['A', 'c']));
end;

procedure TTestExcept.NoSequencesUsedBeforeIteration;
var
  first: IEnumerable<Integer>;
  second: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
  iterator: IEnumerator<Integer>;
begin
  first := TThrowingEnumerable.Create;
  second := TThrowingEnumerable.Create;
  query := TExceptIterator<Integer>.Create(first, second);
  iterator := query.GetEnumerator;
end;

procedure TTestExcept.SecondSequenceReadFullyOnFirstResultIteration;
var
  first: IEnumerable<Integer>;
  second: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
  iterator: IEnumerator<Integer>;
begin
  first := TCollections.CreateList<Integer>([1]);
  second := TCollections.CreateList<Integer>([10, 2, 0]);
  second := TSelectIterator<Integer, Integer>.Create(second,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  query := TExceptIterator<Integer>.Create(first, second);
  iterator := query.GetEnumerator;
  CheckException(EDivByZero,
    procedure
    begin
      iterator.MoveNext;
    end);
  iterator := nil;
end;

{ TTestToLookup }

procedure TTestToLookup.ChangesToSourceSequenceAfterToLookupAreNotNoticed;
var
  source: IList<string>;
  lookup: ILookup<Integer, string>;
begin
  source := TList<string>.Create(['abc']);
  lookup := TLookup<Integer, string>.Create<string>(source,
    function(x: string): Integer
    begin
      Result := Length(x);
    end,
    function(x: string): string
    begin
      Result := x;
    end);
  CheckEquals(1, lookup.Count);
  source.Add('x');
  CheckEquals(1, lookup.Count);
  source.Add('xyz');
  CheckTrue(lookup[3].EqualsTo(['abc']));
end;

type
  TPeople = record
    First: string;
    Last: string;
  end;

procedure TTestToLookup.LookupWithComparerAndElementSelector;
const
  people: array[0..6] of TPeople = (
    (First: 'Jon'; Last: 'Skeet'),
    (First: 'Tom'; Last: 'SKEET'),
    (First: 'Juni'; Last: 'Cortez'),
    (First: 'Holly'; Last: 'Skeet'),
    (First: 'Abbey'; Last: 'Bartlet'),
    (First: 'Carmen'; Last: 'Cortez'),
    (First: 'Jed'; Last: 'Bartlet')
  );
var
  source: IEnumerable<TPeople>;
  lookup: ILookup<string, string>;
  query: IEnumerable<string>;
begin
  source := TCollections.CreateList<TPeople>(people);
  lookup := TLookup<string, string>.Create<TPeople>(source,
    function(p: TPeople): string
    begin
      Result := p.Last;
    end,
    function(p: TPeople): string
    begin
      Result := p.First;
    end,
    TStringComparer.OrdinalIgnoreCase);
  CheckTrue(lookup['Skeet'].EqualsTo(['Jon', 'Tom', 'Holly']));
  CheckTrue(lookup['Cortez'].EqualsTo(['Juni', 'Carmen']));
  CheckTrue(lookup['BARTLET'].EqualsTo(['Abbey', 'Jed']));

  query := TSelectIterator<IGrouping<string, string>, string>.Create(lookup,
    function(x: IGrouping<string, string>): string
    begin
      Result := x.Key;
    end);
  CheckTrue(query.EqualsTo(['Skeet', 'Cortez', 'Bartlet']));
end;

procedure TTestToLookup.LookupWithComparerButNoElementSelector;
var
  source: IEnumerable<string>;
  lookup: ILookup<string,string>;
begin
  source := TCollections.CreateList<string>(['abc', 'def', 'ABC']);
  lookup := TLookup<string, string>.Create<string>(source,
    function(value: string): string
    begin
      Result := value;
    end,
    function(value: string): string
    begin
      Result := value;
    end,
    TStringComparer.OrdinalIgnoreCase);
  CheckTrue(lookup['abc'].EqualsTo(['abc', 'ABC']));
  CheckTrue(lookup['def'].EqualsTo(['def']));
end;

procedure TTestToLookup.LookupWithElementSelectorButNoComparer;
var
  source: IEnumerable<string>;
  lookup: ILookup<Integer, Char>;
begin
  source := TCollections.CreateList<string>(['abc', 'def', 'x', 'y', 'ghi', 'z', '00']);
  lookup := TLookup<Integer, Char>.Create<string>(source,
    function(x: string): Integer
    begin
      Result := Length(x);
    end,
    function(x: string): Char
    begin
      Result := x[1];
    end);
  CheckTrue(lookup[3].EqualsTo(['a', 'd', 'g']));
  CheckTrue(lookup[1].EqualsTo(['x', 'y', 'z']));
  CheckTrue(lookup[2].EqualsTo(['0']));
end;

procedure TTestToLookup.LookupWithNilComparerButNoElementSelector;
var
  source: IEnumerable<string>;
  lookup: ILookup<string,string>;
begin
  source := TCollections.CreateList<string>(['abc', 'def', 'ABC']);
    lookup := TLookup<string, string>.Create<string>(source,
    function(value: string): string
    begin
      Result := value;
    end,
    function(value: string): string
    begin
      Result := value;
    end);
  CheckTrue(lookup['abc'].EqualsTo(['abc']));
  CheckTrue(lookup['ABC'].EqualsTo(['ABC']));
  CheckTrue(lookup['def'].EqualsTo(['def']));
end;

procedure TTestToLookup.LookupWithNoComparerOrElementSelector;
var
  source: IEnumerable<string>;
  lookup: ILookup<Integer, string>;
begin
  source := TCollections.CreateList<string>(['abc', 'def', 'x', 'y', 'ghi', 'z', '00']);
  lookup := TLookup<Integer, string>.Create<string>(source,
    function(x: string): Integer
    begin
      Result := Length(x);
    end,
    function(value: string): string
    begin
      Result := value;
    end);
  CheckTrue(lookup[3].EqualsTo(['abc', 'def', 'ghi']));
  CheckTrue(lookup[1].EqualsTo(['x', 'y', 'z']));
  CheckTrue(lookup[2].EqualsTo(['00']));

  CheckEquals(3, lookup.Count);
  CheckTrue(lookup[100].EqualsTo([]));
end;

procedure TTestToLookup.SourceSequenceIsReadEagerly;
var
  source: IEnumerable<Integer>;
begin
  source := TThrowingEnumerable.Create;
  CheckException(EInvalidOperationException,
    procedure
    begin
      TLookup<Integer, Integer>.Create<Integer>(source,
        function(x: Integer): Integer
        begin
          Result := x;
        end,
        function(x: Integer): Integer
        begin
          Result := x;
        end);
    end);
end;

{ TTestJoin }

procedure TTestJoin.DifferentSourceTypes;
var
  outer: IEnumerable<Integer>;
  inner: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  outer := TCollections.CreateList<Integer>([5, 3, 7]);
  inner := TCollections.CreateList<string>(['bee', 'giraffe', 'tiger', 'badger', 'ox', 'cat', 'dog']);
  query := TJoinIterator<Integer, string, Integer, string>.Create(outer, inner,
    function(outerElement: Integer): Integer
    begin
      Result := outerElement;
    end,
    function(innerElement: string): Integer
    begin
      Result := Length(innerElement);
    end,
    function(outerElement: Integer; innerElement: string): string
    begin
      Result := IntToStr(outerElement) + ':' + innerElement;
    end);
  CheckTrue(query.EqualsTo(['5:tiger', '3:bee', '3:cat', '3:dog', '7:giraffe']));
end;

procedure TTestJoin.ExecutionIsDeferred;
var
  outer: IEnumerable<Integer>;
  inner: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  outer := TThrowingEnumerable.Create;
  inner := TThrowingEnumerable.Create;
  query := TJoinIterator<Integer, Integer, Integer, Integer>.Create(outer, inner,
    function(x: Integer): Integer
    begin
      Result := x;
    end,
    function(y: Integer): Integer
    begin
      Result := y;
    end,
    function(x, y: Integer): Integer
    begin
      Result := x + y;
    end);
end;

procedure TTestJoin.InnerSequenceIsBuffered;
var
  outer: IEnumerable<Integer>;
  inner: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
  iterator: IEnumerator<Integer>;
begin
  outer := TCollections.CreateList<Integer>([1, 2, 3]);
  inner := TCollections.CreateList<Integer>([10, 0, 2]);
  inner := TSelectIterator<Integer, Integer>.Create(inner,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  query := TJoinIterator<Integer, Integer, Integer, Integer>.Create(outer, inner,
    function(x: Integer): Integer
    begin
      Result := x;
    end,
    function(y: Integer): Integer
    begin
      Result := y;
    end,
    function(x, y: Integer): Integer
    begin
      Result := x + y;
    end);
  iterator := query.GetEnumerator;
  CheckException(EDivByZero,
    procedure
    begin
      iterator.MoveNext;
    end);
  iterator := nil;
end;

procedure TTestJoin.OuterSequenceIsStreamed;
var
  outer: IEnumerable<Integer>;
  inner: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
  iterator: IEnumerator<Integer>;
begin
  outer := TCollections.CreateList<Integer>([10, 0, 2]);
  outer := TSelectIterator<Integer, Integer>.Create(outer,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  inner := TCollections.CreateList<Integer>([1, 2, 3]);
  query := TJoinIterator<Integer, Integer, Integer, Integer>.Create(outer, inner,
    function(x: Integer): Integer
    begin
      Result := x;
    end,
    function(y: Integer): Integer
    begin
      Result := y;
    end,
    function(x, y: Integer): Integer
    begin
      Result := x + y;
    end);
  iterator := query.GetEnumerator;
  CheckTrue(iterator.MoveNext);
  CheckEquals(2, iterator.Current);
  CheckException(EDivByZero,
    procedure
    begin
      iterator.MoveNext;
    end);
  iterator := nil;
end;

procedure TTestJoin.SimpleJoin;
var
  outer: IEnumerable<string>;
  inner: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  outer := TCollections.CreateList<string>(['ABCxxx', 'abcyyy', 'defzzz', 'ghizzz']);
  inner := TCollections.CreateList<string>(['000abc', '111gHi', '222333']);
  query := TJoinIterator<string, string, string, string>.Create(outer, inner,
    function(outerElement: string): string
    begin
      Result := Copy(outerElement, 1, 3);
    end,
    function(innerElement: string): string
    begin
      Result := Copy(innerElement, 4);
    end,
    function(outerElement, innerElement: string): string
    begin
      Result := outerElement + ':' + innerElement;
    end,
    TStringComparer.OrdinalIgnoreCase);
  CheckTrue(query.EqualsTo(['ABCxxx:000abc', 'abcyyy:000abc', 'ghizzz:111gHi']));
end;

{ TTestGroupBy }

procedure TTestGroupBy.ChangesToSourceAreIgnoredInWhileIteratingOverResultsAfterFirstElementRetrieved;
var
  source: IList<string>;
  groups: IEnumerable<IGrouping<Integer, string>>;
  iterator: IEnumerator<IGrouping<Integer, string>>;
begin
  source := TCollections.CreateList<string>(['a', 'b', 'c', 'def']);
  groups := TGroupedEnumerable<string, Integer, string>.Create(source,
    function(x: string): Integer
    begin
      Result := Length(x);
    end,
    function(s: string): string
    begin
      Result := s;
    end);
  iterator := groups.GetEnumerator;
  CheckTrue(iterator.MoveNext);
  CheckTrue(iterator.Current.EqualsTo(['a', 'b', 'c']));
  source.Add('ghi');
  CheckTrue(iterator.MoveNext);
  CheckTrue(iterator.Current.EqualsTo(['def']));
  CheckFalse(iterator.MoveNext);

  iterator := groups.GetEnumerator;
  CheckTrue(iterator.MoveNext);
  CheckTrue(iterator.Current.EqualsTo(['a', 'b', 'c']));
  CheckTrue(iterator.MoveNext);
  CheckTrue(iterator.Current.EqualsTo(['def', 'ghi']));
end;

procedure TTestGroupBy.ExecutionIsPartiallyDeferred;
var
  source: IEnumerable<Integer>;
  groups: IEnumerable<IGrouping<Integer, Integer>>;
begin
  source := TThrowingEnumerable.Create;
  groups := TGroupedEnumerable<Integer, Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := x;
    end,
    function(x: Integer): Integer
    begin
      Result := x;
    end);
end;

function Join(const separator: string; const values: IEnumerable): string;
var
  v: TValue;
  i: Integer;
begin
  i := 0;
  for v in values do
  begin
    if i = 0 then
      Result := v.AsString
    else
      Result := Result + separator + v.AsString;
    Inc(i);
  end;
end;

procedure TTestGroupBy.GroupByWithCollectionProjection;
var
  source: IEnumerable<string>;
  groups: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['abc', 'hello', 'def', 'there', 'four']);
  groups := TGroupedEnumerable<string, Integer, string, string>.Create(source,
    function(x: string): Integer
    begin
      Result := Length(x);
    end,
    function(s: string): string
    begin
      Result := s;
    end,
    function(key: Integer; values: IEnumerable<string>): string
    begin
      Result := IntToStr(key) + ':' + Join(';', values);
    end);
  CheckTrue(groups.EqualsTo(['3:abc;def', '5:hello;there', '4:four']));
end;

procedure TTestGroupBy.GroupByWithElementProjection;
var
  source: IEnumerable<string>;
  groups: IEnumerable<IGrouping<Integer, Char>>;
  list: IList<IGrouping<Integer, Char>>;
begin
  source := TCollections.CreateList<string>(['abc', 'hello', 'def', 'there', 'four']);
  groups := TGroupedEnumerable<string, Integer, Char>.Create(source,
    function(x: string): Integer
    begin
      Result := Length(x);
    end,
    function(x: string): Char
    begin
      Result := x[1];
    end);
  list := TCollections.CreateInterfaceList<IGrouping<Integer, Char>>(groups);
  CheckEquals(3, list.Count);

  CheckTrue(list[0].EqualsTo(['a', 'd']));
  CheckEquals(3, list[0].Key);

  CheckTrue(list[1].EqualsTo(['h', 't']));
  CheckEquals(5, list[1].Key);

  CheckTrue(list[2].EqualsTo(['f']));
  CheckEquals(4, list[2].Key);
end;

procedure TTestGroupBy.GroupByWithElementProjectionAndCollectionProjection;
var
  source: IEnumerable<string>;
  groups: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['abc', 'hello', 'def', 'there', 'four']);
  groups := TGroupedEnumerable<string, Integer, Char, string>.Create(source,
    function(x: string): Integer
    begin
      Result := Length(x);
    end,
    function(x: string): Char
    begin
      Result := x[1];
    end,
    function(key: Integer; values: IEnumerable<Char>): string
    begin
      Result := IntToStr(key) + ':' + Join(';', values);
    end);
  CheckTrue(groups.EqualsTo(['3:a;d', '5:h;t', '4:f']));
end;

procedure TTestGroupBy.SequenceIsReadFullyBeforeFirstResultReturned;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
  groups: IEnumerable<IGrouping<Integer, Integer>>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 3, 4, 5, 6, 7, 8, 9, 0]);
  query := TSelectIterator<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  groups := TGroupedEnumerable<Integer, Integer, Integer>.Create(query,
    function(x: Integer): Integer
    begin
      Result := x;
    end,
    function(i: Integer): Integer
    begin
      Result := i;
    end);
  CheckException(EDivByZero,
    procedure
    var
      iterator: IEnumerator<IGrouping<Integer, Integer>>;
    begin
      iterator := groups.GetEnumerator;
      iterator.MoveNext;
    end);
  groups := nil;
end;

procedure TTestGroupBy.SimpleGroupBy;
var
  source: IEnumerable<string>;
  groups: IEnumerable<IGrouping<Integer, string>>;
  list: IList<IGrouping<Integer, string>>;
begin
  source := TCollections.CreateList<string>(['abc', 'hello', 'def', 'there', 'four']);
  groups := TGroupedEnumerable<string, Integer, string>.Create(source,
    function(x: string): Integer
    begin
      Result := Length(x);
    end,
    function(s: string): string
    begin
      Result := s;
    end);
  list := TCollections.CreateInterfaceList<IGrouping<Integer, string>>(groups);
  CheckEquals(3, list.Count);

  CheckTrue(list[0].EqualsTo(['abc', 'def']));
  CheckEquals(3, list[0].Key);

  CheckTrue(list[1].EqualsTo(['hello', 'there']));
  CheckEquals(5, list[1].Key);

  CheckTrue(list[2].EqualsTo(['four']));
  CheckEquals(4, list[2].Key);
end;

{ TTestGroupJoin }

procedure TTestGroupJoin.CustomComparer;
var
  outer: IEnumerable<string>;
  inner: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  outer := TCollections.CreateList<string>(['ABCxxx', 'abcyyy', 'defzzz', 'ghizzz']);
  inner := TCollections.CreateList<string>(['000abc', '111gHi', '222333', '333AbC']);
  query := TGroupJoinIterator<string, string, string, string>.Create(outer, inner,
    function(outerElement: string): string
    begin
      Result := Copy(outerElement, 1, 3);
    end,
    function(innerElement: string): string
    begin
      Result := Copy(innerElement, 4);
    end,
    function(outerElement: string; innerElements: IEnumerable<string>): string
    begin
      Result := outerElement + ':' + Join(';', innerElements);
    end,
    TStringComparer.OrdinalIgnoreCase);
  CheckTrue(query.EqualsTo(['ABCxxx:000abc;333AbC', 'abcyyy:000abc;333AbC', 'defzzz:', 'ghizzz:111gHi']));
end;

procedure TTestGroupJoin.DifferentSourceTypes;
var
  outer: IEnumerable<Integer>;
  inner: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  outer := TCollections.CreateList<Integer>([5, 3, 7, 4]);
  inner := TCollections.CreateList<string>(['bee', 'giraffe', 'tiger', 'badger', 'ox', 'cat', 'dog']);
  query := TGroupJoinIterator<Integer, string, Integer, string>.Create(outer, inner,
    function(outerElement: Integer): Integer
    begin
      Result := outerElement;
    end,
    function(innerElement: string): Integer
    begin
      Result := Length(innerElement);
    end,
    function(outerElement: Integer; innerElement: IEnumerable<string>): string
    begin
      Result := IntToStr(outerElement) + ':' + Join(';', innerElement);
    end);
  CheckTrue(query.EqualsTo(['5:tiger', '3:bee;cat;dog', '7:giraffe', '4:']));
end;

procedure TTestGroupJoin.ExecutionIsDeferred;
var
  outer: IEnumerable<Integer>;
  inner: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  outer := TThrowingEnumerable.Create;
  inner := TThrowingEnumerable.Create;
  query := TGroupJoinIterator<Integer, Integer, Integer, Integer>.Create(outer, inner,
    function(x: Integer): Integer
    begin
      Result := x;
    end,
    function(y: Integer): Integer
    begin
      Result := y;
    end,
    function(x: Integer; y: IEnumerable<Integer>): Integer
    begin
      Result := x + y.Count;
    end);
end;

procedure TTestGroupJoin.SimpleGroupJoin;
var
  outer: IEnumerable<string>;
  inner: IEnumerable<string>;
  query: IEnumerable<string>;
begin
  outer := TCollections.CreateList<string>(['first', 'second', 'third']);
  inner := TCollections.CreateList<string>(['essence', 'offer', 'eating', 'psalm']);
  query := TGroupJoinIterator<string, string, Char, string>.Create(outer, inner,
    function(outerElement: string): Char
    begin
      Result := outerElement[1];
    end,
    function(innerElement: string): Char
    begin
      Result := innerElement[2];
    end,
    function(outerElement: string; innerElements: IEnumerable<string>): string
    begin
      Result := outerElement + ':' + Join(';', innerElements);
    end);
  CheckTrue(query.EqualsTo(['first:offer', 'second:essence;psalm', 'third:']))
end;

{ TTestTake }

procedure TTestTake.CountEqualToSourceLength;
begin
  CheckTrue(TCollections.Range(0, 5).Take(5).EqualsTo([0, 1, 2, 3, 4]));
end;

procedure TTestTake.CountGreaterThanSourceLength;
begin
  CheckTrue(TCollections.Range(0, 5).Take(100).EqualsTo([0, 1, 2, 3, 4]));
end;

procedure TTestTake.CountShorterThanSourceLength;
begin
  CheckTrue(TCollections.Range(0, 5).Take(3).EqualsTo([0, 1, 2]));
end;

procedure TTestTake.ExecutionIsDeferred;
var
  source: IEnumerable<Integer>;
begin
  source := TThrowingEnumerable.Create;
  source.Take(10);
end;

procedure TTestTake.NegativeCount;
begin
  CheckTrue(TCollections.Range(0, 5).Take(-5).EqualsTo([]));
end;

(*
procedure TTestTake.NilSource;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.Take(10);
    end);
end;
*)

procedure TTestTake.OnlyEnumerateTheGivenNumberOfElements;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2, 0]);
  query := TSelectIterator<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  CheckTrue(query.Take(2).EqualsTo([10, 5]));
end;

procedure TTestTake.ZeroCount;
begin
  CheckTrue(TCollections.Range(0, 5).Take(0).EqualsTo([]));
end;

{ TTestSkip }

procedure TTestSkip.CountEqualToSourceLength;
begin
  CheckTrue(TCollections.Range(0, 5).Skip(5).EqualsTo([]));
end;

procedure TTestSkip.CountGreaterThanSourceLength;
begin
  CheckTrue(TCollections.Range(0, 5).Skip(100).EqualsTo([]));
end;

procedure TTestSkip.CountShorterThanSourceLength;
begin
  CheckTrue(TCollections.Range(0, 5).Skip(3).EqualsTo([3, 4]));
end;

procedure TTestSkip.ExecutionIsDeferred;
var
  source: IEnumerable<Integer>;
begin
  source := TThrowingEnumerable.Create;
  source.Skip(10);
end;

procedure TTestSkip.NegativeCount;
begin
  CheckTrue(TCollections.Range(0, 5).Skip(-5).EqualsTo([0, 1, 2, 3, 4]));
end;

(*
procedure TTestSkip.NilSource;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.Skip(10);
    end);
end;
*)

procedure TTestSkip.ZeroCount;
begin
  CheckTrue(TCollections.Range(0, 5).Skip(0).EqualsTo([0, 1, 2, 3, 4]));
end;

{ TTestTakeWhile }

procedure TTestTakeWhile.ExecutionIsDeferred;
var
  source: IEnumerable<Integer>;
begin
  source := TThrowingEnumerable.Create;
  source.TakeWhile(
    function(const x: Integer): Boolean
    begin
      Result := x > 10;
    end);
end;

procedure TTestTakeWhile.NilPredicateNoIndex;
var
  source: IEnumerable<Integer>;
  predicate: TPredicate<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2]);
  predicate := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.TakeWhile(predicate);
    end);
end;

procedure TTestTakeWhile.NilPredicateUsingIndex;
var
  source: IEnumerable<Integer>;
  predicate: TFunc<Integer, Integer, Boolean>;
begin
  source := TCollections.CreateList<Integer>([1, 2]);
  predicate := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.TakeWhile(predicate);
    end);
end;

(*
procedure TTestTakeWhile.NilSourceNoIndex;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.TakeWhile(
        function(const x: Integer): Boolean
        begin
          Result := x > 10;
        end);
    end);
end;

procedure TTestTakeWhile.NilSourceUsingIndex;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.TakeWhile(
        function(x, index: Integer): Boolean
        begin
          Result := x > 10;
        end);
    end);
end;
*)

procedure TTestTakeWhile.PredicateFailingFirstElement;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['zero', 'one', 'two', 'three', 'four', 'five']);
  CheckTrue(source.TakeWhile(
    function(const x: string): Boolean
    begin
      Result := Length(x) > 4;
    end).EqualsTo([]));
end;

procedure TTestTakeWhile.PredicateMatchingAllElements;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['zero', 'one', 'two', 'three', 'four', 'five']);
  CheckTrue(source.TakeWhile(
    function(x: string; index: Integer): Boolean
    begin
      Result := Length(x) < 100;
    end).EqualsTo(['zero', 'one', 'two', 'three', 'four', 'five']));
end;

procedure TTestTakeWhile.PredicateMatchingSomeElements;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['zero', 'one', 'two', 'three', 'four', 'five']);
  CheckTrue(source.TakeWhile(
    function(const x: string): Boolean
    begin
      Result := Length(x) < 5;
    end).EqualsTo(['zero', 'one', 'two']));
end;

procedure TTestTakeWhile.PredicateWithIndexFailingFirstElement;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['zero', 'one', 'two', 'three', 'four', 'five']);
  CheckTrue(source.TakeWhile(
    function(x: string; index: Integer): Boolean
    begin
      Result := index + Length(x) > 4;
    end).EqualsTo([]));
end;

procedure TTestTakeWhile.PredicateWithIndexMatchingAllElements;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['zero', 'one', 'two', 'three', 'four', 'five']);
  CheckTrue(source.TakeWhile(
    function(x: string; index: Integer): Boolean
    begin
      Result := Length(x) < 100;
    end).EqualsTo(['zero', 'one', 'two', 'three', 'four', 'five']));
end;

procedure TTestTakeWhile.PredicateWithIndexMatchingSomeElements;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['zero', 'one', 'two', 'three', 'four', 'five']);
  CheckTrue(source.TakeWhile(
    function(x: string; index: Integer): Boolean
    begin
      Result := Length(x) > index;
    end).EqualsTo(['zero', 'one', 'two', 'three']));
end;

{ TTestSkipWhile }

procedure TTestSkipWhile.ExecutionIsDeferred;
var
  source: IEnumerable<Integer>;
begin
  source := TThrowingEnumerable.Create;
  source.SkipWhile(
    function(const x: Integer): Boolean
    begin
      Result := x > 10;
    end);
end;

procedure TTestSkipWhile.NilPredicateNoIndex;
var
  source: IEnumerable<Integer>;
  predicate: TPredicate<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 2]);
  predicate := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.SkipWhile(predicate);
    end);
end;

procedure TTestSkipWhile.NilPredicateUsingIndex;
var
  source: IEnumerable<Integer>;
  predicate: TFunc<Integer, Integer, Boolean>;
begin
  source := TCollections.CreateList<Integer>([1, 2]);
  predicate := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.SkipWhile(predicate);
    end);
end;

(*
procedure TTestSkipWhile.NilSourceNoIndex;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.SkipWhile(
        function(const x: Integer): Boolean
        begin
          Result := x > 10;
        end);
    end);
end;

procedure TTestSkipWhile.NilSourceUsingIndex;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.SkipWhile(
        function(x: Integer; index: Integer): Boolean
        begin
          Result := x > 10;
        end);
    end);
end;
*)

procedure TTestSkipWhile.PredicateFailingFirstElement;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['zero', 'one', 'two', 'three', 'four', 'five']);
  CheckTrue(source.SkipWhile(
    function(const x: string): Boolean
    begin
      Result := Length(x) > 4;
    end).EqualsTo(['zero', 'one', 'two', 'three', 'four', 'five']));
end;

procedure TTestSkipWhile.PredicateMatchingAllElements;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['zero', 'one', 'two', 'three', 'four', 'five']);
  CheckTrue(source.SkipWhile(
    function(x: string; index: Integer): Boolean
    begin
      Result := Length(x) < 100;
    end).EqualsTo([]));
end;

procedure TTestSkipWhile.PredicateMatchingSomeElements;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['zero', 'one', 'two', 'three', 'four', 'five']);
  CheckTrue(source.SkipWhile(
    function(const x: string): Boolean
    begin
      Result := Length(x) < 5;
    end).EqualsTo(['three', 'four', 'five']));
end;

procedure TTestSkipWhile.PredicateWithIndexFailingFirstElement;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['zero', 'one', 'two', 'three', 'four', 'five']);
  CheckTrue(source.SkipWhile(
    function(x: string; index: Integer): Boolean
    begin
      Result := index + Length(x) > 4;
    end).EqualsTo(['zero', 'one', 'two', 'three', 'four', 'five']));
end;

procedure TTestSkipWhile.PredicateWithIndexMatchingAllElements;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['zero', 'one', 'two', 'three', 'four', 'five']);
  CheckTrue(source.SkipWhile(
    function(x: string; index: Integer): Boolean
    begin
      Result := Length(x) < 100;
    end).EqualsTo([]));
end;

procedure TTestSkipWhile.PredicateWithIndexMatchingSomeElements;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['zero', 'one', 'two', 'three', 'four', 'five']);
  CheckTrue(source.SkipWhile(
    function(x: string; index: Integer): Boolean
    begin
      Result := Length(x) > index;
    end).EqualsTo(['four', 'five']));
end;

{ TTestOrderBy }

type
  TIntPair = TPair<Integer, Integer>;

  TAbsoluteValueComparer = class(TComparer<Integer>)
    function Compare(const Left, Right: Integer): Integer; override;
  end;

function TAbsoluteValueComparer.Compare(const Left, Right: Integer): Integer;
begin
  Result := Abs(Left) - Abs(Right);
end;

procedure TTestOrderBy.CustomComparer;
var
  source: IEnumerable<TIntPair>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<TIntPair>([
    TIntPair.Create(15, 1),
    TIntPair.Create(-13, 2),
    TIntPair.Create(11, 3)]);
  source := TOrderedEnumerable<TIntPair, Integer>.Create(source,
    function(x: TIntPair): Integer
    begin
      Result := x.Key;
    end, TAbsoluteValueComparer.Create);
  query := TSelectIterator<TIntPair, Integer>.Create(source,
    function(x: TIntPair): Integer
    begin
      Result := x.Value;
    end);
  CheckTrue(query.EqualsTo([3, 2, 1]));
end;

procedure TTestOrderBy.ExecutionIsDeferred;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TThrowingEnumerable.Create;
  query := TOrderedEnumerable<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := x;
    end);
end;

procedure TTestOrderBy.KeySelectorIsCalledExactlyOncePerElement;
var
  source: IEnumerable<Integer>;
  count: Integer;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 5, 4, 2, 3, 7, 6, 8, 9]);
  count := 0;
  query := TOrderedEnumerable<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Inc(count);
      Result := x;
    end);
  CheckTrue(query.EqualsTo([1, 2, 3, 4, 5, 6, 7, 8, 9]));
  CheckEquals(9, count);
end;

procedure TTestOrderBy.NilComparerIsDefault;
var
  source: IEnumerable<TIntPair>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<TIntPair>([
    TIntPair.Create(15, 1),
    TIntPair.Create(-13, 2),
    TIntPair.Create(11, 3)]);
  source := TOrderedEnumerable<TIntPair, Integer>.Create(source,
    function(x: TIntPair): Integer
    begin
      Result := x.Key;
    end);
  query := TSelectIterator<TIntPair, Integer>.Create(source,
    function(x: TIntPair): Integer
    begin
      Result := x.Value;
    end);
  CheckTrue(query.EqualsTo([2, 3, 1]));
end;

procedure TTestOrderBy.NilKeySelectorNoComparer;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckException(EArgumentNilException,
    procedure
    begin
      TOrderedEnumerable<Integer, Integer>.Create(source, nil);
    end);
end;

procedure TTestOrderBy.NilKeySelectorWithComparer;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckException(EArgumentNilException,
    procedure
    begin
      TOrderedEnumerable<Integer, Integer>.Create(source, nil, TComparer<Integer>.Default);
    end);
end;

procedure TTestOrderBy.NilSourceNoComparer;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      TOrderedEnumerable<Integer, Integer>.Create(source,
        function(x: Integer): Integer
        begin
          Result := x;
        end);
    end);
end;

procedure TTestOrderBy.NilSourceWithComparer;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      TOrderedEnumerable<Integer, Integer>.Create(source,
        function(x: Integer): Integer
        begin
          Result := x;
        end,
        TComparer<Integer>.Default);
    end);
end;

procedure TTestOrderBy.OrderingIsStable;
var
  source: IEnumerable<TIntPair>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<TIntPair>([
    TIntPair.Create(10, 1),
    TIntPair.Create(11, 2),
    TIntPair.Create(11, 3),
    TIntPair.Create(10, 4)]);
  source := TOrderedEnumerable<TIntPair, Integer>.Create(source,
    function(x: TIntPair): Integer
    begin
      Result := x.Key;
    end);
  query := TSelectIterator<TIntPair, Integer>.Create(source,
    function(x: TIntPair): Integer
    begin
      Result := x.Value;
    end);
  CheckTrue(query.EqualsTo([1, 4, 2, 3]));
end;

procedure TTestOrderBy.SimpleUniqueKeys;
var
  source: IEnumerable<TIntPair>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<TIntPair>([
    TIntPair.Create(10, 1),
    TIntPair.Create(12, 2),
    TIntPair.Create(11, 3)]);
  source := TOrderedEnumerable<TIntPair, Integer>.Create(source,
    function(x: TIntPair): Integer
    begin
      Result := x.Key;
    end);
  query := TSelectIterator<TIntPair, Integer>.Create(source,
    function(x: TIntPair): Integer
    begin
      Result := x.Value;
    end);
  CheckTrue(query.EqualsTo([1, 3, 2]));
end;

{ TTestOrderByDescending }

procedure TTestOrderByDescending.CustomComparer;
var
  source: IEnumerable<TIntPair>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<TIntPair>([
    TIntPair.Create(15, 1),
    TIntPair.Create(-13, 2),
    TIntPair.Create(11, 3)]);
  source := TOrderedEnumerable<TIntPair, Integer>.Create(source,
    function(x: TIntPair): Integer
    begin
      Result := x.Key;
    end, TAbsoluteValueComparer.Create, True);
  query := TSelectIterator<TIntPair, Integer>.Create(source,
    function(x: TIntPair): Integer
    begin
      Result := x.Value;
    end);
  CheckTrue(query.EqualsTo([1, 2, 3]));
end;

procedure TTestOrderByDescending.CustomExtremeComparer;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3, 2, 4, 8, 5, 7, 6]);
  query := TOrderedEnumerable<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := x;
    end,
    TComparer<Integer>.Construct(
      function(const x, y: Integer): Integer
      begin
        if x = y then
          Result := 0
        else if x < y then
          Result := Low(Integer)
        else
          Result := High(Integer);
      end), True);
  CheckTrue(query.EqualsTo([8, 7, 6, 5, 4, 3, 2, 1]));
end;

procedure TTestOrderByDescending.ExecutionIsDeferred;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TThrowingEnumerable.Create;
  query := TOrderedEnumerable<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := x;
    end,
    nil, True);
end;

procedure TTestOrderByDescending.NilComparerIsDefault;
var
  source: IEnumerable<TIntPair>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<TIntPair>([
    TIntPair.Create(15, 1),
    TIntPair.Create(-13, 2),
    TIntPair.Create(11, 3)]);
  source := TOrderedEnumerable<TIntPair, Integer>.Create(source,
    function(x: TIntPair): Integer
    begin
      Result := x.Key;
    end, nil, True);
  query := TSelectIterator<TIntPair, Integer>.Create(source,
    function(x: TIntPair): Integer
    begin
      Result := x.Value;
    end);
  CheckTrue(query.EqualsTo([1, 3, 2]));
end;

procedure TTestOrderByDescending.NilKeySelectorNoComparer;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckException(EArgumentNilException,
    procedure
    begin
      TOrderedEnumerable<Integer, Integer>.Create(source, nil, nil, True);
    end);
end;

procedure TTestOrderByDescending.NilKeySelectorWithComparer;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckException(EArgumentNilException,
    procedure
    begin
      TOrderedEnumerable<Integer, Integer>.Create(source, nil, TComparer<Integer>.Default, True);
    end);
end;

procedure TTestOrderByDescending.NilSourceNoComparer;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      TOrderedEnumerable<Integer, Integer>.Create(source,
        function(x: Integer): Integer
        begin
          Result := x;
        end, nil, True);
    end);
end;

procedure TTestOrderByDescending.NilSourceWithComparer;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      TOrderedEnumerable<Integer, Integer>.Create(source,
        function(x: Integer): Integer
        begin
          Result := x;
        end,
        TComparer<Integer>.Default, True);
    end);
end;

procedure TTestOrderByDescending.OrderingIsStable;
var
  source: IEnumerable<TIntPair>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<TIntPair>([
    TIntPair.Create(10, 1),
    TIntPair.Create(11, 2),
    TIntPair.Create(11, 3),
    TIntPair.Create(10, 4)]);
  source := TOrderedEnumerable<TIntPair, Integer>.Create(source,
    function(x: TIntPair): Integer
    begin
      Result := x.Key;
    end, nil, True);
  query := TSelectIterator<TIntPair, Integer>.Create(source,
    function(x: TIntPair): Integer
    begin
      Result := x.Value;
    end);
  CheckTrue(query.EqualsTo([2, 3, 1, 4]));
end;

procedure TTestOrderByDescending.SimpleUniqueKeys;
var
  source: IEnumerable<TIntPair>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<TIntPair>([
    TIntPair.Create(10, 1),
    TIntPair.Create(12, 2),
    TIntPair.Create(11, 3)]);
  source := TOrderedEnumerable<TIntPair, Integer>.Create(source,
    function(x: TIntPair): Integer
    begin
      Result := x.Key;
    end, nil, True);
  query := TSelectIterator<TIntPair, Integer>.Create(source,
    function(x: TIntPair): Integer
    begin
      Result := x.Value;
    end);
  CheckTrue(query.EqualsTo([2, 3, 1]));
end;

{ TTestReverse }

procedure TTestReverse.ArraysAreBuffered;
var
  source: TArray<Integer>;
  query: IEnumerable<Integer>;
  iterator: IEnumerator<Integer>;
begin
  source := TArray<Integer>.Create(0, 1, 2, 3);
  query := TArrayIterator<Integer>.Create(source);
  query := query.Reversed;
  source[1] := 99;
  iterator := query.GetEnumerator;
  iterator.MoveNext;
  CheckEquals(3, iterator.Current);
  source[2] := 100;
  iterator.MoveNext;
  CheckEquals(2, iterator.Current);
  iterator.MoveNext;
  CheckEquals(99, iterator.Current);
  iterator.MoveNext;
  CheckEquals(0, iterator.Current);
end;

procedure TTestReverse.EmptyInput;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckTrue(source.Reversed.EqualsTo([]));
end;

procedure TTestReverse.ExecutionIsDeferred;
var
  source: IEnumerable<Integer>;
begin
  source := TThrowingEnumerable.Create;
  source.Reversed;
end;

procedure TTestReverse.InputIsBuffered;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([10, 0, 20]);
  query := TSelectIterator<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := 10 div x;
    end);
  query := query.Reversed;
  CheckException(EDivByZero,
    procedure
    var
      iterator: IEnumerator<Integer>;
    begin
      iterator := query.GetEnumerator;
      iterator.MoveNext;
    end);
  query := nil;
end;

(*
procedure TTestReverse.NilSource;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EAccessViolation,//EArgumentNilException,
    procedure
    begin
      source.Reversed;
    end);
end;
*)

procedure TTestReverse.ReversedList;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TList<Integer>.Create([5, 6, 7, 8, 9]);
  query := source.Reversed;
  CheckTrue(query.EqualsTo([9, 8, 7, 6, 5]));
end;

procedure TTestReverse.ReversedRange;
var
  query: IEnumerable<Integer>;
begin
  query := TCollections.Range(5, 5).Reversed;
  CheckTrue(query.EqualsTo([9, 8, 7, 6, 5]));
end;

{ TTestSum }
(*
procedure TTestSumInt32.EmptySequence;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckEquals(0, source.Sum);
end;

procedure TTestSumInt32.EmptySequenceWithSelector;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>;
  CheckEquals(0, source.Sum(
    function(x: string): Integer
    begin
      Result := Length(x);
    end));
end;

procedure TTestSumInt32.NegativeOverflow;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([MinInt, MinInt]);
  CheckException(EIntOverflow,
    procedure
    begin
      source.Sum;
    end);
end;

procedure TTestSumInt32.NilSelector;
var
  source: IEnumerable<string>;
  selector: TFunc<string, Integer>;
begin
  source := TCollections.CreateList<string>;
  selector := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum(selector);
    end);
end;

procedure TTestSumInt32.NilSelectorNullable;
var
  source: IEnumerable<string>;
  selector: TFunc<string, Integer>;
begin
  source := TCollections.CreateList<string>;
  selector := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum(selector);
    end);
end;

procedure TTestSumInt32.NilSourceNoSelector;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum;
    end);
end;

procedure TTestSumInt32.NilSourceWithSelector;
var
  source: IEnumerable<string>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum(
        function(x: string): Integer
        begin
          Result := Length(x);
        end);
    end);
end;

procedure TTestSumInt32.NilSourceNullableNoSelector;
var
  source: IEnumerable<Nullable<Integer>>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum;
    end);
end;

procedure TTestSumInt32.NilSourceNullableWithSelector;
var
  source: IEnumerable<string>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum(
        function(x: string): Nullable<Integer>
        begin
          Result := Length(x);
        end);
    end);
end;

procedure TTestSumInt32.Overflow;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([MinInt, MinInt]);
  CheckException(EIntOverflow,
    procedure
    begin
      source.Sum;
    end);
end;

procedure TTestSumInt32.OverflowWithSelector;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['x', 'y']);
  CheckException(EIntOverflow,
    procedure
    begin
      source.Sum(
        function(x: string): Integer
        begin
          Result := MaxInt;
        end);
    end);
end;

procedure TTestSumInt32.OverflowOfComputableSum;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([MaxInt, 1, -1, -MaxInt]);
  CheckException(EIntOverflow,
    procedure
    begin
      source.Sum;
    end);
end;

procedure TTestSumInt32.SimpleSum;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([1, 3, 2]);
  CheckEquals(6, source.Sum);
end;

procedure TTestSumInt32.SimpleSumWithSelector;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['x', 'abc', 'de']);
  CheckEquals(6, source.Sum(
    function(x: string): Integer
    begin
      Result := Length(x);
    end));
end;

{ TTestSumInt64 }

procedure TTestSumInt64.EmptySequence;
var
  source: IEnumerable<Int64>;
begin
  source := Enumerable<Int64>.Create;
  CheckEquals(0, source.Sum);
end;

procedure TTestSumInt64.EmptySequenceWithSelector;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>;
  CheckEquals(0, source.Sum(
    function(x: string): Int64
    begin
      Result := Length(x);
    end));
end;

procedure TTestSumInt64.NegativeOverflow;
var
  source: IEnumerable<Int64>;
begin
  source := Enumerable<Int64>.Create([Low(Int64), Low(Int64)]);
  CheckException(EIntOverflow,
    procedure
    begin
      source.Sum;
    end);
end;

procedure TTestSumInt64.NilSelector;
var
  source: IEnumerable<string>;
  selector: TFunc<string, Int64>;
begin
  source := TCollections.CreateList<string>;
  selector := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum(selector);
    end);
end;

procedure TTestSumInt64.NilSelectorNullable;
var
  source: IEnumerable<string>;
  selector: TFunc<string, Int64>;
begin
  source := TCollections.CreateList<string>;
  selector := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum(selector);
    end);
end;

procedure TTestSumInt64.NilSourceNoSelector;
var
  source: IEnumerable<Int64>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum;
    end);
end;

procedure TTestSumInt64.NilSourceNullableNoSelector;
var
  source: IEnumerable<Nullable<Int64>>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum;
    end);
end;

procedure TTestSumInt64.NilSourceNullableWithSelector;
var
  source: IEnumerable<string>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum(
        function(x: string): Nullable<Int64>
        begin
          Result := Length(x);
        end);
    end);
end;

procedure TTestSumInt64.NilSourceWithSelector;
var
  source: IEnumerable<string>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum(
        function(x: string): Int64
        begin
          Result := Length(x);
        end);
    end);
end;

procedure TTestSumInt64.Overflow;
var
  source: IEnumerable<Int64>;
begin
  source := Enumerable<Int64>.Create([Low(Int64), Low(Int64)]);
  CheckException(EIntOverflow,
    procedure
    begin
      source.Sum;
    end);
end;

procedure TTestSumInt64.OverflowOfComputableSum;
var
  source: IEnumerable<Int64>;
begin
  source := Enumerable<Int64>.Create([MaxInt64, 1, -1, -MaxInt64]);
  CheckException(EIntOverflow,
    procedure
    begin
      source.Sum;
    end);
end;

procedure TTestSumInt64.OverflowWithSelector;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['x', 'y']);
  CheckException(EIntOverflow,
    procedure
    begin
      source.Sum(
        function(x: string): Int64
        begin
          Result := MaxInt64;
        end);
    end);
end;

procedure TTestSumInt64.SimpleSum;
var
  source: IEnumerable<Int64>;
begin
  source := Enumerable<Int64>.Create([1, 3, 2]);
  CheckEquals(6, source.Sum);
end;

procedure TTestSumInt64.SimpleSumWithSelector;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['x', 'abc', 'de']);
  CheckEquals(6, source.Sum(
    function(x: string): Int64
    begin
      Result := Length(x);
    end));
end;

{ TTestSumSingle }

procedure TTestSumSingle.EmptySequence;
var
  source: IEnumerable<Single>;
begin
  source := Enumerable<Single>.Create;
  CheckEquals(0, source.Sum);
end;

procedure TTestSumSingle.EmptySequenceWithSelector;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>;
  CheckEquals(0, source.Sum(
    function(x: string): Single
    begin
      Result := Length(x);
    end));
end;

procedure TTestSumSingle.NilSelector;
var
  source: IEnumerable<string>;
  selector: TFunc<string, Single>;
begin
  source := TCollections.CreateList<string>;
  selector := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum(selector);
    end);
end;

procedure TTestSumSingle.NilSelectorNullable;
var
  source: IEnumerable<string>;
  selector: TFunc<string, Single>;
begin
  source := TCollections.CreateList<string>;
  selector := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum(selector);
    end);
end;

procedure TTestSumSingle.NilSourceNoSelector;
var
  source: IEnumerable<Single>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum;
    end);
end;

procedure TTestSumSingle.NilSourceNullableNoSelector;
var
  source: IEnumerable<Nullable<Single>>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum;
    end);
end;

procedure TTestSumSingle.NilSourceNullableWithSelector;
var
  source: IEnumerable<string>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum(
        function(x: string): Nullable<Single>
        begin
          Result := Length(x);
        end);
    end);
end;

procedure TTestSumSingle.NilSourceWithSelector;
var
  source: IEnumerable<string>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum(
        function(x: string): Single
        begin
          Result := Length(x);
        end);
    end);
end;

procedure TTestSumSingle.OverflowToInfinity;
var
  source: IEnumerable<Single>;
begin
  source := Enumerable<Single>.Create([MaxSingle, MaxSingle]);
  CheckException(EOverflow,
    procedure
    begin
      CheckEquals(Infinity, source.Sum);
    end);
end;

procedure TTestSumSingle.OverflowToInfinitySingleWithSelector;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['x', 'y']);
  CheckException(EOverflow,
    procedure
    begin
      CheckEquals(Infinity, source.Sum(
        function(x: string): Single
        begin
          Result := MaxSingle;
        end));
    end);
end;

procedure TTestSumSingle.OverflowToNegativeInfinity;
var
  source: IEnumerable<Single>;
begin
  source := Enumerable<Single>.Create([-MaxSingle, -MaxSingle]);
  CheckException(EOverflow,
    procedure
    begin
      CheckEquals(NegInfinity, source.Sum);
    end);
end;

procedure TTestSumSingle.SimpleSum;
var
  source: IEnumerable<Single>;
begin
  source := Enumerable<Single>.Create([1, 3, 2]);
  CheckEquals(6, source.Sum);
end;

procedure TTestSumSingle.SimpleSumWithSelector;
var
  source: IEnumerable<string>;
begin
  source := TCollections.CreateList<string>(['x', 'abc', 'de']);
  CheckEquals(6, source.Sum(
    function(x: string): Single
    begin
      Result := Length(x);
    end));
end;

{ TTestSumDouble }

procedure TTestSumDouble.NilSelector;
var
  source: IEnumerable<string>;
  selector: TFunc<string, Double>;
begin
  source := TCollections.CreateList<string>;
  selector := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum(selector);
    end);
end;

procedure TTestSumDouble.NilSelectorNullable;
var
  source: IEnumerable<string>;
  selector: TFunc<string, Double>;
begin
  source := TCollections.CreateList<string>;
  selector := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum(selector);
    end);
end;

procedure TTestSumDouble.NilSourceNoSelector;
var
  source: IEnumerable<Double>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum;
    end);
end;

procedure TTestSumDouble.NilSourceNullableNoSelector;
var
  source: IEnumerable<Nullable<Double>>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum;
    end);
end;

procedure TTestSumDouble.NilSourceNullableWithSelector;
var
  source: IEnumerable<string>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum(
        function(x: string): Nullable<Double>
        begin
          Result := Length(x);
        end);
    end);
end;

procedure TTestSumDouble.NilSourceWithSelector;
var
  source: IEnumerable<string>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.Sum(
        function(x: string): Double
        begin
          Result := Length(x);
        end);
    end);
end;

{ TTestMinBy }

procedure TTestMinBy.EmptySequence;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  query := source.MinBy<Integer>(
    function(x: Integer): Integer
    begin
      Result := x mod 3;
    end);
  CheckException(EInvalidOperationException,
    procedure
    var
      iterator: IEnumerator<Integer>;
    begin
      iterator := query.GetEnumerator;
      iterator.MoveNext;
    end);
  query := nil;
end;

procedure TTestMinBy.ExecutionIsDeferred;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  query := source.MinBy<Integer>(
    function(x: Integer): Integer
    begin
      Result := x mod 3;
    end);
end;

procedure TTestMinBy.NilKeySelector;
var
  source: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.MinBy<Integer>(nil);
    end);
end;

procedure TTestMinBy.NilSource;
var
  source: IEnumerable<Integer>;
begin
  source := nil;
  CheckException(EArgumentNilException,
    procedure
    begin
      source.MinBy<Integer>(
        function(x: Integer): Integer
        begin
          Result := x mod 3;
        end);
    end);
end;

procedure TTestMinBy.SimpleMinBy;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([2, 5, 0, 7, 4, 3, 6, 2, 1]);
  query := source.MinBy<Integer>(
    function(x: Integer): Integer
    begin
      Result := x mod 3;
    end);
  CheckTrue(query.EqualsTo([0, 3, 6]));
end;
*)
{ TTestMaxBy }

procedure TTestMaxBy.SimpleMaxBy;
var
  source: IEnumerable<Integer>;
  query: IEnumerable<Integer>;
begin
  source := TCollections.CreateList<Integer>([2, 5, 0, 7, 4, 3, 6, 2, 1]);
  query := TExtremaByIterator<Integer, Integer>.Create(source,
    function(x: Integer): Integer
    begin
      Result := x mod 3;
    end,
    function(key, minValue: Integer): Integer
    begin
      Result := TComparer<Integer>.Default.Compare(key, minValue);
    end);
  CheckTrue(query.EqualsTo([2, 5, 2]));
end;

end.
