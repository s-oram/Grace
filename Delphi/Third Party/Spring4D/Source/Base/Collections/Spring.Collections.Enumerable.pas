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

unit Spring.Collections.Enumerable;

{$I Spring.inc}

interface

uses
  Generics.Defaults,
  SysUtils,
  Spring,
  Spring.Collections;

type
  {$REGION 'Documentation'}
  /// <summary>
  ///   Provides a set of methods for querying objects that implement
  ///   <see cref="Spring.Collections|IEnumerable&lt;T&gt;" />.
  /// </summary>
  /// <typeparam name="T">
  ///   The type of elements to enumerate.
  /// </typeparam>
  {$ENDREGION}
  Enumerable<TSource> = record
  private
    source: IEnumerable<TSource>;

    class function IdentityFunction(x: TSource): TSource; static;
    class function Add(const Left, Right): TSource; static;
  public
    class operator Implicit(const value: IEnumerable<TSource>): Enumerable<TSource>;

    constructor Create(const values: array of TSource);

    function GetEnumerator: IEnumerator<TSource>;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Creates a new array which is filled with the elements in the
    ///   collection.
    /// </summary>
    {$ENDREGION}
    function ToArray: TArray<TSource>;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Creates an IDictionary&lt;TKey, TValue&gt; from an IEnumerable&lt;T&
    ///   gt; according to a specified key selector function.
    /// </summary>
    {$ENDREGION}
    function ToDictionary<TKey>(const keySelector: TFunc<TSource, TKey>): IDictionary<TKey, TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Creates an IDictionary&lt;TKey, TValue&gt; from an IEnumerable&lt;T&
    ///   gt; according to a specified key selector function and key comparer.
    /// </summary>
    {$ENDREGION}
    function ToDictionary<TKey>(const keySelector: TFunc<TSource, TKey>;
      const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Creates an Dictionary&lt;TKey, TValue&gt; from an IEnumerable&lt;T&gt;
    ///    according to specified key selector and element selector functions.
    /// </summary>
    {$ENDREGION}
    function ToDictionary<TKey, TElement>(const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>): IDictionary<TKey, TElement>; overload;

    function ToDictionary<TKey, TElement>(const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>;
      const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TElement>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Creates a new list which is filled with the elements in the
    ///   collection.
    /// </summary>
    {$ENDREGION}
    function ToList: IList<TSource>;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Creates a new set which is filled with the elements in the collection.
    /// </summary>
    {$ENDREGION}
    function ToSet: ISet<TSource>;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Applies an accumulator function over a sequence.
    /// </summary>
    {$ENDREGION}
    function Aggregate(const func: TFunc<TSource, TSource, TSource>): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Applies an accumulator function over a sequence. The specified seed
    ///   value is used as the initial accumulator value.
    /// </summary>
    {$ENDREGION}
    function Aggregate<TAccumulate>(const seed: TAccumulate;
      const func: TFunc<TAccumulate, TSource, TAccumulate>): TAccumulate; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Applies an accumulator function over a sequence. The specified seed
    ///   value is used as the initial accumulator value, and the specified
    ///   function is used to select the result value.
    /// </summary>
    {$ENDREGION}
    function Aggregate<TAccumulate, TResult>(const seed: TAccumulate;
      const func: TFunc<TAccumulate, TSource, TAccumulate>;
      const resultSelector: TFunc<TAccumulate, TResult>): TResult; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Determines whether all elements of a sequence satisfy a condition.
    /// </summary>
    {$ENDREGION}
    function All(const predicate: TPredicate<TSource>): Boolean;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Determines whether a sequence contains any elements.
    /// </summary>
    {$ENDREGION}
    function Any: Boolean; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Determines whether any element of a sequence satisfies a condition.
    /// </summary>
    {$ENDREGION}
    function Any(const predicate: TPredicate<TSource>): Boolean; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Computes the average of a sequence of Double values that are obtained
    ///   by invoking a transform function on each element of the input
    ///   sequence.
    /// </summary>
    {$ENDREGION}
    function Average(const selector: TFunc<TSource, Double>): Double; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Computes the average of a sequence of Int64 values that are obtained
    ///   by invoking a transform function on each element of the input
    ///   sequence.
    /// </summary>
    {$ENDREGION}
    function Average(const selector: TFunc<TSource, Int64>): Double; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Casts the elements of the sequence to the specified type.
    /// </summary>
    {$ENDREGION}
    function Cast<TResult>: IEnumerable<TResult>;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Concatenates two sequences.
    /// </summary>
    {$ENDREGION}
    function Concat(const second: array of TSource): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Concatenates two sequences.
    /// </summary>
    {$ENDREGION}
    function Concat(const second: IEnumerable<TSource>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Determines whether a sequence contains a specified element by using
    ///   the default equality comparer.
    /// </summary>
    {$ENDREGION}
    function Contains(const item: TSource): Boolean; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Determines whether a sequence contains a specified element by using a
    ///   specified <c>IEqualityComparer&lt;T&gt;</c>.
    /// </summary>
    {$ENDREGION}
    function Contains(const item: TSource; const comparer: IEqualityComparer<TSource>): Boolean; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the number of elements in a sequence.
    /// </summary>
    {$ENDREGION}
    function Count: Integer; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns a number that represents how many elements in the specified
    ///   sequence satisfy a condition.
    /// </summary>
    {$ENDREGION}
    function Count(const predicate: TPredicate<TSource>): Integer; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the elements of the specified sequence or the type parameter'
    ///   s default value in a singleton collection if the sequence is empty.
    /// </summary>
    {$ENDREGION}
    function DefaultIfEmpty: IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the elements of the specified sequence or the specified value
    ///   in a singleton collection if the sequence is empty.
    /// </summary>
    {$ENDREGION}
    function DefaultIfEmpty(const defaultValue: TSource): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns distinct elements from a sequence by using the default
    ///   equality comparer to compare values.
    /// </summary>
    {$ENDREGION}
    function Distinct: IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns distinct elements from a sequence by using a specified
    ///   <c>IEqualityComparer&lt;T&gt;</c> to compare values.
    /// </summary>
    {$ENDREGION}
    function Distinct(const comparer: IEqualityComparer<TSource>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the element at a specified index in a sequence.
    /// </summary>
    {$ENDREGION}
    function ElementAt(index: Integer): TSource;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the element at a specified index in a sequence or a default
    ///   value if the index is out of range.
    /// </summary>
    {$ENDREGION}
    function ElementAtOrDefault(index: Integer): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the element at a specified index in a sequence or the
    ///   specified default value if the index is out of range.
    /// </summary>
    {$ENDREGION}
    function ElementAtOrDefault(index: Integer; const defaultValue: TSource): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Produces the set difference of two sequences by using the default
    ///   equality comparer to compare values.
    /// </summary>
    {$ENDREGION}
    function ExceptWith(const second: array of TSource): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Produces the set difference of two sequences by using the default
    ///   equality comparer to compare values.
    /// </summary>
    {$ENDREGION}
    function ExceptWith(const second: IEnumerable<TSource>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Produces the set difference of two sequences by using the specified
    ///   IEqualityComparer{T} to compare values.
    /// </summary>
    {$ENDREGION}
    function ExceptWith(const second: IEnumerable<TSource>; const comparer: IEqualityComparer<TSource>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Performs the specified action on each element of a sequence.
    /// </summary>
    {$ENDREGION}
    procedure ForEach(const action: TAction<TSource>);

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the first element of a sequence.
    /// </summary>
    {$ENDREGION}
    function First: TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the first element in a sequence that satisfies a specified
    ///   condition.
    /// </summary>
    {$ENDREGION}
    function First(const predicate: TPredicate<TSource>): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the first element of a sequence, or a default value if the
    ///   sequence contains no elements.
    /// </summary>
    {$ENDREGION}
    function FirstOrDefault: TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the first element of a sequence, or the specified default
    ///   value if the sequence contains no elements.
    /// </summary>
    {$ENDREGION}
    function FirstOrDefault(const defaultValue: TSource): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the first element of the sequence that satisfies a condition
    ///   or a default value if no such element is found.
    /// </summary>
    {$ENDREGION}
    function FirstOrDefault(const predicate: TPredicate<TSource>): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the first element of the sequence that satisfies a condition
    ///   or the specified default value if no such element is found.
    /// </summary>
    {$ENDREGION}
    function FirstOrDefault(const predicate: TPredicate<TSource>; const defaultValue: TSource): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Groups the elements of a sequence according to a specified key
    ///   selector function.
    /// </summary>
    {$ENDREGION}
    function GroupBy<TKey>(const keySelector: TFunc<TSource, TKey>): IEnumerable<IGrouping<TKey, TSource>>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Groups the elements of a sequence according to a specified key
    ///   selector function and compares the keys by using a specified comparer.
    /// </summary>
    {$ENDREGION}
    function GroupBy<TKey>(const keySelector: TFunc<TSource, TKey>;
      const comparer: IEqualityComparer<TKey>): IEnumerable<IGrouping<TKey, TSource>>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Groups the elements of a sequence according to a specified key
    ///   selector function and projects the elements for each group by using a
    ///   specified function.
    /// </summary>
    {$ENDREGION}
    function GroupBy<TKey, TElement>(const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>): IEnumerable<IGrouping<TKey, TElement>>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Groups the elements of a sequence according to a key selector
    ///   function. The keys are compared by using a comparer and each group's
    ///   elements are projected by using a specified function.
    /// </summary>
    {$ENDREGION}
    function GroupBy<TKey, TElement>(const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>;
      const comparer: IEqualityComparer<TKey>): IEnumerable<IGrouping<TKey, TElement>>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Groups the elements of a sequence according to a specified key
    ///   selector function and creates a result value from each group and its
    ///   key.
    /// </summary>
    {$ENDREGION}
    function GroupBy<TKey, TResult>(const keySelector: TFunc<TSource, TKey>;
      const resultSelector: TFunc<TKey, IEnumerable<TSource>, TResult>): IEnumerable<TResult>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Groups the elements of a sequence according to a specified key
    ///   selector function and creates a result value from each group and its
    ///   key. The keys are compared by using a specified comparer.
    /// </summary>
    {$ENDREGION}
    function GroupBy<TKey, TResult>(const keySelector: TFunc<TSource, TKey>;
      const resultSelector: TFunc<TKey, IEnumerable<TSource>, TResult>;
      const comparer: IEqualityComparer<TKey>): IEnumerable<TResult>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Groups the elements of a sequence according to a specified key
    ///   selector function and creates a result value from each group and its
    ///   key. The elements of each group are projected by using a specified
    ///   function.
    /// </summary>
    {$ENDREGION}
    function GroupBy<TKey, TElement, TResult>(const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>;
      const resultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>): IEnumerable<TResult>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Groups the elements of a sequence according to a specified key
    ///   selector function and creates a result value from each group and its
    ///   key. Key values are compared by using a specified comparer, and the
    ///   elements of each group are projected by using a specified function.
    /// </summary>
    {$ENDREGION}
    function GroupBy<TKey, TElement, TResult>(const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>;
      const resultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>;
      const comparer: IEqualityComparer<TKey>): IEnumerable<TResult>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Correlates the elements of two sequences based on equality of keys
    ///   and groups the results. The default equality comparer is used to
    ///   compare keys.
    /// </summary>
    {$ENDREGION}
    function GroupJoin<TInner, TKey, TResult>(const inner: IEnumerable<TInner>;
      const outerKeySelector: TFunc<TSource, TKey>;
      const innerKeySelector: TFunc<TInner, TKey>;
      const resultSelector: TFunc<TSource, IEnumerable<TInner>, TResult>): IEnumerable<TResult>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Correlates the elements of two sequences based on key equality and
    ///   groups the results. A specified IEqualityComparer{T} is used to
    ///   compare keys.
    /// </summary>
    {$ENDREGION}
    function GroupJoin<TInner, TKey, TResult>(const inner: IEnumerable<TInner>;
      const outerKeySelector: TFunc<TSource, TKey>;
      const innerKeySelector: TFunc<TInner, TKey>;
      const resultSelector: TFunc<TSource, IEnumerable<TInner>, TResult>;
      const comparer: IEqualityComparer<TKey>): IEnumerable<TResult>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Produces the set intersection of two sequences by using the default
    ///   equality comparer to compare values.
    /// </summary>
    {$ENDREGION}
    function IntersectWith(const second: array of TSource): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Produces the set intersection of two sequences by using the default
    ///   equality comparer to compare values.
    /// </summary>
    {$ENDREGION}
    function IntersectWith(const second: IEnumerable<TSource>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Produces the set intersection of two sequences by using the specified
    ///   IEqualityComparer{T} to compare values.
    /// </summary>
    {$ENDREGION}
    function IntersectWith(const second: IEnumerable<TSource>; const comparer: IEqualityComparer<TSource>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Correlates the elements of two sequences based on matching keys. The
    ///   default equality comparer is used to compare keys.
    /// </summary>
    {$ENDREGION}
    function Join<TInner, TKey, TResult>(const inner: IEnumerable<TInner>;
      const outerKeySelector: TFunc<TSource, TKey>;
      const innerKeySelector: TFunc<TInner, TKey>;
      const resultSelector: TFunc<TSource, TInner, TResult>): IEnumerable<TResult>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Correlates the elements of two sequences based on matching keys. A
    ///   specified IEqualityComparer{T} is used to compare keys.
    /// </summary>
    {$ENDREGION}
    function Join<TInner, TKey, TResult>(const inner: IEnumerable<TInner>;
      const outerKeySelector: TFunc<TSource, TKey>;
      const innerKeySelector: TFunc<TInner, TKey>;
      const resultSelector: TFunc<TSource, TInner, TResult>;
      const comparer: IEqualityComparer<TKey>): IEnumerable<TResult>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the last element of a sequence.
    /// </summary>
    {$ENDREGION}
    function Last: TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the last element of a sequence that satisfies a specified
    ///   condition.
    /// </summary>
    {$ENDREGION}
    function Last(const predicate: TPredicate<TSource>): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the last element of a sequence, or a default value if the
    ///   sequence contains no elements.
    /// </summary>
    {$ENDREGION}
    function LastOrDefault: TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the last element of a sequence, or the specified default
    ///   value if the sequence contains no elements.
    /// </summary>
    {$ENDREGION}
    function LastOrDefault(const defaultValue: TSource): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the last element of a sequence that satisfies a condition or
    ///   a default value if no such element is found.
    /// </summary>
    {$ENDREGION}
    function LastOrDefault(const predicate: TPredicate<TSource>): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the last element of a sequence that satisfies a condition or
    ///   the specified default value if no such element is found.
    /// </summary>
    {$ENDREGION}
    function LastOrDefault(const predicate: TPredicate<TSource>; const defaultValue: TSource): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the maximum value in a sequence.
    /// </summary>
    {$ENDREGION}
    function Max: TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Invokes a transform function on each element of a sequence and
    ///   returns the maximum Double value.
    /// </summary>
    {$ENDREGION}
    function Max(const selector: TFunc<TSource, Double>): Double; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Invokes a transform function on each element of a sequence and
    ///   returns the maximum Int64 value.
    /// </summary>
    {$ENDREGION}
    function Max(const selector: TFunc<TSource, Int64>): Int64; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the elements with the maximum key value by using the default
    ///   comparer to compare key values.
    /// </summary>
    {$ENDREGION}
    function MaxBy<TKey>(const selector: TFunc<TSource, TKey>): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the elements with the minimum key value by using the
    ///   specified comparer to compare key values.
    /// </summary>
    {$ENDREGION}
    function MaxBy<TKey>(const selector: TFunc<TSource, TKey>; const comparer: IComparer<TKey>): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the minimum value in a sequence.
    /// </summary>
    {$ENDREGION}
    function Min: TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Invokes a transform function on each element of a sequence and
    ///   returns the minumum Double value.
    /// </summary>
    {$ENDREGION}
    function Min(const selector: TFunc<TSource, Double>): Double; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Invokes a transform function on each element of a sequence and
    ///   returns the minimum Int64 value.
    /// </summary>
    {$ENDREGION}
    function Min(const selector: TFunc<TSource, Int64>): Int64; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the elements with the minimum key value by using the default
    ///   comparer to compare key values.
    /// </summary>
    {$ENDREGION}
    function MinBy<TKey>(const selector: TFunc<TSource, TKey>): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the elements with the minimum key value by using the
    ///   specified comparer to compare key values.
    /// </summary>
    {$ENDREGION}
    function MinBy<TKey>(const selector: TFunc<TSource, TKey>; const comparer: IComparer<TKey>): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Filters the elements of a sequence based on a specified type.
    /// </summary>
    {$ENDREGION}
    function OfType<TResult>: IEnumerable<TResult>;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Sorts the elements of a sequence in ascending order according to a
    ///   key.
    /// </summary>
    {$ENDREGION}
    function OrderBy<TKey>(const keySelector: TFunc<TSource, TKey>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Sorts the elements of a sequence in ascending order by using a
    ///   specified comparer.
    /// </summary>
    {$ENDREGION}
    function OrderBy<TKey>(const keySelector: TFunc<TSource, TKey>;
      const comparer: IComparer<TKey>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Sorts the elements of a sequence in descending order according to a
    ///   key.
    /// </summary>
    {$ENDREGION}
    function OrderByDescending<TKey>(
      const keySelector: TFunc<TSource, TKey>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Sorts the elements of a sequence in descending order by using a
    ///   specified comparer.
    /// </summary>
    {$ENDREGION}
    function OrderByDescending<TKey>(const keySelector: TFunc<TSource, TKey>;
      const comparer: IComparer<TKey>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Inverts the order of the elements in a sequence.
    /// </summary>
    {$ENDREGION}
    function Reverse: IEnumerable<TSource>;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Projects each element of a sequence into a new form.
    /// </summary>
    {$ENDREGION}
    function Select(const selector: TFunc<TSource, TSource>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Projects each element of a sequence into a new form.
    /// </summary>
    {$ENDREGION}
    function Select<TResult>(
      const selector: TFunc<TSource, TResult>): IEnumerable<TResult>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Projects each element of a sequence into a new form by incorporating
    ///   the element's index.
    /// </summary>
    {$ENDREGION}
    function Select(const selector: TFunc<TSource, Integer, TSource>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Projects each element of a sequence into a new form by incorporating
    ///   the element's index.
    /// </summary>
    {$ENDREGION}
    function Select<TResult>(
      const selector: TFunc<TSource, Integer, TResult>): IEnumerable<TResult>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Projects each element of a sequence to an IEnumerable{T} and flattens
    ///   the resulting sequences into one sequence.
    /// </summary>
    {$ENDREGION}
    function SelectMany<TResult>(
      const selector: TFunc<TSource, IEnumerable<TResult>>): IEnumerable<TResult>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Projects each element of a sequence to an IEnumerable{T}, and
    ///   flattens the resulting sequences into one sequence. The index of each
    ///   source element is used in the projected form of that element.
    /// </summary>
    {$ENDREGION}
    function SelectMany<TResult>(
      const selector: TFunc<TSource, Integer, IEnumerable<TResult>>): IEnumerable<TResult>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Projects each element of a sequence to an IEnumerable{T}, flattens
    ///   the resulting sequences into one sequence, and invokes a result
    ///   selector function on each element therein.
    /// </summary>
    {$ENDREGION}
    function SelectMany<TCollection, TResult>(
      const collectionSelector: TFunc<TSource, IEnumerable<TCollection>>;
      const resultSelector: TFunc<TSource, TCollection, TResult>): IEnumerable<TResult>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Projects each element of a sequence to an IEnumerable{T}, flattens
    ///   the resulting sequences into one sequence, and invokes a result
    ///   selector function on each element therein. The index of each source
    ///   element is used in the intermediate projected form of that element.
    /// </summary>
    {$ENDREGION}
    function SelectMany<TCollection, TResult>(
      const collectionSelector: TFunc<TSource, Integer, IEnumerable<TCollection>>;
      const resultSelector: TFunc<TSource, TCollection, TResult>): IEnumerable<TResult>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Determines whether two sequences are equal by comparing the elements
    ///   by using the default equality comparer for their type.
    /// </summary>
    {$ENDREGION}
    function SequenceEqual(const second: array of TSource): Boolean; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Determines whether two sequences are equal by comparing the elements
    ///   by using the default equality comparer for their type.
    /// </summary>
    {$ENDREGION}
    function SequenceEqual(const second: IEnumerable<TSource>): Boolean; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Determines whether two sequences are equal by comparing their
    ///   elements by using a specified IEqualityComparer{T}.
    /// </summary>
    {$ENDREGION}
    function SequenceEqual(const second: IEnumerable<TSource>; comparer: IEqualityComparer<TSource>): Boolean; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the only element of a sequence, and throws an exception if
    ///   there is not exactly one element in the sequence.
    /// </summary>
    {$ENDREGION}
    function Single: TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the only element of a sequence, and throws an exception if
    ///   there is not exactly one element in the sequence.
    /// </summary>
    {$ENDREGION}
    function Single(const predicate: TPredicate<TSource>): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the only element of a sequence, or a default value if the
    ///   sequence is empty; this method throws an exception if there is more
    ///   than one element in the sequence.
    /// </summary>
    {$ENDREGION}
    function SingleOrDefault: TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the only element of a sequence, or the specified default
    ///   value if the sequence is empty; this method throws an exception if
    ///   there is more than one element in the sequence.
    /// </summary>
    {$ENDREGION}
    function SingleOrDefault(const defaultValue: TSource): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the only element of a sequence that satisfies a specified
    ///   condition or a default value if no such element exists; this method
    ///   throws an exception if more than one element satisfies the condition.
    /// </summary>
    {$ENDREGION}
    function SingleOrDefault(const predicate: TPredicate<TSource>): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the only element of a sequence that satisfies a specified
    ///   condition or the specified default value if no such element exists;
    ///   this method throws an exception if more than one element satisfies
    ///   the condition.
    /// </summary>
    {$ENDREGION}
    function SingleOrDefault(const predicate: TPredicate<TSource>; const defaultValue: TSource): TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Bypasses a specified number of elements in a sequence and then
    ///   returns the remaining elements.
    /// </summary>
    {$ENDREGION}
    function Skip(count: Integer): IEnumerable<TSource>;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Bypasses elements in a sequence as long as a specified condition is
    ///   true and then returns the remaining elements.
    /// </summary>
    {$ENDREGION}
    function SkipWhile(const predicate: TPredicate<TSource>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Bypasses elements in a sequence as long as a specified condition is
    ///   true and then returns the remaining elements. The element's index is
    ///   used in the logic of the predicate function.
    /// </summary>
    {$ENDREGION}
    function SkipWhile(const predicate: TFunc<TSource, Integer, Boolean>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Computes the sum of a sequence.
    /// </summary>
    {$ENDREGION}
    function Sum: TSource; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Computes the sum of the sequence of Integer values that are obtained
    ///   by invoking a transform function on each element of the input
    ///   sequence.
    /// </summary>
    {$ENDREGION}
    function Sum(const selector: TFunc<TSource, Integer>): Integer; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Computes the sum of the sequence of nullable Integer values that are
    ///   obtained by invoking a transform function on each element of the
    ///   input sequence.
    /// </summary>
    {$ENDREGION}
    function Sum(const selector: TFunc<TSource, Nullable<Integer>>): Nullable<Integer>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Computes the sum of the sequence of Int64 values that are obtained by
    ///   invoking a transform function on each element of the input sequence.
    /// </summary>
    {$ENDREGION}
    function Sum(const selector: TFunc<TSource, Int64>): Int64; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Computes the sum of the sequence of nullable Int64 values that are
    ///   obtained by invoking a transform function on each element of the
    ///   input sequence.
    /// </summary>
    {$ENDREGION}
    function Sum(const selector: TFunc<TSource, Nullable<Int64>>): Nullable<Int64>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Computes the sum of the sequence of Single values that are obtained
    ///   by invoking a transform function on each element of the input
    ///   sequence.
    /// </summary>
    {$ENDREGION}
    function Sum(const selector: TFunc<TSource, Single>): Single; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Computes the sum of the sequence of nullable Single values that are
    ///   obtained by invoking a transform function on each element of the
    ///   input sequence.
    /// </summary>
    {$ENDREGION}
    function Sum(const selector: TFunc<TSource, Nullable<Single>>): Nullable<Single>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Computes the sum of the sequence of Double values that are obtained
    ///   by invoking a transform function on each element of the input
    ///   sequence.
    /// </summary>
    {$ENDREGION}
    function Sum(const selector: TFunc<TSource, Double>): Double; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Computes the sum of the sequence of nullable Double values that are
    ///   obtained by invoking a transform function on each element of the
    ///   input sequence.
    /// </summary>
    {$ENDREGION}
    function Sum(const selector: TFunc<TSource, Nullable<Double>>): Nullable<Double>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns a specified number of contiguous elements from the start of a
    ///   sequence.
    /// </summary>
    {$ENDREGION}
    function Take(count: Integer): IEnumerable<TSource>;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns elements from a sequence as long as a specified condition is
    ///   true.
    /// </summary>
    {$ENDREGION}
    function TakeWhile(const predicate: TPredicate<TSource>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns elements from a sequence as long as a specified condition is
    ///   true. The element's index is used in the logic of the predicate
    ///   function.
    /// </summary>
    {$ENDREGION}
    function TakeWhile(const predicate: TFunc<TSource, Integer, Boolean>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Performs a subsequent ordering of the elements in a sequence in
    ///   ascending order according to a key.
    /// </summary>
    {$ENDREGION}
    function ThenBy<TKey>(const keySelector: TFunc<TSource, TKey>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Performs a subsequent ordering of the elements in a sequence in
    ///   ascending order by using a specified comparer.
    /// </summary>
    {$ENDREGION}
    function ThenBy<TKey>(const keySelector: TFunc<TSource, TKey>;
      const comparer: IComparer<TKey>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Performs a subsequent ordering of the elements in a sequence in
    ///   descending order, according to a key.
    /// </summary>
    {$ENDREGION}
    function ThenByDescending<TKey>(
      const keySelector: TFunc<TSource, TKey>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Performs a subsequent ordering of the elements in a sequence in
    ///   descending order by using a specified comparer.
    /// </summary>
    {$ENDREGION}
    function ThenByDescending<TKey>(const keySelector: TFunc<TSource, TKey>;
      const comparer: IComparer<TKey>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Creates a Lookup{TKey, TElement} from an IEnumerable{T} according to
    ///   a specified key selector function.
    /// </summary>
    {$ENDREGION}
    function ToLookup(const keySelector: TFunc<TSource, TSource>): ILookup<TSource, TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Creates a Lookup{TKey, TElement} from an IEnumerable{T} according to
    ///   a specified key selector function.
    /// </summary>
    {$ENDREGION}
    function ToLookup<TKey>(const keySelector: TFunc<TSource, TKey>): ILookup<TKey, TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Creates a Lookup{TKey, TElement} from an IEnumerable{T} according to
    ///   a specified key selector function and key comparer.
    /// </summary>
    {$ENDREGION}
    function ToLookup(const keySelector: TFunc<TSource, TSource>;
      const comparer: IEqualityComparer<TSource>): ILookup<TSource, TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Creates a Lookup{TKey, TElement} from an IEnumerable{T} according to
    ///   a specified key selector function and key comparer.
    /// </summary>
    {$ENDREGION}
    function ToLookup<TKey>(const keySelector: TFunc<TSource, TKey>;
      const comparer: IEqualityComparer<TKey>): ILookup<TKey, TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Creates a TLookup{TKey, TElement} from an IEnumerable{T} according to
    ///   specified key selector and element selector functions.
    /// </summary>
    {$ENDREGION}
    function ToLookup<TKey, TElement>(const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>): ILookup<TKey, TElement>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Creates a Lookup{TKey, TElement} from an IEnumerable{T} according to
    ///   a specified key selector function, a comparer and an element selector
    ///   function.
    /// </summary>
    {$ENDREGION}
    function ToLookup<TKey, TElement>(const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>;
      const comparer: IEqualityComparer<TKey>): ILookup<TKey, TElement>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Try getting the first element of a sequence.
    /// </summary>
    {$ENDREGION}
    function TryGetFirst(out value: TSource): Boolean; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Returns the first element in a sequence that satisfies a specified
    ///   condition.
    /// </summary>
    {$ENDREGION}
    function TryGetFirst(out value: TSource; const predicate: TPredicate<TSource>): Boolean; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Try getting the last element of a sequence.
    /// </summary>
    {$ENDREGION}
    function TryGetLast(out value: TSource): Boolean; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Try getting the last element in a sequence that satisfies a specified
    ///   condition.
    /// </summary>
    {$ENDREGION}
    function TryGetLast(out value: TSource; const predicate: TPredicate<TSource>): Boolean; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Produces the set union of two sequences by using the default equality
    ///   comparer.
    /// </summary>
    {$ENDREGION}
    function UnionWith(const second: array of TSource): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Produces the set union of two sequences by using the default equality
    ///   comparer.
    /// </summary>
    {$ENDREGION}
    function UnionWith(const second: IEnumerable<TSource>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Produces the set union of two sequences by using a specified
    ///   IEqualityComparer{T}.
    /// </summary>
    {$ENDREGION}
    function UnionWith(const second: IEnumerable<TSource>; const comparer: IEqualityComparer<TSource>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Filters a sequence of values based on a predicate.
    /// </summary>
    {$ENDREGION}
    function Where(const predicate: TPredicate<TSource>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Filters a sequence of values based on a predicate. Each element's
    ///   index is used in the logic of the predicate function.
    /// </summary>
    {$ENDREGION}
    function Where(const predicate: TFunc<TSource, Integer, Boolean>): IEnumerable<TSource>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Applies a specified function to the corresponding elements of two
    ///   sequences, which produces a sequence of the results.
    /// </summary>
    {$ENDREGION}
    function Zip<TSecond, TResult>(const second: IEnumerable<TSecond>;
      const resultSelector: TFunc<TSource, TSecond, TResult>): IEnumerable<TResult>;
  end;

implementation

uses
  TypInfo,
  Spring.Collections.Extensions,
  Spring.ResourceStrings;


{$REGION 'Enumerable<TSource>'}

class function Enumerable<TSource>.IdentityFunction(x: TSource): TSource;
begin
  Result := x;
end;

constructor Enumerable<TSource>.Create(const values: array of TSource);
begin
  source := TArrayIterator<TSource>.Create(values);
end;

class operator Enumerable<TSource>.Implicit(
  const value: IEnumerable<TSource>): Enumerable<TSource>;
begin
  Result.source := value;
end;

class function Enumerable<TSource>.Add(const Left, Right): TSource;
begin
  case PTypeInfo(TypeInfo(TSource)).Kind of
    tkInteger: PInteger(@Result)^ := Integer(Left) + Integer(Right);
    tkInt64: PInt64(@Result)^ := Int64(Left) + Int64(Right);
    tkFloat:
    begin
      case GetTypeData(TypeInfo(TSource)).FloatType of
        ftSingle: PSingle(@Result)^ := System.Single(Left) + System.Single(Right);
        ftDouble: PDouble(@Result)^ := Double(Left) + Double(Right);
        ftExtended: PExtended(@Result)^ := Extended(Left) + Extended(Right);
        ftComp: PComp(@Result)^ := Comp(Left) + Comp(Right);
        ftCurr: PCurrency(@Result)^ := Currency(Left) + Currency(Right);
      end;
    end;
  end;
end;

function Enumerable<TSource>.Aggregate(const func: TFunc<TSource, TSource, TSource>): TSource;
var
  enumerator: IEnumerator<TSource>;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(func), 'func');

  enumerator := GetEnumerator;
  if not enumerator.MoveNext then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
  Result := enumerator.Current;
  while enumerator.MoveNext do
    Result := func(Result, enumerator.Current);
end;

function Enumerable<TSource>.Aggregate<TAccumulate>(const seed: TAccumulate;
  const func: TFunc<TAccumulate, TSource, TAccumulate>): TAccumulate;
var
  enumerator: IEnumerator<TSource>;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(func), 'func');

  enumerator := GetEnumerator;
  Result := seed;
  while enumerator.MoveNext do
    Result := func(Result, enumerator.Current);
end;

function Enumerable<TSource>.Aggregate<TAccumulate, TResult>(const seed: TAccumulate;
  const func: TFunc<TAccumulate, TSource, TAccumulate>;
  const resultSelector: TFunc<TAccumulate, TResult>): TResult;
var
  enumerator: IEnumerator<TSource>;
  accumulate: TAccumulate;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(func), 'func');
  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');

  enumerator := GetEnumerator;
  accumulate := seed;
  while enumerator.MoveNext do
    accumulate := func(accumulate, enumerator.Current);
  Result := resultSelector(accumulate);
end;

function Enumerable<TSource>.All(const predicate: TPredicate<TSource>): Boolean;
var
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := True;
  for item in source do
    if not predicate(item) then
      Exit(False);
end;

function Enumerable<TSource>.Any: Boolean;
var
  enumerator: IEnumerator;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  enumerator := GetEnumerator;
  Result := enumerator.MoveNext;
end;

function Enumerable<TSource>.Any(const predicate: TPredicate<TSource>): Boolean;
var
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := False;
  for item in source do
    if predicate(item) then
      Exit(True);
end;

function Enumerable<TSource>.Average(const selector: TFunc<TSource, Double>): Double;
var
  sum: Double;
  count: Int64;
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  sum := 0;
  count := 0;
  for item in source do
  begin
    sum := sum + selector(item);
    Inc(count);
  end;
  if count > 0 then
    Result := sum / count
  else
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
end;

function Enumerable<TSource>.Average(const selector: TFunc<TSource, Int64>): Double;
var
  sum: Int64;
  count: Int64;
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  sum := 0;
  count := 0;
  for item in source do
  begin
    sum := sum + selector(item);
    Inc(count);
  end;
  if count > 0 then
    Result := sum / count
  else
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
end;

function Enumerable<TSource>.Cast<TResult>: IEnumerable<TResult>;
begin
  Result := TCastIterator<TSource, TResult>.Create(source);
end;

function Enumerable<TSource>.Concat(const second: array of TSource): IEnumerable<TSource>;
begin
  Result := Concat(TArrayIterator<TSource>.Create(second));
end;

function Enumerable<TSource>.Concat(const second: IEnumerable<TSource>): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'first');
//  Guard.CheckNotNull(Assigned(second), 'second');

  Result := TConcatIterator<TSource>.Create(source, second);
end;

function Enumerable<TSource>.Contains(const item: TSource): Boolean;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.Contains(item);
end;

function Enumerable<TSource>.Contains(const item: TSource;
  const comparer: IEqualityComparer<TSource>): Boolean;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.Contains(item, comparer);
end;

function Enumerable<TSource>.Count: Integer;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.Count;
end;

function Enumerable<TSource>.Count(const predicate: TPredicate<TSource>): Integer;
var
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := 0;
  for item in source do
    if predicate(item) then
      Inc(Result);
end;

function Enumerable<TSource>.DefaultIfEmpty: IEnumerable<TSource>;
begin
  Result := DefaultIfEmpty(Default(TSource));
end;

function Enumerable<TSource>.DefaultIfEmpty(const defaultValue: TSource): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');

  Result := TDefaultIfEmptyIterator<TSource>.Create(source, defaultValue);
end;

function Enumerable<TSource>.Distinct: IEnumerable<TSource>;
begin
  Result := Distinct(nil);
end;

function Enumerable<TSource>.Distinct(
  const comparer: IEqualityComparer<TSource>): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');

  Result := TDistinctIterator<TSource>.Create(source, comparer);
end;

function Enumerable<TSource>.ElementAt(index: Integer): TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.ElementAt(index);
end;

function Enumerable<TSource>.ElementAtOrDefault(index: Integer): TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.ElementAtOrDefault(index);
end;

function Enumerable<TSource>.ElementAtOrDefault(index: Integer;
  const defaultValue: TSource): TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.ElementAtOrDefault(index, defaultValue);
end;

function Enumerable<TSource>.ExceptWith(const second: array of TSource): IEnumerable<TSource>;
begin
  Result := ExceptWith(TArrayIterator<TSource>.Create(second));
end;

function Enumerable<TSource>.ExceptWith(const second: IEnumerable<TSource>): IEnumerable<TSource>;
begin
  Result := ExceptWith(second, nil);
end;

function Enumerable<TSource>.ExceptWith(const second: IEnumerable<TSource>;
  const comparer: IEqualityComparer<TSource>): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'first');
//  Guard.CheckNotNull(Assigned(second), 'second');

  Result := TExceptIterator<TSource>.Create(source, second, comparer);
end;

function Enumerable<TSource>.First: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.First;
end;

function Enumerable<TSource>.First(const predicate: TPredicate<TSource>): TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := source.First(predicate);
end;

function Enumerable<TSource>.FirstOrDefault: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.FirstOrDefault;
end;

function Enumerable<TSource>.FirstOrDefault(const defaultValue: TSource): TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.FirstOrDefault(defaultValue);
end;

function Enumerable<TSource>.FirstOrDefault(const predicate: TPredicate<TSource>): TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := source.FirstOrDefault(predicate);
end;

function Enumerable<TSource>.FirstOrDefault(const predicate: TPredicate<TSource>;
  const defaultValue: TSource): TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := source.FirstOrDefault(predicate, defaultValue);
end;

procedure Enumerable<TSource>.ForEach(const action: TAction<TSource>);
var
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(action), 'action');

  for item in source do
    action(item);
end;

function Enumerable<TSource>.GetEnumerator: IEnumerator<TSource>;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.GetEnumerator;
end;

function Enumerable<TSource>.GroupBy<TKey>(
  const keySelector: TFunc<TSource, TKey>): IEnumerable<IGrouping<TKey, TSource>>;
begin
  Result := GroupBy<TKey, TSource>(keySelector, IdentityFunction, nil);
end;

function Enumerable<TSource>.GroupBy<TKey>(const keySelector: TFunc<TSource, TKey>;
  const comparer: IEqualityComparer<TKey>): IEnumerable<IGrouping<TKey, TSource>>;
begin
  Result := GroupBy<TKey, TSource>(keySelector, IdentityFunction, comparer)
end;

function Enumerable<TSource>.GroupBy<TKey, TElement>(
  const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>): IEnumerable<IGrouping<TKey, TElement>>;
begin
  Result := GroupBy<TKey, TElement>(keySelector, elementSelector, nil);
end;

function Enumerable<TSource>.GroupBy<TKey, TElement>(
  const keySelector: TFunc<TSource, TKey>; const elementSelector: TFunc<TSource, TElement>;
  const comparer: IEqualityComparer<TKey>): IEnumerable<IGrouping<TKey, TElement>>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');
//  Guard.CheckNotNull(Assigned(elementSelector), 'elementSelector');

  Result := TGroupedEnumerable<TSource, TKey, TElement>.Create(
    source, keySelector, elementSelector, comparer);
end;

function Enumerable<TSource>.GroupBy<TKey, TResult>(const keySelector: TFunc<TSource, TKey>;
  const resultSelector: TFunc<TKey, IEnumerable<TSource>, TResult>): IEnumerable<TResult>;
begin
  Result := GroupBy<TKey, TSource, TResult>(
    keySelector, IdentityFunction, resultSelector, nil);
end;

function Enumerable<TSource>.GroupBy<TKey, TResult>(const keySelector: TFunc<TSource, TKey>;
  const resultSelector: TFunc<TKey, IEnumerable<TSource>, TResult>;
  const comparer: IEqualityComparer<TKey>): IEnumerable<TResult>;
begin
  Result := GroupBy<TKey, TSource, TResult>(
    keySelector, IdentityFunction, resultSelector, comparer);
end;

function Enumerable<TSource>.GroupBy<TKey, TElement, TResult>(
  const keySelector: TFunc<TSource, TKey>; const elementSelector: TFunc<TSource, TElement>;
  const resultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>): IEnumerable<TResult>;
begin
  Result := GroupBy<TKey, TElement, TResult>(
    keySelector, elementSelector, resultSelector, nil);
end;

function Enumerable<TSource>.GroupBy<TKey, TElement, TResult>(
  const keySelector: TFunc<TSource, TKey>; const elementSelector: TFunc<TSource, TElement>;
  const resultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>;
  const comparer: IEqualityComparer<TKey>): IEnumerable<TResult>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');
//  Guard.CheckNotNull(Assigned(elementSelector), 'elementSelector');
//  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');

  Result := TGroupedEnumerable<TSource, TKey, TElement, TResult>.Create(
    source, keySelector, elementSelector, resultSelector, comparer);
end;

function Enumerable<TSource>.GroupJoin<TInner, TKey, TResult>(
  const inner: IEnumerable<TInner>; const outerKeySelector: TFunc<TSource, TKey>;
  const innerKeySelector: TFunc<TInner, TKey>;
  const resultSelector: TFunc<TSource, IEnumerable<TInner>, TResult>): IEnumerable<TResult>;
begin
  Result := GroupJoin<TInner, TKey, TResult>(
    inner, outerKeySelector, innerKeySelector, resultSelector, nil);
end;

function Enumerable<TSource>.GroupJoin<TInner, TKey, TResult>(
  const inner: IEnumerable<TInner>; const outerKeySelector: TFunc<TSource, TKey>;
  const innerKeySelector: TFunc<TInner, TKey>;
  const resultSelector: TFunc<TSource, IEnumerable<TInner>, TResult>;
  const comparer: IEqualityComparer<TKey>): IEnumerable<TResult>;
begin
//  Guard.CheckNotNull(Assigned(source), 'outer');
//  Guard.CheckNotNull(Assigned(inner), 'inner');
//  Guard.CheckNotNull(Assigned(outerKeySelector), 'outerKeySelector');
//  Guard.CheckNotNull(Assigned(innerKeySelector), 'innerKeySelector');
//  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');

  Result := TGroupJoinIterator<TSource, TInner, TKey, TResult>.Create(
    source, inner, outerKeySelector, innerKeySelector, resultSelector, comparer);
end;

function Enumerable<TSource>.IntersectWith(const second: array of TSource): IEnumerable<TSource>;
begin
  Result := IntersectWith(TArrayIterator<TSource>.Create(second));
end;

function Enumerable<TSource>.IntersectWith(
  const second: IEnumerable<TSource>): IEnumerable<TSource>;
begin
  Result := IntersectWith(second, nil);
end;

function Enumerable<TSource>.IntersectWith(const second: IEnumerable<TSource>;
  const comparer: IEqualityComparer<TSource>): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'first');
//  Guard.CheckNotNull(Assigned(second), 'second');

  Result := TIntersectIterator<TSource>.Create(source, second, comparer);
end;

function Enumerable<TSource>.Join<TInner, TKey, TResult>(
  const inner: IEnumerable<TInner>; const outerKeySelector: TFunc<TSource, TKey>;
  const innerKeySelector: TFunc<TInner, TKey>;
  const resultSelector: TFunc<TSource, TInner, TResult>): IEnumerable<TResult>;
begin
  Result := Join<TInner, TKey, TResult>(
    inner, outerKeySelector, innerKeySelector, resultSelector, nil);
end;

function Enumerable<TSource>.Join<TInner, TKey, TResult>(
  const inner: IEnumerable<TInner>; const outerKeySelector: TFunc<TSource, TKey>;
  const innerKeySelector: TFunc<TInner, TKey>;
  const resultSelector: TFunc<TSource, TInner, TResult>;
  const comparer: IEqualityComparer<TKey>): IEnumerable<TResult>;
begin
//  Guard.CheckNotNull(Assigned(source), 'outer');
//  Guard.CheckNotNull(Assigned(inner), 'inner');
//  Guard.CheckNotNull(Assigned(outerKeySelector), 'outerKeySelector');
//  Guard.CheckNotNull(Assigned(innerKeySelector), 'innerKeySelector');
//  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');

  Result := TJoinIterator<TSource, TInner, TKey, TResult>.Create(
    source, inner, outerKeySelector, innerKeySelector, resultSelector, comparer);
end;

function Enumerable<TSource>.Last: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.Last;
end;

function Enumerable<TSource>.Last(const predicate: TPredicate<TSource>): TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := source.Last(predicate);
end;

function Enumerable<TSource>.LastOrDefault: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.LastOrDefault;
end;

function Enumerable<TSource>.LastOrDefault(const defaultValue: TSource): TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.LastOrDefault(defaultValue);
end;

function Enumerable<TSource>.LastOrDefault(const predicate: TPredicate<TSource>): TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := source.LastOrDefault(predicate);
end;

function Enumerable<TSource>.LastOrDefault(const predicate: TPredicate<TSource>;
  const defaultValue: TSource): TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := source.LastOrDefault(predicate, defaultValue);
end;

function Enumerable<TSource>.Max: TSource;
var
  comparer: IComparer<TSource>;
  flag: Boolean;
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  comparer := source.GetComparer;
  flag := False;
  for item in source do
  begin
    if flag then
    begin
      if comparer.Compare(item, Result) > 0 then
        Result := item;
    end
    else
    begin
      flag := True;
      Result := item;
    end;
  end;
  if not flag then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
end;

function Enumerable<TSource>.Max(const selector: TFunc<TSource, Double>): Double;
begin
  Result := Select<Double>(selector).Max;
end;

function Enumerable<TSource>.Max(const selector: TFunc<TSource, Int64>): Int64;
begin
  Result := Select<Int64>(selector).Max;
end;

function Enumerable<TSource>.MaxBy<TKey>(
  const selector: TFunc<TSource, TKey>): TSource;
begin
  Result := MaxBy<TKey>(selector, TComparer<TKey>.Default);
end;

function Enumerable<TSource>.MaxBy<TKey>(const selector: TFunc<TSource, TKey>;
  const comparer: IComparer<TKey>): TSource;
var
  enumerator: IEnumerator<TSource>;
  max: TSource;
  maxKey: TKey;
  candidate: TSource;
  candidateKey: TKey;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');
  Guard.CheckNotNull(Assigned(comparer), 'comparer');

  enumerator := source.GetEnumerator;
  if not enumerator.MoveNext then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
  max := enumerator.Current;
  maxKey := selector(min);
  while enumerator.MoveNext do
  begin
    candidate := enumerator.Current;
    candidateKey := selector(candidate);
    if comparer.Compare(candidateKey, maxKey) > 0 then
    begin
      max := candidate;
      maxKey := candidateKey;
    end;
  end;
  Result := min;
end;

function Enumerable<TSource>.Min: TSource;
var
  comparer: IComparer<TSource>;
  flag: Boolean;
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  comparer := source.GetComparer;
  flag := False;
  for item in source do
  begin
    if flag then
    begin
      if comparer.Compare(item, Result) < 0 then
        Result := item;
    end
    else
    begin
      flag := True;
      Result := item;
    end;
  end;
  if not flag then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
end;

function Enumerable<TSource>.Min(const selector: TFunc<TSource, Double>): Double;
begin
  Result := Select<Double>(selector).Min;
end;

function Enumerable<TSource>.Min(const selector: TFunc<TSource, Int64>): Int64;
begin
  Result := Select<Int64>(selector).Min;
end;

function Enumerable<TSource>.MinBy<TKey>(
  const selector: TFunc<TSource, TKey>): TSource;
begin
  Result := MinBy<TKey>(selector, TComparer<TKey>.Default);
end;

function Enumerable<TSource>.MinBy<TKey>(const selector: TFunc<TSource, TKey>;
  const comparer: IComparer<TKey>): TSource;
var
  enumerator: IEnumerator<TSource>;
  min: TSource;
  minKey: TKey;
  candidate: TSource;
  candidateKey: TKey;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');
  Guard.CheckNotNull(Assigned(comparer), 'comparer');

  enumerator := source.GetEnumerator;
  if not enumerator.MoveNext then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
  min := enumerator.Current;
  minKey := selector(min);
  while enumerator.MoveNext do
  begin
    candidate := enumerator.Current;
    candidateKey := selector(candidate);
    if comparer.Compare(candidateKey, minKey) < 0 then
    begin
      min := candidate;
      minKey := candidateKey;
    end;
  end;
  Result := min;
end;

function Enumerable<TSource>.OfType<TResult>: IEnumerable<TResult>;
begin
  Result := TOfTypeIterator<TSource, TResult>.Create(source);
end;

function Enumerable<TSource>.OrderBy<TKey>(
  const keySelector: TFunc<TSource, TKey>): IEnumerable<TSource>;
begin
  Result := OrderBy<TKey>(keySelector, nil);
end;

function Enumerable<TSource>.OrderBy<TKey>(const keySelector: TFunc<TSource, TKey>;
  const comparer: IComparer<TKey>): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');

  Result := TOrderedEnumerable<TSource, TKey>.Create(source, keySelector, comparer, False);
end;

function Enumerable<TSource>.OrderByDescending<TKey>(
  const keySelector: TFunc<TSource, TKey>): IEnumerable<TSource>;
begin
  Result := OrderByDescending<TKey>(keySelector, nil);
end;

function Enumerable<TSource>.OrderByDescending<TKey>(
  const keySelector: TFunc<TSource, TKey>;
  const comparer: IComparer<TKey>): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');

  Result := TOrderedEnumerable<TSource, TKey>.Create(source, keySelector, comparer, True);
end;

function Enumerable<TSource>.Reverse: IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');

  Result := TReversedIterator<TSource>.Create(source);
end;

function Enumerable<TSource>.Select(const selector: TFunc<TSource, TSource>): IEnumerable<TSource>;
begin
  Result := Select<TSource>(selector);
end;

function Enumerable<TSource>.Select<TResult>(
  const selector: TFunc<TSource, TResult>): IEnumerable<TResult>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(selector), 'selector');

  Result := TSelectIterator<TSource, TResult>.Create(source, selector);
end;

function Enumerable<TSource>.Select(
  const selector: TFunc<TSource, Integer, TSource>): IEnumerable<TSource>;
begin
  Result := Select<TSource>(selector);
end;

function Enumerable<TSource>.Select<TResult>(
  const selector: TFunc<TSource, Integer, TResult>): IEnumerable<TResult>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(selector), 'selector');

  Result := TSelectIndexIterator<TSource, TResult>.Create(source, selector);
end;

function Enumerable<TSource>.SelectMany<TResult>(
  const selector: TFunc<TSource, IEnumerable<TResult>>): IEnumerable<TResult>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(selector), 'selector');

  Result := TSelectManyIterator<TSource, TResult>.Create(source, selector);
end;

function Enumerable<TSource>.SelectMany<TResult>(
  const selector: TFunc<TSource, Integer, IEnumerable<TResult>>): IEnumerable<TResult>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(selector), 'selector');

  Result := TSelectManyIndexIterator<TSource, TResult>.Create(source, selector);
end;

function Enumerable<TSource>.SelectMany<TCollection, TResult>(
  const collectionSelector: TFunc<TSource, IEnumerable<TCollection>>;
  const resultSelector: TFunc<TSource, TCollection, TResult>): IEnumerable<TResult>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(collectionSelector), 'collectionSelector');
//  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');

  Result := TSelectManyIterator<TSource, TCollection, TResult>.Create(
    source, collectionSelector, resultSelector);
end;

function Enumerable<TSource>.SelectMany<TCollection, TResult>(
  const collectionSelector: TFunc<TSource, Integer, IEnumerable<TCollection>>;
  const resultSelector: TFunc<TSource, TCollection, TResult>): IEnumerable<TResult>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(collectionSelector), 'collectionSelector');
//  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');

  Result := TSelectManyIndexIterator<TSource, TCollection, TResult>.Create(
    source, collectionSelector, resultSelector);
end;

function Enumerable<TSource>.SequenceEqual(const second: array of TSource): Boolean;
begin
  Result := SequenceEqual(TArrayIterator<TSource>.Create(second), nil);
end;

function Enumerable<TSource>.SequenceEqual(const second: IEnumerable<TSource>): Boolean;
begin
  Result := SequenceEqual(second, nil);
end;

function Enumerable<TSource>.SequenceEqual(const second: IEnumerable<TSource>;
  comparer: IEqualityComparer<TSource>): Boolean;
var
  enumerator1, enumerator2: IEnumerator<TSource>;
begin
  Guard.CheckNotNull(Assigned(source), 'first');
  Guard.CheckNotNull(Assigned(second), 'second');

  if not Assigned(comparer) then
    comparer := TEqualityComparer<TSource>.Default;

  enumerator1 := source.GetEnumerator;
  enumerator2 := second.GetEnumerator;
  while enumerator1.MoveNext do
    if not enumerator2.MoveNext or not comparer.Equals(enumerator1.Current, enumerator2.Current) then
      Exit(False);
  if enumerator2.MoveNext then
    Exit(False);
  Result := True;
end;

function Enumerable<TSource>.Single: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.Single;
end;

function Enumerable<TSource>.Single(const predicate: TPredicate<TSource>): TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := source.Single(predicate);
end;

function Enumerable<TSource>.SingleOrDefault: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.SingleOrDefault;
end;

function Enumerable<TSource>.SingleOrDefault(const defaultValue: TSource): TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.SingleOrDefault(defaultValue);
end;

function Enumerable<TSource>.SingleOrDefault(const predicate: TPredicate<TSource>): TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := source.SingleOrDefault(predicate);
end;

function Enumerable<TSource>.SingleOrDefault(const predicate: TPredicate<TSource>;
  const defaultValue: TSource): TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := source.SingleOrDefault(predicate, defaultValue);
end;

function Enumerable<TSource>.Skip(count: Integer): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');

  Result := TSkipIterator<TSource>.Create(source, count);
end;

function Enumerable<TSource>.SkipWhile(const predicate: TPredicate<TSource>): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TSkipWhileIterator<TSource>.Create(source, predicate);
end;

function Enumerable<TSource>.SkipWhile(
  const predicate: TFunc<TSource, Integer, Boolean>): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TSkipWhileIndexIterator<TSource>.Create(source, predicate);
end;

function Enumerable<TSource>.Sum: TSource;
var
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  // TODO: support Nullable<>
  Result := Default(TSource);

  for item in source do
    Result := Add(Result, item);
end;

function Enumerable<TSource>.Sum(const selector: TFunc<TSource, Integer>): Integer;
var
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');

  Result := 0;
  for item in source do
    Result := Result + selector(item);
end;

function Enumerable<TSource>.Sum(
  const selector: TFunc<TSource, Nullable<Integer>>): Nullable<Integer>;
var
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');

  Result := 0;
  for item in source do
    Result := Result.Value + selector(item).GetValueOrDefault;
end;

function Enumerable<TSource>.Sum(const selector: TFunc<TSource, Int64>): Int64;
var
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');

  Result := 0;
  for item in source do
    Result := Result + selector(item);
end;

function Enumerable<TSource>.Sum(
  const selector: TFunc<TSource, Nullable<Int64>>): Nullable<Int64>;
var
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');

  Result := 0;
  for item in source do
    Result := Result.Value + selector(item).GetValueOrDefault;
end;

function Enumerable<TSource>.Sum(const selector: TFunc<TSource, Single>): Single;
var
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');

  Result := 0;
  for item in source do
    Result := Result + selector(item);
end;

function Enumerable<TSource>.Sum(
  const selector: TFunc<TSource, Nullable<Single>>): Nullable<Single>;
var
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');

  Result := 0;
  for item in source do
    Result := Result.Value + selector(item).GetValueOrDefault;
end;

function Enumerable<TSource>.Sum(const selector: TFunc<TSource, Double>): Double;
var
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');

  Result := 0;
  for item in source do
    Result := Result + selector(item);
end;

function Enumerable<TSource>.Sum(
  const selector: TFunc<TSource, Nullable<Double>>): Nullable<Double>;
var
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');

  Result := 0;
  for item in source do
    Result := Result.Value + selector(item).GetValueOrDefault;
end;

function Enumerable<TSource>.Take(count: Integer): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');

  Result := TTakeIterator<TSource>.Create(source, count);
end;

function Enumerable<TSource>.TakeWhile(const predicate: TPredicate<TSource>): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TTakeWhileIterator<TSource>.Create(source, predicate);
end;

function Enumerable<TSource>.TakeWhile(
  const predicate: TFunc<TSource, Integer, Boolean>): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TTakeWhileIndexIterator<TSource>.Create(source, predicate);
end;

function Enumerable<TSource>.ThenBy<TKey>(
  const keySelector: TFunc<TSource, TKey>): IEnumerable<TSource>;
begin
  Result := ThenBy<TKey>(keySelector, nil);
end;

function Enumerable<TSource>.ThenBy<TKey>(const keySelector: TFunc<TSource, TKey>;
  const comparer: IComparer<TKey>): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');

  Result := TOrderedEnumerable<TSource, TKey>.Create(source, keySelector, comparer, False);
end;

function Enumerable<TSource>.ThenByDescending<TKey>(
  const keySelector: TFunc<TSource, TKey>): IEnumerable<TSource>;
begin
  Result := ThenByDescending<TKey>(keySelector, nil);
end;

function Enumerable<TSource>.ThenByDescending<TKey>(const keySelector: TFunc<TSource, TKey>;
  const comparer: IComparer<TKey>): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');

  Result := TOrderedEnumerable<TSource, TKey>.Create(source, keySelector, comparer, True);
end;

function Enumerable<TSource>.ToArray: TArray<TSource>;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.ToArray;
end;

function Enumerable<TSource>.ToDictionary<TKey>(
  const keySelector: TFunc<TSource, TKey>): IDictionary<TKey, TSource>;
begin
  Result := ToDictionary<TKey, TSource>(keySelector, IdentityFunction, nil);
end;

function Enumerable<TSource>.ToDictionary<TKey>(
  const keySelector: TFunc<TSource, TKey>;
  const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TSource>;
begin
  Result := ToDictionary<TKey, TSource>(keySelector, IdentityFunction, comparer);
end;

function Enumerable<TSource>.ToDictionary<TKey, TElement>(
  const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>): IDictionary<TKey, TElement>;
begin
  Result := ToDictionary<TKey, TElement>(keySelector, elementSelector, nil);
end;

function Enumerable<TSource>.ToDictionary<TKey, TElement>(
  const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>;
  const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TElement>;
var
  item: TSource;
begin
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');
  Guard.CheckNotNull(Assigned(elementSelector), 'elementSelector');

  Result := TCollections.CreateDictionary<TKey, TElement>;
  for item in source do
    Result.Add(keySelector(item), elementSelector(item));
end;

function Enumerable<TSource>.ToList: IList<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');

  Result := TCollections.CreateList<TSource>;
  Result.AddRange(source);
end;

function Enumerable<TSource>.ToLookup(
  const keySelector: TFunc<TSource, TSource>): ILookup<TSource, TSource>;
begin
  Result := ToLookup<TSource, TSource>(keySelector, IdentityFunction, nil);
end;

function Enumerable<TSource>.ToLookup<TKey>(
  const keySelector: TFunc<TSource, TKey>): ILookup<TKey, TSource>;
begin
  Result := ToLookup<TKey, TSource>(keySelector, IdentityFunction, nil);
end;

function Enumerable<TSource>.ToLookup(const keySelector: TFunc<TSource, TSource>;
  const comparer: IEqualityComparer<TSource>): ILookup<TSource, TSource>;
begin
  Result := ToLookup<TSource, TSource>(keySelector, IdentityFunction, comparer);
end;

function Enumerable<TSource>.ToLookup<TKey>(const keySelector: TFunc<TSource, TKey>;
  const comparer: IEqualityComparer<TKey>): ILookup<TKey, TSource>;
begin
  Result := ToLookup<TKey, TSource>(keySelector, IdentityFunction, comparer);
end;

function Enumerable<TSource>.ToLookup<TKey, TElement>(
  const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>): ILookup<TKey, TElement>;
begin
  Result := ToLookup<TKey, TElement>(keySelector, elementSelector, nil);
end;

function Enumerable<TSource>.ToLookup<TKey, TElement>(
  const keySelector: TFunc<TSource, TKey>; const elementSelector: TFunc<TSource, TElement>;
  const comparer: IEqualityComparer<TKey>): ILookup<TKey, TElement>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');
//  Guard.CheckNotNull(Assigned(elementSelector), 'elementSelector');

  Result := TLookup<TKey, TElement>.Create<TSource>(source, keySelector, elementSelector, comparer);
end;

function Enumerable<TSource>.ToSet: ISet<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');

  Result := TCollections.CreateSet<TSource>;
  Result.AddRange(source);
end;

function Enumerable<TSource>.TryGetFirst(out value: TSource): Boolean;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.TryGetFirst(value);
end;

function Enumerable<TSource>.TryGetFirst(out value: TSource;
  const predicate: TPredicate<TSource>): Boolean;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.TryGetFirst(value, predicate);
end;

function Enumerable<TSource>.TryGetLast(out value: TSource): Boolean;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.TryGetLast(value);
end;

function Enumerable<TSource>.TryGetLast(out value: TSource;
  const predicate: TPredicate<TSource>): Boolean;
begin
  Guard.CheckNotNull(Assigned(source), 'source');

  Result := source.TryGetLast(value, predicate);
end;

function Enumerable<TSource>.UnionWith(const second: array of TSource): IEnumerable<TSource>;
begin
  Result := UnionWith(TArrayIterator<TSource>.Create(second));
end;

function Enumerable<TSource>.UnionWith(const second: IEnumerable<TSource>): IEnumerable<TSource>;
begin
  Result := UnionWith(second, nil);
end;

function Enumerable<TSource>.UnionWith(const second: IEnumerable<TSource>;
  const comparer: IEqualityComparer<TSource>): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'first');
//  Guard.CheckNotNull(Assigned(second), 'second');

  Result := TUnionIterator<TSource>.Create(source, second, comparer);
end;

function Enumerable<TSource>.Where(const predicate: TPredicate<TSource>): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TWhereIterator<TSource>.Create(source, predicate);
end;

function Enumerable<TSource>.Where(
  const predicate: TFunc<TSource, Integer, Boolean>): IEnumerable<TSource>;
begin
//  Guard.CheckNotNull(Assigned(source), 'source');
//  Guard.CheckNotNull(Assigned(predicate), 'predicate');

  Result := TWhereIndexIterator<TSource>.Create(source, predicate);
end;

function Enumerable<TSource>.Zip<TSecond, TResult>(const second: IEnumerable<TSecond>;
  const resultSelector: TFunc<TSource, TSecond, TResult>): IEnumerable<TResult>;
begin
//  Guard.CheckNotNull(Assigned(source), 'first');
//  Guard.CheckNotNull(Assigned(second), 'second');
//  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');

  Result := TZipIterator<TSource, TSecond, TResult>.Create(
    source, second, resultSelector);
end;

{$ENDREGION}


end.
