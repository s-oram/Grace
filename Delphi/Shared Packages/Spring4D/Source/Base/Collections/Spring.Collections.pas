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

///	<summary>
///	  The Spring.Collections namespaces introduce the Collections Framework in
///	  spring4d.
///	</summary>
unit Spring.Collections;

interface

uses
  Classes,
  Generics.Collections,
  Generics.Defaults,
  SysUtils,
  Spring;

const
  doOwnsKeys = Generics.Collections.doOwnsKeys;
  doOwnsValues = Generics.Collections.doOwnsValues;

type
  {$REGION 'Forward definitions'}
  IEnumerator = interface;
  IEnumerable = interface;
  IReadOnlyCollection = interface;
  ICollection = interface;
  IReadOnlyList = interface;
  IList = interface;
  IReadOnlyDictionary = interface;
  IDictionary = interface;
  IStack = interface;
  IQueue = interface;
  ISet = interface;

  IEnumerator<T> = interface;
  IEnumerable<T> = interface;
  IReadOnlyCollection<T> = interface;
  ICollection<T> = interface;
  IReadOnlyList<T> = interface;
  IList<T> = interface;
  ILinkedList<T> = interface;
  IReadOnlyDictionary<TKey, TValue> = interface;
  IDictionary<TKey, TValue> = interface;
  IStack<T> = interface;
  IQueue<T> = interface;
  ISet<T> = interface;

  IObjectList = interface;
  IInterfaceList = interface;

  IGrouping<TKey, TElement> = interface;
  ILookup<TKey, TElement> = interface;
  {$ENDREGION}

  TCollectionChangedAction = Spring.TCollectionChangedAction;

  TCollectionChangedEvent<T> = procedure(Sender: TObject; const Item: T;
    Action: TCollectionChangedAction) of object;

  ICollectionChangedEvent<T> = interface(IEvent<TCollectionChangedEvent<T>>)
  end;

  ///	<summary>
  ///	  Supports a simple iteration over a non-generic collection.
  ///	</summary>
  IEnumerator = interface(IInvokable)
    ['{A2AD52DC-FA9F-4121-9B54-5C427DA5E62C}']
  {$REGION 'Property Accessors'}
    function GetCurrent: TValue;
  {$ENDREGION}

    ///	<summary>
    ///	  Advances the enumerator to the next element of the collection.
    ///	</summary>
    ///	<returns>
    ///	  <b>True</b> if the enumerator was successfully advanced to the next
    ///	  element; <b>False</b> if the enumerator has passed the end of the
    ///	  collection.
    ///	</returns>
    ///	<exception cref="Spring|EInvalidOperationException">
    ///	  The collection was modified after the enumerator was created.
    ///	</exception>
    function MoveNext: Boolean;

    ///	<summary>
    ///	  Sets the enumerator to its initial position, which is before the
    ///	  first element in the collection.
    ///	</summary>
    ///	<exception cref="Spring|EInvalidOperationException">
    ///	  The collection was modified after the enumerator was created.
    ///	</exception>
    ///	<exception cref="Spring|ENotSupportedException">
    ///	  The Reset method is not supported. 
    ///	</exception>
    procedure Reset;

    ///	<summary>
    ///	  Gets the current element in the collection.
    ///	</summary>
    ///	<value>
    ///	  The current element in the collection.
    ///	</value>
    property Current: TValue read GetCurrent;
  end;

  ///	<summary>
  ///	  Supports a simple iteration over a generic collection.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type of objects to enumerate.
  ///	</typeparam>
  IEnumerator<T> = interface(IEnumerator)
    ['{E6525A22-15EF-46EB-8A68-8CB202DA7D67}']
  {$REGION 'Property Accessors'}
    function GetCurrent: T;
  {$ENDREGION}

    ///	<summary>
    ///	  Gets the current element in the collection.
    ///	</summary>
    ///	<value>
    ///	  The current element in the collection.
    ///	</value>
    property Current: T read GetCurrent;
  end;

  ///	<summary>
  ///	  Exposes an enumerator, which supports a simple iteration over a
  ///	  non-generic collection.
  ///	</summary>
  IEnumerable = interface(IInvokable)
    ['{6BC97F33-C0A8-4770-8E1C-C2017527B7E7}']
  {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetElementType: PTypeInfo;
  {$ENDREGION}

    ///	<summary>
    ///	  Returns the reference to this instance.
    ///	</summary>
    ///	<returns>
    ///	  The <see cref="TObject" /> instance behind this IEnumerable reference.
    ///	</returns>
    function AsObject: TObject;

    ///	<summary>
    ///	  Returns an enumerator that iterates through a collection.
    ///	</summary>
    ///	<returns>
    ///	  An <see cref="IEnumerator" /> object that can be used to iterate
    ///	  through the collection.
    ///	</returns>
    function GetEnumerator: IEnumerator;

    ///	<summary>
    ///	  Returns the number of elements in a sequence.
    ///	</summary>
    ///	<value>
    ///	  The number of elements in the sequence.
    ///	</value>
    property Count: Integer read GetCount;

    ///	<summary>
    ///	  Returns the type of the elements in the sequence.
    ///	</summary>
    ///	<value>
    ///	  The type of the elements in the sequence.
    ///	</value>
    property ElementType: PTypeInfo read GetElementType;

  end;

  ///	<summary>
  ///	  Exposes the enumerator, which supports a simple iteration over a
  ///	  collection of a specified type.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type of objects to enumerate.
  ///	</typeparam>
  ///	<seealso href="http://msdn.microsoft.com/en-us/magazine/cc700332.aspx">
  ///	  The LINQ Enumerable Class
  ///	</seealso>
  IEnumerable<T> = interface(IEnumerable)
    ['{A6B46D30-5B0F-495F-B7EC-46FBC5A75D24}']

    ///	<summary>
    ///	  Returns the specified comparer for this instance.
    ///	</summary>
    ///	<returns>
    ///	  Returns the specified IComparer&lt;T&gt; for this instance.
    ///	</returns>
    function GetComparer: IComparer<T>;

    ///	<summary>
    ///	  Returns an enumerator that iterates through the collection.
    ///	</summary>
    ///	<returns>
    ///	  An <see cref="IEnumerator&lt;T&gt;" /> that can be used to iterate
    ///	  through the collection.
    ///	</returns>
    function GetEnumerator: IEnumerator<T>;

    ///	<summary>
    ///	  Determines whether all elements of a sequence satisfy a condition.
    ///	</summary>
    ///	<param name="predicate">
    ///	  A function to test each element for a condition.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if every element of the source sequence passes the test
    ///	  in the specified predicate, or if the sequence is empty; otherwise,
    ///	  <b>False</b>.
    ///	</returns>
    function All(const predicate: TPredicate<T>): Boolean;

    ///	<summary>
    ///	  Determines whether a sequence contains any elements.
    ///	</summary>
    ///	<returns>
    ///	  <b>True</b> if the source sequence contains any elements; otherwise,
    ///	  <b>False</b>.
    ///	</returns>
    function Any: Boolean; overload;

    ///	<summary>
    ///	  Determines whether any element of a sequence satisfies a condition.
    ///	</summary>
    ///	<param name="predicate">
    ///	  A function to test each element for a condition.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if any elements in the source sequence pass the test in
    ///	  the specified predicate; otherwise, <b>False</b>.
    ///	</returns>
    function Any(const predicate: TPredicate<T>): Boolean; overload;

    ///	<summary>
    ///	  Concatenates two sequences.
    ///	</summary>
    ///	<param name="second">
    ///	  The sequence to concatenate to the first sequence.
    ///	</param>
    ///	<returns>
    ///	  An IEnumerable&lt;T&gt; that contains the concatenated elements of
    ///	  the two input sequences.
    ///	</returns>
    function Concat(const second: IEnumerable<T>): IEnumerable<T>; overload;

    ///	<summary>
    ///	  Determines whether a sequence contains a specified element by using
    ///	  the default equality comparer.
    ///	</summary>
    ///	<param name="value">
    ///	  The value to locate in the sequence.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the source sequence contains an element that has the
    ///	  specified value; otherwise, <b>False</b>.
    ///	</returns>
    function Contains(const value: T): Boolean; overload;

    ///	<summary>
    ///	  Determines whether a sequence contains a specified element by using a
    ///	  specified <see cref="IEqualityComparer&lt;T&gt;" />.
    ///	</summary>
    ///	<param name="value">
    ///	  The value to locate in the sequence.
    ///	</param>
    ///	<param name="comparer">
    ///	  An equality comparer to compare values.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the source sequence contains an element that has the
    ///	  specified value; otherwise, <b>False</b>.
    ///	</returns>
    function Contains(const value: T; const comparer: IEqualityComparer<T>): Boolean; overload;

    function Contains(const value: T; const comparer: TEqualityComparison<T>): Boolean; overload;

    ///	<summary>
    ///	  Returns the element at a specified index in a sequence.
    ///	</summary>
    ///	<param name="index">
    ///	  The zero-based index of the element to retrieve.
    ///	</param>
    ///	<returns>
    ///	  The element at the specified position in the source sequence.
    ///	</returns>
    function ElementAt(index: Integer): T;

    ///	<summary>
    ///	  Returns the element at a specified index in a sequence or a default
    ///	  value if the index is out of range.
    ///	</summary>
    ///	<param name="index">
    ///	  The zero-based index of the element to retrieve.
    ///	</param>
    ///	<returns>
    ///	  <b>Default</b>(<i>T</i>) if the index is outside the bounds of the
    ///	  source sequence; otherwise, the element at the specified position in
    ///	  the source sequence.
    ///	</returns>
    function ElementAtOrDefault(index: Integer): T; overload;

    ///	<summary>
    ///	  Returns the element at a specified index in a sequence or the
    ///	  specified default value if the index is out of range.
    ///	</summary>
    ///	<param name="index">
    ///	  The zero-based index of the element to retrieve.
    ///	</param>
    ///	<param name="defaultValue">
    ///	  The value to return if the index is out of range.
    ///	</param>
    ///	<returns>
    ///	  <i>DefaultValue</i> if the index is outside the bounds of the source
    ///	  sequence; otherwise, the element at the specified position in the
    ///	  source sequence.
    ///	</returns>
    function ElementAtOrDefault(index: Integer; const defaultValue: T): T; overload;

    ///	<summary>
    ///	  Determines whether two sequences are equal by comparing the elements
    ///	  by using the default equality comparer for their type.
    ///	</summary>
    function EqualsTo(const values: array of T): Boolean; overload;

    ///	<summary>
    ///	  Determines whether two sequences are equal by comparing the elements
    ///	  by using the default equality comparer for their type.
    ///	</summary>
    function EqualsTo(const collection: IEnumerable<T>): Boolean; overload;

    ///	<summary>
    ///	  Determines whether two sequences are equal by comparing their
    ///	  elements by using a specified <c>IEqualityComparer&lt;T&gt;.</c>
    ///	</summary>
    function EqualsTo(const collection: IEnumerable<T>; const comparer: IEqualityComparer<T>): Boolean; overload;

    ///	<summary>
    ///	  Returns the first element of a sequence.
    ///	</summary>
    ///	<returns>
    ///	  The first element in the specified sequence.
    ///	</returns>
    function First: T; overload;

    ///	<summary>
    ///	  Returns the first element in a sequence that satisfies a specified
    ///	  condition.
    ///	</summary>
    ///	<param name="predicate">
    ///	  A function to test each element for a condition.
    ///	</param>
    ///	<returns>
    ///	  The first element in the sequence that passes the test in the
    ///	  specified predicate function.
    ///	</returns>
    function First(const predicate: TPredicate<T>): T; overload;

    ///	<summary>
    ///	  Returns the first element of a sequence, or a default value if the
    ///	  sequence contains no elements.
    ///	</summary>
    ///	<returns>
    ///	  <b>Default</b>(<i>T</i>) if source is empty; otherwise, the first
    ///	  element in source.
    ///	</returns>
    function FirstOrDefault: T; overload;

    ///	<summary>
    ///	  Returns the first element of a sequence, or the specified default
    ///	  value if the sequence contains no elements.
    ///	</summary>
    ///	<param name="defaultValue">
    ///	  The value to return if the sequence contains no elements.
    ///	</param>
    ///	<returns>
    ///	  <i>DefaultValue</i> if source is empty; otherwise, the first element
    ///	  in source.
    ///	</returns>
    function FirstOrDefault(const defaultValue: T): T; overload;

    ///	<summary>
    ///	  Returns the first element of the sequence that satisfies a condition
    ///	  or a default value if no such element is found.
    ///	</summary>
    ///	<param name="predicate">
    ///	  A function to test each element for a condition.
    ///	</param>
    ///	<returns>
    ///	  <b>Default</b>(<i>T</i>) if source is empty or if no element passes
    ///	  the test specified by predicate; otherwise, the first element in
    ///	  source that passes the test specified by predicate.
    ///	</returns>
    function FirstOrDefault(const predicate: TPredicate<T>): T; overload;

    ///	<summary>
    ///	  Returns the first element of the sequence that satisfies a condition
    ///	  or the specified default value if no such element is found.
    ///	</summary>
    ///	<param name="predicate">
    ///	  A function to test each element for a condition.
    ///	</param>
    ///	<param name="defaultValue">
    ///	  The value to return if no element is found.
    ///	</param>
    ///	<returns>
    ///	  <i>DefaultValue</i> if source is empty or if no element passes the
    ///	  test specified by predicate; otherwise, the first element in source
    ///	  that passes the test specified by predicate.
    ///	</returns>
    function FirstOrDefault(const predicate: TPredicate<T>; const defaultValue: T): T; overload;

    ///	<summary>
    ///	  Performs the specified action on each element of a sequence.
    ///	</summary>
    procedure ForEach(const action: TAction<T>);

    ///	<summary>
    ///	  Returns the last element of a sequence.
    ///	</summary>
    ///	<returns>
    ///	  The value at the last position in the source sequence.
    ///	</returns>
    function Last: T; overload;

    ///	<summary>
    ///	  Returns the last element of a sequence that satisfies a specified
    ///	  condition.
    ///	</summary>
    ///	<param name="predicate">
    ///	  A function to test each element for a condition.
    ///	</param>
    ///	<returns>
    ///	  The last element in the sequence that passes the test in the
    ///	  specified predicate function.
    ///	</returns>
    function Last(const predicate: TPredicate<T>): T; overload;

    ///	<summary>
    ///	  Returns the last element of a sequence, or a default value if the
    ///	  sequence contains no elements.
    ///	</summary>
    ///	<returns>
    ///	  <b>Default</b>(<i>T</i>) if the source sequence is empty; otherwise,
    ///	  the last element in the IEnumerable&lt;T&gt;.
    ///	</returns>
    function LastOrDefault: T; overload;

    ///	<summary>
    ///	  Returns the last element of a sequence, or the specified default
    ///	  value if the sequence contains no elements.
    ///	</summary>
    ///	<param name="defaultValue">
    ///	  The value to return if the sequence contains no elements.
    ///	</param>
    ///	<returns>
    ///	  <i>DefaultValue</i> if the source sequence is empty; otherwise, the
    ///	  last element in the IEnumerable&lt;T&gt;.
    ///	</returns>
    function LastOrDefault(const defaultValue: T): T; overload;

    ///	<summary>
    ///	  Returns the last element of a sequence that satisfies a condition or
    ///	  a default value if no such element is found.
    ///	</summary>
    ///	<param name="predicate">
    ///	  A function to test each element for a condition.
    ///	</param>
    ///	<returns>
    ///	  <b>Default</b>(<i>T</i>) if the sequence is empty or if no elements
    ///	  pass the test in the predicate function; otherwise, the last element
    ///	  that passes the test in the predicate function.
    ///	</returns>
    function LastOrDefault(const predicate: TPredicate<T>): T; overload;

    ///	<summary>
    ///	  Returns the last element of a sequence that satisfies a condition or
    ///	  the specified default value if no such element is found.
    ///	</summary>
    ///	<param name="predicate">
    ///	  A function to test each element for a condition.
    ///	</param>
    ///	<param name="defaultValue">
    ///	  The value to return if no element is found.
    ///	</param>
    ///	<returns>
    ///	  <i>DefaultValue</i> if the sequence is empty or if no elements pass
    ///	  the test in the predicate function; otherwise, the last element that
    ///	  passes the test in the predicate function.
    ///	</returns>
    function LastOrDefault(const predicate: TPredicate<T>; const defaultValue: T): T; overload;

    ///	<summary>
    ///	  Returns the maximum value in a sequence.
    ///	</summary>
    ///	<returns>
    ///	  The maximum value in the sequence.
    ///	</returns>
    function Max: T; overload;

{$IFDEF DELPHIXE_UP}
    function Max(const selector: TFunc<T, Integer>): Integer; overload;
{$ENDIF}

    ///	<summary>
    ///	  Returns the maximum value in a sequence by using the specified
    ///	  <see cref="IComparer&lt;T&gt;" />.
    ///	</summary>
    ///	<param name="comparer">
    ///	  An <see cref="IComparer&lt;T&gt;" /> to compare values.
    ///	</param>
    ///	<returns>
    ///	  The maximum value in the sequence.
    ///	</returns>
    function Max(const comparer: IComparer<T>): T; overload;
    function Max(const comparer: TComparison<T>): T; overload;

    ///	<summary>
    ///	  Returns the minimum value in a sequence.
    ///	</summary>
    ///	<returns>
    ///	  The minimum value in the sequence.
    ///	</returns>
    function Min: T; overload;

{$IFDEF DELPHIXE_UP}
    function Min(const selector: TFunc<T, Integer>): Integer; overload;
{$ENDIF}

    ///	<summary>
    ///	  Returns the minimum value in a sequence by using the specified
    ///	  <see cref="IComparer&lt;T&gt;" />.
    ///	</summary>
    ///	<param name="comparer">
    ///	  An <see cref="IComparer&lt;T&gt;" /> to compare values.
    ///	</param>
    ///	<returns>
    ///	  The minimum value in the sequence.
    ///	</returns>
    function Min(const comparer: IComparer<T>): T; overload;
    function Min(const comparer: TComparison<T>): T; overload;

    ///	<summary>
    ///	  Sorts the elements of a sequence in ascending order using the default
    ///	  comparer for their type.
    ///	</summary>
    function Ordered: IEnumerable<T>; overload;

    ///	<summary>
    ///	  Sorts the elements of a sequence in ascending order using the
    ///	  specified <see cref="IComparer&lt;T&gt;" />.
    ///	</summary>
    function Ordered(const comparer: IComparer<T>): IEnumerable<T>; overload;
    function Ordered(const comparer: TComparison<T>): IEnumerable<T>; overload;

    ///	<summary>
    ///	  Inverts the order of the elements in a sequence.
    ///	</summary>
    function Reversed: IEnumerable<T>;

    ///	<summary>
    ///	  Returns the only element of a sequence, and throws an exception if
    ///	  there is not exactly one element in the sequence.
    ///	</summary>
    function Single: T; overload;

    ///	<summary>
    ///	  Returns the only element of a sequence, and throws an exception if
    ///	  there is not exactly one element in the sequence.
    ///	</summary>
    function Single(const predicate: TPredicate<T>): T; overload;

    ///	<summary>
    ///	  Returns the only element of a sequence, or a default value if the
    ///	  sequence is empty; this method throws an exception if there is more
    ///	  than one element in the sequence.
    ///	</summary>
    function SingleOrDefault: T; overload;

    ///	<summary>
    ///	  Returns the only element of a sequence, or the specified default
    ///	  value if the sequence is empty; this method throws an exception if
    ///	  there is more than one element in the sequence.
    ///	</summary>
    function SingleOrDefault(const defaultValue: T): T; overload;

    ///	<summary>
    ///	  Returns the only element of a sequence that satisfies a specified
    ///	  condition or a default value if no such element exists; this method
    ///	  throws an exception if more than one element satisfies the condition.
    ///	</summary>
    function SingleOrDefault(const predicate: TPredicate<T>): T; overload;

    ///	<summary>
    ///	  Returns the only element of a sequence that satisfies a specified
    ///	  condition or the specified default value if no such element exists;
    ///	  this method throws an exception if more than one element satisfies
    ///	  the condition.
    ///	</summary>
    function SingleOrDefault(const predicate: TPredicate<T>; const defaultValue: T): T; overload;

    ///	<summary>
    ///	  Bypasses a specified number of elements in a sequence and then
    ///	  returns the remaining elements.
    ///	</summary>
    function Skip(count: Integer): IEnumerable<T>;

    ///	<summary>
    ///	  Bypasses elements in a sequence as long as a specified condition is
    ///	  true and then returns the remaining elements.
    ///	</summary>
    function SkipWhile(const predicate: TPredicate<T>): IEnumerable<T>; overload;

    ///	<summary>
    ///	  Bypasses elements in a sequence as long as a specified condition is
    ///	  true and then returns the remaining elements. The element's index is
    ///	  used in the logic of the predicate function.
    ///	</summary>
    function SkipWhile(const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>; overload;

    ///	<summary>
    ///	  Returns a specified number of contiguous elements from the start of a
    ///	  sequence.
    ///	</summary>
    function Take(count: Integer): IEnumerable<T>;

    ///	<summary>
    ///	  Returns elements from a sequence as long as a specified condition is
    ///	  true.
    ///	</summary>
    function TakeWhile(const predicate: TPredicate<T>): IEnumerable<T>; overload;

    ///	<summary>
    ///	  Returns elements from a sequence as long as a specified condition is
    ///	  true. The element's index is used in the logic of the predicate
    ///	  function.
    ///	</summary>
    function TakeWhile(const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>; overload;

    ///	<summary>
    ///	  Creates a new array which is filled with the elements in the
    ///	  collection.
    ///	</summary>
    function ToArray: TArray<T>;

    ///	<summary>
    ///	  Try getting the first element in a sequence.
    ///	</summary>
    function TryGetFirst(out value: T): Boolean; overload;

    ///	<summary>
    ///	  Try getting the first element in a sequence that satisfies a
    ///	  specified condition.
    ///	</summary>
    function TryGetFirst(out value: T; const predicate: TPredicate<T>): Boolean; overload;

    ///	<summary>
    ///	  Try getting the last element in a sequence.
    ///	</summary>
    function TryGetLast(out value: T): Boolean; overload;

    ///	<summary>
    ///	  Try getting the last element in a sequence that satisfies a specified
    ///	  condition.
    ///	</summary>
    function TryGetLast(out value: T; const predicate: TPredicate<T>): Boolean; overload;

    ///	<summary>
    ///	  Try getting the only element in a sequence.
    ///	</summary>
    function TryGetSingle(out value: T): Boolean; overload;

    ///	<summary>
    ///	  Try getting the only element in a sequence that satisfies a specified
    ///	  condition.
    ///	</summary>
    function TryGetSingle(out value: T; const predicate: TPredicate<T>): Boolean; overload;

    ///	<summary>
    ///	  Filters a sequence of values based on a predicate.
    ///	</summary>
    function Where(const predicate: TPredicate<T>): IEnumerable<T>; overload;

    ///	<summary>
    ///	  Gets the assigned comparer. If not comparer was assigned it returns
    ///	  the default comparer.
    ///	</summary>
    property Comparer: IComparer<T> read GetComparer;
  end;

  IReadOnlyCollection = interface(IEnumerable)
    ['{4DE35086-06DC-4F99-AE63-BCF4ADB2828D}']
  end;

  ICollection = interface(IEnumerable)
    ['{AC8A0302-C530-46A0-83FC-D88302ECCE3D}']
  {$REGION 'Property Accessors'}
    function GetIsReadOnly: Boolean;
    function GetOnChanged: IEvent;
  {$ENDREGION}

    procedure Add(const item: TValue);
    procedure AddRange(const values: array of TValue); overload;
    procedure AddRange(const collection: IEnumerable); overload;

    procedure Clear;

    function Remove(const item: TValue): Boolean;
    procedure RemoveRange(const values: array of TValue); overload;
    procedure RemoveRange(const collection: IEnumerable); overload;

    function Extract(const item: TValue): TValue;
    procedure ExtractRange(const values: array of TValue); overload;
    procedure ExtractRange(const collection: IEnumerable); overload;

    property IsReadOnly: Boolean read GetIsReadOnly;
    property OnChanged: IEvent read GetOnChanged;
  end;

  ///	<summary>
  ///	  Represents a strongly-typed, read-only collection of elements.
  ///	</summary>
  IReadOnlyCollection<T> = interface(IEnumerable<T>)
    ['{E1368FD5-02AE-4481-A9DC-96329DFF606C}']
  end;

  ///	<summary>
  ///	  Defines methods to manipulate generic collections.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type of the elements in the collection.
  ///	</typeparam>
  ICollection<T> = interface(IEnumerable<T>)
    ['{9BFD9B06-45CD-4C80-B145-01B09D432CF0}']
  {$REGION 'Property Accessors'}
    function GetIsReadOnly: Boolean;
    function GetOnChanged: ICollectionChangedEvent<T>;
  {$ENDREGION}

    ///	<summary>
    ///	  Adds an item to the ICollection&lt;T&gt;.
    ///	</summary>
    ///	<param name="item">
    ///	  The element to add to the ICollection&lt;T&gt;.
    ///	</param>
    procedure Add(const item: T);
    procedure AddRange(const values: array of T); overload;
    procedure AddRange(const collection: IEnumerable<T>); overload;

    ///	<summary>
    ///	  Removes all items from the ICollection&lt;T&gt;.
    ///	</summary>
    procedure Clear;

    ///	<summary>
    ///	  Copies the elements of the ICollection&lt;T&gt; to an array, starting
    ///	  at a particular array index.
    ///	</summary>
    ///	<param name="values">
    ///	  The one-dimensional array that is the destination of the elements
    ///	  copied from ICollection&lt;T&gt;. The array must have zero-based
    ///	  indexing.
    ///	</param>
    ///	<param name="index">
    ///	  The zero-based index in array at which copying begins.
    ///	</param>
    procedure CopyTo(var values: TArray<T>; index: Integer);

    procedure MoveTo(const collection: ICollection<T>);

    ///	<summary>
    ///	  Removes the first occurrence of a specific element from the
    ///	  ICollection&lt;T&gt;.
    ///	</summary>
    ///	<param name="item">
    ///	  The element to remove from the ICollection&lt;T&gt;.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if <i>item</i> was successfully removed from the
    ///	  ICollection&lt;T&gt;; otherwise, <b>False</b>. This method also
    ///	  returns <b>False</b> if <i>item</i> is not found in the original
    ///	  ICollection&lt;T&gt;.
    ///	</returns>
    function Remove(const item: T): Boolean;
    procedure RemoveAll(const predicate: TPredicate<T>);
    procedure RemoveRange(const values: array of T); overload;
    procedure RemoveRange(const collection: IEnumerable<T>); overload;

    function Extract(const item: T): T;
    procedure ExtractAll(const predicate: TPredicate<T>);
    procedure ExtractRange(const values: array of T); overload;
    procedure ExtractRange(const collection: IEnumerable<T>); overload;

    ///	<summary>
    ///	  Gets a value indicating whether the ICollection&lt;T&gt; is read-only.
    ///	</summary>
    ///	<value>
    ///	  <b>True</b> if the ICollection&lt;T&gt; is read-only; otherwise,
    ///	  <b>False</b>.
    ///	</value>
    ///	<remarks>
    ///	  A collection that is read-only does not allow the addition, removal,
    ///	  or modification of elements after the collection is created.
    ///	</remarks>
    property IsReadOnly: Boolean read GetIsReadOnly;
    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

  IReadOnlyList = interface(IReadOnlyCollection)
    ['{3DFEBF5A-8BF2-4152-A105-BECF01AFB60F}']
  {$REGION 'Property Accessors'}
    function GetItem(index: Integer): TValue;
  {$ENDREGION}

    function IndexOf(const item: TValue): Integer; overload;
    function IndexOf(const item: TValue; index: Integer): Integer; overload;
    function IndexOf(const item: TValue; index, count: Integer): Integer; overload;

    property Item[index: Integer]: TValue read GetItem; default;
  end;

  IList = interface(ICollection)
    ['{43FF6143-3B87-4298-B48C-2ABB9353BF68}']
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetItem(index: Integer): TValue;
    procedure SetCapacity(value: Integer);
    procedure SetItem(index: Integer; const item: TValue);
  {$ENDREGION}

    function Add(const item: TValue): Integer;

    procedure Insert(index: Integer; const item: TValue);
    procedure InsertRange(index: Integer; const values: array of TValue); overload;
    procedure InsertRange(index: Integer; const collection: IEnumerable); overload;

    procedure Delete(index: Integer);
    procedure DeleteRange(index, count: Integer);

    procedure Exchange(index1, index2: Integer);
    procedure Move(currentIndex, newIndex: Integer);

    procedure Reverse; overload;
    procedure Reverse(index, count: Integer); overload;

    procedure Sort;

    function IndexOf(const item: TValue): Integer; overload;
    function IndexOf(const item: TValue; index: Integer): Integer; overload;
    function IndexOf(const item: TValue; index, count: Integer): Integer; overload;

    function LastIndexOf(const item: TValue): Integer; overload;
    function LastIndexOf(const item: TValue; index: Integer): Integer; overload;
    function LastIndexOf(const item: TValue; index, count: Integer): Integer; overload;

    function AsReadOnlyList: IReadOnlyList;
    procedure TrimExcess;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Items[index: Integer]: TValue read GetItem write SetItem; default;
  end;

  ///	<summary>
  ///	  Represents a read-only collection of elements that can be accessed by
  ///	  index.
  ///	</summary>
  IReadOnlyList<T> = interface(IReadOnlyCollection<T>)
    ['{82A74ABB-509E-4AC0-9268-A993E7DC3AB3}']
  {$REGION 'Property Accessors'}
    function GetItem(index: Integer): T;
  {$ENDREGION}

    ///	<summary>
    ///	  Determines the index of a specific item in the IReadOnlyList&lt;T&gt;.
    ///	</summary>
    ///	<param name="item">
    ///	  The object to locate in the IReadOnlyList&lt;T&gt;.
    ///	</param>
    ///	<returns>
    ///	  The index of <i>item</i> if found in the list; otherwise, -1.
    ///	</returns>
    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;

    ///	<summary>
    ///	  Gets the element at the specified index in the read-only list.
    ///	</summary>
    ///	<param name="index">
    ///	  The zero-based index of the element to get.
    ///	</param>
    ///	<value>
    ///	  The element at the specified index in the read-only list.
    ///	</value>
    property Item[index: Integer]: T read GetItem; default;
  end;

  ///	<summary>
  ///	  Represents a collection of elements that can be individually accessed by
  ///	  index.
  ///	</summary>
  IList<T> = interface(ICollection<T>)
    ['{B6B4E1E1-0D29-40E1-854C-A93DEA8D1AA5}']
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer;
    function GetCount: Integer;
    function GetItem(index: Integer): T;
    procedure SetCapacity(value: Integer);
    procedure SetCount(value: Integer);
    procedure SetItem(index: Integer; const item: T);
  {$ENDREGION}

    function Add(const item: T): Integer;

    ///	<summary>
    ///	  Inserts an item to the IList&lt;T&gt; at the specified index.
    ///	</summary>
    ///	<param name="index">
    ///	  The zero-based index at which item should be inserted.
    ///	</param>
    ///	<param name="item">
    ///	  The element to insert into the IList&lt;T&gt;.
    ///	</param>
    procedure Insert(index: Integer; const item: T);
    procedure InsertRange(index: Integer; const values: array of T); overload;
    procedure InsertRange(index: Integer; const collection: IEnumerable<T>); overload;

    ///	<summary>
    ///	  Removes the item at the specified index.
    ///	</summary>
    ///	<param name="index">
    ///	  The zero-based index of the item to remove.
    ///	</param>
    ///	<exception cref="ArgumentOutOfRangeException">
    ///	  <i>index</i> is not a valid index in the IList&lt;T&gt;.
    ///	</exception>
    procedure Delete(index: Integer);
    procedure DeleteRange(index, count: Integer);

    /// <summary>
    ///   Creates a new list that contains a range of the elements in the
    ///   original list.
    /// </summary>
    /// <param name="index">
    ///   The zero-based index at which the range starts.
    /// </param>
    /// <param name="count">
    ///   The number of elements in the range.
    /// </param>
    /// <remarks>
    ///   If the list contains reference types the elements in the returned
    ///   list point to the same instance as the elements in the original list.
    ///   Also if the original list is a <see cref="Spring.Collections.Lists|TObjectList&lt;T&gt;" />
    ///    it still owns the objects.
    /// </remarks>
    function GetRange(index, count: Integer): IList<T>;

    procedure Exchange(index1, index2: Integer);
    procedure Move(currentIndex, newIndex: Integer);

    procedure Reverse; overload;
    procedure Reverse(index, count: Integer); overload;

    procedure Sort; overload;
    procedure Sort(const comparer: IComparer<T>); overload;
    procedure Sort(const comparer: TComparison<T>); overload;

    ///	<summary>
    ///	  Determines the index of a specific item in the IList&lt;T&gt;.
    ///	</summary>
    ///	<param name="item">
    ///	  The element to locate in the IList&lt;T&gt;.
    ///	</param>
    ///	<returns>
    ///	  The index of <i>item</i> if found in the list; otherwise, -1.
    ///	</returns>
    ///	<remarks>
    ///	  If an element occurs multiple times in the list, the IndexOf method
    ///	  always returns the first instance found.
    ///	</remarks>
    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;

    function LastIndexOf(const item: T): Integer; overload;
    function LastIndexOf(const item: T; index: Integer): Integer; overload;
    function LastIndexOf(const item: T; index, count: Integer): Integer; overload;

    function AsList: IList;

    /// <summary>
    ///   Returns the list as read-only list.
    /// </summary>
    /// <remarks>
    ///   This method will not perform a copy but will return the same instance
    ///   as IReadOnlyList&lt;T&gt;.
    /// </remarks>
    function AsReadOnlyList: IReadOnlyList<T>;
    procedure TrimExcess;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[index: Integer]: T read GetItem write SetItem; default;
  end;

  IObjectList = interface(IList<TObject>)
    ['{78A32DC5-1A5B-4191-9CA5-006CD85CF1AA}']
    // DO NOT ADD ANY METHODS HERE!!!
  end;

  IInterfaceList = interface(IList<IInterface>)
    ['{B6BF9A6E-797C-4982-8D0D-B935E43D917E}']
    // DO NOT ADD ANY METHODS HERE!!!
  end;

  {$REGION 'Documentation'}
  /// <summary>
  ///   Represents a node in a
  ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />. This class
  ///   cannot be inherited.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the element type of the linked list.
  /// </typeparam>
  {$ENDREGION}
  TLinkedListNode<T> = class sealed
  protected
    fList: Pointer;
    fNext: TLinkedListNode<T>;
    fPrev: TLinkedListNode<T>;
    fItem: T;
    function GetList: ILinkedList<T>;
    function GetNext: TLinkedListNode<T>;
    function GetPrevious: TLinkedListNode<T>;
  public
    constructor Create(const value: T); overload;

    property List: ILinkedList<T> read GetList;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Gets the next node in the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </summary>
    /// <value>
    ///   A reference to the next node in the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />, or <b>nil</b>
    ///   if the current node is the last element (Last) of the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </value>
    {$ENDREGION}
    property Next: TLinkedListNode<T> read GetNext;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Gets the previous node in the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </summary>
    /// <value>
    ///   A reference to the previous node in the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />, or null if
    ///   the current node is the first element (First) of the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </value>
    {$ENDREGION}
    property Previous: TLinkedListNode<T> read GetPrevious;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Gets the value contained in the node.
    /// </summary>
    /// <value>
    ///   The value contained in the node.
    /// </value>
    {$ENDREGION}
    property Value: T read fItem write fItem;
  end;

  {$REGION 'Documentation'}
  /// <summary>
  ///   Represents a doubly linked list.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the element type of the linked list.
  /// </typeparam>
  {$ENDREGION}
  ILinkedList<T> = interface(ICollection<T>)
    ['{73351AD9-15A5-4DA0-9BB7-D8FF66A3077E}']
  {$REGION 'Property Accessors'}
    function GetFirst: TLinkedListNode<T>;
    function GetLast: TLinkedListNode<T>;
    function GetOnChanged: ICollectionChangedEvent<T>;
  {$ENDREGION}

    {$REGION 'Documentation'}
    /// <summary>
    ///   Adds the specified new node after the specified existing node in the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </summary>
    /// <param name="node">
    ///   The <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> after
    ///   which to insert <i>newNode</i>.
    /// </param>
    /// <param name="value">
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> to
    ///   add to the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </param>
    /// <exception cref="Spring|EArgumentNullException">
    ///   <para>
    ///     <i>node</i> is <b>nil</b>.
    ///   </para>
    ///   <para>
    ///     -or-
    ///   </para>
    ///   <para>
    ///     <i>newNode</i> is <b>nil</b>.
    ///   </para>
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <para>
    ///     <i>node</i> is not in the current
    ///     <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    ///   </para>
    ///   <para>
    ///     -or-
    ///   </para>
    ///   <para>
    ///     <i>newNode</i> belongs to another
    ///     <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    ///   </para>
    /// </exception>
    {$ENDREGION}
    procedure AddAfter(const node: TLinkedListNode<T>; const newNode: TLinkedListNode<T>); overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Adds a new node containing the specified value after the specified
    ///   existing node in the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </summary>
    /// <param name="node">
    ///   The <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> after
    ///   which to insert a new
    ///   <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> containing
    ///   <i>value</i>.
    /// </param>
    /// <param name="value">
    ///   The value to add to the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </param>
    /// <returns>
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   containing <i>value</i>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNullException">
    ///   <i>node</i> is <b>nil</b>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <i>node</i> is not in the current
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </exception>
    {$ENDREGION}
    function AddAfter(const node: TLinkedListNode<T>; const value: T): TLinkedListNode<T>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Adds the specified new node before the specified existing node in the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </summary>
    /// <param name="node">
    ///   The <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> before
    ///   which to insert <i>newNode</i>.
    /// </param>
    /// <param name="newNode">
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> to
    ///   add to the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </param>
    /// <exception cref="Spring|EArgumentNullException">
    ///   <para>
    ///     <i>node</i> is <b>nil</b>.
    ///   </para>
    ///   <para>
    ///     -or-
    ///   </para>
    ///   <para>
    ///     <i>newNode</i> is <b>nil</b>.
    ///   </para>
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <para>
    ///     <i>node</i> is not in the current
    ///     <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    ///   </para>
    ///   <para>
    ///     -or-
    ///   </para>
    ///   <para>
    ///     <i>newNode</i> belongs to another
    ///     <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    ///   </para>
    /// </exception>
    {$ENDREGION}
    procedure AddBefore(const node: TLinkedListNode<T>; const newNode: TLinkedListNode<T>); overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Adds a new node containing the specified value before the specified
    ///   existing node in the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </summary>
    /// <param name="node">
    ///   The <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> before
    ///   which to insert a new
    ///   <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> containing
    ///   <i>value</i>.
    /// </param>
    /// <param name="value">
    ///   The value to add to the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </param>
    /// <returns>
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   containing <i>value</i>.
    /// </returns>
    /// <exception cref="Spring|EArgumentNullException">
    ///   <i>node</i> is <b>nil</b>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <i>node</i> is not in the current
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </exception>
    {$ENDREGION}
    function AddBefore(const node: TLinkedListNode<T>; const value: T): TLinkedListNode<T>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Adds the specified new node at the start of the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </summary>
    /// <param name="node">
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> to
    ///   add at the start of the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </param>
    /// <exception cref="Spring|EArgumentNullException">
    ///   <i>node</i> is <b>nil</b>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <i>node</i> belongs to another
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </exception>
    {$ENDREGION}
    procedure AddFirst(const node: TLinkedListNode<T>); overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Adds a new node containing the specified value at the start of the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </summary>
    /// <param name="value">
    ///   The value to add at the start of the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </param>
    /// <returns>
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   containing <i>value</i>.
    /// </returns>
    {$ENDREGION}
    function AddFirst(const value: T): TLinkedListNode<T>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Adds the specified new node at the end of the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </summary>
    /// <param name="node">
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> to
    ///   add at the end of the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </param>
    /// <exception cref="Spring|EArgumentNullException">
    ///   <i>node</i> is <b>nil</b>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <i>node</i> belongs to another
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </exception>
    {$ENDREGION}
    procedure AddLast(const node: TLinkedListNode<T>); overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Adds a new node containing the specified value at the end of the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </summary>
    /// <param name="value">
    ///   The value to add at the end of the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </param>
    /// <returns>
    ///   The new <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   containing <i>value</i>.
    /// </returns>
    {$ENDREGION}
    function AddLast(const value: T): TLinkedListNode<T>; overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Finds the first node that contains the specified value.
    /// </summary>
    /// <param name="value">
    ///   The value to locate in the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </param>
    /// <returns>
    ///   The first <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   that contains the specified value, if found; otherwise, <b>nil</b>.
    /// </returns>
    {$ENDREGION}
    function Find(const value: T): TLinkedListNode<T>;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Finds the last node that contains the specified value.
    /// </summary>
    /// <param name="value">
    ///   The value to locate in the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </param>
    /// <returns>
    ///   The last <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   that contains the specified value, if found; otherwise, <b>nil</b>.
    /// </returns>
    {$ENDREGION}
    function FindLast(const value: T): TLinkedListNode<T>;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Removes the specified node from the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </summary>
    /// <param name="node">
    ///   The <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" /> to
    ///   remove from the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />
    ///    .
    /// </param>
    /// <exception cref="Spring|EArgumentNullException">
    ///   <i>node</i> is <b>nil</b>.
    /// </exception>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   <i>node</i> is not in the current
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </exception>
    {$ENDREGION}
    procedure Remove(const node: TLinkedListNode<T>); overload;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Removes the node at the start of the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </summary>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The <see cref="Spring.Collections|ILinkedList&lt;T&gt;" /> is empty.
    /// </exception>
    {$ENDREGION}
    procedure RemoveFirst;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Removes the node at the end of the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </summary>
    /// <exception cref="Spring|EInvalidOperationException">
    ///   The <see cref="Spring.Collections|ILinkedList&lt;T&gt;" /> is empty.
    /// </exception>
    {$ENDREGION}
    procedure RemoveLast;

    {$WARNINGS OFF}
    {$REGION 'Documentation'}
    /// <summary>
    ///   Gets the first node of the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </summary>
    /// <value>
    ///   The first <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </value>
    {$ENDREGION}
    property First: TLinkedListNode<T> read GetFirst;

    {$REGION 'Documentation'}
    /// <summary>
    ///   Gets the last node of the
    ///   <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </summary>
    /// <value>
    ///   The last <see cref="Spring.Collections|TLinkedListNode&lt;T&gt;" />
    ///   of the <see cref="Spring.Collections|ILinkedList&lt;T&gt;" />.
    /// </value>
    {$ENDREGION}
    property Last: TLinkedListNode<T> read GetLast;
    {$WARNINGS ON}

    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

//  TKeyValuePair<TKey, TValue> = record
//  private
//    fKey: TKey;
//    fValue: TValue;
//  public
//    constructor Create(const key: TKey; const value: TValue);
//
//    property Key: TKey read fKey;
//    property Value: TValue read fValue;
//  end;

  IReadOnlyDictionary = interface(IReadOnlyCollection)
    ['{D963ED30-C16F-488B-9BC6-1292DD57B295}']
  {$REGION 'Property Accessors'}
    function GetKeyType: PTypeInfo;
    function GetValueType: PTypeInfo;
  {$ENDREGION}

    function ContainsKey(const key: TValue): Boolean;
    function ContainsValue(const value: TValue): Boolean;
    function TryGetValue(const key: TValue; out value: TValue): Boolean;

    property KeyType: PTypeInfo read GetKeyType;
    property ValueType: PTypeInfo read GetValueType;
  end;

  IDictionary = interface(ICollection)
    ['{9AC642EE-F236-421D-8546-DCA0D8D53791}']
  {$REGION 'Property Accessors'}
    function GetOnKeyChanged: IEvent;
    function GetOnValueChanged: IEvent;
    function GetKeyType: PTypeInfo;
    function GetValueType: PTypeInfo;
  {$ENDREGION}

    procedure Add(const key, value: TValue);
    procedure AddOrSetValue(const key, value: TValue);

    function Remove(const key: TValue): Boolean; overload;

    function ExtractPair(const key: TValue): TPair<TValue, TValue>;

    function ContainsKey(const key: TValue): Boolean;
    function ContainsValue(const value: TValue): Boolean;

    function TryGetValue(const key: TValue; out value: TValue): Boolean;

    function AsReadOnlyDictionary: IReadOnlyDictionary;

    property OnKeyChanged: IEvent read GetOnKeyChanged;
    property OnValueChanged: IEvent read GetOnValueChanged;
    property KeyType: PTypeInfo read GetKeyType;
    property ValueType: PTypeInfo read GetValueType;
  end;

  ///	<summary>
  ///	  Represents a generic read-only collection of key/value pairs.
  ///	</summary>
  ///	<typeparam name="TKey">
  ///	  The type of keys in the read-only dictionary.
  ///	</typeparam>
  ///	<typeparam name="TValue">
  ///	  The type of values in the read-only dictionary.
  ///	</typeparam>
  IReadOnlyDictionary<TKey, TValue> = interface(IReadOnlyCollection<TPair<TKey, TValue>>)
    ['{39F7C68B-373E-4758-808C-705D3978E38F}']
  {$REGION 'Property Accessors'}
    function GetItem(const key: TKey): TValue;
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetKeyType: PTypeInfo;
    function GetValues: IReadOnlyCollection<TValue>;
    function GetValueType: PTypeInfo;
  {$ENDREGION}

    /// <summary>
    ///   Determines whether the read-only dictionary contains the specified
    ///   key/value pair.
    /// </summary>
    function ContainsPair(const key: TKey; const value: TValue): Boolean;

    ///	<summary>
    ///	  Determines whether the read-only dictionary contains an element that
    ///	  has the specified key.
    ///	</summary>
    function ContainsKey(const key: TKey): Boolean;

    ///	<summary>
    ///	  Determines whether the read-only dictionary contains an element that
    ///	  has the specified value.
    ///	</summary>
    function ContainsValue(const value: TValue): Boolean;

    ///	<summary>
    ///	  Gets the value associated with the specified key.
    ///	</summary>
    ///	<param name="key">
    ///	  The key whose value to get.
    ///	</param>
    ///	<param name="value">
    ///	  When this method returns, the value associated with the specified
    ///	  key, if the key is found; otherwise, the default value for the type
    ///	  of the value parameter. This parameter is passed uninitialized.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the object that implements IDictionary&lt;TKey,
    ///	  TValue&gt; contains an element with the specified key; otherwise,
    ///	  <b>False</b>.
    ///	</returns>
    function TryGetValue(const key: TKey; out value: TValue): Boolean;

    ///	<summary>
    ///	  Gets the element that has the specified key in the read-only
    ///	  dictionary.
    ///	</summary>
    property Items[const key: TKey]: TValue read GetItem; default;

    ///	<summary>
    ///	  Gets an enumerable collection that contains the keys in the read-only
    ///	  dictionary.
    ///	</summary>
    property Keys: IReadOnlyCollection<TKey> read GetKeys;

    ///	<summary>
    ///	  Gets an enumerable collection that contains the values in the
    ///	  read-only dictionary.
    ///	</summary>
    property Values: IReadOnlyCollection<TValue> read GetValues;
    property KeyType: PTypeInfo read GetKeyType;
    property ValueType: PTypeInfo read GetValueType;
  end;

  ///	<summary>
  ///	  Represents a generic collection of key/value pairs.
  ///	</summary>
  ///	<typeparam name="TKey">
  ///	  The type of keys in the map.
  ///	</typeparam>
  ///	<typeparam name="TValue">
  ///	  The type of values in the map.
  ///	</typeparam>
  IMap<TKey, TValue> = interface(ICollection<TPair<TKey, TValue>>)
    ['{94262688-16E4-4092-926B-7B17FEF94A86}']
  {$REGION 'Property Accessors'}
    function GetKeys: IReadOnlyCollection<TKey>;
    function GetKeyType: PTypeInfo;
    function GetOnKeyChanged: ICollectionChangedEvent<TKey>;
    function GetOnValueChanged: ICollectionChangedEvent<TValue>;
    function GetValues: IReadOnlyCollection<TValue>;
    function GetValueType: PTypeInfo;
  {$ENDREGION}

    /// <summary>
    ///   Adds an element with the provided key and value to the
    ///   IMap&lt;TKey,TValue&gt;.
    /// </summary>
    /// <param name="key">
    ///   The value to use as the key of the element to add.
    /// </param>
    /// <param name="value">
    ///   The value to use as the value of the element to add.
    /// </param>
    procedure Add(const key: TKey; const value: TValue); overload;

    ///	<summary>
    ///	  Removes the element with the specified key from the
    ///	  IMap&lt;TKey, TValue&gt;.
    ///	</summary>
    ///	<param name="key">
    ///	  The key of the element to remove.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the element is successfully removed; otherwise,
    ///	  <b>False</b>. This method also returns <b>False</b> if <i>key</i> was
    ///	  not found in the original IMap&lt;TKey, TValue&gt;.
    ///	</returns>
    function Remove(const key: TKey): Boolean; overload;

    /// <summary>
    ///   Removes the specified key/value pair from the IMap&lt;TKey,
    ///   TValue&gt;.
    /// </summary>
    /// <param name="key">
    ///   The key of the pair to remove.
    /// </param>
    /// <param name="value">
    ///   The value of the pair to remove,
    /// </param>
    /// <returns>
    ///   <b>True</b> if the pair was successfully removed; otherwise, <b>False</b>
    ///   .
    /// </returns>
    function RemovePair(const key: TKey; const value: TValue): Boolean; overload;

    function ExtractPair(const key: TKey; const value: TValue): TPair<TKey, TValue>;

    /// <summary>
    ///   Determines whether the IMap&lt;TKey,TValue&gt; contains the specified
    ///   key/value pair.
    /// </summary>
    /// <param name="key">
    ///   The key of the pair to locate in the IMap&lt;TKey, TValue&gt;.
    /// </param>
    /// <param name="value">
    ///   The value of the pair to locate in the IMap&lt;TKey, TValue&gt;.
    /// </param>
    /// <returns>
    ///   <b>True</b> if the IMap&lt;TKey, TValue&gt; contains a pair with the
    ///   specified key and value; otherwise <b>False</b>.
    /// </returns>
    function ContainsPair(const key: TKey; const value: TValue): Boolean;

    ///	<summary>
    ///	  Determines whether the IMap&lt;TKey, TValue&gt; contains an
    ///	  element with the specified key.
    ///	</summary>
    ///	<param name="key">
    ///	  The key to locate in the IMap&lt;TKey, TValue&gt;.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the IMap&lt;TKey, TValue&gt; contains an
    ///	  element with the key; otherwise, <b>False</b>.
    ///	</returns>
    function ContainsKey(const key: TKey): Boolean;

    ///	<summary>
    ///	  Determines whether the IMap&lt;TKey, TValue&gt; contains an
    ///	  element with the specified value.
    ///	</summary>
    ///	<param name="value">
    ///	  The value to locate in the IMap&lt;TKey, TValue&gt;.
    ///	</param>
    function ContainsValue(const value: TValue): Boolean;

    ///	<summary>
    ///	  Gets an <see cref="IReadOnlyCollection&lt;T&gt;" /> containing the
    ///	  keys of the IMap&lt;TKey, TValue&gt;.
    ///	</summary>
    ///	<value>
    ///	  An <see cref="IReadOnlyCollection&lt;T&gt;" /> containing the keys of
    ///	  the object that implements IDictionary&lt;TKey, TValue&gt;.
    ///	</value>
    property Keys: IReadOnlyCollection<TKey> read GetKeys;

    ///	<summary>
    ///	  Gets an <see cref="IReadOnlyCollection&lt;T&gt;" /> containing the
    ///	  values in the IMap&lt;TKey, TValue&gt;.
    ///	</summary>
    ///	<value>
    ///	  An <see cref="IReadOnlyCollection&lt;T&gt;" /> containing the values
    ///	  in the object that implements IMap&lt;TKey, TValue&gt;.
    ///	</value>
    property Values: IReadOnlyCollection<TValue> read GetValues;

    property KeyType: PTypeInfo read GetKeyType;
    property OnKeyChanged: ICollectionChangedEvent<TKey> read GetOnKeyChanged;
    property OnValueChanged: ICollectionChangedEvent<TValue> read GetOnValueChanged;
    property ValueType: PTypeInfo read GetValueType;
  end;

  ///	<summary>
  ///	  Represents a generic collection of key/value pairs.
  ///	</summary>
  ///	<typeparam name="TKey">
  ///	  The type of keys in the dictionary.
  ///	</typeparam>
  ///	<typeparam name="TValue">
  ///	  The type of values in the dictionary.
  ///	</typeparam>
  IDictionary<TKey, TValue> = interface(IMap<TKey, TValue>)
    ['{7F0D544F-6A59-4FA0-9C96-DB09029CC835}']
  {$REGION 'Property Accessors'}
    function GetItem(const key: TKey): TValue;
    procedure SetItem(const key: TKey; const value: TValue);
  {$ENDREGION}

    /// <summary>
    ///   Adds an element with the provided key and value to the
    ///   IDictionary&lt;TKey,TValue&gt;. If it already exists in the
    ///   dictionary the provided value for the specified key is set.
    /// </summary>
    /// <param name="key">
    ///   The value to use as the key of the element to add or set.
    /// </param>
    /// <param name="value">
    ///   The value to use as the value of the element to add or set.
    /// </param>
    procedure AddOrSetValue(const key: TKey; const value: TValue);

    /// <summary>
    ///   Removes the value for a specified key without triggering lifetime
    ///   management for objects.
    /// </summary>
    /// <param name="key">
    ///   The key whose value to remove.
    /// </param>
    /// <returns>
    ///   The removed value for the specified key if it existed; <b>default</b>
    ///   otherwise.
    /// </returns>
    function Extract(const key: TKey): TValue; overload;

    /// <summary>
    ///   Removes the value for a specified key without triggering lifetime
    ///   management for objects.
    /// </summary>
    /// <param name="key">
    ///   The key whose value to remove.
    /// </param>
    /// <returns>
    ///   The removed pair for the specified key if it existed; <b>default</b>
    ///   otherwise.
    /// </returns>
    function ExtractPair(const key: TKey): TPair<TKey, TValue>; overload;

    ///	<summary>
    ///	  Gets the value associated with the specified key.
    ///	</summary>
    ///	<param name="key">
    ///	  The key whose value to get.
    ///	</param>
    ///	<param name="value">
    ///	  When this method returns, the value associated with the specified
    ///	  key, if the key is found; otherwise, the default value for the type
    ///	  of the value parameter. This parameter is passed uninitialized.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the object that implements IDictionary&lt;TKey,
    ///	  TValue&gt; contains an element with the specified key; otherwise,
    ///	  <b>False</b>.
    ///	</returns>
    function TryGetValue(const key: TKey; out value: TValue): Boolean;

    /// <summary>
    ///   Returns the dictionary as read-only dictionary.
    /// </summary>
    /// <remarks>
    ///   This method will not perform a copy but will return the same instance
    ///   as IReadOnlyDictionary&lt;TKey, TValue&gt;.
    /// </remarks>
    function AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;

    ///	<summary>
    ///	  Gets or sets the element with the specified key.
    ///	</summary>
    ///	<param name="key">
    ///	  The key of the element to get or set.
    ///	</param>
    ///	<value>
    ///	  The element with the specified key.
    ///	</value>
    property Items[const key: TKey]: TValue read GetItem write SetItem; default;
  end;

  IBidiDictionary<TKey, TValue> = interface(IDictionary<TKey, TValue>)//IMap<TKey, TValue>)
    ['{DA8F1C48-B4F4-4487-ADAD-AF15596DD53C}']
  {$REGION 'Property Accessors'}
    function GetKey(const value: TValue): TKey;
    function GetValue(const key: TKey): TValue;
    procedure SetKey(const value: TValue; const key: TKey);
    procedure SetValue(const key: TKey; const value: TValue);
  {$ENDREGION}

    /// <summary>
    ///   Remove the key that is currently mapped to the specified value
    ///   without triggering lifetime management for objects.
    /// </summary>
    function ExtractKey(const value: TValue): TKey;

    /// <summary>
    ///   Remove the value that is currently mapped to the specified key
    ///   without triggering lifetime management for objects.
    /// </summary>
    function ExtractValue(const key: TKey): TValue;

    /// <summary>
    ///   Remove the key/value pair that is currently mapped to the specified
    ///   key.
    /// </summary>
    function RemoveKey(const key: TKey): Boolean;

    /// <summary>
    ///   Remove the key/value pair that is currently mapped to the specified
    ///   value.
    /// </summary>
    function RemoveValue(const value: TValue): Boolean;

    /// <summary>
    ///   Gets the key associated with the specified key.
    /// </summary>
    /// <returns>
    ///   <b>True</b> if the key was found; otherwise, <b>False</b>.
    /// </returns>
    function TryGetKey(const value: TValue; out key: TKey): Boolean;

    /// <summary>
    ///   Gets the value associated with the specified key.
    /// </summary>
    /// <returns>
    ///   <b>True</b> if the value was found; otherwise, <b>False.</b>
    /// </returns>
    function TryGetValue(const key: TKey; out value: TValue): Boolean;

    property Key[const value: TValue]: TKey read GetKey write SetKey;
    property Value[const key: TKey]: TValue read GetValue write SetValue; default;
  end;

  IMultiMap<TKey, TValue> = interface(IMap<TKey, TValue>)
    ['{8598095E-92A7-4FCC-9F78-8EE7653B8B49}']
  {$REGION 'Property Accessors'}
    function GetItems(const key: TKey): IReadOnlyCollection<TValue>;
  {$ENDREGION}

    function ExtractValues(const key: TKey): IReadOnlyCollection<TValue>;
    function TryGetValues(const key: TKey; out values: IReadOnlyCollection<TValue>): Boolean;
    property Items[const key: TKey]: IReadOnlyCollection<TValue> read GetItems; default;
  end;

  IStack = interface(IEnumerable)
    ['{82F7B40F-3B32-417F-8001-51458BCE553A}']
  {$REGION 'Property Accessors'}
    function GetOnChanged: IEvent;
  {$ENDREGION}

    procedure Clear;
    procedure Push(const item: TValue);
    function Pop: TValue;
    function Peek: TValue;
    function PeekOrDefault: TValue;
    function TryPeek(out item: TValue): Boolean;
    property OnChanged: IEvent read GetOnChanged;
  end;

  ///	<summary>
  ///	  Represents a variable size last-in-first-out (LIFO) collection of
  ///	  instances of the same arbitrary type.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  Specifies the type of elements in the stack.
  ///	</typeparam>
  IStack<T> = interface(IEnumerable<T>)
    ['{5BD7BDD3-0198-4727-B97C-658BF194FF63}']
  {$REGION 'Property Accessors'}
    function GetOnChanged: ICollectionChangedEvent<T>;
  {$ENDREGION}

    ///	<summary>
    ///	  Removes all elements from the IStack&lt;T&gt;.
    ///	</summary>
    procedure Clear;

    ///	<summary>
    ///	  Inserts an element at the top of the IStack&lt;T&gt;.
    ///	</summary>
    ///	<param name="item">
    ///	  The element to push onto the IStack&lt;T&gt;. The value can be 
    ///	  <b>nil</b> for reference types.
    ///	</param>
    procedure Push(const item: T);

    ///	<summary>
    ///	  Removes and returns the element at the top of the
    ///	  IStack&lt;T&gt;.
    ///	</summary>
    ///	<returns>
    ///	  The element removed from the top of the IStack&lt;T&gt;.
    ///	</returns>
    function Pop: T;

    ///	<summary>
    ///	  Returns the element at the top of the IStack&lt;T&gt; without
    ///	  removing it.
    ///	</summary>
    ///	<returns>
    ///	  The element at the top of the IStack&lt;T&gt;.
    ///	</returns>
    function Peek: T;
    function PeekOrDefault: T;
    function TryPeek(out item: T): Boolean;

    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

  IQueue = interface(IEnumerable)
    ['{B3377E32-ADA1-414F-8762-1EA0E4FEF794}']
  {$REGION 'Property Accessors'}
    function GetOnChanged: IEvent;
  {$ENDREGION}

    procedure Clear;
    procedure Enqueue(const item: TValue);
    function Dequeue: TValue;
    function Peek: TValue;
    function PeekOrDefault: TValue;
    function TryPeek(out item: TValue): Boolean;
    property OnChanged: IEvent read GetOnChanged;
  end;

  ///	<summary>
  ///	  Represents a first-in, first-out collection of elements.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  Specifies the type of elements in the queue.
  ///	</typeparam>
  IQueue<T> = interface(IEnumerable<T>)
    ['{D305A076-3F19-497C-94E3-6BD1C7A30F3F}']
  {$REGION 'Property Accessors'}
    function GetOnChanged: ICollectionChangedEvent<T>;
  {$ENDREGION}

    ///	<summary>
    ///	  Removes all elements from the IQueue&lt;T&gt;.
    ///	</summary>
    procedure Clear;

    ///	<summary>
    ///	  Adds an element to the end of the IQueue&lt;T&gt;. 
    ///	</summary>
    ///	<param name="item">
    ///	  The element to add to the IQueue&lt;T&gt;. The value can be <b>nil</b>
    ///	   for reference types.
    ///	</param>
    procedure Enqueue(const item: T);

    ///	<summary>
    ///	  Removes and returns the element at the beginning of the
    ///	  IQueue&lt;T&gt;. 
    ///	</summary>
    ///	<returns>
    ///	  The element that is removed from the beginning of the IQueue&lt;T&gt;.
    ///	</returns>
    function Dequeue: T;

    ///	<summary>
    ///	  Returns the element at the beginning of the IQueue&lt;T&gt; without
    ///	  removing it.
    ///	</summary>
    ///	<returns>
    ///	  The element at the beginning of the IQueue&lt;T&gt;.
    ///	</returns>
    function Peek: T;
    function PeekOrDefault: T;
    function TryPeek(out item: T): Boolean;

    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

  ISet = interface(ICollection)
    ['{D83ED568-A7C8-4142-BA0F-5A273AF1AA07}']
    function Add(const item: TValue): Boolean;
    procedure ExceptWith(const other: IEnumerable);
    procedure IntersectWith(const other: IEnumerable);
    procedure UnionWith(const other: IEnumerable);
    function IsSubsetOf(const other: IEnumerable): Boolean;
    function IsSupersetOf(const other: IEnumerable): Boolean;
    function SetEquals(const other: IEnumerable): Boolean;
    function Overlaps(const other: IEnumerable): Boolean;
  end;

  ///	<summary>
  ///	  Provides the base interface for the abstraction of sets.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type of elements in the set.
  ///	</typeparam>
  ISet<T> = interface(ICollection<T>)
    ['{DC0B211F-E9FD-41D6-BEE0-FCB9F79327AB}']

    ///	<summary>
    ///	  Adds an element to the current set and returns a value to indicate if
    ///	  the element was successfully added.
    ///	</summary>
    ///	<param name="item">
    ///	  The element to add to the set.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the element is added to the set; <b>False</b> if the
    ///	  element is already in the set.
    ///	</returns>
    function Add(const item: T): Boolean;

    ///	<summary>
    ///	  Removes all elements in the specified collection from the current set.
    ///	</summary>
    ///	<param name="other">
    ///	  The collection of items to remove from the set.
    ///	</param>
    ///	<exception cref="EArgumentNullException">
    ///	  <i>other</i> is <b>nil</b>.
    ///	</exception>
    procedure ExceptWith(const other: IEnumerable<T>);

    ///	<summary>
    ///	  Modifies the current set so that it contains only elements that are
    ///	  also in a specified collection.
    ///	</summary>
    ///	<param name="other">
    ///	  The collection to compare to the current set.
    ///	</param>
    ///	<exception cref="EArgumentNullException">
    ///	  <i>other</i> is <b>nil</b>.
    ///	</exception>
    procedure IntersectWith(const other: IEnumerable<T>);

    ///	<summary>
    ///	  Modifies the current set so that it contains all elements that are
    ///	  present in either the current set or the specified collection.
    ///	</summary>
    ///	<param name="other">
    ///	  The collection to compare to the current set.
    ///	</param>
    ///	<exception cref="EArgumentNullException">
    ///	  <i>other</i> is <b>nil</b>.
    ///	</exception>
    procedure UnionWith(const other: IEnumerable<T>);

    ///	<summary>
    ///	  Determines whether a set is a subset of a specified collection.
    ///	</summary>
    ///	<param name="other">
    ///	  The collection to compare to the current set.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the current set is a subset of <i>other</i>;
    ///	  otherwise, <b>False</b>.
    ///	</returns>
    ///	<exception cref="EArgumentNullException">
    ///	  <i>other</i> is <b>nil</b>.
    ///	</exception>
    function IsSubsetOf(const other: IEnumerable<T>): Boolean;

    ///	<summary>
    ///	  Determines whether the current set is a superset of a specified
    ///	  collection.
    ///	</summary>
    ///	<param name="other">
    ///	  The collection to compare to the current set.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the current set is a superset of <i>other</i>;
    ///	  otherwise, <b>False</b>.
    ///	</returns>
    ///	<exception cref="EArgumentNullException">
    ///	  <i>other</i> is <b>nil</b>.
    ///	</exception>
    function IsSupersetOf(const other: IEnumerable<T>): Boolean;

    ///	<summary>
    ///	  Determines whether the current set and the specified collection
    ///	  contain the same elements.
    ///	</summary>
    ///	<param name="other">
    ///	  The collection to compare to the current set.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the current set is equal to <i>other</i>;
    ///	  otherwise, <b>False</b>.
    ///	</returns>
    ///	<exception cref="EArgumentNullException">
    ///	  <i>other</i> is <b>nil</b>.
    ///	</exception>
    function SetEquals(const other: IEnumerable<T>): Boolean;

    ///	<summary>
    ///	  Determines whether the current set overlaps with the specified
    ///	  collection.
    ///	</summary>
    ///	<param name="other">
    ///	  The collection to compare to the current set.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the current set and <i>other</i> share at least
    ///	  one common element; otherwise, <b>False</b>.
    ///	</returns>
    ///	<exception cref="EArgumentNullException">
    ///	  <i>other</i> is <b>nil</b>.
    ///	</exception>
    function Overlaps(const other: IEnumerable<T>): Boolean;
  end;

  ///	<summary>
  ///	  Represents a collection of elements that have a common key.
  ///	</summary>
  ///	<typeparam name="TKey">
  ///	  The type of the key of the IGrouping&lt;TKey, TElement&gt;.
  ///	</typeparam>
  ///	<typeparam name="TElement">
  ///	  The type of the values in the IGrouping&lt;TKey, TElement&gt;.
  ///	</typeparam>
  IGrouping<TKey, TElement> = interface(IEnumerable<TElement>)
    ['{CFC3071C-663A-400A-B21B-1F5E28BA4892}']
  {$REGION 'Property Accessors'}
    function GetKey: TKey;
  {$ENDREGION}

    ///	<summary>
    ///	  Gets the key of the IGrouping&lt;TKey, TElement&gt;.
    ///	</summary>
    ///	<value>
    ///	  The key of the IGrouping&lt;TKey, TElement&gt;.
    ///	</value>
    property Key: TKey read GetKey;
  end;

  ///	<summary>
  ///	  Defines an indexer, size property, and Boolean search method for data
  ///	  structures that map keys to <see cref="IEnumerable&lt;T&gt;" />
  ///	  sequences of values.
  ///	</summary>
  ///	<typeparam name="TKey">
  ///	  The type of the keys in the ILookup&lt;TKey, TElement&gt;.
  ///	</typeparam>
  ///	<typeparam name="TElement">
  ///	  The type of the elements in the <see cref="IEnumerable&lt;T&gt;" />
  ///	  sequences that make up the values in the ILookup&lt;TKey, TElement&gt;.
  ///	</typeparam>
  ILookup<TKey, TElement> = interface(IEnumerable<IGrouping<TKey, TElement>>)
    ['{B2380533-F2B1-465B-84B2-97FA79A6EE09}']
  {$REGION 'Property Accessors'}
    function GetItem(const key: TKey): IEnumerable<TElement>;
  {$ENDREGION}

    ///	<summary>
    ///	  Determines whether a specified key exists in the ILookup&lt;TKey,
    ///	  TElement&gt;.
    ///	</summary>
    ///	<param name="key">
    ///	  The key to search for in the ILookup&lt;TKey, TElement&gt;.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if <i>key</i> is in the ILookup&lt;TKey, TElement&gt;;
    ///	  otherwise, <b>False</b>.
    ///	</returns>
    function Contains(const key: TKey): Boolean;

    ///	<summary>
    ///	  Gets the <see cref="IEnumerable&lt;T&gt;" /> sequence of values
    ///	  indexed by a specified key.
    ///	</summary>
    ///	<param name="key">
    ///	  The key of the desired sequence of values.
    ///	</param>
    ///	<value>
    ///	  The <see cref="IEnumerable&lt;T&gt;" /> sequence of values indexed by
    ///	  the specified key.
    ///	</value>
    property Item[const key: TKey]: IEnumerable<TElement> read GetItem; default;
  end;

  ///	<summary>
  ///	  Internal interface. Reserved for future use.
  ///	</summary>
  IElementType = interface
    ['{FE986DD7-41D5-4312-A2F9-94F7D9E642EE}']
  {$REGION 'Property Accessors'}
    function GetElementType: PTypeInfo;
  {$ENDREGION}

    property ElementType: PTypeInfo read GetElementType;
  end;

  ICollectionOwnership = interface
    ['{6D028EAF-3D14-4362-898C-BFAD1110547F}']
  {$REGION 'Property Accessors'}
      function GetOwnsObjects: Boolean;
      procedure SetOwnsObjects(const value: Boolean);
  {$ENDREGION}

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  /// <summary>
  ///   Provides direct access to an array that is used for internal storage.
  /// </summary>
  IArrayAccess<T> = interface(ICountable)
    ['{0C6C22BE-DBFD-4EBE-9E32-6E4BBA8AC382}']
  {$REGION 'Property Accessors'}
     function GetItems: TArray<T>;
  {$ENDREGION}

    property Items: TArray<T> read GetItems;
  end;

  ///	<summary>
  ///	  Defines the ownership style of an instance.
  ///	</summary>
  TOwnershipType = (
    otReference,
    otOwned
  );

  TDictionaryOwnerships = Generics.Collections.TDictionaryOwnerships;

  ///	<summary>
  ///	  Provides static methods to create an instance of various interfaced
  ///	  generic collections such as <see cref="IList&lt;T&gt;" /> or
  ///	  <see cref="IDictionary&lt;TKey, TValue&gt;" />.
  ///	</summary>
  TCollections = class
  public
    class function CreateList<T>: IList<T>; overload; static;
    class function CreateList<T>(const comparer: IComparer<T>): IList<T>; overload; static;
    class function CreateList<T>(const comparer: TComparison<T>): IList<T>; overload; static;
    class function CreateList<T>(const values: array of T): IList<T>; overload; static;
    class function CreateList<T>(const values: IEnumerable<T>): IList<T>; overload; static;
    class function CreateList<T: class>(ownsObjects: Boolean): IList<T>; overload; static;
    class function CreateList<T: class>(const comparer: IComparer<T>; ownsObjects: Boolean): IList<T>; overload; static;
    class function CreateList<T: class>(const comparer: TComparison<T>; ownsObjects: Boolean): IList<T>; overload; static;
    class function CreateObjectList<T: class>(ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateObjectList<T: class>(const comparer: IComparer<T>; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateObjectList<T: class>(const comparer: TComparison<T>; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateObjectList<T: class>(const values: array of T; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateObjectList<T: class>(const values: IEnumerable<T>; ownsObjects: Boolean = True): IList<T>; overload; static;
    class function CreateInterfaceList<T: IInterface>: IList<T>; overload; static;
    class function CreateInterfaceList<T: IInterface>(const comparer: IComparer<T>): IList<T>; overload; static;
    class function CreateInterfaceList<T: IInterface>(const comparer: TComparison<T>): IList<T>; overload; static;
    class function CreateInterfaceList<T: IInterface>(const values: array of T): IList<T>; overload; static;
    class function CreateInterfaceList<T: IInterface>(const values: IEnumerable<T>): IList<T>; overload; static;

    class function CreateDictionary<TKey, TValue>: IDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(capacity: Integer): IDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(capacity: Integer; const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships; capacity: Integer): IDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(ownerships: TDictionaryOwnerships; capacity: Integer; const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>; overload; static;
    class function CreateDictionary<TKey, TValue>(dictionary: Generics.Collections.TDictionary<TKey, TValue>; ownership: TOwnershipType): IDictionary<TKey, TValue>; overload; static;

    class function CreateMultiMap<TKey, TValue>: IMultiMap<TKey, TValue>; overload; static;
    class function CreateMultiMap<TKey, TValue>(ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>; overload; static;

    class function CreateBidiDictionary<TKey, TValue>: IBidiDictionary<TKey, TValue>; overload; static;

    class function CreateStack<T>: IStack<T>; overload; static;
    class function CreateStack<T: class>(ownsObjects: Boolean): IStack<T>; overload; static;
    class function CreateStack<T>(const values: array of T): IStack<T>; overload; static;
    class function CreateStack<T>(const values: IEnumerable<T>): IStack<T>; overload; static;

    class function CreateQueue<T>: IQueue<T>; overload; static;
    class function CreateQueue<T: class>(ownsObjects: Boolean): IQueue<T>; overload; static;
    class function CreateQueue<T>(const values: array of T): IQueue<T>; overload; static;
    class function CreateQueue<T>(const values: IEnumerable<T>): IQueue<T>; overload; static;

    class function CreateSet<T>: ISet<T>; overload; static;
    class function CreateSet<T>(const comparer: IEqualityComparer<T>): ISet<T>; overload; static;
    class function CreateSet<T>(const values: array of T): ISet<T>; overload; static;
    class function CreateSet<T>(const values: IEnumerable<T>): ISet<T>; overload; static;
  end;

  TEnumerable = class
  public
    ///	<summary>
    ///	  Returns an empty <see cref="IEnumerable&lt;T&gt;" /> that has the
    ///	  specified type argument.
    ///	</summary>
    ///	<typeparam name="T">
    ///	  The type to assign to the type parameter of the returned generic
    ///	  <see cref="IEnumerable&lt;T&gt;" />.
    ///	</typeparam>
    ///	<returns>
    ///	  An empty <see cref="IEnumerable&lt;T&gt;" /> whose type argument is
    ///	  <i>T</i>.
    ///	</returns>
    class function Empty<T>: IEnumerable<T>; static;

    class function Min<T>(const source: IEnumerable<T>;
      const selector: TFunc<T, Integer>): Integer; overload; static;
    class function Max<T>(const source: IEnumerable<T>;
      const selector: TFunc<T, Integer>): Integer; overload; static;

    class function Query<T>(const source: TArray<T>): IEnumerable<T>; overload; static;
    class function Query<T>(const source: TEnumerable<T>): IEnumerable<T>; overload; static;

    class function Range(start, count: Integer): IEnumerable<Integer>; static;

    class function Repeated<T>(const element: T; count: Integer): IEnumerable<T>; static;

    class function Select<T, TResult>(const source: IEnumerable<T>;
      const selector: TFunc<T, TResult>): IEnumerable<TResult>; overload; static;
  end;

  TStringComparer = class(TCustomComparer<string>)
  private
    fLocaleOptions: TLocaleOptions;
    fIgnoreCase: Boolean;
    class var
      fOrdinal: TStringComparer;
      fOrdinalIgnoreCase: TStringComparer;
  protected
    function Compare(const Left, Right: string): Integer; override;
    function Equals(const Left, Right: string): Boolean; override;
    function GetHashCode(const Value: string): Integer; override;
  public
    constructor Create(localeOptions: TLocaleOptions; ignoreCase: Boolean);
    class constructor Create;
    class destructor Destroy;

    class function Ordinal: TStringComparer;
    class function OrdinalIgnoreCase: TStringComparer;
  end;

  TInstanceComparer<T> = class
  public
    class function Default: IComparer<T>;
  end;

  TCollectionHelper = class helper for TCollection
  public
    function AsList: IList<TCollectionItem>; overload;
    function AsList<T: TCollectionItem>: IList<T>; overload;
  end;

function GetInstanceComparer: Pointer;

implementation

uses
  Character,
  SyncObjs,
  Spring.Collections.Dictionaries,
  Spring.Collections.Extensions,
  Spring.Collections.Lists,
  Spring.Collections.LinkedLists,
  Spring.Collections.MultiMaps,
  Spring.Collections.Queues,
  Spring.Collections.Sets,
  Spring.Collections.Stacks,
  Spring.ResourceStrings;


{$REGION 'Instance comparer'}
function NopAddref(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopRelease(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopQueryInterface(inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result := E_NOINTERFACE;
end;

function Compare_Instance(Inst: Pointer; const Left, Right: TObject): Integer;
var
  comparable: IComparable;
begin
  if Supports(Left, IComparable, comparable) then
    Result := comparable.CompareTo(Right)
  else
    if NativeUInt(Left) < NativeUInt(Right) then
      Result := -1
    else if NativeUInt(Left) > NativeUInt(Right) then
      Result := 1
    else
      Result := 0;
end;

const
  InstanceComparer_VTable: array[0..3] of Pointer =
  (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease,
    @Compare_Instance
  );
  InstanceComparer: Pointer = @InstanceComparer_VTable;

function GetInstanceComparer: Pointer;
begin
  Result := @InstanceComparer;
end;
{$ENDREGION}


{$REGION 'TCollections'}

class function TCollections.CreateList<T>: IList<T>;
begin
  Result := TList<T>.Create;
end;

class function TCollections.CreateList<T>(const comparer: IComparer<T>): IList<T>;
begin
  Result := TList<T>.Create(comparer);
end;

class function TCollections.CreateList<T>(
  const comparer: TComparison<T>): IList<T>;
begin
  Result := TList<T>.Create(IComparer<T>(PPointer(@comparer)^));
end;

class function TCollections.CreateList<T>(const values: array of T): IList<T>;
begin
  Result := TList<T>.Create(values);
end;

class function TCollections.CreateList<T>(const values: IEnumerable<T>): IList<T>;
begin
  Result := TList<T>.Create(values);
end;

class function TCollections.CreateList<T>(ownsObjects: Boolean): IList<T>;
begin
  Result := TObjectList<T>.Create(ownsObjects) as IList<T>;
end;

class function TCollections.CreateList<T>(const comparer: IComparer<T>;
  ownsObjects: Boolean): IList<T>;
begin
  Result := TObjectList<T>.Create(comparer, ownsObjects) as IList<T>;
end;

class function TCollections.CreateList<T>(const comparer: TComparison<T>;
  ownsObjects: Boolean): IList<T>;
begin
  Result := TObjectList<T>.Create(
    IComparer<T>(PPointer(@comparer)^), ownsObjects) as IList<T>;
end;

class function TCollections.CreateObjectList<T>(ownsObjects: Boolean): IList<T>;
begin
  Result := TObjectList<T>.Create(ownsObjects) as IList<T>;
end;

class function TCollections.CreateObjectList<T>(const comparer: IComparer<T>;
  ownsObjects: Boolean): IList<T>;
begin
  Result := TObjectList<T>.Create(comparer, ownsObjects) as IList<T>;
end;

class function TCollections.CreateObjectList<T>(const comparer: TComparison<T>;
  ownsObjects: Boolean): IList<T>;
begin
  Result := TObjectList<T>.Create(
    IComparer<T>(PPointer(@comparer)^), ownsObjects) as IList<T>;
end;

class function TCollections.CreateObjectList<T>(const values: array of T;
  ownsObjects: Boolean): IList<T>;
begin
  Result := TObjectList<T>.Create(ownsObjects) as IList<T>;
  Result.AddRange(values);
end;

class function TCollections.CreateObjectList<T>(const values: IEnumerable<T>;
  ownsObjects: Boolean): IList<T>;
begin
  Result := TObjectList<T>.Create(ownsObjects) as IList<T>;
  Result.AddRange(values);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  dictionary: Generics.Collections.TDictionary<TKey, TValue>;
  ownership: TOwnershipType): IDictionary<TKey, TValue>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(dictionary, 'dictionary');
{$ENDIF}

  Result := TDictionary<TKey, TValue>.Create(dictionary, ownership);
end;

class function TCollections.CreateInterfaceList<T>: IList<T>;
begin
  Result := TInterfaceList<T>.Create as IList<T>;
end;

class function TCollections.CreateInterfaceList<T>(
  const comparer: IComparer<T>): IList<T>;
begin
  Result := TInterfaceList<T>.Create(comparer) as IList<T>;
end;

class function TCollections.CreateInterfaceList<T>(
  const comparer: TComparison<T>): IList<T>;
begin
  Result := TInterfaceList<T>.Create(
    IComparer<T>(PPointer(@comparer)^)) as IList<T>;
end;

class function TCollections.CreateInterfaceList<T>(
  const values: array of T): IList<T>;
begin
  Result := TInterfaceList<T>.Create as IList<T>;
  Result.AddRange(values);
end;

class function TCollections.CreateInterfaceList<T>(
  const values: IEnumerable<T>): IList<T>;
begin
  Result := TInterfaceList<T>.Create as IList<T>;
  Result.AddRange(values);
end;

class function TCollections.CreateDictionary<TKey, TValue>: IDictionary<TKey, TValue>;
begin
  Result := TCollections.CreateDictionary<TKey,TValue>(0, TEqualityComparer<TKey>.Default);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  capacity: Integer): IDictionary<TKey, TValue>;
begin
  Result := TCollections.CreateDictionary<TKey, TValue>(
    capacity, TEqualityComparer<TKey>.Default);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>;
begin
  Result := TCollections.CreateDictionary<TKey, TValue>(0, comparer);
end;

class function TCollections.CreateDictionary<TKey, TValue>(capacity: Integer;
  const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>;
var
  dictionary: Generics.Collections.TDictionary<TKey,TValue>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(capacity >= 0, 'capacity');
{$ENDIF}

  dictionary := Generics.Collections.TDictionary<TKey,TValue>.Create(capacity, comparer);
  Result := TDictionary<TKey, TValue>.Create(dictionary, otOwned);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IDictionary<TKey, TValue>;
begin
  Result := TCollections.CreateDictionary<TKey, TValue>(
    ownerships, 0, TEqualityComparer<TKey>.Default);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  ownerships: TDictionaryOwnerships;
  capacity: Integer): IDictionary<TKey, TValue>;
begin
  Result := TCollections.CreateDictionary<TKey, TValue>(
    ownerships, capacity, TEqualityComparer<TKey>.Default);
end;

class function TCollections.CreateDictionary<TKey, TValue>(
  ownerships: TDictionaryOwnerships; capacity: Integer;
  const comparer: IEqualityComparer<TKey>): IDictionary<TKey, TValue>;
var
  dictionary: Generics.Collections.TObjectDictionary<TKey,TValue>;
begin
  dictionary := TObjectDictionary<TKey, TValue>.Create(ownerships, capacity, comparer);
  Result := TDictionary<TKey, TValue>.Create(dictionary, otOwned);
end;

class function TCollections.CreateMultiMap<TKey, TValue>: IMultiMap<TKey, TValue>;
begin
  Result := TMultiMap<TKey, TValue>.Create;
end;

class function TCollections.CreateMultiMap<TKey, TValue>(
  ownerships: TDictionaryOwnerships): IMultiMap<TKey, TValue>;
begin
  Result := TObjectMultiMap<TKey, TValue>.Create(ownerships);
end;

class function TCollections.CreateBidiDictionary<TKey, TValue>: IBidiDictionary<TKey, TValue>;
begin
  Result := TBidiDictionary<TKey, TValue>.Create;
end;

class function TCollections.CreateStack<T>: IStack<T>;
begin
  Result := TStack<T>.Create;
end;

class function TCollections.CreateStack<T>(ownsObjects: Boolean): IStack<T>;
var
  stack: Generics.Collections.TObjectStack<T>;
begin
  stack := Generics.Collections.TObjectStack<T>.Create(ownsObjects);
  Result := TStack<T>.Create(stack, otOwned);
end;

class function TCollections.CreateStack<T>(const values: array of T): IStack<T>;
begin
  Result := TStack<T>.Create(values);
end;

class function TCollections.CreateStack<T>(
  const values: IEnumerable<T>): IStack<T>;
begin
  Result := TStack<T>.Create(values);
end;

class function TCollections.CreateQueue<T>: IQueue<T>;
begin
  Result := TQueue<T>.Create;
end;

class function TCollections.CreateQueue<T>(ownsObjects: Boolean): IQueue<T>;
var
  queue: Generics.Collections.TObjectQueue<T>;
begin
  queue := Generics.Collections.TObjectQueue<T>.Create(ownsObjects);
  Result := TQueue<T>.Create(queue, otOwned);
end;

class function TCollections.CreateQueue<T>(const values: array of T): IQueue<T>;
begin
  Result := TQueue<T>.Create(values);
end;

class function TCollections.CreateQueue<T>(
  const values: IEnumerable<T>): IQueue<T>;
begin
  Result := TQueue<T>.Create(values);
end;

class function TCollections.CreateSet<T>: ISet<T>;
begin
  Result := THashSet<T>.Create;
end;

class function TCollections.CreateSet<T>(
  const comparer: IEqualityComparer<T>): ISet<T>;
begin
  Result := THashSet<T>.Create(comparer);
end;

class function TCollections.CreateSet<T>(const values: array of T): ISet<T>;
begin
  Result := THashSet<T>.Create(values);
end;

class function TCollections.CreateSet<T>(const values: IEnumerable<T>): ISet<T>;
begin
  Result := THashSet<T>.Create(values);
end;

{$ENDREGION}


{$REGION 'TEnumerable'}

class function TEnumerable.Empty<T>: IEnumerable<T>;
begin
  Result := TEmptyEnumerable<T>.Create;
end;

class function TEnumerable.Max<T>(const source: IEnumerable<T>;
  const selector: TFunc<T, Integer>): Integer;
begin
  Result := Select<T, Integer>(source, selector).Max;
end;

class function TEnumerable.Min<T>(const source: IEnumerable<T>;
  const selector: TFunc<T, Integer>): Integer;
begin
  Result := Select<T, Integer>(source, selector).Min;
end;

class function TEnumerable.Query<T>(const source: TArray<T>): IEnumerable<T>;
begin
  Result := TArrayIterator<T>.Create(source);
end;

class function TEnumerable.Query<T>(
  const source: TEnumerable<T>): IEnumerable<T>;
begin
  Result := TEnumerableAdapter<T>.Create(source);
end;

class function TEnumerable.Range(start, count: Integer): IEnumerable<Integer>;
begin
  Result := TRangeIterator<Integer>.Create(start, count);
end;

class function TEnumerable.Repeated<T>(const element: T;
  count: Integer): IEnumerable<T>;
begin
  Result := TRepeatIterator<T>.Create(element, count);
end;

class function TEnumerable.Select<T, TResult>(const source: IEnumerable<T>;
  const selector: TFunc<T, TResult>): IEnumerable<TResult>;
begin
  Result := TSelectIterator<T, TResult>.Create(source, selector);
end;

{$ENDREGION}


{$REGION 'TStringComparer'}

constructor TStringComparer.Create(localeOptions: TLocaleOptions;
  ignoreCase: Boolean);
begin
  inherited Create;
  fLocaleOptions := localeOptions;
  fIgnoreCase := ignoreCase;
end;

class constructor TStringComparer.Create;
begin
  fOrdinal := TStringComparer.Create(loInvariantLocale, False);
  fOrdinalIgnoreCase := TStringComparer.Create(loInvariantLocale, True);
end;

class destructor TStringComparer.Destroy;
begin
  FreeAndNil(fOrdinal);
  FreeAndNil(fOrdinalIgnoreCase);
end;

function TStringComparer.Compare(const Left, Right: string): Integer;
var
  L, R: string;
begin
  if fIgnoreCase then
  begin
{$IFNDEF DELPHIXE4_UP}
    L := TCharacter.ToUpper(Left);
    R := TCharacter.ToUpper(Right);
{$ELSE}
    L := Char.ToUpper(Left);
    R := Char.ToUpper(Right);
{$ENDIF}
  end else
  begin
    L := Left;
    R := Right;
  end;

  Result := CompareStr(L, R, fLocaleOptions);
end;

function TStringComparer.Equals(const Left, Right: string): Boolean;
var
  L, R: string;
begin
  if fIgnoreCase then
  begin
{$IFNDEF DELPHIXE4_UP}
    L := TCharacter.ToUpper(Left);
    R := TCharacter.ToUpper(Right);
{$ELSE}
    L := Char.ToUpper(Left);
    R := Char.ToUpper(Right);
{$ENDIF}
  end else
  begin
    L := Left;
    R := Right;
  end;

  Result := SameStr(L, R, fLocaleOptions);
end;

function TStringComparer.GetHashCode(const Value: string): Integer;
var
  s: string;
begin
  if fIgnoreCase then
{$IFNDEF DELPHIXE4_UP}
    S := TCharacter.ToUpper(Value)
{$ELSE}
    S := Char.ToUpper(Value)
{$ENDIF}
  else
    S := Value;

  Result := BobJenkinsHash(PChar(S)^, SizeOf(Char) * Length(S), 0);
end;

class function TStringComparer.Ordinal: TStringComparer;
begin
  Result := fOrdinal;
end;

class function TStringComparer.OrdinalIgnoreCase: TStringComparer;
begin
  Result := fOrdinalIgnoreCase;
end;

{$ENDREGION}


{$REGION 'TInstanceComparer<T>'}

class function TInstanceComparer<T>.Default: IComparer<T>;
begin
  Result := IComparer<T>(GetInstanceComparer);
end;

{$ENDREGION}


{$REGION 'TLinkedListNode<T>'}

constructor TLinkedListNode<T>.Create(const value: T);
begin
  inherited Create;
  fItem := value;
end;

function TLinkedListNode<T>.GetList: ILinkedList<T>;
begin
  Result := TLinkedList<T>(fList);
end;

function TLinkedListNode<T>.GetNext: TLinkedListNode<T>;
begin
  if Assigned(fNext) and (fNext <> TLinkedList<T>(fList).fHead) then
    Result := fNext
  else
    Result := nil;
end;

function TLinkedListNode<T>.GetPrevious: TLinkedListNode<T>;
begin
  if Assigned(fPrev) and (Self <> TLinkedList<T>(fList).fHead) then
    Result := fPrev
  else
    Result := nil;
end;

{$ENDREGION}


{$REGION 'TKeyValuePair<TKey, TValue>'}

//constructor TKeyValuePair<TKey, TValue>.Create(const key: TKey;
//  const value: TValue);
//begin
//  fKey := key;
//  fValue := value;
//end;

{$ENDREGION}


{$REGION 'TCollectionHelper'}

function TCollectionHelper.AsList: IList<TCollectionItem>;
begin
  Result := TCollectionList<TCollectionItem>.Create(Self);
end;

function TCollectionHelper.AsList<T>: IList<T>;
begin
  Result := TCollectionList<T>.Create(Self);
end;

{$ENDREGION}


end.
