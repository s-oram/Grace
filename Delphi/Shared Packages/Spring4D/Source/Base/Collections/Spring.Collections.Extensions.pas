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

unit Spring.Collections.Extensions;

interface

uses
  Generics.Collections,
  Generics.Defaults,
  SysUtils,
  Types,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Lists;

type
  TEmptyEnumerable<T> = class(TEnumerableBase<T>);

  TArrayIterator<T> = class(TIterator<T>, IReadOnlyList<T>)
  private
    fValues: TArray<T>;
    fIndex: Integer;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetItem(index: Integer): T;
  {$ENDREGION}
  public
    constructor Create(const values: array of T); overload;
    constructor Create(const values: TArray<T>); overload;
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;

    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;

    function ToArray: TArray<T>; override;
  end;

  ///	<summary>
  ///	  The adapter implementation for
  ///	  <see cref="Spring.Collections|IEnumerator&lt;T&gt;" />.
  ///	</summary>
  TEnumeratorAdapter<T> = class(TEnumeratorBase<T>)
  private
    type
      TGenericEnumerable = Generics.Collections.TEnumerable<T>;
      TGenericEnumerator = Generics.Collections.TEnumerator<T>;
  private
    fSource: TGenericEnumerable;
    fEnumerator: TGenericEnumerator;
  protected
    function GetCurrent: T; override;
  public
    constructor Create(const source: TGenericEnumerable);
    destructor Destroy; override;
    function MoveNext: Boolean; override;
    property Current: T read GetCurrent;
  end;

  ///	<summary>
  ///	  The adapter implementation for
  ///	  <see cref="Spring.Collections|IEnumerable&lt;T&gt;" />.
  ///	</summary>
  TEnumerableAdapter<T> = class(TEnumerableBase<T>)
  private
    type
      TGenericEnumerable = Generics.Collections.TEnumerable<T>;
  private
    fSource: TGenericEnumerable;
  public
    constructor Create(const source: TGenericEnumerable);
    function GetEnumerator: IEnumerator<T>; override;
  end;

  TWhereIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fPredicate: TPredicate<T>;
    fEnumerator: IEnumerator<T>;
  public
    constructor Create(const source: IEnumerable<T>;
      const predicate: TPredicate<T>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TWhereIndexIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fPredicate: TFunc<T, Integer, Boolean>;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
  public
    constructor Create(const source: IEnumerable<T>;
      const predicate: TFunc<T, Integer, Boolean>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TSkipIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fCount: Integer;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
  public
    constructor Create(const source: IEnumerable<T>; count: Integer);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TSkipWhileIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fPredicate: TPredicate<T>;
    fEnumerator: IEnumerator<T>;
    fYielding: Boolean;
  public
    constructor Create(const source: IEnumerable<T>; const predicate: TPredicate<T>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TSkipWhileIndexIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fPredicate: TFunc<T, Integer, Boolean>;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
    fYielding: Boolean;
  public
    constructor Create(const source: IEnumerable<T>; const predicate: TFunc<T, Integer, Boolean>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TTakeIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fCount: Integer;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
  public
    constructor Create(const source: IEnumerable<T>; count: Integer);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TTakeWhileIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fPredicate: TPredicate<T>;
    fEnumerator: IEnumerator<T>;
    fStopped: Boolean;
  public
    constructor Create(const source: IEnumerable<T>; const predicate: TPredicate<T>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TTakeWhileIndexIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fPredicate: TFunc<T, Integer, Boolean>;
    fEnumerator: IEnumerator<T>;
    fIndex: Integer;
    fStopped: Boolean;
  public
    constructor Create(const source: IEnumerable<T>; const predicate: TFunc<T, Integer, Boolean>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TConcatIterator<T> = class(TIterator<T>)
  private
    fFirst: IEnumerable<T>;
    fSecond: IEnumerable<T>;
    fEnumerator: IEnumerator<T>;
    fFlag: Boolean;
  public
    constructor Create(const first, second: IEnumerable<T>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TReversedIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fBuffer: TArray<T>;
    fIndex: Integer;
  public
    constructor Create(const source: IEnumerable<T>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TDistinctIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fComparer: IEqualityComparer<T>;
    fSet: ISet<T>;
    fEnumerator: IEnumerator<T>;
  public
    constructor Create(const source: IEnumerable<T>; const comparer: IEqualityComparer<T>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TRangeIterator<T{: Integer}> = class(TIterator<T>)
  private
    fStart: Integer;
    fCount: Integer;
    fIndex: Integer;
  protected
    function GetCount: Integer; override;
  public
    constructor Create(start, count: Integer);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TExceptIterator<T> = class(TIterator<T>)
  private
    fFirst: IEnumerable<T>;
    fSecond: IEnumerable<T>;
    fComparer: IEqualityComparer<T>;
    fSet: ISet<T>;
    fEnumerator: IEnumerator<T>;
  public
    constructor Create(const first, second: IEnumerable<T>); overload;
    constructor Create(const first, second: IEnumerable<T>; const comparer: IEqualityComparer<T>); overload;
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TIntersectIterator<T> = class(TIterator<T>)
  private
    fFirst: IEnumerable<T>;
    fSecond: IEnumerable<T>;
    fComparer: IEqualityComparer<T>;
    fSet: ISet<T>;
    fEnumerator: IEnumerator<T>;
  public
    constructor Create(const first, second: IEnumerable<T>); overload;
    constructor Create(const first, second: IEnumerable<T>; const comparer: IEqualityComparer<T>); overload;
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TUnionIterator<T> = class(TIterator<T>)
  private
    fFirst: IEnumerable<T>;
    fSecond: IEnumerable<T>;
    fComparer: IEqualityComparer<T>;
    fSet: ISet<T>;
    fEnumerator: IEnumerator<T>;
    fFlag: Boolean;
  public
    constructor Create(const first, second: IEnumerable<T>); overload;
    constructor Create(const first, second: IEnumerable<T>; const comparer: IEqualityComparer<T>); overload;
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TSelectIterator<TSource, TResult> = class(TIterator<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fSelector: TFunc<TSource, TResult>;
    fEnumerator: IEnumerator<TSource>;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const selector: TFunc<TSource, TResult>);
    function Clone: TIterator<TResult>; override;
    function MoveNext: Boolean; override;
  end;

  TSelectIndexIterator<TSource, TResult> = class(TIterator<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fSelector: TFunc<TSource, Integer, TResult>;
    fEnumerator: IEnumerator<TSource>;
    fIndex: Integer;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const selector: TFunc<TSource, Integer, TResult>);
    function Clone: TIterator<TResult>; override;
    function MoveNext: Boolean; override;
  end;

  TGroupedEnumerable<TSource, TKey, TElement> = class(TEnumerableBase<IGrouping<TKey, TElement>>)
  private
    type
      TEnumerator = class(TEnumeratorBase<IGrouping<TKey, TElement>>)
      private
        fSource: IEnumerable<TSource>;
        fKeySelector: TFunc<TSource, TKey>;
        fElementSelector: TFunc<TSource, TElement>;
        fComparer: IEqualityComparer<TKey>;
        fLookup: ILookup<TKey, TElement>;
        fEnumerator: IEnumerator<IGrouping<TKey, TElement>>;
      protected
        function GetCurrent: IGrouping<TKey, TElement>; override;
      public
        constructor Create(const source: IEnumerable<TSource>;
          const keySelector: TFunc<TSource, TKey>;
          const elementSelector: TFunc<TSource, TElement>;
          const comparer: IEqualityComparer<TKey>);
        function MoveNext: Boolean; override;
      end;
  private
    fSource: IEnumerable<TSource>;
    fKeySelector: TFunc<TSource, TKey>;
    fElementSelector: TFunc<TSource, TElement>;
    fComparer: IEqualityComparer<TKey>;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>); overload;
    constructor Create(const source: IEnumerable<TSource>;
      const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>;
      const comparer: IEqualityComparer<TKey>); overload;
    function GetEnumerator: IEnumerator<IGrouping<TKey, TElement>>; override;
  end;

  TGroupedEnumerable<TSource, TKey, TElement, TResult> = class(TEnumerableBase<TResult>)
  private
    type
      TEnumerator = class(TEnumeratorBase<TResult>)
      private
        fSource: IEnumerator<IGrouping<TKey, TElement>>;
        fResultSelector: TFunc<TKey, Ienumerable<TElement>, TResult>;
      protected
        function GetCurrent: TResult; override;
      public
        constructor Create(const source: IEnumerator<IGrouping<TKey, TElement>>;
          const resultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>);
        function MoveNext: Boolean; override;
      end;
  private
    fSource: IEnumerable<TSource>;
    fKeySelector: TFunc<TSource, TKey>;
    fElementSelector: TFunc<TSource, TElement>;
    fComparer: IEqualityComparer<TKey>;
    fResultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>;
      const resultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>); overload;
    constructor Create(const source: IEnumerable<TSource>;
      const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>;
      const resultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>;
      const comparer: IEqualityComparer<TKey>); overload;
    function GetEnumerator: IEnumerator<TResult>; override;
  end;

  TLookup<TKey, TElement> = class(TEnumerableBase<IGrouping<TKey, TElement>>, ILookup<TKey, TElement>)
  private
    type
      TGrouping = class(TEnumerableBase<TElement>, IGrouping<TKey, TElement>)
      private
        fKey: TKey;
        fElements: IList<TElement>;
        function GetKey: TKey;
        procedure Add(const item: TElement);
      protected
        function GetCount: Integer; override;
      public
        constructor Create(const key: TKey);
        function GetEnumerator: IEnumerator<TElement>; override;
        property Key: TKey read GetKey;
      end;

      TGroupings = class(TObjectList<TGrouping>)
      protected
{$IFDEF SUPPORTS_GENERIC_FOLDING}
        procedure Changed(const item: TObject;
          action: TCollectionChangedAction); override;
{$ELSE}
        procedure Changed(const item: TGrouping;
          action: TCollectionChangedAction); override;
{$ENDIF}
      public
        constructor Create; override;
      end;

      TEnumerator = class(TEnumeratorBase<IGrouping<TKey, TElement>>)
      private
        fSource: IList<TGrouping>;
        fIndex: Integer;
        fLookup: IInterface;
      protected
        function GetCurrent: IGrouping<TKey, TElement>; override;
      public
        constructor Create(const source: TLookup<TKey, TElement>);
        function MoveNext: Boolean; override;
      end;
  private
    fComparer: IEqualityComparer<TKey>;
    fGroupings: IList<TGrouping>;
    fGroupingKeys: TDictionary<TKey, TGrouping>;
    function GetGrouping(const key: TKey; create: Boolean): TGrouping;
    function GetItem(const key: TKey): IEnumerable<TElement>;
  protected
    function GetCount: Integer; override;
  public
    constructor Create; reintroduce; overload;
    constructor Create(const comparer: IEqualityComparer<TKey>); overload;
    class function Create<TSource>(const source: IEnumerable<TSource>;
      const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>): TLookup<TKey, TElement>; overload; static;
    class function Create<TSource>(const source: IEnumerable<TSource>;
      const keySelector: TFunc<TSource, TKey>;
      const elementSelector: TFunc<TSource, TElement>;
      const comparer: IEqualityComparer<TKey>): TLookup<TKey, TElement>; overload; static;
    class function CreateForJoin(const source: IEnumerable<TElement>;
      const keySelector: TFunc<TElement, TKey>;
      const comparer: IEqualityComparer<TKey>): TLookup<TKey, TElement>; static;
    destructor Destroy; override;

    function Contains(const key: TKey): Boolean;
    function GetEnumerator: IEnumerator<IGrouping<TKey, TElement>>; override;
    property Item[const key: TKey]: IEnumerable<TElement> read GetItem; default;
  end;

  TJoinIterator<TOuter, TInner, TKey, TResult> = class(TIterator<TResult>)
  private
    fOuter: IEnumerable<TOuter>;
    fInner: IEnumerable<TInner>;
    fOuterKeySelector: TFunc<TOuter, TKey>;
    fInnerKeySelector: TFunc<TInner, TKey>;
    fResultSelector: TFunc<TOuter, TInner, TResult>;
    fComparer: IEqualityComparer<TKey>;
    fLookup: TLookup<TKey, TInner>;
    fEnumerator: IEnumerator<TOuter>;
    fFlag: Boolean;
    fGrouping: TLookup<TKey, TInner>.TGrouping;
    fIndex: Integer;
  public
    constructor Create(const outer: IEnumerable<TOuter>;
      const inner: IEnumerable<TInner>;
      const outerKeySelector: TFunc<TOuter, TKey>;
      const innerKeySelector: TFunc<TInner, TKey>;
      const resultSelector: TFunc<TOuter, TInner, TResult>); overload;
    constructor Create(const outer: IEnumerable<TOuter>;
      const inner: IEnumerable<TInner>;
      const outerKeySelector: TFunc<TOuter, TKey>;
      const innerKeySelector: TFunc<TInner, TKey>;
      const resultSelector: TFunc<TOuter, TInner, TResult>;
      const comparer: IEqualityComparer<TKey>); overload;
    destructor Destroy; override;
    function Clone: TIterator<TResult>; override;
    function MoveNext: Boolean; override;
  end;

  TGroupJoinIterator<TOuter, TInner, TKey, TResult> = class(TIterator<TResult>)
  private
    fOuter: IEnumerable<TOuter>;
    fInner: IEnumerable<TInner>;
    fOuterKeySelector: TFunc<TOuter, TKey>;
    fInnerKeySelector: TFunc<TInner, TKey>;
    fResultSelector: TFunc<TOuter, IEnumerable<TInner>, TResult>;
    fComparer: IEqualityComparer<TKey>;
    fLookup: TLookup<TKey, TInner>;
    fEnumerator: IEnumerator<TOuter>;
  public
    constructor Create(const outer: IEnumerable<TOuter>;
      const inner: IEnumerable<TInner>;
      const outerKeySelector: TFunc<TOuter, TKey>;
      const innerKeySelector: TFunc<TInner, TKey>;
      const resultSelector: TFunc<TOuter, IEnumerable<TInner>, TResult>); overload;
    constructor Create(const outer: IEnumerable<TOuter>;
      const inner: IEnumerable<TInner>;
      const outerKeySelector: TFunc<TOuter, TKey>;
      const innerKeySelector: TFunc<TInner, TKey>;
      const resultSelector: TFunc<TOuter, IEnumerable<TInner>, TResult>;
      const comparer: IEqualityComparer<TKey>); overload;
    destructor Destroy; override;
    function Clone: TIterator<TResult>; override;
    function MoveNext: Boolean; override;
  end;

  TSelectManyIterator<TSource, TResult> = class(TIterator<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fSelector: TFunc<TSource, IEnumerable<TResult>>;
    fEnumerator: IEnumerator<TSource>;
    fFlag: Boolean;
    fEnumerator2: IEnumerator<TResult>;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const selector: TFunc<TSource, IEnumerable<TResult>>);
    function Clone: TIterator<TResult>; override;
    function MoveNext: Boolean; override;
  end;

  TSelectManyIndexIterator<TSource, TResult> = class(TIterator<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fSelector: TFunc<TSource, Integer, IEnumerable<TResult>>;
    fEnumerator: IEnumerator<TSource>;
    fFlag: Boolean;
    fIndex: Integer;
    fEnumerator2: IEnumerator<TResult>;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const selector: TFunc<TSource, Integer, IEnumerable<TResult>>);
    function Clone: TIterator<TResult>; override;
    function MoveNext: Boolean; override;
  end;

  TSelectManyIterator<TSource, TCollection, TResult> = class(TIterator<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fCollectionSelector: TFunc<TSource, IEnumerable<TCollection>>;
    fResultSelector: TFunc<TSource, TCollection, TResult>;
    fEnumerator: IEnumerator<TSource>;
    fFlag: Boolean;
    fEnumerator2: IEnumerator<TCollection>;
    fCurrent1: TSource;
    fCurrent2: TCollection;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const collectionSelector: TFunc<TSource, IEnumerable<TCollection>>;
      const resultSelector: TFunc<TSource, TCollection, TResult>);
    function Clone: TIterator<TResult>; override;
    function MoveNext: Boolean; override;
  end;

  TSelectManyIndexIterator<TSource, TCollection, TResult> = class(TIterator<TResult>)
  private
    fSource: IEnumerable<TSource>;
    fCollectionSelector: TFunc<TSource, Integer, IEnumerable<TCollection>>;
    fResultSelector: TFunc<TSource, TCollection, TResult>;
    fEnumerator: IEnumerator<TSource>;
    fFlag: Boolean;
    fIndex: Integer;
    fEnumerator2: IEnumerator<TCollection>;
    fCurrent1: TSource;
    fCurrent2: TCollection;
  public
    constructor Create(const source: IEnumerable<TSource>;
      const collectionSelector: TFunc<TSource, Integer, IEnumerable<TCollection>>;
      const resultSelector: TFunc<TSource, TCollection, TResult>);
    function Clone: TIterator<TResult>; override;
    function MoveNext: Boolean; override;
  end;

  IEnumerableSorter<T> = interface
    procedure ComputeKeys(const elements: TArray<T>; count: Integer);
    function CompareKeys(index1, index2: Integer): Integer;
    function Sort(var elements: TArray<T>; count: Integer): TIntegerDynArray;
  end;

  TEnumerableSorter<T> = class(TInterfacedObject, IEnumerableSorter<T>)
  protected
    procedure ComputeKeys(const elements: TArray<T>; count: Integer); virtual; abstract;
    function CompareKeys(index1, index2: Integer): Integer; virtual; abstract;
    function Sort(var elements: TArray<T>; count: Integer): TIntegerDynArray;
  end;

  TEnumerableSorter<TElement, TKey> = class(TEnumerableSorter<TElement>)
  private
    fKeySelector: TFunc<TElement, TKey>;
    fComparer: IComparer<TKey>;
    fDescending: Boolean;
    fNext: IEnumerableSorter<TElement>;
    fKeys: TArray<TKey>;
  protected
    procedure ComputeKeys(const elements: TArray<TElement>; count: Integer); override;
    function CompareKeys(index1, index2: Integer): Integer; override;
  public
    constructor Create(const keySelector: TFunc<TElement, TKey>;
      const comparer: IComparer<TKey>; descending: Boolean;
      const next: IEnumerableSorter<TElement>);
  end;

  TOrderedEnumerable<T> = class(TEnumerableBase<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fBuffer: TArray<T>;
        fMap: TIntegerDynArray;
        fIndex: Integer;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const source: IEnumerable<T>;
          const sorter: IEnumerableSorter<T>);
        function MoveNext: Boolean; override;
      end;
  private
    fSource: IEnumerable<T>;
  protected
    function GetCount: Integer; override;
    function GetEnumerableSorter(
      const next: IEnumerableSorter<T>): IEnumerableSorter<T>; virtual; abstract;
  public
    function GetEnumerator: IEnumerator<T>; override;
  end;

  TOrderedEnumerable<TElement, TKey> = class(TOrderedEnumerable<TElement>)
  private
    fParent: TOrderedEnumerable<TElement>;
    fKeySelector: TFunc<TElement, TKey>;
    fComparer: IComparer<TKey>;
    fDescending: Boolean;
  protected
    function GetEnumerableSorter(
      const next: IEnumerableSorter<TElement>): IEnumerableSorter<TElement>; override;
  public
    constructor Create(const source: IEnumerable<TElement>;
      const keySelector: TFunc<TElement, TKey>); overload;
    constructor Create(const source: IEnumerable<TElement>;
      const keySelector: TFunc<TElement, TKey>; const comparer: IComparer<TKey>;
      descending: Boolean = False); overload;
  end;

  TOrderedIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fComparer: IComparer<T>;
    fValues: TArray<T>;
    fIndex: Integer;
  public
    constructor Create(const source: IEnumerable<T>;
      const comparer: IComparer<T>);// descending: Boolean);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TZipIterator<TFirst, TSecond, TResult> = class(TIterator<TResult>)
  private
    fFirst: IEnumerable<TFirst>;
    fSecond: IEnumerable<TSecond>;
    fResultSelector: TFunc<TFirst, TSecond, TResult>;
    fEnumerator1: IEnumerator<TFirst>;
    fEnumerator2: IEnumerator<TSecond>;
  public
    constructor Create(const first: IEnumerable<TFirst>;
      const second: IEnumerable<TSecond>;
      const resultSelector: TFunc<TFirst, TSecond, TResult>);
    function Clone: TIterator<TResult>; override;
    function MoveNext: Boolean; override;
  end;

  TDefaultIfEmptyIterator<T> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fDefaultValue: T;
    fEnumerator: IEnumerator<T>;
    fFoundAny: Boolean;
  public
    constructor Create(const source: IEnumerable<T>; const defaultValue: T);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TExtremaByIterator<T, TKey> = class(TIterator<T>)
  private
    fSource: IEnumerable<T>;
    fKeySelector: TFunc<T, TKey>;
    fCompare: TFunc<TKey, TKey, Integer>;
    fResult: IList<T>;
    fEnumerator: IEnumerator<T>;
  public
    constructor Create(const source: IEnumerable<T>;
      const keySelector: TFunc<T, TKey>;
      const compare: TFunc<TKey, TKey, Integer>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TCastIterator<T, TResult> = class(TIterator<TResult>)
  private
    fSource: IEnumerable<T>;
    fEnumerator: IEnumerator<T>;
  public
    constructor Create(const source: IEnumerable<T>);
    function Clone: TIterator<TResult>; override;
    function MoveNext: Boolean; override;
  end;

  TOfTypeIterator<T, TResult> = class(TIterator<TResult>)
  private
    fSource: IEnumerable<T>;
    fEnumerator: IEnumerator<T>;
  public
    constructor Create(const source: IEnumerable<T>);
    function Clone: TIterator<TResult>; override;
    function MoveNext: Boolean; override;
  end;

  TRepeatIterator<T> = class(TIterator<T>)
  public
    fElement: T;
    fCount: Integer;
    fIndex: Integer;
  protected
    function GetCount: Integer; override;
  public
    constructor Create(const element: T; count: Integer);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  TAnonymousIterator<T> = class(TIterator<T>)
  private
    fCount: TFunc<Integer>;
    fItems: TFunc<Integer, T>;
    fIndex: Integer;
  protected
    function GetCount: Integer; override;
  public
    constructor Create(const count: TFunc<Integer>; const items: TFunc<Integer, T>);
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

implementation

uses
  Spring.Collections.Sets,
  Spring.ResourceStrings;


{$REGION 'TArrayIterator<T>'}

constructor TArrayIterator<T>.Create(const values: array of T);
var
  i: Integer;
begin
  inherited Create;
  SetLength(fValues, Length(values));
  for i := Low(values) to High(values) do
    fValues[i] := values[i];
end;

constructor TArrayIterator<T>.Create(const values: TArray<T>);
begin
  inherited Create;
  fValues := values;
end;

function TArrayIterator<T>.GetCount: Integer;
begin
  Result := Length(fValues);
end;

function TArrayIterator<T>.GetItem(index: Integer): T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Count), 'index');
{$ENDIF}

  Result := fValues[index];
end;

function TArrayIterator<T>.IndexOf(const item: T): Integer;
begin
  Result := IndexOf(item, 0, Count);
end;

function TArrayIterator<T>.IndexOf(const item: T; index: Integer): Integer;
begin
  Result := IndexOf(item, index, Count - index);
end;

function TArrayIterator<T>.IndexOf(const item: T; index,
  count: Integer): Integer;
{$IFDEF DELPHI2010}
var
  comparer: IEqualityComparer<T>;
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Length(fValues)), 'index');
  Guard.CheckRange((count >= 0) and (count <= Length(fValues) - index), 'count');
{$ENDIF}

  comparer := EqualityComparer;
  for i := index to index + count - 1 do
    if comparer.Equals(fValues[i], item) then
      Exit(i);
  Result := -1;
{$ELSE}
begin
  Result := TArray.IndexOf<T>(fValues, item, index, count, EqualityComparer);
{$ENDIF}
end;

function TArrayIterator<T>.Clone: TIterator<T>;
begin
  Result := TArrayIterator<T>.Create(fValues);
end;

function TArrayIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fIndex := -1;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    if fIndex < High(fValues) then
    begin
      Inc(fIndex);
      fCurrent := fValues[fIndex];
      Exit(True);
    end;
    fState := STATE_FINISHED;
  end;
end;

function TArrayIterator<T>.ToArray: TArray<T>;
begin
  Result := fValues;
  SetLength(Result, Length(Result));
end;

{$ENDREGION}


{$REGION 'TEnumeratorAdapter<T>'}

constructor TEnumeratorAdapter<T>.Create(const source: TGenericEnumerable);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create;
  fSource := source;
end;

destructor TEnumeratorAdapter<T>.Destroy;
begin
  fEnumerator.Free;
  inherited Destroy;
end;

function TEnumeratorAdapter<T>.GetCurrent: T;
begin
  Result := fEnumerator.Current;
end;

function TEnumeratorAdapter<T>.MoveNext: Boolean;
begin
  if not Assigned(fEnumerator) then
    fEnumerator := fSource.GetEnumerator;
  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TEnumerableAdapter<T>'}

constructor TEnumerableAdapter<T>.Create(const source: TGenericEnumerable);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create;
  fSource := source;
end;

function TEnumerableAdapter<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorAdapter<T>.Create(fSource);
end;

{$ENDREGION}


{$REGION 'TWhereIterator<T>'}

constructor TWhereIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: TPredicate<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fPredicate := predicate;
end;

function TWhereIterator<T>.Clone: TIterator<T>;
begin
  Result := TWhereIterator<T>.Create(fSource, fPredicate);
end;

function TWhereIterator<T>.MoveNext: Boolean;
var
  current: T;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      if fPredicate(current) then
      begin
        fCurrent := current;
        Exit(True);
      end;
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TWhereIndexIterator<T>'}

constructor TWhereIndexIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: TFunc<T, Integer, Boolean>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fPredicate := predicate;
end;

function TWhereIndexIterator<T>.Clone: TIterator<T>;
begin
  Result := TWhereIndexIterator<T>.Create(fSource, fPredicate);
end;

function TWhereIndexIterator<T>.MoveNext: Boolean;
var
  current: T;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fIndex := -1;
    fEnumerator := fSource.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      Inc(fIndex);
      if fPredicate(current, fIndex) then
      begin
        fCurrent := current;
        Exit(True);
      end;
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TSkipIterator<T>'}

constructor TSkipIterator<T>.Create(const source: IEnumerable<T>;
  count: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fCount := count;
end;

function TSkipIterator<T>.Clone: TIterator<T>;
begin
  Result := TSkipIterator<T>.Create(fSource, fCount);
end;

function TSkipIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fIndex := fCount;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while (fIndex > 0) and fEnumerator.MoveNext do
      Dec(fIndex);
    if fEnumerator.MoveNext then
    begin
      fCurrent := fEnumerator.Current;
      Exit(True);
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TSkipWhileIterator<T>'}

constructor TSkipWhileIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: TPredicate<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fPredicate := predicate;
end;

function TSkipWhileIterator<T>.Clone: TIterator<T>;
begin
  Result := TSkipWhileIterator<T>.Create(fSource, fPredicate);
end;

function TSkipWhileIterator<T>.MoveNext: Boolean;
var
  current: T;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      if not fYielding and not fPredicate(current) then
        fYielding := True;
      if fYielding then
      begin
        fCurrent := current;
        Exit(True);
      end;
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TSkipWhileIndexIterator<T>'}

constructor TSkipWhileIndexIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: TFunc<T, Integer, Boolean>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fPredicate := predicate;
end;

function TSkipWhileIndexIterator<T>.Clone: TIterator<T>;
begin
  Result := TSkipWhileIndexIterator<T>.Create(fSource, fPredicate);
end;

function TSkipWhileIndexIterator<T>.MoveNext: Boolean;
var
  current: T;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fIndex := -1;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      Inc(fIndex);
      if not fYielding and not fPredicate(current, fIndex) then
        fYielding := True;
      if fYielding then
      begin
        fCurrent := current;
        Exit(True);
      end;
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TTakeIterator<T>'}

constructor TTakeIterator<T>.Create(const source: IEnumerable<T>;
  count: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fCount := count;
end;

function TTakeIterator<T>.Clone: TIterator<T>;
begin
  Result := TTakeIterator<T>.Create(fSource, fCount);
end;

function TTakeIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fIndex := 0;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while (fIndex < fCount) and fEnumerator.MoveNext do
    begin
      fCurrent := fEnumerator.Current;
      Inc(fIndex);
      Exit(True);
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TTakeWhileIterator<T>'}

constructor TTakeWhileIterator<T>.Create(const source: IEnumerable<T>;
  const predicate: TPredicate<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fPredicate := predicate;
end;

function TTakeWhileIterator<T>.Clone: TIterator<T>;
begin
  Result := TTakeWhileIterator<T>.Create(fSource, fPredicate);
end;

function TTakeWhileIterator<T>.MoveNext: Boolean;
var
  current: T;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while not fStopped and fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      if fPredicate(current) then
      begin
        fCurrent := current;
        Exit(True);
      end
      else
        fStopped := True;
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TTakeWhileIndexIterator<T>'}

constructor TTakeWhileIndexIterator<T>.Create(
  const source: IEnumerable<T>;
  const predicate: TFunc<T, Integer, Boolean>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fPredicate := predicate;
end;

function TTakeWhileIndexIterator<T>.Clone: TIterator<T>;
begin
  Result := TTakeWhileIndexIterator<T>.Create(fSource, fPredicate);
end;

function TTakeWhileIndexIterator<T>.MoveNext: Boolean;
var
  current: T;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fIndex := -1;
    fEnumerator := fSource.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while not fStopped and fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      Inc(fIndex);
      if fPredicate(current, findex) then
      begin
        fCurrent := current;
        Exit(True);
      end
      else
        fStopped := True;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TConcatIterator<T>'}

constructor TConcatIterator<T>.Create(const first, second: IEnumerable<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(first), 'first');
  Guard.CheckNotNull(Assigned(second), 'second');
{$ENDIF}

  inherited Create;
  fFirst := first;
  fSecond := second;
end;

function TConcatIterator<T>.Clone: TIterator<T>;
begin
  Result := TConcatIterator<T>.Create(fFirst, fSecond);
end;

function TConcatIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fFirst.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    repeat
      if fEnumerator.MoveNext then
      begin
        fCurrent := fEnumerator.Current;
        Exit(True);
      end
      else
      begin
        if not fFlag then
        begin
          fEnumerator := fSecond.GetEnumerator;
          fFlag := True;
        end
        else
        begin
          fState := STATE_FINISHED;
          fEnumerator := nil;
          Break;
        end;
      end;
    until Result;
  end;
end;

{$ENDREGION}


{$REGION 'TReversedIterator<T>'}

constructor TReversedIterator<T>.Create(const source: IEnumerable<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
end;

function TReversedIterator<T>.Clone: TIterator<T>;
begin
  Result := TReversedIterator<T>.Create(fSource);
end;

function TReversedIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fBuffer := fSource.ToArray;
    fIndex := Length(fBuffer);
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    if fIndex > 0 then
    begin
      Dec(fIndex);
      fCurrent := fBuffer[fIndex];
      Exit(True);
    end;
    fState := STATE_FINISHED;
    fBuffer := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TDistinctIterator<T>'}

constructor TDistinctIterator<T>.Create(const source: IEnumerable<T>;
  const comparer: IEqualityComparer<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create(source.Comparer);
  fSource := source;
  fComparer := comparer;
end;

function TDistinctIterator<T>.Clone: TIterator<T>;
begin
  Result := TDistinctIterator<T>.Create(fSource, fComparer);
end;

function TDistinctIterator<T>.MoveNext: Boolean;
var
  current: T;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fSet := THashSet<T>.Create(fComparer);
    fEnumerator := fSource.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      if fSet.Add(current) then
      begin
        fCurrent := current;
        Exit(True);
      end;
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
    fSet := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TRangeIterator<T>'}

constructor TRangeIterator<T>.Create(start, count: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(count >= 0, 'count');
  Guard.CheckRange(Int64(start) + Int64(count) - 1 <= Int64(MaxInt), 'count');
{$ENDIF}

  inherited Create;
  fStart := start;
  fCount := count;
end;

function TRangeIterator<T>.Clone: TIterator<T>;
begin
  Result := TRangeIterator<T>.Create(fStart, fCount);
end;

function TRangeIterator<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TRangeIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fIndex := 0;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    if fIndex < fCount then
    begin
      PInteger(@fCurrent)^ := fStart + fIndex;
      Inc(fIndex);
      Exit(True);
    end;
    fState := STATE_FINISHED;
  end;
end;

{$ENDREGION}


{$REGION 'TExceptIterator<T>'}

constructor TExceptIterator<T>.Create(const first, second: IEnumerable<T>);
begin
  Create(first, second, nil);
end;

constructor TExceptIterator<T>.Create(const first, second: IEnumerable<T>;
  const comparer: IEqualityComparer<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(first), 'first');
  Guard.CheckNotNull(Assigned(second), 'second');
{$ENDIF}

  inherited Create;
  fFirst := first;
  fSecond := second;
  fComparer := comparer;
end;

function TExceptIterator<T>.Clone: TIterator<T>;
begin
  Result := TExceptIterator<T>.Create(fFirst, fSecond, fComparer);
end;

function TExceptIterator<T>.MoveNext: Boolean;
var
  current: T;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fSet := THashSet<T>.Create(fComparer);
    fSet.AddRange(fSecond);
    fEnumerator := fFirst.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      if fSet.Add(current) then
      begin
        fCurrent := current;
        Exit(True);
      end;
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
    fSet := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TIntersectIterator<T>'}

constructor TIntersectIterator<T>.Create(const first, second: IEnumerable<T>);
begin
  Create(first, second, nil);
end;

constructor TIntersectIterator<T>.Create(const first, second: IEnumerable<T>;
  const comparer: IEqualityComparer<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(first), 'first');
  Guard.CheckNotNull(Assigned(second), 'second');
{$ENDIF}

  inherited Create;
  fFirst := first;
  fSecond := second;
  fComparer := comparer;
end;

function TIntersectIterator<T>.Clone: TIterator<T>;
begin
  Result := TIntersectIterator<T>.Create(fFirst, fSecond, fComparer);
end;

function TIntersectIterator<T>.MoveNext: Boolean;
var
  current: T;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fSet := THashSet<T>.Create(fComparer);
    fSet.AddRange(fSecond);
    fEnumerator := fFirst.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      if fSet.Remove(current) then
      begin
        fCurrent := current;
        Exit(True);
      end;
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
    fSet := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TUnionIterator<T>'}

constructor TUnionIterator<T>.Create(const first, second: IEnumerable<T>);
begin
  Create(first, second, nil);
end;

constructor TUnionIterator<T>.Create(const first, second: IEnumerable<T>;
  const comparer: IEqualityComparer<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(first), 'first');
  Guard.CheckNotNull(Assigned(second), 'second');
{$ENDIF}

  inherited Create;
  fFirst := first;
  fSecond := second;
  fComparer := comparer;
end;

function TUnionIterator<T>.Clone: TIterator<T>;
begin
  Result := TUnionIterator<T>.Create(fFirst, fSecond, fComparer);
end;

function TUnionIterator<T>.MoveNext: Boolean;
var
  current: T;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fSet := THashSet<T>.Create(fComparer);
    fEnumerator := fFirst.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    repeat
      if fEnumerator.MoveNext then
      begin
        current := fEnumerator.Current;
        if fSet.Add(current) then
        begin
          fCurrent := current;
          Result := True;
        end;
      end
      else
      begin
        if not fFlag then
        begin
          fEnumerator := fSecond.GetEnumerator;
          fFlag := True;
        end
        else
        begin
          fState := STATE_FINISHED;
          fEnumerator := nil;
          fSet := nil;
          Break;
        end;
      end;
    until Result;
  end;
end;

{$ENDREGION}


{$REGION 'TSelectIterator<TSource, TResult>'}

constructor TSelectIterator<TSource, TResult>.Create(
  const source: IEnumerable<TSource>; const selector: TFunc<TSource, TResult>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');
{$ENDIF}

  inherited Create;
  fSource := source;
  fSelector := selector;
end;

function TSelectIterator<TSource, TResult>.Clone: TIterator<TResult>;
begin
  Result := TSelectIterator<TSource, TResult>.Create(fSource, fSelector)
end;

function TSelectIterator<TSource, TResult>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    if fEnumerator.MoveNext then
    begin
      fCurrent := fSelector(fEnumerator.Current);
      Exit(True);
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TSelectIndexIterator<TSource, TResult>'}

constructor TSelectIndexIterator<TSource, TResult>.Create(
  const source: IEnumerable<TSource>;
  const selector: TFunc<TSource, Integer, TResult>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');
{$ENDIF}

  inherited Create;
  fSource := source;
  fSelector := selector;
end;

function TSelectIndexIterator<TSource, TResult>.Clone: TIterator<TResult>;
begin
  Result := TSelectIndexIterator<TSource, TResult>.Create(fSource, fSelector);
end;

function TSelectIndexIterator<TSource, TResult>.MoveNext: Boolean;
var
  current: TSource;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fIndex := -1;
    fEnumerator := fSource.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    if fEnumerator.MoveNext then
    begin
      current := fEnumerator.Current;
      Inc(fIndex);
      fCurrent := fSelector(current, fIndex);
      Exit(True);
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TGroupedEnumerable<TSource, TKey, TElement>'}

constructor TGroupedEnumerable<TSource, TKey, TElement>.Create(
  const source: IEnumerable<TSource>; const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>);
begin
  Create(source, keySelector, elementSelector, TEqualityComparer<TKey>.Default);
end;

constructor TGroupedEnumerable<TSource, TKey, TElement>.Create(
  const source: IEnumerable<TSource>; const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>;
  const comparer: IEqualityComparer<TKey>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');
  Guard.CheckNotNull(Assigned(elementSelector), 'elementSelector');
{$ENDIF}

  inherited Create;
  fSource := source;
  fKeySelector := keySelector;
  fElementSelector := elementSelector;
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := TEqualityComparer<TKey>.Default;
end;

function TGroupedEnumerable<TSource, TKey, TElement>.GetEnumerator: IEnumerator<IGrouping<TKey, TElement>>;
begin
  Result := TEnumerator.Create(fSource, fKeySelector, fElementSelector, fComparer);
end;

{$ENDREGION}


{$REGION 'TGroupedEnumerable<TSource, TKey, TElement>.TEnumerator'}

constructor TGroupedEnumerable<TSource, TKey, TElement>.TEnumerator.Create(
  const source: IEnumerable<TSource>; const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>;
  const comparer: IEqualityComparer<TKey>);
begin
  inherited Create;
  fSource := source;
  fKeySelector := keySelector;
  fElementSelector := elementSelector;
  fComparer := comparer;
end;

function TGroupedEnumerable<TSource, TKey, TElement>.TEnumerator.GetCurrent: IGrouping<TKey, TElement>;
begin
  Result := fEnumerator.Current;
end;

function TGroupedEnumerable<TSource, TKey, TElement>.TEnumerator.MoveNext: Boolean;
begin
  if not Assigned(fEnumerator) then
  begin
    fLookup := TLookup<TKey, TElement>.Create<TSource>(
      fSource, fKeySelector, fElementSelector, fComparer);
    fEnumerator := fLookup.GetEnumerator;
  end;

  Result := fEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TGroupedEnumerable<TSource, TKey, TElement, TResult>'}

constructor TGroupedEnumerable<TSource, TKey, TElement, TResult>.Create(
  const source: IEnumerable<TSource>; const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>;
  const resultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>);
begin
  Create(source, keySelector, elementSelector, resultSelector,
    TEqualityComparer<TKey>.Default);
end;

constructor TGroupedEnumerable<TSource, TKey, TElement, TResult>.Create(
  const source: IEnumerable<TSource>; const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>;
  const resultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>;
  const comparer: IEqualityComparer<TKey>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');
  Guard.CheckNotNull(Assigned(elementSelector), 'elementSelector');
  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');
{$ENDIF}

  inherited Create;
  fSource := source;
  fKeySelector := keySelector;
  fElementSelector := elementSelector;
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := TEqualityComparer<TKey>.Default;
  fResultSelector := resultSelector;
end;

function TGroupedEnumerable<TSource, TKey, TElement, TResult>.GetEnumerator: IEnumerator<TResult>;
begin
  // TODO: deferred execution ?
  Result := TEnumerator.Create(TLookup<TKey, TElement>.Create<TSource>(
    fSource, fKeySelector, fElementSelector, fComparer).GetEnumerator,
    fResultSelector);
end;

{$ENDREGION}


{$REGION 'TGroupedEnumerable<TSource, TKey, TElement, TResult>.TEnumerator'}

constructor TGroupedEnumerable<TSource, TKey, TElement, TResult>.TEnumerator.Create(
  const source: IEnumerator<IGrouping<TKey, TElement>>;
  const resultSelector: TFunc<TKey, IEnumerable<TElement>, TResult>);
begin
  inherited Create;
  fSource := source;
  fResultSelector := resultSelector;
end;

function TGroupedEnumerable<TSource, TKey, TElement, TResult>.TEnumerator.GetCurrent: TResult;
var
  g: IGrouping<TKey, TElement>;
begin
  g := fSource.Current;
  Result := fResultSelector(g.Key, g);
end;

function TGroupedEnumerable<TSource, TKey, TElement, TResult>.TEnumerator.MoveNext: Boolean;
begin
  Result := fSource.MoveNext;
end;

{$ENDREGION}


{$REGION 'TLookup<TKey, TElement>'}

constructor TLookup<TKey, TElement>.Create;
begin
  Create(TEqualityComparer<TKey>.Default);
end;

constructor TLookup<TKey, TElement>.Create(const comparer: IEqualityComparer<TKey>);
begin
  inherited Create;
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := TEqualityComparer<TKey>.Default;
  fGroupings := TGroupings.Create as IList<TGrouping>;
  fGroupingKeys := TDictionary<TKey, TGrouping>.Create(fComparer);
end;

class function TLookup<TKey, TElement>.Create<TSource>(
  const source: IEnumerable<TSource>; const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>): TLookup<TKey, TElement>;
begin
  Result := Create<TSource>(source, keySelector, elementSelector, TEqualityComparer<TKey>.Default);
end;

class function TLookup<TKey, TElement>.Create<TSource>(
  const source: IEnumerable<TSource>; const keySelector: TFunc<TSource, TKey>;
  const elementSelector: TFunc<TSource, TElement>;
  const comparer: IEqualityComparer<TKey>): TLookup<TKey, TElement>;
var
  item: TSource;
begin
  Result := TLookup<TKey, TElement>.Create(comparer);
  try
    for item in source do
      Result.GetGrouping(keySelector(item), True).Add(elementSelector(item));
  except
    FreeAndNil(Result);
    raise;
  end;
end;

class function TLookup<TKey, TElement>.CreateForJoin(
  const source: IEnumerable<TElement>; const keySelector: TFunc<TElement, TKey>;
  const comparer: IEqualityComparer<TKey>): TLookup<TKey, TElement>;
var
  element: TElement;
  key: TKey;
begin
  Result := TLookup<TKey, TElement>.Create(comparer);
  try
    for element in source do
    begin
      key := keySelector(element);
      Result.GetGrouping(key, True).Add(element);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

destructor TLookup<TKey, TElement>.Destroy;
begin
  fGroupingKeys.Free;
  inherited;
end;

function TLookup<TKey, TElement>.Contains(const key: TKey): Boolean;
begin
  Result := Assigned(GetGrouping(key, False));
end;

function TLookup<TKey, TElement>.GetCount: Integer;
begin
  Result := fGroupings.Count;
end;

function TLookup<TKey, TElement>.GetEnumerator: IEnumerator<IGrouping<TKey, TElement>>;
begin
  Result := TEnumerator.Create(Self);
end;

function TLookup<TKey, TElement>.GetGrouping(const key: TKey;
  create: Boolean): TGrouping;
var
  grouping: TGrouping;
begin
  if not fGroupingKeys.TryGetValue(key, Result) and create then
  begin
    grouping := TGrouping.Create(key);
    fGroupings.Add(grouping);
    fGroupingKeys.Add(key, grouping);
    Result := grouping;
  end;
end;

function TLookup<TKey, TElement>.GetItem(
  const key: TKey): IEnumerable<TElement>;
var
  index: Integer;
begin
  Result := GetGrouping(key, False);
  if not Assigned(Result) then
    Result := TEnumerableBase<TElement>.Create;
end;

{$ENDREGION}


{$REGION 'TLookup<TKey, TElement>.TGrouping'}

constructor TLookup<TKey, TElement>.TGrouping.Create(const key: TKey);
begin
  inherited Create;
  fKey := key;
  fElements := TList<TElement>.Create;
end;

procedure TLookup<TKey, TElement>.TGrouping.Add(const item: TElement);
begin
  fElements.Add(item);
end;

function TLookup<TKey, TElement>.TGrouping.GetCount: Integer;
begin
  Result := fElements.Count;
end;

function TLookup<TKey, TElement>.TGrouping.GetEnumerator: IEnumerator<TElement>;
begin
  Result := fElements.GetEnumerator;
end;

function TLookup<TKey, TElement>.TGrouping.GetKey: TKey;
begin
  Result := fKey;
end;

{$ENDREGION}


{$REGION 'TLookup<TKey, TElement>.TGroupings'}

constructor TLookup<TKey, TElement>.TGroupings.Create;
begin
  inherited Create(False);
end;

{$IFDEF SUPPORTS_GENERIC_FOLDING}
procedure TLookup<TKey, TElement>.TGroupings.Changed(const item: TObject;
  action: TCollectionChangedAction);
{$ELSE}
procedure TLookup<TKey, TElement>.TGroupings.Changed(const item: TGrouping;
  action: TCollectionChangedAction);
{$ENDIF}
begin
  inherited;
  case action of
    caAdded: TGrouping(item)._AddRef;
    caRemoved: TGrouping(item)._Release;
  end;
end;

{$ENDREGION}


{$REGION 'TLookup<TKey, TElement>.TEnumerator'}

constructor TLookup<TKey, TElement>.TEnumerator.Create(
  const source: TLookup<TKey, TElement>);
begin
  inherited Create;
  fSource := source.fGroupings;
  fIndex := -1;
  fLookup := source;
end;

function TLookup<TKey, TElement>.TEnumerator.GetCurrent: IGrouping<TKey, TElement>;
begin
  Result := fSource[fIndex];
end;

function TLookup<TKey, TElement>.TEnumerator.MoveNext: Boolean;
begin
  Result := fIndex < fSource.Count - 1;
  if Result then
    Inc(fIndex);
end;

{$ENDREGION}


{$REGION 'TJoinIterator<TOuter, TInner, TKey, TResult>'}

constructor TJoinIterator<TOuter, TInner, TKey, TResult>.Create(
  const outer: IEnumerable<TOuter>; const inner: IEnumerable<TInner>;
  const outerKeySelector: TFunc<TOuter, TKey>;
  const innerKeySelector: TFunc<TInner, TKey>;
  const resultSelector: TFunc<TOuter, TInner, TResult>);
begin
  Create(outer, inner, outerKeySelector, innerKeySelector, resultSelector,
    TEqualityComparer<TKey>.Default);
end;

constructor TJoinIterator<TOuter, TInner, TKey, TResult>.Create(
  const outer: IEnumerable<TOuter>; const inner: IEnumerable<TInner>;
  const outerKeySelector: TFunc<TOuter, TKey>;
  const innerKeySelector: TFunc<TInner, TKey>;
  const resultSelector: TFunc<TOuter, TInner, TResult>;
  const comparer: IEqualityComparer<TKey>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(outer), 'outer');
  Guard.CheckNotNull(Assigned(inner), 'inner');
  Guard.CheckNotNull(Assigned(outerKeySelector), 'outerKeySelector');
  Guard.CheckNotNull(Assigned(innerKeySelector), 'innerKeySelector');
  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');
{$ENDIF}

  inherited Create;
  fOuter := outer;
  fInner := inner;
  fOuterKeySelector := outerKeySelector;
  fInnerKeySelector := innerKeySelector;
  fResultSelector := resultSelector;
  fComparer := comparer;
end;

destructor TJoinIterator<TOuter, TInner, TKey, TResult>.Destroy;
begin
  fLookup.Free;
  inherited;
end;

function TJoinIterator<TOuter, TInner, TKey, TResult>.Clone: TIterator<TResult>;
begin
  Result := TJoinIterator<TOuter, TInner, TKey, TResult>.Create(fOuter, fInner,
    fOuterKeySelector, fInnerKeySelector, fResultSelector, fComparer);
end;

function TJoinIterator<TOuter, TInner, TKey, TResult>.MoveNext: Boolean;
var
  current: TOuter;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fLookup := TLookup<TKey, TInner>.CreateForJoin(fInner, fInnerKeySelector, fComparer);
    fEnumerator := fOuter.GetEnumerator;
    fFlag := True;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while not fFlag or fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      if fFlag then
      begin
        fGrouping := fLookup.GetGrouping(fOuterKeySelector(current), False);
        if not Assigned(fGrouping) then
          Continue;
        fFlag := False;
        fIndex := 0;
      end;

      if fIndex < fGrouping.Count then
      begin
        fCurrent := fResultSelector(current, fGrouping.fElements[fIndex]);
        Inc(fIndex);
        Exit(True);
      end
      else
        fFlag := True;
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
    FreeAndNil(fLookup);
  end;
end;

{$ENDREGION}


{$REGION 'TGroupJoinIterator<TOuter, TInner, TKey, TResult>'}

constructor TGroupJoinIterator<TOuter, TInner, TKey, TResult>.Create(
  const outer: IEnumerable<TOuter>; const inner: IEnumerable<TInner>;
  const outerKeySelector: TFunc<TOuter, TKey>;
  const innerKeySelector: TFunc<TInner, TKey>;
  const resultSelector: TFunc<TOuter, IEnumerable<TInner>, TResult>);
begin
  Create(outer, inner, outerKeySelector, innerKeySelector, resultSelector,
    TEqualityComparer<TKey>.Default);
end;

constructor TGroupJoinIterator<TOuter, TInner, TKey, TResult>.Create(
  const outer: IEnumerable<TOuter>; const inner: IEnumerable<TInner>;
  const outerKeySelector: TFunc<TOuter, TKey>;
  const innerKeySelector: TFunc<TInner, TKey>;
  const resultSelector: TFunc<TOuter, IEnumerable<TInner>, TResult>;
  const comparer: IEqualityComparer<TKey>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(outer), 'outer');
  Guard.CheckNotNull(Assigned(inner), 'inner');
  Guard.CheckNotNull(Assigned(outerKeySelector), 'outerKeySelector');
  Guard.CheckNotNull(Assigned(innerKeySelector), 'innerKeySelector');
  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');
{$ENDIF}

  inherited Create;
  fOuter := outer;
  fInner := inner;
  fOuterKeySelector := outerKeySelector;
  fInnerKeySelector := innerKeySelector;
  fResultSelector := resultSelector;
  fComparer := comparer;
end;

destructor TGroupJoinIterator<TOuter, TInner, TKey, TResult>.Destroy;
begin
  fLookup.Free;
  inherited;
end;

function TGroupJoinIterator<TOuter, TInner, TKey, TResult>.Clone: TIterator<TResult>;
begin
  Result := TGroupJoinIterator<TOuter, TInner, TKey, TResult>.Create(fOuter,
    fInner, fOuterKeySelector, fInnerKeySelector, fResultSelector, fComparer);
end;

function TGroupJoinIterator<TOuter, TInner, TKey, TResult>.MoveNext: Boolean;
var
  current: TOuter;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fLookup := TLookup<TKey, TInner>.CreateForJoin(fInner, fInnerKeySelector, fComparer);
    fEnumerator := fOuter.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    if fEnumerator.MoveNext then
    begin
      current := fEnumerator.Current;
      fCurrent := fResultSelector(current, fLookup[fOuterKeySelector(current)]);
      Exit(True);
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
    FreeAndNil(fLookup);
  end;
end;

{$ENDREGION}


{$REGION 'TSelectManyIterator<TSource, TResult>'}

constructor TSelectManyIterator<TSource, TResult>.Create(
  const source: IEnumerable<TSource>;
  const selector: TFunc<TSource, IEnumerable<TResult>>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');
{$ENDIF}

  inherited Create;
  fSource := source;
  fSelector := selector;
end;

function TSelectManyIterator<TSource, TResult>.Clone: TIterator<TResult>;
begin
  Result := TSelectManyIterator<TSource, TResult>.Create(fSource, fSelector);
end;

function TSelectManyIterator<TSource, TResult>.MoveNext: Boolean;
var
  current: TSource;
  collection: IEnumerable<TResult>;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fFlag := True;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while not fFlag or fEnumerator.MoveNext do
    begin
      if fFlag then
      begin
        current := fEnumerator.Current;
        collection := fSelector(current);
        fEnumerator2 := collection.GetEnumerator;
        fFlag := False;
      end;

      if fEnumerator2.MoveNext then
      begin
        fCurrent := fEnumerator2.Current;
        Exit(True);
      end
      else
        fFlag := True;
    end;
    fState := STATE_FINISHED;
    fEnumerator2 := nil;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TSelectManyIndexIterator<TSource, TResult>'}

constructor TSelectManyIndexIterator<TSource, TResult>.Create(
  const source: IEnumerable<TSource>;
  const selector: TFunc<TSource, Integer, IEnumerable<TResult>>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(selector), 'selector');
{$ENDIF}

  inherited Create;
  fSource := source;
  fSelector := selector;
end;

function TSelectManyIndexIterator<TSource, TResult>.Clone: TIterator<TResult>;
begin
  Result := TSelectManyIndexIterator<TSource, TResult>.Create(fSource, fSelector);
end;

function TSelectManyIndexIterator<TSource, TResult>.MoveNext: Boolean;
var
  current: TSource;
  collection: IEnumerable<TResult>;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fIndex := -1;
    fEnumerator := fSource.GetEnumerator;
    fFlag := True;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while not fFlag or fEnumerator.MoveNext do
    begin
      if fFlag then
      begin
        current := fEnumerator.Current;
        Inc(fIndex);
        collection := fSelector(current, fIndex);
        fEnumerator2 := collection.GetEnumerator;
        fFlag := False;
      end;

      if fEnumerator2.MoveNext then
      begin
        fCurrent := fEnumerator2.Current;
        Exit(True);
      end
      else
        fFlag := True;
    end;
    fState := STATE_FINISHED;
    fEnumerator2 := nil;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TSelectManyIterator<TSource, TCollection, TResult>'}

constructor TSelectManyIterator<TSource, TCollection, TResult>.Create(
  const source: IEnumerable<TSource>;
  const collectionSelector: TFunc<TSource, IEnumerable<TCollection>>;
  const resultSelector: TFunc<TSource, TCollection, TResult>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(collectionSelector), 'collectionSelector');
  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');
{$ENDIF}

  inherited Create;
  fSource := source;
  fCollectionSelector := collectionSelector;
  fResultSelector := resultSelector;
end;

function TSelectManyIterator<TSource, TCollection, TResult>.Clone: TIterator<TResult>;
begin
  Result := TSelectManyIterator<TSource, TCollection, TResult>.Create(
    fSource, fCollectionSelector, fResultSelector);
end;

function TSelectManyIterator<TSource, TCollection, TResult>.MoveNext: Boolean;
var
  collection: IEnumerable<TCollection>;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fFlag := True;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while not fFlag or fEnumerator.MoveNext do
    begin
      if fFlag then
      begin
        fCurrent1 := fEnumerator.Current;
        collection := fCollectionSelector(fCurrent1);
        fEnumerator2 := collection.GetEnumerator;
        fFlag := False;
      end;

      if fEnumerator2.MoveNext then
      begin
        fCurrent2 := fEnumerator2.Current;
        fCurrent := fResultSelector(fCurrent1, fCurrent2);
        Exit(True);
      end
      else
        fFlag := True;
    end;
    fState := STATE_FINISHED;
    fEnumerator2 := nil;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TSelectManyIndexIterator<TSource, TCollection, TResult>'}

constructor TSelectManyIndexIterator<TSource, TCollection, TResult>.Create(
  const source: IEnumerable<TSource>;
  const collectionSelector: TFunc<TSource, Integer, IEnumerable<TCollection>>;
  const resultSelector: TFunc<TSource, TCollection, TResult>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(collectionSelector), 'collectionSelector');
  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');
{$ENDIF}

  inherited Create;
  fSource := source;
  fCollectionSelector := collectionSelector;
  fResultSelector := resultSelector;
end;

function TSelectManyIndexIterator<TSource, TCollection, TResult>.Clone: TIterator<TResult>;
begin
  Result := TSelectManyIndexIterator<TSource, TCollection, TResult>.Create(
    fSource, fCollectionSelector, fResultSelector);
end;

function TSelectManyIndexIterator<TSource, TCollection, TResult>.MoveNext: Boolean;
var
  collection: IEnumerable<TCollection>;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fIndex := -1;
    fEnumerator := fSource.GetEnumerator;
    fFlag := True;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while not fFlag or fEnumerator.MoveNext do
    begin
      if fFlag then
      begin
        fCurrent1 := fEnumerator.Current;
        Inc(fIndex);
        collection := fCollectionSelector(fCurrent1, fIndex);
        fEnumerator2 := collection.GetEnumerator;
        fFlag := False;
      end;

      if fEnumerator2.MoveNext then
      begin
        fCurrent2 := fEnumerator2.Current;
        fCurrent := fResultSelector(fCurrent1, fCurrent2);
        Exit(True);
      end
      else
        fFlag := True;
    end;
    fState := STATE_FINISHED;
    fEnumerator2 := nil;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TEnumerableSorter<T>'}

function TEnumerableSorter<T>.Sort(var elements: TArray<T>;
  count: Integer): TIntegerDynArray;
var
  index: Integer;
  comparer: TComparison<Integer>;
  return: array of Integer;
begin
  ComputeKeys(elements, count);
  SetLength(Result, count);
  for index := 0 to count - 1 do
    Result[index] := index;
  comparer :=
    function(const Left, Right: Integer): Integer
    begin
      Result := CompareKeys(Left, Right);
    end;
  TArray.Sort<Integer>(Result, IComparer<Integer>(PPointer(@comparer)^));
end;

{$ENDREGION}


{$REGION 'TEnumerableSorter<TElement, TKey>'}

constructor TEnumerableSorter<TElement, TKey>.Create(
  const keySelector: TFunc<TElement, TKey>; const comparer: IComparer<TKey>;
  descending: Boolean; const next: IEnumerableSorter<TElement>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');
{$ENDIF}

  inherited Create;
  fKeySelector := keySelector;
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := TComparer<TKey>.Default;
  fDescending := descending;
  fNext := next;
end;

function TEnumerableSorter<TElement, TKey>.CompareKeys(index1,
  index2: Integer): Integer;
const
  MinInt = Low(Integer);
begin
  Result := fComparer.Compare(fKeys[index1], fKeys[index2]);
  if Result = 0 then
  begin
    if fNext = nil then
      Result := index1 - index2
    else
      Result := fNext.CompareKeys(index1, index2);
  end
  else if fDescending then
    if Result = MinInt then
      Result := 1
    else
      Result := -Result;
end;

procedure TEnumerableSorter<TElement, TKey>.ComputeKeys(
  const elements: TArray<TElement>; count: Integer);
var
  index: Integer;
begin
  SetLength(fKeys, count);
  for index := 0 to count - 1 do
    fKeys[index] := fKeySelector(elements[index]);
  if Assigned(fNext) then
    fNext.ComputeKeys(elements, count);
end;

{$ENDREGION}


{$REGION 'TOrderedEnumerable<T>'}

function TOrderedEnumerable<T>.GetCount: Integer;
begin
  Result := fSource.Count;
end;

function TOrderedEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(fSource, GetEnumerableSorter(nil));
end;

{$ENDREGION}


{$REGION 'TOrderedEnumerable<T>.TEnumerator'}

constructor TOrderedEnumerable<T>.TEnumerator.Create(
  const source: IEnumerable<T>; const sorter: IEnumerableSorter<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(sorter), 'sorter');
{$ENDIF}

  inherited Create;
  fBuffer := source.ToArray;
  fMap := sorter.Sort(fBuffer, Length(fBuffer));
  fIndex := -1;
end;

function TOrderedEnumerable<T>.TEnumerator.GetCurrent: T;
begin
  Result := fBuffer[fMap[fIndex]];
end;

function TOrderedEnumerable<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := fIndex < High(fBuffer);
  if Result then
    Inc(fIndex);
end;

{$ENDREGION}


{$REGION 'TOrderedEnumerable<TElement, TKey>'}

constructor TOrderedEnumerable<TElement, TKey>.Create(
  const source: IEnumerable<TElement>;
  const keySelector: TFunc<TElement, TKey>);
begin
  Create(source, keySelector, TComparer<TKey>.Default);
end;

constructor TOrderedEnumerable<TElement, TKey>.Create(
  const source: IEnumerable<TElement>; const keySelector: TFunc<TElement, TKey>;
  const comparer: IComparer<TKey>; descending: Boolean);
var
  obj: TObject;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');
{$ENDIF}

  inherited Create;
  obj := source.AsObject;
  if obj is TOrderedEnumerable<TElement> then
  begin
    fParent := TOrderedEnumerable<TElement>(obj);
    fSource := TOrderedEnumerable<TElement>(obj).fSource;
  end
  else
    fSource := source;
  fKeySelector := keySelector;
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := TComparer<TKey>.Default;
  fDescending := descending;
end;

function TOrderedEnumerable<TElement, TKey>.GetEnumerableSorter(
  const next: IEnumerableSorter<TElement>): IEnumerableSorter<TElement>;
begin
  Result := TEnumerableSorter<TElement, TKey>.Create(
    fKeySelector, fComparer, fDescending, next);
  if Assigned(fParent) then
    Result := fParent.GetEnumerableSorter(Result);
end;

{$ENDREGION}


{$REGION 'TOrderedIterator<T>'}

constructor TOrderedIterator<T>.Create(const source: IEnumerable<T>;
  const comparer: IComparer<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create;
  fSource := source;
  fComparer := comparer;
  if not Assigned(fComparer) then
    fComparer := fSource.Comparer;
end;

function TOrderedIterator<T>.Clone: TIterator<T>;
begin
  Result := TOrderedIterator<T>.Create(fSource, fComparer);
end;

function TOrderedIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fIndex := -1;
    fValues := fSource.ToArray;
    TArray.Sort<T>(fValues, fComparer);
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    if fIndex < High(fValues) then
    begin
      Inc(fIndex);
      fCurrent := fValues[fIndex];
      Exit(True);
    end;
    fState := STATE_FINISHED;
    fValues := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TZipIterator<TFirst, TSecond, TResult>'}

constructor TZipIterator<TFirst, TSecond, TResult>.Create(
  const first: IEnumerable<TFirst>; const second: IEnumerable<TSecond>;
  const resultSelector: TFunc<TFirst, TSecond, TResult>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(first), 'first');
  Guard.CheckNotNull(Assigned(second), 'second');
  Guard.CheckNotNull(Assigned(resultSelector), 'resultSelector');
{$ENDIF}

  inherited Create;
  fFirst := first;
  fSecond := second;
  fResultSelector := resultSelector;
end;

function TZipIterator<TFirst, TSecond, TResult>.Clone: TIterator<TResult>;
begin
  Result := TZipIterator<TFirst, TSecond, TResult>.Create(fFirst, fSecond, fResultSelector);
end;

function TZipIterator<TFirst, TSecond, TResult>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator1 := fFirst.GetEnumerator;
    fEnumerator2 := fSecond.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    if fEnumerator1.MoveNext and fEnumerator2.MoveNext then
    begin
      fCurrent := fResultSelector(fEnumerator1.Current, fEnumerator2.Current);
      Exit(True);
    end;
    fState := STATE_FINISHED;
    fEnumerator2 := nil;
    fEnumerator1 := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TDefaultIfEmptyIterator<T>'}

constructor TDefaultIfEmptyIterator<T>.Create(const source: IEnumerable<T>;
  const defaultValue: T);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create;
  fSource := source;
  fDefaultValue := defaultValue;
end;

function TDefaultIfEmptyIterator<T>.Clone: TIterator<T>;
begin
  Result := TDefaultIfEmptyIterator<T>.Create(fSource, fDefaultValue);
end;

function TDefaultIfEmptyIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    if fEnumerator.MoveNext then
    begin
      fFoundAny := True;
      fCurrent := fEnumerator.Current;
      Result := True;
    end
    else
    begin
      if not fFoundAny then
      begin
        fCurrent := fDefaultValue;
        fFoundAny := True;
        Result := True;
      end
      else
      begin
        fState := STATE_FINISHED;
        fEnumerator := nil;
      end;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TExtremaByIterator<T, TKey>'}

constructor TExtremaByIterator<T, TKey>.Create(const source: IEnumerable<T>;
  const keySelector: TFunc<T, TKey>; const compare: TFunc<TKey, TKey, Integer>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
  Guard.CheckNotNull(Assigned(keySelector), 'keySelector');
  Guard.CheckNotNull(Assigned(compare), 'compare');
{$ENDIF}

  inherited Create;
  fSource := source;
  fKeySelector := keySelector;
  fCompare := compare;
end;

function TExtremaByIterator<T, TKey>.Clone: TIterator<T>;
begin
  Result := TExtremaByIterator<T, TKey>.Create(fSource, fkeySelector, fCompare);
end;

function TExtremaByIterator<T, TKey>.MoveNext: Boolean;
var
  current: T;
  resultKey: TKey;
  key: TKey;
  compareResult: Integer;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fResult := TList<T>.Create;
    fEnumerator := fSource.GetEnumerator;
    if not fEnumerator.MoveNext then
      raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);

    current := fEnumerator.Current;
    resultKey := fKeySelector(current);
    fResult.Add(current);

    while fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      key := fKeySelector(current);
      compareResult := fCompare(key, resultKey);
      if compareResult = 0 then
      begin
        fResult.Add(current);
      end else
      if compareResult > 0 then
      begin
        fResult.Clear;
        fResult.Add(current);
        resultKey := key;
      end;
    end;

    fEnumerator := fResult.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    if fEnumerator.MoveNext then
    begin
      fCurrent := fEnumerator.Current;
      Exit(True);
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
    fResult := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TCastIterator<T, TResult>'}

constructor TCastIterator<T, TResult>.Create(const source: IEnumerable<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create;
  fSource := source;
end;

function TCastIterator<T, TResult>.Clone: TIterator<TResult>;
begin
  Result := TCastIterator<T, TResult>.Create(fSource);
end;

function TCastIterator<T, TResult>.MoveNext: Boolean;
var
  current: T;
  value: TValue;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    if fEnumerator.MoveNext then
    begin
      current := fEnumerator.Current;
      value := TValue.From<T>(current);
      fCurrent := value.AsType<TResult>;
      Exit(True);
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TOfTypeIterator<T, TResult>'}

constructor TOfTypeIterator<T, TResult>.Create(const source: IEnumerable<T>);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(source), 'source');
{$ENDIF}

  inherited Create;
  fSource := source;
end;

function TOfTypeIterator<T, TResult>.Clone: TIterator<TResult>;
begin
  Result := TOfTypeIterator<T, TResult>.Create(fSource);
end;

function TOfTypeIterator<T, TResult>.MoveNext: Boolean;
var
  current: T;
  value: TValue;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fEnumerator := fSource.GetEnumerator;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while fEnumerator.MoveNext do
    begin
      current := fEnumerator.Current;
      value := TValue.From<T>(current);
      if value.TryAsType<TResult>(fCurrent) then
        Exit(True);
    end;
    fState := STATE_FINISHED;
    fEnumerator := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TRepeatIterator<T>'}

constructor TRepeatIterator<T>.Create(const element: T; count: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(count >= 0, 'count');
{$ENDIF}

  inherited Create;
  fElement := element;
  fCount := count;
end;

function TRepeatIterator<T>.Clone: TIterator<T>;
begin
  Result := TRepeatIterator<T>.Create(fElement, fCount);
end;

function TRepeatIterator<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TRepeatIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fIndex := 0;
  end;

  if fState = STATE_RUNNING then
  begin
    if fIndex < fCount then
    begin
      Inc(fIndex);
      fCurrent := fElement;
      Result := True;
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TAnonymousIterator<T>'}

constructor TAnonymousIterator<T>.Create(const count: TFunc<Integer>;
  const items: TFunc<Integer, T>);
begin
  inherited Create;
  fCount := count;
  fItems := items;
end;

function TAnonymousIterator<T>.Clone: TIterator<T>;
begin
  Result := TAnonymousIterator<T>.Create(fCount, fItems);
end;

function TAnonymousIterator<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TAnonymousIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fIndex := -1;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    if fIndex < fCount then
    begin
      Inc(fIndex);
      fCurrent := fItems(fIndex);
      Exit(True);
    end;
    fState := STATE_FINISHED;
  end;
end;

{$ENDREGION}


end.
