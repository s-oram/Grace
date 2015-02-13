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

unit Spring.Collections.Sets;

interface

uses
  Generics.Collections,
  Generics.Defaults,
  Spring.Collections,
  Spring.Collections.Base;

type
  ///	<summary>
  ///	  The abstract base class for <see cref="THashSet&lt;T&gt;" />.
  ///	</summary>
  THashSetBase<T> = class abstract(TCollectionBase<T>)
  private
    type
      TGenericDictionary = Generics.Collections.TDictionary<T, Integer>;
  private
    fDictionary: TGenericDictionary;
  protected
    procedure AddInternal(const item: T); override;
  end;

  ///	<summary>
  ///	  Represents a set of values.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type of elements in the hash set.
  ///	</typeparam>
  THashSet<T> = class(THashSetBase<T>, ISet<T>)
  protected
    function GetCount: Integer; override;
  public
    ///	<summary>
    ///	  Initializes a new instance of the <see cref="THashSet&lt;T&gt;" />
    ///	  class that is empty and uses the default equality comparer for the
    ///	  set type.
    ///	</summary>
    constructor Create; overload; override;

    ///	<summary>
    ///	  Initializes a new instance of the <see cref="THashSet&lt;T&gt;" />
    ///	  class that is empty and uses the specified equality comparer for the
    ///	  set type.
    ///	</summary>
    ///	<param name="comparer">
    ///	  The <see cref="IEqualityComparer&lt;T&gt;" /> implementation to use
    ///	  when comparing values in the set, or <b>nil</b> to use the default
    ///	  <see cref="TEqualityComparer&lt;T&gt;" /> implementation for the set
    ///	  type.
    ///	</param>
    constructor Create(const comparer: IEqualityComparer<T>); overload;

    destructor Destroy; override;

    ///	<summary>
    ///	  Returns an enumerator that iterates through a
    ///	  <see cref="THashSet&lt;T&gt;" /> object.
    ///	</summary>
    ///	<returns>
    ///	  A <see cref="THashSet&lt;T&gt;.TEnumerator" /> object for the
    ///	  <see cref="THashSet&lt;T&gt;" /> object.
    ///	</returns>
    function GetEnumerator: IEnumerator<T>; override;

    ///	<summary>
    ///	  Adds the specified element to a set.
    ///	</summary>
    ///	<param name="item">
    ///	  The element to add to the set.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the element is added to the
    ///	  <see cref="THashSet&lt;T&gt;" /> object; <b>False</b> if the element
    ///	  is already present.
    ///	</returns>
    function Add(const item: T): Boolean; reintroduce;

    ///	<summary>
    ///	  Removes the specified element from a <see cref="THashSet&lt;T&gt;" />
    ///	  object.
    ///	</summary>
    ///	<param name="item">
    ///	  The element to remove.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the element is successfully found and removed;
    ///	  otherwise, <b>False</b>. This method returns <b>False</b> if
    ///	  <i>item</i> is not found in the <see cref="THashSet&lt;T&gt;" />
    ///	  object.
    ///	</returns>
    function Remove(const item: T): Boolean; override;

    function Extract(const item: T): T; override;

    ///	<summary>
    ///	  Removes all elements from a <see cref="THashSet&lt;T&gt;" /> object.
    ///	</summary>
    procedure Clear; override;

    ///	<summary>
    ///	  Determines whether a <see cref="THashSet&lt;T&gt;" /> object contains
    ///	  the specified element.
    ///	</summary>
    ///	<param name="item">
    ///	  The element to locate in the <see cref="THashSet&lt;T&gt;" /> object.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the <see cref="THashSet&lt;T&gt;" /> object contains
    ///	  the specified element; otherwise, <b>False</b>.
    ///	</returns>
    function Contains(const item: T): Boolean; override;

    ///	<summary>
    ///	  Removes all elements in the specified collection from the current
    ///	  <see cref="THashSet&lt;T&gt;" /> object.
    ///	</summary>
    ///	<param name="other">
    ///	  The collection of items to remove from the
    ///	  <see cref="THashSet&lt;T&gt;" /> object.
    ///	</param>
    ///	<exception cref="EArgumentNullException">
    ///	  <i>other</i> is <b>nil</b>.
    ///	</exception>
    procedure ExceptWith(const other: IEnumerable<T>);

    ///	<summary>
    ///	  Modifies the current <see cref="THashSet&lt;T&gt;" /> object to
    ///	  contain only elements that are present in that object and in the
    ///	  specified collection.
    ///	</summary>
    ///	<param name="other">
    ///	  The collection to compare to the current
    ///	  <see cref="THashSet&lt;T&gt;" /> object.
    ///	</param>
    ///	<exception cref="EArgumentNullException">
    ///	  <i>other</i> is <b>nil</b>.
    ///	</exception>
    procedure IntersectWith(const other: IEnumerable<T>);

    ///	<summary>
    ///	  Modifies the current <see cref="THashSet&lt;T&gt;" /> object to
    ///	  contain all elements that are present in itself, the specified
    ///	  collection, or both.
    ///	</summary>
    ///	<param name="other">
    ///	  The collection to compare to the current
    ///	  <see cref="THashSet&lt;T&gt;" /> object.
    ///	</param>
    ///	<exception cref="EArgumentNullException">
    ///	  <i>other</i> is <b>nil</b>.
    ///	</exception>
    procedure UnionWith(const other: IEnumerable<T>);

    ///	<summary>
    ///	  Determines whether a <see cref="THashSet&lt;T&gt;" /> object is a
    ///	  subset of the specified collection.
    ///	</summary>
    ///	<param name="other">
    ///	  The collection to compare to the current
    ///	  <see cref="THashSet&lt;T&gt;" /> object.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the <see cref="THashSet&lt;T&gt;" /> object is a
    ///	  subset of <i>other</i>; otherwise, <b>False</b>.
    ///	</returns>
    ///	<exception cref="EArgumentNullException">
    ///	  <i>other</i> is <b>nil</b>.
    ///	</exception>
    function IsSubsetOf(const other: IEnumerable<T>): Boolean;

    ///	<summary>
    ///	  Determines whether a <see cref="THashSet&lt;T&gt;" /> object is a
    ///	  superset of the specified collection.
    ///	</summary>
    ///	<param name="other">
    ///	  The collection to compare to the current
    ///	  <see cref="THashSet&lt;T&gt;" /> object.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the <see cref="THashSet&lt;T&gt;" /> object is a
    ///	  superset of <i>other</i>; otherwise, <b>False</b>.
    ///	</returns>
    ///	<exception cref="EArgumentNullException">
    ///	  <i>other</i> is <b>nil</b>.
    ///	</exception>
    function IsSupersetOf(const other: IEnumerable<T>): Boolean;

    ///	<summary>
    ///	  Determines whether a <see cref="THashSet&lt;T&gt;" /> object and the
    ///	  specified collection contain the same elements.
    ///	</summary>
    ///	<param name="other">
    ///	  The collection to compare to the current
    ///	  <see cref="THashSet&lt;T&gt;" /> object.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the <see cref="THashSet&lt;T&gt;" /> object is equal
    ///	  to <i>other</i>; otherwise, <b>False</b>.
    ///	</returns>
    ///	<exception cref="EArgumentNullException">
    ///	  <i>other</i> is <b>nil</b>.
    ///	</exception>
    function SetEquals(const other: IEnumerable<T>): Boolean;

    ///	<summary>
    ///	  Determines whether the current <see cref="THashSet&lt;T&gt;" />
    ///	  object and a specified collection share common elements.
    ///	</summary>
    ///	<param name="other">
    ///	  The collection to compare to the current
    ///	  <see cref="THashSet&lt;T&gt;" /> object.
    ///	</param>
    ///	<returns>
    ///	  <b>True</b> if the <see cref="THashSet&lt;T&gt;" /> object and
    ///	  <i>other</i> share at least one common element; otherwise,
    ///	  <b>False</b>.
    ///	</returns>
    ///	<exception cref="EArgumentNullException">
    ///	  <i>other</i> is <b>nil</b>.
    ///	</exception>
    function Overlaps(const other: IEnumerable<T>): Boolean;
  end;

implementation

uses
  Spring,
  Spring.Collections.Extensions,
  Spring.Collections.Lists;


{$REGION 'THashSetBase<T>'}

procedure THashSetBase<T>.AddInternal(const item: T);
begin
  fDictionary.AddOrSetValue(item, 0);
end;

{$ENDREGION}


{$REGION 'THashSet<T>'}

constructor THashSet<T>.Create;
var
  // use variable to pass nil because of codegen bug in XE2 and XE3 in x64
  comparer: IEqualityComparer<T>;
begin
  Create(comparer);
end;

constructor THashSet<T>.Create(const comparer: IEqualityComparer<T>);
begin
  inherited Create;
  fDictionary := TGenericDictionary.Create(comparer);
end;

destructor THashSet<T>.Destroy;
begin
  fDictionary.Free;
  inherited Destroy;
end;

function THashSet<T>.Add(const item: T): Boolean;
begin
  Result := not fDictionary.ContainsKey(item);
  if Result then
    inherited Add(item);
end;

function THashSet<T>.Extract(const item: T): T;
begin
  if fDictionary.ContainsKey(item) then
{$IFDEF DELPHIXE2_UP}
    Result := fDictionary.ExtractPair(item).Key
{$ELSE}
  begin
    Result := item;
    fDictionary.ExtractPair(item);
  end
{$ENDIF}
  else
    Result := Default(T);
end;

function THashSet<T>.Remove(const item: T): Boolean;
begin
  Result := fDictionary.ContainsKey(item);
  if Result then
    fDictionary.Remove(item);
end;

procedure THashSet<T>.Clear;
begin
  fDictionary.Clear;
end;

function THashSet<T>.Contains(const item: T): Boolean;
begin
  Result := fDictionary.ContainsKey(item);
end;

procedure THashSet<T>.ExceptWith(const other: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in other do
    fDictionary.Remove(item);
end;

procedure THashSet<T>.IntersectWith(const other: IEnumerable<T>);
var
  item: T;
  list: IList<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  list := TList<T>.Create;
  for item in Self do
    if not other.Contains(item) then
      list.Add(item);

  for item in list do
    Remove(item);
end;

function THashSet<T>.IsSubsetOf(const other: IEnumerable<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in Self do
    if not other.Contains(item) then
      Exit(False);

  Result := True;
end;

function THashSet<T>.IsSupersetOf(const other: IEnumerable<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in other do
    if not Contains(item) then
      Exit(False);

  Result := True;
end;

procedure THashSet<T>.UnionWith(const other: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in other do
    Add(item);
end;

function THashSet<T>.Overlaps(const other: IEnumerable<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  for item in other do
    if Contains(item) then
      Exit(True);

  Result := False;
end;

function THashSet<T>.SetEquals(const other: IEnumerable<T>): Boolean;
var
  item: T;
  localSet: ISet<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(other), 'other');
{$ENDIF}

  localSet := THashSet<T>.Create;

  for item in other do
  begin
    localSet.Add(item);
    if not Contains(item) then
      Exit(False);
  end;

  for item in Self do
    if not localSet.Contains(item) then
      Exit(False);

  Result := True;
end;

function THashSet<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorAdapter<T>.Create(fDictionary.Keys);
end;

function THashSet<T>.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

{$ENDREGION}


end.
