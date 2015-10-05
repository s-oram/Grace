{**

  Copyright (c) 2000 Ross Judson<P>

  DeCAL is licensed under the terms of the Mozilla Public License.  <P>

  The contents of this file are subject to the Mozilla Public License
  Version 1.0 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the
  License at http://www.mozilla.org/MPL/ <P>

  Use tab size 2. DeCAL code can be processed with DelphiDoc to yield
  HTML documentation.  <P>

  <STRONG>
  Delphi Container and Algorithm Library 1.0
  </STRONG><P>

  Author: Ross Judson                  <BR>
          decal@soletta.com            <P>

  Stepanov's Standard Template Library for C++ demonstrated the power of
  generic programming.  I purchased ObjectSpace's implementation of STL and,
  after climbing the learning curve, came to appreciate the leverage it
  gave me when tackling tough problems.  <P>

  Java lacked the same capabilities until ObjectSpace released JGL, the
  Java Generic Library, which is modelled on STL.  The hierarchies and
  methods that Stepanov designed were extended into Java, with the
  peculiarities and powers of that language well taken into account.  No
  serious Java developer should be without JGL, and no serious C++ developer
  should be without STL.  <P>

  Delphi programmers have lacked similar generic algorithms and containers.
  The container classes provided with Delphi are, at best, primitive.  They
  are, though, easy to use, which is their saving grace.  Serious Delphi
  applications usually try to bend the existing data structures to their
  needs, with varying degrees of success.  <P>

  There also exist one or two simple data structures libraries.  One of these,
  Julian Bucknall's EZStruct, implements a number of useful structures.  I have
  used EZStruct very successfully in the past, but had difficulty with its
  inability to store atomic types and use generic algorithms.  <P>

  What previous solutions were lacking was the strong theoretical foundation
  that the STL model provides for generic programming, and the large number
  of generic algorithms that come along with it.  <P>

  DeCAL brings this power to Delphi developers.  I hope you enjoy using it, and
  I hope that it saves you time and effort.  <P>

  Learn the algorithms, and what they do!  The secret to effectively using
  STL, JGL, and DeCAL is developing an implementation vocabulary that
  frequently makes use of the generic algorithms.  <P>

  DeCAL is packaged into a single unit to make it easy to include in your
  programs.  Some of the names are rather common -- just use DeCAL.xxxxxx to
  call a function if there's a conflict.  <P>

  I wish to express my appreciation to the following authors, whose work has
  helped me own.  <P>

  Martin Waldenburg <BR>
  Julian Bucknall <BR>
  Vladimir Merzlaikov <BR>
  Kurt Westerfeld <P>

}

{
 2009-01-15: (Andreas Hausladen)
    - Fixed: WideString hashing code
    - Added: inline support
    - Added: WideCompareText (with Win95 support) for Delphi 5

 2009-01-08: (Andreas Hausladen)
    - Fixed DMap.SetComparator access violation

 2008-10-23:
    - Applied patches that were not in the original release

 2008-10-22: (Andreas Hausladen)
    - Fixed: WideString hashing code (SizeOf(WideChar))
    - Fixed: FreeAndClear didn't set "container" to "nil"
    - Fixed: DTStrings.popFront did not remove the first item from the list
    - Fixed: abtract DTStrings.iremove was not implemented
    - Improved: DTStrings now accepts AnsiString, WideString and UnicodeString
    - Support for Delphi 2009
}

unit DeCAL;

{$IFDEF VER100}
  {$DEFINE DELPHI3}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE DELPHI3}
{$ENDIF}

{$IFDEF CONDITIONALEXPRESSIONS}
  { Delphi 6 or newer }
  {$DEFINE DELPHI6_UP}
  {$DEFINE HAS_UNIT_VARIANTS}
{$ENDIF}

// {$DEFINE DEBUG}
{$DEFINE USEPOOLS}

{$IFNDEF DELPHI3}
  {$DEFINE USELONGWORD}
{$ENDIF}

interface

uses
  Windows, Classes,
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  {$IFDEF GC}
  gc,
  {$ENDIF}
  SysUtils;

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 17.0} // System unit is included after the "interface" keyword
    {$DEFINE SUPPORTS_INLINE}
  {$IFEND}
{$ENDIF}

const
  DefaultArraySize = 16;
  DefaultBucketCount = 128;

type

  {** DeCALBase class is used as the ultimate base for all DeCAL objects.  We do
  this so we can potentially garbage collect them. }
{$IFDEF GC}
  DBaseClass = TGcObject;
{$ELSE}
  DBaseClass = TObject;
{$ENDIF}

{$IFDEF USELONGWORD}
  DeCALDWord = LongWord;
{$ELSE}
  DeCALDWORD = Integer;
{$ENDIF}

  {** DObject are TVarRecs, and can store any kind of atomic value. }
  DObject = TVarRec;

  {** DArrays keep arrays of DObjects.  We declare them using the MaxInt
  notation so that they can be of any length. }
  DObjectArray = array[0..MaxInt div SizeOf(DObject) - 1] of DObject;
  {** A pointer to an arbitrarily sized array of DObjects. }
  PDObjectArray = ^DObjectArray;

  {** A pointer to an individual DObject. }
  PDObject = ^DObject;

  {$DEFINE FREEPOSSIBLE}

  ////////////////////////////////////////////////////////////////////
  //
  // Forward Declarations
  //
  ////////////////////////////////////////////////////////////////////
  DIterHandler = class;
  DContainer = class;
  DListNode = class;
  DTreeNode = class;
  DRedBlackTree = class;

  ////////////////////////////////////////////////////////////////////
  //
  // Iterators
  //
  ////////////////////////////////////////////////////////////////////

  {** Flags that can exist on iterators.
  <DL>
  <DT>
  diSimple          </DT><DD>
  Indicates that the iterator is of the most basic type.</DD>

  <DT>
  diForward          </DT><DD>
  An iterator that can move forward only (like for single-
  linked lists).</DD>

  <DT>
  diBidirectional     </DT><DD>
  An iterator that can move forward and backward.</DD>

  <DT>
  diRandom             </DT><DD>An iterator that can move forward and backward, or to
  a particular element quickly (indexed access).</DD>
  </DL>}
  DIteratorFlag = (diSimple, diForward, diBidirectional, diRandom, diMarkStart, diMarkFinish, diKey, diIteration);
  DIteratorFlags = set of DIteratorFlag;

  {** Different underlying containers for an iterator. }
  DIteratorStucture = (dsArray, dsList, dsMap, dsSet, dsDeque, dsHash);

  {** DIterators store positional information within a container.
  I'm using a record structure here because records are assignable in Delphi.
  We want to be able to pass these iterators around freely, and not have to worry
  about continually constructing them and destroying them.  That precludes using
  the object model. }
  PDIterator = ^DIterator;
  DIterator = record
    flags : DIteratorFlags;
    Handler : DIterHandler;
    case DIteratorStucture of
      dsArray: ( position : Integer);
      dsList: (dnode : DListNode);
      dsMap, dsSet : (tree : DRedBlackTree; treeNode : DTreeNode);

      // bucketPosition is placed first so that we can pass this same iterator
      // to a secondary sequential structure (like DArray or DList) and make
      // use of the same iterator.  The problem is that we need to iterate
      // over two structures simultaneously.
      dsDeque, dsHash: ( bucketPosition, bucket : Integer);
  end;

  {** A DRange stores the beginning and ending to a range within a container. }
  DRange = record
    start, finish : DIterator;
  end;

  ////////////////////////////////////////////////////////////////////
  //
  // General Structures
  //
  ////////////////////////////////////////////////////////////////////

  {** DPairs store two complete DObjects.  They are frequently used by maps
  to contain key, value pairs. }
  DPair = record
    first, second : DObject;
  end;


  {** Contains a pair of iterators.  Not the same as a range -- ranges
   will have two iterators that are from the same container.  DIteratorPairs
   usually have iterators from two different containers. }
  DIteratorPair = record
    first, second : DIterator;
  end;

  ////////////////////////////////////////////////////////////////////
  //
  // Exceptions
  //
  ////////////////////////////////////////////////////////////////////

  {** DeCALException is the base of all exceptions thrown by DeCAL. All exceptions
  thrown should descend from here. }
  DException = class(Exception)
  end;

  {** An exception indicating that the function has not yet been implemented. }
  DNotImplemented = class(DException)
    constructor Create;
  end;

  {** Exception, upon needing a bidirectional iterator.  The iterator supplied
  is not bidirectional, or better. }
  DNeedBidirectional = class(DException)
    constructor Create;
  end;

  {** Exception upon needing a random access iterator.  The container can't
  support the operation being performed.  }
  DNeedRandom = class(DException)
    constructor Create;
  end;

  {** Exception upon acting on an empty container.  The operation being performed
  requires that the container be non-empty. }
  DEmpty = class(Exception)
    constructor Create;
  end;

  ////////////////////////////////////////////////////////////////////
  //
  // Comparison
  //
  ////////////////////////////////////////////////////////////////////

  {** A closure that can compare two objects and returns less than zero if
  obj1 is less than obj2, 0 if obj1 equals obj2, and greater than zero if
  obj1 is greater than obj2;
  @param obj1 The first object (left hand side).
  @param obj2 The second object (right hand side).}
  DComparator = function (const obj1, obj2 : DObject) : Integer of object;

  {** A procedural equivalent to the DComparator closure.  Use these when you
  want your comparator to be a procedure instead of a closure. They can be
  converted to DComparator with the MakeComparator function. }
  DComparatorProc = function(ptr : Pointer; const obj1, obj2 : DObject) : Integer;

  {** Test to see if the two objects are the same. }
  DEquals = function(const obj1, obj2 : DObject) : Boolean of object;
  {** Procedural equivalent to DEquals. }
  DEqualsProc = function(ptr : Pointer; const obj1, obj2 : DObject) : Boolean;

  {** Apply a generic test to an object.  Usually used to select objects from
   a container. }
  DTest = function(const obj : DObject) : Boolean of object;
  {** Procedural equivalent to DTest. }
  DTestProc = function(ptr : Pointer; const obj : DObject) : Boolean;

  {** Apply a test to two objects. }
  DBinaryTest = function(const obj1, obj2 : DObject) : Boolean of object;
  {** Procedural equivalent to DBinaryTest. }
  DBinaryTestProc = function(ptr : Pointer; const obj1, obj2 : DObject) : Boolean;

  {** Apply a function to an object.  Usually used in apply functions. }
  DApply = procedure(const obj : DObject) of object;
  {** Procedural equivalent to DApply. }
  DApplyProc = procedure(ptr : Pointer; const obj : DObject);

  {** Apply a function to an object.  Usually used in collect functions. }
  DUnary = function(const obj : DObject) : DObject of object;
  {** Procedural equivalent to DUnary. }
  DUnaryProc = function(ptr : Pointer; const obj : DObject) : DObject;

  {** Apply a function to two objects.  Usually used in transform functions. }
  DBinary = function(const obj1, obj2 : DObject) : DObject of object;
  {** Procedural equivalent to DBinary. }
  DBinaryProc = function(ptr : Pointer; const obj1, obj2 : DObject) : DObject;

  {** A generator creates DObjects. }
  DGenerator = function : DObject of object;

  {** Procedural equivalent to DGenerator.}
  DGeneratorProc = function(ptr : Pointer) : DObject;


  ////////////////////////////////////////////////////////////////////
  //
  // IterHandler
  //
  ////////////////////////////////////////////////////////////////////

  {**
    This class is defined separately so that we can create special types
    of iterators that aren't actually containers.  For example, we can
    create an iterator that can put objects to an object stream, or an
    iterator that filters another iterator.
  }
  DIterHandler = class(DBaseClass)
  protected
    //
    // Iterator manipulation.
    //
    {** Subclasses must advance the given iterator.  Must be implemented.
    @param iterator The iterator to be advanced.
    }
    procedure iadvance(var iterator : DIterator); virtual; abstract;

    {** Subclasses must get the object at the given iterator.  Must be implemented.
    @param iterator The iterator at which to get the object.
    }
    function iget(const iterator : DIterator) : PDObject; virtual; abstract;

    {** Subclasses must determine if the two iterators are positioned at the
    same element.
    @param iter1 The first iterator
    @param iter2 The second  iterator
    }
    function iequals(const iter1, iter2 : DIterator) : Boolean; virtual; abstract;

    {** Store an object at the given iterator.  Must be implemented by subclasses.
    @param iterator The position to store at.
    @param obj The object to put there.
    }
    procedure iput(const iterator : DIterator; const obj : DObject); virtual; abstract;

    {** Store an array of objects (or atomic values) in the container.  DContainer
    contains an implementation of this that will repeatedly call iput.
    @param iterator Where to put the objects.
    @param objs The objects to store.
    }
    procedure _iput(const iterator : DIterator; const objs : array of const); virtual; abstract;

    {** Move an iterator.  count can be positive or negative.  The default
    implementation uses repeated advance or retreat functions.  Containers that
    support random access will be able to implement this more effectively.
    @param iterator The iterator to move.
    @param count How much to move it (positive or negative).}
    procedure iadvanceBy(var iterator : DIterator; count : Integer); virtual; abstract;


    {** Determine if the given iterator is at the start of the container.
    @param iterator The iterator to test. }
    function iatStart(const iterator : DIterator) : Boolean; virtual; abstract;

    {** Determine if the given iterator is at the end of the container.
    @param iterator The iterator to test. }
    function iatEnd(const iterator : DIterator) : Boolean; virtual; abstract;

    {** Returns the container associated with the iterator.  If there is no
    container for this iterator, it returns nil.
    @param iterator The iterator whose container should be returned. }
    function igetContainer(const iterator : DIterator) : DContainer; virtual; abstract;

    {** Removes the item the iterator is positioned at, and returns an iterator
    positioned on the item that is next.  Returns an atEnd iterator if there's
    no following item. }
    function iremove(const iterator : DIterator) : DIterator; virtual; abstract;

    {** Determines the number of positions between two iterators.  For example,
    if iter1 points at the second element in an array and iter2 points at the
    fifth, the distance will be three.
    @param iter1 The beginning iterator.
    @param iter2 The ending iterator. }
    function idistance(const iter1, iter2 : DIterator) : Integer; virtual; abstract;

    // bidirectional
    {** Moves an iterator backwards by one position. }
    procedure iretreat(var iterator : DIterator); virtual; abstract;

    {** Moves an iterator backwards by count positions. }
    procedure iretreatBy(var iterator : DIterator; count : Integer); virtual; abstract;

    {** Retrieve the item at a given offset from the current iterator position. }
    function igetAt(const iterator : DIterator; offset : Integer) : PDObject; virtual; abstract;

    {** Puts an item at a given offset from the current iterator position. }
    procedure iputAt(const iterator : DIterator; offset : Integer; const obj : DObject); virtual; abstract;

    {** Returns the integer index associated with an iterator. }
    function iindex(const iterator : DIterator) : Integer; virtual; abstract;

    {** Determines if iter1 is "less" (positioned earlier in the container) than
    iter2. }
    function iless(const iter1, iter2 : DIterator) : Boolean; virtual; abstract;

    // utility
    procedure iflagChange(var iterator : DIterator; oldflags : DIteratorFlags); virtual; abstract;

  end;

  ////////////////////////////////////////////////////////////////////
  //
  // Iterator Adapters
  //
  ////////////////////////////////////////////////////////////////////

  {** DIterAdapter is an abstract base class for other classes that can
  modify the iterator manipulation behavior of a container. }
  DIterAdapter = class(DIterHandler)
  protected
    FTarget : DIterHandler;

    //
    // Iterator manipulation.
    //
    {** Subclasses must advance the given iterator.  Must be implemented.
    @param iterator The iterator to be advanced.
    }
    procedure iadvance(var iterator : DIterator); override;

    {** Subclasses must get the object at the given iterator.  Must be implemented.
    @param iterator The iterator at which to get the object.
    }
    function iget(const iterator : DIterator) : PDObject; override;

    {** Subclasses must determine if the two iterators are positioned at the
    same element.
    @param iter1 The first iterator
    @param iter2 The second  iterator
    }
    function iequals(const iter1, iter2 : DIterator) : Boolean; override;

    {** Store an object at the given iterator.  Must be implemented by subclasses.
    @param iterator The position to store at.
    @param obj The object to put there.
    }
    procedure iput(const iterator : DIterator; const obj : DObject); override;

    {** Store an array of objects (or atomic values) in the container.  DContainer
    contains an implementation of this that will repeatedly call iput.
    @param iterator Where to put the objects.
    @param objs The objects to store.
    }
    procedure _iput(const iterator : DIterator; const objs : array of const); override;

    {** Move an iterator.  count can be positive or negative.  The default
    implementation uses repeated advance or retreat functions.  Containers that
    support random access will be able to implement this more effectively.
    @param iterator The iterator to move.
    @param count How much to move it (positive or negative).}
    procedure iadvanceBy(var iterator : DIterator; count : Integer); override;

    function iremove(const iterator : DIterator) : DIterator; override;

    function iatStart(const iterator : DIterator) : Boolean; override;
    function iatEnd(const iterator : DIterator) : Boolean; override;
    function igetContainer(const iterator : DIterator) : DContainer; override;
    function idistance(const iter1, iter2 : DIterator) : Integer; override;

    // bidirectional
    procedure iretreat(var iterator : DIterator); override;
    procedure iretreatBy(var iterator : DIterator; count : Integer); override;
    function igetAt(const iterator : DIterator; offset : Integer) : PDObject; override;
    procedure iputAt(const iterator : DIterator; offset : Integer; const obj : DObject); override;

    // random
    function iindex(const iterator : DIterator) : Integer; override;
    function iless(const iter1, iter2 : DIterator) : Boolean; override;

    procedure iflagChange(var iterator : DIterator; oldflags : DIteratorFlags); override;

  public
    constructor Create(var target : DIterator);
  end;

  {** DIterFilter adapters apply a test to underlying objects to determine
  if they should be part of the adapted container.  Pass a test to the
  constructor.  Then, each time an iterator is advanced or retreated, items
  that don't pass the test will be skipped over. }
  DIterFilter = class(DIterAdapter)
  protected
    FTest : DTest;

    procedure iadvance(var iterator : DIterator); override;
    procedure iretreat(var iterator : DIterator); override;

  public
    {** Construct a DIterFilter, using test as the filter to determine if
    a given item should be part of the filtered container.
    @param target An iterator representing the container or range to be filtered.
    @param test The test used to determine if an item is part of the sequence or
                not.  Only those items that pass the test will be part of the
                filtered sequence. }
    constructor Create(var target : DIterator; test : DTest);
  end;

  {** DIterSkipper adapters skip forward or backward by an integral number of
  items each time the iterator is advanced or retreated.  Pass the skip
  value to the constructor. }
  DIterSkipper = class(DIterAdapter)
  protected
    FSkipBy : Integer;

    procedure iadvance(var iterator : DIterator); override;
    procedure iretreat(var iterator : DIterator); override;

  public
    constructor Create(var target : DIterator; skipBy : Integer);

  end;

  ////////////////////////////////////////////////////////////////////
  //
  // Container
  //
  ////////////////////////////////////////////////////////////////////

  {** DContainer is the base class of all containers.  It provides a number of
  generic facilities for container usage and management.  The basic iterator
  manipulation routines are made virtual and abstract, forcing subclasses to
  implement them. }

  DContainer = class(DIterHandler)
  private
    fComparator: DComparator;

  protected
    procedure SetComparator(const Value: DComparator); virtual;
    procedure cloneTo(newContainer : DContainer); virtual;

  protected
    //
    // Iterator manipulation.
    //
    procedure _iput(const iterator : DIterator; const objs : array of const); override;

    {** Move an iterator.  count can be positive or negative.  The default
    implementation uses repeated advance or retreat functions.  Containers that
    support random access will be able to implement this more effectively.
    @param iterator The iterator to move.
    @param count How much to move it (positive or negative).}
    procedure iadvanceBy(var iterator : DIterator; count : Integer); override;


    function iatStart(const iterator : DIterator) : Boolean; override;
    function iatEnd(const iterator : DIterator) : Boolean; override;
    function igetContainer(const iterator : DIterator) : DContainer; override;
    function idistance(const iter1, iter2 : DIterator) : Integer; override;

    // bidirectional
    procedure iretreat(var iterator : DIterator); override;
    procedure iretreatBy(var iterator : DIterator; count : Integer); override;
    function igetAt(const iterator : DIterator; offset : Integer) : PDObject; override;
    procedure iputAt(const iterator : DIterator; offset : Integer; const obj : DObject); override;

    // random
    function iindex(const iterator : DIterator) : Integer; override;
    function iless(const iter1, iter2 : DIterator) : Boolean; override;

    procedure iflagChange(var iterator : DIterator; oldflags : DIteratorFlags); override;

    procedure _clear(direct : Boolean); virtual; abstract;

  public
    property comparator : DComparator read fComparator write SetComparator;

    {** Add a DObject to this container.  The object is copied and added to
    the container.

      @param	obj				The object to add.

    }
    procedure _add(const obj : DObject); virtual; abstract;

    {** Add a DObject to this container.  The object is NOT copied -- it
    is moved into the container. Do not clear the object afterwards. }
    procedure addRef(const obj : DObject); virtual;

    {** Add an array of objects to the container.  This call makes use of
    Delphi's open array system, and as such the array can contain any type
    of object.  Each object will be copied into the container. }
    procedure add(const objs : array of const); virtual;

    {** Remove all instances of an object, by value, from the container. }
    procedure _remove(const obj : DObject); virtual; abstract;

    {** Remove all instances of each in an array of objects, by value, from the container. }
    procedure remove(const objs : array of const); virtual;

    {** Clear this container of all contents.  Note that this does not perform
    any type of free or destructor operation.  If you want to free all the
    objects in a container before clearing if, use the ObjFree algorithm. }
    procedure clear; virtual;

    {** Inform the container that "amount" items are going to be inserted.
    Most containers don't have any concept of setting the capacity, but
    for those that do, algorithms can call this to provide a hint to the
    container about how many items are going to be inserted. }
    procedure ensureCapacity(amount : Integer); virtual;

    {** Request that the container use the minimum amount of memory possible
    for its current contents.  Note that this is only a hint to the container;
    it may or may not have any effect. }
    procedure trimToSize; virtual;

    {** Return an iterator positioned after the last element in the container.
    Note that the finish position is a valid insertion point for those containers
    that can have the add operation performed. }
    function finish : DIterator; virtual; abstract;

    {** Return the absolute maximum number of objects that can be stored in
    this container.  The container does not necessarily have this space allocated;
    it is just the maximum that <i>could</i> be allocated. }
    function maxSize : Integer; virtual; abstract;

    {** Return an iterator positioned on the first object in the container. }
    function start : DIterator; virtual; abstract;

    {** Return a complete copy of this container.  This is a copy by value, as
    all objects are stored in DContainers by value. }
    function clone : DContainer; virtual;

    {** Requests that this container compare two DObjects using its current
    comparator.  If obj1 is less than obj2, the result is negative.  If they
    are equal, the result is 0; otherwise it is positive. }
    function binaryCompare(const obj1, obj2 : DObject) : Integer;

    {** Determines if two objects are equal, using this container's current
    comparator. }
    function binaryTest(const obj1, obj2 : DObject) : Boolean;

    {** Requests the comparator currently being used by this container. }
    procedure getComparator(var compare : DComparator); virtual;

    {** Retrives the current comparator of this container as a binary test,
    which enables testing for equality only. }
    procedure getBinaryTest(var bt : DBinaryTest); virtual;

    {** Determines if this container is empty. }
    function isEmpty : Boolean; virtual;

    {** Determines the number of objects currently in this container. }
    function size : Integer; virtual;

    {** Determine if this container has an object matching obj. }
    function _contains(const obj : DObject) : Boolean; virtual;

    {** Determine if this container has an object matching any of objs. }
    function contains(const objs : array of const) : Boolean; virtual;

    {** Determine the number of items matching obj. }
    function _count(const obj : DObject) : Integer; virtual;

    {** Determine the total number of items matching objs. }
    function count(const objs : array of const) : Integer; virtual;

    {** Does this container use pairs?}
    function usesPairs : Boolean; virtual;


    {** A comparator that uses hashes to differentiate objects. }
    function hashComparator(const obj1, obj2 : DObject) : Integer;

    {** A comparator that compares strings without case sensitivity. }
    function CaselessStringComparator(const obj1, obj2 : DObject) : Integer;

    {** The standard comparator that can compare all atomic types. }
    function DObjectComparator(const obj1, obj2 : DObject) : Integer;

    {** Creates this container using the specified comparator for ordering
    elements. }
    constructor CreateWith(compare : DComparator); virtual;

    {** Creates this container using the default comparator (DObjectComparator). }
    constructor Create; virtual;

  end;

  DContainerClass = class of DContainer;

  ////////////////////////////////////////////////////////////////////
  //
  // Container Adapters
  //
  ////////////////////////////////////////////////////////////////////

  {** A container adapter is used to give a container a certain kind of
  interface.  For example, by using the DStack adapter, any sequential
  container can be made to have stack-like behavior.  All adapter classes
  should descend from DAdapter.  }
  DAdapter = class(DContainer)
  protected
    FContainer : DContainer;  // the container we are wrapping.
  public
    // constructor CreateOn(cont : DContainer);
  end;

  DAdapterClass = class of DAdapter;

  DStack = class(DAdapter)
  public
  {
    procedure _Push(const obj : DObject); virtual;
    procedure Push(const obj : array of const); virtual;
    function _Pop : DObject; virtual;
    function PopXXX...
  }
  end;

  DQueue = class(DAdapter)
  end;

  ////////////////////////////////////////////////////////////////////
  //
  // Sequences
  //
  ////////////////////////////////////////////////////////////////////

  {** DSequence is an abstract base class for containers that hold their
  items in a defined order. }
  DSequence = class(DContainer)
  protected
    function iremove(const iterator : DIterator) : DIterator; override;

  public

    // Container overrides

    {** Add a DObject to this container.  The object is copied and added to
    the container.

      @param	obj				The object to add.

    }
    procedure _add(const obj : DObject); override;

    {** Removes an object, by value, from this sequence. }
    procedure _remove(const obj : DObject); override;

    // DSequence stuff
    {** Return the item at the given position.  Note that returning this item
    may or may not be an efficient implementation.  DVector-based structures
    will be more efficient. The returned object can be converted with a toXXX
    function. }
    function at(pos : Integer) : DObject; virtual;

    {** Return a reference to the DObject at the given position.  }
    function atRef(pos : Integer) : PDObject; virtual;

    {** Return a reference to the last item in the sequence. }
    function backRef : PDObject; virtual;

    {** Return the last item in the sequence.  This returned item must be
    correctly disposed of, or converted with a toXXX function. }
    function back : DObject; virtual;

    {** Count the number of times an item occurs in a given range. }
    function _countWithin(_begin, _end : Integer; const obj : DObject) : Integer; virtual;

    {** Count the number of times an item occurs in a given range. }
    function countWithin(_begin, _end : Integer; const objs : array of const) : Integer; virtual;

    {** Return a reference to the first object in the sequence. }
    function frontRef : PDObject; virtual;

    {** Return the first object in the sequence.  This value must be cleaned
    up or converted with a toXXX function. }
    function front : DObject; virtual;

    {** Find out where obj is in the collection.  Returns -1 if not found. }
    function _indexOf(const obj : DObject) : Integer; virtual;

    {** Find out where obj is in the collection.  Returns -1 if not found. }
    function indexOf(const objs : array of const) : Integer; virtual;

    {** Find out where obj is within the given range..  Returns -1 if not found. }
    function _indexOfWithin(_begin, _end : Integer; const obj : DObject) : Integer; virtual;

    {** Find out where obj is within the given range..  Returns -1 if not found. }
    function indexOfWithin(_begin, _end : Integer; const objs : array of const) : Integer; virtual;

    {** Set the item at a given position. }
    procedure _putAt(index : Integer; const obj : DObject); virtual;

    {** Set the item at a given position. }
    procedure putAt(index : Integer; const objs : array of const); virtual;

    {** Remove count items an iterator is positioned at. All following items
    move up by count. }
    function removeAtIter(iter : DIterator; count : Integer) : DIterator; virtual; abstract;

    {** Replace every occurrence of obj1 with obj2 in the sequence.  Copies
    will be made of obj2. }
    procedure _replace(obj1, obj2 : DObject); virtual;

    {** Replace sources with targets, pairwise.  That is, the first element in
    sources will be replaced with the first in targets, the second in sources
    with the second in targets, and so on. }
    procedure replace(const sources, targets : array of const); virtual;


    {** Replace sources with targets, pairwise, in the specified range.  That is, the first element in
    sources will be replaced with the first in targets, the second in sources
    with the second in targets, and so on. }
    procedure _replaceWithin(_begin, _end : Integer; obj1, obj2 : DObject); virtual;

    {** Replace sources with targets, pairwise, in the specified range.  That is, the first element in
    sources will be replaced with the first in targets, the second in sources
    with the second in targets, and so on. }
    procedure replaceWithin(_begin, _end : Integer; const sources, targets : array of const); virtual;

    {** Remove every occurrence of the specified object within the given range. }
    procedure _removeWithin(_begin, _end : Integer; const obj : DObject); virtual; abstract;

    {** Remove every occurrence of each of the specified objects within the given range. }
    procedure removeWithin(_begin, _end : Integer; const objs : array of const); virtual;

    {** Remove the last element in the sequence, returning its value. That value
    must be cleaned up or converted with a toXXX function. }
    function popBack : DObject; virtual; abstract;

    {** Remove the first element in the sequence, returning its value. That value
    must be cleaned up or converted with a toXXX function. }
    function popFront : DObject; virtual; abstract;

    {** Push an object to the back of the sequence.  The object will be copied. }
    procedure _pushBack(const obj : DObject); virtual; abstract;

    {** Push values to the back of the container.  The values will push in the
    order specified in the array. }
    procedure pushBack(const objs : array of const); virtual;

    {** Push an object to the front of the container. }
    procedure _pushFront(const obj : DObject); virtual; abstract;

    {** Push values to the front of the container.  The values will appear at
    the front of the container in the order given. }
    procedure pushFront(const objs : array of const); virtual;

    {** Retrieves at an index as an integer. Asserts if the type is not correct. }
    function atAsInteger(pos : Integer) : Integer;
    {** Retrieves at an index as a Boolean. Asserts if the type is not correct. }
    function atAsBoolean(pos : Integer) : Boolean;
    {** Retrieves at an index as a AnsiChar. Asserts if the type is not correct. }
    function atAsAnsiChar(pos : Integer) : AnsiChar;
    {** Retrieves at an index as a Char. Asserts if the type is not correct. }
    function atAsChar(pos : Integer) : Char;
    {** Retrieves at an index as an extended floating point value. Asserts if the type is not correct. }
    function atAsExtended(pos : Integer) : Extended;
    {** Retrieves at an index as a short string.Asserts if the type is not correct. }
    function atAsShortString(pos : Integer) : ShortString;
    {** Retrieves at an index as an untyped pointer.Asserts if the type is not correct. }
    function atAsPointer(pos : Integer) : Pointer;
    {** Retrieves at an index as a PAnsiChar. Asserts if the type is not correct. }
    function atAsPAnsiChar(pos : Integer) : PAnsiChar;
    {** Retrieves at an index as a PChar. Asserts if the type is not correct. }
    function atAsPChar(pos : Integer) : PChar;
    {** Retrieves at an index as an object reference.  Asserts if the type is not correct. }
    function atAsObject(pos : Integer) : TObject;
    {** Retrieves at an index as a class reference (TClass). Asserts if the type is not correct. }
    function atAsClass(pos : Integer) : TClass;
    {** Retrieves at an index as a WideChar. Asserts if the type is not correct. }
    function atAsWideChar(pos : Integer) : WideChar;
    {** Retrieves at an index as a pointer to a WideChar. Asserts if the type is not correct. }
    function atAsPWideChar(pos : Integer) : PWideChar;
    {** Retrieves at an index as a AnsiString. Asserts if the type is not correct. }
    function atAsAnsiString(pos : Integer) : AnsiString;
    {** Retrieves at an index as a string. Asserts if the type is not correct. }
    function atAsString(pos : Integer) : string;
    {** Retrieves at an index as a currency value. Asserts if the type is not correct. }
    function atAsCurrency(pos : Integer) : Currency;
    {** Retrieves at an index as a variant. Asserts if the type is not correct. }
    function atAsVariant(pos : Integer) : Variant;
    {** Retrieves at an index as an interface pointer. Asserts if the type is not correct. }
    function atAsInterface(pos : Integer) : Pointer;
    {** Retrieves at an index as a WideString. Asserts if the type is not correct. }
    function atAsWideString(pos : Integer) : WideString;
{$IFDEF USELONGWORD}
    function atAsInt64(pos : Integer) : Int64;
{$ENDIF}

  protected
    function _at(pos : Integer) : PDObject; virtual;
  end;

  DSequenceClass = class of DSequence;

  {** DVector is an abstract base class for containers that hold their items
  in an integer-addressable sequence. }
  DVector = class(DSequence)
  public
    //
    // Deques and Arrays (which are both Vectors) can do these...
    //

    {** Returns the number of elements that can fit into this vector without
    any expansion. }
    function capacity : Integer; virtual; abstract;

    {** Inserts an object before the object the iterator is positioned over.
    If the iterator is atEnd, the object will be added at the end. }
    procedure _insertAtIter(iterator : DIterator; const obj : DObject); virtual; abstract;

    {** Inserts values before the object the iterator is positioned over.
    If the iterator is atEnd, the object will be added at the end. The values
    will appear in the order specified. }
    procedure insertAtIter(iterator : DIterator; const objs : array of const); virtual;

    {** Inserts an object before the object at position index.
    If the iterator is atEnd, the object will be added at the end. }
    procedure _insertAt(index : Integer; const obj : DObject); virtual; abstract;

    {** Inserts values before the object at position index.
    If the iterator is atEnd, the object will be added at the end. The values
    will appear in the order specified. }
    procedure insertAt(index : Integer; const objs : array of const); virtual;

    {** Inserts  count copies of obj before the object iterator is positioned at. }
    procedure _insertMultipleAtIter(iterator : DIterator; count : Integer; const obj : DObject); virtual; abstract;

    {** Inserts count copies of a value before the object the iterator is positioned at. }
    procedure insertMultipleAtIter(iterator : DIterator; count : Integer; const obj : array of const); virtual;

    {** Inserts count copies of obj before the object at position index. }
    procedure _insertMultipleAt(index : Integer; count : Integer; const obj : DObject); virtual; abstract;

    {** Inserts count copies of a value before the object at position index. }
    procedure insertMultipleAt(index : Integer; count : Integer; const obj : array of const); virtual;

    {** Inserts copies of the objects in a given range before the object the
    iterator is over. }
    procedure insertRangeAtIter(iterator : DIterator; _start, _finish : DIterator); virtual; abstract;

    {** Inserts copies of the objects in a given range before the object at position
    index. }
    procedure insertRangeAt(index : Integer; _start, _finish : DIterator); virtual; abstract;

    {** Determines if the given index is legal for this container.  Effectively checks
    if index is greater than zero and less than the number of items. }
    function legalIndex(index : Integer) : Boolean; virtual;

    {** Remove the object at the given index. }
    procedure removeAt(index : Integer); virtual; abstract;

    {** Remove all objects between two indicies. }
    procedure removeBetween(_begin, _end : Integer); virtual; abstract;

    {** Remove every occurrence of object between two indicies. }
    procedure _removeWithin(_begin, _end : Integer; const obj : DObject); override; abstract;

    {** Remove every occurence of a value between two indicies. }
    procedure removeWithin(_begin, _end : Integer; const objs : array of const); override;

    {** Ensure that this vector can accomodate at least amount objects without
    expanding. }
    procedure setCapacity(amount : Integer); virtual; abstract;

  end;

  {** DArray is a classic vector of items.  Arrays have very fast indexed
  access to elements.  Very fast addition to the end is possible if you
  call the ensureCapacity method with the right number of elements before
  adding.  As additions are occurring, the DArray will adaptively resize
  itself based on a blocking factor.  DArrays will expand themselves by
  30% or so each time they run out of capacity. }
  DArray = class(DVector)

  protected
    items : PDObjectArray;
    cap, len, blocking : Integer;

    //function addressOf(index : Integer) : PDObject; virtual;
    function makeSpaceAt(index, count : Integer) : PDObject; virtual;
    procedure removeSpaceAt(index, count : Integer); virtual;
    function iterFor(index : Integer) : DIterator; virtual;

    //
    // Iterator manipulation.
    //
    procedure iadvance(var iterator : DIterator); override;
    procedure iadvanceBy(var iterator : DIterator; count : Integer); override;
    function iget(const iterator : DIterator) : PDObject; override;
    function iequals(const iter1, iter2 : DIterator) : Boolean; override;
    procedure iput(const iterator : DIterator; const obj : DObject); override;
    function iremove(const iterator : DIterator) : DIterator; override;
    function idistance(const iter1, iter2 : DIterator) : Integer; override;

    // bidirectional
    procedure iretreat(var iterator : DIterator); override;
    procedure iretreatBy(var iterator : DIterator; count : Integer); override;
    function igetAt(const iterator : DIterator; offset : Integer) : PDObject; override;
    procedure iputAt(const iterator : DIterator; offset : Integer; const obj : DObject); override;

    // random
    function iindex(const iterator : DIterator) : Integer; override;
    function iless(const iter1, iter2 : DIterator) : Boolean; override;

    function _at(pos : Integer) : PDObject; override;
    procedure _clear(direct : Boolean); override;

  public
    constructor Create; override;
    constructor CreateWith(compare : Dcomparator); override;
    constructor createSize(size : Integer);
    destructor Destroy; override;

    {** Add an object to the array.

      @param	obj				The object to add.

    }
    procedure _add(const obj : DObject); override;

    {** Return an iterator positioned after the last item in the array. }
    function finish : DIterator; override;

    {** Return the maximum number of items that may be placed in the
    array.  This is a theoretical limit. }
    function maxSize : Integer; override;

    {** Return the current number of items in the array. }
    function size : Integer; override;

    {** Return an iterator positioned on the first item in the array.  If
    there are no items in the array, the iterator is positioned atEnd. }
    function start : DIterator; override;

    //
    // DSequence overrides;
    //
    {** Return the item at the specified position.
    @param pos Position of the item to retrieve. }
    function at(pos : Integer) : DObject; override;

    {** Returns a reference to the last item in the array. }
    function backRef : PDObject; override;

    {** Returns a reference to the first item in the array. }
    function frontRef : PDObject; override;

    {** Removes and returns the last item in the array. }
    function popBack : DObject; override;

    {** Removes and returns the first item in the array.  }
    function popFront : DObject; override;

    procedure _pushBack(const obj : DObject); override;
    procedure _pushFront(const obj : DObject); override;

    {** Removes the number of items specified in count, starting with the
    given iterator. }
    function removeAtIter(iter : DIterator; count : Integer) : DIterator; override;

    {** Put an object at a specific place.

     @param 	index 		Position to place the object.  What happens when we have a
                        really long comment?  What does it decide to do?  We are curious
                        about the result of a long description.
     @param		obj				The object to put there. }
    procedure _putAt(index : Integer; const obj : DObject); override;

    //
    // DArray specific
    //
    {** Copy the contents of another array into this one. }
    procedure copy(another : DArray);

    {** Copy the contents of this array into another one. }
    procedure copyTo(another : DArray);

    {** Returns the current blocking factor.  When growing the array, the current array capacity is divided by
    the block factor to determine how many new entries to add.  Block factor
    defaults to 4, so that capacity / 4 entries will be added each time the
    array must be grown. }
    function blockFactor : Integer;

    {** Sets the current blocking factor.  The current capacity will be divided
    by this factor to determine the number of entries to add to the array, when
    expansion is necessary. Block factor must be greater than zero. }
    procedure setBlockFactor(factor : Integer);

    //
    // DVector overrides
    //
    {** Return the number of items the array is capable of holding without
    expanding. }
    function capacity : Integer; override;

    {** Ensure that a certain number of items can be held without expanding
    storage (which can be expensive). It's a good idea to set this when
    you're going to add a large number of items to a container. }
    procedure ensureCapacity(amount : Integer); override;

    {** Inserts  count copies of obj before the object iterator is positioned at. }
    procedure _insertAtIter(iterator : DIterator; const obj : DObject); override;

    {** Inserts an object before the object at position index.
    If the iterator is atEnd, the object will be added at the end. }
    procedure _insertAt(index : Integer; const obj : DObject); override;

    {** Inserts  count copies of obj before the object iterator is positioned at. }
    procedure _insertMultipleAtIter(iterator : DIterator; count : Integer; const obj : DObject); override;

    {** Inserts count copies of obj before the object at position index. }
    procedure _insertMultipleAt(index : Integer; count : Integer; const obj : DObject); override;


    {** Inserts copies of the objects in a given range before the object the
    iterator is over. }
    procedure insertRangeAtIter(iterator : DIterator; _start, _finish : DIterator); override;

    {** Inserts copies of the objects in a given range before the object at position
    index. }
    procedure insertRangeAt(index : Integer; _start, _finish : DIterator); override;

    procedure _remove(const obj : DObject); override;
    procedure removeAt(index : Integer); override;
    procedure removeBetween(_begin, _end : Integer); override;
    procedure _removeWithin(_begin, _end : Integer; const obj : DObject); override;

    procedure setCapacity(amount : Integer); override;

    {** Directly set the number of items being held by this array.  If the
    size is smaller than the current size, the extra items will be cleared
    and eliminated.  If the size is larger, the newly created items will be
    filled with an empty value. }
    procedure setSize(newSize : Integer); virtual;

    {** Minimize the storage required by this container.  The storage used
    will shrink to the smallest possible amount. }
    procedure trimToSize; override;

  end;

  {** Node element for DList class. }
  DListNode = class(DBaseClass)
    next, previous : DListNode;
    obj : DObject;

    constructor Create(const anObj : DObject);
    destructor Destroy; override;
    destructor Kill; virtual;

  end;

  {** Double linked list. The classic data structure -- fast insertion,
  fast deletion, slow searching. }
  DList = class(DSequence)
  protected
    head, tail : DListNode;
    length : Integer;

    procedure _clear(direct : Boolean); override;

  public

    {** Construct a new DList. }
    constructor Create; override;

    {** Construct a new DList, that uses the specified comparator for operations
    that require comparators. }
    constructor CreateWith(compare : DComparator); override;
    destructor Destroy; override;

    {** Override of DContainer's _add; usually called internally.  Adds the
    specified DObject to the list.  Copies the DObject. }
    procedure _add(const obj : DObject); override;

    procedure _remove(const obj : DObject); override;

    {** Creates a new DList that is a clone of this one, including copies of
    all the items in this list.  If the items are objects, only the pointer
    is copied, not the object itself (shallow copy). }
    // function clone : DContainer; override;

    {** Returns an iterator positioned at the end of this list.  Inserting
    at the iterator will add to the list. }
    function finish : DIterator; override;

    {** Returns the maximum number of elements that can be placed in the list. }
    function maxSize : Integer; override;

    {** Returns the number of items in this list. }
    function size : Integer; override;

    {** Returns an iterator positioned at the beginning of this list, on the first
    item.  If the iterator has no items in it, it returns an iterator with
    atEnd being true. }
    function start : DIterator; override;

    //
    // DSequence overrides;
    //
    {** Returns a pointer to the last object in the list.  Does not copy the
    object.  The pointer can be derefences to examine its value. }
    function backRef : PDObject; override;

    {** Returns a pointer to the first object in the list.  Does not copy the
    object.  The pointer can be derefences to examine its value. }
    function frontRef : PDObject; override;

    {** Returns the last object in this list, and removes it from the list.
    Note that this is returning a DObject, and as such, the value returned
    must be cleared with ClearDObject if it is not stored in an appropriate
    place. }
    function popBack : DObject; override;

    {** Returns the first object in this list, and removes it from the list.
    Note that this is returning a DObject, and as such, the value returned
    must be cleared with ClearDObject if it is not stored in an appropriate
    place. }
    function popFront : DObject; override;

    {** Adds an object to the end of the list.  Copies the object given. }
    procedure _pushBack(const obj : DObject); override;

    {** Adds an object to the front of the list.  Copies the object given. }
    procedure _pushFront(const obj : DObject); override;

    {** Removes all objects in the list between the two integer positions for
    that are equal to obj. }
    procedure _removeWithin(_begin, _end : Integer; const obj : DObject); override;

    {** Removes count objects beginning at the given iterator. }
    function removeAtIter(iter : DIterator; count : Integer) : DIterator; override;

    //
    // DList specific
    //
    {** Removes all objects between two iterators.  Does not remove under the
    _finish iterator (removes all objects up to but NOT including the _finish
    object. }
    procedure cut(_start, _finish : DIterator); virtual;

    {** Insert an object at the given iterator.  The item the iterator is
    currently positioned at is pushed back.  If an atEnd iterator is passed
    as the location, the object is added to the end (back) of the list. }
    procedure _insertAtIter(iterator : DIterator; const obj : DObject); virtual;

    {** Insert objects at the given iterator.  The item the iterator is
    currently positioned at is pushed back.  If an atEnd iterator is passed
    as the location, the object is added to the end (back) of the list. }
    procedure insertAtIter(iterator : DIterator; const objs : array of const); virtual;

    {** Sort this DList, very efficiently. }
    procedure mergeSort; virtual;

    {** Sort this DList, using the specified comparator. }
    procedure mergeSortWith(compare : DComparator); virtual;


  protected

    procedure removeRange(s,f : DListNode); virtual;
    procedure removeNode(node : DListNode); virtual;
    procedure _mergeSort(var s, f : DListNode; compare : DComparator); virtual;

    //
    // Iterator manipulation.
    //
    procedure iadvance(var iterator : DIterator); override;
    function iget(const iterator : DIterator) : PDObject; override;
    function iequals(const iter1, iter2 : DIterator) : Boolean; override;
    procedure iput(const iterator : DIterator; const obj : DObject); override;
    function iremove(const iterator : DIterator) : DIterator; override;

    // bidirectional
    procedure iretreat(var iterator : DIterator); override;

  end;

  ////////////////////////////////////////////////////////////////////
  //
  // Internal Red-Black Tree
  //
  ////////////////////////////////////////////////////////////////////

  {** Red black tree nodes get colored either red or black.  Surprised? }
  DTreeNodeColor = (tnfBlack, tnfRed);

  {** RBTrees are collections of nodes. }
  DTreeNode = class(DBaseClass)

  public

    pair : DPair;
    left, right, parent : DTreeNode;
    color : DTreeNodeColor;

    constructor Create;
    destructor Destroy; override;
    destructor Kill; virtual;
    constructor CreateWith(const _pair : DPair);
    constructor CreateUnder(const _pair : DPair; _parent : DTreeNode);
    constructor MakeWith(const _pair : DPair; _parent, _left, _right : DTreeNode);

{$IFDEF USEPOOLS}
    class function NewInstance : TObject; override;
    procedure FreeInstance; override;
{$ENDIF}

  end;

  {** Internal class.  Do not use. }
  DRedBlackTree = class(DBaseClass)

  protected

    FContainer : DContainer;
    FHeader : DTreeNode;
    FComparator : DComparator;
    FNodeCount : Integer;
    FInsertAlways : Boolean;

    //
    // Internal functions
    //
    procedure RBInitNil;
    procedure RBIncrement(var node : DTreeNode);
    procedure RBDecrement(var node : DTreeNode);
    function RBMinimum(node : DTreeNode) : DTreeNode;
    function RBMaximum(node : DTreeNode) : DTreeNode;
    procedure RBLeftRotate(node : DTreeNode);
    procedure RBRightRotate(node : DTreeNode);
    procedure RBinsert(insertToLeft : Boolean; x,y,z : DTreeNode);
    function RBErase(z : DTreeNode) : DTreeNode;

    function RBInternalInsert(x,y : DTreeNode; const pair : DPair) : Boolean;
    procedure RBInitializeRoot;
    procedure RBInitializeHeader;
    procedure RBInitialize;
    function RBCopyTree(oldNode, parent : DTreeNode) : DTreeNode;
    procedure RBCopy(tree : DRedBlackTree);
    procedure RBEraseTree(node : DTreeNode; direct : Boolean);
    // function key(node : DTreeNode) : DObject;
    // function value(node : DTreeNode) : DObject;

    function StartNode : DTreeNode;
    function EndNode : DTreeNode;

  public

    constructor Create(insideOf : DContainer; always : Boolean; compare : DComparator);
    destructor Destroy; override;

    function start : DIterator;
    function finish : DIterator;

    function empty : Boolean;
    function size : Integer;
    function maxSize : Integer;

    procedure swap(another : DRedBlackTree);

    function insert(const pair : DPair) : Boolean;
    function insertAt(pos : DIterator; const pair : DPair) : Boolean;
    function insertIn(_start, _finish : DIterator) : Boolean;

    procedure erase(direct : Boolean);
    procedure eraseAt(pos : DIterator);
    function eraseKeyN(const obj : DObject; count : Integer) : Integer;
    function eraseKey(const obj : DObject) : Integer;
    function eraseIn(_start, _finish : DIterator) : Integer;

    function find(const obj : DObject) : DIterator;
    function count(const obj : DObject) : Integer;
    function lower_bound(const obj : DObject) : DIterator;
    function upper_bound(const obj : DObject) : DIterator;
    function equal_range(const obj : DObject) : DRange;

  end;

  ////////////////////////////////////////////////////////////////////
  //
  // Associative Structures
  //
  ////////////////////////////////////////////////////////////////////

  DAssociative = class(DContainer)
  public

    //
    // The following methods need to be overridden by subclasses of
    // DAssociative.
    //
    {** Determine if this map permits duplicates. }
    function allowsDuplicates : Boolean; virtual; abstract;

    {** Return the number of pairs with keys equal to the specified key. }
    // function _count(const key : DObject) : Integer; virtual; abstract;

    {** Return the number of pairs with values equal to the specified value. }
    function _countValues(const value : DObject) : Integer; virtual; abstract;

    {** Retrieve the value for a specified key.  The key must exist in the map. }
    function _getAt(const key : DObject) : DObject; virtual; abstract;

    {** Returns an iterator positioned at the pair with the specified key.
    If the key is not found, the iterator is positioned at the end. }
    function _locate(const key : DObject) : DIterator; virtual; abstract;

    {** Add the specified key, value pair to the map. Copies are made of
    the objects. }
    procedure _putAt(const key, value : DObject); virtual; abstract;

    {** Removes the first count pairs with the specified key. }
    procedure _removeN(const key : DObject; count : Integer); virtual; abstract;

    {** Removes the first pair with the specified value. }
    procedure _removeValueN(const value : DObject; count : Integer); virtual; abstract;

    {** Removes the pair the iterator is pointing to. }
    procedure removeAt(iterator : DIterator); virtual; abstract;

    {** Removes all pairs from start to finish. }
    procedure removeIn(_start, _finish : DIterator); virtual; abstract;

    {** Returns a key oriented iterator positioned at the first pair. }
    function startKey : DIterator; virtual; abstract;


    //
    // These methods are implemented here and subclasses inherit them.
    //
    {** Return the number of pairs with the specified key. Pass only one key. }
    // function count(const key : array of const) : Integer; virtual;

    {** Return the number of pairs with values equal to the specified value. }
    function countValues(const value : array of const) : Integer; virtual;

    {** Retrieve the value for a specified key.  The key must exist in the map. }
    function getAt(const key : array of const) : DObject; virtual;

    {** Returns an iterator positioned at the pair with the specified key.
    If the key is not found, the iterator is positioned at the end. }
    function locate(const key : array of const) : DIterator; virtual;

    {** Add an open array of keys and values to the map.  There must be
    the same number of elements in each array.  The first element in the
    key array is matched with the first in the value array; the second with
    the second, and so on. }
    procedure putAt(const key, value : array of const); virtual;

    {** Add a key, value pair.  You must pass exactly two items in the const
    array. }
    procedure putPair(const pair : array of const); virtual;

    {** Removes the first pair with the specified value. }
    procedure removeValueN(const value : array of const; count : Integer); virtual;

    {** Removes all pairs with the specified value. }
    procedure removeValue(const value : array of const); virtual;

  end;

  DAssociativeClass = class of DAssociative;

  {** Internal class.  Do not use. }
  DInternalMap = class(DAssociative)

  protected

    tree : DRedBlackTree;

    procedure SetComparator(const Value: DComparator); override;

    //
    // Iterator manipulation.
    //
    procedure iadvance(var iterator : DIterator); override;
    function iget(const iterator : DIterator) : PDObject; override;
    function iequals(const iter1, iter2 : DIterator) : Boolean; override;
    procedure iput(const iterator : DIterator; const obj : DObject); override;
    function iremove(const iterator : DIterator) : DIterator; override;

    // bidirectional
    procedure iretreat(var iterator : DIterator); override;
    function igetAt(const iterator : DIterator; offset : Integer) : PDObject; override;
    procedure iputAt(const iterator : DIterator; offset : Integer; const obj : DObject); override;

    procedure MorphIterator(var iterator : DIterator); virtual;

    constructor ProtectedCreate(always_insert : Boolean; compare : DComparator);
    constructor ProtectedDuplicate(another : DInternalMap);

    {** Clears all objects from this map. }
    procedure _clear(direct : Boolean); override;

  public

    //
    // DContainer
    //

    {** Adds an object to this map. }
    procedure _add(const obj : DObject); override;

    {** Clones this map to another one. }
    procedure cloneTo(newContainer : DContainer); override;

    {** Returns an iterator positioned after the last item. }
    function finish : DIterator; override;
    {** Returns the absolute maximum number of items this map can hold. }
    function maxSize : Integer; override;
    {** Returns an iterator positioned at the first pair in the map. }
    function start : DIterator; override;

    {** Returns the number of pairs in this map. }
    function size : Integer; override;

    //
    // Map stuff
    //

    {** Determine if this map permits duplicates. }
    function allowsDuplicates : Boolean; override;

    {** Return the number of pairs with keys equal to the specified key. }
    function _count(const key : DObject) : Integer; override;

    {** Return the number of pairs with values equal to the specified value. }
    function _countValues(const value : DObject) : Integer; override;

    {** Retrieve the value for a specified key.  The key must exist in the map. }
    function _getAt(const key : DObject) : DObject; override;

    {** Returns an iterator positioned at the pair with the specified key.
    If the key is not found, the iterator is positioned at the end. }
    function _locate(const key : DObject) : DIterator; override;

    {** Position an iterator on the first pair that matches or is greater
    than the supplied key. Bound functions are only available on ordered
    map structures. }
    function _lower_bound(const key : DObject) : DIterator;

    {** Position an iterator on the first pair that matches or is greater
    than the supplied key. Bound functions are only available on ordered
    map structures. }
    function lower_bound(const obj : array of const) : DIterator;

    {** Position an iterator after the last pair that matches or is less
    than the supplied key. Bound functions are only available on ordered
    map structures. }
    function _upper_bound(const key : DObject) : DIterator;

    {** Position an iterator after the last pair that matches or is less
    than the supplied key. Bound functions are only available on ordered
    map structures. }
    function upper_bound(const obj : array of const) : DIterator;

    {** Add the specified key, value pair to the map. Copies are made of
    the objects. }
    procedure _putAt(const key, value : DObject); override;

    {** Removes the all pairs with the specified key. }
    procedure _remove(const key : DObject); override;

    {** Removes the first count pairs with the specified key. }
    procedure _removeN(const key : DObject; count : Integer); override;

    {** Removes the first pair with the specified value. }
    procedure _removeValueN(const value : DObject; count : Integer); override;

    {** Removes the pair the iterator is pointing to. }
    procedure removeAt(iterator : DIterator); override;

    {** Removes all pairs from start to finish. }
    procedure removeIn(_start, _finish : DIterator); override;

    {** Returns a key oriented iterator positioned at the first pair. }
    function startKey : DIterator; override;

    destructor Destroy; override;

  end;

  {** Maps associate a key with a value.  If no comparator is supplied during construction, the hashComparator is
  used...making a hash map. }
  DMap = class(DInternalMap)
  public
    function usesPairs : Boolean; override;
    constructor Create; override;
    constructor CreateWith(compare : DComparator); override;
    constructor CreateFrom(another : DMap);

    // function clone : DContainer; override;
  end;

  {** Multi maps permit a key to appear more than once. }
  DMultiMap = class(DInternalMap)
  public
    function usesPairs : Boolean; override;
    constructor Create; override;
    constructor CreateWith(compare : DComparator); override;
    constructor CreateFrom(another : DMultiMap);

    // function clone : DContainer; override;

  end;

  ////////////////////////////////////////////////////////////////////
  //
  // Sets
  //
  ////////////////////////////////////////////////////////////////////

  // Sets are implemented in terms of maps.  Sets us the same structure
  // as maps, but the second half of the pair is always empty.
  DSet = class(DInternalMap)

  protected
    procedure MorphIterator(var iterator : DIterator); override;

  public
    constructor Create; override;
    constructor CreateWith(compare : DComparator); override;
    constructor CreateFrom(another : DSet);

    // function clone : DContainer; override;

    procedure _add(const obj : DObject); override;
    procedure _putAt(const key, value : DObject); override;


    {** Returns true if the specified object is in the set.}
    function _includes(const obj : DObject) : Boolean; virtual;
    {** Returns true if ALL the specified objects are in the set. }
    function includes(const obj : array of const) : Boolean; virtual;

  end;

  DMultiSet = class(DSet)
    constructor Create; override;
    constructor CreateWith(compare : DComparator); override;
    constructor CreateFrom(another : DMultiSet);

    // function clone : DContainer; override;
  end;

  ////////////////////////////////////////////////////////////////////
  //
  // Hash maps
  //
  ////////////////////////////////////////////////////////////////////

  DHash = function(const buffer; byteCount : Integer) : Integer of object;
  DHashProc = function(ptr : Pointer; const buffer; byteCount : Integer) : Integer;

  DInternalHash = class(DAssociative)
  protected
    FHash : DHash;
    FStorageClass : DSequenceClass;
    FAllowDups : Boolean;  		// Do we allow duplicates, or do we replace?
    FBuckets : DArray;
    FBucketCount : Integer;
    FCount : Integer;  				// How many things in this container?
    FIsSet : Boolean;					// Are we doing set-based behavior?

    procedure cloneTo(newContainer : DContainer); override;

    //
    // Iterator manipulation.
    //
    procedure iadvance(var iterator : DIterator); override;
    function iget(const iterator : DIterator) : PDObject; override;
    function iequals(const iter1, iter2 : DIterator) : Boolean; override;
    procedure iput(const iterator : DIterator; const obj : DObject); override;
    function iremove(const iterator : DIterator) : DIterator; override;

    // bidirectional
    procedure iretreat(var iterator : DIterator); override;

    procedure iflagChange(var iterator : DIterator; oldflags : DIteratorFlags); override;

    procedure Setup;
    function HashObj(const obj : DObject) : Integer;
    function RecoverSecondaryIterator(const baseIter : DIterator) : DIterator;

    function BucketSequence(idx : Integer) : DSequence;

    {** Clears all objects from this hash map. }
    procedure _clear(direct : Boolean); override;

  public
    constructor Create; override;
    constructor CreateWith(compare : DComparator); override;
    constructor CreateFrom(another : DInternalHash);
    destructor Destroy; override;

    {** Adds an object to this hash map. }
    procedure _add(const obj : DObject); override;

    {** Returns an iterator positioned after the last item. }
    function finish : DIterator; override;

    {** Returns the absolute maximum number of items this hash map can hold. }
    function maxSize : Integer; override;

    {** Returns an iterator positioned at the first pair in the hash map. }
    function start : DIterator; override;

    {** Returns the number of pairs in this hash map. }
    function size : Integer; override;

    //
    // Associative stuff
    //

    {** Determine if this map permits duplicates. }
    function allowsDuplicates : Boolean; override;

    {** Return the number of pairs with keys equal to the specified key. }
    function _count(const key : DObject) : Integer; override;

    {** Return the number of pairs with values equal to the specified value. }
    function _countValues(const value : DObject) : Integer; override;

    {** Retrieve the value for a specified key.  The key must exist in the map. }
    function _getAt(const key : DObject) : DObject; override;

    {** Returns an iterator positioned at the pair with the specified key.
    If the key is not found, the iterator is positioned at the end. }
    function _locate(const key : DObject) : DIterator; override;

    {** Add the specified key, value pair to the map. Copies are made of
    the objects. }
    procedure _putAt(const key, value : DObject); override;

    {** Removes the all pairs with the specified key. }
    procedure _remove(const key : DObject); override;

    {** Removes the first count pairs with the specified key. }
    procedure _removeN(const key : DObject; count : Integer); override;

    {** Removes the first pair with the specified value. }
    procedure _removeValueN(const value : DObject; count : Integer); override;

    {** Removes the pair the iterator is pointing to. }
    procedure removeAt(iterator : DIterator); override;

    {** Removes all pairs from start to finish. }
    procedure removeIn(_start, _finish : DIterator); override;

    {** Returns a key oriented iterator positioned at the first pair. }
    function startKey : DIterator; override;

    //
    // Hash specific
    //

    {** Choose the number of buckets to be used.  It is an error to call this
    if the container is not empty.
    @param bCount The number of buckets to use.  Using more buckets usually
                  reduces search time, but chews up more memory.  You need
                  to decide on an appropriate tradeoff.  The default is
                  128.}
    procedure SetBucketCount(bCount : Integer);

    {** Choose a different class for the hash buckets.  Supported classes
    are DSequence derivatives.  It is an error to call this function if
    the container is not empty.
    @param cls  The DSequence-derived class to use to store items that
                collide on hash entries.  Arrays are the tightested, but
                don't have good deletion characteristics. Lists delete better,
                but take up more space and iterate more slowly. }
    procedure SetBucketClass(cls : DSequenceClass);

  end;

  DHashMap = class(DInternalHash)
  public
    function usesPairs : Boolean; override;
  end;

  DMultiHashMap = class(DInternalHash)
  public
    function usesPairs : Boolean; override;
    constructor Create; override;
    constructor CreateWith(compare : DComparator); override;
    constructor CreateFrom(another : DMultiHashMap);
  end;

  DHashSet = class(DInternalHash)
  public
    constructor Create; override;
    constructor CreateWith(compare : DComparator); override;
    constructor CreateFrom(another : DHashSet);

    function _includes(const obj : DObject) : Boolean; virtual;
    function includes(const obj : array of const) : Boolean; virtual;

  end;

  DMultiHashSet = class(DHashSet)
  public
    constructor Create; override;
    constructor CreateWith(compare : DComparator); override;
    constructor CreateFrom(another : DMultiHashSet);
  end;


  ////////////////////////////////////////////////////////////////////
  //
  // VCL data structure adapters
  //
  ////////////////////////////////////////////////////////////////////

  //
  // A TStringsAdapter provides a DContainer-like interface to
  //
  DTStrings = class(DVector)

  protected
    FStrings : TStrings;
    FDummy : DObject;

    //
    // Iterator manipulation.
    //
    procedure iadvance(var iterator : DIterator); override;
    procedure iadvanceBy(var iterator : DIterator; count : Integer); override;
    function iget(const iterator : DIterator) : PDObject; override;
    function iequals(const iter1, iter2 : DIterator) : Boolean; override;
    procedure iput(const iterator : DIterator; const obj : DObject); override;
    function idistance(const iter1, iter2 : DIterator) : Integer; override;

    // bidirectional
    procedure iretreat(var iterator : DIterator); override;
    procedure iretreatBy(var iterator : DIterator; count : Integer); override;
    function igetAt(const iterator : DIterator; offset : Integer) : PDObject; override;
    procedure iputAt(const iterator : DIterator; offset : Integer; const obj : DObject); override;

    // random
    function iindex(const iterator : DIterator) : Integer; override;
    function iless(const iter1, iter2 : DIterator) : Boolean; override;

    function _at(pos : Integer) : PDObject; override;
    procedure _clear(direct : Boolean); override;

  public

    // This constructor deliberately overrides the base class one.
    constructor Create(strings : TStrings); reintroduce;
    destructor Destroy; override;

    procedure _add(const obj : DObject); override;
    function clone : DContainer; override;
    function finish : DIterator; override;
    function maxSize : Integer; override;
    function size : Integer; override;
    function start : DIterator; override;

    //
    // DSequence overrides;
    //
    function at(pos : Integer) : DObject; override;
    function backRef : PDObject; override;
    function frontRef : PDObject; override;
    function popBack : DObject; override;
    function popFront : DObject; override;
    procedure _pushBack(const obj : DObject); override;
    procedure _pushFront(const obj : DObject); override;
    function removeAtIter(iter : DIterator; count : Integer) : DIterator; override;

    procedure _putAt(index : Integer; const obj : DObject); override;

    //
    // DVector overrides
    //
    function capacity : Integer; override;
    procedure ensureCapacity(amount : Integer); override;
    procedure _insertAtIter(iterator : DIterator; const obj : DObject); override;
    procedure _insertAt(index : Integer; const obj : DObject); override;
    procedure _insertMultipleAtIter(iterator : DIterator; count : Integer; const obj : DObject); override;
    procedure _insertMultipleAt(index : Integer; count : Integer; const obj : DObject); override;
    procedure insertRangeAtIter(iterator : DIterator; _start, _finish : DIterator); override;
    procedure insertRangeAt(index : Integer; _start, _finish : DIterator); override;
    procedure _remove(const obj : DObject); override;
    procedure removeAt(index : Integer); override;
    procedure removeBetween(_begin, _end : Integer); override;
    procedure _removeWithin(_begin, _end : Integer; const obj : DObject); override;
    procedure setCapacity(amount : Integer); override;
    procedure setSize(newSize : Integer); virtual;
    procedure trimToSize; override;

  end;

  DTList = class(DVector)
  protected
    FList : TList;
  public
    // This constructor deliberately overrides the base class one.
    constructor Create(list : TList); reintroduce;  // intentional hiding of base class method
  end;


  ////////////////////////////////////////////////////////////////////
  //
  // Functors
  //
  ////////////////////////////////////////////////////////////////////

  DUnaryFunction = class(DBaseClass)
    function Execute(const obj : DObject) : DObject; virtual; abstract;
  end;

  DBinaryFunction = class(DBaseClass)
    function Execute(ob1, ob2 : DObject) : DObject; virtual; abstract;
  end;

  ////////////////////////////////////////////////////////////////////
  //
  // Iterator functions
  //
  ////////////////////////////////////////////////////////////////////

  {** Moves the iterator to the next object in the container.
  @param iterator The iterator to advance.}
  procedure advance(var iterator : DIterator); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Returns a new iterator at the next position in the container. }
  function advanceF(const iterator : DIterator) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Moves the iterator forward by count objects.
  @param iterator The iterator to advance.
  @param count The number of positions to advance.}
  procedure advanceBy(var iterator : DIterator; count : Integer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Returns a new iterator at the next position in the container. This is
  a functional version of advanceBy, returning a new iterator. }
  function advanceByF(const iterator : DIterator; count : Integer) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Tests to see if the iterator is at the start of the container.
  @param iterator The iterator to test.}
  function atStart(const iterator : DIterator) : Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Tests to see if the iterator is at the end of the container.  This is
  extremely common during loops.  Containers should make a real effort to
  ensure that this is processed quickly.
  @param iterator The iterator to test.}
  function atEnd(const iterator : DIterator) : Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve the object an iterator is positioned at.  This object is
  returned in DObject form, which is a generic type that can hold any
  object.  Use one of the conversion functions (asObject, etc.) to
  turn it into something useful.  Or, use one of the getXXX functions,
  which are slightly more efficient.
  @param iterator The iterator to get the object from. }
  function get(const iterator : DIterator) : DObject; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve a pointer to the object the iterator is positioned at.
  This is somewhat more efficient than getting the object directly,
  which involves copying.  Many of the internal functions use this to
  avoid copying DObjects around.
  @param iterator The iterator to get the object from.}
  function getRef(const iterator : DIterator) : PDObject; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  // TODO: What the f&^&* to I really need to do here to make this
  // PutRef/PutRefClear dichotomy work?  The answer is that PutRef
  // probably shouldn't exist, 'cause if the source needs to clear
  // it somehow, it's just plain wrong.
  {** Store an object directly to a location.  This routine ensures the
  target location is clean, then makes a direct copy of the object.  Note
  that you should NOT clear the source DObject -- it is now residing
  at the iterator position.}
  procedure putRef(const iterator : DIterator; const obj : DObject);

  {** Retrieve the object at the iterator as an integer.
  @param iterator The iterator to get from. }
  function getInteger(const iterator : DIterator) : Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve the object at the iterator as a boolean.
  @param iterator The iterator to get from. }
  function getBoolean(const iterator : DIterator) : Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve the object at the iterator as a character.
  @param iterator The iterator to get from. }
  function getAnsiChar(const iterator : DIterator) : AnsiChar; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve the object at the iterator as a character.
  @param iterator The iterator to get from. }
  function getChar(const iterator : DIterator) : Char; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve the object at the iterator as an extended floating point value.
  @param iterator The iterator to get from. }
  function getExtended(const iterator : DIterator) : Extended; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve the object at the iterator as a short string (old style string).
  @param iterator The iterator to get from. }
  function getShortString(const iterator : DIterator) : ShortString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve the object at the iterator as an untyped pointer.
  @param iterator The iterator to get from. }
  function getPointer(const iterator : DIterator) : Pointer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve the object at the iterator as a PAnsiChar.
  @param iterator The iterator to get from. }
  function getPAnsiChar(const iterator : DIterator) : PAnsiChar; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve the object at the iterator as a PChar.
  @param iterator The iterator to get from. }
  function getPChar(const iterator : DIterator) : PChar; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve the object at the iterator as an object reference.
  It's a good idea to do a typecast with this using the AS operator.
  @param iterator The iterator to get from. }
  function getObject(const iterator : DIterator) : TObject; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve the object at the iterator as a class reference (TClass).
  @param iterator The iterator to get from. }
  function getClass(const iterator : DIterator) : TClass; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve the object at the iterator as a wide character.
  @param iterator The iterator to get from. }
  function getWideChar(const iterator : DIterator) : WideChar; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve the object at the iterator as a pointer to a wide character.
  @param iterator The iterator to get from. }
  function getPWideChar(const iterator : DIterator) : PWideChar; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve the object at the iterator as a AnsiString.
  @param iterator The iterator to get from. }
  function getAnsiString(const iterator : DIterator) : AnsiString;

  {** Retrieve the object at the iterator as a string.
  @param iterator The iterator to get from. }
  function getString(const iterator : DIterator) : string;

  {** Retrieve the object at the iterator as a currency value.
  @param iterator The iterator to get from. }
  function getCurrency(const iterator : DIterator) : Currency; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve the object at the iterator as a variant.
  @param iterator The iterator to get from. }
  function getVariant(const iterator : DIterator) : Variant; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve the object at the iterator as an interface pointer.
  @param iterator The iterator to get from. }
  function getInterface(const iterator : DIterator) : Pointer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieve the object at the iterator as a WideString.
  @param iterator The iterator to get from. }
  function getWideString(const iterator : DIterator) : WideString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

{$IFDEF USELONGWORD}
  function getInt64(const iterator : DIterator) : Int64; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ENDIF}
  //
  // Atomic data type converters!
  //

  {** Converts the DObject to an integer. Asserts if the type is not correct. }
  function asInteger(const obj : DObject) : Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Converts the DObject to a Boolean. Asserts if the type is not correct. }
  function asBoolean(const obj : DObject) : Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Converts the DObject to a AnsiChar. Asserts if the type is not correct. }
  function asAnsiChar(const obj : DObject) : AnsiChar; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Converts the DObject to a Char. Asserts if the type is not correct. }
  function asChar(const obj : DObject) : Char; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Converts the DObject to an extended floating point value. Asserts if the type is not correct. }
  function asExtended(const obj : DObject) : Extended; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Converts the DObject to a short string.Asserts if the type is not correct. }
  function asShortString(const obj : DObject) : ShortString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Converts the DObject to an untyped pointer.Asserts if the type is not correct. }
  function asPointer(const obj : DObject) : Pointer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Converts the DObject to a PAnsiChar. Asserts if the type is not correct. }
  function asPAnsiChar(const obj : DObject) : PAnsiChar; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Converts the DObject to a PChar. Asserts if the type is not correct. }
  function asPChar(const obj : DObject) : PChar; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Converts the DObject to an object reference.  Asserts if the type is not correct. }
  function asObject(const obj : DObject) : TObject; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Converts the DObject to a class reference (TClass). Asserts if the type is not correct. }
  function asClass(const obj : DObject) : TClass; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Converts the DObject to a WideChar. Asserts if the type is not correct. }
  function asWideChar(const obj : DObject) : WideChar; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Converts the DObject to a pointer to a WideChar. Asserts if the type is not correct. }
  function asPWideChar(const obj : DObject) : PWideChar; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Converts the DObject to a AnsiString. Asserts if the type is not correct. }
  function asAnsiString(const obj : DObject) : AnsiString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Converts the DObject to a string. Asserts if the type is not correct. }
  function asString(const obj : DObject) : string;
  {** Converts the DObject to a currency value. Asserts if the type is not correct. }
  function asCurrency(const obj : DObject) : Currency; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Converts the DObject to a variant. Asserts if the type is not correct. }
  function asVariant(const obj : DObject) : Variant; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Converts the DObject to an interface pointer. Asserts if the type is not correct. }
  function asInterface(const obj : DObject) : Pointer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Converts the DObject to a WideString. Asserts if the type is not correct. }
  function asWideString(const obj : DObject) : WideString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$IFDEF USELONGWORD}
  function asInt64(const obj : DObject) : Int64; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ENDIF}

  //
  // More atomic data converters, except that these ones clear the source
  // DObject automatically.
  //
  {** Retrieves at an index as an integer. Asserts if the type is not correct.  Clears the source DObject. }
  function toInteger(const obj : DObject) : Integer;
  {** Retrieves at an index as a Boolean. Asserts if the type is not correct.  Clears the source DObject. }
  function toBoolean(const obj : DObject) : Boolean;
  {** Retrieves at an index as a AnsiChar. Asserts if the type is not correct.  Clears the source DObject. }
  function toAnsiChar(const obj : DObject) : AnsiChar;
  {** Retrieves at an index as a Char. Asserts if the type is not correct.  Clears the source DObject. }
  function toChar(const obj : DObject) : Char;
  {** Retrieves at an index as an extended floating point value. Asserts if the type is not correct.  Clears the source DObject. }
  function toExtended(const obj : DObject) : Extended;
  {** Retrieves at an index as a short string.Asserts if the type is not correct.  Clears the source DObject. }
  function toShortString(const obj : DObject) : ShortString;
  {** Retrieves at an index as an untyped pointer.Asserts if the type is not correct.  Clears the source DObject. }
  function toPointer(const obj : DObject) : Pointer;
  {** Retrieves at an index as a PAnsiChar. Asserts if the type is not correct.  Clears the source DObject. }
  function toPAnsiChar(const obj : DObject) : PAnsiChar;
  {** Retrieves at an index as a PChar. Asserts if the type is not correct.  Clears the source DObject. }
  function toPChar(const obj : DObject) : PChar;
  {** Retrieves at an index as an object reference.  Asserts if the type is not correct.  Clears the source DObject. }
  function toObject(const obj : DObject) : TObject;
  {** Retrieves at an index as a class reference (TClass). Asserts if the type is not correct.  Clears the source DObject. }
  function toClass(const obj : DObject) : TClass;
  {** Retrieves at an index as a WideChar. Asserts if the type is not correct.  Clears the source DObject. }
  function toWideChar(const obj : DObject) : WideChar;
  {** Retrieves at an index as a pointer to a WideChar. Asserts if the type is not correct.  Clears the source DObject. }
  function toPWideChar(const obj : DObject) : PWideChar;
  {** Retrieves at an index as a AnsiString. Asserts if the type is not correct.  Clears the source DObject. }
  function toAnsiString(const obj : DObject) : AnsiString;
  {** Retrieves at an index as a string. Asserts if the type is not correct.  Clears the source DObject. }
  function toString(const obj : DObject) : string;
  {** Retrieves at an index as a currency value. Asserts if the type is not correct.  Clears the source DObject. }
  function toCurrency(const obj : DObject) : Currency;
  {** Retrieves at an index as a variant. Asserts if the type is not correct.  Clears the source DObject. }
  function toVariant(const obj : DObject) : Variant;
  {** Retrieves at an index as an interface pointer. Asserts if the type is not correct.  Clears the source DObject. }
  function toInterface(const obj : DObject) : Pointer;
  {** Retrieves at an index as a WideString. Asserts if the type is not correct.  Clears the source DObject. }
  function toWideString(const obj : DObject) : WideString;

{$IFDEF USELONGWORD}
  function toInt64(const obj : DObject) : Int64;
{$ENDIF}

  {** Determines if two iterators are equal -- that is, they are pointing at
  the same container and at the same object.
  @param iter1 The first iterator.
  @param iter2 The second iterator.}
  function equals(const iter1, iter2 : DIterator) : Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Retrieves the container associated with an iterator.  You can cast the
  reference returned into the real container type, or just use it as a
  generic container.
  @param iterator The iterator to retrieve a container for. }
  function getContainer(const iterator : DIterator) : DContainer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}


  {** Stores an object at the current iterator location, if possible.  Following
  the put operation, the iterator is advanced.  This is very convenient for
  adding objects to a finish-iterator.
  @param iterator The location to store at.
  @param obj The object to store. }
  procedure _output(var iterator : DIterator; const obj : DObject);
  procedure output(var iterator : DIterator; const objs : array of const);
  procedure _outputRef(var iterator : DIterator; const obj : DObject);

  {** Stores an object at the current iterator location, if possible.  Some
  containers don't permit arbitrary insertions (like trees).  The put operation
  erases whatever was in the location before; it is destructive.
  @param iterator The location to store at.
  @param obj The object to store. }
  procedure _put(const iterator : DIterator; const obj : DObject); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Stores an array of objects at the current iterator location, if possible.  Some
  containers don't permit arbitrary insertions (like trees).  The put operation
  erases whatever was in the location before; it is destructive. <P>
  This form of put is very useful for storing atomic types into a container. <P>
  <CODE>array._put(array.start, [1,2,3])</CODE>
  @param iterator The location to store at.
  @param obj The object to store. }
  procedure put(const iterator : DIterator; const objs : array of const);

  {** Store an integer at the current iterator location. }
  procedure putInteger(const iterator : DIterator; value : Integer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store a boolean at the current iterator location. }
  procedure putBoolean(const iterator : DIterator; value : Boolean); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store a character at the current iterator location. }
  procedure putAnsiChar(const iterator : DIterator; value : AnsiChar); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store a character at the current iterator location. }
  procedure putChar(const iterator : DIterator; value : Char); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store an extended floating point value at the current iterator location. }
  procedure putExtended(const iterator : DIterator; const value : Extended); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store a short string at the current iterator location. }
  procedure putShortString(const iterator : DIterator; const value : ShortString); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store an untyped pointer at the current iterator location. }
  procedure putPointer(const iterator : DIterator; value : Pointer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store a PAnsiChar at the current iterator location. }
  procedure putPAnsiChar(const iterator : DIterator; value : PAnsiChar); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store a PChar at the current iterator location. }
  procedure putPChar(const iterator : DIterator; value : PChar); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store an object reference at the current iterator location. }
  procedure putObject(const iterator : DIterator; value : TObject); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store a class reference at the current iterator location. }
  procedure putClass(const iterator : DIterator; value : TClass); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store a wide character at the current iterator location. }
  procedure putWideChar(const iterator : DIterator; value : WideChar); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store a pointer to a wide character at the current iterator location. }
  procedure putPWideChar(const iterator : DIterator; value : PWideChar); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store a AnsiString at the current iterator location. }
  procedure putAnsiString(const iterator : DIterator; const value : AnsiString); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store a string at the current iterator location. }
  procedure putString(const iterator : DIterator; const value : string); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store a currency value at the current iterator location. }
  procedure putCurrency(const iterator : DIterator; value : Currency); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store a variant at the current iterator location. }
  procedure putVariant(const iterator : DIterator; const value : Variant); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store an interface pointer at the current iterator location. }
  procedure putInterface(const iterator : DIterator; value : Pointer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Store a wide string at the current iterator location. }
  procedure putWideString(const iterator : DIterator; const value : WideString); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$IFDEF USELONGWORD}
  procedure putInt64(const iterator : DIterator; const value : Int64); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ENDIF}

  {** Set an integer to a DObject. }
  procedure setInteger(var obj : DObject; value : Integer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set a boolean to a DObject. }
  procedure setBoolean(var obj : DObject; value : Boolean); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set a character to a DObject. }
  procedure setAnsiChar(var obj : DObject; value : AnsiChar); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set a character to a DObject. }
  procedure setChar(var obj : DObject; value : Char); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set an extended floating point value to a DObject. }
  procedure setExtended(var obj : DObject; const value : Extended); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set a short string to a DObject. }
  procedure setShortString(var obj : DObject; const value : ShortString); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set an untyped pointer to a DObject. }
  procedure setPointer(var obj : DObject; value : Pointer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set a PAnsiChar to a DObject. }
  procedure setPAnsiChar(var obj : DObject; value : PAnsiChar); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set a PChar to a DObject. }
  procedure setPChar(var obj : DObject; value : PChar); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set an object reference to a DObject. }
  procedure setObject(var obj : DObject; value : TObject); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set a class reference to a DObject. }
  procedure setClass(var obj : DObject; value : TClass); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set a wide character to a DObject. }
  procedure setWideChar(var obj : DObject; value : WideChar); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set a pointer to a wide character to a DObject. }
  procedure setPWideChar(var obj : DObject; value : PWideChar); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set a AnsiString to a DObject. }
  procedure setAnsiString(var obj : DObject; const value : AnsiString); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set a string to a DObject. }
  procedure setString(var obj : DObject; const value : string); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set a currency value to a DObject. }
  procedure setCurrency(var obj : DObject; value : Currency); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set a variant to a DObject. }
  procedure setVariant(var obj : DObject; const value : Variant); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set an interface pointer to a DObject. }
  procedure setInterface(var obj : DObject; value : Pointer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Set a wide string to a DObject. }
  procedure setWideString(var obj : DObject; const value : WideString); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

{$IFDEF USELONGWORD}
  procedure setInt64(var obj : Dobject; const value : Int64); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ENDIF}


  {** Create a DObject from an atomic type.  This function is intended to
  make a single DObject only.  It uses the array of const syntax to permit
  the construction of a DObject from any atomic type.  The DObject created
  contains a <STRONG>copy</STRONG> of the object.
  @param obj The object to Create a DObject from. }
  function make(const obj : array of const) : DObject;

  {** Determines the number of positions between iter1 and iter2.  The two
  iterators must point to the same container. }
  function distance(const iter1, iter2 : DIterator) : Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  //
  // bidirectional
  //

  {** Move an iterator backwards one position. }
  procedure retreat(var iterator : DIterator); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Move an iterator backwards by count positions. }
  procedure retreatBy(var iterator : DIterator; count : Integer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Move an iterator backwards one position. }
  function retreatF(const iterator : DIterator) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Move an iterator backwards by count positions. }
  function retreatByF(const iterator : DIterator; count : Integer) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Return the object at the given offset from the iterator.
  @param iter The iterator that gives the starting position.
  @param offset The number of positions to move (positive or negative). }
  function getAt(const iter : DIterator; offset : Integer) : DObject; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Put an object at the given offset from the iterator. }
  procedure putAt(const iter : DIterator; offset : Integer; const obj : DObject); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Use the array of const form to store atomic values at the given offset from
  iter.
  @param iter The iterator to use as the starting position.
  @param offset The number of positions to move.
  @param objs An array of const (array of atomic values). }
  procedure _putAt(const iter : DIterator; offset : Integer; const objs : array of const);

  //
  // random access
  //
  {** Determine the index (position within the container) for the given iterator. }
  function index(const iter : DIterator) : Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Determine if iter1 comes before iter2. }
  function less(const iter1, iter2 : DIterator) : Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Toggle the iterator to retrieve keys.  Only useful with map containers. }
  procedure SetToKey(var iter : DIterator);

  {** Toggle the iterator to retrieve values.  Only useful with map containers. }
  procedure SetToValue(var iter : DIterator);

  {** IterateOver initiates and moves an iterator from its position until the end.
  It returns true as long as there are elements to process.  The best way to
  use this is with a while loop:  iter := container.start;  while IterateOver(iter) do ...  <P>
  Note that you don't need to call an advance on the iterator...the IterateOver
  function does that for you. }
  function IterateOver(var iter : DIterator) : Boolean;

  ////////////////////////////////////////////////////////////////////
  //
  // Applying
  //
  ////////////////////////////////////////////////////////////////////

  {** Apply the unary function to each object in the container.
  @param container The container to iterator over.
  @param unary The function to apply. }
  procedure forEach(container : DContainer; unary : DApply); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Apply the unary function to a range of objects.
  @param _start An iterator set to the start of the range.
  @param _end An iterator set to the end of the range.
  @param unary The function to apply.}
  procedure forEachIn(_start, _end : DIterator; unary : DApply);

  {** Apply a unary function to each object in a container if it
  passes a test.
  @param container The container to iterate over.
  @param unary The function to apply.
  @param test The test that must be passed before the function will be applied. }
  procedure forEachIf(container : DContainer; unary : DApply; test : DTest); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Apply a unary function to a range of objects if the object passes
  a test.
  @param _start An iterator set to the start of the range.
  @param _end An iterator set to the end of the range.
  @param unary The function to apply.
  @param test The test that must be passed before unary will be applied.}
  procedure forEachInIf(_start, _end : DIterator; unary : DApply; test : DTest);


  {** Call binary with obj as the first parameter and the first item in the
  container as the next parameter.  Then call binary with the result of the
  first call to binary as the first parameter, and the second object in the
  container as the second parameter.  Keep going.  Return the last return value
  of binary.  If no items are in the container, return obj. }
  function _inject(container : DContainer; const obj : DObject; binary : DBinary) : DObject;

  {** Call binary with obj as the first parameter and the items in the range
  as the second parameter.  Pass the result of each call to binary to the next
  call to binary (as the first parameter). }
  function _injectIn(_start, _end : DIterator; const obj : DObject; binary : DBinary) : DObject;

  {** Call binary with obj as the first parameter and the first item in the
  container as the next parameter.  Then call binary with the result of the
  first call to binary as the first parameter, and the second object in the
  container as the second parameter.  Keep going.  Return the last return value
  of binary.  If no items are in the container, return obj. }
  function inject(container : DContainer; const obj : array of const; binary : DBinary) : DObject;

  {** Call binary with obj as the first parameter and the items in the range
  as the second parameter.  Pass the result of each call to binary to the next
  call to binary (as the first parameter). }
  function injectIn(_start, _end : DIterator; const obj : array of const; binary : DBinary) : DObject;

  ////////////////////////////////////////////////////////////////////
  //
  // Comparing
  //
  ////////////////////////////////////////////////////////////////////

  {** Tests to see if two containers are equal, testing each object with
  their comparators. }
  function equal(con1, con2 : DContainer) : Boolean; //{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Tests to see if two ranges contain equal objects, according to the
  comparators associated with the containers associated with the iterators. }
  function equalIn(start1, end1, start2 : DIterator) : Boolean;

  {** LexicographicalCompare compares two sequences by comparing their items,
  in order, one by one.  If an item in con1 is found that is different from
  that in con2, the result of the comparison is returned.
  @param con1 The first container to use for comparison.
  @param con2 The second container to use for comparison. }
  function lexicographicalCompare(con1, con2 : DContainer) : Integer;

  {** Lexicographically compare (item by item compare) using a supplied
  comparator.
  @param con1 The first container.
  @param con2 The second container.
  @param compare The comparator to use. }
  function lexicographicalCompareWith(con1, con2 : DContainer; compare : DComparator) : Integer;

  {** Lexicographically compare (item by item compare) within the specified
  range. }
  function lexicographicalCompareIn(start1, end1, start2, end2 : DIterator) : Integer;

  {** Lexicographically compare (item by item compare) within the specified
  range, using the specified comparator. }
  function lexicographicalCompareInWith(start1, end1, start2, end2 : DIterator; compare : DCOmparator) : Integer;

  {** Finds the median of three objects using the given comparator. }
  function _median(const obj1, obj2, obj3 : DObject; compare : DComparator) : DObject;
  function median(const objs : array of const; compare : DComparator) : DObject;

  {** Determines the point at which con1 and con2 begin to differ.  It
  returns a pair of iterators positioned at the appropriate place in
  both containers.  If no mismatch is found, both iterators will be at
  the end. }
  function mismatch(con1, con2 : DContainer) : DIteratorPair;

  {** Determines the point at which con1 and con2 differ, using bt as
  a binary test. The function will halt when bt returns false. }
  function mismatchWith(con1, con2 : DContainer; bt : DBinaryTest) : DIteratorPair;

  {** Tests two ranges to determine at which point they begin to
  differ.  The comparators associated with the iterators will be
  used for the test. }
  function mismatchIn(start1, end1, start2 : DIterator) : DIteratorPair;

  {** Determines the point at which the two ranges differ, using bt
  as a test.  The function will stop when bt returns false.  An iterator
  pair will be returned, positioned at the first mismatched element.
  If no mismatch is found, both iterators will be positioned at the end. }
  function mismatchInWith(start1, end1, start2 : DIterator; bt : DBinaryTest) : DIteratorPair;

  ////////////////////////////////////////////////////////////////////
  //
  // Copying
  //
  ////////////////////////////////////////////////////////////////////

  {** Copies the contents of source to dest. }
  function copyContainer(source, dest : DContainer) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Copies the contents of con1 to the given iterator.  }
  function copyTo(source : DContainer; dest : DIterator) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Copies a range of values to the given iterator. }
  function copyInTo(_start, _end, dest : DIterator) : DIterator;

  {** Copies a reversed range of values to the given iterator. }
  function copyBackward(_start, _end, dest : DIterator) : DIterator;

  ////////////////////////////////////////////////////////////////////
  //
  // Counting
  //
  ////////////////////////////////////////////////////////////////////

  {** Counts the number of times items matching the given obj appear in the
  container.  You may often use the count version instead of this version.
  @param con1 The container to count in.
  @param obj The object to count.}
  function _count(con1 : DContainer; const obj : DObject) : Integer; //{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Counts the number of times items matching the given obj appear in a given
  range.  You may often use the countIn version instead of this version.
  @param _start The start of the range
  @param _end The end of the range
  @param obj The object to count}
  function _countIn(_start, _end : DIterator; const obj : DObject) : Integer;

  {** Counts the number of times items matching objs exist in the container.
  If more than one object is passed in, count sums for all objects.
  @param con1 The counter to count in.
  @param objs An open array of objects to count (counts for all will be summed)}
  function count(con1 : DContainer; const objs : array of const) : Integer;

  {** Counts the number of times items matching objs exist in the given range.
  If more than one object is passed in, count sums for all objects.
  @param _start The start of the range
  @param _end The end of the range
  @param objs An open array of objects to count (counts will be summed)}
  function countIn(_start, _end : DIterator; const objs : array of const) : Integer;

  {** Counts the number of times the test returns true.  Each item in the
  container is passed to the test.
  @param con1 The container to test in
  @param test The DTest to apply to each object in the container}
  function countIf(con1 : DContainer; test : DTest) : Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Counts the number of times the test returns true.  Each item in the
  specified range is passed to the test.
  @param _start The start of the range
  @param _end The end of the range
  @param test The test to apply.}
  function countIfIn(_start, _end : DIterator; test : DTest) : Integer;

  ////////////////////////////////////////////////////////////////////
  //
  // Filling
  //
  ////////////////////////////////////////////////////////////////////

  {** Fill the given container with the specified value. }
  procedure _fill(con : DContainer; const obj : DObject); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Fill a container with N copies of a value.  This will expand
  the container if necessary.}
  procedure _fillN(con : DContainer; count : Integer; const obj : DObject);

  {** Fill the specified range with a value.}
  procedure _fillIn(_start, _end : DIterator; const obj : DObject);

  {** Fill the container with a given value. }
  procedure fill(con : DContainer; const obj : array of const);

  {** Fill a container with values, expanding if necessary. }
  procedure fillN(con : DContainer; count : Integer; const obj : array of const);

  {** Fill the range with a value. }
  procedure fillIn(_start, _end : DIterator; const obj : array of const);

  {** Fill a container using a generator. Executes count times.  The objects
  generated are added to the container with Add. }
  procedure generate(con : DContainer; count : Integer; gen : DGenerator);

  {** Fills a range using a generator. }
  procedure generateIn(_start, _end : DIterator; gen : DGenerator);

  {** Outputs count objects from a generator to the destination iterator,
  which is called by output. }
  procedure generateTo(dest : DIterator; count : Integer; gen : DGenerator);


  ////////////////////////////////////////////////////////////////////
  //
  // Filtering
  //
  ////////////////////////////////////////////////////////////////////

  {** Removes duplicate values from a container, replacing them with a
  single instance. Empty items will have undefined values. }
  function unique(con : DContainer) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Removes duplicate values from the range, replacing them with a
  single instance.  The empty items will have undefined values. }
  function uniqueIn(_start, _end : DIterator) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Removes duplicate values, as defined by the given comparator return true. }
  function uniqueWith(con : DContainer; compare : DBinaryTest) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Removes duplicate values in a range, as defined by comparator returning true. }
  function uniqueInWith(_start, _end : DIterator; compare : DBinaryTest) : DIterator;

  {** Copies a container to a destination, removing duplicates. }
  function uniqueTo(con : DContainer; dest : DIterator) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Copies a range to a destination, removing duplicates. }
  function uniqueInTo(_start, _end, dest : DIterator) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Copy all values to a destination, removing duplicates. }
  function uniqueInWithTo(_start, _end, dest : DIterator; compare : DBinaryTest) : DIterator;

  {** Copy values for which test returns true to the destination. }
  procedure Filter(fromCon, toCon : DContainer; test : DTest); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Copy values for which test returns true to the destination. }
  function FilterTo(con : DContainer; dest : DIterator; test : DTest) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Copy values for which test returns true to the destination. }
  function FilterInTo(_start, _end, dest : DIterator; test : DTest) : DIterator;

  ////////////////////////////////////////////////////////////////////
  //
  // Finding
  //
  ////////////////////////////////////////////////////////////////////

  {** Locates the first pair of consecutive equal items in the container.
  Returns an iterator posisitioned on the first item.  Returns atEnd if
  no such pair is found. }
  function adjacentFind(container : DContainer) : DIterator;

  {** Locates the first pair of consecutive equal items in the container, using the compare specified.
  Returns an iterator posisitioned on the first item.  Returns atEnd if
  no such pair is found. }
  function adjacentFindWith(container : DContainer; compare : DBinaryTest) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Locates the first pair of consecutive equal items in the range.
  Returns an iterator posisitioned on the first item.  Returns _end if
  no such pair is found. }
  function adjacentFindIn(_start, _end : DIterator) : DIterator;

  {** Locates the first pair of consecutive equal items in the range, using the compare specified.
  Returns an iterator posisitioned on the first item.  Returns _end if
  no such pair is found. }
  function adjacentFindInWith(_start, _end : DIterator; compare : DBinaryTest) : DIterator;

  function _binarySearch(con : DContainer; const obj : DObject) : DIterator;
  function _binarySearchIn(_start, _end : DIterator; const obj : DObject) : DIterator;
  function _binarySearchWith(con : DContainer; compare : DComparator; const obj : DObject) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function _binarySearchInWith(_start, _end : DIterator; compare : DComparator; const obj : DObject) : DIterator;

  {** Locates an object in a sorted container using a binary search.  Returns
  an iterator positioned at the item if found.  Returns atEnd if not found. }
  function binarySearch(con : DContainer; const obj : array of const) : DIterator;

  {** Locates an object in a sorted range using a binary search.  Returns
  an iterator positioned at the item if found.  Returns atEnd if not found. }
  function binarySearchIn(_start, _end : DIterator; const obj : array of const) : DIterator;

  {** Locates an object in a sorted container using a binary search and a specified comparator.  Returns
  an iterator positioned at the item if found.  Returns atEnd if not found. }
  function binarySearchWith(con : DContainer; compare : DComparator; const obj : array of const) : DIterator;

  {** Locates an object in a sorted range using a binary search and a specified comparator.  Returns
  an iterator positioned at the item if found.  Returns atEnd if not found. }
  function binarySearchInWith(_start, _end : DIterator; compare : DComparator; const obj : array of const) : DIterator;

  {** Determines the first item for which compare returns true.
  @param container The container whose items should be tested.
  @param compare The test to try. }
  function detectWith(container : DContainer; compare : DTest) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Determines the first item in the range for which the compare returns true. }
  function detectInWith(_start, _end : DIterator; compare : DTest) : DIterator;

  {** Determines if the test returns true for every element in the container. }
  function every(container : DContainer; test : DTest) : Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Determines if the test returns true for every element in the range. }
  function everyIn(_start, _end : DIterator; test : DTest) : Boolean;

  {** Locates the given object in the container, returning an iterator positioned
  at it.  If the object is not found, the iterator is atEnd. }
  function _find(container : DContainer; const obj : DObject) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Locate the object in the given range, returning an iterator positioned
  at it.  If not found, the iterator equals _end. }
  function _findIn(_start, _end : DIterator; const obj : DObject) : DIterator;

  {** Locates an item in a container.  Returns atEnd if the item is not found. }
  function find(container : DContainer; const objs : array of const) : DIterator;

  {** Locates an item in a range.  Returns _end if the item is not found. }
  function findIn(_start, _end : DIterator; const objs : array of const) : DIterator;

  {** Find the first item in a container for which test returns true.  Returns
  atEnd if no such item is found. }
  function findIf(container : DContainer; test : DTest) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Find the first item in a range for which test returns true.  Returns
  _end if no such item is found. }
  function findIfIn(_start, _end : DIterator; test : DTest) : DIterator;

  {** Determines if any of the items in the container return true, when passed
  to test. }
  function some(container : DContainer; test : DTest) : Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Determines if any of the items in the range cause test to return true. }
  function someIn(_start, _end : DIterator; test : DTest) : Boolean;

  ////////////////////////////////////////////////////////////////////
  //
  // Freeing and Deleting
  //
  ////////////////////////////////////////////////////////////////////

  {** Calls TObject.Free on all the objects in the container.  Asserts if
  the type of an item is not TObject. }
  procedure objFree(container : DContainer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Calls TObject.Free on all the items in the range. }
  procedure objFreeIn(_start, _end : DIterator);

  {** Calls FreeMem on all the items in the container.  Assumes the items
  are pointers to memory allocated with GetMem. }
  procedure objDispose(container : DContainer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Calls FreeMem on all the items in the range.  Assumes the items
  are pointers to memory allocated with GetMem. }
  procedure objDisposeIn(_start, _end : DIterator);

  {** Calls TObject.Free on all the items in the container.  Since maps
  store pairs, this calls free on the KEY part of the pair. }
  procedure objFreeKeys(assoc : DAssociative);

  ////////////////////////////////////////////////////////////////////
  //
  // Hashing
  //
  ////////////////////////////////////////////////////////////////////
  {** Hashes all the items in the container.  The order or the items
  is significant. }
  function orderedHash(container : DContainer) : Integer; //{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Hashes all the items in the range.  The order or the items
  is significant. }
  function orderedHashIn(_start, _end : DIterator) : Integer;

  {** Hashes all the items in the container.  The order or the items
  is not significant. }
  function unorderedHash(container : DContainer) : Integer; //{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Hashes all the items in the range.  The order or the items
  is not significant. }
  function unorderedHashIn(_start, _end : DIterator) : Integer;

  ////////////////////////////////////////////////////////////////////
  //
  // Permuting
  //
  ////////////////////////////////////////////////////////////////////
  {** Not implemented. }
  function nextPermutation(container : DContainer; comparator : DComparator) : Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Not implemented. }
  function nextPermutationIn(_start, _end : DIterator; comparator : DComparator) : Boolean;
  {** Not implemented. }
  function prevPermutation(container : DContainer; comparator : DComparator) : Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  {** Not implemented. }
  function prevPermutationIn(_start, _end : DIterator; comparator : DComparator) : Boolean;

  ////////////////////////////////////////////////////////////////////
  //
  // Removing
  //
  ////////////////////////////////////////////////////////////////////
  function _remove(container : DContainer; const obj : DObject) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function _removeIn(_start, _end : DIterator; const obj : DObject) : DIterator;
  function _removeTo(container : DContainer; dest : DIterator; const obj : DObject) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function _removeInTo(_start, _end, dest : DIterator; const obj : DObject) : DIterator;
  function _removeCopy(source, destination : DContainer; const obj : DObject) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function _removeCopyTo(source : DContainer; dest : DIterator; const obj : DObject) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function _removeCopyIn(_start, _end, dest : DIterator; const obj : DObject) : DIterator;

  {** Removes all instances of each item in objs from the container. }
  function remove(container : DContainer; const objs : array of const) : DIterator;

  {** Removes all instances of each item in objs in the range. }
  function removeIn(_start, _end : DIterator; const objs : array of const) : DIterator;

  {** Removes all instances of each item in objs from the container, copying
  the removed items to dest. }
  function removeTo(container : DContainer; dest : DIterator; const objs : array of const) : DIterator;

  {** Removes all instances of each item in objs from the range, copying
  the removed items to dest. }
  function removeInTo(_start, _end, dest : DIterator; const objs : array of const) : DIterator;

  {** Copies the source container to the destination, except for the items
  matching objs. }
  function removeCopy(source, destination : DContainer; const objs : array of const) : DIterator;

  {** Copies the source container to a destination iterator, except for the items
  matching objs. }
  function removeCopyTo(source : DContainer; dest : DIterator; const objs : array of const) : DIterator;

  {** Copies the source range to the destination, except for the items
  matching objs. }
  function removeCopyIn(_start, _end, dest : DIterator; const objs : array of const) : DIterator;

  {** Copies the source container to the destination, except for those items
  for which test returns true. }
  function removeCopyIf(source, destination : DContainer; test : DTest) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Copies the source container to the destination, except for those items
  for which test returns true. }
  function removeCopyIfTo(source : DContainer; dest : DIterator; test : DTest) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Copies the source range to the destination, except for those items
  for which test returns true. }
  function removeCopyIfIn(_start, _end, dest : DIterator; test : DTest) : DIterator;

  {** Removes items from the container for which test returns true. }
  function removeIf(container : DContainer; test : DTest) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Removes items from the range for which test returns true. The extra elements
  left after removal will have undefined values. }
  function removeIfIn(_start, _end : DIterator; test : DTest) : DIterator;

  {** Removes items from the container, where that item returns true from test.
  The removed items are sent to the destination. A pair of iterators is
  returned: the first is positioned after the last element remaining in the
  container.  The second is positioned after the last element written to dest.}
  function removeIfTo(container : DContainer; dest : DIterator; test : DTest) : DIteratorPair; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Removes items from the range, where that item returns true from test.
  The removed items are sent to the destination. A pair of iterators is
  returned.  The first is positioned at the last element remaining in the
  _start, _end range.  The second is positioned after the last element
  written to dest.}
  function removeIfInTo(_start, _end, dest : DIterator; test : DTest) : DIteratorPair;

  ////////////////////////////////////////////////////////////////////
  //
  // Replacing
  //
  ////////////////////////////////////////////////////////////////////
  function _replace(container : DContainer; const obj1, obj2 : DObject) : Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function _replaceIn(_start, _end : DIterator; const obj1, obj2 : DObject) : Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function _replaceCopy(con1, con2 : DContainer; const obj1, obj2 : DObject) : Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function _replaceCopyTo(container : DContainer; dest : DIterator; const obj1, obj2 : DObject) : Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function _replaceCopyInTo(_start, _end, dest : DIterator; const obj1, obj2 : DObject) : Integer;
  function _replaceCopyIf(con1, con2 : DContainer; test : DTest; const obj : DObject) : Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function _replaceCopyIfTo(container : DContainer; dest : DIterator; test : DTest; const obj : DObject) : Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function _replaceCopyIfInTo(_start, _end, dest : DIterator; test : DTest; const obj : DObject) : Integer;
  function _replaceIf(container : DContainer; test : DTest; const obj : DObject) : Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function _replaceIfIn(_start, _end : DIterator; test : DTest; const obj : DObject) : Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** For each pair of items objs1[N] and objs2[N], replace all instances of
  objs1[N] with objs2[N] in the given container.  Returns the number of
  replacements performed. }
  function replace(container : DContainer; const objs1, objs2 : array of const) : Integer;

  {** For each pair of items objs1[N] and objs2[N], replace all instances of
  objs1[N] with objs2[N] in the given range.  Returns the number of
  replacements performed. }
  function replaceIn(_start, _end : DIterator; const objs1, objs2 : array of const) : Integer;

  {** For each pair of items objs1[N] and objs2[N], copies each item in con1 to con2,
  except if the item equals objs1[N], where objs2[N] is copied.  Returns the number of
  replacements performed. }
  function replaceCopy(con1, con2 : DContainer; const objs1, objs2 : array of const) : Integer;

  {** For each pair of items objs1[N] and objs2[N], copies each item in con1 to the destination,
  except if the item equals objs1[N], where objs2[N] is copied.  Returns the number of
  replacements performed. }
  function replaceCopyTo(container : DContainer; dest : DIterator; const objs1, objs2 : array of const) : Integer;

  {** For each pair of items objs1[N] and objs2[N], copies each item in the range to the destination,
  except if the item equals objs1[N], where objs2[N] is copied.  Returns the number of
  replacements performed. }
  function replaceCopyInTo(_start, _end, dest : DIterator; const objs1, objs2 : array of const) : Integer;

  {** Copies the items in con1 to con2, except for those items in con1 who
  return true from the test.  In that case objs[N] will be subsituted.  The
  operation is repeated for each objs[N]. }
  function replaceCopyIf(con1, con2 : DContainer; test : DTest; const objs : array of const) : Integer;

  {** Copies the items in con1 to the destination, except for those items in con1 who
  return true from the test.  In that case objs[N] will be subsituted.  The
  operation is repeated for each objs[N]. }
  function replaceCopyIfTo(container : DContainer; dest : DIterator; test : DTest; const objs : array of const) : Integer;

  {** Copies the items in the range to the destination, except for those items in con1 who
  return true from the test.  In that case objs[N] will be subsituted.  The
  operation is repeated for each objs[N]. }
  function replaceCopyIfInTo(_start, _end, dest : DIterator; test : DTest; const objs : array of const) : Integer;

  {** For each objs[N], replace all items in the container for which test returns
  true with objs[N]. }
  function replaceIf(container : DContainer; test : DTest; const objs : array of const) : Integer;

  {** For each objs[N], replace all items in the range for which test returns
  true with objs[N]. }
  function replaceIfIn(_start, _end : DIterator; test : DTest; const objs : array of const) : Integer;

  ////////////////////////////////////////////////////////////////////
  //
  // Reversing
  //
  ////////////////////////////////////////////////////////////////////

  {** Reverse the order of the items in the container. }
  procedure reverse(container : DContainer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Reverse the order of the items in the range. }
  procedure reverseIn(_start, _end : DIterator);

  {** Copy the items in con1 to con2, in reverse order. }
  procedure reverseCopy(con1, con2 : DContainer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Copy the items in container to the output iterator, in reverse
  order. }
  procedure reverseCopyTo(container : DContainer; dest : DIterator); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Copy the items in the range to the output iterator, in reverse
  order. }
  procedure reverseCopyInTo(_start, _end, dest : DIterator);

  ////////////////////////////////////////////////////////////////////
  //
  // Rotating
  //
  ////////////////////////////////////////////////////////////////////

  {** Rotate the items between the iterators such that the item at the
  first iterator moves to the middle iterator.  Items pushed off the
  end will move to the beginning of the range.  This is a right rotation. }
  procedure rotate(first, middle, last : DIterator);

  {** Rotate the items between the iterators such that the item at the
  first iterator moves to the middle iterator.  Items pushed off the
  end will move to the beginning of the range.  This is a right rotation.
  The items at the original iterators are not altered.  The results of
  this operation are copied to the output iterator. }
  function rotateCopy(first, middle, last, dest : DIterator) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  ////////////////////////////////////////////////////////////////////
  //
  // Sets
  //
  ////////////////////////////////////////////////////////////////////

  {** Determine if master contains all the items in subset.  Requires that
  both containers be sorted. }
  function includes(master, subset : DContainer) : Boolean;

  {** Determines if master contains all the items in subset, comparing
  items with the supplied comparator. Requires that both containers
  be sorted. }
  function includesWith(master, subset : DContainer; comparator : DComparator) : Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Determine if the master range contains all the items in subset range.  Requires that
  both containers be sorted. }
  function includesIn(startMaster, finishMaster, startSubset, finishSubset : DIterator) : Boolean;

  {** Determines if master range contains all the items in subset range, comparing
  items with the supplied comparator. Requires that both containers
  be sorted. }
  function includesInWith(startMaster, finishMaster, startSubset, finishSubset : DIterator; comparator : DComparator) : Boolean;

  {** Finds the set of items that are in con1 but not in con2, and writes
  that set to the destination iterator.  Assumes that both containers are
  sorted. An iterator positioned just after
  the last written item is returned.}
  function setDifference(con1, con2 : DContainer; dest : DIterator) : DIterator;

  {** Finds the set of items that are in range 1 but not in range 2, and writes
  that set to the destination iterator.  Assumes that both ranges are
  sorted. An iterator positioned just after
  the last written item is returned.}
  function setDifferenceIn(start1, finish1, start2, finish2, dest : DIterator) : DIterator;

  {** Finds the set of items that are in con1 but not in con2, using comparator to compare items, and writes
  that set to the destination iterator.  Assumes that both containers are
  sorted. An iterator positioned just after
  the last written item is returned.}
  function setDifferenceWith(con1, con2 : DContainer; dest : DIterator; comparator : DComparator) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Finds the set of items that are in range 1 but not in range 2, using comparator to test them, and writes
  that set to the destination iterator.  Assumes that both ranges are
  sorted. An iterator positioned just after
  the last written item is returned.}
  function setDifferenceInWith(start1, finish1, start2, finish2, dest : DIterator; comparator : DComparator) : DIterator;

  {** Finds the set of items that are in both con1 and con2, and writes
  that set to the destination iterator.  Assumes that both containers are
  sorted. An iterator positioned just after
  the last written item is returned.}
  function setIntersection(con1, con2 : DContainer; dest : DIterator) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Finds the set of items that are in both range 1 and in range 2, and writes
  that set to the destination iterator.  Assumes that both ranges are
  sorted. An iterator positioned just after
  the last written item is returned.}
  function setIntersectionIn(start1, finish1, start2, finish2, dest : DIterator) : DIterator;

  {** Finds the set of items that are in both con1 and con2, using comparator to compare items, and writes
  that set to the destination iterator.  Assumes that both containers are
  sorted. An iterator positioned just after
  the last written item is returned.}
  function setIntersectionWith(con1, con2 : DContainer; dest : DIterator; comparator : DComparator) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Finds the set of items that are in both range 1 and range 2, using comparator to test them, and writes
  that set to the destination iterator.  Assumes that both ranges are
  sorted. An iterator positioned just after
  the last written item is returned.}
  function setIntersectionInWith(start1, finish1, start2, finish2, dest : DIterator; comparator : DComparator) : DIterator;


  {** Finds the set of items that are NOT in both sets.  That is, it finds the items
  that are only in con1, or only in con2.  It writes these items to the
  destination iterator.  Assumes that both containers are sorted. An iterator positioned just after
  the last written item is returned.}
  function setSymmetricDifference(con1, con2 : DContainer; dest : DIterator) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Finds the set of items that are NOT in both sets.  That is, it finds the items
  that are only in range 1, or only in range 2.  It writes these items to the
  destination iterator.  Assumes that both ranges are sorted. An iterator positioned just after
  the last written item is returned.}
  function setSymmetricDifferenceIn(start1, finish1, start2, finish2, dest : DIterator) : DIterator;

  {** Finds the set of items that are NOT in both sets, using the specified comparator.  That is, it finds the items
  that are only in con1, or only in con2.  It writes these items to the
  destination iterator.  Assumes that both containers are sorted. An iterator positioned just after
  the last written item is returned.}
  function setSymmetricDifferenceWith(con1, con2 : DContainer; dest : DIterator; comparator : DComparator) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Finds the set of items that are NOT in both sets, using the specified comparator.  That is, it finds the items
  that are only in range 1, or only in range 2.  It writes these items to the
  destination iterator.  Assumes that both ranges are sorted. An iterator positioned just after
  the last written item is returned.}
  function setSymmetricDifferenceInWith(start1, finish1, start2, finish2, dest : DIterator; comparator : DComparator) : DIterator;

  {** Finds the set of items that are in both con1 and con2.  Note that only
  one copy of each item will show up (a unique operation).  The results are
  written to the destination iterator.  An iterator positioned just after
  the last written item is returned. }
  function setUnion(con1, con2 : DContainer; dest : DIterator) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Finds the set of items that are in both range 1 and range 2.  Note that only
  one copy of each item will show up (a unique operation).  The results are
  written to the destination iterator.  An iterator positioned just after
  the last written item is returned. }
  function setUnionIn(start1, finish1, start2, finish2, dest : DIterator) : DIterator;

  {** Finds the set of items that are in both con1 and con2, using the specified comparator.  Note that only
  one copy of each item will show up (a unique operation).  The results are
  written to the destination iterator.  An iterator positioned just after
  the last written item is returned. }
  function setUnionWith(con1, con2 : DContainer; dest : DIterator; comparator : DComparator) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Finds the set of items that are in both range 1 and range 2, using the specified comparator.  Note that only
  one copy of each item will show up (a unique operation).  The results are
  written to the destination iterator.  An iterator positioned just after
  the last written item is returned. }
  function setUnionInWith(start1, finish1, start2, finish2, dest : DIterator; comparator : DComparator) : DIterator;

  ////////////////////////////////////////////////////////////////////
  //
  // Shuffling
  //
  ////////////////////////////////////////////////////////////////////

  {** Shuffles the items in the container, just like shuffling a deck
  of cards. }
  procedure randomShuffle(container : DContainer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Shuffles the items in the range, just like shuffling a deck
  of cards. }
  procedure randomShuffleIn(_start, _end : DIterator);

  ////////////////////////////////////////////////////////////////////
  //
  // Sorting
  //
  ////////////////////////////////////////////////////////////////////

  {** Sorts the items in the sequence with QuickSort.}
  procedure sort(sequence : DSequence); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Sorts the items in the range with QuickSort. }
  procedure sortIn(_start, _end : DIterator);

  {** Sorts the items in the sequence with QuickSort, using the specified
  comparator to control sorting. }
  procedure sortWith(sequence : DSequence; comparator : DComparator); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Sorts the items in the range with QuickSort, using the specified
  comparator to control sorting. }
  procedure sortInWith(_start, _end : DIterator; comparator : DComparator);

  {** Sorts the items in the sequence with MergeSort.}
  procedure stablesort(sequence : DSequence); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Sorts the items in the range with MergeSort. }
  procedure stablesortIn(_start, _end : DIterator);

  {** Sorts the items in the sequence with MergeSort, using the specified
  comparator to control sorting. }
  procedure stablesortWith(sequence : DSequence; comparator : DComparator); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Sorts the items in the range with MergeSort, using the specified
  comparator to control sorting. }
  procedure stablesortInWith(_start, _end : DIterator; comparator : DComparator);

  ////////////////////////////////////////////////////////////////////
  //
  // Swapping
  //
  ////////////////////////////////////////////////////////////////////

  {** Swaps the items the two iterators are positioned at. }
  procedure iterSwap(iter1, iter2 : DIterator);

  {** Swaps the items in the two containers. }
  procedure swapRanges(con1, con2 : DContainer); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Swaps the items in two ranges.  }
  procedure swaprangesInTo(start1, end1, start2 : DIterator);

  ////////////////////////////////////////////////////////////////////
  //
  // Transforming
  //
  ////////////////////////////////////////////////////////////////////

  {** Collect applies the unary function to each object in the container,
  storing the results in a new container (that is constructed by the function)
  that is of the same type as the existing one.
  @param container The container to operator on.
  @param unary The function to apply }
  function collect(container : DContainer; unary : DUnary) : DContainer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** CollectIn applies the unary function to each object in the given range,
  storing the results in a new container (that is constructed by this function)
  that is of the same type as the starting iterator's associated container.
  @param _start The beginning of the range to collect in.
  @param _end The end of the range.
  @param unary The function to apply. }
  function collectIn(_start, _end : DIterator; unary : DUnary) : DContainer;

  {** TransformBinary applies a binary function to pairs of objects from
  con1 and con2, and stores the result into the output container.  con1 and
  con2 need to have the same number of objects in them.
  @param con1 The container of objects that will be passed as the first parameter to the binary function.
  @param con2 The container of objects that will be passed as the second parameter.
  @param output The container where the results will go.
  @param binary The binary function to be applied. }
  procedure transformBinary(con1, con2, output : DContainer; binary : DBinary); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** TransformBinaryTo applies a binary function to pairs of objects from
  con1 and con2, and stores the result at the output iterator.  con1 and
  con2 need to have the same number of objects in them.
  @param con1 The container of objects that will be passed as the first parameter to the binary function.
  @param con2 The container of objects that will be passed as the second parameter.
  @param output The iterator where the results will go.
  @param binary The binary function to be applied. }
  function transformBinaryTo(con1, con2 : DContainer; output : DIterator; binary : DBinary) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** TransformBinaryInTo applies a binary function to pairs of objects.  The first
  object is taken from the range starting at start1 and ending at finish1.  The
  second object is taken from the range starting at start2.  The results of the
  applied function are put at output.
  @param start1 The start of the range for the first object.
  @param finish1 The end of the range for the first object.  Note that this
                 parameter dictates how many objects will be processed.
  @param output The container where the results will go.
  @param binary The binary function to be applied. }
  function transformBinaryInTo(start1, finish1, start2, output : DIterator; binary : DBinary) : DIterator;

  {** TransformUnary applies a function to each object in a container, storing
  the results in an output container.
  @param container The container of objects to apply the function to.
  @param output The container where the results of the function will go.
  @param unary The function to apply. }
  procedure transformUnary(container, output : DContainer; unary : DUnary); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** TransformUnary applies a function to each object in a container, storing
  the results at an output iterator.
  @param container The container of objects to apply the function to.
  @param output The iterator where the results of the function will go.
  @param unary The function to apply. }
  function transformUnaryTo(container : DContainer; output : DIterator; unary : DUnary) : DIterator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** TransformUnaryInTo applies a unary function to each object in a range,
  storing the results at another iterator.
  @param _start The start of the range of objects.
  @param _finish The end of the range of objects.
  @param output Where the results of the function will go.
  @param unary The function to apply. }
  function transformunaryInTo(_start, _finish, output : DIterator; unary : DUnary) : DIterator;


  ////////////////////////////////////////////////////////////////////
  //
  // Utilities
  //
  ////////////////////////////////////////////////////////////////////

  function MakePair(const ob1, ob2 : DObject) : DPair;
  function MakeRange(s,f : DIterator) : DRange;
  function hashCode(const obj : DObject) : Integer;

  {** Free each of the objects passed in. }
  procedure FreeAll(const objs : array of TObject);

  {** Free the contents of the container (which must be objects).  Then
  free the container, and set the pointer to nil. }
  procedure FreeAndClear(var container);

  function JenkinsHashInteger(v : DeCALDWORD) : Integer;
  function JenkinsHashBuffer(const buffer; length : Integer; initVal : Integer) : Integer;
  {$IFDEF UNICODE}
  function JenkinsHashString(const s : string) : Integer; overload;
  {$ENDIF UNICODE}
  function JenkinsHashString(const s : AnsiString) : Integer; overload;
  function JenkinsHashWideString(const s: WideString) : Integer; 
  function JenkinsHashSingle(s : Single) : Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
  function JenkinsHashDouble(d : Double) : Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Make sure a DObject is empty.  Does not clear out the previous contents.
  Call this when you're creating a DObject (like on the stack) and you're not
  sure if the memory is clear or not. }
  procedure InitDObject(var obj : DObject); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Creates a copy of the DObject. }
  procedure CopyDObject(const source : DObject; var dest : DObject);

  {** Moves a DObject; the original is left empty. }
  procedure MoveDObject(var source, dest : DObject); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Clears an existing DObject, freeing memory if need be.  Do not call
  this on uninitialized memory -- prepare that kind of memory with InitDObject. }
  procedure ClearDObject(var obj : DObject);

  {** Sets an arbitrary value to a DObject.  The destination is assumed to be
  an already-initialized DObject.  It is cleared before the new value is
  written. }
  procedure SetDObject(var obj : DObject; const value : array of const);

  {** Sets an arbitrary dobject to another, clearing the destination's value
  first. }
  procedure _SetDObject(var destination : DObject; const src : DObject);

  {** Swap two dobjects. }
  procedure Swap(var obj1, obj2 : DObject);

  {** Retrieve the type of the DObject an iterator is positioned on. }
  function getVType(const iterator : DIterator) : Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Turn a procedure into a closure, suitable for use as a comparator. }
  function MakeComparatorEx(proc : DComparatorProc; ptr : Pointer) : DComparator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Turn a procedure into a closure, suitable for use as a DEquals. }
  function MakeEqualsEx(proc : DEqualsProc; ptr : Pointer) : DEquals; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Turn a procedure into a closure, suitable for use as a DTest. }
  function MakeTestEx(proc : DTestProc; ptr : Pointer) : DTest; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Turn a procedure into a closure, suitable for use as a DApply. }
  function MakeApplyEx(proc : DApplyProc; ptr : Pointer) : DApply; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Turn a procedure into a closure, suitable for use as a DUnary. }
  function MakeUnaryEx(proc : DUnaryProc; ptr : Pointer) : DUnary; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Turn a procedure into a closure, suitable for use as a DBinary. }
  function MakeBinaryEx(proc : DBinaryProc; ptr : Pointer) : DBinary; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Turn a procedure into a closure, suitable for use as a DHash. }
  function MakeHashEx(proc : DHashProc; ptr : Pointer) : DHash; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Turn a procedure into a closure, suitable for use as a DGenerator. }
  function MakeGeneratorEx(proc : DGeneratorProc; ptr : Pointer) : DGenerator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Turn a procedure into a closure, suitable for use as a comparator. }
  function MakeComparator(proc : DComparatorProc) : DComparator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Turn a procedure into a closure, suitable for use as a DEquals. }
  function MakeEquals(proc : DEqualsProc) : DEquals; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Turn a procedure into a closure, suitable for use as a DTest. }
  function MakeTest(proc : DTestProc) : DTest; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Turn a procedure into a closure, suitable for use as a DApply. }
  function MakeApply(proc : DApplyProc) : DApply; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Turn a procedure into a closure, suitable for use as a DUnary. }
  function MakeUnary(proc : DUnaryProc) : DUnary; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Turn a procedure into a closure, suitable for use as a DBinary. }
  function MakeBinary(proc : DBinaryProc) : DBinary; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Turn a procedure into a closure, suitable for use as a DHash. }
  function MakeHash(proc : DHashProc) : DHash; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

  {** Turn a procedure into a closure, suitable for use as a DGenerator. }
  function MakeGenerator(proc : DGeneratorProc) : DGenerator; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

type
  {** Signature for conversion functions that change objects into printable
  strings.  Create a function with this signature and pass it to
  RegisterDeCALPrinter. }
  DPrinterProc = function (obj : TObject) : string;

  {** Register printable string converters for your own classes. }
  procedure RegisterDeCALPrinter(cls : TClass; prt : DPrinterProc);

  {** Convert a DObject into a printable string representation. }
  function PrintString(const obj : DObject) : string;

  {** A simple printing function -- each obj is printed to the console.  Nice
  for debugging container contents. Suitable for passing to ForEach. }
  procedure ApplyPrint(ptr : Pointer; const obj : DObject);
  procedure ApplyPrintLN(ptr : Pointer; const obj : DObject);

//
// We need to bring a couple of things forward from the Delphi system unit.
//
const
  vtAnsiChar = vtChar;
  vtPAnsiChar = vtPChar;
  vtInt64 = 16;
  vtUnicodeString = 17;
  vtLastVType = vtUnicodeString;

implementation

uses
  {$IFDEF UNICODE}
  AnsiStrings,
  {$ENDIF UNICODE}
  DeCAL_FixedRecSort;

{$IFNDEF SUPPORTS_INLINE}
{type
  PInteger = ^Integer;}
{$ENDIF SUPPORTS_INLINE}

{$IFNDEF UNICODE}
type
  PByte = PAnsiChar;
{$ENDIF ~UNICODE}

type
  DeCALBasicType = vtInteger..vtLastVType;
  DeCALBasicTypes = set of DeCALBasicType;

var
  nil_node : DTreeNode = nil;
  emptyDObject : DObject;

{$IFNDEF DELPHI6_UP}
function DumbItDownFor95(const S1, S2: WideString): Integer;
var
  a1, a2: AnsiString;
begin
  a1 := s1;
  a2 := s2;
  Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    PChar(a1), Length(a1), PChar(a2), Length(a2)) - 2;
end;

function WideCompareText(const S1, S2: WideString): Integer;
begin
  SetLastError(0);
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    PWideChar(S1), Length(S1), PWideChar(S2), Length(S2)) - 2;
  case GetLastError of
    0: ;
    ERROR_CALL_NOT_IMPLEMENTED:
      Result := DumbItDownFor95(S1, S2);
  else
    RaiseLastWin32Error; 
  end;
end;
{$ENDIF ~DELPHI6_UP}


function DeCALAlloc(sz : Integer) : Pointer;
begin
{$IFDEF GC}
  Result := GC_malloc(sz);
{$ELSE}
  GetMem(result, sz);
{$ENDIF}
end;

procedure DeCALFree(ptr : Pointer);
begin
{$IFDEF GC}
  // Don't do a thing!
{$ELSE}
  FreeMem(ptr);
{$ENDIF}
end;

function DeCALRealloc(ptr : Pointer; sz : Integer) : Pointer;
begin
{$IFDEF GC}
  Result := GC_realloc(ptr, sz);
{$ELSE}
  Result := ptr;
  ReallocMem(result, sz);
{$ENDIF}
end;

type
  TPool = class(DBaseClass)
  protected
    FSize : Integer;

  public
    function Alloc : Pointer; virtual; abstract;
    procedure Free(ptr : Pointer); virtual; abstract;
    procedure collapse; virtual; abstract;

    constructor Create(elementSize : Integer; multiThreaded : Boolean);
  end;

  TNoPool = class(TPool)
  public
    function Alloc : Pointer; override;
    procedure Free(ptr : Pointer); override;
    procedure collapse; override;
  end;

  // We're going to define our own, because not everybody has the SyncObjs
  // unit.
  TDeCALCriticalSection = class
  private
    FSection: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
  end;

  TMemPool = class(TPool)
  protected
    FBuffers : TList;
    FCount : Integer;
    FCrit : TDeCALCriticalSection;
    FFreeHead : Pointer;

    procedure SetSize(sz : Integer);

  public
    function Alloc : Pointer; override;
    procedure Free(ptr : Pointer); override;
    procedure collapse; override;

    constructor Create(elementSize : Integer; multiThreaded : Boolean);
    destructor Destroy; override;
  end;

constructor TDeCALCriticalSection.Create;
begin
  inherited Create;
  InitializeCriticalSection(FSection);
end;

destructor TDeCALCriticalSection.Destroy;
begin
  DeleteCriticalSection(FSection);
  inherited Destroy;
end;

procedure TDeCALCriticalSection.Enter;
begin
  EnterCriticalSection(FSection);
end;

procedure TDeCALCriticalSection.Leave;
begin
  LeaveCriticalSection(FSection);
end;

constructor TPool.Create(elementSize : Integer; multiThreaded : Boolean);
begin
  FSize := elementSize;
  inherited Create;
end;

function TNoPool.Alloc : Pointer;
begin
  Result := DeCALAlloc(FSize);
end;

procedure TNoPool.Free(ptr : Pointer);
begin
  DeCALFree(ptr);
end;

procedure TNoPool.collapse;
begin
end;

{
  This memory pool co-locates its free list in the pool.
}
constructor TMemPool.Create(elementSize : Integer; multiThreaded : Boolean);
begin
  if elementSize < SizeOf(Pointer) then
    elementSize := SizeOf(Pointer);

  FBuffers := TList.Create;

  inherited Create(elementSize, multiThreaded);


  if multiThreaded then
    FCrit := TDeCALCriticalSection.Create;

  SetSize(128);
end;

procedure TMemPool.SetSize(sz : Integer);
type PPointer = ^Pointer;
var i : Integer;
    buffer, x,p : PByte;

begin
  if FCount = sz then
    Exit;

  if FCrit <> nil then
    FCrit.Enter;


  if sz > FCount then
    begin

      buffer := DeCALAlloc((sz - fcount) * FSize);
      FBuffers.add(buffer);

      // We're growing the pool.
      p := buffer;
      x := p;
      i := FCount;
      Dec(sz);
      while i < sz do
        begin
          PPointer(p)^ := p + FSize;
          Inc(i);
          Inc(p, FSize);
        end;
      PPointer(p)^ := FFreeHead;
      Inc(sz);
      FFreeHead := x;
      FCount := sz;
    end
  else
    begin
      // We're shrinking the pool.  That means a clear.
      for i := 0 to FBuffers.count - 1 do
        DeCALFree(FBuffers[i]);

      buffer := DeCALAlloc(sz * FSize);
      FBuffers.Add(buffer);
      p := buffer;

      i := 0;
      Dec(sz);
      while i < sz do
        begin
          PPointer(p)^ := p + FSize;
          Inc(i);
          Inc(p, FSize);
        end;
      PPointer(p)^ := nil;
      FFreeHead := buffer;
    end;

  if FCrit <> nil then
    FCrit.Leave;
end;

function TMemPool.Alloc : Pointer;
type PPointer = ^Pointer;
begin
  if FCrit <> nil then
    FCrit.Enter;

  // Need to expand the pool.
  if FFreeHead = nil then
    SetSize(FCount * 2);

  Result := FFreeHead;
  if result <> nil then
    FFreeHead := PPointer(result)^;

  if FCrit <> nil then
    FCrit.Leave;
end;

procedure TMemPool.Free(ptr : Pointer);
type PPointer = ^Pointer;
begin
  if FCrit <> nil then
    FCrit.Enter;

  PPointer(ptr)^ := FFreeHead;
  FFreeHead := ptr;

  if FCrit <> nil then
    FCrit.Leave;
end;

procedure TMemPool.collapse;
begin
  if FCrit <> nil then
    FCrit.Enter;

  raise DException.Create('Not supported');

  if FCrit <> nil then
    FCrit.Leave;
end;

destructor TMemPool.Destroy;
var i : Integer;
begin
  FCrit.Free;

  for i := 0 to FBuffers.Count - 1 do
    DeCALFree(FBuffers[i]);
  FBuffers.Free;

end;

function DeCALCreatePool(elementSize : Integer; multiThreaded : Boolean) : TPool;
begin
{$IFDEF GC}
  Result := TNoPool.Create(elementSize, multiThreaded);
{$ELSE}
  Result := TMemPool.Create(elementSize, multiThreaded);
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////
//
// Iterator Adapters
//
////////////////////////////////////////////////////////////////////
constructor DIterAdapter.Create(var target : DIterator);
begin
  FTarget := target.Handler;
  target.Handler := self;
end;

procedure DIterAdapter.iadvance(var iterator : DIterator);
begin
  FTarget.iadvance(iterator);
end;

function DIterAdapter.iremove(const iterator : DIterator) : DIterator;
begin
  Result := FTarget.iremove(iterator);
end;

function DIterAdapter.iget(const iterator : DIterator) : PDObject;
begin
  Result := FTarget.iget(iterator);
end;

function DIterAdapter.iequals(const iter1, iter2 : DIterator) : Boolean;
begin
  Result := FTarget.iequals(iter1, iter2);
end;

procedure DIterAdapter.iput(const iterator : DIterator; const obj : DObject);
begin
  FTarget.iput(iterator, obj);
end;

procedure DIterAdapter._iput(const iterator : DIterator; const objs : array of const);
begin
  FTarget._iput(iterator, objs);
end;

procedure DIterAdapter.iadvanceBy(var iterator : DIterator; count : Integer);
begin
  FTarget.iadvanceby(iterator, count);
end;

function DIterAdapter.iatStart(const iterator : DIterator) : Boolean;
begin
  Result := FTarget.iatStart(iterator);
end;

function DIterAdapter.iatEnd(const iterator : DIterator) : Boolean;
begin
  Result := FTarget.iatEnd(iterator);
end;

function DIterAdapter.igetContainer(const iterator : DIterator) : DContainer;
begin
  Result := FTarget.igetContainer(iterator);
end;

function DIterAdapter.idistance(const iter1, iter2 : DIterator) : Integer;
begin
  Result := FTarget.idistance(iter1, iter2);
end;

procedure DIterAdapter.iretreat(var iterator : DIterator);
begin
  FTarget.iretreat(iterator);
end;

procedure DIterAdapter.iretreatBy(var iterator : DIterator; count : Integer);
begin
  FTarget.iretreatBy(iterator, count);
end;

function DIterAdapter.igetAt(const iterator : DIterator; offset : Integer) : PDObject;
begin
  Result := FTarget.igetAt(iterator, offset);
end;

procedure DIterAdapter.iputAt(const iterator : DIterator; offset : Integer; const obj : DObject);
begin
  FTarget.iputAt(iterator, offset, obj);
end;

function DIterAdapter.iindex(const iterator : DIterator) : Integer;
begin
  Result := FTarget.iindex(iterator);
end;

function DIterAdapter.iless(const iter1, iter2 : DIterator) : Boolean;
begin
  Result := FTarget.iless(iter1, iter2);
end;

procedure DIterAdapter.iflagChange(var iterator : DIterator; oldflags : DIteratorFlags);
begin
  FTarget.iflagChange(iterator, oldflags);
end;

constructor DIterFilter.Create(var target : DIterator; test : DTest);
begin
  inherited Create(target);
  FTest := test;
end;

procedure DIterFilter.iadvance(var iterator : DIterator);
begin
  if not atEnd(iterator) then
    begin
      FTarget.iadvance(iterator);
      while (not atEnd(iterator)) and (not FTest(getRef(iterator)^)) do
        FTarget.iadvance(iterator);
    end;
end;

//
// TODO: How do we mark the beginning of the container?   This interation
// is not right yet.
//
procedure DIterFilter.iretreat(var iterator : DIterator);
begin
  if not atStart(iterator) then
    begin
      FTarget.iretreat(iterator);
      while (not atStart(iterator)) and (not FTest(getRef(iterator)^)) do
        FTarget.iretreat(iterator);
    end;
end;

constructor DIterSkipper.Create(var target : DIterator; skipBy : Integer);
begin
  inherited Create(target);
  FSkipBy := skipBy;
end;

procedure DIterSkipper.iadvance(var iterator : DIterator);
begin
  FTarget.iadvanceBy(iterator, FSkipBy);
end;

procedure DIterSkipper.iretreat(var iterator : DIterator);
begin
  FTarget.iretreatBy(iterator, FSkipBy);
end;

////////////////////////////////////////////////////////////////////
//
// Hashing
//
////////////////////////////////////////////////////////////////////

// The hashing code is a pascal translation of Jenkins' algorithm.

{$IFOPT Q+}
{#DEFINE TURNITOFF}
{$Q-}
{$ENDIF}

procedure JenkinsHashMix(var a,b,c : DeCALDWORD);
begin
  a := a-b;  a := a-c;  a := a xor (c shr 13);
  b := b-c;  b := b-a;  b := b xor (a shl 8);
  c := c-a;  c := c-b;  c := c xor (b shr 13);
  a := a-b;  a := a-c;  a := a xor (c shr 12);
  b := b-c;  b := b-a;  b := b xor (a shl 16);
  c := c-a;  c := c-b;  c := c xor (b shr 5);
  a := a-b;  a := a-c;  a := a xor (c shr 3);
  b := b-c;  b := b-a;  b := b xor (a shl 10);
  c := c-a;  c := c-b;  c := c xor (b shr 15);
end;

//
// We special case this one for speed.
//
function JenkinsHashInteger(v : DeCALDWORD) : Integer;
const golden = $9e3779b9;
var value, a,b,c : DeCALDWORD;
begin

  b := golden;
  c := 0;
  value := v;

  a := golden + ((value and $FF) shl 24);
  value := value shr 8;
  a := a + ((value and $FF) shl 16);
  value := value shr 8;
  a := a + ((value and $FF) shl 8);
  value := value shr 8;
  a := a + value;

  JenkinsHashMix(a,b,c);

  Result := abs(Integer(c));

end;

function JenkinsHashBuffer(const buffer; length : Integer; initVal : Integer) : Integer;
const golden = $9e3779b9;
type
  TByteArray = array[0..MaxInt-1] of Byte;
  PByteArray = ^TByteArray;
var a,b,c : DeCALDWORD;
    len : Integer;
    k : PByteArray;
begin

  len := length;
  a := golden;
  b := golden;
  c := initVal;

  k := PByteArray(@buffer);

  while len >= 12 do
    begin
      a := a+(k[0]+(DeCALDWORD(k[1]) shl 8)+(DeCALDWORD(k[2]) shl 16) +(DeCALDWORD(k[3]) shl 24));
      b := b+(k[4]+(DeCALDWORD(k[5]) shl 8)+(DeCALDWORD(k[6]) shl 16) +(DeCALDWORD(k[7]) shl 24));
      c := c+(k[8]+(DeCALDWORD(k[9]) shl 8)+(DeCALDWORD(k[10]) shl 16)+(DeCALDWORD(k[11]) shl 24));
      JenkinsHashMix(a,b,c);
      k := PByteArray(Integer(k)+12);
      len := len-12;
    end;

  //------------------------------------- handle the last 11 bytes */
  c := c + DeCALDWORD(length);


  if len >= 11 then
    c := c + (DeCALDWORD(k[10]) shl 24);
  if len >= 10 then
    c := c + (DeCALDWORD(k[9]) shl 16);
  if len >= 9 then
    c := c + (DeCALDWORD(k[8]) shl 8);
  if len >= 8 then
    b := b + (DeCALDWORD(k[7]) shl 24);
  if len >= 7 then
    b := b + (DeCALDWORD(k[6]) shl 16);
  if len >= 6 then
    b := b + (DeCALDWORD(k[5]) shl 8);
  if len >= 5 then
    b := b + k[4];
  if len >= 4 then
    a := a + (DeCALDWORD(k[3]) shl 24);
  if len >= 3 then
    a := a + (DeCALDWORD(k[2]) shl 16);
  if len >= 2 then
    a := a + (DeCALDWORD(k[1]) shl 8);
  if len >= 1 then
    a := a + k[0];

  JenkinsHashMix(a,b,c);

  Result := abs(c);
end;

function JenkinsHashWrapper(ptr : Pointer; const buffer; length : Integer) : Integer;
begin
  Result := JenkinsHashBuffer(buffer, length, 0);
end;

var
  NullStringHash : Integer = 0;

{$IFDEF UNICODE}
function JenkinsHashString(const s : string) : Integer;
begin
  if s <> '' then
    Result := JenkinsHashBuffer(PByte(s)^, Length(s) * SizeOf(Char), 0)
  else
    Result := JenkinsHashInteger(NullStringHash);
end;
{$ENDIF UNICODE}

function JenkinsHashString(const s : AnsiString) : Integer;
begin
  if s <> '' then
    Result := JenkinsHashBuffer(PByte(s)^, Length(s), 0)
  else
    Result := JenkinsHashInteger(NullStringHash);
end;

function JenkinsHashWideString(const s: WideString) : Integer;
begin
  if s <> '' then
    Result := JenkinsHashBuffer(PByte(Pointer(s))^, Length(s) * SizeOf(WideChar), 0)
  else
    Result := JenkinsHashInteger(NullStringHash);
end;

function JenkinsHashSingle(s : Single) : Integer;
begin
  Result := JenkinsHashInteger(PInteger(@s)^);
end;

function JenkinsHashDouble(d : Double) : Integer;
begin
  Result := JenkinsHashBuffer(PByte(@d)^, SizeOf(double), 0);
end;

procedure HashLocation(const obj : DObject; var loc : PByte; var len : Integer);
begin
  case obj.VType of
      vtInteger, vtPointer, vtPAnsiChar, vtObject, vtClass, vtPWideChar, vtInterface:
        begin
          loc := @obj.VInteger;
          len := SizeOf(Integer);
        end;
      vtBoolean:
        begin
          loc := @obj.VBoolean;
          len := SizeOf(obj.VBoolean);
        end;
      vtChar:
        begin
          loc := @obj.VChar;
          len := SizeOf(obj.VChar);
        end;
      vtExtended:
        begin
          loc := @obj.VExtended^;
          len := SizeOf(obj.VExtended^);
        end;
      vtString:
        begin
          loc := PByte(obj.VString);
          len := Length(obj.VString^);
        end;
      vtWideChar:
        begin
          loc := @obj.VWideChar;
          len := SizeOf(obj.VWideChar);
        end;
      vtAnsiString:
          if obj.vAnsistring <> nil then
            begin
              loc := obj.VAnsiString;
              len := Length(AnsiString(obj.VAnsiString));
            end
          else
            begin
              loc := @NullStringHash;
              len := SizeOf(NullStringHash);
            end;
      {$IFDEF UNICODE}
      vtUnicodeString:
          if obj.VUnicodeString <> nil then
            begin
              loc := obj.VUnicodeString;
              len := Length(UnicodeString(obj.VUnicodeString)) * SizeOf(Char);
            end
          else
            begin
              loc := @NullStringHash;
              len := SizeOf(NullStringHash);
            end;
      {$ENDIF UNICODE}
      vtCurrency:
        begin
          loc := PByte(obj.VCurrency);
          len := SizeOf(Currency);
        end;
      vtVariant:
        raise DException.Create('variant type hash not implemented yet');
      vtWideString:
        begin
          if obj.VWideString <> nil then
            begin
              loc := obj.VWideString;
              len := Length(WideString(obj.VWideString)) * SizeOf(WideChar);
            end
          else
            begin
              loc := @NullStringHash;
              len := SizeOf(NullStringHash);
            end;
        end;
{$IFDEF USELONGWORD}
      vtInt64:
        begin
          loc := PByte(obj.VInt64);
          len := SizeOf(Int64);
        end;
{$ENDIF}
  end;
end;

function JenkinsHashDObject(const obj : DObject) : Integer;
begin
  case obj.VType of
      vtInteger, vtPointer, vtPAnsiChar, vtObject, vtClass, vtPWideChar, vtInterface:
        Result := JenkinsHashInteger(obj.VInteger);
      vtBoolean:
        Result := Ord(obj.VBoolean);
      vtChar:
        Result := JenkinsHashBuffer(obj.VChar, 1, 0);
      vtExtended:
        Result := JenkinsHashBuffer(obj.VExtended^, SizeOf(Extended), 0);
      vtString:
        Result := JenkinsHashBuffer(obj.VString^, Length(obj.VString^), 0);
      vtWideChar:
        Result := JenkinsHashBuffer(obj.VWideChar, SizeOf(WideChar), 0);
      vtAnsiString:
        Result := JenkinsHashString(AnsiString(obj.VAnsiString));
      {$IFDEF UNICODE}
      vtUnicodeString:
        Result := JenkinsHashString(UnicodeString(obj.VUnicodeString));
      {$ENDIF UNICODE}
      vtCurrency:
        Result := JenkinsHashBuffer(obj.VCurrency^, SizeOf(Currency), 0);
      vtVariant:
        raise DException.Create('variant type hash not implemented yet');
      vtWideString:
        Result := JenkinsHashWideString(WideString(obj.VWideString));
{$IFDEF USELONGWORD}
      vtInt64:
        Result := JenkinsHashBuffer(obj.VInt64^, SizeOf(Int64), 0);
{$ENDIF}
  else
    Result := 0;
  end;

end;

{$IFDEF TURNITOFF}
{$Q+}
{$ENDIF}

////////////////////////////////////////////////////////////////////
//
// DObject handling
//
////////////////////////////////////////////////////////////////////

var
  specialTypes : DeCALBasicTypes = [
    vtString, vtAnsiString, vtCurrency, vtVariant, vtWideString, vtExtended, vtInt64, vtUnicodeString
  ];

procedure InitDObject(var obj : DObject);
begin
  obj.VType := vtInteger;
  obj.VInteger := 0;
end;

procedure ClearDObject(var obj : DObject);
begin
  if (obj.VType in SpecialTypes) then
    begin
      case obj.VType of
        vtString:
          FreeMem(obj.VString);
        vtAnsiString:
          AnsiString(obj.VAnsiString) := '';
        {$IFDEF UNICODE}
        vtUnicodeString:
          UnicodeString(obj.VUnicodeString) := '';
        {$ENDIF UNICODE}
        vtCurrency:
          FreeMem(obj.VCurrency);
        vtExtended:
          FreeMem(obj.VExtended);
        vtVariant:
          Finalize(obj.VVariant^);
        vtWideString:
          WideString(obj.VWideString) := '';
{$IFDEF USELONGWORD}
        vtInt64:
          FreeMem(obj.vInt64);
{$ENDIF}
      end;
    end;
  obj.VType := vtInteger;
  obj.VInteger := 0;
end;

procedure SetDObject(var obj : DObject; const value : array of const);
begin
  ClearDObject(obj);
  CopyDObject(value[Low(value)], obj);
end;

procedure _SetDObject(var destination : DObject; const src : DObject);
begin
  if Addr(destination) <> Addr(src) then
    begin
      ClearDObject(destination);
      CopyDObject(src, destination);
    end;
end;

procedure Swap(var obj1, obj2 : DObject);
var tmp : DObject;
begin
  tmp := obj1;
  obj1 := obj2;
  obj2 := tmp;
end;

function getVType(const iterator : DIterator) : Integer;
begin
  Result := iterator.handler.iget(iterator).vtype;
end;

procedure InitDObjects(var obj : DObject; count : Integer);
var i : Integer;
begin
  i := 0;
  while i < count do
    begin
      InitDObject(PDObject(PByte(@obj) + SizeOf(DObject) * i)^);
      Inc(i);
    end;
end;

procedure ClearDObjects(var obj : DObject; count : Integer);
var i : Integer;
begin
  i := 0;
  while i < count do
    begin
      ClearDObject(PDObject(PByte(@obj) + SizeOf(DObject) * i)^);
      Inc(i);
    end;
end;

procedure MoveDObject(var source, dest : DObject);
begin
  dest := source;
  source.VType := vtInteger;
end;

procedure CopyDObject(const source : DObject; var dest : DObject);
begin

  // TODO: Is this the desired behavior?
  InitDObject(dest);

  if (source.VType in SpecialTypes) then
    begin

      dest.VType := source.VType;

      // See if we need to handle this specially.
      case source.VType of
        vtString:
          begin
            New(dest.VString);
            dest.VString^ := source.VString^;
          end;
        vtAnsiString:
          begin
            if source.VAnsiString <> nil then
              begin
                AnsiString(dest.VAnsiString) := AnsiString(source.VAnsiString);
                UniqueString(AnsiString(dest.VAnsiString));
              end
            else
              dest.VAnsiString := nil;
          end;
        {$IFDEF UNICODE}
        vtUnicodeString:
          begin
            if source.VUnicodeString <> nil then
              begin
                UnicodeString(dest.VUnicodeString) := UnicodeString(source.VUnicodeString);
                UniqueString(UnicodeString(dest.VUnicodeString));
              end
            else
              dest.VUnicodeString := nil;
          end;
        {$ENDIF UNICODE}
        vtCurrency:
          begin
            New(dest.VCurrency);
            dest.VCurrency^ := source.VCurrency^;
          end;
        vtExtended:
          begin
            New(dest.VExtended);
            dest.VExtended^ := source.VExtended^;
          end;
        vtVariant:
          begin
            New(dest.VVariant);
            dest.VVariant^ := source.VVariant^;
          end;
        vtWideString:
          begin
            WideString(dest.VWideString) := WideString(source.VWideString);
          end;
{$IFDEF USELONGWORD}
        vtInt64:
          begin
            New(dest.VInt64);
            dest.vInt64^ := source.vInt64^;
          end;
{$ENDIF}
      end;
    end
  else
    dest := source;

end;

////////////////////////////////////////////////////////////////////
//
// Implementations
//
////////////////////////////////////////////////////////////////////

constructor DNotImplemented.Create;
begin
  inherited Create('Not implemented.');
end;

constructor DNeedBidirectional.Create;
begin
  inherited Create('Need bidirectional iterator.');
end;

constructor DNeedRandom.Create;
begin
  inherited Create('Need random access iterator.');
end;

constructor DEmpty.Create;
begin
  inherited Create('Empty data structure.');
end;

////////////////////////////////////////////////////////////////////
//
// Utilities
//
////////////////////////////////////////////////////////////////////

function MakePair(const ob1, ob2 : DObject) : DPair;
begin
  with result do
    begin
      CopyDObject(ob1, first);
      CopyDObject(ob2, second);
    end;
end;

function MakeRange(s,f : DIterator) : DRange;
begin
  with result do
    begin
      start := s;
      finish := f;
    end;
end;

var
  simpleHashTypes : DeCALBasicTypes = [
    vtInteger,
    vtBoolean,
    vtChar,
    vtPointer,
    vtPAnsiChar,
    vtObject,
    vtClass,
    vtWideChar,
    vtPWideChar,
    vtInterface
  ];

function hashCode(const obj : DObject) : Integer;
begin
  if obj.VType in simpleHashTypes then
    begin
      // since all the 4 byte types are in the same place, we can just
      // do this...
      Result := JenkinsHashInteger(obj.VInteger);
    end
  else
    begin
      case obj.vType of
        vtExtended:
          Result := JenkinsHashBuffer(obj.VExtended^, SizeOf(Extended), 0);
        vtString:
          Result := JenkinsHashBuffer(obj.VString^[1], Ord(obj.VString^[0]), 0);
        vtAnsiString:
          Result := JenkinsHashString(AnsiString(obj.VAnsiString));
        {$IFDEF UNICODE}
        vtUnicodeString:
          Result := JenkinsHashString(UnicodeString(obj.VUnicodeString));
        {$ENDIF UNICODE}
        vtCurrency:
          Result := JenkinsHashBuffer(obj.VCurrency^, SizeOf(currency), 0);
        vtVariant:
          raise DException.Create('Can''t hash variants.');
        vtWideString:
          Result := JenkinsHashWideString(WideString(obj.VWideString));
{$IFDEF USELONGWORD}
        vtInt64:
          Result := JenkinsHashBuffer(obj.vint64^, SizeOf(int64), 0);
{$ENDIF}
      else
        raise DException.Create('Hash of unknown type.');
      end;
    end;
end;

procedure FreeAll(const objs : array of TObject);
var i : Integer;
begin
  for i := Low(objs) to High(objs) do
    objs[i].free;
end;

procedure FreeAndClear(var container);
var con : DContainer;
begin
  con := DContainer(container);
  if assigned(con) then
    begin
      ObjFree(con);
      con.free;
      DContainer(container) := nil;
    end;
end;

////////////////////////////////////////////////////////////////////
//
// Iterator modifiers
//
////////////////////////////////////////////////////////////////////
procedure SetToKey(var iter : DIterator);
var flags : DIteratorFlags;
begin
  flags := iter.flags;
  Include(iter.flags, diKey);
  iter.Handler.iflagChange(iter, flags);
end;

procedure SetToValue(var iter : DIterator);
var flags : DIteratorFlags;
begin
  flags := iter.flags;
  Exclude(iter.flags, diKey);
  iter.Handler.iflagChange(iter, flags);
end;

function IterateOver(var iter : DIterator) : Boolean;
begin
  if diIteration in iter.flags then
    advance(iter)
  else
    Include(iter.flags, diIteration);
  Result := not atEnd(iter);
  if not result then
    Exclude(iter.flags, diIteration);
end;

////////////////////////////////////////////////////////////////////
//
// Iterator reflectors
//
////////////////////////////////////////////////////////////////////

function make(const obj : array of const) : DObject;
begin
  InitDObject(result);
  CopyDObject(obj[Low(obj)], result);
end;

{** Moves the iterator to the next object in the container. }
procedure advance(var iterator : DIterator);
begin
  iterator.Handler.iadvance(iterator);
end;

{** Returns a new iterator at the next position in the container. }
function advanceF(const iterator : DIterator) : DIterator;
begin
  Result := iterator;
  iterator.Handler.iadvance(result);
end;

{** Moves the iterator forward by count objects. }
procedure advanceBy(var iterator : DIterator; count : Integer);
begin
  iterator.Handler.iadvanceBy(iterator, count);
end;

{** Returns a new iterator at the next position in the container. }
function advanceByF(const iterator : DIterator; count : Integer) : DIterator;
begin
  Result := iterator;
  iterator.Handler.iadvanceBy(result, count);
end;

{** Tests to see if the iterator is at the start of the container. }
function atStart(const iterator : DIterator) : Boolean;
begin
  Result := iterator.Handler.iatStart(iterator);
end;

{** Tests to see if the iterator is at the end of the container.  This is
extremely common during loops. }
function atEnd(const iterator : DIterator) : Boolean;
begin
  Result := iterator.Handler.iatEnd(iterator);
end;

{** Gets the DObject at the iterator's position.  Usually you will use one of
the other forms, such as getObject or getString. }
function get(const iterator : DIterator) : DObject;
begin
  CopyDObject(iterator.Handler.iget(iterator)^, result);
end;

{** Get a pointer to the DObject under the iterator.  This is useful when
faster performance is required and you are willing to do more work to handle
the DObject pointers directly.  Internal routines often use this to prevent
copying DObjects around frequently. }
function getRef(const iterator : DIterator) : PDObject;
begin
  Result := iterator.Handler.iget(iterator);
end;

{
  Here's the reasoning on why obj is a constant instead of a var, and
  why I don't clear it out.  The object being passed in may have come from
  a stack-based array of const, in which case we don't want to mess
  with it.  If you want a putRef that cleans, use putRefClear, which will
  clean out the source object.
}
procedure putRef(const iterator : DIterator; const obj : DObject);
begin
  // We do this to ensure that space is available...
  if atEnd(iterator) then
    begin
      getContainer(iterator).AddRef(obj);
      //_put(iterator, obj);
    end
  else
    getRef(iterator)^ := obj;

  //MoveDObject((PDObject(@obj))^, getRef(iterator)^);
end;

{** Retrieve the integer at the current iterator position.  Verifies the type
if assertions are active. }
function getInteger(const iterator : DIterator) : Integer;
begin
  Assert(iterator.Handler.iget(iterator).VType = vtInteger);
  Result := iterator.Handler.iget(iterator).VInteger;
end;

{** Retrieve the boolean at the current iterator position.  Verifies the type
if assertions are active. }
function getBoolean(const iterator : DIterator) : Boolean;
begin
  Assert(iterator.Handler.iget(iterator).VType = vtBoolean);
  Result := iterator.Handler.iget(iterator).VBoolean;
end;

{** Retrieve the character at the current iterator position.  Verifies the type
if assertions are active. }
function getAnsiChar(const iterator : DIterator) : AnsiChar;
begin
  Assert(iterator.Handler.iget(iterator).VType = vtAnsiChar);
  Result := iterator.Handler.iget(iterator).VChar;
end;

{** Retrieve the character at the current iterator position.  Verifies the type
if assertions are active. }
function getChar(const iterator : DIterator) : Char;
begin
  {$IFDEF UNICODE}
  Result := getWideChar(iterator);
  {$ELSE}
  Result := getAnsiChar(iterator);
  {$ENDIF UNICODE}
end;

{** Retrieve the extended value at the current iterator position.  Verifies the type
if assertions are active. }
function getExtended(const iterator : DIterator) : Extended;
begin
  Assert(iterator.Handler.iget(iterator).VType = vtExtended);
  Result := iterator.Handler.iget(iterator).VExtended^;
end;

{** Retrieve the short string at the current iterator position.  Verifies the type
if assertions are active. }
function getShortString(const iterator : DIterator) : ShortString;
begin
  Assert(iterator.Handler.iget(iterator).VType = vtString);
  Result := iterator.Handler.iget(iterator).VString^;
end;

{** Retrieve the pointer at the current iterator position.  Verifies the type
if assertions are active. }
function getPointer(const iterator : DIterator) : Pointer;
begin
  Assert(iterator.Handler.iget(iterator).VType = vtPointer);
  Result := iterator.Handler.iget(iterator).VPointer
end;

{** Retrieve the PAnsiChar at the current iterator position.  Verifies the type
if assertions are active. }
function getPAnsiChar(const iterator : DIterator) : PAnsiChar;
begin
  Assert(iterator.Handler.iget(iterator).VType = vtPAnsiChar);
  Result := iterator.Handler.iget(iterator).VPChar;
end;

{** Retrieve the PChar at the current iterator position.  Verifies the type
if assertions are active. }
function getPChar(const iterator : DIterator) : PChar;
begin
  {$IFDEF UNICODE}
  Result := getPWideChar(iterator);
  {$ELSE}
  Result := getPAnsiChar(iterator);
  {$ENDIF UNICODE}
end;

{** Retrieve the object at the current iterator position.  Verifies the type
if assertions are active. A typecast following this function call is very frequent. }
function getObject(const iterator : DIterator) : TObject;
begin
  Assert((iterator.Handler.iget(iterator).VType = vtObject) or (iterator.Handler.iget(iterator).VInteger = 0));
  Result := iterator.Handler.iget(iterator).VObject;
end;

{** Retrieve the metaclass (TClass) object at the current iterator position.  Verifies the type
if assertions are active. }
function getClass(const iterator : DIterator) : TClass;
begin
  Assert(iterator.Handler.iget(iterator).VType = vtClass);
  Result := iterator.Handler.iget(iterator).VClass;
end;

{** Retrieve the wide character at the current iterator position.  Verifies the type
if assertions are active. }
function getWideChar(const iterator : DIterator) : WideChar;
begin
  Assert(iterator.Handler.iget(iterator).VType = vtWideChar);
  Result := iterator.Handler.iget(iterator).VWideChar;
end;

{** Retrieve the pointer to wide character at the current iterator position.  Verifies the type
if assertions are active. }
function getPWideChar(const iterator : DIterator) : PWideChar;
begin
  Assert(iterator.Handler.iget(iterator).VType = vtPWideChar);
  Result := iterator.Handler.iget(iterator).VPWideChar;
end;

{** Retrieve the AnsiString at the current iterator position.  Verifies the type
if assertions are active. }
function getAnsiString(const iterator : DIterator) : AnsiString;
begin
  Assert(iterator.Handler.iget(iterator).VType = vtAnsiString);
  Result := AnsiString(iterator.Handler.iget(iterator).VAnsiString);
  UniqueString(result);
end;

{** Retrieve the string (AnsiString) at the current iterator position.  Verifies the type
if assertions are active. }
function getString(const iterator : DIterator) : string;
begin
  {$IFDEF UNICODE}
  if iterator.Handler.iget(iterator).VType = vtUnicodeString then
  begin
    Result := UnicodeString(iterator.Handler.iget(iterator).VUnicodeString);
    UniqueString(result);
  end
  else
  {$ENDIF UNICODE}
  if iterator.Handler.iget(iterator).VType = vtWideString then
    Result := string(getWideString(iterator))
  else
    Result := string(getAnsiString(iterator));
end;

{** Retrieve the currency value at the current iterator position.  Verifies the type
if assertions are active. }
function getCurrency(const iterator : DIterator) : Currency;
begin
  Assert(iterator.Handler.iget(iterator).VType = vtCurrency);
  Result := iterator.Handler.iget(iterator).VCurrency^;
end;

{** Retrieve the variant at the current iterator position.  Verifies the type
if assertions are active. }
function getVariant(const iterator : DIterator) : Variant;
begin
  Assert(iterator.Handler.iget(iterator).VType = vtVariant);
  Result := iterator.Handler.iget(iterator).VVariant^;
end;

{** Retrieve the interface at the current iterator position.  Verifies the type
if assertions are active. }
function getInterface(const iterator : DIterator) : Pointer;
begin
  Assert(iterator.Handler.iget(iterator).VType = vtInterface);
  Result := iterator.Handler.iget(iterator).VInterface;
end;

{** Retrieve the wide string at the current iterator position.  Verifies the type
if assertions are active. }
function getWideString(const iterator : DIterator) : WideString;
begin
  Assert(iterator.Handler.iget(iterator).VType = vtWideString);
  Result := WideString(iterator.Handler.iget(iterator).VWideString);
end;

{$IFDEF USELONGWORD}
function getInt64(const iterator : DIterator) : Int64;
begin
  Assert(iterator.Handler.iget(iterator).VType = vtInt64);
  Result := iterator.handler.iget(iterator).VInt64^;
end;
{$endif}

function asInteger(const obj : DObject) : Integer;
begin
  Assert(obj.VType = vtInteger);
  Result := obj.VInteger;
end;

function asBoolean(const obj : DObject) : Boolean;
begin
  Assert(obj.VType = vtBoolean);
  Result := obj.VBoolean;
end;

function asAnsiChar(const obj : DObject) : AnsiChar;
begin
  Assert(obj.VType = vtAnsiChar);
  Result := obj.VChar;
end;

function asChar(const obj : DObject) : Char;
begin
  {$IFDEF UNICODE}
  Result := asWideChar(obj);
  {$ELSE}
  Result := asAnsiChar(obj);
  {$ENDIF UNICODE}
end;

function asExtended(const obj : DObject) : Extended;
begin
  Assert(obj.VType = vtExtended);
  Result := obj.VExtended^;
end;

function asShortString(const obj : DObject) : ShortString;
begin
  Assert(obj.VType = vtString);
  Result := obj.VString^;
end;

function asPointer(const obj : DObject) : Pointer;
begin
  Assert(obj.VType = vtPointer);
  Result := obj.VPointer;
end;

function asPAnsiChar(const obj : DObject) : PAnsiChar;
begin
  Assert(obj.VType = vtPAnsiChar);
  Result := obj.VPChar;
end;

function asPChar(const obj : DObject) : PChar;
begin
  {$IFDEF UNICODE}
  Result := asPWideChar(obj);
  {$ELSE}
  Result := asPAnsiChar(obj);
  {$ENDIF UNICODE}
end;

function asObject(const obj : DObject) : TObject;
begin
  Assert((obj.VObject = nil) or (obj.VType = vtObject));
  Result := obj.VObject;
end;

function asClass(const obj : DObject) : TClass;
begin
  Assert(obj.VType = vtClass);
  Result := obj.VClass;
end;

function asWideChar(const obj : DObject) : WideChar;
begin
  Assert(obj.VType = vtWideChar);
  Result := obj.VWideChar;
end;

function asPWideChar(const obj : DObject) : PWideChar;
begin
  Assert(obj.VType = vtPWideChar);
  Result := obj.VPWideChar;
end;

function asAnsiString(const obj : DObject) : AnsiString;
begin
  Assert(obj.VType = vtAnsiString);
  Result := AnsiString(obj.VAnsiString);
end;

function asString(const obj : DObject) : string;
begin
  {$IFDEF UNICODE}
  if obj.VType = vtUnicodeString then
    Result := string(obj.VUnicodeString)
  else
  {$ENDIF UNICODE}
  if obj.VType = vtWideString then
    Result := string(asWideString(obj))
  else
    Result := string(asAnsiString(obj));
end;

function asCurrency(const obj : DObject) : Currency;
begin
  Assert(obj.VType = vtCurrency);
  Result := obj.VCurrency^;
end;

function asVariant(const obj : DObject) : Variant;
begin
  Assert(obj.VType = vtVariant);
  Result := obj.VVariant^;
end;

function asInterface(const obj : DObject) : Pointer;
begin
  Assert(obj.VType = vtInterface);
  Result := obj.VInterface;
end;

function asWideString(const obj : DObject) : WideString;
begin
  Assert(obj.VType = vtWideString);
  Result := WideString(obj.VWideString);
end;

{$IFDEF USELONGWORD}
function asInt64(const obj : DObject) : Int64;
begin
  Assert(obj.vtype = vtInt64);
  Result := obj.VInt64^;
end;

{$ENDIF}

function toInteger(const obj : DObject) : Integer;
begin
  Assert(obj.VType = vtInteger);
  Result := obj.VInteger;
  ClearDObject(PDObject(@obj)^);
end;

function toBoolean(const obj : DObject) : Boolean;
begin
  Assert(obj.VType = vtBoolean);
  Result := obj.VBoolean;
  ClearDObject(PDObject(@obj)^);
end;

function toAnsiChar(const obj : DObject) : AnsiChar;
begin
  Assert(obj.VType = vtAnsiChar);
  Result := obj.VChar;
  ClearDObject(PDObject(@obj)^);
end;

function toChar(const obj : DObject) : Char;
begin
  {$IFDEF UNICODE}
  Result := toWideChar(obj);
  {$ELSE}
  Result := toAnsiChar(obj);
  {$ENDIF UNICODE}
end;

function toExtended(const obj : DObject) : Extended;
begin
  Assert(obj.VType = vtExtended);
  Result := obj.VExtended^;
  ClearDObject(PDObject(@obj)^);
end;

function toShortString(const obj : DObject) : ShortString;
begin
  Assert(obj.VType = vtString);
  Result := obj.VString^;
  ClearDObject(PDObject(@obj)^);
end;

function toPointer(const obj : DObject) : Pointer;
begin
  Assert(obj.VType = vtPointer);
  Result := obj.VPointer;
  ClearDObject(PDObject(@obj)^);
end;

function toPAnsiChar(const obj : DObject) : PAnsiChar;
begin
  Assert(obj.VType = vtPAnsiChar);
  Result := obj.VPChar;
  ClearDObject(PDObject(@obj)^);
end;

function toPChar(const obj : DObject) : PChar;
begin
  {$IFDEF UNICODE}
  Result := toPWideChar(obj);
  {$ELSE}
  Result := toPAnsiChar(obj);
  {$ENDIF UNICODE}
end;

function toObject(const obj : DObject) : TObject;
begin
  Assert((obj.VObject = nil) or (obj.VType = vtObject));
  Result := obj.VObject;
  ClearDObject(PDObject(@obj)^);
end;

function toClass(const obj : DObject) : TClass;
begin
  Assert(obj.VType = vtClass);
  Result := obj.VClass;
  ClearDObject(PDObject(@obj)^);
end;

function toWideChar(const obj : DObject) : WideChar;
begin
  Assert(obj.VType = vtWideChar);
  Result := obj.VWideChar;
  ClearDObject(PDObject(@obj)^);
end;

function toPWideChar(const obj : DObject) : PWideChar;
begin
  Assert(obj.VType = vtPWideChar);
  Result := obj.VPWideChar;
  ClearDObject(PDObject(@obj)^);
end;

function toAnsiString(const obj : DObject) : AnsiString;
begin
  Assert(obj.VType = vtAnsiString);
  Result := AnsiString(obj.VAnsiString);
  ClearDObject(PDObject(@obj)^);
end;

function toString(const obj : DObject) : string;
begin
  {$IFDEF UNICODE}
  if obj.VType = vtUnicodeString then
  begin
    Result := string(obj.VUnicodeString);
    ClearDObject(PDObject(@obj)^);
  end
  else
  {$ENDIF UNICODE}
  if obj.VType = vtWideString then
    Result := string(toWideString(obj))
  else
    Result := string(toAnsiString(obj));
end;

function toCurrency(const obj : DObject) : Currency;
begin
  Assert(obj.VType = vtCurrency);
  Result := obj.VCurrency^;
  ClearDObject(PDObject(@obj)^);
end;

function toVariant(const obj : DObject) : Variant;
begin
  Assert(obj.VType = vtVariant);
  Result := obj.VVariant^;
  ClearDObject(PDObject(@obj)^);
end;

function toInterface(const obj : DObject) : Pointer;
begin
  Assert(obj.VType = vtInterface);
  Result := obj.VInterface;
  ClearDObject(PDObject(@obj)^);
end;

function toWideString(const obj : DObject) : WideString;
begin
  Assert(obj.VType = vtWideString);
  Result := WideString(obj.VWideString);
  ClearDObject(PDObject(@obj)^);
end;

{$IFDEF USELONGWORD}
function toInt64(const obj : DObject) : Int64;
begin
  Assert(obj.vtype = vtInt64);
  Result := obj.VInt64^;
  ClearDObject(PDObject(@obj)^);
end;
{$ENDIF}

procedure setInteger(var obj : DObject; value : Integer);
begin
  setDObject(obj, [value]);
end;

procedure setBoolean(var obj : DObject; value : Boolean);
begin
  setDObject(obj, [value]);
end;

procedure setAnsiChar(var obj : DObject; value : AnsiChar);
begin
  setDObject(obj, [value]);
end;

procedure setChar(var obj : DObject; value : Char);
begin
  setDObject(obj, [value]);
end;

procedure setExtended(var obj : DObject; const value : Extended);
begin
  setDObject(obj, [value]);
end;

procedure setShortString(var obj : DObject; const value : ShortString);
begin
  setDObject(obj, [value]);
end;

procedure setPointer(var obj : DObject; value : Pointer);
begin
  setDObject(obj, [value]);
end;

procedure setPAnsiChar(var obj : DObject; value : PAnsiChar);
begin
  setDObject(obj, [value]);
end;

procedure setPChar(var obj : DObject; value : PChar);
begin
  setDObject(obj, [value]);
end;

procedure setObject(var obj : DObject; value : TObject);
begin
  setDObject(obj, [value]);
end;

procedure setClass(var obj : DObject; value : TClass);
begin
  setDObject(obj, [value]);
end;

procedure setWideChar(var obj : DObject; value : WideChar);
begin
  setDObject(obj, [value]);
end;

procedure setPWideChar(var obj : DObject; value : PWideChar);
begin
  setDObject(obj, [value]);
end;

procedure setAnsiString(var obj : DObject; const value : AnsiString);
begin
  setDObject(obj, [value]);
end;

procedure setString(var obj : DObject; const value : string);
begin
  setDObject(obj, [value]);
end;

procedure setCurrency(var obj : DObject; value : Currency);
begin
  setDObject(obj, [value]);
end;

procedure setVariant(var obj : DObject; const value : Variant);
begin
  setDObject(obj, [value]);
end;

procedure setInterface(var obj : DObject; value : Pointer);
begin
  setDObject(obj, [value]);
end;

procedure setWideString(var obj : DObject; const value : WideString);
begin
  setDObject(obj, [value]);
end;

{$IFDEF USELONGWORD}
procedure setInt64(var obj : Dobject; const value : Int64);
begin
  setDObject(obj, [value]);
end;
{$ENDIF}

function equals(const iter1, iter2 : DIterator) : Boolean;
begin
  Result := iter1.Handler.iequals(iter1, iter2);
end;

function getContainer(const iterator : DIterator) : DContainer;
begin
  Result := iterator.Handler.iGetContainer(iterator);
end;

procedure _output(var iterator : DIterator; const obj : DObject);
begin
  _put(iterator, obj);
  if atEnd(iterator) then
    begin
      iterator := getContainer(iterator).finish;
    end
  else
    begin
      advance(iterator);
    end;
end;

procedure _outputRef(var iterator : DIterator; const obj : DObject);
begin
  if atEnd(iterator) then
    begin
      putRef(iterator, obj);
      iterator := getContainer(iterator).finish;
    end
  else
    begin
      putRef(iterator, obj);
      advance(iterator);
    end;
end;

procedure output(var iterator : DIterator; const objs : array of const);
var i : Integer;
begin
  for i := Low(objs) to High(objs) do
    _output(iterator, objs[i]);
end;

procedure _put(const iterator : DIterator; const obj : DObject);
begin
  iterator.Handler.iput(iterator, obj);
end;

procedure put(const iterator : DIterator; const objs : array of const);
var i : Integer;
begin
  for i := High(objs) to Low(objs) do
    _put(iterator, objs[i]);
end;

{** Store an integer at the iterator's location. }
procedure putInteger(const iterator : DIterator; value : Integer);
begin
  iterator.Handler._iput(iterator, [value]);
end;

{** Store a boolean at the iterator's location. }
procedure putBoolean(const iterator : DIterator; value : Boolean);
begin
  iterator.Handler._iput(iterator, [value]);
end;

procedure putAnsiChar(const iterator : DIterator; value : AnsiChar);
begin
  iterator.Handler._iput(iterator, [value]);
end;

procedure putChar(const iterator : DIterator; value : Char);
begin
  iterator.Handler._iput(iterator, [value]);
end;

procedure putExtended(const iterator : DIterator; const value : Extended);
begin
  iterator.Handler._iput(iterator, [value]);
end;

procedure putShortString(const iterator : DIterator; const value : ShortString);
begin
  iterator.Handler._iput(iterator, [value]);
end;

procedure putPointer(const iterator : DIterator; value : Pointer);
begin
  iterator.Handler._iput(iterator, [value]);
end;

procedure putPAnsiChar(const iterator : DIterator; value : PAnsiChar);
begin
  iterator.Handler._iput(iterator, [value]);
end;

procedure putPChar(const iterator : DIterator; value : PChar);
begin
  iterator.Handler._iput(iterator, [value]);
end;

{** Store an object at the iterator's location. }
procedure putObject(const iterator : DIterator; value : TObject);
begin
  iterator.Handler._iput(iterator, [value]);
end;

procedure putClass(const iterator : DIterator; value : TClass);
begin
  iterator.Handler._iput(iterator, [value]);
end;

procedure putWideChar(const iterator : DIterator; value : WideChar);
begin
  iterator.Handler._iput(iterator, [value]);
end;

procedure putPWideChar(const iterator : DIterator; value : PWideChar);
begin
  iterator.Handler._iput(iterator, [value]);
end;

procedure putAnsiString(const iterator : DIterator; const value : AnsiString);
begin
  iterator.Handler._iput(iterator, [value]);
end;

procedure putString(const iterator : DIterator; const value : string);
begin
  iterator.Handler._iput(iterator, [value]);
end;

procedure putCurrency(const iterator : DIterator; value : Currency);
begin
  iterator.Handler._iput(iterator, [value]);
end;

procedure putVariant(const iterator : DIterator; const value : Variant);
begin
  iterator.Handler._iput(iterator, [value]);
end;

procedure putInterface(const iterator : DIterator; value : Pointer);
begin
  iterator.Handler._iput(iterator, [value]);
end;

procedure putWideString(const iterator : DIterator; const value : WideString);
begin
  iterator.Handler._iput(iterator, [value]);
end;

{$IFDEF USELONGWORD}
procedure putInt64(const iterator : DIterator; const value : Int64);
begin
  iterator.handler._iput(iterator, [value]);
end;
{$ENDIF}

function distance(const iter1, iter2 : DIterator) : Integer;
begin
  Result := iter1.Handler.idistance(iter1, iter2);
end;

// bidirectional
procedure retreat(var iterator : DIterator);
begin
  iterator.Handler.iretreat(iterator);
end;

procedure retreatBy(var iterator : DIterator; count : Integer);
begin
  iterator.Handler.iretreatBy(iterator, count);
end;

function retreatF(const iterator : DIterator) : DIterator;
begin
  Result := iterator;
  retreat(result);
end;

function retreatByF(const iterator : DIterator; count : Integer) : DIterator;
begin
  Result := iterator;
  retreatBy(result, count);
end;

function getAt(const iter : DIterator; offset : Integer) : DObject;
begin
  CopyDObject(iter.Handler.igetAt(iter, offset)^, result);
end;

procedure putAt(const iter : DIterator; offset : Integer; const obj : DObject);
begin
  iter.Handler.iputAt(iter, offset, obj);
end;

procedure _putAt(const iter : DIterator; offset : Integer; const objs : array of const);
var i : Integer;
begin
  for i := High(objs) to Low(objs) do
    putAt(iter, offset, objs[i]);
end;

// random
function index(const iter : DIterator) : Integer;
begin
  Result := iter.Handler.iindex(iter);
end;

function less(const iter1, iter2 : DIterator) : Boolean;
begin
  Result := iter1.Handler.iless(iter1, iter2);
end;

////////////////////////////////////////////////////////////////////
//
// DTree
//
////////////////////////////////////////////////////////////////////

constructor DTreeNode.Create;
begin
  parent := nil;
  left := nil;
  right := nil;
  InitDObject(pair.first);
  InitDObject(pair.second);
  color := tnfBlack;
end;

destructor DTreeNode.Destroy;
begin
  {$IFDEF NOTDEFINED}

  // Do I contain a DComposite?  If so, Destroy it.
  if pair.first is DComposite then
    pair.first.free;
  if (pair.second <> pair.first) and (pair.second is DComposite) then
    pair.second.free;

  {$ENDIF}

  ClearDObject(pair.first);
  ClearDObject(pair.second);

  inherited;

end;

destructor DTreeNode.Kill;
begin
  inherited Destroy;
end;

constructor DTreeNode.CreateWith(const _pair : DPair);
begin
  parent := nil;
  left := nil;
  right := nil;
  CopyDObject(_pair.first, pair.first);
  CopyDObject(_pair.second, pair.second);
  color := tnfBlack;
end;

constructor DTreeNode.CreateUnder(const _pair : DPair; _parent : DTreeNode);
begin
  parent := _parent;
  left := nil;
  right := nil;
  CopyDObject(_pair.first, pair.first);
  CopyDObject(_pair.second, pair.second);
  color := tnfBlack;
end;

constructor DTreeNode.MakeWith(const _pair : DPair; _parent, _left, _right : DTreeNode);
begin
  CopyDObject(_pair.first, pair.first);
  CopyDObject(_pair.second, pair.second);
  parent := _parent;
  left := _left;
  right := _right;
  color := tnfBlack;
end;

{$IFDEF USEPOOLS}
var
  treePool : TPool = nil;

class function DTreeNode.NewInstance : TObject;
begin
  if treePool = nil then
    treePool := TMemPool.Create(InstanceSize, false);

  Result := TObject(treePool.Alloc);

  InitInstance(result);

end;

procedure DTreeNode.FreeInstance;
begin
  treePool.Free(self);
end;

procedure CleanupPools;
begin
  if treePool <> nil then
    treePool.Destroy;
end;

{$ENDIF}

constructor DRedBlackTree.Create(insideOf : DContainer; always : Boolean; compare : DComparator);
begin
  FContainer := insideOf;
  FNodeCount := 0;
  FInsertAlways := always;
  FComparator := compare;
  RBInitialize;
end;

destructor DRedBlackTree.Destroy;
begin
  erase(false);
  fHeader.Free;
  inherited;
end;

function DRedBlackTree.start : DIterator;
begin
  with result do
    begin
      if size = 0 then
        flags := [diBidirectional, diMarkFinish]
      else
        flags := [diBidirectional, diMarkStart];
      Handler := self.Fcontainer;
      tree := self;
      treeNode := FHeader.left;
    end;
end;

function DRedBlackTree.finish : DIterator;
begin
  with result do
    begin
      flags := [diBidirectional, diMarkFinish];
      Handler := self.Fcontainer;
      tree := self;
      treeNode := FHeader;
    end;
end;

function DRedBlackTree.RBCopyTree(oldNode, parent : DTreeNode) : DTreeNode;
begin
  if oldNode = nil_node then
    Result := nil_node
  else
    begin
      Result := DTreeNode.CreateWith(oldNode.pair);
      Result.parent := parent;
      Result.left := RBCopyTree(oldNode.left, result);
      Result.right := RBCopyTree(oldNode.right, result);
      Result.color := oldNode.color;
    end;
end;

procedure DRedBlackTree.RBCopy(tree : DRedBlackTree);
begin
  FHeader.parent := RBCopyTree(tree.FHeader.parent, FHeader);
  FHeader.left := RBminimum(tree.FHeader.parent);
  FHeader.right := RBmaximum(tree.FHeader.parent);
  FNodeCount := tree.FNodeCount;
end;

function DRedBlackTree.Empty : Boolean;
begin
  Result := FNodeCount = 0;
end;

function DRedBlackTree.Size : Integer;
begin
  Result := FNodeCount;
end;

function DRedBlackTree.MaxSize : Integer;
begin
  Result := MaxInt;
end;

procedure DRedBlackTree.swap(another : DRedBlackTree);
var tb : Boolean;
    ti : Integer;
    tn : DTreeNode;
    tc : DComparator;
begin
  tn := FHeader;
  FHeader := another.FHeader;
  another.FHeader := tn;

  ti := FNodeCount;
  FNodeCount := another.FNodeCount;
  another.FNodeCount := ti;

  tb := FInsertAlways;
  FInsertAlways := another.FInsertAlways;
  another.FInsertAlways := tb;

  tc := FComparator;
  FComparator := another.FComparator;
  another.FComparator := tc;

end;

function DRedBlackTree.RBInternalInsert(x,y : DTreeNode; const pair : DPair) : Boolean;
var z : DTreeNode;
    toLeft : Boolean;
begin
  Inc(FNodeCount);

  z := DTreeNode.CreateWith(pair);

  toLeft := (y = FHeader) or (x <> nil_node) or (FComparator(z.pair.first, y.pair.first) < 0);

  RBinsert(toLeft, x, y, z);

  Result := True;
end;

{
function DRedBlackTree.insert(const pair : DPair) : Boolean;
var x,y : DTreeNode;
    cmp : Integer;
    comp : Boolean;
    j : DIterator;
begin

  Result := True;

  y := FHeader;
  x := FHeader.parent;
  comp := true;

  while x <> nil_node do
    begin
      y := x;

      cmp := FComparator(pair.first, x.pair.first);
      if (cmp = 0) and (not FInsertAlways) then
        begin
          ClearDObject(x.pair.second);
          CopyDObject(pair.second, x.pair.second);
          Exit;
        end;

      comp := cmp < 0;
      if comp then
        x := x.left
      else
        x := x.right;
    end;

  if FInsertAlways then
    begin
      RBInternalInsert(x,y,pair);
      exit;
    end;

  j := start;
  j.treeNode := y;

  if comp then
    begin
      if j.treeNode = start.treeNode then
        begin
          RBInternalInsert(x,y,pair);
          exit;
        end
      else
        begin
          j.flags := j.flags - [diMarkStart, diMarkFinish];
          retreat(j);
        end;
    end;

  if FComparator(j.treeNode.pair.first, pair.first) < 0 then
    begin
      RBInternalInsert(x,y,pair);
      exit;
    end;

  Result := False;
end;

}

function DRedBlackTree.insert(const pair : DPair) : Boolean;
var x,y : DTreeNode;
    cmp : Integer;
    comp : Boolean;
    j : DTreeNode;
begin

  Result := True;

  y := FHeader;
  x := FHeader.parent;
  comp := true;

  while x <> nil_node do
    begin
      y := x;

      cmp := FComparator(pair.first, x.pair.first);
      if (cmp = 0) and (not FInsertAlways) then
        begin
          _SetDObject(x.pair.second, pair.second);
          Exit;
        end;

      comp := cmp < 0;
      if comp then
        x := x.left
      else
        x := x.right;
    end;

  if FInsertAlways then
    begin
      RBInternalInsert(x,y,pair);
      exit;
    end;

  j := y;

  if comp then
    begin
      if j = Fheader.left then
        begin
          RBInternalInsert(x,y,pair);
          exit;
        end
      else
        RBDecrement(j);
    end;

  if FComparator(j.pair.first, pair.first) < 0 then
    RBInternalInsert(x,y,pair)
  else
    Result := False;
end;

function DRedBlackTree.insertAt(pos : DIterator; const pair : DPair) : Boolean;
begin
  raise DException.Create('Can''t insert to iterator in tree.');
end;

function DRedBlackTree.insertIn(_start, _finish : DIterator) : Boolean;
var pair : Dpair;
begin
  Result := true;
  while not DeCAL.equals(_start, finish) do
    begin
      pair.first := getRef(_start)^;
      pair.second := getRef(_start)^;
      Result := result and insert(pair);
      advance(_start);
    end;
end;

procedure DRedBlackTree.RBEraseTree(node : DTreeNode; direct : Boolean);
begin
  if node <> nil_node then
    begin
      RBEraseTree(node.left, direct);
      RBEraseTree(node.right, direct);

      if direct then
        node.kill
      else
        node.free;

    end;
end;

procedure DRedBlackTree.erase(direct : Boolean);
begin
  RBEraseTree(FHeader.parent, direct);
  FHeader.left := FHeader;
  FHeader.parent := nil_node;
  FHeader.right := FHeader;
  FNodeCount := 0;
end;

procedure DRedBlackTree.eraseAt(pos : DIterator);
var node : DTreeNode;
begin
  Assert(not atEnd(pos));
  node := RBerase(pos.treenode);
  Dec(FNodeCount);
  node.free;
end;

function DRedBlackTree.eraseKeyN(const obj : DObject; count : Integer) : Integer;
var p : DRange;
begin
  p := equal_range(obj);

  if p.start.treenode = p.finish.treenode then
    Result := 0
  else
    begin
      // Assert(not atEnd(p.start), 'Cannot erase non-existent key');

      if count <> MaxInt then
        begin

          Result := distance(p.start, p.finish);
          if result > count then
            begin
              retreatBy(p.finish, result - count);
            end;
        end;

      Result := eraseIn(p.start, p.finish);
    end;

end;

function DRedBlackTree.eraseKey(const obj : DObject) : Integer;
begin
  Result := eraseKeyN(obj, MaxInt);
end;

function DRedBlackTree.eraseIn(_start, _finish : DIterator) : Integer;
var iter : DIterator;
begin
  Result := 0;
  if DeCAL.equals(_start, start) and DeCAL.equals(_finish, finish) then
    erase(false)
  else
    begin
      while not DeCAL.equals(_start, _finish) do
        begin
          iter := advanceF(_start);
          eraseAt(_start);
          Inc(result);
          _start := iter;
        end;
    end;
end;

{function DRedBlackTree.key(node : DTreeNode) : DObject;
begin
  CopyDObject(node.pair.first, result);
end;

function DRedBlackTree.value(node : DTreeNode) : DObject;
begin
  CopyDObject(node.pair.second, result);
end;
}

function DRedBlackTree.find(const obj : DObject) : DIterator;
var j : DIterator;
begin
  j := lower_bound(obj);
  if atEnd(j) or (FComparator(obj, j.treeNode.pair.first) < 0) then
    Result := finish
  else
    Result := j;
end;

function DRedBlackTree.count(const obj : DObject) : Integer;
var r : DRange;
begin
  r := equal_range(obj);
  Result := distance(r.start, r.finish);
end;

function DRedBlackTree.lower_bound(const obj : DObject) : DIterator;
var x,y : DTreeNode;
    comp : Boolean;
begin

  y := FHeader;
  x := FHeader.parent;
  comp := false;

  while x <> nil_node do
    begin
      y := x;
      comp := FComparator(x.pair.first, obj) < 0;
      if comp then
        x := x.right
      else
        x := x.left;
    end;

  Result := start;

  if not atEnd(result) then
    begin
      Result.treenode := y;
      if start.treenode <> Result.treenode then
        begin
          Result.flags := Result.flags - [diMarkStart, diMarkFinish];
        end;
      if finish.treenode = Result.treenode then
        begin
          Result.flags := Result.flags - [diMarkStart] + [diMarkFinish];
        end;

      if comp then
        advance(result);

    end;

end;

function DRedBlackTree.upper_bound(const obj : DObject) : DIterator;
var x,y : DTreeNode;
    comp : Boolean;
begin

  y := FHeader;
  x := y.parent;
  comp := true;

  while x <> nil_node do
    begin
      y := x;
      comp := FComparator(obj, x.pair.first) < 0;
      if comp then
        x := x.left
      else
        x := x.right;
    end;

  Result := start;
  if not atEnd(result) then
    begin
      Result.treeNode := y;

      if start.treenode <> Result.treenode then
        begin
          Result.flags := Result.flags - [diMarkStart, diMarkFinish];
        end;
      if finish.treenode = Result.treenode then
        begin
          Result.flags := Result.flags - [diMarkStart] + [diMarkFinish];
        end;

      if not comp then
        advance(result);
    end;

end;

function DRedBlackTree.equal_range(const obj : DObject) : DRange;
begin
  with result do
    begin
      start := lower_bound(obj);
      finish := upper_bound(obj);
    end;
end;

procedure DRedBlackTree.RBInitializeRoot;
begin
  if nil_node = nil then
    RBinitNil;
  FHeader.parent := nil_node;
  FHeader.left := FHeader;
  FHeader.right := FHeader;
end;

procedure DRedBlackTree.RBInitializeHeader;
begin
  FHeader := DTreeNode.Create;
  FHeader.color := tnfRed;
end;

procedure DRedBlackTree.RBInitialize;
begin
  RBInitializeHeader;
  RBInitializeRoot;
end;


procedure DRedBlackTree.RBinitNil;
begin
  if nil_node = nil then
    nil_node := DTreeNode.Create;
end;

procedure DRedBlackTree.RBincrement(var node : DTreeNode);
var y : DTreeNode;
begin

  if node.right <> nil_node then
    begin
      node := node.right;
      while node.left <> nil_node do
        node := node.left;
    end
  else
    begin
      y := node.parent;
      while node = y.right do
        begin
          node := y;
          y := y.parent;
        end;
      if node.right <> y then
        node := y;
    end;
end;

procedure DRedBlackTree.RBdecrement(var node : DTreeNode);
var y : DTreeNode;
begin

  if (node.color = tnfRed) and (node.parent.parent = node) then
    node := node.right
  else if node.left <> nil_node then
    begin
      y := node.left;
      while y.right <> nil_node do
        y := y.right;
      node := y;
    end
  else
    begin
      y := node.parent;
      while node = y.left do
        begin
          node := y;
          y := y.parent;
        end;
      node := y;
    end;
end;

function DRedBlackTree.RBminimum(node : DTreeNode) : DTreeNode;
begin
  if node = nil_node then
    Result := Fheader
  else
    begin
      while node.left <> nil_node do
        node := node.left;
      Result := node;
    end;
end;

function DRedBlackTree.RBmaximum(node : DTreeNode) : DTreeNode;
begin
  if node = nil_node then
    Result := Fheader
  else
    begin
      while node.right <> nil_node do
        node := node.right;
      Result := node;
    end;
end;

procedure DRedBlackTree.RBLeftRotate(node : DTreeNode);
var y : DTreeNode;
begin
  y := node.right;
  node.right := y.left;
  if y.left <> nil_node then
    y.left.parent := node;
  y.parent := node.parent;
  if node = FHeader.parent then
    FHeader.parent := y
  else if node = node.parent.left then
    node.parent.left := y
  else
    node.parent.right := y;

  y.left := node;
  node.parent := y;

end;

procedure DRedBlackTree.RBRightRotate(node : DTreeNode);
var y : DTreeNode;
begin
  y := node.left;
  node.left := y.right;
  if y.right <> nil_node then
    y.right.parent := node;
  y.parent := node.parent;

  if node = FHeader.parent then
    FHeader.parent := y
  else if node = node.parent.right then
    node.parent.right := y
  else
    node.parent.left := y;

  y.right := node;
  node.parent := y;

end;

procedure DRedBlackTree.RBinsert(insertToLeft : Boolean; x,y,z : DTreeNode);
begin

  if insertToLeft then
    begin
      y.left := z;
      if y = FHeader then
        begin
          FHeader.parent := z;
          FHeader.right := z;
        end
      else if y = FHeader.left then
        Fheader.left := z;
    end
  else
    begin
      y.right := z;
      if y = Fheader.right then
        FHeader.right := z;
    end;

  z.parent := y;
  z.left := nil_node;
  z.right := nil_node;
  x := z;
  x.color := tnfRed;

  while (x <> FHeader.parent) and (x.parent.color = tnfRed) do
    begin
      if x.parent = x.parent.parent.left then
        begin
          y := x.parent.parent.right;
          if y.color = tnfRed then
            begin
              x.parent.color := tnfBlack;
              y.color := tnfBlack;
              x.parent.parent.color := tnfRed;
              x := x.parent.parent;
            end
          else
            begin
              if x = x.parent.right then
                begin
                  x := x.parent;
                  RBLeftRotate(x);
                end;
              x.parent.color := tnfBlack;
              x.parent.parent.color := tnfRed;
              RBRightRotate(x.parent.parent);
            end;
        end
      else
        begin
          y := x.parent.parent.left;
          if y.color = tnfRed then
            begin
              x.parent.color := tnfBlack;
              y.color := tnfBlack;
              x.parent.parent.color := tnfRed;
              x := x.parent.parent;
            end
          else
            begin
              if x = x.parent.left then
                begin
                  x := x.parent;
                  RBRightRotate(x);
                end;
              x.parent.color := tnfBlack;
              x.parent.parent.color := tnfRed;
              RBLeftRotate(x.parent.parent);

            end;
        end;
    end;
  FHeader.parent.color := tnfBlack;

end;

function DRedBlackTree.RBerase(z : DTreeNode) : DTreeNode;
var w,x,y : DTreeNode;
    tmp : DTreeNodeColor;
begin
  y := z;

  if y.left = nil_node then
    x := y.right
  else if y.right = nil_node then
    x := y.left
  else
    begin
      y := y.right;
      while y.left <> nil_node do
        y := y.left;
      x := y.right;
    end;

  // No way should x be the nil_node at this point.
  // Assert(x <> nil_node);

  if y <> z then
    begin
      z.left.parent := y;
      y.left := z.left;
      if y <> z.right then
        begin
          x.parent := y.parent;
          y.parent.left := x;
          y.right := z.right;
          z.right.parent := y;
        end
      else
        x.parent := y;

      if FHeader.parent = z then
        FHeader.parent := y
      else if z.parent.left = z then
        z.parent.left := y
      else
        z.parent.right := y;

      y.parent := z.parent;
      tmp := y.color;
      y.color := z.color;
      z.color := tmp;
      y := z;

    end
  else
    begin
      x.parent := y.parent;
      if FHeader.parent = z then
        FHeader.parent := x
      else if z.parent.left = z then
        z.parent.left := x
      else
        z.parent.right := x;

      if FHeader.left = z then
        if z.right = nil_node then
          FHeader.left := z.parent
        else
          FHeader.left := RBminimum(x);

      if FHeader.right = z then
        if z.left = nil_node then
          FHeader.right := z.parent
        else
          FHeader.right := RBmaximum(x);

    end;

  if y.color <> tnfRed then
    begin
      while (x <> FHeader.parent) and (x.color = tnfBlack) do
        begin

          if x = x.parent.left then
            begin
              w := x.parent.right;
              if w.color = tnfRed then
                begin
                  w.color := tnfBlack;
                  x.parent.color := tnfRed;
                  RBLeftRotate(x.parent);
                  w := x.parent.right;
                end;

              if (w.left.color = tnfBlack) and (w.right.color = tnfBlack) then
                begin
                  w.color := tnfRed;
                  x := x.parent;
                end
              else
                begin
                  if w.right.color = tnfBlack then
                    begin
                      w.left.color := tnfBlack;
                      w.color := tnfRed;
                      RBRightRotate(w);
                      w := x.parent.right;
                    end;

                  w.color := x.parent.color;
                  x.parent.color := tnfBlack;
                  w.right.color := tnfBlack;
                  RBLeftRotate(x.parent);
                  break;
                end;

            end
          else
            begin
              w := x.parent.left;
              if w.color = tnfRed then
                begin
                  w.color := tnfBlack;
                  x.parent.color := tnfRed;
                  RBRightRotate(x.parent);
                  w := x.parent.left;  // TODO: w becomes nil_node?
                end;

              if (w.right.color = tnfBlack) and (w.left.color = tnfBlack) then
                begin
                  w.color := tnfRed;
                  x := x.parent;
                end
              else
                begin
                  if w.left.color = tnfBlack then
                    begin
                      w.right.color := tnfBlack;
                      w.color := tnfRed;
                      RBLeftRotate(w);
                      w := x.parent.left;
                    end;
                  w.color := x.parent.color;
                  x.parent.color := tnfBlack;
                  w.left.color := tnfBlack;
                  RBRightRotate(x.parent);
                  break;
                end;
            end;

        end;
      x.color := tnfBlack;
    end;

  Result := y;

end;

function DRedBlackTree.startNode : DTreeNode;
begin
  Result := FHeader.left;
end;

function DRedBlackTree.endNode : DTreeNode;
begin
  Result := FHeader;
end;

////////////////////////////////////////////////////////////////////
//
// DContainer
//
////////////////////////////////////////////////////////////////////

constructor DContainer.Create;
begin
  comparator := DObjectComparator;
end;

procedure DContainer.SetComparator(const Value: DComparator);
begin
  fComparator := Value;
end;

procedure DContainer.addRef(const obj : DObject);
begin
  // It would be nice to come up with a more efficient way to do this...
  _add(obj);

  // This is a hack -- we want to be able to modify the thing this came from,
  // but we still want to be able to pass constant objects in (like returns
  // from unary or binary functions).  Be careful!
  ClearDObject(PDObject(@obj)^);
end;

procedure DContainer.Add(const objs : array of const);
var i : Integer;
begin
  for i := Low(objs) to High(objs) do
    _add(objs[i]);
end;

procedure DContainer.remove(const objs : array of const);
var i : Integer;
begin
  for i := Low(objs) to High(objs) do
    _remove(objs[i]);
end;

procedure DContainer.ensureCapacity(amount : Integer);
begin
  // This does nothing for most containers, but certain subclasses may
  // implement something here to add in speedy addition of items to a
  // container.
end;

procedure DContainer.trimToSize;
begin
  // This is a NOOP for most containers, but some may respond by packing
  // themselves more tightly.
end;

procedure DContainer.clear;
begin
  _clear(false);
end;

constructor DContainer.CreateWith(compare : Dcomparator);
begin
  comparator := compare;
end;

procedure DContainer.cloneTo(newContainer : DContainer);
begin
  CopyInTo(start, finish, newContainer.finish);
end;

procedure DContainer._iput(const iterator : DIterator; const objs : array of const);
var i : Integer;
begin
  for i := High(objs) to Low(objs) do
    iput(iterator, objs[i]);
end;

procedure DContainer.iadvanceBy(var iterator : DIterator; count : Integer);
begin
  while count > 0 do
    begin
      advance(iterator);
      dec(count);
    end;
end;

function DContainer.iatStart(const iterator : DIterator) : Boolean;
begin
  Result := diMarkStart in iterator.flags;
end;

function DContainer.iatEnd(const iterator : DIterator) : Boolean;
begin
  Result := diMarkFinish in iterator.flags;
end;

function DContainer.igetContainer(const iterator : DIterator) : DContainer;
begin
  Result := self;
end;

//
// This is a real dumb distance function, but can work on any iterator type.
//
function DContainer.idistance(const iter1, iter2 : DIterator) : Integer;
var i : DIterator;
begin
  Result := 0;
  i := iter1;
  while not DeCAL.equals(i, iter2) do
    begin
      Inc(result);
      advance(i);
    end;
end;

// bidirectional
procedure DContainer.iretreat(var iterator : DIterator);
begin
  raise DNeedBidirectional.Create;
end;

procedure DContainer.iretreatBy(var iterator : DIterator; count : Integer);
begin
  while count > 0 do
    begin
      iretreat(iterator);
      Dec(count);
    end;
end;

function DContainer.igetAt(const iterator : DIterator; offset : Integer) : PDObject;
var iter : DIterator;
begin
  iter := iterator;
  if offset > 0 then
    advanceBy(iter, offset)
  else if offset < 0 then
    retreatBy(iter, -offset)
  else
    iter := iterator;
  Result := iget(iter);
end;

procedure DContainer.iputAt(const iterator : DIterator; offset : Integer; const obj : DObject);
var iter : DIterator;
begin
  iter := iterator;
  if offset > 0 then
    advanceBy(iter, offset)
  else if offset < 0 then
    retreatBy(iter, -offset)
  else
    iter := iterator;
  iput(iter, obj);
end;

// random
function DContainer.iindex(const iterator : DIterator) : Integer;
begin
  raise DNeedRandom.Create;
end;

function DContainer.iless(const iter1, iter2 : DIterator) : Boolean;
begin
  raise DNeedRandom.Create;
end;

procedure DContainer.iflagChange(var iterator : DIterator; oldflags : DIteratorFlags);
begin
end;

function DContainer.clone : DContainer;
var cls : DContainerClass;
begin
  cls := DContainerClass(classtype);
  Result := cls.CreateWith(comparator); // Create is virtual!
  cloneTo(result);
end;

function DContainer.isEmpty : Boolean;
begin
  Result := size = 0;
end;

function DContainer.size : Integer;
begin
  Result := distance(start, finish);
end;

function DContainer._contains(const obj : DObject) : Boolean;
var i : DIterator;
    compare : DComparator;
begin
  i := start;
  getComparator(compare);
  while (compare(get(i),obj) <> 0) and (not atEnd(i)) do
    advance(i);
  Result := not atEnd(i);
end;

function DContainer.contains(const objs : array of const) : Boolean;
var i : Integer;
begin
  Result := true;
  for i := Low(objs) to High(objs) do
    Result := result and _contains(objs[i]);
end;

function DContainer._count(const obj : DObject) : Integer;
var iter : DIterator;
begin
  Result := 0;
  iter := start;
  while not atEnd(iter) do
    begin
      if Comparator(obj, getRef(iter)^) = 0 then
        Inc(result);
      advance(iter);
    end;
end;

function DContainer.count(const objs : array of const) : Integer;
var i : Integer;
begin
  Result := 0;
  for i := Low(objs) to High(objs) do
    Result := result + _count(objs[i]);
end;

function DContainer.usesPairs : Boolean;
begin
  Result := false;
end;

function DContainer.binaryCompare(const obj1, obj2 : DObject) : Integer;
begin
  Result := comparator(obj1, obj2);
end;

function DContainer.binaryTest(const obj1, obj2 : DObject) : Boolean;
begin
  Result := comparator(obj1, obj2) = 0;
end;

procedure DContainer.getComparator(var compare : DComparator);
begin
  compare := comparator;
end;

procedure DContainer.getBinaryTest(var bt : DBinaryTest);
begin
  bt := binaryTest;
end;

//
// Do a simple, stupid comparison of pointers.  This should be changed to
// at do, at least, some elementary hashing on the objects in question.
// Some kind of hash on SizeOf(DObject) bytes at each address would be cool.
//
function DContainer.hashComparator(const obj1, obj2 : DObject) : Integer;
var h1, h2 : Integer;
begin
  h1 := HashCode(obj1);
  h2 := HashCode(obj2);
  if h1 < h2 then
    Result := -1
  else if h1 = h2 then
    Result := 0
  else
    Result := 1;
end;

function DContainer.CaselessStringComparator(const obj1, obj2 : DObject) : Integer;
begin
  Assert(obj1.Vtype = obj2.VType);
  {$IFDEF UNICODE}
  if obj1.VType = vtUnicodeString then
    Result := CompareText(UnicodeString(obj1.VUnicodeString), UnicodeString(obj2.VUnicodeString))
  else
  {$ENDIF UNICODE}
  if obj1.VType = vtWideString then
    Result := WideCompareText(WideString(obj1.VWideString), WideString(obj2.VWideString))
  else
  begin
    Assert(obj1.VType = vtAnsiString);
    Assert(obj1.Vtype = obj2.VType);
    Result := CompareText(AnsiString(obj1.VAnsiString), AnsiString(obj2.VAnsiString));
  end;
end;

{** DObjectComparator intelligently compares the atomic types, automatically. }
function DContainer.DObjectComparator(const obj1, obj2 : DObject) : Integer;
  function SignExt(const value : Extended) : Integer;
  begin
    if value < 0 then
      Result := -1
    else if value = 0 then
      Result := 0
    else
      Result := 1;
  end;
  function SignCurrency(const value : Currency) : Integer;
  begin
    if value < 0 then
      Result := -1
    else if value = 0 then
      Result := 0
    else
      Result := 1;
  end;
begin

  Result := 0;

  Assert(obj1.VType = obj2.Vtype);

  case obj1.VType of
      vtInteger:
        Result := obj1.VInteger - obj2.VInteger;
      vtBoolean:
        Result := Ord(obj1.VBoolean) - Ord(obj2.VBoolean);
      vtChar:
        Result := Ord(obj1.VChar) - Ord(obj2.VChar);
      vtExtended:
        Result := SignExt(obj1.VExtended^ - obj2.VExtended^);
      vtString:
        Result := CompareStr(obj1.VString^, obj2.VString^);
      vtPointer:
        Result := Integer(obj1.VPointer) - Integer(obj2.VPointer);
      vtPChar:
        Result := Integer(obj1.VPChar) - Integer(obj2.VPChar);
      vtObject:
        Result := Integer(obj1.VObject) - Integer(obj2.VObject);
      vtClass:
        Result := Integer(obj1.VClass) - Integer(obj2.VClass);
      vtWideChar:
        Result := Ord(obj1.VWideChar) - Ord(obj2.VWideChar);
      vtPWideChar:
        Result := Integer(obj1.VPWideChar) - Integer(obj2.VPWideChar);
      vtAnsiString:
        Result := CompareStr(AnsiString(obj1.VAnsiString), AnsiString(obj2.VAnsiString));
      {$IFDEF UNICODE}
      vtUnicodeString:
        Result := CompareStr(UnicodeString(obj1.VUnicodeString), UnicodeString(obj2.VUnicodeString));
      {$ENDIF UNICODE}
      vtCurrency:
        Result := SignCurrency(obj1.VCurrency^ - obj2.VCurrency^);
      vtVariant:
        raise DException.Create('variant type comparisons not implemented yet');
      vtInterface:
        Result := Integer(obj1.VInterface) - Integer(obj2.VInterface);
      vtWideString:
        if WideString(obj1.VWideString) < WideString(obj2.VWideString) then
          Result := -1
        else if WideString(obj1.VWideString) = WideString(obj2.VWideString) then
          Result := 0
        else
          Result := 1;
  end;

{$IFDEF NOTDEFINED}
  {$IFDEF FREEPOSSIBLE}
  if obj1 is DComposite then
    begin
      Result := DComposite(obj1).Compare(DComposite(obj2));
    end
  else
    begin
      comparator := hashComparator;
      Result := hashComparator(obj1, obj2);
    end;
  {$ELSE}
  // regress to a more primitive hashing scheme.
  comparator := hashComparator;
  Result := hashComparator(obj1, obj2);
  {$ENDIF}
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////
//
// DSequence
//
////////////////////////////////////////////////////////////////////

procedure DSequence._add(const obj : DObject);
begin
  pushBack(obj);
end;

function DSequence.at(pos : Integer) : DObject;
var i : DIterator;
begin
  i := start;
  advanceBy(i,pos);
  Result := get(i);
end;

function DSequence.atRef(pos : Integer) : PDObject;
begin
  Result := _at(pos);
end;

function DSequence._at(pos : Integer) : PDObject;
var i : DIterator;
begin
  i := start;
  advanceBy(i,pos);
  Result := getRef(i);
end;

function DSequence.backRef : PDObject;
var i : DIterator;
begin
  i := finish;
  retreat(i);
  Result := getRef(i);
end;

function DSequence.back : DObject;
begin
  CopyDObject(backRef^, result);
end;

function DSequence._countWithin(_begin, _end : Integer; const obj : DObject) : Integer;
var i : DIterator;
    compare : DComparator;
begin
  Result := 0;
  i := start;
  getComparator(compare);
  if _begin > 0 then
    advanceBy(i,_begin);
  while _begin <= _end do
    begin
      if compare(getRef(i)^,obj) = 0 then
        Inc(result);
      advance(i);
      Inc(_begin);
    end;
end;

function DSequence.countWithin(_begin, _end : Integer; const objs : array of const) : Integer;
var i : Integer;
begin
  Result := 0;
  for i := Low(objs) to High(objs) do
    Result := result + _countWithin(_begin, _end, objs[i]);
end;

function DSequence.frontRef : PDObject;
var i : DIterator;
begin
  i := start;
  Result := getRef(i);
end;

function DSequence.front : DObject;
begin
  CopyDObject(frontRef^, result);
end;

function DSequence._indexOf(const obj : DObject) : Integer;
begin
  Result := indexOfWithin(0, size - 1, obj);
end;

function DSequence.indexOf(const objs : array of const) : Integer;
begin
  if High(objs) <> Low(objs) then
    raise DException.Create('array must be only one object');
  Result := _indexOf(objs[Low(objs)]);
end;

function DSequence._indexOfWithin(_begin, _end : Integer; const obj : DObject) : Integer;
var i : DIterator;
    compare : DComparator;
begin
  Result := -1;
  i := start;
  getComparator(compare);
  if _begin > 0 then
    advanceBy(i, _begin);
  while _begin <= _end do
    begin
      if compare(obj,getRef(i)^) = 0 then
        begin
          Result := _begin;
          exit;
        end;
      advance(i);
      Inc(_begin);
    end;
end;

function DSequence.indexOfWithin(_begin, _end : Integer; const objs : array of const) : Integer;
begin
  if High(objs) <> Low(objs) then
    raise DException.Create('array must be only one object');
  Result := _indexOfWithin(_begin, _end, objs[Low(objs)]);
end;

function DSequence.iremove(const iterator: DIterator): DIterator;
begin
  Result := removeAtIter(iterator, 1);
end;

procedure DSequence._putAt(index : Integer; const obj : DObject);
var i : DIterator;
begin
  i := start;
  if index > 0 then
    advanceBy(i, index);
  _put(i, obj);
end;

procedure DSequence.putAt(index : Integer; const objs : array of const);
var i : Integer;
begin
  for i := High(objs) to Low(objs) do
    _putAt(index, objs[i]);
end;

procedure DSequence._remove(const obj : DObject);
begin
  removeWithin(0, size, obj);
end;

procedure DSequence._replace(obj1, obj2 : DObject);
begin
  replaceWithin(0, size, obj1, obj2);
end;

procedure DSequence.replace(const sources, targets : array of const);
var i : Integer;
begin
  for i := Low(sources) to High(sources) do
    _replace(sources[i], targets[i]);
end;

procedure DSequence._replaceWithin(_begin, _end : Integer; obj1, obj2 : DObject);
var i : DIterator;
    compare : DComparator;
begin
  i := start;
  getComparator(compare);
  if _begin > 0 then
    advanceBy(i,_begin);
  while _begin <= _end do
    begin
      if compare(getRef(i)^,obj1) = 0 then
        DeCAL._put(i,obj2);
      advance(i);
      Inc(_begin);
    end;
end;

procedure DSequence.replaceWithin(_begin, _end : Integer; const sources, targets : array of const);
var i : Integer;
begin
  for i := Low(sources) to High(sources) do
    _replaceWithin(_begin, _end, sources[i], targets[i]);
end;

procedure DSequence.removeWithin(_begin, _end : Integer; const objs : array of const);
var i : Integer;
begin
  for i := Low(objs) to High(objs) do
    _removeWithin(_begin, _end, objs[i]);
end;

procedure DSequence.pushBack(const objs : array of const);
var i : Integer;
begin
  for i := Low(objs) to High(objs) do
    _pushBack(objs[i]);
end;

procedure DSequence.pushFront(const objs : array of const);
var i : Integer;
begin
  for i := Low(objs) to High(objs) do
    _pushFront(objs[i]);
end;

{** Retrieves at an index as an integer. Asserts if the type is not correct. }
function DSequence.atAsInteger(pos : Integer) : Integer;
begin
  Result := asInteger(_at(pos)^);
end;

{** Retrieves at an index as a Boolean. Asserts if the type is not correct. }
function DSequence.atAsBoolean(pos : Integer) : Boolean;
begin
  Result := asBoolean(_at(pos)^);
end;

{** Retrieves at an index as a Char. Asserts if the type is not correct. }
function DSequence.atAsAnsiChar(pos : Integer) : AnsiChar;
begin
  Result := asAnsiChar(_at(pos)^);
end;

{** Retrieves at an index as a Char. Asserts if the type is not correct. }
function DSequence.atAsChar(pos : Integer) : Char;
begin
  Result := asChar(_at(pos)^);
end;

{** Retrieves at an index as an extended floating point value. Asserts if the type is not correct. }
function DSequence.atAsExtended(pos : Integer) : Extended;
begin
  Result := asExtended(_at(pos)^);
end;

{** Retrieves at an index as a short string.Asserts if the type is not correct. }
function DSequence.atAsShortString(pos : Integer) : ShortString;
begin
  Result := asShortString(_at(pos)^);
end;

{** Retrieves at an index as an untyped pointer.Asserts if the type is not correct. }
function DSequence.atAsPointer(pos : Integer) : Pointer;
begin
  Result := asPointer(_at(pos)^);
end;

{** Retrieves at an index as a PChar. Asserts if the type is not correct. }
function DSequence.atAsPAnsiChar(pos : Integer) : PAnsiChar;
begin
  Result := asPAnsiChar(_at(pos)^);
end;

{** Retrieves at an index as a PChar. Asserts if the type is not correct. }
function DSequence.atAsPChar(pos : Integer) : PChar;
begin
  Result := asPChar(_at(pos)^);
end;

{** Retrieves at an index as an object reference.  Asserts if the type is not correct. }
function DSequence.atAsObject(pos : Integer) : TObject;
begin
  Result := asObject(_at(pos)^);
end;

{** Retrieves at an index as a class reference (TClass). Asserts if the type is not correct. }
function DSequence.atAsClass(pos : Integer) : TClass;
begin
  Result := asClass(_at(pos)^);
end;

{** Retrieves at an index as a WideChar. Asserts if the type is not correct. }
function DSequence.atAsWideChar(pos : Integer) : WideChar;
begin
  Result := asWideChar(_at(pos)^);
end;

{** Retrieves at an index as a pointer to a WideChar. Asserts if the type is not correct. }
function DSequence.atAsPWideChar(pos : Integer) : PWideChar;
begin
  Result := asPWideChar(_at(pos)^);
end;

{** Retrieves at an index as a AnsiString. Asserts if the type is not correct. }
function DSequence.atAsAnsiString(pos : Integer) : AnsiString;
begin
  Result := asAnsiString(_at(pos)^);
end;

{** Retrieves at an index as a string. Asserts if the type is not correct. }
function DSequence.atAsString(pos : Integer) : string;
begin
  Result := asString(_at(pos)^);
end;

{** Retrieves at an index as a currency value. Asserts if the type is not correct. }
function DSequence.atAsCurrency(pos : Integer) : Currency;
begin
  Result := asCurrency(_at(pos)^);
end;

{** Retrieves at an index as a variant. Asserts if the type is not correct. }
function DSequence.atAsVariant(pos : Integer) : Variant;
begin
  Result := asVariant(_at(pos)^);
end;

{** Retrieves at an index as an interface pointer. Asserts if the type is not correct. }
function DSequence.atAsInterface(pos : Integer) : Pointer;
begin
  Result := asInterface(_at(pos)^);
end;

{** Retrieves at an index as a WideString. Asserts if the type is not correct. }
function DSequence.atAsWideString(pos : Integer) : WideString;
begin
  Result := asWideString(_at(pos)^);
end;

{$IFDEF USELONGWORD}
function DSequence.atAsInt64(pos : Integer) : Int64;
begin
  Result := asInt64(_at(pos)^);
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////
//
// DVector
//
////////////////////////////////////////////////////////////////////

procedure DVector.insertAtIter(iterator : DIterator; const objs : array of const);
var i : Integer;
begin
  for i := Low(objs) to High(objs) do
    _insertAtIter(iterator, objs[i]);
end;

procedure DVector.insertAt(index : Integer; const objs : array of const);
var i : Integer;
begin
  for i := Low(objs) to High(objs) do
    _insertAt(index, objs[i]);
end;

procedure DVector.insertMultipleAtIter(iterator : DIterator; count : Integer; const obj : array of const);
var i : Integer;
begin
  Assert(low(obj) = high(obj), 'Can do this only with one item');
  for i := Low(obj) to High(obj) do
    _insertMultipleAtIter(iterator, count, obj[i]);
end;

procedure DVector.insertMultipleAt(index : Integer; count : Integer; const obj : array of const);
var i : Integer;
begin
  Assert(low(obj) = high(obj), 'Can do this only with one item');
  for i := Low(obj) to High(obj) do
    _insertMultipleAt(index, count, obj[i]);
end;

function DVector.legalIndex(index: Integer): Boolean;
begin
  Result := (index < size) and (index >= 0);
end;

procedure DVector.removeWithin(_begin, _end : Integer; const objs : array of const);
var i : Integer;
begin
  for i := Low(objs) to High(objs) do
    _removeWithin(_begin, _end, objs[i]);
end;

////////////////////////////////////////////////////////////////////
//
// DList
//
////////////////////////////////////////////////////////////////////

constructor DListNode.Create(const anobj : DObject);
begin
  CopyDObject(anObj, obj);
  next := nil;
  previous := nil;
end;

destructor DListNode.Destroy;
begin
  ClearDObject(obj);
  inherited;
end;

destructor DListNode.Kill;
begin
  inherited Destroy;
end;

constructor DList.Create;
begin
  inherited Create;
  head := nil;
  tail := nil;
  length := 0;
end;

constructor DList.CreateWith(compare : DComparator);
begin
  inherited CreateWith(compare);
  head := nil;
  tail := nil;
  length := 0;
end;

destructor DList.Destroy;
begin
  clear;
  inherited;
end;

procedure DList._add(const obj : DObject);
begin
  pushBack(obj);
end;

procedure DList._remove(const obj : DObject);
var this, next : DListNode;
begin
  this := head;
  while this <> nil do
    begin
      next := this.next;
      if comparator(this.obj, obj) = 0 then
        removeNode(this);
      this := next;
    end;
end;

procedure DList._clear(direct : Boolean);
var this, next : DListNode;
begin
  this := head;
  while this <> nil do
    begin
      next := this.next;
      if not direct then
        this.free
      else
        this.kill;
      this := next;
    end;
  head := nil;
  tail := nil;
  length := 0;
end;

{function DList.clone : DContainer;
begin
  Result := DList.Create;
  copyContainer(self, result);
end;
}

function DList.finish : DIterator;
begin
  with result do
    begin
      flags := [diBidirectional, diMarkFinish];
      Handler := self;
      dnode := nil;
    end;
end;

function DList.maxSize : Integer;
begin
  Result := MaxInt;
end;

function DList.start : DIterator;
begin
  with result do
    begin
      if size = 0 then
        flags := [diBidirectional, diMarkFinish]
      else
        flags := [diBidirectional, diMarkStart];
      Handler := self;
      dnode := head;
    end;
end;

//
// DSequence overrides;
//
function DList.backRef : PDObject;
begin
  Assert(length > 0);
  if tail <> nil then
    Result := @tail.obj
  else
    raise DEmpty.Create;
end;

function DList.frontRef : PDObject;
begin
  Assert(length > 0);
  if head <> nil then
    Result := @head.obj
  else
    raise DEmpty.Create;
end;

function DList.popBack : DObject;
begin
  Assert(length > 0);
  if tail <> nil then
    begin
      Dec(length);
      MoveDObject(tail.obj, result);
      if head = tail then
        begin
          tail.free;
          head := nil;
          tail := nil;
        end
      else
        begin
          tail := tail.previous;
          tail.next.free;
          tail.next := nil;
        end;
    end
  else
    raise DEmpty.Create;
end;

function DList.popFront : DObject;
begin
  Assert(length > 0);
  if head <> nil then
    begin
      Dec(length);
      MoveDObject(head.obj, result);
      if head = tail then
        begin
          head.free;
          head := nil;
          tail := nil;
        end
      else
        begin
          head := head.next;
          head.previous.free;
          head.previous := nil;
        end;
    end
  else
    raise DEmpty.Create;
end;

procedure DList._pushBack(const obj : DObject);
var n : DListNode;
begin
  if head = nil then
    begin
      head := DListNode.Create(obj);
      tail := head;
    end
  else
    begin
      n := DListNode.Create(obj);
      n.previous := tail;
      tail.next := n;
      tail := n;
    end;
  Inc(length);
end;

procedure DList._pushFront(const obj : DObject);
var n : DListNode;
begin
  if head = nil then
    begin
      head := DListNode.Create(obj);
      tail := head;
    end
  else
    begin
      n := DListNode.Create(obj);
      n.next := head;
      head.previous := n;
      head := n;
    end;
  Inc(length);
end;

procedure DList._removeWithin(_begin, _end : Integer; const obj : DObject);
var i1, i2 : DIterator;
    cnt : Integer;
begin
  if _begin <> _end then
    begin
      i1 := start;
      advanceBy(i1, _begin);
      cnt := _end - _begin;
      while cnt > 0 do
        begin
          // Does this one match?
          i2 := i1;
          advance(i2);
          if comparator(i1.dnode.obj, obj) = 0 then
            removeNode(i1.dnode);
          i1 := i2;
          Dec(cnt);
        end;
    end;
end;

procedure DList.removeNode(node : DListNode);
begin
  if node.previous <> nil then
    node.previous.next := node.next
  else
      head := node.next;
  if node.next <> nil then
    node.next.previous := node.previous
  else
    tail := node.previous;
  node.free;
  Dec(length);
end;

//
// Remove a range of nodes.  s and f are the starting and finishing nodes,
// respectively.  s must always point to a valid node.  the node supplied
// as f will NOT be deleted, but everything up until it will be.  if deletion
// until the end is desired, pass nil as f.
//
procedure DList.removeRange(s,f : DListNode);
var n : DListNode;
begin
  Assert(s <> nil);
  if s = f then
    exit;

  // Patch the chain
  if s.previous <> nil then
    s.previous.next := f
  else
    head := f;
  if f <> nil then
    f.previous := s.previous
  else
    tail := s.previous;

  // remove orphaned nodes.
  repeat
    n := s.next;
    s.free;
    s := n;
    Dec(length);
  until s = f;

  {
  if s.previous <> nil then
    s.previous.next := f.next
  else
    head := f.next;
  if f.next <> nil then
    f.next.previous := s.previous
  else
    tail := s.previous;

  // Remove all orphaned nodes.
  repeat
    n := s.next;
    s.free;
    s := n;
    Dec(length);
  until s = f;
  }
end;

procedure DList.cut(_start, _finish : DIterator);
var s,f : DListNode;
begin
  if diMarkFinish in _start.flags then
    exit;

  s := _start.dnode;
  if atEnd(_finish) then
    f := nil
  else
    f := _finish.dnode;
  removeRange(s,f);
end;

procedure DList._insertAtIter(iterator : DIterator; const obj : DObject);
var nd : DListNode;
begin
  if iterator.dnode = nil then
    pushBack(obj)
  else
    begin
      // split the list nodes.
      nd := DListNode.Create(obj);
      nd.previous := iterator.dnode.previous;
      iterator.dnode.previous := nd;
      nd.next := iterator.dnode;
      if head = iterator.dnode then
        head := nd;
      Inc(length);
    end;
end;

procedure DList.insertAtIter(iterator : DIterator; const objs : array of const);
var i : Integer;
begin
  // We go backwards here so that the objects will end up in the order they
  // were passed in.
  for i := High(objs) to Low(objs) do
    _insertAtIter(iterator, objs[i]);
end;

function DList.removeAtIter(iter : DIterator; count : Integer) : DIterator;
var after : DIterator;
begin
  after := iter;
  advanceBy(after, count);
  Result := after;
  cut(iter, after);
end;

procedure DList.mergeSort;
begin
  mergeSortWith(Comparator);
end;

procedure DList.mergeSortWith(compare : DComparator);
begin
  _mergeSort(head, tail, compare);
end;

procedure DList._mergeSort(var s, f : DListNode; compare : DComparator);
begin
  raise DNotImplemented.Create;
{	if s <> f then
    begin

    end; }
end;

//
// Iterator manipulation.
//
procedure DList.iadvance(var iterator : DIterator);
begin
  with iterator do
    begin
      if dnode = nil then
        begin
          raise DException.Create('Can''t advance.');
        end
      else if dnode.next <> nil then
        begin
          dnode := dnode.next;
          flags := flags - [diMarkStart, diMarkFinish];
        end
      else
        begin
          dnode := nil;
          flags := flags - [diMarkStart] + [diMarkFinish];
        end;
    end;
end;

function DList.iget(const iterator : DIterator) : PDObject;
begin
  Result := @iterator.dnode.obj;
end;

function DList.iequals(const iter1, iter2 : DIterator) : Boolean;
begin
  Result := iter1.dnode = iter2.dnode;
end;

procedure DList.iput(const iterator : DIterator; const obj : DObject);
begin
  if atEnd(iterator) then
    _add(obj)
  else
    _SetDObject(iterator.dnode.obj, obj);
end;

function DList.iremove(const iterator : DIterator) : DIterator;
begin
  Assert(not atEnd(iterator));
  Result := iterator;
  iadvance(result);
  removeAtIter(iterator, 1);
end;

// bidirectional
procedure DList.iretreat(var iterator : DIterator);
begin
  with iterator do
    begin
      if (diMarkFinish in flags) or (dnode = nil) then
        begin
          dnode := tail;
          if dnode.previous = nil then
            flags := flags - [diMarkFinish] + [diMarkStart]
          else
            flags := flags - [diMarkStart, diMarkFinish];
        end
      else if (diMarkStart in flags) or (dnode.previous = nil) then
        raise DException.Create('Can''t retreat')
      else
        begin
          dnode := dnode.previous;
          if dnode.previous = nil then
            flags := flags - [diMarkFinish] + [diMarkStart]
          else
            flags := flags - [diMarkStart, diMarkFinish];
        end;
    end;
end;

function DList.size : Integer;
begin
  Result := length;
end;


////////////////////////////////////////////////////////////////////
//
// DArray
//
////////////////////////////////////////////////////////////////////

constructor DArray.Create;
begin
  inherited;
  createSize(DefaultArraySize);
end;

constructor DArray.CreateWith(compare : Dcomparator);
begin
  inherited CreateWith(compare);
  createSize(DefaultArraySize);
end;

constructor DArray.createSize(size : Integer);
begin
  items := DeCALAlloc(size * SizeOf(DObject));
  InitDObjects(items^[0], size);
  len := 0;
  cap := size;
  blocking := 4;
end;

destructor DArray.Destroy;
begin
  Clear;
  DeCALFree(items);
end;

function DArray.Capacity : Integer;
begin
  Result := cap;
end;

//
// DContainer overrides;
//
procedure DArray._add(const obj : DObject);
begin
  insertAt(len, obj);
end;

procedure DArray._clear(direct : Boolean);
begin
  if not direct then
    ClearDObjects(items^[0], len);
  len := 0;
end;

{
function Darray.clone : DContainer;
begin
  Result := DArray.Create;
  DArray(result).copy(self);
end;
}

function Darray.finish : DIterator;
begin
  with result do
    begin
      flags := [diRandom, diMarkFinish];
      Handler := self;
      position := len;
    end;
end;

function DArray.maxSize : Integer;
begin
  Result := cap;
end;

function DArray.size : Integer;
begin
  Result := len;
end;

function DArray.start : DIterator;
begin
  with result do
    begin
      if size = 0 then
        flags := [diRandom, diMarkFinish]
      else
        flags := [diRandom, diMarkStart];
      Handler := self;
      position := 0;
    end;
end;

//
// DSequence overrides;
//
function DArray.at(pos : Integer) : DObject;
begin
  Assert(len > 0);
  result := items^[pos];
end;

function DArray._at(pos : Integer) : PDObject;
begin
  Assert(pos < len);
  Result := @items^[pos];
end;

function DArray.backRef : PDObject;
begin
  Assert(len > 0);
  Result := @items^[len-1];
end;

function DArray.frontRef : PDObject;
begin
  Assert(len > 0);
  Result := @items^[0];
end;

function DArray.popBack : DObject;
begin
  Assert(len > 0);
  MoveDObject(items^[len-1], result);
  Dec(len);
end;

function DArray.popFront : DObject;
begin
  Assert(len > 0);
  MoveDObject(items^[0], result);
  removeSpaceAt(0, 1);
end;

procedure DArray._pushBack(const obj : DObject);
begin
  _add(obj);
end;

procedure DArray._pushFront(const obj : DObject);
begin
  insertAt(0, obj);
end;

procedure DArray._putAt(index : Integer; const obj : DObject);
begin
  Ensurecapacity(index + 1);
  _SetDObject(items^[index], obj);
  if len < index + 1 then
    len := index + 1;
end;

function DArray.removeAtIter(iter : DIterator; count : Integer) : DIterator;
begin
  removeBetween(iter.position, iter.position + count);
  Result := start;
  advanceBy(result, iter.position);
end;

//
// DArray specific
//
procedure DArray.copy(another : DArray);
var i : Integer;
begin
  clear;
  ensureCapacity(another.len);
  i := 0;
  while i < another.len do
    begin
      CopyDObject(another.items^[i], items^[i]);
      Inc(i);
    end;
  len := another.len;
end;

procedure DArray.copyTo(another : DArray);
begin
  another.copy(self);
end;

function DArray.blockFactor : Integer;
begin
  Result := blocking;
end;

procedure DArray.setBlockFactor(factor : Integer);
begin
  Assert(factor > 0);
  blocking := factor;
end;

procedure DArray.setCapacity(amount : Integer);
var na : PDObjectArray;
begin
  if cap <> amount then
    begin
      na := DeCALAlloc(SizeOf(DObject) * amount);
      InitDObjects(na^[0], amount);
      if amount < len then
        len := amount;
      Move(items^[0], na^[0], SizeOf(DObject) * len);
      cap := amount;
      DeCALFree(items);
      items := na;
    end;
end;

procedure DArray.ensureCapacity(amount : Integer);
var increment : Integer;
begin
  if cap < amount then
    begin

      // This is our adaptive array blocking system.  This makes
      // insertion into large arrays substantially more efficient.
      increment := cap div blocking;
      if increment < 16 then
        increment := 16;

      setCapacity(amount + increment);
    end;
end;

procedure DArray.setSize(newSize : Integer);
begin
  if newSize > cap then
    ensureCapacity(newSize);
  len := newSize;
end;

procedure DArray._insertAtIter(iterator : DIterator; const obj : DObject);
begin
  insertAt(iterator.position, obj);
end;

{function DArray.addressOf(index : Integer) : PDObject;
begin
  Result := PDObject(Integer(items) + SizeOf(DObject) * index);
end;}

procedure DArray._insertAt(index : Integer; const obj : DObject);
begin
  insertMultipleAt(index, 1, obj);
end;

procedure DArray._insertMultipleAtIter(iterator : DIterator; count : Integer; const obj : DObject);
begin
  insertMultipleAt(iterator.position, count, obj);
end;

function DArray.makeSpaceAt(index, count : Integer) : PDObject;
begin
  setSize(len + count);
  Move(items^[index], items^[index + count], SizeOf(DObject) * (len - index - count));
  InitDobjects(items^[index], count);
  Result := @items^[index];
end;

procedure DArray.removeSpaceAt(index, count : Integer);
begin
  ClearDObjects(items^[index], count);
  Move(items^[index + count], items^[index], SizeOf(DObject) * (len - index - count));
  len := len - count;
end;

function DArray.iterFor(index : Integer) : DIterator;
begin
  with result do
    begin
      flags := [diRandom];
      Handler := self;
      position := index;
      if index = len then
        flags := flags + [diMarkFinish]
      else if index = 0 then
        flags := flags + [diMarkStart];
    end;
end;

procedure DArray._insertMultipleAt(index : Integer; count : Integer; const obj : DObject);
begin
  Assert(index <= len);
  makeSpaceAt(index, count);
  while count > 0 do
    begin
      CopyDObject(obj, items^[index]);
      Inc(index);
      Dec(count);
    end;
end;

procedure DArray.insertRangeAtIter(iterator : DIterator; _start, _finish : DIterator);
begin
  insertRangeAt(iterator.position, _start, _finish);
end;

procedure DArray.insertRangeAt(index : Integer; _start, _finish : DIterator);
var count : Integer;
begin
  count := distance(_start, _finish);
  if count > 0 then
    begin
      makeSpaceAt(index, count);
      while not DeCAL.equals(_start, _finish) do
        begin
          CopyDObject(getRef(_start)^, items^[index]);
          Inc(index);
          advance(_start);
        end;
    end;
end;

procedure DArray._remove(const obj : DObject);
begin
  if size > 0 then
    _removeWithin(0, size, obj);
end;

procedure DArray.removeAt(index : Integer);
begin
  removeSpaceAt(index, 1);
end;

procedure DArray.removeBetween(_begin, _end : Integer);
begin
  removeSpaceAt(_begin, _end - _begin);
end;

procedure DArray._removeWithin(_begin, _end : Integer; const obj : DObject);
var i,j : Integer;
    pr : PDObject;
begin
  i := _begin;
  j := i;
  while i < _end do
    begin
      if comparator(_at(i)^, obj) <> 0 then
        begin
          if i <> j then
            begin
              pr := _at(j);
              ClearDObject(pr^);
              MoveDObject(_at(i)^, pr^);
            end;
          Inc(j);
        end;
      Inc(i);
    end;
  setSize(j);
end;

procedure DArray.trimToSize;
begin
  setCapacity(len);
end;

procedure DArray.iadvance(var iterator : DIterator);
var sz : Integer;
begin
  with iterator do
    begin
      Inc(position);
      sz := size;
      if position >= sz then
        begin
          flags := flags - [diMarkStart] + [diMarkFinish];
          position := sz;
        end
      else if position > 0 then
        begin
          flags := flags - [diMarkStart, diMarkFinish];
          // position := sz;
        end;
    end;
end;

procedure DArray.iadvanceBy(var iterator: DIterator; count : Integer);
var sz : Integer;
begin
  with iterator do
    begin
      Inc(position, count);
      sz := Handler.igetContainer(iterator).size;
      if position >= sz then
        begin
          flags := flags - [diMarkStart] + [diMarkFinish];
          if sz > 0 then
            position := sz
          else
            position := 0;
        end
      else if position <= 0 then
        begin
          position := 0;
          flags := flags + [diMarkStart] - [diMarkFinish];
        end
      else
        flags := flags - [diMarkStart, diMarkFinish];
    end;
end;

function DArray.idistance(const iter1, iter2 : DIterator) : Integer;
begin
  Result := iter2.position - iter1.position;
end;

function DArray.iequals(const iter1, iter2 : DIterator) : Boolean;
begin
  Result := iter1.position = iter2.position;
end;

function DArray.iget(const iterator : DIterator) : PDObject;
begin
  Assert(not atEnd(iterator));
  Result := _at(iterator.position);
end;

procedure DArray.iput(const iterator : DIterator; const obj : DObject);
begin
  putAt(iterator.position, obj);
end;

function DArray.iremove(const iterator : DIterator) : DIterator;
begin
  Result := iterator;
  removeAt(iterator.position);
  if Result.position >= size then
    begin
      Result := finish;
    end;
end;

function DArray.igetAt(const iterator : DIterator; offset : Integer) : PDObject;
begin
  Result := _at(iterator.position + offset);
end;

procedure DArray.iputAt(const iterator : DIterator; offset : Integer; const obj : DObject);
begin
  putAt(iterator.position + offset, obj);
end;

procedure DArray.iretreat(var iterator : DIterator);
begin
  with iterator do
    begin
      Dec(position);
      if position <= 0 then
        begin
          position := 0;
          flags := flags + [diMarkStart] - [diMarkFinish];
        end
      else
        flags := flags - [diMarkStart, diMarkFinish];
    end;
end;

procedure DArray.iretreatBy(var iterator : DIterator; count : Integer);
begin
  with iterator do
    begin
      Dec(position, count);
      if position <= 0 then
        begin
          position := 0;
          flags := flags + [diMarkStart] - [diMarkFinish];
        end
      else
        flags := flags - [diMarkStart, diMarkFinish];
    end;
end;

function DArray.iindex(const iterator : DIterator) : Integer;
begin
  Result := iterator.position;
end;

function DArray.iless(const iter1, iter2 : DIterator) : Boolean;
begin
  Result := iter1.position < iter2.position;
end;

////////////////////////////////////////////////////////////////////
//
// Associative base class
//
////////////////////////////////////////////////////////////////////

{function DAssociative.count(key : array of const) : Integer;
begin
  Result := _count(key[Low(key)]);
end;
}

function DAssociative.countValues(const value : array of const) : Integer;
begin
  Result := _countValues(value[Low(value)]);
end;

function DAssociative.getAt(const key : array of const) : DObject;
begin
  Result := _getAt(key[Low(key)]);
end;

function DAssociative.locate(const key : array of const) : DIterator;
begin
  // make sure there's only one
  Assert(Low(key) = High(key));
  Result := _locate(key[Low(key)]);
end;

procedure DAssociative.putAt(const key, value : array of const);
var i : Integer;
begin
  for i := Low(key) to High(key) do
    _putAt(key[i], value[i]);
end;

procedure DAssociative.putPair(const pair : array of const);
begin
  Assert(High(pair) = 1, 'You must pass exactly two items.');
  _putAt(pair[0], pair[1]);
end;

procedure DAssociative.removeValueN(const value : array of const; count : Integer);
begin
  _removeValueN(value[Low(value)], count);
end;

procedure DAssociative.removeValue(const value : array of const);
begin
  _removeValueN(value[Low(value)], MaxInt);
end;

////////////////////////////////////////////////////////////////////
//
// Maps
//
////////////////////////////////////////////////////////////////////

constructor DInternalMap.ProtectedCreate(always_insert : Boolean; compare : DComparator);
begin
  inherited CreateWith(compare);
  tree := DRedBlackTree.Create(self, always_insert, compare);
end;

constructor DInternalMap.ProtectedDuplicate(another : DInternalMap);
begin
  inherited CreateWith(another.comparator);
  tree := DRedBlackTree.Create(self, another.tree.FInsertAlways, another.comparator);
  tree.RBCopy(another.tree);
end;

destructor DInternalMap.Destroy;
begin
  tree.free;
  inherited;
end;

//
// Iterator manipulation.
//
procedure DInternalMap.iadvance(var iterator : DIterator);
begin
  with iterator do
    begin
      tree.RBincrement(treeNode);
      if treeNode = tree.endNode then
        flags := flags + [diMarkFinish] - [diMarkStart]
      else
        flags := flags - [diMarkFinish, diMarkStart];
    end;
end;

function DInternalMap.iget(const iterator : DIterator) : PDObject;
begin
  if diKey in iterator.flags then
    Result := @iterator.treeNode.pair.first
  else
    Result := @iterator.treeNode.pair.second;
end;

function DInternalMap.iequals(const iter1, iter2 : DIterator) : Boolean;
begin
  Result := iter1.treeNode = iter2.treeNode;
end;

procedure DInternalMap.iput(const iterator : DIterator; const obj : DObject);
begin
  if usesPairs then
    begin
      Assert(not atEnd(iterator));
      CopyDObject(obj, iterator.treeNode.pair.second);
    end
  else
    begin
      if atEnd(iterator) then
        _add(obj)
      else
        CopyDObject(obj, iterator.treeNode.pair.second);
    end;
end;

function DInternalMap.iremove(const iterator : DIterator) : DIterator;
begin
  Result := iterator;
  iadvance(result);
  removeAt(iterator);
end;

// bidirectional
procedure DInternalMap.iretreat(var iterator : DIterator);
begin
  with iterator do
    begin
      tree.RBdecrement(iterator.treeNode);
      if treeNode = tree.startNode then
        flags := flags + [diMarkStart] - [diMarkFinish]
      else
        flags := flags - [diMarkStart, diMarkFinish];
    end;
end;

function DInternalMap.igetAt(const iterator : DIterator; offset : Integer) : PDObject;
var iter : DIterator;
begin
  iter := iterator;
  if offset > 0 then
    iadvanceBy(iter, offset)
  else
    iretreatBy(iter, -offset);
  Result := getRef(iter);
end;

procedure DInternalMap.iputAt(const iterator : DIterator; offset : Integer; const obj : DObject);
var iter : DIterator;
begin
  iter := iterator;
  if offset > 0 then
    iadvanceBy(iter, offset)
  else
    iretreatBy(iter, -offset);
  _put(iter, obj);
end;

procedure DInternalMap._add(const obj : DObject);
begin
  raise DException.Create('Can''t add a single object to a map.');
end;

procedure DInternalMap._clear(direct : Boolean);
begin
  tree.erase(direct);
end;

procedure DInternalMap.cloneTo(newContainer : DContainer);
var iter : DIterator;
    k,v : PDObject;
begin
  Assert(newContainer.usesPairs);
  iter := start;
  while not atEnd(iter) do
    begin
      SetToKey(iter);
      k := getRef(iter);
      SetToValue(iter);
      v := getRef(iter);
      DAssociative(newContainer)._putAt(k^, v^);
      advance(iter);
    end;
end;

function DInternalMap.finish : DIterator;
begin
  Result := tree.finish;
  MorphIterator(result);
end;

function DInternalMap.maxSize : Integer;
begin
  Result := MaxInt;
end;

procedure DInternalMap.MorphIterator(var iterator : DIterator);
begin
  // Does nothing for most maps (it's here for sets).
end;

function DInternalMap.start : DIterator;
begin
  Result := tree.start;
  MorphIterator(result);
end;

function DInternalMap.size : Integer;
begin
  Result := tree.size;
end;

//
// Map stuff
//
function DInternalMap.allowsDuplicates : Boolean;
begin
  Result := false;
end;

function DInternalMap._count(const key : DObject) : Integer;
begin
  Result := tree.count(key);
end;

function DInternalMap._countValues(const value : DObject) : Integer;
var iter : DIterator;
begin
  iter := start;
  MorphIterator(iter);
  Result := 0;
  while not DeCAL.equals(iter, finish) do
    begin
      if comparator(getRef(iter)^, value) = 0 then
        Inc(result);
      advance(iter);
    end;
end;

function DInternalMap._getAt(const key : DObject) : DObject;
var iter : DIterator;
begin
  iter := tree.find(key);
  MorphIterator(iter);
  Assert(not atEnd(iter));
  Result := get(iter);
end;

function DInternalMap._locate(const key : DObject) : DIterator;
begin
  Result := tree.find(key);
  MorphIterator(result);
end;

function DInternalMap._lower_bound(const key : DObject) : DIterator;
begin
  Result := tree.lower_bound(key);
end;

function DInternalMap.lower_bound(const obj : array of const) : DIterator;
begin
  Assert(Low(obj) = High(obj));
  Result := _lower_bound(obj[Low(obj)]);
end;

function DInternalMap._upper_bound(const key : DObject) : DIterator;
begin
  Result := tree.upper_bound(key);
end;

function DInternalMap.upper_bound(const obj : array of const) : DIterator;
begin
  Result := _upper_bound(obj[Low(obj)]);
end;

procedure DInternalMap._putAt(const key, value : DObject);
var pair : DPair;
begin

  // This might be problematic.  When, exactly, do we copy the DObjects?
  // Should it be done by the internal tree, or should it be done by the
  // map object at this phase?  The problem is the replacement case, where
  // we are "putting" a value to a key that already exists.  if we copy here,
  // then we'll have a duplicate value unless we get rid of the old one first.
  // The answer is -- the internal tree handles it, at the node level.

  pair.first := key;
  pair.second := value;
  tree.insert(pair);
end;

procedure DInternalMap._remove(const key : DObject);
begin
  tree.eraseKey(key);
end;

procedure DInternalMap._removeN(const key : DObject; count : Integer);
begin
  tree.eraseKeyN(key, count);
end;

procedure DInternalMap._removeValueN(const value : DObject; count : Integer);
var iter, iterHold : DIterator;
begin
  iter := start;
  SetToValue(iter);
  while (count > 0) and (not atEnd(iter)) do
    begin
      if Comparator(value, getRef(iter)^) = 0 then
        begin
          iterHold := iter;
          advance(iter);
          tree.eraseAt(iterHold);
          Dec(count);
        end
      else
        advance(iter);
    end;
end;

procedure DInternalMap.removeAt(iterator : DIterator);
begin
  tree.eraseAt(iterator);
end;

procedure DInternalMap.removeIn(_start, _finish : DIterator);
begin
  tree.eraseIn(_start, _finish);
end;

procedure DInternalMap.SetComparator(const Value: DComparator);
begin
  inherited SetComparator(Value);
  if Tree <> nil then
    Tree.fComparator := Value;
end;

function DInternalMap.startKey : DIterator;
begin
  Result := start;
  Result.flags := Result.flags + [diKey];
end;

////////////////////////////////////////////////////////////////////
//
// Single key map
//
////////////////////////////////////////////////////////////////////

constructor DMap.Create;
begin
  ProtectedCreate(false, DObjectComparator);
end;

constructor DMap.CreateWith(compare : DComparator);
begin
  ProtectedCreate(false, compare);
end;

constructor DMap.CreateFrom(another : DMap);
begin
  ProtectedDuplicate(another);
end;

{function DMap.clone : DContainer;
begin
  Result := CreateFrom(self);
end;
}

function DMap.usesPairs : Boolean;
begin
  Result := false;
end;

constructor DMultiMap.Create;
begin
  ProtectedCreate(true, DObjectComparator);
end;

constructor DMultiMap.CreateWith(compare : DComparator);
begin
  ProtectedCreate(true, compare);
end;

constructor DMultiMap.CreateFrom(another : DMultiMap);
begin
  ProtectedDuplicate(another);
end;

function DMultiMap.usesPairs : Boolean;
begin
  Result := true;
end;

{
function DMultiMap.clone : DContainer;
begin
  Result := CreateFrom(self);
end;
}

constructor DSet.Create;
begin
  ProtectedCreate(false, DObjectComparator);
end;

constructor DSet.CreateWith(compare : DComparator);
begin
  ProtectedCreate(false, compare);
end;

constructor DSet.CreateFrom(another : DSet);
begin
  ProtectedDuplicate(another);
end;

procedure DSet.MorphIterator(var iterator : DIterator);
begin
  // Sets iterate over their keys.
  SetToKey(iterator);
end;

{
function DSet.clone : DContainer;
begin
  Result := DSet.CreateFrom(self);
end;
}

procedure DSet._add(const obj : DObject);
begin
  _putAt(obj, emptyDObject);
end;

procedure DSet._putAt(const key, value : DObject);
begin
  // verify that the value is empty, as it must be for a set.
  Assert((value.vtype = vtInteger) and (value.vinteger = 0));
  inherited _putAt(key, value);
end;

function DSet._includes(const obj : DObject) : Boolean;
var iter : DIterator;
begin
  iter := tree.find(obj);
  Result := not DeCAL.equals(iter, finish);
end;

function DSet.includes(const obj : array of const) : Boolean;
begin
  Result := _includes(obj[Low(obj)]);
end;

constructor DMultiSet.Create;
begin
  ProtectedCreate(true, DObjectComparator);
end;

constructor DMultiSet.CreateWith(compare : DComparator);
begin
  ProtectedCreate(true, compare);
end;

constructor DMultiSet.CreateFrom(another : DMultiSet);
begin
  ProtectedDuplicate(another);
end;

{
function DMultiSet.clone : DContainer;
begin
  Result := DMultiSet.CreateFrom(self);
end;
}

////////////////////////////////////////////////////////////////////
//
// Hash Maps
//
////////////////////////////////////////////////////////////////////
constructor DInternalHash.Create;
begin
  inherited Create;
  FBucketCount := DefaultBucketCount;

  // This is the default class being used by the hash stuff.
  FStorageClass := DArray;

  Setup;
end;

constructor DInternalHash.CreateWith(compare : DComparator);
begin
  inherited CreateWith(compare);
  FBucketCount := DefaultBucketCount;
  FStorageClass := DArray;
  Setup;
end;

constructor DInternalHash.CreateFrom(another : DInternalHash);
begin
  Create;
  another.cloneTo(self);
end;

function DInternalHash.BucketSequence(idx : Integer) : DSequence;
begin
  Result := FBuckets.atAsObject(idx) as DSequence;
end;

procedure DInternalHash.cloneTo(newContainer : DContainer);
var h : DInternalHash;
    i : Integer;
    seq1 : DSequence;
begin
  h := newContainer as DInternalHash;

  Assert(FAllowDups = h.FAllowDups, 'Need to be of same type');
  h.FStorageClass := FStorageClass;
  h.FBucketCount := FBucketCount;
  h.Setup;

  // walk through our buckets
  for i := 0 to FBucketCount - 1 do
    begin
      seq1 := BucketSequence(i);

      if seq1 <> nil then
        h.FBuckets.putAt(i, [seq1.clone])
      else
        h.FBuckets.putAt(i, [nil]);

    end;
end;


procedure DInternalHash.Setup;
begin
  FBuckets.Free;
  FBuckets := DArray.Create;
  FBuckets.SetSize(FBucketCount);
end;

destructor DInternalHash.Destroy;
begin
  Clear;
  FBuckets.Free;
  inherited;
end;

//
// Iterator manipulation.
//
procedure DInternalHash.iadvance(var iterator : DIterator);
var secondary : DIterator;
    seq : DSequence;
    by : Integer;
begin
  if not (diMarkFinish in iterator.flags) then
    begin

      if FIsSet then
        by := 1
      else
        by := 2;

      secondary := RecoverSecondaryIterator(iterator);
      advanceBy(secondary, by);

      if atEnd(secondary) then
        begin
          // Find the next bucket with something in it.
          repeat
            Inc(iterator.bucket);

            if iterator.bucket < FBucketCount then
              begin
                seq := Fbuckets.atAsObject(iterator.bucket) as DSequence;
                if (seq <> nil) and (seq.size > 0) then
                  begin
                    secondary := seq.start;

                    if (not FIsSet) and (not (diKey in iterator.Flags)) then
                      begin
                        advance(secondary);
                      end;

                    iterator.bucketPosition := secondary.bucketPosition;
                    Exclude(iterator.flags, diMarkStart);
                    exit;
                  end;
              end;
          until iterator.bucket >= FBucketCount;
          Include(iterator.flags, diMarkFinish);
        end
      else
        begin
          Exclude(iterator.flags, diMarkStart);
          iterator.bucketPosition := secondary.bucketPosition;
        end;
    end;
end;

function DInternalHash.iget(const iterator : DIterator) : PDObject;
var secondary : DIterator;
begin
  if not atEnd(iterator) then
    begin
      secondary := RecoverSecondaryIterator(iterator);
      Result := getRef(secondary);
    end
  else
    raise DException.Create('Can''t get from iterator past end');
end;

function DInternalHash.iequals(const iter1, iter2 : DIterator) : Boolean;
begin
  Assert(iter1.handler = iter2.handler);
  Result := ((diMarkFinish in iter1.flags) and (diMarkFinish in iter2.flags)) or
    ((iter1.bucketPosition = iter2.bucketPosition) and (iter1.bucket = iter2.bucket));
end;

procedure DInternalHash.iput(const iterator : DIterator; const obj : DObject);
begin
  Assert(FIsSet and atEnd(iterator), 'Can only put to iterators at end of hash sets.');
  _add(obj);
end;

function DInternalHash.iremove(const iterator : DIterator) : DIterator;
begin
  Assert(not atEnd(iterator));

  Result := iterator;
end;

// bidirectional
procedure DInternalHash.iretreat(var iterator : DIterator);
label search;
var secondary : DIterator;
    by : Integer;
    seq : DSequence;
begin
  if not atStart(iterator) then
    begin

      if diMarkFinish in iterator.flags then
        begin
          Assert(FCount > 0);
          iterator.bucket := FBucketCount;
          goto search;
        end;

      secondary := RecoverSecondaryIterator(iterator);

      if FIsSet then
        by := 1
      else
        by := 2;

      if not atStart(secondary) then
        begin
          retreatBy(secondary, by);
          Exclude(iterator.flags, diMarkFinish);
        end
      else
        begin
          Assert(iterator.bucket > 0);
search:
          repeat
            Dec(iterator.bucket);

            seq := FBuckets.atAsObject(iterator.bucket) as DSequence;

            if (seq <> nil) and (seq.size > 0) then
              begin
                secondary := seq.finish;

                if FIsSet or (diKey in iterator.flags) then
                  retreatBy(secondary,2)
                else
                  retreat(secondary);

                iterator.bucketPosition := secondary.bucketPosition;
                Exclude(iterator.flags, diMarkFinish);
              end;
          until iterator.bucket < 0;
        end;
    end;
end;

{
  Setting flags is a little more tricky for the hash stuff.  We need to move
  our secondary iterator position back and forth in response to these changes.
}
procedure DInternalHash.iflagChange(var iterator : DIterator; oldflags : DIteratorFlags);
begin
  if not atEnd(iterator) then
    begin
      if (diKey in iterator.flags) and (not (diKey in oldFlags)) then
        begin
          // Change from value to key
          Dec(iterator.bucketposition);
        end
      else if (not (diKey in iterator.flags)) and (diKey in oldFlags) then
        begin
          Inc(iterator.bucketposition);
        end;
    end;
end;

function DInternalHash.HashObj(const obj : DObject) : Integer;
var loc : PByte;
    len : Integer;
begin
  // Calculate the hash code for this new object.
  if Assigned(FHash) then
    begin
      HashLocation(obj, loc, len);
      Result := FHash(loc^, len);
    end
  else
    Result := JenkinsHashDObject(obj);
end;

procedure DInternalHash._add(const obj : DObject);
var h : Integer;
    iter : DIterator;
    bucket : DSequence;
begin

  if FIsSet then
    begin

      h := HashObj(obj) mod FBucketCount;

      // If we need to, identify where this object should go if it needs
      // to replace another.
      bucket := Fbuckets.atAsObject(h) as DSequence;

      if bucket <> nil then
        begin

          if not FAllowDups then
            iter := find(bucket, obj)
          else
            iter := bucket.finish;
        end
      else
        begin
          bucket := FStorageClass.Create;
          Fbuckets.putAt(h, [bucket]);
          iter := bucket.finish;
        end;

      // Add one to our count
      if atEnd(iter) then
        Inc(FCount);

      // Store the object
      _put(iter, obj);

    end
  else
    raise DException.Create('You must add pairs to a HashMap.');

end;

procedure DInternalHash._clear(direct : Boolean);
var iter : DIterator;
    con : DContainer;
begin
  iter := FBuckets.start;
  while not atEnd(iter) do
    begin
      con := getObject(iter) as DContainer;
      if con <> nil then
        begin
          con._clear(direct);
          con.free;
          putObject(iter, nil);
        end;
      advance(iter);
    end;
  FCount := 0;
end;

function DInternalHash.finish : DIterator;
begin
  with result do
    begin
      flags := [diMarkFinish];
      Handler := self;
    end;
end;

function DInternalHash.maxSize : Integer;
begin
  Result := MaxInt div SizeOf(DObject);
end;

function DInternalHash.start : DIterator;
var synthIter : DIterator;
    seq : DSequence;
begin
  with result do
    begin
      if size = 0 then
        flags := [diMarkFinish]
      else
        flags := [diMarkStart];
      bucket := 0;
      bucketPosition := 0;
      Handler := self;

      if size > 0 then
        begin
          while true do
            begin
              if bucket >= FBuckets.size then
                break;
              seq := DSequence(FBuckets.atAsObject(bucket));
              if seq <> nil then
                begin
                  if seq.size > 0 then
                    break;
                end;
              Inc(bucket);
            end;
          Assert(bucket < Fbuckets.size);

          // Now that we've located a bucket with something in it, get our
          // "sub" iterator.
          seq := FBuckets.atAsObject(bucket) as DSequence;
          synthIter := seq.start;
          if (not FIsSet) then
            advance(synthIter);

          // this should container whatever field we need to make this
          // synthetic iterator work.
          bucketPosition := synthIter.bucketPosition;

        end;

      if FIsSet then
        Include(Result.flags, diKey);

    end;
end;

//
// Here we're going to get our secondary iterator back from the bucket count.
//
function DInternalHash.RecoverSecondaryIterator(const baseIter : DIterator) : DIterator;
var seq : DSequence;
begin
  if not atEnd(baseIter) then
    begin
      seq := FBuckets.atAsObject(baseIter.bucket) as DSequence;
      Result := seq.start;
      if Result.bucketPosition <> baseIter.bucketPosition then
        Exclude(Result.flags, diMarkStart);
      Result.bucketPosition := baseIter.bucketPosition;
    end
  else
    Result := finish;
end;

function DInternalHash.size : Integer;
begin
  Result := FCount;
end;

function DInternalHash.allowsDuplicates : Boolean;
begin
  Result := FAllowDups;
end;

function DInternalHash._count(const key : DObject) : Integer;
var iter : DIterator;
begin
  iter := startKey;
  Result := DeCAL.countIn(iter, finish, key);
end;

function DInternalHash._countValues(const value : DObject) : Integer;
begin
  Result := DeCAL.count(self, value);
end;

function DInternalHash._getAt(const key : DObject) : DObject;
var h : Integer;
    bucket : DSequence;
    iter : DIterator;
begin

  Assert(FIsSet, 'getAt only works with maps.');

  // identify the bucket
  h := HashObj(key) mod FBucketCount;

  // search that bucket for a matching key
  bucket := FBuckets.atAsObject(h) as DSequence;
  iter := bucket.start;
  while not atEnd(iter) do
    begin
      if comparator(getRef(iter)^, key) = 0 then
        begin
          // gotcha
          advance(iter);
          Result := get(iter);
          exit;
        end;
      advanceBy(iter,2);
    end;
  raise DException.Create('Object not found');
end;

function DInternalHash._locate(const key : DObject) : DIterator;
var h : Integer;
    bucket : DSequence;
    iter : DIterator;
    by : Integer;
begin

  // identify the bucket
  h := HashObj(key) mod FBucketCount;

  if FIsSet then
    by := 1
  else
    by := 2;

  // search that bucket for a matching key
  bucket := FBuckets.atAsObject(h) as DSequence;

  // was there a bucket there?
  if bucket <> nil then
    begin
      iter := bucket.start;

      while not atEnd(iter) do
        begin
          if comparator(getRef(iter)^, key) = 0 then
            begin
              Result := start;
              Result.bucket := h;
              Result.bucketPosition := iter.position;

              if (not FIsSet) then
                Inc(Result.bucketPosition);

              exit;
            end;
          advanceBy(iter,by);
        end;
      Result := finish;
    end
  else
    Result := finish;
end;

procedure DInternalHash._putAt(const key, value : DObject);
var h : Integer;
    bucket : DSequence;
    iter : DIterator;
begin
  if not FIsSet then
    begin
      h := HashObj(key) mod FBucketCount;

      // If we need to, identify where this object should go if it needs
      // to replace another.
      bucket := Fbuckets.atAsObject(h) as DSequence;
      if bucket = nil then
        begin
          bucket := FStorageClass.Create;
          FBuckets.putAt(h, [bucket]);
          iter := bucket.finish;
        end
      else
        begin

          if not FAllowDups then
            begin

              // note that we can't use the canned find routine because we
              // need to skip every second object.
              iter := bucket.start;

              while not atEnd(iter) do
                begin
                  if Comparator(getRef(iter)^, key) = 0 then
                    break
                  else
                    advanceBy(iter, 2);
                end;

            end
          else
            iter := bucket.finish;

        end;

      // Add one to our count
      if atEnd(iter) then
        Inc(FCount);

      // Store the object
      _output(iter, key);
      _output(iter, value);

    end
  else
    raise DException.Create('You cannot add pairs to a set.');
end;

procedure DInternalHash._remove(const key : DObject);
begin
  _removeN(key, MaxInt);
end;

procedure DInternalHash._removeN(const key : DObject; count : Integer);
var iter, secondary : DIterator;
    bucket : DSequence;
    by : Integer;
    found : Boolean;
begin
  iter := _locate(key);
  if not AtEnd(iter) then
    begin
      secondary := RecoverSecondaryIterator(iter);
      bucket := secondary.handler as DSequence;

      if FIsSet then
        by := 1
      else
        by := 2;

      found := true;

      // remove entries with matching keys (which are in every second element)
      // up to count times.
      while found and (count > 0) do
        begin
          found := false;
          iter := bucket.start;
          while not atEnd(iter) do
            begin
              if comparator(getRef(iter)^, key) = 0 then
                begin
                  Dec(count);
                  Dec(FCount);
                  iter := bucket.removeAtIter(iter, by);
                  found := true;
                  break;
                end
              else
                advanceBy(iter, by);
            end;
        end;
    end;
end;

procedure DInternalHash._removeValueN(const value : DObject; count : Integer);
var iter, secondary : DIterator;
    bucket : DSequence;
    found : Boolean;
begin
  Assert(not FIsSet);
  iter := _locate(value);
  if not AtEnd(iter) then
    begin
      secondary := RecoverSecondaryIterator(iter);
      bucket := secondary.handler as DSequence;

      found := true;

      // remove entries with matching keys (which are in every second element)
      // up to count times.
      while found and (count > 0) do
        begin
          found := false;
          iter := bucket.start;
          advance(iter);
          while not atEnd(iter) do
            begin
              if comparator(getRef(iter)^, value) = 0 then
                begin
                  Dec(count);
                  Dec(FCount);
                  bucket.removeAtIter(iter, 1);
                  found := true;
                  break;
                end;
              advanceBy(iter, 2);
            end;
        end;
    end;
end;

procedure DInternalHash.removeAt(iterator : DIterator);
var secondary : DIterator;
    seq : DSequence;
    by : Integer;
begin
  Assert(not atEnd(iterator));
  seq := FBuckets.AtAsObject(iterator.bucket) as DSequence;
  Assert(seq <> nil);
  secondary := RecoverSecondaryIterator(iterator);
  if FIsSet then
    by := 1
  else
    by := 2;
  seq.removeAtIter(secondary, by);
  Dec(FCount);
end;

//
// Is this procedure necessary?  What does it mean, logically, to do this.
// There is no guarantee that the objects will be related in any way but
// hash value, which isn't supposed to mean anything at all.
procedure DInternalHash.removeIn(_start, _finish : DIterator);
begin
  raise DException.Create('Operation not available on Hashes.');
end;

function DInternalHash.startKey : DIterator;
begin
  Result := start;
  setToKey(result);
end;

//
// Hash specific
//
procedure DInternalHash.SetBucketCount(bCount : Integer);
begin
  Clear;
  FBuckets.Clear;
  FBuckets.setSize(bCount);
  FBucketCount := bCount;
end;

procedure DInternalHash.SetBucketClass(cls : DSequenceClass);
begin
  Assert(FCount = 0);
  FStorageClass := cls;
end;

function DHashMap.usesPairs : Boolean;
begin
  Result := true;
end;

function DMultiHashMap.usesPairs : Boolean;
begin
  Result := true;
end;

constructor DMultiHashMap.Create;
begin
  FAllowDups := True;
  inherited Create;
end;

constructor DMultiHashMap.CreateWith(compare : DComparator);
begin
  FAllowDups := True;
  inherited CreateWith(compare);
end;

constructor DMultiHashMap.CreateFrom(another : DMultiHashMap);
begin
  FAllowDups := True;
  inherited CreateFrom(another);
end;

constructor DHashSet.Create;
begin
  FIsSet := True;
  inherited Create;
end;

constructor DHashSet.CreateWith(compare : DComparator);
begin
  FIsSet := True;
  inherited CreateWith(compare);
end;

constructor DHashSet.CreateFrom(another : DHashSet);
begin
  FIsSet := True;
  inherited CreateFrom(another);
end;

function DHashSet._includes(const obj : DObject) : Boolean;
begin
  Result := not atEnd(_locate(obj));
end;

function DHashSet.includes(const obj : array of const) : Boolean;
var i : Integer;
begin
  Result := true;
  for i := Low(obj) to High(obj) do
    begin
      Result := result and _includes(obj[i]);
      if not result then
        break;
    end;
end;

constructor DMultiHashSet.Create;
begin
  FAllowDups := True;
  inherited Create;
end;

constructor DMultiHashSet.CreateWith(compare : DComparator);
begin
  FAllowDups := True;
  inherited CreateWith(compare);
end;

constructor DMultiHashSet.CreateFrom(another : DMultiHashSet);
begin
  FAllowDups := True;
  inherited CreateFrom(another);
end;

////////////////////////////////////////////////////////////////////
//
// VCL Data Structure Adapters
//
////////////////////////////////////////////////////////////////////

//
// Iterator manipulation.
//
constructor DTStrings.Create(strings : TStrings);
begin
  FStrings := strings;
  InitDObject(FDummy);
  inherited Create;
end;

destructor DTStrings.Destroy;
begin
  ClearDObject(FDummy);
end;

procedure DTStrings.iadvance(var iterator : DIterator);
begin
  iadvanceBy(iterator, 1);
end;

procedure DTStrings.iadvanceBy(var iterator : DIterator; count : Integer);
begin
  if count < 0 then
    iretreatBy(iterator, -count)
  else if count > 0 then
    begin
      Assert(iterator.position + count <= FStrings.Count);
      Inc(iterator.position, count);
      Exclude(iterator.flags, diMarkStart);
      if iterator.position > FStrings.Count then
        iterator.position := FStrings.Count;
      if iterator.position >= FStrings.Count then
        Include(iterator.flags, diMarkFinish);
    end;
end;

function DTStrings.iget(const iterator : DIterator) : PDObject;
begin
  ClearDObject(FDummy);
  {$IFDEF UNICODE}
  FDummy.VType := vtUnicodeString;
  UnicodeString(FDummy.VUnicodeString) := FStrings[iterator.position];
  {$ELSE}
  FDummy.VType := vtAnsiString;
  AnsiString(FDummy.VAnsiString) := FStrings[iterator.position];
  {$ENDIF UNICODE}
  Result := @FDummy;
end;

function DTStrings.iequals(const iter1, iter2 : DIterator) : Boolean;
begin
  Result := iter1.position = iter2.position;
end;

procedure DTStrings.iput(const iterator : DIterator; const obj : DObject);
begin
  Assert(obj.vtype in [vtAnsiString, vtWideString, vtUnicodeString], 'Can only put strings into this container adapter.');
  if iterator.position = FStrings.Count then
    FStrings.Append(asString(obj))
  else
    FStrings[iterator.position] := asString(obj);
end;

function DTStrings.idistance(const iter1, iter2 : DIterator) : Integer;
begin
  Result := iter2.position - iter1.position;
end;


// bidirectional
procedure DTStrings.iretreat(var iterator : DIterator);
begin
  iretreatBy(iterator, 1);
end;

procedure DTStrings.iretreatBy(var iterator : DIterator; count : Integer);
begin
  if count < 0 then
    iadvanceBy(iterator, -count)
  else if count > 0 then
    begin
      Assert(iterator.position - count >= 0);
      Exclude(iterator.flags, diMarkFinish);
      Dec(iterator.position, count);
      if iterator.position < 0 then
        iterator.position := 0;
      if (iterator.position = 0) and (FStrings.Count > 0) then
        Include(iterator.flags, diMarkStart);
      if FStrings.Count = 0 then
        Include(iterator.flags, diMarkFinish);
    end;
end;

function DTStrings.igetAt(const iterator : DIterator; offset : Integer) : PDObject;
begin
  Assert((iterator.position + offset >= 0) and (iterator.position + offset < FStrings.Count));
  ClearDObject(FDummy);
  {$IFDEF UNICODE}
  FDummy.VType := vtUnicodeString;
  UnicodeString(FDummy.VUnicodeString) := FStrings[iterator.position + offset];
  {$ELSE}
  FDummy.VType := vtAnsiString;
  AnsiString(FDummy.VAnsiString) := FStrings[iterator.position + offset];
  {$ENDIF UNICODE}
  Result := @FDummy;
end;

procedure DTStrings.iputAt(const iterator : DIterator; offset : Integer; const obj : DObject);
begin
  Assert((iterator.position + offset >= 0) and (iterator.position + offset <= FStrings.Count));
  if iterator.position + offset = FStrings.Count then
    FStrings.Append(asString(obj))
  else
    FStrings[iterator.position + offset] := asString(obj);
end;


// random
function DTStrings.iindex(const iterator : DIterator) : Integer;
begin
  Result := iterator.position;
end;

function DTStrings.iless(const iter1, iter2 : DIterator) : Boolean;
begin
  Result := iter1.position < iter2.position;
end;


function DTStrings._at(pos : Integer) : PDObject;
begin
  ClearDObject(FDummy);
  {$IFDEF UNICODE}
  FDummy.VType := vtUnicodeString;
  UnicodeString(FDummy.VUnicodeString) := FStrings[pos];
  {$ELSE}
  FDummy.VType := vtAnsiString;
  AnsiString(FDummy.VAnsiString) := FStrings[pos];
  {$ENDIF UNICODE}
  Result := @FDummy;
end;

procedure DTStrings._clear(direct : Boolean);
begin
  FStrings.Clear;
end;


procedure DTStrings._add(const obj : DObject);
begin
  FStrings.Add(asString(obj));
end;

function DTStrings.clone : DContainer;
begin
  raise DException.Create('Can''t clone adapters.');
end;

function DTStrings.finish : DIterator;
begin
  with result do
    begin
      flags := [diMarkFinish, diRandom];
      position := FStrings.Count;
      handler := self;
    end;
end;

function DTStrings.maxSize : Integer;
begin
  Result := MaxInt;
end;

function DTStrings.size : Integer;
begin
  Result := FStrings.Count;
end;

function DTStrings.start : DIterator;
begin
  if FStrings.Count = 0 then
    Result := finish
  else
    with result do
      begin
        flags := [diMarkStart];
        handler := self;
        position := 0;
      end;
end;


//
// DSequence overrides;
//
function DTStrings.at(pos : Integer) : DObject;
begin
  InitDObject(result);
  CopyDObject(_at(pos)^, result);
end;

function DTStrings.backRef : PDObject;
begin
  Assert(FStrings.Count > 0);
  Result := _at(FStrings.count - 1);
end;

function DTStrings.frontRef : PDObject;
begin
  Assert(FStrings.Count > 0);
  Result := _at(0);
end;

function DTStrings.popBack : DObject;
begin
  Assert(FStrings.Count > 0);
  {$IFDEF UNICODE}
  Result.VType := vtUnicodeString;
  UnicodeString(Result.VUnicodeString) := FStrings[FStrings.Count - 1];
  {$ELSE}
  Result.VType := vtAnsiString;
  AnsiString(Result.VAnsiString) := FStrings[FStrings.Count - 1];
  {$ENDIF UNICODE}
  FStrings.Delete(FStrings.Count - 1);
end;

function DTStrings.popFront : DObject;
begin
  Assert(FStrings.Count > 0);
  {$IFDEF UNICODE}
  Result.VType := vtUnicodeString;
  UnicodeString(Result.VUnicodeString) := FStrings[0];
  {$ELSE}
  Result.VType := vtAnsiString;
  AnsiString(Result.VAnsiString) := FStrings[0];
  {$ENDIF UNICODE}
  FStrings.Delete(0);
end;

procedure DTStrings._pushBack(const obj : DObject);
begin
  FStrings.Add(asString(obj));
end;

procedure DTStrings._pushFront(const obj : DObject);
begin
  FStrings.Insert(0, asString(obj));
end;

function DTStrings.removeAtIter(iter : DIterator; count : Integer) : DIterator;
begin
  while (count > 0) and (iter.position + count <= FStrings.count) do
    begin
      FStrings.Delete(iter.position);
      Dec(count);
    end;
  Result := start;
  advanceBy(result, iter.position);
end;

procedure DTStrings._putAt(index : Integer; const obj : DObject);
begin
  FStrings[index] := asString(obj);
end;


//
// DVector overrides
//
function DTStrings.capacity : Integer;
begin
  Result := FStrings.Capacity;
end;

procedure DTStrings.ensureCapacity(amount : Integer);
begin
  FStrings.Capacity := amount;
end;

procedure DTStrings._insertAtIter(iterator : DIterator; const obj : DObject);
begin
  FStrings.Insert(iterator.position, asString(obj));
end;

procedure DTStrings._insertAt(index : Integer; const obj : DObject);
begin
  FStrings.Insert(index, asString(obj));
end;

procedure DTStrings._insertMultipleAtIter(iterator : DIterator; count : Integer; const obj : DObject);
begin
  while count > 0 do
    begin
      _insertAtIter(iterator, obj);
      Dec(Count);
    end;
end;

procedure DTStrings._insertMultipleAt(index : Integer; count : Integer; const obj : DObject);
begin
  while count > 0 do
    begin
      _insertAt(index, obj);
      Dec(Count);
    end;
end;

procedure DTStrings.insertRangeAtIter(iterator : DIterator; _start, _finish : DIterator);
var index : Integer;
begin
  index := iterator.position;
  while not DeCAL.equals(start, finish) do
    begin
      FStrings.Insert(index, getString(_start));
      Inc(index);
      advance(_start);
    end;
end;

procedure DTStrings.insertRangeAt(index : Integer; _start, _finish : DIterator);
begin
  while not DeCAL.equals(start, finish) do
    begin
      FStrings.Insert(index, getString(_start));
      Inc(index);
      advance(_start);
    end;
end;

procedure DTStrings._remove(const obj : DObject);
var i : Integer;
begin
  i := FStrings.Count - 1;
  while i >= 0 do
    begin
      if FStrings[i] = asString(obj) then
        FStrings.Delete(i);
      Dec(i);
    end;
end;

procedure DTStrings.removeAt(index : Integer);
begin
  FStrings.Delete(index);
end;

procedure DTStrings.removeBetween(_begin, _end : Integer);
begin
  repeat
    Dec(_end);
    if _end >= _begin then
      FStrings.Delete(_end);
  until _end < _begin;
end;

procedure DTStrings._removeWithin(_begin, _end : Integer; const obj : DObject);
begin
  repeat
    Dec(_end);
    if (_end >= _begin) and (FStrings[_end] = asString(obj)) then
      FStrings.Delete(_end);
  until _end < _begin;
end;

procedure DTStrings.setCapacity(amount : Integer);
begin
  FStrings.Capacity := amount;
end;

procedure DTStrings.setSize(newSize : Integer);
begin
  if newSize = 0 then
    FStrings.Clear
  else if newSize > FStrings.Count then
    begin
      while FStrings.Count < newSize do
        FStrings.Append('');
    end
  else if newSize < FStrings.Count then
    begin
      while FStrings.Count > newSize do
        FStrings.Delete(newSize);
    end;
end;

procedure DTStrings.trimToSize;
begin
  // can't currently do anything here.
end;

constructor DTList.Create(list : TList);
begin
end;


////////////////////////////////////////////////////////////////////
//
// Applying
//
////////////////////////////////////////////////////////////////////

procedure forEach(container : DContainer; unary : DApply);
begin
  forEachIn(container.start, container.finish, unary);
end;

procedure forEachIn(_start, _end : DIterator; unary : DApply);
begin
  while not equals(_start, _end) do
    begin
      unary(getRef(_start)^);
      advance(_start);
    end;
end;

procedure forEachIf(container : DContainer; unary : DApply; test : DTest);
begin
  forEachInIf(container.start, container.finish, unary, test);
end;

procedure forEachInIf(_start, _end : DIterator; unary : DApply; test : DTest);
var obj : PDObject;
begin
  while not equals(_start, _end) do
    begin
      obj := getRef(_start);
      if test(obj^) then
        unary(obj^);
      advance(_start);
    end;
end;

function _inject(container : DContainer; const obj : DObject; binary : DBInary) : DObject;
var s,f : DIterator;
begin
  s := container.start;
  f := container.finish;
  Result := injectIn(s,f,obj,binary);
end;

function _injectIn(_start, _end : DIterator; const obj : DObject; binary : DBinary) : DObject;
var q : DObject;
begin
  CopyDObject(obj, result);
  while not equals(_start, _end) do
    begin
      q := binary(result, getRef(_start)^);
      ClearDObject(result);
      MoveDObject(q, result);
      advance(_start);
    end;
end;

function inject(container : DContainer; const obj : array of const; binary : DBinary) : DObject;
begin
  Assert(Low(obj) = High(obj));
  Result := _inject(container, obj[Low(obj)], binary);
end;

function injectIn(_start, _end : DIterator; const obj : array of const; binary : DBinary) : DObject;
begin
  Assert(Low(obj) = High(obj));
  Result := _injectIn(_start, _end, obj[Low(obj)], binary);
end;

////////////////////////////////////////////////////////////////////
//
// Comparing
//
////////////////////////////////////////////////////////////////////
function equal(con1, con2 : DContainer) : Boolean;
begin
  Result := (con1.size = con2.size) and equalIn(con1.start, con1.finish, con2.start);
end;

function equalIn(start1, end1, start2 : DIterator) : Boolean;
var bt : DBinaryTest;
begin
  Result := true;
  getContainer(start1).getBinaryTest(bt);
  while not equals(start1, end1) do
    begin
      if not bt(getRef(start1)^, getRef(start2)^) then
        begin
          Result := false;
          break;
        end;
      advance(start1);
      advance(start2);
    end;
end;

function lexicographicalCompare(con1, con2 : DContainer) : Integer;
var compare : DComparator;
begin
  con1.getComparator(compare);
  Result := lexicographicalCompareInWith(con1.start, con1.finish, con2.start, con2.finish, compare);
end;

function lexicographicalCompareWith(con1, con2 : DContainer; compare : DComparator) : Integer;
begin
  Result := lexicographicalCompareInWith(con1.start, con1.finish, con2.start, con2.finish, compare);
end;

function lexicographicalCompareIn(start1, end1, start2, end2 : DIterator) : Integer;
var compare : DComparator;
begin
  getContainer(start1).getComparator(compare);
  Result := lexicographicalCompareInWith(start1, end1, start2, end2, compare);
end;

function lexicographicalCompareInWith(start1, end1, start2, end2 : DIterator; compare : DCOmparator) : Integer;
var c : Integer;
    resolved : Boolean;
    e1, e2 : Boolean;
begin

  resolved := false;
  c := 0;

  while (not equals(start1, end1)) and (not equals(start2,end2)) do
    begin
      c := compare(getRef(start1)^, getRef(start2)^);
      if c < 0 then
        begin
          resolved := true;
          break;
        end
      else if c > 0 then
        begin
          resolved := true;
          break;
        end;
      advance(start1);
      advance(start2);
    end;

  if resolved then
    Result := c
  else
    begin
      e1 := equals(start1, end1);
      e2 := equals(start2, end2);
      if e1 and e2 then
        Result := 0
      else if e1 then
        Result := -1
      else
        Result := 1;
    end;

end;

function _median(const obj1, obj2, obj3 : DObject; compare : DComparator) : DObject;
begin
  if compare(obj1, obj2) > 0 then
    begin
      if compare(obj2, obj3) > 0 then
        CopyDObject(obj2, result)
      else if compare(obj1, obj3) > 0 then
        CopyDObject(obj3, result)
      else
        CopyDObject(obj1, result);
    end
  else if compare(obj1,obj3) > 0 then
    CopyDObject(obj1, result)
  else if compare(obj2, obj3) > 0 then
    CopyDObject(obj3, result)
  else
    CopyDObject(obj2, result);

end;

function median(const objs : array of const; compare : DComparator) : DObject;
begin
  Assert(High(objs) - Low(objs) = 2, 'Must pass exactly 3 elements');
  Result := _median(objs[Low(objs)], objs[Low(objs) + 1], objs[Low(objs) + 2], compare);
end;

function mismatch(con1, con2 : DContainer) : DIteratorPair;
var bt : DBinaryTest;
begin
  con1.getBinaryTest(bt);
  Result := mismatchInWith(con1.start, con1.finish, con2.start,bt);
end;

function mismatchWith(con1, con2 : DContainer; bt : DBinaryTest) : DIteratorPair;
begin
  Result := mismatchInWith(con1.start, con1.finish, con2.start, bt);
end;

function mismatchIn(start1, end1, start2 : DIterator) : DIteratorPair;
var bt : DBinaryTest;
begin
  getContainer(start1).getBinaryTest(bt);
  Result := mismatchInWith(start1, end1, start2, bt);
end;

function mismatchInWith(start1, end1, start2 : DIterator; bt : DBinaryTest) : DIteratorPair;
begin
  while not equals(start1, end1) do
    begin
      if not bt(getRef(start1)^, getRef(start2)^) then
        break;
      advance(start1);
      advance(start2);
    end;
  Result.first := start1;
  Result.second := start2;
end;

////////////////////////////////////////////////////////////////////
//
// Copying
//
////////////////////////////////////////////////////////////////////
function copyContainer(source, dest : DContainer) : DIterator;
begin
  Result := copyInTo(source.start, source.finish, dest.finish);
end;

function copyTo(source : DContainer; dest : DIterator) : DIterator;
begin
  Result := copyInTo(source.start, source.finish, dest);
end;

function copyInTo(_start, _end, dest : DIterator) : DIterator;
begin
  while not equals(_start, _end) do
    begin
      _output(dest, getRef(_start)^);
      advance(_start);
    end;
  Result := dest;
end;

function copyBackward(_start, _end, dest : DIterator) : DIterator;
begin
  while not equals(_start, _end) do
    begin
      retreat(_end);
      _output(dest, getRef(_end)^);
    end;
  Result := dest;
end;

////////////////////////////////////////////////////////////////////
//
// Counting
//
////////////////////////////////////////////////////////////////////
function _count(con1 : DContainer; const obj : DObject) : Integer;
begin
  Result := countIn(con1.start, con1.finish, obj);
end;

function _countIn(_start, _end : DIterator; const obj : DObject) : Integer;
var comparator : DComparator;
begin
  Result := 0;
  getContainer(_start).getComparator(comparator);
  while not equals(_start, _end) do
    begin
      if comparator(getRef(_start)^, obj) = 0 then
        Inc(result);
      advance(_start);
    end;
end;

function count(con1 : DContainer; const objs : array of const) : Integer;
var i : Integer;
begin
  Result := 0;
  for i := Low(objs) to High(objs) do
    Result := result + _count(con1, objs[i]);
end;

function countIn(_start, _end : DIterator; const objs : array of const) : Integer;
var i : Integer;
begin
  Result := 0;
  for i := Low(objs) to High(objs) do
    Result := result + _countIn(_start, _end, objs[i]);
end;

function countIf(con1 : DContainer; test : DTest) : Integer;
begin
  Result := countIfIn(con1.start, con1.finish, test);
end;

function countIfIn(_start, _end : DIterator; test : DTest) : Integer;
begin
  Result := 0;
  while not equals(_start, _end) do
    begin
      if test(getRef(_start)^) then
        Inc(result);
      advance(_start);
    end
end;

////////////////////////////////////////////////////////////////////
//
// Filling
//
////////////////////////////////////////////////////////////////////

procedure _fill(con : DContainer; const obj : DObject);
begin
  _fillIn(con.start, con.finish, obj);
end;

procedure _fillN(con : DContainer; count : Integer; const obj : DObject);
var iter : DIterator;
begin
  iter := con.start;
  while count > 0 do
    begin
      _output(iter, obj);
      Dec(count);
    end;
end;

procedure _fillIn(_start, _end : DIterator; const obj : DObject);
begin
  while not equals(_start, _end) do
    begin
      _put(_start, obj);
      advance(_start);
    end;
end;

procedure fill(con : DContainer; const obj : array of const);
begin
  _fill(con, obj[Low(obj)]);
end;

procedure fillN(con : DContainer; count : Integer; const obj : array of const);
begin
  _fillN(con, count, obj[Low(obj)]);
end;

procedure fillIn(_start, _end : DIterator; const obj : array of const);
begin
  _fillIn(_start, _end, obj[Low(obj)]);
end;

procedure generate(con : DContainer; count : Integer; gen : DGenerator);
var obj : DObject;
begin
  while count > 0 do
    begin
      obj := gen;
      con.add(obj);
      ClearDObject(obj);
      Dec(count);
    end;
end;

procedure generateIn(_start, _end : DIterator; gen : DGenerator);
begin
  while not equals(_start, _end) do
    _outputRef(_start, gen);
end;

procedure generateTo(dest : DIterator; count : Integer; gen : DGenerator);
begin
  while count > 0 do
    begin
      _outputRef(dest, gen);
      Dec(count);
    end;
end;

////////////////////////////////////////////////////////////////////
//
// Filtering
//
////////////////////////////////////////////////////////////////////
function unique(con : DContainer) : DIterator;
begin
  Result := uniqueInWith(con.start, con.finish, con.binaryTest);
end;

function uniqueIn(_start, _end : DIterator) : DIterator;
begin
  Result := uniqueInWith(_start, _end, getContainer(_start).binaryTest);
end;

function uniqueWith(con : DContainer; compare : DBinaryTest) : DIterator;
begin
  Result := uniqueInWith(con.start, con.finish, compare);
end;

function uniqueInWith(_start, _end : DIterator; compare : DBinaryTest) : DIterator;
var iter : DIterator;
begin
  iter := _start;
  while not equals(_start, _end) do
    begin

      // copy this one
      putRef(iter, getRef(_start)^);
      advance(_start);

      // skip duplicates.
      while (not equals(_start, _end)) and compare(getRef(iter)^, getRef(_start)^) do
        advance(_start);

      // advance dest
      advance(iter);

    end;
  Result := iter;
end;

function uniqueTo(con : DContainer; dest : DIterator) : DIterator;
begin
  Result := uniqueInWithTo(con.start, con.finish, dest, con.binaryTest);
end;

function uniqueInTo(_start, _end, dest : DIterator) : DIterator;
begin
  Result := uniqueInWithTo(_start, _end, dest, getContainer(_start).binaryTest);
end;

function uniqueInWithTo(_start, _end, dest : DIterator; compare : DBinaryTest) : DIterator;
var iter : DIterator;
begin
  iter := _start;
  while not equals(_start, _end) do
    begin

      // copy this one
      output(dest, getRef(_start)^);
      advance(_start);

      // skip duplicates.
      while (not equals(_start, _end)) and compare(getRef(iter)^, getRef(_start)^) do
        advance(_start);

      // advance dest
      advance(iter);

    end;
  Result := iter;
end;

procedure Filter(fromCon, toCon : DContainer; test : DTest);
begin
  FilterInTo(fromCon.start, fromCon.finish, toCon.finish, test);
end;

function FilterTo(con : DContainer; dest : DIterator; test : DTest) : DIterator;
begin
  Result := FilterInTo(con.start, con.finish, dest, test);
end;

function FilterInTo(_start, _end, dest : DIterator; test : DTest) : DIterator;
var pd : PDObject;
begin
  while not equals(_start, _end) do
    begin
      pd := getRef(_start);
      if test(pd^) then
        output(dest, pd^);
      advance(_start);
    end;
  Result := dest;
end;

////////////////////////////////////////////////////////////////////
//
// Finding
//
////////////////////////////////////////////////////////////////////

function adjacentFind(container : DContainer) : DIterator;
var bt : DBinaryTest;
begin
  container.getBinaryTest(bt);
  Result := adjacentFindInWith(container.start, container.finish, bt);
end;

function adjacentFindWith(container : DContainer; compare : DBinaryTest) : DIterator;
begin
  Result := adjacentFindInWith(container.start, container.finish, compare);
end;

function adjacentFindIn(_start, _end : DIterator) : DIterator;
var	bt : DBinaryTest;
begin
  getContainer(_start).getBinaryTest(bt);
  Result := adjacentFindInWith(_start, _end, bt);
end;

function adjacentFindInWith(_start, _end : DIterator; compare : DBinaryTest) : DIterator;
var this, next : DIterator;
begin
  if equals(_start, _end) then
    Result := _end
  else
    begin

      Result := _end;

      this := _start;
      next := _start;
      advance(next);

      while not equals(next, _end) do
        begin
          if compare(getRef(this)^, getRef(next)^) then
            begin
              Result := this;
              break;
            end;
          advance(this);
          advance(next);
        end;

    end;
end;

function _binarySearch(con : DContainer; const obj : DObject) : DIterator;
var bt : DComparator;
begin
  con.GetComparator(bt);
  Result := binarySearchInWith(con.start, con.finish, bt, obj);
end;

function _binarySearchIn(_start, _end : DIterator; const obj : DObject) : DIterator;
var bt : DComparator;
begin
  GetContainer(_start).GetComparator(bt);
  Result := binarySearchInWith(_start, _end, bt, obj);
end;

function _binarySearchWith(con : DContainer; compare : DComparator; const obj : DObject) : DIterator;
begin
  Result := binarySearchInWith(con.start, con.finish, compare, obj);
end;

function _binarySearchInWith(_start, _end : DIterator; compare : DComparator; const obj : DObject) : DIterator;
var dist, comp : Integer;
    last : DIterator;
begin

  Assert(diRandom in _start.flags, 'Binary search only on random access iterators');
  last := _end;
  Result := last;

  repeat
    dist := distance(_start, _end) div 2;
    if dist <= 0 then
      begin
        if atEnd(_start) then
          result := last
        else if compare(getRef(_start)^, obj) = 0 then
          Result := _start
        else if atEnd(advanceF(_start)) then
          result := last
        else if compare(getRef(advanceF(_start))^, obj) = 0 then
          Result := advanceF(_start)
        else
          Result := last;
        break;
      end;
    Result := advanceByF(_start, dist);
    comp := compare(getRef(result)^, obj);
    if comp = 0 then
      break
    else if comp < 0 then
      _start := result
    else
      _end := result
  until false;
end;

function binarySearch(con : DContainer; const obj : array of const) : DIterator;
begin
  Assert(Low(obj) = High(obj), 'Can only pass one object.');
  Result := _binarySearch(con, obj[Low(obj)]);
end;

function binarySearchIn(_start, _end : DIterator; const obj : array of const) : DIterator;
begin
  Assert(Low(obj) = High(obj), 'Can only pass one object.');
  Result := _binarySearchIn(_start, _end, obj[Low(obj)]);
end;

function binarySearchWith(con : DContainer; compare : DComparator; const obj : array of const) : DIterator;
begin
  Assert(Low(obj) = High(obj), 'Can only pass one object.');
  Result := _binarySearchWith(con, compare, obj[Low(obj)]);
end;

function binarySearchInWith(_start, _end : DIterator; compare : DComparator; const obj : array of const) : DIterator;
begin
  Assert(Low(obj) = High(obj), 'Can only pass one object.');
  Result := _binarySearchInWith(_start, _end, compare, obj[Low(obj)]);
end;


function detectWith(container : DContainer; compare : DTest) : DIterator;
begin
  Result := detectInWith(container.start, container.finish,compare);
end;

function detectInWith(_start, _end : DIterator; compare : DTest) : DIterator;
begin
  while not equals(_start, _end) do
    begin
      if compare(getRef(_start)^) then
        break;
      advance(_start);
    end;
  Result := _start;
end;

function every(container : DContainer; test : DTest) : Boolean;
begin
  Result := everyIn(container.start, container.finish, test);
end;

function everyIn(_start, _end : DIterator; test : DTest) : Boolean;
var all : Boolean;
begin
  all := True;
  while not equals(_start, _end) do
    begin
      all := all and test(getRef(_start)^);
      if not all then
        break;
      advance(_start);
    end;
  Result := all;
end;

function _find(container : DContainer; const obj : DObject) : DIterator;
begin
  Result := findIn(container.start, container.finish,obj);
end;

function _findIn(_start, _end : DIterator; const obj : DObject) : DIterator;
var compare : DComparator;
begin
  getContainer(_start).getComparator(compare);
  while not equals(_start, _end) do
    begin
      if compare(getRef(_start)^,obj) = 0 then
        break;
      advance(_start);
    end;
  Result := _start;
end;

function find(container : DContainer; const objs : array of const) : DIterator;
begin
  Assert(High(objs) = Low(objs));
  Result := _find(container, objs[Low(objs)]);
end;

function findIn(_start, _end : DIterator; const objs : array of const) : DIterator;
begin
  Assert(High(objs) = Low(objs));
  Result := _findIn(_start, _end, objs[Low(objs)]);
end;

function findIf(container : DContainer; test : DTest) : DIterator;
begin
  Result := findIfIn(container.start, container.finish, test);
end;

function findIfIn(_start, _end : DIterator; test : DTest) : DIterator;
begin
  while not equals(_start, _end) do
    begin
      if test(getRef(_start)^) then
        break;
      advance(_start);
    end;
  Result := _start;
end;

function some(container : DContainer; test : DTest) : Boolean;
begin
  Result := someIn(container.start, container.finish, test);
end;

function someIn(_start, _end : DIterator; test : DTest) : Boolean;
begin
  Result := false;
  while not equals(_start, _end) do
    begin
      if test(getRef(_start)^) then
        begin
          Result := true;
          break;
        end;
      advance(_start);
    end;
end;

////////////////////////////////////////////////////////////////////
//
// Freeing and Deleting
//
////////////////////////////////////////////////////////////////////
procedure objFree(container : DContainer);
begin
  objFreeIn(container.start, container.finish);
end;

procedure objFreeIn(_start, _end : DIterator);
begin
{$IFDEF FREEPOSSIBLE}
  while not equals(_start, _end) do
    begin
      getRef(_start)^.VObject.Free;
      advance(_start);
    end;
{$ELSE}
  raise DException.Create('Can''t free this data type.');
{$ENDIF}
end;

procedure objDispose(container : DContainer);
begin
  objDisposeIn(container.start, container.finish);
end;

procedure objDisposeIn(_start, _end : DIterator);
begin
{$IFDEF FREEPOSSIBLE}
  while not equals(_start, _end) do
    begin
      FreeMem(getRef(_start)^.VPointer);
      advance(_start);
    end
{$ELSE}
  raise DException.Create('Can''t free this data type.');
{$ENDIF}
end;

procedure objFreeKeys(assoc : DAssociative);
var s,e : DIterator;
begin
  s := assoc.start;
  SetToKey(s);
  e := assoc.finish;
  SetToKey(e);
  ObjFreeIn(s,e);
end;

////////////////////////////////////////////////////////////////////
//
// Hashing
//
////////////////////////////////////////////////////////////////////

function orderedHash(container : DContainer) : Integer;
begin
  Result := orderedHashIn(container.start, container.finish);
end;

function orderedHashIn(_start, _end : DIterator) : Integer;
var len : Integer;
    position : Integer;
    skip : Integer;
begin
  Result := 0;
  position := 0;
  len := distance(_start, _end);
  if len < 16 then
    skip := 1
  else
    skip := len div 16;

  while not equals(_start, _end) do
    begin
      Result := result xor (hashCode(getRef(_start)^) div (position mod 16) + 1);
      Inc(position);
      advanceBy(_start, skip);
    end;
end;

function unorderedHash(container : DContainer) : Integer;
begin
  Result := unorderedHashIn(container.start, container.finish);
end;

function unorderedHashIn(_start, _end : DIterator) : Integer;
begin
  Result := 0;
  while not equals(_start, _end) do
    begin
      Result := result xor hashCode(getRef(_start)^);
      advance(_start);
    end;
end;

////////////////////////////////////////////////////////////////////
//
// Permuting
//
////////////////////////////////////////////////////////////////////

function nextPermutation(container : DContainer; comparator : DComparator) : Boolean;
begin
  Result := nextPermutationIn(container.start, container.finish, comparator);
end;

function nextPermutationIn(_start, _end : DIterator; comparator : DComparator) : Boolean;
begin
  raise DNotImplemented.Create;
end;

function prevPermutation(container : DContainer; comparator : DComparator) : Boolean;
begin
  Result := prevPermutationIn(container.start, container.finish, comparator);
end;

function prevPermutationIn(_start, _end : DIterator; comparator : DComparator) : Boolean;
begin
  raise DNotImplemented.Create;
end;

////////////////////////////////////////////////////////////////////
//
// Removing
//
////////////////////////////////////////////////////////////////////
function _remove(container : DContainer; const obj : DObject) : DIterator;
begin
  container._remove(obj);
  // Result := _removeIn(container.start, container.finish, obj);
end;

//
// TODO : This leaves the remaining members undefined.  I don't like that;
// I think it should reduce the size somehow.  But, if you're removing in
// a range, you have no way of knowing what to do to a container that you're
// part of.  Another possibility is to set all the extra elements to empty
// DObjects.  The flaw there is that the container may have mixed types
// in it, which is not good.  This problem is not solved in STL either.
//
function _removeIn(_start, _end : DIterator; const obj : DObject) : DIterator;
var s : DIterator;
begin
  s := findIn(_start, _end, obj);
  if not equals(s, _end) then
    begin
      Result := _removeCopyIn(s, _end, _start, obj);
    end
  else
    Result := _end;
end;

function _removeTo(container : DContainer; dest : DIterator; const obj : DObject) : DIterator;
begin
  Result := _removeInTo(container.start, container.finish, dest, obj);
end;

function _removeInTo(_start, _end, dest : DIterator; const obj : DObject) : DIterator;
var s : DIterator;
    done : Boolean;
begin
  done := False;
  repeat
    s := findIn(_start, _end, obj);
    if not equals(s, _end) then
      begin
        _output(dest, getRef(s)^);
      end
    else
      done := true;
  until done;
  Result := dest;
end;

function _removeCopy(source, destination : DContainer; const obj : DObject) : DIterator;
begin
  Result := _removeCopyIn(source.start, source.finish, destination.finish, obj);
end;

function _removeCopyTo(source : DContainer; dest : DIterator; const obj : DObject) : DIterator;
begin
  Result := _removeCopyIn(source.start, source.finish, dest, obj);
end;

function _removeCopyIn(_start, _end, dest : DIterator; const obj : DObject) : DIterator;
var comparator : DComparator;
    o : PDObject;
begin
  getContainer(_start).getComparator(comparator);
  while not equals(_start, _end) do
    begin
      o := getRef(_start);
      if comparator(o^, obj) <> 0 then
        begin
          _output(dest, o^);
        end;
      advance(_start);
    end;
  Result := dest;
end;

function remove(container : DContainer; const objs : array of const) : DIterator;
var i : Integer;
begin
  for i := Low(objs) to High(objs) do
    _remove(container, objs[i]);
end;

function removeIn(_start, _end : DIterator; const objs : array of const) : DIterator;
var i : Integer;
begin
  for i := Low(objs) to High(objs) do
    _removeIn(_start, _end, objs[i]);
end;

function removeTo(container : DContainer; dest : DIterator; const objs : array of const) : DIterator;
var i : Integer;
begin
  for i := Low(objs) to High(objs) do
    _removeTo(container, dest, objs[i]);
end;

function removeInTo(_start, _end, dest : DIterator; const objs : array of const) : DIterator;
var i : Integer;
begin
  for i := Low(objs) to High(objs) do
    _removeInTo(_start, _end, dest, objs[i]);
end;

function removeCopy(source, destination : DContainer; const objs : array of const) : DIterator;
var i : Integer;
begin
  for i := Low(objs) to High(objs) do
    _removeCopy(source, destination, objs[i]);
end;

function removeCopyTo(source : DContainer; dest : DIterator; const objs : array of const) : DIterator;
var i : Integer;
begin
  for i := Low(objs) to High(objs) do
    _removeCopyTo(source, dest, objs[i]);
end;

function removeCopyIn(_start, _end, dest : DIterator; const objs : array of const) : DIterator;
var i : Integer;
begin
  for i := Low(objs) to High(objs) do
    _removeCopyIn(_start, _end, dest, objs[i]);
end;

function removeCopyIf(source, destination : DContainer; test : DTest) : DIterator;
begin
  Result := removeCopyIfIn(source.start, source.finish, destination.finish, test);
end;

function removeCopyIfTo(source : DContainer; dest : DIterator; test : DTest) : DIterator;
begin
  Result := removeCopyIfIn(source.start, source.finish, dest, test);
end;

function removeCopyIfIn(_start, _end, dest : DIterator; test : DTest) : DIterator;
var o : PDObject;
begin
  while not equals(_start, _end) do
    begin
      o := getRef(_start);
      if test(o^) then
        begin
          _output(dest, o^);
        end;
      advance(_start);
    end;
  Result := dest;
end;

function removeIf(container : DContainer; test : DTest) : DIterator;
begin
  Result := removeIfIn(container.start, container.finish, test);
end;

function removeIfIn(_start, _end : DIterator; test : DTest) : DIterator;
var s : DIterator;
begin
  s := findIfIn(_start, _end, test);
  if not equals(s, _end) then
    Result := removeCopyIfIn(s, _end, s, test)
  else
    Result := _end;
end;

function removeIfTo(container : DContainer; dest : DIterator; test : DTest) : DIteratorPair;
begin
  Result := removeIfInTo(container.start, container.finish, dest, test);
end;

function removeIfInTo(_start, _end, dest : DIterator; test : DTest) : DIteratorPair;
var o : PDObject;
    store : DIterator;
begin
  store := _start;
  while not equals(_start, _end) do
    begin
      o := getRef(_start);
      if test(o^) then
        begin
          _output(dest, o^);
        end
      else
        _output(store, o^);
      advance(_start);
    end;
  Result.first := dest;
  Result.second := store;
end;

////////////////////////////////////////////////////////////////////
//
// Replacing
//
////////////////////////////////////////////////////////////////////
function _replace(container : DContainer; const obj1, obj2 : DObject) : Integer;
begin
  Result := _replaceIn(container.start, container.finish, obj1, obj2);
end;

function _replaceIn(_start, _end : DIterator; const obj1, obj2 : DObject) : Integer;
begin
  Result := _replaceCopyInTo(_start, _end, _start, obj1, obj2);
end;

function _replaceCopy(con1, con2 : DContainer; const obj1, obj2 : DObject) : Integer;
begin
  Result := _replaceCopyInTo(con1.start, con1.finish, con2.finish, obj1, obj2);
end;

function _replaceCopyTo(container : DContainer; dest : DIterator; const obj1, obj2 : DObject) : Integer;
begin
  Result := _replaceCopyInTo(container.start, container.finish, dest, obj1, obj2);
end;

function _replaceCopyInTo(_start, _end, dest : DIterator; const obj1, obj2 : DObject) : Integer;
var comparator : DComparator;
    o : PDObject;
begin
  getContainer(_start).getComparator(comparator);
  Result := 0;
  while not equals(_start, _end) do
    begin
      o := getRef(_start);
      if comparator(o^, obj1) = 0 then
        begin
          _output(dest, obj2);
          Inc(result);
        end
      else
        _output(dest, o^);
      advance(_start);
    end;
end;

function _replaceCopyIf(con1, con2 : DContainer; test : DTest; const obj : DObject) : Integer;
begin
  Result := _replaceCopyIfInTo(con1.start, con1.finish, con2.finish, test, obj);
end;

function _replaceCopyIfTo(container : DContainer; dest : DIterator; test : DTest; const obj : DObject) : Integer;
begin
  Result := _replaceCopyIfInTo(container.start, container.finish, dest, test, obj);
end;

function _replaceCopyIfInTo(_start, _end, dest : DIterator; test : DTest; const obj : DObject) : Integer;
var o : PDObject;
begin
  Result := 0;
  while not equals(_start, _end) do
    begin
      o := getRef(_start);
      if test(o^) then
        begin
          _output(dest, obj);
          Inc(result);
        end
      else
        _output(dest, o^);
      advance(_start);
    end;
end;

function _replaceIf(container : DContainer; test : DTest; const obj : DObject) : Integer;
begin
  Result := _replaceCopyIfInTo(container.start, container.finish, container.start, test, obj);
end;

function _replaceIfIn(_start, _end : DIterator; test : DTest; const obj : DObject) : Integer;
begin
  Result := _replaceCopyIfInTo(_start, _end, _start, test, obj);
end;

function replace(container : DContainer; const objs1, objs2 : array of const) : Integer;
var i : Integer;
begin
  Result := 0;
  Assert(High(objs1) = High(objs2));
  for i := Low(objs1) to High(objs1) do
    Result := result + _replace(container, objs1[i], objs2[i]);
end;

function replaceIn(_start, _end : DIterator; const objs1, objs2 : array of const) : Integer;
var i : Integer;
begin
  Result := 0;
  Assert(High(objs1) = High(objs2));
  for i := Low(objs1) to High(objs1) do
    Result := result + _replaceIn(_start, _end, objs1[i], objs2[i]);
end;

function replaceCopy(con1, con2 : DContainer; const objs1, objs2 : array of const) : Integer;
var i : Integer;
begin
  Result := 0;
  Assert(High(objs1) = High(objs2));
  for i := Low(objs1) to High(objs1) do
    Result := result + _replaceCopy(con1, con2, objs1[i], objs2[i]);
end;

function replaceCopyTo(container : DContainer; dest : DIterator; const objs1, objs2 : array of const) : Integer;
var i : Integer;
begin
  Result := 0;
  Assert(High(objs1) = High(objs2));
  for i := Low(objs1) to High(objs1) do
    Result := result + _replaceCopyTo(container, dest, objs1[i], objs2[i]);
end;

function replaceCopyInTo(_start, _end, dest : DIterator; const objs1, objs2 : array of const) : Integer;
var i : Integer;
begin
  Result := 0;
  Assert(High(objs1) = High(objs2));
  for i := Low(objs1) to High(objs1) do
    Result := result + _replaceCopyInTo(_start, _end, dest, objs1[i], objs2[i]);
end;

function replaceCopyIf(con1, con2 : DContainer; test : DTest; const objs : array of const) : Integer;
var i : Integer;
begin
  Result := 0;
  Assert(High(objs) = Low(objs));
  for i := Low(objs) to High(objs) do
    Result := result + _replaceCopyIf(con1, con2, test, objs[i]);
end;

function replaceCopyIfTo(container : DContainer; dest : DIterator; test : DTest; const objs : array of const) : Integer;
var i : Integer;
begin
  Result := 0;
  Assert(High(objs) = Low(objs));
  for i := Low(objs) to High(objs) do
    Result := result + _replaceCopyIfTo(container, dest, test, objs[i]);
end;

function replaceCopyIfInTo(_start, _end, dest : DIterator; test : DTest; const objs : array of const) : Integer;
var i : Integer;
begin
  Result := 0;
  Assert(High(objs) = Low(objs));
  for i := Low(objs) to High(objs) do
    Result := result + _replaceCopyIfInTo(_start, _end, dest, test, objs[i]);
end;

function replaceIf(container : DContainer; test : DTest; const objs : array of const) : Integer;
var i : Integer;
begin
  Result := 0;
  Assert(High(objs) = Low(objs));
  for i := Low(objs) to High(objs) do
    Result := result + _replaceIf(container, test, objs[i]);
end;

function replaceIfIn(_start, _end : DIterator; test : DTest; const objs : array of const) : Integer;
var i : Integer;
begin
  Result := 0;
  Assert(High(objs) = Low(objs));
  for i := Low(objs) to High(objs) do
    Result := result + _replaceIfIn(_start, _end, test, objs[i]);
end;

////////////////////////////////////////////////////////////////////
//
// Reversing
//
////////////////////////////////////////////////////////////////////
procedure reverse(container : DContainer);
begin
  reverseIn(container.start, container.finish);
end;

procedure reverseIn(_start, _end : DIterator);
var tmp : DObject;
begin
  if not equals(_start, _end) then
    begin
      retreat(_end);
      while not equals(_start, _end) do
        begin
          tmp := getRef(_start)^;
          putRef(_start, getRef(_end)^);
          putRef(_end, tmp);
          retreat(_end);
          if not equals(_start, _end) then
            advance(_start);
        end;
    end;
end;

procedure reverseCopy(con1, con2 : DContainer);
begin
  reverseCopyInTo(con1.start, con1.finish, con1.finish);
end;

procedure reverseCopyTo(container : DContainer; dest : DIterator);
begin
  reverseCopyInTo(container.start, container.finish, dest);
end;

procedure reverseCopyInTo(_start, _end, dest : DIterator);
begin
  if not equals(_start, _end) then
    begin
      retreat(_end);
      repeat
        _output(dest, getRef(_end)^);
        retreat(_end);
      until equals(_start, _end);
    end;
end;

////////////////////////////////////////////////////////////////////
//
// Rotating
//
////////////////////////////////////////////////////////////////////
procedure rotate(first, middle, last : DIterator);
var tmp : DObject;
    i : DIterator;
begin
  if [diBidirectional,diRandom] * first.flags <> [] then
    begin
      reverseIn(first, middle);
      reverseIn(middle, last);
      reverseIn(first, last);
    end
  else if diForward in first.flags then
    begin
      i := middle;
      repeat
        tmp := getRef(first)^;
        putRef(first, getRef(i)^);
        putRef(i, tmp);
        advance(first);
        advance(i);

        if equals(first, middle) then
          begin
            if equals(i, last) then
              exit;
            middle := i;
          end
        else if equals(i, last) then
          i := middle;

      until false;

    end;
end;

function rotateCopy(first, middle, last, dest : DIterator) : DIterator;
begin
  Result := copyInTo(first, middle, copyInTo(middle, last, dest));
end;

////////////////////////////////////////////////////////////////////
//
// Sets
//
////////////////////////////////////////////////////////////////////
function includes(master, subset : DContainer) : Boolean;
var comparator : DComparator;
begin
  master.getComparator(comparator);
  Result := includesInWith(master.start, master.finish, subset.start, subset.finish, comparator);
end;

function includesWith(master, subset : DContainer; comparator : DComparator) : Boolean;
begin
  Result := includesInWith(master.start, master.finish, subset.start, subset.finish, comparator);
end;

function includesIn(startMaster, finishMaster, startSubset, finishSubset : DIterator) : Boolean;
var comparator : DComparator;
begin
  getContainer(startMaster).getComparator(comparator);
  Result := includesInWith(startMaster, finishMaster, startSubset, finishSubset, comparator);
end;

function includesInWith(startMaster, finishMaster, startSubset, finishSubset : DIterator; comparator : DComparator) : Boolean;
var c : Integer;
begin
  while (not equals(startMaster, finishMaster)) and (not equals(startSubset, finishSubset)) do
    begin
      c := comparator(getRef(startSubset)^, getRef(startMaster)^);
      if c < 0 then
        begin
          Result := false;
          exit;
        end
      else if c > 0 then
        advance(startMaster)
      else
        begin
          advance(startMaster);
          advance(startSubset);
        end;
    end;
  Result := equals(startSubset, finishSubset);
end;

function setDifference(con1, con2 : DContainer; dest : DIterator) : DIterator;
var comparator : DComparator;
begin
  con1.getComparator(comparator);
  Result := setDifferenceInWith(con1.start, con1.finish, con2.start, con2.finish, dest, comparator);
end;

function setDifferenceIn(start1, finish1, start2, finish2, dest : DIterator) : DIterator;
var comparator : DComparator;
begin
  getContainer(start1).getComparator(comparator);
  Result := setDifferenceInWith(start1, finish1, start2, finish2, dest, comparator);
end;

function setDifferenceWith(con1, con2 : DContainer; dest : DIterator; comparator : DComparator) : DIterator;
begin
  Result := setDifferenceInWith(con1.start, con1.finish, con2.start, con2.finish, dest, comparator);
end;

function setDifferenceInWith(start1, finish1, start2, finish2, dest : DIterator; comparator : DComparator) : DIterator;
var c : Integer;
begin
  while (not equals(start1, finish1)) and (not equals(start2, finish2)) do
    begin
      c := comparator(getRef(start1)^, getRef(start2)^);
      if c < 0 then
        begin
          _outputRef(dest, getRef(start1)^);
          advance(start1);
        end
      else if c > 0 then
        advance(start2)
      else
        begin
          advance(start1);
          advance(start2);
        end;
    end;
  Result := CopyInTo(start1, finish1, dest);
end;

function setIntersection(con1, con2 : DContainer; dest : DIterator) : DIterator;
begin
  Result := setIntersectionIn(con1.start, con1.finish, con2.start, con2.finish, dest);
end;

function setIntersectionIn(start1, finish1, start2, finish2, dest : DIterator) : DIterator;
var comparator : DComparator;
begin
  getContainer(start1).getComparator(comparator);
  Result := setIntersectionInWith(start1, finish1, start2, finish2, dest, comparator);
end;

function setIntersectionWith(con1, con2 : DContainer; dest : DIterator; comparator : DComparator) : DIterator;
begin
  Result := setIntersectionInWith(con1.start, con1.finish, con2.start, con2.finish, dest, comparator);
end;

function setIntersectionInWith(start1, finish1, start2, finish2, dest : DIterator; comparator : DComparator) : DIterator;
var c : Integer;
begin
  while (not equals(start1, finish1)) and (not equals(start2, finish2)) do
    begin
      c := comparator(getRef(start1)^, getRef(start2)^);
      if c < 0 then
        advance(start1)
      else if c > 0 then
        advance(start2)
      else
        begin
          _outputRef(dest, getRef(start1)^);
          advance(start1);
          advance(start2);
        end;
    end;
  Result := dest;
end;

function setSymmetricDifference(con1, con2 : DContainer; dest : DIterator) : DIterator;
begin
  Result := setSymmetricDifferenceIn(con1.start, con1.finish, con2.start, con2.finish, dest);
end;

function setSymmetricDifferenceIn(start1, finish1, start2, finish2, dest : DIterator) : DIterator;
var comparator : DComparator;
begin
  getContainer(start1).getComparator(comparator);
  Result := setSymmetricDifferenceInWith(start1, finish1, start2, finish2, dest, comparator);
end;

function setSymmetricDifferenceWith(con1, con2 : DContainer; dest : DIterator; comparator : DComparator) : DIterator;
begin
  Result := setSymmetricDifferenceInWith(con1.start, con1.finish, con2.start, con2.finish, dest, comparator);
end;

function setSymmetricDifferenceInWith(start1, finish1, start2, finish2, dest : DIterator; comparator : DComparator) : DIterator;
var c : Integer;
begin
  while (not equals(start1, finish1)) and (not equals(start2, finish2)) do
    begin
      c := comparator(getRef(start1)^, getRef(start2)^);
      if c < 0 then
        begin
          _output(dest, getRef(start1)^);
          advance(start1);
        end
      else if c > 0 then
        begin
          _output(dest, getRef(start2)^);
          advance(start2);
        end
      else
        begin
          advance(start1);
          advance(start2);
        end;
    end;
  Result := CopyInTo(start2, finish2, CopyInTo(start1, finish1, dest));

end;

function setUnion(con1, con2 : DContainer; dest : DIterator) : DIterator;
begin
  Result := setUnionIn(con1.start, con1.finish, con2.start, con2.finish, dest);
end;

function setUnionIn(start1, finish1, start2, finish2, dest : DIterator) : DIterator;
var comparator : DComparator;
begin
  getContainer(start1).getComparator(comparator);
  Result := setUnionInWith(start1, finish1, start2, finish2, dest, comparator);
end;

function setUnionWith(con1, con2 : DContainer; dest : DIterator; comparator : DComparator) : DIterator;
begin
  Result := setUnionInWith(con1.start, con1.finish, con2.start, con2.finish, dest, comparator);
end;

function setUnionInWith(start1, finish1, start2, finish2, dest : DIterator; comparator : DComparator) : DIterator;
var c : Integer;
begin
  while (not equals(start1, finish1)) and (not equals(start2, finish2)) do
    begin
      c := comparator(getRef(start1)^, getRef(start2)^);
      if c < 0 then
        begin
          _outputRef(dest, getRef(start1)^);
          advance(start1);
        end
      else if c > 0 then
        begin
          _outputRef(dest, getRef(start2)^);
          advance(start2);
        end
      else
        begin
          _outputRef(dest, getRef(start1)^);
        end;

    end;

  Result := CopyInTo(start2, finish2, CopyInTo(start1, finish1, dest));

end;

////////////////////////////////////////////////////////////////////
//
// Shuffling
//
////////////////////////////////////////////////////////////////////
procedure randomShuffle(container : DContainer);
begin
  randomShuffleIn(container.start, container.finish);
end;

procedure randomShuffleIn(_start, _end : DIterator);
var i,j : DIterator;
    tmp : DObject;
    total : Integer;
begin
  if not equals(_start, _end) then
    begin
      i := _start;
      total := distance(_start, _end);
      while not equals(i, _end) do
        begin
          j := _start;
          advanceBy(j, Random(total));
          tmp := getRef(i)^;
          putRef(i, getRef(j)^);
          putRef(j, tmp);
          advance(i);
        end;
    end;
end;

////////////////////////////////////////////////////////////////////
//
// Sorting
//
////////////////////////////////////////////////////////////////////

// This is the internal sorting routine, that handle all cases.
procedure masterSort(_start, _end : DIterator; comparator : DComparator; stable : Boolean);
var a : DArray;
    m3arr : TM3Array;
    dist : Integer;
    iter : DIterator;
    comp : TMergeCompareEx;
    i : Integer;
begin
  dist := distance(_start, _end);
  if dist > 1 then
    begin
      // Use merge sort for stability

      m3arr := TM3Array.Create;

      try
        m3arr.Capacity := dist;

        // we need to make a duplicate
        a := DArray.Create;
        a.ensureCapacity(dist);

        try

          // We are creating a direct copy of the input range
          iter := _start;
          while not equals(iter, _end) do
            begin
              a.addRef(getRef(iter)^);
              advance(iter);
            end;

          // add pointers to the m3array
          iter := a.start;
          while not atEnd(iter) do
            begin
              m3arr.add(getRef(iter));
              advance(iter);
            end;

          // prep our comparator -- note that this works because the
          // comparators expect to get const DObjects in their parameters,
          // which Delphi does as pointers.  The Waldenburg sort routines
          // work off pointers as well.
          TMethod(comp).Data := TMethod(comparator).data;
          TMethod(comp).Code := TMethod(comparator).code;

          if stable then
            m3arr.MergeSortEx(comp)
          else
            m3arr.QuickSortEx(comp);

          i := 0;
          iter := _start;
          while not equals(iter, _end) do
            begin
              putRef(iter, PDObject(m3arr.items[i])^);
              inc(i);
              advance(iter);
            end;

        finally
          // This clears everything out of a, but doesn't free the memory.
          // We are doing this because A is a direct copy of the DObjects.
          a._clear(true);
          a.free;
        end;

      finally
        m3arr.free;
      end;
    end;
end;


procedure sort(sequence : DSequence);
begin
  sortIn(sequence.start, sequence.finish);
end;

procedure sortIn(_start, _end : DIterator);
var comparator : DComparator;
begin
  Assert(getContainer(_start) = getContainer(_end));
  getContainer(_start).getComparator(comparator);
  sortInWith(_start, _end, comparator);
end;

procedure sortWith(sequence : DSequence; comparator : DComparator);
begin
  sortInWith(sequence.start, sequence.finish, comparator);
end;

procedure sortInWith(_start, _end : DIterator; comparator : DComparator);
begin
  masterSort(_start, _end, comparator, false);
end;

procedure stableSort(sequence : DSequence);
begin
  stableSortIn(sequence.start, sequence.finish);
end;

procedure stableSortIn(_start, _end : DIterator);
var comparator : DComparator;
begin
  Assert(getContainer(_start) = getContainer(_end));
  getContainer(_start).getComparator(comparator);
  stableSortInWith(_start, _end, comparator);
end;

procedure stableSortWith(sequence : DSequence; comparator : DComparator);
begin
  stableSortInWith(sequence.start, sequence.finish, comparator);
end;

procedure stableSortInWith(_start, _end : DIterator; comparator : DComparator);
begin
  masterSort(_start, _end, comparator, true);
end;


////////////////////////////////////////////////////////////////////
//
// Swapping
//
////////////////////////////////////////////////////////////////////
procedure iterSwap(iter1, iter2 : DIterator);
var tmp : DObject;
begin
  tmp := getRef(iter1)^;
  putRef(iter1, getRef(iter2)^);
  putRef(iter2, tmp);
end;

procedure swapRanges(con1, con2 : DContainer);
begin
  swapRangesInTo(con1.start, con1.finish, con1.start);
end;

procedure swapRangesInTo(start1, end1, start2 : DIterator);
var tmp : DObject;
begin
  while not equals(start1, end1) do
    begin
      tmp := getRef(start1)^;
      putRef(start1, getRef(start2)^);
      putRef(start2, tmp);
      advance(start1);
      advance(start2);
    end;
end;

////////////////////////////////////////////////////////////////////
//
// Transforming
//
////////////////////////////////////////////////////////////////////
function collect(container : DContainer; unary : DUnary) : DContainer;
begin
  Result := collectIn(container.start, container.finish, unary);
end;

function collectIn(_start, _end : DIterator; unary : DUnary) : DContainer;
begin
  Result := getContainer(_start).clone;
  while not equals(_start, _end) do
    begin
      Result.addRef(unary(getRef(_start)^));
    end;
end;

procedure transformBinary(con1, con2, output : DContainer; binary : DBinary);
begin
  transformBinaryTo(con1, con2, output.finish, binary);
end;

function transformBinaryTo(con1, con2 : DContainer; output : DIterator; binary : DBinary) : DIterator;
begin
  Result := transformBinaryInTo(con1.start, con1.finish, con2.start, output, binary);
end;

function transformBinaryInTo(start1, finish1, start2, output : DIterator; binary : DBinary) : DIterator;
begin
  while not equals(start1, finish1) do
    begin
      _outputRef(output, binary(getRef(start1)^, getRef(start2)^));
      advance(start1);
      advance(start2);
    end;
  Result := output;
end;

procedure transformUnary(container, output : DContainer; unary : DUnary);
begin
  transformUnaryTo(container, output.finish, unary);
end;

function transformUnaryTo(container : DContainer; output : DIterator; unary : DUnary) : DIterator;
begin
  Result := transformUnaryInto(container.start, container.finish, output, unary);
end;

function transformunaryInTo(_start, _finish, output : DIterator; unary : DUnary) : DIterator;
begin
  while not equals(_start, _finish) do
    begin
      _outputRef(output, unary(getRef(_start)^));
      advance(_start);
    end;
end;

////////////////////////////////////////////////////////////////////
//
// Morphing closures
//
////////////////////////////////////////////////////////////////////

function MakeComparatorEx(proc : DComparatorProc; ptr : Pointer) : DComparator;
begin
  TMethod(result).data := ptr;
  TMethod(result).code := @proc;
end;

function MakeEqualsEx(proc : DEqualsProc; ptr : Pointer) : DEquals;
begin
  TMethod(result).data := ptr;
  TMethod(result).code := @proc;
end;

function MakeTestEx(proc : DTestProc; ptr : Pointer) : DTest;
begin
  TMethod(result).data := ptr;
  TMethod(result).code := @proc;
end;

function MakeApplyEx(proc : DApplyProc; ptr : Pointer) : DApply;
begin
  TMethod(result).data := ptr;
  TMethod(result).code := @proc;
end;

function MakeUnaryEx(proc : DUnaryProc; ptr : Pointer) : DUnary;
begin
  TMethod(result).data := ptr;
  TMethod(result).code := @proc;
end;

function MakeBinaryEx(proc : DBinaryProc; ptr : Pointer) : DBinary;
begin
  TMethod(result).data := ptr;
  TMethod(result).code := @proc;
end;

function MakeHashEx(proc : DHashProc; ptr : Pointer) : DHash;
begin
  TMethod(result).data := ptr;
  TMethod(result).code := @proc;
end;

function MakeGeneratorEx(proc : DGeneratorProc; ptr : Pointer) : DGenerator;
begin
  TMethod(result).data := ptr;
  TMethod(result).code := @proc;
end;

function MakeComparator(proc : DComparatorProc) : DComparator;
begin
  TMethod(result).data := nil;
  TMethod(result).code := @proc;
end;

function MakeEquals(proc : DEqualsProc) : DEquals;
begin
  TMethod(result).data := nil;
  TMethod(result).code := @proc;
end;

function MakeTest(proc : DTestProc) : DTest;
begin
  TMethod(result).data := nil;
  TMethod(result).code := @proc;
end;

function MakeApply(proc : DApplyProc) : DApply;
begin
  TMethod(result).data := nil;
  TMethod(result).code := @proc;
end;

function MakeUnary(proc : DUnaryProc) : DUnary;
begin
  TMethod(result).data := nil;
  TMethod(result).code := @proc;
end;

function MakeBinary(proc : DBinaryProc) : DBinary;
begin
  TMethod(result).data := nil;
  TMethod(result).code := @proc;
end;

function MakeHash(proc : DHashProc) : DHash;
begin
  TMethod(result).data := nil;
  TMethod(result).code := @proc;
end;

function MakeGenerator(proc : DGeneratorProc) : DGenerator;
begin
  TMethod(result).data := nil;
  TMethod(result).code := @proc;
end;

var
  PrintRegistry : DHashMap = nil;

procedure RegisterDeCALPrinter(cls : TClass; prt : DPrinterProc);
begin
  if PrintRegistry = nil then
    PrintRegistry := DHashMap.Create;
  PrintRegistry.putPair([cls, @prt]);
end;

function PrintString(const obj : DObject) : string;
var dp : DPrinterProc;
    iter : DIterator;
    printed : Boolean;
    cls : TClass;
begin
  case obj.VType of
      vtInteger:
        Result := Format('%d', [obj.VInteger]);
      vtPointer:
        Result := Format('%x', [obj.VInteger]);
      vtObject:
        begin
          printed := False;
          if PrintRegistry <> nil then
            begin
              // Walk the class tree to determine if we have a printing
              // routine suitable for this object.
              cls := obj.VObject.ClassType;
              while (not printed) and (cls <> nil) do
                begin
                  iter := PrintRegistry.locate([obj.VObject.ClassType]);
                  if not atEnd(iter) then
                    begin
                      SetToValue(iter);
                      dp := GetPointer(iter);
                      Result := dp(obj.VObject);
                      printed := true;
                    end
                  else
                    cls := cls.ClassParent;
                end;
            end;
          if not printed then
            Result := Format('%x', [obj.VInteger]);
        end;
      vtPAnsiChar:
        if obj.VPChar = nil then
          Result := 'nil'
        else
          Result := string(AnsiString(obj.VPChar));
      vtClass:
        Result := obj.VClass.ClassName;
      vtPWideChar:
        if obj.VPWideChar = nil then
          Result := 'nil'
        else
          Result := WideCharToString(obj.VPWideChar);
      vtInterface:
        begin
          Result := Format('%x', [obj.VInterface]);
        end;
      vtBoolean:
        begin
          if obj.VBoolean then
            Result := 'True'
          else
            Result := 'False';
        end;
      vtChar:
        {$IFDEF UNICODE}
        if obj.VChar < #128 then
          Result := Char(obj.VChar)
        else
          Result := string(AnsiString(obj.VChar)); // correct conversion for char >= #128
        {$ELSE}
        Result := obj.VChar;
        {$ENDIF UNICODE}
      vtExtended:
        Result := Format('%g', [obj.VExtended^]);
      vtString:
        Result := string(obj.VString^);
      vtWideChar:
        Result := WideCharLenToString(@obj.VWideChar, 1);
      vtAnsiString:
        Result := string(AnsiString(obj.VAnsiString));
      {$IFDEF UNICODE}
      vtUnicodeString:
        Result := UnicodeString(obj.VUnicodeString);
      {$ENDIF UNICODE}
      vtCurrency:
        Result := Format('%m', [obj.VCurrency^]);
      vtVariant:
        Result := VarToStr(obj.VVariant^);
      vtWideString:
        Result := WideCharToString(obj.VWideString);
  end;
end;

procedure ApplyPrint(ptr : Pointer; const obj : DObject);
begin
  write(PrintString(obj));
end;

procedure ApplyPrintLN(ptr : Pointer; const obj : DObject);
begin
  ApplyPrint(ptr, obj);
  Writeln;
end;

procedure Init;
begin
end;

procedure Cleanup;
begin
  nil_node.free;
  PrintRegistry.Free;
{$IFDEF USEPOOLS}
  CleanupPools;
{$ENDIF}
end;

initialization
  Init;
  emptyDObject.VType := vtInteger;
  emptyDObject.VInteger := 0;
finalization
  Cleanup;
end.

