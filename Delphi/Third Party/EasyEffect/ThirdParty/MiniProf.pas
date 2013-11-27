unit miniprof;
{
  Встраиваемый профайлер MiniProfiler 1.5

  Евгений Кацевман                  |
  Eugene Katsevman                  |
                                    |
  Aspi Soft, Kiev, Ukraine          |
  E-mail: aspi@i.kiev.ua            |
  http://miriada.com.ua             |
  Alexander Shlyahto                |
  Александр Шляхто                  |
                                    |
  2005                              |

  Использовано: Встраиваемый профайлер jDPro 1.01
                Mini-profiler for Delphi 1-7 , version 1.01
                Евгений Кацевман
                * Eugene Katsevman, 2005
                * mailto: jandor@yandex.ru , eugene.katsevman@gmail.com
}

(*
Чтобы использовать профайлер
1) подключите этот модуль к профилируемым модулям
2) добавьте MINI_PROFILER в Conditional defines проекта
В окончательном елизе можно убрать этот дефайн и пересобрать
проект уже без профайлера. Поэтому логично заключать вызовы профайлера в
 {$IFDEF MINI_PROFILER}  {$ENDIF}
Для профилирования блока кода поставьте по его краям вызовы
MiniProfiler.SectionBegin(имя блока);
MiniProfiler.SectionEnd;

Для вычисления среднего, максимума или минимума используйте
class procedure CalcMax(const aName: string; aValue: Single);
class procedure CalcMin(const aName: string; aValue: Single);
class procedure CalcAverage(const aName: string; aValue: Single);

Пример :

  MiniProfiler.SectionBegin('Весь цикл');
  for i := 0 to 10000 do
  begin
    MiniProfiler.SectionBegin('Каждая итерация');
      writeln(i);
    MiniProfiler.SectionEnd;
    MiniProfiler.CalcMax('I максимальное', i);
    MiniProfiler.CalcMin('I минимальное', i);
    MiniProfiler.CalcAverage('I среднее', i);
  end;
  MiniProfiler.SectionEnd;

После завершения программы результаты профилирования сохраняются
в файле profile.txt, а min/max/average значения в файле values.txt
в каталоге с программой

Мой результат для данного примера:
profile.txt
           Name|Call Count| Total time (s)|Average time (s)|    Max time (s)|    Min time (s)
      Весь цикл          1       12.250279        12.250279        12.250279        12.250279
Каждая итерация      10001       12.149688         0.001215         0.015335         0.000014
values.txt
          Name Value
I максимальное     10000.000000
 I минимальное         0.000000
     I среднее      5000.000000
*)

interface

{$IFDEF MINI_PROFILER}

//Раскомментируйте для профилирования многопоточных приложений
//(замедляет профайлер на 20%)
//{$DEFINE MINI_PROFILER_MULTITHREAD}

uses Windows, SysUtils, SyncObjs;

const
  PRECISION = 6; //Точность вывода

type

  MiniProfiler = class
  public
    class procedure SectionBegin(const aName: string);
    class procedure SectionEnd;

    class procedure CalcMax(const aName: string; aValue: Single);
    class procedure CalcMin(const aName: string; aValue: Single);
    class procedure CalcAverage(const aName: string; aValue: Single);
    class procedure SaveToFile(const aProfileFileName: string = 'profile.txt';
      const aValuesFileName: string = 'values.txt');
  end;

{$ENDIF}
implementation
{$IFDEF MINI_PROFILER}

type

  PHashElement = ^RHashElement;
  RHashElement = record
    rNext: PHashElement;
    rReturnAddress: Pointer;
    rName: string;
    isSection: Boolean;
    case Integer of
      0: (rCallCount: Int64;
        rTotalTime: Int64;
        rMaxTime: Int64;
        rMinTime: Int64; );
      1: (rCount: Int64;
        rValue: Double;
        rDivide: Boolean);

  end;

  PStackElement = ^RStackElement;
  PPStackElement = ^PStackElement;
  RStackElement = record
    rPrior: PStackElement;
    rSectionAddress: PHashElement;
    rStartTime: Int64;
  end;

  PStackHashElement= ^TStackHashElement;

  TStackHashElement = record
    ID: Integer;
    next: PStackHashElement;
    pStack: PStackElement;
  end;

var
  //SecBegStack: PStackElement;
  HashSecAdrTbl: array[0..255] of PHashElement;
  HashThreadIDTbl: array[0..255] of PStackHashElement;
{$IFDEF MINI_PROFILER_MULTITHREAD}
  ProfLock: TCriticalSection;
{$ENDIF}



function GetStackForThread: PPStackElement;
var
  I,ID: Integer;
  P:PStackHashElement;

begin
  ID:=GetCurrentThread;
  I := ID and $FF;
  P :=  HashThreadIDTbl[I];
  while P <> nil do
  begin

    if P.ID=ID then
    begin
    Result:=@P.pStack;
    Exit;
    end;
   P:=P.next;
  end;
  // Не нашли такой адрес -создаем элемент хеша
  New(P);
  P.ID:=ID;
  P.pStack:=nil;
  P.next := HashThreadIDTbl[I];
  HashThreadIDTbl[I] := P;
  Result:=@P.pStack;
//  Result:=@SecBegStack;
end;





function FindOrCreateSection(aAddress: Pointer; Section: Boolean): PHashElement;
var
  I: Integer;
begin
  I := Integer(aAddress) and $FF;
  Result := HashSecAdrTbl[I];
  while Result <> nil do
  begin
    if Result.rReturnAddress = aAddress then
      Exit;

    Result := Result.rNext;
  end;
  // Не нашли такой адрес -создаем элемент хеша
  New(Result);
  Result.rReturnAddress := aAddress;
  Result.rCount := 0; //независимо от реального типа.
  Result.isSection := Section; // тип
  Result.rNext := HashSecAdrTbl[I];
  HashSecAdrTbl[I] := Result;
end;


class procedure MiniProfiler.SectionBegin(const aName: string);
var
  RetAddr: Pointer;
  StackElement: PStackElement;
  HashElement: PHashElement;
  SecBegStack:PPStackElement;
begin
  // Получение адреса кода, следующего за вызовом этого метода
  asm
    MOV ECX, [EBP + 4]
    MOV RetAddr, ECX
  end;
{$IFDEF MINI_PROFILER_MULTITHREAD}
  ProfLock.Enter;
  try
{$ENDIF}

    //поиск в хеше
    HashElement := FindOrCreateSection(RetAddr, True);
    with HashElement^ do
    begin
      rName := aName;
      Inc(rCallCount);
    end;
    New(StackElement);
    SecBegStack:=GetStackForThread;
    with StackElement^ do
    begin
      rSectionAddress := HashElement;
      rPrior := SecBegStack^;
      SecBegStack^ := StackElement;
//      LogWriteln(SecBegStack^.rSectionAddress.rName+' entered');
      QueryPerformanceCounter(StackElement.rStartTime);
    end
{$IFDEF MINI_PROFILER_MULTITHREAD}
  finally
    ProfLock.Leave;
  end;
{$ENDIF}
end;

class procedure MiniProfiler.SectionEnd;
var
  SE: PStackElement;
  EndTime: Int64;
  Diff: Int64;
  SecBegStack:PPStackElement;
begin
  QueryPerformanceCounter(EndTime);
  SecBegStack:=GetStackForThread;
  if Assigned(SecBegStack^) then
  begin
{$IFDEF MINI_PROFILER_MULTITHREAD}
    ProfLock.Enter;
    try
{$ENDIF}

      with SecBegStack^.rSectionAddress^ do
      begin
        Diff := EndTime - SecBegStack^.rStartTime;
        Inc(rTotalTime, Diff);
        if rCallCount = 1 then
        begin
          rMinTime := Diff;
          rMaxTime := Diff;
        end
        else
        begin
          if rMaxTime < Diff then
            rMaxTime := Diff;
          if rMinTime > Diff then
            rMinTime := Diff;
        end;
      end;

      SE := SecBegStack^.rPrior;
//      LogWriteln(SecBegStack^.rSectionAddress.rName+' exited');
      Dispose(SecBegStack^);
      SecBegStack^ := SE;
{$IFDEF MINI_PROFILER_MULTITHREAD}
    finally
      ProfLock.Leave;
    end;
{$ENDIF}
  end;
end;

class procedure MiniProfiler.SaveToFile(const aProfileFileName: string;
  const aValuesFileName: string);
var
  I: Integer;
  FP, FV: TextFile;
  Freq: Int64;
  OldDecSep: Char;
  maxlenp, maxlenv: Integer;
  p: PHashElement;
  function calcMaxLen(Section: Boolean): Integer;
  var
    i: Integer;
    p: PHashElement;
  begin
    Result := 0;
    for i := 0 to 255 do
    begin
      p := HashSecAdrTbl[i];
      while p <> nil do
      begin
        if (p.isSection = Section) and (length(p.rName) > Result) then
          Result := length(p.rName);
        p := p.rNext;
      end;
    end;
  end;

begin
{$IFDEF MINI_PROFILER_MULTITHREAD}
  ProfLock.Enter;
  try
{$ENDIF}
    OldDecSep := DecimalSeparator;
    DecimalSeparator := '.';

    AssignFile(FP, aProfileFileName);
    AssignFile(FV, aValuesFileName);
    try
      Rewrite(FP);
      Rewrite(FV);
      QueryPerformanceFrequency(Freq);
      maxlenp := calcMaxLen(True);
      maxlenv := calcMaxLen(False);
      writeln(FP, 'Name': maxlenp, '|',
        'Call Count|', 'Total time (s)': 15, '|', 'Average time (s)': 16,
        '|', 'Max time (s)': 16, '|', 'Min time (s)': 16);
      writeln(FV, 'Name': maxlenv, ' Value');

      for I := 0 to 255 do
      begin
        p := HashSecAdrTbl[i];
        while p <> nil do
        begin
          with p^ do
            if isSection then
            begin
              if (rCallCount <> 0) then
                writeln(FP, rName: maxlenp, ' ', rCallCount: 10,
                  ' ', rTotalTime /
                  freq: 15: PRECISION
                  , ' ',
                  rTotalTime / rCallCount / freq: 16: PRECISION
                  , ' ',
                  rMaxTime / freq: 16: PRECISION
                  , ' ',
                  rMinTime / freq: 16: PRECISION);
            end
            else if rDivide then
              writeln(FV, rName: maxlenv, ' ', rValue / rCount: 16: PRECISION)
            else
              writeln(FV, rName: maxlenv, ' ', rValue: 16: PRECISION);
          p := p.rNext;
        end;
      end;

    finally
      DecimalSeparator := OldDecSep;
      CloseFile(FP);
      CloseFile(FV);
    end;

{$IFDEF MINI_PROFILER_MULTITHREAD}
  finally
    ProfLock.Leave;
  end;
{$ENDIF}
end;

procedure DisposeHashAdrTbl;
var
  Tmp, Cur: PHashElement;
  I: Integer;
begin
{$IFDEF MINI_PROFILER_MULTITHREAD}
  ProfLock.Enter;
  try
{$ENDIF}
    for I := Low(HashSecAdrTbl) to High(HashSecAdrTbl) do
    begin
      Cur := HashSecAdrTbl[I];
      while Assigned(Cur) do
      begin
        Tmp := Cur.rNext;
        Dispose(Cur);
        Cur := Tmp;
      end;
    end;
{$IFDEF MINI_PROFILER_MULTITHREAD}
  finally
    ProfLock.Leave;
  end;
{$ENDIF}
end;

procedure DisposeHashThreadIDTbl;
var
  Tmp, Cur: PStackHashElement;
  I: Integer;

procedure PopStack(p:PStackElement);

var
  SE: PStackElement;
  EndTime: Int64;
  Diff: Int64;
begin
  QueryPerformanceCounter(EndTime);
  while Assigned(p) do
  begin
      with p.rSectionAddress^ do
      begin
        Diff := EndTime - p.rStartTime;
        Inc(rTotalTime, Diff);
        if rCallCount = 1 then
        begin
          rMinTime := Diff;
          rMaxTime := Diff;
        end
        else
        begin
          if rMaxTime < Diff then
            rMaxTime := Diff;
          if rMinTime > Diff then
            rMinTime := Diff;
        end;
      end;

//      LogWriteln(p.rSectionAddress.rName+' exited at finalization');
      SE := p.rPrior;
      Dispose(p);
      p := SE;
  end;
end;


begin
{$IFDEF MINI_PROFILER_MULTITHREAD}
  ProfLock.Enter;
  try
{$ENDIF}
    for I := Low(HashThreadIDTbl) to High(HashThreadIDTbl) do
    begin
      Cur := HashThreadIDTbl[I];
      while Assigned(Cur) do
      begin
        Tmp := Cur.next;

        PopStack(Cur.pStack);
        Dispose(Cur);


        Cur := Tmp;
      end;
    end;
{$IFDEF MINI_PROFILER_MULTITHREAD}
  finally
    ProfLock.Leave;
  end;
{$ENDIF}
end;


class procedure MiniProfiler.CalcAverage(const aName: string;
  aValue: Single);
var
  RetAddr: Pointer;
  HashElement: PHashElement;
begin
  // Получение адреса кода, следующего за вызовом этого метода
  asm
    MOV ECX, [EBP + 4]
    MOV RetAddr, ECX
  end;
{$IFDEF MINI_PROFILER_MULTITHREAD}
  ProfLock.Enter;
  try
{$ENDIF}
    HashElement := FindOrCreateSection(RetAddr, False);
    with HashElement^ do
    begin
      rName := aName;
      Inc(rCount);
      rDivide := True;
      if rCount = 1 then
        rValue := aValue
      else
        rValue := rValue + aValue;
    end;
{$IFDEF MINI_PROFILER_MULTITHREAD}
  finally
    ProfLock.Leave;
  end;
{$ENDIF}
end;

class procedure MiniProfiler.CalcMax(const aName: string; aValue: Single);
var
  RetAddr: Pointer;
  HashElement: PHashElement;
begin
  // Получение адреса кода, следующего за вызовом этого метода
  asm
    MOV ECX, [EBP + 4]
    MOV RetAddr, ECX
  end;
{$IFDEF MINI_PROFILER_MULTITHREAD}
  ProfLock.Enter;
  try
{$ENDIF}
    HashElement := FindOrCreateSection(RetAddr, False);
    with HashElement^ do
    begin
      rName := aName;
      Inc(rCount);
      rDivide := False;
      if rCount = 1 then
        rValue := aValue
      else if rValue < aValue then
        rValue := aValue;
    end;
{$IFDEF MINI_PROFILER_MULTITHREAD}
  finally
    ProfLock.Leave;
  end;
{$ENDIF}
end;

class procedure MiniProfiler.CalcMin(const aName: string; aValue: Single);
var
  RetAddr: Pointer;
  HashElement: PHashElement;
begin
  // Получение адреса кода, следующего за вызовом этого метода
  asm
    MOV ECX, [EBP + 4]
    MOV RetAddr, ECX
  end;
{$IFDEF MINI_PROFILER_MULTITHREAD}
  ProfLock.Enter;
  try
{$ENDIF}
    HashElement := FindOrCreateSection(RetAddr, False);
    with HashElement^ do
    begin
      rName := aName;
      Inc(rCount);
      rDivide := False;
      if rCount = 1 then
        rValue := aValue
      else if rValue > aValue then
        rValue := aValue;
    end;
{$IFDEF MINI_PROFILER_MULTITHREAD}
  finally
    ProfLock.Leave;
  end;
{$ENDIF}
end;

initialization
{$IFDEF MINI_PROFILER_MULTITHREAD}
  ProfLock := TCriticalSection.Create;
{$ENDIF}

finalization
{$IFDEF MINI_PROFILER_MULTITHREAD}
  ProfLock.Enter;
{$ENDIF}

//  while Assigned(SecBegStack) do
//    MiniProfiler.SectionEnd;

// убиваем все стеки

  DisposeHashThreadIDTbl;


  MiniProfiler.SaveToFile;
  DisposeHashAdrTbl;

{$IFDEF MINI_PROFILER_MULTITHREAD}
  ProfLock.Leave;
  ProfLock.Free;
{$ENDIF}

{$ENDIF}
end.

