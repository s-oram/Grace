unit duck.vcl;

// *****************************************************************************
//      DUCK DUCK DELPHI - duck.vcl
// *****************************************************************************
//
//  Original Author:
//    Jason Southwell
//
//  Description:
//    This unit extends the duck unit with VCL specific extensions.
//
//  For more information on Duck Typing, please check out this article on
//  wikipedia:
//    http://en.wikipedia.org/wiki/Duck_typing
//
//  For more information on this project, please visit:
//    http:/arcana.sivv.com/duckduckdelphi
//
// *****************************************************************************
//      LICENSE
// *****************************************************************************
//
//  The MIT License (MIT)
//  Copyright (c) 2012 by Sivv LLC, All Rights Reseved
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to
//  deal in the Software without restriction, including without limitation the
//  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
//  sell copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in
//  all copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
//  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
//  IN THE SOFTWARE.
//
// *****************************************************************************
//      RELEASE NOTES
// *****************************************************************************
//   2012-04-10 : R3 Jason Southwell
//                 + Extended TEvent with helper functions for VCL specific events
// *****************************************************************************

interface

uses System.SysUtils, System.Classes, System.Types, duck, VCL.Controls;

type
  TMouseReference = reference to procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  TMouseMoveReference = reference to procedure(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  TMouseActivateReference = reference to procedure(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer; HitTest: Integer; var MouseActivate: TMouseActivate);
  TKeyReference = reference to procedure(Sender: TObject; var Key: Word; Shift: TShiftState);
  TKeyPressReference = reference to procedure(Sender: TObject; var Key: Char) ;
  TDragOverReference = reference to procedure(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean) ;
  TDragDropReference = reference to procedure(Sender, Source: TObject; X, Y: Integer) ;
  TStartDragReference = reference to procedure(Sender: TObject; var DragObject: TDragObject) ;
  TEndDragReference = reference to procedure(Sender, Target: TObject; X, Y: Integer) ;
  TDockDropReference = reference to procedure(Sender: TObject; Source: TDragDockObject; X, Y: Integer) ;
  TDockOverReference = reference to procedure(Sender: TObject; Source: TDragDockObject; X, Y: Integer; State: TDragState; var Accept: Boolean) ;
  TUnDockReference = reference to procedure(Sender: TObject; Client: TControl; NewTarget: TWinControl; var Allow: Boolean) ;
  TStartDockReference = reference to procedure(Sender: TObject; var DragObject: TDragDockObject) ;
  TGetSiteInfoReference = reference to procedure(Sender: TObject; DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean) ;
  TCanResizeReference = reference to procedure(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean) ;
  TConstrainedResizeReference = reference to procedure(Sender: TObject; var MinWidth, MinHeight, MaxWidth, MaxHeight: Integer) ;
  TMouseWheelReference = reference to procedure(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean) ;
  TMouseWheelUpDownReference = reference to procedure(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean) ;
  TContextPopupReference = reference to procedure(Sender: TObject; MousePos: TPoint; var Handled: Boolean) ;	
	
  TEventVCLHelper = class helper for TEvent
  private
  protected
  public
    function Mouse(proc : TMouseReference) : TMouseEvent; virtual;
    procedure MouseRelease(event : TMouseEvent); virtual;

    function MouseMove(proc : TMouseMoveReference) : TMouseMoveEvent; virtual;
    procedure MouseMoveRelease(event : TMouseMoveEvent); virtual;

    function MouseActivate(proc : TMouseActivateReference) : TMouseActivateEvent; virtual;
    procedure MouseActivateRelease(event : TMouseActivateEvent); virtual;

    function Key(proc : TKeyReference) : TKeyEvent; virtual;
    procedure KeyRelease(event : TKeyEvent); virtual;

    function KeyPress(proc : TKeyPressReference) : TKeyPressEvent; virtual;
    procedure KeyPressRelease(event : TKeyPressEvent); virtual;

    function DragOver(proc : TDragOverReference) : TDragOverEvent; virtual;
    procedure DragOverRelease(event : TDragOverEvent); virtual;

    function DragDrop(proc : TDragDropReference) : TDragDropEvent; virtual;
    procedure DragDropRelease(event : TDragDropEvent); virtual;

    function StartDrag(proc : TStartDragReference) : TStartDragEvent; virtual;
    procedure StartDragRelease(event : TStartDragEvent); virtual;

    function EndDrag(proc : TEndDragReference) : TEndDragEvent; virtual;
    procedure EndDragRelease(event : TEndDragEvent); virtual;

    function DockDrop(proc : TDockDropReference) : TDockDropEvent; virtual;
    procedure DockDropRelease(event : TDockDropEvent); virtual;

    function DockOver(proc : TDockOverReference) : TDockOverEvent; virtual;
    procedure DockOverRelease(event : TDockOverEvent); virtual;

    function StartDock(proc : TStartDockReference) : TStartDockEvent; virtual;
    procedure StartDockRelease(event : TStartDockEvent); virtual;

    function GetSiteInfo(proc : TGetSiteInfoReference) : TGetSiteInfoEvent; virtual;
    procedure GetSiteInfoRelease(event : TGetSiteInfoEvent); virtual;

    function CanResize(proc : TCanResizeReference) : TCanResizeEvent; virtual;
    procedure CanResizeRelease(event : TCanResizeEvent); virtual;

    function ConstrainedResize(proc : TConstrainedResizeReference) : TConstrainedResizeEvent; virtual;
    procedure ConstrainedResizeRelease(event : TConstrainedResizeEvent); virtual;

    function MouseWheel(proc : TMouseWheelReference) : TMouseWheelEvent; virtual;
    procedure MouseWheelRelease(event : TMouseWheelEvent); virtual;

    function MouseWheelUpDown(proc : TMouseWheelUpDownReference) : TMouseWheelUpDownEvent; virtual;
    procedure MouseWheelUpDownRelease(event : TMouseWheelUpDownEvent); virtual;

    function ContextPopup(proc : TContextPopupReference) : TContextPopupEvent; virtual;
    procedure ContextPopupRelease(event : TContextPopupEvent); virtual;
  end;


implementation


{ TEventVCLHelper }

function TEventVCLHelper.CanResize(proc: TCanResizeReference): TCanResizeEvent;
begin
  Result := From<TCanResizeEvent, TCanResizeReference>(proc);
end;

procedure TEventVCLHelper.CanResizeRelease(event: TCanResizeEvent);
begin
  Release<TCanResizeEvent>(event);
end;

function TEventVCLHelper.ConstrainedResize(
  proc: TConstrainedResizeReference): TConstrainedResizeEvent;
begin
  Result := From<TConstrainedResizeEvent, TConstrainedResizeReference>(proc);
end;

procedure TEventVCLHelper.ConstrainedResizeRelease(
  event: TConstrainedResizeEvent);
begin
  Release<TConstrainedResizeEvent>(event);
end;

function TEventVCLHelper.ContextPopup(
  proc: TContextPopupReference): TContextPopupEvent;
begin
  Result := From<TContextPopupEvent, TContextPopupReference>(proc);
end;

procedure TEventVCLHelper.ContextPopupRelease(event: TContextPopupEvent);
begin
  Release<TContextPopupEvent>(event);
end;

function TEventVCLHelper.DockDrop(proc: TDockDropReference): TDockDropEvent;
begin
  Result := From<TDockDropEvent, TDockDropReference>(proc);
end;

procedure TEventVCLHelper.DockDropRelease(event: TDockDropEvent);
begin
  Release<TDockDropEvent>(event);
end;

function TEventVCLHelper.DockOver(proc: TDockOverReference): TDockOverEvent;
begin
  Result := From<TDockOverEvent, TDockOverReference>(proc);
end;

procedure TEventVCLHelper.DockOverRelease(event: TDockOverEvent);
begin
  Release<TDockOverEvent>(event);
end;

function TEventVCLHelper.DragDrop(proc: TDragDropReference): TDragDropEvent;
begin
  Result := From<TDragDropEvent, TDragDropReference>(proc);
end;

procedure TEventVCLHelper.DragDropRelease(event: TDragDropEvent);
begin
  Release<TDragDropEvent>(event);
end;

function TEventVCLHelper.DragOver(proc: TDragOverReference): TDragOverEvent;
begin
  Result := From<TDragOverEvent, TDragOverReference>(proc);
end;

procedure TEventVCLHelper.DragOverRelease(event: TDragOverEvent);
begin
  Release<TDragOverEvent>(event);
end;

function TEventVCLHelper.EndDrag(proc: TEndDragReference): TEndDragEvent;
begin
  Result := From<TEndDragEvent, TEndDragReference>(proc);
end;

procedure TEventVCLHelper.EndDragRelease(event: TEndDragEvent);
begin
  Release<TEndDragEvent>(event);
end;

function TEventVCLHelper.GetSiteInfo(
  proc: TGetSiteInfoReference): TGetSiteInfoEvent;
begin
  Result := From<TGetSiteInfoEvent, TGetSiteInfoReference>(proc);
end;

procedure TEventVCLHelper.GetSiteInfoRelease(event: TGetSiteInfoEvent);
begin
  Release<TGetSiteInfoEvent>(event);
end;

function TEventVCLHelper.Key(proc: TKeyReference): TKeyEvent;
begin
  Result := From<TKeyEvent, TKeyReference>(proc);
end;

function TEventVCLHelper.KeyPress(proc: TKeyPressReference): TKeyPressEvent;
begin
  Result := From<TKeyPressEvent, TKeyPressReference>(proc);
end;

procedure TEventVCLHelper.KeyPressRelease(event: TKeyPressEvent);
begin
  Release<TKeyPressEvent>(event);
end;

procedure TEventVCLHelper.KeyRelease(event: TKeyEvent);
begin
  Release<TKeyEvent>(event);
end;

function TEventVCLHelper.Mouse(proc: TMouseReference): TMouseEvent;
begin
  Result := From<TMouseEvent, TMouseReference>(proc);
end;

function TEventVCLHelper.MouseActivate(
  proc: TMouseActivateReference): TMouseActivateEvent;
begin
  Result := From<TMouseActivateEvent, TMouseActivateReference>(proc);
end;

procedure TEventVCLHelper.MouseActivateRelease(event: TMouseActivateEvent);
begin
  Release<TMouseActivateEvent>(event);
end;

function TEventVCLHelper.MouseMove(proc: TMouseMoveReference): TMouseMoveEvent;
begin
  Result := From<TMouseMoveEvent, TMouseMoveReference>(proc);
end;

procedure TEventVCLHelper.MouseMoveRelease(event: TMouseMoveEvent);
begin
  Release<TMouseMoveEvent>(event);
end;

procedure TEventVCLHelper.MouseRelease(event: TMouseEvent);
begin
  Release<TMouseEvent>(event);
end;

function TEventVCLHelper.MouseWheel(
  proc: TMouseWheelReference): TMouseWheelEvent;
begin
  Result := From<TMouseWheelEvent, TMouseWheelReference>(proc);
end;

procedure TEventVCLHelper.MouseWheelRelease(event: TMouseWheelEvent);
begin
  Release<TMouseWheelEvent>(event);
end;

function TEventVCLHelper.MouseWheelUpDown(
  proc: TMouseWheelUpDownReference): TMouseWheelUpDownEvent;
begin
  Result := From<TMouseWheelUpDownEvent, TMouseWheelUpDownReference>(proc);
end;

procedure TEventVCLHelper.MouseWheelUpDownRelease(
  event: TMouseWheelUpDownEvent);
begin
  Release<TMouseWheelUpDownEvent>(event);
end;

function TEventVCLHelper.StartDock(proc: TStartDockReference): TStartDockEvent;
begin
  Result := From<TStartDockEvent, TStartDockReference>(proc);
end;

procedure TEventVCLHelper.StartDockRelease(event: TStartDockEvent);
begin
  Release<TStartDockEvent>(event);
end;

function TEventVCLHelper.StartDrag(proc: TStartDragReference): TStartDragEvent;
begin
  Result := From<TStartDragEvent, TStartDragReference>(proc);
end;

procedure TEventVCLHelper.StartDragRelease(event: TStartDragEvent);
begin
  Release<TStartDragEvent>(event);
end;

end.
