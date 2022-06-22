unit XEventWatcher;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, x, xlib, Dialogs, syncobjs;

type
  TNotifyUpdate = procedure of object;

  TXEventWatcherThread = class(TThread)
  private
    fEvent: TEvent;
    fPaused: Boolean;
    procedure SetPaused(const Value: Boolean);
    procedure Update;
  protected
    procedure Execute; override;
  public
    Display: PDisplay;
    NotifyUpdate: TNotifyUpdate;
    constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt =
      DefaultStackSize);
    destructor Destroy; override;
    property Paused: Boolean read fPaused write SetPaused;
  end;

implementation

procedure TXEventWatcherThread.Update;
begin
  if Assigned(NotifyUpdate) then
    NotifyUpdate;
end;

procedure TXEventWatcherThread.SetPaused(const Value: Boolean);
begin
  if (not Terminated) and (fPaused <> Value) then
  begin
    fPaused := Value;
    if fPaused then
    begin
      fEvent.ResetEvent
    end
    else
    begin
      fEvent.SetEvent;
    end;
  end;
  Update;
end;

procedure TXEventWatcherThread.Execute;
var
  ev: TXEvent;
  root: TWindow;
  //NET_ACTIVE_WINDOW: TAtom;
  attr: TXSetWindowAttributes;
begin
  Display := XOpenDisplay(Pchar(GetEnvironmentVariable('DISPLAY')));
  root := RootWindow(Display, DefaultScreen(Display));
  //NET_ACTIVE_WINDOW := XInternAtom(Display, '_NET_ACTIVE_WINDOW', LongBool(1));
  attr.event_mask := StructureNotifyMask or SubstructureNotifyMask or ExposureMask;
  XChangeWindowAttributes(Display, root, CWEventMask, @attr);
  XNextEvent(Display, @ev);
  while not Terminated do
  begin
    fEvent.WaitFor(INFINITE);
    if XPending(Display) > 1 then
    begin
      { I have no idea about these codes, just copied from FPWM and it works }
      XNextEvent(Display, @ev);
    end
    else if XPending(Display) = 1 then
    begin
      Synchronize(@Update);
      // sometimes there's so much events
      Sleep(500);
      XNextEvent(Display, @ev);
    end
    else
    begin
      // if there's no sleep, the CPU usage going wild
      Sleep(200);
    end;
  end;
  if Assigned(Display) then XCloseDisplay(Display);
end;

constructor TXEventWatcherThread.Create(CreateSuspended: Boolean; const StackSize: SizeUInt =
  DefaultStackSize);
begin
  inherited Create(CreateSuspended, StackSize);
  fPaused := CreateSuspended;
  fEvent := TEvent.Create(nil, true, CreateSuspended, '');
  FreeOnTerminate := True;
end;

destructor TXEventWatcherThread.Destroy;
begin
  inherited Destroy;
end;

end.

