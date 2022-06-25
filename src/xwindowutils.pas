unit XWindowUtils;

{$MODE objfpc}{$H+}

interface

uses
  Classes, SysUtils, x, xlib, xutil, unixtype, fgl, xatom, Dialogs, Forms;

type

  TCardinalList = specialize TFPGList<Cardinal>;
  TWindowProc = procedure(Win: TWindow) of object;

{ TXWindowList }

  TXWindowManager = class
  private
    fList: TCardinalList;
    fDisplay: PDisplay;
    fNet_Wm_Pid: TAtom;
    fWm_Client_machine: TAtom;
    procedure EnumerateTopLevelWindows(Proc: TWindowProc; Display: PDisplay);
    function GetWindowList: TCardinalList;
    procedure XWindowProc(Win: TWindow);
  public
    property Display: PDisplay read fDisplay;
    constructor Create;
    destructor Destroy; override;
    procedure UpdateWindowList;
    property WindowList: TCardinalList read GetWindowList;
    function GetWindowName(Window: TWindow): string;
    function GetWindowPID(Window: TWindow): Cardinal;
    function GetWindowState(Window: TWindow): string;
    function GetWindowCmd(PID: Cardinal): string;
    function GetWindowHost(Window: TWindow): string;
    function GetWindowRect(Window: TWindow): TRect;
    procedure ActivateWindow(Window: TWindow);
    procedure MaximizeWindow(Window: TWindow);
    procedure SetDesktopMode(Window: TWindow);
    procedure SetDockedMode(Window: TWindow);
    procedure SetStrut(Window: TWindow; W, H: integer; StrutPos: integer);
    procedure CloseWindow(Window: TWindow);
    procedure OverrideRedirect(Window: TWindow);
    procedure SetIconGeometry(Window: TWindow; T, L, W, H: integer);
  end;

  TWindowArray = array[0..MaxListSize] of TWindow;
  PWindowArray = ^TWindowArray;

function GetProp(Dpy: PDisplay; Win: TWindow; Prop: TAtom; out Value: Cardinal; Offset: Cardinal = 0): Boolean; overload;
function GetProp(Dpy: PDisplay; Win: TWindow; Prop: TAtom; out Value: string; Offset: Cardinal = 0): Boolean; overload;
function GetState(Dpy: PDisplay; Win: TWindow): Cardinal;
function GetDesktop(Dpy: PDisplay; Win: TWindow): Cardinal;
function SetDesktop(Dpy: PDisplay; Desktop: Cardinal): Boolean;
function GetSkipTaskbar(Dpy: PDisplay; Win: TWindow): string;

threadvar GotBadwindow: boolean;
threadvar OldHandler: TXErrorHandler;


implementation

function GetProp(Dpy: PDisplay; Win: TWindow; Prop: TAtom; out Value: Cardinal; Offset: Cardinal): Boolean;
var
  ActualTypeReturn: TAtom;
  ActualFormatReturn: LongInt;
  NItemsReturn, BytesAfterReturn: Cardinal;
  Ptr: PByte;
begin
  Result := XGetWindowProperty(Dpy, Win, Prop, Offset, 1, LongBool(0), AnyPropertyType, @ActualTypeReturn, @ActualFormatReturn, @NItemsReturn, @BytesAfterReturn, @Ptr) = Success;
  if Result then
  try
    {$message warn: ActualFormatReturn always 0 on 64bit}
    if (ActualTypeReturn = None) {or (ActualFormatReturn <> 32) }or not Assigned(Ptr) then
    begin
      Result := False;
    end;
    if Result then Value := PCardinal(Ptr)^;
  finally
    if Assigned(Ptr) then XFree(Ptr);
  end;
end;

function GetProp(Dpy: PDisplay; Win: TWindow; Prop: TAtom; out Value: string; Offset: Cardinal = 0): Boolean;
var
  ActualTypeReturn: TAtom;
  ActualFormatReturn: LongInt;
  Length, NItemsReturn, BytesAfterReturn: Cardinal;
  Ptr: PByte;
begin
  Result := XGetWindowProperty(Dpy, Win, Prop, Offset, 0, LongBool(0), None, @ActualTypeReturn, @ActualFormatReturn, @NItemsReturn, @BytesAfterReturn, @Ptr) = Success;
  if Result and Assigned(Ptr) then
    XFree(Ptr);
  if (ActualTypeReturn = None) or (ActualFormatReturn <> 8) then Result := False;
  if Result then
  try
    Length := (BytesAfterReturn + 3) div 4;
    Result := XGetWindowProperty(Dpy, Win, Prop, Offset, Length, LongBool(0), ActualTypeReturn, @ActualTypeReturn, @ActualFormatReturn, @NItemsReturn, @BytesAfterReturn, @Ptr) = Success;
    if (ActualTypeReturn = None) or (ActualFormatReturn <> 8) or not Assigned(Ptr) then
      Result := False;
    if Result then
      SetString(Value, PChar(Ptr), NItemsReturn);
  finally
    if Assigned(Ptr) then
      XFree(Ptr);
  end;
end;



function GetState(Dpy: PDisplay; Win: TWindow): Cardinal;
var
  A: TAtom;
begin
  A := XInternAtom(Dpy, 'WM_STATE', LongBool(1));
  if A = None then
    Result := 0
  else if not GetProp(Dpy, Win, A, Result) then
    Result := 0;
end;

{
this block is a piece of useless junk, XLib documentation is confusing af,
focusing on this function is totally waste of a time. Just focus other features.
}
function GetSkipTaskbar(Dpy: PDisplay; Win: TWindow): string;
var
  atoms: PAtom;
  numberatom: Cardinal;
  atomname: string;
  i, j: integer;
  ActualTypeReturn: TAtom;
  ActualFormatReturn: LongInt;
  NItemsReturn, BytesAfterReturn: Cardinal;
  Ptr: PByte;
begin
  Result := '';
  if XGetWindowProperty(Dpy, Win, XInternAtom(Dpy, '_NET_WM_STATE', LongBool(0)), 0, 10, LongBool(0), AnyPropertyType,
        @ActualTypeReturn, @ActualFormatReturn, @NItemsReturn, @BytesAfterReturn, @Ptr) = Success then
  begin
    for i := 0 to Length(TBytes(Ptr)) -1 do
    begin
      //if Ptr[i] = XInternAtom(Dpy, '_NET_WM_STATE_SKIP_TASKBAR', LongBool(0)) then
      { this doesn't returned _NET_WM_STATE related atoms but a piece of random crap }
        Result += XGetAtomName(Dpy ,Ptr[i]) + ';';
    end;
  end;
  //Atoms := XListProperties(Dpy, Win, @numberatom);
  //for i := 0 to numberatom -1 do
  //begin
  //  atomname := XGetAtomName(Dpy, atoms[i]);
  //  if atomname.Equals('_NET_WM_STATE') then
  //  begin
  //    if XGetWindowProperty(Dpy, Win, atoms[i], 0, 10, LongBool(0), AnyPropertyType,
  //      @ActualTypeReturn, @ActualFormatReturn, @NItemsReturn, @BytesAfterReturn, @Ptr) = Success then
  //    begin
  //      for j := 0 to Length(TBytes(Ptr)) -1 do
  //      begin
  //        if atoms[i] = Ptr[j] then
  //          Result += XGetAtomName(Dpy, Ptr[j]) + ';';
  //      end;
  //      //if XGetAtomName(Dpy, ActualTypeReturn) = atomname then
  //        //Result += NItemsReturn.ToString + '/' + atomname + ';';
  //      if Assigned(Ptr) then
  //        XFree(Ptr);
  //    end;
  //  end;
  //end;

end;

function GetDesktop(Dpy: PDisplay; Win: TWindow): Cardinal;
var
  A: TAtom;
begin
  A := XInternAtom(Dpy, '_NET_WM_DESKTOP', LongBool(1));
  if A = None then
    Result := $FFFFFFFF
  else if not GetProp(Dpy, Win, A, Result) then
    Result := $FFFFFFFF;
end;

function SetDesktop(Dpy: PDisplay; Desktop: Cardinal): Boolean;
var
  Net_Current_Desktop: TAtom;
  Root: TWindow;
  CurrentDesktop: Cardinal;
  Event: TXEvent;
begin
  Net_Current_Desktop := XInternAtom(Dpy, '_NET_CURRENT_DESKTOP', LongBool(1));
  if Net_Current_Desktop = None then
    Result := False
  else
  begin
    Root := XDefaultRootWindow(Dpy);
    if not GetProp(Dpy, Root, Net_Current_Desktop, CurrentDesktop) then
      Result := False
    else
    begin
      Event.xclient._type := ClientMessage;
      Event.xclient.send_event := Tbool(true);
      Event.xclient.window := Root;
      Event.xclient.display := Dpy;
      Event.xclient.format := 32;
      Event.xclient.message_type := Net_Current_Desktop;
      Event.xclient.data.l[0] := Desktop;
      Result := XSendEvent(Dpy, Root, LongBool(0), SubstructureNotifyMask or SubstructureRedirectMask, @Event) = Success;
    end;
  end;
end;


function BadwindowHandler(para1: PDisplay; para2: PXErrorEvent): cint; cdecl;
begin
  if para2^.error_code <> BadWindow then
    Result := OldHandler(para1, para2) else
  begin
    GotBadwindow := true;
    Result := 0;
  end;
end;

procedure TXWindowManager.EnumerateTopLevelWindows(Proc: TWindowProc; Display: PDisplay);

  procedure QueryChildren(Dpy: PDisplay; Win: TWindow);
  var
    Root, Parent: TWindow;
    Children: PWindowArray;
    I, Count: Cardinal;
  begin
    if XQueryTree(Dpy, Win, @Root, @Parent, PPWindow(@Children), @Count) = 0 then Exit;
    if Count = 0 then Exit;
    try
      for I := 0 to Count - 1 do
      begin
        GotBadwindow := false;
        QueryChildren(Dpy, Children^[I]);
        XSync(dpy, False);
        if (not GotBadwindow) and (GetState(Dpy, Children^[I]) <> 0) then
        begin
          Proc(Children^[I]);
        end;
      end;
    finally
      XFree(Children);
    end;
  end;

var
  Root: TWindow;

begin
  try
    Root := XRootWindow(Display, XDefaultScreen(Display));
    OldHandler := XSetErrorHandler(@BadwindowHandler);
    QueryChildren(Display, Root);
  finally
    XSetErrorHandler(OldHandler);
  end;
end;


{ TXWindowManager }

function TXWindowManager.GetWindowList: TCardinalList;
begin
  Result := fList;
end;

procedure TXWindowManager.XWindowProc(Win: TWindow);
begin
  fList.Add(Win);
end;

constructor TXWindowManager.Create;
begin
  fDisplay := XOpenDisplay(Pchar(GetEnvironmentVariable('DISPLAY')));
  if not Assigned(fDisplay) then raise Exception.Create('Can not open display');
  fList := TCardinalList.create;
  fWm_Client_machine := XInternAtom(fDisplay, 'WM_CLIENT_MACHINE', LongBool(1));
  fNet_Wm_Pid := XInternAtom(fDisplay, '_NET_WM_PID', LongBool(1));
end;

destructor TXWindowManager.Destroy;
begin
  fList.free;
  if Assigned(fDisplay) then XCloseDisplay(fDisplay);
  inherited Destroy;
end;

procedure TXWindowManager.UpdateWindowList;
begin
  fList.Clear;
  EnumerateTopLevelWindows(@XWindowProc, fDisplay);
end;

function TXWindowManager.GetWindowName(Window: TWindow): string;
var Stat: XLib.TStatus;
  text_prop: TXTextProperty;
  List: PPChar;
  Num: integer;
begin
  Result := '';
  OldHandler := XSetErrorHandler(@BadwindowHandler);
  GotBadwindow := false;
  stat := XGetWMName(fDisplay, Window, @text_prop);
  XSync(fDisplay, False);
  if GotBadwindow then begin XSetErrorHandler(OldHandler); exit; end;
  if ((stat = 0) or (text_prop.value = nil) or (text_prop.nitems = 0)) then begin XSetErrorHandler(OldHandler); exit; end;
  stat := XmbTextPropertyToTextList(fDisplay, @text_prop, @List, @Num);
  if ((stat < Success) or (num = 0) or (list = nil)) then begin XSetErrorHandler(OldHandler); exit; end else
  begin
    XFree(text_prop.value);
    if Num > 0 then Result := List^;
    XFreeStringList(list);
  end;
  XSetErrorHandler(OldHandler);
end;

function TXWindowManager.GetWindowPID(Window: TWindow): Cardinal;
var Pid: Cardinal;
begin
  Result := 0;
  OldHandler := XSetErrorHandler(@BadwindowHandler);
  GotBadwindow := false;
  if GetProp(fDisplay, Window, fNet_Wm_Pid, Pid) then
    if not GotBadwindow then Result := Pid;
  XSetErrorHandler(OldHandler);
end;

function StateName(State: Cardinal): string;
begin
  case State of
    IconicState: Result := 'Iconic';
    NormalState: Result := 'Normal';
    WithdrawnState: Result := 'Withdrawn';
  else Result := IntToStr(State);
  end;
end;


function TXWindowManager.GetWindowState(Window: TWindow): string;
var stat: Cardinal;
begin
  Result := '';
  OldHandler := XSetErrorHandler(@BadwindowHandler);
  GotBadwindow := false;
  Stat := GetState(fDisplay, Window);
  XSync(fDisplay, False);
  if not GotBadwindow then Result := StateName(Stat);
  XSetErrorHandler(OldHandler);
end;

function TXWindowManager.GetWindowCmd(PID: Cardinal): string;
var sl: TStringList;
begin
  Result := '';
  sl := TStringList.Create;
  try
    if FileExists('/proc/' + IntToStr(Pid) + '/cmdline') then
    begin
      sl.LoadFromFile('/proc/' + IntToStr(Pid) + '/cmdline');
      if sl.Count > 0 then
      begin
        sl.Text := StringReplace(sl.Text, #0, LineEnding, [rfReplaceAll]);
        Result := sl[0];
      end;
    end;
  finally
    sl.free;
  end;
end;

function TXWindowManager.GetWindowHost(Window: TWindow): string;
var Host: string;
begin
  Result := '';
  OldHandler := XSetErrorHandler(@BadwindowHandler);
  GotBadwindow := false;
  if GetProp(fDisplay, Window, fWm_Client_Machine, Host) then
    if not GotBadwindow then Result := Host;
  XSetErrorHandler(OldHandler);
end;

function TXWindowManager.GetWindowRect(Window: TWindow): TRect;
var Stat: TStatus;
  Attrs: TXWindowAttributes;
  rx, ry: cint;
  junk: TWindow;
begin
  Result:=Rect(0,0,0,0);
  OldHandler := XSetErrorHandler(@BadwindowHandler);
  GotBadwindow := false;
  Stat := XGetWindowAttributes(fDisplay, Window, @Attrs);
  XSync(fDisplay, False);
  if (not GotBadwindow) and (Stat <> 0) then
  begin
    XTranslateCoordinates(fDisplay, Window, Attrs.root,
      -Attrs.border_width,
      -Attrs.border_width,
      @rx, @ry, @junk);
    Result := Rect(rx, ry, rx + Attrs.width, ry + Attrs.height);
  end;
  XSetErrorHandler(OldHandler);
end;

procedure TXWindowManager.ActivateWindow(Window: TWindow);
var
  Desktop: Cardinal;
  atom: TAtom;
  xev: TXEvent;
  attr: TXWindowAttributes;
begin
  Desktop := GetDesktop(fDisplay, Window);
  if Desktop <> $FFFFFFFF then
  begin
    SetDesktop(fDisplay, Desktop);
    XGetWindowAttributes(fDisplay, Window, @attr);

    if (attr.map_state = IsViewable) or ((attr.map_state = IsUnmapped) and (Getstate(fDisplay, Window) = IconicState)) then
    begin
      XMapWindow(fDisplay, Window);
      XFlush(fDisplay);
      XSync(fDisplay, False);
    end;

    XCirculateSubwindowsUp(fDisplay, Window);
    XRaiseWindow(fDisplay, Window);
    XMapWindow(fDisplay, Window);
    XSetInputFocus(fDisplay, Window, RevertToParent, CurrentTime);

    atom := XInternAtom(fDisplay, '_NET_ACTIVE_WINDOW', False);


    xev.xclient._type := ClientMessage;
    xev.xclient.serial := 0;
    xev.xclient.send_event := 1;
    xev.xclient.display := fDisplay;
    xev.xclient.window := Window;
    xev.xclient.message_type := atom;
    xev.xclient.format := 32;
    xev.xclient.data.l[0] := 2;
    xev.xclient.data.l[1] := 0;
    xev.xclient.data.l[2] := 0;
    xev.xclient.data.l[3] := 0;
    xev.xclient.data.l[4] := 0;

    XSendEvent(fDisplay,
      attr.root, False,
      SubstructureRedirectMask or SubstructureNotifyMask,
      @xev);

    XFlush(fDisplay);
    XSync(fDisplay, False);
  end;
end;

procedure TXWindowManager.CloseWindow(Window: TWindow);
var
  Desktop: Cardinal;
  netclose: TAtom;
  xev: TXEvent;
  attr: TXWindowAttributes;
begin
  Desktop := GetDesktop(fDisplay, Window);
  if Desktop <> $FFFFFFFF then
  begin
    SetDesktop(fDisplay, Desktop);
    XGetWindowAttributes(fDisplay, Window, @attr);

    if (attr.map_state = IsViewable) or ((attr.map_state = IsUnmapped) and (Getstate(fDisplay, Window) = IconicState)) then
    begin
      XMapWindow(fDisplay, Window);
      XFlush(fDisplay);
      XSync(fDisplay, False);
    end;

    netclose := XInternAtom(display, '_NET_CLOSE_WINDOW', False);

    xev.xclient._type := ClientMessage;
    xev.xclient.serial := 0;
    xev.xclient.send_event := 1;
    xev.xclient.display := fDisplay;
    xev.xclient.window := Window;
    xev.xclient.message_type := netclose;
    xev.xclient.format := 32;


    XSendEvent(fDisplay,
      attr.root, False,
      SubstructureRedirectMask or SubstructureNotifyMask,
      @xev);

    XFlush(fDisplay);
    XSync(fDisplay, False);
  end;
  ActivateWindow(Window);
end;

procedure TXWindowManager.MaximizeWindow(Window: TWindow);
var
  Desktop: Cardinal;
  wmstate, wmmaxhorz, wmmaxvert: TAtom;
  xev: TXEvent;
  attr: TXWindowAttributes;
begin
  Desktop := GetDesktop(fDisplay, Window);
  if Desktop <> $FFFFFFFF then
  begin
    SetDesktop(fDisplay, Desktop);
    XGetWindowAttributes(fDisplay, Window, @attr);

    if (attr.map_state = IsViewable) or ((attr.map_state = IsUnmapped) and (Getstate(fDisplay, Window) = IconicState)) then
    begin
      XMapWindow(fDisplay, Window);
      XFlush(fDisplay);
      XSync(fDisplay, False);
    end;

    wmstate := XInternAtom(display, '_NET_WM_STATE', False);
    wmmaxhorz := XInternAtom(display, '_NET_WM_STATE_MAXIMIZED_HORZ', False);
    wmmaxvert := XInternAtom(display, '_NET_WM_STATE_MAXIMIZED_VERT', False);

    xev.xclient._type := ClientMessage;
    xev.xclient.serial := 0;
    xev.xclient.send_event := 1;
    xev.xclient.display := fDisplay;
    xev.xclient.window := Window;
    xev.xclient.message_type := wmstate;
    xev.xclient.format := 32;
    xev.xclient.data.l[0] := 2;
    xev.xclient.data.l[1] := wmmaxhorz;
    xev.xclient.data.l[2] := wmmaxvert;
    xev.xclient.data.l[3] := 1;


    XSendEvent(fDisplay,
      attr.root, False,
      SubstructureRedirectMask or SubstructureNotifyMask,
      @xev);

    XFlush(fDisplay);
    XSync(fDisplay, False);
  end;
  ActivateWindow(Window);
end;

procedure TXWindowManager.OverrideRedirect(Window: TWindow);
var
  Desktop: Cardinal;
  wmstate, wmmaxhorz, wmmaxvert: TAtom;
  xev: TXEvent;
  attr: TXSetWindowAttributes;
  prop: culong;
begin
  Desktop := GetDesktop(fDisplay, Window);


    attr.override_redirect := cint(1);

    XChangeWindowAttributes(fDisplay, Window, CWOverrideRedirect, @attr);
    XUnmapWindow(fDisplay, Window);
    XMapWindow(fDisplay, Window);

end;

procedure TXWindowManager.SetStrut(Window: TWindow; W, H: integer; StrutPos: integer);
var
  wmstrut, wmstrutpartial: TAtom;
  prop: culong;
  propsets: array[0..11] of integer = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
begin



  (*
  int insets[12] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
  insets[2] = height;
  insets[8] = 0;
  insets[9] = width;

  *)

  {

  XChangeProperty(display,
                winId(),
                XInternAtom(QX11Info::display(), "_NET_WM_STRUT", False),
                XA_CARDINAL ,
                32,
                PropModeReplace,
                (unsigned char *)&insets, 4);

   XChangeProperty(display,
                 winId(),
                XInternAtom(QX11Info::display(), "_NET_WM_STRUT_PARTIAL", False),
                XA_CARDINAL ,
                32,
                PropModeReplace,
                (unsigned char *)&insets, 12);

  }
  // left 0, right 1, top 2, bottom 3, left_start_y 4, left_end_y 5,
  // right_start_y 6, right_end_y 7, top_start_x 8, top_end_x 9, bottom_start_x 10
  //propsets[2] := H;
  if StrutPos = 0 then
  begin
    propsets[4] := H;
    propsets[9] := W;
  end
  else if StrutPos = 1 then
  begin
    propsets[6] := h;
    propsets[9] := W;
  end;

  wmstrut := XInternAtom(display, '_NET_WM_STRUT', False);
  wmstrutpartial := XInternAtom(display, '_NET_WM_STRUT_PARTIAL', False);

  XChangeProperty(Display, Window, wmstrut, XA_CARDINAL,
    32, PropModeReplace, @propsets, 4);
  XMapWindow(fDisplay, Window);

  XChangeProperty(Display, Window, wmstrutpartial, XA_CARDINAL,
    32, PropModeReplace, @propsets, 12);
  XMapWindow(fDisplay, Window);
end;

procedure TXWindowManager.SetDesktopMode(Window: TWindow);
var
  wmtype, wmdesktop, wmtdesktop, wmstate, wmbelow, wmdock: TAtom;
  prop: culong;
  propsets: array[0..0] of integer = (0);
begin
  wmtype := XInternAtom(display, '_NET_WM_WINDOW_TYPE', False);
  wmstate := XInternAtom(display, '_NET_WM_STATE', False);
  wmbelow := XInternAtom(display, '_NET_WM_STATE_BELOW', False);
  wmdock := XInternAtom(display, '_NET_WM_WINDOW_TYPE_DOCK', False);
  wmtdesktop := XInternAtom(display, '_NET_WM_WINDOW_TYPE_DESKTOP', False);
  wmdesktop := XInternAtom(display, '_NET_WM_DESKTOP', False);

  prop := $FFFFFFFF;
  propsets[0] := wmtdesktop;

  //XChangeProperty(fDisplay, Window, wmdesktop, XA_CARDINAL,
  //  32, PropModeReplace, @prop, 1);
  //XMapWindow(fDisplay, Window);

  XChangeProperty(fDisplay, Window, wmtype, XA_ATOM,
    32, PropModeReplace, @propsets, 1);
  XFlush(Display);
  //XMapWindow(fDisplay, Window);

  //XChangeProperty(fDisplay, Window, wmstate, XA_ATOM,
  //  32, PropModeAppend, @wmbelow, 1);
  //XMapWindow(fDisplay, Window);
end;

procedure TXWindowManager.SetDockedMode(Window: TWindow);
var
  wmtype, wmdesktop, wmdock: TAtom;
  prop: culong;
begin
  wmtype := XInternAtom(display, '_NET_WM_WINDOW_TYPE', False);
  wmdock := XInternAtom(display, '_NET_WM_WINDOW_TYPE_DOCK', False);
  wmdesktop := XInternAtom(display, '_NET_WM_DESKTOP', False);

  prop := $FFFFFFFF;

  //XChangeProperty(fDisplay, Window, wmdesktop, XA_CARDINAL,
  //  32, PropModeReplace, @prop, 1);
  //XMapWindow(fDisplay, Window);
  //
  XChangeProperty(fDisplay, Window, wmtype, XA_ATOM,
    32, PropModeReplace, @wmdock, 1);
  XMapWindow(fDisplay, Window);


end;

procedure TXWindowManager.SetIconGeometry(Window: TWindow; T, L, W, H: integer);
var
  wmgeo: TAtom;
  prop: culong;
  propsets: array[0..3] of integer = (0, 0, 0, 0);
begin
  // x, y, width, height
  propsets[0] := l;
  propsets[1] := t;
  propsets[2] := w;
  propsets[3] := h;

  wmgeo := XInternAtom(display, '_NET_WM_ICON_GEOMETRY', False);

  XChangeProperty(fDisplay, Window, wmgeo, XA_CARDINAL,
    32, PropModeReplace, @propsets, 4);
  XMapWindow(fDisplay, Window);
end;

end.
