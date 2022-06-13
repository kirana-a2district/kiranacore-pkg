unit WindowListUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, xwindowlist, xlib, x,  fgl, xatom, Dialogs, ctypes;

type
  TWindowData = class(TPersistent)
  private
    fName: string;
    fXWindowList: TXWindowList;
    fWindow: TWindow;
    fState: string;
    fHost: string;
    fCommand: string;
    fWindowPID: cardinal;
    fGeometry: TRect;
  public
    property State: string read fState;
    property Host: string read fHost;
    property Command: string read fCommand;
    property WindowPID: cardinal read fWindowPID;
    property Name: string read fName;
    property Geometry: TRect read fGeometry;
    constructor Create(AXWindowList: TXWindowList; AWindow: TWindow); virtual;
    destructor Destroy; override;
    procedure ActivateWindow;
    procedure MinimizeWindow;
    function FetchAtomNames: string;
  end;

  TWindowDataClass = class of TWindowData;

  TWindowDataList = specialize TFPGObjectList<TWindowData>;

  TWindowList = class(TPersistent)
  private
    fItems: TWindowDataList;
    fXWindowList: TXWindowList;
    fWindowDataClass: TWindowDataClass;
    function getCount: integer;
    function getItems(const index: integer): TWindowData;
    function IsHasIcon(AWindow: TWindow): boolean;
  public
    ActiveIndex: integer;
    property Count: integer read getCount;
    property Items: TWindowDataList read fItems;
    property Values[const index: integer]: TWindowData read getItems; default;
    procedure UpdateDataList;
    constructor Create(WindowDataClass: TWindowDataClass = nil);
    destructor Destroy; override;
  end;

implementation

constructor TWindowData.Create(AXWindowList: TXWindowList; AWindow: TWindow);
begin
  inherited Create;
  fXWindowList := AXWindowList;
  fWindow := AWindow;
  fWindowPID := fXWindowList.GetWindowPID(AWindow);
  fName := fXWindowList.GetWindowName(AWindow);
  fState := fXWindowList.GetWindowState(AWindow);
  fHost := fXWindowList.GetWindowHost(AWindow);
  if fWindowPID <> 0 then
    fCommand := fXWindowList.GetWindowCmd(fWindowPID);
  fGeometry := fXWindowList.GetWindowRect(AWindow);
end;

destructor TWindowData.Destroy;
begin
  inherited Destroy;
end;

function TWindowData.FetchAtomNames: string;
var
  atoms: PAtom;
  numberatom: Cardinal;
  atomname: string;
  i: integer;
begin
  Result := '';
  Atoms := XListProperties(fXWindowList.Display, fWindow, @numberatom);
  for i := 0 to numberatom -1 do
  begin
    atomname := XGetAtomName(fXWindowList.Display, atoms[i]);
    //if atomname.Contains('ICON') then
    Result := Result + ', ' + atomname;
  end;

end;

procedure TWindowData.ActivateWindow;
begin
  fXWindowList.ActivateWindow(fWindow);
end;

procedure TWindowData.MinimizeWindow;
begin
  XIconifyWindow(fXWindowList.Display, fWindow, XDefaultScreen(fXWindowList.Display));
end;

{ TWindowList }

function TWindowList.IsHasIcon(AWindow: TWindow): boolean;
var
  atoms: PAtom;
  numberatom: Cardinal;
  atomname: string;
  i: integer;
begin
  Result := false;
  Atoms := XListProperties(fXWindowList.Display, AWindow, @numberatom);
  for i := 0 to numberatom -1 do
  begin
    atomname := XGetAtomName(fXWindowList.Display, atoms[i]);
    if atomname.Equals('_NET_WM_ICON') then
      Result := true;
  end;
end;

function TWindowList.getItems(const index: integer): TWindowData;
begin
  Result := fItems[index];
end;

function TWindowList.getCount: integer;
begin
  Result := fItems.Count;
end;

procedure TWindowList.UpdateDataList;
var
  i, j: integer;
  currentIndex: integer;
  DoLater: boolean;
  WinItem: TWindowData;
  DeleteList: specialize TFPGList<TWindowData>;
  function CompareGeometry(A, B: TRect): boolean;
  begin
    Result := True;
    if A.Top <> B.Top then
      Result := False;
    if A.Left <> B.Left then
      Result := False;
    if A.Bottom <> B.Bottom then
      Result := False;
    if A.Right <> B.Right then
      Result := False;
  end;
begin
  fXWindowList.UpdateWindowList;
  currentIndex := 0;
  for i := 0 to fXWindowList.WindowList.Count -1 do
  begin
    DoLater := True;
    for j := 0 to fItems.Count -1 do
    begin
      if
        (fXWindowList.GetWindowName(fXWindowList.WindowList[i]) = fItems[j].Name) and
        (fXWindowList.GetWindowState(fXWindowList.WindowList[i]) = fItems[j].State) and
        (fXWindowList.GetWindowHost(fXWindowList.WindowList[i]) = fItems[j].Host) and
        (fXWindowList.GetWindowPID(fXWindowList.WindowList[i]) = fItems[j].WindowPID) and
        CompareGeometry(fXWindowList.GetWindowRect(fXWindowList.WindowList[i]), fItems[j].Geometry)
        then
      begin
        if i > currentIndex then
        begin
          currentIndex := 1;
          ActiveIndex := j;
        end;
        DoLater := False;
      end;
    end;
    if DoLater then
    begin
      if IsHasIcon(fXWindowList.WindowList[i]) then
      begin
        if fWindowDataClass <> nil then
          WinItem := TWindowDataClass(fWindowDataClass).Create(fXWindowList, fXWindowList.WindowList[i])
        else
          WinItem := TWindowData.Create(fXWindowList, fXWindowList.WindowList[i]);
        fItems.Add(WinItem);
      end;
    end;
  end;

  DeleteList := specialize TFPGList<TWindowData>.Create;
  for i := 0 to fItems.Count -1 do
  begin
    DoLater := True;
    for j := 0 to fXWindowList.WindowList.Count -1 do
    begin
      if (fItems[i].Name = fXWindowList.GetWindowName(fXWindowList.WindowList[j])) and
        (fItems[i].State = fXWindowList.GetWindowState(fXWindowList.WindowList[j])) and
        (fItems[i].WindowPID = fXWindowList.GetWindowPID(fXWindowList.WindowList[j])) and
        CompareGeometry(fItems[i].Geometry, fXWindowList.GetWindowRect(fXWindowList.WindowList[j]))
        then
      begin
        DoLater := False;
      end
    end;
    if DoLater then
    begin
      DeleteList.Add(fItems[i]);
    end;
  end;
  for i := 0 to DeleteList.Count -1 do
  begin
    fItems.Remove(DeleteList[i]);
  end;

  FreeAndNil(DeleteList);
end;

constructor TWindowList.Create(WindowDataClass: TWindowDataClass = nil);
begin
  inherited Create;
  fItems := TWindowDataList.Create;
  fXWindowList := TXWindowList.Create;
  fWindowDataClass := WindowDataClass;
end;

destructor TWindowList.Destroy;
begin
  FreeAndNil(fItems);
  FreeAndNil(fXWindowList);
  inherited Destroy;
end;

end.

