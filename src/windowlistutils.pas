unit WindowListUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, xwindowlist, xlib, x,  fgl, xatom, Dialogs, ctypes,
  Graphics,  GraphType, LCLType, LCLIntf, FPImage, BGRABitmap, BGRABitmapTypes;

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
    procedure UpdateInformation;
    function GetIcon: TBGRABitmap;
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
  UpdateInformation;
end;

destructor TWindowData.Destroy;
begin
  inherited Destroy;
end;

procedure TWindowData.UpdateInformation;
begin
  fWindowPID := fXWindowList.GetWindowPID(fWindow);
  fName := fXWindowList.GetWindowName(fWindow);
  fState := fXWindowList.GetWindowState(fWindow);
  fHost := fXWindowList.GetWindowHost(fWindow);
  if fWindowPID <> 0 then
    fCommand := fXWindowList.GetWindowCmd(fWindowPID);
  fGeometry := fXWindowList.GetWindowRect(fWindow);
end;

{ in case useful later }
//function ReadBitmap(Val: PByte; W, H, Size: integer): TBGRABitmap;
//var
//  bmp: TBGRABitmap;
//  a, r, g, b: byte;
//  argb: PByte;
//  i, f, x, y: Integer;
//  imgBuffer: TBytes;
//begin
//  bmp := TBGRABitmap.Create(w, h);
//  SetLength(imgBuffer, Length(TBytes(Val)) div 2);
//  argb := PByte(imgBuffer);
//  i := 0;
//  while i < Length(TBytes(Val)) div 2 do
//  begin
//    argb[i] := Val[(i*2)];
//    argb[i+1] := Val[(i*2)+1];
//    argb[i+2] := Val[(i*2)+2];
//    argb[i+3] := Val[(i*2)+3];
//    i += 4;
//  end;
//  for y := 0 to H -1 do
//  begin
//    for x := 0 to W -1 do
//    begin
//      a := argb[3];
//      r := argb[2];
//      g := argb[1];
//      b := argb[0];
//      //bmp.Canvas.DrawPixel(x, y, FPImage.FPColor(r, g, b, a));
//      bmp.DrawPixel(x, y, BGRA(r, g, b, a));
//
//      Inc(argb, 4);
//    end;
//  end;
//
//  Result := bmp;
//end;

function ReadBitmap(Val: PByte; W, H, Size: integer): TBGRABitmap;
var
  bmp: TBGRABitmap;
  argb: PByte;
  pdest: PBGRAPixel;
  x, y, i: Integer;
  imgBuffer: TBytes;
begin
  bmp := TBGRABitmap.Create(w, h);
  argb := Val;
  for y := 0 to H -1 do
  begin
    pdest := bmp.ScanLine[y];
    for x := 0 to W -1 do
    begin
      pdest^.alpha := argb[3];
      pdest^.red := argb[2];
      pdest^.green := argb[1];
      pdest^.blue := argb[0];
      Inc(argb, {$IFDEF CPU64}8{$ELSE}4{$ENDIF});
      inc(pdest);
    end;
  end;
  bmp.InvalidateBitmap;
  Result := bmp;
end;

function TWindowData.GetIcon: TBGRABitmap;
var
  ActualTypeReturn: TAtom;
  ActualFormatReturn: LongInt;
  NItemsReturn, BytesAfterReturn: Cardinal;
  Ptr: PByte;
  IconAtom: TAtom;
  PropResult: boolean;
  i: integer;
  Width, Height, Size: Cardinal;
  ms: TMemoryStream;
begin
  Width := 0;
  Height := 0;
  IconAtom := XInternAtom(fXWindowList.Display, '_NET_WM_ICON', LongBool(1));

  PropResult := XGetWindowProperty(fXWindowList.Display, fWindow, IconAtom,
    0, 1, 0, AnyPropertyType, @ActualTypeReturn, @ActualFormatReturn,
    @NItemsReturn, @BytesAfterReturn, @Ptr) = Success;
  if PropResult then
    Width := PCardinal(Ptr)^;
  if Assigned(Ptr) then XFree(Ptr);

  PropResult := XGetWindowProperty(fXWindowList.Display, fWindow, IconAtom,
    1, 1, 0, AnyPropertyType, @ActualTypeReturn, @ActualFormatReturn,
    @NItemsReturn, @BytesAfterReturn, @Ptr) = Success;
  if PropResult then
    Height := PCardinal(Ptr)^;
  Size := Width * Height;
  if Assigned(Ptr) then XFree(Ptr);

  PropResult := XGetWindowProperty(fXWindowList.Display, fWindow, IconAtom,
    2, Size, 0, AnyPropertyType, @ActualTypeReturn, @ActualFormatReturn,
    @NItemsReturn, @BytesAfterReturn, @Ptr) = Success;

  if PropResult then
  begin
    Result := ReadBitmap(Ptr, Width, Height, Size);
  end;

  //ms := TMemoryStream.Create;
  //try
  //  ms.Position := 0;
  //
  //  ms.WriteBuffer(Ptr, Size);
  //  ms.SaveToFile(ExtractFilePath(ParamStr(0)) + 'dumpedimages/w'+Width.ToString+'h'+Height.ToString+'_'+fWindowPID.ToString);
  //finally
  //  ms.Free;
  //end;

  if Assigned(Ptr) then XFree(Ptr);
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
      if (fXWindowList.GetWindowPID(fXWindowList.WindowList[i]) = fItems[j].WindowPID)
        then
      begin
        if i <> currentIndex then
        begin
          currentIndex := 1;
          ActiveIndex := j;
        end;
        fItems[j].UpdateInformation;
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
      if (fItems[i].WindowPID = fXWindowList.GetWindowPID(fXWindowList.WindowList[j])) then
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

