unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,FileUtil,Forms,Controls,Graphics,Dialogs,ComCtrls,
  ExtCtrls, WindowListUtils, xwindowlist, x, xlib, BGRABitmap;

type

  TMyWindow = class(TWindowData)
  public
    Img: TImage;
    constructor Create(AXWindowList: TXWindowList; AWindow: TWindow); override;
    destructor Destroy; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    Timer1:TTimer;
    TopLevelList:TListView;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender:TObject);
    procedure FormDestroy(Sender:TObject);
    procedure Timer1Timer(Sender:TObject);
    procedure TopLevelListClick(Sender: TObject);
    procedure TopLevelListDblClick(Sender:TObject);
  private
    { private declarations }
    fWindowList:TWindowList;
  public
    { public declarations }
    procedure UpdateTree;
  end;

var
  Form1: TForm1;

implementation

constructor TMyWindow.Create(AXWindowList: TXWindowList; AWindow: TWindow);
var
  bmp: TBGRABitmap;
begin
  inherited Create(AXWindowList, AWindow);
  //Img := TImage.Create(Form1);

  //ShowMessage('something happen');

  bmp := GetIcon;
  //Img.Width := Form1.Panel1.Height;
  //Img.Height := Form1.Panel1.Height;
  //Img.Parent := Form1.Panel1;
  //Form1.Image1.Picture.Bitmap.BeginUpdate();
  bmp.Draw(Form1.Image1.Canvas, 0, 0);
  //Form1.Image1.Canvas.Draw(0, 0, bmp);
  //Form1.Image1.Picture.Bitmap.EndUpdate();
  //bmp.SaveToFile(ExtractFilePath(Application.ExeName)+'moyang');
  //Img.Picture.Bitmap.LoadFromRawImage(bmp.RawImage, false);

  //DockButton.Width := frDock.pnDock.Width;
  bmp.Free;
end;

destructor TMyWindow.Destroy;
begin
  FreeAndNil(Img);
  inherited Destroy;
end;

{$R *.lfm}

{ TForm1 }


procedure TForm1.FormCreate(Sender:TObject);
begin
fWindowList:=TWindowList.Create(TMyWindow);
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  Deactivate;
end;

procedure TForm1.FormDestroy(Sender:TObject);
begin
fWindowList.Free;
end;

procedure TForm1.Timer1Timer(Sender:TObject);
begin
UpdateTree;
end;

procedure TForm1.TopLevelListClick(Sender: TObject);
var
  currentItem: TListItem;
begin
  Timer1.Enabled := False;
  if TopLevelList.Selected <> nil then
  begin
    currentItem := TopLevelList.Selected;
    Self.Deactivate;
    //ShowMessage(currentItem.SubItems[0]);
    if currentItem.SubItems[0] = 'Iconic' then
    begin
      TWindowData(currentItem.Data).ActivateWindow;
    end
    else
    begin
      if TWindowData(currentItem.Data) = fWindowList[fWindowList.ActiveIndex] then
        TWindowData(currentItem.Data).MinimizeWindow
      else
        TWindowData(currentItem.Data).ActivateWindow;

    end;
  end;
  Caption := fWindowList.ActiveIndex.ToString;
  Timer1.Enabled := true;
end;

procedure TForm1.TopLevelListDblClick(Sender:TObject);
begin

end;

function StringRect(Rct: TRect): string;
begin
  Result := 'L:' + inttostr(Rct.Left) +
    ' T:' + inttostr(Rct.Top) +
    ' R:' + inttostr(Rct.Right) +
    ' B:' + inttostr(Rct.Bottom);
end;


procedure TForm1.UpdateTree;
var i:integer;
Item: TListItem;
PID:Cardinal;
Rec:TRect;
begin
fWindowList.UpdateDataList;
TopLevelList.Clear;
TopLevelList.BeginUpdate;
for i:=0 to fWindowList.Count-1 do
  begin
   Item := TopLevelList.Items.Add;
   Item.Data := Pointer(fWindowList[i]);
   Item.Caption := fWindowList[i].Name;
   Item.SubItems.Add(fWindowList[i].State);
   Item.SubItems.Add(fWindowList[i].Host);
   PID:=fWindowList[i].WindowPID;
   Item.SubItems.Add(fWindowList[i].Command);
   Item.SubItems.Add(PID.ToString);
   Rec:=fWindowList[i].Geometry;
   Item.SubItems.Add(StringRect(Rec));
  end;
TopLevelList.Sort;
TopLevelList.EndUpdate;
end;

end.

