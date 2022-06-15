unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BCButton, SystemAppMonitor, fgl,
  BCTypes;

type

  TBCButtonList = specialize TFPGObjectList<TBCButton>;

  { TForm1 }

  TForm1 = class(TForm)
    btLaunch: TBCButton;
    ScrollBox1: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    AppMon: TSystemAppMonitor;
    BCButtons: TBCButtonList;
    procedure AppMonRefreshed(Sender: TObject);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.AppMonRefreshed(Sender: TObject);
var
  i: integer;
  btn: TBCButton;
begin
  if Visible then
  begin
    for i := 0 to AppMon.Items.Count -1 do
    begin
      btn := TBCButton.Create(nil);
      btn.Assign(btLaunch);
      btn.Parent := ScrollBox1;
      btn.Visible := True;
      btn.GlyphAlignment := bcaCenterTop;
      btn.GlyphOldPlacement := false;
      btn.Constraints.MinWidth := btLaunch.Width;
      btn.Constraints.MinHeight := btLaunch.Height;
      btn.Constraints.MaxWidth := btLaunch.Width;
      btn.Caption := AppMon.Items[i].Name;
      //if FileExists(AppMon.Items[i].IconName) then
        btn.Glyph.Assign(btLaunch.Glyph);
      btn.BorderSpacing.Around := btLaunch.BorderSpacing.Around;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AppMon := TSystemAppMonitor.Create(self);
  AppMon.OnRefreshed := @AppMonRefreshed;
  BCButtons := TBCButtonList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AppMon.Free;
  BCButtons.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  ScrollBox1.ChildSizing.ControlsPerLine := ScrollBox1.Width div
    (btLaunch.Width + (btLaunch.BorderSpacing.Around));
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  AppMon.Refresh(Self);
end;

end.

