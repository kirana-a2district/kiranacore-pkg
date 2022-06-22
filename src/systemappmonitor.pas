unit SystemAppMonitor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SystemAppItem, fgl, FileUtil, IniFiles, DirWatcher, TypInfo,
  Dialogs;

const
  SYSTEMAPPSFOLDER = '/usr/share/applications';
type
  TSystemAppItemList = specialize TFPGList<TSystemAppItem>;
  TSystemAppMonitor = class(TComponent)
  private
//    this watcher is broken
    Watcher: TDirwatcher;
    procedure AddApplication(const AFilePath: string);
    procedure RemoveApplication(AItem: TSystemAppItem);
  public
    Items: TSystemAppItemList;
    OnRefreshed: TNotifyEvent;
    constructor Create(AOwner: TComponent); override;
    procedure Refresh(Sender: TObject);
    destructor Destroy; override;
    function Find(const AFilePAth: string): TSystemAppItem;
  end;
implementation

function DetectDesktopEnvirontment: string;
begin
  Result := GetEnvironmentVariable('XDG_CURRENT_DESKTOP');
  if Result <> '' then
    Result := Result.ToUpper
  else
    Result := 'UNKNOWN';
end;

constructor TSystemAppMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Items := TSystemAppItemList.Create;
  // to-do: add folder watcher
  Watcher := TDirwatcher.Create(Self);
  Watcher.Path := SYSTEMAPPSFOLDER;
  Watcher.OnRefresh := @Refresh;
  Watcher.Start;
end;

destructor TSystemAppMonitor.Destroy;
begin
  Watcher.Stop;
  FreeAndNil(Items);
  inherited Destroy;
end;

function TSystemAppMonitor.Find(const AFilePAth: string): TSystemAppItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Items.Count -1 do
  begin
    if Items[i].Path = AFilePAth then
      Result := Items[i];
  end;
end;

procedure TSystemAppMonitor.Refresh(Sender: TObject);
var
  AddedEntries: TStrings;
  AllEntries: TStrings;
  i: integer;
begin
  AddedEntries := TStringList.Create;
  for i := 0 to Items.Count -1 do
    AddedEntries.Add(Items[i].Path);

  AllEntries := FindAllFiles(SYSTEMAPPSFOLDER, '*.desktop', true);
  try
    for i := 0 to AllEntries.Count -1 do
    begin
      if AddedEntries.IndexOf(AllEntries[i]) = -1 then
        AddApplication(AllEntries[i]);
    end;
    for i := 0 to Items.Count -1 do
    begin
      if AllEntries.IndexOf(Items[i].Path) = -1 then
        RemoveApplication(Items[i]);
    end;
    if Assigned(OnRefreshed) then
    begin
      OnRefreshed(Self);
    end;
  finally
    AddedEntries.Free;
    AllEntries.Free;
  end;
end;

function ParseArgs(s: string; mode: integer): string;
var
  i: Integer;
  L: TStringList;
begin
  Result := '';
  L := TStringList.Create;
  try
    L.Delimiter := ' ';
    L.DelimitedText := StringReplace(s, '''', '"', [rfReplaceAll]);
    if mode = 0 then
    begin
      for i := 0 to L.Count-1 do
      begin
        if i = 0 then
          Result += L[i]
        else
          Result += ' ' + L[i];
      end;
    end
    else if (mode = 1) and (L.Count > 0) then
    begin
      if L.Count > 0 then
        Result := L[0];
    end
    else if (mode = 2) and (L.Count > 0) then
    begin
      for i := 1 to L.Count-1 do
      begin
        if i = 1 then
          Result += L[i]
        else
          Result += ' ' + L[i];
      end;
    end;
  finally
    L.Free;
  end;
end;

procedure TSystemAppMonitor.AddApplication(const AFilePath: string);
var
  DesktopFile: TIniFile;
  AppExec: string = '';
  Item: TSystemAppItem;
const
  DFSECTION = 'Desktop Entry';
begin
  if not FileExists(AFilePath) then
    Exit;

  DesktopFile := TIniFile.Create(AFilePath);
  if DesktopFile.ReadString(DFSECTION, 'Terminal', '').toLower.Contains('true') then
    Exit;
  if DesktopFile.ValueExists(DFSECTION, 'OnlyShowIn') then
    if not DesktopFile.ReadString(DFSECTION, 'OnlyShowIn', '').Contains(
      DetectDesktopEnvirontment) then
      Exit;
  if DesktopFile.ValueExists(DFSECTION, 'NotShowIn') then
    if DesktopFile.ReadString(DFSECTION, 'NotShowIn', '').Contains(
      DetectDesktopEnvirontment) then
      Exit;
  if DesktopFile.ReadString(DFSECTION, 'NoDisplay', '').ToLower.Contains('true') or
    DesktopFile.ReadString(DFSECTION, 'Hidden', '').ToLower.Contains('true') then
    Exit;
  AppExec := DesktopFile.ReadString(DFSECTION, 'Exec', '');

  Item := TSystemAppItem.Create;
  Item.Path := AFilePath;
  Item.Name := DesktopFile.ReadString(DFSECTION, 'Name', '');
  Item.GenericName := DesktopFile.ReadString(DFSECTION, 'GenericName', '');
  Item.Comment := DesktopFile.ReadString(DFSECTION, 'Comment', '');
  Item.IconName := DesktopFile.ReadString(DFSECTION, 'Icon', '');
  Item.StartupWMClass := DesktopFile.ReadString(DFSECTION, 'StartupWMClass', '');
  Item.Exec := ParseArgs(DesktopFile.ReadString(DFSECTION, 'Exec', ''), 1);
  //Item.Args := ;
  Item.ArgStr := ParseArgs(DesktopFile.ReadString(DFSECTION, 'Exec', ''), 2)
    .Replace('%U', '', [rfReplaceAll]);
  Item.ArgStr := Item.ArgStr.Replace('%u', '',
    [rfReplaceAll]);
  Items.Add(Item);
  //ShowMessage(Item.Name);
end;

procedure TSystemAppMonitor.RemoveApplication(AItem: TSystemAppItem);
begin
  Items.Remove(AItem);
  AItem.Free;
end;

end.

