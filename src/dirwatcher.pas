unit DirWatcher;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF UNIX}
  baseunix,
  ctypes,
  linux,
  {$ENDIF}
  contnrs;

type
  TDirWatcher = class(TComponent)
  private
    FWatcherThread: TThread;
  public
    Path: string;
    UpdatedFiles: TStringList;
    OnRefresh: TNotifyEvent;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

  TWatcherThread = class(TThread)
  protected
    FWatcher: TDirWatcher;
    Paths: TStrings;
    procedure UpdatePaths;
  public
    constructor Create(AWatcher: TDirWatcher);
    procedure Execute; override;
  end;


implementation

{ TDirWatcher }

constructor TDirWatcher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UpdatedFiles := TStringList.Create;
end;

destructor TDirWatcher.Destroy;
begin
  FreeAndNil(UpdatedFiles);
  inherited Destroy;
end;

procedure TDirWatcher.Start;
begin
  FWatcherThread := TWatcherThread.Create(Self);
  FWatcherThread.Start;
end;

procedure TDirWatcher.Stop;
begin
  if Assigned(FWatcherThread) then
    FWatcherThread.Terminate;
end;

{ TWatcherThread }

constructor TWatcherThread.Create(AWatcher: TDirWatcher);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FWatcher := AWatcher;
end;

procedure TWatcherThread.UpdatePaths;
begin
  FWatcher.UpdatedFiles.Text := Paths.Text;
  if Assigned(FWatcher.OnRefresh) then
    FWatcher.OnRefresh(FWatcher);
end;

procedure TWatcherThread.Execute;
const
  Events = IN_MODIFY or IN_ATTRIB or IN_CREATE or IN_DELETE;
var
  fd, wd, fnl, len: cint;
  fds: tfdset;
  e: ^inotify_event;
  buf: Array[0..1023*4] of Byte; // 4K Buffer
  fn: string;
  p: pchar;
  d: string;
  UpdateCount: integer;
  Timeout: TTimeval;
begin
  fd := inotify_init;
  d := FWatcher.Path;
  Paths := TStringList.Create;
  timeout.tv_sec:=100 div 1000;
  timeout.tv_usec:=(100 mod 1000)*1000;
  try
    wd := inotify_add_watch(fd, pchar(d), Events);
    fpFD_Zero(fds);
    fpFD_SET(fd, fds);
    while (fpSelect(fd + 1, @fds, nil, nil, @Timeout) >= 0) and (not Terminated) do
    begin
      len := fpRead(fd,buf,sizeof(buf));
      e := @buf;
      Paths.Clear;
      UpdateCount := 0;
      while ((pchar(e) - @buf) < len) and (not Terminated) do
      begin
        fnl := e^.len;
        if fnl > 0 then
        begin
          p := @e^.name + fnl - 1;
          while p^ = #0 do
          begin
            dec(p);
            dec(fnl);
          end;
        end;
        setlength(fn, fnl);
        if (fnl > 0) then
          move(e^.name, fn[1], fnl);
        UpdateCount += 1;
        if FileExists(fn) then
        begin
          Paths.Add(fn);
        end;
        //Writeln('Change ', e^.mask, ' (',
  //                InotifyEventsToString(e^.mask),
                //') detected for file "', fn, '"');
        ptrint(e) := ptrint(e) + sizeof(inotify_event) + e^.len - 1;
      end;
      if UpdateCount > 0 then
      begin
        UpdatePaths;
      end;
    end;
  finally
    fpClose(fd);
    FreeAndNil(Paths);
  end;
end;

end.

