unit SystemAppItem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSystemAppItem = class(TObject)
  public
    Path: string;
    Name: string;
    GenericName: string;
    Comment: string;
    IconName: string;
    StartupWMClass: string;
    Exec: string;
    Args: TStrings;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TSystemAppItem.Create;
begin
  inherited Create;
  Args := TStringList.Create;
end;

destructor TSystemAppItem.Destroy;
begin
  FreeAndNil(Args);
  inherited Destroy;
end;

end.

