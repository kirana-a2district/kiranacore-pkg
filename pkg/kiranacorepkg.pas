{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit kiranacorepkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  DirWatcher, SystemAppItem, SystemAppMonitor, KiranaWindows, XWindowUtils, 
  XEventWatcher, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('kiranacorepkg', @Register);
end.
