{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

Unit gifviewer_pkg;

{$warn 5023 off : no warning about unused units}
Interface

uses
  TypesHelpers, uFastBitmap, uGifViewer, LazarusPackageIntf;

Implementation

Procedure Register;
Begin
  RegisterUnit('uGifViewer', @uGifViewer.Register);
End;

Initialization
  RegisterPackage('gifviewer_pkg', @Register);
End.
