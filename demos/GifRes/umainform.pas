Unit uMainForm;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  LResources, ExtCtrls, // /!\ Unité obligatoire pour charger la resource.
  uGifViewer;

Type

  { TMainForm }

  TMainForm = Class(TForm)
    GIFViewer1: TGIFViewer;
    Panel1: TPanel;
    Procedure FormShow(Sender: TObject);
  private

  public

  End;

Var
  MainForm: TMainForm;

Implementation

{$R *.lfm}



{ TMainForm }

Procedure TMainForm.FormShow(Sender: TObject);
var
  newLeft, newTop : Integer;
Begin
  GIFViewer1.LoadFromResource('pulse_button_01');
  newLeft := (clientWidth div 2) - (GIFViewer1.Width div 2);
  newTop := (clientHeight div 2) - (GIFViewer1.Height div 2);
  GIFViewer1.Left := newLeft;
  GIFViewer1.Top :=  newTop;
  //GIFViewer1.Start;
end;

initialization
  {$I pulsebutton.lrs} // Placer le fichier resource à inclure


End.

