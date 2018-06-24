Unit uMainForm;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin,
  TypesHelpers, uGifViewer,
  gvTranslate;

Type

  { TMainForm }

  TMainForm = Class(TForm)
    btnPauseAnimation: TButton;
    btnStartAnimation: TButton;
    btnStopAnimation: TButton;
    chkCenterGIF: TCheckBox;
    chkStretchGIF: TCheckBox;
    btnChooseBackgroundColor: TColorButton;
    chkTansparent: TCheckBox;
    chkViewRawFrame: TCheckBox;
    cbxLang: TComboBox;
    edtViewFrameIndex: TSpinEdit;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblCurrentFrame: TLabel;
    Label6: TLabel;
    lblFileName: TLabel;
    lblTotalFrame: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblVersion: TLabel;
    lblFrameCount: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel6: TPanel;
    pnlAnimationPlayer: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    pnlSelectFrame: TPanel;
    pnlView: TPanel;
    Procedure btnChooseBackgroundColorColorChanged(Sender: TObject);
    Procedure btnPauseAnimationClick(Sender: TObject);
    Procedure btnStartAnimationClick(Sender: TObject);
    Procedure btnStopAnimationClick(Sender: TObject);
    Procedure cbxLangSelect(Sender: TObject);
    Procedure chkCenterGIFClick(Sender: TObject);
    Procedure chkStretchGIFChange(Sender: TObject);
    Procedure chkTansparentChange(Sender: TObject);
    Procedure edtViewFrameIndexChange(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
  private
  protected
    GifViewer : TGIFViewer;
    GIFLoaded : Boolean;
    LangManager : TGVTranslate;
    procedure DoOnTranslate(Sender:TObject;Const Folder, Lang, FallbackLang: String);
    Procedure DoOnBitmapLoadError(Sender: TObject; Const ErrorCount: Integer; Const ErrorList: TStringList);
    Procedure DoOnFrameChange(Sender: TObject);
  public


  End;

Var
  MainForm: TMainForm;

Implementation

{$R *.lfm}

Uses  Translations, FileCtrl, uErrorBoxForm;


{ TMainForm }


Procedure TMainForm.DoOnBitmapLoadError(Sender: TObject; Const ErrorCount: Integer; Const ErrorList: TStringList);
Begin
  If ErrorCount > 0 then
  begin
    ErrorBoxForm.Memo1.Lines.Clear;
    ErrorBoxForm.Memo1.Lines := ErrorList;
    ErrorBoxForm.ShowModal;
  End;
End;

Procedure TMainForm.DoOnFrameChange(Sender: TObject);
Begin
  lblCurrentFrame.Caption := Succ(GIFViewer.CurrentFrameIndex).ToString;
End;

Procedure TMainForm.FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
var
   ImageFileName : String;
Begin
    Try
      GIFLoaded := False;
      Screen.Cursor := crHourGlass;
      ImageFileName := '';
      ImageFileName := FileNames[0];
      GifViewer.LoadFromFile(ImageFileName);
      lblFileName.Caption := MiniMizeName(ImageFileName, lblFileName.Canvas ,lblFileName.ClientWidth);
      lblVersion.Caption := GifViewer.Version;
      lblFrameCount.Caption := GifViewer.FrameCount.ToString;
      pnlAnimationPlayer.Enabled := (GifViewer.FrameCount>1);
      pnlSelectFrame.Enabled := (GifViewer.FrameCount>1);
      edtViewFrameIndex.MaxValue := GifViewer.FrameCount-1;
      edtViewFrameIndex.Value := 0;
      lblCurrentFrame.Caption := '1';
      lblTotalFrame.Caption := GifViewer.FrameCount.ToString;
      GIFLoaded := True;

    Finally
      Screen.Cursor := crDefault;
    End;
end;

Procedure TMainForm.DoOnTranslate(Sender: TObject; Const Folder, Lang, FallbackLang: String);
Begin
  LangManager.Translate('uGIFViewer');
  LangManager.Translate('uFastBitmap');
 // Translations.TranslateUnitResourceStrings('uGIFViewer', Folder + 'uGIFViewer.'+Lang+'.po', Lang, FallbackLang);
 // Translations.TranslateUnitResourceStrings('uFastBitmap', Folder + 'uFastBitmap.'+Lang+'.po', Lang, FallbackLang);
End;

Procedure TMainForm.btnStartAnimationClick(Sender: TObject);
Begin
  pnlSelectFrame.Enabled := False;
  lblCurrentFrame.Caption := '1';
  GifViewer.Start;
end;

Procedure TMainForm.btnStopAnimationClick(Sender: TObject);
Begin
  pnlSelectFrame.Enabled := True;
  lblCurrentFrame.Caption := '1';
  GifViewer.Stop;
end;

Procedure TMainForm.cbxLangSelect(Sender: TObject);
Begin
  LangManager.Language := cbxLang.Items[cbxLang.ItemIndex];

  // on redémarre
  LangManager.Restart;
end;

Procedure TMainForm.chkCenterGIFClick(Sender: TObject);
Begin
  GifViewer.Center := not(GifViewer.Center);
end;

Procedure TMainForm.chkStretchGIFChange(Sender: TObject);
Begin
  GifViewer.Stretch := not(GifViewer.Stretch);
end;

Procedure TMainForm.chkTansparentChange(Sender: TObject);
Begin
  GifViewer.Transparent := not(GifViewer.Transparent);
end;

Procedure TMainForm.edtViewFrameIndexChange(Sender: TObject);
Begin
  if not(GIFLoaded) then exit;
  if edtViewFrameIndex.Text<>'' then
  begin
    if not(chkViewRawFrame.Checked) then GIFViewer.DisplayFrame(edtViewFrameIndex.Value)
    else GIFViewer.DisplayRawFrame(edtViewFrameIndex.Value);
  End;
end;

Procedure TMainForm.FormCreate(Sender: TObject);
Begin
  LangManager := TGVTranslate.Create;
  LangManager.OnTranslate := @DoOnTranslate;
  GifViewer := TGIFVIewer.Create(Self);
  With GifViewer do
  Begin
    Parent := pnlView;
    Align := alClient;
    Top := 10;
    Left := 10;
    Center := True;
    AutoSize := true;
    OnLoadError := @DoOnBitmapLoadError;
    OnFrameChange := @DoOnFrameChange;
  End;
  LangManager.Translate;
end;

Procedure TMainForm.FormDestroy(Sender: TObject);
Begin
  FreeAndNil(LangManager);
  FreeAndNil(GifViewer);
end;

Procedure TMainForm.btnPauseAnimationClick(Sender: TObject);
Begin
  GifViewer.Pause;
end;

Procedure TMainForm.btnChooseBackgroundColorColorChanged(Sender: TObject);
Begin
  pnlView.Color := btnChooseBackgroundColor.ButtonColor;
  pnlView.Invalidate;
end;

End.

