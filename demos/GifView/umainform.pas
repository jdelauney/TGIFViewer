Unit uMainForm;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin,
  // TGifViewer
  uGifViewer,
  // TGVTranslate Traduction de la langue / Translate language
  //gvTranslate;
  BZApplicationTranslator;

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
    cbxStretchMode: TComboBox;
    edtViewFrameIndex: TSpinEdit;
    GroupBox1: TGroupBox;
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
    mmoComments: TMemo;
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
    Procedure cbxStretchModeSelect(Sender: TObject);
    Procedure chkCenterGIFClick(Sender: TObject);
    Procedure chkStretchGIFChange(Sender: TObject);
    Procedure chkTansparentChange(Sender: TObject);
    Procedure edtViewFrameIndexChange(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
    Procedure FormShow(Sender: TObject);
  private
  protected
    GifViewer : TGIFViewer;
    GIFLoaded, AppLoaded : Boolean;

    LangManager : TBZApplicationTranslator; //TGVTranslate;

    procedure DoOnTranslate(Sender:TObject;Const Folder, Lang, FallbackLang: String);
    Procedure DoOnBitmapLoadError(Sender: TObject; Const ErrorCount: Integer; Const ErrorList: TStringList);
    Procedure DoOnFrameChange(Sender: TObject);
    procedure DoOnStetchChanged(Sender: TObject;  IsStretched : Boolean);
  public


  End;

Var
  MainForm: TMainForm;

Implementation

{$R *.lfm}

Uses  FileCtrl, uErrorBoxForm;

resourcestring
  rsStretchManual = 'Manuel';
  rsStretchAll = 'Toutes';
  rsSTretchOnlyBigger = 'Les plus grandes';
  rsStretchOnlySmaller = 'Les plus petites';

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
  lblCurrentFrame.Caption := IntToStr(Succ(GIFViewer.CurrentFrameIndex));
End;

Procedure TMainForm.DoOnStetchChanged(Sender: TObject; IsStretched: Boolean);
Begin
  chkStretchGIF.Checked := IsStretched;
End;

Procedure TMainForm.FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
var
   ImageFileName : String;
   i: integer;
Begin
    Try
      GIFLoaded := False;
      Screen.Cursor := crHourGlass;
      ImageFileName := '';
      ImageFileName := FileNames[0];
      GifViewer.LoadFromFile(ImageFileName);
      lblFileName.Caption := MiniMizeName(ImageFileName, lblFileName.Canvas ,lblFileName.ClientWidth);
      lblVersion.Caption := GifViewer.Version;
      lblFrameCount.Caption := IntToStr(GifViewer.FrameCount);
      pnlAnimationPlayer.Enabled := (GifViewer.FrameCount>1);
      pnlSelectFrame.Enabled := (GifViewer.FrameCount>1);
      edtViewFrameIndex.MaxValue := GifViewer.FrameCount-1;
      edtViewFrameIndex.Value := 0;
      lblCurrentFrame.Caption := '1';
      lblTotalFrame.Caption := IntToStr(GifViewer.FrameCount);
      GIFLoaded := True;
      mmoComments.Lines.Clear;
      for i:=0 to GifViewer.FrameCount-1 do
      begin
        mmoComments.Lines.AddStrings(GifViewer.RawFrames[i].Comment);
      End;
    Finally
      Screen.Cursor := crDefault;
    End;
end;

Procedure TMainForm.FormShow(Sender: TObject);
Begin
  with cbxStretchMode.Items do
  begin
    clear;
    Add(rsStretchManual);
    Add(rsStretchAll);
    Add(rsStretchOnlyBigger);
    Add(rsStretchOnlySmaller);
  End;
  cbxStretchMode.ItemIndex := 1;

  if LangManager.Language = 'fr' then cbxLang.ItemIndex := 0 else cbxLang.ItemIndex := 1;
  AppLoaded := true;

end;

Procedure TMainForm.DoOnTranslate(Sender: TObject; Const Folder, Lang, FallbackLang: String);
Begin
  LangManager.Translate('GifViewerStrConsts');
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
  // on redémarre

  if AppLoaded then
  begin
    LangManager.Language := cbxLang.Items[cbxLang.ItemIndex];
    LangManager.RestartApplication;
  end;

end;

Procedure TMainForm.cbxStretchModeSelect(Sender: TObject);
Begin
  Case cbxStretchMode.ItemIndex of
    0 : GifViewer.AutoStretchMode := smManual;
    1 : GifViewer.AutoStretchMode := smStretchAll;
    2 : GifViewer.AutoStretchMode := smOnlyStretchBigger;
    3 : GifViewer.AutoStretchMode := smOnlyStretchSmaller;
  End;

  chkStretchGIF.Enabled := (GifViewer.AutoStretchMode = smManual);

end;

Procedure TMainForm.chkCenterGIFClick(Sender: TObject);
Begin
  GifViewer.Center := not(GifViewer.Center);
end;

Procedure TMainForm.chkStretchGIFChange(Sender: TObject);
Begin
  if GIFViewer.AutoStretchMode <> smManual then exit;
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
  AppLoaded := False;

  LangManager := TBZApplicationTranslator.Create;// TGVTranslate.Create;

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
    OnStretchChanged := @DoOnStetchChanged;
    AutoStretchMode := smStretchAll;
  End;
  chkStretchGIF.Enabled := false;
  LangManager.Run; //Translate;
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

