Unit uFastBitmap;
(*==============================================================================
 DESCRIPTION   : Classe de manipulation de bitmap en 32 bit.
 DATE          : 17/06/2018
 VERSION       : 0.3
 AUTEUR        : J.Delauney (BeanzMaster)
 CONTRIBUTEURS : Jipete, Jurassik Pork
 LICENCE       : MPL
================================================================================
*)

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

Interface

Uses
  LCLType, LCLIntf, Classes, SysUtils, Graphics, Contnrs, Dialogs;

Const
  { Constantes utiles pour le calcul sur les masques de couleur }
  {$IFDEF WINDOWS} // Format BGRA
    cBlueOrder = 0;
    cGreenOrder = 1;
    cRedOrder = 2;
    cAlphaOrder = 3;
  {$ELSE} // Format RGBA
    cRedOrder = 0;
    cGreenOrder = 1;
    cBlueOrder = 2;
    cAlphaOrder = 3;
  {$ENDIF}
  cRedShift = cRedOrder * 8;
  cGreenShift = cGreenOrder * 8;
  cBlueShift = cBlueOrder * 8;
  cAlphaShift = cAlphaOrder * 8;

  maskRed = 1;
  maskGreen = 2;
  maskBlue = 4;
  maskAlpha = 8;
  maskRGB = maskRed Or maskGreen Or maskBlue;
  maskRGBA = maskRGB Or maskAlpha;

Type
  { TColorRGB24 : Définition d'un pixel sur 24 bits au format RGB }
  TColorRGB24Type = packed array[0..2] of byte;
  TColorRGB24 = packed record
    procedure Create(R,G,B : Byte); Overload;
    procedure Create(Color:TColor); Overload;
    function ToColor : TColor;

    Case Integer of
     0 : (V:TColorRGB24Type);
     1 : (Red, Green, Blue:Byte);
  end;

  { TColorRGBA32 : Définition d'un pixel sur 32 bits au format RGBA ou BGRA suivant l'OS}
  TColorRGBA32Type = packed array[0..3] of byte;
  TColorRGBA32 = Packed Record
  private
    function getColorComponent(Index : Integer): byte;
    procedure SetColorComponent(Index : Integer; aValue:Byte);
  public
    procedure Create(R,G,B,A : Byte); Overload;
    procedure Create(R,G,B : Byte);   Overload;
    procedure Create(Color : TColor); Overload;
    procedure Create(Color : TColorRGB24); Overload;

    { Conversion vers un TColor }
    function ToColor : TColor;
    { Conversion vers un TColorRGB24 }
    function ToColorRGB24 : TColorRGB24;
    { Mixage de la couleur courrante avec la couleur "Color" avec prise en charge du canal Alpha }
    function Blend(Color : TColorRGBA32): TColorRGBA32;

    class operator =(Color1,Color2 : TColorRGBA32):Boolean;

    { Accès aux composantes de la couleur }
    property Red:Byte Index cRedOrder read GetColorComponent Write SetColorComponent;
    property Green:Byte Index cGreenOrder read GetColorComponent Write SetColorComponent;
    property Blue:Byte Index cBlueOrder read GetColorComponent Write SetColorComponent;
    property Alpha:Byte Index cAlphaOrder read GetColorComponent Write SetColorComponent;

    Case Integer of
     0 : (V:TColorRGBA32Type);
     1 : (AsInteger : Integer);
  End;
  PColorRGBA32 = ^TColorRGBA32;

  { TColorRGBA32Item : Objet persistant englobant une couleur de type TColorRGBA32 }
  TColorRGBA32Item = Class(TPersistent)
  Private
    FColor: TColorRGBA32;
    FName:  String;
    FTag:   Integer;

    Procedure SetRed(Const AValue: Byte);
    Procedure SetGreen(Const AValue: Byte);
    Procedure SetBlue(Const AValue: Byte);
    Procedure SetAlpha(Const AValue: Byte);
    Procedure SetValue(Const AValue: TColorRGBA32);
    Procedure SetColorName(Const aName: String);

    Function getRed: Byte;
    Function getGreen: Byte;
    Function getBlue: Byte;
    Function getAlpha: Byte;
    Function getValue: TColorRGBA32;

  Protected
  Public
    Constructor Create;
    Destructor Destroy; override;

    { Valeur de la couleur }
    Property Value: TColorRGBA32 read getValue write setValue;
    { Nom de la couleur eg : clrRed }
    Property Name: String read FName write setColorName;
  Published
    { Valeur du canal rouge }
    Property Red: Byte read getRed write setRed;
    { Valeur du canal vert }
    Property Green: Byte read getRed write setGreen;
    { Valeur du canal Bleu }
    Property Blue: Byte read getRed write setBlue;
    { Valeur du canal alpha pour la transparence }
    Property Alpha: Byte read getRed write setAlpha;
    {  Valeur complémentaire personnel }
    Property Tag: Integer read FTag write FTag;
  End;

  { TBZColorList : Classe pour la gestion d'une palette (liste) de couleurs }
  TColorRGBA32List = Class(TObjectList)
  Private
  Protected
    Function GetColorItem(index: Integer): TColorRGBA32Item;
    Procedure SetColorItem(index: Integer; val: TColorRGBA32Item);
  Public
    procedure Clear; override;
    { Ajoute une couler à la liste }
    Function AddColor(Const aColor: TColorRGBA32): Integer; Overload;
    { Ajoute une couler à la liste }
    Function AddColor(Const aName: String; Const aColor: TColorRGBA32): Integer; Overload;
    { Ajoute une couler à la liste }
    Function AddColor(Const aColorItem: TColorRGBA32Item): Integer; Overload;
    { Supprime une couleur de la liste }
    Procedure RemoveColor(Const aName: String);
    { Recherche une couleur dans la liste }
    Function FindColorByName(Const aName: String; Out Index: Integer):TColorRGBA32; Overload;
    { Recherche une couleur dans la liste }
    Function FindColorByName(Const aName: String): TColorRGBA32; Overload;

    { Colors : Acceder à la couleur "Index" de la liste }
    Property Colors[Index: Integer]: TColorRGBA32Item read GetColorItem write setColorItem;
  End;

Const
  clrTransparent : TColorRGBA32 = (v:($00,$00,$00,$00));
  clrBlack       : TColorRGBA32 = (v:($00,$00,$00,$FF));
  clrWhite       : TColorRGBA32 = (v:($FF,$FF,$FF,$FF));


Type
  { TFastBitmapDrawMode : Mode d'Affichage pour la fonction PutImage de TFastBitmap }
  TFastBitmapDrawMode = ( dmSet, dmAlpha, dmAlphaCheck);

  { TFastBitmap }
  { Classe d'aide à la manipulation d'une image }
  TFastBitmap = Class
  Strict private
    FTransparentColor : TColor; // Couleur transparent à pour l'affichage via TBitmap de la LCL si besoin

    FData     : PDWord;    // Tampon de stockage des données d'un bitmap
    FWidth    : Integer;   // Largeur du bitmap
    FHeight   : Integer;   // Hauteur du Bitmap
    FSize     : Int64;     // Taille du tampon en octet
  protected

    procedure SetWidth(NewWidth : Integer);
    procedure SetHeight(NewHeight : Integer);

    function BuildBitmap : Graphics.TBitmap;
    function IsClipped(X,Y:Integer) : Boolean;


  Public
    Constructor Create; Overload;
    Constructor Create(NewWidth, NewHeight : Integer); Overload;
    Destructor Destroy; Override;

    { Assigne les donnée d'un autre TFastBitmap }
    procedure Assign(aFastBitmap : TFastBitmap);
    { Modifie les dimensions du bitmap }
    procedure SetSize(NewWidth, NewHeight : Integer);
    { Efface le bitmap avec la couleur "Color" }
    procedure Clear(Color : TColorRGBA32);
    { Retourne le tampon du bitmap }
    function GetSurfaceBuffer : PColorRGBA32;
    { Retourne l'adresse de la ligne "Y" dans le tampon }
    function GetScanLine(Y : Integer) : PColorRGBA32;
    { Retourne l'adresse du pixel à la position "X,Y" dans le tampon }
    function GetPixelPtr(X, Y : Integer) : PColorRGBA32;
    { Ecrit un pixel de couleur "Color" à la position "X,Y }
    procedure PutPixel(X,Y:Integer; Color : TColorRGBA32);
    { Lit un pixel de couleur "Color" à la position "X,Y }
    function GetPixel(X,Y:Integer): TColorRGBA32;
    { Ecrit un pixel de en mixant couleur "Color" avec la couleur du pixel présent dans le tampon à la position "X,Y }
    procedure PutPixelBlend(X,Y : Integer; Color : TColorRGBA32);
    { Copie une image source "Src" depuis la position "SrcX,SrcY" et de dimension "SrcWidthxSrcHeight" dans le bitmap à la position "DstX, DstY
      et suivant le "Mode"
       Mode : TFastBitmapDrawMode
        - dmSet : Copie brute de l'image
        - dmAlpha : Copie les pixel de l'image source en mixant les couleurs avec celles du bitmap en fonction de leur valeur Alpha
        - dmAlphaCheck : Copie les pixel de l'image source seulement si le pixel n'est pas invisible. les autres pixels sont copié de la façon que le mode dmAlpha
       Note : les dimensions et les positions entre le bitmap et l'image source sont automatiquement ajustées si besoin.
    }
    procedure PutImage(Src : TFastBitmap; SrcX, SrcY, SrcWidth, SrcHeight, DstX, DstY : Integer; Mode : TFastBitmapDrawMode);
    { Creation  d'un clone du bitmap (nouvelle instance) }
    function Clone : TFastBitmap;
    { Retourne un bitmap de type LCL ==> Graphics.TBitmap }
    function GetBitmap : Graphics.TBitmap;
    { Dessine le bitmap sur un canvas à la position "X,Y" }
    procedure Draw(ACanvas : TCanvas; X,Y : Integer);  Overload;
    { Dessine le bitmap sur un canvas délimité par "Rect" }
    procedure Draw(ACanvas : TCanvas; Rect : TRect);   Overload;
    { Inverse les composante de couleur Rouge et Bleu du bitmap }
    procedure SwapRB;
    { Information sur la couleur assignée à la transparence (seulement valable si différent de clrTransparent) }
    property TransparentColor : TColor Read FTransparentColor Write FTransparentColor;
    { Largeur du bitmap }
    property Width : Integer Read FWidth Write SetWidth;
    { Hauteur du bitmap }
    property Height : Integer Read FHeight Write SetHeight;
    { Taille du tampon en octet }
    property Size : Int64 Read FSize;
  End;

Implementation

Uses Types, GraphType, Math;

{%region=====[ TColorRGB24 ]====================================================}

Procedure TColorRGB24.Create(R, G, B : Byte);
Begin
 Red := R;
 Green := G;
 Blue := B;
End;

Procedure TColorRGB24.Create(Color : TColor);
Var
  lr,lg,lb : Byte;
Begin
  lr := Color;
  lg := Color shr 8;
  lb := Color shr 16;
  Create(lr,lg,lb);
End;

Function TColorRGB24.ToColor : TColor;
Begin
  Result := Red + (Green shl 8) + (Blue shl 16);
End;

{%endregion%}

{%region=====[ TColorRGBA32 ]===================================================}

Function TColorRGBA32.getColorComponent(Index: Integer): byte;
Begin
  result := v[Index];
End;

Procedure TColorRGBA32.SetColorComponent(Index: Integer; aValue: Byte);
Begin
  v[Index] := aValue;
End;

Procedure TColorRGBA32.Create(R, G, B, A : Byte);
Begin
  Red := R;
  Green := G;
  Blue := B;
  Alpha := A;
End;

Procedure TColorRGBA32.Create(R, G, B : Byte);
Begin
  Create(R,G,B,255);
End;

Procedure TColorRGBA32.Create(Color : TColor);
Var
  ColorRGB24 : TColorRGB24;
Begin
  ColorRGB24.Create(Color);
  Create(ColorRGB24);
End;

Procedure TColorRGBA32.Create(Color : TColorRGB24);
Begin
  Create(Color.Red,Color.Green,Color.Blue);
End;

Function TColorRGBA32.ToColor : TColor;
Begin
 Result := ToColorRGB24.ToColor;
End;

Function TColorRGBA32.ToColorRGB24 : TColorRGB24;
Begin
 Result.Red := Red;
 Result.Green := Green;
 Result.Blue := Blue;
End;

//Function TColorRGBA32.Blend(Color : TColorRGBA32) : TColorRGBA32;
//Var
//  Coef1,Coef2,Coef3,Coef4, Tmp : Cardinal;
//Begin
// Result := Self;
// if (Color.Alpha = 0) or (Result = Color) then Exit;
// Tmp := (255-Color.Alpha);
// Coef1 := 65025 - (255-Result.Alpha) * Tmp;
// Coef2 := Coef1 shr 1;
// Coef3 := Result.Alpha*Tmp;
// Coef4 := Color.Alpha * 255;
// With Result do
// begin
//   Red   := ((Red * Coef3) + (Color.Red * Coef4) + Coef2) div Coef1;
//   Green := ((Green * Coef3) + (Color.Green * Coef4) + Coef2) div Coef1;
//   Blue  := ((Blue * Coef3) + (Color.Blue * Coef4) + Coef2) div Coef1;
//   Alpha := (Coef1 + Coef1 shr 7 ) shr 8;
// end;
//End;

Function TColorRGBA32.Blend(Color : TColorRGBA32) : TColorRGBA32;
var
  factor, factor2:single;
begin
  if Color.Alpha = 255 then Result := Color
  else  if Color.Alpha = 0 then Result:= Self
  else
  begin
    factor := Color.Alpha / 255;
    factor2 := 1 - Factor;
    Result.Red  := Round((Self.Red*Factor)+(Color.Red*factor2));
    Result.Green  := Round((Self.Green*Factor)+(Color.Green*Factor2));
    Result.Blue  := Round((Self.Blue*Factor)+(Color.Blue*Factor2));
    Result.alpha := Round((Self.Alpha*Factor)+(Color.Alpha*Factor2));
  End;
end;

Class Operator TColorRGBA32. = (Color1, Color2 : TColorRGBA32) : Boolean;
Begin
  Result := False;
  if (Color1.Alpha = 0) and (Color2.Alpha = 0) then Result :=True
  else Result := ((Color1.Red = Color2.Red) and (Color1.Green = Color2.Green) and (Color1.Blue = Color2.Blue) and (Color1.Alpha = Color2.Alpha))
End;

{%endregion%}

{%region=====[ TColorRGBA32Item ]===============================================}

Constructor TColorRGBA32Item.Create;
Begin
  Inherited Create;
  FName := 'Black';
  FColor.Create(0,0,0);
  FTag := 0;
End;

Destructor TColorRGBA32Item.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TColorRGBA32Item.SetRed(Const AValue: Byte);
Begin
  If AValue = FColor.red Then exit;
  FColor.Red := AValue;
End;

Procedure TColorRGBA32Item.SetGreen(Const AValue: Byte);
Begin
  If AValue = FColor.Green Then exit;
  FColor.Green := AValue;
End;

Procedure TColorRGBA32Item.SetBlue(Const AValue: Byte);
Begin
  If AValue = FColor.Blue Then exit;
  FColor.Blue := AValue;
End;

Procedure TColorRGBA32Item.SetAlpha(Const AValue: Byte);
Begin
  If AValue = FColor.Alpha Then exit;
  FColor.Alpha := AValue;
End;

Procedure TColorRGBA32Item.SetValue(Const AValue: TColorRGBA32);
Begin
  If AValue = FColor Then exit;
  FColor := AValue;
End;

Function TColorRGBA32Item.getRed: Byte;
Begin
  Result := FColor.Red;
End;

Function TColorRGBA32Item.getGreen: Byte;
Begin
  Result := FColor.Green;
End;

Function TColorRGBA32Item.getBlue: Byte;
Begin
  Result := FColor.Blue;
End;

Function TColorRGBA32Item.getAlpha: Byte;
Begin
  Result := FColor.Alpha;
End;

Function TColorRGBA32Item.getValue: TColorRGBA32;
Begin
  Result := FColor;
End;

Procedure TColorRGBA32Item.SetColorName(Const aName: String);
Begin
  If FName = aName Then exit;
  FName := aName;
End;

{%endregion%}

{%region ====[ TColorRGBA32List ]===============================================}

Function TColorRGBA32List.GetColorItem(index: Integer): TColorRGBA32Item;
Begin
  Result := TColorRGBA32Item(Get(Index));
End;

Procedure TColorRGBA32List.SetColorItem(index: Integer; val: TColorRGBA32Item);
Begin
  Put(Index, Val);
End;

procedure TColorRGBA32List.Clear;
Var
  anItem: TColorRGBA32Item;
  i : Integer;
Begin
  inherited Clear;
  If Count > 0 then
  begin
    For i := 0 to Count -1 do
    begin
      AnItem:= Colors[i];
      if anItem<>nil then anItem.Free;
    End;
  End;
End;

Function TColorRGBA32List.AddColor(Const aColor: TColorRGBA32): Integer;
Var
  aColorItem: TColorRGBA32Item;
Begin
  aColorItem := TColorRGBA32Item.Create;
  aColorItem.Value := aColor;
  Result := Add(aColorItem);
End;

Function TColorRGBA32List.AddColor(Const aName: String; Const aColor: TColorRGBA32): Integer;
Var
  aColorItem: TColorRGBA32Item;
Begin
  aColorItem := TColorRGBA32Item.Create;
  aColorItem.Value := aColor;
  aColorItem.Name := aName;
  Result := Add(aColorItem);
End;

Function TColorRGBA32List.AddColor(Const aColorItem: TColorRGBA32Item): Integer;
Begin
  Result := Add(aColorItem);
End;

Procedure TColorRGBA32List.RemoveColor(Const aName: String);
Var
  I:   Integer;
  Col: TColorRGBA32Item;
Begin
  FindColorByName(aName, I);
  If I > -1 Then
  Begin
    Col := GetColorItem(I);
    If Assigned(Col) Then
      Col.Free;
    Delete(I);
  End;
End;

Function TColorRGBA32List.FindColorByName(Const aName: String; Out Index: Integer): TColorRGBA32;
Var
  i: Integer;
Begin
  Result := clrTransparent;
  Index := -1;
  For i := 0 To Count - 1 Do
    If TColorRGBA32Item(Items[i]).Name = aName Then
    Begin
      Index := I;
      Result := TColorRGBA32Item(Items[i]).Value;
      break;
    End;
End;

Function TColorRGBA32List.FindColorByName(Const aName: String): TColorRGBA32;
Var
  i: Integer;
Begin
  Result := FindColorByName(aName, I);
End;

{%endregion%}

{%region=====[ TFastBitmap ]====================================================}

Constructor TFastBitmap.Create(NewWidth, NewHeight : Integer);
Begin
 inherited Create;
  FWidth  := Max(1,NewWidth);
  FHeight := Max(1,NewHeight);
  FData := Nil;
  FSize := (int64(FWidth) * int64(FHeight))*4;
  ReAllocMem(FData,FSize);
  FTransparentColor := clBlack;
End;

Constructor TFastBitmap.Create;
Begin
  Create(1,1);
End;

Destructor TFastBitmap.Destroy;
Begin
  FreeMem(FData);
  FData := Nil;
  inherited Destroy;
End;

Procedure TFastBitmap.SetWidth(NewWidth : Integer);
Begin
  if NewWidth = FWidth then Exit;
  SetSize(NewWidth, FHeight);
End;

Procedure TFastBitmap.SetHeight(NewHeight : Integer);
Begin
  if NewHeight = FHeight then Exit;
  SetSize(FWidth, NewHeight);
End;

Function TFastBitmap.BuildBitmap: Graphics.TBitmap;
Var
  Temp : Graphics.TBitmap;
  RawImage : TRawImage;
  BmpHandle, MskHandle : HBitmap;
  W,H : Integer;
  Buffer : PByte;
Begin
  Result := nil;

  BmpHandle := 0;
  MskHandle := 0;
  W := FWidth;
  H := FHeight;
  Buffer := PByte(GetSurfaceBuffer);

  RawImage.Init;
  {$IFDEF WINDOWS}
  RawImage.Description.Init_BPP32_B8G8R8A8_BIO_TTB(W,H);
  {$ELSE}
  RawImage.Description.Init_BPP32_R8G8B8A8_BIO_TTB(W,H);
  {$ENDIF}

  RawImage.Data := Buffer;
  RawImage.DataSize := FSize;

  if not RawImage_CreateBitmaps(RawImage, BmpHandle, MskHandle,False) then
    Raise Exception.Create('Impossible de créer le TBitmap')
  else
  begin
    Temp := Graphics.TBitmap.Create;
    Temp.Width := W;
    Temp.Height := H;
    Temp.PixelFormat := pf32bit;
    Temp.Handle := BmpHandle;
    Temp.MaskHandle := MskHandle;
    Temp.Transparent := True;
    //Temp.TransparentColor := FTransparentColor;
    //temp.TransparentMode := tmAuto;
    Result := Temp;
  End;
  if Result = nil then
    Raise Exception.Create('Erreur lors de la création du TBitmap');
End;

Function TFastBitmap.IsClipped(X, Y : Integer) : Boolean;
Begin
  Result := ((X>=0) and (Y>=0) and (X<FWidth) and (Y<FHeight));
End;

Procedure TFastBitmap.SwapRB;
var
  Pixptr: PColorRGBA32;
  AIntColor : Cardinal;
  PixelCount : Integer;
begin
  PixPtr := GetSurfaceBuffer;
  PixelCount := (FWidth * FHeight)-1;
  while pixelCount > 0 do
  begin
    AIntColor := PixPtr^.AsInteger;
    PixPtr^.AsInteger := AIntColor and $FF00FF00 or (AintColor and $000000FF SHL 16) or (AIntColor and $00FF0000 SHR 16);
    Inc(PixPtr);
    Dec(pixelCount);
  end;
end;

Procedure TFastBitmap.Assign(aFastBitmap : TFastBitmap);
Begin
  SetSize(aFastBitMap.Width, aFastBitmap.Height);
  Move(PByte(aFastBitmap.GetSurfaceBuffer)^, PByte(FData)^, FSize);
End;

Procedure TFastBitmap.SetSize(NewWidth, NewHeight : Integer);
Begin
  FWidth  := Max(1,NewWidth);
  FHeight := Max(1,NewHeight);
  FSize :=(int64(FWidth) * int64(FHeight))*4;
  if (FData<>nil) then
  begin
    FreeMem(FData);
    FData := Nil;
  End;
  ReAllocMem(FData,FSize);
  Clear(clrTransparent);
End;

Procedure TFastBitmap.Clear(Color : TColorRGBA32);
Begin
  FillDWord(FData^,FWidth * FHeight, DWord(Color));
End;

Function TFastBitmap.GetSurfaceBuffer: PColorRGBA32;
Begin
   Result := PColorRGBA32(FData);
End;

Function TFastBitmap.GetScanLine(Y : Integer) : PColorRGBA32;
Var
  yy : DWord;
Begin
  If (Y<0) or (Y>=FHeight) then
    Raise Exception.Create('Scanline : Indice hors limite')
  else
  begin
    yy := DWord(FWidth) * DWord(Y);
    Result := PColorRGBA32(FData + YY);
  End;
End;

Function TFastBitmap.GetPixelPtr(X, Y : Integer) : PColorRGBA32;
Begin
  Result := nil;
  if IsClipped(X,Y) then
  Begin
    Result := PColorRGBA32(FData + (FWidth * Y) + X);
  End;
End;

Procedure TFastBitmap.PutPixel(X, Y : Integer; Color : TColorRGBA32);
Var
  PixelPtr : PColorRGBA32;
Begin
  if IsClipped(X,Y) then
  Begin
    PixelPtr := PColorRGBA32(FData + DWord(FWidth * Y));
    Inc(PixelPtr,X);
    PixelPtr^:= Color;
  End;
End;

Function TFastBitmap.GetPixel(X, Y : Integer) : TColorRGBA32;
Var
  PixelPtr : PColorRGBA32;
Begin
  Result := clrTransparent;
  if IsClipped(X,Y) then
  Begin
    PixelPtr := PColorRGBA32(FData + (FWidth * Y) + X);
    Result := PixelPtr^;
  End;
End;

Procedure TFastBitmap.PutPixelBlend(X, Y : Integer; Color : TColorRGBA32);
Var
  PixelPtr : PColorRGBA32;
Begin
  if IsClipped(X,Y) then
  Begin
    PixelPtr := PColorRGBA32(FData + (FWidth * Y) + X);
    PixelPtr^:= PixelPtr^.Blend(Color);
  End;
End;

Procedure TFastBitmap.PutImage(Src : TFastBitmap; SrcX, SrcY, SrcWidth, SrcHeight, DstX, DstY : Integer; Mode : TFastBitmapDrawMode);
Var
  SrcPtr, DstPtr : PColorRGBA32;
  NextSrcLine, NextDstLine : Integer;
  DstCol, SrcCol : TColorRGBA32;
  LineSize,TotalSize,xx,yy,i,J : Integer;

  Procedure ClipCopyRect(Var SrcX, SrcY, rWidth, rHeight, DstX, DstY: Integer; SrcImageWidth, SrcImageHeight: Integer; Const DstClip: Types.TRect);
    Var
      diff, OldDstPosX, OldDstPosY: Integer;
    Begin
      OldDstPosX := 0;
      If (DstX < 0) Then OldDstPosX := DstX;
      OldDstPosY := 0;
      If (DstY < 0) Then OldDstPosY := DstY;

      If DstX < DstClip.Left Then
      Begin
        Diff := DstClip.Left - DstX;
        rWidth := rWidth - Diff;
        SrcX := SrcX + Diff;
        DstX := DstClip.Left;
      End;

      If DstY < DstClip.Top Then
      Begin
        Diff := DstClip.Top - DstY;
        rHeight := rHeight - Diff;
        SrcY := SrcY + Diff;
        DstY := DstClip.Bottom;
      End;

      If SrcX < 0 Then
      Begin
        Width := Width + SrcX - OldDstPosX;
        DstX := DstX - SrcX + OldDstPosX;
        SrcX := 0;
      End;
      If SrcY < 0 Then
      Begin
        rHeight := rHeight + SrcX - OldDstPosY;
        DstY := DstY - SrcY + OldDstPosY;
        SrcY := 0;
      End;

      If ((SrcX + rWidth) > SrcImageWidth) Then rWidth := SrcImageWidth - SrcX;
      If ((SrcY + rHeight) > SrcImageHeight) Then rHeight := SrcImageHeight - SrcY;

      if DstX > FWidth then DstX := 0;
      if DstY > FHeight then DstY := 0;

      If ((DstX + rWidth) >  (DstClip.Right+1)) Then rWidth := DstClip.Right - DstX;
      If ((DstY + rHeight) > (DstClip.Bottom+1)) Then rHeight := DstClip.Bottom - DstY;

    End;
Begin
  //ShowMessage('Before clipCopyRect = rWidth : '+SrcWidth.ToString()+' rHeight : '+ SrcHeight.ToString());
  if (SrcWidth = 0) and (SrcHeight = 0) then exit;
  ClipCopyRect(SrcX, SrcY, SrcWidth,SrcHeight, DstX, DstY, Src.Width, Src.Height, Types.Rect(0,0,FWidth-1, FHeight-1));

  //Showmessage('ClipCoryRect = SrcX : '+SrcX.ToString()+' SrcY : '+SrcY.ToString()+#13+#10+
  //                         'rWidth : '+SrcWidth.ToString()+' rHeight : '+ SrcHeight.ToString()+#13+#10+
  //                         'imgWidth : '+Src.Width.ToString()+' imgHeight : '+ Src.Height.ToString()+#13+#10+
  //                         'DstX : '+DstX.ToString()+' DstY : '+DstY.ToString());

  if (SrcWidth = 1) and (SrcHeight = 1) then
  begin
    Case Mode of
      dmSet :
        begin
          SrcCol := Src.GetPixel(0,0);
          PutPixel(0,0,SrcCol);
        End;
      dmAlpha :
      begin
        SrcCol := Src.GetPixel(0,0);
        DstCol := GetPixel(0,0);
        PutPixel(0,0,DstCol.Blend(SrcCol));
      End;
      dmAlphaCheck :
        begin
          If SrcCol.Alpha > 0 Then
          begin
            SrcCol := Src.GetPixel(0,0);
            DstCol := GetPixel(0,0);
            PutPixel(0,0,DstCol.Blend(SrcCol));
          End
          Else
          begin
            DstCol := GetPixel(0,0);
            PutPixel(0,0,DstCol);
          End;
        End;
    End;
    exit;
  End;

  SrcPtr := Src.GetPixelPtr(SrcX,SrcY);
  DstPtr := GetPixelPtr(DstX, DstY);

  if SrcWidth <= Src.Width then
    nextSrcLine := Src.Width
  else
    nextSrcLine := SrcX + (Src.Width - (SrcX + SrcWidth));
  if Mode = dmSet then
  begin
    if (((Src.Width = FWidth) and (Src.Height = FHeight)) and ((SrcWidth = FWidth) and (SrcHeight = FHeight))) then
      Move(SrcPtr^,DstPtr^,DWord(Src.Size))
    else
    begin
      LineSize := SrcWidth * 4;
      For I := 0 to SrcHeight-1 do
      begin
        Move(SrcPtr^, DstPtr^, LineSize);
        Inc(SrcPtr, NextSrcLine);
        Inc(DstPtr, FWidth);
      End;
    End;
  End
  else
  begin
    totalsize := (Src.Width * Src.Height) - 1;
    Dec(SrcHeight);
    xx := 0;
    Dec(SrcWidth);
    nextSrcLine := SrcX + (Src.Width - (SrcX + SrcWidth));
    nextDstLine := DstX + (FWidth - (DstX + SrcWidth));
    yy := 0;
    xx := 0;
    SrcCol := clrTransparent;
    DstCol := clrTransparent;
    While (yy <= TotalSize) Do
    Begin
      DstCol := DstPtr^;
      SrcCol := SrcPtr^;
      Case Mode of
        dmAlpha :
        begin
          DstPtr^ := DstCol.Blend(SrcCol)
        End;
        dmAlphaCheck :
        begin
          If SrcCol.Alpha > 0 Then
          DstPtr^ := DstCol.Blend(SrcCol)
        Else
          DstPtr^ := DstCol;
        End;
      End;
      Inc(xx);
      Inc(yy);
      If (xx > SrcWidth) Then
      Begin
        xx := 0;
        Inc(DstPtr, NextDstLine);
        Inc(SrcPtr, NextSrcLine);
      End
      Else
      Begin
         Inc(SrcPtr);
         Inc(DstPtr);
      End;
    End;
  End;
End;

Function TFastBitmap.Clone : TFastBitmap;
Var
  NewBmp : TFastBitmap;
Begin
 NewBmp := TFastBitmap.Create;
 NewBmp.Assign(Self);
 Result := NewBmp;
End;

Function TFastBitmap.GetBitmap : Graphics.TBitmap;
Begin
  Result := BuildBitmap;
End;

Procedure TFastBitmap.Draw(ACanvas : TCanvas; X, Y : Integer);
Var
  Tmp : Graphics.TBitmap;
Begin
  Tmp :=  BuildBitmap;
  ACanvas.Draw(X,Y,Tmp);
  FreeAndNil(Tmp);
End;

Procedure TFastBitmap.Draw(ACanvas : TCanvas; Rect : TRect);
Var
  Tmp : Graphics.TBitmap;
Begin
  Tmp :=  BuildBitmap;
  ACanvas.StretchDraw(Rect, Tmp);
  FreeAndNil(Tmp);
End;

{%endregion%}
End.


