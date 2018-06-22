Unit uGifViewer;
(*==============================================================================
 DESCRIPTION   : Composant visuel pour l'affichage d'une image animée au format
                 GIF (Graphic Interchange Format)
 DATE          : 17/06/2018
 VERSION       : 0.3
 AUTEUR        : J.Delauney (BeanzMaster)
 CONTRIBUTEURS : Jipete, Jurassik Pork
 LICENCE       : MPL
================================================================================
*)

{$mode objfpc}{$H+}

Interface

Uses
  Types, Classes, SysUtils, Graphics, Math,  Contnrs, dialogs,
  Controls, ExtCtrls,
  Lresources,
  TypesHelpers, uFastBitmap;

{%region=====[ Définitions des types et constantes utiles pour le format GIF ]===================================}
const
  GIF_MaxColors  : integer	= 256;		// Nombre de couleurs maximum supportées. NE PAS TOUCHER A CETTE VALEUR
  GIF_DelayFactor : Integer  = 10;    // Facteur de multiplication pour les délais en ms entre chaque image de l'animation
  GIF_DefaultDelay : Integer = 100;   // 10*10

type
  TGIFVersion = (gvUnknown, gv87a, gv89a);
  TGIFVersionRec = array[0..2] of AnsiChar;

const
  GIFVersions : array[gv87a..gv89a] of TGIFVersionRec = ('87a', '89a');

type
  { En-tête }
  TGIFFileHeader = packed record
    Signature: array[0..2] of AnsiChar; // 'GIF'
    Version: TGIFVersionRec;   // '87a' ou '89a' }
  end;

  { Description globale de l'image }
  TGIFLogicalScreenDescriptorRec = packed record
    ScreenWidth: word;              // Largeur de l'image en pixels
    ScreenHeight: word;             // Hauteur de l'image en pixels
    PackedFields: byte;             // champs compactés
    BackgroundColorIndex: byte;     // Index globale de la couleur de fond
    AspectRatio: byte;              // Ratio d'échelle = (AspectRatio + 15) / 64
  end;

  { Description d'une image }
  TGIFImageDescriptorRec = packed record
    //Separator: byte;  // On lis toujours un byte avant
    Left: word;		      // Colonne en pixels par rapport au bord gauche de l'écran
    Top: word;		      // Rangée en pixels par rapport au haut de l'écran
    Width: word;	      // Largeur de l'image en cours en pixels
    Height: word;	      // Hauteur de l'image en cours pixels
    PackedFields: byte;	// Champs compactés
  end;

  { Graphic Control Extension bloc a.k.a GCE }
  TGIFGraphicControlExtensionRec = packed record
    // BlockSize: byte;           // Normalement toujours 4 octets
    PackedFields: Byte;           // Champs compacté
    DelayTime: Word;              // Délai entre chaque image en centième de secondes
    TransparentColorIndex: Byte;  // Index dans la palette si plus petit ou égale
   // Terminator: Byte;           // Normalement toujours ZERO
  end;

  TGIFDisposalFlag = (dmNone, dmKeep, dmErase, dmRestore); // Methodes pour l'affichage des images lors de l'animation

  { Plain Text Extension }
  TGIFPlainTextExtensionRec = packed record
     // BlockSize: byte;             // Normalement égal à 12 octets
    Left, Top, Width, Height: Word;  // Positions et dimensions du texte
    CellWidth, CellHeight: Byte;     // Dimensions d'une cellule dans l'image
    TextFGColorIndex,                // Index de la couleur de fond dans la palette
    TextBGColorIndex: Byte;          // Index de la couleur du texte dans la palette
  end;

  { Application Extension }
  TGIFApplicationExtensionRec = packed record
    AppID: array [0..7] of AnsiChar;                  // Identification de l'application majoritairement 'NETSCAPE' ou ''
    AppAuthenticationCode: array [0..2] of AnsiChar;  // Code d'authentification ou numero de version
  end;

  { Informations de "l'application extension" si disponible }
  TGIFNSLoopExtensionRec = packed record
    Loops : Word;        // Nombre de boucle de l'animation 0 = infinie
    BufferSize : DWord;  // Taille du tampon. Usage ?????
  End;

const
  // Description des masques pour la description globale de l'image
  GIF_GLOBALCOLORTABLE = $80;       // Défini si la table de couleurs globale suit la description globale
  GIF_COLORRESOLUTION = $70;        // Résolution de la couleur (BitsPerPixel) - 3 bits
  GIF_GLOBALCOLORTABLESORTED = $08; // Définit si la palette globale est triée - 1 bit
  GIF_COLORTABLESIZE = $07;         // Taille de la palette - 3 bits
  GIF_RESERVED		= $0C;            // Réservé - doit être défini avec $00 - Taille des données = 2^value+1 - 3 bits

  // Descption des masques pour les images
  GIF_LOCALCOLORTABLE = $80;       // Défini si la table de couleurs locale suit la description de l'image
  GIF_INTERLACED = $40;            // Défini si l'image est entrelacée
  GIF_LOCALCOLORTABLESORTED= $20;  // Définit si la palette locale est triée

  // Identification des blocs
  GIF_PLAINTEXT = $01;
  GIF_GRAPHICCONTROLEXTENSION = $F9;
  GIF_COMMENTEXTENSION = $FE;
  GIF_APPLICATIONEXTENSION = $FF;
  GIF_IMAGEDESCRIPTOR = $2C;       // ','
  GIF_EXTENSIONINTRODUCER = $21;   // '!'
  GIF_TRAILER = $3B;               // ';'

  // Graphic Control Extension - Définition des masques pour les paramètres
  GIF_NO_DISPOSAL              = $00;  // 0
  GIF_DO_NOT_DISPOSE           = $04;  // 1
  GIF_RESTORE_BACKGROUND_COLOR = $08;  // 2
  GIF_RESTORE_PREVIOUS         = $12;  // 3
  GIF_DISPOSAL_ALL             = $1C;  // bits 2-4 ($1C)
  GIF_USER_INPUT_FLAG          = $02;
  GIF_TRANSPARENT_FLAG         = $01;
  GIF_RESERVED_FLAG            = $E0;

  // Identification des sous-blocs pour "Application Extension"
  GIF_LOOPEXTENSION	= 1;
  GIF_BUFFEREXTENSION	= 2;

const
  GifGCEDisposalModeStr : array[TGIFDisposalFlag] of string = ('None', 'Keep', 'Erase', 'Restore');

Type
   { Informations sur une image de l'animation }
  TGIFFrameInformations = record
    Left, Top,                     // Position de l'image
    Width, Height : Integer;       // Dimension de l'image
    HasLocalPalette : Boolean;     // Palette locale disponible
    IsTransparent : Boolean;         // Image transparente
    UserInput : Boolean;           // Données personnelle
    BackgroundColorIndex: Byte;    // Normalement seulement valide si une palette globale existe
    TransparentColorIndex: Byte;   // Index de la couleur transparente
    DelayTime: Word;               // Délai d'animation
    Disposal: TGIFDisposalFlag;    // Methode d'affichage
    Interlaced : Boolean;          // Image entrelacée
  end;
  PGifFrameInformations = ^TGifFrameInformations;

{%endregion%}

  { TFastMemoryStream }
  { Classe d'aide à la lecture des données dans un flux en mémoire }
  TFastMemoryStream = class
  private
    FBuffer : PByte;
    FPosition : Int64;
    FBytesRead, FBytesLeft, FSize : Int64;
  Public
    Constructor Create(AStream : TStream);
    Destructor Destroy; Override;

    { Lit un Byte dans le flux }
    function ReadByte : Byte;
    { Lit un Word dans le flux }
    function ReadWord : Word;
    { Lit un DWord dans le flux }
    function ReadDWord : DWord;
    { Lit et retourne un tampon "Buffer" de taille "Count" octets }
    function Read(Var Buffer; Count : Int64):Int64;
    { Déplacement dans le flux de "Offset" depuis  "Origin"
      TSeekOrigin =
        - soBeginning : Depuis le début du flux
        - soCurrent   : a partir de la position courante
        - soEnd       : A partir de la fin du flux
    }
    function Seek(Const Offset:Int64; Origin : TSeekOrigin):Int64;
    { Déplacement dans le flux vers l'avant de "Cnt" octet depuis la position courrante }
    procedure SeekForward(Cnt:Integer);
    { Indique si la fin du flux est atteinte (EOS = End Of Stream) }
    function EOS : Boolean;

    { Retourne la taille du flux en octet }
    property Size : Int64 Read FSize;
    { Retourne la position courrante de lecture dans le flux }
    property Position : Int64 Read FPosition;
  End;

  { TGIFLoadErrorEvent : Fonction d'évènement levée en cas d'erreur(s) dans le chargement }
  TGIFLoadErrorEvent = procedure (Sender: TObject;Const ErrorCount : Integer;Const ErrorList:TStringList) of object;

  { TGIFImageListItem }
  { Définition d'une image contenue dans le fichier GIF }
  TGIFImageListItem = Class
   private
     FBitmap : TFastBitmap;
     FDrawMode : TGIFDisposalFlag;
     FLeft, FTop : Integer;
     FComment : TStringList;
     FDelay : Integer;
     FTransparent : Boolean;
   protected
   public
     constructor Create;
     destructor Destroy; override;

     { Objet contenant l'image }
     property Bitmap : TFastBitmap Read FBitmap Write FBitmap;
     { Mode de rendu de l'image }
     property DrawMode : TGIFDisposalFlag Read FDrawMode Write FDrawMode;
     { Position gauche de l'image }
     property Left : Integer Read FLeft Write FLeft;
     { Position Haut de l'image }
     property Top : Integer Read FTop Write FTop;
     { Temps d'attente entre deux image de l'animation }
     property Delay : Integer Read FDelay Write FDelay;
     { Commentaire sur l'image }
     property Comment : TStringList  Read FComment Write FComment;
     { Retourne TRUE si l'image utilise la transparence }
     property IsTransparent : Boolean Read FTransparent Write FTransparent;
   end;

  { TGIFImageList }
  { Classe d'aide à la gestion des images contenues dans le fichier GIF }
  TGIFImageList = class(TObjectList)
   private
   protected
     function GetItems(Index: integer): TGIFImageListItem;
     procedure SetItems(Index: integer; AGifImage: TGIFImageListItem);
   public
     procedure Clear; override;
     { Ajoute une nouvelle image vide à la liste }
     function AddNewImage : TGIFImageListItem;
     { Ajout d'une image dans la liste }
     function Add(AGifImage: TGIFImageListItem): integer;
     { Extraction d'une image de la liste }
     function Extract(Item: TGIFImageListItem):  TGIFImageListItem;
     { Effacement d'une image dans la liste }
     function Remove(AGifImage: TGIFImageListItem): integer;
     { Retourne l'index de l'image recherchée (retourne -1 si non trouvé) }
     function IndexOf(AGifImage: TGIFImageListItem): integer;
     { Retourne la première image }
     function First: TGIFImageListItem;
     { Retourne la dernière image }
     function Last: TGIFImageListItem;
     { Insertion d'une image à la position "Index" }
     procedure Insert(Index: integer; AGifImage: TGIFImageListItem);

     { Liste des images }
     property Items[Index: integer]: TGIFImageListItem Read GetItems Write SetItems; default;
   end;

  { TGIFImageLoader }
  { Classe spécialisée pour la lecture d'une image au format GIF }
  TGIFImageLoader = Class
  private
    FCurrentLayerIndex : Integer;
    FGIFFIleHeader : TGIFFileHeader;
    FLogicalScreenChunk : TGIFLogicalScreenDescriptorRec;
    FHasGlobalPalette : Boolean;
    FTransparent : Boolean;
    FGlobalPalette : TColorRGBA32List;
    FVersion : String;

    FWidth,
    FHeight : Integer;
    FBackgroundColor : TColorRGBA32;

    FFrames : TGIFImageList;

    FErrorList : TStringList;
    FErrorCount : Integer;
    FOnLoadError : TGIFLoadErrorEvent;
    Procedure SetCurrentLayerIndex(AValue : Integer);

  protected
    Memory : TFastMemoryStream;

    CurrentFrameInfos : TGifFrameInformations;

    function GetFrameCount : Integer;
    Procedure LoadFromMemory();
    Function CheckFormat(): Boolean;
    Function ReadImageProperties: Boolean;
    procedure AddError(Msg:String);
    procedure NotifyError;
  public
    Constructor Create;
    Destructor Destroy; Override;

    { LoadFromStream : Charge les données depuis un flux }
    Procedure LoadFromStream(aStream: TStream); Virtual;
    { LoadFromFile : Charge les données depuis un fichier physique }
    Procedure LoadFromFile(Const FileName: String); Virtual;
    { Chargement depuis une Resource Lazarus }
    procedure LoadFromResource(Const ResName : String);
    { Retourne la version du fichier GIF }
    property Version : String Read FVersion;
    { Retourne la largeur de l'image GIF }
    property Width  : Integer Read FWidth;
    { Retourne la hauteur de l'image GIF }
    property Height : Integer Read FHeight;
    { Retourne la couleur de l'image GIF si elle existe,. Sinon retourne une couleur transparente (clrTransparent) }
    property BackgroundColor : TColorRGBA32 Read FBackgroundColor Write FBackgroundColor;
    { Prise en charge de la transparence dans l'image GIF }
    property Transparent : Boolean Read FTransparent Write FTransparent;
    { Retourne l'index courrant de l'image de l'animation traité}
    property CurrentFrameIndex : Integer Read FCurrentLayerIndex Write SetCurrentLayerIndex;
    { Liste des images de l'animation }
    property Frames : TGIFImageList read FFrames;
    { Nombre d'image de l'animation }
    property FrameCount : Integer Read GetFrameCount;
    { Nombre d'erreur produite loars d'un cahrgement ou d'un enregistrement }
    property ErrorCount : Integer Read FErrorCount;
    { Liste des erreurs }
    property Errors : TStringList Read FErrorList;

    { Evenement pour intercepter les erreurs notifiées lors du chargement des données }
    property OnLoadError : TGIFLoadErrorEvent Read FOnLoadError Write FOnLoadError;
  End;

  { TGIFRenderCacheListItem }
  { Définition d'une image cache de l'animation }
  TGIFRenderCacheListItem = class
  private
    FBitmap : Graphics.TBitmap;
    FDelay: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    { Image cache prérendu de l'animation }
    property Bitmap : Graphics.TBitmap Read FBitmap Write FBitmap;
    { Temps d'attente en ms avec l'image suivante }
    property Delay : Integer Read FDelay Write FDelay;
  End;

  { TGIFRenderCacheList }
  { Classe d'aide à la gestion des images rendues de l'animation }
  TGIFRenderCacheList = class(TObjectList)
  private
  protected
    function GetItems(Index: integer):  TGIFRenderCacheListItem;
    procedure SetItems(Index: integer; AGIFRenderCache:  TGIFRenderCacheListItem);
  public
    procedure Clear; Override;
    { Ajoute un nouvel objet cache vide }
    function AddNewCache : TGIFRenderCacheListItem;
    { Ajoute un nouveau cache }
    function Add(AGIFRenderCache: TGIFRenderCacheListItem): integer;
    { Extrait un cache de la liste }
    function Extract(Item: TGIFRenderCacheListItem): TGIFRenderCacheListItem;
    { Supprime un cache de la liste }
    function Remove(AGIFRenderCache: TGIFRenderCacheListItem): integer;
    { Retourne l'index du cache recherchée (retourne -1 si non trouvé) }
    function IndexOf(AGIFRenderCache: TGIFRenderCacheListItem): integer;
    { Retourne le premier élément de la liste }
    function First: TGIFRenderCacheListItem;
    { Retourne le dernier élément de la liste }
    function Last: TGIFRenderCacheListItem;
    { Insertion d'un cache à la position "Index" }
    procedure Insert(Index: integer; AGIFRenderCache: TGIFRenderCacheListItem);

    { Liste des caches }
    property Items[Index: integer]: TGIFRenderCacheListItem Read GetItems Write SetItems; default;
  end;

  { TGIFViewer }
  { Composant visuel pour afficher une image GIF animée }
  TGIFViewer = Class(TGraphicControl)
  private
    FGIFLoader : TGIFImageLoader;
    FFileName : String;

    FRestoreBitmap, FVirtualView : TFastBitmap;

    FRenderCache : TGIFRenderCacheList;
    FCurrentFrameIndex : Integer;
    FGIFWidth, FGIFHeight : Integer;
    FCurrentView : Graphics.TBitmap;

    FAnimateTimer : TTimer;
    FAnimateSpeed : Integer;
    FAnimated, FPause : Boolean;
    FPainting : Boolean;
    FAutoPlay : Boolean;
    FBorderShow : Boolean;
    FBorderColor : TColor;
    FBorderWidth : Byte;
    FBevelInner, FBevelOuter : TPanelBevel;
    FBevelWidth : TBevelWidth;
    FBevelColor : TColor;

    FCenter, FStretch, FTransparent : Boolean;

    FOnStart,FOnStop, FOnPause, FOnFrameChange : TNotifyEvent;
    FOnLoadError : TGIFLoadErrorEvent;

    function  GetCanvas: TCanvas;
    Function GetFrameCount : Integer;
    Function GetGIFVersion: String;
    procedure SetCenter(Const Value : Boolean);
    procedure SetStretch(Const Value : Boolean);
    procedure SetPause(Const Value : Boolean);
    procedure SetFileName(Const Value : String);
    function  GetFrame(Const Index : Integer):Graphics.TBitmap;
    procedure SetTransparent(Const Value : Boolean);
    procedure SetBevelColor(AValue: TColor);
    procedure SetBevelInner(const Value: TPanelBevel);
    procedure SetBevelOuter(const Value: TPanelBevel);
    procedure SetBevelWidth(const Value: TBevelWidth);
  protected
    procedure DoInternalOnLoadError(Sender: TObject;Const ErrorCount : Integer;Const ErrorList:TStringList);
    procedure DoTimerAnimate(Sender:TObject);

    { Rendu d'une image de l'animation }
    function RenderFrame(Index:Integer):TBitmap; virtual;
    { Creation des image cache pour l'animation }
    procedure ComputeCache; virtual;
    { Calcul de la postion et de la dimension pour l'afficchage sur le "Canvas" }
    function DestRect: TRect; virtual;

    { Fonctions hérités }
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; {%H-}WithThemeSpace: Boolean); override;
    class function GetControlClassDefaultSize: TSize; Override;
    procedure Paint; override;
  public
    { Création du composant }
    constructor Create(AOwner: TComponent); override;
    { Destruction du composant }
    destructor Destroy; override;

    { Mise à jour de la surface de dessin (Canvas) du composant }
    procedure Invalidate; override;
    { Chargement depuis un fichier }
    procedure LoadFromFile(Const aFileName : String);
    { Chargement depuis une Resource Lazarus }
    procedure LoadFromResource(Const ResName : String);
    { Joue l'animation }
    procedure Start;
    { Arrête l'animation }
    procedure Stop;
    { Met en pause l'animation }
    procedure Pause;
    { Retourne l'image brute du GIF à la position Index }
    function GetRawFrame(Index:Integer):TBitmap;
    { Affiche l'image de l'animation mise en cache à la position Index }
    procedure DisplayFrame(Index:Integer);
    { Affiche l'image brute de l'animation à la position Index }
    procedure DisplayRawFrame(Index:Integer);

    {  Retourne le Canvas du composant }
    property Canvas: TCanvas read GetCanvas;
    { Retourne TRUE si l'animation est en pause }
    property Paused : Boolean Read FPause;
    { Retourne TRUE si l'animation est en cours }
    property Playing : Boolean Read FAnimated;
    { Retourne l'index actuel de l'image affichée }
    property CurrentFrameIndex : Integer Read FCurrentFrameIndex;
    { Liste des images de l'animation }
    property Frames[Index:Integer]:TBitmap Read GetFrame;
    { Retourne le nombre d'image de l'animation }
    property FrameCount : Integer Read GetFrameCount;
    { Retourne la version du fichier GIF chargé }
    property Version : String Read GetGIFVersion;
    { Image courante de l'animation affichée }
    property CurrentView : Graphics.TBitmap read FCurrentView;
  published
    { Bordure visible autour du composant }
    property Border : Boolean Read FBorderShow Write FBorderShow;
    { Couleur de la bordure }
    property BorderColor : TColor Read FBorderColor Write FBorderColor;
    { Epaisseur de la bordure }
    property BorderWidth : Byte Read FBorderWidth Write FBorderWidth;
    property BevelColor: TColor read FBevelColor write SetBevelColor default clDefault;
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvRaised;
    property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;

    { Joue l'animation automatiquement lors du chargement d'une image GIF animée }
    property AutoPlay : Boolean Read FAutoPlay Write FAutoPlay;
    { Affichage du GIF avec prise en charge de la transparence }
    property Transparent : Boolean Read FTransparent Write SetTransparent;
    { Centrer l'affichage }
    property Center : Boolean Read FCenter Write SetCenter;
    { Redimensionner l'affichage proportionnellement }
    property Stretch : Boolean Read FStretch Write SetStretch;
    { Nom du fichier à charger }
    property FileName : String Read FFileName Write SetFileName;
    { Evènement déclenché lorsque l'animation débute }
    property OnStart : TNotifyEvent Read FOnStart Write FOnStart;
    { Evènement déclenché lorsque l'animation s'arrête }
    property OnStop : TNotifyEvent Read FOnStop Write FOnStop;
    { Evènement déclenché lorsque l'animation est en pause }
    property OnPause : TNotifyEvent Read FOnPause Write FOnPause;
    { Evènement déclenché lorsque une nouvelle image est affiché lors de l'animation }
    property OnFrameChange : TNotifyEvent Read FOnFrameChange Write FOnFrameChange;
    { Evenement pour intercepter les erreurs notifiées lors du chargement des données }
    property OnLoadError : TGIFLoadErrorEvent Read FOnLoadError Write FOnLoadError;

    { Propriétés héritées }
    property Align;
    property Anchors;
    property AutoSize;
    property Constraints;
    property BorderSpacing;
    property Visible;
    property ParentShowHint;
    property ShowHint;
    { Evènements héritées }
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;

  End;

{ Enregistrement du composant et de l'editeur de propriété dans l'EDI }
procedure Register;

Implementation

uses
  GraphType, LazIDEIntf, propedits; // Pour l'integration de l'éditeur de propriété dans l'EDI

{%region=====[ Constantes et types internes ]===================================}

Const
  // Messages d'erreurs ou de notifications
  sScreenBadColorSize	= 'Nombre de couleur dans la palette globale invalide.';
  sImageBadColorSize	= 'Nombre de couleur dans la palette locale invalide.';
  sBadSignature		= 'Signature GIF invalide';
  sBadScreenSize =' Dimension de l''image Invalides.';
  sEmptyColorMap	= 'Erreur aucune palette de couleur disponible pour cette image !';
  sEmptyImage		  = 'L''Image est vide';
  sUnknownVersion = 'Version GIF inconnue';

type
  // Statut de décodage / encodage LZW
  TLZWDecoderStatus = (
    dsOK,                     // Tout va bien
    dsNotEnoughInput,         // Tampon d'entrée trop petit
    dsOutputBufferTooSmall,   // Tampon de sortie trop petit
    dsInvalidInput,           // Donnée corrompue
    dsBufferOverflow,         // débordement de tampon
    dsInvalidBufferSize,      // Taille d'un des tampons invalide
    dsInvalidInputBufferSize, // Taille du tampon d'entrée invalide
    dsInvalidOutputBufferSize,// Taille du tampon de sortie invalide
    dsInternalError           // Erreur interne signifiant qu'il y a un défaut dans le code
  );

{%endregion%}

{%region=====[ Fonctions utiles ]===============================================}

function FixPathDelimiter(S: string):String;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
  begin
    if (Result[I] = '/') or (Result[I] = '\') then
      Result[I] := PathDelim;
  End;
end;

Function CreateFileStream(Const fileName: String; mode: Word = fmOpenRead + fmShareDenyNone): TStream;
var fn : String;
Begin
  fn:=filename;
  FixPathDelimiter(fn);
  If ((mode And fmCreate) = fmCreate) Or FileExists(fn) Then
    Result := TFileStream.Create(fn, mode)
  Else
    Raise Exception.Create('Fichier non trouvé : "' + fn + '"');

End;

{%endregion%}

{%region=====[ TFastMemoryStream ]==============================================}

Constructor TFastMemoryStream.Create(AStream : TStream);
Var
  ms: TMemoryStream;
Begin
    ms := TMemoryStream.Create;
    With ms Do
    Begin
      CopyFrom(aStream, 0);
      Position := 0;
    End;
    FSize := ms.Size;
    FPosition := 0;
    FBytesLeft := FSize;
    FBytesRead := 0;
    FBuffer := Nil;
    ReAllocMem(FBuffer, FSize);
    Move(PByte(ms.Memory)^,FBuffer^,FSize);
    FreeAndNil(ms);
End;

Destructor TFastMemoryStream.Destroy;
Begin
  if FBuffer<>nil then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
  End;
  Inherited Destroy;
End;

Function TFastMemoryStream.ReadByte : Byte;
Begin
  Result := 0;
  if FBytesLeft>0 then
  begin
    Result := PByte(FBuffer + FPosition)^;
    Inc(FPosition);
    Inc(FBytesRead);
    Dec(FBytesLeft);
  End;
End;

Function TFastMemoryStream.ReadWord : Word;
Begin
  Result := 0;
  If (FBytesLeft >= 2) Then
  Begin
    Result := PWord(FBuffer + FPosition)^;
    Inc(FPosition, 2);
    Inc(FBytesRead, 2);
    Dec(FBytesLeft, 2);
  End;
End;

Function TFastMemoryStream.ReadDWord : DWord;
Begin
  Result := 0;
  If (FBytesLeft >= 4) Then
  Begin
    Result := PDWord(FBuffer + FPosition)^;
    Inc(FPosition, 4);
    Inc(FBytesRead, 4);
    Dec(FBytesLeft, 4);
  End;
End;

Function TFastMemoryStream.Read(Var Buffer; Count : Int64) : Int64;
Var
  NumOfBytesToCopy, NumOfBytesLeft: Longint;  //, NumOfBytesRead
  CachePtr, BufferPtr: PByte;
Begin
  Result := 0;

  if (Count>FBytesLeft) then NumOfBytesLeft := FBytesLeft
  else NumOfBytesLeft := Count;

  BufferPtr := @Buffer;

  While NumOfBytesLeft > 0 Do
  Begin
    // On copie les données
    NumOfBytesToCopy := Min(FSize - FPosition, NumOfBytesLeft);
    CachePtr := FBuffer;
    Inc(CachePtr, FPosition);

    Move(CachePtr^, BufferPtr^, NumOfBytesToCopy);
    Inc(Result, NumOfBytesToCopy);
    Inc(FPosition, NumOfBytesToCopy);
    Inc(BufferPtr, NumOfBytesToCopy);
    // On met à jour les marqueur de notre tampon
    Inc(FBytesRead, NumOfBytesToCopy);
    Dec(FBytesLeft, NumOfBytesToCopy);
    Dec(NumOfBytesLeft, NumOfBytesToCopy);

  End;
End;

Function TFastMemoryStream.Seek(Const Offset : Int64; Origin : TSeekOrigin) : Int64;
Var
  NewPos: Integer;
Begin
  // Calcul de la nouvelle position
  Case Origin Of
    soBeginning: NewPos := Offset;
    soCurrent: NewPos := FPosition + Offset;
    soEnd: NewPos := pred(FSize) - Offset;
    Else
      Raise Exception.Create('TFastStream.Seek: Origine Invalide');
  End;
  Result :=NewPos;
  if Offset = 0 then exit;

  FPosition := NewPos;
  FBytesLeft := FSize - FPosition;
  Result := NewPos;
End;

Procedure TFastMemoryStream.SeekForward(Cnt : Integer);
Begin
  Seek(Cnt,soCurrent);
End;

Function TFastMemoryStream.EOS : Boolean;
Begin
  Result := ((FBytesLeft<=0) or (FPosition>=Pred(FSize)));
End;

{%endregion%}

{%region=====[ TGIFImageListItem ]==============================================}

Constructor TGIFImageListItem.Create;
Begin
  FBitmap := TFastBitmap.Create;
  FLeft  := 0;
  FTop   := 0;
  FDelay  := 0;
  FDrawMode := dmNone;
  FComment := TStringList.Create;
End;

Destructor TGIFImageListItem.Destroy;
Begin
  FreeAndNil(FComment);
  FreeAndNil(FBitmap);
  Inherited Destroy;
End;

{%endregion%}

{%region=====[ TGIFImageList ]==================================================}

Function TGIFImageList.GetItems(Index: integer): TGIFImageListItem;
Begin
  Result := TGIFImageListItem(inherited Items[Index]);
End;

Procedure TGIFImageList.SetItems(Index: integer; AGifImage: TGIFImageListItem);
Begin
  Put(Index, AGifImage);
End;

procedure TGIFImageList.Clear;
Var
  anItem: TGIFImageListItem;
  i : Integer;
Begin
  inherited Clear;
  If Count > 0 then
  begin
    For i := 0 to Count -1 do
    begin
      AnItem:= Items[i];
      if anItem<>nil then anItem.Free;
    End;
  End;
End;

Function TGIFImageList.AddNewImage: TGIFImageListItem;
Var
  anItem: TGIFImageListItem;
Begin
  anitem := TGIFImageListItem.Create;
  Add(anItem);
  Result:= Items[Self.Count-1];
End;

Function TGIFImageList.Add(AGifImage: TGIFImageListItem): integer;
Begin
  Result := inherited Add(AGifImage);
End;

Function TGIFImageList.Extract(Item: TGIFImageListItem):  TGIFImageListItem;
Begin
  Result := TGIFImageListItem(inherited Extract(Item));
End;

Function TGIFImageList.Remove(AGifImage: TGIFImageListItem): integer;
Begin
  Result := inherited Remove(AGifImage);
End;

Function TGIFImageList.IndexOf(AGifImage: TGIFImageListItem): integer;
Begin
  Result := inherited IndexOf(AGifImage);
End;

Function TGIFImageList.First: TGIFImageListItem;
Begin
  Result := TGIFImageListItem(inherited First);
End;

Function TGIFImageList.Last: TGIFImageListItem;
Begin
  Result := TGIFImageListItem(inherited Last);
End;

Procedure TGIFImageList.Insert(Index: integer; AGifImage: TGIFImageListItem);
Begin
  inherited Insert(Index, AGifImage);
End;

{%endregion%}

{%region=====[ TGIFImageLoader ]================================================}

Constructor TGIFImageLoader.Create;
Begin
  Inherited Create;
  FFrames := TGIFImageList.Create(True);
  FErrorList := TStringList.Create;
  FErrorCount := 0;
  FGlobalPalette := nil;
  FTransparent := True;
  FBackgroundColor := clrTransparent;
End;

Destructor TGIFImageLoader.Destroy;
Begin
  FreeAndNil(FFrames);
  FreeAndNil(FErrorList);
  Inherited Destroy;
End;

Function TGIFImageLoader.CheckFormat(): Boolean;
Begin
  Result :=  False;
  // Chargement de l'en-tête
  Memory.Read(FGIFFileHeader, SizeOf(TGIFFileHeader));
  // Vérification de quelques paramètres
  Result := uppercase(string(FGIFFileHeader.Signature)) = 'GIF';
  if Result then
  begin
     // Le fichier est valide
     // On sauvegarde la version du GIF
     FVersion :=  string(FGIFFileHeader.Version);
     if (FVersion = GIFVersions[gv87a]) or (FVersion = GIFVersions[gv89a]) then
       Result := ReadImageProperties // On lit les propriétés
     else
       Raise Exception.Create(sUnknownVersion);
  end
  Else
  begin
    // Signature du fichier GIF Invalide. On lève une exception
    Raise Exception.Create(sBadSignature + ' : ' + uppercase(string(FGIFFileHeader.Signature)));
  End;
End;

Function TGIFImageLoader.ReadImageProperties: Boolean;
Begin
  Result :=  False;

  Memory.Read(FLogicalScreenChunk, SizeOf(TGIFLogicalScreenDescriptorRec));

  // On sauvegarde en local les dimensions de l'image, pour plus tard
  FWidth := FLogicalScreenChunk.ScreenWidth;
  FHeight := FLogicalScreenChunk.ScreenHeight;

  if (FWidth<1) or (FHeight<1) then
  begin
    // Dimensions incorrectes on lève une exception
    Raise Exception.Create(sBadScreenSize + ' : ' +FWidth.ToString+'x'+FHeight.ToString);
    exit;
  End;
  FHasGlobalPalette := (FLogicalScreenChunk.PackedFields and GIF_GLOBALCOLORTABLE) <> 0;

  Result := True;
End;

Procedure TGIFImageLoader.AddError(Msg : String);
Begin
  FErrorList.Add(Msg);
End;

Procedure TGIFImageLoader.NotifyError;
Begin
  if FErrorList.Count>0 then
  begin
    if Assigned(FOnLoadError) then FOnLoadError(Self,FErrorList.Count, FErrorList);
  End;
End;

Procedure TGIFImageLoader.LoadFromStream(aStream : TStream);
Begin
  If Memory <> nil then FreeAndNil(Memory);
  Memory := TFastMemoryStream.Create(aStream);
  if CheckFormat then LoadFromMemory;
  FreeAndNil(Memory);
End;

Procedure TGIFImageLoader.LoadFromFile(Const FileName : String);
Var
  Stream : TStream;
Begin
  FErrorList.Clear;
  FErrorCOunt := 0;
  Stream := CreateFileStream(FileName);
  Try
    LoadFromStream(Stream);
  Finally
    FreeAndNil(Stream);
  End;
End;

Procedure TGIFImageLoader.LoadFromResource(Const ResName: String);
Var
  Stream:  TLazarusResourceStream;
Begin
  FErrorList.Clear;
  FErrorCOunt := 0;
  Stream := TLazarusResourceStream.Create(ResName, nil);
  Try
    LoadFromStream(Stream);
  Finally
    FreeAndNil(Stream);
  End;
End;

Function TGIFImageLoader.GetFrameCount: Integer;
Begin
  Result := FFrames.Count;
End;

Procedure TGIFImageLoader.SetCurrentLayerIndex(AValue : Integer);
Begin
  If FCurrentLayerIndex = AValue Then Exit;
  FCurrentLayerIndex := AValue;
End;

Procedure TGIFImageLoader.LoadFromMemory();
Var
  aRGBColor : TColorRGB24;
  aColor : TColorRGBA32;
  PaletteCount : Integer;
  Done : Boolean;
  BlockID : Byte;
  BlockSize : Byte;
  Terminator{%H-} : Byte;
  CurrentLayer : TGIFImageListItem;

  ImageDescriptor : TGIFImageDescriptorRec;
  GraphicControlExtensionChunk : TGIFGraphicControlExtensionRec;
  ApplicationExtensionChunk : TGIFApplicationExtensionRec;
  NSLoopExtensionChunk : TGIFNSLoopExtensionRec;
  PlainTextChunk : TGIFPlainTextExtensionRec;

  LocalPalette : TColorRGBA32List;
  ColorCount : Integer;
  DMode : Byte;
  ret : TLZWDecoderStatus;

  { Chargement palette globale }
  procedure LoadGlobalPalette;
  Var
    J : Byte;
  begin
    If FHasGlobalPalette then
    Begin
      // Remise à zero de la palette globale si elle existe sinon création de celle-ci
      if FGlobalPalette =  nil then FGlobalPalette := TColorRGBA32List.Create
      else FGlobalPalette.Clear;

      PaletteCount :=2 SHL (FLogicalScreenChunk.PackedFields AND GIF_COLORTABLESIZE);
      // Le cas ou le nombre de couleurs serait plus grand que 256. On prend en charge.
      if (PaletteCount<2) then //or (PaletteCount>256) then
         Raise Exception.Create(sScreenBadColorSize+' : '+PaletteCount.ToString);

      // On charge la palette
      For J := 0 to PaletteCount-1 do
      begin
        Memory.Read(aRGBColor, SizeOF(TColorRGB24));
        aColor.Create(aRGBColor);
        FGlobalPalette.AddColor(aColor);
      End;
    End;
  End;
  { Chargement palette locale }
  procedure LoadLocalPalette;
  Var
    J : Byte;
  begin
    // Aucune palette locale n'a été assignée. On en créer une nouvelle. Sinon on efface simplement son contenu.
    if LocalPalette =  nil then LocalPalette := TColorRGBA32List.Create
    else LocalPalette.Clear;

    // On verifie que le nombre de couleur dans la palette est correcte
    ColorCount :=(2 SHL (ImageDescriptor.PackedFields AND GIF_COLORTABLESIZE));
    // Le cas ou le nombre de couleurs serait plus grand que 256. On prend en charge qudn même et on charge la palette.
    if (ColorCount<2) then //or (ColorCount>256) then
       Raise Exception.Create(sImageBadColorSize+' : '+ColorCount.ToString);

    // On charge la palette
    For J := 0 to ColorCount-1 do
    begin
      Memory.Read(aRGBColor, SizeOF(TColorRGB24));
      aColor.Create(aRGBColor);
      LocalPalette.AddColor(aColor);
    End;
  End;
  { Lecture des extensions }
  procedure ReadExtension;
  Var
    ExtensionID, BlockType : Byte;
    BufStr : array[0..255] of char;
    Loops : Word;
  begin
    // On lit les extension jusqu'a ce qu'un bloc de description d'une image soit détecter ou que jusqu'a la fin du fichier
    repeat
      ExtensionID := Memory.ReadByte;
      // Si c'est un  nouveau marqueur d'introduction d'extension. On lit le nouvel ID
      if (ExtensionID = GIF_EXTENSIONINTRODUCER) then ExtensionID := Memory.ReadByte;
      if (ExtensionID = 0) then
      begin
        // On Saute les ID Nul
        Repeat
          ExtensionID:=Memory.ReadByte;
        until  (ExtensionID <> 0);
      End;
      Case ExtensionID of
        GIF_PLAINTEXT :
          begin
            BlockSize := Memory.ReadByte;
            Memory.Read(PlainTextChunk, SizeOf(TGIFPlainTextExtensionRec));
            repeat
              // On lit la taille du bloc. Si Zero alors fin des données de l'extension
              BlockSize := Memory.ReadByte;
              // On lit la chaine de caractères
              if (BlockSize>0) then
              begin
                fillchar({%H-}BufStr,256,0);
                Memory.Read(BufStr,BlockSize);
                BufStr[BlockSize]:=#0;
                // On place le texte dans les commentaires
                CurrentLayer.Comment.Add(String(BufStr));
              End;
            until (BlockSize = 0);
            // On ajoute une ligne vide de séparation
            CurrentLayer.Comment.Add('');
          End;
        GIF_COMMENTEXTENSION :
          begin
            repeat
              // On lit la taille du commentaire. Si Zero alors fin des données de l'extension
              BlockSize := Memory.ReadByte;
              // On lit la chaine de caractères
              if (BlockSize>0) then
              begin
                Memory.Read(BufStr,BlockSize);
                BufStr[BlockSize]:=#0;
                // On place le texte dans les commentaires
                //CurrentLayer.Comment.Add(String(BufStr));
              end;
            Until (BlockSize<=0);
             // On ajoute une ligne vide de séparation
            //CurrentLayer.Comment.Add('');
          End;
        GIF_APPLICATIONEXTENSION:
          begin
            BlockSize := Memory.ReadByte;
            // Certains vieux filtres d'exportation Adobe, ou d'autres logiciels utilisent par erreur une valeur de 10, ou plus petite ou trop grande
            if (BlockSize <> 11) then
            begin
              FillChar(ApplicationExtensionChunk, SizeOf(TGIFApplicationExtensionRec),0);
            End;

            //else if (BlockSize<11) then
            //   RaiseInvalidImageFile(sBadApplicationExtensionBlockSize + ' : ' + BlockSize.ToString+' octets. ( Taille valide = 11 octets )'); }

            Memory.Read(ApplicationExtensionChunk, SizeOf(TGIFApplicationExtensionRec));
            Repeat
               // On lit la taille du  bloc. Zero si il n'y a pas de données supplémentaires
              BlockSize := Memory.ReadByte;
              if (BlockSize>0) then
              begin
                BlockType := Memory.ReadByte;
                Dec(BlockSize);
                Case (BlockType and $07) of
                  GIF_LOOPEXTENSION	:
                    begin
                      // Lecture du nombre de boucle, Si Zero alors boucle infinie
                      Loops := Memory.ReadWord;
                      if Loops > 0 then Inc(NSLoopExtensionChunk.Loops);
                      Dec(BlockSize,SizeOf(Loops));
                    End;
                  GIF_BUFFEREXTENSION	:
                    begin
                      // Lecture de la taille du tampon. Utilisé pour ??????
                      NSLoopExtensionChunk.BufferSize := Memory.ReadDWord;
                      Dec(BlockSize,SizeOF(NSLoopExtensionChunk.BufferSize));
                    End;
                End;
                // On saute et on ignore les donnée non lues
                if (BlockSize>0) then
                begin
                  Memory.SeekForward(BlockSize);
                  BlockSize := 0;
                End;
              End;
            Until (BlockSize=0);
          End;
        GIF_GRAPHICCONTROLEXTENSION :
          begin
            // On lit la taille de l'extension. Normalement 4 Octets. Cette valeur peut-être erronée. On en tient pas compte ici et on lit les données.
            BlockSize := Memory.ReadByte;
            //if BlockSize = 4 then
            //begin
            Memory.Read(GraphicControlExtensionChunk, SizeOf(TGIFGraphicControlExtensionRec));
            // On renseigne notre tampon d'informations pour les prochaines images décodées
            DMode := ((GraphicControlExtensionChunk.PackedFields and GIF_DISPOSAL_ALL) SHR 2);
            With CurrentFrameInfos do
            begin
              // Ces valeurs peuvent être utilisées pour plusieurs image. Elles restent valides jusqu'a la lecture du prochain "GCE" trouvé.
              Disposal    := TGIFDisposalFlag(DMode);
              IsTransparent := (GraphicControlExtensionChunk.PackedFields and GIF_TRANSPARENT_FLAG) <> 0;
              UserInput   := (GraphicControlExtensionChunk.PackedFields and GIF_USER_INPUT_FLAG) <> 0;
              TransparentColorIndex := GraphicControlExtensionChunk.TransparentColorIndex;
              BackgroundColorIndex := FLogicalScreenChunk.BackgroundColorIndex;
              DelayTime   := GraphicControlExtensionChunk.DelayTime;
            End;

            // Lecture de l'octet de fin de l'extension
            Terminator := Memory.ReadByte;
          end;
      End;
    Until  (ExtensionID = GIF_IMAGEDESCRIPTOR) or Memory.EOS;
    // Si l'ID pour la description de l'image est détecter on revient en arrière pour la prise ne charge par le traitement des données
    if (ExtensionID = GIF_IMAGEDESCRIPTOR) then Memory.Seek(-1,soCurrent);
  End;
  { Chargement d'une image }
  procedure LoadImage;
  var
    DecoderStatus{%H-} : TLZWDecoderStatus;
    BufferSize, TargetBufferSize, BytesRead : Int64;
    InitCodeSize : Byte;
    OldPosition : Int64;
    Buffer, BufferPtr : PByte;
    TargetBuffer, TargetBufferPtr : PByte;
    LinePtr : PColorRGBA32;
    Pass, Increment : Byte;
    x : Integer;
    TargetColor : TColorRGBA32;
    ColIdx : Byte;
    CurrentLine : Integer;
    OutBmp : TFastBitmap;

    // Decodeur GIF LZW. Basé sour le code source de la bibliothèque GraphicEX pour Delphi
    function DecodeLZW(var Source, Dest: Pointer; PackedSize, UnpackedSize: Integer):TLZWDecoderStatus;
    const
      { Constantes pour la décompression LZW }
      _LZWGIFCodeBits		= 12;	  // Nombre maximal de bits par code d'un jeton (12 bits = 4095)
      _LZWGIFCodeMax		= 4096; // Nombre maximum de jeton
      _LZWGIFStackSize  = (2 SHL _LZWGIFCodeBits);	 // Taille de la pile de décompression
      _LZWGIFTableSize	= (1 SHL _LZWGIFCodeBits);	 // Taille de la table de décompression


    var
      J: Integer;
      Data,             // Données actuelle
      Bits,             // Compteur de bit
      Code: Cardinal;   // Valeur courrante du Code
      SourcePtr: PByte;
      InCode: Cardinal; // Tampon pour passé le Code

      CodeSize: Cardinal;
      CodeMask: Cardinal;
      FreeCode: Cardinal;
      OldCode: Cardinal;
      Prefix: array[0.._LZWGIFTableSize] of Cardinal; // LZW prefix
      Suffix,                                         // LZW suffix
      Stack: array [0.._LZWGIFStackSize] of Byte;
      StackPointer: PByte;
      MaxStackPointer: PBYte;
      Target: PByte;
      FirstChar: Byte;  // Tampon de décodage d'un octet
      ClearCode,
      EOICode: Word;
      MaxCode: Boolean;

    Begin
      Result := dsOk;
      DecoderStatus := dsOk;
      if (PackedSize <= 0) or (UnpackedSize <= 0) then
      begin
        // Taille des tampons invalides
        if (PackedSize <= 0) and (UnpackedSize <= 0) then result := dsInvalidBufferSize
        else if PackedSize<=0 then result := dsInvalidInputBufferSize
        else if UnpackedSize<=0 then result := dsInvalidOutputBufferSize;
        Exit;
      End;

      // Initialisation  des paramètres pour la décompression
      CodeSize := InitCodeSize + 1;
      ClearCode := 1 shl InitCodeSize;
      EOICode := ClearCode + 1;
      FreeCode := ClearCode +2;
      OldCode := _LZWGIFCodeMax-1;
      CodeMask := (1 shl CodeSize) - 1;
      MaxCode := False;
      Code := 0;
      Target := PByte(Dest);
      SourcePtr := PByte(Source);

      // Initialisation des tables de Code
      for J := 0 to _LZWGIFTableSize do
      begin
        Prefix[J] := _LZWGIFCodeMax;
        Suffix[J] := J;
      end;

      // Initalisation de la pile
      StackPointer := @Stack;
      MaxStackPointer := @Stack[_LZWGIFStackSize];
      FirstChar := 0;

      Data := 0;
      Bits := 0;
      while (UnpackedSize > 0) and (PackedSize > 0) do
      begin
        // On lit le "Code" dans le tampon d'entrée
        Inc(Data, SourcePtr^ shl Bits);
        Inc(Bits, 8);
        while (Bits > CodeSize) and (UnpackedSize > 0) do
        begin
          // Code actuel
          Code := Data and CodeMask;
          // Préparation pour la donnée suivante
          Data := Data shr CodeSize;
          Dec(Bits, CodeSize);

          // Décompression finie ?
          if Code = EOICode then
          begin
            // Si nous arrivons ici, il y a probablement quelque chose de suspect avec l'image GIF
            // Car normalement on stoppe dès que le tampon de sortie est plein.
            // Cela signifie que nous ne lirons jamais l'EOICode de fermeture dans les images normales.
            // Comme l'état du buffer est déjà vérifié après la boucle principale, nous ne le ferons pas ici.
            Break;
          end;

          // On vérifie s'il s'agit d'un code valide déjà enregistré
          if Code > FreeCode then
          begin
             // Code ne peux à être supérieur à FreeCode. Nous avons donc une image cassée.
             // On notifie l'erreur à l'utilisateur. Et on considère qu'il n'ya pas d'erreur.
             DecoderStatus := dsInvalidInput;
             AddError('Image #'+CurrentFrameIndex.ToString+' : Le décodeur a rencontré une entrée invalide (données corrompues)');
             //NotifyUser('Le décodeur a rencontré une entrée invalide (données corrompues)');
             Code := ClearCode;
            //Break; //Ici, on continue le chargement du reste de l'image au lieu de le stopper
          end;

          // RAZ
          if Code = ClearCode then
          begin
            // réinitialisation de toutes les variables
            CodeSize := InitCodeSize + 1;
            CodeMask := (1 shl CodeSize) - 1; //CodeMasks[CodeSize];
            FreeCode := ClearCode + 2;
            OldCode := _LZWGIFCodeMax;
            MaxCode := False;
          end
          else if OldCode = _LZWGIFCodeMax then
          begin
            // Gestion du premier Code LZW : On le définit dans le tampon de sortie et on le conserve
            FirstChar := Suffix[Code];
            Target^ := FirstChar;
            Inc(Target);
            Dec(UnpackedSize);
            OldCode := Code;
          end
          else
          begin
            //On conserve le Code LZW actuel
            InCode := Code;

            // On place le nouveau code LZW sur la pile sauf quand nous avons déjà utilisé tous les codes disponibles
            if (Code = FreeCode) and not MaxCode then
            begin
              StackPointer^ := FirstChar;
              Inc(StackPointer);
              Code := OldCode;
            end;

            // boucle pour placer les octets décodés sur la pile
            while Code > ClearCode do
            begin
              StackPointer^ := Suffix[Code];
              if StackPointer >= MaxStackPointer then
              begin
                // Ne doit jamais arriver, c'est juste une précaution au cas ou.
                Result := dsBufferOverflow;
                break;
              end;
              Inc(StackPointer);
              Code := Prefix[Code];
            end;
            if Result <> dsOK then break; // Si il ya eu des erreurs on ne va pas plus loin

            // Place le nouveau Code dans la table
            FirstChar := Suffix[Code];
            StackPointer^ := FirstChar;
            Inc(StackPointer);

            //Transfert des données décodées vers notre tampon de sortie
            repeat
              if UnpackedSize <= 0 then
              begin
                // Le tampon de sortie est trop petit. On ne va pas plus loin
                // On notifie l'erreur à l'utilisateur. Et on considère qu'il n'ya pas d'erreur.
                // Afin de pouvoir afficher le GIF et continuer le chargement des images suivantes
                Result := dsOutputBufferTooSmall;
                AddError('Image #'+CurrentFrameIndex.ToString+' : Le décodeur n''a pas pu décoder toutes les données car le tampon de sortie est trop petit');
                break;
              end;
              Dec(StackPointer);
              Target^ := StackPointer^;
              Inc(Target);
              Dec(UnpackedSize);
            until StackPointer = @Stack;
            if Result <> dsOK then break;

            if not MaxCode then
            begin
              if FreeCode<=_LZWGIFCodeMax then
              begin
                Prefix[FreeCode] := OldCode;
                Suffix[FreeCode] := FirstChar;
              end
              else  if FreeCode>_LZWGIFCodeMax then
              begin
                // On a intercepter une donnée corrompue. On continue quand la même décompression sans en tenir compte.
                // On notifie juste l'erreur à l'utilisateur
                DecoderStatus := dsInvalidInput;
                AddError('Image #'+CurrentFrameIndex.ToString+' : Le décodeur a rencontré une entrée invalide (données corrompues)');
                //NotifyUser('Le décodeur a rencontré une entrée invalide (données corrompues)');
                MaxCode := true;
              end;

              // On augmente la taille du Code si nécessaire
              if (FreeCode = CodeMask) and not(MaxCode) then
              begin
                if (CodeSize < _LZWGIFCodeBits) then
                begin
                  Inc(CodeSize);
                  CodeMask := (1 shl CodeSize) - 1;//CodeMasks[CodeSize];
                end
                else //On a atteind la limite maximum
                  MaxCode := True;
              end;

              if FreeCode < _LZWGIFTableSize then Inc(FreeCode);
            end;
            OldCode := InCode;
          end;
        end;
        Inc(SourcePtr);
        Dec(PackedSize);
        if (Result <> dsOK) or (Code = EOICode) then Break;
      end;

      if Result = dsOK then
      begin
        // On vérifie seulement si il n'ya pas eu d'erreur. Si ce n'est pas le cas, nous savons déjà que quelque chose ne va pas.
        // Notez qu'il est normal que PackedSize soit un peu> 0 parce que nous pouvons
        // pas lire l'EOICode mais arrêter dès que notre tampon de sortie est plein et
        // qui devrait normalement être le code juste avant l'EOICode.
        if PackedSize < 0 then
        begin
          Result := dsInternalError;
          // C'est une erreur sérieuse : nous avons eu un dépassement de tampon d'entrée que nous aurions dû intercepter. Nous devons arrêter maintenant.
          Raise Exception.Create('Dépassement du buffer d''entrée dans le décodeur GIF LZW. Signaler ce bug. C''est un bug sérieux !');
          Exit;
        end;
        if UnpackedSize <> 0 then
        begin
          //if UnpackedSize > 0 then
          //begin
          //  //  Image corrompue
          //  DecoderStatus := dsNotEnoughInput;
          //  AddError('Image #'+CurrentFrameIndex.ToString+' : Le décodeur n''a pas pu décoder toutes les données car le tampon d''entrée est trop petit');
          //  //NotifyUser('Le décodeur  n''a pas pu décoder toutes les données car le tampon d''entrée est trop petit');
          //End
          //else
          if UnpackedSize < 0 then
          begin
              Result := dsInternalError;
              // C'est une erreur sérieuse : nous avons eu un dépassement de tampon de sortie que nous aurions dû intercepter. Nous devons arrêter maintenant.
              Raise Exception.Create('Dépassement du buffer de sortie dans le décodeur GIF LZW. Signaler ce bug. C''est un bug sérieux !');
          end;
        end;
      end
    end;

  begin
    BufferSize := 0;
    TargetBufferSize := 0;

    // On lit la description de l'image
    Memory.Read(ImageDescriptor, SizeOf(TGIFImageDescriptorRec));

    // On vérifie que les dimensions sont correctes.
    // Si on trouve des dimensions à zero, il se peut qu'il faudra traiter
    // une extension PlainText et dessiner ce texte en fonction des paramètres
    if (ImageDescriptor.Height = 0) or (ImageDescriptor.Width = 0) then
    begin
      // On assigne les dimensions par défaut du GIF
      ImageDescriptor.Width := FLogicalScreenChunk.ScreenWidth;
      ImageDescriptor.Height := FLogicalScreenChunk.ScreenHeight;
      // On notifie à l'utilisateur que les dimensions de l'image sont erronée. Mais on tente le chargement quand même
      // ShowMessage
    end;

    // Dans le cas ou les dimensions de l'image sont incorrectes dans "l'image descriptor". Ou que la taille des données compressées soit erronée.
    if (ImageDescriptor.Width > FLogicalScreenChunk.ScreenWidth) or  (ImageDescriptor.Height > FLogicalScreenChunk.ScreenHeight) then
    begin
      // On assigne les dimensions par défaut du GIF
      if (ImageDescriptor.Width > FLogicalScreenChunk.ScreenWidth) then ImageDescriptor.Width := FLogicalScreenChunk.ScreenWidth;
      if (ImageDescriptor.Height > FLogicalScreenChunk.ScreenHeight) then ImageDescriptor.Height := FLogicalScreenChunk.ScreenHeight;
      // On notifie à l'utilisateur que les dimensions de l'image sont erronée. Mais on tente le chargement quand même
      // ShowMessage
    end;

    // On renseigne notre tampon d'informations
    With CurrentFrameInfos do
    begin
      Left       := ImageDescriptor.Left;
      Top        := ImageDescriptor.Top;
      Width      := ImageDescriptor.Width;
      Height     := ImageDescriptor.Height;
      Interlaced := (ImageDescriptor.PackedFields And GIF_INTERLACED) = GIF_INTERLACED;
      HasLocalPalette := (ImageDescriptor.PackedFields And GIF_LOCALCOLORTABLE) = GIF_LOCALCOLORTABLE;
    End;

    // L'image possède-t-elle sa propre palette de couleur ? Si oui on la charge.
    If CurrentFrameInfos.HasLocalPalette then LoadLocalPalette;

    // Decompression de l'image

    // On ajoute une nouvelle image si besoin
    if (FCurrentLayerIndex>0) and (FCurrentLayerIndex>FFrames.Count-1) then CurrentLayer := FFrames.AddNewImage;
    // On assigne la nouvelle image au Bitmap de travail
    OutBmp := FFrames.Items[CurrentFrameIndex].Bitmap;

    // On met à jour les informations
    With FFrames.Items[FCurrentLayerIndex] do
    begin
      Drawmode := CurrentFrameInfos.Disposal;
      Left := CurrentFrameInfos.Left;
      Top  := CurrentFrameInfos.Top;
      IsTransparent := CurrentFrameInfos.IsTransparent;
      if CurrentFrameInfos.DelayTime = 0 then Delay:= GIF_DefaultDelay
      else Delay := CurrentFrameInfos.DelayTime * GIF_DelayFactor;
    end;

    // On lit le code d'initalisation de la compression LZW
    InitCodeSize := Memory.ReadByte;
    if InitCodeSize<2 then InitCodeSize := 2;
    if InitCodeSize>8 then InitCodeSize := 8;

    // On sauve la position actuelle dans le flux
    OldPosition := Memory.position;

    BufferSize := 0;

    // 1) On comptabilise la taille totale des données compresser. Afin de les décompresser en une seule fois.
    // On lit la taille du premier bloc
    BlockSize := Memory.ReadByte;
    While (BlockSize>0) and not(Memory.EOS)  do
    begin
      Inc(BufferSize,BlockSize);
      // On saute les données
      Memory.SeekForward(BlockSize);
      if not(Memory.EOS) then BlockSize := Memory.ReadByte else blocksize :=0;
    end;

    // 2) On initalise notre bitmap avec les bonnes dimensions
    OutBmp.SetSize(CurrentFrameInfos.Width, CurrentFrameInfos.Height);

    BufferPtr := nil;
    Buffer := nil;
    // 3) On alloue notre tampon pour les données compressées
    if (BufferSize>0) then Reallocmem(Buffer,BufferSize);

    // 4) On charge toutes les données dans notre tampon
    // On se replace au début des données
    Memory.Seek(OldPosition, soBeginning);
    // On travail toujours sur une copie du "pointer"
    BufferPtr := Buffer;
    // On lit la taille du premier bloque
    BlockSize := Memory.ReadByte;
    While (BlockSize>0) and not(Memory.EOS) do
    begin
      // On charge les données dans le tampon. On previent des erreurs en cas de dépassements
      BytesRead := Memory.Read(BufferPtr^,BlockSize);
      Inc(BufferPtr,BytesRead);
      if not(Memory.EOS) then BlockSize := Memory.ReadByte else blocksize :=0;
    end;
    // On se replace au debut du tampon
    BufferPtr := Buffer;
    // 5) On decompresse les données
    //  On initialise notre buffer ou seront décompressées les données
    TargetBufferSize := int64(CurrentFrameInfos.Width)*Int64(CurrentFrameInfos.Height);
    TargetBufferPtr := nil;
    TargetBuffer := nil;
    // Si la taille est plus grande que zero, on alloue l'espace nécessaire à notre tampon
    if (TargetBufferSize>0) then Reallocmem(TargetBuffer,TargetBufferSize);

    // Décodage des données compressées
    Ret := DecodeLZW(Buffer,TargetBuffer,BufferSize,TargetBufferSize);

    // 6) On transfert les données de l'image vers notre bitmap. Si il n'y a pas eu d'erreurs
    if (Ret=dsOk)  then
    begin
      TargetBufferPtr := TargetBuffer;
      OutBmp.Clear(clrTransparent);

      // Image non entrelacée
      if not(CurrentFrameInfos.Interlaced) then
      begin
        CurrentLine := 0;
        While (CurrentLine<CurrentFrameInfos.Height) do
        begin
          LinePtr :=OutBmp.GetScanLine(CurrentLine);// FFrames.Items[CurrentFrameIndex].Bitmap.GetScanLine(CurrentLine);
          For x:=0 to OutBmp.Width-1 do
          begin
            // Lecture de l'index de la couleur dans la palette
            ColIdx := TargetBufferPtr^;
            // On utilise la palette de couleur locale
            if CurrentFrameInfos.HasLocalPalette then
            begin
              if LocalPalette<>nil then // La palette est-elle chargée ?
              begin
                //if (ColIdx> ColorCount-1) then ColIdx := ColorCount -1;
                if (ColIdx< ColorCount) then
                  TargetColor := LocalPalette.Colors[ColIdx].Value
                else TargetColor := clrTransparent;
              end
              else if FGlobalPalette<>nil then // Non, alors on utilise la palette globale si elle est présente
              begin
                //if (ColIdx> PaletteCount-1) then ColIdx := PaletteCount -1;
                if (ColIdx< PaletteCount) then
                  TargetColor := FGlobalPalette.Colors[ColIdx].Value
                else TargetColor := clrTransparent;
              end
              else
              begin
                AddError(sEmptyColorMap);
                Exit;
              end;
            End
            else // On utilise la palette de couleur globale
            begin
              if FGlobalPalette<>nil then
              begin
                //if (ColIdx> PaletteCount-1) then ColIdx := PaletteCount -1;
                if (ColIdx< PaletteCount) then
                  TargetColor := FGlobalPalette.Colors[ColIdx].Value
                else TargetColor := clrTransparent;
              end
              else if LocalPalette<>nil then
              begin
                //if (ColIdx> ColorCount-1) then ColIdx := ColorCount -1;
                if (ColIdx> ColorCount-1) then //ColIdx := ColorCount -1;
                  TargetColor := LocalPalette.Colors[ColIdx].Value
                else TargetColor := clrTransparent;
              end
              else
              begin
                AddError(sEmptyColorMap);
                Exit;
              end;
            end;

            if CurrentFrameInfos.IsTransparent then
            begin
              if FHasGlobalPalette then
                  if ColIdx< FGlobalPalette.Count then OutBmp.TransparentColor := FGlobalPalette.Colors[ColIdx].Value.ToColor
                else
                  if ColIdx< LocalPalette.Count then OutBmp.TransparentColor := LocalPalette.Colors[ColIdx].Value.ToColor;

              if (Self.FTransparent) then
              begin
                if (ColIdx = CurrentFrameInfos.TransparentColorIndex) then TargetColor.Alpha := 0; //clrTransparent ;
                if (CurrentFrameInfos.TransparentColorIndex = CurrentFrameInfos.BackgroundColorIndex) then FbackgroundColor.Alpha := 0; //clrTransparent; //FBackgroundColor.Alpha := 0;
              End;
            end;
            LinePtr^ := TargetColor;
            // On avance de 1 élément dans nos "pointer"
            inc(TargetBufferPtr);
            inc(LinePtr);
          End;
          Inc(CurrentLine);
        End;
      End
      else // Image entrelacée
      begin
        CurrentLine := 0;
        For pass:=0 to 3 do
        begin
          Case Pass of
            0 :
              begin
                CurrentLine :=  0;
                Increment := 8;
              End;
            1 :
              begin
                CurrentLine :=  4;
                Increment := 8;
              End;
            2 :
              begin
                CurrentLine :=  2;
                Increment := 4;
              End;
            else
              begin
                CurrentLine :=  1;
                Increment := 2;
              End;
          End;
          While (CurrentLine<CurrentFrameInfos.Height) do
          begin
            LinePtr := FFrames.Items[CurrentFrameIndex].Bitmap.GetScanLine(CurrentLine);
            For x:=0 to Pred(FFrames.Items[CurrentFrameIndex].Bitmap.Width) do
            begin
              // Lecture de l'index de la couleur dans la palette
              ColIdx := TargetBufferPtr^;
              // On utilise la palette de couleur locale
              if CurrentFrameInfos.HasLocalPalette then
              begin
                if LocalPalette<>nil then // La palette est-elle chargée ?
                begin
                  if (ColIdx< ColorCount) then // Dans le cas contraire il s'agit d'un index pour la transparence
                    TargetColor := LocalPalette.Colors[ColIdx].Value;
                end
                else if FGlobalPalette<>nil then // Non, alors on utilise la palette globale si elle est présente
                begin
                  if (ColIdx< PaletteCount) then //if (ColIdx< PaletteCount-1) then ColIdx := PaletteCount -1;
                    TargetColor := FGlobalPalette.Colors[ColIdx].Value;
                end
                else
                begin
                  AddError(sEmptyColorMap);
                  Exit;
                end;
              End
              else // On utilise la palette de couleur globale
              begin
                if FGlobalPalette<>nil then
                begin
                  if (ColIdx> PaletteCount-1) then ColIdx := PaletteCount -1;
                  TargetColor := FGlobalPalette.Colors[ColIdx].Value;
                end
                else if LocalPalette<>nil then
                begin
                  if (ColIdx> ColorCount-1) then ColIdx := ColorCount -1;
                  TargetColor := LocalPalette.Colors[ColIdx].Value;
                end
                else
                begin
                  AddError(sEmptyColorMap);
                  Exit;
                end;
              end;

              if CurrentFrameInfos.IsTransparent then
              begin
                if FHasGlobalPalette then
                    if ColIdx< FGlobalPalette.Count then OutBmp.TransparentColor := FGlobalPalette.Colors[ColIdx].Value.ToColor
                  else
                    if ColIdx< LocalPalette.Count then OutBmp.TransparentColor := LocalPalette.Colors[ColIdx].Value.ToColor;

                if (FTransparent) then
                begin
                  if CurrentFrameInfos.TransparentColorIndex = colIdx then TargetColor.Alpha := 0; // := clrTransparent;
                  if (CurrentFrameInfos.TransparentColorIndex = CurrentFrameInfos.BackgroundColorIndex) then FBackgroundColor.Alpha := 0;
                End;

              end;

              LinePtr^ := TargetColor;
              inc(TargetBufferPtr);
              if (CurrentLine<CurrentFrameInfos.Height-1) then inc(LinePtr);
            End;
            Inc(CurrentLine, Increment);
          end;
        End;
      End;
      inc(FCurrentLayerIndex);   // Index pour la prochaine image
    End
    else
    begin
      Case Ret of
        dsInvalidBufferSize : AddError('Image #'+CurrentFrameIndex.ToString+' : La taille du tampon d''entrée et de sortie sont invalides ( Taille <= 0)');
        dsInvalidInputBufferSize : AddError('Image #'+CurrentFrameIndex.ToString+' : La taille du tampon d''entrée est invalide ( Taille <= 0)');
        dsInvalidOutputBufferSize : AddError('Image #'+CurrentFrameIndex.ToString+' : La taille du tampon de sortie est invalide ( Taille <= 0)');
        dsBufferOverflow : AddError('Image #'+CurrentFrameIndex.ToString+' : Le décodeur s''est arrêté pour empêcher un débordement de tampon');
       (* dsOutputBufferTooSmall :
          begin
            // On supprime l'image. Le tampon de sortie étant trop petit, cela va générer des erreurs lors du transfert des données décompressées vers l'image
            //FFrames.Delete(CurrentFrameIndex);
            dec(FCurrentLayerIndex);
          end;*)
      end;
    end;

    // On libére la mémoire allouée pour nos tampons
    if (TargetBufferSize>0) and (targetBuffer<>nil) then FreeMem(TargetBuffer);
    if (BufferSize>0) and (Buffer<>nil) then FreeMem(Buffer);

    //if not(ImageFound) then ImageFound := (FCurrentLayerIndex >0);
  End;

Begin
  PaletteCount := 0;
  ColorCount := 0;
  LocalPalette := nil;
  FFrames.Clear;

  // Par defaut, on considère que la couleur de fond est totalement transparente
  FBackgroundColor := clrTransparent;
  // Si une palette globale existe, alors on charge
  LoadGlobalPalette;
  If FHasGlobalPalette then
  begin
    if FLogicalScreenChunk.BackgroundColorIndex < PaletteCount-1 then
      FBackgroundColor := FGlobalPalette.Colors[FLogicalScreenChunk.BackgroundColorIndex].Value
    else
    begin
      FBackgroundColor := clrTransparent; //FGlobalPalette.Colors[FLogicalScreenChunk.BackgroundColorIndex].Value;
    end;
  End;

  // Les valeurs suivante seront renseignées lors du chargement d'une image
  // On réinitialise juste les valeurs par défaut des informations de l'image en cours au cas ou il n'y aurait pas de GCE
  With CurrentFrameInfos do
  begin
    Left       := 0;
    Top        := 0;
    Width      := FLogicalScreenChunk.ScreenWidth;
    Height     := FLogicalScreenChunk.ScreenHeight;
    Interlaced := False;
    HasLocalPalette := False;
    IsTransparent := false;
  End;
  // On ajoute l'image de départ afin de pouvoir assigner les valeurs des premières extensions (Extensions déclarées avant l'image)
  CurrentLayer := FFrames.AddNewImage;
  // On efface l'image avec la couleur de fond
  //CurrentLayer.Bitmap.Clear(FBackgroundColor);
  FCurrentLayerIndex := 0;
  // On lit le 1er octet
  Done := False;
  While not(Done) do
  begin
    // On verifie l'existence d'extensions avant les données de l'image (Application, Graphic Control, PlainText, Comment)
    if not(Memory.EOS) then BlockID := Memory.ReadByte else BlockID := GIF_Trailer;
    if (BlockID = GIF_Trailer)  then
    begin
      Done := True;
    End;
    if (BlockID = 0) then
    begin
      // On Saute les ID Nul
      While (BlockId = 0) do
        BlockId:=Memory.ReadByte;
    End
    else if (BlockID =  GIF_IMAGEDESCRIPTOR) then  // C'est une image
    begin
      // On charge l'image
      LoadImage;
    End
    else if (BlockID = GIF_EXTENSIONINTRODUCER) then // c'est une extension
    begin
      ReadExtension; // On charge toutes les extensions qui sont à la suite
    End
    else
    begin
      // Extension inconnue on saute jusqu'a trouver un ZERO.
      // A Verifier avec le flag UseInput dans le "Graphic Control Extension"
      // Ici on ignore simplement les données
      While BlockID<>0 do
      begin
        BlockID := Memory.ReadByte;
      End;
    End;
  End;
  // Si il y a des erreurs elles seront notifier à l'utilisateur
  NotifyError;

  // Il n'y a aucune images on notifie l'erreur
  if FFrames.Count=0 then Raise Exception.Create(sEmptyImage);

  // On libere la mémoire, prise par nos palettes de couleurs si besoin
  if (LocalPalette<>nil) then
  begin
    FreeAndNil(LocalPalette);
  End;
  if (FGlobalPalette<>nil) then
  begin
    FreeAndNil(FGlobalPalette);
  End;
End;

{%endregion%}

{%region=====[ TGIFRenderCacheListItem ]========================================}

Constructor TGIFRenderCacheListItem.Create;
Begin
  inherited Create;
 // FBitmap := Graphics.TBitmap.Create;
  FBitmap := Nil;
  FDelay := 0;
End;

Destructor TGIFRenderCacheListItem.Destroy;
Begin
  if FBitmap<>nil then FreeAndNil(FBitmap);
  Inherited Destroy;
End;

{%endregion%}

{%region=====[ TGIFRenderCacheList ]============================================}

Function TGIFRenderCacheList.GetItems(Index: integer): TGIFRenderCacheListItem;
Begin
  Result := TGIFRenderCacheListItem(inherited Items[Index]);
End;

Procedure TGIFRenderCacheList.SetItems(Index: integer; AGIFRenderCache: TGIFRenderCacheListItem);
Begin
  Put(Index, AGIFRenderCache);
End;

procedure TGIFRenderCacheList.Clear;
Var
  anItem: TGIFRenderCacheListItem;
  i : Integer;
Begin
  If Count > 0 then
  begin
    For i := 0 to Count -1 do
    begin
      AnItem:= Items[i];
      if anItem<>nil then anItem.Free;
    End;
  End;
  inherited Clear;
End;

Function TGIFRenderCacheList.AddNewCache: TGIFRenderCacheListItem;
Var
  anItem: TGIFRenderCacheListItem;
Begin
  anitem := TGIFRenderCacheListItem.Create;
  Add(anItem);
  Result:= Items[Self.Count-1];
End;

Function TGIFRenderCacheList.Add(AGIFRenderCache: TGIFRenderCacheListItem): integer;
Begin
  Result := inherited Add(AGIFRenderCache);
End;

Function TGIFRenderCacheList.Extract(Item: TGIFRenderCacheListItem): TGIFRenderCacheListItem;
Begin
  Result := TGIFRenderCacheListItem(inherited Extract(Item));
End;

Function TGIFRenderCacheList.Remove(AGIFRenderCache : TGIFRenderCacheListItem): integer;
Begin
  Result := inherited Remove(AGIFRenderCache);
End;

Function TGIFRenderCacheList.IndexOf(AGIFRenderCache : TGIFRenderCacheListItem): integer;
Begin
  Result := inherited IndexOf(AGIFRenderCache);
End;

Function TGIFRenderCacheList.First: TGIFRenderCacheListItem;
Begin
  Result := TGIFRenderCacheListItem(inherited First);
End;

Function TGIFRenderCacheList.Last: TGIFRenderCacheListItem;
Begin
  Result := TGIFRenderCacheListItem(inherited Last);
End;

Procedure TGIFRenderCacheList.Insert(Index: integer; AGIFRenderCache: TGIFRenderCacheListItem);
Begin
  inherited Insert(Index, AGIFRenderCache);
End;

{%endregion%}

{%region=====[ TGIFViewer ]=====================================================}

Constructor TGIFViewer.Create(AOwner : TComponent);
Begin
  inherited Create(AOwner);
  ControlStyle:= [csCaptureMouse, csClickEvents, csDoubleClicks];
  AutoSize := False;
  FCenter := False;
  FStretch := False;
  FTransparent := True;
  with GetControlClassDefaultSize do SetInitialBounds(0, 0, CX, CY);
  FRestoreBitmap := nil;
  FRenderCache := TGIFRenderCacheList.Create(False);
  FGIFLoader := TGIFImageLoader.Create;
  FGIFLoader.OnLoadError := @DoInternalOnLoadError;
  FVirtualView := TFastBitmap.Create;
  FCurrentView := nil;
  FCurrentView := Graphics.TBitmap.Create;
  FRestoreBitmap := nil;
  FAutoPlay := False;
  FBorderShow := False;
  FBorderColor := clBlack;
  FBorderWidth := 1;
  FAnimateTimer := TTimer.Create(nil);
  with FAnimateTimer do
  begin
    Enabled := False;
    Interval := 1000;
    OnTimer := @DoTimerAnimate;
  End;
  FAnimateSpeed := 12;
  FCurrentFrameIndex := 0;
  FGIFWidth := 90;
  FGIFHeight := 90;
End;

Destructor TGIFViewer.Destroy;
Begin
  FAnimateTimer.Enabled := False;

  FreeAndNil(FAnimateTimer);
  if FCurrentView<>nil then FreeAndNil(FCurrentView);
  if FRestoreBitmap<>nil then FreeAndNil(FRestoreBitmap);
  FreeAndNil(FVirtualView);
  FRenderCache.Clear;
  FreeAndNil(FRenderCache);
  FreeAndNil(FGIFLoader);

  Inherited Destroy;
End;

Procedure TGIFViewer.SetCenter(Const Value : Boolean);
Begin
  if Value = FCenter then exit;
  FCenter := Value;
  Invalidate;
End;

Function TGIFViewer.GetCanvas : TCanvas;
Begin
  Result :=Inherited Canvas;// FCurrentView.Canvas
End;

Function TGIFViewer.GetFrameCount : Integer;
Begin
  Result := FRenderCache.Count;//FGifLoader.FrameCount;
end;

Function TGIFViewer.GetGIFVersion: String;
Begin
  Result := FGIFLoader.Version;
end;

Procedure TGIFViewer.SetStretch(Const Value : Boolean);
Begin
  if Value = FStretch then exit;
  FStretch := Value;
  Invalidate;
End;

Procedure TGIFViewer.SetPause(Const Value : Boolean);
Begin
  if Value = FPause then exit;
  FPause := Value;
  if FPause then FAnimateTimer.Enabled := False;
  If Assigned(FOnPause) then FOnPause(Self);
End;

Procedure TGIFViewer.SetFileName(Const Value : String);
Begin
  if Value = FFileName then exit;
  FFileName := Value;
  LoadFromFile(FFileName);
End;

Function TGIFViewer.GetFrame(Const Index : Integer) : Graphics.TBitmap;
Begin
  Result := nil;
  if (Index>0) and (Index<FrameCount) then
    Result := FRenderCache.Items[Index].Bitmap;
End;

Procedure TGIFViewer.SetTransparent(Const Value: Boolean);
Begin

  if FTransparent = Value then exit;
  FTransparent := Value;
  FGIFLoader.Transparent := Value;
  if FFileName <> '' then LoadFromFile(FFileName);

End;

procedure TGIFViewer.SetBevelColor(AValue: TColor);
begin
  if FBevelColor <> AValue then
  begin
    FBevelColor := AValue;
    Invalidate;
  end;
end;

procedure TGIFViewer.SetBevelWidth(const Value: TBevelWidth);
begin
  if FBevelWidth <> Value then
  begin
    FBevelWidth := Value;
    Invalidate;
  end;
end;

procedure TGIFViewer.SetBevelInner(const Value: TPanelBevel);
begin
  if BevelInner <> Value then
  begin
    FBevelInner := Value;
    Invalidate;
  end;
end;

procedure TGIFViewer.SetBevelOuter(const Value: TPanelBevel);
begin
  if BevelOuter <> Value then
  begin
    FBevelOuter := Value;
    Invalidate;
  end;
end;

Procedure TGIFViewer.DoInternalOnLoadError(Sender: TObject; Const ErrorCount: Integer; Const ErrorList: TStringList);
Begin
  if Assigned(FOnLoadError) then FOnloadError(Self,ErrorCount,ErrorList);
End;

Procedure TGIFViewer.DoTimerAnimate(Sender : TObject);
Begin
  Inc(FCurrentFrameIndex);
  if FCurrentFrameIndex > (FRenderCache.Count-1) then FCurrentFrameIndex := 0;
  If Assigned(FOnFrameChange) then FOnFrameChange(Self);
  FAnimateTimer.Interval :=  FRenderCache.Items[FCurrentFrameIndex].Delay;
  FCurrentView.Assign(FRenderCache.Items[FCurrentFrameIndex].Bitmap);
  Invalidate;
End;

Function TGIFViewer.RenderFrame(Index : Integer) : TBitmap;
var
  Src : TFastBitmap;
  pTop, pLeft : Integer;
  iDrawMode : TFastBitmapDrawMode;
Begin
  Src := FGIFLoader.Frames.Items[Index].Bitmap;
  pLeft := FGIFLoader.Frames.Items[Index].Left;
  pTop  := FGIFLoader.Frames.Items[Index].Top;
  if Index = 0 then
  begin
   if  (FTransparent) then
   begin
     FVirtualView.Clear(clrTransparent);
     iDrawMode := dmAlphaCheck;
   end
   else
   begin
     FVirtualView.Clear(FGIFLoader.BackgroundColor);
     iDrawMode := dmSet;
   end;
   FVirtualView.PutImage(Src,0,0, Src.Width, Src.Height,pLeft,pTop,dmSet);
   //FRestoreBitmap := FVirtualView.Clone;
  End
  else
  begin

    with FGIFLoader.Frames.Items[Index] do
    begin
    Case DrawMode of
      dmNone:
        Begin
          FVirtualView.PutImage(Src,0,0, Src.Width, Src.Height,pLeft,pTop,iDrawMode);
        End;
      dmKeep:
      begin
        FVirtualView.PutImage(Src,0,0, Src.Width, Src.Height,pLeft,pTop,iDrawMode);
        if Assigned(FRestoreBitmap) then FreeAndNil(FRestoreBitmap);
        FRestoreBitmap := FVirtualView.Clone;
      End;
      dmErase:
        begin

          // Normalement PutImage devrait être avec dmAlphaCheck et
          // mais dans le cas ou BackgroundColor est égale à la couleur de transparence.
          // La transparence n'est pas afficher

          // FVirtualView.Clear(FGIFLoader.BackgroundColor); //---> A decommenter pour affichage sans transparence
          // FVirtualView.TransparentColor := FGIFLoader.BackgroundColor.ToColor; // ne fonctionne pas cf TFastBitmap.BuildBitmap
          // FVirtualView.PutImage(Src,0,0, Src.Width, Src.Height,pLeft,pTop,dmAlphaCheck);
          if  (FGIFLoader.Frames.Items[Index].IsTransparent and FTransparent) then FVirtualView.Clear(clrTransparent)
          else FVirtualView.Clear(FGIFLoader.BackgroundColor);
          FVirtualView.PutImage(Src,0,0, Src.Width, Src.Height,pLeft,pTop,iDrawMode);
        End;
      dmRestore:
        begin

           // FVirtualView.Clear(clrTransparent);
          FVirtualView.PutImage(FRestoreBitmap,0,0, FRestoreBitmap.Width, FRestoreBitmap.Height,0,0,dmSet);
          FVirtualView.PutImage(Src,0,0, Src.Width, Src.Height,pLeft,pTop,iDrawMode);
        End;
      else
        FVirtualView.PutImage(Src,0,0, Src.Width, Src.Height,pLeft,pTop,dmSet);
    End;
  End;
  end;
  Result := FVirtualView.GetBitmap;
  if FGIFLoader.FFrames[Index].Delay <> 0 then FAnimateTimer.Interval := FGIFLoader.FFrames[Index].Delay * FAnimateSpeed;
End;

Procedure TGIFViewer.ComputeCache;
Var
  I : Integer;
Begin
  FCurrentFrameIndex := 0;
  FRenderCache.Clear;
  if FGIFLoader.FrameCount>0 then
  begin
    For I:=0 to Pred(FGIFLoader.FrameCount) do
    begin
       FRenderCache.AddNewCache;
       FRenderCache.Items[I].Delay := FGIFLoader.Frames[I].Delay;
       FRenderCache.Items[I].Bitmap := RenderFrame(I);
    End;
    FAnimateTimer.Interval :=  FRenderCache.Items[0].Delay;
    FCurrentView.Assign(FRenderCache.Items[0].Bitmap);
  End;
End;

Procedure TGIFViewer.CalculatePreferredSize(Var PreferredWidth, PreferredHeight : integer; WithThemeSpace : Boolean);
Begin
  PreferredWidth := FGIFWidth+1;
  PreferredHeight := FGIFHeight+1;
End;

Class Function TGIFViewer.GetControlClassDefaultSize : TSize;
Begin
  Result.CX := 90; // = ClientWidth
  Result.CY := 90; // = ClientHeight
End;

Procedure TGIFViewer.Paint;

  procedure DrawFrame;
  begin
    with inherited Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      MoveTo(0, 0);
      LineTo(Self.Width-1, 0);
      LineTo(Self.Width-1, Self.Height-1);
      LineTo(0, Self.Height-1);
      LineTo(0, 0);
    end;
  end;

  procedure PaintBevel(var ARect: TRect; ABevel: GraphType.TGraphicsBevelCut);
  begin
    // Note: Frame3D inflates ARect
    if (ABevel <> bvNone)  then
      if BevelColor = clDefault then
      begin
         with inherited Canvas do
            Canvas.Frame3d(ARect, BevelWidth, ABevel)
      End
      else
      begin
//        InflateRect(ARect, -BorderWidth, -BorderWidth);
        with inherited Canvas do
          Canvas.Frame3d(ARect, BevelColor, BevelColor, BevelWidth);
      End;
  end;


var
  R: TRect;
  C: TCanvas;
begin

  if csDesigning in ComponentState then DrawFrame;

  C := inherited Canvas;
  R := DestRect;
  FPainting:=true;
  try
    C.Lock;

    FCurrentView.Transparent:= FTransparent;
    C.StretchDraw(R, FCurrentView);
    if FBorderShow then
    begin
      if FBorderWidth > 0 then
      With C do
      begin
        Pen.Style := psSolid;
        Pen.Width := FBorderWidth;
        Pen.Color := FBorderColor;
        Brush.Style := bsClear;
        Rectangle(0,0,ClientWidth,ClientHeight);
      End;
    End;
    C.UnLock;
  finally
    FPainting:=false;
  end;

  Inherited Paint;
End;

Function TGIFViewer.DestRect : TRect;
var
  PicWidth: Integer;
  PicHeight: Integer;
  ImgWidth: Integer;
  ImgHeight: Integer;
  w: Integer;
  h: Integer;

begin
  PicWidth := FCurrentView.Width;
  PicHeight := FCurrentView.Height;
  ImgWidth := ClientWidth;
  ImgHeight := ClientHeight;
  if (PicWidth=0) or (PicHeight=0) then Exit(Rect(0, 0, 0, 0));

  if Stretch Then
  begin
    w:=ImgWidth;
    h:=(PicHeight*w) div PicWidth;
    if h>ImgHeight then begin
      h:=ImgHeight;
      w:=(PicWidth*h) div PicHeight;
    end;
    PicWidth:=w;
    PicHeight:=h;
  end;


  Result := Rect(0, 0, PicWidth, PicHeight);

  if Center then
  begin
    Result.Left:= (ImgWidth div 2) -(PicWidth div 2);
    Result.Top := (ImgHeight div 2) -(PicHeight div 2);
    Result.Right := Result.Left + PicWidth;
    Result.Bottom := Result.Top + PicHeight;
  end;
End;

Procedure TGIFViewer.Invalidate;
Begin
  if FPainting then exit;
  inherited Invalidate;
End;

Procedure TGIFViewer.LoadFromFile(Const aFileName: String);
Begin
  FAnimateTimer.Enabled := False;
  FPause := False;
  FAnimated := False;
  FCurrentFrameIndex := 0;
  FGIFLoader.LoadFromFile(aFileName);
  FFileName := aFileName;
  FGIFWidth := FGIFLoader.Width;
  FGIFHeight := FGIFLoader.Height;
  FVirtualView.SetSize(FGIFWidth, FGIFHeight);
  ComputeCache;
  if AutoSize then
  begin
   InvalidatePreferredSize;
   AdjustSize;
  end;
  Invalidate;
  if FAutoPlay then Start;
End;

Procedure TGIFViewer.LoadFromResource(Const ResName: String);
Var
  Resource: TLResource;
Begin
  FAnimateTimer.Enabled := False;
  FPause := False;
  FAnimated := False;
  FCurrentFrameIndex := 0;
  Resource:=LazarusResources.Find(ResName);
  if Resource = nil then Raise Exception.Create('Resource '+ResName+' not found !')
  else
    if CompareText(LazarusResources.Find(ResName).ValueType, 'gif')=0 then
    begin
      FGIFLoader.LoadFromResource(ResName);
      FGIFWidth := FGIFLoader.Width;
      FGIFHeight := FGIFLoader.Height;
      FVirtualView.SetSize(FGIFWidth, FGIFHeight);
      ComputeCache;
      if AutoSize then
      begin
        InvalidatePreferredSize;
        AdjustSize;
      end;
      Invalidate;
      if FAutoPlay then Start;
    End;
End;

Procedure TGIFViewer.Start;
Begin
  if not(FPause) then FCurrentFrameIndex := 0;
  FPause := false;
  FAnimated := True;
  FAnimateTimer.Enabled := True;
  If Assigned(FOnStart) then FOnStart(Self);
End;

Procedure TGIFViewer.Stop;
Begin
  FAnimateTimer.Enabled := False;
  FAnimated := False;
  FPause := False;
  If Assigned(FOnStop) then FOnStop(Self);
  FCurrentframeIndex := 0;
  FAnimateTimer.Interval :=  FRenderCache.Items[0].Delay;
  FCurrentView.Assign(FRenderCache.Items[0].Bitmap);
  Invalidate;
End;

Procedure TGIFViewer.Pause;
Begin
  FAnimateTimer.Enabled := False;
  FPause := True;
End;

Function TGIFViewer.GetRawFrame(Index : Integer) : TBitmap;
Begin
  Result := FGIFLoader.Frames[Index].Bitmap.GetBitmap;
End;

Procedure TGIFViewer.DisplayFrame(Index: Integer);
Begin
  if (Index > FRenderCache.Count-1) then exit;
  FCurrentView.Assign(FRenderCache.Items[Index].Bitmap);
  Invalidate;
End;

Procedure TGIFViewer.DisplayRawFrame(Index: Integer);
Var
  Tmp : Graphics.TBitmap;
Begin
  if (Index > FRenderCache.Count-1) then exit;
  Tmp := GetRawFrame(Index);
  FCurrentView.Assign(Tmp);
  FreeAndNil(Tmp);
  Invalidate;
End;

{%endregion}

{%region=====[ Intégration EDI ]================================================}

Type
  TGIFViewerFileNamePropertyEditor=class(TFileNamePropertyEditor)
  public
    function GetFilter: String; override;
    function GetInitialDirectory: string; override;
  end;


function TGIFViewerFileNamePropertyEditor.GetFilter: String;
begin
  Result := 'Graphic Interchange Format |*.gif';
end;

function TGIFViewerFileNamePropertyEditor.GetInitialDirectory: string;
begin
  Result:= ExtractFilePath(LazarusIDE.ActiveProject.ProjectInfoFile);
end;

procedure Register;
begin
  {$I ..\Resources\gifviewer_icon.lrs}
  RegisterComponents('Beanz Extras', [TGIFViewer]);
  RegisterPropertyEditor(TypeInfo(String),
    TGIFViewer, 'FileName', TGIFViewerFileNamePropertyEditor);
end;

{%endregion%}

initialization



finalization

End.

