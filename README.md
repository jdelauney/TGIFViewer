# TGIFViewer
TGIFViewer :eyes: composant visuel pour l'affichage d'animations au fromat GIF (Graphic Interchange Format) avec Free Pascal et Lazarus 

Capacités de TGIFViewer :
- Chargement depuis un stream, un fichier ou une resource (**fonctions LoadFromStream, LoadFromFile, LoadFromResource**)
- Extraction des images brute (**fonction DisplayRawFrame)
- Extraction des images pré-calculées de l'animation (**fonction DisplayFrame** )
- Affichage avec ou sans transparence (**Transparent**)
- Gestion silencieuse de certaines données mal encodées pour permette l'affichage des images sans perturber l'utilisateur
- Gestion des erreurs pour les fichiers mal compressé" (**OnLoadError**)
- Centrer (**Center**), redimensionnement (**Stretch**) l'affichage 
- Dimension du composant automatique en fonction de l'image (**AutoSize**)
- Evèment à la lecture, l'arrêt ou la mise en pause de l'animation (**OnStart, OnPause, OnStop**)
- Accès aux images et informations du GIF via la propiété **Frames.Items[x]**

:information_source: **Contient également 2 unités** :
- uFastBitmap : Classe pour la manipulation de bitmap 32 bit au format RGBA ou BGRA suivant l'OS
- TypesHelpers : Classes ajoutant des fonctions utiles au type de données (Byte, integer, string, double, TDateTime...) les fonctions sont accesibles directement par le biais complétation du code sous Lazarus.


