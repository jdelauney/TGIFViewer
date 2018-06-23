Unit TypesHelpers;
(*==============================================================================
 DESCRIPTION   : Unité utilitaire qui rajoutes des fonctions aux type de données :
                 Byte, Integer, Single, Double, Char, String, TDateTime.
                 Ces fonctions sont directement accessibles via l'auto-complétation
                 du code, lorsque vous rajouté un "." Vous pouvez également y avoir
                 accès via le raccourci "Ctrl + Espace"
 DATE          : 17/06/2018
 VERSION       : 1.0
 AUTEUR        : J.Delauney (BeanzMaster)
 LICENCE       : MPL
================================================================================
*)

// /!\ Dont' Work with trunk versions
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}
{$ASMMODE INTEL}
{.$mode delphi} // Work with trunk version only if Delphi Mode is defined

//----[ Option Interne ]-------------------------------------------------------------------------
// if Defined then disable Assembly code optimizations
{.$DEFINE NO_ASM_OPTIMIZATIONS}

Interface

Uses
  Classes, SysUtils;

{ Const for computing string "hash" }
Const
  { Random number for calculating 'hash' }
  cHash1 = 130469;
  { Bigger is the size, lower is the risk of collision }
  cHash2 = MaxInt Shr 4;

Type
  { TBooleanHelper : Helpers for Boolean Type}
  TBooleanHelper = record helper for Boolean
  public
    { @name : Convert to a string representation}
    function ToString(Const DefTrue : String = 'True'; Const defFalse : String = 'False'): String;
  end;

  { TCharHelper : Helpers for Char Type }
  TCharHelper = record helper for Char
  public
    { @name : Check if Char is Alpha}
    function IsAlpha: Boolean;
    { @name : Check if Char is Numeric }
    function IsNumeric: Boolean;
    { @name : Returns the character code }
    function ToCharCode : Integer;
    { @name : Converts the character to upper case.}
    function ToUpper : Char;
    { @name : Converts the character to lower case.}
    function ToLower : Char;
  end;

  { TByteHelper : Helper for Byte type }
  TByteHelper =  record helper for Integer
  public
    const
      MaxValue = 255;
      MinValue = 0;
      NormalizedRatio : Single = 1/255;
  public
    { @name : Convert a string to it's Byte representation.
              Returns True on succes. False otherwise and set defValue }
    function Parse(Num : String; Const defValue:Byte=0):Boolean;  overload;
    { @name : Convert to a string representation }
    function ToString: string;
    { @name : Returns the smallest value of Self and vMin}
    function Min(vMin:Byte):Byte;
    { @name : Returns the maximum of Self and vMax}
    function Max(vMax:Byte):Byte;
    { @name : Check if a number is inclusively between a range}
    function InRange(Low, High: Byte): Boolean;
    { @name : Returns random number between MinValue and MaxValue }
    function Random:Byte;
    { @name : Returns random number between vMin and vMax }
    function RandomRange(vMin, vMax :Byte):Byte;
    { @name : Ensure Self value is in range between vMin & vMax and returns value}
    function Clamp(vMin,vMax:Byte):Byte;
    { @name : Returns to boolean representation. Returns @True if value greater than zero. Otherwise returns @False }
    function ToBoolean : Boolean;
    { @name : Returns Hexa string representation }
    Function ToHexString: string;
    { @name : Returns normalized value. In range[0..1] }
    function Normalized : Single;
  end;

  { TShortIntHelper : Helper for ShortInt type }
  TShortIntHelper =  record helper for Integer
  public
    const
      MaxValue = 127;
      MinValue = -128;
      NormalizedRatio : single = 1 / 255;
  public
    { @name : Convert a string to it's ShortInt representation.
              Returns True on succes. False otherwise and set defValue }
    function Parse(Num : String; Const defValue:ShortInt=0):Boolean;
    { @name : Convert to a string representation }
    function ToString(Const Formatted : Boolean = False): string;
    { @name : Returns the smallest value of Self and vMin}
    function Min(vMin:ShortInt):ShortInt;
    { @name : Returns the maximum of Self and vMax}
    function Max(vMax:ShortInt):ShortInt;
    { @name : Check if a number is inclusively between a range}
    function InRange(Low, High: ShortInt): Boolean;
    { @name : Returns random number between MinValue and MaxValue }
    function Random:ShortInt;
    { @name : Returns random number between vMin and vMax }
    function RandomRange(vMin, vMax :ShortInt):ShortInt;
    { @name : Ensure Self value is in range between vMin & vMax and returns value}
    function Clamp(vMin,vMax:ShortInt):ShortInt;
    { @name : Returns the sign of the value using the (-1, 0, +1) convention }
    function Sign:ShortInt;
    { @name : Convert to it's Byte representation. Range in [0..255]}
    function ToByte : Byte;
    { @name : Convert to it's Boolean representation.  Returns @True if value greater than zero. Otherwise returns @False }
    function ToBoolean : Boolean;
    { @name : Returns Hexa string representation }
    function ToHexString: string;
    { @name : Returns normalized value. In range[0..1] }
    function Normalized : Single;
    { @name : Returns Reciprocal (1/value) }
    function Reciprocal : Single;
    { @name : Returns @True if value is a power of two. Otherwise returns @False }
    function IsPowerOfTwo : Boolean;
    { @name : Returns the next power of two }
    function NextPowerOfTwo:ShortInt;
    { @name : Returns the previous power of two }
    function PreviousPowerOfTwo:ShortInt;
  end;

  { TSmallIntHelper : Helper for SmallInt type }
  TSmallIntHelper =  record helper for SmallInt
  public
    const
      MaxValue = 32767;
      MinValue = -32768;
      NormalizedRatio : single = 1 / 32767;
  public
    { @name : Convert a string to it's SmallInt representation.
              Returns True on succes. False otherwise and set defValue }
    function Parse(Num : String; Const defValue:SmallInt=0):Boolean;
    { @name : Convert to a string representation }
    function ToString(Const Formatted : Boolean = False): string;
    { @name : Returns the smallest value of Self and vMin}
    function Min(vMin:SmallInt):SmallInt;
    { @name : Returns the maximum of Self and vMax}
    function Max(vMax:SmallInt):SmallInt;
    { @name : Check if a number is inclusively between a range}
    function InRange(Low, High: SmallInt): Boolean;
    { @name : Returns random number between MinValue and MaxValue }
    function Random:SmallInt;
    { @name : Returns random number between vMin and vMax }
    function RandomRange(vMin, vMax :SmallInt):SmallInt;
    { @name : Ensure Self value is in range between vMin & vMax and returns value}
    function Clamp(vMin,vMax:SmallInt):SmallInt;
    { @name : Returns the sign of the value using the (-1, 0, +1) convention }
    function Sign:SmallInt;
    { @name : Convert to it's Byte representation. Range in [0..255]}
    function ToByte : Byte;
    { @name : Convert to it's Boolean representation.  Returns @True if value greater than zero. Otherwise returns @False }
    function ToBoolean : Boolean;
    { @name : Returns Hexa string representation }
    function ToHexString: string;
    { @name : Returns normalized value. In range[0..1] }
    function Normalized : Single;
    { @name : Returns Reciprocal (1/value) }
    function Reciprocal : Single;
    { @name : Returns @True if value is a power of two. Otherwise returns @False }
    function IsPowerOfTwo : Boolean;
    { @name : Returns the next power of two }
    function NextPowerOfTwo:SmallInt;
    { @name : Returns the previous power of two }
    function PreviousPowerOfTwo:SmallInt;
  end;

  { TWordHelper : Helper for Word type }
  TWordHelper =  record helper for Word
  public
    const
      MaxValue = 65535;
      MinValue = 0;
      NormalizedRatio : single = 1 / 65535;
  public
    { @name : Convert a string to it's Word representation.
              Returns True on succes. False otherwise and set defValue }
    function Parse(Num : String; Const defValue:Word=0):Boolean;
    { @name : Convert to a string representation }
    function ToString(Const Formatted : Boolean = False): string;
    { @name : Returns the smallest value of Self and vMin}
    function Min(vMin:Word):Word;
    { @name : Returns the maximum of Self and vMax}
    function Max(vMax:Word):Word;
    { @name : Check if a number is inclusively between a range}
    function InRange(Low, High: Word): Boolean;
    { @name : Returns random number between MinValue and MaxValue }
    function Random:Word;
    { @name : Returns random number between vMin and vMax }
    function RandomRange(vMin, vMax : Word):Word;
    { @name : Ensure Self value is in range between vMin & vMax and returns value}
    function Clamp(vMin,vMax:Word):Word;
    { @name : Convert to it's Byte representation. Range in [0..255]}
    function ToByte : Byte;
    { @name : Convert to it's Boolean representation.  Returns @True if value greater than zero. Otherwise returns @False }
    function ToBoolean : Boolean;
    { @name : Returns Hexa string representation }
    function ToHexString: string;
    { @name : Returns normalized value. In range[0..1] }
    function Normalized : Single;
    { @name : Returns Reciprocal (1/value) }
    function Reciprocal : Single;
    { @name : Returns @True if value is a power of two. Otherwise returns @False }
    function IsPowerOfTwo : Boolean;
    { @name : Returns the next power of two }
    function NextPowerOfTwo:Word;
    { @name : Returns the previous power of two }
    function PreviousPowerOfTwo:Word;
  end;

  { TCardinalHelper : Helper for Cardinal type (Note : Cardinal = DWord = LongWord ) }
  TCardinalHelper =  record helper for Cardinal
  public
    const
      MaxValue = 4294967295;
      MinValue = 0;
      NormalizedRatio : single = 1 / 4294967295;
  public
    { @name : Convert a string to it's Cardinal representation.
              Returns True on succes. False otherwise and set defValue }
    function Parse(Num : String; Const defValue:Cardinal=0):Boolean;
    { @name : Convert to a string representation }
    function ToString(Const Formatted : Boolean = False): string;
    { @name : Returns the smallest value of Self and vMin}
    function Min(vMin:Cardinal):Cardinal;
    { @name : Returns the maximum of Self and vMax}
    function Max(vMax:Cardinal):Cardinal;
    { @name : Check if a number is inclusively between a range}
    function InRange(Low, High: Cardinal): Boolean;
    { @name : Returns random number between MinValue and MaxValue }
    function Random:Cardinal;
    { @name : Returns random number between vMin and vMax }
    function RandomRange(vMin, vMax :Cardinal):Cardinal;
    { @name : Ensure Self value is in range between vMin & vMax and returns value}
    function Clamp(vMin,vMax:Cardinal):Cardinal;
    { @name : Convert to it's Byte representation. Range in [0..255]}
    function ToByte : Byte;
    { @name : Convert to it's Boolean representation.  Returns @True if value greater than zero. Otherwise returns @False }
    function ToBoolean : Boolean;
    { @name : Returns Hexa string representation }
    function ToHexString: string;
    { @name : Returns normalized value. In range[0..1] }
    function Normalized : Single;
    { @name : Returns Reciprocal (1/value) }
    function Reciprocal : Single;
    { @name : Returns @True if value is a power of two. Otherwise returns @False }
    function IsPowerOfTwo : Boolean;
    { @name : Returns the next power of two }
    function NextPowerOfTwo:Cardinal;
    { @name : Returns the previous power of two }
    function PreviousPowerOfTwo:Cardinal;
  end;

  { TIntegerHelper : Helper for Integer type }
  TIntegerHelper =  record helper for Integer
  public
    const
      MaxValue = 2147483647;
      MinValue = -2147483648;
      NormalizedRatio : single = 1 / 2147483647;
  public
    { @name : Convert a string to it's integer representation.
              Returns True on succes. False otherwise and set defValue }
    function Parse(Num : String; Const defValue:Integer=0):Boolean;
    { @name : Convert to a string representation }
    function ToString(Const Formatted : Boolean = False): string;
    { @name : Returns the smallest value of Self and vMin}
    function Min(vMin:Integer):Integer;
    { @name : Returns the maximum of Self and vMax}
    function Max(vMax:Integer):Integer;
    { @name : Check if a number is inclusively between a range}
    function InRange(Low, High: Integer): Boolean;
    { @name : Returns random number between MinValue and MaxValue }
    function Random:Integer;
    { @name : Returns random number between vMin and vMax }
    function RandomRange(vMin, vMax :Integer):Integer;
    { @name : Ensure Self value is in range between vMin & vMax and returns value}
    function Clamp(vMin,vMax:Integer):Integer;
    { @name : Returns the sign of the value using the (-1, 0, +1) convention }
    function Sign:Integer;
    { @name : Convert to it's Byte representation. Range in [0..255]}
    function ToByte : Byte;
    { @name : Convert to it's Boolean representation.  Returns @True if value greater than zero. Otherwise returns @False }
    function ToBoolean : Boolean;
    { @name : Returns Hexa string representation }
    function ToHexString: string;
    { @name : Returns normalized value. In range[0..1] }
    function Normalized : Single;
    { @name : Returns Reciprocal (1/value) }
    function Reciprocal : Single;
    { @name : Returns @True if value is a power of two. Otherwise returns @False }
    function IsPowerOfTwo : Boolean;
    { @name : Returns the next power of two }
    function NextPowerOfTwo:Integer;
    { @name : Returns the previous power of two }
    function PreviousPowerOfTwo:Integer;
  end;

  { TInt64Helper : Helper for Int64 type }
  TInt64Helper =  record helper for Int64
  public
    const
      MaxValue = 9223372036854775807;
      MinValue = -9223372036854775808;
  public
    { @name : Convert a string to it's Int64 representation.
              Returns True on succes. False otherwise and set defValue }
    function Parse(Num : String; Const defValue:Int64=0):Boolean;
    { @name : Convert to a string representation }
    function ToString(Const Formatted : Boolean = False): string;
    { @name : Returns the smallest value of Self and vMin}
    function Min(vMin:Int64):Int64;
    { @name : Returns the maximum of Self and vMax}
    function Max(vMax:Int64):Int64;
    { @name : Check if a number is inclusively between a range}
    function InRange(Low, High: Int64): Boolean;
    { @name : Returns random number between MinValue and MaxValue }
    function Random:Int64;
    { @name : Returns random number between vMin and vMax }
    function RandomRange(vMin, vMax :Int64):Int64;
    { @name : Ensure Self value is in range between vMin & vMax and returns value}
    function Clamp(vMin,vMax:Int64):Int64;
    { @name : Returns the sign of the value using the (-1, 0, +1) convention }
    function Sign:Int64;
    { @name : Convert to it's Byte representation. Range in [0..255]}
    function ToByte : Byte;
    { @name : Convert to it's Boolean representation.  Returns @True if value greater than zero. Otherwise returns @False }
    function ToBoolean : Boolean;
    { @name : Returns Hexa string representation }
    function ToHexString: string;

  end;

  { TQWordHelper : Helper for QWord type }
  TQWordHelper =  record helper for QWord
  public
    const
      MaxValue = 18446744073709551615;
      MinValue = 0;
  public
    { @name : Convert a string to it's QWord representation.
              Returns True on succes. False otherwise and set defValue }
    function Parse(Num : String; Const defValue:QWord=0):Boolean;
    { @name : Convert to a string representation }
    function ToString(Const Formatted : Boolean = False): string;
    { @name : Returns the smallest value of Self and vMin}
    function Min(vMin:QWord):QWord;
    { @name : Returns the maximum of Self and vMax}
    function Max(vMax:QWord):QWord;
    { @name : Check if a number is inclusively between a range}
    function InRange(Low, High: QWord): Boolean;
    { @name : Returns random number between MinValue and MaxValue }
    function Random:QWord;
    { @name : Returns random number between vMin and vMax }
    function RandomRange(vMin, vMax :QWord):QWord;
    { @name : Ensure Self value is in range between vMin & vMax and returns value}
    function Clamp(vMin,vMax:QWord):QWord;
    { @name : Convert to it's Boolean representation.  Returns @True if value greater than zero. Otherwise returns @False }
    function ToBoolean : Boolean;
    { @name : Returns Hexa string representation }
    function ToHexString: string;
  end;

  { TSingleHelper : Helper for Single type }
  TSingleHelper =  record helper for Single
  public
    {$push}
    {$R-}
    {$Q-}
    const
      Epsilon          : Single = 1.4012984643248170709e-45;
      MaxValue         : Single = 340282346638528859811704183484516925440.0;
      MinValue         : Single = -340282346638528859811704183484516925440.0;
      PositiveInfinity : Single = 1.0/0.0;
      NegativeInfinity : Single = -1.0/0.0;
      NaN              : Single = 0.0/0.0;
    {$POP}
  public
    { @name : Convert to a string representation }
    function ToString(Const Decimals:Integer = 5): string;
    { @name : Convert a string to it's single representation.
              Returns True on succes. False otherwise and set defValue }
    function Parse(Num : String; Const defValue:Single=0):Boolean;
    { @name : Returns the smallest value of Self and vMin}
    function Min(vMin:Single):Single;
    { @name : Returns the maximum of Self and vMax}
    function Max(vMax:Single):Single;
    { @name : Check if a number is inclusively between a range}
    function InRange(Low, High: Single): Boolean;
    { @name : Returns random number between MinValue and MaxValue }
    function Random:Single;
    { @name : Returns random number between vMin and vMax }
    function RandomRange(vMin, vMax :Single):Single;
    { @name : Ensure Self value is in range between vMin & vMax and returns value}
    function Clamp(vMin,vMax:Single):Single;
    { @name : Rounds a value towards zero }
    function Trunc : Integer;
    { @name : Rounds a value towards its nearest integer }
    function Round : Integer;
    { @name : Rounds value towards negative infinity }
    function Floor : Integer;
    { @name : Rounds value towards positive infinity }
    function Ceil : Integer;
    { @name : Rounds the floating point value to the closest Integer. @br
              Behaves like Round but returns a floating point value like Int.}
    function RoundInt : Single;
    { @name : Returns fractionnal part of value }
    function Fract : Single;
    { @name : Returns -1 if self lesser than zero, 1 if self greater than zero and 0 if self equals zero }
    function Sign:Single;
    { @name : Returns True if postive or negative infinity. False otherwise.}
    Function IsInfinity: Boolean;
    { @name : Returns True if NaN. False otherwise.}
    Function IsNan: Boolean;
    { @name : Returns True if postive infinity. False otherwise.}
    Function IsNegativeInfinity: Boolean;
    { @name : Returns True if negative infinity. False otherwise.}
    Function IsPositiveInfinity: Boolean;
  end;

  { TDoubleHelper : Helper for Double type }
  TDoubleHelper =  record helper for Double
  public
    const
    {$push}
    {$R-}
    {$Q-}
      Epsilon          : Double = 4.9406564584124654418e-324;
      MaxValue         : Double = 1.7976931348623157081e+308;
      MinValue         : Double = -1.7976931348623157081e+308;
      PositiveInfinity : Double = 1.0/0.0;
      NegativeInfinity : Double = -1.0/0.0;
      NaN              : Double = 0.0/0.0;
    {$POP}
  public
    { @name : Convert to a string representation }
    function ToString(Const Decimals:Integer = 5): string;
    { @name : Convert a string to it's single representation.
              Returns True on success. False otherwise and set defValue }
    function Parse(Num : String; Const defValue:Double=0):Boolean;
    { @name returns the smallest value of Self and vMin}
    function Min(vMin:Double):Double;
    { @name : Returns the maximum of Self and vMax}
    function Max(vMax:Double):Double;
    { @name : Check if a number is inclusively between a range}
    function InRange(Low, High: Double): Boolean;
    { @name : Returns random number between MinValue and MaxValue }
    function Random:Double;
    { @name : Returns random number between vMin and vMax }
    function RandomRange(vMin, vMax :Double):Double;
    { @name :  Ensure Self value is in range between vMin & vMax and returns value}
    function Clamp(vMin,vMax:Double):Double;
    { @name : Rounds a value towards zero }
    function Trunc : Integer;
    { @name : Rounds a value towards its nearest integer }
    function Round : Integer;
    { @name : Rounds value towards negative infinity }
    function Floor : Integer;
    { @name : Rounds value towards positive infinity }
    function Ceil : Integer;
    { @name : Rounds the floating point value to the closest Integer. @br
              Behaves like Round but returns a floating point value like Int.}
    function RoundInt : Double;
    { @name : Returns fractionnal part of value }
    function Fract : Double;
    { @name : Returns -1 if self lesser than zero, 1 if self greater than zero and 0 if self equals zero }
    function Sign:Double;
    { @name : Returns True if postive or negative infinity. False otherwise.}
    Function IsInfinity: Boolean;
    { @name : Returns True if NaN. False otherwise.}
    Function IsNan: Boolean;
    { @name : Returns True if postive infinity. False otherwise.}
    Function IsNegativeInfinity: Boolean;
    { @name : Returns True if negative infinity. False otherwise.}
    Function IsPositiveInfinity: Boolean;
  end;

  {$IFDEF FPC_HAS_TYPE_EXTENDED}
    { TODO : Add Helper for Extended }
  {$ENDIF}

  { TStringHelper : Helpers for String Type - Use UTF-8 convention }
  TStringHelper =  record helper for String
  Private

    Function GetChar(AIndex : Integer) : Char;
    Function GetLength : Integer;
  public
    Const
      Empty = '';
  public
    { @name : Set string from System string }
    procedure SetFromSys(aString : String);
    { @name : Set string from Console string }
    procedure SetFromConsole(aString:String);
    { @name : Convert and set any string to UTF8, folowwing pascal convention }
    procedure SetToUTF8(aString:String);
    { @name : Returns string as System }
    function ToSys : String;
    { @name : Returns string as console }
    function ToConsole : String;
    { @name : Strips leading/trailing spaces and control characters.}
    function  Trim: String; overload;
    { @name : Strips leading spaces and control characters}
    function  TrimLeft : String;
    { @name : Strips trailing spaces and control characters}
    function  TrimRight : String;
    { @name : Returns true if a string has no characters above ord(' ').@br
              If basically means that it returns true if the string is empty or only contains
              control characters or spaces.@br
              Much faster than @code(if Trim(s)='' then...)}
    function  IsEmpty : Boolean;
    //{ @name : Same as the standard Length function }
    //function GetLength: Integer;
    { @name : Converts all characters to upper case.}
    function  ToUpper : String;
    { @name : Converts all characters to lower case.}
    function  ToLower : String;
    { @name : Returns @True if string matches a value. Optionally ignoring case}
    function IsEquals(const Value: string; Const IgnoreCase: Boolean = False): Boolean;
    { @name : Compares two strings optionally ignoring case returning -1 if A comes before
              before B, 1 if A comes after b, ord 0 if A and B are equal}
    function Compare(const Value: string; Const IgnoreCase: Boolean = False): Integer;
    { @name : Sets the String with a certain character and length.}
    procedure RepeatChar(C : Char; Count : Integer);
    { @name : Repeats a string count times.}
    procedure RepeatStr(const s : String; Count : Integer);
    { @name : Centers String of length TotalLen.}
    function PadCenter(TotalLen: Integer; Const PadChar : Char = ' '): String;
    { @name : Add a character at the end of the string until the length is equal to PadLen. @br
              If PadLen is negative the character will be inserted to the left.}
    function PadCharRight(PadLen : Integer;Const PadChar : Char = ' ') : String;
    { @name : Add a character to the left while Length<PadLen.}
    function PadCharLeft(PadLen : Integer;Const PadChar : Char = ' ') : String;
    { @name : Same as the standard Pos function }
    function Pos(const SubStr : String;StartPos :integer = 1) : Integer; //overload;
    { @name : Returns @true if Substr1 and Substr2 are found and return position of Substr1 and Substr2.
              Else returns @false, and -1 for positions }
    function PosBetween(const SubStr1, SubStr2 : String; var StartPos, EndPos : Integer):Boolean;
    { @name : Same as Pos but with optionally ignoring case}
    function IndexOf(const SubStr: string; Start: Integer=0; IgnoreCase: Boolean = False): Integer;
    { @name : Scans for SubStr, if found the characters after SubStr is returned else
              an empty string is returned.}
    function After(const SubStr : String; Position : Integer = 1) : String;
    { @name : Scans for SubStr, if found the characters before SubStr is returned else
              the complete string is returned.}
    function Before(const SubStr : String; Position : Integer = 1) : String;
    { @name : Scans s for the start/end combination of SubStr1, SubStr2 and returns the text between them.@br
              If SubStr2 is empty SubStr2 will be regarded as identical to SubStr1.@br
              If SubStr2 is not found, then an empty string is returned (as opposed to function Mid).}
    function Between(const SubStr1, SubStr2 : String) : String;
    { @name : Scans String and returns text after the first SubStr1 and before SubStr2 if found.@br
              If SubStr2 is empty SubStr2 will be regarded as identical to SubStr1.@br
              If SubStr2 is not found the complete string after SubStr1 is returned (as opposed to function Between).@br
              This function is the equivalent of a Before/After combination, but faster.}
    function Mid(const SubStr1, SubStr2 : String; Position : Integer = 1) : String;
    { @name : Same as the standard Copy function except Count is by default 2GB.}
    function Copy(aStart, aLength : Integer) : string;
    { @name : Copy String from StartPos to APos}
    function CopyPos(StartPos, EndPos: Integer): String;
    { @name : Insert substr to String at Position}
    function Insert(const SubStr: string; Position: Integer):String;
    { @name : Returns all chars to the left from a specific (including) Position}
    function LeftOf(Position : Integer) : String;
    { @name : Returns all chars to the right from a specific (including) Position}
    function RightOf(Position : Integer) : String;
    { @name : Convert String to WideString }
    function ToWideString : WideString;
    { @name : Convert WideString to String and set self }
    procedure SetWideString(S : WideString);
    { @name : Convert Interger as String}
    procedure Parse(Num : Integer); overload;
    { @name : Convert Single as String  and set self}
    procedure Parse(Num : Single; Const Decimals:Integer = 5); overload;
    { @name : Convert Double as String  and set self}
    procedure Parse(Num : Double; Const Decimals:Integer = 5); overload;
    { @name : Surround String by a String}
    function Surround(chs: string): string;  overload;
    { @name : Surround String with two others String}
    function Surround(chsL, chsR: string): string; overload;
    { @name : Join StringList elements separated by Sep}
    procedure Implode(lst:TStringList;sep : string =';');
    { @name : Split String separated by sep to a StringList}
    function Explode(sep: string = ';'):TStringList;
    { @name : Search for a substring optionally ignoring case}
    function Contains(const SubStr: string; IgnoreCase: Boolean = False): Boolean;
    { @name : Surround String with quotes }
    function Quote : String;
    { @name : Replace all Old Pattern by New Pattern}
    function Replace(const OldPattern, NewPattern: string; IgnoreCase: Boolean = False) : string;
    { @name : Erases all characters on the left from StartIndex }
    function RemoveLeft(StartIndex : Integer):String; overload;
    { @name : Erases aCount character on the left from StartIndex }
    function RemoveLeft(StartIndex, aCount : Integer):String; overload;
    { @name : Erases all characters on the right from StartIndex }
    function RemoveRight(StartIndex : Integer):String; overload;
    { @name : Erases aCount characters on the right from StartIndex }
    function RemoveRight(StartIndex, aCount : Integer):String; overload;
    { @name : Erases all character aChar in the string }
    function RemoveChar(aChar : Char; Const IgnoreCase : Boolean = False):String;
    { @name : Erases all string SubStr in the string }
    function Remove(SubStr:String; Const IgnoreCase : Boolean = False):String;
    { @name : Reverse order of characters in String}
    function Reverse : String;
    { @name : Wrap String to maximum width "MaxCol"}
    function Wrap(MaxCol : Integer):String;
    { @name : Returns the number of Char C present in string }
    function CountChars(C: Char):Integer;
    { @name : Returns @True if string contains DefTrue. Returns @false if string contains DefFalse. Otherwise returns @True }
    function ToBoolean(Const DefTrue : String = 'True'; Const defFalse : String = 'False'; Const IgnoreCase : Boolean = false):Boolean;
    { @name : Convert the string to Integer. Returns zero on error}
    function ToInteger : Integer;
    { @name : Convert the string to Int64. Returns zero on error}
    function ToInt64 : Int64;
    { @name : Convert the string to Single. Returns zero on error}
    function ToSingle : Single;
    { @name : Convert the string to Double. Returns zero on error}
    function ToDouble : Double;
    //function ToInt64 : Int64;
    { @name : Convert the string to Byte. Returns zero on error}
    function ToByte : Byte;
    { @name : Computes the Hash of the string }
    function ComputeHash(Const IgnoreCase : Boolean = False) : Integer;

    { @name : Return the UTF8 Length }
    function LengthUTF8 : Integer;
    //class operator := (const NewString:String):String;

    { Chars : Returns the Char at "AIndex" of the string }
    property Chars[AIndex: Integer]: Char read GetChar;
    { Length : Returns length of the string. }
    property Length: Integer read GetLength;
  End;

  {TDateFormat : Type of formatted DateTime for string representation }
  TDateFormat =(dfUTC, dfGMT, dfCustom);

  { TDateTimeHelper : Helper for TDateTime
    CheatSheet Date/time formatting characters :
    See also : https://www.freepascal.org/docs-html/rtl/sysutils/formatchars.html

      Character   |                  Meaning
    ----------------------------------------------------------------------------
        c         | Short date format (only for FormatDateTime)
        d         | day of month
        dd        | day of month (leading zero)
        ddd       | day of week (abbreviation)
        dddd      | day of week (full)
        ddddd     | Long date format (only for FormatDateTime)
        m         | month or minute if preceed by h
        mm        | month (leading zero)
        mmm       | month (abbreviation)
        mmmm      | month (full)
        y         | year (two digits)
        yy        | year (two digits)
        yyyy      | year (four digits, with century)
        h         | hour
        hh        | hour (leading zero)
        n         | minute
        nn        | minute (leading zero)
        s         | second
        ss        | second (leading zero)
        t         | Short time format (only for FormatDateTime)
        tt        | Long time format (only for FormatDateTime)
        am/pm     | use 12 hour clock and display am and pm accordingly
        a/p       | use 12 hour clock and display a and p accordingly
        /         | insert date seperator
        :         | insert time seperator
        "xx"      | literal text
        ’xx’      | literal text
    ----------------------------------------------------------------------------
    Note for using : '/' as separator you'll need to quote it in your pattern string ( eg : 'YYYY"/"MM"/"DD' )
  }
  TDateTimeHelper = record helper for TDateTime
  private
    function getYear: Word;
    function getMonth: Word;
    function getDay: Word;
    function getHour: Word;
    function getMinute: Word;
    function getSecond: Word;
    Function getMillisecond : Word;

  public
   Const
     fmtFullDate : String = 'dddd dd mmmm yyyy ';
     fmtShortDate : String = 'dd"/"mm"/"yyyy';
     fmtShortTime : String = 'hh":"nn":"ss';
  public
    { @name : Set Date to today }
    procedure SetToDay;
    { @name : Set to current time}
    Procedure SetToTime;
    { @name : Set Date & Time to now }
    procedure SetToNow;
    { @name : Set Time from H, M, S values }
    procedure SetTime(H,M,S : Integer);
    { @name : Set Date from Y, M, D values }
    procedure SetDate(Y,M,D : Integer);
    { @name : Set DateTime from String  }
    procedure SetFromString(ADateTimeStr : String ;Const APAttern : String = 'YYYY"/"MM"/"DD hh":"nn":"ss');
    { @name : Set DateTime from Unix timestamp }
    procedure SetFromUnixTimeStamp(ADate : Int64);
    { @name : Set DateTime from Mac timestamp }
    procedure SetFromMacTimeStamp(ADate : Int64);
    { @name : Set DateTime from UTC DateTime }
    procedure SetFromUTC(UT: TDateTime); overload;
    { @name : Set DateTime from UTC DateTime }
    procedure SetFromUTC(UT: TDateTime; ZOffset : Integer); overload;
    { @name : Set DateTime from Julian DateTime }
    procedure SetFromJulian(JulianDate : Double);
    { @name : Set DateTime from SQL DateTime string representation }
    procedure SetFromSQL(ADataTimeStr : String);
    { @name : Set a random DateTime From Current Date if FromNow is True }
    procedure SetRandomDate;  overload;
    { @name : Set a random DateTime in range of StartDate and EndDate }
    procedure SetRandomDate(StartDate, EndDate : TDateTime); overload;
    { @name : Set DateTime from file }
    procedure SetFromFile(aFileName:String);
    { @name : Convert to a string representation }
    function ToString(Const Format: TDateFormat = dfCustom ; Const CustomFormat: string = 'yyyy-mm-dd hh:nn:ss'): string;
    { @name : Convert to SQL string representation }
    function ToSQL : String;
    { @name : Returns the date to it's string representation eg : YYYY/MM/DD}
    function DateToString(Const FormatStr :string = 'dd"/"mm"/"yyyy'): String;  // Note pour utilisé le séparateur / il faut le "Double Quoter"
    { @name : Returns the time to it's string representation eg : H:M:S}
    function TimeToString(Const FormatStr : string = 'hh:nn:ss') : String;
    { @name : Compare two TDateTime. Returns 0 if equal. Otherwise returns -1 or 1 }
    function Compare(SecondDateTime : TDatetime):Integer;
    { @name : Compare the Date. Returns 0 if equal. Otherwise returns -1 or 1 }
    function CompareDate(SecondDate : TDateTime):Integer;
    { @name : Compare the Time. Returns 0 if equal. Otherwise returns -1 or 1 }
    function CompareTime(SecondTime : TDateTime):Integer;
    { @name : Add Years to current Date }
    function AddYear(const A: Integer=1): TDateTime;
    { @name : Add Months to current Date }
    function AddMonth(const A: Integer=1): TDateTime;
    { @name : Add Days to current Date }
    function AddDay(const A: Integer=1): TDateTime;
    { @name : Add Weeks to current Date }
    function AddWeek(const A: Integer=1): TDateTime;
    { @name : Add Hours to current Time }
    function AddHour(const A: Integer=1): TDateTime;
    { @name : Add minutes to current Time }
    function AddMinute(const A: Integer=1): TDateTime;
    { @name : Add Seconds to current Time }
    function AddSecond(const A: Integer=1): TDateTime;
    { @name : Add Milliseconds to current Time }
    function AddMilliSecond(const A: Integer=1): TDateTime;
    { @name : Convert to a Julian representation }
    function ToJulian : Double;
    { @name : Convert to a Unix timestamp representation }
    function ToUnixTimeStamp : Int64;
    { @name : Convert to a Mac timestamp representation }
    function ToMacTimeStamp : Int64;
    { @name : Convert to a UTC representation }
    function ToUTC : TDateTime; overload;
    { @name : Convert to a UTC representation }
    function ToUTC(ZOffset : Integer) : TDateTime;
    { @name : Returns @True if is leap year. Otherwise returns @False }
    function IsLeapYear : Boolean;
    { @name : Retuns the number of days in the year }
    function GetDaysInYear : Word;
    { @name : Retuns the number of days in the month }
    function GetDaysInMonth : Word;
    { @name : Retuns the number of week in the year }
    function GetWeeksInYear : Word;
    { @name : Retuns current day of the week }
    function GetDayOfTheWeek : Word;
    { @name : Retuns current week of then year }
    function GetWeekOfTheYear : Word;
    { @name : Returns elapsed Years between two date }
    function GetElapsedYears(ATo : TDateTime): Integer;
    { @name : Returns elapsed Months between two date }
    function GetElapsedMonths(ATo : TDateTime): Integer;
    { @name : Returns elapsed Weeks between two date }
    function GetElapsedWeeks(ATo : TDateTime): Integer;
    { @name : Returns elapsed Days between two date }
    function GetElapsedDays(ATo : TDateTime): Integer;
    { @name : Returns elapsed Hours between two time }
    function GetElapsedHours(ATo : TDateTime): Int64;
    { @name : Returns elapsed Minutes between two time }
    function GetElapsedMinutes(ATo : TDateTime): Int64;
    { @name : Returns elapsed Seconds between two time }
    function GetElapsedSeconds(ATo : TDateTime): Int64;
    { @name : Returns elapsed Milliseconds between two time }
    function GetElapsedMilliSeconds(ATo : TDateTime): Int64;
    { @name : Returns elapsed Date and Time between two DateTime }
    function GetElapsedPeriod(ATo : TDateTime): TDateTime;

    { Year : Returns the year of the date }
    property Year : Word read getYear;
    { Month : Returns the Month of the date }
    property Month : Word read getMonth;
    { Day : Returns the Day of the date }
    property Day : Word read getDay;
    { Hour : Returns the Hour of the time }
    property Hour : Word read getHour;
    { Minute : Returns the Minute of the time }
    property Minute : Word read getMinute;
    { Second : Returns the Second of the time }
    property Second : Word read getSecond;
    { MilliSecond : Returns the MilliSecond of the time }
    property MilliSecond : Word read getMilliSecond;
  end;


var
  vPointSeparator, vCommaSeparator: TFormatSettings;

Implementation

Uses
  LazUTF8, Math, DateUtils, LazFileUtils;
  //{$IFDEF WINDOWS} ,windows{$ENDIF};

{%region%====================[ Internal tools ]=================================}

const
  cHexTbl : array[0..15] of char='0123456789ABCDEF';

function _hexstr(val : longint;cnt : byte) : shortstring;
var
  i : Integer;
begin
  Result[0]:=char(cnt);
  for i:=cnt downto 1 do
   begin
     result[i]:=chextbl[val and $f];
     val:=val shr 4;
   end;
end;

{$IFNDEF NO_ASM_OPTIMIZATIONS}
function ClampByte(const Value: Integer): Byte; assembler; nostackframe;
asm
{$IFDEF CPUX86_64}
  {$IFDEF UNIX}
     MOV     EAX,EDI
  {$ELSE}
     MOV     EAX,ECX
  {$ENDIF}
{$ENDIF}
   TEST    EAX,$FFFFFF00
   JNZ     @above
   RET
@above:
   JS      @below
   MOV     EAX,$FF
   RET
@Below:
   XOR     EAX,EAX
end;
{$ELSE}
function ClampByte(const Value: Integer): Byte;
begin
 Result := Value;
 if Value > 255 then Result := 255
 else if Value < 0 then Result := 0;
end;
{$ENDIF}

{%endregion%}

{%region%=====[ TBooleanHelper ]================================================}

Function TBooleanHelper.Tostring(Const Deftrue : String; Const Deffalse : String) : String;
Begin
  if (Self = True) then result := DefTrue else result := DefFalse;
End;

{%endregion%}

{%region%=====[ TCharHelper ]===================================================}

Function TCharHelper.IsAlpha : Boolean;
Begin
 Result := ((Self in ['A'..'Z']) or (Self in ['a'..'z']));
End;

Function TCharHelper.IsNumeric : Boolean;
Begin
  Result := (Self in ['0'..'9']);
End;

Function TCharHelper.ToCharCode : Integer;
Begin
  Result := ord(Self);
End;

Function TCharHelper.ToUpper : Char;
begin
  result :=UpCase(Self);
End;

Function TCharHelper.ToLower : Char;
Begin
  Result := LowerCase(Self);
End;

{%endregion%}

{%region%=====[ TByteHelper ]===================================================}

function TByteHelper.Parse(Num: String; const defValue: Byte): Boolean;
Var
  I: Integer;
Begin
  Result := false;
  Self := defValue;
  if TryStrToInt(Num,I) then
  begin
    Self := ClampByte(I);
    result := true;
  End;
End;

function TByteHelper.ToString: string;
Begin
  Result := IntToStr(Self);
End;

function TByteHelper.Min(vMin: Byte): Byte;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TByteHelper.Max(vMax: Byte): Byte;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TByteHelper.InRange(Low, High: Byte): Boolean;
Begin
  result := ((Self >= Low) and (Self <= High));
End;

Function TByteHelper.Random:Byte;
Begin
  Result := Self.RandomRange(Self.MinValue,Self.MaxValue);
End;

Function TByteHelper.RandomRange(vMin, vMax :Byte):Byte;
Begin
 Result:=System.Random(Abs(vMin-vMax)) + Math.Min(vMin,vMax);
End;

function TByteHelper.Clamp(vMin, vMax: Byte): Byte;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TByteHelper.ToBoolean: Boolean;
begin
  result := (self<>0);
end;

function TByteHelper.ToHexString: string;
begin
 result :=_hexstr(Self,2)
end;

function TByteHelper.Normalized: Single;
begin
  result := self * Self.NormalizedRatio;
end;

{%endregion%}

{%region%=====[ TShortIntHelper ]===============================================}

function TShortIntHelper.Parse(Num: String; const defValue: ShortInt): Boolean;
Var
  I: Integer;
Begin
  Result := false;
  Self := defValue;
  if TryStrToInt(Num,I) then
  begin
    Self := I.Clamp(Shortint.MinValue,ShortInt.MaxValue);
    result := true;
  End;
End;

function TShortIntHelper.ToString(const Formatted: Boolean): string;
Begin
  if not(Formatted) then
    Result := IntToStr(Self)
  else
    Result := Format('%.0n',[Self+0.0],vPointSeparator);
End;

function TShortIntHelper.Min(vMin: ShortInt): ShortInt;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TShortIntHelper.Max(vMax: ShortInt): ShortInt;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TShortIntHelper.InRange(Low, High: ShortInt): Boolean;
Begin
  result := ((Self >= Low) and (Self <= High));
End;

Function TShortIntHelper.Random:ShortInt;
Begin
  Result := Self.RandomRange(Self.MinValue,Self.MaxValue);
End;

Function TShortIntHelper.RandomRange(vMin, vMax :ShortInt):ShortInt;
Begin
 Result:=System.Random(Abs(vMin-vMax))+ Math.Min(vMin,vMax);
End;

function TShortIntHelper.Clamp(vMin, vMax: ShortInt): ShortInt;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TShortIntHelper.Sign: ShortInt;
begin
  If Self < 0 Then
    Result := -1
  Else If Self > 0 Then
    Result := 1
  Else
    Result := 0;
end;

function TShortIntHelper.ToByte: Byte;
Begin
  Result := ClampByte(Self);
End;

function TShortIntHelper.ToBoolean: Boolean;
begin
  Result := (Self>0);
end;

function TShortIntHelper.ToHexString: string;
begin
  Result:=_hexstr(Self,8)
end;

function TShortIntHelper.Normalized: Single;
begin
  Result := Self * Self.NormalizedRatio;
end;

function TShortIntHelper.Reciprocal: Single;
Const
  cEpsilon : Single = 1.4012984643248170709e-45;
Var
  a: ShortInt;
Begin
  Result := 0;
  If Self = 0 Then exit;
  a := Self.Sign;
  If ((a * Self) >= cEpsilon) Then
    Result := 1.0 / Self
  Else
    Result := a * (1.0 / cEpsilon);
End;

function TShortIntHelper.IsPowerOfTwo: Boolean;
Const
  BitCountTable: Array[0..255] Of Byte =
    (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8);

  Function BitCount(Value: Longword): Longword; inline;
  Var
    V: Array[0..3] Of Byte absolute Value;
  Begin
    Result := BitCountTable[V[0]] + BitCountTable[V[1]] + BitCountTable[V[2]] + BitCountTable[V[3]];
  End;
Begin
  Result := BitCount(abs(Self)) = 1;
End;

function TShortIntHelper.NextPowerOfTwo: ShortInt;
var
  value : ShortInt;
begin
  Value := Self;
  If (Value > 0) Then
  Begin
    Dec(Value);
    Value := Value Or (Value Shr 1);
    Value := Value Or (Value Shr 2);
    Value := Value Or (Value Shr 4);
    Value := Value Or (Value Shr 8);
    Value := Value Or (Value Shr 16);
  End;

  Result := Value + 1;
end;

function TShortIntHelper.PreviousPowerOfTwo: ShortInt;
Var
  I, N: ShortInt;
Begin
  Result := 0;
  For I := 14 Downto 2 Do
  Begin
    N := (1 Shl I);
    If N < Self Then
      Break
    Else
      Result := N;
  End;
end;

{%endregion%}

{%region%=====[ TSmallIntHelper ]===============================================}

function TSmallIntHelper.Parse(Num: String; const defValue: SmallInt): Boolean;
Var
  I: Integer;
Begin
  Result := false;
  Self := defValue;
  if TryStrToInt(Num,I) then
  begin
    Self := I.Clamp(SmallInt.MinValue,SmallInt.MaxValue);
    result := true;
  End;
End;

function TSmallIntHelper.ToString(const Formatted: Boolean): string;
Begin
  if not(Formatted) then
    Result := IntToStr(Self)
  else
    Result := Format('%.0n',[Self+0.0],vPointSeparator);
End;

function TSmallIntHelper.Min(vMin: SmallInt): SmallInt;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TSmallIntHelper.Max(vMax: SmallInt): SmallInt;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TSmallIntHelper.InRange(Low, High: SmallInt): Boolean;
Begin
  result := ((Self >= Low) and (Self <= High));
End;

Function TSmallIntHelper.Random:SmallInt;
Begin
  Result := Self.RandomRange(Self.MinValue,Self.MaxValue);
End;

Function TSmallIntHelper.RandomRange(vMin, vMax :SmallInt):SmallInt;
Begin
 Result:=System.Random(Abs(vMin-vMax))+ Math.Min(vMin,vMax);
End;

function TSmallIntHelper.Clamp(vMin, vMax: SmallInt): SmallInt;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TSmallIntHelper.Sign: SmallInt;
begin
  If Self < 0 Then
    Result := -1
  Else If Self > 0 Then
    Result := 1
  Else
    Result := 0;
end;

function TSmallIntHelper.ToByte: Byte;
Begin
  Result := ClampByte(Self);
End;

function TSmallIntHelper.ToBoolean: Boolean;
begin
  Result := (Self>0);
end;

function TSmallIntHelper.ToHexString: string;
begin
  Result:=_hexstr(Self,8)
end;

function TSmallIntHelper.Normalized: Single;
begin
  Result := Self * Self.NormalizedRatio;
end;

function TSmallIntHelper.Reciprocal: Single;
Const
  cEpsilon : Single = 1.4012984643248170709e-45;
Var
  a: SmallInt;
Begin
  Result := 0;
  If Self = 0 Then exit;
  a := Self.Sign;
  If ((a * Self) >= cEpsilon) Then
    Result := 1.0 / Self
  Else
    Result := a * (1.0 / cEpsilon);
End;

function TSmallIntHelper.IsPowerOfTwo: Boolean;
Const
  BitCountTable: Array[0..255] Of Byte =
    (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8);

  Function BitCount(Value: Longword): Longword; inline;
  Var
    V: Array[0..3] Of Byte absolute Value;
  Begin
    Result := BitCountTable[V[0]] + BitCountTable[V[1]] + BitCountTable[V[2]] + BitCountTable[V[3]];
  End;
Begin
  Result := BitCount(abs(Self)) = 1;
End;

function TSmallIntHelper.NextPowerOfTwo: SmallInt;
var
  value : SmallInt;
begin
  Value := Self;
  If (Value > 0) Then
  Begin
    Dec(Value);
    Value := Value Or (Value Shr 1);
    Value := Value Or (Value Shr 2);
    Value := Value Or (Value Shr 4);
    Value := Value Or (Value Shr 8);
    Value := Value Or (Value Shr 16);
  End;

  Result := Value + 1;
end;

function TSmallIntHelper.PreviousPowerOfTwo: SmallInt;
Var
  I, N: SmallInt;
Begin
  Result := 0;
  For I := 14 Downto 2 Do
  Begin
    N := (1 Shl I);
    If N < Self Then
      Break
    Else
      Result := N;
  End;
end;

{%endregion%}

{%region%=====[ TWordHelper ]===================================================}

function TWordHelper.Parse(Num: String; const defValue: Word): Boolean;
Var
  I: Integer;
Begin
  Result := false;
  Self := defValue;
  if TryStrToInt(Num,I) then
  begin
    Self := I.Clamp(0,Self.MaxValue);
    result := true;
  End;
End;

function TWordHelper.ToString(const Formatted: Boolean): string;
Begin
  if not(Formatted) then
    Result := IntToStr(Self)
  else
    Result := Format('%.0n',[Self+0.0],vPointSeparator);
End;

function TWordHelper.Min(vMin: Word): Word;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TWordHelper.Max(vMax: Word): Word;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TWordHelper.InRange(Low, High: Word): Boolean;
Begin
  result := ((Self >= Low) and (Self <= High));
End;

Function TWordHelper.Random:Word;
Begin
  Result := Self.RandomRange(Self.MinValue,Self.MaxValue);
End;

Function TWordHelper.RandomRange(vMin, vMax :Word):Word;
Begin
 Result:=System.Random(Abs(vMin-vMax))+ Math.Min(vMin,vMax);
End;

function TWordHelper.Clamp(vMin, vMax: Word): Word;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TWordHelper.ToByte: Byte;
Begin
  Result := ClampByte(Self);
End;

function TWordHelper.ToBoolean: Boolean;
begin
  Result := (Self>0);
end;

function TWordHelper.ToHexString: string;
begin
  Result:=_hexstr(Self,4)
end;

function TWordHelper.Normalized: Single;
begin
  Result := Self * Self.NormalizedRatio;
end;

function TWordHelper.Reciprocal: Single;
Const
  cEpsilon : Single = 1.4012984643248170709e-45;
Begin
  Result := 0;
  If Self = 0 Then exit;
  If (Self >= cEpsilon) Then
    Result := 1.0 / Self
  Else
    Result :=  (1.0 / cEpsilon);
End;

function TWordHelper.IsPowerOfTwo: Boolean;
Const
  BitCountTable: Array[0..255] Of Byte =
    (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8);

  Function BitCount(Value: Longword): Longword; inline;
  Var
    V: Array[0..3] Of Byte absolute Value;
  Begin
    Result := BitCountTable[V[0]] + BitCountTable[V[1]] + BitCountTable[V[2]] + BitCountTable[V[3]];
  End;
Begin
  Result := BitCount(abs(Self)) = 1;
End;

function TWordHelper.NextPowerOfTwo: Word;
var
  value : Word;
begin
  Value := Self;
  If (Value > 0) Then
  Begin
    Dec(Value);
    Value := Value Or (Value Shr 1);
    Value := Value Or (Value Shr 2);
    Value := Value Or (Value Shr 4);
    Value := Value Or (Value Shr 8);
    Value := Value Or (Value Shr 16);
  End;

  Result := Value + 1;
end;

function TWordHelper.PreviousPowerOfTwo: Word;
Var
  I, N: Word;
Begin
  Result := 0;
  For I := 14 Downto 2 Do
  Begin
    N := (1 Shl I);
    If N < Self Then
      Break
    Else
      Result := N;
  End;
end;

{%endregion%}

{%region%=====[ TCardinalHelper ]===============================================}

function TCardinalHelper.Parse(Num: String; const defValue: Cardinal): Boolean;
Var
  I: Int64;
Begin
  Result := false;
  Self := defValue;
  if TryStrToInt64(Num,I) then
  begin
    Self := I.Clamp(Cardinal.MinValue,Cardinal.MaxValue);
    result := true;
  End;
End;

function TCardinalHelper.ToString(const Formatted: Boolean): string;
Begin
  if not(Formatted) then
    Result := IntToStr(Self)
  else
    Result := Format('%.0n',[Self+0.0],vPointSeparator);
End;

function TCardinalHelper.Min(vMin: Cardinal): Cardinal;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TCardinalHelper.Max(vMax: Cardinal): Cardinal;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TCardinalHelper.InRange(Low, High: Cardinal): Boolean;
Begin
  result := ((Self >= Low) and (Self <= High));
End;

Function TCardinalHelper.Random:Cardinal;
Begin
  Result := Self.RandomRange(Self.MinValue,Self.MaxValue);
End;

Function TCardinalHelper.RandomRange(vMin, vMax :Cardinal):Cardinal;
Begin
 Result:=System.Random(Abs(vMin-vMax))+Math.Min(vMin,vMax);
End;

function TCardinalHelper.Clamp(vMin, vMax: Cardinal): Cardinal;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TCardinalHelper.ToByte: Byte;
Begin
  Result := ClampByte(Self);
End;

function TCardinalHelper.ToBoolean: Boolean;
begin
  Result := (Self>0);
end;

function TCardinalHelper.ToHexString: string;
begin
  Result:=_hexstr(Self,4)
end;

function TCardinalHelper.Normalized: Single;
begin
  Result := Self * Self.NormalizedRatio;
end;

function TCardinalHelper.Reciprocal: Single;
Const
  cEpsilon : Single = 1.4012984643248170709e-45;
Begin
  Result := 0;
  If Self = 0 Then exit;
  If (Self >= cEpsilon) Then
    Result := 1.0 / Self
  Else
    Result :=  (1.0 / cEpsilon);
End;

function TCardinalHelper.IsPowerOfTwo: Boolean;
Const
  BitCountTable: Array[0..255] Of Byte =
    (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8);

  Function BitCount(Value: LongWord): LongWord; inline;
  Var
    V: Array[0..3] Of Byte absolute Value;
  Begin
    Result := BitCountTable[V[0]] + BitCountTable[V[1]] + BitCountTable[V[2]] + BitCountTable[V[3]];
  End;
Begin
  Result := BitCount(abs(Self)) = 1;
End;

function TCardinalHelper.NextPowerOfTwo: Cardinal;
var
  value : Cardinal;
begin
  Value := Self;
  If (Value > 0) Then
  Begin
    Dec(Value);
    Value := Value Or (Value Shr 1);
    Value := Value Or (Value Shr 2);
    Value := Value Or (Value Shr 4);
    Value := Value Or (Value Shr 8);
    Value := Value Or (Value Shr 16);
  End;

  Result := Value + 1;
end;

function TCardinalHelper.PreviousPowerOfTwo: Cardinal;
Var
  I, N: Cardinal;
Begin
  Result := 0;
  For I := 14 Downto 2 Do
  Begin
    N := (1 Shl I);
    If N < Self Then
      Break
    Else
      Result := N;
  End;
end;

{%endregion%}

{%region%=====[ TIntegerHelper ]================================================}

function TIntegerHelper.Parse(Num: String; const defValue: Integer): Boolean;
Var
  I: Integer;
Begin
  Result := false;
  Self := defValue;
  if TryStrToInt(Num,I) then
  begin
    Self := I;
    result := true;
  End;
End;

function TIntegerHelper.ToString(const Formatted: Boolean): string;
Begin
  if not(Formatted) then
    Result := IntToStr(Self)
  else
    Result := Format('%.0n',[Self+0.0],vPointSeparator);
End;

function TIntegerHelper.Min(vMin: Integer): Integer;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TIntegerHelper.Max(vMax: Integer): Integer;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TIntegerHelper.InRange(Low, High: Integer): Boolean;
Begin
  result := ((Self >= Low) and (Self <= High));
End;

Function TIntegerHelper.Random:Integer;
Begin
  Result := Self.RandomRange(Self.MinValue,Self.MaxValue);
End;

Function TIntegerHelper.RandomRange(vMin, vMax :Integer):Integer;
Begin
 Result:=System.Random(Abs(vMin-vMax))+ Math.Min(vMin,vMax);
End;

function TIntegerHelper.Clamp(vMin, vMax: Integer): Integer;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TIntegerHelper.Sign: Integer;
begin
  If Self < 0 Then
    Result := -1
  Else If Self > 0 Then
    Result := 1
  Else
    Result := 0;
end;

function TIntegerHelper.ToByte: Byte;
Begin
  Result := ClampByte(Self);
End;

function TIntegerHelper.ToBoolean: Boolean;
begin
  Result := (Self>0);
end;

function TIntegerHelper.ToHexString: string;
begin
  Result:=_hexstr(Self,8)
end;

function TIntegerHelper.Normalized: Single;
begin
  Result := Self * Self.NormalizedRatio;
end;

function TIntegerHelper.Reciprocal: Single;
Const
  cEpsilon : Single = 1.4012984643248170709e-45;
Var
  a: Integer;
Begin
  Result := 0;
  If Self = 0 Then exit;
  a := Self.Sign;
  If ((a * Self) >= cEpsilon) Then
    Result := 1.0 / Self
  Else
    Result := a * (1.0 / cEpsilon);
End;

function TIntegerHelper.IsPowerOfTwo: Boolean;
Const
  BitCountTable: Array[0..255] Of Byte =
    (0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8);

  Function BitCount(Value: Longword): Longword; inline;
  Var
    V: Array[0..3] Of Byte absolute Value;
  Begin
    Result := BitCountTable[V[0]] + BitCountTable[V[1]] + BitCountTable[V[2]] + BitCountTable[V[3]];
  End;
Begin
  Result := BitCount(abs(Self)) = 1;
End;

function TIntegerHelper.NextPowerOfTwo: Integer;
var
  value : Integer;
begin
  Value := Self;
  If (Value > 0) Then
  Begin
    Dec(Value);
    Value := Value Or (Value Shr 1);
    Value := Value Or (Value Shr 2);
    Value := Value Or (Value Shr 4);
    Value := Value Or (Value Shr 8);
    Value := Value Or (Value Shr 16);
  End;

  Result := Value + 1;
end;

function TIntegerHelper.PreviousPowerOfTwo: Integer;
Var
  I, N: Integer;
Begin
  Result := 0;
  For I := 14 Downto 2 Do
  Begin
    N := (1 Shl I);
    If N < Self Then
      Break
    Else
      Result := N;
  End;
end;

{%endregion%}

{%region%=====[ TInt64Helper ]==================================================}

function TInt64Helper.Parse(Num: String; const defValue: Int64): Boolean;
Var
  I: Int64;
Begin
  Result := false;
  Self := defValue;
  if TryStrToInt64(Num,I) then
  begin
    Self := I;
    result := true;
  End;
End;

function TInt64Helper.ToString(const Formatted: Boolean): string;
Begin
  if not(Formatted) then
    Result := IntToStr(Self)
  else
    Result := Format('%.0n',[Self+0.0],vPointSeparator);
End;

function TInt64Helper.Min(vMin: Int64): Int64;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TInt64Helper.Max(vMax: Int64): Int64;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TInt64Helper.InRange(Low, High: Int64): Boolean;
Begin
  result := ((Self >= Low) and (Self <= High));
End;

Function TInt64Helper.Random:Int64;
Begin
  Result := Self.RandomRange(Self.MinValue,Self.MaxValue);
End;

{ Source : https://oroboro.com/large-random-in-range/ }
Function TInt64Helper.RandomRange(vMin, vMax :Int64):Int64;
Var
  Diff : QWord;
  i, rLow, rHigh, vLow, vHigh :Int64;
Begin
  Diff := vMax - vMin;
  if (Diff<=Int64.MaxValue) then
  begin
    I := Diff;
    result := System.Random(I)+vMin;
  end
  else
  begin
    rLow := System.Random(Int64.MaxValue);
    rHigh := System.Random(Int64.MaxValue);
    vLow := Diff and $FFFFFFFF;
    vHigh := Diff shr 32;
    result  :=  (( rHigh * vLow ) shr 32 )
              + (( rLow * vHigh ) shr 32 )
              + ( rHigh * vHigh )
              + vMin;
  end;
End;


function TInt64Helper.Clamp(vMin, vMax: Int64): Int64;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TInt64Helper.Sign: Int64;
begin
  If Self < 0 Then
    Result := -1
  Else If Self > 0 Then
    Result := 1
  Else
    Result := 0;
end;

function TInt64Helper.ToByte: Byte;
Begin
  Result := ClampByte(Self);
End;

function TInt64Helper.ToBoolean: Boolean;
begin
  Result := (Self>0);
end;

function TInt64Helper.ToHexString: string;
begin
  Result:=IntToHex(Self,16)
end;

{%endregion%}

{%region%=====[ TQWordHelper ]==================================================}

function TQWordHelper.Parse(Num: String; const defValue: QWord): Boolean;
Var
  I: QWord;
Begin
  Result := false;
  Self := defValue;
  if TryStrToQWord(Num,I) then
  begin
    Self := I;
    result := true;
  End;
End;

function TQWordHelper.ToString(const Formatted: Boolean): string;
Begin
  if not(Formatted) then
    Result := IntToStr(Self)
  else
    Result := Format('%.0n',[Self+0.0],vPointSeparator);
End;

function TQWordHelper.Min(vMin: QWord): QWord;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TQWordHelper.Max(vMax: QWord): QWord;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TQWordHelper.InRange(Low, High: QWord): Boolean;
Begin
  result := ((Self >= Low) and (Self <= High));
End;

Function TQWordHelper.Random:QWord;
Begin
  Result := Self.RandomRange(Self.MinValue,Self.MaxValue);
End;

Function TQWordHelper.RandomRange(vMin, vMax :QWord):QWord;
Var
  Diff : QWord;
  i, rLow, rHigh, vLow, vHigh :Int64;
Begin
  Diff := vMax - vMin;
  if (Diff<=Int64.MaxValue) then
  begin
    I := Diff;
    result := System.Random(I)+vMin;
  end
  else
  begin
    rLow := System.Random(Int64.MaxValue);
    rHigh := System.Random(Int64.MaxValue);
    vLow := Diff and $FFFFFFFF;
    vHigh := Diff shr 32;
    result  :=  (( rHigh * vLow ) shr 32 )
              + (( rLow * vHigh ) shr 32 )
              + ( rHigh * vHigh )
              + vMin;
  end;
End;

function TQWordHelper.Clamp(vMin, vMax: QWord): QWord;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TQWordHelper.ToBoolean: Boolean;
begin
  Result := (Self>0);
end;

function TQWordHelper.ToHexString: string;
begin
  Result:=IntToHex(Self,15);
end;

{%endregion%}

{%region%=====[ TSingleHelper ]=================================================}

function TSingleHelper.ToString(const Decimals: Integer ): string; //Const FormatSetting :TFormatSettings
Begin
  result := FloatToStrF(Self, ffNumber, 15 , Decimals, vPointSeparator);
End;

function TSingleHelper.Parse(Num: String; const defValue: Single): Boolean;
var
  I: Single;
  Ok : Boolean;
Begin
  result := False;
  Self := DefValue;
  if Num.Contains('.') then Ok := TryStrToFloat(Num,I,vPointSeparator)
  else if Num.Contains(',') then Ok := TryStrToFloat(Num,I,vCommaSeparator)
  else Ok := TryStrToFloat(Num,I);
  if Ok then
  begin
    Result := True;
    Self := I;
  End;
End;

function TSingleHelper.Min(vMin: Single): Single;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TSingleHelper.Max(vMax: Single): Single;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TSingleHelper.InRange(Low, High: Single): Boolean;
begin
  result := ((Self >= Low) and (Self <= High));
end;

function TSingleHelper.Random : Single;
Begin
   Result :=Self.RandomRange(Self.MinValue, Self.MaxValue);
End;

function TSingleHelper.RandomRange(vMin,vMax : Single): Single;
Begin
   Result := Random * (vMax - vMin) + vMin;
End;

function TSingleHelper.Clamp(vMin, vMax: Single): Single;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TSingleHelper.Trunc: Integer;
begin
  result := System.Trunc(Self);
end;

function TSingleHelper.Round: Integer;
begin
  result := System.Round(Self);
end;

function TSingleHelper.Floor: Integer;
Begin
   {$HINTS OFF}
  result :=0;
  if (Self=0.0) then exit
  else If (Self > 0) Then
    Result := System.Trunc(Self)
  Else
    Result := System.Trunc(Self-0.999999999);
  {$HINTS ON}
End;

function TSingleHelper.Ceil: Integer;
begin
 {$HINTS OFF}
 Result := System.Trunc(Self);
 if (Self - Result) > 0 then Inc(Result);
 {$HINTS ON}
end;

function TSingleHelper.RoundInt: Single;
begin
 {$HINTS OFF}
 //Result := system.int
 Result := System.Round(Self + 0.5);
 {$HINTS ON}
end;

function TSingleHelper.Fract: Single;
begin
  result := Self - System.trunc(Self);
end;

function TSingleHelper.Sign: Single;
begin
 If Self < 0.0 Then
   Result := -1.0
 Else If Self > 0.0 Then
   Result := 1.0
 Else
   Result := 0.0;
end;

function TSingleHelper.IsInfinity: Boolean;
begin
  result := (Self = Self.NegativeInfinity) or (Self = Self.PositiveInfinity);
end;

function TSingleHelper.IsNan: Boolean;
begin
  result := (Self = Self.NaN);
end;

function TSingleHelper.IsNegativeInfinity: Boolean;
begin
  result := (Self = Self.NegativeInfinity)
end;

function TSingleHelper.IsPositiveInfinity: Boolean;
begin
  Result := (Self = Self.PositiveInfinity);
end;

{%endregion%}

{%region%=====[ TDoubleHelper ]=================================================}

function TDoubleHelper.ToString(const Decimals: Integer): string;
Begin
  result := FloatToStrF(Self, ffNumber, 15 , Decimals);
End;

function TDoubleHelper.Parse(Num: String; const defValue: Double): Boolean;
var
  I:Double;
Begin
  result := False;
  Self := DefValue;
  if TryStrToFloat(Num,I) then
  begin
    Result := True;
    Self := I;
  End;
End;

function TDoubleHelper.Min(vMin: Double): Double;
Begin
  if Self<vMin then result := vMin else result := Self;
End;

function TDoubleHelper.Max(vMax: Double): Double;
Begin
  if Self>vMax then result := vMax else result := Self;
End;

function TDoubleHelper.InRange(Low, High: Double): Boolean;
begin
  result := ((Self >= Low) and (Self <= High));
end;

function TDoubleHelper.Random : Double;
Begin
   Result :=Self.RandomRange(Self.MinValue, Self.MaxValue);
End;

function TDoubleHelper.RandomRange(vMin,vMax : Double): Double;
Begin
   Result := Random * (vMax - vMin) + vMin;
End;

function TDoubleHelper.Clamp(vMin, vMax: Double): Double;
begin
  Result := Self;
  if Result > vMax then begin Result := vMax; exit; end;
  if Result < vMin then Result := vMin;
end;

function TDoubleHelper.Trunc: Integer;
begin
  result := System.Trunc(Self);
end;

function TDoubleHelper.Round: Integer;
begin
  result := System.Round(Self);
end;

function TDoubleHelper.Floor: Integer;
Begin
   {$HINTS OFF}
  result :=0;
  if (Self=0.0) then exit
  else If (Self > 0) Then
    Result := System.Trunc(Self)
  Else
    Result := System.Trunc(Self-0.99999999999999999);
  {$HINTS ON}
End;

function TDoubleHelper.Ceil: Integer;
begin
 {$HINTS OFF}
 Result := System.Trunc(Self);
 if (Self - Result) > 0 then Inc(Result);
 {$HINTS ON}
end;

function TDoubleHelper.RoundInt: Double;
begin
 {$HINTS OFF}
 //Result := system.int
 Result := System.Round(Self + 0.5);
 {$HINTS ON}
end;

function TDoubleHelper.Fract: Double;
begin
  result := Self - System.trunc(Self);
end;

function TDoubleHelper.Sign: Double;
begin
  If Self < 0.0 Then
    Result := -1.0
  Else If Self > 0.0 Then
    Result := 1.0
  Else
    Result := 0.0;
end;

function TDoubleHelper.IsInfinity: Boolean;
begin
 Result := (Self = Self.NegativeInfinity) or (Self = Self.PositiveInfinity);
end;

function TDoubleHelper.IsNan: Boolean;
begin
 Result := (Self = Self.NaN);
end;

function TDoubleHelper.IsNegativeInfinity: Boolean;
begin
 Result := (Self = Self.NegativeInfinity);
end;

function TDoubleHelper.IsPositiveInfinity: Boolean;
begin
 Result := (Self = Self.PositiveInfinity);
end;

{%endregion%}

{%region%=====[ TStringHelper ]=================================================}

procedure TStringHelper.SetFromSys(aString: String);
Begin
  Self := SysToUTF8(aString);
End;

procedure TStringHelper.SetFromConsole(aString: String);
Begin
  Self := ConsoleToUTF8(aString);
End;

procedure TStringHelper.SetToUTF8(aString: String);
begin
  Self := Utf8EscapeControlChars(AString);
End;

function TStringHelper.ToSys: String;
Begin
  result := UTF8ToSys(Self);
End;

function TStringHelper.ToConsole: String;
Begin
  result := UTF8ToConsole(Self);
End;

function TStringHelper.Trim: String;
var i, l: Integer;
begin
  l := UTF8Length(Self);
  i := 1;
  while (i <= l) and (Self[i] <= ' ') do Inc(i);
  if i > l then Result := '' else
  begin
    while Self[l] <= ' ' do Dec(l);
    Result := UTF8Copy(Self, i, l - i + 1);
  end;
End;

function TStringHelper.TrimLeft: String;
var i, l : Integer;
begin
  l := UTF8Length(Self);
  i := 1;
  while (i <= l) and (Self[i] <= ' ') do Inc(i);
  Result := UTF8Copy(Self, i, Maxint);
End;

function TStringHelper.TrimRight: String;
var i : Integer;
begin
  i := UTF8Length(Self);
  while (i > 0) and (Self[i] <= ' ') do Dec(i);
  Result := UTF8Copy(Self, 1, i);
End;

function TStringHelper.IsEmpty: Boolean;
var i,l : Integer;
begin
  l := UTF8Length(Self);
  Result := False;
  for i := 1 to l do if Self[i]>=' ' then Exit;
  Result := True;
End;

function TStringHelper.GetChar(AIndex: Integer): Char;
begin
  Result := Self[1];
  if AIndex>0 then
    if (AIndex<=Length) then result := Self[AIndex] else result := Self[Length];
end;

function TStringHelper.GetLength: Integer;
Begin
  Result := System.Length(Self);  //Length(Self); //UTF8Length(Self);
End;

function TStringHelper.ToUpper: String;
Begin
  Result := UTF8UpperString(Self);
End;

function TStringHelper.ToLower: String;
Begin
  Result := UTF8LowerString(Self);
End;

function TStringHelper.IsEquals(const Value: string; const IgnoreCase: Boolean): Boolean;
Begin
  Result := (Self.Compare(Value,IgnoreCase) = 0);
End;

function TStringHelper.Compare(const Value: string; const IgnoreCase: Boolean): Integer;
Var
  S1,S2 : String;
Begin

  if IgnoreCase then
  Begin
    S1 := Self.ToUpper;
    S2 := Value.ToUpper;
  End
  else
  begin
    S1 := Self;
    S2 := Value;
  End;
  Result := UTF8CompareText(S1,S2);
End;

procedure TStringHelper.RepeatChar(C: Char; Count: Integer);
begin
  Self:='';
  SetLength(Self, Count);
  if Count>0
    then FillChar(Self[1], Count, c);
end;

procedure TStringHelper.RepeatStr(const s: String; Count: Integer);
var  p : PChar;
  Slen : Integer;
begin
  SLen := UTF8Length(s);
  SetLength(Self, Count*SLen);
  p := PChar(Self);
  while Count > 0 do
  begin
    Move(PChar(Self)^, p^, SLen);
    Inc(p, SLen);
    Dec(Count);
  end;
end;

function TStringHelper.PadCenter(TotalLen: Integer; const PadChar: Char): String;
Var
  l:Integer;
  S:String;
Begin
  l := UTF8Length(Self);
  if l < TotalLen then
  begin
    S:='';
    S.RepeatChar(PadChar, (TotalLen div 2) - (L div 2));
    Result := S + Self;
    S.RepeatChar(PadChar, TotalLen - UTF8Length(Result));
    Result := Result + S;
  end else Result := Self;
End;

function TStringHelper.PadCharRight(PadLen: Integer; const PadChar: Char): String;
var  i : Integer;
  More : Integer;
  Slen : Integer;
begin
  SLen := UTF8Length(Self);
  More := Abs(PadLen) - Slen;
  if More>0 then
  begin
    if PadLen<0 then
    begin
      SetLength(Result, Abs(PadLen));
      Move(Self[1], Result[More+1], Slen);
      for i := 1 to More do Result[i] := PadChar;
    end else
    begin
      Result := Self;
      SetLength(Result, Abs(PadLen));
      for i := SLen+1 to Slen+More do Result[i] := PadChar;
    end;
  end else Result := Self;
End;

function TStringHelper.PadCharLeft(PadLen: Integer; const PadChar: Char): String;
Begin
  Result := Self.PadCharRight(-PadLen, PadChar);
End;

function TStringHelper.Pos(const SubStr: String; StartPos: integer): Integer;
Begin
  Result := UTF8Pos(SubStr,Self,StartPos);
End;

function TStringHelper.PosBetween(const SubStr1, SubStr2: String; var StartPos, EndPos: Integer): Boolean;
var REndPos, RStartPos :Integer;
Begin
  Result:=False;
  StartPos:=-1;
  EndPos:=-1;
  RStartPos:= UTF8Pos(Substr1,Self);
  REndPos:= UTF8Pos(Substr2,Self);
  if (RStartPos>0) And (REndPos>0) then
  begin
    result:=True;
    StartPos:=RStartPos+UTF8Length(Substr1);
    EndPos:=REndpos-1;
  end;
End;

function TStringHelper.IndexOf(const SubStr: string; Start: Integer;IgnoreCase: Boolean): Integer;
var
  S1, S2 : String;
Begin
  S1 := Self;
  S2 := SubStr;
  if IgnoreCase then
  Begin
    S1 := Self.ToUpper;
    S2 := SubStr.ToUpper;
  End;
  Result := S1.Pos(S2, Start);
End;

function TStringHelper.After(const SubStr: String; Position: Integer): String;
var p,L,Start : Integer;
begin
  p := UTF8Pos(SubStr, Self,Position);
  Result := '';
  if p>=0 then
  begin
    Start:=p+UTF8Length(SubStr);
    L:=UTF8Length(Self)-(Start-1);
    Result := UTF8Copy(Self, Start, L);
  end;
End;

function TStringHelper.Before(const SubStr: String; Position: Integer): String;
var p , L: Integer;
begin
  p := UTF8Pos(SubStr, Self, Position);
  Result := '';
  if p>=0 then
  begin
    L:=p-1;
    Result := Self.Copy(position, L);
  end;
End;

function TStringHelper.Between(const SubStr1, SubStr2: String): String;
var StartPos,EndPos : Integer;
begin
  StartPos:=0;
  EndPos:=0;
  if Self.PosBetween(SubStr1, SubStr2, StartPos, EndPos) then
  begin
    EndPos := EndPos + 1;
    Result := UTF8Copy(Self, StartPos, (EndPos-StartPos));
  end
  else Result := '';
End;

function TStringHelper.Mid(const SubStr1, SubStr2: String; Position: Integer): String;
var p1,p2, ps,pe : Integer;
begin
  p1 := UTF8Pos(SubStr1, Self, Position);
  if p1<=0 then Result := ''
  else
  begin
    if SubStr2='' then p2 := UTF8Pos(SubStr1, Self, p1+UTF8Length(SubStr1))
    else p2 := UTF8Pos(SubStr2, Self, p1+UTF8Length(SubStr1));
    if p2<=0 then
    begin
      ps :=  p1+UTF8Length(SubStr1);
      pe := UTF8Length(Self);
      Result := UTF8Copy(Self,ps, pe)
    end
    else
    begin
      ps :=  p1+UTF8Length(SubStr1);
      pe :=  p2-ps;
      Result := UTF8Copy(Self,ps ,pe);
    end;
  end;
End;

function TStringHelper.Copy(aStart, aLength: Integer): string;
var
  L : Integer;
begin
  L := UTF8Length(Self);
  if (L=0)  or (aStart < 1) or (aLength < 1) then Exit;
  if ((aStart + aLength) > L) then aLength := (L - aStart)+1;
  SetLength(Result,aLength);
  Move(Self[aStart], Result[1], aLength);
 //Result := UTF8Copy(Self,AStart,ALength);
End;

function TStringHelper.CopyPos(StartPos, EndPos: Integer): String;
var Len:integer;
begin
  if EndPos<StartPos then
  begin
    Len:=StartPos+EndPos;
  end
  else
  begin
    Len:=EndPos-StartPos;
  end;
  result:=Self.Copy(StartPos, Len+1);
End;

function TStringHelper.Insert(const SubStr: string; Position: Integer): String;
Begin
  Result := Self;
  UTF8Insert(Substr,Result,Position);
End;

function TStringHelper.LeftOf(Position: Integer): String;
Begin
  Result := UTF8Copy(Self, 1, Position-1);
End;

function TStringHelper.RightOf(Position: Integer): String;
Begin
  Result := Self.Copy(Position+1, UTF8Length(Self));
End;

function TStringHelper.ToWideString: WideString;
Begin
 result := UTF8ToUTF16(Utf8EscapeControlChars(Self));
End;

procedure TStringHelper.SetWideString(S: WideString);
begin
  Self :='';
  if System.Length(S) < 1 then Exit;
  WideCharToStrVar(PWideChar(S), Self);
  Utf8EscapeControlChars(Self)
End;

procedure TStringHelper.Parse(Num : Integer);
Begin
  Self := IntToStr(Num);
End;

procedure TStringHelper.Parse(Num: Single; const Decimals: Integer);
Begin
  Self := FloatToStrF(Num, ffNumber, 15 , Decimals, vPointSeparator);
End;

procedure TStringHelper.Parse(Num: Double; const Decimals: Integer);
Begin
  Self := FloatToStrF(Num, ffNumber, 15 , Decimals, vPointSeparator);
End;

function TStringHelper.Surround(chs: string): string;
Begin
  Result := chs + Self + chs;
End;

function TStringHelper.Surround(chsL, chsR: string): string;
Begin
  Result := chsL + Self + chsR
End;

procedure TStringHelper.Implode(lst: TStringList; sep: string);
var
  i,j : integer;
  s : string;
begin
 S:='';
 j:= lst.Count - 1;
 for i:=0 to j do
 begin
   if i < j then s := s + lst[i] + sep
   else s := s + lst[i];  // don't add last separator
 end;
 Self := s;
End;

function TStringHelper.Explode(sep: string): TStringList;
var
 p : integer;
begin
  p := Self.Pos(sep);
  Result := TStringList.Create;
  while p > 0 do
  begin
    Result.Add(Self.Copy(1,p-1));
    if p <= Self.GetLength then Self := Self.Copy(p+ UTF8length(sep),Self.GetLength);
    p := Self.Pos(sep);
  end;
  Result.Add(Self);
End;

function TStringHelper.Contains(const SubStr: string; IgnoreCase: Boolean): Boolean;
Begin
  if IgnoreCase then
  begin
    Result := (Self.ToUpper.Pos(Substr.ToUpper) > 0);
  end
  else Result := (Self.Pos(Substr) > 0);
End;

function TStringHelper.Quote: String;
Begin
  Result := Self.Surround('"');
End;

function TStringHelper.Replace(const OldPattern, NewPattern: string; IgnoreCase: Boolean): string;
var rFlag : TReplaceFlags;
Begin
  rFlag := [rfReplaceAll];
  if IgnoreCase then rflag := rFlag + [rfIgnoreCase];
  Result := UTF8StringReplace(Self, OldPattern, NewPattern,rFlag);
End;

function TStringHelper.RemoveLeft(StartIndex: Integer): String;
Var
  L : Integer;
Begin
 L := UTF8Length(Self)-StartIndex;
 Result := Self.Copy(StartIndex,L);
End;

function TStringHelper.RemoveLeft(StartIndex, aCount: Integer): String;
Var
  L : Integer;
Begin
 L := UTF8Length(Self)-StartIndex;
 Result := Self.Copy(0,StartIndex-aCount)+Self.Copy(StartIndex,L);
End;

function TStringHelper.RemoveRight(StartIndex: Integer): String;
Begin
 Result := Self.Copy(0,StartIndex);
End;

function TStringHelper.RemoveRight(StartIndex, aCount: Integer): String;
Var
  L : Integer;
Begin
  L := UTF8Length(Self)-StartIndex-aCount;
  Result := Self.Copy(0,StartIndex)+Self.Copy(StartIndex+aCount,L);
End;

function TStringHelper.RemoveChar(aChar: Char; const IgnoreCase: Boolean
  ): String;
var
  I,L : Integer;
  c, c1 : Char;
  c2 : Char;
Begin
  L := UTF8Length(Self);
  Result := '';
  if IgnoreCase then c2 := {%H-}aChar.ToUpper else c2 := aChar;
  For I:=1 to L do
  begin
    c := Self[I];
    if IgnoreCase then c1 := c.ToUpper else c1 := c;
    if (c1<>c2) then result := result + c;
  End;
End;

function TStringHelper.Remove(SubStr: String; const IgnoreCase: Boolean
  ): String;
Begin
  Result := Self.Replace(SubStr,'',IgnoreCase);
End;

function TStringHelper.Reverse: String;
Begin
  Result := Utf8ReverseString(Self);
End;

function TStringHelper.Wrap(MaxCol: Integer): String;
Begin
  Result := UTF8WrapText(Self,MaxCol);
End;

function TStringHelper.CountChars(C: Char): Integer;
Var i,j,L:Integer;
begin
 L:= UTF8Length(Self);
 J:=0;
 For i := 1 to L do
 begin
   if (Self[i] = C) then inc(J);
 end;
 Result := J;
end;

function TStringHelper.ToBoolean(const DefTrue: String; const defFalse: String; const IgnoreCase: Boolean): Boolean;
begin
  if Self.Contains(DefTrue,IgnoreCase) then result := True
  else if Self.Contains(DefFalse,IgnoreCase) then result := False else result := True;
end;

function TStringHelper.ToInteger: Integer;
Var
  I: Integer;
Begin
  Result := 0;
  if TryStrToInt(Self,I) then
  begin
    result := I;
  End;
end;

function TStringHelper.ToSingle: Single;
var
  I: Single;
  Ok : Boolean;
Begin
  result := 0.0;
  if Self.Contains('.') then Ok := TryStrToFloat(Self,I,vPointSeparator)
  else if Self.Contains(',') then Ok := TryStrToFloat(Self,I,vCommaSeparator)
  else Ok := TryStrToFloat(Self,I);
  if Ok then
  begin
    Result := I;
  End;
end;

function TStringHelper.ToDouble: Double;
var
  I: Double;
  Ok : Boolean;
Begin
  result := 0.0;
  if Self.Contains('.') then Ok := TryStrToFloat(Self,I,vPointSeparator)
  else if Self.Contains(',') then Ok := TryStrToFloat(Self,I,vCommaSeparator)
  else Ok := TryStrToFloat(Self,I);
  if Ok then
  begin
    Result := I;
  End;
end;

function TStringHelper.ToInt64: Int64;
Var
  I: Int64;
Begin
  Result := 0;
  if TryStrToInt64(Self,I) then
  begin
    result := I;
  End;
end;

function TStringHelper.ToByte: Byte;
Var
  I: Integer;
Begin
  Result := 0;
  if TryStrToInt(Self,I) then
  begin
    result := ClampByte(I);
  End;
end;

function TStringHelper.ComputeHash(const IgnoreCase: Boolean): Integer;
Var
  i, j: Integer;
Begin
  Result :=-1;
  I := UTF8Length(Self);
  J := Ord(Self[I]);
  Repeat
    if IgnoreCase then J := (J * cHash1 + Ord(UpCase(Self[I])))
    else J := (J * cHash1 + Ord(Self[I]));
    Dec(I);
  Until I = 0;
  if IgnoreCase then Result := (J Mod cHash2) else Result := abs(J Mod cHash2);
End;

function TStringHelper.LengthUTF8: Integer;
begin
  result := UTF8Length(Self);
end;

//class operator TStringHelper.:=(const NewString: String): String;
//begin
//  result.SetToUTF8(NewString);
//end;

{%endregion%}

{%region%=====[ TDateTimeHelper ]===============================================}

function TDateTimeHelper.getYear: Word;
var
  Y, M, D: Word;
begin
  DecodeDate(Self, Y, M, D);
  Result := Y;
end;

function TDateTimeHelper.getMonth: Word;
var
  Y, M, D: Word;
begin
  DecodeDate(Self, Y, M, D);
  Result := M;
end;

function TDateTimeHelper.getDay: Word;
var
  Y, M, D: Word;
begin
  DecodeDate(Self, Y, M, D);
  Result := D;
end;

function TDateTimeHelper.getHour: Word;
var
  H,M,S,MS: Word;
begin
  DecodeTime(Self,H,M,S,MS);
  Result := H;
end;

function TDateTimeHelper.getMinute: Word;
var
  H,M,S,MS: Word;
begin
  DecodeTime(Self,H,M,S,MS);
  Result := M;
end;

function TDateTimeHelper.GetSecond: Word;
var
  H,M,S,MS: Word;
begin
  DecodeTime(Self,H,M,S,MS);
  Result := S;
end;

Function TDateTimeHelper.GetMilliSecond : Word;
var
  H,M,S,MS: Word;
begin
  DecodeTime(Self,H,M,S,MS);
  Result := MS;
end;

Procedure TDateTimeHelper.SetToday;
Begin
  Self := Date();
End;

Procedure TDateTimeHelper.SetToNow;
Begin
  Self := Now;
End;

Procedure TDateTimeHelper.SetToTime;
Begin
  Self := Time();
End;

Procedure TDateTimeHelper.SetTime(H, M, S : Integer);
Begin
 if not(TryEncodeDateTime(Year,Month,Day,H,M,S,0, Self)) then Self.SetToNow;
End;

Procedure TDateTimeHelper.SetDate(Y, M, D : Integer);
Begin
 if not(TryEncodeDateTime(Y,M,D,Hour,Minute,Second,0, Self)) then Self.SetToNow;
End;

Procedure TDateTimeHelper.SetFromString(ADateTimeStr : String ;Const APAttern : String = 'YYYY"/"MM"/"DD hh":"nn":"ss');
Begin
  Self := ScanDateTime(APattern, ADateTimeStr);
End;

Procedure TDateTimeHelper.SetFromUnixTimeStamp(ADate : Int64);
Begin
  Self := UnixToDateTime(ADate);
End;

Procedure TDateTimeHelper.SetFromMacTimeStamp(ADate : Int64);
Begin
  Self := MacToDateTime(ADate);
End;

Procedure TDateTimeHelper.SetFromUTC(Ut : TDateTime);
Begin
  Self :=  UniversalTimeToLocal(UT);
End;

Procedure TDateTimeHelper.SetFromUTC(Ut : TDateTime; ZOffset : Integer);
Begin
  Self :=  UniversalTimeToLocal(UT,ZOffset);
End;

Procedure TDateTimeHelper.SetFromJulian(JulianDate : Double);
Begin
 Self := JulianDateToDateTime(JulianDate);
End;

Procedure TDateTimeHelper.SetFromSQL(ADataTimeStr : String);
var
  YYYY, MM, DD: word;
  S:String;
begin
  try
    S := ADataTimeStr.Copy(1, 4);
    YYYY := S.ToInteger;
    // années sur deux chiffres
    if (YYYY < 100) then
    begin
      if (YYYY < 50) then  YYYY := 2000 + YYYY
                     else  YYYY := 1900 + YYYY;
    end;
    MM   := ADataTimeStr.Copy(6, 2).ToInteger;
    DD   := ADataTimeStr.Copy(9, 2).ToInteger;
    Self := EncodeDate(YYYY, MM, DD);
  except
    Self := TDateTime(0.0);
  end;
End;

{ Source : http://codes-sources.commentcamarche.net/source/36519-unite-randdate-fonction-randrangedate-et-randomdate-generatrices-de-dates-aleatoires }
Procedure TDateTimeHelper.SetRandomDate();
var aD,
    MaxD,D,
    MaxM,M,
    MaxY,Y : Integer;
begin
  MaxD := 31;
  MaxM := 12;
  MaxY := 9999;

  Y := RandomRange(1,MaxY+1);
  Y:=Y.Clamp(1,MaxY);
  if Y = MaxY then
  begin
     M := RandomRange(1,MaxM+1);//.Clamp(1,MaxM);
     M := M.Clamp(1,MaxM);
  end
  else
  begin
     M := RandomRange(1,12+1).Clamp(1,12);
     //M:= M.Clamp(1,12);
  end;
  aD := 0;
  case M of
     2               : if SysUtils.IsLeapYear(Y) then aD := 29 else aD := 28;
     4,6,9,11        : aD := 30;
     1,3,5,7,8,10,12 : aD := 31;
  end;
  if M = MaxM then
  Begin
     D := RandomRange(1,MaxD+1);
     D := D.Clamp(1,MaxD)
  end
  else
  begin
     D := RandomRange(1,aD+1);
     D := D.Clamp(1,aD);
  end;
  Self := EncodeDate(Y,M,D);
End;

Procedure TDateTimeHelper.SetRandomDate(StartDate, EndDate : Tdatetime);
var aD,
    MinD,MaxD,D,
    MinM,MaxM,M,
    MinY,MaxY,Y : Word;
begin
  DecodeDate(StartDate,MinY,MinM,MinD);
  DecodeDate(EndDate,MaxY,MaxM,MaxD);
  Y := RandomRange(MinY,MaxY+1);
  Y := Y.Clamp(MinY,MaxY);
  if Y = MinY then
  begin
     M := RandomRange(MinM,12+1);
     M := M.Clamp(MinM,12);
  end
  else if Y = MaxY then
  begin
     M := RandomRange(1,MaxM+1);
     M := M.Clamp(1,MaxM);
  end
  else
  begin
     M := RandomRange(1,12+1).Clamp(1,12);
    // M := M.Clamp(1,12);
  end;
  aD := 0;
  case M of
    2  : if SysUtils.IsLeapYear(Y) then aD := 29 else aD := 28;
    4,6,9,11  : aD := 30;
    1,3,5,7,8,10,12 : aD := 31;
  end;

  if M = MinM then
  Begin
     D := RandomRange(MinD,aD+1);
     D := D.Clamp(MinD,aD);
  end
  else
  if M = MaxM then
  Begin
     D := RandomRange(1,MaxD+1);
     D := D.Clamp(1,MaxD);
  end
  else
  Begin
     D := RandomRange(1,aD+1);
     D := D.Clamp(1,aD);
  end;

  Self := EncodeDate(Y,M,D);
end;

Procedure TDateTimeHelper.SetFromFile(aFileName:String);
Var
  fa : Longint;
Begin
  fa:=FileAgeUTF8(aFileName);
  If Fa<>-1 then
  begin
    Self :=FileDateToDateTime(fa);
  end;
End;

Function TDateTimeHelper.ToString(Const Format : TDateFormat; Const Customformat : String) : String;
begin
  if Format = dfGMT then
    Result := FormatDateTime('ddd, d mmm yyyy hh:nn:ss', Self) + ' GMT'
  else if Format = dfUTC then
    Result := FormatDateTime('ddd, d mmm yyyy hh:nn:ss', Self) + ' UTC'
  else
    result := FormatDateTime(CustomFormat, Self)
end;

Function TDateTimeHelper.ToSQL : String;
var
  YYYY, MM, DD: word;
begin
  DecodeDate(Self, YYYY, MM, DD);
  Result := Format('%.4d-%.2d-%.2d', [YYYY, MM, DD]);
End;

Function TDateTimeHelper.DateToString(Const FormatStr :string = 'dd"/"mm"/"yyyy'): String;
Begin
  Result :=FormatDateTime(FormatStr, Self);
End;

Function TDateTimeHelper.TimeToString(Const FormatStr : string = 'hh:nn:ss') : String;
Begin
  Result :=FormatDateTime(FormatStr, Self);
End;

Function TDateTimeHelper.Compare(SecondDateTime : TDateTime) : Integer;
Begin
  Result := DateUtils.CompareTime(Self,SecondDateTime);
End;

Function TDateTimeHelper.CompareDate(Seconddate : TDateTime) : Integer;
Begin
  Result := DateUtils.CompareDate(Self,SecondDate);
End;

Function TDateTimeHelper.CompareTime(Secondtime : TDateTime) : Integer;
Begin
  Result := DateUtils.CompareTime(Self,SecondTime);
End;

Function TDateTimeHelper.AddYear(Const A : Integer) : Tdatetime;
Begin
  Result := IncYear(Self,A)
End;

Function TDateTimeHelper.AddMonth(Const A : Integer) : Tdatetime;
Begin
  Result := IncMonth(Self,A)
End;

Function TDateTimeHelper.AddDay(Const A : Integer) : Tdatetime;
Begin
  Result := IncDay(Self,A)
End;

Function TDateTimeHelper.AddWeek(Const A : Integer) : Tdatetime;
Begin
  Result := IncWeek(Self,A)
End;

Function TDateTimeHelper.AddHour(Const A : Integer) : Tdatetime;
Begin
  Result := IncHour(Self,A)
End;

Function TDateTimeHelper.AddMinute(Const A : Integer) : Tdatetime;
begin
  Result := IncMinute(Self,A)
end;

Function TDateTimeHelper.AddSecond(Const A : Integer) : Tdatetime;
Begin
  Result := IncSecond(Self,A)
End;

Function TDateTimeHelper.AddMillisecond(Const A : Integer) : Tdatetime;
Begin
  Result := IncMilliSecond(Self,A)
End;

Function TDateTimeHelper.ToJulian : Double;
Begin
   result := DateTimeToJulianDate(Self);
End;

Function TDateTimeHelper.ToUnixTimeStamp : Int64;
Begin
  result := DateTimeToUnix(Self);
End;

Function TDateTimeHelper.ToMacTimeStamp : Int64;
Begin
   Result := DateTimeToMac(Self);
End;

Function TDateTimeHelper.ToUTC : TDateTime;
Begin
  result := LocalTimeToUniversal(Self);
End;

Function TDateTimeHelper.ToUTC(Zoffset : Integer) : TDateTime;
Begin
 result := LocalTimeToUniversal(Self,ZOffset);
End;

Function TDateTimeHelper.IsLeapYear : Boolean;
Begin
  Result := IsInLeapYear(Self);
End;

Function TDateTimeHelper.GetDaysInYear : Word;
Begin
  result := DaysInYear(Self);
End;

Function TDateTimeHelper.GetDaysInMonth : Word;
Begin
 result := DaysInMonth(Self);
End;

Function TDateTimeHelper.GetWeeksInYear : Word;
Begin
 result := WeeksInYear(Self);
End;

Function TDateTimeHelper.GetDayOfTheWeek : Word;
Begin
  Result := DayOfTheWeek(Self);
End;

Function TDateTimeHelper.GetWeekOfTheYear : Word;
Begin
  Result := WeekOfTheYear(Self);
End;

Function TDateTimeHelper.GetElapsedYears(ATo : TDateTime): Integer;
Begin
   Result :=  YearsBetween(Self,ATo);
End;

Function TDateTimeHelper.GetElapsedMonths(ATo : TDateTime): Integer;
Begin
   Result :=  MonthsBetween(Self,ATo);
End;

Function TDateTimeHelper.GetElapsedWeeks(ATo : TDateTime): Integer;
Begin
  Result :=  WeeksBetween(Self,ATo);
End;

Function TDateTimeHelper.GetElapsedDays(ATo : TDateTime): Integer;
Begin
  Result :=  DaysBetween(Self,ATo);
End;

Function TDateTimeHelper.GetElapsedHours(ATo : TDateTime): Int64;
Begin
  Result :=  HoursBetween(Self,ATo);
End;

Function TDateTimeHelper.GetElapsedMinutes(ATo : TDateTime): Int64;
Begin
  Result :=  MinutesBetween(Self,ATo);
End;

Function TDateTimeHelper.GetElapsedSeconds(ATo : TDateTime): Int64;
Begin
  Result :=  SecondsBetween(Self,ATo);
End;

Function TDateTimeHelper.GetElapsedMilliSeconds(ATo : TDateTime): Int64;
Begin
  Result :=  MilliSecondsBetween(Self,ATo);
End;

Function TDateTimeHelper.GetElapsedPeriod(ATo : TDateTime): TDateTime;
Var
  Y,M,D : Word;
Begin
  PeriodBetween(Self, ATo, Y, M, D);
  Result := EncodeDate(Y,M,D);
End;

{%endregion%}

Initialization
  // Format settings to convert a string to a float
  vPointSeparator := DefaultFormatSettings;
  vPointSeparator.DecimalSeparator := '.';
  vPointSeparator.ThousandSeparator := ' ';// disable the thousand separator
  vCommaSeparator := DefaultFormatSettings;
  vCommaSeparator.DecimalSeparator := ',';
  vCommaSeparator.ThousandSeparator := ' ';// disable the thousand separator

Finalization

End.

