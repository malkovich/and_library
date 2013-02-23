unit magsubs1;
{$WARNINGS OFF}
{ Magenta Systems File Transfer Components.
Updated by Angus Robertson, Magenta Systems Ltd, England, 19th Oct 2005
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

This unit contains a wide range of general purpose functions, some written by
Magenta, some contributed by others but often improved
Thanks to Enzo from iMazzo for MSIEAutoDial improvements



WARNING - currently Delphi 5, 6 and 7 only

1st May 2001, Release 4.60
26th June 2001, Release 4.61 - added GetMACAddresses
11th October 2001, Release 4.70 - added IsDestReachable and IsNetAlive
                           added MSIEAutoDial and MSIEDefConn
                           added AddThouSeps, IntToCStr, Int64ToCStr
31st October 2001, Release 4.71 - added DirectoryExists and ForceDirs
6th Dec 2001 - Release 4.72 - added more GetShellPath literals
1st January 2002, Release 4.72 - added ExcludeTrailingPathDelimiter for D4
26th May 2002 - Release 4.80 - moved MagRasOSVersion here from magrasapi
                corrected MSIEDefConn to work with Windows XP
26th July 2002 - Release 4.90
5 Jan 2004     - Release 4.93 added from magsubs4 to support magcopy, magftp, maghttp
11 Jan 2004    - Release 4.93 made Delphi 5 compatible
22 Jan 2004    - added DTtoAlpha, DTTtoAlpha, DateOnlyPacked
30 Jan 2004    - added MaxLongWrd
6 Feb 2004     - PackedDTtoISODT handles dates only, added SecsToHourStr
1 Apr 2004     - added StrCtrlSafe, StrCtrlRest, UnxToDosPath, DosToUnxPath, StrFileTran
31 May 2004    - added GetSize64File, corrected files over 4 gigs not reported correctly
1  Oct 2004    - messing with FileTimeToDateTime to fix 12/00 bug
               - added CheckFileOpen
11 Oct 2004    - Release 4.94 added MSIEAutoDialOpt, fixed MSIEDefConn for XP
25 Nov 2004    - added FormatLastError, IsWinXPE, IsWin2K3
13 Dec 2004    - added CRLF_
14 Jan 2004 -    added Int2Kbytes, Int2Mbytes, IntToKbyte from magsubs4
28 Jan 2005 -    added EmptyRecycleBin, NULLL to avoid conflict with variants
14 Feb 2005 -    more error handling in IndexFiles
22 Mar 2005 -    added ULINE, TrimWorkingSetMemory
18 Apr 2005 -    added GetTickCountX, DiffTicks, ElapsedTicks, ElapsedSecs, GetTrgMsecs, GetTrgSecs, TestTrgTick
6 May 2005  -    added TimeToNStr always numeric hh:mm:ss, and TimeToZStr hh:mm:ss:zzz
                 added DateToAStr/DateTimeToAStr always alpha month and numeric hh:mm:ss
25 July 2005 -   TFindList moved to magclasses.pas
9 Aug 2005   - added IsWinVista, return Windows Vista for NT6
4 Sept 2005  - better 64-bit file size conversion
21 Sept 2005 - added EqualDateTime and DiffDateTime
19 Oct 2005  - fix in ElapsedTicks for zero elapsed 


}

interface

uses
  Sysutils, Windows, Messages, Classes, ShellAPI, nb30, Registry ;


const
        MaxByte: Byte = 255;
        MaxShortInt: ShortInt = 127;
        MaxWord: Word = 65535;
        MaxTriplet: LongInt  = $FFFFFF ;
        MaxLongInt: LongInt  = $7FFFFFFF;
        MaxInteger = $7FFFFFFF;
        MaxLongWord: LongWord = $FFFFFFFF;
        MaxLongWrd = $FFFFFFFF;
        MaxReal: Real = 1.7e38;
        MaxSingle: Single = 3.4e38;
        MaxDouble: Double = 1.7e308;
        MaxExtended: Extended = 1.1e4932;
        MaxComp: Comp = 9.2e18;

        MinByte: Byte = 0;
        MinShortInt: ShortInt = -128;
        MinInt: Integer = -32768;
        MinWord: Word = 0;
        MinLongInt = $80000000;
        MinReal: Real = 2.9e-39;
        MinSingle: Single = 1.5e-45;
        MinDouble: Double = 5.0e-324;
        MinExtended: Extended = 3.4e-4932;

const
  { several important ASCII codes }
  NULL            =  #0;
  NULLL           =  #0;
  BACKSPACE       =  #8;
  TAB             =  #9;
  LF              = #10;
  FF               = #12;
  CR              = #13;
  EOF_            = #26;
  ESC             = #27;
  FIELDSEP        = #28;
  RECSEP          = #30;
  BLANK           = #32;
  SQUOTE          = #39 ;
  DQUOTE          = #34 ;
  SPACE           = BLANK;
  SLASH           = '\';     { used in filenames }
  BSLASH          = '\';     { used in filenames }
  HEX_PREFIX      = '$';     { prefix for hexnumbers }
  COLON           = ':';
  FSLASH          = '/';
  COMMA           = ',';
  PERIOD          = '.';
  ULINE           = '_';
  CRLF            : PChar = CR+LF;
  CRLF_           = CR+LF;
  UNICODESIG      : PChar = #255 + #254 ;
  ASCII_NULL      = #0;
  ASCII_BELL      = #7;
  ASCII_BS        = #8;
  ASCII_HT        = #9;
  ASCII_LF        = #10;
  ASCII_CR        = #13;
  ASCII_EOF       = #26;
  ASCII_ESC       = #27;
  ASCII_SP        = #32;
  c_Tab           = ASCII_HT;
  c_Space         = ASCII_SP;
  c_EOL           = ASCII_CR + ASCII_LF;
  c_DecimalPoint  = '.';

  { digits as chars }
  ZERO   = '0';  ONE  = '1';  TWO    = '2';  THREE  = '3';  FOUR  = '4';
  FIVE   = '5';  SIX  = '6';  SEVEN  = '7';  EIGHT  = '8';  NINE  = '9';

  { special codes }

  { common computer sizes }
  KBYTE           = Sizeof(Byte) shl 10;
  MBYTE           = KBYTE        shl 10;
  GBYTE           = MBYTE        shl 10;

  DIGITS          : set of Char = [ZERO..NINE];

  NumPadCh: Char = ' '; // Character to use for Left Hand Padding of Numerics

  MinsPerDay = SecsPerDay / 60 ;
  OneSecond: TDateTime = 1 / SecsPerDay ;
  OneMinute: TDateTime = 1 / (SecsPerDay / 60) ;
  OneHour: TDateTime = 1 / (SecsPerDay / (60 * 60)) ;
  FileTimeBase = -109205.0;   // days between years 1601 and 1900
  FileTimeStep: Extended = 24.0 * 60.0 * 60.0 * 1000.0 * 1000.0 * 10.0; // 100 nsec per Day
  FileTimeSecond: int64 = 10000000 ;
  FileTime1980: int64 = 119600064000000000 ;
  FileTime1990: int64 = 122756256000000000 ;
  FileTime2000: int64 = 125911584000000000 ;
  TicksPerDay: longword =  24 * 60 * 60 * 1000 ;
  TicksPerHour: longword = 60 * 60 * 1000 ;
  TicksPerMinute: longword = 60 * 1000 ;
  TicksPerSecond: longword = 1000 ;

  DateOnlyPacked = 'yyyymmdd' ;
  DateMaskPacked = 'yyyymmdd"-"hhnnss' ;
  DateMaskXPacked = 'yyyymmdd"-"hhnnss"-"zzz' ;
  TimeMaskPacked = 'hhnnss' ;
  ISODateMask = 'yyyy-mm-dd' ;
  ISODateTimeMask = 'yyyy-mm-dd"T"hh:nn:ss' ;
  ISOTimeMask = 'hh:nn:ss' ;
  LongTimeMask = 'hh:nn:ss:zzz' ;
  FullDateTimeMask = 'yyyy/mm/dd"-"hh:nn:ss' ;
  DateAlphaMask = 'dd-mmm-yyyy' ;
  ShortTimeMask = 'hh:nn' ;
  SDateMaskPacked = 'yymmddhhnnss' ;
  DateTimeAlphaMask = 'dd-mmm-yyyy hh:nn:ss' ;

// SQL paramater constants
  paramY = SQUOTE + 'Y' + SQUOTE {+ SPACE} ;
  paramN = SQUOTE + 'N' + SQUOTE {+ SPACE} ;
  paramBlank = SQUOTE + SQUOTE ;
  paramSep = ',' ;
  paramNull = 'NULL' ;

type
  CharSet = Set of Char;
  CharSetArray = array of CharSet;
  StringArray = array of string;
  T2DimStrArray = array of array of string ;
  TIntegerArray = array of integer ;

const

// file type extensions - should be in windows.pas, but missing
  FILE_ATTRIBUTE_DEVICE               = $00000040 ;    // old encrypt
  FILE_ATTRIBUTE_SPARSE_FILE          = $00000200 ;    // file is missing records
  FILE_ATTRIBUTE_REPARSE_POINT        = $00000400 ;    // attached function??
  FILE_ATTRIBUTE_NOT_CONTENT_INDEXED  = $00002000 ;
  FILE_ATTRIBUTE_ENCRYPTED            = $00004000 ;    // W2K encrypt

  faNormal = FILE_ATTRIBUTE_COMPRESSED OR FILE_ATTRIBUTE_NORMAL OR
       FILE_ATTRIBUTE_ENCRYPTED OR FILE_ATTRIBUTE_NOT_CONTENT_INDEXED ;    // NTFS
  faNormArch = faNormal OR faArchive ;

// SENS Connectivity APIs from sensapi.h - MSIE5 and later only

const
    SensapiDLL = 'SENSAPI.DLL' ;

{ Literals for IsDestReachable, values for QocInfo.dwFlags.  }
    NETWORK_ALIVE_LAN   = $00000001 ;
    NETWORK_ALIVE_WAN   = $00000002 ;
    NETWORK_ALIVE_AOL   = $00000004 ;

{ Structure for IsDestReachable, dwFlags   }
type
    PQocInfo = ^TQocInfo;
    TQocInfo = record
        dwSize: DWORD;
        dwFlags: DWORD;
        dwInSpeed: DWORD;
        dwOutSpeed: DWORD;
    end;

// performance counter and NowPC stuff
var
   PerfFreqCountsPerSec: int64 ;
   f_PCStartValue: int64 ;
   f_TDStartValue: TDateTime ;
   f_PCCountsPerDay: extended ;
   PerfFreqAligned: boolean = False ;  // clear if clock changes
   TicksTestOffset: longword ;  // 18 Apr 2005, for testing GetTickCount

// functions exported by this unit

{ Converts a fixed length PChar string into a Delphi ANSI string, leaving
  any embedded or trailing nulls. }
function FixedToPasStr (fixstr: PChar; fixsize: integer): string ;

{ Similar to FixedToPasStr, but specialised to split a PChar string into
  two Delphi ANSI strings at the first embedded null. }
function GetDevNamePort (fixstr: PChar; fixsize: integer;
                                            var devport: string): string ;

{ Simple version of StrToInt that does not raise any exceptions, it returns
  zero for an illegal input. }
function AscToInt (value: string): Integer;

{ Simple version of StrToInt64 that does not raise any exceptions, it returns
  zero for an illegal input. }
function AscToInt64 (value: string): Int64 ;

{ Adds thousand separators to an ASCII string (no checking for digits).
  The separator value comes from ThousandSeparator and is typically a
  comma. }
function AddThouSeps (const S: string): string;

{ Converts a 32-bit integer numeric value into a string with thousand
  separators, typically commas, calls AddThouSeps. }
function IntToCStr (const N: integer): string ;

{ Converts a 64-bit integer numeric value into a string with thousand
  separators, typically commas, calls AddThouSeps. }
function Int64ToCStr (const N: int64): string ;

{ Returns the path of the windows directory. }
function GetWinDir: String;

{ Returns the path of a specific windows shell directory, using the
  CSIDL_xx literals. }
function GetShellPath (location: integer): string ;

{ Returns the user name of the current thread. This is the name of the user
 currently logged onto the system. }
function GetUsersName: string;

{ Returns the NetBIOS name of the local computer. This name is established
  at system startup, when the system reads it from the registry.}
function GetCompName: string;

{ Returns Delphi TDateTime converted from a UNIX time stamp, being the
  number of seconds since 1st January 1970. }
function TStamptoDT (stamp: DWORD): TDateTime ;

{ Gets program version information from the string resources keys in an
  EXE or DLL file.

  AppName is the EXE or DLL file name, KeyName is the literal describing the
  key for which information should be returned, includes 'FileDescription',
  'FileVersion', 'ProductVersion' (see Delphi Project Options, Version Info
  for more keys). }
function GetFileVerInfo (const AppName, KeyName: string): string ;

{ Get one or more ethernet card MAC addresses for a specified PC using
  NetBIOS commands.  Pcname is blank for the current PC, or specifies the
  computer name from which to obtain the MAC addresses.  MacAddresses
  is a TStringList that is returned with ASCII representations of the
  MAC address in hex format, ie 00-30-84-27-C2-1E.  Result is number of
  MAC addresses returned, or -1 for an error.  }
function GetMACAddresses (Pcname: string; MacAddresses: TStrings): integer ;

{ Load the Synchronization Manager SENSAPI.DLL, returns false if it's
  not available.  The DLL is only available with MSIE 5 and later, or
  Win98, W2K, XP.  }
function LoadSensapi: Boolean;

{ Determines whether the local system is connected to a network and the
  type of network connection, for example, LAN, WAN, or both. Flags returns
  NETWORK_ALIVE_LAN or NETWORK_ALIVE_WAN (RAS).  Result is true if there
  is a network connection.  Note a LAN connection does not necessarily
  mean that internet access is also available.
  Requires MSIE 5 or later, or Win98, W2K, XP.   }
function IsNetAlive (var Flags: DWORD): boolean ;

{ Determines if the specified destination can be reached and provides
  Quality of Connection (QOC) information for the destination.  Dest can
  be an IP address, a UNC name, or an URL, which will be pinged or blank,
  if QOC only is required.  Returns true if the destination was specified
  and can be reached, QocInfo.Flags returns NETWORK_ALIVE_LAN or
  NETWORK_ALIVE_WAN (RAS), dwInSpeed/dwOutSpeed are the network adaptor
  speed, 1000000 or 100000000 for LANs, 34000, 56000, 64000, etc for RAS.
  Note you can not check the speed of a specific device as such, only a route.
  Warning - this API does not appear to be totally reliable, ping often fails
  or gets blocked.
  Requires MSIE 5 or later, or Win98, W2K, XP.   }
function IsDestReachable (Dest: string; var QocInfo: TQocInfo): boolean ;

// MSIE internet options
const
    CVKey = 'Software\Microsoft\Windows\CurrentVersion' ;

{ Allows the MSIE Internet Option 'Dial-Up Settings' to be checked or
  set, Value is true for 'always dial my default connection', false for
  'never dial a connection' ('dial whenever a connection is not available
  is not supported by this function).  If Update is false, Value returns
  the current setting, if Update is true the Value should be set with the
  new setting.  Result is false if there's a registy error.
  Effectively this is an auto dial option, when set true any application
  attempting to access a remote internet server will cause RAS to dial
  the default connection (see MSIEDefConn).  }
function MSIEAutoDial (var Value: boolean; const Update: boolean): boolean ;

{ Allows the MSIE Internet Option 'Dial-Up Settings' to be checked or set:
     0=Never Dial A Connection
     1=Dial Whenever A Network Connection Is Not Present
     2=Always Dial My Default Connection
  If Update is false, Value returns the current setting, if Update is true
  the Value should be set with the new setting.  Result is false if there's
  a registy error.  Effectively this is an auto dial option, causing any
  applicationattempting to access a remote internet server will cause RAS to
  dial the default connection (see MSIEDefConn).  }
function MSIEAutoDialOpt (var Value: integer; const Update: boolean): boolean;

{ Allows the MSIE Internet Option 'Dial-Up Settings' default or current
  connection entry to be specified.  ConnName is the name is the default
  connection entry.  If Update is false, Value returns the name of the
  current default entry, if Update is true the Value should be set with
  the new default connection.  Result is false if there's a registy error.
  This default connection entry is that used by auto dial,
  see MSIEAutoDial. }
function MSIEDefConn (var ConnName: string; const Update: boolean): boolean ;

function DirectoryExists(const Name: string): Boolean;

function ForceDirs(Dir: string): Boolean;

// OS version stuff
function IsWin95: boolean ;
function IsWinNT: boolean ;
function IsWin2K: boolean ;
function IsWinXP: boolean ;
function IsWinXPE: boolean ;
function IsWin2K3: boolean ;
function IsWinVista: boolean ;
function GetOSVersion: string ;
procedure GetOSInfo ;

// validation routines
function IsDigit(Ch : Char) : Boolean;
function IsDigitsDec (info: string; decimal: boolean) : boolean ;
function IsDigits (info: string) : boolean ;

procedure ConvHexStr (instr: string; var outstr: string) ;
procedure ByteSwaps(DataPtr : Pointer;NoBytes : integer);
function ConIntHex (value: cardinal): string ;  // 32-bit to 8 byte hex
function StripQuotes (filename: string): string ;
function StripNewLines (const S: string): string;

// directory and file listing
function IndexFiles (searchfile: string; mask: integer;
                   var FileList: TStringList; var totsize: cardinal): integer ;
function DeleteOldFiles (fname: string): integer ;

function GetEnvirVar (name: string): string ;
function StripChars (AString, AChars: string): string ;
function UpAndLower(const S: string): string;
function StripChar (const AString: string; const AChar: Char): string ;
function StripSpaces (const AString: string): string ;
function StripCommas (const AString: string): string ;
function StripNulls (const AString: string): string ;
function StripAllCntls (const AString: string): string;

procedure StringTranCh (var S: string; FrCh, ToCh: Char) ;
procedure StringCtrlSafe (var S: string) ;
procedure StringCtrlRest (var S: string) ;
procedure StringFileTran (var S: string) ;
function StringRemCntls (var S: string): boolean ;
procedure DosToUnixPath (var S: string) ;
procedure UnixToDosPath (var S: string) ;
function StrCtrlSafe (const S: string): string ;
function StrCtrlRest (const S: string): string ;
function UnxToDosPath (const S: string): string ;
function DosToUnxPath (const S: string): string ;
function StrFileTran (const S: string): string ;

{ Copy                                                                         }
{   Variantions on Delphi's Copy. Just like Delphi's Copy, illegal values for  }
{   Start (<1,>len), Stop (<start,>len) and Count (<0,>end) are tolerated.     }
Function CopyRange (const S : String; const Start, Stop : Integer) : String;
Function CopyFrom (const S : String; const Start : Integer) : String;
Function CopyLeft (const S : String; const Count : Integer) : String;
Function CopyRight (const S : String; const Count : Integer = 1) : String;

{ Match                                                                        }
{   True if M matches S [Pos] (or S [Pos..Pos+Count-1])                        }
{   Returns False if Pos or Count is invalid                                   }
Function Match (const M : CharSet; const S : String; const Pos : Integer = 1;
         const Count : Integer = 1) : Boolean; overload;
Function Match (const M : CharSetArray; const S : String; const Pos : Integer = 1)
         : Boolean; overload;
Function Match (const M, S : String; const Pos : Integer = 1) : Boolean; overload;           // Blazing

{ PosNext                                                                      }
{   Returns first Match of Find in S after LastPos.                            }
{   To find the first match, set LastPos to 0.                                 }
{   Returns 0 if not found or illegal value for LastPos (<0,>length(s))        }
Function PosNext (const Find : CharSet; const S : String;
         const LastPos : Integer = 0) : Integer; overload;
Function PosNext (const Find : CharSetArray; const S : String;
         const LastPos : Integer = 0) : Integer; overload;
Function PosNext (const Find : String; const S : String;
         const LastPos : Integer = 0) : Integer; overload;
Function PosPrev (const Find : String; const S : String;
         const LastPos : Integer = 0) : Integer;
{ PosN                                                                         }
{   Finds the Nth occurance of Find in S from the left or the right.           }
Function PosN (const Find, S : String; const N : Integer = 1;
         const FromRight : Boolean = False) : Integer;

{ Split/Join                                                                   }
{   Splits S into pieces seperated by Delimiter. If Delimiter='' or S='' then  }
{   returns an empty list. If Token not found in S returns list with one       }
{   item, S.                                                                   }
Function StrArraySplit (const S : String; const Delimiter : String = c_Space) : StringArray;
Function StrArrayJoin (const S : StringArray; const Delimiter : String = c_Space) : String;
procedure StrArrayInsert (var S: StringArray; index: integer; T: string) ;
procedure StrArrayDelete (var S: StringArray; index: integer) ;
procedure StrArrayToList (S: StringArray; var T: TStringList) ;
procedure StrArrayFromList (T: TStringList; var S: StringArray) ;
function StrArrayPosOf (L: string; S: StringArray): integer ;

// file time stamp stuff
function FileTimeToInt64 (const FileTime: TFileTime): Int64 ;
function Int64ToFileTime (const FileTime: Int64): TFileTime ;
function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
function DateTimeToFileTime(DateTime: TDateTime): TFileTime;
function FileTimeToSecs2K (const FileTime: TFileTime): integer ;
function CheckFileOpen(const FName: String): integer;
function GetSizeFile (filename: string): LongInt;
function GetSize64File (filename: string): Int64 ;
function GetFUAgeSizeFile (filename: string ; var FileTime: TFileTime ;
                                            var FSize: Int64): boolean ;
function GetUAgeSizeFile (filename: string ; var FileDT: TDateTime;
                                            var FSize: Int64): boolean ;
function GetFAgeSizeFile (filename: string ; var FileTime: TFileTime ;
                                            var FSize: Int64): boolean ;
function GetAgeSizeFile (filename: string ; var FileDT: TDateTime;
                                            var FSize: Int64): boolean ;
function TrimSpRight(const S: string): string;
function ExtractNameOnly (FileName: string): string;

function GetExceptMess (ExceptObject: TObject): string;

{ Converts a String into a LongInt }
function Str2LInt (const S: String): LongInt;

{ Converts a String into a Word }
function Str2Word (const S: String): Word;

{ Converts a String into a Byte }
function Str2Byte (const S: String): Byte;

{ Converts a String into a ShortInt }
function Str2SInt (const S: String): ShortInt;

{ Converts a String into an Integer }
function Str2Int (const S: String): Integer;

{ Converts a LongInt into a String of length N with
        zeros Padding to the Left }
function Int2StrZ (const L: LongInt; const Len: Byte): String;

{ Converts a LongInt into a String of length N with
        NumPadCh Padding to the Left }
function LInt2Str (const L: LongInt; const Len: Byte): String;

{ Converts a LongInt into a String of length N with
        NumPadCh Padding to the Left }
function Byte2Str (const L: LongInt; const Len: Byte): String;

{ Converts a LongInt into a String of length N with
        NumPadCh Padding to the Left }
function LInt2ZStr (const L: LongInt; const Len: Byte): String;

{ Converts a LongInt into a String of length N with
        NumPadCh Padding to the Left, with blanks returned
       if Value is 0 }
function LInt2ZBStr (const L: LongInt; const Len: Byte): String;

{ Convert a LongInt into a Comma'ed String of length Len,
        with NumPadCh Padding to the Left }
function LInt2CStr (const L : LongInt; const Len : Byte): string;

{ Convert a LongInt into an exact String, No Padding }
function LInt2EStr (const L: LongInt): String;

{ Convert a LongInt into an exact String, No Padding,
        with null returned if Value is 0 }
function LInt2ZBEStr (const L: LongInt): String;

{ Convert a LongInt into a Comma'ed String without Padding }
function LInt2CEStr (const L : LongInt): string;

{ Convert an Int64 to a comma'ed string, no padding }
function Int642CEStr (const L : Int64): string;

{ Returns a string composed of N occurrences of Ch. }
function FillStr (const Ch : Char; const N : Integer): string;

{ Returns a string composed of N blank spaces (i.e. #32) }
function BlankStr (const N : Integer): string;

{ Returns a string composed of N occurrences of '-'. }
function DashStr (const N : Integer): String;

{ Returns a string composed of N occurrences of '='. }
function DDashStr (const N : Integer): string;

{ Returns a string composed of N occurrences of '? (196). }
function LineStr (const N : Integer): string;

{ Returns a string composed of N occurrences of '? (205). }
function DLineStr (const N : Integer): string;

{ Returns a string composed of N occurrences of '*'. }
function StarStr (const N : Integer): string;

{ Returns a string composed of N occurrences of '#'. }
function HashStr (const N : Integer): string;

{ Returns a string with blank spaces added to the end of the
        string until the string is of the given length.
        If Length (S) >= Len then NO padding occurs, and S is returned. }
function PadRightStr (const S : string; const Len : Integer): string;

{ Returns a string with blank spaces added to the beginning of the
        string until the string is of the given length.
        If Length (S) >= Len then NO padding occurs, and S is returned. }
function PadLeftStr (const S : string; const Len : Integer): string;

{ Returns a string with specified characters added to the beginning of the
        string until the string is of the given length.
        If Length (S) >= Len then NO padding occurs, and S is returned. }
function PadChLeftStr (const S : string; const Ch : Char; const Len : Integer): string;

{ time functions }
{$IFDEF VER130} // D5
function TryEncodeTime(Hour, Min, Sec, MSec: Word; var Time: TDateTime): Boolean;
function TryEncodeDate(Year, Month, Day: Word; var Date: TDateTime): Boolean;
{$ENDIF}
function DateTimeToAStr(const DateTime: TDateTime): string; // always alpha month and numeric hh:mm:ss
function DateToAStr(const DateTime: TDateTime): string; // always alpha month
function TimeToNStr(const DateTime: TDateTime): string; // always numeric hh:mm:ss
function TimeToZStr(const DateTime: TDateTime): string; // always numeric hh:mm:ss:zzz
function timeHour(T: TDateTime): Integer;
function timeMin(T: TDateTime): Integer;
function timeSec(T: TDateTime): Integer;
function timeToInt(T: TDateTime): Integer;  // seconds
function HoursToTime (hours: integer): TDateTime ;
function MinsToTime (mins: integer): TDateTime ;
function SecsToTime (secs: integer): TDateTime ;
function TimerToStr (duration: TDateTime): string ;
function PackedISO2Date (info: string): TDateTime ;
function PackedISO2UKStr (info: string): string ;
function DTtoISODT (D: TDateTime): string ;
function AlphaDTtoISODT (sdate, stime: string): string ;
function ISODTtoPacked (ISO: string): string ;
function PackedDTtoISODT (info: string): string ;
function QuoteNull (S: string): string ;
function QuoteSQLDate (D: TDateTime): string ;
function QuoteSQLTime (T: TDateTime): string ;
function Str2DateTime (const S: String): TDateTime ;
function Str2Time (const S: String): TDateTime ;
function Packed2Secs (info: string): integer ;
function Packed2Time (info: string): TDateTime ;
function Packed2Date (info: string): TDateTime ;
function Date2Packed (infoDT: TDateTime): string ;
function Date2XPacked (infoDT: TDateTime): string ;
function ConvUKDate (info: string): TDateTime ;
function ConvUSADate (info: string): TDateTime ;
function SecsToMinStr (secs: integer): string ;
function SecsToHourStr (secs: integer): string ;
function ConvLongDate (info: string): TDateTime ;
function DTtoAlpha (D: TDateTime): string ;
function DTTtoAlpha (D: TDateTime): string ;
function EqualDateTime(const A, B: TDateTime): boolean;
function DiffDateTime(const A, B: TDateTime): integer ; 

function sysTempPath: string ;
procedure sysBeep ;
function sysWindowsDir: String ;

function strLastCh(const S: String): Char;
procedure strStripLast(var S: String);
function strAddSlash(const S: String): String;
function strDelSlash(const S: String): String;
function ExtractUNIXPath(const FileName: string): string;
function ExtractUNIXName(const FileName: string): string;

function CharPos (TheChar: Char; const Str: String): Integer;
function DownCase( ch : Char ) : Char;
function ConvHexQuads (S: string): string ;

// better Now, accurate to nano-seconds (relatively)
function NowPC : TDateTime;
function GetPerfCountsPerSec: int64 ;
function PerfCountCurrent: int64 ;
function PerfCountToMilli (LI: int64): integer ;
function PerfCountGetMilli (startLI: int64): integer ;
function PerfCountGetMillStr (startLI: int64): string ;

function InetParseDate(const DateStr: string): TDateTime;
function URLEncode(const psSrc: string): string;
function URLDecode(const AStr: String): String;

function FormatLastError: string ;
function Int2Kbytes (value: integer): string ;
function Int2Mbytes (value: int64): string ;
function IntToKbyte (Value: Int64): String;  

procedure EmptyRecycleBin (fname: string) ;
procedure TrimWorkingSetMemory ;

// working with ticks
function GetTickCountX: longword ;
function DiffTicks (const StartTick, EndTick: longword): longword ;
function ElapsedTicks (const StartTick: longword): longword ;
function ElapsedSecs (const StartTick: longword): longword ;
function GetTrgMSecs (const MilliSecs: integer): longword ;
function GetTrgSecs (const DurSecs: integer): longword ;
function TestTrgTick (const TrgTick: longword): boolean ;


{ Literals for GetShellPath, to get the windows path to specified
 system shell directories. }
const
    CSIDL_DESKTOP                 = $0000 ;     // <desktop>
    CSIDL_INTERNET                = $0001 ;     // Internet Explorer (icon on desktop)
    CSIDL_PROGRAMS                = $0002 ;     // Start Menu\Programs
    CSIDL_CONTROLS                = $0003 ;     // My Computer\Control Panel
    CSIDL_PRINTERS                = $0004 ;     // My Computer\Printers
    CSIDL_PERSONAL                = $0005 ;     // My Documents
    CSIDL_FAVORITES               = $0006 ;     // <user name>\Favorites
    CSIDL_STARTUP                 = $0007 ;     // Start Menu\Programs\Startup
    CSIDL_RECENT                  = $0008 ;     // <user name>\Recent
    CSIDL_SENDTO                  = $0009 ;     // <user name>\SendTo
    CSIDL_BITBUCKET               = $000a ;     // <desktop>\Recycle Bin
    CSIDL_STARTMENU               = $000b ;     // <user name>\Start Menu
    CSIDL_MYDOCUMENTS             = $000c ;     // the user's My Documents folder
    CSIDL_MYMUSIC                 = $000d ;
    CSIDL_MYVIDEO                 = $000e ;
    CSIDL_DESKTOPDIRECTORY        = $0010 ;     // <user name>\Desktop         16
    CSIDL_DRIVES                  = $0011 ;     // My Computer
    CSIDL_NETWORK                 = $0012 ;     // Network Neighborhood
    CSIDL_NETHOOD                 = $0013 ;     // <user name>\nethood
    CSIDL_FONTS                   = $0014 ;     // windows\fonts               20
	CSIDL_TEMPLATES               = $0015 ;
    CSIDL_COMMON_STARTMENU        = $0016 ;     // All Users\Start Menu
    CSIDL_COMMON_PROGRAMS         = $0017 ;     // All Users\Programs
    CSIDL_COMMON_STARTUP          = $0018 ;     // All Users\Startup           24
    CSIDL_COMMON_DESKTOPDIRECTORY = $0019 ;     // All Users\Desktop
    CSIDL_APPDATA                 = $001a ;     // <user name>\Application Data
    CSIDL_PRINTHOOD               = $001b ;     // <user name>\PrintHood
	CSIDL_LOCAL_APPDATA           = $001C ;     // non roaming, user\Local Settings\Application Data
    CSIDL_ALTSTARTUP              = $001d ;     // non localized startup
    CSIDL_COMMON_ALTSTARTUP       = $001e ;     // non localized common startup 30
    CSIDL_COMMON_FAVORITES        = $001f ;
 	CSIDL_INTERNET_CACHE          = $0020 ;
 	CSIDL_COOKIES                 = $0021 ;
 	CSIDL_HISTORY                 = $0022 ;     //                                34
 	CSIDL_COMMON_APPDATA          = $0023 ;     // All Users\Application Data
 	CSIDL_WINDOWS                 = $0024 ;     // GetWindowsDirectory()
 	CSIDL_SYSTEM                  = $0025 ;     // GetSystemDirectory()
 	CSIDL_PROGRAM_FILES           = $0026 ;     // C:\Program Files             38
 	CSIDL_MYPICTURES              = $0027 ;     // My Pictures, new for Win2K
 	CSIDL_PROGRAM_FILES_COMMON    = $002b ;     // C:\Program Files\Common
 	CSIDL_COMMON_DOCUMENTS        = $002e ;     // All Users\Documents          46
 	CSIDL_COMMON_ADMINTOOLS       = $002f ;     // All Users\Start Menu\Programs\Administrative Tools
	CSIDL_ADMINTOOLS              = $0030 ;     // <user name>\Start Menu\Programs\Administrative Tools  48
    CSIDL_CONNECTIONS             = $0031 ;     // Network and Dial-up Connections - not Win9x           49
    CSIDL_COMMON_MUSIC            = $0035 ;
    CSIDL_COMMON_PICTURES         = $0036 ;
    CSIDL_COMMON_VIDEO            = $0037 ;
    CSIDL_RESOURCES               = $0038 ;
    CSIDL_RESOURCES_LOCALIZED     = $0039 ;
    CSIDL_COMMON_OEM_LINKS        = $003A ;
    CSIDL_CDBURN_AREA             = $003B ;
    CSIDL_COMPUTERSNEARME         = $003D ;

    CSIDL_FLAG_CREATE             = $8000 ;     // combine with CSIDL_ value to force folder creation in SHGetFolderPath()
    CSIDL_FLAG_DONT_VERIFY        = $4000 ;     // combine with CSIDL_ value to return an unverified folder path
    CSIDL_FLAG_NO_ALIAS           = $1000 ;
    CSIDL_FLAG_PER_USER_INIT      = $0800 ;
    CSIDL_FLAG_MASK               = $FF00 ;     // mask for all possible flag values

// literals for SHEmptyRecycleBin
  const
  SHERB_NOCONFIRMATION = $00000001;
  SHERB_NOPROGRESSUI = $00000002;
  SHERB_NOSOUND = $00000004;

type
  TOSVERSIONINFOEX = record  // NT4 SP6 and later - not Win9x
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of AnsiChar; { Maintenance string for PSS usage }
    wServicePackMajor: WORD ;
    wServicePackMinor: WORD ;
    wSuiteMask: WORD ;
    wProductType: BYTE ;
    wReserved: BYTE;
  end;

var
OsInfo: TOSVERSIONINFOEX ;

function GetVersionEx2 (var lpVersionInfo: TOSVERSIONINFOEX): BOOL; stdcall;
function GetVersionEx2; external kernel32 name 'GetVersionExA';

CONST
// wProductType
    VER_NT_WORKSTATION                   = $0000001 ;
    VER_NT_DOMAIN_CONTROLLER             = $0000002 ;
    VER_NT_SERVER                        = $0000003 ;

// wSuiteMask
    VER_SERVER_NT                       = $80000000 ;
    VER_WORKSTATION_NT                  = $40000000 ;
    VER_SUITE_SMALLBUSINESS             = $00000001 ;
    VER_SUITE_ENTERPRISE                = $00000002 ;
    VER_SUITE_BACKOFFICE                = $00000004 ;
    VER_SUITE_COMMUNICATIONS            = $00000008 ;
    VER_SUITE_TERMINAL                  = $00000010 ;
    VER_SUITE_SMALLBUSINESS_RESTRICTED  = $00000020 ;
    VER_SUITE_EMBEDDEDNT                = $00000040 ;
    VER_SUITE_DATACENTER                = $00000080 ;
    VER_SUITE_SINGLEUSERTS              = $00000100 ;
    VER_SUITE_PERSONAL                  = $00000200 ;
    VER_SUITE_BLADE                     = $00000400 ;
    VER_SUITE_EMBEDDED_RESTRICTED       = $00000800 ;
    VER_SUITE_SECURITY_APPLICANCE       = $00001000 ;

// descendent of TList added a Find function using binary search identical to sorting
// following now in magclass.pas
(* type
  TFindList = class(TList)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    function Find(const Item2: Pointer; Compare: TListSortCompare;
                                    var index: longint): Boolean; virtual;
  published
    { Published declarations }
  end;  *)


// handle for DLL
var
    SensapiModule: THandle;

// OS version checking, 4.80 moved from magrasapi

type
  TOSVersion = (OSW9x, OSNT4, OSW2K, OSWXP) ;
var
  MagRasOSVersion: TOSVersion ;



// ----------------------------------------------------------------------------

// externals
var
IsDestinationReachable: function (lpszDestination: PChar ;
                                       var QocInfo: TQocInfo): bool; stdcall;

IsNetworkAlive: function (var Flags: DWORD): bool; stdcall;

function SHGetSpecialFolderLocation (handle: HWND; nFolderL: integer;
								     LPITEMIDLIST: pointer): bool stdcall;
function SHGetPathFromIDList (LPCITEMIDLIST: pointer;
                    						pszPath: PChar): bool stdcall;

function SHEmptyRecycleBin (Wnd:HWnd; LPCTSTR:PChar; DWORD:Word):Integer; stdcall;

function SHGetSpecialFolderLocation; external shell32
									    name 'SHGetSpecialFolderLocation';

function SHGetPathFromIDList; external shell32 name 'SHGetPathFromIDList';

function SHEmptyRecycleBin; external shell32 name 'SHEmptyRecycleBinA';


implementation


// convert fixed length trailing null string to pascal ansi string

function FixedToPasStr (fixstr: PChar; fixsize: integer): string ;
var
    temp: string ;
begin
    SetLength (temp, fixsize);
    Move (fixstr^, PChar (temp)^, fixsize);    // may include embedded nulls
    result := temp ;
end ;

// separate two PChar strings

function GetDevNamePort (fixstr: PChar; fixsize: integer;
                                            var devport: string): string ;
var
    I: integer ;
    temp: string ;
begin
    temp := TrimRight (FixedToPasStr (fixstr, fixsize)) ;
    devport := '' ;
    I := pos (#0, temp) ;  // see if port follows device, NT only
    if I > 1 then
    begin
        temp [I] := '{' ;
        devport := lowercase (trim (copy (temp, I + 1, 99))) ;
        result := trim (copy (temp, 1, I - 1)) ;
    end
    else
        result := temp ;
end ;

// returns %System root%

function GetWinDir: String;
var
    Path: array [0..255] of char;
    PLen: DWORD ;
begin
    PLen := 255 ;
    Path [0] := #0 ;
    GetWindowsDirectory (Path, PLen) ;
    Result := Path ;
end;

// returns a shell path according to the CSIDL literals, ie CSIDL_STARTUP

function GetShellPath (location: integer): string ;
var
    PIDL: Pointer;
	Path: array [0..255] of char;
begin
    result := '' ;
    Path [0] := #0 ;
    SHGetSpecialFolderLocation (HInstance, location, @PIDL) ;
	if SHGetPathFromIDList (PIDL, Path) then result := Path ;
end ;

// Get the name of the currently logged in user

function GetUsersName: string;
var
    Buffer: array[0..64] of char;
    NLen: DWORD;
begin
    Buffer [0] := #0 ;
    NLen := 64 ;
    result := '' ;
    if GetUserName (Buffer, NLen) then result := Buffer ;
end;

// get the computer name from networking

function GetCompName: string;
var
    Buffer: array[0..64] of char;
    NLen: DWORD;
begin
    Buffer [0] := #0 ;
    NLen := 64 ;
    result := '' ;
    if GetComputerName (Buffer, NLen) then result := Buffer ;
end ;

// convert seconds since 1 Jan 1970 (UNIX time stamp) to proper Delphi stuff

function TStamptoDT (stamp: DWORD): TDateTime ;
begin
    result := (stamp / SecsPerDay) + 25569 ;
end ;

// This function gets program version information from the string resources
// keys include FileDescription, FileVersion, ProductVersion

function GetFileVerInfo (const AppName, KeyName: string): string ;
const
    DEFAULT_LANG_ID       = $0409;
    DEFAULT_CHAR_SET_ID   = $04E4;
type
    TTranslationPair = packed record
        Lang,
        CharSet: word;
    end;
    PTranslationIDList = ^TTranslationIDList;
    TTranslationIDList = array[0..MAXINT div SizeOf(TTranslationPair)-1] of TTranslationPair;
var
    buffer, PStr: PChar ;
    bufsize, temp: DWORD ;
    strsize, IDsLen: UInt ;
    succflag: boolean ;
    LangCharSet: string ;
    Dummy: DWORD;
    IDs: PTranslationIDList;
//      IDCount: integer;
begin
    result := '' ;
    bufsize := GetFileVersionInfoSize (PChar(AppName), temp) ;
    if bufsize = 0 then exit ;
    GetMem (buffer, bufsize) ;
    try

    // get all version info into buffer
        succflag := GetFileVersionInfo (PChar(AppName), 0, bufsize, buffer) ;
        if NOT succflag then exit ;

    // set language Id
        LangCharSet := '040904E4' ;
        if VerQueryValue (buffer, PChar('\VarFileInfo\Translation'),
                                                     Pointer(IDs), IDsLen) then
        begin
//          IDCount := IDsLen div SizeOf(TTranslationPair);
//          for Dummy := 0 to IDCount-1 do  // only need first language
//              begin
            Dummy := 0 ;
            if IDs^[Dummy].Lang = 0 then IDs^[Dummy].Lang := DEFAULT_LANG_ID;
            if IDs^[Dummy].CharSet = 0 then
                                IDs^[Dummy].CharSet := DEFAULT_CHAR_SET_ID;
            LangCharSet := Format('%.4x%.4x', [IDs^[Dummy].Lang,
                                                        IDs^[Dummy].CharSet]) ;
//              end;
        end;

    // now read real information
        succflag := VerQueryValue (buffer, PChar ('\StringFileInfo\' +
                    LangCharSet + '\' + KeyName), Pointer (PStr), strsize) ;
        temp := strsize ;
        if succflag then result := StrPas (PStr) ;

    finally
        FreeMem (buffer) ;
    end ;
end ;

// get ethernet MAC address
// WARNING this code is not totally reliable, does not like multiple adaptors
// and sometimes returns the same adaptor more than once
// IpHlpAdaptersInfo is more reliable for OSs that support it

function GetMACAddresses (Pcname: string; MacAddresses: TStrings): integer ;
const
    HEAP_ZERO_MEMORY = $8;
    HEAP_GENERATE_EXCEPTIONS = $4;
type
    TAStat = packed record
        adapt : nb30.TAdapterStatus ;
        NameBuff : array [0..30] of TNameBuffer ;
    end;
var
    NCB: TNCB ;
    Enum: TLanaEnum ;
    PASTAT : Pointer ;
    AST : TAStat ;
    I: integer ;
begin
    result := -1 ;
    if NOT Assigned (MacAddresses) then exit ;  // sanity test
    MacAddresses.Clear ;

  // For machines with multiple network adapters you need to
  // enumerate the LANA numbers and perform the NCBASTAT
  // command on each. Even when you have a single network
  // adapter, it is a good idea to enumerate valid LANA numbers
  // first and perform the NCBASTAT on one of the valid LANA
  // numbers. It is considered bad programming to hardcode the
  // LANA number to 0 (see the comments section below).
    FillChar(NCB, Sizeof(NCB), 0) ;
    NCB.ncb_buffer := Pointer (@Enum) ;
    NCB.ncb_length := SizeOf (Enum) ;
    NCB.ncb_command := Chr (NCBENUM) ;
    if NetBios (@NCB) <> Char (NRC_GOODRET) then exit ;
    for I := 0 to Pred (Ord (Enum.Length)) do
    begin

  // The IBM NetBIOS 3.0 specifications defines four basic
  // NetBIOS environments under the NCBRESET command. Win32
  // follows the OS/2 Dynamic Link Routine (DLR) environment.
  // This means that the first NCB issued by an application
  // must be a NCBRESET, with the exception of NCBENUM.
  // The Windows NT implementation differs from the IBM
  // NetBIOS 3.0 specifications in the NCB_CALLNAME field.
        FillChar(NCB, Sizeof(NCB), 0);
        NCB.ncb_command := Chr(NCBRESET);
        NCB.ncb_lana_num := Enum.lana [I] ;
        NetBios (@NCB) ;

  // To get the Media Access Control (MAC) address for an
  // ethernet adapter programmatically, use the Netbios()
  // NCBASTAT command and provide a "*" as the name in the
  // NCB.ncb_CallName field (in a 16-chr string).
  // NCB.ncb_callname = "* "
        FillChar(NCB, Sizeof (NCB), 0) ;
        FillChar(NCB.ncb_callname [0], 16, ' ') ;
        if PCName = '' then PCName := '*' ;
        Move (PCName [1], NCB.ncb_callname [0], Length (PCName));
        NCB.ncb_command := Chr (NCBASTAT);
        NCB.ncb_lana_num := Enum.lana [I] ;
        NCB.ncb_length := Sizeof (AST);
        PASTAT := HeapAlloc (GetProcessHeap(), HEAP_GENERATE_EXCEPTIONS or
                                         HEAP_ZERO_MEMORY, NCB.ncb_length) ;
        if PASTAT = nil then exit ;
        NCB.ncb_buffer := PASTAT;
        if NetBios (@NCB) = Char (NRC_GOODRET) then
        begin
            CopyMemory (@AST, NCB.ncb_buffer, SizeOf (AST)) ;
            with AST.adapt do
                MacAddresses.Add (Format ('%.2x-%.2x-%.2x-%.2x-%.2x-%.2x',
                   [Ord (adapter_address [0]), Ord (adapter_address [1]),
                    Ord (adapter_address [2]), Ord (adapter_address [3]),
                    Ord (adapter_address [4]), Ord (adapter_address [5])])) ;
            HeapFree (GetProcessHeap, 0, PASTAT);
            inc (result) ;
        end ;
    end ;
end;

// the following functions using SENSAPI.DLL need MSIE 5 or later installed

function LoadSensapi: Boolean;
begin
    Result := True;
    if SensapiModule <> 0 then Exit;

// open DLL
    SensapiModule := LoadLibrary (SensapiDLL);
    if SensapiModule = 0 then
    begin
        Result := false;
        exit ;
    end ;
    IsDestinationReachable := GetProcAddress (SensapiModule,
                                                 'IsDestinationReachableA') ;
    IsNetworkAlive := GetProcAddress (SensapiModule, 'IsNetworkAlive') ;
end;

// check whether local system has a LAN or RAS connections

function IsNetAlive (var Flags: DWORD): boolean ;
begin
    Flags := 0 ;
    result := false ;
    if NOT LoadSensapi then exit ;
    result := IsNetworkAlive (Flags) ;
end ;

// check whether local system has a LAN or RAS connections and/or can reach
// a specific host, returning some quality of connection information
// uses ping to reach host, which is not very reliable!!!

function IsDestReachable (Dest: string; var QocInfo: TQocInfo): boolean ;
begin
    FillChar (QocInfo, SizeOf (QocInfo), #0) ;
    QocInfo.dwSize := SizeOf (QocInfo) ;
    result := false ;
    if NOT LoadSensapi then exit ;
    result := IsDestinationReachable (PChar(Dest), QocInfo) ;
end ;

// get or update MSIE autodial key in registry

function MSIEAutoDial (var Value: boolean; const Update: boolean): boolean ;
var
    IniFile: TRegistry ;
const
    AutoDial = 'EnableAutoDial' ;
    IntSet = 'Internet Settings' ;
begin
    result := false ;
    IniFile := TRegistry.Create ;
    Try
    with IniFile do
    begin
        try
            RootKey := HKEY_CURRENT_USER;
            if OpenKey (CVKey + '\' + IntSet, true) then
            begin
                if Update then
                    WriteBool (AutoDial, Value)
                else
                begin
                    if GetDataType (AutoDial) = rdBinary then  // 4.94
                        ReadBinaryData (AutoDial, Value, GetDataSize(AutoDial))   // 4.94
                    else
                        Value := ReadBool (AutoDial) ;
                end ;
                result := true ;
            end ;
            CloseKey ;
        except
            Value := false ;
        end ;
    end ;
    finally
        if Assigned (IniFile) then IniFile.Free;
    end;
end ;

// get or update MSIE autodial keys in registry  // 4.94
//  0=Never Dial A Connection:  EnableAutoDial=false, NoNetAutodial=false
//  1=Dial Whenever A Network Connection Is Not Present: EnableAutoDial=true, NoNetAutodial=true
//  2=Always Dial My Default Connection: EnableAutoDial=true, NoNetAutodial=false

function MSIEAutoDialOpt (var Value: integer; const Update: boolean): boolean;
var
    IniFile: TRegistry ;
    benabledad, bnonetad: boolean ;
const
    AutoDial = 'EnableAutoDial' ;
    IntSet = 'Internet Settings' ;
    NoNetAutodial= 'NoNetAutodial' ;
begin
    result := false;

    IniFile := TRegistry.Create ;
    Try
    with IniFile do
    begin
        try
            RootKey := HKEY_CURRENT_USER;
            if OpenKey(CVKey + '\' + IntSet, True) then
            begin
                if Update then // Set the values
                begin
                    benabledad := false ;
                    bnonetad := false ;
                    if Value = 1 then bnonetad := true ;
                    if Value >= 1 then benabledad := true ;
                    WriteBool (AutoDial, benabledad) ;
                    WriteBool (NoNetAutodial, bnonetad) ;
                end
                else  // Only READ the values
                begin
                    if GetDataType(AutoDial) = rdBinary then
                        ReadBinaryData(AutoDial, benabledad, GetDataSize(AutoDial))
                    else
                        benabledad := ReadBool (AutoDial);
                    if GetDataType (NoNetAutodial) = rdBinary then
                        ReadBinaryData( NoNetAutodial, bnonetad, GetDataSize(NoNetAutodial))
                    else
                        bnonetad := ReadBool (NoNetAutodial);
                    Value := 0 ;
                    if benabledad then
                    begin
                        if bnonetad then 
                            Value := 1
                        else
                            Value := 2 ;
                    end ;
                end ;
                result := true ;
            end ;
            CloseKey ;
        except
            Value := 0 ;
        end ;
    end ;
    finally
        if Assigned (IniFile) then IniFile.Free ;
    end ;
end ;

// get or update MSIE default connection key in registry

function MSIEDefConn (var ConnName: string; const Update: boolean): boolean ;
var
    IniFile: TRegistry ;
const
    RemAcc = 'RemoteAccess' ;       // W9x/NT4/W2K
    IntProf = 'InternetProfile' ;   // W9x/NT4/W2K
    RasAD = 'Software\Microsoft\RAS AutoDial\Default' ;  // XP
    RasDef = 'DefaultInternet' ;                         // XP
begin
    result := false ;
    if NOT Update then ConnName := '' ;
    IniFile := TRegistry.Create ;
    Try
    with IniFile do
    begin
        try
            if MagRasOSVersion = OSWXP then   // 4.80, new key in Windows XP, 4.94 look in HCU no HLM
            begin
                RootKey := HKEY_CURRENT_USER;
                if OpenKey (RasAD, false) then
                begin
                    if Update then
                        WriteString (RasDef, ConnName)
                    else
                        ConnName := ReadString (RasDef) ;
                    result := true ;
                end ;
            end
            else
            begin
                RootKey := HKEY_CURRENT_USER;
                if OpenKey (RemAcc, false) then
                begin
                    if Update then
                        WriteString (IntProf, ConnName)
                    else
                        ConnName := ReadString (IntProf) ;
                    result := true ;
                end ;
                CloseKey ;
            end ;
        except
        end ;
    end ;
    finally
        if Assigned (IniFile) then IniFile.Free;
    end;
end ;


// borrowed from fileutils - removed raise exception

function ExcludeTrailingBackslash(const S: string): string;
begin
  Result := S;
  if IsPathDelimiter(Result, Length(Result)) then
    SetLength(Result, Length(Result)-1);
end;

function DirectoryExists(const Name: string): Boolean;
var
    Code: DWORD;
begin
    Code := GetFileAttributes(PChar(Name));
    Result := (Code <> $FFFFFFFF) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function ExcludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if IsPathDelimiter(Result, Length(Result)) then
    SetLength(Result, Length(Result)-1);
end;

function ForceDirs (Dir: string): Boolean;
begin
    Result := True;
    if Length(Dir) = 0 then
    begin
        Result := false ;
        exit ;
    end ;
    Dir := ExcludeTrailingPathDelimiter(Dir);
    if (Length(Dir) < 3) or DirectoryExists(Dir)
                or (ExtractFilePath(Dir) = Dir) then Exit; // avoid 'xyz:\' problem.
    Result := ForceDirs (ExtractFilePath(Dir)) and CreateDir (Dir);
end;

// descendent of TList, adding binary FIND
{
function TFindList.Find(const Item2: Pointer; Compare: TListSortCompare;
                                            var index: longint): Boolean;
var
    l, h, i, c: longint;
begin
    Result := False;
    if (List = nil) or (Count = 0) then exit ;
    l := 0;
    h := Count - 1;
    while l <= h do
    begin
        i := (l + h) shr 1;  // binary shifting
        c := Compare (List[i], Item2) ;
        if c < 0 then
            l := i + 1
        else
        begin
            h := i - 1;
            if c = 0 then
            begin
                Result := True;
                l := i;
            end;
        end;
    end;
    index := l;
end;  }

// get windows name and version

function IsWin95: boolean ;
begin
    if OsInfo.dwPlatformId = 0 then GetOSInfo ;
    result := false ;
    if OsInfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS then result := true ;
end ;

function IsWinNT: boolean ;
begin
    if OsInfo.dwPlatformId = 0 then GetOSInfo ;
    result := false ;
    if OsInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then result := true ;
end ;

function IsWin2K: boolean ;
begin
    result := false ;
    if IsWinNT and (OsInfo.dwMajorVersion >= 5) then result := true ;
end ;

function IsWinXP: boolean ;
begin
    result := false ;
    if IsWin2K and (OsInfo.dwMinorVersion > 0) then result := true ;
end ;

function IsWinXPE: boolean ;
begin
    result := false ;
    if IsWinXP and (((OsInfo.wSuiteMask AND VER_SUITE_EMBEDDEDNT) <> 0) or
      ((OsInfo.wSuiteMask AND VER_SUITE_EMBEDDED_RESTRICTED) <> 0)) then result := true ;
end ;

function IsWin2K3: boolean ;
begin
    result := false ;
    if IsWin2K and (OsInfo.dwMinorVersion >= 2) then result := true ;
end ;

function IsWinVista: boolean ;
begin
    result := false ;
    if IsWinNT and (OsInfo.dwMajorVersion >= 6) then result := true ;
end ;

function GetOSVersion: string ;
var
    info, inf2: string ;
begin
    if OsInfo.dwPlatformId = 0 then GetOSInfo ;
    case OsInfo.dwPlatformId of
        VER_PLATFORM_WIN32s: info := 'Windows 3.1';
        VER_PLATFORM_WIN32_WINDOWS:
        begin
            info := 'Windows 95';
            if OsInfo.dwMinorVersion >= 10 then info := 'Windows 98';
            if OsInfo.dwMinorVersion >= 90 then info := 'Windows ME';
        end ;
        VER_PLATFORM_WIN32_NT:
        begin
            inf2 := '' ;
            if OsInfo.wProductType = VER_NT_WORKSTATION then inf2 := ' WS' ;
            if OsInfo.wProductType = VER_NT_DOMAIN_CONTROLLER then inf2 := ' DomCont' ;
            if OsInfo.wProductType = VER_NT_SERVER then inf2 := ' Server' ;
            if (OsInfo.wSuiteMask AND VER_SUITE_SMALLBUSINESS) <> 0 then inf2 := ' SmallBus' ;
            if (OsInfo.wSuiteMask AND VER_SUITE_ENTERPRISE) <> 0 then inf2 := ' Enterprise' ;
            if (OsInfo.wSuiteMask AND VER_SUITE_DATACENTER) <> 0 then inf2 := ' Datacentre' ;
            if (OsInfo.wSuiteMask AND VER_SUITE_BLADE) <> 0 then inf2 := ' Web Server' ;
            info := 'Windows NT' + inf2 ;
            if OsInfo.dwMajorVersion = 5 then
            begin
                info := 'Windows 2000' + inf2 ;
                if OsInfo.dwMinorVersion = 1 then
                begin
                    if OsInfo.wProductType <= VER_NT_WORKSTATION then
                    begin
                        if (OsInfo.wSuiteMask AND VER_SUITE_PERSONAL) <> 0 then
                            info := 'Windows XP Home'
                        else if ((OsInfo.wSuiteMask AND VER_SUITE_EMBEDDEDNT) <> 0) or
                           ((OsInfo.wSuiteMask AND VER_SUITE_EMBEDDED_RESTRICTED) <> 0) then
                            info := 'Windows XP Embedded'
                        else
                            info := 'Windows XP Pro'
                    end ;
                end
                else if OsInfo.dwMinorVersion = 2 then
                    info := 'Windows 2003' + inf2
                else if OsInfo.dwMinorVersion >= 3 then
                    info := 'Unknown Windows version' ;
            end
            else if OsInfo.dwMajorVersion = 6 then
                info := 'Windows Vista'
            else if OsInfo.dwMajorVersion >= 7 then
                info := 'Unknown Windows version' ;
        end
        else
            info := 'Unknown Windows platform' ;
    end;
    info := info + ' ' + IntToStr(OsInfo.dwMajorVersion) +  '.' +
         IntToStr(OsInfo.dwMinorVersion) + '.' + IntToStr(LOWORD(OsInfo.dwBuildNumber)) ;
    if OsInfo.wServicePackMajor <> 0 then
        info := info + ' SP' + IntToStr (OsInfo.wServicePackMajor)
    else if OsInfo.szCSDVersion <> '' then
               info := info + ' ' + OsInfo.szCSDVersion ;
    result := info ;
end ;

// 8 Aug 2002 - try and get extended info with service packs and product

procedure GetOSInfo ;
begin
    FillChar (OsInfo, sizeof (TOSVERSIONINFOEX), 0) ;
    OsInfo.dwOSVersionInfoSize := sizeof (TOSVERSIONINFOEX);  // NT4 SP6 and later
    if GetVersionEx2 (OsInfo) then exit ;
    OsInfo.dwOSVersionInfoSize := sizeof (TOSVERSIONINFO);    // fall back to older version
    GetVersionEx2 (OsInfo) ;
end ;

// validation functions

function IsDigit(Ch : Char) : Boolean;
begin
    Result := (ch >= '0') and (ch <= '9');
end;

function IsDigitsDec (info: string; decimal: boolean) : boolean ;
var
    count, len: integer ;
    onedotflag: boolean ;
begin
    result := false ;
    onedotflag := false ;
    info := trim (info) ;
    len := length (info) ;
    if len = 0 then exit ;
    for count := 1 to len do
    begin
        if NOT IsDigit (info [count]) then
        begin                // allow minus sign at start
            if (count <> 1) then
            begin
                if NOT decimal then exit ;
                if info [count] <> DecimalSeparator then exit ;
                if onedotflag then exit ;
                onedotflag := true ;
            end
            else
            begin
                if (info [1] = '-') or (info [1] = '+') then
                begin
                    if (len = 1) then exit ;
                end
                else
                    exit ;
            end ;
        end ;
    end ;
    result := true ;
end ;

function IsDigits (info: string) : boolean ;
begin
    result := IsDigitsDec (info, false) ;
end ;

// swap any number of bytes, integer, double, extended, anything
// ByteSwaps (@value, sizeof (value)) ;

procedure ByteSwaps(DataPtr : Pointer;NoBytes : integer);
var
  i : integer;
  dp : PChar;
  tmp : char;
begin
  // Perform a sanity check to make sure that the function was called properly
  if (NoBytes > 1) then
  begin
    Dec(NoBytes);
    dp := PChar(DataPtr);
    // we are now safe to perform the byte swapping
    for i := NoBytes downto (NoBytes div 2 + 1) do
    begin
      tmp := Pchar(Integer(dp)+i)^;
      Pchar(Integer(dp)+i)^ := Pchar(Integer(dp)+NoBytes-i)^;
      Pchar(Integer(dp)+NoBytes-i)^ := tmp;
    end;
  end;
end;

// convert binary or BCD strings to hex

procedure ConvHexStr (instr: string; var outstr: string) ;
var
    flen, inx, nr1, nr2, outpos: integer ;
begin
    flen := Length (instr) ;  // original BCD or binary field
    if flen = 0 then exit ;
    SetLength (outstr, flen * 2) ;
    outpos := 1 ;
    for inx := 1 To flen do
    begin
        nr1 := ord (instr [inx]) ;
        nr2 := nr1 SHR 4 ;  // hi nybble
        If (nr2 > 9) then nr2 := nr2 + 7 ;  // handle ascii characters
        outstr [outpos] := Chr (nr2 + 48) ;
        inc (outpos) ;
        nr2 := nr1 and 15 ;  // lo nybble
        If (nr2 > 9) then nr2 := nr2 + 7  ;   // handle ascii characters
        outstr [outpos] := Chr(nr2 + 48) ;
        inc (outpos) ;
    end ;
End ;

// convert cardinal into eight hex bytes

function ConIntHex (value: cardinal): string ;
var
    reshex: string ;
    serbin: string [6] ;
begin
    move (value, serbin [1], 4) ;
    ByteSwaps (@serbin [1], 4) ;
    serbin [0] := chr(4) ;
    ConvHexStr (serbin, reshex) ;
    result := reshex ;
end ;


function StripQuotes (filename: string): string ;
var
    delim: char ;
    flen: integer ;
begin
 // strip file name delimiters
    result := filename ;
    flen := length (filename) ;
    if flen < 2 then exit ;
    delim := filename [1] ;
    if ((delim = SQUOTE) or (delim = DQUOTE)) then
    begin
        if (filename [flen] = delim) then
        begin
            if flen > 2 then
                result := copy (filename, 2, flen - 2)
            else
                result := '' ;
        end ;
    end ;
    if (delim = '<') then
    begin
        if (filename [flen] = '>') then
        begin
            if flen > 2 then
                result := copy (filename, 2, flen - 2)
            else
                result := '' ;
        end ;
    end ;
end ;

function StripNewLines (const S: string): string;
var
    I: Integer;
begin
    result := S ;
    if Length (result) = 0 then exit ;
    for I := 1 to Length (result) do
    begin
        if result [I] in [CR,LF,TAB] then result [I] := Space ;
    end ;
end;

// builds list of files in a directory, but without search path!

function IndexFiles (searchfile: string; mask: integer;
                   var FileList: TStringList; var totsize: cardinal): integer ;
var
    SearchRec: TSearchRec ;
    SearchResult: integer ;
begin
    totsize := 0 ;
    result := 0 ;
    if NOT Assigned (FileList) then exit ;   // 14 Feb 2005
    try
        FileList.Clear ;

// loop through directory getting all file names in directory
        SearchResult := SysUtils.FindFirst (searchfile, mask, SearchRec) ;
        while SearchResult = 0 do
        begin
            if ((SearchRec.Attr and mask) = SearchRec.Attr) then
            begin
                if (SearchRec.Name <> '.') and
                            (SearchRec.Name <> '..') then
                begin
                    FileList.Add (SearchRec.Name) ;
                    inc (totsize, SearchRec.Size) ;
                end ;
            end ;
            SearchResult := SysUtils.FindNext (SearchRec);
        end;
        SysUtils.FindClose (SearchRec);
        FileList.Sort ;
        result := FileList.Count ;
    except
        SysUtils.FindClose (SearchRec);
        result := 0 ;
    end ;
end ;

// delete multiple files, allowing wildcards, returns total zapped

function DeleteOldFiles (fname: string): integer ;
var
    flist: TStringList ;
    I: integer ;
    totsize: cardinal ;
begin
    result := 0 ;
    flist := TStringList.Create ;
    try
        if IndexFiles (fname, faNormArch, flist, totsize) = 0 then exit ;
        for I := 0 to Pred (flist.Count) do
        begin
            if SysUtils.DeleteFile (ExtractFilePath (fname) + flist [I]) then
                                                                  inc (result) ;
        end ;
    finally
        flist.Free ;
    end ;
end ;

function GetEnvirVar (name: string): string ;
var
    Buffer: array[0..1023] of Char;
    len: integer ;
begin
    result := '' ;
    len := GetEnvironmentVariable (pchar (name), Buffer, sizeof (Buffer)) ;
    if len <> 0 then result := buffer ;
end;

function StripChars (AString, AChars: string): string ;
var
    K: integer ;
begin
    if Length (AChars) <> 0 then
    begin
        while Length (AString) <> 0 do
        begin
            K := Pos (AChars, AString) ;
            if K = 0 then break ;
            Delete (AString, K, Length (AChars)) ;
        end ;
    end ;
    result := AString ;
end ;


function StripChar (const AString: string; const AChar: Char): string ;
var
  Ch: Char;
  L, M: Integer;
  Source, Dest: PChar;
begin
  L := Length (AString) ;
  SetLength (Result, L) ;
  Source := Pointer (AString) ;
  Dest := Pointer (Result) ;
  M := 0 ;
  while L <> 0 do
  begin
    Ch := Source^ ;
    if AChar = #255 then   // special case means all control codes
    begin
        if (Ch >= space) then
        begin
            Dest^ := Ch ;
            Inc (Dest) ;
            Inc (M) ;
        end ;
    end
    else
    begin
        if (Ch <> AChar) then
        begin
            Dest^ := Ch ;
            Inc(Dest) ;
            Inc(M) ;
        end ;
    end ;
    Inc(Source);
    Dec(L);
  end;
  SetLength(Result, M);
end ;

function StripSpaces (const AString: string): string ;
begin
    result := StripChar (AString, space) ;
end ;

function StripCommas (const AString: string): string ;
begin
    result := StripChar (AString, comma) ;
end ;

function StripNulls (const AString: string): string ;
begin
    result := StripChar (AString, null) ;
end ;

function StripAllCntls (const AString: string): string;
begin
    result := StripChar (AString, #255) ;
end ;


// convert upper case string to upper and lower (upper start and after punc)

function UpAndLower(const S: string): string;
var
  Ch, LCh: Char;
  L: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  LCh := #32 ;
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= 'A') and (Ch <= 'Z') and (LCh <> #32) then Inc(Ch, 32);
    Dest^ := Ch;
    LCh := Ch ;
    if (LCh in ['-','/','.','(',')','+','_','=']) then LCh := #32 ;
    Inc(Source);
    Inc(Dest);
    Dec(L);
  end;
end;

// translate specific single characters in a string to another single character

procedure StringTranCh (var S: string; FrCh, ToCh: Char) ;
var
    L: Integer;
    Source: PChar;
begin
    UniqueString (S) ;    // 10 July 2002
    L := Length (S) ;
    Source := Pointer (S) ;
    while L <> 0 do
    begin
        if (Source^ = FrCh) then Source^ := ToCh ;
        Inc (Source) ;
        Dec (L) ;
    end;
end;

// translate some common ASCII control codes to hi- 8-bit characters (to save in registry)
// 8-bits are punctuation unlikely to be used in file names or URLs

procedure StringCtrlSafe (var S: string) ;
begin
    StringTranCh (S, CR, #139) ;
    StringTranCh (S, LF, #155) ;
    StringTranCh (S, TAB, #171) ;
    StringTranCh (S, RECSEP, #187) ;
end ;

function StrCtrlSafe (const S: string): string ;
begin
    result := S ;
    StringCtrlSafe (result) ;
end ;

// restore some common ASCII control codes from hi- 8-bit characters

procedure StringCtrlRest (var S: string) ;
begin
    StringTranCh (S, #139, CR) ;
    StringTranCh (S, #155, LF) ;
    StringTranCh (S, #171, TAB) ;
    StringTranCh (S, #187, RECSEP) ;
end ;

function StrCtrlRest (const S: string): string ;
begin
    result := S ;
    StringCtrlRest (result) ;
end ;

// simple translation for illegal file name characters

procedure StringFileTran (var S: string) ;
begin
    StringTranCh (S, '/', ' ') ;
    StringTranCh (S, ':', ' ') ;
    StringTranCh (S, '\', ' ') ;
end ;

function StrFileTran (const S: string): string ;
begin
    result := S ;
    StringCtrlRest (result) ;
end ;

// convert path separators from UNIX to DOS

procedure UnixToDosPath (var S: string) ;
begin
    StringTranCh (S, '/', '\') ;
end;

function UnxToDosPath (const S: string): string ;
begin
    result := S ;
    UnixToDosPath (result) ;
end ;

// convert path separators from DOS to UNIX

procedure DosToUnixPath (var S: string) ;
begin
    StringTranCh (S, '\', '/') ;
end;

function DosToUnxPath (const S: string): string ;
begin
    result := S ;
    DosToUnixPath (result) ;
end ;


// replace control codes with spaces, true if string changed

function StringRemCntls (var S: string): boolean ;
var
    L: Integer;
    Source: PChar;
begin
    result := false ;
    UniqueString (S) ;    // 10 July 2002
    L := Length (S) ;
    Source := Pointer (S) ;
    while L <> 0 do
    begin
        if (Source^ < space) then
        begin
            Source^ := space ;
            result := true ;
        end ;
        Inc (Source) ;
        Dec (L) ;
    end;
end;

// remove control codes
{
function StringDelCntls (const S: string): string;
var
  Ch: Char;
  L, M: Integer;
  Source, Dest: PChar;
begin
  L := Length(S);
  SetLength(Result, L);
  Source := Pointer(S);
  Dest := Pointer(Result);
  M := 0 ;
  while L <> 0 do
  begin
    Ch := Source^;
    if (Ch >= space) then
    begin
        Dest^ := Ch;
        Inc(Dest);
        Inc(M) ;
    end ;
    Inc(Source);
    Dec(L);
  end;
  SetLength(Result, M);
end;  }

{                                                                              }
{ Copy                                                                         }
{                                                                              }
Function CopyRange (const S : String; const Start, Stop : Integer) : String;
  Begin
    Result := Copy (S, Start, Stop - Start + 1);
  End;

Function CopyFrom (const S : String; const Start : Integer) : String;
  Begin
    Result := Copy (S, Start, Length (S) - Start + 1);
  End;

Function CopyLeft (const S : String; const Count : Integer) : String;
  Begin
    Result := Copy (S, 1, Count);
  End;

Function CopyRight (const S : String; const Count : Integer) : String;
  Begin
    Result := Copy (S, Length (S) - Count + 1, Count);
  End;

{                                                                              }
{ Match                                                                        }
{                                                                              }
Function Match (const M : CharSet; const S : String; const Pos : Integer; const Count : Integer) : Boolean;
var I, PosEnd : Integer;
  Begin
    PosEnd := Pos + Count - 1;
    if (M = []) or (Pos < 1) or (Count = 0) or (PosEnd > Length (S)) then
      begin
        Result := False;
        exit;
      end;

    For I := Pos to PosEnd do
      if not (S [I] in M) then
        begin
          Result := False;
          exit;
        end;

    Result := True;
  End;

Function Match (const M : CharSetArray; const S : String; const Pos : Integer) : Boolean;
var J, C : Integer;
  Begin
    C := Length (M);
    if (C = 0) or (Pos < 1) or (Pos + C - 1 > Length (S)) then
      begin
        Result := False;
        exit;
      end;

    For J := 0 to C - 1 do
      if not (S [J + Pos] in M [J]) then
        begin
          Result := False;
          exit;
        end;

    Result := True;
  End;

{ Highly optimized version of Match. Equivalent to, but much faster and more   }
{ memory efficient than: M = Copy (S, Pos, Length (M))                         }
{ Does compare in 32-bit chunks (CPU's native type)                            }
Function Match (const M, S : String; const Pos : Integer) : Boolean;
  Asm
      push esi
      push edi
      push edx                    // save state

      push Pos
      push M
      push S                      // push parameters
      pop edi                     // edi = S [1]
      pop esi                     // esi = M [1]
      pop ecx                     // ecx = Pos
      cmp ecx, 1
      jb @NoMatch                 // if Pos < 1 then @NoMatch

      mov edx, [esi - 4]
      or edx, edx
      jz @NoMatch                 // if Length (M) = 0 then @NoMatch
      add edx, ecx
      dec edx                     // edx = Pos + Length (M) - 1

      cmp edx, [edi - 4]
      ja @NoMatch                 // if Pos + Length (M) - 1 > Length (S) then @NoMatch

      add edi, ecx
      dec edi                     // edi = S [Pos]
      mov ecx, [esi - 4]          // ecx = Length (M)

      // All the following code is an optimization of just two lines:         //
      //     rep cmsb                                                         //
      //     je @Match                                                        //
      mov dl, cl                                                              //
      and dl, $03                                                             //
      shr ecx, 2                                                              //
      jz @CheckMod                 { Length (M) < 4 }                         //
                                                                              //
      { The following is faster than:  {}                                     //
      {     rep cmpsd                  {}                                     //
      {     jne @NoMatch               {}                                     //
      @c1:                             {}                                     //
        mov eax, [esi]                 {}                                     //
        cmp eax, [edi]                 {}                                     //
        jne @NoMatch                   {}                                     //
        add esi, 4                     {}                                     //
        add edi, 4                     {}                                     //
        dec ecx                        {}                                     //
        jnz @c1                        {}                                     //
                                                                              //
      or dl, dl                                                               //
      jz @Match                                                               //
                                                                              //
      { Check remaining dl (0-3) bytes   {}                                   //
    @CheckMod:                           {}                                   //
      mov eax, [esi]                     {}                                   //
      mov ecx, [edi]                     {}                                   //
      cmp al, cl                         {}                                   //
      jne @NoMatch                       {}                                   //
      dec dl                             {}                                   //
      jz @Match                          {}                                   //
      cmp ah, ch                         {}                                   //
      jne @NoMatch                       {}                                   //
      dec dl                             {}                                   //
      jz @Match                          {}                                   //
      and eax, $00ff0000                 {}                                   //
      and ecx, $00ff0000                 {}                                   //
      cmp eax, ecx                       {}                                   //
      je @Match                          {}                                   //

    @NoMatch:
      xor al, al                  // Result := False
      jmp @Fin

    @Match:
      mov al, 1                   // Result := True

    @Fin:
      pop edx                     // restore state
      pop edi
      pop esi
  End;

// borrowed from math.pas

function Max(A,B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;


{                                                                              }
{ PosNext                                                                      }
{                                                                              }
Function PosNext (const Find : CharSet; const S : String; const LastPos : Integer) : Integer;
var I : Integer;
  Begin
    if Find = [] then
      begin
        Result := 0;
        exit;
      end;

    For I := Max (LastPos + 1, 1) to Length (S) do
      if S [I] in Find then
        begin
          Result := I;
          exit;
        end;

    Result := 0;
  End;

Function PosNext (const Find : CharSetArray; const S : String; const LastPos : Integer) : Integer;
var I, C : Integer;
  Begin
    C := Length (Find);
    if C = 0 then
      begin
        Result := 0;
        exit;
      end;

    For I := Max (LastPos + 1, 1) to Length (S) - C + 1 do
      if Match (Find, S, I) then
        begin
          Result := I;
          exit;
        end;

    Result := 0;
  End;

Function PosNext (const Find : String; const S : String; const LastPos : Integer = 0) : Integer;
var I : Integer;
  Begin
    if Find = '' then
      begin
        Result := 0;
        exit;
      end;

    For I := LastPos + 1 to Length (S) - Length (Find) + 1 do
      if Match (Find, S, I) then
        begin
          Result := I;
          exit;
        end;

    Result := 0;
  End;

Function PosPrev (const Find : String; const S : String; const LastPos : Integer = 0) : Integer;
var I, J : Integer;
  Begin
    if Find = '' then
      begin
        Result := 0;
        exit;
      end;

    if LastPos = 0 then
      J := Length (S) - Length (Find) + 1 else
      J := LastPos - 1;
    For I := J downto 1 do
      if Match (Find, S, I) then
        begin
          Result := I;
          exit;
        end;

    Result := 0;
  End;

{                                                                              }
{ PosN                                                                         }
{                                                                              }
Function PosN (const Find, S : String; const N : Integer = 1;
         const FromRight : Boolean = False) : Integer;
var F, I : Integer;
  Begin
    F := 0;
    For I := 1 to N do
      begin
        if FromRight then
          F := PosPrev (Find, S, F) else
          F := PosNext (Find, S, F);
        if F = 0 then
          break;
      end;
    Result := F;
  End;

{                                                                              }
{ Split                                                                        }
{                                                                              }
Function StrArraySplit (const S : String; const Delimiter : String = ' ') : StringArray;
var I, J, L : Integer;
  Begin
    SetLength (Result, 0);
    if (Delimiter = '') or (S = '') then
      exit;

    I := 0;
    L := 0;
    Repeat
      SetLength (Result, L + 1);
      J := PosNext (Delimiter, S, I);
      if J = 0 then
        Result [L] := CopyFrom (S, I + Length (Delimiter)) else
        begin
          Result [L] := CopyRange (S, I + Length (Delimiter), J - 1);
          I := J;
          Inc (L);
        end;
    Until J = 0;
  End;

Function StrArrayJoin (const S : StringArray; const Delimiter : String = c_Space) : String;
var I : Integer;
  Begin
    Result := '';
    For I := 0 to High (S) do
      begin
        if I > 0 then
          Result := Result + Delimiter;
        Result := Result + S [I];
      end;
  End;


procedure StrArrayDelete (var S: StringArray; index: integer) ;
var
    I, tot: integer ;
begin
    tot := Length (S) ;
    if (tot = 0) or (index >= tot) then exit ;
    dec (tot) ;
    if tot > 0 then
    begin
        for I := index to Pred (tot) do S [I] := S [Succ(I)] ;
    end ;
    SetLength (S, tot) ;
end ;

procedure StrArrayInsert (var S: StringArray; index: integer; T: string) ;
var
    I, tot: integer ;
begin
    tot := Length (S) ;
    SetLength (S, Succ(tot)) ;  // increase by one
    if index > tot then index := tot ;  // add at end if index too large
    if (index < tot) and (tot <> 0) then
    begin
        for I := tot downto Succ (index) do S [I] := S [Pred(I)] ;
    end ;
    S [index] := T ;
end ;

procedure StrArrayFromList (T: TStringList; var S: StringArray) ;
var
    I, tot: integer ;
begin
    tot := T.Count ;
    SetLength (S, tot) ;
    if tot = 0 then exit ;
    for I := 0 to Pred (tot) do S [I] := T [I] ;
end ;

procedure StrArrayToList (S: StringArray; var T: TStringList) ;
var
    I, tot: integer ;
begin
    tot := Length (S) ;
    T.Clear ;
    if tot = 0 then exit ;
    for I := 0 to pred (tot) do T.Add (S [I]) ;
end ;

function StrArrayPosOf (L: string; S: StringArray): integer ;
var
    I, tot: integer ;
begin
    tot := Length (S) ;
    result := -1 ;
    if tot = 0 then exit ;
    for I := 0 to pred (tot) do
    begin
        if L = S [I] then
        begin
            result := I ;
            exit ;
        end ;
    end ;
end ;

// file and directory related stuff

function FileTimeToInt64 (const FileTime: TFileTime): Int64 ;
begin
    Move (FileTime, result, SizeOf (result)) ;    // 29 Sept 2004, poss problem with 12/00 mixup
end;

function Int64ToFileTime (const FileTime: Int64): TFileTime ;
begin
    Move (FileTime, result, SizeOf (result)) ;
end;

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
begin
    Result := FileTimeToInt64 (FileTime) / FileTimeStep ;
    Result := Result + FileTimeBase ;
end;

{    // 29 Sept 2004, poss problem with 12/00 mixup
function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
var
    SystemTime: TSystemTime ;
begin
    result := 0 ;
    if NOT FileTimeToSystemTime (FileTime, SystemTime) then exit ;
    result := SystemTimeToDateTime (SystemTime) ;
end;  }

function CheckFileOpen(const FName: String): integer;
var
    H: Integer;
begin
    result := -1;   // file not found
    if NOT FileExists (FName) then exit ;
    H := FileOpen(FName, fmOpenReadWrite);
    result := 1;   // file open
    if H < 0 then exit;
    FileClose(H);
    result := 0;   // file found but closed
end;

// convert Filetime to seconds since year 2000

function FileTimeToSecs2K (const FileTime: TFileTime): integer ;
begin
    result := (FileTimeToInt64 (FileTime) - FileTime2000) div FileTimeSecond ;
end ;

function DateTimeToFileTime(DateTime: TDateTime): TFileTime;
var
  E: Extended;
begin
  E := (DateTime - FileTimeBase) * FileTimeStep;
  result := Int64ToFileTime (Round(E)) ;
end;

// get file written UTC TFileTime and size in bytes - no change for summer time

function GetFUAgeSizeFile (filename: string ; var FileTime: TFileTime ;
                                            var FSize: Int64): boolean ;
var
   SResult: integer ;
   SearchRec: TSearchRec ;
   TempSize: TULargeInteger ;  // 64-bit integer record
begin
   Result := false ;
   SResult := SysUtils.FindFirst(filename, faAnyFile, SearchRec);
   if SResult = 0 then
   begin
//      FSize := SearchRec.FindData.nFileSizeHigh ;
//      FSize := (FSize * MAXDWORD) + SearchRec.FindData.nFileSizeLow ;
        TempSize.LowPart := SearchRec.FindData.nFileSizeLow ;   // 4 Sept 2005
        TempSize.HighPart := SearchRec.FindData.nFileSizeHigh ;
        FSize := TempSize.QuadPart ;
        FileTime := SearchRec.FindData.ftLastWriteTime ;
        result := true ;
   end ;
   SysUtils.FindClose(SearchRec);
end ;

// get file written local TFileTime and size in bytes - changes for summer time

function GetFAgeSizeFile (filename: string ; var FileTime: TFileTime ;
                                            var FSize: Int64): boolean ;
var
   UTCFileTime: TFileTime ;
begin
   Result := GetFUAgeSizeFile (filename, UTCFileTime, FSize);
   if Result then FileTimeToLocalFileTime (UTCFileTime, FileTime) ;
end ;

// get file written UTC TDateTime and size in bytes - no change for summer time

function GetUAgeSizeFile (filename: string ; var FileDT: TDateTime;
                                            var FSize: Int64): boolean ;
var
   UTCFileTime: TFileTime ;
begin
   Result := GetFUAgeSizeFile (filename, UTCFileTime, FSize);
   if Result then FileDT := FileTimeToDateTime (UTCFileTime);
end ;

// get file written local TDateTime and size in bytes - changes for summer time

function GetAgeSizeFile (filename: string ; var FileDT: TDateTime;
                                            var FSize: Int64): boolean ;
var
   LocalFileTime: TFileTime ;
begin
   Result := GetFAgeSizeFile (filename, LocalFileTime, FSize);
   if Result then FileDT := FileTimeToDateTime (LocalFileTime);
end ;

// get file size in bytes

function GetSizeFile (filename: string): LongInt;
var
    FileDT: TDateTime;
    FSize: Int64;
begin
   Result := -1 ;
   if GetAgeSizeFile (filename, FileDT, FSize) then result := FSize ;
end ;

// get file size in bytes

function GetSize64File (filename: string): Int64 ;
var
    FileDT: TDateTime;
    FSize: Int64;
begin
   Result := -1 ;
   if GetAgeSizeFile (filename, FileDT, FSize) then result := FSize ;
end ;

// remove trailing spaces from string

function TrimSpRight(const S: string): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] = ' ') do Dec(I);
  Result := Copy(S, 1, I);
end;

// extract file name less extension, drive and path, true does

function ExtractNameOnly(FileName: string): string;
var
  I: Integer;
begin
  FileName := ExtractFileName (FileName) ;  // remove path
  I := Length(FileName);
  while (I > 0) and not (FileName[I] in ['.', '\', ':']) do Dec(I);
  if (I = 0) or (FileName[I] <> '.') then I := MaxInt;
  Result := Copy(FileName, 1, I - 1) ;
end;

// get exception literal message

function GetExceptMess (ExceptObject: TObject): string;
var
  MsgPtr: PChar;
  MsgEnd: PChar;
  MsgLen: Integer;
  MessEnd: String ;
begin
  MsgPtr := '';
  MsgEnd := '';
  if ExceptObject is Exception then
  begin
    MsgPtr := PChar(Exception(ExceptObject).Message);
    MsgLen := StrLen(MsgPtr);
    if (MsgLen <> 0) and (MsgPtr[MsgLen - 1] <> '.') then MsgEnd := '.';
  end;
  result := Trim (StrPas (MsgPtr)) ;
  MessEnd := Trim (StrPas (MsgEnd)) ;
  if Length (MessEnd) > 5 then result := result + ' - ' + MessEnd ;
end;

// string to numeric conversions

function AscToInt (value: string): Integer;   // simple version of StrToInt
var
    E: Integer;
begin
    Val (value, result, E) ;
end;

function AscToInt64 (value: string): Int64 ;   // simple version of StrToInt
var
    E: Integer;
begin
    Val (value, result, E) ;
end;

function Str2LInt (const S: String): LongInt;
begin
    result := AscToInt (Trim (S)) ; // remove leading and trailing spaces
end;

function Str2Byte (const S: String): Byte;
var
    L: LongInt;
begin
    L := Str2LInt (S);
    if L > MaxByte then
        Result := MaxByte
    else if L < MinByte then
        Result := MinByte
    else
        Result := L;
end;

function Str2SInt (const S: String): ShortInt;
var
    L: LongInt;
begin
    L := Str2LInt (S);
    if L > MaxShortInt then
        Result := MaxShortInt
    else if L < MinShortInt then
        Result := MinShortInt
    else
        Result := L;
end;

function Str2Int (const S: String): Integer;
begin
    result := Str2LInt (S);
end;


function Str2Word (const S: String): Word;
var
    L: LongInt;
begin
    L := Str2LInt (S);
    if L > MaxWord then
        Result := MaxWord
    else if L < MinWord then
        Result := MinWord
    else
        Result := L;
end;


// improved integer to string conversions

function AddThouSeps (const S: string): string;
var
    LS, L2, I : Integer;
    Temp : string;
begin
    result := S ;
    LS := Length (S);
    if LS <= 3 then exit ;
    L2 := (LS - 1) div 3;
    Temp := '';
    for I := 1 to L2 do
            Temp := ThousandSeparator + Copy (S, LS - 3 * I + 1, 3) + Temp;
    Result := Copy (S, 1, (LS - 1) mod 3 + 1) + Temp;
end;

function IntToCStr (const N: integer): string ;
begin
    result := AddThouSeps (IntToStr (N)) ;
end ;

function Int64ToCStr (const N: int64): string ;
begin
    result := AddThouSeps (IntToStr (N)) ;
end ;

function LInt2Str (const L: LongInt; const Len: Byte): String;
begin
    try
        Result := IntToStr (L);
    except
        Result := '';
    end;
    Result := PadChLeftStr (CopyLeft (Result, Len), NumPadCh, Len);
end;

function LInt2EStr (const L: LongInt): String;
begin
    try
        Result := IntToStr (L);
    except
        Result := '';
    end;
end;

function LInt2ZBEStr (const L: LongInt): String;
begin
    if L = 0 then
        Result := ''
    else
        try
            Result := IntToStr (L);
        except
            Result := '';
    end;
end;

function FillStr (const Ch : Char; const N : Integer): string;
begin
    SetLength (Result, N);
    FillChar (Result [1], N, Ch);
end;

function BlankStr (const N : Integer): string;
begin
    Result := FillStr (' ', N);
end;

function DashStr (const N : Integer): string;
begin
    Result := FillStr ('-', N);
end;

function DDashStr (const N : Integer): string;
begin
    Result := FillStr ('=', N);
end;

function LineStr (const N : Integer): string;
begin
    Result := FillStr (#196, N);
end;

function DLineStr (const N : Integer): string;
begin
    Result := FillStr (#205, N);
end;

function StarStr (const N : Integer): string;
begin
    Result := FillStr ('*', N);
end;

function HashStr (const N : Integer): string;
begin
    Result := FillStr ('#', N);
end;

function PadRightStr (const S : string; const Len : Integer): string;
var
    N: Integer;
begin
    N := Length (S);
    if N < Len then
        Result := S + BlankStr (Len - N)
    else
        Result := S;
end;

function PadLeftStr (const S : string; const Len : Integer): string;
var
    N: Integer;
begin
    N := Length (S);
    if N < Len then
        Result := BlankStr (Len - N) + S
    else
        Result := S;
end;

function PadChLeftStr (const S : string; const Ch : Char; const Len : Integer): string;
var
    N: Integer;
begin
    N := Length (S);
    if N < Len then
        Result := FillStr (Ch, Len - N) + S
    else
        Result := S;
end;

// angus, leading zeros

function Int2StrZ (const L: LongInt; const Len: Byte): String;
begin
    try
        Result := IntToStr (L);
    except
        Result := '';
    end;
    Result := PadChLeftStr (CopyLeft (Result, Len), '0', Len);
end;

function Byte2Str (const L: LongInt; const Len: Byte): String;
begin
    try
        Result := IntToStr (L);
    except
        Result := '';
    end;
    Result := PadChLeftStr (CopyLeft (Result, Len), NumPadCh, Len);
end;

function LInt2ZBStr (const L: LongInt; const Len: Byte): String;
begin
    Result := LInt2ZBEStr (L);
    Result := PadChLeftStr (CopyLeft (Result, Len), NumPadCh, Len);
end;

function LInt2ZStr (const L: LongInt; const Len: Byte): String;
begin
    Result := LInt2EStr (L);
    Result := PadChLeftStr (CopyLeft (Result, Len), '0', Len);
end;

function LInt2CStr (const L : LongInt; const Len : Byte): string;
begin
    Result := LInt2CEStr (L);
    Result := PadChLeftStr (CopyLeft (Result, Len), NumPadCh, Len);
end;

function LInt2CEStr (const L : LongInt): string;
begin
    try
        Result := AddThouSeps (IntToStr (L)) ;
    except
        Result := '';
    end;
end;

function Int642CEStr (const L : Int64): string;
begin
    try
        Result := AddThouSeps (IntToStr (L)) ;
    except
        Result := '';
    end;
end;

function Str2DateTime (const S: String): TDateTime;  // WARNING = format is system dependent
begin
    Result := 0 ;
    if length (S) < 8 then exit ;
    if S [1] = space then exit ;
{$IFDEF VER130} // D5
   try
       Result := StrToDateTime  (S)
   except
       Result := 0 ;
   end;
{$ELSE}
{$IFDEF VER120} // D4
   try
       Result := StrToDateTime  (S)
   except
       Result := 0 ;
   end;
{$ELSE}
   Result := StrToDateTimeDef (S, 0) ;  // D6 and later
{$ENDIF}
{$ENDIF}
end;

function Str2Time (const S: String): TDateTime;
begin
    Result := 0 ;
    if length (S) < 3 then exit ;
    if S [1] = space then exit ;
{$IFDEF VER130} // D5
   try
       Result := StrToTime  (S)
   except
       Result := 0 ;
   end;
{$ELSE}
{$IFDEF VER120} // D4
   try
       Result := StrToTime  (S)
   except
       Result := 0 ;
   end;
{$ELSE}
    Result := StrToTimeDef (S, 0) ;  // D6 only
{$ENDIF}
{$ENDIF}
end;

function Date2Packed (infoDT: TDateTime): string ;
begin
    result := '' ;
    if infoDT < 1 then exit ;   // ensure there's a date and not just time
    result := FormatDateTime (DateMaskPacked, infoDT)
end ;

function Date2XPacked (infoDT: TDateTime): string ;
begin
    result := '' ;
    if infoDT < 1 then exit ;   // ensure there's a date and not just time
    result := FormatDateTime (DateMaskXPacked, infoDT)
end ;

// borrowed from sysutils.pas where it's not exported
{$IFDEF VER130} // D5

function TryEncodeTime(Hour, Min, Sec, MSec: Word; var Time: TDateTime): Boolean;
begin
  Result := False;
  if (Hour < 24) and (Min < 60) and (Sec < 60) and (MSec < 1000) then
  begin
    Time := (Hour * 3600000 + Min * 60000 + Sec * 1000 + MSec) / MSecsPerDay;
    Result := True;
  end;
end;

function TryEncodeDate(Year, Month, Day: Word; var Date: TDateTime): Boolean;
var
  I: Integer;
  DayTable: PDayTable;
begin
  Result := False;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= DayTable^[Month]) then
  begin
    for I := 1 to Month - 1 do Inc(Day, DayTable^[I]);
    I := Year - 1;
    Date := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
    Result := True;
  end;
end;
{$ENDIF}

{$IFDEF VER120} // D4

function TryEncodeTime(Hour, Min, Sec, MSec: Word; var Time: TDateTime): Boolean;
begin
  Result := False;
  if (Hour < 24) and (Min < 60) and (Sec < 60) and (MSec < 1000) then
  begin
    Time := (Hour * 3600000 + Min * 60000 + Sec * 1000 + MSec) / MSecsPerDay;
    Result := True;
  end;
end;

function TryEncodeDate(Year, Month, Day: Word; var Date: TDateTime): Boolean;
var
  I: Integer;
  DayTable: PDayTable;
begin
  Result := False;
  DayTable := @MonthDays[IsLeapYear(Year)];
  if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
    (Day >= 1) and (Day <= DayTable^[Month]) then
  begin
    for I := 1 to Month - 1 do Inc(Day, DayTable^[I]);
    I := Year - 1;
    Date := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
    Result := True;
  end;
end;
{$ENDIF}

function Packed2Time (info: string): TDateTime ;
// hhnnss-zzz
// 1234567890
var
    hh, nn, ss, zz: word ;
begin
    result := -1 ;
    info := trim (info) ;
    if length (info) < 6 then exit ;
    zz := 0 ;
    hh := Str2Word (copy (info, 1, 2)) ;
    nn := Str2Word (copy (info, 3, 2)) ;
    ss := Str2Word (copy (info, 5, 2)) ;
    if length (info) = 10 then zz := Str2Word (copy (info, 8, 3)) ;
    if NOT TryEncodeTime (hh, nn, ss, zz, result) then exit ;   // D6 only
end ;

function Packed2Date (info: string): TDateTime ;
// yyyymmdd-hhnnss-zzz (DateMaskXPacked) = 19
// yyyymmdd-hhnnss (DateMaskPacked) = 15
// or just yyyymmdd = 8
// 123456789012345
var
    yy, mm, dd: word ;
    timeDT: TDateTime ;
begin
    result := 0 ;
    info := trim (info) ;
    if length (info) < 8 then exit ;
    yy := Str2Word (copy (info, 1, 4)) ;
    mm := Str2Word (copy (info, 5, 2)) ;
    dd := Str2Word (copy (info, 7, 2)) ;
    if NOT TryEncodeDate (yy, mm, dd, result) then     // D6 only
    begin
        result := -1 ;
        exit ;
    end ;
    if length (info) < 15 then exit ;
    if info [9] <> '-' then exit ;
    timeDT := Packed2Time (copy (info, 10, 10)) ;
    if timeDT < 0 then exit ;
    result := result + timeDT ;
end ;

function PackedISO2Date (info: string): TDateTime ;
// yyyy-mm-ddThh:nn:ss (ISODateTimeMask), might be NULL
// or just yyyy-mm-dd
// 1234567890123456789
var
    yy, mm, dd: word ;
    hh, nn, ss: word ;
    timeDT: TDateTime ;
begin
    result := 0 ;
    info := trim (info) ;
    if length (info) < 10 then exit ;
    if info [5] <> '-' then exit ;
    yy := Str2Word (copy (info, 1, 4)) ;
    mm := Str2Word (copy (info, 6, 2)) ;
    dd := Str2Word (copy (info, 9, 2)) ;
    if NOT TryEncodeDate (yy, mm, dd, result) then     // D6 only
    begin
        result := -1 ;
        exit ;
    end ;
    if length (info) <> 19 then exit ;
    if info [14] <> ':' then exit ;
    hh := Str2Word (copy (info, 12, 2)) ;
    nn := Str2Word (copy (info, 15, 2)) ;
    ss := Str2Word (copy (info, 18, 2)) ;
    if NOT TryEncodeTime (hh, nn, ss, 0, timeDT) then exit ;   // D6 only
    result := result + timeDT ;
end ;

function PackedISO2UKStr (info: string): string ;
// yyyy-mm-ddThh:nn:ss (ISODateTimeMask), might be NULL, to dd/mm/yyyy hh:mm:ss
// or just yyyy-mm-dd
// 1234567890123456789
// null returns blank, zero seconds left blank
begin
    result := '' ;
    info := trim (info) ;
    if length (info) < 10 then exit ;
    if info [5] <> '-' then exit ;
    result := copy (info, 9, 2) + '/' + copy (info, 6, 2) + '/' + copy (info, 1, 4) ;
    if length (info) <> 19 then exit ;
    if info [14] <> ':' then exit ;
    result := result + ' ' + copy (info, 12, 2) + ':' + copy (info, 15, 2) ;
    if copy (info, 18, 2) <> '00' then
    result := result + ':' + copy (info, 18, 2) ;
end ;

function Packed2Secs (info: string): integer ;
// hh:nn:ss   - but with leading characters blank,  12:40 3:50   - timer!!
// 12345678
var
    len: integer ;
begin
    result := 0 ;
    info := trim (info) ;
    len := length (info) ;
    if len < 4 then exit ;
    while length (info) < 8 do info := '0' + info ;  // add leading zeros
    if info [6] <> TimeSeparator then exit ;
    result := AscToInt (copy (info, 1, 2)) * 60 ;
    result := (result + AscToInt (copy (info, 4, 2))) * 60 ;
    result := result + AscToInt (copy (info, 7, 2)) ;
end ;

function ConvLongDate (info: string): TDateTime ;
// yyyy/mm/dd
var
    yy, mm, dd: word ;
begin
    result := 0 ;
    info := trim (info) ;
    if length (info) <> 10 then exit ;
    yy := Str2Word (copy (info, 1, 4)) ;
    mm := Str2Word (copy (info, 6, 2)) ;
    dd := Str2Word (copy (info, 9, 2)) ;
    if NOT TryEncodeDate (yy, mm, dd, result) then   // D6 only
    begin
        result := -1 ;
        exit ;
    end ;
end ;

function ConvUSADate (info: string): TDateTime ;
// mm/dd/yyyy
var
    yy, mm, dd: word ;
begin
    result := 0 ;
    info := trim (info) ;
    if length (info) <> 10 then exit ;
    yy := Str2Word (copy (info, 7, 4)) ;
    mm := Str2Word (copy (info, 1, 2)) ;
    dd := Str2Word (copy (info, 4, 2)) ;
    if NOT TryEncodeDate (yy, mm, dd, result) then result := 0 ;  // D6 only
end ;

function ConvUKDate (info: string): TDateTime ;
// dd/mm/yyyy hh:mm:ss or dd/mm/yyyy or dd/mm/yyyy hh:mm
// 1234567890123456789
var
    yy, mm, dd: word ;
    hh, nn, ss: word ;
    timeDT: TDateTime ;
begin
    result := 0 ;
    info := trim (info) ;
    if length (info) < 10 then exit ;
    if info [3] <> '/' then exit ;
    yy := Str2Word (copy (info, 7, 4)) ;
    mm := Str2Word (copy (info, 4, 2)) ;
    dd := Str2Word (copy (info, 1, 2)) ;
    if NOT TryEncodeDate (yy, mm, dd, result) then
    begin
        result := 0 ;  // D6 only
        exit ;
    end ;
    if length (info) < 16 then exit ;
    if info [14] <> ':' then exit ;
    hh := Str2Word (copy (info, 12, 2)) ;
    nn := Str2Word (copy (info, 15, 2)) ;
    ss := 0 ;
    if length (info) >= 19 then ss := Str2Word (copy (info, 18, 2)) ;
    if NOT TryEncodeTime (hh, nn, ss, 0, timeDT) then exit ;   // D6 only
    result := result + timeDT ;
end ;


//  yyyymmdd and hhnnss to 'yyyy-mm-ddThh:nn:ss'

function AlphaDTtoISODT (sdate, stime: string): string ;
begin
    result := SQUOTE + CopyLeft (sdate, 4) + '-' + Copy (sdate, 5, 2) +
        '-' + Copy (sdate, 7, 2) + 'T' + CopyLeft (stime, 2) + ':' +
                    Copy (stime, 3, 2) + ':' + Copy (stime, 5, 2) + SQUOTE ;
end ;

//  yyyymmdd-hhnnss or yyyymmdd to 'yyyy-mm-ddThh:nn:ss'

function PackedDTtoISODT (info: string): string ;
begin
    result := 'NULL' ;
    if length (info) = 8 then info := info + '-000000' ;
    if length (info) <> 15 then exit ;
    result := AlphaDTtoISODT (copy (info, 1, 8), copy (info, 10, 6)) ;
end ;

// TDateTime to to 'yyyy-mm-ddThh:nn:ss'

function DTtoISODT (D: TDateTime): string ;
begin
    result := SQUOTE + FormatDateTime (ISODateTimeMask, D) + SQUOTE ;
end ;

// TDateTime to dd mmm yyyy

function DTtoAlpha (D: TDateTime): string ;
begin
    result := FormatDateTime (DateAlphaMask, D) ;
end ;

// TDateTime to dd mmm yyyy hh:mm

function DTTtoAlpha (D: TDateTime): string ;
begin
    result := FormatDateTime (DateAlphaMask + ' ' + ShortTimeMask, D) ;
end ;

// yyyy-mm-ddThh:nn:ss to yyyymmdd-hhnnss

function ISODTtoPacked (ISO: string): string ;
var
    L: integer ;
begin
    result := '' ;
    L := Length (ISO) ;
    if L < 10 then exit ;
    if ISO [5] <> '-' then exit ;
    result := CopyLeft (ISO, 4) + Copy (ISO, 6, 2) + Copy (ISO, 9, 2) ;
    if L < 19 then exit ;
    if ISO [11] <> 'T' then exit ;
    result := result + '-' + Copy (ISO, 12, 2) + Copy (ISO, 15, 2) + Copy (ISO, 18, 2) ;
end ;

// fuzzy date/time comparison, within one second
// Warning - does not work with file time stamps, need at least two secs

function EqualDateTime(const A, B: TDateTime): boolean;
begin
    result := (Abs (A - B) < OneSecond) ;
end;

// date/time difference in seconds, max one day

function DiffDateTime(const A, B: TDateTime): integer ; 
begin
    result := SecsPerDay ;
    if Abs (A - B) >= 1 then exit ;
    result := Trunc ((Abs (A - B)) * SecsPerDay) ; 
end;

// quote string, unless blank when NULL (for SQL)

function QuoteNull (S: string): string ;
begin
    if S = '' then
        result := 'NULL'
    else
        result := QuotedStr (S) ;
end ;

// convert date/time to quote SQL ISO date

function QuoteSQLDate (D: TDateTime): string ;
begin
    if D <= 100 then
        result := 'NULL'
    else
        result := QuotedStr (FormatDateTime (ISODateTimeMask, D)) ;
end ;

// convert time to quote SQL ISO date

function QuoteSQLTime (T: TDateTime): string ;
begin
    result := QuotedStr (TimeToNStr (T)) ;
end ;

{ time functions }

function DateTimeToAStr(const DateTime: TDateTime): string; // always alpha month and numeric hh:mm:ss
begin
  DateTimeToString(Result, DateTimeAlphaMask, DateTime);
end;

function DateToAStr(const DateTime: TDateTime): string; // always alpha month
begin
  DateTimeToString(Result, DateAlphaMask, DateTime);
end;


function TimeToNStr(const DateTime: TDateTime): string; // always numeric hh:mm:ss
begin
  DateTimeToString(Result, ISOTimeMask, DateTime);
end;

function TimeToZStr(const DateTime: TDateTime): string; // always numeric hh:mm:ss:zzz
begin
  DateTimeToString(Result, LongTimeMask, DateTime);
end;

function  timeHour(T: TDateTime): Integer;
var
  Hour,Minute,Sec,Sec100: Word;
begin
  DecodeTime(T,Hour,Minute,Sec,Sec100);
  Result:=Hour;
end;

function  timeMin(T: TDateTime): Integer;
var
  Hour,Minute,Sec,Sec100: Word;
begin
  DecodeTime(T,Hour,Minute,Sec,Sec100);
  Result:=Minute;
end;

function  timeSec(T: TDateTime): Integer;
var
  Hour,Minute,Sec,Sec100: Word;
begin
  DecodeTime(T,Hour,Minute,Sec,Sec100);
  Result:=Sec;
end;

function  TimeToInt(T: TDateTime): Integer;   // returns seconds
begin
  Result := -1 ;
  if T > 20000 then exit ;   // too many days for integer
  try
      Result := Trunc ((MSecsPerday * Frac (T)) / 1000);      // time
      Result := Result + (Trunc (T) * SecsPerDay) ;    // date
  except
      Result := 0 ;
  end ;
end;

function HoursToTime (hours: integer): TDateTime ;
begin
    if hours = 0 then
        result := 0
    else
        result := hours / (SecsPerDay / (60 * 60) ) ;
end ;

function MinsToTime (mins: integer): TDateTime ;
begin
    if mins = 0 then
        result := 0
    else
        result := mins / (SecsPerDay / 60) ;
end ;

function SecsToTime (secs: integer): TDateTime ;
begin
    if secs = 0 then
        result := 0
    else
        result := secs / SecsPerDay ;
end ;

function TimerToStr (duration: TDateTime): string ;
var
    hours: integer ;
    info: string [10] ;
begin
    info := copy (FormatDateTime ('hh:mm:ss', frac (duration)), 4, 5) ;
    hours := trunc (duration * 24) ;
    if hours = 0 then
    begin
        if info [1] = '0' then
            result := copy (info, 2, 9)
        else
            result := info ;
        exit ;
    end ;
    result := IntToStr (hours) + TimeSeparator + info ;
end ;

function SecsToMinStr (secs: integer): string ;
begin
    result := '0' ;
    if secs = 0 then exit ;
    result := IntToStr (secs div 60) + ':' + LInt2ZStr (secs mod 60, 2) ;
end ;

function SecsToHourStr (secs: integer): string ;
begin
    result := TimeToNStr (secs / SecsPerDay) ;
end ;

function sysTempPath: String;
var
    Buffer: array [0..1023] of Char ;
begin
    SetString (Result, Buffer, GetTempPath (Sizeof (Buffer) - 1, Buffer)) ;
end;

function sysWindowsDir: String;
var
    Buffer: array [0..255] of char ;
    PathLen: DWORD ;
begin
    PathLen := 255 ;
    Buffer [0] := #0 ;
    GetWindowsDirectory (Buffer, PathLen) ;
    Result := StrPas (Buffer);
end;

procedure sysBeep;
begin
    messageBeep($FFFF);
end;


function strLastCh(const S: string): Char ;
begin
    result := null ;
    if length (S) <> 0 then result := S [Length (S)] ;
end;

procedure strStripLast (var S: string);
begin
    if Length (S) > 0 then Delete (S, Length(S), 1) ;
end;

function strAddSlash(const S: string): string ;
begin
    result := S ;
    if strLastCh (result) <> SLASH then result := result + SLASH ;
end;

function strDelSlash(const S: string): string;
begin
    result := S ;
    if strLastCh (result) = SLASH then Delete (result, Length (result), 1) ;
end;

function ExtractUNIXPath(const FileName: string): string;
var
    I: Integer;
begin
    I := LastDelimiter('/', FileName);
    Result := Copy(FileName, 1, I);
end;

function ExtractUNIXName(const FileName: string): string;
var
    I: Integer;
begin
    I := LastDelimiter('/', FileName);
    Result := Copy(FileName, I + 1, MaxInt);
end;

function CharPos (TheChar: Char; const Str: String): Integer;
// Find a char in a string - faster than Pos
asm
        push        edi             // save needed regs
        or          edx, edx        // got an empty string?
        jz          @@1             // if yes, get out now
        mov         edi, edx        // EDI = source string
        mov         ecx, [edi-4]    // get length of string
        cld                         // specify auto-inc
        repnz       scasb           // find the char
        mov         eax,0           // assume failure
        jnz         @@2             // yup -- char wasn't found
        sub         edi, edx        // calculate index
        xchg        edi, edx        // need result in edx
@@1:    mov         eax, edx        // copy into EAX
@@2:    pop         edi             // restore EDI
end;

function DownCase( ch : Char ) : Char;
asm
{ ->    AL      Character       }
{ <-    AL      Result          }

        CMP     AL,'A'
        JB      @@exit
        CMP     AL,'Z'
        JA      @@exit
        ADD     AL,'a' - 'A'
@@exit:
end;

// convert string to hex quads

function ConvHexQuads (S: string): string ;
var
    I, J: integer ;
begin
    J := 0 ;
    result := '' ;
    if Length (S) = 0 then exit ;
    for I := 1 to Length (S) do
    begin
        result := result + IntToHex (Ord (S [I]), 2) ;
        inc (J) ;
        if J = 4 then
        begin
            J := 0 ;
            result := result + space ;
        end ;
    end ;
end ;

// get performance counter frequency, Win95 and NT3.1 and later
// PC09=3,579,545, might be processor frequency 2 gig

function GetPerfCountsPerSec: int64 ;
begin
    if PerfFreqCountsPerSec = 0 then
                    QueryPerformanceFrequency (PerfFreqCountsPerSec) ;
    result := PerfFreqCountsPerSec ;
end ;

function PerfCountCurrent: int64 ;
begin
     QueryPerformanceCounter (result) ;
end ;

function PerfCountToMilli (LI: int64): integer ;
begin
    result := (LI * 1000) div GetPerfCountsPerSec ;
end ;

function PerfCountGetMilli (startLI: int64): integer ;
var
    curLI: int64 ;
begin
    QueryPerformanceCounter (curLI) ;
    result := PerfCountToMilli (curLI - startLI) ;
end ;

function PerfCountGetMillStr (startLI: int64): string ;
begin
    result := LInt2CEStr (PerfCountGetMilli (startLI)) + 'ms' ;
end ;

// 'NowPC' function that returns the current time and date to a resolution of 200ns....
// Like Now, but returns a value to the performance counter resolution }

// WARNING - need set PerfFreqAligned to false if time is corrected
// check WM_TIMECHANGE message

function NowPC: TDateTime ;
var
   f_Now               : comp;
   LI                  : int64;
   f_ElapsedSinceStart : extended;
begin
  // first access, aligns the performance counter and date / time
    if NOT PerfFreqAligned then
    begin
        f_TDStartValue := Now ;
        QueryPerformanceCounter (LI) ;
        f_PCStartValue := LI ;
        f_PCCountsPerDay := GetPerfCountsPerSec * SecsPerDay ;
        PerfFreqAligned := True ;
    end;
    QueryPerformanceCounter (LI) ;
    f_Now := LI ;
    f_ElapsedSinceStart := f_Now - f_PCStartValue ;
    If f_ElapsedSinceStart < 0.0 then
              f_ElapsedSinceStart := f_ElapsedSinceStart - 1 ;  // Rolled over

// scale to get a TDateTime
    NowPC := f_TDStartValue + (f_ElapsedSinceStart / f_PCCountsPerDay) ;
end;

// date parsing borrowed from HttpApp but adapted to allow time hh:mm without seconds
// and for two digit W2K years, and with fewer exceptions
const
// These strings are NOT to be resourced

  Months: array[1..13] of string = (
    'Jan', 'Feb', 'Mar', 'Apr',
    'May', 'Jun', 'Jul', 'Aug',
    'Sep', 'Oct', 'Nov', 'Dec', '');
  DaysOfWeek: array[1..7] of string = (
    'Sun', 'Mon', 'Tue', 'Wed',
    'Thu', 'Fri', 'Sat');

function InetParseDate(const DateStr: string): TDateTime;
var
  Month, Day, Year, Hour, Minute, Sec: Integer;
  Parser: TParser;
  StringStream: TStringStream;
  temptime: TDateTime ;

  function GetMonth: Boolean;
  begin
    Month := 1;
    while not Parser.TokenSymbolIs(Months[Month]) and (Month < 13) do Inc(Month);
    Result := Month < 13;
  end;

  procedure GetTime;
  begin
    with Parser do
    begin
      Hour := TokenInt;
      NextToken;
      if Token = ':' then NextToken;
      Minute := TokenInt;
      NextToken;
      if Token = ':' then   // angus, allow missing seconds
      begin
          NextToken;
          Sec := TokenInt;
          NextToken;
      end ;
    end;
  end;

begin
  Sec := 0 ;
  result := 0 ;
  if DateStr = '' then exit ;  // angus, ignore blank
  StringStream := TStringStream.Create(DateStr);
  try
    Parser := TParser.Create(StringStream);
    with Parser do
    try
      NextToken;
      if Token = ':' then NextToken;
      NextToken;         // get day of week, might not exixt...
      if Token = ',' then NextToken;
      if GetMonth then
      begin
        NextToken;
        Day := TokenInt;
        NextToken;
        GetTime;
        Year := TokenInt;
      end else
      begin
        Day := TokenInt;
        NextToken;
        if Token = '-' then NextToken;
        GetMonth;
        NextToken;
        if Token = '-' then NextToken;
        Year := TokenInt;
        if Year < 50 then Inc(Year, 2000);   // Y2K pivot
        if Year < 100 then Inc(Year, 1900);
        NextToken;
        GetTime;
      end;
   // avoid exceptions
      if TryEncodeDate (Year, Month, Day, Result) then
      begin
         if TryEncodeTime (Hour, Minute, Sec, 0, temptime) then
                                        result := result + temptime ;
      end ;
    finally
      Free;
    end;
  finally
    StringStream.Free;
  end;
end;


function URLEncode(const psSrc: string): string;
const
  UnsafeChars = ' *#%<>+'; {do not localize}
var
  i: Integer;
begin
  Result := ''; { do not localize }
  for i := 1 to Length(psSrc) do
  begin
    if psSrc[i] = space then
        Result := Result + '+'
    else if (psSrc[i] in [CR, LF,'*','#','%','<','>','+']) or (psSrc[i] >= #$80) then
    begin
      Result := Result + '%' + IntToHex(Ord(psSrc[i]), 2); {do not localize}
    end
    else
    begin
      Result := Result + psSrc[i];
    end;
  end;
end;

function URLDecode(const AStr: String): String;  // borrowed from httpapp.pas
var
  Sp, Rp, Cp: PChar;
  S: String;
begin
  SetLength(Result, Length(AStr));
  Sp := PChar(AStr);
  Rp := PChar(Result);
  Cp := Sp;
  try
    while Sp^ <> #0 do
    begin
      case Sp^ of
        '+': Rp^ := ' ';
        '%': begin
               // Look for an escaped % (%%) or %<hex> encoded character
               Inc(Sp);
               if Sp^ = '%' then
                 Rp^ := '%'
               else
               begin
                 Cp := Sp;
                 Inc(Sp);
                 if (Cp^ <> #0) and (Sp^ <> #0) then
                 begin
                   S := '$' + Cp^ + Sp^;
                   Rp^ := Chr(StrToInt(S));
                 end
                 else
                    exit ;
               //    raise EWebBrokerException.CreateFmt(sErrorDecodingURLText, [Cp - PChar(AStr)]);
               end;
             end;
      else
        Rp^ := Sp^;
      end;
      Inc(Rp);
      Inc(Sp);
    end;
  except
    on E:EConvertError do
      raise EConvertError.CreateFmt('Invalid URL Encoded Char',
                ['%' + Cp^ + Sp^, Cp - PChar(AStr)])
  end;
  SetLength(Result, Rp - PChar(Result));
end;

function FormatLastError: string ;
begin
    result := SysErrorMessage (GetLastError) + ' [' + IntToCStr (GetLastError) + ']' ;
end ;

// display kilobytes
function Int2Kbytes (value: integer): string ;
begin
    if value > 999 then
        result := LInt2CStr ((value + 500) div 1000, 7) + 'K'
    else
        if value = 0 then result := ' 0'
    else
        result := '  < 1K' ;
end ;

// display megabytes, no max!
function Int2Mbytes (value: int64): string ;
begin
    if value > 999999 then
    begin
        value := value div 10 ;
        result := LInt2CStr ((value + 50000) div 100000, 7) + 'M'
    end
    else
        if value = 0 then result := ' 0'
    else
        result := ' < 1M' ;
end ;

function IntToKbyte (Value: Int64): String;
var
    float: Extended	;
begin
    float := value ;
    if (float / 100) >= GBYTE then
        FmtStr (result, '%5.0fG', [float / GBYTE])    // 134G
    else if (float / 10) >= GBYTE then
        FmtStr (result, '%5.1fG', [float / GBYTE])    // 13.4G
    else if float >= GBYTE then
        FmtStr (result, '%5.2fG', [float / GBYTE])    // 3.44G
    else if float >= (MBYTE * 100) then
        FmtStr (result, '%5.0fM', [float / MBYTE])    // 234M
    else if float >= (MBYTE * 10) then
        FmtStr (result, '%5.1fM', [float / MBYTE])    // 12.4M
    else if float >= MBYTE then
        FmtStr (result, '%5.2fM', [float / MBYTE])    // 5.67M
    else if float >= (KBYTE * 100) then
        FmtStr (result, '%5.0fK', [float / KBYTE])    // 678K
    else if float >= (KBYTE * 10) then
        FmtStr (result, '%5.1fK', [float / KBYTE])    // 76.5K
    else if float >= KBYTE then
        FmtStr (result, '%5.2fK', [float / KBYTE])    // 4.78K
    else
        FmtStr (result, '%5.0f ', [float]) ;          // 123
    result := Trim (result) ;
end ;

procedure EmptyRecycleBin (fname: string) ;
begin
    SHEmptyRecycleBin (0, PChar (fname), SHERB_NOCONFIRMATION + SHERB_NOPROGRESSUI)  ;
end;

// effectively pages a program out of main memory

procedure TrimWorkingSetMemory ;
var
    MainHandle: THandle;
begin
    MainHandle := OpenProcess(PROCESS_ALL_ACCESS, FALSE, GetCurrentProcessID) ;
    SetProcessWorkingSetSize (MainHandle, $FFFFFFFF, $FFFFFFFF);
    CloseHandle(MainHandle);
end;

// helper functions for timers and triggers using GetTickCount - which wraps after 49 days

function GetTickCountX: longword ;
var
    newtick: Int64 ;
begin
    result := GetTickCount ;
    if TicksTestOffset = 0 then exit ;  // no testing, byebye

// TicksTestOffset is set in initialization so that the counter wraps five mins after startup 
    newtick := Int64 (result) + Int64 (TicksTestOffset) ;
    if newtick >= MaxLongWrd then
        result := newtick - MaxLongWrd
    else
        result := newtick ;
end ;

function DiffTicks (const StartTick, EndTick: longword): longword ;
begin
    if EndTick >= StartTick then       // 19 Oct 2005, was > but allow for zero
        Result := EndTick - StartTick
    else
        Result := (MaxLongWord - StartTick) + EndTick ;
end ;

function ElapsedTicks (const StartTick: longword): longword ;
begin
    result := DiffTicks (StartTick, GetTickCountX) ;
end ;

function ElapsedSecs (const StartTick: longword): longword ;
begin
    result := (DiffTicks (StartTick, GetTickCountX)) div TicksPerSecond ;
end ;

function GetTrgMsecs (const MilliSecs: integer): longword ;
begin
    result := MilliSecs ;
    if result > (MaxLongWord - GetTickCountX) then
        result := (MaxLongWord - GetTickCountX) + result
    else
        result := result + GetTickCountX ;
end ;

function GetTrgSecs (const DurSecs: integer): longword ;
begin
    result := GetTrgMsecs (longword (DurSecs) * TicksPerSecond) ;
end ;

function TestTrgTick (const TrgTick: longword): boolean ;
var
    curtick: longword ;
begin
    result := false ;
    if TrgTick = MaxLongWord then exit ;  // special case for trigger disabled  
    if TrgTick = 0 then
    begin
        result := true ;  // special case for now
        exit ;
    end ;
    curtick := GetTickCountX ;
    if curtick <= MaxInteger then  // less than 25 days, keep it simple
    begin
        if curtick >= TrgTick then result := true ;
        exit ;
    end ;
    if TrgTick <= MaxInteger then exit ;  // trigger was wrapped, can not have been reached
    if curtick >= TrgTick then result := true ;
end ;

initialization
    SensapiModule := 0 ;
    TicksTestOffset := 0 ;
// force GetTickCount wrap in 5 mins - next line normally commented out
//    TicksTestOffset := MaxLongWord - GetTickCount - (5 * 60 * 1000) ;

// keep OS version
    MagRasOSVersion := OSW9x ;
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
        if Win32MajorVersion >= 5 then
        begin
            MagRasOSVersion := OSW2K ;
            if Win32MinorVersion > 0 then MagRasOSVersion := OSWXP ;   // and 2003
        end
        else
            MagRasOSVersion := OSNT4 ;
    end ;
finalization
    if SensapiModule <> 0 then
    begin
        FreeLibrary (SensapiModule) ;
        SensapiModule := 0 ;
    end ;
end.



