unit SpecialFolderUnit;

interface

uses
  Windows, SysUtils, ShlObj, Classes;

type
  TSpecialFolder = (
    sfDesktop,                // <desktop>
    sfInternet,               // Internet Explorer (icon on desktop)
    sfPrograms,               // Start Menu\Programs
    sfControls,               // My Computer\Control Panel
    sfPrinters,               // My Computer\Printers
    sfPersonal,               // My Documents
    sfFavorites,              // <user name>\Favorites
    sfStartup,                // Start Menu\Programs\Startup
    sfRecent,                 // <user name>\Recent
    sfSendTo,                 // <user name>\SendTo
    sfBitBucket,              // <desktop>\Recycle Bin
    sfStartMenu,              // <user name>\Start Menu
    sfMyDocuments,            // logical "My Documents" desktop icon
    sfMyMusic,                // "My Music" folder
    sfMyVideo,                // "My Videos" folder
    sfDesktopDirectory,       // <user name>\Desktop
    sfDrives,                 // My Computer
    sfNetwork,                // Network Neighborhood (My Network Places)
    sfNethood,                // <user name>\nethood
    sfFonts,                  // windows\fonts
    sfTemplates,              // <user name>\Templates
    sfCommonStartMenu,        // All Users\Start Menu
    sfCommonPrograms,         // All Users\Start Menu\Programs
    sfCommonStartup,          // All Users\Startup
    sfCommonDesktopDirectory, // All Users\Desktop
    sfAppData,                // <user name>\Application Data
    sfPrinthood,              // <user name>\PrintHood
    sfLocalAppData,           // <user name>\Local Settings\Applicaiton Data (non roaming)
    sfALTStartup,             // non localized startup
    sfCommonALTStartup,       // non localized common startup
    sfCommonFavorites,        // All Users\Favorites
    sfInternetCache,          // <user name>\Local Settings\Temporary Internet Files
    sfCookies,                // <user name>\Cookies
    sfHistory,                // <user name>\Local Settings\History
    sfCommonAppData,          // All Users\Application Data
    sfWindows,                // GetWindowsDirectory()
    sfSystem,                 // GetSystemDirectory()
    sfProgramFiles,           // C:\Program Files
    sfMyPictures,             // C:\Program Files\My Pictures
    sfProfile,                // USERPROFILE
    sfSystemX86,              // x86 system directory on RISC
    sfProgramFilesX86,        // x86 C:\Program Files on RISC
    sfProgramFilesCommon,     // C:\Program Files\Common
    sfProgramFilesCommonX86,  // x86 Program Files\Common on RISC
    sfCommonTemplates,        // All Users\Templates
    sfCommonDocuments,        // All Users\Documents
    sfCommonAdminTools,       // All Users\Start Menu\Programs\Administrative Tools
    sfAdminTools,             // <user name>\Start Menu\Programs\Administrative Tools
    sfConnections,            // Network and Dial-up Connections
    sfCommonMusic,            // All Users\My Music
    sfCommonPictures,         // All Users\My Pictures
    sfCommonVideo,            // All Users\My Video
    sfResources,              // Resource Direcotry
    sfResourcesLocalized,     // Localized Resource Direcotry
    sfCommonOEMLinks,         // Links to All Users OEM specific apps
    sfCDBurnArea,             // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
    sfComputersNearMe         // Computers Near Me (computered from Workgroup membership)
  );

function GetSpecialFolder(SpecialFolder: TSpecialFolder): string;    
function GetExactlyDir (BaseDir, StartDir, FileName: String): String;
function GetAppPath (): String;
Function IsEmptyDirectory(PathName:string):boolean;
function GetSplitBlankList (InputStr: String; Separate: TSysCharSet = [' ']): TStringList;
function GetAppExeFromCmdLine (CmdLine: String): String;

implementation

function GetSpecialFolder(SpecialFolder: TSpecialFolder): string;   
const
  SpecialFolderValues: array[TSpecialFolder] of Integer = ($0000, $0001, $0002,
    $0003, $0004, $0005, $0006, $0007, $0008, $0009, $000a, $000b, $000c, $000d,
    $000e, $0010, $0011, $0012, $0013, $0014, $0015, $0016, $0017, $0018, $0019,
    $001a, $001b, $001c, $001d, $001e, $001f, $0020, $0021, $0022, $0023, $0024,
    $0025, $0026, $0027, $0028, $0029, $002a, $002b, $002c, $002d, $002e, $002f,
    $0030, $0031, $0035, $0036, $0037, $0038, $0039, $003a, $003b, $003d);

var
  ItemIDList: PItemIDList;
  Buffer: array [0..MAX_PATH] of Char;
begin
  SHGetSpecialFolderLocation(0, SpecialFolderValues[SpecialFolder], ItemIDList);
  SHGetPathFromIDList(ItemIDList, Buffer);
  Result := StrPas(Buffer);
end;               

function GetFatherDir(Dir: String): String;
begin
  SetLength(Dir, Length(Dir) - 1);
  Result := ExtractFilePath(Dir);
end;

function GetExactlyDir (BaseDir, StartDir, FileName: String): String;
var
  IterFile: String;
begin
  Result := '';
  if BaseDir[Length(BaseDir)] <> '\' then BaseDir := BaseDir + '\';
  if StartDir[Length(StartDir)] <> '\' then StartDir := StartDir + '\';
  BaseDir := UpperCase(BaseDir);
  StartDir := UpperCase(StartDir);

  Repeat
    IterFile := StartDir + FileName;
    if FileExists(IterFile) then
    begin
      Result := IterFile;
      Exit;
    end;
    StartDir := GetFatherDir(StartDir);
    if StartDir = '' then Break;
  until BaseDir =  StartDir;
          
  IterFile := GetSpecialFolder (sfWindows) + FileName;
  if FileExists(IterFile) then
  begin
    Result := IterFile;
    Exit;
  end;

  IterFile := GetSpecialFolder (sfSystem) + FileName;
  if FileExists(IterFile) then
  begin
    Result := IterFile;
  end;
end;


function GetAppPath (): String;
begin
  Result := GetModuleName (0);
  Result := ExtractFilePath (Result);
end;


Function IsEmptyDirectory(PathName:string):boolean;
var
  sTmp:string;
begin
  if PathName[Length(PathName)]<>'\' then
    sTmp:=PathName+'\*.*'
  else
    sTmp:=PathName+'*.*';
  Result:=not FileExists(sTmp);
end;


function GetSplitBlankList (InputStr: String; Separate: TSysCharSet = [' ']): TStringList;
begin
  Result := TStringList.Create;
  if Trim(InputStr) = '' then Exit;
  ExtractStrings (Separate,[' '],PChar(InputStr),Result);
end;

function GetAppExeFromCmdLine (CmdLine: String): String;
var
  ExtStr: String;
  ExtSL: TStringList;
begin
  Result := '';
  if Length (CmdLine) < 7 then exit;

  ExtStr := ExtractFileExt (CmdLine);
  if Length (ExtStr) < 2 then exit;

  Result := Copy (CmdLine, 1, Length(CmdLine) - Length(ExtStr));

  ExtSL := GetSplitBlankList (ExtStr);
  ExtStr := ExtSL[0];
  Result := Result + ExtStr;
end;

end.

