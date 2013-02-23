// ***************************************************************
//  madSecurity.pas           version:  1.1q  ·  date: 2005-05-26
//  -------------------------------------------------------------
//  security, Acls/Aces, Shares, ...
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2006 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2006-05-26 1.1q (1) IAccount cache memory/resource leak fixed
//                 (2) IAccount cache now protected by critical section
// 2005-01-10 1.1p (1) you can now also call "Account('S-1-x-y-...')"
//                 (2) security objects (e.g. "FileSecurity") support wide now
// 2004-10-21 1.1o "AuthenticatedUsers" function added
// 2003-11-10 1.1n (1) "Everyone" function added
//                 (2) IAcl.SetFileAccess is inherited now
// 2003-08-08 1.1m (1) CurrentUser is now more reliabe in the winNT family
//                 (2) IAccount.SidStr property added
//                 (3) ISecurityObject.ProtectedDAcl/SAcl properties added
//                 (4) RegistrySecurity accepts strings like "HKLM" now
// 2002-11-01 1.1l winNT share names are much less restricted than win9x shares
// 2001-03-26 1.1k little bug in SetPrintAccess (win9x) fixed
// 2001-03-21 1.1j support for group accounts in win9x
// 2001-02-22 1.1i new shares in win9x are now automatically shared for everyone
// 2001-02-02 1.1h (1) IAce.SetFlags was not part of the Flags property
//                 (2) IShare.TargetSecurity + .ShareSecurity -> .SecurityObject
// 2000-11-13 1.1g IShare.SetNetName added
// 2000-08-15 1.1f IShare.SetPath added

unit madSecurity;

{$I mad.inc}

interface

uses Windows, madBasic, madTypes;

// ***************************************************************

// if nessecary, "str" gets shorted, so that it contains a valid shareName
// in win9x the maximal share name length is 12, in winNT it's unlimited
// in win9x the share names must be upper case, in winNT not
// examples:
// ClearShareName(         'Dämlich, Gunfried'         ) =          'DÄMLICH· GUN'
// ClearShareName('\\server\Dämlich, Gunfried\test.exe') = '\\server\DÄMLICH· GUN\test.exe'
procedure ClearShareName (var str: string);

// ***************************************************************

type
  // type for IAccount.Type_
  TAccountType = (atUnknown, atUser, atGroup, atDomain, atAlias, atWellKnownGroup,
                  atDeletedAccount, atInvalid, atUnknown2);

  // implements several APIs (e.g. LookupAccountName, several sid APIs, ...)
  // most functionality works only for winNT
  IAccount = interface (IBasic) ['{28D27EC2-3A98-11D3-A52D-00005A180D69}']
    // tests, whether the account still exists  (only winNT)
    function IsStillValid : boolean;

    // get the name of the account
    function GetName : string;
    property Name    : string read GetName;

    // get the type of the account  (win9x only supports "atUser" and "atGroup")
    function GetType : TAccountType;
    property Type_   : TAccountType read GetType;

    // get the name of the domain where the account is found  (only winNT)
    function GetDomain : string;
    property Domain    : string read GetDomain;

    // sid (= security ID) functions  (only winNT)
    function GetPSid    : PSid;
    function GetSidSize : integer;
    function GetSidStr  : string;
    property PSid       : PSid    read GetPSid;
    property SidSize    : integer read GetSidSize;
    property SidStr     : string  read GetSidStr;

    // is this account equal to the other account?
    function IsEqual (const otherAccount: IAccount) : boolean;
  end;

// name -> IAccount;  sid -> IAccount
function Account (name : string; group: TExtBool = other) : IAccount; overload;
function Account (sid  : PSid                           ) : IAccount; overload;

// get the current user
function CurrentUser : IAccount;

// return the "everyone" user account
function Everyone : IAccount;

// return the "authenticated users" account
function AuthenticatedUsers : IAccount;

// all accounts are cached to get a higher performance
// you can clear the cache at any time without any danger
procedure AccountCache_Add (const account: IAccount);
procedure AccountCache_Clear;

// ***************************************************************

type
  // forward...
  IAcl = interface;

  // *******************************************************************

  // types for IAce.Type_ and IAce.Flags
  TAceType  = (atAllowed, atDenied, atSystemAudit, atSystemAlarm,
               atAllowedCompound,
               atAllowedObject, atDeniedObject, atSystemAuditObject, atSystemAlarmObject);
  TAceFlag  = (afObjectInherit, afContainerInherit, afNoPropagateInherit, afInheritOnly, afInherited,
               af20, afSuccessfulAccess, afFailedAccess);
  TAceFlags = set of TAceFlag;

  // implements all Ace stuff, simulated under win9x
  IAce = interface (IBasic) ['{449EEF60-3AB3-11D3-A52D-00005A180D69}']
    // for which account is this Ace installed?
    function GetAccount : IAccount;
    property Account    : IAccount read GetAccount;

    // get/set the type of the Ace (win9x: always atAllowed)
    // can't be changed from non object to object or vice versa
    function  GetType : TAceType;
    procedure SetType (value: TAceType);
    property  Type_   : TAceType read GetType write SetType;

    // get/set the access mask of the Ace (win9x: only 2 bytes used)
    function  GetAccess : cardinal;
    procedure SetAccess (value: cardinal);
    property  Access    : cardinal read GetAccess write SetAccess;

    // get/set the flags of the Ace (win9x: always [])
    function  GetFlags : TAceFlags;
    procedure SetFlags (value: TAceFlags);
    property  Flags    : TAceFlags read GetFlags write SetFlags;

    // get the pointer to the Ace in the Acl and the size of the Ace  (win9x: always nil/0)
    function GetPAce : pointer;
    function GetSize : word;
    property PAce    : pointer read GetPAce;
    property Size    : word    read GetSize;

    // get the Acl to which this Ace belongs
    function GetOwnerAcl : IAcl;
    property OwnerAcl    : IAcl read GetOwnerAcl;
  end;

  // same as IAce, but for object Aces...  (only winNT)
  IObjAce = interface (IAce) ['{CBF310E0-46AC-11D3-A52D-00005A180D69}']
    // get/set additional informations for this object Ace
    function  GetObjFlags         : cardinal;
    function  GetObjType          : TGuid;
    function  GetInheritedObjType : TGuid;
    procedure SetObjFlags         (value: cardinal);
    procedure SetObjType          (value: TGuid   );
    procedure SetInheritedObjType (value: TGuid   );
    property  ObjFlags            : cardinal read GetObjFlags         write SetObjFlags;
    property  ObjType             : TGuid    read GetObjType          write SetObjType;
    property  InheritedObjType    : TGuid    read GetInheritedObjType write SetInheritedObjType;
  end;

  // *******************************************************************

  // forward...
  ISecurityObject = interface;

  // *******************************************************************

  // implements all Acl stuff, simulated under win9x
  IAcl = interface (ICustomBasicList) ['{28D27EC0-3A98-11D3-A52D-00005A180D69}']
    // test whether the Acl is still valid  (only winNT)
    function IsStillValid : boolean;

    // read access to the Ace items
    function GetItem (index: integer) : IAce;
    property Items   [index: integer] : IAce read GetItem; default;

    // create a new Ace and add it to the end of our Acl
    function NewItem (const account : IAccount;
                      access        : cardinal;
                      type_         : TAceType  = atAllowed;
                      flags         : TAceFlags = []       ) : integer;

    // add an Ace to the end of our Acl
    function AddItem (const item: IBasic) : integer;
    // insert an Ace to the Acl at the position "index"
    function InsertItem (const item : IBasic;
                         index      : integer = 0) : integer;

    // get the first found Ace that is installed for "account"
    // the search begins at the Ace no "firstSearchIndex"
    function FindItem (const account    : IAccount;
                       firstSearchIndex : integer = 0) : IAce;

    // delete the Ace no "index" from the Acl
    function DeleteItem (index: integer) : boolean;
    // delete all Aces that are installed for "account" from the Acl
    function DeleteItems (const account: IAccount) : boolean;

    // is the Acl allocated?
    function IsAllocated : boolean;
    // make sure that we don't have a nil Acl
    procedure Allocate;
    // make sure that we have an allocated but empty Acl (access for noone)
    procedure Clear;
    // make sure that we have a nil Acl (access for everyone)
    procedure Deallocate;

    // set the file access of "account" to "read only access" or "full access"
    procedure SetFileAccess (const account: IAccount; write: boolean);

    // set the print access of "account" to "print access" or to "full access"
    // (win9x: always "full access")
    procedure SetPrintAccess (const account: IAccount; admin: boolean);

    // are there changes in the Acl that are not installed yet?
    function IsDirty : boolean;
    // flush all changes we've made to the Acl
    function Flush : boolean;

    // get the pointer/size/usedSize of the Acl  (win9x: always nil/0/0)
    function GetPAcl     : PAcl;
    function GetSize     : integer;
    function GetUsedSize : integer;
    property PAcl        : PAcl    read GetPAcl;
    property Size        : integer read GetSize;
    property UsedSize    : integer read GetUsedSize;

    // get the security object to which this Acl belongs (if any)
    function GetOwnerSecurityObject : ISecurityObject;
    property OwnerSecurityObject    : ISecurityObject read GetOwnerSecurityObject;
  end;

  // *******************************************************************

  // type for ISecurityObject.Type_, must be 4 byte long (needed by NT security APIs)
  {$minenumsize 4}
    TSecurityObjectType = (seUnknown, seFile, seService, sePrinter, seRegistry,
                                      seShare, seKernelObject, seWindowObject);
  {$minenumsize 1}

  // implements all important functionality for security objects
  ISecurityObject = interface (IBasic) ['{810A7100-4813-11D3-A52D-00005A180D69}']
    // get the type of the security object
    function GetType : TSecurityObjectType;
    property Type_   : TSecurityObjectType read GetType;

    // get the name (or path) of the security object
    function GetName : wideString;
    property Name    : wideString read GetName;

    // get the handle of the security object
    function GetHandle : cardinal;
    property Handle    : cardinal read GetHandle;

    // get/set the account which owns the secured object
    function  GetOwner : IAccount;
    procedure SetOwner (const owner: IAccount);
    property  Owner    : IAccount read GetOwner write SetOwner;

    // get/set the primary group to which the secured object belongs
    function  GetGroup : IAccount;
    procedure SetGroup (const group: IAccount);
    property  Group    : IAccount read GetGroup write SetGroup;

    // get/set the discretionary Acl of the secured object
    function  GetDAcl : IAcl;
    procedure SetDAcl (const dacl: IAcl);
    property  DAcl    : IAcl read GetDAcl write SetDAcl;

    // get/set the system Acl of the secured object
    function  GetSAcl : IAcl;
    procedure SetSAcl (const sacl: IAcl);
    property  SAcl    : IAcl read GetSAcl write SetSAcl;

    // get/set the protection state of the DAcl/SAcl
    function  GetProtectedDAcl : boolean;
    procedure SetProtectedDAcl (value: boolean);
    function  GetProtectedSAcl : boolean;
    procedure SetProtectedSAcl (value: boolean);
    property  ProtectedDAcl    : boolean read GetProtectedDAcl write SetProtectedDAcl;
    property  ProtectedSAcl    : boolean read GetProtectedSAcl write SetProtectedSAcl;
  end;

// ***************************************************************

// creates a new Ace
function NewAce (const account : IAccount;
                 access        : cardinal;
                 type_         : TAceType  = atAllowed;
                 flags         : TAceFlags = []       ) : IAce;

// ***************************************************************

// Windows PAcl pointer  ->  IAcl interface
function Acl (acl: PAcl) : IAcl;

// Create a new Acl
function NewAcl : IAcl;

// ***************************************************************

// get a security object interface for a printer
function PrinterSecurity (nameOrUnc : wideString) : ISecurityObject; overload;
function PrinterSecurity (handle    : cardinal  ) : ISecurityObject; overload;

// or for a file/directoy/drive
function FileSecurity (pathOrUnc : wideString) : ISecurityObject; overload;
function FileSecurity (handle    : cardinal  ) : ISecurityObject; overload;

// ... share  (only winNT)
function ShareSecurity (nameOrUnc: wideString) : ISecurityObject;

// ... registry key  (only winNT)
function RegistrySecurity (pathOrUnc : wideString) : ISecurityObject; overload;
function RegistrySecurity (key       : HKEY      ) : ISecurityObject; overload;

// ... service process  (only winNT)
function ServiceSecurity (nameOrUnc : wideString) : ISecurityObject; overload;
function ServiceSecurity (handle    : cardinal  ) : ISecurityObject; overload;

// ... window object  (only winNT)
function WindowObjectSecurity (nameOrUnc : wideString) : ISecurityObject; overload;
function WindowObjectSecurity (handle    : cardinal  ) : ISecurityObject; overload;

// ... kernel object, e.g. a process  (only winNT)
function KernelObjectSecurity (handle: cardinal) : ISecurityObject;

// ***************************************************************

type
  // types for IShare.Type_
  TShareType    = (stDisk, stPrinter, stDevice, stIpc);
  TShareTypeSet = set of TShareType;

  // types for IShare.Access
  TShareAccess    = (aRead, aWrite, aCreate, aExec, aDelete, aAtrib, aPerm, aFindFirst);
  TShareAccessSet = set of TShareAccess;

  // implements all functionality for a share
  IShare = interface (IBasic) ['{0E2E3600-4C3A-11D3-A52D-00005A180D69}']
    // get share type
    function GetType : TShareType;
    property Type_   : TShareType read GetType;

    // get share netname  (win9x: limited to 12 characters)
    function  GetNetName : string;
    procedure SetNetName (netName: string);
    property  NetName    : string read GetNetName write SetNetName;

    // get/set share path, printer name or filesystem path
    function  GetPath : string;
    procedure SetPath (path: string);
    property  Path    : string read GetPath write SetPath;

    // get/set remark
    function  GetRemark : string;
    procedure SetRemark (remark: string);
    property  Remark    : string read GetRemark write SetRemark;

    // get/set access  (only available in share-level)
    function  GetAccess : TShareAccessSet;
    procedure SetAccess (access: TShareAccessSet);
    property  Access    : TShareAccessSet read GetAccess write SetAccess;

    // get/set password  (only available in share-level)
    function  GetPassword : string;
    procedure SetPassword (password: string);
    property  Password    : string read GetPassword write SetPassword;

    // get/set read/write password  (only available in win9x share-level)
    function  GetReadWritePassword : string;
    procedure SetReadWritePassword (password: string);
    property  ReadWritePassword    : string read GetReadWritePassword write SetReadWritePassword;

    // get/set persist state  (only win9x)
    function  GetPersist : boolean;
    procedure SetPersist (persist: boolean);
    property  Persist    : boolean read GetPersist write SetPersist;

    // get/set system state  (only win9x)
    function  GetSystem : boolean;
    procedure SetSystem (system: boolean);
    property  System    : boolean read GetSystem write SetSystem;

    // get number of current uses  (only winNT)
    function GetCurrentUses : integer;
    property CurrentUses    : integer read GetCurrentUses;

    // get/set max number of uses  (only winNT)
    function  GetMaxUses : integer;
    procedure SetMaxUses (maxUses: integer);
    property  MaxUses    : integer read GetMaxUses write SetMaxUses;

    // on which server is this share installed?
    function GetServerName : string;
    property ServerName    : string read GetServerName;

    // reloads all properties from the operating system
    function Refresh : boolean;

    // are there changes that are not flushed yet?
    function IsDirty : boolean;
    // flush any changes
    function Flush : boolean;

    // delete the share
    function Delete : boolean;

    // IAcl interface to change the accesses of the share
    function  GetAcl : IAcl;
    procedure SetAcl (const acl: IAcl);
    property  Acl    : IAcl read GetAcl write SetAcl;

    // get an ISecurityObject interface for the share's security
    function SecurityObject : ISecurityObject;
  end;

  // implements all functionality for some specific shares
  IShares = interface (ICustomBasicList) ['{0E2E3601-4C3A-11D3-A52D-00005A180D69}']
    // are there only shares of a specific type in this list?
    function GetTypes : TShareTypeSet;
    property Types    : TShareTypeSet read GetTypes;

    // do these shares all belong to the same target?
    // if not, Path is '*'
    function GetPath : string;
    property Path    : string read GetPath;

    // on which server are these shares installed?
    function GetServerName : string;
    property ServerName    : string read GetServerName;

    // delete all shares that are in this list
    function Delete : boolean;

    // read access to the items of this list
    function GetItem (index: integer) : IShare;
    property Items   [index: integer] : IShare read GetItem; default;

    // refresh the list, look for new/deleted/changed shares
    function RefreshItems : boolean;
  end;

// ***************************************************************

// net name -> IShare
function Share (netName    : string;
                serverName : string = '') : IShare;

// create a new share to the "path" with the "netName"
function NewShare (path       : string;
{ only valid in }  netName    : string;
{ ************* }  remark     : string          = '';
{ share-level   }  access     : TShareAccessSet = [low(TShareAccess)..high(TShareAccess)];
{ share-level   }  password   : string          = '';
{ share-l. + 9x }  rwPassword : string          = '';
{         win9x }  persist    : boolean         = true;
{         win9x }  system     : boolean         = false;
{         winNT }  maxUses    : integer         = -1;
                   serverName : string          = ''   ) : IShare;

// lists all shares with the specific properties
function Shares (types      : TShareTypeSet = [low(TShareType)..high(TShareType)];
                 path       : string        = '*';
                 serverName : string        = '' ) : IShares;

// ***************************************************************

// error codes
const CErrorNo_CantChangeAceType  = CErrorBase_Security + 1;
      CErrorNo_LonelyAce          = CErrorBase_Security + 2;
      CErrorNo_LonelyAcl          = CErrorBase_Security + 3;
      CErrorStr_CantChangeAceType = 'You can''t change the Ace type from IObjectAce to IAce or vice versa.';
      CErrorStr_LonelyAce         = 'The Ace has no valid owner Acl.';
      CErrorStr_LonelyAcl         = 'The Acl has no valid owner security object.';

// ***************************************************************

implementation

uses SysUtils, WinSpool, madStrings, madTools;

// ***************************************************************

procedure ClearShareName(var str: string);
const CCharExclude_Share : TSChar = ['/','"','<','>','|',  '\',':','*','?',  '[',']',';',',','+','='];

  function internal(str: string) : string;
  var i1 : integer;
      b1 : boolean;
  begin
    if OS.win9x then
         result := UpStr(RetTrimStr(str))
    else result := str;
    b1 := (result <> '') and (result[Length(result)] = '\');
    if b1 then Delete(result, Length(result), 1);
    for i1 := Length(result) downto 1 do
      if result[i1] in CCharExclude_Share then
        case result[i1] of
          '/', '|', '\' : result[i1] := '¦';
          ',', ';', ':' : result[i1] := '·';
          '['           : result[i1] := '(';
          ']'           : result[i1] := ')';
          '*'           : result[i1] := '×';
          '?'           : result[i1] := '¿';
          '+'           : result[i1] := '&';
          '='           : result[i1] := '~';
          '"'           : result[i1] := #39;
          '<'           : result[i1] := '{';
          '>'           : result[i1] := '}';
          else            Delete(result, i1, 1);
        end;
    if OS.win9x and (Length(result) > 12) then
      SetLength(result, 12);
    if b1 and (result <> '') and (result[Length(result)] <> '\') then
      result := result + '\';
  end;

var i1     : integer;
    s1, s2 : string;
begin
  s1 := ExtractFilePath(str);
  if s1 = '' then begin
    str := internal(str);
  end else if s1[1] = '\' then begin
    s1 := ExtractFileDrive(str);
    s2 := Copy(str, Length(s1) + 1, maxInt);
    i1 := PosStr('\', s1, 3);
    str := Copy(s1, 1, i1) + internal(Copy(s1, i1 + 1, Length(s1) - i1));
    if str[Length(str)] <> '\' then str := str + '\';
    str := str + s2;
    if str[Length(str)] = '\' then DeleteR(str, 1);
  end;
end;

// ***************************************************************

type
  // implements IAccount
  TIAccount = class (TIBasic, IAccount)
  public
    FName   : string;
    FDomain : string;
    FSid    : PSid;
    FType   : TAccountType;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: string;
                        name: string; sid: PSid; group: TExtBool);
    destructor Destroy; override;

    function IsStillValid : boolean;

    function GetName : string;

    function GetType : TAccountType;

    function GetDomain : string;

    function GetPSid    : PSid;
    function GetSidSize : integer;
    function GetSidStr  : string;

    function IsEqual (const otherAccount: IAccount) : boolean;

    function GetMaxInterface : IBasic; override;
  end;

var AccountCache   : array of IAccount;
    AccountSection : ICriticalSection;

procedure AccountCache_Add(const account: IAccount);
var i1 : integer;
begin
  if AccountSection = nil then
    AccountSection := NewCriticalSection;
  AccountSection.Enter;
  try
    i1 := Length(AccountCache);
    SetLength(AccountCache, i1 + 1);
    AccountCache[i1] := account;
  finally AccountSection.Leave end;
end;

function AccountCache_Find(name: string; sid: PSid) : IAccount;
var i1 : integer;
begin
  result := nil;
  if AccountSection <> nil then begin
    AccountSection.Enter;
    try
      if name <> '' then begin
        for i1 := 0 to high(AccountCache) do
          if AccountCache[i1].name = name then begin
            result := AccountCache[i1];
            break;
          end;
      end else if sid<>nil then
        for i1 := 0 to high(AccountCache) do
          if EqualSid(AccountCache[i1].PSid, sid) then begin
            result := AccountCache[i1];
            break;
          end;
    finally AccountSection.Leave end;
  end;
end;

procedure AccountCache_Clear;
begin
  if AccountSection <> nil then begin
    AccountSection.Enter;
    try
      AccountCache := nil;
    finally AccountSection.Leave end;
  end;
end;

constructor TIAccount.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: string;
                             name: string; sid: PSid; group: TExtBool);
var i1 : integer;
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    FValid := (name <> '') or (sid <> nil);
    if FValid then begin
      FName := name;
      if sid <> nil then begin
        i1 := GetLengthSid(sid);
        GetMem(FSid, i1);
        Move(sid^, FSid^, i1);
      end;
      if OS.win9x then
        case group of
          no  : FType := atUser;
          yes : FType := atGroup;
          else  if name = '*' then FType := atGroup
                else               FType := atUser;
        end;
    end else SetLastError(ERROR_INVALID_PARAMETER);
  end;
end;

destructor TIAccount.Destroy;
begin
  if FSid <> nil then FreeMem(FSid);
  inherited;
end;

function TIAccount.IsStillValid : boolean;
begin
  if OS.winNT and FValid then
    if GetPSid <> nil then begin
      FValid := IsValidSid(FSid);
      if not FValid then SetLastError(GetLastError);
    end;
  result := FValid;
end;

function TIAccount.GetName : string;
var c1, c2, c3 : cardinal;
begin
  if (FName = '') and CheckValid then begin
    c1 := 0;
    c2 := 0;
    LookupAccountSid(nil, FSid, nil, c1, nil, c2, c3);
    SetLength(FName,   c1);
    SetLength(FDomain, c2);
    FValid := LookupAccountSid(nil, FSid, pchar(FName), c1, pchar(FDomain), c2, c3);
    if FValid then begin
      FType   := TAccountType(c3);
      FName   := pchar(FName);
      FDomain := pchar(FDomain);
      AccountCache_Add(self);
    end else begin
      FName   := '';
      FDomain := '';
      SetLastError(GetLastError);
    end;
  end;
  result := FName;
end;

function TIAccount.GetType : TAccountType;
begin
  if OS.win9x or ((GetName <> '') and (GetPSid <> nil)) then
       result := FType
  else result := atUnknown;
end;

function TIAccount.GetDomain : string;
begin
  if OS.winNT and (GetName <> '') then GetPSid;
  result := FDomain;
end;

function TIAccount.GetPSid : PSid;
var c1, c2, c3 : cardinal;
begin
  if (FSid = nil) and CheckValid then begin
    if OS.winNT then begin
      c1 := 0;
      c2 := 0;
      LookupAccountName(nil, pchar(FName), nil, c1, nil, c2, c3);
      FSid := AllocMem(c1);
      SetLength(FDomain, c2);
      FValid := LookupAccountName(nil, pchar(FName), FSid, c1, pchar(FDomain), c2, c3);
      if FValid then begin
        FType := TAccountType(c3);
        FDomain := pchar(FDomain);
        AccountCache_Add(self);
      end else begin
        FDomain := '';
        FreeMem(FSid);
        FSid := nil;
        SetLastError(GetLastError);
      end;
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
  end;
  result := FSid;
end;

function TIAccount.GetSidSize : integer;
begin
  result := 0;
  if CheckValid then
    if OS.winNT then begin
      if GetPSid <> nil then result := GetLengthSid(FSid);
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

function TIAccount.GetSidStr : string;
var sia : PSidIdentifierAuthority;
    i1  : integer;
    pb  : TPByte;
    pc  : TPCardinal;
begin
  result := '';
  if CheckValid then
    if OS.winNT then begin
      if GetPSid <> nil then begin
        sia := GetSidIdentifierAuthority(FSid);
        if sia <> nil then begin
          result := 'S-1-' + IntToStrEx(dword(sia^.value[5]));
          pb := pointer(GetSidSubAuthorityCount(FSid));
          if pb <> nil then
            for i1 := 1 to integer(pb^) do begin
              pc := pointer(GetSidSubAuthority(FSid, i1 - 1));
              if pc = nil then
                   result := result + '-' + '?'
              else result := result + '-' + IntToStrEx(pc^);
            end;
        end;
      end;
    end else
      SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

function TIAccount.IsEqual(const otherAccount: IAccount) : boolean;

  function EqualName(name1, name2: string) : boolean;
  var i1, i2 : integer;
  begin
    result := IsTextEqual(name1, name2);
    if not result then begin
      i1 := Pos('\', name1);
      i2 := Pos('\', name2);
      result := ((i1 > 0) xor (i2 > 0)) and IsTextEqual(RetDelete(name1, 1, i1), RetDelete(name2, 1, i2));
    end;
  end;

begin
  result := (otherAccount <> nil) and
            ( (otherAccount.SelfAsTObject = self) or
              ( (FValid = otherAccount.IsValid) and
                ( ( OS.win9x and EqualName(FName, otherAccount.Name)     ) or
                  ( OS.winNT and EqualSid(GetPSid, otherAccount.GetPSid) )    ) ) );
end;

function TIAccount.GetMaxInterface : IBasic;
begin
  result := IAccount(self);
end;

function GetProcessSid(processHandle: dword; var saa: PSidAndAttributes) : boolean;
var token, size : dword;
begin
  result := false;
  if OpenProcessToken(processHandle, TOKEN_QUERY, token) then begin
    GetTokenInformation(token, TokenUser, nil, 0, size);
    GetMem(saa, size * 2);
    if GetTokenInformation(token, TokenUser, saa, size * 2, size) then
         result := true
    else FreeMem(saa);
    CloseHandle(token);
  end;
end;

function CurrentUser : IAccount;
var arrCh : array [0..MAX_PATH] of char;
    c1    : cardinal;
    saa   : PSidAndAttributes;
begin
  if OS.winNT then begin
    if GetProcessSid(GetCurrentProcess, saa) then begin
      result := Account(saa.Sid);
      FreeMem(saa);
    end else
      result := TIAccount.Create(false, GetLastError, '', '', nil, other);
  end else begin
    c1 := MAX_PATH;
    if GetUserName(arrCh, c1) then
         result := Account(arrCh)
    else result := TIAccount.Create(false, GetLastError, '', '', nil, other);
  end;
end;

function Everyone : IAccount;
const CEveryoneSia : TSidIdentifierAuthority = (value: (0, 0, 0, 0, 0, 1));
var sid : PSid;
begin
  if GetVersion and $80000000 = 0 then begin
    if AllocateAndInitializeSid(CEveryoneSia, 1, 0, 0, 0, 0, 0, 0, 0, 0, sid) then begin
      result := Account(sid);
      FreeSid(sid);
    end else
      result := TIAccount.Create(false, GetLastError, '', '', nil, other);
  end else
    result := Account('*');
end;

function AuthenticatedUsers : IAccount;
const CAuthenticatedUsersSia : TSidIdentifierAuthority = (value: (0, 0, 0, 0, 0, 5));
var sid : PSid;
begin
  if GetVersion and $80000000 = 0 then begin
    if AllocateAndInitializeSid(CAuthenticatedUsersSia, 1, 11, 0, 0, 0, 0, 0, 0, 0, sid) then begin
      result := Account(sid);
      FreeSid(sid);
    end else
      result := TIAccount.Create(false, GetLastError, '', '', nil, other);
  end else
    result := Account('*');
end;

function SidStrToSid(sidStr: string) : PSid;
var i1, i2, i3 : integer;
    parts      : array [0..8] of dword;
    sia        : TSidIdentifierAuthority;
begin
  result := nil;
  if (Length(sidStr) >= 5) and (UpChar(sidStr[1]) = 'S') and (sidStr[2] = '-') and
     (sidStr[3] = '1') and (sidStr[4] = '-') and (sidStr[5] in ['0'..'9']) then begin
    ZeroMemory(@parts, sizeOf(parts));
    i2 := 0;
    i3 := 5;
    for i1 := 6 to Length(sidStr) do
      if sidStr[i1] = '-' then begin
        if (i1 <= i3) or (i2 = 8) then
          exit;
        parts[i2] := StrToIntEx(false, @sidStr[i3], i1 - i3);
        inc(i2);
        i3 := i1 + 1;
        if i3 > Length(sidStr) then
          exit;
      end else
        if not (sidStr[i1] in ['0', '1'..'9']) then
          exit;
    parts[i2] := StrToIntEx(false, @sidStr[i3], Length(sidStr) + 1 - i3);
    if parts[0] <= 255 then begin
      ZeroMemory(@sia, sizeOf(sia));
      sia.Value[5] := byte(parts[0]);
      if not AllocateAndInitializeSid(sia, i2, parts[1], parts[2], parts[3], parts[4], parts[5], parts[6], parts[7], parts[8], result) then
        result := nil;
    end;
  end;
end;

function Account(name: string; group: TExtBool = other) : IAccount;
var sid : PSid;
begin
  if GetVersion and $80000000 = 0 then
       sid := SidStrToSid(name)
  else sid := nil;
  if sid <> nil then begin
    result := Account(sid);
    FreeSid(sid);
  end else begin
    result := AccountCache_Find(name, nil);
    if result = nil then begin
      result := TIAccount.Create(true, 0, '', name, nil, group);
      result.IsStillValid;
    end;
  end;
end;

function Account(sid: PSid) : IAccount;
begin
  result := AccountCache_Find('', sid);
  if result = nil then begin
    result := TIAccount.Create(true, 0, '', '', sid, other);
    result.IsStillValid;
  end;
end;

// ***************************************************************

type
  // forward...
  TIAcl = class;

  // *******************************************************************

  // Windows' internal structure for an Ace
  TAce = packed record
    type_      : TAceType;
    flags      : TAceFlags;
    size       : word;
    access     : cardinal;
    sidStart   : cardinal;
  end;
  PAce = ^TAce;

  // Windows' internal structure for an object Ace
  TObjAce = packed record
    type_      : TAceType;
    flags      : TAceFlags;
    size       : word;
    access     : cardinal;
    objFlags   : cardinal;
    objType    : TGuid;
    inhObjType : TGuid;
    sidStart   : cardinal;
  end;
  PObjAce = ^TObjAce;

  // implements IAce
  TIAce = class (TIBasic, IAce)
  public
    FAce     : PAce;
    FFreeAce : boolean;
    FAccount : IAccount;
    FAccess  : cardinal;  // only for win9x
    FTAcl    : TIAcl;
    FAcl     : IAcl;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: string;
                        pa: PAce; type_: TAceType; flags: TAceFlags; access: cardinal;
                        const account: IAccount; ownerAcl: TIAcl);
    destructor Destroy; override;

    function GetAccount : IAccount; virtual;

    function  GetType : TAceType;
    procedure SetType (value: TAceType);

    function  GetAccess : cardinal;
    procedure SetAccess (value: cardinal);

    function  GetFlags : TAceFlags;
    procedure SetFlags (value: TAceFlags);

    function GetPAce : pointer;
    function GetSize : word;

    function GetOwnerAcl : IAcl;

    // internal: Sets the "dirty" flag in the owner Acl
    procedure MakeOwnerAclDirty;

    function GetMaxInterface : IBasic; override;
  end;

  // implements IObjAce
  TIObjAce = class (TIAce, IObjAce)
  public
    // must be overridden, because the sid is not at the same place...
    function GetAccount : IAccount; override;

    function  GetObjFlags         : cardinal;
    function  GetObjType          : TGuid;
    function  GetInheritedObjType : TGuid;
    procedure SetObjFlags         (value: cardinal);
    procedure SetObjType          (value: TGuid   );
    procedure SetInheritedObjType (value: TGuid   );

    function GetMaxInterface : IBasic; override;
  end;

  // *******************************************************************

  // implements IAcl
  TIAcl = class(TICustomBasicList, IAcl)
    function IAcl.FindItem = FindItem2;
  public
    FAcl       : PAcl;
    FUsedSize  : integer;
    FSecurity  : ISecurityObject;
    FDirty     : boolean;
    FAllocated : boolean;  // only for win9x

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: string;
                        acl: PAcl; const owner: ISecurityObject);
    destructor Destroy; override;

    function IsStillValid : boolean;

    function GetItem (index: integer) : IAce;

    function NewItem (const account : IAccount;
                      access        : cardinal;
                      type_         : TAceType  = atAllowed;
                      flags         : TAceFlags = []       ) : integer;

    function AddItem    (const item : IBasic     ) : integer; override;
    function InsertItem (const item : IBasic;
                         index      : integer = 0) : integer; override;

    function FindItem2 (const account    : IAccount;
                        firstSearchIndex : integer = 0) : IAce;

    // must be overriden, a lot of more work has to be done
    function DeleteItem  (index         : integer ) : boolean; override;
    function DeleteItems (const account : IAccount) : boolean;

    function  IsAllocated : boolean;
    procedure Allocate;
    procedure Clear; override;
    procedure Deallocate;

    procedure SetFileAccess  (const account: IAccount; write: boolean);
    procedure SetPrintAccess (const account: IAccount; admin: boolean);

    function IsDirty : boolean;
    function Flush   : boolean;

    function GetPAcl     : PAcl;
    function GetSize     : integer;
    function GetUsedSize : integer;

    function GetOwnerSecurityObject : ISecurityObject;

    // internal: set size of Windows' Acl structure
    procedure SetSize (value: integer);

    function GetMaxInterface : IBasic; override;
  end;

  // *******************************************************************

  // internal types for winNT security functions
  TSecurityInfo    = (siOwner, siGroup, siDAcl, siSAcl);
  TSecurityInfoSet = set of TSecurityInfo;
  PPAcl            = ^PAcl;
  PPSid            = ^PSid;

  // internal types for win9x security functions
  TAccessInfo12Item  = packed record
                         userGroupName : pchar;
                         access        : word;
                       end;
  TAccessInfo12Items = array [0..maxInt shr 3 - 1] of TAccessInfo12Item;
  TAccessInfo12      = packed record
                         resource      : pchar;
                         attr          : smallInt;
                         itemCount     : smallInt;
                         items         : TAccessInfo12Items;
                       end;
  TPAccessInfo12     = ^TAccessInfo12;

  // implements ISecurityObject
  TISecurityObject = class (TIBasic, ISecurityObject)
  public
    FType   : TSecurityObjectType;
    FName   : wideString;
    FHandle : cardinal;
    FDAcl   : TIAcl;
    FSAcl   : TIAcl;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: string;
                        type_: TSecurityObjectType; name: wideString; handle: cardinal);

    function GetType : TSecurityObjectType;

    function GetName : wideString;

    function GetHandle : cardinal;

    function  GetOwner : IAccount;
    procedure SetOwner (const owner: IAccount);

    function  GetGroup : IAccount;
    procedure SetGroup (const group: IAccount);

    function  GetDAcl : IAcl;
    procedure SetDAcl (const dacl: IAcl);

    function  GetSAcl : IAcl;
    procedure SetSAcl (const sacl: IAcl);

    function  GetProtectedDAcl : boolean;
    procedure SetProtectedDAcl (value: boolean);
    function  GetProtectedSAcl : boolean;
    procedure SetProtectedSAcl (value: boolean);

    // internal security functions  (only winNT)
    function GetSecurityInfo (name     : wideString;
                              handle   : cardinal;
                              type_    : TSecurityObjectType;
                              what     : TSecurityInfoSet;
                              sidOwner : PPSid;
                              sidGroup : PPSid;
                              dacl     : PPAcl;
                              sacl     : PPAcl;
                              var sd   : PSecurityDescriptor) : boolean;
    function SetSecurityInfo (name     : wideString;
                              handle   : cardinal;
                              type_    : TSecurityObjectType;
                              what     : TSecurityInfoSet;
                              sidOwner : PSid;
                              sidGroup : PSid;
                              dacl     : PAcl;
                              sacl     : PAcl;
                              protDAcl : TPBoolean;
                              protSAcl : TPBoolean) : boolean;

    // internal security functions  (only win9x)
    function GetWin9xDAcl (path         : string;
                           type_        : TSecurityObjectType;
                           const owner  : ISecurityObject    ) : TIAcl;
    function SetWin9xDAcl (path         : string;
                           type_        : TSecurityObjectType;
                           const dacl   : IAcl               ) : boolean;

    function GetMaxInterface : IBasic; override;
  end;

// ***************************************************************

constructor TIAce.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: string;
                         pa: PAce; type_: TAceType; flags: TAceFlags; access: cardinal;
                         const account: IAccount; ownerAcl: TIAcl);
begin
  inherited Create(true, lastErrorNo, lastErrorStr);
  if FValid then begin
    FValid := (pa <> nil) or ((account <> nil) and account.IsValid);
    if FValid then begin
      if OS.winNT then begin
        FFreeAce := pa = nil;
        if FFreeAce then begin
          FAccount := account;
          FAce := AllocMem(sizeOf(TAce) - 4 + FAccount.SidSize);
          FAce^.size := sizeOf(TAce) - 4 + FAccount.SidSize;
          FAce^.type_ := type_;
          FAce^.flags := flags;
          FAce^.access := access;
          Move(FAccount.PSid^, FAce^.sidStart, FAccount.SidSize);
          FTAcl := ownerAcl;
        end else begin
          FAce := pa;
          FTAcl := ownerAcl;
        end;
      end else begin
        FAccount := account;
        FAccess := byte(access);
        FTAcl := ownerAcl;
      end;
    end else SetLastError(ERROR_INVALID_PARAMETER);
  end;
end;

destructor TIAce.Destroy;
begin
  if FFreeAce then FreeMem(FAce);
  inherited;
end;

function TIAce.GetAccount : IAccount;
begin
  if FAccount = nil then
    if CheckValid then
         FAccount := Account(@FAce^.sidStart)
    else FAccount := TIAccount.Create(false, CErrorNo_AmInvalid, CErrorStr_AmInvalid, '', nil, other);
  result := FAccount;
end;

function TIAce.GetType : TAceType;
begin
  if CheckValid and OS.winNT then result := FAce^.type_
  else                            result := atAllowed;
end;

procedure TIAce.SetType(value: TAceType);
begin
  if CheckValid then
    if GetType <> value then
      if OS.winNT then begin
        if (FAce^.type_ >= atAllowedObject) = (value >= atAllowedObject) then begin
          FAce^.type_ := value;
          MakeOwnerAclDirty;
        end else SetLastError(CErrorNo_CantChangeAceType, CErrorStr_CantChangeAceType);
      end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

function TIAce.GetAccess : cardinal;
begin
  if CheckValid and OS.winNT then result := FAce^.access
  else                            result := FAccess;
end;

procedure TIAce.SetAccess(value: cardinal);
begin
  if CheckValid then
    if GetAccess <> value then begin
      if OS.winNT then FAce^.access := value
      else             FAccess      := value;
      MakeOwnerAclDirty;
    end;
end;

function TIAce.GetFlags : TAceFlags;
begin
  if CheckValid and OS.winNT then result := FAce^.flags
  else                            result := [];
end;

procedure TIAce.SetFlags(value: TAceFlags);
begin
  if CheckValid then
    if GetFlags <> value then 
      if OS.winNT then begin
        FAce^.flags := value;
        MakeOwnerAclDirty;
      end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

function TIAce.GetPAce : pointer;
begin
  result := FAce;
end;

function TIAce.GetSize : word;
begin
  if CheckValid and OS.winNT then result := FAce^.size
  else                            result := 0;
end;

function TIAce.GetOwnerAcl : IAcl;
begin
  if FTAcl = nil then begin
    FTAcl := TIAcl.Create(false, CErrorNo_LonelyAce, CErrorStr_LonelyAce, nil, nil);
    FAcl := FTAcl;
    SetLastError(CErrorNo_LonelyAce, CErrorStr_LonelyAce);
  end;
  result := FTAcl;
end;

procedure TIAce.MakeOwnerAclDirty;
begin
  if (FTAcl <> nil) and FTAcl.FValid then
    FTAcl.FDirty := true;
end;

function TIAce.GetMaxInterface : IBasic;
begin
  result := IAce(self);
end;

function TIObjAce.GetAccount : IAccount;
begin
  if FAccount = nil then
    if CheckValid then
         FAccount := Account(@PObjAce(FAce)^.sidStart)
    else FAccount := TIAccount.Create(false, CErrorNo_AmInvalid, CErrorStr_AmInvalid, '', nil, other);
  result := FAccount;
end;

function TIObjAce.GetObjFlags : cardinal;
begin
  if CheckValid then result := PObjAce(FAce)^.objFlags
  else               result := 0;
end;

function TIObjAce.GetObjType : TGuid;
begin
  if CheckValid then result := PObjAce(FAce)^.objType
  else               ZeroMemory(@result,sizeOf(TGuid));
end;

function TIObjAce.GetInheritedObjType : TGuid;
begin
  if CheckValid then result := PObjAce(FAce)^.inhObjType
  else               ZeroMemory(@result,sizeOf(TGuid));
end;

procedure TIObjAce.SetObjFlags(value: cardinal);
begin
  if CheckValid then
    if PObjAce(FAce)^.objFlags <> value then begin
      PObjAce(FAce)^.objFlags := value;
      MakeOwnerAclDirty;
    end;
end;

procedure TIObjAce.SetObjType(value: TGuid);
begin
  if CheckValid then
    if not CompareMem(@PObjAce(FAce)^.objType, @value, sizeOf(TGuid)) then begin
      PObjAce(FAce)^.objType := value;
      MakeOwnerAclDirty;
    end;
end;

procedure TIObjAce.SetInheritedObjType(value: TGuid);
begin
  if CheckValid then
    if not CompareMem(@PObjAce(FAce)^.inhobjType, @value, sizeOf(TGuid)) then begin
      PObjAce(FAce)^.inhObjType := value;
      MakeOwnerAclDirty;
    end;
end;

function TIObjAce.GetMaxInterface : IBasic;
begin
  result := IObjAce(self);
end;

function NewAce(const account : IAccount;
                access        : cardinal;
                type_         : TAceType  = atAllowed;
                flags         : TAceFlags = []       ) : IAce;
begin
  if type_ >= atAllowedObject then
    raise Exception.Create('You can''t create a new IObjectAce interface with this function.');
  result := TIAce.Create(true, 0, '', nil, type_, flags, access, account, nil);
end;

// ***************************************************************

constructor TIAcl.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: string;
                         acl: PAcl; const owner: ISecurityObject);
var ace : PAce;
    i1  : integer;
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    if OS.winNT and (acl <> nil) then begin
      FAcl := AllocMem(acl^.AclSize);
      Move(acl^, FAcl^, acl^.AclSize);
      integer(ace) := integer(FAcl) + 8;
      for i1 := 1 to FAcl^.AceCount do begin
        if ace^.type_ < atAllowedObject then
             inherited AddItem(   IAce(TIAce   .Create(true, 0, '', ace, atAllowed, [], 0, nil, self)))
        else inherited AddItem(IObjAce(TIObjAce.Create(true, 0, '', ace, atAllowed, [], 0, nil, self)));
        integer(ace) := integer(ace) + ace^.size;
      end;
      FUsedSize := integer(ace) - integer(FAcl);
    end;
    FSecurity := owner;
  end;
end;

destructor TIAcl.Destroy;
var acl1 : windows.PAcl;
begin
  if FValid then begin
    if FDirty then Flush;
    if FSecurity <> nil then
      with TISecurityObject(FSecurity.SelfAsTObject) do
        if      FDAcl = self then FDAcl := nil
        else if FSAcl = self then FSAcl := nil;
    acl1 := FAcl;
    FAcl := nil;
    SetCapacity(0);  // must be called before freeing FAcl...
    if acl1 <> nil then FreeMem(acl1);
  end else
    if FAcl <> nil then FreeMem(FAcl);
  inherited;
end;

function TIAcl.IsStillValid : boolean;
begin
  if FValid and (FAcl <> nil) then begin
    FValid := IsValidAcl(FAcl^);
    if not FValid then SetLastError(GetLastError);
  end;
  result := FValid;
end;

function TIAcl.GetItem(index: integer) : IAce;
begin
  if (index < 0) or (index >= FCount) then
       result := TIAce.Create(false, CErrorNo_IndexOutOfRange, CErrorStr_IndexOutOfRange, nil, atAllowed, [], 0, nil, nil)
  else result := IAce(FItems[index]);
end;

function TIAcl.NewItem(const account : IAccount;
                       access        : cardinal;
                       type_         : TAceType  = atAllowed;
                       flags         : TAceFlags = []       ) : integer;
begin
  if CheckValid then result := AddItem(NewAce(account, access, type_, flags))
  else               result := -1;
end;

function TIAcl.AddItem(const item: IBasic) : integer;
var pace1 : PAce;
    ace   : IAce absolute item;
    ias   : integer;
begin
  result := -1;
  if CheckValid then
    if item.IsValid then begin
      if OS.winNT then begin
        ias := ace.size;
        if FUsedSize = 0 then SetSize(sizeOf(TAcl) + ias)
        else                  SetSize(FUsedSize    + ias);
        integer(pace1) := integer(FAcl) + FUsedSize;
        Move(ace.PAce^, pace1^, ias);
        inc(FAcl^.AceCount);
//        inc(FAcl^.AclSize, ias);
        inc(FUsedSize, ias);
        result := FCount;
        FDirty := true;
        if pace1^.type_ < atAllowedObject then
             inherited AddItem(   IAce(TIAce   .Create(true, 0, '', pace1, atAllowed, [], 0, nil, self)))
        else inherited AddItem(IObjAce(TIObjAce.Create(true, 0, '', pace1, atAllowed, [], 0, nil, self)));
      end else begin
        result := inherited AddItem(IAce(TIAce.Create(true, 0, '', nil, atAllowed, [],
                                                      ace.Access, ace.Account, self)));
        FDirty := true;
        FAllocated := true;
      end;
    end else SetLastError(ERROR_INVALID_PARAMETER);
end;

function TIAcl.InsertItem(const item : IBasic;
                          index      : integer = 0) : integer;
var pace1 : PAce;
    i1    : integer;
    ace   : IAce absolute item;
    ias   : integer;
begin
  result := -1;
  if CheckValid then
    if index >= 0 then begin
      if item.IsValid then begin
        if OS.winNT then begin
          ias := ace.size;
          if FUsedSize = 0 then SetSize(sizeOf(TAcl) + ias)
          else                  SetSize(FUsedSize    + ias);
          if      index  = 0      then integer(pace1) := integer(FAcl) + 8
          else if index >= FCount then integer(pace1) := integer(FAcl) + FUsedSize
          else                         pace1 := IAce(FItems[index]).PAce;
          if index < FCount then
            Move(pace1^, pointer(integer(pace1) + ias)^, integer(FAcl) + FUsedSize - integer(pace1));
          Move(ace.PAce^, pace1^, ias);
          inc(FAcl^.AceCount);
          inc(FUsedSize, ias);
          result := index;
          FDirty := true;
          if pace1^.type_ < atAllowedObject then
               inherited InsertItem(   IAce(TIAce   .Create(true, 0, '', pace1, atAllowed, [], 0, nil, self)), index)
          else inherited InsertItem(IObjAce(TIObjAce.Create(true, 0, '', pace1, atAllowed, [], 0, nil, self)), index);
          integer(pace1) := integer(pace1) + pace1^.size;
          for i1 := index + 1 to FCount - 1 do begin
            TIAce(FItems[i1].SelfAsTObject).FAce := pace1;
            integer(pace1) := integer(pace1) + pace1^.size;
          end;
        end else begin
          result := inherited InsertItem(IAce(TIAce.Create(true, 0, '', nil, atAllowed, [],
                                                           ace.Access, ace.Account, self)),
                                         index);
          FDirty := true;
          FAllocated := true;
        end;
      end else SetLastError(ERROR_INVALID_PARAMETER);
    end else SetLastError(CErrorNo_InvalidIndex, CErrorStr_InvalidIndex);
end;

function TIAcl.FindItem2(const account    : IAccount;
                         firstSearchIndex : integer = 0) : IAce;
var i1 : integer;
begin
  if firstSearchIndex >= 0 then begin
    Lock;
    try
      for i1 := firstSearchIndex to FCount - 1 do
        if IAce(FItems[i1]).Account.IsEqual(account) then begin
          result := IAce(FItems[i1]);
          FSuccess := true;
          exit;
        end;
      SetLastError(ERROR_FILE_NOT_FOUND);
    finally Unlock end;
  end else SetLastError(CErrorNo_InvalidIndex, CErrorStr_InvalidIndex);
  if result = nil then
    result := TIAce.Create(false, GetLastErrorNo, GetLastErrorStr, nil, atAllowed, [], 0, nil, nil);
end;

function TIAcl.DeleteItem(index: integer) : boolean;
var i1    : integer;
    pace1 : PAce;
    ias   : integer;
begin
  result := false;
  if CheckValid then
    if (index >= 0) and (index < FCount) then begin
      result := true;
      if OS.winNT then begin
        pace1 := IAce(FItems[index]).PAce;
        ias := pace1^.size;
        with TIAce(FItems[index].SelfAsTObject) do
          if FRefCount > 1 then begin
            FAce := AllocMem(ias);
            Move(pace1^, FAce^, ias);
            FFreeAce := true;
            if FAcl = nil then FTAcl := nil;
          end;
        if FAcl <> nil then begin
          Move(pointer(integer(pace1) + ias)^, pace1^, integer(FAcl) + FUsedSize - integer(pace1) - ias);
          dec(FAcl^.AceCount);
        end;
        dec(FUsedSize, ias);
      end else pace1 := nil;
      FDirty := true;
      inherited DeleteItem(index);
      if FAcl <> nil then
        for i1 := index to FCount - 1 do begin
          TIAce(FItems[i1].SelfAsTObject).FAce := pace1;
          integer(pace1) := integer(pace1) + pace1^.size;
        end;
    end else SetLastError(CErrorNo_InvalidIndex, CErrorStr_InvalidIndex);
end;

function TIAcl.DeleteItems(const account: IAccount) : boolean;
var i1 : integer;
begin
  result := false;
  if CheckValid then
    if (account <> nil) and account.IsValid then begin
      for i1 := FCount - 1 downto 0 do
        if IAce(FItems[i1]).Account.IsEqual(account) then begin
          result := true;
          DeleteItem(i1);
        end;
      if not result then
        SetLastError(ERROR_FILE_NOT_FOUND);
    end else SetLastError(ERROR_INVALID_PARAMETER);
end;

function TIAcl.IsAllocated : boolean;
begin
  result := CheckValid and ((FAcl <> nil) or FAllocated);
end;

procedure TIAcl.Allocate;
begin
  if CheckValid then
    if not IsAllocated then begin
      if OS.win9x then FAllocated := true
      else             SetSize(sizeOf(TAcl));
      FDirty := true;
    end;
end;

procedure TIAcl.Clear;
begin
  Deallocate; Allocate;
end;

procedure TIAcl.Deallocate;
var acl1 : windows.PAcl;
begin
  if CheckValid then
    if IsAllocated then begin
      if OS.winNT then begin
        acl1 := FAcl;
        FAcl := nil;
        SetCapacity(0);
        FreeMem(acl1);
      end else begin
        SetCapacity(0);
        FAllocated := false;
      end;
      FDirty := true;
    end;
end;

procedure TIAcl.SetFileAccess(const account: IAccount; write: boolean);
const CFileAccess : array [boolean, boolean] of cardinal =
                    (($001200A9, $001F01FF), ($00000081, $000000B7));
begin
  if CheckValid then
    if (account <> nil) and account.IsValid then begin
      DeleteItems(account);
      NewItem(account, CFileAccess[OS.win9x, write], atAllowed, [afObjectInherit, afContainerInherit]);
    end else SetLastError(ERROR_INVALID_PARAMETER);
end;

procedure TIAcl.SetPrintAccess(const account: IAccount; admin: boolean);
var ace : IAce;
begin
  if CheckValid then
    if (account <> nil) and account.IsValid then begin
      if OS.winNT then begin
        DeleteItems(account);
        if admin then begin
          NewItem(account, $000F000C, atAllowed, []);
          NewItem(account, $000F0010, atAllowed, [afObjectInherit, afInheritOnly]);
        end else NewItem(account, $00020008, atAllowed, []);
      end else begin
        ace := FindItem2(account);
        if ace.IsValid then ace.SetAccess($B7)
        else                NewItem(account, $B7, atAllowed, []);
      end;
    end else SetLastError(ERROR_INVALID_PARAMETER);
end;

function TIAcl.IsDirty : boolean;
begin
  result := FDirty;
end;

function TIAcl.Flush : boolean;
begin
  result := CheckValid;
  if result then
    if FDirty and (FSecurity <> nil) and FSecurity.IsValid then begin
      with TISecurityObject(FSecurity.SelfAsTObject) do begin
        if      FDacl = self then SetDAcl(self)
        else if FSacl = self then SetSAcl(self);
        result := FSuccess;
      end;
      if not result then
        SetLastError(FSecurity.LastErrorNo, FSecurity.LastErrorStr);
    end;
  FDirty := false;
end;

function TIAcl.GetPAcl : PAcl;
begin
  result := FAcl;
end;

function TIAcl.GetSize : integer;
begin
  if CheckValid and (FAcl <> nil) then result := FAcl^.AclSize
  else                                 result := 0;
end;

function TIAcl.GetUsedSize : integer;
begin
  result := FUsedSize;
end;

function TIAcl.GetOwnerSecurityObject : ISecurityObject;
begin
  if FSecurity = nil then begin
    FSecurity := TISecurityObject.Create(false, CErrorNo_LonelyAcl, CErrorStr_LonelyAcl, seUnknown, '', 0);
    SetLastError(CErrorNo_LonelyAcl, CErrorStr_LonelyAcl);
  end;
  result := FSecurity;
end;

procedure TIAcl.SetSize(value: integer);
const ACL_REVISION = 2;
var i1  : integer;
    ace : PAce;
begin
  if value < FUsedSize then value := FUsedSize;
  if FAcl = nil then begin
    FAcl := AllocMem(value);
    FAcl^.AclRevision := ACL_REVISION;
    FAcl^.AclSize := value;
    FAcl^.AceCount := 0;
    FUsedSize := 8;
  end else
    if (value > FAcl^.AclSize) or (value < FAcl^.AclSize div 4) then begin
      if value > FAcl^.AclSize then value := value + value div 2;
      FAcl^.AclSize := value;
      ReAllocMem(FAcl, value);
      integer(ace) := integer(FAcl) + 8;
      for i1 := 0 to FCount - 1 do begin
        TIAce(FItems[i1].SelfAsTObject).FAce := ace;
        integer(ace) := integer(ace) + ace^.size;
      end;
    end;
end;

function TIAcl.GetMaxInterface : IBasic;
begin
  result := IAcl(self);
end;

function Acl(acl: PAcl) : IAcl;
begin
  result := TIAcl.Create(true, 0, '', acl, nil);
  result.IsStillValid;
end;

function NewAcl : IAcl;
begin
  result := TIAcl.Create(true, 0, '', nil, nil);
end;

// ***************************************************************

var
  // winNT security function variables
  GetNamedSecurityInfoW : function (objectName   : PWideChar;
                                    objectType   : TSecurityObjectType;
                                    securityInfo : cardinal;
                                    sidOwner     : PPSid;
                                    sidGroup     : PPSid;
                                    dacl         : PPAcl;
                                    sacl         : PPAcl;
                                    var sd       : PSecurityDescriptor) : cardinal; stdcall = nil;
  SetNamedSecurityInfoW : function (objectName   : PWideChar;
                                    objectType   : TSecurityObjectType;
                                    securityInfo : cardinal;
                                    sidOwner     : PSid;
                                    sidGroup     : PSid;
                                    dacl         : PAcl;
                                    sacl         : PAcl               ) : cardinal; stdcall = nil;
  GetSecurityInfoW      : function (handle       : cardinal;
                                    objectType   : TSecurityObjectType;
                                    securityInfo : cardinal;
                                    sidOwner     : PPSid;
                                    sidGroup     : PPSid;
                                    dacl         : PPAcl;
                                    sacl         : PPAcl;
                                    var sd       : PSecurityDescriptor) : cardinal; stdcall = nil;
  SetSecurityInfoW      : function (handle       : cardinal;
                                    objectType   : TSecurityObjectType;
                                    securityInfo : cardinal;
                                    sidOwner     : PSid;
                                    sidGroup     : PSid;
                                    dacl         : PAcl;
                                    sacl         : PAcl               ) : cardinal; stdcall = nil;

  // win9x security function variables
  NetAccessGetInfo9x : function (servername     : pchar;
                                 resource       : pchar;
                                 level          : smallInt;
                                 var buf        : TAccessInfo12;
                                 bufSize        : word;
                                 var totalAvail : word    ) : cardinal; stdcall = nil;
  NetAccessSetInfo9x : function (servername     : pchar;
                                 resource       : pchar;
                                 level          : smallInt;
                                 var buf        : TAccessInfo12;
                                 bufSize        : word;
                                 parmNum        : smallInt) : cardinal; stdcall = nil;
  NetAccessDel9x     : function (servername     : pchar;
                                 resource       : pchar   ) : cardinal; stdcall = nil;
  NetAccessAdd9x     : function (servername     : pchar;
                                 level          : smallInt;
                                 var buf        : TAccessInfo12;
                                 bufSize        : word    ) : cardinal; stdcall = nil;

  // security function variables ready?
  SecurityInfoReady : boolean = false;

constructor TISecurityObject.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: string;
                                    type_: TSecurityObjectType; name: wideString; handle: cardinal);
var dll : cardinal;
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    FValid := type_ <> seUnknown;
    if FValid then begin
      FValid := (type_ in [sePrinter, seFile]) or OS.winNT;
      if FValid then begin
        FValid := (handle <> maxCard) and ((name <> '') or ((handle <> 0) and OS.winNT));
        if FValid then begin
          if not SecurityInfoReady then begin
            SecurityInfoReady := true;
            if OS.winNT then begin
              dll := LoadLibrary(advapi32);
              // Windows unloads the library automatically when our program terminates...
              GetNamedSecurityInfoW := GetProcAddress(dll, 'GetNamedSecurityInfoW');
              SetNamedSecurityInfoW := GetProcAddress(dll, 'SetNamedSecurityInfoW');
                   GetSecurityInfoW := GetProcAddress(dll,      'GetSecurityInfo' );
                   SetSecurityInfoW := GetProcAddress(dll,      'SetSecurityInfo' );
            end else begin
              dll := LoadLibrary('svrApi.dll');
              NetAccessGetInfo9x := GetProcAddress(dll, 'NetAccessGetInfo');
              NetAccessSetInfo9x := GetProcAddress(dll, 'NetAccessSetInfo');
              NetAccessDel9x     := GetProcAddress(dll, 'NetAccessDel'    );
              NetAccessAdd9x     := GetProcAddress(dll, 'NetAccessAdd'    );
            end;
          end;
          if OS.win9x and (type_ = sePrinter) then name := '\PRINT\' + name;
          FType := type_;
          FName := name;
          FHandle := handle;
        end else SetLastError(ERROR_INVALID_PARAMETER);
      end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
    end else SetLastError(ERROR_INVALID_PARAMETER);
  end;
end;

function TISecurityObject.GetType : TSecurityObjectType;
begin
  result := FType;
end;

function TISecurityObject.GetName : wideString;
begin
  result := FName;
end;

function TISecurityObject.GetHandle : cardinal;
begin
  result := FHandle;
end;

function TISecurityObject.GetOwner : IAccount;
var psd  : PSecurityDescriptor;
    psid : windows.PSid;
begin
  if CheckValid then
    if OS.winNT then begin
      if GetSecurityInfo(FName, FHandle, FType, [siOwner], @psid, nil, nil, nil, psd) then
        try
          result := Account(psid);
        finally LocalFree(cardinal(psd)) end;
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
  if result = nil then
    result := TIAccount.Create(false, FLastErrorNo, FLastErrorStr, '', nil, other);
end;

procedure TISecurityObject.SetOwner(const owner: IAccount);
begin
  if CheckValid then
    if OS.winNT then begin
      if (owner <> nil) and owner.IsValid then
           SetSecurityInfo(FName, FHandle, FType, [siOwner], owner.PSid, nil, nil, nil, nil, nil)
      else SetLastError(ERROR_INVALID_PARAMETER);
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

function TISecurityObject.GetGroup : IAccount;
var psd  : PSecurityDescriptor;
    psid : windows.PSid;
begin
  if CheckValid then
    if OS.winNT then begin
      if GetSecurityInfo(FName, FHandle, FType, [siGroup], nil, @psid, nil, nil, psd) then
        try
          result := Account(psid);
        finally LocalFree(cardinal(psd)) end;
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
  if result = nil then
    result := TIAccount.Create(false, FLastErrorNo, FLastErrorStr, '', nil, other);
end;

procedure TISecurityObject.SetGroup(const group: IAccount);
begin
  if CheckValid then
    if OS.winNT then begin
      if (group <> nil) and group.IsValid then
           SetSecurityInfo(FName, FHandle, FType, [siGroup], nil, group.PSid, nil, nil, nil, nil)
      else SetLastError(ERROR_INVALID_PARAMETER);
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

function TISecurityObject.GetDAcl : IAcl;
var psd  : PSecurityDescriptor;
    pacl : windows.PAcl;
begin
  if FDAcl = nil then begin
    if CheckValid then
      if OS.winNT then begin
        if GetSecurityInfo(FName, FHandle, FType, [siDAcl], nil, nil, @pacl, nil, psd) then
          try
            FDAcl := TIAcl.Create(true, 0, '', pacl, self);
            result := FDAcl;
          finally LocalFree(cardinal(psd)) end;
      end else begin
        FDAcl := GetWin9xDAcl(FName, FType, self);
        result := FDAcl;
      end;
    if result = nil then
      result := TIAcl.Create(false, FLastErrorNo, FLastErrorStr, nil, nil);
  end else result := FDAcl;
end;

procedure TISecurityObject.SetDAcl(const dacl: IAcl);
begin
  if CheckValid then
    if (dacl <> nil) and dacl.IsValid then begin
      if ((OS.winNT and SetSecurityInfo(FName, FHandle, FType, [siDAcl], nil, nil, dacl.PAcl, nil, nil, nil)) or
          (OS.win9x and SetWin9xDAcl   (FName,          FType,                     dacl                    ))    ) and
         (FDAcl <> dacl.SelfAsTObject) then begin
        if FDAcl <> nil then FDAcl.FSecurity := nil;
        FDAcl := TIAcl(dacl.SelfAsTObject);
        FDAcl.FSecurity := self;
      end;
    end else SetLastError(ERROR_INVALID_PARAMETER);
end;

function TISecurityObject.GetSAcl : IAcl;
var psd  : PSecurityDescriptor;
    pacl : windows.PAcl;
begin
  if FSAcl = nil then begin
    if CheckValid then
      if OS.winNT then begin
        if GetSecurityInfo(FName, FHandle, FType, [siSAcl], nil, nil, nil, @pacl, psd) then
          try
            FSAcl := TIAcl.Create(true, 0, '', pacl, self);
            result := FSAcl;
          finally LocalFree(cardinal(psd)) end;
      end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
    if result = nil then
      result := TIAcl.Create(false, FLastErrorNo, FLastErrorStr, nil, nil);
  end else result := FSAcl;
end;

procedure TISecurityObject.SetSAcl(const sacl: IAcl);
begin
  if CheckValid then
    if OS.winNT then begin
      if (sacl <> nil) and sacl.IsValid then begin
        if SetSecurityInfo(FName, FHandle, FType, [siSAcl], nil, nil, nil, sacl.PAcl, nil, nil) and
           (FSAcl <> sacl.SelfAsTObject) then begin
          if FSAcl <> nil then FSAcl.FSecurity := nil;
          FSAcl := TIAcl(sacl.SelfAsTObject);
          FSAcl.FSecurity := self;
        end;
      end else SetLastError(ERROR_INVALID_PARAMETER);
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

function TISecurityObject.GetProtectedDAcl : boolean;
var psd  : PSecurityDescriptor;
    dacl : PAcl;
begin
  result := false;
  if CheckValid then
    if OS.winNT then begin
      if GetSecurityInfo(FName, FHandle, FType, [siDAcl], nil, nil, @dacl, nil, psd) then begin
        result := psd^.control and SE_DACL_PROTECTED <> 0;
        LocalFree(cardinal(psd));
      end;
    end else
      SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

procedure TISecurityObject.SetProtectedDAcl(value: boolean);
begin
  if CheckValid then
    if OS.winNT then
         SetSecurityInfo(FName, FHandle, FType, [], nil, nil, nil, nil, @value, nil)
    else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

function TISecurityObject.GetProtectedSAcl : boolean;
var psd  : PSecurityDescriptor;
    sacl : PAcl;
begin
  result := false;
  if CheckValid then
    if OS.winNT then begin
      if GetSecurityInfo(FName, FHandle, FType, [siSAcl], nil, nil, nil, @sacl, psd) then begin
        result := psd^.control and SE_SACL_PROTECTED <> 0;
        LocalFree(cardinal(psd));
      end;
    end else
      SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

procedure TISecurityObject.SetProtectedSAcl(value: boolean);
begin
  if CheckValid then
    if OS.winNT then
         SetSecurityInfo(FName, FHandle, FType, [], nil, nil, nil, nil, nil, @value)
    else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

function TISecurityObject.GetSecurityInfo(name     : wideString;
                                          handle   : cardinal;
                                          type_    : TSecurityObjectType;
                                          what     : TSecurityInfoSet;
                                          sidOwner : PPSid;
                                          sidGroup : PPSid;
                                          dacl     : PPAcl;
                                          sacl     : PPAcl;
                                          var sd   : PSecurityDescriptor) : boolean;
var c1 : cardinal;
    b1 : longBool;
begin
  result := false;
  if type_ = seKernelObject then begin
    if name = '' then begin
      c1 := 0;
      GetKernelObjectSecurity(handle, byte(what), nil, 0, c1);
      if c1 > 0 then begin
        sd := pointer(LocalAlloc(LPTR, c1));
        try
          if GetKernelObjectSecurity(handle, byte(what), sd, c1, c1) and
             ( (not (siOwner in what)) or GetSecurityDescriptorOwner(sd, sidOwner^, b1) ) and
             ( (not (siGroup in what)) or GetSecurityDescriptorGroup(sd, sidGroup^, b1) ) and
             ( (not (siDAcl  in what)) or GetSecurityDescriptorDAcl (sd, b1, dacl^, b1) ) and
             ( (not (siSAcl  in what)) or GetSecurityDescriptorSAcl (sd, b1, sacl^, b1) ) then
            result := true
          else
            SetLastError(GetLastError);
        finally
          if not result then
            LocalFree(dword(sd));
        end;
      end else SetLastError(GetLastError);
    end else SetLastError(ERROR_INVALID_PARAMETER);
  end else begin
    if name <> '' then
      c1 := GetNamedSecurityInfoW(PWideChar(name), type_, byte(what), sidOwner, sidGroup, dacl, sacl, sd)
    else
      c1 := GetSecurityInfoW(handle, type_, byte(what), sidOwner, sidGroup, dacl, sacl, sd);
    if c1 = 0 then result := true
    else           SetLastError(c1);
  end;
end;

function TISecurityObject.SetSecurityInfo(name     : wideString;
                                          handle   : cardinal;
                                          type_    : TSecurityObjectType;
                                          what     : TSecurityInfoSet;
                                          sidOwner : PSid;
                                          sidGroup : PSid;
                                          dacl     : PAcl;
                                          sacl     : PAcl;
                                          protDAcl : TPBoolean;
                                          protSAcl : TPBoolean) : boolean;
const PROTECTED_DACL_SECURITY_INFORMATION   = $80000000;
      UNPROTECTED_DACL_SECURITY_INFORMATION = $20000000;
      PROTECTED_SACL_SECURITY_INFORMATION   = $40000000;
      UNPROTECTED_SACL_SECURITY_INFORMATION = $10000000;
var sd         : TSecurityDescriptor;
    c1         : cardinal;
    aclD, aclS : PAcl;
    sdD, sdS   : PSecurityDescriptor;
begin
  result := false;
  if type_ = seKernelObject then begin
    if InitializeSecurityDescriptor(@sd, SECURITY_DESCRIPTOR_REVISION) and
       ( (not (siOwner in what)) or SetSecurityDescriptorOwner(@sd, sidOwner, false) ) and
       ( (not (siGroup in what)) or SetSecurityDescriptorGroup(@sd, sidGroup, false) ) and
       ( (not (siDAcl  in what)) or SetSecurityDescriptorDAcl (@sd, dacl <> nil, dacl, false) ) and
       ( (not (siSAcl  in what)) or SetSecurityDescriptorSAcl (@sd, sacl <> nil, sacl, false) ) and
       SetKernelObjectSecurity(handle, byte(what), @sd) then
      result := true
    else
      SetLastError(GetLastError);
  end else begin
    sdD := nil;
    if (protDAcl <> nil) and (not (siDacl in what)) then 
      if GetSecurityInfo(name, handle, type_, [siDacl], nil, nil, @aclD, nil, sdD) then begin
        what := what + [siDacl];
        dacl := aclD;
      end else begin
        protDAcl := nil;
        sdD      := nil;
      end;
    sdS := nil;
    if (protSAcl <> nil) and (not (siSacl in what)) then
      if GetSecurityInfo(name, handle, type_, [siSacl], nil, nil, nil, @aclS, sdS) then begin
        what := what + [siSacl];
        sacl := aclS;
      end else begin
        protSAcl := nil;
        sdS      := nil;
      end;
    c1 := byte(what);
    if protDAcl <> nil then
      if protDAcl^ then
           c1 := c1 +   PROTECTED_DACL_SECURITY_INFORMATION
      else c1 := c1 + UNPROTECTED_DACL_SECURITY_INFORMATION;
    if protSAcl <> nil then
      if protSAcl^ then
           c1 := c1 +   PROTECTED_SACL_SECURITY_INFORMATION
      else c1 := c1 + UNPROTECTED_SACL_SECURITY_INFORMATION;
    if name <> '' then
      c1 := SetNamedSecurityInfoW(PWideChar(name), type_, c1, sidOwner, sidGroup, dacl, sacl)
    else
      c1 := SetSecurityInfoW(handle, type_, c1, sidOwner, sidGroup, dacl, sacl);
    if c1 = 0 then
         result := true
    else SetLastError(c1);
    if sdD <> nil then LocalFree(dword(sdD));
    if sdS <> nil then LocalFree(dword(sdS));
  end;
end;

function TISecurityObject.GetWin9xDAcl(path        : string;
                                       type_       : TSecurityObjectType;
                                       const owner : ISecurityObject    ) : TIAcl;
var w1   : word;
    ai12 : TPAccessInfo12;
    i1   : integer;
    c1   : cardinal;
begin
  result := nil;
  w1 := 0;
  c1 := NetAccessGetInfo9x(nil, pchar(path), 12, TPAccessInfo12(nil)^, 0, w1);
  if (w1 <> 0) or (c1 = 2222) then begin   // 2222 = resource not found
    if w1 <> 0 then begin
      ai12 := AllocMem(w1);
      try
        c1 := NetAccessGetInfo9x(nil, pchar(path), 12, ai12^, w1, w1);
        if c1 = 0 then begin
          result := TIAcl.Create(true, 0, '', nil, owner);
          if ai12 <> nil then
            for i1 := 0 to ai12^.itemCount - 1 do
              with ai12^.items[i1] do
                if (userGroupName[0] <> #0) and (access <> 0) then
                  result.NewItem(Account(string(userGroupName), TExtBool(access and $8000 <> 0)),
                                 byte(access), atAllowed, []);
        end else SetLastError(c1);
      finally FreeMem(ai12) end;
    end else result := TIAcl.Create(true, 0, '', nil, owner);
  end else SetLastError(c1);
end;

function TISecurityObject.SetWin9xDAcl(path       : string;
                                       type_      : TSecurityObjectType;
                                       const dacl : IAcl               ) : boolean;
var ai12 : TPAccessInfo12;
    s1   : string;
    i1   : integer;
    c1   : cardinal;
begin
  ai12 := nil;
  try
    if dacl.IsAllocated then begin
      if dacl.ItemCount > 0 then begin
        ai12 := AllocMem( sizeOf(TAccessInfo12) - sizeOf(TAccessInfo12Items) +
                          dacl.ItemCount * sizeOf(TAccessInfo12Item)           );
        ai12^.itemCount := dacl.ItemCount;
        for i1 := 0 to dacl.ItemCount - 1 do begin
          ai12^.items[i1].userGroupName := pchar(dacl[i1].Account.Name);
          if dacl[i1].Account.Type_ = atGroup then
               ai12^.items[i1].access := dacl[i1].access or $8000
          else ai12^.items[i1].access := dacl[i1].access;
        end;
      end;
    end else begin
      ai12 := AllocMem(sizeOf(TAccessInfo12) - sizeOf(TAccessInfo12Items) + sizeOf(TAccessInfo12Item));
      s1 := '*';
      ai12^.itemCount := 1;
      ai12^.items[0].userGroupName := pchar(s1);
      ai12^.items[0].access := $80B7;
    end;
    c1 := NetAccessDel9x(nil, pchar(path));
    if ai12 <> nil then begin
      ai12^.resource := pchar(path);
      c1 := NetAccessAdd9x(nil, 2, ai12^,
                           sizeOf(TAccessInfo12) - sizeOf(TAccessInfo12Items) +
                           ai12^.itemCount * sizeOf(TAccessInfo12Item)          );

    end;
    result := c1 = 0;
    if not result then SetLastError(c1);
  finally FreeMem(ai12) end;
end;

function TISecurityObject.GetMaxInterface : IBasic;
begin
  result := ISecurityObject(self);
end;

function PrinterSecurity(nameOrUnc: wideString) : ISecurityObject;
begin
  result := TISecurityObject.Create(true, 0, '', sePrinter, nameOrUnc, 0);
end;

function PrinterSecurity(handle: cardinal) : ISecurityObject;
begin
  result := TISecurityObject.Create(true, 0, '', sePrinter, '', handle);
end;

function FileSecurity(pathOrUnc: wideString) : ISecurityObject;
begin
  result := TISecurityObject.Create(true, 0, '', seFile, pathOrUnc, 0);
end;

function FileSecurity(handle: cardinal) : ISecurityObject;
begin
  result := TISecurityObject.Create(true, 0, '', seFile, '', handle);
end;

function ShareSecurity(nameOrUnc: wideString) : ISecurityObject;
begin
  result := TISecurityObject.Create(true, 0, '', seShare, nameOrUnc, 0);
end;

function RegistrySecurity(pathOrUnc: wideString) : ISecurityObject;

  function Find(str, correctPath: string; faultyPaths: array of string) : boolean;
  var i1 : integer;
  begin
    result := IsTextEqual(str, correctPath);
    if not result then
      for i1 := 0 to high(faultyPaths) do
        if IsTextEqual(str, faultyPaths[i1]) then begin
          result := true;
          break;
        end;
    if result then
      pathOrUnc := correctPath + pathOrUnc;
  end;

var i1 : integer;
    s1 : string;
begin
  if PosTextIs1('HKEY_', pathOrUnc) then
    Delete(pathOrUnc, 1, 5);
  i1 := PosStr('\', pathOrUnc);
  if i1 = 0 then
    i1 := maxInt;
  s1 := Copy(pathOrUnc, 1, i1 - 1);
  Delete(pathOrUnc, 1, i1 - 1);
  if (not Find(s1, 'MACHINE',      ['HKLM', 'LOCAL_MACHINE' ])) and
     (not Find(s1, 'USERS',        ['HKU'                   ])) and
     (not Find(s1, 'CURRENT_USER', ['HKCU', 'USER'          ])) and
     (not Find(s1, 'CONFIG',       ['HKCC', 'CURRENT_CONFIG'])) and
     (not Find(s1, 'CLASSES_ROOT', ['HKCR', 'CLASSES'       ])) then
    pathOrUnc := UpStr(s1) + pathOrUnc;
  result := TISecurityObject.Create(true, 0, '', seRegistry, pathOrUnc, 0);
end;

function RegistrySecurity(key: HKEY) : ISecurityObject;
begin
  result := TISecurityObject.Create(true, 0, '', seRegistry, '', key);
end;

function ServiceSecurity(nameOrUnc: wideString) : ISecurityObject;
begin
  result := TISecurityObject.Create(true, 0, '', seService, nameOrUnc, 0);
end;

function ServiceSecurity(handle: cardinal) : ISecurityObject;
begin
  result := TISecurityObject.Create(true, 0, '', seService, '', handle);
end;

function WindowObjectSecurity(nameOrUnc: wideString) : ISecurityObject;
begin
  result := TISecurityObject.Create(true, 0, '', seWindowObject, nameOrUnc, 0);
end;

function WindowObjectSecurity(handle: cardinal) : ISecurityObject;
begin
  result := TISecurityObject.Create(true, 0, '', seWindowObject, '', handle);
end;

function KernelObjectSecurity(handle: cardinal) : ISecurityObject;
begin
  result := TISecurityObject.Create(true, 0, '', seKernelObject, '', handle);
end;

// ***************************************************************

const LM20_NNLEN = 12;  // max LanManager 2.0 netname length
      SHPWLEN    = 08;  // max share password length

type
  // Windows' internal types for shares  (winNT)
  TShareInfo1 = packed record
    netname     : PWideChar;
    type_       : cardinal;
    remark      : PWideChar;
  end;
  TShareInfo2 = packed record
    netname     : PWideChar;
    type_       : cardinal;
    remark      : PWideChar;
    permissions : cardinal;
    maxUses     : cardinal;
    currentUses : cardinal;
    path        : PWideChar;
    password    : PWideChar;   // share-level
  end;
  TPShareInfo2 = ^TShareInfo2;

  // Windows' internal types for shares  (win9x)
  TShareInfo50 = packed record
    netname     : array [0..LM20_NNLEN] of char;
    type_       : TShareType;
    flags       : word;
    remark      : pchar;
    path        : pchar;
    rw_password : array [0..SHPWLEN] of char;  // read-write share-level
    ro_password : array [0..SHPWLEN] of char;  // read-only  share-level
  end;
  TPShareInfo50 = ^TShareInfo50;

  // forward
  TIShares = class;

  // implements IShare
  TIShare = class (TIBasic, IShare)
  public
    FLevel          : cardinal;
    FServerName     : string;
    FNetName        : string;
    FPath           : string;
    FRemark         : string;
    FSi50           : TPShareInfo50;
    FServerNameW    : wideString;
    FNetNameW       : wideString;
    FPathW          : wideString;
    FRemarkW        : wideString;
    FPasswordW      : wideString;
    FSi2            : TPShareInfo2;
    FDirty          : boolean;
    FVeryDirty      : boolean;
    FAcl            : IAcl;
    FSecurityObject : ISecurityObject;
    FShares         : TIShares;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: string;
                        serverName, netName: string; level: cardinal;
                        si50: TPShareInfo50; si2: TPShareInfo2; copySi50, copySi2: boolean;
                        shares: TIShares);
    destructor Destroy; override;

    function GetType : TShareType;

    function  GetNetName : string;
    procedure SetNetName (netName: string);

    function  GetPath : string;
    procedure SetPath (path: string);

    function  GetRemark : string;
    procedure SetRemark (remark: string);

    function  GetAccess : TShareAccessSet;
    procedure SetAccess (access: TShareAccessSet);

    function  GetPassword : string;
    procedure SetPassword (password: string);

    function  GetReadWritePassword : string;
    procedure SetReadWritePassword (password: string);

    function  GetPersist : boolean;
    procedure SetPersist (persist: boolean);

    function  GetSystem : boolean;
    procedure SetSystem (system: boolean);

    function GetCurrentUses : integer;

    function  GetMaxUses : integer;
    procedure SetMaxUses (maxUses: integer);

    function GetServerName : string;

    function Refresh : boolean;

    function IsDirty : boolean;
    function Flush   : boolean;

    function Delete : boolean;

    function  GetAcl : IAcl;
    procedure SetAcl (const acl: IAcl);

    function SecurityObject : ISecurityObject;

    function GetMaxInterface : IBasic; override;
  end;

  // implements IShares
  TIShares = class (TICustomBasicList, IShares)
  public
    FServerName : string;
    FPath       : string;
    FTypes      : TShareTypeSet;

    constructor Create (valid: boolean; lastErrorNo: cardinal; lastErrorStr: string;
                        refresh: boolean; serverName, path: string; types: TShareTypeSet);

    function GetTypes : TShareTypeSet;

    function GetPath : string;

    function GetServerName : string;

    function Delete : boolean;

    function GetItem (index: integer) : IShare;

    // must be overridden to delete the FShares variable of the deleted item
    function DeleteItem (index: integer) : boolean; override;

    function RefreshItems : boolean;

    function GetMaxInterface : IBasic; override;
  end;

const
  // the share is restored on system startup  (only win9x)
  SHI50F_PERSIST = $0100;
  // the share is not normally visible  (only win9x)
  SHI50F_SYSTEM  = $0200;
  // error constants for NetShareGetInfo9x
  NERR_BASE       = 2100;
  NERR_BUFTOSMALL = NERR_BASE + 23;

var
  // NetShare functions for win9x (from svrapi.h)
  NetShareEnum9x    : function (servername       : pchar;
                                level            : smallInt;
                                buf              : pointer;
                                bufSize          : word;
                                var entriesRead  : word;
                                var totalEntries : word    ) : cardinal; stdcall = nil;
  NetShareAdd9x     : function (servername       : pchar;
                                level            : smallInt;
                                var buf          : TShareInfo50;
                                bufSize          : word    ) : cardinal; stdcall = nil;
  NetShareDel9x     : function (servername       : pchar;
                                netname          : pchar;
                                reserved         : word    ) : cardinal; stdcall = nil;
  NetShareGetInfo9x : function (servername       : pchar;
                                netname          : pchar;
                                level            : smallInt;
                                var buf          : TShareInfo50;
                                bufSize          : word;
                                var totalEntries : word    ) : cardinal; stdcall = nil;
  NetShareSetInfo9x : function (servername       : pchar;
                                netname          : pchar;
                                level            : smallInt;
                                var buf          : TShareInfo50;
                                bufSize          : word;
                                parmNum          : smallInt) : cardinal; stdcall = nil;

  // NetShare functions for winNT
  NetShareEnumNt    : function (servername       : PWideChar;
                                level            : cardinal;
                                var pBuf         : pointer;
                                prefmaxlen       : cardinal;
                                var entriesRead  : cardinal;
                                var totalEntries : cardinal;
                                resumeHandle     : TPCardinal  ) : cardinal; stdcall = nil;
  NetShareAddNt     : function (servername       : PWideChar;
                                level            : cardinal;
                                var buf          : TShareInfo2;
                                parm_err         : TPCardinal  ) : cardinal; stdcall = nil;
  NetShareDelNt     : function (servername       : PWideChar;
                                netname          : PWideChar;
                                reserved         : cardinal    ) : cardinal; stdcall = nil;
  NetShareGetInfoNt : function (servername       : PWideChar;
                                netname          : PWideChar;
                                level            : cardinal;
                                var pBuf         : TPShareInfo2) : cardinal; stdcall = nil;
  NetShareSetInfoNt : function (servername       : PWideChar;
                                netname          : PWideChar;
                                level            : cardinal;
                                var buf          : TShareInfo2;
                                parmErr          : TPCardinal  ) : cardinal; stdcall = nil;
  NetApiBufferFree  : function (buf              : pointer     ) : cardinal; stdcall = nil;

  // dll handle for NetShare dlls
  svrApiDll   : cardinal = 0;
  netApi32Dll : cardinal = 0;

procedure InitNetShareFunctions;
begin
  if OS.win9x then begin
    if svrApiDll = 0 then begin
      svrApiDll := LoadLibrary('svrapi.dll');
      NetShareEnum9x    := GetProcAddress(svrApiDll, 'NetShareEnum'   );
      NetShareAdd9x     := GetProcAddress(svrApiDll, 'NetShareAdd'    );
      NetShareDel9x     := GetProcAddress(svrApiDll, 'NetShareDel'    );
      NetShareGetInfo9x := GetProcAddress(svrApiDll, 'NetShareGetInfo');
      NetShareSetInfo9x := GetProcAddress(svrApiDll, 'NetShareSetInfo');
    end;
  end else
    if netApi32Dll = 0 then begin
      netApi32Dll := LoadLibrary('netapi32.dll');
      NetShareEnumNt    := GetProcAddress(netApi32Dll, 'NetShareEnum'    );
      NetShareGetInfoNt := GetProcAddress(netApi32Dll, 'NetShareGetInfo' );
      NetShareSetInfoNt := GetProcAddress(netApi32Dll, 'NetShareSetInfo' );
      NetShareAddNt     := GetProcAddress(netApi32Dll, 'NetShareAdd'     );
      NetShareDelNt     := GetProcAddress(netApi32Dll, 'NetShareDel'     );
      NetApiBufferFree  := GetProcAddress(netApi32Dll, 'NetApiBufferFree');
    end;
end;

constructor TIShare.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: string;
                           serverName, netName: string; level: cardinal;
                           si50: TPShareInfo50; si2: TPShareInfo2; copySi50, copySi2: boolean;
                           shares: TIShares);
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    InitNetShareFunctions;
    FServerName := serverName;
    FLevel := level;
    FShares := shares;
    if OS.winNT then begin
      FServerNameW := serverName;
      if si2 <> nil then begin
        if copySi2 then begin
          FSi2 := AllocMem(sizeOf(TShareInfo2));
          if FLevel = 2 then Move(si2^, FSi2^, sizeOf(TShareInfo2))
          else               Move(si2^, FSi2^, sizeOf(TShareInfo1));
        end else FSi2 := si2;
        FNetNameW  := FSi2^.netname;  FSi2^.netname  := PWideChar(FNetNameW );
        FRemarkW   := FSi2^.remark;   FSi2^.remark   := PWideChar(FRemarkW  );
        FPasswordW := FSi2^.password; FSi2^.password := PWideChar(FPasswordW);
        FPathW     := FSi2^.path;     FSi2^.path     := PWideChar(FPathW    );
      end else begin
        ClearShareName(netName);
        FNetNameW := netName;
        FValid := Refresh;
      end;
    end else
      if si50 <> nil then begin
        if copySi50 then begin
          GetMem(FSi50, sizeOf(TShareInfo50));
          Move(si50^, FSi50^, sizeOf(TShareInfo50));
        end else FSi50 := si50;
        FRemark := FSi50^.remark; FSi50^.remark := pchar(FRemark);
        FPath   := FSi50^.path;   FSi50^.path   := pchar(FPath  );
      end else begin
        ClearShareName(netName);
        FNetName := netName;
        FValid := Refresh;
      end;
  end;
end;

destructor TIShare.Destroy;
begin
  if FDirty then Flush;
  if FSi50 <> nil then FreeMem(FSi50);
  if FSi2  <> nil then FreeMem(FSi2 );
  inherited;
end;

function TIShare.GetType : TShareType;
begin
  if CheckValid then begin
    if OS.win9x then result := FSi50^.type_
    else             result := TShareType(FSi2^.type_);
  end else result := stDisk;
end;

function TIShare.GetServerName : string;
begin
  result := FServerName;
end;

function TIShare.GetNetName : string;
begin
  if CheckValid then begin
    if OS.win9x then result := FSi50^.netname
    else             result := FNetNameW;
  end else result := '';
end;

procedure TIShare.SetNetName(netName: string);
begin
  if CheckValid then begin
    ClearShareName(netName);
    if not IsTextEqual(netName, GetNetName) then begin
      FDirty     := true;
      FVeryDirty := true;
      if OS.win9x then begin
        StrPLCopy(FSi50^.netname, netName, LM20_NNLEN);
      end else begin
        FNetNameW := netName;
        FSi2^.netname := PWideChar(FNetNameW);
      end;
    end;
  end;
end;

function TIShare.GetPath : string;
begin
  if CheckValid then begin
    if OS.win9x then result := FPath
    else             result := FPathW;
  end else result := '';
end;

procedure TIShare.SetPath(path: string);
begin
  if CheckValid then
    if not IsTextEqual(path, GetPath) then begin
      FDirty     := true;
      FVeryDirty := true;
      if OS.win9x then begin
        FPath := path;
        FSi50^.path := pchar(FPath);
      end else begin
        FPathW := path;
        FSi2^.path := PWideChar(FPathW);
      end;
    end;
end;

function TIShare.GetRemark : string;
begin
  if CheckValid then begin
    if OS.win9x then result := FRemark
    else             result := FRemarkW;
  end else result := '';
end;

procedure TIShare.SetRemark(remark: string);
begin
  if CheckValid then
    if remark <> GetRemark then begin
      FDirty := true;
      if OS.win9x then begin
        FRemark := remark;
        FSi50^.remark := pchar(FRemark);
      end else begin
        FRemarkW := remark;
        FSi2^.remark := PWideChar(FRemarkW);
      end;
    end;
end;

function TIShare.GetAccess : TShareAccessSet;
begin
  if CheckValid then begin
    if OS.win9x then result := TShareAccessSet(byte(FSi50^.flags       and $FF))
    else             result := TShareAccessSet(byte(FSi2 ^.permissions and $FF));
  end else result := [];
end;

procedure TIShare.SetAccess(access: TShareAccessSet);
begin
  if CheckValid then
    if access <> GetAccess then begin
      FDirty := true;
      if OS.win9x then FSi50^.flags       := (FSi50^.flags       and $FF00    ) + byte(access)
      else             FSi2 ^.permissions := (FSi2 ^.permissions and $FFFFFF00) + byte(access);
    end;
end;

function TIShare.GetPassword : string;
begin
  if CheckValid then begin
    if OS.win9x then result := FSi50^.ro_password
    else             result := FPasswordW;
  end else result := '';
end;

procedure TIShare.SetPassword(password: string);
begin
  if CheckValid then
    if password <> GetPassword then begin
      FDirty := true;
      if OS.winNT then begin
        FPasswordW := password;
        FSi2^.password := PWideChar(FPasswordW);
      end else StrPLCopy(FSi50^.ro_password, password, SHPWLEN);
    end;
end;

function TIShare.GetReadWritePassword : string;
begin
  result := '';
  if CheckValid then
    if OS.win9x then result := FSi50^.rw_password
    else             SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

procedure TIShare.SetReadWritePassword(password: string);
begin
  if CheckValid then
    if OS.win9x then begin
      if password <> FSi50^.rw_password then begin
        FDirty := true;
        StrPLCopy(FSi50^.rw_password, password, SHPWLEN);
      end;
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

function TIShare.GetPersist : boolean;
begin
  result := CheckValid and (OS.winNT or (FSi50^.flags and SHI50F_PERSIST <> 0));
end;

procedure TIShare.SetPersist(persist: boolean);
begin
  if CheckValid then
    if persist <> GetPersist then
      if OS.win9x then begin
        FDirty := true;
        if persist then FSi50^.flags := FSi50^.flags or       SHI50F_PERSIST
        else            FSi50^.flags := FSi50^.flags and (not SHI50F_PERSIST);
      end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

function TIShare.GetSystem : boolean;
begin
  result := CheckValid and OS.win9x and (FSi50^.flags and SHI50F_SYSTEM <> 0);
end;

procedure TIShare.SetSystem(system: boolean);
begin
  if CheckValid then
    if system <> GetSystem then
      if OS.win9x then begin
        if system then FSi50^.flags := FSi50^.flags or       SHI50F_SYSTEM
        else           FSi50^.flags := FSi50^.flags and (not SHI50F_SYSTEM);
      end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

function TIShare.GetCurrentUses : integer;
begin
  result := 0;
  if CheckValid then
    if OS.winNT then begin
      if FLevel = 2 then begin
        Refresh;
        result := integer(FSi2^.currentUses);
      end else SetLastError(ERROR_ACCESS_DENIED);
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

function TIShare.GetMaxUses : integer;
begin
  result := -1;
  if CheckValid then
    if OS.winNT then begin
      if FLevel = 2 then begin
        result := integer(FSi2^.maxUses);
      end else SetLastError(ERROR_ACCESS_DENIED);
    end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

procedure TIShare.SetMaxUses(maxUses: integer);
begin
  if CheckValid then
    if maxUses <> GetMaxUses then
      if OS.winNT then begin
        if FLevel = 2 then begin
          FDirty := true;
          FSi2^.maxUses := cardinal(maxUses);
        end else SetLastError(ERROR_ACCESS_DENIED);
      end else SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
end;

function TIShare.Refresh : boolean;
var psi50 : TPShareInfo50;
    psi2  : TPShareInfo2;
    nn    : pchar;
    w1    : word;
    c1    : cardinal;
    level : cardinal;
begin
  result := false;
  if CheckValid then begin
    if FDirty then Flush;
    if OS.win9x then begin
      if FSi50 = nil then nn := pchar(FNetName)
      else                nn := FSi50^.netname;
      psi50 := nil;
      c1 := NetShareGetInfo9x(pchar(FServerName), nn, 50, psi50^, 0, w1);
      if c1 = NERR_BUFTOSMALL then begin
        psi50 := AllocMem(w1);
        try
          c1 := NetShareGetInfo9x(pchar(FServerName), nn, 50, psi50^, w1, w1);
          if c1 = 0 then begin
            result := true;
            if FSi50 <> nil then FreeMem(FSi50);
            FSi50 := psi50;
            psi50 := nil;
            FRemark := FSi50^.remark; FSi50^.remark := pchar(FRemark);
            FPath   := FSi50^.path;   FSi50^.path   := pchar(FPath  );
          end else SetLastError(c1);
        finally
          if psi50 <> nil then FreeMem(psi50);
        end;
      end else SetLastError(c1);
    end else begin
      psi2 := nil; level := 2;
      c1 := NetShareGetInfoNt(PWideChar(FServerNameW), PWideChar(FNetNameW), 2, psi2);
      if (c1 = ERROR_ACCESS_DENIED) and (FLevel = 1) then begin
        level := 1;
        c1 := NetShareGetInfoNt(PWideChar(FServerNameW), PWideChar(FNetNameW), 1, psi2);
      end;
      result := c1 = 0;
      if result then begin
        try
          FLevel := level;
          if FSi2 <> nil then FreeMem(FSi2);
          FSi2 := AllocMem(sizeOf(TShareInfo2));
          if level = 2 then Move(psi2^, FSi2^, sizeOf(TShareInfo2))
          else              Move(psi2^, FSi2^, sizeOf(TShareInfo1));
          FNetNameW  := FSi2^.netname;  FSi2^.netname  := PWideChar(FNetNameW );
          FRemarkW   := FSi2^.remark;   FSi2^.remark   := PWideChar(FRemarkW  );
          FPasswordW := FSi2^.password; FSi2^.password := PWideChar(FPasswordW);
          FPathW     := FSi2^.path;     FSi2^.path     := PWideChar(FPathW    );
        finally NetApiBufferFree(psi2) end;
      end else SetLastError(c1);
    end;
  end;
end;

function TIShare.IsDirty : boolean;
begin
  result := FDirty;
end;

function TIShare.Flush : boolean;
var c1    : cardinal;
    hk    : HKEY;
    acl1  : IAcl;
    arrCh : array [0..MAX_PATH] of char;
begin
  result := false;
  if CheckValid and FDirty then begin
    if FVeryDirty then begin
      acl1 := GetAcl;
      result := Delete;
      if result then begin
        FValid := true;
        if OS.win9x then c1 := NetShareAdd9x(pchar(FServerName), 50, FSi50^, sizeOf(TShareInfo50))
        else             c1 := NetShareAddNt(PWideChar(FServerNameW), FLevel, FSi2^, nil);
        result := c1 = 0;
        if result then begin
          SetAcl(acl1);
          c1 := MAX_PATH;
          GetComputerName(arrCh, c1);
          if GetFileAttributes(pchar('\\' + arrCh + '\' + GetNetName)) = maxCard then
            GetFileAttributes(pchar('\\' + arrCh + '\' + GetNetName));
        end else SetLastError(c1);
      end;
      FVeryDirty := false;
    end else result := true;
    if result then begin
      if OS.win9x then
           c1 := NetShareSetInfo9x(pchar(FServerName), FSi50^.netname, 50, FSi50^, sizeOf(TShareInfo50), 0)
      else c1 := NetShareSetInfoNt(PWideChar(FServerNameW), PWideChar(FNetNameW), FLevel, FSi2^, nil);
      result := c1 = 0;
      if result then begin
        if OS.win9x and (FSi50^.flags and SHI50F_PERSIST <> 0) then
          if RegOpenKey(HKEY_LOCAL_MACHINE,
                        pchar('Software\Microsoft\Windows\CurrentVersion\Network\LanMan\' +
                              FSi50^.netname),
                        hk) = ERROR_SUCCESS then
            try
              RegSetValueEx(hk, 'Path', 0, REG_SZ, pchar(FPath), length(FPath));
            finally RegCloseKey(hk) end;
        FDirty := false;
        Refresh;
        FSuccess := true;
      end else SetLastError(c1);
    end;
  end;
  FDirty := false;
end;

function NtSharePrinter(printer, netName: string; var lastError: cardinal) : boolean;
const adminPrinter : TPrinterDefaults = (pDatatype     : nil;
                                         pDevMode      : nil;
                                         DesiredAccess : PRINTER_ALL_ACCESS);
var c1   : cardinal;
    buf  : PPrinterInfo2;
    size : cardinal;
    i1   : integer;
begin
  result := false;
  i1 := Pos(',', printer);
  if i1 > 0 then Delete(printer, i1, maxInt);
  if OpenPrinter(pchar(printer), c1, @adminPrinter) then begin
    try
      size := 0;
      GetPrinter(c1, 2, nil, 0, @size);
      buf := AllocMem(size);
      try
        if GetPrinter(c1, 2, buf, size, @size) then begin
          if netName <> '' then begin
            buf^.pShareName := pchar(netName);
            buf^.Attributes := buf^.Attributes or PRINTER_ATTRIBUTE_SHARED;
          end else buf^.Attributes := buf^.Attributes and (not PRINTER_ATTRIBUTE_SHARED);
          buf^.pSecurityDescriptor := nil;
          if SetPrinter(c1, 2, buf, 0) then result := true
          else                              lastError := GetLastError;
        end else lastError := GetLastError;
      finally FreeMem(buf, size) end;
    finally ClosePrinter(c1) end;
  end else lastError := GetLastError;
end;

function TIShare.Delete : boolean;
var c1   : cardinal;
    i1   : integer;
    acl1 : IAcl;
begin
  if CheckValid then begin
    if OS.winNT then begin
      if TShareType(FSi2^.type_) <> stPrinter then begin
        c1 := NetShareDelNt(PWideChar(FServerNameW), PWideChar(FNetNameW), 0);
        result := c1 = 0;
      end else result := NtSharePrinter(FPathW, '', c1);
    end else begin
      acl1 := NewAcl;
      acl1.Allocate;
      SecurityObject.SetDAcl(acl1);
      c1 := NetShareDel9x(pchar(FServerName), FSi50^.netname, 0);
      result := c1 = 0;
    end;
    if result then begin
      FValid := false;
      if FShares <> nil then begin
        i1 := GetIndex(FShares);
        if i1 <> -1 then
          FShares.DeleteItem(i1);
        FShares := nil;
      end;
    end else SetLastError(c1);
  end else result := false;
end;

function TIShare.GetAcl : IAcl;
begin
  if CheckValid then
       result := SecurityObject.DAcl
  else result := TIAcl.Create(false, CErrorNo_AmInvalid, CErrorStr_AmInvalid, nil, nil);
end;

procedure TIShare.SetAcl(const acl: IAcl);
var iso : ISecurityObject;
begin
  if CheckValid then begin
    iso := SecurityObject;
    iso.DAcl := acl;
    if not iso.Success then
      SetLastError(iso.LastErrorNo, iso.LastErrorStr);
  end;
end;

function TIShare.SecurityObject : ISecurityObject;
begin
  if FSecurityObject = nil then begin
    CheckValid;
    if OS.win9x or (GetType = stPrinter) then begin
      if GetType = stDisk then
        FSecurityObject := TISecurityObject.Create(FValid, CErrorNo_AmInvalid, CErrorStr_AmInvalid,
                                                   seFile, GetPath, 0)
      else if GetType = stPrinter then
        FSecurityObject := TISecurityObject.Create(FValid, CErrorNo_AmInvalid, CErrorStr_AmInvalid,
                                                   sePrinter, GetPath, 0)
      else
        FSecurityObject := TISecurityObject.Create(false, ERROR_CALL_NOT_IMPLEMENTED, '',
                                                   seUnknown, '', 0);
    end else
      FSecurityObject := TISecurityObject.Create(FValid, CErrorNo_AmInvalid, CErrorStr_AmInvalid,
                                                 seShare, GetNetName, 0);
  end;
  result := FSecurityObject;
end;

function TIShare.GetMaxInterface : IBasic;
begin
  result := IShare(self);
end;

constructor TIShares.Create(valid: boolean; lastErrorNo: cardinal; lastErrorStr: string;
                            refresh: boolean; serverName, path: string; types: TShareTypeSet);
begin
  inherited Create(valid, lastErrorNo, lastErrorStr);
  if FValid then begin
    InitNetShareFunctions;
    FServerName := serverName;
    FPath := path;
    FTypes := types;
    if (stDisk in FTypes) and ((Pos('\', FPath) > 0) or (Pos(':', FPath) > 0)) then begin
      FPath := UpStr(FPath);
      if      (Length(FPath) = 2) and (FPath[2] = ':') then FPath := FPath + '\'
      else if FPath[Length(FPath)] = '\'               then DeleteR(FPath, 1);
    end;
    if refresh then RefreshItems;
  end;
end;

function TIShares.Delete : boolean;
var i1 : integer;
begin
  if CheckValid then begin
    for i1 := FCount - 1 downto 0 do
      IShare(FItems[i1]).Delete;
    result := FCount = 0;
  end else result := false;
end;

function TIShares.GetServerName : string;
begin
  result := FServerName;
end;

function TIShares.GetPath : string;
begin
  result := FPath;
end;

function TIShares.GetTypes : TShareTypeSet;
begin
  result := FTypes;
end;

function TIShares.GetItem(index: integer) : IShare;
begin
  if (index < 0) or (index >= FCount) then
       result := TIShare.Create(false, CErrorNo_IndexOutOfRange, CErrorStr_IndexOutOfRange, '', '', 0, nil, nil, false, false, nil)
  else result := IShare(FItems[index]);
end;

function TIShares.DeleteItem(index: integer) : boolean;
begin
  result := false;
  if CheckValid then
    if (index >= 0) and (index < FCount) then begin
      result := true;
      TIShare(FItems[index].SelfAsTObject).FShares := nil;
      inherited DeleteItem(index);
    end else SetLastError(CErrorNo_InvalidIndex, CErrorStr_InvalidIndex);
end;

function TIShares.RefreshItems : boolean;

  procedure RefreshItem9x(psi: TPShareInfo50);
  var i1  : integer;
      is1 : TIShare;
  begin
    if (psi^.type_ in FTypes) and ((FPath = '*') or IsTextEqual(FPath, psi^.path)) then begin
      for i1 := 0 to FCount - 1 do
        with TIShare(FItems[i1].SelfAsTObject) do
          if (psi^.type_ = FSI50.type_) and IsTextEqual(psi^.netname, FSI50^.netname) then begin
            if (not IsTextEqual(FPath,   psi^.path  )) or
               (not IsTextEqual(FRemark, psi^.remark)) or
               (FSi50^.flags <> psi^.flags) or
               (string(FSi50^.ro_password) <> string(psi^.ro_password)) or
               (string(FSi50^.rw_password) <> string(psi^.rw_password)) then begin
              Change(FItems[i1], true, lctChanged, i1, i1);
              FPath   := psi^.path;   FSi50^.path   := pchar(FPath  );
              FRemark := psi^.remark; FSi50^.remark := pchar(FRemark);
              FSi50^.flags := psi^.flags;
              FSi50^.ro_password := psi^.ro_password;
              FSi50^.rw_password := psi^.rw_password;
              self.FItemInfos[i1].LastChangeType := lctChanged;
              Change(FItems[i1], false, lctChanged, i1, SortItem(i1));
            end else
              self.FItemInfos[i1].LastChangeType := lctUnChanged;
            exit;
          end;
      is1 := TIShare.Create(true, 0, '', FServerName, '', 50, psi, nil, true, false, self);
      if AddItem(IShare(is1)) = -1 then
        is1.Destroy;
    end;
  end;

  procedure RefreshItemNT(psi: TPShareInfo2; level: cardinal);
  var i1  : integer;
      is1 : TIShare;
  begin
    if (TShareType(psi^.type_) in FTypes) and
      ((level = 1) or (FPath = '*') or IsTextEqual(FPath, wideString(psi^.path))) then begin
      for i1 := 0 to FCount - 1 do
        with TIShare(FItems[i1].SelfAsTObject) do
          if (psi^.type_ = FSI2.type_) and
             IsTextEqual(wideString(psi^.netname), wideString(FSI2^.netname)) then begin
            if (not IsTextEqual(FRemark, wideString(psi^.remark))) or
               ( (level = 2) and
                 ( (not IsTextEqual(FPathW,     wideString(psi^.path    ))) or
                   (not IsTextEqual(FPasswordW, wideString(psi^.password))) or
                   (FSi2^.permissions <> psi^.permissions) or
                   (FSi2^.currentUses <> psi^.currentUses) or
                   (FSi2^.maxUses     <> psi^.maxUses    )                     ) ) then begin
              Change(FItems[i1], true, lctChanged, i1, i1);
              FRemarkW := wideString(psi^.remark); FSi2^.remark := PWideChar(FRemarkW);
              if level = 2 then begin
                FPathW     := wideString(psi^.path    ); FSi2^.path     := PWideChar(FPathW    );
                FPasswordW := wideString(psi^.password); FSi2^.password := PWideChar(FPasswordW);
                FSi2^.permissions := psi^.permissions;
                FSi2^.currentUses := psi^.currentUses;
                FSi2^.maxUses     := psi^.maxUses;
              end;
              self.FItemInfos[i1].LastChangeType := lctChanged;
              Change(FItems[i1], false, lctChanged, i1, SortItem(i1));
            end else
              self.FItemInfos[i1].LastChangeType := lctUnChanged;
            exit;
          end;
      is1 := TIShare.Create(true, 0, '', FServerName, '', level, nil, psi, false, true, self);
      if AddItem(IShare(is1)) = -1 then
        is1.Destroy;
    end;
  end;

type TAShareInfo50 = array [0..maxInt shr 6 - 1] of TShareInfo50;
     TAShareInfo2  = array [0..maxInt shr 5 - 1] of TShareInfo2;
     TAShareInfo1  = array [0..maxInt shr 4 - 1] of TShareInfo1;
var w1, w2     : word;
    i1         : integer;
    pasi50     : ^TAShareInfo50;
    pasi1      : ^TAShareInfo1;
    pasi2      : ^TAShareInfo2;
    psi2       : TPShareInfo2;
    c1, c2, c3 : cardinal;
    level      : cardinal;
begin
  result := false;
  if CheckValid then begin
    BeginRefresh;
    if OS.win9x then begin
      c1 := NetShareEnum9x(pchar(FServerName), 50, nil, 0, w1, w2);
      if c1 = ERROR_MORE_DATA then begin
        pasi50 := nil;
        try
          i1 := w2;
          repeat
            i1 := i1 * 2;
            ReAllocMem(pasi50, i1 * sizeOf(TShareInfo50));
          until NetShareEnum9x(pchar(FServerName), 50, pasi50,
                               i1 * sizeOf(TShareInfo50), w1, w2) <> ERROR_MORE_DATA;
          for i1 := 0 to integer(w2) - 1 do
            RefreshItem9x(@pasi50^[i1]);
          result := true;
        finally FreeMem(pasi50) end;
      end else SetLastError(c1);
    end else begin
      level := 2;
      c1 := NetShareEnumNt(PWideChar(wideString(FServerName)), 2, pointer(pasi2), cardinal(-1), c2, c3, nil);
      if c1 = ERROR_ACCESS_DENIED then begin
        level := 1;
        c1 := NetShareEnumNt(PWideChar(wideString(FServerName)), 1, pointer(pasi1), cardinal(-1), c2, c3, nil);
      end;
      if c1 = 0 then begin
        for i1 := 0 to integer(c2) - 1 do begin
          if level = 2 then psi2 := @pasi2^[i1] else psi2 := @pasi1^[i1];
          RefreshItemNT(psi2, level);
        end;
        result := true;
      end else SetLastError(c1);
    end;
    EndRefresh;
  end;
end;

function TIShares.GetMaxInterface : IBasic;
begin
  result := IShares(self);
end;

function Share(netName    : string;
               serverName : string = '') : IShare;
var level : cardinal;
begin
  if OS.win9x then level := 50
  else             level := 2;
  result := TIShare.Create(true, 0, '', serverName, netName, level, nil, nil, false, false, nil);
end;

function NewShare(path       : string;
{ only valid in } netName    : string;
{ ************* } remark     : string          = '';
{ share-level   } access     : TShareAccessSet = [low(TShareAccess)..high(TShareAccess)];
{ share-level   } password   : string          = '';
{ share-l. + 9x } rwPassword : string          = '';
{         win9x } persist    : boolean         = true;
{         win9x } system     : boolean         = false;
{         winNT } maxUses    : integer         = -1;
                  serverName : string          = ''   ) : IShare;
var psi50              : TPShareInfo50;
    psi2               : TPShareInfo2;
    ws1, ws2, ws3, ws4 : wideString;
    typ                : TShareType;
    c1                 : cardinal;
    arrCh              : array [0..MAX_PATH] of char;
begin
  InitNetShareFunctions;
  if (Pos('\', path) > 0) or (Pos(':', path) > 0) then begin
    typ := stDisk;
    path := UpStr(path);
    if      (Length(path) = 2) and (path[2]            = ':') then path := path + '\'
    else if (Length(path) > 3) and (path[Length(path)] = '\') then DeleteR(path, 1);
  end else typ := stPrinter;
  ClearShareName(netName);
  if OS.win9x then begin
    psi50 := AllocMem(sizeOf(TShareInfo50));
    try
      StrPLCopy(psi50^.netname, netName, LM20_NNLEN);
      psi50^.type_ := typ;
      psi50^.flags := byte(access);
      if persist then psi50^.flags := psi50^.flags or SHI50F_PERSIST;
      if system  then psi50^.flags := psi50^.flags or SHI50F_SYSTEM;
      psi50^.remark := pchar(remark);
      psi50^.path := pchar(path);
      if   password <> '' then StrPLCopy(psi50^.ro_password,   password, SHPWLEN);
      if rwPassword <> '' then StrPLCopy(psi50^.rw_password, rwPassword, SHPWLEN);
      c1 := NetShareAdd9x(pchar(serverName), 50, psi50^, sizeOf(TShareInfo50));
      result := TIShare.Create(c1 = 0, c1, '', serverName, '', 50, psi50, nil, false, false, nil);
      psi50 := nil;
      if result.IsValid then result.Acl := NewAcl;
    finally
      if psi50 <> nil then FreeMem(psi50);
    end;
  end else begin
    psi2 := AllocMem(sizeOf(TShareInfo2));
    try
      if typ = stPrinter then begin
        if NtSharePrinter(path, netName, c1) then begin
          result := TIShare.Create(true, 0, '', serverName, netName, 2, nil, nil, false, false, nil);
          result.SetRemark  (remark  );
          result.SetAccess  (access  );
          result.SetPassword(password);
          result.SetMaxUses (maxUses );
          result.Flush;
          TIShare(result.SelfAsTObject).FSuccess := true;
        end else
          result := TIShare.Create(false, c1, '', '', '', 0, nil, nil, false, false, nil);
      end else begin
        psi2^.type_ := ord(typ);
        psi2^.permissions := byte(access);
        ws1 := netName;  psi2^.netname  := PWideChar(ws1);
        ws2 := remark;   psi2^.remark   := PWideChar(ws2);
        ws3 := path;     psi2^.path     := PWideChar(ws3);
        ws4 := password; psi2^.password := PWideChar(ws4);
        psi2^.maxUses := cardinal(maxUses);
        c1 := NetShareAddNt(PWideChar(wideString(serverName)), 2, psi2^, nil);
        result := TIShare.Create(c1 = 0, c1, '', serverName, '', 2, nil, psi2, false, false, nil);
        psi2 := nil;
      end;
    finally
      if psi2 <> nil then FreeMem(psi2);
    end;
  end;
  if result.IsValid then begin
    c1 := MAX_PATH;
    GetComputerName(arrCh, c1);
    if GetFileAttributes(pchar('\\' + arrCh + '\' + netName)) = maxCard then
      GetFileAttributes(pchar('\\' + arrCh + '\' + netName));
  end;
end;

function Shares(types      : TShareTypeSet = [low(TShareType)..high(TShareType)];
                path       : string        = '*';
                serverName : string        = '' ) : IShares;
begin
  result := TIShares.Create(true, 0, '', true, serverName, path, types);
end;

// ***************************************************************

end.
