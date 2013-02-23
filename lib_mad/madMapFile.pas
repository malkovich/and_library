// ***************************************************************
//  madMapFile.pas            version:  1.6i  ·  date: 2006-05-21
//  -------------------------------------------------------------
//  map file functions
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2006 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2006-05-21 1.6i (1) GetMyProcName function added
//                 (2) additional security checks for TDS parsing
// 2005-11-12 1.6h (1) fixed td32 debug info bug (introduced by 1.6g)
//                 (2) td32 debug info parsing slightly improved
// 2005-06-19 1.6g (1) support for ".itext" section added
//                 (2) import for external *.jdbg files added
//                 (3) TD32 debug infos: line numbers are sorted now
//                 (4) support for madExcept future minimal debug info format
//                 (5) outdated "tds" files are ignored now
//                 (6) support for seperate initialization section added
// 2005-01-21 1.6f (1) map file parsing speed slightly improved (up to 25%)
//                 (2) TD32 debug parsing speed greatly improved (up to 7000%)
// 2004-11-15 1.6e (1) bugfix for BCB TD32 debug info parsing
//                 (2) version variable didn't work properly in BCB, anymore
// 2004-11-08 1.6d several improvements in TD32 debug info interpretation
// 2004-06-26 1.6c line number info helper function exported (for madDisAsm)
// 2004-04-09 1.6b (1) "try..except" added to "FindMapFile" (just to be sure)
//                 (2) BCB: two little bugs in td32 debug info parsing
// 2004-03-07 1.6a "Export" optionally supresses publics without line numbers
// 2003-11-10 1.6  (1) full td32 support for Delphi + BCB
//                 (2) incorrect line numbers are ignored now
//                 (3) GetMapFileInfos function added
// 2003-06-09 1.5a (1) tricky linking gives madDisAsm access to map file infos
//                 (2) EntryPoint is saved again -> Armadillo support
// 2002-10-25 1.5  (1) removed lots of stuff, which was not needed for madExcept
//                 (2) IMapFile -> TMapFile reduces footprint
//                 (3) td32 debug info support added, but disabled by default
// 2002-10-21 1.4i (1) map file cache didn't work for external map/mad files
//                 (2) crypt password calculation crashed on D6 < SP2
// 2002-10-09 1.4h (1) map file stream gets zipped now
//                 (2) map file stream gets encrypted now (blowfish)
//                 (3) the map file stream is now loaded by using resource APIs
//                 (4) external "mad" file support (mad = compressed map file)
// 2001-06-06 1.4g the most important find functions got a *lot* faster
// 2001-05-01 1.4f little bug in IMapFile.AddrToStr fixed
// 2001-04-30 1.4e minor changes in order to get rid of SysUtils and Classes
// 2000-11-23 1.4d little bug in FindMapFile fixed

unit madMapFile;

{$I mad.inc}

interface

uses Windows, madTypes;

// ***************************************************************

{$define td32}  // support td32 debug infos? enabled by default
{$define jdbg}  // support jdbg debug files? enabled by default

// ***************************************************************

type
  //Detailed map of segments
  //
  // 0001:00000000 00005C34 C=CODE     S=.text    G=(none)   M=System   ACBP=A9
  // 0001:00005C34 00000174 C=CODE     S=.text    G=(none)   M=SysInit  ACBP=A9
  // 0001:00005DA8 00000512 C=CODE     S=.text    G=(none)   M=mm       ACBP=A9
  // [...]
  TMfSegment = record
    IsValid      : boolean;
    Code         : boolean;
    StartAddress : pointer;
    Length       : dword;
    Unit_        : string;
    LineNumbers  : boolean;
  end;

  //  Address         Publics by Name          |    Address         Publics by Value
  //                                           |
  // 0001:00002328       @AfterConstruction    |   0001:00000228       CloseHandle
  // 0001:00023690       AbortProc             |   0001:00000230       CreateFileA
  // 0001:00116EE4       AbortVideo            |   0001:00000238       GetFileType
  // 0001:0002FD1C       AbsMin                |   0001:00000240       GetSystemTime
  // [...]                                     |   [...]
  TMfPublic = record
    IsValid : boolean;
    Code    : boolean;
    Name    : string;
    Address : pointer;
  end;

  //Line numbers for Consts(Consts.pas) segment .text
  //
  //   351 0001:0000E10C   351 0001:0000E113
  //
  //Line numbers for types(types.pas) segment .text
  //
  //   169 0001:0007027C   170 0001:00070282   171 0001:0007028A   172 0001:0007029B
  //   173 0001:000702A6   174 0001:000702AA   175 0001:000702CD   174 0001:000702EC
  //   177 0001:000702EF   181 0001:00070306   182 0001:0007030D   184 0001:0007034D
  //   [...]
  TMfLine = record
    Line    : integer;
    Address : pointer;
  end;

  // IMapFile reads/parses a complete map file
  TMapFile = class
  private
    FCodeOnly       : boolean;
    FSItems         : array of TMfSegment;
    FSCount         : integer;
    FSCapacity      : integer;
    FPItems         : array of TMfPublic;
    FPCount         : integer;
    FPCapacity      : integer;
    FLItems         : array of TMfLine;
    FLCount         : integer;
    FLCapacity      : integer;
    FEntryPoint     : pointer;
    FBaseOfData     : dword;
    FTopOfData      : dword;
    FHInstance      : dword;
    FValid          : boolean;
    FBaseOfCode     : dword;
    FTopOfCode      : dword;
    FModuleFileName : string;
    FMapFileName    : string;
    FMinDebugInfo   : string;
  public
    constructor Create (valid: boolean; moduleFileName: string; hInstance: dword; codeOnly: boolean);

    // is this map file valid?
    property IsValid : boolean read FValid;

    // does this map file only contain min debug information?
    property MinDebugInfo : string read FMinDebugInfo;

    // find the segment, public and line to which the "address" belongs
    function FindSegment (code: boolean; address: pointer) : TMfSegment; overload;
    function FindSegment (index: integer) : TMfSegment; overload;
    function FindPublic  (address: pointer) : TMfPublic; overload;
    function FindPublic  (code: boolean; unit_, name: string; caseSensitive: boolean = true) : dword; overload;
    function FindPublic  (index: integer) : TMfPublic; overload;
    function FindLine    (address: pointer) : integer; overload;
    function FindLine    (address: pointer; var next: pointer) : integer; overload;

    // return the entry point which was stored in the map file
    property EntryPoint : pointer read FEntryPoint;

    // start/end of the code/data block
    property BaseOfCode : dword read FBaseOfCode;
    property TopOfCode  : dword read FTopOfCode;

    // name of the module
    property ModuleFileName : string read FModuleFileName;

    // name of the map file (= module name, if the map file is attached to the module)
    property MapFileName : string read FMapFileName;

    // exports the map file infos into a compressed and encrypted string stream
    function Export (newFormat, minDebugInfoOnly: boolean; hideUglyItems: boolean = false) : string;
  end;

// find the map file that belongs to the address "addr"
// "addr" can be a code address, a data address or a hInstance value
// (1) looks in which module the address is located
// (2) looks in that module file (exe or dll) if it has an appended map file
// (3) if not, looks in that module file if it has debug infos attached
// (4) if not, looks for a seperate map or mad file
// give in nil or hInstance to get the map file for our own application/library
function FindMapFile (addr: pointer = nil) : TMapFile;

// load and parse the specified seperate *.map or *.mad file
function LoadMapFile (mapFile: string) : TMapFile;

// ***************************************************************

// get all available information for a specific code address
// give in "nil" to get information about "yourself"
function GetMapFileInfos (address : pointer;
                          var moduleName : string;
                          var unitName   : string;
                          var publicName : string;
                          var publicAddr : pointer;
                          var line       : integer) : boolean;

// get information about "yourself" (the code location calling this function)
// Example:
// yourUnit.yourFunction (105)
function GetMyProcName (includeLineNumber : boolean = false) : string;

// ***************************************************************
// internal stuff, please ignore

const CMapFileStreamDescriptor = 'www.madshi.net';
function LoadMapFileEx (mapFile: string; codeOnly: boolean) : TMapFile;

implementation

uses madStrings, madDisAsm, madZip, madCrypt, madTools;

(*var publicFile  : dword = 0;
    segmentFile : dword = 0;

procedure log(var fileHandle: dword; fileName, text: string);
var c1 : dword;
begin
  text := text + #$D#$A;
  if fileHandle = 0 then
    fileHandle := CreateFile(pchar(fileName), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0);
  WriteFile(fileHandle, text[1], Length(text), c1, nil);
end;*)

// ***************************************************************

type
  TMiniStream = class(TObject)
  private
    FMemory   : pointer;
    FSize     : integer;
    FPosition : integer;
    FCapacity : integer;
    procedure SetSize (value: integer);
  public
    destructor Destroy; override;
    function Read  (var   buf; count: integer) : integer;
    function Write (const buf; count: integer) : integer;
    property Position : integer read FPosition write FPosition;
    property Size     : integer read FSize     write SetSize;
    property Memory   : pointer read FMemory;
  end;

destructor TMiniStream.Destroy;
begin
  if FCapacity <> 0 then GlobalFreePtr(FMemory);
  inherited;
end;

procedure TMiniStream.SetSize(value: integer);
const MemoryDelta = $2000;
begin
  FSize := value;
  if FPosition > value then
    FPosition := value;
  if value > 0 then
    value := (value + (MemoryDelta - 1)) and not (MemoryDelta - 1);
  if value <> FCapacity then begin
    if value = 0 then begin
      GlobalFreePtr(FMemory);
      FMemory := nil;
    end else
      if FCapacity = 0 then
           FMemory := GlobalAllocPtr(HeapAllocFlags, value)
      else FMemory := GlobalReallocPtr(FMemory, value, HeapAllocFlags);
    FCapacity := value;
  end;
end;

function TMiniStream.Read(var buf; count: integer) : integer;
begin
  if (FPosition >= 0) and (count >= 0) then begin
    result := FSize - FPosition;
    if result > 0 then begin
      if result > count then result := count;
      Move(pointer(integer(FMemory) + FPosition)^, buf, result);
      inc(FPosition, result);
    end;
  end else
    result := 0;
end;

function TMiniStream.Write(const buf; count: integer) : integer;
var newPos : integer;
begin
  if (FPosition >= 0) and (count > 0) then begin
    newPos := FPosition + count;
    if newPos > FSize then SetSize(newPos);
    Move(buf, pointer(integer(FMemory) + FPosition)^, count);
    FPosition := newPos;
    result := count;
  end else
    result := 0;
end;

// ***************************************************************

function CalcCryptPassword(len: integer) : string;
var s1, s2 : string;
begin
  SetLength(result, 6);
  // character by character, so that the strings is hidden in the code
  result[1] := 'm';
  result[2] := 'a';
  result[3] := 'd';
  result[4] := 's';
  result[5] := 'h';
  result[6] := 'i';
  // original the next 3 lines were 1 line
  // but D6 < SP2 crashes with that <sigh>
  s1 := result + UpStr(IntToHexEx(len, 7));
  s2 := result + IntToStrEx(len, 8);
  result := s1 + s2;
  result := result + result;
  OldEncrypt(result, result);
end;

constructor TMapFile.Create(valid: boolean; moduleFileName: string; hInstance: dword; codeOnly: boolean);
var FSLastAdded : array [boolean] of integer;

  function LoadStrFromFile(file_: string; var str: string) : boolean;
  var c1, c2 : dword;
  begin
    result := false;
    if GetFileAttributes(pchar(file_)) <> dword(-1) then begin
      c1 := CreateFile(pchar(file_), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
                       OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
      if c1 <> INVALID_HANDLE_VALUE then
        try
          SetLength(str, GetFileSize(c1, nil));
          result := (str <> '') and ReadFile(c1, pointer(str)^, Length(str), c2, nil) and (c2 = dword(Length(str)));
        finally CloseHandle(c1) end;
    end;
  end;

  function ReadPackedInteger(st: TMiniStream) : integer;
  var si1 : shortInt;
      b1  : byte absolute si1;
  begin
    st.Read(b1, 1);
    if b1 = $80 then st.Read(result, 4)
    else             result := si1;
  end;

  function ReadPackedString(st: TMiniStream) : string;
  var b1 : byte;
  begin
    st.Read(b1, 1);
    if b1 > st.Size - st.Position then exit;
    SetLength(result, b1);
    st.Read(pointer(result)^, b1);
  end;

  procedure Segment_NewItem(code_, addBase: boolean; startAddress_, length_: dword; unit__: string; td32, sort: boolean);
  var i1 : integer;
  begin
    if FCodeOnly and (not code_) then
      exit;
    if FSCount = FSCapacity then begin
      if FSCapacity < 8 then FSCapacity := 16
      else                   FSCapacity := FSCapacity + FSCapacity div 2;
      SetLength(FSItems, FSCapacity);
    end;
    for i1 := Length(unit__) downto 1 do
      if (unit__[i1] = '.') and
         (Length(unit__) - i1 = 3) and
         ( ((lowChar(unit__[i1 + 1]) = 'd') and (lowChar(unit__[i1 + 2]) = 'p') and (lowChar(unit__[i1 + 3]) = 'r')) or
           ((lowChar(unit__[i1 + 1]) = 'p') and (lowChar(unit__[i1 + 2]) = 'a') and (lowChar(unit__[i1 + 3]) = 's'))    ) then begin
        Delete(unit__, i1, maxInt);
        break;
      end;
    if addBase then
      if code_ then inc(startAddress_, FBaseOfCode)
      else          inc(startAddress_, FBaseOfData);
    {$ifdef td32}
      if td32 then begin
        if FSLastAdded[code_] <> -1 then
          with FSItems[FSLastAdded[code_]] do
            if (Unit_ = unit__) and (dword(StartAddress) <= startAddress_) and
              (startAddress_ <= dword(StartAddress) + Length + 2) then begin
              FSItems[FSLastAdded[code_]].Length := startAddress_ + length_ - dword(StartAddress);
              exit;
            end;
        if (not sort) or (FSCount = 0) then
          i1 := FSCount
        else
          for i1 := FSCount downto 1 do
            if (code_ and (not FSItems[i1 - 1].Code)) or
               ( (code_ = FSItems[i1 - 1].Code) and
                 (startAddress_ < dword(FSItems[i1 - 1].StartAddress)) ) or
               ( (code_ = FSItems[i1 - 1].Code) and
                 (startAddress_ = dword(FSItems[i1 - 1].StartAddress)) and
                 (length_ > FSItems[i1 - 1].Length) ) then
              FSItems[i1] := FSItems[i1 - 1]
            else
              break;
        FSLastAdded[code_] := i1;
      end else
    {$endif}
      i1 := FSCount;
    with FSItems[i1] do begin
      IsValid      := true;
      Code         := code_;
      StartAddress := pointer(startAddress_);
      Length       := length_;
      Unit_        := unit__;
      LineNumbers  := false;
    end;
    inc(FSCount);
  end;

  procedure Segment_Log(fileName: string);
  var i1     : integer;
      c1, c2 : dword;
      s1     : string;
  begin
    c1 := CreateFile(pchar('c:\desktop\' + fileName + '.txt'), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0);
    for i1 := 0 to FSCount - 1 do
      with FSItems[i1] do begin
        s1 := booleanToChar(Code) + ' ' + IntToHexEx(dword(StartAddress), 8) + ' ' +
                                          IntToHexEx(dword(Length      ), 8) + ' ' + Unit_ + #$D#$A;
        WriteFile(c1, pchar(s1)^, system.Length(s1), c2, nil);
      end;
    CloseHandle(c1);
  end;

  function Segment_ParseStr(str: string; pos, len, dataSeg, codeLen: integer) : boolean;
  var seg  : integer;
      i1   : integer;
      addr : dword;
  begin
    result := false;
    try
      if (len > 72) and (str[pos + 4] = ':') and (str[pos + 13] = ' ') and
         (str[pos + 22] = ' ') and (str[pos + 23] = 'C') and (str[pos + 24] = '=') then begin
        for i1 := pos + len - 1 downto pos do
          if str[i1] = ' ' then begin
            len := i1 - pos;
            break;
          end;
        for i1 := pos + len - 1 downto pos do
          if str[i1] <> ' ' then begin
            len := i1 - pos + 1;
            break;
          end;
        for i1 := pos + len downto pos + 1 do
          if str[i1 - 1] in ['=', '\', '|'] then begin
            seg := StrToIntEx(true, pointer(integer(str) - 1 + pos), 4);
            if (seg >= 1) and (seg <= dataSeg) then begin
              addr := dword(StrToIntEx(true, pointer(integer(str) - 1 + pos + 5), 8));
              if (seg > 1) and (seg < dataSeg) then
                inc(addr, codeLen);
              Segment_NewItem(seg < dataSeg, true,
                              addr, StrToIntEx(true, pointer(integer(str) - 1 + pos + 14), 8),
                              Copy(str, i1, pos + len - i1), false, false);
              result := true;
            end;
            break;
          end;
      end;
    except result := false end;
  end;

  function Segment_LoadFromStream(st: TMiniStream) : boolean;
  var i1 : integer;
  begin
    st.Read(FSCount, 4);
    FSCapacity := FSCount;
    SetLength(FSItems, FSCapacity);
    for i1 := 0 to FSCount - 1 do
      with FSItems[i1] do begin
        IsValid := true;
        Code := true;
        st.Read(StartAddress, 4);
        dword(StartAddress) := dword(StartAddress) + FBaseOfCode;
        st.Read(Length, 4);
        if FMinDebugInfo <> '' then begin
          Unit_ := 'segment%' + IntToStrEx(i1);
          st.Read(LineNumbers, 1);
        end else begin
          Unit_ := ReadPackedString(st);
          LineNumbers := false;
        end;
      end;
    result := true;
  end;

  procedure Public_NewItem(code_: boolean; name_: string; address_: dword; td32: boolean);
  var unit_ : string;
      old   : string;
      b1    : boolean;
      mfs   : TMfSegment;
  begin
    if FCodeOnly and (not code_) then
      exit;
    old := name_;
    if (Length(name_) > 5) and (name_[1] = '@') and (name_[2] = '$') and
                               (name_[3] = 'x') and (name_[5] = '$') then
      // this is a type - but we are not interested in types
      exit;
    Unmangle(name_, unit_);
    if code_ then inc(address_, FBaseOfCode)
    else          inc(address_, FBaseOfData);
    b1 := true;
    mfs := FindSegment(code_, pointer(address_));
    if mfs.IsValid then begin
      b1 := false;
      if name_ = mfs.Unit_ then
        name_ := 'initialization';
      if (unit_ <> '') and (not IsTextEqual(unit_, mfs.Unit_)) then begin
        name_ := unit_ + '.' + name_;
        unit_ := mfs.Unit_;
      end;
    end;
    if FPCount = FPCapacity then begin
      if FPCapacity < 8 then FPCapacity := 16
      else                   FPCapacity := FPCapacity + FPCapacity div 2;
      SetLength(FPItems, FPCapacity);
    end;
    with FPItems[FPCount] do begin
      IsValid := true;
      Code    := code_;
      Name    := name_;
      Address := pointer(address_);
    end;
    inc(FPCount);
    if td32 then begin
      if FSLastAdded[code_] >= 0 then
        with FSItems[FSLastAdded[code_]] do
          if address_ = dword(StartAddress) then
            b1 := false
          else
            if address_ > dword(StartAddress) + Length then begin
              Length := address_ - dword(StartAddress);
              if (FSLastAdded[code_] + 1 < FSCount) and (code_ = FSItems[FSLastAdded[code_] + 1].Code) and
                 (dword(StartAddress) + Length > dword(FSItems[FSLastAdded[code_] + 1].StartAddress)) then
                Length := dword(FSItems[FSLastAdded[code_] + 1].StartAddress) - dword(StartAddress);
            end;
      if b1 and ((FSLastAdded[code_] = -1) or (not IsTextEqual(unit_, FSItems[FSLastAdded[code_]].Unit_))) then
        Segment_NewItem(code_, false, address_, 1, unit_, true, true);
    end;
  end;

  function Public_ParseStr(str: string; pos, len, dataSeg, codeLen: integer) : boolean;
  var b1   : boolean;
      i1   : integer;
      seg  : integer;
      addr : dword;
      mfs  : TMfSegment;
      name : string;
  begin
    result := false;
    try
      if (len > 13) and (str[pos + 4] = ':') then begin
        b1 := false;
        if str[pos + len - 1] = ')' then
          for i1 := pos + len - 2 downto pos + 13 do
            if str[i1] = '(' then begin
              len := i1 - pos;
              break;
            end;
        for i1 := pos + len downto pos + 13 do
          if str[i1 - 1] = ' ' then begin
            b1 := true;
            break;
          end;
        if b1 then begin
          seg := StrToIntEx(true, pointer(integer(str) - 1 + pos), 4);
          if ((seg >= 1) and (seg <= dataSeg)) and (i1 < pos + len) then begin
            name := Copy(str, i1, pos + len - i1);
            addr := dword(StrToIntEx(true, pointer(integer(str) - 1 + pos + 5), 8));
            if dataSeg > 2 then begin
              if (seg > 1) and (seg < dataSeg) then
                inc(addr, codeLen);
              mfs := FindSegment(seg < dataSeg, pointer(addr));
              if mfs.IsValid and PosTextIs1(mfs.Unit_, name) then
                Delete(name, 1, Length(mfs.Unit_) + 1);
            end;
            Public_NewItem(seg < dataSeg, name, addr, false);
            result := true;
          end;
        end;
      end;
    except result := false end;
  end;

  function Public_LoadFromStream(st: TMiniStream) : boolean;
  var i1 : integer;
      b1 : boolean;
  begin
    st.Read(FPCount, 4);
    FPCapacity := FPCount;
    SetLength(FPItems, FPCapacity);
    b1 := true;
    for i1 := 0 to FPCount - 1 do
      with FPItems[i1] do begin
        IsValid := true;
        Code := true;
        if FMinDebugInfo <> '' then
             Name := 'public%' + IntToStrEx(i1)
        else Name := ReadPackedString(st);
        if b1 then begin
          b1 := false;
          st.Read(Address, 4);
          dword(Address) := dword(Address) + FBaseOfCode;
        end else
          integer(Address) := integer(FPItems[i1 - 1].Address) + ReadPackedInteger(st);
      end;
    result := true;
  end;

  procedure Line_NewItem(line_: integer; address_: pointer; mustBeHigherThanLast: boolean);
  begin
    if mustBeHigherThanLast and (FLCount > 0) and
       (dword(address_) <= dword(FLItems[FLCount - 1].Address)) then
      // sometimes Delphi map files have a bug in the line number section
      exit;
    if FLCount = FLCapacity then begin
      if FLCapacity < 8 then FLCapacity := 16
      else                   FLCapacity := FLCapacity + FLCapacity div 2;
      SetLength(FLItems, FLCapacity);
    end;
    with FLItems[FLCount] do begin
      Line    := line_;
      Address := address_;
    end;
    inc(FLCount);
  end;

  function Line_ParseStr(str: string; pos, len, dataSeg, codeLen: integer; mustBeHigherThanLast: boolean) : boolean;
  var b1   : boolean;
      i1   : integer;
      seg  : integer;
      addr : dword;
  begin
    result := false;
    try
      len := pos + len - 1;
      while pos < len do begin
        b1 := true;
        for pos := pos to len do
          if str[pos] <> ' ' then begin
            b1 := false;
            break;
          end;
        if b1 then
          exit;
        i1 := PosStr(':', str, pos, pos+len-1);
        if (i1 = 0) or (i1 + 8 > len) or (str[i1 - 5] <> ' ') then begin
          result := false;
          exit;
        end;
        seg := StrToIntEx(true, pointer(integer(str) - 1 + i1 - 4), 4);
        if (seg >= 1) and (seg < dataSeg) then begin
          addr := dword(StrToIntEx(true,  pointer(integer(str) - 1 + i1 + 1), 8)) + FBaseOfCode;
          if seg > 1 then
            inc(addr, codeLen);
          Line_NewItem(StrToIntEx(false, pointer(integer(str) - 1 + pos), i1 - 5 - pos), pointer(addr), mustBeHigherThanLast);
          result := true;
        end;
        pos := i1 + 9;
      end;
    except result := false end;
  end;

  function Line_LoadFromStream(st: TMiniStream) : boolean;
  var i1 : integer;
  begin
    st.Read(FLCount, 4);
    FLCapacity := FLCount;
    SetLength(FLItems, FLCapacity);
    if FLCount > 0 then begin
      st.Read(FLItems[0].Line, 4);
      st.Read(FLItems[0].Address, 4);
      dword(FLItems[0].Address) := dword(FLItems[0].Address) + FBaseOfCode;
      for i1 := 1 to FLCount - 1 do begin
        FLItems[i1].Line := FLItems[i1 - 1].Line + ReadPackedInteger(st);
        integer(FLItems[i1].Address) := integer(FLItems[i1 - 1].Address) + ReadPackedInteger(st);
      end;
    end;
    result := true;
  end;

  function FindMapFile(onlyMad: boolean = false) : boolean; forward;

  function ImportStr(str: string) : boolean;
  var ms : TMiniStream;
      s1 : string;
      i1 : integer;
  begin
    result := (Length(str) > Length(CMapFileStreamDescriptor) + 8) and
              (TPInteger(dword(str) + dword(Length(CMapFileStreamDescriptor)) + 4)^ in [2, 3]) and
              (Copy(str, 1, Length(CMapFileStreamDescriptor)) = CMapFileStreamDescriptor) and
              ( (FMinDebugInfo = '') or
                (FMinDebugInfo = IntToHexEx(Length(str), 8) + ', ' +
                                 IntToHexEx(TPInteger(dword(str) + dword(Length(CMapFileStreamDescriptor)) + 08)^, 8) + ', ' +
                                 IntToHexEx(TPInteger(dword(str) + dword(Length(CMapFileStreamDescriptor)) + 12)^, 8)          ) );
    if result then begin
      if TPInteger(dword(str) + dword(Length(CMapFileStreamDescriptor)) + 16)^ and $1 = 1 then begin
        s1 := ResToStr(FHInstance, pchar(RT_RCDATA), 'TMADEXCEPT');
        i1 := PosStr(#$c + 'MinDebugInfo' + #$c + #$1f#0#0#0, s1);
        if i1 > 0 then begin
          FMinDebugInfo := Copy(s1, i1 + 18, 31);
          if FindMapFile(true) then
            exit;
        end;
      end else
        FMinDebugInfo := '';
      if TPInteger(dword(str) + dword(Length(CMapFileStreamDescriptor)) + 4)^ = 3 then
        str := Copy(str, 1 + Length(CMapFileStreamDescriptor) + 20,
                          TPInteger(dword(str) + dword(Length(CMapFileStreamDescriptor)))^)
      else
        str := Copy(str, 1 + Length(CMapFileStreamDescriptor) + 8,
                          TPInteger(dword(str) + dword(Length(CMapFileStreamDescriptor)))^);
      OldDecrypt(str, CalcCryptPassword(Length(str)));
      str := Uncompress(str);
      FValid := str <> '';
      if FValid then begin
        ms := TMiniStream.Create;
        try
          ms.Size := Length(str);
          Move(pointer(str)^, ms.Memory^, ms.Size);
          FValid := Segment_LoadFromStream(ms) and
                     Public_LoadFromStream(ms) and
                       Line_LoadFromStream(ms);
          if FValid and (ms.Size - ms.Position = 4) then begin
            ms.Read(FEntryPoint, 4);
            dword(FEntryPoint) := FBaseOfCode + dword(FEntryPoint);
          end;
        finally ms.Free end;
      end;
    end;
  end;

  function GetMapFileFromResources : boolean;
  var s1 : string;
  begin
    s1 := ResToStr(FHInstance, 'MAD', 'EXCEPT');
    result := (s1 <> '') and ImportStr(s1);
  end;

  procedure SortLines(l, r: integer);
  var i1, i2, i3 : integer;
      mfl        : TMfLine;
  begin
    repeat
      i1 := l;
      i2 := r;
      i3 := (l + r) shr 1;
      repeat
        while dword(FLItems[i1].Address) < dword(FLItems[i3].Address) do inc(i1);
        while dword(FLItems[i2].Address) > dword(FLItems[i3].Address) do dec(i2);
        if i1 <= i2 then begin
          mfl         := FLItems[i1];
          FLItems[i1] := FLItems[i2];
          FLItems[i2] := mfl;
          if      i3 = i1 then i3 := i2
          else if i3 = i2 then i3 := i1;
          inc(i1);
          dec(i2);
        end;
      until i1 > i2;
      if l < i2 then SortLines(l, i2);
      l := i1;
    until i1 >= r;
  end;

  procedure SortSegments(l, r: integer);
  var i1, i2, i3 : integer;
      mfs        : TMfSegment;
  begin
    repeat
      i1 := l;
      i2 := r;
      i3 := (l + r) shr 1;
      repeat
        while (FSItems[i1].Code and (not FSItems[i3].Code)) or
              ( (FSItems[i1].Code = FSItems[i3].Code) and
                (dword(FSItems[i1].StartAddress) < dword(FSItems[i3].StartAddress)) ) or
              ( (FSItems[i1].Code = FSItems[i3].Code) and
                (dword(FSItems[i1].StartAddress) = dword(FSItems[i3].StartAddress)) and
                (FSItems[i1].Length > FSItems[i3].Length) ) do
          inc(i1);
        while (FSItems[i3].Code and (not FSItems[i2].Code)) or
              ( (FSItems[i3].Code = FSItems[i2].Code) and
                (dword(FSItems[i3].StartAddress) < dword(FSItems[i2].StartAddress)) ) or
              ( (FSItems[i3].Code = FSItems[i2].Code) and
                 (dword(FSItems[i3].StartAddress) = dword(FSItems[i2].StartAddress)) and
                (FSItems[i3].Length > FSItems[i2].Length) ) do
          dec(i2);
        if i1 <= i2 then begin
          mfs         := FSItems[i1];
          FSItems[i1] := FSItems[i2];
          FSItems[i2] := mfs;
          if      i3 = i1 then i3 := i2
          else if i3 = i2 then i3 := i1;
          inc(i1);
          dec(i2);
        end;
      until i1 > i2;
      if l < i2 then SortSegments(l, i2);
      l := i1;
    until i1 >= r;
  end;

  procedure OptimizeSegments(weak: boolean);
  var i1, i2 : integer;
      b1     : boolean;
  begin
    if FSCount = 0 then
      exit;
    i1 := 0;
    i2 := 1;
    while i2 < FSCount do begin
      b1 := false;
      if (FSItems[i1].Length = 0) or (FSItems[i1].StartAddress = FSItems[i2].StartAddress) then
        FSItems[i1] := FSItems[i2]
      else
        if FSItems[i1].Code <> FSItems[i2].Code then
          b1 := true
        else
          if weak and (dword(FSItems[i1].StartAddress) + FSItems[i1].Length < dword(FSItems[i2].StartAddress)) then
            b1 := true
          else begin
            FSItems[i1].Length := dword(FSItems[i2].StartAddress) - dword(FSItems[i1].StartAddress);
            if IsTextEqual(FSItems[i1].Unit_, FSItems[i2].Unit_) then
              inc(FSItems[i1].Length, FSItems[i2].Length)
            else
              b1 := true
          end;
      if b1 then begin
        inc(i1);
        if i1 <> i2 then
          FSItems[i1] := FSItems[i2];
      end;
      inc(i2);
    end;
    FSCount := i1 + 1;
  end;

  procedure ParseStr(str: string);
  var len, pos    : integer;
      nextLineLen : integer;
      nextLinePos : integer;

    function GetNextLine : boolean;
    var i1 : integer;
    begin
      result := pos < len;
      if result then begin
        while (pos < len) and (str[pos] = ' ') do inc(pos);
        nextLinePos := pos;
        i1 := PosStr(#$D#$A, str, pos);
        if i1 <> 0 then begin
          nextLineLen := i1 - pos;
          pos := i1 + 2;
        end else begin
          nextLineLen := len + 1 - pos;
          pos := maxInt;
        end;
      end;
    end;

  var i1, i2, i3 : integer;
      dataSeg    : integer;
      codeLen    : integer;
      b1         : boolean;
  begin
    len := length(str);
    pos := 1;
    dataSeg := 2;
    codeLen := 0;
    while GetNextLine do
      if nextLineLen <> 0 then begin
        if PosStr('Start         Length     Name                   Class', str, nextLinePos, nextLinePos + nextLineLen) <> 0 then begin
          if nextLineLen <> 0 then
            repeat
              if PosStr('.text', str, nextLinePos, nextLinePos + nextLineLen) > 0 then begin
                if (str[nextLinePos + 4] = ':') and (str[nextLinePos + 13] = ' ') and
                   (str[nextLinePos + 22] = 'H') then
                  codeLen := (StrToIntEx(true, @str[nextLinePos + 14], 8) + $1000 - 1) and $fffff000;
              end else
                if PosStr('.data', str, nextLinePos, nextLinePos + nextLineLen) > 0 then begin
                  dataSeg := StrToIntEx(false, @str[nextLinePos], 4);
                  if (dataSeg < 2) or (dataSeg > 9) then
                    dataSeg := 2;
                end;
            until (not GetNextLine) or (nextLineLen = 0);
        end else if PosStr('Detailed map of segments', str, nextLinePos, nextLinePos + nextLineLen) <> 0 then begin
          while GetNextLine and (nextLineLen = 0) do ;
          if nextLineLen <> 0 then
            repeat
              Segment_ParseStr(str, nextLinePos, nextLineLen, dataSeg, codeLen);
            until (not GetNextLine) or (nextLineLen = 0);
          if FSCount > 0 then
            SortSegments(0, FSCount - 1);
        end else if PosStr('Publics by Value', str, nextLinePos, nextLinePos + nextLineLen) <> 0 then begin
          while GetNextLine and (nextLineLen = 0) do ;
          if nextLineLen <> 0 then
            repeat
              Public_ParseStr(str, nextLinePos, nextLineLen, dataSeg, codeLen);
            until (not GetNextLine) or (nextLineLen = 0);
        end else if PosStr('Line numbers for ', str, nextLinePos, nextLinePos + nextLineLen) <> 0 then begin
          b1 := false;
          i1 := nextLinePos + 17;
          i2 := PosStr('(', str, i1, nextLinePos + nextLineLen);
          if i2 <> 0 then begin
            i3 := PosStr(')', str, i2, nextLinePos + nextLineLen);
            if i3 <> 0 then begin
              while GetNextLine and (nextLineLen = 0) do ;
              if nextLineLen <> 0 then
                repeat
                  Line_ParseStr(str, nextLinePos, nextLineLen, dataSeg, codeLen, b1);
                  b1 := true;
                until (not GetNextLine) or (nextLineLen = 0);
            end;
          end;
        end else if PosStr('Program entry point at ', str, nextLinePos, nextLinePos + nextLineLen) <> 0 then begin
          i1 := nextLinePos + 23;
          if (nextLineLen >= 35) and (str[i1 + 4] = ':') then
            FEntryPoint := pointer(FBaseOfCode + cardinal(StrToIntEx(true, @str[i1 + 5], 8)));
        end;
      end;
  end;

  {$ifdef jdbg}
    function ReadJdbg(jdbg: string) : boolean;
    // some parts copied from JCL
    //
    // - http://jcl.sf.net
    // - http://sourceforge.net/projects/jcl/
    // - http://homepages.borland.com/jedi/jcl/
    //
    //**************************************************************************************************
    //
    // Project JEDI Code Library (JCL)
    //
    // The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
    // you may not use this file except in compliance with the License. You may obtain a copy of the
    // License at http://www.mozilla.org/MPL/
    //
    // Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
    // ANY KIND, either express or implied. See the License for the specific language governing rights
    // and limitations under the License.
    //
    // The Original Code is JclDebug.pas.
    //
    // The Initial Developers of the Original Code are Petr Vones and Marcel van Brakel.
    // Portions created by these individuals are Copyright (C) of these individuals.
    // All Rights Reserved.
    //
    // Contributor(s):
    //   Marcel van Brakel
    //   Flier Lu (flier)
    //   Florent Ouchet (outchy)
    //   Robert Marquardt (marquardt)
    //   Robert Rossmair (rrossmair)
    //   Petr Vones (pvones)
    //
    // Various debugging support routines and classes. This includes: Diagnostics routines, Trace
    // routines, Stack tracing and Source Locations a la the C/C++ __FILE__ and __LINE__ macros.
    //
    // Unit owner: Petr Vones
    //
    //**************************************************************************************************

    const JclDbgDataSignature = $4742444A; // JDBG
          JclDbgHeaderVersion = 1;
    type TJclDbgHeader = packed record
                           signature     : dword;
                           version       : byte;
                           units         : integer;
                           sourceNames   : integer;
                           symbols       : integer;
                           lineNumbers   : integer;
                           words         : integer;
                           moduleName    : integer;
                           checkSum      : integer;
                           checkSumValid : boolean;
                         end;

      function GetStr(const buf: TJclDbgHeader; offset: integer) : string;

        function DecodeNameString(s: pchar) : string;

          function SimpleCryptString(const s: string) : string;
          var i1  : integer;
              ch1 : char;
          begin
            SetLength(result, Length(s));
            for i1 := 1 to Length(s) do begin
              ch1 := s[i1];
              if ch1 <> #$AA then
                byte(ch1) := byte(ch1) xor $AA;
              result[i1] := ch1;
            end;
          end;

        var
          i1  : integer;
          ch1 : byte;
          pb  : ^byte;
          buf : array [0..255] of char;
          bi  : integer;
        begin
          result := '';
          bi := 0;
          pb := pointer(s);
          case pb^ of
            1: begin
                 inc(pb);
                 result := SimpleCryptString(pchar(pb));
                 exit;
               end;
            2: begin
                 inc(pb);
                 buf[bi] := '@';
                 inc(bi);
               end;
          end;
          i1 := 0;
          ch1 := 0;
          repeat
            case i1 and $03 of
              0: ch1 := pb^ and $3F;
              1: begin
                   ch1 := (pb^ shr 6) and $03;
                   inc(pb);
                   inc(ch1, (pb^ and $0F) shl 2);
                 end;
              2: begin
                   ch1 := (pb^ shr 4) and $0F;
                   inc(pb);
                   inc(ch1, (pb^ and $03) shl 4);
                 end;
              3: begin
                   ch1 := (pb^ shr 2) and $3F;
                   inc(pb);
                 end;
            end;
            case ch1 of
              $00      : break;
              $01..$0A : inc(ch1, ord('0') - $01);
              $0B..$24 : inc(ch1, ord('A') - $0B);
              $25..$3E : inc(ch1, ord('a') - $25);
              $3F      : ch1 := ord('_');
            end;
            buf[bi] := chr(ch1);
            inc(bi);
            inc(i1);
          until bi >= sizeOf(buf) - 1;
          buf[bi] := #0;
          result := buf;
        end;

      begin
        if offset > 0 then
             result := DecodeNameString(pchar(dword(@buf) + dword(offset) + dword(buf.Words) - 1))
        else result := '';
      end;

      function ReadValue(var buf: TPByte; var value: integer) : boolean;
      var i1 : integer;
          b1 : byte;
      begin
        i1 := 0;
        value := 0;
        repeat
          b1 := buf^;
          inc(buf);
          inc(value, (b1 and $7F) shl i1);
          inc(i1, 7);
        until b1 and $80 = 0;
        result := value <> maxInt;
      end;

    var fh         : dword;
        map        : dword;
        buf        : ^TJclDbgHeader;
        pi         : TPInteger;
        crc        : integer;
        i1         : integer;
        buf2       : TPByte;
        v1, v2, v3 : integer;
        s1         : string;
        mfs        : TMfSegment;
    begin
      result := false;
      fh := CreateFile(pchar(jdbg), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
      if fh <> INVALID_HANDLE_VALUE then begin
        map := CreateFileMapping(fh, nil, PAGE_READONLY, 0, 0, nil);
        if map <> 0 then begin
          buf := MapViewOfFile(map, FILE_MAP_READ, 0, 0, 0);
          if buf <> nil then begin
            result := (GetFileSize(fh, nil) > sizeOf(buf^)) and (GetFileSize(fh, nil) mod 4 = 0) and
                      (buf^.Signature = JclDbgDataSignature) and (buf^.Version = JclDbgHeaderVersion);
            if result and buf^.CheckSumValid then begin
              crc := -buf^.CheckSum;
              pi := pointer(buf);
              for i1 := 1 to GetFileSize(fh, nil) div 4 do begin
                crc := crc + pi^;
                inc(pi);
              end;
              result := (crc shr 8) or (crc shl 24) = buf^.CheckSum;
            end;
            if result then begin
              // read segments
              dword(buf2) := dword(buf) + dword(buf^.SourceNames);
              v1 := 0;
              v2 := 0;
              while ReadValue(buf2, i1) do begin
                inc(v1, i1);
                ReadValue(buf2, i1);
                inc(v2, i1);
                s1 := GetStr(buf^, v2);
                Delete(s1, 1, PosStr('\', s1, maxInt, 1));
                Segment_NewItem(true, true, dword(v1), 1, s1, false, false);
              end;
              // format segments
              if FSCount > 0 then begin
                SortSegments(0, FSCount - 1);
                OptimizeSegments(false);
                if FTopOfCode > 0 then
                  FSItems[FSCount - 1].Length := FTopOfCode - dword(FSItems[FSCount - 1].StartAddress);
              end;
              // read publics
              v1 := 0;
              v2 := 0;
              v3 := 0;
              dword(buf2) := dword(buf) + dword(buf^.Symbols);
              while ReadValue(buf2, i1) do begin
                inc(v1, i1);
                ReadValue(buf2, i1);
                inc(v2, i1);
                ReadValue(buf2, i1);
                inc(v3, i1);
                s1 := GetStr(buf^, v2);
                if v3 > 0 then begin
                  s1 := s1 + '.' + GetStr(buf^, v3);
                  mfs := FindSegment(true, pointer(v1));
                  if mfs.IsValid and PosTextIs1(mfs.Unit_, s1) then
                    Delete(s1, 1, Length(mfs.Unit_) + 1);
                end;
                Public_NewItem(true, s1, dword(v1), false);
              end;
              // line numbers
              v1 := 0;
              v2 := 0;
              dword(buf2) := dword(buf) + dword(buf^.LineNumbers);
              while ReadValue(buf2, i1) do begin
                inc(v1, i1);
                ReadValue(buf2, i1);
                inc(v2, i1);
                Line_NewItem(v2, pointer(dword(v1) + FBaseOfCode), false);
              end;
            end;
            UnmapViewOfFile(buf);
          end;
          CloseHandle(map);
        end;
        CloseHandle(fh);
      end;
    end;
  {$endif}

  function FindMapFile(onlyMad: boolean = false) : boolean;
  var s1, s2 : string;
  begin
    s1 := FMapFileName;
    Delete(s1, PosStr('.', s1, maxInt, 1), maxInt);
    if (not onlyMad) and LoadStrFromFile(s1 + '.map', s2) then begin
      ParseStr(s2);
      result := true;
    end else
      if LoadStrFromFile(s1 + '.mad', s2) and ImportStr(s2) then
        result := true
      else
        {$ifdef jdbg}
          result := (not onlyMad) and ReadJdbg(s1 + '.jdbg');
        {$else}
          result := false;
        {$endif}
  end;

  {$ifdef td32}
    function ParseTd32(dataSeg: integer = 2; codeLen: integer = 0; mh: PImageNtHeaders = nil) : boolean;
    var names : array of pchar;

      procedure ParseNames(buf: dword);
      var i1 : integer;
      begin
        SetLength(names, TPCardinal(buf)^ + 1);
        inc(buf, 4);
        for i1 := 1 to high(names) do begin
          names[i1] := pointer(buf + 1);
          inc(buf, TPByte(buf)^ + 2);
        end;
      end;

      procedure ParseSegments(buf: dword);
      var i1   : integer;
          s1   : string;
          seg  : integer;
          addr : dword;
          b1   : boolean;
      begin
        s1 := names[TPCardinal(buf + 8)^];
        for i1 := Length(s1) downto 1 do
          if s1[i1] = '\' then begin
            Delete(s1, 1, i1);
            break;
          end;
        i1 := TPWord(buf + 4)^;
        inc(buf, 28);
        b1 := true;
        for i1 := 0 to i1 - 1 do begin
          if TPCardinal(buf + 8)^ > 0 then begin
            seg := TPWord(buf + 2)^;
//            if (seg >= 1) and (seg <= dataSeg) then begin
              addr := TPCardinal(buf + 4)^;
//              if (seg > 1) and (seg < dataSeg) then
//                inc(addr, codeLen);
              if (seg = 1) or b1 then
                Segment_NewItem(seg = 1 {seg < dataSeg}, true, addr, TPCardinal(buf + 8)^, s1, true, false);
              if seg <> 1 then
                // we accept only one data segment per module
                b1 := false;
//            end;
          end;
          inc(buf, 12);
        end;
      end;

      procedure ParsePublics(buf, size: dword);
      var ds   : dword;  // debugSymbols
//          seg  : integer;
          addr : dword;
      begin
        ds := buf;
        while (ds + 2 <= buf + size) and (TPWord(ds)^ > 0) and (ds + TPWord(ds)^ <= buf + size) do begin
          if ((TPWord(ds + 2)^ = $204) or (TPWord(ds + 2)^ = $205)) and (TPWord(ds)^ >= 44) and (TPCardinal(ds + 40)^ <> 0) then begin
//            seg := TPWord(ds + 32)^;
//            if (seg >= 1) and (seg <= dataSeg) then begin
              addr := TPCardinal(ds + 28)^;
//              if (seg > 1) and (seg < dataSeg) then
//                inc(addr, codeLen);
              Public_NewItem(true, names[TPCardinal(ds + 40)^], addr, true);
//            end;
          end else
            if ((TPWord(ds + 2)^ = $201) or (TPWord(ds + 2)^ = $202)) and (TPWord(ds)^ >= 20) and (TPCardinal(ds + 16)^ <> 0) then
              Public_NewItem(false, names[TPCardinal(ds + 16)^], TPCardinal(ds + 4)^, true)
            else
              if (TPWord(ds + 2)^ = $20) and (TPWord(ds)^ >= 26) then begin
//                seg := TPWord(ds + 24)^;
//                if (seg >= 1) and (seg <= dataSeg) then begin
                  addr := TPCardinal(ds + 20)^;
//                  if (seg > 1) and (seg < dataSeg) then
//                    inc(addr, codeLen);
                  Public_NewItem(true, names[TPCardinal(ds + 12)^], addr, true);
//                end;
              end else
                if (TPWord(ds + 2)^ = $21) and (TPWord(ds)^ >= 24) then
                  Public_NewItem(false, names[TPCardinal(ds + 12)^], TPCardinal(ds + 20)^, true);
          inc(ds, TPWord(ds)^ + 2);
        end;
      end;

      procedure ParseLines(buf: dword);

        procedure ParseSrcFileLines(buf: dword; var first, last: pointer);
        var po  : TPCardinal;  // offsets
            pl  : TPWord;      // lines
            i1  : integer;
            seg : integer;
        begin
          seg := TPWord(buf)^;
          if (seg >= 1) and (seg <= dataSeg) then begin
            dword(po) := buf + 4;
            dword(pl) := dword(po) + TPWord(buf + 2)^ * 4;
            first := nil;
            last  := nil;
            for i1 := 0 to integer(TPWord(buf + 2)^) - 1 do begin
              last := pointer(dword(po^) + FBaseOfCode);
              if (seg > 1) and (seg < dataSeg) then
                inc(dword(last), codeLen);
              if first = nil then
                first := last;
              Line_NewItem(pl^, last, false);
              inc(po);
              inc(pl);
            end;
          end;
        end;

        procedure ParseSrcFile(base, buf: dword);
        var plo    : TPCardinal;  // line array offsets
            i1     : integer;
            p1, p2 : pointer;
            s1     : string;
        begin
          dword(plo) := buf + 6;
          if TPCardinal(buf + 2)^ <> 0 then begin
            s1 := names[TPCardinal(buf + 2)^];
            for i1 := Length(s1) downto 1 do
              if s1[i1] = '\' then begin
                Delete(s1, 1, i1);
                break;
              end;
          end;
          p1 := nil;
          p2 := nil;
          for i1 := 0 to integer(TPWord(buf)^) - 1 do begin
            if plo^ <> 0 then begin
              ParseSrcFileLines(base + plo^, p1, p2);
              if (s1 <> '') and (p1 <> nil) then
                Segment_NewItem(true, false, dword(p1), dword(p2) - dword(p1) + 1, s1, true, false);
            end;
            inc(plo);
          end;
        end;

      var i1  : integer;
          psf : TPCardinal;  // srcFiles
      begin
        dword(psf) := buf + 4;
        for i1 := 0 to integer(TPWord(buf)^) - 1 do begin
          if psf^ <> 0 then
            ParseSrcFile(buf, buf + psf^);
          inc(psf);
        end;
      end;

      function ParseTypes(buf: dword) : boolean;
      var items : TPACardinal;
          count : integer;
          tr    : pointer;
          i1    : integer;
      begin
        count := TPInteger(buf + 4)^;
        dword(items) := buf + 8;
        result := true;
        for i1 := 0 to count - 1 do begin
          dword(tr) := buf + items^[i1];
          // len: 2 byte
          // typ: 2 byte
        end;
      end;

      function ParseAll(base: dword; info: word) : boolean;
      var i1     : integer;
          dd, di : dword;   // debugDirectory, debugItem
      begin
        result := false;
        dd := base + TPCardinal(base + 4)^;
        repeat
          dword(di) := dword(dd) + TPWord(dd)^;
          for i1 := 0 to TPInteger(dd + 4)^ - 1 do begin
            if TPWord(di)^ = info then begin
              case info of
                $130 : ParseNames    (base + TPCardinal(di + 4)^);
//                $12b : ParseTypes    (base + TPCardinal(di + 4)^);
                $120 : ParseSegments (base + TPCardinal(di + 4)^);
                $127 : ParseLines    (base + TPCardinal(di + 4)^);
                $129 : ParsePublics  (base + TPCardinal(di + 4)^ + 8 * 4, TPCardinal(di + 8)^ - 4);  // bcb only
                $125 : ParsePublics  (base + TPCardinal(di + 4)^ + 4,     TPCardinal(di + 8)^ - 4);
              end;
              result := true;
            end;
            inc(dword(di), TPWord(dd + 2)^);
          end;
          dd := base + TPCardinal(dd + 8)^;
        until dd = base;
      end;

      procedure SortPublics(l, r: integer);
      var i1, i2, i3 : integer;
          mfp        : TMfPublic;
      begin
        repeat
          i1 := l;
          i2 := r;
          i3 := (l + r) shr 1;
          repeat
            while dword(FPItems[i1].Address) < dword(FPItems[i3].Address) do inc(i1);
            while dword(FPItems[i2].Address) > dword(FPItems[i3].Address) do dec(i2);
            if i1 <= i2 then begin
              mfp         := FPItems[i1];
              FPItems[i1] := FPItems[i2];
              FPItems[i2] := mfp;
              if      i3 = i1 then i3 := i2
              else if i3 = i2 then i3 := i1;
              inc(i1);
              dec(i2);
            end;
          until i1 > i2;
          if l < i2 then SortPublics(l, i2);
          l := i1;
        until i1 >= r;
      end;

      function Parse(base: dword) : boolean;
      begin
        result := false;
        if (TPCardinal(base)^ = $39304246) or (TPCardinal(base)^ = $41304246) then begin
          FSLastAdded [false] := -1;
          FSLastAdded [true ] := -1;
          ParseAll(base, $130);
//          ParseAll(base, $12b);
          ParseAll(base, $120);
          ParseAll(base, $127);
          if FSCount > 0 then
            SortSegments(0, FSCount - 1);
          OptimizeSegments(true);
          if not ParseAll(base, $129) then  // if not bcb
            ParseAll(base, $125);
          if FPCount > 0 then
            SortPublics(0, FPCount - 1);
          OptimizeSegments(false);
          result := true;
        end;
      end;

      function IsTdsOld(tds: string) : boolean;

        function GetFileTime(file_: string) : int64;
        var wfd : TWin32FindData;
            c1  : dword;
        begin
          c1 := FindFirstFile(pchar(file_), wfd);
          if c1 <> INVALID_HANDLE_VALUE then begin
            result := int64(wfd.ftLastWriteTime);
            FindClose(c1);
          end else
            result := 0;
        end;

      var i64, i65 : int64;
      begin
        result := false;
        if IsTextEqual(Copy(tds, PosStr('.', tds, maxInt, 1), maxInt), '.tds') then begin
          i64 := GetFileTime(tds);
          i65 := GetFileTime(Copy(tds, 1, Length(tds) - 4) + '.map');
          if (i64 <> 0) and (i65 <> 0) then begin
            i64 := i64 div 10000000 div 60 div 60 div 24;
            i65 := i65 div 10000000 div 60 div 60 div 24;
            if i64 < i65 then
              result := true;
          end;
        end;
      end;

    var idd    : PImageDebugDirectory;
        psh    : PImageSectionHeader;
        fh     : dword;
        map    : dword;
        buf    : pointer;
        s1, s2 : string;
    begin
      result := false;
      if (mh = nil) or (mh^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress = 0) then begin
        s2 := Copy(FMapFileName, PosStr('.', FMapFileName, maxInt, 1), maxInt);
        if not IsTextEqual(s2, '.tds') then begin
          s1 := Copy(FMapFileName, 1, Length(FMapFileName) - Length(s2)) + '.tds';
          if GetFileAttributes(pchar(s1)) = dword(-1) then
            s1 := FMapFileName;
        end else
          s1 := FMapFileName;
        if (GetFileAttributes(pchar(s1)) <> dword(-1)) and (not IsTdsOld(s1)) then begin
          fh := CreateFile(pchar(s1), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
          if fh <> INVALID_HANDLE_VALUE then begin
            map := CreateFileMapping(fh, nil, PAGE_READONLY, 0, 0, nil);
            if map <> 0 then begin
              buf := MapViewOfFile(map, FILE_MAP_READ, 0, 0, 0);
              if buf <> nil then begin
                mh := GetImageNtHeaders(dword(buf));
                if mh <> nil then begin
                  dword(psh) := dword(mh) + sizeOf(mh^);
                  inc(psh);
                  if psh^.Characteristics and IMAGE_SCN_CNT_CODE <> 0 then begin
                    dataSeg := 3;
                    codeLen := psh^.VirtualAddress;
                    dec(psh);
                    dec(codeLen, psh^.VirtualAddress);
                  end else
                    dec(psh);
                  inc(psh, integer(mh^.FileHeader.NumberOfSections) - 1);
                  if (dataSeg = 2) and
                     (psh^.PointerToRawData <> 0) and
                     (psh^.VirtualAddress = mh^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress) then begin
                    dword(idd) := dword(buf) + psh^.PointerToRawData;
                    result := Parse(dword(buf) + idd^.PointerToRawData);
                  end;
                end else
                  result := (dataSeg = 2) and Parse(dword(buf));
                UnmapViewOfFile(buf);
              end;
              CloseHandle(map);
            end;
            CloseHandle(fh);
          end;
        end;
      end else begin
        dword(idd) := FHInstance + mh^.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_DEBUG].VirtualAddress;
        result := (dataSeg = 2) and Parse(FHInstance + idd^.AddressOfRawData);
      end;
    end;
  {$endif}

var mh      : PImageNtHeaders;
    sh      : PImageSectionHeader;
    dataSeg : integer;
    codeLen : integer;
(*i1:integer;
s1:string;
c1,c2:dword;*)
begin
  inherited Create;
  FValid := valid;
  if FValid then begin
    FCodeOnly    := codeOnly;
    FMapFileName := moduleFileName;
    FHInstance   := hInstance;
    if FHInstance <> 0 then begin
      mh := GetImageNtHeaders(FHInstance);
      if mh <> nil then begin
        dataSeg := 2;
        codeLen := 0;
        with mh^.OptionalHeader do begin
          dword(sh) := dword(mh) + sizeOf(mh^);
          if sh^.Characteristics and IMAGE_SCN_CNT_CODE <> 0 then begin
            FBaseOfCode := FHInstance + sh^.VirtualAddress;
            FTopOfCode  := FBaseOfCode + sh^.Misc.VirtualSize;
            inc(sh);
            if sh^.Characteristics and IMAGE_SCN_CNT_CODE <> 0 then begin
              FTopOfCode := FHInstance + sh^.VirtualAddress + sh^.Misc.VirtualSize;
              dataSeg := 3;
              codeLen := sh^.VirtualAddress;
              dec(sh);
              dec(codeLen, sh^.VirtualAddress);
            end;
          end else begin
            FBaseOfCode := FHInstance + BaseOfCode;
            FTopOfCode  := FBaseOfCode + SizeOfCode;
          end;
          FBaseOfData := FHInstance + BaseOfData;
          FTopOfData  := FBaseOfData + SizeOfInitializedData + SizeOfUnInitializedData;
        end;
        FModuleFileName := moduleFileName;
        FValid := GetMapFileFromResources or {$ifdef td32} ParseTd32(dataSeg, codeLen, mh) or {$endif} FindMapFile;
      end else
        FValid := {$ifdef td32} ParseTd32 or {$endif} FindMapFile;
    end else
      FValid := {$ifdef td32} ParseTd32 or {$endif} FindMapFile;
    if FLCount > 0 then
      SortLines(0, FLCount - 1);
  end;
(*  s1 := '';
  for i1 := 0 to FSCount - 1 do
    s1 := s1 + #$D#$A + booleanToChar(FSItems[i1].Code) + IntToHexEx(dword(FSItems[i1].StartAddress), 8) + ' ' + IntToHexEx(dword(FSItems[i1].Length), 4) + ' ' + FSItems[i1].Unit_;
  s1 := s1 + #$D#$A + #$D#$A;
  for i1 := 0 to FPCount - 1 do
    s1 := s1 + #$D#$A + booleanToChar(FPItems[i1].Code) + IntToHexEx(dword(FPItems[i1].Address), 8) + ' ' + FPItems[i1].Name;
  c1 := CreateFile('c:\desktop\log.txt', GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0);
  WriteFile(c1, s1[1], length(s1), c2, nil);
  CloseHandle(c1); *) 
end;

function TMapFile.FindSegment(code: boolean; address: pointer) : TMfSegment;
var i1, i2, i3 : integer;
    b1         : boolean;
    c1         : dword;
begin
  if FValid and (FSCount > 0) then begin
    i3 := FSCount;
    i1 := i3 div 2;
    i2 := (i1 + 2) div 2;
    b1 := false;
    while i2 > 0 do begin
      c1 := dword(FSItems[i1].StartAddress);
      if (code and (not FSItems[i1].Code)) or
         ((code = FSItems[i1].Code) and (dword(address) < c1)) then begin
        dec(i1, i2);
        if i1 < 0 then i1 := 0;
      end else if (code = FSItems[i1].Code) and (dword(address) < c1 + FSItems[i1].Length) then begin
        result := FSItems[i1];
        exit;
      end else begin
        inc(i1, i2);
        if i1 >= i3 then i1 := i3 - 1;
      end;
      if b1 then break;
      if i2 = 1 then b1 := true
      else           i2 := (i2 + 1) div 2;
    end;
  end;
  result.IsValid := false;
  Finalize(result);
end;

function TMapFile.FindSegment(index: integer) : TMfSegment;
begin
  if (index < 0) or (index >= FSCount) then begin
    result.IsValid := false;
    result.Unit_ := '';
  end else
    result := FSItems[index];
end;

function TMapFile.FindPublic(address: pointer) : TMfPublic;
var i1, i2, i3 : integer;
    b1         : boolean;
begin
  if FValid and (FPCount > 0) then begin
    i3 := FPCount;
    i1 := i3 div 2;
    i2 := (i1 + 2) div 2;
    b1 := false;
    while i2 > 0 do begin
      if dword(address) < dword(FPItems[i1].Address) then begin
        dec(i1, i2);
        if i1 < 0 then i1 := 0;
      end else if ((i1 + 1 < i3) and (dword(address) < dword(FPItems[i1 + 1].Address))) or
                  ((i1 + 1 = i3) and ((FTopOfCode = 0) or (dword(address) < FTopOfCode))) then begin
        result := FPItems[i1];
        exit;
      end else begin
        inc(i1, i2);
        if i1 >= i3 then i1 := i3 - 1;
      end;
      if b1 then break;
      if i2 = 1 then b1 := true
      else           i2 := (i2 + 1) div 2;
    end;
  end;
  result.IsValid := false;
  Finalize(result);
end;

function TMapFile.FindPublic(code: boolean; unit_, name: string; caseSensitive: boolean = true) : dword;
var i1, i2 : integer;
begin
  result := 0;
  for i1 := 0 to FSCount - 1 do
    if (FSItems[i1].Code = code) and IsTextEqual(FSItems[i1].Unit_, unit_) then
      for i2 := 0 to FPCount - 1 do
        if (FPItems[i2].Code = code) and
           (dword(FPItems[i2].Address) >= dword(FSItems[i1].StartAddress)) and
           (dword(FPItems[i2].Address) <= dword(FSItems[i1].StartAddress) + FSItems[i1].Length) and
           ( ( (not caseSensitive) and IsTextEqual(FPItems[i2].Name,  name) ) or
             (      caseSensitive  and (           FPItems[i2].Name = name) )    ) then begin
          result := dword(FPItems[i2].Address);
          break;
        end;
  if (result = 0) and (Pos('.', unit_) = 0) then begin
    result := FindPublic(code, unit_ + '.cpp', name, caseSensitive);
    if result = 0 then begin
      result := FindPublic(code, unit_ + '.h', name, caseSensitive);
      if result = 0 then
        result := FindPublic(code, unit_ + '.c', name, caseSensitive);
    end;
  end;
end;

function TMapFile.FindPublic(index: integer) : TMfPublic;
begin
  if (index < 0) or (index >= FPCount) then begin
    result.IsValid := false;
    result.Name := '';
  end else
    result := FPItems[index];
end;

function TMapFile.FindLine(address: pointer) : integer;
var next : pointer;
begin
  result := FindLine(address, next);
end;

function TMapFile.FindLine(address: pointer; var next: pointer) : integer;
var i1, i2 : integer;
    b1     : boolean;
begin
  if FLCount > 0 then begin
    result := 0;
    dword(next) := dword(address) + 1;
    if FValid and (FLCount > 0) then begin
      i1 := FLCount div 2;
      i2 := (i1 + 2) div 2;
      b1 := false;
      while (i2 > 0) and (i2 < FLCount) do begin
        if dword(address) < dword(FLItems[i1].Address) then begin
          dec(i1, i2);
          if i1 < 0 then i1 := 0;
        end else if ((i1 + 1 < FLCount) and (dword(address) < dword(FLItems[i1 + 1].Address))) or
                    ((i1 + 1 = FLCount) and ((FTopOfCode = 0) or (dword(address) < FTopOfCode))) then begin
          with FindSegment(true, address) do
            if IsValid and (dword(FLItems[i1].Address) >= dword(StartAddress)         ) and
                           (dword(FLItems[i1].Address) <= dword(StartAddress) + Length) then begin
              result := FLItems[i1].line;
              if i1 + 1 < FLCount then
                next := FLItems[i1 + 1].Address;
            end;
          exit;
        end else begin
          inc(i1, i2);
          if i1 >= FLCount then i1 := FLCount - 1;
        end;
        if b1 then break;
        if i2 = 1 then b1 := true
        else           i2 := (i2 + 1) div 2;
      end;
    end;
  end else
    with FindSegment(true, address) do
      if IsValid and LineNumbers then
           result := -1
      else result :=  0;
end;

function TMapFile.Export(newFormat, minDebugInfoOnly: boolean; hideUglyItems: boolean = false) : string;

  procedure WritePackedInteger(st: TMiniStream; int: integer);
  var intRec : packed record b1, b2, b3, b4 : byte; end absolute int;
      b1     : byte;
  begin
    if ((int > 127) or (int < -127)) or (intRec.b1 = $80) then begin
      b1 := $80;
      st.Write(b1, 1);
      st.Write(int, 4);
    end else st.Write(intRec.b1, 1);
  end;

  procedure WritePackedString(st: TMiniStream; str: string);
  var b1 : byte;
  begin
    b1 := Length(str) and $FF;
    st.Write(b1, 1);
    st.Write(pointer(str)^, b1);
  end;

  procedure Segment_SaveToStream(st: TMiniStream);
  var i1     : integer;
      oldPos : integer;
      newPos : integer;
      cnt    : integer;
  begin
    oldPos := st.Position;
    cnt := FSCount;
    st.Write(FSCount, 4);
    for i1 := 0 to FSCount - 1 do
      with FSItems[i1] do
        if Code then begin
          st.Write(StartAddress, 4);
          st.Write(Length, 4);
          if minDebugInfoOnly then begin
            LineNumbers := LineNumbers or (FindLine(pointer(dword(StartAddress) + Length - 1)) > 0);
            st.Write(LineNumbers, 1);
          end else
            WritePackedString(st, Unit_);
        end else
          dec(cnt);
    if FSCount <> cnt then begin
      newPos := st.Position;
      st.Position := oldPos;
      st.Write(cnt, 4);
      st.Position := newPos;
    end;
  end;

  procedure Public_SaveToStream(st: TMiniStream);
  var i1     : integer;
      oldPos : integer;
      newPos : integer;
      cnt    : integer;
      b1     : boolean;
      iLast  : integer;
  begin
    oldPos := st.Position;
    cnt := FPCount;
    st.Write(FPCount, 4);
    b1 := true;
    iLast := 0;
    for i1 := 0 to FPCount - 1 do
      if FPItems[i1].Code then begin
        if not minDebugInfoOnly then
          if hideUglyItems and (FindLine(FPItems[i1].Address) = 0) then
               WritePackedString(st, '')
          else WritePackedString(st, FPItems[i1].Name);
        if b1 then begin
          b1 := false;
          st.Write(FPItems[i1].Address, 4);
        end else
          WritePackedInteger(st, dword(FPItems[i1].Address) - dword(FPItems[iLast].Address));
        iLast := i1;
      end else
        dec(cnt);
    if FPCount <> cnt then begin
      newPos := st.Position;
      st.Position := oldPos;
      st.Write(cnt, 4);
      st.Position := newPos;
    end;
  end;

  procedure Line_SaveToStream(st: TMiniStream);
  var i1 : integer;
  begin
    if minDebugInfoOnly then
         i1 := 0
    else i1 := FLCount;
    st.Write(i1, 4);
    if i1 > 0 then begin
      st.Write(FLItems[0].Line, 4);
      st.Write(FLItems[0].Address, 4);
      for i1 := 1 to FLCount - 1 do begin
        WritePackedInteger(st, FLItems[i1].Line - FLItems[i1 - 1].Line);
        WritePackedInteger(st, dword(FLItems[i1].Address) - dword(FLItems[i1 - 1].Address));
      end;
    end;
  end;

var ms     : TMiniStream;
    i1     : integer;
    crc    : integer;
    st     : TSystemTime;
    ft     : TFileTime;
    w1, w2 : word;
begin
  result := '';
  if FValid then begin
    ms := TMiniStream.Create;
    try
      Segment_SaveToStream(ms);
      Public_SaveToStream(ms);
      Line_SaveToStream(ms);
      ms.Write(FEntryPoint, 4);
      SetLength(result, ms.Size);
      Move(ms.Memory^, pointer(result)^, ms.Size);
    finally ms.Free end;
    result := Compress(result);
    crc := TPAInteger(result)^[2];
    OldEncrypt(result, CalcCryptPassword(Length(result)));
    i1 := Length(result);
    if newFormat then begin
      GetSystemTime(st);
      SystemTimeToFileTime(st, ft);
      FileTimeToDosDateTime(ft, w1, w2);
      //        'www.madshi.net'         + len(result) + version  + date/time   + crc      + flags
      result := CMapFileStreamDescriptor + #0#0#0#0    + #3#0#0#0 + #0#0 + #0#0 + #0#0#0#0 + #0#0#0#0 + result;
      TPInteger(dword(result) + dword(Length(CMapFileStreamDescriptor)) + 00)^ := i1;
      TPWord   (dword(result) + dword(Length(CMapFileStreamDescriptor)) + 08)^ := w1;
      TPWord   (dword(result) + dword(Length(CMapFileStreamDescriptor)) + 10)^ := w2;
      TPInteger(dword(result) + dword(Length(CMapFileStreamDescriptor)) + 12)^ := crc;
      TPInteger(dword(result) + dword(Length(CMapFileStreamDescriptor)) + 16)^ := ord(minDebugInfoOnly);
    end else begin
      //        'www.madshi.net'         + len(result) + version
      result := CMapFileStreamDescriptor + #0#0#0#0    + #2#0#0#0 + result;
      TPInteger(dword(result) + dword(Length(CMapFileStreamDescriptor)))^ := i1;
    end;
    i1 := Length(result);
    if i1 mod 4 <> 0 then begin
      SetLength(result, ((i1 + 3) div 4) * 4);
      for i1 := i1 + 1 to Length(result) do
        result[i1] := #0;
    end;
  end;
end;

var MapFileCache : array of record time: dword; mf: TMapFile; end;

procedure MapFileCache_Add(mf: TMapFile);
var wfd : TWin32FindData;
    c1  : dword;
    dtt : array [0..1] of word;
    i1  : integer;
begin
  c1 := FindFirstFile(pchar(mf.MapFileName), wfd);
  Windows.FindClose(c1);
  FileTimeToDosDateTime(wfd.ftLastWriteTime, dtt[0], dtt[1]);
  i1 := Length(MapFileCache);
  SetLength(MapFileCache, i1 + 1);
  MapFileCache[i1].time := dword(dtt);
  MapFileCache[i1].mf   := mf;
end;

function MapFileCache_Get(file_: string) : TMapFile;
var i1, i2 : integer;
    wfd    : TWin32FindData;
    c1     : dword;
    dtt    : array [0..1] of word;
begin
  result := nil;
  for i1 := 0 to high(MapFileCache) do
    if IsTextEqual(MapFileCache[i1].mf.MapFileName, file_) then begin
      c1 := FindFirstFile(pchar(file_), wfd);
      Windows.FindClose(c1);
      FileTimeToDosDateTime(wfd.ftLastWriteTime, dtt[0], dtt[1]);
      if MapFileCache[i1].time <> dword(dtt) then begin
        MapFileCache[i1].mf.Free;
        i2 := High(MapFileCache);
        MapFileCache[i1] := MapFileCache[i2];
        SetLength(MapFileCache, i2);
      end else
        result := MapFileCache[i1].mf;
      break;
    end;
end;

procedure MapFileCache_Clear;
var i1 : integer;
begin
  for i1 := 0 to high(MapFileCache) do
    MapFileCache[i1].mf.Free;
  MapFileCache := nil;
end;

function FindMapFile(addr: pointer = nil) : TMapFile;
var arrCh : array [0..MAX_PATH] of char;
    mbi   : TMemoryBasicInformation;
begin
  if addr = nil then
    addr := pointer(hInstance);
  try
    if (VirtualQuery(addr, mbi, sizeOf(mbi)) = sizeOf(mbi)) and
       (mbi.State = MEM_COMMIT) and (mbi.AllocationBase <> nil) and
       (GetModuleFileName(dword(mbi.AllocationBase), arrCh, MAX_PATH) <> 0) then begin
      result := MapFileCache_Get(arrCh);
      if result = nil then begin
        result := TMapFile.Create(true, arrCh, dword(mbi.AllocationBase), false);
        if result.IsValid then
          MapFileCache_Add(result);
      end;
    end else
      result := TMapFile.Create(false, '', 0, false);
  except result := TMapFile.Create(false, '', 0, false) end;
end;

function LoadMapFile(mapFile: string) : TMapFile;
begin
  result := TMapFile.Create(true, mapFile, 0, false);
end;

function LoadMapFileEx(mapFile: string; codeOnly: boolean) : TMapFile;
begin
  result := TMapFile.Create(true, mapFile, 0, codeOnly);
end;

// ***************************************************************

function GetMapFileInfos(address: pointer;
                         var moduleName : string;
                         var unitName   : string;
                         var publicName : string;
                         var publicAddr : pointer;
                         var line       : integer) : boolean;
var mf   : TMapFile;
    ebp_ : dword;
begin
  asm
    mov ebp_, ebp
  end;
  if address = nil then
    address := pointer(dword(pointer(ebp_ + 4)^) - 1);
  if address <> nil then begin
    mf := FindMapFile(address);
    result := mf.IsValid;
    if result then begin
      moduleName := mf.ModuleFileName;
      unitName   := mf.FindSegment(true, address).Unit_;
      with mf.FindPublic(address) do begin
        publicName := Name;
        publicAddr := Address;
      end;
      line := mf.FindLine(address);
      if line < 0 then
        line := 0;
    end else
      mf.Free;
  end else
    result := false;
end;

function GetMyProcName(includeLineNumber : boolean = false) : string;
var ebp_ : dword;
    line : integer;
    addr : pointer;
    mf   : TMapFile;
begin
  asm
    mov ebp_, ebp
  end;
  addr := pointer(dword(pointer(ebp_ + 4)^) - 1);
  mf := FindMapFile(addr);
  if mf.IsValid then begin
    result := mf.FindSegment(true, addr).Unit_ + '.' + mf.FindPublic(addr).Name;
    if includeLineNumber then begin
      line := mf.FindLine(addr);
      if line > 0 then
        result := result + ' (' + IntToStrEx(line) + ')';
    end;
  end else begin
    mf.Free;
    result := '???';
  end;
end;

function GetProcNameFromMapFile(proc: pointer) : string;
var mf    : TMapFile;
    arrCh : array [0..MAX_PATH] of char;
    s1    : string;
    i1    : integer;
begin
  result := '';
  mf := FindMapFile(proc);
  if mf.IsValid then begin
    with mf.FindPublic(proc) do
      if IsValid and (Address = proc) then begin
        result := mf.FindSegment(true, address).Unit_;
        if result <> '' then
          result := result + '.';
        result := result + Name;
        if (result <> '') and (PosStr('%', result) > 0) and
           (GetModuleFileName(mf.FHInstance, arrCh, MAX_PATH) > 0) then begin
          s1 := arrCh;
          for i1 := Length(s1) downto 1 do
            if s1[i1] = '\' then begin
              Delete(s1, 1, i1);
              break;
            end;
          result := result + ' (' + s1 + ')';
        end;
      end;
  end else
    mf.Free;
end;

procedure GetLineNumber(proc: pointer; var line: integer; var minAddr, maxAddr: pointer);
var mf : TMapFile;
begin
  if (proc <> nil) and ((line = 0) or (dword(proc) < dword(minAddr)) or (dword(proc) >= dword(maxAddr))) then begin
    mf := FindMapFile(proc);
    if mf.IsValid then begin
      line := mf.FindLine(proc, maxAddr);
      if line < 0 then
        line := 0;
      minAddr := proc;
    end else
      mf.Free;
  end;
end;

// ***************************************************************

initialization
  madDisAsm.GetProcNameFromMapFile := GetProcNameFromMapFile;
  madDisAsm.GetLineNumber          := GetLineNumber;
finalization
  MapFileCache_Clear;
end.
