unit DbgLoger;

interface
uses windows;

procedure DBGConsole( fmt:string; parma:array of TVarRec); overload;
procedure DBGConsole( fmt:string); overload;

procedure DebugPrinter(Msg: PChar); stdcall;
procedure DBG( Msg: String ); overload;
procedure DBG( fmt:string; parma:array of TVarRec); overload;

function GetLogFileName:String;
function GetLastCachedLog: String;

type
  TLogMode = (lmDebugView, lmLogFile, lmConsole, lmConsoleLogfile, lmNone);

var
  Max_log_file_size: DWORD = 16 * 1024 * 1024;
  LogMode: TLogMode = lmDebugView;
  HasHead: LongBool = True;
  IsUseCache: BOOL = False;
  DefaultLogFileName: String;
  FullLogFileName: String;
  MyOutputDebugString: procedure (lpOutputString: PChar); stdcall = nil;

implementation
uses SysUtils, forms, Classes, SyncObjs, TCacheQueueThread, madZip;

procedure DBGConsole( fmt:string; parma:array of TVarRec); overload;
var
  dbgStr: String;
begin
  dbgStr := Format(fmt, parma);
  if IsConsole then
    WriteLn(dbgStr);
  DBG(dbgStr);
end;

procedure DBGConsole( fmt:string); overload;
begin
  DBGConsole (fmt, []);
end;

function GetSelfModuleName: string;
var
  SignWord: PWORD;
  SelfHandle: THandle;
  Buffer:array[byte] of char;
begin
  SignWord := @GetSelfModuleName;
  SignWord := Pointer (DWORD (SignWord) And $FFFFF000);

  while SignWord^ <> $5A4D do
    SignWord := Pointer (DWORD(SignWord) - $1000);

  SelfHandle := THandle (SignWord);

  ZeroMemory(@buffer[0], 256);
  GetModuleFileName(SelfHandle, @Buffer, 256);
  result := buffer;
end;

function GetLogFileName:String;
begin
  Result := GetSelfModuleName;
  if DefaultLogFileName = '' then
    Result := ChangeFileExt (Result, '.LOG')
  else
  begin
    Result := ExtractFilePath(Result);
    Result := Result + DefaultLogFileName;
  end;
end;

var
  CRS: TCriticalSection;
  FS: TFileStream;
  FSMutex: THandle;
  PidStr: string;


procedure CutHalfLogLine (var FS: TFileStream);
var
  Buf: PChar;
  SeekPosition: Integer;
  NewSize: Integer;
begin
  NewSize := FS.Size div 2;

  FS.Seek ( - NewSize, soFromEnd);
  Buf := AllocMem (NewSize);
  FS.Read (Buf[0], NewSize);
  FS.Size := 0;

  SeekPosition := 0;
  repeat
      if Buf[SeekPosition] = Chr($D) then
        if Buf[SeekPosition + 1] = Chr($A) then Break;
      Inc (SeekPosition);
  until SeekPosition = NewSize - 3;
  SeekPosition := SeekPosition + 2;

  FS.Seek(0, soFromEnd);
  FS.Write (Buf[SeekPosition], NewSize - SeekPosition);
  FreeMem (Buf);
end;

function LMDReplaceChar (const aValue:String; toReplace, Replace:Char): String;
var
  i:Integer;
begin
  result:=aValue;
  for i:=1 to Length(result) do
    if result[i]=toReplace then
      result[i]:=Replace;
end;

procedure WriteToLogFile (LineStr: String);
var
  MutexName: String;
begin
  if not assigned (FS) then
  begin
    FullLogFileName := GetLogFileName;
    if not FileExists(FullLogFileName) then
      FileClose(FileCreate(FullLogFileName));
    FS := TFileStream.Create (FullLogFileName, fmShareDenyNone or fmOpenWrite);

    MutexName := FullLogFileName;
    MutexName := LMDReplaceChar (MutexName, '\', '_');
    MutexName := LMDReplaceChar (MutexName, '.', '_');
    MutexName := LMDReplaceChar (MutexName, ':', '_');

    FSMutex := OpenMutex (MUTEX_ALL_ACCESS, False, PChar(MutexName));
    if FSMutex = 0 then
      FSMutex := CreateMutex (nil, false, PChar(MutexName));
  end;

  try
    if WAIT_OBJECT_0 = WaitForSingleObject (FSMutex, INFINITE) then
    begin
      if FS.Size > MAX_LOG_FILE_SIZE then
        CutHalfLogLine (FS);

      FS.Seek(0, soFromEnd);
      LineStr := LineStr + #13#10;
      FS.Write (LineStr[1], Length(LineStr));
    end;
  finally
    ReleaseMutex (FSMutex);
  end;
  
end;

var
  CacheLogSL: TStringList;

procedure AddCacheLog (Msg: String);
var
  SLCount: Integer;
begin
  if not assigned (CacheLogSL) then
    CacheLogSL := TStringList.Create;

  CacheLogSL.Add(Msg);

  SLCount := CacheLogSL.Count;

  if SLCount > 500 then
  if (SLCount mod 100) = 0 then
  begin
    repeat
      CacheLogSL.Delete(0);
    until CacheLogSL.Count <= 500;
  end;   
end;
  
function GetLastCachedLog: String;
begin
  if not assigned (CacheLogSL) then
    CacheLogSL := TStringList.Create;

  Result := CacheLogSL.Text;
end;

procedure DebugPrinter (Msg: PChar); stdcall;
var
  FmtStr: String;
begin
  if PidStr = '' then
  begin
    CRS := TCriticalSection.Create;
    PidStr := Format (' [%s|%d] ', [ExtractFileName(Application.ExeName), GetCurrentProcessID]);
    PidStr := #9 + PidStr + #9;
  end;


  CRS.Enter;
  try

    FmtStr := StrPas(Msg);
    if HasHead then
      FmtStr := DateTimeToStr (NOW) +  PidStr + FmtStr;

    if IsUseCache then
      AddCacheLog (FmtStr);

    case LogMode of
      lmDebugView:
          begin
            if assigned (MyOutputDebugString) then
              MyOutputDebugString (PChar(FmtStr))
            else
              OutputDebugString (PChar(FmtStr));
          end;
      lmLogFile:  WriteToLogFile(FmtStr);
      lmNone:;
      lmConsole: if IsConsole then  WriteLn (FmtStr);
      lmConsoleLogfile:
      begin
          WriteToLogFile(FmtStr);
          if IsConsole then
            WriteLn (FmtStr);
      end;
    end;

  finally
    CRS.Leave;
  end;
  
end;

procedure DBG(Msg: String);
begin
  Msg := Trim (Msg);
  if Msg = '' then exit;
  DebugPrinter(PChar(Msg));
end;

procedure DBG( fmt:string; parma:array of TVarRec);
begin
  if fmt = '' then exit;
  DBG(Format(fmt, parma));
end;

initialization

finalization

if FSMutex <> 0 then
  CloseHandle (FSMutex);

end.
