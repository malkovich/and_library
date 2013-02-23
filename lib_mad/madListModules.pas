// ***************************************************************
//  madListModules.pas        version:  1.0   ·  date: 2005-07-09
//  -------------------------------------------------------------
//  madExcept plugin "list of loaded modules (in our process)"
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2005 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2005-07-09 1.0  initial release

unit madListModules;

{$I mad.inc}

interface

// ***************************************************************

// module list of the current process
function GetModuleList : string;

// ***************************************************************

implementation

uses Windows, madExcept, madStrings, madTypes, madTools;

// ***************************************************************

function GetModuleList : string;
var ml         : array of record
                   handle   : dword;
                   fileName : string;
                   filePath : string;
                   version  : string;
                 end;
    i1, i2, i3 : integer;
    s2, s3     : string;
    p1, p2     : pointer;
    mbi        : TMemoryBasicInformation;
    arrCh      : array [0..MAX_PATH] of char;
    i64        : int64;
begin
  result := '';
  p1 := nil;
  p2 := nil;
  i1 := 0;
  ml := nil;
  SetLength(ml, 32);
  while VirtualQuery(p1, mbi, sizeOf(mbi)) = sizeOf(mbi) do begin
    if (mbi.State = MEM_COMMIT) and
       (mbi.AllocationBase <> p2) and (mbi.AllocationBase = mbi.BaseAddress) and
       (GetModuleFileName(dword(mbi.AllocationBase), arrCh, MAX_PATH) > 0) then begin
      if i1 = Length(ml) then
        SetLength(ml, i1 * 2);
      with ml[i1] do begin
        handle   := dword(mbi.AllocationBase);
        filePath := arrCh;
        i64 := GetFileVersion(filePath);
        if i64 <> 0 then
          version := FileVersionToStr(i64);
        fileName := filePath;
        for i2 := Length(fileName) downto 1 do
          if fileName[i2] = '\' then begin
            Delete(fileName, 1, i2);
            break;
          end;
        Delete(filePath, Length(filePath) - Length(fileName), maxInt);
      end;
      inc(i1);
    end;
    p2 := mbi.AllocationBase;
    dword(p1) := dword(p1) + mbi.RegionSize;
  end;
  SetLength(ml, i1);
  i2 := 0;
  i3 := 0;
  for i1 := 0 to high(ml) do
    with ml[i1] do begin
      if Length(fileName) + 1 > i2 then
        i2 := Length(fileName) + 1;
      if Length(version) + 1 > i3 then
        i3 := Length(version) + 1;
    end;
  SetLength(s2, i2);
  SetLength(s3, i3);
  for i1 := 0 to high(ml) do
    with ml[i1] do begin
      Move(pchar(fileName)^, pchar(s2)^, Length(fileName));
      for i2 := Length(fileName) + 1 to Length(s2) do
        s2[i2] := ' ';
      Move(pchar(version)^, pchar(s3)^, Length(version));
      for i2 := Length(version) + 1 to Length(s3) do
        s3[i2] := ' ';
      result := result + #$D#$A + Copy(IntToHexEx(ml[i1].handle, 8), 2, 8) + ' ' + s2 + s3 + ml[i1].filePath;
    end;
  Delete(result, 1, 2);
end;

// ***************************************************************

initialization
  RegisterBugReportPlugin('modules', 'list of loaded modules', GetModuleList);
finalization
  UnregisterBugReportPlugin('modules');
end.
