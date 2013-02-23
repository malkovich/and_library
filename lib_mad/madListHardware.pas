// ***************************************************************
//  madListHardware.pas       version:  1.0   ·  date: 2005-07-09
//  -------------------------------------------------------------
//  madExcept plugin "list of installed hardware"
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2005 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2005-07-09 1.0  initial release

unit madListHardware;

{$I mad.inc}

interface

// ***************************************************************

// hardware tree view (as seen in device manager)
function GetHardwareList : string;

// ***************************************************************

implementation

uses Windows, madExcept, madStrings, madTypes;

// ***************************************************************

function GetHardwareList : string;
var hw : array of record
           name  : string;
           items : array of record
                     name   : string;
                     driver : string;
                   end;
         end;

  procedure AddDevice(className, deviceName, driverPath: string);
  var i1, i2, i3 : integer;
  begin
    i1 := -1;
    for i2 := 0 to high(hw) do begin
      i3 := CompareText(hw[i2].name, className);
      if i3 >= 0 then begin
        i1 := i2;
        break;
      end;
    end;
    if i1 = -1 then begin
      i1 := Length(hw);
      SetLength(hw, i1 + 1);
    end else
      if hw[i1].name <> className then begin
        i2 := Length(hw);
        SetLength(hw, i2 + 1);
        Move(hw[i1], hw[i1 + 1], (i2 - i1) * sizeOf(hw[i1]));
        ZeroMemory(@hw[i1], sizeOf(hw[i1]));
      end;
    with hw[i1] do begin
      name := className;
      i1 := -1;
      for i2 := 0 to high(items) do begin
        i3 := CompareText(items[i2].name, deviceName);
        if i3 >= 0 then begin
          i1 := i2;
          break;
        end;
      end;
      i2 := Length(items);
      SetLength(items, i2 + 1);
      if i1 <> -1 then begin
        Move(items[i1], items[i1 + 1], (i2 - i1) * sizeOf(items[i1]));
        ZeroMemory(@items[i1], sizeOf(items[i1]));
      end else
        i1 := i2;
      items[i1].name   := deviceName;
      items[i1].driver := driverPath;
    end;
  end;

  procedure GetHardwareList9x;
  var key1, key2 : HKEY;
      s1, s2, s3 : string;
      c1, c2     : dword;
      p1         : pchar;
  begin
    if RegOpenKeyEx(HKEY_DYN_DATA, 'Config Manager\Enum', 0, KEY_READ, key1) = 0 then begin
      if RegQueryInfoKey(key1, nil, nil, nil, @c1, @c2, nil, nil, nil, nil, nil, nil) = 0 then begin
        c2 := c2 * 2;
        p1 := pointer(LocalAlloc(LPTR, c2 * 2));
        for c1 := c1 - 1 downto 0 do
          if RegEnumKey(key1, c1, p1, c2) = 0 then begin
            s1 := RegReadStr(key1, p1, 'HardWareKey');
            s2 := RegReadStr(key1, p1, 'Problem');
            if ((Length(s2) <> 4) or (TPCardinal(s2)^ <> $16)) and
               (PosText('\SWENUM\', s1) = 0) and
               (RegOpenKeyEx(HKEY_LOCAL_MACHINE, pchar('Enum\' + s1), 0, KEY_READ, key2) = 0) then begin
              s1 := RegReadStr(key2, '', 'Class');
              if (s1 <> '') and (s1 <> 'NetClient') and (s1 <> 'NetService') and (s1 <> 'NetTrans') then begin
                s1 := RegReadStr(HKEY_LOCAL_MACHINE, 'System\CurrentControlSet\Services\Class\' + s1);
                s2 := RegReadStr(key2, '', 'DeviceDesc');
                s3 := RegReadStr(key2, '', 'CurrentDriveLetterAssignment');
                if s3 <> '' then
                  s2 := '[' + s3 + ':] ' + s2;
                s3 := RegReadStr(key2, '', 'Driver');
                if s3 <> '' then
                  s3 := 'System\CurrentControlSet\Services\Class\' + s3;
                AddDevice(s1, s2, s3);
              end;
              RegCloseKey(key2);
            end;
          end else
            break;
        LocalFree(dword(p1));
      end;
      RegCloseKey(key1);
    end;
  end;

  procedure GetHardwareListNt;
  var handle         : dword;
      di             : packed record
                         cbSize    : dword;
                         classGuid : TGuid;
                         devInst   : dword;
                         reserved  : pointer;
                       end;
      i1             : integer;
      s1, s2, s3, s4 : string;
      dll            : dword;
      gcd            : function (classGuid, enum: pointer; parentWnd, flags: dword) : dword; stdcall;
      ddil           : function (handle: dword) : bool; stdcall;
      edi            : function (handle, index: dword; devInfo: pointer) : bool; stdcall;
      gdrp           : function (handle: dword; devInfo: pointer; infoType: dword;
                                 out dataType: dword; buf: pointer; bufSize: dword;
                                 var reqSize: dword) : bool; stdcall;

    function GetDeviceInfo(infoType: dword) : string;
    var c1, c2 : dword;
        p1     : pchar;
    begin
      result := '';
      c1 := 0;
      gdrp(handle, @di, infoType, c2, nil, 0, c1);
      if c1 <> 0 then begin
        c1 := c1 * 2;
        p1 := pointer(LocalAlloc(LPTR, c1));
        if gdrp(handle, @di, infoType, c2, p1, c1, c1) then
          result := p1;
        LocalFree(dword(p1));
      end;
    end;

  begin
    dll := LoadLibrary('setupapi.dll');
    if dll <> 0 then begin
      gcd  := GetProcAddress(dll, 'SetupDiGetClassDevsA');
      ddil := GetProcAddress(dll, 'SetupDiDestroyDeviceInfoList');
      edi  := GetProcAddress(dll, 'SetupDiEnumDeviceInfo');
      gdrp := GetProcAddress(Dll, 'SetupDiGetDeviceRegistryPropertyA');
      if (@gcd <> nil) and (@ddil <> nil) and (@edi <> nil) and (@gdrp <> nil) then begin
        handle := gcd(nil, nil, 0, 6);  // DIGCF_PRESENT or DIGCF_ALLCLASSES
        if handle <> INVALID_HANDLE_VALUE then begin
          di.cbSize := sizeof(di);
          i1 := 0;
          while edi(handle, i1, @di) do begin
            s1 := GetDeviceInfo($7);    // SPDRP_CLASS
            s2 := GetDeviceInfo($1);    // SPDRP_HARDWAREIE
            if (s1 <> 'LegacyDriver') and (s1 <> 'Unknown') and (s1 <> 'Volume') and
               (not PosTextIs1('sw\', s2)) then begin 
              s1 := GetDeviceInfo($8);    // SPDRP_CLASSGUID
              s2 := RegReadStr(HKEY_LOCAL_MACHINE, 'System\CurrentControlSet\Control\Class\' + s1);
              if s2 <> '' then
                s1 := s2;
              if s1 <> '' then begin
                s2 := GetDeviceInfo($c);    // SPDRP_FRIENDLYNAME
                if s2 = '' then
                  s2 := GetDeviceInfo($0);  // SPDRP_DEVICEDESC
                s3 := GetDeviceInfo($9);    // SPDRP_DRIVER
                if s3 <> '' then
                  s3 := 'System\CurrentControlSet\Control\Class\' + s3;
                s4 := RegReadStr(HKEY_LOCAL_MACHINE, s3, 'Characteristics');
                if (Length(s4) <> 4) or (TPCardinal(s4)^ and $8 = 0) then
                  AddDevice(s1, s2, s3);
              end;
            end;
            inc(i1);
          end;
          ddil(handle);
        end;
      end;
      FreeLibrary(dll);
    end;
  end;

var i1, i2 : integer;
    s1     : string;
begin
  result := '';
  if GetVersion and $80000000 = 0 then
       GetHardwareListNt
  else GetHardwareList9x;
  for i1 := 0 to high(hw) do begin
    result := result + #$D#$A + '+ ' + hw[i1].name;
    for i2 := 0 to high(hw[i1].items) do begin
      result := result + #$D#$A + '  - ' + hw[i1].items[i2].name;
      if hw[i1].items[i2].driver <> '' then begin
        s1 := RegReadStr(HKEY_LOCAL_MACHINE, hw[i1].items[i2].driver, 'ProviderName');
        if (s1 <> '') and (s1[1] <> '(') and (not IsTextEqual(s1, 'Microsoft')) then begin
          s1 := RegReadStr(HKEY_LOCAL_MACHINE, hw[i1].items[i2].driver, 'DriverVersion');
          if s1 = '' then
            s1 := RegReadStr(HKEY_LOCAL_MACHINE, hw[i1].items[i2].driver, 'DriverDate');
          if s1 <> '' then
            result := result + ' (driver ' + RetTrimStr(s1) + ')';
        end;
      end;
    end;
  end;
  Delete(result, 1, 2);
end;

// ***************************************************************

initialization
  RegisterBugReportPlugin('hardware', 'list of installed hardware', GetHardwareList);
finalization
  UnregisterBugReportPlugin('hardware');
end.
