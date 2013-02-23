// ***************************************************************
//  madStackTrace.pas         version:  2.1h  ·  date: 2006-02-23
//  -------------------------------------------------------------
//  stack tracing
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2006 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2006-02-23 2.1h rare crash in FindCodeAddrAndTarget fixed
// 2005-11-09 2.1g (1) support for FastMM4 memory leak stack traces added
//                 (2) "StackTrace" optionally returns dynamic array
// 2005-06-11 2.1f (1) asm code "call @label" support slightly improved
//                 (2) slightly improved stack trace evaluation calculation
//                 (3) choice of disasm location improved
// 2005-01-11 2.1e bug in ComposeResult fixed
// 2004-10-24 2.1d some special processing for bcb added
// 2004-08-15 2.1c (1) asm code "call @label" in Delphi funcs is supported now
//                 (2) better handling of internal errors
//                 (3) relative infos don't modify absolute infos anymore
//                 (4) thread creator is shown even if no module info is found
//                 (5) "hideUglyItems" works differently now (hides more items)
// 2004-04-25 2.1b (1) stack frame tracing might get trapped in endlos loop
//                 (2) stack frame elements don't ever get deleted anymore now
//                 (3) improved madDisAsm makes entry point patch superfluous
// 2004-04-12 2.1a (1) AMD64 NX: LocalAlloc -> VirtualAlloc (in AddFunction)
//                 (2) GetTickCount doesn't overrun now, anymore
//                 (3) InitEbpFrames: more detailed + no exceptions 
// 2004-03-07 2.1  (1) support for relative line numbers added
//                 (2) speed up, when the dpr file as a large begin..end block
//                 (3) stack growing isn't destroyed anymore
//                 (4) fixed bug in RealizeOptimalLinkChain -> better traces!
//                 (5) ebp stack frame evaluation improved
//                 (6) workaround for "DispatchMessageA" trace hole improved
//                 (7) "GetImageProcName" name unmangling is now optional
//                 (8) raw stack tracing was sometimes a bit too strict
// 2003-11-10 2.0c (1) the progress bar was not really thread safe
//                 (2) support for relative stack addresses added
// 2003-06-09 2.0b (1) moved some code into the new "madTools.GetImageProcName"
//                 (2) slight changes to support disassembling in bug report
//                 (3) stack trace quality in win9x improved
//                 (4) the 2 last stack frame items are ignored now, since they
//                     often belong to Windows' built in exception handling
//                 (5) module entry point function patching improved
//                 (6) option "hideUglyItems" hides stack items with no line
//                 (7) calls to uninitialized function variables resulted in
//                     low quality stack traces
// 2002-11-26 2.0a (1) stack trace quality further improved
//                 (2) avoiding of handled exceptions when running inside of IDE
// 2002-11-14 2.0  the whole unit was totally rewritten
//                 (1) much better detection of invalid stack items
//                 (2) exported APIs are listed
//                 (3) functions without line numbers are listed
//                 (4) recursive call stack areas are detected & removed
//                 (5) simplified interface (just one function)
//                 (6) progress bar support for madExcept exception handling
// 2002-09-21 1.5c (1) moved some code from TIStackTrace.Create to the new
//                     method TIStackTrace.AddItem
//                 (2) AsStr format changed
// 2001-11-12 1.5b little bug in the stack trace core logic fixed
// 2001-07-15 1.5a GetMultiLineStr -> AsStr; GetSingleLineStr deleted
// 2001-06-06 1.5  trace logic completely rewritten -> faster & more precise
// 2001-04-30 1.4d minor changes in order to get rid of SysUtils
// 2000-11-24 1.4c bug in TIStackTrace.GetMultiLineStr fixed

unit madStackTrace;

{$I mad.inc}

{ $define log}

interface

uses Windows, madTypes, madNVPrgrAlert;

// ***************************************************************

type
  TStackItem = record
                 Addr         : pointer;   // code address
                 relAddr      : dword;     // relative address to function entry point
                 ModuleName   : string;
                 UnitName     : string;
                 Line         : integer;   // line number (0 = unknown)
                 relLine      : integer;   // relative line number to function entry point
                 FunctionName : string;    // function/method name
               end;
  TStackTrace = array of TStackItem;
  TPStackTrace = ^TStackTrace;

// traces the current stack
// example:
// 00461185 project1.exe unit1    124 FaultProc
// 0040A84B project1.exe unit1    137 Button1Click
// 00418E55 project1.exe project1  80 project1
function StackTrace (hideUglyItems     : boolean        = false;
                     showRelativeAddrs : boolean        = false;
                     showRelativeLines : boolean        = false;
                     stackTrace        : TPStackTrace   = nil;
                     currentAddr       : pointer        = nil;
                     isException       : boolean        = false;
                     extException      : boolean        = false;
                     stackBottom       : dword          = 0;
                     stackTop          : dword          = 0;
                     creator           : pointer        = nil;
                     exceptAddr        : TPPointer      = nil;
                     exceptFunc        : TPPointer      = nil;
                     exceptFuncAddr    : TPPointer      = nil;
                     progressAlert     : IProgressAlert = nil;
                     ebp               : dword          = 0;
                     dumbTrace         : boolean        = false;
                     bcbTermination    : boolean        = false;
                     preparedStack     : pointer        = nil;
                     pAbort            : TPBoolean      = nil;
                     pCrc1             : TPCardinal     = nil;
                     pCrc2             : TPCardinal     = nil;
                     pDelphiThread     : TPBoolean      = nil;
                     pMinDebugInfos    : TPDAString     = nil;
                     dontAddTopItem    : boolean        = false) : string;

// ***************************************************************

// some stuff for internal use only
function FastMM_LogStackTrace(preparedStack: pointer; stackTraceDepth: integer; buf: pchar;
                              hideUglyItems, showRelativeAddrs, showRelativeLines, dumbTrace: boolean) : pchar;
function StackAddrToStr (addr: pointer; relAddr: boolean = false; relLine: boolean = false) : string;
function InternalError (func: string; relAddr, relLine: boolean) : string;
type TPreStackItem   = packed record dontKill: boolean; afterInstr: pointer; end;
     TDAPreStackItem = packed array of TPreStackItem;
function PrepareStackTrace(ebp, stackTop, stackBottom: dword;
                           currentAddr: pointer; isException: boolean;
                           var stack: TDAPreStackItem) : integer;
{$ifdef bcb}
  type BcbTermination  = class (MadException) end;
{$endif}

implementation

uses madMapFile, madDisAsm, madStrings, madTools, madZip;

// ***************************************************************
// caching improves performance, but consumes memory (as usual)

var
  moduleCount : integer;
  moduleCache : array of record
    handle    : dword;
    name      : string;
    codeBegin : dword;      // begin of the code section
    codeEnd   : dword;      // end
    dataBegin : dword;      // begin of the data sections
    dataEnd   : dword;      // end
    mapFile   : TMapFile;   // this module's map file, if available
  end;

var
  functionCount : integer;
  functionCache : array of record
    module   : integer;        // to which module does this function belong?
    name     : string;         // function name, if we can find out
    fi       : TFunctionInfo;  // some disassembling infos
    approved : boolean;        // this entry point is proved to be real
    invalid  : boolean;        // this entry point is proved to be no function
  end;

function AddFunction(code: pointer; name: string; module: integer;
                     approved, force: boolean; tryRead_: dword) : integer; forward;

function AddModule(module: dword; var index: integer; tryRead_: dword) : boolean;
// add the specified module to our internal module cache

(*  function GetHalt0Address : pointer;
  asm
    mov eax, offset System.@Halt0
    cmp word ptr [eax], $25ff
    jnz @done
    mov eax, [eax + 2]
    mov eax, [eax]
   @done:
  end; *)

type TEntryPointPatch = packed record
                          call : byte;     // $e9
                          halt : integer;
                          ret  : byte;     // $c3
                        end;
var arrCh      : array [0..MAX_PATH] of char;
    i1         : integer;
    nh         : PImageNtHeaders;
    sh         : PImageSectionHeader;
//    ci         : TCodeInfo;
//    p1, p2, p3 : pointer;
//    c1         : dword;
//    efp        : ^TEntryPointPatch;
//    w1         : word;
//    ep         : pointer;
begin
  result := false;
  index  := -1;
  for i1 := 0 to moduleCount - 1 do
    if moduleCache[i1].handle = module then begin
      // the module is already in our cache, so return the index and exit
      result := true;
      index  := i1;
      exit;
    end;
  if GetModuleFileName(module, arrCh, MAX_PATH) <> 0 then begin
    nh := GetImageNtHeaders(module);
    if nh <> nil then
      with nh^.OptionalHeader do begin
        // the module has a valid PE image header, so add to our cache
        result := true;
        if moduleCount = Length(moduleCache) then
          if moduleCount = 0 then
               SetLength(moduleCache, 16)
          else SetLength(moduleCache, moduleCount * 2);
        index := moduleCount;
        inc(moduleCount);
        with moduleCache[index] do begin
          handle    := module;
          name      := arrCh;
          for i1 := Length(name) downto 1 do
            if name[i1] = '\' then begin
              Delete(name, 1, i1);
              break;
            end;
          dword(sh) := dword(nh) + sizeOf(nh^);
          if sh^.Characteristics and IMAGE_SCN_CNT_CODE <> 0 then begin
            codeBegin := handle    + sh^.VirtualAddress;
            codeEnd   := codeBegin + sh^.Misc.VirtualSize;
            inc(sh);
            if sh^.Characteristics and IMAGE_SCN_CNT_CODE <> 0 then
              codeEnd := handle + sh^.VirtualAddress + sh^.Misc.VirtualSize;
          end else begin
            codeBegin := handle    + BaseOfCode;
            codeEnd   := codeBegin + SizeOfCode;
          end;
          dataBegin := handle    + BaseOfData;
          dataEnd   := dataBegin + SizeOfInitializedData + SizeOfUnInitializedData;
          mapFile   := FindMapFile(pointer(module));
          if not mapFile.IsValid then begin
            mapFile.Free;
            mapFile := nil;
          end;
(*          if (i1 <> -1) and
             ( (mapFile <> nil) or
               IsTextEqual(name, 'Delphi32.exe') or
               IsTextEqual(name, 'bds.exe'     ) or
               IsTextEqual(name, 'bcb.exe'     )    ) then begin
            // we have a Delphi/BCB compiled module here with map file infos
            // so let's parse the entry point and add it to the function list
            // a big problem is that the entry point function stops with @Halt0
            // instead of with a "ret" instruction, so we have to patch it a bit
            if (mapFile <> nil) and (mapFile.EntryPoint <> nil) then
                 ep := mapFile.EntryPoint
            else ep := pointer(module + nh^.OptionalHeader.AddressOfEntryPoint);
            p1 := ep;
            ci := ParseCode_(p1, tryRead_);
            while ci.IsValid and (dword(ci.Next) < codeEnd) do begin
              p2 := ci.Next;
              if ci.Target <> nil then begin
                if ci.Jmp and ((dword(ci.Target) < codeBegin) or (dword(ci.Target) > codeEnd)) then
                  // the entry point function is already patched
                  break;
                p3 := ci.This;
                if TryRead(ci.Target, @w1, 2, tryRead_) and (w1 = $25ff) then
                  // resolve package import table jump
                  ci := ParseCode_(ci.Target, tryRead_);
                if ci.Target = GetHalt0Address then begin
                  // we found the @Halt0 call of the entry point function
                  if (byte(p3^) = $e8) and VirtualProtect(p3, 5, PAGE_EXECUTE_READWRITE, @c1) then begin
                    // efp gets now "call @Halt0" plus "ret"
                    efp := VirtualAlloc(nil, sizeOf(efp^), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
                    efp^.call := $e8;
                    efp^.halt := integer(ci.Target) - integer(efp) - 5;
                    efp^.ret  := $c3;
                    // "call @Halt" gets to "jmp efp"
                    byte(p3^) := $e9;
                    TPInteger(dword(p3) + 1)^ := integer(efp) - integer(p3) - 5;
                    // finally we can let our function parser do its work
                    i1 := AddFunction(ep, 'EntryPoint', index, true, false, tryRead_);
                    if i1 <> -1 then
                      with functionCache[i1].fi do begin
                        // unfortunately Delphi often misuses a "ret" as a
                        // "jmp" instruction in the entry point function
                        // as a result our function parsing stops too early
                        // just to be sure we add the call to "@Halt0" manually
                        i1 := Length(FarCalls);
                        SetLength(FarCalls, i1 + 1);
                        FarCalls[i1].Call      := ci.Call;
                        FarCalls[i1].CodeAddr1 := p1;
                        FarCalls[i1].CodeAddr2 := p2;
                        FarCalls[i1].Target    := ci.Target;
                        FarCalls[i1].RelTarget := ci.RelTarget;
                        FarCalls[i1].PTarget   := ci.PTarget;
                        FarCalls[i1].PPTarget  := ci.PPTarget;
                      end;
                  end;
                  break;
                end;
              end;
              p1 := p2;
              ci := ParseCode_(p2, tryRead_);
            end;
          end; *)
        end;
      end;
  end;
end;

var DispatchMessageA : pointer = nil;
function AddFunction(code: pointer; name: string; module: integer;
                     approved, force: boolean; tryRead_: dword) : integer;
// parse the specified function and add it to our internal function cache
// if the parsing was successful, also try to find a name for the function

  procedure ApproveFunction(func: integer); overload; forward;
  // approve a function and it's jump children
  procedure ApproveFunction(code: pointer); overload;
  var i1 : integer;
  begin
    for i1 := 0 to functionCount - 1 do
      if functionCache[i1].fi.EntryPoint = code then
        ApproveFunction(i1);
  end;
  procedure ApproveFunction(func: integer); overload;
  var i1 : integer;
  begin
    with functionCache[func], fi do begin
      approved := true;
      for i1 := 0 to high(FarCalls) do
        if not FarCalls[i1].Call then
          ApproveFunction(FarCalls[i1].Target);
    end;
  end;

  function GetFunctionName(module: integer; code: pointer) : string;
  // get the function name for the specified code
  // the name might come from the map file or from the export table
  begin
    result := '';
    if module <> -1 then
      with moduleCache[module] do
        if mapFile = nil then
             result := GetImageProcName(handle, code, false)
        else result := mapFile.FindPublic(code).Name;
  end;

var fi     : TFunctionInfo;
    i1, i2 : integer;
    mi     : TMemoryBasicInformation;
begin
  Finalize(fi);
  for i1 := 0 to functionCount - 1 do
    if functionCache[i1].fi.EntryPoint = code then begin
      // the function is already in our internal cache, so return the index
      result := i1;
      if approved and (not functionCache[i1].approved) then
        // the function is approved, but the cache entry is not yet, so:
        ApproveFunction(i1);
      exit;
    end;
  if (code <> nil) or force then begin
    if module = -1 then begin
      for i1 := 0 to moduleCount - 1 do
        if ((dword(code) > moduleCache[i1].codeBegin) and (dword(code) < moduleCache[i1].codeEnd)) or
           ((dword(code) > moduleCache[i1].dataBegin) and (dword(code) < moduleCache[i1].dataEnd)) then begin
          // the module to which this function belongs is already in the cache
          module := i1;
          break;
        end;
      if (module = -1) and
         (VirtualQuery(code, mi, sizeOf(mi)) = sizeOf(mi)) and
         (mi.State = MEM_COMMIT) and (mi.AllocationBase <> nil) then
        // the module is not already in the cache, so let's add it
        AddModule(dword(mi.AllocationBase), module, tryRead_);
    end;
    if (module = -1) or (moduleCache[module].handle = HInstance) or (moduleCache[module].mapFile = nil) then
      fi := ParseFunction_(code, tryRead_, nil, nil, nil, nil, nil)
    else
      with moduleCache[module].mapFile do
        fi := ParseFunction_(code, tryRead_,
                             pointer(FindPublic(true, 'System', '@HandleAnyException')),
                             pointer(FindPublic(true, 'System', '@HandleOnException')),
                             pointer(FindPublic(true, 'System', '@HandleAutoException')),
                             pointer(FindPublic(true, 'System', '@HandleFinally')),
                             pointer(FindPublic(true, 'System', '@Halt0')));
    if (not fi.IsValid) and force then begin
      // we fake a valid function, if this is the top item (exception) itself
      fi.EntryPoint := code;
      fi.CodeBegin  := code;
      fi.CodeLen    := 0;
      fi.IsValid    := true;
    end;
    if fi.IsValid then begin
      // the disassembling worked fine
      if (DispatchMessageA = nil) and (GetVersion and $80000000 <> 0) then
        // we need this to fix a stack hole in win9x (see below)
        DispatchMessageA := GetImageProcAddress(GetModuleHandle(user32), 'DispatchMessageA');
      for i1 := 0 to high(fi.FarCalls) do
        if (DispatchMessageA <> nil) and (fi.FarCalls[i1].Target = DispatchMessageA) then begin
          // in win9x a call to DispatchMessageA results in a broken stack trace
          // we "fix" this by evaluating a call to DispatchMessageA as an unknown call
          i2 := Length(fi.UnknownTargets);
          SetLength(fi.UnknownTargets, i2 + 1);
          fi.UnknownTargets[i2].Call      := true;
          fi.UnknownTargets[i2].CodeAddr1 := fi.FarCalls[i1].CodeAddr1;
          fi.UnknownTargets[i2].CodeAddr2 := fi.FarCalls[i1].CodeAddr2;
        end;
      // now let's add the function to our cache
      if functionCount = Length(functionCache) then
        if functionCount = 0 then
             SetLength(functionCache, 16)
        else SetLength(functionCache, functionCount * 2);
      result := functionCount;
      inc(functionCount);
      functionCache[result].module   := module;
      functionCache[result].fi       := fi;
      functionCache[result].approved := approved;
      // if the name is not yet known, we try to retrieve it
      if name = '' then
           functionCache[result].name := GetFunctionName(module, code)
      else functionCache[result].name := name;
      for i1 := 0 to high(fi.FarCalls) do
        if not fi.FarCalls[i1].Call then
          AddFunction(fi.FarCalls[i1].Target, '', -1, approved, false, tryRead_);
{          if (i2 <> -1) and fi.FarCalls[i1].Call then
            with functionCache[i2].fi do
              for i2 := 0 to high(UnknownTargets) do
                if not UnknownTargets[i2].Call then begin
                  SetLength(fi.UnknownTargets, 1);
                  fi.UnknownTargets[0].Call := false;
                  fi.UnknownTargets[0].CodeAddr1 := fi.FarCalls[i1].CodeAddr1;
                  fi.UnknownTargets[0].CodeAddr2 := fi.FarCalls[i1].CodeAddr2;
                  functionCache[result].fi := fi;
                  break;
                end;
        end;
{          i2 := Length(pfi^.FarCalls);
          SetLength(pfi^.FarCalls, i2 + Length(fi.FarCalls));
          for i1 := 0 to high(fi.FarCalls) do
            if not fi.FarCalls[i1].Call then begin
              pfi^.FarCalls[i2] := fi.FarCalls[i1];
              inc(i2);
            end;
          SetLength(pfi^.FarCalls, i2);
          i2 := Length(pfi^.UnknownTargets);
          SetLength(pfi^.UnknownTargets, i2 + Length(fi.UnknownTargets));
          for i1 := 0 to high(fi.UnknownTargets) do
            if not fi.UnknownTargets[i1].Call then begin
              pfi^.UnknownTargets[i2] := fi.UnknownTargets[i1];
              inc(i2);
            end;
          SetLength(pfi^.UnknownTargets, i2);
          for i1 := 0 to high(fi.FarCalls) do
            if not fi.FarCalls[i1].Call then
              AddFunction(fi.FarCalls[i1].Target, '', -1, approved, false, false, pfi); }
    end else result := -1;
  end else result := -1;
end;

function CheckFunctionAreas(func: integer; afterInstr: pointer) : boolean;
// check whether the specified "afterInstr" address lies within the code
// areas of the specified function
var i1 : integer;
begin
  result := false;
  if func <> -1 then
    with functionCache[func], fi do
      if not invalid then
        for i1 := 0 to high(CodeAreas) do
          if (dword(afterInstr) >  dword(CodeAreas[i1].AreaBegin)    ) and
             (dword(afterInstr) <= dword(CodeAreas[i1].AreaEnd  ) + 1) then begin
            result := true;
            break;
          end;
end;

function FindCodeAddrAndTarget(func: integer; beforeInstr: pointer;
                               var afterInstr: pointer; topItem: boolean;
                               var hasTarget: boolean; var codeAddr, target: pointer;
                               tryRead_: dword) : boolean;
// check whether the specified function or one of it's jump children has
// a call/jmp with the specified "afterInstr" address
var i1, i2, i3 : integer;
    ci         : TCodeInfo;
begin
  result := false;
  if func <> -1 then
    if not functionCache[func].invalid then
      if topItem then begin
        // we're checking the top item itself
        // that is either the exception location or the current thread's eip
        // in this case we can't just check the calls
        // first of all we must find out whether the item lies inside of our
        // function's code areas
        if functionCache[func].fi.CodeLen > 0 then begin
          if beforeInstr <> nil then
            // this is a delphi internal exception
            // we check whether it comes from a "raise ..." call
            // in that case we use this raise call as the exception address
            for i2 := 0 to high(functionCache[func].fi.FarCalls) do
              if functionCache[func].fi.FarCalls[i2].CodeAddr2 = beforeInstr then begin
                i3 := AddFunction(functionCache[func].fi.FarCalls[i2].Target, '', -1, false, false, tryRead_);
                if (i3 <> -1) and
                   ( (PosText('@RaiseExcept', functionCache[i3].Name) > 0) or
                     (PosText('Error',        functionCache[i3].Name) > 0)    ) then begin
                  // bingo
                  result := true;
                  codeAddr   := functionCache[func].fi.FarCalls[i2].CodeAddr1;
                  afterInstr := functionCache[func].fi.FarCalls[i2].CodeAddr2;
                  break;
                end;
              end;
          if not result then
            for i1 := 0 to high(functionCache[func].fi.CodeAreas) do
              if (dword(afterInstr) >  dword(functionCache[func].fi.CodeAreas[i1].AreaBegin)    ) and
                 (dword(afterInstr) <= dword(functionCache[func].fi.CodeAreas[i1].AreaEnd  ) + 1) then begin
                // the item does lie inside of our function's code areas
                // so we check whether the item fits to the code
                // if not, the function is probably invalid
                // or our top item is strange, perhaps due to a random jump
                ci.Next := functionCache[func].fi.CodeAreas[i1].AreaBegin;
                repeat
                  ci := ParseCode_(ci.Next, tryRead_);
                until dword(ci.Next) >= dword(afterInstr);
                result := ci.Next = afterInstr;
                if result then
                  // everything fits, so we fill the "codeAddr" return value
                  codeAddr := ci.This;
                break;
              end;
        end else begin
          // this is a faked function, the top item is in unreadable memory
          result := afterInstr = functionCache[func].fi.EntryPoint;
          if result then
            codeAddr := afterInstr;
        end;
      end else begin
        // this is not the top item, but only a secondary call stack item
        // it *must* be directly after a valid call instruction
        // otherwise it's not valid
        for i1 := 0 to high(functionCache[func].fi.UnknownTargets) do
          if functionCache[func].fi.UnknownTargets[i1].CodeAddr2 = afterInstr then begin
            // we found it, it's valid, but the call target is unknown  :-(
            codeAddr  := functionCache[func].fi.UnknownTargets[i1].CodeAddr1;
            target    := nil;
            hasTarget := false;
            result    := true;
            exit;
          end;
        for i1 := 0 to high(functionCache[func].fi.FarCalls) do
          if functionCache[func].fi.FarCalls[i1].CodeAddr2 = afterInstr then begin
            // we found and we even have a valid call target!  :-)
            codeAddr  := functionCache[func].fi.FarCalls[i1].CodeAddr1;
            target    := functionCache[func].fi.FarCalls[i1].Target;
            hasTarget := true;
            result    := true;
            exit;
          end;
{        for i1 := 0 to high(functionCache[func].fi.FarCalls) do
          if not functionCache[func].fi.FarCalls[i1].Call then begin
            // we didn't find it, but this function has some long jumps
            // such long jumps are not part of our call stack array, because
            // long jumps don't have a return address on the stack
            // so we manually follow each long jump:
            i2 := AddFunction(functionCache[func].fi.FarCalls[i1].Target, '', -1, approved, false);
            result := FindCodeAddrAndTarget(i2, beforeInstr, afterInstr, false, hasTarget, codeAddr, target);
            if result then exit;
          end; }
      end;
end;

// ***************************************************************

function StackAddrToStr(addr: pointer; relAddr: boolean = false; relLine: boolean = false) : string;
var s1, s2, s3 : string;
    i1, i2     : integer;
    b1         : boolean;
    pa         : pointer;
begin
  result := RetDelete(IntToHexEx(dword(addr), 8), 1, 1);
  try
    b1 := GetMapFileInfos(addr, s1, s2, s3, pa, i1);
  except b1 := false end;
  if b1 then begin
    if relAddr then
      result := result + ' +' + RetDelete(IntToHexEx(dword(addr) - dword(pa)), 1, 1);
    for i2 := Length(s1) downto 1 do
      if s1[i2] = '\' then begin
        Delete(s1, 1, i2);
        break;
      end;
    result := result + ' ' + s1 + ' ' + s2 + ' ';
    if i1 > 0 then
      result := result + IntToStrEx(i1) + ' ';
    if relLine then begin
      i2 := 0;
      GetLineNumber(pa, i2, pa, pa);
      if i2 >= 0 then
           result := result + '+' + IntToStrEx(i1 - i2) + ' '
      else result := result + '+0 ';
    end;
    result := result + s3;
  end else begin
    if relAddr then
      result := result + ' +0';
    result := result + ' ???';
  end;
end;

function InternalError(func: string; relAddr, relLine: boolean) : string;
type
  // internal type to get access to the current ExceptAddr and ExceptObject
  TRaiseFrame = record
    NextRaise       : pointer;
    ExceptAddr      : pointer;
    ExceptObject    : TObject;
    ExceptionRecord : PExceptionRecord;
  end;
begin
  result := '';
  try
    if RaiseList <> nil then
      result := ':' + #$D#$A + StackAddrToStr(TRaiseFrame(RaiseList^).ExceptAddr, relAddr, relLine);
  except end;
  result := '>> internal error in ' + func + result;
end;

// ***************************************************************

function GetTryFinallyExitRetAddr : pointer;

  function GetTryFinallyExitAddr : pointer;
  asm
    mov eax, offset System.@TryFinallyExit
    cmp word ptr [eax], $25ff
    jnz @done
    mov eax, [eax + 2]
    mov eax, [eax]
   @done:
  end;

var fi : TFunctionInfo;
begin
  fi := ParseFunction(GetTryFinallyExitAddr);
  if Length(fi.UnknownTargets) = 1 then
       result := fi.UnknownTargets[0].CodeAddr2
  else result := nil;
end;

function StackTrace(hideUglyItems     : boolean        = false;
                    showRelativeAddrs : boolean        = false;
                    showRelativeLines : boolean        = false;
                    stackTrace        : TPStackTrace   = nil;
                    currentAddr       : pointer        = nil;
                    isException       : boolean        = false;
                    extException      : boolean        = false;
                    stackBottom       : dword          = 0;
                    stackTop          : dword          = 0;
                    creator           : pointer        = nil;
                    exceptAddr        : TPPointer      = nil;
                    exceptFunc        : TPPointer      = nil;
                    exceptFuncAddr    : TPPointer      = nil;
                    progressAlert     : IProgressAlert = nil;
                    ebp               : dword          = 0;
                    dumbTrace         : boolean        = false;
                    bcbTermination    : boolean        = false;
                    preparedStack     : pointer        = nil;
                    pAbort            : TPBoolean      = nil;
                    pCrc1             : TPCardinal     = nil;
                    pCrc2             : TPCardinal     = nil;
                    pDelphiThread     : TPBoolean      = nil;
                    pMinDebugInfos    : TPDAString     = nil;
                    dontAddTopItem    : boolean        = false) : string;
type
  TStackLink = record
    Link      : integer;  // linked item
    CodeAddr  : pointer;  // code address of this item's call instruction
    HasTarget : boolean;  // is a target available? (only used for down links)
    Target    : pointer;  // call target (only used for down links)
    Func      : integer;  // index into function cache
    Quality   : int64;    // used for final stage of optimal path linking
  end;

  // up/down links to possibly connected stack items
  TStackLinks = array [boolean] of record
    Count : integer;              // number of down links
    Links : array of TStackLink;  // possibly linked stack items
    Best  : integer;              // used for final stage of optimal path linking
  end;

  // our big info collection for each stack item
  TInternalStackItem = record
    AfterInstr   : pointer;                   // this is the address found on the stack
    DontKill     : boolean;                   // this item is either part of a long ebp chain or it's the top item
    TopItem      : boolean;                   // this is either the exception location or the thread's current eip
    Module       : integer;                   // index into module cache
    Links        : TStackLinks;               // up/down links
    Invalid      : boolean;                   // this stack item proved to be incorrect
    FinalOutput  : record                     // for creating the final string output
                     Addr       : pointer;    // code address
                     ModuleName : string;
                     UnitName   : string;
                     Line       : integer;    // line number (0 = unknown)
                     PublicName : string;     // function name
                     PublicAddr : pointer;    // function entry point address
                     PublicLine : integer;    // line number where function begins (0 = unknown)
                   end;
  end;

var stackArr : array of TInternalStackItem;  // this dynamic array holds all possible stack items
    tryRead_ : dword;

  function GetStackAddr : dword;
  // ask the current thread's current stack pointer
  asm
    mov EAX, ESP
  end;

  function GetStackTop : dword;
  // ask the current thread's stack starting address
  asm
    MOV EAX, FS:[4]
  end;

  procedure AddLink(item: integer; down: boolean;
                    link: integer; hasTarget: boolean;
                    codeAddr, target: pointer; func: integer);
  // add a link to the up/down list
  begin
    with stackArr[item].Links[down] do begin
      if Count = Length(Links) then
        if Count = 0 then
             SetLength(Links, 8)
        else SetLength(Links, Count * 2);
      links[Count].Link      := link;
      links[Count].CodeAddr  := codeAddr;
      links[Count].HasTarget := hasTarget;
      links[Count].Target    := target;
      links[Count].Func      := func;
      inc(Count);
    end;
  end;

  procedure DeleteLink(item: integer; down: boolean; index: integer);
  // delete the specified link
  begin
    with stackArr[item].Links[down] do begin
      dec(Count);
      if index < Count then
        Move(Links[index + 1], Links[index], (Count - index) * sizeOf(Links[index]));
    end;
  end;

  procedure InvalidateStackItem(index: integer);
  // delete all references to/of this stack item

    procedure FindAndDeleteLink(item: integer; down: boolean; link, func: integer);
    // looks for specific links and kill them, if any are found
    var i1 : integer;
    begin
      with stackArr[item].Links[down] do
        for i1 := Count - 1 downto 0 do
          if (Links[i1].Link = link) and (Links[i1].Func = func) then
            DeleteLink(item, down, i1);
    end;

  var i1 : integer;
      b1 : boolean;
  begin
    if index <> -1 then
      with stackArr[index] do
        if not dontKill then begin
          // we don't free it to avoid confusing "for" loops
          // instead we only set a flag and remove the references
          invalid := true;
          for b1 := false to true do
            with Links[b1] do begin
              for i1 := 0 to Count - 1 do
                with Links[i1] do
                  if Link <> -1 then
                    FindAndDeleteLink(Link, not b1, index, Func);
              Count := 0;
              Best  := -1;
              Links := nil;
            end;
        end;
  end;

  procedure CollectPossibleStackItems;
  // walk through the whole stack and put those items in our "stackArr"
  // which look like they might be valid call stack items
  var ebpFrames : array of pointer;
      ebpIndex  : integer;
      count     : integer;
      ignore    : pointer;

    function AddStackItem(sa, ai: pointer; force: boolean = false; forceDontKill: boolean = false) : boolean;
    // add the specified stack item, if first tests say that it might be valid

      function CouldBeAfterCall(ai: pointer) : boolean;
      // heuristic test whether the specified "afterInstr" might be
      // an instruction after an asm "call" instruction
      var c4, c8 : cardinal;
          i64    : int64;
      begin
        result := TryRead(pointer(dword(ai) - 8), @i64, 8, tryRead_);
        if result then begin
          c4 := i64 shr 32;
          c8 := dword(i64);
          result := ((c8 and $FF000000) = $E8000000) or
                    ((c4 and $30FF0000) = $10FF0000) or
                    ((c4 and $0030FF00) = $0010FF00) or
                    ((c4 and $000030FF) = $000010FF) or
                    ((c8 and $30FF0000) = $10FF0000) or
                    ((c8 and $0030FF00) = $0010FF00) or
                    ((c8 and $0000FF00) = $00009A00);
        end;
      end;

      function FindNearestExport(module, ai: dword) : integer;
      // find the API which is nearest to the specified afterInstr
      // the entry point is also an option
      var pied : PImageExportDirectory;
          pinh : PImageNtHeaders;
          i1   : integer;
          c1   : dword;
          p1   : pointer;
      begin
        with moduleCache[module] do begin
          p1 := nil;
          pied := GetImageExportDirectory(handle);
          if pied <> nil then
            with pied^ do
              for i1 := 0 to NumberOfFunctions - 1 do begin
                c1 := handle + TPACardinal(handle + AddressOfFunctions)^[i1];
                // we search for the API which is located before our instruction
                // but as near to our instruction as possible
                if (c1 < ai) and (c1 > dword(p1)) then
                  p1 := pointer(c1);
              end;
          pinh := GetImageNtHeaders(handle);
          if pinh <> nil then begin
            c1 := handle + pinh^.OptionalHeader.AddressOfEntryPoint;
            // if our module's entry point is nearer then any API, no problem, either
            if (c1 < ai) and (c1 > dword(p1)) then
              p1 := pointer(c1);
          end;
          // now we add the found API (or entry point) to our function cache
          // and return the index to the function cache
          result := AddFunction(p1, '', module, true, false, tryRead_);
        end;
      end;

    var mi             : TMemoryBasicInformation;
        bi             : pointer;  // beforeInstr
        i1, i2, i3, i4 : integer;
        ca, tg         : pointer;  // codeAddr + target
        hc, ht         : boolean;  // hasCodeAddr + hasTarget
        p1             : pointer;
        i64            : int64;
    begin
      ca := nil;
      if ai = ignore then begin
        result := false;
        exit;
      end;
      // force = are we about to add the top item itself?
      force := force or (isException and (ai <> nil) and (ai = currentAddr));
      if force then begin
        // this is a thread's current eip, so "after" is actually "before"
        bi := ai;
        // now try to get the correct "after"
        if TryRead(bi, @i64, 8, tryRead_) then
          with ParseCode_(bi, tryRead_) do
            if IsValid then
              ai := Next;
        ca := bi;
      end else
        // for first tests we just guess the "before" address to be "after" - 1
        // this might be guessed right or it's *inside* of the "beforeInstr"
        // which is good enough for now
        bi := pointer(dword(ai) - 1);
      // the thread's creator also ends up in "force"
      if (count = 0) and (creator <> nil) then
        force := true;
      // add the stack item to our list if it
      // (1) lies in an allocated memory area and
      // (2) seems to be after a call instruction (heuristic test) and
      // (3) is either
      //     (a) outside of any module or
      //     (b) is in the module's code or data area
      i1 := -1;
      result := (VirtualQuery(bi, mi, sizeOf(mi)) = sizeOf(mi)) and
                (mi.State = MEM_COMMIT) and (mi.AllocationBase <> nil) and
                (dword(ai) > 7) and
                (force or ((*(dword(ai) - dword(mi.BaseAddress) > 7) and*) CouldBeAfterCall(ai))) and
                ( (not AddModule(dword(mi.AllocationBase), i1, tryRead_)) or
                  ((dword(ai) > moduleCache[i1].dataBegin) and (dword(ai) <= moduleCache[i1].dataEnd)) or
                  ((dword(ai) > moduleCache[i1].codeBegin) and (dword(ai) <= moduleCache[i1].codeEnd))    );
      if force or result then begin
        // first we loop through the ebp frames to see if one fits
        while (ebpIndex >= 0) and (dword(ebpFrames[ebpIndex]) > dword(sa)) do
          dec(ebpIndex);
        i2 := -1;
        tg := nil;
        ht := false;
        if (i1 <> -1) and (dword(ai) > moduleCache[i1].codeBegin) and (dword(ai) <= moduleCache[i1].codeEnd) then begin
          if moduleCache[i1].mapFile <> nil then begin
            // we have a map file and the stack item points into the code area
            // so we *must* find the function name and address
            // if we don't this is no valid stack item
            // furthermore the function's asm code must fit to our stack item
            with moduleCache[i1].mapFile.FindPublic(bi) do
              if IsValid then begin
                i2 := AddFunction(Address, Name, i1, true, false, tryRead_);
                if extException then
                     p1 := nil
                else p1 := bi;
                result := (i2 <> -1) and FindCodeAddrAndTarget(i2, p1, ai, force, ht, ca, tg, tryRead_);
                if (i2 <> -1) and (not result) then
                  with functionCache[i2].fi do
                    for i3 := 0 to high(FarCalls) do
                      if (dword(FarCalls[i3].Target) > dword(EntryPoint)) and
                         ( (dword(FarCalls[i3].Target) < dword(EntryPoint) + dword(CodeLen)) or
                           (moduleCache[i1].mapFile.FindPublic(FarCalls[i3].Target).Address = Address) ) then begin
                        // this is a special case for asm code "call @label"
                        // e.g. SysUtils.FormatBuf uses that
                        i4 := AddFunction(FarCalls[i3].Target, Name + '.@label', i1, true, false, tryRead_);
                        result := (i4 <> -1) and FindCodeAddrAndTarget(i4, p1, ai, force, ht, ca, tg, tryRead_);
                        if result then begin
                          i2 := i4;
                          break;
                        end;
                      end;
              end else
                result := false;
          end else
            if moduleCache[i1].handle <> GetModuleHandle(nil) then begin
              // we have a dll and the stack item points into the code area
              // so we try to locate the API to which this stack item belongs
              // if we find the API, it's asm code must fit to our stack item
              // it's luck whether we find the API, though, it's just guessing
              i2 := FindNearestExport(i1, dword(ai));
              if (i2 <> -1) and CheckFunctionAreas(i2, ai) then
                   result := FindCodeAddrAndTarget(i2, nil, ai, force, ht, ca, tg, tryRead_)
              else i2 := -1;
            end;
        end;
        hc := ca <> nil;
        if (not result) and force then begin
          i2 := -1;
          if (not isException) or extException then begin
            ca := bi;
            hc := true;
            if isException and (not TryRead(bi, @i64, 1, tryRead_)) then begin
              i2 := AddFunction(ai, '', -1, true, true, tryRead_);
              result := true;
            end;
          end;
        end;
        if force or result or forceDontKill or ((ebpIndex >= 0) and (ebpFrames[ebpIndex] = sa)) then begin
          // this is seemingly a correct stack item
          // (or it's the top item, or it's part of the ebp stack frame)
          // so add it to our list
          if count = Length(stackArr) then
            SetLength(stackArr, count * 2);
          with stackArr[count] do begin
            // fill up some values we know for sure
            AfterInstr := ai;
            Module     := i1;
            DontKill   := force or forceDontKill or ((ebpIndex >= 0) and (ebpFrames[ebpIndex] = sa));
            TopItem    := force and ((count <> 0) or (creator = nil));
            Links[true ].Best := -1;
            Links[false].Best := -1;
            if hc then
              // we found the function/API to which this stack item belongs
              // and the asm code fits to our stack item
              // so we set up a down link with the available information
              AddLink(count, true, -1, ht, ca, tg, i2);
          end;
          inc(count);
        end;
      end;
    end;

    procedure InitEbpFrames(ebp: dword);
    var i1 : integer;
        c1 : dword;
    begin
      SetLength(ebpFrames, 64);
      i1 := 0;
      if (ebp + 4 > stackBottom) and (ebp + 4 < stackTop) then begin
        ebpFrames[i1] := pointer(ebp + 4);
        inc(i1);
      end;
      while (ebp + 4 < stackTop) and TryRead(pointer(ebp), @c1, 4, tryRead_) and (c1 > ebp) do begin
        ebp := c1;
        if ebp + 4 > stackBottom then begin
          if i1 = Length(ebpFrames) then
            SetLength(ebpFrames, i1 * 2);
          ebpFrames[i1] := pointer(ebp + 4);
          inc(i1);
        end;
      end;
      SetLength(ebpFrames, i1);
    end;

  var sw  : TPPointer;  // stack walker
      bsi : ^TPreStackItem;
      i1  : integer;
  begin
    count := 0;
    ignore := GetTryFinallyExitRetAddr;
    ebpIndex := -1;
    ebpFrames := nil;
    stackArr := nil;
    SetLength(stackArr, 64);
    if creator <> nil then
      // first we insert the creator of this thread
      AddStackItem(nil, creator);
    if (preparedStack = nil) and (ebp <> 0) then begin
      // after that we set up the ebp stack frame chain
      // the creator of our thread is definately not part of the ebp frame
      // so we do it in this order
      InitEbpFrames(ebp);
      ebpIndex := high(ebpFrames);
    end;
    if preparedStack <> nil then begin
      i1 := 0;
      dword(bsi) := dword(preparedStack) + 4;
      while (i1 < integer(preparedStack^)) and
            ((count = 0) or (not stackArr[count - 1].TopItem)) do begin
        if (pAbort <> nil) and pAbort^ then exit;
        AddStackItem(nil, bsi^.afterInstr, false, bsi^.dontKill);
        if progressAlert <> nil then
          progressAlert.Position := i1 * 25 div integer(preparedStack^);
        inc(bsi);
        inc(i1);
      end;
    end else begin
      sw := pointer(stackTop - 4);
      while (dword(sw) >= stackBottom) and
            ((count = 0) or (not stackArr[count - 1].TopItem)) do begin
        if (pAbort <> nil) and pAbort^ then exit;
        if AddStackItem(sw, sw^) and stackArr[count - 1].DontKill and (not stackArr[count - 1].TopItem) then
          // this item seems to be part of the stack frame chain
          // in this case the next 4 bytes belong to the chain structure
          dec(sw);
        dec(sw);
        if progressAlert <> nil then
          progressAlert.Position := (stackTop - dword(sw)) * 25 div (stackTop - stackBottom);
      end;
    end;
    if (not dontAddTopItem) and ((count = 0) or (not stackArr[count - 1].TopItem)) then
      // the top item was not part of the stack trace, so we add it manually
      AddStackItem(nil, currentAddr, true)
    else
      // the top end of the stack trace was properly detected
      // we clear "preparedStack" to signal that we don't need to do post processing
      preparedStack := nil;
    SetLength(stackArr, count);
  end;

  procedure RemoveRecursiveAreas(fullInvalidate: boolean);
  // remove recursive stack areas

    function RemoveRecursiveArea(index: integer) : boolean;

      function FindValidStackArr(var index: integer) : boolean;
      var i1 : integer;
      begin
        for i1 := index - 1 downto 0 do
          if not stackArr[i1].Invalid then begin
            result := true;
            index  := i1;
            exit;
          end;
        result := false;
      end;

    var i1, i2, i3, i4 : integer;
        b1             : boolean;
        ai             : pointer;
        rd             : integer;  // recursive depth >= 0
    begin
      result := false;
      ai := stackArr[index].AfterInstr;
      rd := 0;
      for i1 := index - 1 downto 0 do
        if not stackArr[i1].Invalid then begin
          if stackArr[i1].AfterInstr = ai then begin
            // the same item is stored twice on the stack
            // this can be recursive - or not
            if i1 >= rd then begin
              b1 := true;
              i3 := index;
              i4 := i1;
              for i2 := 1 to rd do
                if (not FindValidStackArr(i3)) or
                   (not FindValidStackArr(i4)) or
                   (stackArr[i3].AfterInstr <> stackArr[i4].AfterInstr) then begin
                  // no, it's no recursive area
                  b1 := false;
                  break;
                end;
              if b1 then begin
                i3 := index;
                for i2 := 0 to rd do begin
                  // now we can safely invalidate the recursive element
                  stackArr[i3].dontKill := false;
                  if fullInvalidate then
                       InvalidateStackItem(i3)
                  else stackArr[i3].Invalid := true;
                  // get the next valid stack item
                  FindValidStackArr(i3);
                end;
                result := true;
              end;
            end;
            break;
          end;
          inc(rd);
        end;
    end;

  var i1 : integer;
      b1 : boolean;
  begin
    repeat
      b1 := true;
      // check all items
      for i1 := high(stackArr) downto 1 do begin
        if (pAbort <> nil) and pAbort^ then exit;
        if (not stackArr[i1].Invalid) and RemoveRecursiveArea(i1) then
          b1 := false;
      end;
      // repeat until no recursive areas are found, anymore
    until b1;
  end;

  procedure InDeepCheck(index: integer);
  // check the specified item in deep, collect a lot of additional information
  // sort out items, which are probably invalid

    procedure InvalidateFunction(func: integer);
    // invalidate the function and remove all links that reference it
    var i1, i2 : integer;
        b1     : boolean;
    begin
      if func <> -1 then begin
        // we don't delete the function, we keep it with an "invalid" flag
        // the information that a function is invalid, is still precious
        functionCache[func].invalid := true;
        // but we kill all links that built on this invalid function
        for i1 := 0 to high(stackArr) do
          for b1 := false to true do
            with stackArr[i1].Links[b1] do
              for i2 := 0 to Count - 1 do
                if Links[i2].Func = func then
                  DeleteLink(i1, b1, i2);
      end;
    end;

    function FindApprovedDownLink : integer;
    // return the first found approved link, if any
    var i1 : integer;
    begin
      result := -1;
      with stackArr[index].Links[true] do
        for i1 := 0 to Count - 1 do
          if (Links[i1].Func <> -1) and functionCache[Links[i1].Func].approved then begin
            result := i1;
            break;
          end;
    end;

    procedure FillUpLink(hasTarget: boolean; codeAddr, target: pointer; approved: boolean);
    // fill the up link list with possible (but not sure) links
    var i1, i2, i3 : integer;
        ca, tg     : pointer;  // codeAddr + target
        ht         : boolean;  // hasTarget
        b1         : boolean;
    begin
      with stackArr[index] do begin
        ca := nil;
        tg := nil;
        if hasTarget then begin
          // we know the target, so let's disassemble it
          // this way we have way more info about which links are possible
          i1 := AddFunction(target, '', -1, approved, false, tryRead_);
          b1 := i1 <> -1;
          if b1 then 
            for i2 := 0 to high(functionCache[i1].fi.UnknownTargets) do
              if not functionCache[i1].fi.UnknownTargets[i2].Call then begin
                // the target function contains an unknown jump
                // so we add unknown up links to the list
                FillUpLink(false, codeAddr, nil, approved);
                break;
              end;
        end else begin
          b1 := true;
          // unknown targets are really bad for a good stack trace
          // but make at least sure that we don't store unknown links twice
          for i1 := 0 to Links[false].Count - 1 do
            if Links[false].Links[i1].Func = -1 then begin
              b1 := false;
              break;
            end;
          i1 := -1;
        end;
        if b1 then
          // we either have a valid target with a valid disassembler information
          // or we have an unknown target which was not there before
          // so we check all items above us about whether we could link to them
          for i2 := index + 1 to high(stackArr) do
            with stackArr[i2] do begin
              if (not invalid) and
                 ((i1 = -1) or FindCodeAddrAndTarget(i1, nil, AfterInstr, TopItem, ht, ca, tg, tryRead_)) then begin
                // if we have an unknown target we can always link (sigh)
                // if it's a known target, we actually found a probable link
                b1 := true;
                if (not approved) and (ca <> nil) then
                  with Links[true] do
                    for i3 := 0 to Count - 1 do
                      if (Links[i3].Func <> -1) and (Links[i3].CodeAddr <> ca) and
                         functionCache[Links[i3].Func].approved then begin
                        // there's a known approved function which contradicts
                        // our guessed function, so drop it
                        b1 := false;
                        break;
                      end;
                if b1 then begin
                  // everything's fine, the item "stackArr[i2]" fits to us
                  AddLink(i2,    true,  index, ht,    ca,       tg,  i1);
                  AddLink(index, false, i2,    false, codeAddr, nil, i1);
                end;
              end;
              if DontKill then
                // only search until the next fixed item
                // cause links can't go further than that!
                break;
            end;
      end;
    end;

  var b1     : boolean;
      i1, i2 : integer;
      ca, tg : pointer;  // codeAddr + target
      ht     : boolean;  // hasTarget
      ci     : TCodeInfo;
      c1     : dword;
      mbi    : TMemoryBasicInformation;
  begin
    with stackArr[index] do begin
      // do we have a down link with an approved (not guessed) function?
      b1 := FindApprovedDownLink <> -1;
      if not b1 then
        // we didn't, so we check through our internal function cache
        for i1 := 0 to functionCount - 1 do
          if functionCache[i1].approved and CheckFunctionAreas(i1, AfterInstr) then
            // we found an approved function, which fits to our stack item
            if FindCodeAddrAndTarget(i1, nil, AfterInstr, TopItem, ht, ca, tg, tryRead_) then begin
              // the function's asm also fits, so set up a down link
              b1 := true;
              AddLink(index, true, -1, ht, ca, tg, i1);
              break;
            end else begin
              // the function's asm did not fit, so invalidate the item
              InvalidateStackItem(index);
              exit;
            end;
      if b1 then begin
        // we have an approved down link
        i1 := FindApprovedDownLink;
        // now let's walk through all down links and kill invalid links
        with Links[true] do begin
          for i2 := 0 to Count - 1 do
            if Links[i1].CodeAddr <> Links[i2].CodeAddr then
              InvalidateFunction(Links[i2].Func);
          // finally set up the up link(s) to other matching stack items
          // if we don't find any, this item looks nice, but isn't true
          // it will be deleted in the next step, not now
          FillUpLink(Links[i1].HasTarget, Links[i1].CodeAddr, Links[i1].Target, true);
        end;
      end else
        // we didn't find an approved function, so we have to guess again  :-(
        // we try all possible call instructions, if one looks promising
        // we try to set up links to other matching stack items
        if (dword(AfterInstr) > 7) and
           (VirtualQuery(pointer(dword(AfterInstr) - 1), mbi, sizeOf(mbi)) = sizeOf(mbi)) and
           (mbi.State = MEM_COMMIT) and
           (dword(mbi.BaseAddress) <= dword(AfterInstr) - 2) then begin
          ca := pointer(dword(AfterInstr) - 7);
          if dword(ca) < dword(mbi.BaseAddress) then
            ca := mbi.BaseAddress;
          for c1 := dword(ca) to dword(AfterInstr) - 2 do begin
            ci := ParseCode_(pointer(c1), tryRead_);
            if (ci.Next = AfterInstr) and ci.Call then
              FillUpLink((ci.PTarget <> nil) or (ci.PPTarget <> nil), pointer(c1), ci.Target, false);
          end;
        end;
    end;
  end;

  procedure RealizeOptimalLinkChain(lo, hi: integer);
  // realize the optimal link chain between the 2 specified items

    function EvaluateLinks(directlyConnected: boolean; first, last: integer) : int64;
    // calculate all possible links (recursively)
    // returned is the highest found quality
    var i1, i2 : integer;
    begin
      result := $1000000000000000;  // neutral
      if (pAbort <> nil) and pAbort^ then exit;
      with stackArr[first], Links[first > last] do
        for i1 := 0 to Count - 1 do begin
          if (pAbort <> nil) and pAbort^ then exit;
          with Links[i1] do
            if Link <> -1 then begin
              if Quality = 0 then begin
                if Link = last then begin
                  if Func <> -1 then
                       Quality := $7000000000000000  // lo/hi connected (target)
                  else Quality := $5000000000000000; // lo/hi connected (unknown)
                end else
                  Quality := EvaluateLinks(directlyConnected and (Func <> -1), Link, last);
                i2 := 0;
                if Func <> -1 then begin
                  inc(i2, 14);
                  if directlyConnected or (Quality and $2000000000000000 <> 0) then
                    // directly connected, good!
                    inc(i2, 14);
                  if functionCache[Func].approved then
                    // approved target link, good!
                    inc(i2, 14);
                end else
                  Quality := Quality and $DFFFFFFFFFFFFFFF;  // lo/hi target -> unknown
                if (Module <> -1) and
                   (stackArr[Link].Module <> -1) and
                   (moduleCache[Module].mapFile <> nil) then begin
                  // does this stack item have a map file?
                  inc(i2, 3);
                  if moduleCache[Module].mapFile.FindLine(pointer(dword(AfterInstr) - 1)) <> 0 then begin
                    // does this stack item have a line number?
                    inc(i2, 3);
                    if (moduleCache[stackArr[Link].Module].mapFile <> nil) and
                       (moduleCache[stackArr[Link].Module].mapFile.FindLine(pointer(dword(stackArr[Link].AfterInstr) - 1)) <> 0) then
                      // this the stack item this item links to have a line number?
                      inc(i2, 3);
                  end;
                end;
                inc(Quality, int64(1) shl i2);  
              end;
              if Quality > result then begin
                result := Quality;
                Best   := i1;
              end;
            end;
        end;
    end;

    procedure RealizeLinks(first, last: integer);
    // validate all items, which belong to the optimal link chain
    var i1 : integer;
    begin
      if (pAbort <> nil) and pAbort^ then exit;
      with stackArr[first].Links[first > last] do begin
        if Best <> -1 then begin
          i1 := Links[Best].Link;
          if i1 <> last then begin
            stackArr[i1].Invalid := false;
            RealizeLinks(i1, last);
          end;
        end;
      end;
    end;

  var q1, q2 : int64;
      i1     : integer;
  begin
    if hi - lo > 1 then begin
      // first of all let's evaluate the up link chain
      // "lo = -1" is a special case to evaluate the bottom stack item
      if lo > -1 then
           q1 := EvaluateLinks(true, lo, hi)
      else q1 := 0;
      if (pAbort <> nil) and pAbort^ then exit;
      // we're done if the up link evaluation was able to connect "lo" and "hi"
      // because in that case the down link eval. would return the same result
      // otherwise we also evaluate the down link chain
      // sometimes it gives a better result as the up link chain
      if q1 and $4000000000000000 = 0 then
           q2 := EvaluateLinks(true, hi, lo)
      else q2 := 0;
      // invalidate all items between "lo" and "hi"
      for i1 := lo + 1 to hi - 1 do
        stackArr[i1].Invalid := true;
      // validate the items that belong to the calculated optimal link chain
      if q1 > q2 then
           RealizeLinks(lo, hi)
      else RealizeLinks(hi, lo);
    end;
  end;

  procedure FillFinalOutput;
  // collect all important information and store it into the "FinalOutput" record

    procedure AddMinDebugInfoItem(moduleName: string; mf: TMapFile);
    var s1 : string;
        i1 : integer;
    begin
      if pMinDebugInfos <> nil then begin
        s1 := mf.MinDebugInfo;
        if s1 <> '' then begin
          s1 := moduleName + '|' + s1;
          for i1 := 0 to high(pMinDebugInfos^) do
            if IsTextEqual(pMinDebugInfos^[i1], s1) then
              exit;
          SetLength(pMinDebugInfos^, Length(pMinDebugInfos^) + 1);
          pMinDebugInfos^[high(pMinDebugInfos^)] := s1;
        end;
      end;
    end;

  var first : boolean;
      index : integer;
      i1    : integer;
      b1    : boolean;
      {$ifdef bcb}
        bcbKillOsHandler : boolean;  // kill "ntdll.KiUserExceptionDispatcher" item
        i2               : integer;
      {$endif}
  begin
    first  := true;
    {$ifdef bcb}
      bcbKillOsHandler := false;
    {$endif}
    for index := high(stackArr) downto 0 do begin
      if (pAbort <> nil) and pAbort^ then exit;
      with stackArr[index], FinalOutput do
        if not invalid then begin
          // first of all let's find the address and function name
          // not as easy as it sounds, since we have to check multiple links
          b1 := true;
          for i1 := 0 to Links[true].Count - 1 do
            with Links[true].Links[i1] do
              if (Func <> -1) and functionCache[Func].approved then begin
                // we only accept function names for approved functions
                // better show no name than a possibly wrong (= confusing) name
                Addr       := CodeAddr;
                PublicName := functionCache[Func].name;
                PublicAddr := functionCache[Func].fi.EntryPoint;
                Unmangle(PublicName, UnitName);
                b1 := false;
                break;
              end;
          if b1 then
            for i1 := 0 to Links[true].Count - 1 do
              if Links[true].Links[i1].CodeAddr <> nil then begin
                // this item was never really 100% approved
                // but we have a valid down link from which we can get the address
                Addr := Links[true].Links[i1].CodeAddr;
                b1 := false;
                break;
              end;
          if b1 then
            if Links[false].Best <> -1 then
                 Addr := Links[false].Links[Links[false].Best].CodeAddr  // valid up link -> code address
            else Addr := AfterInstr;                                     // worst case
          // now the rest is easy
          if Module <> -1 then begin
            // this item is linked to a valid module, so we have a module name
            ModuleName := moduleCache[Module].name;
            if moduleCache[Module].mapFile <> nil then begin
              // also we seemingly have a full map file
              // so let's ask the unit name and line number
              if PublicName = '' then
                with moduleCache[Module].mapFile.FindPublic(addr) do begin
                  PublicName := Name;
                  PublicAddr := Address;
                end;
              UnitName := moduleCache[Module].mapFile.FindSegment(true, addr).Unit_;
              Line     := moduleCache[Module].mapFile.FindLine   (      addr);
              if (Line > 0) and showRelativeLines and (PublicAddr <> nil) then
                PublicLine := moduleCache[Module].mapFile.FindLine(PublicAddr);
              if PublicLine > 0 then begin
                Line := Line - PublicLine;
                inc(PublicLine, Line);
              end;
              AddMinDebugInfoItem(ModuleName, moduleCache[Module].mapFile);
            end;
          end else
            // this item is not linked to any module, can happen
            ModuleName := '???';
          if first and (exceptAddr <> nil) then
            exceptAddr^ := Addr;
          first := false;
          {$ifdef bcb}
            // some bcb stack tweaks
            if bcbTermination and
               ( (PublicName = '_ThrowExceptionLDTC') or
                 (PublicName = '_ReThrowException'  )    ) then begin
              // this is a "abnormal program termination" in BCB
              // the stack contains much too many items, let's cut them off
              bcbTermination := false;
              for i2 := index to high(stackArr) do
                stackArr[i2].invalid := true;
            end;
            if bcbKillOsHandler and (PublicName = 'KiUserExceptionDispatcher') then
              // see comments below
              invalid := true;
            if PublicName <> '' then
              bcbKillOsHandler := false;
            if (preparedStack <> nil) and (PublicName = '__ExceptionHandler') then begin
              // this is a BCB OS exception which is handled in a catch statement
              // if the BCB project was compiled in debug mode, there are some
              // ugly items in the stack which we don't want to see there
              // so we cut them off here
              preparedStack := nil;
              for i2 := index to high(stackArr) - 1 do
                stackArr[i2].invalid := true;
              // if the next valid item is "KiUserExceptionDispatcher": kill it
              bcbKillOsHandler := true;
            end;
          {$endif}
        end;
    end;
  end;

  procedure ComposeResult;
  // take all the collected informations and put them together nicely
  var s1, s2, s3, s4, s5, s6 : string;
      i1, i2, i3, i4, i5, i6 : integer;
      lastAddr               : pointer;
      b1                     : boolean;
      first                  : boolean;
      setExceptFunc          : boolean;
      crc                    : dword;
      cnt                    : integer;
  begin
    result := '';
    i2 := 0;
    i3 := 0;
    i4 := 0;
    i5 := 0;
    i6 := 0;
    cnt := 0;
    b1 := false;//true;
    setExceptFunc := true;
    // first of all calculate the columns' widths
    for i1 := high(stackArr) downto 0 do begin
      if (pAbort <> nil) and pAbort^ then exit;
      with stackArr[i1], FinalOutput do
        if (not invalid) and
           ( TopItem or ((i1 = 0) and (creator <> nil)) or
             ( (PublicName <> '') and
               ((Line > 0) or (not HideUglyItems) or b1) ) ) then begin
          inc(cnt);
          if ShowRelativeAddrs and (PublicAddr <> nil) and (dword(Addr) > dword(PublicAddr)) and
             (Length(IntToHexEx(dword(Addr) - dword(PublicAddr))) + 1 > i2) then
            i2 := Length(IntToHexEx(dword(Addr) - dword(PublicAddr))) + 1;
          if Length(ModuleName) + 1 > i3 then
            i3 := Length(ModuleName) + 1;
          if Length(UnitName) + 1 > i4 then
            i4 := Length(UnitName) + 1;
          if (PublicLine > 0) and (Length(IntToStrEx(PublicLine)) + 1 > i5) then
            i5 := Length(IntToStrEx(PublicLine)) + 1;
          if (PublicLine > 0) or (Line > 0) then begin
            if showRelativeLines and (PublicLine > 0) then begin
              if Length(IntToStrEx(Line)) + 2 > i6 then
                i6 := Length(IntToStrEx(Line)) + 2;
            end else
              if Length(IntToStrEx(Line)) + 1 > i6 then
                i6 := Length(IntToStrEx(Line)) + 1;
            b1 := false;
          end;
          if setExceptFunc and (PublicAddr <> nil) and (PublicLine > 0) then begin
            if exceptFunc <> nil then
              exceptFunc^ := PublicAddr;
            if exceptFuncAddr <> nil then
              exceptFuncAddr^ := Addr;
            setExceptFunc := false;
          end;
        end;
    end;
    // preallocate the column strings
    SetLength(s2, i2);
    SetLength(s3, i3);
    SetLength(s4, i4);
    SetLength(s5, i5); if i5 > 0 then s5[i5] := ' ';
    SetLength(s6, i6); if i6 > 0 then s6[i6] := ' ';
    if stackTrace <> nil then
      SetLength(stackTrace^, cnt);
    cnt := 0;
    lastAddr := nil;
    b1 := false;//true;
    first := true;
    for i1 := high(stackArr) downto 0 do begin
      if (pAbort <> nil) and pAbort^ then exit;
      with stackArr[i1], FinalOutput do
        if (not invalid) and
           ( TopItem or ((i1 = 0) and (creator <> nil)) or
             ( (PublicName <> '') and
               ((Line > 0) or (not HideUglyItems) or b1) and
               ((Addr <> lastAddr) or (result = ''))         ) ) then begin
          if stackTrace <> nil then begin
            stackTrace^[cnt].Addr := Addr;
            if (PublicAddr <> nil) and (dword(Addr) > dword(PublicAddr)) then
                 stackTrace^[cnt].relAddr := dword(Addr) - dword(PublicAddr)
            else stackTrace^[cnt].relAddr := 0;
            stackTrace^[cnt].ModuleName := ModuleName;
            stackTrace^[cnt].UnitName   := UnitName;
            stackTrace^[cnt].Line       := PublicLine;
            if (PublicLine > 0) or (Line > 0) then
                 stackTrace^[cnt].relLine := Line
            else stackTrace^[cnt].relLine := 0;
            stackTrace^[cnt].FunctionName := PublicName;
            inc(cnt);
          end;
          // put each item into the column variables
          if i2 > 0 then begin
            if ShowRelativeAddrs and (PublicAddr <> nil) and (dword(Addr) > dword(PublicAddr)) then begin
              s2 := IntToHexEx(dword(Addr) - dword(PublicAddr), i2 - 2) + ' ';
              //Addr := PublicAddr;
            end else
              s2 := IntToHexEx(dword(0), i2 - 2) + ' ';
            s2[1] := '+';
          end;
          s1 := ModuleName;
          Move(pchar(s1)^, pchar(s3)^, Length(s1));
          for i3 := Length(s1) + 1 to Length(s3) do
            s3[i3] := ' ';
          Move(pchar(UnitName)^, pchar(s4)^, Length(UnitName));
          for i3 := Length(UnitName) + 1 to Length(s4) do
            s4[i3] := ' ';
          if PublicLine > 0 then begin
            s1 := IntToStrEx(PublicLine);
            Move(pchar(s1)^, s5[i5 - Length(s1)], Length(s1));
          end else
            s1 := '';
          for i3 := 1 to i5 - Length(s1) - 1 do
            s5[i3] := ' ';
          if (PublicLine > 0) or (Line > 0) then begin
            s1 := IntToStrEx(Line);
            if showRelativeLines and (PublicLine > 0) then
              s1 := '+' + s1;
            Move(pchar(s1)^, s6[i6 - Length(s1)], Length(s1));
            b1 := false;
          end else
            s1 := '';
          for i3 := 1 to i6 - Length(s1) - 1 do
            s6[i3] := ' ';
          // finally put all the columns together into one result line
          result := result + #$D#$A + Copy(IntToHexEx(dword(Addr), 8), 2, 8) + ' ' + s2 + s3 + s4 + s5 + s6 + PublicName;
          lastAddr := addr;
          if (pCrc1 <> nil) and (pCrc2 <> nil) and (pDelphiThread <> nil) then begin
            if (PublicAddr <> nil) and (ModuleName <> '') and (PublicName <> '') then begin
              crc := dword(Addr) - dword(PublicAddr);
              crc := UpdateCrc32(crc, LowStr(ModuleName)[1], Length(ModuleName));
              if UnitName <> '' then
                crc := UpdateCrc32(crc, LowStr(UnitName)[1], Length(UnitName));
              crc := UpdateCrc32(crc, LowStr(PublicName)[1], Length(PublicName));
            end else
              crc := dword(Addr);
            if first then begin
              pCrc1^ := crc;
              pCrc2^ := crc;
              pDelphiThread^ := Line > 0;
              first  := false;
            end else begin
              pCrc2^ := UpdateCrc32(pCrc2^, crc, 4);
              if Line > 0 then
                pDelphiThread^ := true;
            end;
          end;
          if setExceptFunc and (PublicAddr <> nil) then begin
            if exceptFunc <> nil then
              exceptFunc^ := PublicAddr;
            if exceptFuncAddr <> nil then
              exceptFuncAddr^ := Addr;
            setExceptFunc := false;
          end;
        end;
    end;
    if stackTrace <> nil then
      SetLength(stackTrace^, cnt);
    // remove the leading line break
    Delete(result, 1, 2);
  end;

  {$ifdef log}
    procedure LogResult;
    var fh : dword;

      procedure log(str: string);
      var c1 : dword;
      begin
        SetFilePointer(fh, 0, nil, FILE_END);
        str := str + #$D#$A;
        WriteFile(fh, pointer(str)^, length(str), c1, nil);
      end;

      procedure logStack(index: integer);
      var i1 : integer;
      begin
        log('#' + IntToStrEx(index));
        with stackArr[index] do begin
          log('  Valid         : ' + booleanToChar(not Invalid));
          if DontKill then
            log('  DontKill      : +');
          log('  AfterInstr    : ' + IntToHexEx(dword(AfterInstr), 8));
          if Module <> -1 then
            log('  Module        : ' + moduleCache[Module].name);
          for i1 := 0 to Links[false].Count - 1 do
            with Links[false].Links[i1] do begin
              log('  UpLink:');
              if Link <> -1 then
                log('    Link     : #' + IntToStrEx(Link));
              if Link <> -1 then
                log('    Quality  : ' + IntToHexEx(Quality));
              if CodeAddr <> nil then
                log('    CodeAddr : ' + IntToHexEx(dword(CodeAddr), 8));
              if Func <> -1 then
                with functionCache[Func] do begin
                  log('    Func     : Approved   : ' + booleanToChar(approved));
                  if fi.EntryPoint <> nil then
                    log('               EntryPoint : ' + IntToHexEx(dword(fi.EntryPoint), 8));
                  if Module <> -1 then
                    log('               Module     : ' + moduleCache[Module].name);
                  if name <> '' then
                    log('               Name       : ' + name);
                end;
            end;
          for i1 := 0 to Links[true].Count - 1 do
            with Links[true].Links[i1] do begin
              log('  DownLink:');
              if Link <> -1 then
                log('    Link     : #' + IntToStrEx(Link));
              if Link <> -1 then
                log('    Quality  : ' + IntToHexEx(Quality));
              if CodeAddr <> nil then
                log('    CodeAddr : ' + IntToHexEx(dword(CodeAddr), 8));
              if HasTarget then
                log('    Target   : ' + IntToHexEx(dword(Target), 8));
              if Func <> -1 then
                with functionCache[Func] do begin
                  log('    Func     : Approved   : ' + booleanToChar(approved));
                  if fi.EntryPoint <> nil then
                    log('               EntryPoint : ' + IntToHexEx(dword(fi.EntryPoint), 8));
                  if Module <> -1 then
                    log('               Module     : ' + moduleCache[Module].name);
                  if name <> '' then
                    log('               Name       : ' + name);
                end;
            end;
          log('');
        end;
      end;

    var i1 : integer;
    begin
      fh := CreateFile('c:\desktop\stack.txt', GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_ALWAYS, 0, 0);
      if fh <> INVALID_HANDLE_VALUE then begin
        for i1 := high(stackArr) downto 0 do
          logStack(i1);
        CloseHandle(fh);
      end;
    end;
  {$endif}

  function InternalError2(func: string) : string;
  begin
    result := InternalError(func, showRelativeAddrs, showRelativeLines);
    if isException then
      result := result + #$D#$A +
                '>> original exception location: ' + #$D#$A +
                StackAddrToStr(currentAddr, showRelativeAddrs, showRelativeLines);
  end;

  function GetEbp : dword;
  asm
    mov eax, ebp
  end;

var i1, i2 : integer;
begin
  // initialize return values
  result := '';
  if exceptAddr <> nil then exceptAddr^ := nil;
  if exceptFunc <> nil then exceptFunc^ := nil;
  if exceptFuncAddr <> nil then exceptFuncAddr^ := nil;
  if stackTrace <> nil then stackTrace^ := nil;
  // we don't want a debugger to stop at handled exceptions
  if stackBottom = 0 then stackBottom := GetStackAddr;
  if stackTop    = 0 then stackTop    := GetStackTop;
  if (not isException) and (currentAddr = nil) then
    currentAddr := TPAPointer(GetEbp)^[1];
  if not IsBadReadPtr(pointer(stackBottom), stackTop - stackBottom) then begin
    tryRead_ := StartTryRead;
    try
      // the stack memory seems to be readable, so first of do a rough enumeration
      // of available call stack items
      try
        CollectPossibleStackItems;
      except
        result := InternalError2('CollectPossibleStackItems');
        exit;
      end;
      if (pAbort <> nil) and pAbort^ then exit;
      if progressAlert <> nil then
        progressAlert.Position := 25;
      // remove recursive stack areas
      try
        RemoveRecursiveAreas(true);
      except
        result := InternalError2('RemoveRecursiveStackAreas');
        exit;
      end;
      if (pAbort <> nil) and pAbort^ then exit;
      // now we look for more detailed information about all collected stack items
      // some items might already prove to be invalid
      try
        for i1 := ord(creator <> nil) to high(stackArr) do begin
          if (pAbort <> nil) and pAbort^ then exit;
          if not stackArr[i1].Invalid then
            InDeepCheck(i1);
          if progressAlert <> nil then
            progressAlert.Position := 25 + i1 * 625 div length(stackArr);
        end;
      except
        result := InternalError2('InDeepChecks');
        exit;
      end;
      if (pAbort <> nil) and pAbort^ then exit;
      if progressAlert <> nil then
        progressAlert.Position := 650;
      if not dumbTrace then begin
        // now comes the real work: we go through all the ebp frame chains
        // and search for the optimal link chain between each 2 fixed items
        // fixed items are either ebp frame chain elements or the top item itself
        try
          i2 := high(stackArr);
          for i1 := high(stackArr) - 1 downto 0 do begin
            if (pAbort <> nil) and pAbort^ then exit;
            with stackArr[i1] do
              if DontKill then begin
                RealizeOptimalLinkChain(i1, i2);
                if progressAlert <> nil then
                  progressAlert.Position := 650 + (high(stackArr) - i1) * 350 div length(stackArr);
                i2 := i1;
              end;
          end;
          if (pAbort <> nil) and pAbort^ then exit;
          // also check the items between the lowest fixed item and the bottom item
          RealizeOptimalLinkChain(-1, i2);
        except
          result := InternalError2('RealizeOptimalLinkChains');
          exit;
        end;
      end;
      if (pAbort <> nil) and pAbort^ then exit;
      if progressAlert <> nil then
        progressAlert.Position := 1000;
      // remove recursive stack areas (again)
      try
        RemoveRecursiveAreas(false);
      except
        result := InternalError2('RemoveRecursiveAreas');
        exit;
      end;
      if (pAbort <> nil) and pAbort^ then exit;
      // finally let's fill the "FinalOutput" values of the remaining stack items
      try
        FillFinalOutput;
      except
        result := InternalError2('FillFinalOutput');
        exit;
      end;
      if (pAbort <> nil) and pAbort^ then exit;
      // take all the collected informations and put them together nicely
      try
        ComposeResult;
      except
        result := InternalError2('ComposeResult');
        exit;
      end;
      {$ifdef log}
        // log all stack items to "c:\stack.txt"
        LogResult;
      {$endif}
    finally EndTryRead(tryRead_) end;
  end;
end;

function FastMM_LogStackTrace(preparedStack: pointer; stackTraceDepth: integer; buf: pchar;
                              hideUglyItems, showRelativeAddrs, showRelativeLines, dumbTrace: boolean) : pchar;
var i1  : integer;
    p1  : pointer;
    bsi : ^TPreStackItem;
    s1  : string;
begin
  for i1 := 0 to stackTraceDepth - 1 do
    if TPAInteger(preparedStack)[i1] = 0 then begin
      stackTraceDepth := i1;
      break;
    end;
  p1 := pointer(LocalAlloc(LPTR, 4 + stackTraceDepth * 5));
  TPInteger(p1)^ := stackTraceDepth;
  dword(bsi) := dword(p1) + 4;
  for i1 := stackTraceDepth - 1 downto 0 do begin
    bsi^.dontKill := false;
    bsi^.afterInstr := TPAPointer(preparedStack)[i1];
    inc(bsi);
  end;
  s1 := #$D#$A +
        StackTrace(hideUglyItems, showRelativeAddrs, showRelativeLines, nil, nil, true, false, 0, 0,
                   nil, nil, nil, nil, nil, 0, dumbTrace, false, p1, nil, nil, nil, nil, nil, true);
  LocalFree(dword(p1));
  result := buf;
  Move(pointer(s1)^, result^, Length(s1));
  inc(result, Length(s1));
end;

function PrepareStackTrace(ebp, stackTop, stackBottom: dword;
                           currentAddr: pointer; isException: boolean;
                           var stack: TDAPreStackItem) : integer;
var tryRead_ : dword;

  function CollectPossibleStackItems(ebp: dword) : integer;
  var ebpFrames : array of pointer;
      ebpIndex  : integer;
      count     : integer;

    function AddStackItem(sa, ai: pointer) : boolean;

      function CouldBeAfterCall(ai: pointer) : boolean;
      var c4, c8 : cardinal;
          i64    : int64;
      begin
        result := TryRead(pointer(dword(ai) - 8), @i64, 8, tryRead_);
        if result then begin
          c4 := i64 shr 32;
          c8 := dword(i64);
          result := ((c8 and $FF000000) = $E8000000) or
                    ((c4 and $30FF0000) = $10FF0000) or
                    ((c4 and $0030FF00) = $0010FF00) or
                    ((c4 and $000030FF) = $000010FF) or
                    ((c8 and $30FF0000) = $10FF0000) or
                    ((c8 and $0030FF00) = $0010FF00) or
                    ((c8 and $0000FF00) = $00009A00);
        end;
      end;

    begin
      result := (dword(ai) > 7) and CouldBeAfterCall(ai);
      if result then begin
        while (ebpIndex >= 0) and (dword(ebpFrames[ebpIndex]) > dword(sa)) do
          dec(ebpIndex);
        if count = Length(stack) then
          SetLength(stack, count * 2);
        with stack[count] do begin
          afterInstr := ai;
          dontKill   := (ebpIndex >= 0) and (ebpFrames[ebpIndex] = sa);
        end;
        inc(count);
      end;
    end;

    procedure InitEbpFrames(ebp: dword);
    var i1 : integer;
        c1 : dword;
    begin
      ebpIndex := -1;
      ebpFrames := nil;
      SetLength(ebpFrames, 64);
      i1 := 0;
      if (ebp + 4 > stackBottom) and (ebp + 4 < stackTop) then begin
        ebpFrames[i1] := pointer(ebp + 4);
        inc(i1);
      end;
      while (ebp + 4 < stackTop) and TryRead(pointer(ebp), @c1, 4, tryRead_) and (c1 > ebp) do begin
        ebp := c1;
        if ebp + 4 > stackBottom then begin
          if i1 = Length(ebpFrames) then
            SetLength(ebpFrames, i1 * 2);
          ebpFrames[i1] := pointer(ebp + 4);
          inc(i1);
        end;
      end;
      SetLength(ebpFrames, i1);
      ebpIndex := high(ebpFrames);
    end;

  var sw : TPPointer;
  begin
    InitEbpFrames(ebp);
    count := 1;
    SetLength(stack, 64);
    sw := pointer(stackTop - 4);
    while (dword(sw) >= stackBottom) and ((currentAddr = nil) or (sw^ <> currentAddr)) do begin
      if AddStackItem(sw, sw^) and stack[count - 1].DontKill then
        dec(sw);
      dec(sw);
    end;
    result := count;
  end;

  function GetStackAddr : dword; asm mov eax, esp    end;
  function GetStackTop  : dword; asm mov eax, fs:[4] end;

begin
  result := 0;
  try
    stack := nil;
    if stackBottom = 0 then
      stackBottom := GetStackAddr;
    if stackTop = 0 then
      stackTop := GetStackTop;
    if not isException then
      currentAddr := nil;
    if not IsBadReadPtr(pointer(stackBottom), stackTop - stackBottom) then begin
      tryRead_ := StartTryRead;
      try
        result := CollectPossibleStackItems(ebp);
        if result > 0 then
          dec(result);
      except stack := nil end;
      EndTryRead(tryRead_);
    end;
  except integer(stack) := 0 end;
end;

// ***************************************************************

end.
