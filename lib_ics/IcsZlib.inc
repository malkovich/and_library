{ Interface with zlib dll

  file   : xZlib.inc
  date   : 18/02/2005
  author : Xavier Le Bris
  e-mail : xavier.lebris@free.fr

  see xZlibDll.pas
}

{$DEFINE USE_ZLIB_OBJ}

{$IFNDEF USE_ZLIB_OBJ}

const
   {$IfDef Def_ZLib1Dll}
   ZLibDllName    = 'ZLIB1.DLL';       // for official dll version 1.2.2 with cdecl
   ZLibDllNameBis = 'ZLIBXLB.DLL';
   ZLibDllNameTer = 'ZLIB.DLL';
   {$Else}
   ZLibDllName    = 'ZLIBXLB.DLL';     // for unofficial dll version 1.2.2 with stdcall and assembler (http://www.winimage.com/zLibDll/unzip.html)
   ZLibDllNameBis = 'ZLIB.DLL';        // ZLIBXLB is the same as ZLIBWAPI
   ZLibDllNameTer = 'ZLIB1.DLL';
   {$Endif}

{$ENDIF}

