// ***************************************************************
//  madNVBitmap.pas           version:  1.0c  ·  date: 2006-05-21
//  -------------------------------------------------------------
//  thread safe "non VCL" bitmap functionality
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2006 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2006-05-21 1.0c grayscale pngs were sometimes distorted
// 2006-01-05 1.0b app only screenshots are automatically cropped now 
// 2005-10-09 1.0a (1) some fixes for multi monitor support
//                 (2) INVBitmap.AsBmpStr/AsPngStr added
// 2005-07-09 1.0  initial release

unit madNVBitmap;

{$I mad.inc}

interface

uses Windows, madZip, madTypes;

// ***************************************************************

type
  // which mode shall be used for png saving?
  TPngFormat = (pf256Colors, pf16Grays, pf50kb, pf100kb, pf200kb, pf300kb);

  // the central bitmap interface
  INVBitmap = interface ['{3EE757B5-CC56-4610-A917-E8731737D5BE}']
    function GetWidth  : integer;
    function GetHeight : integer;
    property Width  : integer read GetWidth;
    property Height : integer read GetHeight;

    function Draw (dc: dword; x, y: integer) : boolean;

    function AsBmpStr                                    : string;
    function AsPngStr (format: TPngFormat = pf256Colors) : string;

    function SaveBmp (bmpFile: string                                  ) : boolean;
    function SavePng (pngFile: string; format: TPngFormat = pf256Colors) : boolean;

    function Zoom (width, height: integer) : INVBitmap;
  end;

// load a bitmap file
function LoadBitmap (bmpFile: string) : INVBitmap;

// returns a screenshot of the current screen
function ScreenShot (thisAppOnly: boolean = false) : INVBitmap;

// ***************************************************************

implementation

// ***************************************************************

type
  TINVBitmap = class (TInterfacedObject, INVBitmap)
    FBIH     : TBitmapInfoHeader;
    FBmpH    : dword;
    FOldBmpH : dword;
    FDC      : dword;
    FBits    : pointer;
    constructor Create (const bih: TBitmapInfoHeader; bmpH, oldBmpH, dc: dword; bits: pointer);
    destructor Destroy; override;
    function GetWidth  : integer;
    function GetHeight : integer;
    function Draw (dc: dword; x, y: integer) : boolean;
    function AsBmpStr                                    : string;
    function AsPngStr (format: TPngFormat = pf256Colors) : string;
    function SaveBmp (bmpFile: string) : boolean;
    function SavePng (pngFile: string; format: TPngFormat = pf256Colors) : boolean;
    function Zoom (width, height: integer) : INVBitmap;
  end;

function CreateBmp(iw, ih: integer; var bih: TBitmapInfoHeader; var bmpH, oldBmpH, dc: dword; var bits: pointer) : boolean;
begin
  result := false;
  ZeroMemory(@bih, sizeOf(bih));
  bih.biSize      := sizeOf(bih);
  bih.biWidth     := iw;
  bih.biHeight    := ih;
  bih.biPlanes    := 1;
  bih.biBitCount  := 32;
  bih.biSizeImage := iw * 4 * ih;
  dc := CreateCompatibleDC(0);
  if dc <> 0 then begin
    bits := nil;
    bmpH := CreateDIBSection(0, PBitmapInfo(@bih)^, DIB_RGB_COLORS, pointer(bits), 0, 0);
    result := (bmpH <> 0) and (bits <> nil);
    if result then
         oldBmpH := SelectObject(dc, bmpH)
    else DeleteDC(dc);
  end;
end;

procedure CloseBmp(bmpH, oldBmpH, dc: dword);
begin
  SelectObject(dc, oldBmpH);
  DeleteDC(dc);
  DeleteObject(bmpH);
end;

constructor TINVBitmap.Create(const bih: TBitmapInfoHeader; bmpH, oldBmpH, dc: dword; bits: pointer);
begin
  inherited Create;
  FBIH     := bih;
  FBmpH    := bmpH;
  FOldBmpH := oldBmpH;
  FDC      := dc;
  FBits    := bits;
end;

destructor TINVBitmap.Destroy;
begin
  CloseBmp(FBmpH, FOldBmpH, FDC);
  inherited;
end;

function TINVBitmap.GetWidth : integer;
begin
  result := FBIH.biWidth;
end;

function TINVBitmap.GetHeight : integer;
begin
  result := FBIH.biHeight;
end;

function TINVBitmap.Draw(dc: dword; x, y: integer) : boolean;
begin
  result := BitBlt(dc, x, y, FBIH.biWidth, FBIH.biHeight, FDC, 0, 0, SRCCOPY);
end;

function TINVBitmap.AsBmpStr : string;
type TBitmapFile = packed record
                     bfh  : TBitmapFileHeader;
                     bih  : TBitmapInfoHeader;
                     data : dword;
                   end;
var bf : ^TBitmapFile;
begin
  SetLength(result, sizeOf(TBitmapFileHeader) + sizeOf(FBIH) + FBIH.biSizeImage);
  bf := pointer(result);
  bf.bfh.bfType      := $4d42;  // magic *.bmp file signature
  bf.bfh.bfSize      := sizeOf(TBitmapFileHeader) + sizeOf(FBIH) + FBIH.biSizeImage;
  bf.bfh.bfReserved1 := 0;
  bf.bfh.bfReserved2 := 0;
  bf.bfh.bfOffBits   := sizeOf(TBitmapFileHeader) + sizeOf(FBIH);
  bf.bih := FBIH;
  Move(FBits^, bf.data, FBIH.biSizeImage);
end;

function TINVBitmap.AsPngStr(format: TPngFormat = pf256Colors) : string;
type TPalette = array [0..255] of packed record r, g, b : byte end;
//     TBigPal  = array [0..255] of packed record r, g, b, a : byte end;

  function CreatePng(width, height: dword; palette, bitmap: string) : string;

    function SwapDword(dw: dword) : dword;
    begin
      result := (dw shr 24)               +
                (dw shr  8) and $0000ff00 +
                (dw shl  8) and $00ff0000 +
                (dw shl 24);
    end;

    function GetPngHeader : string;
    var pdw : ^dword;
    begin
      SetLength(result, 8);
      if length(palette) = sizeOf(TPalette) then
           result := result + #$08#$03#00#00#00
      else result := result + #$04#$03#00#00#00;
      pointer(pdw) := pointer(result);
      pdw^ := SwapDword(width);
      inc(pdw);
      pdw^ := SwapDword(height);
    end;

    function CompressBitmap : string;

      function Adler32(buf: PByte; len: integer) : dword;
      const Base = dword(65521); // largest prime smaller than 65536
            NMAX = 3854;         // Code with signed 32 bit integer
      var c1, c2 : dword;
          i1     : integer;
      begin
        c1 := 1;
        c2 := 0;
        while len > 0 do begin
          if len < NMAX then
               i1 := len
          else i1 := NMAX;
          dec(len, i1);
          while i1 > 0 do begin
            inc(c1, buf^);
            inc(c2, c1);
            inc(buf);
            dec(i1);
          end;
          c1 := c1 mod Base;
          c2 := c2 mod Base;
        end;
        result := (c2 shl 16) or c1;
      end;

    var i1  : integer;
        pdw : ^dword;
    begin
      SetLength(result, length(bitmap) * 11 div 10 + 12);
      i1 := Compress(pointer(bitmap), pointer(result), length(bitmap), length(result));
      if i1 > 0 then begin
        SetLength(result, i1);
        result := #$78#$01 + result + 'adlr';
        pointer(pdw) := pchar(result) + length(result) - 4;
        pdw^ := SwapDword(Adler32(pointer(bitmap), length(bitmap)));
      end;
    end;

    procedure AddPngPart(var png: string; name, data: string);
    var pdw : ^dword;
        crc : dword;
    begin
      png := png + 'len ' + name + data + 'crc ';
      pointer(pdw) := pchar(png) + Length(png) - 4 - Length(data) - 8;
      crc := not UpdateCrc32($ffffffff, pointer(dword(pdw) + 4)^, 4 + Length(data));
      pdw^ := SwapDword(Length(data));
      pointer(pdw) := pchar(png) + Length(png) - 4;
      pdw^ := SwapDword(crc);
    end;

  begin
    result := #$89#$50#$4e#$47#$0d#$0a#$1a#$0a;
    AddPngPart(result, 'IHDR', GetPngHeader);
    if palette <> '' then
      AddPngPart(result, 'PLTE', palette);
    AddPngPart(result, 'IDAT', CompressBitmap);
    AddPngPart(result, 'IEND', '');
  end;

  procedure FindOptimalPalette(bmp: pchar; pixels: integer; var palette: TPalette; noOfCols: integer);
  //  C Implementation of Wu's Color Quantizer (v. 2)
  //  (see Graphics Gems vol. II, pp. 126-133)
  //
  //    Author:	Xiaolin Wu
  //    Dept. of Computer Science
  //    Univ. of Western Ontario
  //    London, Ontario N6A 5B7
  //    wu@csd.uwo.ca
  //
  //  Algorithm: Greedy orthogonal bipartition of RGB space for variance
  //       minimization aided by inclusion-exclusion tricks.
  //       For speed no nearest neighbor search is done. Slightly
  //       better performance can be expected by more sophisticated
  //       but more expensive versions.
  //
  // The author thanks Tom Lane at Tom_Lane@G.GP.CS.CMU.EDU for much of
  // additional documentation and a cure to a previous bug.
  //
  // Free to distribute, comments and suggestions are appreciated.
  type
    TADouble  = array [0..maxInt shr 3 - 1] of double;
    TPADouble = ^TADouble;
    TDir = (dRed, dGreen, dBlue);
    TBox = record
      r0, r1 : integer;  // x0 = min value, exclusive
      g0, g1 : integer;  // x1 = max value, inclusive
      b0, b1 : integer;
      vol    : integer;
    end;
    T333Integer = array [0..32, 0..32, 0..32] of integer;
    T333Double  = array [0..32, 0..32, 0..32] of double;
  var m2             : ^T333Double;
      wt, mr, mg, mb : ^T333Integer;

    procedure Hist3d(wt, mr, mg, mb: TPAInteger; m2: TPADouble);
    // build 3-D color histogram of counts, r/g/b, c^2
    var ind, r, g, b  : integer;
        inr, ing, inb : integer;
        table         : array [0..255] of integer;
        i             : integer;
    begin
      for i := 0 to 255 do
        table[i] := i * i;
      for i := 0 to pixels - 1 do begin
        r := byte(bmp[2]);
        g := byte(bmp[1]);
        b := byte(bmp[0]);
        inc(bmp, 4);
        inr := r shr 3 + 1;
        ing := g shr 3 + 1;
        inb := b shr 3 + 1;
        ind := inr shl 10 + inr shl 6 + inr + ing shl 5 + ing + inb;  // ind := [inr][ing][inb]
        inc(wt[ind]);
        inc(mr[ind], r);
        inc(mg[ind], g);
        inc(mb[ind], b);
        m2[ind] := m2[ind] + (table[r] + table[g] + table[b]);
      end;
    end;

    procedure M3d(wt, mr, mg, mb: TPAInteger; m2: TPADouble);
    // compute cumulative moments.
    var ind1, ind2                   : word;
        i, r, g, b                   : byte;
        line, line_r, line_g, line_b : integer;
        area, area_r, area_g, area_b : array [0..32] of integer;
        line2                        : double;
        area2                        : array [0..32] of double;
    begin
      for r := 1 to 32 do begin
        for i := 0 to 32 do begin
          area   [i] := 0;
          area_r [i] := 0;
          area_g [i] := 0;
          area_b [i] := 0;
          area2  [i] := 0;
        end;
        for g := 1 to 32 do begin
          line   := 0;
          line_r := 0;
          line_g := 0;
          line_b := 0;
          line2  := 0;
          for b := 1 to 32 do begin
            ind1 := r shl 10 + r shl 6 + r + g shl 5 + g + b;  // ind1 := [r][g][b]
            line := line + wt[ind1];
            line_r := line_r + mr[ind1];
            line_g := line_g + mg[ind1];
            line_b := line_b + mb[ind1];
            line2  := line2 + m2[ind1];
            area[b] := area[b] + line;
            area_r[b] := area_r[b] + line_r;
            area_g[b] := area_g[b] + line_g;
            area_b[b] := area_b[b] + line_b;
            area2[b] := area2[b] + line2;
            ind2 := ind1 - 1089;  // ind2 := [r-1][g][b]
            wt[ind1] := wt[ind2] + area[b];
            mr[ind1] := mr[ind2] + area_r[b];
            mg[ind1] := mg[ind2] + area_g[b];
            mb[ind1] := mb[ind2] + area_b[b];
            m2[ind1] := m2[ind2] + area2[b];
          end;
        end;
      end;
    end;

    function Vol(var cube: TBox; var mmt: T333Integer) : integer;
    // Compute sum over a box of any given statistic
    begin
      result := + mmt[cube.r1, cube.g1, cube.b1]
                - mmt[cube.r1, cube.g1, cube.b0]
                - mmt[cube.r1, cube.g0, cube.b1]
                + mmt[cube.r1, cube.g0, cube.b0]
                - mmt[cube.r0, cube.g1, cube.b1]
                + mmt[cube.r0, cube.g1, cube.b0]
                + mmt[cube.r0, cube.g0, cube.b1]
                - mmt[cube.r0, cube.g0, cube.b0];
    end;

    function Bottom(var cube: TBox; dir: TDir; var mmt: T333Integer) : integer;
    // Compute part of Vol(cube, mmt) that doesn't depend on r1, g1, or b1 (depending on dir)
    begin
      case dir of
        dRed   : result := - mmt[cube.r0, cube.g1, cube.b1]
                           + mmt[cube.r0, cube.g1, cube.b0]
                           + mmt[cube.r0, cube.g0, cube.b1]
                           - mmt[cube.r0, cube.g0, cube.b0];
        dGreen : result := - mmt[cube.r1, cube.g0, cube.b1]
                           + mmt[cube.r1, cube.g0, cube.b0]
                           + mmt[cube.r0, cube.g0, cube.b1]
                           - mmt[cube.r0, cube.g0, cube.b0];
        else     result := - mmt[cube.r1, cube.g1, cube.b0]
                           + mmt[cube.r1, cube.g0, cube.b0]
                           + mmt[cube.r0, cube.g1, cube.b0]
                           - mmt[cube.r0, cube.g0, cube.b0];
      end;
    end;

    function Top(var cube: TBox; dir: TDir; pos: integer; var mmt: T333Integer) : integer;
    // Compute remainder of Vol(cube, mmt), substituting pos for r1, g1, or b1 (depending on dir)
    begin
      case dir of
        dRed   : result := + mmt[pos, cube.g1, cube.b1]
                           - mmt[pos, cube.g1, cube.b0]
                           - mmt[pos, cube.g0, cube.b1]
                           + mmt[pos, cube.g0, cube.b0];
        dGreen : result := + mmt[cube.r1, pos, cube.b1]
                           - mmt[cube.r1, pos, cube.b0]
                           - mmt[cube.r0, pos, cube.b1]
                           + mmt[cube.r0, pos, cube.b0];
        else     result := + mmt[cube.r1, cube.g1, pos]
                           - mmt[cube.r1, cube.g0, pos]
                           - mmt[cube.r0, cube.g1, pos]
                           + mmt[cube.r0, cube.g0, pos];
      end;
    end;

    function Var_(var cube: TBox) : double;
    // Compute the weighted variance of a box
    var dr, dg, db, xx : double;
    begin
      dr := Vol(cube, mr^);
      dg := Vol(cube, mg^);
      db := Vol(cube, mb^);
      xx := + m2[cube.r1, cube.g1, cube.b1]
            - m2[cube.r1, cube.g1, cube.b0]
            - m2[cube.r1, cube.g0, cube.b1]
            + m2[cube.r1, cube.g0, cube.b0]
            - m2[cube.r0, cube.g1, cube.b1]
            + m2[cube.r0, cube.g1, cube.b0]
            + m2[cube.r0, cube.g0, cube.b1]
            - m2[cube.r0, cube.g0, cube.b0];
      result := xx - (dr * dr + dg * dg + db * db) / Vol(cube, wt^);
    end;

    function Maximize(var cube: TBox; dir: TDir; first, last: integer; var cut: integer;
                      whole_r, whole_g, whole_b, whole_w: integer) : double;
    // We want to minimize the sum of the variances of two subboxes.
    // The sum(c^2) terms can be ignored since their sum over both subboxes
    // is the same (the sum for the whole box) no matter where we split.
    // The remaining terms have a minus sign in the variance formula,
    // so we drop the minus sign and MAXIMIZE the sum of the two terms.
    var half_r, half_g, half_b, half_w : integer;
        base_r, base_g, base_b, base_w : integer;
        i                              : integer;
        temp1, temp2, max              : double;
    begin
      base_r := Bottom(cube, dir, mr^);
      base_g := Bottom(cube, dir, mg^);
      base_b := Bottom(cube, dir, mb^);
      base_w := Bottom(cube, dir, wt^);
      max := 0.0;
      cut := -1;
      for i := first to last - 1 do begin
        half_r := base_r + Top(cube, dir, i, mr^);
        half_g := base_g + Top(cube, dir, i, mg^);
        half_b := base_b + Top(cube, dir, i, mb^);
        half_w := base_w + Top(cube, dir, i, wt^);
        // now half_x is sum over lower half of box, if split at i
        if half_w = 0 then   // subbox could be empty of pixels!
          continue;          // never split into an empty box
        temp1 := half_r * half_r;
        temp1 := temp1 + half_g * half_g;
        temp1 := temp1 + half_b * half_b;
        temp1 := temp1 / half_w;
        half_r := whole_r - half_r;
        half_g := whole_g - half_g;
        half_b := whole_b - half_b;
        half_w := whole_w - half_w;
        if half_w = 0 then   // subbox could be empty of pixels!
          continue;          // never split into an empty box
        temp2 := half_r * half_r;
        temp2 := temp2 + half_g * half_g;
        temp2 := temp2 + half_b * half_b;
        temp2 := temp2 / half_w;
        temp1 := temp1 + temp2;
        if temp1 > max then begin
          max := temp1;
          cut := i;
        end;
      end;
      result := max;
    end;

    function Cut(var set1, set2: TBox) : boolean;
    var dir                                : TDir;
        cutr, cutg, cutb                   : integer;
        maxr, maxg, maxb                   : double;
        whole_r, whole_g, whole_b, whole_w : integer;
    begin
      whole_r := Vol(set1, mr^);
      whole_g := Vol(set1, mg^);
      whole_b := Vol(set1, mb^);
      whole_w := Vol(set1, wt^);
      maxr := Maximize(set1, dRed,   set1.r0 + 1, set1.r1, cutr, whole_r, whole_g, whole_b, whole_w);
      maxg := Maximize(set1, dGreen, set1.g0 + 1, set1.g1, cutg, whole_r, whole_g, whole_b, whole_w);
      maxb := Maximize(set1, dBlue,  set1.b0 + 1, set1.b1, cutb, whole_r, whole_g, whole_b, whole_w);
      if (maxr >= maxg) and (maxr >= maxb) then begin
        dir := dRed;
        if cutr < 0 then begin
          result := false;  // can't split the box
          exit;
        end;
      end else
        if (maxg >= maxr) and (maxg >= maxb) then
          dir := dGreen
        else
          dir := dBlue;
      set2.r1 := set1.r1;
      set2.g1 := set1.g1;
      set2.b1 := set1.b1;
      case dir of
        dRed   : begin
                   set1.r1 := cutr;
                   set2.r0 := cutr;
                   set2.g0 := set1.g0;
                   set2.b0 := set1.b0;
                 end;
        dGreen : begin
                   set1.g1 := cutg;
                   set2.g0 := cutg;
                   set2.r0 := set1.r0;
                   set2.b0 := set1.b0;
                 end;
        else     begin
                   set1.b1 := cutb;
                   set2.b0 := cutb;
                   set2.r0 := set1.r0;
                   set2.g0 := set1.g0;
                 end;
      end;
      set1.vol := (set1.r1 - set1.r0) * (set1.g1 - set1.g0) * (set1.b1 - set1.b0);
      set2.vol := (set2.r1 - set2.r0) * (set2.g1 - set2.g0) * (set2.b1 - set2.b0);
      result := true;
    end;

  type T256Boxes   = array [0..255] of TBox;
       T256Doubles = array [0..255] of double;
  var cube         : ^T256Boxes;
      next         : integer;
      i, weight, k : integer;
      vv           : ^T256Doubles;
      temp         : double;
  begin
    m2   := pointer(LocalAlloc(LPTR or LMEM_ZEROINIT, sizeOf(m2^  )));
    wt   := pointer(LocalAlloc(LPTR or LMEM_ZEROINIT, sizeOf(wt^  )));
    mr   := pointer(LocalAlloc(LPTR or LMEM_ZEROINIT, sizeOf(mr^  )));
    mg   := pointer(LocalAlloc(LPTR or LMEM_ZEROINIT, sizeOf(mg^  )));
    mb   := pointer(LocalAlloc(LPTR or LMEM_ZEROINIT, sizeOf(mb^  )));
    cube := pointer(LocalAlloc(LPTR or LMEM_ZEROINIT, sizeOf(cube^)));
    vv   := pointer(LocalAlloc(LPTR or LMEM_ZEROINIT, sizeOf(vv^  )));
    Hist3d(pointer(wt), pointer(mr), pointer(mg), pointer(mb), pointer(m2));
    M3d   (pointer(wt), pointer(mr), pointer(mg), pointer(mb), pointer(m2));
    cube[0].r0 := 0;
    cube[0].g0 := 0;
    cube[0].b0 := 0;
    cube[0].r1 := 32;
    cube[0].g1 := 32;
    cube[0].b1 := 32;
    next := 0;
    i := 1;
    while i < noOfCols do begin
      if Cut(cube[next], cube[i]) then begin
        if cube[next].vol > 1 then
             vv[next] := Var_(cube[next])
        else vv[next] := 0.0;
        if cube[i].vol > 1 then
             vv[i] := Var_(cube[i])
        else vv[i] := 0.0;
      end else begin
        vv[next] := 0.0;
        dec(i);
      end;
      next := 0;
      temp := vv[0];
      for k := 1 to i do
        if vv[k] > temp then begin
          temp := vv[k];
          next := k;
        end;
      if temp <= 0.0 then begin
        noOfCols := i + 1;
        break;
      end;
      inc(i);
    end;
    LocalFree(dword(vv));
    LocalFree(dword(m2));
    for k := 0 to noOfCols - 1 do begin
      weight := Vol(cube[k], wt^);
      if weight <> 0 then begin
        palette[k].r := Vol(cube[k], mr^) div weight;
        palette[k].g := Vol(cube[k], mg^) div weight;
        palette[k].b := Vol(cube[k], mb^) div weight;
      end;
    end;
    LocalFree(dword(cube));
    LocalFree(dword(wt  ));
    LocalFree(dword(mr  ));
    LocalFree(dword(mg  ));
    LocalFree(dword(mb  ));
  end;

  function InitColorLookup : TDASmallInt;
  var i1 : integer;
  begin
    SetLength(result, 1 shl 15);
    for i1 := 0 to high(result) do
      result[i1] := -1;
  end;

  function ColorLookup(const lookup: TDASmallInt; const pal: TPalette; r, g, b: byte) : byte;
  var i1, i2, i3, i4 : integer;
  begin
    i1 := r shr 3 + (g and $f8) shl 2 + (b and $f8) shl 7;
    if lookup[i1] = -1 then begin
      i3 := maxInt;
      for i2 := 0 to 255 do begin
        i4 := abs(r - pal[i2].r) + abs(g - pal[i2].g) + abs(b - pal[i2].b);
        if i4 < i3 then begin
          lookup[i1] := i2;
          i3 := i4;
        end;
      end;
    end;
    result := lookup[i1];
  end;

  function GetGrayPal(const values: array of byte) : string;
  var i1, i2 : integer;
  begin
    SetLength(result, 3 * length(values) + 3);
    i2 := 0;
    for i1 := 0 to length(values) do
      with TPalette(pointer(result)^)[i1] do begin
        r := i2;
        g := i2;
        b := i2;
        if i1 < length(values) then
          inc(i2, values[i1]);
      end;
  end;

  function InitGrayLookup : TDASmallInt;
  var i1 : integer;
  begin
    SetLength(result, 256);
    for i1 := 0 to high(result) do
      result[i1] := -1;
  end;

  function GrayLookup(const lookup: TDASmallInt; const pal: TPalette; value: byte) : byte;
  var i2, i3, i4 : integer;
  begin
    if lookup[value] = -1 then begin
      i3 := maxInt;
      for i2 := 0 to 15 do begin
        i4 := abs(value - pal[i2].r);
        if i4 < i3 then begin
          lookup[value] := i2;
          i3 := i4;
        end;
      end;
    end;
    result := lookup[value];
  end;

var src, dst   : pchar;
    bmpBuf     : string;
    palBuf     : string;
    iw, ih     : integer;
    i1, i2, i3 : integer;
    b1         : boolean;
    lookup     : TDASmallInt;
    s1         : string;
//    palette    : dword;
//    dc         : dword;
//    bigPal     : ^TBigPal;
begin
  result := '';
  lookup := nil;
  iw := FBIH.biWidth;
  ih := FBIH.biHeight;
  if format <> pf16Grays then begin
    lookup := InitColorLookup;
    SetLength(palBuf, sizeOf(TPalette));

(*    dc := CreateCompatibleDC(0);
    palette := CreateHalftonePalette(dc);
    New(bigPal);
    GetPaletteEntries(palette, 0, 256, bigPal^);
    for i1 := 0 to 255 do begin
      TPalette(pointer(palBuf)^)[i1].r := bigPal[i1].r;
      TPalette(pointer(palBuf)^)[i1].g := bigPal[i1].g;
      TPalette(pointer(palBuf)^)[i1].b := bigPal[i1].b;
    end;
    Dispose(bigPal);
    DeleteObject(palette);
    DeleteDC(dc); *)

    FindOptimalPalette(FBits, iw * ih, TPalette(pointer(palBuf)^), 256);

    SetLength(bmpBuf, iw * ih + ih);
    dst := pointer(bmpBuf);
    for i1 := 0 to ih - 1 do begin
      src := FBits;
      inc(src, iw * 4 * (ih - 1 - i1));
      dst^ := #0;
      inc(dst);
      for i2 := 0 to iw - 1 do begin
        byte(dst^) := ColorLookup(lookup, TPalette(pointer(palBuf)^),
                                  byte(src[2]), byte(src[1]), byte(src[0]));
        inc(dst);
        inc(src, 4);
      end;
    end;
    result := CreatePng(iw, ih, palBuf, bmpBuf);
  end;
  b1 := result = '';
  if not b1 then
    case format of
      pf50kb  : if length(result) >  50 * 1024 then b1 := true;
      pf100kb : if length(result) > 100 * 1024 then b1 := true;
      pf200kb : if length(result) > 200 * 1024 then b1 := true;
      pf300kb : if length(result) > 300 * 1024 then b1 := true;
    end;
  if b1 then begin
    src := FBits;
    for i1 := 1 to ih * iw do begin
      i3 := (integer(byte(src[2])) * 77 + integer(byte(src[1])) * 151 + integer(byte(src[0])) * 28) shr 8;
      byte(src[0]) := i3;
      byte(src[1]) := i3;
      byte(src[2]) := i3;
      inc(src, 4);
    end;
    lookup := InitGrayLookup;
    palBuf := GetGrayPal([28, 24, 21, 19, 17, 16, 15, 15, 15, 15, 14, 14, 14, 14, 14]);
    SetLength(bmpBuf, ((iw + 1) div 2) * ih + ih);
    dst := pointer(bmpBuf);
    for i1 := 0 to ih - 1 do begin
      src := FBits;
      inc(src, (ih - 1 - i1) * iw * 4);
      dst^ := #0;
      inc(dst);
      for i2 := 0 to iw - 1 do begin
        i3 := GrayLookup(lookup, TPalette(pointer(palBuf)^), byte(src[0]));
        if odd(i2) then
             byte(dst^) := byte(dst^) and $f0 + i3
        else byte(dst^) := i3 shl 4;
        if odd(i2) then
          inc(dst);
        inc(src, 4);
      end;
      if odd(iw) then
        inc(dst);
    end;
    s1 := CreatePng(iw, ih, palBuf, bmpBuf);
    if (s1 <> '') and ((result = '') or (Length(s1) < Length(result))) then
      result := s1;
  end;
end;

function TINVBitmap.SaveBmp(bmpFile: string) : boolean;
var fh : dword;
    c1 : dword;
    s1 : string;
begin
  fh := CreateFile(pchar(bmpFile), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0);
  if fh <> INVALID_HANDLE_VALUE then begin
    s1 := AsBmpStr;
    result := WriteFile(fh, pointer(s1)^, length(s1), c1, nil) and (c1 = dword(length(s1)));
    CloseHandle(fh);
    if not result then
      DeleteFile(pchar(bmpFile));
  end else
    result := false;
end;

function TINVBitmap.SavePng(pngFile: string; format: TPngFormat) : boolean;
var s1     : string;
    fh, c1 : dword;
begin
  result := false;
  s1 := AsPngStr(format);
  if s1 <> '' then begin
    fh := CreateFile(pchar(pngFile), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0);
    if fh <> INVALID_HANDLE_VALUE then begin
      result := WriteFile(fh, pointer(s1)^, length(s1), c1, nil) and (c1 = dword(length(s1)));
      CloseHandle(fh);
      if not result then
        DeleteFile(pchar(pngFile));
    end;
  end;
end;

function TINVBitmap.Zoom(width, height: integer) : INVBitmap;

  procedure MitchellResampling32(sw, sh, dw, dh: integer; src, dst: pointer);
  type
    TContributorPixel = record
      pixel  : integer;  // source pixel
      weight : integer;  // weight of this source pixel
    end;
    TPContributorPixel = ^TContributorPixel;

    // list of source pixels contributing to the destination pixel
    TContributorPixels = record
      itemCount : integer;
      items     : TPContributorPixel;
    end;

    // list for the full width/height of a bitmap
    TContributorList  = record
      itemCount : integer;
      items     : array [0..maxInt shr 4 - 1] of TContributorPixels;
    end;
    TPContributorList = ^TContributorList;

    function GetContributorList(src_, dst_: integer) : TPContributorList;
    var scale, center, weight : single;
        capacity              : integer;
        i1, i2                : integer;
        pcp                   : TPContributorPixel;
    begin
      result := pointer(LocalAlloc(LPTR, 4 + dst_ * sizeof(TContributorPixels)));
      result^.itemCount := dst_;
      if dst_ < src_ then begin
        scale := dst_ / src_;
        capacity := (4 * src_ div dst_ + 1) * sizeOf(TContributorPixel);
        for i1 := 0 to dst_ - 1 do
          with result^.items[i1] do begin
            items := pointer(LocalAlloc(LPTR, capacity));
            pcp := items;
            for i2 := ((i1 - 2) * src_ - dst_ + 1) div dst_ to ((i1 + 2) * src_ + dst_ - 1) div dst_ do begin
              weight := abs(i1 - i2 * scale);
              if weight < 2 then begin
                if weight < 1 then
                     pcp^.weight := round((((  7 /  6 * weight - 2) * weight         ) * weight +  8 / 9) * scale * $10000)
                else pcp^.weight := round((((- 7 / 18 * weight + 2) * weight - 10 / 3) * weight + 16 / 9) * scale * $10000);
                if      i2 <  0    then pcp^.pixel := - i2
                else if i2 >= src_ then pcp^.pixel := src_ * 2 - i2 - 1
                else                    pcp^.pixel := i2;
                inc(pcp);
              end;
            end;
            itemCount := (integer(pcp) - integer(items)) div sizeOf(TContributorPixel) - 1;
          end;
      end else begin
        scale := src_ / dst_;
        for i1 := 0 to dst_ - 1 do
          with result^.items[i1] do begin
            items := pointer(LocalAlloc(LPTR, 5 * sizeOf(TContributorPixel)));
            pcp := items;
            center := i1 * scale;
            for i2 := (i1 * src_ - 3 * dst_ + 1) div dst_ to (i1 * src_ + 3 * dst_ - 1) div dst_ do begin
              weight := abs(center - i2);
              if weight < 2 then begin
                if weight < 1 then
                     pcp^.weight := round((((  7 /  6 * weight - 2) * weight         ) * weight +  8 / 9) * $10000)
                else pcp^.weight := round((((- 7 / 18 * weight + 2) * weight - 10 / 3) * weight + 16 / 9) * $10000);
                if      i2 <  0    then pcp^.pixel := - i2
                else if i2 >= src_ then pcp^.pixel := src_ * 2 - i2 - 1
                else                    pcp^.pixel := i2;
                inc(pcp);
              end;
            end;
            itemCount := (integer(pcp) - integer(items)) div sizeOf(TContributorPixel) - 1;
          end;
      end;
    end;

    procedure FreeContributorList(cl: TPContributorList);
    var i1 : integer;
    begin
      for i1 := 0 to cl^.itemCount - 1 do
        LocalFree(dword(cl^.items[i1].items));
      LocalFree(dword(cl));
    end;

  type TRGB = packed record r, g, b: byte end;
  var ix, iy, ic     : integer;
      weight         : integer;
      clx, cly       : TPContributorList;
      fr, fg, fb     : integer;
      sbBits, dbBits : integer;
      sbDif,  dbDif  : integer;
      sbLine, tbBuf  : TPAByte;
      dbPix          : ^byte;
      pcp            : TPContributorPixel;
  begin
    clx := GetContributorList(sw, dw);
    cly := GetContributorList(sh, dh);
    tbBuf  := pointer(LocalAlloc(LPTR or LMEM_ZEROINIT, sh * 4 + sh * 1));
    sbBits := integer(src);
    dbBits := integer(dst);
    sbDif  := sw * 4;
    dbDif  := dw * 4 - 2;
    for ix := 0 to dw - 1 do begin
      dbPix  := pointer(tbBuf );
      sbLine := pointer(sbBits);
      for iy := 0 to sh - 1 do begin
        fr  := 0;
        fg  := 0;
        fb  := 0;
        pcp := clx^.items[ix].items;
        for ic := 0 to clx^.items[ix].itemCount do begin
          weight := pcp^.weight;
          with TRGB(pointer(@sbLine^[pcp^.pixel * 4])^) do begin
            inc(fr, r * weight);
            inc(fg, g * weight);
            inc(fb, b * weight);
          end;
          inc(pcp);
        end;
        if      fr <       0 then dbPix^ := 0
        else if fr > $FF0000 then dbPix^ := 255
        else                      dbPix^ := fr shr 16;
        inc(dbPix);
        if      fg <       0 then dbPix^ := 0
        else if fg > $FF0000 then dbPix^ := 255
        else                      dbPix^ := fg shr 16;
        inc(dbPix);
        if      fb <       0 then dbPix^ := 0
        else if fb > $FF0000 then dbPix^ := 255
        else                      dbPix^ := fb shr 16;
        inc(dbPix, 2);
        inc(integer(sbLine), sbDif);
      end;
      dbPix := pointer(dbBits + ix * 4);
      for iy := 0 to dh - 1 do begin
        fr  := 0;
        fg  := 0;
        fb  := 0;
        pcp := cly^.items[iy].items;
        for ic := 0 to cly^.items[iy].itemCount do begin
          weight := pcp^.weight;
          with TRGB(pointer(@tbBuf^[pcp^.pixel * 4])^) do begin
            inc(fr, r * weight);
            inc(fg, g * weight);
            inc(fb, b * weight);
          end;
          inc(pcp);
        end;
        if      fr <       0 then dbPix^ := 0
        else if fr > $FF0000 then dbPix^ := 255
        else                      dbPix^ := fr shr 16;
        inc(dbPix);
        if      fg <       0 then dbPix^ := 0
        else if fg > $FF0000 then dbPix^ := 255
        else                      dbPix^ := fg shr 16;
        inc(dbPix);
        if      fb <       0 then dbPix^ := 0
        else if fb > $FF0000 then dbPix^ := 255
        else                      dbPix^ := fb shr 16;
        inc(dbPix, dbDif);
      end;
    end;
    LocalFree(dword(tbBuf));
    FreeContributorList(clx);
    FreeContributorList(cly);
  end;

var bih             : TBitmapInfoHeader;
    bmpH, oldBmpH   : dword;
    dc              : dword;
    bits            : pointer;
    height2, width2 : integer;
begin
  height2 := FBIH.biHeight * width  div FBIH.biWidth;
  width2  := FBIH.biWidth  * height div FBIH.biHeight;
  if (height2 > height) or (width2 > width) then
    if (FBIH.biWidth shl 16) div width > (FBIH.biHeight shl 16) div height then
         height := height2
    else width  := width2;
  if CreateBmp(width, height, bih, bmpH, oldBmpH, dc, bits) then begin
    MitchellResampling32(FBIH.biWidth, FBIH.biHeight, width, height, FBits, bits);
    result := TINVBitmap.Create(bih, bmpH, oldBmpH, dc, bits);
  end else
    result := nil;
end;

function DcToBmp(sdc: dword; ix, iy, iw, ih: integer; drawCursor, thisAppOnly: boolean) : INVBitmap;

  procedure DrawCursorOnDC(dc: dword);
  var ci  : TCursorInfo;
      gci : function (var ci: TCursorInfo) : bool; stdcall;
      wnd : dword;
      tid : dword;
      ii  : TIconInfo;
  begin
    ZeroMemory(@ci, sizeOf(ci));
    ci.cbSize := sizeOf(ci);
    gci := GetProcAddress(GetModuleHandle(user32), 'GetCursorInfo');
    if (@gci = nil) or (not gci(ci)) then
      if GetCursorPos(ci.ptScreenPos) then begin
        wnd := WindowFromPoint(ci.ptScreenPos);
        if wnd <> 0 then
             tid := GetWindowThreadProcessID(wnd, nil)
        else tid := 0;
        if tid <> 0 then
          AttachThreadInput(GetCurrentThreadID, tid, true);
        ci.hCursor := GetCursor;
        if tid <> 0 then
          AttachThreadInput(GetCurrentThreadID, tid, false);
      end;
    if (ci.hCursor <> 0) and (ci.flags and CURSOR_SHOWING <> 0) then begin
      if GetIconInfo(ci.hCursor, ii) then begin
        if ii.hbmMask <> 0 then
          DeleteObject(ii.hbmMask);
        if ii.hbmColor <> 0 then
          DeleteObject(ii.hbmColor);
        dec(ci.ptScreenPos.X, ii.xHotSpot);
        dec(ci.ptScreenPos.Y, ii.yHotSpot);
      end;
      DrawIconEx(dc, ci.ptScreenPos.X - ix, ci.ptScreenPos.Y - iy, ci.hCursor, 0, 0, 0, 0, DI_NORMAL);
    end;
  end;

  type
    TSnapshotRec = record
      ix, iy : integer;
      region : dword;
      rect   : TRect;
    end;

  function CalculateArea(window: dword; var snr: TSnapshotRec) : bool; stdcall;
  var pid : dword;
      r1  : TRect;
  begin
    GetWindowThreadProcessId(window, @pid);
    if (pid = GetCurrentProcessId) and IsWindowVisible(window) and (not IsIconic(window)) then begin
      GetWindowRect(window, r1);
      OffsetRect(r1, -snr.ix, -snr.iy);
      if r1.Left   < snr.rect.Left   then snr.rect.Left   := r1.Left;
      if r1.Top    < snr.rect.Top    then snr.rect.Top    := r1.Top;
      if r1.Right  > snr.rect.Right  then snr.rect.Right  := r1.Right;
      if r1.Bottom > snr.rect.Bottom then snr.rect.Bottom := r1.Bottom;
    end;
    result := true;
  end;

  function CalculateRegion(window: dword; var snr: TSnapshotRec) : bool; stdcall;
  var pid : dword;
      r1  : TRect;
      c1  : dword;
  begin
    GetWindowThreadProcessId(window, @pid);
    if (pid = GetCurrentProcessId) and IsWindowVisible(window) and (not IsIconic(window)) then begin
      GetWindowRect(window, r1);
      OffsetRect(r1, -snr.ix, -snr.iy);
      c1 := CreateRectRgnIndirect(r1);
      if snr.region <> 0 then begin
        CombineRgn(snr.region, snr.region, c1, RGN_OR);
        DeleteObject(c1);
      end else
        snr.region := c1;
    end;
    result := true;
  end;

var bih           : TBitmapInfoHeader;
    bmpH, oldBmpH : dword;
    dc            : dword;
    bits          : pointer;
    r1            : TRect;
    snr           : TSnapshotRec;
    b1            : boolean;
begin
  result := nil;
  snr.region := 0;
  if thisAppOnly then begin
    snr.ix := ix;
    snr.iy := iy;
    snr.rect.Left   := maxInt;
    snr.rect.Top    := maxInt;
    snr.rect.Right  := integer($80000000);
    snr.rect.Bottom := integer($80000000);
    EnumWindows(@CalculateArea, integer(@snr));
    if snr.rect.Left < maxInt then begin                
      if snr.rect.Left   < 0  then snr.rect.Left   := 0;
      if snr.rect.Top    < 0  then snr.rect.Top    := 0;
      if snr.rect.Right  > iw then snr.rect.Right  := iw;
      if snr.rect.Bottom > ih then snr.rect.Bottom := ih;
      iw := snr.rect.Right  - snr.rect.Left;
      ih := snr.rect.Bottom - snr.rect.Top;
      ix := ix + snr.rect.Left;
      iy := iy + snr.rect.Top;
      snr.ix := ix;
      snr.iy := iy;
      EnumWindows(@CalculateRegion, integer(@snr));
    end;
  end;
  if ((not thisAppOnly) or (snr.region <> 0)) and CreateBmp(iw, ih, bih, bmpH, oldBmpH, dc, bits) then begin
    if snr.region <> 0 then begin
      r1.Left   := 0;
      r1.Top    := 0;
      r1.Right  := iw;
      r1.Bottom := ih;
      FillRect(dc, r1, GetStockObject(WHITE_BRUSH));
      SelectClipRgn(dc, snr.region);
    end;
    b1 := BitBlt(dc, 0, 0, iw, ih, sdc, ix, iy, SRCCOPY);
    if snr.region <> 0 then begin
      SelectClipRgn(dc, 0);
      DeleteObject(snr.region);
    end;
    if b1 then begin
      if drawCursor then
        DrawCursorOnDC(dc);
      result := TINVBitmap.Create(bih, bmpH, oldBmpH, dc, bits);
    end else
      CloseBmp(bmpH, oldBmpH, dc);
  end else
    if snr.region <> 0 then begin
      SelectClipRgn(dc, 0);
      DeleteObject(snr.region);
    end;
end;

function LoadBitmap(bmpFile: string) : INVBitmap;
var sdc         : dword;
    sbmp, osbmp : dword;
    iw, ih      : integer;
    bi          : ^TBitmap;
begin
  result := nil;
  sbmp := LoadImage(0, pchar(bmpFile), IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE or LR_CREATEDIBSECTION);
  if sbmp <> 0 then begin
    bi := pointer(LocalAlloc(LPTR, sizeOf(TBitmap)));
    if bi <> nil then begin
      if GetObject(sbmp, sizeOf(TBitmap), bi) > 0 then begin
        iw := bi^.bmWidth;
        ih := abs(bi^.bmHeight);
        sdc := CreateCompatibleDC(0);
        if sdc <> 0 then begin
          osbmp := SelectObject(sdc, sbmp);
          result := DcToBmp(sdc, 0, 0, iw, ih, false, false);
          SelectObject(sdc, osbmp);
          DeleteDC(sdc);
        end;
      end;
      LocalFree(dword(bi));
    end;
    DeleteObject(sbmp);
  end;
end;

function ScreenShot(thisAppOnly: boolean = false) : INVBitmap;
const SM_XVIRTUALSCREEN  = 76;
      SM_YVIRTUALSCREEN  = 77;
      SM_CXVIRTUALSCREEN = 78;
      SM_CYVIRTUALSCREEN = 79;
      SM_CMONITORS       = 80;
var sdc    : dword;
    ix, iy : integer;
    iw, ih : integer;
begin
  result := nil;
  sdc := CreateDC('DISPLAY', nil, nil, nil);
  if sdc <> 0 then begin
    if GetSystemMetrics(SM_CMONITORS) > 0 then begin
      ix := GetSystemMetrics(SM_XVIRTUALSCREEN);
      iy := GetSystemMetrics(SM_YVIRTUALSCREEN);
      iw := GetSystemMetrics(SM_CXVIRTUALSCREEN);
      ih := GetSystemMetrics(SM_CYVIRTUALSCREEN);
    end else begin
      ix := 0;
      iy := 0;
      iw := GetDeviceCaps(sdc, HORZRES);
      ih := GetDeviceCaps(sdc, VERTRES);
    end;
    result := DcToBmp(sdc, ix, iy, iw, ih, true, thisAppOnly);
    DeleteDC(sdc);
  end;
end;

// ***************************************************************

end.
