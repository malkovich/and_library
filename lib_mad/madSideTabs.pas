// ***************************************************************
//  madSideTabs.pas           version:  1.0a  ·  date: 2005-09-18
//  -------------------------------------------------------------
//  madExcept IDE wizard: side tabs for the settings dialog
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2005 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2005-09-18 1.0a minor change for big fonts
// 2005-07-09 1.0  initial release

unit madSideTabs;

{$I mad.inc}

interface

uses Windows, ExtCtrls, Controls, Graphics;

// ***************************************************************

type
  TSideTabs = class
  private
    FPanel           : TPanel;
    FTabs            : array of record
                         box     : TRect;
                         panel   : TPanel;
                         name    : string;
                         bmps    : array [0..2] of TBitmap;
                         caption : string;
                       end;
    FBox             : TRect;
    FMouseOverTab    : integer;
    FFocusedTab      : integer;
    FTimerID         : dword;
    FOldPanelWndProc : pointer;
    FNewPanelWndProc : pointer;
    function PanelWndProc (hwnd, msg: dword; wParam, lParam: integer) : integer; stdcall;
  public
    constructor Create (panel: TPanel);
    destructor Destroy; override;
    procedure AddTab  (panel: TPanel; name, caption: string);
    function  FindTab (x, y: integer) : integer;
  end;

// calculate a ligher (dec = true) or darker (dec = false) color
function DecCol (col: dword; dif: dword; dec: boolean) : dword;

// ***************************************************************

implementation

uses Messages, madTools, madTypes, madRes, madStrings, madGraphics;

// ***************************************************************

function DecCol(col: dword; dif: dword; dec: boolean) : dword;
var b1, b2, b3 : byte;
    d1, d2, d3 : byte;
    i1         : integer;
begin
  b1 := byte(col       ); d1 := byte(dif       );
  b2 := byte(col shr  8); d2 := byte(dif shr  8);
  b3 := byte(col shr 16); d3 := byte(dif shr 16);
  i1 := 0;
  if dec then begin
    if dword(b1) + dword(d1) > $ff then begin inc(i1); b1 := $ff end else b1 := b1 + d1;
    if dword(b2) + dword(d2) > $ff then begin inc(i1); b2 := $ff end else b2 := b2 + d2;
    if dword(b3) + dword(d3) > $ff then begin inc(i1); b3 := $ff end else b3 := b3 + d3;
  end else begin
    if b1 < d1 then begin inc(i1); b1 := 0 end else b1 := b1 - d1;
    if b2 < d2 then begin inc(i1); b2 := 0 end else b2 := b2 - d2;
    if b3 < d3 then begin inc(i1); b3 := 0 end else b3 := b3 - d3;
  end;
  if i1 > 1 then
       result := DecCol(col, dif, not dec)
  else result := b3 shl 16 + b2 shl 8 + b1;
end;

// ***************************************************************

constructor TSideTabs.Create(panel: TPanel);
begin
  inherited Create;
  FPanel           := panel;
  FBox.Left        := 0;
  FBox.Top         := 0;
  FBox.Right       := panel.Width - 1;
  FBox.Bottom      := 43;
  FFocusedTab      := 0;
  FMouseOverTab    := -1;
  FNewPanelWndProc := MethodToProcedure(Self, @TSideTabs.PanelWndProc);
  FOldPanelWndProc := pointer(SetWindowLong(FPanel.Handle, GWL_WNDPROC, integer(FNewPanelWndProc)));
end;

destructor TSideTabs.Destroy;
var i1 : integer;
begin
  if FTimerID <> 0 then
    KillTimer(FPanel.Handle, FTimerID);
  SetWindowLong(FPanel.Handle, GWL_WNDPROC, integer(FOldPanelWndProc));
  VirtualFree(FNewPanelWndProc, 0, MEM_RELEASE);
  for i1 := 0 to high(FTabs) do begin
    FTabs[i1].bmps[0].Free;
    FTabs[i1].bmps[1].Free;
    FTabs[i1].bmps[2].Free;
  end;
  inherited;
end;

procedure TSideTabs.AddTab(panel: TPanel; name, caption: string);

  procedure GetBitmap(name: string; var col, half, gray: TBitmap);

    procedure AlphaBlendBitmap(data: pointer; bmp: TBitmap; backColor: dword);
    var src, dst : TPACardinal;
        i1       : integer;
        alpha    : dword;
    begin
      backColor := (backColor and $0000FF) shl 16 +    // swap rgb to bgr
                   (backColor and $00FF00)        +
                   (backColor and $FF0000) shr 16;
      src := data;
      dst := bmp.ScanLine[bmp.Height - 1];
      for i1 := 0 to bmp.Width * bmp.Height - 1 do begin
        alpha := src[i1] shr 24 + 1;
        dst[i1] := ( (      alpha  * (src[i1]   and $00FF00FF) +
                     (256 - alpha) * (backColor and $00FF00FF)   ) shr 8) and $00FF00FF +
                   ( (      alpha  * (src[i1]   and $0000FF00) +
                     (256 - alpha) * (backColor and $0000FF00)   ) shr 8) and $0000FF00;
      end;
    end;

  var c1, c2 : dword;
      ih     : TPIconGroup;
      ii     : integer;
      bi     : PBitmapInfo;
  begin
    col  := nil;
    half := nil;
    gray := nil;
    ii := -1;
    c1 := FindResource(HInstance, pchar(UpStr(name)), RT_GROUP_ICON);
    if c1 <> 0 then begin
      c2 := LoadResource(HInstance, c1);
      if c2 <> 0 then begin
        ih := LockResource(c2);
        ii := ih^.items[0].id;
        UnlockResource(c2);
        FreeResource(c2);
      end;
    end;
    if ii <> -1 then begin
      c1 := FindResource(HInstance, pchar(ii), RT_ICON);
      if c1 <> 0 then begin
        c2 := LoadResource(HInstance, c1);
        if c2 <> 0 then begin
          bi := LockResource(c2);
          if (bi.bmiHeader.biWidth = 32) and (bi.bmiHeader.biHeight in [32, 64]) and
             (bi.bmiHeader.biBitCount = 32) and (bi.bmiHeader.biSizeImage = 32 * 32 * 4 + $80) then begin
            col := TBitmap.Create;
            col.PixelFormat := pf32Bit;
            col.Width  := 32;
            col.Height := 32;
            half := TBitmap.Create;
            half.PixelFormat := pf32Bit;
            half.Width  := 32;
            half.Height := 32;
            gray := TBitmap.Create;
            gray.PixelFormat := pf32Bit;
            gray.Width  := 32;
            gray.Height := 32;
            AlphaBlendBitmap(@bi^.bmiColors, col,  DecCol(GetSysColor(COLOR_BTNFACE), $080808, true ));
            AlphaBlendBitmap(@bi^.bmiColors, half,        GetSysColor(COLOR_BTNFACE)                 );
            AlphaBlendBitmap(@bi^.bmiColors, gray, DecCol(GetSysColor(COLOR_BTNFACE), $080808, false));
            Grayscale(half, gp50);
            Grayscale(gray, gp75);
          end;
          UnlockResource(c2);
          FreeResource(c2);
        end;
      end;
    end;
  end;

var index, i1 : integer;
    dc        : dword;
    ppi       : integer;
begin
  index := Length(FTabs);
  SetLength(FTabs, index + 1);
  FTabs[index].box     := FBox;
  FTabs[index].panel   := panel;
  FTabs[index].name    := name;
  FTabs[index].caption := caption;
  GetBitmap(name, FTabs[index].bmps[2], FTabs[index].bmps[1], FTabs[index].bmps[0]);
  dc := GetDC(0);
  ppi := GetDeviceCaps(dc, LOGPIXELSY);
  ReleaseDC(0, dc);
  FTabs[index].box.Bottom := FTabs[index].box.Bottom * ppi div 96;
  if index > 0 then
    FTabs[index].box.Top := FTabs[index - 1].box.Bottom - 1;
  inc(FBox.Top,    42);
  inc(FBox.Bottom, 42);
  panel.Color := DecCol(GetSysColor(COLOR_BTNFACE), $080808, true);
  for i1 := 0 to panel.ControlCount - 1 do
    if panel.Controls[i1] is TPanel then
      (panel.Controls[i1] as TPanel).Color := panel.Color;
end;

function TSideTabs.FindTab(x, y: integer) : integer;
var r1 : TRect;
    i1 : integer;
begin
  result := -1;
  for i1 := 0 to high(FTabs) do begin
    r1 := FTabs[i1].box;
    PPoint(@r1.Left )^ := FPanel.ClientToScreen(PPoint(@r1.Left )^);
    PPoint(@r1.Right)^ := FPanel.ClientToScreen(PPoint(@r1.Right)^);
    if (x >= r1.Left) and (x <= r1.Right ) and
       (y >= r1.Top ) and (y <= r1.Bottom) then begin
      result := i1;
      break;
    end;
  end;
end;

function TSideTabs.PanelWndProc(hwnd, msg: dword; wParam, lParam: integer) : integer; stdcall;
var i1  : integer;
    r1  : TRect;
    bmp : TBitmap;
    tmp : TBitmap;
    ps  : TPaintStruct;
    dc  : dword;
    tab : integer;
    p1  : TPoint;
begin
  if msg = WM_ERASEBKGND then
    result := 0
  else
    if msg = WM_PAINT then begin
      dc := BeginPaint(hwnd, ps);
      tmp := TBitmap.Create;
      tmp.PixelFormat := pf32Bit;
      tmp.Width  := FTabs[0].box.Right  - FTabs[0].box.Left + 1;
      tmp.Height := FTabs[0].box.Bottom - FTabs[0].box.Top  + 1;
      for i1 := 0 to high(FTabs) do
        with FTabs[i1] do
          if IntersectRect(r1, ps.rcPaint, box) and (not IsRectEmpty(r1)) then begin
            with tmp.Canvas do begin
              Pen.Color := clWindow;
              Pen.Width := 2;
              r1 := box;
              OffsetRect(r1, -r1.Left + 1, -r1.Top + 1);
              if i1 = FFocusedTab then begin
                Brush.Color := DecCol(GetSysColor(COLOR_BTNFACE), $080808, true);
                Font.Color := GetSysColor(COLOR_WINDOWTEXT);
                bmp := Bmps[2];
                inc(r1.Right, 2);
              end else
                if i1 = FMouseOverTab then begin
                  Brush.Color := GetSysColor(COLOR_BTNFACE);
                  Font.Color := DecCol(GetSysColor(COLOR_WINDOWTEXT), $303030, true);
                  bmp := Bmps[1];
                end else begin
                  Brush.Color := DecCol(GetSysColor(COLOR_BTNFACE), $080808, false);
                  Font.Color := DecCol(GetSysColor(COLOR_WINDOWTEXT), $606060, true);
                  bmp := Bmps[0];
                end;
              Rectangle(r1.Left, r1.Top, r1.Right, r1.Bottom);
              InflateRect(r1, -1, -1);
              dec(r1.Right);
              dec(r1.Bottom);
              FillRect(r1);
              r1.Top := r1.Top + (r1.Bottom - 42) div 2;
              if bmp <> nil then
                BitBlt(Handle, r1.Left + 5, r1.Top + 5, 32, 32, bmp.Canvas.Handle, 0, 0, SRCCOPY);
              TextOut(r1.Left + 44, r1.Top +  6, SubStr(caption, 1));
              TextOut(r1.Left + 44, r1.Top + 21, SubStr(caption, 2));
            end;
            BitBlt(dc, box.Left, box.Top, tmp.Width, tmp.Height, tmp.Canvas.Handle, 0, 0, SRCCOPY);
          end;
      tmp.Free;
      EndPaint(hwnd, ps);
      result := 0;
    end else
      if msg = WM_LBUTTONDOWN then begin
        p1.X := word(lParam);
        p1.Y := dword(lParam) shr 16;
        p1 := FPanel.ClientToScreen(p1);
        tab := FindTab(p1.X, p1.Y);
        if (tab <> -1) and (tab <> FFocusedTab) then begin
          r1 := FTabs[FFocusedTab].box;
          InflateRect(r1, 1, 1);
          InvalidateRect(FPanel.Handle, @r1, false);
          FTabs[tab].panel.Visible := true;
          FTabs[FFocusedTab].panel.Visible := false;
          FFocusedTab := tab;
          r1 := FTabs[tab].box;
          InflateRect(r1, 1, 1);
          InvalidateRect(FPanel.Handle, @r1, false);
        end;
        result := 0;
      end else
        if (msg = WM_TIMER) or (msg = WM_MOUSEMOVE) then begin
          GetCursorPos(p1);
          tab := FindTab(p1.X, p1.Y);
          if tab <> FMouseOverTab then begin
            if FMouseOverTab <> -1 then begin
              i1 := FMouseOverTab;
              FMouseOverTab := -1;
              InvalidateRect(FPanel.Handle, @FTabs[i1].box, false);
              KillTimer(FPanel.Handle, FTimerID);
              FTimerID := 0;
            end;
            if (tab <> -1) and (tab <> FFocusedTab) and IsWindowEnabled(FPanel.Parent.Handle) then begin
              FMouseOverTab := tab;
              InvalidateRect(FPanel.Handle, @FTabs[tab].box, false);
              FTimerID := SetTimer(FPanel.Handle, 0, 200, nil);
            end;
          end;
          result := 0;
        end else
          result := CallWindowProc(FOldPanelWndProc, hwnd, msg, wParam, lParam);
end;

// ***************************************************************

end.
