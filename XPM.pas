(*
    Greenfish Icon Editor Pro
    Copyright (c) 2012-13 B. Szalkai

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)
unit XPM;

interface

uses
  LclIntf, LclType,
  SysUtils, Classes, BitmapEx, bmExUtils,
  PixelFormats, Math, StreamEx, StrUtils, Dialogs;

function xpmDetect(Data: Pointer; Size: integer): boolean;

function xpmLoadFromStream(bm: TBitmap32; var HotSpot: TPoint; st: TStream): boolean;
procedure xpmSaveToStream(bm: TBitmap32; const xpmID: string;
  const HotSpot: TPoint; st: TStream);

function xpmLoadFromFile(bm: TBitmap32; var HotSpot: TPoint; const fn: string): boolean;
procedure xpmSaveToFile(bm: TBitmap32; const xpmID: string;
  const HotSpot: TPoint; const fn: string);

implementation

function xpmDetect;
var
  s: string;

begin
  if (Size >= 2) and (PChar(Data)[0] = '/') and (PChar(Data)[1] = '*') then
    Exit(True);
  SetLength(s, Size);
  Move(Data^, s[1], Size);
  Result := (Pos('/* XPM */', s) > 0) or (Pos('char*', s) > 0) or
    (Pos('char *', s) > 0);
end;

function xpmValidID(const s: string): string;
var
  i: integer;

begin
  Result := s;

  for i := 1 to Length(Result) do if not (Result[i] in ['0'..'9', 'A'..'Z',
    'a'..'z', '_']) then Result[i] := '_';
  if (Length(Result) = 0) or (Result[1] in ['0'..'9']) then
    Result := '_' + Result;
end;

const
  cl32XpmNone = cl32Lime;

type
  TxpmStringArray = array of string;

// Splits a whitespace-separated string
procedure xpmSplitString(const s: string; var a: TxpmStringArray);
const
  WhiteSpace: set of char = [#0, #9, #10, #13, #32, #255];

var
  i, Start: integer;

begin
  SetLength(a, 0);

  Start := 0;
  for i := 1 to Length(s) + 1 do
    if (Start <> 0) and ((i > Length(s)) or (s[i] in WhiteSpace)) then
    begin
      SetLength(a, Length(a) + 1);
      a[Length(a) - 1] := Copy(s, Start, i - Start);
      Start := 0;
    end else
    if (Start = 0) and (i <= Length(s)) and not (s[i] in WhiteSpace) then
      Start := i;
end;

// Reads the next quoted character sequence
function xpmReadString(sr: TBufferedReader): string;
var
  c: char;
  CurrEscape, PrevEscape: boolean;
  
begin
  Result := '';

  // find and read the next quoted C-style string
  repeat sr.Read(@c, 1); until c = '"';

  PrevEscape := False;
  while True do
  begin
    sr.Read(@c, 1);
    CurrEscape := False;

    if PrevEscape then
    begin
      case c of
        '0': c := #0;
        'a': c := #7;
        'b': c := #8;
        't': c := #9;
        'n': c := #10;
        'v': c := #11;
        'f': c := #12;
        'r': c := #13;
      end;
      Result += c;
    end else
    case c of
      '"': Break; // end of string
      '\': CurrEscape := True;
      else Result += c;
    end; // if not escaped

    PrevEscape := CurrEscape;
  end;
end;

// Parses an #rr...gg...bb... identifier or a symbolic color name
function xpmStrToColor(s: string): TColor32;
var
  i, cpc: integer;
  b: byte;
  
begin
  Result := cl32XpmNone;
  s := UpperCase(s);

  if s = '' then Exit else
  if s[1] = '#' then
  // rgb code
  begin
    // chars per component
    cpc := (Length(s) - 1) div 3;
    if cpc <> 0 then
    begin
      Result := 0;

      // read red, green and blue component
      for i := 0 to 2 do
      begin
        if cpc = 1 then b := StrToInt('$' + s[2 + i]) shl 4 else
          b := StrToInt('$' + Copy(s, 2 + i*cpc, 2));

        Result := (Result shl 8) or b;
      end;
      
      Result := FlipColor32(Result) or cl32Opaque;
    end; // if cpc <> 0
  end else
  // a selection of symbolic names
  if s = 'BLACK' then Result := cl32Black else
  if s = 'BLUE' then Result := cl32Blue else
  if s = 'GREEN' then Result := cl32Green else
  if s = 'CYAN' then Result := cl32Teal else
  if s = 'RED' then Result := cl32Red else
  if s = 'YELLOW' then Result := cl32Yellow else
  if s = 'MAROON' then Result := cl32Maroon else
  if (s = 'GRAY') or (s = 'GREY') then Result := cl32Gray else
  if s = 'WHITE' then Result := cl32White else
  if (s = 'NONE') or (s = 'TRANSPARENT') then Result := cl32Transparent;
end;

type
  TxpmPalEntry = record
    Key: string;
    Value: TColor32;
  end;

function xpmLoadFromStream;
var
  sr: TBufferedReader;
  i, j, x, y, cpp: integer;
  s, s2, sPix, Pivot: string;
  a: TxpmStringArray;
  Palette: array of TxpmPalEntry;
  tmp: TxpmPalEntry;
  c: TColor32;

  procedure PaletteQuickSort(iLo, iHi: integer);
  var
    Lo, Hi: Integer;

  begin
    repeat
      Lo := iLo;
      Hi := iHi;
      Pivot := Palette[(iLo + iHi) div 2].Key;

      repeat
        while Palette[Lo].Key < Pivot do Inc(Lo);
        while Palette[Hi].Key > Pivot do Dec(Hi);
        if Lo <= Hi then
        begin
          tmp := Palette[Lo];
          Palette[Lo] := Palette[Hi];
          Palette[Hi] := tmp;
          Inc(Lo);
          Dec(Hi);
        end;
      until Lo > Hi;

      if Hi > iLo then PaletteQuickSort(iLo, Hi);
      // if Lo < iHi then PaletteQuickSort(Lo, iHi);
      iLo := Lo;
    until Lo >= iHi;
  end;

  function PaletteLookup(const s: string): TColor32;
  var
    lo, hi, mid: integer;

  begin
    // find the corresponding color
    lo := 0;
    hi := Length(Palette);
    if Palette[lo].Key = s then Exit(Palette[lo].Value);
    while hi - lo > 1 do
    begin
      mid := (lo + hi) div 2;
      if Palette[mid].Key = s then Exit(Palette[mid].Value);
      if Palette[mid].Key < s then lo := mid else hi := mid;
    end;
    Result := cl32XpmNone;
  end;

begin
  Result := False;
  try
    sr := TBufferedReader.Create(st);
    try
      // load the header
      s := xpmReadString(sr);
      xpmSplitString(s, a);

      // apply loaded data
      bm.Resize(StrToInt(a[0]), StrToInt(a[1]));
      SetLength(Palette, StrToInt(a[2]));
      cpp := StrToInt(a[3]);
      if Length(a) >= 6 then
      begin
        TryStrToInt(a[4], HotSpot.X);
        TryStrToInt(a[5], HotSpot.Y);
      end;

      // load the palette
      for i := 0 to Length(Palette) - 1 do
      begin
        s := xpmReadString(sr);

        // memorize character sequence which means this color
        s2 := Copy(s, 1, cpp);
        Delete(s, 1, cpp+1);

        // look for the c (color visual) entry
        xpmSplitString(s, a);
        c := cl32XpmNone;
        for j := 0 to Length(a) - 2 do if UpperCase(a[j]) = 'C' then
        begin
          c := xpmStrToColor(a[j+1]);
          Break;
        end;

        // add entry
        with Palette[i] do
        begin
          Key := s2;
          Value := c;
        end;
      end;

      // sort the palette
      PaletteQuickSort(0, Length(Palette) - 1);

      // load the pixmap
      SetLength(sPix, cpp);
      for y := 0 to bm.Height - 1 do
      begin
        s := xpmReadString(sr);

        for x := 0 to bm.Width - 1 do
        begin
          // load the code
          Move(s[1 + x*cpp], sPix[1], cpp);
          bm.PixelAddr(x, y)^ := PaletteLookup(sPix);
        end; // for x
      end; // for y
    finally
      sr.Free;
    end;

    // success
    Result := True;
  except
  end;
end;

// We can use the characters from #32 to #126, except " and \
// We could escape them, but that may cause trouble in some image editors
const
  XPM_CHAR_COUNT = 126-32+1 - 2;

function xpmGetCharsPerPixel(Count: integer): integer;
begin
  if Count <= 0 then Result := 1 else Result := Ceil(Ln(Count) / Ln(XPM_CHAR_COUNT));
end;

function xpmIndexToStr(Index, cpp: integer): string;
var
  i: integer;
  c: char;

begin
  Result := '';
  for i := 1 to cpp do
  begin
    c := Char(32 + Index mod XPM_CHAR_COUNT);
    // jump over " and \
    if c >= '"' then inc(c);
    if c >= '\' then inc(c);
    Result += c;

    // iterate
    Index := Index div XPM_CHAR_COUNT;
  end;
end;

procedure xpmSaveToStream;
var
  i, x, y, Index, cpp: integer;
  s, s2: string;
  Palette: TiePalette;
  CreateBMNew: boolean;
  bmNew: TBitmap32;
  p: PColor32;

  procedure DoWrite(const s: string);
  begin
    st.WriteBuffer(s[1], Length(s));
  end;

begin
  // we need a palette
  Palette := TiePalette.Create;
  try
    // remove alpha channel
    CreateBMNew := GetPixelFormat32(bm, nil, nil, nil, nil) = pf32_32bit;
    if CreateBMNew then
    begin
      bmNew := TBitmap32.Create;
      bmNew.Assign(bm);
      bmNew.ThresholdAlpha;
    end else
      bmNew := bm;

    // build palette
    p := bmNew.Data;
    for i := 0 to bmNew.Width*bmNew.Height - 1 do
    begin
      if p^ and cl32Opaque <> 0 then Palette.Add(p^ and not cl32Opaque, False);
      inc(p);
    end;

    try
      // +1, because the transparent color is not the part of the palette
      cpp := xpmGetCharsPerPixel(Palette.Count + 1);

      // write header
      DoWrite(Format('/* XPM */'#10'static char *%s[] = {'#10 +
        '"%d %d %d %d %d %d",'#10,
        [xpmValidID(xpmID), bm.Width, bm.Height, Palette.Count + 1, cpp,
          HotSpot.X, HotSpot.Y]));

      // write palette
      for i := 0 to Palette.Count do
      begin
        s := xpmIndexToStr(i, cpp);
        if i < Palette.Count then s2 := '#' + IntToHex(
          FlipColor(Integer(Palette[i] and not cl32Opaque)), 6) else
          s2 := 'None';

        DoWrite(Format('"%s c %s",'#10, [s, s2]));
      end;

      // write bitmap
      for y := 0 to bmNew.Height - 1 do
      begin
        s := '"';

        for x := 0 to bmNew.Width - 1 do
        begin
          p := bmNew.PixelAddr(x, y);

          if PByteArray(p)[3] = 0 then Index := Palette.Count else
            Index := Palette.IndexOf(p^ and not cl32Opaque);

          s := s + xpmIndexToStr(Index, cpp);
        end; // for x

        if y = bmNew.Height - 1 then s := s + '"'#10'};'#10 else
          s := s + '",'#10;
        DoWrite(s);
      end; // for y
    finally
      if CreateBMNew then bmNew.Free;
    end; // try bmNew
  finally
    Palette.Free;
  end; // try Palette
end;

function xpmLoadFromFile;
var
  s: TStream;

begin
  try
    s := TFileStream.Create(fn, fmOpenRead);
    try
      Result := xpmLoadFromStream(bm, HotSpot, s);
    finally
      s.Free;
    end;
  except
    Result := False;
  end;
end;

procedure xpmSaveToFile;
var
  s: TStream;

begin
  s := TFileStream.Create(fn, fmCreate);
  try
    xpmSaveToStream(bm, xpmID, HotSpot, s);
  finally
    s.Free;
  end;
end;

end.
