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
unit PNG;

interface

uses
  LclIntf, LclType, CIntf, SysUtils, Classes, Graphics, BitmapEx, Math;

const
  SIG_PNG = $474e5089; // PNG file signature
  PNG_COMPRESSION_NORMAL = 5;
  PNG_COMPRESSION_HIGH = 9;

function pngDetect(Data: Pointer; Size: integer): boolean;

function pngLoadFromStream(bm: TBitmap32; s: TStream; out dpi: double): boolean;
procedure pngSaveToStream(bm: TBitmap32; s: TStream;
  CompressionLevel: integer; dpi: double);
function pngLoadFromFile(bm: TBitmap32; const fn: string): boolean;
procedure pngSaveToFile(bm: TBitmap32; const fn: string; CompressionLevel: integer);

// Loads a PNG file into a TBitmap object
function pngLoadFromFileBM(bm: TBitmap; const fn: string; BackColor: TColor): boolean;

implementation

function pngDetect;
begin
  Result := (Size >= 4) and (PCardinal(Data)^ = SIG_PNG);
end;

function pngLoadFromStream;
var
  p, w, h: integer;

begin
  dpi := 0;
  Result := False;
  try
    p := s.Position;
    Result := pngLoad(nil, @w, @h, @dpi, IOFunction_ReadFromStream, s);
    s.Position := p;
    if not Result then Exit;

    bm.Resize(w, h);
    Result := pngLoad(bm.Data, @w, @h, @dpi, IOFunction_ReadFromStream, s);
    if not Result then Exit;

    Result := True;
  except
  end;
end;

procedure pngSaveToStream;
begin
  pngSave(bm.Data, bm.Width, bm.Height, dpi, IOFunction_WriteToStream, s,
  	CompressionLevel);
end;

function pngLoadFromFile;
var
  s: TStream;
  dpi: double;

begin
  try
    s := TFileStream.Create(fn, fmOpenRead);
    try
      Result := pngLoadFromStream(bm, s, dpi);
    finally
      s.Free;
    end;
  except
    Result := False;
  end;
end;

procedure pngSaveToFile;
var
  s: TStream;

begin
  s := TFileStream.Create(fn, fmCreate);
  try
    pngSaveToStream(bm, s, CompressionLevel, 0.0);
  finally
    s.Free;
  end;
end;

function pngLoadFromFileBM;
var
  bm32: TBitmap32;
  
begin
  bm32 := TBitmap32.Create;
  try
    Result := pngLoadFromFile(bm32, fn);
    if not Result then Exit;
    bm32.ToBitmap(bm, BackColor);
  finally
    bm32.Free;
  end;
end;

end.
