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
// Interface to C library
unit CIntf;

{$mode delphi}

interface

uses
  SysUtils, Classes, bmExUtils;

const
{$IFDEF WINDOWS}
  {$IFDEF CPU386}
    gfiec = 'libgfie32c.dll';
  {$ELSE}
    {$IFDEF CPUX86_64}
      gfiec = 'libgfie64c.dll';
    {$ELSE}
      {$ERROR No GFIE C library for this architecture}
    {$ENDIF}
  {$ENDIF}
{$ELSE}
  {$IFDEF LINUX}
    {$IFDEF CPU386}
      gfiec = 'gfie32c';
    {$ELSE}
      {$IFDEF CPUX86_64}
        gfiec = 'gfie64c';
      {$ELSE}
        {$ERROR No GFIE C library for this architecture}
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
    {$ERROR No GFIE C library for this platform}
  {$ENDIF}
{$ENDIF}

// Define this if you want to debug GFIE without the C library
{ $define cintf_dummy}

// Function pointer for reading/writing 'count' bytes to/from 'data'
// from/to the stream 'userData'
// Returns number of bytes read/written
type
  TIOFunction = function(userData: Pointer; data: PChar; count: integer): integer; cdecl;

// Interface functions to TStream
function IOFunction_ReadFromStream(userData: Pointer; data: PChar; count: integer): integer; cdecl;
function IOFunction_WriteToStream(userData: Pointer; data: PChar; count: integer): integer; cdecl;

// Uncompresses PNG data into 32-bit bitmap. Returns true if succeeded, false otherwise.
// w and h will contain image dimensions
// dpi will contain dpi, or 0 if not set
// If bm==NULL, then it just returns image dimensions into w and h
function pngLoad(bm: Pointer; w, h: PInteger; dpi: PDouble;
  readCallback: TIOFunction; userData: Pointer): boolean;
{$ifndef cintf_dummy}cdecl; external gfiec;{$endif}

// Saves bitmap as PNG image to stream.
function pngSave(bm: Pointer; w, h: Integer; dpi: Double;
  writeCallback: TIOFunction; userData: Pointer;
  compressionLevel: integer): boolean;
{$ifndef cintf_dummy}cdecl; external gfiec;{$endif}

// Uncompresses JP2 data into 32-bit bitmap. Returns true if succeeded, false otherwise.
// w and h will contain image dimensions
// bm will contain a pointer to the image -- this must be freed with jp2Free when done with processing
// loads the image from (data, dataSize)
function jp2Load(var bm: Pointer; var w, h: integer; data: Pointer; dataSize: integer): boolean;
{$ifndef cintf_dummy}cdecl; external gfiec;{$endif}

// Saves bitmap as JP2 image to memory
// data must be freed afterwards using jp2Free
// quality is the Peak Signal to Noise Ratio. if quality = 0, then lossless compression is used
function jp2Save(bm: Pointer; w, h: integer; quality: single;
  var data: Pointer; var dataSize: integer): boolean;
{$ifndef cintf_dummy}cdecl; external gfiec;{$endif}

procedure jp2Free(p: Pointer);
{$ifndef cintf_dummy}cdecl; external gfiec;{$endif}
// If the functions above return false, then they output a null pointer,
// so a call to jp2Free is not necessary

implementation

function IOFunction_ReadFromStream;
begin
  Result := TStream(userData).Read(data^, count);
end;

function IOFunction_WriteToStream;
begin
  Result := TStream(userData).Write(data^, count);
end;

{$ifdef cintf_dummy}
function pngLoad(bm: Pointer; w, h: PInteger;
  readCallback: TIOFunction; userData: Pointer): boolean;
begin Result := False; end;

function pngSave(bm: Pointer; w, h: Integer;
  writeCallback: TIOFunction; userData: Pointer;
  compressionLevel: integer): boolean;
begin Result := False; end;

function jp2Load(var bm: Pointer; var w, h: integer; data: Pointer; dataSize: integer): boolean;
begin Result := False; end;

function jp2Save(bm: Pointer; w, h: integer; quality: single;
  var data: Pointer; var dataSize: integer): boolean;
begin Result := False; end;

procedure jp2Free(p: Pointer);
begin end;
{$endif}

end.

