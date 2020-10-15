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
// Interface to the ImageConverter utility and other utilities, for loading/saving certain images.
unit ImageConverter_Intf;

{$mode delphi}

interface

uses
  Classes, SysUtils, DocClass, Math, BitmapEx;

const
  IMAGE_CONVERTER_DOTNET_VERSION = '3.5';
{$ifdef WINDOWS}
  IMAGE_CONVERTER_EXE: string = 'ImageConverter'+DirectorySeparator+'bin'+DirectorySeparator+'Debug'+DirectorySeparator+'ImageConverter.exe';
  CWEBP_EXE: string = 'webp'+DirectorySeparator+'cwebp.exe';
  DWEBP_EXE: string = 'webp'+DirectorySeparator+'dwebp.exe';
{$endif}
{$ifdef LINUX}
  IMAGE_CONVERTER_EXE: string = './ImageConverter'+DirectorySeparator+'ImageConverter.sh';
  CWEBP_EXE: string = 'cwebp';
  DWEBP_EXE: string = 'dwebp';
{$endif}

type

  { TSvgInfo }

  TSvgInfo = record
    Width: double; // default document width, in pixels
    Height: double; // default document height, in pixels
    Dpi: double; // default document dpi

    procedure Clear;
  end;

function ImageConverterAvailable: boolean;
function ImageConverterInstalled: boolean;
function ImageConverterFrameworkInstalled: boolean;

function svgDetect(Data: Pointer; Size: integer): boolean;
function svgGetInfo(var info: TSvgInfo; const fn: string): boolean;
function svgLoadFromStream(Doc: TIconDoc; s: TStream; scaleFactor: double): boolean;

function tiffDetect(Data: Pointer; Size: integer): boolean;
function tiffLoadFromStream(Doc: TIconDoc; s: TStream): boolean;
function tiffSaveToStream(Doc: TIconDoc; s: TStream): TieDataLosses;

function webpInstalled: boolean;
function webpDetect(Data: Pointer; Size: integer): boolean;
function webpLoadFromStream(bm: TBitmap32; s: TStream): boolean;
function webpSaveToStream(bm: TBitmap32; s: TStream; Quality: Single): boolean;

implementation

uses
  ieShared, dlgDebug, LangPack, PNG, StrUtils;

function ImageConverterInstalled: boolean;
begin
  Result := FileExists(IMAGE_CONVERTER_EXE);
end;

function ImageConverterFrameworkInstalled: boolean;
begin
  Result := DotNetInstalled(IMAGE_CONVERTER_DOTNET_VERSION);
end;

function ImageConverterAvailable: boolean;
begin
  Result := ImageConverterInstalled and ImageConverterFrameworkInstalled;
end;

function anyLoadFromStream(Doc: TIconDoc; s: TStream; srcType, destType, param: string): boolean;
var
  HelperExitCode: integer;
  fnInput, fnOutput: string;
  fs: TFileStream;

begin
  fnInput := GetTempFileName;

  fs := TFileStream.Create(fnInput, fmCreate);
  try
    fs.CopyFrom(s, s.Size - s.Position);
  finally
    fs.Free;
  end;

  fnOutput := GetTempFileName;

  HelperExitCode := RunCommandHidden(AppDir,
    IMAGE_CONVERTER_EXE,
    [srcType+'2'+destType, fnInput, fnOutput, param, '--silent']);
  if HelperExitCode = 0 then
  begin
    Result := Doc.LoadFromFile(fnOutput, Pref_MaxWidth, Pref_MaxHeight, 1, lpGet('LY_BACKGROUND')) <> iftNone;
  end else
  begin
    {$IFDEF GFIEDEBUG}
      Log('ImageConverter exited with code '+IntToStr(HelperExitCode));
    {$ENDIF}
    Result := False;
  end;

  DeleteFile(fnInput);
  DeleteFile(fnOutput);
end;

function anySaveToStream(Doc: TIconDoc; s: TStream; srcType, destType, param: string): TieDataLosses;
var
  HelperExitCode: integer;
  fnInput, fnOutput: string;
  fs: TFileStream;
  ft: TImageFileType;

begin
  fnInput := GetTempFileName; // a file that GFIE can produce

  if srcType = 'png' then
    ft := iftPng
  else
    ft := iftGfie;
  Doc.SaveToFile(fnInput, ft, 0, '', 0, false, Result);

  fnOutput := GetTempFileName;

  HelperExitCode := RunCommandHidden(AppDir, IMAGE_CONVERTER_EXE,
    [srcType+'2'+destType, fnInput, fnOutput, param, '--silent']);
  if HelperExitCode = 0 then
  begin
    fs := TFileStream.Create(fnOutput, fmOpenRead);
    try
      s.CopyFrom(fs, fs.Size);
    finally
      fs.Free;
    end;
  end
  else
  begin
{$IFDEF GFIEDEBUG}
  Log('ImageConverter exited with code '+IntToStr(HelperExitCode));
{$ENDIF}
    Result := [dlError];
  end;

  DeleteFile(fnInput);
  DeleteFile(fnOutput);
end;

function DataContains(Data: Pointer; Size: integer; const sHeader: string): boolean;
var
  DataStart, DataEnd, p: PChar;
begin
  if Size < Length(sHeader) then
    Exit(false);
  DataStart := PChar(Data);
  DataEnd := DataStart + Size - Length(sHeader);
  p := DataStart;
  while p <= DataEnd do
  begin
    if strlicomp(@sHeader[1], p, Length(sHeader)) = 0 then
      Exit(True);
    inc(p);
  end;
  Exit(False);
end;

function svgDetect(Data: Pointer; Size: integer): boolean;
begin
  Result := (Size >= 4)
    and (PCardinal(Data)^ <> 0) // avoid misdetecting .res files, they may contain svg as resource!
    and DataContains(Data, Math.Min(256, Size), '<?xml')
    and DataContains(Data, Size, '<svg');
end;

function tiffDetect(Data: Pointer; Size: integer): boolean;
begin
  Result := (Size >= 4) and ((PCardinal(Data)^ = $002a4949) or (PCardinal(Data)^ = $2a004d4d));
end;

function svgGetInfo(var info: TSvgInfo; const fn: string): boolean;
var
  HelperExitCode, p: integer;
  s, key, value, fnOutput: string;
  f: textfile;

begin
  info.Clear;
  fnOutput := GetTempFileName;

  HelperExitCode := RunCommandHidden(AppDir, IMAGE_CONVERTER_EXE,
    ['svginfo', fn, fnOutput, '', '--silent']);
  if HelperExitCode = 0 then
  begin
    Result := True;
    AssignFile(f, fnOutput);
    Reset(f);
    try
      while not Eof(f) do
      begin
        Readln(f, s);
        p := Pos('=', s);
        if p <= 0 then Continue;
        key := Copy(s, 1, p-1);
        value := Copy(s, p+1, Length(s)-p);
        if key = 'width_px' then
          info.Width := StrToFloat(value)
        else if key = 'height_px' then
          info.Height := StrToFloat(value)
        else if key = 'dpi' then
          info.Dpi := StrToFloat(value);
      end;
    finally
      CloseFile(f);
    end;
  end else
  begin
    {$IFDEF GFIEDEBUG}
      Log('ImageConverter svginfo exited with code '+IntToStr(HelperExitCode));
    {$ENDIF}
    Result := False;
  end;

  DeleteFile(fnOutput);
end;

function svgLoadFromStream(Doc: TIconDoc; s: TStream; scaleFactor: double): boolean;
begin
  Result := anyLoadFromStream(Doc, s, 'svg', 'gfie', FloatToStr(scaleFactor));
end;

function tiffLoadFromStream(Doc: TIconDoc; s: TStream): boolean;
begin
  Result := anyLoadFromStream(Doc, s, 'tiff', 'gfie', '');
end;

function tiffSaveToStream(Doc: TIconDoc; s: TStream): TieDataLosses;
var
  DocToSave: TIconDoc;

begin
  if Doc.HasNonTrivialLayers then
  begin
    // TODO do not copy all layers then flatten. Just render them to DocToSave in one pass.
    DocToSave := TIconDoc.Create;
    DocToSave.Assign(Doc);
    DocToSave.Flatten;
  end else
    DocToSave := Doc;

  try
    Result := anySaveToStream(DocToSave, s, 'gfie', 'tiff', '');
  finally
    if DocToSave <> Doc then
      DocToSave.Free;
  end;
end;

function webpInstalled: boolean;
begin
{$ifdef WINDOWS}
  Result := FileExists(CWEBP_EXE) and FileExists(DWEBP_EXE);
{$endif}
{$ifdef LINUX}
  Result := FileExists('/usr/bin/cwebp') or FileExists('/usr/local/bin/cwebp')
    or FileExists('webp_installed'); // for suppressing any error in the future
{$endif}
end;

function webpDetect(Data: Pointer; Size: integer): boolean;
begin
  // RIFF....WEBP
  Result := (Size >= 12) and (PCardinal(Data)^ = $46464952) and (PIntegerArray(Data)[2] = $50424557);
end;

function webpLoadFromStream(bm: TBitmap32; s: TStream): boolean;
var
  HelperExitCode: integer;
  fnInput, fnOutput: string;
  fs: TFileStream;

begin
  fnInput := GetTempFileName;

  fs := TFileStream.Create(fnInput, fmCreate);
  try
    fs.CopyFrom(s, s.Size - s.Position);
  finally
    fs.Free;
  end;

  fnOutput := GetTempFileName;

  HelperExitCode := RunCommandHidden(AppDir, DWEBP_EXE,
    ['-mt', fnInput, '-o', fnOutput]);
  if HelperExitCode = 0 then
  begin
    Result := pngLoadFromFile(bm, fnOutput);
  end else
  begin
    {$IFDEF GFIEDEBUG}
      Log('cwebp exited with code '+IntToStr(HelperExitCode));
    {$ENDIF}
    Result := False;
  end;

  DeleteFile(fnInput);
  DeleteFile(fnOutput);
end;

function webpSaveToStream(bm: TBitmap32; s: TStream; Quality: Single): boolean;
var
  HelperExitCode: integer;
  fnInput, fnOutput: string;
  fs: TFileStream;
  lossless: boolean;
  webpLossless, webpQuality: string;

begin
  fnInput := GetTempFileName; // a file that GFIE can produce
  pngSaveToFile(bm, fnInput, PNG_COMPRESSION_NORMAL);
  fnOutput := GetTempFileName;

  lossless := (Quality = 0);
  webpQuality := IfThen(lossless, '100', IntToStr(Round(Quality)));
  webpLossless := IfThen(lossless, '-lossless', '-quiet'); // -quiet is a dummy option here

  HelperExitCode := RunCommandHidden(AppDir, CWEBP_EXE,
    ['-q', webpQuality, webpLossless, '-mt', fnInput, '-o', fnOutput]);
  if HelperExitCode = 0 then
  begin
    fs := TFileStream.Create(fnOutput, fmOpenRead);
    try
      s.CopyFrom(fs, fs.Size);
    finally
      fs.Free;
    end;
    Result := true;
  end
  else
  begin
{$IFDEF GFIEDEBUG}
    Log('cwebp exited with code '+IntToStr(HelperExitCode));
{$ENDIF}
    Result := false;
  end;

  DeleteFile(fnInput);
  DeleteFile(fnOutput);
end;

{ TSvgInfo }

procedure TSvgInfo.Clear;
begin
  Width := 0;
  Height := 0;
  Dpi := 96;
end;

end.

