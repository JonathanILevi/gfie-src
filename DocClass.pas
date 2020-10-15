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
unit DocClass;

interface

uses
  SysUtils, Classes, Graphics, BitmapEx, bmExUtils, Layers,
  Math, PixelFormats, BlendModes, PortableAnyMap;

type

  { TDocPage }

  TDocPage = class(TPersistent)
  public
    Layers: TLayers;
    HotSpot: TPoint;
    FrameRate: integer; // in millisecs (if animated)
    DPI: double; // If zero, parent DPI is used.

    constructor Create;
    destructor Destroy; override;
    procedure Assign(Src: TPersistent); override;
  end;

  TImageFileType = (iftNone, iftGfie, iftIco, iftCur, iftAni, iftIcns,
    iftPng, iftXpm, iftBmp, iftJpeg, iftGif, iftTiff, iftJpeg2000, iftPcx, iftTarga,
    iftPbm, iftPgm, iftPnm, iftPpm, iftWebp,
    // Read-only file types. These must come after the other ones.
    iftSvg);

const
  // TODO png, jpeg, jp2 DPI
  iftDefaultExt: array[TImageFileType] of string =
    ('', '.gfie', '.ico', '.cur', '.ani', '.icns',
      '.png', '.xpm', '.bmp', '.jpg', '.gif', '.tif', '.jp2', '.pcx', '.tga',
      '.pbm', '.pgm', '.pnm', '.ppm', '.webp', '.svg');
  iftFirstReadOnly = iftSvg;
  iftUseImageConverter: set of TImageFileType = [iftTiff, iftSvg];

type
  TieDataLoss = (dlMultiPage, dlLayers, dlSize256, dlTransparency, dlColorDepth,
    dlIcns, dlError);
  TieDataLosses = set of TieDataLoss;

  TIconDocMetadata = record
    // Ani
    Title: string;
    Author: string;
    Copyright: string;
    Comments: string;

    LoopCount: integer; // Gif
    DPI: double; // Parent DPI. If zero, a default value is used.
  end;

type

  { TIconDoc }

  TIconDoc = class(TPersistent)
  private
    FPages: TList; // of TDocPage

    function GetPage(Index: integer): TDocPage;
    procedure SetPage(Index: integer; const Value: TDocPage);
  public
    Metadata: TIconDocMetadata;

    constructor Create;
    destructor Destroy; override;
    procedure Assign(Src: TPersistent); override;
    procedure Append(Src: TIconDoc);
    procedure Clear;

    function PageCount: integer;
    function NewPage: TDocPage;
    function InsertPage(Index: integer): TDocPage;
    procedure DeletePage(Index: integer);
    procedure MovePage(Src, Dest: integer);
    procedure DeleteAllPagesButFirst;

    function ActualDocumentDPI(DefaultDPI: double): double;
    function ActualPageDPI(Page: TDocPage; DefaultDPI: double): double;

    procedure GetThumbnail(Size: integer; bm: TBitmap32; Antialias: boolean);
    function HasNonTrivialLayers: boolean;
    procedure Flatten;

    // MaxWidth and MaxHeight are ignored if reading ICO, CUR or ANI file
    function LoadFromStream(s: TStream;
      MaxWidth, MaxHeight: integer;
      vectorScaleFactor: double; // only for vector graphics like SVG
      const DefaultLayerName: string): TImageFileType;
    function SaveToStream(s: TStream; FileType: TImageFileType;
      PNGLimit: integer; // for ICO, ANI: minimum size for saving page as PNG
      const xpmID: string; // for XPM
      Quality: single; // for JPEG, JPEG2000
      Lossless: boolean; // for JPEG2000
      out DataLoss: TieDataLosses): boolean;

    function LoadFromFile(const fn: string;
      MaxWidth, MaxHeight: integer;
      vectorScaleFactor: double;
      const DefaultLayerName: string): TImageFileType;
    function SaveToFile(const fn: string; FileType: TImageFileType;
      PNGLimit: integer; const xpmID: string; Quality: single; Lossless: boolean;
      out DataLoss: TieDataLosses): boolean;

    // WARNING: avoid SetPage when possible (when using it, the old page won't be freed)
    property Pages[Index: integer]: TDocPage read GetPage write SetPage; default;
  end;

// Detects file type from stream contents
function DetectImageFileType(s: TStream): TImageFileType;
// Detects file type from extension of filename
function DetectImageFileTypeFromName(const fn: string): TImageFileType;
function PNGCompressIcon(w, h, Limit: integer): boolean;

implementation

uses
  GFIEFormat, ICO, ANI, ICNS, PNG, XPM, BMP, GIF, Jpeg2000, PCX, Targa,
  ImageConverter_Intf, ieShared, Dialogs, LangPack;

const
  mdDefault: TIconDocMetadata =
    (Title: ''; Author: ''; Copyright: ''; Comments: '';
     LoopCount: 0; DPI: 0);

function DetectImageFileType;
const
  MAX_HEADER_SIZE = 512;

var
  oldPos, HeaderSize: integer;
  Header: array[0..MAX_HEADER_SIZE-1] of byte;

begin
  Result := iftNone;
  try
    oldPos := s.Position;
    HeaderSize := s.Read(Header, MAX_HEADER_SIZE);
    s.Position := oldPos;

    if gfieDetect(@Header, HeaderSize) then Result := iftGfie else
    if icoDetect(@Header, HeaderSize, False) then Result := iftIco else
    if icoDetect(@Header, HeaderSize, True) then Result := iftCur else
    if aniDetect(@Header, HeaderSize) then Result := iftAni else
    if icnsDetect(@Header, HeaderSize) then Result := iftIcns else
    if pngDetect(@Header, HeaderSize) then Result := iftPng else
    if xpmDetect(@Header, HeaderSize) then Result := iftXpm else
    if bmpDetect(@Header, HeaderSize) then Result := iftBmp else
    if jpegDetect(@Header, HeaderSize) then Result := iftJpeg else
    if gifDetect(@Header, HeaderSize) then Result := iftGif else
    if tiffDetect(@Header, HeaderSize) then Result := iftTiff else
    if jp2Detect(@Header, HeaderSize) then Result := iftJpeg2000 else
    if pcxDetect(@Header, HeaderSize) then Result := iftPcx else
    if svgDetect(@Header, HeaderSize) then Result := iftSvg else
    // PBM, PGM and PPM are a subset of PNM, so do not detect them separately
    if pnmDetect(@Header, HeaderSize) then Result := iftPnm else
    if webpDetect(@Header, HeaderSize) then Result := iftWebp else
    // TGA detection is not very reliable (TGA has no signature)
    // so leave this last
    if tgaDetect(@Header, HeaderSize) then Result := iftTarga;
  except
  end;
end;

function DetectImageFileTypeFromName;
var
  Ext: string;

begin
  Ext := UpperCase(ExtractFileExt(fn));

  if (Ext = '.GFIE') or (Ext = '.GFI') then Result := iftGfie else

  if Ext = '.ICO' then Result := iftIco else
  if Ext = '.CUR' then Result := iftCur else
  if Ext = '.ANI' then Result := iftAni else
  if Ext = '.ICNS' then Result := iftIcns else

  if Ext = '.PNG' then Result := iftPng else
  if Ext = '.XPM' then Result := iftXpm else
  if Ext = '.BMP' then Result := iftBmp else
  if (Ext = '.JPG') or (Ext = '.JPEG') or (Ext = '.JPE') then Result := iftJpeg else
  if Ext = '.GIF' then Result := iftGif else
  if (Ext = '.TIFF') or (Ext = '.TIF') then Result := iftTiff else
  if (Ext = '.JP2') or (Ext = '.J2K') or (Ext = '.JPF') or (Ext = '.JPX') then
    Result := iftJpeg2000 else
  if Ext = '.PCX' then Result := iftPcx else
  if Ext = '.TGA' then Result := iftTarga else
  if Ext = '.PBM' then Result := iftPbm else
  if Ext = '.PGM' then Result := iftPgm else
  if Ext = '.PNM' then Result := iftPnm else
  if Ext = '.PPM' then Result := iftPpm else
  if Ext = '.WEBP' then Result := iftWebp else
  if Ext = '.SVG' then Result := iftSvg else

    Result := iftNone;
end;

function PNGCompressIcon;
begin
  Result := (w >= Limit) or (h >= Limit);
end;

// TDocPage

constructor TDocPage.Create;
begin
  inherited;

  Layers := TLayers.Create;
  HotSpot := Point(0, 0);
  FrameRate := 0;
  DPI := 0;
end;

destructor TDocPage.Destroy;
begin
  Layers.Free;

  inherited;
end;

procedure TDocPage.Assign(Src: TPersistent);
var
  x: TDocPage;

begin
  if Src is TDocPage then
  begin
    x := Src as TDocPage;

    Layers.Assign(x.Layers);
    HotSpot := x.HotSpot;
    FrameRate := x.FrameRate;
    DPI := x.DPI;
  end else
    inherited;
end;

// TIconDoc

function TIconDoc.GetPage(Index: integer): TDocPage;
begin
  Result := FPages[Index];
end;

procedure TIconDoc.SetPage(Index: integer; const Value: TDocPage);
begin
  FPages[Index] := Value;
end;

constructor TIconDoc.Create;
begin
  inherited;

  FPages := TList.Create;
  Clear;
end;

destructor TIconDoc.Destroy;
begin
  Clear;
  FPages.Free;

  inherited;
end;

procedure TIconDoc.Assign(Src: TPersistent);
var
  i: integer;
  x: TIconDoc;
  bm32: TBitmap32;

begin
  if Src is TIconDoc then
  begin
    x := Src as TIconDoc;

    // copy all pages
    Clear;
    for i := 0 to x.PageCount - 1 do NewPage.Assign(x.Pages[i]);

    // copy metadata
    Metadata := x.Metadata;
  end else
  if Src is TBitmap32 then
  begin
    bm32 := Src as TBitmap32;
    Clear;
    NewPage.Layers.Assign(bm32);
  end else
    inherited;
end;

procedure TIconDoc.Append(Src: TIconDoc);
var
  i: integer;

begin
  for i := 0 to Src.PageCount - 1 do
    NewPage.Assign(Src.Pages[i]);
end;

procedure TIconDoc.Clear;
var
  i: integer;
  
begin
  for i := 0 to PageCount - 1 do Pages[i].Free;
  FPages.Clear;
  Metadata := mdDefault;
end;

function TIconDoc.PageCount: integer;
begin
  Result := FPages.Count;
end;

function TIconDoc.NewPage: TDocPage;
begin
  Result := InsertPage(PageCount);
end;

function TIconDoc.InsertPage(Index: integer): TDocPage;
begin
  Result := TDocPage.Create;
  FPages.Insert(Index, Result);
end;

procedure TIconDoc.DeletePage(Index: integer);
begin
  Pages[Index].Free;
  FPages.Delete(Index);
end;

procedure TIconDoc.MovePage(Src, Dest: integer);
begin
  FPages.Move(Src, Dest);
end;

function TIconDoc.ActualPageDPI;
begin
  Result := Page.DPI;
  if Result <= 0 then Result := ActualDocumentDPI(DefaultDPI);
end;

procedure TIconDoc.DeleteAllPagesButFirst;
var
  i: integer;
begin
  for i := PageCount - 1 downto 1 do
    DeletePage(i);
end;

function TIconDoc.ActualDocumentDPI;
begin
  Result := Metadata.DPI;
  if Result <= 0 then Result := DefaultDPI;
end;

procedure TIconDoc.GetThumbnail(Size: integer; bm: TBitmap32; Antialias: boolean
  );
var
  i: integer;
  b1, b2: boolean;
  pgBest, pg: TDocPage;
  bmTemp: TBitmap32;
  
begin
  // No pages?
  if PageCount = 0 then
  begin
    bm.Resize(Size, Size);
    bm.FillTransparent;
    Exit;
  end;

  // We have to find the smallest page, which is larger or equal to Size
  // If there is no such page, we have to find the greatest page
  pgBest := Pages[0];
  for i := 1 to PageCount - 1 do
  begin
    pg := Pages[i];
    b1 := pg.Layers.Width >= Size;
    b2 := pg.Layers.Width > pgBest.Layers.Width;

    if (b1 and (not b2 or (pgBest.Layers.Width < Size))) or
      (not b1 and b2) then pgBest := pg;
  end;

  bmTemp := TBitmap32.Create;
  try
    bmTemp.Assign(pgBest.Layers);
    bm.CreateThumbnail(bmTemp, Size, Antialias);
  finally
    bmTemp.Free;
  end;
end;

function TIconDoc.HasNonTrivialLayers: boolean;
var
  i: integer;
begin
  for i := 0 to PageCount - 1 do
    if Pages[i].Layers.HasNonTrivialLayers then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

procedure TIconDoc.Flatten;
var
  i: integer;
begin
  for i := 0 to PageCount - 1 do
    Pages[i].Layers.Merge(lssAll);
end;

function TIconDoc.LoadFromStream(s: TStream; MaxWidth, MaxHeight: integer;
  vectorScaleFactor: double; const DefaultLayerName: string): TImageFileType;
var
  i, j: integer;
  Page: TDocPage;
  bm: TBitmap32;
  FileType: TImageFileType;
  hdpi, vdpi: word;
  PagesDropped: integer;
  
begin
  Result := iftNone;
  try
    FileType := DetectImageFileType(s);
    if FileType = iftNone then Exit;

    if (FileType in iftUseImageConverter) and not ImageConverterAvailable then
      Exit;
    if (FileType = iftWebp) and not webpInstalled then
      Exit;

    case FileType of
      iftGfie: if not gfieLoadFromStream(Self, s, MaxWidth, MaxHeight) then Exit;

      iftIco, iftCur: if not icoLoadFromStream(Self, s) then Exit;
      iftAni: if not aniLoadFromStream(Self, s) then Exit;
      iftIcns: begin
        if not icnsLoadFromStream(Self, s, PagesDropped) then Exit;
        if PagesDropped > 0 then
          ShowMessage(Format(lpGet('MSG_DL_ICNS_LOAD_DROP_PAGES'), [PagesDropped]));
      end;

      // single-page image files
      iftPng, iftXpm, iftBmp, iftJpeg2000, iftPcx, iftTarga, iftPbm, iftPgm, iftPnm, iftPpm, iftWebp: begin
        Clear;
        Page := NewPage;

        bm := TBitmap32.Create;
        try
          case FileType of
            iftPng: if not pngLoadFromStream(bm, s, Page.DPI) then Exit;
            iftXpm: if not xpmLoadFromStream(bm, Page.HotSpot, s) then Exit;
            iftBmp: if not bmpLoadFromStream(bm, s) then Exit;
            iftJpeg2000: if not jp2LoadFromStream(bm, s) then Exit;
            iftPcx: begin
              if not pcxLoadFromStream(bm, hdpi, vdpi, s) then Exit;
              Page.DPI := 0.5*(Integer(hdpi) + vdpi);
            end;
            iftTarga: if not tgaLoadFromStream(bm, s) then Exit;
            iftPbm: if not pbmLoadFromStream(bm, s) then Exit;
            iftPgm: if not pgmLoadFromStream(bm, s) then Exit;
            iftPnm: if not pnmLoadFromStream(bm, s) then Exit;
            iftPpm: if not ppmLoadFromStream(bm, s) then Exit;
            iftWebp: if not webpLoadFromStream(bm, s) then Exit;
          end; // case ft, if single-page

          bm.Resize(Min(bm.Width, MaxWidth), Min(bm.Height, MaxHeight));
          Page.Layers.Assign(bm);
        finally
          bm.Free;
        end;
      end; // case single-page

      iftJpeg: if not jpegLoadDocFromStream(Self, s) then Exit;
      iftGif: if not gifLoadFromStream(Self, s, MaxWidth, MaxHeight) then Exit;
      iftTiff: if not tiffLoadFromStream(Self, s) then Exit;
      iftSvg: if not svgLoadFromStream(Self, s, vectorScaleFactor) then Exit;
    end; // case FileType

    if FileType <> iftGfie then
    // rename layers
    for i := 0 to PageCount - 1 do
    for j := 0 to Pages[i].Layers.LayerCount - 1 do
      Pages[i].Layers[j].Name := DefaultLayerName;

    // success
    Result := FileType;
  except
  end;
end;

function IsGray(bm: TBitmap32): boolean;
var
  p: PColor32;
  i: integer;

begin
  p := bm.Data;
  for i := 1 to bm.Width*bm.Height do
  begin
    if (p^ xor (p^ shr 8)) and $ffff <> 0 then
    begin
      result := false;
      break;
    end;
    inc(p);
  end;
  result := true;
end;

function TIconDoc.SaveToStream(s: TStream; FileType: TImageFileType;
  PNGLimit: integer; const xpmID: string; Quality: single; Lossless: boolean;
  out DataLoss: TieDataLosses): boolean;
var
  i: integer;
  onePageDPI: double;
  pcxDpi: word;
  ColorLoss, Transparent: boolean;
  pt: TPoint;
  bm, bmFirst: TBitmap32;
  DroppedPages: TicnsDroppedPages;

begin
  Result := False;
  if (FileType = iftNone) or (FileType >= iftFirstReadOnly) then
    Exit;
  if (FileType in iftUseImageConverter) and not ImageConverterAvailable then
    Exit;
  if (FileType = iftWebp) and not webpInstalled then
    Exit;

  // Determine which parts of data will be lost to inform the user
  DataLoss := [];

  for i := 0 to PageCount - 1 do with Pages[i].Layers do
  begin
    if (FileType in [iftIco, iftCur, iftAni]) and
      ((Width > 256) or (Height > 256)) then
    DataLoss += [dlSize256];

    if (FileType <> iftGfie) and HasNonTrivialLayers then
      DataLoss += [dlLayers];
  end;

  // one-page file formats
  if FileType in [iftPng, iftXpm, iftBmp, iftJpeg, iftJpeg2000, iftPcx, iftTarga,
    iftPbm, iftPgm, iftPnm, iftPpm, iftWebp] then
  begin
    if PageCount <> 1 then DataLoss += [dlMultiPage];

    bmFirst := TBitmap32.Create;
    onePageDPI := ActualDocumentDPI(72);
    if PageCount > 0 then
    begin
      bmFirst.Assign(Pages[0].Layers);
      onePageDPI := ActualPageDPI(Pages[0], 72);
    end;
  end else
    bmFirst := nil;

  try // for bmFirst
    if (FileType in [iftJpeg, iftPcx, iftPbm, iftPgm, iftPnm, iftPpm]) and bmFirst.HasTransparency then
      DataLoss += [dlTransparency];

    ColorLoss := False;
    case FileType of
      iftXpm: ColorLoss := (GetPixelFormat32(bmFirst, nil, nil, nil, nil) = pf32_32bit);

      iftGif: begin
        bm := TBitmap32.Create;
        try
          for i := 0 to PageCount - 1 do
          begin
            bm.Assign(Pages[i].Layers);
            if not gifGet256Colors(bm, nil, Transparent) then
            begin
              ColorLoss := True;
              Break;
            end;
          end; // for i
        finally
          bm.Free;
        end;
      end;

      iftPbm: ColorLoss := (GetPixelFormat32(bmFirst, nil, nil, nil, nil) > pf32_1bit);
      iftPgm: ColorLoss := not IsGray(bmFirst);
    end;
    if ColorLoss then DataLoss += [dlColorDepth];

    case FileType of
      iftNone: begin end;

      iftGfie: gfieSaveToStream(Self, s, true);

      iftIco, iftCur: icoSaveToStream(Self, s, FileType = iftCur, PNGLimit);
      iftAni: aniSaveToStream(Self, s, PNGLimit);

      iftIcns: begin
        icnsSaveToStream(Self, s, DroppedPages);
        if Length(DroppedPages) > 0 then DataLoss += [dlIcns];
      end;

      iftPng: pngSaveToStream(bmFirst, s, PNG_COMPRESSION_HIGH, onePageDPI);
      iftXpm: begin
        if PageCount > 0 then pt := Pages[0].HotSpot else pt := Point(0, 0);
        xpmSaveToStream(bmFirst, xpmID, pt, s);
      end;
      iftBmp: bmpSaveToStream(bmFirst, s);
      iftJpeg: jpegSaveDocToStream(Self, s, Round(Quality));
      iftGif: gifSaveToStream(Self, s);
      iftTiff: DataLoss += tiffSaveToStream(Self, s);
      iftJpeg2000: jp2SaveToStream(bmFirst, s, IfThen(Lossless, 0, Quality));
      iftPcx: begin
        pcxDpi := Round(Min(High(pcxDpi), onePageDPI));
        pcxSaveToStream(bmFirst, pcxDpi, pcxDpi, s);
      end;
      iftTarga: tgaSaveToStream(bmFirst, s);
      iftPbm: pbmSaveToStream(bmFirst, s);
      iftPgm: pgmSaveToStream(bmFirst, s);
      iftPnm: pnmSaveToStream(bmFirst, s);
      iftPpm: ppmSaveToStream(bmFirst, s);
      iftWebp: webpSaveToStream(bmFirst, s, IfThen(Lossless, 0, Quality));
    end; // case FileType
  finally
    if Assigned(bmFirst) then bmFirst.Free;
  end;
  Result := True;
end;

function TIconDoc.LoadFromFile(const fn: string; MaxWidth, MaxHeight: integer;
  vectorScaleFactor: double; const DefaultLayerName: string): TImageFileType;
var
  s: TStream;

begin
  Result := iftNone;
  try
    s := TFileStream.Create(fn, fmOpenRead);
    try
      Result := LoadFromStream(s, MaxWidth, MaxHeight, vectorScaleFactor, DefaultLayerName);
    finally
      s.Free;
    end;
  except
  end;
end;

function TIconDoc.SaveToFile(const fn: string; FileType: TImageFileType;
  PNGLimit: integer; const xpmID: string; Quality: single; Lossless: boolean;
  out DataLoss: TieDataLosses): boolean;
var
  s: TStream;

begin
  if (FileType = iftNone) or (FileType >= iftFirstReadOnly) then
    Exit(false);
  s := TFileStream.Create(fn, fmCreate);
  try
    Result := SaveToStream(s, FileType, PNGLimit, xpmID, Quality, Lossless, DataLoss);
  finally
    s.Free;
  end;
end;

end.

