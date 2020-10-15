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
// ICNS import/export
unit ICNS;

{$mode delphi}

interface

uses
  SysUtils, Classes, BitmapEx, bmExUtils, DocClass, Math,
  PixelFormats;

type
  TicnsDroppedPages = array of integer;

function icnsDetect(Data: Pointer; Size: integer): boolean;
function icnsLoadFromStream(Doc: TIconDoc; s: TStream; out IcnsPagesDropped: integer): boolean;
// Should save all icns pages
procedure icnsSaveToStream(Doc: TIconDoc; s: TStream;
  out DroppedPages: TicnsDroppedPages);

// Structure of an ICNS file:
// - signature 'icns'
// - total file size
// - [array of]:
//     - image format signature 'ICON', 'icl8', etc.
//     - image format total size
//     - image format data
// All sizes are in big endian

type
  TicnsImageSize = (iisNone, iis16x12,
    iis16, iis32, iis48, iis128, iis256, iis512, iis1024);

const
  icnsWidth: array[TicnsImageSize] of integer =
    (-1, 16, 16, 32, 48, 128, 256, 512, 1024);
  icnsHeight: array[TicnsImageSize] of integer =
    (-1, 12, 16, 32, 48, 128, 256, 512, 1024);

type
  TicnsPixelFormat = (ipfNone, ipf1bit, ipf4bit, ipf8bit, ipf32bit);
  TicnsFormatSet = array[TicnsImageSize, TicnsPixelFormat] of boolean;

  TicnsTag = (itNone,
    it1024x1024_32, it512x512_32, it256x256_32,
    it128x128_32, it128x128_8m,
    it48x48_1i, it48x48_4, it48x48_8, it48x48_32, it48x48_8m,
    it32x32_1i, it32x32_4, it32x32_8, it32x32_32, it32x32_8m,
    it16x16_1i, it16x16_4, it16x16_8, it16x16_32, it16x16_8m,
    it16x12_1i, it16x12_4, it16x12_8);
  TicnsTags = set of TicnsTag;

const
  icnsImageTag: array[TicnsImageSize, TicnsPixelFormat] of TicnsTag =
    ((itNone, itNone, itNone, itNone, itNone),
     (itNone, it16x12_1i, it16x12_4, it16x12_8, itNone),
     (itNone, it16x16_1i, it16x16_4, it16x16_8, it16x16_32),
     (itNone, it32x32_1i, it32x32_4, it32x32_8, it32x32_32),
     (itNone, it48x48_1i, it48x48_4, it48x48_8, it48x48_32),
     (itNone, itNone, itNone, itNone, it128x128_32),
     (itNone, itNone, itNone, itNone, it256x256_32),
     (itNone, itNone, itNone, itNone, it512x512_32),
     (itNone, itNone, itNone, itNone, it1024x1024_32));
  icnsMaskTag: array[TicnsImageSize, TicnsPixelFormat] of TicnsTag =
    ((itNone, itNone, itNone, itNone, itNone),
     (itNone, it16x12_1i, it16x12_1i, it16x12_1i, itNone),
     (itNone, it16x16_1i, it16x16_1i, it16x16_1i, it16x16_8m),
     (itNone, it32x32_1i, it32x32_1i, it32x32_1i, it32x32_8m),
     (itNone, it48x48_1i, it48x48_1i, it48x48_1i, it48x48_8m),
     (itNone, itNone, itNone, itNone, it128x128_8m),
     (itNone, itNone, itNone, itNone, itNone),
     (itNone, itNone, itNone, itNone, itNone),
     (itNone, itNone, itNone, itNone, itNone));
  icnsValidFormat: TicnsFormatSet =
    ((False, False, False, False, False),
     (False, True, True, True, False),
     (False, True, True, True, True),
     (False, True, True, True, True),
     (False, True, True, True, True),
     (False, False, False, False, True),
     (False, False, False, False, True),
     (False, False, False, False, True),
     (False, False, False, False, True));

  // Four-character codes
  icnsTagFCC: array[TicnsTag] of string =
    (
    '    ',
    'ic10', 'ic09', 'ic08',
    'it32', 't8mk',
    'ich#', 'ich4', 'ich8', 'ih32', 'h8mk',
    'ICN#', 'icl4', 'icl8', 'il32', 'l8mk',
    'ics#', 'ics4', 'ics8', 'is32', 's8mk',
    'icm#', 'icm4', 'icm8'
    );

  icnsImageSize: array[TicnsTag] of TicnsImageSize =
    (
    iisNone, iis1024, iis512, iis256, iis128, iis128,
    iis48, iis48, iis48, iis48, iis48,
    iis32, iis32, iis32, iis32, iis32,
    iis16, iis16, iis16, iis16, iis16,
    iis16x12, iis16x12, iis16x12
    );
  icnsPixelFormat: array[TicnsTag] of TicnsPixelFormat =
    (
    ipfNone, ipf32bit, ipf32bit, ipf32bit,
    ipf32bit, ipfNone,
    ipf1bit, ipf4bit, ipf8bit, ipf32bit, ipfNone,
    ipf1bit, ipf4bit, ipf8bit, ipf32bit, ipfNone,
    ipf1bit, ipf4bit, ipf8bit, ipf32bit, ipfNone,
    ipf1bit, ipf4bit, ipf8bit
    );

const
  ICNS_SIGNATURE = 'icns';

implementation

uses
  PNG, Jpeg2000, Layers;

const
  // These are the tags which always represent a mask
  icnsTagMask: TicnsTags = [it128x128_8m, it48x48_8m, it32x32_8m, it16x16_8m];

type
  PicnsRawImage = ^TicnsRawImage;
  TicnsRawImage = record
    Tag: TicnsTag;
    Data: array of byte;
  end;
  TicnsRawImages = array of TicnsRawImage;

function icnsFindRawImage(var RawImages: TicnsRawImages;
  Tag: TicnsTag; CanCreate: boolean = False): PicnsRawImage;
var
  i: integer;

begin
  for i := 0 to Length(RawImages) - 1 do
    if RawImages[i].Tag = Tag then Exit(@RawImages[i]);
  if CanCreate then
  begin
    SetLength(RawImages, Length(RawImages) + 1);
    Result := @RawImages[Length(RawImages) - 1];
    Result.Tag := Tag;
    Exit;
  end;
  Exit(nil);
end;

// AsMask is only taken into account for 1-bit Image+Mask rawimages
// (AsMask = whether image or mask data should be used)
function icnsDecodeRawImage(bm: TBitmap32;
  const ri: TicnsRawImage; AsMask: boolean): boolean;
var
  i, j, n, Channel, runLength, PixelIndex, PixelCount, DataLength: integer;
  dpi: double;
  Compressed: boolean;
  iis: TicnsImageSize;
  pSrc, pDest: PColor32;
  pba: PByteArray;
  b: byte;
  s: TStream;

  // Helper for decoding RLE
  function AddPixel(Value: byte): boolean;
  begin
    // check
    Result := (Channel < 3);
    if not Result then Exit;
    // store
    pba[PixelIndex * 4 + Channel] := Value;
    // iterate
    Inc(PixelIndex);
    if PixelIndex < n then Exit;
    Inc(Channel);
    PixelIndex := 0;
  end;

begin
  // Resize image
  iis := icnsImageSize[ri.Tag];
  bm.Resize(icnsWidth[iis], icnsHeight[iis]);

  // Decode data to 32-bit bitmap
  case ri.Tag of
    // 32-bit JPEG-2000 or PNG image
    it1024x1024_32, it512x512_32, it256x256_32:
    begin
      // make memory stream from raw data
      s := TMemoryStream.Create;
      try
        s.WriteBuffer(ri.Data[0], Length(ri.Data));
        s.Position := 0;

        // try to decode image
        if not pngLoadFromStream(bm, s, dpi) and
          not jp2LoadFromStream(bm, s) then
            Exit(false); // there is nothing we can do
      finally
        s.Free;
      end;
    end;

    // 32-bit image
    it128x128_32, it48x48_32, it32x32_32, it16x16_32:
    begin
      DataLength := Length(ri.Data);
      Compressed := DataLength <> 4 * bm.Width * bm.Height;
      if Compressed then // RLE
      begin
        bm.FillColor(cl32Opaque);
        n := bm.Width * bm.Height;
        i := 0;

        // sometimes there is 4 bytes zero padding at the beginning
        if (DataLength >= 4) and (PInteger(@ri.Data[0])^ = 0) then
          i := 4;

        // Red, Green then Blue are encoded separately, one after the other
        pba := bm.Data;
        Channel := 0;
        PixelIndex := 0;
        // read bytes
        while i < DataLength do
        begin
          if ri.Data[i] < $80 then
          begin
            // different bytes
            runLength := ri.Data[i] + 1;
            Inc(i);
            for j := 0 to runLength - 1 do
            begin
              if (i >= DataLength) then Break;
              if not AddPixel(ri.Data[i]) then Break;
              Inc(i);
            end;
          end
          else
          begin
            // following byte must be repeated
            runLength := ri.Data[i] - 125;
            Inc(i);

            if i >= DataLength then
              Break;
            // Color value
            b := ri.Data[i];
            Inc(i);

            for j := 0 to runLength - 1 do
              if not AddPixel(b) then Break;
          end; // repeat byte
        end; // while has byte
      end // Compressed
      else // Uncompressed
      begin
        // read ARGB pixels
        // and convert them to RGBA
        pSrc := PColor32(@ri.Data[0]);
        pDest := bm.Data;
        for i := 1 to bm.Width * bm.Height do
        begin
          pDest^ := RorDWord(pSrc^, 8);
          if (pDest^ and cl32Opaque = 0) then
            pDest^ := cl32Transparent; // remove cl32Inverted
          Inc(pSrc);
          Inc(pDest);
        end;
      end;
    end;

    // 8-bit mask
    it128x128_8m, it48x48_8m, it32x32_8m, it16x16_8m:
    begin
      pDest := bm.Data;
      for i := 0 to bm.Width * bm.Height - 1 do
      begin
        PByteArray(pDest)[3] := ri.Data[i];
        if (pDest^ and cl32Opaque = 0) then
          pDest^ := cl32Transparent; // remove cl32Inverted
        Inc(pDest);
      end;
    end;

    // 8-bit image
    it48x48_8, it32x32_8, it16x16_8, it16x12_8:
    begin
      pDest := bm.Data;
      for i := 0 to bm.Width * bm.Height - 1 do
      begin
        pDest^ := TColor32(palMac256[ri.Data[i]]) or cl32Opaque;
        Inc(pDest);
      end;
    end;

    // 4-bit image
    it48x48_4, it32x32_4, it16x16_4, it16x12_4:
    begin
      Assert(bm.Width * bm.Height mod 2 = 0);
      pDest := bm.Data;
      for i := 0 to bm.Width * bm.Height div 2 - 1 do
      begin
        pDest^ := TColor32(palMac16[ri.Data[i] shr 4]) or cl32Opaque;
        Inc(pDest);
        pDest^ := TColor32(palMac16[ri.Data[i] and $f]) or cl32Opaque;
        Inc(pDest);
      end;
    end;

    // 1-bit image with mask
    it48x48_1i, it32x32_1i, it16x16_1i, it16x12_1i:
    begin
      PixelCount := bm.Width * bm.Height;

      if not AsMask then
      begin
        // process image
        pDest := bm.Data;
        b := 0;
        for i := 0 to PixelCount - 1 do
        begin
          if i and 7 = 0 then
            b := ri.Data[i shr 3]
          else
            b := b shl 1;
          pDest^ := IfThen(b and $80 <> 0, cl32Black, cl32White);
          Inc(pDest);
        end;
      end else
      begin
        // process mask
        pDest := bm.Data;
        b := 0;
        for i := PixelCount to 2 * PixelCount - 1 do
        begin
          if i and 7 = 0 then
            b := ri.Data[i shr 3]
          else
            b := b shl 1;
          if b and $80 = 0 then
            pDest^ := cl32Transparent;
          Inc(pDest);
        end;
      end; // if AsMask
    end; // 1-bit image with mask
  end; // case tag
  Result := true;
end;

function icnsDetect;
const
  FCC_icns = $736e6369;

begin
  Result := (Size >= 4) and (PCardinal(Data)^ = FCC_icns);
end;

function icnsLoadFromStream;
var
  fcc: string;
  i, TotalSize, BytesRead, ImageCount, ImageDataSize: integer;
  PageDropped: boolean;
  RawImages: TicnsRawImages;
  ri, riMask: PicnsRawImage;
  bm: TBitmap32;
  it, itMask: TicnsTag;

begin
  Result := False;
  IcnsPagesDropped := 0;

  try
    BytesRead := 0;

    // read signature
    fcc := '    ';
    s.ReadBuffer(fcc[1], 4);
    Inc(BytesRead, 4);
    if fcc <> ICNS_SIGNATURE then
      Exit;

    // read total length
    s.ReadBuffer(TotalSize, 4);
    Inc(BytesRead, 4);
    TotalSize := BSwapS(TotalSize);

    // read individual raw images
    SetLength(RawImages, 0);
    ImageCount := 0;
    while BytesRead < TotalSize do
    begin
      Inc(ImageCount);
      if ImageCount > Length(RawImages) then
        SetLength(RawImages, ImageCount or $ff);
      ri := @RawImages[ImageCount - 1];

      // read fourcc
      fcc := '    ';
      s.ReadBuffer(fcc[1], 4);
      Inc(BytesRead, 4);

      // determine tag
      ri.Tag := itNone;
      for it in TicnsTag do
        if icnsTagFCC[it] = fcc then
        begin
          ri.Tag := it;
          Break;
        end;

      // read size
      s.ReadBuffer(ImageDataSize, 4);
      Inc(BytesRead, 4);
      ImageDataSize := BSwapS(ImageDataSize);
      Dec(ImageDataSize, 8); // -header

      // too big image size?
      if ImageDataSize > 1024 * 1024 * 4 * 2 then
        Exit;

      // read raw image data
      SetLength(ri.Data, ImageDataSize);
      s.ReadBuffer(ri.Data[0], ImageDataSize);
      Inc(BytesRead, ImageDataSize);
    end; // while read images
    SetLength(RawImages, ImageCount);

    // decode images
    Doc.Clear;
    for i := 0 to Length(RawImages) - 1 do
      if (RawImages[i].Tag <> itNone) and not (RawImages[i].Tag in icnsTagMask) then
      begin
        ri := @RawImages[i];
        PageDropped := False;

        bm := TBitmap32.Create;
        try
          // Parse image data
          if not icnsDecodeRawImage(bm, ri^, False) then
            PageDropped := true;

          // Parse mask data, if present
          itMask := icnsMaskTag[icnsImageSize[ri.Tag], icnsPixelFormat[ri.Tag]];
          if itMask <> itNone then
          begin
            riMask := icnsFindRawImage(RawImages, itMask);
            if riMask <> nil then
              icnsDecodeRawImage(bm, riMask^, True);
          end;

          (*
          // This is some tricky workaround here
          // If it is a 1-bit image+mask, image is black and there is
          // 4-bit or 8-bit image for this size --> do not show this page
          // It is most likely just a mask
          if icnsPixelFormat[ri.Tag] = ipf1bit then
          begin
            // is the image completely black?
            IsBlack := True;
            p := bm.Data;
            for j := 1 to bm.Width*bm.Height do
            begin
              if (p^ and cl32Opaque <> 0) and (p^ <> cl32Black) then
              begin
                IsBlack := False;
                Break;
              end;
              inc(p);
            end;

            iis := icnsImageSize[ri.Tag]; // this is not iisNone
            // the icnsImageTag[..., ...] values are never itNone here
            if IsBlack and
              ((icnsFindRawImage(RawImages, icnsImageTag[iis, ipf4bit])<>nil) or
               (icnsFindRawImage(RawImages, icnsImageTag[iis, ipf8bit])<>nil)) then
            PageDropped := True; // this is just a mask, do not load this as an image
          end;
          *)

          if not PageDropped then Doc.NewPage.Layers.Assign(bm)
          else inc(IcnsPagesDropped);
        finally
          bm.Free;
        end;
      end; // for i

    // success
    Result := True;
  except
  end;
end;

// SAVING

function icnsParseImageSize(w, h: integer): TicnsImageSize;
var
  i: TicnsImageSize;

begin
  for i in TicnsImageSize do
    if (icnsWidth[i] = w) and (icnsHeight[i] = h) then
      Exit(i);
  Result := iisNone;
end;

procedure icnsSavePage(var RawImages: TicnsRawImages; bm: TBitmap32;
  iis: TicnsImageSize; ipf: TicnsPixelFormat);
const
  MIN_RUN_LENGTH = 3;
  MAX_RUN_LENGTH = 130;

var
  i, j, PixelCount, DestIndex: integer;
  iNext, Channel, Start, RunLength, RunEnd, ColorValue: integer;
  b: byte;
  pSrc: PColor32;
  pb: PByteArray;
  ImageTag, MaskTag: TicnsTag;
  ri: PicnsRawImage;
  Big, Write0: boolean;
  st: TStream;

begin
  ImageTag := icnsImageTag[iis, ipf];
  if ImageTag = itNone then Exit;
  MaskTag := icnsMaskTag[iis, ipf];
  PixelCount := bm.Width*bm.Height;

  // WRITE IMAGE
  ri := icnsFindRawImage(RawImages, ImageTag, True);
  // image write modes: 1-bit, 4-bit, 8-bit, 32-bit RLE, 32-bit JP2
  case ipf of
    ipf1bit: begin
      SetLength(ri.Data, PixelCount div 4); // 2*PixelCount div 8
      // process image
      pSrc := bm.Data;
      b := 0;
      for i := 0 to PixelCount - 1 do
      begin
        b := b or Byte(pSrc^ = cl32Black);
        if i and 7 = 7 then
        begin
          ri.Data[i shr 3] := b;
          b := 0;
        end
        else
          b := b shl 1;
        Inc(pSrc);
      end;
    end; // ipf1bit

    ipf4bit: begin
      SetLength(ri.Data, PixelCount div 2);
      // process image
      pSrc := bm.Data;
      for i := 0 to PixelCount div 2 - 1 do
      begin
        b := palMac16.IndexOf(pSrc^ and not cl32Opaque) shl 4;
        Inc(pSrc);
        b := b or palMac16.IndexOf(pSrc^ and not cl32Opaque);
        Inc(pSrc);

        ri.Data[i] := b;
      end;
    end; // ipf4bit

    ipf8bit: begin
      SetLength(ri.Data, PixelCount);
      // process image
      pSrc := bm.Data;
      for i := 0 to PixelCount - 1 do
      begin
        ri.Data[i] := palMac256.IndexOf(pSrc^ and not cl32Opaque);
        inc(pSrc);
      end;
    end; // ipf8bit

    ipf32bit: begin
      Big := (ImageTag in [it256x256_32, it512x512_32, it1024x1024_32]);
      if Big then
      begin
        st := TMemoryStream.Create;
        try
          if ImageTag = it1024x1024_32 then
            pngSaveToStream(bm, st, PNG_COMPRESSION_HIGH, 0.0)
          else
            jp2SaveToStream(bm, st, 0); // lossless JP2
          st.Position := 0;
          SetLength(ri.Data, st.Size);
          st.ReadBuffer(ri.Data[0], st.Size);
        finally
          st.Free;
        end;
      end else
      // RLE
      begin
        SetLength(ri.Data, 4 + 6*PixelCount); // this is always enough
        DestIndex := 0;

        // Sometimes 4 zeroes must be written at the beginning...
        Write0 := (iis = iis128); // weird...
        if Write0 then
        begin
          PInteger(@ri.Data[0])^ := 0;
          DestIndex := 4;
        end;

        // compress
        for Channel := 0 to 2 do
        begin
          pb := bm.Data;
          Start := 0; // start of block of different bytes
          i := 0;
          while i <= PixelCount do
          begin
            iNext := i+1;

            if i < PixelCount then
            begin
              // determine run length
              ColorValue := pb[4*i + Channel];
              RunEnd := i+1;
              while (RunEnd < Min(PixelCount, i + MAX_RUN_LENGTH)) and
                (pb[4*RunEnd + Channel] = ColorValue) do
                inc(RunEnd);
              RunLength := RunEnd - i;
            end else
              RunLength := 0;

            // write block of accumulated different bytes
            if (((i = PixelCount) or (RunLength >= MIN_RUN_LENGTH)) and
              (i > Start)) or (i - Start = 128) then
            begin
              // write count
              ri.Data[DestIndex] := i-Start-1;
              inc(DestIndex);
              // write bytes
              for j := Start to i-1 do
              begin
                ri.Data[DestIndex] := pb[4*j + Channel];
                inc(DestIndex);
              end;
              // start a new block of different bytes
              Start := i;
            end; // write uncompressed string

            // write block of same bytes
            if RunLength >= MIN_RUN_LENGTH then
            begin
              // write count
              ri.Data[DestIndex] := RunLength + ($80 - MIN_RUN_LENGTH);
              inc(DestIndex);
              // write byte
              ri.Data[DestIndex] := ColorValue;
              inc(DestIndex);
              // skip repeated bytes
              iNext := RunEnd;
              // start a new block of different bytes
              Start := RunEnd;
            end; // write RLE string

            // iterate
            i := iNext;
          end; // while i
        end; // for channel

        // set length
        SetLength(ri.Data, DestIndex);
      end; // write RLE
    end; // ipf32bit
  end; // case ipf -- write image data

  // WRITE MASK
  if MaskTag <> itNone then
  begin
    ri := icnsFindRawImage(RawImages, MaskTag, True);
    // mask write modes: none, 1-bit, 8-bit
    case ipf of
      ipf1bit, ipf4bit, ipf8bit: // 1-bit mask
      begin
        if Length(ri.Data) = 0 then
        begin
          // we are the first here -- fill the image with blackness
          SetLength(ri.Data, PixelCount div 4); // 2*PixelCount div 8
          FillChar(ri.Data[0], Length(ri.Data), $ff);
        end;

        // process mask
        pSrc := bm.Data;
        b := 0;
        for i := PixelCount to 2 * PixelCount - 1 do
        begin
          b := b or Byte(pSrc^ and cl32Opaque <> 0);
          if i and 7 = 7 then
          begin
            ri.Data[i shr 3] := b;
            b := 0;
          end
          else
            b := b shl 1;
          Inc(pSrc);
        end; // for i
      end; // indexed pf -- 1bit mask

      ipf32bit: // 8-bit mask (if has mask, then it's always 8-bit!)
      begin
        SetLength(ri.Data, PixelCount);
        pSrc := bm.Data;
        for i := 0 to PixelCount - 1 do
        begin
          ri.Data[i] := PByteArray(pSrc)[3];
          Inc(pSrc);
        end;
      end; // ipf32bit -- 8bit mask
    end; // case ipf -- write mask data
  end; // write mask
end;

procedure icnsSaveToStream;
var
  i, x, y, TotalSize: integer;
  PageDropped, OK: boolean;
  iis: TicnsImageSize;
  ipf: TicnsPixelFormat;
  pf: TPixelFormat32;
  bm, mbm: TBitmap32;
  // The pages which are selected for saving
  PagesToSave: array[TicnsImageSize, TicnsPixelFormat] of TBitmap32;
  // The source of 1-bit mask (these are references, so don't free them)
  Mask1Source: array[TicnsImageSize] of TBitmap32;
  Palette: TiePalette;
  RawImages: TicnsRawImages;
  ls: TLayers;
  fcc: string;

begin
  // init
  SetLength(DroppedPages, 0);
  for iis in TicnsImageSize do
  begin
    Mask1Source[iis] := nil;
    for ipf in TicnsPixelFormat do
      PagesToSave[iis, ipf] := nil;
  end;

  try
    // determine which pages to save
    Palette := TiePalette.Create;
    try
      for i := 0 to Doc.PageCount - 1 do
      begin
        PageDropped := True;

        ls := Doc.Pages[i].Layers;
        iis := icnsParseImageSize(ls.Width, ls.Height);
        if iis <> iisNone then
        begin
          bm := TBitmap32.Create;
          bm.Assign(ls);

          // Get bitmap palette - with no restrictions
          pf := GetPixelFormat32(bm, nil, nil, nil, Palette);
          // Get ICNS pixel format
          // -- lowest value which is applicable, valid and free
          for ipf in TicnsPixelFormat do
          begin
            // valid?
            if not icnsValidFormat[iis, ipf] then Continue;
            // free?
            if PagesToSave[iis, ipf] <> nil then Continue;
            // applicable?
            if (pf >= pf32_24bit) and (ipf <> ipf32bit) then Continue;
            case ipf of
              ipf1bit: if not Palette.IsSubsetOf(palBW) then Continue;
              ipf4bit: if not Palette.IsSubsetOf(palMac16) then Continue;
              ipf8bit: if not Palette.IsSubsetOf(palMac256) then Continue;
            end;
            // mask is OK?
            mbm := Mask1Source[iis];
            if (ipf < ipf32bit) and (mbm <> nil) then
            begin
              OK := True;
              for x := 0 to mbm.Width - 1 do
              for y := 0 to mbm.Height - 1 do
              if (mbm.PixelAddr(x, y)^ and cl32Opaque <>
                bm.PixelAddr(x, y)^ and cl32Opaque) then
              begin
                OK := False;
                Break;
              end;
              if not OK then Continue;
            end;

            // everything is OK!
            PagesToSave[iis, ipf] := bm;
            if ipf < ipf32bit then Mask1Source[iis] := bm;
            PageDropped := False;

            // exit loop
            Break;
          end; // for ICNS pixel format

          if PageDropped then bm.Free;
        end; // if iis <> iisNone

        if PageDropped then
        begin
          SetLength(DroppedPages, Length(DroppedPages) + 1);
          DroppedPages[Length(DroppedPages) - 1] := i;
        end; // if page was dropped
      end; // for i (pages)
    finally
      Palette.Free;
    end;

    // save pages
    SetLength(RawImages, 0);
    for iis in TicnsImageSize do
    for ipf in TicnsPixelFormat do
      if Assigned(PagesToSave[iis, ipf]) then
        icnsSavePage(RawImages, PagesToSave[iis, ipf], iis, ipf);

    // save icns document
    // write signature
    fcc := ICNS_SIGNATURE;
    s.WriteBuffer(fcc[1], 4);

    // write total length
    TotalSize := 8; // header
    for i := 0 to Length(RawImages) - 1 do
      Inc(TotalSize, 8 + Length(RawImages[i].Data));
    TotalSize := BSwapS(TotalSize);
    s.WriteBuffer(TotalSize, 4);

    // write raw images
    for i := 0 to Length(RawImages) - 1 do
    begin
      // write header
      s.WriteBuffer(icnsTagFCC[RawImages[i].Tag][1], 4);
      // write size
      TotalSize := BSwapS(8 + Length(RawImages[i].Data));
      s.WriteBuffer(TotalSize, 4);
      // write data
      s.WriteBuffer(RawImages[i].Data[0], Length(RawImages[i].Data));
    end;
  finally
    // clean up
    for iis in TicnsImageSize do
    for ipf in TicnsPixelFormat do
      if PagesToSave[iis, ipf] <> nil then
        PagesToSave[iis, ipf].Free;
  end; // try
end;

end.

