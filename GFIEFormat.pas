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
// GFIE native document format reading and writing
unit GFIEFormat;

interface

uses
  LclIntf, LclType,
  SysUtils, Classes, Graphics, DocClass;

function gfieDetect(Data: Pointer; Size: integer): boolean;
function gfieLoadFromStream(Doc: TIconDoc; s: TStream;
  MaxWidth, MaxHeight: integer): boolean;
procedure gfieSaveToStream(Doc: TIconDoc; s: TStream; compressed: boolean);

implementation

uses
  Math, StrUtils, BitmapEx, bmExUtils, Layers, BlendModes, gfDataTree, BMP, PNG, ieShared;

// Converting enums to string

const
  SelStateToStr: array[TSelState] of string =
    ('none', 'selecting', 'floating');

function StrToSelState(s: string): TSelState;
var
  i: TSelState;

begin
  s := UpperCase(s);

  for i in TSelState do
    if s = UpperCase(SelStateToStr[i]) then Exit(i);
  Result := stNone;
end;

const
  BlendModeToStr: array[TBlendMode] of string =
    ('normal', 'mask', 'behind', 'dissolve',
     'hue', 'hueShift', 'saturation',
     'darken', 'multiply', 'colorBurn', 'linearBurn', 'darkerColor',
     'lighten', 'screen', 'colorDodge', 'linearDodge', 'lighterColor',
     'overlay', 'softLight', 'hardLight',
     'vividLight', 'linearLight', 'pinLight', 'hardMix',
     'difference', 'exclusion');

function StrToBlendMode(s: string): TBlendMode;
var
  i: TBlendMode;

begin
  s := UpperCase(s);

  for i in TBlendMode do
    if s = UpperCase(BlendModeToStr[i]) then Exit(i);
  Result := bmNormal;
end;

function gfieDetect;
begin
  Result := (Size >= 4) and (PCardinal(Data)^ = GF_DATA_TREE_SIG);
end;

function gfieLoadFromStream;
var
  i, j: integer;
  p: TPoint;
  t: TgfDataTree;
  pg: TDocPage;
  ls: TLayers;
  l: TLayer;

  function ReadPoint: TPoint;
  begin
    if t.Descend('x') then begin Result.X := t.Node.AsInt; t.Ascend; end else Result.X := 0;
    if t.Descend('y') then begin Result.Y := t.Node.AsInt; t.Ascend; end else Result.Y := 0;
  end;

  function ReadRect: TRect;
  begin
    if t.Descend('left') then begin Result.Left := t.Node.AsInt; t.Ascend; end else Result.Left := 0;
    if t.Descend('top') then begin Result.Top := t.Node.AsInt; t.Ascend; end else Result.Top := 0;
    if t.Descend('right') then begin Result.Right := t.Node.AsInt; t.Ascend; end else Result.Right := 0;
    if t.Descend('bottom') then begin Result.Bottom := t.Node.AsInt; t.Ascend; end else Result.Bottom := 0;
  end;

  procedure ReadRawImage(bm: TBitmap32; const fmt: string);
  var
    sMem: TMemoryStream;
    dpi: double;

  begin
    sMem := TMemoryStream.Create;
    try
      sMem.Size := t.Node.DataSize;
      Move(t.Node.Data^, sMem.Memory^, t.Node.DataSize);

      if fmt = 'BMP' then
        bmpLoadFromStream(bm, sMem) else
      if fmt = 'PNG' then
        pngLoadFromStream(bm, sMem, dpi);

      bm.Resize(Min(bm.Width, MaxWidth), Min(bm.Height, MaxHeight));
    finally
      sMem.Free;
    end;
  end;

  procedure ReadBitmap32(bm: TBitmap32);
  var
    i: integer;
    pSrc, pDest: PColor32;
    fmt: string;
    bmTemp: TBitmap32;

  begin
    // get image format
    if not t.Descend('format') then Exit;
      fmt := UpperCase(t.Node.AsString);
    t.Ascend;

    if (fmt <> 'BMP') and (fmt <> 'PNG') then Exit;

    // load image
    if not t.Descend('data') then Exit;
      ReadRawImage(bm, fmt);
    t.Ascend;

    // load inversion mask
    if t.Descend('inversionMask') then
    begin
      bmTemp := TBitmap32.Create;
      try
        ReadRawImage(bmTemp, fmt);
        t.Ascend;

        pSrc := bmTemp.Data;
        pDest := bm.Data;
        for i := 1 to bm.Width * bm.Height do
        begin
          if pSrc^ = cl32White then pDest^ := cl32Inverted;
          inc(pSrc); inc(pDest);
        end;
      finally
        bmTemp.Free;
      end;
    end; // inversion mask
  end;

  procedure ReadBitmap1(bm: TBitmap1);
  var
    bm32: TBitmap32;

  begin
    bm32 := TBitmap32.Create;
    try
      ReadBitmap32(bm32);
      bm.Assign(bm32);
    finally
      bm32.Free;
    end;
  end;

begin
  Result := False;

  try
    t := TgfDataTree.Create;
    try
      // load data file
      if not t.LoadFromStream(s) then Exit;

      // clear the document
      Doc.Clear;

      // load metadata
      if t.Descend('metadata') then
      with Doc.Metadata do
      begin
        if t.Descend('title') then begin Title := t.Node.AsString; t.Ascend; end;
        if t.Descend('author') then begin Author := t.Node.AsString; t.Ascend; end;
        if t.Descend('copyright') then begin Copyright := t.Node.AsString; t.Ascend; end;
        if t.Descend('comments') then begin Comments := t.Node.AsString; t.Ascend; end;
        if t.Descend('loopCount') then begin LoopCount := t.Node.AsInt; t.Ascend; end;
        if t.Descend('dpi') then begin DPI := t.Node.AsDouble; t.Ascend; end;

        t.Ascend;
      end;

      // load pages
      if t.Descend('pages') then
      begin
        for i := 0 to High(i) do
        begin
          if not t.Descend('page' + IntToStr(i)) then Break;

          pg := Doc.NewPage;

          // load layers
          if t.Descend('layers') then
          begin
            ls := pg.Layers;

            // load dimensions
            if t.Descend('size') then begin p := ReadPoint; ls.Resize(Min(MaxWidth, p.X), Min(MaxHeight, p.Y)); t.Ascend; end;

            // load layer objects
            for j := 0 to High(j) do
            begin
              if not t.Descend('layer' + IntToStr(j)) then Break;

              l := ls.NewLayer;

              if t.Descend('name') then begin l.Name := t.Node.AsString; t.Ascend; end;
              if t.Descend('visible') then begin l.Visible := t.Node.AsBoolean; t.Ascend; end;
              if t.Descend('selected') then begin l.Selected := t.Node.AsBoolean; t.Ascend; end;
              if t.Descend('image') then begin ReadBitmap32(l.Image); t.Ascend; end;
              if t.Descend('opacity') then begin l.Opacity := t.Node.AsInt; t.Ascend; end;
              if t.Descend('blendMode') then begin l.BlendMode := StrToBlendMode(t.Node.AsString); t.Ascend; end;

              t.Ascend;
            end; // if descend to layerX

            // load selection
            if t.Descend('selection') then
            begin
              if t.Descend('state') then begin ls.SelState := StrToSelState(t.Node.AsString); t.Ascend; end;

              case ls.SelState of
                stSelecting: if t.Descend('mask') then
                begin
                  ReadBitmap1(ls.Selection.Mask);
                  t.Ascend;
                end;

                stFloating: begin
                  if t.Descend('image') then begin ReadBitmap32(ls.Selection.Image); t.Ascend; end;
                  if t.Descend('box') then begin ls.Selection.Box := ReadRect; t.Ascend; end;
                  if t.Descend('angle') then begin ls.Selection.Angle := t.Node.AsDouble; t.Ascend; end;
                  if t.Descend('depth') then begin ls.Selection.Depth := t.Node.AsInt; t.Ascend; end;
                end;
              end; // case selection state
            
              t.Ascend;
            end; // if descend to selection

            t.Ascend;
          end; // if descend to layers

          // load etc.
          if t.Descend('hotSpot') then begin pg.HotSpot := ReadPoint; t.Ascend; end;
          if t.Descend('frameRate') then begin pg.FrameRate := t.Node.AsInt; t.Ascend; end;
          if t.Descend('dpi') then begin pg.DPI := t.Node.AsDouble; t.Ascend; end;

          t.Ascend;
        end; // if descend to pageX

        t.Ascend;
      end; // if descend to pages
    finally
      t.Free;
    end;

    // success
    Result := True;
  except
  end;
end;

procedure gfieSaveToStream;
var
  i, j: integer;
  t: TgfDataTree;
  pg: TDocPage;
  ls: TLayers;
  l: TLayer;

  procedure WritePoint(const Value: TPoint);
  begin
    t.NewChild('x'); t.Node.AsInt := Value.X; t.Ascend;
    t.NewChild('y'); t.Node.AsInt := Value.Y; t.Ascend;
  end;

  procedure WriteRect(const Value: TRect);
  begin
    t.NewChild('left'); t.Node.AsInt := Value.Left; t.Ascend;
    t.NewChild('top'); t.Node.AsInt := Value.Top; t.Ascend;
    t.NewChild('right'); t.Node.AsInt := Value.Right; t.Ascend;
    t.NewChild('bottom'); t.Node.AsInt := Value.Bottom; t.Ascend;
  end;

  procedure WriteRawImage(bm: TBitmap32);
  var
    sMem: TMemoryStream;
    
  begin
    sMem := TMemoryStream.Create;
    try
      if compressed then
        pngSaveToStream(bm, sMem, PNG_COMPRESSION_HIGH, 0.0)
      else
        bmpSaveToStream(bm, sMem);

      t.Node.DataSize := sMem.Size;
      Move(sMem.Memory^, t.Node.Data^, sMem.Size);
    finally
      sMem.Free;
    end;
  end;

  procedure WriteBitmap32(bm: TBitmap32);
  var
    i: integer;
    pSrc, pDest: PColor32;
    bmTemp: TBitmap32;

  begin
    // write format
    t.NewChild('format');
    t.Node.AsString := StrUtils.IfThen(compressed, 'png', 'bmp');
    t.Ascend;

    // write image data
    t.NewChild('data');
    WriteRawImage(bm);
    t.Ascend;

    // write inversion mask
    if bm.HasColor(cl32Inverted) then
    begin
      bmTemp := TBitmap32.Create;
      try
        bmTemp.Resize(bm.Width, bm.Height);

        pSrc := bm.Data;
        pDest := bmTemp.Data;
        for i := 1 to bm.Width * bm.Height do
        begin
          if pSrc^ = cl32Inverted then pDest^ := cl32White;
          inc(pSrc); inc(pDest);
        end;

        t.NewChild('inversionMask');
        WriteRawImage(bmTemp);
        t.Ascend;
      finally
        bmTemp.Free;
      end;
    end; // inversion mask
  end;

  procedure WriteBitmap1(bm: TBitmap1);
  var
    bm32: TBitmap32;

  begin
    bm32 := TBitmap32.Create;
    try
      bm32.Resize(bm.Width, bm.Height);
      bm32.FillColor(cl32Black);
      bm32.Draw(0, 0, bm);
      
      WriteBitmap32(bm32);
    finally
      bm32.Free;
    end;
  end;

begin
  t := TgfDataTree.Create;
  try
    // save metadata
    t.NewChild('metadata');
    with Doc.Metadata do
    begin
      if Title <> '' then begin t.NewChild('title'); t.Node.AsString := Title; t.Ascend; end;
      if Author <> '' then begin t.NewChild('author'); t.Node.AsString := Author; t.Ascend; end;
      if Copyright <> '' then begin t.NewChild('copyright'); t.Node.AsString := Copyright; t.Ascend; end;
      if Comments <> '' then begin t.NewChild('comments'); t.Node.AsString := Comments; t.Ascend; end;
      t.NewChild('loopCount'); t.Node.AsInt := LoopCount; t.Ascend;
      if DPI > 0 then begin t.NewChild('dpi'); t.Node.AsDouble := DPI; t.Ascend; end;
    end;
    t.Ascend;

    // save pages
    t.NewChild('pages');
      for i := 0 to Doc.PageCount - 1 do
      begin
        t.NewChild('page' + IntToStr(i));
        pg := Doc.Pages[i];

        // save layers
        t.NewChild('layers');
          ls := pg.Layers;

          // save dimensions
          t.NewChild('size'); WritePoint(Point(ls.Width, ls.Height)); t.Ascend;

          // save layer objects
          for j := 0 to ls.LayerCount - 1 do
          begin
            t.NewChild('layer' + IntToStr(j));
              l := ls[j];

              t.NewChild('name'); t.Node.AsString := l.Name; t.Ascend;
              t.NewChild('visible'); t.Node.AsBoolean := l.Visible; t.Ascend;
              t.NewChild('selected'); t.Node.AsBoolean := l.Selected; t.Ascend;
              t.NewChild('image'); WriteBitmap32(l.Image); t.Ascend;
              t.NewChild('opacity'); t.Node.AsInt := l.Opacity; t.Ascend;
              t.NewChild('blendMode'); t.Node.AsString := BlendModeToStr[l.BlendMode]; t.Ascend;
            t.Ascend; // layer
          end;

          // save selection
          t.NewChild('selection');
            t.NewChild('state'); t.Node.AsString := SelStateToStr[ls.SelState]; t.Ascend;

            case ls.SelState of
              stSelecting: begin
                t.NewChild('mask'); WriteBitmap1(ls.Selection.Mask); t.Ascend;
              end;

              stFloating: begin
                t.NewChild('image'); WriteBitmap32(ls.Selection.Image); t.Ascend;
                t.NewChild('box'); WriteRect(ls.Selection.Box); t.Ascend;
                t.NewChild('angle'); t.Node.AsDouble := ls.Selection.Angle; t.Ascend;
                t.NewChild('depth'); t.Node.AsInt := ls.Selection.Depth; t.Ascend;
              end;
            end; // case selection state
          t.Ascend; // selection
        t.Ascend; // layers

        t.NewChild('hotSpot'); WritePoint(pg.HotSpot); t.Ascend;
        t.NewChild('frameRate'); t.Node.AsInt := pg.FrameRate; t.Ascend;
        if pg.DPI > 0 then begin t.NewChild('dpi'); t.Node.AsDouble := pg.DPI; t.Ascend; end;

        t.Ascend; // pageX
      end; // for i

    t.Ascend; // pages

    // save data file
    t.SaveToStream(s);
  finally
    t.Free;
  end;
end;

end.
