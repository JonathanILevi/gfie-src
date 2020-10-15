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
unit ANI;

interface

uses
  LclIntf, LclType, Types, Classes, DocClass;

function aniDetect(Data: Pointer; Size: integer): boolean;
function aniLoadFromStream(Doc: TIconDoc; s: TStream): boolean;
procedure aniSaveToStream(Doc: TIconDoc; s: TStream; PNGLimit: integer);

implementation

uses
  BitmapEx, bmExUtils, ICO, RIFF, fnvHash;

const
  // Animation flags for ANI files
  AF_ICON = 1;
  AF_SEQUENCE = 2;

type
  ANIHEADER = packed record
    cbSize, NumFrames, NumSteps, Width, Height,
      BitCount, Planes, JifRate, Flags: integer;
  end;

function aniDetect;
const
  FCC_RIFF = $46464952;
  FCC_ACON = $4e4f4341;

begin
  Result := (Size >= 12) and
    (PCardinal(Data)^ = FCC_RIFF) and
    (PIntegerArray(Data)[2] = FCC_ACON);
end;

function aniLoadFromStream;
var
  i, j, ChunkSize: integer;
  aRate, aSeq: TArrayOfInteger;
  rr: TRIFFReader;
  Doc2: TIconDoc;
  Page: TDocPage;
  anih: ANIHEADER;
  IsRateChunk, IsSeqChunk: boolean;

  procedure ReadInfo(const Chunk: string; var Data: string);
  begin
    if rr.Seek('RIFF(ACON).LIST(INFO).' + Chunk, 1, @ChunkSize) then
    begin
      SetLength(Data, ChunkSize);
      s.ReadBuffer(Data[1], ChunkSize);

      // trim trailing zeroes
      Data := String(PChar(Data));
    end;
  end;

begin
  Result := False;
  try
    Doc.Clear;

    rr := TRIFFReader.Create(s);
    try
      // read metadata
      with Doc.Metadata do
      begin
        ReadInfo('INAM', Title);
        ReadInfo('IART', Author);
        ReadInfo('ICOP', Copyright);
        ReadInfo('ICMT', Comments);
      end;

      // read ani header
      if not rr.Seek('RIFF(ACON).anih', 1, nil) then Exit;
      s.ReadBuffer(anih, SizeOf(anih));
      // TODO: DIB-format animated cursors are not yet supported
      if anih.Flags and AF_ICON = 0 then Exit;

      // get rate and sequence data, if present
      IsRateChunk := rr.Seek('RIFF(ACON).rate', 1, @ChunkSize) and
        (ChunkSize = 4*anih.NumSteps);
      if IsRateChunk then
      begin
        SetLength(aRate, anih.NumSteps);
        s.ReadBuffer(aRate[0], ChunkSize);
      end;

      IsSeqChunk := (anih.Flags and AF_SEQUENCE <> 0) and
        rr.Seek('RIFF(ACON).seq ', 1, @ChunkSize) and
        (ChunkSize = 4*anih.NumSteps);
      if IsSeqChunk then
      begin
        SetLength(aSeq, anih.NumSteps);
        s.ReadBuffer(aSeq[0], ChunkSize);
      end;

      // read frames
      Doc2 := TIconDoc.Create;
      try
        for i := 0 to anih.NumSteps - 1 do
        begin
          // get image index from sequence data
          if IsSeqChunk then j := aSeq[i] else j := i;
          // seek
          if not rr.Seek('RIFF(ACON).LIST(fram).icon', j + 1, nil) then Exit;

          // read icon image
          if not icoLoadFromStream(Doc2, s) then Exit;

          Page := Doc.NewPage;
          Page.Assign(Doc2.Pages[0]);

          // set frame rate
          if IsRateChunk then
            Page.FrameRate := aRate[i] else
            Page.FrameRate := anih.JifRate;
          Page.FrameRate := (Page.FrameRate * 1000 + 30) div 60;
        end; // for i
      finally
        Doc2.Free;
      end;

      // seek after RIFF file
      s.Position := rr.EndOfFile;
    finally
      rr.Free;
    end;

    // success
    Result := True;
  except
  end;
end;

// The order of RIFF chunks is very important!
// Some virus scanners can report wrongly organized ANI files as suspicious
procedure aniSaveToStream;
var
  i, j, zero, FrameRate0, FramesWritten: integer;
  WriteSeq: boolean;
  aRate, aSeq: TArrayOfInteger;
  WriteFrame: array of boolean;
  aHash: array of TfnvKey;
  rw: TRIFFWriter;
  Doc2: TIconDoc;
  bm: TBitmap32;
  anih: ANIHEADER;

  procedure WriteInfo(const Chunk, Data: string);
  begin
    if Data <> '' then
    begin
      rw.Descend(Chunk);
        rw.WriteString(Data);
        s.WriteBuffer(zero, 1);
      rw.Ascend;
    end;
  end;

begin
  zero := 0;
  rw := TRIFFWriter.Create(s);
  try
    rw.Descend('RIFF'); rw.WriteString('ACON');
      // write info
      with Doc.Metadata do
      if (Title <> '') or (Author <> '') or
        (Copyright <> '') or (Comments <> '') then
      begin
        rw.Descend('LIST'); rw.WriteString('INFO');
          WriteInfo('INAM', Title);
          WriteInfo('IART', Author);
          WriteInfo('ICOP', Copyright);
          WriteInfo('ICMT', Comments);
        rw.Ascend;
      end;

      // hash all images to find duplicates and produce seq chunk
      SetLength(aHash, Doc.PageCount);
      SetLength(aSeq, Doc.PageCount);
      WriteSeq := False;
      SetLength(WriteFrame, Doc.PageCount);
      FramesWritten := 0;

      bm := TBitmap32.Create;
      try
        for i := 0 to Doc.PageCount - 1 do
        with Doc.Pages[i] do
        begin
          // hash layers image and page hot spot
          bm.Assign(Layers);
          aHash[i] := fnvAppend(bm.Hash(False), @HotSpot, SizeOf(HotSpot));

          // find a duplicate frame
          WriteFrame[i] := True;
          for j := 0 to i - 1 do
          if aHash[i] = aHash[j] then
          // don't save the frame
          begin
            WriteFrame[i] := False;
            WriteSeq := True;
            aSeq[i] := aSeq[j];

            Break;
          end;

          if WriteFrame[i] then
          begin
            inc(FramesWritten);
            aSeq[i] := FramesWritten - 1;
          end;
        end;
      finally
        bm.Free;
      end;

      // write ANI header
      FrameRate0 := Doc.Pages[0].FrameRate;
      FillChar(anih, SizeOf(anih), 0);
      with anih do
      begin
        cbSize := SizeOf(anih);
        NumFrames := FramesWritten;
        NumSteps := Doc.PageCount;
        JifRate := (FrameRate0 * 60 + 500) div 1000;
        Flags := AF_ICON or AF_SEQUENCE;
      end;

      rw.Descend('anih');
        s.WriteBuffer(anih, SizeOf(anih));
      rw.Ascend; // anih

      // write frame rates, if needed
      for i := 0 to Doc.PageCount - 1 do
        if Doc.Pages[i].FrameRate <> FrameRate0 then
      begin
        SetLength(aRate, Doc.PageCount);
        for j := 0 to Doc.PageCount - 1 do
          aRate[j] := (Doc.Pages[j].FrameRate * 60 + 500) div 1000;

        rw.Descend('rate');
          s.WriteBuffer(aRate[0], Doc.PageCount * 4);
        rw.Ascend; // rate

        Break;
      end;

      // write sequence data
      if WriteSeq then
      begin
        rw.Descend('seq ');
          s.WriteBuffer(aSeq[0], Doc.PageCount * 4);
        rw.Ascend;
      end;

      // write frames and produce sequence data
      rw.Descend('LIST'); rw.WriteString('fram');
        Doc2 := TIconDoc.Create;
        try
          Doc2.NewPage;
          
          for i := 0 to Doc.PageCount - 1 do if WriteFrame[i] then
          begin
            Doc2.Pages[0].Assign(Doc.Pages[i]);

            rw.Descend('icon');
              icoSaveToStream(Doc2, s, True, PNGLimit);
            rw.Ascend;
          end; // for i
        finally
          Doc2.Free;
        end;
      rw.Ascend; // LIST - fram
    rw.Ascend; // RIFF - ACON
  finally
    rw.Free;
  end;
end;

end.
