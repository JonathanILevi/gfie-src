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

// Include this after defining the boxBlurFunc and TBoxBlurColorAverage macros

procedure boxBlurFunc(bm: TBitmap32; Mask: TBitmap1;
  StepCount: integer; Step: PBoxBlurStep);
var
  bmTemp, bmSrc, bmDest: TBitmap32;
  avg: TBoxBlurColorAverage;
  StepIndex, x, y, radLeft, radRight: integer;
  pSub, pAdd: PColor32;

  // Calculate the blurred value of the current pixel
  // and replace it
  procedure CalcPixel; inline;
  var
    pDest: PColor32;

  begin
    if not Assigned(Mask) or Mask[x, y] then
    begin
      pDest := bmDest.PixelAddr(x, y);
      if pDest^ <> cl32Inverted then pDest^ := avg.GetAverage;
    end;
  end;

  procedure SwapBuffers; inline;
  var
    _bm: TBitmap32;
  begin
    _bm := bmSrc;
    bmSrc := bmDest;
    bmDest := _bm;
  end;

begin
  if (bm.Width = 0) or (bm.Height = 0) then Exit;

  bmTemp := TBitmap32.Create;
  try
    avg := TBoxBlurColorAverage.Create;
    try
      bmTemp.Resize(bm.Width, bm.Height);
      bmSrc := bm;
      bmDest := bmTemp;

      for StepIndex := 1 to StepCount do
      begin
        // if Size is even, the two radiuses are different
        // We can decide if radRight or radLeft should be greater
        // if Shift, radRight is greater, else radLeft is greater
        radLeft := Step.Size div 2;
        if not Odd(Step.Size) and Step.Shift then dec(radLeft);
        radRight := Step.Size - 1 - radLeft;

        // Do horizontal blur
        // Only horizontal stripes of pixels are averaged first
        for y := 0 to bmSrc.Height - 1 do
        begin
          // initialize
          avg.Clear;
          if Step.Toric then
            for x := -radLeft-1 to radRight-1 do
              avg.Add(bmSrc.PixelAddr(SignedMod(x, bmSrc.Width), y)^) else
            for x := 0 to Min(bmSrc.Width, radRight) - 1 do
              avg.Add(bmSrc.PixelAddr(x, y)^);

          // exclude leftmost point and include rightmost point in each step
          if Step.Toric then
          for x := 0 to bmSrc.Width - 1 do
          begin
            avg.Sub(bmSrc.PixelAddr(SignedMod(x - (radLeft+1), bmSrc.Width), y)^);
            avg.Add(bmSrc.PixelAddr(SignedMod(x + radRight, bmSrc.Width), y)^);

            CalcPixel;
          end else // for x
          // not toric
          begin
            pSub := bmSrc.PixelAddr(-(radLeft+1), y);
            pAdd := bmSrc.PixelAddr(radRight, y);
            for x := 0 to bmSrc.Width - 1 do
            begin
              if x > radLeft then
                avg.Sub(pSub^);
              if x < bmSrc.Width-1 - radRight then
                avg.Add(pAdd^);

              CalcPixel;
              inc(pSub);
              inc(pAdd);
            end; // for x
          end; // if not toric
        end; // for y
        // finished
        SwapBuffers;

        // Do vertical blur
        for x := 0 to bmSrc.Width - 1 do
        begin
          // initialize
          avg.Clear;
          if Step.Toric then
            for y := -radLeft-1 to radRight-1 do
              avg.Add(bmSrc.PixelAddr(x, SignedMod(y, bm.Height))^) else
            for y := 0 to Min(bmSrc.Height, radRight) - 1 do
              avg.Add(bmSrc.PixelAddr(x, y)^);

          // exclude topmost point and include bottommost point in each step
          if Step.Toric then
          for y := 0 to bmSrc.Height - 1 do
          begin
            avg.Sub(bmSrc.PixelAddr(x, SignedMod(y - (radLeft+1), bmSrc.Height))^);
            avg.Add(bmSrc.PixelAddr(x, SignedMod(y + radRight, bmSrc.Height))^);

            CalcPixel;
          end else // for x
          // not toric
          begin
            pSub := bmSrc.PixelAddr(x, -(radLeft+1));
            pAdd := bmSrc.PixelAddr(x, radRight);
            for y := 0 to bmSrc.Height - 1 do
            begin
              if y > radLeft then
                avg.Sub(pSub^);
              if y < bmSrc.Height-1 - radRight then
                avg.Add(pAdd^);

              CalcPixel;
              inc(pSub, bmSrc.Width);
              inc(pAdd, bmSrc.Width);
            end; // for y
          end; // if not toric
        end; // for x
        // finished
        SwapBuffers;

        // iterate
        inc(Step);
      end; // for steps

      // we swapped buffers an even number of times
      // so the result (bmSrc) must be in bm
      Assert(bmSrc = bm);
    finally
      avg.Free;
    end;
  finally
    bmTemp.Free;
  end;
end;

