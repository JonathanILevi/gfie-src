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
unit dlgDocPage;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, NumberEdit, AdjustControl, ExtCtrls, PixelFormats,
  ieShared, ImageAnchors, bmExUtils, DocClass, Layers, ComCtrls;

const
  cbSize_ItemIndex_ToSize: array[0..8] of integer =
    (16, 20, 24, 32, 48, 64, 256, 512, 1024);

type
  TdpWhenResizing = (wrStretch, wrCrop);

  TdpSettings = record
    // Input parameters
    Caption: string;
    showColors: boolean;
    showCreateFrom: boolean;
    showWhenResizing: boolean;

    // Input/output parameters
    Width: integer;
    Height: integer;
    FrameRate: integer;
    DPI: double;

    // Output parameters
    cr: TColorReduction;
    CreateFrom: boolean;
    WhenResizing: TdpWhenResizing;
    AnchorHoriz: TImageAnchorHoriz;
    AnchorVert: TImageAnchorVert;
  end;

  { TfrmDocPage }

  TfrmDocPage = class(TForm)
    alDPI: TAdjustLabel;
    bOK: TButton;
    bCancel: TButton;
    cbCreateFrom: TCheckBox;
    gbWhenResizing: TGroupBox;
    iaCrop: TImageAnchors;
    neDPI: TNumberEdit;
    rbStretch: TRadioButton;
    rbCrop: TRadioButton;
    lSize: TLabel;
    lCR: TLabel;
    gbCustomSize: TGroupBox;
    alWidth: TAdjustLabel;
    alHeight: TAdjustLabel;
    neWidth: TNumberEdit;
    neHeight: TNumberEdit;
    cbSquare: TCheckBox;
    cbSize: TComboBox;
    cbCR: TComboBox;
    gbOther: TGroupBox;
    alFrameRate: TAdjustLabel;
    neFrameRate: TNumberEdit;
    procedure neWidthChange(Sender: TObject);
    procedure neHeightChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbSquareClick(Sender: TObject);
    procedure cbSizeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure iaCropClick(Sender: TObject);
  private
    procedure GetSize(var w, h: integer);
    procedure SetSize(w, h: integer);
    procedure SetSizeComboBox;
  public
    frmUpdating: integer;

    procedure ApplyLanguagePack;
    function Execute(var Settings: TdpSettings): boolean;
    // Resizes the page and performs color reduction
    // Note: resizing also affects selection and hot spot
    procedure ConvertSizeAndColors(pg: TDocPage; const st: TdpSettings);
  end;

var
  frmDocPage: TfrmDocPage;

implementation

{$R *.lfm}

uses LangPack;

procedure TfrmDocPage.GetSize(var w, h: integer);
begin
  if cbSize.ItemIndex = cbSize.Items.Count - 1 then
  begin
    w := Round(neWidth.Value);
    h := Round(neHeight.Value);
  end else
  begin
    w := cbSize_ItemIndex_ToSize[cbSize.ItemIndex];
    h := w;
  end;
end;

procedure TfrmDocPage.SetSize(w, h: integer);
begin
  inc(frmUpdating);
    cbSquare.Checked := w = h;
    neWidth.Value := w;
    neHeight.Value := h;
  dec(frmUpdating);

  SetSizeComboBox;
end;

procedure TfrmDocPage.SetSizeComboBox;
var
  i, w, h: integer;

begin
  w := Round(neWidth.Value);
  h := Round(neHeight.Value);

  inc(frmUpdating);
    cbSize.ItemIndex := cbSize.Items.Count - 1;
    if w = h then for i := 0 to Length(cbSize_ItemIndex_ToSize) - 1 do
      if w = cbSize_ItemIndex_ToSize[i] then cbSize.ItemIndex := i;
  dec(frmUpdating);
end;

procedure TfrmDocPage.ApplyLanguagePack;
begin
  lSize.Caption := lpGet('LABEL_SIZE')+':';
  SetComboItems(cbSize,
    Format('16 x 16 (%s)'+LineEnding+
      '20 x 20'+LineEnding+
      '24 x 24 (%s)'+LineEnding+
      '32 x 32 (%s)'+LineEnding+
      '48 x 48 (%s)'+LineEnding+
      '64 x 64 (%s)'+LineEnding+
      '256 x 256 (%s)'+LineEnding+
      '512 x 512 (%s)'+LineEnding+
      '1024 x 1024 (%s)'+LineEnding+
      '%s',
      [lpGet('PG_SIZE_SMALLEST'), lpGet('PG_SIZE_TOOLBAR'),
       lpGet('PG_SIZE_SMALL'), lpGet('PG_SIZE_MEDIUM'),
       lpGet('PG_SIZE_LARGE'), lpGet('PG_SIZE_HUGE_VISTA'),
       lpGet('PG_SIZE_APPLE'), lpGet('PG_SIZE_APPLE'),
       lpGet('PG_CUSTOM_SIZE')]));

  gbCustomSize.Caption := lpGet('PG_CUSTOM_SIZE');
  alWidth.Caption := lpGet('LABEL_WIDTH')+':';
  alHeight.Caption := lpGet('LABEL_HEIGHT')+':';
  cbSquare.Caption := lpGet('PG_SQUARE');

  lCR.Caption := lpGet('PG_CR')+':';
  SetComboItems(cbCR,
    lpGet('PG_CR_BW')+LineEnding+
    lpGet('PG_CR_16_WIN')+LineEnding+lpGet('PG_CR_16_MAC')+LineEnding+
    lpGet('PG_CR_256_ADAPT')+LineEnding+lpGet('PG_CR_256_MAC')+LineEnding+
    lpGet('PG_CR_24')+LineEnding+lpGet('PG_CR_32'));

  gbOther.Caption := lpGet('PG_OTHER');
  alFrameRate.Caption := lpGet('PG_FRAME_RATE')+':';
  alDPI.Caption := lpGet('PG_DPI')+':';

  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');

  cbCreateFrom.Caption := lpGet('PG_CREATE_FROM_SELECTED');

  gbWhenResizing.Caption := lpGet('PG_WHEN_RESIZING');
  rbStretch.Caption := lpGet('PG_STRETCH');
  rbCrop.Caption := lpGet('PG_CROP');
  iaCrop.Hint := lpGet('PG_IMAGE_ANCHORS');
end;

function TfrmDocPage.Execute(var Settings: TdpSettings): boolean;
begin
  // set up controls and look
  Caption := Settings.Caption;

  lCR.Visible := Settings.showColors;
  cbCR.Visible := Settings.showColors;
  cbCreateFrom.Visible := Settings.showCreateFrom;
  gbWhenResizing.Visible := Settings.showWhenResizing;

  // update values
  SetSize(Settings.Width, Settings.Height);
  cbCR.ItemIndex := Ord(cr32);
  neFrameRate.Value := Settings.FrameRate;
  neDPI.Value := Settings.DPI;

  Result := ShowModal = mrOk;
  if Result then
  begin
    GetSize(Settings.Width, Settings.Height);
    Settings.cr := TColorReduction(cbCR.ItemIndex);
    Settings.FrameRate := Round(neFrameRate.Value);
    Settings.DPI := neDPI.Value;

    Settings.CreateFrom := cbCreateFrom.Checked;
    if rbStretch.Checked then Settings.WhenResizing := wrStretch else
    begin
      Settings.WhenResizing := wrCrop;
      Settings.AnchorHoriz := iaCrop.AnchorHoriz;
      Settings.AnchorVert := iaCrop.AnchorVert;
    end;
  end; // if Result
end;

procedure TfrmDocPage.ConvertSizeAndColors(pg: TDocPage; const st: TdpSettings);
var
  i, xOffs, yOffs: integer;
  pgDest: TDocPage;
  lSrc, lDest: TLayer;
  sSrc: TSelInfo;

  function xTransform(q: integer): integer;
  begin
    case st.WhenResizing of
      wrStretch: Result := Round(q / pg.Layers.Width * pgDest.Layers.Width);
      wrCrop: Result := q + xOffs;
    end;
  end;

  function yTransform(q: integer): integer;
  begin
    case st.WhenResizing of
      wrStretch: Result := Round(q / pg.Layers.Height * pgDest.Layers.Height);
      wrCrop: Result := q + yOffs;
    end;
  end;

begin
  // no change?
  if (st.Width = pg.Layers.Width) and (st.Height = pg.Layers.Height) and
    (st.cr = cr32) then Exit;
    
  pgDest := TDocPage.Create;
  try
    pgDest.Layers.Assign(pg.Layers); // This will transfer visibility, caption, etc.
    pgDest.Layers.Resize(st.Width, st.Height);
    
    // determine position where the source layers will be painted
    if st.WhenResizing = wrCrop then
    begin
      case st.AnchorHoriz of
        iahLeft: xOffs := 0;
        iahCenter: xOffs := (pgDest.Layers.Width - pg.Layers.Width) div 2;
        iahRight: xOffs := pgDest.Layers.Width - pg.Layers.Width;
      end;

      case st.AnchorVert of
        iavTop: yOffs := 0;
        iavCenter: yOffs := (pgDest.Layers.Height - pg.Layers.Height) div 2;
        iavBottom: yOffs := pgDest.Layers.Height - pg.Layers.Height;
      end;
    end; // if cropping

    // convert etc.
    pgDest.HotSpot := Point(xTransform(pg.HotSpot.X), yTransform(pg.HotSpot.Y));
    pgDest.FrameRate := pg.FrameRate;
    pgDest.DPI := pg.DPI;

    // convert layers
    for i := 0 to pg.Layers.LayerCount - 1 do
    begin
      lDest := pgDest.Layers[i];
      lSrc := pg.Layers[i];

      lDest.Image.FillTransparent;

      // resample
      case st.WhenResizing of
        wrStretch: lDest.Image.TransformDraw(lDest.Image.ClientRect, 0, lSrc.Image, True);
        wrCrop: lDest.Image.Draw(xOffs, yOffs, lSrc.Image);
      end;

      // perform color reduction
      ReduceColors(lDest.Image, st.cr, nil);
    end;

    // convert selection
    sSrc := pg.Layers.Selection;
    with pgDest.Layers do
    begin
      SelState := pg.Layers.SelState;

      case SelState of
        stSelecting:
        begin
          Selection.Mask.FillTransparent;
          case st.WhenResizing of
            wrStretch: Selection.Mask.TransformDraw(Selection.Mask.ClientRect, 0, sSrc.Mask, False);
            wrCrop: Selection.Mask.Draw(xOffs, yOffs, sSrc.Mask);
          end;
        end;

        stFloating: begin
          Selection.Image.Assign(sSrc.Image);
          ReduceColors(Selection.Image, st.cr, nil);

          with sSrc.Box do Selection.Box :=
            Rect(xTransform(Left), yTransform(Top),
              xTransform(Right), yTransform(Bottom));
          Selection.Angle := sSrc.Angle;
          Selection.Depth := sSrc.Depth;
        end;
      end; // case SelState
    end; // convert selection

    pg.Assign(pgDest);
  finally
    pgDest.Free;
  end;
end;

procedure TfrmDocPage.neWidthChange(Sender: TObject);
begin
  if frmUpdating = 0 then
  begin
    if cbSquare.Checked then
    begin
      inc(frmUpdating);
        neHeight.Value := neWidth.Value;
      dec(frmUpdating);
    end;

    SetSizeComboBox;
  end;
end;

procedure TfrmDocPage.neHeightChange(Sender: TObject);
begin
  if frmUpdating = 0 then
  begin
    if cbSquare.Checked then
    begin
      inc(frmUpdating);
        neWidth.Value := neHeight.Value;
      dec(frmUpdating);
    end;

    SetSizeComboBox;
  end;
end;

procedure TfrmDocPage.FormCreate(Sender: TObject);
begin
  ApplyLanguagePack;
  frmUpdating := 0;
end;

procedure TfrmDocPage.cbSquareClick(Sender: TObject);
begin
  if (frmUpdating = 0) and cbSquare.Checked then neWidthChange(neWidth);
end;

procedure TfrmDocPage.cbSizeChange(Sender: TObject);
begin
  if (frmUpdating = 0) and (cbSize.ItemIndex < cbSize.Items.Count - 1) then
  begin
    inc(frmUpdating);
      neWidth.Value := cbSize_ItemIndex_ToSize[cbSize.ItemIndex];
      neHeight.Value := neWidth.Value;
    dec(frmUpdating);
  end;
end;

procedure TfrmDocPage.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;

  neWidth.Max := Pref_MaxWidth;
  neHeight.Max := Pref_MaxHeight;
end;

procedure TfrmDocPage.iaCropClick(Sender: TObject);
begin
  rbCrop.Checked := True;
end;

end.
