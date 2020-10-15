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
unit dlgPrint;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, NumberEdit, AdjustControl, dlgDoc, BitmapEx,
  Printers, DocClass, PixelFormats, LangPack, PrintersDlgs, FileUtil;

type

  { TfrmPrint }

  TfrmPrint = class(TForm)
    bPrinterSetup: TButton;
    neCopies: TNumberEdit;
    rgPages: TRadioGroup;
    neZoom: TNumberEdit;
    alCopies: TAdjustLabel;
    alZoom: TAdjustLabel;
    lCaption: TLabel;
    eCaption: TEdit;
    bOk: TButton;
    bCancel: TButton;
    psd: TPrinterSetupDialog;
    procedure bPrinterSetupClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    frmDoc: TGraphicFrame;
    procedure ApplyLanguagePack;
    procedure Execute(_frmDoc: TGraphicFrame);
  end;

var
  frmPrint: TfrmPrint;

implementation

{$R *.lfm}

procedure TfrmPrint.Execute(_frmDoc: TGraphicFrame);
var
  i, j, yPos, yNewPos, th: integer;
  ppmm: double;

  procedure ReserveGap(Height: integer);
  begin
    // not enough free place on the current page
    if yPos + Height >= Printer.PageHeight then
    begin
      Printer.NewPage;
      yPos := 0;
      // header
      if eCaption.Text <> '' then
      begin
        Printer.Canvas.TextOut(0, 0, eCaption.Text);
        inc(yPos, th);
      end;
    end;
    
    yNewPos := yPos + Height;
  end;

  // prints a string to a new line
  procedure PrintLn(const s: string);
  begin
    ReserveGap(th);
      Printer.Canvas.TextOut(0, yPos, s);
    yPos := yNewPos;
  end;

  procedure PrintPage(Index: integer);
  var
    i, j: integer;
    bm32: TBitmap32;
    bm: TBitmap;
    q: double;

  begin
    bm32 := TBitmap32.Create;
    try
      bm32.Assign(frmDoc.Doc.Pages[Index].Layers);

      // print size and color depth
      PrintLn(Format('%d x %d @ %s:', [bm32.Width, bm32.Height,
        pf32ToStr[GetPixelFormat32(bm32, palBW, palWin16, nil, nil)]]));

      bm := TBitmap.Create;
      try
        // produce bitmap
        bm.PixelFormat := pf24bit;
        bm.Width := bm32.Width;
        bm.Height := bm32.Height;
        bm.Canvas.Brush.Color := clWhite;
        bm.Canvas.FillRect(Rect(0, 0, bm.Width, bm.Height));
        bm32.DrawTo24(bm, 0, 0);

        // calculate dimensions in logical units
        q := ppmm*25.4/72 * neZoom.Value * 0.01;
        i := Round(bm.Width * q);
        j := Round(bm.Height * q);

        // draw the bitmap to the printer canvas
        ReserveGap(j);
          Printer.Canvas.StretchDraw(Rect(0, yPos, i, yPos + j), bm);
        yPos := yNewPos;
      finally
        bm.Free;
      end;
    finally
      bm32.Free;
    end;
  end;
  
begin
  frmDoc := _frmDoc;

  neCopies.Value := 1;
  neZoom.Value := 100;
  eCaption.Text := SysToUTF8(frmDoc.FileName);
  rgPages.ItemIndex := 0;
  rgPages.Enabled := (frmDoc.Doc.PageCount > 1);

  if ShowModal = mrOk then
  begin
    Printer.Copies := Round(neCopies.Value);
    Printer.BeginDoc;
    try
      // get resolution: pixels per mm
      ppmm := Printer.XDPI / 25.4;
      if ppmm = 0 then ShowMessage(lpGet('MSG_PAGE_SIZE_0')) else
      begin
        // initialize font
        with Printer.Canvas.Font do
        begin
          Name := 'Tahoma';
          Size := 10;
        end;
        th := Printer.Canvas.TextHeight('Mg');

        for j := 1 to Round(neCopies.Value) do
        begin
          yPos := 0;

          if eCaption.Text <> '' then PrintLn(eCaption.Text);

          if rgPages.ItemIndex = 0 then
            for i := 0 to frmDoc.Doc.PageCount - 1 do PrintPage(i) else
            PrintPage(frmDoc.ImageIndex);
        end;
      end; // physical page width is OK
    finally
      Printer.EndDoc;
    end; // try BeginDoc
  end; // if ShowModal
end;

procedure TfrmPrint.bPrinterSetupClick(Sender: TObject);
begin
  psd.Execute;
end;

procedure TfrmPrint.FormCreate(Sender: TObject);
begin
  ApplyLanguagePack;
end;

procedure TfrmPrint.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;
end;

procedure TfrmPrint.ApplyLanguagePack;
begin
  Caption := lpGet('MI_FILE_PRINT');
  bPrinterSetup.Caption := lpGet('B_PRINTER_SETUP')+'...';
  alCopies.Caption := lpGet('PRINT_COPIES')+':';
  alZoom.Caption := lpGet('PRINT_ZOOM')+' (%):';
  lCaption.Caption := lpGet('PRINT_CAPTION')+':';
  rgPages.Caption := lpGet('PRINT_PAGES');
  rgPages.Items.Text := lpGet('PRINT_PAGES_BUTTONS');
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

end.
