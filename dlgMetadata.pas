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
unit dlgMetadata;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, dlgDoc, NumberEdit, AdjustControl;

type

  { TfrmMetadata }

  TfrmMetadata = class(TForm)
    alDPI: TAdjustLabel;
    gbSummary: TGroupBox;
    lTitle: TLabel;
    lAuthor: TLabel;
    lCopyright: TLabel;
    lComments: TLabel;
    eTitle: TEdit;
    eAuthor: TEdit;
    eCopyright: TEdit;
    mComments: TMemo;
    bOK: TButton;
    bCancel: TButton;
    gbGIF: TGroupBox;
    alLoopCount: TAdjustLabel;
    neLoopCount: TNumberEdit;
    neDPI: TNumberEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure gbGIFClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ApplyLanguagePack;
    procedure Execute(frmDoc: TGraphicFrame);
  end;

var
  frmMetadata: TfrmMetadata;

implementation

{$R *.lfm}

uses LangPack;

procedure TfrmMetadata.gbGIFClick(Sender: TObject);
begin

end;

procedure TfrmMetadata.FormCreate(Sender: TObject);
begin
  ApplyLanguagePack;
end;

procedure TfrmMetadata.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;
end;

procedure TfrmMetadata.ApplyLanguagePack;
begin
  Caption := lpGet('MI_FILE_METADATA');

  gbSummary.Caption := Format(lpGet('MD_TYPE'), ['GFIE/ANI/TIFF']);
  lTitle.Caption := lpGet('MD_TITLE')+':';
  lAuthor.Caption := lpGet('MD_AUTHOR')+':';
  lCopyright.Caption := lpGet('MD_COPYRIGHT')+':';
  lComments.Caption := lpGet('MD_COMMENTS')+':';

  gbGIF.Caption := Format(lpGet('MD_TYPE'), ['GIF']);
  alLoopCount.Caption := lpGet('MD_LOOP_COUNT')+':';
  alDPI.Caption := lpGet('MD_DPI')+':';

  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

procedure TfrmMetadata.Execute(frmDoc: TGraphicFrame);
begin
  with frmDoc.Doc.Metadata do
  begin
    eTitle.Text := Title;
    eAuthor.Text := Author;
    eCopyright.Text := Copyright;
    mComments.Text := Comments;
    neLoopCount.Value := LoopCount;
    neDPI.Value := DPI;

    if ShowModal = mrOk then
    begin
      Title := eTitle.Text;
      Author := eAuthor.Text;
      Copyright := eCopyright.Text;
      Comments := mComments.Text;
      LoopCount := Round(neLoopCount.Value);
      DPI := neDPI.Value;

      frmDoc.Modified := True;
    end;
  end;
end;

end.
