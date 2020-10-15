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
unit dlgLib;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms, FileUtil,
  Dialogs, ExtCtrls, Buttons, Menus, ResList, LangPack,
  BitmapEx, bmExUtils, Filters, DocClass, ieShared, ShellEx, StrUtils,
  gfListBox, gfMath, Math, dlgExeFormat, dlgDebug, types, BMP, UndoObject,
  GenericList;

type
  TDlgResItemType = (dritIcon, dritCursor, dritBitmap);

  PDlgResItemId = ^TDlgResItemId;
  TDlgResItemId = record
    _Type: TDlgResItemType;
    Name: string;
    Language: word;
  end;

  TDlgResItem = class(TPersistent)
  private
    FDoc: TIconDoc;
  public
    Id: TDlgResItemId;
    Selected: boolean;

    property Doc: TIconDoc read FDoc;

    constructor Create(CreateDoc: boolean);
    destructor Destroy; override;
    procedure Assign(src: TPersistent); override;
  end;

{ TLibraryFrame }

  TLibraryFrame = class(TDocumentFrame)
    pToolbar: TPanel;
    sbAdd: TSpeedButton;
    sbRemove: TSpeedButton;
    sbReplace: TSpeedButton;
    sbProperties: TSpeedButton;
    sbExtractEdit: TSpeedButton;
    Bevel1: TBevel;
    sbExtractSave: TSpeedButton;
    sbSave: TSpeedButton;
    Bevel2: TBevel;
    lb: TgfListBox;
    procedure lbListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure sbPropertiesClick(Sender: TObject);
    procedure sbRemoveClick(Sender: TObject);
    procedure lbKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure sbAddClick(Sender: TObject);
    procedure sbReplaceClick(Sender: TObject);
    procedure sbExtractEditClick(Sender: TObject);
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure sbExtractSaveClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
    procedure lbResize(Sender: TObject);
    procedure lbGetCount(Sender: TObject; var Value: Integer);
    procedure lbItemGetSelected(Sender: TObject; Index: Integer;
      var Value: Boolean);
    procedure lbItemSetSelected(Sender: TObject; Index: Integer;
      Value: Boolean);
    procedure lbListDblClick(Sender: TObject);
    procedure lbItemPaint(Sender: TObject; ACanvas: TCanvas; Index: Integer;
      ItemRect: TRect; ListFocused: boolean);
  protected
    // Editable resources
    rlIcon: TGenericList<TDlgResItem>;
    // Other (hidden) resources
    rlOther: TResList;
  public

    // This is in fact much like a property
    FileType: TResFileType;

    procedure DocFrameTabAssigned; override;
    destructor Destroy; override;
    function DocFrameCanClose: boolean; override;
    procedure PerformUndoRedo(Undo: boolean); override;

    procedure ApplyLanguagePack;
    procedure UpdateCaption; override;

    procedure rlIconSort;
    function rlIconIndexOf(AType: TDlgResItemType;
      const AName: string; ALanguage: integer): integer;
    // select only one resource
    function SelectOneResource: integer;

    procedure DoClear;
    function DoLoad: boolean;
    // dsoSilent is an invalid option here
    function DoSave(const fn: string; ft: TResFileType;
      Options: TDocumentSaveOptions): boolean;

    procedure DoAdd;
    procedure DoRemove;
    procedure DoReplace;
    procedure DoProperties;
    procedure ChangeResourceProperties(const Id, newId: TDlgResItemId);
    procedure DoExtractEdit;
    procedure DoExtractSave;
  end;

function GetDlgResItemTypeLP(drit: TDlgResItemType): string;
function TDlgResItem_Compare(const x, y: TDlgResItem): integer;

implementation

uses Main, dlgDoc, dlgResProp, ICO, dlgStartupFrame, Locales;

{$R *.lfm}

const
  DlgResItemTypeExt: array[TDlgResItemType] of string = ('.ico', '.cur', '.bmp');
  DlgResItemTypeFT: array[TDlgResItemType] of TImageFileType = (iftIco, iftCur, iftBmp);
  DlgResItemTypeLP: array[TDlgResItemType] of string = ('RES_TYPE_ICON', 'RES_TYPE_CURSOR', 'RES_TYPE_BITMAP');

type
    { TLibAlterResourceUndo }

  // When the image contents of some resources are altered, some are added, and some are deleted.
  TLibAlterResourceUndo = class(TUndoObject)
  protected
    // For resources which did not exists, Doc is nil.
    FChangedResItems: TGenericList<TDlgResItem>;
  public
    constructor Create(const ACaption: string; ChangedOrRemovedResItems, AddedResItems: TGenericList<TDlgResItem>);
    destructor Destroy; override;

    procedure Perform(frm: TDocumentFrame); override;
    function Invert(frm: TDocumentFrame): TUndoObject; override;
  end;

  { TLibResourcePropertiesUndo }

  TLibResourcePropertiesUndo = class(TUndoObject)
  protected
    FOldId, FNewId: TDlgResItemId;
  public
    constructor Create(const ACaption: string; OldId, NewId: TDlgResItemId);

    procedure Perform(frm: TDocumentFrame); override;
    function Invert(frm: TDocumentFrame): TUndoObject; override;
  end;

// TDlgResItem

constructor TDlgResItem.Create;
begin
  inherited Create;

  if CreateDoc then
    FDoc := TIconDoc.Create;
  Selected := False;
end;

destructor TDlgResItem.Destroy;
begin
  FreeAndNil(FDoc);

  inherited;
end;

procedure TDlgResItem.Assign(src: TPersistent);
var
  x: TDlgResItem;
begin
  if src is TDlgResItem then
  begin
    x := TDlgResItem(src);

    if (x.Doc <> nil) and (FDoc = nil) then
    begin
      FDoc := TIconDoc.Create;
      FDoc.Assign(x.Doc);
    end else
    if (x.Doc = nil) and (FDoc <> nil) then
      FreeAndNil(FDoc)
    else
    if (x.Doc <> nil) and (FDoc <> nil) then
      FDoc.Assign(x.Doc);

    Id := x.Id;
    Selected := x.Selected;
  end
  else inherited;
end;

function TDlgResItem_Compare(const x, y: TDlgResItem): integer;
begin
  Result := integer(x.Id._Type) - integer(y.Id._Type);
  if Result = 0 then
  begin
    Result := CompareStr(x.Id.Name, y.Id.Name);
    if Result = 0 then
      Result := integer(x.Id.Language) - integer(y.Id.Language);
  end;
end;


{ TLibResourcePropertiesUndo }

constructor TLibResourcePropertiesUndo.Create(const ACaption: string; OldId,
  NewId: TDlgResItemId);
begin
  Caption := ACaption;
  FOldId := OldId;
  FNewId := NewId;
end;

procedure TLibResourcePropertiesUndo.Perform(frm: TDocumentFrame);
begin
  TLibraryFrame(frm).ChangeResourceProperties(FNewId, FOldId);
end;

function TLibResourcePropertiesUndo.Invert(frm: TDocumentFrame): TUndoObject;
begin
  Result:=TLibResourcePropertiesUndo.Create(Caption, FNewId, FOldId);
end;

{ TLibAlterResourceUndo }

constructor TLibAlterResourceUndo.Create(const ACaption: string;
  ChangedOrRemovedResItems, AddedResItems: TGenericList<TDlgResItem>);
var
  ri, riSave: TDlgResItem;
begin
  inherited Create;

  Caption := ACaption;
  FChangedResItems := TGenericList<TDlgResItem>.Create;
  if ChangedOrRemovedResItems <> nil then
  for ri in ChangedOrRemovedResItems do
  begin
    riSave := TDlgResItem.Create(true);
    riSave.Assign(ri);
    FChangedResItems.Add(riSave);
  end;
  if AddedResItems <> nil then
  for ri in AddedResItems do
  begin
    riSave := TDlgResItem.Create(false);
    riSave.Id := ri.Id;
    FChangedResItems.Add(riSave);
  end;
end;

destructor TLibAlterResourceUndo.Destroy;
var
  ri: TDlgResItem;
begin
  for ri in FChangedResItems do
    ri.Free;
  FreeAndNil(FChangedResItems);
  inherited;
end;

procedure TLibAlterResourceUndo.Perform(frm: TDocumentFrame);
var
  ri, riSave: TDlgResItem;
  resIndex: integer;
begin
  for riSave in FChangedResItems do
  begin
    resIndex := TLibraryFrame(frm).rlIconIndexOf(riSave.Id._Type, riSave.Id.Name, riSave.Id.Language);
    // modify?
    if resIndex >= 0 then
    begin
      ri := TLibraryFrame(frm).rlIcon[resIndex];
      if riSave.Doc <> nil then
      // undo modify
      begin
        ri.Assign(riSave);
      end else
      // undo add
      begin
        ri.Free;
        TLibraryFrame(frm).rlIcon.Delete(resIndex);
      end;
    end else
    begin
      if riSave.Doc <> nil then
      // undo delete
      begin
        ri := TDlgResItem.Create(true);
        ri.Assign(riSave);
        TLibraryFrame(frm).rlIcon.Add(ri);
      end else
      // deleted in both snapshots -> error
        assert(false);
    end;
  end;
end;

function TLibAlterResourceUndo.Invert(frm: TDocumentFrame): TUndoObject;
var
  lsChanged, lsAdded: TGenericList<TDlgResItem>;
  resIndex: integer;
  ri, riSave: TDlgResItem;
begin
  lsChanged := TGenericList<TDlgResItem>.Create;
  lsAdded := TGenericList<TDlgResItem>.Create;
  try
    for riSave in FChangedResItems do
    begin
      resIndex := TLibraryFrame(frm).rlIconIndexOf(riSave.Id._Type, riSave.Id.Name, riSave.Id.Language);
      if resIndex >= 0 then
      begin
        ri := TLibraryFrame(frm).rlIcon[resIndex];
        lsChanged.Add(ri);
      end else
      begin
        lsAdded.Add(riSave);
      end;
    end;
    Result := TLibAlterResourceUndo.Create(Caption, lsChanged, lsAdded);
  finally
    lsChanged.Free;
    lsAdded.Free;
  end;
end;

// TLibraryFrame

procedure TLibraryFrame.DocFrameTabAssigned;
begin
  rlIcon := TGenericList<TDlgResItem>.Create;
  rlOther := TResList.Create;

  // load glyphs
  GetMiscGlyph(sbSave.Glyph, mgSave);
  GetMiscGlyph(sbAdd.Glyph, mgPlus);
  GetMiscGlyph(sbRemove.Glyph, mgMinus);
  GetMiscGlyph(sbReplace.Glyph, mgReplace);
  GetMiscGlyph(sbProperties.Glyph, mgProperties);
  GetMiscGlyph(sbExtractEdit.Glyph, mgExtractEdit);
  GetMiscGlyph(sbExtractSave.Glyph, mgExtractSave);

  UpdateCaption;
  Resize;
  ApplyLanguagePack;

  frmMain.TabVisible[TStartupScreenTab] := False;
end;

destructor TLibraryFrame.Destroy;
var
  i: integer;
  
begin
  // avoid repainting
  lb.Visible := False;

  for i := 0 to rlIcon.Count - 1 do rlIcon[i].Free;
  rlIcon.Free;
  rlOther.Free;
  
  inherited;
end;

procedure TLibraryFrame.ApplyLanguagePack;
begin
  sbSave.Hint := lpGet('MI_FILE_SAVE');
  sbAdd.Hint := lpGet('MI_LIB_ADD')+'...';
  sbRemove.Hint := lpGet('MI_LIB_REMOVE');
  sbReplace.Hint := lpGet('MI_LIB_REPLACE')+'...';
  sbProperties.Hint := lpGet('MI_LIB_PROP')+'...';
  sbExtractEdit.Hint := lpGet('MI_LIB_EXTRACT_EDIT');
  sbExtractSave.Hint := lpGet('MI_LIB_EXTRACT_SAVE')+'...';
end;

procedure TLibraryFrame.UpdateCaption;
var
  s: string;

begin
  if FileName = '' then
    s := lpGet('UNTITLED')
  else
    s := SysToUTF8(ExtractFileName(FileName));
  if Modified then s := s + '*';

  TabCaption := s;
end;

procedure TLibraryFrame.rlIconSort;
begin
  rlIcon.Sort(TDlgResItem_Compare);
end;

function TLibraryFrame.rlIconIndexOf;
begin
  for Result := 0 to rlIcon.Count - 1 do
  with rlIcon[Result] do
    if (Id._Type = AType) and (Id.Name = AName) and (Id.Language = ALanguage) then
  Exit;

  Result := -1;
end;

function TLibraryFrame.SelectOneResource;
begin
  lb.ItemIndex := Max(0, lb.ItemIndex);
  Result := lb.ItemIndex;
end;

procedure TLibraryFrame.DoClear;
var
  i: integer;
  
begin
  for i := 0 to rlIcon.Count - 1 do rlIcon[i].Free;
  rlIcon.Clear;
  rlOther.Clear;
end;

function TLibraryFrame.DoLoad;
var
  i: integer;
  rl: TResList;
  e: TResEntry;
  dri: TDlgResItem;
  ms: TMemoryStream;
  bm: TBitmap32;

begin
  Screen.Cursor := crHourglass;
  try
    rl := TResList.Create;
    try
      FileType := rl.LoadFromFile(FileName);
      Result := FileType <> rftNone;
      if not Result then Exit;

      DoClear;

      // process resources
      for i := 0 to rl.EntryCount - 1 do
      begin
        e := rl[i];
        
        if (e._Type = resIconType[True, ritGroup]) or
          (e._Type = resIconType[False, ritGroup]) then
        // icon/cursor resource
        begin
          dri := TDlgResItem.Create(true);
          dri.Id.Name := e.Name;
          dri.Id.Language := e.Language;

          if e._Type = resIconType[True, ritGroup] then
            dri.Id._Type:=dritIcon else dri.Id._Type:=dritCursor;

          if rl.GetIconToDoc(dri.Id._Type = dritIcon, dri.Id.Name, dri.Id.Language, dri.Doc) then
          begin
            {$IFDEF GFIEDEBUG}
            //rl.GetIconToFile(dri.Id._Type = dritIcon, dri.Id.Name, dri.Id.Language,
            //  AppDir+'temp'+DirectorySeparator+dri.Id.Name+IfThen(dri.Id._Type = dritIcon, '.ico', '.cur'));
            {$ENDIF}
            rlIcon.Add(dri)
          end
          else
          begin
            {$IFDEF GFIEDEBUG}
            Log('Could not load resource!');
            LogDump(e.Data, Min(32, e.DataSize));
            {$ENDIF}
            dri.Free;
          end;
        end else

        if e._Type = IntToStr(PtrInt(RT_BITMAP)) then
        begin
          dri := TDlgResItem.Create(true);
          dri.Id.Name := e.Name;
          dri.Id.Language := e.Language;

          dri.Id._Type := dritBitmap;

          ms := TMemoryStream.Create;
          try
            ms.Write(e.Data^, e.DataSize);
            ms.Position := 0;
            bm := TBitmap32.Create;
            try
              if dibRead(bm, ms, 0, false) then
              begin
                dri.Doc.Assign(bm);
                rlIcon.Add(dri);
              end else
              begin
                {$IFDEF GFIEDEBUG}
                Log('Could not load resource bitmap '+e.Name+', size '+IntToStr(e.DataSize));
                LogDump(e.Data, Min(32, e.DataSize));
                {$ENDIF}
                rlOther.NewEntry.Assign(e); // load as "other resource"
                dri.Free;
              end;
            finally
              bm.Free;
            end;
          finally
            ms.Free;
          end;
        end else

        if (e._Type <> resIconType[True, ritPage]) and
          (e._Type <> resIconType[False, ritPage]) then
        // no icon/cursor resource
        rlOther.NewEntry.Assign(e);
      end; // for i

      rlIconSort;
      lb.InvalidateListBox;
      Modified := False;
    finally
      rl.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

function TLibraryFrame.DoSave;
const
  PE_INDEX = 0;
  NE_INDEX = 1;

var
  i: integer;
  rl: TResList;
  s: TMemoryStream;
  dri: TDlgResItem;
  SaveOK: boolean;
  ExeFormatSave: TExeFormat;
  dl: TieDataLosses;

begin
  Result := False;

  if ft = rftExe then
  begin
    ExeFormatSave := exeDetectFormat(fn);
    case ExeFormatSave of
      exeNone: begin
        // show a dialog for ICL format: 32-bit or 16-bit?
        DoCreateForm(TfrmExeFormat, frmExeFormat);
        frmExeFormat.rg.ItemIndex := IfThen(CanHandlePE, PE_INDEX, NE_INDEX);
        frmExeFormat.rg.Enabled := CanHandlePE;
        frmExeFormat.ShowModal;
        case frmExeFormat.rg.ItemIndex of
          PE_INDEX: if CanHandlePE then ExeFormatSave := exePE else
          begin
            ShowMessage('Error: PE format is not supported on this platform.');
            Exit(false);
          end;
          else ExeFormatSave := exeNE;
        end;
      end;

      // Warn: saving to NE format is destructive (overwrites file contents)
      exeNE: if MessageDlg(lpGet('MSG_CONFIRM_DESTROY_EXE'), mtConfirmation,
        [mbYes, mbNo], 0) <> mrYes then Exit;
    end; // case exe format
  end else
    ExeFormatSave := exeNone;

  try
    Screen.Cursor := crHourglass;
    try
      rl := TResList.Create;
      try
        // write icon/cursor resources
        s := TMemoryStream.Create;
        try
          for i := 0 to rlIcon.Count - 1 do
          begin
            dri := rlIcon[i];

            s.Clear;
            case dri.Id._Type of
              dritIcon, dritCursor:
                begin
                  icoSaveToStream(dri.Doc, s, dri.Id._Type=dritCursor, Pref_PNGLimit);
                  s.Position := 0;
                  rl.SetIconFromStream(dri.Id._Type=dritIcon, dri.Id.Name, dri.Id.Language, s);
                end;

              dritBitmap:
                begin
                  // save DIB only
                  if dri.Doc.SaveToStream(s, iftBmp, Pref_PNGLimit, '', 1, true, dl) then
                    rl.SetResourceFromMemory(IntToStr(PtrInt(RT_BITMAP)),
                      dri.Id.Name, dri.Id.Language,
                      s.Memory + SizeOf(BITMAPFILEHEADER),
                      s.Size - SizeOf(BITMAPFILEHEADER));
                end;
            end;
          end;
        finally
          s.Free;
        end;

        // write other resources
        rl.Append(rlOther);

        SaveOK := rl.SaveToFile(fn, ft, ExeFormatSave);
      finally
        rl.Free;
      end;
    finally
      Screen.Cursor := crDefault;
    end;

    if not SaveOK then
    begin
      ShowMessage(lpGet('MSG_ERROR_WRITE_RES')+LineEnding);
      Exit;
    end;

    if not (dsoSaveACopy in Options) then
    begin
      Modified := False;
      FileName := fn;
      FileType := ft;
    end;

    // success
    Result := True;
  except
  end;
end;

procedure TLibraryFrame.DoAdd;
var
  i: integer;
  fn: string;
  dri: TDlgResItem;
  ift: TImageFileType;
  changed: boolean;
  lsAdded: TGenericList<TDlgResItem>;

begin
  if frmMain.od.Execute then
  begin
    changed := false;
    lb.ItemIndex := -1;

    lsAdded := TGenericList<TDlgResItem>.Create;
    try
      for i := 0 to frmMain.od.Files.Count - 1 do
      begin
        fn := UTF8ToSys(frmMain.od.Files[i]);

        dri := TDlgResItem.Create(true);
        ift := dri.Doc.LoadFromFile(fn, Pref_MaxWidth, Pref_MaxHeight, 1, '');
        if ift = iftNone then
        begin
          if not FileExists(fn) then
            ShowMessage(Format(lpGet('MSG_NOT_EXIST'), [SysToUTF8(fn)])) else
            ShowMessage(Format(lpGet('MSG_UNKNOWN_FILE_TYPE'), [SysToUTF8(fn)]));

          FreeAndNil(dri);
        end else
        begin
          case ift of
            iftIco, iftIcns: dri.Id._Type := dritIcon;
            iftCur: dri.Id._Type := dritCursor;
            iftAni:
              begin
                dri.Id._Type := dritCursor;
                dri.Doc.DeleteAllPagesButFirst;
              end;
            else begin
              dri.Id._Type := dritBitmap;
              dri.Doc.Flatten;
              dri.Doc.DeleteAllPagesButFirst;
            end;
          end;
          dri.Id.Name := resValidName(ExtractOnlyFileName(fn));
          dri.Id.Language := 0;
          dri.Selected := True;

          // modify name if not unique
          while rlIconIndexOf(dri.Id._Type, dri.Id.Name, dri.Id.Language) >= 0 do
            dri.Id.Name := dri.Id.Name + '_';

          rlIcon.Add(dri);
          lsAdded.Add(dri);
          changed := true;
        end;
      end; // for i

      if changed then
      begin
        rlIconSort;
        if dri <> nil then
          lb.ScrollTo(rlIcon.IndexOf(dri));
        Modified := True;
        AddUndo(TLibAlterResourceUndo.Create('MI_LIB_ADD', Nil, lsAdded));
      end;
    finally
      lsAdded.Free;
    end;
    lb.InvalidateListBox;
  end;
end;

procedure TLibraryFrame.DoRemove;
var
  i: integer;
  lsRemoved: TGenericList<TDlgResItem>;

begin
  if lb.SelectedCount = 0 then ShowMessage(lpGet('MSG_SELECT_ITEMS')) else
  begin
    lsRemoved := TGenericList<TDlgResItem>.Create;
    try
      for i := lb.Count - 1 downto 0 do if lb.Selected[i] then
      begin
        lsRemoved.Add(rlIcon[i]);
        rlIcon.Delete(i);
      end;
      AddUndo(TLibAlterResourceUndo.Create('MI_LIB_REMOVE', lsRemoved, nil));
    finally
      for i := 0 to lsRemoved.Count - 1 do
        lsRemoved[i].Free;
      lsRemoved.Free;
    end;

    // changed
    lb.InvalidateListBox;
    Modified := True;
  end;
end;

procedure TLibraryFrame.DoReplace;
var
  i: integer;
  ooSave: TOpenOptions;
  ift: TImageFileType;
  fn, fnSys: string;
  lsChanged: TGenericList<TDlgResItem>;
  driSave: TDlgResItem;
  NewImageDoc: TIconDoc;

begin        
  if lb.SelectedCount = 0 then ShowMessage(lpGet('MSG_SELECT_ITEMS')) else
  begin
    ooSave := frmMain.od.Options;
    frmMain.od.Options := frmMain.od.Options - [ofAllowMultiSelect];
    if frmMain.od.Execute then
    begin
      fn := frmMain.od.FileName;
      fnSys := UTF8ToSys(fn);
      NewImageDoc := TIconDoc.Create;
      try
        ift := NewImageDoc.LoadFromFile(fnSys, Pref_MaxWidth, Pref_MaxHeight, 1, '');
        if ift = iftNone then
        begin
          if not FileExists(fnSys) then
            ShowMessage(Format(lpGet('MSG_NOT_EXIST'), [fn])) else
            ShowMessage(Format(lpGet('MSG_UNKNOWN_FILE_TYPE'), [fn]));
        end else
        begin
          lsChanged := TGenericList<TDlgResItem>.Create;
          try
            for i := 0 to lb.Count - 1 do
              if lb.Selected[i] then
            begin
              driSave := TDlgResItem.Create(true);
              driSave.Assign(rlIcon[i]);
              lsChanged.Add(driSave);
              rlIcon[i].Doc.Assign(NewImageDoc);
            end;
            AddUndo(TLibAlterResourceUndo.Create('MI_LIB_REPLACE', lsChanged, nil));
          finally
            for i := 0 to lsChanged.Count - 1 do
              lsChanged[i].Free;
            lsChanged.Free;
          end;
        end;
      finally
        NewImageDoc.Free;
      end;

      // changed
      lb.InvalidateListBox;
      Modified := True;
    end;
    frmMain.od.Options := ooSave;
  end; // selected
end;

procedure TLibraryFrame.DoProperties;
var
  ii: integer;
  dri: TDlgResItem;
  newId: TDlgResItemId;

begin
  DoCreateForm(TfrmResProp, frmResProp);
  ii := SelectOneResource;
  if ii < 0 then
  begin
    ShowMessage(lpGet('MSG_SELECT_ITEMS'));
    Exit;
  end;

  dri := rlIcon[ii];

  with frmResProp do
  begin
    cbType.ItemIndex := Integer(dri.Id._Type);
    cbType.Enabled := False; // not implemented
    eName.Text := dri.Id.Name;
    neLanguage.Value := dri.Id.Language;

    if ShowModal <> mrOk then Exit;

    // load settings from form
    newId._Type := TDlgResItemType(cbType.ItemIndex);
    newId.Name := resValidName(eName.Text);
    newId.Language := Round(neLanguage.Value);
  end;

  AddUndo(TLibResourcePropertiesUndo.Create('MI_LIB_PROP', dri.Id, newId));
  ChangeResourceProperties(dri.Id, newId);
end;

procedure TLibraryFrame.ChangeResourceProperties(const Id, newId: TDlgResItemId);
var
  oldIndex, newIndex: integer;
  dri: TDlgResItem;
begin
  oldIndex := rlIconIndexOf(Id._Type, Id.Name, Id.Language);
  newIndex := rlIconIndexOf(newId._Type, newId.Name, newId.Language);

  // nothing changed?
  if newIndex = oldIndex then Exit;
  // already exists?
  if newIndex >= 0 then
  begin
    ShowMessage(Format(lpGet('MSG_RES_EXIST'), [newId.Name, newId.Language]));
    Exit;
  end;

  // change resource
  dri := rlIcon[oldIndex];
  dri.Id := newId;

  // changed
  rlIconSort;
  lb.InvalidateListBox;
  Modified := True;
end;

procedure TLibraryFrame.DoExtractEdit;
var
  i: integer;

begin
  if lb.SelectedCount = 0 then ShowMessage(lpGet('MSG_SELECT_ITEMS')) else
  for i := 0 to lb.Count - 1 do if lb.Selected[i] then
  with TGraphicFrame(frmMain.NewDocument(TGraphicFrame)) do
  begin
    Doc.Assign(rlIcon[i].Doc);

    PageCountChanged;
    PageSizeChanged;
  end;
end;

procedure TLibraryFrame.DoExtractSave;
var
  i, fnIndex: integer;
  dir, fn, Ext: string;
  ft: TImageFileType;
  dl: TieDataLosses;

begin
  if lb.SelectedCount = 0 then ShowMessage(lpGet('MSG_SELECT_ITEMS')) else
  begin
    dir := BrowseForFolder(lpGet('HINT_RES_FOLDER')+':', '');
    if DirectoryExists(dir) then // extract all selected resources
    begin
      Screen.Cursor := crHourGlass;
      try
        dir := IncludeTrailingPathDelimiter(dir);

        for i := 0 to lb.Count - 1 do if lb.Selected[i] then
        begin
          Ext := DlgResItemTypeExt[rlIcon[i].Id._Type];
          ft := DlgResItemTypeFT[rlIcon[i].Id._Type];

          // gen. unique file name
          fnIndex := 1; fn := dir + rlIcon[i].Id.Name;
          while FileExists(fn + Ext) do
          begin
            inc(fnIndex);
            fn := dir + rlIcon[i].Id.Name + ' (' + IntToStr(fnIndex) + ')';
          end;

          rlIcon[i].Doc.SaveToFile(fn + Ext, ft, Pref_PNGLimit, '', 0, false, dl);
        end; // for i

        OpenDocument(SysToUTF8(dir));
      finally
        Screen.Cursor := crDefault;
      end;
    end; // if directory exists
  end; // SelCount <> 0
end;

procedure TLibraryFrame.sbPropertiesClick(Sender: TObject);
begin
  DoProperties;
end;

procedure TLibraryFrame.lbListMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  index: integer;
  status: string;
  dri: TDlgResItem;
begin
  index := lb.ItemAtPos(x, y);
  if index >= 0 then
  begin
    dri := rlIcon[index];
    status := dri.Id.Name + ' (' + lpGet(DlgResItemTypeLP[dri.Id._Type])
      + ', ' + lpGet('RP_LANGUAGE') + ': ' + LangToStr(dri.Id.Language) + ')';
  end
  else
    status := '';
  frmMain.SetStatus(status);
end;

procedure TLibraryFrame.sbRemoveClick(Sender: TObject);
begin
  DoRemove;
end;

procedure TLibraryFrame.lbKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then DoRemove;
end;

procedure TLibraryFrame.sbAddClick(Sender: TObject);
begin
  DoAdd;
end;

procedure TLibraryFrame.sbReplaceClick(Sender: TObject);
begin
  DoReplace;
end;

function TLibraryFrame.DocFrameCanClose: boolean;
begin
  Result := True;

  if Modified then
  case QuerySaveChanges(FileName) of
    IDYES: Result := DoSave(FileName, FileType, []);
    IDCANCEL: Result := False;
  end;
end;

procedure TLibraryFrame.PerformUndoRedo(Undo: boolean);
begin
  inherited;
  rlIconSort;
  lb.InvalidateListBox;
end;

procedure TLibraryFrame.sbExtractEditClick(Sender: TObject);
begin
  DoExtractEdit;
end;

procedure TLibraryFrame.ControlMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  frmMain.SetStatus((Sender as TControl).Hint);
end;

procedure TLibraryFrame.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  frmMain.SetStatus('');
end;

procedure TLibraryFrame.sbExtractSaveClick(Sender: TObject);
begin
  DoExtractSave;
end;

procedure TLibraryFrame.sbSaveClick(Sender: TObject);
begin
  frmMain.DoSave;
end;

procedure TLibraryFrame.lbResize(Sender: TObject);
begin
  lb.Columns := lb.ClientArea.Height div lb.ItemHeight;
end;

procedure TLibraryFrame.lbGetCount(Sender: TObject; var Value: Integer);
begin
  Value := rlIcon.Count;
end;

procedure TLibraryFrame.lbItemGetSelected(Sender: TObject; Index: Integer;
  var Value: Boolean);
begin
  Value := rlIcon[Index].Selected;
end;

procedure TLibraryFrame.lbItemSetSelected(Sender: TObject; Index: Integer;
  Value: Boolean);
begin
  rlIcon[Index].Selected := Value;
end;

procedure TLibraryFrame.lbListDblClick(Sender: TObject);
begin
  DoExtractEdit;
end;

procedure TLibraryFrame.lbItemPaint(Sender: TObject; ACanvas: TCanvas;
  Index: Integer; ItemRect: TRect; ListFocused: boolean);
const
  MaxLength = 14;

var
  yText, h: integer;
  s: string;
  dri: TDlgResItem;
  bmItem, bmThumb: TBitmap32;
  r: TRect;

begin
  dri := rlIcon[Index];
  bmItem := TBitmap32.Create;
  try
    bmItem.Resize(ItemRect.Width, ItemRect.Height);
    bmItem.FillColor(TColor32(lb.Color) or cl32Opaque);

    // background
    if lb.Selected[Index] then
    begin
      r := bmItem.ClientRect.Inflate(-8, -8);
      bmItem.Rectangle(r, IfThen(ListFocused, $30a03030, $10000000), True, 0);
      fltBoxBlur(bmItem, nil, 9, False, False, False);
      bmItem.Gradient(cl32Transparent, $20000000,
        Point(0, 0), Point(0, bmItem.Height),
        r, gkLinear, grNone);
    end;

    // draw text
    bmItem.Font.Name := 'Tahoma';
    bmItem.Font.Size := 8;
    h := bmItem.TextExtent('Mg').Y;
    yText := bmItem.Height - 8 - 2*h;
    // resource name
    s := dri.Id.Name;
    if Length(s) > MaxLength then s := Copy(s, 1, MaxLength) + '...';
    bmItem.TextOut((bmItem.Width - bmItem.TextExtent(s).X) div 2, yText, s, cl32Black, True);
    // resource type
    s := lpGet(DlgResItemTypeLP[dri.Id._Type]);
    bmItem.TextOut((bmItem.Width - bmItem.TextExtent(s).X) div 2, yText + h, s, cl32Gray, True);

    // draw thumbnail
    bmThumb := TBitmap32.Create;
    try
      dri.Doc.GetThumbnail(48, bmThumb, False);
      bmItem.Draw((bmItem.Width - bmThumb.Width) div 2, (yText - bmThumb.Height) div 2, bmThumb);
    finally
      bmThumb.Free;
    end;

    bmItem.DrawToCanvas(ACanvas, ItemRect.Left, ItemRect.Top, Pref_Hatch);
  finally
    bmItem.Free;
  end;
end;

function GetDlgResItemTypeLP(drit: TDlgResItemType): string;
begin
  Result := DlgResItemTypeLP[drit];
end;

end.

