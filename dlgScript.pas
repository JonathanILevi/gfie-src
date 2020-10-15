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
unit dlgScript;

interface

uses
  LclIntf, LclType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  FileUtil, SynMemo, SynHighlighterPas, SynCompletion, Dialogs, ExtCtrls, Menus,
  StdCtrls, Accordion, uPSComponent, uPSComponent_Default, uPSComponent_Forms,
  uPSComponent_Controls, uPSComponent_StdCtrls, uPSCompiler, uPSRuntime,
  uPSComponent_COM, uPSComponent_DB, types, uPSI_AllMySources,
  uPSI_Dialogs, uPSI_ExtCtrls, uPSR_graphics, uPSC_graphics,
  uPSI_ComCtrls, Math, uPSI_PSImportGenerics;

type

  { TfrmScript }

  TfrmScript = class(TForm)
    aConsole: TAccordion;
    lConsole: TListBox;
    miStop: TMenuItem;
    miSaveScriptAs: TMenuItem;
    mmScript: TMainMenu;
    miOpenScript: TMenuItem;
    miSaveScript: TMenuItem;
    miRunGroup: TMenuItem;
    miRun: TMenuItem;
    MenuItem5: TMenuItem;
    miClose: TMenuItem;
    miFile: TMenuItem;
    odScript: TOpenDialog;
    sdScript: TSaveDialog;
    SynFreePascalSyn1: TSynFreePascalSyn;
    mScript: TSynMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miFileClick(Sender: TObject);
    procedure miOpenScriptClick(Sender: TObject);
    procedure miRunClick(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure miRunGroupClick(Sender: TObject);
    procedure miSaveScriptAsClick(Sender: TObject);
    procedure miSaveScriptClick(Sender: TObject);
    procedure miStopClick(Sender: TObject);
    procedure mScriptChange(Sender: TObject);
    procedure PSScript1AfterExecute(Sender: TPSScript);
    procedure PSScript1Compile(Sender: TPSScript);
    procedure PSScript1Execute(Sender: TPSScript);
  private
    FModified: boolean;
    PSScript1: TPSScript;
    FFileName: string;
    procedure SetFileName(AValue: string);
    procedure SetModified(AValue: boolean);
  public
    procedure PSWriteln(const s: string);
    procedure PSSleep(ms: integer);

    procedure ApplyLanguagePack;
    procedure UpdateCaption(scriptDefinitelyRunning: boolean = false);
    procedure OpenScript;
    procedure SaveScript;
    function DoSaveScript: boolean;
    function SavePromptOk: boolean;
    function IsRunning: boolean;

    property FileName: string read FFileName write SetFileName;
    property Modified: boolean read FModified write SetModified;
  end;

var
  frmScript: TfrmScript;

implementation

{$R *.lfm}

uses
  Main, LangPack, ieShared;

type

  { TPSImport_graphics }

  TPSImport_graphics = class(TPSPlugin)
  public
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;

{ TPSImport_graphics }

procedure TPSImport_graphics.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_Graphics(CompExec.Comp, true);
end;

procedure TPSImport_graphics.ExecImport1(CompExec: TPSScript;
  const ri: TPSRuntimeClassImporter);
begin
  RIRegister_Graphics(ri, true);
end;

{ TfrmScript }

procedure TfrmScript.miRunClick(Sender: TObject);
var
  i: integer;
  FoundError, CompileOk: boolean;
  s: string;

begin
  lConsole.Items.Clear;
  PSScript1.Script.Assign(mScript.Lines);

  lConsole.Items.Add(lpGet('SC_COMPILING_SCRIPT'));
  CompileOk := PSScript1.Compile;
  FoundError := false;
  for i := 0 to PSScript1.CompilerMessageCount - 1 do
  begin
    s := PSScript1.CompilerErrorToStr(i);
    if Pos('##', s) > 0 then Continue;
    lConsole.Items.Add(s);
    if not FoundError and (PSScript1.CompilerMessages[i] is TIFPSPascalCompilerError) then
    begin
      FoundError := True;
      mScript.SelStart := PSScript1.CompilerMessages[i].Pos;
    end;
  end;

  if CompileOk then
  begin
    lConsole.Items.Add(lpGet('SC_COMPILED'));
    lConsole.Items.Add(lpGet('SC_RUNNING_SCRIPT'));
    if PSScript1.Execute then
      lConsole.Items.Add(lpGet('SC_TERMINATED'))
    else
    begin
      mScript.SelStart := PSScript1.ExecErrorPosition;
      lConsole.Items.Add(PSScript1.ExecErrorToString +' @ '+Inttostr(PSScript1.ExecErrorProcNo)+'.'+Inttostr(PSScript1.ExecErrorByteCodePosition));
      PSScript1.Stop;
    end;
  end else
  begin
    lConsole.Items.Add(lpGet('SC_COMPILE_FAILED'));
  end;

  UpdateCaption;
end;

procedure TfrmScript.miCloseClick(Sender: TObject);
begin
  if IsRunning then
    PSScript1.Stop;
  Close;
end;

procedure TfrmScript.miRunGroupClick(Sender: TObject);
begin
  miRun.Enabled := not IsRunning;
  miStop.Enabled := IsRunning;
end;

procedure TfrmScript.miSaveScriptAsClick(Sender: TObject);
begin
  sdScript.FileName := SysToUTF8(FileName);
  if sdExecuteWithCanClose(sdScript) then
  begin
    FileName := UTF8ToSys(sdScript.FileName);
    SaveScript;
  end;
end;

procedure TfrmScript.miSaveScriptClick(Sender: TObject);
begin
  DoSaveScript;
end;

procedure TfrmScript.miStopClick(Sender: TObject);
begin
  PSScript1.Stop;
end;

procedure TfrmScript.FormCreate(Sender: TObject);
begin
  PSScript1 := TPSScript.Create(self);
  PSScript1.CompilerOptions := [icAllowNoBegin, icAllowNoEnd, icBooleanShortCircuit];
  PSScript1.OnCompile := PSScript1Compile;
  PSScript1.OnExecute := PSScript1Execute;
  PSScript1.OnAfterExecute := PSScript1AfterExecute;

  FFileName := '';
  FModified := false;

  TPSPluginItem(PSScript1.Plugins.Add).Plugin := TPSImport_Classes.Create(self);
  TPSPluginItem(PSScript1.Plugins.Add).Plugin := TPSImport_graphics.Create(self);
  TPSPluginItem(PSScript1.Plugins.Add).Plugin := TPSImport_DateUtils.Create(self);
  TPSPluginItem(PSScript1.Plugins.Add).Plugin := TPSImport_Controls.Create(self);
  TPSPluginItem(PSScript1.Plugins.Add).Plugin := TPSImport_DB.Create(self);
  TPSPluginItem(PSScript1.Plugins.Add).Plugin := TPSImport_Forms.Create(self);
  TPSPluginItem(PSScript1.Plugins.Add).Plugin := TPSImport_StdCtrls.Create(self);
  TPSPluginItem(PSScript1.Plugins.Add).Plugin := TPSImport_Dialogs.Create(self);
  TPSPluginItem(PSScript1.Plugins.Add).Plugin := TPSImport_ExtCtrls.Create(self);
  TPSPluginItem(PSScript1.Plugins.Add).Plugin := TPSImport_ComCtrls.Create(self);
  TPSPluginItem(PSScript1.Plugins.Add).Plugin := TPSImport_ComObj.Create(self);
  PSImportAllMySources(PSScript1);
  TPSPluginItem(PSScript1.Plugins.Add).Plugin := TPSImport_PSImportGenerics.Create(self);

  ApplyLanguagePack;
end;

procedure TfrmScript.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;

  if Pref_DisplayScriptWarning then
  begin
    ShowMessage(lpGet('SC_WARNING'));
    Pref_DisplayScriptWarning := False;
  end;
end;

procedure TfrmScript.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := SavePromptOk;
end;

procedure TfrmScript.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FileName := '';
  Modified := false;
end;

procedure TfrmScript.miFileClick(Sender: TObject);
begin
  miOpenScript.Enabled := not IsRunning;
end;

procedure TfrmScript.miOpenScriptClick(Sender: TObject);
begin
  odScript.FileName := SysToUTF8(FileName);
  if odScript.Execute and SavePromptOk then
  begin
    FileName := UTF8ToSys(odScript.FileName);
    OpenScript;
  end;
end;

procedure TfrmScript.mScriptChange(Sender: TObject);
begin
  Modified := True;
end;

procedure TfrmScript.PSScript1AfterExecute(Sender: TPSScript);
begin
  UpdateCaption;
end;

procedure TfrmScript.PSScript1Compile(Sender: TPSScript);
begin
  Sender.AddMethod(Self, @TfrmScript.PSWriteln, 'procedure Writeln(s: string);');
  Sender.AddMethod(Self, @TfrmScript.PSSleep, 'procedure Sleep(ms: integer);');

  Sender.AddRegisteredVariable('Application', 'TApplication');
  Sender.AddRegisteredVariable('frmMain', 'TfrmMain');
end;

procedure TfrmScript.PSScript1Execute(Sender: TPSScript);
begin
  Sender.SetVarToInstance('APPLICATION', Application);
  Sender.SetVarToInstance('FRMMAIN', frmMain);
  UpdateCaption(true);
end;

procedure TfrmScript.SetFileName(AValue: string);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
  UpdateCaption;
end;

procedure TfrmScript.SetModified(AValue: boolean);
begin
  if FModified=AValue then Exit;
  FModified:=AValue;
  UpdateCaption;
end;

procedure TfrmScript.PSWriteln(const s: string);
begin
  lConsole.Items.Add(s);
end;

procedure TfrmScript.PSSleep(ms: integer);
var
  i: integer;
begin
  for i := 1 to ms div 500 do
  begin
    if not IsRunning then
      Exit;
    Sleep(500);
    Application.ProcessMessages;
  end;
  if not IsRunning then
    Exit;
  Sleep(ms mod 500);
end;

procedure TfrmScript.ApplyLanguagePack;
var
  fltPas: string;
begin
  miFile.Caption:=lpGet('MI_FILE');
  miOpenScript.Caption:=lpGet('MI_FILE_OPEN_SCRIPT')+'...';
  miSaveScript.Caption:=lpGet('MI_FILE_SAVE_SCRIPT');
  miSaveScriptAs.Caption:=lpGet('MI_FILE_SAVE_SCRIPT_AS')+'...';
  miClose.Caption:=lpGet('MI_FILE_CLOSE');
  miRunGroup.Caption:=lpGet('MI_RUN');
  miRun.Caption:=lpGet('MI_RUN_RUN');
  miStop.Caption:=lpGet('MI_RUN_STOP');
  aConsole.Caption:=lpGet('SC_CONSOLE');
  aConsole.Invalidate;

  fltPas := Format('%s (*.pas)|*.pas', [lpGet('FF_PAS')]);
  odScript.Filter := fltPas;
  sdScript.Filter := fltPas;

  odScript.Title := lpGet('MI_FILE_OPEN_SCRIPT');
  sdScript.Title := lpGet('MI_FILE_SAVE_SCRIPT');

  UpdateCaption;
end;

procedure TfrmScript.UpdateCaption(scriptDefinitelyRunning: boolean);
var
  s: string;
begin
  s := lpGet('SC_TITLE');
  if FileName <> '' then
    s += ' - '+SysToUTF8(FileName);
  if Modified then
    s += ' *';
  if IsRunning or scriptDefinitelyRunning then
    s += ' ('+lpGet('SC_RUNNING')+')';
  Caption := s;
end;

procedure TfrmScript.OpenScript;
begin
  mScript.Lines.LoadFromFile(FileName);
  Modified := False;
end;

procedure TfrmScript.SaveScript;
begin
  mScript.Lines.SaveToFile(FileName);
  Modified := False;
end;

function TfrmScript.DoSaveScript: boolean;
begin
  if FileName <> '' then
    SaveScript
  else
    miSaveScriptAsClick(nil);
  Result := not Modified;
end;

function TfrmScript.SavePromptOk: boolean;
begin
  Result := True;
  if Modified then
  begin
    case QuerySaveChanges(FileName) of
      idYes: begin
        miSaveScriptClick(nil);
        Result := not Modified;
      end;
      idNo: Result := True;
      else Result := False;
    end;
  end;
end;

function TfrmScript.IsRunning: boolean;
begin
  Result := (PSScript1.Exec.Status = uPSRuntime.isRunning) or (PSScript1.Exec.Status = uPSRuntime.isPaused);
end;

end.

