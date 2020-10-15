program CMDimp;

{$MODE Delphi}

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  ParserU in 'ParserU.pas',
  ParserUtils in 'ParserUtils.pas';

var
  s: TStringList;
  x: TUnitParser;

begin
  if ParamCount = 0 then
  begin
    Writeln('Command Line Unit Importing');
    writeln;
    writeln('cmdimp <filename>');
    halt(1);
  end;
  Writeln(ParamStr(1));
  s := TStringList.Create;
  try
    s.LoadFromFile(Paramstr(1));
    x := TUnitParser.Create('');
    x.Unitname := ExtractFileName(paramstr(1));
    x.SingleUnit := true;
    x.ParseUnit(s.Text);
    x.SaveToPath(ExtractFilePath(Paramstr(1)));
    x.AutoRenameOverloadedMethods := true;
    Writeln('Successfully processed. Saved to file: '+ExtractFilePath(Paramstr(1))+x.UnitNameCmp);

  finally
    s.Free;
  end;
end.
