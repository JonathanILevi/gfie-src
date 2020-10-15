unit Utils3d;

{$mode delphi}

interface

uses
  Classes, SysUtils, gl, bmExUtils, BitmapEx, GenericList;

type

  { T3dFace }

  T3dFace = record
    Vertices: TInteger3;
    TexCoords: TInteger3;
    Normals: TInteger3;

    procedure ParseFromObj(const s1, s2, s3: string);
  end;

  { T3dObject }

  T3dObject = class
  private
    Vertices: TGenericList<TDouble3>;
    Colors: TGenericList<TDouble4>;
    Normals: TGenericList<TDouble3>;
    TexCoords: TGenericList<TDouble3>;
    Faces: TGenericList<T3dFace>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function LoadFromObjFile(const fn: string): boolean;
    function HasTextureCoords: boolean;
    procedure glLoad;
    procedure glUnload;
    procedure glDrawFace(const Face: T3dFace);
    procedure glDraw;
  end;

procedure glTextureFromBitmap32(texture: GLuint; bm: TBitmap32);
procedure glCheck;

implementation

uses
  Dialogs, glu, StrUtils;

procedure glTextureFromBitmap32(texture: GLuint; bm: TBitmap32);
begin
  glBindTexture(GL_TEXTURE_2D, texture);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, bm.Width, bm.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, bm.Data);
end;

procedure glCheck;
var
  x: GLenum;
begin
  x := glGetError;
  if x <> GL_NO_ERROR then
    ShowMessage('OpenGL error: '+IntToStr(x)+' '+gluErrorString(x));
end;

{ T3dFace }

procedure T3dFace.ParseFromObj(const s1, s2, s3: string);

  procedure DoParse(index: integer; const s: string);
  var
    p, p2, v: integer;
  begin
    p := Pos('/', s);
    if p <= 0 then // only one index
    begin
      v := StrToInt(s);
      Vertices[index] := v;
      TexCoords[index] := v;
      Normals[index] := v;
    end else
    begin
      p2 := PosEx('/', s, p+1);
      if p2 <= 0 then Exit;
      Vertices[index] := StrToInt(Copy(s, 1, p-1));
      if p2 = p+1 then TexCoords[index] := v
      else TexCoords[index] := StrToInt(Copy(s, p+1, p2-p-1));
      Normals[index] := StrToInt(Copy(s, p2+1, Length(s)-p2));
    end;
  end;

begin
  DoParse(0, s1);
  DoParse(1, s2);
  DoParse(2, s3);
end;

{ T3dObject }

constructor T3dObject.Create;
begin
  Vertices := TGenericList<TDouble3>.Create;
  Colors := TGenericList<TDouble4>.Create;
  Normals := TGenericList<TDouble3>.Create;
  TexCoords := TGenericList<TDouble3>.Create;
  Faces := TGenericList<T3dFace>.Create;
  Clear;
end;

destructor T3dObject.Destroy;
begin
  glUnload;
  Vertices.Free;
  Colors.Free;
  Normals.Free;
  TexCoords.Free;
  Faces.Free;
  inherited Destroy;
end;

procedure T3dObject.Clear;
begin
  glUnload;
  Vertices.Clear;
  Colors.Clear;
  Normals.Clear;
  TexCoords.Clear;
  Faces.Clear;
end;

function T3dObject.LoadFromObjFile(const fn: string): boolean;
var
  i: integer;
  fval: double;
  f: textfile;
  s, cmd: string;
  tokens: TStringList;

begin
  Clear;
  AssignFile(f, fn);
  Reset(f);
  try
    Tokens := TStringList.Create;
    try
      while not Eof(f) do
      begin
        Readln(f, s);
        s := Trim(s);

        if s = '' then
          Continue;
        if s[1] = '#' then
          Continue; // comment

        Tokens.Clear;
        ExtractStrings([' ', #9], [], PChar(s), Tokens);
        if Tokens.Count >= 2 then
        begin
          cmd := AnsiLowerCase(Tokens[0]);
          if (cmd = 'v') and (Tokens.Count >= 4) then
          begin
            Vertices.Count := Vertices.Count+1;
            for i := 0 to 2 do
              Vertices.GetItemPtr(Vertices.Count-1)^[i] := StrToFloat(Tokens[1+i]);
          end else
          if (cmd = 'vn') and (Tokens.Count >= 4) then
          begin
            Normals.Count := Normals.Count+1;
            for i := 0 to 2 do
              Normals.GetItemPtr(Normals.Count-1)^[i] := StrToFloat(Tokens[1+i]);
          end else
          if (cmd = 'vt') and (Tokens.Count >= 3) then
          begin
            TexCoords.Count := TexCoords.Count+1;
            for i := 0 to 2 do
            begin
              fval := 0;
              if 1+i < Tokens.Count then
                fval := StrToFloat(Tokens[1+i]);
              TexCoords.GetItemPtr(TexCoords.Count-1)^[i] := fval;
            end;
          end else
          if (cmd = 'f') and (Tokens.Count >= 4) then
          begin
            Faces.Count := Faces.Count+1;
            Faces.GetItemPtr(Faces.Count-1).ParseFromObj(Tokens[1], Tokens[2], Tokens[3]);
          end;
        end;
      end;
    finally
      Tokens.Free;
    end;
  finally
    CloseFile(f);
  end;
  Result := True;
end;

function T3dObject.HasTextureCoords: boolean;
begin
  Result := TexCoords.Count > 0;
end;

procedure T3dObject.glLoad;
begin
  // nothing here
end;

procedure T3dObject.glUnload;
begin
  // nothing here
end;

procedure T3dObject.glDrawFace(const Face: T3dFace);
var
  j, v, vn, vt: integer;

begin
  for j := 0 to 2 do
  begin
    v := Face.Vertices[j] - 1;
    vn := Face.Normals[j] - 1;
    vt := Face.TexCoords[j] - 1;

    if (v >= 0) and (v < Colors.Count) then
      glColor4dv(PGLDouble(Colors[v]))
    else
      glColor4f(1, 1, 1, 1);

    if (vn >= 0) and (vn < Normals.Count) then
      glNormal3dv(PGLDouble(Normals[vn]))
    else
      glNormal3f(1, 0, 0);

    if (vt >= 0) and (vt < TexCoords.Count) then
      glTexCoord3dv(PGLDouble(TexCoords[vt]))
    else
      glTexCoord3f(0, 0, 0);

    if (v >= 0) and (v < Vertices.Count) then
      glVertex3dv(PGLDouble(Vertices[v]))
    else
      glVertex3f(0, 0, 0);
  end;
end;

procedure T3dObject.glDraw;
var
  i: integer;

begin
  glBegin(GL_TRIANGLES);
  for i := 0 to Faces.Count - 1 do
    glDrawFace(Faces[i]);
  glEnd;
end;

end.

