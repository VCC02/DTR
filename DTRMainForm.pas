{
    Copyright (C) 2025 VCC
    creation date: 26 Oct 2025
    initial release date: 29 Oct 2025

    author: VCC
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"),
    to deal in the Software without restriction, including without limitation
    the rights to use, copy, modify, merge, publish, distribute, sublicense,
    and/or sell copies of the Software, and to permit persons to whom the
    Software is furnished to do so, subject to the following conditions:
    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
    DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
    OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}


unit DTRMainForm;

{$mode objfpc}{$H+}

interface

uses
  LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, Menus, frDTRFrame;

type

  { TfrmDTRMain }

  TfrmDTRMain = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    frDTR: TfrDTR;

    procedure LoadSettingsFromIni;
    procedure SaveSettingsToIni;

  public

  end;

var
  frmDTRMain: TfrmDTRMain;

implementation

{$R *.frm}

uses
  IniFiles, Clipbrd;

{ TfrmDTRMain }


procedure TfrmDTRMain.LoadSettingsFromIni;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'DTR.ini');
  try
    Left := Ini.ReadInteger('Window', 'Left', Left);
    Top := Ini.ReadInteger('Window', 'Top', Top);
    Width := Ini.ReadInteger('Window', 'Width', Width);
    Height := Ini.ReadInteger('Window', 'Height', Height);

    frDTR.LoadSettingsFromIni(Ini, '');
  finally
    Ini.Free;
  end;
end;


procedure TfrmDTRMain.SaveSettingsToIni;
var
  Ini: TMemIniFile;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'DTR.ini');
  try
    Ini.WriteInteger('Window', 'Left', Left);
    Ini.WriteInteger('Window', 'Top', Top);
    Ini.WriteInteger('Window', 'Width', Width);
    Ini.WriteInteger('Window', 'Height', Height);

    frDTR.SaveSettingsToIni(Ini, '');

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TfrmDTRMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  try
    SaveSettingsToIni;
  except
  end;

  frDTR.Free;
end;


procedure TfrmDTRMain.FormCreate(Sender: TObject);
begin
  frDTR := TfrDTR.Create(Self);
  frDTR.Parent := Self;
  frDTR.Left := 0;
  frDTR.Top := 0;
  frDTR.Width := Width;
  frDTR.Height := Height;
  frDTR.Anchors := [akLeft, akTop, akRight, akBottom];

  try
    LoadSettingsFromIni;
  except
  end;
end;


end.

