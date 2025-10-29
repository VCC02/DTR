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


unit AllDTRsMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ECTabCtrl, ECTypes,
  ComCtrls, StdCtrls, ExtCtrls, frTabsFrame;

type
  { TfrmAllDTRsMain }

  TfrmAllDTRsMain = class(TForm)
    edtSearchL1: TEdit;
    edtSearchL2: TEdit;
    tmrStartup: TTimer;
    tmrSearch: TTimer;
    procedure edtSearchL1Change(Sender: TObject);
    procedure edtSearchL2Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure tmrSearchTimer(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
  private
    frTabs: TfrTabs;
    FActiveTabIndexOnEmptySearch: Integer;

    procedure LoadSettingsFromIni;
    procedure SaveSettingsToIni;

    procedure HandleOnAddTab(out ATabContent: Pointer);
    procedure HandleOnDeleteTab(ATabContent: Pointer);
    procedure HandleOnChangeTab(AOldTabContent, ANewTabContent: Pointer);

    //Content handlers
    procedure HandleOnSetProjectName(Sender: TObject; AName: string);
  public

  end;

var
  frmAllDTRsMain: TfrmAllDTRsMain;

implementation

{$R *.frm}


uses
  frDTRFrame, IniFiles;

{ TfrmAllDTRsMain }

procedure TfrmAllDTRsMain.FormCreate(Sender: TObject);
begin
  frTabs := TfrTabs.Create(frmAllDTRsMain);
  frTabs.Parent := frmAllDTRsMain;
  frTabs.Left := 0;
  frTabs.Top := 0;
  frTabs.Width := edtSearchL1.Left - 8;
  frTabs.Height := 26;
  frTabs.Anchors := [akLeft, akTop, akRight];

  frTabs.OnAddTab := @HandleOnAddTab;
  frTabs.OnDeleteTab := @HandleOnDeleteTab;
  frTabs.OnChangeTab := @HandleOnChangeTab;

  FActiveTabIndexOnEmptySearch := -1;
  tmrStartup.Enabled := True;
end;


procedure TfrmAllDTRsMain.LoadSettingsFromIni;
var
  Ini: TMemIniFile;
  TabCount, i: Integer;
  ProjectName: string;
  Content: TfrDTR;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'AllDTRs.ini');
  try
    Left := Ini.ReadInteger('Window', 'Left', Left);
    Top := Ini.ReadInteger('Window', 'Top', Top);
    Width := Ini.ReadInteger('Window', 'Width', Width);
    Height := Ini.ReadInteger('Window', 'Height', Height);

    TabCount := Ini.ReadInteger('Settings', 'TabCount', 0);

    for i := 0 to TabCount - 1 do
    begin
      Content := TfrDTR(frTabs.AddTabToEnd);
      ProjectName := Ini.ReadString('Settings', 'ProjectName_' + IntToStr(i), '');

      if ProjectName <> '' then
        Content.LoadDTRProject(ProjectName);

      Content.LoadSettingsFromIni(Ini, '_' + IntToStr(i));
    end;

    frTabs.ActiveTabIndex := Ini.ReadInteger('Settings', 'ActiveTabIndex', 0);
  finally
    Ini.Free;
  end;
end;


procedure TfrmAllDTRsMain.SaveSettingsToIni;
var
  Ini: TMemIniFile;
  i: Integer;
  Content: TfrDTR;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'AllDTRs.ini');
  try
    Ini.WriteInteger('Window', 'Left', Left);
    Ini.WriteInteger('Window', 'Top', Top);
    Ini.WriteInteger('Window', 'Width', Width);
    Ini.WriteInteger('Window', 'Height', Height);

    Ini.WriteInteger('Settings', 'TabCount', frTabs.TabCount);
    for i := 0 to frTabs.TabCount - 1 do
    begin
      Content := TfrDTR(frTabs.Content[i]);
      Content.SaveSettingsToIni(Ini, '_' + IntToStr(i));
      Ini.WriteString('Settings', 'ProjectName_' + IntToStr(i), Content.ProjectName);
    end;

    Ini.WriteInteger('Settings', 'ActiveTabIndex', frTabs.ActiveTabIndex);

    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;


procedure TfrmAllDTRsMain.tmrStartupTimer(Sender: TObject);
begin
  tmrStartup.Enabled := False;
  LoadSettingsFromIni;
end;


procedure TfrmAllDTRsMain.tmrSearchTimer(Sender: TObject);
var
  i: Integer;
  FirstVisibleIndex, OldActiveIndex: Integer;
  Visibility: Boolean;
begin
  tmrSearch.Enabled := False;

  FirstVisibleIndex := -1;
  OldActiveIndex := frTabs.ActiveTabIndex;

  for i := 0 to frTabs.TabCount - 1 do
  begin
    Visibility := TfrDTR(frTabs.Content[i]).vstDual.VisibleCount > 0;
    frTabs.SetTabVisibilityByIndex(i, Visibility);

    if Visibility and (FirstVisibleIndex = -1) then
      FirstVisibleIndex := i;
  end;

  if (FirstVisibleIndex > -1) and (FirstVisibleIndex <> OldActiveIndex) then
  begin
    if TfrDTR(frTabs.Content[OldActiveIndex]).vstDual.VisibleCount = 0 then //set only if the current tab is not visible
      frTabs.ActiveTabIndex := FirstVisibleIndex;
  end;

  if (edtSearchL1.Text = '') and (edtSearchL2.Text = '') then
    if FActiveTabIndexOnEmptySearch <> -1 then
      frTabs.ActiveTabIndex := FActiveTabIndexOnEmptySearch;
end;


procedure TfrmAllDTRsMain.edtSearchL1Change(Sender: TObject);
var
  i: Integer;
begin
  if edtSearchL1.Text > '' then
    edtSearchL1.Color := clYellow
  else
    edtSearchL1.Color := clWindow;

  for i := 0 to frTabs.TabCount - 1 do
    TfrDTR(frTabs.Content[i]).SetSearchL1(edtSearchL1.Text);

  tmrSearch.Enabled := True;
end;


procedure TfrmAllDTRsMain.edtSearchL2Change(Sender: TObject);
var
  i: Integer;
begin
  if edtSearchL2.Text > '' then
    edtSearchL2.Color := clYellow
  else
    edtSearchL2.Color := clWindow;

  for i := 0 to frTabs.TabCount - 1 do
    TfrDTR(frTabs.Content[i]).SetSearchL2(edtSearchL2.Text);

  tmrSearch.Enabled := True;
end;


procedure TfrmAllDTRsMain.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveSettingsToIni;
end;


procedure TfrmAllDTRsMain.HandleOnAddTab(out ATabContent: Pointer);
var
  frDTR: TfrDTR;
begin
  frDTR := TfrDTR.Create(Self);
  frDTR.Name := 'frDTR_' + IntToStr(PtrUInt(frDTR));
  frDTR.Parent := Self;
  frDTR.Left := 0;
  frDTR.Top := frTabs.Height;
  frDTR.Width := Width;
  frDTR.Height := Height - (frTabs.Top + frTabs.Height);
  frDTR.Anchors := [akLeft, akTop, akRight, akBottom];
  frDTR.OnSetProjectName := @HandleOnSetProjectName;

  ATabContent := frDTR; //pointer to the DTR frame
end;


procedure TfrmAllDTRsMain.HandleOnDeleteTab(ATabContent: Pointer);
var
  frDTR: TfrDTR;
begin
  frDTR := TfrDTR(ATabContent);
  frDTR.Free;
end;


procedure TfrmAllDTRsMain.HandleOnChangeTab(AOldTabContent, ANewTabContent: Pointer);
var
  {frDTROld,} frDTRNew: TfrDTR;
begin
  //frDTROld := TfrDTR(AOldTabContent);
  frDTRNew := TfrDTR(ANewTabContent);
  //
  //frDTROld.Hide;
  frDTRNew.BringToFront;

  if (edtSearchL1.Text = '') and (edtSearchL2.Text = '') then
    FActiveTabIndexOnEmptySearch := frTabs.ActiveTabIndex;
end;


procedure TfrmAllDTRsMain.HandleOnSetProjectName(Sender: TObject; AName: string);
begin
  frTabs.SetTabCaption(Sender, '   ' + ExtractFileName(AName) + '   ');
end;

end.

