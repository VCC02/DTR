{
    Copyright (C) 2026 VCC
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
  ComCtrls, StdCtrls, ExtCtrls, Buttons, Menus, frTabsFrame, IniFiles;

type
  { TfrmAllDTRsMain }

  TfrmAllDTRsMain = class(TForm)
    edtSearchL1: TEdit;
    edtSearchL2: TEdit;
    MenuItem_AddSearchBoxValuesAsKeyReplacement: TMenuItem;
    MenuItem_KeyReplacements: TMenuItem;
    Separator2: TMenuItem;
    MenuItem_SelectProjectGroup: TMenuItem;
    Separator1: TMenuItem;
    MenuItem_RemoveProjectGroup: TMenuItem;
    MenuItem_AddProjectGroup: TMenuItem;
    pmProjectGroups: TPopupMenu;
    spdbtnProjectGroups: TSpeedButton;
    tmrStartup: TTimer;
    tmrSearch: TTimer;
    procedure edtSearchL1Change(Sender: TObject);
    procedure edtSearchL2Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem_AddProjectGroupClick(Sender: TObject);
    procedure MenuItem_AddSearchBoxValuesAsKeyReplacementClick(Sender: TObject);
    procedure spdbtnProjectGroupsClick(Sender: TObject);
    procedure tmrSearchTimer(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
  private
    frTabs: TfrTabs;
    FActiveTabIndexOnEmptySearch: Integer;
    FActiveGroupIndex: Integer;
    FProjectGroupsCount: Integer;

    function GetGroupPrefixFromIndex: string;
    procedure LoadActiveProjectGroup(Ini: TMemIniFile; GroupPrefix: string);
    procedure LoadSettingsFromIni;
    procedure SaveActiveProjectGroup(Ini: TMemIniFile; GroupPrefix: string);
    procedure SaveSettingsToIni;

    procedure CreateOneProjectGroupItem;
    procedure CreateAllProjectGroupItems;
    procedure UpdateProjectGroupMenuItemTags;
    procedure CloseProjectGroup;

    procedure HandleOnRemoveProjectGroup(Sender: TObject);
    procedure HandleOnSelectProjectGroup(Sender: TObject);

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
  frDTRFrame;

{ TfrmAllDTRsMain }

procedure TfrmAllDTRsMain.FormCreate(Sender: TObject);
begin
  frTabs := TfrTabs.Create(frmAllDTRsMain);
  frTabs.Parent := frmAllDTRsMain;
  frTabs.Left := 0;
  frTabs.Top := 0;
  frTabs.Width := spdbtnProjectGroups.Left - 8;
  frTabs.Height := 26;
  frTabs.Anchors := [akLeft, akTop, akRight];

  frTabs.OnAddTab := @HandleOnAddTab;
  frTabs.OnDeleteTab := @HandleOnDeleteTab;
  frTabs.OnChangeTab := @HandleOnChangeTab;

  FActiveTabIndexOnEmptySearch := -1;
  FActiveGroupIndex := -1;
  FProjectGroupsCount := 0;

  tmrStartup.Enabled := True;
end;


procedure TfrmAllDTRsMain.HandleOnRemoveProjectGroup(Sender: TObject);
var
  Idx, i: Integer;
  ProjectNames, GroupPrefix, TempProjectName: string;
  Ini: TMemIniFile;
begin
  Idx := (Sender as TMenuItem).Tag;

  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'AllDTRs.ini');
  try
    ProjectNames := '';
    for i := 0 to 3 do
      if i < frTabs.TabCount then
      begin
        GroupPrefix := 'Grp_' + IntToStr(Idx) + '.';
        TempProjectName := Ini.ReadString('Settings', GroupPrefix + 'ProjectName_' + IntToStr(i), '');
        ProjectNames := ProjectNames + ExtractFileName(TempProjectName) + #13#10;
      end;
  finally
    Ini.Free;
  end;

  ProjectNames := ProjectNames + '...';

  if MessageDlg('Are you sure you want to remove this project group?' + #13#10 + ProjectNames, mtConfirmation, [mbYes, mbNo], 0, mbYes) = mrNo then
    Exit;

  MenuItem_RemoveProjectGroup.Delete(Idx);
  MenuItem_SelectProjectGroup.Delete(Idx);
  FActiveGroupIndex := 0;
  Dec(FProjectGroupsCount);

  UpdateProjectGroupMenuItemTags;
end;


procedure TfrmAllDTRsMain.HandleOnSelectProjectGroup(Sender: TObject);
var
  Ini: TMemIniFile;
  GroupPrefix: string;
  NewActiveGroupIndex: Integer;
begin
  NewActiveGroupIndex := (Sender as TMenuItem).Tag;
  if NewActiveGroupIndex = FActiveGroupIndex then
    Exit;

  SaveSettingsToIni; //save group settings
  CloseProjectGroup;

  FActiveGroupIndex := NewActiveGroupIndex;
  GroupPrefix := GetGroupPrefixFromIndex;

  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'AllDTRs.ini');
  try
    MenuItem_SelectProjectGroup.Items[FActiveGroupIndex].Checked := True;
    LoadActiveProjectGroup(Ini, GroupPrefix);
  finally
    Ini.Free;
  end;
end;


procedure TfrmAllDTRsMain.CreateOneProjectGroupItem;
var
  TempMenuItem: TMenuItem;
begin
  //TempMenuItem := TMenuItem.Create(nil);
  try
    TempMenuItem := TMenuItem.Create(nil);
    TempMenuItem.OnClick := @HandleOnRemoveProjectGroup;
    MenuItem_RemoveProjectGroup.Add(TempMenuItem);

    TempMenuItem := TMenuItem.Create(nil);
    TempMenuItem.GroupIndex := 0;
    TempMenuItem.AutoCheck := True;
    TempMenuItem.RadioItem := True;
    TempMenuItem.Checked := False; //do not select yet
    TempMenuItem.OnClick := @HandleOnSelectProjectGroup;
    MenuItem_SelectProjectGroup.Add(TempMenuItem);
  finally
    //TempMenuItem.Free;
  end;
end;


procedure TfrmAllDTRsMain.CreateAllProjectGroupItems;
var
  i: Integer;
begin
  for i := 0 to FProjectGroupsCount - 1 do
    CreateOneProjectGroupItem;

  UpdateProjectGroupMenuItemTags;
end;


procedure TfrmAllDTRsMain.UpdateProjectGroupMenuItemTags;
var
  i: Integer;
begin
  for i := 0 to FProjectGroupsCount - 1 do
  begin
    MenuItem_RemoveProjectGroup.Items[i].Tag := i;
    MenuItem_SelectProjectGroup.Items[i].Tag := i;

    MenuItem_RemoveProjectGroup.Items[i].Caption := 'Project group ' + IntToStr(i);
    MenuItem_SelectProjectGroup.Items[i].Caption := 'Project group ' + IntToStr(i);
  end;
end;


procedure TfrmAllDTRsMain.MenuItem_AddProjectGroupClick(Sender: TObject);
begin
  Inc(FProjectGroupsCount);
  CreateOneProjectGroupItem;
  UpdateProjectGroupMenuItemTags;

  if FProjectGroupsCount = 1 then
    if MessageDlg('Do you want the currently loaded project group to be automatically set as the first project group of the list?', mtConfirmation, [mbYes, mbNo], 0, mbYes) = mrYes then
    begin
      FActiveGroupIndex := FProjectGroupsCount - 1;
      MenuItem_SelectProjectGroup.Items[FActiveGroupIndex].Checked := True; //select the new group when adding
      SaveSettingsToIni; //save group settings
    end;
end;


procedure TfrmAllDTRsMain.MenuItem_AddSearchBoxValuesAsKeyReplacementClick(Sender: TObject);
begin
  //
end;


procedure TfrmAllDTRsMain.spdbtnProjectGroupsClick(Sender: TObject);
begin
  pmProjectGroups.PopUp;
end;


function TfrmAllDTRsMain.GetGroupPrefixFromIndex: string;
begin
  Result := 'Grp_' + IntToStr(FActiveGroupIndex) + '.';
end;


procedure TfrmAllDTRsMain.LoadActiveProjectGroup(Ini: TMemIniFile; GroupPrefix: string);
var
  TabCount, i, ActiveTabIndex: Integer;
  ProjectName: string;
  Content: TfrDTR;
  KeyReplacementsCount: Integer;
begin
  TabCount := Ini.ReadInteger('Settings', GroupPrefix + 'TabCount', 0);

  for i := 0 to TabCount - 1 do
  begin
    Content := TfrDTR(frTabs.AddTabToEnd);
    ProjectName := Ini.ReadString('Settings', GroupPrefix + 'ProjectName_' + IntToStr(i), '');

    if ProjectName <> '' then
      Content.LoadDTRProject(ProjectName);

    Content.LoadSettingsFromIni(Ini, GroupPrefix + '_' + IntToStr(i));

    KeyReplacementsCount := Ini.ReadInteger('KeyReplacements', GroupPrefix + 'Count', 0);
    if (KeyReplacementsCount < 0) or (KeyReplacementsCount > 100) then
      KeyReplacementsCount := 100;

    SetLength(Content.KeyReplacementArr, KeyReplacementsCount);
  end;

  ActiveTabIndex := Ini.ReadInteger('Settings', GroupPrefix + 'ActiveTabIndex', 0);
  if ActiveTabIndex < 0 then
    if frTabs.TabCount > 0 then
      ActiveTabIndex := 0;

  frTabs.ActiveTabIndex := ActiveTabIndex;

  //Apply settings again:
  Application.ProcessMessages;
  for i := 0 to TabCount - 1 do
    Content.LoadSettingsFromIni(Ini, GroupPrefix + '_' + IntToStr(i));
end;


procedure TfrmAllDTRsMain.LoadSettingsFromIni;
var
  Ini: TMemIniFile;
  GroupPrefix: string;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'AllDTRs.ini');
  try
    Left := Ini.ReadInteger('Window', 'Left', Left);
    Top := Ini.ReadInteger('Window', 'Top', Top);
    Width := Ini.ReadInteger('Window', 'Width', Width);
    Height := Ini.ReadInteger('Window', 'Height', Height);

    FProjectGroupsCount := Ini.ReadInteger('ProjectGroups', 'ProjectGroupsCount', 0);
    if FProjectGroupsCount <= 0 then
    begin
      GroupPrefix := '';
      FActiveGroupIndex := -1;
    end
    else
    begin
      FActiveGroupIndex := Ini.ReadInteger('ProjectGroups', 'ActiveGroupIndex', -1);
      if (FActiveGroupIndex < 0) or (FActiveGroupIndex > FProjectGroupsCount - 1) then
        GroupPrefix := ''
      else
        GroupPrefix := GetGroupPrefixFromIndex;
    end;

    CreateAllProjectGroupItems;
    LoadActiveProjectGroup(Ini, GroupPrefix);

    if (FActiveGroupIndex >= 0) and (FActiveGroupIndex < FProjectGroupsCount) then
      MenuItem_SelectProjectGroup.Items[FActiveGroupIndex].Checked := True;
  finally
    Ini.Free;
  end;
end;


procedure TfrmAllDTRsMain.SaveActiveProjectGroup(Ini: TMemIniFile; GroupPrefix: string);
var
  i: Integer;
  Content: TfrDTR;
begin
  Ini.WriteInteger('Settings', GroupPrefix + 'TabCount', frTabs.TabCount);
  for i := 0 to frTabs.TabCount - 1 do
  begin
    Content := TfrDTR(frTabs.Content[i]);
    Content.SaveSettingsToIni(Ini, GroupPrefix + '_' + IntToStr(i));
    Ini.WriteString('Settings', GroupPrefix + 'ProjectName_' + IntToStr(i), Content.ProjectName);
  end;

  Ini.WriteInteger('Settings', GroupPrefix + 'ActiveTabIndex', frTabs.ActiveTabIndex);
end;


procedure TfrmAllDTRsMain.SaveSettingsToIni;
var
  Ini: TMemIniFile;
  GroupPrefix: string;
begin
  Ini := TMemIniFile.Create(ExtractFilePath(ParamStr(0)) + 'AllDTRs.ini');
  try
    Ini.WriteInteger('Window', 'Left', Left);
    Ini.WriteInteger('Window', 'Top', Top);
    Ini.WriteInteger('Window', 'Width', Width);
    Ini.WriteInteger('Window', 'Height', Height);

    Ini.WriteInteger('ProjectGroups', 'ProjectGroupsCount', FProjectGroupsCount);
    Ini.WriteInteger('ProjectGroups', 'ActiveGroupIndex', FActiveGroupIndex);

    if FActiveGroupIndex < 0 then
      GroupPrefix := ''
    else
      GroupPrefix := GetGroupPrefixFromIndex;

    SaveActiveProjectGroup(Ini, GroupPrefix);

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


procedure TfrmAllDTRsMain.CloseProjectGroup;
var
  i: Integer;
begin
  for i := frTabs.TabCount - 1 downto 0 do
    frTabs.DeleteTab(i);
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

