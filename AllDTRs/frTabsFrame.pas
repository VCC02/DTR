{
    Copyright (C) 2025 VCC
    creation date: 28 Oct 2025
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


unit frTabsFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ECTabCtrl, ECTypes;

type
  TOnAddTab = procedure(out ATabContent: Pointer) of object;  //ATabContent is a pointer to the object, created by the assigned OnAddTab handler (likely the content of a tab sheet). It is kept by this frame.
  TOnDeleteTab = procedure(ATabContent: Pointer) of object;
  TOnChangeTab = procedure(AOldTabContent, ANewTabContent: Pointer) of object; //When switching tabs, AOldTab is the one being hidden and ANewTab is the one being focused.

  { TfrTabs }

  TfrTabs = class(TFrame)
    ECTabCtrl1: TECTabCtrl;
    tmrSetTabCaption: TTimer;
    tmrTabChange: TTimer;
    procedure ECTabCtrl1Add(Sender: TObject; AIndex: Integer);
    procedure ECTabCtrl1Change(Sender: TObject);
    procedure ECTabCtrl1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tmrTabChangeTimer(Sender: TObject);
  private
    FTabIDs: TStringList;
    FTabContents: TStringList;
    FOldTabIndex: Integer;

    FOnAddTab: TOnAddTab;
    FOnDeleteTab: TOnDeleteTab;
    FOnChangeTab: TOnChangeTab;

    function GetTabCount: Integer;
    function GetActiveTabIndex: Integer;
    procedure SetActiveTabIndex(Value: Integer);
    function GetContent(Index: Integer): Pointer;

    function GetTabIndexByIDStr(ATabIDStr: string): Integer;
    function GetTabIndexByContent(ATabContent: Pointer): Integer;
    function GetDeletedTabIndex: Integer;

    procedure DoOnAddTab(out ATabContent: Pointer);
    procedure DoOnDeleteTab(ATabContent: Pointer);
    procedure DoOnChangeTab(AOldTabContent, ANewTabContent: Pointer);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetTabCaption(ATabContent: Pointer; ANewCaption: string);
    procedure SetTabVisibilityByIndex(AIndex: Integer; AVisibility: Boolean);
    function AddTabToEnd: Pointer; //returns tab content

    property TabCount: Integer read GetTabCount;
    property ActiveTabIndex: Integer read GetActiveTabIndex write SetActiveTabIndex;
    property Content[Index: Integer]: Pointer read GetContent;

    property OnAddTab: TOnAddTab write FOnAddTab;
    property OnDeleteTab: TOnDeleteTab write FOnDeleteTab;
    property OnChangeTab: TOnChangeTab write FOnChangeTab;
  end;

implementation

{$R *.frm}

{ TfrTabs }


constructor TfrTabs.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  ECTabCtrl1.Options := ECTabCtrl1.Options + [etcoAddTabButton, etcoCloseBtnActiveOnly, etcoMiddleButtonClose];
  FTabIDs := TStringList.Create;
  FTabContents := TStringList.Create;

  FOnAddTab := nil;
  FOnDeleteTab := nil;
  FOnChangeTab := nil;

  FOldTabIndex := -1;
end;


destructor TfrTabs.Destroy;
begin
  FreeAndNil(FTabIDs);
  FreeAndNil(FTabContents);
  inherited Destroy;
end;


function TfrTabs.GetTabCount: Integer;
begin
  Result := FTabIDs.Count;
end;


function TfrTabs.GetActiveTabIndex: Integer;
begin
  Result := ECTabCtrl1.TabIndex;
end;


procedure TfrTabs.SetActiveTabIndex(Value: Integer);
begin
  if ECTabCtrl1.TabIndex <> Value then
    if (Value > -1) and (Value < ECTabCtrl1.Tabs.Count) then
      ECTabCtrl1.TabIndex := Value;
end;


function TfrTabs.GetContent(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index > FTabIDs.Count - 1) then
  begin
    Result := nil;
    Exit;
  end;

  Result := Pointer(PtrUInt(StrToInt64Def(FTabContents.Strings[Index], 0)));
end;


function TfrTabs.GetTabIndexByIDStr(ATabIDStr: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to ECTabCtrl1.Tabs.Count - 1 do
    if ATabIDStr = IntToStr(PtrUInt(ECTabCtrl1.Tabs.Items[i])) then
    begin
      Result := i;
      Break;
    end;
end;


function TfrTabs.GetTabIndexByContent(ATabContent: Pointer): Integer;
begin
  Result := FTabContents.IndexOf(IntToStr(PtrUInt(ATabContent)));
end;


function TfrTabs.GetDeletedTabIndex: Integer;
var
  i, j: Integer;
  TabID: PtrUInt;
  Found: Boolean;
begin
  Result := -1;
  for i := 0 to FTabIDs.Count - 1 do
  begin
    TabID := StrToInt64Def(FTabIDs.Strings[i], -1);

    if TabID <> 0 then
    begin
      Found := False;
      for j := 0 to ECTabCtrl1.Tabs.Count - 1 do
        if TabID = PtrUInt(ECTabCtrl1.Tabs.Items[j].Tag) then
        begin
          Found := True;
          Break;
        end;

      if not Found then
      begin
        Result := i;
        Exit;
      end;
    end;
  end;
end;


procedure TfrTabs.SetTabCaption(ATabContent: Pointer; ANewCaption: string);
var
  TabIndex: Integer;
begin
  TabIndex := GetTabIndexByContent(ATabContent);
  if TabIndex = -1 then
    raise Exception.Create('Tab not found by content.');

  ECTabCtrl1.Tabs.Items[TabIndex].Text := ANewCaption;
end;


procedure TfrTabs.SetTabVisibilityByIndex(AIndex: Integer; AVisibility: Boolean);
begin
  if (AIndex < 0) or (AIndex > ECTabCtrl1.Tabs.Count -1) then
    raise Exception.Create('Tab not found by index.');

  if AVisibility then
    ECTabCtrl1.Tabs.Items[AIndex].Options := ECTabCtrl1.Tabs.Items[AIndex].Options + [etoVisible]
  else
    ECTabCtrl1.Tabs.Items[AIndex].Options := ECTabCtrl1.Tabs.Items[AIndex].Options - [etoVisible];
end;


function TfrTabs.AddTabToEnd: Pointer; //returns tab content
var
  Tab: TECTab;
begin
  Tab := ECTabCtrl1.AddTab(etaLast, False);
  Result := Content[Tab.Index];
end;


procedure TfrTabs.DoOnAddTab(out ATabContent: Pointer);
begin
  if Assigned(FOnAddTab) then
    FOnAddTab(ATabContent)
  else
    ATabContent := nil;
end;


procedure TfrTabs.DoOnDeleteTab(ATabContent: Pointer);
begin
  if Assigned(FOnDeleteTab) then
    FOnDeleteTab(ATabContent);
end;


procedure TfrTabs.DoOnChangeTab(AOldTabContent, ANewTabContent: Pointer);
begin
  if Assigned(FOnChangeTab) then
    FOnChangeTab(AOldTabContent, ANewTabContent);
end;


procedure TfrTabs.ECTabCtrl1Add(Sender: TObject; AIndex: Integer);
var
  TabContent: Pointer;
begin
  ECTabCtrl1.Tabs.Items[AIndex].Options := [etoCanBeFolded, etoCanFold, etoCloseable, etoCloseBtn, etoVisible];   //do not show the close button
  ECTabCtrl1.Tabs.Items[AIndex].Tag := PtrUInt(ECTabCtrl1.Tabs.Items[AIndex]);
  FTabIDs.Add(IntToStr(PtrUInt(ECTabCtrl1.Tabs.Items[AIndex])));
  ECTabCtrl1.Tabs.Items[AIndex].Text := 'New tab';

  DoOnAddTab(TabContent);
  FTabContents.Add(IntToStr(PtrUInt(TabContent)));
end;


procedure TfrTabs.ECTabCtrl1Change(Sender: TObject);  //Called before ECTabCtrl1Add with an updated ECTabCtrl1.Tabs.Count.
var
  TabContent: Pointer;
  TabIndex: Integer;
begin
  if ECTabCtrl1.Tabs.Count < FTabIDs.Count then
  begin //deleted a tab
    TabIndex := GetDeletedTabIndex;
    if TabIndex = -1 then
      raise Exception.Create('Deleted unknown tab.')
    else
    begin
      TabContent := Pointer(StrToInt64Def(FTabContents.Strings[TabIndex], -1));  //used later to delete frames
      DoOnDeleteTab(TabContent);
      FTabIDs.Delete(TabIndex);
      FTabContents.Delete(TabIndex);
    end;
  end
  else
  begin
    FOldTabIndex := ECTabCtrl1.TabIndex;
    tmrTabChange.Enabled := True;  //the timer handler is executed after updating TabIndex
  end;
end;


procedure TfrTabs.ECTabCtrl1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i, j, NewIndex: Integer;
  s: string;
begin
  //This handler is executed when moving tabs. It is not executed when adding or deleting tabs (the tab count is the same).
  //FTabIDs and FTabContents should move the same item, to the same position as the tab.

  for i := 0 to ECTabCtrl1.Tabs.Count - 1 do
    if FTabIDs.Strings[i] <> IntToStr(PtrUInt(ECTabCtrl1.Tabs.Items[i])) then  //found moved tab
    begin
      NewIndex := GetTabIndexByIDStr(FTabIDs.Strings[i]); //this is where the tab was moved

      if NewIndex > -1 then
      begin
        FTabIDs.Move(i, NewIndex);
        FTabContents.Move(i, NewIndex);
      end
      else
      begin
        s := '';
        for j := 0 to ECTabCtrl1.Tabs.Count - 1 do
          s := s + '  ' + IntToStr(PtrUInt(ECTabCtrl1.Tabs.Items[j]));

        raise Exception.Create('Tab not found by index:  ' + FTabIDs.Strings[i] + ' in  ' + s);
      end;

      Break;
    end;
end;


procedure TfrTabs.tmrTabChangeTimer(Sender: TObject);
begin
  tmrTabChange.Enabled := False;
  DoOnChangeTab(Content[FOldTabIndex], Content[ECTabCtrl1.TabIndex]);
end;


end.

