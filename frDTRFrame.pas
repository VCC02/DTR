{
    Copyright (C) 2025 VCC
    creation date: 10 Sep 2025
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


unit frDTRFrame;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, Menus, VirtualTrees, IniFiles;

type
  TDL = record
    L1, L2: string;
  end;

  TDLArr = array of TDL;

  TOnSetProjectName = procedure(Sender: TObject; AName: string) of object;

  { TfrDTR }

  TfrDTR = class(TFrame)
    btnOverwriteLeft: TButton;
    btnLoadProject: TButton;
    btnOverwriteRight: TButton;
    btnSaveProject: TButton;
    btnAddLine: TButton;
    btnNewProject: TButton;
    edtSearchL1: TEdit;
    edtSearchL2: TEdit;
    lblModified: TLabel;
    lblProjectName: TLabel;
    memL1: TMemo;
    memL2: TMemo;
    MenuItem_AppendToRight: TMenuItem;
    MenuItem_RemoveSelectedLines: TMenuItem;
    pnlSplitterVST: TPanel;
    pnlL: TPanel;
    pnlL1: TPanel;
    pnlL2: TPanel;
    pnlProjectName: TPanel;
    pmExtraOverwriteRight: TPopupMenu;
    Separator1: TMenuItem;
    MenuItem_CopyL2ToClipboard: TMenuItem;
    MenuItem_CopyL1ToClipboard: TMenuItem;
    MenuItem_AppendToLeft: TMenuItem;
    MenuItem_SaveProjectAs: TMenuItem;
    OpenDialog1: TOpenDialog;
    pmExtraSave: TPopupMenu;
    pmExtraOverwriteLeft: TPopupMenu;
    pmVST: TPopupMenu;
    SaveDialog1: TSaveDialog;
    spdbtnExtraOverwriteRight: TSpeedButton;
    spdbtnExtraSave: TSpeedButton;
    spdbtnExtraOverwriteLeft: TSpeedButton;
    tmrEdit: TTimer;
    tmrSearch: TTimer;
    vstDual: TVirtualStringTree;
    procedure btnAddLineClick(Sender: TObject);
    procedure btnLoadProjectClick(Sender: TObject);
    procedure btnNewProjectClick(Sender: TObject);
    procedure btnOverwriteLeftClick(Sender: TObject);
    procedure btnOverwriteRightClick(Sender: TObject);
    procedure btnSaveProjectClick(Sender: TObject);
    procedure edtSearchL1Change(Sender: TObject);
    procedure edtSearchL2Change(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure MenuItem_AppendToLeftClick(Sender: TObject);
    procedure MenuItem_AppendToRightClick(Sender: TObject);
    procedure MenuItem_CopyL1ToClipboardClick(Sender: TObject);
    procedure MenuItem_CopyL2ToClipboardClick(Sender: TObject);
    procedure MenuItem_RemoveSelectedLinesClick(Sender: TObject);
    procedure MenuItem_SaveProjectAsClick(Sender: TObject);
    procedure pnlSplitterVSTMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlSplitterVSTMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pnlSplitterVSTMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure spdbtnExtraOverwriteLeftClick(Sender: TObject);
    procedure spdbtnExtraOverwriteRightClick(Sender: TObject);
    procedure spdbtnExtraSaveClick(Sender: TObject);
    procedure tmrEditTimer(Sender: TObject);
    procedure tmrSearchTimer(Sender: TObject);
    procedure vstDualCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure vstDualDblClick(Sender: TObject);
    procedure vstDualEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure vstDualEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: boolean);
    procedure vstDualGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstDualKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vstDualMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure vstDualNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: string);
  private
    FDLArr: TDLArr;
    FProjectName: string;
    FModified: Boolean;
    FEditingText: string;
    FHitInfo: THitInfo;
    FTextEditorEditBox: TEdit;

    FHold: Boolean;
    FSplitterMouseDownGlobalPos: TPoint;
    FSplitterMouseDownImagePos: TPoint;

    FOnSetProjectName: TOnSetProjectName;

    procedure SetModified(Value: Boolean);
    procedure RemoveLineFromArr(AIndex: Integer);
    procedure RemoveSelectedLines;
    procedure SearchInVST;
    procedure ResizeFrameSectionsBySplitter(NewLeft: Integer);

    procedure LoadProject(AFnm: string);
    procedure SaveProject(AFnm: string);

    procedure DoOnSetProjectName(Sender: TObject; AName: string);

    property Modified: Boolean read FModified write SetModified;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadSettingsFromIni(AIni: TMemIniFile; AFramePrefix: string);
    procedure SaveSettingsToIni(AIni: TMemIniFile; AFramePrefix: string);

    procedure LoadDTRProject(AFnm: string);

    procedure SetSearchL1(ASearchText: string);
    procedure SetSearchL2(ASearchText: string);

    property ProjectName: string read FProjectName;
    property OnSetProjectName: TOnSetProjectName write FOnSetProjectName;
  end;


implementation

{$R *.frm}

uses
  Clipbrd, Math;

{ TfrDTR }


constructor TfrDTR.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FEditingText := '';
  FHold := False;
end;


destructor TfrDTR.Destroy;
begin
  //
  inherited Destroy;
end;


procedure TfrDTR.SetModified(Value: Boolean);
begin
  FModified := Value;
  lblModified.Visible := Value;
  lblProjectName.Caption := 'Project: ' + FProjectName;
end;


procedure TfrDTR.LoadSettingsFromIni(AIni: TMemIniFile; AFramePrefix: string);
var
  i, NewLeft: Integer;
begin
  for i := 0 to vstDual.Header.Columns.Count - 1 do
    vstDual.Header.Columns[i].Width := AIni.ReadInteger(AFramePrefix + 'VST', 'Width_' + IntToStr(i), vstDual.Header.Columns[i].Width);

  NewLeft := AIni.ReadInteger(AFramePrefix + 'VST', 'SplitterVST.Left_' + IntToStr(i), pnlSplitterVST.Left);
  ResizeFrameSectionsBySplitter(NewLeft);
end;


procedure TfrDTR.SaveSettingsToIni(AIni: TMemIniFile; AFramePrefix: string);
var
  i: Integer;
begin
  for i := 0 to vstDual.Header.Columns.Count - 1 do
    AIni.WriteInteger(AFramePrefix + 'VST', 'Width_' + IntToStr(i), vstDual.Header.Columns[i].Width);

  AIni.WriteInteger(AFramePrefix + 'VST', 'SplitterVST.Left_' + IntToStr(i), pnlSplitterVST.Left);
end;


procedure TfrDTR.LoadProject(AFnm: string);
var
  Ini: TMemIniFile;
  L1, L2: TStringList;
  L1Raw, L2Raw: TStringList;
  i: Integer;
begin
  Ini := TMemIniFile.Create(AFnm);
  try
    L1 := TStringList.Create;
    L2 := TStringList.Create;
    L1Raw := TStringList.Create;
    L2Raw := TStringList.Create;
    try
      Ini.ReadSectionValues('L1', L1);
      Ini.ReadSectionValues('L2', L2);
      Ini.ReadSectionValues('L1Raw', L1Raw);
      Ini.ReadSectionValues('L2Raw', L2Raw);

      SetLength(FDLArr, L1.Count);
      for i := 0 to Length(FDLArr) - 1 do
      begin
        try
          FDLArr[i].L1 := L1.ValueFromIndex[i];
          FDLArr[i].L2 := L2.ValueFromIndex[i];
        except
          FDLArr[i].L2 := 'ex';
        end;
      end;

      memL1.Clear;
      memL2.Clear;

      for i := 0 to L1Raw.Count - 1 do
        memL1.Lines.Add(L1Raw.ValueFromIndex[i]);

      for i := 0 to L2Raw.Count - 1 do
        memL2.Lines.Add(L2Raw.ValueFromIndex[i]);
    finally
      L1.Free;
      L2.Free;
      L1Raw.Free;
      L2Raw.Free;
    end;
  finally
    Ini.Free;
  end;

  vstDual.RootNodeCount := Length(FDLArr);
  vstDual.Repaint;
end;


procedure TfrDTR.SaveProject(AFnm: string);
var
  Content: TStringList;
  i: Integer;
begin
  Content := TStringList.Create;
  try
    Content.Add('[L1]');
    for i := 0 to Length(FDLArr) - 1 do
      Content.Add('i_' + IntToStr(i) + '=' + FDLArr[i].L1);

    Content.Add('');
    Content.Add('[L2]');
    for i := 0 to Length(FDLArr) - 1 do
      Content.Add('i_' + IntToStr(i) + '=' + FDLArr[i].L2);

    Content.Add('');
    Content.Add('[L1Raw]');
    for i := 0 to memL1.Lines.Count - 1 do
      Content.Add('i_' + IntToStr(i) + '=' + memL1.Lines.Strings[i]);

    Content.Add('');
    Content.Add('[L2Raw]');
    for i := 0 to memL2.Lines.Count - 1 do
      Content.Add('i_' + IntToStr(i) + '=' + memL2.Lines.Strings[i]);

    Content.SaveToFile(AFnm);
  finally
    Content.Free;
  end;
end;


procedure TfrDTR.LoadDTRProject(AFnm: string);
begin
  FProjectName := AFnm;
  LoadProject(FProjectName);
  Modified := False;
  DoOnSetProjectName(Self, FProjectName);
end;


procedure TfrDTR.btnLoadProjectClick(Sender: TObject);
begin
  if not OpenDialog1.Execute then
    Exit;

  FProjectName := OpenDialog1.FileName;
  LoadProject(FProjectName);
  Modified := False;
  DoOnSetProjectName(Self, FProjectName);
end;


procedure TfrDTR.btnNewProjectClick(Sender: TObject);
begin
  if MessageDlg('Are you sure you want to clear the existing project?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    Exit;

  FProjectName := '';
  SetLength(FDLArr, 0);
  vstDual.RootNodeCount := 0;
  vstDual.Repaint;
  memL1.Clear;
  memL2.Clear;

  Modified := False;
  DoOnSetProjectName(Self, 'New project');
end;


procedure TfrDTR.btnOverwriteLeftClick(Sender: TObject);
var
  i: Integer;
begin
  SetLength(FDLArr, memL1.Lines.Count);

  for i := 0 to Length(FDLArr) - 1 do
  begin
    try
      FDLArr[i].L1 := memL1.Lines.Strings[i];
      FDLArr[i].L2 := memL2.Lines.Strings[i];
    except
      FDLArr[i].L2 := 'ex';
    end;
  end;

  Modified := True;
  vstDual.RootNodeCount := Length(FDLArr);
  vstDual.Repaint;
end;


procedure TfrDTR.btnOverwriteRightClick(Sender: TObject);
var
  i: Integer;
begin
  memL1.Lines.Clear;
  memL2.Lines.Clear;

  for i := 0 to Length(FDLArr) - 1 do
  begin
    try
      memL1.Lines.Add(FDLArr[i].L1);
      memL2.Lines.Add(FDLArr[i].L2);
    except
      FDLArr[i].L2 := 'ex';
    end;
  end;

  Modified := True;
end;


procedure TfrDTR.btnSaveProjectClick(Sender: TObject);
var
  FnmUpdatedSuccessfully: Boolean;
begin
  FnmUpdatedSuccessfully := False;

  if FProjectName = '' then
  begin
    if not SaveDialog1.Execute then
      Exit;

    FProjectName := SaveDialog1.FileName;
    FnmUpdatedSuccessfully := True;
  end;

  SaveProject(FProjectName);
  Modified := False;

  if FnmUpdatedSuccessfully then
    DoOnSetProjectName(Self, FProjectName);
end;


procedure TfrDTR.edtSearchL1Change(Sender: TObject);
begin
  tmrSearch.Enabled := True;

  if edtSearchL1.Text > '' then
    edtSearchL1.Color := clYellow
  else
    edtSearchL1.Color := clWindow;
end;


procedure TfrDTR.edtSearchL2Change(Sender: TObject);
begin
  tmrSearch.Enabled := True;

  if edtSearchL2.Text > '' then
    edtSearchL2.Color := clYellow
  else
    edtSearchL2.Color := clWindow;
end;


procedure TfrDTR.MenuItem_AppendToLeftClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to memL1.Lines.Count - 1 do
  begin
    SetLength(FDLArr, Length(FDLArr) + 1);
    try
      FDLArr[Length(FDLArr) - 1].L1 := memL1.Lines.Strings[i];
      FDLArr[Length(FDLArr) - 1].L2 := memL2.Lines.Strings[i];
    except
      FDLArr[Length(FDLArr) - 1].L2 := 'ex';
    end;
  end;

  Modified := True;
  vstDual.RootNodeCount := Length(FDLArr);
  vstDual.Repaint;
end;


procedure TfrDTR.MenuItem_AppendToRightClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Length(FDLArr) - 1 do
  begin
    memL1.Lines.Add(FDLArr[i].L1);
    memL2.Lines.Add(FDLArr[i].L2);
  end;

  Modified := True;
end;


procedure TfrDTR.MenuItem_CopyL1ToClipboardClick(Sender: TObject);
var
  Content: TStringList;
  i: Integer;
begin
  Content := TStringList.Create;
  try
    for i := 0 to Length(FDLArr) - 1 do
      Content.Add(FDLArr[i].L1);

    Clipboard.AsText := Content.Text;
  finally
    Content.Free;
  end;
end;


procedure TfrDTR.MenuItem_CopyL2ToClipboardClick(Sender: TObject);
var
  Content: TStringList;
  i: Integer;
begin
  Content := TStringList.Create;
  try
    for i := 0 to Length(FDLArr) - 1 do
      Content.Add(FDLArr[i].L2);

    Clipboard.AsText := Content.Text;
  finally
    Content.Free;
  end;
end;


procedure TfrDTR.RemoveLineFromArr(AIndex: Integer);
var
  i: Integer;
begin
  if (AIndex < 0) or (AIndex > Length(FDLArr) - 1) then
    Exit;

  for i := AIndex to Length(FDLArr) - 2 do
    FDLArr[i] := FDLArr[i + 1];

  SetLength(FDLArr, Length(FDLArr) - 1);
  vstDual.RootNodeCount := Length(FDLArr);
end;


procedure TfrDTR.RemoveSelectedLines;
var
  Node: PVirtualNode;
begin
  Node := vstDual.GetFirst;
  if Node = nil then
    Exit;

  if MessageDlg('Are you sure you want to remove the selected lines?', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
    Exit;

  vstDual.BeginUpdate;
  try
    Node^.PrevSibling := nil;

    Node := vstDual.GetLast;
    if Node = nil then
      Exit;

    repeat
      if vstDual.Selected[Node] then
        RemoveLineFromArr(Node^.Index);

      Node := Node^.PrevSibling;
    until Node = nil;
  finally
    vstDual.EndUpdate;
  end;

  vstDual.Repaint; //EndUpdate may call Invalidate
end;


procedure TfrDTR.MenuItem_RemoveSelectedLinesClick(Sender: TObject);
begin
  RemoveSelectedLines;
end;


procedure TfrDTR.MenuItem_SaveProjectAsClick(Sender: TObject);
begin
  if FModified then
    if MessageDlg('The current project is modified. Do you want to save it?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      btnSaveProjectClick(nil);

  if not SaveDialog1.Execute then
    Exit;

  FProjectName := SaveDialog1.FileName;
  SaveProject(FProjectName);
  Modified := False;
  DoOnSetProjectName(Self, FProjectName);
end;


procedure TfrDTR.pnlSplitterVSTMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Shift <> [ssLeft] then
    Exit;

  if not FHold then
  begin
    GetCursorPos(FSplitterMouseDownGlobalPos);

    FSplitterMouseDownImagePos.X := pnlSplitterVST.Left;
    FHold := True;
  end;
end;


procedure TfrDTR.pnlSplitterVSTMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  tp: TPoint;
  NewLeft: Integer;
begin
  if Shift <> [ssLeft] then
    Exit;

  if not FHold then
    Exit;

  GetCursorPos(tp);
  NewLeft := FSplitterMouseDownImagePos.X + tp.X - FSplitterMouseDownGlobalPos.X;

  ResizeFrameSectionsBySplitter(NewLeft);
end;


procedure TfrDTR.pnlSplitterVSTMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FHold := False;
end;


procedure TfrDTR.FrameResize(Sender: TObject);
var
  NewLeft: Integer;
begin                                   //this method doesn't seem to be called before showing the owner window/frame
  NewLeft := pnlSplitterVST.Left;       //that is why Width has its initial (small) value, causing NewLeft to adapt to it
  ResizeFrameSectionsBySplitter(NewLeft);
end;


procedure TfrDTR.ResizeFrameSectionsBySplitter(NewLeft: Integer);
begin
  if NewLeft < vstDual.Constraints.MinWidth then
    NewLeft := vstDual.Constraints.MinWidth;

  if NewLeft > Width - 544 then
    NewLeft := Width - 544;

  pnlSplitterVST.Left := NewLeft;

  pnlL.Left := pnlSplitterVST.Left + pnlSplitterVST.Width;
  pnlL.Width := Width - pnlL.Left;

  pnlL1.Width := pnlL.Width shr 1 - 4;
  pnlL2.Width := pnlL1.Width;
  pnlL2.Left := pnlL1.Left + pnlL1.Width + 4;

  vstDual.Width := pnlSplitterVST.Left;
end;


procedure TfrDTR.spdbtnExtraOverwriteLeftClick(Sender: TObject);
begin
  pmExtraOverwriteLeft.PopUp;
end;


procedure TfrDTR.spdbtnExtraOverwriteRightClick(Sender: TObject);
begin
  pmExtraOverwriteRight.PopUp;
end;


procedure TfrDTR.spdbtnExtraSaveClick(Sender: TObject);
begin
  pmExtraSave.PopUp;
end;


procedure TfrDTR.tmrEditTimer(Sender: TObject);
var
  TempBounds: TRect;
begin
  tmrEdit.Enabled := False;
  if FHitInfo.HitNode = nil then
    Exit;

  vstDual.EditNode(FHitInfo.HitNode, FHitInfo.HitColumn);

  if FHitInfo.HitColumn in [0..1] then
    if Assigned(vstDual.EditLink) then
    begin
      TempBounds := vstDual.EditLink.GetBounds;
      TempBounds.Left := TempBounds.Left - 2;
      TempBounds.Right := Max(TempBounds.Right, TempBounds.Left + vstDual.Header.Columns[FHitInfo.HitColumn].MinWidth);

      if FHitInfo.HitColumn = 0 then
        TempBounds.Right := TempBounds.Right - vstDual.Indent;

      {$IFDEF UNIX}
        TempBounds.Top := TempBounds.Top - 1;
        TempBounds.Height := TempBounds.Height + 2;
      {$ENDIF}

      vstDual.EditLink.SetBounds(TempBounds);

      FTextEditorEditBox.Height := vstDual.DefaultNodeHeight;

      {$IFDEF UNIX}
        FTextEditorEditBox.Font.Size := FTextEditorEditBox.Font.Size + 2;
        FTextEditorEditBox.Height := FTextEditorEditBox.Height + 2;
      {$ENDIF}
    end;
end;


procedure TfrDTR.tmrSearchTimer(Sender: TObject);
begin
  tmrSearch.Enabled := False;
  SearchInVST;
end;


procedure TfrDTR.vstDualCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
var
  TempStringEditLink: TStringEditLink;
begin
  TempStringEditLink := TStringEditLink.Create;
  EditLink := TempStringEditLink;

  FTextEditorEditBox := TEdit(TCustomEdit(TempStringEditLink.Edit));
  FTextEditorEditBox.Font.Name := 'Tahoma';
  FTextEditorEditBox.Font.Size := 8;
  //FTextEditorEditBox.Height := vstVariables.DefaultNodeHeight - 3;  //set again in timer

  FTextEditorEditBox.Show;
end;


procedure TfrDTR.vstDualDblClick(Sender: TObject);
begin
  tmrEdit.Enabled := True;
end;


procedure TfrDTR.vstDualEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
begin
  case Column of
    0:
    begin
      if FDLArr[Node^.Index].L1 <> FEditingText then
        Modified := True;

      FDLArr[Node^.Index].L1 := FEditingText;
    end;

    1:
    begin
      if FDLArr[Node^.Index].L2 <> FEditingText then
        Modified := True;

      FDLArr[Node^.Index].L2 := FEditingText;
    end;
  end;
end;


procedure TfrDTR.vstDualEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: boolean);
begin
  Allowed := True;

  case Column of
    0:
      FEditingText := FDLArr[Node^.Index].L1;

    1:
      FEditingText := FDLArr[Node^.Index].L2;
  end;
end;


procedure TfrDTR.vstDualGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
  try
    case Column of
      0: CellText := FDLArr[Node^.Index].L1;
      1: CellText := FDLArr[Node^.Index].L2;
    end;
  except
    CellText := 'bug';
  end;
end;


procedure TfrDTR.vstDualKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Node: PVirtualNode;
begin
  case Key of
    VK_DELETE:
      RemoveSelectedLines;

    Ord('F'):
    begin
      if ssCtrl in Shift then
        if FHitInfo.HitColumn = 1 then
          edtSearchL2.SetFocus
        else
          edtSearchL1.SetFocus; //default
    end;

    Ord('C'):
    begin
      Node := vstDual.GetFirstSelected;
      if Node = nil then
        Exit;

      if FHitInfo.HitColumn = 1 then
        Clipboard.AsText := FDLArr[Node^.Index].L2
      else
        Clipboard.AsText := FDLArr[Node^.Index].L1;
    end;
  end; //case
end;


procedure TfrDTR.vstDualMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  vstDual.GetHitTestInfoAt(X, Y, True, FHitInfo);
end;


procedure TfrDTR.vstDualNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; const NewText: string);
begin
  FEditingText := NewText;
end;


procedure TfrDTR.btnAddLineClick(Sender: TObject);
begin
  SetLength(FDLArr, Length(FDLArr) + 1);
  vstDual.RootNodeCount := Length(FDLArr);
  vstDual.Repaint;
  Modified := True;
end;


procedure TfrDTR.SearchInVST;
var
  Node: PVirtualNode;
  s1, s2: string;
  su1, su2: string;
begin
  Node := vstDual.GetFirst;
  if Node = nil then
    Exit;

  s1 := edtSearchL1.Text;
  s2 := edtSearchL2.Text;
  su1 := UpperCase(s1);
  su2 := UpperCase(s2);

  vstDual.BeginUpdate;
  try
    repeat
      vstDual.IsVisible[Node] := ((s1 = '') or (Pos(su1, UpperCase(FDLArr[Node^.Index].L1)) > 0)) and
                               ((s2 = '') or (Pos(su2, UpperCase(FDLArr[Node^.Index].L2)) > 0));
      Node := Node^.NextSibling;
    until Node = nil;
  finally
    vstDual.EndUpdate;
  end;

  vstDual.Repaint;
end;


procedure TfrDTR.SetSearchL1(ASearchText: string);
begin
  edtSearchL1.Text := ASearchText;
end;


procedure TfrDTR.SetSearchL2(ASearchText: string);
begin
  edtSearchL2.Text := ASearchText;
end;


procedure TfrDTR.DoOnSetProjectName(Sender: TObject; AName: string);
begin
  if Assigned(FOnSetProjectName) then
    FOnSetProjectName(Self, AName);
end;

end.

