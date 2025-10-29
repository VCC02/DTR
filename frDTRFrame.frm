object frDTR: TfrDTR
  Left = 0
  Height = 478
  Top = 0
  Width = 1164
  ClientHeight = 478
  ClientWidth = 1164
  Constraints.MinHeight = 478
  Constraints.MinWidth = 1164
  TabOrder = 0
  OnResize = FrameResize
  DesignLeft = 86
  DesignTop = 85
  object vstDual: TVirtualStringTree
    Cursor = 63
    Left = 8
    Height = 392
    Top = 40
    Width = 608
    Anchors = [akTop, akLeft, akBottom]
    Constraints.MinWidth = 400
    DefaultNodeHeight = 20
    DefaultText = 'Node'
    Header.AutoSizeIndex = 0
    Header.Columns = <    
      item
        MinWidth = 300
        Position = 0
        Text = 'L1'
        Width = 300
      end    
      item
        MinWidth = 300
        Position = 1
        Text = 'L2'
        Width = 300
      end>
    Header.DefaultHeight = 21
    Header.Height = 21
    Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible]
    PopupMenu = pmVST
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes, toDisableAutoscrollOnFocus, toAutoChangeScale, toDisableAutoscrollOnEdit]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnDblClick]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
    OnCreateEditor = vstDualCreateEditor
    OnDblClick = vstDualDblClick
    OnEdited = vstDualEdited
    OnEditing = vstDualEditing
    OnGetText = vstDualGetText
    OnKeyDown = vstDualKeyDown
    OnMouseDown = vstDualMouseDown
    OnNewText = vstDualNewText
  end
  object btnOverwriteLeft: TButton
    Left = 460
    Height = 25
    Top = 8
    Width = 140
    Caption = '<< Overwrite to left'
    TabOrder = 3
    OnClick = btnOverwriteLeftClick
  end
  object btnLoadProject: TButton
    Left = 56
    Height = 25
    Top = 8
    Width = 104
    Caption = 'Load project...'
    TabOrder = 4
    OnClick = btnLoadProjectClick
  end
  object btnSaveProject: TButton
    Left = 168
    Height = 25
    Top = 8
    Width = 104
    Caption = 'Save project'
    TabOrder = 5
    OnClick = btnSaveProjectClick
  end
  object edtSearchL1: TEdit
    Left = 8
    Height = 23
    Top = 440
    Width = 280
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    TextHint = 'Search L1'
    OnChange = edtSearchL1Change
  end
  object edtSearchL2: TEdit
    Left = 304
    Height = 23
    Top = 440
    Width = 280
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    TextHint = 'Search L2'
    OnChange = edtSearchL2Change
  end
  object spdbtnExtraSave: TSpeedButton
    Left = 272
    Height = 25
    Top = 8
    Width = 16
    Glyph.Data = {
      EA000000424DEA0000000000000036000000280000000B000000050000000100
      180000000000B400000000000000000000000000000000000000FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFF39841AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFF
      FFFFFFFFFFFFFFFFFFFF39841A39841A39841AFFFFFFFFFFFFFFFFFFFFFFFF00
      0000FFFFFFFFFFFFFFFFFF39841A39841A39841A39841A39841AFFFFFFFFFFFF
      FFFFFF000000FFFFFFFFFFFF39841A39841A39841A39841A39841A39841A3984
      1AFFFFFFFFFFFF000000FFFFFF39841A39841A39841A39841A39841A39841A39
      841A39841A39841AFFFFFF000000
    }
    OnClick = spdbtnExtraSaveClick
  end
  object spdbtnExtraOverwriteLeft: TSpeedButton
    Left = 600
    Height = 25
    Top = 8
    Width = 16
    Glyph.Data = {
      EA000000424DEA0000000000000036000000280000000B000000050000000100
      180000000000B400000000000000000000000000000000000000FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFF39841AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFF
      FFFFFFFFFFFFFFFFFFFF39841A39841A39841AFFFFFFFFFFFFFFFFFFFFFFFF00
      0000FFFFFFFFFFFFFFFFFF39841A39841A39841A39841A39841AFFFFFFFFFFFF
      FFFFFF000000FFFFFFFFFFFF39841A39841A39841A39841A39841A39841A3984
      1AFFFFFFFFFFFF000000FFFFFF39841A39841A39841A39841A39841A39841A39
      841A39841A39841AFFFFFF000000
    }
    OnClick = spdbtnExtraOverwriteLeftClick
  end
  object btnAddLine: TButton
    Left = 304
    Height = 25
    Top = 8
    Width = 104
    Caption = 'Add line'
    TabOrder = 6
    OnClick = btnAddLineClick
  end
  object btnNewProject: TButton
    Left = 8
    Height = 25
    Top = 8
    Width = 40
    Caption = 'New'
    TabOrder = 7
    OnClick = btnNewProjectClick
  end
  object lblModified: TLabel
    Left = 1112
    Height = 15
    Top = 8
    Width = 50
    Anchors = [akTop, akRight]
    Caption = 'Modified'
    Font.Color = clMaroon
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
  object pnlProjectName: TPanel
    Left = 784
    Height = 25
    Top = 8
    Width = 306
    Anchors = [akTop, akLeft, akRight]
    ClientHeight = 25
    ClientWidth = 306
    TabOrder = 8
    object lblProjectName: TLabel
      Left = 1
      Height = 15
      Top = 1
      Width = 40
      Caption = 'Project'
      Font.Color = clGreen
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object btnOverwriteRight: TButton
    Left = 624
    Height = 25
    Top = 8
    Width = 140
    Caption = 'Overwrite to right >>'
    TabOrder = 9
    OnClick = btnOverwriteRightClick
  end
  object spdbtnExtraOverwriteRight: TSpeedButton
    Left = 764
    Height = 25
    Top = 8
    Width = 16
    Glyph.Data = {
      EA000000424DEA0000000000000036000000280000000B000000050000000100
      180000000000B400000000000000000000000000000000000000FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFF39841AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFF
      FFFFFFFFFFFFFFFFFFFF39841A39841A39841AFFFFFFFFFFFFFFFFFFFFFFFF00
      0000FFFFFFFFFFFFFFFFFF39841A39841A39841A39841A39841AFFFFFFFFFFFF
      FFFFFF000000FFFFFFFFFFFF39841A39841A39841A39841A39841A39841A3984
      1AFFFFFFFFFFFF000000FFFFFF39841A39841A39841A39841A39841A39841A39
      841A39841A39841AFFFFFF000000
    }
    OnClick = spdbtnExtraOverwriteRightClick
  end
  object pnlL: TPanel
    Left = 624
    Height = 392
    Top = 40
    Width = 540
    Anchors = [akTop, akRight, akBottom]
    BevelOuter = bvNone
    ClientHeight = 392
    ClientWidth = 540
    TabOrder = 10
    object pnlL1: TPanel
      Left = 0
      Height = 393
      Top = 0
      Width = 266
      Alignment = taLeftJustify
      Anchors = [akTop, akLeft, akBottom]
      Caption = 'L1 Raw'
      ClientHeight = 393
      ClientWidth = 266
      Color = 16312022
      ParentBackground = False
      ParentColor = False
      TabOrder = 0
      VerticalAlignment = taAlignTop
      object memL1: TMemo
        Left = 1
        Height = 368
        Top = 24
        Width = 264
        Anchors = [akTop, akLeft, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object pnlL2: TPanel
      Left = 274
      Height = 393
      Top = 0
      Width = 266
      Alignment = taLeftJustify
      Anchors = [akTop, akRight, akBottom]
      Caption = 'L2 Raw'
      ClientHeight = 393
      ClientWidth = 266
      Color = 16312022
      ParentBackground = False
      ParentColor = False
      TabOrder = 1
      VerticalAlignment = taAlignTop
      object memL2: TMemo
        Left = 1
        Height = 368
        Top = 24
        Width = 264
        Anchors = [akTop, akLeft, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object pnlSplitterVST: TPanel
    Cursor = crHSplit
    Left = 616
    Height = 392
    Top = 40
    Width = 9
    Anchors = [akTop, akLeft, akBottom]
    Color = 13496775
    ParentBackground = False
    ParentColor = False
    TabOrder = 11
    OnMouseDown = pnlSplitterVSTMouseDown
    OnMouseMove = pnlSplitterVSTMouseMove
    OnMouseUp = pnlSplitterVSTMouseUp
  end
  object OpenDialog1: TOpenDialog
    Left = 704
    Top = 64
  end
  object SaveDialog1: TSaveDialog
    Left = 792
    Top = 64
  end
  object pmExtraSave: TPopupMenu
    Left = 264
    Top = 64
    object MenuItem_SaveProjectAs: TMenuItem
      Caption = 'Save project as...'
      OnClick = MenuItem_SaveProjectAsClick
    end
  end
  object pmExtraOverwriteLeft: TPopupMenu
    Left = 584
    Top = 64
    object MenuItem_AppendToLeft: TMenuItem
      Caption = 'Append to left'
      OnClick = MenuItem_AppendToLeftClick
    end
  end
  object pmVST: TPopupMenu
    Left = 264
    Top = 131
    object MenuItem_CopyL1ToClipboard: TMenuItem
      Caption = 'Copy L1 to clipboard'
      OnClick = MenuItem_CopyL1ToClipboardClick
    end
    object MenuItem_CopyL2ToClipboard: TMenuItem
      Caption = 'Copy L2 to clipboard'
      OnClick = MenuItem_CopyL2ToClipboardClick
    end
    object Separator1: TMenuItem
      Caption = '-'
    end
    object MenuItem_RemoveSelectedLines: TMenuItem
      Caption = 'Remove selected lines...'
      OnClick = MenuItem_RemoveSelectedLinesClick
    end
  end
  object tmrSearch: TTimer
    Enabled = False
    Interval = 200
    OnTimer = tmrSearchTimer
    Left = 424
    Top = 384
  end
  object tmrEdit: TTimer
    Interval = 10
    OnTimer = tmrEditTimer
    Left = 512
    Top = 352
  end
  object pmExtraOverwriteRight: TPopupMenu
    Left = 584
    Top = 128
    object MenuItem_AppendToRight: TMenuItem
      Caption = 'Append to right'
      OnClick = MenuItem_AppendToRightClick
    end
  end
end
