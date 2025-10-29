object frmAllDTRsMain: TfrmAllDTRsMain
  Left = 373
  Height = 500
  Top = 185
  Width = 1170
  Caption = 'All DTRs'
  ClientHeight = 500
  ClientWidth = 1170
  Constraints.MinWidth = 1170
  LCLVersion = '8.4'
  OnClose = FormClose
  OnCreate = FormCreate
  object edtSearchL1: TEdit
    Left = 598
    Height = 23
    Top = 0
    Width = 280
    Anchors = [akTop, akRight]
    TabOrder = 0
    TextHint = 'Search All L1'
    OnChange = edtSearchL1Change
  end
  object edtSearchL2: TEdit
    Left = 886
    Height = 23
    Top = 0
    Width = 280
    Anchors = [akTop, akRight]
    TabOrder = 1
    TextHint = 'Search All L2'
    OnChange = edtSearchL2Change
  end
  object tmrSearch: TTimer
    Enabled = False
    Interval = 500
    OnTimer = tmrSearchTimer
    Left = 552
  end
  object tmrStartup: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrStartupTimer
    Left = 472
  end
end
