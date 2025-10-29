object frTabs: TfrTabs
  Left = 0
  Height = 50
  Top = 0
  Width = 588
  ClientHeight = 50
  ClientWidth = 588
  Constraints.MinHeight = 20
  TabOrder = 0
  DesignLeft = 86
  DesignTop = 85
  object ECTabCtrl1: TECTabCtrl
    Left = 0
    Height = 25
    Top = 0
    Width = 584
    Anchors = [akTop, akLeft, akRight]
    ColorActiveTab = 12186269
    OnAdd = ECTabCtrl1Add
    OnChange = ECTabCtrl1Change
    OnMouseUp = ECTabCtrl1MouseUp
    Options = [etcoAutoDragTabs, etcoDontShrinkActiveTab, etcoDropDownMenu, etcoEnlargeTabsToFit, etcoNewTabActive]
    TabOrder = 0
    Tabs = <>
  end
  object tmrTabChange: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrTabChangeTimer
    Left = 272
  end
  object tmrSetTabCaption: TTimer
    Enabled = False
    Interval = 10
    Left = 392
    Top = 3
  end
end
