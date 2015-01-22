object Form1: TForm1
  Left = 192
  Top = 110
  Caption = 'TDelphiTwain simple example'
  ClientHeight = 388
  ClientWidth = 543
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ImageHolder: TImage
    Left = 0
    Top = 33
    Width = 543
    Height = 355
    Align = alClient
    Proportional = True
    Stretch = True
  end
  object Title: TPanel
    Left = 0
    Top = 0
    Width = 543
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Simple example using TDelphiTwain'
    Color = clAppWorkSpace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindow
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object GoAcquire: TButton
    Left = 32
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Acquire'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = GoAcquireClick
  end
end
