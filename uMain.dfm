object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 587
  ClientWidth = 695
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 59
    Height = 16
    Caption = 'Game URL'
  end
  object Label2: TLabel
    Left = 8
    Top = 70
    Width = 27
    Height = 16
    Caption = 'login'
  end
  object Label3: TLabel
    Left = 8
    Top = 100
    Width = 55
    Height = 16
    Caption = 'password'
  end
  object Label4: TLabel
    Left = 8
    Top = 38
    Width = 50
    Height = 16
    Caption = 'World ID'
  end
  object Label5: TLabel
    Left = 8
    Top = 170
    Width = 47
    Height = 16
    Caption = 'log level'
  end
  object Label6: TLabel
    Left = 504
    Top = 8
    Width = 44
    Height = 16
    Caption = 'Editors:'
  end
  object edGameUrl: TEdit
    Left = 72
    Top = 5
    Width = 401
    Height = 24
    TabOrder = 0
    Text = 'agame.varena.info'
  end
  object edLogin: TEdit
    Left = 72
    Top = 67
    Width = 121
    Height = 24
    TabOrder = 1
    Text = 'merlok'
  end
  object edPassword: TEdit
    Left = 72
    Top = 97
    Width = 121
    Height = 24
    PasswordChar = '*'
    TabOrder = 2
    Text = 'bossboss'
  end
  object btRun: TButton
    Left = 486
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 3
    OnClick = btRunClick
  end
  object edWorldID: TEdit
    Left = 72
    Top = 35
    Width = 121
    Height = 24
    TabOrder = 4
    Text = '5'
  end
  object lbLog: TListBox
    Left = 16
    Top = 197
    Width = 657
    Height = 380
    TabOrder = 5
  end
  object btSave: TBitBtn
    Left = 118
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Save'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 6
    OnClick = btSaveClick
  end
  object btInit: TBitBtn
    Left = 405
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Init'
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 7
    OnClick = btInitClick
  end
  object cbAutoRun: TCheckBox
    Left = 567
    Top = 140
    Width = 97
    Height = 17
    Caption = 'autorun'
    TabOrder = 8
  end
  object btCreateWorld: TButton
    Left = 249
    Top = 136
    Width = 104
    Height = 25
    Caption = 'Create world'
    TabOrder = 9
    OnClick = btCreateWorldClick
  end
  object btLoad: TButton
    Left = 37
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Load'
    TabOrder = 10
    OnClick = btLoadClick
  end
  object edLogLevel: TEdit
    Left = 72
    Top = 167
    Width = 121
    Height = 24
    TabOrder = 11
    Text = '100'
    OnChange = edLogLevelChange
  end
  object Button1: TButton
    Left = 504
    Top = 29
    Width = 75
    Height = 25
    Caption = 'Strategies'
    TabOrder = 12
  end
  object Button2: TButton
    Left = 585
    Top = 29
    Width = 75
    Height = 25
    Caption = 'Research'
    TabOrder = 13
  end
  object Button3: TButton
    Left = 504
    Top = 60
    Width = 75
    Height = 25
    Caption = 'planets bld'
    TabOrder = 14
  end
  object Button4: TButton
    Left = 585
    Top = 60
    Width = 75
    Height = 25
    Caption = 'build types'
    TabOrder = 15
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 640
    Top = 136
  end
end
