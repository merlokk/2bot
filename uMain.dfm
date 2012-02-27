object Form1: TForm1
  Left = 0
  Top = 0
  Caption = '2moons bot'
  ClientHeight = 936
  ClientWidth = 728
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 728
    Height = 936
    ActivePage = tsEdit
    Align = alClient
    TabOrder = 0
    object tsEdit: TTabSheet
      Caption = 'Edit'
      object Label8: TLabel
        Left = 3
        Top = 68
        Width = 58
        Height = 16
        Caption = 'proxy port'
      end
      object Label7: TLabel
        Left = 3
        Top = 38
        Width = 45
        Height = 16
        Caption = 'proxy ip'
      end
      object Label6: TLabel
        Left = 296
        Top = 322
        Width = 44
        Height = 16
        Caption = 'Editors:'
      end
      object Label5: TLabel
        Left = 3
        Top = 213
        Width = 47
        Height = 16
        Caption = 'log level'
      end
      object Label4: TLabel
        Left = 3
        Top = 108
        Width = 50
        Height = 16
        Caption = 'World ID'
      end
      object Label3: TLabel
        Left = 3
        Top = 170
        Width = 55
        Height = 16
        Caption = 'password'
      end
      object Label2: TLabel
        Left = 3
        Top = 140
        Width = 27
        Height = 16
        Caption = 'login'
      end
      object Label1: TLabel
        Left = 3
        Top = 8
        Width = 59
        Height = 16
        Caption = 'Game URL'
      end
      object edProxyPort: TEdit
        Left = 67
        Top = 65
        Width = 121
        Height = 24
        TabOrder = 0
      end
      object edProxyIP: TEdit
        Left = 67
        Top = 35
        Width = 121
        Height = 24
        TabOrder = 1
      end
      object btLoadProxyList: TButton
        Left = 118
        Top = 344
        Width = 104
        Height = 25
        Caption = 'load proxies'
        TabOrder = 2
        OnClick = btLoadProxyListClick
      end
      object Button4: TButton
        Left = 545
        Top = 344
        Width = 75
        Height = 25
        Caption = 'build types'
        TabOrder = 3
      end
      object Button3: TButton
        Left = 464
        Top = 344
        Width = 75
        Height = 25
        Caption = 'planets bld'
        TabOrder = 4
      end
      object Button2: TButton
        Left = 377
        Top = 344
        Width = 75
        Height = 25
        Caption = 'Research'
        TabOrder = 5
      end
      object Button1: TButton
        Left = 296
        Top = 344
        Width = 75
        Height = 25
        Caption = 'Strategies'
        TabOrder = 6
      end
      object edLogLevel: TEdit
        Left = 67
        Top = 210
        Width = 121
        Height = 24
        TabOrder = 7
        Text = '100'
        OnChange = edLogLevelChange
      end
      object btLoad: TButton
        Left = 8
        Top = 256
        Width = 104
        Height = 25
        Caption = 'Load'
        TabOrder = 8
        OnClick = btLoadClick
      end
      object btCreateWorld: TButton
        Left = 8
        Top = 344
        Width = 104
        Height = 25
        Caption = 'Create world'
        TabOrder = 9
        OnClick = btCreateWorldClick
      end
      object cbAutoRun: TCheckBox
        Left = 594
        Top = 105
        Width = 97
        Height = 17
        Caption = 'autorun'
        TabOrder = 10
      end
      object btInit: TBitBtn
        Left = 424
        Top = 97
        Width = 75
        Height = 25
        Caption = 'Init'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 11
        OnClick = btInitClick
      end
      object btSave: TBitBtn
        Left = 118
        Top = 256
        Width = 104
        Height = 25
        Caption = 'Save'
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 12
        OnClick = btSaveClick
      end
      object edWorldID: TEdit
        Left = 67
        Top = 105
        Width = 121
        Height = 24
        TabOrder = 13
        Text = '5'
      end
      object btRun: TButton
        Left = 505
        Top = 97
        Width = 75
        Height = 25
        Caption = 'Run'
        TabOrder = 14
        OnClick = btRunClick
      end
      object edPassword: TEdit
        Left = 67
        Top = 167
        Width = 121
        Height = 24
        PasswordChar = '*'
        TabOrder = 15
      end
      object edLogin: TEdit
        Left = 67
        Top = 137
        Width = 121
        Height = 24
        TabOrder = 16
      end
      object edGameUrl: TEdit
        Left = 67
        Top = 5
        Width = 401
        Height = 24
        TabOrder = 17
        Text = 'agame.varena.info'
      end
      object btStartBot: TButton
        Left = 208
        Top = 97
        Width = 203
        Height = 25
        Caption = 'Start BOT )))'
        TabOrder = 18
        OnClick = btStartBotClick
      end
    end
    object tsStat: TTabSheet
      Caption = 'Statistic'
      ImageIndex = 1
      DesignSize = (
        720
        905)
      object lbStat: TLabel
        Left = 8
        Top = 8
        Width = 709
        Height = 50
        AutoSize = False
        Caption = '                '
      end
      object lbLog: TListBox
        Left = 3
        Top = 529
        Width = 714
        Height = 373
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 0
      end
      object lvPlanets: TListView
        Left = 3
        Top = 64
        Width = 714
        Height = 459
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'id'
          end
          item
            Caption = 'name'
            Width = 100
          end
          item
            Caption = 'f fields'
          end
          item
            Caption = 'energy'
            Width = 70
          end
          item
            Caption = 'buildings'
            Width = 70
          end
          item
            Caption = 'ship builds'
            Width = 70
          end
          item
            Caption = 'MET/CRY/DEI'
            Width = 110
          end
          item
            Caption = 'plan'
            Width = 100
          end>
        DoubleBuffered = True
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        TabOrder = 1
        ViewStyle = vsReport
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'txt'
    FileName = '*.*'
    Filter = '*.*|all files'
    InitialDir = '.'
    Left = 472
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 544
  end
end
