unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uFasade, uDB, uLogger, ExtCtrls, Buttons, ComCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    tsEdit: TTabSheet;
    tsStat: TTabSheet;
    OpenDialog1: TOpenDialog;
    Timer1: TTimer;
    edProxyPort: TEdit;
    edProxyIP: TEdit;
    btLoadProxyList: TButton;
    Button4: TButton;
    Button3: TButton;
    Button2: TButton;
    Button1: TButton;
    edLogLevel: TEdit;
    btLoad: TButton;
    btCreateWorld: TButton;
    cbAutoRun: TCheckBox;
    btInit: TBitBtn;
    btSave: TBitBtn;
    edWorldID: TEdit;
    btRun: TButton;
    edPassword: TEdit;
    edLogin: TEdit;
    edGameUrl: TEdit;
    Label8: TLabel;
    Label7: TLabel;
    Label6: TLabel;
    Label5: TLabel;
    Label4: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    lbLog: TListBox;
    lvPlanets: TListView;
    lbStat: TLabel;
    btStartBot: TButton;
    procedure btRunClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btInitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btCreateWorldClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btLoadClick(Sender: TObject);
    procedure edLogLevelChange(Sender: TObject);
    procedure btLoadProxyListClick(Sender: TObject);
    procedure btStartBotClick(Sender: TObject);
  private
    { Private declarations }
    game: TMoonFasade;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btCreateWorldClick(Sender: TObject);
begin
  if game = nil then btInit.Click;

  game.CreateWorld;
end;

procedure TForm1.btInitClick(Sender: TObject);
begin
  game := TMoonFasade.Create;

  game.ServerURL := 'http://' + edGameUrl.Text + '/';
  game.ServerID := StrToIntDef(edWorldID.Text, 0);
  game.UserName := edLogin.Text;
  game.Password := edPassword.Text;

  if edProxyIP.Text <> '' then
    game.SetProxy(edProxyIP.Text, edProxyPort.Text);

  game.Init;
end;

procedure TForm1.btLoadClick(Sender: TObject);
var
  db: TMoonDB;
begin
  db := TMoonDB.GetInstance;
  if not db.Connected then db.Connect;
  if db.Connected then
  begin
    edGameUrl.Text := db.GetParam('GameURL', 'agame.varena.info');
    edWorldID.Text := db.GetParam('WorldID', '5');
    edLogin.Text := db.GetParam('Login', '');
    edPassword.Text := db.GetParam('Password', '');
    edLogLevel.Text := db.GetParam('LogLevel', '100');
    edProxyIP.Text := db.GetParam('ProxyIP', '');
    edProxyPort.Text := db.GetParam('ProxyPort', '');
  end;

end;

procedure TForm1.btLoadProxyListClick(Sender: TObject);
begin
  if game = nil then btInit.Click;

  if OpenDialog1.Execute then
    game.AddProxyToDB(OpenDialog1.FileName);
end;

procedure TForm1.btRunClick(Sender: TObject);
begin
  tsStat.Show;
  if game = nil then btInit.Click;

  game.Run;
end;

procedure TForm1.btSaveClick(Sender: TObject);
var
  db: TMoonDB;
begin
  db := TMoonDB.GetInstance;
  if not db.Connected then db.Connect;
  if not db.Connected then exit;

  db.SetParam('GameURL', edGameUrl.Text);
  db.SetParam('WorldID', edWorldID.Text);
  db.SetParam('Login', edLogin.Text);
  db.SetParam('Password', edPassword.Text);
  db.SetParam('LogLevel', edLogLevel.Text);
  db.SetParam('ProxyIP', edProxyIP.Text);
  db.SetParam('ProxyPort', edProxyPort.Text);
end;

procedure TForm1.btStartBotClick(Sender: TObject);
begin
  btRun.Click;
  cbAutoRun.Checked := true;
end;

procedure TForm1.edLogLevelChange(Sender: TObject);
begin
  SetLogLevel(StrToIntDef(edLogLevel.Text, 100));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  game := nil;
  edLogLevelChange(Self);
  btLoad.Click;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if cbAutoRun.Checked and (game <> nil) then btRun.Click;

  GetSLLog(TStringList(lbLog.Items));
  if game <> nil then
  begin
    game.StatPlanetList(lvPlanets);
    lbStat.Caption := game.StrStat;
  end;
end;

end.
