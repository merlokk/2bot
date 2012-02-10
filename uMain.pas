unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uFasade, uDB, uLogger, ExtCtrls, Buttons;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    edGameUrl: TEdit;
    Label2: TLabel;
    edLogin: TEdit;
    Label3: TLabel;
    edPassword: TEdit;
    btRun: TButton;
    Label4: TLabel;
    edWorldID: TEdit;
    lbLog: TListBox;
    Timer1: TTimer;
    btSave: TBitBtn;
    btInit: TBitBtn;
    cbAutoRun: TCheckBox;
    btCreateWorld: TButton;
    btLoad: TButton;
    edLogLevel: TEdit;
    Label5: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label6: TLabel;
    procedure btRunClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btInitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btCreateWorldClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btLoadClick(Sender: TObject);
    procedure edLogLevelChange(Sender: TObject);
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
  end;

end;

procedure TForm1.btRunClick(Sender: TObject);
begin
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
end;

end.
