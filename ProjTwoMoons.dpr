program ProjTwoMoons;

uses
  Forms,
  uMain in 'uMain.pas' {Form1},
  uFasade in 'uFasade.pas',
  uDefs in 'uDefs.pas',
  uServerConnect in 'uServerConnect.pas',
  uStrategy in 'uStrategy.pas',
  uLogger in 'uLogger.pas',
  uGameItems in 'uGameItems.pas',
  uHTMLParser in 'uHTMLParser.pas',
  superobject in 'JSON\superobject.pas',
  uDB in 'uDB.pas',
  uProxyCheck in 'uProxyCheck.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
