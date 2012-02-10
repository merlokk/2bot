unit uFasade;

interface
uses
  SysUtils, pFIBQuery,
  uLogger, uServerConnect, uGameItems, uStrategy, uDB;

type
 TMoonFasade = class
 private
   FServerURL: string;
   FServerID: integer;
   FUserName,
   FPassword: string;

   FWorking: boolean;

   FMoonServer: TMoonServerConnect;
   FDB: TMoonDB;
   FImperium: TImperium;
   FStExec: TStrategyExecutor;
 public
   constructor Create;
   destructor Destroy; override;

   procedure Clear;
   procedure Init;
   procedure Run;

   procedure CreateWorld;

   property ServerURL: string read FServerURL write FServerURL;
   property ServerID: integer read FServerID write FServerID;
   property UserName: string read FUserName write FUserName;
   property Password: string read FPassword write FPassword;
 end;

implementation

{ TMoonFasade }

procedure TMoonFasade.Clear;
begin
  FMoonServer := nil;
  FImperium := nil;
  FStExec := nil;
  FWorking := false;
end;

constructor TMoonFasade.Create;
begin
  inherited;

  FDB := TMoonDB.GetInstance;
end;

procedure TMoonFasade.CreateWorld;
var
 i,
 j: integer;
begin
  FDB.ClearUniverse;
  AddLog('universe cleared...');

  for i := 1 to 9 do
    for j := 1 to 499 do
      FDB.AddSystem(i, j);
  FDB.Commit;
  AddLog('universe commited.');
end;

destructor TMoonFasade.Destroy;
begin
  inherited;

  FStExec.Free;
  FMoonServer.Free;
  FImperium.Free;
end;

procedure TMoonFasade.Init;
var
  qry: TpFIBQuery;
  id: integer;
begin
  AddLog('starting...', 0);

 FDB := TMoonDB.GetInstance;
 FDB.Connect;

 FMoonServer := TMoonServerConnect.Create;
 FMoonServer.Login(FServerURL, FServerID, FUserName, FPassword);

 FImperium := TImperium.Create;
 FStExec := TStrategyExecutor.Create(FImperium, FMoonServer);

// FMoonServer.UpdateImperium(FImperium);
// FMoonServer.BuildBuilding(FImperium, 638, 'Рудник кристалла', 0, false);

 qry := FDB.GetStrategyList;
 while not qry.Eof do
 begin
   id := -1;
   try
     id := qry.FieldByName('ID').AsInteger;
     FStExec.AddStrategy(
       id,
       qry.FieldByName('TYPE_ID').AsInteger,
       qry.FieldByName('PARAMS').AsString);

     if qry.FieldByName('ACTIVE').AsInteger <> 0 then
       FStExec.ActivateStrategy(id);

     AddLog('init strategy(' + IntToStr(id) + '/' +
       qry.FieldByName('TYPE_ID').AsString + '): "' +
       qry.FieldByName('NAME').AsString + '" ac=' +
       qry.FieldByName('ACTIVE').AsString, 5);
   except
     FStExec.DeactivateStrategy(id);
   end;

   qry.Next;
 end;
 qry.Close;

  FStExec.ApplyMacroStrategy(-1);

  AddLog('init ok', 0);
end;

procedure TMoonFasade.Run;
begin
 if FWorking then exit;
 FWorking := true;
 try
   if FStExec = nil then Init;

   FStExec.Execute;
 finally
   FWorking := false;
 end;
end;

end.
