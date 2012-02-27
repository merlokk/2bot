unit uFasade;

interface
uses
  SysUtils, Classes, pFIBQuery, ComCtrls,
  uLogger, uServerConnect, uGameItems, uStrategy, uDB, uProxyCheck,
  uDefs;

type
 TMoonFasade = class
 private
   FServerURL: string;
   FServerID: integer;
   FUserName,
   FPassword: string;

   FProxy: TProxy;

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
   procedure AddProxyToDB(filename: string);

   procedure ClearProxy;
   procedure SetProxy(ip, port: string);

   procedure StatPlanetList(lv: TListView);
   function StrStat: string;

   property ServerURL: string read FServerURL write FServerURL;
   property ServerID: integer read FServerID write FServerID;
   property UserName: string read FUserName write FUserName;
   property Password: string read FPassword write FPassword;

   property Proxy: TProxy read FProxy;
 end;

implementation

{ TMoonFasade }

procedure TMoonFasade.AddProxyToDB(filename: string);
var
  pc: TProxyChecker;
  sl: TStringList;
  prx: TProxies;
begin
  try
    sl := TStringList.Create;
    sl.LoadFromFile(filename);

    pc := TProxyChecker.Create;
    prx := pc.CheckList(sl.Text);


    pc.Free;
    sl.Free;
  except
  end;
end;

procedure TMoonFasade.Clear;
begin
  FMoonServer := nil;
  FImperium := nil;
  FStExec := nil;
  FWorking := false;

  ClearProxy;
end;

procedure TMoonFasade.ClearProxy;
begin
  FProxy.Active := false;
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
 FMoonServer.SetProxy(FProxy);
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

procedure TMoonFasade.SetProxy(ip, port: string);
begin
  FProxy.Active := false;

  if (ip = '') or (StrToIntDef(port, 0) <= 0) then exit;

  FProxy.Active := true;
  FProxy.IP := ip;
  FProxy.Port := port;
end;

procedure TMoonFasade.StatPlanetList(lv: TListView);
var
  li: TListItem;
  i: Integer;
  pl: TPlanet;
begin
  lv.Items.Clear;
  if (FImperium = nil) or (not FImperium.Valid) then exit;

  lv.Items.BeginUpdate;
  try
    for i := 0 to FImperium.PlanetsCount - 1 do
    begin
      pl := FImperium.GetPlanetI(i);
      li := lv.Items.Add;
      li.Caption := IntToStr(pl.ID);
      if not pl.isMoon then
        li.SubItems.Add(pl.Name)
      else
        li.SubItems.Add('(m)' + pl.Name);
      li.SubItems.Add(IntToStr(pl.FreeFieldsCount));
      li.SubItems.Add(ResToStr(pl.FreeEnergy));
      li.SubItems.Add(pl.StrBuildsBuilding);
      li.SubItems.Add(pl.StrShipsBuilding);
      li.SubItems.Add(
        ResToStr(pl.CurRes.Metal) + '/' +
        ResToStr(pl.CurRes.Crystal) + '/' +
        ResToStr(pl.CurRes.Deiterium));
      if pl.BuildingPlan.EndPlaningDate > Now then
      begin
        li.SubItems.Add(pl.BuildingPlan.Item.Name + ' ' +
          DateTimeToStr(pl.BuildingPlan.EndPlaningDate));
      end
      else
        li.SubItems.Add(' ');
    end;
  finally
    lv.Items.EndUpdate;
  end;

end;

function TMoonFasade.StrStat: string;
begin
  if (FImperium = nil) then
  begin
    Result := 'imperium n/a';
    exit;
  end;
  if (not FImperium.Valid) then
  begin
    Result := 'imperium not valid';
    exit;
  end;

  Result := DateTimeToStr(FImperium.LastUpdate) + ': ' +
      ' dm=' + IntToStr(FImperium.DarkMatery) +
      ' pl=' + IntToStr(FImperium.PlanetsCount) +
      ' moon=' + IntToStr(FImperium.MoonsCount) +
      ' rsrch=' + IntToStr(FImperium.ResearchCount) +
      #10#13 +
      'researching=' + FImperium.StrResearching +
      #10#13 +
      'research plan=' + FImperium.StrResearcPlan;
end;

end.
