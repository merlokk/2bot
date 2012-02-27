unit uStrategy;

interface
uses
  Classes, SysUtils, StrUtils, Math,
  pFIBQuery, superobject, uGameItems, uDefs, uServerConnect, uDB,
  uLogger;

type

  TStrategy = class
  private
    FID,
    FGroupID,
    FPriority: integer;
    FActive: boolean;
    FImperium: TImperium;
    FServer: TMoonServerConnect;
    FDB: TMoonDB;

    FExecOnce: boolean;
    FLastExecute,
    FNextExecute: TDateTime;

    FromHour,
    ToHour,

    FInterval,
    FIntDeviation,

    FFromPlanetID: integer;

    procedure PlaninigNextExecution;
  public
    constructor Create(imp: TImperium; srv: TMoonServerConnect; db: TMoonDB); virtual;

    procedure Clear; virtual;
    function LoadConfig(cfg: string): ISuperObject; virtual;
    function Execute: boolean;
    function IntExecute: boolean; virtual;

    property Active: boolean read FActive write FActive;
    property Priority: integer read FPriority write FPriority;
    property ID: integer read FID write FID;
    property GroupID: integer read FGroupID;
  end;

  TStInit = class(TStrategy)
  private
  public
    constructor Create(imp: TImperium; srv: TMoonServerConnect; db: TMoonDB); override;

    function LoadConfig(cfg: string): ISuperObject; override;
    procedure Clear; override;
    function IntExecute: boolean; override;
  end;

  TStExpedition = class(TStrategy)
  protected
    FWaitAllShipsAvail: boolean;
    FDelMsgEvery,
    FDelMsgCount: integer;
    FFleet: TFleetOrderArr;
  public
    constructor Create(imp: TImperium; srv: TMoonServerConnect; db: TMoonDB); override;

    function LoadConfig(cfg: string): ISuperObject; override;
    procedure Clear; override;
    function IntExecute: boolean; override;
  end;

  TStImperiumUpdate = class(TStrategy)
  public
    constructor Create(imp: TImperium; srv: TMoonServerConnect; db: TMoonDB); override;

    function LoadConfig(cfg: string): ISuperObject; override;
    procedure Clear; override;
    function IntExecute: boolean; override;
  end;

  TStMapUpdate = class(TStrategy)
  private
    FOnePass: integer;
  public
    constructor Create(imp: TImperium; srv: TMoonServerConnect; db: TMoonDB); override;

    function LoadConfig(cfg: string): ISuperObject; override;
    procedure Clear; override;
    function IntExecute: boolean; override;
  end;

  TStFleetBuild = class(TStrategy)
  private
    FWaitForRes,
    FLockRes: boolean;
    FFleet: TFleetOrderArr;

    function intBuild(PlanetID: integer): boolean;
  public
    constructor Create(imp: TImperium; srv: TMoonServerConnect; db: TMoonDB); override;

    function LoadConfig(cfg: string): ISuperObject; override;
    procedure Clear; override;
    function IntExecute: boolean; override;
  end;

  TStColonize = class(TStrategy)
  private
    FDelMsgEvery,
    FDelMsgCount: integer;
    FWaitAllShipsAvail: boolean;
    FFromGalaxy,
    FToGalaxy,
    FFromSystem,
    FToSystem,
    FFromPlanet,
    FToPlanet,
    FWorkPlanetCount,
    FMaxPlanetFields: integer;

    FFleet: TFleetOrderArr;

    FCurrCoords: TGameCoords;
    function CalcNextCoords: boolean;
  public
    constructor Create(imp: TImperium; srv: TMoonServerConnect; db: TMoonDB); override;

    function LoadConfig(cfg: string): ISuperObject; override;
    procedure Clear; override;
    function IntExecute: boolean; override;
  end;

  TStPlanetBuild = class(TStrategy)
  private
    FPlaningLock: integer;
    FLockRes: boolean;
    FBuildType: integer;

    function IntBuildPlanet(PlanetID: integer): boolean;
    procedure CalcBuild(pl: TPlanet; btree: TGameItems; var indx: Integer);
    procedure CalcBuildPlan(pl: TPlanet; btree: TGameItems; var indx: Integer);
  public
    constructor Create(imp: TImperium; srv: TMoonServerConnect; db: TMoonDB); override;

    function LoadConfig(cfg: string): ISuperObject; override;
    procedure Clear; override;
    function IntExecute: boolean; override;
  end;

  TStResearch = class(TStrategy)
  private
    FPlaningLock: integer;
    FLockRes: boolean;
    FMaxLength: integer;
    procedure CalcResearch(rtree: TGameItems; pl: TPlanet; var indx: Integer);
    procedure CalcResearchPlan(rtree: TGameItems; pl: TPlanet; var indx: Integer);
  public
    constructor Create(imp: TImperium; srv: TMoonServerConnect; db: TMoonDB); override;

    function LoadConfig(cfg: string): ISuperObject; override;
    procedure Clear; override;
    function IntExecute: boolean; override;
  end;

  TStFleetMove = class(TStrategy)
  private
    FWaitAllShipsAvail: boolean;
    FFleetOrder: TFleetOrder;
    FCoords: TGameCoords;
    FSpeed: integer;
    FTimeThere: extended;
    FFleet: TFleetOrderArr;
  public
    constructor Create(imp: TImperium; srv: TMoonServerConnect; db: TMoonDB); override;

    function LoadConfig(cfg: string): ISuperObject; override;
    procedure Clear; override;
    function IntExecute: boolean; override;
  end;

  TStSatelitesBuild = class(TStrategy)
  private
    FLockRes: boolean;
    FIncCount: integer;
  public
    constructor Create(imp: TImperium; srv: TMoonServerConnect; db: TMoonDB); override;

    function LoadConfig(cfg: string): ISuperObject; override;
    procedure Clear; override;
    function IntExecute: boolean; override;
  end;

  TStMineDarkMatery = class(TStrategy)
  private
    FMinersCount,
    FExpeditionsCount: integer;
    FMaxDarkMatery: int64;
  public
    constructor Create(imp: TImperium; srv: TMoonServerConnect; db: TMoonDB); override;

    function LoadConfig(cfg: string): ISuperObject; override;
    procedure Clear; override;
    function IntExecute: boolean; override;
  end;

  TStrategyExecutor = class
  private
    FImperium: TImperium;
    FServer: TMoonServerConnect;
    FStrat: array of TStrategy;

    function GetStrategy(ID: integer): TStrategy;
    function GetStrategyI(ID: integer): integer;
  public
    constructor Create(imp: TImperium; srv: TMoonServerConnect);

    procedure ApplyMacroStrategy(MacroStID: integer);

    procedure AddStrategy(ID, TypeID: integer; Params: string);
    procedure ActivateStrategy(ID: integer);
    procedure DeactivateStrategy(ID: integer);
    procedure DelStrategy(ID: integer);
    procedure UpdateStrategy(ID: integer; Params: string);

    procedure Execute;
  end;

implementation

{ TStrategy }

procedure TStrategy.Clear;
begin
  FPriority := -1;
  FActive := false;
  FImperium := nil;
  FExecOnce := false;
  FLastExecute := 0;
  FNextExecute := 0;
  FromHour := 0;
  ToHour := 0;
  FInterval := 0;
  FIntDeviation := 0;
  FFromPlanetID := 0;
end;

constructor TStrategy.Create(imp: TImperium; srv: TMoonServerConnect; db: TMoonDB);
begin
  inherited Create;

  FGroupID := 0; // тип стратегии
  Clear;
  FImperium := imp;
  FServer := srv;
  FDB := db;
end;

function TStrategy.Execute: boolean;
begin
  Result := true; // execute next strategy after this

  if not FActive then exit;
  if (FLastExecute <> 0) and FExecOnce then exit;
  if (FNextExecute <> 0) and (Now < FNextExecute) then exit;
  if (FromHour <> 0) and
     (not TimeBetween(Now, FromHour, ToHour)) then exit;

  IntExecute;

  PlaninigNextExecution;
end;

function TStrategy.IntExecute: boolean;
begin
  Result := true;

end;

function TStrategy.LoadConfig(cfg: string): ISuperObject;
var
  obj: ISuperObject;
begin
  Result := nil;

  FActive := false;
  FPriority := -1;

  try
    obj := TSuperObject.ParseString(PWideChar(cfg), false);
    if obj = nil then exit;
    Result := obj;

    FExecOnce := SOAsBooleanDef(obj, 'ExecOnce', false);
    FromHour := SOAsIntegerDef(obj, 'ExecFromHour', 0);
    ToHour := SOAsIntegerDef(obj, 'ExecToHour', 0);
    FInterval := SOAsIntegerDef(obj, 'Interval', 0);
    FIntDeviation := SOAsIntegerDef(obj, 'IntDeviation', 0);
    FFromPlanetID := SOAsIntegerDef(obj, 'FromPlanetID', 0);
  except
  end;
end;

procedure TStrategy.PlaninigNextExecution;
var
 sec: integer;
begin
  try
    FLastExecute := Now;
    sec := FInterval + (FIntDeviation - Random(FIntDeviation * 2));
    if sec < 4 then sec := 60;
    FNextExecute := FLastExecute + 1 / (24 * 60 * 60) * sec;
  except
  end;
end;

{ TStrategyExecutor }

procedure TStrategyExecutor.ActivateStrategy(ID: integer);
var
  strg: TStrategy;
begin
  strg := GetStrategy(ID);
  if strg = nil then exit;
  strg.Active := true;
end;

procedure TStrategyExecutor.AddStrategy(ID, TypeID: integer; Params: string);
var
  strg: TStrategy;
begin
  strg := nil;
  case TypeID of
    1: strg := TStExpedition.Create(FImperium, FServer, TMoonDB.GetInstance);
    2: strg := TStImperiumUpdate.Create(FImperium, FServer, TMoonDB.GetInstance);
    3: strg := TStMapUpdate.Create(FImperium, FServer, TMoonDB.GetInstance);
    4: strg := TStFleetBuild.Create(FImperium, FServer, TMoonDB.GetInstance);
    5: strg := TStColonize.Create(FImperium, FServer, TMoonDB.GetInstance);
    6: strg := TStPlanetBuild.Create(FImperium, FServer, TMoonDB.GetInstance);
    7: strg := TStResearch.Create(FImperium, FServer, TMoonDB.GetInstance);
    8: strg := TStFleetMove.Create(FImperium, FServer, TMoonDB.GetInstance);
    9: strg := TStSatelitesBuild.Create(FImperium, FServer, TMoonDB.GetInstance);
   10: strg := TStMineDarkMatery.Create(FImperium, FServer, TMoonDB.GetInstance);

  100: strg := TStInit.Create(FImperium, FServer, TMoonDB.GetInstance);
  end;

  if strg = nil then exit;

  strg.ID := ID;
  strg.LoadConfig(Params);
  SetLength(FStrat, length(FStrat) + 1);
  FStrat[length(FStrat) - 1] := strg;
end;

procedure TStrategyExecutor.ApplyMacroStrategy(MacroStID: integer);
var
  i: Integer;
begin
  if MacroStID = -1 then
    for i := 0 to length(FStrat) -1 do
      FStrat[i].Priority := i;

end;

constructor TStrategyExecutor.Create(imp: TImperium; srv: TMoonServerConnect);
begin
  inherited Create;

  SetLength(FStrat, 0);
  FImperium := imp;
  FServer := srv;
end;

procedure TStrategyExecutor.DeactivateStrategy(ID: integer);
var
  strg: TStrategy;
begin
  strg := GetStrategy(ID);
  if strg = nil then exit;
  strg.Active := false;
end;

procedure TStrategyExecutor.DelStrategy(ID: integer);
begin

end;

procedure TStrategyExecutor.Execute;
var
 i: integer;
 Prio: integer;
 nPrio,
 nIndx: integer;
begin
  Prio := -1; // start strategy priority

  while True do
  try
    nPrio := -1;
    nIndx := -1;
    for i := 0 to length(FStrat) - 1 do
      if (FStrat[i].FPriority > Prio) and
         ((nPrio < 0) or (FStrat[i].FPriority < nPrio))
      then
      begin
        nPrio := FStrat[i].FPriority;
        nIndx := i;
      end;

      if nIndx < 0 then break;
      Prio := nPrio;

      if not FStrat[nIndx].Execute then exit;
  except
  end;
end;

function TStrategyExecutor.GetStrategy(ID: integer): TStrategy;
var
 indx: integer;
begin
  Result := nil;

  indx := GetStrategyI(ID);
  if indx < 0 then exit;
  Result := FStrat[indx];
end;

function TStrategyExecutor.GetStrategyI(ID: integer): integer;
var
 i: integer;
begin
  Result := -1;

  for i := 0 to length(FStrat) - 1 do
    if FStrat[i].ID = ID then
    begin
      Result := i;
      break;
    end;
end;

procedure TStrategyExecutor.UpdateStrategy(ID: integer; Params: string);
var
  strg: TStrategy;
begin
  strg := GetStrategy(ID);
  if strg = nil then exit;
  strg.LoadConfig(Params);
end;

{ TStExpedition }

procedure TStExpedition.Clear;
begin
  inherited;

  FWaitAllShipsAvail := true;
  FDelMsgEvery := 0;
  FDelMsgCount := 0;
end;

constructor TStExpedition.Create(imp: TImperium; srv: TMoonServerConnect; db: TMoonDB);
begin
  inherited;

  FGroupID := 1; // expedition
end;

function TStExpedition.IntExecute: boolean;
var
 c: TGameCoords;
 pl: TPlanet;
 i: integer;
 ships: TFleetOrderArr;
begin
  Result := true;

  pl := FImperium.GetPlanet(FFromPlanetID);
  if pl = nil then exit;

  if FWaitAllShipsAvail then
  begin
    for i := 0 to length(FFleet) - 1 do
      if FFleet[i].Value > pl.GetShipsCount(FFleet[i].Name, true, false)
      then exit;
  end;

  c.Clear;
  c.FromStr('1:1:16');
  c.PlType := ptPlanet;
  c.Galaxy := Random(8) + 1;
  c.System := Random(499) + 1;

  ships := Copy(FFleet, 0, length(FFleet));
  FServer.FleetMove(FImperium, FFromPlanetID, c, ships, foExpedition, 10, 1);

  if FDelMsgEvery > 0 then
  begin
    if FDelMsgCount > FDelMsgEvery then
    begin
      FServer.DeleteMessages(integer(foExpedition));
      FDelMsgCount := 0;
    end;
    FDelMsgCount := FDelMsgCount + 1;
  end;
end;

function TStExpedition.LoadConfig(cfg: string): ISuperObject;
begin
  Result := inherited LoadConfig(cfg);

  FWaitAllShipsAvail := SOAsBooleanDef(Result, 'WaitAllShipsAvail', true);
  FDelMsgEvery := SOAsIntegerDef(Result, 'DelMsgEvery', 0);

  FFleet := SOAsShips(Result, 'ships');
end;

{ TStImperiumUpdate }

procedure TStImperiumUpdate.Clear;
begin
  inherited;

end;

constructor TStImperiumUpdate.Create(imp: TImperium; srv: TMoonServerConnect; db: TMoonDB);
begin
  inherited;

  FGroupID := 2; // Imperium Update
end;

function TStImperiumUpdate.IntExecute: boolean;
begin
  Result := true;

  // не чаще раза в минуту
  if FImperium.LastUpdate < (Now - 1/(24*60)) then
    Result := FServer.UpdateImperium(FImperium);
end;

function TStImperiumUpdate.LoadConfig(cfg: string): ISuperObject;
begin
  Result := inherited LoadConfig(cfg);

end;

{ TStMapUpdate }

procedure TStMapUpdate.Clear;
begin
  inherited;
  FOnePass := 1;
end;

constructor TStMapUpdate.Create(imp: TImperium; srv: TMoonServerConnect; db: TMoonDB);
begin
  inherited;

  FGroupID := 3; // Map Update
end;

function TStMapUpdate.IntExecute: boolean;
var
 pls: TPlanetSystem;
 usr: TUserList;
 qry: TpFIBQuery;
 res: boolean;
 cnt: integer;
begin
 Result := true;
 qry := FDB.GetSystemToScan(FOnePass);
 if qry = nil then exit;

 try
   cnt := 0;
   while not qry.Eof do
   begin
     res := FServer.UpdateGalaxyPlanets(
       qry.FieldByName('GALAXY').AsInteger,
       qry.FieldByName('SYSTEM').AsInteger,
       gmvNone,
       pls,
       usr);

     if res then
     begin
       if length(usr) > 0 then
         FServer.UpdateUsersInfo(usr);

       FDB.UpdateUsers(usr);
       FDB.UpdatePlanetSystem(pls);
       cnt := cnt + 1;
     end;

     qry.Next;
   end;
   qry.Close;

   FDB.Commit;
   Result := false;
   AddLog('update galaxy. systems=' + IntToStr(cnt));
 except
 end;
end;

function TStMapUpdate.LoadConfig(cfg: string): ISuperObject;
begin
  Result := inherited LoadConfig(cfg);
  FOnePass := SOAsIntegerDef(Result, 'OnePassSystemsCount', 1);
end;

{ TStShipBuild }

procedure TStFleetBuild.Clear;
begin
  inherited;

  FWaitForRes := true;
  FLockRes := false;
  SetLength(FFleet, 0);
end;

constructor TStFleetBuild.Create(imp: TImperium; srv: TMoonServerConnect;
  db: TMoonDB);
begin
  inherited;

  FGroupID := 4; // Ship build
end;

function TStFleetBuild.IntExecute: boolean;
var
  i: integer;
begin
  try
    if FFromPlanetID <> 0 then
    begin
      Result := intBuild(FFromPlanetID);
      exit;
    end;

    Result := true;
    for i := 0 to FImperium.PlanetsCount - 1 do
      Result := Result or intBuild(FImperium.GetPlanetI(i).ID);

  except
    Result := false;
  end;
end;

function TStFleetBuild.intBuild(PlanetID: integer): boolean;
var
  res: Boolean;
  ships: TFleetOrderArr;
begin
  Result := true;
  res := FImperium.PlanetHaveResources(
    FFromPlanetID,
    TCalc.ShipsBuildCost(FFleet));

  // we dont have needed resources
  if (not res) and (FWaitForRes) then exit;

  ships := Copy(FFleet, 0, length(FFleet));
  Result :=
    FServer.BuildFleet(FImperium, PlanetID, ships, FWaitForRes, true);
end;

function TStFleetBuild.LoadConfig(cfg: string): ISuperObject;
begin
  Result := inherited LoadConfig(cfg);

  FWaitForRes := SOAsBooleanDef(Result, 'WaitForRes', true);
  FLockRes := SOAsBooleanDef(Result, 'LockRes', false);
  FFleet := SOAsShips(Result, 'ships');
end;

{ TStColonize }

function TStColonize.CalcNextCoords: boolean;
var
  i: Integer;
  pls: TPlanetSystem;
  usr: TUserList;
  LastCoords: TGameCoords;
  firststep: boolean;
begin
 Result := false;
 i := 0;
 LastCoords := FCurrCoords;
 LastCoords.Inc(FFromGalaxy, FToGalaxy, FFromSystem, FToSystem,
   FFromPlanet, FToPlanet, 1);

 firststep := true;
 while true do
 begin
   FCurrCoords.Inc(FFromGalaxy, FToGalaxy, FFromSystem, FToSystem,
     FFromPlanet, FToPlanet, 1);
   if LastCoords.Equal(FCurrCoords) and not firststep then break;
   if FImperium.GetPlanetC(FCurrCoords) <> nil then continue;
   firststep := false;
   // если тут была планета то скорее всего она там и есть....
   if not FDB.GetPlanet(FCurrCoords).isEmpty then continue;

   // проверка на то, что планета не по€вилась
   FServer.UpdateGalaxyPlanets(
     FCurrCoords.Galaxy,
     FCurrCoords.System,
     gmvNone, pls, usr);
   FDB.UpdatePlanetSystem(pls);
   FDB.Commit;
   i := i + 1;
   if i > 100 then break;

   if FDB.GetPlanet(FCurrCoords).isEmpty then
   begin
     Result := true;
     exit;
   end;
 end;
end;

procedure TStColonize.Clear;
begin
  inherited;

  FDelMsgEvery := 10;
  FDelMsgCount := 0;
  FWaitAllShipsAvail := false;
  FFromGalaxy := 0;
  FToGalaxy := 0;
  FFromSystem := 0;
  FToSystem := 0;
  FFromPlanet := 0;
  FToPlanet := 0;
  FWorkPlanetCount := 0;
  FMaxPlanetFields := 300;
  SetLength(FFleet, 0);

  FCurrCoords.Clear;
  FCurrCoords.PlType := ptPlanet;
end;

constructor TStColonize.Create(imp: TImperium; srv: TMoonServerConnect;
  db: TMoonDB);
begin
  inherited;

  FGroupID := 5; // Colonize
end;

function TStColonize.IntExecute: boolean;
var
  MaxPlanetsCount: integer;
  ExpCnt: integer;
  i: Integer;
  pl: TPlanet;
  epl: TEnemyPlanet;
  MinFields,
  MinIndx: integer;
  ships: TFleetOrderArr;
begin
  Result := true;

  pl := FImperium.GetPlanet(FFromPlanetID);
  if pl = nil then exit;

  if FWaitAllShipsAvail then
  begin
    for i := 0 to length(FFleet) - 1 do
      if FFleet[i].Value > pl.GetShipsCount(FFleet[i].Name, true, false)
      then exit;
  end;

  MaxPlanetsCount := FImperium.MaxPlanetsCount;

  // delete planets
  ExpCnt := FWorkPlanetCount - (MaxPlanetsCount - FImperium.PlanetsCount(false));
  if ExpCnt < 0 then ExpCnt := 0;
  if ExpCnt > FWorkPlanetCount then ExpCnt := FWorkPlanetCount;

  while ExpCnt > 0 do
  begin
    MinIndx := -1;
    MinFields := 1000;
    for i := 0 to FImperium.PlanetsCount - 1 do
    begin
      pl := FImperium.GetPlanetI(i);
      if (pl = nil) or (pl.Name <> 'ѕланета') or
         (pl.MaxFields >= FMaxPlanetFields) or
         (pl.isMoon) then continue;

      if pl.MaxFields < MinFields then
      begin
        MinIndx := i;
        MinFields := pl.MaxFields;
      end;
    end;

    if MinIndx >= 0 then
    begin
      pl := FImperium.GetPlanetI(MinIndx);
      if pl <> nil then
      begin
        if FServer.DeletePlanet(pl.ID) then
        begin
          epl.Clear;
          epl.Coords := pl.Coords;
          epl.Name := 'deleted my planet';
          FDB.AddPlanet(epl);
        end;

        pl.Name := 'deleted';
        ExpCnt := ExpCnt - 1;

        sleep(1000);
      end;
    end
    else
      break;
  end;

  sleep(1000);
  FServer.UpdateImperium(FImperium);
  sleep(1000);

  // normalize expedition count
  if FImperium.PlanetsCount(false) >= MaxPlanetsCount then exit;
  ExpCnt := MaxPlanetsCount - FImperium.PlanetsCount(false);
  if ExpCnt > FWorkPlanetCount then ExpCnt := FWorkPlanetCount;

  // launch expedition
  if FCurrCoords.Galaxy = 0 then
  begin
    FCurrCoords.Galaxy := FFromGalaxy;
    FCurrCoords.System := FFromSystem;
    FCurrCoords.Planet := FFromPlanet - 1;
  end;

  for i := 1 to ExpCnt do
  begin
    if not CalcNextCoords then break;
    ships := Copy(FFleet, 0, length(FFleet));
    FServer.FleetMove(FImperium, FFromPlanetID, FCurrCoords, ships, foColonize, 10, 1);
    sleep(2000);
  end;

  if FDelMsgEvery > 0 then
  begin
    if FDelMsgCount > FDelMsgEvery then
    begin
      FServer.DeleteMessages(4);
      FDelMsgCount := 0;
    end;
    FDelMsgCount := FDelMsgCount + 1;
  end;
end;

function TStColonize.LoadConfig(cfg: string): ISuperObject;
begin
  Result := inherited LoadConfig(cfg);

  FDelMsgEvery := SOAsIntegerDef(Result, 'DelMsgEvery', 0);
  FWaitAllShipsAvail := SOAsBooleanDef(Result, 'WaitAllShipsAvail', true);
  FFromGalaxy := SOAsIntegerDef(Result, 'FromGalaxy', 0);
  FToGalaxy := SOAsIntegerDef(Result, 'ToGalaxy', 0);
  FFromSystem := SOAsIntegerDef(Result, 'FromSystem', 0);
  FToSystem := SOAsIntegerDef(Result, 'ToSystem', 0);
  FFromPlanet := SOAsIntegerDef(Result, 'FromPlanet', 0);
  FToPlanet := SOAsIntegerDef(Result, 'ToPlanet', 0);
  FWorkPlanetCount := SOAsIntegerDef(Result, 'WorkPlanetCount', 0);
  FMaxPlanetFields := SOAsIntegerDef(Result, 'MaxPlanetFields', 300);
  FFleet := SOAsShips(Result, 'ships');
end;

{ TStSatelitesBuild }

procedure TStSatelitesBuild.Clear;
begin
  inherited;

  FLockRes := false;
  FIncCount := 0;
end;

constructor TStSatelitesBuild.Create(imp: TImperium; srv: TMoonServerConnect;
  db: TMoonDB);
begin
  inherited;

  FGroupID := 9;
end;

function TStSatelitesBuild.IntExecute: boolean;
var
 res: boolean;
 Fleet: TFleetOrderArr;
 bg,
 en: integer;
 pl: TPlanet;
 i: Integer;
begin
  Result := true;
  if FIncCount <= 0 then exit;
  try
    if FFromPlanetID <> 0 then
    begin
      bg := FImperium.GetPlanetIndx(FFromPlanetID);
      en := bg;
      if bg < 0 then exit;
    end
    else
    begin
      bg := 0;
      en := FImperium.PlanetsCount - 1;
    end;

    Result := true;
    for i := bg to en do
    begin
      SetLength(Fleet, 1);
      Fleet[0].Name := '—олнечный спутник';
      Fleet[0].Value := FIncCount;

      pl := FImperium.GetPlanetI(i);
      if (pl = nil) or (pl.FreeEnergy >= 0) or (pl.ShipsBuilding)
      then continue;

      res := FImperium.PlanetHaveResources(
        pl.ID,
        TCalc.ShipsBuildCost(Fleet));

      // we dont have needed resources
      if (not res) then continue;

      Fleet[0].Value :=
        ( (pl.GetShipsCount(Fleet[0].Name, true, true) +
          Fleet[0].Value)
         div Fleet[0].Value) * Fleet[0].Value;

      res := FServer.BuildFleet(
          FImperium, pl.ID, Fleet, false, true);

      Result := Result and res;
    end;
  except
    Result := false;
  end;
end;

function TStSatelitesBuild.LoadConfig(cfg: string): ISuperObject;
begin
  Result := inherited LoadConfig(cfg);

  FLockRes := SOAsBooleanDef(Result, 'LockRes', false);
  FIncCount := SOAsIntegerDef(Result, 'IncCount', 0);
end;

{ TStMineDarkMatery }

procedure TStMineDarkMatery.Clear;
begin
  inherited;

  FMinersCount := 1;
  FExpeditionsCount := 1;
  FMaxDarkMatery := 0;
end;

constructor TStMineDarkMatery.Create(imp: TImperium; srv: TMoonServerConnect;
  db: TMoonDB);
begin
  inherited;

  FGroupID := 10;
end;

function TStMineDarkMatery.IntExecute: boolean;
var
 res: boolean;
 Fleet: TFleetOrderArr;
 bg,
 en: integer;
 pl: TPlanet;
 cnt,
 i: Integer;
 coords: TGameCoords;
begin
  Result := true;
  if (FMaxDarkMatery <> 0) and
     (FImperium.DarkMatery > FMaxDarkMatery) then exit;

  try
    if FFromPlanetID <> 0 then
    begin
      bg := FImperium.GetPlanetIndx(FFromPlanetID);
      en := bg;
      if bg < 0 then exit;
    end
    else
    begin
      bg := 0;
      en := FImperium.PlanetsCount - 1;
    end;

    SetLength(Fleet, 1);
    Fleet[0].Name := '—борщик “Ємной материи';
    Fleet[0].Value := FMinersCount;

    Result := true;
    cnt := 0;
    for i := bg to en do
    begin
      pl := FImperium.GetPlanetI(i);
      if (pl = nil) or
         (pl.isMoon) or
         (FImperium.GetPlanetC(pl.Coords, true) = nil) or
         (pl.GetShipsCount(Fleet[0].Name, true, false) < Fleet[0].Value)
      then continue;

      coords := pl.Coords;
      coords.PlType := ptMoon;

      res := FServer.FleetMove(
          FImperium, pl.ID, coords, Fleet, foDarkMateryMine, 10, 100);

      Result := Result and res;
      // only one expedition to the moon (2 moons v.1.4) allowed at 1 time
      cnt := cnt + 1;
      if cnt >= FExpeditionsCount then break;
    end;
  except
    Result := false;
  end;
end;

function TStMineDarkMatery.LoadConfig(cfg: string): ISuperObject;
begin
  Result := inherited LoadConfig(cfg);

  FMinersCount := SOAsIntegerDef(Result, 'MinersCount', 1);
  FExpeditionsCount := SOAsIntegerDef(Result, 'ExpeditionsCount', 10);
  FMaxDarkMatery := SOAsIntegerDef(Result, 'MaxDarkMatery', 0);
end;

{ TStFleetMove }

procedure TStFleetMove.Clear;
begin
  inherited;

  FWaitAllShipsAvail := true;
  FFleetOrder := foNone;
  FCoords.Clear;
  FSpeed := 10;
  FTimeThere := 1;
  SetLength(FFleet, 0);
end;

constructor TStFleetMove.Create(imp: TImperium; srv: TMoonServerConnect;
  db: TMoonDB);
begin
  inherited;

  FGroupID := 8;
end;

function TStFleetMove.IntExecute: boolean;
var
  pl: TPlanet;
  cnt,
  i: integer;
begin
  Result := true;

  pl := FImperium.GetPlanet(FFromPlanetID);
  if pl = nil then exit;

  if FWaitAllShipsAvail then
  begin
    for i := 0 to length(FFleet) - 1 do
      if FFleet[i].Value > pl.GetShipsCount(FFleet[i].Name, true, false)
      then exit;
  end;

  // normalize count
  for i := 0 to length(FFleet) - 1 do
  begin
    cnt := pl.GetShipsCount(FFleet[i].Name, true, false);
    if FFleet[i].Value > cnt then
      FFleet[i].Value := cnt;
  end;

  FServer.FleetMove(FImperium, FFromPlanetID, FCoords, FFleet, FFleetOrder, FSpeed, FTimeThere);
end;

function TStFleetMove.LoadConfig(cfg: string): ISuperObject;
begin
  Result := inherited LoadConfig(cfg);

  FWaitAllShipsAvail := SOAsBooleanDef(Result, 'WaitAllShipsAvail', true);
  FFleetOrder := TFleetOrder(SOAsIntegerDef(Result, 'FleetOrder', 0));
  FCoords.FromStr(SOAsString(Result, 'coords'));
  FSpeed := SOAsIntegerDef(Result, 'Speed', 10);
  FTimeThere := SOAsIntegerDef(Result, 'TimeThere', 1);
  FFleet := SOAsShips(Result, 'ships');
end;

{ TStPlanetBuild }

procedure TStPlanetBuild.CalcBuildPlan(pl: TPlanet; btree: TGameItems;
  var indx: Integer);
var
  i: Integer;
  bres,
  res: TGameRes;
  MinResSumm: Int64;
  level: Integer;
begin
  if pl = nil then exit;

  indx := -1;
  MinResSumm := -1;
  for i := 0 to length(btree) - 1 do
  begin
    if btree[i].GroupName <> 'ѕостройки' then continue;

    level := pl.GetBuildLevel(btree[i].Name);
    res := FDB.GetBuildingRes(btree[i].Name, level);
    if res.Eq0 then continue;
    bres := pl.CurRes;
    res.Sub(bres);
    res.NormalizeLow(0);
    if (btree[i].Level > level) and
       (FImperium.PlanetCanBuild(pl.ID, btree[i].Name)) and
       ((MinResSumm < 0) or (MinResSumm > res.ResSumm)) then
    begin
      MinResSumm := res.ResSumm;
      indx := i;
    end;
  end;
end;

procedure TStPlanetBuild.Clear;
begin
  inherited;

  FLockRes := false;
  FBuildType := -1;
  FPlaningLock := 60 * 30;
end;

constructor TStPlanetBuild.Create(imp: TImperium; srv: TMoonServerConnect;
  db: TMoonDB);
begin
  inherited;

end;

function TStPlanetBuild.IntBuildPlanet(PlanetID: integer): boolean;
var
  ABuildType: integer;
  pl: TPlanet;
  btree: TGameItems;
  indx: Integer;
begin
  Result := false;
  try
    ABuildType := FBuildType;

    pl := FImperium.GetPlanet(PlanetID);
    if pl = nil then exit;
    if pl.BuildsBuilding then exit;
    if pl.FreeFieldsCount <=0 then exit;

    if ABuildType < 0 then
    begin
      ABuildType := FDB.GetPlanetBuildType(PlanetID);

      if ABuildType < 0 then exit;
    end;

    btree := FDB.GetPlanetBuildTree(ABuildType);
    CalcBuild(pl, btree, indx);
    if indx >= 0 then
    begin
      Result := FServer.BuildBuilding(FImperium, PlanetID,
        btree[indx].Name, btree[indx].Level, false);
      exit;
    end;

    // планирование
    CalcBuildPlan(pl, btree, indx);
    if indx >= 0 then
    begin
      pl.BuildingPlan.Clear;
      pl.BuildingPlan.Item := btree[indx];
      //!!!!!!!!!!!!!!!!!
      pl.BuildingPlan.NeedRes := btree[indx].BuildRes;
      pl.BuildingPlan.EndPlaningDate :=
        Now + 1 / SecsPerDay * FPlaningLock;
    end;

  except
  end;
end;

function TStPlanetBuild.IntExecute: boolean;
var
  i: Integer;
begin
  Result := true;

  // build one planet
  if FFromPlanetID <> 0 then
  begin
    IntBuildPlanet(FFromPlanetID);
    exit;
  end;

  // build all planets
  for i := 0 to FImperium.PlanetsCount - 1 do
  begin
    IntBuildPlanet(FImperium.GetPlanetI(i).ID);
  end;
  FServer.UpdateImperium(FImperium);
end;

procedure TStPlanetBuild.CalcBuild(pl: TPlanet; btree: TGameItems;
  var indx: Integer);
var
  i: Integer;
  res: TGameRes;
  MinResSumm: Int64;
  level: Integer;
begin
  if pl = nil then exit;

  indx := -1;
  MinResSumm := -1;
  for i := 0 to length(btree) - 1 do
  begin
    level := pl.GetBuildLevel(btree[i].Name);
    res := FDB.GetBuildingRes(btree[i].Name, level);
    if (btree[i].GroupName = 'ѕостройки') and
       (btree[i].Level > level) and
       (not res.Eq0) and
       (pl.HaveResources(res)) and
       (FImperium.PlanetCanBuild(pl.ID, btree[i].Name)) and
       ((MinResSumm < 0) or (MinResSumm > res.EkvResSumm)) then
    begin
      MinResSumm := res.EkvResSumm;
      indx := i;
    end;
  end;
end;

function TStPlanetBuild.LoadConfig(cfg: string): ISuperObject;
begin
  Result := inherited LoadConfig(cfg);

  FLockRes := SOAsBooleanDef(Result, 'LockRes', false);
  FBuildType := SOAsIntegerDef(Result, 'BuildType', -1);
end;

{ TStInit }

procedure TStInit.Clear;
begin
  inherited;
end;

constructor TStInit.Create(imp: TImperium; srv: TMoonServerConnect;
  db: TMoonDB);
begin
  inherited;

  FGroupID := 100;
end;

function TStInit.IntExecute: boolean;
var
  techdeps,
  tech: TGameItems;
  i: integer;
begin
  Result := true;
  try
    //  update my_planets into DB
    FServer.UpdateImperium(FImperium);
    FDB.UpdateMyPlanets(FImperium);
    FDB.Commit;

    // update technoogy tree
    if FServer.UpdateTechnologies(FImperium, tech, techdeps) then
    begin
      FDB.UpdateTechnologies(tech);
      FDB.ClearTechDeps;
      FDB.UpdateTechDeps(techdeps);
      FDB.Commit;
    end;

    // update buildings build params
    for i := 0 to FImperium.PlanetsCount - 1 do
    begin
      FServer.UpdateBuildings(FImperium, FImperium.GetPlanetI(i).ID);
    end;

    // update researching...
    FServer.UpdateResearching(FImperium);
  except
  end;
end;

function TStInit.LoadConfig(cfg: string): ISuperObject;
begin
  Result := inherited LoadConfig(cfg);
end;

{ TStResearch }

procedure TStResearch.CalcResearchPlan(rtree: TGameItems; pl: TPlanet;
  var indx: Integer);
var
  bres,
  res: TGameRes;
  MinResSumm: Int64;
  i: Integer;
  level: Integer;
  rlen: Integer;
begin
  indx := -1;
  MinResSumm := -1;
  for i := 0 to length(rtree) - 1 do
  begin
    if rtree[i].GroupName <> '»сследовани€' then continue;

    level := FImperium.GetResearchLevel(rtree[i].Name);
    bres := FDB.GetBuildingRes(rtree[i].Name, level);
    if bres.Eq0 then continue;
    res := pl.CurRes;
    res.Sub(bres);
    res.NormalizeLow(0);
    rlen := Trunc(FDB.GetBuildingLength(rtree[i].Name, level) * 24 * 60 * 60);
    if (rtree[i].Level > level) and
       ((FMaxLength = 0) or (rlen < FMaxLength)) and
       ((MinResSumm < 0) or (MinResSumm > res.ResSumm)) and
       (FImperium.CanResearch(rtree[i].Name)) then
    begin
      MinResSumm := res.ResSumm;
      indx := i;
    end;
  end;
end;

procedure TStResearch.Clear;
begin
  inherited;

  FLockRes := false;
  FMaxLength := 0;
  FPlaningLock := 60 * 30;
end;

constructor TStResearch.Create(imp: TImperium; srv: TMoonServerConnect;
  db: TMoonDB);
begin
  inherited;

end;

function TStResearch.IntExecute: boolean;
var
  pl: TPlanet;
  rtree1,
  rtree2: TGameItems;
  indx1,
  indx2: integer;
begin
  Result := true;
  try
    if FImperium.MakeResearch then exit;

    pl := FImperium.GetPlanet(FFromPlanetID);
    if (pl = nil) or
       (pl.GetBuildLevel('»сследовательска€ лаборатори€') < 1)
    then exit;

    // пробуем исследовать записанные исследовани€
    rtree1 := FDB.GetPlanetBuildTree(100);
    CalcResearch(rtree1, pl, indx1);

    // пробуем исследовать исследовани€ дл€ дерева технологий
    // необходимых дл€ развити€ планет
    rtree2 := FDB.GetImperiumBuildTree;
    CalcResearch(rtree2, pl, indx2);

    // выбрать минимальное по времени исследование
    if (indx1 >= 0) and (indx2 >= 0) then
    begin
      if FDB.GetBuildingLength(
            rtree1[indx1].Name,
            FImperium.GetResearchLevel(rtree1[indx1].Name)) >
         FDB.GetBuildingLength(rtree2[indx2].Name,
            FImperium.GetResearchLevel(rtree1[indx2].Name))
      then
        indx1 := 0;
    end;

    if indx1 >= 0 then
    begin
      Result := FServer.MakeResearch(FImperium, FFromPlanetID,
        rtree1[indx1].Name, rtree1[indx1].Level);
      exit;
    end;

    if indx2 >= 0 then
    begin
      Result := FServer.MakeResearch(FImperium, FFromPlanetID,
        rtree2[indx2].Name, rtree2[indx2].Level);
      exit;
    end;

    // планируем исследовани€, если счас ничего не исследуетс€
    // или ничего не получилось исследовать
    CalcResearchPlan(rtree1, pl, indx1);
    CalcResearchPlan(rtree2, pl, indx2);

    if indx1 >= 0 then
    begin
      FImperium.ResearchingPlan.Clear;
      FImperium.ResearchingPlan.Item := rtree1[indx1];
      FImperium.ResearchingPlan.EndPlaningDate :=
        Now + 1 / SecsPerDay * FPlaningLock;
      exit;
    end;

    if indx2 >= 0 then
    begin
      FImperium.ResearchingPlan.Clear;
      FImperium.ResearchingPlan.Item := rtree2[indx2];
      FImperium.ResearchingPlan.EndPlaningDate :=
        Now + 1 / SecsPerDay * FPlaningLock;
      exit;
    end;
  except
  end;
end;

procedure TStResearch.CalcResearch(rtree: TGameItems; pl: TPlanet;
  var indx: Integer);
var
  res: TGameRes;
  MinResSumm: Int64;
  i: Integer;
  level: Integer;
  rlen: Integer;
begin
  indx := -1;
  MinResSumm := -1;
  for i := 0 to length(rtree) - 1 do
  begin
    if rtree[i].GroupName <> '»сследовани€' then continue;

    level := FImperium.GetResearchLevel(rtree[i].Name);
    res := FDB.GetBuildingRes(rtree[i].Name, level);
    rlen := Trunc(FDB.GetBuildingLength(rtree[i].Name, level) * 24 * 60 * 60);
    if (rtree[i].Level > level) and
       ((FMaxLength = 0) or (rlen < FMaxLength)) and
       (not res.Eq0) and
       (pl.HaveResources(res)) and
       ((MinResSumm < 0) or (MinResSumm > res.EkvResSumm)) and
       (FImperium.CanResearch(rtree[i].Name)) then
    begin
      MinResSumm := res.EkvResSumm;
      indx := i;
    end;
  end;
end;

function TStResearch.LoadConfig(cfg: string): ISuperObject;
begin
  Result := inherited LoadConfig(cfg);

  FLockRes := SOAsBooleanDef(Result, 'LockRes', false);
  FMaxLength := SOAsIntegerDef(Result, 'MaxLength', 0);
  FPlaningLock := SOAsIntegerDef(Result, 'PlaningLock', 60 * 30);
end;

end.
