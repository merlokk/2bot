unit uGameItems;

interface
uses
  Classes, SysUtils,
  uHTMLParser;

type
  TPlanetType = (ptPlanet = 1, ptMoon = 3, ptCrashField = 2);

  TGameCoords = record
    Galaxy: integer;
    System: integer;
    Planet: integer;
    PlType: TPlanetType;

    procedure Clear;
    procedure Inc; overload;
    procedure Inc(GM, GP, SM, SP, PM, PP, cnt: integer); overload;

    function Equal(c: TGameCoords): boolean;
    function ToStr: string;
    function FromStr(s: string): boolean;
  end;

  TIntAttribRec = packed record
    Name: string;
    Value: int64;
  end;

  TGameRes = packed record
    Metal,
    Crystal,
    Deiterium: int64;

    procedure Clear;
    procedure Add(r: TGameRes); overload;
    procedure Add(r: TGameRes; m: int64); overload;
    procedure Sub(r: TGameRes);
    procedure Mul(m: int64);
    function Eq0: boolean;
    function MoreThan(r: TGameRes): boolean;
    function EkvResSumm: int64;
    function ResSumm: int64;
  end;

  TLockResRec = packed record
    StrategyID: integer;
    Res: TGameRes;
    LockFrom,
    LockTo: TDateTime;
    procedure Clear;
  end;

  TGameItem = packed record
    ID: integer;
    GroupName,
    Name: string;
    Level: integer;
    BuildRes: TGameRes;
    BuildTime: TDateTime;
    Tag: int64;
    procedure Clear;
  end;
  TGameItems = array of TGameItem;

  TShip = packed record
    ID: integer;
    Name: string;
    Tag: string;
    Count: integer;
    BuildRes: TGameRes;
    BuildTime: TDateTime;
    BuildMaxCount: integer;
    procedure Clear;
  end;
  TShips = array of TShip;

  TFleetOrder = (foNone = 0, foAtack=1, foTransport=3, foLeave=4, foState=5,
    foSpy=6, foColonize=7, foRecycle=8, foDarkMateryMine = 11, foExpedition = 15);
  TFleetOrderArr = array of TIntAttribRec;

  TBuildCmd = (bcInsert, bcDestroy);

  TCalc = class
    class function ShipsBuildCost(Fleet: TFleetOrderArr): TGameRes;
    class function ShipsBuildEstim(Fleet: TFleetOrderArr): TDateTime;
    class function CutFleet(Fleet: TFleetOrderArr; Res: TGameRes): boolean;
    class function UpdateShipBook(ships: TShips): boolean;

    class function UpdateBuildingBook(ships: TGameItems): boolean;
  end;

  TPlanet = class
  protected
    FAttr: array of TIntAttribRec;

    FBuildingShipsEndDT: TDateTime;
    FBuildingShips: TFleetOrderArr;
    FBuildingBuilds: TGameItems;

    FLockResources: TLockResRec;
  public
    ID: int64;
    Name: string;
    Coords: TGameCoords;
    MaxFields,
    CurFields: integer;
    CurRes: TGameRes;
    Energy,
    FreeEnergy: int64;
    isMoon: boolean;

    serial: cardinal;

    UpdateDT: TDateTime;

    constructor Create;

    procedure Clear;
    procedure UpdateAttr(AName: string; AValue: int64);
    procedure FinishUpdate;

    procedure SetBuildingShips(Fleet: TFleetOrderArr; BuildEstimate: TDateTime = 0);
    procedure UpdateFromShipYard(ships: TShips);
    function GetShipsCount(name: string; CountCurrent, CountBuilding: boolean): int64;
    function ShipsBuilding: boolean;

    procedure SetBuildingBuilds(Builds: TGameItems);
    procedure UpdateFromBuildings(builds: TGameItems);
    function GetBuildLevel(name: string): integer;
    function BuildsBuilding: boolean;

    function HaveResources(res: TGameRes): boolean;
    function isResoucesLocked: boolean;
    function LockResources(StrategyID: integer; res: TGameRes; LockLen: TDateTime = 0): boolean;
  end;

  TImperium = class
  protected
    FPlanets: array of TPlanet;
    FResearch: array of TIntAttribRec;
    FResearching: TGameItems;
  public
    Valid: boolean;
    LastUpdate: TDateTime;

    DarkMatery: int64;

    MaxExpedition,
    CurExpedition,
    MaxFleets,
    CurFleets: integer;

    constructor Create;

    procedure Clear;
    procedure ClearPlanets;
    procedure ClearResearch;

    procedure UpdateResearchLevel(Name: string; Level: integer);
    procedure UpdateResearchList(resch: TGameItems);
    function GetResearchLevel(Name: string): integer;
    function ResearchCount: integer;

    function AddPlanet(ID: int64): TPlanet;
    function GetOrCreatePlanet(ID: int64): TPlanet;
    procedure DeletePlanets(serial: cardinal = 0);
    function GetPlanet(ID: int64): TPlanet;
    function GetPlanetIndx(ID: int64): integer;
    function GetPlanetI(indx: integer): TPlanet;
    function GetPlanetC(coords: TGameCoords; isMoon: boolean = false): TPlanet;
    procedure UpdatePlanetAttr(ID: int64; Name: string; Value: integer);
    function PlanetsCount(CountMoons: boolean = true): integer;
    function MoonsCount: integer;
    function MaxPlanetsCount: integer;
    function PlanetHaveResources(ID: int64; res: TGameRes): boolean;
    function PlanetCanBuild(PlanetID: int64; name: string): boolean;

    function UpdateResearching(resch: TGameItems): boolean;
    function CanResearch(name: string): boolean;
    function MakeResearch: boolean;
  end;

  TUser = class
  protected
  public
    ID: int64;
    Name,
    MainPlanetName: string;
    MainPlanetCoords: TGameCoords;
    Alliance: string;

    Score: int64;
  end;
  TUserList = array of TUser;

  TGalaxyMapMove = (gmvNone, gmvGalaxyLeft, gmvGalaxyRight, gmvSystemLeft, gmvSystemRight);

  TEnemyPlanet = packed record
    Coords: TGameCoords;
    Name: string;
    UserID: int64;
    isEmpty,
    HaveMoon,
    HaveCrashField: boolean;
    procedure Clear;
  end;

  TPlanetSystem = array of TEnemyPlanet;

  TGalaxy = class
  protected
  public
  end;

const
  GalaxyMapMoveStr: array [gmvNone .. gmvSystemRight] of string = (
    '',
    'galaxyLeft',
    'galaxyRight',
    'systemLeft',
    'systemRight');
  BuildCmdStr: array [bcInsert .. bcDestroy] of string = (
    'insert',
    'destroy');

implementation
uses
  uDB;

{ TImperium }

function TImperium.GetOrCreatePlanet(ID: int64): TPlanet;
begin
  Result := GetPlanet(ID);
  if Result = nil then
    Result := AddPlanet(ID);
end;

function TImperium.AddPlanet(ID: int64): TPlanet;
begin
  SetLength(FPlanets, length(FPlanets) + 1);
  FPlanets[length(FPlanets) - 1] := TPlanet.Create;
  FPlanets[length(FPlanets) - 1].ID := ID;
  Result := FPlanets[length(FPlanets) - 1];
end;

function TImperium.CanResearch(name: string): boolean;
var
  dep: TGameItems;
  i: integer;
begin
  Result := false;
  if MakeResearch then exit;

  Result := true;
  dep := TMoonDB.GetInstance.GetTechDeps(name);
  for i := 0 to length(dep) - 1 do
    if (dep[i].GroupName = 'Исследования') and
       (GetResearchLevel(dep[i].Name) < dep[i].Level) then
    begin
      Result := false;
      exit;
    end;
end;

procedure TImperium.Clear;
begin
  Valid := false;
  LastUpdate := 0;

  DarkMatery := 0;

  MaxExpedition := 0;
  CurExpedition := 0;
  MaxFleets := 0;
  CurFleets := 0;

  ClearPlanets;
  ClearResearch;
end;

procedure TImperium.ClearPlanets;
var
 i: integer;
begin
  for i := 0 to length(FPlanets) - 1 do
    if FPlanets[i] <> nil then FPlanets[i].Free;

  SetLength(FPlanets, 0);
end;

procedure TImperium.ClearResearch;
begin
  SetLength(FResearch, 0);
  SetLength(FResearching, 0);
end;

constructor TImperium.Create;
begin
  inherited;

  Clear;
end;

procedure TImperium.DeletePlanets(serial: cardinal);
var
  i,
  j: Integer;
begin
  for i := length(FPlanets) - 1 downto 0 do
    if FPlanets[i].serial <> serial then
    begin
      FPlanets[i].Free;
      for j := i to length(FPlanets) - 2 do
        FPlanets[j] := FPlanets[j + 1];
      SetLength(FPlanets, length(FPlanets) - 1);
    end;

end;

function TImperium.GetPlanet(ID: int64): TPlanet;
var
 i: integer;
begin
  Result := nil;

  for i := 0 to length(FPlanets) - 1 do
    if (FPlanets[i].ID = ID) then
    begin
      Result := FPlanets[i];
      break;
    end;
end;

function TImperium.GetPlanetC(coords: TGameCoords; isMoon: boolean = false): TPlanet;
var
 i: integer;
begin
  Result := nil;

  for i := 0 to length(FPlanets) - 1 do
    if (FPlanets[i].Coords.Equal(coords)) and (FPlanets[i].isMoon = isMoon) then
    begin
      Result := FPlanets[i];
      break;
    end;
end;

function TImperium.GetPlanetI(indx: integer): TPlanet;
begin
  Result := nil;

  if (indx >= 0) and (indx < length(FPlanets)) then
    Result := FPlanets[indx];
end;

function TImperium.GetPlanetIndx(ID: int64): integer;
var
 i: integer;
begin
  Result := -1;

  for i := 0 to length(FPlanets) - 1 do
    if (FPlanets[i].ID = ID) then
    begin
      Result := i;
      break;
    end;
end;

function TImperium.GetResearchLevel(Name: string): integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to length(FResearch) - 1 do
    if FResearch[i].Name = Name then
    begin
      Result := FResearch[i].Value;
      break;
    end;
end;

function TImperium.MakeResearch: boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to length(FResearching) - 1 do
    if FResearching[i].BuildTime > Now then
    begin
      Result := true;
      exit;
    end;
end;

function TImperium.MaxPlanetsCount: integer;
begin
  Result := GetResearchLevel('Астрофизика');
  if Result = 0 then Result := 20;
  if Result > 20 then Result := 20;
end;

function TImperium.MoonsCount: integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to length(FPlanets) - 1 do
    if FPlanets[i].isMoon then
      Result := Result + 1;
end;

function TImperium.PlanetCanBuild(PlanetID: int64; name: string): boolean;
var
  pl: TPlanet;
  dep: TGameItems;
  i: Integer;
begin
  Result := true;
  pl := GetPlanet(PlanetID);

  if ((name = 'Исследовательская лаборатория') or
      (name = 'Технополис')) and
     (MakeResearch) then
  begin
    Result := false;
    exit;
  end;

  if (name = 'Верфь') and
     ((pl <> nil) and (pl.ShipsBuilding)) then
  begin
    Result := false;
    exit;
  end;

  if (name = 'Фабрика нанитов') and
     ((pl <> nil) and (pl.ShipsBuilding)) then
  begin
    Result := false;
    exit;
  end;

  dep := TMoonDB.GetInstance.GetTechDeps(name);
  for i := 0 to length(dep) - 1 do
    if (dep[i].GroupName = 'Постройки') and
       (pl.GetBuildLevel(dep[i].Name) < dep[i].Level) or
       (dep[i].GroupName = 'Исследования') and
       (GetResearchLevel(dep[i].Name) < dep[i].Level)
    then
    begin
      Result := false;
      exit;
    end;
end;

function TImperium.PlanetHaveResources(ID: int64; res: TGameRes): boolean;
var
  pl: TPlanet;
begin
  Result := false;

  pl := GetPlanet(ID);
  if pl = nil then exit;
  Result := pl.HaveResources(res);
end;

function TImperium.PlanetsCount(CountMoons: boolean = true): integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to length(FPlanets) - 1 do
    if not FPlanets[i].isMoon or CountMoons then
      Result := Result + 1;
end;

function TImperium.ResearchCount: integer;
begin
  Result := length(FResearch);
end;

function TImperium.UpdateResearching(resch: TGameItems): boolean;
begin
  Result := true;
  FResearching := resch;
end;

procedure TImperium.UpdatePlanetAttr(ID: int64; Name: string; Value: integer);
var
 vPlanet: TPlanet;
begin
  vPlanet := GetPlanet(ID);
  if vPlanet = nil then exit;
  vPlanet.UpdateAttr(Name, Value);
end;

procedure TImperium.UpdateResearchLevel(Name: string; Level: integer);
var
 i: integer;
begin
  for i := 0 to length(FResearch) - 1 do
    if FResearch[i].Name = Name then
    begin
      FResearch[i].Value := Level;
      exit;
    end;

  SetLength(FResearch, length(FResearch) + 1);
  FResearch[length(FResearch) - 1].Name := Name;
  FResearch[length(FResearch) - 1].Value := Level;
end;

procedure TImperium.UpdateResearchList(resch: TGameItems);
var
  i: Integer;
begin
  for i := 0 to length(resch) - 1 do
    UpdateResearchLevel(resch[i].Name, resch[i].Level);
end;

{ TPlanet }

function TPlanet.BuildsBuilding: boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to length(FBuildingBuilds) - 1 do
    if FBuildingBuilds[i].BuildTime > now then
    begin
      Result := true;
      break;
    end;
end;

procedure TPlanet.Clear;
begin
  ID := 0;
  Name := '';
  Coords.Clear;
  MaxFields := 0;
  CurFields := 0;
  CurRes.Clear;
  Energy := 0;
  FreeEnergy := 0;
  UpdateDT := 0;
  isMoon := false;
  SetLength(FAttr, 0);

  FBuildingShipsEndDT := 0;
  SetLength(FBuildingShips, 0);
  SetLength(FBuildingBuilds, 0);

  FLockResources.Clear;

  serial := 0;
end;

constructor TPlanet.Create;
begin
  inherited;
  Clear;
end;

procedure TPlanet.FinishUpdate;
begin
  UpdateDT := Now;
end;

function TPlanet.GetBuildLevel(name: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to length(FAttr) - 1 do
    if FAttr[i].Name = name then
    begin
      Result := FAttr[i].Value;
      break;
    end;
end;

function TPlanet.GetShipsCount(name: string; CountCurrent,
  CountBuilding: boolean): int64;
var
  i: integer;
begin
  Result := 0;

  if CountBuilding and (FBuildingShipsEndDT > now) then
    for i := 0 to length(FBuildingShips) - 1 do
      if FBuildingShips[i].Name = name then
        Result := Result + FBuildingShips[i].Value;

  if CountCurrent then
    for i := 0 to length(FAttr) - 1 do
      if FAttr[i].Name = name then
        Result := Result + FAttr[i].Value;
end;

function TPlanet.HaveResources(res: TGameRes): boolean;
var
  r: TGameRes;
begin
  r := CurRes;
  if isResoucesLocked then r.Sub(FLockResources.Res);

  Result := r.MoreThan(res)
end;

function TPlanet.LockResources(StrategyID: integer; res: TGameRes;
  LockLen: TDateTime): boolean;
begin
  Result := false;
  if isResoucesLocked then exit;

  FLockResources.StrategyID := StrategyID;
  FLockResources.Res := res;
  FLockResources.LockFrom := Now;
  FLockResources.LockTo := LockLen;

  Result := true;
end;

function TPlanet.isResoucesLocked: boolean;
begin
  Result :=
    (FLockResources.LockTo >= Now) and
    (not FLockResources.Res.Eq0);
end;

procedure TPlanet.SetBuildingBuilds(Builds: TGameItems);
begin
  FBuildingBuilds := Builds;
end;

procedure TPlanet.SetBuildingShips(Fleet: TFleetOrderArr; BuildEstimate: TDateTime = 0);
begin
  if length(Fleet) = 0 then
  begin
    FBuildingShipsEndDT := 0;
    SetLength(FBuildingShips, 0);
    exit;
  end;

  FBuildingShips := Fleet;
  if BuildEstimate = 0 then
    FBuildingShipsEndDT := Now + TCalc.ShipsBuildEstim(Fleet)
  else
    FBuildingShipsEndDT := Now + BuildEstimate;
end;

function TPlanet.ShipsBuilding: boolean;
begin
  Result :=
    (FBuildingShipsEndDT > now) and
    (length(FBuildingShips) > 0);
end;

procedure TPlanet.UpdateAttr(AName: string; AValue: int64);
var
 i: integer;
begin
  for i := 0 to length(FAttr) - 1 do
    if FAttr[i].Name = AName then
    begin
      FAttr[i].Value := AValue;
      exit;
    end;

  SetLength(FAttr, length(FAttr) + 1);
  FAttr[length(FAttr) - 1].Name := AName;
  FAttr[length(FAttr) - 1].Value := AValue;
end;

procedure TPlanet.UpdateFromBuildings(builds: TGameItems);
var
  i: Integer;
begin
  for i := 0 to length(builds) - 1 do
    UpdateAttr(builds[i].Name, builds[i].Level);
end;

procedure TPlanet.UpdateFromShipYard(ships: TShips);
var
  i: Integer;
begin
  for i := 0 to length(ships) - 1 do
    UpdateAttr(ships[i].Name, ships[i].Count);
end;

{ TGameCoords }

procedure TGameCoords.Clear;
begin
  Galaxy := 0;
  System := 0;
  Planet := 0;
  PlType := ptPlanet;
end;

function TGameCoords.Equal(c: TGameCoords): boolean;
begin
  Result :=
    (c.Galaxy = Galaxy) and
    (c.System = System) and
    (c.Planet = Planet);
end;

function TGameCoords.FromStr(s: string): boolean;
var
 sl: TStringList;
begin
  Result := false;
  if s = '' then exit;
  try
    if s[1] = '[' then s := Copy(s, 2, length(s) - 2);
    sl := TStringList.Create;
    sl.Delimiter := ':';
    sl.DelimitedText := s;
    if sl.Count = 3 then
    begin
      Galaxy := StrToIntDef(sl[0], -1);
      System := StrToIntDef(sl[1], -1);
      Planet := StrToIntDef(sl[2], -1);

      if (Galaxy > 0) and (System > 0) and (Planet > 0)
      then
        Result := true;
    end;
    sl.Free;
  except
  end;
end;

procedure TGameCoords.Inc;
begin
  Inc(1, 9, 1, 499, 1, 16, 1);
end;

procedure TGameCoords.Inc(GM, GP, SM, SP, PM, PP, cnt: integer);
begin
  Planet := Planet + cnt;
  while (Planet > PP) or (System > SP) or (Galaxy > GP) do
  begin
    if Planet > PP then
    begin
      Planet := PM + (Planet - PP - 1);
      System := System + 1;
    end;
    if System > SP then
    begin
      System := SM + (System - SP - 1);
      Galaxy := Galaxy + 1;
    end;
    if Galaxy > GP then Galaxy := GM + (Galaxy - GP - 1);
  end;

end;

function TGameCoords.ToStr: string;
begin
  Result := '[' + IntToStr(Galaxy) + ':' +
    IntToStr(System) + ':' + IntToStr(Planet) + ']';
end;

{ TGameRes }

procedure TGameRes.Add(r: TGameRes);
begin
  Metal := r.Metal;
  Crystal := r.Crystal;
  Deiterium := r.Deiterium;
end;

procedure TGameRes.Add(r: TGameRes; m: int64);
begin
  Metal := r.Metal * m;
  Crystal := r.Crystal * m;
  Deiterium := r.Deiterium * m;
end;

procedure TGameRes.Clear;
begin
  Crystal := 0;
  Metal := 0;
  Deiterium := 0;
end;

function TGameRes.Eq0: boolean;
begin
  Result :=
    (Metal = 0) and
    (Crystal = 0) and
    (Deiterium = 0);
end;

function TGameRes.MoreThan(r: TGameRes): boolean;
begin
  Result :=
    (Metal > r.Metal) and
    (Crystal > r.Crystal) and
    (Deiterium > r.Deiterium);
end;

procedure TGameRes.Mul(m: int64);
begin
  Metal := Metal * m;
  Crystal := Crystal * m;
  Deiterium := Deiterium * m;
end;

function TGameRes.ResSumm: int64;
begin
  Result := Metal + Crystal + Deiterium;
end;

procedure TGameRes.Sub(r: TGameRes);
begin
  Metal := Metal - r.Metal;
  Crystal := Crystal - r.Crystal;
  Deiterium := Deiterium - r.Deiterium;
end;

function TGameRes.EkvResSumm: int64;
begin
try
  Result := Metal div 4 + Crystal div 2 + Deiterium;
except
  Result := 0;
end;
end;

{ TCalc }

class function TCalc.CutFleet(Fleet: TFleetOrderArr; Res: TGameRes): boolean;
begin
  Result := false;

end;

class function TCalc.ShipsBuildCost(Fleet: TFleetOrderArr): TGameRes;
var
  i: Integer;
begin
  Result.Clear;
  for i := 0 to length(Fleet) - 1 do
    Result.Add(
      TMoonDB.GetInstance.GetShipBook(Fleet[i].Name).BuildRes,
      Fleet[i].Value);
end;

class function TCalc.ShipsBuildEstim(Fleet: TFleetOrderArr): TDateTime;
var
  cnt,
  i: Integer;
begin
  cnt := 0;
  for i := 0 to length(Fleet) -1 do
    cnt := cnt + Fleet[i].Value;

  Result := 1 / (24 * 60 * 60) * (cnt + 10);
end;

class function TCalc.UpdateBuildingBook(ships: TGameItems): boolean;
begin
  Result := TMoonDB.GetInstance.UpdateBuildingBook(ships);
  TMoonDB.GetInstance.Commit;
end;

class function TCalc.UpdateShipBook(ships: TShips): boolean;
begin
  Result := TMoonDB.GetInstance.UpdateShipBook(ships);
  TMoonDB.GetInstance.Commit;
end;

{ TEnemyPlanet }

procedure TEnemyPlanet.Clear;
begin
  Coords.Clear;
  Name := '';
  UserID := 0;
  isEmpty := false;
  HaveMoon := false;
  HaveCrashField := false;
end;

{ TShip }

procedure TShip.Clear;
begin
  ID := 0;
  Name := '';
  Tag := '';
  Count := 0;
  BuildRes.Clear;
  BuildTime := 0;
  BuildMaxCount := 0;
end;

{ TGameItem }

procedure TGameItem.Clear;
begin
  ID := 0;
  GroupName := '';
  Name := '';
  Level := 0;
  BuildRes.Clear;
  BuildTime := 0;
end;

{ TLockResRec }

procedure TLockResRec.Clear;
begin
  StrategyID := 0;
  Res.Clear;
  LockFrom := 0;
  LockTo := 0;
end;

end.
