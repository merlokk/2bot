unit uDB;

interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Forms, math, Types,
  FIBDatabase, pFIBDatabase, FIBQuery, pFIBQuery, pFIBProps, IniFiles,
  StrUtils, Generics.Collections, Generics.Defaults, uGameItems;

type
  TMoonDB = class
  protected
    class var FInstance: TMoonDB;
    class constructor ClassCreate;
  private
    FFloatFormat: TFormatSettings;

    FConnected: boolean;

    FIBDatabase: TpFIBDatabase;
    FIBQuery: TpFIBQuery;
    FIBQueryCurs,
    FIBQueryCursOut: TpFIBQuery;
    FIBTransaction: TpFIBTransaction;

  public
    class function GetInstance: TMoonDB; static;

    constructor Create;
    destructor Destory;

    property FloatFormat: TFormatSettings read FFloatFormat;

    property Connected: boolean read FConnected;
    function Connect: boolean;
    procedure Commit;

    function GetStrategyList: TpFIBQuery;

    function GetGameItemGroupID(GroupName: string): integer;
    function GetGameItemID(Name: string): integer;
    function GetGameItem(Name: string): TGameItem; overload;
    function GetGameItem(ID: integer): TGameItem; overload;
    function UpdateTechnologies(techs: TGameItems): boolean; overload;
    function UpdateTechnologies(tech: TGameItem): boolean; overload;
    function ClearTechDeps: boolean;
    function GetTechDeps(ItemName: string): TGameItems; overload;
    function GetTechDeps(ItemID: integer): TGameItems; overload;
    function UpdateTechDeps(techdeps: TGameItems): boolean; overload;
    function UpdateTechDeps(techdep: TGameItem): boolean; overload;

    function UpdateMyPlanets(imp: TImperium): boolean;
    function UpdateMyPlanet(pl: TPlanet; serial: integer = 0): boolean;
    function DeleteMyPlanets(serial: integer = 0): boolean;
    function GetPlanetBuildType(PlanetID: integer): integer;
    function GetPlanetBuildTree(BuildType: integer): TGameItems;
    function GetImperiumBuildTree: TGameItems;

    function ClearUniverse: boolean;
    function AddSystem(Galaxy, System: integer): boolean;
    function GetSystemToScan(cnt: integer): TpFIBQuery;
    function ClearPlanetSystem(Galaxy, System: integer): boolean;
    function UpdatePlanetSystem(PlanetSystem: TPlanetSystem): boolean;
    function GetPlanet(coords: TGameCoords): TEnemyPlanet;
    function AddPlanet(pl: TEnemyPlanet): boolean;

    function UpdateUsers(users: TUserList): boolean;
    function UpdateUser(user: TUser): boolean;

    function UpdateShipBook(ships: TShips): boolean; overload;
    function UpdateShipBook(ship: TShip): boolean; overload;
    function GetShipBook(name: string): TShip;

    function UpdateBuildingBook(Buildings: TGameItems): boolean; overload;
    function UpdateBuildingBook(Building: TGameItem): boolean; overload;
    function intGetBuildingRes(name: string; level: integer): TGameRes;
    function GetBuildingKoef(name: string): Extended;
    function GetBuildingRes(name: string; level: integer): TGameRes;
    function GetBuildingLength(name: string; level: integer): TDateTime;

    function SetParam(ParamName, ParamValue: string): boolean; overload;
    function SetParam(ParamName: string; ParamValue: int64): boolean; overload;
    function GetParam(ParamName: string; Default: string = ''): string; overload;
    function GetParam(ParamName: string; Default: int64 = 0): int64; overload;
  end;

implementation

{ TMoonDB }

function TMoonDB.AddPlanet(pl: TEnemyPlanet): boolean;
begin
  Result := false;
  try
    FIBQuery.SQL.Text := 'update or insert into universe ' +
      '(galaxy, system, planet, name, user_id, have_moon, have_crashfield, last_update) values (' +
      IntToStr(pl.Coords.Galaxy) + ',' +
      IntToStr(pl.Coords.System) + ',' +
      IntToStr(pl.Coords.Planet) + ',''' +
      Trim(pl.Name) + ''',' +
      IntToStr(pl.UserID) + ',' +
      IntToStr(integer(pl.HaveMoon)) + ',' +
      IntToStr(integer(pl.HaveCrashField)) + ',' +
      'current_timestamp ) matching (galaxy, system, planet)';
    FIBQuery.ExecQuery;

    Result := true;
  except
  end;
end;

function TMoonDB.AddSystem(Galaxy, System: integer): boolean;
begin
  Result := false;
  if not FConnected then exit;
  try
    FIBQuery.SQL.Text := 'insert into universe (galaxy, system, planet, name) values ' +
      '(' + IntToStr(Galaxy) + ', ' + IntToStr(System) + ', 0, ''[' +
      IntToStr(Galaxy) + ',' + IntToStr(System) + ']'')';
    FIBQuery.ExecQuery;

    Result := true;
  except
  end;

end;

class constructor TMoonDB.ClassCreate;
begin
  inherited;
  FInstance := nil;
end;

function TMoonDB.ClearPlanetSystem(Galaxy, System: integer): boolean;
begin
  Result := false;
  try
    FIBQuery.SQL.Text := 'delete from UNIVERSE where ' +
      '(system=' + IntToStr(System) + ') and ' +
      '(galaxy=' + IntToStr(Galaxy) + ') and ' +
      '(planet <> 0)';
    FIBQuery.ExecQuery;
    Result := true;
  except
  end;
end;

function TMoonDB.ClearTechDeps: boolean;
begin
  Result := false;
  if not FConnected then exit;
  try
    FIBQuery.SQL.Text := 'delete from TECH_DEPS ';
    FIBQuery.ExecQuery;

    Result := true;
  except
  end;
end;

function TMoonDB.ClearUniverse: boolean;
begin
  Result := false;
  try
    FIBQuery.SQL.Text := 'delete from UNIVERSE';
    FIBQuery.ExecQuery;
    Commit;
    Result := true;
  except
  end;
end;

procedure TMoonDB.Commit;
begin
  FIBTransaction.Commit;
end;

function TMoonDB.Connect: boolean;
begin
  Result := false;
  if Connected then exit;

  with TIniFile.Create(ExtractFilePath(Application.ExeName) + '2moons.ini')do
  try
    FIBDatabase.DBName :=
      ReadString('DB', 'path', 'C:\Projects\!chrome\2moons\2moons.fdb');
    FIBDatabase.DBParams.Values['user_name'] :=
      ReadString('DB', 'username', 'SYSDBA');
    FIBDatabase.DBParams.Values['password'] :=
      ReadString('DB', 'password', 'masterkey');
    FIBDatabase.DBParams.Values['lc_ctype'] := 'utf-8';
    FIBDatabase.SQLDialect := 3;
    FIBDatabase.WaitForRestoreConnect := 100;
    FIBDatabase.Open(true);
  finally
    Free;
  end;

  FConnected := FIBDatabase.Connected;
  Result := FConnected;
end;

constructor TMoonDB.Create;
begin
  inherited;

  FFloatFormat := TFormatSettings.Create;
  FFloatFormat.DecimalSeparator := '.';

  FIBDatabase := TpFIBDatabase.Create(nil);

  FIBTransaction := TpFIBTransaction.Create(nil);
  FIBDatabase.DefaultTransaction := FIBTransaction;
  FIBDatabase.DefaultUpdateTransaction := FIBTransaction;

  FIBQuery := TpFIBQuery.Create(nil);
  FIBQuery.Database := FIBDatabase;
  FIBQuery.Options := FIBQuery.Options + [qoStartTransaction];
  FIBQuery.GoToFirstRecordOnExecute := false;

  FIBQueryCurs := TpFIBQuery.Create(nil);
  FIBQueryCurs.Database := FIBDatabase;
  FIBQueryCurs.Options := FIBQueryCurs.Options + [qoStartTransaction];
  FIBQueryCurs.GoToFirstRecordOnExecute := true;

  FIBQueryCursOut := TpFIBQuery.Create(nil);
  FIBQueryCursOut.Database := FIBDatabase;
  FIBQueryCursOut.Options := FIBQueryCursOut.Options + [qoStartTransaction];
  FIBQueryCursOut.GoToFirstRecordOnExecute := true;
end;

function TMoonDB.DeleteMyPlanets(serial: integer): boolean;
begin
  Result := false;
  try
    FIBQuery.SQL.Text := 'delete from MY_PLANETS where ' +
      '(serial<>' + IntToStr(serial) + ')';
    FIBQuery.ExecQuery;
    Result := true;
  except
  end;
end;

destructor TMoonDB.Destory;
begin
  inherited;

end;

function TMoonDB.GetGameItem(Name: string): TGameItem;
begin
  Result.ID := 0;
  if not FConnected then exit;
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text := 'select g.*, gs.Name as GROUP_NAME from gitems g, gitem_groups gs where ' +
       '(g.GROUP_ID=gs.ID) and ' +
       '(g.name=''' + Name + ''') ';
    FIBQueryCurs.ExecQuery;

    if Trim(FIBQueryCurs.FieldByName('NAME').AsString) = '' then exit;
    Result.ID := FIBQueryCurs.FieldByName('ID').AsInteger;
    Result.GroupName := FIBQueryCurs.FieldByName('GROUP_NAME').AsString;
    Result.Name := FIBQueryCurs.FieldByName('NAME').AsString;

    FIBQueryCurs.Close;
  except
  end;
end;

function TMoonDB.intGetBuildingRes(name: string; level: integer): TGameRes;
begin
  Result.Clear;
  if not FConnected then exit;
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text := 'select * from BUILDING_BOOK where ' +
       '(id=' + IntToStr(GetGameItemID(name)) + ') and ' +
       '(build_level=' + IntToStr(level) + ')';
    FIBQueryCurs.ExecQuery;

    if FIBQueryCurs.FieldByName('ID').AsString = '' then exit;

    Result.Metal := FIBQueryCurs.FieldByName('BUILD_ME').AsInt64;
    Result.Crystal := FIBQueryCurs.FieldByName('BUILD_CRY').AsInt64;
    Result.Deiterium := FIBQueryCurs.FieldByName('BUILD_DEI').AsInt64;

    FIBQueryCurs.Close;
  except
  end;
end;

function TMoonDB.GetBuildingKoef(name: string): Extended;
var
 k,
 k1,
 k2,
 k3: extended;
begin
  Result := 2;
  k1 := 0;
  k2 := 0;
  k3 := 0;
  if not FConnected then exit;
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text :=
      'select B1.ID, B1.BUILD_LEVEL, B2.BUILD_LEVEL, B1.BUILD_ME MM, B2.BUILD_ME NM, B1.BUILD_CRY MC, B2.BUILD_CRY NC, ' +
      '       B1.BUILD_DEI MD, B2.BUILD_DEI ND ' +
      'from BUILDING_BOOK B1 ' +
      'left join BUILDING_BOOK B2 on (B2.ID = B1.ID and B2.BUILD_LEVEL = B1.BUILD_LEVEL - 1) ' +
      'where (B2.ID is not null) and ' +
      '      (B1.ID = 1) ' +
      'order by B1.BUILD_LEVEL ' +
      'rows 1';
    FIBQueryCurs.ExecQuery;

    if FIBQueryCurs.FieldByName('ID').AsString = '' then exit;

    if FIBQueryCurs.FieldByName('NM').AsDouble <> 0 then
      k1 := FIBQueryCurs.FieldByName('MM').AsDouble / FIBQueryCurs.FieldByName('NM').AsDouble;
    if FIBQueryCurs.FieldByName('NC').AsDouble <> 0 then
      k2 := FIBQueryCurs.FieldByName('MC').AsDouble / FIBQueryCurs.FieldByName('NC').AsDouble;
    if FIBQueryCurs.FieldByName('ND').AsDouble <> 0 then
      k3 := FIBQueryCurs.FieldByName('MD').AsDouble / FIBQueryCurs.FieldByName('ND').AsDouble;

    FIBQueryCurs.Close;

    k := max(k1, max(k2, k3));
    if (k > 1.1) and (k < 10) then Result := k;
  except
  end;
end;

function TMoonDB.GetBuildingLength(name: string; level: integer): TDateTime;
begin
  Result := 0;
  if not FConnected then exit;
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text := 'select * from BUILDING_BOOK where ' +
       '(id=' + IntToStr(GetGameItemID(name)) + ') and ' +
       '(build_level=' + IntToStr(level) + ')';
    FIBQueryCurs.ExecQuery;

    if FIBQueryCurs.FieldByName('ID').AsString = '' then exit;

    Result := FIBQueryCurs.FieldByName('BUILD_TIME').AsDateTime;

    FIBQueryCurs.Close;
  except
  end;
end;

function TMoonDB.GetBuildingRes(name: string; level: integer): TGameRes;
var
  k: extended;
begin
  Result := intGetBuildingRes(name, level);
  if Result.Eq0 then
  begin
    Result := intGetBuildingRes(name, 0);
    k := GetBuildingKoef(name);
    Result.Mul(Round(Power(k, level)));
  end;
end;

function TMoonDB.GetGameItem(ID: integer): TGameItem;
begin
  Result.ID := 0;
  if not FConnected then exit;
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text := 'select * from gitem_groups where ' +
       '(id=' + IntToStr(ID) + ') ';
    FIBQueryCurs.ExecQuery;

    if Trim(FIBQueryCurs.FieldByName('NAME').AsString) = '' then exit;
    Result.ID := FIBQueryCurs.FieldByName('ID').AsInteger;

    FIBQueryCurs.Close;
  except
  end;
end;

function TMoonDB.GetGameItemGroupID(GroupName: string): integer;
begin
  Result := 0;
  if not FConnected then exit;
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text := 'select * from gitem_groups where ' +
       '(name=''' + GroupName + ''') ';
    FIBQueryCurs.ExecQuery;

    if Trim(FIBQueryCurs.FieldByName('NAME').AsString) = '' then exit;
    Result := FIBQueryCurs.FieldByName('ID').AsInteger;

    FIBQueryCurs.Close;
  except
  end;
end;

function TMoonDB.GetGameItemID(Name: string): integer;
begin
  Result := GetGameItem(Name).ID;
end;

function TMoonDB.GetImperiumBuildTree: TGameItems;
begin
  SetLength(Result, 0);
  if not FConnected then exit;
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text :=
      'with BT ' +
      'as (select distinct PLANET_BUILD_TYPE TYPE_ID ' +
      '    from MY_PLANETS ' +
      '    where PLANET_BUILD_TYPE is not null) ' +
      'select TT.ID, TT.NAME, max(TT.LVL) LVL ' +
      'from BT, PLANET_TYPE_TECH_TREE(BT.TYPE_ID) TT ' +
      'where TT.GROUP_ID = 3 ' +
      'group by TT.ID, TT.NAME  ';
    FIBQueryCurs.ExecQuery;

    while not FIBQueryCurs.Eof do
    begin
      if Trim(FIBQueryCurs.FieldByName('NAME').AsString) = '' then continue;

      SetLength(Result, length(Result) + 1);
      Result[length(Result) - 1].ID :=
        FIBQueryCurs.FieldByName('ID').AsInteger;
      Result[length(Result) - 1].Name :=
        FIBQueryCurs.FieldByName('NAME').AsString;
      Result[length(Result) - 1].Tag := 3;
      Result[length(Result) - 1].GroupName :=
        'Исследования';
      Result[length(Result) - 1].Level :=
        FIBQueryCurs.FieldByName('LVL').AsInteger;

      FIBQueryCurs.Next;
    end;

    FIBQueryCurs.Close;
  except
  end;
end;

class function TMoonDB.GetInstance: TMoonDB;
begin
  if not Assigned(FInstance) then
    FInstance := TMoonDB.Create;
  Result := FInstance;
end;

function TMoonDB.GetParam(ParamName: string; Default: string = ''): string;
begin
  Result := Default;
  if not FConnected then exit;
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text := 'select * from params where NAME=''' + ParamName + ''' ';
    FIBQueryCurs.ExecQuery;

    Result := FIBQueryCurs.FieldByName('VAL').AsString;

    FIBQueryCurs.Close;
  except
    Result := Default;
  end;
end;

function TMoonDB.GetParam(ParamName: string; Default: int64): int64;
begin
  Result := StrToIntDef(GetParam(ParamName, IntToStr(Default)), Default);
end;

function TMoonDB.GetPlanet(coords: TGameCoords): TEnemyPlanet;
begin
  Result.isEmpty := true;
  if not FConnected then exit;
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text := 'select * from universe where ' +
       '(galaxy=' + IntToStr(coords.Galaxy) + ') and ' +
       '(system=' + IntToStr(coords.System) + ') and ' +
       '(planet=' + IntToStr(coords.Planet) + ')';
    FIBQueryCurs.ExecQuery;

    if Trim(FIBQueryCurs.FieldByName('NAME').AsString) = '' then exit;

    Result.Coords := coords;
    Result.Name := FIBQueryCurs.FieldByName('NAME').AsString;
    Result.UserID := FIBQueryCurs.FieldByName('USER_ID').AsInt64;
    Result.isEmpty := false;

    FIBQueryCurs.Close;
  except
  end;
end;

function TMoonDB.GetPlanetBuildTree(BuildType: integer): TGameItems;
begin
  SetLength(Result, 0);
  if not FConnected then exit;
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text := 'select * from PLANET_TYPE_TECH_TREE(' +
       IntToStr(BuildType) + ') ';
    FIBQueryCurs.ExecQuery;

    while not FIBQueryCurs.Eof do
    begin
      if Trim(FIBQueryCurs.FieldByName('NAME').AsString) = '' then continue;

      SetLength(Result, length(Result) + 1);
      Result[length(Result) - 1].ID :=
        FIBQueryCurs.FieldByName('ID').AsInteger;
      Result[length(Result) - 1].Name :=
        FIBQueryCurs.FieldByName('NAME').AsString;
      Result[length(Result) - 1].Tag :=
        FIBQueryCurs.FieldByName('GROUP_ID').AsInteger;
      Result[length(Result) - 1].GroupName :=
        FIBQueryCurs.FieldByName('GROUP_NAME').AsString;
      Result[length(Result) - 1].Level :=
        FIBQueryCurs.FieldByName('LVL').AsInteger;

      FIBQueryCurs.Next;
    end;

    FIBQueryCurs.Close;
  except
  end;
end;

function TMoonDB.GetPlanetBuildType(PlanetID: integer): integer;
begin
  Result := -1;
  if not FConnected then exit;
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text := 'select * from my_planets where ' +
       '(id=' + IntToStr(PlanetID) + ') ';
    FIBQueryCurs.ExecQuery;

    if FIBQueryCurs.FieldByName('PLANET_BUILD_TYPE').IsNull then exit;

    Result := FIBQueryCurs.FieldByName('PLANET_BUILD_TYPE').AsInteger;

    FIBQueryCurs.Close;
  except
  end;
end;

function TMoonDB.GetShipBook(name: string): TShip;
begin
  Result.Clear;
  if not FConnected then exit;
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text := 'select * from ship_book where ' +
       '(name=''' + name + ''') ';
    FIBQueryCurs.ExecQuery;

    if Trim(FIBQueryCurs.FieldByName('NAME').AsString) = '' then exit;

    Result.ID := FIBQueryCurs.FieldByName('ID').AsInteger;
    Result.Name := FIBQueryCurs.FieldByName('NAME').AsString;
    Result.Tag := FIBQueryCurs.FieldByName('TAG').AsString;
    Result.BuildRes.Metal := FIBQueryCurs.FieldByName('BUILD_ME').AsInt64;
    Result.BuildRes.Crystal := FIBQueryCurs.FieldByName('BUILD_CRY').AsInt64;
    Result.BuildRes.Deiterium := FIBQueryCurs.FieldByName('BUILD_DEI').AsInt64;
    Result.BuildTime := FIBQueryCurs.FieldByName('BUILD_TIME').AsDateTime;

    FIBQueryCurs.Close;
  except
  end;
end;

function TMoonDB.GetStrategyList: TpFIBQuery;
begin
  Result := nil;
  if not Connected then exit;

  try
    FIBQueryCursOut.SQL.Text :=
      'select * from strategy ' +
      'order by id';
    FIBQueryCursOut.ExecQuery;
    Result := FIBQueryCursOut;
  except
  end;
end;

function TMoonDB.GetSystemToScan(cnt: integer): TpFIBQuery;
begin
  Result := nil;
  if not Connected then exit;

  try
    FIBQueryCursOut.SQL.Text :=
      'select first ' + IntToStr(cnt) + ' * from universe where planet = 0 ' +
      'order by LAST_UPDATE, galaxy, system';
    FIBQueryCursOut.ExecQuery;
    Result := FIBQueryCursOut;
  except
  end;
end;

function TMoonDB.GetTechDeps(ItemName: string): TGameItems;
begin
  Result := GetTechDeps(GetGameItemID(ItemName));
end;

function TMoonDB.GetTechDeps(ItemID: integer): TGameItems;
begin
  SetLength(Result, 0);
  if not FConnected then exit;
  try
    FIBQueryCurs.Close;
    FIBQueryCurs.SQL.Text := 'select * from GET_TECH_DEP(' +
       IntToStr(ItemID) + ') ';
    FIBQueryCurs.ExecQuery;

    while not FIBQueryCurs.Eof do
    begin
      if Trim(FIBQueryCurs.FieldByName('ITEM_ID').AsString) = '' then continue;

      SetLength(Result, length(Result) + 1);
      Result[length(Result) - 1].ID :=
        FIBQueryCurs.FieldByName('ITEM_ID').AsInteger;
      Result[length(Result) - 1].Tag :=
        FIBQueryCurs.FieldByName('GROUP_ID').AsInteger;
      Result[length(Result) - 1].GroupName :=
        FIBQueryCurs.FieldByName('GROUP_NAME').AsString;
      Result[length(Result) - 1].Name :=
        FIBQueryCurs.FieldByName('NAME').AsString;
      Result[length(Result) - 1].Level :=
        FIBQueryCurs.FieldByName('LEVEL').AsInteger;

      FIBQueryCurs.Next;
    end;

    FIBQueryCurs.Close;
  except
  end;
end;

function TMoonDB.SetParam(ParamName: string; ParamValue: int64): boolean;
begin
  Result := SetParam(ParamName, IntToStr(ParamValue));
end;

function TMoonDB.UpdateBuildingBook(Buildings: TGameItems): boolean;
var
  i: Integer;
begin
  Result := true;
  for i := 0 to length(Buildings) - 1 do
    Result := Result and UpdateBuildingBook(Buildings[i]);
end;

function TMoonDB.UpdateBuildingBook(Building: TGameItem): boolean;
begin
  Result := false;
  if not FConnected then exit;
  try
    FIBQuery.SQL.Text := 'update or insert into BUILDING_BOOK ' +
      '(id, build_level, tag, build_me, build_cry, build_dei, build_time, last_update) values (' +
      IntToStr(Building.ID) + ',' +
      IntToStr(Building.Level) + ',' +
      IntToStr(Building.Tag) + ',' +
      IntToStr(Building.BuildRes.Metal) + ',' +
      IntToStr(Building.BuildRes.Crystal) + ',' +
      IntToStr(Building.BuildRes.Deiterium) + ',''' +
      DateTimeToStr(Building.BuildTime) + ''',' +
      'current_timestamp) matching (id, build_level)';
    FIBQuery.ExecQuery;

    Result := true;
  except
  end;
end;

function TMoonDB.UpdateMyPlanet(pl: TPlanet; serial: integer = 0): boolean;
begin
  Result := false;
  if not FConnected then exit;
  try
    FIBQuery.SQL.Text := 'update or insert into MY_PLANETS ' +
      '(id, galaxy, system, planet, is_moon, name, serial, last_update) values (' +
      IntToStr(pl.ID) + ',' +
      IntToStr(pl.Coords.Galaxy) + ',' +
      IntToStr(pl.Coords.System) + ',' +
      IntToStr(pl.Coords.Planet) + ',' +
      IntToStr(integer(pl.isMoon)) + ',''' +
      pl.Name + ''',' +
      IntToStr(serial) + ',' +
      'current_timestamp) matching (id)';
    FIBQuery.ExecQuery;

    Result := true;
  except
  end;
end;

function TMoonDB.UpdateMyPlanets(imp: TImperium): boolean;
var
  serial,
  i: Integer;
begin
  Result := true;
  serial := GetTickCount;
  for i := 0 to imp.PlanetsCount - 1 do
    Result := Result and UpdateMyPlanet(imp.GetPlanetI(i), serial);

  Result := Result and DeleteMyPlanets(serial);
end;

function TMoonDB.UpdatePlanetSystem(PlanetSystem: TPlanetSystem): boolean;
var
  i: Integer;
begin
  Result := false;
  if not FConnected then exit;

  if length(PlanetSystem) > 0 then
  try
    ClearPlanetSystem(
      PlanetSystem[0].Coords.Galaxy,
      PlanetSystem[0].Coords.System);

      FIBQuery.SQL.Text := 'update or insert into universe ' +
        '(galaxy, system, planet, last_update) values (' +
        IntToStr(PlanetSystem[0].Coords.Galaxy) + ',' +
        IntToStr(PlanetSystem[0].Coords.System) + ',' +
        '0,' +
        'current_timestamp ) matching (galaxy, system, planet)';
      FIBQuery.ExecQuery;

    for i := 0 to length(PlanetSystem) - 1 do
      if not PlanetSystem[i].isEmpty then
        AddPlanet(PlanetSystem[i]);

    Result := true;
  except
  end;

end;

function TMoonDB.UpdateShipBook(ship: TShip): boolean;
begin
  Result := false;
  if not FConnected then exit;
  try
    FIBQuery.SQL.Text := 'update or insert into SHIP_BOOK ' +
      '(id, name, tag, build_me, build_cry, build_dei, build_time, last_update) values (' +
      IntToStr(ship.ID) + ',''' +
      ship.Name + ''',''' +
      ship.Tag + ''',' +
      IntToStr(ship.BuildRes.Metal) + ',' +
      IntToStr(ship.BuildRes.Crystal) + ',' +
      IntToStr(ship.BuildRes.Deiterium) + ',''' +
      DateTimeToStr(ship.BuildTime) + ''',' +
      'current_timestamp) matching (id)';
    FIBQuery.ExecQuery;

    Result := true;
  except
  end;
end;

function TMoonDB.UpdateTechDeps(techdeps: TGameItems): boolean;
var
  i: Integer;
begin
  Result := true;
  for i := 0 to length(techdeps) - 1 do
    Result := Result and UpdateTechDeps(techdeps[i]);
end;

function TMoonDB.UpdateTechDeps(techdep: TGameItem): boolean;
begin
  Result := false;
  if not FConnected then exit;
  try
    FIBQuery.SQL.Text := 'update or insert into TECH_DEPS ' +
      '(item_id, dep_item_id, level, last_update) values (' +
      IntToStr(techdep.ID) + ',' +
      IntToStr(GetGameItemID(techdep.Name)) + ',' +
      IntToStr(techdep.Level) + ',' +
      'current_timestamp) matching (item_id, dep_item_id)';
    FIBQuery.ExecQuery;

    Result := true;
  except
  end;
end;

function TMoonDB.UpdateTechnologies(tech: TGameItem): boolean;
begin
  Result := false;
  if not FConnected then exit;
  try
    FIBQuery.SQL.Text := 'update or insert into GITEMS ' +
      '(id, group_id, name, last_update) values (' +
      IntToStr(tech.ID) + ',''' +
      IntToStr(GetGameItemGroupID(tech.GroupName)) + ''',''' +
      tech.Name + ''',' +
      'current_timestamp) matching (id)';
    FIBQuery.ExecQuery;

    Result := true;
  except
  end;
end;

function TMoonDB.UpdateTechnologies(techs: TGameItems): boolean;
var
  i: Integer;
begin
  Result := true;
  for i := 0 to length(techs) - 1 do
    Result := Result and UpdateTechnologies(techs[i]);
end;

function TMoonDB.UpdateShipBook(ships: TShips): boolean;
var
  i: Integer;
begin
  Result := true;
  for i := 0 to length(ships) - 1 do
    Result := Result and UpdateShipBook(ships[i]);
end;

function TMoonDB.UpdateUser(user: TUser): boolean;
begin
  Result := false;
  if not FConnected then exit;
  try
    FIBQuery.SQL.Text := 'update or insert into gamers ' +
      '(id, name, alliance, score, last_update) values (' +
      IntToStr(user.ID) + ',''' +
      user.Name + ''',''' +
      user.Alliance + ''',' +
      IntToStr(user.Score) + ',' +
      'current_timestamp) matching (id)';
    FIBQuery.ExecQuery;

    Result := true;
  except
  end;
end;

function TMoonDB.UpdateUsers(users: TUserList): boolean;
var
  i: Integer;
begin
  Result := true;
  for i := 0 to length(users) - 1 do
    Result := Result and UpdateUser(users[i]);
end;

function TMoonDB.SetParam(ParamName, ParamValue: string): boolean;
begin
  Result := false;
  if not FConnected then exit;
  try
    FIBQuery.SQL.Text := 'update or insert into params (id, name, val) values ' +
      '(gen_id(gen_params_id,1), ''' + ParamName + ''', ''' + ParamValue + ''') matching (name)';
    FIBQuery.ExecQuery;

    Commit;
    Result := true;
  except
  end;
end;

end.
