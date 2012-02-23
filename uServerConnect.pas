unit uServerConnect;

interface
uses
  Classes, Windows, SysUtils, StrUtils, Forms,
  clHttpRequest, clGZip, clCookies, clConnection, clDownLoader,
  clTcpClient, clHttp, superobject,
  uLogger, uGameItems, uHTMLParser, uDefs, uProxyCheck;

type
 TMoonServerConnect = class
 private
   const
   MaxLoginFailCount = 4;
   var
   FLoginFailCount: integer;
   FProxy: TProxy;

   clHttpRequest,
   clHttpRequest2,
   clHttpRequest3: TclHttpRequest;
   clHttp: TclHttp;
   clCookieManager: TclCookieManager;

   FServerUrl: string;
   FServerID: integer;
   FUserName,
   FPassword: string;

   function HTTPGetRequest(vURL: string; vHttpRequest: TclHttpRequest; sl: TstringList): boolean;
   function HTTPPostRequest(vURL: string; vHttpRequest: TclHttpRequest; sl: TstringList): boolean;
   function FillForm(vHttpRequest: TclHttpRequest; sl: TstringList): boolean;
   function Active: boolean;

   procedure GetUsersListFromGalaxy(s: string; var users: TUserList);
   procedure locUpdateCurrentFleets(s: string; imp: TImperium);
   procedure locUpdateDarkMatery(s: string; imp: TImperium);

   function FleetMove0(imp: TImperium; CFrom: integer; Fleet: TFleetOrderArr; NextParams: TStringList): boolean;
   function FleetMove1(imp: TImperium; Params: TStringList): boolean;
   function FleetMove2(imp: TImperium; Params: TStringList; FleetOrder: TFleetOrder; TimeThere: extended; res: TGameRes): boolean;
   function FleetMove3(imp: TImperium; Params: TStringList): boolean;

   function BuildFleet0(imp: TImperium; CFrom: integer; ships: TShips): boolean;
   procedure locShipyardGetBuildlist(data: string; pl: TPlanet);

   function BuildBuilding0(imp: TImperium; PlanetID: integer; var BuildingList: TGameItems): boolean;
   function BuildBuilding1(imp: TImperium; PlanetID: integer; BuildingID: integer; BuildCmd: TBuildCmd): boolean;
   procedure locBuildingGetBuildlist(data: string; pl: TPlanet);

   function MakeResearch0(imp: TImperium; PlanetID: integer; var ResearchList: TGameItems): boolean;
   function MakeResearch1(imp: TImperium; PlanetID: integer; ItemID: integer): boolean;
   procedure locUpdateCurrentResearch(data: string; imp: TImperium);
 public
   constructor Create;
   destructor Destroy; override;

   procedure SetProxy(proxy: TProxy);

   function Login(AServerURL: string; AServerID: integer; AUserName, APassword: string): boolean;
   function DeleteMessages(MsgGroupID: integer): boolean;
   function DeletePlanet(PlanetID: integer): boolean;

   function UpdateImperium(var imp: TImperium): boolean;
   function UpdateTechnologies(imp: TImperium; var tech, techdeps: TGameItems): boolean;

   function UpdateUsersInfo(users: TUserList): boolean;
   function UpdateUserInfo(user: TUser): boolean;
   function UpdateGalaxyPlanets(Galaxy, System: integer; MoveDir: TGalaxyMapMove;
               var pls: TPlanetSystem; var users: TUserList): boolean;

   function FleetMove(imp: TImperium; CFrom: integer; CTo: TGameCoords; Fleet: TFleetOrderArr;
               FleetOrder: TFleetOrder; Speed: integer; UpToTimeThere: extended): boolean;

   function UpdateShipyard(imp: TImperium; PlanetID: integer): boolean; overload;
   function UpdateShipyard(imp: TImperium; PlanetID: integer; var ships: TShips): boolean; overload;
   function BuildFleet(imp: TImperium; PlanetID: integer; Fleet: TFleetOrderArr;
               WaitForRes, BuildUpTo: boolean): boolean;

   function UpdateBuildings(imp: TImperium; PlanetID: integer): boolean;
   function BuildBuilding(imp: TImperium; PlanetID: integer; Name: string; ToLevel: integer; ForceDowngrade: boolean = false): boolean;

   function UpdateResearching(imp: TImperium): boolean;
   function MakeResearch(imp: TImperium; PlanetID: integer; Name: string; ToLevel: integer): boolean;
 end;

implementation

{ TMoonServerConnect }

function TMoonServerConnect.Active: boolean;
begin
  Result := FLoginFailCount <= 0;
end;

function TMoonServerConnect.BuildBuilding(imp: TImperium; PlanetID: integer;
  Name: string; ToLevel: integer; ForceDowngrade: boolean): boolean;
var
  bldlist: TGameItems;
  indx,
  i: Integer;
  logt: string;
begin
  Result := false;
  if not Active then exit;

  Result := BuildBuilding0(imp, PlanetID, bldlist);
  if not Result then exit;

  indx := -1;
  for i := 0 to length(bldlist) - 1 do
    if bldlist[i].Name = name then
    begin
      indx := i;
      break;
    end;
  if indx < 0 then exit;

  logt := 'not needed';
  try
    if ToLevel > bldlist[indx].Level then
    begin
      Result := BuildBuilding1(imp, PlanetID, bldlist[indx].ID, bcInsert);
      bldlist[indx].Level := bldlist[indx].Level + 1;
      logt := 'upgraded';
      exit;
    end;

    if (ToLevel < bldlist[indx].Level) and ForceDowngrade then
    begin
      Result := BuildBuilding1(imp, PlanetID, bldlist[indx].ID, bcDestroy);
      bldlist[indx].Level := bldlist[indx].Level - 1;
      logt := 'downgraded';
      exit;
    end;
  finally
    AddLog('build ' + logt + ' ("' +
      name + '"-' +
      IntToStr(ToLevel) + '/' +
      IntToStr(bldlist[indx].Level) + ') pl=' +
      IntToStr(PlanetID), 1);
  end;
end;

function TMoonServerConnect.BuildBuilding0(imp: TImperium; PlanetID: integer;
  var BuildingList: TGameItems): boolean;
var
  sl: TStringList;
  tb: string;
  pl: TPlanet;
  s,
  row,
  col: string;
  cnt: integer;
begin
  Result := false;
  SetLength(BuildingList, 0);
  try
    pl := imp.GetPlanet(PlanetID);
    if pl = nil then exit;

    clHttpRequest2.Assign(clHttpRequest);

    sl := TStringList.Create;
    HTTPGetRequest(
      FServerUrl + 'game.php?page=buildings&mode=&cp=' + IntToStr(PlanetID),
      clHttpRequest2,
      sl);

    locUpdateDarkMatery(sl.Text, imp);
    locBuildingGetBuildlist(sl.Text, pl);

    tb := TParser.CopyFromI(sl.Text, 'id="content"');
    if Pos('id="buildlist"', tb) > 0 then
    begin
      tb := TParser.CopyFromI(tb, '<table');
      tb := TParser.CopyFromI(tb, '</table');
    end;

    tb := TParser.CopyFromI(tb, '<table');
    if tb = '' then exit;

    while true do
    begin
      tb := TParser.CopyFromI(tb, 'Dialog.info(');
      tb := TParser.CopyFromI(tb, 'Dialog.info(');
      row := TParser.CopyToI(tb, 'Dialog.info(');
      s := TParser.CopyToI(row, ')');
      cnt := StrToIntDef(s, 0);
      if row = '' then break;
      if cnt <= 0 then continue;

      // разбор хедера строения
      SetLength(BuildingList, length(BuildingList) + 1);
      BuildingList[length(BuildingList) - 1].ID := cnt;
      BuildingList[length(BuildingList) - 1].Name := TParser.GetTagText(row);
      s := TParser.CopyFromI(row, '(уровень');
      s := Trim(TParser.CopyToI(s, ')'));
      BuildingList[length(BuildingList) - 1].Level := TParser.ResToInt64(s);

      // разбор ресурсов строения
      row := TParser.GetTable(tb);
      row := TParser.GetTag(row, 'tr');
      col := TParser.GetNextColumn(row);
      s := TParser.CopyFromI(col, 'id="metal');
      s := TParser.CopyToI(s, '<');
      s := TParser.GetTagText(s);
      BuildingList[length(BuildingList) - 1].BuildRes.Metal := TParser.ResToInt64(s);

      s := TParser.CopyFromI(col, 'id="crystal');
      s := TParser.CopyToI(s, '<');
      s := TParser.GetTagText(s);
      BuildingList[length(BuildingList) - 1].BuildRes.Crystal := TParser.ResToInt64(s);

      s := TParser.CopyFromI(col, 'id="deuterium');
      s := TParser.CopyToI(s, '<');
      s := TParser.GetTagText(s);
      BuildingList[length(BuildingList) - 1].BuildRes.Deiterium := TParser.ResToInt64(s);

      // разбор продолжительности строительства единицы
      tb := TParser.CopyFromIOrFull(tb, 'Продолжительность:');
      row := TParser.CopyFromI(tb, '<tr');
      row := TParser.CopyToIOrFull(row, '<table');
      col := TParser.GetNextColumn(row);

      // build speed
      col := TParser.GetNextColumn(row);
      col := Trim(TParser.GetTagText(col));
      BuildingList[length(BuildingList) - 1].BuildTime :=
        TParser.StrTimeToDT(col, 0);
    end;

    pl.UpdateFromBuildings(BuildingList);
    TCalc.UpdateBuildingBook(BuildingList);
    Result := true;
  except
  end;
end;

function TMoonServerConnect.BuildBuilding1(imp: TImperium; PlanetID: integer; BuildingID: integer;
  BuildCmd: TBuildCmd): boolean;
var
  sl: TStringList;
  pl: TPlanet;
begin
  Result := false;
  try
    pl := imp.GetPlanet(PlanetID);
    if pl = nil then exit;

    clHttpRequest2.Assign(clHttpRequest);

    sl := TStringList.Create;
    HTTPGetRequest(
      FServerUrl + 'game.php?page=buildings&cmd=' +
        BuildCmdStr[BuildCmd] +
        '&building=' + IntToStr(BuildingID),
      clHttpRequest2,
      sl);

    Result := Pos('page=buildings', clHttp.Url.AbsoluteUri) > 0;

    if Result then
    begin
      locUpdateDarkMatery(sl.Text, imp);
      locBuildingGetBuildlist(sl.Text, pl);
    end;
  except
  end;
end;

function TMoonServerConnect.BuildFleet(imp: TImperium; PlanetID: integer;
  Fleet: TFleetOrderArr; WaitForRes, BuildUpTo: boolean): boolean;
var
  ships: TShips;
  res: TGameRes;
  pl: TPlanet;
  i: Integer;
  j: Integer;
begin
  Result := false;
  if not Active then exit;

  pl := imp.GetPlanet(PlanetID);
  if pl = nil then exit;

  Result := UpdateShipyard(imp, PlanetID, ships);
  if not Result then exit;

  // надо построить до определенного количества
  if BuildUpTo then
  begin
    for i := length(Fleet) - 1 downto 0 do
    begin
      // sub current ships count
      Fleet[i].Value :=
        Fleet[i].Value - pl.GetShipsCount(Fleet[i].Name, true, true);

      // delete if less then 1
      if Fleet[i].Value < 1 then
      begin
        for j := i to length(Fleet) - 2 do
          Fleet[j] := Fleet[j + 1];
        SetLength(Fleet, length(Fleet) - 1);
      end;
    end;
  end;
  if length(Fleet) = 0 then exit;

  //  ждать / не ждать ресурсы - и если чего резать флот
  res := TCalc.ShipsBuildCost(Fleet);
  if (WaitForRes) and (not pl.HaveResources(res)) then exit;
  if (not WaitForRes) and (not pl.HaveResources(res)) then
    TCalc.CutFleet(Fleet, pl.CurRes); //!!!!!!!!!!!!!!

  // сборка формы постройки
  if length(Fleet) = 0 then exit;

  for j := 0 to length(ships) - 1 do
    ships[j].Count := 0;

  for i := 0 to length(Fleet) - 1 do
    for j := 0 to length(ships) - 1 do
      if Fleet[i].Name = ships[j].Name then
        ships[j].Count := Fleet[i].Value;

  // отправка формы постройки
  Result := BuildFleet0(imp, PlanetID, ships);

  AddLog('fleet build ok (' +
    IntToStr(length(Fleet)) + ') pl=' +
    IntToStr(PlanetID), 1);
end;

function TMoonServerConnect.BuildFleet0(imp: TImperium; CFrom: integer; ships: TShips): boolean;
var
  sl: TStringList;
  tb: string;
  i: Integer;
  pl: TPlanet;
begin
  Result := false;
  if length(ships) = 0 then exit;
  try
    pl := imp.GetPlanet(CFrom);
    if pl = nil then exit;

    clHttpRequest2.Assign(clHttpRequest);
    for i := 0 to length(ships) - 1 do
      clHttpRequest2.AddFormField(
        ships[i].Tag,
        IntToStr(ships[i].Count));

    sl := TStringList.Create;
    HTTPPostRequest(
      FServerUrl + 'game.php?page=buildings&mode=fleet&cp=' + IntToStr(CFrom),
      clHttpRequest2,
      sl);

    locShipyardGetBuildlist(sl.Text, pl);

    tb := TParser.CopyFromI(sl.Text, 'id="content"');
    tb := TParser.GetTable(tb);

    Result := tb <> '';
  except
  end;
end;

procedure TMoonServerConnect.locBuildingGetBuildlist(data: string; pl: TPlanet);
var
  tb,
  row,
  col,
  s,
  s1,
  taga,
  name,
  level,
  id: string;
  bld: TGameItems;
begin
  SetLength(bld, 0);
  tb := TParser.CopyFromI(data, 'id="content"');
  if Pos('id="buildlist"', tb) = 0 then exit;

  tb := TParser.CopyFromI(tb, 'id="buildlist"');
  tb := TParser.GetTable(tb);
  if tb = '' then exit;
  while true do
  begin
    row := TParser.GetNextRow(tb);
    if row = '' then break;

    col := TParser.GetNextColumn(row);
    s := TParser.CopyFromI(col, '.:');
    taga := TParser.GetTagA(s);
    id := TParser.GetTagAttrib(taga, 'href');
    id := TParser.URLDecode(id);
    id := TParser.GetURLParam(id, 'building');
    if taga <> '' then
    begin
      s := TParser.GetTagText(taga);
    end
    else
    begin
      s := TParser.CopyToI(s, '<');
    end;
    s := Trim(s);
    // name   "Рудник кристалла 51"
    s1 := ReverseString(s);
    level := TParser.CopyToI(s1, ' ');
    level := ReverseString(level);
    s1 := Copy(s1, length(level) + 1, length(s1));
    name := Trim(ReverseString(s1));

    SetLength(bld, length(bld) + 1);
    bld[length(bld) - 1].Name := name;
    bld[length(bld) - 1].ID := StrToIntDef(id, 0);
    bld[length(bld) - 1].Level := StrToIntDef(level, 0);

    col := TParser.GetNextColumn(row);
    s := TParser.GetTag(col, 'span');
    s := TParser.GetTagText(s);
    // datetime  "07. Feb 2012, 14:42:59"
    bld[length(bld) - 1].BuildTime :=
      TParser.StrDateTimeToDT(s, 0);
  end;

  pl.SetBuildingBuilds(bld);
end;

procedure TMoonServerConnect.locShipyardGetBuildlist(data: string; pl: TPlanet);
var
  tb: string;
  TimeToBuilt: TDateTime;
  obj: ISuperObject;
  arr2: TSuperArray;
  arr: TSuperArray;
  row: string;
  i: Integer;
  s: string;
  Fleet: TFleetOrderArr;
begin
  tb := TParser.CopyFromI(data, 'value="Строить"');
  tb := TParser.GetTagText(TParser.GetTag(tb, 'script'));
  tb := AnsiReplaceStr(tb, ''#9'', ' ');
  row := TParser.CopyFromI(tb, 'data');
  row := Trim(TParser.CopyFromI(row, '='));
  row := Trim(TParser.CopyToI(row, ';'));
  TimeToBuilt := 0;
  if (row <> '') and (row <> '[]') then
  begin
    obj := TSuperObject.ParseString(PWideChar(row), false);
    if obj <> nil then
    begin
      s := SOAsString(obj, 'pretty_time_b_hangar');
      TimeToBuilt := TParser.StrTimeToDT(s, 0);
      arr := obj.A['Queue'];
      if arr <> nil then
        for i := 0 to arr.Length - 1 do
        begin
          arr2 := arr.O[i].AsArray;
          if (arr2 <> nil) and (arr2.Length > 0) then
          begin
            SetLength(Fleet, length(Fleet) + 1);
            if arr2.Length >= 1 then
              Fleet[length(Fleet) - 1].Name := arr2.O[0].AsString;
            if arr2.Length >= 2 then
              Fleet[length(Fleet) - 1].Value := StrToIntDef(arr2.O[1].AsString, 0);
          end;
        end;
    end;
  end;
  pl.SetBuildingShips(Fleet, TimeToBuilt);
end;

constructor TMoonServerConnect.Create;
begin
  inherited;

  clHttpRequest := TclHttpRequest.Create(Nil);
  clHttpRequest.Header.AcceptEncoding := 'gzip,deflate,sdch';
  clHttpRequest.Header.AcceptCharSet := 'windows-1251,utf-8;q=0.7,*;q=0.3';
  clHttpRequest.Header.AcceptLanguage := 'ru-RU,ru;q=0.8,en-US;q=0.6,en;q=0.4';
//  clHttpRequest.Header.Referer := 'http://agame.varena.info/index.php';
  clHttpRequest.Header.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/535.1 (KHTML, like Gecko) Chrome/14.0.835.186 Safari/535.1';

  clHttpRequest2 := TclHttpRequest.Create(Nil);
  clHttpRequest2.Assign(clHttpRequest);

  clHttpRequest3 := TclHttpRequest.Create(Nil);
  clHttpRequest3.Assign(clHttpRequest);

  clHttp := TclHttp.Create(Nil);
  clHttp.KeepConnection := true;
  clHTTP.Request := clHttpRequest;

  clCookieManager := TclCookieManager.Create(Nil);
  clHttp.CookieManager := clCookieManager;

  FLoginFailCount := -1;
end;

function TMoonServerConnect.DeleteMessages(MsgGroupID: integer): boolean;
var
  sl: TStringList;
begin
  Result := false;
  if not Active then exit;
  try
    if MsgGroupID = 0 then MsgGroupID := 100;

    clHttpRequest2.Assign(clHttpRequest);
    clHttpRequest2.AddFormField('deletemessages', 'deletetypeall');

    sl := TStringList.Create;
    HTTPPostRequest(
      FServerUrl + 'game.php?page=messages&mode=delMessages&messcat=' +
        IntToStr(MsgGroupID)+ '&ajax=1',
      clHttpRequest2,
      sl);

    Result := Pos('page=messages', clHttp.Url.Urlpath) > 0;
    AddLog('delete messages(' + IntToStr(MsgGroupID) + '). valid=' + BoolToStr(Result, true), 5)
  except
  end;
end;

function TMoonServerConnect.DeletePlanet(PlanetID: integer): boolean;
var
  sl: TStringList;
  obj: ISuperObject;
begin
  Result := false;
  if not Active then exit;
  try
    clHttpRequest2.Assign(clHttpRequest);
    sl := TStringList.Create;
    HTTPGetRequest(
      FServerUrl + 'game.php?page=overview&mode=&cp=' + IntToStr(PlanetID),
      clHttpRequest2,
      sl);

    clHttpRequest2.Assign(clHttpRequest);
    clHttpRequest2.Header.ExtraFields.Add('X-Requested-With:XMLHttpRequest');
    clHttpRequest2.Header.Referer :=
      FServerUrl + 'game.php?page=overview&mode=&cp=' + IntToStr(PlanetID);
    HTTPPostRequest(
      FServerUrl + 'ajax.php?action=deleteplanet&lang=ru&password=' + FPassword,
      clHttpRequest2,
      sl);

    obj := TSuperObject.ParseString(PWideChar(sl.Text), false);
    if obj = nil then exit;

    Result := LowerCase(SOAsString(obj, 'ok')) = 'true';

    sl.Free;

    AddLog('planet(' + IntToStr(PlanetID) + ') deleted: ' + BoolToStr(Result, true), 1);
  except
  end;
end;

destructor TMoonServerConnect.Destroy;
begin
  inherited;

end;

function TMoonServerConnect.FillForm(vHttpRequest: TclHttpRequest;
  sl: TstringList): boolean;
var
 i: integer;
begin
  Result := false;
  try
    for i := 0 to sl.Count - 1 do
      clHttpRequest2.AddFormField(sl.Names[i], sl.ValueFromIndex[i]);

    Result := true;
  except
  end;
end;

function TMoonServerConnect.FleetMove(imp: TImperium; CFrom: integer; CTo: TGameCoords; Fleet: TFleetOrderArr;
  FleetOrder: TFleetOrder; Speed: integer; UpToTimeThere: extended): boolean;
var
 res: TGameRes;
 NextParams: TStringList;
 pfrom: TPlanet;
begin
  Result := false;
  if not Active then exit;
  try
    pfrom := imp.GetPlanet(CFrom);
    if pfrom = nil then exit;

    NextParams := TStringList.Create;
    NextParams.Values['galaxy'] := IntToStr(pfrom.Coords.Galaxy);
    NextParams.Values['system'] := IntToStr(pfrom.Coords.System);
    NextParams.Values['planet'] := IntToStr(pfrom.Coords.Planet);
    NextParams.Values['planet_type'] := '1';
    NextParams.Values['mission'] := '0';
    NextParams.Values['target_mission'] := '0';

    if not FleetMove0(imp, CFrom, Fleet, NextParams) then exit;
    if not FleetMove1(imp, NextParams) then exit;

    NextParams.Values['galaxy'] := IntToStr(CTo.Galaxy);
    NextParams.Values['system'] := IntToStr(CTo.System);
    NextParams.Values['planet'] := IntToStr(CTo.Planet);
    NextParams.Values['planettype'] := IntToStr(integer(CTo.PlType));
    NextParams.Values['speed'] := IntToStr(Speed);

    res.Clear;
    if not FleetMove2(imp, NextParams, FleetOrder, UpToTimeThere, res) then exit;

    if not FleetMove3(imp, NextParams) then exit;

    AddLog('fleet move ok (' +
      IntToStr(length(Fleet)) + ') ' +
      IntToStr(CFrom) + '>>' + CTo.ToStr, 1);
    Result := true;
  except
  end;
end;

function TMoonServerConnect.FleetMove0(imp: TImperium; CFrom: integer; Fleet: TFleetOrderArr;
  NextParams: TStringList): boolean;
var
 sl: TStringList;
 tb,
 row,
 taga,
 tagi: string;
 i,
 cnt: integer;
begin
  Result := false;
  try
    clHttpRequest2.Assign(clHttpRequest);

    sl := TStringList.Create;
    HTTPGetRequest(
      FServerUrl + 'game.php?page=fleet&mode=&cp=' + IntToStr(CFrom),
      clHttpRequest2,
      sl);

    tb := TParser.CopyFromI(sl.Text, 'id="content"');
    tb := TParser.GetTable(tb);
    locUpdateCurrentFleets(tb, imp);

    NextParams.Values['maxepedition'] := IntToStr(imp.MaxExpedition);
    NextParams.Values['curepedition'] := IntToStr(imp.CurExpedition);
    if (imp.MaxFleets < 1) and (imp.MaxExpedition < 1) then exit;

    tb := TParser.CopyFromI(sl.Text, 'id="content"');
    tb := TParser.CopyFromI(tb, '</table');
    tb := TParser.GetTable(tb);
    if tb = '' then exit;

    while true do
    begin
      row := TParser.GetNextRow(tb);
      if row = '' then break;

      taga := TParser.GetTagA(row);
      taga := TParser.GetTagText(taga);
      if taga <> '' then
      begin
        tagi := TParser.GetSingleTag(row, 'input');
        tagi := TParser.GetTagAttrib(tagi, 'name');
        if tagi <> '' then
        begin
          cnt := 0;
          for i := 0 to length(Fleet) - 1 do
            if Fleet[i].Name = taga then
              cnt := Fleet[i].Value;

          NextParams.Values[tagi] := IntToStr(cnt);
        end;
      end;
    end;

    Result := true;
  except
  end;
end;

function TMoonServerConnect.FleetMove1(imp: TImperium; Params: TStringList): boolean;
var
  sl: TStringList;
  tb,
  s: string;
begin
  Result := false;
  try
    clHttpRequest2.Assign(clHttpRequest);
    FillForm(clHttpRequest2, Params);

    sl := TStringList.Create;
    HTTPPostRequest(
      FServerUrl + 'game.php?page=fleet1',
      clHttpRequest2,
      sl);

    tb := TParser.CopyFromI(sl.Text, 'id="resources"');
    tb := TParser.CopyToIOrFull(tb, 'id="content"');
    tb := TParser.CopyFromI(tb, '<form');

    Params.Clear;
    while true do
    begin
      s := TParser.GetSingleTag(tb, 'input');
      if s = '' then break;
      Params.Values[TParser.GetTagAttrib(s, 'name')] :=
        TParser.GetTagAttrib(s, 'value');
      tb := TParser.CopyFromI(tb, '<input');
    end;

    Result := true;
  except
  end;
end;


function TMoonServerConnect.FleetMove2(imp: TImperium; Params: TStringList;
  FleetOrder: TFleetOrder; TimeThere: extended; res: TGameRes): boolean;
var
  sl: TStringList;
  tb,
  s: string;
  idx: integer;
  v: integer;
  e: extended;
begin
  Result := false;
  try
    clHttpRequest2.Assign(clHttpRequest);
    FillForm(clHttpRequest2, Params);

    sl := TStringList.Create;
    HTTPPostRequest(
      FServerUrl + 'game.php?page=fleet2',
      clHttpRequest2,
      sl);

    Params.SaveToFile(ExtractFilePath(Application.ExeName) + 'fleet2.txt');
    sl.SaveToFile(ExtractFilePath(Application.ExeName) + 'fleet2.html');

    tb := TParser.CopyFromI(sl.Text, 'id="resources"');
    tb := TParser.CopyToIOrFull(tb, 'id="content"');
    tb := TParser.CopyFromI(tb, '<form');

    Params.Clear;
    while true do
    begin
      s := TParser.GetSingleTag(tb, 'input');
      if s = '' then break;
      Params.Values[TParser.GetTagAttrib(s, 'name')] :=
        TParser.GetTagAttrib(s, 'value');
      tb := TParser.CopyFromI(tb, '<input');
    end;

    Params.Values['metal'] := IntToStr(res.Metal);
    Params.Values['crystal'] := IntToStr(res.Crystal);
    Params.Values['deuterium'] := IntToStr(res.Deiterium);

    // holdingtime
    tb := TParser.CopyFromI(sl.Text, 'id="content"');
    tb := TParser.CopyFromI(tb, 'name="holdingtime"');
    tb := TParser.CopyToI(tb, '</select');
    idx := 0;

    while true do
    begin
      s := TParser.GetTag(tb, 'option');
      if s = '' then break;
      v := StrToIntDef(TParser.GetTagAttrib(s, 'value'), 0);
      e := TParser.StrToExtended(TParser.GetTagText(s), 0);

      if (v > 0) and (abs(e) > 1E-20) then
      begin
        if e < TimeThere then idx := v;
      end;

      tb := TParser.CopyFromI(tb, '<option');
    end;
    if idx <> 0 then
      Params.Values['holdingtime'] := IntToStr(idx);

    // can we FleetOrder?
    idx := 0;
    tb := TParser.CopyFromI(sl.Text, 'id="content"');
    tb := TParser.CopyFromI(tb, '<table');
    tb := TParser.GetTable(tb);

    while true do
    begin
      s := TParser.GetSingleTag(tb, 'input');
      if s = '' then break;

      if (TParser.GetTagAttrib(s, 'name') = 'mission') and
         (StrToIntDef(TParser.GetTagAttrib(s, 'value'), -1) = integer(FleetOrder))
      then
        idx := integer(FleetOrder);

      tb := TParser.CopyFromI(tb, '<input');
    end;
    if idx = 0 then exit;

    Params.Values['mission'] := IntToStr(idx);

    Result := true;
  except
  end;
end;

function TMoonServerConnect.FleetMove3(imp: TImperium;
  Params: TStringList): boolean;
var
  sl: TStringList;
  tb: string;
begin
  Result := false;
  try
    clHttpRequest2.Assign(clHttpRequest);
    FillForm(clHttpRequest2, Params);

    sl := TStringList.Create;
    HTTPPostRequest(
      FServerUrl + 'game.php?page=fleet3',
      clHttpRequest2,
      sl);

    Params.Clear;
    tb := TParser.CopyFromI(sl.Text, 'id="content"');

    Result := true;
  except
  end;
end;

procedure TMoonServerConnect.GetUsersListFromGalaxy(s: string;
  var users: TUserList);
var
 s1: string;
begin
  while s <> '' do
  begin
    s := TParser.CopyFromI(s, 'Dialog.Playercard');
    s := TParser.CopyFromI(s, '(');
    s1 := TParser.CopyToI(s, ')');
    s1 := ReplaceStr(s1, '&quot;', '');
    if s1 <> '' then
    begin
      SetLength(users, length(users) + 1);
      users[length(users) - 1] := TUser.Create;
      users[length(users) - 1].ID := StrToInt64Def(
        TParser.CopyToI(s, ','), 0);
      users[length(users) - 1].Name := trim(
        TParser.CopyFromI(s1, ','));
    end;
  end;
end;

function TMoonServerConnect.Login(AServerURL: string; AServerID: integer; AUserName, APassword: string): boolean;
var
 sl: TStringList;
begin
  Result := false;
  FServerUrl := AServerURL;
  FServerID := AServerID;
  FUserName := AUserName;
  FPassword := APassword;

  if FLoginFailCount > MaxLoginFailCount then
  begin
    AddLog('login "' + FUserName + '": Max login fails ' + IntToStr(FLoginFailCount));
    exit;
  end;

  try
    if FProxy.Active then
    begin
      clHttp.ProxySettings.Server := FProxy.IP;
      clHttp.ProxySettings.Port := StrToIntDef(FProxy.Port, 80);
    end;

    clHttpRequest3.Assign(clHttpRequest);
    clHttpRequest3.AddFormField('uni', IntToStr(FServerID));
    clHttpRequest3.AddFormField('username', AUserName);
    clHttpRequest3.AddFormField('password', APassword);
    sl := TStringList.Create;
    HTTPPostRequest(
      FServerUrl + 'index.php?page=login',
      clHttpRequest3,
      sl);
    sl.Free;

    Result := Pos('game.php', clHttp.Url.Urlpath) > 0;
    if Result then
      FLoginFailCount := 0
    else
      FLoginFailCount := FLoginFailCount + 1;

    AddLog('login "' + FUserName + '": ' + BoolToStr(Result, true))
  except
  end;
end;

function TMoonServerConnect.MakeResearch(imp: TImperium; PlanetID: integer;
  Name: string; ToLevel: integer): boolean;
var
  reslist: TGameItems;
  i,
  indx: integer;
begin
  Result := false;
  if not Active then exit;

  Result := MakeResearch0(imp, PlanetID, reslist);
  if not Result then exit;

  indx := -1;
  for i := 0 to length(reslist) - 1 do
    if reslist[i].Name = name then
    begin
      indx := i;
      break;
    end;
  if indx < 0 then exit;
  if ToLevel <= reslist[indx].Level then
  begin
    AddLog('research not needed ("' +
      name + '"-' + IntToStr(ToLevel));
    exit;
  end;

  Result := MakeResearch1(imp, PlanetID, reslist[indx].ID);

  AddLog('research ("' +
    name + '"-' +
    IntToStr(ToLevel) + '/' +
    IntToStr(reslist[indx].Level) + ') pl=' +
    IntToStr(PlanetID), 1);
end;

function TMoonServerConnect.MakeResearch0(imp: TImperium; PlanetID: integer;
  var ResearchList: TGameItems): boolean;
var
  sl: TStringList;
  tb: string;
  s,
  row,
  col: string;
  cnt: integer;
begin
  Result := false;
  SetLength(ResearchList, 0);
  try
    clHttpRequest2.Assign(clHttpRequest);

    sl := TStringList.Create;
    HTTPGetRequest(
      FServerUrl + 'game.php?page=buildings&mode=research&cp=' + IntToStr(PlanetID),
      clHttpRequest2,
      sl);

    locUpdateDarkMatery(sl.Text, imp);
    locUpdateCurrentResearch(sl.Text, imp);

    tb := TParser.CopyFromI(sl.Text, 'id="content"');
    if Pos('id="buildlist"', tb) > 0 then
    begin
      tb := TParser.CopyFromI(tb, '<table');
      tb := TParser.CopyFromI(tb, '</table');
    end;

    tb := TParser.CopyFromI(tb, '<table');
    if tb = '' then exit;

    while true do
    begin
      tb := TParser.CopyFromI(tb, 'Dialog.info(');
      tb := TParser.CopyFromI(tb, 'Dialog.info(');
      row := TParser.CopyToI(tb, 'Dialog.info(');
      s := TParser.CopyToI(row, ')');
      cnt := StrToIntDef(s, 0);
      if row = '' then break;
      if cnt <= 0 then continue;

      // разбор хедера строения
      SetLength(ResearchList, length(ResearchList) + 1);
      ResearchList[length(ResearchList) - 1].ID := cnt;
      ResearchList[length(ResearchList) - 1].Name := TParser.GetTagText(row);
      s := TParser.CopyFromI(row, '(уровень');
      s := Trim(TParser.CopyToI(s, ')'));
      ResearchList[length(ResearchList) - 1].Level := TParser.ResToInt64(s);

      // разбор ресурсов строения
      row := TParser.GetTable(tb);
      row := TParser.GetTag(row, 'tr');
      col := TParser.GetNextColumn(row);
      s := TParser.CopyFromI(col, 'id="metal');
      s := TParser.CopyToI(s, '<');
      s := TParser.GetTagText(s);
      ResearchList[length(ResearchList) - 1].BuildRes.Metal := TParser.ResToInt64(s);

      s := TParser.CopyFromI(col, 'id="crystal');
      s := TParser.CopyToI(s, '<');
      s := TParser.GetTagText(s);
      ResearchList[length(ResearchList) - 1].BuildRes.Crystal := TParser.ResToInt64(s);

      s := TParser.CopyFromI(col, 'id="deuterium');
      s := TParser.CopyToI(s, '<');
      s := TParser.GetTagText(s);
      ResearchList[length(ResearchList) - 1].BuildRes.Deiterium := TParser.ResToInt64(s);

      // разбор продолжительности строительства единицы
      tb := TParser.CopyFromIOrFull(tb, 'Продолжительность:');
      row := TParser.CopyFromI(tb, '<tr');
      row := TParser.CopyToIOrFull(row, '<table');
      col := TParser.GetNextColumn(row);

      // build speed
      col := TParser.GetNextColumn(row);
      col := Trim(TParser.GetTagText(col));
      ResearchList[length(ResearchList) - 1].BuildTime :=
        TParser.StrTimeToDT(col, 0);
    end;

    imp.UpdateResearchList(ResearchList);
    TCalc.UpdateBuildingBook(ResearchList);
    Result := true;
  except
  end;
end;

function TMoonServerConnect.MakeResearch1(imp: TImperium; PlanetID,
  ItemID: integer): boolean;
var
  sl: TStringList;
begin
  Result := false;
  try
    clHttpRequest2.Assign(clHttpRequest);
    sl := TStringList.Create;
    HTTPGetRequest(
      FServerUrl + 'game.php?page=buildings&mode=research&cmd=insert' +
        '&tech=' + IntToStr(ItemID),
      clHttpRequest2,
      sl);

    Result := Pos('page=buildings', clHttp.Url.AbsoluteUri) > 0;

    if Result then
    begin
      locUpdateDarkMatery(sl.Text, imp);
      locUpdateCurrentResearch(sl.Text, imp);
    end;
  except
  end;
end;

procedure TMoonServerConnect.SetProxy(proxy: TProxy);
begin
  FProxy := proxy;
end;

function TMoonServerConnect.UpdateShipyard(imp: TImperium; PlanetID: integer; var ships: TShips): boolean;
var
 sl: TStringList;
 tb,
 row,
 col,
 s: string;
 cnt: integer;
 pl: TPlanet;
begin
  Result := false;
  if not Active then exit;
  try
    pl := imp.GetPlanet(PlanetID);
    if pl = nil then exit;

    clHttpRequest2.Assign(clHttpRequest);

    sl := TStringList.Create;
    HTTPGetRequest(
      FServerUrl + 'game.php?page=buildings&mode=fleet&cp=' + IntToStr(PlanetID),
      clHttpRequest2,
      sl);

    locUpdateDarkMatery(sl.Text, imp);
    locShipyardGetBuildlist(sl.Text, pl);

    SetLength(ships, 0);

    tb := TParser.CopyFromI(sl.Text, 'id="content"');
    if Pos('<table', tb) < Pos('<form', tb) then
      tb := TParser.CopyFromI(tb, '<form');

    tb := TParser.CopyFromI(tb, '<form');
    tb := TParser.CopyFromI(tb, '<table');
    if tb = '' then exit;

    while true do
    begin
      tb := TParser.CopyFromI(tb, 'Dialog.info(');
      tb := TParser.CopyFromI(tb, 'Dialog.info(');
      row := TParser.CopyToI(tb, '</span');
      s := TParser.CopyToI(row, ')');
      cnt := StrToIntDef(s, 0);
      if row = '' then break;
      if cnt <= 0 then continue;

      // разбор хедера корабля
      SetLength(ships, length(ships) + 1);
      ships[length(ships) - 1].ID := cnt;
      ships[length(ships) - 1].Name := TParser.GetTagText(row);
      s := TParser.CopyFromI(row, 'Доступно:');
      s := Trim(TParser.CopyToI(s, ')'));
      ships[length(ships) - 1].Count := TParser.ResToInt64(s);

      // разбор ресурсов корабля
      row := TParser.GetTable(tb);
      row := TParser.GetTag(row, 'tr');
      col := TParser.GetNextColumn(row);
      s := TParser.CopyFromI(col, 'id="metal');
      s := TParser.CopyToI(s, '<');
      s := TParser.GetTagText(s);
      ships[length(ships) - 1].BuildRes.Metal := TParser.ResToInt64(s);

      s := TParser.CopyFromI(col, 'id="crystal');
      s := TParser.CopyToI(s, '<');
      s := TParser.GetTagText(s);
      ships[length(ships) - 1].BuildRes.Crystal := TParser.ResToInt64(s);

      s := TParser.CopyFromI(col, 'id="deuterium');
      s := TParser.CopyToI(s, '<');
      s := TParser.GetTagText(s);
      ships[length(ships) - 1].BuildRes.Deiterium := TParser.ResToInt64(s);

      // разбор переменных формы
      col := TParser.GetNextColumn(row);

      s := TParser.GetSingleTag(col, 'input');
      ships[length(ships) - 1].Tag := TParser.GetTagAttrib(s, 'name');

      // разбор продолжительности строительства единицы
      tb := TParser.CopyFromIOrFull(tb, 'Продолжительность:');
      row := TParser.CopyFromI(tb, '<tr');
      row := TParser.CopyToIOrFull(row, '<table');

      // max count
      col := TParser.GetNextColumn(row);
      col := TParser.GetTag(col, 'span');
      col := Trim(TParser.GetTagText(col));
      ships[length(ships) - 1].BuildMaxCount :=
        TParser.ResToInt64(col);

      // build speed
      col := TParser.GetNextColumn(row);
      col := Trim(TParser.GetTagText(col));
      ships[length(ships) - 1].BuildTime :=
        TParser.StrTimeToDT(col, 0);
    end;

    pl.UpdateFromShipYard(ships);
    TCalc.UpdateShipBook(ships);

    Result := true;
  except
  end;
end;

function TMoonServerConnect.UpdateTechnologies(imp: TImperium; var tech, techdeps: TGameItems): boolean;
var
  sl: TStringList;
  s,
  sp,
  rname,
  row,
  col,
  tb,
  taga: string;
begin
  Result := false;
  if not Active then exit;
  try
    SetLength(tech, 0);
    SetLength(techdeps, 0);

    clHttpRequest2.Assign(clHttpRequest);
    sl := TStringList.Create;
    HTTPGetRequest(
      FServerUrl + 'game.php?page=techtree',
      clHttpRequest2,
      sl);

    locUpdateDarkMatery(sl.Text, imp);

    tb := TParser.CopyFromI(sl.Text, 'id="content"');
    tb := TParser.GetTable(tb);

    rname := '';
    while true do
    begin
      row := TParser.GetNextRow(tb);
      if row = '' then break;

      s := TParser.GetTagText(
        TParser.GetTag(row, 'th'));
      if s <> '' then
      begin
        rname := s;
        continue;
      end;

      // name
      col := TParser.GetNextColumn(row);
      taga := TParser.GetTagA(col);
      s := TParser.GetTagAttrib(taga, 'onclick');
      s := TParser.CopyFromI(s, '(');
      s := TParser.CopyToI(s, ')');
      if StrToIntDef(s, 0) = 0 then continue;

      SetLength(tech, length(tech) + 1);
      tech[length(tech) - 1].GroupName := rname;
      tech[length(tech) - 1].ID := StrToIntDef(s, 0);

      s := TParser.GetTagText(taga);
      tech[length(tech) - 1].Name := s;

      // dependencies
      col := TParser.GetNextColumn(row);
      while true do
      begin
        sp := TParser.GetTag(col, 'span');
        col := TParser.CopyFromI(col, '</span');
        if sp = '' then break;

        sp := Trim(TParser.GetTagText(sp));
        s := TParser.CopyToI(sp, '(');
        if s = '' then continue;

        SetLength(techdeps, length(techdeps) + 1);
        techdeps[length(techdeps) - 1].ID := tech[length(tech) - 1].ID;
        techdeps[length(techdeps) - 1].Name := Trim(s);

        s := TParser.CopyFromI(sp, '(');
        s := TParser.CopyFromI(s, '/');
        s := Trim(TParser.CopyToI(s, ')'));
        techdeps[length(techdeps) - 1].Level := StrToIntDef(s, 0);
      end;
    end;

    Result := true;
    AddLog('cmd init imperium. ', 1);
  except
  end;
end;

procedure TMoonServerConnect.locUpdateCurrentFleets(s: string; imp: TImperium);
var
 s1,
 s2: string;
begin
  s1 := TParser.CopyFromI(s, 'class="transparent"');
  s2 := TParser.GetTagText(s1);
  s2 := ReplaceStr(s2, 'Флоты', '');
  s2 := ReplaceStr(s2, ' ', '');
  TParser.Delim2Ints(s2, '/', imp.CurFleets, imp.MaxFleets);

  s1 := TParser.CopyFromI(s1, 'class="transparent"');
  s2 := TParser.GetTagText(s1);
  s2 := ReplaceStr(s2, 'Экспедиции', '');
  s2 := ReplaceStr(s2, ' ', '');
  TParser.Delim2Ints(s2, '/', imp.CurExpedition, imp.MaxExpedition);

  // TODO добавить летящие флоты в империю


end;

procedure TMoonServerConnect.locUpdateCurrentResearch(data: string; imp: TImperium);
var
  tb,
  row,
  col,
  s,
  s1,
  taga,
  name,
  level,
  id: string;
  resch: TGameItems;
begin
  SetLength(resch, 0);
  tb := TParser.CopyFromI(data, 'id="content"');
  if Pos('id="buildlist"', tb) = 0 then exit;

  tb := TParser.CopyFromI(tb, 'id="buildlist"');
  tb := TParser.GetTable(tb);
  if tb = '' then exit;
  while true do
  begin
    row := TParser.GetNextRow(tb);
    if row = '' then break;

    col := TParser.GetNextColumn(row);
    s := TParser.CopyFromI(col, '.:');
    taga := TParser.GetTagA(s);
    if taga <> '' then
    begin
      id := TParser.GetTagAttrib(taga, 'href');
      id := TParser.URLDecode(id);
      id := TParser.GetURLParam(id, 'building');
      s := TParser.GetTagText(taga);
    end
    else
    begin
      id := '0';
      s := TParser.CopyToIOrFull(s, '<');
    end;

    // delete build planet from name
    if Pos('@', s) > 0 then s := TParser.CopyToI(s, '@');
    s := Trim(s);

    // name   "Рудник кристалла 51"
    s1 := ReverseString(s);
    level := TParser.CopyToI(s1, ' ');
    level := ReverseString(level);
    s1 := Copy(s1, length(level) + 1, length(s1));
    name := Trim(ReverseString(s1));

    SetLength(resch, length(resch) + 1);
    resch[length(resch) - 1].Name := name;
    resch[length(resch) - 1].ID := StrToIntDef(id, 0);
    resch[length(resch) - 1].Level := StrToIntDef(level, 0);

    col := TParser.GetNextColumn(row);
    s := TParser.GetTag(col, 'span');
    s := TParser.GetTagText(s);
    // datetime  "07. Feb 2012, 14:42:59"
    resch[length(resch) - 1].BuildTime :=
      TParser.StrDateTimeToDT(s, 0);
  end;

  imp.UpdateResearching(resch);
end;

procedure TMoonServerConnect.locUpdateDarkMatery(s: string; imp: TImperium);
var
 s1,
 s2: string;
begin
  s1 := TParser.CopyFromI(s, 'id="resources"');
  s1 := TParser.CopyToI(s1, '<script');
  s2 := TParser.CopyFromI(s1, 'alt="Тёмная материя"');
  s2 := TParser.CopyToI(s2, '</div');
  s2 := TParser.CopyFromI(s2, '<p');
  s2 := TParser.GetTag(s2, 'p');
  s2 := TParser.GetTagAttrib(s2, 'name');

  imp.DarkMatery := TParser.ResToInt64(s2);
end;

function TMoonServerConnect.UpdateBuildings(imp: TImperium;
  PlanetID: integer): boolean;
var
  BuildingList: TGameItems;
begin
  Result := BuildBuilding0(imp, PlanetID, BuildingList);
end;

function TMoonServerConnect.UpdateGalaxyPlanets(Galaxy, System: integer; MoveDir: TGalaxyMapMove;
  var pls: TPlanetSystem; var users: TUserList): boolean;
var
  sl: TStringList;
  tb,
  row,
  col,
  s,
  taga: string;
  colIndx,
  PlanetPos: integer;
  i: Integer;
begin
  Result := false;
  SetLength(pls, 0);
  SetLength(users, 0);
  if not Active then exit;
  try
    clHttpRequest2.Assign(clHttpRequest);
    if GalaxyMapMoveStr[MoveDir] <> '' then
      clHttpRequest2.AddFormField(GalaxyMapMoveStr[MoveDir], 'dr');
    clHttpRequest2.AddFormField('galaxy', IntToStr(Galaxy));
    clHttpRequest2.AddFormField('system', IntToStr(System));

    sl := TStringList.Create;
    HTTPPostRequest(
      FServerUrl + 'game.php?page=galaxy&mode=1',
      clHttpRequest2,
      sl);

    tb := TParser.CopyFromI(sl.Text, 'id="content"');
    tb := '<table ' + TParser.CopyFromI(tb, 'table569');
    GetUsersListFromGalaxy(tb, users);
    tb := TParser.GetTextWOAttrVal(tb, 'name', '<table');
    tb := TParser.GetTableWoInner(tb);
    TParser.GetNextRow(tb);
    TParser.GetNextRow(tb);

    while true do
    begin
      row := TParser.GetNextRow(tb);
      if row = '' then break;
//      if TParser.GetTag(row, 'th') <> '' then continue;

      col := TParser.GetNextColumn(row);
      taga := TParser.GetTagA(col);
      PlanetPos := StrToIntDef(TParser.GetTagText(taga), 0);
      if PlanetPos = 0 then continue;
      SetLength(pls, length(pls) + 1);
      pls[Length(pls) - 1].Clear;
      pls[Length(pls) - 1].Coords.Galaxy := Galaxy;
      pls[Length(pls) - 1].Coords.System := System;
      pls[Length(pls) - 1].Coords.Planet := PlanetPos;
      pls[Length(pls) - 1].isEmpty := true;

      colIndx := 0;
      while col <> '' do
      begin
        col := TParser.GetNextColumn(row);
        if col = '' then break;

        // имя
        if colIndx = 1 then
        begin
          pls[Length(pls) - 1].Name := TParser.GetTagText(col);
          if pls[Length(pls) - 1].Name <> '' then
            pls[Length(pls) - 1].isEmpty := false;
        end;

        // луна
        if colIndx = 2 then ;

        // поле обломков
        if colIndx = 3 then
        begin
          taga := Trim(TParser.GetTagA(col));
          if taga <> '' then
            pls[Length(pls) - 1].HaveCrashField := true;
        end;

        // игрок
        if colIndx = 4 then
        begin
          s := TParser.GetTagText(TParser.GetTag(col, 'span'));
          if s <> '' then
            for i := 0 to length(users) - 1 do
              if users[i].Name = s then
                pls[Length(pls) - 1].UserID := users[i].ID;
        end;

        // альянс
        if colIndx = 5 then ;

        colIndx := colIndx + 1;
        if colIndx > 5 then break;
      end;
    end;

    sl.Free;

    AddLog('update galaxy planets. [' +
      IntToStr(Galaxy) + ',' + IntToStr(System) + ']', 5);
    Result := true;
  except
  end;
end;

function TMoonServerConnect.UpdateImperium(var imp: TImperium): boolean;
var
  sl: TStringList;
  tb,
  row,
  rname,
  cname,
  col,
  taga,
  s: string;
  PlanetIDs: array of integer;
  PlanetIndx,
  PlanetNum:integer;
  pl: TPlanet;
  serial: cardinal;
begin
  Result := false;
  if not Active then exit;
  if imp = nil then
    imp := TImperium.Create;

  serial := GetTickCount;
  try
    clHttpRequest2.Assign(clHttpRequest);
    sl := TStringList.Create;
    HTTPGetRequest(
      FServerUrl + 'game.php?page=imperium',
      clHttpRequest2,
      sl);

    locUpdateDarkMatery(sl.Text, imp);

    tb := TParser.CopyFromI(sl.Text, 'id="content"');
    tb := TParser.GetTable(tb);

    rname := '';
    SetLength(PlanetIDs, 0);

    while true do
    begin
      row := TParser.GetNextRow(tb);
      if row = '' then break;

      s := TParser.GetTagText(
        TParser.GetTag(row, 'th'));
      if s <> '' then
      begin
        rname := s;
        continue;
      end;

      col := TParser.GetNextColumn(row);
      cname := TParser.GetTagText(col);
      PlanetIndx := 0;
      while col <> '' do
      begin
        col := TParser.GetNextColumn(row);
        if col = '' then break;


        if rname = 'Обзор империи' then
        begin
          if cname = 'Планета' then
          begin
            taga := TParser.GetTagA(col);

            s := TParser.GetTagAttrib(taga, 'href');
            s := TParser.URLDecode(s);
            s := TParser.GetURLParam(s, 'cp');
            PlanetNum := StrToIntDef(s, 0);
            SetLength(PlanetIDs, length(PlanetIDs) + 1);
            PlanetIDs[length(PlanetIDs) - 1] := PlanetNum;
            pl := imp.GetOrCreatePlanet(PlanetNum);
            pl.Coords.Clear;
            pl.serial := serial;
            pl.UpdateDT := Now;

            // is moon?
            s := TParser.GetSingleTag(taga, 'img');
            s :=  TParser.GetTagAttrib(s, 'src');
            if Pos('s_mond.jpg', s) > 0 then
            begin
              pl.Coords.PlType := ptMoon;
              pl.isMoon := true;
            end
            else
            begin
              pl.Coords.PlType := ptPlanet;
              pl.isMoon := false;
            end
          end;

          pl := imp.GetPlanet(PlanetIDs[PlanetIndx]);
          if pl = nil then
          begin
            PlanetIndx := PlanetIndx + 1;
            continue;
          end;

          if cname = 'Название' then
          begin
            pl.Name := TParser.GetTagText(col);
          end;

          if cname = 'Координаты' then
          begin
            taga := TParser.GetTagA(col);
            pl.Coords.FromStr(TParser.GetTagText(taga));
            pl.FinishUpdate;
          end;

          if cname = 'Поля' then
          begin
           TParser.Delim2Ints(
             TParser.GetTagText(col),
             '/', pl.CurFields, pl.MaxFields);
          end;
        end;

        pl := imp.GetPlanet(PlanetIDs[PlanetIndx]);
        if pl = nil then
        begin
          PlanetIndx := PlanetIndx + 1;
          continue;
        end;

        if rname = 'Ресурсы' then
        begin
          if cname = 'Металл' then
            pl.CurRes.Metal :=
              TParser.ResToInt64(TParser.GetTagText(col));
          if cname = 'Кристалл' then
            pl.CurRes.Crystal :=
              TParser.ResToInt64(TParser.GetTagText(col));
          if cname = 'Дейтерий' then
            pl.CurRes.Deiterium :=
              TParser.ResToInt64(TParser.GetTagText(col));
          if cname = 'Энергия' then
           TParser.Delim2Res(
             TParser.GetTagText(col),
             '/', pl.FreeEnergy, pl.Energy);
        end;

        if rname = 'Постройки' then
        begin
          pl.UpdateAttr(
            cname,
            TParser.ResToInt64(TParser.GetTagText(col)));
        end;

        if rname = 'Исследования' then
        begin
          imp.UpdateResearchLevel(
            cname,
            TParser.ResToInt64(TParser.GetTagText(col)));
        end;

        if rname = 'Флот' then
        begin
          pl.UpdateAttr(
            cname,
            TParser.ResToInt64(TParser.GetTagText(col)));
        end;

        if rname = 'Оборона' then
        begin
          pl.UpdateAttr(
            cname,
            TParser.ResToInt64(TParser.GetTagText(col)));
        end;

        PlanetIndx := PlanetIndx + 1;
      end;
    end;

    imp.DeletePlanets(serial);
    imp.Valid := (imp.PlanetsCount > 0) and (imp.ResearchCount > 0);
    if imp.Valid then imp.LastUpdate := Now;

    sl.Free;

    Result := imp.Valid;
    AddLog('update imperium. valid=' + BoolToStr(Result, true) +
      ' pl=' + IntToStr(imp.PlanetsCount) +
      ' moon=' + IntToStr(imp.MoonsCount) +
      ' rsrch=' + IntToStr(imp.ResearchCount), 5);
  except
    imp.Valid := false;
  end;
end;

function TMoonServerConnect.UpdateResearching(imp: TImperium): boolean;
var
  resch: TGameItems;
  i : integer;
begin
  Result := false;
  for i := 0 to  imp.PlanetsCount - 1 do
  begin
    if imp.GetPlanetI(i).GetBuildLevel('Исследовательская лаборатория') > 0 then
      Result := MakeResearch0(imp, imp.GetPlanetI(i).ID, resch);

    break;
  end;
end;

function TMoonServerConnect.UpdateShipyard(imp: TImperium; PlanetID: integer): boolean;
var
 ships: TShips;
begin
  Result := UpdateShipyard(imp, PlanetID, ships);
end;

function TMoonServerConnect.UpdateUserInfo(user: TUser): boolean;
begin
  Result := false;
  if not Active then exit;

end;

function TMoonServerConnect.UpdateUsersInfo(users: TUserList): boolean;
var
  i: integer;
begin
  Result := true;
  for i := 0 to length(users) - 1 do
    Result := Result and UpdateUserInfo(users[i]);
end;

function TMoonServerConnect.HTTPGetRequest(vURL: string; vHttpRequest: TclHttpRequest;
  sl: TstringList): boolean;
var
 i: integer;
begin
  Result := false;
  sl.Clear;
  if not Active then exit;

  for i := 0 to 2 do
  begin
    try
      clHttp.Get(vURL, vHttpRequest, sl);
      if clHttp.StatusCode = 410 then
      begin
        clHttp.Close;
        continue;
      end;

      if Pos('index.php?code=', clHttp.Url.AbsoluteUri) > 0 then
      begin
        if not Login(FServerUrl, FServerID, FUserName, FPassword)
        then exit;
      end;

    except
      clHttp.Close;
      continue;
    end;

    sl.Text := Utf8ToAnsi(RawByteString(sl.Text));

    Result := true;
    break;
  end;
end;

function TMoonServerConnect.HTTPPostRequest(vURL: string; vHttpRequest: TclHttpRequest;
  sl: TstringList): boolean;
var
 i: integer;
begin
  Result := false;
  sl.Clear;
  if not Active then exit;

  for i := 0 to 2 do
  begin
    try
      clHttp.Post(vURL, vHttpRequest, sl);
      if clHttp.StatusCode = 410 then
      begin
        clHttp.Close;
        continue;
      end;

      if Pos('index.php?code=', clHttp.Url.AbsoluteUri) > 0 then
      begin
        if not Login(FServerUrl, FServerID, FUserName, FPassword)
        then exit;
      end;
    except
      clHttp.Close;
      continue;
    end;

    sl.Text := Utf8ToAnsi(RawByteString(sl.Text));

    Result := true;
    break;
  end;
end;


end.
