unit uDefs;

interface
uses
  Classes, SysUtils, DateUtils, SuperObject, uGameItems;

function SOAsString(obj: ISuperObject; name: string): string;
function SOAsIntegerDef(obj: ISuperObject; name: string; default: integer): integer;
function SOAsBooleanDef(obj: ISuperObject; name: string; default: boolean): boolean;
function SOAsShips(obj: ISuperObject; name: string): TFleetOrderArr;

function TimeBetween(dt: TDateTime; FromHour, ToHour: integer): boolean;

implementation

function SOAsString(obj: ISuperObject; name: string): string;
var
 obj1: ISuperObject;
begin
  try
    obj1 := obj[name];
    if obj1 = nil then
      Result := ''
    else
      Result := obj1.AsString;
  except
    Result := '';
  end;
end;

function SOAsIntegerDef(obj: ISuperObject; name: string; default: integer): integer;
var
 obj1: ISuperObject;
begin
  try
    Result := default;
    if obj = nil then exit;

    obj1 := obj[name];
    if obj1 = nil then
      Result := default
    else
      Result := obj1.AsInteger;
  except
    Result := default;
  end;
end;

function SOAsBooleanDef(obj: ISuperObject; name: string; default: boolean): boolean;
begin
  if default then
    Result := SOAsIntegerDef(obj, name, 1) <> 0
  else
    Result := SOAsIntegerDef(obj, name, 0) <> 0;
end;

function SOAsShips(obj: ISuperObject; name: string): TFleetOrderArr;
var
  sarr: TSuperArray;
  item: TSuperTableString;
  i: Integer;
begin
  SetLength(Result, 0);

  sarr := obj.A[name];
  if sarr <> nil then
  begin
    SetLength(Result, sarr.Length);

    for i := 0 to sarr.Length - 1 do
    begin
      if sarr.O[i] = nil then continue;
      item := sarr.O[i].AsObject;
      if (item = nil) or (item.count <> 1) then continue;

      Result[i].Name := item.GetNames['0'].AsString;
      Result[i].Value := StrToIntDef(item.GetValues['0'].AsString, 0);
    end;
  end;
end;

function TimeBetween(dt: TDateTime; FromHour, ToHour: integer): boolean;
begin
  Result :=
    (HourOf(dt) >= FromHour) and (HourOf(dt) <= ToHour);
end;

end.
