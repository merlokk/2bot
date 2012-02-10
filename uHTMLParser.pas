unit uHTMLParser;

interface
uses
  Classes, StrUtils, SysUtils, DateUtils;

type
  TParser = class
  public
    class function CopyFromI(s, I: string): string;
    class function CopyFromIOrFull(s, I: string): string;
    class function CopyToI(s, I: string): string;
    class function CopyToIOrFull(s, I: string): string;

    class function Delim2Ints(s, delim: string; var int1, int2: integer): boolean;
    class function Delim2Res(s, delim: string; var int1, int2: int64): boolean;
    class function ResToInt64(res: string): int64;
    class function StrToExtended(s: string; default: extended): extended;
    class function StrTimeToDT(s: string; default: TDateTime): TDateTime;
    class function StrDateWeekTimeToDT(sdt: string; default: TDateTime): TDateTime;
    class function StrDateTimeToDT(sdt: string; default: TDateTime): TDateTime;

    class function URLDecode(s: string): string;
    class function GetURLParam(s, name: string): string;

    class function GetNextCell(var s: string; delim: char): string;
    class function GetDblQuotesText(s: string): string;
    class function GetTagText(s: string): string;
    class function GetTagAttribs(s: string): string;
    class function GetTagAttrib(s, name: string): string;

    class function GetTag(s, name: string): string;
    class function GetSingleTag(s, name: string): string;
    class function GetTagA(s: string): string;
    class function GetTagImg(s: string): string;

    class function GetTextWOAttrVal(s, AttrName, AttrStart: string): string;
    class function GetTable(s: string): string;
    class function GetTableWoInner(s: string): string;
    class function GetNextRow(var s: string): string;
    class function GetNextColumn(var s: string): string;
  end;

implementation

const
  SMonthNames:array [1..12] of string=
  ('Jan',
  'Feb',
  'Mar',
  'Apr',
  'May',
  'Jun',
  'Jul',
  'Aug',
  'Sep',
  'Oct',
  'Nov',
  'Dec');

 SDayNames:array[1..7] of string=
  ('Sun',
  'Mon',
  'Tue',
  'Wed',
  'Thu',
  'Fri',
  'Sat');

{ THTMLParser }

class function TParser.CopyFromI(s, I: string): string;
begin
  Result := '';
  if Pos(I, s) = 0 then exit;
  Result := Copy(s, Pos(I, s) + length(I), length(s));
end;

class function TParser.CopyFromIOrFull(s, I: string): string;
begin
  if Pos(I, s) <> 0 then
    Result := Copy(s, Pos(I, s) + length(I), length(s))
  else
    Result := s;
end;

class function TParser.CopyToI(s, I: string): string;
begin
  Result := '';
  if Pos(I, s) = 0 then exit;
  Result := Copy(s, 1, Pos(I, s) - 1);
end;

class function TParser.CopyToIOrFull(s, I: string): string;
begin
  if Pos(I, s) <> 0 then
    Result := Copy(s, 1, Pos(I, s) - 1)
  else
    Result := s;
end;

class function TParser.Delim2Ints(s, delim: string; var int1,
  int2: integer): boolean;
begin
  Result := false;
  if Pos(delim, s) <= 0 then exit;

  int1 := StrToIntDef(CopyToI(s, delim), 0);
  int2 := StrToIntDef(CopyFromI(s, delim), 0);
end;

class function TParser.Delim2Res(s, delim: string; var int1,
  int2: int64): boolean;
begin
  Result := false;
  if Pos(delim, s) <= 0 then exit;

  int1 := ResToInt64(CopyToI(s, delim));
  int2 := ResToInt64(CopyFromI(s, delim));
end;

class function TParser.GetTag(s, name: string): string;
begin
  Result := CopyFromI(s, '<' + name);
  Result := CopyToI(Result, '</' + name);
end;

class function TParser.GetTagA(s: string): string;
begin
  Result := CopyFromI(s, '<a');
  Result := CopyToI(Result, '</a');
end;

class function TParser.GetTagImg(s: string): string;
begin
  Result := CopyFromI(s, '<img');
  Result := CopyToI(Result, '>');
end;

class function TParser.GetTagAttrib(s, name: string): string;
begin
  Result := GetTagAttribs(s);
  Result := CopyFromI(Result, name + '=');
  if Result = '' then exit;
  if Result[1] = '"' then
    Result := GetDblQuotesText(Result)
  else
    Result := CopyToIOrFull(Result, ' ');
end;

class function TParser.GetTagAttribs(s: string): string;
begin
  Result := CopyToIOrFull(s, '>');
end;

class function TParser.GetTagText(s: string): string;
begin
  Result := CopyFromIOrFull(s, '>');
  Result := CopyToIOrFull(Result, '<');
end;

class function TParser.GetTextWOAttrVal(s, AttrName, AttrStart: string): string;
var
 tbegin,
 tend: integer;
 stmp,
 stmp2: string;
begin
  Result := s;
  while true do
  begin
    tbegin := Pos(AttrName + '="' + AttrStart, Result);
    if tbegin <> 0 then tbegin := tbegin + length(AttrName) + 1;
    tend := PosEx('"', Result, tbegin + 1);
    if (tbegin = 0) or (tbegin >= tend) then break;

    stmp := Copy(Result, 1, tbegin);
    stmp2 := Copy(Result, tend, length(Result));
    Result := stmp + stmp2;
  end;
end;

class function TParser.GetURLParam(s, name: string): string;
begin
  Result := '';
  if Pos(name + '=', s) = 0 then exit;

  Result := CopyFromI(s, name + '=');
  if Result = '' then exit;
  if Result[1] = '"' then
    Result := CopyToI(Result, '"')
  else
    Result := CopyToIOrFull(Result, '&');
end;

class function TParser.ResToInt64(res: string): int64;
begin
  res := ReplaceStr(res, '.', '');
  Result := StrToInt64Def(res, 0);
end;

class function TParser.GetNextCell(var s: string; delim: char): string;
var
  l: Longint;
begin
  l := 1;
  while l <= Length(s) do
  begin
    if s[l] = delim then
    begin
      Result := copy(s, 1, l - 1);
      Delete(s, 1, l);
      exit;
    end;
    l := l + 1;
  end;
  Result := s;
  s := '';
end;

class function TParser.StrDateWeekTimeToDT(sdt: string;
  default: TDateTime): TDateTime;
var
  i,
  k:integer;
  s:string;
  aWeekDay:string;
  aYear,
  aMonth,
  aDay:word;
  aHour,
  aMin,
  aSec:integer;
begin
  Result := default;
  sdt := StringReplace(sdt, ':', ' ', [rfReplaceAll, rfIgnoreCase]);
  aMonth := 0;
  aDay := 0;
  aHour := 0;
  aMin := 0;
  aSec := 0;

  for i := 1 to 6 do
  begin
    s := GetNextCell(sdt, ' ');
    case i of
     1: aWeekDay := s;
     2://Месяцы
       for k := Low(SMonthNames) to High(SMonthNames) do
       begin
         if CompareText(s, SMonthNames[k]) = 0 then
         begin
           aMonth := k;
           break;
         end;
       end;
     3://Дата
       aDay := StrToIntDef(s, 0);
     4://Часы
       aHour := StrToIntDef(s, 0);
     5://Минуты
       aMin := StrToIntDef(s, 0);
     6://секунды
       aSec := StrToIntDef(s, 0);
    end;
  end;

  aYear:=YearOf(now);

  for i := aYear - 1 to aYear + 1 do
  begin
    result := EncodeDate(i, aMonth, aDay);
    if AnsiCompareText(aWeekDay, SDayNames[dayofweek(result)])=0
    then break;
  end;

  result :=
    trunc(result) +
    aHour / HoursPerDay +
    aMin / MinsPerDay +
    aSec / SecsPerDay;
end;

class function TParser.StrDateTimeToDT(sdt: string;
  default: TDateTime): TDateTime;
var
  i,
  k: integer;
  s: string;
  aYear,
  aMonth,
  aDay: word;
  aHour,
  aMin,
  aSec: integer;
begin
  try
    // format string "07. Feb 2012, 14:42:59"
    sdt := ReplaceStr(sdt, ':', ' ');
    sdt := ReplaceStr(sdt, '.', '');
    sdt := ReplaceStr(sdt, ',', '');
    sdt := Trim(sdt);
    aYear := 0;
    aMonth := 0;
    aDay := 0;
    aHour := 0;
    aMin := 0;
    aSec := 0;

    for i := 1 to 6 do
    begin
      s := GetNextCell(sdt, ' ');
      case i of
       1://Дата
         aDay := StrToIntDef(s, 0);
       2://Месяцы
         for k := Low(SMonthNames) to High(SMonthNames) do
         begin
           if CompareText(s, SMonthNames[k]) = 0 then
           begin
             aMonth := k;
             break;
           end;
         end;
       3://Год
         aYear := StrToIntDef(s, 0);
       4://Часы
         aHour := StrToIntDef(s, 0);
       5://Минуты
         aMin := StrToIntDef(s, 0);
       6://секунды
         aSec := StrToIntDef(s, 0);
      end;
    end;

    if (aYear = 0) then aYear := YearOf(Now);
    if (aYear < 1990) or
       (aYear > 2090) or
       (aMonth < 1) or
       (aMonth > 12) or
       (aDay < 1) or
       (aDay > 31) or
       (aHour < 1) or
       (aHour > 23) or
       (aMin < 1) or
       (aMin > 59) or
       (aSec < 1) or
       (aSec > 59)
    then
    begin
      Result := default;
      exit;
    end;

    Result := EncodeDateTime(
      aYear,
      aMonth,
      aDay,
      aHour,
      aMin,
      aSec,
      0);
  except
    Result := default;
  end;
end;

class function TParser.StrTimeToDT(s: string; default: TDateTime): TDateTime;
var
  sl : TStringList;
  hh,
  mm,
  ss: integer;
  i: Integer;
begin
  Result := default;
  try
    hh := 0;
    mm := 0;
    ss := 0;
    if length(s) < 2 then exit;
    sl := TStringList.Create;
    sl.Delimiter := ' ';
    sl.DelimitedText := s;
    try
      if sl.Count < 1 then exit;
      for i := 0 to sl.Count - 1 do
      begin
        if (sl[i] <> '') and CharInSet(sl[i][length(sl[i])], ['h', 'm', 's'])
        then
        begin
          if sl[i][length(sl[i])] = 'h' then
            hh := StrToIntDef(Copy(sl[i], 1, length(sl[i]) -1), 0);
          if sl[i][length(sl[i])] = 'm' then
            mm := StrToIntDef(Copy(sl[i], 1, length(sl[i]) -1), 0);
          if sl[i][length(sl[i])] = 's' then
            ss := StrToIntDef(Copy(sl[i], 1, length(sl[i]) -1), 0);
        end;
      end;

    finally
      sl.Free;
    end;
    Result := EncodeTime(hh, mm, ss, 0);
  except
    Result := default;
  end;
end;

class function TParser.StrToExtended(s: string; default: extended): extended;
begin
  s := ReplaceStr(s, '.', ',');
  Result := StrToFloatDef(s, default)
end;

class function TParser.URLDecode(s: string): string;
begin
  Result := ReplaceStr(s, '&amp', '&');
end;

class function TParser.GetDblQuotesText(s: string): string;
begin
  Result := CopyFromI(s, '"');
  Result := CopyToI(Result, '"');
end;

class function TParser.GetNextColumn(var s: string): string;
begin
  s := CopyFromI(s, '<td');
  Result := CopyToI(s, '</td');
  s := CopyFromI(s, '</td');
end;

class function TParser.GetNextRow(var s: string): string;
begin
  s := CopyFromI(s, '<tr');
  Result := CopyToI(s, '</tr');
  s := CopyFromI(s, '</tr');
end;

class function TParser.GetSingleTag(s, name: string): string;
begin
  Result := CopyFromI(s, '<' + name);
  Result := CopyToI(Result, '>');
end;

class function TParser.GetTable(s: string): string;
begin
  Result := CopyFromI(s, '<table');
  Result := CopyToI(Result, '</table');
end;

class function TParser.GetTableWoInner(s: string): string;
var
 tbegin,
 tend: integer;
 stmp,
 stmp2: string;
begin
  Result := CopyFromI(s, '<table');

  tbegin := Pos('<table', Result);
  tend := Pos('</table', Result);
  while (tbegin <> 0) and (tbegin < tend) do
  begin
    stmp := Copy(Result, 1, tbegin - 1);
    stmp2 := Copy(Result, tend + 1, length(Result));
    stmp2 := Copy(stmp2, Pos('>', stmp2) + 1, length(stmp2));
    Result := stmp + stmp2;

    tbegin := Pos('<table', Result);
    tend := Pos('</table', Result);
  end;

  Result := CopyToI(Result, '</table');
end;

end.
