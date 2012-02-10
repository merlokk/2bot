unit uLogger;

interface
uses
  Classes, SysUtils, StrUtils, SyncObjs, Forms;

type
  TLogger = class
    class var FLogger: TLogger;
   private
    FCS: TCriticalSection;

    FActive: boolean;
    FStream: array of string;
    MaxStreamCount: integer;
    FMaxFileLength: integer;

    FLogFileNamePrefix,
    FLogFilePath,
    FLogFileName: string;

    function GetNewLogFileName: string;
    class constructor Create;
   protected
    procedure PutLogMessageToFile(ALine: string);
    procedure PutLogMessageToStream(ALine: string);
   public
    class function GetInstance: TLogger;
    constructor Create(ALogFileNamePrefix, ALogFilePath: string);
    destructor Destroy; override;

    property Active: boolean read FActive write FActive;
  end;

procedure SetLogLevel(LogLevel: integer);
procedure AddLog(data: string; level: integer = 0);
procedure GetSLLog(sl: TStringList);

implementation
const
  FMaxSLCount = 23;

var
  FSLLog: TStringList;
  FLogLevel: integer;

procedure AddLog(data: string; level: integer = 0);
var
 msg: string;
begin
  if level > FLogLevel then exit;

  msg := IntToStr(level) + '|' + DateTimeToStr(Now) + ' ' + data;
  FSLLog.Add(msg);
  if FSLLog.Count > FMaxSLCount then FSLLog.Delete(0);

  TLogger.GetInstance.PutLogMessageToFile(msg);
end;

procedure SetLogLevel(LogLevel: integer);
begin
  FLogLevel := LogLevel;
end;

procedure GetSLLog(sl: TStringList);
begin
  sl.Assign(FSLLog);
end;

//        ------------------ TLogger ------------------
class function TLogger.GetInstance: TLogger;
begin
  if not assigned(FLogger) then
    FLogger := TLogger.Create(
      '',
      ExtractFilePath(Application.ExeName) + 'logs\');
  Result := FLogger;
end;

function TLogger.GetNewLogFileName: string;
begin
 Result := FLogFilePath + FLogFileNamePrefix + FormatDateTime('DDMMYY-HHNNSS', Now) + '.log';
end;

procedure TLogger.PutLogMessageToFile(ALine: string);
var
 f       : TextFile;
 isEx    : boolean;
 FileLen : integer;
 sr      : TSearchRec;
begin
 if not FActive then exit;

 FCS.Enter;
 try
  isEx := FileExists(FLogFileName);
  FileLen := 0;
  if isEx then
   begin
    FindFirst(FLogFileName, faAnyFile, sr);
    FileLen := sr.Size;
    FindClose(sr);
   end;

  AssignFile(f, FLogFileName);
  if isEx then
   Append(f)
  else
   Rewrite(f);

  Writeln(f, ALine);

  flush(f);
  CloseFile(f);

  if (FileLen <> 0) and (FileLen > FMaxFileLength) then FLogFileName := GetNewLogFileName;
 except
 end;
 FCS.Leave;
end;

procedure TLogger.PutLogMessageToStream(ALine: string);
var
 i: integer;
begin
 FCS.Enter;
 try
  if length(FStream) >= MaxStreamCount then
   begin
    for i := 0 to MaxStreamCount - 2 do
     FStream[i] := FStream[i + 1];
   end
  else
   SetLength(FStream, length(FStream) + 1);

  FStream[length(FStream) - 1] := ALine;
 except
 end;
 FCS.Leave;
end;

constructor TLogger.Create(ALogFileNamePrefix, ALogFilePath: string);
begin
 FCS := TCriticalSection.Create;
 SetLength(FStream, 0);
 MaxStreamCount := 40;
 FMaxFileLength := 100000; // bytes
 FActive := false;

 FLogFileNamePrefix := ALogFileNamePrefix;
 FLogFilePath := ALogFilePath;
 if not DirectoryExists(ALogFilePath) then CreateDir(ALogFilePath);

 FLogFileName := GetNewLogFileName;
end;

class constructor TLogger.Create;
begin
  FLogger := nil;
end;

destructor TLogger.Destroy;
begin
 SetLength(FStream, 0);
 FCS.Destroy;
end;

initialization
  FLogLevel := 100;
  FSLLog := TStringList.Create;
  TLogger.GetInstance.Active := true;
end.
