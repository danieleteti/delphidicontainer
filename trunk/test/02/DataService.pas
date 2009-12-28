unit DataService;

interface

uses
  IOUtils, Classes, AnsiStrings;

type
  TDataService = class abstract
  private
    FFile: TStreamWriter;
  public
    constructor Create; virtual; abstract; //required by DIContainer
    procedure Open(FileName: String); virtual;
    destructor Destroy; override;
    procedure Add(Values: array of String); virtual; abstract;
  end;

  TPaddedDataService = class(TDataService)
  public
    constructor Create; override;  //required by DIContainer
    class procedure Register; // dummy method
    procedure Add(Values: array of String); override;
  end;

  TCSVDataService = class(TDataService)
  public
    constructor Create; override;  //required by DIContainer
    class procedure Register; // dummy method
    procedure Add(Values: array of String); override;
  end;

implementation

uses
  SysUtils;

{ TDataService }

procedure TDataService.Open(FileName: String);
begin
  FFile := TFile.CreateText(FileName);
end;

destructor TDataService.Destroy;
begin
  if Assigned(FFile) then
  begin
    FFile.Close;
    FFile.Free;
  end;
  inherited;
end;

procedure TPaddedDataService.Add(Values: array of String);
var
  s: string;
begin
  for s in Values do
    FFile.Write(Format('%20s', [s]));
  FFile.Write(sLineBreak);
end;

procedure TCSVDataService.Add(Values: array of String);
var
  s: string;
begin
  for s in Values do
    FFile.Write(Format('%s;', [s]));
  FFile.Write(sLineBreak);
end;

constructor TCSVDataService.Create;
begin
  inherited;

end;

class procedure TCSVDataService.Register;
begin

end;

constructor TPaddedDataService.Create;
begin
  inherited;

end;

class procedure TPaddedDataService.Register;
begin

end;

initialization
//with those lines the linked will include the actual code for the services
TCSVDataService.Register;
TPaddedDataService.Register;

end.
