unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DIContainer;

type
  TForm2 = class(TForm)
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    DIContainer: TDIContainer;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses DataService, IoUtils;

{$R *.dfm}

procedure TForm2.Button2Click(Sender: TObject);
var
  ds: TDataService;
const
  DATAFILE = 'datafile.dat';
begin
  ShowMessage('Retrieving "dataservice" service fom DIContainer');
  ds := DIContainer.Get('dataservice') as TDataService;
  try
    ds.Open(DATAFILE);
    ds.Add(['value1', 'value2']);
    ds.Add(['value3', 'value4']);
    ds.Add(['value5', 'value6']);
    ShowMessage('All data are been written in "' + DATAFILE + '" file');
  finally
    ds.Free;
  end;
  ShowMessage('Finished');
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  s: string;
begin
  FreeAndNil(DIContainer);
  //DIContainer still doesn't provide a standard configuration file syntax.
  //so for this example I'm using a simple file with one row containing
  //fully qualified name for the class used as service
  s := TFile.ReadAllText(ChangeFileExt(Application.ExeName, '.config'));
  Caption := 'Actual Config: ' + s;
  DIContainer := TDIContainer.Create;
  DIContainer.AddComponent(s, 'dataservice', TDIContainerInitType.CreateNewInstance);
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  FreeAndNil(DIContainer);
end;

end.
