unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DIContainer;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
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

procedure TForm2.Button1Click(Sender: TObject);
var
  ds: TDataService;
begin
//  ds := TCSVDataService.Create('danieleutf8');
//  try
//    ds.Add(['value1', 'value2']);
//    ds.Add(['value3', 'value4']);
//    ds.Add(['value5', 'value6']);
//  finally
//    ds.Free;
//  end;
//
//  ds := TPaddedDataService.Create('danieleansi');
//  try
//    ds.Add(['value1', 'value2']);
//    ds.Add(['value3', 'value4']);
//    ds.Add(['value5', 'value6']);
//  finally
//    ds.Free;
//  end;
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  ds: TDataService;
begin
  ShowMessage('Retrieving "dataservice" service fom DIContainer');
  ds := DIContainer.Get('DATAFILE.DAT') as TDataService;
  try
    ds.Open('datafile');
    ds.Add(['value1', 'value2']);
    ds.Add(['value3', 'value4']);
    ds.Add(['value5', 'value6']);
    ShowMessage('All data are been written in "DATAFILE.DAT" file');
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
  s := TFile.ReadAllText(ChangeFileExt(Application.ExeName, '.config'));
  DIContainer := TDIContainer.Create;
  DIContainer.AddComponent(s, 'dataservice', TDIContainerInitType.CreateNewInstance);
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  FreeAndNil(DIContainer);
end;

end.
