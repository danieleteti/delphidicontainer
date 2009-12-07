unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DIContainer, StdCtrls, ServiceTestObjectsU;

type
  TForm6 = class(TForm)
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    di: TDIContainer;
    { Private declarations }
  public

  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

procedure TForm6.Button1Click(Sender: TObject);
var
  srv1: IInterfaceService1;
begin
  srv1 := di.GetInterfaceByAlias('myservice') as IInterfaceService1;
  srv1.SetMessage('Hello World');
  ShowMessage(srv1.GetMessage);
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
  di := TDIContainer.Create;
  di.LoadConfiguration('container.conf');
end;


end.