program Test01;

uses
  Forms,
  MainForm in 'MainForm.pas' { Form6 },
  DIContainer in '..\..\src\DIContainer.pas',
  ServiceTestObjectsU in '..\..\UnitTest\ServiceTestObjectsU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
