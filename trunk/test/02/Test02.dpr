program Test02;

uses
  Forms,
  Unit2 in 'Unit2.pas' {Form2},
  DataService in 'DataService.pas',
  DIContainer in '..\..\src\DIContainer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
