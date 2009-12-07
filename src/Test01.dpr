program Test01;
{$APPTYPE CONSOLE}

uses
  SysUtils,
  DIContainer in 'DIContainer.pas',
  ServiceTestObjectsU in '..\UnitTest\ServiceTestObjectsU.pas';

var
  Cont: TDIContainer;
  s1: TService1;
  s2: TService2;
  s3: TService3;

begin
  try
    Cont := TDIContainer.Create;
    try
      Cont.AddComponent(TService1);
      Cont.AddComponent('ServiceTestObjectsU.TService2');
      Cont.AddComponent('ServiceTestObjectsU.TService3');

      s1 := Cont.GetComponent('ServiceTestObjectsU.TService1') as TService1;
      s1.Message := 'I''m the first message';
      WriteLn(s1.Message);

      s2 := Cont.GetComponent(TService2) as TService2;
      s2.Message := 'I''m the second message';
      WriteLn(s2.Message);

      s3 := Cont.GetComponent('ServiceTestObjectsU.TService3') as TService3;
      WriteLn(s3.GetCompoundMessage);
    finally
      Cont.Free;
    end;
  except
    on E: Exception do
      WriteLn(E.ClassName, E.Message);
  end;
  readln;
end.
