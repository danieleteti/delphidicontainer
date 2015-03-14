# delphidicontainer
A (simple) Delphi Dependency Injection Framework for Delphi 2010 or better on Windows and MacOSX.

```delphi
        program Test01;
        {$APPTYPE CONSOLE}
        
        uses
          SysUtils,
          DIContainer in 'DIContainer.pas',
          ServiceTestObjectsU in '..\UnitTest\ServiceTestObjectsU.pas';
        
        var
          DIContainer: TDIContainer;
          s1: TService1;
          s2: TService2;
          s3: TService3;
          s6: TService6;
          s7: TService7;
        begin
          ReportMemoryLeaksOnShutdown := True;
          try
            DIContainer := TDIContainer.Create;
            try
              // AddComponent with TClass with and   InitType = Singleton
              DIContainer.AddComponent(TService1, TDIContainerInitType.Singleton);
              // AddComponent with QualifiedName and InitType = Singleton
              DIContainer.AddComponent('ServiceTestObjectsU.TService2',
                TDIContainerInitType.Singleton);
              // AddComponent with QualifiedName and InitType = CreateNewInstance
              DIContainer.AddComponent('ServiceTestObjectsU.TService3',
                TDIContainerInitType.CreateNewInstance);
              // GetComponent with QualifiedName
              s1 := DIContainer.GetComponent('ServiceTestObjectsU.TService1')
                as TService1;
              s1.Message := 'I''m the first message';
              WriteLn(s1.Message);
        
              // GetComponent with TClass
              s2 := DIContainer.GetComponent(TService2) as TService2;
              s2.Message := 'I''m the second message';
              WriteLn(s2.Message);
        
              // GetComponent with a dependent service (TService3 depends upon TService1 and TService2)
              s3 := DIContainer.GetComponent('ServiceTestObjectsU.TService3')
                as TService3;
              WriteLn(s3.GetCompoundMessage);
              // s3 is not created as Singleton, so after use it I must free it
              s3.Free;
        
              // AddComponent with QualifiedClassName, a custom initializer, an alias.
              // Component will be created as singleton (single instance managed by Container)
        
              DIContainer.AddComponent(DIContainerUtils.GetQualifiedClassName
                  (TService6),
                  function: TObject
                  begin
                    Result := TService6.Create(DIContainer.Get(TService1) as TService1,DIContainer.Get(TService1) as TService1);
                  end,
                  'srv6',
                TDIContainerInitType.Singleton);
        
              s6 := DIContainer.Get('srv6') as TService6;
              WriteLn(s6.ToString);
              s6 := DIContainer.Get('srv6') as TService6;
              WriteLn(s6.ToString);
        
              // AddComponent with QualifiedClassName, a custom initializer, an alias.
              // Component will be created as singleton (single instance managed by Container)
              DIContainer.AddComponent(DIContainerUtils.GetQualifiedClassName
                  (TService7),
                    function: TObject
                    begin
                      Result := TService7.Create(DIContainer.Get(TService1) as TService1,DIContainer.Get(TService1) as TService1);
                    end,
                    'srv7intf',
                  TDIContainerInitType.Singleton);
        
              s7 := DIContainer.Get('srv7intf') as TService7;
              WriteLn(s7.ToString);
            finally
              DIContainer.Free;
            end;
          except
            on E: Exception do
              WriteLn(E.ClassName, E.Message);
          end;
          // readln;
        end.
```
