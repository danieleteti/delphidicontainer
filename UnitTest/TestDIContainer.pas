{**************************************************************
 Copyright (c) <2009> <Daniele Teti>

 Permission is hereby granted, free of charge, to any person
 obtaining a copy of this software and associated documentation
 files (the "Software"), to deal in the Software without
 restriction, including without limitation the rights to use,
 copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the
 Software is furnished to do so, subject to the following
 conditions:

 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 OTHER DEALINGS IN THE SOFTWARE.
****************************************************************}

unit TestDIContainer;

interface

uses
  TestFramework,
  IOCContainer,
  Types,
  Generics.Collections,
  TypInfo,
  RTTI,
  DIContainer;

type
  TestContainer = class(TTestCase)
  strict private
    DIContainer: TDIContainer;
  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestLoadConfiguration;
    procedure TestRegisterComponentCreateNewInstance;
    procedure TestRegisterComponentSingleton;
    procedure TestGetDependentObject;
    procedure TestGetServices;
    procedure TestGetComponentByAlias;
    procedure TestAddComponentWithAlias;
    procedure TestGetComponentByWrongAlias;
    procedure TestGetComponentByAliasAlreadyUsed;
    procedure TestGetComponentWithInterface;
    procedure TestRecursiveDependence;
    procedure TestSameServiceInConstructor;
    procedure TestInitializationBlock;
    procedure TestInitializationBlockSingleton;
  end;

implementation

uses
  ServiceTestObjectsU,
  SysUtils;

procedure TestContainer.SetUp;
begin
  inherited;
  DIContainer := TDIContainer.Create;
end;

procedure TestContainer.TearDown;
begin
  DIContainer.Free;
  DIContainer := nil;
  inherited;
end;

procedure TestContainer.TestRecursiveDependence;
var
  s: TService5;
begin
  DIContainer.AddComponent(TService4);
  DIContainer.AddComponent(TService5);
  ExpectedException := EDIContainer;
  s := DIContainer.GetComponent(TService5) as TService5;
  CheckTrue(s.ClassNameIs('TService5'));
end;

procedure TestContainer.TestRegisterComponentCreateNewInstance;
var
  s1, s2: TService1;
begin
  DIContainer.AddComponent(
    ContainerUtils.GetQualifiedClassName(TService1),
    TDIContainerInitType.CreateNewInstance);
  s1 := DIContainer.GetComponent(TService1) as TService1;
  s2 := DIContainer.GetComponent(TService1) as TService1;
  CheckFalse(s1 = s2);
  s1.Free;
  s2.Free;
end;

procedure TestContainer.TestRegisterComponentSingleton;
var
  s1, s2: TService1;
begin
  DIContainer.AddComponent(ContainerUtils.GetQualifiedClassName(TService1), TDIContainerInitType.Singleton);
  s1 := DIContainer.GetComponent(TService1) as TService1;
  s2 := DIContainer.GetComponent(TService1) as TService1;
  CheckTrue(s1 = s2);
  // s1 and s2 references are managed by DIContainer
end;

procedure TestContainer.TestSameServiceInConstructor;
var
  s: TService6;
begin
  DIContainer.AddComponent(TService1);
  DIContainer.AddComponent(TService6);
  ExpectedException := EDIContainer;
  s := DIContainer.GetComponent(TService6) as TService6;
  CheckTrue(s.ClassNameIs('TService6'));
end;

procedure TestContainer.TestAddComponentWithAlias;
var
  srv3: TService3;
begin
  DIContainer
    .AddComponent(TService1, TDIContainerInitType.Singleton)
    .AddComponent(TService2, TDIContainerInitType.Singleton)
    .AddComponent(TService3).SetAlias(TService1, 'service01')
    .SetAlias(TService2, 'service02').SetAlias(TService3, 'service03');
  // service3 is not managed by DIContainer becouse it's not a TDIContainerInitType.Singleton
  srv3 := DIContainer.GetComponentByAlias('service03') as TService3;
  try
    CheckEquals('TService1', DIContainer.GetComponentByAlias('service01').ClassName);
    CheckEquals('TService2', DIContainer.GetComponentByAlias('service02').ClassName);
    CheckEquals('TService3', srv3.ClassName);
  finally
    srv3.Free;
  end;
end;

procedure TestContainer.TestGetComponentByAlias;
var
  srv3: TService3;
begin
  DIContainer.AddComponent(TService1, TDIContainerInitType.Singleton).AddComponent
    (TService2, TDIContainerInitType.Singleton).AddComponent(TService3).SetAlias(TService1, 'service01').SetAlias
    (TService2, 'service02').SetAlias(TService3, 'service03');
  // service3 is not managed by DIContainer becouse it's not a TDIContainerInitType.Singleton
  srv3 := DIContainer.GetComponentByAlias('service03') as TService3;
  try
    CheckEquals('TService1', DIContainer.GetComponentByAlias('service01').ClassName);
    CheckEquals('TService2', DIContainer.GetComponentByAlias('service02').ClassName);
    CheckEquals('TService3', srv3.ClassName);
  finally
    srv3.Free;
  end;
end;

procedure TestContainer.TestGetComponentByAliasAlreadyUsed;
begin
  ExpectedException := EDIContainer;
  DIContainer.AddComponent(TService1, TDIContainerInitType.Singleton).AddComponent
    (TService2, TDIContainerInitType.Singleton).SetAlias(TService1, 'service01').SetAlias
    (TService2, ContainerUtils.GetQualifiedClassName(TService1))
end;

procedure TestContainer.TestGetComponentByWrongAlias;
begin
  ExpectedException := EDIContainer;
  DIContainer.AddComponent(TService1, TDIContainerInitType.Singleton).AddComponent
    (TService2, TDIContainerInitType.Singleton).SetAlias(TService1, 'service01').SetAlias(TService2, 'service01');
end;

procedure TestContainer.TestGetComponentWithInterface;
begin
  DIContainer.AddComponent(TInterfacedService1).SetAlias(TInterfacedService1, 'myintf1').AddComponent
    (TInterfacedService2).SetAlias(TInterfacedService2, 'myintf2');
  CheckNotNull(DIContainer.GetInterfaceByAlias('myintf1'));
  CheckNotNull(DIContainer.GetInterfaceByAlias('myintf2'));
end;

procedure TestContainer.TestGetDependentObject;
var
  srv3: TService3;
begin
  DIContainer.AddComponent(TService1, TDIContainerInitType.Singleton).AddComponent
    (TService2, TDIContainerInitType.Singleton).AddComponent(TService3);

  TService1(DIContainer.GetComponent(TService1)).Message := 'MESSAGE[Service1]';
  TService2(DIContainer.GetComponent(TService2)).Message := 'MESSAGE[Service2]';
  srv3 := TService3(DIContainer.GetComponent(TService3));
  try
    CheckEquals('MESSAGE[Service1]MESSAGE[Service2]', srv3.GetCompoundMessage);
  finally
    srv3.Free;
  end;
end;

procedure TestContainer.TestGetServices;
begin
  CheckEquals(3, DIContainer.AddComponent(TService1).AddComponent(TService2).AddComponent(TService3)
      .RegisteredComponents.Count);
end;

procedure TestContainer.TestInitializationBlock;
var
  srv1, srv2: TService1;
begin
  DIContainer.AddComponent(
    ContainerUtils.GetQualifiedClassName(TService1),
    function: TObject
    begin
      Result := TService1.Create;
    end,
    '',
    TDIContainerInitType.CreateNewInstance
    );
  DIContainer.SetAlias(ContainerUtils.GetQualifiedClassName(TService1), 'service1');
  srv1 := DIContainer.GetComponentByAlias('service1') as TService1;
  CheckIs(srv1, TService1);
  srv2 := DIContainer.GetComponentByAlias('service1') as TService1;
  CheckTrue(srv1 <> srv2);
  srv1.Free;
  srv2.Free;

end;

procedure TestContainer.TestInitializationBlockSingleton;
var
  srv1, srv2: TService1;
begin
  DIContainer.AddComponent(
    ContainerUtils.GetQualifiedClassName(TService1),
    function: TObject
    begin
      Result := TService1.Create;
      TService1(Result).Message := DateTimeToStr(now);
    end,
    '',
    TDIContainerInitType.Singleton
    );
  DIContainer.SetAlias(ContainerUtils.GetQualifiedClassName(TService1), 'service1');
  srv1 := DIContainer.GetComponentByAlias('service1') as TService1;
  CheckIs(srv1, TService1);
  srv2 := DIContainer.GetComponentByAlias('service1') as TService1;
  CheckTrue(srv1 = srv2);
  CheckTrue(srv1.Message = srv2.Message);
end;

procedure TestContainer.TestLoadConfiguration;
var
  intf: IInterfaceService1;
begin
  DIContainer.LoadConfiguration('container.conf');
  intf := DIContainer.GetInterfaceByAlias('myservice') as IInterfaceService1;
  CheckNotNull(intf);
  intf.SetMessage('hello');
  CheckEquals('hello', intf.GetMessage);
end;

initialization

// Register any test cases with the test runner
RegisterTest(TestContainer.Suite);

end.
