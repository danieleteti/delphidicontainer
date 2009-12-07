{ **************************************************************
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
  **************************************************************** }

unit DIContainer;
{$SCOPEDENUMS ON}

interface

uses
  Types,
  Generics.Collections,
  RTTI,
  TypInfo,
  ioutils,
  strutils,
  SysUtils;

type
  EDIContainer = class(Exception)
  end;

  TDIContainerInitType = (Singleton, CreateNewInstance);

  TDIContainerItem = class
  public
    FInstance: TObject;
    FInitType: TDIContainerInitType;
    constructor Create(AInitType: TDIContainerInitType);
    destructor Destroy; override;
  end;

  TDIContainer = class
  private
    ctx: TRttiContext;
    FClasses: TDictionary<string, TDIContainerItem>;
    FComponentAliases: TDictionary<string, string>;
    function CreateObjectWithDependencies(List: TList<string>; AQualifiedClassName: string): TObject;

  protected
    function GetConstructor(AType: TRttiType): TRttiMethod;
    function GetParameters(List: TList<string>; AConstructorParameters: TArray<TRttiParameter>): TArray<TValue>;
    function InternalGetComponent(AList: TList<string>; AQualifiedClassName: String): TObject; overload;

  public
    constructor Create; virtual;
    destructor Destroy; override;
    function LoadConfiguration(const AFileName: String): TDIContainer;
    function SetAlias(AQualifiedClassName, AAlias: String): TDIContainer; overload;
    function SetAlias(AClass: TClass; AAlias: String): TDIContainer; overload;
    function AddComponent(AQualifiedClassName: String;
      AContainerInitType: TDIContainerInitType = TDIContainerInitType.CreateNewInstance): TDIContainer; overload;
    function AddComponent(AClass: TClass;
      AContainerInitType: TDIContainerInitType = TDIContainerInitType.CreateNewInstance): TDIContainer; overload;
    function GetComponentByAlias(AAlias: String): TObject;
    function GetInterfaceByAlias(AAlias: String): IInterface;
    function GetComponent(AClass: TClass): TObject; overload;
    function GetComponent(AQualifiedClassName: String): TObject; overload;
    function RegisteredComponents: TDictionary<string, TDIContainerItem>;
  end;

  DIContainerUtils = class
    class function GetQualifiedClassName(AClass: TClass): String;
  end;

implementation

{ Container }

constructor TDIContainer.Create;
begin
  inherited;
  FClasses := TDictionary<string, TDIContainerItem>.Create;
  FComponentAliases := TDictionary<string, string>.Create;
end;

destructor TDIContainer.Destroy;
var
  item: TPair<string, TDIContainerItem>;
begin
  FComponentAliases.Free;
  for item in FClasses do
  begin
    item.Value.Free;
  end;
  FClasses.Free;
  inherited;
end;

function TDIContainer.InternalGetComponent(AList: TList<string>; AQualifiedClassName: String): TObject;
var
  ContType: TDIContainerItem;
begin
  if FClasses.ContainsKey(AQualifiedClassName) then
  begin
    ContType := FClasses[AQualifiedClassName];
    if ContType.FInitType = TDIContainerInitType.CreateNewInstance then
    begin
      if AList.Contains(AQualifiedClassName) then
        raise EDIContainer.Create('Ciclic dependecies');
      AList.Add(AQualifiedClassName);
      Result := CreateObjectWithDependencies(AList, AQualifiedClassName);
    end
    else
    begin
      if not Assigned(ContType.FInstance) then
      begin
        if AList.Contains(AQualifiedClassName) then
          raise EDIContainer.Create('Ciclic dependecies');
        AList.Add(AQualifiedClassName);
        ContType.FInstance := CreateObjectWithDependencies(AList, AQualifiedClassName);
      end;
      Result := ContType.FInstance;
    end;
  end
  else
    raise EDIContainer.Create('Cannot find service ' + AQualifiedClassName);
end;

function TDIContainer.LoadConfiguration(const AFileName: String): TDIContainer;
var
  components: TStringDynArray;
  component, classname, alias: string;
begin
  components := TFile.ReadAllLines(AFileName);
  for component in components do
  begin
    if Pos('#', component) = 0 then
    begin
      classname := LeftStr(component, Pos('=', component) - 1);
      alias := Copy(component, Pos('=', component) + 1, MaxInt);
      AddComponent(classname).SetAlias(classname, alias);
    end;
  end;
end;

function TDIContainer.GetConstructor(AType: TRttiType): TRttiMethod;
var
  methods: TArray<RTTI.TRttiMethod>;
  method: RTTI.TRttiMethod;
begin
  methods := AType.GetDeclaredMethods;
  for method in methods do
    if method.IsConstructor then
      Exit(method);
  raise EDIContainer.Create('Cannot find constructor for type ' + AType.ToString);
end;

function TDIContainer.GetInterfaceByAlias(AAlias: String): IInterface;
begin
  Result := GetComponentByAlias(AAlias) as TInterfacedObject;
end;

function TDIContainer.GetComponent(AClass: TClass): TObject;
var
  List: TList<string>;
begin
  List := TList<string>.Create;
  try
    Result := InternalGetComponent(List, AClass.UnitName + '.' + AClass.classname);
  finally
    List.Free;
  end;
end;

function TDIContainer.GetComponent(AQualifiedClassName: String): TObject;
var
  List: TList<string>;
begin
  List := TList<string>.Create;
  try
    Result := InternalGetComponent(List, AQualifiedClassName);
  finally
    List.Free;
  end;
end;

function TDIContainer.GetComponentByAlias(AAlias: String): TObject;
var
  List: TList<string>;
begin
  List := TList<string>.Create;
  try
    if FComponentAliases.ContainsKey(AAlias) then
      Result := InternalGetComponent(List, FComponentAliases.Items[AAlias])
    else
      raise EDIContainer.CreateFmt('Alias %s not found', [AAlias]);
  finally
    List.Free;
  end;
end;

function TDIContainer.GetParameters(List: TList<string>; AConstructorParameters: TArray<TRttiParameter>)
  : TArray<TValue>;
var
  I: Integer;
begin
  SetLength(Result, Length(AConstructorParameters));
  for I := 0 to Length(AConstructorParameters) - 1 do
  begin
    Result[I] := InternalGetComponent(List, AConstructorParameters[I].ParamType.QualifiedName);
  end;
end;

function TDIContainer.AddComponent(AClass: TClass;
  AContainerInitType: TDIContainerInitType = TDIContainerInitType.CreateNewInstance): TDIContainer;
begin
  Result := AddComponent(DIContainerUtils.GetQualifiedClassName(AClass), AContainerInitType);
end;

function TDIContainer.RegisteredComponents: TDictionary<string, TDIContainerItem>;
begin
  Result := FClasses;
end;

function TDIContainer.SetAlias(AClass: TClass; AAlias: String): TDIContainer;
begin
  Result := SetAlias(DIContainerUtils.GetQualifiedClassName(AClass), AAlias);
end;

function TDIContainer.SetAlias(AQualifiedClassName, AAlias: String): TDIContainer;
begin
  if FClasses.ContainsKey(AQualifiedClassName) and
    (not FClasses.ContainsKey(AAlias) and (not FComponentAliases.ContainsKey(AAlias))) then
    FComponentAliases.Add(AAlias, AQualifiedClassName)
  else
    raise EDIContainer.CreateFmt('Alias [%s] is already used or is the same name of a service QualifiedClassName', [AAlias]);
  Result := Self;
end;

function TDIContainer.CreateObjectWithDependencies(List: TList<string>; AQualifiedClassName: string): TObject;
var
  t: TRttiInstanceType;
  parameters: TArray<TValue>;
  rtti_method: TRttiMethod;
begin
  t := ctx.FindType(AQualifiedClassName) as TRttiInstanceType;
  if Assigned(t) then
  begin
    rtti_method := GetConstructor(t);
    parameters := GetParameters(List, rtti_method.GetParameters);
    Result := rtti_method.Invoke(t.MetaclassType, parameters).AsObject;
  end
  else
    raise EDIContainer.CreateFmt('Cannot find type [%s]',[AQualifiedClassName]);
end;

function TDIContainer.AddComponent(AQualifiedClassName: String;
  AContainerInitType: TDIContainerInitType = TDIContainerInitType.CreateNewInstance): TDIContainer;
begin
  if not FClasses.ContainsKey(AQualifiedClassName) then
    FClasses.Add(AQualifiedClassName, TDIContainerItem.Create(AContainerInitType));
  Result := Self;
end;

{ TContainerItem }

constructor TDIContainerItem.Create(AInitType: TDIContainerInitType);
begin
  inherited Create;
  FInitType := AInitType;
  FInstance := nil;
end;

{ ContainerUtils }

class function DIContainerUtils.GetQualifiedClassName(AClass: TClass): String;
begin
  Result := AClass.UnitName + '.' + AClass.classname;
end;

destructor TDIContainerItem.Destroy;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);
  inherited;
end;

end.
