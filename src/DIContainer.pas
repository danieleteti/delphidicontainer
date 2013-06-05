// @author Daniele Teti - http://www.danieleteti.it
// @version 0.2

{ @license
  **************************************************************
  Copyright (c) <2009-2013> <Daniele Teti>

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
  SysUtils,
  SyncObjs;

type
  // Specific DIContainer Exception
  EDIContainer = class(Exception)
  end;

  { Kind of Initialiazation type.
    Singleton: Every service is created only one and will be freed by DIContainer
    CreateNewInstance: Every call create new object }
  TDIContainerInitType = (Singleton, CreateNewInstance);

  // Internal Container item class containing every information and code about services
  TDIContainerItem = class
  private
    procedure SetCustomInitializationBlock(const Value: TFunc<TObject>);
    procedure SetInitType(const Value: TDIContainerInitType);
    procedure SetSingletonInstance(const Value: TObject);
  protected
    FCustomInitializationBlock: TFunc<TObject>;
    FSingletonInstance: TObject;
    FInitType: TDIContainerInitType;
  public
    constructor Create(AInitType: TDIContainerInitType;
      AInitializationBlock: TFunc<TObject>);
    destructor Destroy; override;
    property CustomInitializationBlock
      : TFunc<TObject> read FCustomInitializationBlock write
      SetCustomInitializationBlock;
    property SingletonInstance: TObject read FSingletonInstance write
      SetSingletonInstance;
    property InitType: TDIContainerInitType read FInitType write SetInitType;
  end;

  // The Container where you should register (AddComponent) and retrieve (Get*) your service
  TDIContainer = class
  private
    cs: TCriticalSection;
    ctx: TRttiContext;
    FFreeStack: TStack<TDIContainerItem>;
    FClasses: TDictionary<string, TDIContainerItem>;
    FComponentAliases: TDictionary<string, string>;
    function HasCustomInitializationBlock(AQualifiedClassName: string;
      out AInitBlock: TFunc<TObject>): boolean;
    function CreateObjectWithDependencies(List: TList<string>;
      AQualifiedClassName: string): TObject;
  protected
    function GetConstructor(AType: TRttiType): TRttiMethod;
    function GetParameters(List: TList<string>;
      AConstructorParameters: TArray<TRttiParameter>): TArray<TValue>;
    function InternalGetComponent(AList: TList<string>;
      AQualifiedClassName: String): TObject; overload;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function LoadConfiguration(const AFileName: String): TDIContainer;
    function SetAlias(AQualifiedClassName, AAlias: String): TDIContainer;
      overload;
    function SetAlias(AClass: TClass; AAlias: String): TDIContainer; overload;

    function AddComponent(AQualifiedClassName: String; AAlias: String;
      AContainerInitType: TDIContainerInitType =
      TDIContainerInitType.CreateNewInstance): TDIContainer; overload;

    function AddComponent(AQualifiedClassName: String;
      AContainerInitType: TDIContainerInitType =
      TDIContainerInitType.CreateNewInstance): TDIContainer; overload;

    function AddComponent(AClass: TClass;
      AContainerInitType: TDIContainerInitType =
      TDIContainerInitType.CreateNewInstance): TDIContainer; overload;

    function AddComponent(AQualifiedClassName: String;
      AnInitializationBlock: TFunc<TObject>; AAlias: String = '';
      AContainerInitType: TDIContainerInitType =
      TDIContainerInitType.CreateNewInstance): TDIContainer; overload;

    function AddComponent(AClass: TClass; AAlias: String;
      AContainerInitType: TDIContainerInitType =
      TDIContainerInitType.CreateNewInstance): TDIContainer; overload;
    // Return a component as TObject.
    function Get(AAlias: String): TObject; overload;
    function Get<T: class>(AAlias: String): T; overload;
    function Get(AClass: TClass): TObject; overload;
    function Get<T: class>: T; overload;
    function GetComponentByAlias(AAlias: String): TObject;
    function GetComponent(AClass: TClass): TObject; overload;
    function GetComponent(AQualifiedClassName: String): TObject; overload;
    function RegisteredComponents: TDictionary<string, TDIContainerItem>;
  end;

  { @description Utils methods }
  DIContainerUtils = class
    {
      @description QualifiedClassName from class name
    }
    class function GetQualifiedClassName(AClass: TClass): String;
  end;

implementation

{ Container }

function TDIContainer.AddComponent(AClass: TClass;
  AContainerInitType: TDIContainerInitType): TDIContainer;
begin
  Result := AddComponent(DIContainerUtils.GetQualifiedClassName(AClass),
    AContainerInitType);
end;

constructor TDIContainer.Create;
begin
  inherited;
  cs := TCriticalSection.Create;
  FClasses := TDictionary<string, TDIContainerItem>.Create;
  FFreeStack := TStack<TDIContainerItem>.Create;
  FComponentAliases := TDictionary<string, string>.Create;
end;

destructor TDIContainer.Destroy;
var
  PairItem: TPair<string, TDIContainerItem>;
  Item: TDIContainerItem;
begin
  FComponentAliases.Free;
  while FFreeStack.Count > 0 do
    FFreeStack.Pop.Free;
  FClasses.Free;
  FFreeStack.Free;
  cs.Free;
  inherited;
end;

function TDIContainer.InternalGetComponent(AList: TList<string>;
  AQualifiedClassName: String): TObject;
var
  ContType: TDIContainerItem;
begin
  //OutputDebugString(PChar('Getting: ' + AQualifiedClassName));
  if FClasses.ContainsKey(AQualifiedClassName) then
  begin
    ContType := FClasses[AQualifiedClassName];
    if ContType.FInitType = TDIContainerInitType.CreateNewInstance then
    begin
      if AList.Contains(AQualifiedClassName) then
        raise EDIContainer.CreateFmt(
          'Ciclic dependecies for [%s]. Probably you have got also a memory leak now.',
          [AQualifiedClassName]);
      AList.Add(AQualifiedClassName);
      Result := CreateObjectWithDependencies(AList, AQualifiedClassName);
    end
    else
    begin
      if not Assigned(ContType.FSingletonInstance) then
      begin
        cs.Acquire;
        try
          if not Assigned(ContType.FSingletonInstance) then // Double check
          begin
            TMonitor.Enter(AList);
            try
              if AList.Contains(AQualifiedClassName) then
                raise EDIContainer.CreateFmt('Ciclic dependecies for [%s]',
                  [AQualifiedClassName]);
              AList.Add(AQualifiedClassName);
              ContType.FSingletonInstance := CreateObjectWithDependencies
                (AList, AQualifiedClassName);
            finally
              TMonitor.Exit(AList);
            end;
          end;
        finally
          cs.Leave;
        end;
      end;
      Result := ContType.FSingletonInstance;
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
    if (trim(component) <> '') and (Pos('#', component) = 0) then
    begin
      classname := LeftStr(component, Pos('=', component) - 1);
      alias := Copy(component, Pos('=', component) + 1, MaxInt);
      AddComponent(classname).SetAlias(classname, alias);
    end;
  end;
  Result := Self;
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
  raise EDIContainer.Create
    ('Cannot find constructor for type ' + AType.ToString);
end;

function TDIContainer.GetComponent(AClass: TClass): TObject;
var
  List: TList<string>;
begin
  List := TList<string>.Create;
  try
    Result := InternalGetComponent(List,
      AClass.UnitName + '.' + AClass.classname);
  finally
    List.Free;
  end;
end;

function TDIContainer.Get<T>(AAlias: String): T;
begin
  Result := Get(AAlias) as T;
end;

function TDIContainer.Get(AAlias: String): TObject;
begin
  Result := GetComponentByAlias(AAlias);
end;

function TDIContainer.Get(AClass: TClass): TObject;
begin
  Result := GetComponent(AClass);
end;

function TDIContainer.Get<T>: T;
begin
  Result := GetComponent(ctx.GetType(T.ClassInfo).QualifiedName) as T;
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
  Result := nil;
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

function TDIContainer.GetParameters(List: TList<string>;
  AConstructorParameters: TArray<TRttiParameter>): TArray<TValue>;
var
  I: Integer;
begin
  SetLength(Result, Length(AConstructorParameters));
  for I := 0 to Length(AConstructorParameters) - 1 do
  begin
    Result[I] := InternalGetComponent(List,
      AConstructorParameters[I].ParamType.QualifiedName);
  end;
end;

function TDIContainer.HasCustomInitializationBlock(AQualifiedClassName: string;
  out AInitBlock: TFunc<TObject>): boolean;
var
  DIContItem: TDIContainerItem;
begin
  AInitBlock := nil;
  Result := FClasses.TryGetValue(AQualifiedClassName, DIContItem);
  if Result then
  begin
    Result := Assigned(DIContItem.FCustomInitializationBlock);
    AInitBlock := DIContItem.FCustomInitializationBlock;
  end;
end;

function TDIContainer.AddComponent(AClass: TClass; AAlias: String;
  AContainerInitType: TDIContainerInitType): TDIContainer;
begin
  Result := AddComponent(DIContainerUtils.GetQualifiedClassName(AClass),
    AAlias, AContainerInitType);
end;

function TDIContainer.AddComponent(AQualifiedClassName: String;
  AContainerInitType: TDIContainerInitType): TDIContainer;
begin
  Result := AddComponent(AQualifiedClassName, '', AContainerInitType);
end;

function TDIContainer.AddComponent(AQualifiedClassName: String;
  AnInitializationBlock: TFunc<TObject>; AAlias: String;
  AContainerInitType: TDIContainerInitType): TDIContainer;
var
  Item: TDIContainerItem;
begin
  if not FClasses.ContainsKey(AQualifiedClassName) then
  begin
    Item := TDIContainerItem.Create(AContainerInitType, AnInitializationBlock);
    FClasses.Add(AQualifiedClassName, Item);
    FFreeStack.Push(Item);
    if AAlias <> EmptyStr then
      SetAlias(AQualifiedClassName, AAlias);
  end
  else
    raise EDIContainer.CreateFmt('Service already registered [%s]',
      [AQualifiedClassName]);
  Result := Self;
end;

function TDIContainer.RegisteredComponents: TDictionary<string,
  TDIContainerItem>;
begin
  Result := FClasses;
end;

function TDIContainer.SetAlias(AClass: TClass; AAlias: String): TDIContainer;
begin
  Result := SetAlias(DIContainerUtils.GetQualifiedClassName(AClass), AAlias);
end;

function TDIContainer.SetAlias(AQualifiedClassName, AAlias: String)
  : TDIContainer;
begin
  if FClasses.ContainsKey(AQualifiedClassName) and
    (not FClasses.ContainsKey(AAlias) and (not FComponentAliases.ContainsKey
    (AAlias))) then
    FComponentAliases.Add(AAlias, AQualifiedClassName)
  else
    raise EDIContainer.CreateFmt(
      'Alias [%s] is already used or is the same name of a service QualifiedClassName',
      [AAlias]);
  Result := Self;
end;

function TDIContainer.CreateObjectWithDependencies(List: TList<string>;
  AQualifiedClassName: string): TObject;
var
  T: TRttiInstanceType;
  parameters: TArray<TValue>;
  rtti_method: TRttiMethod;
  InitBlock: TFunc<TObject>;
begin
  T := ctx.FindType(AQualifiedClassName) as TRttiInstanceType;
  if Assigned(T) then
  begin
    if not HasCustomInitializationBlock(AQualifiedClassName, InitBlock) then
    begin
      // InitBlock := nil;
      rtti_method := GetConstructor(T);
      parameters := GetParameters(List, rtti_method.GetParameters);
      Result := rtti_method.Invoke(T.MetaclassType, parameters).AsObject;
    end
    else
    begin
      Result := InitBlock();
      // InitBlock := nil;
    end;
  end
  else
    raise EDIContainer.CreateFmt('Cannot find type [%s]',
      [AQualifiedClassName]);
end;

function TDIContainer.AddComponent(AQualifiedClassName: String; AAlias: String;
  AContainerInitType: TDIContainerInitType): TDIContainer;
begin
  Result := AddComponent(AQualifiedClassName, nil, AAlias, AContainerInitType);
end;

{ TContainerItem }

constructor TDIContainerItem.Create(AInitType: TDIContainerInitType;
  AInitializationBlock: TFunc<TObject>);
begin
  inherited Create;
  FInitType := AInitType;
  FCustomInitializationBlock := AInitializationBlock;
  FSingletonInstance := nil;
end;

{ ContainerUtils }

class function DIContainerUtils.GetQualifiedClassName(AClass: TClass): String;
begin
  Result := AClass.UnitName + '.' + AClass.classname;
end;

destructor TDIContainerItem.Destroy;
begin
  FCustomInitializationBlock := nil;
  if Assigned(FSingletonInstance) then
  begin
    if FSingletonInstance is TInterfacedObject then
    begin
      if TInterfacedObject(FSingletonInstance).RefCount <= 1 then
        FSingletonInstance := nil
      else
        raise EDIContainer.CreateFmt(
          'Singleton service [%s] still had [%d] references, cannot free it',
          [DIContainerUtils.GetQualifiedClassName
          (FSingletonInstance.ClassType),
          TInterfacedObject(FSingletonInstance).RefCount]);
    end
    else
      FSingletonInstance.Free;
  end;
  inherited;
end;

procedure TDIContainerItem.SetCustomInitializationBlock
  (const Value: TFunc<TObject>);
begin
  FCustomInitializationBlock := Value;
end;

procedure TDIContainerItem.SetInitType(const Value: TDIContainerInitType);
begin
  FInitType := Value;
end;

procedure TDIContainerItem.SetSingletonInstance(const Value: TObject);
begin
  FSingletonInstance := Value;
end;

end.
