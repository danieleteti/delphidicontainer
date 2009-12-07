unit TestObjectsU;

interface

type
  TService1 = class
    function SayHello: String;
    constructor Create;
  end;

  TService2 = class
    function SayGoodbye: String;
    constructor Create;
  end;

  TService3 = class
  public
    constructor Create(Service1: TService1; Service2: TService2);
    function SayAll: String;

  private
    FService1: TService1;
    FService2: TService2;
  end;

implementation

{ TService2 }

constructor TService2.Create;
begin
  inherited;
end;

function TService2.SayGoodbye: String;
begin
  Result := 'Goodbye';
end;

{ TService1 }

constructor TService1.Create;
begin
  inherited;
end;

function TService1.SayHello: String;
begin
  Result := 'Sayhello';
end;

{ TService3 }

constructor TService3.Create(Service1: TService1; Service2: TService2);
begin
  inherited Create;
  FService1 := Service1;
  FService2 := Service2;
end;

function TService3.SayAll: String;
begin
  Result := 'SAYALL [' + FService1.SayHello + ' --> ' + FService2.SayGoodbye + ']';
end;

end.
