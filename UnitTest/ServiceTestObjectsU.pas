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

unit ServiceTestObjectsU;

interface

type
//  IInterfaceService1 = interface
//  ['{C17AB2A5-63E6-40B8-9BDC-F31B76BE2361}']
//    function GetMessage: String;
//    procedure SetMessage(Value: String);
//    function ObjectClassName: String;
//  end;

  TInterfacedService1 = class(TObject)
  private
    FMessage: String;
  public
    constructor Create;
    function GetMessage: String;
    procedure SetMessage(Value: String);
    function ObjectClassName: String;
    class procedure Register;
  end;

  TAnotherInterfacedService1 = class(TObject)
  private
    FMessage: String;
  public
    constructor Create;
    function GetMessage: String;
    procedure SetMessage(Value: String);
    function ObjectClassName: String;
    class procedure Register;
  end;


  IInterfaceService2 = interface
  ['{6CADD01A-8546-49B0-AE23-F458F387484C}']
    function GetMessage: String;
    procedure SetMessage(Value: String);
  end;

  IInterfaceService6 = IInterfaceService2;

  TInterfacedService2 = class(TObject)
  private
    FMessage: String;
  public
    constructor Create;
    function GetMessage: String;
    procedure SetMessage(Value: String);
  end;


  TService1 = class
  private
    FMessage: string;
    procedure SetMessage(const Value: String);
    function GetMessage: String;
  public
    constructor Create;
    property Message: String read GetMessage write SetMessage;
  end;

  TService2 = class
  private
    FMessage: string;
    procedure SetMessage(const Value: String);
    function GetMessage: String;
  public
    constructor Create;
    property Message: String read GetMessage write SetMessage;
  end;

  TService3 = class
  public
    constructor Create(Service1: TService1; Service2: TService2);
    destructor Destroy; override;
    function GetCompoundMessage: String;
  private
    FService1: TService1;
    FService2: TService2;
  end;

  TService5 = class;
  TService4 = class
  private
    FService5: TService5;
  public
    constructor Create(Service5: TService5);
  end;
  TService5 = class
  private
    FService4: TService4;
  public
    constructor Create(Service4: TService4);
  end;

  TService6 = class(TObject)
  private
    FMessage: string;
    s1: TService1;
    s2: TService1;
  public
    constructor Create(Service1A, Service1B: TService1);
    destructor Destroy; override;
    function ToString: String; override;
    function GetMessage: string;
    procedure SetMessage(Value: string);
  end;

  TService7 = class(TService6)
  end;

implementation

uses
  SysUtils;

{ TService2 }

constructor TService2.Create;
begin
  FMessage := 'GOODBYE';
  inherited;
end;

function TService2.GetMessage: String;
begin
  Result := FMessage;
end;

procedure TService2.SetMessage(const Value: String);
begin
  FMessage := Value;
end;

{ TService1 }

constructor TService1.Create;
begin
  inherited;
end;

function TService1.GetMessage: String;
begin
  Result := FMessage;
end;

procedure TService1.SetMessage(const Value: String);
begin
  FMessage := Value;
end;

{ TService3 }

constructor TService3.Create(Service1: TService1; Service2: TService2);
begin
  inherited Create;
  FService1 := Service1;
  FService2 := Service2;
end;

destructor TService3.Destroy;
begin
  inherited;
end;

function TService3.GetCompoundMessage: String;
begin
  Result := FService1.Message + FService2.Message;
end;

{ TInterfacedService1 }

constructor TInterfacedService1.Create;
begin
  inherited;
end;

function TInterfacedService1.GetMessage: String;
begin
  Result := FMessage;
end;

function TInterfacedService1.ObjectClassName: String;
begin
  Result := ClassName;
end;

class procedure TInterfacedService1.Register;
begin
  //Empty
end;

procedure TInterfacedService1.SetMessage(Value: String);
begin
  FMessage := Value;
end;

{ TInterfacedService2 }

constructor TInterfacedService2.Create;
begin
  inherited;
end;

function TInterfacedService2.GetMessage: String;
begin
  Result := FMessage;
end;

procedure TInterfacedService2.SetMessage(Value: String);
begin
  FMessage := Value;
end;

{ TService4 }

constructor TService4.Create(Service5: TService5);
begin
  inherited Create;
  FService5 := Service5;
end;

{ TService5 }

constructor TService5.Create(Service4: TService4);
begin
  inherited Create;
  FService4 := Service4;
end;

{ TService6 }

constructor TService6.Create(Service1A, Service1B: TService1);
begin
  inherited Create;
  s1 := Service1A;
  s2 := Service1B;
end;

destructor TService6.Destroy;
begin
  inherited;
end;

function TService6.GetMessage: string;
begin
  Result := FMessage;
end;

procedure TService6.SetMessage(Value: string);
begin
  FMessage := Value;
end;

function TService6.ToString: String;
begin
  Result := Format('This object (%d) has been built with a %s and a %s', [cardinal(self), s1.ClassName, s2.ClassName]);
end;

{ TAnotherInterfacedService1 }

constructor TAnotherInterfacedService1.Create;
begin
  inherited;
end;

function TAnotherInterfacedService1.GetMessage: String;
begin
  Result := FMessage + ' (from ' + ClassName + ')';
end;

function TAnotherInterfacedService1.ObjectClassName: String;
begin
  Result := ClassName;
end;

class procedure TAnotherInterfacedService1.Register;
begin
  //
end;

procedure TAnotherInterfacedService1.SetMessage(Value: String);
begin
  FMessage := Value + ' ' + datetimetostr(now);
end;

initialization
  TInterfacedService1.Register;
  TAnotherInterfacedService1.Register;

end.
