object Form6: TForm6
  Left = 0
  Top = 0
  Caption = 'Form6'
  ClientHeight = 358
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 433
    Height = 121
    Caption = 
      'Click me, but I'#39'll always use alias "myservice" and cast'#13#10'return' +
      'ed object to "IInterfaceService1".'#13#10'If you want to change implem' +
      'entation, modify file container.conf'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    WordWrap = True
    OnClick = Button1Click
  end
end
