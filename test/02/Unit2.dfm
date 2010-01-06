object Form2: TForm2
  Left = 537
  Top = 165
  BorderStyle = bsSingle
  Caption = 'DIContainer - Sample 2'
  ClientHeight = 345
  ClientWidth = 583
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button2: TButton
    Left = 365
    Top = 72
    Width = 200
    Height = 58
    Caption = '2. Use service'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 365
    Top = 8
    Width = 200
    Height = 58
    Caption = '1. Init DIContainer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 365
    Top = 136
    Width = 200
    Height = 58
    Caption = '3. Free DIContainer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    OnClick = Button4Click
  end
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 326
    Height = 339
    Align = alLeft
    Lines.Strings = (
      'In this demo you can see a simple configuration '
      'for DelphiDIContainer.'
      'Run "configure_csv_dataservice.bat" in the '
      'program folder and this demo will use '
      'a datasevice based on CSV.'
      'Run "configure_padded_dataservice.bat" in the '
      'program folder and this demo will use '
      'a datasevice based on Padded values.'
      ''
      'This configuration are completely decoupled '
      'from demo code.'
      'This demo program know only base class for '
      'dataservice TDataService.'
      ''
      'STEP TO FOLLOW'
      '1. Initialize DIContainer reading simple config file named '
      '"Test02.config"'
      '2. Use the service by "NAME"'
      '3. Free the container'
      '')
    ReadOnly = True
    TabOrder = 3
  end
end
