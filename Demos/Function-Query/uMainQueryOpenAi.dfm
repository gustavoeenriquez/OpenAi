object Form64: TForm64
  Left = 0
  Top = 0
  Caption = 'Ejecutar Query desde OpenAI Chat'
  ClientHeight = 670
  ClientWidth = 901
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 901
    Height = 123
    Align = alTop
    TabOrder = 0
    object SpeedButton1: TSpeedButton
      Left = 814
      Top = 1
      Width = 86
      Height = 121
      Align = alRight
      OnClick = SpeedButton1Click
      ExplicitLeft = 704
      ExplicitTop = -3
    end
    object MemoPrompt: TMemo
      Left = 1
      Top = 1
      Width = 813
      Height = 121
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 123
    Width = 901
    Height = 309
    Align = alClient
    TabOrder = 1
    object DBGrid1: TDBGrid
      Left = 1
      Top = 1
      Width = 899
      Height = 307
      Align = alClient
      DataSource = DataSource1
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -12
      TitleFont.Name = 'Segoe UI'
      TitleFont.Style = []
    end
  end
  object Memo2: TMemo
    Left = 0
    Top = 432
    Width = 901
    Height = 238
    Align = alBottom
    TabOrder = 2
  end
  object Button1: TButton
    Left = 818
    Top = 32
    Width = 75
    Height = 65
    Caption = 'Enviar'
    TabOrder = 3
  end
  object AiOpenChat1: TAiOpenChat
    ApiKey = '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'
    Model = 'gpt-3.5-turbo-1106'
    Logprobs = False
    Max_tokens = 300
    N = 1
    Response_format = tiaChatRfJson
    Seed = 0
    Asynchronous = False
    Temperature = 1.000000000000000000
    Tools.Strings = (
      '['
      '   {'
      '      "type":"function",'
      '      "function":{'
      '         "name":"get_fecha",'
      '         "description":"obtiene la fecha y la hora del sistema",'
      '         "parameters":{'
      ''
      '         }'
      '      }'
      '   },'
      '   {'
      '      "type":"function",'
      '      "function":{'
      '         "name":"consulta_ventas",'
      
        '         "description":"crea consultas sobre la base de datos de' +
        ' ventas \r\n La tabla de ventas tiene la siguiente estructura: \' +
        'r\n create table ventas ( \r\n fecha date, \r\n cod_producto cha' +
        'racter varying(20), \r\n cod_sede character varying(20), \r\n co' +
        'd_ciudad character varying(20), \r\n cantidad double precision, ' +
        '\r\n descuento double precision, \r\n impuesto double precision,' +
        ' \r\n total double precision, \r\n neto double precision, \r\n p' +
        'rimary key (fecha, cod_producto, cod_sede, cod_ciudad))",'
      '         "parameters":{'
      '            "type":"object",'
      '            "properties":{'
      '               "query":{'
      '                  "type":"string",'
      
        '                  "description":"Env'#237'a un query a la base de dat' +
        'os para retornar los datos"'
      '               }'
      '            }'
      '         },'
      '         "required":['
      '            "query"'
      '         ]'
      '      }'
      '   }'
      ']'
      ''
      ''
      ''
      ''
      ''
      '')
    Tool_Active = True
    User = 'user'
    InitialInstructions.Strings = (
      'Eres un asistente muy '#250'til y servicial')
    Prompt_tokens = 0
    Completion_tokens = 0
    Total_tokens = 0
    OnReceiveDataEnd = AiOpenChat1ReceiveDataEnd
    OnCallToolFunction = AiOpenChat1CallToolFunction
    Left = 280
    Top = 224
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    Left = 192
    Top = 328
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=pruebas'
      'User_Name=admin'
      'Password=masterkey'
      'Server=192.168.0.109'
      'DriverID=PG')
    LoginPrompt = False
    Left = 392
    Top = 344
  end
  object DataSource1: TDataSource
    DataSet = FDQuery1
    Left = 384
    Top = 256
  end
  object FDStanStorageJSONLink1: TFDStanStorageJSONLink
    Left = 544
    Top = 256
  end
end
