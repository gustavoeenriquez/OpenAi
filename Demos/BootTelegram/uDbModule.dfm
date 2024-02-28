object DbModule: TDbModule
  OnCreate = DataModuleCreate
  Height = 480
  Width = 640
  object Chat1: TAiOpenChat
    ApiKey = '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'
    Model = 'gpt-3.5-turbo-1106'
    Logprobs = False
    Max_tokens = 300
    N = 1
    Response_format = tiaChatRfText
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
      '         "name":"genera_foto",'
      
        '         "description":"crea im'#225'genes utilizando la tecnolog'#237'a d' +
        'all-e, retorna el nombre del archivo generado",'
      '         "parameters":{'
      '            "type":"object",'
      '            "properties":{'
      '               "peticion":{'
      '                  "type":"string",'
      
        '                  "description":"la petici'#243'n es el texto que se ' +
        'env'#237'a a dall-e para la generaci'#243'n de la im'#225'gen"'
      '               }'
      '            }'
      '         },'
      '         "required":['
      '            "peticion"'
      '         ]'
      '      }'
      '   },'
      '   {'
      '      "type":"function",'
      '      "function":{'
      '         "name":"busca_foto",'
      
        '         "description":"busca una foto convirtiendo la petici'#243'n ' +
        'en un embedding para busquedas contextuales, retorna la lista de' +
        ' nombres de archivos encontrados",'
      '         "parameters":{'
      '            "type":"object",'
      '            "properties":{'
      '               "peticion":{'
      '                  "type":"string",'
      
        '                  "description":"la petici'#243'n es el texto que se ' +
        'env'#237'a para generar el embedding y realizar la b'#250'squeda"'
      '               }'
      '            }'
      '         },'
      '         "required":['
      '            "peticion"'
      '         ]'
      '      }'
      '   }'
      ']')
    Tool_Active = True
    User = 'user'
    InitialInstructions.Strings = (
      'Eres un asistente muy '#250'til y servicial')
    Prompt_tokens = 0
    Completion_tokens = 0
    Total_tokens = 0
    OnCallToolFunction = Chat1CallToolFunction
    Left = 192
    Top = 224
  end
  object DbConn: TFDConnection
    Params.Strings = (
      'Database=pruebas'
      'User_Name=admin'
      'Password=masterkey'
      'Server=192.168.0.110'
      'DriverID=PG')
    LoginPrompt = False
    Left = 320
    Top = 232
  end
end
