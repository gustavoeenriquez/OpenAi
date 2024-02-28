object DbModule: TDbModule
  OnCreate = DataModuleCreate
  Height = 480
  Width = 640
  object DBConn: TFDConnection
    Params.Strings = (
      'User_Name=admin'
      'Password=masterkey'
      'Server=192.168.0.110'
      'Database=pruebas'
      'DriverID=PG')
    LoginPrompt = False
    Left = 112
    Top = 96
  end
end
