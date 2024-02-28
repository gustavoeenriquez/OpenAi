unit uDbModule;

interface

uses
  System.SysUtils, System.Classes, uAiOpenChat, uOpenAi, System.JSON,
  Rest.JSON, System.IOUtils,
  fastTelega.AvailableTypes, fastTelega.Bot, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.ConsoleUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Phys.PGDef, FireDAC.Phys.PG, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FMX.Graphics;

type
  TDbModule = class(TDataModule)
    Chat1: TAiOpenChat;
    DbConn: TFDConnection;
    procedure Chat1CallToolFunction(Sender: TObject; AiToolCall: TAiToolsFunction);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
     Bot: TftBot;
    Function Genera_Foto(peticion: String): String;
    Function InsertarImagen(peticion, descripcion, path: String; embedding: TEmbeddingArray): String;
    Function Busca_Foto(peticion: String): String;
  end;

Const
  bot_key = 'Escriba aquí el BotKey de Telegram';
  Apikey = 'Escriba aquí el ApiKey de OpenAi';
  IdChatGustavo = 3423423423; // Escriba aquí el id del chat de usuario de telegram
  IdChatCimaGrupo = -654983123; // Escriba aquí el id del chatbot de telegram

var
  DbModule: TDbModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}
{$R *.dfm}

function TDbModule.Busca_Foto(peticion: String): String;
Var
  Query: TFDQuery;
  Emb : TEmbedding;
  Embedding : TAiEmbeddings;
  sEmb, FileName : String;
begin

     Query := TFDQuery.Create(Self);
     Query.Connection := DbConn;
     Embedding := TAiEmbeddings.Create(Chat1.ApiKey);
   Try

    Emb := embedding.CreateEmbedding(peticion, 'user')[0];
    sEmb := TAiEmbeddings.ToJsonArray(Emb).ToJSON;

    Query.Sql.Clear;
    Query.Sql.Add('SELECT id, path, embedding <-> ''' + sEmb + ''' as distancia');
    Query.Sql.Add('FROM imagenes');
    Query.Sql.Add('ORDER BY embedding <-> ''' + sEmb + '''');
    Query.Sql.Add('LIMIT 5');
    Query.Open;

    While not Query.Eof do
    Begin
      FileName := Query.FieldByName('path').AsString;
      Bot.API.sendPhoto(IdChatGustavo, TftInputFile.fromFile(FileName, 'image/png'), '');

      Result := Result+sLineBreak+FileName;

      Query.Next;
    End;

   Finally
     Query.Free;
   End;

end;

procedure TDbModule.Chat1CallToolFunction(Sender: TObject; AiToolCall: TAiToolsFunction);
Var
  JObj: TJSonObject;
  peticion: String;
begin

  If AiToolCall.Name = 'get_fecha' then
  Begin
    AiToolCall.Response := FormatDateTime('YYYY-MM-DD hh:nn.ss', Now);
  End;

  If AiToolCall.Name = 'genera_foto' then
  Begin
    JObj := TJSonObject(TJSonObject.ParseJSONValue(AiToolCall.Arguments));
    peticion := JObj.GetValue<String>('peticion');
    AiToolCall.Response := Genera_Foto(peticion);
  End;

  If AiToolCall.Name = 'busca_foto' then
  Begin
    JObj := TJSonObject(TJSonObject.ParseJSONValue(AiToolCall.Arguments));
    peticion := JObj.GetValue<String>('peticion');
    AiToolCall.Response := Busca_Foto(peticion);
  End;

end;

procedure TDbModule.DataModuleCreate(Sender: TObject);
begin
  Bot := TftBot.Create(bot_key, 'https://api.telegram.org');
end;

function TDbModule.Genera_Foto(peticion: String): String;
Var
  Imagen: TAiImages;
  Vision: TAiVision;
  embedding: TAiEmbeddings;
  FFile: TAiImagesFile;
  FileName, descripcion, Id: String;
  Emb: TEmbeddingArray;
begin

  Imagen := TAiImages.Create(Chat1.ApiKey);
  Vision := TAiVision.Create(Chat1.ApiKey);
  embedding := TAiEmbeddings.Create(Chat1.ApiKey);

  FileName := 'c:\temp\demo_' + TPath.GetGUIDFileName + '.png';

  Try

    FFile := Imagen.Generate(peticion, 'user', TAiImageSize.TiaSize512, 1, TAiImageResponseFormat.tiaRUrl, True, TAiImageAStyleFormat.tiaStyleVivid, True);
    FFile.Image.SaveToFile(FileName);

    Bot.API.sendMessage(IdChatGustavo, 'generando imagen...');

    Descripcion := Vision.GenerateByUrl('genera un texto sobre la imagen para crear un embedding de busqueda contextual', FFile.Url);

    Emb := embedding.CreateEmbedding(Peticion+'.'+sLineBreak+descripcion, 'user');

    Id := InsertarImagen(peticion, Descripcion, FileName, Emb);

    Result := 'La imagen se generó con el id = '+Id+' y con el nombre de '+FileName;

    Bot.API.sendPhoto(IdChatGustavo, TftInputFile.fromFile(FileName, 'image/png'), FileName);

  Finally
    Imagen.Free;
    Vision.Free;
    embedding.Free;
  End;
end;

function TDbModule.InsertarImagen(peticion, descripcion, path: String; embedding: TEmbeddingArray): String;
Var
  Query: TFDQuery;
  I: Integer;
  sEmbedding: String;
begin

  Query := TFDQuery.Create(Self);
  Query.Connection := DbConn;

  Try
    For I := 0 to Length(embedding) - 1 do
    Begin

      sEmbedding := TAiEmbeddings.ToJsonArray(embedding[I]).ToJSON;

      Query.Sql.Clear;
      Query.Sql.Add('INSERT INTO public.imagenes(');
      Query.Sql.Add('peticion, descripcion, path, embedding)');
      Query.Sql.Add('VALUES (:peticion, :descripcion, :path, ''' + sEmbedding + ''')');
      Query.Sql.Add('Returning Id');
      Query.Params.ParamByName('peticion').AsString := peticion;
      Query.Params.ParamByName('descripcion').AsString := descripcion;
      Query.Params.ParamByName('path').AsString := path;
      // Query.Params.ParamByName('embedding').AsString := TAiEmbeddings.ToJsonArray(embedding[I]).ToJSON;
      Query.Open;

      Result := Query.FieldByName('Id').AsString;
    End;
  Finally
    Query.Free;
  End;
end;

end.
