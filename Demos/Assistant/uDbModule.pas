unit uDbModule;

interface

uses
  System.SysUtils, System.Classes, System.JSON, Rest.JSON, System.IOUtils, System.StrUtils,
  System.DateUtils, System.Threading,
  Winapi.Windows, Winapi.ShellAPI, System.UITypes,

  fastTelega.AvailableTypes, fastTelega.Bot, fastTelega.LongPoll,

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Phys.PGDef,
  FireDAC.FMXUI.Wait, FireDAC.Comp.UI, FireDAC.Phys.PG, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.VCLUI.Wait,
  uOpenAi, FireDAC.Stan.StorageJSON;

type

  TOnTeleMessage = Procedure(Sender: TObject; Msg: String) of object;

  TCita = Class(TObject)
  Public
    id: Integer;
    dia: TDateTime;
    ffechahora_inicial: TDateTime;
    ffechahora_final: TDateTime;
    asunto: String;
    categoria: String;
    nom_paciente: String;
    ffecha: TDateTime;
  End;

  THistoria = Class(TObject)
  Public
    id: Integer;
    cod_paciente: String;
    dia: TDateTime;
    ffechahora_inicial: TDateTime;
    ffechahora_final: TDateTime;
    asunto: String;
    categoria: String;
    nom_paciente: String;
    ffecha: TDateTime;
  End;

  TBotAssistant = Class(TObject)
  private
    FOnTeleMessage: TOnTeleMessage;
    Bot: TFTBot;
    FActive: Boolean;
    FApiKey: String;
    procedure SetOnTeleMessage(const Value: TOnTeleMessage);
    procedure SetActive(const Value: Boolean);
    procedure SetApiKey(const Value: String);
  Public
    LongPoll: TFTLongPoll;
    Constructor Create(aApiKey: String);
    Procedure Run;
  published
    Property OnTeleMessage: TOnTeleMessage read FOnTeleMessage write SetOnTeleMessage;
    Property Active: Boolean read FActive write SetActive;
    Property ApiKey: String read FApiKey write SetApiKey;

  End;

  TDbModule = class(TDataModule)
    DBConn: TFDConnection;
    procedure DataModuleCreate(Sender: TObject);
  private
    FApiKey: String;
    procedure SetApiKey(const Value: String);
    { Private declarations }
  public
    Function NewQuery: TFDQuery;

    Function Cita_Delete(id: Integer): Boolean;
    Function Cita_Insert(jCita: TJSonObject): String;
    Function Cita_Get(id: String = ''): TJSonArray;

    Function Historia_Delete(id: Integer): Boolean;
    Function Historia_Insert(Params: TJSonObject): String;
    Function Historia_Get(NomPaciente, Descripcion: String): TJSonArray;

    Function Consulta_Ventas(Params: TJSonObject): String;

    Property ApiKey: String read FApiKey write SetApiKey;

  end;


Const

  bot_key = 'Escriba aquí el BotId que le indica telegram';
  Apikey  = 'Escriba aquí el ApiKey que le indica OpenAI';
  IdChatUsuario = 3242423423; //Escriba aquí el Id del Chat de telegram con su usuario;
  IdChatGrupo = -23523523452; //Escriba aquí el Id del canal del bot de telegram;


  Ico_Connect = 0;
  Ico_Disconnect = 1;
  Ico_clear = 2;
  Ico_Trash = 3;
  Ico_Delete = 4;
  Ico_Refresh = 5;
  Ico_Attach = 6;
  Ico_Upload = 7;
  Ico_Download = 8;
  Ico_Play = 9;
  Ico_Pause = 10;
  Ico_doc = 11;
  Ico_Save = 12;
  Ico_Config = 13;
  Ico_Mic = 14;
  Ico_Internet = 15;
  Ico_Chat = 16;

  // var
  // DbModule: TDbModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}
{$R *.dfm}
{ TDbModule }

function TDbModule.NewQuery: TFDQuery;
begin
  Result := TFDQuery.Create(Self);
  Result.Connection := DBConn;
end;

procedure TDbModule.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

function TDbModule.Cita_Delete(id: Integer): Boolean;
Var
  Query: TFDQuery;
begin
  Query := NewQuery;
  Try
    Try
      Query.SQL.Add('delete from citas where id = :Id');
      Query.Params.ParamByName('id').AsInteger := id;
      Query.ExecSQL;
      Result := True;
    Finally
      Query.Free;
    End;
  Except
    ON E: Exception do
    Begin
      Result := False;
    End;
  End;
end;

function TDbModule.Cita_Get(id: String): TJSonArray;
Var
  Query: TFDQuery;
  JObj: TJSonObject;
begin
  Query := NewQuery;
  Try
    Try
      Query.SQL.Add('Select Id, Dia, fechahora_inicial, fechahora_final,');
      Query.SQL.Add('     asunto, categoria, nom_paciente, fecha');
      Query.SQL.Add('from citas');
      If id.Trim <> '' then
        Query.SQL.Add('Where id = :Id');
      Query.SQL.Add('order by 1,2');

      If id.Trim <> '' then
        Query.Params.ParamByName('Id').AsInteger := id.ToInteger;

      Query.Open;

      Result := TJSonArray.Create;

      While not Query.Eof do
      Begin
        JObj := TJSonObject.Create;
        JObj.AddPair('id', Query.FieldByName('id').AsString);
        JObj.AddPair('dia', Query.FieldByName('dia').AsString);
        JObj.AddPair('fechahora_inicial', Query.FieldByName('fechahora_inicial').AsString);
        JObj.AddPair('fechahora_final', Query.FieldByName('fechahora_final').AsString);
        JObj.AddPair('asunto', Query.FieldByName('asunto').AsString);
        JObj.AddPair('categoria', Query.FieldByName('categoria').AsString);
        JObj.AddPair('nom_paciente', Query.FieldByName('nom_paciente').AsString);
        JObj.AddPair('fecha', Query.FieldByName('fecha').AsString);
        Result.Add(JObj);
        Query.Next;
      End;
    Finally
      Query.Free;
    End;
  Except
    ON E: Exception do
    Begin
      Result := TJSonArray.Create.Add(TJSonObject.Create.AddPair('error', 'Ocurrió el siguiente error :' + E.Message));
    End;
  End;
end;

function TDbModule.Cita_Insert(jCita: TJSonObject): String;
Var
  Query: TFDQuery;
  S: String;
  Cita: TCita;
begin

  Try

    Cita := TJson.JsonToObject<TCita>(jCita);

    Query := NewQuery;
    Try
      Query.SQL.Clear;
      Query.SQL.Add('INSERT INTO public.citas(');
      Query.SQL.Add('	dia, fechahora_inicial, fechahora_final, asunto, categoria, nom_paciente)');
      Query.SQL.Add('	VALUES (:dia, :fechahora_inicial, :fechahora_final, :asunto, :categoria, :nom_paciente)');
      Query.SQL.Add('Returning ID');

      Query.ParamByName('dia').AsDateTime := Cita.dia;
      Query.ParamByName('fechahora_inicial').AsDateTime := Cita.ffechahora_inicial;
      Query.ParamByName('fechahora_final').AsDateTime := Cita.ffechahora_final;
      Query.ParamByName('asunto').AsString := Cita.asunto;
      Query.ParamByName('categoria').AsString := Cita.categoria;
      Query.ParamByName('nom_paciente').AsString := Cita.nom_paciente;
      Query.Open;
      Result := Query.FieldByName('id').AsString;
    Finally
      Query.Free;
    End;
  Except
    ON E: Exception do
    Begin
      Result := 'Ocurrió el siguiente error :' + E.Message;
    End;
  End;
end;

function TDbModule.Consulta_Ventas(Params: TJSonObject): String;
Var
  SQL: String;
  Query: TFDQuery;
  ST: TStringStream;
begin
  Query := NewQuery;
  SQL := Params.GetValue<String>('query');
  ST := TStringStream.Create;

  Try
    Try
      Query.SQL.Text := SQL;
      Query.Open;
      Query.SaveToStream(ST, TFDStorageFormat.sfJSON);
      Result := ST.DataString;
    Finally
      Query.Free;
      ST.Free;
    End;
  Except
    ON E: Exception do
    Begin
      Result := 'Error: ' + E.Message;
    End;
  End;
end;

procedure TDbModule.DataModuleCreate(Sender: TObject);
begin
  FormatSettings.DateSeparator := '-';
  FormatSettings.TimeSeparator := ':';
  FormatSettings.ThousandSeparator := ',';
  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  FormatSettings.ShortTimeFormat := 'hh:mm:ss';

end;

function TDbModule.Historia_Delete(id: Integer): Boolean;
Var
  Query: TFDQuery;
begin
  Query := NewQuery;
  Try
    Try
      Query.SQL.Add('delete from historia where id = :Id');
      Query.Params.ParamByName('id').AsInteger := id;
      Query.ExecSQL;
      Result := True;
    Finally
      Query.Free;
    End;
  Except
    ON E: Exception do
    Begin
      Result := False;
    End;
  End;
end;

function TDbModule.Historia_Get(NomPaciente, Descripcion: String): TJSonArray;
Var
  Query: TFDQuery;
  JObj: TJSonObject;
  sEmbedding: String;
  AiEmb: TAiEmbeddings;
  Emb: TEmbedding;
begin

  Try
    If Descripcion.Trim <> '' then
    Begin
      AiEmb := TAiEmbeddings.Create(ApiKey);
      Try
        Emb := AiEmb.CreateEmbedding(NomPaciente + sLineBreak + Descripcion, 'user')[0];
        sEmbedding := TAiEmbeddings.ToJsonArray(Emb).ToJSON;
      Finally
        AiEmb.Free;
      End;
    End;

    Query := NewQuery;
    Try
      Query.SQL.Add('Select Id, Dia, cod_paciente, fechahora_inicial, fechahora_final,');
      Query.SQL.Add('     asunto, categoria, nom_paciente, fecha');
      Query.SQL.Add('from historia');
      Query.SQL.Add('Where 1=1');

      If NomPaciente <> '' then
        Query.SQL.Add('and Upper(nom_paciente) = :NomPaciente');

      If Descripcion.Trim <> '' then
        Query.SQL.Add('  and embedding <-> ''' + sEmbedding + ''' < 5.5');

      If NomPaciente <> '' then
        Query.Params.ParamByName('NomPaciente').AsString := Uppercase(NomPaciente);
      Query.Open;

      Result := TJSonArray.Create;

      While not Query.Eof do
      Begin
        JObj := TJSonObject.Create;
        JObj.AddPair('id', Query.FieldByName('id').AsString);
        JObj.AddPair('cod_paciente', Query.FieldByName('cod_paciente').AsString);
        JObj.AddPair('dia', Query.FieldByName('dia').AsString);
        JObj.AddPair('fechahora_inicial', Query.FieldByName('fechahora_inicial').AsString);
        JObj.AddPair('fechahora_final', Query.FieldByName('fechahora_final').AsString);
        JObj.AddPair('asunto', Query.FieldByName('asunto').AsString);
        JObj.AddPair('categoria', Query.FieldByName('categoria').AsString);
        JObj.AddPair('nom_paciente', Uppercase(Query.FieldByName('nom_paciente').AsString));
        JObj.AddPair('fecha', Query.FieldByName('fecha').AsString);
        Result.Add(JObj);
        Query.Next;
      End;
    Finally
      Query.Free;
    End;
  Except
    ON E: Exception do
    Begin
      Result := TJSonArray.Create.Add(TJSonObject.Create.AddPair('error', 'Ocurrió el siguiente error :' + E.Message));
    End;
  End;
end;

function TDbModule.Historia_Insert(Params: TJSonObject): String;
Var
  Query: TFDQuery;
  sObj, sEmbedding: String;
  AiEmb: TAiEmbeddings;
  Emb: TEmbedding;
  Historia: THistoria;

begin

  Try
    sObj := Params.ToJSON;

    Historia := TJson.JsonToObject<THistoria>(Params);

    AiEmb := TAiEmbeddings.Create(ApiKey);
    Try
      Emb := AiEmb.CreateEmbedding(sObj, 'user')[0];
      sEmbedding := TAiEmbeddings.ToJsonArray(Emb).ToJSON;
    Finally
      AiEmb.Free;
    End;

    Query := NewQuery;
    Try
      Query.SQL.Clear;
      Query.SQL.Add('INSERT INTO public.historia(');
      Query.SQL.Add('	cod_paciente, dia, fechahora_inicial, fechahora_final,');
      Query.SQL.Add('      asunto, categoria, nom_paciente, embedding)');
      Query.SQL.Add('	VALUES (:cod_paciente, :dia, :fechahora_inicial, :fechahora_final,');
      Query.SQL.Add('      :asunto, :categoria, :nom_paciente, ''' + sEmbedding + ''')');
      Query.SQL.Add('Returning Id');

      Query.ParamByName('dia').AsDate := Historia.dia;
      Query.ParamByName('cod_paciente').AsString := Historia.cod_paciente;
      Query.ParamByName('fechahora_inicial').AsDateTime := Historia.ffechahora_inicial;
      Query.ParamByName('fechahora_final').AsDateTime := Historia.ffechahora_final;
      Query.ParamByName('asunto').AsString := Historia.asunto;
      Query.ParamByName('categoria').AsString := Historia.categoria;
      Query.ParamByName('nom_paciente').AsString := Historia.nom_paciente;
      Query.Open;

      Result := Query.FieldByName('Id').AsString;
    Finally
      Query.Free;
    End;
  Except
    ON E: Exception do
    Begin
      Result := 'Ocurrió el siguiente error :' + E.Message;
    End;
  End;
end;

{ TBotAssistant }

constructor TBotAssistant.Create(aApiKey: String);
begin
  FApiKey := aApiKey;

  Bot := TFTBot.Create(bot_key, 'https://api.telegram.org');

  try

    Bot.Events.OnCommand('start',
      procedure(const FTMessage: TObject)
      begin
        Bot.API.sendMessage(TftMessage(FTMessage).Chat.id, 'Hola!');
      end);

    Bot.Events.OnAnyMessage(
      procedure(const FTMessage: TObject)
      Var
        Msg: TftMessage;
        Peticion: String;
        ST: TMemoryStream;
        FileInfo: TfTFileInfo;
        Audio: TAiAudio;

      begin
        Msg := TftMessage(FTMessage);
        Peticion := Msg.Text;

        If Active and Assigned(FOnTeleMessage) then
        Begin
          // Si el contenido en un mensaje de voz
          If Assigned(Msg.Voice) and (Msg.Voice.Duration > 0) then
          Begin
            Try
              FileInfo := Bot.API.getFileInfo(Msg.Voice.FileId);
              ST := Bot.API.downloadFile(FileInfo.filePath);
              Audio := TAiAudio.Create(ApiKey);
              Try

                ST.SaveToFile('c:\temp\audio3.ogg');
                ST.Position := 0;

                Peticion := Audio.Transcription(ST, 'audio3.ogg', 'transcript');
                Bot.API.sendMessage(Msg.Chat.id, 'Transcripcion: ' + Peticion);
              Finally
                ST.Free;
                Audio.Free;
              End;
            Except
              // No genere el evento de excepciones
            End;
          End;

          TTask.Run(
            Procedure
            Begin
              TThread.Synchronize(nil,
                procedure
                begin
                  If Active and Assigned(FOnTeleMessage) then
                    FOnTeleMessage(Self, Peticion);
                end);
            End);

        End;
      end);

    Bot.API.deleteWebhook();

    LongPoll := TFTLongPoll.Create(Bot);

  except
  end;

end;

procedure TBotAssistant.Run;
begin
  TTask.Run(
    Procedure
    Begin
      while (True) do
      begin
        LongPoll.start();
      end
    End);
end;

procedure TBotAssistant.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TBotAssistant.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TBotAssistant.SetOnTeleMessage(const Value: TOnTeleMessage);
begin
  FOnTeleMessage := Value;
end;

end.
