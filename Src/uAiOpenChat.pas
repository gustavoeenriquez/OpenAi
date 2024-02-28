//  IT License
//
//  Copyright (c) <year> <copyright holders>
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  o use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in
//  all copies or substantial portions of the Software.
//
//  HE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
//  THE SOFTWARE.
//
//Nombre: Gustavo Enríquez
//  Redes Sociales:
//- Email: gustavoeenriquez@gmail.com
//  - Telegram: +57 3128441700
//  - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
//  - Youtube: https://www.youtube.com/@cimamaker3945
//  - GitHub: https://github.com/gustavoeenriquez/


unit uAiOpenChat;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Threading, System.NetEncoding,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  System.JSON, Rest.JSON, uOpenAi;

type

  TAiOpenChatMessage = Class(TObject)
  Private
    FRole: String;
    FContent: String;
    FPrompt: String;
    FCompletion_tokens: integer;
    FTotal_tokens: integer;
    FPrompt_tokens: integer;
    FId: integer;
    FTollCallId: String;
    FFunctionName: String;
    FTool_calls: String;
    FVisionUrls: TStringList;
    FVisionBase64: TStringList;

    procedure SetContent(const Value: String);
    procedure SetRole(const Value: String);
    procedure SetPrompt(const Value: String);
    procedure SetFunctionName(const Value: String);
    procedure SetTollCallId(const Value: String);
    procedure SetTool_calls(const Value: String);
    procedure SetVisionUrls(const Value: TStringList);
    procedure SetVisionBase64(const Value: TStringList);
  Protected
  Public
    Constructor Create(aPrompt, aRole: String; aToolCallId: String = ''; aFunctionName: String = '');
    Destructor Destroy; Override;

    Procedure AddStreamImage(Stream: TMemoryStream);
    Function StreamToBase64(Stream: TMemoryStream): String;

    Property Id: integer Read FId;
    Property Role: String read FRole write SetRole;
    Property Content: String read FContent write SetContent;
    Property Prompt: String read FPrompt write SetPrompt;
    Property Prompt_tokens: integer read FPrompt_tokens;
    Property Completion_tokens: integer read FCompletion_tokens;
    Property Total_tokens: integer read FTotal_tokens;
    Property TollCallId: String read FTollCallId write SetTollCallId;
    Property FunctionName: String read FFunctionName write SetFunctionName;
    Property Tool_calls: String read FTool_calls write SetTool_calls;
    Property VisionUrls: TStringList read FVisionUrls write SetVisionUrls;
    Property VisionBase64: TStringList read FVisionBase64 write SetVisionBase64;
  End;

  TAiOpenChatMessages = Class(TList<TAiOpenChatMessage>)
  Private
    function GetAsText: String;
    procedure SetAsText(const Value: String);
  Protected
  Public
    Function ToJSon: TJSonArray;
    Procedure SaveToStream(Stream: TStream);
    Procedure SaveToFile(FileName: String);
    Procedure LoadFromStream(Stream: TStream);
    Procedure LoadFromFile(FileName: String);
    Property AsText: String Read GetAsText Write SetAsText;
  End;

  TAiOpenChatResponseFormat = (tiaChatRfText, tiaChatRfJson);
  TAiOpenChatDataEvent = procedure(const Sender: TObject; Msg: TAiOpenChatMessage; Response: TJSonObject; Role, Text: String) of object;
  TAiOpenChatBeforeSendEvent = procedure(const Sender: TObject; var Msg: TAiOpenChatMessage) of object;

  TAiOpenChat = class(TComponent)
  Private
    FOwner: TObject;
    FApiKey: String;
    FClient: TNetHTTPClient;
    FMessages: TAiOpenChatMessages;
    FSeed: integer;
    FTool_choice: string;
    FN: integer;
    FTop_p: Double;
    FLogprobs: Boolean;
    FResponse_format: TAiOpenChatResponseFormat;
    FFrequency_penalty: Double;
    FStop: string;
    FLogit_bias: String;
    FTemperature: Double;
    FPresence_penalty: Double;
    FUser: String;
    FMax_tokens: integer;
    FAsynchronous: Boolean;
    FTop_logprobs: String;
    FResponse: TStringStream;
    FOnReceiveDataEvent: TAiOpenChatDataEvent;
    FOnReceiveDataEnd: TAiOpenChatDataEvent;
    FModel: String;
    FInitialInstructions: TStrings;
    FTools: TStrings;
    FLastContent: String;
    FCompletion_tokens: integer;
    FTotal_tokens: integer;
    FPrompt_tokens: integer;
    FLastPrompt: String;
    FOnAddMessage: TAiOpenChatDataEvent;
    FTmpRole: String;
    FTmpResponseText: String;
    FTmpResponseText1: String;
    FAbort: Boolean;
    FBusy: Boolean;
    FOnCallToolFunction: TOnCallToolFunction;
    FTool_Active: Boolean;
    FOnBeforeSendMessage: TAiOpenChatBeforeSendEvent;

    procedure SetApiKey(const Value: String);
    procedure SetFrequency_penalty(const Value: Double);
    procedure SetLogit_bias(const Value: String);
    procedure SetLogprobs(const Value: Boolean);
    procedure SetMax_tokens(const Value: integer);
    procedure SetN(const Value: integer);
    procedure SetPresence_penalty(const Value: Double);
    procedure SetResponse_format(const Value: TAiOpenChatResponseFormat);
    procedure SetSeed(const Value: integer);
    procedure SetStop(const Value: string);
    procedure SetTemperature(const Value: Double);
    procedure SetTool_choice(const Value: string);
    procedure SetTop_p(const Value: Double);
    procedure SetUser(const Value: String);
    procedure SetAsynchronous(const Value: Boolean);
    procedure SetTop_logprobs(const Value: String);
    procedure SetOnReceiveDataEvent(const Value: TAiOpenChatDataEvent);
    procedure SetOnReceiveDataEnd(const Value: TAiOpenChatDataEvent);
    procedure SetModel(const Value: String);
    procedure SetInitialInstructions(const Value: TStrings);
    procedure SetTools(const Value: TStrings);
    procedure SetOnAddMessage(const Value: TAiOpenChatDataEvent);
    procedure SetOnCallToolFunction(const Value: TOnCallToolFunction);
    procedure SetTool_Active(const Value: Boolean);
    procedure SetOnBeforeSendMessage(const Value: TAiOpenChatBeforeSendEvent);
    procedure SetCompletion_tokens(const Value: integer);
    procedure SetPrompt_tokens(const Value: integer);
    procedure SetTotal_tokens(const Value: integer);
  Protected
    Procedure OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
    Function InternalAddMessage(aPrompt, aRole: String; aToolCallId: String = ''; aFunctionName: String = ''): String;
    Function InitChatCompletions: String;
    Procedure ParseChat(jObj: TJSonObject);
    Function ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;

  Public
    Constructor Create(Sender: TComponent); Override;
    Destructor Destroy; Override;
    Function AddMessageAndRun(aPrompt, aRole: String): String;
    Function NewMessage(aPrompt, aRole: String): TAiOpenChatMessage;
    Function Run(aMsg: TAiOpenChatMessage = Nil): String;
    Function GetLastMessage: TAiOpenChatMessage;
    Function RemoveMesage(Msg: TAiOpenChatMessage): Boolean; Overload;
    Function RemoveMesage(IdMsg: integer): Boolean; Overload;
    Procedure NewChat;
    Procedure Abort;

    Property Messages: TAiOpenChatMessages read FMessages;

  Published
    Property ApiKey: String read FApiKey write SetApiKey;
    Property Model: String read FModel write SetModel;
    Property Frequency_penalty: Double read FFrequency_penalty write SetFrequency_penalty; // -2 to 2
    Property Logit_bias: String read FLogit_bias write SetLogit_bias;
    // Vacío o entre -100 y 100
    Property Logprobs: Boolean read FLogprobs write SetLogprobs;
    Property Top_logprobs: String read FTop_logprobs write SetTop_logprobs;
    // vacio o between 0 and 5
    Property Max_tokens: integer read FMax_tokens write SetMax_tokens;
    // 0 = null o el máximo
    Property N: integer read FN write SetN;
    // Defaults to 1 How many Chat completion choices to generate for each input message.Note that you will be charged based on the number of generated tokens across all of the choices.Keep n as 1 to minimize costs.
    Property Presence_penalty: Double read FPresence_penalty write SetPresence_penalty; // Defaults to 0 number between - 2.0 and 2.0
    Property Response_format: TAiOpenChatResponseFormat read FResponse_format write SetResponse_format;
    // object Optional an object specifying the format that the model must output.Compatible with gpt - 4 - 1106 - preview and gpt - 3.5 - turbo - 1106.
    Property Seed: integer read FSeed write SetSeed; // 0 no se envía
    Property Stop: string read FStop write SetStop;
    // Array de palabras separado por comas
    Property Asynchronous: Boolean read FAsynchronous write SetAsynchronous;
    Property Temperature: Double read FTemperature write SetTemperature;
    // Defaults to 1  between 0 and 2.
    Property Top_p: Double read FTop_p write SetTop_p;
    // Defaults to 0 si es 0 no se envía,  entre 0 y 1
    Property Tools: TStrings read FTools write SetTools;
    Property Tool_choice: string read FTool_choice write SetTool_choice;
    Property Tool_Active: Boolean read FTool_Active write SetTool_Active;
    Property User: String read FUser write SetUser;
    Property InitialInstructions: TStrings read FInitialInstructions write SetInitialInstructions;
    Property Prompt_tokens: integer read FPrompt_tokens write SetPrompt_tokens;
    Property Completion_tokens: integer read FCompletion_tokens write SetCompletion_tokens;
    Property Total_tokens: integer read FTotal_tokens write SetTotal_tokens;
    Property LastContent: String Read FLastContent;
    Property LastPrompt: String Read FLastPrompt;
    Property Busy: Boolean Read FBusy;
    Property OnReceiveData: TAiOpenChatDataEvent read FOnReceiveDataEvent write SetOnReceiveDataEvent;
    Property OnReceiveDataEnd: TAiOpenChatDataEvent read FOnReceiveDataEnd write SetOnReceiveDataEnd;
    Property OnAddMessage: TAiOpenChatDataEvent read FOnAddMessage write SetOnAddMessage;
    Property OnCallToolFunction: TOnCallToolFunction read FOnCallToolFunction write SetOnCallToolFunction;
    Property OnBeforeSendMessage: TAiOpenChatBeforeSendEvent read FOnBeforeSendMessage write SetOnBeforeSendMessage;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('OpenAssistant', [TAiOpenChat]);
end;

{ TAiChat }

procedure TAiOpenChat.Abort;
begin
  FAbort := True;
end;

function TAiOpenChat.InternalAddMessage(aPrompt, aRole: String; aToolCallId: String = ''; aFunctionName: String = ''): String;
Var
  Msg: TAiOpenChatMessage;
begin

  Try
    // Comienza con las instrucciones iniciales y le adiciona cada 20 mensajes para evitar que se olvide
    If (FMessages.Count = 0) or ((FMessages.Count mod 20) = 0) then
    Begin
      Msg := TAiOpenChatMessage.Create(FInitialInstructions.Text, 'system');
      Msg.FId := FMessages.Count + 1;
      FMessages.Add(Msg);
      If Assigned(FOnAddMessage) then
        FOnAddMessage(Self, Msg, Nil, 'system', FInitialInstructions.Text);
    End;

    // Adiciona el mensaje a la lista
    Msg := TAiOpenChatMessage.Create(aPrompt, aRole, aToolCallId, aFunctionName);
    Msg.FId := FMessages.Count + 1;
    FMessages.Add(Msg);
    FLastPrompt := aPrompt;

    If (aToolCallId = '') and Assigned(FOnAddMessage) then
    Begin
      FOnAddMessage(Self, Msg, Nil, aRole, aPrompt);
    End;

    If Assigned(FOnBeforeSendMessage) then
      FOnBeforeSendMessage(Self, Msg);

  Finally
  End;
end;

function TAiOpenChat.AddMessageAndRun(aPrompt, aRole: String): String;
begin
  InternalAddMessage(aPrompt, aRole);
  Result := Run;
end;

constructor TAiOpenChat.Create(Sender: TComponent);
begin
  inherited;
  FOwner := Sender;
  FMessages := TAiOpenChatMessages.Create;
  FTools := TStringList.Create;
  FInitialInstructions := TStringList.Create;
  FResponse := TStringStream.Create('', TEncoding.UTF8);
  FClient := TNetHTTPClient.Create(Self);
  FClient.OnReceiveData := Self.OnInternalReceiveData;

  FModel := 'gpt-3.5-turbo-1106';
  FN := 1;
  FResponse_format := TAiOpenChatResponseFormat.tiaChatRfText;
  FTemperature := 1;
  FUser := 'user';
  FInitialInstructions.Text := 'Eres un asistente muy útil y servicial';
  FMax_tokens := 300;
end;

destructor TAiOpenChat.Destroy;
begin

  FResponse.Free;
  FTools.Free;
  FClient.Free;
  inherited;
end;

function TAiOpenChat.ExtractToolCallFromJson(jChoices: TJSonArray): TAiToolsFunctions;
Var
  jObj, Msg: TJSonObject;
  JVal, JVal1: TJSonValue;
  Fun: TAiToolsFunction;
  JToolCalls: TJSonArray;
begin
  Result := TAiToolsFunctions.Create;

  For JVal1 in jChoices do
  Begin
    Msg := TJSonObject(JVal1).GetValue<TJSonObject>('message');

    If Msg.TryGetValue<TJSonArray>('tool_calls', JToolCalls) then
    Begin
      For JVal in JToolCalls do
      Begin
        jObj := TJSonObject(JVal);
        If jObj.GetValue<String>('type') = 'function' then
        Begin
          jObj := TJSonObject(JVal);
          Fun := TAiToolsFunction.Create;
          Fun.Id := jObj.GetValue<String>('id');
          Fun.Tipo := jObj.GetValue<String>('type');
          Fun.Name := jObj.GetValue<TJSonObject>('function').GetValue<String>('name');
          Fun.Arguments := jObj.GetValue<TJSonObject>('function').GetValue<String>('arguments');
          Result.Add(Fun.Id, Fun);
        End;
      End;
    End;
  End;
end;

function TAiOpenChat.GetLastMessage: TAiOpenChatMessage;
begin
  Result := Nil;
  If FMessages.Count > 0 then
    Result := FMessages[FMessages.Count - 1];
end;

function TAiOpenChat.InitChatCompletions: String;
Var
  AJSONObject, jObj, jToolChoice: TJSonObject;
  JArr: TJSonArray;
  JStop: TJSonArray;
  Lista: TStringList;
  I: integer;
  LAsincronico: Boolean;
begin

  If FUser = '' then
    FUser := 'user';

  If FModel = '' then
    FModel := 'gpt-3.5-turbo-1106';

  LAsincronico := Self.Asynchronous and (not Self.Tool_Active);

  FClient.Asynchronous := LAsincronico;

  AJSONObject := TJSonObject.Create;
  Lista := TStringList.Create;

  Try

    AJSONObject.AddPair('stream', TJSONBool.Create(LAsincronico));

    If Tool_Active and (Trim(FTools.Text) <> '') then
    Begin
      JArr := TJSonArray(TJSonArray.ParseJSONValue(FTools.Text));
      If Not Assigned(JArr) then
        Raise Exception.Create('La propiedad Tools están mal definido, debe ser un JsonArray');
      AJSONObject.AddPair('tools', JArr);

      If (Trim(FTool_choice) <> '') then
      Begin
        jToolChoice := TJSonObject(TJSonArray.ParseJSONValue(FTool_choice));
        If Assigned(jToolChoice) then
          AJSONObject.AddPair('tools_choice', jToolChoice);
      End;

    End;

    AJSONObject.AddPair('messages', FMessages.ToJSon);
    AJSONObject.AddPair('model', FModel);

    AJSONObject.AddPair('temperature', TJSONNumber.Create(Trunc(FTemperature * 100) / 100));
    AJSONObject.AddPair('max_tokens', TJSONNumber.Create(FMax_tokens));

    If FTop_p <> 0 then
      AJSONObject.AddPair('top_p', TJSONNumber.Create(FTop_p));

    AJSONObject.AddPair('frequency_penalty', TJSONNumber.Create(Trunc(FFrequency_penalty * 100) / 100));
    AJSONObject.AddPair('presence_penalty', TJSONNumber.Create(Trunc(FPresence_penalty * 100) / 100));
    AJSONObject.AddPair('user', User);
    AJSONObject.AddPair('n', TJSONNumber.Create(FN));


    // --------- se desactiva maraca error, no se envía ---------------------
    // If LAsincronico or (FResponse_format = tiaChatRfJson) then
    // AJSONObject.AddPair('response_format', 'json_object');
    // Else
    // AJSONObject.AddPair('response_format', 'text');

    Lista.CommaText := FStop;
    If Lista.Count > 0 then
    Begin
      JStop := TJSonArray.Create;
      For I := 0 to Lista.Count - 1 do
        JStop.Add(Lista[I]);
      AJSONObject.AddPair('stop', JStop);
    End;

    If FLogprobs = True then
    Begin
      If FLogit_bias <> '' then
        AJSONObject.AddPair('logit_bias', TJSONNumber.Create(FLogit_bias));

      AJSONObject.AddPair('logprobs', TJSONBool.Create(FLogprobs));

      If FTop_logprobs <> '' then
        AJSONObject.AddPair('top_logprobs', TJSONNumber.Create(FTop_logprobs));
    End;

    If Seed > 0 then
      AJSONObject.AddPair('seed', TJSONNumber.Create(FSeed));

    Result := UTF8ToString(AJSONObject.ToJSon);
  Finally
    AJSONObject.Free;
    Lista.Free;
  End;
end;

procedure TAiOpenChat.NewChat;
Var
  I: integer;
begin
  For I := FMessages.Count - 1 to 0 do
  Begin
    FMessages[I].Free;
    FMessages.Delete(I);
  End;
  FMessages.Clear;
end;

function TAiOpenChat.NewMessage(aPrompt, aRole: String): TAiOpenChatMessage;
begin
  Result := TAiOpenChatMessage.Create(aPrompt, aRole);
end;

procedure TAiOpenChat.OnInternalReceiveData(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean);
Var
  jObj, Delta: TJSonObject;
  sJson, Value, Role1: String;
  P: integer;
  Msg: TAiOpenChatMessage;
begin

  If FClient.Asynchronous = False then
    Exit;

  AAbort := FAbort;

  If FAbort = True then
  Begin
    FBusy := False;
    If Assigned(FOnReceiveDataEnd) then
      FOnReceiveDataEnd(Self, Nil, Nil, 'system', 'abort');
  End;

  Try
    FTmpResponseText := FTmpResponseText + FResponse.DataString;
    FTmpResponseText1 := FTmpResponseText1 + FResponse.DataString;

    FResponse.Clear;
    FResponse.Position := 0;

    If Copy(FTmpResponseText, 1, 5) = 'data:' then
      FTmpResponseText := Copy(FTmpResponseText, 6, Length(FTmpResponseText));

    Repeat
      P := Pos('data:', FTmpResponseText);
      If P > 0 then
      Begin
        sJson := Trim(Copy(FTmpResponseText, 1, P - 1));
        FTmpResponseText := Copy(FTmpResponseText, P + 6, Length(FTmpResponseText));
      End
      Else
      Begin
        If Trim(FTmpResponseText) = '[DONE]' then // Terminó el proceso
        Begin
          sJson := Trim(FTmpResponseText);
          FTmpResponseText := '';
        End
        Else
          sJson := '';
      End;

      If sJson = '[DONE]' then // Terminó el proceso
      Begin
        sJson := '';
        Msg := TAiOpenChatMessage.Create(FLastContent, FTmpRole);
        Msg.FId := FMessages.Count + 1;
        FMessages.Add(Msg);
        FBusy := False;

        If Assigned(FOnReceiveDataEnd) then
          FOnReceiveDataEnd(Self, Msg, Nil, FTmpRole, FLastContent);
      End
      Else If sJson <> '' then
      Begin
        jObj := TJSonObject(TJSonObject.ParseJSONValue(sJson));

        Try
          If Assigned(jObj) then
          Begin
            Delta := jObj.GetValue<TJSonArray>('choices')[0].GetValue<TJSonObject>('delta');
            Value := '';
            Delta.TryGetValue<String>('content', Value);
            Delta.TryGetValue<String>('role', Role1);

            If Role1 <> '' then
              FTmpRole := Role1;

            FLastContent := FLastContent + Value;

            If (Value <> '') and Assigned(FOnReceiveDataEvent) then
            Begin
              Value := StringReplace(Value, #$A, sLineBreak, [rfReplaceAll]);
              FOnReceiveDataEvent(Self, Nil, jObj, FTmpRole, Value);
            End;
          End;
        Finally
          jObj.Free;
        End;
      End;

    Until sJson = '';

  Except

  End;
end;

procedure TAiOpenChat.ParseChat(jObj: TJSonObject);
Var
  choices, JToolCalls: TJSonArray;
  JItem: TJSonObject;
  JVal: TJSonValue;
  jMessage: TJSonObject;
  uso: TJSonObject;
  aPrompt_tokens, aCompletion_tokens, aTotal_tokens: integer;
  Role, Respuesta: String;
  Msg: TAiOpenChatMessage;
  LFunciones: TAiToolsFunctions;
  ToolCall: TAiToolsFunction;

  TaskList: array of ITask;
  I, NumTasks: integer;
  Clave, sToolCalls: String;

begin

  // Id := JObj.GetValue('id').Value;
  // IdObject := JObj.GetValue('object').Value;
  // IdCreate := JObj.GetValue('created').GetValue<String>;
  Model := jObj.GetValue('model').Value;
  uso := jObj.GetValue('usage') as TJSonObject;
  aPrompt_tokens := uso.GetValue<integer>('prompt_tokens');
  aCompletion_tokens := uso.GetValue<integer>('completion_tokens');
  aTotal_tokens := uso.GetValue<integer>('total_tokens');

  jObj.TryGetValue<TJSonArray>('choices', choices);

  For JVal in choices do
  Begin
    JItem := TJSonObject(JVal);
    jMessage := JItem.GetValue<TJSonObject>('message');
    Respuesta := Respuesta + jMessage.GetValue<String>('content') + sLineBreak;
    Role := jMessage.GetValue<String>('role');

    If jMessage.TryGetValue<TJSonArray>('tool_calls', JToolCalls) then
      sToolCalls := JToolCalls.Format;
  End;

  Respuesta := Trim(Respuesta);
  Self.FLastContent := Respuesta;
  FPrompt_tokens := FPrompt_tokens + aPrompt_tokens;
  FCompletion_tokens := FCompletion_tokens + aCompletion_tokens;
  FTotal_tokens := FTotal_tokens + aTotal_tokens;

  Msg := TAiOpenChatMessage.Create(Respuesta, Role);
  Msg.FPrompt := Respuesta;
  Msg.FTool_calls := sToolCalls;
  Msg.FPrompt_tokens := aPrompt_tokens;
  Msg.FCompletion_tokens := aCompletion_tokens;
  Msg.FTotal_tokens := aTotal_tokens;
  Msg.FId := FMessages.Count + 1;
  FMessages.Add(Msg);

  // If Assigned(FOnAddMessage) then
  // FOnAddMessage(Self, jObj, Role, Respuesta);

  LFunciones := ExtractToolCallFromJson(choices);

  Try
    If LFunciones.Count > 0 then
    Begin

      NumTasks := LFunciones.Count;
      SetLength(TaskList, NumTasks);
      // Ajusta el tamaño del array para el número de tareas

      I := 0;
      For Clave in LFunciones.Keys do
      Begin
        ToolCall := LFunciones[Clave];

        TaskList[I] := TTask.Create(
          procedure
          begin
            If Assigned(FOnCallToolFunction) then
              FOnCallToolFunction(Self, ToolCall)
          end);
        TaskList[I].Start;
        Inc(I);

      End;
      TTask.WaitForAll(TaskList);

      For Clave in LFunciones.Keys do
      Begin
        ToolCall := LFunciones[Clave];
        Msg := TAiOpenChatMessage.Create(ToolCall.Response, 'tool', ToolCall.Id, ToolCall.Name);
        Msg.FId := FMessages.Count + 1;
        FMessages.Add(Msg);
      End;

      Self.Run;

    End
    Else
    Begin
      FBusy := False;
      If Assigned(FOnReceiveDataEnd) then
        FOnReceiveDataEnd(Self, Msg, jObj, Role, Respuesta);
    End;
  Finally
    LFunciones.Free;
  End;
end;

function TAiOpenChat.RemoveMesage(IdMsg: integer): Boolean;
Var
  I: integer;
  Msg: TAiOpenChatMessage;
begin
  For I := 0 to FMessages.Count - 1 do
  Begin
    Msg := FMessages[I];
    If Msg.Id = IdMsg then
    Begin
      FMessages.Remove(Msg);
      Break;
    End;
  End;
  Result := True;
end;

function TAiOpenChat.Run(aMsg: TAiOpenChatMessage = Nil): String;
Var
  ABody: String;
  sUrl: String;
  Res: IHTTPResponse;
  St: TStringStream;
  FHeaders: TNetHeaders;
  jObj: TJSonObject;
  Msg: TAiOpenChatMessage;
begin

  FBusy := True; // Marca como ocupado al sistema
  FAbort := False; // Inicia como no en abort

  St := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/chat/completions';

  Try
    FHeaders := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    FClient.ContentType := 'application/json';

    // Comienza con las instrucciones iniciales y le adiciona cada 20 mensajes para evitar que se olvide
    If (FMessages.Count = 0) or ((FMessages.Count mod 20) = 0) then
    Begin
      Msg := TAiOpenChatMessage.Create(FInitialInstructions.Text, 'system');
      Msg.FId := FMessages.Count + 1;
      FMessages.Add(Msg);
      If Assigned(FOnAddMessage) then
        FOnAddMessage(Self, Msg, Nil, 'system', FInitialInstructions.Text);
    End;

    If Assigned(aMsg) then
    Begin

      If Assigned(FOnBeforeSendMessage) then
        FOnBeforeSendMessage(Self, Msg);

      aMsg.FId := FMessages.Count + 1;
      FMessages.Add(aMsg);
      FLastPrompt := aMsg.Prompt;

      If Assigned(FOnAddMessage) then
        FOnAddMessage(Self, Msg, Nil, aMsg.Role, aMsg.Prompt);

    End;

    ABody := InitChatCompletions;

    St.WriteString(ABody);
    St.Position := 0;
    // St.SaveToFile('c:\temp\peticion.txt');
    // St.Position := 0;

    FResponse.Clear;
    FResponse.Position := 0;

    Res := FClient.Post(sUrl, St, FResponse, FHeaders);

    // FResponse.SaveToFile('c:\temp\respuesta.txt');
    // FResponse.Position := 0;

    FLastContent := '';

    If FClient.Asynchronous = False then
    Begin
      if Res.StatusCode = 200 then
      Begin
        jObj := TJSonObject(TJSonObject.ParseJSONValue(Res.ContentAsString));
        Try
          ParseChat(jObj);
          Result := FLastContent;
          FBusy := False;

        Finally
          FreeAndNil(jObj);
        End;
      End
      else
      begin
        Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      end;
    End;
  Finally
    If FClient.Asynchronous = False then
      St.Free; // Esto no funciona en multiarea, así que se libera cuando no lo es.
  End;
end;

function TAiOpenChat.RemoveMesage(Msg: TAiOpenChatMessage): Boolean;
begin
  If FMessages.ItemValue(Msg) >= 0 then
    FMessages.Remove(Msg);
end;

procedure TAiOpenChat.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiOpenChat.SetAsynchronous(const Value: Boolean);
begin
  FAsynchronous := Value;
  FClient.Asynchronous := Value;
end;

procedure TAiOpenChat.SetCompletion_tokens(const Value: integer);
begin
  FCompletion_tokens := Value;
end;

procedure TAiOpenChat.SetFrequency_penalty(const Value: Double);
begin
  FFrequency_penalty := Value;
end;

procedure TAiOpenChat.SetInitialInstructions(const Value: TStrings);
begin
  FInitialInstructions.Text := Value.Text;
end;

procedure TAiOpenChat.SetLogit_bias(const Value: String);
begin
  FLogit_bias := Value;
end;

procedure TAiOpenChat.SetLogprobs(const Value: Boolean);
begin
  FLogprobs := Value;
end;

procedure TAiOpenChat.SetMax_tokens(const Value: integer);
begin
  FMax_tokens := Value;
end;

procedure TAiOpenChat.SetModel(const Value: String);
begin
  FModel := Value;
end;

procedure TAiOpenChat.SetN(const Value: integer);
begin
  FN := Value;
end;

procedure TAiOpenChat.SetOnAddMessage(const Value: TAiOpenChatDataEvent);
begin
  FOnAddMessage := Value;
end;

procedure TAiOpenChat.SetOnBeforeSendMessage(const Value: TAiOpenChatBeforeSendEvent);
begin
  FOnBeforeSendMessage := Value;
end;

procedure TAiOpenChat.SetOnCallToolFunction(const Value: TOnCallToolFunction);
begin
  FOnCallToolFunction := Value;
end;

procedure TAiOpenChat.SetOnReceiveDataEnd(const Value: TAiOpenChatDataEvent);
begin
  FOnReceiveDataEnd := Value;
end;

procedure TAiOpenChat.SetOnReceiveDataEvent(const Value: TAiOpenChatDataEvent);
begin
  FOnReceiveDataEvent := Value;
end;

procedure TAiOpenChat.SetPresence_penalty(const Value: Double);
begin
  FPresence_penalty := Value;
end;

procedure TAiOpenChat.SetPrompt_tokens(const Value: integer);
begin
  FPrompt_tokens := Value;
end;

procedure TAiOpenChat.SetResponse_format(const Value: TAiOpenChatResponseFormat);
begin
  FResponse_format := Value;
end;

procedure TAiOpenChat.SetSeed(const Value: integer);
begin
  FSeed := Value;
end;

procedure TAiOpenChat.SetStop(const Value: string);
begin
  FStop := Value;
end;

procedure TAiOpenChat.SetTemperature(const Value: Double);
begin
  FTemperature := Value;
end;

procedure TAiOpenChat.SetTools(const Value: TStrings);
begin
  FTools.Text := Value.Text;
end;

procedure TAiOpenChat.SetTool_Active(const Value: Boolean);
begin
  FTool_Active := Value;
end;

procedure TAiOpenChat.SetTool_choice(const Value: string);
begin
  FTool_choice := Value;
end;

procedure TAiOpenChat.SetTop_logprobs(const Value: String);
begin
  FTop_logprobs := Value;
end;

procedure TAiOpenChat.SetTop_p(const Value: Double);
begin
  FTop_p := Value;
end;

procedure TAiOpenChat.SetTotal_tokens(const Value: integer);
begin
  FTotal_tokens := Value;
end;

procedure TAiOpenChat.SetUser(const Value: String);
begin
  FUser := Value;
end;

{ TAOpeniChatMessage }

procedure TAiOpenChatMessage.AddStreamImage(Stream: TMemoryStream);
Var
  Base64: String;
begin
  Base64 := StreamToBase64(Stream);
  FVisionBase64.Add(Base64);
end;

constructor TAiOpenChatMessage.Create(aPrompt, aRole: String; aToolCallId: String = ''; aFunctionName: String = '');
begin
  Inherited Create;
  FVisionUrls := TStringList.Create;
  FVisionBase64 := TStringList.Create;
  Self.FRole := aRole;
  Self.FPrompt := aPrompt;
  Self.FFunctionName := aFunctionName;
  Self.FTollCallId := aToolCallId;
end;

destructor TAiOpenChatMessage.Destroy;
begin
  FVisionUrls.Free;
  FVisionBase64.Free;
  inherited;
end;

procedure TAiOpenChatMessage.SetContent(const Value: String);
begin
  FContent := Value;
end;

procedure TAiOpenChatMessage.SetFunctionName(const Value: String);
begin
  FFunctionName := Value;
end;

procedure TAiOpenChatMessage.SetPrompt(const Value: String);
begin
  FPrompt := Value;
end;

procedure TAiOpenChatMessage.SetRole(const Value: String);
begin
  FRole := Value;
end;

procedure TAiOpenChatMessage.SetTollCallId(const Value: String);
begin
  FTollCallId := Value;
end;

procedure TAiOpenChatMessage.SetTool_calls(const Value: String);
begin
  FTool_calls := Value;
end;

procedure TAiOpenChatMessage.SetVisionBase64(const Value: TStringList);
begin
  FVisionBase64.Text := Value.Text;
end;

procedure TAiOpenChatMessage.SetVisionUrls(const Value: TStringList);
begin
  FVisionUrls.Text := Value.Text;
end;

function TAiOpenChatMessage.StreamToBase64(Stream: TMemoryStream): String;
begin
  Stream.Position := 0;
  Result := TNetEncoding.Base64.EncodeBytesToString(Stream.Memory, Stream.Size);
end;

{ TAiOpenChatMessages }

function TAiOpenChatMessages.GetAsText: String;
Var
  St: TStringStream;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  Try
    SaveToStream(St);
    Result := St.DataString;
  Finally
    St.Free;
  End;
end;

procedure TAiOpenChatMessages.LoadFromFile(FileName: String);
Var
  St: TStringStream;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  Try
    If FileExists(FileName) then
    Begin
      St.LoadFromFile(FileName);
      St.Position := 0;
      LoadFromStream(St);
    End;
  Finally
    St.Free;
  End;
end;

procedure TAiOpenChatMessages.LoadFromStream(Stream: TStream);
Var
  jObj, JItem: TJSonObject;
  JArr: TJSonArray;
  sJson, Model, S: String;
  St: TStringStream;
  Data: TAiOpenChatMessages;
  Item: TAiOpenChatMessage;
  I: integer;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  Try
    St.LoadFromStream(Stream);
    sJson := St.DataString;

    jObj := TJSonObject(TJSonObject.ParseJSONValue(sJson));

    If Assigned(jObj) and (jObj.TryGetValue<String>('model', Model)) then
    Begin
      If Model = 'AiOpenChat' then
      Begin
        JArr := TJSonArray(jObj.GetValue<TJSonArray>('data'));

        S := JArr.Format;
        If JArr.Count > 0 then
          Self.Clear;

        For I := 0 to JArr.Count - 1 do
        Begin
          JItem := TJSonObject(JArr[I]);

          Item := TJSon.JsonToObject<TAiOpenChatMessage>(JItem);
          Self.Add(Item);
        End;
      End;
    End;
  Finally
    St.Free;
    FreeAndNil(jObj);
  End;
end;

procedure TAiOpenChatMessages.SaveToFile(FileName: String);
Var
  St: TStringStream;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  Try
    Self.SaveToStream(St);
    St.Position := 0;
    St.SaveToFile(FileName);
  Finally
    St.Free;
  End;
end;

procedure TAiOpenChatMessages.SaveToStream(Stream: TStream);
Var
  jObj, jItem: TJSonObject;
  JArr: TJSonArray;
  St: TStringStream;
  I : Integer;
  Item : TAiOpenChatMessage;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  jObj := TJSonObject.Create;
  jArr := TJSonArray.Create;

  Try
    jObj.AddPair('model', 'AiOpenChat');
    jObj.AddPair('type', 'Messages');
    jObj.AddPair('ver', '1.0');

    For I := 0 to Self.Count - 1 do
    Begin
      Item := Self.Items[I];
      jItem := TJson.ObjectToJsonObject(Item);
      JArr.Add(JItem);
    End;

    jObj.AddPair('data', JArr);
    St.WriteString(jObj.Format);
    St.SaveToStream(Stream);
  Finally
    St.Free;
    jObj.Free;
  End;
end;

procedure TAiOpenChatMessages.SetAsText(const Value: String);
Var
  St: TStringStream;
begin
  St := TStringStream.Create('', TEncoding.UTF8);
  Try
    If Trim(Value) <> '' then
    Begin
      St.WriteString(Value);
      St.Position := 0;
      LoadFromStream(St);
    End;
  Finally
    St.Free;
  End;
end;

function TAiOpenChatMessages.ToJSon: TJSonArray;
Var
  I, J: integer;
  Msg: TAiOpenChatMessage;
  jObj, JMsg: TJSonObject;
  JContent: TJSonArray;
  ImagePayload: TStringStream;
  Base64: String;
begin
  Result := TJSonArray.Create;

  For I := 0 to Count - 1 do
  Begin
    Msg := Self.Items[I];
    jObj := TJSonObject.Create;
    jObj.AddPair('role', Msg.FRole);

    If Msg.FTollCallId <> '' then
      jObj.AddPair('tool_call_id', Msg.FTollCallId);

    If Msg.FFunctionName <> '' then
      jObj.AddPair('name', Msg.FFunctionName);

    If Msg.FVisionUrls.Count > 0 then
    Begin
      JContent := TJSonArray.Create;

      JMsg := TJSonObject.Create;
      JMsg.AddPair('type', 'text');
      JMsg.AddPair('text', Msg.FPrompt);
      JContent.Add(JMsg);

      For J := 0 to Msg.FVisionUrls.Count - 1 do
      Begin
        JMsg := TJSonObject.Create;
        JMsg.AddPair('type', 'image_url');
        JMsg.AddPair('image_url', Msg.FVisionUrls[J]);
        JContent.Add(JMsg);
      End;
      jObj.AddPair('content', JContent);
    End
    Else If Msg.FVisionBase64.Count > 0 then
    Begin
      JContent := TJSonArray.Create;

      JMsg := TJSonObject.Create;
      JMsg.AddPair('type', 'text');
      JMsg.AddPair('text', Msg.FContent);
      JContent.Add(JMsg);

      For J := 0 to Msg.FVisionBase64.Count - 1 do
      Begin
        Base64 := Msg.FVisionBase64[J];

        ImagePayload := TStringStream.Create('{"type": "image_url", "image_url": {"url": "data:image/jpeg;base64,' + Base64 + '"}}', TEncoding.UTF8);
        try
          JContent.Add(TJSonObject.ParseJSONValue(ImagePayload.DataString) as TJSonObject);
        finally
          ImagePayload.Free;
        end;
      End;
      jObj.AddPair('content', JContent);
    End

    Else
      jObj.AddPair('content', Msg.FPrompt);

    If Msg.FTool_calls <> '' then
      jObj.AddPair('tool_calls', TJSonArray(TJSonArray.ParseJSONValue(Msg.FTool_calls)));

    Result.Add(jObj);
  End;
end;

end.

{
  "id": "chatcmpl-8hooLP5ORZkZgiXXgPfxAtKjRDhmS",
  "object": "chat.completion",
  "created": 1705454113,
  "model": "gpt-3.5-turbo-1106",
  "choices": [
  {
  "index": 0,
  "message": {
  "role": "assistant",
  "content": null,
  "tool_calls": [
  {
  "id": "call_P5vp7qTq4U8E0FljLMDUDEp4",
  "type": "function",
  "function": {
  "name": "get_current_weather",
  "arguments": "{\"location\": \"Cali\", \"unit\": \"celsius\" } " } },
{
  "id": "call_1ZP6MiyBjE1KUoGQZQQjz9gf",
  "type": "function",
  "function": {
  "name": "get_current_weather",
  "arguments": "{\"location\": \"Bogota\", \"unit\": \"celsius\" } " } }]}, " Logprobs ": null, " finish_reason ": " Tool_calls " }], " usage ": {
  "prompt_tokens": 129,
  "completion_tokens": 58,
  "total_tokens": 187
} , " system_fingerprint ": " fp_fe56e538d5 " }

  Forma de responder
{ "role": "tool", "tool_call_id": "ID_TOOL_CALL", "name": "Function_Name", "content": results }
