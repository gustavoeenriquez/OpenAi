unit uAiOpenAssistant;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Threading,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client, uOpenAi;

type
  TAiOpenAssistant = class(TComponent)
  Private
    FStatus: String;
    FAiRun: TAiRun;
    FThRead: TAiThRead;
    FAssistant: TAiAssistant;
    // FModel: String;
    // FAssistantName: String;
    FOnCallToolFunction: TOnCallToolFunction;
    FOnStatusChange: TOnStatusNotifyEvent;
    FAsynchronous: Boolean;
    FApiKey: String;
    FOwner: TComponent;
    FOnResponse: TOnAssistantResponse;
    FActive: Boolean;
    FOnBeforeResponse: TOnBeforeResponse;
    FAutoRemoveThread: Boolean;
    FThreadId: String;
    FAssistantId: String;
    // FCode_Interpreter: Boolean;
    // FFunciones: TAiToolsFunctions;
    // FRetrieval: Boolean;
    FInstructions: TStringList;
    FMetadata: TAiMetadata;
    FFilesIds: TStringList;
    FTools: TStrings;
    function GetBusy: Boolean;
    procedure SetOnCallToolFunction(const Value: TOnCallToolFunction);
    procedure SetOnStatusChange(const Value: TOnStatusNotifyEvent);
    procedure SetAsynchronous(const Value: Boolean);
    procedure SetApiKey(const Value: String);
    procedure SetOnResponse(const Value: TOnAssistantResponse);
    procedure SetAssistantName(const Value: String);
    procedure SetActive(const Value: Boolean);
    procedure SetOnBeforeResponse(const Value: TOnBeforeResponse);
    procedure SetAutoRemoveThread(const Value: Boolean);
    procedure SetThreadId(const Value: String);
    procedure SetCode_Interpreter(const Value: Boolean);
    procedure SetFilesIds(const Value: TStringList);
    procedure SetFunciones(const Value: TAiToolsFunctions);
    procedure SetInstructions(const Value: TStringList);
    procedure SetMetadata(const Value: TAiMetadata);
    procedure SetModel(const Value: String);
    procedure SetRetrieval(const Value: Boolean);
    procedure SetTools(const Value: TStrings);
    function GetCode_Interpreter: Boolean;
    function GetRetrieval: Boolean;
    function GetModel: String;
    function GetFunciones: TAiToolsFunctions;
    function GetAssistantName: String;
  Protected
    Procedure DoStatusChange(aStatus: String);
    Procedure DoResponse(Response: TAiMessage);
    Function InternalAddMessage(aMessage: String; aRole: String = ''; aFiles_ids: String = ''): TAiMessage;
    Procedure InitToolsFunctions;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;

    Function LoadAssistant(AssistantName: String; ThReadId: String = ''): Boolean;
    Function AddMessage(aMessage: String; aRole: String = ''; aFiles_ids: String = ''): TAiMessage;
    Function GetLasMessage: TAiMessage;
    Function GetResponse: String;

    Function CreateNewAssistant: Boolean;
    Function RemoveAssistant: Boolean;
    Function ApplyUpdates: Boolean;
    Function RemoveThread: Boolean;
    Function CreateNewThread(aMensajes: TAiMessages; aMetadata: TAiMetadata): Boolean;
    Procedure CancelRun;
    Function UploadAssistantFile(FileName: String): TAiFile;
    Function DeleteAssistantFile(FileId: String): Boolean;
    Function UploadFile(FileName: String): TAiFile;
    Function DeleteFile(FileId: String): Boolean;
    Function DownloadFile(FileId: String): TAiFile;

    Function ListFiles: String;
    Function ListFilesArray: TAiFilesArray;
    Function ListAssistantFiles: String;
    Function ListAssistantFilesArray: TAiFilesArray;

    Function GetAssistantsList: String;
    Function GetModelsList: String;

    Procedure AddFunction(aBody: String); Overload;
    Procedure AddFunction(aBody: TJsonObject); Overload;

    Property Assistant: TAiAssistant read FAssistant;
    Property ThRead: TAiThRead read FThRead;
    Property AiRun: TAiRun read FAiRun;
    Property Status: String read FStatus;
    Property Busy: Boolean Read GetBusy;
    Property Metadata: TAiMetadata read FMetadata write SetMetadata;
    Property Funciones: TAiToolsFunctions read GetFunciones write SetFunciones;

  Published
    Property OnCallToolFunction: TOnCallToolFunction read FOnCallToolFunction write SetOnCallToolFunction;
    Property OnStatusChange: TOnStatusNotifyEvent read FOnStatusChange write SetOnStatusChange;
    Property OnResponse: TOnAssistantResponse read FOnResponse write SetOnResponse;
    Property OnBeforeResponse: TOnBeforeResponse read FOnBeforeResponse write SetOnBeforeResponse;
    Property Asynchronous: Boolean read FAsynchronous write SetAsynchronous;
    Property ApiKey: String read FApiKey write SetApiKey;
    Property AssistantName: String read GetAssistantName write SetAssistantName;
    Property AssistantId: String read FAssistantId;
    Property ThReadId: String read FThreadId write SetThreadId;
    Property Active: Boolean read FActive write SetActive;
    Property AutoRemoveThread: Boolean read FAutoRemoveThread write SetAutoRemoveThread;
    Property Model: String read GetModel write SetModel;
    Property Instructions: TStringList read FInstructions write SetInstructions;
    Property FilesIds: TStringList read FFilesIds write SetFilesIds;
    Property Code_Interpreter: Boolean read GetCode_Interpreter write SetCode_Interpreter;
    Property Retrieval: Boolean read GetRetrieval write SetRetrieval;
    Property Tools: TStrings read FTools write SetTools;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('OpenAssistant', [TAiOpenAssistant]);
end;

{ TAiOpenAssistant }

procedure TAiOpenAssistant.AddFunction(aBody: TJsonObject);
begin
   FAssistant.Funciones.AddFunction(aBody);
end;

procedure TAiOpenAssistant.AddFunction(aBody: String);
begin
   FAssistant.Funciones.AddFunction(aBody);
end;

function TAiOpenAssistant.AddMessage(aMessage, aRole, aFiles_ids: String): TAiMessage;
begin

  If Not FActive then
    Raise Exception.Create('Cargue el asistente antes de continuar');

  Try
    If Self.FAsynchronous then
    Begin
      TTask.Run(
        Procedure
        Begin
          InternalAddMessage(aMessage, aRole, aFiles_ids);
        End);
    End
    Else
    Begin
      Result := InternalAddMessage(aMessage, aRole, aFiles_ids);
    End;

  Finally

  End;
end;

procedure TAiOpenAssistant.InitToolsFunctions;
Var
  JObj, Fun: TJSonObject;
  JVal: TJSonValue;
  JArr: TJsonArray;
  I: Integer;
  FName: String;
  AiTool: TAiToolsFunction;
  BSave: Boolean;
begin
  BSave := False;
  If FTools.Text.Trim <> '' then
  Begin
    JArr := TJsonArray(TJsonArray.ParseJSONValue(FTools.Text));

    If Not Assigned(JArr) then
      Raise Exception.Create('La propiedad Tools est�n mal definido, debe ser un JsonArray');

    For JVal in JArr do
    Begin
      JObj := TJSonObject(JVal);

      If JObj.TryGetValue<TJSonObject>('function', Fun) then
      Begin

        FName := Fun.GetValue('name').Value;

        If FName <> '' then
        Begin
          If Funciones.TryGetValue(FName, AiTool) then
          Begin // Si existe no la actualiza

          End
          Else // Si no existe la crea
          Begin
            Funciones.AddFunction(JObj);
            BSave := True;
          End;
        End;
      End;
    End;

    If BSave then
      Self.ApplyUpdates;
  End;

  If Assigned(Funciones) then
    Tools.Text := Funciones.ToFunctionsJSon.Format;

end;

function TAiOpenAssistant.InternalAddMessage(aMessage, aRole, aFiles_ids: String): TAiMessage;
Var
  Msg: TAiMessage;
begin
  ThRead.AddMessage(aMessage, aRole, aFiles_ids);

  FAiRun.OnCallToolFunction := FOnCallToolFunction; // Se asegura que llame a la funci�n

  FAiRun.Run;

  While (FAiRun.Status <> 'completed') and (FAiRun.Status <> 'cancelled') do // and (FAiRun.Status <> 'requires_action') do
  Begin

    TThread.Synchronize(nil,
      procedure
      begin
        DoStatusChange(FAiRun.Status);
      end);

    Sleep(2000);
    FAiRun.Retrieve;
  End;

  TThread.Synchronize(nil,
    procedure
    begin
      DoStatusChange(FAiRun.Status);
    end);

  { If (FAiRun.Status = 'requires_action') then
    Begin
    FAiRun.Retrieve;
    End;
  }

  If Self.FAsynchronous then
  Begin
    TThread.Synchronize(nil,
      procedure
      begin
        DoStatusChange(FAiRun.Status);
        DoResponse(Self.GetLasMessage);
      end);
  End
  Else
  Begin
    DoStatusChange(FAiRun.Status);
    Msg := Self.GetLasMessage;
    DoResponse(Msg);
    Result := Msg;
  End;
end;

function TAiOpenAssistant.ApplyUpdates: Boolean;
begin
  Assistant.ApplyUpdates;
  Result := True;
end;

procedure TAiOpenAssistant.CancelRun;
begin
  If Busy then
    FAiRun.Cancel;
end;

constructor TAiOpenAssistant.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FOwner := AOwner;
  FAssistant := TAiAssistant.Create(FApiKey);
  FThRead := TAiThRead.Create(FAssistant);
  FAiRun := TAiRun.Create(FAssistant, FThRead);
  FMetadata := TAiMetadata.Create;
  FFilesIds := TStringList.Create;
  FTools := TStringList.Create;
  FInstructions := TStringList.Create;
  FAssistant.OnBeforeResponse := Self.OnBeforeResponse;
  FInstructions.Text := 'Eres un asistente muy servicial';
end;

function TAiOpenAssistant.CreateNewAssistant: Boolean;
begin
  FThreadId := '';
  Result := FAssistant.CreateNewAssistant(Model, AssistantName, FInstructions.Text, FFilesIds.CommaText, Funciones, FMetadata, Retrieval, Code_Interpreter);
  If Result then
    LoadAssistant(AssistantName);
end;

function TAiOpenAssistant.CreateNewThread(aMensajes: TAiMessages; aMetadata: TAiMetadata): Boolean;
begin
  Result := ThRead.CreateNewThread(aMensajes, aMetadata);
  If Result then
    Self.ThReadId := ThRead.ThReadId;
end;

function TAiOpenAssistant.DeleteAssistantFile(FileId: String): Boolean;
begin
    FAssistant.RemoveFileId(FileId);
    Result := True;
end;

function TAiOpenAssistant.DeleteFile(FileId: String): Boolean;
Var
  AiFiles: TAiFiles;
begin
  AiFiles := TAiFiles.Create(FApiKey);
  Try
    Result := AiFiles.DeleteFile(FileId);
  Finally
    AiFiles.Free;
  End;
end;

destructor TAiOpenAssistant.Destroy;
begin

  FAssistant.OnBeforeResponse := Nil;
  Active := False;

  FInstructions.Free;
  FTools.Free;

  FAiRun := TAiRun.Create(FAssistant, FThRead);
  FMetadata.Free;
  FFilesIds.Free;

  If Assigned(FThRead) then
  Begin
    If FAutoRemoveThread and (FThreadId <> '') then
      FThRead.Remove;
    FThRead.Free;
  End;
  If Assigned(FAssistant) then
    FAssistant.Free;
  inherited;
end;

function TAiOpenAssistant.ListAssistantFiles: String;
Var
  JArr: TJsonArray;
  jItem: TJSonValue;
  Lista: TStringList;
begin
  JArr := FAssistant.ListFiles.GetValue<TJsonArray>('data');

  Lista := TStringList.Create;
  Try
    For jItem in JArr do
      Lista.Add(jItem.GetValue<String>('Id'));

    Result := Lista.CommaText;
  Finally
    Lista.Free;
  End;
end;

function TAiOpenAssistant.ListAssistantFilesArray: TAiFilesArray;
begin
  Result := FAssistant.ListAssistantFilesArray;
end;

function TAiOpenAssistant.ListFiles: String;
Var
  AiFiles: TAiFiles;
  AiFile: TAiFile;
  List: TAiFilesArray;
  Clave: String;
begin
  AiFiles := TAiFiles.Create(ApiKey);
  Try
    List := AiFiles.ListFiles;

    For Clave in List.Keys do
    Begin
      AiFile := List.Items[Clave];
      Result := Result + sLineBreak + AiFile.id;
    End;
  Finally
    List.Free;
  End;
end;

function TAiOpenAssistant.ListFilesArray: TAiFilesArray;
Var
  AiFiles: TAiFiles;
begin
  AiFiles := TAiFiles.Create(ApiKey);
  Try
    Result := AiFiles.ListFiles;
  Finally
    // AiFiles.Free;
  End;
end;

function TAiOpenAssistant.LoadAssistant(AssistantName: String; ThReadId: String = ''): Boolean;
Var
  AssistantId: String;
begin
  Result := False;
  FActive := False;

  If AssistantName = '' then
    Exit;

  AssistantId := TAiAssistant.GetAssistantIdByName(FApiKey, AssistantName);

  If AssistantId <> '' then
  Begin
    FAssistant.LoadAssistant(AssistantId);
    // Model := FAssistant.Model;
    FAssistantId := FAssistant.AssistantId;
    // FAssistantName := FAssistant.Name;
    // FCode_Interpreter := FAssistant.Code_Interpreter;
    // FFunciones := FAssistant.Funciones;
    // FRetrieval := FAssistant.Retrieval;
    FInstructions.Text := FAssistant.Instructions;
    FMetadata.AsText := FAssistant.Metadata.AsText;
    FFilesIds.Text := FAssistant.FilesIds.Text;

    If (ThReadId <> '') then
      FThRead.LoadThRead(ThReadId)
    Else
      FThRead.CreateNewThread(Nil, Nil);

    FThreadId := FThRead.ThReadId;

    InitToolsFunctions; // Actualiza las funciones en el assistant

    Result := True;
    FActive := True;
  End;
end;

function TAiOpenAssistant.RemoveAssistant: Boolean;
begin
  FAssistant.Remove;
  Result := True;
end;

function TAiOpenAssistant.RemoveThread: Boolean;
begin
  ThRead.Remove;
  Result := True;
end;

procedure TAiOpenAssistant.DoResponse(Response: TAiMessage);
Var
  Content: String;
  Msg: TAiMessageContent;
begin
  If Assigned(FOnResponse) then
  Begin
    for Msg in Response.Content do
      Content := Content + sLineBreak + Msg.Text;

    FOnResponse(Self, Response, Content);
  End;
end;

procedure TAiOpenAssistant.DoStatusChange(aStatus: String);
begin
  If Assigned(FOnStatusChange) then
  Begin
    Self.FStatus := aStatus;
    FOnStatusChange(Self, aStatus);
  End;
end;

function TAiOpenAssistant.DownloadFile(FileId: String): TAiFile;
Var
  AiFiles: TAiFiles;
begin
  AiFiles := TAiFiles.Create(FApiKey);
  Try
    Result := AiFiles.GetFile(FileId);
  Finally
    AiFiles.Free;
  End;
end;

function TAiOpenAssistant.GetAssistantName: String;
begin
  Result := FAssistant.Name;
end;

function TAiOpenAssistant.GetBusy: Boolean;
begin
  If Assigned(FAiRun) then
    Result := FAiRun.Busy;
end;

function TAiOpenAssistant.GetCode_Interpreter: Boolean;
begin
  Result := FAssistant.Code_Interpreter;
end;

function TAiOpenAssistant.GetFunciones: TAiToolsFunctions;
begin
  Result := FAssistant.Funciones;
end;

function TAiOpenAssistant.GetLasMessage: TAiMessage;
Var
  AiMessages: TAiMessages;
begin
  Result := Nil;
  If Assigned(FThRead) then
  Begin
    AiMessages := FThRead.ListMessages(1);
    If AiMessages.Count > 0 then
      Result := AiMessages[0];
  End;
end;

function TAiOpenAssistant.GetAssistantsList: String;
Var
  JObj: TJSonObject;
  JVal: TJSonValue;
  JArr: TJsonArray;
  Lista: TStringList;
begin
  JObj := FAssistant.GetList(ApiKey);
  Lista := TStringList.Create;

  Try
    JArr := JObj.GetValue<TJsonArray>('data');

    For JVal in JArr do
      Lista.Add(JVal.GetValue<String>('name'));

    Result := Lista.CommaText;

  Finally
    JObj.Free;
    Lista.Free;
  End;
end;

function TAiOpenAssistant.GetModelsList: String;
Var
  JObj: TJSonObject;
  JVal: TJSonValue;
  JArr: TJsonArray;
  Lista: TStringList;
begin
  JObj := FAssistant.GetModels(ApiKey);
  Lista := TStringList.Create;

  Try
    JArr := JObj.GetValue<TJsonArray>('data');

    For JVal in JArr do
      Lista.Add(JVal.GetValue<String>('id'));

    Result := Lista.CommaText;

  Finally
    JObj.Free;
    Lista.Free;
  End;
end;

function TAiOpenAssistant.GetModel: String;
begin
  Result := FAssistant.Model;
end;

function TAiOpenAssistant.GetResponse: String;
Var
  AiMessages: TAiMessages;
begin
  Result := '';
  If Assigned(FThRead) then
  Begin
    AiMessages := FThRead.ListMessages(1);
    If AiMessages.Count > 0 then
      Result := AiMessages[0].Content[0].Text;
  End;
end;

function TAiOpenAssistant.GetRetrieval: Boolean;
begin
  Result := FAssistant.Retrieval;
end;

procedure TAiOpenAssistant.SetActive(const Value: Boolean);
begin
  If (FActive <> Value) then
  Begin
    If (Value = True) then
    Begin
      FActive := LoadAssistant(AssistantName, FThreadId);
    End
    Else
    Begin
      If FAutoRemoveThread and (FThreadId <> '') then
      Begin
        Try
          FThRead.Remove;
          Sleep(500);
        Except
        End;
        FThreadId := '';
      End;

      Model := '';
      FAssistantId := '';
      // FAssistantName := FAssistant.Name;
      Code_Interpreter := False;
      Funciones.Clear;
      Retrieval := False;
      FInstructions.Text := '';
      FMetadata.AsText := '';
      FFilesIds.Text := '';
      FActive := False;
      FTools.Text := '';
    End;
  end;
end;

procedure TAiOpenAssistant.SetApiKey(const Value: String);
begin
  FApiKey := Value;
  FAssistant.ApiKey := FApiKey;
end;

procedure TAiOpenAssistant.SetAssistantName(const Value: String);
begin
  FAssistant.Name := Value;
end;

procedure TAiOpenAssistant.SetAsynchronous(const Value: Boolean);
begin
  FAsynchronous := Value;
end;

procedure TAiOpenAssistant.SetAutoRemoveThread(const Value: Boolean);
begin
  FAutoRemoveThread := Value;
end;

procedure TAiOpenAssistant.SetCode_Interpreter(const Value: Boolean);
begin
  FAssistant.Code_Interpreter := Value;
end;

procedure TAiOpenAssistant.SetFilesIds(const Value: TStringList);
begin
  FFilesIds := Value;
end;

procedure TAiOpenAssistant.SetFunciones(const Value: TAiToolsFunctions);
begin
  FAssistant.Funciones := Value;
end;

procedure TAiOpenAssistant.SetInstructions(const Value: TStringList);
begin
  FInstructions.Text := Value.Text;
end;

procedure TAiOpenAssistant.SetMetadata(const Value: TAiMetadata);
begin
  FMetadata := Value;
end;

procedure TAiOpenAssistant.SetModel(const Value: String);
begin
  FAssistant.Model := Value;
end;

procedure TAiOpenAssistant.SetOnBeforeResponse(const Value: TOnBeforeResponse);
begin
  FOnBeforeResponse := Value;
  If Assigned(FAssistant) then
    FAssistant.OnBeforeResponse := Value;
end;

procedure TAiOpenAssistant.SetOnCallToolFunction(const Value: TOnCallToolFunction);
begin
  FOnCallToolFunction := Value;
  If Assigned(FAiRun) then
    FAiRun.OnCallToolFunction := Value;
end;

procedure TAiOpenAssistant.SetOnResponse(const Value: TOnAssistantResponse);
begin
  FOnResponse := Value;
end;

procedure TAiOpenAssistant.SetOnStatusChange(const Value: TOnStatusNotifyEvent);
begin
  FOnStatusChange := Value;
end;

procedure TAiOpenAssistant.SetRetrieval(const Value: Boolean);
begin
  FAssistant.Retrieval := Value;
end;

procedure TAiOpenAssistant.SetThreadId(const Value: String);
begin
  If Value <> FThreadId then
  Begin
    Active := False;
  End;

  FThreadId := Value;
end;

procedure TAiOpenAssistant.SetTools(const Value: TStrings);
begin
  FTools.Text := Value.Text;
end;

function TAiOpenAssistant.UploadAssistantFile(FileName: String): TAiFile;
Var
  AiFiles: TAiFiles;
begin
  AiFiles := TAiFiles.Create(FApiKey);
  Try
    Result := AiFiles.UploadFile(FileName, True);
    Assistant.AddFileId(Result.id);
  Finally
    AiFiles.Free;
  End;
end;

function TAiOpenAssistant.UploadFile(FileName: String): TAiFile;
Var
  AiFiles: TAiFiles;
begin
  AiFiles := TAiFiles.Create(FApiKey);
  Try
    Result := AiFiles.UploadFile(FileName, True);
  Finally
    AiFiles.Free;
  End;
end;

end.
