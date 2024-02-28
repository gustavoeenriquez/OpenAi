unit uOpenAi;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Threading,
  System.Variants, System.Net.Mime, System.IOUtils, System.Generics.Collections,
  System.NetEncoding,
  System.JSON, System.StrUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent,
  REST.JSON, REST.Types, REST.Client;

type
  TAiImageSize = (TiaSize256, TiaSize512, TiaSize1024, TiaSize1024_1792, TiaSize1792_1024);
  TAiImageResponseFormat = (tiaRUrl, tiaRB64);
  TAiImageAStyleFormat = (tiaStyleVivid, tiaStyleNatural);

  TAiMetadata = Class;
  TAiMessage = Class;

  TAiFile = Class(TObject)
  Private
    Ffilename: String;
    Fcreated_at: String;
    Fid: String;
    Fpurpose: String;
    Fstatus: String;
    FTypeObject: String;
    FFileType: String;
    FContent: TMemoryStream;
    Fbytes: Integer;
    FApiKey: String;
    procedure Setfilename(const Value: String);
    function GetContent: TMemoryStream;
    procedure Setid(const Value: String);
    procedure SetApiKey(const Value: String);
  Protected
  Public
    Constructor Create(aApiKey: String);
    Destructor Destroy; Override;
    Property TypeObject: String read FTypeObject;
    Property id: String read Fid write Setid;
    Property purpose: String read Fpurpose;
    Property filename: String read Ffilename write Setfilename;
    Property bytes: Integer read Fbytes;
    Property created_at: String read Fcreated_at;
    Property status: String read Fstatus;
    Property Content: TMemoryStream read GetContent;
    Property FileType: String read FFileType;
    Property ApiKey: String read FApiKey write SetApiKey;
  End;

  TAiFilesArray = Class(TDictionary<String, TAiFile>)
  Private
  Protected
  Public
    function GetFileById(AiFileId: String; Out AiFile: TAiFile): Boolean;
    Function GetFileByName(aFileName: String; Out AiFile: TAiFile): Boolean;
  End;

  TAiFiles = Class(TObject)
  Private
    FApiKey: String;
    FFileList: TAiFilesArray;
    procedure SetApiKey(const Value: String);
    function GetFileList: TAiFilesArray;
  Protected
  Public
    Constructor Create(aApiKey: String);
    Destructor Destroy; Override;
    Function UploadFile(filename: String; IsForAssistant: Boolean): TAiFile;
    Function ListFiles: TAiFilesArray;
    Function DeleteFile(aFileId: String): Boolean;
    Function GetFile(aFileId: String): TAiFile;
    Function GetFileByName(aFileName: String): TAiFile;

    Property ApiKey: String read FApiKey write SetApiKey;
    Property FileList: TAiFilesArray Read GetFileList;
  End;

{$REGION 'Open Assitant Classes'}

  TAiToolsFunction = class(TObject)
    id: string;
    Tipo: string;
    Name: string;
    Description: String;
    Arguments: string;
    &Function: string;
    Response: String;
    Body: TJsonObject;
    Metadata: TAiMetadata;

    Constructor Create;
    Destructor Destroy; Override;
    Procedure ParseFunction(JObj: TJsonObject);
    Procedure Assign(aSource: TAiToolsFunction);
  end;

  TAiToolsFunctions = Class(TDictionary<String, TAiToolsFunction>)
  Private
  Protected
  Public
    Function ToOutputJSon: TJSonArray;
    Function ToFunctionsJSon: TJSonArray;
    Procedure AddFunction(aBody: String); Overload;
    Procedure AddFunction(aBody: TJsonObject); Overload;
  End;

  TOnCallToolFunction = Procedure(Sender: TObject; AiToolCall: TAiToolsFunction) of object;
  TOnAssistantResponse = Procedure(Sender: TObject; Response: TAiMessage; Content: String) of object;
  TOnStatusNotifyEvent = Procedure(Sender: TObject; aStatus: String) of object;
  TOnBeforeResponse = Procedure(Sender: TObject; Funcion, Response: String) of object;

  TAiMetadata = Class(TDictionary<String, String>)
  Private
    function GetAsText: String;
    procedure SetAsText(const Value: String);
    function GetJSonText: String;
    procedure SetJsonText(const Value: String);
  Protected
  Public
    Function ToJSon: TJsonObject;
    Property AsText: String Read GetAsText Write SetAsText;
    Property JsonText: String Read GetJSonText Write SetJsonText;
  End;

  TAiMessageAnotations = Record
    Tipo: String;
    Text: String;
    filename: String;
    FileType: String;
    FileId: String;
  End;

  TAiMessageAnotationsArray = Array of TAiMessageAnotations;

  TAiMessageContent = Record
    Tipo: String;
    Text: String;
    Anotations: TAiMessageAnotationsArray;
  End;

  TAiMessageContentArray = Array of TAiMessageContent;

  TAiMessage = Class(TObject)
  private
    FRole: String;
    Ffile_ids: TStringList;
    FMetadata: TAiMetadata;
    FMessageId: String;
    FAssisistantId: String;
    FRunId: String;
    FFiles: TAiFilesArray;
    FThReadId: String;
    FApiKey: String;
    FContent: TAiMessageContentArray;
    procedure Setrole(const Value: String);
    procedure Setfile_ids(const Value: TStringList);
    procedure SetMetadata(const Value: TAiMetadata);
    procedure SetMessageId(const Value: String);
    procedure SetAssisistantId(const Value: String);
    procedure SetRunId(const Value: String);
    procedure SetThReadId(const Value: String);
    procedure SetApiKey(const Value: String);
    procedure SetContent(const Value: TAiMessageContentArray);
  Public
    Constructor Create(aApiKey: String);
    Destructor Destroy; Override;
    Function ToJSon(ShowAll: Boolean = False): TJsonObject;

    Property MessageId: String read FMessageId write SetMessageId;
    Property role: String read FRole write Setrole;
    Property Content: TAiMessageContentArray read FContent write SetContent;
    Property file_ids: TStringList read Ffile_ids write Setfile_ids;
    Property Files: TAiFilesArray read FFiles;
    Property Metadata: TAiMetadata read FMetadata write SetMetadata;
    Property AssistantId: String read FAssisistantId write SetAssisistantId;
    Property ThReadId: String read FThReadId write SetThReadId;
    Property RunId: String read FRunId write SetRunId;
    Property ApiKey: String read FApiKey write SetApiKey;
  End;

  TAiMessages = Class(TList<TAiMessage>)
  Private
  Protected
  Public
    // Function AddMessage(aRole, aContent, aFiles_ids: String): TAiMessage;
    Function ToJSon(ShowAll: Boolean = False): TJSonArray;
  End;

  TAiAssistant = Class(TObject)
  private
    FName: String;
    FModel: String;
    FInstructions: String;
    FMetadata: TAiMetadata;
    FAssistantId: String;
    FJSonObject: TJsonObject;
    FFilesIds: TStringList;
    FCode_Interpreter: Boolean;
    FRetrieval: Boolean;
    FFunciones: TAiToolsFunctions;
    FTxtJson: String;
    FApiKey: String;
    FOnBeforeResponse: TOnBeforeResponse;
    procedure SetInstructions(const Value: String);
    procedure SetMetadata(const Value: TAiMetadata);
    procedure SetModel(const Value: String);
    procedure SetName(const Value: String);
    procedure SetAssistantId(const Value: String);
    procedure SetJSonObject(const Value: TJsonObject);
    procedure SetFilesIds(const Value: TStringList);
    procedure SetCode_Interpreter(const Value: Boolean);
    procedure SetRetrieval(const Value: Boolean);
    procedure SetFunciones(const Value: TAiToolsFunctions);
    procedure SetApiKey(const Value: String);
    procedure SetOnBeforeResponse(const Value: TOnBeforeResponse);

  Protected
    Procedure ParseAssistantJson(Obj: TJsonObject);
  Public
    Constructor Create(aApiKey: String);
    Destructor Destroy; Override;

    Procedure DoBeforeResponse(Sender: TObject; Funcion, Value: String);

    Class Function GetList(aApiKey: String; Limit: Integer = 20; Order: String = 'desc'): TJsonObject;
    Class Function GetAssistantIdByName(aApiKey: String; AssistantName: String): String;

    Function LoadAssistant(aAssistantId: String): Boolean;
    Function CreateNewAssistant(aModelo, aNombre, aInstrucciones, aFilesId: String; aFunciones: TAiToolsFunctions; aMetadata: TAiMetadata; aRetrieval, aCodeInterpreter: Boolean): Boolean;
    Class Function GetModels(aApiKey: String): TJsonObject;

    Function ApplyUpdates: TJsonObject;
    Function Remove: TJsonObject;
    Function AddFileId(FileId: string): TJsonObject;
    Function ListFiles: TJsonObject;
    Function ListAssistantFilesArray: TAiFilesArray;

    Function RemoveFileId(FileId: string): TJsonObject;

    Property Model: String read FModel write SetModel;
    Property Name: String read FName write SetName;
    Property Instructions: String read FInstructions write SetInstructions;
    Property FilesIds: TStringList read FFilesIds write SetFilesIds;
    Property Metadata: TAiMetadata read FMetadata write SetMetadata;
    Property AssistantId: String read FAssistantId write SetAssistantId;
    Property JSonObject: TJsonObject read FJSonObject write SetJSonObject;
    Property Code_Interpreter: Boolean read FCode_Interpreter write SetCode_Interpreter;
    Property Retrieval: Boolean read FRetrieval write SetRetrieval;
    Property Funciones: TAiToolsFunctions read FFunciones write SetFunciones;
    Property TxtJson: String Read FTxtJson;
    Property ApiKey: String read FApiKey write SetApiKey;
    Property OnBeforeResponse: TOnBeforeResponse read FOnBeforeResponse write SetOnBeforeResponse;
  End;

  TAiThRead = Class(TObject)
  Private
    FMessages: TAiMessages;
    FMetadata: TAiMetadata;
    FThReadId: String;
    FJSonObject: TJsonObject;
    FAiAssistant: TAiAssistant;
    procedure SetMessages(const Value: TAiMessages);
    procedure SetMetadata(const Value: TAiMetadata);
    procedure SetThReadId(const Value: String);
    procedure SetJSonObject(const Value: TJsonObject);
    procedure SetAiAssistant(const Value: TAiAssistant);
  Protected
    Procedure ParseThReadJson(JObj: TJsonObject);
    Function ParseMessage(JObj: TJsonObject): TAiMessage;
  Public
    Constructor Create(aAiAssistant: TAiAssistant);
    // Constructor Create(ThReadId: String); Overload;
    // Constructor Create(aMensajes: TAiMessages; aMetadata: TAiMetadata); Overload;
    Destructor Destroy; Override;

    Function LoadThRead(ThReadId: String): Boolean;
    Function CreateNewThRead(aMensajes: TAiMessages; aMetadata: TAiMetadata): Boolean;

    Function ApplyUpdates: Boolean;
    Function Remove: Boolean;
    Function AddMessage(aMessage: String; aRole: String = ''; aFiles_ids: String = ''): TAiMessage;
    Function ListMessages(Limit: Integer = 20; Order: String = 'desc'): TAiMessages;
    Function GetMessage(aMessageId: String): TAiMessage;
    Function GetMessageFile(aThReadId, aMessageId, aFileId: String): TJsonObject;
    Function ListMessageFiles(aMsgId: String): TJsonObject;
    Function MessageApplyUpdate(aMessage: TAiMessage): TJsonObject;

    Property Messages: TAiMessages read FMessages write SetMessages;
    Property Metadata: TAiMetadata read FMetadata write SetMetadata;
    Property ThReadId: String read FThReadId write SetThReadId;
    Property JSonObject: TJsonObject read FJSonObject write SetJSonObject;
    Property AiAssistant: TAiAssistant read FAiAssistant write SetAiAssistant;
  End;

  TAiRun = Class(TObject)
  Private
    FThRead: TAiThRead;
    FAssistant: TAiAssistant;
    FRunId: String;
    Fstatus: String;
    FLastError: String;
    FMetadata: TAiMetadata;
    FJObjectRun: TJsonObject;
    FToolsCalls: TAiToolsFunctions;
    FOnCallToolFunction: TOnCallToolFunction;
    FBusy: Boolean;
    procedure SetMetadata(const Value: TAiMetadata);
    procedure SetJObjectRun(const Value: TJsonObject);
    procedure SetToolsCalls(const Value: TAiToolsFunctions);
    procedure SetOnCallToolFunction(const Value: TOnCallToolFunction);
  Protected
    Procedure ParseRun(JObj: TJsonObject);
    Function ExtractToolCallsFromJson(JSonObject: TJsonObject): Boolean;
    Procedure InternalCallToolFunction(AiToolCall: TAiToolsFunction);
  Public
    Constructor Create(aAssistant: TAiAssistant; aThRead: TAiThRead; aAditionalInstructions: String = ''; aMetadata: TAiMetadata = Nil); Overload;
    Destructor Destroy; Override;

    Function Run: Boolean;

    Function ListRuns: TJsonObject;
    Function ListRunsSteps: TJsonObject;
    Function Retrieve: TJsonObject;
    Function RetrieveStep(StepId: String): TJsonObject;

    Function ApplyUpdates: TJsonObject;

    Function SubmitTool(AitoolsOutputs: TAiToolsFunctions): TJsonObject;
    Function Cancel: TJsonObject;

    Property Assistant: TAiAssistant read FAssistant;
    Property ThRead: TAiThRead read FThRead;
    Property RunId: String read FRunId;
    Property status: String Read Fstatus;
    Property LastError: String Read FLastError;
    Property Metadata: TAiMetadata read FMetadata write SetMetadata;
    Property JObjectRun: TJsonObject read FJObjectRun write SetJObjectRun;
    Property ToolsCalls: TAiToolsFunctions read FToolsCalls write SetToolsCalls;
    Property OnCallToolFunction: TOnCallToolFunction read FOnCallToolFunction write SetOnCallToolFunction;
    Property Busy: Boolean read FBusy;

  End;

{$ENDREGION}

  TAiAudio = Class(TObject)
  Private
    FApiKey: String;
    procedure SetApiKey(const Value: String);
  Protected
    Function IsValidExtension(FileExtension: String): Boolean;
    Function ConvertFileFormat(Origen: TMemoryStream; filename: String; out Destino: TMemoryStream; Out DestinoFileName: String): Boolean;
  Public
    Constructor Create(aApiKey: String);
    Destructor Destroy; Override;
    Function Speech(Text: String; Quality: Boolean = True; Voice: String = 'nova'; Format: String = 'mp3'; Speed: Single = 1): TMemoryStream;
    Function Transcription(Stream: TMemoryStream; filename, Prompt: String; Languaje: String = 'es'; ResponseFormat: String = 'text'; Model: String = 'whisper-1'; Temperature: String = '0'): String;
    Function Translation(Stream: TMemoryStream; filename, Prompt: String; ResponseFormat: String = 'text'; Model: String = 'whisper-1'; Temperature: String = '0'): String;
    Property ApiKey: String read FApiKey write SetApiKey;
  End;

  TEmbedding = Array [0 .. 1535] of Double;
  TEmbeddingArray = TArray<TEmbedding>;

  TAiEmbeddings = Class(TObject)
  Private
    FApiKey: String;
    FModel: String;
    Ftotal_tokens: Integer;
    Fprompt_tokens: Integer;
    FData: TEmbeddingArray;
    procedure SetApiKey(const Value: String);
    procedure SetModel(const Value: String);
    procedure SetData(const Value: TEmbeddingArray);
  Protected
  Public
    Constructor Create(aApiKey: String);
    Destructor Destroy; Override;
    Function CreateEmbedding(Input, User: String; Dimensions: Integer = 1536; Model: String = 'text-embedding-3-small'; EncodingFormat: String = 'float'): TEmbeddingArray;
    Procedure ParseEmbedding(JObj: TJsonObject);
    Function ToJsonArray: TJSonArray; Overload;
    class Function ToJsonArray(Val: TEmbedding): TJSonArray; Overload;

    class function Magnitude(const V: TEmbedding): Double;
    class function DotProduct(const A, B: TEmbedding): Double;
    class function CosineSimilarity(const A, B: TEmbedding): Double;

    Property ApiKey: String read FApiKey write SetApiKey;
    Property Data: TEmbeddingArray read FData write SetData;
    Property Model: String read FModel write SetModel;
    Property prompt_tokens: Integer read Fprompt_tokens;
    Property total_tokens: Integer read Ftotal_tokens;
  End;

  TAiImagesFile = Class(TObject)
  Private
    FImage: TMemoryStream;
    FBase64: String;
    Frevised_prompt: String;
    FUrl: String;
    function GetImage: TMemoryStream;
  Protected
    function Base64ToStream(Base64: String): TMemoryStream;
    Function LoadImage(Url: String): TMemoryStream;
    Procedure ParseImage(JObj: TJsonObject);
  Public
    Constructor Create;
    Destructor Destroy; Override;

  Published
    Property Revised_Prompt: String read Frevised_prompt;
    Property Base64: String read FBase64;
    Property Url: String read FUrl;
    Property Image: TMemoryStream Read GetImage;
  End;

  TAiImagesFiles = Array of TAiImagesFile;

  TAiImages = Class(TObject)
  Private
    FApiKey: String;
    Frevised_prompt: String;
    FImages: TAiImagesFiles;
    FPrompt: String;
    procedure SetApiKey(const Value: String);
    procedure Setrevised_prompt(const Value: String);
    procedure SetImages(const Value: TAiImagesFiles);
  Protected
    Procedure ParseGenerate(JObj: TJsonObject);
    Procedure ParseVariations(JObj: TJsonObject);
  Public
    Constructor Create(aApiKey: String);
    Destructor Destroy; Override;
    Function Generate(aPrompt, aUser: String; aSize: TAiImageSize; N: Integer; ResponseFormat: TAiImageResponseFormat; HdQuality: Boolean; Style: TAiImageAStyleFormat; UseDalle3: Boolean): TAiImagesFile;
    Function Edit(aImage, aMask: TMemoryStream; aPrompt, aUser: String; aSize: TAiImageSize; N: Integer; ResponseFormat: TAiImageResponseFormat): TAiImagesFile;
    Function Variation(aImage: TMemoryStream; aUser: String; aSize: TAiImageSize; N: Integer; ResponseFormat: TAiImageResponseFormat): TAiImagesFile;

    Property ApiKey: String read FApiKey write SetApiKey;
    Property Revised_Prompt: String read Frevised_prompt write Setrevised_prompt;
    Property Images: TAiImagesFiles read FImages write SetImages;
    Property Prompt: String Read FPrompt;
  End;

  TAiVision = Class(TObject)
  Private
    FApiKey: String;
    Fcompletion_tokens: Integer;
    FModel: String;
    Ftotal_tokens: Integer;
    FFinish_reason: String;
    FRole: String;
    Fid: String;
    FContent: String;
    FPrompt_Tokenes: Integer;
    procedure SetApiKey(const Value: String);
  Protected
    Function ParseVision(Response: TJsonObject): String;
  Public
    Constructor Create(aApiKey: String);
    Destructor Destroy; Override;
    Function GenerateByUrl(Text, Url: String; role: String = 'user'; Max_tokens: Integer = 4000; detail: Boolean = False): String;
    function GenerateByBase64(Text, Base64: String; role: String = 'user'; Max_tokens: Integer = 4000; detail: Boolean = False): String;
    function GenerateByStream(Text: String; Stream: TMemoryStream; role: String = 'user'; Max_tokens: Integer = 4000; detail: Boolean = False): String;

    Property ApiKey: String read FApiKey write SetApiKey;
    Property id: String read Fid;
    Property Model: String read FModel;
    Property Finish_reason: String read FFinish_reason;
    Property role: String read FRole;
    Property Content: String read FContent;
    Property Prompt_Tokenes: Integer read FPrompt_Tokenes;
    Property completion_tokens: Integer read Fcompletion_tokens;
    Property total_tokens: Integer read Ftotal_tokens;
  End;

implementation

{$IFDEF LINUX}

uses uLinuxUtils;
{$ENDIF}
{$IFDEF MSWINDOWS}

uses ShellAPI, WinApi.Windows;
{$ENDIF}
{$REGION 'Utilidades varias' }

function GetParametrosURL(Parametros: TStringList): string;
var
  i: Integer;
begin
  Result := '';
  if Assigned(Parametros) and (Parametros.Count > 0) then
  begin
    Result := '?';
    for i := 0 to Parametros.Count - 1 do
    begin
      Result := Result + Parametros.Names[i] + '=' + Parametros.ValueFromIndex[i];
      if i < Parametros.Count - 1 then
        Result := Result + '&';
    end;
  end;
end;

function StreamToBase64(Stream: TMemoryStream): String;
begin
  Stream.Position := 0;
  Result := TNetEncoding.Base64.EncodeBytesToString(Stream.Memory, Stream.Size);
end;

function GetContentType(FileExtension: string): string;
begin

  FileExtension := LowerCase(Trim(StringReplace(FileExtension, '.', '', [rfReplaceAll])));

  if SameText(FileExtension, 'mp3') then
    Result := 'audio/mpeg'
  else if SameText(FileExtension, 'mp4') then
    Result := 'video/mp4'
  else if SameText(FileExtension, 'mpeg') then
    Result := 'video/mpeg'
  else if SameText(FileExtension, 'mpga') then
    Result := 'audio/mpeg'
  else if SameText(FileExtension, 'm4a') then
    Result := 'audio/mp4'
  else if SameText(FileExtension, 'ogg') then
    Result := 'audio/ogg'
  else if SameText(FileExtension, 'wav') then
    Result := 'audio/wav'
  else if SameText(FileExtension, 'webm') then
    Result := 'video/webm'
  else
    Result := 'application/octet-stream';
  // Tipo de contenido predeterminado para otras extensiones
end;

{$ENDREGION}
{$REGION 'Open Assitant Classes'}
{ TAiMessage }

constructor TAiMessage.Create(aApiKey: String);
begin
  Inherited Create;
  Ffile_ids := TStringList.Create;
  FMetadata := TAiMetadata.Create;
  FFiles := TAiFilesArray.Create;
end;

destructor TAiMessage.Destroy;
begin
  Ffile_ids.Free;
  FMetadata.Free;
  FFiles.Free;
  inherited Destroy;
end;

procedure TAiMessage.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiMessage.SetAssisistantId(const Value: String);
begin
  FAssisistantId := Value;
end;

procedure TAiMessage.SetContent(const Value: TAiMessageContentArray);
begin
  FContent := Value;
end;

procedure TAiMessage.Setfile_ids(const Value: TStringList);
begin
  Ffile_ids := Value;
end;

procedure TAiMessage.SetMessageId(const Value: String);
begin
  FMessageId := Value;
end;

procedure TAiMessage.SetMetadata(const Value: TAiMetadata);
begin
  FMetadata := Value;
end;

procedure TAiMessage.Setrole(const Value: String);
begin
  FRole := Value;
end;

procedure TAiMessage.SetRunId(const Value: String);
begin
  FRunId := Value;
end;

procedure TAiMessage.SetThReadId(const Value: String);
begin
  FThReadId := Value;
end;

function TAiMessage.ToJSon(ShowAll: Boolean = False): TJsonObject;
Var
  JFiles: TJSonArray;
  i: Integer;
  Con: String;
begin
  Result := TJsonObject.Create;

  If FRole <> '' then
    Result.AddPair('role', FRole);

  If Length(FContent) > 0 then
  Begin
    For i := 0 to Length(FContent) - 1 do
      Con := Con + FContent[i].Text + sLineBreak;

    Result.AddPair('content', Trim(Con));
  End;

  If ShowAll then
  Begin
    If FMessageId <> '' then
      Result.AddPair('MessageId', UTF8Encode(FMessageId));

    If FAssisistantId <> '' then
      Result.AddPair('AsissistantId', UTF8Encode(FAssisistantId));

    If RunId <> '' then
      Result.AddPair('RunId', UTF8Encode(FRunId));
  End;

  If Ffile_ids.Count > 0 then
  Begin
    JFiles := TJSonArray.Create;

    For i := 0 to file_ids.Count - 1 do
    Begin
      If file_ids[i].Trim <> '' then
        JFiles.Add(file_ids[i].Trim);
    End;

    Result.AddPair('file_ids', JFiles);
  End;

  Result.AddPair('metadata', FMetadata.ToJSon);

end;

{ TAiMessages }

{
  function TAiMessages.AddMessage(aRole, aContent, aFiles_ids: String): TAiMessage;
  Var
  AiMessage: TAiMessage;
  begin
  AiMessage := TAiMessage.Create(aRole, aContent, aFiles_ids);
  Self.Add(AiMessage);
  end;
}

function TAiMessages.ToJSon(ShowAll: Boolean = False): TJSonArray;
Var
  AiMessage: TAiMessage;
begin
  Result := TJSonArray.Create;

  For AiMessage in Self do
    Result.Add(AiMessage.ToJSon(ShowAll));

end;

{ TAiMetadata }

function TAiMetadata.GetAsText: String;
Var
  Lista: TStringList;
  Clave: String;
begin

  Lista := TStringList.Create;
  Try
    For Clave in Self.Keys do
      Lista.Values[Clave] := Self.Items[Clave];

    Result := Lista.Text;

  Finally
    Lista.Free;
  End;
end;

function TAiMetadata.GetJSonText: String;
Var
  JObj: TJsonObject;
  Clave: String;
begin
  JObj := TJsonObject.Create;

  Try
    For Clave in Self.Keys do
      JObj.AddPair(Clave, Self.Items[Clave]);

    Result := JObj.Format;
  Finally
    JObj.Free;
  End;
end;

procedure TAiMetadata.SetAsText(const Value: String);
Var
  Lista: TStringList;
  Clave, Valor: String;
  i: Integer;
begin

  Lista := TStringList.Create;

  Try
    Lista.Text := Value;
    Self.Clear;
    For i := 0 to Lista.Count - 1 do
    Begin
      Clave := Lista.Names[i];
      Valor := Lista.Values[Clave];
      Self.Add(Clave, Valor);
    End;
  Finally
    Lista.Free;
  End;

end;

procedure TAiMetadata.SetJsonText(const Value: String);
Var
  JObj: TJsonObject;
  Pair: TJSONPair;
begin
  JObj := TJsonObject(TJsonObject.ParseJSONValue(Value));

  Self.Clear;
  For Pair in JObj do
    Self.Add(Pair.JsonString.Value, Pair.JsonValue.Value)
end;

function TAiMetadata.ToJSon: TJsonObject;
Var
  Clave: String;
begin
  Result := TJsonObject.Create;

  For Clave in Self.Keys do
    Result.AddPair(Clave, Self.Items[Clave]);
end;

{ TAitools_outputs }

procedure TAiToolsFunctions.AddFunction(aBody: TJsonObject);
Var
  Func, Func1: TAiToolsFunction;
begin
  Func := TAiToolsFunction.Create;
  Func.ParseFunction(aBody);

  If Self.TryGetValue(Func.Name, Func1) = False then
    Self.Add(Func.Name, Func)
  Else
  Begin
    Func1.Assign(Func);
    Func.Free;
  End;
end;

procedure TAiToolsFunctions.AddFunction(aBody: String);
Var
  Func: TJsonObject;
begin
  Func := TJsonObject(TJsonObject.ParseJSONValue(aBody));
  AddFunction(Func);

end;

function TAiToolsFunctions.ToFunctionsJSon: TJSonArray;
Var
  Clave: String;
  TObj: TJsonObject;
  Func: TAiToolsFunction;
begin
  Result := TJSonArray.Create;

  For Clave in Self.Keys do
  Begin
    Func := Self.Items[Clave];
    // Result.Add(TJSonObject(TJSonObject.ParseJSONValue(Self.Items[Clave].&Function)));
    TObj := TJsonObject(Func.Body.Clone);
    // TObj.AddPair('type', 'function');
    // TObj.AddPair('function', TJsonObject(Func.Body.Clone));
    Result.Add(TObj);
  End;
end;

function TAiToolsFunctions.ToOutputJSon: TJSonArray;
Var
  Clave: String;
  TObj: TJsonObject;
begin
  Result := TJSonArray.Create;

  For Clave in Self.Keys do // La clave es el nombre de la función
  Begin
    TObj := TJsonObject.Create;
    TObj.AddPair('tool_call_id', Self.Items[Clave].id);
    TObj.AddPair('output', Self.Items[Clave].Response);
    Result.Add(TObj);
  End;
end;

{ TAssistant }

function TAiAssistant.AddFileId(FileId: string): TJsonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJsonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin

  If FAssistantId = '' then
    Raise Exception.Create('Primero debe crear un asistente');

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/assistants/' + FAssistantId + '/files';
  JObj := TJsonObject.Create;

  Try
    JObj.AddPair('file_id', FileId);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        DoBeforeResponse(Self, 'TAiAssistant.AddFileId', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      Result := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
  End;
end;

constructor TAiAssistant.Create(aApiKey: String);
begin
  Inherited Create;
  FApiKey := aApiKey;
  FMetadata := TAiMetadata.Create;
  FFilesIds := TStringList.Create;
  FFunciones := TAiToolsFunctions.Create;
end;

Function TAiAssistant.CreateNewAssistant(aModelo, aNombre, aInstrucciones, aFilesId: String; aFunciones: TAiToolsFunctions; aMetadata: TAiMetadata; aRetrieval, aCodeInterpreter: Boolean): Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJsonObject;
  JTools, JFiles: TJSonArray;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  i: Integer;
  ListFilesIds: TStringList;
begin
  If FApiKey = '' then
    Raise Exception.Create('Inicialice el ApiKey antes de continuar');

  FMetadata.Clear;
  FFilesIds.Clear;
  FFunciones.Clear;
  FName := '';
  FModel := '';
  FInstructions := '';
  FAssistantId := '';
  FreeAndNil(FJSonObject);
  FCode_Interpreter := False;
  FRetrieval := False;

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  ListFilesIds := TStringList.Create;
  JObj := TJsonObject.Create;

  sUrl := 'https://api.openai.com/v1/assistants';

  Try
    JObj.AddPair('model', aModelo);
    JObj.AddPair('name', aNombre);
    JObj.AddPair('instructions', aInstrucciones);

    JTools := aFunciones.ToFunctionsJSon;

    If aCodeInterpreter = True then
      JTools.Add(TJsonObject.Create.AddPair('type', 'code_interpreter'));

    If aRetrieval = True then
      JTools.Add(TJsonObject.Create.AddPair('type', 'retrieval'));

    JObj.AddPair('tools', JTools);

    JFiles := TJSonArray.Create;

    ListFilesIds.CommaText := aFilesId;

    For i := 0 to ListFilesIds.Count - 1 do
      JFiles.Add(ListFilesIds[i]);

    JObj.AddPair('file_ids', JFiles);

    JObj.AddPair('metadata', aMetadata.ToJSon);

    St.WriteString(JObj.Format);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    FTxtJson := JObj.Format;

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        DoBeforeResponse(Self, 'TAiAssistant.CreateNewAssistant', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      FJSonObject := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      ParseAssistantJson(FJSonObject);
      Result := True;
    End
    else
    begin
      // FJSonObject := TJsonObject.Create.AddPair('error', Res.ContentAsString);
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    ListFilesIds.Free;
    JObj.Free;
  End;
end;

destructor TAiAssistant.Destroy;
begin
  FOnBeforeResponse := Nil;
  FMetadata.Free;
  FFilesIds.Free;
  FFunciones.Free;
  Inherited Destroy;
end;

procedure TAiAssistant.DoBeforeResponse(Sender: TObject; Funcion, Value: String);
begin
  If Assigned(FOnBeforeResponse) then
    FOnBeforeResponse(Sender, Funcion, Value);
end;

class function TAiAssistant.GetAssistantIdByName(aApiKey: String; AssistantName: String): String;
Var
  JArr: TJSonArray;
  JVal: TJSonValue;
  JObj: TJsonObject;
  sId, sName: String;
begin
  Result := '';

  JObj := GetList(aApiKey, 100);
  JArr := JObj.GetValue<TJSonArray>('data');

  For JVal in JArr do
  Begin
    JObj := TJsonObject(JVal);
    sId := JObj.GetValue<String>('id');
    sName := JObj.GetValue<String>('name');

    If SameText(AssistantName, sName) then
    Begin
      Result := sId;
      Break;
    End;
  End;
end;

Class function TAiAssistant.GetList(aApiKey: String; Limit: Integer = 20; Order: String = 'desc'): TJsonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl: String;
begin

  If Limit > 100 then
    Limit := 100;

  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/assistants?limit=' + Limit.ToString + '&order=' + Order;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + aApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      Result := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
    Response.Free;
  End;
end;

Class function TAiAssistant.GetModels(aApiKey: String): TJsonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/models';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + aApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      Result := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
    Response.Free;
  End;
end;

function TAiAssistant.ListAssistantFilesArray: TAiFilesArray;
Var
  JArr: TJSonArray;
  JVal: TJSonValue;
  AiFiles: TAiFiles;
  AllFiles: TAiFilesArray;
  FFile: TAiFile;
  id: String;
begin

  Result := TAiFilesArray.Create;

  JArr := ListFiles.GetValue<TJSonArray>('data');
  AiFiles := TAiFiles.Create(ApiKey);

  AllFiles := AiFiles.ListFiles;

  Try
    If Assigned(JArr) then
    Begin
      For JVal in JArr do
      Begin
        id := JVal.GetValue<String>('id');

        If AllFiles.GetFileById(id, FFile) then
          Result.Add(id, FFile);
      End;
    End;
  Finally
    JArr.Free;
    AiFiles.Free;
  End;

end;

function TAiAssistant.ListFiles: TJsonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl: String;
begin
  If AssistantId = '' then
    Raise Exception.Create('Primero debe crear un asistente');

  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/assistants/' + AssistantId + '/files';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        DoBeforeResponse(Self, 'TAiAssistant.ListFiles', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      Result := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
    Response.Free;
  End;
end;

function TAiAssistant.LoadAssistant(aAssistantId: String): Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl: String;
begin

  FMetadata.Clear;
  FFilesIds.Clear;
  FFunciones.Clear;
  FName := '';
  FModel := '';
  FInstructions := '';
  FAssistantId := '';
  FreeAndNil(FJSonObject);
  FCode_Interpreter := False;
  FRetrieval := False;

  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/assistants/' + aAssistantId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    // Response.SaveToFile('c:\temp\loadasistant.json.txt');

    TThread.Synchronize(nil,
      procedure
      begin
        DoBeforeResponse(Self, 'TAiAssistant.LoadAssistant', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      FJSonObject := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      ParseAssistantJson(FJSonObject);
      Result := True;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
    Response.Free;
  End;
end;

function TAiAssistant.ApplyUpdates: TJsonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJsonObject;
  JTools, JFiles: TJSonArray;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  i: Integer;
begin

  If AssistantId = '' then
    Raise Exception.Create('Primero debe crear un asistente');

  If FModel.Trim = '' then
    Raise Exception.Create('El nombre del modelo es obligatorio');

  If Name.Trim = '' then
    Raise Exception.Create('El nombre del Asistente es obligatorio');

  If FInstructions.Trim = '' then
    Raise Exception.Create('Eres un asistente muy servicial');

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/assistants/' + Self.AssistantId;

  Try
    JObj := TJsonObject.Create;
    JObj.AddPair('model', FModel);
    JObj.AddPair('name', FName);
    JObj.AddPair('instructions', FInstructions);

    JTools := FFunciones.ToFunctionsJSon;

    If FCode_Interpreter = True then
      JTools.Add(TJsonObject.Create.AddPair('type', 'code_interpreter'));

    If FRetrieval = True then
      JTools.Add(TJsonObject.Create.AddPair('type', 'retrieval'));

    JObj.AddPair('tools', JTools);

    JFiles := TJSonArray.Create;

    For i := 0 to FFilesIds.Count - 1 do
      JFiles.Add(FFilesIds[i].Trim);

    JObj.AddPair('file_ids', JFiles);

    JObj.AddPair('metadata', FMetadata.ToJSon);

    St.WriteString(JObj.Format);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    // St.SaveToFile('c:\temp\request.json.txt');

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        DoBeforeResponse(Self, 'TAiAssistant.ApplyUpdates', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      FJSonObject := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      Result := TJsonObject(FJSonObject.Clone);
      ParseAssistantJson(FJSonObject);
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
    St.Free;
    Response.Free;
  End;
end;

procedure TAiAssistant.ParseAssistantJson(Obj: TJsonObject);
Var
  Tools, FileIds: TJSonArray;
  JVal: TJSonValue;
  JObj, JFunc, jMetadata: TJsonObject;
  Pair: TJSONPair;
  Tipo, FunName, S: String;
begin

  FCode_Interpreter := False;
  FRetrieval := False;
  FFunciones.Clear;
  FMetadata.Clear;
  FFilesIds.Clear;

  FAssistantId := FJSonObject.GetValue<String>('id');
  FName := FJSonObject.GetValue<String>('name');
  FModel := FJSonObject.GetValue<String>('model');
  FInstructions := FJSonObject.GetValue<String>('instructions');
  Tools := FJSonObject.GetValue<TJSonArray>('tools');

  Self.FFunciones.Clear; // Limpia la lista de funciones para actualizar

  For JVal in Tools do
  Begin
    JObj := TJsonObject(JVal);

    S := JObj.Format;
    Tipo := JObj.GetValue<String>('type');

    If Tipo = 'code_interpreter' then
      FCode_Interpreter := True;

    If Tipo = 'retrieval' then
      FRetrieval := True;

    If Tipo = 'function' then
    Begin
      JFunc := JObj.GetValue<TJsonObject>('function');
      FunName := JFunc.GetValue<string>('name');
      FFunciones.AddFunction(JObj);
    End;
  End;

  FileIds := FJSonObject.GetValue<TJSonArray>('file_ids');

  For JVal in FileIds do
    FFilesIds.Add(JVal.Value);

  jMetadata := FJSonObject.GetValue<TJsonObject>('metadata');

  For Pair in jMetadata do
    FMetadata.Add(Pair.JsonString.Value, Pair.JsonValue.Value);
end;

function TAiAssistant.Remove: TJsonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl: String;
  id: String;
begin

  id := TAiAssistant.GetAssistantIdByName(ApiKey, Name);

  If id = '' then
    Raise Exception.Create('El asistente "' + Name + '" no existe, no se puede eliminar');

  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/assistants/' + id;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Delete(sUrl, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        DoBeforeResponse(Self, 'TAiAssistant.Remove', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      Result := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    Response.Free;
  End;
end;

function TAiAssistant.RemoveFileId(FileId: string): TJsonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl: String;
begin
  If AssistantId = '' then
    Raise Exception.Create('Primero debe crear o instanciar un asistente');

  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/assistants/' + AssistantId + '/files/' + FileId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Delete(sUrl, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        DoBeforeResponse(Self, 'TAiAssistant.RemoveFileId', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      Result := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    Response.Free;
  End;
end;

procedure TAiAssistant.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiAssistant.SetAssistantId(const Value: String);
begin
  FAssistantId := Value;
end;

procedure TAiAssistant.SetCode_Interpreter(const Value: Boolean);
begin
  FCode_Interpreter := Value;
end;

procedure TAiAssistant.SetFilesIds(const Value: TStringList);
begin
  FFilesIds := Value;
end;

procedure TAiAssistant.SetFunciones(const Value: TAiToolsFunctions);
begin
  FFunciones := Value;
end;

procedure TAiAssistant.SetInstructions(const Value: String);
begin
  FInstructions := Value;
end;

procedure TAiAssistant.SetJSonObject(const Value: TJsonObject);
begin
  FJSonObject := Value;
end;

procedure TAiAssistant.SetMetadata(const Value: TAiMetadata);
begin
  FMetadata := Value;
end;

procedure TAiAssistant.SetModel(const Value: String);
begin
  FModel := Value;
end;

procedure TAiAssistant.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TAiAssistant.SetOnBeforeResponse(const Value: TOnBeforeResponse);
begin
  FOnBeforeResponse := Value;
end;

procedure TAiAssistant.SetRetrieval(const Value: Boolean);
begin
  FRetrieval := Value;
end;

{ TAiThRead }

constructor TAiThRead.Create(aAiAssistant: TAiAssistant);
begin
  Inherited Create;
  FAiAssistant := aAiAssistant;
  FMessages := TAiMessages.Create;
  FMetadata := TAiMetadata.Create;
end;

function TAiThRead.AddMessage(aMessage: String; aRole: String = ''; aFiles_ids: String = ''): TAiMessage;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJsonObject;
  JFiles: TJSonArray;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  i: Integer;
  ListFilesIds: TStringList;
  AiMessage: TAiMessage;
  JRes: TJsonObject;
begin

  If ThReadId = '' then
    Raise Exception.Create('Debe crear primero un thread');

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  ListFilesIds := TStringList.Create;

  If aRole = '' then
    aRole := 'user';

  sUrl := 'https://api.openai.com/v1/threads/' + ThReadId + '/messages';
  JObj := TJsonObject.Create;

  Try
    JObj.AddPair('role', aRole);
    JObj.AddPair('content', aMessage);

    If (aFiles_ids <> '') then
    Begin
      ListFilesIds.CommaText := aFiles_ids;
      JFiles := TJSonArray.Create;

      For i := 0 to ListFilesIds.Count - 1 do
        JFiles.Add(ListFilesIds[i]);

      JObj.AddPair('file_ids', JFiles);
    End;

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.AddMessage', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      AiMessage := ParseMessage(JRes);
      FMessages.Insert(0, AiMessage);
      Result := AiMessage;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
    ListFilesIds.Free;
  End;
end;

function TAiThRead.ApplyUpdates: Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJsonObject;
  jMetadata: TJsonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  Clave: String;
begin

  If Self.FThReadId = '' then
    Raise Exception.Create('Debe crear primero el hilo');

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/threads/' + ThReadId;
  JObj := TJsonObject.Create;

  Try
    If Assigned(FMetadata) and (FMetadata.Count > 0) then
    Begin
      jMetadata := TJsonObject.Create;

      For Clave in Metadata.Keys do
        jMetadata.AddPair(Clave, Metadata.Items[Clave]);

      JObj.AddPair('metadata', jMetadata);
    End;

    St.WriteString(JObj.Format);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.ApplyUpdates', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      FJSonObject := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      ParseThReadJson(FJSonObject);
      Result := True;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
  End;
end;

function TAiThRead.CreateNewThRead(aMensajes: TAiMessages; aMetadata: TAiMetadata): Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJsonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin

  FMessages.Clear;
  FMetadata.Clear;

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/threads';
  JObj := TJsonObject.Create;

  Try
    If Assigned(aMensajes) and (aMensajes.Count > 0) then
      JObj.AddPair('messages', aMensajes.ToJSon);

    If Assigned(aMetadata) and (aMetadata.Count > 0) then
      JObj.AddPair('metadata', aMetadata.ToJSon);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.CreateNewThRead', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      FJSonObject := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      ParseThReadJson(FJSonObject);
      Result := True;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
  End;
end;

destructor TAiThRead.Destroy;
begin
  FMessages.Free;
  FMetadata.Free;

  inherited;
end;

function TAiThRead.GetMessage(aMessageId: String): TAiMessage;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: String;
  JRes: TJsonObject;
begin
  Client := THTTPClient.Create;
  sUrl := 'https://api.openai.com/v1/threads/' + ThReadId + '/messages/' + aMessageId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.GetMessage', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      Result := ParseMessage(JRes);
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
  End;
end;

function TAiThRead.GetMessageFile(aThReadId, aMessageId, aFileId: String): TJsonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: String;
  Response: TMemoryStream;
begin

  If aMessageId = '' then
    Raise Exception.Create('Debe ser un MessageId válido');

  If aFileId = '' then
    Raise Exception.Create('Debe ser un FileId válido');

  Client := THTTPClient.Create;
  Response := TMemoryStream.Create;
  sUrl := 'https://api.openai.com/v1/threads/' + ThReadId + '/messages/' + aMessageId + '/files/' + aFileId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.GetMessageFile', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      Result := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    Response.Free;
  End;
end;

function TAiThRead.ListMessageFiles(aMsgId: String): TJsonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  JRes: TJsonObject;
  sUrl: String;
begin
  If ThReadId = '' then
    Raise Exception.Create('debe crear primero un thread');

  Client := THTTPClient.Create;
  sUrl := 'https://api.openai.com/v1/threads/' + Self.ThReadId + '/messages/' + aMsgId + '/files';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.ListMessages', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      Result := JRes;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
  End;
end;

function TAiThRead.ListMessages(Limit: Integer; Order: String): TAiMessages;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj, JRes: TJsonObject;
  JVal: TJSonValue;
  Res: IHTTPResponse;
  sUrl: String;
  Data: TJSonArray;
  AiMessages: TAiMessages;
  AiMessage: TAiMessage;
begin
  If ThReadId = '' then
    Raise Exception.Create('debe crear primero un thread');

  Client := THTTPClient.Create;
  sUrl := 'https://api.openai.com/v1/threads/' + ThReadId + '/messages?limit=' + Limit.ToString + '&order=' + Order;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.ListMessages', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));

      AiMessages := TAiMessages.Create;

      Data := JRes.GetValue<TJSonArray>('data');
      For JVal in Data do
      Begin
        JObj := TJsonObject(JVal);
        AiMessage := ParseMessage(JObj);
        // AiMessages.Insert(0, AiMessage);
        AiMessages.Add(AiMessage);
      End;

      Result := AiMessages;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
  End;
end;

function TAiThRead.LoadThRead(ThReadId: String): Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: String;
begin
  FMessages := TAiMessages.Create;
  FMetadata := TAiMetadata.Create;

  Client := THTTPClient.Create;
  sUrl := 'https://api.openai.com/v1/threads/' + ThReadId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.LoadThRead', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      FJSonObject := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      ParseThReadJson(FJSonObject);
      Result := True;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
  End;
end;

function TAiThRead.MessageApplyUpdate(aMessage: TAiMessage): TJsonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJsonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/threads/' + ThReadId + '/messages/' + aMessage.MessageId;
  JObj := TJsonObject.Create;

  Try

    If Assigned(aMessage.Metadata) and (aMessage.Metadata.Count > 0) then
      JObj.AddPair('metadata', aMessage.Metadata.ToJSon);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.MessageApplyUpdate', Res.ContentAsString);
      end);

    Result := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
  End;
end;

procedure TAiThRead.ParseThReadJson(JObj: TJsonObject);
Var
  sMeta: String;
begin
  Self.FThReadId := JObj.GetValue<String>('id');
  sMeta := JObj.GetValue<TJsonObject>('metadata').Format;
  FMetadata.SetJsonText(sMeta);
end;

function TAiThRead.ParseMessage(JObj: TJsonObject): TAiMessage;
Var
  JObj1, jText, JObjAnn: TJsonObject;
  JContent: TJSonArray;
  JFiles, JAnnotations: TJSonArray;
  JVal, JVal1: TJSonValue;
  AiFile: TAiFile;
  AiFiles: TAiFiles;
  MessageId, ThReadId, FileId, Value: String;
  filename: String;
  i, J: Integer;
begin
  Result := TAiMessage.Create(FAiAssistant.ApiKey);
  AiFiles := TAiFiles.Create(FAiAssistant.ApiKey);

  Try
    MessageId := JObj.GetValue<String>('id');
    ThReadId := JObj.GetValue<String>('thread_id');

    Result.FMessageId := MessageId;
    Result.ThReadId := ThReadId;
    Result.role := JObj.GetValue<String>('role');
    Result.FAssisistantId := JObj.GetValue<String>('assistant_id');
    Result.RunId := JObj.GetValue<String>('run_id');

    JFiles := JObj.GetValue<TJSonArray>('file_ids');
    For JVal in JFiles do
    Begin
      FileId := JVal.Value;

      Result.Ffile_ids.Add(FileId);

      If Result.Files.GetFileById(FileId, AiFile) = False then
      Begin
        AiFile := TAiFile.Create(FAiAssistant.ApiKey);
        AiFile.id := FileId;
        // GetMessageFile(ThReadId, MessageId, JVal.Value);
        // Aquí debe recuperar la información del archivo
        Result.Files.Add(FileId, AiFile);
      End;

    End;

    Metadata.JsonText := JObj.GetValue<TJsonObject>('metadata').Format;

    JContent := JObj.GetValue<TJSonArray>('content');

    SetLength(Result.FContent, JContent.Count);

    i := 0;
    For JVal in JContent do
    Begin
      JObj1 := TJsonObject(JVal);

      If JObj1.GetValue<String>('type') = 'text' then
      Begin
        jText := JObj1.GetValue<TJsonObject>('text');
        Value := jText.GetValue<String>('value');

        Result.FContent[i].Tipo := 'text';
        Result.FContent[i].Text := Value;

        JAnnotations := jText.GetValue<TJSonArray>('annotations');
        SetLength(Result.FContent[i].Anotations, JAnnotations.Count);
        J := 0;

        For JVal1 in JAnnotations do
        Begin
          JObjAnn := TJsonObject(JVal1);
          If JObjAnn.GetValue<String>('type') = 'file_path' then
          Begin
            filename := JObjAnn.GetValue<String>('text');

{$IFDEF MSWINDOWS}
            filename := StringReplace(filename, '/', '\', [rfReplaceAll]);
{$ENDIF}
            filename := ExtractFileName(filename);

            FileId := JObjAnn.GetValue<TJsonObject>('file_path').GetValue<String>('file_id');

            Result.FContent[i].Anotations[J].Tipo := 'file_path';
            Result.FContent[i].Anotations[J].Text := JObjAnn.GetValue<String>('text');
            Result.FContent[i].Anotations[J].filename := filename;
            Result.FContent[i].Anotations[J].FileType := ExtractFileExt(filename);
            Result.FContent[i].Anotations[J].FileId := FileId;

            If Result.Files.GetFileById(FileId, AiFile) = False then
            Begin
              AiFile := TAiFile.Create(FAiAssistant.ApiKey);
              AiFile.id := FileId;
              Result.Files.Add(FileId, AiFile);
            End;

            AiFile.Fid := FileId;
            AiFile.FFileType := Result.FContent[i].Anotations[J].FileType;
            AiFile.Ffilename := filename;
          End;
          Inc(J);
        End;
        Inc(i);
      End;

      If JObj1.GetValue<String>('type') = 'image_file' then
      Begin
        FileId := JObj1.GetValue<TJsonObject>('image_file').GetValue<String>('file_id');

        If Result.Files.GetFileById(FileId, AiFile) = False then
        Begin
          AiFile := TAiFile.Create(FAiAssistant.ApiKey);
          AiFile.id := FileId;
          Result.Files.Add(FileId, AiFile);
        End;

        AiFile.Fid := FileId;
        AiFile.FFileType := '.png';
        AiFile.Ffilename := TPath.GetTempFileName + '.png';

        // GetMessageFile(ThReadId, MessageId, FileId);
        // AiFile := AiFiles.GetFile(FileId);
        // AiFile.Content.Position := 0;
        // AiFile.Content.SaveToFile('c:\temp\prueba.png');
      End;
    End;
  Finally
    AiFiles.Free;
  End;
end;

function TAiThRead.Remove: Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: String;
begin
  If ThReadId = '' then
    Exit;

  Client := THTTPClient.Create;
  sUrl := 'https://api.openai.com/v1/threads/' + ThReadId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAiAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Delete(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAiAssistant) then
          FAiAssistant.DoBeforeResponse(Self, 'TAiThRead.Remove', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      // Result := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      Result := True;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;
  Finally
    Client.Free;
  End;
end;

procedure TAiThRead.SetAiAssistant(const Value: TAiAssistant);
begin
  FAiAssistant := Value;
end;

procedure TAiThRead.SetJSonObject(const Value: TJsonObject);
begin
  FJSonObject := Value;
end;

procedure TAiThRead.SetMessages(const Value: TAiMessages);
begin
  FMessages := Value;
end;

procedure TAiThRead.SetMetadata(const Value: TAiMetadata);
begin
  FMetadata := Value;
end;

procedure TAiThRead.SetThReadId(const Value: String);
begin
  FThReadId := Value;
end;

{ TAiRun }

function TAiRun.ApplyUpdates: TJsonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJsonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  JRes: TJsonObject;
begin
  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/threads/' + ThRead.ThReadId + '/runs/' + RunId;
  JObj := TJsonObject.Create;

  Try

    If Assigned(Metadata) and (Metadata.Count > 0) then
      JObj.AddPair('metadata', Metadata.ToJSon);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAssistant) then
          FAssistant.DoBeforeResponse(Self, 'TAiRun.ApplyUpdates', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      ParseRun(JRes);
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
  End;
end;

function TAiRun.Cancel: TJsonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  JRes: TJsonObject;
begin
  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/threads/' + ThRead.ThReadId + '/runs/' + RunId + '/cancel';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAssistant) then
          FAssistant.DoBeforeResponse(Self, 'TAiRun.Cancel', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      ParseRun(JRes);
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
  End;
end;

constructor TAiRun.Create(aAssistant: TAiAssistant; aThRead: TAiThRead; aAditionalInstructions: String; aMetadata: TAiMetadata);
begin
  Inherited Create;

  If Not Assigned(aAssistant) then
    Raise Exception.Create('Se requiere el AssistantId');

  If Not Assigned(aThRead) then
    Raise Exception.Create('Se requiere el ThRead');

  FAssistant := aAssistant;
  ToolsCalls := TAiToolsFunctions.Create;

  FMetadata := TAiMetadata.Create;

  If Assigned(aMetadata) then
    FMetadata.AsText := aMetadata.AsText;

  If Assigned(aThRead) then
    FThRead := aThRead
  Else
  Begin
    FThRead := TAiThRead.Create(FAssistant);
    FThRead.CreateNewThRead(Nil, Nil);
  End;
end;

destructor TAiRun.Destroy;
begin
  FMetadata.Free;
  FToolsCalls.Free;
  FAssistant := Nil;
  FThRead := Nil;
  inherited;
end;

Function TAiRun.ExtractToolCallsFromJson(JSonObject: TJsonObject): Boolean;
var
  ToolCallsArray: TJSonArray;
  ToolCallObject: TJsonObject;
  ToolCall: TAiToolsFunction;
  JSubmitTools: TJsonObject;
  JVal: TJSonValue;
  Tipo: String;
begin
  FToolsCalls.Clear;

  try

    Tipo := JSonObject.GetValue<String>('type');

    If Tipo = 'submit_tool_outputs' then
    Begin
      If JSonObject.TryGetValue<TJsonObject>('submit_tool_outputs', JSubmitTools) then
      Begin

        ToolCallsArray := JSubmitTools.GetValue<TJSonArray>('tool_calls');

        for JVal in ToolCallsArray do
        begin
          ToolCallObject := TJsonObject(JVal);

          ToolCall := TAiToolsFunction.Create;
          ToolCall.id := ToolCallObject.GetValue('id').Value;
          ToolCall.Tipo := ToolCallObject.GetValue('type').Value;
          ToolCall.Name := ToolCallObject.GetValue<TJsonObject>('function').GetValue('name').Value;
          ToolCall.Arguments := ToolCallObject.GetValue<TJsonObject>('function').GetValue('arguments').Value;
          ToolCall.&Function := ToolCallObject.GetValue<TJsonObject>('function').Value;
          FToolsCalls.Add(ToolCall.id, ToolCall);
        end;
      End;
    End;
  finally
    JSonObject.Free;
  end;

  Result := FToolsCalls.Count > 0;

end;

procedure TAiRun.InternalCallToolFunction(AiToolCall: TAiToolsFunction);
begin
  // AiToolCall.Response := 'no tengo ninguna respuesta para esta función "' + AiToolCall.Name;
  // AiToolCall.Response := 'La temperatura es de 26 grados centígrados '
end;

function TAiRun.ListRuns: TJsonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  sUrl := 'https://api.openai.com/v1/threads/' + ThRead.ThReadId + '/runs';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAssistant) then
          FAssistant.DoBeforeResponse(Self, 'TAiRun.ListRuns', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      Result := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
  End;
end;

function TAiRun.ListRunsSteps: TJsonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  sUrl := 'https://api.openai.com/v1/threads/' + ThRead.ThReadId + '/runs/' + Self.RunId + '/steps';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAssistant) then
          FAssistant.DoBeforeResponse(Self, 'TAiRun.ListRunsSteps', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      Result := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
  End;
end;

procedure TAiRun.ParseRun(JObj: TJsonObject);
var
  TaskList: array of ITask;
  i, NumTasks: Integer;

  JTool: TJsonObject;
  sRequiredAction: String;
  ToolCall: TAiToolsFunction;
  Clave: String;

begin

  Self.FRunId := JObj.GetValue<String>('id');
  FLastError := JObj.GetValue<String>('last_error');
  Fstatus := JObj.GetValue<String>('status');

  // queued succeeded cancelled running, failed uploaded, processed requires_action

  FBusy := (Fstatus <> 'completed') and (Fstatus <> 'succeeded') and (Fstatus <> 'cancelled') and (Fstatus <> 'failed');

  JTool := Nil;
  Try
    JObj.TryGetValue<TJsonObject>('required_action', JTool);
  Except
  End;

  If Assigned(JTool) then
  Begin
    // JTool := JObj.GetValue<TJsonObject>('required_action');

    FToolsCalls.Clear;

    If ExtractToolCallsFromJson(JTool) = True then
    Begin

      NumTasks := FToolsCalls.Count;
      SetLength(TaskList, NumTasks);
      // Ajusta el tamaño del array para el número de tareas

      i := 0;
      For Clave in FToolsCalls.Keys do
      Begin
        ToolCall := FToolsCalls[Clave];

        TaskList[i] := TTask.Create(
          procedure
          begin
            If Assigned(FOnCallToolFunction) then
              FOnCallToolFunction(Self, ToolCall)
            Else
              InternalCallToolFunction(ToolCall);
          end);
        TaskList[i].Start;
        Inc(i);

      End;
      TTask.WaitForAll(TaskList);
      SubmitTool(Self.FToolsCalls);
    End;
  end;
end;

function TAiRun.Retrieve: TJsonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: String;
  JRes: TJsonObject;
begin
  Client := THTTPClient.Create;
  sUrl := 'https://api.openai.com/v1/threads/' + FThRead.ThReadId + '/runs/' + RunId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAssistant) then
          FAssistant.DoBeforeResponse(Self, 'TAiRun.Retrieve', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      FJObjectRun := JRes;
      ParseRun(JRes);
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
  End;
end;

function TAiRun.RetrieveStep(StepId: String): TJsonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  sUrl := 'https://api.openai.com/v1/threads/' + ThRead.ThReadId + '/runs/' + RunId + '/steps/' + StepId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAssistant) then
          FAssistant.DoBeforeResponse(Self, 'TAiRun.RetrieveStep', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      Result := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
  End;
end;

function TAiRun.Run: Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JRes, JObj: TJsonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin
  If Not Assigned(FAssistant) then
    Raise Exception.Create('Se requiere el AssistantId');

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/threads/' + FThRead.ThReadId + '/runs';
  JObj := TJsonObject.Create;

  Try

    JObj.AddPair('assistant_id', FAssistant.AssistantId);

    // If FAditionalInstructions <> '' then
    // JObj.AddPair('additional_instructions', FAditionalInstructions);

    If Assigned(FMetadata) and (FMetadata.Count > 0) then
      JObj.AddPair('metadata', FMetadata.ToJSon);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAssistant) then
          FAssistant.DoBeforeResponse(Self, 'TAiRun.Run', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      ParseRun(JRes);
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
  End;
end;

procedure TAiRun.SetOnCallToolFunction(const Value: TOnCallToolFunction);
begin
  FOnCallToolFunction := Value;
end;

procedure TAiRun.SetJObjectRun(const Value: TJsonObject);
begin
  FJObjectRun := Value;
end;

procedure TAiRun.SetMetadata(const Value: TAiMetadata);
begin
  FMetadata := Value;
end;

procedure TAiRun.SetToolsCalls(const Value: TAiToolsFunctions);
begin
  FToolsCalls := Value;
end;

function TAiRun.SubmitTool(AitoolsOutputs: TAiToolsFunctions): TJsonObject;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJsonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
  JRes: TJsonObject;
begin
  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/threads/' + ThRead.ThReadId + '/runs/' + RunId + '/submit_tool_outputs';
  JObj := TJsonObject.Create;

  Try

    If Assigned(AitoolsOutputs) and (AitoolsOutputs.Count > 0) then
      JObj.AddPair('tool_outputs', AitoolsOutputs.ToOutputJSon);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FAssistant.ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    TThread.Synchronize(nil,
      procedure
      begin
        If Assigned(FAssistant) then
          FAssistant.DoBeforeResponse(Self, 'SubmitTool', Res.ContentAsString);
      end);

    if Res.StatusCode = 200 then
    Begin
      JRes := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      ParseRun(JRes);
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
  End;
end;

{ TAiToolFunction }

procedure TAiToolsFunction.Assign(aSource: TAiToolsFunction);
begin
  Self.id := aSource.id;
  Self.Tipo := aSource.Tipo;
  Self.Name := aSource.Name;
  Self.Description := aSource.Description;
  Self.Arguments := aSource.Arguments;
  Self.&Function := aSource.&Function;
  Self.Response := aSource.Response;
  Self.Body := aSource.Body;
  Metadata.JsonText := aSource.Metadata.JsonText;
end;

constructor TAiToolsFunction.Create;
begin
  inherited;
  Metadata := TAiMetadata.Create;
end;

destructor TAiToolsFunction.Destroy;
begin
  Metadata.Free;
  inherited;
end;

procedure TAiToolsFunction.ParseFunction(JObj: TJsonObject);
Var
  JFunc: TJsonObject;
  FunName: String;
begin
  JFunc := JObj.GetValue<TJsonObject>('function');
  FunName := JFunc.GetValue<string>('name');

  Begin
    Name := JFunc.GetValue<String>('name');
    Self.Description := JFunc.GetValue<String>('description');
    &Function := JFunc.Format;
    Body := JObj; // La funcion original completa
  End;
end;
{$ENDREGION}
{ TAiFiles }

Constructor TAiFiles.Create(aApiKey: String);
begin
  inherited Create;
  FFileList := TAiFilesArray.Create;
  FApiKey := aApiKey;
end;

Destructor TAiFiles.Destroy;
begin
  FFileList.Free;
  inherited;
end;

function TAiFiles.DeleteFile(aFileId: String): Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/files/' + aFileId;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Delete(sUrl, Response, Headers);
    if Res.StatusCode = 200 then
    Begin
      // TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      Result := True;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    Response.Free;
  End;
end;

function TAiFiles.GetFile(aFileId: String): TAiFile;
begin
  If FFileList.GetFileById(aFileId, Result) = False then
  Begin
    Result := TAiFile.Create(FApiKey);
    Result.id := aFileId;
  End;

  Result.GetContent;

  { Client := THTTPClient.Create;
    sUrl := 'https://api.openai.com/v1/files/' + aFileId + '/content';
    Response := TMemoryStream.Create;

    Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
    Response.Position := 0;
    Result := TAiFile.Create(Self.ApiKey);
    Result.Content.LoadFromStream(Response);
    Result.Content.Position := 0;
    End
    else
    Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);

    Finally
    Client.Free;
    Response.Free;
    End;
  }

end;

function TAiFiles.GetFileByName(aFileName: String): TAiFile;
Var
  AiFile: TAiFile;
begin
  ListFiles; // Actualiza los archivos
  If FFileList.GetFileByName(aFileName, AiFile) = False then
    Raise Exception.Create('No se encuentra el archivo ' + aFileName);

  Result := Self.GetFile(AiFile.id);
end;

function TAiFiles.GetFileList: TAiFilesArray;
begin
  ListFiles;
  Result := FFileList;
end;

function TAiFiles.ListFiles: TAiFilesArray;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJsonObject;
  JArr: TJSonArray;
  JVal: TJSonValue;

  Res: IHTTPResponse;
  sUrl: String;
  AiFile: TAiFile;
begin
  FFileList.Clear;

  Client := THTTPClient.Create;
  sUrl := 'https://api.openai.com/v1/files';

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Nil, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      JArr := JObj.GetValue<TJSonArray>('data');

      For JVal in JArr do
      Begin
        AiFile := TJson.JsonToObject<TAiFile>(TJsonObject(JVal));
        FFileList.Add(AiFile.id, AiFile);
      End;
      Result := FFileList;
    End
    else
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);

  Finally
    Client.Free;
  End;
end;

procedure TAiFiles.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

function TAiFiles.UploadFile(filename: String; IsForAssistant: Boolean): TAiFile;
Var
  Body: TMultipartFormData;
  Client: THTTPClient;
  sUrl: String;
  Res: IHTTPResponse;
  Stream: TFileStream;
  JObj: TJsonObject;
begin
  sUrl := 'https://api.openai.com/v1/files';

  Client := THTTPClient.Create;
  Try
    Body := TMultipartFormData.Create;
    Try
      Stream := TFileStream.Create(filename, fmOpenRead);
      Try
        Body.AddStream('file', Stream, ExtractFileName(filename));
        // Asumir que 'mydata.jsonl' es application/json

        If IsForAssistant then
          Body.AddField('purpose', 'assistants')
        Else
          Body.AddField('purpose', 'fine-tune');

        // Añadir el header de autorización
        Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;

        // La llamada Post debería cuidar del Content-Type automáticamente
        Res := Client.Post(sUrl, Body, nil);

        if Res.StatusCode = 200 then
        Begin
          JObj := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
          Result := TJson.JsonToObject<TAiFile>(JObj);
        End
        else
          Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      Finally
        Stream.Free;
      End;
    Finally
      Body.Free;
    End;
  Finally
    Client.Free;
  End;
end;

{ TAiFilesArray }

function TAiFilesArray.GetFileById(AiFileId: String; Out AiFile: TAiFile): Boolean;
Var
  Clave: String;
begin
  For Clave in Self.Keys do
  Begin
    If SameText(Self.Items[Clave].id, AiFileId) = True then
    Begin
      Result := True;
      AiFile := Self.Items[Clave];
      Break;
    End;
  End;
end;

function TAiFilesArray.GetFileByName(aFileName: String; Out AiFile: TAiFile): Boolean;
begin
  Result := Self.TryGetValue(aFileName, AiFile);
end;

{ TAiAudio }

procedure RunCommand(const Command: string);
begin

{$IFDEF LINUX}
  TLinuxUtils.RunCommandLine(Command);
{$ENDIF}
{$IFDEF MSWINDOWS}
  ShellExecute(0, nil, 'cmd.exe', PChar('/C ' + Command), nil, SW_HIDE);
{$ENDIF}
end;

function TAiAudio.ConvertFileFormat(Origen: TMemoryStream; filename: String; out Destino: TMemoryStream; out DestinoFileName: String): Boolean;
Var
  FOrigen, FDestino: String;
  CommandLine: String;
begin
  filename := LowerCase(filename);
  FDestino := ChangeFileExt(filename, '.mp3');

  FOrigen := System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetTempPath, filename);
  FDestino := System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetTempPath, FDestino);

  Origen.Position := 0;
  Origen.SaveToFile(FOrigen);

  CommandLine := 'ffmpeg -i ' + FOrigen + ' ' + FDestino;

  RunCommand(CommandLine);

  Destino := TMemoryStream.Create;
  Destino.LoadFromFile(FDestino);
  Destino.Position := 0;
  DestinoFileName := ExtractFileName(FDestino);

  TFile.Delete(FOrigen);
  TFile.Delete(FDestino);

end;

constructor TAiAudio.Create(aApiKey: String);
begin
  Inherited Create;
  FApiKey := aApiKey;
end;

destructor TAiAudio.Destroy;
begin

  inherited;
end;

function TAiAudio.IsValidExtension(FileExtension: String): Boolean;
begin
  FileExtension := LowerCase(Trim(StringReplace(FileExtension, '.', '', [rfReplaceAll])));
  Result := (FileExtension = 'mp3') or (FileExtension = 'mp4') or (FileExtension = 'mpeg') or (FileExtension = 'mpga') or (FileExtension = 'm4a') or (FileExtension = 'ogg') or (FileExtension = 'wav') or (FileExtension = 'webm');
end;

procedure TAiAudio.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

function TAiAudio.Speech(Text: String; Quality: Boolean = True; Voice: String = 'nova'; Format: String = 'mp3'; Speed: Single = 1): TMemoryStream;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJsonObject;
  Res: IHTTPResponse;
  Response: TMemoryStream;
  St: TStringStream;
  sUrl: String;
begin

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TMemoryStream.Create;
  sUrl := 'https://api.openai.com/v1/audio/speech';
  JObj := TJsonObject.Create;

  Speed := Trunc(Speed * 10) / 10;

  Try
    If Quality then
      JObj.AddPair('model', 'tts-1-hd')
    Else
      JObj.AddPair('model', 'tts-1');

    JObj.AddPair('input', Text);
    JObj.AddPair('voice', Voice); // alloy, echo, fable, onyx, nova, and shimmer
    JObj.AddPair('response_format', Format);
    JObj.AddPair('speed', Speed);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      Response.Position := 0;
      Result := Response;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    // Response.Free;  //No se libera, se pasa al usuario
    JObj.Free;
  End;
end;

function TAiAudio.Transcription(Stream: TMemoryStream; filename, Prompt, Languaje, ResponseFormat, Model, Temperature: String): String;
Var
  Body: TMultipartFormData;
  Client: THTTPClient;
  Headers: TNetHeaders;
  sUrl, FileNameDestino: String;
  Destino: TMemoryStream;
  Res: IHTTPResponse;
begin

  If IsValidExtension(ExtractFileExt(filename)) = False then
  Begin
    ConvertFileFormat(Stream, filename, Destino, FileNameDestino);
    Stream.Clear;
    Stream.LoadFromStream(Destino);
    Stream.Position := 0;
    filename := FileNameDestino;
    Destino.Free;
  End;

  sUrl := 'https://api.openai.com/v1/audio/transcriptions';

  Client := THTTPClient.Create;
  Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
  Client.ContentType := 'application/json';
  Body := TMultipartFormData.Create;

  Try
    Stream.Position := 0;

    Body.AddStream('file', Stream, filename, GetContentType(ExtractFileExt(filename)));
    Body.AddField('model', Model);
    Body.AddField('prompt', Prompt);
    Body.AddField('response_format', ResponseFormat);
    // json, text, srt, verbose_json, or vtt
    Body.AddField('temperature', Temperature);
    Body.AddField('language', Languaje);

    Client.Accept := 'application/text';
    Client.ContentType := 'multipart/form-data';

    Res := Client.Post(sUrl, Body, Nil, Headers);

    if Res.StatusCode = 200 then
    Begin
      Result := Res.ContentAsString
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Body.Free;
    Client.Free;
  End;
end;

function TAiAudio.Translation(Stream: TMemoryStream; filename, Prompt, ResponseFormat, Model, Temperature: String): String;
Var
  Body: TMultipartFormData;
  Client: THTTPClient;
  Headers: TNetHeaders;
  sUrl, FileNameDestino: String;
  Destino: TMemoryStream;
  Res: IHTTPResponse;
begin
  If IsValidExtension(ExtractFileExt(filename)) = False then
  Begin
    ConvertFileFormat(Stream, filename, Destino, FileNameDestino);
    Stream.Clear;
    Stream.LoadFromStream(Destino);
    Stream.Position := 0;
    filename := FileNameDestino;
    Destino.Free;
  End;

  sUrl := 'https://api.openai.com/v1/audio/translations';

  Client := THTTPClient.Create;
  Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
  Client.ContentType := 'application/json';
  Body := TMultipartFormData.Create;

  Try
    Stream.Position := 0;

    Body.AddStream('file', Stream, filename, GetContentType(ExtractFileExt(filename)));
    Body.AddField('model', Model);
    Body.AddField('prompt', Prompt);
    Body.AddField('response_format', ResponseFormat);
    // json, text, srt, verbose_json, or vtt
    Body.AddField('temperature', Temperature);

    Client.Accept := 'application/text';
    Client.ContentType := 'multipart/form-data';

    Res := Client.Post(sUrl, Body, Nil, Headers);

    if Res.StatusCode = 200 then
    Begin
      Result := Res.ContentAsString
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Body.Free;
    Client.Free;
  End;
end;

{ TEmbeddings }

class function TAiEmbeddings.CosineSimilarity(const A, B: TEmbedding): Double;
var
  MagA, MagB: Double;
begin
  MagA := Magnitude(A);
  MagB := Magnitude(B);
  if (MagA = 0) or (MagB = 0) then
    Result := 0 // Para evitar división por cero
  else
    Result := DotProduct(A, B) / (MagA * MagB);
end;

constructor TAiEmbeddings.Create(aApiKey: String);
begin
  Inherited Create;
  FApiKey := aApiKey;
end;

destructor TAiEmbeddings.Destroy;
begin

  inherited;
end;

class function TAiEmbeddings.DotProduct(const A, B: TEmbedding): Double;
var
  i: Integer;
begin
  Result := 0.0;
  for i := Low(A) to High(A) do
    Result := Result + A[i] * B[i];
end;

class function TAiEmbeddings.Magnitude(const V: TEmbedding): Double;
var
  Sum: Double;
  i: Integer;
begin
  Sum := 0.0;
  for i := Low(V) to High(V) do
    Sum := Sum + V[i] * V[i];
  Result := Sqrt(Sum);
end;

procedure TAiEmbeddings.ParseEmbedding(JObj: TJsonObject);
Var
  JArr, jData: TJSonArray;
  Emb: TEmbedding;
  JVal: TJSonValue;
  i, J: Integer;

begin

  FModel := JObj.GetValue<String>('model');
  Fprompt_tokens := JObj.GetValue<TJsonObject>('usage').GetValue<Integer>('prompt_tokens');
  Ftotal_tokens := JObj.GetValue<TJsonObject>('usage').GetValue<Integer>('total_tokens');

  jData := JObj.GetValue<TJSonArray>('data');

  SetLength(FData, jData.Count);

  i := 0;
  For JVal in jData do
  Begin
    JArr := TJsonObject(JVal).GetValue<TJSonArray>('embedding');
    FillChar(Emb, Length(Emb) * SizeOf(Double), 0);

    For J := 0 to JArr.Count - 1 do
      Emb[J] := JArr.Items[J].GetValue<Double>;

    FData[i] := Emb;
    Inc(i);
  End;
end;

procedure TAiEmbeddings.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiEmbeddings.SetData(const Value: TEmbeddingArray);
begin
  FData := Value;
end;

procedure TAiEmbeddings.SetModel(const Value: String);
begin
  FModel := Value;
end;

class function TAiEmbeddings.ToJsonArray(Val: TEmbedding): TJSonArray;
Var
  i: Integer;
begin
  Result := TJSonArray.Create;

  For i := 0 to Length(Val) - 1 do
    Result.Add(Val[i]);
end;

function TAiEmbeddings.ToJsonArray: TJSonArray;
Var
  i, J: Integer;
  jData, JEmb: TJSonArray;
  Emb: TEmbedding;
begin
  jData := TJSonArray.Create;

  Try
    For i := 0 to Length(FData) - 1 do
    Begin
      Emb := FData[i];
      JEmb := TJSonArray.Create;

      For J := 0 to Length(Emb) - 1 do
        JEmb.Add(Emb[J]);

      jData.Add(JEmb);
    End;

    Result := jData;
  Finally

  End;
end;

function TAiEmbeddings.CreateEmbedding(Input, User: String; Dimensions: Integer; Model, EncodingFormat: String): TEmbeddingArray;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJsonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/embeddings';
  JObj := TJsonObject.Create;

  Try
    JObj.AddPair('input', Input);
    JObj.AddPair('model', Model);
    JObj.AddPair('user', User);
    JObj.AddPair('dimensions', Dimensions);
    JObj.AddPair('encoding_format', EncodingFormat);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      ParseEmbedding(JObj);
      Result := Self.FData;

    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
  End;
end;

{ TAiImages }

constructor TAiImages.Create(aApiKey: String);
begin
  Inherited Create;
  FApiKey := aApiKey;
  SetLength(FImages, 0);
end;

destructor TAiImages.Destroy;
Var
  i: Integer;
begin
  For i := 0 to Length(FImages) - 1 do
    FImages[i].Free;

  SetLength(FImages, 0);
  inherited;
end;

function TAiImages.Edit(aImage, aMask: TMemoryStream; aPrompt, aUser: String; aSize: TAiImageSize; N: Integer; ResponseFormat: TAiImageResponseFormat): TAiImagesFile;
Var
  Body: TMultipartFormData;
  Client: THTTPClient;
  sUrl: String;
  Res: IHTTPResponse;
  JObj: TJsonObject;
begin
  sUrl := 'https://api.openai.com/v1/images/edits';

  aImage.Position := 0;

  If Assigned(aMask) then
    aMask.Position := 0;

  Client := THTTPClient.Create;
  Try
    Body := TMultipartFormData.Create;
    Try
      Try
        Body.AddStream('image', aImage, ExtractFileName('origen.png'));

        If Assigned(aMask) then
          Body.AddStream('mask', aImage, True, ExtractFileName('mask.png'));

        Body.AddField('prompt', aPrompt);
        Body.AddField('user', aUser);
        Body.AddField('model', 'dall-e-2'); // Solo acepta esta versión
        Body.AddField('n', N.ToString);

        Case aSize of
          TiaSize256:
            Body.AddField('size', '256x256');
          TiaSize512:
            Body.AddField('size', '512x512');
          TiaSize1024:
            Body.AddField('size', '1024x1024');
          TiaSize1024_1792:
            Body.AddField('size', '1024x1024'); // '1024x1792');
          TiaSize1792_1024:
            Body.AddField('size', '1024x1024'); // '1792x1024');
        End;

        If ResponseFormat = TAiImageResponseFormat.tiaRUrl then
          Body.AddField('response_format', 'url')
        else
          Body.AddField('response_format', 'b64_json');

        // Añadir el header de autorización
        Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;

        // La llamada Post debería cuidar del Content-Type automáticamente
        Res := Client.Post(sUrl, Body, nil);

        if Res.StatusCode = 200 then
        Begin
          JObj := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
          ParseGenerate(JObj);
          Result := FImages[0];
        End
        else
          Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      Finally
      End;
    Finally
      Body.Free;
    End;
  Finally
    Client.Free;
  End;
end;

function TAiImages.Generate(aPrompt, aUser: String; aSize: TAiImageSize; N: Integer; ResponseFormat: TAiImageResponseFormat; HdQuality: Boolean; Style: TAiImageAStyleFormat; UseDalle3: Boolean): TAiImagesFile;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj: TJsonObject;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin
  // prompt           string Required
  // model            string Optional dall-e-2 dall-e-3
  // n                integer or null Optional Defaults to 1 The number of images to generate. Must be between 1 and 10. For dall-e-3, only n=1 is supported.
  // quality          string Optional Defaults to standard The quality of the image that will be generated. hd creates images with finer details and greater consistency across the image. This param is only supported for dall-e-3.
  // response_format  string or null Optional Defaults to url The format in which the generated images are returned. Must be one of url or b64_json.
  // size             string or null Optional Defaults to 1024x1024 The size of the generated images. Must be one of 256x256, 512x512, or 1024x1024 for dall-e-2. Must be one of 1024x1024, 1792x1024, or 1024x1792 for dall-e-3 models.
  // style            string or null Optional Defaults to vivid The style of the generated images. Must be one of vivid or natural. Vivid causes the model to lean towards generating hyper-real and dramatic images. Natural causes the model to produce more natural, less hyper-real looking images. This param is only supported for dall-e-3.
  // user             string Optional A unique identifier representing your end-user, which can help OpenAI to monitor and detect abuse. Learn more.

  FPrompt := aPrompt;

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/images/generations';

  JObj := TJsonObject.Create;

  Try
    JObj.AddPair('prompt', aPrompt);

    If UseDalle3 then
    Begin
      JObj.AddPair('model', 'dall-e-3');
      N := 1;

      Case aSize of
        TiaSize256:
          aSize := TiaSize1024; // pequeña
        TiaSize512:
          aSize := TiaSize1024_1792; // Mediana
        TiaSize1024:
          aSize := TiaSize1792_1024; // Grande
      End;

    End
    else
    Begin
      JObj.AddPair('model', 'dall-e-2');
      Case aSize of
        TiaSize1024:
          aSize := TiaSize256;
        TiaSize1024_1792:
          aSize := TiaSize512;
        TiaSize1792_1024:
          aSize := TiaSize1024;
      End;
    End;

    JObj.AddPair('n', N);

    If HdQuality then
      JObj.AddPair('quality', 'hd');

    Case aSize of
      TiaSize256:
        JObj.AddPair('size', '256x256');
      TiaSize512:
        JObj.AddPair('size', '512x512');
      TiaSize1024:
        JObj.AddPair('size', '1024x1024');
      TiaSize1024_1792:
        JObj.AddPair('size', '1024x1792');
      TiaSize1792_1024:
        JObj.AddPair('size', '1792x1024');
    End;

    If ResponseFormat = TAiImageResponseFormat.tiaRUrl then
      JObj.AddPair('response_format', 'url')
    else
      JObj.AddPair('response_format', 'b64_json');

    If Style = TAiImageAStyleFormat.tiaStyleVivid then
      JObj.AddPair('style', 'vivid')
    Else
      JObj.AddPair('style', 'natural');

    JObj.AddPair('user', aUser);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      ParseGenerate(JObj);
      Result := FImages[0];
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
  End;

end;

procedure TAiImages.ParseGenerate(JObj: TJsonObject);
Var
  Data: TJSonArray;
  JObj1: TJsonObject;
  i: Integer;
  Image: TAiImagesFile;
begin
  Data := JObj.GetValue<TJSonArray>('data');

  SetLength(FImages, Data.Count);

  For i := 0 to Data.Count - 1 do
  Begin
    JObj1 := TJsonObject(Data.Items[i]);
    Image := TAiImagesFile.Create;
    FImages[i] := Image;
    Image.ParseImage(JObj1);
  End;
end;

procedure TAiImages.ParseVariations(JObj: TJsonObject);
Var
  Data: TJSonArray;
  JObj1: TJsonObject;
  i: Integer;
  Image: TAiImagesFile;
begin
  Data := JObj.GetValue<TJSonArray>('data');

  SetLength(FImages, Data.Count);

  For i := 0 to Data.Count - 1 do
  Begin
    JObj1 := TJsonObject(Data.Items[i]);
    Image := TAiImagesFile.Create;
    FImages[i] := Image;
    Image.ParseImage(JObj1);
  End;
end;

procedure TAiImages.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiImages.SetImages(const Value: TAiImagesFiles);
begin
  FImages := Value;
end;

procedure TAiImages.Setrevised_prompt(const Value: String);
begin
  Frevised_prompt := Value;
end;

function TAiImages.Variation(aImage: TMemoryStream; aUser: String; aSize: TAiImageSize; N: Integer; ResponseFormat: TAiImageResponseFormat): TAiImagesFile;
Var
  Body: TMultipartFormData;
  Client: THTTPClient;
  sUrl: String;
  Res: IHTTPResponse;
  JObj: TJsonObject;
begin
  sUrl := 'https://api.openai.com/v1/images/variations';

  aImage.Position := 0;

  Client := THTTPClient.Create;
  Try
    Body := TMultipartFormData.Create;
    Try
      Try
        Body.AddStream('image', aImage, ExtractFileName('origen.png'));

        Body.AddField('user', aUser);
        Body.AddField('model', 'dall-e-2'); // Solo acepta esta versión
        Body.AddField('n', N.ToString);

        Case aSize of
          TiaSize256:
            Body.AddField('size', '256x256');
          TiaSize512:
            Body.AddField('size', '512x512');
          TiaSize1024:
            Body.AddField('size', '1024x1024');
          TiaSize1024_1792:
            Body.AddField('size', '1024x1024'); // '1024x1792');
          TiaSize1792_1024:
            Body.AddField('size', '1024x1024'); // '1792x1024');
        End;

        If ResponseFormat = TAiImageResponseFormat.tiaRUrl then
          Body.AddField('response_format', 'url')
        else
          Body.AddField('response_format', 'b64_json');

        // Añadir el header de autorización
        Client.CustomHeaders['Authorization'] := 'Bearer ' + ApiKey;

        // La llamada Post debería cuidar del Content-Type automáticamente
        Res := Client.Post(sUrl, Body, nil);

        if Res.StatusCode = 200 then
        Begin

          JObj := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
          ParseGenerate(JObj);
          Result := FImages[0];

        End
        else
          Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
      Finally
      End;
    Finally
      Body.Free;
    End;
  Finally
    Client.Free;
  End;
end;

{ TAiFile }

constructor TAiFile.Create(aApiKey: String);
begin
  inherited Create;
  FApiKey := aApiKey;
  FContent := TMemoryStream.Create;
end;

destructor TAiFile.Destroy;
begin

  inherited;
end;

function TAiFile.GetContent: TMemoryStream;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Response: TMemoryStream;
  Res: IHTTPResponse;
  sUrl: String;
begin

  FContent.Clear;
  FContent.Position := 0;

  Client := THTTPClient.Create;
  sUrl := 'https://api.openai.com/v1/files/' + Fid + '/content';
  Response := TMemoryStream.Create;

  Try
    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + ApiKey)];
    Headers := Headers + [TNetHeader.Create('OpenAI-Beta', 'assistants=v1')];
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      Response.Position := 0;
      FContent.LoadFromStream(Response);
      FContent.Position := 0;
      Result := FContent;
    End
    else
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);

  Finally
    Client.Free;
    Response.Free;
  End;
end;

procedure TAiFile.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

procedure TAiFile.Setfilename(const Value: String);
begin
  Ffilename := Value;
end;

procedure TAiFile.Setid(const Value: String);
begin
  Fid := Value;
end;

{ TAiVision }

constructor TAiVision.Create(aApiKey: String);
begin
  Inherited Create;
  FApiKey := aApiKey;
end;

destructor TAiVision.Destroy;
begin

  inherited;
end;

function TAiVision.GenerateByBase64(Text, Base64: String; role: String; Max_tokens: Integer; detail: Boolean): String;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj, JMsg, Obj1: TJsonObject;
  JMessages, JContent: TJSonArray;
  Res: IHTTPResponse;
  Response: TStringStream;
  ImagePayload: TStringStream;
  St: TStringStream;
  sUrl: String;
begin
  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/chat/completions';
  JObj := TJsonObject.Create;

  JMessages := TJSonArray.Create;
  JContent := TJSonArray.Create;

  Try
    JObj.AddPair('model', 'gpt-4-vision-preview');
    JObj.AddPair('max_tokens', 4000);

    JMsg := TJsonObject.Create;
    JMsg.AddPair('role', 'user');

    Obj1 := TJsonObject.Create;
    Obj1.AddPair('type', 'text');
    Obj1.AddPair('text', Text);
    JContent.Add(Obj1);

    ImagePayload := TStringStream.Create('{"type": "image_url", "image_url": {"url": "data:image/jpeg;base64,' + Base64 + '"}}', TEncoding.UTF8);
    try
      JContent.Add(TJsonObject.ParseJSONValue(ImagePayload.DataString) as TJsonObject);
    finally
      ImagePayload.Free;
    end;

    JMsg.AddPair('content', JContent);
    JMessages.Add(JMsg);
    JObj.AddPair('messages', JMessages);

    St.WriteString(UTF8Encode(JObj.Format));
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      ParseVision(JObj);
      Result := Self.Content;
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
  End;
end;

function TAiVision.GenerateByStream(Text: String; Stream: TMemoryStream; role: String; Max_tokens: Integer; detail: Boolean): String;
Var
  Base64: String;
begin
  Base64 := StreamToBase64(Stream);
  Result := GenerateByBase64(Text, Base64, role, Max_tokens, detail);
end;

function TAiVision.GenerateByUrl(Text, Url, role: String; Max_tokens: Integer; detail: Boolean): String;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  JObj, JMsg, Obj1: TJsonObject;
  JMessages, JContent: TJSonArray;
  Res: IHTTPResponse;
  Response: TStringStream;
  St: TStringStream;
  sUrl: String;
begin

  Client := THTTPClient.Create;
  St := TStringStream.Create('', TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := 'https://api.openai.com/v1/chat/completions';
  JObj := TJsonObject.Create;

  JMessages := TJSonArray.Create;

  JContent := TJSonArray.Create;

  Try
    JObj.AddPair('model', 'gpt-4-vision-preview');
    JObj.AddPair('max_tokens', 4000);

    JMsg := TJsonObject.Create;
    JMsg.AddPair('role', 'user');

    Obj1 := TJsonObject.Create;
    Obj1.AddPair('type', 'text');
    Obj1.AddPair('text', Text);
    JContent.Add(Obj1);

    Obj1 := TJsonObject.Create;
    Obj1.AddPair('type', 'image_url');
    Obj1.AddPair('image_url', TJsonObject.Create.AddPair('url', Url));
    JContent.Add(Obj1);

    JMsg.AddPair('content', JContent);

    JMessages.Add(JMsg);

    JObj.AddPair('messages', JMessages);

    St.WriteString(JObj.Format);
    St.Position := 0;

    Headers := [TNetHeader.Create('Authorization', 'Bearer ' + FApiKey)];
    Client.ContentType := 'application/json';

    Res := Client.Post(sUrl, St, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      Result := ParseVision(JObj);
    End
    else
    begin
      Raise Exception.CreateFmt('Error Received: %d, %s', [Res.StatusCode, Res.ContentAsString]);
    end;

  Finally
    Client.Free;
    St.Free;
    Response.Free;
    JObj.Free;
  End;
end;

Function TAiVision.ParseVision(Response: TJsonObject): String;
Var
  id, Model, role: String;
  // Prompt_Tokenes, completion_tokens, total_tokens, index: Integer;
  JArr: TJSonArray;
  i: Integer;

begin

  id := Response.GetValue<String>('id');
  Model := Response.GetValue<String>('model');
  FPrompt_Tokenes := Response.GetValue<TJsonObject>('usage').GetValue<Integer>('prompt_tokens');
  Fcompletion_tokens := Response.GetValue<TJsonObject>('usage').GetValue<Integer>('completion_tokens');
  Ftotal_tokens := Response.GetValue<TJsonObject>('usage').GetValue<Integer>('total_tokens');
  JArr := Response.GetValue<TJSonArray>('choices');
  FContent := '';

  For i := 0 to JArr.Count - 1 do
  Begin
    role := JArr.Items[i].GetValue<TJsonObject>('message').GetValue<String>('role');
    FContent := FContent + JArr.Items[i].GetValue<TJsonObject>('message').GetValue<String>('content') + sLineBreak;
  End;
  FContent := Trim(FContent);
  Result := FContent;

end;

procedure TAiVision.SetApiKey(const Value: String);
begin
  FApiKey := Value;
end;

{ TAiImagesFiles }

Function TAiImagesFile.Base64ToStream(Base64: String): TMemoryStream;
begin
  Result := TBytesStream.Create(TNetEncoding.Base64.DecodeStringToBytes(Base64));
end;

constructor TAiImagesFile.Create;
begin
  Inherited Create;
  FImage := TMemoryStream.Create;
end;

destructor TAiImagesFile.Destroy;
begin
  FImage.Free;
  inherited;
end;

function TAiImagesFile.GetImage: TMemoryStream;
begin
  If (FImage.Size <= 0) then
  Begin
    If Url <> '' then
      LoadImage(Url);
  End;

  Result := FImage;
end;

Function TAiImagesFile.LoadImage(Url: String): TMemoryStream;
Var
  NetHttp: TNetHTTPClient;
  Resp: IHTTPResponse;

Begin
  Result := Nil;
  FImage.Clear;
  FImage.Position := 0;
  NetHttp := TNetHTTPClient.Create(Nil);
  try
    Resp := NetHttp.Get(Url, FImage);
    FImage.Position := 0;
    if Resp.StatusCode = 200 then // file was found
    Begin
      Result := FImage;
    End;
  finally
    NetHttp.Free;
  end;
End;

procedure TAiImagesFile.ParseImage(JObj: TJsonObject);
begin
  If JObj.TryGetValue<String>('url', FUrl) then
  Begin

  End;

  If JObj.TryGetValue<String>('b64_json', FBase64) then
  Begin
    FImage.Free;
    FImage := Base64ToStream(FBase64);
    FImage.Position := 0;
  End;

  JObj.TryGetValue<String>('revised_prompt', Frevised_prompt);

end;

end.
