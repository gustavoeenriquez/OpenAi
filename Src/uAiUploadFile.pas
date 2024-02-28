unit uAiUploadFile;

interface

Uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.JSON, System.Net.Mime, System.Net.URLClient, System.Net.HttpClient,
  System.Net.HttpClientComponent;

Type
  TAigUploadFile = Class(TObject)
  Private
    FLocalFileName: String;
    FRemoteFileName: String;
    FUrl: String;
    FPassword: String;
    FLogin: String;
    procedure SetLocalFileName(const Value: String);
    procedure SetRemoteFileName(const Value: String);
    procedure SetUrl(const Value: String);
    procedure SetLogin(const Value: String);
    procedure SetPassword(const Value: String);
  Protected
  Public
    Constructor Create(aUrl: String);
    Class Function UploadFile(url, FileName : String) : String;
    Class Function DownLoadFile(Url, FileName : String) : TMemoryStream;
    Function Upload(FileName: String = ''): String;
    Function Delete(FileName: String = ''): Boolean;
    Function DownLoad(FileName: String = ''): TMemoryStream;
    Function FileExists(FileName: String = ''): Boolean;
    Property LocalFileName: String read FLocalFileName write SetLocalFileName;
    Property RemoteFileName: String read FRemoteFileName write SetRemoteFileName;
    Property Url: String read FUrl write SetUrl;
    Property Login: String read FLogin write SetLogin;
    Property Password: String read FPassword write SetPassword;
  End;

implementation

{ TAiUploadFile }

constructor TAigUploadFile.Create(aUrl: String);
begin
  Inherited Create;
  FUrl := aUrl;
end;

function TAigUploadFile.Delete(FileName: String): Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl: String;
  JObj: TJsonObject;
begin

  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + '/deletefile?filename=' + ExtractFileName(FileName);

  Try
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      JObj := TJsonObject(TJsonObject.ParseJSONValue(Res.ContentAsString));
      Result := JObj.GetValue<Boolean>('status');
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

function TAigUploadFile.DownLoad(FileName: String): TMemoryStream;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl: String;
begin

  Result := TMemoryStream.Create;
  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + '/' + ExtractFileName(FileName);;

  Try
    Client.ContentType := 'application/octet-stream';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      Response.Position := 0;
      Result.LoadFromStream(Response);
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

class function TAigUploadFile.DownLoadFile(Url, FileName: String): TMemoryStream;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  sUrl: String;
begin

  Result := TMemoryStream.Create;
  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := Url + '/' + ExtractFileName(FileName);;

  Try
    Client.ContentType := 'application/octet-stream';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      Response.Position := 0;
      Result.LoadFromStream(Response);
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

function TAigUploadFile.FileExists(FileName: String): Boolean;
Var
  Client: THTTPClient;
  Headers: TNetHeaders;
  Res: IHTTPResponse;
  Response: TStringStream;
  S, sUrl: String;
  JObj: TJsonObject;
begin

  Client := THTTPClient.Create;
  Response := TStringStream.Create('', TEncoding.UTF8);
  sUrl := FUrl + '/exitsfile?filename=' + ExtractFileName(FileName);

  Try
    Client.ContentType := 'application/json';

    Res := Client.Get(sUrl, Response, Headers);

    if Res.StatusCode = 200 then
    Begin
      S := Res.ContentAsString;
      JObj := TJsonObject(TJsonObject.ParseJSONValue(S));
      Result := JObj.GetValue<Boolean>('status');
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

function TAigUploadFile.Upload(FileName: String): String;
Var
  Body: TMultipartFormData;
  Client: THTTPClient;
  S, sUrl: String;
  Res: IHTTPResponse;
  Stream: TFileStream;
  JObj: TJsonObject;
begin
  sUrl := FUrl + '/uploadfile';

  FLocalFileName := FileName;

  Client := THTTPClient.Create;
  Try
    Body := TMultipartFormData.Create;
    Try
      Stream := TFileStream.Create(FileName, fmOpenRead);
      Try
        Body.AddStream('filename', Stream, ExtractFileName(FileName)); // Asumir que 'mydata.jsonl' es application/json

        // Body.AddField('adicional', adicional);

        // Añadir el header de autorización
        // Client.CustomHeaders['Authorization'] := 'Bearer millave';

        // La llamada Post debería cuidar del Content-Type automáticamente
        Res := Client.Post(sUrl, Body, nil);

        if Res.StatusCode = 200 then
        Begin
          S := Res.ContentAsString;
          JObj := TJsonObject(TJsonObject.ParseJSONValue(S));
          Result := JObj.GetValue('filename').Value;
          FRemoteFileName := Result;
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

class function TAigUploadFile.UploadFile(url, FileName: String): String;
Var
  Body: TMultipartFormData;
  Client: THTTPClient;
  S, sUrl: String;
  Res: IHTTPResponse;
  Stream: TFileStream;
  JObj: TJsonObject;
begin
  sUrl := Url + '/uploadfile';

  Client := THTTPClient.Create;
  Try
    Body := TMultipartFormData.Create;
    Try
      Stream := TFileStream.Create(FileName, fmOpenRead);
      Try
        Body.AddStream('filename', Stream, ExtractFileName(FileName)); // Asumir que 'mydata.jsonl' es application/json

        // Body.AddField('adicional', adicional);

        // Añadir el header de autorización
        // Client.CustomHeaders['Authorization'] := 'Bearer millave';

        // La llamada Post debería cuidar del Content-Type automáticamente
        Res := Client.Post(sUrl, Body, nil);

        if Res.StatusCode = 200 then
        Begin
          S := Res.ContentAsString;
          JObj := TJsonObject(TJsonObject.ParseJSONValue(S));
          Result := JObj.GetValue('filename').Value;
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

procedure TAigUploadFile.SetLocalFileName(const Value: String);
begin
  FLocalFileName := Value;
end;

procedure TAigUploadFile.SetLogin(const Value: String);
begin
  FLogin := Value;
end;

procedure TAigUploadFile.SetPassword(const Value: String);
begin
  FPassword := Value;
end;

procedure TAigUploadFile.SetRemoteFileName(const Value: String);
begin
  FRemoteFileName := Value;
end;

procedure TAigUploadFile.SetUrl(const Value: String);
begin
  FUrl := Value;
end;

end.
