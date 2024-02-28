unit uMainAssistant;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.JSON, Rest.JSON, System.Actions, System.IOUtils, System.Threading,
  Winapi.ShellAPI, Winapi.Windows,

  fastTelega.AvailableTypes, fastTelega.Bot, fastTelega.LongPoll,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.ActnList, FMX.Objects, FMX.Platform, FMX.Clipboard,
  uAiOpenAssistant, uOpenAi, uDbModule,

  FMX.Ani, FMX.TabControl, System.ImageList, FMX.ImgList, FMX.Menus, FMX.Media;

type
  TMainAssistant = class(TForm)
    MainLayout: TLayout;
    LayoutPrompt: TLayout;
    LayStatus: TLayout;
    Layout3: TLayout;
    LayCliente: TLayout;
    Layout5: TLayout;
    Layout6: TLayout;
    MemoPrompt: TMemo;
    BtnRun: TButton;
    Splitter2: TSplitter;
    Label1: TLabel;
    RecStatus: TRectangle;
    LblStatus: TLabel;
    Assistant1: TAiOpenAssistant;
    AniIndicator1: TAniIndicator;
    LayToolButtons: TLayout;
    BtnClearChat: TButton;
    Layout1: TLayout;
    TabControl1: TTabControl;
    TabFilesGlobal: TTabItem;
    TabFilesGenerados: TTabItem;
    OpenDialog1: TOpenDialog;
    ListBoxGenerados: TListBox;
    ListBoxArchivos: TListBox;
    ImageList1: TImageList;
    Layout9: TLayout;
    BtnListarArchivos: TSpeedButton;
    BtnUploadFile: TSpeedButton;
    Layout10: TLayout;
    Layout8: TLayout;
    MemoResponse: TMemo;
    Layout11: TLayout;
    BtnClearOutput: TSpeedButton;
    SaveDialog1: TSaveDialog;
    BtnBorrarArchivo: TSpeedButton;
    BtnGuardarCambios: TButton;
    BtnCopyResponse: TSpeedButton;
    Splitter1: TSplitter;
    BtnBorrarArchivosGenerado: TSpeedButton;
    LayAnexos: TLayout;
    MemoAnexos: TMemo;
    BtnAnexos: TSpeedButton;
    SpeedButton1: TSpeedButton;
    Layout4: TLayout;
    SpeedButton2: TSpeedButton;
    SpeedButton4: TSpeedButton;
    ProgressBar1: TProgressBar;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    ActionList1: TActionList;
    ac_NuevoAsistente: TAction;
    ac_EditarAsistente: TAction;
    ac_GuardarCambios: TAction;
    ac_EliminarAsistente: TAction;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Layout2: TLayout;
    BtnActive: TButton;
    ChRetreival: TCheckBox;
    ChCodeInterpreter: TCheckBox;
    EditNombreAsistente: TComboBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    UploadAssistantFile: TSpeedButton;
    Layout7: TLayout;
    BtnBorrarAssistantArchivo: TSpeedButton;
    ChUseTelegram: TCheckBox;
    Label2: TLabel;
    MediaPlayer1: TMediaPlayer;
    Button1: TButton;
    procedure Assistant1StatusChange(Sender: TObject; aStatus: string);
    procedure Assistant1Response(Sender: TObject; Response: TAiMessage; Content: string);
    procedure BtnRunClick(Sender: TObject);
    procedure BtnActiveClick(Sender: TObject);
    procedure Assistant1CallToolFunction(Sender: TObject; AiToolCall: TAiToolsFunction);
    procedure MemoPromptKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure MemoPromptChange(Sender: TObject);
    procedure BtnClearChatClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ChRetreivalChange(Sender: TObject);
    procedure ChCodeInterpreterChange(Sender: TObject);
    procedure BtnListarArchivosClick(Sender: TObject);
    procedure BtnUploadFileClick(Sender: TObject);
    procedure BtnBorrarArchivoClick(Sender: TObject);
    procedure BtnGuardarCambiosClick(Sender: TObject);
    procedure BtnClearOutputClick(Sender: TObject);
    procedure BtnCopyResponseClick(Sender: TObject);
    procedure ListBoxGeneradosDblClick(Sender: TObject);
    procedure BtnBorrarArchivosGeneradoClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure BtnAnexosClick(Sender: TObject);
    procedure ListBoxItem1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MemoAnexosDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure MemoAnexosDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure BtnAnexosDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure ListBoxArchivosChange(Sender: TObject);
    procedure ListBoxGeneradosChange(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure BtnBorrarArchivoDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure BtnBorrarArchivoDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure ac_EditarAsistenteExecute(Sender: TObject);
    procedure ac_EditarAsistenteUpdate(Sender: TObject);
    procedure ac_GuardarCambiosExecute(Sender: TObject);
    procedure ac_GuardarCambiosUpdate(Sender: TObject);
    procedure ac_EliminarAsistenteUpdate(Sender: TObject);
    procedure ac_EliminarAsistenteExecute(Sender: TObject);
    procedure ac_NuevoAsistenteExecute(Sender: TObject);
    procedure UploadAssistantFileClick(Sender: TObject);
    procedure UploadAssistantFileDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure UploadAssistantFileDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure BtnBorrarAssistantArchivoDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure BtnBorrarAssistantArchivoClick(Sender: TObject);
    procedure ChUseTelegramChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FModificado: Boolean;
    Function GetDiaSemana: String;
    Procedure EnviarMensaje;
    Procedure BeforeChatMessage;
    Procedure AfterChatMessage;
    Procedure AfterDisconnect;
    Procedure AddFileToGenerados(FileName: String);
    Procedure ListarArchivos;
    Procedure UploadFileToAssistant;
    Procedure MenuItemBorrarArchivo;
    Procedure MenuItemBorrarArchivoAssistant;

    Procedure MenuItemBorrarArchivoGenerado;
    Procedure AbrirArchivo(FileName: String);
    procedure SetModificado(const Value: Boolean);
    Function ListarAsistentes: String;

    Procedure UpdateAssistantNames;
    Procedure OnTeleMessage(Sender: TObject; Msg: String);
    Procedure EnviarTextoAVoz(Texto: String);
  public
    Property Modificado: Boolean read FModificado write SetModificado;
  end;

var
  MainAssistant: TMainAssistant;

  Bot: TftBot;
  AiBot: TBotAssistant;

implementation

{$R *.fmx}

uses uManAssistant;

procedure TMainAssistant.AbrirArchivo(FileName: String);
begin
  if FileExists(FileName) then
  begin
    ShellExecute(0, 'OPEN', PChar(FileName), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TMainAssistant.ac_EditarAsistenteExecute(Sender: TObject);
Var
  FMan: TFManAssistant;
begin
  FMan := TFManAssistant.Create(Self);
  FMan.EditModelo.Items.CommaText := Assistant1.GetModelsList;

  FMan.EditNombreAsistente.Text := Assistant1.AssistantName;
  FMan.EditModelo.ItemIndex := FMan.EditModelo.Items.IndexOf(Assistant1.Model);
  FMan.MemoInstrucciones.Text := Assistant1.Instructions.Text;
  FMan.MemoFunciones.Text := Assistant1.Tools.Text;
  FMan.ChRetrieval.IsChecked := Assistant1.Retrieval;
  FMan.ChCodeInterpreter.IsChecked := Assistant1.Code_Interpreter;

  Try
    If FMan.ShowModal = MrOk then
    Begin
      Assistant1.AssistantName := FMan.EditNombreAsistente.Text.Trim;;
      Assistant1.Model := FMan.EditModelo.Text;
      Assistant1.Instructions.Text := FMan.MemoInstrucciones.Text.Trim;
      Assistant1.Tools.Text := FMan.MemoFunciones.Text.Trim;
      Assistant1.Retrieval := FMan.ChRetrieval.IsChecked;
      Assistant1.Code_Interpreter := FMan.ChCodeInterpreter.IsChecked;
      Self.Cursor := crHourGlass;
      If Assistant1.ApplyUpdates then
      Begin
        Self.Cursor := crDefault;
        ShowMessage('El asistente "' + Assistant1.AssistantName + '" se actualizó correctamente');
      End;
    End;
  Finally
    FMan.Free;
  End;
end;

procedure TMainAssistant.ac_EditarAsistenteUpdate(Sender: TObject);
begin
  ac_EditarAsistente.Enabled := Assistant1.Active;
end;

procedure TMainAssistant.ac_EliminarAsistenteExecute(Sender: TObject);
begin
  //
end;

procedure TMainAssistant.ac_EliminarAsistenteUpdate(Sender: TObject);
begin
  ac_EliminarAsistente.Enabled := Assistant1.Active;
end;

procedure TMainAssistant.ac_GuardarCambiosExecute(Sender: TObject);
begin
  //
end;

procedure TMainAssistant.ac_GuardarCambiosUpdate(Sender: TObject);
begin
  ac_GuardarCambios.Enabled := Modificado;
end;

procedure TMainAssistant.ac_NuevoAsistenteExecute(Sender: TObject);
Var
  FMan: TFManAssistant;
  Assistente: TAiOpenAssistant;
begin
  FMan := TFManAssistant.Create(Self);
  Var
  Lista := TStringList.Create;
  Try
    Lista.Text := Assistant1.GetModelsList;
    Lista.Sort;
    FMan.EditModelo.Items.CommaText := Lista.CommaText;
  Finally
    Lista.Free;
  End;

  Assistente := TAiOpenAssistant.Create(Self);

  Try
    If FMan.ShowModal = MrOk then
    Begin
      Assistente.ApiKey := Assistant1.ApiKey;
      Assistente.AssistantName := FMan.EditNombreAsistente.Text.Trim;;
      Assistente.Model := FMan.EditModelo.Text;
      Assistente.Instructions.Text := FMan.MemoInstrucciones.Text.Trim;
      Assistente.Tools.Text := FMan.MemoFunciones.Text.Trim;
      Assistente.Retrieval := FMan.ChRetrieval.IsChecked;
      Assistente.Code_Interpreter := FMan.ChCodeInterpreter.IsChecked;
      Self.Cursor := crHourGlass;
      If Assistente.CreateNewAssistant then
      Begin
        UpdateAssistantNames;
        Self.Cursor := crDefault;
        ShowMessage('El asistente "' + Assistente.AssistantName + '" se creó correctamente');
      End;
    End;
  Finally
    FMan.Free;
    Assistente.Free;
  End;
end;

procedure TMainAssistant.AddFileToGenerados(FileName: String);
Var
  Item: TListBoxItem;
  Ext: String;
begin
  Ext := LowerCase(ExtractFileExt(FileName).Trim);
  Item := TListBoxItem.Create(Self);
  Item.StyleLookup := 'listboxitembottomdetail';
  Item.Text := ExtractFileName(FileName);
  Item.ItemData.Detail := FileName;
  Item.ItemData.Accessory := TListBoxItemData.TAccessory.aDetail;
  Item.Height := 40;
  Item.TagString := FileName;

  If (Ext = '.png') or (Ext = '.bmp') or (Ext = '.jpg') then
  Begin
    Item.ItemData.Bitmap.LoadFromFile(FileName);
    If ChUseTelegram.IsChecked then
      Bot.API.sendPhoto(IdChatUsuario, TftInputFile.fromFile(FileName, 'image/png'), FileName);
  End
  Else
  Begin
    If ChUseTelegram.IsChecked then
      Bot.API.sendDocument(IdChatUsuario, TftInputFile.fromFile(FileName, ''));
    Item.ImageIndex := 11;
  End;

  ListBoxGenerados.AddObject(Item);
  BtnBorrarArchivosGenerado.Enabled := Assigned(ListBoxGenerados.Selected);
end;

procedure TMainAssistant.BeforeChatMessage;
begin
  AniIndicator1.Visible := True;
  AniIndicator1.Enabled := True;
  LayToolButtons.Enabled := False;
  RecStatus.Fill.Color := TAlphaColorRec.Gold;
  BtnRun.ImageIndex := Ico_Pause;
  MemoPrompt.Enabled := False;
  BtnAnexos.IsPressed := False;
  LayAnexos.Visible := False;
  LayStatus.Enabled := False;
  ProgressBar1.Value := 0
end;

procedure TMainAssistant.AfterChatMessage;
begin
  BtnRun.Enabled := True;
  AniIndicator1.Visible := False;
  AniIndicator1.Enabled := False;
  RecStatus.Fill.Color := TAlphaColorRec.Lightgreen;
  BtnRun.ImageIndex := Ico_Play;
  MemoPrompt.Enabled := True;
  LayToolButtons.Enabled := True;
  MemoPrompt.SetFocus;
  BtnAnexos.IsPressed := False;
  LayAnexos.Visible := False;
  LayCliente.Enabled := True;
  LayStatus.Enabled := True;
  ProgressBar1.Value := 0
end;

procedure TMainAssistant.AfterDisconnect;
begin
  BtnRun.Enabled := False;
  BtnRun.ImageIndex := Ico_Play;
  AniIndicator1.Visible := False;
  AniIndicator1.Enabled := False;
  LayToolButtons.Enabled := False;
  RecStatus.Fill.Color := TAlphaColorRec.Red;
  MemoPrompt.Enabled := False;
  MemoPrompt.Lines.Clear;
  MemoResponse.Lines.Clear;
  BtnAnexos.IsPressed := False;
  LayAnexos.Visible := False;
  ListBoxArchivos.Clear;
  LayCliente.Enabled := False;
  LayStatus.Enabled := False;
  TabControl1.ActiveTab := TabFilesGlobal;
  ProgressBar1.Value := 0
end;

procedure TMainAssistant.Assistant1CallToolFunction(Sender: TObject; AiToolCall: TAiToolsFunction);
Var
  JObj: TJSonObject;
  Id: Integer;
  DbModule: TDbModule;
begin
  If AiToolCall.Name = 'get_fecha' then
  Begin
    AiToolCall.Response := GetDiaSemana + ' ' + FormatDateTime('yyyy-mm-dd hh:mm:ss', Now);
  End;

  If AiToolCall.Name = 'cita_get' then
  Begin
    DbModule := TDbModule.Create(Self);
    Try
      AiToolCall.Response := DbModule.Cita_Get('').ToJSON;
    Finally
      DbModule.Free;
    End;
  End;

  If AiToolCall.Name = 'cita_delete' then
  Begin
    JObj := TJSonObject(TJSonObject.ParseJSONValue(AiToolCall.Arguments));
    If Assigned(JObj) then
    Begin
      Id := JObj.GetValue<Integer>('id_cita');

      DbModule := TDbModule.Create(Self);
      Try
        AiToolCall.Response := 'El resultado de la eliminación de la cita es ' + DbModule.Cita_Delete(Id).ToString;
      Finally
        DbModule.Free;
      End;
    End;
  End;

  If AiToolCall.Name = 'cita_insert' then
  Begin
    JObj := TJSonObject(TJSonObject.ParseJSONValue(AiToolCall.Arguments));
    If Assigned(JObj) then
    Begin
      DbModule := TDbModule.Create(Self);
      Try
        AiToolCall.Response := 'La cita se insertó con el Id ' + DbModule.Cita_Insert(JObj);
      Finally
        DbModule.Free;
      End;
    End;
  End;

  If AiToolCall.Name = 'consulta_ventas' then
  Begin
    JObj := TJSonObject(TJSonObject.ParseJSONValue(AiToolCall.Arguments));
    If Assigned(JObj) then
    Begin
      DbModule := TDbModule.Create(Self);
      Try
        AiToolCall.Response := DbModule.Consulta_Ventas(JObj);
      Finally
        DbModule.Free;
      End;
    End;
  End;

  If AiToolCall.Name = 'historia_clinica_get' then
  Begin
    Var
      NomPaciente, Descripcion: String;

    JObj := TJSonObject(TJSonObject.ParseJSONValue(AiToolCall.Arguments));
    If Assigned(JObj) then
    Begin
      JObj.TryGetValue<String>('nom_paciente', NomPaciente);
      JObj.TryGetValue<String>('descripcion', Descripcion);

      DbModule := TDbModule.Create(Self);
      DbModule.ApiKey := Assistant1.ApiKey;

      Try
        AiToolCall.Response := DbModule.Historia_Get(NomPaciente, Descripcion).ToString;
      Finally
        DbModule.Free;
      End;
    End;
  End;

  If AiToolCall.Name = 'historia_clinica_delete' then
  Begin
    JObj := TJSonObject(TJSonObject.ParseJSONValue(AiToolCall.Arguments));
    If Assigned(JObj) then
    Begin
      Id := JObj.GetValue<Integer>('id_cita');

      DbModule := TDbModule.Create(Self);
      DbModule.ApiKey := Assistant1.ApiKey;
      Try
        AiToolCall.Response := 'El resultado de la eliminación de la historia es ' + DbModule.historia_Delete(Id).ToString;
      Finally
        DbModule.Free;
      End;
    End;
  End;

  If AiToolCall.Name = 'historia_clinica_insert' then
  Begin
    JObj := TJSonObject(TJSonObject.ParseJSONValue(AiToolCall.Arguments));
    If Assigned(JObj) then
    Begin
      DbModule := TDbModule.Create(Self);
      DbModule.ApiKey := Assistant1.ApiKey;
      Try
        AiToolCall.Response := 'La historia se insertó con el Id ' + DbModule.historia_Insert(JObj);
      Finally
        DbModule.Free;
      End;
    End;
  End;

end;

procedure TMainAssistant.Assistant1Response(Sender: TObject; Response: TAiMessage; Content: string);
Var
  Clave, FileName: String;
  AiFile: TAiFile;
begin
  Try
    If Assigned(Response) then
    Begin
      MemoResponse.Lines.Add(Response.role + ' : ' + Trim(Content));
      MemoResponse.Lines.Add('');
      MemoResponse.GoToTextEnd;

      If ChUseTelegram.IsChecked then
      Begin
        Bot.API.sendMessage(IdChatUsuario, Content);
        EnviarTextoAVoz(Content);
      End;
    End;

    For Clave in Response.Files.Keys do
    Begin
      AiFile := Response.Files[Clave];
      FileName := System.IOUtils.TPath.Combine('c:\temp', ExtractFileName(AiFile.FileName));
      AiFile.Content.SaveToFile(FileName);
      Assistant1.DeleteFile(AiFile.Id);
      AddFileToGenerados(FileName);
      TabControl1.ActiveTab := TabFilesGenerados;

    End;
  Finally
    AfterChatMessage;
  End;
end;

procedure TMainAssistant.Assistant1StatusChange(Sender: TObject; aStatus: string);
begin
  If Copy(LblStatus.Text, 1, length(aStatus)) = aStatus then
  Begin
    LblStatus.Text := LblStatus.Text + '.';
    If ProgressBar1.Value >= 50 then
      ProgressBar1.Value := 0
    Else
      ProgressBar1.Value := ProgressBar1.Value + 1;
  End
  Else
    LblStatus.Text := aStatus;
end;

procedure TMainAssistant.BtnActiveClick(Sender: TObject);
Var
  Cita: TCita;
begin

  Self.Cursor := crHourGlass;

  Try
    If Assistant1.Active then
      Assistant1.Active := False
    Else
    Begin
      Assistant1.AssistantName := EditNombreAsistente.Text.Trim;
      Assistant1.Active := True;
    End;

    If Assistant1.Active then
    Begin
      Try
        BtnActive.Text := 'Desconectar';
        BtnActive.ImageIndex := Ico_Disconnect;

        ListarArchivos;
        ChRetreival.IsChecked := Assistant1.Retrieval;
        ChCodeInterpreter.IsChecked := Assistant1.Code_Interpreter;
        Modificado := False;
      Finally
        AfterChatMessage;
      End;
    End
    Else
    Begin
      BtnActive.Text := 'Conectar';
      BtnActive.ImageIndex := Ico_Connect;
      RecStatus.Fill.Color := TAlphaColorRec.Red;
      Modificado := False;
      AfterDisconnect;
    End;
  Finally
    Self.Cursor := crDefault;
  End;
end;

procedure TMainAssistant.BtnBorrarArchivoClick(Sender: TObject);
begin
  MenuItemBorrarArchivo;
end;

procedure TMainAssistant.BtnBorrarArchivoDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
Var
  Valor: String;
begin
  If Data.Source is TListBoxItem then
  Begin
    ListBoxArchivos.SelectRange(TListBoxItem(Data.Source), TListBoxItem(Data.Source));
    MenuItemBorrarArchivo;
  End;
end;

procedure TMainAssistant.BtnBorrarArchivoDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  If Data.Source is TListBoxItem then
    Operation := TDragOperation.Copy;

end;

procedure TMainAssistant.BtnBorrarArchivosGeneradoClick(Sender: TObject);
begin
  MenuItemBorrarArchivoGenerado;
end;

procedure TMainAssistant.BtnBorrarAssistantArchivoClick(Sender: TObject);
begin
  If Assigned(ListBoxArchivos.Selected) then
  Begin
    MenuItemBorrarArchivoAssistant;
  End;
end;

procedure TMainAssistant.BtnBorrarAssistantArchivoDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
Var
  Valor: String;
begin
  If Data.Source is TListBoxItem then
  Begin
    ListBoxArchivos.SelectRange(TListBoxItem(Data.Source), TListBoxItem(Data.Source));
    MenuItemBorrarArchivoAssistant;
  End;
end;

procedure TMainAssistant.BtnClearChatClick(Sender: TObject);
begin
  Self.Cursor := crHourGlass;
  Try
    MemoPrompt.Lines.Clear;
    MemoResponse.Lines.Clear;
    If Assistant1.AutoRemoveThread = False then
    Begin
      Assistant1.RemoveThread;
      Assistant1.ThReadId := '';
    End;
    Assistant1.Active := False;
    Assistant1.Active := True;
  Finally
    Self.Cursor := crDefault;
  End;
end;

procedure TMainAssistant.BtnClearOutputClick(Sender: TObject);
begin
  MemoResponse.Lines.Clear;
end;

procedure TMainAssistant.BtnCopyResponseClick(Sender: TObject);
var
  uClipBoard: IFMXExtendedClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXExtendedClipboardService, uClipBoard) then
    uClipBoard.SetClipboard(MemoResponse.Text);
end;

procedure TMainAssistant.BtnGuardarCambiosClick(Sender: TObject);
begin
  Self.Cursor := crHourGlass;
  Try
    Assistant1.ApplyUpdates;
    Modificado := False;
  Finally
    Self.Cursor := crDefault;
  End;
end;

procedure TMainAssistant.BtnRunClick(Sender: TObject);
begin
  EnviarMensaje;
end;

procedure TMainAssistant.BtnUploadFileClick(Sender: TObject);
begin
  If OpenDialog1.Execute then
  Begin
    Assistant1.UploadFile(OpenDialog1.FileName);
    ListarArchivos;
  End;
end;

procedure TMainAssistant.Button1Click(Sender: TObject);
Var
  Fun: String;
begin

  Fun := '''
  { " type ": "
  function", "
  function": {
    "name": "get_nombre",
    "description": "retorna el nombre que utilizarás como asistente",
    "parameters": {
    "type": "object",
    "properties": {
  } , " required ": []} } }
  ''';

  Assistant1.Funciones.AddFunction(Fun);
  Assistant1.ApplyUpdates;

end;

procedure TMainAssistant.ChCodeInterpreterChange(Sender: TObject);
begin
  Assistant1.Code_Interpreter := ChCodeInterpreter.IsChecked;
  Modificado := True;
end;

procedure TMainAssistant.ChRetreivalChange(Sender: TObject);
begin
  Assistant1.Retrieval := ChRetreival.IsChecked;
  Modificado := True;
end;

procedure TMainAssistant.ChUseTelegramChange(Sender: TObject);
begin
  AiBot.Active := ChUseTelegram.IsChecked;
end;

procedure TMainAssistant.EnviarMensaje;
Var
  Lista: TStringList;
  S, Valor: String;
  I: Integer;
begin
  If Assistant1.Busy then
  Begin
    Assistant1.CancelRun;
    AfterChatMessage;
  End
  Else
  Begin
    BeforeChatMessage;

    Lista := TStringList.Create;
    Try
      For I := 0 to MemoAnexos.Lines.Count - 1 do
      Begin
        S := Trim(MemoAnexos.Lines[I]);
        If Pos(':', S) > 3 then
        Begin
          Valor := Trim(Copy(S, Pos(':', S) + 1, length(S)));
          If Valor <> '' then
            Lista.Add(Valor);
        End;
      End;

      Assistant1.AddMessage(MemoPrompt.Text, 'user', Lista.Text.Trim);
      MemoResponse.Lines.Add('User: ' + Trim(MemoPrompt.Text));
      MemoResponse.Lines.Add('');
      MemoPrompt.Text := '';
      MemoResponse.GoToTextEnd;
    Finally
      Lista.Free;
    End;
  End;
end;

procedure TMainAssistant.EnviarTextoAVoz(Texto: String);
begin
  TThread.CreateAnonymousThread(
    Procedure
    Var
      Audio: TAiAudio;
      St: TMemoryStream;
      FileName: String;
    Begin
      Try
        Audio := TAiAudio.Create(ApiKey);
        FileName := 'c:\temp\' + System.IOUtils.TPath.GetGUIDFileName + '.mp3';

        St := Audio.Speech(Texto, False, 'nova', 'mp3', 1);
        St.SaveToFile(FileName);

        If ChUseTelegram.IsChecked then
          Bot.API.sendAudio(IdChatUsuario, TftInputFile.fromFile(FileName, 'audio/mpeg'));

        TThread.Synchronize(Nil,
          Procedure
          Begin
            MediaPlayer1.FileName := FileName;
            MediaPlayer1.Play;

          End);
      Finally
      End;
    End).Start;
end;

procedure TMainAssistant.FormActivate(Sender: TObject);
begin
  If Not Assigned(AiBot) then
  Begin
    AiBot := TBotAssistant.Create(Assistant1.ApiKey);
    AiBot.Active := False;
    AiBot.OnTeleMessage := OnTeleMessage;
    AiBot.Run;
  End;
end;

procedure TMainAssistant.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  AiBot.Active := False;
  AiBot.LongPoll.Free;
  Assistant1.Active := False;
  Sleep(500);
end;

procedure TMainAssistant.FormCreate(Sender: TObject);
begin
  Bot := TftBot.Create(bot_key, 'https://api.telegram.org');

  FormatSettings.DateSeparator := '-';
  FormatSettings.TimeSeparator := ':';
  FormatSettings.ThousandSeparator := ',';
  FormatSettings.DecimalSeparator := '.';
  FormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  FormatSettings.ShortTimeFormat := 'hh:mm:ss';
  AfterDisconnect;
  UpdateAssistantNames;
end;

function TMainAssistant.GetDiaSemana: String;
Var
  DiaSemana: Integer;
begin
  DiaSemana := DayOfWeek(Date);

  case DiaSemana of
    1:
      Result := 'Domingo';
    2:
      Result := 'Lunes';
    3:
      Result := 'Martes';
    4:
      Result := 'Miércoles';
    5:
      Result := 'Jueves';
    6:
      Result := 'Viernes';
    7:
      Result := 'Sábado';
  end;
end;

procedure TMainAssistant.ListarArchivos;
Begin
  BeforeChatMessage;
  TTask.Run(
    Procedure
    Var
      List, List1: TAiFilesArray;
      AiFile: TAiFile;
    Begin
      List := Assistant1.ListFilesArray;
      List1 := Assistant1.ListAssistantFilesArray;

      TThread.Synchronize(nil,
        procedure
        Var
          Clave: String;
          Item: TListBoxItem;
          Header: TListBoxGroupHeader;
        begin
          Try
            ListBoxArchivos.Clear;
            BtnBorrarArchivo.Enabled := Assigned(ListBoxArchivos.Selected);

            Header := TListBoxGroupHeader.Create(Self);
            Header.Text := '   General Files';
            Header.Width := 30;
            ListBoxArchivos.AddObject(Header);

            If List.Count > 0 then
            Begin
              For Clave in List.Keys do
              Begin
                AiFile := List.Items[Clave];
                Item := TListBoxItem.Create(Self);
                Item.StyleLookup := 'listboxitembottomdetail';
                Item.Text := AiFile.FileName;
                Item.ItemData.Detail := AiFile.Id;
                Item.TagObject := AiFile;
                Item.ItemData.Accessory := TListBoxItemData.TAccessory.aDetail;
                Item.Height := 40;
                Item.ImageIndex := 0;
                Item.OnMouseDown := ListBoxItem1MouseDown;
                ListBoxArchivos.AddObject(Item);
              End;
            End;

            Item := TListBoxItem.Create(Self);
            Item.Text := '';
            Item.Height := 30;
            ListBoxArchivos.AddObject(Item);

            Header := TListBoxGroupHeader.Create(Self);
            Header.Text := '   Assistant Files';
            Header.Width := 30;
            ListBoxArchivos.AddObject(Header);

            If List1.Count > 0 then
            Begin
              For Clave in List1.Keys do
              Begin
                AiFile := List.Items[Clave];
                Item := TListBoxItem.Create(Self);
                Item.StyleLookup := 'listboxitembottomdetail';
                Item.Text := AiFile.FileName;
                Item.ItemData.Detail := AiFile.Id;
                Item.TagObject := AiFile;
                Item.ItemData.Accessory := TListBoxItemData.TAccessory.aDetail;
                Item.Height := 40;
                Item.ImageIndex := 0;
                Item.OnMouseDown := ListBoxItem1MouseDown;
                ListBoxArchivos.AddObject(Item);
              End;
            End;

          Finally
            AfterChatMessage;
          End;
        end);
    End);
end;

function TMainAssistant.ListarAsistentes: String;
Var
  JObj, JItem: TJSonObject;
  Arr: TJSonArray;
begin
  // JObj := Assistant1.lis

end;

procedure TMainAssistant.ListBoxArchivosChange(Sender: TObject);
begin
  BtnBorrarArchivo.Enabled := Assigned(ListBoxArchivos.Selected);
end;

procedure TMainAssistant.ListBoxGeneradosChange(Sender: TObject);
begin
  BtnBorrarArchivosGenerado.Enabled := Assigned(ListBoxGenerados.Selected);
end;

procedure TMainAssistant.ListBoxGeneradosDblClick(Sender: TObject);
Var
  Item: TListBoxItem;
begin
  If Assigned(ListBoxGenerados.Selected) then
  Begin
    Item := ListBoxGenerados.Selected;
    If Item.TagString <> '' then
      AbrirArchivo(Item.TagString);
  End;
end;

procedure TMainAssistant.ListBoxItem1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Svc: IFMXDragDropService;
  DragData: TDragObject;
  DragImage: TBitmap;
begin
  if (Assigned(Sender)) and (Sender Is TListBoxItem) and TPlatformServices.Current.SupportsPlatformService(IFMXDragDropService, Svc) then
  begin
    DragData.Source := Sender;
    DragImage := TListBoxItem(Sender).MakeScreenshot;
    try
      DragData.Data := DragImage;
      Svc.BeginDragDrop(Self, DragData, DragImage);
    finally
      DragImage.Free;
    end;
  end;
end;

procedure TMainAssistant.MemoAnexosDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
Var
  Valor: String;
begin
  If Data.Source is TListBoxItem then
  Begin
    Valor := TListBoxItem(Data.Source).Text + ' : ' + TListBoxItem(Data.Source).ItemData.Detail;
    MemoAnexos.Lines.Add(Valor);
  End;
end;

procedure TMainAssistant.MemoAnexosDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  Operation := TDragOperation.Copy;
end;

procedure TMainAssistant.MemoPromptChange(Sender: TObject);
var
  TextHeight: Single;
  Memo: TMemo;
begin
  Memo := Sender as TMemo;
  TextHeight := Memo.ContentBounds.Bottom;

  if TextHeight > Memo.Height then
  begin
    LayoutPrompt.Height := TextHeight + 60;
  end
  Else
    LayoutPrompt.Height := 120;
end;

procedure TMainAssistant.MemoPromptKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin

  If (ssCtrl in Shift) and (Key = vkReturn) then
    EnviarMensaje;

end;

procedure TMainAssistant.MenuItemBorrarArchivo;
Var
  Item: TListBoxItem;
  FFile: TAiFile;
  AiFiles: TAiFiles;
begin
  If Assigned(ListBoxArchivos.Selected) then
  Begin
    Item := ListBoxArchivos.Selected;
    FFile := TAiFile(Item.TagObject);
    If Assigned(FFile) then
    Begin
      If Assistant1.DeleteFile(FFile.Id) then
        ShowMessage('El archivo se eliminó correctamente');
      MemoAnexos.Lines.Clear;
      ListarArchivos;
    End;
  End;
end;

procedure TMainAssistant.MenuItemBorrarArchivoAssistant;
Var
  Item: TListBoxItem;
  FFile: TAiFile;
  AiFiles: TAiFiles;
begin
  If Assigned(ListBoxArchivos.Selected) then
  Begin
    Item := ListBoxArchivos.Selected;
    FFile := TAiFile(Item.TagObject);
    If Assigned(FFile) then
    Begin
      If Assistant1.DeleteAssistantFile(FFile.Id) then
        ShowMessage('El archivo se eliminó correctamente');
      MemoAnexos.Lines.Clear;
      ListarArchivos;
    End;
  End;
end;

procedure TMainAssistant.MenuItemBorrarArchivoGenerado;
Var
  Item: TListBoxItem;
begin
  If Assigned(ListBoxGenerados.Selected) then
  Begin
    Item := ListBoxGenerados.Selected;
    If (Item.TagString <> '') and FileExists(Item.TagString) then
    Begin
      System.IOUtils.TFile.Delete(Item.TagString);
      Item.Free;
    End;
  End;
end;

procedure TMainAssistant.OnTeleMessage(Sender: TObject; Msg: String);
begin
  MemoPrompt.Text := Msg;
  EnviarMensaje;
end;

procedure TMainAssistant.SetModificado(const Value: Boolean);
begin
  FModificado := Value;
  BtnGuardarCambios.Enabled := FModificado;
end;

procedure TMainAssistant.SpeedButton1Click(Sender: TObject);
Var
  AiFiles: TAiFiles;
  AiFile: TAiFile;
begin
  If OpenDialog1.Execute then
  Begin
    AiFiles := TAiFiles.Create(Assistant1.ApiKey);
    Try
      AiFile := AiFiles.UploadFile(OpenDialog1.FileName, True);
      MemoAnexos.Lines.Add(ExtractFileName(OpenDialog1.FileName) + '  : ' + AiFile.Id);
    Finally
      AiFiles.Free;
    End;
  End;
  ListarArchivos;
end;

procedure TMainAssistant.SpeedButton2Click(Sender: TObject);
begin
  MemoAnexos.Lines.Clear;
end;

procedure TMainAssistant.SpeedButton4Click(Sender: TObject);
var
  uClipBoard: IFMXExtendedClipboardService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXExtendedClipboardService, uClipBoard) then
    uClipBoard.SetClipboard(MemoAnexos.Text);
end;

procedure TMainAssistant.BtnAnexosClick(Sender: TObject);
begin
  LayAnexos.Visible := BtnAnexos.IsPressed;
end;

procedure TMainAssistant.BtnAnexosDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  If Data.Source is TListBoxItem then
  Begin
    BtnAnexos.IsPressed := True;
    LayAnexos.Visible := True;
  End;
end;

procedure TMainAssistant.BtnListarArchivosClick(Sender: TObject);
begin
  ListarArchivos;
end;

procedure TMainAssistant.UpdateAssistantNames;
begin

  TTask.Run(
    Procedure
    Var
      JObj: TJSonObject;
      JVal: TJSonValue;
      JArr: TJSonArray;
      Lista: TStringList;
      S: String;
    Begin

      JObj := TAiAssistant.GetList(Assistant1.ApiKey);
      Lista := TStringList.Create;

      Try
        JArr := JObj.GetValue<TJSonArray>('data');

        For JVal in JArr do
          Lista.Add(JVal.GetValue<String>('name'));

        S := Lista.CommaText;

        TThread.Synchronize(nil,
          procedure
          begin
            EditNombreAsistente.Items.CommaText := S;
          end);

      Finally
        JObj.Free;
        Lista.Free;
      End;

    End);

  EditNombreAsistente.Items.CommaText := Assistant1.GetAssistantsList;
  Exit;

end;

procedure TMainAssistant.UploadAssistantFileClick(Sender: TObject);
begin
  if (ChRetreival.IsChecked = False) and (ChCodeInterpreter.IsChecked = False) then
    Raise Exception.Create('Solo puede cargar archivos si el Retreival o el Code_Interpreter está activo');
  UploadFileToAssistant;
end;

procedure TMainAssistant.UploadAssistantFileDragDrop(Sender: TObject; const Data: TDragObject; const Point: TPointF);
Var
  Valor: String;
  Item: TListBoxItem;
  FFile: TAiFile;
  AiFiles: TAiFiles;
begin
  If Assigned(ListBoxArchivos.Selected) then
  Begin
    Item := ListBoxArchivos.Selected;
    FFile := TAiFile(Item.TagObject);
    If Assigned(FFile) then
    Begin
      Assistant1.Assistant.AddFileId(FFile.Id);
      ListarArchivos;
    End;
  End;
end;

procedure TMainAssistant.UploadAssistantFileDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  If Data.Source is TListBoxItem then
    Operation := TDragOperation.Copy;
end;

procedure TMainAssistant.UploadFileToAssistant;
begin
  If OpenDialog1.Execute then
  Begin
    Assistant1.UploadAssistantFile(OpenDialog1.FileName);
    ListarArchivos;
  End;
end;

end.
