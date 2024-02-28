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
//Nombre: Gustavo Enr�quez
//  Redes Sociales:
//- Email: gustavoeenriquez@gmail.com
//  - Telegram: +57 3128441700
//  - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
//  - Youtube: https://www.youtube.com/@cimamaker3945
//  - GitHub: https://github.com/gustavoeenriquez/
//
// puedes ver el video del funcionamiento de este demo en https://www.youtube.com/@cimamaker3945



unit uMainQueryOpenAi;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Phys, FireDAC.VCLUI.Wait,
  Data.DB, Vcl.Buttons, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids,
  System.JSON,
  System.Threading,

  FireDAC.Comp.Client, FireDAC.Comp.DataSet, Vcl.ExtCtrls, uAiOpenChat, uOpenAi,
  FireDAC.Phys.PG, FireDAC.Phys.PGDef, FireDAC.Stan.StorageJSON;

type
  TForm64 = class(TForm)
    AiOpenChat1: TAiOpenChat;
    Panel1: TPanel;
    Panel2: TPanel;
    FDQuery1: TFDQuery;
    FDConnection1: TFDConnection;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    MemoPrompt: TMemo;
    SpeedButton1: TSpeedButton;
    Memo2: TMemo;
    FDStanStorageJSONLink1: TFDStanStorageJSONLink;
    Button1: TButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure AiOpenChat1CallToolFunction(Sender: TObject; AiToolCall: TAiToolsFunction);
    procedure AiOpenChat1ReceiveDataEnd(const Sender: TObject; Msg: TAiOpenChatMessage; Response: TJSONObject; Role, Text: string);
  private
    Function NewQuery: TFDQuery;
  public
    { Public declarations }
  end;

var
  Form64: TForm64;

implementation

{$R *.dfm}

procedure TForm64.AiOpenChat1CallToolFunction(Sender: TObject; AiToolCall: TAiToolsFunction);
Var
  Sq: String;
  JObj: TJSONObject;
  ST: TStringStream;
  Query: TFDQuery;
begin
  If AiToolCall.Name = 'get_fecha' then
    AiToolCall.Response := FormatDateTime('YYYY/MM/DD hh:nn:ss', Now);

  If AiToolCall.Name = 'consulta_ventas' then
  Begin
    JObj := TJSONObject(TJSONObject.ParseJSONValue(AiToolCall.Arguments));

    Sq := JObj.GetValue<String>('query');

    FDQuery1.SQL.Text := Sq;

    AiToolCall.Response := 'No hay datos disponibles';

    ST := TStringStream.Create;
    Query := NewQuery;
    Query.SQL.Text := Sq;
    Query.Open;
    Query.SaveToStream(ST, TFDStorageFormat.sfJSON);
    AiToolCall.Response := ST.DataString;
    Query.Close;
    Query.Free;
  End;

end;

procedure TForm64.AiOpenChat1ReceiveDataEnd(const Sender: TObject; Msg: TAiOpenChatMessage; Response: TJSONObject; Role, Text: string);
begin
  If FDQuery1.SQL.Text.Trim <> '' then
    FDQuery1.Open;
end;

function TForm64.NewQuery: TFDQuery;
begin
  Result := TFDQuery.Create(Self);
  Result.Connection := FDConnection1;
end;

procedure TForm64.SpeedButton1Click(Sender: TObject);
Var
  ST: TStringStream;
begin
  FDQuery1.Close;
  FDQuery1.SQL.Text := '';
  Memo2.Lines.Text := AiOpenChat1.AddMessageAndRun(MemoPrompt.Text, 'user');
end;

end.
