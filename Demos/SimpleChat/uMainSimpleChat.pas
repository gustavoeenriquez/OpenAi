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

unit uMainSimpleChat;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  uAiOpenChat, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Objects, FMX.Layouts, REST.Json, System.Json;

type
  TForm64 = class(TForm)
    MainLayout: TLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Rectangle1: TRectangle;
    MemoPrompt: TMemo;
    BtnPlay: TSpeedButton;
    MemoChat: TMemo;
    Label1: TLabel;
    AiOpenChat1: TAiOpenChat;
    procedure AiOpenChat1ReceiveData(const Sender: TObject;
      Msg: TAiOpenChatMessage; Response: TJSONObject; Role, Text: string);
    procedure BtnPlayClick(Sender: TObject);
    procedure AiOpenChat1ReceiveDataEnd(const Sender: TObject;
      Msg: TAiOpenChatMessage; Response: TJSONObject; Role, Text: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form64: TForm64;

implementation

{$R *.fmx}

procedure TForm64.AiOpenChat1ReceiveData(const Sender: TObject;
  Msg: TAiOpenChatMessage; Response: TJSONObject; Role, Text: string);
begin
  MemoChat.BeginUpdate;
  Try
    MemoChat.Lines.Text := MemoChat.Lines.Text + Text;
    MemoChat.SelStart := Length(MemoChat.Text);
  Finally
    MemoChat.EndUpdate;
  End;
end;

procedure TForm64.AiOpenChat1ReceiveDataEnd(const Sender: TObject;
  Msg: TAiOpenChatMessage; Response: TJSONObject; Role, Text: string);
begin
    BtnPlay.StyleLookup := 'playtoolbutton';
end;

procedure TForm64.BtnPlayClick(Sender: TObject);
begin
  If AiOpenChat1.Busy and AiOpenChat1.Asynchronous then
    AiOpenChat1.Abort
  Else
  Begin
    AiOpenChat1.AddMessageAndRun(MemoPrompt.Text, 'user');
    BtnPlay.StyleLookup := 'pausetoolbutton';
  End;
end;

end.
