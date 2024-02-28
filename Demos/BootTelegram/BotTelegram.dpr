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
//
// puedes ver el video del funcionamiento de este demo en https://www.youtube.com/@cimamaker3945

program BotTelegram;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  fastTelega.AvailableTypes,
  fastTelega.Bot,
  fastTelega.LongPoll,
  FMX.Graphics,
  UOpenAi,
  UAiOpenChat,
  uDbModule in 'uDbModule.pas' {DbModule: TDataModule};

Var
  Bot: TFTBot;
  LongPoll: TFTLongPoll;
  Bm: TBitMap;
  DbModule: TDBModule;


begin
  Bm := TBitMap.Create;
  Bm.SetSize(100, 100);
  DbModule := TDBModule.Create(Nil); //

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
        Res: String;
      begin
        Msg := TftMessage(FTMessage);

        Res := DbModule.Chat1.AddMessageAndRun(Msg.text, 'user');
        Bot.API.sendMessage(Msg.Chat.id, Res);
      end);

    Writeln('El Nombre del bot es: ' + Bot.API.getMe.username);
    Bot.API.deleteWebhook();

    LongPoll := TFTLongPoll.Create(Bot);
    while (True) do
    begin
      Writeln('Long poll iniciado');
      LongPoll.start();
    end

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
