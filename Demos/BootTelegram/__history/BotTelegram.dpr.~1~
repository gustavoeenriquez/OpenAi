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
