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
