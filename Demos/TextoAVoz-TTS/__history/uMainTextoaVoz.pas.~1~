unit uMainTextoaVoz;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Threading, System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  uAiOpenChat, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Objects, FMX.Layouts, REST.Json, System.Json, uOpenAi, FMX.Media;

type
  TForm64 = class(TForm)
    MainLayout: TLayout;
    Layout1: TLayout;
    Layout3: TLayout;
    Rectangle1: TRectangle;
    MemoPrompt: TMemo;
    BtnPlay: TSpeedButton;
    Label1: TLabel;
    Layout4: TLayout;
    AniIndicator1: TAniIndicator;
    Image1: TImage;
    MediaPlayer1: TMediaPlayer;
    procedure BtnPlayClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form64: TForm64;

Const
  ApiKey = 'Escriba aquí el ApiKey de OpenAi';

implementation

{$R *.fmx}

procedure TForm64.BtnPlayClick(Sender: TObject);
Var
  Fprompt: String;
begin
  Fprompt := MemoPrompt.Text;

  AniIndicator1.Enabled := True;
  BtnPlay.Enabled := False;

  TThread.CreateAnonymousThread(
    Procedure
    Var
      Audio : TAiAudio;
      St : TMemoryStream;
      FileName : String;
    Begin
      Try
         Audio := TAiAudio.Create(ApiKey);
         FileName := 'c:\temp\'+System.IOUtils.TPath.GetGUIDFileName+'.mp3';

        St := Audio.Speech(FPrompt, False, 'onyx', 'mp3', 1);  //Onyx, Nova
        St.SaveToFile(FileName);


        TThread.Synchronize(Nil,
          Procedure
          Begin
             MediaPlayer1.FileName := FileName;
             MediaPlayer1.Play;

          End);
      Finally

        TThread.Synchronize(Nil,
          Procedure
          Begin
            AniIndicator1.Enabled := False;
            BtnPlay.Enabled := True;
          End);
      End;
    End).Start;
End;

end.
