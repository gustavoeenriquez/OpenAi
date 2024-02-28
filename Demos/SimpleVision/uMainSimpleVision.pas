unit uMainSimpleVision;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.IOUtils,
  System.Variants, System.Threading, FMX.Surfaces,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  uAiOpenChat, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Objects, FMX.Layouts, REST.Json, System.Json, uOpenAi;

type
  TForm64 = class(TForm)
    MainLayout: TLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Rectangle1: TRectangle;
    MemoPrompt: TMemo;
    BtnPlay: TSpeedButton;
    Label1: TLabel;
    Image1: TImage;
    AniIndicator1: TAniIndicator;
    Layout4: TLayout;
    Splitter1: TSplitter;
    MemoResponse: TMemo;
    SpeedButton1: TSpeedButton;
    OpenDialog1: TOpenDialog;
    procedure BtnPlayClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
  end;

var
  Form64: TForm64;

Const
  ApiKey = 'Escriba aquí la ApiKey de OpenAI';

implementation

{$R *.fmx}

procedure TForm64.BtnPlayClick(Sender: TObject);
Var
  FPrompt : String;
begin

  Fprompt := MemoPrompt.Text;

  AniIndicator1.Enabled := True;
  AniIndicator1.Visible := True;
  BtnPlay.Enabled := False;

  TThread.CreateAnonymousThread(
    Procedure
    Var
       Img : TAiVision;
       Res : String;
       St : TMemoryStream;
    Begin
      Try

        Img := TAiVision.Create(ApiKey);
        St := TMemoryStream.Create;

        Image1.Bitmap.SaveToStream(St);
        St.Position := 0;

        Res := Img.GenerateByStream(FPrompt, St, 'user', 1000, True);

        TThread.Synchronize(Nil,
          Procedure
          Begin
            MemoResponse.Text := Res;
            AniIndicator1.Enabled := False;
            BtnPlay.Enabled := True;
          End);
      Finally

        TThread.Synchronize(Nil,
          Procedure
          Begin
            AniIndicator1.Enabled := False;
            AniIndicator1.Visible := False;
            BtnPlay.Enabled := True;
          End);
      End;
    End).Start;
End;




procedure TForm64.SpeedButton1Click(Sender: TObject);
begin
   If OpenDialog1.Execute then
      Image1.Bitmap.LoadFromFile(OpenDialog1.FileName);
end;

end.
