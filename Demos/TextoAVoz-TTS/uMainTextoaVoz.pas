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
  ApiKey = 'Escriba aqu� el ApiKey de OpenAi';

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
