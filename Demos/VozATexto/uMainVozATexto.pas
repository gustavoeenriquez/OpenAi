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

unit uMainVozATexto;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Threading, REST.Json, System.Json,

  FMX.Media, FMX.Memo.Types, FMX.StdCtrls,
  FMX.Objects, FMX.Controls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.Types, FMX.Layouts, FMX.Forms, FMX.Graphics, FMX.Dialogs,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  uAiOpenChat, uOpenAi, uRecAudio;

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
    AniIndicator1: TAniIndicator;
    Layout4: TLayout;
    MemoResponse: TMemo;
    procedure BtnPlayClick(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  Form64: TForm64;

Const
  ApiKey = 'aquí debe ir el apikey de openai';

implementation

{$R *.fmx}
{$IFDEF MSWINDOWS}
Function ConvertSound(Source, Dest: PWideChar; Duration: Int64): Boolean; cdecl;
  external 'converttowave';
{$ENDIF}

procedure TForm64.BtnPlayClick(Sender: TObject);
Var
  FileName: String;
  FileNameUp: String;
  FPrompt: String;
  Res: String;
  St : TMemoryStream;

  Audio: TAiAudio;
begin
  FileName := 'c:\temp\miaudio.nn';
  FileNameUp := 'c:\temp\miaudio.wav';

  If FRecAudio.Status = TRecStatus.rsRecRecording then
  Begin
    FRecAudio.RecStop;
    BtnPlay.StyleLookup := 'playtoolbutton';

    ConvertSound(pWideChar(FileName), pWideChar(FileNameUp), 200);

    Audio := TAiAudio.Create(ApiKey);
    St := TMemoryStream.Create;
    St.LoadFromFile(FileNameUp);
    St.Position := 0;

    FPrompt := MemoPrompt.Text;

    Res := Audio.Transcription(St, ExtractFileName(FileNameUp), FPrompt);

    MemoResponse.Lines.Add(Res);
    FRecAudio.PlayStart;
  End
  Else
  Begin
    FRecAudio.FileName := FileName;
    FRecAudio.RecStart;
    BtnPlay.StyleLookup := 'pausetoolbutton';
  End;

end;

end.
