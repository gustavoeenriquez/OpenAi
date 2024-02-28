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
  ApiKey = 'Escriba aquí el ApiKey de OpenAi';

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
