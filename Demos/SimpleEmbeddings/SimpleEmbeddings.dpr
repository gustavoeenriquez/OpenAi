program SimpleEmbeddings;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit64 in 'Unit64.pas' {Form64};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm64, Form64);
  Application.Run;
end.
