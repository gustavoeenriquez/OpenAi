program VozATexto;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainVozATexto in 'uMainVozATexto.pas' {Form64};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm64, Form64);
  Application.Run;
end.
