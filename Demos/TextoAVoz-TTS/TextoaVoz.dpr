program TextoaVoz;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainTextoaVoz in 'uMainTextoaVoz.pas' {Form64};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm64, Form64);
  Application.Run;
end.
