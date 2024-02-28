program SimpleVisionproj;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainSimpleVision in 'uMainSimpleVision.pas' {Form64};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm64, Form64);
  Application.Run;
end.
