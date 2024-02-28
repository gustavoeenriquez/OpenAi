program AssistanDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainAssistant in 'uMainAssistant.pas' {MainAssistant},
  uDbModule in 'uDbModule.pas' {DbModule: TDataModule},
  uManAssistant in 'uManAssistant.pas' {FManAssistant},
  uFrmEditMemo in 'uFrmEditMemo.pas' {FFrmEditMemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainAssistant, MainAssistant);
  Application.Run;
end.
