unit uFrmEditMemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.JSON, Rest.JSON,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, FMX.Objects, FMX.Controls.Presentation,
  FMX.Layouts;

type
  TFFrmEditMemo = class(TForm)
    MainLayout: TLayout;
    Layout1: TLayout;
    BtnCancel: TButton;
    Image1: TImage;
    Label1: TLabel;
    BtnOk: TButton;
    Image2: TImage;
    Label2: TLabel;
    MemoEdit: TMemo;
    procedure BtnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FFrmEditMemo: TFFrmEditMemo;

implementation

{$R *.fmx}

procedure TFFrmEditMemo.BtnOkClick(Sender: TObject);
Var
  JObj: TJSonObject;
begin
  If Caption = 'Editar Funciones' then
  Begin
    Try
      JObj := TJSonObject(TJSonObject.ParseJSONValue(MemoEdit.Text));
      If Assigned(JObj) then
      Begin
        ModalResult := mrOk;
      End;
    Except
      On E: Exception do
      Begin
        ShowMessage('El formato del json no es válido');
      End;
    End;
  End
  Else
  Begin
    ModalResult := mrOk;
  End
end;

end.
