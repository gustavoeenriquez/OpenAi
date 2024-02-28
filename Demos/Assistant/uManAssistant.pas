unit uManAssistant;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Objects, FMX.Controls.Presentation, FMX.Layouts, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.ListBox;

type
  TFManAssistant = class(TForm)
    MainLayout: TLayout;
    Layout1: TLayout;
    BtnCancel: TButton;
    Image1: TImage;
    Label1: TLabel;
    BtnOk: TButton;
    Image2: TImage;
    Label2: TLabel;
    EditModelo: TComboBox;
    Label4: TLabel;
    EditNombreAsistente: TEdit;
    Label3: TLabel;
    ChRetrieval: TCheckBox;
    ChCodeInterpreter: TCheckBox;
    MemoInstrucciones: TMemo;
    Label5: TLabel;
    MemoFunciones: TMemo;
    Label6: TLabel;
    BtnEditInstrucciones: TSpeedButton;
    Image4: TImage;
    BtnEditFunciones: TSpeedButton;
    Image3: TImage;
    procedure BtnEditFuncionesClick(Sender: TObject);
    procedure BtnEditInstruccionesClick(Sender: TObject);
    procedure ChRetrievalChange(Sender: TObject);
    procedure EditNombreAsistenteTyping(Sender: TObject);
  private
    Procedure UpdateGUI;
  public
    { Public declarations }
  end;

var
  FManAssistant: TFManAssistant;

implementation

{$R *.fmx}

uses uFrmEditMemo;

procedure TFManAssistant.BtnEditFuncionesClick(Sender: TObject);
Var
  FEditMemo: TFFrmEditMemo;
begin
  FEditMemo := TFFrmEditMemo.Create(Self);
  FEditMemo.MemoEdit.Text := MemoFunciones.Text;
  FEditMemo.Caption := 'Editar Funciones';

  If FEditMemo.ShowModal = mrOk then
  Begin
    MemoFunciones.Text := FEditMemo.MemoEdit.Text;
  End;

  FEditMemo.Free;
end;

procedure TFManAssistant.BtnEditInstruccionesClick(Sender: TObject);
Var
  FEditMemo: TFFrmEditMemo;
begin
  FEditMemo := TFFrmEditMemo.Create(Self);
  FEditMemo.MemoEdit.Text := MemoInstrucciones.Text;
  FEditMemo.Caption := 'Editar Instrucciones';

  If FEditMemo.ShowModal = mrOk then
  Begin
    MemoInstrucciones.Text := FEditMemo.MemoEdit.Text;
  End;

  FEditMemo.Free;
end;

procedure TFManAssistant.ChRetrievalChange(Sender: TObject);
begin
  UpdateGUI;
end;

procedure TFManAssistant.EditNombreAsistenteTyping(Sender: TObject);
begin
  UpdateGUI
end;

procedure TFManAssistant.UpdateGUI;
begin
  MemoFunciones.Enabled := ChRetrieval.IsChecked or ChCodeInterpreter.IsChecked;
  BtnOk.Enabled := (EditModelo.ItemIndex >= 0) and (EditNombreAsistente.Text.Trim <> '');
end;

end.
