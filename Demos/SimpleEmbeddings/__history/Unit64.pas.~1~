unit Unit64;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Edit, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo, FMX.Layouts, uOpenAi;

type
  TForm64 = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    MemoEmbedding1: TMemo;
    MemoVector1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Splitter1: TSplitter;
    Layout3: TLayout;
    MemoEmbedding2: TMemo;
    MemoVector2: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    Layout4: TLayout;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    Emb1, Emb2 : TEmbedding;
  end;

var
  Form64: TForm64;

Const
  ApiKey = 'Escriba aquí el apikey';

implementation

{$R *.fmx}

procedure TForm64.Button1Click(Sender: TObject);
Var
   Embedding : TAiEmbeddings;
begin
   Embedding := TAiEmbeddings.Create(ApiKey);
   Try
   Emb1 := Embedding.CreateEmbedding(MemoEmbedding1.Text, 'user')[0];
   MemoVector1.Text := Embedding.ToJsonArray.Format;
   Finally
     Embedding.Free;
   End;
end;

procedure TForm64.Button2Click(Sender: TObject);
Var
   Embedding : TAiEmbeddings;
begin
   Embedding := TAiEmbeddings.Create(ApiKey);
   Try
   Emb2 := Embedding.CreateEmbedding(MemoEmbedding2.Text, 'user')[0];
   MemoVector2.Text := Embedding.ToJsonArray.Format;
   Finally
     Embedding.Free;
   End;

end;

procedure TForm64.Button3Click(Sender: TObject);
Var
  F : Double;
begin
   F := TAiEmbeddings.CosineSimilarity(Emb1, Emb2);
   Edit1.Text := F.ToString;
end;

end.
