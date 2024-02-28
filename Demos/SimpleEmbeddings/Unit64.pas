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
//Nombre: Gustavo Enr�quez
//  Redes Sociales:
//- Email: gustavoeenriquez@gmail.com
//  - Telegram: +57 3128441700
//  - LinkedIn: https://www.linkedin.com/in/gustavo-enriquez-3937654a/
//  - Youtube: https://www.youtube.com/@cimamaker3945
//  - GitHub: https://github.com/gustavoeenriquez/
//
// puedes ver el video del funcionamiento de este demo en https://www.youtube.com/@cimamaker3945


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
  ApiKey = 'Escriba aqu� el apikey';

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
