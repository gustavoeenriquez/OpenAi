unit uImageEdit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Permissions, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Gestures, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Objects, FMX.Controls.Presentation,
  FMX.DialogService, FMX.Layouts, System.Actions, FMX.ActnList, FMX.StdActns,
  FMX.MediaLibrary.Actions, FMX.Platform, FMX.MediaLibrary,
  MobilePermissions.Model.Signature, MobilePermissions.Model.Dangerous,
  MobilePermissions.Model.Standard, MobilePermissions.Component, uFormulariosListBox,
  FMX.ListBox, FMX.Colors, FMX.TabControl, FMX.Media, uRecAudio;

type

  TImMode = (TImmNone, tImmEdit);
  TImOpcion = (TImoNone, tImoPain, tImoErase);

  TFImageEdit = class(TForm)
    MainLayout: TLayout;
    Layout2: TLayout;
    BtnContinuar: TButton;
    Image2: TImage;
    Label1: TLabel;
    BtnAyuda: TButton;
    Image1: TImage;
    LblAskToIA: TLabel;
    LayoutModelo: TLayout;
    LblModelo: TLabel;
    MemoModelo: TMemo;
    LayoutTexto: TLayout;
    Splitter1: TSplitter;
    ImagenEditar: TImage;
    alGetFromCamera: TActionList;
    TakePhotoFromCameraAction1: TTakePhotoFromCameraAction;
    MobilePermissions1: TMobilePermissions;
    TakePhotoFromLibraryAction1: TTakePhotoFromLibraryAction;
    Layout1: TLayout;
    Rectangle1: TRectangle;
    BtnOpenDialog: TButton;
    Image3: TImage;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    BtnSaveDialog: TButton;
    Image4: TImage;
    BtnShare: TButton;
    Image5: TImage;
    ShowShareSheetAction1: TShowShareSheetAction;
    BtnOpenVideo: TButton;
    Image6: TImage;
    Button2: TButton;
    Image8: TImage;
    Button1: TButton;
    Image7: TImage;
    Layout3: TLayout;
    BtnGenereImagen: TButton;
    Image9: TImage;
    Label2: TLabel;
    EditTamano: TComboBox;
    RectImage: TRectangle;
    AniIndicator1: TAniIndicator;
    TabMainEdit: TTabControl;
    TabItemEdit: TTabItem;
    TabItemPaint: TTabItem;
    Button3: TButton;
    Image10: TImage;
    Button4: TButton;
    Image11: TImage;
    BtnBorrado: TSpeedButton;
    Image12: TImage;
    BtnPintar: TSpeedButton;
    Image13: TImage;
    BoxPenWidth: TComboBox;
    ColorBox1: TComboColorBox;
    Button7: TButton;
    Image14: TImage;
    RectOperar: TRectangle;
    APoint1: TEllipse;
    ListBoxMenu: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    TabItemEffect: TTabItem;
    Selection1: TSelection;
    MediaPlayer1: TMediaPlayer;
    Image16: TImage;
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure TakePhotoFromCameraAction1DidFinishTaking(Image: TBitmap);
    procedure FormCreate(Sender: TObject);
    procedure BtnAyudaClick(Sender: TObject);
    procedure TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
    procedure BtnContinuarClick(Sender: TObject);
    procedure BtnOpenDialogClick(Sender: TObject);
    procedure BtnSaveDialogClick(Sender: TObject);
    procedure ShowShareSheetAction1BeforeExecute(Sender: TObject);
    procedure BtnShareClick(Sender: TObject);
    procedure BtnOpenVideoClick(Sender: TObject);
    procedure EditTamanoChange(Sender: TObject);
    procedure BtnGenereImagenClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RectOperarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure RectOperarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure RectOperarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Button7Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure BtnBorradoClick(Sender: TObject);
    procedure BoxPenWidthChange(Sender: TObject);
    procedure ListBoxItem1Click(Sender: TObject);
    procedure ListBoxItem2Click(Sender: TObject);
    procedure ListBoxItem3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RectImageResized(Sender: TObject);
    procedure Rect1DragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure Rect2DragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
    procedure Image16MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Image16MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  private
    FBm: TBitmap;
    FBmData, FBMWriteData: TBitmapData;
    FPIni, FMouseIni, FMouseFin: TPointF;
    FZoom: Double;
    FIsMouseDown: Boolean;
    FPenWith: Integer;

    FMode: TImMode; // = (TImmNone, tImmEdit);
    FOpcion: TImOpcion; // = (TImoNone, tImoPain, tImoErase);

    FBmStack: TList<TBitmap>;
    FLastImage: Boolean;
    FIndicador: Boolean;

    procedure DrawLineBresenham(x1, y1, x2, y2, r: Integer);
    Procedure AjusteEscala(aRTar, aROrg: TRectF; Out aROut: TRectF; Out aZoom: Double);
    Procedure EscaleImagen;
    Procedure SetModoBorrado(Activo: Boolean);

    Procedure ImagePush(BM: TBitmap);
    Function ImagePop: TBitmap;
    Procedure ImageUndo;
    Procedure ImageRotar;

    Procedure CargarImagen(BM: TBitmap);
    procedure SetIndicador(const Value: Boolean);

  public
    ItemEdit: TFrmItemEdit;
    ImgTamano: String;
    Procedure InitData;
    Function GetImageFinal: TBitmap;

    Procedure InitAudioRec;
    Procedure OnChangeRecState(Sender: TObject; Status: TRecStatus);
    Procedure GetIATexto;
    procedure EjecuteProcConEspera(Proc: TProc);
    Procedure GenereImagen;

    Property Indicador: Boolean read FIndicador write SetIndicador;

  end;

var
  FImageEdit: TFImageEdit;
  FLastAngle: Double;
  FLastDistance: Integer;
  FImgW, FImgH: Single;

implementation

{$R *.fmx}

uses
  System.Math, uDataModuleApp, UWebFiles;

procedure TFImageEdit.AjusteEscala(aRTar, aROrg: TRectF; out aROut: TRectF; out aZoom: Double);
Var
  MW, MH: Double;
begin
  aZoom := Min(aRTar.Width / aROrg.Width, aRTar.Height / aROrg.Height);

  aROut.Width := aROrg.Width * aZoom;
  aROut.Height := aROrg.Height * aZoom;

  MW := (aRTar.Width - aROut.Width) / 2;
  MH := (aRTar.Height - aROut.Height) / 2;

  aROut.Offset(MW, MH);

  RectOperar.BoundsRect := aROut;

end;

procedure TFImageEdit.BoxPenWidthChange(Sender: TObject);
begin
  FPenWith := StrToInt(BoxPenWidth.Items[BoxPenWidth.ItemIndex]);
  APoint1.Width := FPenWith;
  APoint1.Height := FPenWith;
end;

procedure TFImageEdit.BtnAyudaClick(Sender: TObject);
begin
  If LayoutModelo.Visible = False then
  Begin

    TDialogService.MessageDialog(('La ayuda tendrá un costo de hasta $500 pesos por cada solicitud, Desea continuar?'), System.UITypes.TMsgDlgType.mtConfirmation, [System.UITypes.TMsgDlgBtn.mbYes, System.UITypes.TMsgDlgBtn.mbNo],
      System.UITypes.TMsgDlgBtn.mbYes, 0,
      procedure(const AResult: System.UITypes.TModalResult)
      begin
        case AResult of
          mrYES:
            begin
              LayoutModelo.Visible := True;
              // MemoModelo.Lines.Text := Memo1.Lines.Text;
              // Memo1.Lines.Text := '';
              LblAskToIA.Text := 'Ocultar Ayuda';
              Splitter1.Height := 20;
              TDialogService.ShowMessage('Edite el texto y presione nuevamente el botón para generar la imágen');
            end;
        end;
      end);

  End
  Else
  Begin
    LblAskToIA.Text := 'Ayuda';
    LayoutModelo.Visible := False;
    Splitter1.Height := 3;
  End;
end;

procedure TFImageEdit.BtnBorradoClick(Sender: TObject);
begin
  If BtnBorrado.IsPressed then
  Begin
    FOpcion := TImOpcion.tImoErase; // = (TImoNone, tImoPain, tImoErase);
    SetModoBorrado(BtnBorrado.IsPressed);
  End
  Else If BtnPintar.IsPressed then
    FOpcion := TImOpcion.tImoPain // = (TImoNone, tImoPain, tImoErase);
  Else
    FOpcion := TImOpcion.TImoNone; // = (TImoNone, tImoPain, tImoErase);
end;

procedure TFImageEdit.BtnContinuarClick(Sender: TObject);
Var
  BM: TBitmap;
begin
  BM := GetImageFinal;

  If ItemEdit.Objeto is TImage then
  Begin
    ItemEdit.Modificado := True;
    TImage(ItemEdit.Objeto).Bitmap.Assign(BM);
  End;
  Close;
end;

procedure TFImageEdit.BtnGenereImagenClick(Sender: TObject);
Begin
  GenereImagen;
End;

Procedure TFImageEdit.GenereImagen;
begin
  // FMainAppEmp.Indicador := True;
  // MainLayout.Enabled := False;
  AniIndicator1.Visible := True;
  AniIndicator1.Enabled := True;

  TThread.CreateAnonymousThread(
    procedure
    Var
      BM: TBitmap;
    begin
      Try
        Try
          MainLayout.Enabled := False;
          BM := FDM.AskToIAImage(ImgTamano, MemoModelo.Lines.Text);
          If Assigned(BM) then
          Begin
            TThread.Synchronize(nil,
              procedure
              begin

                CargarImagen(BM);
                MainLayout.Enabled := True;
                // FMainAppEmp.Indicador := False;
                AniIndicator1.Visible := False;
                AniIndicator1.Enabled := False;
              end);
          End;
        Except
          On E: Exception do
          Begin
            TThread.Synchronize(nil,
              procedure
              begin
                TDialogService.ShowMessage(E.Message);
              end);
          End;
        End;
      Finally
        MainLayout.Enabled := True;
        // FMainAppEmp.Indicador := False;
        AniIndicator1.Visible := False;
        AniIndicator1.Enabled := False;
      End;
    end).Start;
end;

procedure TFImageEdit.BtnOpenDialogClick(Sender: TObject);
Var
  BM: TBitmap;
begin
{$IFDEF MSWINDOWS}
  If OpenDialog1.Execute then
  Begin
    BM := TBitmap.Create;
    BM.LoadFromFile(OpenDialog1.FileName);
    CargarImagen(BM);
    BM.Free;
  End;
{$ELSE}
  TakePhotoFromLibraryAction1.Execute;
{$ENDIF}
end;

procedure TFImageEdit.BtnOpenVideoClick(Sender: TObject);
begin
{$IFDEF ANDROID}
  TakePhotoFromCameraAction1.Execute;
{$ENDIF}
end;

procedure TFImageEdit.BtnSaveDialogClick(Sender: TObject);
Var
  Service: IFMXPhotoLibrary;
  BM: TBitmap;
begin
{$IFDEF MSWINDOWS}
  If SaveDialog1.Execute then
  Begin
    FBm.SaveToFile(SaveDialog1.FileName);
  End;
{$ELSE}
{$IFDEF ANDROID}
  if TPlatformServices.Current.SupportsPlatformService(IFMXPhotoLibrary, Service) then
    Service.AddImageToSavedPhotosAlbum(FBm);

  TDialogService.ShowMessage('La imágen se guardó en la galería');
{$ENDIF}
{$ENDIF}
end;

procedure TFImageEdit.BtnShareClick(Sender: TObject);
begin
  ShowShareSheetAction1.Execute;
end;

procedure TFImageEdit.Button1Click(Sender: TObject);
begin
  ListBoxMenu.Visible := True;
end;

procedure TFImageEdit.Button3Click(Sender: TObject);
begin
  ImageUndo;
end;

procedure TFImageEdit.Button4Click(Sender: TObject);
begin
  ImageRotar;
end;

procedure TFImageEdit.Button7Click(Sender: TObject);
begin
  ImageUndo;
end;

procedure TFImageEdit.CargarImagen(BM: TBitmap);
Var
  RTar: TRectF;
begin
  FBmStack.Clear;

  FBm.Width := BM.Width;
  FBm.Height := BM.Height;
  FBm.CopyFromBitmap(BM);

  ImagePush(FBm);

  ImagenEditar.Bitmap.Assign(FBm);
  ImagenEditar.RotationAngle := 0;
  AjusteEscala(RectImage.BoundsRect, FBm.BoundsF, RTar, FZoom);

  // RectOperar.BoundsRect := RTar;
  RectOperar.Visible := True;
end;

procedure TFImageEdit.DrawLineBresenham(x1, y1, x2, y2, r: Integer);
var
  dx, dy, incE, incNE, d, X, Y: Integer;
  I, J: Integer;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  d := 2 * dy - dx;
  incE := 2 * dy;
  incNE := 2 * (dy - dx);
  X := x1;
  Y := y1;

  FBMWriteData.SetPixel(X, Y, 0);

  for I := X - r to X + r do
  begin
    for J := Y - r to Y + r do
    begin
      if (I >= 0) and (J >= 0) and (I < FBm.Width) and (J < FBm.Height) then
      begin
        if (Round(Sqrt(Power(X - I, 2) + Power(Y - J, 2))) <= r) then
          FBMWriteData.SetPixel(I, J, 0);
      end;
    end;
  end;

  while X < x2 do
  begin
    if d <= 0 then
    begin
      d := d + incE;
      X := X + 1;
    end
    else
    begin
      d := d + incNE;
      X := X + 1;
      Y := Y + 1;
    end;
    // Bitmap.Canvas.Pixels[x, y] := clBlack;
    FBMWriteData.SetPixel(X, Y, 0);
  end;
end;

procedure TFImageEdit.EditTamanoChange(Sender: TObject);
begin
  Case EditTamano.ItemIndex of
    0:
      Begin
        ImgTamano := '256x256';
        TDialogService.ShowMessage('Las imágenes de 256x256 tienen un costo de $300 por generación');
      end;
    1:
      Begin
        ImgTamano := '512x512';
        TDialogService.ShowMessage('Las imágenes de 512x512 tienen un costo de $400 por generación');
      end;
    2:
      Begin
        ImgTamano := '1024x1024';
        TDialogService.ShowMessage('Las imágenes de 1024x1024 tienen un costo de $500 por generación');
      end;
  End;
end;

procedure TFImageEdit.EjecuteProcConEspera(Proc: TProc);
var
  Thread: TThread;
begin
  Indicador := True;
  Thread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        Sleep(200);
        TThread.Synchronize(nil,
          procedure
          begin
            try
              Proc;
              // Aquí va todo lo visual
            finally
              Indicador := False;
            end;
          end);
      except
        On E: Exception do
        begin
          // ****** En caso de error *************
          TThread.Synchronize(nil,
            procedure
            begin
              Indicador := False;
              Raise Exception.Create(E.Message)
            end);
        end;
      end;
    end);

  Thread.Start;
end;

procedure TFImageEdit.EscaleImagen;
Var
  RTar: TRectF;
begin
  If Assigned(FBm) and (FBm.Width > 0) then
    AjusteEscala(RectImage.BoundsRect, FBm.BoundsF, RTar, FZoom);
end;

procedure TFImageEdit.FormActivate(Sender: TObject);
Var
  H, W, L: Single;
begin

  H := LayoutTexto.Height;
  W := LayoutTexto.Width;

  If H < W then
    L := H
  Else
    L := W;

  L := L - 15;

  ImagenEditar.Width := L;
  ImagenEditar.Height := L;

  FImgW := ImagenEditar.Width;
  FImgH := ImagenEditar.Height;

end;

procedure TFImageEdit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.CaFree;
end;

procedure TFImageEdit.FormCreate(Sender: TObject);
Var
  RTar, ROrg: TRectF;
  MW, MH: Double;
  BM: TBitmap;
begin
  LayoutModelo.Visible := False;
  RectOperar.Visible := False;
  ListBoxMenu.Visible := False;

  ImgTamano := '256x256';
  MobilePermissions1.Dangerous.ReadExternalStorage := True;
  MobilePermissions1.Dangerous.WriteExternalStorage := True;
  MobilePermissions1.Dangerous.Camera := True;
  MobilePermissions1.Apply;

  FPenWith := 8;
  BoxPenWidth.ItemIndex := 7;
  ColorBox1.Color := TAlphaColors.Red;

  FMode := TImMode.TImmNone; // = (TImmNone, tImmEdit);
  FOpcion := TImOpcion.TImoNone; // = (TImoNone, tImoPain, tImoErase);

  FBmStack := TList<TBitmap>.Create;
  FBm := TBitmap.Create;

  InitAudioRec;

end;

procedure TFImageEdit.FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  LObj: IControl;
  LImage: TImage;
  LImageCenter: TPointF;
begin
  {
    if EventInfo.GestureID = igiRotate then
    begin
    LObj := Self.ObjectAtPoint(ClientToScreen(EventInfo.Location));
    if LObj is TImage then
    begin
    LImage := TImage(LObj.GetObject);
    if (TInteractiveGestureFlag.gfBegin in EventInfo.Flags) then
    FLastAngle := LImage.RotationAngle
    else if EventInfo.Angle <> 0 then
    LImage.RotationAngle := FLastAngle - (EventInfo.Angle * 180) / Pi;
    end;
    end;
  }

  if EventInfo.GestureID = igiZoom then
  begin
    LObj := Self.ObjectAtPoint(ClientToScreen(EventInfo.Location));
    if LObj is TImage then
    begin
      if (not(TInteractiveGestureFlag.gfBegin in EventInfo.Flags)) and (not(TInteractiveGestureFlag.gfEnd in EventInfo.Flags)) then
      begin
        { zoom the image }
        LImage := TImage(LObj.GetObject);
        LImageCenter := LImage.Position.Point + PointF(LImage.Width / 2, LImage.Height / 2);
        LImage.Width := Max(LImage.Width + (EventInfo.Distance - FLastDistance), 10);
        LImage.Height := Max(LImage.Height + (EventInfo.Distance - FLastDistance), 10);
        LImage.Position.X := LImageCenter.X - LImage.Width / 2;
        LImage.Position.Y := LImageCenter.Y - LImage.Height / 2;
      end;
      FLastDistance := EventInfo.Distance;
    end;
  end;
end;

function TFImageEdit.GetImageFinal: TBitmap;
begin
  Result := ImagenEditar.Bitmap

  // Aquí debe hacer todas las modificaciones a la imágen para reenviarla
  // Result := TBitMap.Create;
  // Result.Assign(ImagenEditar.Bitmap);
end;

procedure TFImageEdit.Image16MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FDM.PlayBeep(TPlayBeeb.pbBeep);
  Sleep(300);
  FRecAudio.RecStart;
end;

procedure TFImageEdit.Image16MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FRecAudio.RecStop;
  FRecAudio.PlayStart;

  If FRecAudio.Duration > 2 then
  Begin
    FDM.PlayBeep(TPlayBeeb.pbBeep);
    Sleep(300);
    GetIATexto;
  End;
end;

function TFImageEdit.ImagePop: TBitmap;
begin
  Result := Nil;

  If FLastImage = True then
  Begin
    If FBmStack.Count > 1 then
      FBmStack.Delete(0);
    FLastImage := False;
  End;

  If FBmStack.Count > 0 then
  Begin
    Result := FBmStack[0];
    FBmStack.Delete(0);
  End;
end;

procedure TFImageEdit.ImagePush(BM: TBitmap);
Var
  BM1: TBitmap;
begin
  BM1 := TBitmap.Create(BM.Width, BM.Height);
  BM1.Canvas.BeginScene;
  Try
    BM1.Canvas.CopyBitmap(BM, BM1);

  Finally
    BM1.Canvas.EndScene;
  End;
  FBmStack.Insert(0, BM1);
  FLastImage := True;
end;

procedure TFImageEdit.ImageRotar;
Var
  RTar: TRectF;
begin
  FBm.Rotate(270);
  AjusteEscala(RectImage.BoundsRect, FBm.BoundsF, RTar, FZoom);
  ImagenEditar.Bitmap.Assign(FBm);
end;

procedure TFImageEdit.ImageUndo;
Var
  r: TRectF;
  BM: TBitmap;
begin
  If FBmStack.Count > 0 then
  Begin
    BM := ImagePop;

    r := BM.BoundsF;
    FBm.Canvas.BeginScene;
    Try
      FBm.Canvas.DrawBitmap(BM, r, r, 1);
    Finally
      FBm.Canvas.EndScene;
    End;
    If FBmStack.Count = 0 then
    Begin
      ImagePush(BM);
    End
    Else
      BM.Free;
    ImagenEditar.Bitmap.Clear(TAlphaColors.White);
    ImagenEditar.Bitmap.Assign(FBm);
  End;
end;

procedure TFImageEdit.InitData;
begin
  If Assigned(ItemEdit) and (ItemEdit.Objeto is TImage) then
    ImagenEditar.Bitmap.Assign(TImage(ItemEdit.Objeto).Bitmap);
end;

procedure TFImageEdit.ListBoxItem1Click(Sender: TObject);
begin
  TabMainEdit.ActiveTab := TabItemEdit;
  ListBoxMenu.Visible := False;
end;

procedure TFImageEdit.ListBoxItem2Click(Sender: TObject);
begin
  TabMainEdit.ActiveTab := TabItemPaint;
  ListBoxMenu.Visible := False;
end;

procedure TFImageEdit.ListBoxItem3Click(Sender: TObject);
begin
  TabMainEdit.ActiveTab := TabItemEffect;
  ListBoxMenu.Visible := False;
end;

procedure TFImageEdit.Rect1DragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  Operation := TDragOperation.Move;
end;

procedure TFImageEdit.Rect2DragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  Operation := TDragOperation.Move;
end;

procedure TFImageEdit.RectImageResized(Sender: TObject);
begin
  EscaleImagen;
end;

procedure TFImageEdit.RectOperarMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FIsMouseDown := True;
  FMouseIni := TPointF.Create(X, Y);
end;

procedure TFImageEdit.RectOperarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
Var
  FP1, FP2, Pix: TPointF;
  Color: TAlphaColor;
begin
  FMouseIni := TPointF.Create(X, Y);

  APoint1.Position.X := X;
  APoint1.Position.Y := Y;
  Pix := TPointF.Create(X * (1 / FZoom), Y * (1 / FZoom));
  FP1 := FMouseIni * (1 / FZoom);
  FP2 := TPointF.Create(X, Y) * (1 / FZoom);

  // LblPosition.Text := FormatFloat('#0', X) + ':' + FormatFloat('#0', Y) + ' -- ' + Format('%x', [Color]);

  If FIsMouseDown then
  Begin
    Try
      Case FOpcion of
        TImOpcion.tImoErase:
          Begin

            Color := FBmData.GetPixel(Round(Pix.X), Round(Pix.Y));
            FBm.Map(TMapAccess.Write, FBMWriteData);
            DrawLineBresenham(Round(FP1.X), Round(FP1.Y), Round(FP2.X), Round(FP2.Y), FPenWith);
            FBm.Unmap(FBMWriteData);

          End;
        TImOpcion.tImoPain:
          Begin
            FBm.Canvas.BeginScene;
            Try
              FBm.Canvas.Stroke.Color := ColorBox1.Color;
              FBm.Canvas.Stroke.Thickness := FPenWith;
              FBm.Canvas.DrawLine(FP1, FP2, 1);
            Finally
              FBm.Canvas.EndScene;
            End;

          End;

      End;

      ImagenEditar.Bitmap.Assign(FBm);
      FMouseIni := TPointF.Create(X, Y);
    Finally
    End;
  End;
end;

procedure TFImageEdit.RectOperarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FIsMouseDown := False;

  If FOpcion <> TImOpcion.TImoNone then
  Begin
    ImagePush(FBm);
  End;
end;

procedure TFImageEdit.SetIndicador(const Value: Boolean);
begin
  FIndicador := Value;
  AniIndicator1.Enabled := FIndicador;
  AniIndicator1.Visible := FIndicador;
end;

procedure TFImageEdit.SetModoBorrado(Activo: Boolean);
begin
  If Activo then
  Begin
    FBm.Map(TMapAccess.Read, FBmData);
  End
  Else
  Begin
    FBm.Unmap(FBmData);
  End;
end;

procedure TFImageEdit.ShowShareSheetAction1BeforeExecute(Sender: TObject);
begin
  ShowShareSheetAction1.Bitmap.Assign(ImagenEditar.Bitmap);
end;

procedure TFImageEdit.TakePhotoFromCameraAction1DidFinishTaking(Image: TBitmap);
begin
  CargarImagen(Image);
end;

procedure TFImageEdit.TakePhotoFromLibraryAction1DidFinishTaking(Image: TBitmap);
begin
  CargarImagen(Image);
end;

procedure TFImageEdit.InitAudioRec;
Begin
  Try
    FRecAudio := TRecAudio.Create(Self);
    FRecAudio.OnEvent := OnChangeRecState;
  Except
    On E: Exception do
      ShowMessage(E.Message);
  End;
End;

procedure TFImageEdit.OnChangeRecState(Sender: TObject; Status: TRecStatus);
Begin

  case Status of
    rsNone:
      Begin
      end;
    rsRecRecording:
      Begin
      end;
    rsRecStop:
      Begin
      end;
    rsPlaying:
      Begin
      end;
    rsPlayPause:
      Begin
      end;
    rsPlayStop:
      Begin
      end;
  end;
End;

procedure TFImageEdit.GetIATexto;
Begin
  EjecuteProcConEspera(
    Procedure
    Var
      FileName, STT, Texto: String;
      St: TMemoryStream;
    Begin
      FileName := FRecAudio.FileName;
      St := TMemoryStream.Create;
      Try
        St.LoadFromFile(FileName);
        Texto := FDM.SpeechToText(St, FileName);
        MemoModelo.Lines.Text := Texto;
        GenereImagen;
      Finally
        St.Free;
      End;
    End);

End;

end.
