unit uFormPuzzle15;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Objects, FMX.Effects, FMX.Filter.Effects, FMX.Ani, Math,
  FMX.Controls.Presentation;

type
  TFormPuzzle15 = class(TForm)
    PanelClient: TPanel;
    Rectangle1: TRectangle;
    RecText: TText;
    RecGradientAni: TGradientAnimation;
    PanelMenuCl: TRectangle;
    MenuFloatAnimation: TFloatAnimation;
    GridPanelLayout1: TGridPanelLayout;
    Rectangle33: TRectangle;
    Text33: TText;
    TimerCreateRectangles: TTimer;
    ColorAnimation33: TColorAnimation;
    ColorAnimation4: TColorAnimation;
    Rectangle44: TRectangle;
    Text44: TText;
    ColorAnimation5: TColorAnimation;
    ColorAnimation6: TColorAnimation;
    Rectangle55: TRectangle;
    Text55: TText;
    ColorAnimation55: TColorAnimation;
    ColorAnimation3: TColorAnimation;
    RectangleClose: TRectangle;
    RectangleCloseColorAni: TColorAnimation;
    CloseGloomEffect: TGloomEffect;
    CloseGloomEffectAni: TFloatAnimation;
    ImageClose: TImage;
    PanelTop: TRectangle;
    LayoutCenter: TLayout;
    RectangleShuffle: TRectangle;
    ImageShuffle: TImage;
    ShuffleColorAnimation: TColorAnimation;
    ShuffleGloomEffect: TGloomEffect;
    ShuffleGloomEffectAni: TFloatAnimation;
    RectangleMenu: TRectangle;
    ImageMenu: TImage;
    ColorAnimation2: TColorAnimation;
    GloomEffect4: TGloomEffect;
    FloatAnimation4: TFloatAnimation;
    RectangleTime: TRectangle;
    TextTime: TText;
    TimerReShuffle: TTimer;
    TimerResize: TTimer;
    TimerTime: TTimer;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Panel1: TPanel;
    procedure ButtonCloneClick(Sender: TObject);
    procedure ButtonClone2Click(Sender: TObject);
    procedure CreateRectangles;
    procedure AnimatePlaceRectanglesSlow(Sender: TObject);
    procedure ButtonMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonChangeBaseClick(Sender: TObject);
    procedure TimerCreateRectanglesTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonShuffleClick(Sender: TObject);
    procedure TimerReShuffleTimer(Sender: TObject);
    procedure Rectangle1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure RectangleClientResize(Sender: TObject);
    procedure TimerResizeTimer(Sender: TObject);
    procedure TimerTimeTimer(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure RectangleCloseClick(Sender: TObject);
    type
      TMode = (Game, GameOver, JustShuffled, Folded);
  private
    { Private declarations }
    FMode: TMode;
    FBase: integer;
    procedure SetMode(const Value: TMode);
    procedure SetMaxTime;
    procedure SetBase(const Value: integer);
    procedure AnimatePlaceRectanglesFast;
    procedure AnimateRectanglesDisappeare;
  public
    { Public declarations }
    Rectangles: array of TRectangle;

    RectSize: integer;
    RecSpacing: integer;
    SpaceX, SpaceY: integer;
    RectangleFillColor, RectangleFillColor1: TColor;
    LastResizeTime: TDateTime;

    property Base: integer read FBase write SetBase;
    property Mode: TMode read FMode write SetMode;
    procedure CalcConsts;
    procedure MoveRectangle(ARectangle: TRectangle; NewRow, NewCol: Word);
    procedure CheckFolded;
  end;

var
  FormPuzzle15: TFormPuzzle15;

implementation

{$R *.fmx}

procedure TFormPuzzle15.FormCreate(Sender: TObject);
begin
  RectangleFillColor := Rectangle1.Fill.Gradient.InterpolateColor(0);
  RectangleFillColor1 := Rectangle1.Fill.Gradient.InterpolateColor(1);

  Base := 4;
end;



procedure TFormPuzzle15.ButtonMenuClick(Sender: TObject);
begin
  if PanelMenuCl.Visible and (PanelMenuCl.Height = MenuFloatAnimation.StopValue) then
  begin
    MenuFloatAnimation.Inverse := true;
    MenuFloatAnimation.Start;
    exit;
  end;

  PanelMenuCl.Visible := true;
  MenuFloatAnimation.Inverse := false;
  MenuFloatAnimation.Start;
end;



procedure TFormPuzzle15.CalcConsts;
begin
  with PanelClient do
    if (Height > Width) then
    begin
      SpaceX := Round(Width / 20);
      RectSize := Round((Width - SpaceX * 2) / Base);
      SpaceY := SpaceX + Round((Height - Width) / 2);
    end
    else
    begin
      SpaceY := Round(Height / 20);
      RectSize := Round((Height - SpaceY * 2) / Base);
      SpaceX := SpaceY + Round((Width - Height) / 2);
    end;

  RecSpacing := Round(RectSize * 0.06);
  RectSize := Round(RectSize * 0.94);

  SpaceX := SpaceX + Round(RecSpacing / 2);
  SpaceY := SpaceY + Round(RecSpacing / 2);

end;


procedure TFormPuzzle15.AnimatePlaceRectanglesSlow(Sender: TObject);
var
  i : Integer;
  Row, Col: Word;
  ScaleX, ScaleY: Extended;
begin
  CalcConsts;

  for i := 0 to Length(Rectangles) - 1 do
    if (Rectangles[i] <> nil) then
    begin
      ScaleX := RectSize / Rectangles[i].Width;
      ScaleY := RectSize / Rectangles[i].Height;
      Rectangles[i].AnimateFloatDelay('Scale.X', ScaleX, 0.5, 0.9 + 0.1 * i);
      Rectangles[i].AnimateFloatDelay('Scale.Y', ScaleY, 0.5, 0.8 + 0.1 * i);

      DivMod(i, Base, Row, Col);

      Rectangles[i].Tag := i;
      Rectangles[i].AnimateFloatDelay('Position.X', SpaceX + Round(Col * (Rectangles[i].Width * ScaleX + RecSpacing)), 0.4, 0.5 + 0.1 * i);
      Rectangles[i].AnimateFloatDelay('Position.Y', SpaceY + Round(Row * (Rectangles[i].Height * ScaleY + RecSpacing)), 0.3, 0.5 + 0.1 * i);
    end;

end;


procedure TFormPuzzle15.AnimatePlaceRectanglesFast;
var
  i: Integer;
  Row, Col: Word;
  ScaleX, ScaleY: Extended;
begin
  for i := 0 to Length(Rectangles) - 1 do
    if (Rectangles[i] <> nil) then
    begin
      ScaleX := RectSize / Rectangles[i].Width;
      ScaleY := RectSize / Rectangles[i].Height;
      Rectangles[i].AnimateFloatDelay('Scale.X', ScaleX, 0.2, 0.2 + 0.03 * i);
      Rectangles[i].AnimateFloatDelay('Scale.Y', ScaleY, 0.2, 0.1 + 0.03 * i);

      DivMod(i, Base, Row, Col);

      Rectangles[i].Tag := i;
      Rectangles[i].AnimateFloatDelay('Position.X', SpaceX + Round(Col * (Rectangles[i].Width * ScaleX + RecSpacing)), 0.2, 0 + 0.03 * i);
      Rectangles[i].AnimateFloatDelay('Position.Y', SpaceY + Round(Row * (Rectangles[i].Height * ScaleY + RecSpacing)), 0.1, 0 + 0.03 * i);
      {, TAnimationType.atIn, TInterpolationType.Back}
    end;
end;





procedure TFormPuzzle15.CreateRectangles;
var
  i : Integer;
begin
  Rectangle1.Position.X := Self.Width - Rectangle1.Width - 10;
  Rectangle1.Position.Y := Self.Height - Rectangle1.Height - 10;
  Rectangle1.Tag := 0;
  Rectangle1.TagFloat := 0;
//  Rectangle1.Opacity := 1;


  for i := 0 to Length(Rectangles) - 1 do
    if (Rectangles[i] <> nil) then
      if (Rectangles[i] = Rectangle1) then
        Rectangles[i] := nil
      else
        FreeAndNil(Rectangles[i]);



  SetLength(Rectangles, Base * Base);
  Rectangles[0] := Rectangle1;


  for i := 1 to Length(Rectangles) - 2 do
    if (Rectangles[i] = nil) then
    begin
//      Rectangles[i] := CloneRectangle(Rectangles[0], 'RectangleCl' + IntToStr(i));
      Rectangles[i] := TRectangle(Rectangle1.Clone(Self));


      Rectangles[i].OnMouseDown := Rectangle1MouseDown;
      (Rectangles[i].Children[0] as TText).Text := IntToStr(i + 1);
      (Rectangles[i].Children[1] as TGradientAnimation).StartValue.Assign(Rectangles[i].Fill.Gradient);
      (Rectangles[i].Children[1] as TGradientAnimation).StopValue.Assign(Rectangles[i].Fill.Gradient);
//      (Rectangles[i].Children[1] as TGradientAnimation).OnFinish := RecGradientAniFinish;

      Rectangles[i].TagFloat := i;


//      Rectangles[i].Position.X := Rectangles[0].Position.X;
//      Rectangles[i].Position.Y := Rectangles[0].Position.Y;
//      Rectangles[i].Opacity := 1;
      Rectangles[i].Parent := PanelClient;
      Rectangles[i].SendToBack;
    end;

  if (Rectangles[Length(Rectangles) - 1] <> nil) then
    Rectangles[Length(Rectangles) - 1] := nil;


end;










procedure TFormPuzzle15.ButtonChangeBaseClick(Sender: TObject);
begin
  Base := (Sender as TRectangle).Tag;
end;


procedure TFormPuzzle15.SetBase(const Value: integer);
var
  i : Integer;
begin
  if (Value = Base) then
  // animate base not changed
  begin
    for i := 0 to Length(Rectangles) - 1 do
      if (Rectangles[i] <> nil) then
      begin
        Rectangles[i].AnimateFloatDelay('RotationAngle', -20, 0.1, 0,
          TAnimationType.InOut, TInterpolationType.Linear  );

        Rectangles[i].AnimateFloatDelay('RotationAngle', 20, 0.25, 0.1,
          TAnimationType.InOut, TInterpolationType.Exponential  );

        Rectangles[i].AnimateFloatDelay('RotationAngle', 0, 0.25, 0.35,
          TAnimationType.Out, TInterpolationType.Back  );
      end;

    exit;
  end;

  Mode := GameOver;
  AnimateRectanglesDisappeare;

  FBase := Value;
  SetMaxTime;

  if Length(Rectangles) > 0 then
    TimerCreateRectangles.Interval := 520 + 30 * Length(Rectangles)
  else
    TimerCreateRectangles.Interval := 10;
  TimerCreateRectangles.Enabled := true;
end;



procedure TFormPuzzle15.TimerCreateRectanglesTimer(Sender: TObject);
var
  i : Integer;
begin
  TimerCreateRectangles.Enabled := false;
  CreateRectangles;

  for i := 0 to Length(Rectangles) - 1 do
    if (Rectangles[i] <> nil) then
    begin
      Rectangles[i].AnimateFloatDelay('Opacity', 1, 0.4, 0.1 + 0.03 * i );
      Rectangles[i].AnimateFloatDelay('RotationAngle', 0, 0.4, 0.03 * i );
    end;

//  RectangleClientResize(Self);
  AnimatePlaceRectanglesSlow(Self);
end;

procedure TFormPuzzle15.TimerResizeTimer(Sender: TObject);
begin
  RectangleClientResize(TimerResize);
  TimerResize.Enabled := false;
end;

procedure TFormPuzzle15.RectangleClientResize(Sender: TObject);
var Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(Time - LastResizeTime, Hour, Min, Sec, MSec);
  if (Sec * 1000 + MSec < 500) and (Sender <> TimerResize) then
  begin
    TimerResize.Enabled := false;
    TimerResize.Enabled := true;
    LastResizeTime := Time;
    exit;
  end;

  LastResizeTime := Time;
  CalcConsts;
  AnimatePlaceRectanglesFast;
end;


procedure TFormPuzzle15.RectangleCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormPuzzle15.AnimateRectanglesDisappeare;
var
  i: Integer;
begin
  for i := 0 to Length(Rectangles) - 1 do
    if (Rectangles[i] <> nil) then
    begin
      Rectangles[i].AnimateFloatDelay('Scale.X', 0.1, 0.4, 0.03 * i);
      Rectangles[i].AnimateFloatDelay('Scale.Y', 0.1, 0.4, 0.03 * i);
      Rectangles[i].AnimateFloatDelay('RotationAngle', 45, 0.4, 0.03 * i);
      Rectangles[i].AnimateFloatDelay('Position.Y', Rectangles[i].Position.Y + RectSize, 0.4, 0.03 * i, TAnimationType.In, TInterpolationType.Back);
      Rectangles[i].AnimateFloatDelay('Position.X', Rectangles[i].Position.X + Round(RectSize / 2), 0.4, 0.03 * i);
      Rectangles[i].AnimateFloatDelay('Opacity', 0, 0.4, 0.1 + 0.03 * i);
    end;
end;






procedure TFormPuzzle15.CheckFolded;
var
  i : Integer;
  LFolded: Boolean;
//  Delay: Extended;
var  GradientAni: TGradientAnimation ;
begin
  LFolded := true;
  for i := 0 to Length(Rectangles) - 1 do
    if (Rectangles[i] <> nil) then
      if (Rectangles[i].TagFloat <> Rectangles[i].Tag) then
      begin
        LFolded := false;
        Break;
      end;

  if LFolded and (Mode = Game) then
  begin
    Mode := Folded;

    for i := 0 to Length(Rectangles) - 1 do
      if (Rectangles[i] <> nil) then
      begin

//        if (i = 0) then
//          Delay := 0
//        else
//          Delay := {1.7388 *} Ln(i){ + 1.9267};

//        Delay := Ln(i+1);

        Rectangles[i].AnimateFloatDelay('RotationAngle', 360, 1, 0.35,
          TAnimationType.Out, TInterpolationType.Back  );

        GradientAni := Rectangles[i].Children[1] as TGradientAnimation;

        GradientAni.Stop;
        GradientAni.StopValue.Color  := TAlphaColors.Gold;
        GradientAni.StopValue.Color1 := TAlphaColors.Lawngreen;

        GradientAni.Delay := 1 + i *0.1;
        GradientAni.Duration := 0.5;
        GradientAni.Tag := 0;
        GradientAni.Start;

      end;

  end;


  if (not LFolded) and ((Mode = Folded) or (Mode = JustShuffled)) then
  begin
    for i := 0 to Length(Rectangles) - 1 do
      if (Rectangles[i] <> nil) then
      begin
        GradientAni := Rectangles[i].Children[1] as TGradientAnimation;

        GradientAni.StopValue.Color := RectangleFillColor;
        GradientAni.StopValue.Color1 := RectangleFillColor1;

        GradientAni.Delay := 0.5;
        GradientAni.Duration := 0.6;
        GradientAni.Tag := 0;
        GradientAni.Start;

      end;

      if (Mode = Folded) then
        Mode := GameOver;
  end;

end;






procedure TFormPuzzle15.MoveRectangle(ARectangle: TRectangle; NewRow, NewCol: Word);
begin
  ARectangle.Tag := NewRow * Base + NewCol;

  ARectangle.AnimateFloatDelay('Position.X',
    SpaceX + Round(NewCol * (ARectangle.Width * ARectangle.Scale.X + RecSpacing)),
    0.15, 0, TAnimationType.Out, TInterpolationType.Exponential);
  ARectangle.AnimateFloatDelay('Position.Y',
    SpaceY + Round(NewRow * (ARectangle.Height * ARectangle.Scale.Y + RecSpacing)),
    0.15, 0, TAnimationType.Out, TInterpolationType.Exponential);

  CheckFolded;
end;




procedure TFormPuzzle15.ButtonShuffleClick(Sender: TObject);
var
  RectanglesOld: array of TRectangle;
  i, NewI, j : Integer;
  NewRow, NewCol: Word;
  inv, iValue, jValue: Integer;
begin
  SetLength(RectanglesOld, Length(Rectangles));
  for i := 0 to Length(Rectangles) - 1 do
  begin
    RectanglesOld[i] := Rectangles[i];
    Rectangles[i] := nil;
  end;

  for i := 0 to Length(Rectangles) - 1 do
    if (RectanglesOld[i] <> nil) then
    repeat
      newI := Random(Length(Rectangles));
      if (Rectangles[NewI] = nil) then
      begin
        Rectangles[NewI] := RectanglesOld[i];
        Rectangles[NewI].Tag := NewI;
        Break;
      end;

    until false;

  for i := 0 to Length(Rectangles) - 1 do
    if (Rectangles[i] <> nil) then
    begin
      DivMod(Rectangles[i].Tag, Base, NewRow, NewCol);

      Rectangles[i].AnimateFloatDelay('Position.X',
        SpaceX + Round(NewCol * (Rectangles[i].Width * Rectangles[i].Scale.X + RecSpacing)),
        0.4, 0, TAnimationType.Out, TInterpolationType.Exponential );
      Rectangles[i].AnimateFloatDelay('Position.Y',
        SpaceY + Round(NewRow * (Rectangles[i].Height * Rectangles[i].Scale.Y + RecSpacing)),
        0.4, 0.01 * i, TAnimationType.Out, TInterpolationType.Exponential );
    end;

  SetMaxTime;

  ShuffleGloomEffectAni.Enabled := false;
//  ShuffleGloomEffect.Enabled := true;
  ShuffleGloomEffect.BaseIntensity := 0.5;
  ShuffleGloomEffect.BaseSaturation := 0.5;
  ShuffleGloomEffect.GloomIntensity := 0.5;
  ShuffleGloomEffect.GloomSaturation := 0.05;


  inv := 0;
  for i := 0 to Length(Rectangles) - 1 do
    if (Rectangles[i] <> nil) then
      for j := 0 to i - 1 do
      begin
        iValue := Rectangles[i].Tag + 1;

        if (Rectangles[j] = nil) then
          jValue := 0
        else
          jValue := Rectangles[j].Tag + 1;

        if (jValue > iValue) then
          Inc(inv);
      end;

  for i := 0 to Length(Rectangles) - 1 do
    if (Rectangles[i] = nil) then
      Inc(inv, (i div 4) + 1);

  if Odd(inv) then
  begin
    TimerReShuffle.Interval := 400 + 10 * Length(Rectangles);
    TimerReShuffle.Enabled := true;
    exit;
  end;


  Mode := JustShuffled;

  CheckFolded;
end;



procedure TFormPuzzle15.TimerReShuffleTimer(Sender: TObject);
begin
  TimerReShuffle.Enabled := false;
  ButtonShuffleClick(TimerReShuffle);
end;






procedure TFormPuzzle15.SetMaxTime;
var
  Sec, Min: Word;
begin
  TextTime.Tag := ((Base * Base * Base * Base) div 20) * 10;
  DivMod(TextTime.Tag, 60, Min, Sec);
  TextTime.Text := Format('%d:%.2d', [Min, Sec]);
end;


procedure TFormPuzzle15.SetMode(const Value: TMode);
begin
  FMode := Value;
  TimerTime.Enabled := (FMode = Game);
end;



function rc(Row, Col: Integer): Integer; inline;
begin
  Result := Row * FormPuzzle15.Base + Col;
end;



procedure TFormPuzzle15.Rectangle1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  Row, Col: Word;
  RowI, ColI, NilRow, NilCol, RowMoveI, ColMoveI: integer;
  SenderRectangle: TRectangle absolute Sender;
begin
//  RectangleMenuClHide(Sender);
//  if (Mode = GameOver) then exit;

  if (Mode = JustShuffled) then
    Mode := Game;

  DivMod(SenderRectangle.Tag, Base, Row, Col);

  for RowI := 0 to Base - 1 do
    if (Rectangles[rc(RowI, Col)] = nil) then
    begin
      NilRow := RowI;

      if (NilRow > Row) then
        for RowMoveI := NilRow - 1 downto Row do
        begin
          Rectangles[rc(RowMoveI + 1 , Col)] := Rectangles[rc(RowMoveI , Col)];
          Rectangles[rc(RowMoveI , Col)] := nil;
          MoveRectangle(Rectangles[rc(RowMoveI + 1 , Col)], RowMoveI + 1, Col);
        end;

      if (Row > NilRow) then
        for RowMoveI := NilRow + 1 to Row do
        begin
          Rectangles[rc(RowMoveI - 1 , Col)] := Rectangles[rc(RowMoveI , Col)];
          Rectangles[rc(RowMoveI , Col)] := nil;
          MoveRectangle(Rectangles[rc(RowMoveI - 1 , Col)], RowMoveI - 1, Col);
        end;

      Exit;
    end;


  for ColI := 0 to Base - 1 do
    if (Rectangles[rc(Row , ColI)] = nil) then
    begin
      NilCol := ColI;

      if (NilCol > Col) then
        for ColMoveI := NilCol - 1 downto Col do
        begin
          Rectangles[rc(Row , ColMoveI + 1)] := Rectangles[rc(Row , ColMoveI)];
          Rectangles[rc(Row , ColMoveI)] := nil;
          MoveRectangle(Rectangles[rc(Row , ColMoveI + 1)], Row, ColMoveI + 1);
        end;

      if (Col > NilCol) then
        for ColMoveI := NilCol + 1 to Col do
        begin
          Rectangles[rc(Row , ColMoveI - 1)] := Rectangles[rc(Row , ColMoveI)];
          Rectangles[rc(Row , ColMoveI)] := nil;
          MoveRectangle(Rectangles[rc(Row , ColMoveI - 1)], Row, ColMoveI - 1);
        end;

    end;

end;










procedure TFormPuzzle15.Button1Click(Sender: TObject);
begin
//  Rectangle1.Stroke.Thickness := Rectangle1.Stroke.Thickness + 0.5;
  AnimateRectanglesDisappeare;
end;

procedure TFormPuzzle15.Button2Click(Sender: TObject);
begin
//  FrameRectangle1.Scale.Point := PointF(FrameRectangle1.Scale.X + 0.5, FrameRectangle1.Scale.Y + 0.5);
//  Rectangle1.Scale.Point := PointF(Rectangle1.Scale.X + 0.5, Rectangle1.Scale.Y + 0.5)
  Button3Click(nil);
  AnimatePlaceRectanglesFast;
end;


procedure TFormPuzzle15.Button3Click(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to Length(Rectangles) - 1 do
    if (Rectangles[i] <> nil) then
    begin
      Rectangles[i].Position := Rectangle1.Position;
      Rectangles[i].AnimateFloatDelay('Opacity', 1, 0.4, 0.1 + 0.03 * i );
      Rectangles[i].AnimateFloatDelay('RotationAngle', 0, 0.4, 0.03 * i );
    end;

end;

procedure TFormPuzzle15.ButtonCloneClick(Sender: TObject);
var    RectangleNew: TRectangle;
begin
//  RectangleNew := TRectangle(Rectangle1.Clone(Self));
//  RectangleNew.Position.Y := Rectangle1.Position.Y * (ButtonClone.Tag + 2);
//  Inc(ButtonClone.Tag);
//  RectangleNew.Parent := Self;
end;

procedure TFormPuzzle15.ButtonClone2Click(Sender: TObject);
var    RectangleNew: TRectangle;
begin
//  RectangleNew := CloneRectangle(Rectangle1, 'RectangleCl' + IntToStr(CloneNum2 + 2));
//
//  RectangleNew.Position.X := Rectangle1.Position.X + 50;
//  RectangleNew.Position.Y := Rectangle1.Position.Y * (CloneNum2 + 2);
//  Inc(CloneNum2);
//
//  RectangleNew.Parent := Self;
end;




procedure TFormPuzzle15.TimerTimeTimer(Sender: TObject);
var
  Min, Sec: Word;
  i: Integer;
  GradientAllertAni: TGradientAnimation;
begin
  TextTime.Tag := TextTime.Tag - 1;
  DivMod(TextTime.Tag, 60, Min, Sec);
  TextTime.Text := Format('%d:%.2d', [Min, Sec]);

  if (TextTime.Tag = 0) then
  begin
    Mode := GameOver;

    ShuffleGloomEffect.BaseIntensity := 1;
    ShuffleGloomEffect.BaseSaturation := 1;
    ShuffleGloomEffect.GloomIntensity := 0;
    ShuffleGloomEffect.GloomSaturation := 0;

//    ShuffleGloomEffect.Enabled := false;
    ShuffleGloomEffectAni.Enabled := true;
  end;

  if (TextTime.Tag > 10) then exit;



  for i := 0 to Length(Rectangles) - 1 do
    if (Rectangles[i] <> nil) then
    begin

      GradientAllertAni := (Rectangles[i].Children[1] as TGradientAnimation);
      GradientAllertAni.StopValue.Color  := TAlphaColors.Darkorange;
      GradientAllertAni.StopValue.Color1 := RectangleFillColor1;

      GradientAllertAni.Delay := 0;
      GradientAllertAni.Duration := 0.15;
      GradientAllertAni.Tag := 1;
      GradientAllertAni.Start;

    end;
end;

end.
