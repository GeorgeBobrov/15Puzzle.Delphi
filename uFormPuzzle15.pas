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
    Tile1: TRectangle;
    TileText: TText;
    TileGradientAni: TGradientAnimation;
    PanelMenuCl: TRectangle;
    MenuFloatAnimation: TFloatAnimation;
    GridPanelLayout1: TGridPanelLayout;
    Button3x3: TRectangle;
    Text33: TText;
    TimerCreateTiles: TTimer;
    ColorAnimation33: TColorAnimation;
    ColorAnimation4: TColorAnimation;
    Button4x4: TRectangle;
    Text44: TText;
    ColorAnimation5: TColorAnimation;
    ColorAnimation6: TColorAnimation;
    Button5x5: TRectangle;
    Text55: TText;
    ColorAnimation55: TColorAnimation;
    ColorAnimation3: TColorAnimation;
    ButtonClose: TRectangle;
    ButtonCloseColorAni: TColorAnimation;
    CloseGloomEffect: TGloomEffect;
    CloseGloomEffectAni: TFloatAnimation;
    ImageClose: TImage;
    PanelTop: TRectangle;
    LayoutCenter: TLayout;
    ButtonShuffle: TRectangle;
    ImageShuffle: TImage;
    ShuffleColorAnimation: TColorAnimation;
    ShuffleGloomEffect: TGloomEffect;
    ShuffleGloomEffectAni: TFloatAnimation;
    ButtonMenu: TRectangle;
    ImageMenu: TImage;
    ColorAnimation2: TColorAnimation;
    GloomEffect4: TGloomEffect;
    FloatAnimation4: TFloatAnimation;
    PanelTime: TRectangle;
    TextTime: TText;
    TimerReShuffle: TTimer;
    TimerResize: TTimer;
    TimerTime: TTimer;
    ButtonDisappeare: TButton;
    ButtonPlace: TButton;
    PanelDebug: TPanel;
    ButtonTimeRunningOut: TButton;
    ButtonBaseNotChanged: TButton;
    TimerClose: TTimer;
    ButtonPuzzleMatched: TButton;
    ButtonTimeOver: TButton;
    procedure CreateTiles;
    procedure ButtonMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonChangeBaseClick(Sender: TObject);
    procedure TimerCreateTilesTimer(Sender: TObject);
    procedure ButtonDisappeareClick(Sender: TObject);
    procedure ButtonPlaceClick(Sender: TObject);
    procedure ButtonShuffleClick(Sender: TObject);
    procedure TimerReShuffleTimer(Sender: TObject);
    procedure Tile1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure PanelClientResize(Sender: TObject);
    procedure TimerResizeTimer(Sender: TObject);
    procedure TimerTimeTimer(Sender: TObject);
    procedure ButtonTimeRunningOutClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonBaseNotChangedClick(Sender: TObject);
    procedure TimerCloseTimer(Sender: TObject);
    procedure ButtonPuzzleMatchedClick(Sender: TObject);
    procedure ButtonTimeOverClick(Sender: TObject);
    type
      TMode = (Game, GameOver, JustShuffled, PuzzleMatched);
  private
    { Private declarations }
    FMode: TMode;
    FBase: integer;
    procedure SetMode(const Value: TMode);
    procedure SetMaxTime;
    procedure SetBase(const Value: integer);
    procedure AnimatePlaceTilesFast;
    procedure AnimatePlaceTilesSlow;
    procedure AnimateTilesDisappeare;
    procedure AnimatePrepareBeforePlace;
    procedure AnimateBaseNotChanged;
    procedure AnimateTimeRunningOut;
    procedure AnimatePuzzleMatched;
    procedure AnimateTimeOver;
    procedure AnimateNormalizeTilesColor;
  public
    { Public declarations }
    Tiles: array of TRectangle;

    TileSize: integer;
    TileSpacing: integer;
    SpaceX, SpaceY: integer;
    TileFillNormalColor, TileFillNormalColor1: TColor;
    LastResizeTime: TDateTime;
    ClosingAnimation: Boolean;

    property Base: integer read FBase write SetBase;
    property Mode: TMode read FMode write SetMode;
    procedure CalcConsts;
    procedure MoveTile(ATile: TRectangle; NewRow, NewCol: Word);
    procedure CheckPuzzleMatched;
  end;

var
  FormPuzzle15: TFormPuzzle15;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TFormPuzzle15.FormCreate(Sender: TObject);
begin
  TileFillNormalColor := Tile1.Fill.Gradient.InterpolateColor(0);
  TileFillNormalColor1 := Tile1.Fill.Gradient.InterpolateColor(1);
  Base := 4;
end;




procedure TFormPuzzle15.SetMode(const Value: TMode);
begin
  FMode := Value;
  TimerTime.Enabled := (FMode = Game);
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
  begin
    AnimateBaseNotChanged;

    exit;
  end;

  Mode := GameOver;
  AnimateTilesDisappeare;

  FBase := Value;
  SetMaxTime;

  if Length(Tiles) > 0 then
    TimerCreateTiles.Interval := 520 + 30 * Length(Tiles)
  else
    TimerCreateTiles.Interval := 50;
  TimerCreateTiles.Enabled := true;
end;




procedure TFormPuzzle15.TimerCreateTilesTimer(Sender: TObject);
var
  i : Integer;
begin
  TimerCreateTiles.Enabled := false;
  CreateTiles;

  AnimatePrepareBeforePlace;
  AnimatePlaceTilesFast;
end;


procedure TFormPuzzle15.CreateTiles;
var
  i : Integer;
  NewTile: TRectangle;
begin

//  Tile1.Position.X := Self.Width - Tile1.Width - 10;
//  Tile1.Position.Y := Self.Height - Tile1.Height - 10;
  Tile1.Tag := 0;
  Tile1.TagFloat := 0;

  Tile1.Opacity := 0;


  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
      if (Tiles[i] = Tile1) then
        Tiles[i] := nil
      else
        FreeAndNil(Tiles[i]);


  SetLength(Tiles, Base * Base);
  Tiles[0] := Tile1;


  for i := 1 to Length(Tiles) - 2 do
    if (Tiles[i] = nil) then
    begin
      NewTile := TRectangle(Tile1.Clone(Self));

      NewTile.OnMouseDown := Tile1MouseDown;
      (NewTile.Children[0] as TText).Text := IntToStr(i + 1);
      (NewTile.Children[1] as TGradientAnimation).StartValue.Assign(NewTile.Fill.Gradient);
      (NewTile.Children[1] as TGradientAnimation).StopValue.Assign(NewTile.Fill.Gradient);
//      (NewTile.Children[1] as TGradientAnimation).OnFinish := RecGradientAniFinish;

      NewTile.TagFloat := i;


//      NewTile.Position.X := Tile1.Position.X;
//      NewTile.Position.Y := Tile1.Position.Y;
//      NewTile.Opacity := 1;
      NewTile.Parent := PanelClient;
      NewTile.SendToBack;

      Tiles[i] := NewTile;
    end;

  if (Tiles[Length(Tiles) - 1] <> nil) then
    Tiles[Length(Tiles) - 1] := nil;


end;













function ind(Row, Col: Integer): Integer; inline;
begin
  Result := Row * FormPuzzle15.Base + Col;
end;



procedure TFormPuzzle15.Tile1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  Row, Col: Word;
  RowI, ColI, RowEmpty, ColEmpty, RowMoveI, ColMoveI: integer;
  SenderTile: TRectangle absolute Sender;
begin

  if (Mode = JustShuffled) then
    Mode := Game;

  DivMod(SenderTile.Tag, Base, Row, Col);

  for RowI := 0 to Base - 1 do
    if (Tiles[ind(RowI, Col)] = nil) then
    begin
      RowEmpty := RowI;

      if (RowEmpty > Row) then
        for RowMoveI := RowEmpty - 1 downto Row do
        begin
          Tiles[ind(RowMoveI + 1 , Col)] := Tiles[ind(RowMoveI , Col)];
          Tiles[ind(RowMoveI , Col)] := nil;
          MoveTile(Tiles[ind(RowMoveI + 1 , Col)], RowMoveI + 1, Col);
        end;

      if (Row > RowEmpty) then
        for RowMoveI := RowEmpty + 1 to Row do
        begin
          Tiles[ind(RowMoveI - 1 , Col)] := Tiles[ind(RowMoveI , Col)];
          Tiles[ind(RowMoveI , Col)] := nil;
          MoveTile(Tiles[ind(RowMoveI - 1 , Col)], RowMoveI - 1, Col);
        end;

      Exit;
    end;


  for ColI := 0 to Base - 1 do
    if (Tiles[ind(Row , ColI)] = nil) then
    begin
      ColEmpty := ColI;

      if (ColEmpty > Col) then
        for ColMoveI := ColEmpty - 1 downto Col do
        begin
          Tiles[ind(Row , ColMoveI + 1)] := Tiles[ind(Row , ColMoveI)];
          Tiles[ind(Row , ColMoveI)] := nil;
          MoveTile(Tiles[ind(Row , ColMoveI + 1)], Row, ColMoveI + 1);
        end;

      if (Col > ColEmpty) then
        for ColMoveI := ColEmpty + 1 to Col do
        begin
          Tiles[ind(Row , ColMoveI - 1)] := Tiles[ind(Row , ColMoveI)];
          Tiles[ind(Row , ColMoveI)] := nil;
          MoveTile(Tiles[ind(Row , ColMoveI - 1)], Row, ColMoveI - 1);
        end;

    end;

end;


procedure TFormPuzzle15.MoveTile(ATile: TRectangle; NewRow, NewCol: Word);
begin
  ATile.Tag := NewRow * Base + NewCol;

  ATile.AnimateFloatDelay('Position.X',
    SpaceX + Round(NewCol * (ATile.Width * ATile.Scale.X + TileSpacing)),
    0.15, 0, TAnimationType.Out, TInterpolationType.Exponential);
  ATile.AnimateFloatDelay('Position.Y',
    SpaceY + Round(NewRow * (ATile.Height * ATile.Scale.Y + TileSpacing)),
    0.15, 0, TAnimationType.Out, TInterpolationType.Exponential);

  CheckPuzzleMatched;
end;



procedure TFormPuzzle15.CheckPuzzleMatched;
var
  i : Integer;
  LPuzzleMatched: Boolean;
begin
  LPuzzleMatched := true;
  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
      if (Tiles[i].TagFloat <> Tiles[i].Tag) then
      begin
        LPuzzleMatched := false;
        Break;
      end;

  if LPuzzleMatched and (Mode = Game) then
  begin
    Mode := PuzzleMatched;

    AnimatePuzzleMatched;
  end;


  if (not LPuzzleMatched) and ((Mode = PuzzleMatched) or (Mode = JustShuffled)) then
  begin
    AnimateNormalizeTilesColor;

    if (Mode = PuzzleMatched) then
      Mode := GameOver;
  end;

end;










procedure TFormPuzzle15.ButtonShuffleClick(Sender: TObject);
var
  TilesOld: array of TRectangle;
  i, NewI, j : Integer;
  NewRow, NewCol: Word;
  inv, iValue, jValue: Integer;
begin
  SetLength(TilesOld, Length(Tiles));
  for i := 0 to Length(Tiles) - 1 do
  begin
    TilesOld[i] := Tiles[i];
    Tiles[i] := nil;
  end;

  for i := 0 to Length(Tiles) - 1 do
    if (TilesOld[i] <> nil) then
    repeat
      newI := Random(Length(Tiles));
      if (Tiles[NewI] = nil) then
      begin
        Tiles[NewI] := TilesOld[i];
        Tiles[NewI].Tag := NewI;
        Break;
      end;

    until false;

  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      DivMod(Tiles[i].Tag, Base, NewRow, NewCol);

      Tiles[i].AnimateFloatDelay('Position.X',
        SpaceX + Round(NewCol * (Tiles[i].Width * Tiles[i].Scale.X + TileSpacing)),
        0.4, 0, TAnimationType.Out, TInterpolationType.Exponential );
      Tiles[i].AnimateFloatDelay('Position.Y',
        SpaceY + Round(NewRow * (Tiles[i].Height * Tiles[i].Scale.Y + TileSpacing)),
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
  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
      for j := 0 to i - 1 do
      begin
        iValue := Tiles[i].Tag + 1;

        if (Tiles[j] = nil) then
          jValue := 0
        else
          jValue := Tiles[j].Tag + 1;

        if (jValue > iValue) then
          Inc(inv);
      end;

  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] = nil) then
      Inc(inv, (i div 4) + 1);

  if Odd(inv) then
  begin
    TimerReShuffle.Interval := 400 + 10 * Length(Tiles);
    TimerReShuffle.Enabled := true;
    exit;
  end;


  Mode := JustShuffled;

  CheckPuzzleMatched;
end;


procedure TFormPuzzle15.SetMaxTime;
var
  Sec, Min: Word;
begin
  TextTime.Tag := ((Base * Base * Base * Base) div 20) * 10;
  DivMod(TextTime.Tag, 60, Min, Sec);
  TextTime.Text := Format('%d:%.2d', [Min, Sec]);
end;


procedure TFormPuzzle15.TimerReShuffleTimer(Sender: TObject);
begin
  TimerReShuffle.Enabled := false;
  ButtonShuffleClick(TimerReShuffle);
end;


procedure TFormPuzzle15.PanelClientResize(Sender: TObject);
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
  AnimatePlaceTilesFast;
end;


procedure TFormPuzzle15.TimerResizeTimer(Sender: TObject);
begin
  PanelClientResize(TimerResize);
  TimerResize.Enabled := false;
end;





procedure TFormPuzzle15.CalcConsts;
begin
  with PanelClient do
    if (Height > Width) then
    begin
      SpaceX := Round(Width / 20);
      TileSize := Round((Width - SpaceX * 2) / Base);
      SpaceY := SpaceX + Round((Height - Width) / 2);
    end
    else
    begin
      SpaceY := Round(Height / 20);
      TileSize := Round((Height - SpaceY * 2) / Base);
      SpaceX := SpaceY + Round((Width - Height) / 2);
    end;

  TileSpacing := Round(TileSize * 0.06);
  TileSize := Round(TileSize * 0.94);

  SpaceX := SpaceX + Round(TileSpacing / 2);
  SpaceY := SpaceY + Round(TileSpacing / 2);

end;

var slowdown: double = 1;

procedure TFormPuzzle15.AnimatePlaceTilesSlow;
var
  i, X, Y : Integer;
  Row, Col: Word;
  ScaleX, ScaleY: Extended;
begin
  CalcConsts;

  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      ScaleX := TileSize / Tiles[i].Width;
      ScaleY := TileSize / Tiles[i].Height;
      Tiles[i].AnimateFloatDelay('Scale.X', ScaleX, 0.5, 0.9 + 0.1 * i);
      Tiles[i].AnimateFloatDelay('Scale.Y', ScaleY, 0.5, 0.8 + 0.1 * i);

      DivMod(i, Base, Row, Col);

      X := SpaceX + Round(Col * (Tiles[i].Width * ScaleX + TileSpacing));
      Y := SpaceY + Round(Row * (Tiles[i].Height * ScaleY + TileSpacing));

      Tiles[i].Tag := i;
      Tiles[i].AnimateFloatDelay('Position.X', X, 0.4, 0.5 + 0.1 * i);
      Tiles[i].AnimateFloatDelay('Position.Y', Y, 0.3, 0.5 + 0.1 * i);
    end;

end;







procedure TFormPuzzle15.AnimatePlaceTilesFast;
var
  i, X, Y: Integer;
  Row, Col: Word;
  ScaleX, ScaleY: Extended;
begin
  CalcConsts;

  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      ScaleX := TileSize / Tiles[i].Width;
      ScaleY := TileSize / Tiles[i].Height;
      Tiles[i].AnimateFloatDelay('Scale.X', ScaleX, 0.2 * slowdown, (0.2 + 0.03 * i) * slowdown);
      Tiles[i].AnimateFloatDelay('Scale.Y', ScaleY, 0.2 * slowdown, (0.1 + 0.03 * i) * slowdown);

      DivMod(i, Base, Row, Col);

      X := SpaceX + Round(Col * (Tiles[i].Width * ScaleX + TileSpacing));
      Y := SpaceY + Round(Row * (Tiles[i].Height * ScaleY + TileSpacing));

      Tiles[i].Tag := i;
      Tiles[i].AnimateFloatDelay('Position.X', X, 0.2 * slowdown, (0 + 0.03 * i) * slowdown);
      Tiles[i].AnimateFloatDelay('Position.Y', Y, 0.1 * slowdown, (0 + 0.03 * i) * slowdown);
      {, TAnimationType.atIn, TInterpolationType.Back}
    end;
end;


procedure TFormPuzzle15.AnimateBaseNotChanged;
var
  i : Integer;
begin
  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      Tiles[i].AnimateFloatDelay('RotationAngle', -20, 0.1 * slowdown, 0 * slowdown,
        TAnimationType.InOut, TInterpolationType.Linear  );

      Tiles[i].AnimateFloatDelay('RotationAngle', 20, 0.25 * slowdown, 0.1 * slowdown,
        TAnimationType.InOut, TInterpolationType.Exponential  );

      Tiles[i].AnimateFloatDelay('RotationAngle', 0, 0.25 * slowdown, 0.35 * slowdown,
        TAnimationType.Out, TInterpolationType.Back  );
    end;

end;




procedure TFormPuzzle15.AnimateTilesDisappeare;
var
  i: Integer;
begin
  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      Tiles[i].AnimateFloatDelay('Scale.X', 0.1, 0.4, 0.03 * i);
      Tiles[i].AnimateFloatDelay('Scale.Y', 0.1, 0.4, 0.03 * i);
      Tiles[i].AnimateFloatDelay('RotationAngle', 45, 0.4, 0.03 * i);
      Tiles[i].AnimateFloatDelay('Position.Y', Tiles[i].Position.Y + TileSize, 0.4, 0.03 * i,
        TAnimationType.In, TInterpolationType.Back);
      Tiles[i].AnimateFloatDelay('Position.X', Tiles[i].Position.X + Round(TileSize / 2), 0.4, 0.03 * i);
      Tiles[i].AnimateFloatDelay('Opacity', 0, 0.4, 0.1 + 0.03 * i);
    end;
end;




procedure TFormPuzzle15.AnimatePrepareBeforePlace;
var
  i, X, Y: Integer;
  Row, Col: Word;
  ScaleX, ScaleY: Extended;
begin
  CalcConsts;

  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      ScaleX := TileSize / Tiles[i].Width;
      ScaleY := TileSize / Tiles[i].Height;

      DivMod(i, Base, Row, Col);

      X := SpaceX + Round(Col * (Tiles[i].Width * ScaleX + TileSpacing));
      Y := SpaceY + Round(Row * (Tiles[i].Height * ScaleY + TileSpacing));

      Tiles[i].Scale.X := 0.1;
      Tiles[i].Scale.Y := 0.1;
      Tiles[i].RotationAngle := 45;
      Tiles[i].Opacity := 0;
      Tiles[i].Position.X := X + Round(TileSize / 2);
      Tiles[i].Position.Y := Y + TileSize;
    end;


  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      Tiles[i].Position := Tile1.Position;
      Tiles[i].AnimateFloatDelay('Opacity', 1, 0.4 * slowdown, (0.1 + 0.03 * i) * slowdown );
      Tiles[i].AnimateFloatDelay('RotationAngle', 0, 0.4 * slowdown, (0.03 * i) * slowdown );
    end;

end;



procedure TFormPuzzle15.AnimateTimeRunningOut;
var
  i: Integer;
  GradientAni: TGradientAnimation;
begin
  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin

      GradientAni := (Tiles[i].Children[1] as TGradientAnimation);
      GradientAni.StopValue.Color  := TAlphaColors.Darkorange;
      GradientAni.StopValue.Color1 := TileFillNormalColor1;

      GradientAni.Delay := 0;
      GradientAni.Duration := 0.15 * slowdown;
      GradientAni.AutoReverse := true;

      GradientAni.Start;
    end;

end;


procedure TFormPuzzle15.AnimateTimeOver;
var
  i : Integer;
var  GradientAni: TGradientAnimation ;
begin
  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      GradientAni := Tiles[i].Children[1] as TGradientAnimation;

      GradientAni.StopValue.Color := TAlphaColors.Red;
      GradientAni.StopValue.Color1 := TAlphaColors.Gray;

      GradientAni.Delay := 0;
      GradientAni.Duration := 0.6;
      GradientAni.AutoReverse := false;

      GradientAni.Start;
    end;

end;



procedure TFormPuzzle15.AnimateNormalizeTilesColor;
var
  i : Integer;
var  GradientAni: TGradientAnimation ;
begin
  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      GradientAni := Tiles[i].Children[1] as TGradientAnimation;

      GradientAni.StopValue.Color := TileFillNormalColor;
      GradientAni.StopValue.Color1 := TileFillNormalColor1;

      GradientAni.Delay := 0;
      GradientAni.Duration := 0.2;
      GradientAni.AutoReverse := false;

      GradientAni.Start;
    end;

end;

procedure TFormPuzzle15.AnimatePuzzleMatched;
var
  i : Integer;
//  Delay: Extended;
var  GradientAni: TGradientAnimation ;
begin
  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin

//      if (i = 0) then
//        Delay := 0
//      else
//        Delay := {1.7388 *} Ln(i){ + 1.9267};
//
//      Delay := Ln(i+1);

      Tiles[i].AnimateFloatDelay('RotationAngle', 360, 1, 0.35,
        TAnimationType.Out, TInterpolationType.Back  );

      GradientAni := Tiles[i].Children[1] as TGradientAnimation;

      GradientAni.Stop;
      GradientAni.StopValue.Color  := TAlphaColors.Gold;
      GradientAni.StopValue.Color1 := TAlphaColors.Lawngreen;

      GradientAni.Delay := 1 + i * 0.1;
      GradientAni.Duration := 0.5;
      GradientAni.AutoReverse := false;

      GradientAni.Start;
    end;

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










procedure TFormPuzzle15.TimerTimeTimer(Sender: TObject);
var
  Min, Sec: Word;
begin
  TextTime.Tag := TextTime.Tag - 1;
  DivMod(TextTime.Tag, 60, Min, Sec);
  TextTime.Text := Format('%d:%.2d', [Min, Sec]);

  if (TextTime.Tag = 0) then
  begin
    Mode := GameOver;

    AnimateTimeOver;

    ShuffleGloomEffect.BaseIntensity := 1;
    ShuffleGloomEffect.BaseSaturation := 1;
    ShuffleGloomEffect.GloomIntensity := 0;
    ShuffleGloomEffect.GloomSaturation := 0;

//    ShuffleGloomEffect.Enabled := false;
    ShuffleGloomEffectAni.Enabled := true;

    exit;
  end;

  if (TextTime.Tag <= 10) then
    AnimateTimeRunningOut;
end;


procedure TFormPuzzle15.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;



procedure TFormPuzzle15.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not ClosingAnimation then
  begin
    AnimateTilesDisappeare;
    ClosingAnimation := true;
    TimerClose.Interval := 520 + 30 * Length(Tiles);
    TimerClose.Enabled := true;

    Action := TCloseAction.caNone;
  end;
end;

procedure TFormPuzzle15.TimerCloseTimer(Sender: TObject);
begin
  Close;
end;



procedure TFormPuzzle15.ButtonDisappeareClick(Sender: TObject);
begin
  AnimateTilesDisappeare;
end;

procedure TFormPuzzle15.ButtonPlaceClick(Sender: TObject);
begin
  AnimatePrepareBeforePlace;
  AnimatePlaceTilesFast;
end;


procedure TFormPuzzle15.ButtonTimeRunningOutClick(Sender: TObject);
begin
  AnimateTimeRunningOut;
end;

procedure TFormPuzzle15.ButtonTimeOverClick(Sender: TObject);
begin
  AnimateTimeOver;
end;

procedure TFormPuzzle15.ButtonPuzzleMatchedClick(Sender: TObject);
begin
  AnimatePuzzleMatched;
end;

procedure TFormPuzzle15.ButtonBaseNotChangedClick(Sender: TObject);
begin
  AnimateNormalizeTilesColor;
  AnimateBaseNotChanged;
end;


end.
