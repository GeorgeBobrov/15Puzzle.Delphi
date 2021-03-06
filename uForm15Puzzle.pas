unit uForm15Puzzle;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Objects, FMX.Effects, FMX.Filter.Effects, FMX.Ani, Math,
  FMX.Controls.Presentation, FMX.ListBox, FMX.ScrollBox, FMX.Memo, FMX.Edit,
  System.Generics.Collections;

type
  TForm15Puzzle = class(TForm)
    PanelClient: TPanel;
    Tile1: TRectangle;
    TileText: TText;
    TileGradientAni: TGradientAnimation;
    PanelMenu: TRectangle;
    PanelMenuAnimation: TFloatAnimation;
    GridPanelLayout1: TGridPanelLayout;
    Button3x3: TRectangle;
    Text33: TText;
    TimerCreateTiles: TTimer;
    ColorAnimation33Fill: TColorAnimation;
    ColorAnimation33Stroke: TColorAnimation;
    Button4x4: TRectangle;
    Text44: TText;
    ColorAnimation44Stroke: TColorAnimation;
    ColorAnimation44Fill: TColorAnimation;
    Button5x5: TRectangle;
    Text55: TText;
    ColorAnimation55Fill: TColorAnimation;
    ColorAnimation55Stroke: TColorAnimation;
    PanelTop: TRectangle;
    LayoutCenter: TLayout;
    ButtonShuffle: TRectangle;
    ImageShuffle: TImage;
    ShuffleStrokeColorAni: TColorAnimation;
    ShuffleGloomEffect: TGloomEffect;
    ShuffleGloomEffectAni: TFloatAnimation;
    ButtonMenu: TRectangle;
    ImageMenu: TImage;
    ButtonMenuStrokeColorAni: TColorAnimation;
    ButtonMenuGloomEffect: TGloomEffect;
    ButtonMenuGloomAni: TFloatAnimation;
    PanelTime: TRectangle;
    TextTime: TText;
    TimerResize: TTimer;
    TimerTime: TTimer;
    ButtonDisappeare: TButton;
    ButtonPlace: TButton;
    PanelDebug: TPanel;
    ButtonTimeRunningOut: TButton;
    ButtonBaseNotChanged: TButton;
    ButtonPuzzleMatched: TButton;
    ButtonTimeOver: TButton;
    PanelDebugAnimation: TFloatAnimation;
    ShuffleFillColorAni: TColorAnimation;
    TrackBarSlowdown: TTrackBar;
    LabelSpeed: TLabel;
    ShuffleFillColorAniMouseOver: TColorAnimation;
    procedure CreateTiles;
    procedure ButtonMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonChangeBaseClick(Sender: TObject);
    procedure TimerCreateTilesTimer(Sender: TObject);
    procedure ButtonDisappeareClick(Sender: TObject);
    procedure ButtonPlaceClick(Sender: TObject);
    procedure ButtonShuffleClick(Sender: TObject);
    procedure TileMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure PanelClientResize(Sender: TObject);
    procedure TimerResizeTimer(Sender: TObject);
    procedure TimerTimeTimer(Sender: TObject);
    procedure ButtonTimeRunningOutClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonBaseNotChangedClick(Sender: TObject);
    procedure ButtonPuzzleMatchedClick(Sender: TObject);
    procedure ButtonTimeOverClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure TrackBarSlowdownChange(Sender: TObject);
    procedure ShowDebug(Sender: TObject);
    procedure PanelClientClick(Sender: TObject);
    type
      TMode = (Game, GameOver, JustShuffled, PuzzleMatched);
  private
    { Private declarations }
    FMode: TMode;
    FBase: integer;
    procedure SetMode(const Value: TMode);
    procedure SetMaxTime;
    procedure SetBase(const Value: integer);
    function ind(Row, Col: Integer): Integer; inline;
  public
    { Public declarations }
    Tiles: array of TRectangle;

    TileSize: integer;
    TileSpacing: integer;
    SpaceX, SpaceY: integer;
    TileFillNormalColor, TileFillNormalColor1: TColor;
    LastResizeTime: TDateTime;
    ClosingAnimation: Boolean;
    WaitAnimationEnd: Boolean;

    property Base: integer read FBase write SetBase;
    property Mode: TMode read FMode write SetMode;

    function TryMoveTile(TilePosition: integer; MoveAniDuration: single): Boolean;
    procedure AnimateMoveTile(ATile: TRectangle; MoveAniDuration: single);

    function CheckCanPuzzleMatch: Boolean;
    procedure CheckPuzzleMatched;

    procedure CalcConsts;

    procedure AnimatePlaceTilesFast;
    procedure AnimatePlaceTilesSlow;
    procedure AnimateTilesDisappeare;
    procedure AnimatePrepareBeforePlace;
    procedure AnimateBaseNotChanged;
    procedure AnimateBaseNotChanged1;
    procedure AnimateBaseNotChanged2;
    procedure AnimateTimeRunningOut;
    procedure AnimatePuzzleMatched;
    procedure AnimateTimeOver;
    procedure AnimateNormalizeTilesColor;
    procedure StartBlinkShuffle;
    procedure StopBlinkShuffle;
  end;

var
  Form15Puzzle: TForm15Puzzle;

function AnimateFloatDelayEx(const Target: TFmxObject; const APropertyName: string; const NewValue: Single;
  Duration: Single = 0.2; Delay: Single = 0.0; AType: TAnimationType = TAnimationType.In;
  AInterpolation: TInterpolationType = TInterpolationType.Linear;
  DeleteWhenStopped: boolean = true; WaitAnimationEnd: boolean = false): TFloatAnimation;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

const
  MaxMoveAniDuration = 0.15;
  MinMoveAniDuration = 0.001;


procedure TForm15Puzzle.FormCreate(Sender: TObject);
begin
  LastResizeTime := Time;   //To prevent resize on start on Android

{$IF defined(ANDROID)}
   ButtonShuffle.Fill.Kind := TBrushKind.Solid;
   ButtonMenu.Fill.Kind := TBrushKind.Solid;

   ShuffleGloomEffect.Trigger := '';
   ShuffleGloomEffect.Enabled := false;

   ButtonMenuGloomEffect.Trigger := '';
   ButtonMenuGloomEffect.Enabled := false;
{$ELSE}
   ShuffleFillColorAniMouseOver.Trigger := '';
   ShuffleFillColorAniMouseOver.TriggerInverse := '';

   ButtonShuffle.Fill.Kind := TBrushKind.None;
   ButtonMenu.Fill.Kind := TBrushKind.None;
{$ENDIF}

  TileFillNormalColor := Tile1.Fill.Gradient.InterpolateColor(0);
  TileFillNormalColor1 := Tile1.Fill.Gradient.InterpolateColor(1);

  PanelClient.BringToFront;

  Base := 4;
end;







procedure TForm15Puzzle.SetMode(const Value: TMode);
begin
  FMode := Value;
  TimerTime.Enabled := (FMode = Game);
end;





procedure TForm15Puzzle.ButtonChangeBaseClick(Sender: TObject);
begin
  Base := (Sender as TRectangle).Tag;
end;


procedure TForm15Puzzle.SetBase(const Value: integer);
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

  TimerCreateTiles.Interval := 100;
  TimerCreateTiles.Enabled := true;
end;




procedure TForm15Puzzle.TimerCreateTilesTimer(Sender: TObject);
begin
  TimerCreateTiles.Enabled := false;
  CreateTiles;

  AnimatePrepareBeforePlace;
  AnimatePlaceTilesFast;
end;


procedure TForm15Puzzle.CreateTiles;
var
  i : Integer;
  NewTile: TRectangle;
begin
//  Tile1.Position.X := Self.Width - Tile1.Width - 10;
//  Tile1.Position.Y := Self.Height - Tile1.Height - 10;

  Tile1.Tag := 0;          //Actual Position of Tile in flat array, see ind()
  Tile1.TagFloat := 0;     //Position at creation time = Text number on Tile - 1

  Tile1.Opacity := 0;
  Tile1.Visible := true;

  Tile1.Fill.Gradient.Color := TileFillNormalColor;
  Tile1.Fill.Gradient.Color1 := TileFillNormalColor1;
  (Tile1.Children[1] as TGradientAnimation).StartValue.Assign(Tile1.Fill.Gradient);
  (Tile1.Children[1] as TGradientAnimation).StopValue.Assign(Tile1.Fill.Gradient);



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

      NewTile.OnMouseDown := TileMouseDown;
      (NewTile.Children[0] as TText).Text := IntToStr(i + 1);

      NewTile.TagFloat := i;

      NewTile.Parent := PanelClient;
      NewTile.SendToBack;

      Tiles[i] := NewTile;
    end;

  if (Tiles[Length(Tiles) - 1] <> nil) then
    Tiles[Length(Tiles) - 1] := nil;
end;






function TForm15Puzzle.ind(Row, Col: Integer): Integer;
begin
  Result := Row * Base + Col;
end;



procedure TForm15Puzzle.TileMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  SenderTile: TRectangle absolute Sender;
  WasMoved: Boolean;
begin
  if (Mode = JustShuffled) then
    Mode := Game;

  WasMoved := TryMoveTile(SenderTile.Tag, MaxMoveAniDuration);

  if WasMoved then
    CheckPuzzleMatched;
end;




function TForm15Puzzle.TryMoveTile(TilePosition: integer; MoveAniDuration: single): Boolean;
var
  RowPressed, ColPressed: Word;
  Row, Col, RowNoTile, ColNoTile, RowToMove, ColToMove: integer;
  WasMoved: Boolean;
  NewPosition: integer;

procedure MoveTile(OldPosition: integer; NewPosition: integer);
var temp: TRectangle;
begin
	temp := Tiles[NewPosition];
	Tiles[NewPosition] := Tiles[OldPosition];
	Tiles[OldPosition] := temp;

  Tiles[NewPosition].Tag := NewPosition;
	AnimateMoveTile(Tiles[NewPosition], MoveAniDuration);
end;

begin
  DivMod(TilePosition, Base, RowPressed, ColPressed);

  WasMoved := false;
  for Row := 0 to Base - 1 do
    if (Tiles[ind(Row, ColPressed)] = nil) then
    begin
      RowNoTile := Row;

      if (RowNoTile > RowPressed) then // Move tiles down
        for RowToMove := RowNoTile - 1 downto RowPressed do
        begin
          MoveTile(ind(RowToMove, ColPressed), ind(RowToMove + 1, ColPressed));
          WasMoved := true;
        end;

      if (RowPressed > RowNoTile) then // Move tiles up
        for RowToMove := RowNoTile + 1 to RowPressed do
        begin
          MoveTile(ind(RowToMove, ColPressed), ind(RowToMove - 1, ColPressed));
          WasMoved := true;
        end;

    end;

  if not WasMoved then
  for Col := 0 to Base - 1 do
    if (Tiles[ind(RowPressed , Col)] = nil) then
    begin
      ColNoTile := Col;

      if (ColNoTile > ColPressed) then // Move tiles right
        for ColToMove := ColNoTile - 1 downto ColPressed do
        begin
          MoveTile(ind(RowPressed, ColToMove), ind(RowPressed, ColToMove + 1));
          WasMoved := true;
        end;

      if (ColPressed > ColNoTile) then // Move tiles left
        for ColToMove := ColNoTile + 1 to ColPressed do
        begin
          MoveTile(ind(RowPressed, ColToMove), ind(RowPressed, ColToMove - 1));
          WasMoved := true;
        end;

    end;

  Result := WasMoved;
end;


procedure TForm15Puzzle.AnimateMoveTile(ATile: TRectangle; MoveAniDuration: single);
var
  NewRow, NewCol: Word;
  X, Y : Integer;
begin
  DivMod(ATile.Tag, Base, NewRow, NewCol);

  X := SpaceX + Round(NewCol * (ATile.Width * ATile.Scale.X + TileSpacing));
  Y := SpaceY + Round(NewRow * (ATile.Height * ATile.Scale.Y + TileSpacing));

  if MoveAniDuration > 0 then
  begin
    TAnimator.AnimateFloatDelay(ATile, 'Position.X', X,
      MoveAniDuration, 0, TAnimationType.Out, TInterpolationType.Exponential);

    if WaitAnimationEnd then
      TAnimator.AnimateFloatWait(ATile, 'Position.Y', Y,
        MoveAniDuration, TAnimationType.Out, TInterpolationType.Exponential)
    else
      TAnimator.AnimateFloatDelay(ATile, 'Position.Y', Y,
        MoveAniDuration, 0, TAnimationType.Out, TInterpolationType.Exponential);
  end
  else
  begin
    ATile.Position.X := X;
    ATile.Position.Y := Y;
  end;

end;



procedure TForm15Puzzle.CheckPuzzleMatched;
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


  if (not LPuzzleMatched) and (Mode = PuzzleMatched) then
  begin
    AnimateNormalizeTilesColor;

    if (Mode = PuzzleMatched) then
      Mode := GameOver;
  end;

end;






procedure TForm15Puzzle.ButtonShuffleClick(Sender: TObject);
var
  i, NewI : Integer;
  MoveCount: Integer;
  MoveAniDuration: Single;
begin
  AnimateNormalizeTilesColor;

  WaitAnimationEnd := true;
  MoveCount := Length(Tiles) * Length(Tiles);
  MoveAniDuration := MaxMoveAniDuration;
  for i := 1 to MoveCount do
  begin
    if i <= 10 then
      MoveAniDuration := MinMoveAniDuration + (MaxMoveAniDuration *
        (1 - (i / 10)));

    if i >= MoveCount - 10 then
      MoveAniDuration := MinMoveAniDuration + ((MaxMoveAniDuration / 2) *
        (1 - ((MoveCount - i) / 10)));

    if (i > 20) and (i < MoveCount - 20) then
      if (i mod 10) = 0 then
        MoveAniDuration := MinMoveAniDuration
      else
        MoveAniDuration := 0;

    repeat
      newI := Random(Length(Tiles));
    until TryMoveTile(newI, MoveAniDuration);
  end;
  WaitAnimationEnd := false;


  SetMaxTime;

  StopBlinkShuffle;

  Mode := JustShuffled;

  CheckPuzzleMatched;
end;






function TForm15Puzzle.CheckCanPuzzleMatch: Boolean;
var
  i, j: Integer;
  iValue, jValue: Integer;
  inv: Integer;
begin
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
      Inc(inv, (i div Base) + 1);

  Result := not Odd(inv);
end;



procedure TForm15Puzzle.SetMaxTime;
var
  Sec, Min: Word;
begin
  TextTime.Tag := ((Base * Base * Base * Base) div 20) * 10;
  DivMod(TextTime.Tag, 60, Min, Sec);
  TextTime.Text := Format('%d:%.2d', [Min, Sec]);
end;



procedure TForm15Puzzle.TimerTimeTimer(Sender: TObject);
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
    StartBlinkShuffle;

    exit;
  end;

  if (TextTime.Tag <= 10) then
    AnimateTimeRunningOut;
end;





procedure TForm15Puzzle.PanelClientResize(Sender: TObject);
begin
    TimerResize.Enabled := false;
    TimerResize.Enabled := true;
end;






procedure TForm15Puzzle.TimerResizeTimer(Sender: TObject);
var
  TimeFromLastResize_ms: Extended;
begin
  TimerResize.Enabled := false;

  TimeFromLastResize_ms := (Time - LastResizeTime) * SecsPerDay * 1000;

  if TimeFromLastResize_ms > 500 then
  begin
    AnimatePlaceTilesFast;
    LastResizeTime := Time;
  end;

end;






procedure TForm15Puzzle.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkHardwareBack then
  begin
    Key := 0;
    Close;
  end;

end;

procedure TForm15Puzzle.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not ClosingAnimation then
  begin
    AnimateTilesDisappeare;
    ClosingAnimation := true;
  end;
end;




//-------------------------------   Animations   -----------------------------



procedure TForm15Puzzle.CalcConsts;
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

procedure TForm15Puzzle.TrackBarSlowdownChange(Sender: TObject);
begin
  case Round(TrackBarSlowdown.Value) of
    0: slowdown := 0.5;
    4: slowdown := 5;
    5: slowdown := 10;
  else
    slowdown := TrackBarSlowdown.Value;
  end;

  LabelSpeed.Text := 'Speed'#9'1/' + FloatToStr(slowdown);
end;




procedure TForm15Puzzle.AnimatePlaceTilesSlow;
var
  Tile: TRectangle;
  i, X, Y : Integer;
  Row, Col: Word;
  ScaleX, ScaleY: Extended;
begin
  CalcConsts;

  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      Tile := Tiles[i];
      ScaleX := TileSize / Tile.Width;
      ScaleY := TileSize / Tile.Height;
      TAnimator.AnimateFloatDelay(Tile, 'Scale.X', ScaleX, 0.5, 0.9 + 0.1 * i);
      TAnimator.AnimateFloatDelay(Tile, 'Scale.Y', ScaleY, 0.5, 0.8 + 0.1 * i);

      DivMod(i, Base, Row, Col);

      X := SpaceX + Round(Col * (Tile.Width * ScaleX + TileSpacing));
      Y := SpaceY + Round(Row * (Tile.Height * ScaleY + TileSpacing));

      Tile.Tag := i;
      TAnimator.AnimateFloatDelay(Tile, 'Position.X', X, 0.4, 0.5 + 0.1 * i);
      TAnimator.AnimateFloatDelay(Tile, 'Position.Y', Y, 0.3, 0.5 + 0.1 * i);
    end;

end;




procedure TForm15Puzzle.AnimatePlaceTilesFast;
var
  Tile: TRectangle;
  i, X, Y: Integer;
  Row, Col: Word;
  ScaleX, ScaleY: Extended;
begin
  CalcConsts;

  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      Tile := Tiles[i];
      ScaleX := TileSize / Tile.Width;
      ScaleY := TileSize / Tile.Height;
      TAnimator.AnimateFloatDelay(Tile, 'Scale.X', ScaleX, 0.2 * slowdown, (0.2 + 0.03 * i) * slowdown);
      TAnimator.AnimateFloatDelay(Tile, 'Scale.Y', ScaleY, 0.2 * slowdown, (0.1 + 0.03 * i) * slowdown);

      DivMod(i, Base, Row, Col);

      X := SpaceX + Round(Col * (Tile.Width * ScaleX + TileSpacing));
      Y := SpaceY + Round(Row * (Tile.Height * ScaleY + TileSpacing));

      Tile.Tag := i;
      TAnimator.AnimateFloatDelay(Tile, 'Position.X', X, 0.2 * slowdown, (0 + 0.03 * i) * slowdown);
      TAnimator.AnimateFloatDelay(Tile, 'Position.Y', Y, 0.1 * slowdown, (0 + 0.03 * i) * slowdown);
      {, TAnimationType.atIn, TInterpolationType.Back}
    end;
end;



procedure TForm15Puzzle.AnimateBaseNotChanged;
begin
  case ButtonBaseNotChanged.Tag mod 2 of
  0: AnimateBaseNotChanged1;
  1: AnimateBaseNotChanged2;
  else
    begin
      AnimatePrepareBeforePlace;
      AnimatePlaceTilesFast;
    end;
  end;

  ButtonBaseNotChanged.Tag := ButtonBaseNotChanged.Tag + 1;
end;


procedure TForm15Puzzle.AnimateBaseNotChanged1;
var
  Tile: TRectangle;
  i: Integer;
begin
  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      Tile := Tiles[i];
      TAnimator.AnimateFloatDelay(Tile, 'RotationAngle', -20, 0.1 * slowdown, 0 * slowdown,
        TAnimationType.InOut, TInterpolationType.Linear);

      TAnimator.AnimateFloatDelay(Tile, 'RotationAngle', 20, 0.25 * slowdown, 0.1 * slowdown,
        TAnimationType.InOut, TInterpolationType.Exponential);

      TAnimator.AnimateFloatDelay(Tile, 'RotationAngle', 0, 0.25 * slowdown, 0.35 * slowdown,
        TAnimationType.Out, TInterpolationType.Back);
    end;
end;


procedure TForm15Puzzle.AnimateBaseNotChanged2;
var
  Tile: TRectangle;
  i, X, Y: Integer;
  Row, Col: Word;
  OrigScaleX, OrigScaleY: Extended;
  Offset: Extended;
  OrigPosition: TPosition;
begin
  i := 0;
  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      Tile := Tiles[i];
      OrigPosition := Tile.Position;
      OrigScaleX := Tile.Scale.X;
      OrigScaleY := Tile.Scale.Y;

      DivMod(i, Base, Row, Col);

      Offset := (Tile.Width * Tile.Scale.X) / 4 ;

      X := Round(OrigPosition.X + Offset);
      Y := Round(OrigPosition.Y + Offset);

      TAnimator.AnimateFloatDelay(Tile, 'Scale.X', OrigScaleX / 2, 0.3 * slowdown,
        (0 + 0.03 * i) * slowdown, TAnimationType.In, TInterpolationType.Back);
      TAnimator.AnimateFloatDelay(Tile, 'Scale.Y', OrigScaleY / 2, 0.3 * slowdown,
        (0 + 0.03 * i) * slowdown, TAnimationType.In, TInterpolationType.Back);
      TAnimator.AnimateFloatDelay(Tile, 'Position.X', X, 0.3 * slowdown,
        (0 + 0.03 * i) * slowdown, TAnimationType.In, TInterpolationType.Back);
      TAnimator.AnimateFloatDelay(Tile, 'Position.Y', Y, 0.3 * slowdown,
        (0 + 0.03 * i) * slowdown, TAnimationType.In, TInterpolationType.Back);

      TAnimator.AnimateFloatDelay(Tile, 'Scale.X', OrigScaleX, 0.3 * slowdown,
        (0.35 + 0.03 * i) * slowdown, TAnimationType.Out, TInterpolationType.Back);
      TAnimator.AnimateFloatDelay(Tile, 'Scale.Y', OrigScaleY, 0.3 * slowdown,
        (0.35 + 0.03 * i) * slowdown, TAnimationType.Out, TInterpolationType.Back);
      TAnimator.AnimateFloatDelay(Tile, 'Position.X', OrigPosition.X, 0.3 * slowdown,
        (0.35 + 0.03 * i) * slowdown, TAnimationType.Out, TInterpolationType.Back);
      TAnimator.AnimateFloatDelay(Tile, 'Position.Y', OrigPosition.Y, 0.3 * slowdown,
        (0.35 + 0.03 * i) * slowdown, TAnimationType.Out, TInterpolationType.Back);
    end;
end;



procedure TForm15Puzzle.AnimateTilesDisappeare;
var
  i: Integer;
  Tile: TRectangle;
  AniList: TList<TFloatAnimation>;
  SomeAniRunning: boolean;
  Animation: TFloatAnimation;
begin
  AniList := TList<TFloatAnimation>.Create;

  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      Tile := Tiles[i];
      TAnimator.AnimateFloatDelay(Tile, 'Scale.X', 0.1, 0.4 * slowdown, (0.03 * i) * slowdown);
      TAnimator.AnimateFloatDelay(Tile, 'Scale.Y', 0.1, 0.4 * slowdown, (0.03 * i) * slowdown);

      Animation := AnimateFloatDelayEx(Tile, 'RotationAngle', 45,
        0.4 * slowdown, (0.03 * i) * slowdown,
        TAnimationType.In,  TInterpolationType.Linear, {DeleteWhenStopped} false);

      AniList.Add(Animation);

      TAnimator.AnimateFloatDelay(Tile, 'Position.Y', Tile.Position.Y + TileSize,
        0.4 * slowdown, (0.03 * i) * slowdown, TAnimationType.In, TInterpolationType.Back);
      TAnimator.AnimateFloatDelay(Tile, 'Position.X',
        Tile.Position.X + Round(TileSize / 2), 0.4 * slowdown, (0.03 * i) * slowdown);

      Animation := AnimateFloatDelayEx(Tile, 'Opacity', 0,
        0.4 * slowdown, (0.1 + 0.03 * i) * slowdown,
        TAnimationType.In,  TInterpolationType.Linear, {DeleteWhenStopped} false);

      AniList.Add(Animation);
    end;

//Wait end of all animations;
  repeat
    SomeAniRunning := false;

    for Animation in AniList do
      if (Animation.Running) then
        SomeAniRunning := true;

    if SomeAniRunning then
    begin
      Application.ProcessMessages;
      Sleep(0);
    end
    else
      break;

  until false;

    for Animation in AniList do
      Animation.DisposeOf;
end;




procedure TForm15Puzzle.AnimatePrepareBeforePlace;
var
  Tile: TRectangle;
  i, X, Y: Integer;
  Row, Col: Word;
  ScaleX, ScaleY: Extended;
begin
  CalcConsts;

  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      Tile := Tiles[i];
      ScaleX := TileSize / Tile.Width;
      ScaleY := TileSize / Tile.Height;

      DivMod(i, Base, Row, Col);

      X := SpaceX + Round(Col * (Tile.Width * ScaleX + TileSpacing));
      Y := SpaceY + Round(Row * (Tile.Height * ScaleY + TileSpacing));

      Tile.Scale.X := 0.01;
      Tile.Scale.Y := 0.01;
      Tile.RotationAngle := 45;
      Tile.Opacity := 0;
      Tile.Position.X := X + Round(TileSize / 2);
      Tile.Position.Y := Y + TileSize;
    end;


  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      Tile := Tiles[i];
      Tile.Position := Tile1.Position; //it is prettier

      TAnimator.AnimateFloatDelay(Tile, 'Opacity', 1, 0.4 * slowdown, (0.1 + 0.03 * i) * slowdown);
      TAnimator.AnimateFloatDelay(Tile, 'RotationAngle', 0, 0.4 * slowdown, (0.03 * i) * slowdown);
    end;

end;



procedure TForm15Puzzle.AnimateTimeRunningOut;
var
  i: Integer;
  Tile: TRectangle;
  GradientAni: TGradientAnimation;
begin
  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      Tile := Tiles[i];
      GradientAni := (Tile.Children[1] as TGradientAnimation);
      GradientAni.StopValue.Color  := TAlphaColors.Darkorange;
      GradientAni.StopValue.Color1 := TileFillNormalColor1;

      GradientAni.Delay := 0;
      GradientAni.Duration := 0.15 * slowdown;
      GradientAni.AutoReverse := true;

      GradientAni.Start;
    end;

end;


procedure TForm15Puzzle.AnimateTimeOver;
var
  i: Integer;
  Tile: TRectangle;
var  GradientAni: TGradientAnimation ;
begin
  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      Tile := Tiles[i];
      GradientAni := Tile.Children[1] as TGradientAnimation;
      GradientAni.Stop;

      GradientAni.StopValue.Color := TAlphaColors.Red;
      GradientAni.StopValue.Color1 := TAlphaColors.Gray;

      GradientAni.Delay := 0;
      GradientAni.Duration := 0.6 * slowdown;
      GradientAni.AutoReverse := false;

      GradientAni.Start;
    end;

end;



procedure TForm15Puzzle.AnimateNormalizeTilesColor;
var
  i: Integer;
  Tile: TRectangle;
var  GradientAni: TGradientAnimation ;
begin
  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      Tile := Tiles[i];
      GradientAni := Tile.Children[1] as TGradientAnimation;

      GradientAni.StopValue.Color := TileFillNormalColor;
      GradientAni.StopValue.Color1 := TileFillNormalColor1;

      GradientAni.Delay := 0;
      GradientAni.Duration := 0.5;
      GradientAni.AutoReverse := false;

      GradientAni.Start;
    end;

end;

procedure TForm15Puzzle.AnimatePuzzleMatched;
var
  Tile: TRectangle;
  i : Integer;
var  GradientAni: TGradientAnimation ;
begin
  for i := 0 to Length(Tiles) - 1 do
    if (Tiles[i] <> nil) then
    begin
      Tile := Tiles[i];
      TAnimator.AnimateFloatDelay(Tile, 'RotationAngle', 360, 1 * slowdown, 0.35 * slowdown,
        TAnimationType.Out, TInterpolationType.Back);

      GradientAni := Tile.Children[1] as TGradientAnimation;

      GradientAni.Stop;
      GradientAni.StopValue.Color  := TAlphaColors.Gold;
      GradientAni.StopValue.Color1 := TAlphaColors.Lawngreen;

      GradientAni.Delay := (1 + i * 0.1) * slowdown;
      GradientAni.Duration := 0.5 * slowdown;
      GradientAni.AutoReverse := false;

      GradientAni.Start;
    end;

end;

procedure TForm15Puzzle.StartBlinkShuffle;
begin
  ShuffleGloomEffect.BaseIntensity := 1;
  ShuffleGloomEffect.BaseSaturation := 1;
  ShuffleGloomEffect.GloomIntensity := 0;
  ShuffleGloomEffect.GloomSaturation := 0;
  ShuffleGloomEffectAni.Enabled := true;

{$IF defined(ANDROID)}
  ButtonShuffle.Stroke.Color := ShuffleStrokeColorAni.StopValue;
  ShuffleFillColorAni.Enabled := true;
{$ENDIF}
end;

procedure TForm15Puzzle.StopBlinkShuffle;
begin
  ShuffleGloomEffectAni.Enabled := false;
  ShuffleGloomEffect.BaseIntensity := 0.5;
  ShuffleGloomEffect.BaseSaturation := 0.5;
  ShuffleGloomEffect.GloomIntensity := 0.5;
  ShuffleGloomEffect.GloomSaturation := 0.05;

{$IF defined(ANDROID)}
  ShuffleFillColorAni.Enabled := false;

  ButtonShuffle.Fill.Color := ShuffleFillColorAniMouseOver.StartValue;
  ButtonShuffle.Stroke.Color := ShuffleStrokeColorAni.StartValue;
{$ENDIF}
end;

//-----------------------------   -----------------------------	-----------------------------

procedure TForm15Puzzle.ButtonMenuClick(Sender: TObject);
begin
  if not PanelMenu.Visible then
  begin
    PanelMenu.Height := 0;
    PanelMenu.Visible := true;
  end;

  if (PanelMenu.Height = 0) then
    PanelMenu.Position.Y := 10;

  PanelMenuAnimation.Inverse := (PanelMenu.Height = PanelMenuAnimation.StopValue);

  PanelMenuAnimation.Start;
end;



procedure TForm15Puzzle.PanelClientClick(Sender: TObject);
var
  TimeFromLastTap_ms: Extended;
begin
  TimeFromLastTap_ms := (Time - PanelClient.TagFloat) * SecsPerDay * 1000;

  if (PanelClient.TagFloat > 0) and (TimeFromLastTap_ms < 300) then
    ShowDebug(Sender);

  PanelClient.TagFloat := Time;
end;


procedure TForm15Puzzle.ShowDebug(Sender: TObject);
begin

  if not PanelDebug.Visible then
  begin
    PanelDebug.Height := 0;
    PanelDebug.Visible := true;
  end;

  if (PanelDebug.Height = 0) then
    PanelDebug.Position.Y := PanelMenu.Position.Y + 100;

  PanelDebugAnimation.Inverse := (PanelDebug.Height = PanelDebugAnimation.StopValue);
//  SpeedButtonEffects.IsPressed := not PanelDebugAnimation.Inverse;

  PanelDebugAnimation.Start;
end;



//-------------------------------  Test different Animations   -----------------------------


procedure TForm15Puzzle.ButtonDisappeareClick(Sender: TObject);
begin
  AnimateTilesDisappeare;
end;



procedure TForm15Puzzle.ButtonPlaceClick(Sender: TObject);
begin
  AnimateNormalizeTilesColor;
  AnimatePrepareBeforePlace;
  AnimatePlaceTilesFast;
end;


procedure TForm15Puzzle.ButtonTimeRunningOutClick(Sender: TObject);
begin
  AnimateTimeRunningOut;
end;

procedure TForm15Puzzle.ButtonTimeOverClick(Sender: TObject);
begin
  AnimateTimeOver;
end;

procedure TForm15Puzzle.ButtonPuzzleMatchedClick(Sender: TObject);
begin
  AnimatePuzzleMatched;
end;





procedure TForm15Puzzle.ButtonBaseNotChangedClick(Sender: TObject);
begin
  AnimateBaseNotChanged;
end;


//---------------------------  Realization of Property Animation   -----------------------------
type
  TAnimationDestroyer = class
  private
    procedure DoAniFinished(Sender: TObject);
  end;

procedure TAnimationDestroyer.DoAniFinished(Sender: TObject);
begin
  TThread.ForceQueue(nil, procedure begin
    TAnimation(Sender).DisposeOf;
  end);
end;

var
  FDestroyer: TAnimationDestroyer;

// Based on FMX.Ani.TAnimator.AnimateFloatDelay

function AnimateFloatDelayEx(const Target: TFmxObject; const APropertyName: string; const NewValue: Single;
  Duration: Single = 0.2; Delay: Single = 0.0; AType: TAnimationType = TAnimationType.In;
  AInterpolation: TInterpolationType = TInterpolationType.Linear;
  DeleteWhenStopped: boolean = true; WaitAnimationEnd: boolean = false): TFloatAnimation;
var
  Animation: TFloatAnimation;
begin
  if FDestroyer = nil then
    FDestroyer := TAnimationDestroyer.Create;

  Animation := TFloatAnimation.Create(nil);
  Animation.Parent := Target;
  Animation.AnimationType := AType;
  Animation.Interpolation := AInterpolation;
  Animation.Delay := Delay;
  Animation.Duration := Duration;
  Animation.PropertyName := APropertyName;
  Animation.StartFromCurrent := True;

  if (DeleteWhenStopped) then
    Animation.OnFinish := FDestroyer.DoAniFinished;

  Animation.StopValue := NewValue;
  Animation.Start;

  if (WaitAnimationEnd) then
    while Animation.Running do
    begin
      Application.ProcessMessages;
      Sleep(0);
    end;

  Result := Animation;
end;


//procedure TForm15Puzzle.ComboBoxQualityChange(Sender: TObject);
//begin
//  Quality := TCanvasQuality(ComboBoxQuality.ItemIndex);

//  case Quality of
//    TCanvasQuality.SystemDefault:   LabelQuality.Text := 'SystemDefault';
//    TCanvasQuality.HighPerformance: LabelQuality.Text := 'HighPerformance';
//    TCanvasQuality.HighQuality:     LabelQuality.Text := 'HighQuality';
//  end;
//end;


//procedure TForm15Puzzle.ButtonFontClick(Sender: TObject);
//var
//  i : Integer;
//begin
//  for i := 0 to Length(Tiles) - 1 do
//    if (Tiles[i] <> nil) then
//    begin
//      (Tiles[i].Children[0] as TText).Font.Family := EditFont.Text;
//    end;
//end;

end.
