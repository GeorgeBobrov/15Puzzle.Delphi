program P15Pazzle;

uses
  System.StartUpCopy,
  FMX.Forms,
  uForm15Puzzle in 'uForm15Puzzle.pas' {Form15Puzzle};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.InvertedPortrait, TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TForm15Puzzle, Form15Puzzle);
  Application.Run;
end.
